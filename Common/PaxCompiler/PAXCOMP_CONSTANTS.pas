////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_CONSTANTS.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_CONSTANTS;
interface

type
  TTargetPlatform = (tpNONE,
                     tpWin32,
                     tpWin64,
                     tpOSX32,
                     tpIOSSim,
                     tpIOSDEv,
                     tpANDROID,
                     tpLINUX32);

  TRunnerKind = (rkNONE, rkPROGRAM, rkINTERPRETER);

const
  MaxHash = 9973; //199; //99991;
  PaxSignatureLength = 19;
  MaxInsertPoints = 30;

{$IFDEF ARC}
type
  TPaxSignature = array[0..PaxSignatureLength] of Byte;
  ShortString = array[0..255] of Byte;
  PShortString = ^ShortString;

function Length(const S: ShortString): Integer; overload;
function Length(const S: String): Integer; overload;

const
  strPaxSignature: TPaxSignature = (19, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                    11, 12, 13, 14, 15, 16, 17, 18, 19);
{$ELSE}
const
  strPaxSignature = 'This is a pax class';
type
  TPaxSignature = String[PaxSignatureLength];
{$ENDIF}

type
{$IFDEF UNIC}
  UnicString = UnicodeString;
{$ELSE}
  UnicString = WideString;
{$ENDIF}

{$IFDEF PAX64}
  IntPax = Int64;
{$ELSE}
  IntPax = Integer;
{$ENDIF}
  PIntPax = ^IntPax;

  PUnicString = ^UnicString;

  TPaxLang = (lngPascal, lngC, lngBasic, lngJS);

  TIPoint = record
    Line, Indent: Integer;
  end;

  TParamMode = (parNone, parVar, parOut, parConst);

var
  IsDump: Boolean = false;
  DUMP_PATH: String = '';
{$IFDEF GENERICS}
  GENERICS_ALLOWED: Boolean = true;
{$ELSE}
  GENERICS_ALLOWED: Boolean = false;
{$ENDIF}
  Id_TObject_ClassName: Integer = 0;

  typeNATIVEINT: Integer;

const
  IdsSet = [Ord('a')..Ord('z'), Ord('A')..Ord('Z'), Ord('0')..Ord('9'), Ord('_')];
  WhiteSpaces = [32, 8, 9, 11, 12];

  ANONYMOUS_METHOD_NAME =  '__ANONYM_MTD_';
  ANONYMOUS_CLASS_PREFIX = '__ANONYM_CLS_';
  ANONYMOUS_OBJECT_PREFIX = '__ANONYM_OBJ_';

  EXTRA_KEYWORD = '__extra__';

  PAX_SEH = 17177;

  VARIANT_SIZE = SizeOf(Variant);

  StrExprResult = '__result';
  StrOuterThis = '_outer_this_';  // keeps instance of outer class

  MAX_COMMENTED_TOKENS: Integer = 7;

  MAX_INTERFACE_IMPLEMENT_METHODS = 1000;

  PM_BYVAL = 0;
  PM_BYREF = 1;
  PM_CONST = 2;
  PM_OUT = 3;

  PCU_FILE_EXT = 'PCU';

  PRM_FILE_EXT = 'PRM';
  PRR_FILE_EXT = 'PRR';

  LOC_FILE_EXT = 'LOC';
  LOR_FILE_EXT = 'LOR';

  SLF_FILE_EXT = 'SLF';
  FLD_FILE_EXT = 'FLD';

  MAGIC_INITIALIZATION_JMP_COUNT = 5;
  MAGIC_FINALIZATION_JMP_COUNT = 7;

  TAG_DISCARD_VIRTUAL_CALL = 1;
  TAG_DISCARD_STMT = 2;

  DummyName = '__DummyName';
  PARAMS_DELIMITER = '-';

  DUMMYPROC_PREFIX = 'DummyProc_';

  READ_PREFIX = '_Read_';
  WRITE_PREFIX = '_Write_';

  IntMaxArgs = 20;

  ccSTDCALL = 1;
  ccREGISTER = 2;
  ccCDECL = 3;
  ccPASCAL = 4;
  ccSAFECALL = 5;
  ccMSFASTCALL = 6;
  cc64 = 7;

  typeVOID = 1;
  typeBOOLEAN = 2;
  typeBYTE = 3;
{$IFNDEF PAXARM}
  typeANSICHAR = 4;
  typeANSISTRING = 5;
{$ENDIF}
  typeWORD = 6;
  typeINTEGER = 7;
  typeDOUBLE = 8;
  typePOINTER = 9;
  typeRECORD = 10;
  typeARRAY = 11;
  typeALIAS = 12;
  typeENUM = 13;
  typePROC = 14;
  typeSET = 15;
{$IFNDEF PAXARM}
  typeSHORTSTRING = 16;
{$ENDIF}
  typeSINGLE = 17;
  typeEXTENDED = 18;
  typeCLASS = 19;
  typeCLASSREF = 20;
  typeWIDECHAR = 21;
{$IFNDEF PAXARM}
  typeWIDESTRING = 22;
{$ENDIF}
  typeVARIANT = 23;
  typeDYNARRAY = 24;
  typeINT64 = 25;
  typeINTERFACE = 26;
  typeCARDINAL = 27;
  typeEVENT = 28;
  typeCURRENCY = 29;
  typeSMALLINT = 30;
  typeSHORTINT = 31;
  typeWORDBOOL = 32;
  typeLONGBOOL = 33;
  typeBYTEBOOL = 34;
  typeOLEVARIANT = 35;
  typeUNICSTRING = 36;
  typeOPENARRAY = 37;
  typeTYPEPARAM = 38;
  typeUINT64 = 39;
  typeVOBJECT = 40;
  typeHELPER = 41;
{$IFNDEF PAXARM}
  typePANSICHAR = 49;
{$ENDIF}
  typePVOID = 50;
  typePWIDECHAR = 51;

{$IFDEF UNIC}
  typeSTRING = typeUNICSTRING;
  typeCHAR = typeWIDECHAR;
  typePCHAR = typePWIDECHAR;
{$ELSE}
  typeSTRING = typeANSISTRING;
  typeCHAR = typeANSICHAR;
  typePCHAR = typePANSICHAR;
{$ENDIF}

  CURR_MUL = 10000;

  rmRUN = 0;
  rmTRACE_INTO = 1;
  rmSTEP_OVER = 2;
  rmRUN_TO_CURSOR = 3;
  rmNEXT_SOURCE_LINE = 4;

  PASCAL_LANGUAGE = 0;
  C_LANGUAGE = 1;
  BASIC_LANGUAGE = 2;
  JS_LANGUAGE = 3;
  JAVA_LANGUAGE = 4;

  CHAR_EOF = #255;
  CHAR_REMOVE = #254;
  CHAR_AP  = '''';
  CHAR_DOUBLE_AP  = '"';

  cmNONE = 0;
  cmVIRTUAL = 1;
  cmOVERRIDE = 2;
  cmDYNAMIC = 3;
  cmSTATIC = 4;

  _NOREG = 0;
  _EAX = _NOREG + 1;
  _ECX = _NOREG + 2;
  _EDX = _NOREG + 3;
  _EBX = _NOREG + 4;
  _ESP = _NOREG + 5;
  _EBP = _NOREG + 6;
  _ESI = _NOREG + 7;
  _EDI = _NOREG + 8;
  _R8 = _NOREG + 9;
  _R9 = _NOREG + 10;
  _R10 = _NOREG + 11;
  _R11 = _NOREG + 12;
  _R12 = _NOREG + 13;
  _R13 = _NOREG + 14;
  _R14 = _NOREG + 15;
  _R15 = _NOREG + 16;

  R32 = [_EAX.._EDI];
  R64 = [_R8.._R15];

  _XMM0 = 100;
  _XMM1 = 101;
  _XMM2 = 102;
  _XMM3 = 103;
  _XMM4 = 104;

  CommonRegisters = [_EAX.._EBX, _R8, _R9];

  strExtraPascalUnit = '__extra_pascal__';
  strExtraBasicUnit = '__extra_basic__';
  strExtraJavaUnit = '__extra_java__';

  strPascalLanguage = 'Pascal';
  strPascalNamespace = 'PascalNamespace';

  strBasicLanguage = 'Basic';
  strBasicNamespace = 'BasicNamespace';

  strJavaScriptLanguage = 'JavaScript';
  strJavaScriptNamespace = 'JavaScriptNamespace';
  strJavaScriptTempNamespace = 'JavaScriptTempNamespace';

  strWrite = 'write';
  strWriteln = 'writeln';
  strGetTickCount = 'GetTickCount';
  strUnassigned = 'Unassigned';

//------------------ Dynamic array support routines ----------------------------

  _strGetOLEProperty = '_GetOleProperty';
  _strSetOLEProperty = '_SetOleProperty';

//------ JS --------

  strCreate = 'Create';
  strInternalFuncAddr = 'InternalFuncAddr';
  strInternalCreate = 'InternalCreate';
  strInternalLength = 'InternalLength';
  strInternalCall = 'InternalCall';
  strInternalConstructor = 'InternalConstructor';
  str__this = '__this';
  strProgram = 'aprogram';

  strInternalFWArrayCreate = '__FWArrayCreate';

{$IFDEF MACOS32}
  MEM_COMMIT = 0;
  PAGE_EXECUTE_READWRITE = 0;
{$ENDIF}

{$IFDEF FPC}
const
{$ELSE}
resourcestring
{$ENDIF}

  errInternalError = 'Internal error';
  errInternalErrorMethodIndex = 'Internal error - method index is not set correctly';
  errWrongCall = 'Wrong call - instance is not created.';

  errEmptyModuleList = 'Empty module list. Use AddModule to add modules.';
  errUnregisteredLanguage = 'Unregistered language %s';
  errSyntaxError = 'Syntax error';
  errUnterminatedString = 'Unterminated string';
  errTokenExpected = '"%s" expected but "%s" found';
  errIdentifierExpected = 'Identifier expected but "%s" found';
  errIdentifierExpectedNoArgs = 'Identifier expected';
  errLabelExpected = 'Label expected';
  errPCharLiteralExpected = 'PChar literal expected';
  errIncompatibleTypesNoArgs = 'Incompatible types';
  errIncompatibleTypes = 'Incompatible types "%s" and "%s"';
  errOperatorNotApplicableToThisOperandType = 'Operator not applicable to "%s" type';
  errOperatorNotApplicableToThisOperandTypeNoArgs = 'Operator not applicable to this type';
  errLeftSideCannotBeAssignedTo = 'Left side cannot be assigned to';
  errUndeclaredIdentifier = 'Undeclared identifier "%s"';
  errUndeclaredType = 'Undeclared type "%s"';
  errRedeclaredIdentifier = 'Redeclared identifier "%s"';
  errModuleNotFound = 'Module "%s" not found';
  errLanguageNotRegistered = 'Language "%s" not registered';
  errFileNotFound = 'File "%s" not found';
  errTooManyActualParameters = 'Too many actual parameters';
  errNotEnoughActualParameters = 'Not enough actual parameters';
  errLabelIsDeclaredButNeverUsed = 'Label "%s" is declared but never used';
  errThereIsNoOverloaded = 'There is no overloaded version of "%s" that can be called with these arguments';
  errAmbiguousOverloadedCall = 'Ambiguous overloaded call to "%s"';
  errDefaultParameterMustBeByValueOrConst = 'Default parameter "%s" must be by-value or const';
  errParameterNotAllowedHere = 'Parameter "%s" not allowed here due to default value';
  errConstantExpressionExpected = 'Constant expression expected';
  errCannotInitializeLocalVariables = 'Cannot initialize local variables';
  errRecordRequired = 'Record required';
  errClassTypeRequired = 'Class type required';
  errArrayTypeRequired = 'Array type required';
  errSetTypeRequired = 'Set type required';
  errOrdinalTypeRequired = 'Ordinal type required';
  errIllegalTypeInWriteStatememt = 'Illegal type in Write/Writeln statement';
  errConstantExpressionViolatesSubrangeBounds = 'Constant expression violates subrange bounds';
  errLowBoundExceedsHighBound = 'Low bound exceeds high bound';
  errPACKEDNotAllowedHere = 'PACKED not allowed here';
  errVariableRequired = 'Variable required';
  errLineTerminatorExpected = 'Line terminator expected';
  errStatementTerminatorExpected = 'Statement terminator expected';
  errTypeOfExpressionMustBe = 'Type of expression must be %s';
  errBreakOrContinueOutsideOfLoop = 'BREAK or CONTINUE outside of loop';
  errNextControlVariableDoesNotMatchForLoopControlVariable = 'Next control variable does not match For loop control variable %s';
  errDivisionByZero = 'Division by zero';
  errCannotApplyCall = 'Cannot apply () to "%s"';
  errUnsatisfiedForwardOrExternalDeclaration = 'Unsatisfied forward or external declaration: "%s"';
  errCircularUnitReference = 'Circular unit reference to "%s"';
  errTypesOfActualAndFormalVarParametersMustBeIdentical = 'Types of actual and formal var parameters must be identical';
  errClassNotFound = 'Class "%s" not found';
  errTypeNotFound = 'Type "%s" not found';
  errThisFormOfMethodCanOnlyAllowedForClassMethod = 'This form of method can only allowed for class methods';
  errCannotAssignToReadOnlyProperty = 'Cannot assign to read-only property';
  errCannotReadWriteOnlyProperty = 'Cannot read a write-only property';
  errClassDoesNotHaveDefaultProperty = 'Class "%s" does not have a default property';
  errCannotRegisterClass = 'Cannot register class "%s"';
  errIMPORT_ActiveX = 'Error. You have to add "IMPORT_ActiveX.pas" to your project';
  errIntegerOverflow = 'Integer overflow';
  errRangeCheckError = 'Range check error';
  errThisFormOfMethodCallOnlyAllowedInMethodsOfDerivedTypes = 'This form of method call only allowed in methods of derived types';
  errYouCannotUseIncOnProperties = 'You can''t use Inc on properties because it modifies the parameter.';
  errYouCannotUseDecOnProperties = 'You can''t use Dec on properties because it modifies the parameter.';
  errDefaultValueRequired = 'Default value required for "%s".';
  errClassIsNotRegistered = 'Class "%s" is not registered.';
  errAbstractMethodsMustBeVirtual = 'Abstract methods must be virtual.';
  errAbstractMethodCall = 'Abstract method call.';
  errDebugModeIsRequred = 'Debug mode is required.';
  errProgramIsNotPaused = 'Program is not paused.';
  errInvalidId = 'Invalid id "%d"';
  errInvalidIndex = 'Invalid index "%d"';
  errInvalidValue = 'Invalid value';
  errMemoryNotInitialized = 'Memory not initialized';
  errNotValidObject = 'Not a valid object';
  errDllNotFound = 'This script has failed to start because %s was not found.';
  errProcNotFound = 'The procedure entry point %s could not be located in the' +
   ' dynamic link library %s.';
  errProcNotFoundInPCU = 'The procedure entry point %s could not be located in the' +
   ' pcu %s.';
  errEntryPointNotFoundInPCU = 'The entry point %s could not be located in the' +
   ' pcu %s.';
  errConstructorNotFoundInClass = 'Constructor not found in class "%s"';
  errPropertyIsForbidden = 'Property "%s" of class "%s" is forbidden.';
  errHostMemberIsNotDefined = 'Host member "%s" is not defined.';
  errCannotRegisterHostMember = 'Cannot register host member "%s".';
  errInvalidAlignmentValue = '"%d" is invalid alignment value. Must be 1, 2, 4 or 8.';
  errInvalidCompilerDirective = 'Invalid compiler directive "%s".';
  errTooManyNestedCaseBlocks = 'Too many nested case blocks.';
  errProtectionLevel = ' "%s" is inaccessible due to its protection level.';
  errMissingENDIFdirective = 'Missing ENDIF directive.';
  errPropertyDoesNotExistsInTheBaseClass = 'Property "%s" does not exist in the base class.';
  errMethodDoesNotExistsInTheBaseClass = 'Method "%s" does not exist in the base class.';
  errIncorrectValue = 'Incorrect value.';
  errUnresolvedClassReference = 'Unresolved class reference "%s".';
  errUnresolvedAddress = 'Unresolved address "%s".';
  errIncorrectStreamVersion = 'Incorrect stream version.';
  errIncorrectCompiledScriptVersion = 'Incorrect compiled script version';
  errInvalidVariantType = 'Variant type not safe on debugger or invalid variant type';
  errClassTypeExpected = 'Class type expected';
  errUndeclaredInterface = 'Undeclared interface "%s"';
  errDeclarationDiffersFromPreviousDeclaration = 'Declaration of "%s" differs from previous declaration';
  errTheCallOfInheritedConstructorIsMandatory = 'The Call of inherited constructor is mandatory';
  errFieldDefinitionNotAllowedAfter = 'Field definition not allowed after methods or properties.';
  errKeywordNotAllowedInInterfaceDeclaration = '"%s" not allowed in interface declaration.';
  errUnknownDirective = 'Unknown directive "%s"';
  errUnknownLanguage = 'Unknown language "%s"';
  errCannotOverrideStaticMethod = 'Cannot override a static method';
  errNoDefinitionForAbstractMethodAllowed = 'No definition for abstract method "%s" allowed.';
  errWrongRegistration = 'Wrong registration';
  errConstantObjectCannotBePassedAsVarParameter = 'Constant object cannot be passed as var parameter';
  errError = 'error';
  errCanceled = 'Canceled';
  errPropertyInaccessibleHere = 'Property "%s" inaccessible here';

  STooManyParams = 'Dispatch methods do not support more than 64 parameters';
  errLabelNotFound = 'Label not found';
  errRoutineNotFound = 'Routine "%s" not found';
  errPropertyNotFound = 'Property "%s" not found';
  errUnknownStreamFormat = 'Unknown stream format';
  errTypeHasNotEnumerator = 'Type "%s" has not enumerator';
  errParameterCannotHaveDefaultValue = 'Parameters of this type cannot have default values';

  errE2015 = 'Operator not applicable to this operand type';
  errE2376 = 'STATIC can only be used on non-virtual class methods';
  errE2379 = 'Virtual methods not allowed in record types';
  errE2393 = 'Invalid operator declaration';
  errE2398 = 'Class methods in record types must be static';
  errE2517 = 'Operator "%s" must take "%d" parameter';

  errE2072 = 'Number of elements "%d" differs from declaration "%d" ';

  errOverloadExpected = 'Overloaded procedure "%s"  must be marked with "overload" directive';

  errCannotImport = 'Cannot import "%s"';
  errTypeWasNotImported = 'Type was not imported for "%s"';

  errIncorrectCustomDataSize = 'Incorrect size of custom data';

  strNBounds = 'NBounds';
  strElFinalTypeId = 'ElFinTypeId';
  strElTypeId = 'ElTypeId';
  strElSize = 'ElSize';

  pascal_Implicit = 'Implicit';
  pascal_Explicit = 'Explicit';
  pascal_Add = 'Add';
  pascal_Divide = 'Divide';
  pascal_IntDivide = 'IntDivide';
  pascal_Modulus = 'Modulus';
  pascal_Multiply = 'Multiply';
  pascal_Subtract = 'Subtract';
  pascal_Negative = 'Negative';
  pascal_Positive = 'Positive';
  pascal_LogicalNot = 'LogicalNot';
  pascal_LeftShift = 'LeftShift';
  pascal_RightShift = 'RightShift';
  pascal_LogicalAnd = 'LogicalAnd';
  pascal_LogicalOr = 'LogicalOr';
  pascal_LogicalXor = 'LogicalXor';
  pascal_LessThan = 'LessThan';
  pascal_LessThanOrEqual = 'LessThanOrEqual';
  pascal_GreaterThan = 'GreaterThan';
  pascal_GreaterThanOrEqual = 'GreaterThanOrEqual';
  pascal_Equal = 'Equal';
  pascal_NotEqual = 'NotEqual';
  pascal_Inc = 'Inc';
  pascal_Dec = 'Inc';

  gen_Implicit = '&op_Implicit';
  gen_Explicit = '&op_Explicit';
  gen_Add = '&op_Addition';
  gen_Divide = '&op_Division';
  gen_IntDivide = '&op_IntDivide';
  gen_Modulus = '&op_Modulus';
  gen_Multiply = '&op_Multiply';
  gen_Subtract = '&op_Subtraction';
  gen_Negative = '&op_UnaryNegation';
  gen_Positive = '&op_UnaryPlus';
  gen_LogicalNot = '&op_LogicalNot';
  gen_LeftShift = '&op_LeftShift';
  gen_RightShift = '&op_RightShift';
  gen_LogicalAnd = '&op_LogicalAnd';
  gen_LogicalOr = '&op_LogicalOr';
  gen_LogicalXor = '&op_LogicalXor';
  gen_LessThan = '&op_LessThan';
  gen_LessThanOrEqual = '&op_LessThanOrEqual';
  gen_GreaterThan = '&op_GreaterThan';
  gen_GreaterThanOrEqual = '&op_GreaterThanOrEqual';
  gen_Equal = '&op_Equality';
  gen_NotEqual = '&op_Inequality';
  gen_Inc = '&op_Increment';
  gen_Dec = '&op_Decrement';

//Basic
  errThisWayOfCallIsAllowedOnlyForProcedures = 'This way of call is allowed only for procedures';

//JS
  errCannotConvertToFunctionObject = 'Cannot convert to Function Object';
//  errJSUnitHasBeenNotIncluded = 'JavaScript unit has been not included in current project';
  errCannotConvertToJS_Object = 'Cannot convert to JavaScript object';
  errReferenceError = 'Reference error';

  errNotImplementedYet = 'Not implemented yet';

  wrnNeverUsedIn = 'Variable "%s" is declared but never used in "%s"';
  wrnNeverUsed = 'Variable "%s" is declared but never used';
  wrnPrivateNeverUsed = 'Private symbol "%s" is declared but never used';
  wrnNotInit = 'Variable "%s" might not have been initialized';
  wrnReturnValue = 'Return value of function "%s" might be undefined';

  errExplicitTypeDeclarationRequired = 'Explicit declaration of type required';
  errOverridenMethodIsFinal = 'Overriden method "%s" is final.';
  errCannotInheritFromFinalClass = 'Cannot inherit from final class "%s".';
  errCannotInheritFromSealedClass = 'Cannot inherit from sealed class "%s".';

  errTypeParameterNotAllowed = 'Type parameters not allowed on this type';
  errTypeIsNotValidConstraint = 'Type "%s" is not a valid constraint';
  errInvalidTypeCast = 'Invalid typecast';
  errInvalidSet = 'Sets may have at most 256 elements';

  errVirtualObjectMethodCallEventNotAssigned = 'OnVirtualObjectMethodCall event is not assigned';
  errVirtualObjectPutPropertyEventNotAssigned = 'OnVirtualObjectPutProperty event is not assigned';

  errProtectedNotValid = 'PROTECTED section valid only in class types';
  errTryExceptNotImplemented = 'Try-except statement is not implemented for this platform.'#13#10 +
       'Use TPaxInterpreter instead of TPaxProgram to provide exception handling.';
  errRaiseNotImplemented = 'Raise statement is not implemented for this platform.#13#10' +
       'Use TPaxInterpreter instead of TPaxProgram to provide exception handling.';

  // C
  errDeclarationSyntaxError = 'Declaration syntax error';
  errUnknownPreprocessorDirective = 'Unknown preprocessor directive';
  errParameterNameMissing = 'Parameter "%d" name missing';

implementation

{$IFDEF ARC}

function Length(const S: ShortString): Integer;
begin
  result := S[0];
end;

function Length(const S: String): Integer; overload;
begin
  result := System.Length(S);
end;

{$ENDIF}

end.

