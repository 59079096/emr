unit IMPORT_SysUtils;
interface
uses
  SysUtils,
  PaxRegister,
  PaxCompiler;

procedure Register_SysUtils;

implementation

function _Format(const S: string; const Args: array of const): string;
begin
  result := Format(S, Args);
end;

// Exception -------------------------------------------------------------------

function Exception_GetMessage(Self: Exception): String; 
begin
  result := Self.Message;
end;

procedure Exception_SetMessage(Self: Exception; const Value: String);
begin
  Self.Message := Value;
end;

function Exception_GetHelpContext(Self: Exception): Integer;
begin
  result := Self.HelpContext;
end;

procedure Exception_SetHelpContext(Self: Exception; Value: Integer);
begin
  Self.HelpContext := Value;
end;

procedure Register_SysUtils;
var
  H, G: Integer;
begin
  H := RegisterNamespace(0, 'SysUtils');

  RegisterRTTIType(H, TypeInfo(TReplaceFlags));

  G := RegisterRecordType(H, 'TTimeStamp');
  RegisterRecordTypeField(G, 'Time', _typeINTEGER);
  RegisterRecordTypeField(G, 'Date', _typeINTEGER);

  G := RegisterRecordType(H, 'TSystemTime');
  RegisterRecordTypeField(G, 'wYear', _typeWORD);
  RegisterRecordTypeField(G, 'wMonth', _typeWORD);
  RegisterRecordTypeField(G, 'wDayOfWeek', _typeWORD);
  RegisterRecordTypeField(G, 'wDay', _typeWORD);
  RegisterRecordTypeField(G, 'wHour', _typeWORD);
  RegisterRecordTypeField(G, 'wMinute', _typeWORD);
  RegisterRecordTypeField(G, 'wSecond', _typeWORD);
  RegisterRecordTypeField(G, 'wMilliSecond', _typeWORD);

{ File open modes }

  RegisterConstant(H, 'fmOpenRead', fmOpenRead);
  RegisterConstant(H, 'fmOpenWrite', fmOpenWrite);
  RegisterConstant(H, 'fmOpenReadWrite', fmOpenReadWrite);
  RegisterConstant(H, 'fmShareExclusive', fmShareExclusive);
  RegisterConstant(H, 'fmShareDenyWrite', fmShareDenyWrite);
  RegisterConstant(H, 'fmShareDenyNone', fmShareDenyNone);
{$IFNDEF MACOS}
  RegisterConstant(H, 'fmShareCompat', fmShareCompat);
  RegisterConstant(H, 'fmShareDenyRead', fmShareDenyRead);
{$ENDIF}

{ File attribute constants }

  RegisterConstant(H, 'faReadOnly', faReadOnly);
  RegisterConstant(H, 'faHidden', faHidden);
  RegisterConstant(H, 'faSysFile', faSysFile);
  RegisterConstant(H, 'faVolumeID', faVolumeID);
  RegisterConstant(H, 'faDirectory', faDirectory);
  RegisterConstant(H, 'faArchive', faArchive);
  RegisterConstant(H, 'faAnyFile', faAnyFile);

{ File mode magic numbers }

  RegisterConstant(H, 'fmClosed', fmClosed);
  RegisterConstant(H, 'fmInput', fmInput);
  RegisterConstant(H, 'fmOutput', fmOutput);
  RegisterConstant(H, 'fmInOut', fmInOut);

{ Seconds and milliseconds per day }

  RegisterConstant(H, 'SecsPerDay', SecsPerDay);
  RegisterConstant(H, 'MSecsPerDay', MSecsPerDay);

{ Days between 1/1/0001 and 12/31/1899 }

  RegisterConstant(H, 'DateDelta', DateDelta);

  RegisterHeader(H, 'function UpperCase(const S: string): string;', @UpperCase);
  RegisterHeader(H, 'function LowerCase(const S: string): string;', @LowerCase);
  RegisterHeader(H, 'function CompareStr(const S1, S2: string): Integer;', @CompareStr);
  RegisterHeader(H, 'function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;', @CompareMem);
  RegisterHeader(H, 'function CompareText(const S1, S2: string): Integer;', @CompareText);
  RegisterHeader(H, 'function SameText(const S1, S2: string): Boolean;', @SameText);
  RegisterHeader(H, 'function AnsiUpperCase(const S: string): string;', @AnsiUpperCase);
  RegisterHeader(H, 'function AnsiLowerCase(const S: string): string;', @AnsiLowerCase);
  RegisterHeader(H, 'function AnsiCompareStr(const S1, S2: string): Integer;', @AnsiCompareStr);
  RegisterHeader(H, 'function AnsiSameStr(const S1, S2: string): Boolean;', @AnsiSameStr);
  RegisterHeader(H, 'function AnsiCompareText(const S1, S2: string): Integer;', @AnsiCompareText);
  RegisterHeader(H, 'function AnsiSameText(const S1, S2: string): Boolean;', @AnsiSameText);
  RegisterHeader(H, 'function AnsiStrComp(S1, S2: PChar): Integer;', @AnsiStrComp);
  RegisterHeader(H, 'function AnsiStrIComp(S1, S2: PChar): Integer;', @AnsiStrIComp);
  RegisterHeader(H, 'function AnsiStrLComp(S1, S2: PChar; MaxLen: Cardinal): Integer;', @AnsiStrLComp);
  RegisterHeader(H, 'function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;', @AnsiStrLIComp);
  RegisterHeader(H, 'function AnsiStrLower(Str: PChar): PChar;', @AnsiStrLower);
  RegisterHeader(H, 'function AnsiStrUpper(Str: PChar): PChar;', @AnsiStrUpper);
  RegisterHeader(H, 'function AnsiLastChar(const S: string): PChar;', @AnsiLastChar);
  RegisterHeader(H, 'function AnsiStrLastChar(P: PChar): PChar;', @AnsiStrLastChar);
  RegisterHeader(H, 'function Trim(const S: string): string;', @Trim);
  RegisterHeader(H, 'function TrimLeft(const S: string): string;', @TrimLeft);
  RegisterHeader(H, 'function TrimRight(const S: string): string;', @TrimRight);
  RegisterHeader(H, 'function QuotedStr(const S: string): string;', @QuotedStr);
  RegisterHeader(H, 'function AnsiQuotedStr(const S: string; Quote: Char): string;', @AnsiQuotedStr);
  RegisterHeader(H, 'function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string;', @AnsiExtractQuotedStr);
  RegisterHeader(H, 'function AdjustLineBreaks(const S: string): string;', @AdjustLineBreaks);
  RegisterHeader(H, 'function IsValidIdent(const Ident: string): Boolean;', @IsValidIdent);
  RegisterHeader(H, 'function IntToStr(Value: Integer): string;', @IntToStr);
  RegisterHeader(H, 'function IntToHex(Value: Integer; Digits: Integer): string;', @IntToHex);
  RegisterHeader(H, 'function StrToInt(const S: string): Integer;', @StrToInt);
  RegisterHeader(H, 'function StrToIntDef(const S: string; Default: Integer): Integer;', @StrToIntDef);
  RegisterHeader(H, 'function LoadStr(Ident: Integer): string;', @LoadStr);
  RegisterHeader(H, 'function FileOpen(const FileName: string; Mode: LongWord): Integer;', @FileOpen);
  RegisterHeader(H, 'function FileCreate(const FileName: string): Integer;', @FileCreate);
  RegisterHeader(H, 'function FileRead(Handle: Integer; var Buffer; Count: LongWord): Integer;', @FileRead);
  RegisterHeader(H, 'function FileWrite(Handle: Integer; const Buffer; Count: LongWord): Integer;', @FileWrite);
  RegisterHeader(H, 'function FileSeek(Handle, Offset, Origin: Integer): Integer;', @FileSeek);
  RegisterHeader(H, 'procedure FileClose(Handle: Integer);', @FileClose);
  RegisterHeader(H, 'function FileAge(const FileName: string): Integer;', @FileAge);
  RegisterHeader(H, 'function FileExists(const FileName: string): Boolean;', @FileExists);
  RegisterHeader(H, 'function FileGetDate(Handle: Integer): Integer;', @FileGetDate);
  RegisterHeader(H, 'function FileSetDate(Handle: Integer; Age: Integer): Integer;', @FileSetDate);
  RegisterHeader(H, 'function FileGetAttr(const FileName: string): Integer;', @FileGetAttr);
  RegisterHeader(H, 'function DeleteFile(const FileName: string): Boolean;', @DeleteFile);
  RegisterHeader(H, 'function RenameFile(const OldName, NewName: string): Boolean;', @RenameFile);
  RegisterHeader(H, 'function ChangeFileExt(const FileName, Extension: string): string;', @ChangeFileExt);
  RegisterHeader(H, 'function ExtractFilePath(const FileName: string): string;', @ExtractFilePath);
  RegisterHeader(H, 'function ExtractFileDir(const FileName: string): string;', @ExtractFileDir);
  RegisterHeader(H, 'function ExtractFileDrive(const FileName: string): string;', @ExtractFileDrive);
  RegisterHeader(H, 'function ExtractFileName(const FileName: string): string;', @ExtractFileName);
  RegisterHeader(H, 'function ExtractFileExt(const FileName: string): string;', @ExtractFileExt);
  RegisterHeader(H, 'function ExpandFileName(const FileName: string): string;', @ExpandFileName);
  RegisterHeader(H, 'function ExpandUNCFileName(const FileName: string): string;', @ExpandUNCFileName);
  RegisterHeader(H, 'function ExtractRelativePath(const BaseName, DestName: string): string;', @ExtractRelativePath);
  RegisterHeader(H, 'function FileSearch(const Name, DirList: string): string;', @FileSearch);
  RegisterHeader(H, 'function GetCurrentDir: string;', @GetCurrentDir);
  RegisterHeader(H, 'function SetCurrentDir(const Dir: string): Boolean;', @SetCurrentDir);
  RegisterHeader(H, 'function CreateDir(const Dir: string): Boolean;', @CreateDir);
  RegisterHeader(H, 'function RemoveDir(const Dir: string): Boolean;', @RemoveDir);

  RegisterHeader(H, 'function StrLen(const Str: PChar): Cardinal;', @StrLen);
  RegisterHeader(H, 'function StrEnd(const Str: PChar): PChar;', @StrEnd);
  RegisterHeader(H, 'function StrMove(Dest: PChar; const Source: PChar; Count: Cardinal): PChar;', @StrMove);
  RegisterHeader(H, 'function StrCopy(Dest: PChar; const Source: PChar): PChar;', @StrCopy);
  RegisterHeader(H, 'function StrECopy(Dest:PChar; const Source: PChar): PChar;', @StrECopy);
  RegisterHeader(H, 'function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar;', @StrLCopy);
  RegisterHeader(H, 'function StrPCopy(Dest: PChar; const Source: string): PChar;', @StrPCopy);
  RegisterHeader(H, 'function StrPLCopy(Dest: PChar; const Source: string; MaxLen: Cardinal): PChar;', @StrPLCopy);
  RegisterHeader(H, 'function StrCat(Dest: PChar; const Source: PChar): PChar;', @StrCat);
  RegisterHeader(H, 'function StrLCat(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar;', @StrLCat);
  RegisterHeader(H, 'function StrComp(const Str1, Str2: PChar): Integer;', @StrComp);
  RegisterHeader(H, 'function StrIComp(const Str1, Str2: PChar): Integer;', @StrIComp);
  RegisterHeader(H, 'function StrLComp(const Str1, Str2: PChar; MaxLen: Cardinal): Integer;', @StrLComp);
  RegisterHeader(H, 'function StrLIComp(const Str1, Str2: PChar; MaxLen: Cardinal): Integer;', @StrLIComp);
  RegisterHeader(H, 'function StrScan(const Str: PChar; Chr: Char): PChar;', @StrScan);
  RegisterHeader(H, 'function StrRScan(const Str: PChar; Chr: Char): PChar;', @StrRScan);
  RegisterHeader(H, 'function StrPos(const Str1, Str2: PChar): PChar;', @StrPos);
  RegisterHeader(H, 'function StrUpper(Str: PChar): PChar;', @StrUpper);
  RegisterHeader(H, 'function StrLower(Str: PChar): PChar;', @StrLower);
  RegisterHeader(H, 'function StrPas(const Str: PChar): string;', @StrPas);
  RegisterHeader(H, 'function StrAlloc(Size: Cardinal): PChar;', @StrAlloc);
  RegisterHeader(H, 'function StrBufSize(const Str: PChar): Cardinal;', @StrBufSize);
  RegisterHeader(H, 'function StrNew(const Str: PChar): PChar;', @StrNew);
  RegisterHeader(H, 'procedure StrDispose(Str: PChar);', @StrDispose);

  RegisterHeader(H, 'function FloatToStr(Value: Extended): string;', @FloatToStr);
  RegisterHeader(H, 'function FormatFloat(const Format: string; Value: Extended): string;', @FormatFloat);
  RegisterHeader(H, 'function StrToFloat(const S: string): Extended;', @StrToFloat);
  RegisterHeader(H, 'function StrToFloatDef(const S: string; const Default: Extended): Extended;', @StrToFloatDef);

  RegisterHeader(H, 'function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;', @DateTimeToTimeStamp);
  RegisterHeader(H, 'function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;', @TimeStampToDateTime);
  RegisterHeader(H, 'function EncodeDate(Year, Month, Day: Word): TDateTime;', @EncodeDate);
  RegisterHeader(H, 'function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;', @EncodeTime);
  RegisterHeader(H, 'procedure DecodeDate(Date: TDateTime; var Year, Month, Day: Word);', @DecodeDate);
  RegisterHeader(H, 'procedure DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);', @DecodeTime);
  RegisterHeader(H, 'function DayOfWeek(Date: TDateTime): Integer;', @DayOfWeek);
  RegisterHeader(H, 'function Date: TDateTime;', @Date);
  RegisterHeader(H, 'function Time: TDateTime;', @Time);
  RegisterHeader(H, 'function Now: TDateTime;', @Now);
  RegisterHeader(H, 'function IncMonth(const Date: TDateTime; NumberOfMonths: Integer): TDateTime;', @IncMonth);
  RegisterHeader(H, 'procedure ReplaceTime(var DateTime: TDateTime; const NewTime: TDateTime);', @ReplaceTime);
  RegisterHeader(H, 'procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime);', @ReplaceDate);
  RegisterHeader(H, 'function IsLeapYear(Year: Word): Boolean;', @IsLeapYear);
  RegisterHeader(H, 'function DateToStr(Date: TDateTime): string;', @DateToStr);
  RegisterHeader(H, 'function TimeToStr(Time: TDateTime): string;', @TimeToStr);
  RegisterHeader(H, 'function DateTimeToStr(DateTime: TDateTime): string;', @DateTimeToStr);
  RegisterHeader(H, 'function StrToDate(const S: string): TDateTime;', @StrToDate);
  RegisterHeader(H, 'function StrToTime(const S: string): TDateTime;', @StrToTime);
  RegisterHeader(H, 'function StrToDateTime(const S: string): TDateTime;', @StrToDateTime);

  RegisterHeader(H, 'function FormatDateTime(const Format: string; DateTime: TDateTime): string;', @FormatDateTime);
  RegisterHeader(H, 'procedure GetFormatSettings;', @GetFormatSettings);

  RegisterHeader(H, 'function StringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string;', @StringReplace);

  RegisterHeader(H, 'procedure FreeAndNil(var Obj);', @FreeAndNil);

  RegisterHeader(H, 'function Format(const S: string; const Args: array of const): string;',
                 @_Format);

{$IFNDEF MACOS}
  RegisterHeader(H, 'function ExtractShortPathName(const FileName: string): string;', @ExtractShortPathName);
  RegisterHeader(H, 'function FileSetAttr(const FileName: string; Attr: Integer): Integer;', @FileSetAttr);
  RegisterHeader(H, 'procedure DateTimeToSystemTime(DateTime: TDateTime; var SystemTime: TSystemTime);', @DateTimeToSystemTime);
  RegisterHeader(H, 'function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;', @SystemTimeToDateTime);
{$ENDIF}

// Exception -------------------------------------------------------------------

  G := RegisterClassType(H, Exception);
  RegisterClassReferenceType(H, 'ExceptClass', G);

  RegisterHeader(G, 'constructor Create(const Msg: string);', @Exception.Create);

  RegisterHeader(G, 'function _GetMessage: String;', @Exception_GetMessage);
  RegisterHeader(G, 'procedure _SetMessage(const Value: String);', @Exception_SetMessage);
  RegisterProperty(G, 'property Message: string read _GetMessage write _SetMessage;');

  RegisterHeader(G, 'function _GetHelpContext: Integer;', @Exception_GetHelpContext);
  RegisterHeader(G, 'procedure _SetHelpContext(Value: Integer);', @Exception_SetHelpContext);
  RegisterProperty(G, 'property HelpContext: Integer read _GetHelpContext write _SetHelpContext;');

// EAbort ----------------------------------------------------------------------

  G := RegisterClassType(H, EAbort);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EAbort.Create);

// EOutOfMemory ----------------------------------------------------------------

  G := RegisterClassType(H, EOutOfMemory);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EOutOfMemory.Create);

// EInOutError -----------------------------------------------------------------

  G := RegisterClassType(H, EInOutError);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EInOutError.Create);
  RegisterClassTypeField(G, 'ErrorCode', _typeINTEGER, Integer(@EInOutError(nil).ErrorCode));

// EExternal -------------------------------------------------------------------

  G := RegisterClassType(H, EExternal);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EExternal.Create);

// EExternalException ----------------------------------------------------------

  G := RegisterClassType(H, EExternalException);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EExternalException.Create);

// EIntError -------------------------------------------------------------------

  G := RegisterClassType(H, EIntError);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EIntError.Create);

// EDivByZero ------------------------------------------------------------------

  G := RegisterClassType(H, EDivByZero);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EDivByZero.Create);

// ERangeError -----------------------------------------------------------------

  G := RegisterClassType(H, ERangeError);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @ERangeError.Create);

// EIntOverflow ----------------------------------------------------------------

  G := RegisterClassType(H, EIntOverflow);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EIntOverflow.Create);

// EMathError ------------------------------------------------------------------

  G := RegisterClassType(H, EMathError);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EMathError.Create);

// EInvalidOp ------------------------------------------------------------------

  G := RegisterClassType(H, EInvalidOp);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EInvalidOp.Create);

// EZeroDivide -----------------------------------------------------------------

  G := RegisterClassType(H, EZeroDivide);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EZeroDivide.Create);

// EOverflow -------------------------------------------------------------------

  G := RegisterClassType(H, EOverflow);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EOverflow.Create);

// EUnderflow ------------------------------------------------------------------

  G := RegisterClassType(H, EUnderflow);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EUnderflow.Create);

// EInvalidPointer -------------------------------------------------------------

  G := RegisterClassType(H, EInvalidPointer);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EInvalidPointer.Create);

// EInvalidCast ----------------------------------------------------------------

  G := RegisterClassType(H, EInvalidCast);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EInvalidCast.Create);

// EConvertError ---------------------------------------------------------------

  G := RegisterClassType(H, EConvertError);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EConvertError.Create);

// EAccessViolation ---------------------------------------------------------------

  G := RegisterClassType(H, EAccessViolation);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EAccessViolation.Create);

// EPrivilege ------------------------------------------------------------------

  G := RegisterClassType(H, EPrivilege);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EPrivilege.Create);

// EStackOverflow ------------------------------------------------------------------

  G := RegisterClassType(H, EStackOverflow);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EStackOverflow.Create);

// EControlC -------------------------------------------------------------------

  G := RegisterClassType(H, EControlC);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EControlC.Create);

// EVariantError ---------------------------------------------------------------

  G := RegisterClassType(H, EVariantError);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EVariantError.Create);

// EPropReadOnly ---------------------------------------------------------------

  G := RegisterClassType(H, EPropReadOnly);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EPropReadOnly.Create);

// EPropWriteOnly --------------------------------------------------------------

  G := RegisterClassType(H, EPropWriteOnly);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EPropWriteOnly.Create);

// EAssertionFailed ------------------------------------------------------------

  G := RegisterClassType(H, EAssertionFailed);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EAssertionFailed.Create);

// EAbstractError --------------------------------------------------------------

  G := RegisterClassType(H, EAbstractError);
  RegisterHeader(G, 'constructor Create(const Msg: string);', @EAbstractError.Create);
end;

end.
