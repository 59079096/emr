unit IMPORT_Classes;
interface
uses
  Classes,
  SysUtils,
  PaxRegister,
  PaxCompiler;

procedure Register_Classes;

implementation


// TList -----------------------------------------------------------------------

function TList_GetCapacity(Self: TList): Integer;
begin
  result := Self.Capacity;
end;

procedure TList_SetCapacity(Self: TList; Value: Integer);
begin
  Self.Capacity := Value;
end;

function TList_GetCount(Self: TList): Integer;
begin
  result := Self.Count;
end;

function TList_GetItem(Self: TList; I: Integer): Pointer;
begin
  result := Self.Items[I];
end;

procedure TList_SetItem(Self: TList; I: Integer; Value: Pointer);
begin
  Self.Items[I] := Value;
end;

{
function TList_GetList(Self: TList): PPointerList;
begin
  result := Self.List;
end;
}

// TBits -----------------------------------------------------------------------

function TBits_GetBit(Self: TBits; I: Integer): Boolean;
begin
  result := Self.Bits[I];
end;

procedure TBits_SetBit(Self: TBits; I: Integer; Value: Boolean);
begin
  Self.Bits[I] := Value;
end;

function TBits_GetSize(Self: TBits): Integer;
begin
  result := Self.Size;
end;

// TCollectionItem -------------------------------------------------------------

function TCollectionItem_GetCollection(Self: TCollectionItem): TCollection;
begin
  result := Self.Collection;
end;

procedure TCollectionItem_SetCollection(Self: TCollectionItem; Value: TCollection);
begin
  Self.Collection := Value;
end;

function TCollectionItem_GetID(Self: TCollectionItem): Integer;
begin
  result := Self.ID;
end;

function TCollectionItem_GetIndex(Self: TCollectionItem): Integer;
begin
  result := Self.Index;
end;

procedure TCollectionItem_SetIndex(Self: TCollectionItem; Value: Integer);
begin
  Self.Index := Value;
end;

function TCollectionItem_GetDisplayName(Self: TCollectionItem): String;
begin
  result := Self.DisplayName;
end;

procedure TCollectionItem_SetDisplayName(Self: TCollectionItem; const Value: String);
begin
  Self.DisplayName := Value;
end;

// TCollection -----------------------------------------------------------------

function TCollection_GetCount(Self: TCollection): Integer;
begin
  result := Self.Count;
end;

function TCollection_GetItemClass(Self: TCollection): TCollectionItemClass;
begin
  result := Self.ItemClass;
end;

function TCollection_GetItem(Self: TCollection; I: Integer): TCollectionItem;
begin
  result := Self.Items[I];
end;

procedure TCollection_SetItem(Self: TCollection; I: Integer; Value: TCollectionItem);
begin
  Self.Items[I] := Value;
end;

// TStrings --------------------------------------------------------------------

function TStrings_GetCapacity(Self: TStrings): Integer;
begin
  result := Self.Capacity;
end;

procedure TStrings_SetCapacity(Self: TStrings; Value: Integer);
begin
  Self.Capacity := Value;
end;

function TStrings_GetCommaText(Self: TStrings): String;
begin
  result := Self.CommaText;
end;

procedure TStrings_SetCommaText(Self: TStrings; const Value: String);
begin
  Self.CommaText := Value;
end;

function TStrings_GetCount(Self: TStrings): Integer;
begin
  result := Self.Count;
end;

function TStrings_GetName(Self: TStrings; I: Integer): String;
begin
  result := Self.Names[I];
end;

function TStrings_GetObject(Self: TStrings; I: Integer): TObject;
begin
  result := Self.Objects[I];
end;

procedure TStrings_SetObject(Self: TStrings; I: Integer; Value: TObject);
begin
  Self.Objects[I] := Value;
end;

function TStrings_GetValue(Self: TStrings; const I: String): String;
begin
  result := Self.Values[I];
end;

procedure TStrings_SetValue(Self: TStrings; const I: String; const Value: String);
begin
  Self.Values[I] := Value;
end;

function TStrings_GetString(Self: TStrings; I: Integer): String;
begin
  result := Self.Strings[I];
end;

procedure TStrings_SetString(Self: TStrings; I: Integer; const Value: String);
begin
  Self.Strings[I] := Value;
end;

function TStrings_GetText(Self: TStrings): String;
begin
  result := Self.Text;
end;

procedure TStrings_SetText(Self: TStrings; const Value: String);
begin
  Self.Text := Value;
end;

{
procedure TStrings_Clear(Self: TStrings);
begin
  self.Clear;
end;
}

// TStringList -----------------------------------------------------------------

function TStringList_GetDuplicates(Self: TStringList): TDuplicates;
begin
  result := Self.Duplicates;
end;

procedure TStringList_SetDuplicates(Self: TStringList; Value: TDuplicates);
begin
  Self.Duplicates := Value;
end;

function TStringList_GetSorted(Self: TStringList): Boolean;
begin
  result := Self.Sorted;
end;

procedure TStringList_SetSorted(Self: TStringList; Value: Boolean);
begin
  Self.Sorted := Value;
end;

// TStream ---------------------------------------------------------------------

function TStream_GetPosition(Self: TStream): Integer;
begin
  result := Self.Position;
end;

procedure TStream_SetPosition(Self: TStream; Value: Integer);
begin
  Self.Position := Value;
end;

function TStream_GetSize(Self: TStream): Longint;
begin
  result := Self.Size;
end;

procedure TStream_SetSize(Self: TStream; Value: Longint);
begin
  Self.Size := Value;
end;

// TStream ---------------------------------------------------------------------

function THandleStream_GetHandle(Self: THandleStream): Integer;
begin
  result := Self.Handle;
end;

// TCustomMemoryStream ---------------------------------------------------------

function TCustomMemoryStream_GetMemory(Self: TCustomMemoryStream): Pointer;
begin
  result := Self.Memory;
end;

// TParser ---------------------------------------------------------------------

function TParser_GetFloatType(Self: TParser): Char;
begin
  result := Self.FloatType;
end;

function TParser_GetSourceLine(Self: TParser): Integer;
begin
  result := Self.SourceLine;
end;

function TParser_GetToken(Self: TParser): Char;
begin
  result := Self.Token;
end;

// TComponent ------------------------------------------------------------------

function TComponent_GetComponent(Self: TComponent; I: Integer): TComponent;
begin
  result := Self.Components[I];
end;

function TComponent_GetComponentCount(Self: TComponent): Integer;
begin
  result := Self.ComponentCount;
end;

function TComponent_GetComponentIndex(Self: TComponent): Integer;
begin
  result := Self.ComponentIndex;
end;

procedure TComponent_SetComponentIndex(Self: TComponent; Value: Integer);
begin
  Self.ComponentIndex := Value;
end;

function TComponent_GetComponentState(Self: TComponent): TComponentState;
begin
  result := Self.ComponentState;
end;

function TComponent_GetComponentStyle(Self: TComponent): TComponentStyle;
begin
  result := Self.ComponentStyle;
end;

function TComponent_GetDesignInfo(Self: TComponent): Integer;
begin
  result := Self.DesignInfo;
end;

procedure TComponent_SetDesignInfo(Self: TComponent; Value: Integer);
begin
  Self.DesignInfo := Value;
end;

function TComponent_GetOwner(Self: TComponent): TComponent;
begin
  result := Self.Owner;
end;

//------------------------------------------------------------------------------

function TStringStream_GetDataString(Self: TStringStream): String;
begin
  result := Self.DataString;
end;


procedure Register_Classes;
var
  H, G, J: Integer;
begin
  H := RegisterNamespace(0, 'Classes');

{ Maximum TList size }

  RegisterConstant(H, 'MaxListSize', MaxListSize);

{ TStream seek origins }

  RegisterConstant(H, 'soFromBeginning', soFromBeginning);
  RegisterConstant(H, 'soFromCurrent', soFromCurrent);
  RegisterConstant(H, 'soFromEnd', soFromEnd);

{ TFileStream create mode }

  RegisterConstant(H, 'fmCreate', fmCreate);

{ TParser special tokens }

  RegisterConstant(H, 'toEOF', toEOF);
  RegisterConstant(H, 'toSymbol', toSymbol);
  RegisterConstant(H, 'toString', toString);
  RegisterConstant(H, 'toInteger', toInteger);
  RegisterConstant(H, 'toFloat', toFloat);
  RegisterConstant(H, 'toWString', toWString);

{ Text alignment types }

  J := RegisterRTTIType(H, TypeInfo(TAlignment));
//  RegisterRTTIType(H, TypeInfo(TLeftRight));
  RegisterSubrangeType(H, 'TLeftRight', J,           
    Ord(taLeftJustify), Ord(taRightJustify));

  RegisterRTTIType(H, TypeInfo(TBiDiMode));

{ Types used by standard events }

  RegisterRTTIType(H, TypeInfo(TShiftState));
  RegisterRTTIType(H, TypeInfo(THelpContext));

{ Duplicate management }

  RegisterRTTIType(H, TypeInfo(TDuplicates));

  RegisterClassType(H, TComponent);
  RegisterClassType(H, TStream);
  RegisterClassType(H, TFiler);
  RegisterClassType(H, TReader);
  RegisterClassType(H, TWriter);

// TList -----------------------------------------------------------------------

  G := RegisterArrayType(H, 'TPointerList',
                            RegisterSubrangeType(H, '', _typeINTEGER, 0, MaxListSize - 1),
                            _typePOINTER);
  RegisterPointerType(H, 'PPointerList', G);

  G := RegisterHeader(H, 'function __TListSortCompare(Item1, Item2: Pointer): Integer;', nil);
  RegisterProceduralType(H, 'TListSortCompare', G);
  RegisterRTTIType(H, TypeInfo(TListNotification));

  G := RegisterClassType(H, TList);

  RegisterHeader(G, 'constructor Create;', @TList.Create);
  RegisterHeader(G, 'function Add(Item: Pointer): Integer;', @TList.Add);
  RegisterHeader(G, 'procedure Clear; virtual;', @TList.Clear);
  RegisterHeader(G, 'procedure Delete(Index: Integer);', @TList.Delete);
  RegisterHeader(G, 'procedure Exchange(Index1, Index2: Integer);', @TList.Exchange);
  RegisterHeader(G, 'function Expand: TList;', @TList.Expand);
  RegisterHeader(G, 'function Extract(Item: Pointer): Pointer;', @TList.Extract);
  RegisterHeader(G, 'function First: Pointer;', @TList.First);
  RegisterHeader(G, 'function IndexOf(Item: Pointer): Integer;', @TList.IndexOf);
  RegisterHeader(G, 'procedure Insert(Index: Integer; Item: Pointer);', @TList.Insert);
  RegisterHeader(G, 'function Last: Pointer;', @TList.Last);
  RegisterHeader(G, 'procedure Move(CurIndex, NewIndex: Integer);', @TList.Move);
  RegisterHeader(G, 'function Remove(Item: Pointer): Integer;', @TList.Remove);
  RegisterHeader(G, 'procedure Pack;', @TList.Pack);
  RegisterHeader(G, 'procedure Sort(Compare: TListSortCompare);', @TList.Sort);

  RegisterFakeHeader(G, 'function _GetCapacity: Integer;', @TList_GetCapacity);
  RegisterFakeHeader(G, 'procedure _SetCapacity(Value: Integer);', @TList_SetCapacity);
  RegisterProperty(G, 'property Capacity: Integer read _GetCapacity write _SetCapacity;');

  RegisterFakeHeader(G, 'function _GetCount: Integer;', @TList_GetCount);
  RegisterProperty(G, 'property Count: Integer read _GetCount;');

  RegisterFakeHeader(G, 'function _GetItem(I: Integer): Pointer;', @TList_GetItem);
  RegisterFakeHeader(G, 'procedure _SetItem(I: Integer; Value: Pointer);', @TList_SetItem);
  RegisterProperty(G, 'property Items[Index: Integer]: Pointer read _GetItem write _SetItem; default;');

//  RegisterHeader(G, 'function _GetList: PPointerList;', @TList_GetList);
//  RegisterProperty(G, 'property List: PPointerList read _GetList;');

// TBits -----------------------------------------------------------------------

  G := RegisterClassType(H, TBits);
  RegisterHeader(G, 'constructor Create;', @TBits.Create);
  RegisterHeader(G, 'function OpenBit: Integer;', @TBits.OpenBit);

  RegisterFakeHeader(G, 'function _GetBit(I: Integer): Boolean;', @TBits_GetBit);
  RegisterFakeHeader(G, 'procedure _SetBit(I: Integer; Value: Boolean);', @TBits_SetBit);
  RegisterProperty(G, 'property Bits[Index: Integer]: Boolean read _GetBit write _SetBit; default;');

  RegisterFakeHeader(G, 'function _GetSize: Integer;', @TBits_GetSize);
  RegisterProperty(G, 'property Size: Integer read _GetSize;');

// TPersistent -----------------------------------------------------------------

  G := RegisterClassType(H, TPersistent);
  RegisterClassReferenceType(H, 'TPersistentClass', G);

  RegisterHeader(G, 'constructor Create;', @TPersistent.Create);
  RegisterHeader(G, 'procedure Assign(Source: TPersistent); virtual;', @TPersistent.Assign);
  RegisterHeader(G, 'function  GetNamePath: string; dynamic;', @TPersistent.GetNamePath);

// TCollectionItem -------------------------------------------------------------

  RegisterClassType(H, TCollection);
  G := RegisterClassType(H, TCollectionItem);
  RegisterClassReferenceType(H, 'TCollectionItemClass', G);

  RegisterHeader(G, 'constructor Create(Collection: TCollection); virtual;', @TCollectionItem.Create);
  RegisterHeader(G, 'function GetNamePath: string; override;', @TCollectionItem.GetNamePath);

  RegisterFakeHeader(G, 'function _GetCollection: TCollection;', @TCollectionItem_GetCollection);
  RegisterFakeHeader(G, 'procedure _SetCollection(Value: TCollection);', @TCollectionItem_SetCollection);
  RegisterProperty(G, 'property Collection: TCollection read _GetCollection write _SetCollection;');

  RegisterFakeHeader(G, 'function _GetID: Integer;', @TCollectionItem_GetID);
  RegisterProperty(G, 'property ID: Integer read _GetID;');

  RegisterFakeHeader(G, 'function _GetIndex: Integer;', @TCollectionItem_GetIndex);
  RegisterFakeHeader(G, 'procedure _SetIndex(Value: Integer);', @TCollectionItem_SetIndex);
  RegisterProperty(G, 'property Index: Integer read _GetIndex write _SetIndex;');

  RegisterFakeHeader(G, 'function _GetDisplayName: String;', @TCollectionItem_GetDisplayName);
  RegisterFakeHeader(G, 'procedure _SetDisplayName(const Value: String);', @TCollectionItem_SetDisplayName);
  RegisterProperty(G, 'property DisplayName: string read _GetDisplayName write _SetDisplayName;');

// TCollection -----------------------------------------------------------------

  G := RegisterClassType(H, TCollection);

  RegisterHeader(G, 'constructor Create(ItemClass: TCollectionItemClass);', @TCollection.Create);
  RegisterHeader(G, 'function Add: TCollectionItem;', @TCollection.Add);
  RegisterHeader(G, 'procedure Assign(Source: TPersistent); override;', @TCollection.Assign);
  RegisterHeader(G, 'procedure BeginUpdate; virtual;', @TCollection.BeginUpdate);
  RegisterHeader(G, 'procedure Clear;', @TCollection.Clear);
  RegisterHeader(G, 'procedure Delete(Index: Integer);',  @TCollection.Delete);
  RegisterHeader(G, 'procedure EndUpdate; virtual;',  @TCollection.EndUpdate);
  RegisterHeader(G, 'function FindItemID(ID: Integer): TCollectionItem;', @TCollection.FindItemId);
  RegisterHeader(G, 'function GetNamePath: string; override;',  @TCollection.GetNamePath);
  RegisterHeader(G, 'function Insert(Index: Integer): TCollectionItem;',  @TCollection.Insert);

  RegisterFakeHeader(G, 'function _GetCount: Integer;', @TCollection_GetCount);
  RegisterProperty(G, 'property Count: Integer read _GetCount;');

  RegisterFakeHeader(G, 'function _GetItemClass: TCollectionItemClass;',
                    @TCollection_GetItemClass);
  RegisterProperty(G, 'property ItemClass: TCollectionItemClass read _GetItemClass;');

  RegisterFakeHeader(G, 'function _GetItem(I: Integer): TCollectionItem;', @TCollection_GetItem);
  RegisterFakeHeader(G, 'procedure _SetItem(I: Integer; Value: TCollectionItem);', @TCollection_SetItem);
  RegisterProperty(G, 'property Items[Index: Integer]: TCollectionItem read _GetItem write _SetItem;');

// TStrings --------------------------------------------------------------------

  G := RegisterClassType(H, TStrings);

  RegisterHeader(G, 'constructor Create;', @TStrings.Create);

  RegisterHeader(G, 'function Get(Index: Integer): string; virtual; abstract;', nil);
  RegisterHeader(G, 'function GetCount: Integer; virtual; abstract;', nil);

  RegisterHeader(G, 'function Add(const S: string): Integer; virtual;', @TStrings.Add);
  RegisterHeader(G, 'function AddObject(const S: string; AObject: TObject): Integer; virtual;', @TStrings.AddObject);
  RegisterHeader(G, 'procedure Append(const S: string);', @TStrings.Append);
  RegisterHeader(G, 'procedure AddStrings(Strings: TStrings); virtual;', @TStrings.AddStrings);
  RegisterHeader(G, 'procedure Assign(Source: TPersistent); override;', @TStrings.Assign);
  RegisterHeader(G, 'procedure BeginUpdate;', @TStrings.BeginUpdate);
  RegisterHeader(G, 'procedure Clear; virtual; abstract;', nil);

  RegisterHeader(G, 'procedure Delete(Index: Integer); virtual; abstract;', nil);
  RegisterHeader(G, 'procedure EndUpdate;', @TStrings.EndUpdate);
  RegisterHeader(G, 'function Equals(Strings: TStrings): Boolean;', @TStrings.Equals);
  RegisterHeader(G, 'procedure Exchange(Index1, Index2: Integer); virtual;', @TStrings.Exchange);
  RegisterHeader(G, 'function GetText: PChar; virtual;', @TStrings.GetText);
  RegisterHeader(G, 'function IndexOf(const S: string): Integer; virtual;', @TStrings.IndexOf);
  RegisterHeader(G, 'function IndexOfName(const Name: string): Integer;', @TStrings.IndexOfName);
  RegisterHeader(G, 'function IndexOfObject(AObject: TObject): Integer;', @TStrings.IndexOfObject);
  RegisterHeader(G, 'procedure Insert(Index: Integer; const S: string); virtual; abstract;', nil);
  RegisterHeader(G, 'procedure InsertObject(Index: Integer; const S: string; AObject: TObject);', @TStrings.InsertObject);
  RegisterHeader(G, 'procedure LoadFromFile(const FileName: string); virtual;', @TStrings.LoadFromFile);
  RegisterHeader(G, 'procedure LoadFromStream(Stream: TStream); virtual;', @TStrings.LoadFromStream);
  RegisterHeader(G, 'procedure Move(CurIndex, NewIndex: Integer); virtual;', @TStrings.Move);
  RegisterHeader(G, 'procedure SaveToFile(const FileName: string); virtual;', @TStrings.SaveToFile);
  RegisterHeader(G, 'procedure SaveToStream(Stream: TStream); virtual;', @TStrings.SaveToStream);
  RegisterHeader(G, 'procedure SetText(Text: PChar); virtual;', @TStrings.SetText);

  RegisterFakeHeader(G, 'function _GetCapacity: Integer;', @TStrings_GetCapacity);
  RegisterFakeHeader(G, 'procedure _SetCapacity(Value: Integer);', @TStrings_SetCapacity);
  RegisterProperty(G, 'property Capacity: Integer read _GetCapacity write _SetCapacity;');

  RegisterFakeHeader(G, 'function _GetCommaText: String;', @TStrings_GetCommaText);
  RegisterFakeHeader(G, 'procedure _SetCommaText(const Value: String);', @TStrings_SetCommaText);
  RegisterProperty(G, 'property CommaText: string read _GetCommaText write _SetCommaText;');

  RegisterFakeHeader(G, 'function _GetCount: Integer;', @TStrings_GetCount);
  RegisterProperty(G, 'property Count: Integer read _GetCount;');

  RegisterFakeHeader(G, 'function _GetName(I: Integer): String;', @TStrings_GetName);
  RegisterProperty(G, 'property Names[Index: Integer]: string read _GetName;');

  RegisterFakeHeader(G, 'function GetObjects(I: Integer): TObject;', @TStrings_GetObject);
  RegisterFakeHeader(G, 'procedure SetObjects(I: Integer; Value: TObject);', @TStrings_SetObject);
  RegisterProperty(G, 'property Objects[Index: Integer]: TObject read GetObjects write SetObjects;');

  RegisterFakeHeader(G, 'function _GetValue(const I: String): String;', @TStrings_GetValue);
  RegisterFakeHeader(G, 'procedure _SetValue(const I: String; const Value: String);', @TStrings_GetValue);
  RegisterProperty(G, 'property Values[const Name: string]: string read _GetValue write _SetValue;');

  RegisterFakeHeader(G, 'function _GetString(I: Integer): String;', @TStrings_GetString);
  RegisterFakeHeader(G, 'procedure _SetString(I: Integer; const Value: String);', @TStrings_SetString);
  RegisterProperty(G, 'property Strings[Index: Integer]: string read _GetString write _SetString; default;');

  RegisterFakeHeader(G, 'function _GetText: String;', @TStrings_GetText);
  RegisterFakeHeader(G, 'procedure _SetText(const Value: String);', @TStrings_SetText);
  RegisterProperty(G, 'property Text: string read _GetText write _SetText;');

// TStringList -----------------------------------------------------------------

  G := RegisterClassType(H, TStringList);

  RegisterProceduralType(H, 'TStringListSortCompare',  RegisterHeader(H, 'function __TStringListSortCompare(List: TStringList; Index1, Index2: Integer): Integer;', nil));

  RegisterHeader(G, 'constructor Create;', @TStringList.Create);
  RegisterHeader(G, 'function Add(const S: string): Integer; override;', @TStringList.Add);
  RegisterHeader(G, 'procedure Clear; override;', @TStringList.Clear);
  RegisterHeader(G, 'procedure Delete(Index: Integer); override;', @TStringList.Delete);
  RegisterHeader(G, 'procedure Exchange(Index1, Index2: Integer); override;', @TStringList.Exchange);
  RegisterHeader(G, 'function Find(const S: string; var Index: Integer): Boolean; virtual;', @TStringList.Find);
  RegisterHeader(G, 'function IndexOf(const S: string): Integer; override;', @TStringList.IndexOf);
  RegisterHeader(G, 'procedure Insert(Index: Integer; const S: string); override;', @TStringList.Insert);
  RegisterHeader(G, 'procedure Sort; virtual;', @TStringList.Sort);
  RegisterHeader(G, 'procedure CustomSort(Compare: TStringListSortCompare); virtual;', @TStringList.CustomSort);

  RegisterFakeHeader(G, 'function _GetDuplicates: TDuplicates;', @TStringList_GetDuplicates);
  RegisterFakeHeader(G, 'procedure _SetDuplicates(Value: TDuplicates);', @TStringList_SetDuplicates);
  RegisterProperty(G, 'property Duplicates: TDuplicates read _GetDuplicates write _SetDuplicates;');

  RegisterFakeHeader(G, 'function _GetSorted: Boolean;', @TStringList_GetSorted);
  RegisterFakeHeader(G, 'procedure _SetSorted(Value: Boolean);', @TStringList_SetSorted);
  RegisterProperty(G, 'property Sorted: Boolean read _GetSorted write _SetSorted;');

// TStream ---------------------------------------------------------------------

  G := RegisterClassType(H, TStream);

  RegisterHeader(G, 'constructor Create;', @TStream.Create);
  RegisterHeader(G, 'function Read(var Buffer; Count: Longint): Longint; virtual; abstract;', nil);
  RegisterHeader(G, 'function Write(const Buffer; Count: Longint): Longint; virtual; abstract;', nil);
  RegisterHeader(G, 'function Seek(Offset: Longint; Origin: Word): Longint; virtual; abstract;', nil);
  RegisterHeader(G, 'procedure ReadBuffer(var Buffer; Count: Longint);', @TStream.ReadBuffer);
  RegisterHeader(G, 'procedure WriteBuffer(const Buffer; Count: Longint);', @TStream.WriteBuffer);
  RegisterHeader(G, 'function CopyFrom(Source: TStream; Count: Longint): Longint;', @TStream.CopyFrom);
  RegisterHeader(G, 'function ReadComponent(Instance: TComponent): TComponent;', @TStream.ReadComponent);
  RegisterHeader(G, 'function ReadComponentRes(Instance: TComponent): TComponent;', @TStream.ReadComponentRes);
  RegisterHeader(G, 'procedure WriteComponent(Instance: TComponent);', @TStream.WriteComponent);
  RegisterHeader(G, 'procedure WriteResourceHeader(const ResName: string; out FixupInfo: Integer);', @TStream.WriteResourceHeader);
  RegisterHeader(G, 'procedure FixupResourceHeader(FixupInfo: Integer);', @TStream.FixupResourceHeader);
  RegisterHeader(G, 'procedure ReadResHeader;', @TStream.ReadResHeader);

  RegisterFakeHeader(G, 'function _GetPosition: Integer;', @TStream_GetPosition);
  RegisterFakeHeader(G, 'procedure _SetPosition(Value: Integer);', @TStream_SetPosition);
  RegisterProperty(G, 'property Position: Longint read _GetPosition write _SetPosition;');

  RegisterFakeHeader(G, 'function _GetSize: Longint;', @TStream_GetSize);
  RegisterFakeHeader(G, 'procedure _SetSize(Value: Longint);', @TStream_SetSize);
  RegisterProperty(G, 'property Size: Longint read _GetSize write _SetSize;');

// THandleStream ---------------------------------------------------------------

  G := RegisterClassType(H, THandleStream);

  RegisterHeader(G, 'constructor Create(AHandle: Integer);', @THandleStream.Create);
  RegisterHeader(G, 'function Read(var Buffer; Count: Longint): Longint; override;', @THandleStream.Read);
  RegisterHeader(G, 'function Write(const Buffer; Count: Longint): Longint; override;', @THandleStream.Write);
  RegisterHeader(G, 'function Seek(Offset: Longint; Origin: Word): Longint; override;', @THandleStream.Seek);

  RegisterFakeHeader(G, 'function _GetHandle: Integer;', @THandleStream_GetHandle);
  RegisterProperty(G, 'property Handle: Integer read _GetHandle;');

// TFileStream -----------------------------------------------------------------

  G := RegisterClassType(H, TFileStream);

  RegisterHeader(G, 'constructor Create(const FileName: string; Mode: Word);', @TFileStream.Create);

// TCustomMemoryStream ---------------------------------------------------------

  G := RegisterClassType(H, TCustomMemoryStream);

  RegisterHeader(G, 'constructor Create;', @TCustomMemoryStream.Create);
  RegisterHeader(G, 'function Read(var Buffer; Count: Longint): Longint; override;', @TCustomMemoryStream.Read);
  RegisterHeader(G, 'function Seek(Offset: Longint; Origin: Word): Longint; override;', @TCustomMemoryStream.Seek);
  RegisterHeader(G, 'procedure SaveToStream(Stream: TStream);', @TCustomMemoryStream.SaveToStream);
  RegisterHeader(G, 'procedure SaveToFile(const FileName: string);', @TCustomMemoryStream.SaveToFile);

  RegisterFakeHeader(G, 'function _GetMemory: Pointer;', @TCustomMemoryStream_GetMemory);
  RegisterProperty(G, 'property Memory: Pointer read _GetMemory;');

// TMemoryStream ---------------------------------------------------------------

  G := RegisterClassType(H, TMemoryStream);

  RegisterHeader(G, 'constructor Create;', @TMemoryStream.Create);
  RegisterHeader(G, 'procedure Clear;', @TMemoryStream.Clear);
  RegisterHeader(G, 'procedure LoadFromStream(Stream: TStream);', @TMemoryStream.LoadFromStream);
  RegisterHeader(G, 'procedure LoadFromFile(const FileName: string);', @TMemoryStream.LoadFromFile);
  RegisterHeader(G, 'procedure SetSize(NewSize: Longint); override;', @TMemoryStream.SetSize);
  RegisterHeader(G, 'function Write(const Buffer; Count: Longint): Longint; override;', @TMemoryStream.Write);

// TStringStream ---------------------------------------------------------------

  G := RegisterClassType(H, TStringStream);

  RegisterHeader(G, 'constructor Create(const AString: string); overload;', @TStringStream.Create);

  RegisterHeader(G, 'function Read(var Buffer; Count: Longint): Longint; override;', @TStringStream.Read);
  RegisterHeader(G, 'function ReadString(Count: Longint): string;', @TStringStream.ReadString);
  RegisterHeader(G, 'function Seek(Offset: Longint; Origin: Word): Longint; override;', @TStringStream.Seek);
  RegisterHeader(G, 'function Write(const Buffer; Count: Longint): Longint; override;', @TStringStream.Write);
  RegisterHeader(G, 'procedure WriteString(const AString: string);', @TStringStream.WriteString);

  RegisterFakeHeader(G, 'function _GetDataString: String;', @TStringStream_GetDataString);
  RegisterProperty(G, 'property DataString: string read _GetDataString;');

// TResourceStream -------------------------------------------------------------

  G := RegisterClassType(H, TResourceStream);

  RegisterHeader(G, 'constructor Create(Instance: THandle; const ResName: string; ResType: PChar);', @TResourceStream.Create);
  RegisterHeader(G, 'constructor CreateFromID(Instance: THandle; ResID: Integer; ResType: PChar);', @TResourceStream.CreateFromID);
  RegisterHeader(G, 'function Write(const Buffer; Count: Longint): Longint; override;', @TResourceStream.Write);

// TParser ---------------------------------------------------------------------

  G := RegisterClassType(H, TParser);

  RegisterHeader(G, 'constructor Create(Stream: TStream);', @TParser.Create);
  RegisterHeader(G, 'procedure CheckToken(T: Char);', @TParser.CheckToken);
  RegisterHeader(G, 'procedure CheckTokenSymbol(const S: string);', @TParser.CheckTokenSymbol);
  RegisterHeader(G, 'procedure Error(const Ident: string);', @TParser.Error);
  RegisterHeader(G, 'procedure ErrorStr(const Message: string);', @TParser.ErrorStr);
  RegisterHeader(G, 'procedure HexToBinary(Stream: TStream);', @TParser.HexToBinary);
  RegisterHeader(G, 'function NextToken: Char;', @TParser.NextToken);
  RegisterHeader(G, 'function SourcePos: Longint;', @TParser.SourcePos);
  RegisterHeader(G, 'function TokenComponentIdent: string;', @TParser.TokenComponentIdent);
  RegisterHeader(G, 'function TokenFloat: Extended;', @TParser.TokenFloat);
  RegisterHeader(G, 'function TokenString: string;', @TParser.TokenString);
  RegisterHeader(G, 'function TokenSymbolIs(const S: string): Boolean;', @TParser.TokenSymbolIs);

  RegisterFakeHeader(G, 'function _GetFloatType: Char;', @TParser_GetFloatType);
  RegisterProperty(G, 'property FloatType: Char read _GetFloatType;');

  RegisterFakeHeader(G, 'function _GetSourceLine: Integer;', @TParser_GetSourceLine);
  RegisterProperty(G, 'property SourceLine: Integer read _GetSourceLine;');

  RegisterFakeHeader(G, 'function _GetToken: Char;', @TParser_GetToken);
  RegisterProperty(G, 'property Token: Char read _GetToken;');

// TComponent ------------------------------------------------------------------

  RegisterClassType(H, TBasicAction);
  RegisterRTTIType(H, TypeInfo(TComponentState));
  RegisterRTTIType(H, TypeInfo(TComponentStyle));

  G := RegisterClassType(H, TComponent);

  RegisterHeader(G, 'constructor Create(AOwner: TComponent); virtual;', @TComponent.Create);
  RegisterHeader(G, 'procedure BeforeDestruction; override;', @TComponent.BeforeDestruction);
  RegisterHeader(G, 'procedure DestroyComponents;', @TComponent.DestroyComponents);
  RegisterHeader(G, 'procedure Destroying;', @TComponent.Destroying);
  RegisterHeader(G, 'function ExecuteAction(Action: TBasicAction): Boolean; dynamic;', @TComponent.ExecuteAction);
  RegisterHeader(G, 'function FindComponent(const AName: string): TComponent;', @TComponent.FindComponent);
  RegisterHeader(G, 'procedure FreeNotification(AComponent: TComponent);', @TComponent.FreeNotification);
  RegisterHeader(G, 'procedure RemoveFreeNotification(AComponent: TComponent);', @TComponent.RemoveFreeNotification);
  RegisterHeader(G, 'procedure FreeOnRelease;', @TComponent.FreeOnRelease);
  RegisterHeader(G, 'function GetParentComponent: TComponent; dynamic;', @TComponent.GetParentComponent);
  RegisterHeader(G, 'function GetNamePath: string; override;', @TComponent.GetNamePath);
  RegisterHeader(G, 'function HasParent: Boolean; dynamic;', @TComponent.HasParent);
  RegisterHeader(G, 'procedure InsertComponent(AComponent: TComponent);', @TComponent.InsertComponent);
  RegisterHeader(G, 'procedure RemoveComponent(AComponent: TComponent);', @TComponent.RemoveComponent);
  RegisterHeader(G, 'function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;', @TComponent.SafeCallException);
  RegisterHeader(G, 'function UpdateAction(Action: TBasicAction): Boolean; dynamic;', @TComponent.UpdateAction);

  RegisterFakeHeader(G, 'function _GetComponent(I: Integer): TComponent;', @TComponent_GetComponent);
  RegisterProperty(G, 'property Components[Index: Integer]: TComponent read _GetComponent;');

  RegisterFakeHeader(G, 'function _GetComponentCount: Integer;', @TComponent_GetComponentCount);
  RegisterProperty(G, 'property ComponentCount: Integer read _GetComponentCount;');

  RegisterFakeHeader(G, 'function _GetComponentIndex: Integer;', @TComponent_GetComponentIndex);
  RegisterFakeHeader(G, 'procedure _SetComponentIndex(Value: Integer);', @TComponent_SetComponentIndex);
  RegisterProperty(G, 'property ComponentIndex: Integer read _GetComponentIndex write _SetComponentIndex;');

  RegisterFakeHeader(G, 'function _GetComponentState: TComponentState;', @TComponent_GetComponentState);
  RegisterProperty(G, 'property ComponentState: TComponentState read _GetComponentState;');

  RegisterFakeHeader(G, 'function _GetComponentStyle: TComponentStyle;', @TComponent_GetComponentStyle);
  RegisterProperty(G, 'property ComponentStyle: TComponentStyle read _GetComponentStyle;');

  RegisterFakeHeader(G, 'function _GetDesignInfo: Integer;', @TComponent_GetDesignInfo);
  RegisterFakeHeader(G, 'procedure _SetDesignInfo(Value: Integer);', @TComponent_SetDesignInfo);
  RegisterProperty(G, 'property DesignInfo: Longint read _GetDesignInfo write _SetDesignInfo;');

  RegisterFakeHeader(G, 'function _GetOwner: TComponent;', @TComponent_GetOwner);
  RegisterProperty(G, 'property Owner: TComponent read _GetOwner;');

{ Point and rectangle constructors }

  RegisterHeader(H, 'function Point(AX, AY: Integer): TPoint;', @Point);
  RegisterHeader(H, 'function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;', @Rect);
  RegisterHeader(H, 'function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;', @Bounds);

{ Class registration routines }

  RegisterHeader(H, 'procedure RegisterClass(AClass: TPersistentClass);', @RegisterClass);
  RegisterHeader(H, 'procedure UnRegisterClass(AClass: TPersistentClass);', @UnRegisterClass);
  RegisterHeader(H, 'function FindClass(const ClassName: string): TPersistentClass;', @FindClass);
  RegisterHeader(H, 'function GetClass(const AClassName: string): TPersistentClass;', @GetClass);

  RegisterRTTIType(H, TypeInfo(TOperation));


end;


end.
