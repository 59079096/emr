{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit HCEmrElementItem;

interface

uses
  Windows, Classes, Controls, Graphics, SysUtils, HCStyle, HCItem,
  HCTextItem, HCEditItem, HCComboboxItem, HCDateTimePicker, HCRadioGroup, HCTableItem,
  HCTableCell, HCCheckBoxItem, HCFractionItem, HCFloatBarCodeItem, HCCommon,
  HCCustomData, HCXml, HCImageItem, Generics.Collections;

const
  EMRSTYLE_TOOTH = -1001;  // 牙齿公式 THCStyle.Custom - 1
  EMRSTYLE_FANGJIAO = -1002;  // 房角公式 THCStyle.Custom - 2
  EMRSTYLE_YUEJING = -1003;  // 月经公式

type
  TStyleExtra = (cseNone, cseDel, cseAdd);  // 痕迹样式

  /// <summary> 数据元属性 </summary>
  TDeProp = class(TObject)
  public
    const
      Index = 'Index';
      Code = 'Code';
      &Name = 'Name';
      //Text = 'Text';
      /// <summary> 类别 单选、多选、数值、日期时间等 </summary>
      Frmtp = 'Frmtp';
      /// <summary> 单位 </summary>
      &Unit = 'Unit';
      /// <summary> 隐藏单位 </summary>
      HideUnit = 'HdUnit';

      /// <summary> 表示格式 </summary>
      PreFormat = 'PRFMT';

      /// <summary> 原始数据 </summary>
      Raw = 'Raw';

      /// <summary> 受控词汇表(值域代码) </summary>
      CMV = 'CMV';

      /// <summary> 受控词汇编码(值编码) </summary>
      CMVVCode = 'CMVVCode';

      /// <summary> 痕迹信息 </summary>
      Trace = 'Trace';

      /// <summary> 隐私信息 </summary>
      Secret = 'Secret';
  end;

  /// <summary> 数据元类型 </summary>
  TDeFrmtp = class(TObject)
  public
    const
      /// <summary> 单选 </summary>
      Radio = 'RS';
      /// <summary> 多选 </summary>
      Multiselect = 'MS';
      /// <summary> 数值 </summary>
      Number = 'N';
      /// <summary> 文本 </summary>
      &String = 'S';
      /// <summary> 日期 </summary>
      Date = 'D';
      /// <summary> 时间 </summary>
      Time = 'T';
      /// <summary> 日期时间 </summary>
      DateTime = 'DT';
  end;

  TGroupProp = class(TObject)
  public
    const
      /// <summary> 数据组类型 </summary>
      SubType = 'RT';
  end;

  TSubType = class(TObject)
  public
    const
      /// <summary> 病程 </summary>
      Proc = 'P';
  end;

  TEmrSyntaxProblem = (
    espContradiction,  // 矛盾
    espWrong  // 错误
  );

  /// <summary> 电子病历文本语法信息对象 </summary>
  TEmrSyntax = class(TObject)
  public
    Problem: TEmrSyntaxProblem;
    Offset, Length: Integer;
  end;

  TSyntaxPaintEvent = procedure(const AData: THCCustomData; const AItemNo: Integer;
    const ADrawText: string; const ASyntax: TEmrSyntax; const ARect: TRect; const ACanvas: TCanvas) of object;

  /// <summary> 电子病历文本对象 </summary>
  TEmrTextItem = class(THCTextItem)
  private
    FSyntaxs: TObjectList<TEmrSyntax>;
  public
    //constructor Create; override;
    destructor Destroy; override;
    procedure SyntaxClear;
    procedure SyntaxAdd(const AOffset, ALength: Integer; const AProblem: TEmrSyntaxProblem);
    function SyntaxCount: Integer;
    property Syntaxs: TObjectList<TEmrSyntax> read FSyntaxs;
  end;

  /// <summary> 电子病历数据元对象 </summary>
  TDeItem = class sealed(TEmrTextItem)  // 不可继承
  private
    FMouseIn,
    FOutOfRang,  // 值不在正常范围内
    FEditProtect,  // 编辑保护、不允许手动录入
    FCopyProtect,  // 复制保护，不允许复制
    FDeleteAllow,  // 是否允许删除
    FAllocValue  // 是否分配过值
      : Boolean;
    FStyleEx: TStyleExtra;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
    function GetIsElement: Boolean;
  protected
    procedure SetText(const Value: string); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class procedure GetSecretRange(const ASecret: string; var ALow, AHi: Integer);

    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure SetActive(const Value: Boolean); override;
    procedure Assign(Source: THCCustomItem); override;
    function CanConcatItems(const AItem: THCCustomItem): Boolean; override;
    function GetHint: string; override;
    procedure DeleteProperty(const APropName: string);
    function AcceptAction(const AOffset: Integer; const ARestrain: Boolean;
      const AAction: THCAction): Boolean; override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
//    procedure ToJson(const AJsonObj: TJSONObject);
//    procedure ParseJson(const AJsonObj: TJSONObject);

    property IsElement: Boolean read GetIsElement;
    property MouseIn: Boolean read FMouseIn;
    property StyleEx: TStyleExtra read FStyleEx write FStyleEx;
    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property CopyProtect: Boolean read FCopyProtect write FCopyProtect;
    property AllocValue: Boolean read FAllocValue write FAllocValue;
    property OutOfRang: Boolean read FOutOfRang write FOutOfRang;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeTable = class(THCTableItem)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  public
    constructor Create(const AOwnerData: THCCustomData; const ARowCount, AColCount,
      AWidth: Integer); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
//    procedure ToJson(const AJsonObj: TJSONObject);
//    procedure ParseJson(const AJsonObj: TJSONObject);

    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeCheckBox = class(THCCheckBoxItem)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string; const AChecked: Boolean); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
//    procedure ToJson(const AJsonObj: TJSONObject);
//    procedure ParseJson(const AJsonObj: TJSONObject);

    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeEdit = class(THCEditItem)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
//    procedure ToJson(const AJsonObj: TJSONObject);
//    procedure ParseJson(const AJsonObj: TJSONObject);

    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeCombobox = class(THCComboboxItem)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
//    procedure ToJson(const AJsonObj: TJSONObject);
//    procedure ParseJson(const AJsonObj: TJSONObject);

    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeDateTimePicker = class(THCDateTimePicker)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  public
    constructor Create(const AOwnerData: THCCustomData; const ADateTime: TDateTime); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
//    procedure ToJson(const AJsonObj: TJSONObject);
//    procedure ParseJson(const AJsonObj: TJSONObject);

    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeRadioGroup = class(THCRadioGroup)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
//    procedure ToJson(const AJsonObj: TJSONObject);
//    procedure ParseJson(const AJsonObj: TJSONObject);

    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeFloatBarCodeItem = class(THCFloatBarCodeItem)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
//    procedure ToJson(const AJsonObj: TJSONObject);
//    procedure ParseJson(const AJsonObj: TJSONObject);

    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeImageItem = class(THCImageItem)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
//    procedure ToJson(const AJsonObj: TJSONObject);
//    procedure ParseJson(const AJsonObj: TJSONObject);

    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  /// <summary> 创建指定样式的Item </summary>
  /// <param name="AData">要创建Item的Data</param>
  /// <param name="AStyleNo">要创建的Item样式</param>
  /// <returns>创建好的Item</returns>
  function CreateEmrStyleItem(const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem;

implementation

uses
  HCParaStyle, HCEmrYueJingItem, HCEmrToothItem, HCEmrFangJiaoItem;

function CreateEmrStyleItem(const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem;
begin
  case AStyleNo of
    THCStyle.Table:
      Result := TDeTable.Create(AData, 1, 1, 1);

    THCStyle.CheckBox:
      Result := TDeCheckBox.Create(AData, '勾选框', False);

    THCStyle.Edit:
      Result := TDeEdit.Create(AData, '');

    THCStyle.Combobox:
      Result := TDeCombobox.Create(AData, '');

    THCStyle.DateTimePicker:
      Result := TDeDateTimePicker.Create(AData, Now);

    THCStyle.RadioGroup:
      Result := TDeRadioGroup.Create(AData);

    THCStyle.Express, EMRSTYLE_YUEJING:
      Result := TEmrYueJingItem.Create(AData, '', '', '', '');

    EMRSTYLE_TOOTH:
      Result := TEmrToothItem.Create(AData, '', '', '', '');

    EMRSTYLE_FANGJIAO:
      Result := TEMRFangJiaoItem.Create(AData, '', '', '', '');

    THCStyle.FloatBarCode:
      Result := TDeFloatBarCodeItem.Create(AData);

    THCStyle.Image:
      Result := TDeImageItem.Create(AData);
  else
    Result := nil;
  end;
end;

{ TDeItem }

class procedure TDeItem.GetSecretRange(const ASecret: string; var ALow,
  AHi: Integer);
var
  vPos: Integer;
  vS: string;
begin
  ALow := -1;
  AHi := -1;
  if ASecret = '' then Exit;

  vPos := Pos('-', ASecret);
  if vPos = 0 then  // 2
    ALow := StrToInt(ASecret)
  else
  if vPos = 1 then  // -8  -
  begin
    ALow := 1;
    vS := Copy(ASecret, vPos + 1, System.Length(ASecret) - vPos);
    if vS <> '' then  // -
      AHi := StrToInt(vS);
  end
  else  // 2-7  3-
  begin
    vS := Copy(ASecret, 1, vPos - 1);
    ALow := StrToInt(vS);
    vS := Copy(ASecret, vPos + 1, System.Length(ASecret) - vPos);
    if vS <> '' then
      AHi := StrToInt(vS);
  end;
end;

procedure TDeItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FStyleEx := (Source as TDeItem).StyleEx;
  FEditProtect := (Source as TDeItem).EditProtect;
  FDeleteAllow := (Source as TDeItem).DeleteAllow;
  FCopyProtect := (Source as TDeItem).CopyProtect;
  FOutOfRang := (Source as TDeItem).OutOfRang;
  FPropertys.Assign((Source as TDeItem).Propertys);
end;

function TDeItem.AcceptAction(const AOffset: Integer; const ARestrain: Boolean;
  const AAction: THCAction): Boolean;
begin
  Result := inherited AcceptAction(AOffset, ARestrain, AAction);

  if Result then
  begin
    case AAction of
      actInsertText:
        begin
          if FEditProtect then  // 两头允许输入，触发actConcatText时返回供Data层处理新TextItem还是连接
            Result := (AOffset = 0) or (AOffset = Self.Length);
        end;

      actConcatText:
        begin
          if FEditProtect then
            Result := False
          else
          if IsElement then
            Result := False;// not ARestrain;
        end;

      actBackDeleteText:
        begin
          if FEditProtect then
            Result := AOffset = 0;
        end;

      actDeleteText:
        begin
          if FEditProtect then
            Result := AOffset = Self.Length;
        end;
    end;
  end;
end;

function TDeItem.CanConcatItems(const AItem: THCCustomItem): Boolean;
var
  vDeItem: TDeItem;
begin
  Result := inherited CanConcatItems(AItem);
  if Result then
  begin
    vDeItem := AItem as TDeItem;
    Result := (Self[TDeProp.Index] = vDeItem[TDeProp.Index])
      and (FStyleEx = vDeItem.FStyleEx)
      and (FEditProtect = vDeItem.EditProtect)
      and (FDeleteAllow = vDeItem.DeleteAllow)
      and (FCopyProtect = vDeItem.CopyProtect)
      and (Self[TDeProp.Trace] = vDeItem[TDeProp.Trace]);
  end;
end;

constructor TDeItem.Create;
begin
  inherited Create;
  FPropertys := TStringList.Create;

  FAllocValue := False;
  FCopyProtect := False;
  FEditProtect := False;
  FDeleteAllow := True;
  FOutOfRang := False;
  FMouseIn := False;
end;

procedure TDeItem.DeleteProperty(const APropName: string);
var
  vIndex: Integer;
begin
  vIndex := FPropertys.IndexOfName(APropName);
  if vIndex >= 0 then
    FPropertys.Delete(vIndex);
end;

destructor TDeItem.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited;
end;

function TDeItem.GetHint: string;
begin
  case FStyleEx of
    cseNone: Result := Self[TDeProp.Name];
  else
    Result := Self[TDeProp.Trace];
  end;
end;

function TDeItem.GetIsElement: Boolean;
begin
  Result := FPropertys.IndexOfName(TDeProp.Index) >= 0;
end;

function TDeItem.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 23 then
    AStream.ReadBuffer(vByte, SizeOf(vByte));

  FEditProtect := Odd(vByte shr 7);
  FOutOfRang := Odd(vByte shr 6);
  FCopyProtect := Odd(vByte shr 5);
  FAllocValue := Odd(vByte shr 4);

  if AFileVersion > 34 then
    FDeleteAllow := Odd(vByte shr 3)
  else
    FDeleteAllow := True;

  AStream.ReadBuffer(FStyleEx, SizeOf(TStyleExtra));
  HCLoadTextFromStream(AStream, vS, AFileVersion);
  FPropertys.Text := vS;
end;

procedure TDeItem.MouseEnter;
begin
  inherited MouseEnter;
  FMouseIn := True;
end;

procedure TDeItem.MouseLeave;
begin
  inherited MouseLeave;
  FMouseIn := False;
end;

//procedure TDeItem.ParseJson(const AJsonObj: TJSONObject);
//var
//  i: Integer;
//  vS: string;
//  vDeInfo, vDeProp: TJSONObject;
//begin
//  Self.Propertys.Clear;
//
//  vS := AJsonObj.GetValue('DeType').Value;
//  if vS = 'DeItem' then
//  begin
//    vDeInfo := AJsonObj.GetValue('DeInfo') as TJSONObject;
//    Self.Text := vDeInfo.GetValue('Text').Value;
//
//    i := StrToInt(vDeInfo.GetValue('StyleNo').Value);
//    if i >= 0 then
//      Self.StyleNo := i;
//
//    vDeProp := vDeInfo.GetValue('Property') as TJSONObject;
//
//    for i := 0 to vDeProp.Count - 1 do
//    begin
//      vS := vDeProp.Pairs[i].JsonString.Value;
//      Self.Propertys.Add(vS + '=' + vDeProp.Pairs[i].JsonValue.Value);
//    end;
//  end;
//end;

procedure TDeItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  if ANode.HasAttribute('editprotect') then
    FEditProtect := ANode.Attributes['editprotect']
  else
    FEditProtect := False;

  if ANode.HasAttribute('outofrang') then
    FOutOfRang := ANode.Attributes['outofrang']
  else
    FOutOfRang := False;

  if ANode.HasAttribute('copyprotect') then
    FCopyProtect := ANode.Attributes['copyprotect']
  else
    FCopyProtect := False;

  if ANode.HasAttribute('allocvalue') then
    FAllocValue := ANode.Attributes['allocvalue']
  else
    FAllocValue := False;

  if ANode.HasAttribute('deleteallow') then
    FDeleteAllow := ANode.Attributes['deleteallow']
  else
    FDeleteAllow := True;

  FStyleEx := ANode.Attributes['styleex'];
  FPropertys.Text := GetXmlRN(ANode.Attributes['property']);
end;

procedure TDeItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStream(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FOutOfRang then
    vByte := vByte or (1 shl 6);

  if FCopyProtect then
    vByte := vByte or (1 shl 5);

  if FAllocValue then
    vByte := vByte or (1 shl 4);

  if FDeleteAllow then
    vByte := vByte or (1 shl 3);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  AStream.WriteBuffer(FStyleEx, SizeOf(TStyleExtra));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeItem.SetActive(const Value: Boolean);
begin
  if not Value then
    FMouseIn := False;
  inherited SetActive(Value);
end;

procedure TDeItem.SetText(const Value: string);
begin
  //FAllocValue := True;  // 加载时赋值不能认为是处理过值了
  if Value <> '' then
    inherited SetText(Value)
  else
  begin
    if IsElement and FEditProtect then  // 数据元值为空时默认使用名称
      Text := FPropertys.Values[TDeProp.Name]
    else
      inherited SetText('');
  end;
end;

procedure TDeItem.SetValue(const Key, Value: string);
begin
  if Pos('=', Value) > 0 then
    raise Exception.Create('属性值中不允许有"="号');

  FPropertys.Values[Key] := Value;
end;

//procedure TDeItem.ToJson(const AJsonObj: TJSONObject);
//var
//  i: Integer;
//  vDeInfo, vDeProp: TJSONObject;
//  vS: string;
//begin
//  AJsonObj.AddPair('DeType', 'DeItem');
//
//  vDeInfo := TJSONObject.Create;
//
//  vDeInfo.AddPair('StyleNo', Self.StyleNo.ToString);
//  vDeInfo.AddPair('Text', Self.Text);
//
//  vDeProp := TJSONObject.Create;
//  for i := 0 to Self.Propertys.Count - 1 do
//  begin
//    vS := Self.Propertys.Names[i];
//    vDeProp.AddPair(vS, Self.Propertys.ValueFromIndex[i]);
//  end;
//
//  vDeInfo.AddPair('Property', vDeProp);
//  AJsonObj.AddPair('DeInfo', vDeInfo);
//end;

procedure TDeItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';

  if FOutOfRang then
    ANode.Attributes['outofrang'] := '1';

  if FCopyProtect then
    ANode.Attributes['copyprotect'] := '1';

  if FAllocValue then
    ANode.Attributes['allocvalue'] := '1';

  if FDeleteAllow then
    ANode.Attributes['deleteallow'] := '1';

  ANode.Attributes['styleex'] := FStyleEx;
  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeEdit }

procedure TDeEdit.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FEditProtect := (Source as TDeEdit).EditProtect;
  FDeleteAllow := (Source as TDeEdit).DeleteAllow;
  FPropertys.Assign((Source as TDeEdit).Propertys);
end;

constructor TDeEdit.Create(const AOwnerData: THCCustomData;
  const AText: string);
begin
  FEditProtect := False;
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  inherited Create(AOwnerData, AText);
end;

destructor TDeEdit.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

function TDeEdit.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeEdit.LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
  const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 23 then
    AStream.ReadBuffer(vByte, SizeOf(vByte));

  FEditProtect := Odd(vByte shr 7);

  if AFileVersion > 34 then
    FDeleteAllow := Odd(vByte shr 6)
  else
    FDeleteAllow := True;

  HCLoadTextFromStream(AStream, vS, AFileVersion);
  FPropertys.Text := vS;
end;

//procedure TDeEdit.ParseJson(const AJsonObj: TJSONObject);
//var
//  i: Integer;
//  vDeInfo, vPropertys: TJSONObject;
//begin
//  Self.Propertys.Clear;
//
//  vDeInfo := AJsonObj.GetValue('DeInfo') as TJSONObject;
//  Self.Text := vDeInfo.GetValue('Text').Value;
//
//  vPropertys := vDeInfo.GetValue('Property') as TJSONObject;
//  for i := 0 to vPropertys.Count - 1 do
//    Self.Propertys.Add(vPropertys.Pairs[i].JsonString.Value + '=' + vPropertys.Pairs[i].JsonValue.Value);
//end;

procedure TDeEdit.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  if ANode.HasAttribute('editprotect') then
    FEditProtect := ANode.Attributes['editprotect']
  else
    FEditProtect := False;

  if ANode.HasAttribute('deleteallow') then
    FDeleteAllow := ANode.Attributes['deleteallow']
  else
    FDeleteAllow := True;

  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeEdit.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStream(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeEdit.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
end;

//procedure TDeEdit.ToJson(const AJsonObj: TJSONObject);
//var
//  i: Integer;
//  vDeInfo, vPropertys: TJSONObject;
//begin
//  AJsonObj.AddPair('DeType', 'Edit');
//
//  vPropertys := TJSONObject.Create;
//  for i := 0 to FPropertys.Count - 1 do
//    vPropertys.AddPair(FPropertys.Names[i], FPropertys.ValueFromIndex[i]);
//
//  vDeInfo := TJSONObject.Create;
//  vDeInfo.AddPair('Text', Self.Text);
//  vDeInfo.AddPair('Property',vPropertys);
//
//  AJsonObj.AddPair('DeInfo', vDeInfo);
//end;

procedure TDeEdit.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';

  if FDeleteAllow then
    ANode.Attributes['deleteallow'] := '1';

  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeCombobox }

procedure TDeCombobox.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FEditProtect := (Source as TDeCombobox).EditProtect;
  FDeleteAllow := (Source as TDeCombobox).DeleteAllow;
  FPropertys.Assign((Source as TDeCombobox).Propertys);
end;

constructor TDeCombobox.Create(const AOwnerData: THCCustomData;
  const AText: string);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  inherited Create(AOwnerData, AText);
  SaveItem := False;
end;

destructor TDeCombobox.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

function TDeCombobox.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeCombobox.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 23 then
    AStream.ReadBuffer(vByte, SizeOf(vByte));

  FEditProtect := Odd(vByte shr 7);

  if AFileVersion > 34 then
    FDeleteAllow := Odd(vByte shr 6)
  else
    FDeleteAllow := True;

  HCLoadTextFromStream(AStream, vS, AFileVersion);
  FPropertys.Text := vS;
end;

//procedure TDeCombobox.ParseJson(const AJsonObj: TJSONObject);
//var
//  i: Integer;
//  vDeInfo, vItems, vPropertys: TJSONObject;
//begin
//  Self.Items.Clear;
//  Self.Propertys.Clear;
//
//  vDeInfo := AJsonObj.GetValue('DeInfo') as TJSONObject;
//  Self.Text := vDeInfo.GetValue('Text').Value;
//  vItems := vDeInfo.GetValue('Items') as TJSONObject;
//  for i := 0 to vItems.Count - 1 do
//    Self.Items.Add(vItems.Pairs[i].JsonValue.Value);
//
//  vPropertys := vDeInfo.GetValue('Property') as TJSONObject;
//  for i := 0 to vPropertys.Count - 1 do
//    Self.Propertys.Add(vPropertys.Pairs[i].JsonString.Value + '=' + vPropertys.Pairs[i].JsonValue.Value);
//end;

procedure TDeCombobox.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  if ANode.HasAttribute('editprotect') then
    FEditProtect := ANode.Attributes['editprotect']
  else
    FEditProtect := False;

  if ANode.HasAttribute('deleteallow') then
    FDeleteAllow := ANode.Attributes['deleteallow']
  else
    FDeleteAllow := True;

  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeCombobox.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStream(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeCombobox.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
end;

//procedure TDeCombobox.ToJson(const AJsonObj: TJSONObject);
//var
//  i: Integer;
//  vDeInfo, vItems, vPropertys: TJSONObject;
//begin
//  AJsonObj.AddPair('DeType', 'Combobox');
//
//  vPropertys := TJSONObject.Create;
//  for i := 0 to FPropertys.Count - 1 do
//    vPropertys.AddPair(FPropertys.Names[i], FPropertys.ValueFromIndex[i]);
//
//  vItems := TJSONObject.Create;
//  for i := 0 to Self.Items.Count - 1 do
//    vItems.AddPair(i.ToString, Self.Items[i]);
//
//  vDeInfo := TJSONObject.Create;
//  vDeInfo.AddPair('Text', Self.Text);
//  vDeInfo.AddPair('Items', vItems);
//  vDeInfo.AddPair('Property',vPropertys);
//
//  AJsonObj.AddPair('DeInfo', vDeInfo);
//end;

procedure TDeCombobox.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';

  if FDeleteAllow then
    ANode.Attributes['deleteallow'] := '1';

  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeDateTimePicker }

procedure TDeDateTimePicker.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FEditProtect := (Source as TDeDateTimePicker).EditProtect;
  FDeleteAllow := (Source as TDeDateTimePicker).DeleteAllow;
  FPropertys.Assign((Source as TDeDateTimePicker).Propertys);
end;

constructor TDeDateTimePicker.Create(const AOwnerData: THCCustomData;
  const ADateTime: TDateTime);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  inherited Create(AOwnerData, ADateTime);
end;

destructor TDeDateTimePicker.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

function TDeDateTimePicker.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeDateTimePicker.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 23 then
    AStream.ReadBuffer(vByte, SizeOf(vByte));

  FEditProtect := Odd(vByte shr 7);

  if AFileVersion > 34 then
    FDeleteAllow := Odd(vByte shr 6)
  else
    FDeleteAllow := True;

  HCLoadTextFromStream(AStream, vS, AFileVersion);
  FPropertys.Text := vS;
end;

//procedure TDeDateTimePicker.ParseJson(const AJsonObj: TJSONObject);
//begin
//
//end;

procedure TDeDateTimePicker.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  if ANode.HasAttribute('editprotect') then
    FEditProtect := ANode.Attributes['editprotect']
  else
    FEditProtect := False;

  if ANode.HasAttribute('deleteallow') then
    FDeleteAllow := ANode.Attributes['deleteallow']
  else
    FDeleteAllow := True;

  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeDateTimePicker.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStream(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeDateTimePicker.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
end;

//procedure TDeDateTimePicker.ToJson(const AJsonObj: TJSONObject);
//begin
//
//end;

procedure TDeDateTimePicker.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';

  if FDeleteAllow then
    ANode.Attributes['deleteallow'] := '1';

  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeRadioGroup }

procedure TDeRadioGroup.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FEditProtect := (Source as TDeRadioGroup).EditProtect;
  FDeleteAllow := (Source as TDeRadioGroup).DeleteAllow;
  FPropertys.Assign((Source as TDeRadioGroup).Propertys);
end;

constructor TDeRadioGroup.Create(const AOwnerData: THCCustomData);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  inherited Create(AOwnerData);
end;

destructor TDeRadioGroup.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

function TDeRadioGroup.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeRadioGroup.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 23 then
    AStream.ReadBuffer(vByte, SizeOf(vByte));

  FEditProtect := Odd(vByte shr 7);

  if AFileVersion > 34 then
    FDeleteAllow := Odd(vByte shr 6)
  else
    FDeleteAllow := True;

  HCLoadTextFromStream(AStream, vS, AFileVersion);
  FPropertys.Text := vS;
end;

//procedure TDeRadioGroup.ParseJson(const AJsonObj: TJSONObject);
//begin
//
//end;

procedure TDeRadioGroup.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  if ANode.HasAttribute('editprotect') then
    FEditProtect := ANode.Attributes['editprotect']
  else
    FEditProtect := False;

  if ANode.HasAttribute('deleteallow') then
    FDeleteAllow := ANode.Attributes['deleteallow']
  else
    FDeleteAllow := True;

  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeRadioGroup.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStream(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeRadioGroup.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
end;

//procedure TDeRadioGroup.ToJson(const AJsonObj: TJSONObject);
//begin
//
//end;

procedure TDeRadioGroup.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';

  if FDeleteAllow then
    ANode.Attributes['deleteallow'] := '1';

  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeTable }

procedure TDeTable.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FEditProtect := (Source as TDeTable).EditProtect;
  FDeleteAllow := (Source as TDeTable).DeleteAllow;
  FPropertys.Assign((Source as TDeTable).Propertys);
end;

constructor TDeTable.Create(const AOwnerData: THCCustomData; const ARowCount,
  AColCount, AWidth: Integer);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  inherited Create(AOwnerData, ARowCount, AColCount, AWidth);
end;

destructor TDeTable.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

function TDeTable.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeTable.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 23 then
    AStream.ReadBuffer(vByte, SizeOf(vByte));

  FEditProtect := Odd(vByte shr 7);

  if AFileVersion > 34 then
    FDeleteAllow := Odd(vByte shr 6)
  else
    FDeleteAllow := True;

  HCLoadTextFromStream(AStream, vS, AFileVersion);
  FPropertys.Text := vS;
end;

//procedure TDeTable.ParseJson(const AJsonObj: TJSONObject);
//var
//  i, j, vR, vC: Integer;
//  r, g, b: Byte;
//  vS: string;
//  vCells, vCellInfo, vItems, vDeInfo, vJson: TJSONObject;
//  vDeItem: TDeItem;
//  vArrayString: TArray<string>;
//begin
//  vCells := AJsonObj.GetValue('Cells') as TJSONObject;
//
//  for i := 0 to vCells.Count - 1 do
//  begin
//    vS := vCells.Pairs[i].JsonString.Value;
//    vR := StrToInt(System.Copy(vS, 1, Pos(',', vS) - 1));
//    vC := StrToInt(System.Copy(vS, Pos(',', vS) + 1, vS.Length));
//
//    vCellInfo := vCells.Pairs[i].JsonValue as TJSONObject;
//
//    Self.Cells[vR, vC].RowSpan := StrToInt(vCellInfo.GetValue('RowSpan').Value);
//    Self.Cells[vR, vC].ColSpan := StrToInt(vCellInfo.GetValue('ColSpan').Value);
//
//    if (Self.Cells[vR, vC].RowSpan < 0) or (Self.Cells[vR, vC].ColSpan < 0) then
//    begin
//      Self.Cells[vR, vC].CellData.Free;
//      Self.Cells[vR, vC].CellData := nil;
//    end
//    else
//    begin
//      if vCellInfo.GetValue('BorderSides-Left').Value = 'False' then
//        Self.Cells[vR, vC].BorderSides := Self.Cells[vR, vC].BorderSides - [cbsLeft];
//      if vCellInfo.GetValue('BorderSides-Top').Value = 'False' then
//        Self.Cells[vR, vC].BorderSides := Self.Cells[vR, vC].BorderSides - [cbsTop];
//      if vCellInfo.GetValue('BorderSides-Right').Value = 'False' then
//        Self.Cells[vR, vC].BorderSides := Self.Cells[vR, vC].BorderSides - [cbsRight];
//      if vCellInfo.GetValue('BorderSides-Bottom').Value = 'False' then
//        Self.Cells[vR, vC].BorderSides := Self.Cells[vR, vC].BorderSides - [cbsBottom];
//
//      vS := vCellInfo.GetValue('BackgroundColor').Value;
//
//      vArrayString := vS.Split([',']);
//      r := StrToInt(vArrayString[0]);
//      g := StrToInt(vArrayString[1]);
//      b := StrToInt(vArrayString[2]);
//      Self.Cells[vR, vC].BackgroundColor := RGB(r, g, b);
//
//      vItems := vCellInfo.GetValue('Items') as TJSONObject;
//      for j := 0 to vItems.Count - 1 do
//      begin
//        vJson := vItems.Pairs[j].JsonValue as TJSONObject;
//        vS := vJson.GetValue('DeType').Value;
//        if vS = 'DeItem' then
//        begin
//          vDeInfo := vJson.GetValue('DeInfo') as TJSONObject;
//          vS := vDeInfo.GetValue('Text').Value;
//          if vS <> '' then
//          begin
//            vDeItem := TDeItem.Create;  // Text
//            vDeItem.ParseJson(vJson);
//
//            Self.Cells[vR, vC].CellData.InsertItem(vDeItem);
//          end;
//        end
//        else
//        if vS = 'DeText' then
//        begin
//          vDeInfo := vJson.GetValue('DeInfo') as TJSONObject;
//          vS := vDeInfo.GetValue('Text').Value;
//          if vS <> '' then
//            Self.Cells[vR, vC].CellData.InsertText(vS);
//        end;
//      end;
//
//      Self.Cells[vR, vC].CellData.ReadOnly := vCellInfo.GetValue('ReadOnly').Value = 'True';
//    end;
//  end;
//end;

procedure TDeTable.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  if ANode.HasAttribute('editprotect') then
    FEditProtect := ANode.Attributes['editprotect']
  else
    FEditProtect := False;

  if ANode.HasAttribute('deleteallow') then
    FDeleteAllow := ANode.Attributes['deleteallow']
  else
    FDeleteAllow := True;

  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeTable.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStream(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeTable.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
end;

//procedure TDeTable.ToJson(const AJsonObj: TJSONObject);
//
//  procedure TColor2RGB(const Color: LongInt; var R, G, B: Byte);
//  begin
//    R := Color and $FF;
//    G := (Color shr 8) and $FF;
//    B := (Color shr 16) and $FF;
//  end;
//
//var
//  vDeInfo, vCells, vCellInfo, vCellItems, vItemInfo: TJSONObject;
//  i, vR, vC: Integer;
//  r, g, b: Byte;
//  vTableCell: THCTableCell;
//begin
//  AJsonObj.AddPair('DeType', 'Table');
//
//  vDeInfo := TJSONObject.Create;
//  vDeInfo.AddPair('RowCount', Self.RowCount.ToString);
//  vDeInfo.AddPair('ColCount', Self.ColCount.ToString);
//
//  vCells := TJSONObject.Create;
//  for vR := 0 to Self.RowCount - 1 do
//  begin
//    for vC := 0 to Self.ColCount - 1 do
//    begin
//      vTableCell := Self.Cells[vR, vC];
//
//      vCellInfo := TJSONObject.Create;
//      vCellInfo.AddPair('RowSpan', vTableCell.RowSpan.ToString);
//      vCellInfo.AddPair('ColSpan', vTableCell.ColSpan.ToString);
//
//      if (vTableCell.RowSpan >= 0) and (vTableCell.ColSpan >= 0) then
//      begin
//        if vTableCell.CellData.ReadOnly then
//          vCellInfo.AddPair('ReadOnly', 'True')
//        else
//          vCellInfo.AddPair('ReadOnly', 'False');
//
//        if cbsLeft in vTableCell.BorderSides then
//          vCellInfo.AddPair('BorderSides-Left', 'True')
//        else
//          vCellInfo.AddPair('BorderSides-Left', 'False');
//        if cbsTop in vTableCell.BorderSides then
//          vCellInfo.AddPair('BorderSides-Top', 'True')
//        else
//          vCellInfo.AddPair('BorderSides-Top', 'False');
//        if cbsRight in vTableCell.BorderSides then
//          vCellInfo.AddPair('BorderSides-Right', 'True')
//        else
//          vCellInfo.AddPair('BorderSides-Right', 'False');
//        if cbsBottom in vTableCell.BorderSides then
//          vCellInfo.AddPair('BorderSides-Bottom', 'True')
//        else
//          vCellInfo.AddPair('BorderSides-Bottom', 'False');
//
//        TColor2RGB(ColorToRGB(vTableCell.BackgroundColor), r, g, b);
//        vCellInfo.AddPair('BackgroundColor', r.ToString + ',' + g.ToString + ',' + b.ToString);
//
//        vCellItems := TJSONObject.Create;
//        for i := 0 to vTableCell.CellData.Items.Count - 1 do
//        begin
//          if vTableCell.CellData.Items[i] is TDeItem then
//          begin
//            vItemInfo := TJSONObject.Create;
//            (vTableCell.CellData.Items[i] as TDeItem).ToJson(vItemInfo);
//            vCellItems.AddPair(i.ToString, vItemInfo);
//          end;
//        end;
//
//        vCellInfo.AddPair('Items', vCellItems);
//      end;
//
//      vCells.AddPair(vR.ToString + ',' + vC.ToString, vCellInfo);
//    end;
//  end;
//
//  vDeInfo.AddPair('Cells', vCells);
//  AJsonObj.AddPair('DeInfo', vDeInfo);
//end;

procedure TDeTable.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';

  if FDeleteAllow then
    ANode.Attributes['deleteallow'] := '1';

  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeCheckBox }

procedure TDeCheckBox.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FEditProtect := (Source as TDeCheckBox).EditProtect;
  FDeleteAllow := (Source as TDeCheckBox).DeleteAllow;
  FPropertys.Assign((Source as TDeCheckBox).Propertys);
end;

constructor TDeCheckBox.Create(const AOwnerData: THCCustomData;
  const AText: string; const AChecked: Boolean);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  inherited Create(AOwnerData, AText, AChecked);
end;

destructor TDeCheckBox.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited;
end;

function TDeCheckBox.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeCheckBox.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 23 then
    AStream.ReadBuffer(vByte, SizeOf(vByte));

  FEditProtect := Odd(vByte shr 7);

  if AFileVersion > 34 then
    FDeleteAllow := Odd(vByte shr 6)
  else
    FDeleteAllow := True;

  HCLoadTextFromStream(AStream, vS, AFileVersion);
  FPropertys.Text := vS;
end;

//procedure TDeCheckBox.ParseJson(const AJsonObj: TJSONObject);
//begin
//
//end;

procedure TDeCheckBox.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  if ANode.HasAttribute('editprotect') then
    FEditProtect := ANode.Attributes['editprotect']
  else
    FEditProtect := False;

  if ANode.HasAttribute('deleteallow') then
    FDeleteAllow := ANode.Attributes['deleteallow']
  else
    FDeleteAllow := True;

  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeCheckBox.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStream(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeCheckBox.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
end;

//procedure TDeCheckBox.ToJson(const AJsonObj: TJSONObject);
//begin
//
//end;

procedure TDeCheckBox.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';

  if FDeleteAllow then
    ANode.Attributes['deleteallow'] := '1';

  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeFloatBarCodeItem }

procedure TDeFloatBarCodeItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FEditProtect := (Source as TDeFloatBarCodeItem).EditProtect;
  FDeleteAllow := (Source as TDeFloatBarCodeItem).DeleteAllow;
  FPropertys.Assign((Source as TDeFloatBarCodeItem).Propertys);
end;

constructor TDeFloatBarCodeItem.Create(const AOwnerData: THCCustomData);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  inherited Create(AOwnerData);
end;

destructor TDeFloatBarCodeItem.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

function TDeFloatBarCodeItem.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeFloatBarCodeItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 23 then
    AStream.ReadBuffer(vByte, SizeOf(vByte));

  FEditProtect := Odd(vByte shr 7);

  if AFileVersion > 34 then
    FDeleteAllow := Odd(vByte shr 6)
  else
    FDeleteAllow := True;

  HCLoadTextFromStream(AStream, vS, AFileVersion);
  FPropertys.Text := vS;
end;

//procedure TDeFloatBarCodeItem.ParseJson(const AJsonObj: TJSONObject);
//begin
//
//end;

procedure TDeFloatBarCodeItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  if ANode.HasAttribute('editprotect') then
    FEditProtect := ANode.Attributes['editprotect']
  else
    FEditProtect := False;

  if ANode.HasAttribute('deleteallow') then
    FDeleteAllow := ANode.Attributes['deleteallow']
  else
    FDeleteAllow := True;

  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeFloatBarCodeItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStream(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeFloatBarCodeItem.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
end;

//procedure TDeFloatBarCodeItem.ToJson(const AJsonObj: TJSONObject);
//begin
//
//end;

procedure TDeFloatBarCodeItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';

  if FDeleteAllow then
    ANode.Attributes['deleteallow'] := '1';

  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeImageItem }

procedure TDeImageItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FEditProtect := (Source as TDeImageItem).EditProtect;
  FDeleteAllow := (Source as TDeImageItem).DeleteAllow;
  FPropertys.Assign((Source as TDeImageItem).Propertys);
end;

constructor TDeImageItem.Create(const AOwnerData: THCCustomData);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  inherited Create(AOwnerData);
end;

destructor TDeImageItem.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

procedure TDeImageItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  if Self.Image.Empty and Self.Active and (not APaintInfo.Print) then  // 非打印状态下的空白图片
  begin
    ACanvas.Font.Size := 12;
    ACanvas.Font.Style := [fsItalic];
    ACanvas.TextOut(ADrawRect.Left + 2, ADrawRect.Top + 2, 'DeIndex:' + Self[TDeProp.Index]);
  end;
end;

function TDeImageItem.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeImageItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(vByte, SizeOf(vByte));

  FEditProtect := Odd(vByte shr 7);

  if AFileVersion > 34 then
    FDeleteAllow := Odd(vByte shr 6)
  else
    FDeleteAllow := True;

  HCLoadTextFromStream(AStream, vS, AFileVersion);
  FPropertys.Text := vS;
end;

//procedure TDeImageItem.ParseJson(const AJsonObj: TJSONObject);
//begin
//
//end;

procedure TDeImageItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  if ANode.HasAttribute('editprotect') then
    FEditProtect := ANode.Attributes['editprotect']
  else
    FEditProtect := False;

  if ANode.HasAttribute('deleteallow') then
    FDeleteAllow := ANode.Attributes['deleteallow']
  else
    FDeleteAllow := True;

  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeImageItem.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStream(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeImageItem.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
end;

//procedure TDeImageItem.ToJson(const AJsonObj: TJSONObject);
//begin
//
//end;

procedure TDeImageItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';

  if FDeleteAllow then
    ANode.Attributes['deleteallow'] := '1';

  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TEmrTextItem }

destructor TEmrTextItem.Destroy;
begin
  if Assigned(FSyntaxs) then
    FreeAndNil(FSyntaxs);

  inherited Destroy;
end;

procedure TEmrTextItem.SyntaxAdd(const AOffset, ALength: Integer; const AProblem: TEmrSyntaxProblem);
var
  vSyntax: TEmrSyntax;
begin
  vSyntax := TEmrSyntax.Create;
  vSyntax.Offset := AOffset;
  vSyntax.Length := ALength;
  vSyntax.Problem := AProblem;
  if not Assigned(FSyntaxs) then
    FSyntaxs := TObjectList<TEmrSyntax>.Create;

  FSyntaxs.Add(vSyntax);
end;

procedure TEmrTextItem.SyntaxClear;
begin
  if Assigned(FSyntaxs) then
    FSyntaxs.Clear;
end;

function TEmrTextItem.SyntaxCount: Integer;
begin
  if Assigned(FSyntaxs) then
    Result := FSyntaxs.Count
  else
    Result := 0;
end;

end.
