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
  HCTextItem, HCEditItem, HCComboboxItem, HCDateTimePicker, HCRadioGroup, HCTableItem, HCTableRow,
  HCTableCell, HCCheckBoxItem, HCFractionItem, HCFloatBarCodeItem, HCCommon, HCButtonItem,
  HCCustomData, HCXml, HCImageItem, Generics.Collections
  {$IFDEF VER320}
  , System.JSON
  {$ENDIF}
  ;

const
  EMRSTYLE_TOOTH = -1001;  // 牙齿公式 THCStyle.Custom - 1
  EMRSTYLE_FANGJIAO = -1002;  // 房角公式 THCStyle.Custom - 2
  EMRSTYLE_YUEJING = -1003;  // 月经公式

  EmrViewVersion = 1;  // Byte类型的文件版本

type
  TDeTraceStyle = (cseDel, cseAdd, cseMod);  // 痕迹样式，删除，新增，修改
  TDeTraceStyles = set of TDeTraceStyle;

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

      /// <summary> 痕迹信息(为兼容历史) </summary>
      Trace = 'Trace';
      /// <summary> 删除痕迹信息 </summary>
      TraceDel = 'TcDel';
      /// <summary> 添加痕迹信息 </summary>
      TraceAdd = 'TcAdd';
      /// <summary> 删除痕迹的级别 </summary>
      TraceDelLevel = 'TcDelL';
      /// <summary> 添加痕迹的级别 </summary>
      TraceAddLevel = 'TcAddL';

      /// <summary> 隐私信息 </summary>
      Secret = 'Secret';
  end;

  TDeTraceLevel = class(TObject)
  public
    const
      /// <summary> 无医师级别 </summary>
      None = '';
      /// <summary> 住院医师 </summary>
      One = '1';
      /// <summary> 主治医师 </summary>
      Two = '2';
      /// <summary> 主任医师 </summary>
      Three = '3';
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
    FIsElement,
    FEditProtect,  // 编辑保护、不允许处理值
    FCopyProtect,  // 复制保护，不允许复制
    FDeleteAllow,  // 是否允许删除
    FAllocOnly,
    FAllocValue  // 是否分配过值
      : Boolean;
    FTraceStyles: TDeTraceStyles;
    FPropertys, FScripts: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
    function GetIndex: string;
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
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    function ToHtml(const APath: string): string; override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    procedure PropertyChange;
    {$IFDEF VER320}
    procedure ToJson(const AJsonObj: TJSONObject);
    procedure ParseJson(const AJsonObj: TJSONObject);
    {$ENDIF}
    property IsElement: Boolean read FIsElement;
    property MouseIn: Boolean read FMouseIn;
    property TraceStyles: TDeTraceStyles read FTraceStyles write FTraceStyles;
    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property CopyProtect: Boolean read FCopyProtect write FCopyProtect;
    property AllocOnly: Boolean read FAllocOnly write FAllocOnly;
    property AllocValue: Boolean read FAllocValue write FAllocValue;
    property OutOfRang: Boolean read FOutOfRang write FOutOfRang;
    property Propertys: TStringList read FPropertys;
    property Index: string read GetIndex;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeTableRow = class(THCTableRow)
  private
    FEditProtect: Boolean;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  protected
    function DoCreateCell(const AStyle: THCStyle): THCTableCell; override;
  public
    constructor Create(const AStyle: THCStyle; const AColCount: Integer); override;
    destructor Destroy; override;
    procedure SaveToStream(const AStream: TStream); override;
    procedure LoadFromStream(const AStream: TStream; const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeTableCell = class(THCTableCell)
  private
    FEditProtect: Boolean;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
    procedure SetEditProtect(const Value: Boolean);
  public
    constructor Create(const AStyle: THCStyle); override;
    destructor Destroy; override;
    procedure SaveToStream(const AStream: TStream); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    property Propertys: TStringList read FPropertys;
    property EditProtect: Boolean read FEditProtect write SetEditProtect;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeTable = class(THCTableItem)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys, FScripts: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  public
    procedure CellChangeByAction(const ARow, ACol: Integer; const AProcedure: THCProcedure); override;
    function DoCreateRow(const AStyle: THCStyle; const AColCount: Integer): THCTableRow; override;
    constructor Create(const AOwnerData: THCCustomData; const ARowCount, AColCount,
      AWidth: Integer); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    {$IFDEF CompilerVersion > 32}  // VER320
    procedure ToJson(const AJsonObj: TJSONObject); override;
    procedure ParseJson(const AJsonObj: TJSONObject); override;
    {$ENDIF}
    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeCheckBox = class(THCCheckBoxItem)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys, FScripts: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  protected
    procedure DoSetChecked(const Value: Boolean); override;
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string; const AChecked: Boolean); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
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

  TDeButton = class(THCButtonItem)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys, FScripts: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeEdit = class(THCEditItem)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys, FScripts: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;
    function InsertText(const AText: string): Boolean; override;
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    {$IFDEF VER320}
    procedure ToJson(const AJsonObj: TJSONObject);
    procedure ParseJson(const AJsonObj: TJSONObject);
    {$ENDIF}
    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeCombobox = class(THCComboboxItem)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys, FScripts: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoPopup; override;
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;
    function InsertText(const AText: string): Boolean; override;
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    {$IFDEF VER320}
    procedure ToJson(const AJsonObj: TJSONObject);
    procedure ParseJson(const AJsonObj: TJSONObject);
    {$ENDIF}
    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeDateTimePicker = class(THCDateTimePicker)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys, FScripts: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(const AOwnerData: THCCustomData; const ADateTime: TDateTime); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;
    function InsertText(const AText: string): Boolean; override;
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
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
    FPropertys, FScripts: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  protected
    procedure DoSetItemChecked(const AIndex: Integer; const Value: Boolean); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
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

  TDeBarCodeItem = class(THCBarCodeItem)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

    property EditProtect: Boolean read FEditProtect write FEditProtect;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeQRCodeItem = class(THCQRCodeItem)
  private
    FEditProtect, FDeleteAllow: Boolean;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;

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

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
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
    FPropertys, FScripts: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
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

var
  ToHtmlUseTrace: Boolean = False;

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

    THCStyle.Button:
      Result := TDeButton.Create(AData, '');

    THCStyle.RadioGroup:
      Result := TDeRadioGroup.Create(AData);

    THCStyle.Express, EMRSTYLE_YUEJING:
      Result := TEmrYueJingItem.Create(AData, '', '', '', '');

    EMRSTYLE_TOOTH:
      Result := TEmrToothItem.Create(AData, '', '', '', '');

    EMRSTYLE_FANGJIAO:
      Result := TEmrFangJiaoItem.Create(AData, '', '', '', '');

    THCStyle.BarCode:
      Result := TDeBarCodeItem.Create(AData, '');

    THCStyle.QRCode:
      Result := TDeQRCodeItem.Create(AData, '');

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
  FTraceStyles := (Source as TDeItem).TraceStyles;
  FEditProtect := (Source as TDeItem).EditProtect;
  FDeleteAllow := (Source as TDeItem).DeleteAllow;
  FCopyProtect := (Source as TDeItem).CopyProtect;
  FAllocOnly := (Source as TDeItem).AllocOnly;
  FAllocValue := (Source as TDeItem).AllocValue;
  FOutOfRang := (Source as TDeItem).OutOfRang;
  FPropertys.Assign((Source as TDeItem).Propertys);
  PropertyChange;
  FScripts.Assign((Source as TDeItem).FScripts);
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
          if FEditProtect or FAllocOnly then  // 两头允许输入，触发actConcatText时返回供Data层处理新TextItem还是连接
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
          if FEditProtect or FAllocOnly then
            Result := AOffset = 0;
        end;

      actDeleteText:
        begin
          if FEditProtect or FAllocOnly then
            Result := AOffset = Self.Length;
        end;

      actDeleteItem:
        begin
          Result := not FEditProtect;
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
      and (FTraceStyles = vDeItem.TraceStyles)
      and (FEditProtect = vDeItem.EditProtect)
      and (FDeleteAllow = vDeItem.DeleteAllow)
      and (FCopyProtect = vDeItem.CopyProtect)
      and (Self[TDeProp.TraceDel] = vDeItem[TDeProp.TraceDel])
      and (Self[TDeProp.TraceAdd] = vDeItem[TDeProp.TraceAdd])
      and (Self[TDeProp.TraceDelLevel] = vDeItem[TDeProp.TraceDelLevel])
      and (Self[TDeProp.TraceAddLevel] = vDeItem[TDeProp.TraceAddLevel]);
  end;
end;

constructor TDeItem.Create;
begin
  inherited Create;
  FPropertys := TStringList.Create;
  FScripts := CreateScriptObject;

  FAllocValue := False;
  FAllocOnly := False;
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
  FreeAndNil(FScripts);
  inherited Destroy;
end;

function TDeItem.GetHint: string;
var
  vsAdd, vsDel: string;
begin
  if FTraceStyles = [] then
    Result := Self[TDeProp.Name]
  else
  begin
    vsAdd := Self[TDeProp.TraceAdd];
    vsDel := Self[TDeProp.TraceDel];
    if vsAdd <> '' then
    begin
      if vsDel <> '' then
        Result := vsAdd + #13#10 + vsDel
      else
        Result := vsAdd;
    end
    else
      Result := vsDel;
  end;
end;

function TDeItem.GetIndex: string;
begin
  Result := Self[TDeProp.Index];
end;

procedure TDeItem.PropertyChange;
begin
  FIsElement := FPropertys.IndexOfName(TDeProp.Index) >= 0;
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

  FAllocOnly := Odd(vByte shr 2);

  if AFileVersion > 46 then
    AStream.ReadBuffer(FTraceStyles, SizeOf(TDeTraceStyles))
  else
  begin
    AStream.ReadBuffer(vByte, SizeOf(vByte));
    if vByte = 0 then
      FTraceStyles := []
    else
    if vByte = 1 then
      FTraceStyles := FTraceStyles + [TDeTraceStyle.cseDel]
    else
      FTraceStyles := FTraceStyles + [TDeTraceStyle.cseAdd];
  end;

  HCLoadTextFromStream(AStream, vS, AFileVersion);
  FPropertys.Text := vS;

  if AFileVersion <= 46 then
  begin
    if Self[TDeProp.Trace] <> '' then
    begin
      if vByte = 0 then

      else
      if vByte = 1 then
        Self[TDeProp.TraceDel] := Self[TDeProp.Trace]
      else
        Self[TDeProp.TraceAdd] := Self[TDeProp.Trace];

      Self[TDeProp.Trace] := '';
    end;
  end;

  PropertyChange;
  if AFileVersion > 57 then
  begin
    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FScripts.Text := vS;
  end;
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

{$IFDEF VER320}
procedure TDeItem.ParseJson(const AJsonObj: TJSONObject);
var
  i: Integer;
  vS: string;
  vDeInfo, vDeProp: TJSONObject;
begin
  Self.Propertys.Clear;

  vS := AJsonObj.GetValue('DeType').Value;
  if vS = 'DeItem' then
  begin
    vDeInfo := AJsonObj.GetValue('DeInfo') as TJSONObject;
    Self.Text := vDeInfo.GetValue('Text').Value;

    i := StrToInt(vDeInfo.GetValue('StyleNo').Value);
    if i >= 0 then
      Self.StyleNo := i;

    vDeProp := vDeInfo.GetValue('Property') as TJSONObject;

    for i := 0 to vDeProp.Count - 1 do
    begin
      vS := vDeProp.Pairs[i].JsonString.Value;
      Self.Propertys.Add(vS + '=' + vDeProp.Pairs[i].JsonValue.Value);
    end;
  end;
end;

procedure TDeItem.ToJson(const AJsonObj: TJSONObject);
var
  i: Integer;
  vDeInfo, vDeProp: TJSONObject;
  vS: string;
begin
  AJsonObj.AddPair('DeType', 'DeItem');

  vDeInfo := TJSONObject.Create;

  vDeInfo.AddPair('StyleNo', Self.StyleNo.ToString);
  vDeInfo.AddPair('Text', Self.Text);

  vDeProp := TJSONObject.Create;
  for i := 0 to Self.Propertys.Count - 1 do
  begin
    vS := Self.Propertys.Names[i];
    vDeProp.AddPair(vS, Self.Propertys.ValueFromIndex[i]);
  end;

  vDeInfo.AddPair('Property', vDeProp);
  AJsonObj.AddPair('DeInfo', vDeInfo);
end;
{$ENDIF}

procedure TDeItem.ParseXml(const ANode: IHCXMLNode);
var
  vByte: Byte;
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

  if ANode.HasAttribute('alloconly') then
    FAllocOnly := ANode.Attributes['alloconly']
  else
    FAllocOnly := False;

  if ANode.HasAttribute('styleex') then
  begin
    vByte := ANode.Attributes['styleex'];
    if vByte = 0 then
      FTraceStyles := []
    else
    if vByte = 1 then
      FTraceStyles := FTraceStyles + [TDeTraceStyle.cseDel]
    else
      FTraceStyles := FTraceStyles + [TDeTraceStyle.cseAdd];
  end
  else
  if ANode.HasAttribute('tracestyle') then
  begin
    vByte := ANode.Attributes['tracestyle'];
    if vByte = 0 then
      FTraceStyles := []
    else
    if vByte = 1 then
      FTraceStyles := FTraceStyles + [TDeTraceStyle.cseDel]
    else
      FTraceStyles := FTraceStyles + [TDeTraceStyle.cseAdd];
  end
  else
  if ANode.HasAttribute('tracestyles') then
    FTraceStyles := TDeTraceStyles(Byte(ANode.Attributes['tracestyles']));

  if ANode.HasAttribute('property') then
    FPropertys.Text := GetXmlRN(ANode.Attributes['property']);
end;

procedure TDeItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

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

  if FAllocOnly then
    vByte := vByte or (1 shl 2);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  AStream.WriteBuffer(FTraceStyles, SizeOf(TDeTraceStyles));
  HCSaveTextToStream(AStream, FPropertys.Text);
  HCSaveTextToStream(AStream, FScripts.Text);
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
  else  // 设置为空字符
  begin
    if IsElement then  // 数据元值为空时默认使用名称
      Text := FPropertys.Values[TDeProp.Name]
    else
      inherited SetText('');
  end;
end;

procedure TDeItem.SetValue(const Key, Value: string);
begin
  if Key = 'Text' then
  begin
    //if Value <> '' then  // Text的修改需要经过Data层，因为需要重新格式化
    //  Self.Text := Value;
  end
  else
  if Key = 'EditProtect' then
    Self.FEditProtect := StrToBoolDef(Value, Self.FEditProtect)
  else
  if Key = 'DeleteAllow' then
    Self.FDeleteAllow := StrToBoolDef(Value, Self.FDeleteAllow)
  else
  if Key = 'CopyProtect' then
    Self.FCopyProtect := StrToBoolDef(Value, Self.FCopyProtect)
  else
  if Key = 'AllocValue' then
    Self.FAllocValue := StrToBoolDef(Value,Self.FAllocValue)
  else
  begin
    HCSetProperty(FPropertys, Key, Value);
    PropertyChange;
  end;
end;

function TDeItem.ToHtml(const APath: string): string;
var
  vStyle: string;
begin
  if not ToHtmlUseTrace then
  begin
    if cseDel in FTraceStyles then
      Result := ''
    else
      Result := inherited ToHtml(APath);
  end
  else
  begin
    vStyle := '';
    Result := '<a class="fs' + IntToStr(StyleNo) + '"';
    if cseAdd in FTraceStyles then
      vStyle := ' underline';

    if cseDel in FTraceStyles then
      vStyle := vStyle + ' line-through';

    if vStyle <> '' then
      vStyle := ' style="text-decoration:' + vStyle + '"';

    Result := Result + vStyle + '>' + Text + '</a>';
  end;
end;

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

  if FAllocOnly then
    ANode.Attributes['alloconly'] := '1';

  ANode.Attributes['tracestyles'] := Byte(FTraceStyles);
  if FPropertys.Text <> '' then
    ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeEdit }

procedure TDeEdit.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FEditProtect := (Source as TDeEdit).EditProtect;
  FDeleteAllow := (Source as TDeEdit).DeleteAllow;
  FPropertys.Assign((Source as TDeEdit).Propertys);
  FScripts.Assign((Source as TDeEdit).FScripts);
end;

constructor TDeEdit.Create(const AOwnerData: THCCustomData;
  const AText: string);
begin
  FEditProtect := False;
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  FScripts := CreateScriptObject;
  inherited Create(AOwnerData, AText);
end;

destructor TDeEdit.Destroy;
begin
  FreeAndNil(FPropertys);
  FreeAndNil(FScripts);
  inherited Destroy;
end;

function TDeEdit.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

function TDeEdit.InsertText(const AText: string): Boolean;
begin
  if not FEditProtect then
    Result := inherited InsertText(AText)
  else
    Result := False;
end;

procedure TDeEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not FEditProtect then
    inherited KeyDown(Key, Shift);
end;

procedure TDeEdit.KeyPress(var Key: Char);
begin
  if not FEditProtect then
    inherited KeyPress(Key);
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
  if AFileVersion > 57 then
  begin
    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FScripts.Text := vS;
  end;
end;

{$IFDEF VER320}
procedure TDeEdit.ParseJson(const AJsonObj: TJSONObject);
var
  i: Integer;
  vDeInfo, vPropertys: TJSONObject;
begin
  Self.Propertys.Clear;

  vDeInfo := AJsonObj.GetValue('DeInfo') as TJSONObject;
  Self.Text := vDeInfo.GetValue('Text').Value;

  vPropertys := vDeInfo.GetValue('Property') as TJSONObject;
  for i := 0 to vPropertys.Count - 1 do
    Self.Propertys.Add(vPropertys.Pairs[i].JsonString.Value + '=' + vPropertys.Pairs[i].JsonValue.Value);
end;

procedure TDeEdit.ToJson(const AJsonObj: TJSONObject);
var
  i: Integer;
  vDeInfo, vPropertys: TJSONObject;
begin
  AJsonObj.AddPair('DeType', 'Edit');

  vPropertys := TJSONObject.Create;
  for i := 0 to FPropertys.Count - 1 do
    vPropertys.AddPair(FPropertys.Names[i], FPropertys.ValueFromIndex[i]);

  vDeInfo := TJSONObject.Create;
  vDeInfo.AddPair('Text', Self.Text);
  vDeInfo.AddPair('Property',vPropertys);

  AJsonObj.AddPair('DeInfo', vDeInfo);
end;
{$ENDIF}

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

procedure TDeEdit.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
  HCSaveTextToStream(AStream, FScripts.Text);
end;

procedure TDeEdit.SetValue(const Key, Value: string);
begin
  HCSetProperty(FPropertys, Key, Value);
end;

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
  FScripts.Assign((Source as TDeCombobox).FScripts);
end;

constructor TDeCombobox.Create(const AOwnerData: THCCustomData;
  const AText: string);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  FScripts := CreateScriptObject;
  inherited Create(AOwnerData, AText);
  SaveItem := False;
end;

destructor TDeCombobox.Destroy;
begin
  FreeAndNil(FPropertys);
  FreeAndNil(FScripts);
  inherited Destroy;
end;

procedure TDeCombobox.DoPopup;
begin
  if not FEditProtect then
    inherited DoPopup;
end;

function TDeCombobox.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

function TDeCombobox.InsertText(const AText: string): Boolean;
begin
  if not FEditProtect then
    Result := inherited InsertText(AText)
  else
    Result := False;
end;

procedure TDeCombobox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not FEditProtect then
    inherited KeyDown(Key, Shift);
end;

procedure TDeCombobox.KeyPress(var Key: Char);
begin
  if not FEditProtect then
    inherited KeyPress(Key);
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
  if AFileVersion > 57 then
  begin
    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FScripts.Text := vS;
  end;
end;

{$IFDEF VER320}
procedure TDeCombobox.ParseJson(const AJsonObj: TJSONObject);
var
  i: Integer;
  vDeInfo, vItems, vPropertys: TJSONObject;
begin
  Self.Items.Clear;
  Self.Propertys.Clear;

  vDeInfo := AJsonObj.GetValue('DeInfo') as TJSONObject;
  Self.Text := vDeInfo.GetValue('Text').Value;
  vItems := vDeInfo.GetValue('Items') as TJSONObject;
  for i := 0 to vItems.Count - 1 do
    Self.Items.Add(vItems.Pairs[i].JsonValue.Value);

  vPropertys := vDeInfo.GetValue('Property') as TJSONObject;
  for i := 0 to vPropertys.Count - 1 do
    Self.Propertys.Add(vPropertys.Pairs[i].JsonString.Value + '=' + vPropertys.Pairs[i].JsonValue.Value);
end;

procedure TDeCombobox.ToJson(const AJsonObj: TJSONObject);
var
  i: Integer;
  vDeInfo, vItems, vPropertys: TJSONObject;
begin
  AJsonObj.AddPair('DeType', 'Combobox');

  vPropertys := TJSONObject.Create;
  for i := 0 to FPropertys.Count - 1 do
    vPropertys.AddPair(FPropertys.Names[i], FPropertys.ValueFromIndex[i]);

  vItems := TJSONObject.Create;
  for i := 0 to Self.Items.Count - 1 do
    vItems.AddPair(i.ToString, Self.Items[i]);

  vDeInfo := TJSONObject.Create;
  vDeInfo.AddPair('Text', Self.Text);
  vDeInfo.AddPair('Items', vItems);
  vDeInfo.AddPair('Property',vPropertys);

  AJsonObj.AddPair('DeInfo', vDeInfo);
end;
{$ENDIF}

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

procedure TDeCombobox.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
  HCSaveTextToStream(AStream, FScripts.Text);
end;

procedure TDeCombobox.SetValue(const Key, Value: string);
begin
  HCSetProperty(FPropertys, Key, Value);
end;

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
  FScripts.Assign((Source as TDeDateTimePicker).FScripts);
end;

constructor TDeDateTimePicker.Create(const AOwnerData: THCCustomData;
  const ADateTime: TDateTime);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  FScripts := CreateScriptObject;
  inherited Create(AOwnerData, ADateTime);
end;

destructor TDeDateTimePicker.Destroy;
begin
  FreeAndNil(FPropertys);
  FreeAndNil(FScripts);
  inherited Destroy;
end;

function TDeDateTimePicker.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

function TDeDateTimePicker.InsertText(const AText: string): Boolean;
begin
  if not FEditProtect then
    Result := inherited InsertText(AText)
  else
    Result := False;
end;

procedure TDeDateTimePicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not FEditProtect then
    inherited KeyDown(Key, Shift);
end;

procedure TDeDateTimePicker.KeyPress(var Key: Char);
begin
  if not FEditProtect then
    inherited KeyPress(Key);
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
  if AFileVersion > 57 then
  begin
    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FScripts.Text := vS;
  end;  
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

procedure TDeDateTimePicker.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
  HCSaveTextToStream(AStream, FScripts.Text);
end;

procedure TDeDateTimePicker.SetValue(const Key, Value: string);
begin
  HCSetProperty(FPropertys, Key, Value);
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
  FScripts.Assign((Source as TDeRadioGroup).FScripts);
end;

constructor TDeRadioGroup.Create(const AOwnerData: THCCustomData);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  FScripts := CreateScriptObject;
  inherited Create(AOwnerData);
end;

destructor TDeRadioGroup.Destroy;
begin
  FreeAndNil(FPropertys);
  FreeAndNil(FScripts);
  inherited Destroy;
end;

procedure TDeRadioGroup.DoSetItemChecked(const AIndex: Integer; const Value: Boolean);
begin
  if not FEditProtect then
    inherited DoSetItemChecked(AIndex, Value);
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
  if AFileVersion > 57 then
  begin
    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FScripts.Text := vS;
  end;
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

procedure TDeRadioGroup.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
  HCSaveTextToStream(AStream, FScripts.Text);
end;

procedure TDeRadioGroup.SetValue(const Key, Value: string);
begin
  HCSetProperty(FPropertys, Key, Value);
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
  FScripts.Assign((Source as TDeTable).FScripts);
end;

constructor TDeTable.Create(const AOwnerData: THCCustomData; const ARowCount,
  AColCount, AWidth: Integer);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  FScripts := CreateScriptObject;
  inherited Create(AOwnerData, ARowCount, AColCount, AWidth);
end;

destructor TDeTable.Destroy;
begin
  FreeAndNil(FPropertys);
  FreeAndNil(FScripts);
  inherited Destroy;
end;

procedure TDeTable.CellChangeByAction(const ARow, ACol: Integer; const AProcedure: THCProcedure);
begin
  inherited CellChangeByAction(ARow, ACol, AProcedure);
end;

function TDeTable.DoCreateRow(const AStyle: THCStyle; const AColCount: Integer): THCTableRow;
begin
  Result := TDeTableRow.Create(AStyle, AColCount);
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
  if AFileVersion > 57 then
  begin
    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FScripts.Text := vS;
  end;
end;

{$IFDEF CompilerVersion > 32}
procedure TDeTable.ParseJson(const AJsonObj: TJSONObject);
var
  i, j, vR, vC: Integer;
  r, g, b: Byte;
  vS: string;
  vCells, vCellInfo, vItems, vDeInfo, vJson: TJSONObject;
  vDeItem: TDeItem;
  vArrayString: TArray<string>;
begin
  vCells := AJsonObj.GetValue('Cells') as TJSONObject;

  for i := 0 to vCells.Count - 1 do
  begin
    vS := vCells.Pairs[i].JsonString.Value;
    vR := StrToInt(System.Copy(vS, 1, Pos(',', vS) - 1));
    vC := StrToInt(System.Copy(vS, Pos(',', vS) + 1, vS.Length));

    vCellInfo := vCells.Pairs[i].JsonValue as TJSONObject;

    Self.Cells[vR, vC].RowSpan := StrToInt(vCellInfo.GetValue('RowSpan').Value);
    Self.Cells[vR, vC].ColSpan := StrToInt(vCellInfo.GetValue('ColSpan').Value);

    if (Self.Cells[vR, vC].RowSpan < 0) or (Self.Cells[vR, vC].ColSpan < 0) then
    begin
      Self.Cells[vR, vC].CellData.Free;
      Self.Cells[vR, vC].CellData := nil;
    end
    else
    begin
      if vCellInfo.GetValue('BorderSides-Left').Value = 'False' then
        Self.Cells[vR, vC].BorderSides := Self.Cells[vR, vC].BorderSides - [cbsLeft];
      if vCellInfo.GetValue('BorderSides-Top').Value = 'False' then
        Self.Cells[vR, vC].BorderSides := Self.Cells[vR, vC].BorderSides - [cbsTop];
      if vCellInfo.GetValue('BorderSides-Right').Value = 'False' then
        Self.Cells[vR, vC].BorderSides := Self.Cells[vR, vC].BorderSides - [cbsRight];
      if vCellInfo.GetValue('BorderSides-Bottom').Value = 'False' then
        Self.Cells[vR, vC].BorderSides := Self.Cells[vR, vC].BorderSides - [cbsBottom];

      vS := vCellInfo.GetValue('BackgroundColor').Value;

      vArrayString := vS.Split([',']);
      r := StrToInt(vArrayString[0]);
      g := StrToInt(vArrayString[1]);
      b := StrToInt(vArrayString[2]);
      Self.Cells[vR, vC].BackgroundColor := RGB(r, g, b);

      vItems := vCellInfo.GetValue('Items') as TJSONObject;
      for j := 0 to vItems.Count - 1 do
      begin
        vJson := vItems.Pairs[j].JsonValue as TJSONObject;
        vS := vJson.GetValue('DeType').Value;
        if vS = 'DeItem' then
        begin
          vDeInfo := vJson.GetValue('DeInfo') as TJSONObject;
          vS := vDeInfo.GetValue('Text').Value;
          if vS <> '' then
          begin
            vDeItem := TDeItem.Create;  // Text
            vDeItem.ParseJson(vJson);

            Self.Cells[vR, vC].CellData.InsertItem(vDeItem);
          end;
        end
        else
        if vS = 'DeText' then
        begin
          vDeInfo := vJson.GetValue('DeInfo') as TJSONObject;
          vS := vDeInfo.GetValue('Text').Value;
          if vS <> '' then
            Self.Cells[vR, vC].CellData.InsertText(vS);
        end;
      end;

      Self.Cells[vR, vC].CellData.ReadOnly := vCellInfo.GetValue('ReadOnly').Value = 'True';
    end;
  end;
end;

procedure TDeTable.ToJson(const AJsonObj: TJSONObject);

  procedure TColor2RGB(const Color: LongInt; var R, G, B: Byte);
  begin
    R := Color and $FF;
    G := (Color shr 8) and $FF;
    B := (Color shr 16) and $FF;
  end;

var
  vDeInfo, vCells, vCellInfo, vCellItems, vItemInfo: TJSONObject;
  i, vR, vC: Integer;
  r, g, b: Byte;
  vTableCell: THCTableCell;
begin
  AJsonObj.AddPair('DeType', 'Table');

  vDeInfo := TJSONObject.Create;
  vDeInfo.AddPair('RowCount', Self.RowCount.ToString);
  vDeInfo.AddPair('ColCount', Self.ColCount.ToString);

  vCells := TJSONObject.Create;
  for vR := 0 to Self.RowCount - 1 do
  begin
    for vC := 0 to Self.ColCount - 1 do
    begin
      vTableCell := Self.Cells[vR, vC];

      vCellInfo := TJSONObject.Create;
      vCellInfo.AddPair('RowSpan', vTableCell.RowSpan.ToString);
      vCellInfo.AddPair('ColSpan', vTableCell.ColSpan.ToString);

      if (vTableCell.RowSpan >= 0) and (vTableCell.ColSpan >= 0) then
      begin
        if vTableCell.CellData.ReadOnly then
          vCellInfo.AddPair('ReadOnly', 'True')
        else
          vCellInfo.AddPair('ReadOnly', 'False');

        if cbsLeft in vTableCell.BorderSides then
          vCellInfo.AddPair('BorderSides-Left', 'True')
        else
          vCellInfo.AddPair('BorderSides-Left', 'False');
        if cbsTop in vTableCell.BorderSides then
          vCellInfo.AddPair('BorderSides-Top', 'True')
        else
          vCellInfo.AddPair('BorderSides-Top', 'False');
        if cbsRight in vTableCell.BorderSides then
          vCellInfo.AddPair('BorderSides-Right', 'True')
        else
          vCellInfo.AddPair('BorderSides-Right', 'False');
        if cbsBottom in vTableCell.BorderSides then
          vCellInfo.AddPair('BorderSides-Bottom', 'True')
        else
          vCellInfo.AddPair('BorderSides-Bottom', 'False');

        TColor2RGB(ColorToRGB(vTableCell.BackgroundColor), r, g, b);
        vCellInfo.AddPair('BackgroundColor', r.ToString + ',' + g.ToString + ',' + b.ToString);

        vCellItems := TJSONObject.Create;
        for i := 0 to vTableCell.CellData.Items.Count - 1 do
        begin
          if vTableCell.CellData.Items[i] is TDeItem then
          begin
            vItemInfo := TJSONObject.Create;
            (vTableCell.CellData.Items[i] as TDeItem).ToJson(vItemInfo);
            vCellItems.AddPair(i.ToString, vItemInfo);
          end;
        end;

        vCellInfo.AddPair('Items', vCellItems);
      end;

      vCells.AddPair(vR.ToString + ',' + vC.ToString, vCellInfo);
    end;
  end;

  vDeInfo.AddPair('Cells', vCells);
  AJsonObj.AddPair('DeInfo', vDeInfo);
end;
{$ENDIF}

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

procedure TDeTable.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
  HCSaveTextToStream(AStream, FScripts.Text);
end;

procedure TDeTable.SetValue(const Key, Value: string);
begin
  HCSetProperty(FPropertys, Key, Value);
end;

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
  FScripts.Assign((Source as TDeCheckBox).FScripts);
end;

constructor TDeCheckBox.Create(const AOwnerData: THCCustomData;
  const AText: string; const AChecked: Boolean);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  FScripts := CreateScriptObject;
  inherited Create(AOwnerData, AText, AChecked);
end;

destructor TDeCheckBox.Destroy;
begin
  FreeAndNil(FPropertys);
  FreeAndNil(FScripts);
  inherited Destroy;
end;

procedure TDeCheckBox.DoSetChecked(const Value: Boolean);
begin
  if not FEditProtect then
    inherited DoSetChecked(Value);
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
  if AFileVersion > 57 then
  begin
    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FScripts.Text := vS;
  end;
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

procedure TDeCheckBox.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
  HCSaveTextToStream(AStream, FScripts.Text);
end;

procedure TDeCheckBox.SetValue(const Key, Value: string);
begin
  HCSetProperty(FPropertys, Key, Value);
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

procedure TDeFloatBarCodeItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

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
  HCSetProperty(FPropertys, Key, Value);
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
  FScripts.Assign((Source as TDeImageItem).FScripts);
end;

constructor TDeImageItem.Create(const AOwnerData: THCCustomData);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  FScripts := CreateScriptObject;
  inherited Create(AOwnerData);
end;

destructor TDeImageItem.Destroy;
begin
  FreeAndNil(FPropertys);
  FreeAndNil(FScripts);
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
  if AFileVersion > 57 then
  begin
    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FScripts.Text := vS;
  end;
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

procedure TDeImageItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
  HCSaveTextToStream(AStream, FScripts.Text);
end;

procedure TDeImageItem.SetValue(const Key, Value: string);
begin
  HCSetProperty(FPropertys, Key, Value);
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

{ TDeButton }

procedure TDeButton.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FEditProtect := (Source as TDeButton).EditProtect;
  FDeleteAllow := (Source as TDeButton).DeleteAllow;
  FPropertys.Assign((Source as TDeButton).Propertys);
  FScripts.Assign((Source as TDeButton).FScripts);
end;

constructor TDeButton.Create(const AOwnerData: THCCustomData; const AText: string);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  FScripts := CreateScriptObject;
  inherited Create(AOwnerData, AText);
end;

destructor TDeButton.Destroy;
begin
  FreeAndNil(FPropertys);
  FreeAndNil(FScripts);
  inherited Destroy;
end;

function TDeButton.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeButton.LoadFromStream(const AStream: TStream;
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
  if AFileVersion > 57 then
  begin
    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FScripts.Text := vS;
  end;
end;

procedure TDeButton.ParseXml(const ANode: IHCXMLNode);
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

procedure TDeButton.SaveToStreamRange(const AStream: TStream; const AStart,
  AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
  HCSaveTextToStream(AStream, FScripts.Text);
end;

procedure TDeButton.SetValue(const Key, Value: string);
begin
  HCSetProperty(FPropertys, Key, Value);
end;

procedure TDeButton.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';

  if FDeleteAllow then
    ANode.Attributes['deleteallow'] := '1';

  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeTableRow }

constructor TDeTableRow.Create(const AStyle: THCStyle; const AColCount: Integer);
begin
  FEditProtect := False;
  FPropertys := TStringList.Create;
  inherited Create(AStyle, AColCount);
end;

destructor TDeTableRow.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

function TDeTableRow.DoCreateCell(const AStyle: THCStyle): THCTableCell;
begin
  Result := TDeTableCell.Create(AStyle);
end;

function TDeTableRow.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeTableRow.LoadFromStream(const AStream: TStream; const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AFileVersion);
  if AFileVersion > 53 then
  begin
    if AFileVersion > 59 then
    begin
      AStream.ReadBuffer(vByte, SizeOf(vByte));
      FEditProtect := Odd(vByte shr 7);
    end;

    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FPropertys.Text := vS;
  end;
end;

procedure TDeTableRow.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FPropertys.Text := GetXmlRN(ANode.Attributes['property']);
  if ANode.HasAttribute('editprotect') then
    FEditProtect := ANode.Attributes['editprotect']
  else
    FEditProtect := False;
end;

procedure TDeTableRow.SaveToStream(const AStream: TStream);
var
  vByte: Byte;
begin
  inherited SaveToStream(AStream);
  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeTableRow.SetValue(const Key, Value: string);
begin
  HCSetProperty(FPropertys, Key, Value);
end;

procedure TDeTableRow.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['property'] := FPropertys.Text;
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';
end;

{ TDeTableCell }

constructor TDeTableCell.Create(const AStyle: THCStyle);
begin
  FPropertys := TStringList.Create;
  FEditProtect := False;
  inherited Create(AStyle);
end;

destructor TDeTableCell.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

function TDeTableCell.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeTableCell.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 53 then
  begin
    if AFileVersion > 56 then
    begin
      AStream.ReadBuffer(vByte, SizeOf(vByte));
      EditProtect := Odd(vByte shr 7);  // 方便应用，不直接赋值FEditProtect
    end;

    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FPropertys.Text := vS;
  end;
end;

procedure TDeTableCell.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FPropertys.Text := GetXmlRN(ANode.Attributes['property']);
  if ANode.HasAttribute('editprotect') then
    FEditProtect := ANode.Attributes['editprotect']
  else
    FEditProtect := False;
end;

procedure TDeTableCell.SaveToStream(const AStream: TStream);
var
  vByte: Byte;
begin
  inherited SaveToStream(AStream);

  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeTableCell.SetValue(const Key, Value: string);
begin
  HCSetProperty(FPropertys, Key, Value);
end;

procedure TDeTableCell.SetEditProtect(const Value: Boolean);
begin
  FEditProtect := Value;
  if Assigned(CellData) then
    CellData.ReadOnly := FEditProtect;
end;

procedure TDeTableCell.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['property'] := FPropertys.Text;
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';
end;

{ TDeBarCodeItem }

procedure TDeBarCodeItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FEditProtect := (Source as TDeBarCodeItem).EditProtect;
  FDeleteAllow := (Source as TDeBarCodeItem).DeleteAllow;
  FPropertys.Assign((Source as TDeBarCodeItem).Propertys);
end;

constructor TDeBarCodeItem.Create(const AOwnerData: THCCustomData; const AText: string);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  inherited Create(AOwnerData, AText);
end;

destructor TDeBarCodeItem.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

function TDeBarCodeItem.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeBarCodeItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 60 then
  begin
    AStream.ReadBuffer(vByte, SizeOf(vByte));
    FEditProtect := Odd(vByte shr 7);
    FDeleteAllow := Odd(vByte shr 6);

    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FPropertys.Text := vS;
  end;
end;

procedure TDeBarCodeItem.ParseXml(const ANode: IHCXMLNode);
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

procedure TDeBarCodeItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);
  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeBarCodeItem.SetValue(const Key, Value: string);
begin
  HCSetProperty(FPropertys, Key, Value);
end;

procedure TDeBarCodeItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';

  if FDeleteAllow then
    ANode.Attributes['deleteallow'] := '1';

  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeQRCodeItem }

procedure TDeQRCodeItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FEditProtect := (Source as TDeQRCodeItem).EditProtect;
  FDeleteAllow := (Source as TDeQRCodeItem).DeleteAllow;
  FPropertys.Assign((Source as TDeQRCodeItem).Propertys);
end;

constructor TDeQRCodeItem.Create(const AOwnerData: THCCustomData; const AText: string);
begin
  FDeleteAllow := True;
  FPropertys := TStringList.Create;
  inherited Create(AOwnerData, AText);
end;

destructor TDeQRCodeItem.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

function TDeQRCodeItem.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TDeQRCodeItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  if AFileVersion > 60 then
  begin
    AStream.ReadBuffer(vByte, SizeOf(vByte));
    FEditProtect := Odd(vByte shr 7);
    FDeleteAllow := Odd(vByte shr 6);

    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FPropertys.Text := vS;
  end;
end;

procedure TDeQRCodeItem.ParseXml(const ANode: IHCXMLNode);
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

procedure TDeQRCodeItem.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);
  vByte := 0;
  if FEditProtect then
    vByte := vByte or (1 shl 7);

  if FDeleteAllow then
    vByte := vByte or (1 shl 6);

  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeQRCodeItem.SetValue(const Key, Value: string);
begin
  HCSetProperty(FPropertys, Key, Value);
end;

procedure TDeQRCodeItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  if FEditProtect then
    ANode.Attributes['editprotect'] := '1';

  if FDeleteAllow then
    ANode.Attributes['deleteallow'] := '1';

  ANode.Attributes['property'] := FPropertys.Text;
end;

end.
