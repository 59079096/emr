{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit EmrElementItem;

interface

uses
  Windows, Classes, Controls, Graphics, SysUtils, System.JSON, HCStyle, HCItem,
  HCTextItem, HCEditItem, HCComboboxItem, HCDateTimePicker, HCRadioGroup, HCTableItem,
  HCTableCell, HCCheckBoxItem, HCFractionItem, HCCommon, HCCustomData, HCXml;

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
      &Unit = 'Unit';

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

      /// <summary> 删除保护 </summary>
      DeleteProtect = 'DelPrtc';
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

  /// <summary> 电子病历文本对象 </summary>
  TEmrTextItem = class(THCTextItem);

  /// <summary> 电子病历数据元对象 </summary>
  TDeItem = class sealed(TEmrTextItem)  // 不可继承
  private
    FMouseIn, FDeleteProtect: Boolean;
    FStyleEx: TStyleExtra;
    FPropertys: TStringList;
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
    procedure SetDeleteProtect(const Value: Boolean);
    function GetIsElement: Boolean;
  protected
    procedure SetText(const Value: string); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure SetActive(const Value: Boolean); override;
    procedure Assign(Source: THCCustomItem); override;
    function CanConcatItems(const AItem: THCCustomItem): Boolean; override;
    function GetHint: string; override;
    function CanAccept(const AOffset: Integer; const AAction: THCItemAction): Boolean; override;

    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    procedure ToJson(const AJsonObj: TJSONObject);
    procedure ParseJson(const AJsonObj: TJSONObject);

    property IsElement: Boolean read GetIsElement;
    property StyleEx: TStyleExtra read FStyleEx write FStyleEx;
    property DeleteProtect: Boolean read FDeleteProtect write SetDeleteProtect;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeTable = class(THCTableItem)
  private
    FDeleteProtect: Boolean;
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
    procedure ToJson(const AJsonObj: TJSONObject);
    procedure ParseJson(const AJsonObj: TJSONObject);

    property DeleteProtect: Boolean read FDeleteProtect write FDeleteProtect;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeCheckBox = class(THCCheckBoxItem)
  private
    FDeleteProtect: Boolean;
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
    procedure ToJson(const AJsonObj: TJSONObject);
    procedure ParseJson(const AJsonObj: TJSONObject);

    property DeleteProtect: Boolean read FDeleteProtect write FDeleteProtect;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeEdit = class(THCEditItem)
  private
    FDeleteProtect: Boolean;
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
    procedure ToJson(const AJsonObj: TJSONObject);
    procedure ParseJson(const AJsonObj: TJSONObject);

    property DeleteProtect: Boolean read FDeleteProtect write FDeleteProtect;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeCombobox = class(THCComboboxItem)
  private
    FDeleteProtect: Boolean;
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
    procedure ToJson(const AJsonObj: TJSONObject);
    procedure ParseJson(const AJsonObj: TJSONObject);

    property DeleteProtect: Boolean read FDeleteProtect write FDeleteProtect;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeDateTimePicker = class(THCDateTimePicker)
  private
    FDeleteProtect: Boolean;
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
    procedure ToJson(const AJsonObj: TJSONObject);
    procedure ParseJson(const AJsonObj: TJSONObject);

    property DeleteProtect: Boolean read FDeleteProtect write FDeleteProtect;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeRadioGroup = class(THCRadioGroup)
  private
    FDeleteProtect: Boolean;
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
    procedure ToJson(const AJsonObj: TJSONObject);
    procedure ParseJson(const AJsonObj: TJSONObject);

    property DeleteProtect: Boolean read FDeleteProtect write FDeleteProtect;
    property Propertys: TStringList read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

implementation

uses
  HCParaStyle;

const
  DE_CHECKCOLOR = clBtnFace;  // 元素填写后背景色
  DE_NOCHECKCOLOR = $0080DDFF;  // 元素未填写时背景色

{ TDeItem }

procedure TDeItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FStyleEx := (Source as TDeItem).StyleEx;
  FPropertys.Assign((Source as TDeItem).Propertys);
end;

function TDeItem.CanAccept(const AOffset: Integer; const AAction: THCItemAction): Boolean;
begin
  Result := inherited CanAccept(AOffset, AAction);

  if Result then
  begin
    if IsElement then  // 我是数据元
    begin
      case AAction of
        hiaInsertChar: Result := False;  // 数据元的值只能通过选择框完成
        hiaBackDeleteChar, hiaDeleteChar, hiaRemove:
          Result := not FDeleteProtect;  // 受保护的数据元不能删除
      end;
    end
    else
      Result := not FDeleteProtect;
  end;

  if not Result then  // 是元素，不可编辑
    Beep;
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
      and (FDeleteProtect = vDeItem.DeleteProtect)
      and (Self[TDeProp.Trace] = vDeItem[TDeProp.Trace]);
  end;
end;

constructor TDeItem.Create;
begin
  inherited Create;
  FPropertys := TStringList.Create;

  FDeleteProtect := False;
  FMouseIn := False;
end;

destructor TDeItem.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited;
end;

procedure TDeItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vTop: Integer;
  vAlignVert, vTextHeight: Integer;
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  if (not APaintInfo.Print) and IsElement then  // 是数据元
  begin
    if FMouseIn or Active then  // 鼠标移入和光标在其中
    begin
      if IsSelectPart or IsSelectComplate then
      begin

      end
      else
      begin
        if Self[TDeProp.Name] <> Self.Text then  // 已经填写过了
          ACanvas.Brush.Color := DE_CHECKCOLOR
        else  // 没填写过
          ACanvas.Brush.Color := DE_NOCHECKCOLOR;

        ACanvas.FillRect(ADrawRect);
      end;
    end;
  end;

  case FStyleEx of  // 痕迹
    //cseNone: ;
    cseDel:
      begin
        // 垂直居中
        vTextHeight := ACanvas.TextHeight('H');
        case AStyle.ParaStyles[Self.ParaNo].AlignVert of
          pavCenter: vAlignVert := DT_CENTER;
          pavTop: vAlignVert := DT_TOP;
        else
          vAlignVert := DT_BOTTOM;
        end;

        case vAlignVert of
          DT_TOP: vTop := ADrawRect.Top;
          DT_CENTER: vTop := ADrawRect.Top + (ADrawRect.Bottom - ADrawRect.Top - vTextHeight) div 2;
        else
          vTop := ADrawRect.Bottom - vTextHeight;
        end;
        // 绘制删除线
        ACanvas.Pen.Style := psSolid;
        ACanvas.Pen.Color := clRed;
        vTop := vTop + (ADrawRect.Bottom - vTop) div 2;
        ACanvas.MoveTo(ADrawRect.Left, vTop - 1);
        ACanvas.LineTo(ADrawRect.Right, vTop - 1);
        ACanvas.MoveTo(ADrawRect.Left, vTop + 2);
        ACanvas.LineTo(ADrawRect.Right, vTop + 2);
      end;

    cseAdd:
      begin
        ACanvas.Pen.Style := psSolid;
        ACanvas.Pen.Color := clBlue;
        ACanvas.MoveTo(ADrawRect.Left, ADrawRect.Bottom);
        ACanvas.LineTo(ADrawRect.Right, ADrawRect.Bottom);
      end;
  end;
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
  vIndex: Integer;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(FStyleEx, SizeOf(TStyleExtra));
  HCLoadTextFromStream(AStream, vS);
  FPropertys.Text := vS;

  vIndex := FPropertys.IndexOfName(TDeProp.DeleteProtect);
  if vIndex >= 0 then
    FDeleteProtect := FPropertys.Values[TDeProp.DeleteProtect] = '1'
  else
    FDeleteProtect := False;
end;

procedure TDeItem.MouseEnter;
begin
  inherited MouseEnter;
  FMouseIn := True;
  //GUpdateInfo.RePaint := True;
end;

procedure TDeItem.MouseLeave;
begin
  inherited MouseLeave;
  FMouseIn := False;
  //GUpdateInfo.RePaint := True;
end;

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

procedure TDeItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  AStream.WriteBuffer(FStyleEx, SizeOf(TStyleExtra));
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeItem.SetActive(const Value: Boolean);
begin
  if not Value then
    FMouseIn := False;
  inherited SetActive(Value);
end;

procedure TDeItem.SetDeleteProtect(const Value: Boolean);
var
  vIndex: Integer;
begin
  if FDeleteProtect <> Value then
  begin
    FDeleteProtect := Value;

    if FDeleteProtect then  // 不可删除
      FPropertys.Values[TDeProp.DeleteProtect] := '1'
    else  // 可删除
    begin
      vIndex := FPropertys.IndexOfName(TDeProp.DeleteProtect);
      if vIndex >= 0 then
        FPropertys.Delete(vIndex);
    end;
  end;
end;

procedure TDeItem.SetText(const Value: string);
begin
  if Value <> '' then
    inherited SetText(Value)
  else
  begin
    if IsElement and FDeleteProtect then  // 数据元值为空时默认使用名称
      Text := FPropertys.Values[TDeProp.Name]
    else
      inherited SetText('');
  end;
end;

procedure TDeItem.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
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

procedure TDeItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeEdit }

procedure TDeEdit.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FPropertys.Assign((Source as TDeEdit).Propertys);
end;

constructor TDeEdit.Create(const AOwnerData: THCCustomData;
  const AText: string);
begin
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
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, vS);
  FPropertys.Text := vS;
end;

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

procedure TDeEdit.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeEdit.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeEdit.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
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

procedure TDeEdit.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeCombobox }

procedure TDeCombobox.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FPropertys.Assign((Source as TDeCombobox).Propertys);
end;

constructor TDeCombobox.Create(const AOwnerData: THCCustomData;
  const AText: string);
begin
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
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, vS);
  FPropertys.Text := vS;
end;

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

procedure TDeCombobox.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeCombobox.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeCombobox.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
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

procedure TDeCombobox.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeDateTimePicker }

procedure TDeDateTimePicker.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FPropertys.Assign((Source as TDeDateTimePicker).Propertys);
end;

constructor TDeDateTimePicker.Create(const AOwnerData: THCCustomData;
  const ADateTime: TDateTime);
begin
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
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, vS);
  FPropertys.Text := vS;
end;

procedure TDeDateTimePicker.ParseJson(const AJsonObj: TJSONObject);
begin

end;

procedure TDeDateTimePicker.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeDateTimePicker.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeDateTimePicker.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
end;

procedure TDeDateTimePicker.ToJson(const AJsonObj: TJSONObject);
begin

end;

procedure TDeDateTimePicker.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeRadioGroup }

procedure TDeRadioGroup.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FPropertys.Assign((Source as TDeRadioGroup).Propertys);
end;

constructor TDeRadioGroup.Create(const AOwnerData: THCCustomData);
begin
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
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, vS);
  FPropertys.Text := vS;
end;

procedure TDeRadioGroup.ParseJson(const AJsonObj: TJSONObject);
begin

end;

procedure TDeRadioGroup.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeRadioGroup.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeRadioGroup.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
end;

procedure TDeRadioGroup.ToJson(const AJsonObj: TJSONObject);
begin

end;

procedure TDeRadioGroup.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeTable }

procedure TDeTable.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FPropertys.Assign((Source as TDeTable).Propertys);
end;

constructor TDeTable.Create(const AOwnerData: THCCustomData; const ARowCount,
  AColCount, AWidth: Integer);
begin
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
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, vS);
  FPropertys.Text := vS;
end;

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

procedure TDeTable.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeTable.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeTable.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
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

procedure TDeTable.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TDeCheckBox }

procedure TDeCheckBox.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FPropertys.Assign((Source as TDeCheckBox).Propertys);
end;

constructor TDeCheckBox.Create(const AOwnerData: THCCustomData;
  const AText: string; const AChecked: Boolean);
begin
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
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, vS);
  FPropertys.Text := vS;
end;

procedure TDeCheckBox.ParseJson(const AJsonObj: TJSONObject);
begin

end;

procedure TDeCheckBox.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FPropertys.Text := ANode.Attributes['property'];
end;

procedure TDeCheckBox.SaveToStream(const AStream: TStream; const AStart,
  AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FPropertys.Text);
end;

procedure TDeCheckBox.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
end;

procedure TDeCheckBox.ToJson(const AJsonObj: TJSONObject);
begin

end;

procedure TDeCheckBox.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['property'] := FPropertys.Text;
end;

end.
