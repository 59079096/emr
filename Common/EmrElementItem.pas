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
  HCTextItem, HCEditItem, HCComboboxItem, HCCommon, HCCustomData;

type
  TStyleExtra = (cseNone, cseDel, cseAdd);  // 痕迹样式

  TDeProp = class(TObject)
  public
    const
      Index = 'Index';
      Code = 'Code';
      &Name = 'Name';
      //Text = 'Text';
      Frmtp = 'Frmtp';  // 类别 单选、多选、数值、日期时间等
      &Unit = 'Unit';
      CMV = 'CMV';  // 受控词汇表(值域代码)
      CMVVCode = 'CMVVCode';  // 受控词汇编码(值编码)
      Trace = 'Trace';  // 痕迹信息
  end;

  TDeFrmtp = class(TObject)
  public
    const
      Radio = 'RS';  // 单选
      Multiselect = 'MS';  // 多选
      Number = 'N';  // 数值
      &String = 'S';  // 文本
      Date = 'D';  // 日期
      Time = 'T';  // 时间
      DateTime = 'DT';  // 日期时间
  end;

  /// <summary> 电子病历文本对象 </summary>
  TEmrTextItem = class(THCTextItem);

  /// <summary> 电子病历数据元对象 </summary>
  TDeItem = class sealed(TEmrTextItem)  // 不可继承
  private
    FMouseIn, FDeleteProtect: Boolean;
    FStyleEx: TStyleExtra;
    FPropertys: TStringList;
  protected
    procedure SetText(const Value: string); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    //
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
    function GetIsElement: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure SetActive(const Value: Boolean); override;
    procedure Assign(Source: THCCustomItem); override;
    function CanConcatItems(const AItem: THCCustomItem): Boolean; override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    function GetHint: string; override;
    function CanAccept(const AOffset: Integer): Boolean; override;

    procedure ToJson(const AJsonObj: TJSONObject);
    procedure ParseJson(const AJsonObj: TJSONObject);

    property IsElement: Boolean read GetIsElement;
    property StyleEx: TStyleExtra read FStyleEx write FStyleEx;
    property Propertys: TStringList read FPropertys;
    property DeleteProtect: Boolean read FDeleteProtect write FDeleteProtect;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

  TDeEdit = class(THCEditItem)
  private
    FPropertys: TStringList;
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); override;
    destructor Destroy; override;
    property Propertys: TStringList read FPropertys;
  end;

  TDeCombobox = class(THCComboboxItem)
  private
    FPropertys: TStringList;
  public
    constructor Create(const AOwnerData: THCCustomData; const AText: string); override;
    destructor Destroy; override;
    property Propertys: TStringList read FPropertys;
  end;

implementation

uses
  HCParaStyle;

const
  DE_CHECKCOLOR = clBtnFace;
  DE_NOCHECKCOLOR = $0080DDFF;

{ TDeItem }

procedure TDeItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  Self.FStyleEx := (Source as TDeItem).StyleEx;
  Self.FPropertys.Assign((Source as TDeItem).Propertys);
end;

function TDeItem.CanAccept(const AOffset: Integer): Boolean;
begin
  Result := not Self.IsElement;
  if not Result then
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
      and (Self.FStyleEx = vDeItem.FStyleEx)
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
  vRect: TRect;
  vAlignVert, vTextHeight: Integer;
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom, ADataScreenTop,
    ADataScreenBottom, ACanvas, APaintInfo);

  ACanvas.Refresh;
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

        vRect := ADrawRect;
        //InflateRect(vRect, 0, AStyle.ParaStyles[Self.ParaNo].LineSpaceHalf);

        ACanvas.FillRect(vRect);
      end;
    end;
  end;

  case FStyleEx of  // 痕迹
    //cseNone: ;
    cseDel:
      begin
        // 垂直居中
        vTextHeight := ACanvas.TextHeight('字');
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
    cseNone: Result := Self.Values[TDeProp.Name];
  else
    Result := Self.Values[TDeProp.Trace];
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
  vSize: Word;
  vBuffer: TBytes;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(FStyleEx, SizeOf(TStyleExtra));
  AStream.ReadBuffer(vSize, SizeOf(vSize));
  if vSize > 0 then
  begin
    SetLength(vBuffer, vSize);
    AStream.Read(vBuffer[0], vSize);
    Propertys.Text := StringOf(vBuffer);
  end;
end;

procedure TDeItem.MouseEnter;
begin
  inherited;
  FMouseIn := True;
  //GUpdateInfo.RePaint := True;
end;

procedure TDeItem.MouseLeave;
begin
  inherited;
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

procedure TDeItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);
var
  vBuffer: TBytes;
  vSize: Word;
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  AStream.WriteBuffer(FStyleEx, SizeOf(TStyleExtra));

  vBuffer := BytesOf(FPropertys.Text);
  vSize := System.Length(vBuffer);

  AStream.WriteBuffer(vSize, SizeOf(vSize));
  if vSize > 0 then
    AStream.WriteBuffer(vBuffer[0], vSize);
end;

procedure TDeItem.SetActive(const Value: Boolean);
begin
  if not Value then
    FMouseIn := False;
  inherited SetActive(Value);
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

{ TDeEdit }

constructor TDeEdit.Create(const AOwnerData: THCCustomData;
  const AText: string);
begin
  inherited Create(AOwnerData, AText);
  FPropertys := TStringList.Create;
end;

destructor TDeEdit.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

{ TDeCombobox }

constructor TDeCombobox.Create(const AOwnerData: THCCustomData;
  const AText: string);
begin
  inherited Create(AOwnerData, AText);
  FPropertys := TStringList.Create;
end;

destructor TDeCombobox.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

end.
