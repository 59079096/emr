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
  Windows, Classes, Controls, Graphics, SysUtils, HCStyle, HCItem, HCTextItem,
  HCCommon;

type
  TStyleExtra = (cseNone, cseDel, cseAdd);  // 痕迹样式

  TDeProp = class(TObject)
  public
    const
      Index = '0';
      Code = '1';
      &Name = '2';
      Frmtp = '3';  // 类别 单选、多选、数值、日期时间等
      &Unit = '4';
      CMV = '5';  // 受控词汇表(值域代码)
      CMVVCode = '6';  // 受控词汇编码(值编码)
      Trace = '7';  // 痕迹信息
  end;

  TDeFrmtp = class(TObject)
  public
    const
      Radio = 'RS';  // 单选
      Multiselect = 'MS';  // 多选
      Number = 'N';
      &String = 'S';  // 字符串
      Date = 'D';  // 日期
      Time = 'T';  // 时间
      DateTime = 'DT';  // 日期时间
  end;

  /// <summary> 电子病历文本对象 </summary>
  TEmrTextItem = class sealed(THCTextItem)  // 不可继承
  private
    FMouseIn: Boolean;
    FStyleEx: TStyleExtra;
    FPropertys: TStrings;
  protected
    procedure SetText(const Value: string); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure SetActive(const Value: Boolean); override;
    procedure Assign(Source: THCCustomItem); override;
    function CanConcatItems(const AItem: THCCustomItem): Boolean; override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    function GetHint: string; override;
    //
    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);
    function GetIsDE: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    property IsDE: Boolean read GetIsDE;
    property StyleEx: TStyleExtra read FStyleEx write FStyleEx;
    property Propertys: TStrings read FPropertys;
    property Values[const Key: string]: string read GetValue write SetValue; default;
  end;

implementation

uses
  HCParaStyle;

const
  DE_CHECKCOLOR = clBtnFace;
  DE_NOCHECKCOLOR = $0080DDFF;

{ TEmrTextItem }

procedure TEmrTextItem.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  Self.FStyleEx := (Source as TEmrTextItem).StyleEx;
  Self.FPropertys.Assign((Source as TEmrTextItem).Propertys);
end;

function TEmrTextItem.CanConcatItems(const AItem: THCCustomItem): Boolean;
var
  vEmrTextItem: TEmrTextItem;
begin
  Result := inherited CanConcatItems(AItem);
  if Result then
  begin
    vEmrTextItem := AItem as TEmrTextItem;
    Result := (Self[TDeProp.Index] = vEmrTextItem[TDeProp.Index])
      and (Self.FStyleEx = vEmrTextItem.FStyleEx)
      and (Self[TDeProp.Trace] = vEmrTextItem[TDeProp.Trace]);
  end;
end;

constructor TEmrTextItem.Create;
begin
  inherited Create;
  FPropertys := TStringList.Create;
end;

destructor TEmrTextItem.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited;
end;

procedure TEmrTextItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
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
  if (not APaintInfo.Print) and IsDE then  // 是数据元
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
        InflateRect(vRect, 0, AStyle.ParaStyles[Self.ParaNo].LineSpaceHalf);

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

function TEmrTextItem.GetHint: string;
begin
  case FStyleEx of
    cseNone: Result := Self.Values[TDeProp.Name];
  else
    Result := Self.Values[TDeProp.Trace];
  end;
end;

function TEmrTextItem.GetIsDE: Boolean;
begin
  Result := FPropertys.IndexOfName(TDeProp.Index) >= 0;
end;

function TEmrTextItem.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

procedure TEmrTextItem.LoadFromStream(const AStream: TStream;
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

procedure TEmrTextItem.MouseEnter;
begin
  inherited;
  FMouseIn := True;
  //GUpdateInfo.RePaint := True;
end;

procedure TEmrTextItem.MouseLeave;
begin
  inherited;
  FMouseIn := False;
  //GUpdateInfo.RePaint := True;
end;

procedure TEmrTextItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);
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

procedure TEmrTextItem.SetActive(const Value: Boolean);
begin
  //if Active <> Value then
  //  GUpdateInfo.RePaint := True;
  if not Value then
    FMouseIn := False;
  inherited;
end;

procedure TEmrTextItem.SetText(const Value: string);
begin
  if Value <> '' then
    inherited SetText(Value)
  else
  begin
    if IsDE then  // 数据元值为空时默认使用名称
      Text := FPropertys.Values[TDeProp.Name]
    else
      inherited SetText('');
  end;
end;

procedure TEmrTextItem.SetValue(const Key, Value: string);
begin
  FPropertys.Values[Key] := Value;
end;

end.
