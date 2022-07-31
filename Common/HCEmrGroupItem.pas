{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit HCEmrGroupItem;

interface

{$I HCEmrView.inc}

uses
  Windows, Classes, Graphics, SysUtils, IniFiles, HCStyle, HCCommon, HCItem,
  HCRectItem, HCCustomData, HCXml, HCStyleMatch
  {$IFDEF VER320}
  , System.JSON
  {$ENDIF}
  ;

type
  TGroupProp = class(TObject)
  public
    const
      /// <summary> 数据组唯一索引 </summary>
      Index = 'Index';
      /// <summary> 数据组名称 </summary>
      Name = 'Name';
      /// <summary> 数据组类型 </summary>
      SubType = 'RT';
      /// <summary> 全部属性 </summary>
      Propertys = 'Propertys';
  end;

  TSubType = class(TObject)
  public
    const
      /// <summary> 病程 </summary>
      Proc = 'P';
  end;

  TDeGroup = class(THCDomainItem)
  private
    FReadOnly, FChanged, FDeleteAllow: Boolean;
    FTextStyleNo: Integer;
    {$IFDEF PROCSERIES}
    FIsProc: Boolean;
    {$ENDIF}
    FPropertys, FScripts: TStringList;
    //
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);
    function GetIndex: string;
    {$IFDEF PROCSERIES}
    function GetIsProcBegin: Boolean;
    function GetIsProcEnd: Boolean;
    {$ENDIF}
  protected
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;
    procedure ApplySelectTextStyle(const AStyle: THCStyle; const AMatchStyle: THCStyleMatch); override;
    procedure MarkStyleUsed(const AMark: Boolean); override;
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure PaintTop(const ACanvas: TCanvas); override;
    function GetOffsetAt(const X: Integer): Integer; override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    {$IFDEF VER320}
    procedure ToJson(const AJsonObj: TJSONObject);
    procedure ParseJson(const AJsonObj: TJSONObject);
    {$ENDIF}
    procedure CheckPropertys;

    property Propertys: TStringList read FPropertys;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Changed: Boolean read FChanged write FChanged;
    property DeleteAllow: Boolean read FDeleteAllow write FDeleteAllow;
    property Index: string read GetIndex;
    {$IFDEF PROCSERIES}
    property IsProc: Boolean read FIsProc;
    property IsProcBegin: Boolean read GetIsProcBegin;
    property IsProcEnd: Boolean read GetIsProcEnd;
    {$ENDIF}
    property Values[const Name: string]: string read GetValue write SetValue; default;
  end;

  TProcInfo = class(THCDomainInfo)
  public
    Index: string;
    SectionIndex: Integer;
    constructor Create; override;
    procedure Clear; override;
    procedure Assign(const ASource: THCDomainInfo); override;
  end;

implementation

uses
  HCTextStyle;

{ TDeGroup }

procedure TDeGroup.ApplySelectTextStyle(const AStyle: THCStyle; const AMatchStyle: THCStyleMatch);
begin
  FTextStyleNo := AMatchStyle.GetMatchStyleNo(AStyle, FTextStyleNo);
end;

procedure TDeGroup.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FReadOnly := (Source as TDeGroup).ReadOnly;
  FDeleteAllow := (Source as TDeGroup).DeleteAllow;
  FPropertys.Assign((Source as TDeGroup).Propertys);
  CheckPropertys;
  FScripts.Assign((Source as TDeGroup).FScripts);
end;

procedure TDeGroup.CheckPropertys;
begin
  // 为减少不必要的调用，并没有在FPropertys.OnChange事件里做这些
  {$IFDEF PROCSERIES}
  FIsProc := FPropertys.Values[TGroupProp.SubType] = TSubType.Proc;
  {$ENDIF}
end;

constructor TDeGroup.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  FPropertys := TStringList.Create;
  FScripts := CreateScriptObject;
  FReadOnly := False;
  FChanged := False;
  FDeleteAllow := False;
  FTextStyleNo := THCStyle.Domain;
  {$IFDEF PROCSERIES}
  FIsProc := False;
  {$ENDIF}
end;

destructor TDeGroup.Destroy;
begin
  FreeAndNil(FPropertys);
  FreeAndNil(FScripts);
  inherited Destroy;
end;

procedure TDeGroup.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom,
    ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
end;

procedure TDeGroup.FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer);
var
  vItem: THCCustomItem;
  vTextStyle: THCTextStyle;
begin
  Self.Width := 0;
  if FTextStyleNo < THCStyle.Null then
  begin
    if OwnerData.Style.States.Contain(THCState.hosLoading) then
    begin
      vTextStyle := THCTextStyle.Create;
      try
        FTextStyleNo := ARichData.Style.GetStyleNo(vTextStyle, True);
      finally
        FreeAndNil(vTextStyle);
      end;
    end
    else
      FTextStyleNo := ARichData.Style.GetDefaultStyleNo;
  end;

  ARichData.Style.ApplyTempStyle(FTextStyleNo);
  Self.Height :=
    ARichData.CalculateLineHeight(ARichData.Style.TextStyles[FTextStyleNo], ARichData.Style.ParaStyles[Self.ParaNo]) - ARichData.Style.LineSpaceMin;  // RectItem格式化时会补充LineSpaceMin，这里减掉以实现和文本高度一样

  Empty := False;
  if MarkType = TMarkType.cmtBeg then
  begin
    if AItemNo < ARichData.Items.Count - 1 then
    begin
      vItem := ARichData.Items[AItemNo + 1];
      if (vItem.StyleNo = Self.StyleNo)
        and ((vItem as THCDomainItem).MarkType = TMarkType.cmtEnd)
      then
      begin
        Self.Width := 10;
        Empty := True;
      end
      else
      if vItem.ParaFirst then
        Self.Width := 10;
    end
    else
      Self.Width := 10;
  end
  else
  begin
    vItem := ARichData.Items[AItemNo - 1];
    if (vItem.StyleNo = Self.StyleNo)
      and ((vItem as THCDomainItem).MarkType = TMarkType.cmtBeg)
    then
    begin
      Self.Width := 10;
      Empty := True;
    end
    else
    if Self.ParaFirst then
      Self.Width := 10;;
  end;
end;

procedure TDeGroup.PaintTop(const ACanvas: TCanvas);
var
  vH: Integer;
begin
  vH := (FDrawRect.Height - Self.OwnerData.Style.TextStyles[FTextStyleNo].FontHeight) div 2;
  ACanvas.Pen.Width := 1;
  if Self.MarkType = cmtBeg then
  begin
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clBlue;
    ACanvas.MoveTo(FDrawRect.Left + 2, FDrawRect.Top + vH);
    ACanvas.LineTo(FDrawRect.Left, FDrawRect.Top + vH);
    ACanvas.LineTo(FDrawRect.Left, FDrawRect.Bottom - vH);
    ACanvas.LineTo(FDrawRect.Left + 2, FDrawRect.Bottom - vH);
  end
  else
  begin
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Color := clBlue;
    ACanvas.MoveTo(FDrawRect.Right - 2, FDrawRect.Top + vH);
    ACanvas.LineTo(FDrawRect.Right, FDrawRect.Top + vH);
    ACanvas.LineTo(FDrawRect.Right, FDrawRect.Bottom - vH);
    ACanvas.LineTo(FDrawRect.Right - 2, FDrawRect.Bottom - vH);
  end;
end;

{$IFDEF PROCSERIES}
function TDeGroup.GetIsProcBegin: Boolean;
begin
  if Self.MarkType = TMarkType.cmtBeg then
    Result := FIsProc
  else
    Result := False;
end;

function TDeGroup.GetIsProcEnd: Boolean;
begin
  if Self.MarkType = TMarkType.cmtEnd then
    Result := FIsProc
  else
    Result := False;
end;
{$ENDIF}

function TDeGroup.GetIndex: string;
begin
  Result := Self[TGroupProp.Index];
end;

function TDeGroup.GetOffsetAt(const X: Integer): Integer;
begin
  {$IFDEF PROCSERIES}
  if GetIsProcEnd then
    Result := OffsetBefor
  else
  if GetIsProcBegin then
    Result := OffsetAfter
  else
  {$ENDIF}
    Result := inherited GetOffsetAt(X);
end;

function TDeGroup.GetValue(const Name: string): string;
begin
  Result := FPropertys.Values[Name];
end;

procedure TDeGroup.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vS: string;
  vByte: Byte;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);

  if AFileVersion > 58 then
  begin
    AStream.ReadBuffer(vByte, SizeOf(vByte));
    FDeleteAllow := Odd(vByte shr 3);
    Self.Empty := Odd(vByte shr 4);
  end
  else
    FDeleteAllow := False;

  if AFileVersion > 52 then
  begin
    AStream.ReadBuffer(FTextStyleNo, SizeOf(FTextStyleNo));

    if not OwnerData.Style.States.Contain(THCState.hosLoading) then
    begin
      if Assigned(AStyle) then
        FTextStyleNo := OwnerData.Style.GetStyleNo(AStyle.TextStyles[FTextStyleNo], True)
      else
        FTextStyleNo := 0;
    end;
  end;

  HCLoadTextFromStream(AStream, vS, AFileVersion);
  FPropertys.Text := vS;
  CheckPropertys;
  if AFileVersion > 57 then
  begin
    HCLoadTextFromStream(AStream, vS, AFileVersion);
    FScripts.Text := vS;
  end;
end;

procedure TDeGroup.MarkStyleUsed(const AMark: Boolean);
begin
  if AMark then
    OwnerData.Style.TextStyles[FTextStyleNo].CheckSaveUsed := True
  else
    FTextStyleNo := OwnerData.Style.TextStyles[FTextStyleNo].TempNo;
end;

{$IFDEF VER320}
procedure TDeGroup.ParseJson(const AJsonObj: TJSONObject);
var
  i: Integer;
  vS: string;
  vDeInfo: TJSONObject;
begin
  Self.Propertys.Clear;

  vS := AJsonObj.GetValue('DeType').Value;
  if vS = 'DeGroup' then
  begin
    vDeInfo := AJsonObj.GetValue('DeInfo') as TJSONObject;
    vS := vDeInfo.GetValue('Index').Value;
    if vS <> '' then
    begin
      for i := 0 to vDeInfo.Count - 1 do
      begin
        vS := vDeInfo.Pairs[i].JsonString.Value;
        Self.Propertys.Add(vS + '=' + vDeInfo.Pairs[i].JsonValue.Value);
      end;
    end;
  end;
end;

procedure TDeGroup.ToJson(const AJsonObj: TJSONObject);
var
  i: Integer;
  vJsonValue: TJSONObject;
  vS: string;
begin
  AJsonObj.AddPair('DeType', 'DeGroup');

  vJsonValue := TJSONObject.Create;

  if Self.MarkType = TMarkType.cmtBeg then
    vJsonValue.AddPair('MarkType', 'cmtBeg')
  else
    vJsonValue.AddPair('MarkType', 'cmtEnd');

  for i := 0 to Self.Propertys.Count - 1 do
  begin
    vS := Self.Propertys.Names[i];
    vJsonValue.AddPair(vS, Self.Propertys.ValueFromIndex[i]);
  end;

  AJsonObj.AddPair('DeInfo', vJsonValue);
end;
{$ENDIF}

procedure TDeGroup.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  FPropertys.Text := ANode.Attributes['property'];
  CheckPropertys;
end;

procedure TDeGroup.SaveToStreamRange(const AStream: TStream; const AStart, AEnd: Integer);
var
  vByte: Byte;
begin
  inherited SaveToStreamRange(AStream, AStart, AEnd);

  vByte := 0;
  if FDeleteAllow then
    vByte := vByte or (1 shl 3);  // 为了以后和DeItem对应，所以从3开始

  //if Self.Empty then
  //  vByte := vByte or (1 shl 4);

  AStream.WriteBuffer(vByte, SizeOf(vByte));

  AStream.WriteBuffer(FTextStyleNo, SizeOf(FTextStyleNo));
  HCSaveTextToStream(AStream, FPropertys.Text);
  HCSaveTextToStream(AStream, FScripts.Text);
end;

procedure TDeGroup.SetValue(const Name, Value: string);
begin
  if Pos('=', Value) > 0 then
    raise Exception.Create('属性值中不允许有"="号');

  FPropertys.Values[Name] := Value;
end;

procedure TDeGroup.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ANode.Attributes['property'] := FPropertys.Text;
end;

{ TProcInfo }

procedure TProcInfo.Assign(const ASource: THCDomainInfo);
begin
  inherited Assign(ASource);
  Index := (ASource as TProcInfo).Index;
  SectionIndex := (ASource as TProcInfo).SectionIndex;
end;

procedure TProcInfo.Clear;
begin
  Index := '';
  SectionIndex := -1;
  inherited Clear;
end;

constructor TProcInfo.Create;
begin
  inherited Create;
  Clear;
end;

end.
