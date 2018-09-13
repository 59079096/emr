{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit EmrGroupItem;

interface

uses
  Windows, Classes, Graphics, SysUtils, IniFiles, HCStyle, HCCommon, HCItem,
  HCRectItem, HCCustomData, System.JSON;

type
  TDeGroup = class(THCDomainItem)
  private
    FPropertys: TStringList;
  protected
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    //
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);
  public
    constructor Create(const AOwnerData: THCCustomData); override;
    destructor Destroy; override;
    procedure Assign(Source: THCCustomItem); override;

    procedure ToJson(const AJsonObj: TJSONObject);
    procedure ParseJson(const AJsonObj: TJSONObject);

    property Propertys: TStringList read FPropertys;
    property Values[const Name: string]: string read GetValue write SetValue; default;
  end;

implementation

{ TDeGroup }

procedure TDeGroup.Assign(Source: THCCustomItem);
begin
  inherited Assign(Source);
  FPropertys.Assign((Source as TDeGroup).Propertys);
end;

constructor TDeGroup.Create(const AOwnerData: THCCustomData);
begin
  inherited Create(AOwnerData);
  FPropertys := TStringList.Create;
end;

destructor TDeGroup.Destroy;
begin
  FPropertys.Free;
  inherited Destroy;
end;

procedure TDeGroup.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
begin
  inherited DoPaint(AStyle, ADrawRect, ADataDrawTop, ADataDrawBottom,
    ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
end;

function TDeGroup.GetValue(const Name: string): string;
begin
  Result := FPropertys.Values[Name]
end;

procedure TDeGroup.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
var
  vSize: Word;
  vBuffer: TBytes;
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  AStream.ReadBuffer(vSize, SizeOf(vSize));
  if vSize > 0 then
  begin
    SetLength(vBuffer, vSize);
    AStream.Read(vBuffer[0], vSize);
    Propertys.Text := StringOf(vBuffer);
  end;
end;

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

procedure TDeGroup.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);
var
  vBuffer: TBytes;
  vSize: Word;
begin
  inherited SaveToStream(AStream, AStart, AEnd);;

  vBuffer := BytesOf(FPropertys.Text);
  vSize := System.Length(vBuffer);

  AStream.WriteBuffer(vSize, SizeOf(vSize));
  if vSize > 0 then
    AStream.WriteBuffer(vBuffer[0], vSize);
end;

procedure TDeGroup.SetValue(const Name, Value: string);
begin
  FPropertys.Values[Name] := Value;
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

end.
