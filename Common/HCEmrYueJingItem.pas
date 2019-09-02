{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit HCEmrYueJingItem;

interface

uses
  Windows, Classes, Controls, Graphics, HCCustomData, HCExpressItem, HCXml, emr_Common;

type
  TEmrYueJingItem = class(THCExpressItem)  // 月经公式(上、下、左、右文本，带十字线)
  private
    function GetMenarcheAge: string;
    procedure SetMenarcheAge(const Value: string);
    function GetMenstrualDuration: string;
    procedure SetMenstrualDuration(const Value: string);
    function GetMenstrualCycle: string;
    procedure SetMenstrualCycle(const Value: string);
    function GetMenstrualPause: string;
    procedure SetMenstrualPause(const Value: string);
  public
    constructor Create(const AOwnerData: THCCustomData;
      const ALeftText, ATopText, ARightText, ABottomText: string); override;
    procedure ToXmlEmr(const ANode: IHCXMLNode);
    procedure ParseXmlEmr(const ANode: IHCXMLNode);
    /// <summary> 初潮年龄 </summary>
    property MenarcheAge: string read GetMenarcheAge write SetMenarcheAge;
    /// <summary> 月经持续天数 </summary>
    property MenstrualDuration: string read GetMenstrualDuration write SetMenstrualDuration;
    /// <summary> 月经周期 </summary>
    property MenstrualCycle: string read GetMenstrualCycle write SetMenstrualCycle;
    /// <summary> 绝经年龄 </summary>
    property MenstrualPause: string read GetMenstrualPause write SetMenstrualPause;
  end;

implementation

uses
  System.SysUtils;

{ TEmrYueJingItem }

constructor TEmrYueJingItem.Create(const AOwnerData: THCCustomData;
  const ALeftText, ATopText, ARightText, ABottomText: string);
begin
  inherited Create(AOwnerData, ALeftText, ATopText, ARightText, ABottomText);
  Self.StyleNo := EMRSTYLE_YUEJING;
end;

function TEmrYueJingItem.GetMenarcheAge: string;
begin
  Result := Self.LeftText;
end;

function TEmrYueJingItem.GetMenstrualCycle: string;
begin
  Result := Self.BottomText;
end;

function TEmrYueJingItem.GetMenstrualDuration: string;
begin
  Result := Self.TopText;
end;

function TEmrYueJingItem.GetMenstrualPause: string;
begin
  Result := Self.RightText;
end;

procedure TEmrYueJingItem.ParseXmlEmr(const ANode: IHCXMLNode);
begin
  if ANode.Attributes['DeCode'] = IntToStr(EMRSTYLE_YUEJING) then
  begin
    TopText := ANode.Attributes['toptext'];
    BottomText := ANode.Attributes['bottomtext'];
    LeftText := ANode.Attributes['lefttext'];
    RightText := ANode.Attributes['righttext'];
  end;
end;

procedure TEmrYueJingItem.SetMenarcheAge(const Value: string);
begin
  Self.LeftText := Value;
end;

procedure TEmrYueJingItem.SetMenstrualCycle(const Value: string);
begin
  Self.BottomText := Value;
end;

procedure TEmrYueJingItem.SetMenstrualDuration(const Value: string);
begin
  Self.TopText := Value;
end;

procedure TEmrYueJingItem.SetMenstrualPause(const Value: string);
begin
  Self.RightText := Value;
end;

procedure TEmrYueJingItem.ToXmlEmr(const ANode: IHCXMLNode);
begin
  ANode.Attributes['DeCode'] := IntToStr(EMRSTYLE_YUEJING);
  ANode.Attributes['toptext'] := TopText;
  ANode.Attributes['bottomtext'] := BottomText;
  ANode.Attributes['lefttext'] := LeftText;
  ANode.Attributes['righttext'] := RightText
end;

end.
