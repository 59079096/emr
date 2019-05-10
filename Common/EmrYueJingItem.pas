{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit EmrYueJingItem;

interface

uses
  Windows, Classes, Controls, Graphics, HCCustomData, HCExpressItem, HCXml, emr_Common;

type
  TToothArea = (ctaNone, ctaLeftTop, ctaLeftBottom, ctaRightTop, ctaRightBottom);

  TEmrYueJingItem = class(THCExpressItem)  // 月经公式(上、下、左、右文本，带十字线)
  public
    constructor Create(const AOwnerData: THCCustomData;
      const ALeftText, ATopText, ARightText, ABottomText: string); override;
    procedure ToXmlEmr(const ANode: IHCXMLNode);
    procedure ParseXmlEmr(const ANode: IHCXMLNode);
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

procedure TEmrYueJingItem.ToXmlEmr(const ANode: IHCXMLNode);
begin
  ANode.Attributes['DeCode'] := IntToStr(EMRSTYLE_YUEJING);
  ANode.Attributes['toptext'] := TopText;
  ANode.Attributes['bottomtext'] := BottomText;
  ANode.Attributes['lefttext'] := LeftText;
  ANode.Attributes['righttext'] := RightText
end;

end.
