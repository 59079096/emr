{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit EmrFangJiaoItem;

interface

uses
  Windows, Classes, Controls, Graphics, HCStyle, HCItem, HCRectItem, HCCustomData,
  HCCommon, HCExpressItem, HCXml, emr_Common;

type
  TEmrFangJiaoItem = class(THCExpressItem)
  protected
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(const AOwnerData: THCCustomData;
      const ALeftText, ATopText, ARightText, ABottomText: string); override;
    procedure ToXmlEmr(const ANode: IHCXMLNode);
    procedure ParseXmlEmr(const ANode: IHCXMLNode);
  end;

implementation

uses
  System.SysUtils, Math;

{ TEmrFangJiaoItem }

constructor TEmrFangJiaoItem.Create(const AOwnerData: THCCustomData;
  const ALeftText, ATopText, ARightText, ABottomText: string);
begin
  inherited Create(AOwnerData, ALeftText, ATopText, ARightText, ABottomText);
  Self.StyleNo := EMRSTYLE_FANGJIAO;
end;

procedure TEmrFangJiaoItem.DoPaint(const AStyle: THCStyle;
  const ADrawRect: TRect; const ADataDrawTop, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vFocusRect: TRect;
begin
  if Self.Active and (not APaintInfo.Print) then
  begin
    ACanvas.Brush.Color := clBtnFace;
    ACanvas.FillRect(ADrawRect);
  end;

  AStyle.TextStyles[TextStyleNo].ApplyStyle(ACanvas, APaintInfo.ScaleY / APaintInfo.Zoom);
  ACanvas.TextOut(ADrawRect.Left + LeftRect.Left, ADrawRect.Top + LeftRect.Top, LeftText);
  ACanvas.TextOut(ADrawRect.Left + TopRect.Left, ADrawRect.Top + TopRect.Top, TopText);
  ACanvas.TextOut(ADrawRect.Left + RightRect.Left, ADrawRect.Top + RightRect.Top, RightText);
  ACanvas.TextOut(ADrawRect.Left + BottomRect.Left, ADrawRect.Top + BottomRect.Top, BottomText);

  ACanvas.Pen.Color := clBlack;
  ACanvas.MoveTo(ADrawRect.Left, ADrawRect.Top);
  ACanvas.LineTo(ADrawRect.Right, ADrawRect.Bottom);
  ACanvas.MoveTo(ADrawRect.Right, ADrawRect.Top);
  ACanvas.LineTo(ADrawRect.Left, ADrawRect.Bottom);

  if not APaintInfo.Print then
  begin
    if FActiveArea <> ceaNone then
    begin
      case FActiveArea of
        ceaLeft: vFocusRect := LeftRect;
        ceaTop: vFocusRect := TopRect;
        ceaRight: vFocusRect := RightRect;
        ceaBottom: vFocusRect := BottomRect;
      end;

      vFocusRect.Offset(ADrawRect.Location);
      vFocusRect.Inflate(2, 2);
      ACanvas.Pen.Color := clGray;
      ACanvas.Rectangle(vFocusRect);
    end;

    if (FMouseMoveArea <> ceaNone) and (FMouseMoveArea <> FActiveArea) then
    begin
      case FMouseMoveArea of
        ceaLeft: vFocusRect := LeftRect;
        ceaTop: vFocusRect := TopRect;
        ceaRight: vFocusRect := RightRect;
        ceaBottom: vFocusRect := BottomRect;
      end;

      vFocusRect.Offset(ADrawRect.Location);
      vFocusRect.Inflate(2, 2);
      ACanvas.Pen.Color := clMedGray;
      ACanvas.Rectangle(vFocusRect);
    end;
  end;
end;

procedure TEmrFangJiaoItem.ParseXmlEmr(const ANode: IHCXMLNode);
begin
  if ANode.Attributes['DeCode'] = IntToStr(EMRSTYLE_FANGJIAO) then
  begin
    TopText := ANode.Attributes['toptext'];
    BottomText := ANode.Attributes['bottomtext'];
    LeftText := ANode.Attributes['lefttext'];
    RightText := ANode.Attributes['righttext'];
  end;
end;

procedure TEmrFangJiaoItem.ToXmlEmr(const ANode: IHCXMLNode);
begin
  ANode.Attributes['DeCode'] := IntToStr(EMRSTYLE_FANGJIAO);
  ANode.Attributes['toptext'] := TopText;
  ANode.Attributes['bottomtext'] := BottomText;
  ANode.Attributes['lefttext'] := LeftText;
  ANode.Attributes['righttext'] := RightText
end;

end.
