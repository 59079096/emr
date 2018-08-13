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
  HCCommon, HCExpressItem;

const
  EMRSTYLE_FANGJIAO = THCStyle.RsCustom - 2;  // -102

type
  TEMRFangJiaoItem = class(THCExperssItem)
  protected
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  end;

implementation

uses
  Math;

{ TEMRFangJiaoItem }

procedure TEMRFangJiaoItem.DoPaint(const AStyle: THCStyle;
  const ADrawRect: TRect; const ADataDrawTop, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
var
  vFocusRect: TRect;
begin
  if Self.Active then
  begin
    ACanvas.Brush.Color := clBtnFace;
    ACanvas.FillRect(ADrawRect);
  end;

  AStyle.TextStyles[TextStyleNo].ApplyStyle(ACanvas);
  ACanvas.TextOut(ADrawRect.Left + LeftRect.Left, ADrawRect.Top + LeftRect.Top, LeftText);
  ACanvas.TextOut(ADrawRect.Left + TopRect.Left, ADrawRect.Top + TopRect.Top, TopText);
  ACanvas.TextOut(ADrawRect.Left + RightRect.Left, ADrawRect.Top + RightRect.Top, RightText);
  ACanvas.TextOut(ADrawRect.Left + BottomRect.Left, ADrawRect.Top + BottomRect.Top, BottomText);

  ACanvas.Pen.Color := clBlack;
  ACanvas.MoveTo(ADrawRect.Left, ADrawRect.Top);
  ACanvas.LineTo(ADrawRect.Right, ADrawRect.Bottom);
  ACanvas.MoveTo(ADrawRect.Right, ADrawRect.Top);
  ACanvas.LineTo(ADrawRect.Left, ADrawRect.Bottom);

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

end.
