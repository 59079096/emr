{*******************************************************}
{                                                       }
{         ����HCView�ĵ��Ӳ�������  ���ߣ���ͨ          }
{                                                       }
{ �˴������ѧϰ����ʹ�ã�����������ҵĿ�ģ��ɴ�������  }
{ �����ʹ���߳е�������QQȺ 649023932 ����ȡ����ļ��� }
{ ������                                                }
{                                                       }
{*******************************************************}

unit HCEmrToothItem;

interface

uses
  Windows, Classes, Controls, Graphics, HCStyle, HCItem, HCRectItem, HCCustomData,
  HCCommon, HCXml, emr_Common;

type
  TToothArea = (ctaNone, ctaLeftTop, ctaLeftBottom, ctaRightTop, ctaRightBottom);

  TEmrToothItem = class(THCTextRectItem)  // ��ʽ(�ϡ��¡������ı�����ʮ����)
  private
    FLeftTopText, FLeftBottomText, FRightTopText, FRightBottomText: string;
    FLeftTopRect, FLeftBottomRect, FRightTopRect, FRightBottomRect: TRect;
    FPadding: Byte;
    FActiveArea, FMouseMoveArea: TToothArea;
    FCaretOffset: ShortInt;
    FMouseLBDowning, FOutSelectInto: Boolean;
    function GetToothArea(const X, Y: Integer): TToothArea;
  protected
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure SetActive(const Value: Boolean); override;
  public
    constructor Create(const AOwnerData: THCCustomData;
      const ALeftTopText, ARightTopText, ALeftBottomText, ARightBottomText: string);
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    function GetOffsetAt(const X: Integer): Integer; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary> ��������ʱ�ڲ��Ƿ���ָ����Key��Shif </summary>
    function WantKeyDown(const Key: Word; const Shift: TShiftState): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function InsertText(const AText: string): Boolean; override;
    procedure GetCaretInfo(var ACaretInfo: THCCaretInfo); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
    procedure ToXml(const ANode: IHCXMLNode); override;
    procedure ParseXml(const ANode: IHCXMLNode); override;
    procedure ToXmlEmr(const ANode: IHCXMLNode);
    procedure ParseXmlEmr(const ANode: IHCXMLNode);
  end;

implementation

uses
  SysUtils, System.Math;

{ TEmrToothItem }

constructor TEmrToothItem.Create(const AOwnerData: THCCustomData;
  const ALeftTopText, ARightTopText, ALeftBottomText, ARightBottomText: string);
begin
  inherited Create(AOwnerData);
  Self.StyleNo := EMRSTYLE_TOOTH;
  FPadding := 5;
  FActiveArea := TToothArea.ctaNone;
  FCaretOffset := -1;

  FLeftTopText := ALeftTopText;
  FLeftBottomText := ALeftBottomText;
  FRightTopText := ARightTopText;
  FRightBottomText := ARightBottomText;
end;

procedure TEmrToothItem.DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
  const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vFocusRect: TRect;
begin
  if Self.Active and (not APaintInfo.Print) then
  begin
    ACanvas.Brush.Color := clBtnFace;
    ACanvas.FillRect(ADrawRect);
  end;

  AStyle.TextStyles[TextStyleNo].ApplyStyle(ACanvas, APaintInfo.ScaleY / APaintInfo.Zoom);
  ACanvas.TextOut(ADrawRect.Left + FLeftTopRect.Left, ADrawRect.Top + FLeftTopRect.Top, FLeftTopText);
  ACanvas.TextOut(ADrawRect.Left + FLeftBottomRect.Left, ADrawRect.Top + FLeftBottomRect.Top, FLeftBottomText);
  ACanvas.TextOut(ADrawRect.Left + FRightTopRect.Left, ADrawRect.Top + FRightTopRect.Top, FRightTopText);
  ACanvas.TextOut(ADrawRect.Left + FRightBottomRect.Left, ADrawRect.Top + FRightBottomRect.Top, FRightBottomText);

  ACanvas.Pen.Color := clBlack;
  ACanvas.MoveTo(ADrawRect.Left + FPadding, ADrawRect.Top + FLeftTopRect.Bottom + FPadding);
  ACanvas.LineTo(ADrawRect.Right - FPadding, ADrawRect.Top + FLeftTopRect.Bottom + FPadding);
  ACanvas.MoveTo(ADrawRect.Left + FLeftTopRect.Right + FPadding, ADrawRect.Top + FPadding);
  ACanvas.LineTo(ADrawRect.Left + FLeftTopRect.Right + FPadding, ADrawRect.Bottom - FPadding);

  if not APaintInfo.Print then
  begin
    if FActiveArea <> ctaNone then  // ���������
    begin
      case FActiveArea of
        ctaLeftTop: vFocusRect := FLeftTopRect;
        ctaLeftBottom: vFocusRect := FLeftBottomRect;
        ctaRightTop: vFocusRect := FRightTopRect;
        ctaRightBottom: vFocusRect := FRightBottomRect;
      end;

      vFocusRect.Offset(ADrawRect.Location);
      vFocusRect.Inflate(2, 2);
      ACanvas.Pen.Color := clGray;
      ACanvas.Rectangle(vFocusRect);
    end;

    if (FMouseMoveArea <> ctaNone) and (FMouseMoveArea <> FActiveArea) then  // Hot����
    begin
      case FMouseMoveArea of
        ctaLeftTop: vFocusRect := FLeftTopRect;
        ctaLeftBottom: vFocusRect := FLeftBottomRect;
        ctaRightTop: vFocusRect := FRightTopRect;
        ctaRightBottom: vFocusRect := FRightBottomRect;
      end;

      vFocusRect.Offset(ADrawRect.Location);
      vFocusRect.Inflate(2, 2);
      ACanvas.Pen.Color := clMedGray;
      ACanvas.Rectangle(vFocusRect);
    end;
  end;
end;

procedure TEmrToothItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
var
  vH, vW, vLeftTopW, vLeftBottomW, vRightTopW, vRightBottomW: Integer;
  vStyle: THCStyle;
begin
  vStyle := ARichData.Style;
  vStyle.ApplyTempStyle(TextStyleNo);
  vH := vStyle.TextStyles[TextStyleNo].FontHeight;
  vLeftTopW := Max(vStyle.TempCanvas.TextWidth(FLeftTopText), FPadding);
  vLeftBottomW := Max(vStyle.TempCanvas.TextWidth(FLeftBottomText), FPadding);
  vRightTopW := Max(vStyle.TempCanvas.TextWidth(FRightTopText), FPadding);
  vRightBottomW := Max(vStyle.TempCanvas.TextWidth(FRightBottomText), FPadding);

  // ����ߴ�
  vW := 4 * FPadding;
  if vLeftTopW > vLeftBottomW then  // ���ϱ����¿�
    vW := vW + vLeftTopW
  else
    vW := vW + vLeftBottomW;

  if vRightTopW > vRightBottomW then  // ���ϱ����¿�
    vW := vW + vRightTopW
  else
    vW := vW + vRightBottomW;

  Width := vW;
  Height := vH * 2 + 4 * FPadding;

  // ������ַ���λ��
  if vLeftTopW > vLeftBottomW then  // ���Ͽ��ȴ�������
  begin
    FLeftTopRect := Bounds(FPadding, FPadding, vLeftTopW, vH);
    FLeftBottomRect := Bounds(FPadding, Height - FPadding - vH, vLeftTopW, vH);
  end
  else
  begin
    FLeftTopRect := Bounds(FPadding, FPadding, vLeftBottomW, vH);
    FLeftBottomRect := Bounds(FPadding, Height - FPadding - vH, vLeftBottomW, vH);
  end;

  if vRightTopW > vRightBottomW then
  begin
    FRightTopRect := Bounds(FLeftTopRect.Right + FPadding + FPadding, FPadding, vRightTopW, vH);
    FRightBottomRect := Bounds(FLeftTopRect.Right + FPadding + FPadding, Height - FPadding - vH, vRightTopW, vH);
  end
  else
  begin
    FRightTopRect := Bounds(FLeftTopRect.Right + FPadding + FPadding, FPadding, vRightBottomW, vH);
    FRightBottomRect := Bounds(FLeftTopRect.Right + FPadding + FPadding, Height - FPadding - vH, vRightBottomW, vH);
  end;
end;

procedure TEmrToothItem.GetCaretInfo(var ACaretInfo: THCCaretInfo);
begin
  if FActiveArea <> TToothArea.ctaNone then
  begin
    OwnerData.Style.ApplyTempStyle(TextStyleNo);
    case FActiveArea of
      ctaLeftTop:
        begin
          ACaretInfo.Height := FLeftTopRect.Bottom - FLeftTopRect.Top;
          ACaretInfo.X := FLeftTopRect.Left + OwnerData.Style.TempCanvas.TextWidth(Copy(FLeftTopText, 1, FCaretOffset));
          ACaretInfo.Y := FLeftTopRect.Top;
        end;

      ctaLeftBottom:
        begin
          ACaretInfo.Height := FLeftBottomRect.Bottom - FLeftBottomRect.Top;
          ACaretInfo.X := FLeftBottomRect.Left + OwnerData.Style.TempCanvas.TextWidth(Copy(FLeftBottomText, 1, FCaretOffset));
          ACaretInfo.Y := FLeftBottomRect.Top;
        end;

      ctaRightTop:
        begin
          ACaretInfo.Height := FRightTopRect.Bottom - FRightTopRect.Top;
          ACaretInfo.X := FRightTopRect.Left + OwnerData.Style.TempCanvas.TextWidth(Copy(FRightTopText, 1, FCaretOffset));
          ACaretInfo.Y := FRightTopRect.Top;
        end;

      ctaRightBottom:
        begin
          ACaretInfo.Height := FRightBottomRect.Bottom - FRightBottomRect.Top;
          ACaretInfo.X := FRightBottomRect.Left + OwnerData.Style.TempCanvas.TextWidth(Copy(FRightBottomText, 1, FCaretOffset));
          ACaretInfo.Y := FRightBottomRect.Top;
        end;
    end;
  end
  else
    ACaretInfo.Visible := False;
end;

function TEmrToothItem.GetToothArea(const X, Y: Integer): TToothArea;
var
  vPt: TPoint;
begin
  Result := TToothArea.ctaNone;
  vPt := Point(X, Y);
  if PtInRect(FLeftTopRect, vPt) then
    Result := TToothArea.ctaLeftTop
  else
  if PtInRect(FLeftBottomRect, vPt) then
    Result := TToothArea.ctaLeftBottom
  else
  if PtInRect(FRightTopRect, vPt) then
    Result := TToothArea.ctaRightTop
  else
  if PtInRect(FRightBottomRect, vPt) then
    Result := TToothArea.ctaRightBottom;
end;

function TEmrToothItem.InsertText(const AText: string): Boolean;
begin
  Result := False;
  if FActiveArea <> ctaNone then
  begin
    case FActiveArea of
      ctaLeftTop: System.Insert(AText, FLeftTopText, FCaretOffset + 1);
      ctaLeftBottom: System.Insert(AText, FLeftBottomText, FCaretOffset + 1);
      ctaRightTop: System.Insert(AText, FRightTopText, FCaretOffset + 1);
      ctaRightBottom: System.Insert(AText, FRightBottomText, FCaretOffset + 1);
    end;

    Inc(FCaretOffset, System.Length(AText));

    Self.SizeChanged := True;
    Result := True;
  end;
end;

function TEmrToothItem.GetOffsetAt(const X: Integer): Integer;
begin
  if FOutSelectInto then
    Result := inherited GetOffsetAt(X)
  else
  begin
    if X <= 0 then
      Result := OffsetBefor
    else
    if X >= Width then
      Result := OffsetAfter
    else
      Result := OffsetInner;
  end;
end;

procedure TEmrToothItem.KeyDown(var Key: Word; Shift: TShiftState);

  procedure BackspaceKeyDown;

    procedure BackDeleteChar(var S: string);
    begin
      if FCaretOffset > 0 then
      begin
        System.Delete(S, FCaretOffset, 1);
        Dec(FCaretOffset);
      end;
    end;

  begin
    case FActiveArea of
      ctaLeftTop: BackDeleteChar(FLeftTopText);
      ctaLeftBottom: BackDeleteChar(FLeftBottomText);
      ctaRightTop: BackDeleteChar(FRightTopText);
      ctaRightBottom: BackDeleteChar(FRightBottomText);
    end;

    Self.SizeChanged := True;
  end;

  procedure LeftKeyDown;
  begin
    if FCaretOffset > 0 then
      Dec(FCaretOffset);
  end;

  procedure RightKeyDown;
  var
    vS: string;
  begin
    case FActiveArea of
      ctaLeftTop: vS := FLeftTopText;
      ctaLeftBottom: vS := FLeftBottomText;
      ctaRightTop: vS := FRightTopText;
      ctaRightBottom: vS := FRightBottomText;
    end;

    if FCaretOffset < System.Length(vS) then
      Inc(FCaretOffset);
  end;

  procedure DeleteKeyDown;

    procedure DeleteChar(var S: string);
    begin
      if FCaretOffset < System.Length(S) then
        System.Delete(S, FCaretOffset + 1, 1);
    end;

  begin
    case FActiveArea of
      ctaLeftTop: DeleteChar(FLeftTopText);
      ctaLeftBottom: DeleteChar(FLeftBottomText);
      ctaRightTop: DeleteChar(FRightTopText);
      ctaRightBottom: DeleteChar(FRightBottomText);
    end;

    Self.SizeChanged := True;
  end;

  procedure HomeKeyDown;
  begin
    FCaretOffset := 0;
  end;

  procedure EndKeyDown;
  var
    vS: string;
  begin
    case FActiveArea of
      ctaLeftTop: vS := FLeftTopText;
      ctaLeftBottom: vS := FLeftBottomText;
      ctaRightTop: vS := FRightTopText;
      ctaRightBottom: vS := FRightBottomText;
    end;

    FCaretOffset := System.Length(vS);
  end;

begin
  case Key of
    VK_BACK: BackspaceKeyDown;  // ��ɾ
    VK_LEFT: LeftKeyDown;       // �����
    VK_RIGHT: RightKeyDown;     // �ҷ����
    VK_DELETE: DeleteKeyDown;   // ɾ����
    VK_HOME: HomeKeyDown;       // Home��
    VK_END: EndKeyDown;         // End��
  end;
end;

procedure TEmrToothItem.KeyPress(var Key: Char);
begin
  if FActiveArea <> ctaNone then
    InsertText(Key)
  else
    Key := #0;
end;

procedure TEmrToothItem.LoadFromStream(const AStream: TStream;
  const AStyle: THCStyle; const AFileVersion: Word);
begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  HCLoadTextFromStream(AStream, FLeftTopText, AFileVersion);
  HCLoadTextFromStream(AStream, FLeftBottomText, AFileVersion);
  HCLoadTextFromStream(AStream, FRightTopText, AFileVersion);
  HCLoadTextFromStream(AStream, FRightBottomText, AFileVersion);
end;

procedure TEmrToothItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vS: string;
  vX: Integer;
  vOffset: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  FMouseLBDowning := (Button = mbLeft) and (Shift = [ssLeft]);
  FOutSelectInto := False;

  if FMouseMoveArea <> FActiveArea then
  begin
    FActiveArea := FMouseMoveArea;
    OwnerData.Style.UpdateInfoReCaret;
  end;

  case FActiveArea of
    //ceaNone: ;
    ctaLeftTop:
      begin
        vS := FLeftTopText;
        vX := X - FLeftTopRect.Left;
      end;

    ctaLeftBottom:
      begin
        vS := FLeftBottomText;
        vX := X - FLeftBottomRect.Left;
      end;

    ctaRightTop:
      begin
        vS := FRightTopText;
        vX := X - FRightTopRect.Left;
      end;

    ctaRightBottom:
      begin
        vS := FRightBottomText;
        vX := X - FRightBottomRect.Left;
      end;
  end;

  if FActiveArea <> TToothArea.ctaNone then
  begin
    OwnerData.Style.ApplyTempStyle(TextStyleNo);
    vOffset := GetNorAlignCharOffsetAt(OwnerData.Style.TempCanvas, vS, vX)
  end
  else
    vOffset := -1;

  if vOffset <> FCaretOffset then
  begin
    FCaretOffset := vOffset;
    OwnerData.Style.UpdateInfoReCaret;
  end;
end;

procedure TEmrToothItem.MouseLeave;
begin
  inherited MouseLeave;
  FMouseMoveArea := ctaNone;
end;

procedure TEmrToothItem.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vArea: TToothArea;
begin
  if (not FMouseLBDowning) and (Shift = [ssLeft]) then
    FOutSelectInto := True;

  if not FOutSelectInto then
  begin
    vArea := GetToothArea(X, Y);
    if vArea <> FMouseMoveArea then
    begin
      FMouseMoveArea := vArea;
      OwnerData.Style.UpdateInfoRePaint;
    end;
  end
  else
    FMouseMoveArea := ctaNone;

  inherited MouseMove(Shift, X, Y);
end;

procedure TEmrToothItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FMouseLBDowning := False;
  FOutSelectInto := False;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TEmrToothItem.ParseXml(const ANode: IHCXMLNode);
begin
  inherited ParseXml(ANode);
  ParseXmlEmr(ANode);
end;

procedure TEmrToothItem.ParseXmlEmr(const ANode: IHCXMLNode);
begin
  if ANode.Attributes['DeCode'] = IntToStr(EMRSTYLE_TOOTH) then
  begin
    FLeftTopText := ANode.Attributes['lefttop'];
    FRightTopText := ANode.Attributes['righttop'];
    FLeftBottomText := ANode.Attributes['leftbottom'];
    FRightBottomText := ANode.Attributes['rightbottom'];
  end;
end;

procedure TEmrToothItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);
begin
  inherited SaveToStream(AStream, AStart, AEnd);
  HCSaveTextToStream(AStream, FLeftTopText);
  HCSaveTextToStream(AStream, FLeftTopText);
  HCSaveTextToStream(AStream, FLeftBottomText);
  HCSaveTextToStream(AStream, FRightTopText);
  HCSaveTextToStream(AStream, FRightBottomText);
end;

procedure TEmrToothItem.SetActive(const Value: Boolean);
begin
  inherited SetActive(Value);
  if not Value then
    FActiveArea := ctaNone;
end;

procedure TEmrToothItem.ToXml(const ANode: IHCXMLNode);
begin
  inherited ToXml(ANode);
  ToXmlEmr(ANode);
end;

procedure TEmrToothItem.ToXmlEmr(const ANode: IHCXMLNode);
begin
  ANode.Attributes['DeCode'] := IntToStr(EMRSTYLE_TOOTH);
  ANode.Attributes['lefttop'] := FLeftTopText;
  ANode.Attributes['righttop'] := FRightTopText;
  ANode.Attributes['leftbottom'] := FLeftBottomText;
  ANode.Attributes['rightbottom'] := FRightBottomText;
end;

function TEmrToothItem.WantKeyDown(const Key: Word;
  const Shift: TShiftState): Boolean;
begin
  Result := True;
end;

end.