{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit EmrToothItem;

interface

uses
  Windows, Classes, Controls, Graphics, HCStyle, HCItem, HCRectItem, HCCustomData,
  HCCommon;

const
  EMRSTYLE_TOOTH = THCStyle.RsCustom - 1;  // -101

type
  TToothArea = (ctaNone, ctaLeftTop, ctaLeftBottom, ctaRightTop, ctaRightBottom);

  TEmrToothItem = class(THCTextRectItem)  // 公式(上、下、左、右文本，带十字线)
  private
    FLeftTopText, FLeftBottomText, FRightTopText, FRightBottomText: string;
    FLeftTopRect, FLeftBottomRect, FRightTopRect, FRightBottomRect: TRect;
    FPadding: Byte;
    FActiveArea, FMouseMoveArea: TToothArea;
    FCaretOffset: ShortInt;
    FMouseLBDowning, FOutSelectInto: Boolean;
    function GetToothArea(const X, Y: Integer): TToothArea;
  protected
    procedure FormatToDrawItem(const ARichData: THCCustomData; const AItemNo: Integer); override;
    procedure DoPaint(const AStyle: THCStyle; const ADrawRect: TRect;
      const ADataDrawTop, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    function GetOffsetAt(const X: Integer): Integer; override;
    procedure SetActive(const Value: Boolean); override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary> 正在其上时内部是否处理指定的Key和Shif </summary>
    function WantKeyDown(const Key: Word; const Shift: TShiftState): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function InsertText(const AText: string): Boolean; override;
    procedure GetCaretInfo(var ACaretInfo: TCaretInfo); override;
    procedure SaveToStream(const AStream: TStream; const AStart, AEnd: Integer); override;
    procedure LoadFromStream(const AStream: TStream; const AStyle: THCStyle;
      const AFileVersion: Word); override;
  public
    constructor Create(const AOwnerData: THCCustomData;
      const ALeftTopText, ARightTopText, ALeftBottomText, ARightBottomText: string);
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
  if Self.Active then
  begin
    ACanvas.Brush.Color := clBtnFace;
    ACanvas.FillRect(ADrawRect);
  end;

  AStyle.TextStyles[TextStyleNo].ApplyStyle(ACanvas);
  ACanvas.TextOut(ADrawRect.Left + FLeftTopRect.Left, ADrawRect.Top + FLeftTopRect.Top, FLeftTopText);
  ACanvas.TextOut(ADrawRect.Left + FLeftBottomRect.Left, ADrawRect.Top + FLeftBottomRect.Top, FLeftBottomText);
  ACanvas.TextOut(ADrawRect.Left + FRightTopRect.Left, ADrawRect.Top + FRightTopRect.Top, FRightTopText);
  ACanvas.TextOut(ADrawRect.Left + FRightBottomRect.Left, ADrawRect.Top + FRightBottomRect.Top, FRightBottomText);

  ACanvas.Pen.Color := clBlack;
  ACanvas.MoveTo(ADrawRect.Left + FPadding, ADrawRect.Top + FLeftTopRect.Bottom + FPadding);
  ACanvas.LineTo(ADrawRect.Right - FPadding, ADrawRect.Top + FLeftTopRect.Bottom + FPadding);
  ACanvas.MoveTo(ADrawRect.Left + FLeftTopRect.Right + FPadding, ADrawRect.Top + FPadding);
  ACanvas.LineTo(ADrawRect.Left + FLeftTopRect.Right + FPadding, ADrawRect.Bottom - FPadding);

  if FActiveArea <> ctaNone then
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

  if (FMouseMoveArea <> ctaNone) and (FMouseMoveArea <> FActiveArea) then
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

procedure TEmrToothItem.FormatToDrawItem(const ARichData: THCCustomData;
  const AItemNo: Integer);
var
  vH, vW, vLeftTopW, vLeftBottomW, vRightTopW, vRightBottomW: Integer;
  vStyle: THCStyle;
begin
  vStyle := ARichData.Style;
  vStyle.TextStyles[TextStyleNo].ApplyStyle(vStyle.DefCanvas);
  vH := vStyle.DefCanvas.TextHeight('字');
  vLeftTopW := Max(vStyle.DefCanvas.TextWidth(FLeftTopText), FPadding);
  vLeftBottomW := Max(vStyle.DefCanvas.TextWidth(FLeftBottomText), FPadding);
  vRightTopW := Max(vStyle.DefCanvas.TextWidth(FRightTopText), FPadding);
  vRightBottomW := Max(vStyle.DefCanvas.TextWidth(FRightBottomText), FPadding);

  // 计算尺寸
  vW := 4 * FPadding;
  if vLeftTopW > vLeftBottomW then  // 左上比左下宽
    vW := vW + vLeftTopW
  else
    vW := vW + vLeftBottomW;

  if vRightTopW > vRightBottomW then  // 右上比右下宽
    vW := vW + vRightTopW
  else
    vW := vW + vRightBottomW;

  Width := vW;

  Height := vH * 2 + 4 * FPadding;

  // 计算各字符串位置
  if vLeftTopW > vLeftBottomW then  // 左上宽度大于左下
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

procedure TEmrToothItem.GetCaretInfo(var ACaretInfo: TCaretInfo);
begin
  if FActiveArea <> TToothArea.ctaNone then
  begin
    OwnerData.Style.TextStyles[TextStyleNo].ApplyStyle(OwnerData.Style.DefCanvas);
    case FActiveArea of
      ctaLeftTop:
        begin
          ACaretInfo.Height := FLeftTopRect.Bottom - FLeftTopRect.Top;
          ACaretInfo.X := FLeftTopRect.Left + OwnerData.Style.DefCanvas.TextWidth(Copy(FLeftTopText, 1, FCaretOffset));
          ACaretInfo.Y := FLeftTopRect.Top;
        end;

      ctaLeftBottom:
        begin
          ACaretInfo.Height := FLeftBottomRect.Bottom - FLeftBottomRect.Top;
          ACaretInfo.X := FLeftBottomRect.Left + OwnerData.Style.DefCanvas.TextWidth(Copy(FLeftBottomText, 1, FCaretOffset));
          ACaretInfo.Y := FLeftBottomRect.Top;
        end;

      ctaRightTop:
        begin
          ACaretInfo.Height := FRightTopRect.Bottom - FRightTopRect.Top;
          ACaretInfo.X := FRightTopRect.Left + OwnerData.Style.DefCanvas.TextWidth(Copy(FRightTopText, 1, FCaretOffset));
          ACaretInfo.Y := FRightTopRect.Top;
        end;

      ctaRightBottom:
        begin
          ACaretInfo.Height := FRightBottomRect.Bottom - FRightBottomRect.Top;
          ACaretInfo.X := FRightBottomRect.Left + OwnerData.Style.DefCanvas.TextWidth(Copy(FRightBottomText, 1, FCaretOffset));
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
    VK_BACK: BackspaceKeyDown;  // 回删
    VK_LEFT: LeftKeyDown;       // 左方向键
    VK_RIGHT: RightKeyDown;     // 右方向键
    VK_DELETE: DeleteKeyDown;   // 删除键
    VK_HOME: HomeKeyDown;       // Home键
    VK_END: EndKeyDown;         // End键
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

  procedure LoadPartText(var S: string);
  var
    vSize: Word;
    vBuffer: TBytes;
  begin
    AStream.ReadBuffer(vSize, SizeOf(vSize));
    if vSize > 0 then
    begin
      SetLength(vBuffer, vSize);
      AStream.Read(vBuffer[0], vSize);
      S := StringOf(vBuffer);
    end
    else
      S := '';
  end;

begin
  inherited LoadFromStream(AStream, AStyle, AFileVersion);
  LoadPartText(FLeftTopText);
  LoadPartText(FLeftBottomText);
  LoadPartText(FRightTopText);
  LoadPartText(FRightBottomText);
end;

procedure TEmrToothItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vS: string;
  vX: Integer;
  vOffset: Integer;
begin
  inherited;
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
    OwnerData.Style.TextStyles[TextStyleNo].ApplyStyle(OwnerData.Style.DefCanvas);
    vOffset := GetCharOffsetByX(OwnerData.Style.DefCanvas, vS, vX)
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

procedure TEmrToothItem.SaveToStream(const AStream: TStream; const AStart, AEnd: Integer);

  procedure SavePartText(const S: string);
  var
    vBuffer: TBytes;
    vSize: Word;
  begin
    vBuffer := BytesOf(S);
    if System.Length(vBuffer) > MAXWORD then
      raise Exception.Create(HCS_EXCEPTION_TEXTOVER);
    vSize := System.Length(vBuffer);
    AStream.WriteBuffer(vSize, SizeOf(vSize));
    if vSize > 0 then
      AStream.WriteBuffer(vBuffer[0], vSize);
  end;

begin
  inherited SaveToStream(AStream, AStart, AEnd);
  SavePartText(FLeftTopText);
  SavePartText(FLeftBottomText);
  SavePartText(FRightTopText);
  SavePartText(FRightBottomText);
end;

procedure TEmrToothItem.SetActive(const Value: Boolean);
begin
  inherited SetActive(Value);
  if not Value then
    FActiveArea := ctaNone;
end;

function TEmrToothItem.WantKeyDown(const Key: Word;
  const Shift: TShiftState): Boolean;
begin
  Result := True;
end;

end.
