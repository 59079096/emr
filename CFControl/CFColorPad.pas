unit CFColorPad;

interface

uses
  Windows, Classes, Controls, Graphics, CFControl, CFButton, CFEdit;

type
  TCFColorPad = class(TCFCustomControl)
  strict private
    FPad: TBitmap;
    FSelect, FNearColor1, FNearColor2, FNearColor3, FNearColor4: TColor;
    FOnChange: TNotifyEvent;
    procedure SetSelect(const Value: TColor); virtual;
  protected
    procedure DoChange;
    procedure PaintPad(const ACanvas: TCanvas); virtual;
    procedure CreateHandle; override;
    procedure DrawControl(ACanvas: TCanvas); override;
    procedure UpdateView; virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    property Pad: TBitmap read FPad;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    //procedure DrawTo(const ACanvas: TCanvas); override;
    property Select: TColor read FSelect write SetSelect;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCFRichColorPad = class(TCFColorPad)
  private
    FBtnOk: TCFButton;
    FEdtR, FEdtG, FEdtB: TCFEdit;
    FCircleWidth: Byte;  // 色环大小
    procedure PaintColorCircle(const ABitmap: TBitmap);
  protected
    procedure PaintPad(const ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TCFColorPad }

const
  PadWidth = 20;
  PadSpliter = 5;

constructor TCFColorPad.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := GBackColor;
  FSelect := clRed;// Color;

  FPad := TBitmap.Create;
  FNearColor1 := clBtnFace;
  FNearColor2 := clGrayText;
  FNearColor3 := clHighlight;
  FNearColor4 := clActiveBorder;

  Width := PadSpliter * 5 + PadWidth * 4;
  Height := Width + PadSpliter + PadWidth;
end;

procedure TCFColorPad.CreateHandle;
begin
  inherited;
  UpdateView;
end;

destructor TCFColorPad.Destroy;
begin
  FPad.Free;
  inherited Destroy;
end;

procedure TCFColorPad.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCFColorPad.DrawControl(ACanvas: TCanvas);
begin
  ACanvas.Draw(0, 0, FPad);
end;

//procedure TCFColorPad.DrawTo(const ACanvas: TCanvas);
//begin
//  PaintPad(ACanvas);
//end;

procedure TCFColorPad.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  Select := FPad.Canvas.Pixels[X, Y];
  DoChange;
end;

procedure TCFColorPad.PaintPad(const ACanvas: TCanvas);
var
  vLeft, vTop: Integer;
begin
  ACanvas.Brush.Color := Color;
  ACanvas.FillRect(Rect(0, 0, Width, Height));

  if BorderVisible then
  begin
    ACanvas.Pen.Color := GBorderColor;
    with ACanvas do
    begin
      MoveTo(0, 0);
      LineTo(Width - 1, 0);
      LineTo(Width - 1, Height - 1);
      LineTo(0, Height - 1);
      LineTo(0, 0);
    end;
  end;

  // 第1行，4个近期使用
  vLeft := PadSpliter;
  vTop := PadSpliter;

  ACanvas.Brush.Color := FNearColor1;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := FNearColor2;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := FNearColor3;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := FNearColor4;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  // 第2行 4个
  vLeft := PadSpliter;
  vTop := vTop + PadWidth + PadSpliter;

  ACanvas.Brush.Color := clRed;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := clGreen;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := clBlue;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := clYellow;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  // 第3行 4个
  vLeft := PadSpliter;
  vTop := vTop + PadWidth + PadSpliter;

  ACanvas.Brush.Color := clBlack;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := clOlive;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := clLime;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  // 第4行 4个
  vLeft := PadSpliter;
  vTop := vTop + PadWidth + PadSpliter;

  ACanvas.Brush.Color := clNavy;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := clAqua;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := clWebOrange;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := clInfoBk;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  // 第5行 4个
  vLeft := PadSpliter;
  vTop := vTop + PadWidth + PadSpliter;

  ACanvas.Brush.Color := $00400080;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := $00804000;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := clTeal;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));

  vLeft := vLeft + PadWidth + PadSpliter;
  ACanvas.Brush.Color := $008000FF;
  ACanvas.FillRect(Bounds(vLeft, vTop, PadWidth, PadWidth));
end;

procedure TCFColorPad.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  UpdateView;
end;

procedure TCFColorPad.SetSelect(const Value: TColor);
begin
  if FSelect <> Value then
  begin
    FSelect := Value;
    UpdateView;
  end;
end;

procedure TCFColorPad.UpdateView;
begin
  if not HandleAllocated then Exit;
  if not Assigned(FPad) then Exit;

  FPad.SetSize(Width, Height);
  PaintPad(FPad.Canvas);
end;

{ TCFRichColorPad }

constructor TCFRichColorPad.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCircleWidth := Height;
  Width := Width + FCircleWidth - PadSpliter;
  Height := Height + PadSpliter + PadWidth + 20 + PadSpliter;

  FEdtR := TCFEdit.Create(Self);
  FEdtR.Width := 30;
  FEdtR.Parent := Self;
  FEdtR.Left := PadSpliter;
  FEdtR.Top := Height - PadSpliter - FEdtR.Height;

  FEdtG := TCFEdit.Create(Self);
  FEdtG.Width := 30;
  FEdtG.Parent := Self;
  FEdtG.Left := FEdtR.Left + FEdtR.Width + PadSpliter;
  FEdtG.Top := FEdtR.Top;

  FEdtB := TCFEdit.Create(Self);
  FEdtB.Width := 30;
  FEdtB.Parent := Self;
  FEdtB.Left := FEdtG.Left + FEdtG.Width + PadSpliter;
  FEdtB.Top := FEdtR.Top;

  FBtnOk := TCFButton.Create(Self);
  FBtnOk.Height := FEdtB.Height;
  FBtnOk.Width := 50;
  FBtnOk.Text := '确定';
  FBtnOk.Parent := Self;
  FBtnOk.Left := FEdtB.Left + FEdtB.Width + PadSpliter;
  FBtnOk.Top := FEdtB.Top;
end;

procedure TCFRichColorPad.PaintColorCircle(const ABitmap: TBitmap);
var
  i, j, x, y: Integer;
  vRadius: Integer;
  vPerimeter, vArc, vDegree, vStep: Double;
  vR, vG, vB: byte;
  vColor: TColor;
begin
  vRadius := Round(ABitmap.Width / 2);
  vR := 255;
  vG := 0;
  vB := 0;
  with ABitmap do
  begin
    pixelFormat := pf24bit;
    Canvas.Brush.Color := RGB(vR,vG,vB);
    x := ABitmap.Width + 1;
    y := Round(vRadius) + 1;
    Canvas.FillRect(Rect(ABitmap.Width, Round(vRadius),x,y));
    for j := 0 to ABitmap.Width do
    begin
      vPerimeter := (ABitmap.Width - j) * PI + 1;
      vArc := vPerimeter / 6;
      vStep := ( 255 * 6 ) / vPerimeter ;  // 颜色渐变步长
      for i := 0 to Round(vPerimeter) - 1 do
      begin
        vDegree := 360 / vPerimeter * i;
        x := Round(cos(vDegree * PI / 180) * (ABitmap.Width - j + 1) / 2) + vRadius;  // 数学公式，最后加上的是圆心点
        y := Round(sin(vDegree * PI / 180) * (ABitmap.Width - j + 1) / 2) + vRadius;

        if (vDegree > 0) and (vDegree <= 60) then
        begin
          vR := 255;
          vG := 0;
          vB := Round(vStep * i);
        end;

        if (vDegree > 60) and (vDegree <= 120) then
        begin
          if vPerimeter / 3 / 120 * (vDegree - 60) > 1.0 then
            vR := 255 - Round(vStep * (i - vArc))
          else
            vR := 255 - Round(vStep * Abs(i - vArc));

          vG := 0;
          vB := 255;
        end;

        if (vDegree > 120) and (vDegree <= 180) then
        begin
          vR := 0;

          if vPerimeter / 3 / 120 * (vDegree - 120) > 1.0 then
            vG := Round(vStep * (i - 2 * vArc))
          else
            vG := Round(vStep * Abs(i - 2 * vArc));

          vB := 255;
        end;
        if (vDegree > 180) and (vDegree <= 240) then
        begin
          vR := 0;
          vG := 255;
          if vPerimeter / 3 / 120 * (vDegree - 120) > 1.0 then
            vB := 255 - Round(vStep * (i - vPerimeter / 2))
          else
            vB := 255 - Round(vStep * Abs(i - vPerimeter / 2));
        end;
        if (vDegree > 240) and (vDegree <= 300) then
        begin
          if vPerimeter / 3 / 120 * (vDegree - 240) > 1.0 then
            vR := Round(vStep * (i - 4 * vArc))
          else
            vR := Round(vStep * Abs(i - 4 * vArc));

          vG := 255;
          vB := 0;
        end;

        if (vDegree > 300) and (vDegree <= 360) then
        begin
          vR := 255;
          if vPerimeter / 3 / 120 * (vDegree - 300) > 1.0 then
            vG := 255 - Round(vStep * (i - 5 * vArc))
          else
            vG := 255 - Round(vStep * Abs(i - 5 * vArc));

          vB := 0;
        end;

        vColor := RGB(Round(vR + (255 - vR) / ABitmap.Width * j),
          Round(vG + (255 - vG) / ABitmap.Width * j),
          Round(vB + (255 - vB) / ABitmap.Width * j));

        Canvas.Brush.Color := vColor;
        //为了绘制出来的圆好看，分成四个部分进行绘制
        if (vDegree >= 0) and (vDegree <= 45) then
          Canvas.FillRect(Rect(x, y, x - 2, y - 1));

        if (vDegree > 45) and (vDegree <= 135) then
          Canvas.FillRect(Rect(x, y, x - 1, y - 2));

        if (vDegree > 135) and (vDegree <= 225) then
          Canvas.FillRect(Rect(x, y, x + 2, y + 1));

        if (vDegree > 215) and (vDegree <= 315) then
          Canvas.FillRect(Rect(x, y, x + 1, y + 2));

        if (vDegree > 315) and (vDegree <= 360) then
          Canvas.FillRect(Rect(x, y, x - 2, y - 1));
      end;
    end;
  end;
end;

procedure TCFRichColorPad.PaintPad(const ACanvas: TCanvas);
var
  vBitmap: TBitmap;
  i, vW: Integer;
  vR: Byte;
begin
  inherited PaintPad(ACanvas);
  // 色环
  vBitmap := TBitmap.Create;
  try
    vBitmap.SetSize(FCircleWidth - PadSpliter - PadSpliter, FCircleWidth - PadSpliter - PadSpliter);
    PaintColorCircle(vBitmap);
    ACanvas.Draw(Width - FCircleWidth + PadSpliter, PadSpliter, vBitmap);
  finally
    vBitmap.Free;
  end;

  // 灰度
  vW := Width - PadSpliter - PadSpliter;
  for i := PadSpliter to Width - PadSpliter do
  begin
    vR := 255 - Round((i - PadSpliter) / vW * 255);
    ACanvas.Pen.Color := RGB(vR, vR, vR);
    ACanvas.MoveTo(i, Height - PadSpliter - FEdtR.Height - PadSpliter - PadWidth);
    ACanvas.LineTo(i, Height - PadSpliter - FEdtR.Height - PadSpliter);
  end;

  FEdtR.PaintTo(ACanvas, FEdtR.Left, FEdtR.Top);
  FEdtG.PaintTo(ACanvas, FEdtG.Left, FEdtG.Top);
  FEdtB.PaintTo(ACanvas, FEdtB.Left, FEdtB.Top);
end;

end.
