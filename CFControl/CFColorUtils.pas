unit CFColorUtils;

interface

uses
  Windows, Classes, Graphics;

type
//  TRGBColor = record
//    Red,
//    Green,
//    Blue: Byte;
//  end;

  THSBColor = record
    Hue,
    Saturnation,
    Brightness: Double;
  end;

function GetHotColor(const ABasColor: TColor): TColor;

function GetDownColor(const ABasColor: TColor): TColor;

function GetBorderColor(const ABasColor: TColor): TColor;

function GetBorderHotColor(const ABasColor: TColor): TColor;

procedure DrawColorCircle(const ACanvas: TCanvas; const ASize: Integer);

function Convert2Gray(const Color: TColor; const AOffset: SmallInt = 0): TColor;

implementation

uses
  Math;

// RGB值转Color
function RGB2TColor(const R, G, B: Byte): Integer;
begin
  Result := R + G shl 8 + B shl 16;
end;

// Color转RGB值
procedure TColor2RGB(const Color: TColor; var R, G, B: Byte);
begin
  R := Color and $FF;
  G := (Color shr 8) and $FF;
  B := (Color shr 16) and $FF;
end;

function Convert2Gray(const Color: TColor; const AOffset: SmallInt = 0): TColor;
var
  R, G, B, Gr: Byte;
begin
  TColor2RGB(Color, R, G, B);
  Gr := HiByte(R * 77 + G * 151 + B * 28);
  Gr := Trunc(B * 0.11 + G * 0.59 + R * 0.3);
  Gr := Gr + AOffset;
  Result := RGB(Gr, Gr, Gr);
end;

function RGBToHSB(R, G, B: Byte): THSBColor;
var
  minRGB, maxRGB, delta: Double;
begin
  Result.Hue := 0.0;
  minRGB := Min(Min(R, G), B);
  maxRGB := Max(Max(R, G), B);
  delta := ( maxRGB - minRGB );
  Result.Brightness := maxRGB;
  if maxRGB <> 0.0 then
    Result.Saturnation := 255.0 * Delta / maxRGB
  else
    Result.Saturnation := 0.0;
  if Result.Saturnation <> 0.0 then
  begin
    if R = maxRGB then
      Result.Hue := (G - B) / Delta
    else
    if G = maxRGB then
      Result.Hue := 2.0 + (B - R) / Delta
    else
    if B = maxRGB then
      Result.Hue := 4.0 + (R - G) / Delta
  end
  else
    Result.Hue := -1.0;
  Result.Hue := Result.Hue * 60;
  if Result.Hue < 0.0 then
    Result.Hue := Result.Hue + 360.0;
  Result.Saturnation := Result.Saturnation * 100 / 255;
  Result.Brightness := Result.Brightness * 100 / 255;
end;

function HSB2RGB(h, s, b: Double):TColor;
var
  Hi: Integer;
  f,p,q,t: Double;
  rr,gg,bb: Integer;
begin
  Hi := Floor(h / 60) mod 6;
  f := h /60 - Hi;
  p := b * (1-s/100);
  q := b * (1-f * s /100);
  t := b * (1-(1-f)*s/100);
  case Hi of
    0:
    begin
      rr := round(b*2.55);
      gg := round(t*2.55);
      bb := round(p*2.55);
    end;
    1:
    begin
      rr := round(q*2.55);
      gg := round(b*2.55);
      bb := round(p*2.55);
    end;
    2:
    begin
      rr := round(p*2.55);
      gg := round(b*2.55);
      bb := round(t*2.55);
    end;
    3:
    begin
      rr := round(p*2.55);
      gg := round(q*2.55);
      bb := round(b*2.55);
    end;
    4:
    begin
      rr := round(t*2.55);
      gg := round(p*2.55);
      bb := round(b*2.55);
    end;
    5:
    begin
      rr := round(b*2.55);
      gg := round(p*2.55);
      bb := round(q*2.55);
    end;
  end;
  if rr > 255 then
  begin
    Hi := rr - 255;
    gg := gg + Hi * 2;
    bb := bb + Hi;
    rr := 255;
  end;

  if gg > 255 then
  begin
    Hi := gg - 255;
    gg := 255;
    bb := bb + Hi * 2;
    rr := rr + Hi;
  end;

  if bb > 255 then
  begin
    Hi := bb - 255;
    gg := gg + Hi;
    bb := 255;
    rr := rr + Hi * 2;
  end;

  if rr > 255 then
    rr := 255;
  if gg > 255 then
    gg := 255;
  if bb > 255 then
    bb := 255;
  Result := rr + gg shl 8 + bb shl 16;
end;

function GetHotColor(const ABasColor: TColor): TColor;
var
  R, G, B: Byte;
  vHSB: THSBColor;
begin
  R := ABasColor and $FF;
  G := (ABasColor shr 8) and $FF;
  B := (ABasColor shr 16) and $FF;

  vHSB := RGBToHSB(R, G, B);
  vHSB.Brightness := vHSB.Brightness + 10;

  Result := HSB2RGB(vHSB.Hue, vHSB.Saturnation, vHSB.Brightness);
end;

function GetDownColor(const ABasColor: TColor): TColor;
var
  R, G, B: Byte;
  vHSB: THSBColor;
begin
  R := ABasColor and $FF;
  G := (ABasColor shr 8) and $FF;
  B := (ABasColor shr 16) and $FF;

  vHSB := RGBToHSB(R, G, B);
  vHSB.Brightness := vHSB.Brightness + 5;

  Result := HSB2RGB(vHSB.Hue, vHSB.Saturnation, vHSB.Brightness);
end;

function GetBorderColor(const ABasColor: TColor): TColor;
var
  R, G, B: Byte;
  vHSB: THSBColor;
begin
  R := ABasColor and $FF;
  G := (ABasColor shr 8) and $FF;
  B := (ABasColor shr 16) and $FF;

  vHSB := RGBToHSB(R, G, B);
  vHSB.Saturnation := vHSB.Saturnation;
  vHSB.Brightness := vHSB.Brightness - 10;

  Result := HSB2RGB(vHSB.Hue, vHSB.Saturnation, vHSB.Brightness);
end;

function GetBorderHotColor(const ABasColor: TColor): TColor;
var
  R, G, B: Byte;
  vHSB: THSBColor;
begin
  R := ABasColor and $FF;
  G := (ABasColor shr 8) and $FF;
  B := (ABasColor shr 16) and $FF;

  vHSB := RGBToHSB(R, G, B);
  vHSB.Saturnation := vHSB.Saturnation;
  vHSB.Brightness := vHSB.Brightness - 30;

  Result := HSB2RGB(vHSB.Hue, vHSB.Saturnation, vHSB.Brightness);
end;

procedure DrawColorCircle(const ACanvas: TCanvas; const ASize: Integer);
var
  i,j,x,y: Integer;
  radius: integer;
  perimeter,arc, vDegree,step: double;
  R,G,B: byte;
  vColor: TColor;
begin
  radius := round(ASize / 2);
  R := 255;
  G := 0;
  B := 0;

  ACanvas.Brush.Color := RGB(R, G, B);
  x := ASize + 1;
  y := round(radius) + 1;
  ACanvas.FillRect(Rect(ASize,round(radius), x, y));
  for j := 0 to ASize do
  begin
    perimeter := (ASize - j) * PI + 1;
    arc := perimeter / 6;
    step := ( 255 * 6 ) / perimeter ; //颜色渐变步长
    for i := 0 to round(perimeter) - 1 do
    begin
      vDegree := 360 / perimeter * i;
      x := round(cos(vDegree * PI / 180) * (ASize - j + 1) / 2) + radius;//数学公式，最后加上的是圆心点
      y := round(sin(vDegree * PI / 180) * (ASize - j + 1) / 2) + radius;

      if (vDegree > 0) and (vDegree <= 60) then
      begin
        R := 255;
        G := 0;
        B := round(step * i);
      end;
      if (vDegree > 60) and (vDegree <= 120) then
      begin
        if perimeter / 3 / 120 * (vDegree - 60) > 1.0 then
          R := 255 - round(step * (i - arc))
        else
          R := 255 - round(step * ABS(i - arc));
        G := 0;
        B := 255;
      end;
      if (vDegree > 120) and (vDegree <= 180) then
      begin
        R := 0;
        if perimeter / 3 / 120 * (vDegree - 120) > 1.0 then
          G := round(step * (i - 2 * arc))
        else
          G := round(step * ABS(i - 2 * arc));
        B := 255;
      end;
      if (vDegree > 180) and (vDegree <= 240) then
      begin
        R := 0;
        G := 255;
        if perimeter / 3 / 120 * (vDegree - 120) > 1.0 then
          B := 255 - round(step * (i - perimeter / 2))
        else
          B := 255 - round(step * ABS(i - perimeter / 2));
      end;
      if (vDegree > 240) and (vDegree <= 300) then
      begin
        if perimeter / 3 / 120 * (vDegree - 240) > 1.0 then
          R := round(step * (i - 4 * arc))
        else
          R := round(step * ABS(i - 4 * arc)) ;
        G := 255;
        B := 0;
      end;
      if (vDegree > 300) and (vDegree <= 360) then
      begin
        R := 255;
        if perimeter / 3 / 120 * (vDegree - 300) > 1.0 then
          G := 255 - round(step * (i - 5 * arc))
        else
          G := 255 - round(step * ABS(i - 5 * arc));
        B := 0;
      end;
      vColor := RGB( ROUND(R + (255 - R) / ASize * j), ROUND(G + (255 - G) / ASize * j), ROUND(B + (255 - B) / ASize * j));
      ACanvas.Brush.Color := vColor;
      //为了绘制出来的圆好看，分成四个部分进行绘制
      if (vDegree >= 0) and (vDegree <= 45) then
        ACanvas.FillRect(Rect(x, y, x - 2, y - 1));
      if (vDegree > 45) and (vDegree <= 135) then
        ACanvas.FillRect(Rect(x, y, x - 1, y - 2));
      if (vDegree > 135) and (vDegree <= 215) then
        ACanvas.FillRect(Rect(x, y, x + 2, y + 1));
      if (vDegree > 215) and (vDegree <= 315) then
        ACanvas.FillRect(Rect(x, y, x + 1, y + 2));
      if (vDegree > 315) and (vDegree <= 360) then
        ACanvas.FillRect(Rect(x, y, x - 2, y - 1));
    end;
  end;
end;

function CreateColorCircle(const size: integer): TBitmap;
var
  i,j,x,y: Integer;
  radius: integer;
  perimeter,arc,degree,step: double;
  R,G,B: byte;
  color: TColor;
begin
  radius := round(size / 2);
  RESULT := TBitmap.Create;
  R:=255;
  G:=0;
  B:=0;
  with RESULT do
  begin
    width := size;
    height:= size;
    pixelFormat := pf24bit;
    Canvas.Brush.Color := RGB(R,G,B);
    x := size + 1;
    y := round(radius) + 1;
    Canvas.FillRect(Rect(size,round(radius),x,y));
    for j := 0 to size do
    begin
      perimeter := (size - j) * PI + 1;
      arc := perimeter / 6;
      step := ( 255 * 6 ) / perimeter ; //颜色渐变步长
      for i := 0 to round(perimeter) - 1 do
      begin
        degree := 360 / perimeter * i;
        x := round(cos(degree * PI / 180) * (size - j + 1) / 2) + radius;//数学公式，最后加上的是圆心点
        y := round(sin(degree * PI / 180) * (size - j + 1) / 2) + radius;

        if (degree > 0) and (degree <= 60) then
        begin
          R := 255;
          G := 0;
          B := round(step * i);
        end;
        if (degree > 60) and (degree <= 120) then
        begin
          if perimeter / 3 / 120 * (degree - 60) > 1.0 then
            R := 255 - round(step * (i - arc))
          else
            R := 255 - round(step * ABS(i - arc));
          G := 0;
          B := 255;
        end;
        if (degree > 120) and (degree <= 180) then
        begin
          R := 0;
          if perimeter / 3 / 120 * (degree - 120) > 1.0 then
            G := round(step * (i - 2 * arc))
          else
            G := round(step * ABS(i - 2 * arc));
          B := 255;
        end;
        if (degree > 180) and (degree <= 240) then
        begin
          R := 0;
          G := 255;
          if perimeter / 3 / 120 * (degree - 120) > 1.0 then
            B := 255 - round(step * (i - perimeter / 2))
          else
            B := 255 - round(step * ABS(i - perimeter / 2));
        end;
        if (degree > 240) and (degree <= 300) then
        begin
          if perimeter / 3 / 120 * (degree - 240) > 1.0 then
            R := round(step * (i - 4 * arc))
          else
            R := round(step * ABS(i - 4 * arc)) ;
          G := 255;
          B := 0;
        end;
        if (degree > 300) and (degree <= 360) then
        begin
          R := 255;
          if perimeter / 3 / 120 * (degree - 300) > 1.0 then
            G := 255 - round(step * (i - 5 * arc))
          else
            G := 255 - round(step * ABS(i - 5 * arc));
          B := 0;
        end;
        color := RGB( ROUND(R + (255 - R)/size * j),ROUND(G + (255 - G) / size * j),ROUND(B + (255 - B) / size * j));
        Canvas.Brush.Color := color;
        //为了绘制出来的圆好看，分成四个部分进行绘制
        if (degree >= 0) and (degree <= 45) then
          Canvas.FillRect(Rect(x,y,x-2,y-1));
        if (degree > 45) and (degree <= 135) then
          Canvas.FillRect(Rect(x,y,x-1,y-2));
        if (degree > 135) and (degree <= 225) then
          Canvas.FillRect(Rect(x,y,x+2,y+1));
        if (degree > 215) and (degree <= 315) then
          Canvas.FillRect(Rect(x,y,x+1,y+2));
        if (degree > 315) and (degree <= 360) then
          Canvas.FillRect(Rect(x,y,x-2,y-1));
      end;
    end;
  end;
end;

end.
