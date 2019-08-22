unit CFToolButton;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Controls, Winapi.Messages, Vcl.Graphics, CFControl;

type
  TCFToolButton = class(TGraphicControl)
  private
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    /// <summary> 鼠标移入 </summary>
    procedure CMMouseEnter(var Msg: TMessage ); message CM_MOUSEENTER;

    /// <summary> 鼠标移出 </summary>
    procedure CMMouseLeave(var Msg: TMessage ); message CM_MOUSELEAVE;
  protected
    FMouseIn, FMouseDown: Boolean;
    /// <summary> 绘制 </summary>
    /// <param name="ACanvas">呈现画布</param>
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Caption;
    property Align;
    property OnClick;
  end;

  TCFMenuButton = class(TCFToolButton)
  protected
    /// <summary> 绘制 </summary>
    /// <param name="ACanvas">呈现画布</param>
    procedure Paint; override;
    /// <summary> 单击事件 </summary>
    procedure Click; override;
  published
    property PopupMenu;
  end;

implementation

uses
  CFColorUtils;

{ TCFToolButton }

procedure TCFToolButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseIn := True;
  Self.Repaint;
end;

procedure TCFToolButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseIn := False;
  Self.Repaint;
end;

constructor TCFToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMouseIn := False;
  FMouseDown := False;
  Width := 75;
  Height := 25;
end;

procedure TCFToolButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FMouseDown := True;
  Self.Repaint;
end;

procedure TCFToolButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FMouseDown := False;
  Self.Repaint;
end;

procedure TCFToolButton.Paint;
var
  vRect: TRect;
  vText: string;
  vBackColor: TColor;
begin
  vRect := Rect(0, 0, Width, Height);
  if FMouseIn then  // 鼠标在控件内
  begin
    vBackColor := GAreaBackColor;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := GetBorderColor(vBackColor);

    if FMouseDown then  // 鼠标按下
      Canvas.Brush.Color := GetDownColor(vBackColor)
    else
      Canvas.Brush.Color := GetHotColor(vBackColor);

    Canvas.FillRect(vRect);
  end
  else  // 普通状态
    Canvas.Brush.Style := bsClear;

  vText := Caption;
  Canvas.TextRect(vRect, vText, [tfSingleLine, tfCenter,tfVerticalCenter]);
end;

procedure TCFToolButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

{ TCFMenuButton }

procedure TCFMenuButton.Click;
var
  vPt: TPoint;
begin
  if Assigned(PopupMenu) then
  begin
    vPt := Self.ClientToScreen(Point(0, Self.Height));
    PopupMenu.Popup(vPt.X, vPt.Y);
  end;
end;

procedure TCFMenuButton.Paint;
var
  vRect: TRect;
  vText: string;
  vBackColor: TColor;
begin
  vRect := Rect(0, 0, Width, Height);
  if FMouseIn then  // 鼠标在控件内
  begin
    vBackColor := GAreaBackColor;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := GetBorderColor(vBackColor);

    if FMouseDown then  // 鼠标按下
      Canvas.Brush.Color := GetDownColor(vBackColor)
    else
      Canvas.Brush.Color := GetHotColor(vBackColor);

    Canvas.FillRect(vRect);
  end
  else  // 普通状态
    Canvas.Brush.Style := bsClear;

  vRect.Right := vRect.Right - 16;
  vText := Caption;
  Canvas.TextRect(vRect, vText, [tfSingleLine, tfCenter,tfVerticalCenter]);
  // 下拉按钮
  if False then
  begin
    Canvas.Pen.Color := $00848484;
    Canvas.MoveTo(Width - 16, 2);
    Canvas.LineTo(Width - 16, Height - 2);
  end;

  Canvas.Pen.Color := clBlack;
  vRect.Left := Width - 12;
  vRect.Top := (Height - 4) div 2;
  Canvas.MoveTo(vRect.Left, vRect.Top);
  Canvas.LineTo(vRect.Left + 7, vRect.Top);
  Canvas.MoveTo(vRect.Left + 1, vRect.Top + 1);
  Canvas.LineTo(vRect.Left + 6, vRect.Top + 1);
  Canvas.MoveTo(vRect.Left + 2, vRect.Top + 2);
  Canvas.LineTo(vRect.Left + 5, vRect.Top + 2);
  Canvas.MoveTo(vRect.Left + 3, vRect.Top + 3);
  Canvas.LineTo(vRect.Left + 4, vRect.Top + 3);
end;

end.
