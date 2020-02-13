unit CFToolButton;

interface

uses
  Windows, Classes, Controls, Messages, Graphics, CFControl;

type
  TCFButtonStates = set of (cfmsIn, cfmsDown, cfmsChecked);

  TOnPaintIconEvent = procedure(const AImageIndex: Integer; const ACanvas: TCanvas; const ARect: TRect) of object;

  TCFToolButton = class(TGraphicControl)
  private
    FImageIndex: Integer;
    FOnPaintIcon: TOnPaintIconEvent;

    function GetChecked: Boolean;
    procedure SetChecked(const Value: Boolean);

    procedure SetImageIndex(const Value: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    /// <summary> 鼠标移入 </summary>
    procedure CMMouseEnter(var Msg: TMessage ); message CM_MOUSEENTER;

    /// <summary> 鼠标移出 </summary>
    procedure CMMouseLeave(var Msg: TMessage ); message CM_MOUSELEAVE;
  protected
    FStates: TCFButtonStates;
    /// <summary> 绘制 </summary>
    /// <param name="ACanvas">呈现画布</param>
    procedure Paint; override;
    function GetImageRect: TRect; virtual;
    function GetTextRect: TRect; virtual;
    procedure UpdateView;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property OnPaintIcon: TOnPaintIconEvent read FOnPaintIcon write FOnPaintIcon;
  published
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Checked: Boolean read GetChecked write SetChecked;
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
    function GetTextRect: TRect; override;
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
  Include(FStates, cfmsIn);
  UpdateView;
end;

procedure TCFToolButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  Exclude(FStates, cfmsIn);
  UpdateView;
end;

constructor TCFToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageIndex := -1;
  Width := 75;
  Height := 25;
end;

function TCFToolButton.GetChecked: Boolean;
begin
  Result := cfmsChecked in FStates;
end;

function TCFToolButton.GetImageRect: TRect;
begin
  if FImageIndex >= 0 then
    Result := Rect(0, 0, 24, Height)
  else
    Result := Rect(0, 0, 0, 0);
end;

function TCFToolButton.GetTextRect: TRect;
begin
  if FImageIndex >= 0 then
    Result := Rect(24, 0, Width, Height)
  else
    Result := Rect(0, 0, Width, Height);
end;

procedure TCFToolButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  Include(FStates, cfmsDown);
  UpdateView;
end;

procedure TCFToolButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  Exclude(FStates, cfmsDown);
  UpdateView;
end;

procedure TCFToolButton.Paint;
var
  vRect: TRect;
  vText: string;
  vBackColor: TColor;
  vImage: TGraphic;
begin
  vRect := Rect(0, 0, Width, Height);
  if cfmsIn in FStates then  // 鼠标在控件内
  begin
    vBackColor := GAreaBackColor;
    //Canvas.Pen.Width := 1;
    //Canvas.Pen.Color := GetBorderColor(vBackColor);

    if cfmsDown in FStates then  // 鼠标按下
      Canvas.Brush.Color := GetDownColor(vBackColor)
    else
      Canvas.Brush.Color := GetHotColor(vBackColor);

    Canvas.FillRect(vRect);
  end
  else  // 普通状态
    Canvas.Brush.Style := bsClear;

  if (FImageIndex >= 0) and (Assigned(FOnPaintIcon)) then
  begin
    vRect := GetImageRect;
    FOnPaintIcon(FImageIndex, Canvas, vRect);
  end;

  vRect := GetTextRect;
  vText := Caption;
  Canvas.TextRect(vRect, vText, [tfSingleLine, tfCenter,tfVerticalCenter]);

  if cfmsChecked in FStates then
  begin
    vBackColor := GAreaBackColor;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := GetBorderColor(vBackColor);
    vRect := Rect(0, 0, Width, Height);
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(vRect);
  end;
end;

procedure TCFToolButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TCFToolButton.SetChecked(const Value: Boolean);
begin
  if Value then
    Include(FStates, cfmsChecked)
  else
    Exclude(FStates, cfmsChecked);

  UpdateView;
end;

procedure TCFToolButton.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    UpdateView;
  end;
end;

procedure TCFToolButton.UpdateView;
begin
  Self.Invalidate;
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

function TCFMenuButton.GetTextRect: TRect;
begin
  Result := inherited GetTextRect;
  Result.Right := Result.Right - 12;
end;

procedure TCFMenuButton.Paint;
var
  vRect: TRect;
  vText: string;
  vBackColor: TColor;
begin
  inherited Paint;
  vRect := Rect(0, 0, Width, Height);
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
