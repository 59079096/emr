unit CFColorCombobox;

interface

uses
  Windows, Classes, Graphics, Controls, Messages, CFControl, CFPopupForm, CFPopupFormBase,
  CFColorPad;

type
  TPadForm = class(TfrmPopupFormBase)
  private
    FColorPad: TCFRichColorPad;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ColorPad: TCFRichColorPad read FColorPad;
  end;

  TCFColorPopup = class(TCFPopupForm)
  private
    FPadForm: TPadForm;
    FOnChanged: TNotifyEvent;

    procedure DoColorChange(Sender: TObject);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure DoPopupClose(Sender: TObject);
  protected
    function StopPeekMessage(const AMsg: TMsg): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Color: TColor read GetColor write SetColor;
  published
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TCFColorCombobox = class(TCFCustomControl)
  private
    FColorPopup: TCFColorPopup;
    //FOnCloseUp: TCloseUpEvent;
    function GetButtonRect: TRect;
    procedure PopupItem;
    procedure DoColorChange(Sender: TObject);
  protected
    procedure DrawControl(ACanvas: TCanvas); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R CFCombobox.res}

{ TCFColorCombobox }

constructor TCFColorCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorPopup := TCFColorPopup.Create(Self);
  FColorPopup.OnChanged := DoColorChange;

  Width := 120;
  Height := 20;
end;

destructor TCFColorCombobox.Destroy;
begin
  FColorPopup.Free;
  inherited;
end;

procedure TCFColorCombobox.DoColorChange(Sender: TObject);
begin
  Self.UpdateDirectUI;
end;

procedure TCFColorCombobox.DrawControl(ACanvas: TCanvas);

  procedure DrawDownArrow;
  var
    vBmp: TBitmap;
  begin
    {if cmsMouseDown in MouseState then
    begin
      ACanvas.Brush.Color := $00E5C27F;
      ACanvas.FillRect(FButtonRect);

      ACanvas.Pen.Color := GBorderHotColor;
      ACanvas.MoveTo(FButtonRect.Left - 1, FButtonRect.Top);
      ACanvas.LineTo(FButtonRect.Left - 1, FButtonRect.Bottom);

    end
    else
    if cmsMouseIn in MouseState then
    begin
      ACanvas.Brush.Color := GHotColor;
      ACanvas.FillRect(FButtonRect);

      ACanvas.Pen.Color := GBorderColor;
      ACanvas.MoveTo(FButtonRect.Left - 1, FButtonRect.Top);
      ACanvas.LineTo(FButtonRect.Left - 1, FButtonRect.Bottom);
    end;}

    vBmp := TBitmap.Create;
    try
      vBmp.Transparent := True;
      vBmp.LoadFromResourceName(HInstance, 'DROPDOWN');
      ACanvas.Draw(Width - GSpace - GIconWidth, GSpace, vBmp);
    finally
      vBmp.Free;
    end;
  end;

var
  vRect: TRect;
begin
  inherited DrawControl(ACanvas);

  vRect := Rect(0, 0, Width, Height);

  if Self.Focused or (cmsMouseIn in MouseState) then
    ACanvas.Pen.Color := GBorderHotColor
  else
    ACanvas.Pen.Color := GBorderColor;

  if BorderVisible then
    ACanvas.Pen.Style := psSolid
  else
    ACanvas.Pen.Style := psClear;

  if BorderVisible then  // 显示边框时圆角区域
    ACanvas.Rectangle(vRect)
  else
    ACanvas.FillRect(vRect);

  InflateRect(vRect, -GSpace, -GSpace);
  if FColorPopup.Color <> clNone then
  begin
    ACanvas.Brush.Color := FColorPopup.Color;
    ACanvas.FillRect(vRect);
  end
  else
  begin

  end;

  DrawDownArrow;
end;

function TCFColorCombobox.GetButtonRect: TRect;
begin
  Result := Bounds(Width - GSpace - GIconWidth, GSpace, GIconWidth, GIconWidth);
end;

procedure TCFColorCombobox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vRect: TRect;
begin
  vRect := GetButtonRect;
  if (not (csDesigning in ComponentState)) and PtInRect(vRect, Point(X, Y)) then
    PopupItem
  else
    inherited;
end;

procedure TCFColorCombobox.PopupItem;
begin
  FColorPopup.Popup(Self);
end;

{ TCFColorPopup }

constructor TCFColorPopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPadForm := TPadForm.Create(nil);
  FPadForm.ColorPad.OnChange := DoColorChange;
  Self.PopupControl := FPadForm;
  Self.OnPopupClose := DoPopupClose;
end;

destructor TCFColorPopup.Destroy;
begin
  FPadForm.Free;
  inherited Destroy;
end;

procedure TCFColorPopup.DoColorChange(Sender: TObject);
begin
  Self.ClosePopup(False);
end;

procedure TCFColorPopup.DoPopupClose(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TCFColorPopup.GetColor: TColor;
begin
  Result := FPadForm.ColorPad.Select;
end;

procedure TCFColorPopup.SetColor(const Value: TColor);
begin
  FPadForm.ColorPad.Select := Value;
end;

function TCFColorPopup.StopPeekMessage(const AMsg: TMsg): Boolean;
//var
//  vPt: TPoint;
//  vRect: TRect;
begin
  Result := inherited StopPeekMessage(AMsg);

  {case AMsg.message of
    WM_NCLBUTTONDOWN, WM_NCLBUTTONDBLCLK, WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
    WM_NCRBUTTONDOWN, WM_NCRBUTTONDBLCLK, WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
    WM_NCMBUTTONDOWN, WM_NCMBUTTONDBLCLK, WM_MBUTTONDOWN, WM_MBUTTONDBLCLK:
      begin
        if not IsPopupWindow(AMsg.hwnd) then  // 不在弹出窗体上
          Result := True;
      end;
  end;}
end;

{ TPadForm }

constructor TPadForm.Create(AOwner: TComponent);
begin
  inherited;
  FColorPad := TCFRichColorPad.Create(Self);
  Self.Width := FColorPad.Width;
  Self.Height := FColorPad.Height;
  FColorPad.Parent := Self;
end;

destructor TPadForm.Destroy;
begin
  FColorPad.Free;
  inherited Destroy;
end;

end.
