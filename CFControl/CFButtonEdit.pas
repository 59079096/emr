unit CFButtonEdit;

interface

uses
  Windows, Classes, Controls, CFControl, Graphics, Messages, CFEdit;

type
  TCBEdit = class(TCFEdit);

  TCButtonStyle = (cbsLookUp, cbsDrop, cbsFind, cbsFold, cbsNew, cbsOpen,
    cbsDateTime);

  TCCustomButtonEdit = class(TCFTextControl)
  private
    FEdit: TCBEdit;
    FButtonStyle: TCButtonStyle;
    FBtnMouseState: TMouseState;
    FButtonLeft: Integer;

    FOnButtonClick: TNotifyEvent;
    procedure SetButtonStyle(Value: TCButtonStyle);
  protected
    procedure DrawControl(ACanvas: TCanvas); override;
    procedure AdjustBounds; override;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    function GetText: string;
    procedure SetText(Value: string);
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(Value: TNotifyEvent);
    // 通过Tab移入焦点
    procedure CMUIActivate(var Message: TMessage); message CM_UIACTIVATE;
    // 通过Tab移出焦点
    procedure CMUIDeActivate(var Message: TMessage); message CM_UIDEACTIVATE;
    // 字体变化
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    // 左键双击
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    //
    procedure CMMouseEnter(var Msg: TMessage ); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage ); message CM_MOUSELEAVE;

    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    //
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ButtonStyle: TCButtonStyle read FButtonStyle write SetButtonStyle;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Text: string read GetText write SetText;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
  end;

  TCFButtonEdit = class(TCCustomButtonEdit)
  published
    property ButtonStyle;
    property OnButtonClick;
    property ReadOnly;
    property Text;
    property OnChange;
  end;

implementation

{$R CFButtonEdit.RES}

{ TCCustomButtonEdit }

procedure TCCustomButtonEdit.AdjustBounds;
var
  DC, vHeight, vWidth: HDC;
begin
  if not (csReading in ComponentState) then
  begin
    DC := GetDC(0);
    try
      Canvas.Handle := DC;
      Canvas.Font := Font;

      vWidth := GIconWidth + 2 * GPadding + GetSystemMetrics(SM_CYBORDER) * 4 + GMinWdith;
      if vWidth < Width then
      begin
        FEdit.Width := Width - (GIconWidth + 2 * GPadding + GetSystemMetrics(SM_CYBORDER) * 4);
        vWidth := Width;
      end;
      vHeight := Canvas.TextHeight('荆') + GetSystemMetrics(SM_CYBORDER) * 4;

      Canvas.Handle := 0;
    finally
      ReleaseDC(0, DC);
    end;

    SetBounds(Left, Top, vWidth, vHeight);
  end;
end;

procedure TCCustomButtonEdit.CMFontChanged(var Message: TMessage);
begin
  FEdit.Font := Font;
  inherited;
end;

procedure TCCustomButtonEdit.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  UpdateDirectUI;
end;

procedure TCCustomButtonEdit.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FBtnMouseState := FBtnMouseState - [cmsMouseIn];
  UpdateDirectUI;
end;

procedure TCCustomButtonEdit.CMUIActivate(var Message: TMessage);
begin
  Message.Result := FEdit.Perform(Message.Msg, Message.WParam, Message.LParam);
end;

procedure TCCustomButtonEdit.CMUIDeActivate(var Message: TMessage);
begin
  Message.Result := FEdit.Perform(Message.Msg, Message.WParam, Message.LParam);
end;

constructor TCCustomButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBtnMouseState := [];
  FEdit := TCBEdit.Create(Self);
  FEdit.BorderVisible := False;
  Width := Width + GIconWidth;
end;

destructor TCCustomButtonEdit.Destroy;
begin
  FEdit.Free;
  inherited;
end;

procedure TCCustomButtonEdit.DrawControl(ACanvas: TCanvas);
var
  vRect: TRect;
  vIcon: HICON;
begin
  inherited;
  vRect := Rect(0, 0, Width, Height);
  // 背景
  ACanvas.Brush.Style := bsSolid;
  if not FEdit.ReadOnly then
    ACanvas.Brush.Color := GThemeColor
  else
    ACanvas.Brush.Color := clInfoBk;
  ACanvas.Pen.Style := psClear;
  ACanvas.RoundRect(vRect, GRoundSize, GRoundSize);

  FEdit.DrawControl(ACanvas);

  if BorderVisible then  // 边框
  begin
    ACanvas.Brush.Style := bsClear;
    if Self.Focused or (cmsMouseIn in MouseState) then
      ACanvas.Pen.Color := GBorderHotColor
    else
      ACanvas.Pen.Color := GBorderColor;
    if BorderVisible then
      ACanvas.Pen.Style := psSolid
    else
      ACanvas.Pen.Style := psClear;
    ACanvas.RoundRect(vRect, GRoundSize, GRoundSize);
  end;

  case FButtonStyle of
    cbsDrop: vIcon := LoadIcon(HInstance, 'DROPDOWN');
    cbsFind: vIcon := LoadIcon(HInstance, 'FIND');
    cbsLookUp: vIcon := LoadIcon(HInstance, 'LOOKUP');
    cbsFold: vIcon := LoadIcon(HInstance, 'FOLD');
    cbsNew: vIcon := LoadIcon(HInstance, 'NEW');
    cbsOpen: vIcon := LoadIcon(HInstance, 'OPEN');
    cbsDateTime: vIcon := LoadIcon(HInstance, 'DATETIME');
  end;
  ACanvas.Pen.Color := GLineColor;
  ACanvas.MoveTo(FButtonLeft, GBorderWidth);
  ACanvas.LineTo(FButtonLeft, Height - GBorderWidth);
  DrawIconEx(ACanvas.Handle, FButtonLeft, (Height - GIconWidth) div 2,
    vIcon, GIconWidth, GIconWidth, 0, 0, DI_NORMAL);
end;

function TCCustomButtonEdit.GetOnChange: TNotifyEvent;
begin
  Result := FEdit.OnChange;
end;

function TCCustomButtonEdit.GetReadOnly: Boolean;
begin
  Result := FEdit.ReadOnly;
end;

function TCCustomButtonEdit.GetText: string;
begin
  Result := FEdit.Text;
end;

procedure TCCustomButtonEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FEdit.KeyDown(Key, Shift);
end;

procedure TCCustomButtonEdit.KeyPress(var Key: Char);
begin
  FEdit.KeyPress(Key);
end;

procedure TCCustomButtonEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if PtInRect(FEdit.ClientRect, Point(X, Y)) then
    FEdit.MouseDown(Button, Shift, X, Y);
end;

procedure TCCustomButtonEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vRect: TRect;
begin
  inherited;
  if PtInRect(FEdit.ClientRect, Point(X, Y)) then
    FEdit.MouseMove(Shift, X, Y)
  else
  begin
    vRect := Rect(Width - GIconWidth - GPadding, (Height - GIconWidth) div 2,
      Width - GBorderWidth, Height - GBorderWidth);
    if PtInRect(vRect, Point(X, Y)) then
    begin
      if not (cmsMouseIn in FBtnMouseState) then
      begin
        FBtnMouseState := FBtnMouseState + [cmsMouseIn];
        UpdateDirectUI(vRect);
      end;
    end
    else
    begin
      if cmsMouseIn in FBtnMouseState then
      begin
        FBtnMouseState := FBtnMouseState - [cmsMouseIn];
        UpdateDirectUI(vRect);
      end;
    end;
  end;
end;

procedure TCCustomButtonEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if PtInRect(FEdit.ClientRect, Point(X, Y)) then
    FEdit.MouseUp(Button, Shift, X, Y)
  else
  if PtInRect(Rect(Width - GIconWidth - GPadding * 2,
                   GBorderWidth,
                   Width - GBorderWidth,
                   Height - GBorderWidth),
              Point(X, Y))
  then
  begin
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self);
  end;
end;

procedure TCCustomButtonEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  FEdit.Left := Left;
  FEdit.Top := Top;
  FEdit.Width := Width - GIconWidth - GPadding div 2;
  FButtonLeft := FEdit.Width;
end;

procedure TCCustomButtonEdit.SetButtonStyle(Value: TCButtonStyle);
begin
  if FButtonStyle <> Value then
  begin
    FButtonStyle := Value;
    UpdateDirectUI;
  end;
end;

procedure TCCustomButtonEdit.SetOnChange(Value: TNotifyEvent);
begin
  FEdit.OnChange := Value;
end;

procedure TCCustomButtonEdit.SetReadOnly(Value: Boolean);
begin
  FEdit.ReadOnly := Value;
end;

procedure TCCustomButtonEdit.SetText(Value: string);
begin
  FEdit.Text := Value;
end;

procedure TCCustomButtonEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  if PtInRect(FEdit.ClientRect, Point(Message.XPos, Message.YPos)) then
    FEdit.WMLButtonDblClk(Message);
end;

end.
