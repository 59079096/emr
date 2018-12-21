unit CFControl;

interface

uses
  Windows, Classes, Controls, Messages, Graphics;

const
  WM_C_CAPTIONBAR = WM_USER + $100;
  WM_C_CARETCHANGE = WM_USER + $101;
  WM_C_LBUTTONDOWN = WM_USER + $102;
  WM_C_LBUTTONUP = WM_USER + $103;
  WM_C_MOUSEMOVE = WM_USER + $104;
  WM_C_LBUTTONDBLCLK = WM_USER + $105;
  WM_C_REMOVECONTROL = WM_USER + $106;

type
  TMouseState = set of (cmsMouseIn, cmsMouseDown);
  TAlginPosition = (capLeft, capTop ,capRight, capBottom, capTopCenter, capBottomCenter);

  TOrientation = (coHorizontal, coVertical);  // 水平/垂直、横向/纵向

  NaturalNumber = 1..High(Integer);

  TContainerControl = class;

  /// <summary>
  /// CControl基类
  /// </summary>
  TCFCustomControl = class(TGraphicControl)
  private
    FAlpha: Byte;
    FMouseState: TMouseState;
    FBorderVisible: Boolean;
    FBorderColor: TColor;
    FUpdateCount: Integer;
    FCParent: TContainerControl;
  protected
    procedure DrawControl(ACanvas: TCanvas); virtual;
    procedure SetAlpha(Value: Byte);
    procedure SetBorderVisible(Value: Boolean);
    procedure SetCParent(AParent: TContainerControl); virtual;
    //
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    function GetDeviceContext(var WindowHandle: HWND): HDC; overload; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    //procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function GetFocus: Boolean; virtual;
    procedure SetFocus(Value: Boolean); virtual;
    function GetTabStop: Boolean; virtual;
    procedure SetTabStop(Value: Boolean); virtual;

    procedure CMMouseEnter(var Msg: TMessage ); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage ); message CM_MOUSELEAVE;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    property OnClick;
  public
    constructor Create(AOwner: TComponent); override;
    function RootForm: TWinControl;
    function GetUIHandle: THandle; virtual;
    function CanFocus: Boolean; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure TimerProc(const AElapse: Word); virtual;
    procedure DrawTo(const ACanvas: TCanvas); virtual;
    procedure UpdateDirectUI; overload;
    procedure UpdateDirectUI(ARect: TRect); overload;
    //
    property MouseState: TMouseState read FMouseState write FMouseState;
    property Focus: Boolean read GetFocus write SetFocus;
  published
    property Alpha: Byte read FAlpha write SetAlpha;
    property BorderVisible: Boolean read FBorderVisible write SetBorderVisible;
    property TabStop: Boolean read GetTabStop write SetTabStop;
    property CParent: TContainerControl read FCParent write SetCParent;
    //
    property Align;
    property Enabled;
    property Visible;
  end;

  /// <summary>
  /// 文本类控件基类
  /// </summary>
  TCFTextControl = class(TCFCustomControl)
  private
    FOnChange: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;

    function DoKeyDown(var Message: TWMKey): Boolean;
    function DoKeyPress(var Message: TWMKey): Boolean;
    function DoKeyUp(var Message: TWMKey): Boolean;
  protected
    procedure DrawControl(ACanvas: TCanvas); override;
    procedure AdjustBounds; dynamic;
    procedure Loaded; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyUp(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyPress(var Key: Char); dynamic;

    function TextMetricWidth(const AText: string): Integer;
    function TextMetricHeight(const AText: string): Integer;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
  public
    class function TextMetric(const AText: string; AFont: TFont): TSize;
    property Text;
  published
    property Font;
  end;

  /// <summary>
  /// 容器控件基类
  /// </summary>
  TContainerControl = class(TCFCustomControl)
  private
    FCControls: TList;
  protected
    procedure DrawControl(ACanvas: TCanvas); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Remove(AControl: TCFCustomControl); virtual;
    procedure Insert(AControl: TCFCustomControl); virtual;
    property CControls: TList read FCControls;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  function KeysToShiftState(Keys: Word): TShiftState;

var
  GRoundSize: Byte = 5;
  GPadding: Byte = 2;
  GIconWidth: Byte = 16;
  GBorderWidth: Byte = 1;
  GSpace: Byte = 2;
  GMinWdith: Integer = 10;
  GMinHeight: Integer = 20;
  // 全局颜色
  GThemeColor: TColor = $FCFCFC;
  GTitleBackColor: TColor = $E8FFDF;
  GBackColor: TColor = $88DA83;
  GHotColor: TColor = $98F392;
  GAlertColor: TColor = $839EDA;
  GDownColor: TColor = $90E78B;
  GBorderColor: TColor = $78C074;
  GBorderHotColor: TColor = $588D55;
  GLineColor: TColor = clMedGray;
  GHightLightColor: TColor = clHighlight;

  {GThemeColor: TColor = $00F7FFF6;
  GTitleBackColor: TColor = $00F0F0F0;
  //GTitleForegColor: TColor = $00F0F0F0;
  GBackColor: TColor = $0080DE79;
  GHotColor: TColor = $008FF887;
  GAlertColor: TColor = $00799BDE;
  GDownColor: TColor = $0087EB80;
  GBorderColor: TColor = $0063AB5D;
  GBorderHotColor: TColor = $0071C56B;
  GLineColor: TColor = clMedGray;}

procedure CalcThemePartColor;

implementation

uses
  SysUtils, CFColorUtils, CFForm;

function KeysToShiftState(Keys: Word): TShiftState;
begin
  Result := [];
  if Keys and MK_SHIFT <> 0 then Include(Result, ssShift);
  if Keys and MK_CONTROL <> 0 then Include(Result, ssCtrl);
  if Keys and MK_LBUTTON <> 0 then Include(Result, ssLeft);
  if Keys and MK_RBUTTON <> 0 then Include(Result, ssRight);
  if Keys and MK_MBUTTON <> 0 then Include(Result, ssMiddle);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;

function KeyDataToShiftState(KeyData: Longint): TShiftState;
const
  AltMask = $20000000;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if KeyData and AltMask <> 0 then Include(Result, ssAlt);
end;

{ TCFCustomControl }

procedure TCFCustomControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

function TCFCustomControl.CanFocus: Boolean;
var
  vControl: TWinControl;
  //Form: TCustomForm;
begin
  Result := False;
  if not (Visible and Enabled) then Exit;
  vControl := Self.Parent;
  if not (vControl.Visible and vControl.Enabled) then Exit;
  Result := True;
  {Form := GetParentForm(Self);
  if Form <> nil then
  begin
    Control := Self;
    while Control <> Form do
    begin
      if not (Control.FVisible and Control.Enabled) then Exit;
      Control := Control.Parent;
    end;
    Result := True;
  end;}
end;

function TCFCustomControl.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if NewWidth < GMinWdith then
    NewWidth := GMinWdith;
  if NewHeight < GMinHeight then
    NewHeight := GMinHeight;
end;

procedure TCFCustomControl.CMColorChanged(var Message: TMessage);
begin
  Message.Result := 1;  // 屏蔽基类的重绘功能
end;

procedure TCFCustomControl.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseState := FMouseState + [cmsMouseIn];
end;

procedure TCFCustomControl.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseState := FMouseState - [cmsMouseIn, cmsMouseDown];
end;

procedure TCFCustomControl.CMParentColorChanged(var Message: TMessage);
begin
  Message.Result := 1;
end;

constructor TCFCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlpha := 255;
  FUpdateCount := 0;
  FBorderColor := GBorderColor;
  FBorderVisible := True;
  //ControlStyle := ControlStyle + [csReplicatable];  控件通过PaintTo方法将它的图像复制到任意Canvas中去
end;

procedure TCFCustomControl.DrawTo(const ACanvas: TCanvas);
var
  vMemDC: HDC;
  vMemBitmap, vOldBitmap: HBITMAP;
  vCanvas: TCanvas;
  vBlendFunction: TBlendFunction;
  vDCState: Integer;
  //vPoint: TPoint;
begin
  if FAlpha <> 255 then
  begin
    // 不使用GDI因为在dll中初始化gdi时界面不显示
    vMemBitmap := CreateCompatibleBitmap(ACanvas.Handle, Width, Height);
    try
      vMemDC := CreateCompatibleDC(ACanvas.Handle);
      vOldBitmap := SelectObject(vMemDC, vMemBitmap);
      BitBlt(vMemDC, 0, 0, Width, Height, ACanvas.Handle, Left, Top, SRCCOPY);  // 拷贝Canvas此位置的图像
      try
        vCanvas := TCanvas.Create;
        vCanvas.Handle := vMemDC;
        vDCState := Windows.SaveDC(vCanvas.Handle);  // 保存设备环境到上下文栈上
        try
          DrawControl(vCanvas);  // 绘制控件(为支持直接调用DrawTo处理透明效果，所以透明处理放在各控件的DrawTo中)
        finally
          Windows.RestoreDC(vCanvas.Handle, vDCState);  // 从设备上下文栈中对恢复设备环境
        end;

        vBlendFunction.BlendOp := AC_SRC_OVER;
        vBlendFunction.BlendFlags := 0;
        vBlendFunction.AlphaFormat := AC_SRC_OVER;  // 源位图必须是32位深
        vBlendFunction.SourceConstantAlpha := FAlpha; // 透明度
        Windows.AlphaBlend(ACanvas.Handle,
                           Left,
                           Top,
                           Width,
                           Height,
                           vMemDC,
                           0,
                           0,
                           Width,
                           Height,
                           vBlendFunction
                           );
      finally
        SelectObject(vMemDC, vOldBitmap)
      end;
    finally
      vCanvas.Free;
      DeleteDC(vMemDC);
      DeleteObject(vMemBitmap);
    end;
  end
  else
    DrawControl(ACanvas);
end;

procedure TCFCustomControl.EndUpdate;
begin
  Dec(FUpdateCount);
  UpdateDirectUI;  // 其内校验 FUpdateCount = 0
end;

procedure TCFCustomControl.DrawControl(ACanvas: TCanvas);
begin
  {ACanvas.Brush.Color := GThemeColor;
  ACanvas.FillRect(ClientRect);}
end;

function TCFCustomControl.GetDeviceContext(var WindowHandle: HWND): HDC;
begin
  WindowHandle := GetUIHandle;
  if csDesigning in ComponentState then
    Result := GetDCEx(WindowHandle, 0, DCX_CACHE or DCX_CLIPSIBLINGS)
  else
    Result := GetDC(WindowHandle);
  if Result = 0 then
    raise Exception.Create('GUI异常：GetDeviceContext 创建窗体设备上下文错误！');
    //raise EOutOfResources.CreateRes({$IFNDEF CLR}@{$ENDIF}SWindowDCError);
end;

function TCFCustomControl.GetFocus: Boolean;
begin
end;

function TCFCustomControl.GetTabStop: Boolean;
begin
  Result := False;  // 默认不支持Tab切换成焦点
end;

function TCFCustomControl.GetUIHandle: THandle;
var
  vRootForm: TWinControl;
begin
  Result := 0;
  vRootForm := RootForm;
  if vRootForm.HandleAllocated then  // 判断句柄有效性，如果直接赋值RootForm的Handle会造成新创建句柄，而此时界面可能从没有显示过，造成错误
    Result := RootForm.Handle;
end;

procedure TCFCustomControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FMouseState := FMouseState + [cmsMouseDown];
end;

procedure TCFCustomControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FMouseState := FMouseState - [cmsMouseDown];
end;

procedure TCFCustomControl.Paint;
begin
  inherited;
  if FUpdateCount <> 0 then Exit;  // 再次判断，防止非调用UpdateDirectUI造成的重绘
  //if Self.CParent <> nil then Exit;  // 如果有父控件，由父控件绘制自己
  DrawTo(Canvas);
end;

function TCFCustomControl.RootForm: TWinControl;
var
  vOwner: TComponent;
begin
  Result := nil;
  if Self.Parent <> nil then
    Result := Self.Parent
  else
  begin
    vOwner := Self;
    while True do
    begin
      if vOwner.Owner <> nil then
        vOwner := vOwner.Owner
      else
        raise Exception.Create('异常：RootForm找不到窗体！');
      if vOwner is TWinControl then
      begin
        Result := (vOwner as TWinControl);
        Break;
      end;
    end;
  end;
end;

procedure TCFCustomControl.SetAlpha(Value: Byte);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    UpdateDirectUI
  end;
end;

procedure TCFCustomControl.SetBorderVisible(Value: Boolean);
begin
  if FBorderVisible <> Value then
  begin
    FBorderVisible := Value;
    UpdateDirectUI;
  end;
end;

procedure TCFCustomControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
//var
//  vLeft, vTop, vWidth, vHeight: Integer;
begin
  {if Self.CParent <> nil then
  begin
    if Align <> alNone then
    begin
      ControlState := ControlState + [csAligning];
      try
        case Align of
          alTop:
            begin
              ALeft := CParent.Left;
              ATop := CParent.Top;
              AWidth := CParent.Width;
              AHeight := FExplicitHeight;
            end;
          alBottom:
            begin
              ALeft := CParent.Left;
              ATop := CParent.Top + CParent.Height - Self.Height;
              AWidth := CParent.Width;
              AHeight := FExplicitHeight;
            end;
          alLeft:
            begin
              ALeft := CParent.Left;
              ATop := CParent.Top;
              AWidth := FExplicitWidth;
              AHeight := CParent.Height;
            end;
          alRight:
            begin
              ALeft := CParent.Left + CParent.Width - Self.Width;
              ATop := CParent.Top;
              AWidth := FExplicitWidth;
              AHeight := CParent.Height;
            end;
          alClient:
            begin
              ALeft := CParent.Left;
              ATop := CParent.Top;
              AWidth := CParent.Width;
              AHeight := CParent.Height;
            end;
          alCustom: ;
        end;
        inherited SetBounds(ALeft, ATop, AWidth, AHeight);
      finally
        ControlState := ControlState - [csAligning];
      end;
    end;
  end; }
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TCFCustomControl.SetCParent(AParent: TContainerControl);
begin
  if FCParent <> AParent then
  begin
    if AParent = Self then
      raise Exception.Create('异常：不能将自己设置为CParent！');
    if FCParent <> nil then
      FCParent.Remove(Self);
    if AParent <> nil then
      AParent.Insert(Self);
  end;
end;

procedure TCFCustomControl.SetFocus(Value: Boolean);
begin
end;

procedure TCFCustomControl.SetTabStop(Value: Boolean);
begin
end;

procedure TCFCustomControl.TimerProc(const AElapse: Word);
begin
end;

procedure TCFCustomControl.UpdateDirectUI(ARect: TRect);
var
  vBounds: TRect;
  vHandle: THandle;
begin
  if FUpdateCount <> 0 then Exit;

  vHandle := GetUIHandle;
  if vHandle <> 0 then  // 有效UI
  begin
    vBounds := BoundsRect;
    OffsetRect(ARect, vBounds.Left, vBounds.Top);
    InvalidateRect(vHandle, ARect, False);  // 绘制区域，False 不进行擦除
  end;
end;

procedure TCFCustomControl.UpdateDirectUI;
begin
  UpdateDirectUI(ClientRect);
//  InvalidateRect(GetUIHandle, BoundsRect, False);
end;

{ TCFTextControl }

procedure TCFTextControl.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

class function TCFTextControl.TextMetric(const AText: string; AFont: TFont): TSize;
var
  vDC: HDC;
  hOldFont: hGdiObj;
begin
  vDC := GetDC(0);
  try
    hOldFont := GetCurrentObject(vDC, OBJ_FONT);
    SelectObject(vDC, AFont.Handle);
    try
      Windows.GetTextExtentPoint32(vDC, AText, Length(AText), Result);
    finally
      SelectObject(vDC, hOldFont);
    end;
  finally
    ReleaseDC(0, vDC);
  end;
end;

function TCFTextControl.TextMetricHeight(const AText: string): Integer;
begin
  Result := TextMetric(AText, Font).cy;
end;

function TCFTextControl.TextMetricWidth(const AText: string): Integer;
begin
  Result := TextMetric(AText, Font).cx;
end;

procedure TCFTextControl.WMChar(var Message: TWMChar);
begin
  if not DoKeyPress(Message) then
    inherited;
end;

procedure TCFTextControl.WMKeyDown(var Message: TWMKeyDown);
begin
  if not DoKeyDown(Message) then
    inherited;
end;

procedure TCFTextControl.WMKeyUp(var Message: TWMKeyUp);
begin
  if not DoKeyUp(Message) then
    inherited;
end;

procedure TCFTextControl.AdjustBounds;
var
  DC: HDC;
  vNewHeight, vNewWidth: Integer;
begin
  if not (csReading in ComponentState) then
  begin
    DC := GetDC(0);
    try
      Canvas.Handle := DC;
      Canvas.Font := Font;
      vNewHeight := Canvas.TextHeight('字') + GetSystemMetrics(SM_CYBORDER) * 4;
      vNewWidth := Canvas.TextWidth(Caption) + GetSystemMetrics(SM_CYBORDER) * 4;
      if vNewWidth < GMinWdith then
        vNewWidth := GMinWdith;
      Canvas.Handle := 0;
    finally
      ReleaseDC(0, DC);
    end;
    SetBounds(Left, Top, vNewWidth, vNewHeight);
  end;
end;

procedure TCFTextControl.CMFontChanged(var Message: TMessage);
begin
  AdjustBounds;
  inherited;
  UpdateDirectUI;
end;

procedure TCFTextControl.CMTextChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  UpdateDirectUI;
end;

function TCFTextControl.DoKeyDown(var Message: TWMKey): Boolean;
var
  ShiftState: TShiftState;
  Form: TWinControl;
  LCharCode: Word;
begin
  Result := True;
  Form := RootForm;
  if (Form <> nil)
    and TJTForm(Form).KeyPreview
    and TJTForm(Form).DoKeyDown(Message)
  then
    Exit;

  with Message do
  begin
    ShiftState := KeyDataToShiftState(KeyData);
    if not (csNoStdEvents in ControlStyle) then
    begin
      LCharCode := CharCode;
      KeyDown(LCharCode, ShiftState);
      CharCode := LCharCode;
      if LCharCode = 0 then Exit;
    end;
  end;
  Result := False;
end;

function TCFTextControl.DoKeyPress(var Message: TWMKey): Boolean;
var
  Form: TWinControl;
  Ch: Char;
begin
  Result := True;
  Form := RootForm;
  if (Form <> nil)
    and TJTForm(Form).KeyPreview
    and TJTForm(Form).DoKeyPress(Message)
  then
    Exit;

  if not (csNoStdEvents in ControlStyle) then
    with Message do
    begin
      Ch := Char(CharCode);
      KeyPress(Ch);
      CharCode := Word(Ch);
      if Char(CharCode) = #0 then Exit;
    end;
  Result := False;
end;

function TCFTextControl.DoKeyUp(var Message: TWMKey): Boolean;
var
  ShiftState: TShiftState;
  Form: TWinControl;
  LCharCode: Word;
begin
  Result := True;
  Form := RootForm;
  if (Form <> nil)
    and TJTForm(Form).KeyPreview
    and TJTForm(Form).DoKeyUp(Message)
  then
    Exit;

  with Message do
  begin
    ShiftState := KeyDataToShiftState(KeyData);
    if not (csNoStdEvents in ControlStyle) then
    begin
      LCharCode := CharCode;
      KeyUp(LCharCode, ShiftState);
      CharCode := LCharCode;
      if LCharCode = 0 then Exit;
    end;
  end;
  Result := False;
end;

procedure TCFTextControl.DrawControl(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Font := Font;
end;

procedure TCFTextControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Key, Shift);
end;

procedure TCFTextControl.KeyPress(var Key: Char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Self, Key);
end;

procedure TCFTextControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Key, Shift);
end;

procedure CalcThemePartColor;
begin
  GHotColor := GetHotColor(GBackColor);
  GDownColor := GetDownColor(GBackColor);
  GBorderColor := GetBorderColor(GBackColor);
  GBorderHotColor := GetBorderHotColor(GBackColor);
  //GTitleForegColor := GetDownColor(GTitleBackColor)
end;

{ TContainerControl }

constructor TContainerControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  FCControls := TList.Create;
end;

destructor TContainerControl.Destroy;
begin
  FCControls.Free;
  inherited Destroy;
end;

procedure TContainerControl.DrawControl(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Brush.Color := GThemeColor;
  ACanvas.FillRect(ClientRect);
  if BorderVisible then  // 绘制边框
  begin
    with ACanvas do
    begin
      Pen.Color := GBorderColor;
      MoveTo(0, 0);
      LineTo(Width - 1, 0);
      LineTo(Width - 1, Height - 1);
      LineTo(0, Height - 1);
      LineTo(0, 0);
    end;
  end;
end;

procedure TContainerControl.Insert(AControl: TCFCustomControl);
begin
  FCControls.Add(AControl);
  if AControl.Left < Left then
    AControl.Left := Left;
  if AControl.Left > Left + Width then
    AControl.Left := Left + Width - AControl.Width;

  if AControl.Top < Top then
    AControl.Top := Top;
  if AControl.Top > Top + Height then
    AControl.Top := Top + Height - AControl.Height;
  AControl.BringToFront;
  AControl.FCParent := Self;
end;

procedure TContainerControl.Remove(AControl: TCFCustomControl);
begin
  FCControls.Remove(AControl);
  AControl.FCParent := nil;
end;

procedure TContainerControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
{var
  i, vIncLeft, vIncTop: Integer;
  vCControl: TCFCustomControl;}
begin
  {vIncLeft := ALeft - Left;
  vIncTop := ATop - Top;
  for i := 0 to FCControls.Count - 1 do
  begin
    vCControl := FCControls[i];
    vCControl.Left := vCControl.Left + vIncLeft;
    vCControl.Top := vCControl.Top + vIncTop;
  end;}
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

initialization
  CalcThemePartColor;

end.
