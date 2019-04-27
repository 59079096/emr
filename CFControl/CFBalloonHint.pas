unit CFBalloonHint;

interface

uses
  Windows, Classes, Messages, SysUtils, Graphics, CFMMTimer;

type
  TBalloonAlignment = (cbaCenter, cbaFollowMouse);

  TCFBalloonHint = class(TObject)
  private
    FHintWindow: HWND;
    FTimer: TCFMMTimer;
    FAlignment: TBalloonAlignment;
    FText: string;
    //FOnDely: TNotifyEvent;
    procedure IniFont(var AFont: TLogFont);
    function GetDelay: Cardinal;
    procedure SetDelay(const Value: Cardinal);
    procedure DoDelayTimer(Sender: TObject);
    procedure DoPaintHint(const ADC: HDC);
    procedure SetText(const AText: string);
  protected
    procedure RegFormClass;
    procedure CreateHandle;
    procedure WndProc(var Message: TMessage); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ShowHint;
    property Delay: Cardinal read GetDelay write SetDelay;
    property Text: string read FText write SetText;
  published
    //property OnDely: TNotifyEvent read FOnDely write FOnDely;
  end;

  procedure BalloonMessage(const AText: string; const ADelay: Cardinal = 1500);

implementation

uses
  TypInfo, MMSystem;

var
  FMouseHook: HHOOK;
  FHW: HWND;

procedure BalloonMessage(const AText: string; const ADelay: Cardinal = 1500);

  {procedure DoBalloonOnDely(Sender: TObject);
  begin
    FreeAndNil(TCFBalloonHint(Sender));
  end;}

var
  vBalloonHint: TCFBalloonHint;
  //vMth: TMethod;
begin
  vBalloonHint := TCFBalloonHint.Create;
  vBalloonHint.Delay := ADelay;
  vBalloonHint.Text := AText;
  //vMth.Data := vBalloonHint;
  //vMth.Code := @DoBalloonOnDely;
  //SetMethodProp(vBalloonHint, 'OnDely', vMth);
  //System.TypInfo.SetMethodProp(vBalloonHint, 'OnDely', vMth);
  vBalloonHint.ShowHint;
end;

{ TCFBalloonHint }

constructor TCFBalloonHint.Create;
begin
  inherited Create;
  FHintWindow := 0;
  FMouseHook := 0;
  FText := '';
  RegFormClass;
  CreateHandle;
  FHW := FHintWindow;
  FAlignment := cbaFollowMouse;
  FTimer := TCFMMTimer.Create;
  FTimer.OnTimer := DoDelayTimer;
end;

procedure TCFBalloonHint.CreateHandle;
var
  vClassName: string;
begin
  if not IsWindow(FHintWindow) then  // 如果提示窗体没有创建
  begin
    vClassName := ClassName;
    FHintWindow := CreateWindowEx(
        WS_EX_TOPMOST or WS_EX_TOOLWINDOW{顶层窗口} or WS_EX_LAYERED,
        PChar(vClassName),
        nil,
        WS_POPUP or WS_DISABLED,  // 弹出式窗口,支持双击
        0, 0, 100, 20, 0, 0, HInstance, nil);

    SetLayeredWindowAttributes(FHintWindow, 0, 200, LWA_ALPHA);
    SetWindowLong(FHintWindow, GWL_WNDPROC, Longint(MakeObjectInstance(WndProc)));  // 窗口函数替换为类方法
  end;
end;

destructor TCFBalloonHint.Destroy;
var
  vThread: TThread;
begin
  if FMouseHook<> 0 then
    UnhookWindowsHookEx(FMouseHook);

  FreeAndNil(FTimer);
  //SendMessage(FPopupWindow, WM_DESTROY, 0, 0);
  if IsWindow(FHintWindow) then
  begin
    if DestroyWindow(FHintWindow) then
      FHintWindow := 0
    else
      raise Exception.Create('释放出错：' + IntToStr(GetLastError));
  end;

  FHW := FHintWindow;
  inherited Destroy;
end;

procedure TCFBalloonHint.DoDelayTimer(Sender: TObject);
begin
  //FreeAndNil(Self);  // 自己里面释放自己不行，是因为创建窗体和释放不在同一线程而导致无法释放吗？
  // 利用了WM_CLOSE消息里释放
  SendMessage(FHintWindow, WM_CLOSE, 0, 0);
end;

procedure TCFBalloonHint.DoPaintHint(const ADC: HDC);
var
  vRect: TRect;
  //vCanvas: TCanvas;
  vBrush: HBRUSH;
  vLogFont: TLogFont;
  vFont, vFontOld: HFONT;
begin
  GetClientRect(FHintWindow, vRect);
  {vCanvas := TCanvas.Create;
  try
    vCanvas.Handle := ADC;
    DrawText(ADC, FText, -1, vRect, DT_SINGLELINE or DT_CENTER);
  finally
    FreeAndNil(vCanvas);
  end;}

  vBrush := CreateSolidBrush(clRed);
  try
    FillRect(ADC, vRect, vBrush);
  finally
    DeleteObject(vBrush)
  end;

  IniFont(vLogFont);
  vFont := CreateFontIndirect(vLogFont);
  vFontOld := SelectObject(ADC, vFont);
  try
    //SetTextColor(ADC, clBlack);
    SetBkMode(ADC, TRANSPARENT); {透明模式}
    DrawText(ADC, FText, -1, vRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  finally
    DeleteObject(vFont);
    DeleteObject(vFontOld);
  end;
end;

function TCFBalloonHint.GetDelay: Cardinal;
begin
  Result := FTimer.Internal;
end;

procedure TCFBalloonHint.IniFont(var AFont: TLogFont);
begin
  AFont.lfHeight := -16;
  AFont.lfWidth := 0;
  AFont.lfEscapement := 0;
  AFont.lfWeight := 500;
  AFont.lfItalic := 0;
  AFont.lfUnderline := 0;
  AFont.lfStrikeOut := 0;
  AFont.lfFaceName := '宋体';
end;

procedure TCFBalloonHint.RegFormClass;
var
  vWndCls: TWndClassEx;
  vClassName: string;
begin
  vClassName := ClassName;
  if not GetClassInfoEx(HInstance, PChar(vClassName), vWndCls) then
  begin
    vWndCls.cbSize        := SizeOf(TWndClassEx);
    vWndCls.lpszClassName := PChar(vClassName);
    vWndCls.style         := CS_VREDRAW or CS_HREDRAW
      or CS_DROPSHADOW or CS_DBLCLKS;  // 通过此样式实现窗口边框阴影效果，只能在注册窗口类时使用此属性，注册后可通过SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) or CS_DROPSHADOW);再增加

    vWndCls.hInstance     := HInstance;
    vWndCls.lpfnWndProc   := @DefWindowProc;
    vWndCls.cbClsExtra    := 0;
    vWndCls.cbWndExtra    := SizeOf(DWord) * 2;
    vWndCls.hIcon         := LoadIcon(hInstance,MakeIntResource('MAINICON'));
    vWndCls.hIconSm       := LoadIcon(hInstance,MakeIntResource('MAINICON'));
    vWndCls.hCursor       := LoadCursor(0, IDC_ARROW);
    vWndCls.hbrBackground := GetStockObject(white_Brush);
    vWndCls.lpszMenuName  := nil;

    if RegisterClassEx(vWndCls) = 0 then
      raise Exception.Create('异常：注册类' + vClassName + '错误!');
  end;
end;

procedure TCFBalloonHint.SetDelay(const Value: Cardinal);
begin
  FTimer.Internal := Value;
end;

procedure TCFBalloonHint.SetText(const AText: string);
var
  vDC: HDC;
  vSize: TSize;
  vLogFont: TLogFont;
  vFont, vFontOld: HFONT;
  vRgn: HRGN;
begin
  if FText <> AText then
  begin
    FText := AText;
    vDC := CreateCompatibleDC(0);
    try
      IniFont(vLogFont);
      vFont := CreateFontIndirect(vLogFont);
      vFontOld := SelectObject(vDC, vFont);
      try
        Windows.GetTextExtentPoint32(vDC, FText, Length(FText), vSize);
      finally
        DeleteObject(vFont);
        DeleteObject(vFontOld);
      end;

      SetWindowPos(FHintWindow, 0, 0, 0, vSize.cx + 10, vSize.cy + 10, SWP_NOACTIVATE{无焦点} or SWP_NOZORDER);
      vRgn := CreateRoundRectRgn(0, 0, vSize.cx + 10, vSize.cy + 10, 5, 5);
      try
        SetWindowRgn(FHintWindow, vRgn, True);
      finally
        DeleteObject(vRgn);
      end;
    finally
      DeleteDC(vDC);
    end;
  end;
end;

{钩子函数, 鼠标消息太多(譬如鼠标移动), 必须要有选择, 这里选择了鼠标左键按下}
function MouseHook(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  vBound: TRect;
  vX, vY, vW, vH: Integer;
begin
  if wParam = WM_MOUSEMOVE then
  begin
    GetWindowRect(FHW, vBound);
    vW := vBound.Right - vBound.Left;
    vH := vBound.Bottom - vBound.Top;
    GetCursorPos(vBound.TopLeft);
    vX := vBound.Left + 20;
    vY := vBound.Top;
    MoveWindow(FHW, vX, vY, vW, vH, False);
  end;

  Result := CallNextHookEx(FMouseHook, nCode, wParam, lParam);
end;

procedure TCFBalloonHint.ShowHint;
var
  vBound: TRect;
  vX, vY, vW, vH: Integer;
  //vCursorInfo: TCursorInfo;
  //vIconInfo: TIconInfo;
  //vBITMPA: BITMAP;
begin
  GetWindowRect(FHintWindow, vBound);
  vW := vBound.Right - vBound.Left;
  vH := vBound.Bottom - vBound.Top;

  if FAlignment = cbaCenter then  // 屏幕居中
  begin
    SystemParametersInfo(SPI_GETWORKAREA, 0, vBound, 0);
    vX := ((vBound.Right - vBound.Left) - vW) div 2;
    vY := ((vBound.Bottom - vBound.Top) - vH) div 2;
  end
  else  // 鼠标跟随
  begin
    GetCursorPos(vBound.TopLeft);
    {vCursorInfo.cbSize := SizeOf(vCursorInfo);
    GetCursorInfo(vCursorInfo);
    GetIconInfo(vCursorInfo.hCursor, vIconInfo);
    GetObject(vIconInfo.hbmColor, sizeof(vBITMPA), @vBITMPA);}

    vX := vBound.Left + 20;  // + vBITMPA.bmWidth;
    vY := vBound.Top;  // + (vBITMPA.bmHeight - vH) div 2;
  end;
  //
  MoveWindow(FHintWindow, vX, vY, vW, vH, True);
  ShowWindow(FHintWindow, SW_SHOWNOACTIVATE);
  FTimer.Enable := True;
  FMouseHook := SetWindowsHookEx(WH_MOUSE, Addr(MouseHook), HInstance, GetCurrentThreadId)
end;

procedure TCFBalloonHint.WndProc(var Message: TMessage);
var
  vDC: HDC;
  vPS: PAINTSTRUCT;
begin
  case Message.Msg of
    WM_PAINT:
      begin
        vDC := BeginPaint(FHintWindow, vPS);
        try
          DoPaintHint(vDC);
        finally
          EndPaint(FHintWindow, vPS);
        end;
      end;

//    WM_ACTIVATE:
//      Message.Result := MA_NOACTIVATE;

    WM_MOUSEACTIVATE:
      Message.Result := MA_NOACTIVATE;

    WM_NCACTIVATE:
      Message.Result := 1;

    WM_CLOSE:
      begin
        FreeAndNil(Self);
        //Message.Result := DefWindowProc(FPopupWindow, Message.Msg, Message.WParam, Message.LParam);
      end
  else
    Message.Result := DefWindowProc(FHintWindow, Message.Msg, Message.WParam, Message.LParam);
  end;
end;

end.
