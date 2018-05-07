unit CFPopup;

interface

uses
  Windows, Classes, Controls, CFControl, Graphics, Messages;

type
  TPopupCloseEvent = procedure(Sender: TObject) of object;

  TCFCustomPopup = class(TComponent)
  private
    FPopupWindow: HWND;
    FAlignment: TAlignment;
    FOnPopupClose: TPopupCloseEvent;
    FRemoveMessageOnClose,  // 点击在非Popup窗体关闭时，是否移除消息(不传递到鼠标位置控件上)
    FOpened: Boolean;
  protected
    procedure RegFormClass;
    procedure CreateHandle;
    function GetWidth: Integer; virtual;
    procedure WndProc(var Message: TMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Popup(X, Y: Integer); overload; virtual;
    procedure Popup(APt: TPoint); overload; virtual;
    procedure Popup(const AControl: TControl); overload; virtual;
    procedure UpdatePopup; virtual;
    procedure ClosePopup(const ACancel: Boolean);

    property Width: Integer read GetWidth;
    property Opened: Boolean read FOpened;
    property OnPopupClose: TPopupCloseEvent read FOnPopupClose write FOnPopupClose;
  published
    property Alignment: TAlignment read FAlignment write FAlignment default taRightJustify;
    property RemoveMessageOnClose: Boolean read FRemoveMessageOnClose write FRemoveMessageOnClose;
  end;

  TDrawEvent = procedure(const ADC: HDC; const AClentRect: TRect) of object;

  TCFPopup = class(TCFCustomPopup)
  private
    FPopupControl: TCFCustomControl;
    FOnDrawWindow: TDrawEvent;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    procedure SetSize(const AWidth, AHeight: Integer);
    property PopupControl: TCFCustomControl read FPopupControl write FPopupControl;
    //
    property OnDrawWindow: TDrawEvent read FOnDrawWindow write FOnDrawWindow;
  end;

  TCFWinPopup = class(TCFCustomPopup)
  private
    FPopupControl: TWinControl;
    FPopupControlOldParent: THandle;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    procedure Popup(X, Y: Integer); override;
    property PopupControl: TWinControl read FPopupControl write FPopupControl;
  end;

implementation

uses
  Forms, SysUtils;

type
  TApplicationAccess = class(TApplication);

var
  ApplicationCallWndProcHook: HHOOK = 0;
  //OldWndProc: Pointer;

{ TCFCustomPopup }

{function WndProc(hWnd: HWND; Msg: Windows.UINT; WParam: WPARAM; LParam: LPARAM): LRESULT stdcall;
begin
  SetWindowLong(hwnd, GWL_WNDPROC, Longint(OldWndProc));
  Result := 1;
end;}

function ApplicationCallWndProcHookProc(Code: Integer;
  WParam, LParam: Longint): Longint stdcall;

  {procedure LockMessage(AWnd: HWND);
  begin
    OldWndProc := Pointer(GetWindowLong(AWnd, GWL_WNDPROC));
    SetWindowLong(AWnd, GWL_WNDPROC, Longint(@WndProc));
  end;}

  procedure RemoveHooks;
  begin
    if ApplicationCallWndProcHook <> 0 then
    begin
      UnhookWindowsHookEx(ApplicationCallWndProcHook);
      ApplicationCallWndProcHook := 0;
    end;
  end;

begin
// 暂时去掉Hook
//  if Windows.PCWPStruct(LParam)^.message = WM_ACTIVATEAPP then
//  begin
//    if Windows.PCWPStruct(LParam)^.wParam = 0 then
//    begin
//      SendMessage(FPopupWindow, WM_NCACTIVATE, 0, 0);  // 解决20160708001因PM_NOREMOVE造成弹出后从任务栏程序按钮右键关闭时不关闭，点击窗体才关闭的问题
//      RemoveHooks;
//    end;
//  end
//  {else
//  if Windows.PCWPStruct(LParam)^.message = CM_DEACTIVATE then
//  begin
//    LockMessage(Windows.PCWPStruct(LParam)^.hwnd);
//  end};
//
//  Result := CallNextHookEx(ApplicationCallWndProcHook, Code, WParam, LParam);
end;

procedure TCFCustomPopup.ClosePopup(const ACancel: Boolean);
begin
  // 先触发事件再关闭Popup，这样会感觉响应更快
  if (not ACancel) and Assigned(FOnPopupClose) then
    FOnPopupClose(Self);
  ShowWindow(FPopupWindow, SW_HIDE);
  FOpened := False;
end;

constructor TCFCustomPopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRemoveMessageOnClose := False;
  FPopupWindow := 0;
  FOpened := False;
  RegFormClass;
  CreateHandle;
end;

procedure TCFCustomPopup.CreateHandle;
var
  vClassName: string;
begin
  if not IsWindow(FPopupWindow) then  // 如果提示窗体没有创建
  begin
    vClassName := ClassName;
    FPopupWindow := CreateWindowEx(
        WS_EX_TOPMOST or WS_EX_TOOLWINDOW,  // 顶层窗口
        PChar(vClassName),
        nil,
        WS_POPUP,  // 弹出式窗口,支持双击
        0, 0, 100, 100, 0, 0, HInstance, nil);
    SetWindowLong(FPopupWindow, GWL_WNDPROC, Longint(MakeObjectInstance(WndProc)));  // 窗口函数替换为类方法
  end;
end;

destructor TCFCustomPopup.Destroy;
begin
  if IsWindow(FPopupWindow) then
    DestroyWindow(FPopupWindow);
  inherited;
end;

function TCFCustomPopup.GetWidth: Integer;
var
  vRect: TRect;
begin
  GetWindowRect(FPopupWindow, vRect);
  Result := vRect.Right - vRect.Left;
end;

procedure TCFCustomPopup.Popup(APt: TPoint);
begin
  Popup(APt.X, APt.Y);
end;

procedure TCFCustomPopup.Popup(const AControl: TControl);
var
  vRect: TRect;
  vW: Integer;
begin
  //GetWindowRect((AControl as TWinControl).Handle, vRect)
  vRect := AControl.BoundsRect;
  vW := vRect.Right - vRect.Left;

  if AControl is TCFCustomControl then  // 当自定义控件是另一自定义控件的内部嵌套控件时，转换相对窗体的坐标
  begin
    ClientToScreen((AControl as TCFCustomControl).GetUIHandle, vRect.TopLeft);
    ClientToScreen((AControl as TCFCustomControl).GetUIHandle, vRect.BottomRight);
  end
  else
  if AControl.Parent <> nil then
  begin
    ClientToScreen(AControl.Parent.Handle, vRect.TopLeft);
    ClientToScreen(AControl.Parent.Handle, vRect.BottomRight);
  end;
  case FAlignment of
    taLeftJustify:
      Popup(vRect.Left + 1, vRect.Bottom);
    taRightJustify:
      Popup(vRect.Right - Width - 1, vRect.Bottom);
    taCenter:
      begin
        vW := (Width - (vRect.Right - vRect.Left)) div 2;
        Popup(vRect.Left - vW, vRect.Bottom);
      end;
  end;
end;

procedure TCFCustomPopup.RegFormClass;
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
    begin
      //MessageBox(0, '注册TCustomPopup错误!', 'TCFCustomPopup', MB_OK);
      raise Exception.Create('异常：注册TCustomPopup错误!');
      Exit;
    end;
  end;
end;

procedure TCFCustomPopup.UpdatePopup;
var
  vRect: TRect;
begin
  if IsWindowVisible(FPopupWindow) then
  begin
    GetClientRect(FPopupWindow, vRect);
    InvalidateRect(FPopupWindow, vRect, False);
  end;
end;

procedure TCFCustomPopup.WndProc(var Message: TMessage);
begin
  //Message.Result := 0;

  case Message.Msg of
    //WM_SETCURSOR:
    //  StripSetCursor(AWnd, lParam);
    //WM_ERASEBKGND:  // 通知已经重画背景了
    //  Result := 1;
    //WM_CAPTURECHANGED:
    //  ShowWindow(AWnd, SW_HIDE);
         //WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP, WM_DESTROY:
    WM_MOUSEACTIVATE:
      Message.Result := MA_NOACTIVATE;
    WM_NCACTIVATE:
      begin
        FOpened := False;
        Message.Result := 1;
      end
  else
    Message.Result := DefWindowProc(FPopupWindow, Message.Msg, Message.WParam, Message.LParam);
  end;
end;

procedure TCFCustomPopup.Popup(X, Y: Integer);
var
  vMsg: TMsg;

  function IsFPopupWindow(Wnd: HWnd): Boolean;
  begin
    while (Wnd <> 0) and (Wnd <> FPopupWindow) do
      Wnd := GetParent(Wnd);
    Result := Wnd = FPopupWindow;
  end;

  {$REGION 'MessageLoop'}
  procedure MessageLoop;
  begin
    try
      repeat
        if not FOpened then Exit;

        if PeekMessage(vMsg, 0, 0, 0, PM_NOREMOVE) then  // 20160708001 以查看的方式从系统中获取消息，可以不将消息从系统中移除
        begin
          case vMsg.message of
            WM_NCLBUTTONDOWN, WM_NCLBUTTONDBLCLK, WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
            WM_NCRBUTTONDOWN, WM_NCRBUTTONDBLCLK, WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
            WM_NCMBUTTONDOWN, WM_NCMBUTTONDBLCLK, WM_MBUTTONDOWN, WM_MBUTTONDBLCLK:
              begin
                if not IsFPopupWindow(vMsg.hwnd) then
                begin
                  if FRemoveMessageOnClose then
                    PeekMessage(vMsg, 0, 0, 0, PM_REMOVE);  // 退出后移除当前窗体消息(点击Popup窗体外的按钮时关闭Popup窗体不执行按钮事件)防止仅为关闭Popup窗体而误操作
                  Break;
                end;
                //if vMsg.hwnd = FPopupWindow then  // 兼容 TCPopup中没有实际的Control置于FPopupWindow上的情况
                //  Break;
                //PeekMessage(vMsg, 0, vMsg.message, vMsg.message, PM_REMOVE);
                //SendMessage(FPopupWindow, vMsg.Message, vMsg.WParam, vMsg.LParam);
              end;

            {WM_LBUTTONUP:
              begin
                if IsFPopupWindow(vMsg.hwnd) then
                begin
                  //PeekMessage(vMsg, 0, 0, 0, PM_REMOVE);
                  //SendMessage(FPopupWindow, vMsg.Message, vMsg.WParam, vMsg.LParam);
                end;
              end;}

            WM_MOUSEWHEEL:  // 弹出后响应所有滚轮事件
              begin
                PeekMessage(vMsg, 0, 0, 0, PM_REMOVE);
                SendMessage(FPopupWindow, vMsg.Message, vMsg.WParam, vMsg.LParam);
                Continue;
              end;

            WM_KEYFIRST..WM_KEYLAST:
              begin
                PeekMessage(vMsg, 0, 0, 0, PM_REMOVE);
                SendMessage(FPopupWindow, vMsg.Message, vMsg.WParam, vMsg.LParam);
                Continue;
              end;

            //WM_C_KILLPOPUP: Exit;  // 外部发送关闭Popu消息

            WM_KILLFOCUS:
              Exit;

            CM_DEACTIVATE, WM_ACTIVATEAPP:
              Break;
          end;
            Application.HandleMessage;
        end
        else
          TApplicationAccess(Application).Idle(vMsg);
      until Application.Terminated;
    finally
      if FOpened then
        ClosePopup(True);
    end;
  end;
  {$ENDREGION}

  {$REGION '通过控件句柄获取控件实例，暂时未使用'}
  {
    ---------------通过控件句柄获取控件实例，暂时未使用--------------------------------
    ---------------原理详见 Classes.pas 单元，13045行 <Delphi7>------------------------
    ---------------原理详见 Classes.pas 单元，11613行 <Delphi2007>---------------------
    ---------------原理详见 Classes.pas 单元，13045行 <Delphi2010>---------------------
    ---------------原理详见 Classes.pas 单元，13512行 <DelphiXE>-----------------------
  }
  function GetInstanceFromhWnd(const hWnd: Cardinal): TWinControl;
  type
    PObjectInstance = ^TObjectInstance;

    TObjectInstance = packed record
      Code: Byte;            { 短跳转 $E8 }
      Offset: Integer;       { CalcJmpOffset(Instance, @Block^.Code); }
      Next: PObjectInstance; { MainWndProc 地址 }
      Self: Pointer;         { 控件对象地址 }
    end;
  var
    wc: PObjectInstance;
  begin
    Result := nil;
    wc := Pointer(GetWindowLong(hWnd, GWL_WNDPROC));
    if wc <> nil then
    begin
      Result := wc.Self;
    end;
  end;
  {$ENDREGION}

var
  vBound: TRect;
  vW, vH: Integer;
  vMonitor: TMonitor;
begin
  GetWindowRect(FPopupWindow, vBound);
  vW := vBound.Right - vBound.Left;
  vH := vBound.Bottom - vBound.Top;

  vMonitor := Screen.MonitorFromPoint(Point(X, Y));

  if vMonitor <> nil then
  begin
    if X + vW > vMonitor.WorkareaRect.Right then
      X := vMonitor.WorkareaRect.Right - vW;
    if Y + vH > vMonitor.WorkareaRect.Bottom then
      Y := vBound.Top - vH;

    if X < vMonitor.WorkareaRect.Left then
      X := vMonitor.WorkareaRect.Left;
    if Y < vMonitor.WorkareaRect.Top then
      Y := vMonitor.WorkareaRect.Top;
  end
  else // Monitor is nil, use Screen object instead
  begin
    if X + vW > Screen.WorkareaRect.Right then
      X := Screen.WorkareaRect.Right - vW;
    if Y + vH > Screen.WorkareaRect.Bottom then
      Y := vBound.Top - vH;

    if X < Screen.WorkareaRect.Left then
      X := Screen.WorkareaRect.Left;
    if Y < Screen.WorkareaRect.Top then
      Y := Screen.WorkareaRect.Top;
  end;
  //
  MoveWindow(FPopupWindow, X, Y, vW, vH, True);
  ShowWindow(FPopupWindow, SW_SHOWNOACTIVATE);  // SW_SHOWNOACTIVATE SW_SHOW
  FOpened := True;
  {暂时去掉Hook
  if FPopupWindow <> 0 then
    ApplicationCallWndProcHook := SetWindowsHookEx(WH_CALLWNDPROC, ApplicationCallWndProcHookProc, 0, GetCurrentThreadId);}
  MessageLoop;
end;

{ TCFWinPopup }

procedure TCFWinPopup.Popup(X, Y: Integer);
var
  vPopupControlBounds: TRect;
  vW, vH: Integer;
begin
  if FPopupControl is TWinControl then
  begin
    if FPopupControl.HandleAllocated then
      GetWindowRect(FPopupControl.Handle, vPopupControlBounds)
    else
      vPopupControlBounds := FPopupControl.BoundsRect;
  end;
  vW := vPopupControlBounds.Right - vPopupControlBounds.Left;
  vH := vPopupControlBounds.Bottom - vPopupControlBounds.Top;

  if FPopupControl.Parent <> nil then
  begin
    FPopupControlOldParent := GetParent(FPopupControl.Handle);
    SetWindowPos(FPopupControl.Handle, 0, 0, 0, vW, vH, SWP_NOZORDER);
  end
  else
    FPopupControlOldParent := 0;
  SetParent(FPopupControl.Handle, FPopupWindow);
  //
  MoveWindow(FPopupWindow, X, Y, vW, vH, True);
  inherited;
end;

procedure TCFWinPopup.WndProc(var Message: TMessage);
begin
  inherited;
//  if FPopupControl <> nil then
//    SendMessage(FPopupControl.Handle, Message.Msg, Message.WParam, Message.LParam);
end;

{ TCFPopup }

procedure TCFPopup.SetSize(const AWidth, AHeight: Integer);
var
  vRect: TRect;
begin
  if GetWindowRect(FPopupWindow, vRect) then
    MoveWindow(FPopupWindow, vRect.Left, vRect.Top, AWidth, AHeight, False);
end;

procedure TCFPopup.WndProc(var Message: TMessage);
var
  vFormDC, vMemDC: HDC;
  vMemBitmap, vOldBitmap: HBITMAP;
  ps: TPaintStruct;
  vClientRect: TRect;
begin
  if FPopupControl <> nil then
  begin
    case Message.Msg of
      WM_PAINT:
        begin
          if Assigned(FOnDrawWindow) then
          begin
            // 双缓冲绘图
            vFormDC := BeginPaint(FPopupWindow, ps);
            try
              GetClientRect(FPopupWindow, vClientRect);

              vMemBitmap := CreateCompatibleBitmap(vFormDC, ps.rcPaint.Right - ps.rcPaint.Left,
                ps.rcPaint.Bottom - ps.rcPaint.Top);
              try
                vMemDC := CreateCompatibleDC(vFormDC);
                vOldBitmap := SelectObject(vMemDC, vMemBitmap);
                try
                  FOnDrawWindow(vMemDC, vClientRect);
                  BitBlt(vFormDC, PS.rcPaint.Left, PS.rcPaint.Top,
                    PS.rcPaint.Right - PS.rcPaint.Left,
                    PS.rcPaint.Bottom - PS.rcPaint.Top,
                    vMemDC,
                    PS.rcPaint.Left, PS.rcPaint.Top,
                    SRCCOPY);
                finally
                  SelectObject(vMemDC, vOldBitmap);
                end;
              finally
                DeleteDC(vMemDC);
                DeleteObject(vMemBitmap);
              end;
            finally
              EndPaint(FPopupWindow, ps);
            end;
          end;
        end;

      WM_LBUTTONDOWN:
        begin
          // 如果直接Perform消息WM_LBUTTONDOWN会造成系统认为现在点击在
          // FPopupControl(DirectUI时为FPopupControl所在窗体)上，导致
          // Popup消息循环中触发的WM_LBUTTONUP是由FPopupControl(或所在的窗体)触发，
          // 进而不能触发此处的WM_LBUTTONDOWN事件
          FPopupControl.Perform(WM_C_LBUTTONDOWN, Message.WParam, Message.LParam);
        end;

      WM_ACTIVATEAPP:
        begin
          if FOpened and (Message.WParam = 0) then  // 用FOpened处理窗体关闭时再次执行此处CloseUp的问题
            ClosePopup(True);
        end;

      WM_LBUTTONDBLCLK:
        FPopupControl.Perform(WM_C_LBUTTONDBLCLK, Message.WParam, Message.LParam);

      WM_LBUTTONUP:
        FPopupControl.Perform(WM_C_LBUTTONUP, Message.WParam, Message.LParam);

      WM_MOUSEWHEEL:
        FPopupControl.Perform(Message.Msg, Message.WParam, Message.LParam);

      WM_MOUSEMOVE:
        FPopupControl.Perform(WM_C_MOUSEMOVE, Message.WParam, Message.LParam);
    end;
  end;
  inherited;
end;

end.
