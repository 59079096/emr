unit CFPopupForm;

interface

uses
  Windows, Classes, Controls, Graphics, Messages;

type
  TPopupCloseEvent = procedure(Sender: TObject) of object;

  TCustomPopup = class(TComponent)
  private
    FPopupWindow: HWND;
    FAlignment: TAlignment;
    FOnPopupClose: TPopupCloseEvent;
    //FClosePopup,  // 外部关闭Popup
    FOpened: Boolean;
  protected
    function IsPopupWindow(const AWnd: HWnd): Boolean;
    procedure RegFormClass;
    procedure CreateHandle;
    function StopPeekMessage(const AMsg: TMsg): Boolean; virtual;
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
  end;

  TDrawEvent = procedure(const ADC: HDC; const AClentRect: TRect) of object;

  TCFPopupForm = class(TCustomPopup)
  private
    FPopupControl: TWinControl;
    FOldParent: TWinControl;
    procedure SetPopupControl(const Value: TWinControl);
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    procedure Popup(X, Y: Integer); override;
    property PopupControl: TWinControl read FPopupControl write SetPopupControl;
  end;

  TCFHintPopupForm = class(TCFPopupForm)
  protected
    function StopPeekMessage(const AMsg: TMsg): Boolean; override;
  end;

implementation

uses
  Forms, SysUtils;

type
  TApplicationAccess = class(TApplication);

var
  ApplicationCallWndProcHook: HHOOK = 0;
  //OldWndProc: Pointer;

{ TCustomPopup }

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

procedure TCustomPopup.ClosePopup(const ACancel: Boolean);
begin
  // 先触发事件再关闭Popup，这样会感觉响应更快
  if (not ACancel) and Assigned(FOnPopupClose) then
    FOnPopupClose(Self);
  ShowWindow(FPopupWindow, SW_HIDE);
  FOpened := False;
end;

constructor TCustomPopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopupWindow := 0;
  FOpened := False;
  RegFormClass;
  CreateHandle;
end;

procedure TCustomPopup.CreateHandle;
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

destructor TCustomPopup.Destroy;
begin
  if IsWindow(FPopupWindow) then
    DestroyWindow(FPopupWindow);
  inherited;
end;

function TCustomPopup.StopPeekMessage(const AMsg: TMsg): Boolean;
begin
  Result := False;
end;

function TCustomPopup.GetWidth: Integer;
var
  vRect: TRect;
begin
  GetWindowRect(FPopupWindow, vRect);
  Result := vRect.Right - vRect.Left;
end;

function TCustomPopup.IsPopupWindow(const AWnd: HWnd): Boolean;
var
  vWnd: HWND;
begin
  Result := False;

  vWnd := AWnd;
  while (vWnd <> 0) and (vWnd <> FPopupWindow) do
    vWnd := GetParent(vWnd);

  Result := vWnd = FPopupWindow;

  if not Result then  // 没找到
  begin
    vWnd := FPopupWindow;
    while (vWnd <> 0) and (vWnd <> AWnd) do  // 在popup窗体顶层
      vWnd := GetNextWindow(vWnd, GW_HWNDPREV);

    Result := vWnd = AWnd;
  end;
end;

procedure TCustomPopup.Popup(APt: TPoint);
begin
  Popup(APt.X, APt.Y);
end;

procedure TCustomPopup.Popup(const AControl: TControl);
var
  vRect: TRect;
  vW: Integer;
begin
  //GetWindowRect((AControl as TWinControl).Handle, vRect)
  vRect := AControl.BoundsRect;
  vW := vRect.Right - vRect.Left;

  if AControl.Parent <> nil then
  begin
    ClientToScreen(AControl.Parent.Handle, vRect.TopLeft);
    ClientToScreen(AControl.Parent.Handle, vRect.BottomRight);
  end;

  case FAlignment of
    taLeftJustify:
      Popup(vRect.Left, vRect.Bottom);

    taRightJustify:
      Popup(vRect.Right - Width, vRect.Bottom);

    taCenter:
      begin
        vW := (Width - (vRect.Right - vRect.Left)) div 2;
        Popup(vRect.Left - vW, vRect.Bottom);
      end;
  end;
end;

procedure TCustomPopup.RegFormClass;
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

procedure TCustomPopup.UpdatePopup;
var
  vRect: TRect;
begin
  if IsWindowVisible(FPopupWindow) then
  begin
    GetClientRect(FPopupWindow, vRect);
    InvalidateRect(FPopupWindow, vRect, False);
  end;
end;

procedure TCustomPopup.WndProc(var Message: TMessage);
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
        //FOpened := False;
        Message.Result := 1;
      end
  else
    Message.Result := DefWindowProc(FPopupWindow, Message.Msg, Message.WParam, Message.LParam);
  end;
end;

procedure TCustomPopup.Popup(X, Y: Integer);
var
  vMsg: TMsg;

  {$REGION 'MessageLoop'}
  procedure MessageLoop;
  begin
    try
      repeat
        if not FOpened then Exit;

        if PeekMessage(vMsg, 0, 0, 0, PM_NOREMOVE) then  // 20160708001 以查看的方式从系统中获取消息，可以不将消息从系统中移除
        begin
          if StopPeekMessage(vMsg) then
          begin
            PeekMessage(vMsg, 0, 0, 0, PM_REMOVE);  // 退出后移除当前窗体消息(点击Popup窗体外的按钮时关闭Popup窗体不执行按钮事件)防止仅为关闭Popup窗体而误操作
            Break;
          end;

          case vMsg.message of
            WM_NCLBUTTONDOWN, WM_NCLBUTTONDBLCLK, WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
            WM_NCRBUTTONDOWN, WM_NCRBUTTONDBLCLK, WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
            WM_NCMBUTTONDOWN, WM_NCMBUTTONDBLCLK, WM_MBUTTONDOWN, WM_MBUTTONDBLCLK:
              begin
                if not IsPopupWindow(vMsg.hwnd) then
                begin
                  PeekMessage(vMsg, 0, 0, 0, PM_REMOVE);  // 退出后移除当前窗体消息(点击Popup窗体外的按钮时关闭Popup窗体不执行按钮事件)防止仅为关闭Popup窗体而误操作
                  SendMessage(vMsg.hwnd, vMsg.Message, vMsg.WParam, vMsg.LParam);
                  Break;
                end;
                //if vMsg.hwnd = FPopupWindow then  // 兼容 TCPopup中没有实际的Control置于FPopupWindow上的情况
                //  Break;
                //PeekMessage(vMsg, 0, vMsg.message, vMsg.message, PM_REMOVE);
                //SendMessage(FPopupWindow, vMsg.Message, vMsg.WParam, vMsg.LParam);
              end;

            WM_MOUSEMOVE:  // 防止触发PopupForm范围外控件的事件
              begin
                if not IsPopupWindow(vMsg.hwnd) then
                begin
                  PeekMessage(vMsg, 0, 0, 0, PM_REMOVE);
                  Continue;
                end;
              end;

            {WM_LBUTTONUP:
              begin
                if IsFPopupWindow(vMsg.hwnd) then
                begin
                  //PeekMessage(vMsg, 0, 0, 0, PM_REMOVE);
                  //SendMessage(FPopupWindow, vMsg.Message, vMsg.WParam, vMsg.LParam);
                end;
              end;}

            {WM_MOUSEWHEEL:  // 弹出后响应所有滚轮事件
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
              end;}

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
  Application.ProcessMessages;  // StopPeekMessage支持鼠标移出时自动消息，需要先把弹出时原控件的消息处理完
  FOpened := True;
  {暂时去掉Hook
  if FPopupWindow <> 0 then
    ApplicationCallWndProcHook := SetWindowsHookEx(WH_CALLWNDPROC, ApplicationCallWndProcHookProc, 0, GetCurrentThreadId);}
  MessageLoop;
end;

{ TCFPopupForm }

procedure TCFPopupForm.Popup(X, Y: Integer);
var
  vPopupControlBounds: TRect;
  vW, vH: Integer;
begin
  if not Assigned(FPopupControl) then Exit;

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
    FOldParent := FPopupControl.Parent;
    SetWindowPos(FPopupControl.Handle, 0, 0, 0, vW, vH, SWP_NOZORDER);
  end
  else
    FOldParent := nil;
    
  SetParent(FPopupControl.Handle, FPopupWindow);

  if not FPopupControl.Visible then
    FPopupControl.Show;
  //
  MoveWindow(FPopupWindow, X, Y, vW, vH, True);
  inherited Popup(X, Y);
end;

procedure TCFPopupForm.SetPopupControl(const Value: TWinControl);
begin
  if FPopupControl <> Value then
  begin
    if (not (csDesigning in ComponentState)) and Assigned(FOldParent) then
    begin
      FPopupControl.Parent := FOldParent;
      FOldParent := nil;
    end;

    FPopupControl := Value;
    FPopupControl.Visible := False;
  end;
end;

procedure TCFPopupForm.WndProc(var Message: TMessage);
begin
  inherited;
end;

{ TCFHintPopupForm }

function TCFHintPopupForm.StopPeekMessage(const AMsg: TMsg): Boolean;
begin
  Result := inherited StopPeekMessage(AMsg);

  if AMsg.message = WM_MOUSEMOVE then
  begin
    if not IsPopupWindow(AMsg.hwnd) then
      Result := True;
  end;
end;

end.
