unit CFForm;

interface

uses
  Windows, Classes, Forms, Types, Controls, CFControl, Messages;

type
  TJTForm = class(TForm)
  private
    FFocusControl: TCFCustomControl;
    // 定时器相关
    FPCallBackProc: Pointer;
    FMMTimerID: Cardinal;
    FInterval: Word;  // 动画间隔时间(毫秒)

    procedure DriectPaint;
    procedure PaintWindow(ADC: HDC); override;
    procedure PaintControlsEx(ADC: HDC; AFirst: TCFCustomControl);

    function FindNextCControl(CurControl: TCFCustomControl;
      GoForward, CheckTabStop, CheckParent: Boolean): TCFCustomControl;
    procedure SelectNextCControl(ACurControl: TCFCustomControl;
      AGoForward, ACheckTabStop: Boolean);
    // 定时器相关
    procedure CreateTimeSet;
    procedure KillTimeSet;
    procedure ProtimeCallBack(uTimerID, uMessage: UINT; dwUser, dw1, dw2: DWORD) stdcall;
  protected
    function GetTimerActive: Boolean;
    procedure SetInterval(Value: Word);
    procedure SetTimerActive(const Value: Boolean);
    procedure SetFocusControl(Value: TCFCustomControl);
    procedure MouseWheelHandler(var Message: TMessage); override;
    //procedure AlignControls(AControl: TControl; var Rect: TRect); override;  // 只能在运行时有效
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY; // 处理Tab键切换焦点控件
    procedure CMRemoveControl(var Message: TMessage); message WM_C_REMOVECONTROL;  // 控件变更
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // 当KeyPreview为true时供TextControl调用
    function DoKeyDown(var Message: TWMKey): Boolean;
    function DoKeyPress(var Message: TWMKey): Boolean;
    function DoKeyUp(var Message: TWMKey): Boolean;
    //
    function FindCControl(const ControlName: string): TCFCustomControl;
    property FocusControl: TCFCustomControl read FFocusControl write SetFocusControl;
  published
    property Interval: Word read FInterval write SetInterval;
    property TimerActive: Boolean read GetTimerActive write SetTimerActive;
  end;

implementation

uses
  MMSystem, CFCallBackToMethod;

{ TJTForm }

{procedure TJTForm.AlignControls(AControl: TControl; var Rect: TRect);
begin
  if AControl is TCFCustomControl then
  begin
    if (AControl as TCFCustomControl).CParent <> nil then
      Exit;
  end;
  inherited;
end;}

procedure TJTForm.CMRemoveControl(var Message: TMessage);
begin
  if Message.LParam = 0 then  // 移除
  begin
    if Integer(FFocusControl) = message.WParam then
      FocusControl := nil;
  end;
end;

procedure TJTForm.CMDialogKey(var Message: TCMDialogKey);
begin
  if GetKeyState(VK_MENU) >= 0 then
  begin
    with Message do
      case CharCode of
        VK_TAB:
          if GetKeyState(VK_CONTROL) >= 0 then
          begin
            SelectNextCControl(FFocusControl, GetKeyState(VK_SHIFT) >= 0, True);
            Result := 1;
            Exit;
          end;
        VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN:
          begin
            if FFocusControl <> nil then
            begin
              if not FFocusControl.TabStop then
              begin
                TJTForm(FFocusControl.Parent).SelectNextCControl(FFocusControl,
                  (CharCode = VK_RIGHT) or (CharCode = VK_DOWN), False);
                Result := 1;
                Exit;
              end;
            end;
          end;
      end;
  end;
  inherited;
end;

constructor TJTForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterval := 100;
  if not (csDesigning in ComponentState) then
  begin
    Color := GThemeColor;
    Position := poScreenCenter;
  end;
end;

procedure TJTForm.CreateTimeSet;
var
  vMethod: TMethod;
begin
  KillTimeSet;

  if FPCallBackProc = nil then
  begin
    vMethod.Code := @TJTForm.ProtimeCallBack;
    vMethod.Data := Self;
    FPCallBackProc := MakeInstruction(vMethod);
  end;

  FMMTimerID := timeSetEvent(
      FInterval, // 以毫秒指定事件的周期
      FInterval, // 以毫秒指定延时的精度，数值越小定时器事件分辨率越高。缺省值为1ms。
      FPCallBackProc, // 回调函数
      0, // 存放用户提供的回调数据
      //定时器事件类型
      TIME_PERIODIC  // 每隔uDelay毫秒周期性地产生事件
      or TIME_CALLBACK_FUNCTION);
end;

destructor TJTForm.Destroy;
begin
  KillTimeSet;
  if FPCallBackProc <> nil then
    FreeInstruction(FPCallBackProc);
  inherited;
end;

function TJTForm.DoKeyDown(var Message: TWMKey): Boolean;
begin
  Result := inherited DoKeyDown(Message);
end;

function TJTForm.DoKeyPress(var Message: TWMKey): Boolean;
begin
  Result := inherited DoKeyPress(Message);
end;

function TJTForm.DoKeyUp(var Message: TWMKey): Boolean;
begin
  Result := inherited DoKeyUp(Message);
end;

procedure TJTForm.DriectPaint;
var
  vFormDC, vMemDC: HDC;
  vMemBitmap, vOldBitmap: HBITMAP;
  PS: TPaintStruct;
  i, vSaveIndex, vClip: Integer;
begin
  vFormDC := BeginPaint(Handle, PS);
  try
    vMemBitmap := CreateCompatibleBitmap(vFormDC, PS.rcPaint.Right - PS.rcPaint.Left,
      PS.rcPaint.Bottom - PS.rcPaint.Top);
    try
      vMemDC := CreateCompatibleDC(vFormDC);
      vOldBitmap := SelectObject(vMemDC, vMemBitmap);
      try
        SetWindowOrgEx(vMemDC, PS.rcPaint.Left, PS.rcPaint.Top, nil); // 恢复上一次绘制过程对视口原点的修改
        //Perform(WM_ERASEBKGND, vMemDC, vMemDC);
        if ControlCount = 0 then
          PaintWindow(vMemDC)
        else
        begin
          vSaveIndex := SaveDC(vMemDC);
          try
            vClip := SimpleRegion;  // 剪切区域是单个矩形
            for i := 0 to ControlCount - 1 do
            begin
              with TControl(Controls[i]) do
              begin
                if (Visible
                    and (
                          not (csDesigning in ComponentState)
                          or not (csDesignerHide in ControlState)
                        )
                    or (
                         (csDesigning in ComponentState)
                         and not (csDesignerHide in ControlState)
                       )
                    and not (csNoDesignVisible in ControlStyle))
                    and (csOpaque in ControlStyle)
                then
                begin
                  // 当前剪切区域减去一个特定的矩形区域(保留需要绘制的部分)
                  vClip := ExcludeClipRect(vMemDC, Left, Top, Left + Width, Top + Height);
                  if vClip = NullRegion then  // 剪切区域为空
                    Break;
                end;
              end;
            end;
            if vClip <> NullRegion then
              PaintWindow(vMemDC);
          finally
            RestoreDC(vMemDC, vSaveIndex);
          end;
        end;
        PaintControlsEx(vMemDC, nil);

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
    EndPaint(Handle, PS);
  end;
end;

function TJTForm.FindCControl(const ControlName: string): TCFCustomControl;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Self.ControlCount - 1 do
  begin
    if Self.Controls[i] is TCFCustomControl then
    begin
      if Self.Controls[i].Name = ControlName then
      begin
        Result := Self.Controls[i] as TCFCustomControl;
        Break;
      end;
    end;
  end;
end;

function TJTForm.FindNextCControl(CurControl: TCFCustomControl; GoForward,
  CheckTabStop, CheckParent: Boolean): TCFCustomControl;
var
  i, StartIndex, vCount: Integer;
  //List: TList;
begin
  Result := nil;
  StartIndex := -1;
  vCount := ControlCount;
  if vCount = 0 then Exit;

  for i := 0 to vCount - 1 do
  begin
    if Controls[i] = CurControl then
    begin
      StartIndex := i;
      Break;
    end;
  end;
  if StartIndex = -1 then
  begin
    if GoForward then
      StartIndex := vCount - 1
    else
      StartIndex := 0;
  end;

  i := StartIndex;
  repeat
    if GoForward then
    begin
      Inc(i);
      if i = vCount then
        i := 0;
    end
    else
    begin
      if i = 0 then
        i := vCount;
      Dec(i);
    end;
    if Controls[i] is TCFCustomControl then
    begin
      CurControl := Controls[i] as TCFCustomControl;
      if CurControl.CanFocus and
        (not CheckTabStop or CurControl.TabStop) and
        (not CheckParent or (CurControl.Parent = Self))
      then
        Result := CurControl;
    end;
  until (Result <> nil) or (I = StartIndex);

  {List := TList.Create;
  try
    GetTabOrderList(List);
    if List.Count > 0 then
    begin
      StartIndex := List.IndexOf(CurControl);
      if StartIndex = -1 then
        if GoForward then StartIndex := List.Count - 1 else StartIndex := 0;
      I := StartIndex;
      repeat
        if GoForward then
        begin
          Inc(I);
          if I = List.Count then I := 0;
        end else
        begin
          if I = 0 then I := List.Count;
          Dec(I);
        end;
        CurControl := TWinControl(List[I]);
        if CurControl.CanFocus and
          (not CheckTabStop or CurControl.TabStop) and
          (not CheckParent or (CurControl.Parent = Self)) then
          Result := CurControl;
      until (Result <> nil) or (I = StartIndex);
    end;
  finally
    List.Free;
  end;}
end;

function TJTForm.GetTimerActive: Boolean;
begin
  Result := FMMTimerID > 0;
end;

procedure TJTForm.KillTimeSet;
begin
  if FMMTimerID > 0 then
    timeKillEvent(FMMTimerID); //销毁计时器线程，停止计数
end;

procedure TJTForm.MouseWheelHandler(var Message: TMessage);
begin
  if FFocusControl <> nil then
    Message.Result := FFocusControl.Perform(CM_MOUSEWHEEL, Message.WParam, Message.LParam)
  else
    inherited MouseWheelHandler(Message);
end;

procedure TJTForm.PaintControlsEx(ADC: HDC; AFirst: TCFCustomControl);
var
  i, vCount, vSaveIndex: Integer;
  //FrameBrush: HBRUSH;
begin
  if DockSite and UseDockManager and (DockManager <> nil) then
    DockManager.PaintSite(ADC);  // ?
  vCount := ControlCount;
  if vCount > 0 then
  begin
    i := 0;
    if AFirst <> nil then
    begin
      for i := 0 to vCount - 1 do
      begin
        if Controls[i] = AFirst then
          Break;
      end;
    end;
    while i < vCount do
    begin
      with TControl(Controls[i]) do
      begin
        if ( Visible
             and ( not (csDesigning in ComponentState)
                   or not (csDesignerHide in ControlState)
                 )
             or ( (csDesigning in ComponentState)
                  and not (csDesignerHide in ControlState)
                )
             and not (csNoDesignVisible in ControlStyle)
           )
           //and ( (Controls[i] is TCFCustomControl) and ((Controls[i] as TCFCustomControl).CParent = nil) )  // 无父控件
           and RectVisible(ADC, Types.Rect(Left, Top, Left + Width, Top + Height))  // 指定矩形的任何部分是否在设备上下文环境的剪切区域之内
        then
        begin
          if csPaintCopy in Self.ControlState then
            ControlState := ControlState + [csPaintCopy];
          vSaveIndex := SaveDC(ADC);  // 保存(设备上下文环境)现场
          try
            MoveWindowOrg(ADC, Left, Top);
            IntersectClipRect(ADC, 0, 0, Width, Height);  // 创建新的剪切区域，该区域是当前剪切区域和一个特定矩形的交集
            Perform(WM_PAINT, ADC, 0);  // 不再支持TControl
            //TCFCustomControl(Controls[i]).DrawTo(ADC); 必需使用WM_PAINT触发绘制，否则没有TGraphicControl.WMPaint中将Canvas.Handle := 0的操作，各控件使用Canvas时不正确
          finally
            RestoreDC(ADC, vSaveIndex);  // 恢复(设备上下文环境)现场
          end;
          ControlState := ControlState - [csPaintCopy];
        end;
      end;
      Inc(i);
    end;
  end;
  {if WinControls <> nil then
    for I := 0 to FWinControls.Count - 1 do
      with TWinControl(FWinControls[I]) do
        if FCtl3D and (csFramed in ControlStyle) and
          (((not (csDesigning in ComponentState)) and Visible) or
          ((csDesigning in ComponentState) and
          not (csNoDesignVisible in ControlStyle) and not (csDesignerHide in ControlState))) then
        begin
          FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
          FrameRect(DC, Types.Rect(Left - 1, Top - 1,
            Left + Width, Top + Height), FrameBrush);
          DeleteObject(FrameBrush);
          FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
          FrameRect(DC, Types.Rect(Left, Top, Left + Width + 1,
            Top + Height + 1), FrameBrush);
          DeleteObject(FrameBrush);
        end;}
end;

procedure TJTForm.PaintWindow(ADC: HDC);
var
  vClientRect: TRect;
  vSaveIndex: Integer;
begin
  Canvas.Lock;
  try
    Canvas.Handle := ADC;
    try
      vSaveIndex := SaveDC(ADC);
      try
        vClientRect := ClientRect;
        SetBkColor(Canvas.Handle, GThemeColor);
        Canvas.FillRect(vClientRect);
      finally
        RestoreDC(ADC, vSaveIndex);
      end;
      if Designer <> nil then
        Designer.PaintGrid
      else
        Paint;
    finally
      Canvas.Handle := 0;
    end;
  finally
    Canvas.Unlock;
  end;
end;

procedure TJTForm.ProtimeCallBack(uTimerID, uMessage: UINT; dwUser, dw1,
  dw2: DWORD);
var
  i: Integer;
begin
  for i := 0 to ControlCount - 1 do
  begin
    if Controls[i] is TCFCustomControl then
      (Controls[i] as TCFCustomControl).TimerProc(FInterval);
  end;
end;

procedure TJTForm.SelectNextCControl(ACurControl: TCFCustomControl; AGoForward,
  ACheckTabStop: Boolean);
var
  vNextCControl: TCFCustomControl;
begin
  vNextCControl := FindNextCControl(ACurControl, AGoForward,
    ACheckTabStop, not ACheckTabStop);
  if vNextCControl <> nil then
  begin
    if FFocusControl <> nil then
      FFocusControl.Perform(CM_UIDEACTIVATE, 0, 0);  // 通过Tab键移除焦点

    FFocusControl := vNextCControl;
    FFocusControl.Perform(CM_UIACTIVATE, 0, 0);  // 通过Tab键移入焦点
  end;
end;

procedure TJTForm.SetFocusControl(Value: TCFCustomControl);
begin
  if FFocusControl <> Value then
  begin
    if FFocusControl <> nil then
      FFocusControl.Focus := False;

    FFocusControl := Value;
    if FFocusControl <> nil then
      FFocusControl.Focus := True;
  end;
end;

procedure TJTForm.SetInterval(Value: Word);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    CreateTimeSet;
  end;
end;

procedure TJTForm.SetTimerActive(const Value: Boolean);
begin
  if Value then
    CreateTimeSet  // 创建定时器
  else
    KillTimeSet;
end;

procedure TJTForm.WndProc(var Message: TMessage);
var
  vControl: TControl;
begin
  case Message.Msg of
    WM_LBUTTONDOWN,
    WM_MBUTTONDOWN,
    WM_RBUTTONDOWN:
      begin
        // ControlAtPos(SmallPointToPoint(Message.Pos), False)
        vControl := ControlAtPos(Point(Message.LParamLo, Message.LParamHi), False{, True, False});
        if vControl is TCFCustomControl then
          FocusControl := vControl as TCFCustomControl
        else  // 鼠标下无焦点控件时，取消已经有焦点的控件
          SetFocusControl(nil);
      end;

    WM_ACTIVATE:  // 发给被激活或失去激活的那个窗口
    begin
      if Message.WParam = WA_INACTIVE then  // 失去焦点(另WA_CLICKACTIVE：通过鼠标单击激活了该窗口 WA_ACTIVE：通过鼠标以外的工具（如键盘）激活了该窗口)
        SetFocusControl(nil);
    end;
    //WM_ACTIVATEAPP  // 会发给被激活或失去激活的应用程序所拥有的所有窗口，比如所有的按钮，所有的EDITBOX，等等而

    WM_KEYDOWN:
      begin
        if FFocusControl is TCFCustomControl then
          FFocusControl.Perform(Message.Msg, Message.WParam, Message.LParam);
      end;

    WM_CHAR:
      begin
        if FFocusControl is TCFCustomControl then
          FFocusControl.Perform(Message.Msg, Message.WParam, Message.LParam);
      end;

    WM_ERASEBKGND:  // Result to 1 to indicate background is painted by the control
      begin
        Message.Result := 1;
        Exit;
      end;

    WM_PAINT:
      begin
        DriectPaint;
        Message.Result := 1;
        Exit;
      end;
  end;

  inherited;  // 不能放最开始，为处理CCombobx点击按钮弹出下拉不受消息钩子和消息循环的影响仍能触发WM_LBUTTONDOWN以记录当前焦点CControl

  if Message.Msg = WM_LBUTTONDOWN then  // 左键在空白处拖动，移动窗口
  begin
    if FFocusControl = nil then
    begin
      ReleaseCapture;
      SendMessage(Handle, WM_SYSCOMMAND, $F011, 0);  {参数3在 $F011-$F01F 之间均可都是移动控件}
    end;
  end;
end;

end.
