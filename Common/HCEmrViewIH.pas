unit HCEmrViewIH;

interface

{$I HCEmrView.inc}

uses
  Windows, Classes, SysUtils, Messages, Imm, {$IFDEF VIEWTOOL}HCViewTool{$ELSE}HCView{$ENDIF},
  HCCallBackMethod, frm_InputHelper, HCCustomData, HCItem, HCStyle, HCRectItem;

type
  THCEmrViewIH = class({$IFDEF VIEWTOOL}THCViewTool{$ELSE}THCView{$ENDIF})
  private
    FInputHelper: TfrmInputHelper;
    {$IFDEF GLOBALSHORTKEY}  // 全局alt + space显示辅助输入窗体
    FPProc: Pointer;
    /// <summary> 低级键盘钩子的回调函数，在里面过滤消息 </summary>
    /// <param name="nCode">Hook的标志</param>
    /// <param name="wParam">表示消息的类型</param>
    /// <param name="lParam">指向KBDLLHOOKSTRUCT结构的指针</param>
    /// <returns>如果不为0，Windows丢掉这个消息程序不会再收到这个消息</returns>
    function KeyboardProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
    procedure SetIHKeyHook;  // 设置低级键盘钩子
    procedure UnSetIHKeyHook;  // 卸载低级键盘钩子
    {$ENDIF}

    function GetInputHelpEnable: Boolean;
    procedure SetInputHelpEnable(const Value: Boolean);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    /// <summary> 是否上屏输入法输入的词条屏词条ID和词条 </summary>
    function DoProcessIMECandi(const ACandi: string): Boolean; virtual;
    procedure WMImeNotify(var Message: TMessage); message WM_IME_NOTIFY;
    procedure UpdateImeComposition(const ALParam: Integer); override;
    procedure UpdateImePosition; override;  // IME 通知输入法更新位置
    /// <summary> 光标移动后取光标前后文本 </summary>
    procedure DoCaretChange; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PreProcessMessage(var Msg: TMsg): Boolean; override;
    property InputHelpEnable: Boolean read GetInputHelpEnable write SetInputHelpEnable;
  end;

implementation

const
  CARETSTOPCHAR = '，,。;；：:';
{$IFDEF GLOBALSHORTKEY}
  WH_KEYBOARD_LL = 13;  // //低级键盘钩子的索引值
  LLKHF_ALTDOWN = $20;

type
  tagKBDLLHOOKSTRUCT = packed record  // 在Windows NT 4 sp3以上系统中才能使用
    vkCode: DWORD;//虚拟键值
    scanCode: DWORD;//扫描码值（没有用过，我也不懂^_^）
    {一些扩展标志，这个标志值的第六位数（二进制）为1时ALT键按下为0相反。}
    flags: DWORD;
    time: DWORD;//消息时间戳
    dwExtraInfo: DWORD;//和消息相关的扩展信息
  end;
  KBDLLHOOKSTRUCT = tagKBDLLHOOKSTRUCT;
  PKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT;

var
  HHKLowLevelKybd: HHOOK;
{$ENDIF}

{ THCEmrViewIH }

constructor THCEmrViewIH.Create(AOwner: TComponent);
{$IFDEF GLOBALSHORTKEY}
var
  vM: TMethod;
{$ENDIF}
begin
  inherited Create(AOwner);

  {$IFDEF GLOBALSHORTKEY}
  vM.Code := @THCEmrViewIH.KeyboardProc;
  vM.Data := Self;
  FPProc := HCMakeInstruction(vM);
  SetIHKeyHook;  // 调试时可关掉提升效率
  {$ENDIF}

  FInputHelper := TfrmInputHelper.Create(nil);
end;

destructor THCEmrViewIH.Destroy;
begin
  FreeAndNil(FInputHelper);
  {$IFDEF GLOBALSHORTKEY}
  UnSetIHKeyHook;  // 调试时可关掉提升效率
  HCFreeInstruction(FPProc);
  {$ENDIF}
  inherited Destroy;
end;

procedure THCEmrViewIH.DoCaretChange;

  {$REGION '取光标前后字符串'}
  function GetCharBefor(const AOffset: Integer; var AChars: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := AOffset downto 1 do
    begin
      if Pos(AChars[i], CARETSTOPCHAR) > 0 then
      begin
        AChars := System.Copy(AChars, i + 1, AOffset - i);
        Result := True;
        Break;
      end;
    end;
  end;

  procedure GetBeforString(const AData: THCCustomData; const AStartItemNo: Integer; var ABefor: string);
  var
    i: Integer;
    vText: string;
  begin
    for i := AStartItemNo - 1 downto 0 do  // 向前
    begin
      vText := AData.Items[i].Text;
      if (vText <> '') and GetCharBefor(Length(vText), vText) then
      begin
        ABefor := vText + ABefor;
        Break;
      end
      else
        ABefor := vText + ABefor;
    end;
  end;

  function GetCharAfter(const AOffset: Integer; var AChars: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := AOffset to Length(AChars) do
    begin
      if Pos(AChars[i], CARETSTOPCHAR) > 0 then
      begin
        AChars := System.Copy(AChars, AOffset, i - AOffset);
        Result := True;
        Break;
      end;
    end;
  end;

  procedure GetAfterString(const AData: THCCustomData; const AStartItemNo: Integer; var AAfter: string);
  var
    i: Integer;
    vText: string;
  begin
    for i := AStartItemNo + 1 to AData.Items.Count - 1 do  // 向前
    begin
      vText := AData.Items[i].Text;
      if (vText <> '') and GetCharAfter(1, vText) then
      begin
        AAfter := AAfter + vText;
        Break;
      end
      else
        AAfter := AAfter + vText;
    end;
  end;
  {$ENDREGION}

var
  vTopData: THCCustomData;
  vCurItemNo, i: Integer;
  vCurItem: THCCustomItem;
  vText, vsBefor, vsAfter: string;
begin
  inherited DoCaretChange;
  if not FInputHelper.EnableEx then Exit;
  if not Self.Style.UpdateInfo.ReStyle then Exit;

  vsBefor := '';
  vsAfter := '';

  vTopData := Self.ActiveSectionTopLevelData;
  vCurItemNo := vTopData.SelectInfo.StartItemNo;
  vCurItem := vTopData.GetActiveItem;
  if vCurItem.StyleNo < THCStyle.Null then  // 光标在RectItem
  begin
    if vTopData.SelectInfo.StartItemOffset = OffsetBefor then  // 光标在前
      GetBeforString(vTopData, vCurItemNo - 1, vsBefor)
    else  // 光标在后
      GetAfterString(vTopData, vCurItemNo + 1, vsAfter);
  end
  else  // 文本
  begin
    // 取光标前
    vText := vCurItem.Text;
    if GetCharBefor(vTopData.SelectInfo.StartItemOffset, vText) then  // 从当前位置往前
      vsBefor := vText
    else  // 当前没取到
    begin
      vsBefor := System.Copy(vText, 1, vTopData.SelectInfo.StartItemOffset);
      GetBeforString(vTopData, vCurItemNo - 1, vsBefor);
    end;

    // 取光标后
    vText := vCurItem.Text;
    if GetCharAfter(vTopData.SelectInfo.StartItemOffset + 1, vText) then  // 从当前位置往后
      vsAfter := vText
    else  // 当前没取到
    begin
      vsAfter := System.Copy(vText, vTopData.SelectInfo.StartItemOffset + 1, Length(vText) - vTopData.SelectInfo.StartItemOffset);
      GetAfterString(vTopData, vCurItemNo + 1, vsAfter);
    end;
  end;

  FInputHelper.SetCaretString(vsBefor, vsAfter);
end;

function THCEmrViewIH.DoProcessIMECandi(const ACandi: string): Boolean;
begin
  Result := True;
end;

function THCEmrViewIH.GetInputHelpEnable: Boolean;
begin
  Result := FInputHelper.EnableEx;
end;

{$IFDEF GLOBALSHORTKEY}
function THCEmrViewIH.KeyboardProc(nCode: Integer; wParam: WPARAM;
  lParam: LPARAM): LRESULT;
var
  vEatKeystroke: Boolean;
  vPKB: PKBDLLHOOKSTRUCT;
begin
  Result := 0;
  vEatKeystroke := False;

  if nCode = HC_ACTION then  // 表示WParam和LParam参数包涵了按键消息
  begin
    case WParam of
      WM_SYSKEYDOWN, WM_SYSKEYUP:
        begin
          vPKB := PKBDLLHOOKSTRUCT(LParam);
          if vPKB.flags = LLKHF_ALTDOWN then
          begin
            if vPKB.vkCode = VK_SPACE then
            begin
              FInputHelper.ShowEx;
              vEatKeystroke := True;
            end;
          end;
        end;

      //WM_SYSKEYDOWN,
      //WM_KEYDOWN,
      //WM_KEYUP: ;
        //vEatKeystroke := ((vPKB.vkCode = VK_SPACE) and ((vPKB.flags and LLKHF_ALTDOWN) <> 0));
        //(p.vkCode = VK_RWIN) or (p.vkCode = VK_LWIN)
        //or ((p.vkCode = VK_TAB) and ((p.flags and LLKHF_ALTDOWN) <> 0))
        //or ((p.vkCode = VK_ESCAPE) and ((GetKeyState(VK_CONTROL) and $8000) <> 0));
    end;
  end;

  if vEatKeystroke then
    Result := 1
  else
  if nCode <> 0 then
    Result := CallNextHookEx(0, nCode, WPARAM, LParam);
end;

procedure THCEmrViewIH.SetIHKeyHook;
begin
  if HHKLowLevelKybd = 0 then  // 没设置过
  begin
    HHKLowLevelKybd := SetWindowsHookExW(WH_KEYBOARD_LL, FPProc, Hinstance, 0);

    if HHKLowLevelKybd = 0 then
      MessageBox(Handle, 'HCView辅助输入快捷键设置失败！', '提示', MB_OK);
  end;
end;

procedure THCEmrViewIH.UnSetIHKeyHook;
begin
  if HHKLowLevelKybd <> 0 then
  begin
    if UnhookWindowsHookEx(HHKLowLevelKybd) then  // 卸载成功
      HHKLowLevelKybd := 0;
  end;
end;
{$ENDIF}

procedure THCEmrViewIH.KeyDown(var Key: Word; Shift: TShiftState);

  function IsImeExtShow: Boolean;
  begin
    Result := (Shift = [ssCtrl{, ssAlt}]) and (Key = Ord('H'));  // ctrl + h主动弹出
  end;

  function IsImeExtClose: Boolean;
  begin
    Result := (Shift = []) and (Key = VK_ESCAPE);
  end;

begin
  if (not ReadOnly) and FInputHelper.EnableEx and IsImeExtShow then
    FInputHelper.ShowEx
  else
  if (not ReadOnly) and FInputHelper.EnableEx and IsImeExtClose then
    FInputHelper.CloseEx
  else
    inherited KeyDown(Key, Shift);
end;

function THCEmrViewIH.PreProcessMessage(var Msg: TMsg): Boolean;
var
  vVirtualKey: Cardinal;
  vText: string;
begin
  if (FInputHelper.VisibleEx) and (Msg.message = WM_KEYDOWN) and (Msg.wParam = VK_PROCESSKEY) then // 输入法转换的键
  begin
    //Msg.WParam := ImmGetVirtualKey(Msg.hwnd);  // 原样返回按啥显示啥
    vVirtualKey := ImmGetVirtualKey(Msg.hwnd);
    if vVirtualKey - 127 = 59 then  // ; 需要设置输入法;号的功能，如二三候选
    begin
      FInputHelper.ActiveEx := not FInputHelper.ActiveEx;
      Result := True;
      Exit;
    end
    else
    if FInputHelper.ActiveEx and (vVirtualKey in [32, 49..57]) then  // 空格, 1..9
    begin
      keybd_event(VK_ESCAPE, 0, 0, 0);
      keybd_event(VK_ESCAPE, 0, KEYEVENTF_KEYUP, 0);
      if vVirtualKey = 32 then
        vVirtualKey := 49;

      vText := FInputHelper.GetCandiText(vVirtualKey - 49);
      Self.InsertText(vText);

      Result := True;
      Exit;
    end;
  end;

  Result := inherited PreProcessMessage(Msg);
end;

procedure THCEmrViewIH.SetInputHelpEnable(const Value: Boolean);
begin
  FInputHelper.EnableEx := Value;
end;

procedure THCEmrViewIH.UpdateImeComposition(const ALParam: Integer);

  function GetCompositionStr(const AhImc: HIMC; const AType: Cardinal): string;
  var
    vSize: Integer;
    vBuffer: TBytes;
  begin
    Result := '';
    if AhImc <> 0 then
    begin
      vSize := ImmGetCompositionString(AhImc, AType, nil, 0);  // 获取IME结果字符串的大小
      if vSize > 0 then  	// 如果IME结果字符串不为空，且没有错误
      begin
        // 取出字符串
        SetLength(vBuffer, vSize);
        ImmGetCompositionString(AhImc, AType, vBuffer, vSize);
        Result := WideStringOf(vBuffer);
      end;
    end;
  end;

var
  vhIMC: HIMC;
  vS: string;
  vCF: TCompositionForm;
begin
  vhIMC := ImmGetContext(Handle);
  if vhIMC <> 0 then
  begin
    try
      if FInputHelper.EnableEx and ((ALParam and GCS_COMPSTR) <> 0) then  // 输入内容有变化
      begin
        vS := GetCompositionStr(vhIMC, GCS_COMPSTR);
        FInputHelper.SetCompositionString(vS);

        if FInputHelper.SizeChange then  // 知识输入法窗体大小有变化时重新处理输入法窗体位置
        begin
          if ImmGetCompositionWindow(vhIMC, @vCF) then
          begin
            if FInputHelper.ResetImeCompRect(vCF.ptCurrentPos) then
              ImmSetCompositionWindow(vhIMC, @vCF);
          end;
        end;
      end;

      if (ALParam and GCS_RESULTSTR) <> 0 then  // 通知检索或更新上屏字符串
      begin
        // 处理上屏文本一次性插入，否则会不停的触发KeyPress事件
        vS := GetCompositionStr(vhIMC, GCS_RESULTSTR);
        if vS <> '' then
        begin
          if DoProcessIMECandi(vS) then
            InsertText(vS);
        end;
      end
    finally
      ImmReleaseContext(Handle, vhIMC);
    end;
  end;
end;

procedure THCEmrViewIH.UpdateImePosition;
var
  vhIMC: HIMC;
  vCF: TCompositionForm;
begin
  vhIMC := ImmGetContext(Handle);
  try
    // 告诉输入法当前光标位置信息
    vCF.ptCurrentPos := Point(Caret.X, Caret.Y + Caret.Height + 4);  // 输入法弹出窗体位置

    if FInputHelper.EnableEx then
      FInputHelper.ResetImeCompRect(vCF.ptCurrentPos);

    vCF.dwStyle := CFS_FORCE_POSITION;  // 强制按我的位置 CFS_RECT
    vCF.rcArea := ClientRect;
    ImmSetCompositionWindow(vhIMC, @vCF);

    if FInputHelper.EnableEx then
    begin
      //ImmGetCompositionWindow() 输入法窗体可能会适配桌面区域，所以要重新计算位置
      FInputHelper.CompWndMove(Self.Handle, Caret.X, Caret.Y + Caret.Height);
    end;
  finally
    ImmReleaseContext(Handle, vhIMC);
  end;
end;

procedure THCEmrViewIH.WMImeNotify(var Message: TMessage);
begin
  inherited;
  if FInputHelper.EnableEx then
  begin
    case Message.WParam of
      {IMN_OPENSTATUSWINDOW:  // 打开输入法
        FImeExt.ImeOpen := True;

      IMN_CLOSESTATUSWINDOW:  // 关闭了输入法
        FImeExt.ImeOpen := False; }

      IMN_SETCOMPOSITIONWINDOW,  // 输入法输入窗体位置变化
      IMN_CLOSECANDIDATE:  // 关了备选
        FInputHelper.CompWndMove(Self.Handle, Caret.X, Caret.Y + Caret.Height);
    end;
  end;
end;

procedure THCEmrViewIH.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if Assigned(FInputHelper) and FInputHelper.EnableEx then
    FInputHelper.Close;
end;

end.
