unit HCViewIH;

interface

uses
  Windows, Classes, SysUtils, Messages, Imm, HCView, HCInputHelper, HCCustomData;

type
  THCViewIH = class(THCView)
  private
    FInputHelper: THCInputHelper;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    /// <summary> 是否上屏输入法输入的词条屏词条ID和词条 </summary>
    function DoProcessIMECandi(const ACandi: string): Boolean; virtual;
    procedure WMImeNotify(var Message: TMessage); message WM_IME_NOTIFY;
    procedure UpdateImeComposition(const ALParam: Integer); override;
    procedure UpdateImePosition; override;  // IME 通知输入法更新位置
    /// <summary> 实现插入文本 </summary>
    function DoInsertText(const AText: string): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ THCViewIH }

constructor THCViewIH.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInputHelper := THCInputHelper.Create;
end;

destructor THCViewIH.Destroy;
begin
  FreeAndNil(FInputHelper);
  inherited;
end;

function THCViewIH.DoInsertText(const AText: string): Boolean;
var
  vTopData: THCCustomData;
begin
  Result := inherited DoInsertText(AText);
  vTopData := Self.ActiveSectionTopLevelData;
end;

function THCViewIH.DoProcessIMECandi(const ACandi: string): Boolean;
begin
  Result := True;
end;

procedure THCViewIH.KeyDown(var Key: Word; Shift: TShiftState);

  function IsImeExtShow: Boolean;
  begin
    Result := (Shift = [ssCtrl, ssAlt]) and (Key = VK_SPACE);
  end;

  function IsImeExtClose: Boolean;
  begin
    Result := (Shift = []) and (Key = VK_ESCAPE);
  end;

begin
  if FInputHelper.Enable and IsImeExtShow then
    FInputHelper.Show
  else
  if FInputHelper.Enable and IsImeExtClose then
    FInputHelper.Close
  else
    inherited KeyDown(Key, Shift);
end;

procedure THCViewIH.UpdateImeComposition(const ALParam: Integer);

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
      if FInputHelper.Enable and ((ALParam and GCS_COMPSTR) <> 0) then  // 输入内容有变化
      begin
        vS := GetCompositionStr(vhIMC, GCS_COMPSTR);
        FInputHelper.SetCompositionString(vS);

        if FInputHelper.Resize then  // 知识输入法窗体大小有变化时重新处理输入法窗体位置
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

procedure THCViewIH.UpdateImePosition;
var
  vhIMC: HIMC;
  vCF: TCompositionForm;
begin
  vhIMC := ImmGetContext(Handle);
  try
    // 告诉输入法当前光标位置信息
    vCF.ptCurrentPos := Point(Caret.X, Caret.Y + Caret.Height + 4);  // 输入法弹出窗体位置

    if FInputHelper.Enable then
      FInputHelper.ResetImeCompRect(vCF.ptCurrentPos);

    vCF.dwStyle := CFS_FORCE_POSITION;  // 强制按我的位置  CFS_RECT
    vCF.rcArea := ClientRect;
    ImmSetCompositionWindow(vhIMC, @vCF);

    if FInputHelper.Enable then
    begin
      //ImmGetCompositionWindow() 输入法窗体可能会适配桌面区域，所以要重新计算位置
      FInputHelper.CompWndMove(Self.Handle, Caret.X, Caret.Y + Caret.Height);
    end;
  finally
    ImmReleaseContext(Handle, vhIMC);
  end;
end;

procedure THCViewIH.WMImeNotify(var Message: TMessage);
begin
  if FInputHelper.Enable then
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

  inherited;
end;

end.
