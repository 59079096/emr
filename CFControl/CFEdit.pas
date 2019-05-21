unit CFEdit;

interface

uses
  Windows, Classes, Controls, CFControl, Graphics, Messages;

type
  TScrollAlign = (csaLeft, csaRight);

  TCFEdit = class(TCFTextControl)
  private
    FTopPadding,           // 上偏移多少开始显示文本
    FLeftPadding,          // 左偏移多少开始显示文本
    FRightPadding          // 右偏移多少停止显示文本
      : Byte;
    FDrawLeftOffs, FTextWidth: Integer;  // 绘制时左偏移，文本内容宽度
    FSelStart, FSelEnd: Integer;  // 选择起始和结束发生在第几个后面 >0 有效
    FCanSelect: Boolean;
    FSelecting: Boolean;
    FReadOnly: Boolean;
    FHelpText: string;
    /// <summary>
    /// 根据位置在第几个字符后面(>0有效)
    /// </summary>
    /// <param name="AX">位置</param>
    /// <returns>在第几个字符后面</returns>
    function GetOffsetBeforAt(const AX: Integer): Integer;
    procedure ScrollTo(const AOffset: Integer; const AAlign: TScrollAlign);
    procedure MoveCaretAfter(const AOffset: Integer);
    procedure RightKeyPress;
    procedure CopyText;
    procedure CutText;
    procedure PasteText;
    procedure SelectAll;
  protected
    /// <summary> 绘制到画布 </summary>
    procedure DrawControl(ACanvas: TCanvas); override;
    procedure DeleteSelect(const AUpdate: Boolean = False);
    procedure DisSelect;
    //
    procedure SetCanSelect(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetHelpText(Value: string);
    procedure AdjustBounds; override;
    //
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    //
    procedure CMMouseEnter(var Msg: TMessage ); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage ); message CM_MOUSELEAVE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;

    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;

    function SelectExist: Boolean;
    function TextLength: Integer;
    function SelText: string;
    //
    property RightPadding: Byte read FRightPadding write FRightPadding;
    property LeftPadding: Byte read FLeftPadding write FLeftPadding;
  published
    property CanSelect: Boolean read FCanSelect write SetCanSelect;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property HelpText: string read FHelpText write SetHelpText;
    //
    property Text;
    property OnChange;
    property OnKeyDown;
    property OnKeyPress;
  end;

implementation

uses
  Clipbrd;

{ TCFEdit }

procedure TCFEdit.ScrollTo(const AOffset: Integer; const AAlign: TScrollAlign);
var
  vS: string;
  vW, vDecW: Integer;
begin
  vS := Copy(Text, 1, AOffset);
  vW := Canvas.TextWidth(vS);
  if AAlign = csaRight then  // 向左滚动到AOffset恰好显示
  begin
    vDecW := FDrawLeftOffs + FLeftPadding + vW - (Width - FRightPadding);
    if vDecW > 0 then  // 超过宽度
    begin
      FDrawLeftOffs := FDrawLeftOffs - vDecW;
      UpdateDirectUI;
    end
    else
    if vDecW < 0 then  // 没有超过宽度
    begin
      if FDrawLeftOffs < 0 then  // 左侧有没显示出来的
      begin
        FDrawLeftOffs := FDrawLeftOffs - vDecW;
        if FDrawLeftOffs > 0 then
          FDrawLeftOffs := 0;
        UpdateDirectUI;
      end;
    end;
  end
  else
  begin
    vDecW := FDrawLeftOffs + vW;
    if vDecW < 0 then
    begin
      FDrawLeftOffs := FDrawLeftOffs - vDecW;
      UpdateDirectUI;
    end;
  end;
end;

procedure TCFEdit.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  UpdateDirectUI;
end;

procedure TCFEdit.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  UpdateDirectUI;
end;

procedure TCFEdit.CMTextChanged(var Message: TMessage);
begin
  inherited;

  AdjustBounds;
  UpdateDirectUI;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TCFEdit.CopyText;
begin
  if not FReadOnly then
    Clipboard.AsText := SelText;
end;

constructor TCFEdit.Create(AOwner: TComponent);
begin
  inherited;
  FCanSelect := True;
  FSelecting := False;
  FTopPadding := 2;
  FLeftPadding := 2;
  FRightPadding := 2;
  FDrawLeftOffs := 0;
  FSelStart := -1;
  FSelEnd := -1;
  Width := 120;
  Height := 20;
  //Text := 'TCFEdit';  // 处理win32自带属性默认值
end;

procedure TCFEdit.CutText;
var
  vS: string;
begin
  if not FReadOnly then
  begin
    vS := SelText;
    Clipboard.AsText := vS;
    DeleteSelect;
    FDrawLeftOffs := FDrawLeftOffs + Canvas.TextWidth(vS);
    if FDrawLeftOffs > 0 then
      FDrawLeftOffs := 0;
    UpdateDirectUI;
  end;
end;

procedure TCFEdit.DeleteSelect(const AUpdate: Boolean = False);
var
  vS: string;
begin
  if SelectExist then  // 有选中
  begin
    vS := Text;
    if FSelEnd < FSelStart then  // 选中结束位置在起始位置前面
      System.Delete(vS, FSelEnd + 1, FSelStart - FSelEnd)
    else  // 选中结束位置在起始位置后面
      System.Delete(vS, FSelStart + 1, FSelEnd - FSelStart);
    Text := vS;

    if FSelEnd < FSelStart then
      FSelStart := FSelEnd;
    FSelEnd := -1;
    if AUpdate then
      UpdateDirectUI;
    MoveCaretAfter(FSelStart);
  end;
end;

procedure TCFEdit.DisSelect;
begin
  if SelectExist then
  begin
    FSelStart := FSelEnd;
    FSelEnd := -1;
    UpdateDirectUI;
  end;
end;

procedure TCFEdit.DrawControl(ACanvas: TCanvas);
var
  vRect: TRect;
  vS: string;
  vLeft, vRight: Integer;
  vSaveDC: Integer;
  //vRgn: HRGN;
  //vBmp: TBitmap;
begin
  inherited;
  // 外观矩形
  vRect := Rect(0, 0, Width, Height);
  ACanvas.Brush.Style := bsSolid;

  if not FReadOnly then
    ACanvas.Brush.Color := GThemeColor
  else
    ACanvas.Brush.Color := clInfoBk;

  if Self.Focused or (cmsMouseIn in MouseState) then
    ACanvas.Pen.Color := GBorderHotColor
  else
    ACanvas.Pen.Color := GBorderColor;

  if BorderVisible then
    ACanvas.Pen.Style := psSolid
  else
    ACanvas.Pen.Style := psClear;

  if BorderVisible then  // 显示边框时圆角区域
    //ACanvas.RoundRect(vRect, GRoundSize, GRoundSize)
    ACanvas.Rectangle(vRect)
  else
    ACanvas.FillRect(vRect);

  // 设置可绘制区域
  InflateRect(vRect, 0, -FTopPadding);
  vRect.Left := vRect.Left + FLeftPadding;
  vRect.Right := vRect.Right - FRightPadding;

  //vRgn := CreateRectRgnIndirect(vRect);
  //SelectClipRgn(ACanvas.Handle, vRgn);  //ExtSelectClipRgn(ACanvas.Handle, vRgn)
  IntersectClipRect(ACanvas.Handle, vRect.Left, vRect.Top, vRect.Right, vRect.Bottom);
  try
    // 绘制文本
    if Text <> '' then  // 有内容
    begin
      //ACanvas.Font.Assign(Font);
      if FSelEnd >= 0 then  // 有选中
      begin
        vLeft := FLeftPadding + FDrawLeftOffs;  // 偏移到可显示起始位置

        if FSelEnd < FSelStart then  // 选中结束位置在起始位置前面
        begin
          vS := Copy(Text, 1, FSelEnd);
          if vS <> '' then
            vLeft := vLeft + ACanvas.TextWidth(vS);

          vS := Copy(Text, 1, FSelStart);
          vRight := FLeftPadding + FDrawLeftOffs + ACanvas.TextWidth(vS);
        end
        else  // 选中结束位置在起始位置后面
        begin
          vS := Copy(Text, 1, FSelStart);
          if vS <> '' then
            vLeft := vLeft + ACanvas.TextWidth(vS);

          vS := Copy(Text, 1, FSelEnd);
          vRight := FLeftPadding + FDrawLeftOffs + ACanvas.TextWidth(vS);
        end;
        // 绘制选中区域背景
        ACanvas.Brush.Color := GHightLightColor;
        ACanvas.FillRect(Rect(vLeft, FTopPadding, vRight, Height - FTopPadding));
        ACanvas.Brush.Style := bsClear;
      end;
      // 绘制文本
      vRect := Bounds(0, 0, FTextWidth, Height);
      OffsetRect(vRect, FDrawLeftOffs + FLeftPadding, 0);
      vS := Text;
      ACanvas.TextRect(vRect, vS, [tfLeft, tfVerticalCenter, tfSingleLine]);
    end
    else
    if FHelpText <> '' then  // 提示信息
    begin
      vSaveDC := SaveDC(ACanvas.Handle);  // 保存(设备上下文环境)现场
      try
        ACanvas.Font.Style := [fsItalic];
        ACanvas.Font.Color := clMedGray;  // clGrayText;
        vRect := Bounds(0, 0, Canvas.TextWidth(FHelpText), Height);
        OffsetRect(vRect, FLeftPadding, 0);
        vS := FHelpText;
        ACanvas.TextRect(vRect, vS, [tfLeft, tfVerticalCenter, tfSingleLine]);
      finally
        RestoreDC(ACanvas.Handle, vSaveDC);  // 恢复(设备上下文环境)现场
      end;
    end;
  finally
    SelectClipRgn(ACanvas.Handle, 0);  // 清除剪切区域
    //DeleteObject(vRgn)
  end;
end;

procedure TCFEdit.AdjustBounds;
var
  Rect: TRect;
begin
  if not (csReading in ComponentState) then
  begin
    Rect := ClientRect;

    Canvas.Font := Font;
    Rect.Bottom := Rect.Top + Canvas.TextHeight('字') + GetSystemMetrics(SM_CYBORDER) * 4;
    FTextWidth := Canvas.TextWidth(Text);
    Canvas.Handle := 0;

    SetBounds(Left, Top, Rect.Right, Rect.Bottom);
  end;
end;

function TCFEdit.GetOffsetBeforAt(const AX: Integer): Integer;
var
  i, vW, vRight: Integer;
begin
  Result := -1;
  //if (AX < FDrawLeftOffs) or (AX > FTextWidth) then Exit;
  if AX < FDrawLeftOffs then
  begin
    Result := 0;
    Exit;
  end;
  if AX > FTextWidth then
  begin
    Result := TextLength;
    Exit;
  end;

  if AX < Canvas.TextWidth(Text) then  // 在文本中
  begin
    vRight := 0;
    for i := 1 to Length(Text) do
    begin
      vW := Canvas.TextWidth(Text[i]);
      if vRight + vW > AX then
      begin
        if vRight + vW div 2 > AX then
          Result := i - 1
        else
        begin
          Result := i;
          vRight := vRight + vW;
        end;
        Break;
      end
      else
        vRight := vRight + vW;
    end;
  end
  else  // 不在文本中
    Result := TextLength;
end;

procedure TCFEdit.KeyDown(var Key: Word; Shift: TShiftState);

  {$REGION 'DoBackKey'}
  procedure DoBackKey;
  var
    vS: string;
  begin
    if SelectExist then
      DeleteSelect(True)
    else
    begin
      if (Text <> '') and (FSelStart > 0) then
      begin
        vS := Text;
        Delete(vS, FSelStart, 1);
        Text := vS;
        Dec(FSelStart);
        if FSelStart = TextLength then  // 在最后面向前删除
        begin
          if FDrawLeftOffs < 0 then  // 前面有没显示出来的
            ScrollTo(FSelStart, csaRight);
        end
        else
          UpdateDirectUI;
        MoveCaretAfter(FSelStart);
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'DoDeleteKey'}
  procedure DoDeleteKey;
  var
    vS: string;
  begin
    if SelectExist then
      DeleteSelect(True)
    else
    begin
      if (Text <> '') and (FSelStart < TextLength) then
      begin
        vS := Text;
        Delete(vS, FSelStart + 1, 1);
        Text := vS;
        if FSelStart = TextLength then  // 在最后面向前删除
        begin
          if FDrawLeftOffs < 0 then  // 前面有没显示出来的
          begin
            ScrollTo(FSelStart, csaRight);
            MoveCaretAfter(FSelStart);
          end;
        end;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'DoHomeKey'}
  procedure DoHomeKey;
  begin
    DisSelect;
    FSelStart := 0;
    FDrawLeftOffs := 0;
    MoveCaretAfter(FSelStart);
  end;
  {$ENDREGION}

  {$REGION 'DoEndKey'}
  procedure DoEndKey;
  begin
    DisSelect;
    FSelStart := TextLength;
    ScrollTo(FSelStart, csaRight);
    MoveCaretAfter(FSelStart);
  end;
  {$ENDREGION}

  {$REGION 'DoLeftKeyPress'}
  procedure DoLeftKeyPress;
  begin
    DisSelect;
    if FSelStart > 0 then
    begin
      Dec(FSelStart);
      ScrollTo(FSelStart, csaLeft);
      MoveCaretAfter(FSelStart);
    end;
  end;
  {$ENDREGION}

begin
  inherited;
  if FReadOnly then Exit;
  if Shift = [ssCtrl] then
  begin
    case Key of
      Ord('C'): CopyText;
      Ord('X'): CutText;
      Ord('V'): PasteText;
    end;
    Exit;
  end;

  case Key of
    VK_BACK: DoBackKey;
    VK_RETURN: ;
    VK_DELETE: DoDeleteKey;
    VK_LEFT: DoLeftKeyPress;
    VK_RIGHT: RightKeyPress;
    VK_HOME: DoHomeKey;
    VK_END: DoEndKey;
  end;
end;

procedure TCFEdit.KeyPress(var Key: Char);
var
  vS: string;
begin
  inherited;
  if FReadOnly then Exit;

  //if Ord(Key) in [VK_BACK, VK_RETURN] then Exit;
  if ((Key < #32) or (Key = #127)) and (Ord(Key) <> VK_TAB) then Exit;

  DeleteSelect;  // 删除选中
  vS := Text;
  Insert(Key, vS, FSelStart + 1);
  Text := vS;

  RightKeyPress;
end;

procedure TCFEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vS: string;
  vOffs: Integer;
  vSelStart: Integer absolute vOffs;
  vReUpdate: Boolean;
begin
  inherited;
  if FReadOnly then Exit;
  if (X < FLeftPadding) or (X > Width - FRightPadding) then
  begin
    DisSelect;
    Exit;
  end;
  vReUpdate := False;
  //FTextWidth := Canvas.TextWidth(Text);
  if FSelEnd >= 0 then
  begin
    FSelEnd := -1;
    vReUpdate := True;
  end;
  vSelStart := GetOffsetBeforAt(X - FDrawLeftOffs - FLeftPadding);
  if FSelStart <> vSelStart then
  begin
    FSelStart := vSelStart;
    vReUpdate := True;
  end;
  // 当显示最右侧的字符有半个露出时，滚动到全显示
  vS := Copy(Text, 1, FSelStart);
  vOffs := Width - FLeftPadding - FRightPadding - (Canvas.TextWidth(vS) + FDrawLeftOffs);
  if vOffs < 0 then
  begin
    FDrawLeftOffs := FDrawLeftOffs + vOffs;
    vReUpdate := True;
  end;
  if vReUpdate then
    UpdateDirectUI;
  MoveCaretAfter(FSelStart);
  FSelecting := True;
end;

procedure TCFEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vS: string;
  vSelEnd, vW: Integer;
  vReUpdate: Boolean;
begin
  inherited;
  if FReadOnly then Exit;
  if FSelecting and (ssLeft in Shift) then  // 划选
  begin
    if not FCanSelect then Exit;

    vReUpdate := False;
    try
      if X > Width - FRightPadding then  // 鼠标在控件右侧
      begin
        vW := FTextWidth - (Width - FLeftPadding - FRightPadding);
        if FDrawLeftOffs + vW > 0 then  // 右侧内容未显示完全
        begin
          Dec(FDrawLeftOffs, GPadding);
          if FDrawLeftOffs < -vW then
            FDrawLeftOffs := -vW;
          vReUpdate := True;
        end;
        vSelEnd := GetOffsetBeforAt(Width - FDrawLeftOffs + FLeftPadding - FRightPadding);
        vS := Copy(Text, 1, vSelEnd);
        if Canvas.TextWidth(vS) + FDrawLeftOffs > Width - FLeftPadding - FRightPadding then
          Dec(vSelEnd);
      end
      else
      if X < FLeftPadding then
      begin
        if FDrawLeftOffs < 0 then  // 左侧内容未显示完全
        begin
          Inc(FDrawLeftOffs, GPadding);
          if FDrawLeftOffs > 0 then
            FDrawLeftOffs := 0;
          vReUpdate := True;
        end;
        vSelEnd := GetOffsetBeforAt({FLeftPadding - 当字符一半小于FLeftPadding时计算不准确}-FDrawLeftOffs);
        vS := Copy(Text, 1, vSelEnd);
        if Canvas.TextWidth(vS) + FDrawLeftOffs < 0 then
          Inc(vSelEnd);
      end
      else
      begin
        vSelEnd := GetOffsetBeforAt(X - FDrawLeftOffs - FLeftPadding);
        vS := Copy(Text, 1, vSelEnd);
        if Canvas.TextWidth(vS) + FDrawLeftOffs + FLeftPadding > Width - FRightPadding then
          Dec(vSelEnd);
      end;

      if vSelEnd < 0 then  // 鼠标不在内容中
      begin
        if SelectExist then  // 划选到控件外时，如果有选中，则选中结束位置不改动
          Exit;
      end;

      //if FSelEnd <> vSelEnd then  // 暂时保留下面的begin end
      begin
        FSelEnd := vSelEnd;
        if FSelEnd = FSelStart then  // 选择起始和结束位置相同
          FSelEnd := -1;
        if FSelEnd < 0 then Exit;
        vS := Copy(Text, 1, FSelEnd);
        SetCaretPos(Left + FLeftPadding + Canvas.TextWidth(vS) + FDrawLeftOffs, Top + FTopPadding);
        //PostMessage(GetUIHandle, WM_C_CARETCHANGE, 0, Height - 2 * FTopPadding);
        vReUpdate := True;
      end;
    finally
      if vReUpdate then
        UpdateDirectUI;
    end;
  end;
end;

procedure TCFEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if FReadOnly then Exit;
  FSelecting := False;
end;

procedure TCFEdit.MoveCaretAfter(const AOffset: Integer);
var
  vS: string;
  vCaretLeft, vCaretHeight: Integer;
begin
  if AOffset < 0 then Exit;
  vCaretHeight := Height - 2 * FTopPadding;
  vCaretLeft := Left + FLeftPadding;

  if AOffset > 0 then
  begin
    vS := Copy(Text, 1, AOffset);
    vCaretLeft := vCaretLeft + Canvas.TextWidth(vS) + FDrawLeftOffs;
  end;

  DestroyCaret;
  CreateCaret(Handle, 0, 1, vCaretHeight);
  SetCaretPos(vCaretLeft, Top + FTopPadding);
  ShowCaret(Handle);
  //PostMessage(GetUIHandle, WM_C_CARETCHANGE, 0, vCaretHeight);
end;

procedure TCFEdit.PasteText;
var
  vsClip, vS: string;
begin
  if not FReadOnly then
  begin
    DeleteSelect;
    vS := Text;
    vsClip := Clipboard.AsText;
    Insert(vsClip, vS, FSelStart + 1);
    Text := vS;
    FSelStart := FSelStart + Length(vsClip);
    if FDrawLeftOffs < 0 then
      FDrawLeftOffs := FDrawLeftOffs - Canvas.TextWidth(vsClip)
    else
      ScrollTo(FSelStart, csaRight);
    MoveCaretAfter(FSelStart);
    UpdateDirectUI;
  end;
end;

procedure TCFEdit.RightKeyPress;
begin
  DisSelect;
  if FSelStart < TextLength then
  begin
    Inc(FSelStart);
    ScrollTo(FSelStart, csaRight);
    MoveCaretAfter(FSelStart);
  end;
end;

procedure TCFEdit.SelectAll;
begin
  FSelecting := False;  // 处理双击并移动鼠标时，虽然双击全选，但移动又将选中结束位置改为当前鼠标处
  FSelStart := 0;
  FSelEnd := TextLength;
  ScrollTo(FSelEnd, csaRight);
  UpdateDirectUI;
  MoveCaretAfter(FSelEnd);
end;

function TCFEdit.SelectExist: Boolean;
begin
  Result := (FSelEnd >= 0) and (FSelEnd <> FSelStart);
end;

function TCFEdit.SelText: string;
begin
  if SelectExist then
    Result := Copy(Text, FSelStart + 1, FSelEnd - FSelStart)
  else
    Result := '';
end;

procedure TCFEdit.SetCanSelect(Value: Boolean);
begin
  if FCanSelect <> Value then
  begin
    FCanSelect := Value;
    if not FCanSelect then
      DisSelect;
  end;
end;

procedure TCFEdit.SetHelpText(Value: string);
begin
  if FHelpText <> Value then
  begin
    FHelpText := Value;
    UpdateDirectUI;
  end;
end;

procedure TCFEdit.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    DisSelect;
    DestroyCaret;
  end;
end;

function TCFEdit.TextLength: Integer;
begin
  Result := Length(Text);
end;

procedure TCFEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
  SelectAll;
end;

end.
