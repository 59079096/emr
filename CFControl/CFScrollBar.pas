unit CFScrollBar;

interface

uses
  Windows, Controls, Classes, Graphics, Messages, CFControl;

type
  TScrollCode = (scLineUp, scLineDown, scPageUp, scPageDown, scPosition,
    scTrack, scTop, scBottom, scEndScroll);

  TScrollEvent = procedure(Sender: TObject; ScrollCode: TScrollCode;
    var ScrollPos: Integer) of object;

  TBarControl = (cbcBar, cbcLeftBtn, cbcThum, cbcRightBtn);
  TOrientation = (coHorizontal, coVertical);  // 水平/垂直、横向/纵向

  TCFScrollBar = class(TCFCustomControl)
  private
    /// <summary> 滚动条位置的最小值 </summary>
    FMin,
    /// <summary> 滚动条位置的最大值 </summary>
    FMax,
    /// <summary> 滚动条的最大位置与最小位置差 </summary>
    FRange,
    /// <summary> 垂直滚动条当前位置 </summary>
    FPosition: Integer;

    /// <summary> 界面可移动范围和实际范围的比率 </summary>
    FPercent: Single;

    /// <summary> 点击按钮可移动的大小 </summary>
    FBtnStep: Integer;

    /// <summary> 页面大小 </summary>
    FPageSize: Integer;

    /// <summary> 是水平还是垂直滚动条 </summary>
    FOrientation: TOrientation;

    /// <summary> 滚动事件 </summary>
    FOnScroll: TScrollEvent;

    FMouseDownControl: TBarControl;

    /// <summary> 滑块区域 </summary>
    FThumRect: TRect;

    /// <summary>
    /// 水平滚动条对应左按钮，垂直滚动条对应上按钮
    /// </summary>
    FLeftBtnRect: TRect;

    /// <summary>
    /// 水平滚动条对应右按钮，垂直滚动条对应下按钮
    /// </summary>
    FRightBtnRect: Trect;

    FOnVisibleChanged: TNotifyEvent;

    /// <summary>
    /// 得到鼠标上去要实现改变的区域
    /// </summary>
    procedure ReCalcButtonRect;

    /// <summary>
    /// 计算滑块区域
    /// </summary>
    procedure ReCalcThumRect;

    /// <summary>
    /// 设置滚动条类型（垂直滚动条、水平滚动条）
    /// </summary>
    /// <param name="Value">滚动条类型</param>
    procedure SetOrientation(Value: TOrientation);

    /// <summary>
    /// 设置滚动条的最小值
    /// </summary>
    /// <param name="Value">最小值</param>
    procedure SetMin(const Value: Integer);

    /// <summary>
    /// 设置滚动条的最大值
    /// </summary>
    /// <param name="Value">最大值</param>
    procedure SetMax(const Value: Integer);

    /// <summary>
    /// 设置滚动条的初始位置
    /// </summary>
    /// <param name="Value">初始位置</param>
    procedure SetPosition(Value: Integer);

    /// <summary>
    /// 设置滚动条表示的页面大小（相对Max - Min）
    /// </summary>
    /// <param name="Value">页面大小</param>
    procedure SetPageSize(const Value :Integer);

    /// <summary>
    /// 点击滚动条按钮页面移动范围
    /// </summary>
    /// <param name="Value">移动范围</param>
    procedure SetBtnStep(const Value: Integer);
  protected
    //procedure Resize; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure ScrollStep(ScrollCode: TScrollCode);
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawControl(ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  published
    property Align;
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property Rang: Integer read FRange;
    property PageSize: Integer read FPageSize write SetPageSize;
    property BtnStep: Integer read FBtnStep write SetBtnStep;
    property Position: Integer read FPosition write SetPosition;
    property Orientation: TOrientation read FOrientation write SetOrientation default coHorizontal;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
    property OnVisibleChanged: TNotifyEvent read FOnVisibleChanged write FOnVisibleChanged;
  end;

implementation

{$R CFScrollBar.RES}

uses
  SysUtils, Math, CFColorUtils;

const
  ButtonSize = 20;

var
  FMousePt: TPoint;

{ TCFScrollBar }

procedure TCFScrollBar.ReCalcThumRect;
var
  vPer: Single;
  vThumHeight, vPos: Integer;
begin
  case FOrientation of
    coHorizontal:
      begin
        FThumRect.Top := 0;
        FThumRect.Bottom := Height;
        if FPageSize < FRange then  // 页面小于范围
        begin
          vPer := FPageSize / FRange;  // 计算滑块比例
          // 计算滑块的高度
          vThumHeight := Round((Width - 2 * ButtonSize) * vPer);
          if vThumHeight < ButtonSize then  // 滑块高不能小于默认最小高度
            vThumHeight := ButtonSize;

          FPercent := (Width - 2 * ButtonSize - vThumHeight) / (FRange - FPageSize);  // 界面可滚动范围和实际代表范围的比率
          if FPercent < 0 then Exit;  // 防止vThumHeight小于Leftbtn、RightBtn、ThumBtn默认高度总和 3 * ButtonSize时计算出错
          if FPercent = 0 then
            FPercent := 1;

          FThumRect.Left := ButtonSize + Round(FPosition * FPercent);
          if FThumRect.Left + vThumHeight > Width - ButtonSize then  // 修改Max、Min、滑动在最低部时改变大小等操作时重新计算滑块高度
          begin
            FThumRect.Left := Width - ButtonSize - vThumHeight;
            FThumRect.Right := FThumRect.Left + vThumHeight;
            vPos := Round(FThumRect.Left / FPercent);  // 根据比率换算位置
            if vPos > FRange - FPageSize then  // 改变大小等操作时约束位置
              vPos := FRange - FPageSize;

            if FPosition <> vPos then
            begin
              FPosition := vPos;
              Scroll(scTrack, FPosition);  //鼠标移动改变滑块的垂直位置
            end;
          end
          else
            FThumRect.Right := FThumRect.Left + vThumHeight;
        end
        else  // 滚动轨道大于等于范围
        begin
          FThumRect.Left := ButtonSize;
          FThumRect.Right := Width - ButtonSize;
        end;
      end;

    coVertical:
      begin
        FThumRect.Left := 0;
        FThumRect.Right := Width;
        if FPageSize < FRange then  // 页面小于范围
        begin
          vPer := FPageSize / FRange;  // 计算滑块比例
          // 计算滑块的高度
          vThumHeight := Round((Height - 2 * ButtonSize) * vPer);
          if vThumHeight < ButtonSize then  // 滑块高不能小于默认最小高度
            vThumHeight := ButtonSize;

          FPercent := (Height - 2 * ButtonSize - vThumHeight) / (FRange - FPageSize);  // 界面可滚动范围和实际代表范围的比率
          if FPercent < 0 then Exit;  // 防止vThumHeight小于Leftbtn、RightBtn、ThumBtn默认高度总和 3 * ButtonSize时计算出错
          if FPercent = 0 then
            FPercent := 1;

          FThumRect.Top := ButtonSize + Round(FPosition * FPercent);
          if FThumRect.Top + vThumHeight > Height - ButtonSize then  // 修改Max、Min、滑动在最低部时改变大小等操作时重新计算滑块高度
          begin
            FThumRect.Top := Height - ButtonSize - vThumHeight;
            FThumRect.Bottom := FThumRect.Top + vThumHeight;
            vPos := Round(FThumRect.Top / FPercent);  // 根据比率换算位置
            if vPos > FRange - FPageSize then  // 改变大小等操作时约束位置
              vPos := FRange - FPageSize;

            if FPosition <> vPos then
            begin
              FPosition := vPos;
              Scroll(scTrack, FPosition);  //鼠标移动改变滑块的垂直位置
            end;
          end
          else
            FThumRect.Bottom := FThumRect.Top + vThumHeight;
        end
        else  // 滚动轨道大于等于范围
        begin
          FThumRect.Top := ButtonSize;
          FThumRect.Bottom := Height - ButtonSize;
        end;
      end;
  end;
end;

procedure TCFScrollBar.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if not Visible then
    FPosition := FMin;

  if Assigned(FOnVisibleChanged) then
    FOnVisibleChanged(Self);
end;

constructor TCFScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMin := 0;
  FMax := 100;
  FRange := 100;
  FPageSize := 0;
  BtnStep := 5;
  //
  Width := 20;
  Height := 20;
end;

destructor TCFScrollBar.Destroy;
begin

  inherited;
end;

procedure TCFScrollBar.DrawControl(ACanvas: TCanvas);
var
  vRect: TRect;
  vBmp: TBitmap;  // vIcon: HICON;
begin
  ACanvas.Brush.Color := Convert2Gray(GTitleBackColor, -20);
  ACanvas.FillRect(Bounds(0, 0, Width, Height));
  case FOrientation of
    coHorizontal:  // 水平滚动条
      begin
        //ACanvas.Brush.Color := GTitleForegColor;
        //ACanvas.FillRect(vRect);
        vBmp := TBitmap.Create;
        try
          vBmp.Transparent := True;
          // 水平滚动条左按钮
          vRect := FLeftBtnRect;
          vBmp.LoadFromResourceName(HInstance, 'DROPLEFT');
          ACanvas.Draw(vRect.Left + (vRect.Right - vRect.Left - GIconWidth) div 2, vRect.Top + (vRect.Bottom - vRect.Top - GIconWidth) div 2, vBmp);
          //DrawIconEx(ACanvas.Handle, vRect.Left + (vRect.Right - vRect.Left - GIconWidth) div 2, vRect.Top + (vRect.Bottom - vRect.Top - GIconWidth) div 2, vIcon, GIconWidth, GIconWidth, 0, 0, DI_NORMAL);

          // 水平滚动条右按钮
          vRect := FRightBtnRect;
          //ACanvas.FillRect(vRect);
          vBmp.LoadFromResourceName(HInstance, 'DROPRIGHT');
          ACanvas.Draw(vRect.Left + (vRect.Right - vRect.Left - GIconWidth) div 2, vRect.Top + (vRect.Bottom - vRect.Top - GIconWidth) div 2, vBmp);
          //DrawIconEx(ACanvas.Handle, vRect.Left + (vRect.Right - vRect.Left - GIconWidth) div 2, vRect.Top + (vRect.Bottom - vRect.Top - GIconWidth) div 2, vIcon, GIconWidth, GIconWidth, 0, 0, DI_NORMAL);
        finally
          vBmp.Free;
        end;

        // 水平滑块
        vRect := FThumRect;
        InflateRect(vRect, 0, -1);
        ACanvas.Brush.Color := Convert2Gray(GetDownColor(GTitleBackColor));
        ACanvas.Pen.Color := GLineColor;
        ACanvas.Rectangle(vRect);
        // 滑块上的修饰
        vRect.Left := vRect.Left + (vRect.Right - vRect.Left) div 2;
        ACanvas.MoveTo(vRect.Left, 5);
        ACanvas.LineTo(vRect.Left, Height - 5);
        ACanvas.MoveTo(vRect.Left + 3, 5);
        ACanvas.LineTo(vRect.Left + 3, Height - 5);
        ACanvas.MoveTo(vRect.Left - 3, 5);
        ACanvas.LineTo(vRect.Left - 3, Height - 5);
      end;

    coVertical:  // 垂直滚动条
      begin
        //ACanvas.Brush.Color := GTitleForegColor;
        //ACanvas.FillRect(vRect);
        vBmp := TBitmap.Create;
        try
          vBmp.Transparent := True;
          // 上按钮
          vRect := FLeftBtnRect;
          vBmp.LoadFromResourceName(HInstance, 'DROPUP');
          ACanvas.Draw(vRect.Left + (vRect.Right - vRect.Left - GIconWidth) div 2, vRect.Top + (vRect.Bottom - vRect.Top - GIconWidth) div 2, vBmp);
          //DrawIconEx(ACanvas.Handle, vRect.Left + (vRect.Right - vRect.Left - GIconWidth) div 2, vRect.Top + (vRect.Bottom - vRect.Top - GIconWidth) div 2, vIcon, GIconWidth, GIconWidth, 0, 0, DI_NORMAL);

          // 下按钮
          vRect := FRightBtnRect;
          ACanvas.FillRect(vRect);
          vBmp.LoadFromResourceName(HInstance, 'DROPDOWN');
          ACanvas.Draw(vRect.Left + (vRect.Right - vRect.Left - GIconWidth) div 2, vRect.Top + (vRect.Bottom - vRect.Top - GIconWidth) div 2, vBmp);
          //DrawIconEx(ACanvas.Handle, vRect.Left + (vRect.Right - vRect.Left - GIconWidth) div 2, vRect.Top + (vRect.Bottom - vRect.Top - GIconWidth) div 2, vIcon, GIconWidth, GIconWidth, 0, 0, DI_NORMAL);
        finally
          vBmp.Free;
        end;

        // 滑块
        vRect := FThumRect;
        InflateRect(vRect, -1, 0);
        //vRect.Right := vRect.Right - 1;
        ACanvas.Brush.Color := Convert2Gray(GetDownColor(GTitleBackColor));
        ACanvas.Pen.Color := GLineColor;
        ACanvas.Rectangle(vRect);
        // 滑块上的修饰
        vRect.Top := vRect.Top + (vRect.Bottom - vRect.Top) div 2;
        ACanvas.MoveTo(5, vRect.Top);
        ACanvas.LineTo(Width - 5, vRect.Top);
        ACanvas.MoveTo(5, vRect.Top - 3);
        ACanvas.LineTo(Width - 5, vRect.Top - 3);
        ACanvas.MoveTo(5, vRect.Top + 3);
        ACanvas.LineTo(Width - 5, vRect.Top + 3);
      end;
  end;
end;

procedure TCFScrollBar.ReCalcButtonRect;
begin
  case FOrientation of
    coHorizontal:
      begin
        FLeftBtnRect := Rect(0, 0, ButtonSize, Height);
        FRightBtnRect := Rect(Width - ButtonSize, 0, Width, Height);
      end;

    coVertical:
      begin
        FLeftBtnRect := Rect(0, 0, Width, ButtonSize);
        FRightBtnRect := Rect(0, Height - ButtonSize, Width, Height);
      end;
  end;
end;

procedure TCFScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FMousePt.X := X;
  FMousePt.Y := Y;
  if PtInRect(FLeftBtnRect, FMousePt) then  // 判断鼠标是否在滚动条上/左按钮区域
  begin
    FMouseDownControl := cbcLeftBtn;  // 鼠标所在区域类型
    ScrollStep(scLineUp);  // 数据向上（左）滚动
  end
  else
  if PtInRect(FThumRect, FMousePt) then  // 鼠标在滑块区域
    FMouseDownControl := cbcThum
  else
  if PtInRect(FRightBtnRect, FMousePt) then  // 鼠标在右/下区域
  begin
    FMouseDownControl := cbcRightBtn;
    ScrollStep(scLineDown);  // 数据向下（右）滚动
  end
  else  // 鼠标在滚动条的其他区域
  begin
    FMouseDownControl := cbcBar;  // 滚动条其他区域类型
    if (FThumRect.Top > Y) or (FThumRect.Left > X) then
      ScrollStep(scPageUp)  // 数据向上（左）翻页
    else
    if (FThumRect.Bottom < Y) or (FThumRect.Right < X) then
        ScrollStep(scPageDown);  // 数据向下（右）翻页
  end;
end;

procedure TCFScrollBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vOffsX, vOffsY, vPos: Integer;
begin
  inherited;
  if ssLeft in Shift then  // 拖动
  begin
    case FOrientation of
      coHorizontal:
        begin
          if FMouseDownControl = cbcThum then  // 鼠标在水平滚动条滑块区域
          begin
            vOffsX := X - FMousePt.X;
            if FThumRect.Right + vOffsX > Width - ButtonSize then  // 滑块移动超出移动区域（水平超右）
              vOffsX := Width - ButtonSize - FThumRect.Right  // 滑块向右最大偏移量
            else
            if FThumRect.Left + vOffsX < Left + ButtonSize then  // 滑块移动超出移动区域（水平超左）
              vOffsX := FThumRect.Left - ButtonSize - Left;  // 滑块向左最大偏移量

            OffsetRect(FThumRect, vOffsX, 0);  // 水平偏移
            if FThumRect.Left < ButtonSize + 2 then  // 通过误差2来修正拖动到最左端时不能到0的问题
              OffsetRect(FThumRect, ButtonSize - FThumRect.Left, 0);  // 垂直偏移

            vPos := Round((FThumRect.Left - ButtonSize) / FPercent);  // 当前数据起始值（相对Max）
            Position := vPos;
            FMousePt.X := X;  // 对水平坐标赋值
          end;
        end;

      coVertical:
        begin
          if FMouseDownControl = cbcThum then  // 在滑块内拖动
          begin
            vOffsY := Y - FMousePt.Y;
            if FThumRect.Bottom + vOffsY > Height - ButtonSize then  // 滑块移动超出移动区域（垂直超下）
            begin
              vOffsY := Height - ButtonSize - FThumRect.Bottom;  // 滑块向下最大偏移量
            end
            else
            if FThumRect.Top + vOffsY < Top + ButtonSize then  // 滑块移动超出移动区域（垂直超上）
            begin
              vOffsY := FThumRect.Top - ButtonSize - Top;  // 滑块向上最大偏移量
            end;

            OffsetRect(FThumRect, 0, vOffsY);  // 垂直偏移
            if FThumRect.Top < ButtonSize + 2 then  // 通过误差2来修正拖动到最上端时不能到0的问题
              OffsetRect(FThumRect, 0, ButtonSize - FThumRect.Top);  // 垂直偏移

            vPos := Round((FThumRect.Top - ButtonSize) / FPercent);
            Position := vPos;
            FMousePt.Y := Y;  // 对垂直坐标赋当前Y值
          end;
        end;
    end;
  end;
end;

procedure TCFScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

end;

//procedure TCFScrollBar.Resize;
//begin
//  inherited;
//  ReCalcThumRect;  // 重新计算滑块区域
//  ReCalcButtonRect;  // 重新计算按钮区域
//end;

procedure TCFScrollBar.Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Assigned(FOnScroll) then  // 滚动
    FOnScroll(Self, ScrollCode, ScrollPos);
end;

procedure TCFScrollBar.ScrollStep(ScrollCode: TScrollCode);
var
  vPos: Integer;
begin
  case ScrollCode of
    scLineUp:  // 点击上（左）按钮
      begin
        vPos := Position - FBtnStep;
        if vPos < FMin then  // 控制上（左）越界
          vPos := FMin;
        if FPosition <> vPos then
        begin
          Position := vPos;
          //Scroll(scLineUp, FPosition);
        end;
      end;

    scLineDown:
      begin
        vPos := Position + FBtnStep;
        if vPos > FRange - FPageSize then  // 控制下（右）越界
          vPos := FRange - FPageSize;
        if FPosition <> vPos then
        begin
          Position := vPos;
          //Scroll(scLineDown, FPosition);
        end;
      end;

    scPageUp:
      begin
        vPos := Position - FPageSize;
        {if FKind = sbVertical then
          vPos := Position - Height
        else
          vPos := Position - Width;}
        if vPos < FMin then
          vPos := FMin;
        if FPosition <> vPos then
        begin
          Position := vPos;
          //Scroll(scPageUp, FPosition);
        end;
      end;

    scPageDown:
      begin
        vPos := Position + FPageSize;
        {if FKind = sbVertical then
          vPos := Position + Height
        else
          vPos := Position + Width;}
        if vPos > FRange - FPageSize then
          vPos := FRange - FPageSize;
        if FPosition <> vPos then
        begin
          Position := vPos;
          //Scroll(scPageDown, FPosition);
        end;
      end;

    scPosition: ;
    scTrack: ;
    scTop: ;
    scBottom: ;
    scEndScroll: ;
  end;
end;

procedure TCFScrollBar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  // 当Align为非none时，父容器改变大小时不触发Resize所以需要用SetBounds
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  ReCalcThumRect;  // 重新计算滑块区域
  ReCalcButtonRect;  // 重新计算按钮区域
end;

procedure TCFScrollBar.SetBtnStep(const Value: Integer);
begin
  if FBtnStep <> Value then
    FBtnStep := Value;
end;

procedure TCFScrollBar.SetOrientation(Value: TOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if Value = coHorizontal then  // 设置为水平滚动条
      Height := 20  // 赋值水平滚动条的高度为 20
    else
    if Value = coVertical then  // 垂直滚动条
      Width := 20;

    ReCalcButtonRect;
    ReCalcThumRect;
    UpdateDirectUI;  // 重绘
  end;
end;

procedure TCFScrollBar.SetMax(const Value: Integer);
begin
  if FMax <> Value then
  begin
    if Value < FMin then
      FMax := FMin
    else
      FMax := Value;

    if FPosition > FMax then
      FPosition := FMax;

    FRange := FMax - FMin;
    ReCalcThumRect;  // 滑块区域
    UpdateDirectUI;  // 重绘
  end;
end;

procedure TCFScrollBar.SetMin(const Value: Integer);
begin
  if FMin <> Value then
  begin
    if Value > FMax then
      FMin := FMax
    else
      FMin := Value;

    if FPosition < FMin then
      FPosition := FMin;

    FRange := FMax - FMin;
    ReCalcThumRect;  // 滑块区域
    UpdateDirectUI;  // 重绘
  end;
end;

procedure TCFScrollBar.SetPageSize(const Value: Integer);
begin
  if FPageSize <> Value then
  begin
    FPageSize := Value;
    ReCalcThumRect;  // 重新计算相对比率（相对Max - Min）
    //UpdateDirectUI;  // 重绘 当其所有者大小改变时，造成系统中窗体后面的窗体闪烁
  end;
end;

procedure TCFScrollBar.SetPosition(Value: Integer);
begin
  if Value < FMin then
    Value := FMin
  else
  if Value > FMax then
    Value := FMax;

  if FPosition <> Value then
  begin
    FPosition := Value;
    ReCalcThumRect;  // 滑块区域
    Scroll(scTrack, FPosition);  //鼠标移动改变滑块的垂直位置
  end;
end;

end.
