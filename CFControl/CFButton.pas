unit CFButton;

interface

uses
  Windows, Classes, Controls, Graphics, Messages, CFControl;

type
  TCFButton = class(TCFTextControl)
  private
    { Private declarations }
    FModalResult: TModalResult;
  protected
    { Protected declarations }
    /// <summary>
    /// 绘制
    /// </summary>
    /// <param name="ACanvas">呈现画布</param>
    procedure DrawControl(ACanvas: TCanvas); override;

    /// <summary> 单击事件 </summary>
    procedure Click; override;

    /// <summary> 处理默认大小和范围 </summary>
    procedure AdjustBounds; override;

    /// <summary>
    /// 设置模式结果
    /// </summary>
    /// <param name="Value">模式</param>
    procedure SetModalResult(Value: TModalResult);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    /// <summary> 鼠标移入 </summary>
    procedure CMMouseEnter(var Msg: TMessage ); message CM_MOUSEENTER;

    /// <summary> 鼠标移出 </summary>
    procedure CMMouseLeave(var Msg: TMessage ); message CM_MOUSELEAVE;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    /// <summary> 按钮的模式结果(确定、关闭等) </summary>
    property ModalResult: TModalResult read FModalResult write SetModalResult default 0;
    property Caption;
    property Alpha;
    property OnClick;
  end;

implementation

uses
  CFColorUtils;

{ TCFButton }

procedure TCFButton.AdjustBounds;
var
  DC: HDC;
  vNewHeight, vNewWidth: Integer;
begin
  if not (csReading in ComponentState) then  // 不处理控件初始加载状态
  begin
    DC := GetDC(0);  // 临时DC
    try
      Canvas.Handle := DC;
      Canvas.Font := Font;
      vNewHeight := Canvas.TextHeight('荆') + GetSystemMetrics(SM_CYBORDER) * 4;
      vNewWidth := Canvas.TextWidth(Caption) + GetSystemMetrics(SM_CYBORDER) * 8;
      Canvas.Handle := 0;
    finally
      ReleaseDC(0, DC);
    end;
    if vNewHeight < 25 then
      vNewHeight := 25;
    if vNewWidth < Width then
    begin
      vNewWidth := Width;
      if vNewWidth < 75 then
        vNewWidth := 75;
    end;

    SetBounds(Left, Top, vNewWidth, vNewHeight);
  end;
end;

procedure TCFButton.Click;
begin
  if Assigned(OnClick) then  // 有赋值单击事件
    inherited
  else
  begin
    {case FModalResult of
      mrClose: PostMessage(GetUIHandle, WM_CLOSE, 0, 0);  // 关闭当前窗体
    end;}
  end;
end;

procedure TCFButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  UpdateDirectUI;
end;

procedure TCFButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  UpdateDirectUI;
end;

constructor TCFButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 75;
  Height := 25;
end;

procedure TCFButton.DrawControl(ACanvas: TCanvas);
var
  vRect: TRect;
  vText: string;
  vBackColor: TColor;
  // 绘制半透明需要的变量
//  vMemDC: HDC;
//  vMemBitmap, vOldBitmap: HBITMAP;
//  vCanvas: TCanvas;
//  vBlendFunction: TBlendFunction;
  //vRgn: HRGN;
  //vPng: TPngImage;
  //AGraphics: TdxGPGraphics;
  //vPen: TdxGPPen;
  //vRgn: HRGN;
  //vBmp: TBitmap;
begin
  inherited DrawControl(ACanvas);

  case FModalResult of  // 处理颜色
    mrClose:
      vBackColor := GAlertColor;
  else
    vBackColor := GAreaBackColor;
  end;

  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := GetBorderColor(vBackColor);
  if cmsMouseIn in MouseState then  // 鼠标在控件内
  begin
    if cmsMouseDown in MouseState then  // 鼠标按下
      ACanvas.Brush.Color := GetDownColor(vBackColor)
    else
      ACanvas.Brush.Color := GetHotColor(vBackColor);
  end
  else  // 普通状态
    ACanvas.Brush.Color := vBackColor;

  vRect := Rect(0, 0, Width, Height);
  if RoundCorner > 0 then
    ACanvas.RoundRect(vRect, GRoundSize, GRoundSize)
  else
    ACanvas.FillRect(vRect);

  vText := Caption;
  ACanvas.TextRect(vRect, vText, [tfSingleLine, tfCenter,tfVerticalCenter]);

{$REGION '调试透明度绘制代码'}
//  if AAlpha <> 255 then
//  begin
//    // 不使用GDI因为在dll中初始化gdi时界面不显示
//    vMemBitmap := CreateCompatibleBitmap(ACanvas.Handle, Width, Height);
//    try
//      vMemDC := CreateCompatibleDC(ACanvas.Handle);
//      vOldBitmap := SelectObject(vMemDC, vMemBitmap);
//      BitBlt(vMemDC, 0, 0, Width, Height, ACanvas.Handle, X, Y, SRCCOPY);  // 拷贝原图此位置的图像
//      try
//        vCanvas := TCanvas.Create;
//        vCanvas.Handle := vMemDC;
//        DrawTo(vCanvas, 0, 0, 255);
//
//        vBlendFunction.BlendOp := AC_SRC_OVER;
//        vBlendFunction.BlendFlags := 0;
//        vBlendFunction.AlphaFormat := AC_SRC_OVER;  // 源位图必须是32位深
//        vBlendFunction.SourceConstantAlpha := AAlpha; // 透明度
//        Windows.AlphaBlend(ACanvas.Handle,
//                           X,
//                           Y,
//                           Width,
//                           Height,
//                           vMemDC,
//                           0,
//                           0,
//                           Width,
//                           Height,
//                           vBlendFunction
//                           );
//      finally
//        SelectObject(vMemDC, vOldBitmap)
//      end;
//    finally
//      vCanvas.Free;
//      DeleteDC(vMemDC);
//      DeleteObject(vMemBitmap);
//    end;
//
//{使用bmp完成
//    vBmp := TBitmap.Create;
//    vBmp.SetSize(Width, Height);
//    BitBlt(vBmp.Canvas.Handle, 0, 0, Width, Height, ACanvas.Handle, X, Y, SRCCOPY);
//    DrawTo(vBmp.Canvas, 0, 0, 255);
//    //vBmp.SaveToFile('C:\a.BMP');
//
//
//    vBlendFunction.BlendOp := AC_SRC_OVER;
//    vBlendFunction.BlendFlags := 0;
//    vBlendFunction.AlphaFormat := AC_SRC_OVER;  // 源位图必须是32位深
//    vBlendFunction.SourceConstantAlpha := AAlpha; // 透明度
//
//    Windows.AlphaBlend(ACanvas.Handle,
//                       X,
//                       Y,
//                       Width,
//                       Height,
//                       vBmp.Canvas.Handle,
//                       0,
//                       0,
//                       Width,
//                       Height,
//                       vBlendFunction
//                       );
//
//    vBmp.Free;}
//
//
////    vRgn := CreateRoundRectRgn(X, Y, Width, Height, 5, 5);
////    SelectClipRgn(ACanvas.Handle, vRgn);
////    AlphaBlend(ACanvas.Handle, X, Y, Width, Height, Canvas.Handle, 0, 0, Width, Height, vBlendFunction);
////    SelectClipRgn(ACanvas.Handle, 0);
////    DeleteObject(vRgn);
////    vPng := TPngImage.CreateBlank(6,  // COLOR_RGBALPHA
////      8, 500, 500);
////    BitBlt(vPng.Canvas.Handle, X, Y, Width, Height, ACanvas.Handle, 0, 0, SRCCOPY);
////    vPng.SaveToFile('c:\a.png');
////
////
////    vBmp := TBitmap.Create;
////    vBmp.SetSize(500, 500);
////    BitBlt(vBmp.Canvas.Handle, X, Y, Width, Height, ACanvas.Handle, 0, 0, SRCCOPY);
////    vBmp.SaveToFile('c:\a.bmp');
////    vBmp.Free;
////
////    vPng := TPngImage.Create;
////    vPng.LoadFromFile('E:\参考程序\MedPlatform_V2\Source\Resource\1.png');
////    DrawTo(vPng.Canvas, 0, 0, 255);
////    vPng.SaveToFile('c:\1.png');
////    ACanvas.Draw(X, Y, vPng);
////    vPng.Free;
//
////    Rgn := CreateRoundRectRgn(X, Y, X + Width, Y + Height, 5, 5);
////    SelectClipRgn(ACanvas.Handle, Rgn);
////    DeleteObject(Rgn);
////
////    vBlendFunction.BlendOp := AC_SRC_OVER;
////    vBlendFunction.BlendFlags := 0;
////    vBlendFunction.AlphaFormat := AC_SRC_ALPHA;  // 源位图必须是32位深
////    vBlendFunction.SourceConstantAlpha := AAlpha; // 透明度
////    //BitBlt(ACanvas.Handle, X, Y, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
////    Windows.AlphaBlend(ACanvas.Handle,
////                       X,
////                       Y,
////                       Width,
////                       Height,
////                       Canvas.Handle,
////                       0,
////                       0,
////                       Width,
////                       Height,
////                       vBlendFunction
////                       );
////     SelectClipRgn(ACanvas.Handle, 0);  // 清除剪切区域
//{    vMemBitmap := CreateCompatibleBitmap(ACanvas.Handle, Width, Height);
//    try
//      vMemDC := CreateCompatibleDC(ACanvas.Handle);
//      vOldBitmap := SelectObject(vMemDC, vMemBitmap);
//      try
//        vCanvas := TCanvas.Create;
//        vCanvas.Handle := vMemDC;
//        vCanvas.Brush.Color := 1;
//        vCanvas.FillRect(Rect(0, 0, Width, Height));
//        DrawTo(vCanvas, 0, 0, 255);
//        TransparentBlt(ACanvas.Handle, X, Y, Width, Height, vMemDC, 0, 0, Width, Height, 1);
////        vRect := Rect(X, Y, X + Width, Y + Height);
////        vText := Caption;
////        ACanvas.TextRect(vRect, vText, [tfSingleLine, tfCenter,tfVerticalCenter]);
////        vBlendFunction.BlendOp := AC_SRC_OVER;
////        vBlendFunction.BlendFlags := 0;
////        vBlendFunction.AlphaFormat := AC_SRC_OVER;  // 源位图必须是32位深
////        vBlendFunction.SourceConstantAlpha := AAlpha; // 透明度
////        Windows.AlphaBlend(ACanvas.Handle,
////                           X,
////                           Y,
////                           Width,
////                           Height,
////                           vMemDC,
////                           0,
////                           0,
////                           Width,
////                           Height,
////                           vBlendFunction
////                           );
//        //vPng.Free;
//      finally
//        SelectObject(vMemDC, vOldBitmap)
//      end;
//    finally
//      vCanvas.Free;
//      DeleteDC(vMemDC);
//      DeleteObject(vMemBitmap);
//    end; }
//  end
//  else
//  begin
//    ACanvas.Pen.Width := 1;
//    ACanvas.Pen.Color := FBorderColor;
//    if FMouseEnter then
//    begin
//      if cbsMouseDown in MouseState then
//        ACanvas.Brush.Color := FDownColor
//      else
//        ACanvas.Brush.Color := FHotColor;
//    end
//    else
//      ACanvas.Brush.Color := Color;
//    vRect := Rect(X, Y, X + Width, Y + Height);
//    //vRgn := CreateRectRgnIndirect(BoundsRect);
//    //SelectClipRgn(ACanvas.Handle, vRgn);
//    //try
//      ACanvas.RoundRect(vRect, GRoundSize, GRoundSize);
//      vText := Caption;
//      ACanvas.TextRect(vRect, vText, [tfSingleLine, tfCenter,tfVerticalCenter]);
//    //finally
//    //  SelectClipRgn(ACanvas.Handle, 0);
//    //  DeleteObject(vRgn);
//    //end;
//  end;
{$ENDREGION}
end;

procedure TCFButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  UpdateDirectUI;
end;

procedure TCFButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  UpdateDirectUI;
end;

procedure TCFButton.SetModalResult(Value: TModalResult);
begin
  if FModalResult <> Value then
  begin
    FModalResult := Value;
    case Value of  // 处理显示文本
      //mrNone;
      mrOk: Caption := '确 定';
      mrCancel: Caption := '取 消';
      mrAbort: Caption := '中 止';
      mrRetry: Caption := '重 试';
      mrIgnore: Caption := '忽 略';
      mrYes: Caption := '是';
      mrNo: Caption := '否';
      mrAll: Caption := '全 部';
      mrNoToAll: Caption := '全部否';
      mrYesToAll: Caption := '全部是';
      mrClose: Caption := '关 闭';
    end;
    UpdateDirectUI;
  end;
end;

end.
