unit CFCNMonthCalendar;

interface

uses Windows, Classes, CFMonthCalendar, Graphics;

type
  TCFCNMonthCalendar = class(TCFCustomMonthCalendar)
  private
    FOnPaintCNDate: TPaintDateEvent;

    /// <summary>
    /// 获取相应的农历日期
    /// </summary>
    /// <param name="ADate">阳历日期</param>
    /// <returns>农历</returns>
    function GetLunarDate(const ADate: TDate): string;

    /// <summary>
    /// 进行绘制农历和节气
    /// </summary>
    /// <param name="Sender"></param>
    /// <param name="ACanvas"></param>
    /// <param name="ADate">阳历日期</param>
    /// <param name="ARect">指定日期区域</param>
    procedure DoOnPaintDate(Sender: TObject; const ACanvas: TCanvas; const ADate: TDate; const ARect: TRect);

  protected
    /// <summary> 利用画布绘制 </summary>
    procedure DrawControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnPaintCNDate: TPaintDateEvent read FOnPaintCNDate write FOnPaintCNDate;
    property Date;
    property MaxDate;
    property MinDate;
  end;

implementation

uses
  hxCalendar, DateUtils, SysUtils;

{ TCFCNMonthCalendar }

var
  FJQ: Boolean;  // 标记是否为节气

constructor TCFCNMonthCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.OnPaintDate := DoOnPaintDate;
end;

destructor TCFCNMonthCalendar.Destroy;
begin

  inherited;
end;

procedure TCFCNMonthCalendar.DrawControl(ACanvas: TCanvas);
begin
  inherited;

end;

function TCFCNMonthCalendar.GetLunarDate(const ADate: TDate): string;
var
  vThxCalendar: ThxCalendar;
  vHzDate: THzDate;
begin
  FJQ := False;
  vHzDate := vThxCalendar.ToLunar(ADate);  // 得到农历
  if vThxCalendar.GetJQ(ADate) <> '' then  // 节气存在返回节气
  begin
    Result := vThxCalendar.GetJQ(ADate);
    FJQ := True;
  end
  else
    Result := vThxCalendar.FormatLunarDay(vHzDate.Day);
end;

procedure TCFCNMonthCalendar.DoOnPaintDate(Sender: TObject; const ACanvas: TCanvas;
  const ADate: TDate; const ARect: TRect);
var
  vS: string;
  vOldFontSize: Byte;
  vRect: TRect;
  vOldFontColor: TColor;
  vHeight: Integer;
begin
  vRect := ARect;
  vHeight := ARect.Bottom - ARect.Top;
  vRect.Bottom := vRect.Bottom - Round(vHeight * 0.55);  // 定义放日期的区域，乘0.55是为了公历字体大于农历
  vS := FormatDateTime('D', DateOf(ADate));
  DrawTextEx(ACanvas.Handle, PChar(vS), -1, vRect, DT_CENTER or DT_BOTTOM or DT_SINGLELINE, nil);

  // 记住之前相关的画笔字体大小和颜色
  vOldFontSize := ACanvas.Font.Size;
  vOldFontColor := ACanvas.Font.Color;

  // 得到相应的农历和节气并进行显示
  try
    vS := GetLunarDate(ADate);
    ACanvas.Font.Size := Round(vOldFontSize * 0.7);
    if FJQ then  // 节气显示为红色
      ACanvas.Font.Color := clRed
    else  // 显示农历
      ACanvas.Font.Color := clGrayText;
    // 输出对应的农历
    vRect := Rect(ARect.Left, ARect.Top + Round(vHeight * 0.55), ARect.Right, ARect.Bottom);  // 定义农历文字的区域
    DrawTextEx(ACanvas.Handle, PChar(vS), -1, vRect, DT_CENTER or DT_TOP or DT_SINGLELINE, nil);
  finally
    // 恢复画笔的字体的大小和颜色
    ACanvas.Font.Size := vOldFontSize;
    ACanvas.Font.Color := vOldFontColor;
  end;
  if Assigned(FOnPaintCNDate) then
    FOnPaintCNDate(Self, ACanvas, ADate, ARect);
end;

end.
