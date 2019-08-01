unit CFMonthCalendar;

interface

uses
  Windows, Graphics, Classes, CFControl, Controls;

type
  TDisplayModel = (cdmDate, cdmMonth, cdmYear, cdmCentury);
  TPaintDateEvent = procedure(Sender: TObject; const ACanvas: TCanvas; const ADate: TDate; const ARect: TRect) of object;

  TCFCustomMonthCalendar = class(TCFTextControl)
  private
    /// <summary> 当前日期、日期下限、日期上限 </summary>
    FDate: TDateTime;
    FMinDate, FMaxDate: TDate;
    /// <summary> 不同显示模式下行高 </summary>
    FRowHeight,
    /// <summary> 不同显示械下列宽 </summary>
    FColWidth,
    /// <summary> 标题高度 </summary>
    FTitleBandHeight,
    /// <summary> 星期高度 </summary>
    FWeekBandHeight,
    /// <summary> 今天高度 </summary>
    FTodayBandHeight
      : Integer;
    FOnPaintDate: TPaintDateEvent;

    /// <summary>
    /// 根据日期范围判断日期是否超出，如超出，则修正为边界值
    /// </summary>
    /// <param name="ADate">日期</param>
    procedure CheckValidDate(var ADate: TDateTime);

    /// <summary>
    /// 判断指定的日期是否超出有效范围
    /// </summary>
    /// <param name="ADate">日期</param>
    /// <returns>True：超出范围</returns>
    function DateOutRang(const ADate: TDate): Boolean;

    /// <summary>
    /// 获取不同模式下鼠标点击处的日期、月份、年、10年区间
    /// </summary>
    /// <param name="X">横坐标</param>
    /// <param name="Y">纵坐标</param>
    /// <returns>日期or月份or年or10年区间</returns>
    function GetDateAt(const X, Y: Integer): TDate;

    /// <summary>
    /// 在日期模式下，返回指定日期相对控件客户区的区域
    /// </summary>
    /// <param name="ADate">日期</param>
    /// <returns>区域</returns>
    function GetDataRect(const ADate: TDate): TRect;
    procedure SetDisplayModelProperty(const AModel: TDisplayModel);
  protected
    FMoveDate: TDate;  // 为DateTimePicker下拉弹出时鼠标移动绘制放到此作用域下
    /// <summary> 时间模式 </summary>
    FDisplayModel: TDisplayModel;

    function GetDate: TDateTime;
    procedure SetDate(Value: TDateTime);
    procedure SetMaxDate(Value: TDate);
    procedure SetMinDate(Value: TDate);
    /// <summary> 设置边框 </summary>
    procedure AdjustBounds; override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure DrawControl(ACanvas: TCanvas); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;

    /// <summary> 为方便相关控件(如DateTimePicker)使用，Date实际为TDateTime类型 </summary>
    property Date: TDateTime read GetDate write SetDate;
    property MaxDate: TDate read FMaxDate write SetMaxDate;
    property MinDate: TDate read FMinDate write SetMinDate;
    property TitleBandHeight: Integer read FTitleBandHeight write FTitleBandHeight;
    property OnPaintDate: TPaintDateEvent read FOnPaintDate write FOnPaintDate;
  end;

  TCFMonthCalendar = class(TCFCustomMonthCalendar)
  published
    property Date;
    property MaxDate;
    property MinDate;
    property OnPaintDate;
    property OnChange;
  end;

implementation

uses
  SysUtils, DateUtils;

{ TCFCustomMonthCalendar }

procedure TCFCustomMonthCalendar.AdjustBounds;
var
  DC: HDC;
  vNewHeight, vNewWidth, vHeight: Integer;
begin
  //if not (csReading in ComponentState) then
  begin
    DC := GetDC(0);
    try
      Canvas.Handle := DC;
      Canvas.Font := Font;

      vHeight := Canvas.TextHeight('国');
      FTitleBandHeight := vHeight + Round(vHeight * 0.25);
      FWeekBandHeight := vHeight;
      FRowHeight := Round(vHeight + vHeight * 0.5);
      FColWidth := Canvas.TextWidth('中国');
      FTodayBandHeight := Round(vHeight + vHeight * 5);

      vNewHeight := FTitleBandHeight + FWeekBandHeight + 6 * FRowHeight + FTodayBandHeight + GPadding * 2;  // 共 12.5 个字体的高度
      if vNewHeight < Height then
        vNewHeight := Height;

      vNewWidth := 7 * FColWidth + GPadding * 2;
      if vNewWidth < Width then
        vNewWidth := Width;
      Canvas.Handle := 0;
    finally
      ReleaseDC(0, DC);
    end;
    SetBounds(Left, Top, vNewWidth, vNewHeight);
  end;
end;

function TCFCustomMonthCalendar.CanResize(var NewWidth, NewHeight: Integer): Boolean;
var
  vSize: TSize;
  vWidth, vHeight: Integer;
begin
  Result := True;

  vSize := Canvas.TextExtent('中国');

  vWidth := vSize.cx + GPadding * 2;
  vHeight := Round(vSize.cy * 12.5) ;
  if NewWidth < vWidth then
    NewWidth := vWidth
  else
    FColWidth := (Width - GPadding * 2) div 7;
  if NewHeight < vHeight then
    NewHeight := vHeight
  else
  begin
    vHeight := Round((Height - GPadding * 2) / 12.5);  // 每个字体预留的高度
    FRowHeight := Round(vHeight * 1.5);  // 共 12.5 个字体的高度, 一行又1.5倍的字体的高度
    FTitleBandHeight := Round(vHeight * 1.25);  // 标题的高度
    FWeekBandHeight := vHeight;
    FTodayBandHeight := Round(vHeight * 1.25);
  end;
end;

procedure TCFCustomMonthCalendar.CheckValidDate(var ADate: TDateTime);
begin
  if (FMaxDate <> 0.0) and (ADate > FMaxDate) then
    ADate := FMaxDate;
  if (FMinDate <> 0.0) and (ADate < FMinDate) then
    ADate := FMinDate;
end;

constructor TCFCustomMonthCalendar.Create(AOwner: TComponent);
begin
  inherited;
  FDisplayModel := cdmDate;
  SetDisplayModelProperty(FDisplayModel);
  FDate := Now;
  FMaxDate := 0.0;
  FMinDate := 0.0;
  Color := GBackColor;
end;

function TCFCustomMonthCalendar.DateOutRang(const ADate: TDate): Boolean;
begin
  Result := False;
  if (FMaxDate <> 0.0) and (ADate > FMaxDate) then
    Result := True;

  if not Result then
  begin
    if (FMinDate <> 0.0) and (ADate < FMinDate) then
      Result := True;
  end;
end;

procedure TCFCustomMonthCalendar.DrawControl(ACanvas: TCanvas);
var
  vLeft, vTop: Integer;
  vS: string;
  vRect: TRect;

  {$REGION '日期模式绘制'}

  {$REGION 'DrawModelTitle绘制标题和今天'}
  procedure DrawModelTitle(const ATitle: string);
  var
    vBmp: TBitmap; //vIcon: HICON;
  begin
    // 绘制标题年月
    vRect := Bounds(GPadding, GPadding, Width - GPadding, GPadding + FTitleBandHeight);
    DrawTextEx(ACanvas.Handle, PChar(ATitle), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);

    // 绘制标题上的左右三角按钮，实现翻月
    vBmp := TBitmap.Create;
    try
      vBmp.Transparent := True;
      //vIcon := LoadIcon(HInstance, 'DROPLEFT');
      vBmp.LoadFromResourceName(HInstance, 'DROPLEFT');
      ACanvas.Draw(GPadding, GPadding + Round((GPadding + FTitleBandHeight - GIconWidth) / 2), vBmp);
      //DrawIconEx(ACanvas.Handle, GPadding, GPadding + Round((GPadding + FTitleBandHeight - GIconWidth) / 2), vIcon,
      //  GIconWidth, GIconWidth, 0, 0, DI_NORMAL);
      vBmp.LoadFromResourceName(HInstance, 'DROPRIGHT');
      ACanvas.Draw(Width - GPadding - GIconWidth, GPadding + Round((GPadding + FTitleBandHeight - GIconWidth) / 2), vBmp);
      //DrawIconEx(ACanvas.Handle, Width - GPadding - GIconWidth, GPadding + Round((GPadding + FTitleBandHeight - GIconWidth) / 2), vIcon,
      //  GIconWidth, GIconWidth, 0, 0, DI_NORMAL);
    finally
      vBmp.Free;
    end;

    // 绘制今天
    vRect := Bounds(GPadding, Height - GPadding - Round(FTodayBandHeight * 0.8),  Width - 2 * GPadding, FTodayBandHeight);  // 控制今天界面区域偏低，在原来的高度基础上*0.8
    vS := '今天：' + FormatDateTime('YYYY/MM/DD', Now);
    DrawTextEx(ACanvas.Handle, PChar(vS), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);
  end;
  {$ENDREGION}

  {$REGION 'DrawDateModelWeek绘制周'}
  procedure DrawDateModelWeek;
  begin
    vLeft := GPadding;
    FWeekBandHeight := FRowHeight;
    vTop := GPadding + FTitleBandHeight + Round(FWeekBandHeight * 0.9) ;
    // 画星期和日期的间隔线
    ACanvas.Pen.Color := clBtnFace;
    ACanvas.MoveTo(GPadding, vTop);
    ACanvas.LineTo(Width - GPadding, vTop);

    // 画星期集合
    vTop := GPadding + FTitleBandHeight;
    vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
    DrawTextEx(ACanvas.Handle, PChar('周日'), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);

    Inc(vLeft, FColWidth);
    vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
    DrawTextEx(ACanvas.Handle, PChar('周一'), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);

    Inc(vLeft, FColWidth);
    vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
    DrawTextEx(ACanvas.Handle, PChar('周二'), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);

    Inc(vLeft, FColWidth);
    vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
    DrawTextEx(ACanvas.Handle, PChar('周三'), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);

    Inc(vLeft, FColWidth);
    vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
    DrawTextEx(ACanvas.Handle, PChar('周四'), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);

    Inc(vLeft, FColWidth);
    vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
    DrawTextEx(ACanvas.Handle, PChar('周五'), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);

    Inc(vLeft, FColWidth);
    vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
    DrawTextEx(ACanvas.Handle, PChar('周六'), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);
  end;
  {$ENDREGION}

  {$REGION 'DrawDateModel绘制日期和今天'}
  procedure DrawDateModel;
  var
    vStartDate, vEndDate: TDate;
    vWeekNo,  // 周几
    vCount  // 记录绘制了多少个日期
      : Byte;
  begin
    vStartDate := StartOfTheMonth(FDate);
    vEndDate := EndOfTheMonth(FDate);
    vWeekNo := DayOfTheWeek(vStartDate);
    vLeft := GPadding + vWeekNo * FColWidth;
    vTop := GPadding + FTitleBandHeight + FWeekBandHeight;
    while vStartDate < vEndDate do  // 控制不能超出本月的最后一天
    begin
      while vWeekNo < 7 do  // 一行能是从周日到周六
      begin
        vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
        if IsSameDay(vStartDate, Now) then  // 今天的底色用 clBtnFace 颜色标记
        begin
          ACanvas.Brush.Color := clBtnFace;
          ACanvas.FillRect(vRect);
          ACanvas.Brush.Style := bsClear;
        end;
        if IsSameDay(vStartDate, FMoveDate) then  // 是鼠标移动到的日期，高亮处理
        begin
          ACanvas.Brush.Color := GHightLightColor;
          ACanvas.FillRect(vRect);
          ACanvas.Brush.Style := bsClear;
        end;
        if IsSameDay(vStartDate, FDate) then  // 鼠标点下用 GBorderColor 颜色标记
        begin
          ACanvas.Brush.Style := bsClear;
          ACanvas.Pen.Color := GBorderColor;
          ACanvas.Rectangle(vRect);
        end;

        if Assigned(FOnPaintDate) then
          FOnPaintDate(Self, ACanvas, vStartDate, vRect)
        else
        begin
          if DateOutRang(vStartDate) then
            ACanvas.Font.Color := clMedGray
          else
            ACanvas.Font.Color := clBlack;
          vS := FormatDateTime('D', DateOf(vStartDate));
          DrawTextEx(ACanvas.Handle, PChar(vS), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);
        end;
        vStartDate := IncDay(vStartDate);
        if vStartDate >= vEndDate then
          Break;
        Inc(vWeekNo);
        vLeft := vLeft + FColWidth;
      end;
      vWeekNo := 0;
      vTop := vTop + FRowHeight;
      vLeft := GPadding;
    end;

    // 绘制上个月的最后几天(倒序)
    ACanvas.Font.Color := clGray;
    vStartDate := StartOfTheMonth(FDate);
    vWeekNo := DayOfTheWeek(vStartDate);
    if vWeekNo <> 7 then  // 不是周日
      vLeft := GPadding + (vWeekNo - 1)  * FColWidth
    else  // 是周日从上一行最后开始
      vLeft := Width - GPadding - FColWidth;
    vTop := GPadding + FTitleBandHeight + FWeekBandHeight;

    repeat
      vStartDate := IncDay(vStartDate, -1);
      Dec(vWeekNo);
      vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
      if IsSameDay(vStartDate, FMoveDate) then  // 是鼠标移动到的日期，高亮处理
      begin
        ACanvas.Brush.Color := GHightLightColor;
        ACanvas.FillRect(vRect);
        ACanvas.Brush.Style := bsClear;
      end;

      if Assigned(FOnPaintDate) then
        FOnPaintDate(Self, ACanvas, vStartDate, vRect)
      else
      begin
        if DateOutRang(vStartDate) then
          ACanvas.Font.Color := clMedGray
        else
          ACanvas.Font.Color := clBlack;
        vS := FormatDateTime('D', DateOf(vStartDate));
        DrawTextEx(ACanvas.Handle, PChar(vS), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);
      end;

      vLeft := vLeft - FColWidth;
    until vWeekNo = 0;

    // 计算绘制了多少个日期
    vStartDate := StartOfTheMonth(FDate);
    vCount := DayOfTheWeek(vStartDate);  // 上个月绘制了几天
    vCount := vCount + DaysInMonth(FDate);  // 当前月 + 上个月

    // 绘制下一个月的头几天
    vCount := 42 - vCount;
    if vCount > 7 then  // 还需要绘制2行
      vTop := GPadding + FTitleBandHeight + FWeekBandHeight + 4 * FRowHeight
    else // 还需要绘制1行
      vTop := GPadding + FTitleBandHeight + FWeekBandHeight + 5 * FRowHeight;
    vStartDate := EndOfTheMonth(FDate);  // 本月的最后一天
    vWeekNo := DayOfTheWeek(vStartDate);  // 本月的最后一天是星期几

    if vWeekNo < 6 then  // 本月的最后一天不是周六和周日（）
      vLeft := GPadding + (vWeekNo + 1) * FColWidth
    else
    if vWeekNo > 6 then  // 本月的最后一天是周日（也就是 vWeekNo = 7）
      vLeft := GPadding + FColWidth
    else  // 本月的最后一天是周六
      vLeft := GPadding;

    vStartDate := IncDay(vStartDate);
    Inc(vWeekNo);
    repeat  // 控制直到日期填满所有的日期位置
      vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);

      if IsSameDay(vStartDate, FMoveDate) then  // 是鼠标移动到的日期，高亮处理
      begin
        ACanvas.Brush.Color := GHightLightColor;
        ACanvas.FillRect(vRect);
        ACanvas.Brush.Style := bsClear;
      end;
      if Assigned(FOnPaintDate) then
        FOnPaintDate(Self, ACanvas, vStartDate, vRect)
      else
      begin
        if DateOutRang(vStartDate) then
          ACanvas.Font.Color := clMedGray
        else
          ACanvas.Font.Color := clBlack;
        vS := FormatDateTime('D', DateOf(vStartDate));
        DrawTextEx(ACanvas.Handle, PChar(vS), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);
      end;
      vStartDate := IncDay(vStartDate);
      Inc(vWeekNo);
      vLeft := vLeft + FColWidth;
      if vWeekNo = 7 then  // 周日则是下一行的开始
      begin
        vTop := vTop + FRowHeight;
        vLeft := GPadding;
        vWeekNo := 0;
      end;
      Dec(vCount);
    until vCount = 0;
  end;
  {$ENDREGION}

  {$ENDREGION}

  {$REGION '月份模式绘制'}

  {$REGION 'DrawManthModel绘制月份'}
  procedure DrawMonthModel;
  var
    vCount: Byte;
  const
    Month: array[1..12] of string = ('一月','二月', '三月', '四月', '五月', '六月', '七月', '八月', '九月', '十月', '十一月', '十二月');
  begin
    vLeft := GPadding;
    vTop := GPadding + FTitleBandHeight;

    for vCount := 1 to Length(Month) do  // 绘制 12 个月份
    begin
      vS := Month[vCount];
      vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);

      if MonthOf(FMoveDate) = vCount then  // 鼠标移动到的月份，高亮处理
      begin
        ACanvas.Brush.Color := GHightLightColor;
        ACanvas.FillRect(vRect);
        ACanvas.Brush.Style := bsClear;
      end;

      DrawTextEx(ACanvas.Handle, PChar(vS), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);

      if MonthOf(FDate) = vCount then  // 是鼠标选中的月份
      begin
        ACanvas.Brush.Style := bsClear;
        ACanvas.Pen.Color := GBorderColor;
        ACanvas.Rectangle(vRect);
      end;

      if vCount mod 4 <> 0 then  // 每行只能放4个月
        Inc(vLeft, FColWidth)
      else  // 每行的开始需要加行高
      begin
        vLeft := GPadding;
        vTop := vTop + FRowHeight;
      end;
    end;
  end;
  {$ENDREGION}

  {$ENDREGION}

  {$REGION '年限模式绘制'}

  {$REGION 'DrawYearModel绘制年限'}
  procedure DrawYearModel;
  var
    vStartYear, vEndYear, // 开始年限和结束年限
    vIndex  // 当前要绘制的年
      : Integer;
  begin
    // 绘制年限（开始年和结束年）
    vStartYear := YearOf(FDate) div 10 * 10 - 1;  // 显示年限的起始年，由于控件空间可绘制12个年，主要显示10年，所以减1从上一个10年的最后开始
    vEndYear := vStartYear + 11;  // 显示年限的结束年
    vIndex := 1;

    vLeft := GPadding;
    vTop := GPadding + FTitleBandHeight;
    while vStartYear <= vEndYear do
    begin
      if (vIndex = 1) or (vIndex = 12) then  // 第1个和最后1个
        ACanvas.Font.Color := clGray
      else
        ACanvas.Font.Color := clBackground;
      vS := IntToStr(vStartYear);
      vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
      // 绘制当前月日期
      if vStartYear > 1899 then  // 1899年以前的时间点用 IsSameday 时出现错误，这里做了限制
      begin
        if vStartYear = YearOf(FMoveDate) then // 鼠标移动到的年，高亮处理
        begin
          ACanvas.Brush.Color := GHightLightColor;
          ACanvas.FillRect(vRect);
          ACanvas.Brush.Style := bsClear;
        end;
        DrawTextEx(ACanvas.Handle, PChar(vS), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);
        if vStartYear = YearOf(FDate) then  // 当前选中年
        begin
          ACanvas.Brush.Style := bsClear;
          ACanvas.Pen.Color := GBorderColor;
          ACanvas.Rectangle(vRect);
        end;
      end;
      if (vIndex mod 4) = 0 then  // 是每行的最后一个年，下一年的需要加高，并且左边要设为边界线位置
      begin
        vLeft := GPadding;
        vTop := vTop + FRowHeight;
      end
      else  // 不是每行的最后一个年
      begin
        vLeft := vLeft + FColWidth;
      end;
      Inc(vStartYear);
      Inc(vIndex);
    end;
  end;
  {$ENDREGION}

  {$ENDREGION}

  {$REGION '世纪模式绘制'}

  {$REGION '绘制世纪DrawCenturyModel'}
  procedure DrawCenturyModel;
  var
    vStartYear, vEndYear, vCount: Integer;  // 开始年限和结束年限
  begin
    // 绘制年限（开始年和结束年）
    vStartYear := YearOf(FDate) div 100 * 100 - 10;  // 世纪模式的开始年
    vEndYear := vStartYear + 110;  // 世纪模式的结束年
    vCount := 1;

    vLeft := GPadding;
    vTop := GPadding + FTitleBandHeight;
    while vStartYear <= vEndYear do  // 绘制世纪中的年区间
    begin
      if (vCount = 1) or (vCount = 12) then  // 第一个年区间和最后一个年区间
        ACanvas.Font.Color := clGray
      else  // 本世纪的年区间
        ACanvas.Font.Color := clBackground;
      vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);

      if vStartYear > 1899 then  // 1899年以前的时间点用 IsSameday 时出现错误，这里做了限制
      begin
        if vStartYear = YearOf(FMoveDate) div 10 * 10 then  // 鼠标移动到的年区间，高亮处理
        begin
          ACanvas.Brush.Color := GHightLightColor;
          ACanvas.FillRect(vRect);
          ACanvas.Brush.Style := bsClear;
        end;
        // 区间年的起始
        vS := IntToStr(vStartYear) + '-';
        vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight div 2);
        DrawTextEx(ACanvas.Handle, PChar(vS), -1, vRect, DT_CENTER or DT_BOTTOM or DT_SINGLELINE, nil);
        // 区间年的起始
        vS := IntToStr(vStartYear + 9) + ' ';
        vRect := Bounds(vLeft, vTop + FRowHeight div 2, FColWidth, FRowHeight div 2);
        DrawTextEx(ACanvas.Handle, PChar(vS), -1, vRect, DT_CENTER or DT_TOP or DT_SINGLELINE, nil);
        if vStartYear = YearOf(FDate) div 10 * 10 then  // 当前年区间（十年是一区间，区间的开始算法是 year div 10 * 10）
        begin
          vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
          ACanvas.Brush.Style := bsClear;
          ACanvas.Pen.Color := GBorderColor;
          ACanvas.Rectangle(vRect);
        end;
      end;
      if (vCount mod 4) <> 0 then  // 不是每行的最后一个年区间（下一个年区间开始的时候不需要加高）
      begin
        vLeft := vLeft + FColWidth;
      end
      else  // 是每行的最后一个年区间，下一个年区间则是一个行的开始，需要加行高，并且左边的起点为边界线
      begin
        vLeft := GPadding;
        vTop := vTop + FRowHeight;
      end;
      Inc(vStartYear, 10);  // 一个年区间为 10 年，需要 + 10
      Inc(vCount);
    end;
  end;
  {$ENDREGION}
  {$ENDREGION}

begin
  inherited;
  if not HandleAllocated then Exit;

  vRect := ClientRect;
  ACanvas.Brush.Style := bsSolid;
  if BorderVisible then  // 边框
  begin
    if Self.Focused or (cmsMouseIn in MouseState) then
      ACanvas.Pen.Color := GBorderHotColor
    else
      ACanvas.Pen.Color := GBorderColor;

    ACanvas.Pen.Style := psSolid;
  end
  else
    ACanvas.Pen.Style := psClear;

  if RoundCorner > 0 then
    ACanvas.RoundRect(vRect, RoundCorner, RoundCorner)
  else
    ACanvas.Rectangle(vRect);

  SetDisplayModelProperty(FDisplayModel);  // 根据相应的模式设置相应

  // 绘制当前月日期
  if YearOf(FDate) < 1900 then Exit;  // 1899年以前的时间点用 IsSameday 时出现错误，这里做了限制

  case FDisplayModel of
    cdmDate:
      begin
        {$REGION '日期模式绘制'}
        // 处理背景月份绘制
        ACanvas.Font.Size := FRowHeight * 4;  //设置描绘文字的大小
        ACanvas.Pen.Color := clBtnFace;  // 设置描绘的颜色
        vS := FormatDateTime('M', FDate);  // 描绘的内容
        vRect.Top := GPadding + FTitleBandHeight + FWeekBandHeight;
        BeginPath(ACanvas.Handle);
        ACanvas.Brush.Style := bsClear;
        DrawTextEx(ACanvas.Handle, PChar(vS), -1, vRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, nil);
        EndPath(ACanvas.Handle);
        StrokePath(ACanvas.Handle);  // 描绘路径轮廓
        // 绘制日期模式各部分
        ACanvas.Font := Font;
        vS := FormatDateTime('YYYY年MM月', FDate);
        DrawModelTitle(vS);  // 绘制标题
        DrawDateModelWeek;   // 绘制周
        DrawDateModel;       // 绘制日期
        {$ENDREGION}
      end;

    cdmMonth:
      begin
        {$REGION '月份模式绘制'}
        ACanvas.Font := Font;
        vS := FormatDateTime('YYYY年', FDate);
        DrawModelTitle(vS);  // 绘制标题
        DrawMonthModel;  // 绘制月份
        {$ENDREGION}
      end;

    cdmYear:
      begin
        {$REGION '年模式绘制'}
        ACanvas.Font := Font;
        vS := FormatDateTime('YYYY', FDate);
        vS := IntToStr(StrToInt(vS) div 10 * 10) + '-' + IntToStr((StrToInt(vS) div 10 + 1) * 10 - 1);
        DrawModelTitle(vS);  // 绘制标题
        DrawYearModel;  // 绘制年限
        {$ENDREGION}
      end;

    cdmCentury:
      begin
        {$REGION '世纪模式绘制'}
        ACanvas.Font := Font;
        vS := FormatDateTime('YYYY', FDate);
        vS := IntToStr(StrToInt(vS) div 100 * 100) + '-' + IntToStr((StrToInt(vS) div 100 + 1) * 100 - 1);
        DrawModelTitle(vS);  // 绘制标题
        DrawCenturyModel;  // 绘制世纪
        {$ENDREGION}
      end;
  end;
end;

function TCFCustomMonthCalendar.GetDate: TDateTime;
begin
  CheckValidDate(FDate);
  Result := FDate;
end;

function TCFCustomMonthCalendar.GetDataRect(const ADate: TDate): TRect;
var
  vStartDate, vEndDate: TDate;
  vLeft, vTop, vWeekNo, vCount: Integer;
  vIStartYear, vIEndYear: Integer;
begin
  case FDisplayModel of
    cdmDate:
      begin
        {$REGION '获取日期模式下日期区域'}
        // 在当前月中点击日期
        vStartDate := StartOfTheMonth(FDate);
        vEndDate := EndOfTheMonth(FDate);
        vWeekNo := DayOfTheWeek(vStartDate);
        vLeft := GPadding + vWeekNo * FColWidth;
        vTop := GPadding + FTitleBandHeight + FWeekBandHeight;
        // 判断点击是否在当月日期
        while vStartDate < vEndDate do  // 控制日期范围
        begin
          while vWeekNo < 7 do  // 日期数据
          begin
            if IsSameDay(vStartDate, ADate) then  // 鼠标移动到的日期区域
            begin
              Result:= Bounds(vLeft, vTop, FColWidth, FRowHeight);
              Exit;
            end;
            vStartDate := IncDay(vStartDate);
            if vStartDate >= vEndDate then  // 控制日期范围
              Break;
            vLeft := vLeft + FColWidth;
            Inc(vWeekNo);
          end;
          vWeekNo := 0;
          vTop := vTop + FRowHeight;  // + 行高
          vLeft := GPadding;
        end;

        // 绘制上个月的后几天
        vStartDate := StartOfTheMonth(FDate);
        vWeekNo := DayOfTheWeek(vStartDate);
        if vWeekNo <> 7 then  // 不是周日
          vLeft := GPadding + (vWeekNo - 1) * FColWidth
        else  // 是周日从上一行最后开始
          vLeft := Width - GPadding - FColWidth;
        vTop := GPadding + FTitleBandHeight + FWeekBandHeight;

        repeat
          vStartDate := IncDay(vStartDate, -1);
          Dec(vWeekNo);
          if IsSameDay(vStartDate, ADate) then  // 鼠标移动到的日期区域
          begin
            Result:= Bounds(vLeft, vTop, FColWidth, FRowHeight);
            Exit;
          end;
          vLeft := vLeft - FColWidth;
        until vWeekNo = 0;

        // 计算绘制了多少个日期
        vStartDate := StartOfTheMonth(FDate);
        vCount := DayOfTheWeek(vStartDate);
        vCount := vCount + DaysInMonth(FDate);

        // 绘制下一个月的头几天
        vCount := 42 - vCount;
        if vCount > 7 then  // 还需要绘制2行
          vTop := GPadding + FTitleBandHeight + FWeekBandHeight + 4 * FRowHeight
        else // 还需要绘制1行
          vTop := GPadding + FTitleBandHeight + FWeekBandHeight + 5 * FRowHeight;
        vStartDate := EndOfTheMonth(FDate);
        vWeekNo := DayOfTheWeek(vStartDate);

        if vWeekNo < 6 then  // 本月的最后一天不是周六和周日（其他星期）
          vLeft := GPadding + (vWeekNo + 1) * FColWidth
        else
        if vWeekNo > 6 then  // 本月的最后一天是周日（也就是 vWeekNo = 7）
          vLeft := GPadding + FColWidth
        else  // 本月的最后一天是周六
          vLeft := GPadding;

        vStartDate := IncDay(vStartDate);
        Inc(vWeekNo);
        repeat
          if IsSameDay(vStartDate, ADate) then  // 鼠标移动到的日期区域
          begin
            Result:= Bounds(vLeft, vTop, FColWidth, FRowHeight);
            Exit;
          end;
          vStartDate := IncDay(vStartDate);
          Inc(vWeekNo);
          vLeft := vLeft + FColWidth;
          if vWeekNo = 7 then // 如果是周日，则是日期行的开始
          begin
            vTop := vTop + FRowHeight;
            vLeft := GPadding;
            vWeekNo := 0;
          end;
          Dec(vCount);
        until vCount = 0;
        {$ENDREGION}
      end;

    cdmMonth:
      begin
        {$REGION '获取月份模式下月份区域'}
        vLeft := GPadding;
        vTop := GPadding + FTitleBandHeight;
        for vCount := 1 to 12 do  //  从1月到12月进行遍历 找出选中月
        begin
          if MonthOf(ADate) = vCount then  // 鼠标移动到的月份区域
          begin
            Result := Bounds(vLeft, vTop, FColWidth, FRowHeight);
            Exit;
          end;
          if vCount mod 4 <> 0 then  // 每行只能放4个月
            Inc(vLeft, FColWidth)
          else  // 每行的最后一个月的下一个月要进行加行高
          begin
            vLeft := GPadding;
            vTop := vTop + FRowHeight;
          end;
        end;
        {$ENDREGION}
      end;

    cdmYear:
      begin
        {$REGION '获取年模式下年区域'}
        // 绘制年限（开始年和结束年）
        vIStartYear := YearOf(FDate) div 10 * 10 - 1;  // 开始年
        vIEndYear := vIStartYear + 11;  // 结束年
        vCount := 1;

        vLeft := GPadding;
        vTop := GPadding + FTitleBandHeight;
        while vIStartYear <= vIEndYear do  // 找出鼠标移动到的年
        begin
          if YearOf(ADate) = vIStartYear then  // 鼠标移动到的年区域
          begin
            Result := Bounds(vLeft, vTop, FColWidth, FRowHeight);
            Exit;
          end;
          if (vCount mod 4) <> 0 then  // 不是每行的最后一个年
          begin
            vLeft := vLeft + FColWidth;
          end
          else  // 是每行的最后一个年，则下一年的要加行高
          begin
            vLeft := GPadding;
            vTop := vTop + FRowHeight;
          end;
          Inc(vIStartYear);
          Inc(vCount);
        end;
        {$ENDREGION}
      end;

    cdmCentury:
      begin
        {$REGION '获取世纪模式下年区间区域'}
        vIStartYear := YearOf(FDate) div 100 * 100 - 10;  // 记录第一个年限区间的开始年
        vIEndYear := vIStartYear + 110;  // 记录最后一个年限区间中的开始年
        vCount := 1;

        vLeft := GPadding;
        vTop := GPadding + FTitleBandHeight;
        while vIStartYear <= vIEndYear do  // 找到鼠标移动到的年区间
        begin
          if vIStartYear = YearOf(ADate) div 10 * 10 then  // 鼠标移动到的年区间区域
          begin
            Result := Bounds(vLeft, vTop, FColWidth, FRowHeight);
            Exit;
          end;
          if (vCount mod 4) <> 0 then  // 不是每行的每行的最后一个年限区间
          begin
            vLeft := vLeft + FColWidth;
          end
          else  // 是每行的最后一个年限区间，下一个年限区间要加行高
          begin
            vLeft := GPadding;
            vTop := vTop + FRowHeight;
          end;
          Inc(vIStartYear, 10);
          Inc(vCount);
        end;
        {$ENDREGION}
      end;
  end;
end;

function TCFCustomMonthCalendar.GetDateAt(const X, Y: Integer): TDate;
var
  vStartDate, vEndDate: TDate;
  vS: string;
  vWeekNo: Byte;
  vLeft, vTop, vCount: Integer;
  vRect: TRect;
  vIStartYear, vIEndYear: Integer;
begin
  Result := 0;
  case FDisplayModel of
    cdmDate:
      begin
        {$REGION '获取所在日期时间'}
        // 在当前月中点击日期
        vStartDate := StartOfTheMonth(FDate);
        vEndDate := EndOfTheMonth(FDate);
        vWeekNo := DayOfTheWeek(vStartDate);
        vLeft := GPadding + vWeekNo * FColWidth;
        vTop := GPadding + FTitleBandHeight + FWeekBandHeight;
        // 判断点击是否在当月日期
        while vStartDate < vEndDate do  // 控制日期范围
        begin
          while vWeekNo < 7 do  // 日期数据
          begin
            vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
            if PtInRect(vRect, Point(X, Y)) then  // 鼠标点击日期所在日期区域
            begin
              Result := vStartDate;
              Exit;
            end;
            vStartDate := IncDay(vStartDate);
            if vStartDate >= vEndDate then
              Break;
            vLeft := vLeft + FColWidth;
            Inc(vWeekNo);
          end;
          vWeekNo := 0;
          vTop := vTop + FRowHeight;  // + 行高
          vLeft := GPadding;
        end;

        // 判断是否点击在上个月的后几天
        vStartDate := StartOfTheMonth(FDate);
        vWeekNo := DayOfTheWeek(vStartDate);
        if vWeekNo <> 7 then  // 不是周日
          vLeft := GPadding + (vWeekNo - 1) * FColWidth
        else  // 是周日从上一行最后开始
          vLeft := Width - GPadding - FColWidth;
        vTop := GPadding + FTitleBandHeight + FWeekBandHeight;

        repeat
          vStartDate := IncDay(vStartDate, -1);
          Dec(vWeekNo);
          vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
          if PtInRect(vRect, Point(X, Y)) then  // 鼠标点击日期所在日期区域
          begin
            Result := vStartDate;
            Exit;
          end;
          vLeft := vLeft - FColWidth;
        until vWeekNo = 0;

        // 计算绘制了多少个日期
        vStartDate := StartOfTheMonth(FDate);
        vCount := DayOfTheWeek(vStartDate);
        vCount := vCount + DaysInMonth(FDate);
        // 判断是否点击在下一个月的头几天
        vCount := 42 - vCount;
        if vCount > 7 then  // 还需要绘制2行
          vTop := GPadding + FTitleBandHeight + FWeekBandHeight + 4 * FRowHeight
        else // 还需要绘制1行
          vTop := GPadding + FTitleBandHeight + FWeekBandHeight + 5 * FRowHeight;
        vStartDate := EndOfTheMonth(FDate);
        vWeekNo := DayOfTheWeek(vStartDate);

        if vWeekNo < 6 then  // 本月的最后一天不是周六和周日（其他星期）
          vLeft := GPadding + (vWeekNo + 1) * FColWidth
        else
        if vWeekNo > 6 then  // 本月的最后一天是周日（也就是 vWeekNo = 7）
          vLeft := GPadding + FColWidth
        else  // 本月的最后一天是周六
          vLeft := GPadding;

        vStartDate := IncDay(vStartDate);
        Inc(vWeekNo);
        repeat
          vS := FormatDateTime('D', DateOf(vStartDate));
          vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
          if PtInRect(vRect, Point(X, Y)) then  // 鼠标点击日期所在日期区域
          begin
            Result := vStartDate;
            Exit;
          end;
          vStartDate := IncDay(vStartDate);
          Inc(vWeekNo);
          vLeft := vLeft + FColWidth;
          if vWeekNo = 7 then // 如果是周日，则是日期行的开始
          begin
            vTop := vTop + FRowHeight;
            vLeft := GPadding;
            vWeekNo := 0;
          end;
          Dec(vCount);
        until vCount = 0;
        {$ENDREGION}
      end;

    cdmMonth:
      begin
        {$REGION '获取月份所在时间'}
        vLeft := GPadding;
        vTop := GPadding + FTitleBandHeight;
        for vCount := 1 to 12 do  //  从1月到12月进行遍历 找出选中月
        begin
          vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
          if PtInRect(vRect, Point(X, Y)) then  // 是鼠标点击月
          begin
            Result := RecodeMonth(FDate, vCount);
            Break;
          end;
          if vCount mod 4 <> 0 then  //
            Inc(vLeft, FColWidth)
          else
          begin
            vLeft := GPadding;
            vTop := vTop + FRowHeight;
          end;
        end;
        {$ENDREGION}
      end;

    cdmYear:
      begin
        {$REGION '获取年限所在时间'}
        // 绘制年限（开始年和结束年）
        vIStartYear := YearOf(FDate) div 10 * 10 - 1;  // 开始年
        vIEndYear := vIStartYear + 11;  // 结束年
        vCount := 1;

        vLeft := GPadding;
        vTop := GPadding + FTitleBandHeight;
        while vIStartYear <= vIEndYear do  // 找出鼠标点击年
        begin
          if vIStartYear > 1899 then
          begin
            vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
            if PtInRect(vRect, Point(X, Y)) then  // 得到鼠标点击年
            begin
              Result := RecodeYear(FDate, vIStartYear);
              Exit;
            end;
          end
          else
            Result := StrToDate('1899/ 1/1');
          if (vCount mod 4) <> 0 then  // 不是每行的最后一个年
          begin
            vLeft := vLeft + FColWidth;
          end
          else  // 是每行的最后一个年，则下一年的要加行高
          begin
            vLeft := GPadding;
            vTop := vTop + FRowHeight;
          end;
          Inc(vIStartYear);
          Inc(vCount);
        end;
        {$ENDREGION}
      end;

    cdmCentury:
      begin
        {$REGION '获取世纪所在时间'}
        vIStartYear := YearOf(FDate) div 100 * 100 - 10;  // 记录第一个年限区间的开始年
        vIEndYear := vIStartYear + 110;  // 记录最后一个年限区间中的开始年
        vCount := 1;

        vLeft := GPadding;
        vTop := GPadding + FTitleBandHeight;
        while vIStartYear <= vIEndYear do  // 找到鼠标点击年区间
        begin
          if vIStartYear > 1899 then
          begin
            vRect := Bounds(vLeft, vTop, FColWidth, FRowHeight);
            if PtInRect(vRect, Point(X, Y)) then  // 找到鼠标点击年区间
            begin
              Result := RecodeYear(FDate, (StrToInt(FormatDateTime('YYYY', FDate)) div 100 * 100 - 10) + (vCount - 1) * 10  + StrToInt(FormatDateTime('YYYY', FDate)) mod 10);
              Exit;
            end;
          end
          else
            Result := StrToDate('1899/1/1');
          if (vCount mod 4) <> 0 then  // 不是每行的每行的最后一个年限区间
          begin
            vLeft := vLeft + FColWidth;
          end
          else  // 是每行的最后一个年限区间，下一个年限区间要加行高
          begin
            vLeft := GPadding;
            vTop := vTop + FRowHeight;
          end;
          Inc(vIStartYear, 10);
          Inc(vCount);
        end;
        {$ENDREGION}
      end;
  end;
end;

procedure TCFCustomMonthCalendar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vDate, vOldDate: TDate;
  vRect: TRect;
begin
  inherited;
  if FDisplayModel = cdmDate then  // 日期模式日期区域
    vRect := Bounds(GPadding, GPadding + FTitleBandHeight + FWeekBandHeight, 7 * FColWidth, 6 * FRowHeight)
  else
    vRect := Bounds(GPadding, GPadding + FTitleBandHeight, 4 * FColWidth, 3 * FRowHeight);

  if PtInRect(vRect, Point(X, Y)) then  // 鼠标点击在数据区
  begin
    vDate := GetDateAt(X, Y);  // 传入数据坐标
    if DateOutRang(vDate) then Exit;

    if YearOf(vDate) > 1899 then
    begin
      if vDate <> 0 then  // 返回鼠标所指时间不为 0
      begin
        vOldDate := FDate;
        FDate := vDate;
        if FDisplayModel > cdmDate then  // 不是日期模式
          Dec(FDisplayModel);

        UpdateDirectUI;
        // 由于日历点击首尾其他月时，切换到其他月整个日历所以不能只更新原选择日期部分
        // 也就是说，只有在当前月不同日期切换时，才需要仅更新上次选择中日期
        // 故暂时全部整体更新
        {if FDisplayModel > cdmDate then  // 不是日期模式
        begin
          Dec(FDisplayModel);
          UpdateDirectUI;
        end
        else  // 日期模式
        begin
          // 清除原来区域
          vRect := GetDataRect(vOldDate);
          UpdateDirectUI(vRect);
          // 重绘新区域
          vRect := GetDataRect(FDate);
          UpdateDirectUI(vRect);
        end;}
      end;

      Exit;
    end;
  end;

  // 回到今天区域
  if FDisplayModel = cdmDate then  // 日期模式日期区域
    vRect := Bounds(GPadding, GPadding + FTitleBandHeight + FWeekBandHeight + 6 * FRowHeight, 7 * FColWidth, FTodayBandHeight)
  else  // 其他模式下的今天区
    vRect := Bounds(GPadding, GPadding + FTitleBandHeight + 3 * FRowHeight, 4 * FColWidth, FTodayBandHeight);

  if PtInRect(vRect, Point(X, Y)) then  // 鼠标点击在今天区
  begin
    Date := Today;
    UpdateDirectUI;

    Exit;
  end;

  // 标题左三角区域内
  vRect := Bounds(2 * GPadding, GPadding + (FTitleBandHeight - GIconWidth) div 2, 2 * GIconWidth, 2 * GIconWidth);  // 三角区范围设置高和宽为2倍的图标宽度
  if PtInRect(vRect, Point(X, Y)) then
  begin
    case FDisplayModel of
      cdmDate:
        begin
          Date := IncMonth(FDate, -1);
          if YearOf(FDate) < 1900 then  // 1899年以前的时间点用 IsSameday 时出现错误，这里做了限制
          begin
            Date := IncMonth(FDate, 1);
            Exit;
          end;
          UpdateDirectUI;

          Exit;
        end;

      cdmMonth:
        begin
          Date := IncYear(FDate, -1);
          if YearOf(FDate) < 1900 then  // 1899年以前的时间点用 IsSameday 时出现错误，这里做了限制
          begin
            Date := IncYear(FDate, 1);
            Exit;
          end;
          UpdateDirectUI;

          Exit;
        end;

      cdmYear:
        begin
          Date := IncYear(FDate, - 10);
          if YearOf(FDate) < 1900 then  // 1899年以前的时间点用 IsSameday 时出现错误，这里做了限制
          begin
            Date := IncYear(FDate, 10);  // 保持原来的日期
            Exit;
          end;
          UpdateDirectUI;

          Exit;
        end;

      cdmCentury:
        begin
          Date := IncYear(FDate, - 100);
          if YearOf(FDate) < 1900 then  // 1899年以前的时间点用 IsSameday 时出现错误，这里做了限制
          begin
            Date := IncYear(FDate, 100);  // 保持原来的日期
            Exit;
          end;
          UpdateDirectUI;

          Exit;
        end;
    end;
  end;

  // 标题右三角区域内
  vRect := Bounds(Width - GPadding - 2 * GIconWidth, GPadding + (FTitleBandHeight - GIconWidth) div 2, 2 * GIconWidth, 2 * GIconWidth);
  if PtInRect(vRect, Point(X, Y)) then
  begin
    case FDisplayModel of
      cdmDate:
        begin
          Date := IncMonth(FDate);
          UpdateDirectUI;

          Exit;
        end;

      cdmMonth:
        begin
          Date := IncYear(FDate, 1);
          UpdateDirectUI;
          Exit;
        end;
      cdmYear:
        begin
          Date := IncYear(FDate, 10);
          UpdateDirectUI;

          Exit;
        end;

      cdmCentury:
        begin
          Date := IncYear(FDate, 100);
          UpdateDirectUI;

          Exit;
        end;
    end;
  end;
end;

procedure TCFCustomMonthCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vRect: TRect;
  vDate, vOldDate: TDate;
begin
  inherited;
  if FDisplayModel = cdmDate then  // 日期模式日期区域
    vRect := Bounds(GPadding, GPadding + FTitleBandHeight + FWeekBandHeight, 7 * FColWidth, 6 * FRowHeight)
  else
    vRect := Bounds(GPadding, GPadding + FTitleBandHeight, 4 * FColWidth, 3 * FRowHeight);

  if PtInRect(vRect, Point(X, Y)) then  // 鼠标点击在数据区
  begin
    vDate := GetDateAt(X, Y);  // 得到鼠标处的日期
    if DateOutRang(vDate) then Exit;

    if (vDate <> 0) and not IsSameDay(vDate, FMoveDate) then  // 返回鼠标所指日期不为 0
    begin
      vOldDate := FMoveDate;
      FMoveDate := vDate;
      // 清除原来区域
      vRect := GetDataRect(vOldDate);
      UpdateDirectUI(vRect);
      // 重绘新区域
      vRect := GetDataRect(FMoveDate);
      UpdateDirectUI(vRect);
    end;
  end;
end;

procedure TCFCustomMonthCalendar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vRect: TRect;
begin
  inherited;
  // 标题区
  vRect := Rect(GPadding + GIconWidth, GPadding, Width - GPadding - GIconWidth, GPadding + FTitleBandHeight);
  if PtInRect(vRect, Point(X, Y)) then  // 在标题区
  begin
    if FDisplayModel < cdmCentury then
    begin
      Inc(FDisplayModel);
      UpdateDirectUI;
    end;
  end;
end;

procedure TCFCustomMonthCalendar.SetDate(Value: TDateTime);
begin
  if FDate <> Value then
  begin
    FDate := Value;
    CheckValidDate(FDate);
    UpdateDirectUI;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TCFCustomMonthCalendar.SetDisplayModelProperty(const AModel: TDisplayModel);
var
  vWidth, vHeight: Integer;
begin
  vHeight := Round((Height - GPadding  * 2) / 12.5);  // 每个字体预留的高度
  FRowHeight := Round(vHeight * 1.5);  // 共 12.25 个字体的高度, 一行又1.5倍的字体的高度
  FTitleBandHeight := Round(vHeight * 1.25);  // 标题的高度
  FWeekBandHeight := vHeight;
  FTodayBandHeight := Round(vHeight * 1.25);
  FColWidth := (Width - GPadding * 2) div 7;
  if AModel <> cdmDate then
  begin
    FColWidth := (Width - GPadding * 2) div 4;
    FRowHeight := (FWeekBandHeight + 6 * FRowHeight) div 3;
  end;
end;

procedure TCFCustomMonthCalendar.SetMaxDate(Value: TDate);
begin
  if FMaxDate <> Value then
  begin
    if Value < FMinDate then Exit;

    FMaxDate := Value;
    CheckValidDate(FDate);
  end;
end;

procedure TCFCustomMonthCalendar.SetMinDate(Value: TDate);
begin
  if FMinDate <> Value then
  begin
    if Value > FMaxDate then Exit;

    FMinDate := Value;
    CheckValidDate(FDate);
  end;
end;

end.

