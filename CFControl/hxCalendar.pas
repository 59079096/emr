unit hxCalendar;

interface

uses Classes, Controls, Messages, Windows, Forms, Graphics, StdCtrls,
Grids, SysUtils,DateUtils;

type
  TDayOfWeek = 0..6;
  TDroppedCell = procedure(Sender: TObject; ACol, ARow: LongInt; var Value: string) of object;
  TCellDragOver = procedure(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean) of object;
  TCalendarStrings = array[0..6, 0..6] of TStringList;

  THzDate = record //农历日期
    Year: integer;
    Month: integer;
    Day: integer;
    isLeap: Boolean; //闰月
  end;

  TGzDate = record //干支日期
    Year: integer;
    Month: integer;
    Day: integer;
  end;

  ThxCalendar = class(TCustomGrid)
  private
    FDate: TDate;
    FViewDate: TDate;
    //FCalColors: TLssCalColors;
    FYear: word;
    FMonth: word;
    FDay: word;
    FCalStrings: TCalendarStrings;
    FOnDroppedCell: TDroppedCell;
    FOnCellDragOver: TCellDragOver;
    FMonthOffset: Integer;
    FOnChange: TNotifyEvent;
    FReadOnly: Boolean;
    FStartOfWeek: TDayOfWeek;
    FUpdating: Boolean;
    FUseCurrentDate: Boolean;
    function GetCellText(ACol, ARow: Integer): string;
    function GetDateElement(Index: Integer): Integer;
    procedure SetCalendarDate(Value: TDate);
    procedure SetDateElement(Index: Integer; Value: Integer);
    procedure SetStartOfWeek(Value: TDayOfWeek);
    procedure SetUseCurrentDate(Value: Boolean);
    function StoreCalendarDate: Boolean;
    procedure SetCellString(ACol, ARow, ADay: Integer; Value: string); virtual;
  protected
    { Protected declarations }

    procedure AcceptDropped(Sender, Source: TObject; X, Y: integer);
    procedure CellDragOver(Sender, Source: TObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
    procedure Change; dynamic;
    procedure ChangeMonth(Delta: Integer);
    procedure Click; override;
    function DaysPerMonth(AYear, AMonth: Integer): Integer; virtual;
    function DaysThisMonth: Integer; virtual;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    function IsLeapYear(AYear: Integer): Boolean; virtual;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;

    //返回农历 y年的总天数
    function DaysOfLunarYear(y: integer): integer;
    //返回农历 y年闰月的天数
    function daysofleapMonth(y: integer): integer;
    //返回农历 y年闰哪个月 1-12 , 没闰返回 0
    function leapMonth(y: integer): integer;
    //返回农历 y年m月的总天数
    function Daysofmonth(y, m: integer): integer;
    //算出农历, 传入公历日期, 返回农历日期
    function ToLunar(TheDate: TDate): THzDate;
    //传入 offset 返回干支, 0=甲子
    function cyclical(num: integer): string;
    //算出公历, 传入农历日期控件, 返回公历
    function ToGreg(objDate: THzDate): TDate;
    //检查农历日期是否合法
    function ChkHzDate(objDate: THzDate): Boolean;
    //某年的第n个节气为几日(从0小寒起算)
    function sTerm(y, n: integer): TDateTime;
    //求年柱，月柱，日柱(年，月为农历数字，TheDate为当天的公历日期)
    function GetGZ(y, m: integer; TheDate: TDate): TGzDate;
    //取汉字日期
    function FormatLunarDay(day:integer): string;
    //汉字月份
    function FormatLunarMonth(month:integer;isLeap:boolean): string;
    //汉字年份
    function FormatLunarYear(year:integer): string;
    // 取得指定日期的节气
    function GetJQ(TheDate: TDate): string;
    // 取得新历节日
    function GetsFtv(TheDate: TDate): string;
    // 取得农历节日
    function GetlFtv(TheDate: ThzDate): string;


    property CalendarDate: TDate read FDate write SetCalendarDate stored StoreCalendarDate;

    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);


    property CellText[ACol, ARow: Integer]: string read GetCellText;
    procedure NextMonth;
    procedure NextYear;
    procedure PrevMonth;
    procedure PrevYear;
    procedure UpdateCalendar; virtual;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property Day: Integer index 3 read GetDateElement write SetDateElement stored False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property GridLineWidth;
    property Month: Integer index 2 read GetDateElement write SetDateElement stored False;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property ShowHint;
    property StartOfWeek: TDayOfWeek read FStartOfWeek write SetStartOfWeek;
    property TabOrder;
    property TabStop;
    property UseCurrentDate: Boolean read FUseCurrentDate write SetUseCurrentDate default True;
    property Visible;
    property Year: Integer index 1 read GetDateElement write SetDateElement stored False;
    property OnClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

const
  lunarInfo: array[0..200] of WORD =(
    $4bd8,$4ae0,$a570,$54d5,$d260,$d950,$5554,$56af,$9ad0,$55d2,
    $4ae0,$a5b6,$a4d0,$d250,$d295,$b54f,$d6a0,$ada2,$95b0,$4977,
    $497f,$a4b0,$b4b5,$6a50,$6d40,$ab54,$2b6f,$9570,$52f2,$4970,
    $6566,$d4a0,$ea50,$6a95,$5adf,$2b60,$86e3,$92ef,$c8d7,$c95f,
    $d4a0,$d8a6,$b55f,$56a0,$a5b4,$25df,$92d0,$d2b2,$a950,$b557,
    $6ca0,$b550,$5355,$4daf,$a5b0,$4573,$52bf,$a9a8,$e950,$6aa0,
    $aea6,$ab50,$4b60,$aae4,$a570,$5260,$f263,$d950,$5b57,$56a0,
    $96d0,$4dd5,$4ad0,$a4d0,$d4d4,$d250,$d558,$b540,$b6a0,$95a6,
    $95bf,$49b0,$a974,$a4b0,$b27a,$6a50,$6d40,$af46,$ab60,$9570,
    $4af5,$4970,$64b0,$74a3,$ea50,$6b58,$5ac0,$ab60,$96d5,$92e0, //1999
    $c960,$d954,$d4a0,$da50,$7552,$56a0,$abb7,$25d0,$92d0,$cab5,
    $a950,$b4a0,$baa4,$ad50,$55d9,$4ba0,$a5b0,$5176,$52bf,$a930,
    $7954,$6aa0,$ad50,$5b52,$4b60,$a6e6,$a4e0,$d260,$ea65,$d530,
    $5aa0,$76a3,$96d0,$4afb,$4ad0,$a4d0,$d0b6,$d25f,$d520,$dd45,
    $b5a0,$56d0,$55b2,$49b0,$a577,$a4b0,$aa50,$b255,$6d2f,$ada0,
    $4b63,$937f,$49f8,$4970,$64b0,$68a6,$ea5f,$6b20,$a6c4,$aaef,
    $92e0,$d2e3,$c960,$d557,$d4a0,$da50,$5d55,$56a0,$a6d0,$55d4,
    $52d0,$a9b8,$a950,$b4a0,$b6a6,$ad50,$55a0,$aba4,$a5b0,$52b0,
    $b273,$6930,$7337,$6aa0,$ad50,$4b55,$4b6f,$a570,$54e4,$d260,
    $e968,$d520,$daa0,$6aa6,$56df,$4ae0,$a9d4,$a4d0,$d150,$f252,
    $d520);

  Gan: array[0..9] of string[2] =
  ('甲','乙','丙','丁','戊','己','庚','辛','壬','癸');

  Zhi: array[0..11] of string[2] =
  ('子','丑','寅','卯','辰','巳','午','未','申','酉','戌','亥');

  Animals: Array[0..11] of string[2] =
  ('鼠','牛','虎','兔','龙','蛇','马','羊','猴','鸡','狗','猪');

  solarTerm: Array[0..23] of string[4] =
  ('小寒','大寒','立春','雨水','惊蛰','春分','清明','谷雨'
  ,'立夏','小满','芒种','夏至','小暑','大暑','立秋','处暑'
  ,'白露','秋分','寒露','霜降','立冬','小雪','大雪','冬至');

  sTermInfo: Array[0..23] of integer =
  (0,21208,42467,63836,85337,107014,128867,150921
  ,173149,195551,218072,240693,263343,285989,308563,331033
  ,353350,375494,397447,419210,440795,462224,483532,504758);

  nStr1: array[0..10] of string[2] =
  ('日','一','二','三','四','五','六','七','八','九','十');

  nStr2: Array[0..3] of string[2] = ('初','十','廿','卅');
  sFtv : Array[0..22] of string =('0101*元旦','0214 情人节','0308 妇女节'
  ,'0312 植树节','0315 消费者权益日','0401 愚人节','0501 劳动节','0504 青年节'
  ,'0512 护士节','0601 儿童节','0701 建党节 香港回归纪念'
  ,'0801 建军节','0808 父亲节','0909 毛泽东逝世纪念','0910 教师节'
  ,'0928 孔子诞辰','1001*国庆节','1006 老人节','1024 联合国日','1112 孙中山诞辰纪念'
  ,'1220 澳门回归纪念','1225 Christmas Day','1226 毛泽东诞辰纪念');
  lFtv:Array[0..9] of string =('0101*春节','0115 元宵节','0505 端午节'
  ,'0707 七夕情人节','0715 中元节','0815 中秋节','0909 重阳节','1208 腊八节','1224 小年','0100*除夕');

implementation

constructor ThxCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { defaults }
  FUseCurrentDate := True;
  FixedCols := 0;
  FixedRows := 1;
  ColCount := 7;
  RowCount := 7;
  ScrollBars := ssNone;
  Options := Options - [goRangeSelect] + [goDrawFocusSelected];
  FDate := Date;
  UpdateCalendar;
end;

procedure ThxCalendar.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;
procedure ThxCalendar.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

{ AcceptDropped override }
procedure ThxCalendar.AcceptDropped(Sender, Source: TObject; X, Y: integer);
var
  ACol, ARow: LongInt;
  Value: string;
begin
  { convert X and Y to Col and Row for convenience }
  MouseToCell(X, Y, ACol, ARow);
  { let user respond to event }
  if Assigned(FOnDroppedCell) then FOnDroppedCell(Source, ACol, ARow, Value);
  { if user returns a string add it to the cells list }
  if Value <> '' then SetCellString(ACol, ARow, 0, Value);
  { set focus to hxCalendar }
  SetFocus;
  { force redraw }
  Invalidate;
end;

{ CellDragOver override }
procedure ThxCalendar.CellDragOver(Sender, Source: TObject; X, Y: Integer;
State: TDragState; var Accept: Boolean);
var
  ACol, ARow: LongInt;
begin
  { convert X and Y to Col and Row for convenience }
  MouseToCell(X, Y, ACol, ARow);
  { allow user to set Accept the way they want }
  if Assigned(FOnCellDragOver) then
    FOnCellDragOver(Sender, Source, ACol, ARow, State, Accept);
  { if Accept = true then apply further logic else leave Accept = false }
  if Accept = true then
  if (not FUpdating) and (not FReadOnly) and (CellText[ACol, ARow] <> '') then
    Accept := true
  else
    Accept := false;
end;
{ SetCellString - adds a string to the cells stringlist based on Col
or Row or Day of month. }
procedure ThxCalendar.SetCellString(ACol, ARow, ADay: Integer; Value: string);
var
  i, j: integer;
  TheCellText: string;
begin
  if (not FUpdating) and (not FReadOnly) and (CellText[ACol, ARow] <> '') then
  begin
    { if ADay is being used calc ACol and ARow. Doesn't matter if
    ACol and ARow are <> 0 we just calc them anyway }
    if ADay <> 0 then
    begin
      for i := 0 to 6 do
        for j := 1 to 6 do
        begin
          TheCellText := CellText[i, j];
          if (TheCellText <> '') and (ADay = StrToInt(TheCellText)) then
          begin
            ACol := i;
            ARow := j;
          end;
        end;
    end;
    { if no StringList assigned then create one }
    if FCalStrings[ACol, ARow] = nil then
      FCalStrings[ACol, ARow] := TStringList.Create;
    { add the line of text }
    FCalStrings[ACol, ARow].Add(Value);
  end;
end;


procedure ThxCalendar.Click;
var
  TheCellText: string;
begin
  inherited Click;
  TheCellText := CellText[Col, Row];
  if TheCellText <> '' then Day := StrToInt(TheCellText);
end;

function ThxCalendar.IsLeapYear(AYear: Integer): Boolean;
begin
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

function ThxCalendar.DaysPerMonth(AYear, AMonth: Integer): Integer;
const
  DaysInMonth: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then
    Inc(Result); { leap-year Feb is special }
end;

function ThxCalendar.DaysThisMonth: Integer;
begin
  Result := DaysPerMonth(Year, Month);
end;

{procedure ThxCalendar.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
TheText: string;
begin
TheText := CellText[ACol, ARow];
with ARect, Canvas do
TextRect(ARect, Left + (Right - Left - TextWidth(TheText)) div 2,
Top + (Bottom - Top - TextHeight(TheText)) div 2, TheText);
end;}
procedure ThxCalendar.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  HzDate:THzDate;
  TheText,ry,dz,hzdaystr,sf: string;
  MyDate:tdate;
begin
  TheText := CellText[ACol, ARow];
  // DecodeDate(FDate, AYear, AMonth, ADay);
  if (TheText<>'') and (ARow<>0) then
  begin
    MyDate := EncodeDate(year, month, strtoint(TheText));
    HzDate := ToLunar(MyDate);
    dz:= GetJQ(MyDate);
    if dz = '' Then
      if HzDate.Day = 1 then
        ry:=FormatLunarMonth(HzDate.Month,HzDate.isLeap)
      else
        ry := FormatLunarDay(Hzdate.Day);
    if GetsFtv(MyDate)<>'' then sf :=GetsFtv(MyDate);
    if GetlFtv(hzDate)<>'' then sf :=sf+GetlFtv(hzDate);
  end
  else MyDate := 0;

  with ARect, Canvas do
  begin
    if dz<>'' then
    begin
      Font.Color := $000000FF ;
      TextRect(ARect, Left +2,
      Top +2, TheText+sf);
      TextOut(ARect.Left + Font.Size + 10, ARect.Top + 25, dz);
    end
    else
    begin
      if sf<>'' then
      begin
        Font.Color :=$000000FF;
        TextRect(ARect, Left + 2,
        Top + 2, TheText + sf);
      end
      else
      begin
        Font.Color :=clBlue;
        TextRect(ARect, Left + 2,
        Top +2, TheText+sf);
      end;
      Font.Color :=clBlue;
      TextOut(ARect.Left + Font.Size + 10, ARect.Top + 25, ry);
    end;
  end;
end;

function ThxCalendar.GetCellText(ACol, ARow: Integer): string;
var
  DayNum: Integer;
begin
  if ARow = 0 then { day names at tops of columns }
    Result := FormatSettings.ShortDayNames[(StartOfWeek + ACol) mod 7 + 1]
  else
  begin
    DayNum := FMonthOffset + ACol + (ARow - 1) * 7;
    if (DayNum < 1) or (DayNum > DaysThisMonth) then
      Result := ''
    else
      Result := IntToStr(DayNum);
  end;
end;

function ThxCalendar.SelectCell(ACol, ARow: Longint): Boolean;
begin
  if ((not FUpdating) and FReadOnly) or (CellText[ACol, ARow] = '') then
    Result := False
  else
    Result := inherited SelectCell(ACol, ARow);
end;

procedure ThxCalendar.SetCalendarDate(Value: TDate);
begin
  FDate := Value;
  UpdateCalendar;
  Change;
end;

function ThxCalendar.StoreCalendarDate: Boolean;
begin
  Result := not FUseCurrentDate;
end;

function ThxCalendar.GetDateElement(Index: Integer): Integer;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  case Index of
    1: Result := AYear;
    2: Result := AMonth;
    3: Result := ADay;
  else
    Result := -1;
  end;
end;

procedure ThxCalendar.SetDateElement(Index: Integer; Value: Integer);
var
  AYear, AMonth, ADay: Word;
begin
  if Value > 0 then
  begin
    DecodeDate(FDate, AYear, AMonth, ADay);
    case Index of
      1: if AYear <> Value then AYear := Value else Exit;
      2: if (Value <= 12) and (Value <> AMonth) then AMonth := Value else Exit;
      3: if (Value <= DaysThisMonth) and (Value <> ADay) then ADay := Value else Exit;
    else
      Exit;
    end;
    FDate := EncodeDate(AYear, AMonth, ADay);
    FUseCurrentDate := False;
    UpdateCalendar;
    Change;
  end;
end;

procedure ThxCalendar.SetStartOfWeek(Value: TDayOfWeek);
begin
if Value <> FStartOfWeek then
begin
FStartOfWeek := Value;
UpdateCalendar;
end;
end;

procedure ThxCalendar.SetUseCurrentDate(Value: Boolean);
begin
if Value <> FUseCurrentDate then
begin
FUseCurrentDate := Value;
if Value then
begin
FDate := Date; { use the current date, then }
UpdateCalendar;
end;
end;
end;

{ Given a value of 1 or -1, moves to Next or Prev month accordingly }
procedure ThxCalendar.ChangeMonth(Delta: Integer);
var
AYear, AMonth, ADay: Word;
NewDate: TDate;
CurDay: Integer;
begin
DecodeDate(FDate, AYear, AMonth, ADay);
CurDay := ADay;
if Delta > 0 then ADay := DaysPerMonth(AYear, AMonth)
else ADay := 1;
NewDate := EncodeDate(AYear, AMonth, ADay);
NewDate := NewDate + Delta;
DecodeDate(NewDate, AYear, AMonth, ADay);
if DaysPerMonth(AYear, AMonth) > CurDay then ADay := CurDay
else ADay := DaysPerMonth(AYear, AMonth);
CalendarDate := EncodeDate(AYear, AMonth, ADay);
end;

procedure ThxCalendar.PrevMonth;
begin
ChangeMonth(-1);
end;

procedure ThxCalendar.NextMonth;
begin
ChangeMonth(1);
end;

procedure ThxCalendar.NextYear;
begin
if IsLeapYear(Year) and (Month = 2) and (Day = 29) then Day := 28;
Year := Year + 1;
end;

procedure ThxCalendar.PrevYear;
begin
if IsLeapYear(Year) and (Month = 2) and (Day = 29) then Day := 28;
Year := Year - 1;
end;

procedure ThxCalendar.UpdateCalendar;
var
AYear, AMonth, ADay: Word;
FirstDate: TDate;
begin
FUpdating := True;
try
DecodeDate(FDate, AYear, AMonth, ADay);
FirstDate := EncodeDate(AYear, AMonth, 1);
FMonthOffset := 2 - ((DayOfWeek(FirstDate) - StartOfWeek + 7) mod 7); { day of week for 1st of month }
if FMonthOffset = 2 then FMonthOffset := -5;
MoveColRow((ADay - FMonthOffset) mod 7, (ADay - FMonthOffset) div 7 + 1,
False, False);
Invalidate;
finally
FUpdating := False;
end;
end;

procedure ThxCalendar.WMSize(var Message: TWMSize);
var
GridLines: Integer;
begin
GridLines := 6 * GridLineWidth;
DefaultColWidth := (Message.Width - GridLines) div 7;
DefaultRowHeight := (Message.Height - GridLines) div 7;
end;
function ThxCalendar.DaysOfLunarYear(y: integer): integer;
var
i, sum: integer;
begin
sum:= 348; //29 * 12
i:= $8000;
while i > $8 do
begin
if (lunarInfo[y - 1900] and i) > 0 then sum := sum + 1 ;
i:= i shr 1;
end;
Result:= sum + DaysOfLeapMonth(y);
end;

// 返回农历 y年闰月的天数
function ThxCalendar.DaysOfLeapMonth(y: integer): integer;
begin
if leapMonth(y) > 0 then
if (lunarInfo[y - 1899] and $f) = $f then
Result := 30
else
Result := 29
else
Result := 0;
end;

//返回农历 y年闰哪个月 1-12 , 没闰返回 0
function ThxCalendar.leapMonth(y: integer): integer;
var
lm: Word;
begin
lm:= lunarInfo[y - 1900] and $f;
if lm = $f then Result:= 0 else Result:= lm;
end;

//返回农历 y年m月的天数
function ThxCalendar.DaysOfMonth(y, m: integer): integer;
var
temp1, temp2, temp3: Word;
begin
temp1:= lunarInfo[y - 1900];
temp2:= $8000;
if m > 1 then temp2:= $8000 shr (m - 1);
temp3:= temp1 and temp2;
if temp3 > 0 then
Result:= 30
else Result:= 29;
end;

//算出农历, 传入公历日期, 返回农历日期
function ThxCalendar.ToLunar(TheDate: TDate): THzDate;
var
TheYear, TheMonth,leap, temp, offset: integer;
begin
if (32 > TheDate) or (TheDate >= 73416) then //73415=EncodeDate(2100,12,31)
begin //32 = EncodeDate(1900,1,31) 农历1900年1月1日
Result.Year := 0;
Result.Month:= 0;
Result.Day := 0;
Result.isLeap := False;
exit;
end;
offset:= DaysBetween(32,TheDate);
TheYear:= 1900;
while offset > 0 do
begin
temp:= DaysOfLunarYear(TheYear);
TheYear := theYear + 1;
offset:= offset - temp;
end;
if offset < 0 then
begin
offset:= offset + temp;
TheYear:= TheYear - 1;
end;

leap:= leapMonth(TheYear); //闰哪个月
result.isLeap := False;
TheMonth := 0;
while offset >= 0 do
begin
TheMonth:= TheMonth + 1;
temp:= DaysOfMonth(TheYear, TheMonth);
offset:= offset - temp; //减去该月天数
if (offset >= 0) and (TheMonth = Leap) then //如果还有剩余天数且本月有闰
begin //减去闰月天数；
temp:= DaysOfLeapMonth(TheYear);
offset:= offset - temp;
if offset < 0 then
result.isLeap := True; //置闰月标志为真；
end;
end;
if offset < 0 then
begin
offset:= offset + temp;
end;
Result.Year := TheYear;
Result.Month:= TheMonth;
Result.Day:= offset + 1;
end;

// 求年柱，月柱，日柱
// 年，月为农历数字，objDate为当天的公历日期
function ThxCalendar.GetGZ(y, m: integer; TheDate: TDate): TGzDate;
var
term: TDate;
sy, sm, sd: Word;
begin
DecodeDate(TheDate, sy, sm, sd);
term:= sTerm(sy, (sm - 1) * 2); // 当月的节气日期

//年柱 1900年立春后为庚子年(60进制36)
Result.Year:= sy - 1900 + 36;
//依立春日期调整年柱.立春日固定在公历2月
if (sm = 2) and (TheDate < term) then
Result.Year:= sy - 1900 + 35;

//月柱 农历1900年1月小寒以前为 丙子月(60进制12)
Result.Month:= (sy - 1900) * 12 + sm + 11;
//依节气调整月柱
if TheDate >= DateOf(term) then Result.Month:= (sy - 1900) * 12 + sm + 12;

// 1900/1/1 日柱为甲辰日(60进制10)
Result.Day:= DaysBetween(EncodeDate(1900,1,1),TheDate) + 10;
end;

// 算出公历, 传入农历日期控件, 返回公历
function ThxCalendar.ToGreg(objDate: THzDate): TDate;
var
i, j, t, leap, temp, offset: integer;
isLeap: Boolean;
y, m: integer;
begin

Result:= EncodeDate(1,1,1);
if not ChkHzDate(objDate) then exit;

isLeap:= False;
y:= objDate.Year;
m:= objDate.Month;
leap:= leapMonth(y);

//本年内从大年初一过来的天数
offset:= 0;
i:= 1;
while i < m do
begin
if i = leap then
begin
if isLeap then
begin
temp:= DaysOfleapMonth(y);
isLeap:= False;
end
else begin
temp:= daysOfmonth(y, i);
isLeap:= True;
i:= i - 1;
end;
end else
temp:= daysofmonth(y, i);
offset:= offset + temp;
Inc(i);
end;

offset:= offset + objDate.Day - 1;
if (m = leap) and objDate.isLeap then //若为闰月，再加上前一个非闰月天数
offset:= offset + DaysOfMonth(y, m);

// 该年到 2000.1.1 这几年的天数
if y > 2000 then
begin
i:= 2000;
j:= y - 1;
end
else begin
i:= y;
j:= 1999;
end;

temp:= 0;
for t:= i to j do
begin
temp:= temp + DaysOfLunarYear(t);
end;

if y > 1999 then offset:= offset + temp
else offset:= offset - temp;

//农历二零零零年大年初一的阳历为 2000.2.5
Result:= incDay(EncodeDate(2000,2,5),offset);
end;

// 检查农历日期是否合法
function ThxCalendar.ChkHzDate(objDate: THzDate): Boolean;
begin
if (objDate.Year > 2099) or (objDate.Year < 1901)
or (objDate.Month > 12) or (objDate.Day > 30) then
begin
Result:= False;
exit;
end;

Result:= True;
if objDate.isLeap then
begin
if leapMonth(objDate.Year) = objDate.Month then
begin
if DaysOfleapMonth(objDate.Year) < objDate.Day then
Result:= False;
end else Result:= False;
end else
begin
if DaysOfMonth(objDate.Year,objDate.Month) < objDate.Day then
Result:= False;
end;
end;

// 某年的第n个节气为几日(从0小寒起算)
function ThxCalendar.sTerm(y, n: integer): TDateTime;
var
temp: TDateTime;
t: real;
i: Int64;
begin
t:= sTermInfo[n];
t:= t * 60000;
t:= t + 31556925974.7 * (y - 1900);
i:= Round(t);
Temp:= IncMilliSecond(EncodeDateTime(1900,1,6,2,5,0,0),i);
Result:= temp;
end;

// 传入 offset 返回干支, 0=甲子
function ThxCalendar.cyclical(num: integer): string;
begin
Result:= Gan[num mod 10] + Zhi[num mod 12]+'('+Animals[num mod 12]+'年)';
end;

function ThxCalendar.FormatLunarDay(day:integer): string;
begin
case day of
1..10: Result:= nStr2[0] + nStr1[day];
11..19: Result:= nStr2[1] + nStr1[day - 10];
20: Result:= nStr1[2] + nStr1[10];
21..29: Result:= nStr2[2] + nStr1[day - 20];
30: Result:= nStr1[3] + nStr1[10];
else Result :='';
end;
end;

function ThxCalendar.FormatLunarMonth(month: integer; isLeap: boolean): string;
begin
  case month of
    1..10: Result:= nStr1[month];
    11..12: Result:= nStr1[10] + nStr1[month - 10];
  else
    result :='';
  end;
  if isLeap then
    Result:= '闰' + Result;
  Result:= Result + '月';
end;

function ThxCalendar.FormatLunarYear(year:integer): string;
var
temp: integer;
zero: string;
begin
zero:= '零';

temp:= year div 1000;
Result:= nStr1[temp];
year:= year - temp * 1000;

if year >= 100 then
begin
temp:= year div 100;
Result:= Result + nStr1[temp];
year:= year - temp * 100;
end
else
Result:= Result + zero;

if year >= 10 then
begin
temp:= year div 10;
Result:= Result + nStr1[temp];
year:= year - temp * 10;
end
else
Result:= Result + zero;

if year = 0 then Result:= Result + zero else
Result:= Result + nStr1[year];
Result:= Result + '年';
end;

// 取得指定日期的节气
function ThxCalendar.GetJQ(TheDate: TDate): string;
var
  jq: Integer;
  term: TDateTime;
begin
  Result:= '';
  jq:= (MonthOf(TheDate) - 1) * 2;
  term:= sTerm(Yearof(TheDate), jq); //节气时间
  if DateOf(term) = TheDate then
    Result:= solarTerm[jq]
  else
  begin
    term:= sTerm(Yearof(TheDate), jq + 1); //中气时间
    if DateOf(term) = TheDate then
      Result:= solarTerm[jq+1];
  end;
end;
function ThxCalendar.GetsFtv(TheDate: TDate): string;
var
sf:string;
jlsl:integer;
begin
for jlsl :=0 to 22 do
begin
sf:=formatdatetime('mmdd',TheDate);
if sf=copy(sFtv[jlsl],1,4)then
begin
Result:=copy(sFtv[jlsl],5,length(sFtv[jlsl])-4);
end;
end;
end;
function ThxCalendar.GetlFtv(TheDate: ThzDate): string;
var
sf,m,d:string;
jlsl:integer;
begin
//HzDate := ToLunar(MyDate);
for jlsl :=0 to 9 do
begin
if TheDate.month<10 then m:='0'+inttostr(TheDate.month)else m:=inttostr(TheDate.month);
if TheDate.day=0 then d:='01';
if TheDate.day<10 then d:='0'+inttostr(TheDate.day)else d:=inttostr(TheDate.day);
sf:=m+d;
if sf=copy(lFtv[jlsl],1,4)then
begin
Result:=copy(lFtv[jlsl],5,length(lFtv[jlsl])-4);
end;
end;
end;
end.
