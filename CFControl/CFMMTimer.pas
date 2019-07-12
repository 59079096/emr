unit CFMMTimer;

interface

uses
  Windows, Classes, MMSystem, TypInfo;

type
  TCFMMTimer = class(TObject)
  private
    FInternal: Cardinal;  // 定时器的间隔时间，单位为毫秒，默认为1000毫秒
    FEnable: Boolean;  // 定时器是否在运行，默认为FALSE
    FProcCallback: TFnTimeCallback;  // 回调函数指针
    FOnTimer: TNotifyEvent;  // 周期回调时触发的定时器事件
    FHTimerID: Integer;  // 定时器ID停止定时器使用

    procedure SetInternal(const Value: Cardinal);
    procedure SetEnable(const Value: Boolean);
  public
    property Internal: Cardinal read FInternal write SetInternal default 1000;
    property Enable: Boolean read FEnable write SetEnable default FALSE;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TCFMMTimer }

procedure DoTimer(uTimerID, uMessage: UINT; dwUser, dw1, dw2: DWORD_PTR) stdcall;
var
  vObj: TCFMMTimer;
begin
  vObj := TCFMMTimer(dwUser);  // dwUser实际上是定时器对象的地址 ??
  if Assigned(vObj.OnTimer)then
    vObj.OnTimer(vObj);
end;

constructor TCFMMTimer.Create;
begin
  inherited Create;
  FInternal := 1000;  // 默认为1秒
  FEnable := False;  // 创建并不启动定时器
  FOnTimer := nil;
  FProcCallback := DoTimer;  // 对象中的函数指针只有一个，需要考虑利用dwUser来区分不同的对象函数回调
end;

destructor TCFMMTimer.Destroy;
begin
  SetEnable(False);
  inherited Destroy;
end;

procedure TCFMMTimer.SetEnable(const Value: Boolean);
begin
  if FEnable <> Value then
  begin
    FEnable := Value;
    if FEnable then
      FHTimerID := TimeSetEvent(FInternal, 0, FProcCallback, Integer(Self), 1 )  // 这里把对象地址传入，回调时传回，最后一个参数表示周期回调，第二个参数0为最高精度
    else
      TimeKillEvent( FHTimerID );
  end;
end;

procedure TCFMMTimer.SetInternal(const Value: Cardinal);
begin
  if FInternal <> Value then
  begin
    FInternal := Value;
    if FEnable then  // 如果定时器已经启动并更改周期时长则需要先停止再重新启动定时器
    begin
      Enable := False;
      Enable := True;
    end;
  end;
end;

end.
