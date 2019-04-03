{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit FunctionImp;

interface

uses
  PluginImp, FunctionConst, FunctionIntf;

type
  TFunBLLFormShow = class(TPluginFunction, IFunBLLFormShow)
  private
    FAppHandle: THandle;
    FOnNotifyEvent: TFunctionNotifyEvent;
  public
    constructor Create; override;
    {IFunBLLFormShow}
    function GetAppHandle: THandle;
    procedure SetAppHandle(const Value: THandle);
    function GetOnNotifyEvent: TFunctionNotifyEvent;
    procedure SetOnNotifyEvent(const Value: TFunctionNotifyEvent);
    property AppHandle: THandle read GetAppHandle write SetAppHandle;
    property OnNotifyEvent: TFunctionNotifyEvent read GetOnNotifyEvent write SetOnNotifyEvent;
  end;

implementation

{ TFunBLLFormShow }

constructor TFunBLLFormShow.Create;
begin
  ID := FUN_BLLFORMSHOW;
  Name := FUN_BLLFORMSHOW_NAME;
end;

function TFunBLLFormShow.GetAppHandle: THandle;
begin
  Result := FAppHandle;
end;

function TFunBLLFormShow.GetOnNotifyEvent: TFunctionNotifyEvent;
begin
  Result := FOnNotifyEvent;
end;

procedure TFunBLLFormShow.SetAppHandle(const Value: THandle);
begin
  FAppHandle := Value;
end;

procedure TFunBLLFormShow.SetOnNotifyEvent(const Value: TFunctionNotifyEvent);
begin
  FOnNotifyEvent := Value;
end;

end.
