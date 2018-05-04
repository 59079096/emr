{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit emr_PluginObject;

interface

uses
  FunctionIntf;

type
  IUserInfo = interface(IPluginObject)
    ['{60512600-C4C7-477D-A9A3-D56F667303BD}']
    function GetUserID: string;
    procedure SetUserID(const Value: string);
    property UserID: string read GetUserID write SetUserID;
  end;

  IServerInfo = interface(IPluginObject)
    ['{72D906BD-6D54-4BD2-8F82-9A50C4F1646D}']
    function GetHost: string;
    procedure SetHost(const Value: string);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetTimeOut: Integer;
    procedure SetTimeOut(const Value: Integer);
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
    property TimeOut: Integer read GetTimeOut write SetTimeOut;
  end;

  TUserInfoIntf = class(TInterfacedObject, IUserInfo)
  private
    FUserID: string;
    function GetUserID: string;
    procedure SetUserID(const Value: string);
  end;

  TServerInfo = class(TInterfacedObject, IServerInfo)
  private
    FHost: string;
    FPort, FTimeOut: Integer;
    function GetHost: string;
    procedure SetHost(const Value: string);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetTimeOut: Integer;
    procedure SetTimeOut(const Value: Integer);
  end;

implementation

{ TUserInfoIntf }

function TUserInfoIntf.GetUserID: string;
begin
  Result := FUserID;
end;

procedure TUserInfoIntf.SetUserID(const Value: string);
begin
  FUserID := Value;
end;

{ TServerInfo }

function TServerInfo.GetHost: string;
begin
  Result := FHost
end;

function TServerInfo.GetPort: Integer;
begin
  Result := FPort;
end;

function TServerInfo.GetTimeOut: Integer;
begin
  Result := FTimeOut;
end;

procedure TServerInfo.SetHost(const Value: string);
begin
  FHost := Value
end;

procedure TServerInfo.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

procedure TServerInfo.SetTimeOut(const Value: Integer);
begin
  FTimeOut := Value;
end;

end.


