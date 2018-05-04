{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit emr_BLLDataBase;

interface

uses
  Generics.Collections, SysUtils, emr_DataBase;

type
  TBLLDBConnection = class(TDataBase)  // 数据库连接对象
  strict private
    FConnID: Integer;
  public
    property ConnID: Integer read FConnID write FConnID;
  end;

  TBLLDataBase = class  // 数据库连接对象管理类
  strict private
    FBLLDBConnections: TObjectList<TBLLDBConnection>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DisConnect;
    function GetBLLDataBase(const AConnID, ADBType: Integer; const AServer: string;
      const AProt: Integer; const ADBName, AUserName, APassword: string): TBLLDBConnection;
    function NewBLLDataBase: TBLLDBConnection;
  end;

var
  frameBLLDB: TBLLDataBase;

implementation

{ TBLLDataBase }

constructor TBLLDataBase.Create;
begin
  FBLLDBConnections := TObjectList<TBLLDBConnection>.Create;
end;

destructor TBLLDataBase.Destroy;
begin
  FreeAndNil(FBLLDBConnections);
  inherited;
end;

procedure TBLLDataBase.DisConnect;
var
  i: Integer;
begin
  for i := 0 to FBLLDBConnections.Count do
    FBLLDBConnections[i].DisConnect;
end;

function TBLLDataBase.GetBLLDataBase(const AConnID, ADBType: Integer; const AServer: string;
  const AProt: Integer; const ADBName, AUserName, APassword: string): TBLLDBConnection;
var
  i: Integer;
begin
  Result := nil;
  if AConnID > 0 then  // ID必需大于0
  begin
    for i := 0 to FBLLDBConnections.Count - 1 do  // 查找指定的数据库连接已经创建过
    begin
      if FBLLDBConnections[i].ConnID = AConnID then
      begin
        Result := FBLLDBConnections[i];
        Break;
      end;
    end;
  end;
  if Result = nil then  // 没找到则创建新的数据库连接
  begin
    Result := NewBLLDataBase;
    Result.DBType := TDBType(ADBType);
    Result.ConnID := AConnID;
    Result.Server := AServer;
    Result.Port := AProt;
    Result.DBName := ADBName;
    Result.UserName := AUserName;
    Result.Password := APassword;
    Result.Connect;
  end;
end;

function TBLLDataBase.NewBLLDataBase: TBLLDBConnection;
begin
  Result := TBLLDBConnection.Create(nil);
  FBLLDBConnections.Add(Result);
end;

end.
