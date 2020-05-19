{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit emr_DataBase;

interface

uses
  Classes, FireDAC.Comp.Client, FireDAC.Stan.Def, FireDAC.Phys.MSSQL,
  FireDAC.DApt, FireDAC.Stan.Async, FireDAC.Stan.Option, ActiveX, DB;

type
  TDBType = (dbSqlServer, dbOracle, dbDB2, dbSqlite, dbMySQL, dbMongoDB, dbPostgre);

  TDataBase = class(TObject)
  private
    FConnection: TFDConnection;
    FDBType: TDBType;
  protected
    procedure SetDBType(Value: TDBType);
    function GetServer: string;
    function GetDBName: string;
    function GetUserName: string;
    function GetPassword: string;
    function GetPort: Integer;
    function GetConnected: Boolean;

    procedure SetPassword(const Value: string);
    procedure SetServer(const Value: string);
    procedure SetDBName(const Value: string);
    procedure SetUserName(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetConnected(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure Connect;
    procedure DisConnect;
    function GetQuery: TFDQuery;

    procedure SelectSql(const ASql: string; const AQuery: TFDQuery); overload;
    //procedure SelectSql(const ASql: string; const ACDS: TClientDataSet); overload;
    function SelectSql(const ASql: string): Integer; overload;

    function ExecSql(const ASql: string): Integer;
    //
    function GetDateTime: TDateTime;

    property DBType: TDBType read FDBType write SetDBType;
    property Server: string read GetServer write SetServer;
    property DBName: string read GetDBName write SetDBName;
    property Port: Integer read GetPort write SetPort;
    property UserName: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword stored False;
    property Connected: Boolean read GetConnected write SetConnected;
    property Connection: TFDConnection read FConnection;
  end;

implementation

uses
  SysUtils;

{ TDataBase }

procedure TDataBase.Connect;
begin
  Connected := True;
end;

constructor TDataBase.Create(AOwner: TComponent);
begin
  inherited Create;
  FConnection := TFDConnection.Create(AOwner);
  FConnection.LoginPrompt := False;
  //FConnection.FetchOptions.RecordCountMode := TFDRecordCountMode.cmTotal;  // 增加后执行不了带返回select的存储过程了
  FConnection.FetchOptions.RowsetSize := 200;
end;

destructor TDataBase.Destroy;
begin
  FConnection.Connected := False;
  inherited Destroy;
end;

procedure TDataBase.DisConnect;
begin
  FConnection.Connected := False;
end;

function TDataBase.ExecSql(const ASql: string): Integer;
begin

end;

function TDataBase.GetConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TDataBase.GetDBName: string;
begin
  Result := FConnection.Params.Values['Database'];
end;

function TDataBase.GetDateTime: TDateTime;
var
  vQuery: TFDQuery;
begin
  vQuery := GetQuery;
  try
    vQuery.Close;
    case FDBType of
      dbSqlServer: vQuery.SQL.Text := 'SELECT GETDATE() AS dt';
      dbOracle: ;
      dbDB2: ;
      dbSqlite: ;
    end;
    vQuery.Open;
    Result := vQuery.FieldByName('dt').AsDateTime;
  finally
    vQuery.Free;
  end;
end;

function TDataBase.GetPassword: string;
begin
  Result := FConnection.Params.Values['Password'];
end;

function TDataBase.GetPort: Integer;
begin
  Result := FConnection.Params.Values['Port'].ToInteger;
end;

function TDataBase.GetQuery: TFDQuery;
begin
  {需要改为对象池}
  Result := TFDQuery.Create(nil);
  Result.Connection := FConnection;
end;

function TDataBase.GetServer: string;
begin
  Result := FConnection.Params.Values['Server'];
end;

function TDataBase.GetUserName: string;
begin
  Result := FConnection.Params.Values['User_Name'];
end;

procedure TDataBase.SelectSql(const ASql: string; const AQuery: TFDQuery);
begin
  AQuery.Connection := FConnection;
  AQuery.Close;
  AQuery.SQL.Text := ASql;
  AQuery.Open;
end;

function TDataBase.SelectSql(const ASql: string): Integer;
var
  vQuery: TFDQuery;
begin
  vQuery := TFDQuery.Create(nil);
  try
    //vQuery.Connection := FConnection;
    SelectSql(ASql, vQuery);
    //vQuery.SQL.Text := ASql;
    //vQuery.ExecSQL;
    Result := vQuery.RecordCount;
  finally
    vQuery.Free;
  end;
end;

procedure TDataBase.SetConnected(const Value: Boolean);
begin
  if Value then
    FConnection.Connected := True
  else
    FConnection.Connected := False;
end;

procedure TDataBase.SetDBName(const Value: string);
begin
  FConnection.Params.Values['Database'] := Value;
end;

procedure TDataBase.SetDBType(Value: TDBType);
begin
  if FConnection.Connected then
    FConnection.Connected := False;

  case Value of  // 参考 FireDAC.Stan.Consts 单元常量
    dbSqlServer: FConnection.DriverName := 'MSSQL';
    dbOracle: FConnection.DriverName := 'Ora';
    dbDB2: FConnection.DriverName := 'DB2';
    dbSqlite: FConnection.DriverName := 'SQLite';
    dbMySQL: FConnection.DriverName := 'MySQL';
    dbMongoDB: FConnection.DriverName := 'Mongo';
    dbPostgre: FConnection.DriverName := 'PG';
  end;
end;

procedure TDataBase.SetPassword(const Value: string);
begin
  FConnection.Params.Values['Password'] := Value;
end;

procedure TDataBase.SetPort(const Value: Integer);
begin
  FConnection.Params.Values['Port'] := Value.ToString;
end;

procedure TDataBase.SetServer(const Value: string);
begin
  FConnection.Params.Values['Server'] := Value;
end;

procedure TDataBase.SetUserName(const Value: string);
begin
  FConnection.Params.Values['User_Name'] := Value;
end;

end.
