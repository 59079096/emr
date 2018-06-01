{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_DM;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.FMXUI.Wait,
  FireDAC.Comp.UI, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.VCLUI.Wait;

type
  Tdm = class(TDataModule)
    conn: TFDConnection;
    fdgxwtcrsr: TFDGUIxWaitCursor;
    fdphysqltdrvrlnk: TFDPhysSQLiteDriverLink;
    qryTemp: TFDQuery;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GetPatListInfo;
    procedure OpenSql(const ASql: string);
    procedure ExecSql(const ASql: string);
    procedure GetCacheTable(const ATableName: string);
    function GetParamStr(const AName: string): string;
    function GetParamInt(const AName: string; const ADefValue: Integer): Integer;
    function SetParam(const AName, AValue: string): Boolean;
  end;

var
  dm: Tdm;

implementation

uses
  emr_Common;

{$R *.dfm}

procedure Tdm.DataModuleCreate(Sender: TObject);
var
  vDBPath: string;
begin
  vDBPath := ExtractFilePath(ParamStr(0)) + 'clt.db';
  if FileExists(vDBPath) then
    conn.ConnectionString := 'DriverID=SQLite;Password=emr171212.;Database=' + vDBPath
  else  // 创建数据库
  begin
    conn.Params.Add('DriverID=SQLite');
    conn.Params.Add('Database=' + vDBPath);
    conn.Params.Add('Password=emr171212.');
  end;

  // 判断参数表是否存在，不存在则创建
  qryTemp.Open('SELECT COUNT(*) AS tbcount FROM sqlite_master where type=''table'' and name=''params''');
  if qryTemp.FieldByName('tbcount').AsInteger = 0 then  // 不存在params表
  begin
    conn.ExecSQL('CREATE TABLE params (' +
      'name nvarchar(20) primary key, ' +      // 参数名(主键)
      'value nvarchar(255))');      // 参数值
    conn.ExecSQL(Format('INSERT INTO params (name, value) VALUES (''%s'', ''%s'')', [PARAM_LOCAL_MSGHOST, '']));  // 消息服务器
    conn.ExecSQL(Format('INSERT INTO params (name, value) VALUES (''%s'', ''%s'')', [PARAM_LOCAL_BLLHOST, '']));  // 业务服务器
    conn.ExecSQL(Format('INSERT INTO params (name, value) VALUES (''%s'', ''%s'')', [PARAM_LOCAL_MSGPORT, '']));  // 消息端口
    conn.ExecSQL(Format('INSERT INTO params (name, value) VALUES (''%s'', ''%s'')', [PARAM_LOCAL_BLLPORT, '']));  // 业务端口
    conn.ExecSQL(Format('INSERT INTO params (name, value) VALUES (''%s'', %d)', [PARAM_LOCAL_VERSIONID, 0]));  // 版本号
    conn.ExecSQL(Format('INSERT INTO params (name, value) VALUES (''%s'', ''%s'')', [PARAM_LOCAL_UPDATEHOST, '']));  // 更新服务器
    conn.ExecSQL(Format('INSERT INTO params (name, value) VALUES (''%s'', %d)', [PARAM_LOCAL_UPDATEPORT, '']));  // 更新服务器端口
  end;

  //conn.ExecSQL('drop table clientcache');
  qryTemp.Open('SELECT COUNT(*) AS tbcount FROM sqlite_master where type=''table'' and name=''clientcache''');
  if qryTemp.FieldByName('tbcount').AsInteger = 0 then  // 不存在clientcache表
  begin
    conn.ExecSQL('CREATE TABLE clientcache (' +
      'id int not null primary key, ' +  // 序号
      'tbName nvarchar(32) not null, ' +  // 表名
      'dataVer int not null)');  // 数据版本
  end;
end;

procedure Tdm.OpenSql(const ASql: string);
begin
  //qryTemp.Open(ASql);
  qryTemp.Close;
  qryTemp.SQL.Text := ASql;
  qryTemp.Open;
end;

function Tdm.SetParam(const AName, AValue: string): Boolean;
begin
  qryTemp.Open('SELECT COUNT(*) AS fieldcount FROM params WHERE name=:a', [AName]);
  if qryTemp.FieldByName('fieldcount').AsInteger > 0 then
    Result := conn.ExecSQL('UPDATE [params] SET value=:b WHERE name=:a', [AValue, AName]) = 1
  else
    Result := conn.ExecSQL('INSERT INTO [params] (value, name) VALUES (:a, :b)', [AValue, AName]) = 1;
end;

procedure Tdm.ExecSql(const ASql: string);
begin
  qryTemp.ExecSQL(ASql);
end;

procedure Tdm.GetCacheTable(const ATableName: string);
begin
  qryTemp.Open(Format('SELECT * FROM %s', [ATableName]));
end;

function Tdm.GetParamInt(const AName: string;
  const ADefValue: Integer): Integer;
var
  vsValue: string;
begin
  vsValue := GetParamStr(AName);
  Result := StrToIntDef(vsValue, ADefValue);
end;

function Tdm.GetParamStr(const AName: string): string;
begin
  qryTemp.Open(Format('SELECT value FROM params WHERE name=''%s''',[AName]));
  Result := qryTemp.FieldByName('value').AsString;
  qryTemp.Close;
end;

procedure Tdm.GetPatListInfo;
begin
  qryTemp.Open('SELECT id, col, colname, left, top, right, bottom, fontsize, visible, sys FROM pat_list');
end;

end.
