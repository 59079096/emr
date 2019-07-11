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
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Comp.UI;

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
    function OpenSql(const ASql: string): TFDQuery;
    function ExecSql(const ASql: string): Boolean;
  end;

var
  dm: Tdm;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ Tdm }

procedure Tdm.DataModuleCreate(Sender: TObject);
var
  vDBPath: string;
begin
  vDBPath := ExtractFilePath(ParamStr(0)) + 'upServer.db';
  if FileExists(vDBPath) then
    conn.ConnectionString := 'DriverID=SQLite;Password=up190512.;Database=' + vDBPath
  else  // 创建数据库
  begin
    conn.Params.Add('DriverID=SQLite');
    conn.Params.Add('Database=' + vDBPath);
    conn.Params.Add('Password=up190512.');
  end;

  // 判断升级信息表是否存在，不存在则创建
  qryTemp.Open('SELECT COUNT(*) AS tbcount FROM sqlite_master where type=''table'' and name=''UpdateInfo''');
  if qryTemp.FieldByName('tbcount').AsInteger = 0 then  // 不存在params表
  begin
    conn.ExecSQL('CREATE TABLE UpdateInfo (' +
      'verno integer primary key AutoIncRement, ' +  // 版本序号
      'version nvarchar(20), ' +  // 版本号
      'memo nvarchar(255), ' +  // 版本说明
      'files nvarchar(1024))');  // 升级文件xml格式
  end;
end;

function Tdm.ExecSql(const ASql: string): Boolean;
begin
  Result := qryTemp.ExecSQL(ASql) > 0;
end;

function Tdm.OpenSql(const ASql: string): TFDQuery;
begin
  Result := TFDQuery.Create(nil);
  Result.Connection := conn;
  //qryTemp.Open(ASql);
  Result.Close;
  Result.SQL.Text := ASql;
  Result.Open;
end;

end.
