{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit BLLServerParam;

interface

{ 做为中间服务器时，配置要连接的主服务器参数 }

uses
  IniFiles;

type
  TIniFileEvent = reference to procedure(const AIniFile: TIniFile);

  TBLLServerParams = class(TObject)  // 业务服务端参数(仅Win平台使用)
  private
    FFileName: string;
    FRemoteActive: Boolean;
    FRemoteBLLIP: string;
    FRemoteBLLPort: Word;
    FDataBaseServer: string;
    FDataBaseName: string;
    FDataBaseUsername: string;
    FDataBasePassword: string;
  protected
    procedure SetRemoteActive(const Value: Boolean);
    procedure SetRemoteBLLIP(const Value: string);
    procedure SetRemoteBLLPort(const Value: Word);
    procedure SetDataBaseServer(const Value: string);
    procedure SetDataBaseName(const Value: string);
    procedure SetDataBaseUsername(const Value: string);
    procedure SetDataBasePassword(const Value: string);
  public
    constructor Create(const AFileName: string);
    // 远程服务器
    property RemoteActive: Boolean read FRemoteActive write SetRemoteActive;
    property RemoteBLLIP: string read FRemoteBLLIP write SetRemoteBLLIP;
    property RemoteBLLPort: Word read FRemoteBLLPort write SetRemoteBLLPort;
    // 数据库
    property DataBaseServer: string read FDataBaseServer write SetDataBaseServer;
    property DataBaseName: string read FDataBaseName write SetDataBaseName;
    property DataBaseUsername: string read FDataBaseUsername write SetDataBaseUsername;
    property DataBasePassword: string read FDataBasePassword write SetDataBasePassword;

    /// <summary> 读取或写入Ini文件 </summary>
    /// <param name="AIniFileEvent">操作Ini文件</param>
    procedure ReadOrWriteIniFile(const AIniFileEvent: TIniFileEvent);

  end;

var
  BLLServerParams: TBLLServerParams;

implementation

uses
  SysUtils, Soap.EncdDecd;

{ TBLLServerParams }

constructor TBLLServerParams.Create(const AFileName: string);
begin
  inherited Create;
  if not FileExists(AFileName) then
    raise Exception.Create('异常：未找到配置文件' + AFileName)
  else
  begin
    FFileName := AFileName;
    ReadOrWriteIniFile(
      procedure(const AIniFile: TIniFile)
      begin
        FRemoteActive := AIniFile.ReadBool('RemoteServer', 'active', False);
        FRemoteBLLIP := AIniFile.ReadString('RemoteServer', 'ip', '');
        FRemoteBLLPort := AIniFile.ReadInteger('RemoteServer', 'port', 12726);
        FDataBaseServer := AIniFile.ReadString('DataBase', 'ip', '');
        FDataBaseName := AIniFile.ReadString('DataBase', 'DBName', '');
        FDataBaseUsername := AIniFile.ReadString('DataBase', 'Username', '');
        FDataBasePassword := DecodeString(AIniFile.ReadString('DataBase', 'Password', ''));  // 解密
      end);
  end;
end;

procedure TBLLServerParams.SetDataBaseName(const Value: string);
begin
  if FDataBaseName <> Value then
  begin
    ReadOrWriteIniFile(
      procedure(const AIniFile: TIniFile)
      begin
        AIniFile.WriteString('DataBase', 'DBName', Value);
      end);
    FDataBaseName := Value;
  end;
end;

procedure TBLLServerParams.SetDataBasePassword(const Value: string);
begin
  if FDataBasePassword <> Value then
  begin
    ReadOrWriteIniFile(
      procedure(const AIniFile: TIniFile)
      begin
        AIniFile.WriteString('DataBase', 'Password', Value);  // 加密
      end);
    FDataBasePassword := Value;
  end;
end;

procedure TBLLServerParams.SetDataBaseServer(const Value: string);
begin
  if FDataBaseServer <> Value then
  begin
    ReadOrWriteIniFile(
      procedure(const AIniFile: TIniFile)
      begin
        AIniFile.WriteString('DataBase', 'ip', Value);
      end);
    FDataBaseServer := Value;
  end;
end;

procedure TBLLServerParams.SetDataBaseUsername(const Value: string);
begin
  if FDataBaseUsername <> Value then
  begin
    ReadOrWriteIniFile(
      procedure(const AIniFile: TIniFile)
      begin
        AIniFile.WriteString('DataBase', 'Username', Value);
      end);
    FDataBaseUsername := Value;
  end;
end;

procedure TBLLServerParams.SetRemoteActive(const Value: Boolean);
begin
  if FRemoteActive <> Value then
  begin
    ReadOrWriteIniFile(
      procedure(const AIniFile: TIniFile)
      begin
        AIniFile.WriteBool('RemoteServer', 'active', Value);
      end);
    FRemoteActive := Value;
  end;
end;

procedure TBLLServerParams.SetRemoteBLLIP(const Value: string);
begin
  if FRemoteBLLIP <> Value then
  begin
    ReadOrWriteIniFile(
      procedure(const AIniFile: TIniFile)
      begin
        AIniFile.WriteString('RemoteServer', 'ip', Value);
      end);
    FRemoteBLLIP := Value;
  end;
end;

procedure TBLLServerParams.SetRemoteBLLPort(const Value: Word);
begin
  if FRemoteBLLPort <> Value then
  begin
    ReadOrWriteIniFile(
      procedure(const AIniFile: TIniFile)
      begin
        AIniFile.WriteInteger('RemoteServer', 'port', Value);
      end);
    FRemoteBLLPort := Value;
  end;
end;

procedure TBLLServerParams.ReadOrWriteIniFile(const AIniFileEvent: TIniFileEvent);
var
  vIni: TIniFile;
begin
  vIni := TIniFile.Create(FFileName);
  try
    AIniFileEvent(vIni);
  finally
    vIni.Free;
  end;
end;

end.
