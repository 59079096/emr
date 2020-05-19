{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit emr_BLLInvoke;

interface

uses
  Classes, SysUtils, emr_Common, emr_Entry, emr_MsgPack,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, Data.DB, FireDAC.Stan.Intf,
  FireDAC.Stan.StorageBin, diocp_tcp_blockClient;

const
  /// <summary> 获取服务器当前时间 </summary>
  BLL_SRVDT = 1;

  /// <summary> 执行Sql语句 </summary>
  BLL_EXECSQL = 2;

  /// <summary> 获取所有表和表说明 </summary>
  BLL_GETAllTABLE = 3;

  /// <summary> 保存数据(插入、更新)到指定的表 配合TTableOper类的常量 </summary>
  BLL_TABLESAVE = 4;

  /// <summary> 查询指定表的指定字段 </summary>
  BLL_TABLEQUERY = 5;

  BLL_BASE = 1000;  // 业务常量起始值

  { 业务常量(从1000开始) }
  /// <summary> 全部用户 </summary>
  BLL_COMM_ALLUSER = BLL_BASE;

  /// <summary> 验证账号密码匹配 </summary>
  BLL_CERTIFICATE = BLL_BASE + 1;

  /// <summary> 获取指定用户信息 </summary>
  BLL_GETUSERINFO = BLL_BASE + 2;

  /// <summary> 获取用户的工作组 </summary>
  BLL_GETUSERGROUPS = BLL_BASE + 3;

  /// <summary> 获取用户的角色 </summary>
  BLL_GETUSERROLES = BLL_BASE + 4;

  /// <summary> 获取指定用户配置的所有功能 </summary>
  BLL_GETUSERFUNS = BLL_BASE + 5;

  /// <summary> 获取指定用户所有工作组对应的科室 </summary>
  BLL_GETUSERGROUPDEPTS = BLL_BASE + 6;

  /// <summary> 获取参数 </summary>
  BLL_COMM_GETPARAM = BLL_BASE + 7;

  /// <summary> 获取服务端缓存表数据 </summary>
  BLL_GETCLIENTCACHE = BLL_BASE + 8;

  /// <summary> 获取指定窗体上所有受权限控制的控件 </summary>
  BLL_GETCONTROLSAUTH = BLL_BASE + 9;

  /// <summary> 获取要升级的最新版本号 </summary>
  BLL_GETLASTVERSION = BLL_BASE + 10;

  /// <summary> 获取要升级的文件 </summary>
  BLL_GETUPDATEINFO = BLL_BASE + 11;

  /// <summary> 上传升级信息 </summary>
  BLL_UPLOADUPDATEINFO = BLL_BASE + 12;

  /// <summary> 获取在院患者 </summary>
  BLL_HIS_GETINPATIENT = BLL_BASE + 13;

  /// <summary> 获取数据集(根目录)信息 </summary>
  BLL_GETDATAELEMENTSETROOT = BLL_BASE + 14;

  /// <summary> 获取数据集(全目录)信息 </summary>
  BLL_GETDATAELEMENTSETALL = BLL_BASE + 15;

  /// <summary> 获取指定数据集对应的模板 </summary>
  BLL_GETTEMPLATELIST = BLL_BASE + 16;

  /// <summary> 新建模板 </summary>
  BLL_NEWTEMPLATE = BLL_BASE + 17;

  /// <summary> 获取模板内容 </summary>
  BLL_GETTEMPLATECONTENT = BLL_BASE + 18;

  /// <summary> 保存模板内容 </summary>
  BLL_SAVETEMPLATECONTENT = BLL_BASE + 19;

  /// <summary> 删除模板及内容 </summary>
  BLL_DELETETEMPLATE = BLL_BASE + 20;

  /// <summary> 获取数据元 </summary>
  BLL_GETDATAELEMENT = BLL_BASE + 21;

  /// <summary> 获取数据元值域选项 </summary>
  BLL_GETDOMAINITEM = BLL_BASE + 22;

  /// <summary> 保存数据元选项值域对应的内容 </summary>
  BLL_SAVEDOMAINITEMCONTENT = BLL_BASE + 23;

  /// <summary> 获取数据元选项值域对应的内容 </summary>
  BLL_GETDOMAINITEMCONTENT = BLL_BASE + 24;

  /// <summary> 删除数据元选项值域对应的内容 </summary>
  BLL_DELETEDOMAINITEMCONTENT = BLL_BASE + 25;

  /// <summary> 获取指定的住院患者病历列表 </summary>
  BLL_GETINCHRECORDLIST = BLL_BASE + 26;

  /// <summary> 新建住院病历 </summary>
  BLL_NEWINCHRECORD = BLL_BASE + 27;

  /// <summary> 获取指定住院病历内容 </summary>
  BLL_GETINCHRECORDCONTENT = BLL_BASE + 28;

  /// <summary> 保存指定住院病历内容 </summary>
  BLL_SAVERECORDCONTENT = BLL_BASE + 29;

  /// <summary> 获取指定患者数据集(根目录)对应的病历内容 </summary>
  BLL_GETDESETRECORDCONTENT = BLL_BASE + 30;

  /// <summary> 删除指定的住院病历 </summary>
  BLL_DELETEINCHRECORD = BLL_BASE + 31;

  /// <summary> 获取指定数据元的属性信息 </summary>
  BLL_GETDEPROPERTY = BLL_BASE + 32;

  /// <summary> 住院病历签名 </summary>
  BLL_INCHRECORDSIGNATURE = BLL_BASE + 33;

  /// <summary> 获取住院病历签名信息 </summary>
  BLL_GETINCHRECORDSIGNATURE = BLL_BASE + 34;

  /// <summary> 获取模板信息 </summary>
  BLL_GETTEMPLATEINFO = BLL_BASE + 35;

  /// <summary> 修改模板信息 </summary>
  BLL_SETTEMPLATEINFO = BLL_BASE + 36;

  /// <summary> 获取指定数据元信息 </summary>
  BLL_GETDEINFO = BLL_BASE + 37;

  /// <summary> 修改指定数据元信息 </summary>
  BLL_SETDEINFO = BLL_BASE + 38;

  /// <summary> 新建数据元 </summary>
  BLL_NEWDE = BLL_BASE + 39;

  /// <summary> 删除数据元 </summary>
  BLL_DELETEDE = BLL_BASE + 40;

  /// <summary> 获取指定据元值域选项 </summary>
  BLL_GETDOMAINITEMINFO = BLL_BASE + 41;

  /// <summary> 修改数据元值域选项 </summary>
  BLL_SETDOMAINITEMINFO = BLL_BASE + 42;

  /// <summary> 新建数据元值域选项 </summary>
  BLL_NEWDOMAINITEM = BLL_BASE + 43;

  /// <summary> 删除数据元值域选项 </summary>
  BLL_DELETEDOMAINITEM = BLL_BASE + 44;

  /// <summary> 获取所有值域 </summary>
  BLL_GETDOMAIN = BLL_BASE + 45;

  /// <summary> 新建值域 </summary>
  BLL_NEWDOMAIN = BLL_BASE + 46;

  /// <summary> 修改值域 </summary>
  BLL_SETDOMAIN = BLL_BASE + 47;

  /// <summary> 删除值域 </summary>
  BLL_DELETEDOMAIN = BLL_BASE + 48;

  /// <summary> 删除值域对应的所有选项 </summary>
  BLL_DELETEDOMAINALLITEM = BLL_BASE + 49;

  /// <summary> 获取数据集包含的所有数据元 </summary>
  BLL_GETDATASETELEMENT = BLL_BASE + 50;
  {
  /// <summary> 新建住院会诊信息 </summary>
  BLL_BASE + 51;

  /// <summary> 新建会诊受邀信息 </summary>
  BLL_BASE + 52;

  /// <summary> 查询会诊受邀信息 </summary>
  BLL_BASE + 53;

  /// <summary> 新建住院会诊意见 </summary>
  BLL_BASE + 54;   }

  /// <summary> 保存病历结构内容 </summary>
  BLL_SAVERECORDSTRUCTURE = BLL_BASE + 55;

  /// <summary> 修改病历结构内容 </summary>
  BLL_UPDATERECORDSTRUCTURE = BLL_BASE + 56;

  /// <summary> 获取取指定的病历结构内容 </summary>
  BLL_GETRECORDSTRUCTURE = BLL_BASE + 57;

  /// <summary> 获取指定数据集的宏替换信息 </summary>
  BLL_GetDataElementSetMacro = BLL_BASE + 58;

  /// <summary> 获取指定患者指定数据集的病历结构数据 </summary>
  BLL_GetPatDesStructure = BLL_BASE + 59;

  /// <summary> 添加病历锁定信息 </summary>
  BLL_NewLockInRecord = BLL_BASE + 60;

  /// <summary> 获取指定的病历当前编辑锁定信息 </summary>
  BLL_GetInRecordLock = BLL_BASE + 61;

  /// <summary> 删除指定的病历编辑锁定信息 </summary>
  BLL_DeleteInRecordLock = BLL_BASE + 62;

  /// <summary> 获取指定数据元的控制脚本 </summary>
  BLL_GetDataElementScript = BLL_BASE + 63;

  /// <summary> 设置指定数据元的控制脚本 </summary>
  BLL_SetDataElementScript = BLL_BASE + 64;

  /// <summary> 查询指定患者非某次住院外的其他次住院就诊信息 </summary>
  BLL_PatientHisInchInfo = BLL_BASE + 67;

  /// <summary> 文件生成PDF </summary>
  BLL_RecordToPDF = BLL_BASE + 68;

type
  TTableOper = class(TObject)
  public
    const
      /// <summary> 要操作的表名 </summary>
      Table = 'TB';
      /// <summary> 要操作的主键，多个以";"隔开 </summary>
      PrimKeys = 'PK';
      /// <summary> 要操作的字段，多个以";"隔开 </summary>
      Fields = 'FD';
  end;

  TBLLServerProxy = class(TObject)
  private
    FReconnect: Boolean;
    FTcpClient: TDiocpBlockTcpClient;
    FDataStream: TMemoryStream;
    FErrCode: Integer;  // 错误代码
    FErrMsg: string;  // 发生错误时的 ip和端口
    procedure CheckConnect;
    function SendStream(pvStream: TStream): Integer;
    function SendDataStream: Integer;
    function RecvDataStream: Boolean;
    procedure DoError(const AErrCode: Integer; const AParam: string);
    /// <summary> 获取指定的参数 </summary>
    function Param(const AParamName: string): TMsgPack;
  protected
    FMsgPack: TMsgPack;
    function GetHost: string;
    procedure SetHost(const AHost: string);
    function GetPort: Integer;
    procedure SetPort(const APort: Integer);
    function GetActive: Boolean;
    function GetCmd: Integer;
    procedure SetCmd(const Value: Integer);
    function GetBackDataSet: Boolean;
    procedure SetBackDataSet(const Value: Boolean);

    function GetBatch: Boolean;
    procedure SetBatch(const Value: Boolean);

    function GetTrans: Boolean;
    procedure SetTrans(const Value: Boolean);

    function GetTimeOut: Integer;
    procedure SetTimeOut(const Value: Integer);
    //
    function SendDataBuffer(buf:Pointer; len:Cardinal): Cardinal; stdcall;
    function RecvDataBuffer(buf:Pointer; len:Cardinal): Cardinal; stdcall;
    function PeekDataBuffer(buf:Pointer; len:Cardinal): Cardinal; stdcall;
  public
    constructor Create; virtual;
    constructor CreateEx(const AHost: string; const APort: Integer;
      AReconnect: Boolean = True);
    destructor Destroy; override;

    function Connected: Boolean;
    procedure ReConnectServer;

    /// <summary> 向服务端传递客户端调用数据 </summary>
    /// <param name="AMsgPack"></param>
    /// <returns>服务端处理此次调用是否成功(仅表示服务端响应客户端调用成功，不代表方法执行的结果)</returns>
    function DispatchPack(const AMsgPack: TMsgPack): Boolean; overload;
    function DispatchPack: Boolean; overload;

    /// <summary> 存放客户端调用服务端方法时Sql语句的字段参数 </summary>
    function ExecParam: TMsgPack;

    /// <summary> 存放客户端调用服务端方法时Sql语句的替换参数 </summary>
    /// <returns></returns>
    function ReplaceParam: TMsgPack;

    /// <summary> 向服务端调用的方法添加一个要返回的字段 </summary>
    procedure AddBackField(const AFieldName: string);

    /// <summary> 获取服务端方法返回的指定字段数据 </summary>
    function BackField(const AFieldName: string): TMsgPack;

    /// <summary> 客户端调用的服务端具体方法执行是否成功 </summary>
    function MethodRunOk: Boolean;

    // 记录服务端响应方法错误时的信息(BACKMSG)或响应成功时方法执行结果错误时的信息(BLL_METHODMSG)
    function MethodError: string;

    // 记录服务端响应成功时方法执行结果数据集的个数
    function RecordCount: Integer;

    /// <summary> 将返回的业务数据集写入内存流 </summary>
    /// <param name="AStream">存放数据集</param>
    procedure GetBLLDataSet(const AStream: TMemoryStream);

    //property MsgPack: TMsgPack read FMsgPack;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;

    /// <summary> 每次调用服务时重连服务器(调用完服务后断开节省资源) </summary>
    property Reconnect: Boolean read FReconnect write FReconnect;
    property Active: Boolean read GetActive;
    property Cmd: Integer read GetCmd write SetCmd;
    /// <summary> 服务端是否返回数据集 </summary>
    property BackDataSet: Boolean read GetBackDataSet write SetBackDataSet;

    /// <summary> 是否批量处理数据 </summary>
    property Batch: Boolean read GetBatch write SetBatch;

    /// <summary> 服务端处理数据时是否使用事务 </summary>
    property Trans: Boolean read GetTrans write SetTrans;

    property TimeOut: Integer read GetTimeOut write SetTimeOut;
    property ErrCode: Integer read FErrCode;
    property ErrMsg: string read FErrMsg;
  end;

  TBLLServerReadyEvent = reference to procedure(const ABLLServerReady: TBLLServerProxy);
  TBLLServerRunEvent = reference to procedure(const ABLLServerRun: TBLLServerProxy; const AMemTable: TFDMemTable = nil);

  TBLLInvoke = class(TObject)  // 业务服务端
  public
    /// <summary> 创建一个服务端代理 </summary>
    class function GetBLLServerProxy: TBLLServerProxy;
    class function CommonLastError: string;
    /// <summary> 获取服务端时间 </summary>
    class function GetServerDateTime: TDateTime;
    /// <summary> 查询指定表指定字段(不带where条件) </summary>
    /// <param name="AFields">以英文逗号分隔的字段，*表示所有字段</param>
    /// <returns>>=0查询到的数据行数 <0表示查询出错</returns>
    class function Query(const ATable: string; const AFields: string; const AProc: TBLLServerRunEvent): Integer;
    /// <summary> 获取全局系统参数 </summary>
    /// <param name="AParamName">参数名</param>
    /// <returns></returns>
    class function GetParam(const AParamName: string): string;
    class procedure GetParamAll(const AProc: TBLLServerRunEvent);
    /// <summary> 获取业务服务端是否在指定时间内可响应 </summary>
    /// <param name="AMesc">指定的时间</param>
    /// <returns></returns>
    class function GetBLLServerResponse(const AMesc: Word): Boolean;
    /// <summary> 验证用户信息 </summary>
    class procedure Certification(const AUserCert: TUserCert);
    /// <summary> 为指定的病历签名 </summary>
    class function SignatureInchRecord(const ARecordID: Integer; const AUserID: string): Boolean;
    /// <summary> 获取指定病历是否签过名 </summary>
    class function GetInchRecordSignature(const ARecordID: Integer): Boolean;
    /// <summary> 取指定的模板内容 </summary>
    class procedure GetTemplateContent(const ATempID: Cardinal; const AStream: TStream);
    /// <summary> 取指定的病历内容 </summary>
    class procedure GetRecordContent(const ARecordID: Cardinal; const AStream: TStream);
    /// <summary> 取患者住院病历列表 </summary>
    class procedure GetPatientInchRecord(const APatID: string; const AVisit: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> 取患者除某次住院外的其他诊次就诊信息 </summary>
    class procedure GetPatientHisInchInfo(const APatID: string; const AVisit: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> 取指定的数据元属性 </summary>
    class procedure GetDeProperty(const ADeIndex: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> 取指定数据集包含的数据元 </summary>
    class procedure GetDataSetElement(const ADesID: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> 取所有数据元 </summary>
    class procedure GetDataElementTable(const AProc: TBLLServerRunEvent);
    /// <summary> 删除患者指定的病历 </summary>
    class procedure DeletePatientRecord(const ARecordID: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> 删除值域选项关联的内容 </summary>
    class procedure DeleteDomainItemContent(const ADItemID: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> 删除值域某个选项 </summary>
    class procedure DeleteDomainItem(const ADItemID: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> 删除值域所有选项 </summary>
    class procedure DeleteDomainAllItem(const ADomainID: Integer; const AProc: TBLLServerRunEvent);
  end;

  /// <summary> 通过调用指定业务操作执行业务后返回的查询数据 </summary>
  /// <param name="ABLLServerReady">准备调用业务</param>
  /// <param name="ABLLServerRun">操作执行业务后返回的数据</param>
  procedure BLLServerExec(const ABLLServerReady: TBLLServerReadyEvent; const ABLLServerRun: TBLLServerRunEvent);

implementation

uses
  utils_zipTools, utils_byteTools, DiocpError;

procedure BLLServerExec(const ABLLServerReady: TBLLServerReadyEvent; const ABLLServerRun: TBLLServerRunEvent);
var
  vBLLSrvProxy: TBLLServerProxy;
  vMemTable: TFDMemTable;
  vMemStream: TMemoryStream;
begin
  vBLLSrvProxy := TBLLInvoke.GetBLLServerProxy;
  try
    ABLLServerReady(vBLLSrvProxy);  // 设置调用业务

    // 下面是多层的处理，在这里可以利用 vBLLSrvProxy.Cmd 改为两层结构

    if vBLLSrvProxy.DispatchPack then  // 服务端响应成功
    begin
      if vBLLSrvProxy.BackDataSet then  // 返回数据集
      begin
        vMemTable := TFDMemTable.Create(nil);
        vMemStream := TMemoryStream.Create;
        try
          vBLLSrvProxy.GetBLLDataSet(vMemStream);
          vMemStream.Position := 0;
          vMemTable.LoadFromStream(vMemStream, TFDStorageFormat.sfBinary);
        finally
          FreeAndNil(vMemStream);
        end;
      end
      else
        vMemTable := nil;

      ABLLServerRun(vBLLSrvProxy, vMemTable);  // 操作执行业务后返回的查询数据
    end;
  finally
    if vMemTable <> nil then
      FreeAndNil(vMemTable);
    FreeAndNil(vBLLSrvProxy);
  end;
end;

{ TBLLInvoke }

class procedure TBLLInvoke.Certification(const AUserCert: TUserCert);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)  // 获取登录用户的信息
    begin
      ABLLServerReady.Cmd := BLL_CERTIFICATE;  // 核对登录信息
      //vExecParam.I[BLL_VER] := 1;  // 业务版本
      ABLLServerReady.ExecParam.S[TUser.ID] := AUserCert.ID;
      ABLLServerReady.ExecParam.S[TUser.Password] := AUserCert.Password;

      ABLLServerReady.AddBackField(BLL_RECORDCOUNT);
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
      begin
        raise Exception.Create(ABLLServer.MethodError);
        Exit;
      end;

      if ABLLServer.BackField(BLL_RECORDCOUNT).AsInteger = 1 then
        AUserCert.State := cfsPass
      else
      if ABLLServer.BackField(BLL_RECORDCOUNT).AsInteger = 0 then
        AUserCert.State := cfsError
      else
      if ABLLServer.BackField(BLL_RECORDCOUNT).AsInteger > 1 then
        AUserCert.State := cfsConflict;
    end);
end;

class function TBLLInvoke.CommonLastError: string;
begin

end;

class procedure TBLLInvoke.DeleteDomainAllItem(const ADomainID: Integer;
  const AProc: TBLLServerRunEvent);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_DELETEDOMAINALLITEM;  // 删除值域对应的所有选项
      ABLLServerReady.ExecParam.I['DomainID'] := ADomainID;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      AProc(ABLLServer, AMemTable);
    end);
end;

class procedure TBLLInvoke.DeleteDomainItem(const ADItemID: Integer;
  const AProc: TBLLServerRunEvent);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_DELETEDOMAINITEM;  // 删除值域选项
      ABLLServerReady.ExecParam.I['ID'] := ADItemID;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      AProc(ABLLServer, AMemTable);
    end);
end;

class procedure TBLLInvoke.DeleteDomainItemContent(const ADItemID: Integer;
  const AProc: TBLLServerRunEvent);
begin
  BLLServerExec(procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_DELETEDOMAINITEMCONTENT;  // 删除值域选项关联内容
      ABLLServerReady.ExecParam.I['DItemID'] := ADItemID;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      AProc(ABLLServer, AMemTable);
    end);
end;

class procedure TBLLInvoke.DeletePatientRecord(const ARecordID: Integer; const AProc: TBLLServerRunEvent);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_DELETEINCHRECORD;  // 删除指定的住院病历
      ABLLServerReady.ExecParam.I['RID'] := ARecordID;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      AProc(ABLLServer, AMemTable);
    end);
end;

class function TBLLInvoke.GetBLLServerProxy: TBLLServerProxy;
begin
  Result := TBLLServerProxy.CreateEx(ClientCache.ClientParam.BLLServerIP,
    ClientCache.ClientParam.BLLServerPort);

  Result.TimeOut := ClientCache.ClientParam.TimeOut;
  Result.ReConnectServer;
end;

class function TBLLInvoke.GetBLLServerResponse(const AMesc: Word): Boolean;
var
  vServerProxy: TBLLServerProxy;
begin
  Result := False;
  vServerProxy := TBLLServerProxy.CreateEx(ClientCache.ClientParam.BLLServerIP,
    ClientCache.ClientParam.BLLServerPort);
  try
    vServerProxy.TimeOut := AMesc;
    vServerProxy.ReConnectServer;
    Result := vServerProxy.Active;
  finally
    FreeAndNil(vServerProxy);
  end;
end;

class procedure TBLLInvoke.GetDataSetElement(const ADesID: Integer;
  const AProc: TBLLServerRunEvent);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDATASETELEMENT;  // 获取住院病历签名信息
      ABLLServerReady.ExecParam.I['DsID'] := ADesID;
      ABLLServerReady.AddBackField('DeID');
      ABLLServerReady.AddBackField('KX');
      ABLLServerReady.BackDataSet := True;  // 告诉服务端要将查询数据集结果返回
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      AProc(ABLLServer, AMemTable);
    end);
end;

class procedure TBLLInvoke.GetDataElementTable(const AProc: TBLLServerRunEvent);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDATAELEMENT;  // 获取数据元列表
      ABLLServerReady.BackDataSet := True;  // 告诉服务端要将查询数据集结果返回
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      AProc(ABllServer, AMemTable);
    end);
end;

class procedure TBLLInvoke.GetDeProperty(const ADeIndex: Integer;
  const AProc: TBLLServerRunEvent);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDEPROPERTY;  // 获取指定数据元的属性信息
      ABLLServerReady.ExecParam.I['deid'] := ADeIndex;
      ABLLServerReady.AddBackField('frmtp');
      ABLLServerReady.AddBackField('deunit');
      ABLLServerReady.AddBackField('domainid');
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      AProc(ABllServer, AMemTable);
    end);
end;

class function TBLLInvoke.GetInchRecordSignature(
  const ARecordID: Integer): Boolean;
var
  vSignatureCount: Integer;
begin
  Result := False;
  vSignatureCount := 0;

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETINCHRECORDSIGNATURE;  // 获取住院病历签名信息
      ABLLServerReady.ExecParam.I['RID'] := ARecordID;
      ABLLServerReady.BackDataSet := True;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
        raise Exception.Create(ABLLServer.MethodError);

      if AMemTable <> nil then
        vSignatureCount := AMemTable.RecordCount;
    end);

  Result := vSignatureCount > 0;
end;

class function TBLLInvoke.GetParam(const AParamName: string): string;
var
  vBLLSrvProxy: TBLLServerProxy;
  vExecParam: TMsgPack;
begin
  vBLLSrvProxy := GetBLLServerProxy;
  try
    vBLLSrvProxy.Cmd := BLL_COMM_GETPARAM;  // 调用获取服务端参数功能
    vExecParam := vBLLSrvProxy.ExecParam;  // 传递到服务端的参数数据存放的列表
    vExecParam.S['Name'] := AParamName;
    vBLLSrvProxy.AddBackField('value');

    if vBLLSrvProxy.DispatchPack then  // 执行方法成功(不代表方法执行的结果，仅表示服务端成功收到客户端调用请求并且处理完成)
      Result := vBLLSrvProxy.BackField('value').AsString;
  finally
    vBLLSrvProxy.Free;
  end;
end;

class procedure TBLLInvoke.GetParamAll(const AProc: TBLLServerRunEvent);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_COMM_GETPARAM;  // 调用获取服务端参数功能
      ABLLServerReady.BackDataSet := True;  // 告诉服务端要将查询数据集结果返回
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      AProc(ABLLServer, AMemTable);
    end);
end;

class procedure TBLLInvoke.GetPatientHisInchInfo(const APatID: string;
  const AVisit: Integer; const AProc: TBLLServerRunEvent);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_PatientHisInchInfo;  // 查询指定患者非某次住院外的其他次住院就诊信息
      ABLLServerReady.ExecParam.S['PatID'] := APatID;
      ABLLServerReady.ExecParam.I['VisitID'] := AVisit;
      ABLLServerReady.BackDataSet := True;  // 告诉服务端要将查询数据集结果返回
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      AProc(ABLLServer, AMemTable);
    end);
end;

class procedure TBLLInvoke.GetPatientInchRecord(const APatID: string;
  const AVisit: Integer; const AProc: TBLLServerRunEvent);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETINCHRECORDLIST;  // 获取指定的住院患者病历列表
      ABLLServerReady.ExecParam.S['PatID'] := APatID;
      ABLLServerReady.ExecParam.I['VisitID'] := AVisit;
      ABLLServerReady.BackDataSet := True;  // 告诉服务端要将查询数据集结果返回
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      AProc(ABLLServer, AMemTable);
    end);
end;

class procedure TBLLInvoke.GetRecordContent(const ARecordID: Cardinal;
  const AStream: TStream);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETINCHRECORDCONTENT;
      ABLLServerReady.ExecParam.I['RID'] := ARecordID;
      ABLLServerReady.AddBackField('content');
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
        raise Exception.Create(ABLLServer.MethodError);

      ABLLServer.BackField('content').SaveBinaryToStream(AStream);
    end);
end;

class function TBLLInvoke.GetServerDateTime: TDateTime;
var
  vBLLSrvProxy: TBLLServerProxy;
begin
  vBLLSrvProxy := GetBLLServerProxy;
  try
    vBLLSrvProxy.Cmd := BLL_SRVDT;  // 调用获取服务端时间功能
    vBLLSrvProxy.AddBackField('dt');

    if vBLLSrvProxy.DispatchPack then  // 执行方法成功(不代表方法执行的结果，仅表示服务端成功收到客户端调用请求并且处理完成)
      Result := vBLLSrvProxy.BackField('dt').AsDateTime;
  finally
    vBLLSrvProxy.Free;
  end;
end;

class procedure TBLLInvoke.GetTemplateContent(const ATempID: Cardinal;
  const AStream: TStream);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETTEMPLATECONTENT;  // 获取模板分组子分组和模板
      ABLLServerReady.ExecParam.I['TID'] := ATempID;
      ABLLServerReady.AddBackField('content');
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
        raise Exception.Create(ABLLServer.MethodError);

      ABLLServer.BackField('content').SaveBinaryToStream(AStream);
    end);
end;

class function TBLLInvoke.Query(const ATable, AFields: string;
  const AProc: TBLLServerRunEvent): Integer;
var
  vBLLSrvProxy: TBLLServerProxy;
  vMemTable: TFDMemTable;
  vMemStream: TMemoryStream;
begin
  Result := 0;
  vBLLSrvProxy := GetBLLServerProxy;
  try
    vBLLSrvProxy.Cmd := BLL_TABLEQUERY;  // 查询指定表的指定字段
    vBLLSrvProxy.ExecParam.Path('TB').AsString := ATable;
    vBLLSrvProxy.ExecParam.Path('FD').AsString := AFields;

    if vBLLSrvProxy.DispatchPack then  // 执行方法成功(不代表方法执行的结果，仅表示服务端成功收到客户端调用请求并且处理完成)
    begin
      Result := vBLLSrvProxy.BackField(BLL_RECORDCOUNT).AsInteger;

      vMemTable := TFDMemTable.Create(nil);
      try
        vMemStream := TMemoryStream.Create;
        try
          //vBLLSrvProxy.GetBLLDataSet(vMemStream);
          vBLLSrvProxy.BackField(BLL_DATASET).SaveBinaryToStream(vMemStream);
          vMemStream.Position := 0;
          vMemTable.LoadFromStream(vMemStream, TFDStorageFormat.sfBinary);
        finally
          FreeAndNil(vMemStream);
        end;

        AProc(vBLLSrvProxy, vMemTable);  // 操作执行业务后返回的查询数据
      finally
        FreeAndNil(vMemTable)
      end;
    end
    else
      Result := -1;  // 表示执行出错
  finally
    vBLLSrvProxy.Free;
  end;
end;

class function TBLLInvoke.SignatureInchRecord(const ARecordID: Integer;
  const AUserID: string): Boolean;
begin
  Result := False;

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_INCHRECORDSIGNATURE;  // 住院病历签名
      ABLLServerReady.ExecParam.I['RID'] := ARecordID;
      ABLLServerReady.ExecParam.S['UserID'] := AUserID;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
        raise Exception.Create(ABLLServer.MethodError);
    end);

  Result := True;
end;

{ TBLLServerProxy }

procedure TBLLServerProxy.AddBackField(const AFieldName: string);
begin
  Param(BLL_BACKFIELD).Add(AFieldName);
end;

function TBLLServerProxy.BackField(const AFieldName: string): TMsgPack;
begin
  Result := Param(BLL_BACKFIELD).O[AFieldName];
end;

procedure TBLLServerProxy.CheckConnect;
begin
  if (not FTcpClient.Active) then
    FTcpClient.Connect;
end;

function TBLLServerProxy.Connected: Boolean;
begin
  Result := FTcpClient.Active;
end;

constructor TBLLServerProxy.Create;
begin
  inherited Create;
  FErrCode := -1;
  FErrMsg := '';
  FReconnect := True;
  FTcpClient := TDiocpBlockTcpClient.Create(nil);
  FTcpClient.ReadTimeOut := 5000;  // 设置超时等待5秒
  FTcpClient.OnError := DoError;
  FDataStream := TMemoryStream.Create;
  FMsgPack := TMsgPack.Create;
end;

constructor TBLLServerProxy.CreateEx(const AHost: string; const APort: Integer;
  AReconnect: Boolean);
begin
  Create;
  FTcpClient.Host := AHost;
  FTcpClient.Port := APort;
  FReconnect := AReconnect;
end;

destructor TBLLServerProxy.Destroy;
begin
  FTcpClient.Disconnect;
  FTcpClient.Free;
  FDataStream.Free;
  FMsgPack.Free;
  inherited Destroy;
end;

function TBLLServerProxy.DispatchPack(const AMsgPack: TMsgPack): Boolean;
begin
  FErrCode := -1;
  FErrMsg := '';

  CheckConnect;
  // 初始化连接时用到的对象
  FDataStream.Clear;
  // 设置调用时各对象值
  if AMsgPack.I[BLL_VER] < 1 then
    AMsgPack.ForcePathObject(BLL_VER).AsInteger := 1;  // 业务版本
  //AMsgPack.ForcePathObject(BLL_DEVICE).AsInteger := Ord(TDeviceType.cdtMobile);  // 设备类型
  AMsgPack.EncodeToStream(FDataStream);  // 打包传输的调用数据
  TZipTools.ZipStream(FDataStream, FDataStream);  // 压缩传输的调用数据
  SendDataStream;  // 数据发送到服务端
  RecvDataStream;  // 获取服务端返回数据
  TZipTools.UnZipStream(FDataStream, FDataStream);  // 解压缩返回的数据
  FDataStream.Position := 0;
  AMsgPack.DecodeFromStream(FDataStream);  // 解包返回的数据
  Result := AMsgPack.Result;  // 服务端处理此次调用是否成功(仅表示服务端响应客户端调用成功，不代表方法执行的结果)
  if not Result then  // 服务端处理此次调用出错
  begin
    if AMsgPack.ForcePathObject(BACKMSG).AsString <> '' then  // 出错信息
    begin
      FMsgPack.ForcePathObject(BLL_ERROR).AsString := '【服务端错误】'
        + sLineBreak + AMsgPack.ForcePathObject(BACKMSG).AsString;
    end;
  end
  else  // 方法执行返回错误信息
  begin
    if AMsgPack.ForcePathObject(BLL_METHODMSG).AsString <> '' then
    begin
      FMsgPack.ForcePathObject(BLL_ERROR).AsString := '【执行方法错误】'
        + sLineBreak + AMsgPack.ForcePathObject(BLL_METHODMSG).AsString;
    end;
  end;

  if FReconnect then  // 调用完服务后断开，节省资源，以后可改为代理池
    FTcpClient.Disconnect;
end;

function TBLLServerProxy.DispatchPack: Boolean;
begin
  Result := DispatchPack(FMsgPack);
end;

procedure TBLLServerProxy.DoError(const AErrCode: Integer;
  const AParam: string);
begin
  FErrCode := AErrCode;
  FErrMsg := AParam;
end;

function TBLLServerProxy.ExecParam: TMsgPack;
begin
  Result := Param(BLL_EXECPARAM);
end;

function TBLLServerProxy.GetActive: Boolean;
begin
  Result := FTcpClient.Active;
end;

function TBLLServerProxy.GetBackDataSet: Boolean;
begin
  Result := Param(BLL_BACKDATASET).AsBoolean;
end;

function TBLLServerProxy.GetBatch: Boolean;
begin
  Result := Param(BLL_BATCH).AsBoolean;
end;

procedure TBLLServerProxy.GetBLLDataSet(const AStream: TMemoryStream);
begin
  if FMsgPack.O[BLL_DATASET] <> nil then
    FMsgPack.O[BLL_DATASET].SaveBinaryToStream(AStream)
  else
    AStream.Size := 0;
end;

function TBLLServerProxy.GetCmd: Integer;
begin
  Result := FMsgPack.ForcePathObject(BLL_CMD).AsInteger;
end;

function TBLLServerProxy.GetHost: string;
begin
  Result := FTcpClient.Host;
end;

function TBLLServerProxy.GetPort: Integer;
begin
  Result := FTcpClient.Port;
end;

function TBLLServerProxy.GetTimeOut: Integer;
begin
  Result := FTcpClient.ReadTimeOut;
end;

function TBLLServerProxy.GetTrans: Boolean;
begin
  Result := Param(BLL_TRANS).AsBoolean;
end;

function TBLLServerProxy.MethodError: string;
begin
  Result := Param(BLL_ERROR).AsString;
end;

function TBLLServerProxy.MethodRunOk: Boolean;
begin
  Result := Param(BLL_METHODRESULT).AsBoolean;
end;

function TBLLServerProxy.Param(const AParamName: string): TMsgPack;
begin
  Result := FMsgPack.ForcePathObject(AParamName);
end;

function TBLLServerProxy.PeekDataBuffer(buf: Pointer; len: Cardinal): Cardinal;
begin
  if FReconnect then
  begin
    if not FTcpClient.Active then
      FTcpClient.Connect;
    try
      FTcpClient.Recv(buf, len);
      Result := len;
    except
      FTcpClient.Disconnect;
      raise;
    end;
  end
  else
  begin
    FTcpClient.Recv(buf, len);
    Result := len;
  end;
end;

procedure TBLLServerProxy.ReConnectServer;
begin
  CheckConnect;
end;

function TBLLServerProxy.RecordCount: Integer;
begin
  Result := Param(BLL_RECORDCOUNT).AsInteger;
end;

function TBLLServerProxy.RecvDataBuffer(buf: Pointer; len: Cardinal): Cardinal;
begin
  if FReconnect then
  begin
    if not FTcpClient.Active then
      FTcpClient.Connect;
    try
      FTcpClient.Recv(buf, len);
      Result := len;
    except
      FTcpClient.Disconnect;
      raise;
    end;
  end
  else
  begin
    FTcpClient.Recv(buf, len);
    Result := len;
  end;
end;

function TBLLServerProxy.RecvDataStream: Boolean;
var
  vBytes: TBytes;
  vReadLen, vTempLen: Integer;
  vPACK_FLAG: Word;
  vDataLen: Integer;
  vVerifyValue, vVerifyDataValue: Cardinal;
  vPByte: PByte;
begin
  RecvDataBuffer(@vPACK_FLAG, 2);

  if vPACK_FLAG <> PACK_FLAG then  // 错误的包数据
  begin
    FTcpClient.Disconnect;
    raise Exception.Create(strRecvException_ErrorFlag);
  end;

  //veri value
  RecvDataBuffer(@vVerifyValue, SizeOf(vVerifyValue));

  //headlen
  RecvDataBuffer(@vReadLen, SizeOf(vReadLen));
  vDataLen := TByteTools.swap32(vReadLen);

  if vDataLen > MAX_OBJECT_SIZE then  // 文件头过大，错误的包数据
  begin
    FTcpClient.Disconnect;
    raise Exception.Create(strRecvException_ErrorData);
  end;

  SetLength(vBytes,vDataLen);
  vPByte := PByte(@vBytes[0]);
  vReadLen := 0;
  while vReadLen < vDataLen do
  begin
    vTempLen := RecvDataBuffer(vPByte, vDataLen - vReadLen);
    if vTempLen = -1 then
    begin
      RaiseLastOSError;
    end;
    Inc(vPByte, vTempLen);
    vReadLen := vReadLen + vTempLen;
  end;

{$IFDEF POSIX}
  vVerifyDataValue := verifyData(lvBytes[0], lvDataLen);
{$ELSE}
  vVerifyDataValue := verifyData(vBytes[0], vDataLen);
{$ENDIF}

  if vVerifyDataValue <> vVerifyValue then
    raise Exception.Create(strRecvException_VerifyErr);

  FDataStream.Clear;
  FDataStream.Write(vBytes[0], vDataLen);
  Result := true;
end;

function TBLLServerProxy.ReplaceParam: TMsgPack;
begin
  Result := Param(BLL_REPLACEPARAM);
end;

function TBLLServerProxy.SendDataBuffer(buf: Pointer; len: Cardinal): Cardinal;
begin
  if FReconnect then
  begin
    if not FTcpClient.Active then
      FTcpClient.Connect;
    try
      Result := FTcpClient.SendBuffer(buf, len);
    except
      FTcpClient.Disconnect;
      raise;
    end;
  end
  else
    Result := FTcpClient.SendBuffer(buf, len);
end;

function TBLLServerProxy.SendDataStream: Integer;
var
  lvPACK_FLAG: WORD;
  lvDataLen, lvWriteIntValue: Integer;
  lvBuf: TBytes;
  lvStream: TMemoryStream;
  lvVerifyValue: Cardinal;
begin
  lvPACK_FLAG := PACK_FLAG;

  lvStream := TMemoryStream.Create;
  try
    FDataStream.Position := 0;

    if FDataStream.Size > MAX_OBJECT_SIZE then
      raise Exception.CreateFmt(strSendException_TooBig, [MAX_OBJECT_SIZE]);

    lvStream.Write(lvPACK_FLAG, 2);  // 包头

    lvDataLen := FDataStream.Size;

    // stream data
    SetLength(lvBuf, lvDataLen);
    FDataStream.Read(lvBuf[0], lvDataLen);
    //veri value
    lvVerifyValue := verifyData(lvBuf[0], lvDataLen);

    lvStream.Write(lvVerifyValue, SizeOf(lvVerifyValue));

    lvWriteIntValue := TByteTools.swap32(lvDataLen);

    // stream len
    lvStream.Write(lvWriteIntValue, SizeOf(lvWriteIntValue));

    // send pack
    lvStream.write(lvBuf[0], lvDataLen);

    Result := SendStream(lvStream);
  finally
    lvStream.Free;
  end;
end;

function TBLLServerProxy.SendStream(pvStream: TStream): Integer;
var
  lvBufBytes: array[0..MAX_BLOCK_SIZE - 1] of byte;
  l, j, r, lvTotal: Integer;
  P: PByte;
begin
  Result := 0;
  if pvStream = nil then Exit;
  if pvStream.Size = 0 then Exit;
  lvTotal :=0;

  pvStream.Position := 0;
  repeat
    //FillMemory(@lvBufBytes[0], SizeOf(lvBufBytes), 0);
    l := pvStream.Read(lvBufBytes[0], SizeOf(lvBufBytes));
    if (l > 0) then
    begin
      P := PByte(@lvBufBytes[0]);
      j := l;
      while j > 0 do
      begin
        r := SendDataBuffer(P, j);
        if r = -1 then
          RaiseLastOSError;
        Inc(P, r);
        Dec(j, r);
      end;
      lvTotal := lvTotal + l;
    end
    else
      Break;
  until (l = 0);
  Result := lvTotal;
end;

procedure TBLLServerProxy.SetBackDataSet(const Value: Boolean);
begin
  Param(BLL_BACKDATASET).AsBoolean := Value;
end;

procedure TBLLServerProxy.SetBatch(const Value: Boolean);
begin
  Param(BLL_BATCH).AsBoolean := Value;
end;

procedure TBLLServerProxy.SetCmd(const Value: Integer);
begin
  FMsgPack.ForcePathObject(BLL_CMD).AsInteger := Value;
end;

procedure TBLLServerProxy.SetHost(const AHost: string);
begin
  FTcpClient.Host := AHost;
end;

procedure TBLLServerProxy.SetPort(const APort: Integer);
begin
  FTcpClient.Port := APort;
end;

procedure TBLLServerProxy.SetTimeOut(const Value: Integer);
begin
  FTcpClient.ReadTimeOut := Value;
end;

procedure TBLLServerProxy.SetTrans(const Value: Boolean);
begin
  Param(BLL_TRANS).AsBoolean := Value;
end;

end.