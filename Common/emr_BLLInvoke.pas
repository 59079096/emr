{*******************************************************}
{                                                       }
{         ����HCView�ĵ��Ӳ�������  ���ߣ���ͨ          }
{                                                       }
{ �˴������ѧϰ����ʹ�ã�����������ҵĿ�ģ��ɴ�������  }
{ �����ʹ���߳е�������QQȺ 649023932 ����ȡ����ļ��� }
{ ������                                                }
{                                                       }
{*******************************************************}

unit emr_BLLInvoke;

interface

uses
  Classes, SysUtils, emr_Common, emr_Entry, emr_MsgPack,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, Data.DB, FireDAC.Stan.Intf,
  FireDAC.Stan.StorageBin, diocp_tcp_blockClient;

const
  /// <summary> ��ȡ��������ǰʱ�� </summary>
  BLL_SRVDT = 1;

  /// <summary> ִ��Sql��� </summary>
  BLL_EXECSQL = 2;

  /// <summary> ��ȡ���б�ͱ�˵�� </summary>
  BLL_GETAllTABLE = 3;

  /// <summary> ��������(���롢����)��ָ���ı� ���TTableOper��ĳ��� </summary>
  BLL_TABLESAVE = 4;

  /// <summary> ��ѯָ�����ָ���ֶ� </summary>
  BLL_TABLEQUERY = 5;

  BLL_BASE = 1000;  // ҵ������ʼֵ

  { ҵ����(��1000��ʼ) }
  /// <summary> ȫ���û� </summary>
  BLL_COMM_ALLUSER = BLL_BASE;

  /// <summary> ��֤�˺�����ƥ�� </summary>
  BLL_CERTIFICATE = BLL_BASE + 1;

  /// <summary> ��ȡָ���û���Ϣ </summary>
  BLL_GETUSERINFO = BLL_BASE + 2;

  /// <summary> ��ȡ�û��Ĺ����� </summary>
  BLL_GETUSERGROUPS = BLL_BASE + 3;

  /// <summary> ��ȡ�û��Ľ�ɫ </summary>
  BLL_GETUSERROLES = BLL_BASE + 4;

  /// <summary> ��ȡָ���û����õ����й��� </summary>
  BLL_GETUSERFUNS = BLL_BASE + 5;

  /// <summary> ��ȡָ���û����й������Ӧ�Ŀ��� </summary>
  BLL_GETUSERGROUPDEPTS = BLL_BASE + 6;

  /// <summary> ��ȡ���� </summary>
  BLL_COMM_GETPARAM = BLL_BASE + 7;

  /// <summary> ��ȡ����˻�������� </summary>
  BLL_GETCLIENTCACHE = BLL_BASE + 8;

  /// <summary> ��ȡָ��������������Ȩ�޿��ƵĿؼ� </summary>
  BLL_GETCONTROLSAUTH = BLL_BASE + 9;

  /// <summary> ��ȡҪ���������°汾�� </summary>
  BLL_GETLASTVERSION = BLL_BASE + 10;

  /// <summary> ��ȡҪ�������ļ� </summary>
  BLL_GETUPDATEINFO = BLL_BASE + 11;

  /// <summary> �ϴ�������Ϣ </summary>
  BLL_UPLOADUPDATEINFO = BLL_BASE + 12;

  /// <summary> ��ȡ��Ժ���� </summary>
  BLL_HIS_GETINPATIENT = BLL_BASE + 13;

  /// <summary> ��ȡ���ݼ�(��Ŀ¼)��Ϣ </summary>
  BLL_GETDATAELEMENTSETROOT = BLL_BASE + 14;

  /// <summary> ��ȡ���ݼ�(ȫĿ¼)��Ϣ </summary>
  BLL_GETDATAELEMENTSETALL = BLL_BASE + 15;

  /// <summary> ��ȡָ�����ݼ���Ӧ��ģ�� </summary>
  BLL_GETTEMPLATELIST = BLL_BASE + 16;

  /// <summary> �½�ģ�� </summary>
  BLL_NEWTEMPLATE = BLL_BASE + 17;

  /// <summary> ��ȡģ������ </summary>
  BLL_GETTEMPLATECONTENT = BLL_BASE + 18;

  /// <summary> ����ģ������ </summary>
  BLL_SAVETEMPLATECONTENT = BLL_BASE + 19;

  /// <summary> ɾ��ģ�弰���� </summary>
  BLL_DELETETEMPLATE = BLL_BASE + 20;

  /// <summary> ��ȡ����Ԫ </summary>
  BLL_GETDATAELEMENT = BLL_BASE + 21;

  /// <summary> ��ȡ����Ԫֵ��ѡ�� </summary>
  BLL_GETDOMAINITEM = BLL_BASE + 22;

  /// <summary> ��������Ԫѡ��ֵ���Ӧ������ </summary>
  BLL_SAVEDOMAINITEMCONTENT = BLL_BASE + 23;

  /// <summary> ��ȡ����Ԫѡ��ֵ���Ӧ������ </summary>
  BLL_GETDOMAINITEMCONTENT = BLL_BASE + 24;

  /// <summary> ɾ������Ԫѡ��ֵ���Ӧ������ </summary>
  BLL_DELETEDOMAINITEMCONTENT = BLL_BASE + 25;

  /// <summary> ��ȡָ����סԺ���߲����б� </summary>
  BLL_GETINCHRECORDLIST = BLL_BASE + 26;

  /// <summary> �½�סԺ���� </summary>
  BLL_NEWINCHRECORD = BLL_BASE + 27;

  /// <summary> ��ȡָ��סԺ�������� </summary>
  BLL_GETINCHRECORDCONTENT = BLL_BASE + 28;

  /// <summary> ����ָ��סԺ�������� </summary>
  BLL_SAVERECORDCONTENT = BLL_BASE + 29;

  /// <summary> ��ȡָ���������ݼ�(��Ŀ¼)��Ӧ�Ĳ������� </summary>
  BLL_GETDESETRECORDCONTENT = BLL_BASE + 30;

  /// <summary> ɾ��ָ����סԺ���� </summary>
  BLL_DELETEINCHRECORD = BLL_BASE + 31;

  /// <summary> ��ȡָ������Ԫ��������Ϣ </summary>
  BLL_GETDEPROPERTY = BLL_BASE + 32;

  /// <summary> סԺ����ǩ�� </summary>
  BLL_INCHRECORDSIGNATURE = BLL_BASE + 33;

  /// <summary> ��ȡסԺ����ǩ����Ϣ </summary>
  BLL_GETINCHRECORDSIGNATURE = BLL_BASE + 34;

  /// <summary> ��ȡģ����Ϣ </summary>
  BLL_GETTEMPLATEINFO = BLL_BASE + 35;

  /// <summary> �޸�ģ����Ϣ </summary>
  BLL_SETTEMPLATEINFO = BLL_BASE + 36;

  /// <summary> ��ȡָ������Ԫ��Ϣ </summary>
  BLL_GETDEINFO = BLL_BASE + 37;

  /// <summary> �޸�ָ������Ԫ��Ϣ </summary>
  BLL_SETDEINFO = BLL_BASE + 38;

  /// <summary> �½�����Ԫ </summary>
  BLL_NEWDE = BLL_BASE + 39;

  /// <summary> ɾ������Ԫ </summary>
  BLL_DELETEDE = BLL_BASE + 40;

  /// <summary> ��ȡָ����Ԫֵ��ѡ�� </summary>
  BLL_GETDOMAINITEMINFO = BLL_BASE + 41;

  /// <summary> �޸�����Ԫֵ��ѡ�� </summary>
  BLL_SETDOMAINITEMINFO = BLL_BASE + 42;

  /// <summary> �½�����Ԫֵ��ѡ�� </summary>
  BLL_NEWDOMAINITEM = BLL_BASE + 43;

  /// <summary> ɾ������Ԫֵ��ѡ�� </summary>
  BLL_DELETEDOMAINITEM = BLL_BASE + 44;

  /// <summary> ��ȡ����ֵ�� </summary>
  BLL_GETDOMAIN = BLL_BASE + 45;

  /// <summary> �½�ֵ�� </summary>
  BLL_NEWDOMAIN = BLL_BASE + 46;

  /// <summary> �޸�ֵ�� </summary>
  BLL_SETDOMAIN = BLL_BASE + 47;

  /// <summary> ɾ��ֵ�� </summary>
  BLL_DELETEDOMAIN = BLL_BASE + 48;

  /// <summary> ɾ��ֵ���Ӧ������ѡ�� </summary>
  BLL_DELETEDOMAINALLITEM = BLL_BASE + 49;

  /// <summary> ��ȡ���ݼ���������������Ԫ </summary>
  BLL_GETDATASETELEMENT = BLL_BASE + 50;
  {
  /// <summary> �½�סԺ������Ϣ </summary>
  BLL_BASE + 51;

  /// <summary> �½�����������Ϣ </summary>
  BLL_BASE + 52;

  /// <summary> ��ѯ����������Ϣ </summary>
  BLL_BASE + 53;

  /// <summary> �½�סԺ������� </summary>
  BLL_BASE + 54;   }

  /// <summary> ���没���ṹ���� </summary>
  BLL_SAVERECORDSTRUCTURE = BLL_BASE + 55;

  /// <summary> �޸Ĳ����ṹ���� </summary>
  BLL_UPDATERECORDSTRUCTURE = BLL_BASE + 56;

  /// <summary> ��ȡȡָ���Ĳ����ṹ���� </summary>
  BLL_GETRECORDSTRUCTURE = BLL_BASE + 57;

  /// <summary> ��ȡָ�����ݼ��ĺ��滻��Ϣ </summary>
  BLL_GetDataElementSetMacro = BLL_BASE + 58;

  /// <summary> ��ȡָ������ָ�����ݼ��Ĳ����ṹ���� </summary>
  BLL_GetPatDesStructure = BLL_BASE + 59;

  /// <summary> ��Ӳ���������Ϣ </summary>
  BLL_NewLockInRecord = BLL_BASE + 60;

  /// <summary> ��ȡָ���Ĳ�����ǰ�༭������Ϣ </summary>
  BLL_GetInRecordLock = BLL_BASE + 61;

  /// <summary> ɾ��ָ���Ĳ����༭������Ϣ </summary>
  BLL_DeleteInRecordLock = BLL_BASE + 62;

  /// <summary> ��ȡָ������Ԫ�Ŀ��ƽű� </summary>
  BLL_GetDataElementScript = BLL_BASE + 63;

  /// <summary> ����ָ������Ԫ�Ŀ��ƽű� </summary>
  BLL_SetDataElementScript = BLL_BASE + 64;

  /// <summary> ��ѯָ�����߷�ĳ��סԺ���������סԺ������Ϣ </summary>
  BLL_PatientHisInchInfo = BLL_BASE + 67;

  /// <summary> �ļ�����PDF </summary>
  BLL_RecordToPDF = BLL_BASE + 68;

type
  TTableOper = class(TObject)
  public
    const
      /// <summary> Ҫ�����ı��� </summary>
      Table = 'TB';
      /// <summary> Ҫ�����������������";"���� </summary>
      PrimKeys = 'PK';
      /// <summary> Ҫ�������ֶΣ������";"���� </summary>
      Fields = 'FD';
  end;

  TBLLServerProxy = class(TObject)
  private
    FReconnect: Boolean;
    FTcpClient: TDiocpBlockTcpClient;
    FDataStream: TMemoryStream;
    FErrCode: Integer;  // �������
    FErrMsg: string;  // ��������ʱ�� ip�Ͷ˿�
    procedure CheckConnect;
    function SendStream(pvStream: TStream): Integer;
    function SendDataStream: Integer;
    function RecvDataStream: Boolean;
    procedure DoError(const AErrCode: Integer; const AParam: string);
    /// <summary> ��ȡָ���Ĳ��� </summary>
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

    /// <summary> �����˴��ݿͻ��˵������� </summary>
    /// <param name="AMsgPack"></param>
    /// <returns>����˴���˴ε����Ƿ�ɹ�(����ʾ�������Ӧ�ͻ��˵��óɹ�����������ִ�еĽ��)</returns>
    function DispatchPack(const AMsgPack: TMsgPack): Boolean; overload;
    function DispatchPack: Boolean; overload;

    /// <summary> ��ſͻ��˵��÷���˷���ʱSql�����ֶβ��� </summary>
    function ExecParam: TMsgPack;

    /// <summary> ��ſͻ��˵��÷���˷���ʱSql�����滻���� </summary>
    /// <returns></returns>
    function ReplaceParam: TMsgPack;

    /// <summary> �����˵��õķ������һ��Ҫ���ص��ֶ� </summary>
    procedure AddBackField(const AFieldName: string);

    /// <summary> ��ȡ����˷������ص�ָ���ֶ����� </summary>
    function BackField(const AFieldName: string): TMsgPack;

    /// <summary> �ͻ��˵��õķ���˾��巽��ִ���Ƿ�ɹ� </summary>
    function MethodRunOk: Boolean;

    // ��¼�������Ӧ��������ʱ����Ϣ(BACKMSG)����Ӧ�ɹ�ʱ����ִ�н������ʱ����Ϣ(BLL_METHODMSG)
    function MethodError: string;

    // ��¼�������Ӧ�ɹ�ʱ����ִ�н�����ݼ��ĸ���
    function RecordCount: Integer;

    /// <summary> �����ص�ҵ�����ݼ�д���ڴ��� </summary>
    /// <param name="AStream">������ݼ�</param>
    procedure GetBLLDataSet(const AStream: TMemoryStream);

    //property MsgPack: TMsgPack read FMsgPack;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;

    /// <summary> ÿ�ε��÷���ʱ����������(����������Ͽ���ʡ��Դ) </summary>
    property Reconnect: Boolean read FReconnect write FReconnect;
    property Active: Boolean read GetActive;
    property Cmd: Integer read GetCmd write SetCmd;
    /// <summary> ������Ƿ񷵻����ݼ� </summary>
    property BackDataSet: Boolean read GetBackDataSet write SetBackDataSet;

    /// <summary> �Ƿ������������� </summary>
    property Batch: Boolean read GetBatch write SetBatch;

    /// <summary> ����˴�������ʱ�Ƿ�ʹ������ </summary>
    property Trans: Boolean read GetTrans write SetTrans;

    property TimeOut: Integer read GetTimeOut write SetTimeOut;
    property ErrCode: Integer read FErrCode;
    property ErrMsg: string read FErrMsg;
  end;

  TBLLServerReadyEvent = reference to procedure(const ABLLServerReady: TBLLServerProxy);
  TBLLServerRunEvent = reference to procedure(const ABLLServerRun: TBLLServerProxy; const AMemTable: TFDMemTable = nil);

  TBLLInvoke = class(TObject)  // ҵ������
  public
    /// <summary> ����һ������˴��� </summary>
    class function GetBLLServerProxy: TBLLServerProxy;
    class function CommonLastError: string;
    /// <summary> ��ȡ�����ʱ�� </summary>
    class function GetServerDateTime: TDateTime;
    /// <summary> ��ѯָ����ָ���ֶ�(����where����) </summary>
    /// <param name="AFields">��Ӣ�Ķ��ŷָ����ֶΣ�*��ʾ�����ֶ�</param>
    /// <returns>>=0��ѯ������������ <0��ʾ��ѯ����</returns>
    class function Query(const ATable: string; const AFields: string; const AProc: TBLLServerRunEvent): Integer;
    /// <summary> ��ȡȫ��ϵͳ���� </summary>
    /// <param name="AParamName">������</param>
    /// <returns></returns>
    class function GetParam(const AParamName: string): string;
    class procedure GetParamAll(const AProc: TBLLServerRunEvent);
    /// <summary> ��ȡҵ�������Ƿ���ָ��ʱ���ڿ���Ӧ </summary>
    /// <param name="AMesc">ָ����ʱ��</param>
    /// <returns></returns>
    class function GetBLLServerResponse(const AMesc: Word): Boolean;
    /// <summary> ��֤�û���Ϣ </summary>
    class procedure Certification(const AUserCert: TUserCert);
    /// <summary> Ϊָ���Ĳ���ǩ�� </summary>
    class function SignatureInchRecord(const ARecordID: Integer; const AUserID: string): Boolean;
    /// <summary> ��ȡָ�������Ƿ�ǩ���� </summary>
    class function GetInchRecordSignature(const ARecordID: Integer): Boolean;
    /// <summary> ȡָ����ģ������ </summary>
    class procedure GetTemplateContent(const ATempID: Cardinal; const AStream: TStream);
    /// <summary> ȡָ���Ĳ������� </summary>
    class procedure GetRecordContent(const ARecordID: Cardinal; const AStream: TStream);
    /// <summary> ȡ����סԺ�����б� </summary>
    class procedure GetPatientInchRecord(const APatID: string; const AVisit: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> ȡ���߳�ĳ��סԺ���������ξ�����Ϣ </summary>
    class procedure GetPatientHisInchInfo(const APatID: string; const AVisit: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> ȡָ��������Ԫ���� </summary>
    class procedure GetDeProperty(const ADeIndex: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> ȡָ�����ݼ�����������Ԫ </summary>
    class procedure GetDataSetElement(const ADesID: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> ȡ��������Ԫ </summary>
    class procedure GetDataElementTable(const AProc: TBLLServerRunEvent);
    /// <summary> ɾ������ָ���Ĳ��� </summary>
    class procedure DeletePatientRecord(const ARecordID: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> ɾ��ֵ��ѡ����������� </summary>
    class procedure DeleteDomainItemContent(const ADItemID: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> ɾ��ֵ��ĳ��ѡ�� </summary>
    class procedure DeleteDomainItem(const ADItemID: Integer; const AProc: TBLLServerRunEvent);
    /// <summary> ɾ��ֵ������ѡ�� </summary>
    class procedure DeleteDomainAllItem(const ADomainID: Integer; const AProc: TBLLServerRunEvent);
  end;

  /// <summary> ͨ������ָ��ҵ�����ִ��ҵ��󷵻صĲ�ѯ���� </summary>
  /// <param name="ABLLServerReady">׼������ҵ��</param>
  /// <param name="ABLLServerRun">����ִ��ҵ��󷵻ص�����</param>
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
    ABLLServerReady(vBLLSrvProxy);  // ���õ���ҵ��

    // �����Ƕ��Ĵ���������������� vBLLSrvProxy.Cmd ��Ϊ����ṹ

    if vBLLSrvProxy.DispatchPack then  // �������Ӧ�ɹ�
    begin
      if vBLLSrvProxy.BackDataSet then  // �������ݼ�
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

      ABLLServerRun(vBLLSrvProxy, vMemTable);  // ����ִ��ҵ��󷵻صĲ�ѯ����
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
    procedure(const ABLLServerReady: TBLLServerProxy)  // ��ȡ��¼�û�����Ϣ
    begin
      ABLLServerReady.Cmd := BLL_CERTIFICATE;  // �˶Ե�¼��Ϣ
      //vExecParam.I[BLL_VER] := 1;  // ҵ��汾
      ABLLServerReady.ExecParam.S[TUser.ID] := AUserCert.ID;
      ABLLServerReady.ExecParam.S[TUser.Password] := AUserCert.Password;

      ABLLServerReady.AddBackField(BLL_RECORDCOUNT);
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // ����˷�������ִ�в��ɹ�
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
      ABLLServerReady.Cmd := BLL_DELETEDOMAINALLITEM;  // ɾ��ֵ���Ӧ������ѡ��
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
      ABLLServerReady.Cmd := BLL_DELETEDOMAINITEM;  // ɾ��ֵ��ѡ��
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
      ABLLServerReady.Cmd := BLL_DELETEDOMAINITEMCONTENT;  // ɾ��ֵ��ѡ���������
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
      ABLLServerReady.Cmd := BLL_DELETEINCHRECORD;  // ɾ��ָ����סԺ����
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
      ABLLServerReady.Cmd := BLL_GETDATASETELEMENT;  // ��ȡסԺ����ǩ����Ϣ
      ABLLServerReady.ExecParam.I['DsID'] := ADesID;
      ABLLServerReady.AddBackField('DeID');
      ABLLServerReady.AddBackField('KX');
      ABLLServerReady.BackDataSet := True;  // ���߷����Ҫ����ѯ���ݼ��������
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
      ABLLServerReady.Cmd := BLL_GETDATAELEMENT;  // ��ȡ����Ԫ�б�
      ABLLServerReady.BackDataSet := True;  // ���߷����Ҫ����ѯ���ݼ��������
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
      ABLLServerReady.Cmd := BLL_GETDEPROPERTY;  // ��ȡָ������Ԫ��������Ϣ
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
      ABLLServerReady.Cmd := BLL_GETINCHRECORDSIGNATURE;  // ��ȡסԺ����ǩ����Ϣ
      ABLLServerReady.ExecParam.I['RID'] := ARecordID;
      ABLLServerReady.BackDataSet := True;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // ����˷�������ִ�в��ɹ�
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
    vBLLSrvProxy.Cmd := BLL_COMM_GETPARAM;  // ���û�ȡ����˲�������
    vExecParam := vBLLSrvProxy.ExecParam;  // ���ݵ�����˵Ĳ������ݴ�ŵ��б�
    vExecParam.S['Name'] := AParamName;
    vBLLSrvProxy.AddBackField('value');

    if vBLLSrvProxy.DispatchPack then  // ִ�з����ɹ�(��������ִ�еĽ��������ʾ����˳ɹ��յ��ͻ��˵��������Ҵ������)
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
      ABLLServerReady.Cmd := BLL_COMM_GETPARAM;  // ���û�ȡ����˲�������
      ABLLServerReady.BackDataSet := True;  // ���߷����Ҫ����ѯ���ݼ��������
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
      ABLLServerReady.Cmd := BLL_PatientHisInchInfo;  // ��ѯָ�����߷�ĳ��סԺ���������סԺ������Ϣ
      ABLLServerReady.ExecParam.S['PatID'] := APatID;
      ABLLServerReady.ExecParam.I['VisitID'] := AVisit;
      ABLLServerReady.BackDataSet := True;  // ���߷����Ҫ����ѯ���ݼ��������
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
      ABLLServerReady.Cmd := BLL_GETINCHRECORDLIST;  // ��ȡָ����סԺ���߲����б�
      ABLLServerReady.ExecParam.S['PatID'] := APatID;
      ABLLServerReady.ExecParam.I['VisitID'] := AVisit;
      ABLLServerReady.BackDataSet := True;  // ���߷����Ҫ����ѯ���ݼ��������
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
      if not ABLLServer.MethodRunOk then  // ����˷�������ִ�в��ɹ�
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
    vBLLSrvProxy.Cmd := BLL_SRVDT;  // ���û�ȡ�����ʱ�书��
    vBLLSrvProxy.AddBackField('dt');

    if vBLLSrvProxy.DispatchPack then  // ִ�з����ɹ�(��������ִ�еĽ��������ʾ����˳ɹ��յ��ͻ��˵��������Ҵ������)
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
      ABLLServerReady.Cmd := BLL_GETTEMPLATECONTENT;  // ��ȡģ������ӷ����ģ��
      ABLLServerReady.ExecParam.I['TID'] := ATempID;
      ABLLServerReady.AddBackField('content');
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // ����˷�������ִ�в��ɹ�
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
    vBLLSrvProxy.Cmd := BLL_TABLEQUERY;  // ��ѯָ�����ָ���ֶ�
    vBLLSrvProxy.ExecParam.Path('TB').AsString := ATable;
    vBLLSrvProxy.ExecParam.Path('FD').AsString := AFields;

    if vBLLSrvProxy.DispatchPack then  // ִ�з����ɹ�(��������ִ�еĽ��������ʾ����˳ɹ��յ��ͻ��˵��������Ҵ������)
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

        AProc(vBLLSrvProxy, vMemTable);  // ����ִ��ҵ��󷵻صĲ�ѯ����
      finally
        FreeAndNil(vMemTable)
      end;
    end
    else
      Result := -1;  // ��ʾִ�г���
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
      ABLLServerReady.Cmd := BLL_INCHRECORDSIGNATURE;  // סԺ����ǩ��
      ABLLServerReady.ExecParam.I['RID'] := ARecordID;
      ABLLServerReady.ExecParam.S['UserID'] := AUserID;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // ����˷�������ִ�в��ɹ�
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
  FTcpClient.ReadTimeOut := 5000;  // ���ó�ʱ�ȴ�5��
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
  // ��ʼ������ʱ�õ��Ķ���
  FDataStream.Clear;
  // ���õ���ʱ������ֵ
  if AMsgPack.I[BLL_VER] < 1 then
    AMsgPack.ForcePathObject(BLL_VER).AsInteger := 1;  // ҵ��汾
  //AMsgPack.ForcePathObject(BLL_DEVICE).AsInteger := Ord(TDeviceType.cdtMobile);  // �豸����
  AMsgPack.EncodeToStream(FDataStream);  // �������ĵ�������
  TZipTools.ZipStream(FDataStream, FDataStream);  // ѹ������ĵ�������
  SendDataStream;  // ���ݷ��͵������
  RecvDataStream;  // ��ȡ����˷�������
  TZipTools.UnZipStream(FDataStream, FDataStream);  // ��ѹ�����ص�����
  FDataStream.Position := 0;
  AMsgPack.DecodeFromStream(FDataStream);  // ������ص�����
  Result := AMsgPack.Result;  // ����˴���˴ε����Ƿ�ɹ�(����ʾ�������Ӧ�ͻ��˵��óɹ�����������ִ�еĽ��)
  if not Result then  // ����˴���˴ε��ó���
  begin
    if AMsgPack.ForcePathObject(BACKMSG).AsString <> '' then  // ������Ϣ
    begin
      FMsgPack.ForcePathObject(BLL_ERROR).AsString := '������˴���'
        + sLineBreak + AMsgPack.ForcePathObject(BACKMSG).AsString;
    end;
  end
  else  // ����ִ�з��ش�����Ϣ
  begin
    if AMsgPack.ForcePathObject(BLL_METHODMSG).AsString <> '' then
    begin
      FMsgPack.ForcePathObject(BLL_ERROR).AsString := '��ִ�з�������'
        + sLineBreak + AMsgPack.ForcePathObject(BLL_METHODMSG).AsString;
    end;
  end;

  if FReconnect then  // ����������Ͽ�����ʡ��Դ���Ժ�ɸ�Ϊ�����
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

  if vPACK_FLAG <> PACK_FLAG then  // ����İ�����
  begin
    FTcpClient.Disconnect;
    raise Exception.Create(strRecvException_ErrorFlag);
  end;

  //veri value
  RecvDataBuffer(@vVerifyValue, SizeOf(vVerifyValue));

  //headlen
  RecvDataBuffer(@vReadLen, SizeOf(vReadLen));
  vDataLen := TByteTools.swap32(vReadLen);

  if vDataLen > MAX_OBJECT_SIZE then  // �ļ�ͷ���󣬴���İ�����
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

    lvStream.Write(lvPACK_FLAG, 2);  // ��ͷ

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