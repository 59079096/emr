{*******************************************************}
{                                                       }
{         ����HCView�ĵ��Ӳ�������  ���ߣ���ͨ          }
{                                                       }
{ �˴������ѧϰ����ʹ�ã�����������ҵĿ�ģ��ɴ�������  }
{ �����ʹ���߳е�������QQȺ 649023932 ����ȡ����ļ��� }
{ ������                                                }
{                                                       }
{*******************************************************}

unit emr_Common;

interface

uses
  Winapi.Windows, Classes, SysUtils, Vcl.ComCtrls, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
  System.Generics.Collections, FunctionIntf, frm_Hint, System.Rtti, TypInfo, Vcl.Grids;

const
  // ����ע���Сд���޸ĺ�Ҫ����sqlite���ж�Ӧ���ֶδ�Сдһ��
  // ���ز���
  PARAM_LOCAL_MSGHOST = 'MsgHost';    // ��Ϣ������IP
  PARAM_LOCAL_MSGPORT = 'MsgPort';    // ��Ϣ�������˿�
  PARAM_LOCAL_BLLHOST = 'BLLHost';    // ҵ�������IP
  PARAM_LOCAL_BLLPORT = 'BLLPort';    // ҵ��������˿�
  PARAM_LOCAL_UPDATEHOST = 'UpdateHost';  // ���·�����IP
  PARAM_LOCAL_UPDATEPORT = 'UpdatePort';  // ���·������˿�
  //PARAM_LOCAL_DEPTCODE = 'DeptCode';  // ����
  PARAM_LOCAL_VERSIONID = 'VersionID';  // �汾��
  //PARAM_LOCAL_PLAYSOUND = 'PlaySound';  // �����������
  // ����˲���
  //PARAM_GLOBAL_HOSPITAL = 'Hospital';  // ҽԺ

//��������ĸƴ��
const
  py: array[216..247] of AnsiString = (
  {216}'CJWGNSPGCGNESYPB' + 'TYYZDXYKYGTDJNMJ' + 'QMBSGZSCYJSYYZPG' +
  {216}'KBZGYCYWYKGKLJSW' + 'KPJQHYZWDDZLSGMR' + 'YPYWWCCKZNKYDG',
  {217}'TTNJJEYKKZYTCJNM' + 'CYLQLYPYQFQRPZSL' + 'WBTGKJFYXJWZLTBN' +
  {217}'CXJJJJZXDTTSQZYC' + 'DXXHGCKBPHFFSSYY' + 'BGMXLPBYLLLHLX',
  {218}'SPZMYJHSOJNGHDZQ' + 'YKLGJHXGQZHXQGKE' + 'ZZWYSCSCJXYEYXAD' +
  {218}'ZPMDSSMZJZQJYZCD' + 'JEWQJBDZBXGZNZCP' + 'WHKXHQKMWFBPBY',
  {219}'DTJZZKQHYLYGXFPT' + 'YJYYZPSZLFCHMQSH' + 'GMXXSXJJSDCSBBQB' +
  {219}'EFSJYHXWGZKPYLQB' + 'GLDLCCTNMAYDDKSS' + 'NGYCSGXLYZAYBN',
  {220}'PTSDKDYLHGYMYLCX' + 'PYCJNDQJWXQXFYYF' + 'JLEJBZRXCCQWQQSB' +
  {220}'ZKYMGPLBMJRQCFLN' + 'YMYQMSQYRBCJTHZT' + 'QFRXQHXMJJCJLX',
  {221}'QGJMSHZKBSWYEMYL' + 'TXFSYDSGLYCJQXSJ' + 'NQBSCTYHBFTDCYZD' +
  {221}'JWYGHQFRXWCKQKXE' + 'BPTLPXJZSRMEBWHJ' + 'LBJSLYYSMDXLCL',
  {222}'QKXLHXJRZJMFQHXH' + 'WYWSBHTRXXGLHQHF' + 'NMCYKLDYXZPWLGGS' +
  {222}'MTCFPAJJZYLJTYAN' + 'JGBJPLQGDZYQYAXB' + 'KYSECJSZNSLYZH',
  {223}'ZXLZCGHPXZHZNYTD' + 'SBCJKDLZAYFMYDLE' + 'BBGQYZKXGLDNDNYS' +
  {223}'KJSHDLYXBCGHXYPK' + 'DQMMZNGMMCLGWZSZ' + 'XZJFZNMLZZTHCS',
  {224}'YDBDLLSCDDNLKJYK' + 'JSYCJLKOHQASDKNH' + 'CSGANHDAASHTCPLC' +
  {224}'PQYBSDMPJLPCJOQL' + 'CDHJJYSPRCHNKNNL' + 'HLYYQYHWZPTCZG',
  {225}'WWMZFFJQQQQYXACL' + 'BHKDJXDGMMYDJXZL' + 'LSYGXGKJRYWZWYCL' +
  {225}'ZMSSJZLDBYDCPCXY' + 'HLXCHYZJQSQQAGMN' + 'YXPFRKSSBJLYXY',
  {226}'SYGLNSCMHCWWMNZJ' + 'JLXXHCHSYD CTXRY' + 'CYXBYHCSMXJSZNPW' +
  {226}'GPXXTAYBGAJCXLYS' + 'DCCWZOCWKCCSBNHC' + 'PDYZNFCYYTYCKX',
  {227}'KYBSQKKYTQQXFCWC' + 'HCYKELZQBSQYJQCC' + 'LMTHSYWHMKTLKJLY' +
  {227}'CXWHEQQHTQHZPQSQ' + 'SCFYMMDMGBWHWLGS' + 'LLYSDLMLXPTHMJ',
  {228}'HWLJZYHZJXHTXJLH' + 'XRSWLWZJCBXMHZQX' + 'SDZPMGFCSGLSXYMJ' +
  {228}'SHXPJXWMYQKSMYPL' + 'RTHBXFTPMHYXLCHL' + 'HLZYLXGSSSSTCL',
  {229}'SLDCLRPBHZHXYYFH' + 'BBGDMYCNQQWLQHJJ' + 'ZYWJZYEJJDHPBLQX' +
  {229}'TQKWHLCHQXAGTLXL' + 'JXMSLXHTZKZJECXJ' + 'CJNMFBYCSFYWYB',
  {230}'JZGNYSDZSQYRSLJP' + 'CLPWXSDWEJBJCBCN' + 'AYTWGMPABCLYQPCL' +
  {230}'ZXSBNMSGGFNZJJBZ' + 'SFZYNDXHPLQKZCZW' + 'ALSBCCJXJYZHWK',
  {231}'YPSGXFZFCDKHJGXD' + 'LQFSGDSLQWZKXTMH' + 'SBGZMJZRGLYJBPML' +
  {231}'MSXLZJQQHZSJCZYD' + 'JWBMJKLDDPMJEGXY' + 'HYLXHLQYQHKYCW',
  {232}'CJMYYXNATJHYCCXZ' + 'PCQLBZWWYTWBQCML' + 'PMYRJCCCXFPZNZZL' +
  {232}'JPLXXYZTZLGDLDCK' + 'LYRLZGQTGJHHGJLJ' + 'AXFGFJZSLCFDQZ',
  {233}'LCLGJDJCSNCLLJPJ' + 'QDCCLCJXMYZFTSXG' + 'CGSBRZXJQQCTZHGY' +
  {233}'QTJQQLZXJYLYLBCY' + 'AMCSTYLPDJBYREGK' + 'JZYZHLYSZQLZNW',
  {234}'CZCLLWJQJJJKDGJZ' + 'OLBBZPPGLGHTGZXY' + 'GHZMYCNQSYCYHBHG' +
  {234}'XKAMTXYXNBSKYZZG' + 'JZLQJDFCJXDYGJQJ' + 'JPMGWGJJJPKQSB',
  {235}'GBMMCJSSCLPQPDXC' + 'DYYKYWCJDDYYGYWR' + 'HJRTGZNYQLDKLJSZ' +
  {235}'ZGZQZJGDYKSHPZMT' + 'LCPWNJAFYZDJCNMW' + 'ESCYGLBTZCGMSS',
  {236}'LLYXQSXSBSJSBBGG' + 'GHFJLYPMZJNLYYWD' + 'QSHZXTYYWHMCYHYW' +
  {236}'DBXBTLMSYYYFSXJC' + 'SDXXLHJHF SXZQHF' + 'ZMZCZTQCXZXRTT',
  {237}'DJHNNYZQQMNQDMMG' + 'LYDXMJGDHCDYZBFF' + 'ALLZTDLTFXMXQZDN' +
  {237}'GWQDBDCZJDXBZGSQ' + 'QDDJCMBKZFFXMKDM' + 'DSYYSZCMLJDSYN',
  {238}'SPRSKMKMPCKLGDBQ' + 'TFZSWTFGGLYPLLJZ' + 'HGJJGYPZLTCSMCNB' +
  {238}'TJBQFKTHBYZGKPBB' + 'YMTDSSXTBNPDKLEY' + 'CJNYCDYKZDDHQH',
  {239}'SDZSCTARLLTKZLGE' + 'CLLKJLQJAQNBDKKG' + 'HPJTZQKSECSHALQF' +
  {239}'MMGJNLYJBBTMLYZX' + 'DCJPLDLPCQDHZYCB' + 'ZSCZBZMSLJFLKR',
  {240}'ZJSNFRGJHXPDHYJY' + 'BZGDLJCSEZGXLBLH' + 'YXTWMABCHECMWYJY' +
  {240}'ZLLJJYHLGBDJLSLY' + 'GKDZPZXJYYZLWCXS' + 'ZFGWYYDLYHCLJS',
  {241}'CMBJHBLYZLYCBLYD' + 'PDQYSXQZBYTDKYYJ' + 'YYCNRJMPDJGKLCLJ' +
  {241}'BCTBJDDBBLBLCZQR' + 'PPXJCGLZCSHLTOLJ' + 'NMDDDLNGKAQHQH',
  {242}'JHYKHEZNMSHRP QQ' + 'JCHGMFPRXHJGDYCH' + 'GHLYRZQLCYQJNZSQ' +
  {242}'TKQJYMSZSWLCFQQQ' + 'XYFGGYPTQWLMCRNF' + 'KKFSYYLQBMQAMM',
  {243}'MYXCTPSHCPTXXZZS' + 'MPHPSHMCLMLDQFYQ' + 'XSZYJDJJZZHQPDSZ' +
  {243}'GLSTJBCKBXYQZJSG' + 'PSXQZQZRQTBDKYXZ' + 'KHHGFLBCSMDLDG',
  {244}'DZDBLZYYCXNNCSYB' + 'ZBFGLZZXSWMSCCMQ' + 'NJQSBDQSJTXXMBLT' +
  {244}'XZCLZSHZCXRQJGJY' + 'LXZFJPHYXZQQYDFQ' + 'JJLZZNZJCDGZYG',
  {245}'CTXMZYSCTLKPHTXH' + 'TLBJXJLXSCDQXCBB' + 'TJFQZFSLTJBTKQBX' +
  {245}'XJJLJCHCZDBZJDCZ' + 'JDCPRNPQCJPFCZLC' + 'LZXBDMXMPHJSGZ',
  {246}'GSZZQLYLWTJPFSYA' + 'SMCJBTZYYCWMYTCS' + 'JJLQCQLWZMALBXYF' +
  {246}'BPNLSFHTGJWEJJXX' + 'GLLJSTGSHJQLZFKC' + 'GNNDSZFDEQFHBS',
  {247}'AQTGYLBXMMYGSZLD' + 'YDQMJJRGBJTKGDHG' + 'KBLQKBDMBYLXWCXY' +
  {247}'TTYBKMRTJZXQJBHL' + 'MHMJJZMQASLDCYXY' + 'QDLQCAFYWYXQHZ'
  );

type
  TEventMessage = class
    Msg: tagMSG;
    Handled: Boolean;
  end;

  TServerParam = class(TObject)  // ����˲���
  strict private
    FHospital: string;
    FPasteDifferent: Boolean;  // �Ƿ�����ͬ����֮��ճ�����Ƶ�����
    FPasteOutside: Boolean;  // �Ƿ�����������ճ��������������
  public
    constructor Create; virtual;
    property PasteDifferent: Boolean read FPasteDifferent write FPasteDifferent;
    property PasteOutside: Boolean read FPasteOutside write FPasteOutside;
    property Hospital: string read FHospital write FHospital;
  end;

  TClientParam = class(TObject)  // �ͻ��˲���(��Winƽ̨ʹ��)
  private
    FMsgServerIP, FBLLServerIP: string;
    FMsgServerPort, FBLLServerPort: Word;
    FTimeOut, FVersionID: Cardinal;
  public
    /// <summary> ��Ϣ������IP </summary>
    property MsgServerIP: string read FMsgServerIP write FMsgServerIP;

    /// <summary> ҵ�������IP </summary>
    property BLLServerIP: string read FBLLServerIP write FBLLServerIP;

    /// <summary> ��Ϣ�������˿� </summary>
    property MsgServerPort: Word read FMsgServerPort write FMsgServerPort;

    /// <summary> ҵ��������˿� </summary>
    property BLLServerPort: Word read FBLLServerPort write FBLLServerPort;

    /// <summary> ��Ӧ��ʱʱ�� </summary>
    property TimeOut: Cardinal read FTimeOut write FTimeOut;

    /// <summary> ���ؿͻ��˰汾 </summary>
    property VersionID: Cardinal read FVersionID write FVersionID;
  end;

  TDataSetInfo = class(TObject)  // ���ݼ���Ϣ
  public
    const
      // ���ݼ�
      /// <summary> ���ݼ����� </summary>
      CLASS_PAGE = 1;
      /// <summary> ���ݼ�ҳü </summary>
      CLASS_HEADER = 2;
      /// <summary> ���ݼ�ҳ�� </summary>
      CLASS_FOOTER = 3;

      // ʹ�÷�Χ 1�ٴ� 2���� 3�ٴ�������
      /// <summary> ģ��ʹ�÷�Χ �ٴ� </summary>
      USERANG_CLINIC = 1;
      /// <summary> ģ��ʹ�÷�Χ ���� </summary>
      USERANG_NURSE = 2;
      /// <summary> ģ��ʹ�÷�Χ �ٴ������� </summary>
      USERANG_CLINICANDNURSE = 3;

      // סԺor���� 1סԺ 2���� 3סԺ������
      /// <summary> סԺ </summary>
      INOROUT_IN = 1;
      /// <summary> ���� </summary>
      INOROUT_OUT = 2;
      /// <summary> סԺ������ </summary>
      INOROUT_INOUT = 3;
  public
    ID, PID,
    GroupClass,  // ģ����� 1���� 2ҳü 3ҳ��
    GroupType,  // ģ������ 1���ݼ�ģ�� 2������ģ��
    UseRang,  // ʹ�÷�Χ 1�ٴ� 2���� 3�ٴ�������
    InOrOut  // סԺor���� 1סԺ 2���� 3סԺ������
      : Integer;
    GroupCode, GroupName: string;
  end;

  THCThread = class(TThread)
  private
    FOnExecute: TNotifyEvent;
    procedure DoExecute;
  protected
    procedure Execute; override;
  public
    constructor Create;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;

  TClientCache = class(TObject)
  private
    FDataSetElementDT, FDataElementDT: TFDMemTable;
    FServerParam: TServerParam;
    FClientParam: TClientParam;
    FRunPath: string;
    FDataSetInfos: TObjectList<TDataSetInfo>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetServerParam;
    /// <summary> �ڴ�����Ԫ�� </summary>
    procedure GetDataElementTable;
    /// <summary> �ڴ����ݼ���Ϣ </summary>
    procedure GetDataSetTable;
    procedure GetCacheData;
    /// <summary> ����ָ�������ݼ�ID�����ذ���������Ԫ��Ϣ </summary>
    procedure GetDataSetElement(const ADesID: Integer);
    function FindDataElementByIndex(const ADeIndex: string): Boolean;
    function GetDataSetInfo(const ADesID: Integer): TDataSetInfo;
    property DataElementDT: TFDMemTable read FDataElementDT;
    property DataSetElementDT: TFDMemTable read FDataSetElementDT;
    property ServerParam: TServerParam read FServerParam;
    property ClientParam: TClientParam read FClientParam;
    property DataSetInfos: TObjectList<TDataSetInfo> read FDataSetInfos;
    property RunPath: string read FRunPath write FRunPath;
  end;

  TOnErrorEvent = procedure(const AErrCode: Integer; const AParam: string) of object;

  /// <summary> ��֤״̬ ʧ�ܡ�ͨ�����˺Ų�Ψһ��ͻ </summary>
  TCertificateState = (cfsError, cfsPass, cfsConflict);

  TUserCert = class(TObject)
  public
    ID, Password: string;
    State: TCertificateState;  // ��֤״̬
  end;

  /// <summary> ������Ϣ </summary>
  {TConsultation = class(TObject)
  public
    ID: Integer;
    Apl_UserID: Integer;
    Apl_DT: TDateTime;
    PatID: string;
    PatDeptID: Integer;
    Coslt_DT: TDateTime;
    Coslt_Place: string;
    Coslt_Abstract: string;
  end;

  TConsultationNotify = class(TObject)
  public
    Apl_UserID: Integer;
    Apl_UserName: string;
    Apl_PatID: string;
    Apl_PatName: string;
    Apl_PatDeptName: string;
    Invitee_DeptID: Integer;
  end;

  /// <summary> ����������Ϣ </summary>
  TConsultationInvitee = class(TObject)

  end;}

  TCustomUserInfo = class(TObject)
  strict private
    FID: string;  // �û�ID
    FName: string;  // �û���
    FDeptID: string;  // �û���������ID
    FDeptName: string;  // �û�������������
  protected
    procedure Clear; virtual;
    procedure SetUserID(const Value: string); virtual;
  public
    function FieldByName(const AFieldName: string): TValue; virtual;
    property ID: string read FID write SetUserID;
    property &Name: string read FName write FName;
    property DeptID: string read FDeptID write FDeptID;
    property DeptName: string read FDeptName write FDeptName;
  end;

  TUserInfo = class(TCustomUserInfo)  // ��¼�û���Ϣ
  private
    FGroupDeptIDs: string;  // �û����й������Ӧ����
    FFunCDS: TFDMemTable;
    procedure IniUserInfo;  //�����û�������Ϣ
    procedure IniFuns;  // ����ָ���û����н�ɫ��Ӧ�Ĺ���
    procedure IniGroupDepts;  // ����ָ���û����й������Ӧ�Ŀ���
  protected
    procedure SetUserID(const Value: string); override;  // �û����н�ɫ��Ӧ�Ĺ���
    procedure Clear; override;
    /// <summary>
    /// �жϵ�ǰ�û��Ƿ���ĳ����Ȩ�ޣ���������ж�ADeptID�Ƿ��ڵ�ǰ�û�ʹ�øù���Ҫ��Ŀ��ҷ�Χ
    /// ��APerID�Ƿ��ǵ�ǰ�û�
    /// </summary>
    /// <param name="AFunID">����ID</param>
    /// <param name="ADeptID">����ID</param>
    /// <param name="APerID">�û�ID</param>
    /// <returns>True: �д�Ȩ��</returns>
    function FunAuth(const AFunID, ADeptID: Integer; const APerID: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function FieldByName(const AFieldName: string): TValue; override;

    {���ڲ�ͬҽԺά���Ĺ��ܲ�ͬ��������ݿ���ͬһ����ID��Ӧ�Ŀ����ǲ�ͬ�Ĺ��ܣ�
       ���Դ��벻���ù���ID��Ϊ�����ж��Ƿ���Ȩ�ޣ���ʹ�ÿ����õĿؼ�����������}
    /// <summary>
    /// ���ݲ�������ID���������ж�ָ������ǿؼ��๦���Ƿ���Ȩ��(�����ڽ��о���������¼�ʱ�ж��û�����Ȩ��)
    /// </summary>
    /// <param name="AFormAuthControls">����������Ȩ�޹���Ŀؼ�����Ӧ�Ĺ���ID</param>
    /// <param name="AControlName">�ؼ�����</param>
    /// <param name="ADeptID">����</param>
    /// <param name="APerID">������</param>
    /// <returns>True: ��Ȩ�޲���</returns>
    function FormUnControlAuth(const AFormAuthControls: TFDMemTable; const AControlName: string;
      const ADeptID: Integer; const APerID: string): Boolean;

    /// <summary>
    /// ����ָ���Ŀ���ID����������Ϣ����ָ��������Ȩ�޿��ƿؼ���״̬(�����ڻ����л���ʱ�����û�����ѡ�л��ߵ�Ȩ�����ô���ؼ�״̬)
    /// </summary>
    /// <param name="AForm">����</param>
    /// <param name="ADeptID">����ID</param>
    /// <param name="APersonID">������</param>
    procedure SetFormAuthControlState(const AForm: TComponent; const ADeptID: Integer; const APersonID: string);

    /// <summary> ��ȡָ����������Ȩ�޿��ƵĿؼ���Ϣ����ӵ�ǰ�û�Ȩ����Ϣ(�������û���¼���򿪴����) </summary>
    /// <param name="AForm">����</param>
    /// <param name="AAuthControls">����������Ȩ�޿��ƵĿؼ����ؼ���Ӧ�Ĺ���ID</param>
    procedure IniFormControlAuthInfo(const AForm: TComponent; const AAuthControls: TFDMemTable);

    property FunCDS: TFDMemTable read FFunCDS;
    property GroupDeptIDs: string read FGroupDeptIDs;
  end;

  TPatientInfo = class(TObject)
  private
    FPatID, FInpNo, FBedNo, FName, FSex, FAge, FDeptName: string;
    FDeptID: Cardinal;
    FInDateTime, FInDeptDateTime: TDateTime;
    FCareLevel,  // ������
    FVisitID  // סԺ����
      : Byte;
  public
    procedure Assign(const ASource: TPatientInfo);
    function FieldByName(const AFieldName: string): TValue;
    class procedure SetProposal(const AInsertList, AItemList: TStrings);
    //
    property PatID: string read FPatID write FPatID;
    property &Name: string read FName write FName;
    property Sex: string read FSex write FSex;
    property Age: string read FAge write FAge;
    property BedNo: string read FBedNo write FBedNo;
    property InpNo: string read FInpNo write FInpNo;
    property InDateTime: TDateTime read FInDateTime write FInDateTime;
    property InDeptDateTime: TDateTime read FInDeptDateTime write FInDeptDateTime;
    property CareLevel: Byte read FCareLevel write FCareLevel;
    property VisitID: Byte read FVisitID write FVisitID;
    property DeptID: Cardinal read FDeptID write FDeptID;
    property DeptName: string read FDeptName write FDeptName;
  end;

  TRecordDataSetInfo = class(TObject)
  private
    FDesPID: Cardinal;
  public
    property DesPID: Cardinal read FDesPID write FDesPID;
  end;

  TRecordInfo = class(TObject)  // �ܷ�� TTemplateDeSetInfo �ϲ���
  private
    FID,
    FDesID  // ���ݼ�ID
      : Cardinal;
    //FSignature: Boolean;  // �ͷ��Ѿ�ǩ��
    FRecName: string;
    FDT, FLastDT: TDateTime;
  public
    class procedure SetProposal(const AInsertList, AItemList: TStrings);
    property ID: Cardinal read FID write FID;
    property DesID: Cardinal read FDesID write FDesID;
    property RecName: string read FRecName write FRecName;
    property DT: TDateTime read FDT write FDT;
    property LastDT: TDateTime read FLastDT write FLastDT;
  end;

  TServerInfo = class(TObject)
  private
    FDateTime: TDateTime;
  public
    function FieldByName(const AFieldName: string): TValue;
    property DateTime: TDateTime read FDateTime write FDateTime;
  end;

  TTemplateInfo = class(TObject)  // ģ����Ϣ
    ID, Owner, DesID: Integer;
    NameEx, OwnerID: string;
  end;

  TUpdateHint = procedure(const AHint: string) of object;
  THintProcesEvent = reference to procedure(const AUpdateHint: TUpdateHint);

  procedure HintFormShow(const AHint: string; const AHintProces: THintProcesEvent);

  /// <summary> ����ָ���ĸ�ʽ������� </summary>
  /// <param name="AFormatStr"> ��ʽ </param>
  /// <param name="ASize"> ���� </param>
  /// <returns> ��ʽ�������� </returns>
  function FormatSize(const AFormatStr: string; const ASize: Int64): string;

  function TreeNodeIsTemplate(const ANode: TTreeNode): Boolean;
  function TreeNodeIsRecordDataSet(const ANode: TTreeNode): Boolean;
  function TreeNodeIsRecord(const ANode: TTreeNode): Boolean;
  procedure ClearRecordNode(const ATreeView: TTreeView);
  procedure GetNodeRecordInfo(const ANode: TTreeNode; var ADesPID, ADesID, ARecordID: Integer);

  procedure SaveStringGridRow(var ARow, ATopRow: Integer; const AGrid: TStringGrid);
  procedure RestoreStringGridRow(const ARow, ATopRow: Integer; const AGrid: TStringGrid);
  function MD5(const AText: string): string;
  function IsPY(const AChar: Char): Boolean;
  function GetValueAsString(const AValue: TValue): string;
  function CalcTickCount(const AStart, AEnd: Cardinal): Cardinal;
  function ChnToPY(const AValue: AnsiString): AnsiString; // ����ĸƴ��

var
  ClientCache: TClientCache;
  //EmrFormatSettings: TFormatSettings;

implementation

uses
  Variants, emr_MsgPack, emr_Entry, Data.DB, FireDAC.Stan.Intf, FireDAC.Stan.StorageBin,
  IdHashMessageDigest, emr_BLLInvoke;

function ChnPy(const AValue: array of AnsiChar): AnsiChar;
begin
  Result := #0;
  case Byte(AValue[0]) of
    176:
      case Byte(AValue[1]) of
        161..196: Result := 'A';
        197..254: Result := 'B';
      end;

    177:
      Result := 'B';

    178:
      case Byte(AValue[1]) of
        161..192: Result := 'B';
        193..205: Result := 'C';
        206: Result := 'S'; //��
        207..254: Result := 'C';
      end;

    179:
      Result := 'C';

    180:
      case Byte(AValue[1]) of
        161..237: Result := 'C';
        238..254: Result := 'D';
      end;

    181:
      Result := 'D';

    182:
      case Byte(AValue[1]) of
        161..233: Result := 'D';
        234..254: Result := 'E';
      end;

    183:
      case Byte(AValue[1]) of
        161: Result := 'E';
        162..254: Result := 'F';
      end;

    184:
      case Byte(AValue[1]) of
        161..192: Result := 'F';
        193..254: Result := 'G';
      end;

    185:
      case Byte(AValue[1]) of
        161..253: Result := 'G';
        254: Result := 'H';
      end;

    186:
      Result := 'H';

    187:
      case Byte(AValue[1]) of
        161..246: Result := 'H';
        247..254: Result := 'J';
      end;

    188..190:
      Result := 'J';

    191:
      case Byte(AValue[1]) of
        161..165: Result := 'J';
        166..254: Result := 'K';
      end;

    192:
      case Byte(AValue[1]) of
        161..171: Result := 'K';
        172..254: Result := 'L';
      end;

    193:
      Result := 'L';

    194:
      case Byte(AValue[1]) of
        161..231: Result := 'L';
        232..254: Result := 'M';
      end;

    195:
      Result := 'M';

    196:
      case Byte(AValue[1]) of
        161..194: Result := 'M';
        195..254: Result := 'N';
      end;

    197:
      case Byte(AValue[1]) of
        161..181: Result := 'N';
        182..189: Result := 'O';
        190..254: Result := 'P';
      end;

    198:
      case Byte(AValue[1]) of
        161..217: Result := 'P';
        218..254: Result := 'Q';
      end;

    199:
      Result := 'Q';

    200:
      case Byte(AValue[1]) of
        161..186: Result := 'Q';
        187..245: Result := 'R';
        246..254: Result := 'S';
      end;

    201..202:
      Result := 'S';

    203:
      case Byte(AValue[1]) of
        161..249: Result := 'S';
        250..254: Result := 'T';
      end;

    204:
      Result := 'T';

    205:
      case Byte(AValue[1]) of
        161..217: Result := 'T';
        218..254: Result := 'W';
      end;

    206:
      case Byte(AValue[1]) of
        161..243: Result := 'W';
        244..254: Result := 'X';
      end;

    207..208:
      Result := 'X';

    209:
      case Byte(AValue[1]) of
        161..184: Result := 'X';
        185..254: Result := 'Y';
      end;

    210..211:
      Result := 'Y';

    212:
      case Byte(AValue[1]) of
        161..208: Result := 'Y';
        209..254: Result := 'Z';
      end;

    213..215:
      Result := 'Z';

    216..247:
      Result := py[Byte(AValue[0])][Byte(AValue[1]) - 160];
  end;
end;

function ChnToPY(const AValue: AnsiString): AnsiString;
var
  i, vLen: Integer;
  vAnsiChar: array[0..1] of AnsiChar;
  vPY: AnsiChar;
begin
  Result := '';

  vLen := Length(AValue);
  i := 1;
  while i <= vLen - 1 do
  begin
    if AValue[i] < #160 then
    begin
      Result := Result + AValue[i];
      Inc(i);
    end
    else
    begin
      vAnsiChar[0] := AValue[i];
      vAnsiChar[1] := AValue[i + 1];
      vPY := ChnPY(vAnsiChar);
      if vPY <> #0 then
        Result := Result + vPY;

      Inc(i, 2);
    end;
  end;

  if i = vLen then
    Result := Result + AValue[vLen];
end;

function CalcTickCount(const AStart, AEnd: Cardinal): Cardinal;
begin
  if AEnd >= AStart then
    Result := AEnd - AStart
  else
    Result := High(Cardinal) - AStart + AEnd;
end;

function MD5(const AText: string): string;
var
  vMD5: TIdHashMessageDigest5;
begin
  vMD5 := TIdHashMessageDigest5.Create;
  try
    Result := vMD5.HashStringAsHex(AText);
  finally
    vMD5.Free;
  end;
end;

function IsPY(const AChar: Char): Boolean;
begin
  Result := AChar in ['a'..'z', 'A'..'Z'];
end;

function GetValueAsString(const AValue: TValue): string;
begin
  if AValue.TypeInfo.Name = 'TDateTime' then
    Result := FormatDateTime('YYYY-MM-DD HH:mm', AValue.AsType<TDatetime>)
  else
    Result := AValue.AsString;
end;

procedure HintFormShow(const AHint: string; const AHintProces: THintProcesEvent);
var
  vFrmHint: TfrmHint;
begin
  vFrmHint := TfrmHint.Create(nil);
  try
    vFrmHint.Show;
    vFrmHint.UpdateHint(AHint);

    if Assigned(AHintProces) then
      AHintProces(vFrmHint.UpdateHint);
  finally
    FreeAndNil(vFrmHint);
  end;
end;

function TreeNodeIsTemplate(const ANode: TTreeNode): Boolean;
begin
  Result := (ANode <> nil) and (TObject(ANode.Data) is TTemplateInfo);
end;

function TreeNodeIsRecordDataSet(const ANode: TTreeNode): Boolean;
begin
  Result := (ANode <> nil) and (TObject(ANode.Data) is TRecordDataSetInfo);
end;

function TreeNodeIsRecord(const ANode: TTreeNode): Boolean;
begin
  Result := (ANode <> nil) and (TObject(ANode.Data) is TRecordInfo);
end;

procedure ClearRecordNode(const ATreeView: TTreeView);
var
  i: Integer;
  vNode: TTreeNode;
begin
  for i := 0 to ATreeView.Items.Count - 1 do
  begin
    //ClearTemplateGroupNode(tvTemplate.Items[i]);
    vNode := ATreeView.Items[i];
    if vNode <> nil then
    begin
      if TreeNodeIsRecordDataSet(vNode) then
        TRecordDataSetInfo(vNode.Data).Free
      else
        TRecordInfo(vNode.Data).Free;
    end;
  end;

  ATreeView.Items.Clear;
end;

procedure GetNodeRecordInfo(const ANode: TTreeNode; var ADesPID, ADesID, ARecordID: Integer);
var
  vNode: TTreeNode;
begin
  ADesPID := -1;
  ADesID := -1;
  ARecordID := -1;

  if TreeNodeIsRecord(ANode) then  // �����ڵ�
  begin
    ADesID := TRecordInfo(ANode.Data).DesID;
    ARecordID := TRecordInfo(ANode.Data).ID;

    ADesPID := -1;
    vNode := ANode;
    while vNode.Parent <> nil do
    begin
      vNode := vNode.Parent;
      if TreeNodeIsRecordDataSet(vNode) then
      begin
        ADesPID := TRecordDataSetInfo(vNode.Data).DesPID;  // �����������ݼ�����
        Break;
      end;
    end;
  end;
end;

function FormatSize(const AFormatStr: string; const ASize: Int64): string;
begin
  Result := '';
  if ASize < 1024 then  // �ֽ�
    Result := ASize.ToString + 'B'
  else
  if (ASize >= 1024) and (ASize < 1024 * 1024) then  // KB
    Result := FormatFloat(AFormatStr, ASize / 1024) + 'KB'
  else  // MB
    Result := FormatFloat(AFormatStr, ASize / (1024 * 1024)) + 'MB';
end;

procedure SaveStringGridRow(var ARow, ATopRow: Integer; const AGrid: TStringGrid);
begin
  ARow := AGrid.Row;
  ATopRow := AGrid.TopRow;
end;

procedure RestoreStringGridRow(const ARow, ATopRow: Integer; const AGrid: TStringGrid);
begin
  if ATopRow > AGrid.FixedRows - 1 then  // Ϊ0ʱ�ٸ�ֵTopRow���ظ�2�б���
    AGrid.TopRow := ATopRow;

  if ARow > 0 then
  begin
    if ARow > AGrid.RowCount - 1 then
      AGrid.Row := AGrid.RowCount - 1
    else
      AGrid.Row := ARow;
  end;
end;

{ TUserInfo }

procedure TUserInfo.Clear;
begin
  inherited Clear;
  FGroupDeptIDs := '';
  if not FFunCDS.IsEmpty then  // �������
    FFunCDS.EmptyDataSet;
end;

constructor TUserInfo.Create;
begin
  FFunCDS := TFDMemTable.Create(nil);
end;

destructor TUserInfo.Destroy;
begin
  FFunCDS.Free;
  inherited;
end;

function TUserInfo.FieldByName(const AFieldName: string): TValue;
begin
  Result := inherited FieldByName(AFieldName);
end;

function TUserInfo.FormUnControlAuth(const AFormAuthControls: TFDMemTable;
  const AControlName: string; const ADeptID: Integer;
  const APerID: string): Boolean;
begin

end;

function TUserInfo.FunAuth(const AFunID, ADeptID: Integer;
  const APerID: string): Boolean;
begin
  Result := False;
end;

procedure TUserInfo.IniFormControlAuthInfo(const AForm: TComponent;
  const AAuthControls: TFDMemTable);
//var
//  i: Integer;
begin
  // �Ƚ��ؼ���Ȩ�������ͷŷ�ֹ��һ�ε���ϢӰ�챾�ε���
//  for i := 0 to AForm.ComponentCount - 1 do
//  begin
//    if (AForm.Components[i] is TControl) and ((AForm.Components[i] as TControl).TagObject <> nil) then
//      (AForm.Components[i] as TControl).TagObject.Free;
//  end;
//
//  if not AAuthControls.IsEmpty then  // ������е�Ȩ�޿ؼ�����
//    AAuthControls.EmptyDataSet;
//
//  BLLServerExec(
//    procedure(const ABLLServer: TBLLServerProxy)
//    var
//      vExecParam: TMsgPack;
//    begin
//      ABLLServer.Cmd := BLL_GETCONTROLSAUTH;  // ��ȡָ��������������Ȩ�޿��ƵĿؼ�
//      vExecParam := ABLLServer.ExecParam;
//      vExecParam.S['FormName'] := AForm.Name;  // ������
//      ABLLServer.BackDataSet := True;
//    end,
//    procedure(const ABLLServer: TBLLServerProxy)
//
//    var
//      vHasAuth: Boolean;
//      vControl: TControl;
//      vCustomFunInfo: TCustomFunInfo;
//    begin
//      if not ABLLServer.MethodRunOk then
//        raise Exception.Create('�쳣����ȡ������Ȩ�޿��ƿؼ�����');
//
//      if not VarIsEmpty(ABLLServer.BLLDataSet) then  // ����Ȩ�޿��ƵĿؼ�����Ϊ��Ȩ��״̬
//      begin
//        AAuthControls.Data := ABLLServer.BLLDataSet;  // �洢��ǰ����������Ȩ�޹���Ŀؼ����ؼ���Ӧ�Ĺ���ID
//        AAuthControls.First;
//        while not AAuthControls.Eof do
//        begin
//          vHasAuth := False;
//          vControl := GetControlByName(AForm, AAuthControls.FieldByName('ControlName').AsString);
//          if vControl <> nil then  // �ҵ���Ȩ�޿��ƵĿؼ�
//          begin
//            // ���ƿؼ���״̬
//            if not GUserInfo.FunCDS.IsEmpty then  // ��ǰ�û��й���Ȩ������
//            begin
//              if GUserInfo.FunCDS.Locate('FunID', AAuthControls.FieldByName('FunID').AsInteger,
//                [TLocateOption.loCaseInsensitive])
//              then  // ��ǰ�û��д˹��ܵ�Ȩ��
//              begin
//                // ���ݵ�ǰ�û�ʹ�ô˹��ܵ�Ȩ�޷�Χ���ÿؼ���Ȩ������
//                if vControl.TagObject <> nil then  // ����ؼ���Ȩ�����������ͷ�
//                  vControl.TagObject.Free;
//
//                // ����ǰ�û�ʹ�øÿؼ���Ȩ�޷�Χ�󶨵��ؼ���
//                vCustomFunInfo := TCustomFunInfo.Create;
//                vCustomFunInfo.FunID := AAuthControls.FieldByName('FunID').AsInteger;
//                vCustomFunInfo.VisibleType := AAuthControls.FieldByName('VisibleType').AsInteger;
//                vCustomFunInfo.RangeID := GUserInfo.FunCDS.FieldByName('RangeID').AsInteger;
//                vCustomFunInfo.RangeDepts := GUserInfo.FunCDS.FieldByName('RangeDept').AsString;
//                vControl.TagObject := vCustomFunInfo;
//
//                vHasAuth := True;
//              end;
//            end;
//
//            if vHasAuth then  // �й��ܵ�Ȩ��
//            begin
//              vControl.Visible := True;
//              vControl.Enabled := True;
//            end
//            else  // ��ǰ�û��޴˹��ܵ�Ȩ��
//            begin
//              if AAuthControls.FieldByName('VisibleType').AsInteger = 0 then  // ��Ȩ��ʱ����ʾ
//                vControl.Visible := False
//              else
//              if AAuthControls.FieldByName('VisibleType').AsInteger = 1 then  // ��Ȩ��ʱ������
//              begin
//                vControl.Visible := True;
//                vControl.Enabled := False;
//              end;
//            end;
//          end;
//
//          AAuthControls.Next;
//        end;
//      end;
//    end);
end;

procedure TUserInfo.IniFuns;
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETUSERFUNS;  // ��ȡ�û����õ����й���
      ABLLServerReady.ExecParam.S[TUser.ID] := ID;
      ABLLServerReady.BackDataSet := True;
    end,
    procedure(const ABLLServerRun: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServerRun.MethodRunOk then
        raise Exception.Create(ABLLServerRun.MethodError); // Exit;  // ShowMessage(ABLLServer.MethodError);

      if AMemTable <> nil then
      begin
        FFunCDS.Close;
        FFunCDS.Data := AMemTable.Data;
      end;
    end);
end;

procedure TUserInfo.IniGroupDepts;
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETUSERGROUPDEPTS;  // ��ȡָ���û����й������Ӧ�Ŀ���
      ABLLServerReady.ExecParam.S[TUser.ID] := ID;
      ABLLServerReady.BackDataSet := True;
    end,
    procedure(const ABLLServerRun: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServerRun.MethodRunOk then
        raise Exception.Create(ABLLServerRun.MethodError);  //Exit;  // ShowMessage(ABLLServer.MethodError);

      if AMemTable <> nil then
      begin
        AMemTable.First;
        while not AMemTable.Eof do  // ��������
        begin
          if FGroupDeptIDs = '' then
            FGroupDeptIDs := AMemTable.FieldByName(TUser.DeptID).AsString
          else
            FGroupDeptIDs := FGroupDeptIDs + ',' + AMemTable.FieldByName(TUser.DeptID).AsString;

          AMemTable.Next;
        end;
      end;
    end);
end;

procedure TUserInfo.IniUserInfo;
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETUSERINFO;  // ��ȡָ���û�����Ϣ
      ABLLServerReady.ExecParam.S[TUser.ID] := ID;  // �û�ID

      ABLLServerReady.AddBackField(TUser.NameEx);
      ABLLServerReady.AddBackField(TUser.DeptID);
      ABLLServerReady.AddBackField(TUser.DeptName);
    end,

    procedure(const ABLLServerRun: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServerRun.MethodRunOk then
        raise Exception.Create(ABLLServerRun.MethodError);  //Exit;

      Name := ABLLServerRun.BackField(TUser.NameEx).AsString;  // �û�����
      DeptID := ABLLServerRun.BackField(TUser.DeptID).AsString;  // ��������ID
      DeptName := ABLLServerRun.BackField(TUser.DeptName).AsString;  // ����
    end);
end;

procedure TUserInfo.SetFormAuthControlState(const AForm: TComponent;
  const ADeptID: Integer; const APersonID: string);
//var
//  i: Integer;
//  vControl: TControl;
begin
//  for i := 0 to AForm.ComponentCount - 1 do  // ������������пؼ�
//  begin
//    if AForm.Components[i] is TControl then
//    begin
//      vControl := AForm.Components[i] as TControl;
//      if vControl.TagObject <> nil then
//      begin
//        if Self.FunAuth((vControl.TagObject as TCustomFunInfo).FunID, ADeptID, APersonID) then  // ��Ȩ��
//        begin
//          vControl.Visible := True;
//          vControl.Enabled := True;
//        end
//        else  // û��Ȩ��
//        begin
//          if (vControl.TagObject as TCustomFunInfo).VisibleType = 0 then  // ��Ȩ�ޣ����ɼ�
//            vControl.Visible := False
//          else
//          if (vControl.TagObject as TCustomFunInfo).VisibleType = 1 then  // ��Ȩ�ޣ�������
//          begin
//            vControl.Visible := True;
//            vControl.Enabled := False;
//          end;
//        end;
//      end;
//    end;
//  end;
end;

procedure TUserInfo.SetUserID(const Value: string);
begin
  Clear;
  inherited SetUserID(Value);
  if ID <> '' then
  begin
    IniUserInfo;    // ȡ�û�������Ϣ
    IniGroupDepts;  // ȡ�������Ӧ�����п���
    IniFuns;        // ȡ��ɫ��Ӧ�����й��ܼ���Χ
  end;
end;

{ TCustomUserInfo }

procedure TCustomUserInfo.Clear;
begin
  FID := '';
  FName := '';
  FDeptID := '';
  FDeptName := '';
end;

function TCustomUserInfo.FieldByName(const AFieldName: string): TValue;
var
  vRttiContext: TRttiContext;
  vRttiType: TRttiType;
begin
  vRttiType := vRttiContext.GetType(TCustomUserInfo);
  Result := vRttiType.GetProperty(AFieldName).GetValue(Self);
end;

procedure TCustomUserInfo.SetUserID(const Value: string);
begin
  if FID <> Value then
    FID := Value;
end;

{ TPatientInfo }

procedure TPatientInfo.Assign(const ASource: TPatientInfo);
begin
  FInpNo := ASource.InpNo;
  FBedNo := ASource.BedNo;
  FName := ASource.Name;
  FSex := ASource.Sex;
  FAge := ASource.Age;
  FDeptID := ASource.DeptID;
  FDeptName := ASource.DeptName;
  FPatID := ASource.PatID;
  FInDateTime := ASource.InDateTime;
  FInDeptDateTime := ASource.InDeptDateTime;
  FCareLevel := ASource.CareLevel;
  FVisitID := ASource.VisitID;
end;

function TPatientInfo.FieldByName(const AFieldName: string): TValue;
var
  vRttiContext: TRttiContext;
  vRttiType: TRttiType;
  vRttiProperty: TRttiProperty;
begin
  Result := nil;
  vRttiType := vRttiContext.GetType(TPatientInfo);
  vRttiProperty := vRttiType.GetProperty(AFieldName);
  if Assigned(vRttiProperty) then
    Result := vRttiProperty.GetValue(Self);
end;

class procedure TPatientInfo.SetProposal(const AInsertList, AItemList: TStrings);
begin
  AInsertList.Add('PatID');
  AItemList.Add('property \column{}\style{+B}PatID\style{-B}: string;  // ���߱��');
  AInsertList.Add('Name');
  AItemList.Add('property \column{}\style{+B}Name\style{-B}: string;  // ����');
  AInsertList.Add('Sex');
  AItemList.Add('property \column{}\style{+B}Sex\style{-B}: string;  // �Ա�');
  AInsertList.Add('Age');
  AItemList.Add('property \column{}\style{+B}Age\style{-B}: string;  // ����(����λ)');
  AInsertList.Add('BedNo');
  AItemList.Add('property \column{}\style{+B}BedNo\style{-B}: string;  // ����');
  AInsertList.Add('InpNo');
  AItemList.Add('property \column{}\style{+B}InpNo\style{-B}: string;  // סԺ��');
  AInsertList.Add('InDateTime');
  AItemList.Add('property \column{}\style{+B}InDateTime\style{-B}: TDateTime;  // ��Ժʱ��');
  AInsertList.Add('InDeptDateTime');
  AItemList.Add('property \column{}\style{+B}InDeptDateTime\style{-B}: TDateTime;  // ���ʱ��');
  AInsertList.Add('CareLevel');
  AItemList.Add('property \column{}\style{+B}CareLevel\style{-B}: Byte;  // ������');
  AInsertList.Add('VisitID');
  AItemList.Add('property \column{}\style{+B}VisitID\style{-B}: Byte;  // ���');
  AInsertList.Add('DeptID');
  AItemList.Add('property \column{}\style{+B}DeptID\style{-B}: Cardinal;  // ��ǰ����ID');
  AInsertList.Add('DeptName');
  AItemList.Add('property \column{}\style{+B}DeptName\style{-B}: string;  // ��ǰ����');
end;

{ TClientCache }

constructor TClientCache.Create;
begin
//  GetLocaleFormatSettings(GetUserDefaultLCID, EmrFormatSettings);
//  EmrFormatSettings.DateSeparator := '-';
//  EmrFormatSettings.TimeSeparator := ':';
//  EmrFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
//  EmrFormatSettings.ShortTimeFormat := 'hh:mm:ss';

  FRunPath := ExtractFilePath(ParamStr(0));
  FDataSetElementDT := TFDMemTable.Create(nil);
  FDataElementDT := TFDMemTable.Create(nil);
  FServerParam := TServerParam.Create;
  FClientParam := TClientParam.Create;
  FDataSetInfos := TObjectList<TDataSetInfo>.Create;
end;

destructor TClientCache.Destroy;
begin
  FreeAndNil(FDataSetElementDT);
  FreeAndNil(FDataElementDT);
  FreeAndNil(FClientParam);
  FreeAndNil(FDataSetInfos);
  inherited Destroy;
end;

function TClientCache.FindDataElementByIndex(const ADeIndex: string): Boolean;
var
  vIndex: Integer;
begin
  Result := False;
  if TryStrToInt(ADeIndex, vIndex) then
    Result := DataElementDT.Locate('deid', vIndex);
end;

procedure TClientCache.GetCacheData;
begin
  GetDataElementTable;  // ȡ����Ԫ
  GetDataSetTable;  // ȡ���ݼ�
  GetServerParam;  // ȡ�����ȫ�ֲ���
end;

procedure TClientCache.GetDataSetTable;
begin
  FDataSetInfos.Clear;

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDATAELEMENTSETROOT;  // ��ȡ���ݼ�(��Ŀ¼)��Ϣ
      ABLLServerReady.BackDataSet := True;  // ���߷����Ҫ����ѯ���ݼ��������
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    var
      vDataSetInfo: TDataSetInfo;
    begin
      if not ABLLServer.MethodRunOk then  // ����˷�������ִ�в��ɹ�
      begin
        raise Exception.Create(ABLLServer.MethodError);
        Exit;
      end;

      if AMemTable <> nil then
      begin
        with AMemTable do
        begin
          First;
          while not Eof do
          begin
            vDataSetInfo := TDataSetInfo.Create;
            vDataSetInfo.ID := FieldByName('id').AsInteger;
            vDataSetInfo.PID := FieldByName('pid').AsInteger;
            vDataSetInfo.GroupClass := FieldByName('Class').AsInteger;
            vDataSetInfo.GroupType := FieldByName('Type').AsInteger;
            vDataSetInfo.GroupName := FieldByName('Name').AsString;
            FDataSetInfos.Add(vDataSetInfo);

            Next;
          end;
        end;
      end;
    end);
end;

procedure TClientCache.GetServerParam;
begin
  TBLLInvoke.GetParamAll(procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    var
      vParam: string;
    begin
      if not ABLLServer.MethodRunOk then  // ����˷�������ִ�в��ɹ�
      begin
        raise Exception.Create(ABLLServer.MethodError);
        Exit;
      end;

      if AMemTable <> nil then
      begin
        AMemTable.First;
        while not AMemTable.Eof do
        begin
          vParam := AMemTable.FieldByName('NAME').AsString;
          if vParam = 'Hospital' then
            FServerParam.Hospital := AMemTable.FieldByName('Value').AsString
          else
          if vParam = 'PasteDifferent' then
            FServerParam.PasteDifferent := AMemTable.FieldByName('Value').AsInteger = 1
          else
          if vParam = 'PasteOutside' then
            FServerParam.PasteOutside := AMemTable.FieldByName('Value').AsInteger = 1;

          AMemTable.Next;
        end;
      end;
    end);
end;

procedure TClientCache.GetDataSetElement(const ADesID: Integer);
var
  vBLLSrvProxy: TBLLServerProxy;
  vExecParam: TMsgPack;
  vMemStream: TMemoryStream;
begin
  if not DataSetElementDT.IsEmpty then
    DataSetElementDT.EmptyDataSet;

  TBLLInvoke.GetDataSetElement(ADesID,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      vMemStream := TMemoryStream.Create;
      try
        ABLLServer.GetBLLDataSet(vMemStream);
        vMemStream.Position := 0;
        DataSetElementDT.LoadFromStream(vMemStream, TFDStorageFormat.sfBinary);
      finally
        FreeAndNil(vMemStream);
      end;
    end);
end;

procedure TClientCache.GetDataElementTable;
var
  vMemStream: TMemoryStream;
begin
  if not FDataElementDT.IsEmpty then
    FDataElementDT.EmptyDataSet;

  FDataElementDT.Filtered := False;

  TBLLInvoke.GetDataElementTable(
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      vMemStream := TMemoryStream.Create;
      try
        ABLLServer.GetBLLDataSet(vMemStream);
        vMemStream.Position := 0;
        FDataElementDT.LoadFromStream(vMemStream, TFDStorageFormat.sfBinary);
      finally
        FreeAndNil(vMemStream);
      end;
    end);
end;

function TClientCache.GetDataSetInfo(const ADesID: Integer): TDataSetInfo;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FDataSetInfos.Count - 1 do
  begin
    if FDataSetInfos[i].ID = ADesID then
    begin
      Result := FDataSetInfos[i];
      Break;
    end;
  end;
end;

{ THCThread }

constructor THCThread.Create;
begin
  inherited Create(True);
  Self.FreeOnTerminate := False;
end;

procedure THCThread.DoExecute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

procedure THCThread.Execute;
begin
  while not Terminated do
  begin
    DoExecute;
    Sleep(1);
  end;
end;

{ TServerInfo }

function TServerInfo.FieldByName(const AFieldName: string): TValue;
var
  vRttiContext: TRttiContext;
  vRttiType: TRttiType;
begin
  vRttiType := vRttiContext.GetType(TServerInfo);
  Result := vRttiType.GetProperty(AFieldName).GetValue(Self);
end;

{ TRecordInfo }

class procedure TRecordInfo.SetProposal(const AInsertList, AItemList: TStrings);
begin
  AInsertList.Add('ID');
  AItemList.Add('property \column{}\style{+B}ID\style{-B}: Cardinal;  // ����ID');
  AInsertList.Add('DesID');
  AItemList.Add('property \column{}\style{+B}DesID\style{-B}: Cardinal;  // �������ݼ�ID');
  AInsertList.Add('RecName');
  AItemList.Add('property \column{}\style{+B}RecName\style{-B}: string;  // ��������');
  AInsertList.Add('DT');
  AItemList.Add('property \column{}\style{+B}DT\style{-B}: TDateTime;  // ��������ʱ��');
  AInsertList.Add('LastDT');
  AItemList.Add('property \column{}\style{+B}LastDT\style{-B}: TDateTime;  // ������󴴽�ʱ��');
end;

{ TServerParam }

constructor TServerParam.Create;
begin
  FHospital := '';
  FPasteOutside := True;
  FPasteDifferent := True;
end;

end.
