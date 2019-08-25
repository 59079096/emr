{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit emr_Common;

interface

uses
  Winapi.Windows, Classes, SysUtils, Vcl.ComCtrls, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
  System.Generics.Collections, FunctionIntf, frm_Hint, System.Rtti, TypInfo, Vcl.Grids;

const
  // 常量注意大小写有修改后，要处理sqlite库中对应的字段大小写一致
  // 本地参数
  PARAM_LOCAL_MSGHOST = 'MsgHost';    // 消息服务器IP
  PARAM_LOCAL_MSGPORT = 'MsgPort';    // 消息服务器端口
  PARAM_LOCAL_BLLHOST = 'BLLHost';    // 业务服务器IP
  PARAM_LOCAL_BLLPORT = 'BLLPort';    // 业务服务器端口
  PARAM_LOCAL_UPDATEHOST = 'UpdateHost';  // 更新服务器IP
  PARAM_LOCAL_UPDATEPORT = 'UpdatePort';  // 更新服务器端口
  //PARAM_LOCAL_DEPTCODE = 'DeptCode';  // 科室
  PARAM_LOCAL_VERSIONID = 'VersionID';  // 版本号
  //PARAM_LOCAL_PLAYSOUND = 'PlaySound';  // 插入呼叫声音
  // 服务端参数
  //PARAM_GLOBAL_HOSPITAL = 'Hospital';  // 医院

const
  EMRSTYLE_TOOTH = -1001;  // 牙齿公式 THCStyle.Custom - 1
  EMRSTYLE_FANGJIAO = -1002;  // 房角公式 THCStyle.Custom - 2
  EMRSTYLE_YUEJING = -1003;  // 月经公式

type
  TEventMessage = class
    Msg: tagMSG;
    Handled: Boolean;
  end;

  TClientParam = class(TObject)  // 客户端参数(仅Win平台使用)
  private
    FMsgServerIP, FBLLServerIP: string;
    FMsgServerPort, FBLLServerPort: Word;
    FTimeOut, FVersionID: Cardinal;
  public
    /// <summary> 消息服务器IP </summary>
    property MsgServerIP: string read FMsgServerIP write FMsgServerIP;

    /// <summary> 业务服务器IP </summary>
    property BLLServerIP: string read FBLLServerIP write FBLLServerIP;

    /// <summary> 消息服务器端口 </summary>
    property MsgServerPort: Word read FMsgServerPort write FMsgServerPort;

    /// <summary> 业务服务器端口 </summary>
    property BLLServerPort: Word read FBLLServerPort write FBLLServerPort;

    /// <summary> 响应超时时间 </summary>
    property TimeOut: Cardinal read FTimeOut write FTimeOut;

    /// <summary> 本地客户端版本 </summary>
    property VersionID: Cardinal read FVersionID write FVersionID;
  end;

  TDataSetInfo = class(TObject)  // 数据集信息
  public
    const
      // 数据集
      /// <summary> 数据集正文 </summary>
      CLASS_PAGE = 1;
      /// <summary> 数据集页眉 </summary>
      CLASS_HEADER = 2;
      /// <summary> 数据集页脚 </summary>
      CLASS_FOOTER = 3;

      // 使用范围 1临床 2护理 3临床及护理
      /// <summary> 模板使用范围 临床 </summary>
      USERANG_CLINIC = 1;
      /// <summary> 模板使用范围 护理 </summary>
      USERANG_NURSE = 2;
      /// <summary> 模板使用范围 临床及护理 </summary>
      USERANG_CLINICANDNURSE = 3;

      // 住院or门诊 1住院 2门诊 3住院及门诊
      /// <summary> 住院 </summary>
      INOROUT_IN = 1;
      /// <summary> 门诊 </summary>
      INOROUT_OUT = 2;
      /// <summary> 住院及门诊 </summary>
      INOROUT_INOUT = 3;
  public
    ID, PID,
    GroupClass,  // 模板类别 1正文 2页眉 3页脚
    GroupType,  // 模板类型 1数据集模板 2数据组模板
    UseRang,  // 使用范围 1临床 2护理 3临床及护理
    InOrOut  // 住院or门诊 1住院 2门诊 3住院及门诊
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
    FClientParam: TClientParam;
    FRunPath: string;
    FDataSetInfos: TObjectList<TDataSetInfo>;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary> 内存数据元表 </summary>
    procedure GetDataElementTable;
    /// <summary> 内存数据集信息 </summary>
    procedure GetDataSetTable;
    procedure GetCacheData;
    /// <summary> 根据指定的数据集ID，返回包含的数据元信息 </summary>
    procedure GetDataSetElement(const ADesID: Integer);
    function FindDataElementByIndex(const ADeIndex: string): Boolean;
    function GetDataSetInfo(const ADesID: Integer): TDataSetInfo;
    property DataElementDT: TFDMemTable read FDataElementDT;
    property DataSetElementDT: TFDMemTable read FDataSetElementDT;
    property ClientParam: TClientParam read FClientParam;
    property DataSetInfos: TObjectList<TDataSetInfo> read FDataSetInfos;
    property RunPath: string read FRunPath write FRunPath;
  end;

  TOnErrorEvent = procedure(const AErrCode: Integer; const AParam: string) of object;

  /// <summary> 认证状态 失败、通过、账号不唯一冲突 </summary>
  TCertificateState = (cfsError, cfsPass, cfsConflict);

  TUserCert = class(TObject)
  public
    ID, Password: string;
    State: TCertificateState;  // 认证状态
  end;

  /// <summary> 会诊信息 </summary>
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

  /// <summary> 会诊受邀信息 </summary>
  TConsultationInvitee = class(TObject)

  end;}

  TCustomUserInfo = class(TObject)
  strict private
    FID: string;  // 用户ID
    FName: string;  // 用户名
    FDeptID: string;  // 用户所属科室ID
    FDeptName: string;  // 用户所属科室名称
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

  TUserInfo = class(TCustomUserInfo)  // 记录用户信息
  private
    FGroupDeptIDs: string;  // 用户所有工作组对应科室
    FFunCDS: TFDMemTable;
    procedure IniUserInfo;  //设置用户基本信息
    procedure IniFuns;  // 设置指定用户所有角色对应的功能
    procedure IniGroupDepts;  // 设置指定用户所有工作组对应的科室
  protected
    procedure SetUserID(const Value: string); override;  // 用户所有角色对应的功能
    procedure Clear; override;
    /// <summary>
    /// 判断当前用户是否有某功能权限，如果有则判断ADeptID是否在当前用户使用该功能要求的科室范围
    /// 或APerID是否是当前用户
    /// </summary>
    /// <param name="AFunID">功能ID</param>
    /// <param name="ADeptID">科室ID</param>
    /// <param name="APerID">用户ID</param>
    /// <returns>True: 有此权限</returns>
    function FunAuth(const AFunID, ADeptID: Integer; const APerID: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function FieldByName(const AFieldName: string): TValue; override;

    {由于不同医院维护的功能不同，因此数据库中同一功能ID对应的可能是不同的功能，
       所以代码不能用功能ID作为参数判断是否有权限，而使用可配置的控件名称来处理。}
    /// <summary>
    /// 根据操作科室ID、操作人判断指定窗体非控件类功能是否有权限(适用于进行具体操作的事件时判断用户有无权限)
    /// </summary>
    /// <param name="AFormAuthControls">窗体所有受权限管理的控件及对应的功能ID</param>
    /// <param name="AControlName">控件名称</param>
    /// <param name="ADeptID">科室</param>
    /// <param name="APerID">操作人</param>
    /// <returns>True: 有权限操作</returns>
    function FormUnControlAuth(const AFormAuthControls: TFDMemTable; const AControlName: string;
      const ADeptID: Integer; const APerID: string): Boolean;

    /// <summary>
    /// 根据指定的科室ID、操作人信息设置指定窗体受权限控制控件的状态(适用于患者切换后及时根据用户对新选中患者的权限设置窗体控件状态)
    /// </summary>
    /// <param name="AForm">窗体</param>
    /// <param name="ADeptID">科室ID</param>
    /// <param name="APersonID">操作人</param>
    procedure SetFormAuthControlState(const AForm: TComponent; const ADeptID: Integer; const APersonID: string);

    /// <summary> 获取指定窗体上受权限控制的控件信息并添加当前用户权限信息(适用于用户登录后或打开窗体后) </summary>
    /// <param name="AForm">窗体</param>
    /// <param name="AAuthControls">窗体所有受权限控制的控件及控件对应的功能ID</param>
    procedure IniFormControlAuthInfo(const AForm: TComponent; const AAuthControls: TFDMemTable);

    property FunCDS: TFDMemTable read FFunCDS;
    property GroupDeptIDs: string read FGroupDeptIDs;
  end;

  TPatientInfo = class(TObject)
  private
    FPatID, FInpNo, FBedNo, FName, FSex, FAge, FDeptName: string;
    FDeptID: Cardinal;
    FInDateTime, FInDeptDateTime: TDateTime;
    FCareLevel,  // 护理级别
    FVisitID  // 住院次数
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

  TRecordInfo = class(TObject)  // 能否和 TTemplateDeSetInfo 合并？
  private
    FID,
    FDesID  // 数据集ID
      : Cardinal;
    //FSignature: Boolean;  // 就否已经签名
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

  TTemplateInfo = class(TObject)  // 模板信息
    ID, Owner, OwnerID, DesID: Integer;
    NameEx: string;
  end;

  TUpdateHint = procedure(const AHint: string) of object;
  THintProcesEvent = reference to procedure(const AUpdateHint: TUpdateHint);

  procedure HintFormShow(const AHint: string; const AHintProces: THintProcesEvent);

  /// <summary> 按照指定的格式输出数据 </summary>
  /// <param name="AFormatStr">格式</param>
  /// <param name="ASize">数据</param>
  /// <returns>格式化的数据</returns>
  function FormatSize(const AFormatStr: string; const ASize: Int64): string;

  function TreeNodeIsTemplate(const ANode: TTreeNode): Boolean;
  function TreeNodeIsRecordDataSet(const ANode: TTreeNode): Boolean;
  function TreeNodeIsRecord(const ANode: TTreeNode): Boolean;
  procedure ClearRecordNode(const ATreeView: TTreeView);
  procedure GetNodeRecordInfo(const ANode: TTreeNode; var ADesPID, ADesID, ARecordID: Integer);

  procedure SaveStringGridRow(var ARow, ATopRow: Integer; const AGrid: TStringGrid);
  procedure RestoreStringGridRow(const ARow, ATopRow: Integer; const AGrid: TStringGrid);
  procedure DeleteGridRow(const AGrid: TStringGrid; const ARow: Integer = -1);
  function MD5(const AText: string): string;
  function IsPY(const AChar: Char): Boolean;
  function GetValueAsString(const AValue: TValue): string;
  function CalcTickCount(const AStart, AEnd: Cardinal): Cardinal;

var
  ClientCache: TClientCache;
  //EmrFormatSettings: TFormatSettings;

implementation

uses
  Variants, emr_MsgPack, emr_Entry, Data.DB, FireDAC.Stan.Intf, FireDAC.Stan.StorageBin,
  IdHashMessageDigest, emr_BLLInvoke;

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

  if TreeNodeIsRecord(ANode) then  // 病历节点
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
        ADesPID := TRecordDataSetInfo(vNode.Data).DesPID;  // 病历所属数据集大类
        Break;
      end;
    end;
  end;
end;

function FormatSize(const AFormatStr: string; const ASize: Int64): string;
begin
  Result := '';
  if ASize < 1024 then  // 字节
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
  if ATopRow > AGrid.FixedRows - 1 then  // 为0时再赋值TopRow会重复2行标题
    AGrid.TopRow := ATopRow;

  if ARow > 0 then
  begin
    if ARow > AGrid.RowCount - 1 then
      AGrid.Row := AGrid.RowCount - 1
    else
      AGrid.Row := ARow;
  end;
end;

procedure DeleteGridRow(const AGrid: TStringGrid; const ARow: Integer = -1);
var
  i, j, vRow: Integer;
begin
  if ARow < 0 then
    vRow := AGrid.Row
  else
    vRow := ARow;

  if vRow > AGrid.FixedRows - 1 then
  begin
    for i := vRow to AGrid.RowCount - 2 do
    begin
      for j := 0 to AGrid.ColCount - 1 do
        AGrid.Cells[j, i] := AGrid.Cells[j, i + 1];
    end;

    AGrid.RowCount := AGrid.RowCount - 1;
  end;
end;

{ TUserInfo }

procedure TUserInfo.Clear;
begin
  inherited Clear;
  FGroupDeptIDs := '';
  if not FFunCDS.IsEmpty then  // 清除功能
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
  // 先将控件的权限属性释放防止上一次的信息影响本次调用
//  for i := 0 to AForm.ComponentCount - 1 do
//  begin
//    if (AForm.Components[i] is TControl) and ((AForm.Components[i] as TControl).TagObject <> nil) then
//      (AForm.Components[i] as TControl).TagObject.Free;
//  end;
//
//  if not AAuthControls.IsEmpty then  // 清空已有的权限控件数据
//    AAuthControls.EmptyDataSet;
//
//  BLLServerExec(
//    procedure(const ABLLServer: TBLLServerProxy)
//    var
//      vExecParam: TMsgPack;
//    begin
//      ABLLServer.Cmd := BLL_GETCONTROLSAUTH;  // 获取指定窗体上所有受权限控制的控件
//      vExecParam := ABLLServer.ExecParam;
//      vExecParam.S['FormName'] := AForm.Name;  // 窗体名
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
//        raise Exception.Create('异常：获取窗体受权限控制控件错误！');
//
//      if not VarIsEmpty(ABLLServer.BLLDataSet) then  // 有受权限控制的控件处理为无权限状态
//      begin
//        AAuthControls.Data := ABLLServer.BLLDataSet;  // 存储当前窗体所有受权限管理的控件及控件对应的功能ID
//        AAuthControls.First;
//        while not AAuthControls.Eof do
//        begin
//          vHasAuth := False;
//          vControl := GetControlByName(AForm, AAuthControls.FieldByName('ControlName').AsString);
//          if vControl <> nil then  // 找到受权限控制的控件
//          begin
//            // 控制控件的状态
//            if not GUserInfo.FunCDS.IsEmpty then  // 当前用户有功能权限数据
//            begin
//              if GUserInfo.FunCDS.Locate('FunID', AAuthControls.FieldByName('FunID').AsInteger,
//                [TLocateOption.loCaseInsensitive])
//              then  // 当前用户有此功能的权限
//              begin
//                // 根据当前用户使用此功能的权限范围设置控件的权限属性
//                if vControl.TagObject <> nil then  // 如果控件有权限属性则先释放
//                  vControl.TagObject.Free;
//
//                // 将当前用户使用该控件的权限范围绑定到控件上
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
//            if vHasAuth then  // 有功能的权限
//            begin
//              vControl.Visible := True;
//              vControl.Enabled := True;
//            end
//            else  // 当前用户无此功能的权限
//            begin
//              if AAuthControls.FieldByName('VisibleType').AsInteger = 0 then  // 无权限时不显示
//                vControl.Visible := False
//              else
//              if AAuthControls.FieldByName('VisibleType').AsInteger = 1 then  // 无权限时不可用
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
      ABLLServerReady.Cmd := BLL_GETUSERFUNS;  // 获取用户配置的所有功能
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
      ABLLServerReady.Cmd := BLL_GETUSERGROUPDEPTS;  // 获取指定用户所有工作组对应的科室
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
        while not AMemTable.Eof do  // 遍历科室
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
      ABLLServerReady.Cmd := BLL_GETUSERINFO;  // 获取指定用户的信息
      ABLLServerReady.ExecParam.S[TUser.ID] := ID;  // 用户ID

      ABLLServerReady.AddBackField(TUser.NameEx);
      ABLLServerReady.AddBackField(TUser.DeptID);
      ABLLServerReady.AddBackField(TUser.DeptName);
    end,

    procedure(const ABLLServerRun: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServerRun.MethodRunOk then
        raise Exception.Create(ABLLServerRun.MethodError);  //Exit;

      Name := ABLLServerRun.BackField(TUser.NameEx).AsString;  // 用户姓名
      DeptID := ABLLServerRun.BackField(TUser.DeptID).AsString;  // 所属科室ID
      DeptName := ABLLServerRun.BackField(TUser.DeptName).AsString;  // 科室
    end);
end;

procedure TUserInfo.SetFormAuthControlState(const AForm: TComponent;
  const ADeptID: Integer; const APersonID: string);
//var
//  i: Integer;
//  vControl: TControl;
begin
//  for i := 0 to AForm.ComponentCount - 1 do  // 遍历窗体的所有控件
//  begin
//    if AForm.Components[i] is TControl then
//    begin
//      vControl := AForm.Components[i] as TControl;
//      if vControl.TagObject <> nil then
//      begin
//        if Self.FunAuth((vControl.TagObject as TCustomFunInfo).FunID, ADeptID, APersonID) then  // 有权限
//        begin
//          vControl.Visible := True;
//          vControl.Enabled := True;
//        end
//        else  // 没有权限
//        begin
//          if (vControl.TagObject as TCustomFunInfo).VisibleType = 0 then  // 无权限，不可见
//            vControl.Visible := False
//          else
//          if (vControl.TagObject as TCustomFunInfo).VisibleType = 1 then  // 无权限，不可用
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
    IniUserInfo;    // 取用户基本信息
    IniGroupDepts;  // 取工作组对应的所有科室
    IniFuns;        // 取角色对应的所有功能及范围
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
begin
  vRttiType := vRttiContext.GetType(TPatientInfo);
  Result := vRttiType.GetProperty(AFieldName).GetValue(Self);
end;

class procedure TPatientInfo.SetProposal(const AInsertList, AItemList: TStrings);
begin
  AInsertList.Add('PatID');
  AItemList.Add('property \column{}\style{+B}PatID\style{-B}: string;  // 患者编号');
  AInsertList.Add('Name');
  AItemList.Add('property \column{}\style{+B}Name\style{-B}: string;  // 姓名');
  AInsertList.Add('Sex');
  AItemList.Add('property \column{}\style{+B}Sex\style{-B}: string;  // 性别');
  AInsertList.Add('Age');
  AItemList.Add('property \column{}\style{+B}Age\style{-B}: string;  // 年龄(含单位)');
  AInsertList.Add('BedNo');
  AItemList.Add('property \column{}\style{+B}BedNo\style{-B}: string;  // 床号');
  AInsertList.Add('InpNo');
  AItemList.Add('property \column{}\style{+B}InpNo\style{-B}: string;  // 住院号');
  AInsertList.Add('InDateTime');
  AItemList.Add('property \column{}\style{+B}InDateTime\style{-B}: TDateTime;  // 入院时间');
  AInsertList.Add('InDeptDateTime');
  AItemList.Add('property \column{}\style{+B}InDeptDateTime\style{-B}: TDateTime;  // 入科时间');
  AInsertList.Add('CareLevel');
  AItemList.Add('property \column{}\style{+B}CareLevel\style{-B}: Byte;  // 护理级别');
  AInsertList.Add('VisitID');
  AItemList.Add('property \column{}\style{+B}VisitID\style{-B}: Byte;  // 诊次');
  AInsertList.Add('DeptID');
  AItemList.Add('property \column{}\style{+B}DeptID\style{-B}: Cardinal;  // 当前科室ID');
  AInsertList.Add('DeptName');
  AItemList.Add('property \column{}\style{+B}DeptName\style{-B}: string;  // 当前科室');
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
  GetDataElementTable;
  GetDataSetTable;
end;

procedure TClientCache.GetDataSetTable;
begin
  FDataSetInfos.Clear;

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDATAELEMENTSETROOT;  // 获取数据集(根目录)信息
      ABLLServerReady.BackDataSet := True;  // 告诉服务端要将查询数据集结果返回
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    var
      vDataSetInfo: TDataSetInfo;
    begin
      if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
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
  AItemList.Add('property \column{}\style{+B}ID\style{-B}: Cardinal;  // 病历ID');
  AInsertList.Add('DesID');
  AItemList.Add('property \column{}\style{+B}DesID\style{-B}: Cardinal;  // 病历数据集ID');
  AInsertList.Add('RecName');
  AItemList.Add('property \column{}\style{+B}RecName\style{-B}: string;  // 病历名称');
  AInsertList.Add('DT');
  AItemList.Add('property \column{}\style{+B}DT\style{-B}: TDateTime;  // 病历创建时间');
  AInsertList.Add('LastDT');
  AItemList.Add('property \column{}\style{+B}LastDT\style{-B}: TDateTime;  // 病历最后创建时间');
end;

end.
