{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit emr_Entry;

interface

const
  PatientCount = 'PatientCount';
  PatientCountToday = 'PatientCountToday';
  ShowPhone = 'ShowPhone';
  BoolTrue = 'True';
  BoolFalse = 'False';
  Key_Value = 'Value';

  // 常量注意大小写有修改后，要处理sqlite库中对应的字段大小写一致
  // 本地参数
  PARAM_LOCAL_MSGHOST = 'MsgHost';    // 消息服务器IP
  PARAM_LOCAL_MSGPORT = 'MsgPort';    // 消息服务器端口
  PARAM_LOCAL_BLLHOST = 'BLLHost';    // 业务服务器IP
  PARAM_LOCAL_BLLPORT = 'BLLPort';    // 业务服务器端口
  PARAM_LOCAL_UPDATEHOST = 'UpdateHost';  // 更新服务器IP
  PARAM_LOCAL_UPDATEPORT = 'UpdatePort';  // 更新服务器端口
  PARAM_LOCAL_DEPTID = 'DeptID';  // 科室ID
  PARAM_LOCAL_VERSIONID = 'VersionID';  // 版本号
  PARAM_LOCAL_PLAYSOUND = 'PlaySound';  // 插入呼叫声音
  // 服务端参数
  PARAM_GLOBAL_HOSPITAL = 'Hospital';  // 医院

  Location = 'Location';  // 定位
  Camera = 'Camera';  // 摄像

type
  TUser = class
  public
    /// <summary> ID </summary>
    const ID = 'UserID';
    /// <summary> 姓名 </summary>
    const NameEx = 'UserName';
    /// <summary> 科室ID </summary>
    const DeptID = 'DeptID';
    /// <summary> 科室名 </summary>
    const DeptName = 'DeptName';
    /// <summary> 密码 </summary>
    const Password = 'PAW';
  end;

  TPatient = class
  public
    /// <summary> 住院号 </summary>
    const InpNo = 'InpNo';
    /// <summary> 姓名 </summary>
    const NameEx = 'name';
    /// <summary> 性别 </summary>
    const Sex = 'Sex';
    /// <summary> 年龄 </summary>
    const Age = 'Age';
    /// <summary> 床号 </summary>
    const BedNo = 'BedNo';
    /// <summary> 科室ID </summary>
    const DeptID = 'DeptID';
    /// <summary> 科室名称 </summary>
    const DeptName = 'DeptName';
    /// <summary> 诊断 </summary>
    const Diagnosis = 'Diagnosis';
    /// <summary> 联系电话 </summary>
    const LinkPhone = 'LinkPhone';
    /// <summary> 护理级别 </summary>
    const CareLevel = 'CareLevel';
    /// <summary> 病情状态 </summary>
    const IllState = 'IllState';
    /// <summary> 输液液体总重量 </summary>
    const TransWeight = 'TransWeight';
    /// <summary> 输液液体剩余重量 </summary>
    const TransRemain = 'TransRemain';
    /// <summary> 生命体征信息 </summary>
    const VitalSigns = 'VitalSigns';
    /// <summary> 滴下一滴 </summary>
    const TransDrip = 'TransDrip';
    /// <summary> 输液滴速 </summary>
    const TransDripSpeed = 'TransDripSpeed';
    /// <summary> 输液开始时间 </summary>
    const TransTimeStart = 'TransTimeStart';
    /// <summary> 输液停止 </summary>
    const TransStop = 'TransStop';
    /// <summary> 输液预计多久结束 </summary>
    const TransTimeOver = 'TransTimeOver';
    /// <summary> 患者呼叫 </summary>
    const Call = 'Call';
    /// <summary> 体温 </summary>
    const Temperature = 'Temperature';
    /// <summary> 脉搏 </summary>
    const Pulse = 'Pulse';
    /// <summary> 心率 </summary>
    const Heartrate = 'Heartrate';
    /// <summary> 呼吸 </summary>
    const Breathe = 'Breathe';
    /// <summary> 数据最后一次更新时间 </summary>
    const LastDateTime = 'LastDataTime';
    /// <summary> 入院日期 </summary>
    const InDate = 'InDate';
    /// <summary> 过敏药物 </summary>
    const AllergicDrug = 'AllergicDrug';
    /// <summary> 检验数量 </summary>
    const LisCount = 'LisCount';
    /// <summary> 检查数量 </summary>
    const PacsCount = 'PacsCount';
    /// <summary> 摘要 </summary>
    const Summarys = 'Summarys';
    /// <summary> 医生ID </summary>
    const OneDrID = 'OneDrID';
    /// <summary> 异常 </summary>
    const Abnormal = 'Abnormal';
    /// <summary> 异常值类型 </summary>
    const AbnormalType = 'AbnormalType';
    /// <summary> 异常数据 </summary>
    const AbnormalData = 'AbnormalData';
  end;

implementation

end.