{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit emr_BLLConst;

interface

uses
  SysUtils;

const
  BLLVERSION = 1;  // 业务版本

  BLL_CMD = 'b.cmd';
  //BACKDATA = 'p.data';  // 用于返回调用方法返回的数据集
  BLL_PROXYTYPE = 'b.type';  // 服务代理类型
  BLL_VER = 'b.ver';  // 业务版本
  BLL_METHODRESULT = 'b.ret';  // 服务端方法返回给客户端的方法执行是否成功
  BLL_RECORDCOUNT = 'b.rcount';  // 存放服务端方法执行时数据集的个数
  BLL_METHODMSG = 'b.msg';  // 服务端服方法执行时回传给给客户端的消息(如失败原因等)
  BLL_EXECPARAM = 'b.exp';  // 存放客户端调用业务时传递的：Sql字段参数数据
  BLL_REPLACEPARAM = 'b.rep';  // 存放客户端调用业务时传递的：Sql替换参数数据

  BLL_BATCH = 'b.bat';  // 存放客户端调用业务时是否是批量操作
  BLL_BATCHDATA = 'b.batdata';  // 存放客户端调用业务时批量传递的数据集

  BLL_BACKDATASET = 'b.bkds';  // 存放客户端调用业务时传递的：通知服务端需要返回数据集(优先级高于BLLBACKPARAM)
  BLL_DATASET = 'b.ds';   // 存放客户端调用业务时传递的：服务端返回的数据集
  BLL_BACKFIELD = 'b.field';  // 存放客户端调用业务时传递的：通知需要返回的数据字段(优先级低于BLLBACKDATA)
  BLL_DEVICE = 'b.dc';      // 连接服务端的设备类型
  BLL_DEVICEINFO = 'b.dcf'; // 连接服务端的设备信息
  BLL_ERROR = 'b.err';  // 错误信息，供调用业务失败时方便调用处提示

  /// <summary> 获取服务器当前时间 </summary>
  BLL_SRVDT = 1;

  /// <summary> 执行Sql语句 </summary>
  BLL_EXECSQL = 2;

  /// <summary> 获取所有表和表说明 </summary>
  BLL_GETAllTABLE = 3;

  BLL_BASE = 1000;  // 业务常量起始值

  { 业务常量(从1000开始) }
  /// <summary> 全部用户 </summary>
  BLL_COMM_ALLUSER = BLL_BASE;

  /// <summary> 核对登录信息 </summary>
  BLL_LOGIN = BLL_BASE + 1;

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

//  function GetBLLMethodName(const AMethodID: Integer): string;

implementation

//function GetBLLMethodName(const AMethodID: Integer): string;
//begin
//  case AMethodID of
//    BLL_SRVDT: Result := 'BLL_SRVDT(1 获取数据库时间)';
//    BLL_EXECSQL: Result := 'BLL_EXECSQL(2 执行Sql语句)';
//    BLL_GETAllTABLE: Result := 'BLL_GETAllTABLE(3 获取所有表和表说明)';
//    BLL_LOGIN: Result := 'BLL_LOGIN(1001 核对登录信息)';
//    BLL_GETUSERINFO: Result := 'BLL_GETUSERINFO(1002 获取指定用户的信息)';
//    BLL_GETUSERGROUPS: Result := 'BLL_GETUSERGROUPS(1003 获取用户的工作组)';
//    BLL_GETUSERROLES: Result := 'BLL_GETUSERROLES(1004 获取用户的角色)';
//    BLL_GETUSERFUNS: Result := 'BLL_GETUSERFUNS(1005 获取指定用户配置的所有功能)';
//    BLL_GETUSERGROUPDEPTS: Result := 'BLL_GETUSERGROUPDEPTS(1006 获取指定用户所有工作组对应的科室)';
//    BLL_COMM_GETPARAM: Result := 'BLL_COMM_GETPARAM(1007 获取参数)';
//    BLL_GETCLIENTCACHE: Result := 'BLL_GETCLIENTCACHE(1008 获取服务端缓存表数据)';
//    BLL_GETCONTROLSAUTH: Result := 'BLL_GETCONTROLSAUTH(1009 获取指定窗体上所有受权限控制的控件)';
//    BLL_GETLASTVERSION: Result := 'BLL_GETLASTVERSION(1010 获取要升级的最新版本号)';
//    BLL_GETUPDATEINFO: Result := 'BLL_GETUPDATEINFO(1011 获取要升级的文件)';
//    BLL_UPLOADUPDATEINFO: Result := 'BLL_UPLOADUPDATEINFO(1012 上传升级信息)';
//    BLL_HIS_GETINPATIENT: Result := 'BLL_HIS_GETPATIENT(1013 获取在院患者)';
//    BLL_GETDATAELEMENTSET: Result := 'BLL_GETDATAELEMENTSET(1014 获取数据集目录)';
//    BLL_GETTEMPLATELIST: Result := 'BLL_GETTEMPLATELIST(1015 获取模板分组包含的子分组和模板)';
//    BLL_NEWTEMPLATE: Result := 'BLL_NEWTEMPLATE(1016 新建模板)';
//    BLL_GETTEMPLATECONTENT: Result := 'BLL_GETTEMPLATECONTENT(1017 获取模板内容)';
//    BLL_SAVETEMPLATECONTENT: Result := 'BLL_SAVETEMPLATECONTENT(1018 保存模板内容)';
//    BLL_DELETETEMPLATE: Result := 'BLL_DELETETEMPLATE(1019 删除模板及内容)';
//    BLL_GETDATAELEMENT: Result := 'BLL_GETDATAELEMENT(1020 获取数据元列表)';
//    BLL_GETDATAELEMENTDOMAIN: Result := 'BLL_GETDATAELEMENTDOMAIN(1021 获取数据元值域选项)';
//    BLL_SAVEDOMAINCONTENT: Result := 'BLL_SAVEDOMAINCONTENT(1022 保存数据元选项值域对应的内容)';
//    BLL_GETDOMAINCONTENT: Result := 'BLL_GETDOMAINCONTENT(1023 获取数据元选项值域对应的内容)';
//    BLL_GETINCHRECORDLIST: Result := 'BLL_GETINCHRECORDLIST(1024 获取指定的住院患者病历列表)';
//    BLL_NEWINCHRECORD: Result := 'BLL_NEWINCHRECORD(1025 新建住院病历)';
//    BLL_GETINCHRECORDCONTENT: Result := 'BLL_GETINCHRECORDCONTENT(1026 获取指定住院病历内容)';
//    BLL_SAVERECORDCONTENT: Result := 'BLL_SAVERECORDCONTENT(1027 保存指定住院病历内容)';
//  else
//    Result := '未定义[' + IntToStr(AMethodID) + ']';
//  end;
//end;

end.
