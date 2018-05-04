unit emr_MsgConst;

interface

const
  MSG_CMD = 'm.cmd';
  MSG_DEVICETYPE = 'm.dc';
  MSG_STRING = 'm.str';
  BLL_CMD = 'b.cmd';
  { 消息指令码 }
  CMD_KEEPALIVE = 1;  // 心跳包
  CMD_DIOCP = 1000;  // 分界点 1000 以下分配给DIOCP通讯框架用1000以上由业务使用

  CMD_MSG = CMD_DIOCP + 1;  // 发送即时消息
  CMD_LOGIN = CMD_DIOCP + 2;  // 登录
  CMD_LOGOUT = CMD_DIOCP + 3;  // 登出
  CMD_DOWNLOAD = CMD_DIOCP + 4;  // 下载数据
  CMD_UPLOAD = CMD_DIOCP + 5;  // 上传数据

  CMD_SERVER = 2000;
  CMD_SRV_CLOSE = CMD_SERVER + 1;  // 服务端关闭
  CMD_SRV_ENFORCELOGOUT = CMD_SERVER + 2;  // 服务端强制要求客户端下线
  CMD_SRV_BLL = CMD_SERVER + 3;  // 业务数据
  CMD_BLL_COLLECTORDATA = CMD_SRV_BLL + 1;  // 采集器发来的数据

  { 参数常量 }
  SRVDT = 'p.srvdt';    // 服务端当前时间
  TOID = 'p.toid';      // 消息目标人ID to work number
  FROMID = 'p.fromid';  // 消息来源人ID
  TOCLIENTTYPE = 'p.tocct';  // 消息目标客户端类型，TClientType枚举 大屏、用户、采集设备等
  FROMCLIENTTYPE = 'p.fromcct';  //  消息来源客户端类型

  ITEMTYPE = 'i.type';
  ITEMVALUE = 'i.val';
  ITEMPATNO = 'i.pno';
type
  /// <summary> 设备类型 </summary>
  TDeviceType = (
    /// <summary> 无设备 </summary>
    cdtNone,
    /// <summary> PC </summary>
    cdtPC,
    /// <summary> 移动设备 </summary>
    cdtMobile
    );

  TClientType = (
    /// <summary> 无类型 </summary>
    cctNone,
    /// <summary> 大屏 </summary>
    cctLSD,
    /// <summary> 用户 </summary>
    cctUser,
    /// <summary> 数据采集器 </summary>
    cctCollector
  );

implementation

end.
