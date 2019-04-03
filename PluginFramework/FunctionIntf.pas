{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit FunctionIntf;

interface

uses
  FunctionConst;

const
  FUN_CUSTOM = '{146E64A2-6C78-497B-B6A3-9DFBC8CE7B91}';
  FUN_PLUGIN = '{14085FF8-D940-41B6-869D-49101CFA4BF1}';

type

  ICustomFunction = interface(IInterface)  // 插件提供的功能信息基类
    [FUN_CUSTOM]
    /// <summary> 返回功能的ID,即唯一标识 </summary>
    /// <returns>ID</returns>
    function GetID: string;

    /// <summary> 设置功能的GUID </summary>
    /// <param name="Value">GUID</param>
    procedure SetID(const Value: string);

    property ID: string read GetID write SetID;
  end;

  IObjectFunction = interface(ICustomFunction)
    [FUN_OBJECT]
    function GetObject: TObject;
    procedure SetObject(const Value: TObject);
    property &Object: TObject read GetObject write SetObject;
  end;

  IPluginFunction = interface(ICustomFunction)
    [FUN_PLUGIN]
    /// <summary> 在界面显示功能调用入口 </summary>
    function GetShowEntrance: Boolean;

    /// <summary> 设置是否在界面显示功能调用入口 </summary>
    /// <param name="ASingleton">True:显示,False:不显示</param>
    procedure SetShowEntrance(const Value: Boolean);

    function GetName: string;
    procedure SetName(const Value: string);

    property Name: string read GetName write SetName;
    property ShowEntrance: Boolean read GetShowEntrance write SetShowEntrance;
  end;

  /// <summary> 插件向主程序请求指定功能事件 </summary>
  /// <param name="APluginID">请求功能的插件ID</param>
  /// <param name="AFunctionID">请求的功能ID</param>
  /// <param name="AData">功能对应的数据</param>
  TFunctionNotifyEvent = procedure(const APluginID, AFunctionID: string;
    const AObjectFun: IObjectFunction);

  /// <summary> 业务窗体功能 </summary>
  IFunBLLFormShow = interface(IPluginFunction)
    [FUN_BLLFORMSHOW]
    /// <summary> 获取主程序Application的句柄 </summary>
    /// <returns>句柄</returns>
    function GetAppHandle: THandle;

    /// <summary> 设置主程序句柄 </summary>
    /// <param name="Value">句柄</param>
    procedure SetAppHandle(const Value: THandle);

    /// <summary> 返回插件保存的主程序供插件请求处理功能的方法 </summary>
    /// <returns>方法</returns>
    function GetOnNotifyEvent: TFunctionNotifyEvent;

    /// <summary> 插件保存主程序供插件请求处理功能的方法 </summary>
    /// <param name="Value"></param>
    procedure SetOnNotifyEvent(const Value: TFunctionNotifyEvent);

    /// <summary> 主程序句柄 </summary>
    property AppHandle: THandle read GetAppHandle write SetAppHandle;

    /// <summary> 主程序处理功能事件 </summary>
    property OnNotifyEvent: TFunctionNotifyEvent read GetOnNotifyEvent write SetOnNotifyEvent;
  end;

implementation

end.
