{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit PluginIntf;

interface

uses
  Classes, FunctionIntf;

const
  PLUGIN_INFO = '{09A9DC5B-97FC-43D1-ACFD-B1E86D878238}';

type
  IPlugin = interface(IInterface)  // 插件信息
    [PLUGIN_INFO]
    /// <summary> 返回插件的cpi文件名(带路径) </summary>
    /// <returns></returns>
    function GetFileName: string;
    procedure SetFileName(const AFileName: string);

    /// <summary> 加载插件 </summary>
    /// <param name="AFileName">插件文件名</param>
    /// <returns>True:加载成功,False加载失败</returns>
    procedure LoadPlugin;

    /// <summary> 卸载插件 </summary>
    /// <returns></returns>
    procedure UnLoadPlugin;

    procedure GetPluginInfo;

    /// <summary> 插件注册一个可向外部提供的功能 </summary>
    /// <param name="AID">功能唯一ID</param>
    /// <param name="AName">功能名称</param>
    /// <returns>返回注册好的功能</returns>
    function RegFunction(const AID, AName: string): IPluginFunction;

    /// <summary> 插件执行一个功能 </summary>
    /// <param name="AIFun">功能</param>
    procedure ExecFunction(const AIFun: ICustomFunction);

    /// <summary> 返回插件提供的功能数量 </summary>
    /// <returns>功能数量</returns>
    function GetFunctionCount: Integer;

    /// <summary> 返回插件指定功能 </summary>
    /// <param name="AIndex"></param>
    /// <returns></returns>
    function GetFunction(const AIndex: Integer): IPluginFunction; overload;
    function GetFunction(const AID: string): IPluginFunction; overload;

    /// <summary> 返回插件的作者 </summary>
    /// <returns>作者</returns>
    function GetAuthor: string;

    /// <summary> 指定插件的作者 </summary>
    /// <param name="Value">作者</param>
    procedure SetAuthor(const Value: string);

    /// <summary> 返回插件的说明 </summary>
    /// <returns>说明信息</returns>
    function GetComment: string;

    /// <summary> 设置插件的说明 </summary>
    /// <param name="Value">说明信息</param>
    procedure SetComment(const Value: string);

    /// <summary> 返回插件的唯一ID </summary>
    /// <returns>ID</returns>
    function GetID: string;

    /// <summary> 设置插件的唯一ID(GUID) </summary>
    /// <param name="Value">GUID</param>
    procedure SetID(const Value: string);

    /// <summary> 返回插件的功能或业务名称 </summary>
    /// <returns>功能或业务名称</returns>
    function GetName: string;

    /// <summary> 设置插件的功能或业务名称 </summary>
    /// <param name="Value">功能或业务名称</param>
    procedure SetName(const Value: string);

    /// <summary> 返回插件的版本号 </summary>
    /// <returns>版本号</returns>
    function GetVersion: string;

    /// <summary> 设置插件的版本号 </summary>
    /// <param name="Value">版本号</param>
    procedure SetVersion(const Value: string);

    // 接口属性
    property ID: string read GetID write SetID;
    property Author: string read GetAuthor write SetAuthor;
    property Comment: string read GetComment write SetComment;
    property Name: string read GetName write SetName;
    property Version: string read GetVersion write SetVersion;
    property FunctionCount: Integer read GetFunctionCount;
    property FileName: string read GetFileName write SetFileName;
  end;

  TPluginList = class(TList);

  IPluginManager = interface(IInterface)
    ['{3B27642C-376E-4140-B5E0-B25AD258B7FC}']

    /// <summary> 加载指定目录下指定后缀名的所有插件 </summary>
    /// <param name="APath">路径</param>
    /// <param name="AExt">后缀名</param>
    /// <returns></returns>
    function LoadPlugins(const APath, AExt: string): Boolean;

    /// <summary> 加载指定的插件 </summary>
    /// <param name="AFileName">插件文件名</param>
    /// <returns>True：加载成功，False：加载失败</returns>
    function LoadPlugin(const AFileName: string): Boolean;

    function UnLoadPlugin(const APluginID: string): Boolean;

    /// <summary> 根据插件ID获取插件 </summary>
    /// <param name="APluginID">插件ID</param>
    /// <returns></returns>
    function GetPlugin(const APluginID: string): IPlugin;

    /// <summary> 返回插件列表 </summary>
    /// <returns>插件列表</returns>
    function PluginList: TPluginList;

    /// <summary> 插件数量 </summary>
    /// <returns>插件数量</returns>
    function Count: Integer;

    /// <summary> 向所有插件广播一个服务 </summary>
    procedure FunBroadcast(const AFun: ICustomFunction);

    /// <summary> 卸载所有插件 </summary>
    /// <returns></returns>
    function UnLoadAllPlugin: Boolean;
  end;

implementation

end.
