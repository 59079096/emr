{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}
{                                                       }
{                  插件功能ID定义部分                   }
{             注意：Function的ID第1位必须是1            }
{                                                       }
{*******************************************************}

unit FunctionConst;

interface

const
  /// <summary> 业务主窗体显示 </summary>
  FUN_BLLFORMSHOW = '{1E5DBC75-B7CE-49D4-9897-9B8D65E51764}';

  /// <summary> 业务主窗体关闭 </summary>
  FUN_BLLFORMDESTROY = '{1DC8CC53-E029-4437-87DD-8240BF6EBA6E}';

  /// <summary> 主窗体显示 </summary>
  FUN_MAINFORMSHOW = '{1D9750FD-DD19-411B-B078-52A2C9B8A37C}';

  /// <summary> 主窗体隐藏 </summary>
  FUN_MAINFORMHIDE = '{1E10987C-3332-4A6F-97EA-0112AF8C824E}';

  /// <summary> Application 空闲事件 </summary>
  FUN_APPEVENTSIDLE = '{186BB780-7FF5-40C8-9B4D-828738BAB752}';

  /// <summary> 用户信息功能 </summary>
  FUN_USERINFO = '{15FB3FE4-19DD-406C-B543-CF60FE364E88}';

  /// <summary> 获取客户端缓存 </summary>
  FUN_CLIENTCACHE = '{1DF659CE-7108-4027-BF3F-1020CBD82075}';
//
//  /// <summary> 获取消息服务端地址、端口 </summary>
//  FUN_MSGSERVERINFO = '{134282C1-2555-4552-8DBD-C2A0AC796C8F}';
//
//  /// <summary> 获取升级服务端地址、端口 </summary>
//  FUN_UPDATESERVERINFO = '{1629791C-CA99-44EF-A403-8963B1ADC5C0}';

  /// <summary> 获取本地数据库操作模块 </summary>
  FUN_LOCALDATAMODULE = '{16DC5603-5ECD-4DF7-827E-5F85DDC9FB89}';

  /// <summary> 获取插件中的对象 </summary>
  //FUN_GETOBJECT = '{1A820E87-FE39-43F1-8EFF-8D08F0716378}';

  /// <summary> 获取插件中的资源 </summary>
  //FUN_GETRESOURCE = '{1E24BDC4-4541-49B5-B147-405BE1BE3ECA}';

  // '{1BC0FA9B-146F-4847-9F6F-4F62B82C4743}'
  // '{1B1DAA31-082F-41B9-8CD1-5070659DEA6E}'

implementation

end.
