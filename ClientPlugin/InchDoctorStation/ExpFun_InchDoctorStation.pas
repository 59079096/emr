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
{     住院医生站插件功能实现单元 hc 2016-6-7            }
{                                                       }
{     导出函数单元，为主程序和插件交互提供以下函数：    }
{     1.GetPluginInfo获取插件信息                       }
{     2.ExecFunction调用插件某功能                      }
{*******************************************************}

unit ExpFun_InchDoctorStation;

interface

uses
  PluginIntf, FunctionIntf;

/// <summary>
/// 返回插件信息和注册插件提供的功能
/// </summary>
/// <param name="AIPlugin">插件信息</param>
procedure GetPluginInfo(const AIPlugin: IPlugin); stdcall;

/// <summary>
/// 卸载插件
/// </summary>
/// <param name="AIPlugin">插件信息</param>
procedure UnLoadPlugin(const AIPlugin: IPlugin); stdcall;

/// <summary>
/// 执行功能
/// </summary>
/// <param name="AIService">功能类型</param>
procedure ExecFunction(const AIFun: ICustomFunction); stdcall;

exports
   GetPluginInfo,
   ExecFunction,
   UnLoadPlugin;

implementation

uses
  FunctionImp, PluginConst, FunctionConst, Vcl.Forms, frm_InchDoctorStation;

// 插件信息和注册功能
procedure GetPluginInfo(const AIPlugin: IPlugin); stdcall;
begin
  AIPlugin.Author := 'HC';  // 插件模块作者
  AIPlugin.Comment := '完成住院医生站业务';  // 插件说明
  AIPlugin.ID := PLUGIN_INCHDOCTORSTATION; // 插件GUID，即唯一标识
  AIPlugin.Name := '住院医生站';  // 插件功能或业务名称
  AIPlugin.Version := '1.0.0';  // 插件版本号
  //
  with AIPlugin.RegFunction(FUN_BLLFORMSHOW, '住院医生站') do
    ShowEntrance := True;  // 在界面显示调用入口
end;

procedure ExecFunction(const AIFun: ICustomFunction); stdcall;
var
  vID: string;
  vIFun: IFunBLLFormShow;
begin
  vID := AIFun.ID;
  if vID = FUN_BLLFORMSHOW then  // 显示业务窗体
  begin
    vIFun := TFunBLLFormShow.Create;
    vIFun.AppHandle := (AIFun as IFunBLLFormShow).AppHandle;
    //Application.Handle := vIFun.AppHandle;
    vIFun.ShowEntrance := (AIFun as IFunBLLFormShow).ShowEntrance;  // 显示入口点
    vIFun.OnNotifyEvent := (AIFun as IFunBLLFormShow).OnNotifyEvent;  // 插件事件

    PluginShowInchDoctorStationForm(vIFun);
  end
  else
  if vID = FUN_BLLFORMDESTROY then  // 业务窗体关闭
    PluginCloseInchDoctorStationForm;
end;

procedure UnLoadPlugin(const AIPlugin: IPlugin); stdcall;
begin
  PluginCloseInchDoctorStationForm;
end;

end.
