unit frm_Demo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FunctionIntf;

type
  TfrmDemo = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FOnFunctionNotify: TFunctionNotifyEvent;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  end;

  procedure PluginShowDemoForm(AIFun: IFunBLLFormShow);
  procedure PluginCloseDemoForm;

var
  frmDemo: TfrmDemo;
  PlugInID: string;

implementation

uses
  PluginConst, FunctionConst;

{$R *.dfm}

procedure PluginShowDemoForm(AIFun: IFunBLLFormShow);
begin
  if not Assigned(frmDemo) then
  begin
    Application.CreateForm(TfrmDemo, frmDemo);
    PluginID := PLUGIN_DEMO;
  end;

  frmDemo.FOnFunctionNotify := AIFun.OnNotifyEvent;  // 插件和主程序通过此方法交互
  frmDemo.Show;
end;

procedure PluginCloseDemoForm;
begin
  if frmDemo <> nil then
    FreeAndNil(frmDemo);
end;

procedure TfrmDemo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  //Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TfrmDemo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FOnFunctionNotify(PluginID, FUN_MAINFORMSHOW, nil);  // 显示主窗体
  FOnFunctionNotify(PluginID, FUN_BLLFORMDESTROY, nil);  // 释放业务窗体资源
end;

procedure TfrmDemo.FormShow(Sender: TObject);
begin
  FOnFunctionNotify(PluginID, FUN_MAINFORMHIDE, nil);  // 隐藏主窗体
end;

end.
