{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_login;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, FunctionIntf, Vcl.StdCtrls, Vcl.Dialogs;

type
  TfrmLogin = class(TForm)
    btnOk: TButton;
    lbl1: TLabel;
    edtUserID: TEdit;
    edtPassword: TEdit;
    btnCancel: TButton;
    lbl2: TLabel;
    lblSet: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure lblSetClick(Sender: TObject);
  private
    { Private declarations }
    FOnFunctionNotify: TFunctionNotifyEvent;
  public
    { Public declarations }
  end;

  procedure PluginShowLoginForm(AIFun: IFunBLLFormShow);
  procedure PluginCloseLoginForm;

var
  frmLogin: TfrmLogin;
  PlugInID: string;

implementation

uses
  PluginConst, FunctionConst, FunctionImp, emr_Common, emr_BLLServerProxy,
  emr_MsgPack, emr_Entry, FireDAC.Comp.Client, frm_ConnSet;

{$R *.dfm}

procedure PluginShowLoginForm(AIFun: IFunBLLFormShow);
//var
//  vObjectInfo: IPlugInObjectInfo;
begin
  if FrmLogin = nil then
    FrmLogin := Tfrmlogin.Create(nil);

  FrmLogin.FOnFunctionNotify := AIFun.OnNotifyEvent;

  FrmLogin.ShowModal;

//  if FrmLogin.ModalResult = mrOk then
//  begin
//    vObjectInfo := TPlugInObjectInfo.Create;
//    vObjectInfo.&Object := TObject(FrmLogin.FUserID);
//    FrmLogin.FOnFunctionNotify(PlugInID, FUN_USERINFO, vObjectInfo);  // 告诉主程序登录用户名
//  end;

  FrmLogin.FOnFunctionNotify(PlugInID, FUN_BLLFORMDESTROY, nil);  // 释放业务窗体资源
end;

procedure PluginCloseLoginForm;
begin
  if FrmLogin <> nil then
    FreeAndNil(FrmLogin);
end;

procedure TfrmLogin.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmLogin.btnOkClick(Sender: TObject);
begin
  HintFormShow('正在登录...', procedure(const AUpdateHint: TUpdateHint)
  var
    vObjFun: IObjectFunction;
    vCertificate: TCertificate;
  begin
    vObjFun := TObjectFunction.Create;
    vCertificate := TCertificate.Create;
    try
      vCertificate.ID := edtUserID.Text;
      vCertificate.Password := MD5(edtPassword.Text);
      vObjFun.&Object := vCertificate;
      FOnFunctionNotify(PlugInID, FUN_LOGINCERTIFCATE, vObjFun);
      case vCertificate.State of
        cfsError: ShowMessage('登录失败：无效的用户或者错误的密码！');
        cfsPass: Close;
        cfsConflict: ShowMessage('登录失败：存在多个相同的用户，请联系管理员确认！');
      end;
    finally
      FreeAndNil(vCertificate);
    end;
  end);
end;

procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  PlugInID := PLUGIN_LOGIN;
  //SetWindowLong(Handle, GWL_EXSTYLE, (GetWindowLong(handle, GWL_EXSTYLE) or WS_EX_APPWINDOW));
end;

procedure TfrmLogin.lblSetClick(Sender: TObject);
var
  vFrmConnSet: TfrmConnSet;
begin
  vFrmConnSet := TfrmConnSet.Create(Self);
  try
    vFrmConnSet.ShowModal;
  finally
    FreeAndNil(vFrmConnSet);
  end;
end;

end.
