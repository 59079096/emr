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
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FUserID: string;
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
  emr_MsgPack, emr_PluginObject, emr_Entry, IdHashMessageDigest,
  FireDAC.Comp.Client, frm_ConnSet;

{$R *.dfm}

procedure PluginShowLoginForm(AIFun: IFunBLLFormShow);
var
  vUserInfo: IPlugInUserInfo;
begin
  if FrmLogin = nil then
    FrmLogin := Tfrmlogin.Create(nil);

  FrmLogin.FOnFunctionNotify := AIFun.OnNotifyEvent;

  FrmLogin.ShowModal;

  if FrmLogin.ModalResult = mrOk then
  begin
    vUserInfo := TPlugInUserInfo.Create;
    vUserInfo.UserID := FrmLogin.FUserID;
    FrmLogin.FOnFunctionNotify(PlugInID, FUN_USERINFO, vUserInfo);  // 告诉主程序登录用户名
  end;

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
  begin
    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)  // 获取登录用户的信息
      var
        vExecParam: TMsgPack;
        vPAWMD5: TIdHashMessageDigest5;
      begin
        ABLLServerReady.Cmd := BLL_LOGIN;  // 核对登录信息
        vExecParam := ABLLServerReady.ExecParam;
        vExecParam.S[TUser.ID] := edtUserID.Text;
        vPAWMD5 :=  TIdHashMessageDigest5.Create;
        try
          vExecParam.S[TUser.Password] := vPAWMD5.HashStringAsHex(edtPassword.Text);
        finally
          vPAWMD5.Free;
        end;

        ABLLServerReady.AddBackField(TUser.ID);
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      begin
        if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
        begin
          ShowMessage(ABLLServer.MethodError);
          Exit;
        end;
        if ABLLServer.RecordCount = 1 then
        begin
          FUserID := ABLLServer.BackField(TUser.ID).AsString;
          Self.ModalResult := mrOk;
        end
        else
        if ABLLServer.RecordCount = 0 then
          ShowMessage('登录失败：无效的用户或者错误的密码！')
        else
        if ABLLServer.RecordCount > 1 then
          ShowMessage('登录失败：存在多个相同的用户，请管理员确认');
      end);
  end);
end;

procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  PlugInID := PLUGIN_LOGIN;
  //SetWindowLong(Handle, GWL_EXSTYLE, (GetWindowLong(handle, GWL_EXSTYLE) or WS_EX_APPWINDOW));
end;

procedure TfrmLogin.FormShow(Sender: TObject);
var
  vObjectInfo: IPlugInObjectInfo;
begin
  // 获取客户缓存对象
  vObjectInfo := TPlugInObjectInfo.Create;
  FOnFunctionNotify(PluginID, FUN_CLIENTCACHE, vObjectInfo);
  ClientCache := TClientCache(vObjectInfo.&Object);
end;

procedure TfrmLogin.lblSetClick(Sender: TObject);
var
  vFrmConnSet: TfrmConnSet;
begin
  vFrmConnSet := TfrmConnSet.Create(Self);
  try
    vFrmConnSet.edtBLLServerIP.Text := ClientCache.ClientParam.BLLServerIP;
    vFrmConnSet.edtBLLServerPort.Text := ClientCache.ClientParam.BLLServerPort.ToString;
    vFrmConnSet.edtMsgServerIP.Text := ClientCache.ClientParam.MsgServerIP;
    vFrmConnSet.edtMsgServerPort.Text := ClientCache.ClientParam.MsgServerPort.ToString;
    vFrmConnSet.edtUpdateServerIP.Text := ClientCache.ClientParam.UpdateServerIP;
    vFrmConnSet.edtUpdateServerPort.Text := ClientCache.ClientParam.UpdateServerPort.ToString;
    vFrmConnSet.ShowModal;
  finally
    FreeAndNil(vFrmConnSet);
  end;
end;

end.
