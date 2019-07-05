{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_ConnSet;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmConnSet = class(TForm)
    edtBLLServerIP: TEdit;
    edtBLLServerPort: TEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    edtMsgServerIP: TEdit;
    edtMsgServerPort: TEdit;
    lbl4: TLabel;
    lbl5: TLabel;
    lbl6: TLabel;
    btnSave: TButton;
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  System.IniFiles, emr_Common;

{$R *.dfm}

procedure TfrmConnSet.btnSaveClick(Sender: TObject);
var
  vIniFile: TIniFile;
begin
  vIniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'emr.ini');
  try
    //vIniFile.WriteInteger('Client', 'TimeOut', 3000);  // 3秒
    //vIniFile.WriteInteger('Client', PARAM_LOCAL_VERSIONID, 0);

    vIniFile.WriteString('BLLServer', PARAM_LOCAL_BLLHOST, edtBLLServerIP.Text);  // 业务服务端
    vIniFile.WriteString('BLLServer', PARAM_LOCAL_BLLPORT, edtBLLServerPort.Text);  // 业务服务端端口

    vIniFile.WriteString('MsgServer', PARAM_LOCAL_MSGHOST, edtMsgServerIP.Text);  // 消息服务端
    vIniFile.WriteString('MsgServer', PARAM_LOCAL_MSGPORT, edtMsgServerPort.Text);  // 消息服务端端口
  finally
    FreeAndNil(vIniFile);
  end;

  ShowMessage('保存成功！');
end;

procedure TfrmConnSet.FormShow(Sender: TObject);
var
  vIniFile: TIniFile;
begin
  vIniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'emr.ini');
  try
    edtBLLServerIP.Text := vIniFile.ReadString('BLLServer', PARAM_LOCAL_BLLHOST, '127.0.0.1');  // 业务服务端
    edtBLLServerPort.Text := vIniFile.ReadString('BLLServer', PARAM_LOCAL_BLLPORT, '12830');  // 业务服务端端口
    edtMsgServerIP.Text := vIniFile.ReadString('MsgServer', PARAM_LOCAL_MSGHOST, '127.0.0.1');  // 消息服务端
    edtMsgServerPort.Text := vIniFile.ReadString('MsgServer', PARAM_LOCAL_MSGPORT, '12832');  // 消息服务端端口
  finally
    FreeAndNil(vIniFile);
  end;
end;

end.
