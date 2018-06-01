{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_DomainOper;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmDomainOper = class(TForm)
    lbl1: TLabel;
    lbl2: TLabel;
    edtName: TEdit;
    edtCode: TEdit;
    btnSave: TButton;
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
    FDID: Integer;
  public
    { Public declarations }
    property DID: Integer read FDID write FDID;
  end;

implementation

uses
  emr_Common, emr_BLLConst, emr_BLLServerProxy, FireDAC.Comp.Client;

{$R *.dfm}

procedure TfrmDomainOper.btnSaveClick(Sender: TObject);
var
  vCMD: Integer;
begin
  if Trim(edtName.Text) = '' then
  begin
    ShowMessage('错误，请填写值域名称！');
    Exit;
  end;

  if FDID > 0 then  // 修改
    vCMD := BLL_SETDOMAIN
  else  // 添加
    vCMD := BLL_NEWDOMAIN;

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := vCMD;

      if FDID > 0 then  // 修改
        ABLLServerReady.ExecParam.I['DID'] := FDID;

      ABLLServerReady.ExecParam.S['DCode'] := edtCode.Text;
      ABLLServerReady.ExecParam.S['DName'] := edtName.Text;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then
        ShowMessage(ABLLServer.MethodError)
      else
        ShowMessage('保存成功！');
    end);

  if FDID = 0 then  // 新建后关闭
    Close;

  Self.ModalResult := mrOk;
end;

end.
