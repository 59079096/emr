{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_TemplateInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmTemplateInfo = class(TForm)
    lbl1: TLabel;
    edtTName: TEdit;
    btnSave: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
    FTempID: Integer;
    FTempName: string;
  public
    { Public declarations }
    property TempID: Integer read FTempID write FTempID;
    property TempName: string read FTempName write FTempName;
  end;

implementation

uses
  emr_Common, emr_BLLInvoke, FireDAC.Comp.Client;

{$R *.dfm}

procedure TfrmTemplateInfo.btnSaveClick(Sender: TObject);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_SETTEMPLATEINFO;  // 保存模板信息
      ABLLServerReady.ExecParam.I['id'] := FTempID;
      ABLLServerReady.ExecParam.ForcePathObject('tname').AsString := Trim(edtTName.Text);
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if ABLLServer.MethodRunOk then  // 获取成功
      begin
        FTempName := Trim(edtTName.Text);
        ShowMessage('修改成功！');
      end
      else
        ShowMessage(ABLLServer.MethodError);
    end);
end;

procedure TfrmTemplateInfo.FormShow(Sender: TObject);
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETTEMPLATEINFO;  // 获取模板信息
      ABLLServerReady.ExecParam.I['id'] := FTempID;
      ABLLServerReady.AddBackField('tname');
      ABLLServerReady.AddBackField('desid');
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if ABLLServer.MethodRunOk then  // 获取成功
      begin
        FTempName := ABLLServer.BackField('tname').AsString;
        edtTName.Text := FTempName;
        Self.Caption := FTempName;
      end
      else
        ShowMessage(ABLLServer.MethodError);
    end);
end;

end.
