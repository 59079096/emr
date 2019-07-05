{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_DomainItem;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmDomainItem = class(TForm)
    lbl1: TLabel;
    edtName: TEdit;
    lbl2: TLabel;
    edtCode: TEdit;
    lbl3: TLabel;
    edtPY: TEdit;
    btnSave: TButton;
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FItemID, FDomainID: Integer;
  public
    { Public declarations }
    property ItemID: Integer read FItemID write FItemID;
    property DomainID: Integer read FDomainID write FDomainID;
  end;

implementation

uses
  emr_Common, emr_BLLServerProxy, FireDAC.Comp.Client;

{$R *.dfm}

procedure TfrmDomainItem.btnSaveClick(Sender: TObject);
var
  vCMD: Integer;
begin
  if Trim(edtName.Text) = '' then
  begin
    ShowMessage('错误，填写项目名称！');
    Exit;
  end;

  if FItemID > 0 then  // 修改
    vCMD := BLL_SETDOMAINITEMINFO
  else  // 添加
    vCMD := BLL_NEWDOMAINITEM;

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := vCMD;

      if FItemID > 0 then  // 修改
        ABLLServerReady.ExecParam.I['ID'] := FItemID;

      ABLLServerReady.ExecParam.I['domainid'] := FDomainID;
      ABLLServerReady.ExecParam.S['code'] := edtCode.Text;
      ABLLServerReady.ExecParam.S['devalue'] := edtName.Text;
      ABLLServerReady.ExecParam.S['py'] := edtPY.Text;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then
        ShowMessage(ABLLServer.MethodError)
      else
        ShowMessage('保存成功！');
    end);

  if FItemID = 0 then  // 新建后关闭
    Close;

  Self.ModalResult := mrOk;
end;

procedure TfrmDomainItem.FormShow(Sender: TObject);
begin
  if FItemID > 0 then  // 修改
  begin
    HintFormShow('正在获取选项信息...', procedure(const AUpdateHint: TUpdateHint)
    begin
      BLLServerExec(
        procedure(const ABLLServerReady: TBLLServerProxy)
        begin
          ABLLServerReady.Cmd := BLL_GETDOMAINITEMINFO;  // 获取值域选项信息
          ABLLServerReady.ExecParam.I['ID'] := FItemID;
          ABLLServerReady.BackDataSet := True;
        end,
        procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
        begin
          if ABLLServer.MethodRunOk then  //
          begin
            if AMemTable <> nil then
            begin
              edtCode.Text := AMemTable.FieldByName('code').AsString;
              edtName.Text := AMemTable.FieldByName('devalue').AsString;
              edtPY.Text := AMemTable.FieldByName('py').AsString;
            end;
          end
          else
            ShowMessage(ABLLServer.MethodError);
        end);
    end);
  end
end;

end.
