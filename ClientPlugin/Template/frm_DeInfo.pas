{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_DeInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmDeInfo = class(TForm)
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    lbl6: TLabel;
    lbl7: TLabel;
    lbl8: TLabel;
    edtCode: TEdit;
    edtName: TEdit;
    edtPY: TEdit;
    edtDefine: TEdit;
    edtType: TEdit;
    edtFormat: TEdit;
    edtUnit: TEdit;
    lbl9: TLabel;
    edtDomainID: TEdit;
    btnSave: TButton;
    cbbFrmtp: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
    FDeID: Integer;
  public
    { Public declarations }
    property DeID: Integer read FDeID write FDeID;
  end;

implementation

uses
  emr_Common, emr_BLLConst, emr_BLLServerProxy, FireDAC.Comp.Client, EmrElementItem;

{$R *.dfm}

function GetFrmptText(const AFrmpt: string): string;
begin
  if AFrmpt = TDeFrmtp.Radio then
    Result := '单选'
  else
  if AFrmpt = TDeFrmtp.Multiselect then
    Result := '多选'
  else
  if AFrmpt = TDeFrmtp.Number then
    Result := '数值'
  else
  if AFrmpt = TDeFrmtp.String then
    Result := '文本'
  else
  if AFrmpt = TDeFrmtp.Date then
    Result := '日期'
  else
  if AFrmpt = TDeFrmtp.Time then
    Result := '时间'
  else
  if AFrmpt = TDeFrmtp.DateTime then
    Result := '日期时间'
  else
    Result := '';
end;

function GetFrmpt(const AText: string): string;
begin
  if AText = '单选' then
    Result := TDeFrmtp.Radio
  else
  if AText = '多选' then
    Result := TDeFrmtp.Multiselect
  else
  if AText = '数值' then
    Result := TDeFrmtp.Number
  else
  if AText = '文本' then
    Result := TDeFrmtp.String
  else
  if AText = '日期' then
    Result := TDeFrmtp.Date
  else
  if AText = '时间' then
    Result := TDeFrmtp.Time
  else
  if AText = '日期时间' then
    Result := TDeFrmtp.DateTime
  else
    Result := '';
end;

procedure TfrmDeInfo.btnSaveClick(Sender: TObject);
var
  vDomainID, vCMD: Integer;
begin
  if Trim(edtName.Text) = '' then
  begin
    ShowMessage('错误，填写数据元名称！');
    Exit;
  end;

  if not TryStrToInt(edtDomainID.Text, vDomainID) then
  begin
    ShowMessage('错误，填写的值域不能转为整数！');
    Exit;
  end;

  if FDeID > 0 then  // 修改
    vCMD := BLL_SETDEINFO
  else  // 新建
    vCMD := BLL_NEWDE;

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := vCMD;

      if FDeID > 0 then  // 修改
        ABLLServerReady.ExecParam.I['DeID'] := FDeID;

      ABLLServerReady.ExecParam.S['decode'] := edtCode.Text;
      ABLLServerReady.ExecParam.S['dename'] := edtName.Text;
      ABLLServerReady.ExecParam.S['py'] := edtPY.Text;
      ABLLServerReady.ExecParam.S['dedefine'] := edtDefine.Text;
      ABLLServerReady.ExecParam.S['detype'] := edtType.Text;
      ABLLServerReady.ExecParam.S['deformat'] := edtFormat.Text;
      ABLLServerReady.ExecParam.S['deunit'] := edtUnit.Text;
      ABLLServerReady.ExecParam.S['frmtp'] := GetFrmpt(cbbFrmtp.Text);
      ABLLServerReady.ExecParam.I['domainid'] := vDomainID;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then
        ShowMessage(ABLLServer.MethodError)
      else
        ShowMessage('保存成功！');
    end);

  if FDeID = 0 then  // 新建后关闭
    Close;

  Self.ModalResult := mrOk;
end;

procedure TfrmDeInfo.FormShow(Sender: TObject);
begin
  if FDeID > 0 then
  begin
    Caption := '数据元维护-' + FDeID.ToString;

    HintFormShow('正在获取数据元信息...', procedure(const AUpdateHint: TUpdateHint)
    begin
      BLLServerExec(
        procedure(const ABLLServerReady: TBLLServerProxy)
        begin
          ABLLServerReady.Cmd := BLL_GETDEINFO;  // 获取数据元信息
          ABLLServerReady.ExecParam.I['DeID'] := FDeID;
          ABLLServerReady.BackDataSet := True;
        end,
        procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
        begin
          if ABLLServer.MethodRunOk then  //
          begin
            if AMemTable <> nil then
            begin
              edtCode.Text := AMemTable.FieldByName('decode').AsString;
              edtName.Text := AMemTable.FieldByName('dename').AsString;
              edtPY.Text := AMemTable.FieldByName('py').AsString;
              edtDefine.Text := AMemTable.FieldByName('dedefine').AsString;
              edtType.Text := AMemTable.FieldByName('detype').AsString;
              edtFormat.Text := AMemTable.FieldByName('deformat').AsString;
              edtUnit.Text := AMemTable.FieldByName('deunit').AsString;
              cbbFrmtp.ItemIndex := cbbFrmtp.Items.IndexOf(GetFrmptText(AMemTable.FieldByName('frmtp').AsString));
              edtDomainID.Text := AMemTable.FieldByName('domainid').AsString;
            end;
          end
          else
            ShowMessage(ABLLServer.MethodError);
        end);
    end);
  end
  else
    Caption := '新建数据元'
end;

end.
