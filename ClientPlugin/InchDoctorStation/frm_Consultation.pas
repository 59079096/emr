{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_Consultation;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmConsultation = class(TForm)
    btnApply: TButton;
    procedure btnApplyClick(Sender: TObject);
  private
    { Private declarations }
    /// <summary> 新建会诊申请 </summary>
    procedure NewConsultation;
    /// <summary> 新建会诊受邀信息 </summary>
    procedure NewConsultationInvitee;
  public
    { Public declarations }
  end;

var
  frmConsultation: TfrmConsultation;

implementation

{$R *.dfm}

procedure TfrmConsultation.btnApplyClick(Sender: TObject);
begin
  NewConsultation;
  NewConsultationInvitee;
end;

procedure TfrmConsultation.NewConsultation;
begin

end;

procedure TfrmConsultation.NewConsultationInvitee;
begin

end;

end.
