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
