program msgServer;

uses
  Vcl.Forms,
  frm_MsgServer in 'frm_MsgServer.pas' {frmMsgServer};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMsgServer, frmMsgServer);
  Application.Run;
end.
