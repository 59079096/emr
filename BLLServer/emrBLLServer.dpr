program emrBLLServer;

uses
  Vcl.Forms,
  frm_BLLServer in 'frm_BLLServer.pas' {frmBLLServer};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmBLLServer, frmBLLServer);
  Application.Run;
end.
