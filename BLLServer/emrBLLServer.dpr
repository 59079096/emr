program emrBLLServer;

uses
  Forms,
  frm_BLLServer in 'frm_BLLServer.pas' {frmBLLServer},
  frm_Set in 'frm_Set.pas' {frmSet};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmBLLServer, frmBLLServer);
  Application.CreateForm(TfrmSet, frmSet);
  Application.Run;
end.
