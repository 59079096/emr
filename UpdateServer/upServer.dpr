program upServer;

{$IFDEF not DEBUG}
  {$IF CompilerVersion >= 21.0}
    {$WEAKLINKRTTI ON}
    {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
  {$IFEND}
{$ENDIF}

uses
  Vcl.Forms,
  frm_UpdateServer in 'frm_UpdateServer.pas' {frmUpdateServer},
  frm_DM in 'frm_DM.pas' {dm: TDataModule},
  frm_Upgrade in 'frm_Upgrade.pas' {frmUpgrade},
  UPCommon in '..\Common\UPCommon.pas',
  UPClientContext in 'UPClientContext.pas',
  UPMsgPack in '..\Common\UPMsgPack.pas',
  UPMsgCoder in '..\Common\UPMsgCoder.pas',
  frm_UpgradeHis in 'frm_UpgradeHis.pas' {frmUpgradeHis};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmUpdateServer, frmUpdateServer);
  Application.CreateForm(Tdm, dm);
  Application.CreateForm(TfrmUpgradeHis, frmUpgradeHis);
  Application.Run;
end.
