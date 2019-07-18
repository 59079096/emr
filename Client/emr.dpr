program emr;

{$IFDEF not DEBUG}
  {$IF CompilerVersion >= 21.0}
    {$WEAKLINKRTTI ON}
    {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
  {$IFEND}
{$ENDIF}

uses
  System.ShareMem,
  Vcl.Forms,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  Vcl.Dialogs,
  Winapi.Windows,
  Winapi.ShellAPI,
  emr_Common,
  frm_Hint,
  frm_ConnSet,
  frm_Emr in 'frm_Emr.pas' {frmEmr},
  frm_DM in '..\Common\frm_DM.pas' {dm: TDataModule},
  frm_Update in '..\Common\frm_Update.pas',
  UPCommon in '..\Common\UPCommon.pas',
  UPClient in '..\Common\UPClient.pas',
  UPMsgPack in '..\Common\UPMsgPack.pas',
  UPMsgCoder in '..\Common\UPMsgCoder.pas';

{$R *.res}

const
  STR_UNIQUE = '{CC1EB815-7992-41F5-B112-571DE13CD8DF}';

var
  vFrmHint: TfrmHint;
  vMutHandle: THandle;
  vFrmConnSet: TfrmConnSet;
begin
  vMutHandle := OpenMutex(MUTEX_ALL_ACCESS, False, STR_UNIQUE);  // 打开互斥对象
  if vMutHandle = 0 then
    vMutHandle := CreateMutex(nil, False, STR_UNIQUE) // 建立互斥对象
  else
  begin
    ShowMessage('EMR客户端已经在运行！');
    Exit;
  end;

  Application.Initialize;
  Application.Title := '电子病历';
  Application.MainFormOnTaskbar := False;

  {$IFNDEF DEBUG}
  if HCUpdate then Exit;
  {$ENDIF};

  vFrmHint := TfrmHint.Create(nil);
  try
    vFrmHint.Show;
    vFrmHint.UpdateHint('正在启动EMR客户端，请稍候...');

    if not Assigned(ClientCache) then
      ClientCache := TClientCache.Create;

    GetClientParam;  // 获取本地参数

    dm := Tdm.Create(nil);

    try
      vFrmHint.UpdateHint('正在加载缓存，请稍候...');
      ClientCache.GetCacheData;
    except
      on E: Exception do
      begin
        if MessageDlg('EMR客户端启动出现异常，打开连接配置界面？' + #13#10 + #13#10
          + '异常信息：' + E.Message,
          TMsgDlgType.mtError, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes
        then
        begin
          FreeAndNil(vFrmHint);
          Application.CreateForm(TFrmConnSet, vFrmConnSet);
  // 创建连接配置界面
          Application.Run;
        end;

        FreeAndNil(vFrmConnSet);
        if Assigned(ClientCache) then
          FreeAndNil(ClientCache);

        FreeAndNil(dm);

        Exit;
      end;
    end;

    vFrmHint.UpdateHint('正在启动程序，请稍候...');
    Application.CreateForm(TfrmEmr, frmEmr);
  finally
    FreeAndNil(vFrmHint);
  end;

  if frmEmr.LoginPluginExecute then  // 登录成功
    Application.Run;

  FreeAndNil(frmEmr);
  FreeAndNil(dm);
  if Assigned(ClientCache) then
    FreeAndNil(ClientCache);
end.
