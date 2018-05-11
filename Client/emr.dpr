program emr;

{ 关闭RTTI反射机制减少EXE文件尺寸 }
{$IF CompilerVersion >= 21.0}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}

uses
  Vcl.Forms,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  Vcl.Dialogs,
  Winapi.Windows,
  Winapi.ShellAPI,
  emr_UpDownLoadClient,
  emr_Common,
  frm_DM,
  frm_Hint,
  frm_ConnSet,
  frm_Emr in 'frm_Emr.pas' {frmEmr};

{$R *.res}

var
  vFrmHint: TfrmHint;

{$REGION 'DownLoadUpdateExe下载Update.exe文件'}
function DownLoadUpdateExe: Boolean;
var
  vFileStream: TFileStream;
  vUpDownLoadClient: TUpDownLoadClient;
begin
  Result := False;
  vUpDownLoadClient := TUpDownLoadClient.Create;
  try
    vUpDownLoadClient.Host := GClientParam.UpdateServerIP;  // 更新服务器IP
    vUpDownLoadClient.Port := GClientParam.UpdateServerPort;  // 更新服务器端口
    try
      vUpDownLoadClient.Connect;
    except
      ShowMessage('异常：连接升级服务器失败，请检查('
        + GClientParam.UpdateServerIP + ':'
        + GClientParam.UpdateServerPort.ToString + ')！');

      Exit;
    end;

    if vUpDownLoadClient.Connected then  // 连接更新服务器成功
    begin
      vFileStream := TFileStream.Create(ExtractFilePath(ParamStr(0)) + 'update.exe', fmCreate or fmShareDenyWrite);
      try
        if vUpDownLoadClient.DownLoadFile('update.exe', vFileStream,
          procedure(const AReciveSize, AFileSize: Integer)
          begin
            vFrmHint.UpdateHint('正在下载更新程序，请稍候...' + Round(AReciveSize / AFileSize * 100).ToString + '%');
          end)
        then  // 下载update.exe成功
          Result := True
        else
          raise Exception.Create('异常：下载升级文件update.exe失败！' + vUpDownLoadClient.CurError);
      finally
        vFileStream.Free;
      end;
    end
    else
    begin
      raise Exception.Create('异常：连接升级服务器失败，请检查('
        + GClientParam.UpdateServerIP + ':'
        + GClientParam.UpdateServerPort.ToString + ')！');
    end;
  finally
    vUpDownLoadClient.Free;
  end;
end;
{$ENDREGION}

var
  vLastVerID, vClientVersionID: Integer;
  vLastVerStr: string;
  vFrmConnSet: TfrmConnSet;
begin
  Application.Initialize;
  Application.Title := '电子病历';
  Application.MainFormOnTaskbar := False;

  vFrmHint := TfrmHint.Create(nil);
  try
    vFrmHint.Show;
    vFrmHint.UpdateHint('正在启动emr程序，请稍候...');

    Application.CreateForm(Tdm, dm);
    GetClientParam;  // 获取本地参数

    // 校验升级
    try
      GetLastVersion(vLastVerID, vLastVerStr);  // 服务端当前最新的客户端版本号
      vClientVersionID := StrToIntDef(dm.GetParamStr('VersionID'), 0);  // 本地客户端版本号

      if vClientVersionID <> vLastVerID then  // 版本不一致
      begin
        if vClientVersionID > vLastVerID then  // 客户端版高于服务端当前最新的客户端版本号
          ShowMessage('客户端版高于服务端版本，程序不配套！')
        else
        if vClientVersionID < vLastVerID then  // 需要升级
        begin
          if DownLoadUpdateExe then  // 下载Update.exe文件，内部会处理错误和下载失败时提示信息
          begin
            vFrmHint.UpdateHint('正在启动更新程序，请稍候...');
            ShellExecute(GetDesktopWindow, nil, 'update.exe', nil, nil, SW_SHOWNORMAL);  // 启动Update.exe更新程序
          end;
        end;

        dm.Free;
        GClientParam.Free;
        Exit;
      end;
    except
      on E: Exception do
      begin
        if MessageDlg('emr系统客户端启动出现异常：' + E.Message + ' 是否打开连接配置界面？',
          TMsgDlgType.mtError, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes
        then
        begin
          Application.CreateForm(TfrmConnSet, vFrmConnSet);  // 创建连接配置界面
          Application.Run;
        end;

        dm.Free;
        GClientParam.Free;
        Exit;
      end;
    end;
  finally
    FreeAndNil(vFrmHint);
  end;

  Application.CreateForm(TfrmEmr, frmEmr);
  if frmEmr.LoginPluginExec then  // 登录成功
    Application.Run;

  FreeAndNil(frmEmr);
  FreeAndNil(dm);
  FreeAndNil(GClientParam);
end.
