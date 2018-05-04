program emrUpdate;

uses
  Vcl.Forms,
  System.SysUtils,
  Dialogs,
  Winapi.Windows,
  Winapi.ShellAPI,
  Winapi.TlHelp32,
  frm_DM,
  emr_Common,
  frm_Update in 'frm_Update.pas' {frmUpdate},
  emr_UpDownLoadClient in '..\Common\emr_UpDownLoadClient.pas';

{$R *.res}

function FindMvsProcessID: DWORD;
var
  vSnapShot: DWORD;
  vPE: TProcessEntry32;
  vFound: Boolean;
begin
  Result := 0;
  vSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);  // 给系统内所有的进程拍个快照
  vPE.dwSize := SizeOf(TProcessEntry32);  // 在使用这个结构前，先设置它的大小

  // 遍历进程快照，轮流显示每个进程的信息
  vFound := Process32First(vSnapShot, vPE);
  while vFound do
  begin
    if (UpperCase(ExtractFileName(vPE.szExeFile)) = 'EMR.EXE') or (UpperCase(vPE.szExeFile) = 'EMR.EXE') then
    begin
      Result := vPE.th32ProcessID;  // 找到emr.exe进程ID
      Break;
    end;
    vFound := Process32Next(vSnapShot, vPE);
  end;
end;

var
  EMRProcessID: DWORD;
  AppProcess: THandle;
  vVerStr: string;
begin
  Application.Initialize;
  try
    GetLastVersion(LastVersionID, vVerStr);  // 获取服务端当前最新的客户端版本号
    ClientVersionID := StrToIntDef(dm.GetParamStr('VersionID'), 0);  // 本地客户端版本号

    if ClientVersionID > LastVersionID then  // 客户端版高于最新的客户端版本程序(不配套)
      raise Exception.Create('异常：客户端版高于服务端版本，程序不配套！')
    else
    if ClientVersionID < LastVersionID then  // 升级
    begin
      EMRProcessID := FindMvsProcessID;  // 找emr.exe进程ID
      if EMRProcessID > 0 then  // 找到mvs.exe进程ID
      begin
        AppProcess := OpenProcess(PROCESS_VM_OPERATION or Winapi.Windows.SYNCHRONIZE,FALSE, EMRProcessID);  // 打开进程，获取权限
        WaitForSingleObject(AppProcess, INFINITE);  // 等待mvs.exe进程关闭
      end;

      Application.CreateForm(TfrmUpdate, frmUpdate);
    end
    else
    begin
      ShowMessage('emr系统当前已经是最新版本！');
      Exit;
    end;
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
      Exit;
    end;
  end;

  Application.Run;
  Application.Run;
end.
