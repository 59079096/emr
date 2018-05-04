unit frm_Update;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Types, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Generics.Collections, emr_Common, emr_UpDownLoadClient, Vcl.Grids, Vcl.ComCtrls,
  System.IOUtils;

type
  TfrmUpdate = class(TForm)
    btnOK: TButton;
    mmoUpdateExplain: TMemo;
    sgdUpdateFiles: TStringGrid;
    lblHint: TLabel;
    pb: TProgressBar;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FUpDownLoadClient: TUpDownLoadClient;
    FUpdateFiles: TObjectList<TUpdateFile>;  // 存放更新的文件

    /// <summary> 获取需要升级的所有文件 </summary>
    /// <param name="AMinVersion">最小版本号</param>
    procedure GetUpdateInfo(const AMinVersion: Integer);

    /// <summary>
    /// 获取指定文件在更新文件集的索引
    /// </summary>
    /// <param name="AFile">文件</param>
    /// <returns>索引</returns>
    function GetUpdateFileIndex(const AFile: TUpdateFile): Integer;

    /// <summary> 下载更新文件是否成功 </summary>
    /// <returns>True: 下载成功</returns>
    function DownLoadUpdateFiles: Boolean;

    /// <summary> 备份要升级的文件 </summary>
    procedure BackUpUpdateFiles;

    /// <summary> 下载好的文件替换现有文件 </summary>
    /// <returns>True: 替换成功</returns>
    function ReplaceUpdateFiles: Boolean;

    /// <summary> 更新本地数据库中程序版本号 </summary>
    /// <param name="AVersionID">版本号</param>
    procedure UpdateVersion(const AVersionID: Integer);

    /// <summary> 删除指定时间之前的备份文件 </summary>
    /// <param name="ADays">天数</param>
    procedure DeleteBackupFilesBeforeDays(const ADays: Integer);

    /// <summary>
    /// 获取指定文件名在Gird所在的行
    /// </summary>
    /// <param name="AFileName">文件名</param>
    /// <returns>行</returns>
    function GetRowByFileName(const AFileName: string): Integer;

    /// <summary> 将更新的文件显示在表格上 </summary>
    procedure ShowUpdateFiles;
  public
    { Public declarations }
  end;

var
  frmUpdate: TfrmUpdate;
  ClientVersionID, LastVersionID: Integer;

implementation

uses
  System.DateUtils, emr_BLLServerProxy, emr_MsgPack, emr_BLLConst, FireDAC.Comp.Client,
  Xml.XMLDoc, Xml.XMLIntf, frm_DM;

{$R *.dfm}

procedure TfrmUpdate.BackUpUpdateFiles;
var
  vFilePath, vFileName: string;
  i: Integer;
begin
  vFilePath := ExtractFilePath(ParamStr(0)) + 'backup\' + FormatDateTime('yyyy-MM-dd hh-mm', Now);
  if not DirectoryExists(vFilePath) then  // 创建当前时间的备份文件夹
    CreateDirectory(PChar(vFilePath), nil);
  for i := 0 to FUpdateFiles.Count - 1 do  // 遍历升级的文件
  begin
    vFileName := ExtractFilePath(ParamStr(0)) + FUpdateFiles[i].RelativePath + FUpdateFiles[i].FileName;
    if not FileExists(vFileName) then  // 文件不存在则不备份
    begin
      pb.Position := pb.Position + 1;
      Continue;
    end;

    if not DirectoryExists(PChar(ExtractFileDir(vFilePath + '\' + FUpdateFiles[i].RelativePath))) then  // 备份文件夹里的文件路径不存时创建
      CreateDirectory(PChar(ExtractFileDir(vFilePath + '\' + FUpdateFiles[i].RelativePath)), nil);

    lblHint.Caption := '正在备份文件 ' + FUpdateFiles[i].RelativePath + FUpdateFiles[i].FileName;
    Application.ProcessMessages;
    if not CopyFile(PChar(vFileName), PChar(vFilePath + '\' + FUpdateFiles[i].RelativePath + FUpdateFiles[i].FileName), False) then  // 备份文件
      ShowMessage('备份文件' + vFileName + '失败，请检查是否占用！');
    pb.Position := pb.Position + 1;
  end;
end;

procedure TfrmUpdate.DeleteBackupFilesBeforeDays(const ADays: Integer);
var
  vDirectories: TStringDynArray;
  i: Integer;
  vFileName: string;
begin
  FormatSettings.DateSeparator := '-';
  vDirectories := TDirectory.GetDirectories(ExtractFilePath(ParamStr(0)) + 'backup\');
  for i := 0 to Length(vDirectories) - 1 do
  begin
    vFileName := ExtractFileName(vDirectories[i]);
    if DaysBetween(StrToDateTime(vFileName), Now) > ADays then  // 超过指定时间则删除备份文件夹
      TDirectory.Delete(vDirectories[i]);
  end;
end;

function TfrmUpdate.DownLoadUpdateFiles: Boolean;
var
  i: Integer;
  vFileStream: TFileStream;
  vRelativeFilePath, vDecisiveFilePath: string;  // 相对文件路径
begin
  Result := False;
  if FUpdateFiles.Count = 0 then Exit;  // 没有下载文件

  for i := 0 to FUpdateFiles.Count - 1 do  // 遍历更新文件
  begin
    vRelativeFilePath := FUpdateFiles[i].RelativePath + FUpdateFiles[i].FileName;  // 当前更新文件相对于Update.exe的路径
    sgdUpdateFiles.Row := GetRowByFileName(vRelativeFilePath);  // 更新文件在Grid所在行
    lblHint.Caption := '正在下载文件 ' + vRelativeFilePath;
    sgdUpdateFiles.Cells[3, sgdUpdateFiles.Row] := '正在下载...';
    Application.ProcessMessages;

    vDecisiveFilePath := ExtractFilePath(ParamStr(0)) + 'upgrade\' + vRelativeFilePath;  // 当前更新文件的绝对路径
    if not DirectoryExists(ExtractFilePath(vDecisiveFilePath)) then  // 创建当前更新文件夹
      CreateDirectory(PChar(ExtractFilePath(vDecisiveFilePath)), nil);

    vFileStream := TFileStream.Create(vDecisiveFilePath, fmCreate or fmOpenWrite);
    try
      if not FUpDownLoadClient.DownLoadFile(vRelativeFilePath, vFileStream,
        procedure(const AReciveSize, AFileSize: Integer)
        begin
          if AReciveSize <> AFileSize then  // 文件没有全部接收
            sgdUpdateFiles.Cells[3, sgdUpdateFiles.Row] := '正在下载...' + Round(AReciveSize / AFileSize * 100).ToString + '%'
          else  // 文件全部接收完
            sgdUpdateFiles.Cells[3, sgdUpdateFiles.Row] := '已下载，待更新...';

          Application.ProcessMessages;
        end)
      then
      begin
        sgdUpdateFiles.Cells[3, sgdUpdateFiles.Row] := '下载失败:  ' + FUpDownLoadClient.CurError;
        Exit;
      end;
      pb.Position := pb.Position + 1;
    finally
      vFileStream.Free;
    end;
  end;
  Result := True;
end;

procedure TfrmUpdate.FormCreate(Sender: TObject);
begin
  FUpdateFiles := TObjectList<TUpdateFile>.Create;
  FUpDownLoadClient := TUpDownLoadClient.Create(True);
  btnOk.Enabled := False;  // 初始确定按钮不可操作
end;

function TfrmUpdate.GetRowByFileName(const AFileName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to sgdUpdateFiles.RowCount - 1 do
  begin
    if sgdUpdateFiles.Cells[1, i] = AFileName then  // 找到文件名
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TfrmUpdate.GetUpdateFileIndex(const AFile: TUpdateFile): Integer;
var
  i: Integer;
begin
  Result := - 1;
  for i := 0 to FUpdateFiles.Count - 1 do
  begin
    if (FUpdateFiles[i].RelativePath = AFile.RelativePath)
      and (FUpdateFiles[i].FileName = AFile.FileName)
    then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TfrmUpdate.GetUpdateInfo(const AMinVersion: Integer);
begin
  FUpdateFiles.Clear;
  mmoUpdateExplain.Lines.Clear;
  BLLServerExec(
    procedure(const ABllServerReady: TBLLServerProxy)
    var
      vExecParam: TMsgPack;
    begin
      ABllServerReady.Cmd := BLL_GETUPDATEINFO;  // 获取要升级的文件
      vExecParam := ABllServerReady.ExecParam;
      vExecParam.S['MinVersion'] := AMinVersion.ToString;
      ABllServerReady.BackDataSet := True;  // 通知服务端执行返回数据集
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    var
      i, vIndex: Integer;
      vUpdateFile: TUpdateFile;
      vXml: IXMLDocument;
      vNode: IXMLNode;
    begin
      if not ABLLServer.MethodRunOk then
      begin
        ShowMessage(ABLLServer.MethodError);
        Exit;
      end;
      if AMemTable <> nil then  // 有返回数据
      begin
        vXml := TXMLDocument.Create(nil);
        AMemTable.First;
        while not AMemTable.Eof do  // 遍历每一版升级信息
        begin
          // 升级说明
          mmoUpdateExplain.Lines.Add(AMemTable.FieldByName('Version').AsString + '版本');
          mmoUpdateExplain.Lines.Add('  ' + AMemTable.FieldByName('Explain').AsString);
          // 合并升级文件
          vXml.LoadFromXML(AMemTable.FieldByName('Files').AsString);
          if vXml.DocumentElement.LocalName = 'xml' then
          begin
            for i := 0 to vXml.DocumentElement.ChildNodes.Count - 1 do  // 遍历升级文件并合并
            begin
              vNode := vXml.DocumentElement.ChildNodes[i];
              vUpdateFile := TUpdateFile.Create(vNode.Text,  // 升级文件信息
                vNode.Attributes['Path'], vNode.Attributes['Version'],
                vNode.Attributes['Hash'], vNode.Attributes['Size'],
                AMemTable.FieldByName('id').AsInteger,
                AMemTable.FieldByName('Enforce').AsBoolean);

              vIndex := GetUpdateFileIndex(vUpdateFile);  // 是否在升级文件列表中已经存在
              if vIndex < 0 then  // 不存在则添加
                FUpdateFiles.Add(vUpdateFile)
              else  // 存在则更新
              if vUpdateFile.VerID > FUpdateFiles[vIndex].VerID then  // 版本最新
                FUpdateFiles[vIndex] := vUpdateFile;
            end;
          end;

          AMemTable.Next;
        end;
      end;
    end);
end;

function TfrmUpdate.ReplaceUpdateFiles: Boolean;
var
  i, vRow: Integer;
  vUpdateFile, vReplaceFile: string;
begin
  Result := True;
  for i := 0 to FUpdateFiles.Count - 1 do
  begin
    vUpdateFile := ExtractFilePath(ParamStr(0)) + 'upgrade\' + FUpdateFiles[i].RelativePath +
      FUpdateFiles[i].FileName;
    if not FileExists(vUpdateFile) then  // 更新文件不存在则不替换
    begin
      Result := False;
      ShowMessage('文件' + vUpdateFile + '丢失，请重启运行升级程序！');
      Break
    end;

    vReplaceFile := ExtractFilePath(ParamStr(0)) + FUpdateFiles[i].RelativePath + FUpdateFiles[i].FileName;  // 要替换的文件
    if not DirectoryExists(ExtractFilePath(vReplaceFile)) then  // 不存在要替换的文件夹则创建
      CreateDirectory(PChar(ExtractFilePath(vReplaceFile)), nil);

    vRow := GetRowByFileName(FUpdateFiles[i].RelativePath + FUpdateFiles[i].FileName);  // 获取指定文件名在Gird所在的行
    sgdUpdateFiles.Row := vRow;  // 选中正在更新的行
    sgdUpdateFiles.Cells[3, vRow] := '正在更新...';
    lblHint.Caption := '正在更新文件 ' + FUpdateFiles[i].RelativePath + FUpdateFiles[i].FileName;

    Application.ProcessMessages;
    if CopyFile(PChar(vUpdateFile), PChar(vReplaceFile), False) then  // 替换文件
    begin
      sgdUpdateFiles.Cells[3, vRow] := '已更新';
      pb.Position := pb.Position + 1;
    end
    else
    begin
      Result := False;
      sgdUpdateFiles.Cells[3, vRow] := '替换文件' + vUpdateFile + '失败，系统错误：' + Winapi.Windows.GetLastError.ToString;
      //ShowMessage('替换文件' + vUpdateFile + '失败，请检查是否占用！');
    end;
  end;
end;

procedure TfrmUpdate.ShowUpdateFiles;
var
  i: Integer;
begin
  for i := 0 to FUpdateFiles.Count - 1 do  // 遍历更新文件
  begin
    sgdUpdateFiles.RowCount := sgdUpdateFiles.RowCount + 1;
    sgdUpdateFiles.Cells[0, sgdUpdateFiles.RowCount - 1] := (i + 1).ToString;  // 序号
    sgdUpdateFiles.Cells[1, sgdUpdateFiles.RowCount - 1] := FUpdateFiles[i].RelativePath +  // 文件路径 + 文件名
      FUpdateFiles[i].FileName;
    sgdUpdateFiles.Cells[2, sgdUpdateFiles.RowCount - 1] := FormatSize('0.00', FUpdateFiles[i].Size);  // 文件大小
    sgdUpdateFiles.Cells[3, sgdUpdateFiles.RowCount - 1] := '';  // 初始状态
  end;
end;

procedure TfrmUpdate.UpdateVersion(const AVersionID: Integer);
begin
  dm.SetParam(PARAM_LOCAL_VERSIONID, IntToStr(AVersionID));
end;

end.
