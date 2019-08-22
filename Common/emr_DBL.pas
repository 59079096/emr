{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit emr_DBL;

interface

uses
  Winapi.Windows, Classes, SysUtils, DB, Provider, FireDAC.Comp.Client, FireDAC.Stan.Intf,
  FireDAC.Stan.StorageBin, emr_DataBase, emr_BLLDataBase, emr_MsgPack,
  BLLCompiler;

Type
  TExecutelog = procedure(const ALog: string) of object;

  TDBL = class(TObject)  // DataBase Logic
  private
    FDB: TDataBase;
    FBLLDB: TBLLDataBase;
    FMsgPack: TMsgPack;
    // 业务脚本需要的变量
    FBLLObj: TBLLObj;
    FScriptBin: TMemoryStream;
    FCompiler: TBLLCompiler;
    //
    FOnExecuteLog: TExecuteLog;
    procedure DoCompilerException(const E: Exception; const ModuleName: String; SourceLineNumber: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExecuteMsgPack;
    property DB: TDataBase read FDB;
    property MsgPack: TMsgPack read FMsgPack;
    property OnExecuteLog: TExecutelog read FOnExecuteLog write FOnExecuteLog;
  end;

implementation

uses
  emr_MsgConst;

{ TBLLServerMethod }

constructor TDBL.Create;
begin
  FDB := TDataBase.Create(nil);
  FBLLDB := TBLLDataBase.Create;
  FMsgPack := TMsgPack.Create;
  FBLLObj := TBLLObj.Create;
  FCompiler := TBLLCompiler.CreateByScriptType(nil);
  FCompiler.ResetRegister;
  FCompiler.RegClassVariable(@FMsgPack, @FBLLObj);
  FCompiler.OnException := DoCompilerException;

  FScriptBin := TMemoryStream.Create;
end;

destructor TDBL.Destroy;
begin
  FreeAndNil(FDB);
  FreeAndNil(FBLLDB);
  FreeAndNil(FMsgPack);
  FreeAndNil(FCompiler);
  FreeAndNil(FBLLObj);
  FreeAndNil(FScriptBin);
  inherited Destroy;
end;

procedure TDBL.DoCompilerException(const E: Exception; const ModuleName: String;
  SourceLineNumber: Integer);
begin
  FBLLObj.ErrorInfo := '脚本运行异常：' + E.Message
    + sLineBreak + '模块' + ModuleName + '，代码行：' + IntToStr(SourceLineNumber + 1) // + FCompiler.Modules[ModuleName][SourceLineNumber]
    + sLineBreak + '堆栈：' + E.StackTrace;
end;

procedure TDBL.ExecuteMsgPack;

  function IsSelectSql(const ASql: string): Boolean;
  begin
    Result := LowerCase(Copy(TrimLeft(ASql), 1, 6)) = 'select';
  end;

  function IsInsertSql(const ASql: string): Boolean;
  begin
    Result := LowerCase(Copy(TrimLeft(ASql), 1, 6)) = 'insert';
  end;

  procedure DoBackErrorMsg(const AMsg: string);
  begin
    FMsgPack.Clear;  // 将客户端调用时传来的参数值清除掉，减少不必要的回传数据量
    FMsgPack.S[BLL_METHODMSG] := AMsg;
    if Assigned(FOnExecuteLog) then
      FOnExecuteLog(AMsg);
  end;

var
  vQuery: TFDQuery;
  vBLLDataBase: TDataBase;
  vBLLDataBaseID: Integer;
  vFrameSql: string;

  function CheckBllDataBase: Boolean;
  begin
    Result := False;
    try
      if vBLLDataBaseID > 0 then
      begin
        vFrameSql := Format('SELECT dbtype, server, port, dbname, username, paw FROM frame_blldbconn WHERE id=%d',
          [vBLLDataBaseID]);
        vQuery.Close;
        vQuery.SQL.Text := vFrameSql;
        vQuery.Open;

        vBLLDataBase := FBLLDB.GetBLLDataBase(vBLLDataBaseID,
          vQuery.FieldByName('dbtype').AsInteger,
          vQuery.FieldByName('server').AsString,
          vQuery.FieldByName('port').AsInteger,
          vQuery.FieldByName('dbname').AsString,
          vQuery.FieldByName('username').AsString,
          vQuery.FieldByName('paw').AsString);
      end
      else
        vBLLDataBase := FDB;

      Result := True;
    except
      on E: Exception do
        DoBackErrorMsg(Format('异常(服务端)：没有找到ConnID为 %d 的业务数据连接信息', [vBLLDataBaseID])
          + sLineBreak + '语句：' + vFrameSql + sLineBreak + '错误信息：' + E.Message);
    end;
  end;

var
  //vData: OleVariant;
  vDeviceType: TDeviceType;
  i, j, vCMD, vVer, vRecordCount, vIDENTITY: Integer;
  vProvider: TDataSetProvider;
  vExecParams, vReplaceParams, vBatchData, vBackParam: TMsgPack;
  vBLLSql, vBLLScript, vBLLInfo: string;
  vMemStream: TMemoryStream;
  vMemTable: TFDMemTable;
  vBlobField: TBlobField;
  vTick: Cardinal;
  vScriptRunOk: Boolean;
begin
  FMsgPack.Result := False;

  vCMD := FMsgPack.ForcePathObject(BLL_CMD).AsInteger;
  vDeviceType := TDeviceType(FMsgPack.I[BLL_DEVICE]);
  vVer := FMsgPack.I[BLL_VER];
  vBLLScript := '';
  vBLLInfo := '[' + vCMD.ToString + ']';
  vQuery := FDB.GetQuery;
  try
    // 取业务语句并查询
    vFrameSql := Format('SELECT dbconnid, sqltext, name, script, scriptbin FROM frame_bllsql WHERE bllid = %d AND ver = %d',
      [vCMD, vVer]);

    vQuery.Close;
    vQuery.SQL.Text := vFrameSql;
    vQuery.Open;
    if vQuery.RecordCount = 1 then  // 查询到唯一
    begin
      try
        vBLLInfo := vBLLInfo + vQuery.FieldByName('name').AsString;  // 业务名称
        // 取处理该业务的数据库连接对象
        vBLLDataBaseID := vQuery.FieldByName('dbconnid').AsInteger;
        vBLLSql := vQuery.FieldByName('sqltext').AsString;
        vBLLScript := vQuery.FieldByName('script').AsString;

        if vBLLScript <> '' then
        begin
          vBlobField := vQuery.FieldByName('scriptbin') as TBlobField;
          FScriptBin.SetSize(vBlobField.BlobSize);
          FScriptBin.Position := 0;
          vBlobField.SaveToStream(FScriptBin);
          //vQuery.FieldByName('scriptbin').GetData(FScriptBin.Memory, False);
        end;

        if CheckBllDataBase then
        begin
          vFrameSql := '';
          vQuery.Close;
          vQuery.Connection := vBLLDataBase.Connection;

          vRecordCount := 0;
          vIDENTITY := 0;

          if vBLLScript <> '' then
          begin
            FBLLObj.DB := FDB;
            FBLLObj.BLLDB := vBLLDataBase;
            FBLLObj.BLLQuery := vQuery;
            FBLLObj.DebugInfoClear;

            vScriptRunOk := False;
            vTick := GetTickCount;
            // 防止脚本设置窗体打开后将变量设置为nil，如果不使用脚本设置窗体可以去下面2行，开 20190719001
            FCompiler.ResetRegister;
            FCompiler.RegClassVariable(@FMsgPack, @FBLLObj);

            try
              if FScriptBin.Size = 0 then
              //if True then
              begin
                // 20190719001
                //FCompiler.ResetRegister;
                //FCompiler.RegClassVariable(@FMsgPack, @FBLLObj);
                vScriptRunOk := FCompiler.RunScript(vBLLScript);
              end
              else
                vScriptRunOk := FCompiler.RunScriptBin(FScriptBin);
            except
              // 错误信息通过 DoCompilerException 传递给 BLLObj.ErrorInfo了
            end;

            if not vScriptRunOk then  // 脚本运行出错
            begin
              vBLLInfo := vBLLInfo + ' 失败！';
              if FBLLObj.ErrorInfo <> '' then
                vBLLInfo := vBLLInfo + sLineBreak + '异常：' + FBLLObj.ErrorInfo;

              if FCompiler.ErrorCount > 0 then
              begin
                vBLLInfo := vBLLInfo + sLineBreak + '业务脚本有 ' + IntToStr(FCompiler.ErrorCount) + ' 处错误：';
                for i := 0 to FCompiler.ErrorCount - 1 do
                begin
                  vBLLInfo := vBLLInfo + sLineBreak + '  '
                    + IntToStr(FCompiler.ErrorLineNumber[i] + 1) + '行：'
                    + FCompiler.ErrorMessage[i] + '[' + FCompiler.ErrorLine[i] + ']';
                end;
              end;

              DoBackErrorMsg(vBLLInfo);
            end
            else  // 脚本运行成功
            begin
              vTick := GetTickCount - vTick;

              if FScriptBin.Size = 0 then
                vBLLInfo := vBLLInfo + ' 成功(脚本)，用时：' + IntToStr(vTick) + 'ms'
              else
                vBLLInfo := vBLLInfo + ' 成功(Bin)，用时：' + IntToStr(vTick) + 'ms';

              if FBLLObj.DebugInfo.Count > 0 then
                vBLLInfo := vBLLInfo + sLineBreak + '调试信息：' + sLineBreak + FBLLObj.DebugInfo.Text;

              if Assigned(FOnExecuteLog) then  // 输出日志
                FOnExecuteLog(vBLLInfo);
            end;

            FMsgPack.ForcePathObject(BLL_EXECPARAM).Clear;  // 将客户端调用时传来的参数值清除掉，减少不必要的回传数据量
            FMsgPack.ForcePathObject(BLL_METHODRESULT).AsBoolean := vScriptRunOk  // 客户端调用是否成功
          end
          else
          {$REGION '执行SQL语句'}
          begin
            if FMsgPack.B[BLL_BATCH] then  // 批量处理
            begin
              vBatchData := FMsgPack.ForcePathObject(BLL_BATCHDATA);
              vMemStream := TMemoryStream.Create;
              try
                vMemTable := TFDMemTable.Create(nil);
                try
                  vBatchData.SaveBinaryToStream(vMemStream);
                  vMemStream.Position := 0;
                  vMemTable.LoadFromStream(vMemStream, TFDStorageFormat.sfBinary);

                  if vMemTable.RecordCount > 0 then  // 批量执行
                  begin
                    vQuery.SQL.Text := vBLLSql;
                    vQuery.Params.ArraySize := vMemTable.RecordCount;
                    for i := 0 to vMemTable.RecordCount - 1 do
                    begin
                      for j := 0 to vQuery.Params.Count - 1 do
                      begin
                        vQuery.Params[j].Values[i] :=
                          vMemTable.SourceView.Rows[i].GetData(vQuery.Params[j].Name);
                      end;
                    end;

                    if FMsgPack.B[BLL_TRANS] then  // 使用事务
                    begin
                      vQuery.Connection.StartTransaction;  // 开始一个事务
                      try
                        vQuery.Execute(vQuery.Params.ArraySize);
                        vQuery.Connection.Commit;  // 提交操作
                      except
                        on E: Exception do
                        begin
                          vQuery.Connection.Rollback;  // 出错回滚
                          DoBackErrorMsg('异常回滚(服务端)：执行方法 ' + vBLLInfo
                            + sLineBreak + '语句：' + vBLLSql + sLineBreak + '错误信息：' + E.Message);
                          Exit;
                        end;
                      end;
                    end
                    else  // 不使用事务
                      vQuery.Execute(vQuery.Params.ArraySize);

                    if Assigned(FOnExecuteLog) then
                    begin
                      FOnExecuteLog(vBLLInfo + sLineBreak + '语句：' + vBLLSql + sLineBreak + '批量处理'
                        + vQuery.RowsAffected.ToString + '条数据');
                    end;
                  end;
                finally
                  FreeAndNil(vMemTable);
                end;
              finally
                FreeAndNil(vMemStream);
              end;
            end
            else  // 单条处理
            begin
              // 处理Sql语句中的替换参数
              vReplaceParams := FMsgPack.ForcePathObject(BLL_REPLACEPARAM);
              for i := 0 to vReplaceParams.Count - 1 do
                vBLLSql := StringReplace(vBLLSql, '{' + vReplaceParams[i].NameEx + '}', vReplaceParams[i].AsString, [rfIgnoreCase]);

              // 处理Sql语句中的字段参数
              vQuery.SQL.Text := vBLLSql;
              if vQuery.Params.Count > 0 then  // 有字段参数
              begin
                vExecParams := FMsgPack.ForcePathObject(BLL_EXECPARAM);
                for i := 0 to vQuery.Params.Count - 1 do
                begin
                  case vExecParams.ForcePathObject(vQuery.Params[i].Name).DataType of
                    mptString, mptInteger, mptBoolean, mptDouble, mptSingle:
                      vFrameSql := vFrameSql + sLineBreak + vQuery.Params[i].Name
                        + ' = ' + vExecParams.ForcePathObject(vQuery.Params[i].Name).AsString;

                    mptDateTime:
                      vFrameSql := vFrameSql + sLineBreak + vQuery.Params[i].Name + ' = '
                        + FormatDateTime('YYYY-MM-DD HH:mm:ss', vExecParams.ForcePathObject(vQuery.Params[i].Name).AsDateTime);

                    mptBinary:
                      vFrameSql := vFrameSql + sLineBreak + vQuery.Params[i].Name + ' = [二进制]';
                  else
                    vFrameSql := vFrameSql + sLineBreak + vQuery.Params[i].Name + ' = [不正确的参数值(空、未知)]';
                  end;

                  vQuery.Params[i].Value := vExecParams.ForcePathObject(vQuery.Params[i].Name).AsVariant;
                end;
              end;

              if Assigned(FOnExecuteLog) then
              begin
                if vFrameSql <> '' then
                  FOnExecuteLog(vBLLInfo + sLineBreak + '语句：' + vBLLSql + sLineBreak + '参数：' + vFrameSql)
                else
                  FOnExecuteLog(vBLLInfo + sLineBreak + '语句：' + vBLLSql);
              end;

              if IsSelectSql(vBLLSql)
                or FMsgPack.B[BLL_BACKDATASET]
                or (FMsgPack.O[BLL_BACKFIELD] <> nil)
              then  // 查询类
              begin
                vQuery.Open;
                vRecordCount := vQuery.RecordCount;
              end
              else  // 操作类
              begin
                if FMsgPack.B[BLL_TRANS] then  // 使用事务
                begin
                  vQuery.Connection.StartTransaction;  // 开始一个事务
                  try
                    vQuery.ExecSQL;
                    vQuery.Connection.Commit;  // 提交操作
                  except
                    on E: Exception do
                    begin
                      vQuery.Connection.Rollback;  // 出错回滚
                      DoBackErrorMsg('异常回滚(服务端)：执行方法 ' + vBLLInfo
                        + sLineBreak + '语句：' + vBLLSql + sLineBreak + '参数：' + vFrameSql + sLineBreak + '错误信息：' + E.Message);
                      Exit;
                    end;
                  end;
                end
                else
                  vQuery.ExecSQL;

                vRecordCount := vQuery.RowsAffected;

                if (vBLLDataBase.DBType = TDBType.dbSqlServer) and IsInsertSql(vBLLSql) then
                begin
                  vQuery.Close;
                  vQuery.SQL.Clear;
                  vQuery.SQL.Text := 'SELECT SCOPE_IDENTITY() AS id';
                  vQuery.Open();
                  if not vQuery.IsEmpty then
                    vIDENTITY := vQuery.FieldByName('id').AsInteger;
                end;
              end;
            end;

            // 处理客户端需要返回的数据集或指定字段
            if FMsgPack.B[BLL_BACKDATASET] then  // 客户端需要返回数据集
            begin
              vMemStream := TMemoryStream.Create;
              try
                vQuery.SaveToStream(vMemStream, TFDStorageFormat.sfBinary);
                FMsgPack.ForcePathObject(BLL_DATASET).LoadBinaryFromStream(vMemStream);
              finally
                FreeAndNil(vMemStream);
              end;
            end
            else
            if (FMsgPack.O[BLL_BACKFIELD] <> nil) and (vRecordCount > 0) then  // 客户端需要返回指定字段
            begin
              vBackParam := FMsgPack.ForcePathObject(BLL_BACKFIELD);
              for i := 0 to vBackParam.Count - 1 do
                vBackParam.Items[i].AsVariant := vQuery.FieldByName(vBackParam.Items[i].NameLower).AsVariant;
            end;

            { 返回语句执行结果和数据 }
            // 先返回协议定义好的
            FMsgPack.ForcePathObject(BLL_EXECPARAM).Clear;  // 将客户端调用时传来的参数值清除掉，减少不必要的回传数据量
            FMsgPack.ForcePathObject(BLL_METHODRESULT).AsBoolean := True;  // 客户端调用成功
            FMsgPack.ForcePathObject(BLL_RECORDCOUNT).AsInteger := vRecordCount;
            if vIDENTITY > 0 then
              FMsgPack.ForcePathObject(BLL_INSERTINDENT).AsInteger := vIDENTITY;
          end;
          {$ENDREGION}
        end;
      except
        on E: Exception do
        begin
          if vBLLScript <> '' then
            DoBackErrorMsg('异常(服务端)：执行脚本错误：'
              + sLineBreak + '    ' + vBLLInfo + '，' + E.Message)
          else
            DoBackErrorMsg('异常(服务端)：执行方法 ' + vBLLInfo
              + sLineBreak + '语句：' + vBLLSql + sLineBreak + '参数：' + vFrameSql + sLineBreak + '错误信息：' + E.Message);
        end;
      end;
    end
    else  // 没找到业务对应的语句
      DoBackErrorMsg('(服务端)业务' + vBLLInfo + '对应执行语句不存在或有多条'
        + sLineBreak + '版本：' + vVer.ToString);
  finally
    vQuery.Free;
  end;

  FMsgPack.Result := True;
end;

end.
