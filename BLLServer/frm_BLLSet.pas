unit frm_BLLSet;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.ExtCtrls, Vcl.StdCtrls,
  emr_DataBase, frm_ScriptIDE, BLLCompiler, Vcl.Menus, HCCompiler;

type
  TfrmBLLSet = class(TForm)
    sgdbll: TStringGrid;
    spl1: TSplitter;
    pnlScript: TPanel;
    pnl2: TPanel;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    lbl6: TLabel;
    edtBLLID: TEdit;
    edtName: TEdit;
    cbbDB: TComboBox;
    edtVer: TEdit;
    mmoSQL: TMemo;
    btnSave: TButton;
    pmBLL: TPopupMenu;
    mniNewBLL: TMenuItem;
    mniDelBLL: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sgdbllSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure mniNewBLLClick(Sender: TObject);
  private
    { Private declarations }
    FNewBLL: Boolean;
    FDB: TDataBase;
    FCompiler: TBLLCompiler;
    FFrmScriptIDE: TfrmScriptIDE;
    procedure DoScriptSave(Sender: TObject);
    procedure DoScriptCompile(Sender: TObject);
    procedure DoScriptCompilePreview(Sender: TObject);
    procedure GetBLLData;
    procedure GetBLLInfoByID(const AID: string);
  public
    { Public declarations }
  end;

implementation

uses
  BLLServerParam, FireDAC.Comp.Client, FireDAC.VCLUI.Wait, Data.DB;

{$R *.dfm}

type
  TConnInfo = class(TObject)
  public
    ConID: Integer;
    ConName: string;
  end;

procedure TfrmBLLSet.DoScriptCompile(Sender: TObject);
var
  i: Integer;
begin
  FFrmScriptIDE.ClearDebugInfo;

  FCompiler.ResetRegister;
  FCompiler.RegClassVariable(nil, nil);
  if not FCompiler.CompileScript(FFrmScriptIDE.Script) then
  begin
    FFrmScriptIDE.SetDebugCaption('Messages ' + 'Error(' + IntToStr(FCompiler.ErrorCount) + ')'
      + ' Warning(' + IntToStr(FCompiler.WarningCount) + ')');

    for i := 0 to FCompiler.WarningCount - 1 do
      FFrmScriptIDE.AddWarning(FCompiler.WarningLineNumber[i],
        'Warning[' + IntToStr(FCompiler.WarningLineNumber[i] + 1) + ']' + FCompiler.WarningMessage[i]);

    for i := 0 to FCompiler.ErrorCount - 1 do
      FFrmScriptIDE.AddError(FCompiler.ErrorLineNumber[i],
        'Error[' + IntToStr(FCompiler.ErrorLineNumber[i] + 1) + ']' + FCompiler.ErrorMessage[i]);
  end
  else
  begin
    FFrmScriptIDE.SetDebugCaption('Messages ' + 'Error(0)'
      + ' Warning(' + IntToStr(FCompiler.WarningCount) + ')');

    for i := 0 to FCompiler.WarningCount - 1 do
      FFrmScriptIDE.AddWarning(FCompiler.WarningLineNumber[i],
        'Warning[' + IntToStr(FCompiler.WarningLineNumber[i] + 1) + ']' + FCompiler.WarningMessage[i]);
  end;
end;

procedure TfrmBLLSet.DoScriptCompilePreview(Sender: TObject);
begin
  FCompiler.ResetRegister;
  FCompiler.RegClassVariable(nil, nil);
  FCompiler.CompileScript(FFrmScriptIDE.Script);
end;

procedure TfrmBLLSet.DoScriptSave(Sender: TObject);
var
  vQuery: TFDQuery;
  vStream: TMemoryStream;
  vSaveBin: Boolean;
begin
  vSaveBin := True;

  DoScriptCompile(Sender);
  if FCompiler.ErrorCount > 0 then
  begin
    vSaveBin := False;
    if MessageDlg('脚本编译有错误，确定保存？', mtWarning, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  end;

  vQuery := FDB.GetQuery;
  try
    vQuery.SQL.Text := Format('UPDATE frame_bllsql SET script = %s, scriptbin = :scriptbin WHERE id = %d',
      [QuotedStr(FFrmScriptIDE.Script), StrToInt(sgdbll.Cells[0, sgdbll.Row])]);

    vStream := TMemoryStream.Create;
    try
      if vSaveBin then
        FCompiler.SaveToStream(vStream);

      vStream.Position := 0;
      vQuery.ParamByName('scriptbin').LoadFromStream(vStream, TFieldType.ftBlob);
      vQuery.ExecSQL;
    finally
      FreeAndNil(vStream);
    end;
  finally
    FreeAndNil(vQuery);
  end;

  if vSaveBin then
    ShowMessage('脚本编译并保存成功！')
  else
    ShowMessage('脚本保存成功，但编译失败！')
end;

procedure TfrmBLLSet.FormCreate(Sender: TObject);
begin
  FCompiler := TBLLCompiler.CreateByScriptType(nil);
  FCompiler.ResetRegister;
  FCompiler.RegClassVariable(nil, nil);

  FFrmScriptIDE := TfrmScriptIDE.Create(nil);
  FFrmScriptIDE.BorderStyle := bsNone;
  FFrmScriptIDE.Align := alClient;
  FFrmScriptIDE.Parent := pnlScript;
  FFrmScriptIDE.OnProposal := FCompiler.Proposal;
  FFrmScriptIDE.OnCodeCompletion := FCompiler.CodeCompletion;
  FFrmScriptIDE.OnSave := DoScriptSave;
  FFrmScriptIDE.OnCompile := DoScriptCompile;
  FFrmScriptIDE.OnCompilePreview := DoScriptCompilePreview;
  FFrmScriptIDE.Show;

  FDB := TDataBase.Create(nil);
  FDB.DBType := dbSqlServer;
  FDB.Server := BLLServerParams.DataBaseServer;
  FDB.DBName := BLLServerParams.DataBaseName;
  FDB.Username := BLLServerParams.DataBaseUsername;
  FDB.Password := BLLServerParams.DataBasePassword;
  FDB.Connect;
end;

procedure TfrmBLLSet.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to cbbDB.Items.Count - 1 do
  begin
    if cbbDB.Items.Objects[i] <> nil then
      TConnInfo(cbbDB.Items.Objects[i]).Free;
  end;

  FreeAndNil(FDB);
  FreeAndNil(FFrmScriptIDE);
  FreeAndNil(FCompiler);
end;

procedure TfrmBLLSet.FormShow(Sender: TObject);
begin
  sgdbll.Cells[0, 0] := '序';
  sgdbll.Cells[1, 0] := '业务ID';
  sgdbll.Cells[2, 0] := '名称';
  sgdbll.Cells[3, 0] := '版本';
  sgdbll.Cells[4, 0] := '数据源';

  GetBLLData;
end;

procedure TfrmBLLSet.GetBLLData;
var
  vQuery: TFDQuery;
  vConnInfo: TConnInfo;
  vRow: Integer;
begin
  vQuery := FDB.GetQuery;
  try
    vQuery.Open('SELECT BLL.id, BLL.bllid, BLL.name, BLL.dbconnid, Conn.conname, BLL.ver '
      + 'FROM frame_bllsql AS BLL '
      + 'LEFT JOIN frame_blldbconn AS Conn ON BLL.dbconnid = Conn.id');

    sgdbll.RowCount := vQuery.RecordCount + 1;
    if sgdbll.RowCount > 1 then
      sgdbll.FixedRows := 1;

    vRow := 1;
    with vQuery do
    begin
      First;

      while not Eof do
      begin
        sgdbll.Cells[0, vRow] := FieldByName('id').AsString;
        sgdbll.Cells[1, vRow] := FieldByName('bllid').AsString;
        sgdbll.Cells[2, vRow] := FieldByName('name').AsString;
        sgdbll.Cells[3, vRow] := FieldByName('ver').AsString;

        if FieldByName('dbconnid').AsInteger > 0 then
        begin
          vConnInfo := TConnInfo.Create;
          vConnInfo.ConID := FieldByName('dbconnid').AsInteger;
          vConnInfo.ConName := FieldByName('conname').AsString;
          cbbDB.Items.AddObject(vConnInfo.ConName, vConnInfo);

          sgdbll.Cells[4, vRow] := vConnInfo.ConName;
        end
        else
          sgdbll.Cells[4, vRow] := '默认';

        Inc(vRow);
        Next;
      end;
    end;
  finally
    FreeAndNil(vQuery);
  end;

  if sgdbll.RowCount > 1 then
    GetBLLInfoByID(sgdbll.Cells[0, 1]);
end;

procedure TfrmBLLSet.GetBLLInfoByID(const AID: string);

  function GetDBConnIndex(const AConID: Integer): Integer;
  var
    i: Integer;
  begin
    for i := 0 to cbbDB.Items.Count - 1 do
    begin
      if cbbDB.Items.Objects[i] <> nil then
      begin
        if TConnInfo(cbbDB.Items.Objects[i]).ConID = AConID then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;

var
  vQuery: TFDQuery;
begin
  FFrmScriptIDE.ClearDebugInfo;

  if AID = '' then
  begin
    edtBLLID.Clear;
    edtName.Clear;
    cbbDB.ItemIndex := 0;
    edtVer.Clear;
    mmoSQL.Clear;
    FFrmScriptIDE.Script := '';

    Exit;
  end;

  vQuery := FDB.GetQuery;
  try
    vQuery.Open('SELECT BLL.id, BLL.bllid, BLL.name, BLL.dbconnid, Conn.conname, BLL.sqltext, BLL.script, BLL.ver '
      + 'FROM frame_bllsql AS BLL '
      + 'LEFT JOIN frame_blldbconn AS Conn ON BLL.dbconnid = Conn.id '
      + 'WHERE BLL.id = ' + AID);

    if vQuery.RecordCount = 1 then
    begin
      edtBLLID.Text := vQuery.FieldByName('bllid').AsString;
      edtName.Text := vQuery.FieldByName('name').AsString;

      if vQuery.FieldByName('dbconnid').AsInteger > 0 then
        cbbDB.ItemIndex := GetDBConnIndex(vQuery.FieldByName('dbconnid').AsInteger)
      else
        cbbDB.ItemIndex := 0;

      edtVer.Text := vQuery.FieldByName('ver').AsString;
      mmoSQL.Text := vQuery.FieldByName('sqltext').AsString;
      FFrmScriptIDE.Script := vQuery.FieldByName('script').AsString;
    end
    else
    begin
      edtBLLID.Clear;
      edtName.Clear;
      cbbDB.ItemIndex := 0;
      edtVer.Clear;
      mmoSQL.Clear;
      FFrmScriptIDE.Script := '';
    end;
  finally
    FreeAndNil(vQuery);
  end;
end;

procedure TfrmBLLSet.mniNewBLLClick(Sender: TObject);
begin
  FNewBLL := True;
end;

procedure TfrmBLLSet.sgdbllSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  FNewBLL := False;

  if (ARow > 0) and (sgdbll.Cells[1, ARow] <> edtBLLID.Text) then
    GetBLLInfoByID(sgdbll.Cells[0, ARow]);
end;

end.
