unit frm_ScriptIDE;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, HCCompiler,
  frm_Script, SynEdit, Vcl.ExtCtrls, Vcl.StdCtrls, SynCompletionProposal;

type
  TfrmScriptIDE = class(TForm)
    pnl1: TPanel;
    btnSave: TButton;
    btnCompile: TButton;
    pnlMessage: TPanel;
    pnl2: TPanel;
    lblMsg: TLabel;
    lstMessage: TListBox;
    spl2: TSplitter;
    lbl1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCompileClick(Sender: TObject);
    procedure lstMessageDblClick(Sender: TObject);
  private
    { Private declarations }
    //FCompiler: THCCompiler;  // 编译器
    FFrmScript: TfrmScript;  // 代码书写窗体
    FLastChange: Boolean;  // 代码变动刻度
    FOnSave,  // 保存脚本
    FOnCompile,  // 编译
    FOnCompilePreview  // 预编译为代码提示服务
      : TNotifyEvent;
    FOnProposal: TProposalEvent;
    function GetOnCodeCompletion: TCodeCompletionEvent;
    procedure SetOnCodeCompletion(const Value: TCodeCompletionEvent);
    procedure SetScript(const AScript: string);
    function GetScript: string;
    procedure DoSynEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoSynEditChange(Sender: TObject);
    procedure DoProposal(const AWord: string; const AInsertList, AItemList: TStrings);
    procedure SetErrorLineNo(const ALine: Integer);
  public
    { Public declarations }
    procedure ClearDebugInfo;
    procedure SetDebugCaption(const AText: string);
    procedure AddWarning(const ALineNo: Integer; const AWarning: string);
    procedure AddError(const ALineNo: Integer; const AError: string);
    property Script: string read GetScript write SetScript;
//    property Compiler: THCCompiler read FCompiler;
    property OnProposal: TProposalEvent read FOnProposal write FOnProposal;
    property OnCodeCompletion: TCodeCompletionEvent read GetOnCodeCompletion write SetOnCodeCompletion;
    property OnSave: TNotifyEvent read FOnSave write FOnSave;
    property OnCompile: TNotifyEvent read FOnCompile write FOnCompile;
    property OnCompilePreview: TNotifyEvent read FOnCompilePreview write FOnCompilePreview;
  end;

implementation

type
  TDebugInfo = class(TObject)
  public
    Line: Integer;
  end;

{$R *.dfm}

procedure TfrmScriptIDE.btnSaveClick(Sender: TObject);
begin
  if Assigned(FOnSave) then
    FOnSave(Self)
  else
    Self.ModalResult := mrOk;
end;

procedure TfrmScriptIDE.ClearDebugInfo;
var
  i: Integer;
begin
  for i := 0 to lstMessage.Items.Count - 1 do
    TDebugInfo(lstMessage.Items.Objects[i]).Free;

  lstMessage.Clear;
end;

procedure TfrmScriptIDE.DoProposal(const AWord: string; const AInsertList,
  AItemList: TStrings);
begin
  if Assigned(FOnCompilePreview) then  // 预编译为代码提示服务
  begin
    if FLastChange then  // 上次预编译完有变动
    begin
      FLastChange := False;

      try
        FOnCompilePreview(Self);
      except
      end;
    end;
  end;

  if Assigned(FOnProposal) then
    FOnProposal(AWord, AInsertList, AItemList);
end;

procedure TfrmScriptIDE.DoSynEditChange(Sender: TObject);
begin
  FLastChange := True;
end;

procedure TfrmScriptIDE.DoSynEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Ord('S')) then
    btnSaveClick(Sender);
end;

procedure TfrmScriptIDE.AddError(const ALineNo: Integer; const AError: string);
var
  vDebugInfo: TDebugInfo;
begin
  vDebugInfo := TDebugInfo.Create;
  vDebugInfo.Line := ALineNo;
  lstMessage.AddItem(AError, vDebugInfo);
end;

procedure TfrmScriptIDE.AddWarning(const ALineNo: Integer;
  const AWarning: string);
var
  vDebugInfo: TDebugInfo;
begin
  vDebugInfo := TDebugInfo.Create;
  vDebugInfo.Line := ALineNo;
  lstMessage.AddItem(AWarning, vDebugInfo);
end;

procedure TfrmScriptIDE.btnCompileClick(Sender: TObject);
begin
  if Assigned(FOnCompile) then
    FOnCompile(Self);
end;

procedure TfrmScriptIDE.FormCreate(Sender: TObject);
begin
  FLastChange := False;
//  FCompiler := THCCompiler.CreateByScriptType(nil);

  FFrmScript := TfrmScript.Create(nil);
  FFrmScript.SynEdit.OnKeyDown := DoSynEditKeyDown;
  FFrmScript.SynEdit.OnChange := DoSynEditChange;
  FFrmScript.BorderStyle := bsNone;
  FFrmScript.Align := alClient;
  FFrmScript.Parent := Self;
  FFrmScript.OnProposal := DoProposal;
  FFrmScript.Show;
end;

procedure TfrmScriptIDE.FormDestroy(Sender: TObject);
begin
  ClearDebugInfo;
  FreeAndNil(FFrmscript);
//  FreeAndNil(FCompiler);
end;

function TfrmScriptIDE.GetOnCodeCompletion: TCodeCompletionEvent;
begin
  Result := FFrmScript.OnCodeCompletion;
end;

function TfrmScriptIDE.GetScript: string;
begin
  Result := FFrmScript.SynEdit.Text;
end;

procedure TfrmScriptIDE.lstMessageDblClick(Sender: TObject);
begin
  if lstMessage.Items.Objects[lstMessage.ItemIndex] <> nil then
    SetErrorLineNo(TDebugInfo(lstMessage.Items.Objects[lstMessage.ItemIndex]).Line + 1);
end;

procedure TfrmScriptIDE.SetDebugCaption(const AText: string);
begin
  lblMsg.Caption := AText;
end;

procedure TfrmScriptIDE.SetErrorLineNo(const ALine: Integer);
begin
  FFrmScript.SynEdit.SetErrorLine(ALine);
end;

procedure TfrmScriptIDE.SetOnCodeCompletion(const Value: TCodeCompletionEvent);
begin
  FFrmScript.OnCodeCompletion := Value;
end;

procedure TfrmScriptIDE.SetScript(const AScript: string);
begin
  FFrmScript.SynEdit.Text := AScript;
end;

end.
