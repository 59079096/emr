unit frm_ScriptIDE;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, HCCompiler,
  frm_Script, SynEdit, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfrmScriptIDE = class(TForm)
    pnl1: TPanel;
    btnSave: TButton;
    btnCompile: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCompileClick(Sender: TObject);
  private
    { Private declarations }
    FCompiler: THCCompiler;  // ±‡“Î∆˜
    FFrmScript: TfrmScript;  // ¥˙¬Î È–¥¥∞ÃÂ
    FOnSave, FOnCompile: TNotifyEvent;
    function GetOnProposal: TProposalEvent;
    procedure SetOnProposal(const Value: TProposalEvent);
    procedure SetScript(const AScript: string);
    function GetScript: string;
    procedure DoSynEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    { Public declarations }
    procedure SetErrorLine(const ALine: Integer);
    property Script: string read GetScript write SetScript;
    property Compiler: THCCompiler read FCompiler;
    property OnProposal: TProposalEvent read GetOnProposal write SetOnProposal;
    property OnSave: TNotifyEvent read FOnSave write FOnSave;
    property OnCompile: TNotifyEvent read FOnCompile write FOnCompile;
  end;

implementation

{$R *.dfm}

procedure TfrmScriptIDE.btnSaveClick(Sender: TObject);
begin
  if Assigned(FOnSave) then
    FOnSave(Self)
  else
    Self.ModalResult := mrOk;
end;

procedure TfrmScriptIDE.DoSynEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Ord('S')) then
    btnSaveClick(Sender);
end;

procedure TfrmScriptIDE.btnCompileClick(Sender: TObject);
begin
  if Assigned(FOnCompile) then
    FOnCompile(Self);
end;

procedure TfrmScriptIDE.FormCreate(Sender: TObject);
begin
  FCompiler := THCCompiler.CreateByScriptType(nil);

  FFrmScript := TfrmScript.Create(nil);
  FFrmScript.SynEdit.OnKeyDown := DoSynEditKeyDown;
  FFrmScript.BorderStyle := bsNone;
  FFrmScript.Align := alClient;
  FFrmScript.Parent := Self;
  FFrmScript.Show;
end;

procedure TfrmScriptIDE.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFrmscript);
  FreeAndNil(FCompiler);
end;

function TfrmScriptIDE.GetOnProposal: TProposalEvent;
begin
  Result := FFrmScript.OnProposal;
end;

function TfrmScriptIDE.GetScript: string;
begin
  Result := FFrmScript.SynEdit.Text;
end;

procedure TfrmScriptIDE.SetErrorLine(const ALine: Integer);
begin
  FFrmScript.SynEdit.SetErrorLine(ALine);
end;

procedure TfrmScriptIDE.SetOnProposal(const Value: TProposalEvent);
begin
  FFrmScript.OnProposal := Value;
end;

procedure TfrmScriptIDE.SetScript(const AScript: string);
begin
  FFrmScript.SynEdit.Text := AScript;
end;

end.
