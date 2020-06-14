unit frm_ImportRecord;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, HCEmrView, HCEmrViewLite;

type
  TfrmImportRecord = class(TForm)
    pnl1: TPanel;
    btnImportAll: TButton;
    btnImportSelect: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnImportAllClick(Sender: TObject);
    procedure btnImportSelectClick(Sender: TObject);
  private
    { Private declarations }
    FEmrViewLite: THCEmrView;
    FOnImportAsText: THCImportAsTextEvent;
  public
    { Public declarations }
    property EmrView: THCEmrView read FEmrViewLite;
    property OnImportAsText: THCImportAsTextEvent read FOnImportAsText write FOnImportAsText;
  end;

var
  frmImportRecord: TfrmImportRecord;

implementation

{$R *.dfm}

procedure TfrmImportRecord.btnImportAllClick(Sender: TObject);
begin
  if Assigned(FOnImportAsText) then
    FOnImportAsText(FEmrViewLite.SaveToText);
end;

procedure TfrmImportRecord.btnImportSelectClick(Sender: TObject);
begin
  if Assigned(FOnImportAsText) then
    FOnImportAsText(FEmrViewLite.ActiveSection.ActiveData.GetSelectText);
end;

procedure TfrmImportRecord.FormCreate(Sender: TObject);
begin
  FEmrViewLite := THCEmrView.Create(Self);
  FEmrViewLite.Align := alClient;
  FEmrViewLite.Parent := Self;
end;

procedure TfrmImportRecord.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEmrViewLite);
end;

end.
