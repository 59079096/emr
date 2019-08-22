unit frm_RecordSet;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CFControl, CFSplitter,
  Vcl.ComCtrls, frm_Record, CFGrid, Vcl.ExtCtrls, Vcl.StdCtrls, HCParaStyle;

type
  TfrmRecordSet = class(TForm)
    spl1: TCFSplitter;
    pnl1: TPanel;
    sgdRecord: TCFGrid;
    pnl2: TPanel;
    btnShow: TButton;
    lbl1: TLabel;
    edtPageNo: TEdit;
    chkPageBlankTip: TCheckBox;
    edtPageBlankTip: TEdit;
    lbl2: TLabel;
    edtPageNoFmt: TEdit;
    chkShowTrace: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
  private
    { Private declarations }
    FRecord: TfrmRecord;
    procedure GetPatientInchRecord(const APatID: string; const AVisit: Integer);
  public
    { Public declarations }
    procedure ShowDialog(const APatID: string; const AVisit: Integer);
  end;

var
  frmRecordSet: TfrmRecordSet;

implementation

uses
  emr_BLLInvoke, FireDAC.Comp.Client;

{$R *.dfm}

{ TfrmRecordSet }

procedure TfrmRecordSet.btnShowClick(Sender: TObject);
var
  i, vRecID: Integer;
  vFirst: Boolean;
  vStream: TMemoryStream;
begin
  FRecord.EmrView.Clear;
  FRecord.EmrView.PageNoFormat := edtPageNoFmt.Text;
  FRecord.EmrView.HideTrace := not chkShowTrace.Checked;
  if chkPageBlankTip.Checked then
    FRecord.EmrView.PageBlankTip := edtPageBlankTip.Text
  else
    FRecord.EmrView.PageBlankTip := '';

  vFirst := True;
  vStream := TMemoryStream.Create;
  try
    for i := 0 to sgdRecord.RowCount - 1 do
    begin
      if sgdRecord.Rows[i][0].AsBoolean then
      begin
        vStream.Clear;
        vRecID := sgdRecord.Rows[i][4].AsInteger;
        TBLLInvoke.GetRecordContent(vRecID, vStream);
        vStream.Position := 0;
        if vFirst then
        begin
          FRecord.EmrView.LoadFromStream(vStream);
          FRecord.EmrView.Sections[0].PageNoFrom := StrToInt(edtPageNo.Text);
          vFirst := False;
        end
        else
        begin
          FRecord.EmrView.ActiveSection.ActiveData.SelectLastItemAfterWithCaret;
          if sgdRecord.Rows[i][1].AsBoolean then  // 另起页
            FRecord.EmrView.InsertPageBreak
          else
            FRecord.EmrView.InsertBreak;

          FRecord.EmrView.ApplyParaAlignHorz(TParaAlignHorz.pahLeft);
          FRecord.EmrView.InsertStream(vStream);
        end;
      end;
    end;
  finally
    vStream.Free;
  end;
end;

procedure TfrmRecordSet.FormCreate(Sender: TObject);
begin
  FRecord := TfrmRecord.Create(Self);
  FRecord.PrintToolVisible := True;
  FRecord.Align := alClient;
  FRecord.Parent := Self;
  FRecord.Show;

  edtPageNoFmt.Text := FRecord.EmrView.PageNoFormat;

  sgdRecord.VScrollBar.Width := 20;
  sgdRecord.HScrollBar.Height := 20;
  sgdRecord.DefaultColWidth := 10;
  sgdRecord.ColWidth[0] := 30;
  sgdRecord.ColStyle[0] := TCFColStyle.ccsCheckBox;

  sgdRecord.ColWidth[1] := 50;
  sgdRecord.ColStyle[1] := TCFColStyle.ccsCheckBox;

  sgdRecord.ColWidth[2] := 100;
  sgdRecord.ColWidth[3] := 80;
  sgdRecord.ColWidth[4] := 30;

  sgdRecord.TitleText[0] := '选择';
  sgdRecord.TitleText[1] := '另起页';
  sgdRecord.TitleText[2] := '名称';
  sgdRecord.TitleText[3] := '时间';
  sgdRecord.TitleText[4] := 'ID';
end;

procedure TfrmRecordSet.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRecord);
end;

procedure TfrmRecordSet.GetPatientInchRecord(const APatID: string;
  const AVisit: Integer);
var
  vRow: Integer;
begin
  sgdRecord.RowCount := 0;

  TBLLInvoke.GetPatientInchRecord(APatID, AVisit,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
      begin
        ShowMessage(ABLLServer.MethodError);
        Exit;
      end;

      if AMemTable.RecordCount > 0 then
      begin
        sgdRecord.RowCount := AMemTable.RecordCount;
        vRow := 0;

        with AMemTable do
        begin
          First;
          while not Eof do
          begin
            sgdRecord.Cells[vRow, 2] := FieldByName('Name').AsString;
            sgdRecord.Cells[vRow, 3] := FormatDateTime('YYYY-MM-DD HH:mm:SS', FieldByName('Dt').AsDateTime);
            sgdRecord.Cells[vRow, 4] := FieldByName('ID').AsString;

            Inc(vRow);
            Next;
          end;
        end;
      end;
    end);
end;

procedure TfrmRecordSet.ShowDialog(const APatID: string; const AVisit: Integer);
begin
  GetPatientInchRecord(APatID, AVisit);
  ShowModal;
end;

end.
