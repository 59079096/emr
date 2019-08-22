unit frm_PatientHisRecord;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, FireDAC.Comp.Client,
  emr_Common, HCEmrViewLite, CFControl, CFSplitter, frm_ImportRecord, emr_BLLInvoke;

type
  TfrmPatientHisRecord = class(TForm)
    tvRecord: TTreeView;
    spl1: TCFSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvRecordExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure tvRecordDblClick(Sender: TObject);
  private
    { Private declarations }
    FfrmImportRecord: TfrmImportRecord;
    procedure RefreshRecordNode;
    function GetOnImportAsText: THCImportAsTextEvent;
    procedure SetOnImportAsText(const Value: THCImportAsTextEvent);
  public
    { Public declarations }
    PatientInfo: TPatientInfo;
    property OnImportAsText: THCImportAsTextEvent read GetOnImportAsText write SetOnImportAsText;
  end;

implementation

{$R *.dfm}

procedure TfrmPatientHisRecord.FormCreate(Sender: TObject);
begin
  FfrmImportRecord := TfrmImportRecord.Create(nil);
  FfrmImportRecord.BorderStyle := bsNone;
  FfrmImportRecord.Align := alClient;
  FfrmImportRecord.Parent := Self;
  FfrmImportRecord.Show;
end;

procedure TfrmPatientHisRecord.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FfrmImportRecord);
  ClearRecordNode(tvRecord);
end;

procedure TfrmPatientHisRecord.FormShow(Sender: TObject);
begin
  RefreshRecordNode;
end;

function TfrmPatientHisRecord.GetOnImportAsText: THCImportAsTextEvent;
begin
  Result := FfrmImportRecord.OnImportAsText;
end;

procedure TfrmPatientHisRecord.RefreshRecordNode;
var
  vNode: TTreeNode;
  vPatInfo: TPatientInfo;
begin
  ClearRecordNode(tvRecord);

  TBLLInvoke.GetPatientHisInchInfo(PatientInfo.PatID, PatientInfo.VisitID,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    var
      vNode, vRecNode: TTreeNode;
    begin
      if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
      begin
        ShowMessage(ABLLServer.MethodError);
        Exit;
      end;

      if AMemTable <> nil then
      begin
        if AMemTable.RecordCount > 0 then
        begin
          tvRecord.Items.BeginUpdate;
          try
            with AMemTable do
            begin
              First;
              while not Eof do
              begin
                vPatInfo := TPatientInfo.Create;
                vPatInfo.PatID := FieldByName('PatID').AsString;
                vPatInfo.VisitID := FieldByName('VisitID').AsInteger;

                vNode := tvRecord.Items.AddObject(nil, '第' + FieldByName('VisitID').AsString
                  + FieldByName('BedNo').AsString + ' ' + FieldByName('DeptName').AsString, vPatInfo);
                vNode.HasChildren := True;
                vNode.ImageIndex := -1;
                vNode.SelectedIndex := -1;

                Next;
              end;
            end;
          finally
            tvRecord.Items.EndUpdate;
          end;
        end;
      end;
    end);
end;

procedure TfrmPatientHisRecord.SetOnImportAsText(
  const Value: THCImportAsTextEvent);
begin
  FfrmImportRecord.OnImportAsText := Value;
end;

procedure TfrmPatientHisRecord.tvRecordDblClick(Sender: TObject);
var
  vDesPID, vDesID, vRecordID, vPageIndex: Integer;
  vStream: TMemoryStream;
begin
  if not TreeNodeIsRecord(tvRecord.Selected) then Exit;  // 不是病历节点

  FfrmImportRecord.EmrView.Clear();

  GetNodeRecordInfo(tvRecord.Selected, vDesPID, vDesID, vRecordID);

  if vRecordID > 0 then
  begin
    vStream := TMemoryStream.Create;
    try
      TBLLInvoke.GetRecordContent(vRecordID, vStream);
      if vStream.Size > 0 then
        FfrmImportRecord.EmrView.LoadFromStream(vStream);
    finally
      FreeAndNil(vStream);
    end;
  end;
end;

procedure TfrmPatientHisRecord.tvRecordExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  vPatNode: TTreeNode;
  vPatientInfo: TPatientInfo;
begin
  if Node.Parent = nil then  // 某次住院信息根结点
  begin
    if Node.Count = 0 then  // 仅在无病历节点时才获取
    begin
      vPatientInfo := Node.Data;

      TBLLInvoke.GetPatientInchRecord(vPatientInfo.PatID, vPatientInfo.VisitID,
        procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
        var
          vRecordInfo: TRecordInfo;
          vRecordDataSetInfo: TRecordDataSetInfo;
          vDesPID: Integer;
          vNode, vRecNode: TTreeNode;
        begin
          if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
          begin
            ShowMessage(ABLLServer.MethodError);
            Exit;
          end;

          vDesPID := 0;

          if AMemTable <> nil then
          begin
            if AMemTable.RecordCount > 0 then
            begin
              tvRecord.Items.BeginUpdate;
              try
                with AMemTable do
                begin
                  First;
                  while not Eof do
                  begin
                    if vDesPID <> FieldByName('desPID').AsInteger then
                    begin
                      vDesPID := FieldByName('desPID').AsInteger;
                      vRecordDataSetInfo := TRecordDataSetInfo.Create;
                      vRecordDataSetInfo.DesPID := vDesPID;

                      vNode := tvRecord.Items.AddChildObject(Node,
                        ClientCache.GetDataSetInfo(vDesPID).GroupName, vRecordDataSetInfo);

                      vNode.ImageIndex := -1;
                      vNode.SelectedIndex := -1;
                    end;

                    vRecordInfo := TRecordInfo.Create;
                    vRecordInfo.ID := FieldByName('ID').AsInteger;
                    vRecordInfo.DesID := FieldByName('desID').AsInteger;
                    vRecordInfo.RecName := FieldByName('Name').AsString;
                    vRecordInfo.LastDT := FieldByName('LastDT').AsDateTime;  // 病历结构存储业务完全上线后可以去掉 201905081300

                    vRecNode := tvRecord.Items.AddChildObject(vNode,
                      vRecordInfo.RecName + '(' + FormatDateTime('YYYY-M-D HH:mm', vRecordInfo.LastDT) + ')', vRecordInfo);

                    vRecNode.ImageIndex := 3;
                    vRecNode.SelectedIndex := 4;

                    Next;
                  end;
                end;
              finally
                tvRecord.Items.EndUpdate;
              end;
            end;
          end;
        end);

      // 无病历时患者节点展开后去掉+号
      if Node.Count = 0 then
        Node.HasChildren := False;
    end;
  end;
end;

end.
