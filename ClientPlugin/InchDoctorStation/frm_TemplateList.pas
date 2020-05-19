{*******************************************************}
{                                                       }
{         ����HCView�ĵ��Ӳ�������  ���ߣ���ͨ          }
{                                                       }
{ �˴������ѧϰ����ʹ�ã�����������ҵĿ�ģ��ɴ�������  }
{ �����ʹ���߳е�������QQȺ 649023932 ����ȡ����ļ��� }
{ ������                                                }
{                                                       }
{*******************************************************}

unit frm_TemplateList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.Grids, CFControl, CFDateTimePicker;

type
  TfrmTemplateList = class(TForm)
    pnl1: TPanel;
    tvTemplate: TTreeView;
    spl1: TSplitter;
    pnl2: TPanel;
    edtRecordName: TEdit;
    btnOK: TButton;
    sgdTempList: TStringGrid;
    lbl1: TLabel;
    lbl2: TLabel;
    dtpRecDT: TCFDateTimePicker;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure tvTemplateChange(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
    FTemplateID, FDesID: Integer;
    FRecordName: string;
    procedure ClearTemplateGroup;
    procedure GetTemplateGroup;
    procedure ClearTemplateList;
  public
    { Public declarations }
    /// <summary> ѡ���ģ��ID </summary>
    property TemplateID: Integer read FTemplateID write FTemplateID;

    /// <summary> ѡ���ģ���Ӧ�����ݼ�DesID </summary>
    property DesID: Integer read FDesID write FDesID;

    /// <summary> ѡ���ģ������ </summary>
    property RecordName: string read FRecordName write FRecordName;
  end;

implementation

uses
  emr_Common, emr_BLLInvoke, FireDAC.Comp.Client;

{$R *.dfm}

procedure TfrmTemplateList.btnOKClick(Sender: TObject);
begin
  if sgdTempList.Row > 0 then
  begin
    FTemplateID := sgdTempList.Cells[3, sgdTempList.Row].ToInteger;
    FRecordName := edtRecordName.Text;

    if Trim(FRecordName) <> '' then
    begin
      Close;
      Self.ModalResult := mrOk;
    end
    else
    begin
      ShowMessage('����д��ȷ�Ĳ������ƣ�');
      edtRecordName.SetFocus;
    end;
  end
  else
    ShowMessage('��ѡ��ģ�壡');
end;

procedure TfrmTemplateList.ClearTemplateGroup;
var
  i: Integer;
  vNode: TTreeNode;
begin
  for i := 0 to tvTemplate.Items.Count - 1 do
  begin
    vNode := tvTemplate.Items[i];
    if vNode <> nil then
    begin
      if TreeNodeIsTemplate(vNode) then
        TTemplateInfo(vNode.Data).Free
      else
        TDataSetInfo(vNode.Data).Free;
    end;
  end;

  tvTemplate.Items.Clear;
end;

procedure TfrmTemplateList.ClearTemplateList;
begin
  sgdTempList.FixedRows := 0;
  sgdTempList.RowCount := 1;
  edtRecordName.Text := '';
  sgdTempList.Row := 0;
  FDesID := 0;
  FRecordName := '';
end;

procedure TfrmTemplateList.FormShow(Sender: TObject);
begin
  GetTemplateGroup;
  sgdTempList.Cells[0, 0] := '����';
  sgdTempList.Cells[1, 0] := '����';
  sgdTempList.Cells[2, 0] := '����ID';
  sgdTempList.Cells[3, 0] := 'ID';

  sgdTempList.ColWidths[0] := 200;
  sgdTempList.ColWidths[1] := 50;
  sgdTempList.ColWidths[2] := 50;
  sgdTempList.ColWidths[3] := 50;
end;

procedure TfrmTemplateList.GetTemplateGroup;
begin
  ClearTemplateGroup;

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDATAELEMENTSETALL;  // ��ȡ���ݼ�(ȫĿ¼)��Ϣ
      ABLLServerReady.BackDataSet := True;  // ���߷����Ҫ����ѯ���ݼ��������
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)

      {$REGION 'GetParentNode'}
      function GetParentNode(const APID: Integer): TTreeNode;
      var
        i: Integer;
      begin
        Result := nil;
        for i := 0 to tvTemplate.Items.Count - 1 do
        begin
          if tvTemplate.Items[i].Data <> nil then
          begin
            if TDataSetInfo(tvTemplate.Items[i].Data).ID = APID then
            begin
              Result := tvTemplate.Items[i];
              Break;
            end;
          end;
        end;
      end;
      {$ENDREGION}

    var
      vDataSetInfo: TDataSetInfo;
      vNode: TTreeNode;
      vPID: Integer;
    begin
      if not ABLLServer.MethodRunOk then  // ����˷�������ִ�в��ɹ�
      begin
        ShowMessage(ABLLServer.MethodError);
        Exit;
      end;

      if AMemTable <> nil then
      begin
        tvTemplate.Items.BeginUpdate;
        try
          with AMemTable do
          begin
            First;
            while not Eof do
            begin
              if ((FieldByName('UseRang').AsInteger = TDataSetInfo.USERANG_CLINIC)  // �ٴ�
                  or (FieldByName('UseRang').AsInteger = TDataSetInfo.USERANG_CLINICANDNURSE)  // �ٴ��ͻ���
                 )
                and ((FieldByName('InOrOut').AsInteger = TDataSetInfo.INOROUT_IN)  // סԺ
                     or (FieldByName('InOrOut').AsInteger = TDataSetInfo.INOROUT_INOUT)  // סԺ������
                    )
              then
              begin
                vNode := nil;

                vPID := FieldByName('pid').AsInteger;
                if vPID <> 0 then
                begin
                  vNode := GetParentNode(vPID);
                  if not Assigned(vNode) then
                  begin
                    Next;
                    Continue;
                  end;
                end;

                vDataSetInfo := TDataSetInfo.Create;
                vDataSetInfo.ID := FieldByName('id').AsInteger;
                vDataSetInfo.PID := FieldByName('pid').AsInteger;
                vDataSetInfo.GroupClass := FieldByName('Class').AsInteger;
                vDataSetInfo.GroupType := FieldByName('Type').AsInteger;
                vDataSetInfo.GroupName := FieldByName('Name').AsString;
                vDataSetInfo.UseRang := FieldByName('UseRang').AsInteger;
                vDataSetInfo.InOrOut := FieldByName('InOrOut').AsInteger;

                if Assigned(vNode) then
                  tvTemplate.Items.AddChildObject(vNode, vDataSetInfo.GroupName, vDataSetInfo)
                else
                  tvTemplate.Items.AddObject(nil, vDataSetInfo.GroupName, vDataSetInfo);
              end;

              Next;
            end;
          end;
        finally
          tvTemplate.Items.EndUpdate;
        end;
      end;
    end);
end;

procedure TfrmTemplateList.tvTemplateChange(Sender: TObject; Node: TTreeNode);
begin
  ClearTemplateList;

  if tvTemplate.Selected.Count = 0 then
  begin
    FDesID := TDataSetInfo(Node.Data).ID;
    FRecordName := Node.Text;
    edtRecordName.Text := FRecordName;

    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)
      begin
        ABLLServerReady.Cmd := BLL_GETTEMPLATELIST;  // ��ȡָ�����ݼ���Ӧ��ģ��
        ABLLServerReady.ExecParam.I['desID'] := FDesID;
        ABLLServerReady.BackDataSet := True;  // ���߷����Ҫ����ѯ���ݼ��������
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      var
        i: Integer;
      begin
        if not ABLLServer.MethodRunOk then  // ����˷�������ִ�в��ɹ�
        begin
          ShowMessage(ABLLServer.MethodError);
          Exit;
        end;

        if AMemTable <> nil then
        begin
          if AMemTable.RecordCount > 0 then
          begin
            sgdTempList.RowCount := AMemTable.RecordCount + 1;
            sgdTempList.FixedRows := 1;
            i := 1;

            with AMemTable do
            begin
              First;
              while not Eof do
              begin
                sgdTempList.Cells[0, i] := FieldByName('tname').AsString;
                sgdTempList.Cells[1, i] := FieldByName('Owner').AsString;
                sgdTempList.Cells[2, i] := FieldByName('OwnerID').AsString;
                sgdTempList.Cells[3, i] := FieldByName('id').AsString;
                Inc(i);

                Next;
              end;
            end;
          end;
        end;
      end);
  end;
end;

end.
