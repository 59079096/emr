{*******************************************************}
{                                                       }
{         ����HCView�ĵ��Ӳ�������  ���ߣ���ͨ          }
{                                                       }
{ �˴������ѧϰ����ʹ�ã�����������ҵĿ�ģ��ɴ�������  }
{ �����ʹ���߳е�������QQȺ 649023932 ����ȡ����ļ��� }
{ ������                                                }
{                                                       }
{*******************************************************}

unit frm_PatientList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, emr_Common, StdCtrls, Vcl.Grids, Vcl.Menus, FireDAC.Comp.Client,
  System.Generics.Collections, frm_PatientRecord, Vcl.ExtCtrls, CFControl,
  CFPanel, CFButtonEdit, CFGridEdit, CFEdit, CFPopupEdit, CFButton, CFCombobox,
  CFDateRang, CFDateTimePicker, CFPopupForm;

type
  TShowPatientRecord = procedure(const APatInfo: TPatientInfo) of object;

  TfrmPatientList = class(TForm)
    sgdPatient: TStringGrid;
    pnl1: TPanel;
    pnlSearch: TPanel;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    gdtDepts: TCFGridEdit;
    edtSeachValue: TCFEdit;
    lbl4: TLabel;
    btnSearch: TCFButton;
    lblSeachInfo: TLabel;
    cbbState: TCFCombobox;
    lbl5: TLabel;
    edtSearch: TCFPopupEdit;
    dtr1: TCFDateRang;
    pnlOverView: TPanel;
    btn1: TButton;
    lbl6: TLabel;
    pm1: TPopupMenu;
    mniN1111: TMenuItem;
    mniN22221: TMenuItem;
    mniN3331: TMenuItem;
    lbl7: TLabel;
    lbl8: TLabel;
    lbl9: TLabel;
    lbl10: TLabel;
    lbl11: TLabel;
    lbl12: TLabel;
    lbl13: TLabel;
    lbl14: TLabel;
    btn2: TButton;
    lblPatBedNo: TLabel;
    lblPatName: TLabel;
    procedure sgdPatientDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure sgdPatientMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
    FHintPopup: TCFHintPopupForm;
    //
    FPatientMTB: TFDMemTable;
    FOnShowPatientRecord: TShowPatientRecord;
    procedure IniPatientGrid;
    procedure GetPatients;
    procedure ShowPopupHint(const X, Y: Integer);
  public
    { Public declarations }
    UserInfo: TUserInfo;
    property OnShowPatientRecord: TShowPatientRecord read FOnShowPatientRecord write FOnShowPatientRecord;
  end;

implementation

uses
  frm_DoctorLevel, emr_MsgPack, emr_Entry, emr_BLLInvoke;

{$R *.dfm}

procedure TfrmPatientList.btnSearchClick(Sender: TObject);
begin
  lblSeachInfo.Caption := '���ҡ�' + cbbState.Text;
  if edtSeachValue.Text <> '' then
    lblSeachInfo.Caption := lblSeachInfo.Caption + '��' + edtSeachValue.Text;

  edtSearch.ClosePopup;
end;

procedure TfrmPatientList.FormCreate(Sender: TObject);
begin
  FPatientMTB := TFDMemTable.Create(nil);
  FHintPopup := TCFHintPopupForm.Create(nil);
  FHintPopup.PopupControl := pnlOverView;
end;

procedure TfrmPatientList.FormDestroy(Sender: TObject);
begin
  FPatientMTB.Free;
  FHintPopup.Free;
end;

procedure TfrmPatientList.FormShow(Sender: TObject);
begin
  TBLLInvoke.Query('Comm_Dept', 'id,name', procedure(const ABLL: TBLLServerProxy; const AMemTable: TFDMemTable)
    begin
      gdtDepts.LoadFromDataSet(AMemTable);
    end);

  gdtDepts.Text := UserInfo.DeptName;

  GetPatients;
end;

procedure TfrmPatientList.GetPatients;
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)  // ��ȡ����
    var
      vReplaceParam: TMsgPack;
    begin
      ABLLServerReady.Cmd := BLL_HIS_GETINPATIENT;  // ��ȡ����
      vReplaceParam := ABLLServerReady.ReplaceParam;
      vReplaceParam.S[TUser.DeptID] := UserInfo.GroupDeptIDs;
      ABLLServerReady.BackDataSet := True;  // ���߷����Ҫ����ѯ���ݼ��������
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // ����˷�������ִ�в��ɹ�
      begin
        ShowMessage(ABLLServer.MethodError);
        Exit;
      end;

      FPatientMTB.Close;
      FPatientMTB.Data := AMemTable.Data;
      lblSeachInfo.Caption := FPatientMTB.RecordCount.ToString + '�����ߡ�';

      IniPatientGrid;
    end);
end;

procedure TfrmPatientList.IniPatientGrid;
var
  vRow, vCol: Integer;
begin
  sgdPatient.ColCount := 15;

  vCol := 0;
  sgdPatient.Cells[vCol, 0] := '���';
  sgdPatient.ColWidths[vCol] := 50;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := '����';  // 1
  sgdPatient.ColWidths[vCol] := 30;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := 'סԺ��';  // 2
  sgdPatient.ColWidths[vCol] := 50;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := '���';  // 3
  sgdPatient.ColWidths[vCol] := 30;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := '����';  // 4
  sgdPatient.ColWidths[vCol] := 50;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := '�Ա�';  // 5
  sgdPatient.ColWidths[vCol] := 30;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := '����';  // 6
  sgdPatient.ColWidths[vCol] := 50;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := '��Ժʱ��';  // 7
  sgdPatient.ColWidths[vCol] := 100;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := '���';  // 8
  sgdPatient.ColWidths[vCol] := 100;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := '����';  // 9
  sgdPatient.ColWidths[vCol] := 30;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := '������';  // 10
  sgdPatient.ColWidths[vCol] := 30;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := '���߱��';  // 11
  sgdPatient.ColWidths[vCol] := 50;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := '��ǰ����';  // 12
  sgdPatient.ColWidths[vCol] := 100;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := '��ǰ����ID';  // 13
  sgdPatient.ColWidths[vCol] := 30;

  Inc(vCol);
  sgdPatient.Cells[vCol, 0] := '�Ա����';  // 14
  sgdPatient.ColWidths[vCol] := 0;

  sgdPatient.RowCount := FPatientMTB.RecordCount + 1;

  vRow := 1;
  with FPatientMTB do  // ����RefreshPatientItem������ʱvDataSet���ǵ�ǰ�б�û�У������ӵĻ���
  begin
    First;
    while not Eof do
    begin
      sgdPatient.Cells[0, vRow] := '';
      sgdPatient.Cells[1, vRow] := FieldByName('BedNO').AsString;
      sgdPatient.Cells[2, vRow] := FieldByName('PatID').AsString;
      sgdPatient.Cells[3, vRow] := FieldByName('VisitID').AsString;
      sgdPatient.Cells[4, vRow] := FieldByName('Name').AsString;
      sgdPatient.Cells[5, vRow] := FieldByName('Sex').AsString;
      sgdPatient.Cells[6, vRow] := FieldByName('Age').AsString;
      sgdPatient.Cells[7, vRow] := FormatDateTime('YYYY-MM-DD HH:mm', FieldByName('InDate').AsDateTime);
      sgdPatient.Cells[8, vRow] := FieldByName('Diagnosis').AsString;
      sgdPatient.Cells[9, vRow] := FieldByName('IllState').AsString;
      sgdPatient.Cells[10, vRow] := FieldByName('CareLevel').AsString;
      sgdPatient.Cells[11, vRow] := FieldByName('InpNo').AsString;
      sgdPatient.Cells[12, vRow] := FieldByName('DeptName').AsString;
      sgdPatient.Cells[13, vRow] := FieldByName('DeptID').AsString;
      sgdPatient.Cells[14, vRow] := FieldByName('SexCode').AsString;

      Inc(vRow);
      Next;
    end;
  end;
end;

procedure TfrmPatientList.sgdPatientDblClick(Sender: TObject);
var
  vPatientInfo: TPatientInfo;
begin
  if (sgdPatient.Cells[1, sgdPatient.Row] <> '') and Assigned(FOnShowPatientRecord) then
  begin
    if FPatientMTB.Locate('PatID;VisitID',
      VarArrayOf([sgdPatient.Cells[2, sgdPatient.Row],
      sgdPatient.Cells[3, sgdPatient.Row]]))
    then
    begin
      vPatientInfo := TPatientInfo.Create;
      try
        vPatientInfo.BedNo := sgdPatient.Cells[1, sgdPatient.Row];
        vPatientInfo.DeptID := StrToInt(sgdPatient.Cells[2, sgdPatient.Row]);
        vPatientInfo.VisitID := StrToInt(sgdPatient.Cells[3, sgdPatient.Row]);
        vPatientInfo.&Name := sgdPatient.Cells[4, sgdPatient.Row];
        vPatientInfo.Sex := sgdPatient.Cells[5, sgdPatient.Row];
        vPatientInfo.Age := sgdPatient.Cells[6, sgdPatient.Row];
        vPatientInfo.InDeptDateTime := FPatientMTB.FieldByName('InDate').AsDateTime;// StrToDateTime(sgdPatient.Cells[6, sgdPatient.Row], EmrFormatSettings);
        vPatientInfo.InpNo := sgdPatient.Cells[13, sgdPatient.Row];
        vPatientInfo.PatID := sgdPatient.Cells[11, sgdPatient.Row];
        vPatientInfo.DeptName := sgdPatient.Cells[12, sgdPatient.Row];
        vPatientInfo.SexCode := StrToInt(sgdPatient.Cells[14, sgdPatient.Row]);

        FOnShowPatientRecord(vPatientInfo);
      finally
        FreeAndNil(vPatientInfo);
      end;
    end;
  end;
end;

procedure TfrmPatientList.sgdPatientMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  vRow, vCol: Integer;
  vRect: TRect;
  vPt: TPoint;
begin
  sgdPatient.MouseToCell(X, Y, vCol, vRow);
  if (vRow > 0) and (vCol = 0) then
  begin
    lblPatBedNo.Caption := sgdPatient.Cells[1, vRow];
    lblPatName.Caption := sgdPatient.Cells[4, vRow];
    vRect := sgdPatient.CellRect(0, vRow);
    vPt := vRect.TopLeft;
    //vPt := sgdPatient.ClientToParent(vPt, Self);
    vPt := sgdPatient.ClientToScreen(vPt);
    //GetCursorPos(vPt);
    vPt.X := vPt.X;
    vPt.Y := vPt.Y - pnlOverView.Height;

    FHintPopup.Bleed.Bottom := sgdPatient.RowHeights[vRow] + 2;
    ShowPopupHint(vPt.X, vPt.Y);
  end;
end;

procedure TfrmPatientList.ShowPopupHint(const X, Y: Integer);
begin
  if not FHintPopup.Opened then
    FHintPopup.Popup(X, Y);
end;

end.
