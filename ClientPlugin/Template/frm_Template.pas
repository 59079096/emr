{*******************************************************}
{                                                       }
{         ����HCView�ĵ��Ӳ�������  ���ߣ���ͨ          }
{                                                       }
{ �˴������ѧϰ����ʹ�ã�����������ҵĿ�ģ��ɴ�������  }
{ �����ʹ���߳е�������QQȺ 649023932 ����ȡ����ļ��� }
{ ������                                                }
{                                                       }
{*******************************************************}

unit frm_Template;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FunctionIntf, FunctionImp,
  Vcl.ComCtrls, Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.Menus, Data.DB,
  Vcl.StdCtrls, Vcl.Grids, emr_Common, frm_Record, FireDAC.Comp.Client,
  frm_DataElement, frm_DataElementDomain;

type
  TfrmTemplate = class(TForm)
    spl1: TSplitter;
    pgTemplate: TPageControl;
    tsHelp: TTabSheet;
    tvTemplate: TTreeView;
    il: TImageList;
    pmTemplate: TPopupMenu;
    mniNewTemplate: TMenuItem;
    mniDeleteTemplate: TMenuItem;
    pmpg: TPopupMenu;
    mniCloseTemplate: TMenuItem;
    spl3: TSplitter;
    mniInsertTemplate: TMenuItem;
    mniTemplateProperty: TMenuItem;
    mniCloseAll: TMenuItem;
    pnlDE: TPanel;
    pnlCV: TPanel;
    spl2: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvTemplateDblClick(Sender: TObject);
    procedure tvTemplateExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure mniNewTemplateClick(Sender: TObject);
    procedure pmTemplatePopup(Sender: TObject);
    procedure mniDeleteTemplateClick(Sender: TObject);
    procedure pgTemplateMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mniCloseTemplateClick(Sender: TObject);
    procedure mniInsertTemplateClick(Sender: TObject);
    procedure mniTemplatePropertyClick(Sender: TObject);
    procedure tvTemplateCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure mniCloseAllClick(Sender: TObject);
  private
    { Private declarations }
    FUserInfo: TUserInfo;
    FfrmDataElement: TfrmDataElement;
    FfrmDataElementDomain: TfrmDataElementDomain;
    FOnFunctionNotify: TFunctionNotifyEvent;
    procedure ClearTemplateDeSet;
    procedure ShowTemplateDeSet;
    function GetRecordEditPageIndex(const ATempID: Integer): Integer;
    function GetActiveRecord: TfrmRecord;
    procedure InsertDataElementAs(const AProc: TProc<TfrmRecord>);
    //
    procedure CloseTemplatePage(const APageIndex: Integer;
      const ASaveChange: Boolean = True);
    procedure DoSaveTempContent(Sender: TObject);
    procedure DoRecordChangedSwitch(Sender: TObject);
    procedure DoDESelectChange(Sender: TObject);
    procedure DoDEInsertAsDeItem(Sender: TObject);
    procedure DoDEInsertAsDeGroup(Sender: TObject);
    procedure DoDEInsertAsDeEdit(Sender: TObject);
    procedure DoDEInsertAsDeCombobox(Sender: TObject);
    procedure DoDEInsertAsDeDateTime(Sender: TObject);
    procedure DoDEInsertAsDeRadioGroup(Sender: TObject);
    procedure DoDEInsertAsDeCheckBox(Sender: TObject);
    procedure DoDEInsertAsFloatBarCode(Sender: TObject);
    procedure DoDEInsertAsDeImage(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  end;

  procedure PluginShowTemplateForm(AIFun: IFunBLLFormShow);
  procedure PluginCloseTemplateForm;

var
  frmTemplate: TfrmTemplate;
  PluginID: string;

implementation

uses
  Vcl.Clipbrd, PluginConst, FunctionConst, emr_BLLInvoke, emr_MsgPack,
  emr_Entry, HCEmrElementItem, HCEmrGroupItem, HCCommon, CFBalloonHint,
  HCEmrView, frm_TemplateInfo;

{$R *.dfm}

procedure PluginShowTemplateForm(AIFun: IFunBLLFormShow);
begin
  if not Assigned(frmTemplate) then
    Application.CreateForm(TfrmTemplate, frmTemplate);

  frmTemplate.FOnFunctionNotify := AIFun.OnNotifyEvent;
  frmTemplate.Show;
end;

procedure PluginCloseTemplateForm;
begin
  if Assigned(frmTemplate) then
    FreeAndNil(frmTemplate);
end;

procedure TfrmTemplate.ClearTemplateDeSet;

  {procedure ClearTemplateGroupNode(const ANode: TTreeNode);
  var
    i: Integer;
  begin
    for i := 0 to ANode.Count - 1 do
      ClearTemplateGroupNode(ANode.Item[i]);

    if TObject(ANode.Data) is TTemplateGroupInfo then
      TTemplateGroupInfo(ANode.Data).Free
    else
      TTemplateInfo(ANode.Data).Free;
  end;}

var
  i: Integer;
  vNode: TTreeNode;
begin
  for i := 0 to tvTemplate.Items.Count - 1 do
  begin
    //ClearTemplateGroupNode(tvTemplate.Items[i]);
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

procedure TfrmTemplate.CloseTemplatePage(const APageIndex: Integer;
  const ASaveChange: Boolean = True);
var
  i: Integer;
  vPage: TTabSheet;
  vFrmRecord: TfrmRecord;
begin
  if APageIndex >= 0 then
  begin
    vPage := pgTemplate.Pages[APageIndex];

    for i := 0 to vPage.ControlCount - 1 do
    begin
      if vPage.Controls[i] is TfrmRecord then
      begin
        if ASaveChange then  // ��Ҫ���䶯
        begin
          vFrmRecord := (vPage.Controls[i] as TfrmRecord);
          if vFrmRecord.EmrView.IsChanged then  // �б䶯
          begin
            if MessageDlg('�Ƿ񱣴�ģ�� ' + TTemplateInfo(vFrmRecord.ObjectData).NameEx + ' ��',
              mtWarning, [mbYes, mbNo], 0) = mrYes
            then
            begin
              DoSaveTempContent(vFrmRecord);
            end;
          end;
        end;

        vPage.Controls[i].Free;
        Break;
      end;
    end;

    vPage.Free;

    if APageIndex > 0 then
      pgTemplate.ActivePageIndex := APageIndex - 1;
  end;
end;

procedure TfrmTemplate.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  //Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TfrmTemplate.DoDESelectChange(Sender: TObject);
begin
  FfrmDataElementDomain.DeName := FfrmDataElement.GetDeName;
  FfrmDataElementDomain.DomainID := FfrmDataElement.GetDomainID;
end;

procedure TfrmTemplate.DoDEInsertAsDeCheckBox(Sender: TObject);
begin
  InsertDataElementAs(procedure(AFrmRecord: TfrmRecord)
    begin
      // ������ʾ��Щѡ����Ϊѡ��������̫��ʱ��ʾ����
      AfrmRecord.InsertDeCheckBox(FfrmDataElement.GetDeIndex, FfrmDataElement.GetDeName);
    end);
end;

procedure TfrmTemplate.DoDEInsertAsDeCombobox(Sender: TObject);
begin
  InsertDataElementAs(procedure(AFrmRecord: TfrmRecord)
    begin
      AfrmRecord.InsertDeCombobox(FfrmDataElement.GetDeIndex, FfrmDataElement.GetDeName);
    end);
end;

procedure TfrmTemplate.DoDEInsertAsDeDateTime(Sender: TObject);
begin
  InsertDataElementAs(procedure(AFrmRecord: TfrmRecord)
    begin
      AfrmRecord.InsertDeDateTime(FfrmDataElement.GetDeIndex, FfrmDataElement.GetDeName);
    end);
end;

procedure TfrmTemplate.DoDEInsertAsDeEdit(Sender: TObject);
begin
  InsertDataElementAs(procedure(AFrmRecord: TfrmRecord)
    begin
      AfrmRecord.InsertDeEdit(FfrmDataElement.GetDeIndex, FfrmDataElement.GetDeName);
    end);
end;

procedure TfrmTemplate.DoDEInsertAsDeGroup(Sender: TObject);
begin
  InsertDataElementAs(procedure(AFrmRecord: TfrmRecord)
    begin
      AfrmRecord.InsertDeGroup(FfrmDataElement.GetDeIndex, FfrmDataElement.GetDeName);
    end);
end;

procedure TfrmTemplate.DoDEInsertAsDeImage(Sender: TObject);
begin
  InsertDataElementAs(procedure(AFrmRecord: TfrmRecord)
    begin
      AfrmRecord.InsertDeImage(FfrmDataElement.GetDeIndex, FfrmDataElement.GetDeName);
    end);
end;

procedure TfrmTemplate.DoDEInsertAsDeItem(Sender: TObject);
begin
  InsertDataElementAs(procedure(AFrmRecord: TfrmRecord)
    begin
      AfrmRecord.InsertDeItem(FfrmDataElement.GetDeIndex, FfrmDataElement.GetDeName);
    end);
end;

procedure TfrmTemplate.DoDEInsertAsDeRadioGroup(Sender: TObject);
begin
  InsertDataElementAs(procedure(AFrmRecord: TfrmRecord)
    begin
      // ������ʾ��Щѡ����Ϊѡ��������̫��ʱ��ʾ����
      AfrmRecord.InsertDeRadioGroup(FfrmDataElement.GetDeIndex, FfrmDataElement.GetDeName);
    end);
end;

procedure TfrmTemplate.DoDEInsertAsFloatBarCode(Sender: TObject);
begin
  InsertDataElementAs(procedure(AFrmRecord: TfrmRecord)
    begin
      AfrmRecord.InsertDeFloatBarCode(FfrmDataElement.GetDeIndex, FfrmDataElement.GetDeName);
    end);
end;

procedure TfrmTemplate.DoRecordChangedSwitch(Sender: TObject);
var
  vText: string;
begin
  if (Sender is TfrmRecord) then
  begin
    if (Sender as TfrmRecord).Parent is TTabSheet then
    begin
      if (Sender as TfrmRecord).EmrView.IsChanged then
        vText := TTemplateInfo((Sender as TfrmRecord).ObjectData).NameEx + '*'
      else
        vText := TTemplateInfo((Sender as TfrmRecord).ObjectData).NameEx;

      ((Sender as TfrmRecord).Parent as TTabSheet).Caption := vText;
    end;
  end;
end;

procedure TfrmTemplate.DoSaveTempContent(Sender: TObject);
var
  vSM: TMemoryStream;
  vTempID: Integer;
begin
  vSM := TMemoryStream.Create;
  try
    (Sender as TfrmRecord).EmrView.SaveToStream(vSM);

    vTempID := TTemplateInfo((Sender as TfrmRecord).ObjectData).ID;

    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)  // ��ȡ����
      begin
        ABLLServerReady.Cmd := BLL_SAVETEMPLATECONTENT;  // ��ȡģ������б�
        ABLLServerReady.ExecParam.I['tid'] := vTempID;
        ABLLServerReady.ExecParam.ForcePathObject('content').LoadBinaryFromStream(vSM);
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      begin
        if ABLLServer.MethodRunOk then  // ����˷�������ִ�гɹ�
        begin
          (Sender as TfrmRecord).EmrView.IsChanged := False;  // ������ĵ���ʶΪ���޸�
          ShowMessage('����ɹ���');
        end
        else
          ShowMessage(ABLLServer.MethodError);
      end);
  finally
    FreeAndNil(vSM);
  end;
end;

procedure TfrmTemplate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FOnFunctionNotify(PluginID, FUN_MAINFORMSHOW, nil);  // ��ʾ������
  FOnFunctionNotify(PluginID, FUN_BLLFORMDESTROY, nil);  // �ͷ�ҵ������Դ
end;

procedure TfrmTemplate.FormCreate(Sender: TObject);
begin
  PluginID := PLUGIN_TEMPLATE;
  //SetWindowLong(Handle, GWL_EXSTYLE, (GetWindowLong(handle, GWL_EXSTYLE) or WS_EX_APPWINDOW));
end;

procedure TfrmTemplate.FormDestroy(Sender: TObject);
var
  i, j: Integer;
begin
  ClearTemplateDeSet;

  for i := 0 to pgTemplate.PageCount - 1 do
  begin
    for j := 0 to pgTemplate.Pages[i].ControlCount - 1 do
    begin
      if pgTemplate.Pages[i].Controls[j] is TfrmRecord then
      begin
        pgTemplate.Pages[i].Controls[j].Free;
        Break;
      end;
    end;
  end;

  FreeAndNil(FfrmDataElement);
  FreeAndNil(FfrmDataElementDomain);
end;

procedure TfrmTemplate.FormShow(Sender: TObject);
var
  vObjFun: IObjectFunction;
begin
  // ��ȡ�ͻ��������
  vObjFun := TObjectFunction.Create;
  FOnFunctionNotify(PluginID, FUN_CLIENTCACHE, vObjFun);
  ClientCache := TClientCache(vObjFun.&Object);

  // ��ǰ��¼�û�ID
  FOnFunctionNotify(PluginID, FUN_USERINFO, vObjFun);  // ��ȡ�������¼�û���
  FUserInfo := TUserInfo(vObjFun.&Object);
  //
  FOnFunctionNotify(PluginID, FUN_MAINFORMHIDE, nil);  // ����������

  ShowTemplateDeSet;  // ��ȡ����ʾģ�����ݼ���Ϣ

  // ѡ���
  FfrmDataElementDomain := TfrmDataElementDomain.Create(nil);
  FfrmDataElementDomain.BorderStyle := bsNone;
  FfrmDataElementDomain.Align := alClient;
  FfrmDataElementDomain.Parent := pnlCV;
  FfrmDataElementDomain.Show;

  // ����Ԫ����
  FfrmDataElement := TfrmDataElement.Create(nil);
  FfrmDataElement.BorderStyle := bsNone;
  FfrmDataElement.Align := alClient;
  FfrmDataElement.Parent := pnlDE;
  FfrmDataElement.OnSelectChange := DoDESelectChange;
  FfrmDataElement.OnInsertAsDeItem := DoDEInsertAsDeItem;
  FfrmDataElement.OnInsertAsDeGroup := DoDEInsertAsDeGroup;
  FfrmDataElement.OnInsertAsDeEdit := DoDEInsertAsDeEdit;
  FfrmDataElement.OnInsertAsDeCombobox := DoDEInsertAsDeCombobox;
  FfrmDataElement.OnInsertAsDeDateTime := DoDEInsertAsDeDateTime;
  FfrmDataElement.OnInsertAsDeRadioGroup := DoDEInsertAsDeRadioGroup;
  FfrmDataElement.OnInsertAsDeCheckBox := DoDEInsertAsDeCheckBox;
  FfrmDataElement.OnInsertAsDeFloatBarCode := DoDEInsertAsFloatBarCode;
  FfrmDataElement.OnInsertAsDeImage := DoDEInsertAsDeImage;
  FfrmDataElement.Show;
end;

function TfrmTemplate.GetActiveRecord: TfrmRecord;
var
  vPage: TTabSheet;
  i: Integer;
begin
  Result := nil;

  vPage := pgTemplate.ActivePage;
  for i := 0 to vPage.ControlCount - 1 do
  begin
    if vPage.Controls[i] is TfrmRecord then
    begin
      Result := (vPage.Controls[i] as TfrmRecord);
      Break;
    end;
  end;
end;

function TfrmTemplate.GetRecordEditPageIndex(const ATempID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to pgTemplate.PageCount - 1 do
  begin
    if pgTemplate.Pages[i].Tag = ATempID then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TfrmTemplate.InsertDataElementAs(const AProc: TProc<TfrmRecord>);
var
  vFrmRecord: TfrmRecord;
begin
  vFrmRecord := GetActiveRecord;
  if Assigned(vFrmRecord) then
  begin
    if not vFrmRecord.EmrView.Focused then  // �ȸ����㣬���ڴ����괦��
      vFrmRecord.EmrView.SetFocus;

    AProc(vFrmRecord);
  end
  else
    ShowMessage('δ���ִ򿪵�ģ�壡');
end;

procedure TfrmTemplate.ShowTemplateDeSet;
begin
  ClearTemplateDeSet;

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDATAELEMENTSETALL;  // ��ȡ���ݼ�(ȫĿ¼)��Ϣ
      ABLLServerReady.BackDataSet := True;  // ���߷����Ҫ����ѯ���ݼ��������
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)

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

    var
      vNode: TTreeNode;
      vDataSetInfo: TDataSetInfo;
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
              vDataSetInfo := TDataSetInfo.Create;
              vDataSetInfo.ID := FieldByName('id').AsInteger;
              vDataSetInfo.PID := FieldByName('pid').AsInteger;
              vDataSetInfo.GroupClass := FieldByName('Class').AsInteger;
              vDataSetInfo.GroupType := FieldByName('Type').AsInteger;
              vDataSetInfo.GroupName := FieldByName('Name').AsString;

              if vDataSetInfo.PID <> 0 then
              begin
                vNode := tvTemplate.Items.AddChildObject(GetParentNode(vDataSetInfo.PID),
                  vDataSetInfo.GroupName, vDataSetInfo)
              end
              else
                vNode := tvTemplate.Items.AddObject(nil, vDataSetInfo.GroupName, vDataSetInfo);

              vNode.HasChildren := True;
              vNode.ImageIndex := -1;
              vNode.SelectedIndex := -1;

              Next;
            end;
          end;
        finally
          tvTemplate.Items.EndUpdate;
        end;
      end;
    end);
end;

procedure TfrmTemplate.mniDeleteTemplateClick(Sender: TObject);
var
  vTempID: Integer;
begin
  if TreeNodeIsTemplate(tvTemplate.Selected) then
  begin
    if MessageDlg('ȷ��Ҫɾ��ģ�� ' + TTemplateInfo(tvTemplate.Selected.Data).NameEx + ' ��',
      mtWarning, [mbYes, mbNo], 0) = mrYes
    then
    begin
      vTempID := TTemplateInfo(tvTemplate.Selected.Data).ID;

      BLLServerExec(
        procedure(const ABLLServerReady: TBLLServerProxy)  // ��ȡģ������ӷ����ģ��
        begin
          ABLLServerReady.Cmd := BLL_DELETETEMPLATE;  // ɾ��ģ��
          ABLLServerReady.ExecParam.I['tid'] := vTempID;
        end,
        procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
        begin
          if ABLLServer.MethodRunOk then  // ɾ���ɹ�
          begin
            tvTemplate.Items.Delete(tvTemplate.Selected);  // ɾ���ڵ�
            vTempID := GetRecordEditPageIndex(vTempID);
            if vTempID >= 0 then
              CloseTemplatePage(vTempID, False);
          end
          else
            ShowMessage(ABLLServer.MethodError);
        end);
    end;
  end;
end;

procedure TfrmTemplate.mniInsertTemplateClick(Sender: TObject);
var
  vNode: TTreeNode;
  vFrmRecord: TfrmRecord;
  vSM: TMemoryStream;
  vEmrView: THCEmrView;
  vGroupClass: Integer;
begin
  if TreeNodeIsTemplate(tvTemplate.Selected) then
  begin
    vFrmRecord := GetActiveRecord;

    if Assigned(vFrmRecord) then
    begin
      vNode := tvTemplate.Selected;

      vSM := TMemoryStream.Create;
      try
        TBLLInvoke.GetTemplateContent(TTemplateInfo(vNode.Data).ID, vSM);

        while vNode.Parent <> nil do
          vNode := vNode.Parent;

        vGroupClass := TDataSetInfo(vNode.Data).GroupClass;
        case vGroupClass of
          TDataSetInfo.CLASS_PAGE:  // ����
            begin
              vSM.Position := 0;
              vFrmRecord.EmrView.InsertStream(vSM);
            end;

          TDataSetInfo.CLASS_HEADER,  // ҳü��ҳ��
          TDataSetInfo.CLASS_FOOTER:
            begin
              vEmrView := THCEmrView.Create(nil);
              try
                vEmrView.LoadFromStream(vSM);
                vSM.Clear;
                vEmrView.Sections[0].Header.SaveToStream(vSM);
                vSM.Position := 0;
                if vGroupClass = TDataSetInfo.CLASS_HEADER then
                  vFrmRecord.EmrView.ActiveSection.Header.LoadFromStream(vSM, vEmrView.Style, HC_FileVersionInt)
                else
                  vFrmRecord.EmrView.ActiveSection.Footer.LoadFromStream(vSM, vEmrView.Style, HC_FileVersionInt);

                vFrmRecord.EmrView.IsChanged := True;
                vFrmRecord.EmrView.UpdateView;
              finally
                FreeAndNil(vEmrView);
              end;
            end;
        end;
      finally
        FreeAndNil(vSM);
      end;
    end;
  end;
end;

procedure TfrmTemplate.mniCloseTemplateClick(Sender: TObject);
begin
  CloseTemplatePage(pgTemplate.ActivePageIndex);
end;

procedure TfrmTemplate.mniTemplatePropertyClick(Sender: TObject);
var
  vFrmTemplateInfo: TfrmTemplateInfo;
begin
  if TreeNodeIsTemplate(tvTemplate.Selected) then
  begin
    vFrmTemplateInfo := TfrmTemplateInfo.Create(nil);
    try
      vFrmTemplateInfo.TempID := TTemplateInfo(tvTemplate.Selected.Data).ID;
      vFrmTemplateInfo.ShowModal;

      TTemplateInfo(tvTemplate.Selected.Data).NameEx := vFrmTemplateInfo.TempName;
      tvTemplate.Selected.Text := vFrmTemplateInfo.TempName;
    finally
      FreeAndNil(vFrmTemplateInfo);
    end;
  end;
end;

procedure TfrmTemplate.mniCloseAllClick(Sender: TObject);
begin
  while pgTemplate.PageCount > 1 do
    CloseTemplatePage(pgTemplate.PageCount - 1);
end;

procedure TfrmTemplate.mniNewTemplateClick(Sender: TObject);
var
  vTName: string;
begin
  vTName := InputBox('�½�', 'ģ������', '');
  if vTName.Trim = '' then Exit;

  // �˵�����ʾʱ�Ѿ�ȷ��ѡ������ڵ�����
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)  // ��ȡģ������ӷ����ģ��
    begin
      ABLLServerReady.Cmd := BLL_NEWTEMPLATE;  // �½�ģ��

      if TreeNodeIsTemplate(tvTemplate.Selected) then
        ABLLServerReady.ExecParam.I['desid'] := TTemplateInfo(tvTemplate.Selected.Data).DesID
      else
        ABLLServerReady.ExecParam.I['desid'] := TDataSetInfo(tvTemplate.Selected.Data).ID;

      ABLLServerReady.ExecParam.I['owner'] := 1;
      ABLLServerReady.ExecParam.I['ownerid'] := 0;
      ABLLServerReady.ExecParam.ForcePathObject('tname').AsString := vTName;
      //
      ABLLServerReady.AddBackField('tempid');
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    var
      vTempInfo: TTemplateInfo;
    begin
      if ABLLServer.MethodRunOk then  // �½�ģ��ִ�гɹ�
      begin
        vTempInfo := TTemplateInfo.Create;
        vTempInfo.ID := ABLLServer.BackField('tempid').AsInteger;
        vTempInfo.Owner := 1;
        vTempInfo.OwnerID := '';
        vTempInfo.NameEx := vTName;
        tvTemplate.Selected := tvTemplate.Items.AddChildObject(tvTemplate.Selected, vTempInfo.NameEx, vTempInfo);
      end
      else
        ShowMessage(ABLLServer.MethodError);
    end);
end;

procedure TfrmTemplate.pgTemplateMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vTabIndex: Integer;
  vPt: TPoint;
begin
  if Y < 20 then  // Ĭ�ϵ� pgRecordEdit.TabHeight ��ͨ����ȡ����ϵͳ�����õ�����ȷ��
  begin
    vTabIndex := pgTemplate.IndexOfTabAt(X, Y);

    if pgTemplate.Pages[vTabIndex].Tag = 0 then Exit; // ����

    if (vTabIndex >= 0) and (vTabIndex = pgTemplate.ActivePageIndex) then
    begin
      if Button = TMouseButton.mbRight then
      begin
        vPt := pgTemplate.ClientToScreen(Point(X, Y));
        pmpg.Popup(vPt.X, vPt.Y);
      end
      else
      if ssDouble in Shift then
        CloseTemplatePage(pgTemplate.ActivePageIndex);
    end;
  end;
end;

procedure TfrmTemplate.pmTemplatePopup(Sender: TObject);
begin
  mniNewTemplate.Enabled := not TreeNodeIsTemplate(tvTemplate.Selected);
  mniDeleteTemplate.Enabled := not mniNewTemplate.Enabled;
  mniInsertTemplate.Enabled := not mniNewTemplate.Enabled;
end;

procedure TfrmTemplate.tvTemplateCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
//var
//  vRect: TRect;
begin
  //DefaultDraw := False;
  if (not TreeNodeIsTemplate(Node)) and (TDataSetInfo(Node.Data).ID = 74) then  // ������
  begin
    tvTemplate.Canvas.Font.Style := [fsBold];
    tvTemplate.Canvas.Font.Color := clRed;
  end;

//  vRect := Node.DisplayRect(False);
//  tvTemplate.Canvas.TextOut(vRect.Left + (Node.Level + 1) * 16, vRect.Top, Node.Text);
end;

procedure TfrmTemplate.tvTemplateDblClick(Sender: TObject);
var
  vPageIndex, vTempID: Integer;
  vPage: TTabSheet;
  vFrmRecord: TfrmRecord;
  vSM: TMemoryStream;
begin
  if TreeNodeIsTemplate(tvTemplate.Selected) then
  begin
    vTempID := TTemplateInfo(tvTemplate.Selected.Data).ID;
    vPageIndex := GetRecordEditPageIndex(vTempID);
    if vPageIndex >= 0 then
    begin
      pgTemplate.ActivePageIndex := vPageIndex;
      Exit;
    end;

    vSM := TMemoryStream.Create;
    try
      TBLLInvoke.GetTemplateContent(vTempID, vSM);

      vFrmRecord := TfrmRecord.Create(nil);  // �����༭��
      vFrmRecord.EmrView.DesignMode := True;  // ���ģʽ
      vFrmRecord.ObjectData := tvTemplate.Selected.Data;
      if vSM.Size > 0 then
        vFrmRecord.EmrView.LoadFromStream(vSM);
    finally
      vSM.Free;
    end;

    if vFrmRecord <> nil then
    begin
      vPage := TTabSheet.Create(pgTemplate);
      vPage.Caption := tvTemplate.Selected.Text;
      vPage.Tag := vTempID;
      vPage.PageControl := pgTemplate;

      vFrmRecord.OnSave := DoSaveTempContent;
      vFrmRecord.OnChangedSwitch := DoRecordChangedSwitch;
      vFrmRecord.Parent := vPage;
      vFrmRecord.Align := alClient;
      vFrmRecord.Show;

      pgTemplate.ActivePage := vPage;

      vFrmRecord.EmrView.SetFocus;
    end;
  end;
end;

procedure TfrmTemplate.tvTemplateExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if Node.Count = 0 then
  begin
    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)
      begin
        ABLLServerReady.Cmd := BLL_GETTEMPLATELIST;  // ��ȡģ������ӷ����ģ��
        ABLLServerReady.ExecParam.I['desID'] := TDataSetInfo(Node.Data).ID;
        ABLLServerReady.BackDataSet := True;  // ���߷����Ҫ����ѯ���ݼ��������
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      var
        vTempInfo: TTemplateInfo;
        vTpltNode: TTreeNode;
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
            if AMemTable.RecordCount > 0 then
            begin
              with AMemTable do
              begin
                First;
                while not Eof do
                begin
                  vTempInfo := TTemplateInfo.Create;
                  vTempInfo.ID := FieldByName('id').AsInteger;
                  vTempInfo.DesID := FieldByName('desid').AsInteger;
                  vTempInfo.Owner := FieldByName('Owner').AsInteger;
                  vTempInfo.OwnerID := FieldByName('OwnerID').AsString;
                  vTempInfo.NameEx := FieldByName('tname').AsString;
                  vTpltNode := tvTemplate.Items.AddChildObject(Node, vTempInfo.NameEx, vTempInfo);
                  vTpltNode.ImageIndex := 0;

                  Next;
                end;
              end;
            end
            else
              Node.HasChildren := False;
          finally
            tvTemplate.Items.EndUpdate;
          end;
        end;
      end);
  end;
end;

end.
