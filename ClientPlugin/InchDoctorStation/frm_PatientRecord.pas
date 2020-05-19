{*******************************************************}
{                                                       }
{         ����HCView�ĵ��Ӳ�������  ���ߣ���ͨ          }
{                                                       }
{ �˴������ѧϰ����ʹ�ã�����������ҵĿ�ģ��ɴ�������  }
{ �����ʹ���߳е�������QQȺ 649023932 ����ȡ����ļ��� }
{ ������                                                }
{                                                       }
{*******************************************************}

unit frm_PatientRecord;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frm_Record, Vcl.ExtCtrls,
  Vcl.ComCtrls, emr_Common, Vcl.Menus, HCCustomData, System.ImageList, HCItem,
  Vcl.ImgList, HCEmrElementItem, HCEmrGroupItem, HCDrawItem, HCSection, Vcl.StdCtrls,
  Xml.XMLDoc, Xml.XMLIntf, FireDAC.Comp.Client, System.Generics.Collections, HCEmrView,
  HCCompiler, emr_Compiler, CFPageControl, CFControl, CFSplitter, frm_PatientHisRecord,
  HCSectionData, HCCustomFloatItem;

type
  TXmlStruct = class(TObject)
  private
    FXmlDoc: IXMLDocument;
    FDeGroupNodes: TList;
    FDETable: TFDMemTable;
    FOnlyDeItem: Boolean;  // �Ƿ�ֻ������Ԫ��������ͨ�ı�
  public
    constructor Create;
    destructor Destroy; override;
    procedure TraverseItem(const AData: THCCustomData;
      const AItemNo, ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean);
    property XmlDoc: IXMLDocument read FXmlDoc;
  end;

  TStructDoc = class(TObject)
  strict private
    FPatID: string;
    FDesID: Integer;
    FXmlDoc: IXMLDocument;
  public
    constructor Create(const APatID: string; const ADesID: Integer);
    destructor Destroy; override;
    class function GetDeItemNode(const ADeIndex: string; const AXmlDoc: IXMLDocument): IXmlNode;
    property PatID: string read FPatID write FPatID;
    property DesID: Integer read FDesID write FDesID;
    property XmlDoc: IXMLDocument read FXmlDoc;
  end;

  TfrmPatientRecord = class(TForm)
    tvRecord: TTreeView;
    pmRecord: TPopupMenu;
    mniNew: TMenuItem;
    mniEdit: TMenuItem;
    mniDelete: TMenuItem;
    mniView: TMenuItem;
    mniMergeView: TMenuItem;
    il: TImageList;
    mniN1: TMenuItem;
    mniN2: TMenuItem;
    mniXML: TMenuItem;
    spl1: TCFSplitter;
    pnlRecord: TPanel;
    mniHisRecord: TMenuItem;
    mniReSync: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mniNewClick(Sender: TObject);
    procedure tvRecordExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure tvRecordDblClick(Sender: TObject);
    procedure mniEditClick(Sender: TObject);
    procedure mniViewClick(Sender: TObject);
    procedure mniDeleteClick(Sender: TObject);
    procedure mniMergeViewClick(Sender: TObject);
    procedure pmRecordPopup(Sender: TObject);
    procedure mniN2Click(Sender: TObject);
    procedure mniXMLClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mniHisRecordClick(Sender: TObject);
    procedure mniReSyncClick(Sender: TObject);
  private
    { Private declarations }
    FPatientInfo: TPatientInfo;
    FServerInfo: TServerInfo;
    FSyncDataDesID: Integer;
    FDataElementSetMacro: TFDMemTable;
    FStructDocs: TObjectList<TStructDoc>;
    FCompiler: TEmrCompiler;
    FRecPages: TCFPageControl;
    FFrmHisRecord: TfrmPatientHisRecord;

    procedure ClearRecPages;
    procedure DoSetDeItemText(Sender: TObject; const ADeItem: TDeItem;
      var AText: string; var ACancel: Boolean);
    function DoDeItemPopup(const ADeItem: TDeItem): Boolean;
    procedure DoPrintPreview(Sender: TObject);
    procedure DoTraverseItem(const AData: THCCustomData; const AItemNo, ATags: Integer;
      const ADomainStack: TDomainStack; var AStop: Boolean);
    procedure DoSyntaxCheck(const AData: THCCustomData; const ADomainStack: TDomainStack; const AItemNo: Integer);

    procedure PrepareSyncData(const ADesID: Integer);
    function GetDeValueFromStruct(const APatID: string; ADesID: Integer; const ADeIndex: string): string;
    function GetDeItemNodeFromStructDoc(const APatID: string; const ADesID: Integer; ADeIndex: string): IXMLNode;
    function GetMarcoSqlResult(const AObjID, AMacro: string): string;
    function GetDeItemValueTry(const ADeIndex: string): string;

    /// <summary> ��ȡָ������Ԫͬ����ֵ </summary>
    /// <param name="aDeItem"></param>
    /// <returns></returns>
    function DoDeItemGetSyncValue(const ADesID: Integer; const ADeItem: TDeItem): string;
    procedure DoSyncDeItem(const Sender: TObject; const AData: THCCustomData; const AItem: THCCustomItem);
    procedure SyncDeGroupByStruct(const AEmrView: THCEmrView);

    procedure RefreshRecordNode;
    procedure DoSaveRecordContent(Sender: TObject);
    procedure DoSaveRecordStructure(Sender: TObject);
    procedure DoRecordChangedSwitch(Sender: TObject);
    procedure DoRecordReadOnlySwitch(Sender: TObject);
    function DoRecordCopyRequest(const AFormat: Word): Boolean;
    function DoRecordPasteRequest(const AFormat: Word): Boolean;
    function DoRecordCopyAsStream(const AStream: TStream): Boolean;
    function DoRecordPasteFromStream(const AStream: TStream): Boolean;
    procedure DoImportAsText(const AText: string);

    procedure DoPageButtonClick(const APageIndex: Integer; const AButton: TCFPageButton);
    function GetActiveRecord: TfrmRecord;
    function GetRecordPageIndex(const ARecordID: Integer): Integer;
    function GetPageRecord(const APageIndex: Integer): TfrmRecord;
    procedure NewPageAndRecord(const ARecordInfo: TRecordInfo; var AFrmRecord: TfrmRecord);
    function GetPatientNode: TTreeNode;

    procedure GetPatientRecordListUI;

    /// <summary> ��ȡ����ָ�����ݼ������в��� </summary>
    /// <param name="ADeSetID"></param>
    //procedure LoadPatientDataSetContent(const ADeSetID: Integer);
    /// <summary> ��ȡ����ָ������ </summary>
    procedure LoadPatientRecordContent(const ARecordInfo: TRecordInfo);
    /// <summary> ɾ������ָ������ </summary>
    procedure DeletePatientRecord(const ARecordID: Integer);

    /// <summary> �򿪽ڵ��Ӧ�Ĳ���(�����༭�������أ�������������) </summary>
    //procedure OpenPatientDeSet(const ADeSetID, ARecordID: Integer);

    /// <summary> ����ָ��������Ӧ�Ľڵ� </summary>
    function FindRecordNode(const ARecordID: Integer): TTreeNode;

    /// <summary> �����ĵ����ݽṹ��XML�ļ� </summary>
    procedure SaveStructureToXml(const AFrmRecord: TfrmRecord; const AFileName: string);
    function GetStructureToXml(const AFrmRecord: TfrmRecord): IXMLDocument;
    /// <summary> ��ȡ�����没���ṹ </summary>
    /// <param name="ARecordID"></param>
    /// <param name="AFrmRecord"></param>
    /// <param name="AInsert">True���룬False����</param>
    procedure SaveRecordStructure(const ARecordID: Integer; const AFrmRecord: TfrmRecord; const AInsert: Boolean);
    /// <summary> ��ȡָ�������Ľṹ </summary>
    procedure GetRecordStructure(const ARecordID: Integer; const AFrmRecord: TfrmRecord);
  public
    { Public declarations }
    UserInfo: TUserInfo;
    /// <summary> �ر����в���ҳ </summary>
    function CloseAllRecordPage: Boolean;

    /// <summary> �ر�ָ���Ĳ���ҳ </summary>
    /// <param name="APageIndex">ҳ���</param>
    /// <param name="ASaveChange">�Ƿ񱣴�䶯</param>
    /// <returns>�Ƿ�رճɹ�</returns>
    function CloseRecordPage(const APageIndex: Integer;
      const ASaveChange: Boolean = True): Boolean;
    property PatientInfo: TPatientInfo read FPatientInfo;
  end;

implementation

uses
  DateUtils, HCCommon, HCStyle, HCParaStyle, frm_DM, frm_RecordOverView,
  frm_TemplateList, Data.DB, HCEmrToothItem, HCEmrYueJingItem, HCEmrFangJiaoItem,
  HCRectItem, HCViewData, CFBalloonHint, frm_RecordSet, emr_BLLInvoke;

{$R *.dfm}

procedure TfrmPatientRecord.ClearRecPages;
var
  i: Integer;
begin
  for i := 0 to FRecPages.Count - 1 do
    (FRecPages[i].Control as TForm).Free;

  FRecPages.Clear;
end;

function TfrmPatientRecord.CloseAllRecordPage: Boolean;
begin
  Result := False;
  while FRecPages.Count > 1 do
  begin
    if not CloseRecordPage(FRecPages.Count - 1) then
      Exit;
  end;

  Result := True;
end;

function TfrmPatientRecord.CloseRecordPage(const APageIndex: Integer;
  const ASaveChange: Boolean = True): Boolean;
var
  i: Integer;
  vFrmRecord: TfrmRecord;
begin
  Result := False;

  if (APageIndex >= 0) and (FRecPages[APageIndex].Control is TfrmRecord) then
  begin
    vFrmRecord := (FRecPages[APageIndex].Control as TfrmRecord);
    if ASaveChange and (vFrmRecord.EmrView.IsChanged) then  // ��Ҫ���䶯���ǲ���
    begin
      case MessageDlg('�Ƿ񱣴没�� ' + TRecordInfo(vFrmRecord.ObjectData).RecName + ' ��',
        mtConfirmation, [mbYes, mbNo, mbCancel], 0)
      of
        mrYes: DoSaveRecordContent(vFrmRecord);
        mrCancel: Exit;
      end;
    end;

    (FRecPages[APageIndex].Control as TfrmRecord).Free;
    FRecPages[APageIndex].Control := nil;
    FRecPages.DeletePage(APageIndex);
  end;

  Result := True;
end;

procedure TfrmPatientRecord.DeletePatientRecord(const ARecordID: Integer);
begin
  TBLLInvoke.DeletePatientRecord(ARecordID,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // ����˷�������ִ�в��ɹ�
        raise Exception.Create(ABLLServer.MethodError);
    end);
end;

function TfrmPatientRecord.DoDeItemGetSyncValue(const ADesID: Integer; const ADeItem: TDeItem): string;
var
  vDeIndex: string;
begin
  Result := '';
  vDeIndex := ADeItem[TDeProp.Index];
  if vDeIndex <> '' then
  begin
    if FSyncDataDesID <> ADesID then
      PrepareSyncData(ADesID);

    Result := GetDeItemValueTry(vDeIndex);;
  end;
end;

function TfrmPatientRecord.DoDeItemPopup(const ADeItem: TDeItem): Boolean;
begin
  Result := True;
end;

procedure TfrmPatientRecord.DoImportAsText(const AText: string);
var
  vFrmRecord: TfrmRecord;
begin
  vFrmRecord := GetActiveRecord;
  if Assigned(vFrmRecord) then
    vFrmRecord.EmrView.InsertText(AText)
  else
    ShowMessage('δ���ִ򿪵Ĳ�����');
end;

procedure TfrmPatientRecord.DoPageButtonClick(const APageIndex: Integer;
  const AButton: TCFPageButton);
begin
  if (FRecPages[APageIndex].Control is TfrmRecord) then
    CloseRecordPage(APageIndex);
end;

procedure TfrmPatientRecord.DoPrintPreview(Sender: TObject);
var
  vFrmRecordSet: TfrmRecordSet;
begin
  vFrmRecordSet := TfrmRecordSet.Create(nil);
  try
    vFrmRecordSet.ShowDialog(FPatientInfo.PatID, FPatientInfo.VisitID,
      TRecordInfo(Self.GetActiveRecord.ObjectData).ID);
  finally
    FreeAndNil(vFrmRecordSet);
  end;
end;

procedure TfrmPatientRecord.DoRecordChangedSwitch(Sender: TObject);
var
  vText: string;
begin
  if (Sender is TfrmRecord) then
  begin
    if (Sender as TfrmRecord).Parent is TTabSheet then
    begin
      if (Sender as TfrmRecord).EmrView.IsChanged then
        vText := TRecordInfo((Sender as TfrmRecord).ObjectData).RecName + '*'
      else
        vText := TRecordInfo((Sender as TfrmRecord).ObjectData).RecName;

      ((Sender as TfrmRecord).Parent as TTabSheet).Caption := vText;
    end;
  end;
end;

function TfrmPatientRecord.DoRecordCopyAsStream(const AStream: TStream): Boolean;
begin
  if not ClientCache.ServerParam.PasteDifferent then  // ������ͬ����֮��ճ������
    HCSaveTextToStream(AStream, FPatientInfo.PatID);  // д�뻼��PatID
end;

function TfrmPatientRecord.DoRecordCopyRequest(const AFormat: Word): Boolean;
begin
  Result := False;

  if AFormat = HC_FILEFORMAT then  // ����ΪHC��ʽ
    Result := True
  else  // ���Ǹ���ΪHC��ʽ
  if ClientCache.ServerParam.PasteOutside then  // �����Ƶ�����
    Result := True;
end;

function TfrmPatientRecord.DoRecordPasteFromStream(const AStream: TStream): Boolean;
var
  vPatID: string;
begin
  Result := False;
  if not ClientCache.ServerParam.PasteDifferent then  // ������ͬ����֮��ճ������
  begin
    HCLoadTextFromStream(AStream, vPatID, HC_FileVersionInt);
    if vPatID = FPatientInfo.PatID then
      Result := True
    else
      ShowMessage('��Ҫճ����������Դ���������ߣ���ǰϵͳ��ֹճ����ͬ����֮��Ĳ�����');
  end
  else
    Result := True;
end;

function TfrmPatientRecord.DoRecordPasteRequest(const AFormat: Word): Boolean;
begin
  Result := False;

  if AFormat = HC_FILEFORMAT then  // ճ��HC��ʽ
    Result := True  // ������������Դ���ĸ����ߵ�������DoPasteDataBefor���ж�
  else  // ճ����HC��ʽ
  if ClientCache.ServerParam.PasteDifferent then  // ����ͬ����֮��ճ������
    Result := True;
end;

procedure TfrmPatientRecord.DoRecordReadOnlySwitch(Sender: TObject);
begin
  if (Sender is TfrmRecord) then
  begin
    if (Sender as TfrmRecord).Parent is TTabSheet then
    begin
      if (Sender as TfrmRecord).EmrView.ActiveSection.Page.ReadOnly then
        ((Sender as TfrmRecord).Parent as TTabSheet).ImageIndex := 1
      else
        ((Sender as TfrmRecord).Parent as TTabSheet).ImageIndex := 0;
    end;
  end;
end;

procedure TfrmPatientRecord.DoSaveRecordContent(Sender: TObject);
var
  vSM: TMemoryStream;
  vRecordInfo: TRecordInfo;
  vFrmRecord: TfrmRecord;
begin
  vFrmRecord := Sender as TfrmRecord;

  if not vFrmRecord.EmrView.IsChanged then
  begin
    if MessageDlg('δ�����仯��ȷ��Ҫִ�б��棿',
      TMsgDlgType.mtError, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes
    then
      Exit;
  end;

  vRecordInfo := TRecordInfo(vFrmRecord.ObjectData);

  if vFrmRecord.EmrView.Trace then
  begin
    FServerInfo.DateTime := TBLLInvoke.GetServerDateTime;
    vFrmRecord.TraverseElement(DoTraverseItem, [saPage], TTravTag.WriteTraceInfo or TTravTag.HideTrace);  // ����ĵ��ʿء��ۼ�������
  end;

  vSM := TMemoryStream.Create;
  try
    vFrmRecord.EmrView.SaveToStream(vSM);

    if vRecordInfo.ID > 0 then  // �޸ĺ󱣴�
    begin
      BLLServerExec(procedure(const ABLLServerReady: TBLLServerProxy)
      begin
        ABLLServerReady.Cmd := BLL_SAVERECORDCONTENT;  // ����ָ����סԺ����
        ABLLServerReady.ExecParam.I['RID'] := vRecordInfo.ID;
        ABLLServerReady.ExecParam.S['LastUserID'] := UserInfo.ID;  // ��������
        ABLLServerReady.ExecParam.ForcePathObject('content').LoadBinaryFromStream(vSM);
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      begin
        if ABLLServer.MethodRunOk then  // ����˷�������ִ�гɹ�
        begin
          //if CompareDateTime(vRecordInfo.LastDT, EncodeDate(2019, 5, 7)) < 0 then  // ֮ǰû�д没���ṹ���ȱ��� 201905081300
          //  SaveRecordStructure(vRecordInfo.ID, vFrmRecord, True)
          //else
            SaveRecordStructure(vRecordInfo.ID, vFrmRecord, False);  // ��ȡ�����没���ṹ

          vFrmRecord.EmrView.IsChanged := False;
          BalloonMessage('���没�� ' + vRecordInfo.RecName + ' �ɹ���');
        end
        else
          ShowMessage('���没��ʧ�ܣ������ԣ�' + #13#10 + ABLLServer.MethodError);
      end);
    end
    else  // �����½��Ĳ���
    begin
      HintFormShow('���ڱ��没��...', procedure(const AUpdateHint: TUpdateHint)
      begin
        BLLServerExec(procedure(const ABLLServerReady: TBLLServerProxy)
        begin
          ABLLServerReady.Cmd := BLL_NEWINCHRECORD;  // �����½�����
          ABLLServerReady.ExecParam.S['PatID'] := FPatientInfo.PatID;
          ABLLServerReady.ExecParam.I['VisitID'] := FPatientInfo.VisitID;
          ABLLServerReady.ExecParam.I['desid'] := vRecordInfo.DesID;
          ABLLServerReady.ExecParam.S['Name'] := vRecordInfo.RecName;
          ABLLServerReady.ExecParam.DT['DT'] := vRecordInfo.DT;
          ABLLServerReady.ExecParam.I['DeptID'] := FPatientInfo.DeptID;
          ABLLServerReady.ExecParam.S['CreateUserID'] := UserInfo.ID;
          ABLLServerReady.ExecParam.ForcePathObject('Content').LoadBinaryFromStream(vSM);
          //
          ABLLServerReady.AddBackField('RecordID');  // �õ������Ĳ���ID
        end,
        procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
        begin
          if ABLLServer.MethodRunOk then  // ����˷�������ִ�гɹ�
          begin
            if ABLLServer.BackField('RecordID').AsInteger > 0 then
            begin
              vRecordInfo.ID := ABLLServer.BackField('RecordID').AsInteger;
              SaveRecordStructure(vRecordInfo.ID, vFrmRecord, True);  // ��ȡ�����没���ṹ

              vFrmRecord.EmrView.IsChanged := False;
              GetPatientRecordListUI;
              tvRecord.Selected := FindRecordNode(vRecordInfo.ID);
              BalloonMessage('���没�� ' + vRecordInfo.RecName + ' �ɹ���');
            end
            else
              ShowMessage('���没��ʧ�ܣ��õ��Ĳ���IDΪ0��');
          end
          else
            ShowMessage('���没��ʧ�ܣ������ԣ�' + #13#10 + ABLLServer.MethodError);
        end);
      end);
    end;

    // ��������ΪͼƬ
    //vFrmRecord.SaveToImage('c:\', IntToStr(vRecordInfo.ID) + '_' + IntToStr(vRecordInfo.DesID));
  finally
    FreeAndNil(vSM);
  end;
end;

procedure TfrmPatientRecord.DoSaveRecordStructure(Sender: TObject);
var
  vRecordInfo: TRecordInfo;
  vFrmRecord: TfrmRecord;
begin
  vFrmRecord := Sender as TfrmRecord;

  if not vFrmRecord.EmrView.IsChanged then
  begin
    if MessageDlg('δ�����仯��ȷ��Ҫ���²����ṹ���ݣ�',
      TMsgDlgType.mtError, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes
    then
      Exit;
  end;

  vRecordInfo := TRecordInfo(vFrmRecord.ObjectData);

  SaveRecordStructure(vRecordInfo.ID, vFrmRecord, False);  // ���²����ṹ����
  BalloonMessage('���²��� ' + vRecordInfo.RecName + ' �ṹ�ɹ���');
end;

procedure TfrmPatientRecord.DoSetDeItemText(Sender: TObject; const ADeItem: TDeItem;
  var AText: string; var ACancel: Boolean);
var
  vBLLSrvProxy: TBLLServerProxy;
  vClassType, i: Integer;
  vScript: string;
begin
  vScript := '';  // 'begin Text := RecordInfo.RecName; end.';
  vBLLSrvProxy := TBLLInvoke.GetBLLServerProxy;
  try
    vBLLSrvProxy.Cmd := BLL_GetDataElementScript;  // ��ȡ����Ԫ�ű�
    vBLLSrvProxy.ExecParam.S['DEID'] := ADeItem[TDeProp.Index];
    vBLLSrvProxy.AddBackField('Pascal');
    if vBLLSrvProxy.DispatchPack then  // �������Ӧ�ɹ�
      vScript := vBLLSrvProxy.BackField('Pascal').AsString;
  finally
    FreeAndNil(vBLLSrvProxy);
  end;

  if Trim(vScript) = '' then Exit;  // ��Ч�ű�

  FCompiler.ResetRegister;
  FCompiler.RegClassVariable(@ADeItem, @FPatientInfo,
    @TRecordInfo((Sender as TfrmRecord).Objectdata), @AText);

  if not FCompiler.RunScript(vScript) then
  begin
    vScript := '��ǰ����Ԫ�п��ƽű��������д���ԭ��';
    for i := 0 to FCompiler.ErrorCount - 1 do
      vScript := vScript + #13#10 + FCompiler.ErrorMessage[i];

    ShowMessage(vScript);
  end;
end;

procedure TfrmPatientRecord.DoSyncDeItem(const Sender: TObject;
  const AData: THCCustomData; const AItem: THCCustomItem);
var
  vDeItem: TDeItem;
  vDeIndex, vsResult: string;
begin
  if AItem is TDeItem then
  begin
    vDeItem := AItem as TDeItem;
    if vDeItem.IsElement then
    begin
      vDeIndex := vDeItem[TDeProp.Index];
      if vDeIndex <> '' then  // ������Ԫ
      begin
        vsResult := GetDeItemValueTry(vDeIndex);
        if vsResult <> '' then
          vDeItem.Text := vsResult;
      end;
    end;
  end
  else
  if AItem is TDeEdit then
  begin
    vDeIndex := (AItem as TDeEdit)[TDeProp.Index];
    if vDeIndex <> '' then  // ������Ԫ
    begin
      vsResult := GetDeItemValueTry(vDeIndex);
      if vsResult <> '' then
        (AItem as TDeEdit).Text := vsResult;
    end;
  end
  else
  if AItem is TDeCombobox then
  begin
    vDeIndex := (AItem as TDeCombobox)[TDeProp.Index];
    if vDeIndex <> '' then  // ������Ԫ
    begin
      vsResult := GetDeItemValueTry(vDeIndex);
      if vsResult <> '' then
        (AItem as TDeCombobox).Text := vsResult;
    end;
  end
  else
  if AItem is TDeFloatBarCodeItem then
  begin
    vDeIndex := (AItem as TDeFloatBarCodeItem)[TDeProp.Index];
    if vDeIndex <> '' then  // ������Ԫ
    begin
      vsResult := GetDeItemValueTry(vDeIndex);
      if vsResult <> '' then
        (AItem as TDeFloatBarCodeItem).Text := vsResult;
    end;
  end
  else
  if AItem is TDeImageItem then
  begin
    vDeIndex := (AItem as TDeImageItem)[TDeProp.Index];
    if vDeIndex <> '' then  // ������Ԫ
    begin
      //����vDeIndex��ֵ��ͬ��ͼƬ
      // (AItem as TDeImageItem).LoadFromBmpFile('');
    end;
  end;
end;

procedure TfrmPatientRecord.DoSyntaxCheck(const AData: THCCustomData;
  const ADomainStack: TDomainStack; const AItemNo: Integer);
var
  vDeItem: TDeItem;
  vText, vDeIndex, vKey: string;
  vPos, vPosBase: Integer;
  vDomainInfo: THCDomainInfo;
begin
  vDeItem := AData.Items[AItemNo] as TDeItem;
  vDeItem.SyntaxClear;
  vText := vDeItem.Text;

  if FPatientInfo.Sex = '��' then
  begin
    vDeIndex := '';
    if ADomainStack.Count > 0 then
    begin
      vDomainInfo := ADomainStack.Peek;
      vDeIndex := (vDomainInfo.Data.Items[vDomainInfo.BeginNo] as TDeGroup)[TDeProp.Index]
    end;

    vKey := '�ӹ�';

    //if vDeIndex = '4' then  // ���Դ�����ָ����������﷨���
    //begin

    //end;

    //
    vPosBase := 0;
    vPos := Pos(vKey, vText);
    while vPos > 0 do
    begin
      vDeItem.SyntaxAdd(vPos + vPosBase, vKey.Length, TEmrSyntaxProblem.espContradiction);
      vPosBase := vPos + vPosBase + vKey.Length - 1;
      System.Delete(vText, 1, vPos + vKey.Length - 1);
      vPos := Pos(vKey, vText);
    end;
  end;
end;

procedure TfrmPatientRecord.DoTraverseItem(const AData: THCCustomData;
  const AItemNo, ATags: Integer; const ADomainStack: TDomainStack; var AStop: Boolean);
var
  vDeItem: TDeItem;
begin
  if (not (AData.Items[AItemNo] is TDeItem))
    //or (not (AData.Items[AItemNo] is TDeGroup))
  then
    Exit;  // ֻ��Ԫ����Ч

  vDeItem := AData.Items[AItemNo] as TDeItem;

  if TTravTag.Contains(ATags, TTravTag.WriteTraceInfo) then // ����Ԫ������
  begin
    case vDeItem.StyleEx of
      cseNone: vDeItem[TDeProp.Trace] := '';

      cseDel:
        begin
          if vDeItem[TDeProp.Trace] = '' then  // �ºۼ�
            vDeItem[TDeProp.Trace] := UserInfo.Name + '(' + UserInfo.ID + ') ɾ�� ' + FormatDateTime('YYYY-MM-DD HH:mm:SS', FServerInfo.DateTime);
        end;

      cseAdd:
        begin
          if vDeItem[TDeProp.Trace] = '' then  // �ºۼ�
            vDeItem[TDeProp.Trace] := UserInfo.Name + '(' + UserInfo.ID + ') ��� ' + FormatDateTime('YYYY-MM-DD HH:mm:SS', FServerInfo.DateTime);
        end;
    end;
  end;
end;

function TfrmPatientRecord.FindRecordNode(const ARecordID: Integer): TTreeNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to tvRecord.Items.Count - 1 do
  begin
    if TreeNodeIsRecord(tvRecord.Items[i]) then
    begin
      if ARecordID = TRecordInfo(tvRecord.Items[i].Data).ID then
      begin
        Result := tvRecord.Items[i];
        Break;
      end;
    end;
  end;
end;

procedure TfrmPatientRecord.FormCreate(Sender: TObject);
begin
  //SetWindowLong(Handle, GWL_EXSTYLE, (GetWindowLong(handle, GWL_EXSTYLE) or WS_EX_APPWINDOW));
  FPatientInfo := TPatientInfo.Create;
  FServerInfo := TServerInfo.Create;
  FStructDocs := TObjectList<TStructDoc>.Create;
  FSyncDataDesID := -1;
  FDataElementSetMacro := TFDMemTable.Create(nil);
  FCompiler := TEmrCompiler.CreateByScriptType(nil);

  pnlRecord.Color := GBackColor;

  FRecPages := TCFPageControl.Create(nil);
  FRecPages.PageHeight := 28;
  FRecPages.Images := il;
  FRecPages.Align := alTop;
  FRecPages.BorderVisible := False;
  FRecPages.Color := GBackColor;
  FRecPages.ActivePageColor := GDownColor;
  FRecPages.Parent := pnlRecord;
  FRecPages.OnPageButtonClick := DoPageButtonClick;
end;

procedure TfrmPatientRecord.FormDestroy(Sender: TObject);
begin
  ClearRecPages;

  if Assigned(FFrmHisRecord) then
    FreeAndNil(FFrmHisRecord);

  FreeAndNil(FPatientInfo);
  FreeAndNil(FServerInfo);
  FreeAndNil(FStructDocs);
  FreeAndNil(FDataElementSetMacro);
  FreeAndNil(FCompiler);
end;

procedure TfrmPatientRecord.FormShow(Sender: TObject);
var
  vPage: TCFPage;
  vFrmRecOverView: TfrmRecordOverView;
begin
  vFrmRecOverView := TfrmRecordOverView.Create(nil);
  vFrmRecOverView.BorderStyle := bsNone;
  vFrmRecOverView.Align := alClient;
  vFrmRecOverView.Parent := pnlRecord;

  FRecPages.BeginUpdate;
  try
    vPage := FRecPages.AddPage('��������', vFrmRecOverView);
    vPage.ImageIndex := 4;
  finally
    FRecPages.EndUpdate;
  end;

  FRecPages.BackGroundText := FPatientInfo.BedNo + '����' + FPatientInfo.Name + '��'
    + FPatientInfo.Sex + '��' + FPatientInfo.Age + '�꣬'// + FPatientInfo.PatID.ToString + '��'
    + '��' + FPatientInfo.VisitID.ToString + '��סԺ��'
    + FormatDateTime('YYYY-MM-DD HH:mm', FPatientInfo.InDeptDateTime) + '��ƣ�'
    + FPatientInfo.CareLevel.ToString + '������';

  GetPatientRecordListUI;
end;

function TfrmPatientRecord.GetActiveRecord: TfrmRecord;
begin
  if FRecPages.ActivePage.Control is TfrmRecord then
    Result := FRecPages.ActivePage.Control as TfrmRecord
  else
    Result := nil;
end;

function TfrmPatientRecord.GetDeItemNodeFromStructDoc(const APatID: string;
  const ADesID: Integer; ADeIndex: string): IXMLNode;
var
  i: Integer;
  vStructDoc: TStructDoc;
  vXmlDoc: IXMLDocument;
  vStream: TMemoryStream;
begin
  Result := nil;
  vXmlDoc := nil;
  for i := 0 to FStructDocs.Count - 1 do
  begin
    if (FStructDocs[i].PatID = APatID) and (FStructDocs[i].DesID = ADesID) then
    begin
      vXmlDoc := FStructDocs[i].XmlDoc;
      Break;
    end;
  end;

  if not Assigned(vXmlDoc) then
  begin
    vStream := TMemoryStream.Create;
    try
      BLLServerExec(procedure(const ABLLServerReady: TBLLServerProxy)
      begin
        ABLLServerReady.Cmd := BLL_GetPatDesStructure;  // ��ȡָ������ָ�����ݼ��Ĳ����ṹ����
        ABLLServerReady.ExecParam.S['Patient_ID'] := APatID;
        ABLLServerReady.ExecParam.I['DesID'] := ADesID;  // ���ݼ�ID
        ABLLServerReady.AddBackField('structure');
      end,
      procedure(const ABLLServerRun: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      begin
        if not ABLLServerRun.MethodRunOk then  // ����˷�������ִ�в��ɹ�
          raise Exception.Create(ABLLServerRun.MethodError);

        ABLLServerRun.BackField('structure').SaveBinaryToStream(vStream);

        if vStream.Size > 0 then
        begin
          vStream.Position := 0;

          vStructDoc := TStructDoc.Create(APatID, ADesID);
          vStructDoc.XmlDoc.LoadFromStream(vStream);
          vXmlDoc := vStructDoc.XmlDoc;
          //vXmlDoc.SaveToFile('x.xml');
        end;
      end);
    finally
      FreeAndNil(vStream);
    end;
  end;

  if Assigned(vXmlDoc) then
    Result := TStructDoc.GetDeItemNode(ADeIndex, vXmlDoc);
end;

function TfrmPatientRecord.GetDeItemValueTry(const ADeIndex: string): string;
begin
  Result := '';
  FDataElementSetMacro.Filtered := False;
  FDataElementSetMacro.Filter := 'ObjID = ' + ADeIndex;
  FDataElementSetMacro.Filtered := True;
  if FDataElementSetMacro.RecordCount = 1 then  // �д�����Ԫ���滻��Ϣ
  begin
    case FDataElementSetMacro.FieldByName('MacroType').AsInteger of
      1:  // ������Ϣ(�ͻ��˴���)
        Result := GetValueAsString(FPatientInfo.FieldByName(FDataElementSetMacro.FieldByName('Macro').AsString));

      2:  // �û���Ϣ(�ͻ��˴���)
        Result := GetValueAsString(UserInfo.FieldByName(FDataElementSetMacro.FieldByName('Macro').AsString));

      3: // ������Ϣ(����˴���)
        Result := GetDeValueFromStruct(FPatientInfo.PatID, FDataElementSetMacro.FieldByName('Macro').AsInteger, ADeIndex);

      4:  // ������Ϣ(����ˣ��統ǰʱ���)
        Result := GetValueAsString(FServerInfo.FieldByName(FDataElementSetMacro.FieldByName('Macro').AsString));

      5:  // SQL�ű�(����˴���)
        Result := GetMarcoSqlResult(ADeIndex, FDataElementSetMacro.FieldByName('Macro').AsString);
    end;
  end;
end;

function TfrmPatientRecord.GetDeValueFromStruct(const APatID: string;
  ADesID: Integer; const ADeIndex: string): string;
var
  vXmlNode: IXMLNode;
begin
  Result := '';
  vXmlNode := GetDeItemNodeFromStructDoc(APatID, ADesID, ADeIndex);

  if Assigned(vXmlNode) then
  begin
    if vXmlNode.Text <> '' then
      Result := vXmlNode.Text;
  end;
end;

function TfrmPatientRecord.GetMarcoSqlResult(const AObjID,
  AMacro: string): string;
var
  vSqlResult: string;
  vQuery: TFDQuery;
  i: Integer;
begin
  Result := '';
  vSqlResult := AMacro;

  vQuery := TFDQuery.Create(nil);
  try
    vQuery.SQL.Text := AMacro;
    //if (vQuery.FieldList.Count > 0) or (vQuery.FieldDefs.Count > 0) or (vQuery.FieldDefList.Count > 0) or (vQuery.Fields.Count > 0) then
    if vQuery.Params.Count > 0 then  // ���ֶβ���
    begin
      for i := 0 to vQuery.Params.Count - 1 do
      begin
        if Pos('patientinfo', LowerCase(vQuery.Params[i].Name)) > 0 then
        begin
          vSqlResult := StringReplace(vSqlResult, ':' + vQuery.Params[i].Name,
            GetValueAsString(FPatientInfo.FieldByName(
              Copy(vQuery.Params[i].Name, Pos('.', vQuery.Params[i].Name) + 1, 20) ) ), [rfReplaceAll, rfIgnoreCase]);
        end
        else
        if Pos('userinfo', LowerCase(vQuery.Params[i].Name)) > 0 then
        begin
          vSqlResult := StringReplace(vSqlResult, ':' + vQuery.Params[i].Name,
            GetValueAsString(UserInfo.FieldByName(
              Copy(vQuery.Params[i].Name, Pos('.', vQuery.Params[i].Name) + 1, 20) ) ), [rfReplaceAll, rfIgnoreCase]);
        end
        else
        if Pos('serverinfo', LowerCase(vQuery.Params[i].Name)) > 0 then
        begin
          vSqlResult := StringReplace(vSqlResult, ':' + vQuery.Params[i].Name,
            GetValueAsString(FServerInfo.FieldByName(
              Copy(vQuery.Params[i].Name, Pos('.', vQuery.Params[i].Name) + 1, 20) ) ), [rfReplaceAll, rfIgnoreCase]);
        end;
      end;
    end;
  finally
    FreeAndNil(vQuery);
  end;

  BLLServerExec(procedure(const ABLLServerReady: TBLLServerProxy)
  begin
    ABLLServerReady.Cmd := BLL_EXECSQL;  // ��ȡָ���û�����Ϣ
    ABLLServerReady.ReplaceParam.S['Sql'] := vSqlResult;
    ABLLServerReady.AddBackField('value');
  end,
  procedure(const ABLLServerRun: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
  begin
    if not ABLLServerRun.MethodRunOk then
      raise Exception.Create(ABLLServerRun.MethodError);

    vSqlResult := ABLLServerRun.BackField('value').AsString;
  end);

  if vSqlResult <> '' then
    Result := vSqlResult;
end;

procedure TfrmPatientRecord.GetPatientRecordListUI;
var
  vPatNode: TTreeNode;
begin
  RefreshRecordNode;  // ������нڵ㣬Ȼ����ӱ���סԺ��Ϣ�ڵ�

  vPatNode := GetPatientNode;

  TBLLInvoke.GetPatientInchRecord(FPatientInfo.PatID, FPatientInfo.VisitID,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    var
      vRecordInfo: TRecordInfo;
      vRecordDataSetInfo: TRecordDataSetInfo;
      vDesPID: Integer;
      vNode, vRecNode: TTreeNode;
    begin
      if not ABLLServer.MethodRunOk then  // ����˷�������ִ�в��ɹ�
      begin
        ShowMessage(ABLLServer.MethodError);
        Exit;
      end;

      vDesPID := 0;
      vNode := vPatNode;
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

                  vNode := tvRecord.Items.AddChildObject(vPatNode,
                    ClientCache.GetDataSetInfo(vDesPID).GroupName, vRecordDataSetInfo);

                  vNode.ImageIndex := -1;
                  vNode.SelectedIndex := -1;
                end;

                vRecordInfo := TRecordInfo.Create;
                vRecordInfo.ID := FieldByName('ID').AsInteger;
                vRecordInfo.DesID := FieldByName('desID').AsInteger;
                vRecordInfo.RecName := FieldByName('Name').AsString;
                vRecordInfo.LastDT := FieldByName('LastDT').AsDateTime;  // �����ṹ�洢ҵ����ȫ���ߺ����ȥ�� 201905081300

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
end;

function TfrmPatientRecord.GetRecordPageIndex(const ARecordID: Integer): Integer;
var
  i: Integer;
  vFrmRecord: TfrmRecord;
begin
  Result := -1;
  for i := 0 to FRecPages.Count - 1 do
  begin
    if FRecPages[i].Control is TfrmRecord then
    begin
      vFrmRecord := FRecPages[i].Control as TfrmRecord;
      if TRecordInfo(vFrmRecord.ObjectData).ID = ARecordID then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

procedure TfrmPatientRecord.GetRecordStructure(const ARecordID: Integer;
  const AFrmRecord: TfrmRecord);
var
  vXmlDoc: IXMLDocument;
begin
  HintFormShow('���ڻ�ȡ�����ṹ...', procedure(const AUpdateHint: TUpdateHint)
  begin
    BLLServerExec(procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GetRECORDSTRUCTURE;  // ��ȡ�����ṹ
      ABLLServerReady.ExecParam.I['RecordID'] := ARecordID;
      ABLLServerReady.AddBackField('Structure');
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if ABLLServer.MethodRunOk then  // ����˷�������ִ�гɹ�
        ABLLServer.BackField('Structure').AsString
      else
        ShowMessage('��ȡ�����ṹʧ�ܣ�ԭ��' + #13#10 + ABLLServer.MethodError);
    end);
  end);
end;

function TfrmPatientRecord.GetStructureToXml(const AFrmRecord: TfrmRecord): IXMLDocument;
var
  vItemTraverse: THCItemTraverse;
  vXmlStruct: TXmlStruct;
begin
  Result := nil;
  vItemTraverse := THCItemTraverse.Create;
  try
    //vItemTraverse.Tag := TTraverse.Normal;
    vXmlStruct := TXmlStruct.Create;
    try
      vItemTraverse.Areas := [TSectionArea.saPage];
      vItemTraverse.Process := vXmlStruct.TraverseItem;

      vXmlStruct.XmlDoc.DocumentElement.Attributes['DesID'] := TRecordInfo(AFrmRecord.ObjectData).DesID;
      vXmlStruct.XmlDoc.DocumentElement.Attributes['DocName'] := TRecordInfo(AFrmRecord.ObjectData).RecName;

      AFrmRecord.EmrView.TraverseItem(vItemTraverse);

      Result := vXmlStruct.XmlDoc;
    finally
      vXmlStruct.Free;
    end;
  finally
    vItemTraverse.Free;
  end;
end;

{procedure TfrmPatientRecord.LoadPatientDataSetContent(const ADeSetID: Integer);
var
  vFrmRecord: TfrmRecord;
  vSM: TMemoryStream;
  vPage: TCFPage;
  vPageButton: TCFPageButton;
  vIndex: Integer;
begin
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDESETRECORDCONTENT;  // ��ȡָ���������ݼ�(��Ŀ¼)��Ӧ�Ĳ�������
      ABLLServerReady.ExecParam.S['PatID'] := FPatientInfo.PatID;
      ABLLServerReady.ExecParam.I['VisitID'] := FPatientInfo.VisitID;
      ABLLServerReady.ExecParam.I['pid'] := ADeSetID;
      ABLLServerReady.BackDataSet := True;  // ���߷����Ҫ����ѯ���ݼ��������
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
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
          vIndex := 0;

          vFrmRecord := TfrmRecord.Create(nil);  // �����༭��
          vFrmRecord.OnReadOnlySwitch := DoRecordReadOnlySwitch;
          vFrmRecord.Align := alClient;
          vFrmRecord.Parent := Self;

          vPage := FRecPages.AddPage('���̼�¼', vFrmRecord);
          vPage.ImageIndex := 1;
          vPageButton := vPage.AddButton;
          vPageButton.ImageIndex := 5;
          vPageButton.HotImageIndex := 6;
          vPageButton.DownImageIndex := 7;

          vFrmRecord.EmrView.BeginUpdate;
          try
            vSM := TMemoryStream.Create;
            try
              with AMemTable do
              begin
                First;
                while not Eof do
                begin
                  vSM.Clear;

                  (AMemTable.FieldByName('content') as TBlobField).SaveToStream(vSM);
                  if vSM.Size > 0 then
                  begin
                    if vIndex > 0 then  // �ӵڶ�����������ǰһ�����滻���ٲ���
                    begin
                      vFrmRecord.EmrView.ActiveSection.ActiveData.SelectLastItemAfterWithCaret;
                      vFrmRecord.EmrView.InsertBreak;
                      vFrmRecord.EmrView.ApplyParaAlignHorz(TParaAlignHorz.pahLeft);
                    end;

                    vFrmRecord.EmrView.InsertStream(vSM);  // ��������

                    Inc(vIndex);
                  end;

                  Next;
                end;
              end;
            finally
              vSM.Free;
            end;
          finally
            vFrmRecord.EmrView.EndUpdate;
          end;

          vFrmRecord.Show;
        end
        else
          ShowMessage('û�в��̲�����');
      end;
    end);
end;}

procedure TfrmPatientRecord.LoadPatientRecordContent(const ARecordInfo: TRecordInfo);
var
  vSM: TMemoryStream;
  vFrmRecord: TfrmRecord;
  vPageIndex: Integer;
begin
  vSM := TMemoryStream.Create;
  try
    TBLLInvoke.GetRecordContent(ARecordInfo.ID, vSM);
    if vSM.Size > 0 then
    begin
      NewPageAndRecord(ARecordInfo, vFrmRecord);

      try
        ClientCache.GetDataSetElement(ARecordInfo.DesID);
        vFrmRecord.EmrView.LoadFromStream(vSM);
        vFrmRecord.EmrView.ReadOnly := True;
        vFrmRecord.Show;
      except
        on E: Exception do
        begin
          vPageIndex := GetRecordPageIndex(ARecordInfo.ID);
          if vPageIndex >= 0 then
            CloseRecordPage(vPageIndex);

          ShowMessage('���󣺴򿪲���ʱ����' + E.Message);
        end;
      end;
    end;
  finally
    vSM.Free;
  end;
end;

procedure TfrmPatientRecord.mniDeleteClick(Sender: TObject);
var
  vDesPID, vDesID, vRecordID, vPageIndex: Integer;
begin
  if not TreeNodeIsRecord(tvRecord.Selected) then Exit;  // ���ǲ����ڵ�

  GetNodeRecordInfo(tvRecord.Selected, vDesPID, vDesID, vRecordID);

  if vRecordID > 0 then  // ��Ч�Ĳ���
  begin
    if MessageDlg('ɾ������ ' + tvRecord.Selected.Text + ' ��',
      mtWarning, [mbYes, mbNo], 0) = mrYes
    then
    begin
      vPageIndex := GetRecordPageIndex(vRecordID);
      if vPageIndex >= 0 then  // ����
        CloseRecordPage(vPageIndex, False);

      DeletePatientRecord(vRecordID);  // �ύ�����

      tvRecord.Items.Delete(tvRecord.Selected);
    end;
  end;
end;

procedure TfrmPatientRecord.mniEditClick(Sender: TObject);
var
  i, vDesPID, vDesID, vRecordID, vPageIndex: Integer;
  vFrmRecord: TfrmRecord;
begin
  if not TreeNodeIsRecord(tvRecord.Selected) then Exit;  // ���ǲ����ڵ�

  GetNodeRecordInfo(tvRecord.Selected, vDesPID, vDesID, vRecordID);  // ȡ�ڵ���Ϣ

  if vRecordID > 0 then
  begin
    vPageIndex := GetRecordPageIndex(vRecordID);
    if vPageIndex < 0 then  // û��
    begin
      LoadPatientRecordContent(TRecordInfo(tvRecord.Selected.Data));  // ��������
      vPageIndex := GetRecordPageIndex(vRecordID);
    end
    else  // �Ѿ������л���
      FRecPages.PageIndex := vPageIndex;

    // �л���д����
    vFrmRecord := GetPageRecord(vPageIndex);

    { to do: ��ѯ�ò����ǲ����ɱ����ڱ༭ BLL_GetInRecordLock}

    { to do: ��Ӳ���������Ϣ BLL_NewLockInRecord }

    vFrmRecord.EmrView.ReadOnly := False;
    // ����2�п�ʵ��ֻ�����Ŀ��޸�
    //vFrmRecord.EmrView.ActiveSection.Header.ReadOnly := True;
    //vFrmRecord.EmrView.ActiveSection.Footer.ReadOnly := True;
    vFrmRecord.EmrView.UpdateView;

    try
      vFrmRecord.EmrView.Trace := TBLLInvoke.GetInchRecordSignature(vRecordID);
      if vFrmRecord.EmrView.Trace then
      begin
        //vfrmRecordEdit.EmrView.ShowAnnotation := True;
        ShowMessage('�����Ѿ�ǩ�����������޸Ľ������޸ĺۼ���');
      end;
    except
      vFrmRecord.EmrView.ReadOnly := True;  // ��ȡʧ�����л�Ϊֻ��
    end;
  end;
end;

procedure TfrmPatientRecord.mniHisRecordClick(Sender: TObject);
begin
  if not Assigned(FFrmHisRecord) then
  begin
    FFrmHisRecord := TfrmPatientHisRecord.Create(nil);
    FFrmHisRecord.PatientInfo := FPatientInfo;
    FFrmHisRecord.OnImportAsText := DoImportAsText;
    FFrmHisRecord.PopupParent := Self;
  end;

  FFrmHisRecord.Show;
end;

function TfrmPatientRecord.GetPageRecord(const APageIndex: Integer): TfrmRecord;
begin
  Result := FRecPages[APageIndex].Control as TfrmRecord;
end;

function TfrmPatientRecord.GetPatientNode: TTreeNode;
begin
  Result := tvRecord.Items[0];
end;

procedure TfrmPatientRecord.mniN2Click(Sender: TObject);
var
  vDesPID, vDesID, vRecordID, vPageIndex: Integer;
  vFrmRecord: TfrmRecord;
begin
  if not TreeNodeIsRecord(tvRecord.Selected) then Exit;  // ���ǲ����ڵ�

  GetNodeRecordInfo(tvRecord.Selected, vDesPID, vDesID, vRecordID);

  if vRecordID > 0 then
  begin
    if TBLLInvoke.SignatureInchRecord(vRecordID, UserInfo.ID) then
      ShowMessage(UserInfo.Name + '��ǩ���ɹ����������޸Ľ������޸ĺۼ�');

    vPageIndex := GetRecordPageIndex(vRecordID);
    if vPageIndex >= 0 then  // ���ˣ����л������ۼ�
    begin
      vFrmRecord := GetPageRecord(vPageIndex);
      vFrmRecord.EmrView.Trace := True;
    end;
  end;
end;

procedure TfrmPatientRecord.mniNewClick(Sender: TObject);
var
  vFrmRecord: TfrmRecord;
  //vOpenDlg: TOpenDialog;
  vFrmTempList: TfrmTemplateList;
  vTemplateID, vPageIndex: Integer;
  vSM: TMemoryStream;
  vRecordInfo: TRecordInfo;
begin
  // ѡ��ģ��
  vTemplateID := -1;
  vFrmTempList := TfrmTemplateList.Create(nil);
  try
    vFrmTempList.Parent := Self;
    vFrmTempList.dtpRecDT.MinDateTime := FPatientInfo.InDeptDateTime;
    vFrmTempList.dtpRecDT.DateTime := Now;
    vFrmTempList.dtpRecDT.MaxDateTime := Now;
    vFrmTempList.ShowModal;
    if vFrmTempList.ModalResult = mrOk then
    begin
      vTemplateID := vFrmTempList.TemplateID;
      // ������Ϣ����
      vRecordInfo := TRecordInfo.Create;
      vRecordInfo.DesID := vFrmTempList.DesID;
      vRecordInfo.RecName := vFrmTempList.RecordName;
      vRecordInfo.DT := vFrmTempList.dtpRecDT.DateTime;
    end
    else
      Exit;
  finally
    FreeAndNil(vFrmTempList);
  end;

  //if vTemplateID < 0 then Exit;  // û��ѡ��ģ��

  vSM := TMemoryStream.Create;
  try
    TBLLInvoke.GetTemplateContent(vTemplateID, vSM);  // ȡģ������

    try
      NewPageAndRecord(vRecordInfo, vFrmRecord);  // ����pageҳ�����ϵĲ�������

      if vSM.Size > 0 then  // �����ݣ���������
      begin
        // ��ȡ��ǰ���ݼ�����Щ���ݿ��Ա��滻�����ݣ��ŵ�����FDataElementSetMacro��
        PrepareSyncData(vRecordInfo.DesID);

        vFrmRecord.EmrView.OnSyncDeItem := DoSyncDeItem;
        try
          vFrmRecord.EmrView.BeginUpdate;
          try
            // ����ģ�壬���ع��̻����DoSyncDeItem����ÿһ������Ԫ��FDataElementSetMacro����
            // �Լ�Ҫ�滻Ϊʲô���ݵĻ���
            vFrmRecord.EmrView.LoadFromStream(vSM);  // ����ģ��

            // �滻�����������
            SyncDeGroupByStruct(vFrmRecord.EmrView);
            vFrmRecord.EmrView.FormatData;
            vFrmRecord.EmrView.IsChanged := True;          
          finally
            vFrmRecord.EmrView.EndUpdate;
          end;
        finally
          vFrmRecord.EmrView.OnSyncDeItem := nil;
        end;
      end;

      vFrmRecord.Show;  // ��ʾ������
    except
      On E: Exception do
      begin
        vPageIndex := GetRecordPageIndex(vRecordInfo.ID);
        if vPageIndex >= 0 then
          CloseRecordPage(vPageIndex);

        ShowMessage('�����½�����ʱ�����¼���TfrmPatientRecord.mniNewClick���쳣��' + E.Message);
      end;
    end;
  finally
    vSM.Free;
  end;
end;

procedure TfrmPatientRecord.mniReSyncClick(Sender: TObject);
var
  vDesPID, vDesID, vRecordID, vPageIndex: Integer;
  vFrmRecord: TfrmRecord;
  vItem: THCCustomItem;
  vDeItem: TDeItem;
  vValue: string;
begin
  vDesPID := -1;
  vDesID := -1;
  vRecordID := -1;

  GetNodeRecordInfo(tvRecord.Selected, vDesPID, vDesID, vRecordID);
  if vRecordID > 0 then
  begin
    vPageIndex := GetRecordPageIndex(vRecordID);
    if vPageIndex < 0 then
    begin
      ShowMessage('���ȴ򿪲��������ڱ༭״̬��');
      Exit;
    end
    else
    begin
      FRecPages.PageIndex := vPageIndex;
      vFrmRecord := GetPageRecord(vPageIndex);
      if vFrmRecord.EmrView.ReadOnly then
      begin
        ShowMessage('���Ƚ����������ڱ༭״̬��');
        Exit;
      end;

      vItem := nil;
      vDeItem := nil;

      vFrmRecord.EmrView.BeginUpdate();
      try
        vFrmRecord.TraverseElement(
          procedure (const AData: THCCustomData; const AItemNo, ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean)
          begin
            vItem := aData.Items[aItemNo];
            if vItem.StyleNo < THCStyle.Null then
              (vItem as THCCustomRectItem).FormatDirty()
            else
            if vItem is TDeItem then
            begin
              vDeItem := vItem as TDeItem;  // ÿ������һ������Ԫ

              vValue := DoDeItemGetSyncValue(vDesID, vDeItem);  // ȡ����Ԫ��ͬ��ֵ
              if vValue <> '' then
              begin
                vDeItem.Text := vValue;
                //vDeItem[DeProp.CMVVCode] = ""; ֵ�����
              end;
            end;
          end, [saHeader, saPage, saFooter], 0);  // ��������Ԫ
      finally
        vFrmRecord.EmrView.EndUpdate();
      end;
    end;
  end;
end;

procedure TfrmPatientRecord.mniMergeViewClick(Sender: TObject);
var
  vFrmRecordSet: TfrmRecordSet;
begin
  vFrmRecordSet := TfrmRecordSet.Create(nil);
  try
    vFrmRecordSet.ShowDialog(FPatientInfo.PatID, FPatientInfo.VisitID);
  finally
    FreeAndNil(vFrmRecordSet);
  end;
end;
{var
  vDesPID, vDesID, vRecordID, vPageIndex: Integer;
begin
  if not TreeNodeIsRecord(tvRecord.Selected) then Exit;  // ���ǲ����ڵ�

  GetNodeRecordInfo(tvRecord.Selected, vDesPID, vDesID, vRecordID);

  if vDesPID = TDataSetInfo.Proc then  // ���̼�¼
  begin
    vPageIndex := GetRecordPageIndex(-vDesPID);
    if vPageIndex < 0 then
      LoadPatientDataSetContent(vDesPID)
    else
      FRecPages.PageIndex := vPageIndex;
  end;
end;}

procedure TfrmPatientRecord.mniViewClick(Sender: TObject);
var
  vDesPID, vDesID, vRecordID, vPageIndex: Integer;
  vFrmRecord: TfrmRecord;
begin
  if not TreeNodeIsRecord(tvRecord.Selected) then Exit;  // ���ǲ����ڵ�

  GetNodeRecordInfo(tvRecord.Selected, vDesPID, vDesID, vRecordID);

  if vRecordID > 0 then
  begin
    vPageIndex := GetRecordPageIndex(vRecordID);
    if vPageIndex < 0 then  // û��
    begin
      LoadPatientRecordContent(TRecordInfo(tvRecord.Selected.Data));  // ��������
      vPageIndex := GetRecordPageIndex(vRecordID);
    end
    else  // �Ѿ������л���
      FRecPages.PageIndex := vPageIndex;

    try
      vFrmRecord := GetPageRecord(vPageIndex);
    finally
      vFrmRecord.EmrView.ReadOnly := True;
    end;

    vFrmRecord.EmrView.Trace := TBLLInvoke.GetInchRecordSignature(vRecordID);
  end;
end;

procedure TfrmPatientRecord.mniXMLClick(Sender: TObject);
var
  vDesPID, vDesID, vRecordID, vPageIndex: Integer;
  vFrmRecord: TfrmRecord;
  vSaveDlg: TSaveDialog;
  vFileName: string;
begin
  if not TreeNodeIsRecord(tvRecord.Selected) then Exit;  // ���ǲ����ڵ�

  GetNodeRecordInfo(tvRecord.Selected, vDesPID, vDesID, vRecordID);

  if vRecordID > 0 then
  begin
    vSaveDlg := TSaveDialog.Create(nil);
    try
      vSaveDlg.Filter := 'XML|*.xml';
      if vSaveDlg.Execute then
      begin
        if vSaveDlg.FileName <> '' then
        begin
          HintFormShow('���ڵ���XML�ṹ...', procedure(const AUpdateHint: TUpdateHint)
          begin
            vPageIndex := GetRecordPageIndex(vRecordID);
            if vPageIndex < 0 then  // û��
            begin
              LoadPatientRecordContent(TRecordInfo(tvRecord.Selected.Data));  // ��������
              vPageIndex := GetRecordPageIndex(vRecordID);
            end
            else  // �Ѿ������л���
              FRecPages.PageIndex := vPageIndex;

            vFrmRecord := GetPageRecord(vPageIndex);

            vFileName := ExtractFileExt(vSaveDlg.FileName);
            if LowerCase(vFileName) <> '.xml' then
              vFileName := vSaveDlg.FileName + '.xml'
            else
              vFileName := vSaveDlg.FileName;

            SaveStructureToXml(vFrmRecord, vFileName);
          end);
        end;
      end;
    finally
      FreeAndNil(vSaveDlg);
    end;
  end;
end;

procedure TfrmPatientRecord.NewPageAndRecord(const ARecordInfo: TRecordInfo;
  var AFrmRecord: TfrmRecord);
var
  vPage: TCFPage;
  vPageButton: TCFPageButton;
begin
  // ������������
  AFrmRecord := TfrmRecord.Create(nil);
  AFrmRecord.OnSave := DoSaveRecordContent;
  AFrmRecord.OnSaveStructure := DoSaveRecordStructure;
  AFrmRecord.OnChangedSwitch := DoRecordChangedSwitch;
  AFrmRecord.OnReadOnlySwitch := DoRecordReadOnlySwitch;
  AFrmRecord.OnSetDeItemText := DoSetDeItemText;
  AFrmRecord.OnDeItemPopup := DoDeItemPopup;
  AFrmRecord.OnPrintPreview := DoPrintPreview;
  AFrmRecord.OnDeItemGetSyncValue := DoDeItemGetSyncValue;
  AFrmRecord.OnSyntaxCheck := DoSyntaxCheck;
  //AFrmRecord.OnSyntaxPaint := nil;

  AFrmRecord.OnCopyRequest := DoRecordCopyRequest;
  AFrmRecord.OnPasteRequest := DoRecordPasteRequest;
  AFrmRecord.OnCopyAsStream := DoRecordCopyAsStream;
  AFrmRecord.OnPasteFromStream := DoRecordPasteFromStream;

  AFrmRecord.ObjectData := ARecordInfo;
  AFrmRecord.Align := alClient;
  AFrmRecord.Parent := pnlRecord;

  FRecPages.BeginUpdate;
  try
    vPage := FRecPages.AddPage(ARecordInfo.RecName, AFrmRecord);
    vPage.ImageIndex := 1;
    vPageButton := vPage.AddButton;
    vPageButton.ImageIndex := 5;
    vPageButton.HotImageIndex := 6;
    vPageButton.DownImageIndex := 7;
  finally
    FRecPages.EndUpdate;
  end;
end;

procedure TfrmPatientRecord.pmRecordPopup(Sender: TObject);
var
  vDesPID, vDesID, vRecordID: Integer;
begin
  if not TreeNodeIsRecord(tvRecord.Selected) then  // ���ǲ����ڵ�
  begin
    mniView.Visible := False;
    mniEdit.Visible := False;
    mniDelete.Visible := False;
    //mniPreview.Visible := False;  // ���̼�¼
  end
  else
  begin
    GetNodeRecordInfo(tvRecord.Selected, vDesPID, vDesID, vRecordID);

    mniView.Visible := vRecordID > 0;
    mniEdit.Visible := vRecordID > 0;
    mniDelete.Visible := vRecordID > 0;
    //mniPreview.Visible := vDesPID = 13;  // ���̼�¼
  end;
end;

procedure TfrmPatientRecord.PrepareSyncData(const ADesID: Integer);
begin
  FSyncDataDesID := ADesID;
  // ȡDataElementSetMacro;
  BLLServerExec(procedure(const ABLLServerReady: TBLLServerProxy)
  begin
    ABLLServerReady.Cmd := BLL_GetDataElementSetMacro;
    ABLLServerReady.ExecParam.I['DesID'] := ADesID;  // ���ݼ�ID
    ABLLServerReady.BackDataSet := True;
  end,
  procedure(const ABLLServerRun: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
  begin
    if not ABLLServerRun.MethodRunOk then
      raise Exception.Create(ABLLServerRun.MethodError);  //Exit;

    if AMemTable <> nil then
    begin
      FDataElementSetMacro.Close;
      FDataElementSetMacro.Data := AMemTable.Data;
    end;
  end);

  ClientCache.GetDataSetElement(ADesID);  // ȡ���ݼ�����������Ԫ
  FServerInfo.DateTime := TBLLInvoke.GetServerDateTime;
end;

procedure TfrmPatientRecord.RefreshRecordNode;
var
  vNode: TTreeNode;
begin
  ClearRecordNode(tvRecord);

  // ����סԺ�ڵ�
  vNode := tvRecord.Items.AddObject(nil, FPatientInfo.BedNo + ' ' + FPatientInfo.Name
    + ' ' + FormatDateTime('YYYY-MM-DD HH:mm', FPatientInfo.InDeptDateTime), nil);
  vNode.HasChildren := True;
  vNode.ImageIndex := -1;
  vNode.SelectedIndex := -1;
end;

procedure TfrmPatientRecord.SaveRecordStructure(const ARecordID: Integer;
  const AFrmRecord: TfrmRecord; const AInsert: Boolean);
var
  vXmlDoc: IXMLDocument;
begin
  HintFormShow('�������ɲ����ṹ...', procedure(const AUpdateHint: TUpdateHint)
  begin
    vXmlDoc := GetStructureToXml(AFrmRecord);
    if not Assigned(vXmlDoc) then Exit;

    AUpdateHint('���ڱ��没���ṹ�������...');
    BLLServerExec(procedure(const ABLLServerReady: TBLLServerProxy)
    var
      vMS: TMemoryStream;
    begin
      if AInsert then
        ABLLServerReady.Cmd := BLL_SAVERECORDSTRUCTURE  // ���没���ṹ����
      else
        ABLLServerReady.Cmd := BLL_UPDATERECORDSTRUCTURE;

      ABLLServerReady.ExecParam.I['RID'] := ARecordID;

      vMS := TMemoryStream.Create;
      try
        vXmlDoc.SaveToStream(vMS);
        ABLLServerReady.ExecParam.ForcePathObject('Structure').LoadBinaryFromStream(vMS);
      finally
        FreeAndNil(vMS);
      end;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // ����˷�������ִ�гɹ�
        ShowMessage('���没���ṹʧ�ܣ�ԭ��' + #13#10 + ABLLServer.MethodError);
    end);
  end);
end;

procedure TfrmPatientRecord.SaveStructureToXml(
  const AFrmRecord: TfrmRecord; const AFileName: string);
var
  vXmlDoc: IXMLDocument;
begin
  vXmlDoc := GetStructureToXml(AFrmRecord);
  try
    vXmlDoc.SaveToFile(AFileName);
  finally
    vXmlDoc := nil;
  end;
end;

procedure TfrmPatientRecord.SyncDeGroupByStruct(const AEmrView: THCEmrView);
var
  vDeGroupIndex, vText: string;
  i, j, vItemNo: Integer;
  vXmlNode: IXMLNode;
  vData: THCViewData;
begin
  for i := 0 to AEmrView.Sections.Count - 1 do
  begin
    vData := AEmrView.Sections[0].Page;
    vItemNo := vData.Items.Count - 1;

    while vItemNo >= 0 do
    begin
      if THCDomainItem.IsBeginMark(vData.Items[vItemNo]) then
      begin
        vDeGroupIndex := (vData.Items[vItemNo] as TDeGroup)[TDeProp.Index];

        FDataElementSetMacro.Filtered := False;
        FDataElementSetMacro.Filter := 'MacroType = 3 and ObjID = ' + vDeGroupIndex;
        FDataElementSetMacro.Filtered := True;
        //
        if FDataElementSetMacro.RecordCount > 0 then
        begin
          vXmlNode := GetDeItemNodeFromStructDoc(FPatientInfo.PatID,
            FDataElementSetMacro.FieldByName('Macro').AsInteger, vDeGroupIndex);

          if Assigned(vXmlNode) then
          begin
            vText := '';
            for j := 0 to vXmlNode.ChildNodes.Count - 1 do
              vText := vText + vXmlNode.ChildNodes[j].Text;

            if vText <> '' then
              AEmrView.SetDataDeGroupText(vData, vItemNo, vText);
          end;
        end
        //else
        //if vDeGroupIndex = '197' then
        //  aEmrView.SetDeGroupText(vData, vItemNo, '��һ��ҽ��#13#10    ��һ��ҽ����ҽ��#13#10    ��һ��ҽ����ҽ��#13#10�ڶ���ҽ��#13#10    �ڶ���ҽ����ҽ��#13#10    �ڶ���ҽ����ҽ��');
      end;

      Dec(vItemNo);
    end;
  end;
end;

procedure TfrmPatientRecord.tvRecordDblClick(Sender: TObject);
begin
  mniViewClick(Sender);
end;

procedure TfrmPatientRecord.tvRecordExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  vPatNode: TTreeNode;
begin
  if Node.Parent = nil then  // ����סԺ��Ϣ�����
  begin
    if Node.Count = 0 then  // �����޲����ڵ�ʱ�Ż�ȡ�����ε��½�������ɴ���ѡ�нڵ�Ĵ���
    begin
      GetPatientRecordListUI;  // ��ȡ���߲����б�

      // �޲���ʱ���߽ڵ�չ����ȥ��+��
      vPatNode := GetPatientNode;
      if vPatNode.Count = 0 then
        vPatNode.HasChildren := False;
    end;
  end;
end;

{ TXmlStruct }

constructor TXmlStruct.Create;
begin
  FOnlyDeItem := False;

  FDETable := TFDMemTable.Create(nil);
  FDETable.FilterOptions := [foCaseInsensitive{�����ִ�Сд, foNoPartialCompare��֧��ͨ���(*)����ʾ�Ĳ���ƥ��}];

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDATAELEMENT;  // ��ȡ����Ԫ�б�
      ABLLServerReady.BackDataSet := True;  // ���߷����Ҫ����ѯ���ݼ��������
    end,
    procedure(const ABLLServerRun: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServerRun.MethodRunOk then  // ����˷�������ִ�в��ɹ�
        raise Exception.Create(ABLLServerRun.MethodError);

      if AMemTable <> nil then
        FDETable.CloneCursor(AMemTable);
    end);

  FDeGroupNodes := TList.Create;

  FXmlDoc := TXMLDocument.Create(nil);
  FXmlDoc.Active := True;
  FXmlDoc.DocumentElement := FXmlDoc.CreateNode('DocInfo', ntElement, '');
  FXmlDoc.DocumentElement.Attributes['SourceTool'] := 'HCEMRView';
end;

destructor TXmlStruct.Destroy;
begin
  FreeAndNil(FDETable);
  FreeAndNil(FDeGroupNodes);
  FXmlDoc := nil;
  inherited;
end;

procedure TXmlStruct.TraverseItem(const AData: THCCustomData; const AItemNo,
  ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean);
var
  vDeItem: TDeItem;
  vDeGroup: TDeGroup;
  vXmlNode: IXMLNode;
begin
  if (AData is THCHeaderData) or (AData is THCFooterData) then Exit;

  if AData.Items[AItemNo] is TDeGroup then  // ������
  begin
    vDeGroup := AData.Items[AItemNo] as TDeGroup;
    if vDeGroup.MarkType = TMarkType.cmtBeg then
    begin
      if FDeGroupNodes.Count > 0 then
        vXmlNode := IXMLNode(FDeGroupNodes[FDeGroupNodes.Count - 1]).AddChild('DeGroup', -1)
      else
        vXmlNode := FXmlDoc.DocumentElement.AddChild('DeGroup', -1);

      vXmlNode.Attributes['Index'] := vDeGroup[TDeProp.Index];

      FDETable.Filtered := False;
      FDETable.Filter := 'DeID = ' + vDeGroup[TDeProp.Index];
      FDETable.Filtered := True;

      if FDETable.RecordCount = 1 then
      begin
        FDETable.First;
        vXmlNode.Attributes['Code'] := FDETable.FieldByName('decode').AsString;  // vDeGroup[TDeProp.Code];
        vXmlNode.Attributes['Name'] := FDETable.FieldByName('dename').AsString;  // vDeGroup[TDeProp.Name];
      end;

      FDeGroupNodes.Add(vXmlNode);
    end
    else
    begin
      if FDeGroupNodes.Count > 0 then
        FDeGroupNodes.Delete(FDeGroupNodes.Count - 1);
    end;
  end
  else
  if AData.Items[AItemNo].StyleNo > THCStyle.Null then  // �ı���
  begin
    if (AData.Items[AItemNo] as TDeItem).IsElement then  // ����Ԫ
    begin
      vDeItem := AData.Items[AItemNo] as TDeItem;
      if vDeItem[TDeProp.Index] <> '' then
      begin
        if FDeGroupNodes.Count > 0 then
          vXmlNode := IXMLNode(FDeGroupNodes[FDeGroupNodes.Count - 1]).AddChild('DeItem', -1)
        else
          vXmlNode := FXmlDoc.DocumentElement.AddChild('DeItem', -1);

        vXmlNode.Text := vDeItem.Text;
        vXmlNode.Attributes['Index'] := vDeItem[TDeProp.Index];

        FDETable.Filtered := False;
        FDETable.Filter := 'DeID = ' + vDeItem[TDeProp.Index];
        FDETable.Filtered := True;

        if FDETable.RecordCount = 1 then
        begin
          vXmlNode.Attributes['Code'] := FDETable.FieldByName('DeCode').AsString;
          vXmlNode.Attributes['Name'] := FDETable.FieldByName('DeName').AsString;
        end;
      end;
    end
    else
    if not FOnlyDeItem then
    begin
      if FDeGroupNodes.Count > 0 then
        vXmlNode := IXMLNode(FDeGroupNodes[FDeGroupNodes.Count - 1]).AddChild('Text', -1)
      else
        vXmlNode := FXmlDoc.DocumentElement.AddChild('Text', -1);

      vXmlNode.Text := AData.Items[AItemNo].Text;
    end;
  end
  else  // ���ı���
  begin
    if AData.Items[AItemNo] is TEmrYueJingItem then
    begin
      if FDeGroupNodes.Count > 0 then
        vXmlNode := IXMLNode(FDeGroupNodes[FDeGroupNodes.Count - 1]).AddChild('DeItem', -1)
      else
        vXmlNode := FXmlDoc.DocumentElement.AddChild('DeItem', -1);

      (AData.Items[AItemNo] as TEmrYueJingItem).ToXmlEmr(vXmlNode);
    end
    else
    if AData.Items[AItemNo] is TEmrToothItem then
    begin
      if FDeGroupNodes.Count > 0 then
        vXmlNode := IXMLNode(FDeGroupNodes[FDeGroupNodes.Count - 1]).AddChild('DeItem', -1)
      else
        vXmlNode := FXmlDoc.DocumentElement.AddChild('DeItem', -1);

      (AData.Items[AItemNo] as TEmrToothItem).ToXmlEmr(vXmlNode);
    end
    else
    if AData.Items[AItemNo] is TEmrFangJiaoItem then
    begin
      if FDeGroupNodes.Count > 0 then
        vXmlNode := IXMLNode(FDeGroupNodes[FDeGroupNodes.Count - 1]).AddChild('DeItem', -1)
      else
        vXmlNode := FXmlDoc.DocumentElement.AddChild('DeItem', -1);

      (AData.Items[AItemNo] as TEmrFangJiaoItem).ToXmlEmr(vXmlNode);
    end;
  end;
end;

{ TStructDoc }

constructor TStructDoc.Create(const APatID: string; const ADesID: Integer);
begin
  FPatID := APatID;
  FDesID := ADesID;
  FXmlDoc := TXmlDocument.Create(nil);
end;

destructor TStructDoc.Destroy;
begin
  FXmlDoc := nil;
  inherited;
end;

class function TStructDoc.GetDeItemNode(const ADeIndex: string;
  const AXmlDoc: IXMLDocument): IXmlNode;

  function _GetDeNode(const ANodes: IXMLNodeList): IXMLNode;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to ANodes.Count - 1 do
    begin
      if ANodes[i].Attributes['Index'] = ADeIndex then
      begin
        Result := ANodes[i];
        Break;
      end
      else
      begin
        Result := _GetDeNode(ANodes[i].ChildNodes);
        if Assigned(Result) then
          Break;
      end;
    end;
  end;

begin
  Result := _GetDeNode(AXmlDoc.DocumentElement.ChildNodes);
end;

end.
