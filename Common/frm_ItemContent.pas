{*******************************************************}
{                                                       }
{         ����HCView�ĵ��Ӳ�������  ���ߣ���ͨ          }
{                                                       }
{ �˴������ѧϰ����ʹ�ã�����������ҵĿ�ģ��ɴ�������  }
{ �����ʹ���߳е�������QQȺ 649023932 ����ȡ����ļ��� }
{ ������                                                }
{                                                       }
{*******************************************************}

unit frm_ItemContent;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, HCEmrEdit,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ToolWin, System.ImageList,
  Vcl.ImgList, HCTextStyle, Vcl.Menus, FireDAC.Comp.Client, Vcl.Buttons,
  frm_RecordPop, HCEmrElementItem, frm_DataElement;

type
  TfrmItemContent = class(TForm)
    pnlEdit: TPanel;
    spl1: TSplitter;
    il1: TImageList;
    tlbFontSize: TToolBar;
    cbbFontSize: TComboBox;
    cbFontColor: TColorBox;
    btnBold: TToolButton;
    btnItalic: TToolButton;
    btnUnderLine: TToolButton;
    btnStrikeOut: TToolButton;
    btnSuperScript: TToolButton;
    btnSubScript: TToolButton;
    btn2: TToolButton;
    btnLineSpace: TToolButton;
    btn9: TToolButton;
    pmLineSpace: TPopupMenu;
    mniLineSpace: TMenuItem;
    mniN17: TMenuItem;
    mniN21: TMenuItem;
    cbbFont: TComboBox;
    btnSave: TSpeedButton;
    pnlDE: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBoldClick(Sender: TObject);
    procedure cbbFontChange(Sender: TObject);
    procedure cbbFontSizeChange(Sender: TObject);
    procedure cbFontColorChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
    FDomainItemID: Integer;
    FEmrEdit: TEmrEdit;
    FfrmDataElement: TfrmDataElement;

    FMouseDownTick: Cardinal;
    FfrmRecordPop: TfrmRecordPop;
    FOnSetDeItemText: TDeItemSetTextEvent;

    procedure DoSaveItemContent;
    procedure SetDomainItemID(Value: Integer);

    /// <summary> ���õ�ǰ����Ԫ���ı����� </summary>
    procedure DoSetActiveDeItemText(const ADeItem: TDeItem; const AText: string; var ACancel: Boolean);

    /// <summary> ���õ�ǰ����Ԫ������Ϊ��չ���� </summary>
    procedure DoSetActiveDeItemExtra(const ADeItem: TDeItem; const AStream: TStream);

    /// <summary> ��ȡ����Ԫֵ������ </summary>
    function PopupForm: TfrmRecordPop;
    /// <summary> ��Ԫֵ������ر��¼� </summary>
    procedure PopupFormClose;
    procedure DoEmrEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoEmrEditMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure DoDEInsertAsDeItem(Sender: TObject);
    procedure DoDEInsertAsDeGroup(Sender: TObject);
    procedure DoDEInsertAsDeEdit(Sender: TObject);
    procedure DoDEInsertAsDeCombobox(Sender: TObject);
    procedure DoDEInsertAsDeDateTime(Sender: TObject);
    procedure DoDEInsertAsDeRadioGroup(Sender: TObject);
    procedure DoDEInsertAsDeCheckBox(Sender: TObject);
  public
    { Public declarations }
    property DomainItemID: Integer read FDomainItemID write SetDomainItemID;
  end;

implementation

uses
  HCCommon, HCEmrGroupItem, emr_Common, emr_BLLInvoke, HCItem, HCTextItem,
  HCRectItem, HCStyle, HCDrawItem, HCRichData, Data.DB;

{$R *.dfm}

procedure TfrmItemContent.btnBoldClick(Sender: TObject);
begin
  case (Sender as TToolButton).Tag of
    0: FEmrEdit.ApplyTextStyle(THCFontStyle.tsBold);
    1: FEmrEdit.ApplyTextStyle(THCFontStyle.tsItalic);
    2: FEmrEdit.ApplyTextStyle(THCFontStyle.tsUnderline);
    3: FEmrEdit.ApplyTextStyle(THCFontStyle.tsStrikeOut);
    4: FEmrEdit.ApplyTextStyle(THCFontStyle.tsSuperscript);
    5: FEmrEdit.ApplyTextStyle(THCFontStyle.tsSubscript);
  end;
end;

procedure TfrmItemContent.btnSaveClick(Sender: TObject);
begin
  DoSaveItemContent;
end;

procedure TfrmItemContent.cbbFontChange(Sender: TObject);
begin
  FEmrEdit.ApplyTextFontName(cbbFont.Text);
end;

procedure TfrmItemContent.cbbFontSizeChange(Sender: TObject);
begin
  FEmrEdit.ApplyTextFontSize(GetFontSize(cbbFontSize.Text));
end;

procedure TfrmItemContent.cbFontColorChange(Sender: TObject);
begin
  FEmrEdit.ApplyTextColor(cbFontColor.Selected);
end;

procedure TfrmItemContent.DoDEInsertAsDeCheckBox(Sender: TObject);
var
  vDeCheckBox: TDeCheckBox;
begin
  vDeCheckBox := TDeCheckBox.Create(FEmrEdit.TopLevelData, FfrmDataElement.GetDeName, False);
  vDeCheckBox[TDeProp.Index] := FfrmDataElement.GetDeIndex;
  vDeCheckBox[TDeProp.Name] := FfrmDataElement.GetDeName;
  FEmrEdit.InsertItem(vDeCheckBox);
end;

procedure TfrmItemContent.DoDEInsertAsDeCombobox(Sender: TObject);
var
  vDeCombobox: TDeCombobox;
begin
  vDeCombobox := TDeCombobox.Create(FEmrEdit.TopLevelData, FfrmDataElement.GetDeName);
  vDeCombobox.SaveItem := False;
  vDeCombobox[TDeProp.Index] := FfrmDataElement.GetDeIndex;
  vDeCombobox[TDeProp.Name] := FfrmDataElement.GetDeName;
  FEmrEdit.InsertItem(vDeCombobox);
end;

procedure TfrmItemContent.DoDEInsertAsDeDateTime(Sender: TObject);
var
  vDeDateTime: TDeDateTimePicker;
begin
  vDeDateTime := TDeDateTimePicker.Create(FEmrEdit.TopLevelData, Now);
  vDeDateTime[TDeProp.Index] := FfrmDataElement.GetDeIndex;
  vDeDateTime[TDeProp.Name] := FfrmDataElement.GetDeName;
  FEmrEdit.InsertItem(vDeDateTime);
end;

procedure TfrmItemContent.DoDEInsertAsDeEdit(Sender: TObject);
var
  vDeEdit: TDeEdit;
begin
  vDeEdit := TDeEdit.Create(FEmrEdit.TopLevelData, FfrmDataElement.GetDeName);
  vDeEdit[TDeProp.Index] := FfrmDataElement.GetDeIndex;
  vDeEdit[TDeProp.Name] := FfrmDataElement.GetDeName;

  FEmrEdit.InsertItem(vDeEdit);
end;

procedure TfrmItemContent.DoDEInsertAsDeGroup(Sender: TObject);
var
  vDeGroup: TDeGroup;
begin
  vDeGroup := TDeGroup.Create(FEmrEdit.TopLevelData);
  try
    vDeGroup[TDeProp.Index] := FfrmDataElement.GetDeIndex;
    vDeGroup[TDeProp.Name] := FfrmDataElement.GetDeName;

    FEmrEdit.InsertDeGroup(vDeGroup);
  finally
    vDeGroup.Free;
  end;
end;

procedure TfrmItemContent.DoDEInsertAsDeItem(Sender: TObject);
var
  vDeItem: TDeItem;
begin
  vDeItem := FEmrEdit.NewDeItem(FfrmDataElement.GetDeName);
  vDeItem[TDeProp.Index] := FfrmDataElement.GetDeIndex;
  vDeItem[TDeProp.Name] := FfrmDataElement.GetDeName;
  FEmrEdit.InsertDeItem(vDeItem);
end;

procedure TfrmItemContent.DoDEInsertAsDeRadioGroup(Sender: TObject);
var
  vDeRadioGropu: TDeRadioGroup;
begin
  vDeRadioGropu := TDeRadioGroup.Create(FEmrEdit.TopLevelData);
  vDeRadioGropu[TDeProp.Index] := FfrmDataElement.GetDeIndex;
  vDeRadioGropu[TDeProp.Name] := FfrmDataElement.GetDeName;
  // ȡ����Ԫ��ѡ�ѡ��̫��ʱ��ʾ�Ƿ񶼲���
  vDeRadioGropu.AddItem('ѡ��1');
  vDeRadioGropu.AddItem('ѡ��2');
  vDeRadioGropu.AddItem('ѡ��3');

  FEmrEdit.InsertItem(vDeRadioGropu);
end;

procedure TfrmItemContent.DoEmrEditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PopupFormClose;
  FMouseDownTick := GetTickCount;
end;

procedure TfrmItemContent.DoEmrEditMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vActiveItem: THCCustomItem;
  vDeItem: TDeItem;
  vDeGroup: TDeGroup;
  vDeEdit: TDeEdit;
  vDeCombobox: TDeCombobox;
  vDeDateTimePicker: TDeDateTimePicker;
  vDeRadioGroup: TDeRadioGroup;
  vActiveDrawItem: THCCustomDrawItem;
  vPt: TPoint;
  vDrawItemRect: TRect;
  vInfo: string;
  vTopData: THCRichData;
begin
  vInfo := '';

  vActiveItem := FEmrEdit.Data.GetTopLevelItem;
  if vActiveItem <> nil then
  begin
    if FEmrEdit.Data.ActiveDomain.BeginNo >= 0 then
    begin
      vDeGroup := FEmrEdit.Data.Items[FEmrEdit.Data.ActiveDomain.BeginNo] as TDeGroup;

      vInfo := vDeGroup[TDeProp.Name];
    end;

    if vActiveItem is TDeItem then
    begin
      vDeItem := vActiveItem as TDeItem;
      if vDeItem.TraceStyles <> [] then
        vInfo := vInfo + '-' + vDeItem.GetHint
      else
      if vDeItem.Active
        and (vDeItem[TDeProp.Index] <> '')
        and (not vDeItem.IsSelectComplate)
        and (not vDeItem.IsSelectPart)
        and (CalcTickCount(FMouseDownTick, GetTickCount) < 500)  // ����ѡ��Ի���
      then
      begin
        if ClientCache.FindDataElementByIndex(vDeItem[TDeProp.Index]) then
          vInfo := vInfo + '-' + ClientCache.DataElementDT.FieldByName('dename').AsString + '(' + vDeItem[TDeProp.Index] + ')'
        else
          vInfo := vInfo + '-[ȱ��Index]';

        if FEmrEdit.Data.ReadOnly then Exit;

        vPt := FEmrEdit.Data.GetTopLevelDrawItemCoord;  // �õ����EmrEdit������
        vActiveDrawItem := FEmrEdit.Data.GetTopLevelDrawItem;
        vDrawItemRect := vActiveDrawItem.Rect;
        vDrawItemRect := Bounds(vPt.X, vPt.Y, vDrawItemRect.Width, vDrawItemRect.Height);

        if PtInRect(vDrawItemRect, Point(X, Y)) then
        begin
          vPt.Y := vPt.Y + vActiveDrawItem.Height;
          vPt := FEmrEdit.ClientToParent(vPt, Self);
          //PopupForm.Left := vPt.X + FEmrView.Left;
          //PopupForm.Top := vPt.Y + FEmrView.Top;
          vPt := ClientToScreen(vPt);

          PopupForm.PopupDeItem(vDeItem, vPt);
        end;
      end;
    end;
  end;
end;

procedure TfrmItemContent.DoSaveItemContent;
var
  vSM: TMemoryStream;
begin
  vSM := TMemoryStream.Create;
  try
    FEmrEdit.SaveToStream(vSM);

    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)  // ��ȡ����
      begin
        ABLLServerReady.Cmd := BLL_SAVEDOMAINITEMCONTENT;  // ��ȡģ������б�
        ABLLServerReady.ExecParam.I['DItemID'] := FDomainItemID;
        ABLLServerReady.ExecParam.ForcePathObject('Content').LoadBinaryFromStream(vSM);
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      begin
        if ABLLServer.MethodRunOk then  // ����˷�������ִ�гɹ�
          ShowMessage('����ɹ���')
        else
          ShowMessage(ABLLServer.MethodError);
      end);
  finally
    FreeAndNil(vSM);
  end;
end;

procedure TfrmItemContent.DoSetActiveDeItemExtra(const ADeItem: TDeItem;
  const AStream: TStream);
begin
  FEmrEdit.SetActiveItemExtra(AStream);
end;

procedure TfrmItemContent.DoSetActiveDeItemText(const ADeItem: TDeItem;
  const AText: string; var ACancel: Boolean);
var
  vText: string;
begin
  if Assigned(FOnSetDeItemText) then
  begin
    vText := AText;
    FOnSetDeItemText(Self, ADeItem, vText, ACancel);
    if not ACancel then
      FEmrEdit.SetActiveItemText(vText);
  end
  else
    FEmrEdit.SetActiveItemText(AText);
end;

procedure TfrmItemContent.FormCreate(Sender: TObject);
begin
  FDomainItemID := 0;

  HCDefaultTextItemClass := TDeItem;
  HCDefaultDomainItemClass := TDeGroup;

  FEmrEdit := TEmrEdit.Create(Self);
  FEmrEdit.Parent := Self.pnlEdit;
  FEmrEdit.Align := alClient;

  FEmrEdit.OnMouseDown := DoEmrEditMouseDown;
  FEmrEdit.OnMouseUp := DoEmrEditMouseUp;

  FfrmDataElement := TfrmDataElement.Create(nil);
  FfrmDataElement.BorderStyle := bsNone;
  FfrmDataElement.Align := alClient;
  FfrmDataElement.Parent := pnlDE;

  FfrmDataElement.OnInsertAsDeItem := DoDEInsertAsDeItem;
  FfrmDataElement.OnInsertAsDeGroup := DoDEInsertAsDeGroup;
  FfrmDataElement.OnInsertAsDeEdit := DoDEInsertAsDeEdit;
  FfrmDataElement.OnInsertAsDeCombobox := DoDEInsertAsDeCombobox;
  FfrmDataElement.OnInsertAsDeDateTime := DoDEInsertAsDeDateTime;
  FfrmDataElement.OnInsertAsDeRadioGroup := DoDEInsertAsDeRadioGroup;
  FfrmDataElement.OnInsertAsDeCheckBox := DoDEInsertAsDeCheckBox;

  FfrmDataElement.Show;
end;

procedure TfrmItemContent.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEmrEdit);
  FreeAndNil(FfrmDataElement);
end;

function TfrmItemContent.PopupForm: TfrmRecordPop;
begin
  if not Assigned(FfrmRecordPop) then
  begin
    FfrmRecordPop := TfrmRecordPop.Create(nil);
    FfrmRecordPop.OnSetActiveItemText := DoSetActiveDeItemText;
    FfrmRecordPop.OnSetActiveItemExtra := DoSetActiveDeItemExtra;
    //FfrmRecordPop.Parent := Self;
  end;

  Result := FfrmRecordPop;
end;

procedure TfrmItemContent.PopupFormClose;
begin
  if Assigned(FfrmRecordPop) and FfrmRecordPop.Visible then  // ʹ��PopupForm�ᵼ��û��FfrmRecordPopʱ����һ���ٹرգ�������
    FfrmRecordPop.Close;
end;

procedure TfrmItemContent.SetDomainItemID(Value: Integer);
begin
  FDomainItemID := Value;
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDOMAINITEMCONTENT;  // ��ȡ����Ԫѡ��ֵ���Ӧ������
      ABLLServerReady.ExecParam.I['DItemID'] := FDomainItemID;
      ABLLServerReady.AddBackField('Content');
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    var
      vSM: TMemoryStream;
    begin
      if not ABLLServer.MethodRunOk then  // ����˷�������ִ�в��ɹ�
      begin
        ShowMessage(ABLLServer.MethodError);
        Exit;
      end;

      vSM := TMemoryStream.Create;
      try
        ABLLServer.BackField('Content').SaveBinaryToStream(vSM);
        if vSM.Size > 0 then
          FEmrEdit.LoadFromStream(vSM)
        else
          FEmrEdit.Clear;
      finally
        FreeAndNil(vSM);
      end;
    end);
end;

end.
