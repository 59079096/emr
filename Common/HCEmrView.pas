{*******************************************************}
{                                                       }
{         ����HCView�ĵ��Ӳ�������  ���ߣ���ͨ          }
{                                                       }
{ �˴������ѧϰ����ʹ�ã�����������ҵĿ�ģ��ɴ�������  }
{ �����ʹ���߳е�������QQȺ 649023932 ����ȡ����ļ��� }
{ ������                                                }
{                                                       }
{*******************************************************}

unit HCEmrView;

interface

{$I HCEmrView.inc}

uses
  Windows, Classes, Controls, Graphics, HCView, HCEmrViewIH, HCStyle, HCItem,
  HCTextItem, HCDrawItem, HCCustomData, HCRichData, HCViewData, HCSectionData,
  HCEmrElementItem, HCCommon, HCRectItem, HCEmrGroupItem, HCCustomFloatItem,
  HCImageItem, HCSection, Generics.Collections, Messages;

type
  TSyncDeItemEvent = procedure(const Sender: TObject; const AData: THCCustomData; const AItem: THCCustomItem) of object;
  THCCopyPasteStreamEvent = function(const AStream: TStream): Boolean of object;

  THCEmrView = class({$IFDEF VIEWINPUTHELP}THCEmrViewIH{$ELSE}THCView{$ENDIF})
  private
    FDesignMode,
    FHideTrace,  // ���غۼ�
    FTrace,  // �Ƿ������ۼ�״̬
    FSecret: Boolean;  // �Ƿ�����˽��ʾ״̬
    FTraceInfoAnnotate: Boolean;  // ������Ϣ����ע��ʽ��ʾ
    FIgnoreAcceptAction: Boolean;
    FTraceCount: Integer;  // ��ǰ�ĵ��ۼ�����
    FDeDoneColor, FDeUnDoneColor, FDeHotColor: TColor;
    FPageBlankTip: string;  // ҳ��հ�������ʾ
    FPropertyObject: TObject;
    FOnCanNotEdit: TNotifyEvent;
    FOnSyncDeItem: TSyncDeItemEvent;
    // ����ճ������¼�
    FOnCopyRequest, FOnPasteRequest: THCCopyPasteEvent;
    FOnCopyAsStream, FOnPasteFromStream: THCCopyPasteStreamEvent;
    // �﷨�������¼�
    FOnSyntaxCheck: TDataDomainItemNoEvent;
    FOnSyntaxPaint: TSyntaxPaintEvent;

    procedure SetHideTrace(const Value: Boolean);
    procedure SetPageBlankTip(const Value: string);
    procedure DoSyntaxCheck(const AData: THCCustomData; const AItemNo, ATag: Integer;
      const ADomainStack: TDomainStack; var AStop: Boolean);
    procedure DoSyncDeItem(const Sender: TObject; const AData: THCCustomData; const AItem: THCCustomItem);
    procedure InsertEmrTraceItem(const AText: string);
    function CanNotEdit: Boolean;
  protected
    /// <summary> ������Item������ɺ󴥷����¼� </summary>
    /// <param name="Sender">Item�������ĵ���</param>
    procedure DoSectionCreateItem(Sender: TObject); override;

    /// <summary> ������Item����ʱ���� </summary>
    /// <param name="AData">����Item��Data</param>
    /// <param name="AStyleNo">Ҫ������Item��ʽ</param>
    /// <returns>�����õ�Item</returns>
    function DoSectionCreateStyleItem(const AData: THCCustomData;
      const AStyleNo: Integer): THCCustomItem; override;

    procedure DoSectionCaretItemChanged(const Sender: TObject; const AData: THCCustomData;
      const AItem: THCCustomItem); override;

    /// <summary> ����ĳData��Item����󴥷� </summary>
    /// <param name="Sender">���ĸ��ĵ��ڲ���</param>
    /// <param name="AData">���ĸ�Data����</param>
    /// <param name="AItem">�Ѳ����Item</param>
    procedure DoSectionInsertItem(const Sender: TObject;
      const AData: THCCustomData; const AItem: THCCustomItem); override;

    /// <summary> ������ĳData��Itemɾ���󴥷� </summary>
    /// <param name="Sender">���ĸ��ĵ���ɾ��</param>
    /// <param name="AData">���ĸ�Dataɾ��</param>
    /// <param name="AItem">��ɾ����Item</param>
    procedure DoSectionRemoveItem(const Sender: TObject;
      const AData: THCCustomData; const AItem: THCCustomItem); override;

    /// <summary> ָ���Ľڵ�ǰ�Ƿ�ɱ���ָ����Item </summary>
    function DoSectionSaveItem(const Sender: TObject;
      const AData: THCCustomData; const AItemNo: Integer): Boolean; override;

    function DoSectionPaintDomainRegion(const Sender: TObject; const AData: THCCustomData; const AItemNo: Integer): Boolean; override;

    procedure DoSectionItemMouseDown(const Sender: TObject;
      const AData: THCCustomData; const AItemNo, AOffset: Integer;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    /// <summary> ָ���Ľڵ�ǰ�Ƿ�ɱ༭ </summary>
    /// <param name="Sender">�ĵ���</param>
    /// <returns>True���ɱ༭��False�����ɱ༭</returns>
    function DoSectionCanEdit(const Sender: TObject): Boolean; override;

    /// <summary> ָ���Ľڵ�ǰ�Ƿ��ɾ��ָ����Item </summary>
    function DoSectionAcceptAction(const Sender: TObject; const AData: THCCustomData;
      const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean; override;

    /// <summary> �������� </summary>
    /// <param name="Key">����ֵ</param>
    /// <param name="Shift">Shift״̬</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    /// <summary> ������ѹ </summary>
    /// <param name="Key">����ֵ</param>
    procedure KeyPress(var Key: Char); override;

    /// <summary> �ڵ�ǰλ�ò����ı� </summary>
    /// <param name="AText">Ҫ������ַ���(֧�ִ�#13#10�Ļس�����)</param>
    /// <returns>True������ɹ���False������ʧ��</returns>
    function DoInsertText(const AText: string): Boolean; override;

    /// <summary> ����ǰ�����ڿ����Ƿ������� </summary>
    function DoCopyRequest(const AFormat: Word): Boolean; override;

    /// <summary> ճ��ǰ�����ڿ����Ƿ�����ճ�� </summary>
    function DoPasteRequest(const AFormat: Word): Boolean; override;

    /// <summary> ����ǰ�����ڶ�������������������Դ </summary>
    procedure DoCopyAsStream(const AStream: TStream); override;

    /// <summary> ճ��ǰ������ȷ�϶�������������������Դ </summary>
    function DoPasteFromStream(const AStream: TStream): Boolean; override;

    procedure DoSectionDrawItemPaintBefor(const Sender: TObject;
      const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect;
      const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    procedure DoSectionDrawItemPaintContent(const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
      const ADrawText: string; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
      ADataScreenBottom: Integer; const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    /// <summary> �ĵ�ĳ�ڵ�Item������� </summary>
    /// <param name="AData">��ǰ���Ƶ�Data</param>
    /// <param name="ADrawItemIndex">Item��Ӧ��DrawItem���</param>
    /// <param name="ADrawRect">Item��Ӧ�Ļ�������</param>
    /// <param name="ADataDrawLeft">Data����ʱ��Left</param>
    /// <param name="ADataDrawBottom">Data����ʱ��Bottom</param>
    /// <param name="ADataScreenTop">����ʱ����Data��Topλ��</param>
    /// <param name="ADataScreenBottom">����ʱ����Data��Bottomλ��</param>
    /// <param name="ACanvas">����</param>
    /// <param name="APaintInfo">����ʱ��������Ϣ</param>
    procedure DoSectionDrawItemPaintAfter(const Sender: TObject;
      const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect;
      const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary> ����Item </summary>
    /// <param name="ATraverse">����ʱ��Ϣ</param>
    procedure TraverseItem(const ATraverse: THCItemTraverse);

    /// <summary> ���������� </summary>
    /// <param name="ADeGroup">��������Ϣ</param>
    /// <returns>True���ɹ���False��ʧ��</returns>
    function InsertDeGroup(const ADeGroup: TDeGroup): Boolean;

    /// <summary> ��������Ԫ </summary>
    /// <param name="ADeItem">����Ԫ��Ϣ</param>
    /// <returns>True���ɹ���False��ʧ��</returns>
    function InsertDeItem(const ADeItem: TDeItem): Boolean;

    /// <summary> �½�����Ԫ </summary>
    /// <param name="AText">����Ԫ�ı�</param>
    /// <returns>�½��õ�����Ԫ</returns>
    function NewDeItem(const AText: string): TDeItem;

    /// <summary>
    /// ��ȡָ������Ԫ���ı�����
    /// </summary>
    /// <param name="aDeIndex"></param>
    /// <returns></returns>
    function GetDeItemText(const ADeIndex: string; var AText: string): Boolean;

    /// <summary>
    /// ��ȡָ������Ԫָ������ֵ
    /// </summary>
    /// <param name="aDeIndex"></param>
    /// <param name="aPropName"></param>
    /// <param name="aPropValue"></param>
    /// <returns></returns>
    function GetDeItemProperty(const ADeIndex, APropName: string; var APropValue: string): Boolean;

    /// <summary>
    /// ����ָ������Ԫ��ֵ
    /// </summary>
    /// <param name="ADeIndex"></param>
    /// <param name="AText"></param>
    /// <returns>�Ƿ����óɹ�</returns>
    function SetDeItemText(const ADeIndex, AText: string): Boolean;

    function SetDeImageGraphic(const ADeIndex: string; const AGraphicStream: TStream): Boolean;

    /// <summary> ͬ��AStartData��ARefDeItem�����ArefDeItem��ͬԪ�ص����� </summary>
    procedure SyncDeItem(const AStartData: THCCustomData; const ARefDeItem: TDeItem);

    /// <summary> �����Ŀ�ʼ����ͬ��Ԫ�� </summary>
    function FindSameDeItem(const ADeItem: TDeItem): TDeItem;

    /// <summary> ����DLL�ﹳ�Ӵ��ݵķ������TAB�� </summary>
    procedure KeyDownLib(var AKey: Word);

    /// <summary>
    /// ����ָ������Ԫָ�����Ե�ֵ
    /// </summary>
    /// <param name="aDeIndex"></param>
    /// <param name="aPropName"></param>
    /// <param name="aPropValue"></param>
    /// <returns>�Ƿ����óɹ�</returns>
    function SetDeObjectProperty(const ADeIndex, APropName, APropValue: string): Boolean;

    /// <summary> ֱ�����õ�ǰ����Ԫ��ֵΪ��չ���� </summary>
  	/// <param name="AStream">��չ������</param>
    procedure SetActiveItemExtra(const AStream: TStream);

    function CheckDeGroupStart(const AData: THCViewData; const AItemNo: Integer;
      const ADeIndex: string): Boolean;

    function CheckDeGroupEnd(const AData: THCViewData; const AItemNo: Integer;
      const ADeIndex: string): Boolean;

    // ��ָ����StartNoλ����ǰ����ҵ�һ������DeIndex��������������ʼ������Χ
    procedure GetDataDeGroupItemNo(const AData: THCViewData; const ADeIndex: string;
      const AForward: Boolean; var AStartNo, AEndNo: Integer);

    /// <summary> ��ȡָ���������е��ı����� </summary>
    /// <param name="AData">ָ�����ĸ�Data���ȡ</param>
    /// <param name="ADeGroupStartNo">ָ�����������ʼItemNo</param>
    /// <param name="ADeGroupEndNo">ָ��������Ľ���ItemNo</param>
    /// <returns>�������ı�����</returns>
    function GetDataDeGroupText(const AData: THCViewData;
      const ADeGroupStartNo, ADeGroupEndNo: Integer): string;

    /// <summary> �ӵ�ǰ��������ʼλ����ǰ����ͬ����������� </summary>
    /// <param name="AData">ָ�����ĸ�Data���ȡ</param>
    /// <param name="ADeGroupStartNo">ָ�����ĸ�λ�ÿ�ʼ��ǰ��</param>
    /// <returns>��ͬ�������ı���ʽ������</returns>
    function GetDataForwardDeGroupText(const AData: THCViewData;
      const ADeGroupStartNo: Integer): string;

    /// <summary> ���������������Ϊָ�����ı� </summary>
    /// <param name="AData">���������ڵ�Data</param>
    /// <param name="ADeGroupNo">�������ItemNo</param>
    /// <param name="AText">�ı�����</param>
    procedure SetDataDeGroupText(const AData: THCViewData; const ADeGroupNo: Integer; const AText: string);
    procedure SetActionDataDeGroupText(const ASection: THCSection;
      const AArea: TSectionArea; const ADeIndex, AText: string; const AStartLast: Boolean = True);
    procedure SetActionDataDeGroupStream(const ASection: THCSection;
      const AArea: TSectionArea; const ADeIndex: string; const AStream: TStream; const AStartLast: Boolean = True);

    /// <summary> ��ָ���������е�����д�뵽�� </summary>
    procedure GetDataDeGroupToStream(const AData: THCViewData;
      const ADeGroupStartNo, ADeGroupEndNo: Integer; const AStream: TStream);

    /// <summary> ��ָ���������е�����д�뵽�� </summary>
    procedure SetDataDeGroupFromStream(const AData: THCViewData;
      const ADeGroupStartNo, ADeGroupEndNo: Integer; const AStream: TStream);

    /// <summary> �﷨��� </summary>
    procedure SyntaxCheck;

    /// <summary> �Ƿ����ĵ����ģʽ </summary>
    property DesignMode: Boolean read FDesignMode write FDesignMode;

    /// <summary> �Ƿ����غۼ� </summary>
    property HideTrace: Boolean read FHideTrace write SetHideTrace;

    /// <summary> �Ƿ�������״̬ </summary>
    property Trace: Boolean read FTrace write FTrace;

    /// <summary> �Ƿ�����˽��ʾ״̬ </summary>
    property Secret: Boolean read FSecret write FSecret;

    /// <summary> ������Ϣ����ע��ʽ��ʾ�������ĵ�ǰ���úô����ԣ� </summary>
    property TraceInfoAnnotate: Boolean read FTraceInfoAnnotate write FTraceInfoAnnotate;

    /// <summary> �ĵ����м����ۼ� </summary>
    property TraceCount: Integer read FTraceCount;

    /// <summary> ҳ�����ݲ���ʱ�ײ��հ���ʾ </summary>
    property PageBlankTip: string read FPageBlankTip write SetPageBlankTip;

    property DeDoneColor: TColor read FDeDoneColor write FDeDoneColor;
    property DeUnDoneColor: TColor read FDeUnDoneColor write FDeUnDoneColor;
    property DeHotColor: TColor read FDeHotColor write FDeHotColor;

    /// <summary> ����AcceptAction�Ĵ��� </summary>
    //property IgnoreAcceptAction: Boolean read FIgnoreAcceptAction write FIgnoreAcceptAction;

    /// <summary> ��ǰ�ĵ����� </summary>
    property FileName;

    /// <summary> ��ǰ�ĵ���ʽ�� </summary>
    property Style;

    /// <summary> �Ƿ�ԳƱ߾� </summary>
    property SymmetryMargin;

    /// <summary> ��ǰ�������ҳ����� </summary>
    property ActivePageIndex;

    /// <summary> ��ǰԤ����ҳ��� </summary>
    property PagePreviewFirst;

    /// <summary> ��ҳ�� </summary>
    property PageCount;

    /// <summary> ��ǰ������ڽڵ���� </summary>
    property ActiveSectionIndex;

    /// <summary> ˮƽ������ </summary>
    property HScrollBar;

    /// <summary> ��ֱ������ </summary>
    property VScrollBar;

    /// <summary> ����ֵ </summary>
    property Zoom;

    /// <summary> ��ǰ�ĵ����н� </summary>
    property Sections;

    /// <summary> �Ƿ���ʾ��ǰ��ָʾ�� </summary>
    property ShowLineActiveMark;

    /// <summary> �Ƿ���ʾ�к� </summary>
    property ShowLineNo;

    /// <summary> �Ƿ���ʾ�»��� </summary>
    property ShowUnderLine;

    /// <summary> ��ǰ�ĵ��Ƿ��б仯 </summary>
    property IsChanged;

    /// <summary> ���༭ֻ��״̬��Dataʱ���� </summary>
    property OnCanNotEdit: TNotifyEvent read FOnCanNotEdit write FOnCanNotEdit;

    /// <summary> ��������ǰ���� </summary>
    property OnCopyRequest: THCCopyPasteEvent read FOnCopyRequest write FOnCopyRequest;

    /// <summary> ճ������ǰ���� </summary>
    property OnPasteRequest: THCCopyPasteEvent read FOnPasteRequest write FOnPasteRequest;

    property OnCopyAsStream: THCCopyPasteStreamEvent read FOnCopyAsStream write FOnCopyAsStream;

    property OnPasteFromStream: THCCopyPasteStreamEvent read FOnPasteFromStream write FOnPasteFromStream;

    /// <summary> ����Ԫ��Ҫͬ������ʱ���� </summary>
    property OnSyncDeItem: TSyncDeItemEvent read FOnSyncDeItem write FOnSyncDeItem;

    /// <summary> ����Ԫ��Ҫ���﷨����������ʱ���� </summary>
    property OnSyntaxCheck: TDataDomainItemNoEvent read FOnSyntaxCheck write FOnSyntaxCheck;

    /// <summary> ����Ԫ�����﷨����ʱ���� </summary>
    property OnSyntaxPaint: TSyntaxPaintEvent read FOnSyntaxPaint write FOnSyntaxPaint;
  published
    { Published declarations }

    /// <summary> �����µ�Item����ʱ���� </summary>
    property OnSectionCreateItem;

    /// <summary> �����µ�Item����ʱ���� </summary>
    property OnSectionItemInsert;

    /// <summary> Item���ƿ�ʼǰ���� </summary>
    property OnSectionDrawItemPaintBefor;

    /// <summary> Item������ɺ󴥷� </summary>
    property OnSectionDrawItemPaintAfter;

    /// <summary> ��ҳü����ʱ���� </summary>
    property OnSectionPaintHeaderAfter;

    /// <summary> ��ҳ�Ż���ʱ���� </summary>
    property OnSectionPaintFooterAfter;

    /// <summary> ��ҳ�����ʱ���� </summary>
    property OnSectionPaintPageAfter;

    /// <summary> ����ҳ����ǰ���� </summary>
    property OnSectionPaintPaperBefor;

    /// <summary> ����ҳ���ƺ󴥷� </summary>
    property OnSectionPaintPaperAfter;

    /// <summary> ��ֻ�������б仯ʱ���� </summary>
    property OnSectionReadOnlySwitch;

    /// <summary> ������ʾģʽ��ҳ�桢Web </summary>
    property ViewModel;

    /// <summary> �Ƿ���ݿ���Զ��������ű��� </summary>
    property AutoZoom;

    /// <summary> ����Section�Ƿ�ֻ�� </summary>
    property ReadOnly;

    /// <summary> ��갴��ʱ���� </summary>
    property OnMouseDown;

    /// <summary> ��굯��ʱ���� </summary>
    property OnMouseUp;

    /// <summary> ���λ�øı�ʱ���� </summary>
    property OnCaretChange;

    /// <summary> ��ֱ����������ʱ���� </summary>
    property OnVerScroll;

    /// <summary> �ĵ����ݱ仯ʱ���� </summary>
    property OnChange;

    /// <summary> �ĵ�Change״̬�л�ʱ���� </summary>
    property OnChangedSwitch;

    /// <summary> �����ػ濪ʼʱ���� </summary>
    property OnPaintViewBefor;

    /// <summary> �����ػ�����󴥷� </summary>
    property OnPaintViewAfter;

    property PopupMenu;

    property Align;
  end;

/// <summary> ע��HCEmrView�ؼ����ؼ���� </summary>
procedure Register;

implementation

uses
  SysUtils, Forms, HCPrinters, HCTextStyle, HCParaStyle, HCEmrViewLite;

procedure Register;
begin
  RegisterComponents('HCEmrViewVCL', [THCEmrView]);
end;

{ TEmrView }

function THCEmrView.CanNotEdit: Boolean;
begin
  Result := (not Self.ActiveSection.ActiveData.CanEdit) or (not (Self.ActiveSectionTopLevelData as THCRichData).CanEdit);
  if Result and Assigned(FOnCanNotEdit) then
    FOnCanNotEdit(Self);
end;

function THCEmrView.CheckDeGroupEnd(const AData: THCViewData;
  const AItemNo: Integer; const ADeIndex: string): Boolean;
var
  vDeGroup: TDeGroup;
begin
  Result := False;
  if AData.Items[AItemNo] is TDeGroup then
  begin
    vDeGroup := AData.Items[AItemNo] as TDeGroup;
    Result := (vDeGroup.MarkType = TMarkType.cmtEnd)
      and (vDeGroup[TDeProp.Index] = ADeIndex);
  end;
end;

function THCEmrView.CheckDeGroupStart(const AData: THCViewData;
  const AItemNo: Integer; const ADeIndex: string): Boolean;
var
  vDeGroup: TDeGroup;
begin
  Result := False;
  if AData.Items[AItemNo] is TDeGroup then
  begin
    vDeGroup := AData.Items[AItemNo] as TDeGroup;
    Result := (vDeGroup.MarkType = TMarkType.cmtBeg)
      and (vDeGroup[TDeProp.Index] = ADeIndex);
  end;
end;

constructor THCEmrView.Create(AOwner: TComponent);
begin
  FHideTrace := False;
  FTrace := False;
  FSecret := False;
  FTraceInfoAnnotate := True;
  FIgnoreAcceptAction := False;
  FTraceCount := 0;
  FDesignMode := False;
  HCDefaultTextItemClass := TDeItem;
  HCDefaultDomainItemClass := TDeGroup;
  inherited Create(AOwner);
  Self.Width := 100;
  Self.Height := 100;
  FDeDoneColor := clBtnFace;  // Ԫ����д�󱳾�ɫ
  FDeUnDoneColor := $0080DDFF;  // Ԫ��δ��дʱ����ɫ
  FDeHotColor := $00F4E0CC;
  FPageBlankTip := '';  // '--------��ҳ���¿հ�--------'
  Self.Style.DefaultTextStyle.Size := GetFontSize('С��');
  Self.Style.DefaultTextStyle.Family := '����';
  Self.HScrollBar.AddStatus(200);
end;

destructor THCEmrView.Destroy;
begin
  inherited Destroy;
end;

procedure THCEmrView.DoCopyAsStream(const AStream: TStream);
begin
  if Assigned(FOnCopyAsStream) then
    FOnCopyAsStream(AStream)
  else
    inherited DoCopyAsStream(AStream);
end;

function THCEmrView.DoCopyRequest(const AFormat: Word): Boolean;
begin
  if Assigned(FOnCopyRequest) then
    Result := FOnCopyRequest(AFormat)
  else
    Result := inherited DoCopyRequest(AFormat);
end;

function THCEmrView.DoInsertText(const AText: string): Boolean;
begin
  Result := False;
  if CanNotEdit then Exit;

  if FTrace then
  begin
    InsertEmrTraceItem(AText);
    Result := True;
  end
  else
    Result := inherited DoInsertText(AText);
end;

function THCEmrView.DoPasteFromStream(const AStream: TStream): Boolean;
begin
  if Assigned(FOnPasteFromStream) then
    Result := FOnPasteFromStream(AStream)
  else
    Result := inherited DoPasteFromStream(AStream);
end;

function THCEmrView.DoPasteRequest(const AFormat: Word): Boolean;
var
  vItem: THCCustomItem;
begin
  vItem := Self.ActiveSectionTopLevelData.GetActiveItem;
  if (vItem is TDeItem) and (vItem as TDeItem).IsElement then
  begin
    if AFormat <> CF_TEXT then
    begin
      Result := False;
      Exit;
    end;
  end;

  if Assigned(FOnPasteRequest) then
    Result := FOnPasteRequest(AFormat)
  else
    Result := inherited DoPasteRequest(AFormat);
end;

function THCEmrView.DoSectionCanEdit(const Sender: TObject): Boolean;
var
  vViewData: THCViewData;
begin
  vViewData := Sender as THCViewData;
  if (vViewData.ActiveDomain <> nil) and (vViewData.ActiveDomain.BeginNo >= 0) then
    Result := not (vViewData.Items[vViewData.ActiveDomain.BeginNo] as TDeGroup).ReadOnly
  else
    Result := True;
end;

procedure THCEmrView.DoSectionCaretItemChanged(const Sender: TObject;
  const AData: THCCustomData; const AItem: THCCustomItem);
var
  vActiveItem: THCCustomItem;
  vDeItem: TDeItem;
  vDeGroup: TDeGroup;
  vDeEdit: TDeEdit;
  vDeCombobox: TDeCombobox;
  vDeDateTimePicker: TDeDateTimePicker;
  vInfo: string;
begin
  vInfo := '';
  vActiveItem := Self.GetTopLevelItem;
  if vActiveItem <> nil then
  begin
    if Self.ActiveSection.ActiveData.ActiveDomain.BeginNo >= 0 then
    begin
      vDeGroup := Self.ActiveSection.ActiveData.Items[
        Self.ActiveSection.ActiveData.ActiveDomain.BeginNo] as TDeGroup;

      vInfo := vDeGroup[TDeProp.Name] + '(' + vDeGroup[TDeProp.Index] + ')';
    end;

    if vActiveItem is TDeItem then
    begin
      vDeItem := vActiveItem as TDeItem;
      if vDeItem.StyleEx <> cseNone then
        vInfo := vInfo + '-' + vDeItem.GetHint
      else
      if vDeItem.IsElement then
      begin
        if vInfo <> '' then
          vInfo := vInfo + ' > ' + vDeItem[TDeProp.Name] + '(' + vDeItem[TDeProp.Index] + ')'
        else
          vInfo := vDeItem[TDeProp.Name] + '(' + vDeItem[TDeProp.Index] + ')';
      end;
    end
    else
    if vActiveItem is TDeEdit then
    begin
      vDeEdit := vActiveItem as TDeEdit;
      if vInfo <> '' then
        vInfo := vInfo + ' > ' + vDeEdit[TDeProp.Name] + '(' + vDeEdit[TDeProp.Index] + ')'
      else
        vInfo := vDeEdit[TDeProp.Name] + '(' + vDeEdit[TDeProp.Index] + ')';
    end
    else
    if vActiveItem is TDeCombobox then
    begin
      vDeCombobox := vActiveItem as TDeCombobox;
      if vInfo <> '' then
        vInfo := vInfo + ' > ' + vDeCombobox[TDeProp.Name] + '(' + vDeCombobox[TDeProp.Index] + ')'
      else
        vInfo := vDeCombobox[TDeProp.Name] + '(' + vDeCombobox[TDeProp.Index] + ')';
    end
    else
    if vActiveItem is TDeDateTimePicker then
    begin
      vDeDateTimePicker := vActiveItem as TDeDateTimePicker;
      if vInfo <> '' then
        vInfo := vInfo + ' > ' + vDeDateTimePicker[TDeProp.Name] + '(' + vDeDateTimePicker[TDeProp.Index] + ')'
      else
        vInfo := vDeDateTimePicker[TDeProp.Name] + '(' + vDeDateTimePicker[TDeProp.Index] + ')';
    end;
  end;

  Self.HScrollBar.Statuses[1].Text := vInfo;

  inherited DoSectionCaretItemChanged(Sender, AData, AItem);
end;

procedure THCEmrView.DoSectionCreateItem(Sender: TObject);
begin
  if (not Style.States.Contain(hosLoading)) and FTrace then
    (Sender as TDeItem).StyleEx := TStyleExtra.cseAdd;

  inherited DoSectionCreateItem(Sender);
end;

function THCEmrView.DoSectionCreateStyleItem(const AData: THCCustomData;
  const AStyleNo: Integer): THCCustomItem;
begin
  Result := HCEmrElementItem.CreateEmrStyleItem(AData, AStyleNo);
end;

function THCEmrView.DoSectionAcceptAction(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean;
var
  vItem: THCCustomItem;
  vDeItem: TDeItem;
begin
  if FIgnoreAcceptAction then Exit(True);

  //THCViewData(AData).GetDomainFrom(AItemNo, AOffset, vDomainInfo);
  //THCViewData(AData).GetDomainStackFrom(AItemNo, AOffset);
  //if (AData.Items[vDomainInfo.BeginNo] as TDeGroup).ReadOnly then

  Result := inherited DoSectionAcceptAction(Sender, AData, AItemNo, AOffset, AAction);
  if Result then
  begin
    case AAction of
      actBackDeleteText,
      actDeleteText:
        begin
          if (not FDesignMode) and (AData.Items[AItemNo] is TDeItem) then
          begin
            vDeItem := AData.Items[AItemNo] as TDeItem;

            if vDeItem.IsElement and (vDeItem.Length = 1) and not vDeItem.DeleteAllow then
            begin
              if vDeItem[TDeProp.Name] <> '' then
                Self.SetActiveItemText(vDeItem[TDeProp.Name])
              else
                Self.SetActiveItemText('δ��д');

              vDeItem.AllocValue := False;

              Result := False;
            end;
          end;
        end;

      actSetItemText:
        begin
          if AData.Items[AItemNo] is TDeItem then
          begin
            vDeItem := AData.Items[AItemNo] as TDeItem;
            vDeItem.AllocValue := True;
          end;
        end;

      actReturnItem:
        begin
          if AData.Items[AItemNo] is TDeItem then
          begin
            vDeItem := AData.Items[AItemNo] as TDeItem;
            if (AOffset > 0) and (AOffset < vDeItem.Length) and vDeItem.IsElement then
              Result := False;
          end;
        end;

      actDeleteItem:
        begin
          if not FDesignMode then  // �����ģʽ������ֱ��ɾ��
          begin
            vItem := AData.Items[AItemNo];
            if vItem is TDeGroup then
              Result := False
            else
            if vItem is TDeItem then
              Result := (vItem as TDeItem).DeleteAllow
            else
            if vItem is TDeTable then
              Result := (vItem as TDeTable).DeleteAllow
            else
            if vItem is TDeCheckBox then
              Result := (vItem as TDeCheckBox).DeleteAllow
            else
            if vItem is TDeEdit then
              Result := (vItem as TDeEdit).DeleteAllow
            else
            if vItem is TDeCombobox then
              Result := (vItem as TDeCombobox).DeleteAllow
            else
            if vItem is TDeDateTimePicker then
              Result := (vItem as TDeDateTimePicker).DeleteAllow
            else
            if vItem is TDeRadioGroup then
              Result := (vItem as TDeRadioGroup).DeleteAllow
            else
            if vItem is TDeFloatBarCodeItem then
              Result := (vItem as TDeFloatBarCodeItem).DeleteAllow
            else
            if vItem is TDeImageItem then
              Result := (vItem as TDeImageItem).DeleteAllow;
          end;
        end;
    end;
  end;
end;

procedure THCEmrView.DoSectionDrawItemPaintAfter(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect;
  const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);

  procedure DrawBlankTip_(const ALeft, ATop, ARight: Integer);
  begin
    if ATop + 14 <= ADataDrawBottom then
    begin
      ACanvas.Font.Size := 12;
      ACanvas.Font.Style := [];
      ACanvas.Font.Color := clBlack;
      ACanvas.TextOut(ALeft + ((ARight - ALeft) - ACanvas.TextWidth(FPageBlankTip)) div 2,
        ATop, FPageBlankTip);
    end;
  end;

  procedure DrawTraceHint_(const ADeItem: TDeItem);
  var
    vSize: TSize;
    vRect: TRect;
    vTrace: string;
  begin
    ACanvas.Font.Size := 12;
    ACanvas.Font.Color := clBlack;
    vTrace := ADeItem[TDeProp.Trace];
    vSize := ACanvas.TextExtent(vTrace);
    vRect := Bounds(ADrawRect.Left, ADrawRect.Top - vSize.cy - 5, vSize.cx, vSize.cy);
    if vRect.Right > ADataDrawRight then
      OffsetRect(vRect, ADataDrawRight - vRect.Right, 0);

    if ADeItem.StyleEx = TStyleExtra.cseDel then
      ACanvas.Brush.Color := clMedGray
    else
      ACanvas.Brush.Color := clInfoBk;
    //ACanvas.FillRect(vRect);
    //ACanvas.Pen.Color := clBlue;
    ACanvas.TextRect(vRect, vTrace);
    ACanvas.Pen.Color := clGray;
    ACanvas.Pen.Width := 2;
    ACanvas.MoveTo(vRect.Left + 2, vRect.Bottom + 1);
    ACanvas.LineTo(vRect.Right, vRect.Bottom + 1);
    ACanvas.MoveTo(vRect.Right + 1, vRect.Top + 2);
    ACanvas.LineTo(vRect.Right + 1, vRect.Bottom + 1);
  end;

var
  vItem: THCCustomItem;
  vDeItem: TDeItem;
  vDrawAnnotate: THCDrawAnnotateDynamic;

  vDrawItem: THCCustomDrawItem;
  vSecretLow, vSecretHi: Integer;
begin
  if APaintInfo.Print then  // ��ӡʱû����д��������Ԫ����ӡ
  begin
    vItem := AData.Items[AItemNo];
    if vItem.StyleNo > THCStyle.Null then
    begin
      vDeItem := vItem as TDeItem;
      if vDeItem.IsElement and (not vDeItem.AllocValue) then
      begin
        ACanvas.Brush.Color := clWhite;
        ACanvas.FillRect(ADrawRect);
        Exit;
      end;
    end;
  end;

  if (not FHideTrace) and (FTraceCount > 0) then  // ��ʾ�ۼ����кۼ�
  begin
    vItem := AData.Items[AItemNo];
    if vItem.StyleNo > THCStyle.Null then
    begin
      vDeItem := vItem as TDeItem;
      if (vDeItem.StyleEx <> TStyleExtra.cseNone) then  // �Ǻۼ�
      begin
        if FTraceInfoAnnotate then  // ����ע��ʽ��ʾ�ۼ�
        begin
          vDrawAnnotate := THCDrawAnnotateDynamic.Create;
          vDrawAnnotate.DrawRect := ADrawRect;
          vDrawAnnotate.Title := vDeItem.GetHint;
          vDrawAnnotate.Text := AData.GetDrawItemText(ADrawItemNo);

          Self.AnnotatePre.AddDrawAnnotate(vDrawAnnotate);
          //Self.VScrollBar.AddAreaPos(AData.DrawItems[ADrawItemNo].Rect.Top, ADrawRect.Height);
        end
        else
        if ADrawItemNo = (AData as THCRichData).HotDrawItemNo then  // ��괦DrawItem
          DrawTraceHint_(vDeItem);
      end;
    end;
  end;

  if FSecret then
  begin
    vItem := AData.Items[AItemNo];
    if (vItem.StyleNo > THCStyle.Null) and ((vItem as TDeItem)[TDeProp.Secret] <> '') then  // �����ﴦ����ʵʱ��Ч�ʲ��ߣ�����DeItem��Load���������Ե���ʼ�ͽ������浽�ֶ���������ӿռ�ռ�ã�ռ�ռ仹��ռʱ���أ�
    begin
      TDeItem.GetSecretRange((vItem as TDeItem)[TDeProp.Secret], vSecretLow, vSecretHi);
      if vSecretLow > 0 then
      begin
        if vSecretHi < 0 then
          vSecretHi := vItem.Length;

        vDrawItem := AData.DrawItems[ADrawItemNo];
        if vSecretLow <= vDrawItem.CharOffsetEnd then  // =�Ǵ���Low��Hi��ͬ��Hi��OffsetEnd�غ�ʱ�����
        begin
          if vSecretLow < vDrawItem.CharOffs then
            vSecretLow := vDrawItem.CharOffs;

          if vSecretHi > vDrawItem.CharOffsetEnd then
            vSecretHi := vDrawItem.CharOffsetEnd;

          vSecretLow := vSecretLow - vDrawItem.CharOffs + 1;
          if vSecretLow > 0 then
            Dec(vSecretLow);  // תΪ���Offset

          vSecretHi := vSecretHi - vDrawItem.CharOffs + 1;  // ����ת���Offset����Ϊ�����ں���

          if vSecretHi >= 0 then
          begin
            //ACanvas.Brush.Color := clBtnFace;
            ACanvas.Brush.Style := bsDiagCross;
            ACanvas.FillRect(Rect(ADrawRect.Left + AData.GetDrawItemOffsetWidth(ADrawItemNo, vSecretLow), ADrawRect.Top,
              ADrawRect.Left + AData.GetDrawItemOffsetWidth(ADrawItemNo, vSecretHi), ADrawRect.Bottom));
            ACanvas.Brush.Style := bsSolid;
          end;
        end;
      end;
    end;
  end;

  if (not APaintInfo.Print) and (AData.Items[AItemNo] is TDeGroup) then  // ���Ʋ��̵�ǰ��ָʾ��ͷ
  begin
    if (AData.Items[AItemNo] as TDeGroup).MarkType = TMarkType.cmtBeg then
    begin
      if (AData.Items[AItemNo] as TDeGroup)[TGroupProp.SubType] = TSubType.Proc then
      begin
        if (AItemNo > 0) and (AData.Items[AItemNo - 1] is TDeGroup)
          and ((AData.Items[AItemNo - 1] as TDeGroup)[TGroupProp.SubType] = TSubType.Proc)
        then
          HCDrawArrow(ACanvas, clBlue, ADrawRect.Left - 10, ADrawRect.Top, 0);

        HCDrawArrow(ACanvas, clBlue, ADrawRect.Left - 10, ADrawRect.Top + 12, 1);
      end;
    end;
  end;

  if (FPageBlankTip <> '') and (AData is THCPageData) then
  begin
    if ADrawItemNo < AData.DrawItems.Count - 1 then
    begin
      if AData.Items[AData.DrawItems[ADrawItemNo + 1].ItemNo].PageBreak then
        DrawBlankTip_(ADataDrawLeft, ADrawRect.Top + ADrawRect.Height + AData.GetLineBlankSpace(ADrawItemNo), ADataDrawRight);
    end
    else
      DrawBlankTip_(ADataDrawLeft, ADrawRect.Top + ADrawRect.Height + AData.GetLineBlankSpace(ADrawItemNo), ADataDrawRight);
  end;

  inherited DoSectionDrawItemPaintAfter(Sender, AData, AItemNo, ADrawItemNo, ADrawRect,
    ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
end;

procedure THCEmrView.DoSectionDrawItemPaintBefor(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
  const ADrawRect: TRect; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom,
  ADataScreenTop, ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
var
  vDeItem: TDeItem;
  vTop: Integer;
  vTextHeight: Integer;
begin
  if APaintInfo.Print then Exit;

  if (AData.Items[AItemNo] is TDeGroup) then
  begin
    if ((AData.Items[AItemNo] as TDeGroup).MarkType = TMarkType.cmtBeg)
      and ((AData.Items[AItemNo] as TDeGroup)[TGroupProp.SubType] = TSubType.Proc)
    then  // ���Ʋ��̵�ǰ��ָ���
    begin
      ACanvas.Pen.Style := psDashDotDot;
      ACanvas.Pen.Color := clBlue;
      ACanvas.MoveTo(ADataDrawLeft, ADrawRect.Top - 1);
      ACanvas.LineTo(ADataDrawRight, ADrawRect.Top - 1);
    end;
  end;

  if not (AData.Items[AItemNo] is TDeItem) then Exit;

  vDeItem := AData.Items[AItemNo] as TDeItem;
  if not vDeItem.Selected then
  begin
    if vDeItem.IsElement then  // ������Ԫ
    begin
      if vDeItem.MouseIn or vDeItem.Active then  // ����������������
      begin
        ACanvas.Brush.Color := FDeHotColor;
        ACanvas.FillRect(ADrawRect);
      end
      else
      if FDesignMode then  // ���ģʽ
      begin
        if vDeItem.AllocValue then  // �Ѿ���д����
          ACanvas.Brush.Color := FDeDoneColor
        else  // û��д��
          ACanvas.Brush.Color := FDeUnDoneColor;

        ACanvas.FillRect(ADrawRect);
      end
      else  // �����ģʽ
      begin
        if vDeItem.OutOfRang then  // ����Χ
        begin
          ACanvas.Brush.Color := clRed;
          ACanvas.FillRect(ADrawRect);
        end
        else  // û����Χ
        begin
          if vDeItem.AllocValue then  // �Ѿ���д����
            ACanvas.Brush.Color := FDeDoneColor
          else  // û��д��
            ACanvas.Brush.Color := FDeUnDoneColor;

          ACanvas.FillRect(ADrawRect);
        end;
      end;
    end
    else  // ��������Ԫ
    if FDesignMode or vDeItem.MouseIn or vDeItem.Active then
    begin
      if vDeItem.EditProtect or vDeItem.CopyProtect then
      begin
        ACanvas.Brush.Color := clBtnFace;
        ACanvas.FillRect(ADrawRect);
      end;
    end;
  end;

  if not FHideTrace then  // ��ʾ�ۼ�
  begin
    case vDeItem.StyleEx of  // �ۼ�
      //cseNone: ;
      cseDel:
        begin
          vTextHeight := Style.TextStyles[vDeItem.StyleNo].FontHeight;
          case Style.ParaStyles[vDeItem.ParaNo].AlignVert of
            pavTop: vTop := ADrawRect.Top + vTextHeight div 2;
            pavCenter: vTop := ADrawRect.Top + (ADrawRect.Bottom - ADrawRect.Top) div 2;
          else
            vTop := ADrawRect.Bottom - vTextHeight div 2;
          end;

          // ����ɾ����
          ACanvas.Pen.Style := psSolid;
          ACanvas.Pen.Color := clRed;
          ACanvas.Pen.Width := 1;
          //vTop := vTop + (ADrawRect.Bottom - vTop) div 2;
          ACanvas.MoveTo(ADrawRect.Left, vTop - 1);
          ACanvas.LineTo(ADrawRect.Right, vTop - 1);
          ACanvas.MoveTo(ADrawRect.Left, vTop + 2);
          ACanvas.LineTo(ADrawRect.Right, vTop + 2);
        end;

      cseAdd:
        begin
          vTextHeight := Style.TextStyles[vDeItem.StyleNo].FontHeight;
          case Style.ParaStyles[vDeItem.ParaNo].AlignVert of
            pavTop: vTop := ADrawRect.Top + vTextHeight;
            pavCenter: vTop := ADrawRect.Top + (ADrawRect.Bottom - ADrawRect.Top + vTextHeight) div 2;
          else
            vTop := ADrawRect.Bottom;
          end;

          ACanvas.Pen.Style := psSolid;
          ACanvas.Pen.Color := clBlue;
          ACanvas.Pen.Width := 1;
          ACanvas.MoveTo(ADrawRect.Left, vTop);
          ACanvas.LineTo(ADrawRect.Right, vTop);
        end;
    end;
  end;
end;

procedure THCEmrView.DoSectionDrawItemPaintContent(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
  const ADrawText: string; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom,
  ADataScreenTop, ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
var
  vDeItem: TDeItem;
  vRect: TRect;
  vDT, vDrawSyntax: Boolean;
  i, vOffset, vOffsetEnd, vSyOffset, vSyOffsetEnd, vStart, vLen: Integer;
begin
  if APaintInfo.Print then Exit;
  if not (AData.Items[AItemNo] is TDeItem) then Exit;

  vDeItem := AData.Items[AItemNo] as TDeItem;
  if (vDeItem.SyntaxCount > 0) and (not vDeItem.IsSelectComplate) then
  begin
    vOffset := AData.DrawItems[ADrawItemNo].CharOffs;
    vOffsetEnd := AData.DrawItems[ADrawItemNo].CharOffsetEnd;

    for i := 0 to vDeItem.Syntaxs.Count - 1 do
    begin
      vSyOffset := vDeItem.Syntaxs[i].Offset;
      if vSyOffset > vOffsetEnd then  // �﷨������ʼ�ڴ�DrawItem֮��
        Continue;

      vSyOffsetEnd := vSyOffset + vDeItem.Syntaxs[i].Length - 1;
      if vSyOffsetEnd < vOffset then  // �﷨��������ڴ�DrawItem֮ǰ
        Continue;

      vDrawSyntax := False;
      if (vSyOffset <= vOffset) and (vSyOffsetEnd >= vOffsetEnd) then  // ���������DrawItem
      begin
        vDrawSyntax := True;
        vRect.Left := AClearRect.Left;
        vRect.Right := AClearRect.Right;
      end
      else
      if vSyOffset >= vOffset then  // �н���
      begin
        vDrawSyntax := True;
        if vSyOffsetEnd <= vOffsetEnd then  // ������DrawItem�м�
        begin
          vStart := vSyOffset - vOffset;
          vLen := vDeItem.Syntaxs[i].Length;
          vRect.Left := AClearRect.Left //+ ACanvas.TextWidth(System.Copy(ADrawText, 1, vStart - 1));
            + AData.GetDrawItemOffsetWidth(ADrawItemNo, vStart, ACanvas);
          vRect.Right := AClearRect.Left //+ ACanvas.TextWidth(System.Copy(ADrawText, 1, vStart + vLen - 1));
            + AData.GetDrawItemOffsetWidth(ADrawItemNo, vStart + vLen, ACanvas);
        end
        else  // DrawItem�������һ����
        begin
          vRect.Left := AClearRect.Left
            + AData.GetDrawItemOffsetWidth(ADrawItemNo, vSyOffset - vOffset, ACanvas);
          vRect.Right := AClearRect.Right;
        end;
      end
      else  // vSyOffset < vOffset
      if vSyOffsetEnd <= vOffsetEnd then  // �н�����DrawItem�������һ����
      begin
        vDrawSyntax := True;
        vRect.Left := AClearRect.Left;
        vRect.Right := AClearRect.Left //+ ACanvas.TextWidth(System.Copy(ADrawText, 1, vLen));
          + AData.GetDrawItemOffsetWidth(ADrawItemNo, vSyOffsetEnd - vOffset + 1, ACanvas);
      end;

      if vDrawSyntax then  // ��DrawItem�����﷨����
      begin
        vRect.Top := AClearRect.Top;
        vRect.Bottom := AClearRect.Bottom;

        if Assigned(FOnSyntaxPaint) then
          FOnSyntaxPaint(AData, AItemNo, ADrawText, vDeItem.Syntaxs[i], vRect, ACanvas)
        else
        begin
          case vDeItem.Syntaxs[i].Problem of
            espContradiction: ACanvas.Pen.Color := clRed;
            espWrong: ACanvas.Pen.Color := clWebOrange;
          end;

          vDT := False;
          vStart := vRect.Left;
          ACanvas.MoveTo(vStart, vRect.Bottom);
          while vStart < vRect.Right do
          begin
            vStart := vStart + 2;
            if vStart > vRect.Right then
              vStart := vRect.Right;

            if not vDT then
              ACanvas.LineTo(vStart, vRect.Bottom + 2)
            else
              ACanvas.LineTo(vStart, vRect.Bottom);

            vDT := not vDT;
          end;
        end;
      end;
    end;
  end;
end;

procedure THCEmrView.DoSectionInsertItem(const Sender: TObject;
  const AData: THCCustomData; const AItem: THCCustomItem);
var
  vDeItem: TDeItem;
begin
  if AItem is TDeItem then
  begin
    vDeItem := AItem as TDeItem;
    //if AData.Style.States.Contain(THCState.hosPasting) then
    //  DoPasteItem();
    if vDeItem.StyleEx <> TStyleExtra.cseNone then
    begin
      Inc(FTraceCount);
      if FTraceInfoAnnotate then
        Self.AnnotatePre.InsertDataAnnotate(nil);
    end
    else
      DoSyncDeItem(Sender, AData, AItem);
  end
  else
  if AItem is TDeEdit then
    DoSyncDeItem(Sender, AData, AItem)
  else
  if AItem is TDeCombobox then
    DoSyncDeItem(Sender, AData, AItem)
  else
  if AItem is TDeFloatBarCodeItem then
    DoSyncDeItem(Sender, AData, AItem)
  else
  if AItem is TDeImageItem then
    DoSyncDeItem(Sender, AData, AItem);

  inherited DoSectionInsertItem(Sender, AData, AItem);
end;

procedure THCEmrView.DoSectionItemMouseDown(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, AOffset: Integer;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vItem: THCCustomItem;
begin
  inherited DoSectionItemMouseDown(Sender, AData, AItemNo, AOffset, Button, Shift, X, Y);
  if not (Sender as THCCustomSection).SelectExists then
  begin
    vItem := AData.Items[aItemNo];
    if ((vItem is TDeItem) and AData.SelectInfo.StartRestrain) then  // ��ͨ��Լ��ѡ�е�,���������,��������Ԫ��������ͨ����
      vItem.Active := False;
  end;
end;

function THCEmrView.DoSectionPaintDomainRegion(const Sender: TObject;
  const AData: THCCustomData; const AItemNo: Integer): Boolean;
begin
  Result := (AData.Items[AItemNo] as TDeGroup)[TGroupProp.SubType] <> TSubType.Proc;
end;

procedure THCEmrView.DoSectionRemoveItem(const Sender: TObject;
  const AData: THCCustomData; const AItem: THCCustomItem);
var
  vDeItem: TDeItem;
begin
  if AItem is TDeItem then
  begin
    vDeItem := AItem as TDeItem;

    if vDeItem.StyleEx <> TStyleExtra.cseNone then
    begin
      Dec(FTraceCount);
      if FTraceInfoAnnotate then
        Self.AnnotatePre.RemoveDataAnnotate(nil);
    end;
  end;

  inherited DoSectionRemoveItem(Sender, AData, AItem);
end;

function THCEmrView.DoSectionSaveItem(const Sender: TObject;
  const AData: THCCustomData; const AItemNo: Integer): Boolean;
begin
  Result := inherited DoSectionSaveItem(Sender, AData, AItemNo);
  if Style.States.Contain(THCState.hosCopying) then  // ���Ʊ���
  begin
    if (AData.Items[AItemNo] is TDeGroup) and (not FDesignMode) then  // �����ģʽ������������
      Result := False
    else
    if AData.Items[AItemNo] is TDeItem then
      Result := not (AData.Items[AItemNo] as TDeItem).CopyProtect;  // �Ƿ��ֹ����
  end;
end;

procedure THCEmrView.DoSyncDeItem(const Sender: TObject;
  const AData: THCCustomData; const AItem: THCCustomItem);
begin
  if Assigned(FOnSyncDeItem) then
    FOnSyncDeItem(Sender, AData, AItem);
end;

procedure THCEmrView.DoSyntaxCheck(const AData: THCCustomData; const AItemNo,
  ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean);
begin
  //if Assigned(FOnSyntaxCheck) then ����ǰ�Ѿ��ж���
  if AData.Items[AItemNo].StyleNo > THCStyle.Null then
    FOnSyntaxCheck(AData, ADomainStack, AItemNo);
end;

function THCEmrView.FindSameDeItem(const ADeItem: TDeItem): TDeItem;
var
  vItemTraverse: THCItemTraverse;
  vDeItem, vResult: TDeItem;
  vFind: Boolean;
begin
  Result := nil;
  vResult := nil;

  vItemTraverse := THCItemTraverse.Create;
  try
    vItemTraverse.Tag := 0;
    vItemTraverse.Areas := [saPage];
    vItemTraverse.Process := procedure (const AData: THCCustomData; const AItemNo,
      ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean)
    begin
      if AData.Items[AItemNo].StyleNo > THCStyle.Null then
      begin
        vDeItem := AData.Items[AItemNo] as TDeItem;
        if vDeItem[TDeProp.Index] = ADeItem[TDeProp.Index] then
        begin
          if vDeItem.AllocValue then
          begin
            vResult := vDeItem;
            AStop := True;
          end;
        end;
      end;
    end;

    Self.TraverseItem(vItemTraverse);
  finally
    vItemTraverse.Free;
  end;

  Result := vResult;
end;

function THCEmrView.GetDataForwardDeGroupText(const AData: THCViewData;
  const ADeGroupStartNo: Integer): string;
var
  vBeginNo, vEndNo: Integer;
  vDeIndex: string;
begin
  Result := '';

  vBeginNo := ADeGroupStartNo;
  vEndNo := -1;
  vDeIndex := (AData.Items[ADeGroupStartNo] as TDeGroup)[TDeProp.Index];

  GetDataDeGroupItemNo(AData, vDeIndex, True, vBeginNo, vEndNo);
  if vEndNo > 0 then
    Result := GetDataDeGroupText(AData, vBeginNo, vEndNo);
end;

function THCEmrView.GetDeItemProperty(const ADeIndex, APropName: string;
  var APropValue: string): Boolean;
var
  vItemTraverse: THCItemTraverse;
  vItem: THCCustomItem;
  vText: string;
  vResult: Boolean;
begin
  Result := False;
  vResult := False;
  vText := '';

  vItemTraverse := THCItemTraverse.Create;
  try
    vItemTraverse.Tag := 0;
    vItemTraverse.Areas := [saPage, saHeader, saFooter];
    vItemTraverse.Process := procedure (const AData: THCCustomData; const AItemNo,
      ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean)
    begin
      vItem := AData.Items[AItemNo];
      if (vItem is TDeItem) and ((vItem as TDeItem)[TDeProp.Index] = ADeIndex) then
      begin
        if APropName = 'Text' Then
          vText := vItem.Text
        else
          vText := (vItem as TDeItem)[APropName];

        vResult := True;
        AStop := True;
      end;
    end;

    Self.TraverseItem(vItemTraverse);
    if vResult then
    begin
      APropValue := vText;
      Result := vResult;
    end;
  finally
    vItemTraverse.Free;
  end;
end;

function THCEmrView.GetDeItemText(const ADeIndex: string;
  var AText: string): Boolean;
begin
  Result := GetDeItemProperty(ADeIndex, 'Text', AText);
end;

procedure THCEmrView.GetDataDeGroupItemNo(const AData: THCViewData; const ADeIndex: string;
  const AForward: Boolean; var AStartNo, AEndNo: Integer);
var
  i, vBeginNo, vEndNo: Integer;
  vDeGroup: TDeGroup;
begin
  AEndNo := -1;
  vBeginNo := -1;
  vEndNo := -1;

  if AStartNo < 0 then
    AStartNo := 0;

  if AForward then  // ��AStartNo��ǰ��
  begin
    for i := AStartNo downto 0 do  // �ҽ�βItemNo
    begin
      if CheckDeGroupEnd(AData, i, ADeIndex) then
      begin
        vEndNo := i;
        Break;
      end;
    end;

    if vEndNo >= 0 then  // ����ǰ����ʼItemNo
    begin
      for i := vEndNo - 1 downto 0 do
      begin
        if CheckDeGroupStart(AData, i, ADeIndex) then
        begin
          vBeginNo := i;
          Break;
        end;
      end;
    end;
  end
  else  // ��AStartNo������
  begin
    for i := AStartNo to AData.Items.Count - 1 do  // ����ʼItemNo
    begin
      if CheckDeGroupStart(AData, i, ADeIndex) then
      begin
        vBeginNo := i;
        Break;
      end;
    end;

    if vBeginNo >= 0 then  // �ҽ�βItemNo
    begin
      for i := vBeginNo + 1 to AData.Items.Count - 1 do
      begin
        if CheckDeGroupEnd(AData, i, ADeIndex) then
        begin
          vEndNo := i;
          Break;
        end;
      end;
    end;
  end;

  if (vBeginNo >= 0) and (vEndNo >= 0) then
  begin
    AStartNo := vBeginNo;
    AEndNo := vEndNo;
  end
  else
    AStartNo := -1;
end;

procedure THCEmrView.GetDataDeGroupToStream(const AData: THCViewData;
  const ADeGroupStartNo, ADeGroupEndNo: Integer; const AStream: TStream);
begin
  _SaveFileFormatAndVersion(AStream);  // �ļ���ʽ�Ͱ汾
  Self.Style.SaveToStream(AStream);
  AData.SaveItemToStream(AStream, ADeGroupStartNo + 1, 0, ADeGroupEndNo - 1, AData.Items[ADeGroupEndNo - 1].Length);
end;

function THCEmrView.GetDataDeGroupText(const AData: THCViewData;
  const ADeGroupStartNo, ADeGroupEndNo: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := ADeGroupStartNo + 1 to ADeGroupEndNo - 1 do
  begin
    if AData.Items[i].ParaFirst then
      Result := Result + sLineBreak + AData.Items[i].Text
    else
      Result := Result + AData.Items[i].Text;
  end;
end;

function THCEmrView.InsertDeGroup(const ADeGroup: TDeGroup): Boolean;
begin
  Result := InsertDomain(ADeGroup);
end;

function THCEmrView.InsertDeItem(const ADeItem: TDeItem): Boolean;
begin
  Result := Self.InsertItem(ADeItem);
end;

procedure THCEmrView.InsertEmrTraceItem(const AText: string);
var
  vEmrTraceItem: TDeItem;
begin
  // ������Ӻۼ�Ԫ��
  vEmrTraceItem := TDeItem.CreateByText(AText);
  if Self.CurStyleNo < THCStyle.Null then
    vEmrTraceItem.StyleNo := 0
  else
    vEmrTraceItem.StyleNo := Self.CurStyleNo;

  vEmrTraceItem.ParaNo := Self.CurParaNo;
  vEmrTraceItem.StyleEx := TStyleExtra.cseAdd;

  Self.InsertItem(vEmrTraceItem);
end;

procedure THCEmrView.KeyDown(var Key: Word; Shift: TShiftState);
var
  vData: THCRichData;
  vText, vCurTrace: string;
  vStyleNo, vParaNo: Integer;
  vDeItem: TDeItem;
  vCurItem: THCCustomItem;
  vCurStyleEx: TStyleExtra;
begin
  if FTrace then  // ����
  begin
    if IsKeyDownEdit(Key) then
    begin
      if CanNotEdit then Exit;

      vText := '';
      vCurTrace := '';
      vStyleNo := THCStyle.Null;
      vParaNo := THCStyle.Null;
      vCurStyleEx := TStyleExtra.cseNone;

      vData := Self.ActiveSectionTopLevelData as THCRichData;
      if vData.SelectExists then
      begin
        Self.DisSelect;
        Exit;
      end;

      if vData.SelectInfo.StartItemNo < 0 then Exit;

      if vData.Items[vData.SelectInfo.StartItemNo].StyleNo < THCStyle.Null then
      begin
        if vData.SelectInfo.StartItemOffset = OffsetBefor then  // ����ǰ��
        begin
          if Key = VK_BACK then  // ��ɾ
          begin
            if vData.SelectInfo.StartItemNo = 0 then
              Exit  // ��һ����ǰ���򲻴���
            else  // ���ǵ�һ����ǰ��
            begin
              vData.SelectInfo.StartItemNo := vData.SelectInfo.StartItemNo - 1;
              vData.SelectInfo.StartItemOffset := vData.Items[vData.SelectInfo.StartItemNo].Length;
              Self.KeyDown(Key, Shift);
            end;
          end
          else
          if Key = VK_DELETE then  // ��ɾ
          begin
            vData.SelectInfo.StartItemOffset := OffsetAfter;
            //Self.KeyDown(Key, Shift);
          end
          else
            inherited KeyDown(Key, Shift);
        end
        else
        if vData.SelectInfo.StartItemOffset = OffsetAfter then  // �������
        begin
          if Key = VK_BACK then
          begin
            vData.SelectInfo.StartItemOffset := OffsetBefor;
            Self.KeyDown(Key, Shift);
          end
          else
          if Key = VK_DELETE then
          begin
            if vData.SelectInfo.StartItemNo = vData.Items.Count - 1 then
              Exit
            else
            begin
              vData.SelectInfo.StartItemNo := vData.SelectInfo.StartItemNo + 1;
              vData.SelectInfo.StartItemOffset := 0;
              Self.KeyDown(Key, Shift);
            end;
          end
          else
            inherited KeyDown(Key, Shift);
        end
        else
          inherited KeyDown(Key, Shift);

        Exit;
      end;

      // ȡ��괦���ı�
      with vData do
      begin
        if Key = VK_BACK then  // ��ɾ
        begin
          if (SelectInfo.StartItemNo = 0) and (SelectInfo.StartItemOffset = 0) then  // ��һ����ǰ���򲻴���
            Exit
          else  // ���ǵ�һ����ǰ��
          if SelectInfo.StartItemOffset = 0 then  // ��ǰ�棬�ƶ���ǰһ������洦��
          begin
            if Items[SelectInfo.StartItemNo].Text <> '' then  // ��ǰ�в��ǿ���
            begin
              SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
              SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;
              Self.KeyDown(Key, Shift);
            end
            else  // ���в�����ֱ��Ĭ�ϴ���
              inherited KeyDown(Key, Shift);

            Exit;
          end
          else  // ���ǵ�һ��Item��Ҳ������Item��ǰ��
          if Items[SelectInfo.StartItemNo] is TDeItem then  // �ı�
          begin
            vDeItem := Items[SelectInfo.StartItemNo] as TDeItem;
            vText := vDeItem.SubString(SelectInfo.StartItemOffset, 1);
            vStyleNo := vDeItem.StyleNo;
            vParaNo := vDeItem.ParaNo;
            vCurStyleEx := vDeItem.StyleEx;
            vCurTrace := vDeItem[TDeProp.Trace];
          end;
        end
        else
        if Key = VK_DELETE then  // ��ɾ
        begin
          if (SelectInfo.StartItemNo = Items.Count - 1)
            and (SelectInfo.StartItemOffset = Items[Items.Count - 1].Length)
          then  // ���һ��������򲻴���
            Exit
          else  // �������һ�������
          if SelectInfo.StartItemOffset = Items[SelectInfo.StartItemNo].Length then  // ����棬�ƶ�����һ����ǰ�洦��
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
            SelectInfo.StartItemOffset := 0;
            Self.KeyDown(Key, Shift);

            Exit;
          end
          else  // �������һ��Item��Ҳ������Item�����
          if Items[SelectInfo.StartItemNo] is TDeItem then  // �ı�
          begin
            vDeItem := Items[SelectInfo.StartItemNo] as TDeItem;
            vText := vDeItem.SubString(SelectInfo.StartItemOffset + 1, 1);
            vStyleNo := vDeItem.StyleNo;
            vParaNo := vDeItem.ParaNo;
            vCurStyleEx := vDeItem.StyleEx;
            vCurTrace := vDeItem[TDeProp.Trace];
          end;
        end;
      end;

      // ɾ�����������Ժۼ�����ʽ����
      Self.BeginUpdate;
      try
        inherited KeyDown(Key, Shift);

        if FTrace and (vText <> '') then  // ��ɾ��������
        begin
          if (vCurStyleEx = TStyleExtra.cseAdd) and (vCurTrace = '') then Exit;  // �����δ��Ч�ۼ�����ֱ��ɾ��

          // ����ɾ���ַ���Ӧ��Item
          vDeItem := TDeItem.CreateByText(vText);
          vDeItem.StyleNo := vStyleNo;  // Style.CurStyleNo;
          vDeItem.ParaNo := vParaNo;  // Style.CurParaNo;

          if (vCurStyleEx = TStyleExtra.cseDel) and (vCurTrace = '') then  // ԭ����ɾ��δ��Ч�ۼ�
            vDeItem.StyleEx := TStyleExtra.cseNone  // ȡ��ɾ���ۼ�
          else  // ����ɾ���ۼ�
            vDeItem.StyleEx := TStyleExtra.cseDel;

          // ����ɾ���ۼ�Item
          vCurItem := vData.Items[vData.SelectInfo.StartItemNo];
          if vData.SelectInfo.StartItemOffset = 0 then  // ��Item��ǰ��
          begin
            if vDeItem.CanConcatItems(vCurItem) then // ���Ժϲ�
            begin
              vCurItem.Text := vDeItem.Text + vCurItem.Text;

              if Key = VK_DELETE then  // ��ɾ
                vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset + 1;

              Self.ActiveSection.ReFormatActiveItem;
            end
            else  // ���ܺϲ�
            begin
              vDeItem.ParaFirst := vCurItem.ParaFirst;
              vCurItem.ParaFirst := False;
              Self.InsertItem(vDeItem);
              if Key = VK_BACK then  // ��ɾ
                vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset - 1;
            end;
          end
          else
          if vData.SelectInfo.StartItemOffset = vCurItem.Length then  // ��Item�����
          begin
            if vCurItem.CanConcatItems(vDeItem) then // ���Ժϲ�
            begin
              vCurItem.Text := vCurItem.Text + vDeItem.Text;

              if Key = VK_DELETE then  // ��ɾ
                vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset + 1;

              Self.ActiveSection.ReFormatActiveItem;
            end
            else  // �����Ժϲ�
            begin
              Self.InsertItem(vDeItem);
              if Key = VK_BACK then  // ��ɾ
                vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset - 1;
            end;
          end
          else  // ��Item�м�
          begin
            Self.InsertItem(vDeItem);
            if Key = VK_BACK then  // ��ɾ
              vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset - 1;
          end;
        end;
      finally
        Self.EndUpdate;
      end;
    end
    else
      inherited KeyDown(Key, Shift);
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure THCEmrView.KeyDownLib(var AKey: Word);
begin
  Self.KeyDown(AKey, []);
end;

procedure THCEmrView.KeyPress(var Key: Char);
var
  vData: THCCustomData;
begin
  if IsKeyPressWant(Key) then
  begin
    if CanNotEdit then Exit;

    if FTrace then
    begin
      vData := Self.ActiveSectionTopLevelData;

      if vData.SelectInfo.StartItemNo < 0 then Exit;

      if vData.SelectExists then
        Self.DisSelect
      else
        InsertEmrTraceItem(Key);

      Exit;
    end;

    inherited KeyPress(Key);
  end;
end;

function THCEmrView.NewDeItem(const AText: string): TDeItem;
begin
  Result := TDeItem.CreateByText(AText);
  if Self.CurStyleNo > THCStyle.Null then
    Result.StyleNo := Self.CurStyleNo
  else
    Result.StyleNo := 0;

  Result.ParaNo := Self.CurParaNo;
end;

procedure THCEmrView.SetActionDataDeGroupStream(const ASection: THCSection;
  const AArea: TSectionArea; const ADeIndex: string; const AStream: TStream;
  const AStartLast: Boolean);
var
  vStartNo, vEndNo: Integer;
  vData: THCSectionData;
begin
  vStartNo := -1;
  vEndNo := -1;
  case AArea of
    saHeader: vData := ASection.Header;
    saPage: vData := ASection.Page;
    saFooter: vData := ASection.Footer;
  end;

  if AStartLast then
  begin
    vStartNo := vData.Items.Count - 1;
    GetDataDeGroupItemNo(vData, ADeIndex, True, vStartNo, vEndNo)
  end
  else
    GetDataDeGroupItemNo(vData, ADeIndex, False, vStartNo, vEndNo);

  if vEndNo > 0 then
  begin
    ASection.DataAction(vData, function(): Boolean
    begin
      // ѡ�У�ʹ�ò���ʱɾ����ǰ�������е�����
      vData.SetSelectBound(vStartNo, OffsetAfter, vEndNo, OffsetBefor);
      FIgnoreAcceptAction := True;
      try
        Self.InsertStream(AStream);
        //vData.InsertStream(AStream);
      finally
        FIgnoreAcceptAction := False;
      end;
      Result := True;
    end);
  end;
end;

procedure THCEmrView.SetActionDataDeGroupText(const ASection: THCSection;
  const AArea: TSectionArea; const ADeIndex, AText: string; const AStartLast: Boolean = True);
var
  vStartNo, vEndNo: Integer;
  vData: THCSectionData;
begin
  vStartNo := -1;
  vEndNo := -1;
  case AArea of
    saHeader: vData := ASection.Header;
    saPage: vData := ASection.Page;
    saFooter: vData := ASection.Footer;
  end;

  if AStartLast then
  begin
    vStartNo := vData.Items.Count - 1;
    GetDataDeGroupItemNo(vData, ADeIndex, True, vStartNo, vEndNo)
  end
  else
    GetDataDeGroupItemNo(vData, ADeIndex, False, vStartNo, vEndNo);

  if vEndNo > 0 then
  begin
    ASection.DataAction(vData, function(): Boolean
    begin
      // ѡ�У�ʹ�ò���ʱɾ����ǰ�������е�����
      vData.SetSelectBound(vStartNo, OffsetAfter, vEndNo, OffsetBefor);
      FIgnoreAcceptAction := True;
      try
        if AText <> '' then
          vData.InsertText(AText)
        else
          vData.DeleteSelected;
      finally
        FIgnoreAcceptAction := False;
      end;

      Result := True;
    end);
  end;
end;

procedure THCEmrView.SetActiveItemExtra(const AStream: TStream);
var
  vFileFormat: string;
  vFileVersion: Word;
  vLang: Byte;
  vStyle: THCStyle;
  vTopData: THCRichData;
begin
  _LoadFileFormatAndVersion(AStream, vFileFormat, vFileVersion, vLang);  // �ļ���ʽ�Ͱ汾
  vStyle := THCStyle.Create;
  try
    vStyle.LoadFromStream(AStream, vFileVersion);
    Self.BeginUpdate;
    try
      Self.UndoGroupBegin;
      try
        vTopData := Self.ActiveSectionTopLevelData as THCRichData;
        Self.DeleteActiveDataItems(vTopData.SelectInfo.StartItemNo);
        ActiveSection.InsertStream(AStream, vStyle, vFileVersion);
      finally
        Self.UndoGroupEnd;
      end;
    finally
      Self.EndUpdate;
    end;
  finally
    FreeAndNil(vStyle);
  end;
end;

procedure THCEmrView.SetDataDeGroupFromStream(const AData: THCViewData;
  const ADeGroupStartNo, ADeGroupEndNo: Integer; const AStream: TStream);
var
  vFileExt: string;
  viVersion: Word;
  vLang: Byte;
  vStyle: THCStyle;
begin
  FIgnoreAcceptAction := True;
  try
    Self.BeginUpdate;
    try
      AData.BeginFormat;
      try
        if ADeGroupEndNo - ADeGroupStartNo > 1 then  // �м�������
          AData.DeleteItems(ADeGroupStartNo + 1,  ADeGroupEndNo - 1, False)
        else
          AData.SetSelectBound(ADeGroupStartNo, OffsetAfter, ADeGroupStartNo, OffsetAfter);

        AStream.Position := 0;
        _LoadFileFormatAndVersion(AStream, vFileExt, viVersion, vLang);  // �ļ���ʽ�Ͱ汾
        vStyle := THCStyle.Create;
        try
          vStyle.LoadFromStream(AStream, viVersion);
          AData.InsertStream(AStream, vStyle, viVersion);
        finally
          vStyle.Free;
        end;
      finally
        AData.EndFormat(False);
      end;

      Self.FormatData;
    finally
      Self.EndUpdate;
    end;
  finally
    FIgnoreAcceptAction := False;
  end;
end;

procedure THCEmrView.SetDataDeGroupText(const AData: THCViewData;
  const ADeGroupNo: Integer; const AText: string);
var
  vGroupBeg, vGroupEnd: Integer;
begin
  vGroupEnd := AData.GetDomainAnother(ADeGroupNo);

  if vGroupEnd > ADeGroupNo then
    vGroupBeg := ADeGroupNo
  else
  begin
    vGroupBeg := vGroupEnd;
    vGroupEnd := ADeGroupNo;
  end;

  // ѡ�У�ʹ�ò���ʱɾ����ǰ�������е�����
  AData.SetSelectBound(vGroupBeg, OffsetAfter, vGroupEnd, OffsetBefor);
  FIgnoreAcceptAction := True;
  try
    if AText <> '' then
      AData.InsertText(AText)
    else
      AData.DeleteSelected;
  finally
    FIgnoreAcceptAction := False;
  end;
end;

function THCEmrView.SetDeObjectProperty(const ADeIndex, APropName,
  APropValue: string): Boolean;
var
  vItemTraverse: THCItemTraverse;
  vItem: THCCustomItem;
  vResult, vReformat: Boolean;
begin
  Result := False;
  vResult := False;
  vReformat := False;

  vItemTraverse := THCItemTraverse.Create;
  try
    vItemTraverse.Tag := 0;
    vItemTraverse.Areas := [saPage, saHeader, saFooter];
    vItemTraverse.Process := procedure (const AData: THCCustomData; const AItemNo,
      ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean)
    var
      vPropertys: TStringList;
      i: Integer;
    begin
      vItem := AData.Items[AItemNo];
      if (vItem is TDeItem) and ((vItem as TDeItem)[TDeProp.Index] = ADeIndex) then
      begin
        if APropName = 'Text' then
        begin
          if APropValue <> '' then
          begin
            vItem.Text := APropValue;
            (vItem as TDeItem).AllocValue := True;
          end;

          vReformat := True;
        end
        else
        if APropName = 'Propertys' then  // ��������һ�δ���
        begin
          vPropertys := TStringList.Create;
          try
            vPropertys.Text := APropValue;
            for i := 0 to vPropertys.Count - 1 do
            begin
              if vPropertys.Names[i] = 'Text' then
              begin
                if vPropertys.ValueFromIndex[i] <> '' then
                begin
                  vItem.Text := vPropertys.ValueFromIndex[i];
                  (vItem as TDeItem).AllocValue := True;
                end;

                vReformat := True;
              end
              else
                (vItem as TDeItem)[vPropertys.Names[i]] := vPropertys.ValueFromIndex[i];
            end;
          finally
            vPropertys.Free;
          end;
        end
        else
          (vItem as TDeItem)[APropName] := APropValue;

        vResult := True;
        //AStop := True;
      end
      else
      if (vItem is TDeImageItem) and ((vItem as TDeImageItem)[TDeProp.Index] = ADeIndex) then
      begin
        if APropName = 'Graphic' then
        begin
          (vItem as TDeImageItem).LoadGraphicStream(TStream(FPropertyObject), False);
          vReformat := True;
        end;

        vResult := True;
        AStop := True;
      end;
    end;

    Self.TraverseItem(vItemTraverse);
  finally
    vItemTraverse.Free;
  end;

  if vResult then
  begin
    if vReformat then
      Self.FormatData;

    Result := vResult;
  end;
end;

procedure THCEmrView.SetHideTrace(const Value: Boolean);
var
  vItemTraverse: THCItemTraverse;
begin
  if FHideTrace <> Value then
  begin
    FHideTrace := Value;

    vItemTraverse := THCItemTraverse.Create;
    try
      vItemTraverse.Areas := [saPage];
      vItemTraverse.Process := procedure (const AData: THCCustomData; const AItemNo,
        ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean)
      var
        vDeItem: TDeItem;
      begin
        if AData.Items[AItemNo] is TDeItem then
        begin
          vDeItem := AData.Items[AItemNo] as TDeItem;
          if vDeItem.StyleEx = TStyleExtra.cseDel then
            vDeItem.Visible := not FHideTrace;  // ����/��ʾ�ۼ�
        end;
      end;

      Self.TraverseItem(vItemTraverse);

    finally
      vItemTraverse.Free;
    end;

    Self.FormatData;
    if FHideTrace then  // ������ʾ�ۼ�
    begin
      if not Self.ReadOnly then
        Self.ReadOnly := True;
    end;
  end;
end;

function THCEmrView.SetDeImageGraphic(const ADeIndex: string;
  const AGraphicStream: TStream): Boolean;
begin
  FPropertyObject := AGraphicStream;
  Result := SetDeObjectProperty(ADeIndex, 'Graphic', '');
end;

function THCEmrView.SetDeItemText(const ADeIndex, AText: string): Boolean;
begin
  Result := SetDeObjectProperty(ADeIndex, 'Text', AText);
end;

procedure THCEmrView.SetPageBlankTip(const Value: string);
begin
  if FPageBlankTip <> Value then
  begin
    FPageBlankTip := Value;
    Self.UpdateView;
  end;
end;

procedure THCEmrView.SyncDeItem(const AStartData: THCCustomData; const ARefDeItem: TDeItem);
var
  vItemTraverse: THCItemTraverse;
  vItem: THCCustomItem;
  vDeItem: TDeItem;
  vData: THCCustomData;
  vStart, vFind: Boolean;
begin
  vStart := False;
  vFind := False;

  if Assigned(AStartData) then
    vData := AStartData
  else
    vData := Self.ActiveSection.Page;

  vItemTraverse := THCItemTraverse.Create;
  try
    vItemTraverse.Tag := 0;
    vItemTraverse.Areas := [saPage];
    vItemTraverse.Process := procedure (const AData: THCCustomData; const AItemNo,
      ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean)
    begin
      vItem := AData.Items[AItemNo];
      if vStart then
      begin
        if vItem.StyleNo > THCStyle.Null then
        begin
          vDeItem := vItem as TDeItem;
          if vDeItem[TDeProp.Index] = ARefDeItem[TDeProp.Index] then
          begin
            vDeItem.Text := ARefDeItem.Text;
            vDeItem.AllocValue := True;
            vDeItem[TDeProp.CMVVCode] := ARefDeItem[TDeProp.CMVVCode];
            vFind := True;
          end;
        end;
      end
      else
      if vItem = ARefDeItem then
        vStart := True;
    end;

    Self.TraverseItem(vItemTraverse);
  finally
    vItemTraverse.Free;
  end;

  if vFind then
    Self.FormatData;
end;

procedure THCEmrView.SyntaxCheck;
var
  vItemTraverse: THCItemTraverse;
begin
  if not Assigned(FOnSyntaxCheck) then Exit;

  vItemTraverse := THCItemTraverse.Create;
  try
    vItemTraverse.Tag := 0;
    vItemTraverse.Areas := [saPage];
    vItemTraverse.Process := DoSyntaxCheck;
    Self.TraverseItem(vItemTraverse);
    Self.UpdateView;
  finally
    vItemTraverse.Free;
  end;
end;

procedure THCEmrView.TraverseItem(const ATraverse: THCItemTraverse);
var
  i: Integer;
begin
  if ATraverse.Areas = [] then Exit;

  for i := 0 to Self.Sections.Count - 1 do
  begin
    if not ATraverse.Stop then
    begin
      with Self.Sections[i] do
      begin
        if saHeader in ATraverse.Areas then
          Header.TraverseItem(ATraverse);

        if (not ATraverse.Stop) and (saPage in ATraverse.Areas) then
          Page.TraverseItem(ATraverse);

        if (not ATraverse.Stop) and (saFooter in ATraverse.Areas) then
          Footer.TraverseItem(ATraverse);
      end;
    end;
  end;
end;

procedure THCEmrView.WndProc(var Message: TMessage);
var
  Form: TCustomForm;
  ShiftState: TShiftState;
begin
  if (Message.Msg = WM_KEYDOWN) or (Message.Msg = WM_KEYUP) then
  begin
    if message.WParam in [VK_LEFT..VK_DOWN, VK_RETURN, VK_TAB] then
    begin
      Form := GetParentForm(Self);
      if Form = nil then
      begin
        if Application.Handle <> 0 then  // ��exe������
        begin
          if Message.WParam <> VK_RETURN then
          begin
            ShiftState := KeyDataToShiftState(TWMKey(Message).KeyData);
            Self.KeyDown(TWMKey(Message).CharCode, ShiftState);
            Exit;
          end;
        end
        else  // �������������
        begin
          if Message.WParam = VK_RETURN then
          begin
            ShiftState := KeyDataToShiftState(TWMKey(Message).KeyData);
            Self.KeyDown(TWMKey(Message).CharCode, ShiftState);

            Exit;
          end;
        end;
      end;
    end;
  end;

  inherited WndProc(Message);
end;

end.
