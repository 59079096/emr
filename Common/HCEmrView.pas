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

    {$IFDEF PROCSERIES}
    FUnEditProcBKColor: TColor;  // ���ܱ༭�Ĳ������򱳾�ɫ
    FShowProcSplit: Boolean;  // ����2�����̵ļ����
    FProcCount: Integer;  // ��ǰ�ĵ���������
    FCaretProcInfo,  // ��ǰ��괦�Ĳ�����Ϣ
    FEditProcInfo  // ��ǰ���ڱ༭�Ĳ�����Ϣ
      : TProcInfo;
    FEditProcIndex: string;  // ��ǰ����༭�Ĳ���
    {$ENDIF}

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

    {$IFDEF PROCSERIES}
    /// <summary> ȡ��괦������Ϣ </summary>
    procedure CheckCaretProcInfo;
    /// <summary> ȡSection��괦������Ϣ </summary>
    procedure GetSectionCaretProcInfo(const ASectionIndex: Integer; const AProcInfo: TProcInfo);
    {$ENDIF}
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

    procedure Clear; override;

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

    procedure DoSectionPaintPageBefor(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo); override;

    procedure DoSectionDrawItemPaintBefor(const Sender: TObject;
      const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
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
      const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
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

    function DeleteDeGroup(const ADeIndex: string): Boolean;

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
    function SetSignatureGraphic(const ADeIndex: string; const AGraphicStream: TStream): Boolean;

    /// <summary> ͬ��AStartData��ARefDeItem�����ArefDeItem��ͬԪ�ص����� </summary>
    procedure SyncDeItemAfterRef(const AStartData: THCCustomData; const ARefDeItem: TDeItem);

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

    {$IFDEF PROCSERIES}
    function InsertProc(const AProcIndex, APropertys, ABeforProcIndex: string): Boolean;
    function DeleteProc(const AProcIndex: string): Boolean;
    function GetCaretProcProperty(const APropName: string): string;
    function GetProcProperty(const AProcIndex, APropName: string): string;
    function SetProcProperty(const AProcIndex, APropName, APropValue: string): Boolean;
    function GetProcAsText(const AProcIndex: string; var AText: string): Boolean;
    function SetProcByText(const AProcIndex, AText: string): Boolean;
    function GetProcAsStream(const AProcIndex: string; const AStream: TStream): Boolean;
    /// <summary> �ʺ���д�����е��滻 </summary>
    function SetProcByStream(const AProcIndex: string; const AStream: TStream): Boolean;
    /// <summary> �Ӳ����ļ�������ָ�����̵�����(�ʺϵ�һ��д����ʱ����ģ��) </summary>
    function SetProcByFileSteam(const AProcIndex: string; const AStream: TStream): Boolean;
    procedure SetEditProcIndex(const Value: string);
    procedure ScrollToProc(const AProcIndex: string);

    /// <summary> ȡ���̵���ʼ����ItemNo </summary>
    function GetProcItemNo(const AProcIndex: string; var ASectionIndex, AStartNo, AEndNo: Integer): Boolean;

    procedure GetProcInfoAt(const AData: THCSectionData; const AItemNo, AOffset: Integer; const AProcInfo: TProcInfo);
    {$ENDIF}

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

    function GetDeGroupAsText(const ADeIndex: string): string;

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
    procedure SetDeGroupByText(const ASection: THCSection;
      const AArea: TSectionArea; const ADeIndex, AText: string; const AStartLast: Boolean = True);
    procedure SetDeGroupByFileStream(const ASection: THCSection;
      const AArea: TSectionArea; const ADeIndex: string; const AStream: TStream; const AStartLast: Boolean = True);

    /// <summary> ��ָ���������е�����д�뵽�� </summary>
    procedure GetDataDeGroupToStream(const AData: THCViewData;
      const ADeGroupStartNo, ADeGroupEndNo: Integer; const AStream: TStream);

    /// <summary> ��ָ���������е�����д�뵽�� </summary>
    procedure SetDataDeGroupFromStream(const AData: THCViewData;
      const ADeGroupStartNo, ADeGroupEndNo: Integer; const AStream: TStream);

    function SaveSelectToText: string;
    /// <summary> ����ѡ�в�����һ����ѡ�е��������ʶ�� </summary>
    procedure SaveSelectToStream(const AStream: TStream);

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

    {$IFDEF PROCSERIES}
    property ProcCount: Integer read FProcCount;
    property EditProcIndex: string read FEditProcIndex write SetEditProcIndex;
    property ShowProcSplit: Boolean read FShowProcSplit write FShowProcSplit;
    property UnEditProcBKColor: TColor read FUnEditProcBKColor write FUnEditProcBKColor;
    {$ENDIF}

    /// <summary> ҳ�����ݲ���ʱ�ײ��հ���ʾ </summary>
    property PageBlankTip: string read FPageBlankTip write SetPageBlankTip;

    property DeDoneColor: TColor read FDeDoneColor write FDeDoneColor;
    property DeUnDoneColor: TColor read FDeUnDoneColor write FDeUnDoneColor;
    property DeHotColor: TColor read FDeHotColor write FDeHotColor;

    /// <summary> ����AcceptAction�Ĵ��� </summary>
    property IgnoreAcceptAction: Boolean read FIgnoreAcceptAction write FIgnoreAcceptAction;

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
  SysUtils, Forms, Printers, HCTextStyle, HCParaStyle;

procedure Register;
begin
  RegisterComponents('HCEmrViewVCL', [THCEmrView]);
end;

{ TEmrView }

function THCEmrView.CanNotEdit: Boolean;
begin
  //Result := (not Self.ActiveSection.ActiveData.CanEdit) or (not (Self.ActiveSectionTopLevelData as THCRichData).CanEdit);
  Result := not (Self.ActiveSectionTopLevelData as THCRichData).CanEdit;
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

procedure THCEmrView.Clear;
begin
  FTraceCount := 0;
  {$IFDEF PROCSERIES}
  FProcCount := 0;
  FCaretProcInfo.Clear;
  FEditProcInfo.Clear;
  FEditProcIndex := '';
  {$ENDIF}
  inherited Clear;
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
  FDeHotColor := $00F4E0CC;  // ����ƶ���Ԫ���ϱ���ɫ
  FPageBlankTip := '';  // '--------��ҳ���¿հ�--------'
  Self.Style.DefaultTextStyle.Size := GetFontSize('С��');
  Self.Style.DefaultTextStyle.Family := '����';
  Self.HScrollBar.AddStatus(200);
  {$IFDEF PROCSERIES}
  FUnEditProcBKColor := clBtnFace;  // ���ܱ༭�Ĳ������򱳾�ɫ
  FShowProcSplit := True;
  FProcCount := 0;
  FCaretProcInfo := TProcInfo.Create;
  FEditProcInfo := TProcInfo.Create;
  FEditProcIndex := '';
  {$ENDIF}
end;

function THCEmrView.DeleteDeGroup(const ADeIndex: string): Boolean;
var
  vStartNo, vEndNo: Integer;
begin
  Result := False;
  if ADeIndex = '' then Exit;
  vStartNo := 0;
  GetDataDeGroupItemNo(Self.ActiveSection.Page, ADeIndex, False, vStartNo, vEndNo);
  if vEndNo > 0 then
  begin
    Result := Self.ActiveSection.DataAction(Self.ActiveSection.Page, function(): Boolean
    begin
      FIgnoreAcceptAction := True;
      try
        //Self.ActiveSection.Page.DeleteItems(vStartNo, vEndNo, False);
        Self.ActiveSection.Page.DeleteDomainByItemNo(vStartNo, vEndNo);
      finally
        FIgnoreAcceptAction := False;
      end;
      Result := True;
    end);

    {$IFDEF PROCSERIES}
    CheckCaretProcInfo;
    {$ENDIF}
  end;
end;

destructor THCEmrView.Destroy;
begin
  {$IFDEF PROCSERIES}
  FreeAndNil(FCaretProcInfo);
  FreeAndNil(FEditProcInfo);
  {$ENDIF}
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
  if FIgnoreAcceptAction then Exit(True);

  {$IFDEF PROCSERIES}
  if FEditProcIndex <> '' then  // �����ڱ༭�Ĳ���
  begin
    if FEditProcIndex <> FCaretProcInfo.Index then  // ��괦�͵�ǰ����༭�Ĳ�ͬ
    begin
      Result := False;  // ������༭
      Exit;
    end;
  end;
  {$ENDIF}

  Result := inherited DoSectionCanEdit(Sender);
  if Result then
  begin
    vViewData := Sender as THCViewData;
    if (vViewData.ActiveDomain <> nil) and (vViewData.ActiveDomain.BeginNo >= 0) then
      Result := not (vViewData.Items[vViewData.ActiveDomain.BeginNo] as TDeGroup).ReadOnly
    else
      Result := True;
  end;
end;

procedure THCEmrView.DoSectionCaretItemChanged(const Sender: TObject;
  const AData: THCCustomData; const AItem: THCCustomItem);
var
  vData: THCViewData;
  vActiveItem: THCCustomItem;
  vDeItem: TDeItem;
  vDeGroup: TDeGroup;
  vDeEdit: TDeEdit;
  vDeCombobox: TDeCombobox;
  vDeDateTimePicker: TDeDateTimePicker;
  vDeImageItem: TDeImageItem;
  vInfo: string;
begin
  vInfo := '';
  vActiveItem := Self.GetTopLevelItem;
  if vActiveItem <> nil then
  begin
    {$IFDEF PROCSERIES}
    if (FProcCount > 0) and (AData = Self.ActiveSection.Page) then  // �в���
    begin
      CheckCaretProcInfo; // ��ǰλ�ò�����Ϣ
      if FCaretProcInfo.EndNo > 0 then
      begin
        vDeGroup := Self.ActiveSection.ActiveData.Items[FCaretProcInfo.BeginNo] as TDeGroup;
        vInfo := vDeGroup[TDeProp.Name];// + '(' + vDeGroup[TDeProp.Index] + ')';
      end;
    end;
    {$ENDIF}

    vData := Self.ActiveSectionTopLevelData as THCViewData;
    if vData.ActiveDomain.EndNo > 0 then
    begin
      vDeGroup := vData.Items[vData.ActiveDomain.BeginNo] as TDeGroup;
      {$IFDEF PROCSERIES}
      if not vDeGroup.IsProc then
      {$ENDIF}
      begin
        if vInfo <> '' then
          vInfo := vInfo + '>' + vDeGroup[TDeProp.Name] + '(' + vDeGroup[TDeProp.Index] + ')'
        else
          vInfo := vDeGroup[TDeProp.Name] + '(' + vDeGroup[TDeProp.Index] + ')';
      end;
    end;

    if vActiveItem is TDeItem then
    begin
      vDeItem := vActiveItem as TDeItem;
      if vDeItem.TraceStyle <> cseNone then
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
    end
    else
    if vActiveItem is TDeImageItem then
    begin
      vDeImageItem := vActiveItem as TDeImageItem;
      if vInfo <> '' then
        vInfo := vInfo + ' > ' + vDeImageItem[TDeProp.Name] + '(' + vDeImageItem[TDeProp.Index] + ')'
      else
        vInfo := vDeImageItem[TDeProp.Name] + '(' + vDeImageItem[TDeProp.Index] + ')';
    end;
  end;

  Self.HScrollBar.Statuses[1].Text := vInfo;

  inherited DoSectionCaretItemChanged(Sender, AData, AItem);
end;

procedure THCEmrView.DoSectionCreateItem(Sender: TObject);
begin
  if (not Style.States.Contain(hosLoading)) and FTrace then
    (Sender as TDeItem).TraceStyle := TDeTraceStyle.cseAdd;

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

  {$IFDEF PROCSERIES}
  if (AAction = THCAction.actDeleteSelected) and (AData = Self.ActiveSection.Page) and (FEditProcInfo.EndNo > 0) then
  begin
    if (AData.SelectInfo.StartItemNo < FEditProcInfo.BeginNo) or (AData.SelectInfo.EndItemNo > FEditProcInfo.EndNo) then  // �ڵ�ǰ�༭����������
    begin
      Result := False;
      Exit;
    end;
  end;
  {$ENDIF}

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
  const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
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
    vRect := Bounds(AClearRect.Left, AClearRect.Top - vSize.cy - 5, vSize.cx, vSize.cy);
    if vRect.Right > ADataDrawRight then
      OffsetRect(vRect, ADataDrawRight - vRect.Right, 0);

    if ADeItem.TraceStyle = TDeTraceStyle.cseDel then
      ACanvas.Brush.Color := clBtnFace
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
  vDeGroup: TDeGroup;
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
        ACanvas.FillRect(AClearRect);
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
      if (vDeItem.TraceStyle <> TDeTraceStyle.cseNone) then  // �Ǻۼ�
      begin
        if FTraceInfoAnnotate then  // ����ע��ʽ��ʾ�ۼ�
        begin
          vDrawAnnotate := THCDrawAnnotateDynamic.Create;
          vDrawAnnotate.DrawRect := AClearRect;
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

  if FSecret then  // ��˽��ʾ״̬
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
            ACanvas.FillRect(Rect(AClearRect.Left + AData.GetDrawItemOffsetWidth(ADrawItemNo, vSecretLow), AClearRect.Top,
              AClearRect.Left + AData.GetDrawItemOffsetWidth(ADrawItemNo, vSecretHi), AClearRect.Bottom));
            ACanvas.Brush.Style := bsSolid;
          end;
        end;
      end;
    end;
  end;

  {$IFDEF PROCSERIES}
  if (not APaintInfo.Print) and (AData.Items[AItemNo] is TDeGroup) then  // ���Ʋ��̵�ǰ��ָʾ��ͷ
  begin
    vDeGroup := AData.Items[AItemNo] as TDeGroup;
    if vDeGroup.MarkType = TMarkType.cmtBeg then  // ͷ
    begin
      if vDeGroup[TGroupProp.SubType] = TSubType.Proc then  // ����ͷ
      begin
        if (AItemNo > 0) and (AData.Items[AItemNo - 1] is TDeGroup)
          and ((AData.Items[AItemNo - 1] as TDeGroup)[TGroupProp.SubType] = TSubType.Proc)  // ��һ���ǲ���β
        then
          HCDrawArrow(ACanvas, clMedGray, AClearRect.Left - 10, AClearRect.Top, 0);  // ���ϼ�ͷ

        if FEditProcInfo.BeginNo = AItemNo then
          HCDrawArrow(ACanvas, clBlue, AClearRect.Left - 10, AClearRect.Top + 12, 1)
        else
          HCDrawArrow(ACanvas, clMedGray, AClearRect.Left - 10, AClearRect.Top + 12, 1);  // ���¼�ͷ
      end;
    end
    else  // β
    begin
      if vDeGroup[TGroupProp.SubType] = TSubType.Proc then  // ����β
      begin
        if (AItemNo < AData.Items.Count - 1) and (AData.Items[AItemNo + 1] is TDeGroup)
          and ((AData.Items[AItemNo + 1] as TDeGroup)[TGroupProp.SubType] = TSubType.Proc)  // ��һ���ǲ���ͷ
        then
          HCDrawArrow(ACanvas, clMedGray, AClearRect.Right + 10, AClearRect.Top + 12, 1);  // ���¼�ͷ

        if FEditProcInfo.EndNo = AItemNo then
          HCDrawArrow(ACanvas, clBlue, AClearRect.Right + 10, AClearRect.Top, 0)
        else
          HCDrawArrow(ACanvas, clMedGray, AClearRect.Right + 10, AClearRect.Top, 0);  // ���ϼ�ͷ
      end;
    end;
  end;
  {$ENDIF}

  if (FPageBlankTip <> '') and (AData is THCPageData) then
  begin
    if ADrawItemNo < AData.DrawItems.Count - 1 then
    begin
      if AData.Items[AData.DrawItems[ADrawItemNo + 1].ItemNo].PageBreak then
        DrawBlankTip_(ADataDrawLeft, AClearRect.Top + AClearRect.Height + AData.GetLineBlankSpace(ADrawItemNo), ADataDrawRight);
    end
    else
      DrawBlankTip_(ADataDrawLeft, AClearRect.Top + AClearRect.Height + AData.GetLineBlankSpace(ADrawItemNo), ADataDrawRight);
  end;

  inherited DoSectionDrawItemPaintAfter(Sender, AData, AItemNo, ADrawItemNo, ADrawRect, AClearRect,
    ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
end;

procedure THCEmrView.DoSectionDrawItemPaintBefor(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer;
  const ADrawRect, AClearRect: TRect; const ADataDrawLeft, ADataDrawRight, ADataDrawBottom,
  ADataScreenTop, ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
var
  vDeItem: TDeItem;
  vDeGroup: TDeGroup;
  vTop: Integer;
  vTextHeight: Integer;
begin
  if APaintInfo.Print then Exit;

  {$IFDEF PROCSERIES}
  if FShowProcSplit and (FProcCount > 0) then
  begin
    if (AData is THCPageData) and (AData.Items[AItemNo] is TDeGroup) then  // ����ͷ����ǰ��ָ���
    begin
      vDeGroup := AData.Items[AItemNo] as TDeGroup;
      if vDeGroup.IsProcBegin then  // �ǲ���
      begin
        ACanvas.Pen.Style := psDashDotDot;
        ACanvas.Pen.Color := clBlue;
        ACanvas.MoveTo(ADataDrawLeft, ADrawRect.Top - 1);
        ACanvas.LineTo(ADataDrawRight, ADrawRect.Top - 1);
      end;
    end;
  end;
  {$ENDIF}

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

      if (AItemNo < AData.Items.Count - 1)
        and (not AData.Items[AItemNo + 1].ParaFirst)
        and (AData.Items[AItemNo + 1].StyleNo > THCStyle.Null)
        and (AData.Items[AItemNo + 1] as TDeItem).IsElement
      then  // ���氤����һ��Ԫ��
      begin
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Color := Style.BackgroundColor;
        ACanvas.MoveTo(ADrawRect.Right, ADrawRect.Bottom - 5);
        ACanvas.LineTo(ADrawRect.Right, ADrawRect.Bottom);

        ACanvas.MoveTo(ADrawRect.Right - 1, ADrawRect.Bottom - 4);
        ACanvas.LineTo(ADrawRect.Right - 1, ADrawRect.Bottom);

        ACanvas.MoveTo(ADrawRect.Right - 2, ADrawRect.Bottom - 3);
        ACanvas.LineTo(ADrawRect.Right - 2, ADrawRect.Bottom);

        ACanvas.MoveTo(ADrawRect.Right - 3, ADrawRect.Bottom - 2);
        ACanvas.LineTo(ADrawRect.Right - 3, ADrawRect.Bottom);

        ACanvas.MoveTo(ADrawRect.Right - 4, ADrawRect.Bottom - 1);
        ACanvas.LineTo(ADrawRect.Right - 4, ADrawRect.Bottom);
      end
      else
      if (AItemNo > 0)
        and (not AData.Items[AItemNo].ParaFirst)
        and (AData.Items[AItemNo -1].StyleNo > THCStyle.Null)
        and (AData.Items[AItemNo - 1] as TDeItem).IsElement
      then
      begin
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Color := Style.BackgroundColor;
        ACanvas.MoveTo(ADrawRect.Left, ADrawRect.Bottom - 5);
        ACanvas.LineTo(ADrawRect.Left, ADrawRect.Bottom);

        ACanvas.MoveTo(ADrawRect.Left + 1, ADrawRect.Bottom - 4);
        ACanvas.LineTo(ADrawRect.Left + 1, ADrawRect.Bottom);

        ACanvas.MoveTo(ADrawRect.Left + 2, ADrawRect.Bottom - 3);
        ACanvas.LineTo(ADrawRect.Left + 2, ADrawRect.Bottom);

        ACanvas.MoveTo(ADrawRect.Left + 3, ADrawRect.Bottom - 2);
        ACanvas.LineTo(ADrawRect.Left + 3, ADrawRect.Bottom);

        ACanvas.MoveTo(ADrawRect.Left + 4, ADrawRect.Bottom - 1);
        ACanvas.LineTo(ADrawRect.Left + 4, ADrawRect.Bottom);
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
    case vDeItem.TraceStyle of  // �ۼ�
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
    if vDeItem.TraceStyle <> TDeTraceStyle.cseNone then
    begin
      Inc(FTraceCount);
      if FTraceInfoAnnotate then
        Self.AnnotatePre.InsertDataAnnotate(nil);
    end;

    DoSyncDeItem(Sender, AData, AItem);  // �������ò�������ʱ����ۼ���Ϣ
  end
  else
  {$IFDEF PROCSERIES}
  if AItem is TDeGroup then
  begin
    if (AItem as TDeGroup).IsProcBegin then
      Inc(FProcCount);
  end
  else
  {$ENDIF}
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

procedure THCEmrView.DoSectionPaintPageBefor(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
var
  vPt: TPoint;
  vData: THCPageData;
begin
  inherited DoSectionPaintPageBefor(Sender, APageIndex, ARect, ACanvas, APaintInfo);
  {$IFDEF PROCSERIES}
  if (not APaintInfo.Print) and (FEditProcInfo.EndNo > 0) then
  begin
    vData := FEditProcInfo.Data as THCPageData;
    vPt := vData.DrawItems[vData.Items[FEditProcInfo.BeginNo].FirstDItemNo].Rect.TopLeft;
    vPt := Self.GetFormatPointToViewCoord(vPt);
    if vPt.Y > ARect.Top then  // �ڵ�ǰ�༭�Ĳ���ͷ����
    begin
      ACanvas.Brush.Color := FUnEditProcBKColor;
      ACanvas.FillRect(Rect(ARect.Left, ARect.Top, ARect.Right, vPt.Y));
    end;

    if FEditProcInfo.EndNo < vData.Items.Count - 1 then  // ��ǰ�༭�Ĳ���β����������
    begin
      vPt := vData.DrawItems[vData.Items[FEditProcInfo.EndNo].FirstDItemNo].Rect.BottomRight;
      vPt := Self.GetFormatPointToViewCoord(vPt);
      if vPt.Y < ARect.Bottom then  // �ڵ�ǰ�༭����β����
      begin
        ACanvas.Brush.Color := FUnEditProcBKColor;
        // ����vPt.X��������ŵ�ǰҳ��������ײ�
        vPt.X := ARect.Top + THCSection(Sender).GetPageDataHeight(APageIndex);
        if vPt.X < ARect.Bottom then
          ACanvas.FillRect(Rect(ARect.Left, vPt.Y, ARect.Right, vPt.X))
        else
          ACanvas.FillRect(Rect(ARect.Left, vPt.Y, ARect.Right, ARect.Bottom));
      end;
    end;
  end;
  {$ENDIF}
end;

procedure THCEmrView.DoSectionRemoveItem(const Sender: TObject;
  const AData: THCCustomData; const AItem: THCCustomItem);
var
  vDeItem: TDeItem;
begin
  if AItem is TDeItem then
  begin
    vDeItem := AItem as TDeItem;

    if vDeItem.TraceStyle <> TDeTraceStyle.cseNone then
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
    //if (AData.Items[AItemNo] is TDeGroup) and (not FDesignMode) then  // �����ģʽ������������
    //  Result := False
    //else
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

function THCEmrView.GetDeGroupAsText(const ADeIndex: string): string;
var
  vStartNo, vEndNo: Integer;
begin
  Result := '';
  vStartNo := 0;
  GetDataDeGroupItemNo(ActiveSection.Page, ADeIndex, False, vStartNo, vEndNo);
  if vEndNo > 0 then
    Result := GetDataDeGroupText(ActiveSection.Page, vStartNo, vEndNo);
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

{$IFDEF PROCSERIES}
function THCEmrView.InsertProc(const AProcIndex, APropertys, ABeforProcIndex: string): Boolean;
var
  vPageData: THCViewData;
  vDeGroup: TDeGroup;
  vStrings: TStringList;
  vSection: THCSection;
  i, vSectionIndex, vStartNo, vEndNo: Integer;
begin
  Result := False;
  if AProcIndex = '' then Exit;

  vSection := Self.ActiveSection;
  vPageData := Self.ActiveSectionTopLevelData as THCViewData;
  if vPageData = Self.ActiveSection.Page then  // ֻ�������Ĳ��벡��
  begin
    vDeGroup := TDeGroup.Create(vPageData);
    try
      vDeGroup[TDeProp.Index] := AProcIndex;
      vDeGroup[TGroupProp.SubType] := TSubType.Proc;

      if APropertys <> '' then
      begin
        vStrings := TStringList.Create;
        try
          vStrings.Text := APropertys;
          for i := 0 to vStrings.Count - 1 do
          begin
            if Trim(vStrings.Names[i]) <> '' then
              vDeGroup[vStrings.Names[i]] := vStrings.ValueFromIndex[i];
          end;
        finally
          FreeAndNil(vStrings);
        end;
      end;

      FIgnoreAcceptAction := True;
      try
        if ABeforProcIndex <> '' then  // ��ָ������ǰ�����
        begin
          if GetProcItemNo(ABeforProcIndex, vSectionIndex, vStartNo, vEndNo) then  // ��Ч
          begin
            if vSectionIndex <> Self.ActiveSectionIndex then
              Self.ActiveSectionIndex := vSectionIndex;

            vSection := Self.Sections[vSectionIndex];
            vPageData := vSection.Page;
            vPageData.SetSelectBound(vEndNo, OffsetAfter, vEndNo, OffsetAfter);
          end
          else
            Exit;
        end
        else  // �����׷��
          vPageData.SelectLastItemAfterWithCaret;

        if not vPageData.IsEmptyData then
          Self.InsertBreak;

        Self.ApplyParaAlignHorz(TParaAlignHorz.pahLeft);
        Result := Self.InsertDeGroup(vDeGroup);

        vEndNo := vPageData.SelectInfo.StartItemNo;
        vPageData.SetSelectBound(vEndNo, OffsetBefor, vEndNo, OffsetBefor);  // ��������CheckCaretProcInfo��ȡ�Ͳ����¼��
      finally
        FIgnoreAcceptAction := False;
      end;
    finally
      vDeGroup.Free;
    end;

    CheckCaretProcInfo;
    Self.UpdateView;
  end;
end;

function THCEmrView.DeleteProc(const AProcIndex: string): Boolean;
var
  vSectionIndex, vStartNo, vEndNo: Integer;
  vPage: THCPageData;
begin
  Result := False;
  if AProcIndex = '' then Exit;
  if GetProcItemNo(AProcIndex, vSectionIndex, vStartNo, vEndNo) then
  begin
    Self.BeginUpdate;  // ���ɾ�����ǵ�ǰ�༭�Ĳ��̣���ֹ�ػ�ʱ������ʼ����ItemNoû�����¼�������Խ��
    try
      vPage := Self.Sections[vSectionIndex].Page;
      Result := Self.Sections[vSectionIndex].DataAction(vPage, function(): Boolean
      begin
        FIgnoreAcceptAction := True;
        try
          vPage.DeleteItems(vStartNo, vEndNo, False);
          //vPage.DeleteDomainByItemNo(vStartNo, vEndNo);  ����ɾ����һ�����̻����п���
        finally
          FIgnoreAcceptAction := False;
        end;
        Result := True;
      end);

      {$IFDEF PROCSERIES}
      CheckCaretProcInfo;
      if AProcIndex = FEditProcIndex then  // ɾ���˵�ǰ�༭�Ĳ���
        FEditProcInfo.Clear;
      {$ENDIF}
    finally
      Self.EndUpdate;
    end;
  end;
end;

procedure THCEmrView.GetProcInfoAt(const AData: THCSectionData; const AItemNo: Integer; const AOffset: Integer; const AProcInfo: TProcInfo);
var
  i, vStartNo, vEndNo: Integer;
  vLevel: Byte;
begin
  // ��HCViewData��Ԫ��GetDomainFrom��ͬ����ô�ϲ��أ�
  AProcInfo.Clear;

  if (AItemNo < 0) or (AOffset < 0) then Exit;

  { ����ʼ��ʶ }
  vStartNo := AItemNo;
  vEndNo := AItemNo;
  if AData.Items[AItemNo]is TDeGroup then  // ��ʼλ�þ���Group
  begin
    if (AData.Items[AItemNo] as TDeGroup).IsProcBegin then  // ��ʼλ������ʼ���
    begin
      if AOffset = OffsetAfter then  // ����ں���
      begin
        AProcInfo.Data := AData;
        AProcInfo.BeginNo := AItemNo;  // ��ǰ��Ϊ��ʼ��ʶ
        vLevel := (AData.Items[AItemNo] as TDeGroup).Level;
        vEndNo := AItemNo + 1;
      end
      else  // �����ǰ��
      begin
        if AItemNo > 0 then  // ���ǵ�һ��
          vStartNo := AItemNo - 1  // ��ǰһ����ǰ
        else  // ���ڵ�һ��ǰ��
          Exit;  // ��������
      end;
    end
    else  // ����λ���ǽ������
    if (AData.Items[AItemNo] as TDeGroup).IsProcEnd then
    begin
      if AOffset = OffsetAfter then  // ����ں���
      begin
        if AItemNo < AData.Items.Count - 1 then  // �������һ��
          vEndNo := AItemNo + 1
        else  // �����һ������
          Exit;  // ��������
      end
      else  // �����ǰ��
      begin
        AProcInfo.EndNo := AItemNo;
        vStartNo := AItemNo - 1;
      end;
    end;
  end;

  if AProcInfo.BeginNo < 0 then  // û�ҵ���ʼ
  begin
    if vStartNo < AData.Items.Count div 2 then  // ��ǰ���
    begin
      for i := vStartNo downto 0 do  // ����ǰ����ʼ
      begin
        if AData.Items[i] is TDeGroup then
        begin
          if (AData.Items[i] as TDeGroup).IsProcBegin then  // ��ʼ���
          begin
            AProcInfo.Data := AData;
            AProcInfo.BeginNo := i;
            vLevel := (AData.Items[i] as TDeGroup).Level;
            Break;
          end;
        end;
      end;

      if (AProcInfo.BeginNo >= 0) and (AProcInfo.EndNo < 0) then  // �ҽ�����ʶ
      begin
        for i := vEndNo to AData.Items.Count - 1 do
        begin
          if AData.Items[i] is TDeGroup then
          begin
            if (AData.Items[i] as TDeGroup).IsProcEnd then  // �ǽ�β
            begin
              if (AData.Items[i] as TDeGroup).Level = vLevel then
              begin
                AProcInfo.EndNo := i;
                Break;
              end;
            end;
          end;
        end;

        if AProcInfo.EndNo < 0 then
          raise Exception.Create('�쳣����ȡ���̽���λ�ó���');
      end;
    end
    else  // �ں���
    begin
      for i := vEndNo to AData.Items.Count - 1 do  // �������ҽ���
      begin
        if AData.Items[i] is TDeGroup then
        begin
          if (AData.Items[i] as TDeGroup).IsProcEnd then  // �������
          begin
            AProcInfo.EndNo := i;
            vLevel := (AData.Items[i] as TDeGroup).Level;
            Break;
          end;
        end;
      end;

      if (AProcInfo.EndNo >= 0) and (AProcInfo.BeginNo < 0) then  // ����ʼ��ʶ
      begin
        for i := vStartNo downto 0 do
        begin
          if AData.Items[i] is TDeGroup then
          begin
            if (AData.Items[i] as TDeGroup).IsProcBegin then  // ����ʼ
            begin
              if (AData.Items[i] as TDeGroup).Level = vLevel then
              begin
                AProcInfo.Data := AData;
                AProcInfo.BeginNo := i;
                Break;
              end;
            end;
          end;
        end;

        if AProcInfo.BeginNo < 0 then
          raise Exception.Create('�쳣����ȡ������ʼλ�ó���');
      end;
    end;
  end
  else
  if AProcInfo.EndNo < 0 then // �ҵ���ʼ�ˣ��ҽ���
  begin
    for i := vEndNo to AData.Items.Count - 1 do
    begin
      if AData.Items[i] is TDeGroup then
      begin
        if (AData.Items[i] as TDeGroup).IsProcEnd then  // �ǽ�β
        begin
          if (AData.Items[i] as TDeGroup).Level = vLevel then
          begin
            AProcInfo.EndNo := i;
            Break;
          end;
        end;
      end;
    end;

    if AProcInfo.EndNo < 0 then
      raise Exception.Create('�쳣����ȡ���̽���λ�ó���');
  end;

  if AProcInfo.EndNo > 0 then
    AProcInfo.Index := (AData.Items[AProcInfo.EndNo] as TDeGroup)[TGroupProp.Index];
end;

procedure THCEmrView.CheckCaretProcInfo;
begin
  GetSectionCaretProcInfo(Self.ActiveSectionIndex, FCaretProcInfo);
  if FCaretProcInfo.Index = FEditProcIndex then
    FEditProcInfo.Assign(FCaretProcInfo);
end;

procedure THCEmrView.GetSectionCaretProcInfo(const ASectionIndex: Integer; const AProcInfo: TProcInfo);
var
  vPage: THCPageData;
begin
  vPage := Self.Sections[ASectionIndex].Page;
  GetProcInfoAt(vPage, vPage.SelectInfo.StartItemNo, vPage.SelectInfo.StartItemOffset, AProcInfo);
  AProcInfo.SectionIndex := ASectionIndex;
end;

function THCEmrView.GetCaretProcProperty(const APropName: string): string;
var
  vBeginGroup: TDeGroup;
begin
  Result := '';
  if FCaretProcInfo.EndNo > 0 then
  begin
    if APropName = TGroupProp.Index then
    begin
      Result := FCaretProcInfo.Index;
      Exit;
    end;

    vBeginGroup := Self.ActiveSection.Page.Items[FCaretProcInfo.BeginNo] as TDeGroup;

    if APropName = TGroupProp.Name then
      Result := vBeginGroup[TDeProp.Name]
    else
    if APropName = TGroupProp.Propertys then  // ��������һ�δ���
      Result := vBeginGroup.Propertys.Text
    else
      Result := vBeginGroup[APropName];
  end;
end;

function THCEmrView.GetProcItemNo(const AProcIndex: string; var ASectionIndex, AStartNo, AEndNo: Integer): Boolean;
var
  i, j: Integer;
  vData: THCSectionData;
begin
  Result := False;
  ASectionIndex := -1;
  AStartNo := -1;
  AEndNo := -1;

  for i := 0 to Self.Sections.Count - 1 do
  begin
    vData := Self.Sections[i].Page;
    for j := 0 to vData.Items.Count - 1 do
    begin
      if (vData.Items[j] is TDeGroup) and ((vData.Items[j] as TDeGroup)[TDeProp.Index] = AProcIndex) then
      begin
        ASectionIndex := i;
        AStartNo := j;
        Break;
      end;
    end;
  end;

  if AStartNo >= 0 then
  begin
    AEndNo := vData.GetDomainAnother(AStartNo);
    Result := AEndNo >= 0;
  end;
end;

function THCEmrView.GetProcProperty(const AProcIndex, APropName: string): string;
var
  vSectionIndex, vBeginNo, vEndNo: Integer;
  vBeginGroup: TDeGroup;
begin
  Result := '';
  if GetProcItemNo(AProcIndex, vSectionIndex, vBeginNo, vEndNo) then
  begin
    vBeginGroup := Self.Sections[vSectionIndex].Page.Items[vBeginNo] as TDeGroup;

    if APropName = TGroupProp.Name then
      Result := vBeginGroup[TDeProp.Name]
    else
    if APropName = TGroupProp.Propertys then  // ��������һ�δ���
      Result := vBeginGroup.Propertys.Text
    else
      Result := vBeginGroup[APropName];
  end;
end;

procedure THCEmrView.SetEditProcIndex(const Value: string);
var
  vSectionIndex, vBeginNo, vEndNo: Integer;
begin
  if FEditProcIndex <> Value then
  begin
    FEditProcIndex := Value;
    FEditProcInfo.Clear;
    GetProcItemNo(Value, vSectionIndex, vBeginNo, vEndNo);
    if vEndNo > 0 then
    begin
      if Self.ActiveSectionIndex <> vSectionIndex then
        Self.ActiveSectionIndex := vSectionIndex;

      FEditProcInfo.SectionIndex := vSectionIndex;
      FEditProcInfo.Data := Self.ActiveSection.Page;
      FEditProcInfo.BeginNo := vBeginNo;
      FEditProcInfo.EndNo := vEndNo;
      FEditProcInfo.Index := Value;
    end;

    Self.UpdateView;
  end;
end;

function THCEmrView.SetProcProperty(const AProcIndex, APropName, APropValue: string): Boolean;
var
  i, vSectionIndex, vBeginNo, vEndNo: Integer;
  vBeginGroup, vEndGroup: TDeGroup;
  vPropertys: TStringList;
begin
  Result := False;
  if GetProcItemNo(AProcIndex, vSectionIndex, vBeginNo, vEndNo) then
  begin
    vBeginGroup := Self.Sections[vSectionIndex].Page.Items[vBeginNo] as TDeGroup;
    vEndGroup := Self.Sections[vSectionIndex].Page.Items[vEndNo] as TDeGroup;

    if APropName = TGroupProp.Name then
    begin
      if APropValue <> '' then
      begin
        vBeginGroup[TDeProp.Name] := APropValue;
        vEndGroup[TDeProp.Name] := APropValue;
      end;
    end
    else
    if APropName = TGroupProp.Propertys then  // ��������һ�δ���
    begin
      vPropertys := TStringList.Create;
      try
        vPropertys.Text := APropValue;
        for i := 0 to vPropertys.Count - 1 do
        begin
          if vPropertys.Names[i] = TGroupProp.Name then
          begin
            if vPropertys.ValueFromIndex[i] <> '' then
            begin
              vBeginGroup[TDeProp.Name] := APropValue;
              vEndGroup[TDeProp.Name] := APropValue;
            end;
          end
          else
          begin
            vBeginGroup[vPropertys.Names[i]] := vPropertys.ValueFromIndex[i];
            vEndGroup[vPropertys.Names[i]] := vPropertys.ValueFromIndex[i];
          end;
        end;
      finally
        vPropertys.Free;
      end;
    end
    else
    begin
      vBeginGroup[APropName] := APropValue;
      vEndGroup[APropName] := APropValue;
    end;

    Result := True;
  end;
end;

function THCEmrView.GetProcAsText(const AProcIndex: string; var AText: string): Boolean;
var
  vSectionIndex, vStartNo, vEndNo: Integer;
  vSection: THCSection;
begin
  Result := False;

  if GetProcItemNo(AProcIndex, vSectionIndex, vStartNo, vEndNo) then
  begin
    if vEndNo = vStartNo + 1 then Exit;  // �м�û������

    AText := Sections[vSectionIndex].Page.SaveToText(vStartNo + 1, 0,
      vEndNo - 1, Sections[vSectionIndex].Page.GetItemOffsetAfter(vEndNo - 1));

    Result := True;
  end;
end;

function THCEmrView.SetProcByText(const AProcIndex, AText: string): Boolean;
var
  vSectionIndex, vStartNo, vEndNo: Integer;
  vSection: THCSection;
begin
  Result := False;
  if CanNotEdit then Exit;

  if GetProcItemNo(AProcIndex, vSectionIndex, vStartNo, vEndNo) then
  begin
    Self.BeginUpdate;
    try
      Self.UndoGroupBegin;
      try
        vSection := Self.Sections[vSectionIndex];
        // ѡ�У�ʹ�ò���ʱɾ����ǰ�������е�����
        vSection.Page.SetSelectBound(vStartNo, OffsetAfter, vEndNo, OffsetBefor);
        FIgnoreAcceptAction := True;
        try
          vSection.InsertText(AText);
        finally
          FIgnoreAcceptAction := False;
        end;

        CheckCaretProcInfo;
        Result := True;
      finally
        Self.UndoGroupEnd;
      end;
    finally
      Self.EndUpdate;
    end;
  end;
end;

function THCEmrView.GetProcAsStream(const AProcIndex: string; const AStream: TStream): Boolean;
var
  vSectionIndex, vStartNo, vEndNo: Integer;
  vParaFirst: Boolean;
  vSection: THCSection;
begin
  Result := False;

  if GetProcItemNo(AProcIndex, vSectionIndex, vStartNo, vEndNo) then
  begin
    if vEndNo = vStartNo + 1 then Exit;  // �м�û������

    DataSaveLiteStream(AStream, procedure()
    begin
      vSection := Sections[vSectionIndex];
      vParaFirst := vSection.Page.Items[vStartNo].ParaFirst;
      if not vParaFirst then  // [ ���Ƕ���(ʮ�а˾�)
        vSection.Page.Items[vStartNo].ParaFirst := True;  // ��֤��ĵ�һ���Ƕ���(Σ�ղ���ͨ��try����)

      try
        vSection.Page.SaveToStream(AStream, vStartNo + 1, 0,
          vEndNo - 1, vSection.Page.GetItemOffsetAfter(vEndNo - 1));
      finally
        if not vParaFirst then  // ��������
          vSection.Page.Items[vStartNo].ParaFirst := False;
      end;
    end);

    Result := True;
  end;
end;

function THCEmrView.SetProcByStream(const AProcIndex: string; const AStream: TStream): Boolean;
var
  vSectionIndex, vStartNo, vEndNo: Integer;
  vSection: THCSection;
begin
  Result := False;
  if CanNotEdit then Exit;

  if GetProcItemNo(AProcIndex, vSectionIndex, vStartNo, vEndNo) then
  begin
    DataLoadLiteStream(AStream, procedure(const AFileVersion: Word; const AStyle: THCStyle)
    begin
      Self.BeginUpdate;
      try
        Self.UndoGroupBegin;
        try
          vSection := Self.Sections[vSectionIndex];
          // ѡ�У�ʹ�ò���ʱɾ����ǰ�������е�����
          vSection.Page.SetSelectBound(vStartNo, OffsetAfter, vEndNo, OffsetBefor);
          FIgnoreAcceptAction := True;
          try
            Self.Style.States.Include(hosDomainWholeReplace);
            try
              vSection.InsertStream(AStream, AStyle, AFileVersion);
            finally
              Self.Style.States.Exclude(hosDomainWholeReplace);
            end;
          finally
            FIgnoreAcceptAction := False;
          end;
        finally
          Self.UndoGroupEnd;
        end;
      finally
        Self.EndUpdate;
      end;
    end);

    CheckCaretProcInfo;
    Result := True;
  end;
end;

function THCEmrView.SetProcByFileSteam(const AProcIndex: string;
  const AStream: TStream): Boolean;
var
  vSectionIndex, vStartNo, vEndNo: Integer;
  vSection: THCSection;
begin
  Result := False;
  if CanNotEdit then Exit;

  vStartNo := -1;
  vEndNo := -1;
  if GetProcItemNo(AProcIndex, vSectionIndex, vStartNo, vEndNo) then
  begin
    if Self.ActiveSectionIndex <> vSectionIndex then
      Self.ActiveSectionIndex := vSectionIndex;

    vSection := Sections[vSectionIndex];
    // ѡ�У�ʹ�ò���ʱɾ����ǰ�������е�����
    vSection.Page.SetSelectBound(vStartNo, OffsetAfter, vEndNo, OffsetBefor);
    FIgnoreAcceptAction := True;
    try
      Self.Style.States.Include(hosDomainWholeReplace);
      try
        Self.InsertStream(AStream);
      finally
        Self.Style.States.Exclude(hosDomainWholeReplace);
      end;
    finally
      FIgnoreAcceptAction := False;
    end;

    CheckCaretProcInfo;
    Result := True;
  end;
end;

procedure THCEmrView.ScrollToProc(const AProcIndex: string);
var
  vItemNo, vSecIndex, vEndNo: Integer;
  vPage: THCPageData;
  vPos: Integer absolute vEndNo;
begin
  if AProcIndex = '' then Exit;

  if AProcIndex = FEditProcIndex then
  begin
    vItemNo := FEditProcInfo.BeginNo;
    vSecIndex := FEditProcInfo.SectionIndex;
  end
  else
    GetProcItemNo(AProcIndex, vSecIndex, vItemNo, vEndNo);

  if vItemNo >= 0 then
  begin
    vPage := Self.Sections[vSecIndex].Page;
    vPos := vPage.DrawItems[vPage.Items[vItemNo].FirstDItemNo].Rect.Top;
    vPos := Self.Sections[vSecIndex].PageDataFormtToFilmCoord(vPos);
    vPos := vPos + Self.GetSectionTopFilm(vSecIndex);
    Self.VScrollBar.Position := vPos;
  end;
end;
{$ENDIF}

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
  DataSaveLiteStream(AStream, procedure()
  begin
    AData.SaveItemToStream(AStream, ADeGroupStartNo + 1, 0,
      ADeGroupEndNo - 1, AData.Items[ADeGroupEndNo - 1].Length);
  end);
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
  {$IFDEF PROCSERIES}
  CheckCaretProcInfo;
  {$ENDIF}
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
  vEmrTraceItem.TraceStyle := TDeTraceStyle.cseAdd;

  Self.InsertItem(vEmrTraceItem);
end;

procedure THCEmrView.KeyDown(var Key: Word; Shift: TShiftState);
var
  vData: THCRichData;
  vText, vCurTrace: string;
  vStyleNo, vParaNo: Integer;
  vDeItem: TDeItem;
  vCurItem: THCCustomItem;
  vCurTraceStyle: TDeTraceStyle;
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
      vCurTraceStyle := TDeTraceStyle.cseNone;

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
            vCurTraceStyle := vDeItem.TraceStyle;
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
            vCurTraceStyle := vDeItem.TraceStyle;
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
          if (vCurTraceStyle = TDeTraceStyle.cseAdd) and (vCurTrace = '') then Exit;  // �����δ��Ч�ۼ�����ֱ��ɾ��

          // ����ɾ���ַ���Ӧ��Item
          vDeItem := TDeItem.CreateByText(vText);
          vDeItem.StyleNo := vStyleNo;  // Style.CurStyleNo;
          vDeItem.ParaNo := vParaNo;  // Style.CurParaNo;

          if (vCurTraceStyle = TDeTraceStyle.cseDel) and (vCurTrace = '') then  // ԭ����ɾ��δ��Ч�ۼ�
            vDeItem.TraceStyle := TDeTraceStyle.cseNone  // ȡ��ɾ���ۼ�
          else  // ����ɾ���ۼ�
            vDeItem.TraceStyle := TDeTraceStyle.cseDel;

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
  Result.StyleNo := Self.Style.GetStyleNo(Self.Style.DefaultTextStyle, True);
  {if Self.CurStyleNo > THCStyle.Null then
    Result.StyleNo := Self.CurStyleNo
  else
    Result.StyleNo := 0;}

  Result.ParaNo := Self.CurParaNo;
end;

procedure THCEmrView.SetDeGroupByFileStream(const ASection: THCSection;
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
    {$IFDEF PROCSERIES}
    if FEditProcIndex <> '' then
      vStartNo := FEditProcInfo.EndNo
    else
    {$ENDIF}
    vStartNo := vData.Items.Count - 1;
    GetDataDeGroupItemNo(vData, ADeIndex, True, vStartNo, vEndNo)
  end
  else
  begin
    {$IFDEF PROCSERIES}
    if FEditProcIndex <> '' then
      vStartNo := FEditProcInfo.BeginNo
    else
    {$ENDIF}
    GetDataDeGroupItemNo(vData, ADeIndex, False, vStartNo, vEndNo);
  end;

  if vEndNo > 0 then
  begin
    {$IFDEF PROCSERIES}
    if FEditProcIndex <> '' then
    begin
      if (vStartNo < FEditProcInfo.BeginNo) or (vEndNo > FEditProcInfo.EndNo) then
        Exit;
    end;
    {$ENDIF}

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

    {$IFDEF PROCSERIES}
    CheckCaretProcInfo;
    {$ENDIF}
  end;
end;

procedure THCEmrView.SetDeGroupByText(const ASection: THCSection;
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
    {$IFDEF PROCSERIES}
    if FEditProcIndex <> '' then
      vStartNo := FEditProcInfo.EndNo
    else
    {$ENDIF}
    vStartNo := vData.Items.Count - 1;
    GetDataDeGroupItemNo(vData, ADeIndex, True, vStartNo, vEndNo)
  end
  else
  begin
    {$IFDEF PROCSERIES}
    if FEditProcIndex <> '' then
      vStartNo := FEditProcInfo.BeginNo
    else
    {$ENDIF}
    GetDataDeGroupItemNo(vData, ADeIndex, False, vStartNo, vEndNo);
  end;

  if vEndNo > 0 then
  begin
    {$IFDEF PROCSERIES}
    if FEditProcIndex <> '' then
    begin
      if (vStartNo < FEditProcInfo.BeginNo) or (vEndNo > FEditProcInfo.EndNo) then
        Exit;
    end;
    {$ENDIF}

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

    {$IFDEF PROCSERIES}
    CheckCaretProcInfo;
    {$ENDIF}
  end;
end;

procedure THCEmrView.SaveSelectToStream(const AStream: TStream);
begin
  DataSaveLiteStream(AStream, procedure()
  begin
    Self.Style.States.Include(THCState.hosCopying);  // ȥ��������һ���������
    try
      Self.ActiveSectionTopLevelData.SaveSelectToStream(AStream);
    finally
      Self.Style.States.Exclude(THCState.hosCopying);
    end;
  end);
end;

function THCEmrView.SaveSelectToText: string;
begin
  Result := Self.ActiveSectionTopLevelData.SaveSelectToText;
end;

procedure THCEmrView.SetActiveItemExtra(const AStream: TStream);
begin
  DataLoadLiteStream(AStream, procedure(const AFileVersion: Word; const AStyle: THCStyle)
  var
    vTopData: THCRichData;
  begin
    Self.BeginUpdate;
    try
      Self.UndoGroupBegin;
      try
        vTopData := Self.ActiveSectionTopLevelData as THCRichData;
        Self.DeleteActiveDataItems(vTopData.SelectInfo.StartItemNo);
        ActiveSection.InsertStream(AStream, AStyle, AFileVersion);
      finally
        Self.UndoGroupEnd;
      end;
    finally
      Self.EndUpdate;
    end;
  end);
end;

procedure THCEmrView.SetDataDeGroupFromStream(const AData: THCViewData;
  const ADeGroupStartNo, ADeGroupEndNo: Integer; const AStream: TStream);
begin
  AStream.Position := 0;
  DataLoadLiteStream(AStream, procedure(const AFileVersion: Word; const AStyle: THCStyle)
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
          // ����ο�����ѡ������Ϊ���ĵط������ʽ�����ǲ��Ǹ���
          AData.InsertStream(AStream, AStyle, AFileVersion);
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
  end);

  {$IFDEF PROCSERIES}
  CheckCaretProcInfo;
  {$ENDIF}
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

  {$IFDEF PROCSERIES}
  CheckCaretProcInfo;
  {$ENDIF}
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
    vItemTraverse.Areas := [saPage];//, saHeader, saFooter];
    vItemTraverse.Process := procedure (const AData: THCCustomData; const AItemNo,
      ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean)
    var
      vPropertys: TStringList;
      i: Integer;
    begin
      if not AData.CanEdit then  // �й�겻�ڵ�ǰ�༭�Ĳ���ʱ������༭���µĲ����滻������
      begin
        AStop := True;
        Exit;
      end;

      {$IFDEF PROCSERIES}
      if FEditProcIndex <> '' then
      begin
        if (FEditProcInfo.SectionIndex = vItemTraverse.SectionIndex) and (AData = FEditProcInfo.Data) then
        begin
         if (AItemNo < FEditProcInfo.BeginNo) or (AItemNo > FEditProcInfo.EndNo) then
           Exit;
        end;
      end;
      {$ENDIF}

      vItem := AData.Items[AItemNo];
      if (vItem is TDeItem) and ((vItem as TDeItem)[TDeProp.Index] = ADeIndex) then
      begin
        if APropName = 'Text' then
        begin
          if APropValue <> '' then
          begin
            vItem.Text := APropValue;
            (vItem as TDeItem).AllocValue := True;
            AData.SilenceChange;
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
                  AData.SilenceChange;
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
          if vDeItem.TraceStyle = TDeTraceStyle.cseDel then
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

function THCEmrView.SetSignatureGraphic(const ADeIndex: string; const AGraphicStream: TStream): Boolean;
var
  vItemTraverse: THCItemTraverse;
  vBeginNo, vEndNo: Integer;
  vTravItem: THCCustomItem;
  vResult: Boolean;
begin
  Result := False;
  vResult := False;

  {$IFDEF PROCSERIES}
  if FEditProcInfo.EndNo > 0 then
  begin
    vBeginNo := FEditProcInfo.BeginNo;
    vEndNo := FEditProcInfo.EndNo;
  end
  else
  {$ENDIF}
  begin
    vBeginNo := 0;
    vEndNo := Self.ActiveSection.Page.Items.Count - 1;
  end;

  vItemTraverse := THCItemTraverse.Create;
  try
    vItemTraverse.Tag := 0;
    vItemTraverse.Areas := [saPage];
    vItemTraverse.Process := procedure (const AData: THCCustomData; const AItemNo,
      ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean)
    begin
      if AData is THCPageData then
      begin
        if AItemNo >= vBeginNo then
        begin
          vTravItem := AData.Items[AItemNo];
          if (vTravItem is TDeImageItem) and ((vTravItem as TDeImageItem)[TDeProp.Index] = ADeIndex) then
          begin
            (vTravItem as TDeImageItem).LoadGraphicStream(AGraphicStream, False);
            vResult := True;
            AStop := True;
          end;
        end;

        if AItemNo = vEndNo then
          AStop := True;
      end;
    end;

    Self.ActiveSection.Page.TraverseItem(vItemTraverse);
  finally
    vItemTraverse.Free;
  end;

  if vResult then
  begin
    Self.FormatData;
    Result := vResult;
  end;
end;

procedure THCEmrView.SyncDeItemAfterRef(const AStartData: THCCustomData; const ARefDeItem: TDeItem);
var
  vItemTraverse: THCItemTraverse;
  vItem: THCCustomItem;
  vDeItem: TDeItem;
  //vData: THCCustomData;
  vStart, vFind: Boolean;
begin
  vStart := False;
  vFind := False;

  {if Assigned(AStartData) then
    vData := AStartData
  else
    vData := Self.ActiveSection.Page;}

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
        ATraverse.SectionIndex := i;

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
