{*******************************************************}
{                                                       }
{         ����HCView�ĵ��Ӳ�������  ���ߣ���ͨ          }
{                                                       }
{ �˴������ѧϰ����ʹ�ã�����������ҵĿ�ģ��ɴ�������  }
{ �����ʹ���߳е�������QQȺ 649023932 ����ȡ����ļ��� }
{ ������                                                }
{                                                       }
{*******************************************************}

unit frm_Record;

interface

{$I HCEmrView.inc}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.XPMan,
  System.ImageList, Vcl.ImgList, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ToolWin,
  System.Generics.Collections, HCEmrView, HCView, HCRichData, HCItem, HCCustomData,
  HCEmrGroupItem, HCEmrElementItem, HCDrawItem, HCSection, frm_RecordPop, System.Actions,
  frm_DataElement, Vcl.ActnList, Vcl.Buttons, HCCommon, CFControl, CFEdit,
  CFCombobox, CFLable, CFToolButton, frm_Symbol;

type
  TTravTag = class(TObject)
    public
      class function Contains(const ATags, ATag: Integer): Boolean;
    const
      WriteTraceInfo = 1;  // �������ݣ�Ϊ�ºۼ����Ӻۼ���Ϣ
      HideTrace = 1 shl 1;  // ���غۼ�����
      //DataSetElement = 1 shl 2;  // ������ݼ���Ҫ������Ԫ
  end;

  //TTraverseTags = set of TTraverseTag;
  TDeItemInsertEvent = procedure(const AEmrView: THCEmrView; const ASection: THCSection;
    const AData: THCCustomData; const AItem: THCCustomItem) of object;
  TDeItemPopupEvent = function(const ADeItem: TDeItem): Boolean of object;
  TDeItemGetSyncValue = function(const ADesID: Integer; const ADeItem: TDeItem): string of object;

  TfrmRecord = class(TForm)
    tlbTool: TToolBar;
    btnFile: TToolButton;
    btnprint: TToolButton;
    btn3: TToolButton;
    cbbZoom: TComboBox;
    btnSymmetryMargin: TToolButton;
    btn4: TToolButton;
    cbbFont: TComboBox;
    btn1: TToolButton;
    cbbFontSize: TComboBox;
    cbBackColor: TColorBox;
    cbFontColor: TColorBox;
    btnBold: TToolButton;
    btnItalic: TToolButton;
    btnUnderLine: TToolButton;
    btnStrikeOut: TToolButton;
    btnSuperScript: TToolButton;
    btnSubScript: TToolButton;
    btnAlignLeft: TToolButton;
    btnAlignCenter: TToolButton;
    btnAlignRight: TToolButton;
    btnAlignJustify: TToolButton;
    btnAlignScatter: TToolButton;
    btnLineSpace: TToolButton;
    btn9: TToolButton;
    il1: TImageList;
    pmView: TPopupMenu;
    mniCut: TMenuItem;
    mniCopy: TMenuItem;
    mniPaste: TMenuItem;
    mniTable: TMenuItem;
    mniInsertRowTop: TMenuItem;
    mniInsertRowBottom: TMenuItem;
    mniInsertColLeft: TMenuItem;
    mniInsertColRight: TMenuItem;
    mniDeleteCurRow: TMenuItem;
    mniDeleteCurCol: TMenuItem;
    mniN25: TMenuItem;
    mniDisBorder: TMenuItem;
    mniPara: TMenuItem;
    pmLineSpace: TPopupMenu;
    mniLineSpace100: TMenuItem;
    mniLineSpace150: TMenuItem;
    mniLineSpace200: TMenuItem;
    pmFile: TPopupMenu;
    mniOpen: TMenuItem;
    mniSave: TMenuItem;
    mniSaveAs: TMenuItem;
    mniPageSet: TMenuItem;
    mniPrint: TMenuItem;
    mniMerge: TMenuItem;
    mniClear: TMenuItem;
    pmInsert: TPopupMenu;
    mniInsertTable: TMenuItem;
    mniN4: TMenuItem;
    mniCheckbox: TMenuItem;
    mniInsertImage: TMenuItem;
    mniN7: TMenuItem;
    mniYueJing: TMenuItem;
    mniInsertLine: TMenuItem;
    mniPageBreak: TMenuItem;
    mniSection: TMenuItem;
    mniInsertGif: TMenuItem;
    actlst: TActionList;
    actSave: TAction;
    mniDeItem: TMenuItem;
    mniDeGroup: TMenuItem;
    mniDeItemProp: TMenuItem;
    mniDeleteDeItem: TMenuItem;
    mniDeleteGroup: TMenuItem;
    mniN13: TMenuItem;
    mniN14: TMenuItem;
    mniN15: TMenuItem;
    mniTooth: TMenuItem;
    mniEditItem: TMenuItem;
    mniReSyncDeGroup: TMenuItem;
    mniLineSpace115: TMenuItem;
    mniControlItem: TMenuItem;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    mniSplitRow: TMenuItem;
    mniSplitCol: TMenuItem;
    mniN21: TMenuItem;
    mniCombobox: TMenuItem;
    mniTableProperty: TMenuItem;
    mniFangJiao: TMenuItem;
    mniN5: TMenuItem;
    btnRightIndent: TToolButton;
    btnLeftIndent: TToolButton;
    btnUndo: TToolButton;
    btnRedo: TToolButton;
    actUndo: TAction;
    actRedo: TAction;
    btnSave: TSpeedButton;
    btnInsert: TSpeedButton;
    mniSplit: TMenuItem;
    mniDeleteProtect: TMenuItem;
    mniSaveStructure: TMenuItem;
    mniFastPrint: TMenuItem;
    mniPirntPreview: TMenuItem;
    mniPrintCurLine: TMenuItem;
    mniPrintSelect: TMenuItem;
    mniN20: TMenuItem;
    mniInsertDeItem: TMenuItem;
    mniHideTrace: TMenuItem;
    mniN1: TMenuItem;
    mniN2: TMenuItem;
    mniSupSub: TMenuItem;
    mniDateTime: TMenuItem;
    mniRadioGroup: TMenuItem;
    pmOddEvenPrint: TPopupMenu;
    mniPrintOdd: TMenuItem;
    mniPrintEven: TMenuItem;
    pnlPrint: TPanel;
    cflbl1: TCFLable;
    btnPrintAll: TCFToolButton;
    btnOdevity: TCFMenuButton;
    cflbl2: TCFLable;
    edtPageStart: TCFEdit;
    edtPageEnd: TCFEdit;
    btnPrintRange: TCFToolButton;
    cflbl3: TCFLable;
    chkPrintHeader: TCheckBox;
    chkPrintFooter: TCheckBox;
    btnPrintCurLine: TCFToolButton;
    btnPrintSelect: TCFToolButton;
    cflbl4: TCFLable;
    cbbPrinter: TComboBox;
    mniN3: TMenuItem;
    mniViewFilm: TMenuItem;
    mniViewPage: TMenuItem;
    mniInputHelp: TMenuItem;
    mniN6: TMenuItem;
    mniFloatLine: TMenuItem;
    mniN8: TMenuItem;
    mniBarCode: TMenuItem;
    mniQRCode: TMenuItem;
    mniCopyProtect: TMenuItem;
    mniContentAlign: TMenuItem;
    mniCellVTHL: TMenuItem;
    mniCellVTHM: TMenuItem;
    mniCellVTHR: TMenuItem;
    mniN16: TMenuItem;
    mniCellVMHL: TMenuItem;
    mniCellVMHM: TMenuItem;
    mniCellVMHR: TMenuItem;
    mniN22: TMenuItem;
    mniCellVBHL: TMenuItem;
    mniCellVBHM: TMenuItem;
    mniCellVBHR: TMenuItem;
    mniFloatBarCode: TMenuItem;
    mniFloatItemProperty: TMenuItem;
    mniViewText: TMenuItem;
    mniSyntax: TMenuItem;
    btnPrintCurLineToPage: TCFToolButton;
    mniReSyncDeItem: TMenuItem;
    mniSymbol: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBoldClick(Sender: TObject);
    procedure btnprintClick(Sender: TObject);
    procedure cbbZoomChange(Sender: TObject);
    procedure btnSymmetryMarginClick(Sender: TObject);
    procedure cbbFontChange(Sender: TObject);
    procedure cbbFontSizeChange(Sender: TObject);
    procedure cbBackColorChange(Sender: TObject);
    procedure cbFontColorChange(Sender: TObject);
    procedure btnAlignLeftClick(Sender: TObject);
    procedure mniLineSpace100Click(Sender: TObject);
    procedure mniOpenClick(Sender: TObject);
    procedure mniMergeClick(Sender: TObject);
    procedure mniInsertRowTopClick(Sender: TObject);
    procedure mniInsertRowBottomClick(Sender: TObject);
    procedure mniInsertColLeftClick(Sender: TObject);
    procedure mniInsertColRightClick(Sender: TObject);
    procedure mniDeleteCurRowClick(Sender: TObject);
    procedure mniDeleteCurColClick(Sender: TObject);
    procedure mniDisBorderClick(Sender: TObject);
    procedure mniParaClick(Sender: TObject);
    procedure mniSaveAsClick(Sender: TObject);
    procedure pmViewPopup(Sender: TObject);
    procedure mniClearClick(Sender: TObject);
    procedure mniInsertTableClick(Sender: TObject);
    procedure mniCheckboxClick(Sender: TObject);
    procedure mniInsertImageClick(Sender: TObject);
    procedure mniYueJingClick(Sender: TObject);
    procedure mniInsertLineClick(Sender: TObject);
    procedure mniPageBreakClick(Sender: TObject);
    procedure mniSectionClick(Sender: TObject);
    procedure mniInsertGifClick(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mniDeleteGroupClick(Sender: TObject);
    procedure mniDeleteDeItemClick(Sender: TObject);
    procedure mniPageSetClick(Sender: TObject);
    procedure mniToothClick(Sender: TObject);
    procedure mniEditItemClick(Sender: TObject);
    procedure mniReSyncDeGroupClick(Sender: TObject);
    procedure mniSplitRowClick(Sender: TObject);
    procedure mniSplitColClick(Sender: TObject);
    procedure mniComboboxClick(Sender: TObject);
    procedure mniControlItemClick(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure mniTablePropertyClick(Sender: TObject);
    procedure mniFangJiaoClick(Sender: TObject);
    procedure mniN5Click(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure mniDeItemPropClick(Sender: TObject);
    procedure mniDeleteProtectClick(Sender: TObject);
    procedure mniSaveStructureClick(Sender: TObject);
    procedure mniPrintCurLineClick(Sender: TObject);
    procedure mniFastPrintClick(Sender: TObject);
    procedure mniPirntPreviewClick(Sender: TObject);
    procedure mniPrintSelectClick(Sender: TObject);
    procedure mniInsertDeItemClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mniHideTraceClick(Sender: TObject);
    procedure pmFilePopup(Sender: TObject);
    procedure mniSupSubClick(Sender: TObject);
    procedure mniDateTimeClick(Sender: TObject);
    procedure mniRadioGroupClick(Sender: TObject);
    procedure btnPrintAllClick(Sender: TObject);
    procedure mniPrintOddClick(Sender: TObject);
    procedure mniPrintEvenClick(Sender: TObject);
    procedure btnPrintRangeClick(Sender: TObject);
    procedure btnPrintCurLineClick(Sender: TObject);
    procedure btnPrintSelectClick(Sender: TObject);
    procedure mniViewFilmClick(Sender: TObject);
    procedure mniViewPageClick(Sender: TObject);
    procedure mniInputHelpClick(Sender: TObject);
    procedure mniFloatLineClick(Sender: TObject);
    procedure mniBarCodeClick(Sender: TObject);
    procedure mniQRCodeClick(Sender: TObject);
    procedure mniCopyProtectClick(Sender: TObject);
    procedure mniCellVMHMClick(Sender: TObject);
    procedure mniCellVTHLClick(Sender: TObject);
    procedure mniCellVTHMClick(Sender: TObject);
    procedure mniCellVTHRClick(Sender: TObject);
    procedure mniCellVMHLClick(Sender: TObject);
    procedure mniCellVMHRClick(Sender: TObject);
    procedure mniCellVBHLClick(Sender: TObject);
    procedure mniCellVBHMClick(Sender: TObject);
    procedure mniCellVBHRClick(Sender: TObject);
    procedure mniFloatBarCodeClick(Sender: TObject);
    procedure mniFloatItemPropertyClick(Sender: TObject);
    procedure mniViewTextClick(Sender: TObject);
    procedure mniSyntaxClick(Sender: TObject);
    procedure btnPrintCurLineToPageClick(Sender: TObject);
    procedure mniReSyncDeItemClick(Sender: TObject);
    procedure mniSymbolClick(Sender: TObject);
  private
    { Private declarations }
    //FMouseDownTick: Cardinal;
    FMouseInElementFire: Boolean;
    FfrmRecordPop: TfrmRecordPop;
    FEmrView: THCEmrView;
    FfrmSymbol: TfrmSymbol;
    FOnSave, FOnSaveStructure, FOnChangedSwitch, FOnReadOnlySwitch: TNotifyEvent;
    FOnInsertDeItem: TDeItemInsertEvent;
    FOnSetDeItemText: TDeItemSetTextEvent;
    FOnDeItemPopup: TDeItemPopupEvent;
    FOnPrintPreview: TNotifyEvent;
    FOnDeItemGetSyncValue: TDeItemGetSyncValue;

    function GetOnCopyRequest: THCCopyPasteEvent;
    procedure SetOnCopyRequest(const Value: THCCopyPasteEvent);

    function GetOnPasteRequest: THCCopyPasteEvent;
    procedure SetOnPasteRequest(const Value: THCCopyPasteEvent);

    function GetOnCopyAsStream: THCCopyPasteStreamEvent;
    procedure SetOnCopyAsStream(const Value: THCCopyPasteStreamEvent);

    function GetOnPasteFromStream: THCCopyPasteStreamEvent;
    procedure SetOnPasteFromStream(const Value: THCCopyPasteStreamEvent);

    function GetOnSyntaxCheck: TDataDomainItemNoEvent;
    procedure SetOnSyntaxCheck(const Value: TDataDomainItemNoEvent);

    function GetOnSyntaxPaint: TSyntaxPaintEvent;
    procedure SetOnSyntaxPaint(const Value: TSyntaxPaintEvent);

    function GetEditToolVisible: Boolean;
    procedure SetEditToolVisible(const Value: Boolean);

    function GetPrintToolVisible: Boolean;
    procedure SetPrintToolVisible(const Value: Boolean);

    /// <summary> ��������ۼ����ػ���ʾ </summary>
    procedure DoHideTraceTraverse(const AData: THCCustomData;
      const AItemNo, ATags: Integer; const ADomainStack: TDomainStack; var AStop: Boolean);

    function GetHideTrace: Boolean;
    /// <summary> ���õ�ǰ�Ƿ����غۼ� </summary>
    procedure SetHideTrace(const Value: Boolean);

    /// <summary> �ĵ����λ�÷����仯ʱ���� </summary>
    procedure DoCaretChange(Sender: TObject);

    /// <summary> �ĵ��䶯״̬�����仯ʱ���� </summary>
    procedure DoChangedSwitch(Sender: TObject);

    /// <summary> �ĵ��༭ʱֻ����ǰλ�ò��ɱ༭ʱ���� </summary>
    procedure DoCanNotEdit(Sender: TObject);

    /// <summary> �ĵ�ֻ��״̬�����仯ʱ���� </summary>
    procedure DoReadOnlySwitch(Sender: TObject);

    /// <summary> �ĵ���ֱ����������ʱ���� </summary>
    procedure DoVerScroll(Sender: TObject);

    /// <summary> ����ҳ����ǰ�¼� </summary>
    procedure DoPaintPaperBefor(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);

    /// <summary> ���õ�ǰ����Ԫ���ı����� </summary>
    procedure DoSetActiveDeItemText(const ADeItem: TDeItem; const AText: string; var AReject: Boolean);

    function DoInsertTextBefor(const AData: THCCustomData; const AItemNo, AOffset: Integer;
      const AText: string): Boolean;

    procedure DoSyntaxPaint(const AData: THCCustomData; const AItemNo: Integer;
      const ADrawText: string; const ASyntax: TEmrSyntax; const ARect: TRect; const ACanvas: TCanvas);

    function DoPasteRequest(const AFormat: Word): Boolean;

    /// <summary> ��ǰ�����DeItemͬ��Ϊ��ǰ������ͬ������Ԫ���� </summary>
    function ActiveDeItemSync(const AActiveDeItem: TDeItem): Boolean;

    /// <summary> ���õ�ǰ����Ԫ������Ϊ��չ���� </summary>
    procedure DoSetActiveDeItemExtra(const ADeItem: TDeItem; const AStream: TStream);

    /// <summary> DeCombobox����ѡ�� </summary>
    procedure DoDeComboboxGetItem(Sender: TObject);

    /// <summary> ��ǰλ���ı���ʽ����һλ�ò�һ��ʱ�¼� </summary>
    procedure CurTextStyleChange(const ANewStyleNo: Integer);

    /// <summary> ��ǰλ�ö���ʽ����һλ�ò�һ��ʱ�¼� </summary>
    procedure CurParaStyleChange(const ANewStyleNo: Integer);

    /// <summary> ��ȡ����Ԫֵ������ </summary>
    function PopupForm: TfrmRecordPop;

    /// <summary> ��Ԫֵ������ر��¼� </summary>
    procedure PopupFormClose;
  protected
    function DoDeItemPopup(const ADeItem: TDeItem): Boolean;
    procedure DoEmrViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoEmrViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoSectionDrawItemMouseMove(const Sender: TObject; const AData: THCCustomData;
      const AItemNo, AOffset, ADrawItemNo: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    /// <summary> �������µ�Item����ʱ���� </summary>
    procedure DoInsertItem(const Sender: TObject; const AData: THCCustomData;
      const AItem: THCCustomItem);

    /// <summary> ���ñ��没������ </summary>
    procedure DoSave;

    /// <summary> ���ñ��没���ṹ���� </summary>
    procedure DoSaveStructure;
  public
    { Public declarations }
    ObjectData: Pointer;
    FfrmDataElement: TfrmDataElement;

    /// <summary> ���ع����� </summary>
    procedure HideToolbar;

    /// <summary> ����һ������Ԫ(�ı���ʽ) </summary>
    /// <param name="AIndex">����ԪΨһ��ʶ</param>
    /// <param name="AName">����Ԫ����</param>
    function InsertDeItem(const AIndex, AName: string): TDeItem;

    /// <summary> ����һ�������� </summary>
    /// <param name="AIndex">������Ψһ��ʶ</param>
    /// <param name="AName">����������</param>
    procedure InsertDeGroup(const AIndex, AName: string);

    /// <summary> ����һ������Ԫ(Edit��ʽ) </summary>
    /// <param name="AIndex">����ԪΨһ��ʶ</param>
    /// <param name="AName">����Ԫ����</param>
    function InsertDeEdit(const AIndex, AName: string): TDeEdit;

    /// <summary> ����һ������Ԫ(Combobox��ʽ) </summary>
    /// <param name="AIndex">����ԪΨһ��ʶ</param>
    /// <param name="AName">����Ԫ����</param>
    function InsertDeCombobox(const AIndex, AName: string): TDeCombobox;

    /// <summary> ����һ������Ԫ(DateTime��ʽ) </summary>
    /// <param name="AIndex">����ԪΨһ��ʶ</param>
    /// <param name="AName">����Ԫ����</param>
    function InsertDeDateTime(const AIndex, AName: string): TDeDateTimePicker;

    /// <summary> ����һ������Ԫ(RadioGroup��ʽ) </summary>
    /// <param name="AIndex">����ԪΨһ��ʶ</param>
    /// <param name="AName">����Ԫ����</param>
    function InsertDeRadioGroup(const AIndex, AName: string): TDeRadioGroup;
    function InsertDeFloatBarCode(const AIndex, AName: string): TDeFloatBarCodeItem;
    function InsertDeImage(const AIndex, AName: string): TDeImageItem;

    /// <summary> ����һ������Ԫ(CheckBox��ʽ) </summary>
    /// <param name="AIndex">����ԪΨһ��ʶ</param>
    /// <param name="AName">����Ԫ����</param>
    function InsertDeCheckBox(const AIndex, AName: string): TDeCheckBox;

    /// <summary> �����ĵ�ָ��Data��Item </summary>
    /// <param name="ATravEvent">ÿ������һ��Itemʱ�������¼�</param>
    /// <param name="AAreas">Ҫ������Data</param>
    /// <param name="ATag">������ʶ</param>
    procedure TraverseElement(const ATravEvent: TTraverseItemEvent;
      const AAreas: TSectionAreas = [saHeader, saPage, saFooter]; const ATag: Integer = 0);

    /// <summary>
    /// ���ĵ�����ΪͼƬ
    /// </summary>
    /// <param name="aPath">ͼƬ·��</param>
    /// <param name="aPrefix">ͼƬ����ǰ׺</param>
    /// <param name="aImageType">ͼƬ��ʽ</param>
    procedure SaveToImage(const APath, APrefix: string; const AImageType: string = 'PNG');

    /// <summary> �����༭�� </summary>
    property EmrView: THCEmrView read FEmrView;

    property PrintToolVisible: Boolean read GetPrintToolVisible write SetPrintToolVisible;

    property EditToolVisible: Boolean read GetEditToolVisible write SetEditToolVisible;

    /// <summary> ��ʾ���غۼ� </summary>
    property HideTrace: Boolean read GetHideTrace write SetHideTrace;

    /// <summary> ���没��ʱ���õķ��� </summary>
    property OnSave: TNotifyEvent read FOnSave write FOnSave;

    /// <summary> ���没���ṹʱ���õķ��� </summary>
    property OnSaveStructure: TNotifyEvent read FOnSaveStructure write FOnSaveStructure;

    /// <summary> �ĵ�Change״̬�л�ʱ���õķ��� </summary>
    property OnChangedSwitch: TNotifyEvent read FOnChangedSwitch write FOnChangedSwitch;

    /// <summary> ��ֻ�������б仯ʱ���õķ��� </summary>
    property OnReadOnlySwitch: TNotifyEvent read FOnReadOnlySwitch write FOnReadOnlySwitch;

    /// <summary> �����µ�Item����ʱ���õķ��� </summary>
    property OnInsertDeItem: TDeItemInsertEvent read FOnInsertDeItem write FOnInsertDeItem;

    /// <summary> ����DeItemֵʱ�����ķ��� </summary>
    property OnSetDeItemText: TDeItemSetTextEvent read FOnSetDeItemText write FOnSetDeItemText;

    /// <summary> ���DeItem�󵯳�������ǰ�����ķ��� </summary>
    property OnDeItemPopup: TDeItemPopupEvent read FOnDeItemPopup write FOnDeItemPopup;

    /// <summary> �����ӡԤ�������ķ��� </summary>
    property OnPrintPreview: TNotifyEvent read FOnPrintPreview write FOnPrintPreview;

    property OnDeItemGetSyncValue: TDeItemGetSyncValue read FOnDeItemGetSyncValue write FOnDeItemGetSyncValue;

    /// <summary> ��������ǰ���� </summary>
    property OnCopyRequest: THCCopyPasteEvent read GetOnCopyRequest write SetOnCopyRequest;

    /// <summary> ճ������ǰ���� </summary>
    property OnPasteRequest: THCCopyPasteEvent read GetOnPasteRequest write SetOnPasteRequest;

    property OnCopyAsStream: THCCopyPasteStreamEvent read GetOnCopyAsStream write SetOnCopyAsStream;

    property OnPasteFromStream: THCCopyPasteStreamEvent read GetOnPasteFromStream write SetOnPasteFromStream;

    property OnSyntaxCheck: TDataDomainItemNoEvent read GetOnSyntaxCheck write SetOnSyntaxCheck;

    property OnSyntaxPaint: TSyntaxPaintEvent read GetOnSyntaxPaint write SetOnSyntaxPaint;
  end;

implementation

uses
  Vcl.Clipbrd, HCStyle, HCTextStyle, HCParaStyle, System.DateUtils, HCPrinters, Printers,
  frm_InsertDeTable, frm_Paragraph, HCRectItem, HCImageItem, HCGifItem, HCEmrYueJingItem,
  HCSupSubScriptItem, HCViewData, HCEmrToothItem, HCEmrFangJiaoItem, frm_PageSet,
  frm_DeControlProperty, frm_DeTableProperty, frm_TableBorderBackColor, frm_DeProperty,
  frm_PrintView, emr_Common, HCCustomFloatItem, HCFloatLineItem, HCBarCodeItem,
  HCQRCodeItem, HCSectionData, frm_DeFloatItemProperty, frm_DeCombobox, frm_DeEdit,
  frm_DeRadioGroup, frm_DeChecBox, frm_DeDateTime, frm_DeImage, frm_DeFloatBarCode;

{$R *.dfm}

procedure TfrmRecord.actCopyExecute(Sender: TObject);
begin
  FEmrView.Copy;
end;

procedure TfrmRecord.actCutExecute(Sender: TObject);
begin
  FEmrView.Cut;
end;

function TfrmRecord.ActiveDeItemSync(const AActiveDeItem: TDeItem): Boolean;
var
  vSameDeItem: TDeItem;
begin
  Result := False;
  vSameDeItem := FEmrView.FindSameDeItem(AActiveDeItem);
  if Assigned(vSameDeItem) then
  begin
    AActiveDeItem[TDeProp.CMVVCode] := vSameDeItem[TDeProp.CMVVCode];
    FEmrView.SetActiveItemText(vSameDeItem.Text);
    Result := True;
  end;
end;

procedure TfrmRecord.actPasteExecute(Sender: TObject);
begin
  FEmrView.Paste;
end;

procedure TfrmRecord.actRedoExecute(Sender: TObject);
begin
  FEmrView.Redo;
end;

procedure TfrmRecord.actSaveExecute(Sender: TObject);
begin
  DoSave;
end;

procedure TfrmRecord.actUndoExecute(Sender: TObject);
begin
  FEmrView.Undo;
end;

procedure TfrmRecord.btnInsertClick(Sender: TObject);
var
  vPt: TPoint;
begin
  vPt := btnInsert.ClientToScreen(Point(0, btnInsert.Height));
  pmInsert.Popup(vPt.X, vPt.Y);
end;

procedure TfrmRecord.btnPrintAllClick(Sender: TObject);
begin
  FEmrView.Print(cbbPrinter.Text);
end;

procedure TfrmRecord.btnAlignLeftClick(Sender: TObject);
begin
  case (Sender as TToolButton).Tag of
    0: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahLeft);
    1: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahCenter);
    2: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahRight);
    3: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahJustify);  // ����
    4: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahScatter);  // ��ɢ
    5: FEmrView.ApplyParaLeftIndent;
    6: FEmrView.ApplyParaLeftIndent(False);
  end;
end;

procedure TfrmRecord.btnBoldClick(Sender: TObject);
begin
  case (Sender as TToolButton).Tag of
    0: FEmrView.ApplyTextStyle(THCFontStyle.tsBold);
    1: FEmrView.ApplyTextStyle(THCFontStyle.tsItalic);
    2: FEmrView.ApplyTextStyle(THCFontStyle.tsUnderline);
    3: FEmrView.ApplyTextStyle(THCFontStyle.tsStrikeOut);
    4: FEmrView.ApplyTextStyle(THCFontStyle.tsSuperscript);
    5: FEmrView.ApplyTextStyle(THCFontStyle.tsSubscript);
  end;
end;

procedure TfrmRecord.btnprintClick(Sender: TObject);
var
  vPrintDlg: TPrintDialog;
begin
  vPrintDlg := TPrintDialog.Create(nil);
  try
    vPrintDlg.MaxPage := FEmrView.PageCount;
    if vPrintDlg.Execute then
      FEmrView.Print(HCPrinter.Printers[Printer.PrinterIndex]);  // ����HCPrinter��PrinterIndex����ȷ������
  finally
    FreeAndNil(vPrintDlg);
  end;
end;

procedure TfrmRecord.btnPrintCurLineClick(Sender: TObject);
begin
  HCPrinter.PrinterIndex := HCPrinter.Printers.IndexOf(cbbPrinter.Text);
  FEmrView.PrintCurPageByActiveLine(cbbPrinter.Text, chkPrintHeader.Checked, chkPrintFooter.Checked);
end;

procedure TfrmRecord.btnPrintCurLineToPageClick(Sender: TObject);
begin
  HCPrinter.PrinterIndex := HCPrinter.Printers.IndexOf(cbbPrinter.Text);
  FEmrView.PrintCurPageByActiveLine(cbbPrinter.Text, chkPrintHeader.Checked, chkPrintFooter.Checked);

  if FEmrView.ActivePageIndex < FEmrView.PageCount - 1 then
      FEmrView.Print(cbbPrinter.Text, FEmrView.ActivePageIndex + 1, FEmrView.PageCount - 1, 1);
end;

procedure TfrmRecord.btnPrintRangeClick(Sender: TObject);
var
  vStartPage, vEndPage: Integer;
  vPages: array of Integer;
begin
  vStartPage := StrToIntDef(edtPageStart.Text, 0);
  if vStartPage < 0 then
    vStartPage := 0;

  vEndPage := StrToIntDef(edtPageEnd.Text, 0);
  if vEndPage > FEmrView.PageCount - 1 then
    vEndPage := FEmrView.PageCount - 1;

  FEmrView.Print(cbbPrinter.Text, vStartPage, vEndPage, 1);
end;

procedure TfrmRecord.btnPrintSelectClick(Sender: TObject);
begin
  HCPrinter.PrinterIndex := HCPrinter.Printers.IndexOf(cbbPrinter.Text);
  FEmrView.PrintCurPageSelected(cbbPrinter.Text, chkPrintHeader.Checked, chkPrintFooter.Checked);
end;

procedure TfrmRecord.btnSymmetryMarginClick(Sender: TObject);
begin
  FEmrView.SymmetryMargin := not FEmrView.SymmetryMargin;
end;

procedure TfrmRecord.cbbFontChange(Sender: TObject);
begin
  FEmrView.ApplyTextFontName(cbbFont.Text);
  if not FEmrView.Focused then
    FEmrView.SetFocus;
end;

procedure TfrmRecord.cbbFontSizeChange(Sender: TObject);
begin
  FEmrView.ApplyTextFontSize(GetFontSize(cbbFontSize.Text));
  if not FEmrView.Focused then
    FEmrView.SetFocus;
end;

procedure TfrmRecord.cbbZoomChange(Sender: TObject);
begin
  FEmrView.Zoom := (StrToInt(cbbZoom.Text) / 100);
end;

procedure TfrmRecord.cbBackColorChange(Sender: TObject);
begin
  FEmrView.ApplyTextBackColor(cbBackColor.Selected);
  if not FEmrView.Focused then
    FEmrView.SetFocus;
end;

procedure TfrmRecord.cbFontColorChange(Sender: TObject);
begin
  FEmrView.ApplyTextColor(cbFontColor.Selected);
  if not FEmrView.Focused then
    FEmrView.SetFocus;
end;

procedure TfrmRecord.CurParaStyleChange(const ANewStyleNo: Integer);
var
  vAlignHorz: TParaAlignHorz;
begin
  if ANewStyleNo >= 0 then
  begin
    vAlignHorz := FEmrView.Style.ParaStyles[ANewStyleNo].AlignHorz;

    btnAlignLeft.Down := vAlignHorz = TParaAlignHorz.pahLeft;
    btnAlignRight.Down := vAlignHorz = TParaAlignHorz.pahRight;
    btnAlignCenter.Down := vAlignHorz = TParaAlignHorz.pahCenter;
    btnAlignJustify.Down := vAlignHorz = TParaAlignHorz.pahJustify;
    btnAlignScatter.Down := vAlignHorz = TParaAlignHorz.pahScatter;

    case FEmrView.Style.ParaStyles[ANewStyleNo].LineSpaceMode of
      TParaLineSpaceMode.pls100: mniLineSpace100.Checked := True;
      TParaLineSpaceMode.pls115: mniLineSpace115.Checked := True;
      TParaLineSpaceMode.pls150: mniLineSpace150.Checked := True;
      TParaLineSpaceMode.pls200: mniLineSpace200.Checked := True;
    end;
  end;
end;

procedure TfrmRecord.CurTextStyleChange(const ANewStyleNo: Integer);
begin
  if ANewStyleNo >= 0 then
  begin
    cbbFont.ItemIndex := cbbFont.Items.IndexOf(FEmrView.Style.TextStyles[ANewStyleNo].Family);
    cbbFontSize.ItemIndex := cbbFontSize.Items.IndexOf(GetFontSizeStr(FEmrView.Style.TextStyles[ANewStyleNo].Size));
    btnBold.Down := tsBold in FEmrView.Style.TextStyles[ANewStyleNo].FontStyles;
    btnItalic.Down := tsItalic in FEmrView.Style.TextStyles[ANewStyleNo].FontStyles;
    btnUnderline.Down := tsUnderline in FEmrView.Style.TextStyles[ANewStyleNo].FontStyles;
    btnStrikeOut.Down := tsStrikeOut in FEmrView.Style.TextStyles[ANewStyleNo].FontStyles;
    btnSuperscript.Down := tsSuperscript in FEmrView.Style.TextStyles[ANewStyleNo].FontStyles;
    btnSubscript.Down := tsSubscript in FEmrView.Style.TextStyles[ANewStyleNo].FontStyles;
  end
  else
  begin
    btnBold.Down := False;
    btnItalic.Down := False;
    btnUnderline.Down := False;
    btnStrikeOut.Down := False;
    btnSuperscript.Down := False;
    btnSubscript.Down := False;
  end;
end;

procedure TfrmRecord.DoSectionDrawItemMouseMove(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, AOffset, ADrawItemNo: Integer;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vText: string;
  vLen, vWf, vWl: Integer;
  vRect: TRect;
begin
  FMouseInElementFire := False;
  if AData.Items[AItemNo].StyleNo < THCStyle.Null then Exit;
  if not (AData.Items[AItemNo] as TDeItem).IsElement then Exit;

  vText := AData.GetDrawItemText(ADrawItemNo);
  vLen := Length(vText);
  if vLen = 1 then
  begin
    vRect := AData.DrawItems[ADrawItemNo].Rect;
    OffsetRect(vRect, -vRect.Left, -vRect.Top);
    vWf := FEmrView.Style.TempCanvas.TextWidth(vText);
    if (X > vWf div 3) and (X < vRect.Right - vWf div 3) then
    begin
      GCursor := crArrow;
      FMouseInElementFire := True;
    end;
  end
  else
  if vLen > 1 then
  begin
    vRect := AData.DrawItems[ADrawItemNo].Rect;
    OffsetRect(vRect, -vRect.Left, -vRect.Top);
    FEmrView.Style.TextStyles[AData.Items[AItemNo].StyleNo].ApplyStyle(FEmrView.Style.TempCanvas);
    vWf := FEmrView.Style.TempCanvas.TextWidth(vText[1]);
    vWl := FEmrView.Style.TempCanvas.TextWidth(vText[vLen]);
    if (X > vWf div 2) and (X < vRect.Right - vWl div 2) then
    begin
      GCursor := crArrow;
      FMouseInElementFire := True;
    end;
  end;
end;

procedure TfrmRecord.DoSetActiveDeItemExtra(const ADeItem: TDeItem; const AStream: TStream);
begin
  FEmrView.SetActiveItemExtra(AStream);
end;

procedure TfrmRecord.DoSetActiveDeItemText(const ADeItem: TDeItem; const AText: string; var AReject: Boolean);
var
  vText: string;
begin
  if Assigned(FOnSetDeItemText) then
  begin
    vText := AText;
    FOnSetDeItemText(Self, ADeItem, vText, AReject);
    if not AReject then
      FEmrView.SetActiveItemText(vText);
  end
  else
    FEmrView.SetActiveItemText(AText);
  //FEmrView.ActiveSection.ReFormatActiveItem;
end;

procedure TfrmRecord.DoSyntaxPaint(const AData: THCCustomData; const AItemNo: Integer;
    const ADrawText: string; const ASyntax: TEmrSyntax; const ARect: TRect; const ACanvas: TCanvas);
begin
  if ASyntax.Problem = TEmrSyntaxProblem.espContradiction then  // ì��
    ACanvas.Brush.Color := clRed
  else
    ACanvas.Brush.Color := clBlue;

  ACanvas.FillRect(ARect);
end;

procedure TfrmRecord.DoHideTraceTraverse(const AData: THCCustomData; const AItemNo,
  ATags: Integer; const ADomainStack: TDomainStack; var AStop: Boolean);
var
  vDeItem: TDeItem;
begin
  if (not (AData.Items[AItemNo] is TDeItem))
    //or (not (AData.Items[AItemNo] is TDeGroup))
  then
    Exit;  // ֻ��Ԫ����Ч

  vDeItem := AData.Items[AItemNo] as TDeItem;
  if vDeItem.StyleEx = TStyleExtra.cseDel then
    vDeItem.Visible := not (ATags = TTravTag.HideTrace);  // ����/��ʾ�ۼ�
end;

procedure TfrmRecord.DoCanNotEdit(Sender: TObject);
begin
  ShowMessage('��ǰλ��ֻ�������ɱ༭��');
end;

procedure TfrmRecord.DoCaretChange(Sender: TObject);
begin
  CurTextStyleChange(FEmrView.CurStyleNo);
  CurParaStyleChange(FEmrView.CurParaNo);
end;

procedure TfrmRecord.DoChangedSwitch(Sender: TObject);
begin
  if Assigned(FOnChangedSwitch) then
    FOnChangedSwitch(Self);
end;

procedure TfrmRecord.DoInsertItem(const Sender: TObject;
  const AData: THCCustomData; const AItem: THCCustomItem);
begin
  if AItem is TDeCombobox then
    (AItem as TDeCombobox).OnPopupItem := DoDeComboboxGetItem;

  if Assigned(FOnInsertDeItem) then
    FOnInsertDeItem(FEmrView, Sender as THCSection, AData, AItem);
end;

function TfrmRecord.DoInsertTextBefor(const AData: THCCustomData;
  const AItemNo, AOffset: Integer; const AText: string): Boolean;
var
  vItem: THCCustomItem;
  vDeItem: TDeItem;
begin
  Result := False;
  vItem := AData.Items[AItemNo];
  if vItem is TDeItem then
  begin
    vDeItem := vItem as TDeItem;
    if (vDeItem.IsElement and (not vDeItem.AllocValue) and vItem.IsSelectComplate) then  // ����Ԫû����ֵ��ȫѡ���ˣ��޵�����ʱ����Ϊȫѡ�С��ֶ�ȫѡ�У�
    begin
      FEmrView.UndoGroupBegin;
      try
        FEmrView.SetActiveItemText(AText);
        (AData as THCRichData).UndoItemMirror(AItemNo, AOffset);
        if vDeItem.Propertys.IndexOfName(TDeProp.CMVVCode) >= 0 then
          vDeItem[TDeProp.CMVVCode] := '';
      finally
        FEmrView.UndoGroupEnd;
      end;

      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

procedure TfrmRecord.DoEmrViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PopupFormClose;
  //FMouseDownTick := GetTickCount;
end;

procedure TfrmRecord.DoEmrViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vActiveItem: THCCustomItem;
  vDeItem: TDeItem;
  vActiveDrawItem: THCCustomDrawItem;
  vPt: TPoint;
  vDrawItemRect: TRect;

  vDeGroup: TDeGroup;
  vData: THCViewData;
  vDomain: THCDomainInfo;
begin
  vActiveItem := FEmrView.GetTopLevelItem;
  if vActiveItem is TDeItem then
  begin
    vDeItem := vActiveItem as TDeItem;
    if FEmrView.ActiveSection.ActiveData.ReadOnly or vDeItem.EditProtect then Exit;
    if vDeItem.StyleEx <> cseNone then

    else
    if vDeItem.Active
      and (vDeItem[TDeProp.Index] <> '')
      and (not vDeItem.IsSelectComplate)
      and (not vDeItem.IsSelectPart)
      and FMouseInElementFire  // (CalcTickCount(FMouseDownTick, GetTickCount) < 500)  // ����ѡ��Ի���
    then
    begin
      if (Shift = [ssCtrl]) and ActiveDeItemSync(vDeItem) then Exit;

      vPt := FEmrView.GetTopLevelDrawItemViewCoord;  // �õ����EmrView������
      vActiveDrawItem := FEmrView.GetTopLevelDrawItem;
      vDrawItemRect := Bounds(vPt.X, vPt.Y, vActiveDrawItem.Rect.Width, vActiveDrawItem.Rect.Height);

      if PtInRect(vDrawItemRect, Point(X, Y)) then
      begin
        vPt.Y := vPt.Y + FEmrView.ZoomIn(vActiveDrawItem.Height);
        vPt.Offset(FEmrView.Left, FEmrView.Top);
        vPt := ClientToScreen(vPt);

        if DoDeItemPopup(vDeItem) then
        begin
          if not PopupForm.PopupDeItem(vDeItem, vPt) then  // ���õ�������ֵʱ���ж��״�����ֱ���滻ԭ����
          begin
            vData := FEmrView.ActiveSectionTopLevelData as THCViewData;
            if vData.SelectExists then
                Exit;

            if not vDeItem.AllocValue then  // û�д����ֵ
            begin
              vData.SetSelectBound(vData.SelectInfo.StartItemNo, 0,
                vData.SelectInfo.StartItemNo, vData.GetItemOffsetAfter(vData.SelectInfo.StartItemNo), false);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmRecord.DoPaintPaperBefor(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
begin
  if (not APaintInfo.Print) and THCSection(Sender).ReadOnly then
  begin
    ACanvas.Font.Size := 48;
    ACanvas.Font.Color := clMedGray;
    ACanvas.Font.Name := '����';
    ACanvas.TextOut(ARect.Left + 10, ARect.Top + 10, 'ֻ��');
  end;
end;

function TfrmRecord.DoPasteRequest(const AFormat: Word): Boolean;
begin
  if AFormat = CF_BITMAP then
    Result := False
  else
    Result := True;
end;

procedure TfrmRecord.DoDeComboboxGetItem(Sender: TObject);
var
  vCombobox: TDeCombobox;
begin
  vCombobox := Sender as TDeCombobox;
  if not vCombobox.SaveItem then
  begin
    //if DoDeItemPopup(vCombobox) then
    PopupForm.PopupDeCombobox(vCombobox);
  end;
end;

function TfrmRecord.DoDeItemPopup(const ADeItem: TDeItem): Boolean;
begin
  if Assigned(FOnDeItemPopup) then
    Result := FOnDeItemPopup(ADeItem)
  else
    Result := True;
end;

procedure TfrmRecord.DoReadOnlySwitch(Sender: TObject);
begin
  if Assigned(FOnReadOnlySwitch) then
    FOnReadOnlySwitch(Self);
end;

procedure TfrmRecord.DoSave;
begin
  if Assigned(FOnSave) then
    FOnSave(Self);
end;

procedure TfrmRecord.DoSaveStructure;
begin
  if Assigned(FOnSaveStructure) then
    FOnSaveStructure(Self);
end;

procedure TfrmRecord.DoVerScroll(Sender: TObject);
begin
  PopupFormClose;
end;

procedure TfrmRecord.FormCreate(Sender: TObject);
begin
  cbbFont.Items := Screen.Fonts;
  cbbFont.ItemIndex := cbbFont.Items.IndexOf('����');

  FEmrView := THCEmrView.Create(Self);
  FEmrView.OnSectionItemInsert := DoInsertItem;
  FEmrView.OnMouseDown := DoEmrViewMouseDown;
  FEmrView.OnMouseUp := DoEmrViewMouseUp;
  FEmrView.OnCaretChange := DoCaretChange;
  FEmrView.OnVerScroll := DoVerScroll;
  FEmrView.OnChangedSwitch := DoChangedSwitch;
  FEmrView.OnSectionReadOnlySwitch := DoReadOnlySwitch;
  FEmrView.OnCanNotEdit := DoCanNotEdit;
  FEmrView.OnSectionPaintPaperBefor := DoPaintPaperBefor;
  FEmrView.OnSectionInsertTextBefor := DoInsertTextBefor;
  FEmrView.OnPasteRequest := DoPasteRequest;
  FEmrView.PopupMenu := pmView;
  FEmrView.Parent := Self;
  FEmrView.Align := alClient;
  {$IFDEF VIEWTOOL}
  FEmrView.OnTableToolPropertyClick := mniTablePropertyClick;
  {$ENDIF}
  FEmrView.OnSyntaxPaint := DoSyntaxPaint;
  //FEmrView.ShowHint := True; ��������Ԫ�ص����������ʾHint�Զ���ʧ
end;

procedure TfrmRecord.FormDestroy(Sender: TObject);
begin
  if Assigned(FfrmRecordPop) then
    FreeAndNil(FfrmRecordPop);

  if Assigned(FfrmDataElement) then
    FreeAndNil(FfrmDataElement);

  FEmrView.Free;
end;

procedure TfrmRecord.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = Ord('S')) then
    actSaveExecute(Sender);
end;

procedure TfrmRecord.FormShow(Sender: TObject);
begin
  cbbFont.Items := Screen.Fonts;
  cbbFont.ItemIndex := cbbFont.Items.IndexOf('����');
  SendMessage(cbbFont.Handle, CB_SETDROPPEDWIDTH, 200, 0);
  PopupForm;  // �������ǰ��ʼ�����״ε����ѡ��ȫѡ��ֱ������ʱ������Ϊ������ʧȥ����
  FEmrView.SetFocus;
end;

function TfrmRecord.GetEditToolVisible: Boolean;
begin
  Result := tlbTool.Visible;
end;

function TfrmRecord.GetHideTrace: Boolean;
begin
  Result := FEmrView.HideTrace;
end;

function TfrmRecord.GetOnCopyAsStream: THCCopyPasteStreamEvent;
begin
  Result := FEmrView.OnCopyAsStream;
end;

function TfrmRecord.GetOnCopyRequest: THCCopyPasteEvent;
begin
  Result := FEmrView.OnCopyRequest;
end;

function TfrmRecord.GetOnPasteFromStream: THCCopyPasteStreamEvent;
begin
  Result := FEmrView.OnPasteFromStream;
end;

function TfrmRecord.GetOnPasteRequest: THCCopyPasteEvent;
begin
  Result := FEmrView.OnPasteRequest;
end;

function TfrmRecord.GetOnSyntaxCheck: TDataDomainItemNoEvent;
begin
  Result := FEmrView.OnSyntaxCheck;
end;

function TfrmRecord.GetOnSyntaxPaint: TSyntaxPaintEvent;
begin
  Result := FEmrView.OnSyntaxPaint;
end;

function TfrmRecord.GetPrintToolVisible: Boolean;
begin
  Result := pnlPrint.Visible;
end;

procedure TfrmRecord.HideToolbar;
begin
  tlbTool.Visible := False;
end;

function TfrmRecord.InsertDeCheckBox(const AIndex, AName: string): TDeCheckBox;
begin
  Result := nil;
  if (AIndex = '') or (AName = '') then
  begin
    ShowMessage('Ҫ�����CheckBox���������Ʋ���Ϊ�գ�');
    Exit;
  end;

  Result := TDeCheckBox.Create(FEmrView.ActiveSectionTopLevelData, AName, False);
  Result[TDeProp.Index] := AIndex;
  Result[TDeProp.Name] := AName;
  FEmrView.InsertItem(Result);
end;

function TfrmRecord.InsertDeCombobox(const AIndex, AName: string): TDeCombobox;
begin
  Result := nil;
  if (AIndex = '') or (AName = '') then
  begin
    ShowMessage('Ҫ�����Combobox���������Ʋ���Ϊ�գ�');
    Exit;
  end;

  Result := TDeCombobox.Create(FEmrView.ActiveSectionTopLevelData, AName);
  Result.SaveItem := False;
  Result[TDeProp.Index] := AIndex;
  Result[TDeProp.Name] := AName;
  FEmrView.InsertItem(Result);
end;

function TfrmRecord.InsertDeDateTime(const AIndex, AName: string): TDeDateTimePicker;
begin
  Result := nil;
  if (AIndex = '') or (AName = '') then
  begin
    ShowMessage('Ҫ�����DateTiem���������Ʋ���Ϊ�գ�');
    Exit;
  end;

  Result := TDeDateTimePicker.Create(FEmrView.ActiveSectionTopLevelData, Now);
  Result[TDeProp.Index] := AIndex;
  Result[TDeProp.Name] := AName;
  FEmrView.InsertItem(Result);
end;

function TfrmRecord.InsertDeEdit(const AIndex, AName: string): TDeEdit;
begin
  Result := nil;
  if (AIndex = '') or (AName = '') then
  begin
    ShowMessage('Ҫ�����Edit���������Ʋ���Ϊ�գ�');
    Exit;
  end;

  Result := TDeEdit.Create(FEmrView.ActiveSectionTopLevelData, AName);
  Result[TDeProp.Index] := AIndex;
  Result[TDeProp.Name] := AName;
  FEmrView.InsertItem(Result);
end;

function TfrmRecord.InsertDeFloatBarCode(const AIndex,
  AName: string): TDeFloatBarCodeItem;
begin
  Result := nil;
  if AIndex = '' then
  begin
    ShowMessage('Ҫ�����FloatBarCode��������Ϊ�գ�');
    Exit;
  end;

  Result := TDeFloatBarCodeItem.Create(FEmrView.ActiveSection.ActiveData);
  Result[TDeProp.Index] := AIndex;
  FEmrView.InsertFloatItem(Result);
end;

procedure TfrmRecord.InsertDeGroup(const AIndex, AName: string);
var
  vDeGroup: TDeGroup;
begin
  if (AIndex = '') or (AName = '') then
  begin
    ShowMessage('Ҫ��������������������Ʋ���Ϊ�գ�');
    Exit;
  end;

  vDeGroup := TDeGroup.Create(FEmrView.ActiveSectionTopLevelData);
  try
    vDeGroup[TDeProp.Index] := AIndex;
    vDeGroup[TDeProp.Name] := AName;

    FEmrView.InsertDeGroup(vDeGroup);
  finally
    vDeGroup.Free;
  end;
end;

function TfrmRecord.InsertDeImage(const AIndex, AName: string): TDeImageItem;
begin
  Result := nil;
  if AIndex = '' then
  begin
    ShowMessage('Ҫ�����DeImage��������Ϊ�գ�');
    Exit;
  end;

  Result := TDeImageItem.Create(FEmrView.ActiveSection.ActiveData);
  Result[TDeProp.Index] := AIndex;
  FEmrView.InsertItem(Result);
end;

function TfrmRecord.InsertDeItem(const AIndex, AName: string): TDeItem;
begin
  Result := nil;
  if (AIndex = '') or (AName = '') then
  begin
    ShowMessage('Ҫ���������Ԫ���������Ʋ���Ϊ�գ�');
    Exit;
  end;

  Result := FEmrView.NewDeItem(AName);
  Result[TDeProp.Index] := AIndex;
  Result[TDeProp.Name] := AName;
  FEmrView.InsertDeItem(Result);
end;

function TfrmRecord.InsertDeRadioGroup(const AIndex, AName: string): TDeRadioGroup;
begin
  Result := nil;
  if (AIndex = '') or (AName = '') then
  begin
    ShowMessage('Ҫ�����RadioGroup���������Ʋ���Ϊ�գ�');
    Exit;
  end;

  Result := TDeRadioGroup.Create(FEmrView.ActiveSectionTopLevelData);
  Result[TDeProp.Index] := AIndex;
  Result[TDeProp.Name] := AName;
  // ȡ����Ԫ��ѡ�ѡ��̫��ʱ��ʾ�Ƿ񶼲���
  Result.AddItem('ѡ��1');
  Result.AddItem('ѡ��2');
  Result.AddItem('ѡ��3');

  FEmrView.InsertItem(Result);
end;

procedure TfrmRecord.mniOpenClick(Sender: TObject);
var
  vOpenDlg: TOpenDialog;
  vExt: string;
begin
  if FEmrView.ReadOnly then
  begin
    ShowMessage('��ǰ�ĵ�ֻ����');
    Exit;
  end;

  vOpenDlg := TOpenDialog.Create(Self);
  try
    vOpenDlg.Filter := '֧�ֵ��ļ�|*' + HC_EXT + '; *.xml|HCView (*.hcf)|*' + HC_EXT + '|HCView xml (*.xml)|*.xml';
    if vOpenDlg.Execute then
    begin
      if vOpenDlg.FileName <> '' then
      begin
        Application.ProcessMessages;
        vExt := LowerCase(ExtractFileExt(vOpenDlg.FileName)); // ��׺
        if vExt = HC_EXT then
          FEmrView.LoadFromFile(vOpenDlg.FileName)
        else
        if vExt = '.xml' then
          FEmrView.LoadFromXml(vOpenDlg.FileName);
      end;
    end;
  finally
    FreeAndNil(vOpenDlg);
  end;
end;

procedure TfrmRecord.mniSaveAsClick(Sender: TObject);
var
  vSaveDlg: TSaveDialog;
  vExt: string;
begin
  vSaveDlg := TSaveDialog.Create(nil);
  try
    vSaveDlg.Filter := '�ļ�|*' + HC_EXT
      + '|HCView xml|*.xml|Word 2007 Document (*.docx)|*.docx|pdf�ļ�|*.pdf|htmlҳ��|*.html';

    if vSaveDlg.Execute then
    begin
      if vSaveDlg.FileName <> '' then
      begin
        vExt := '';
        case vSaveDlg.FilterIndex of
          1: vExt := HC_EXT;
          2: vExt := '.xml';
          3: vExt := '.docx';
          4: vExt := '.pdf';
          5: vExt := '.html';
        else
          Exit;
        end;

        if ExtractFileExt(vSaveDlg.FileName) <> vExt then  // �����ظ���׺
          vSaveDlg.FileName := vSaveDlg.FileName + vExt;

        case vSaveDlg.FilterIndex of
          1: FEmrView.SaveToFile(vSaveDlg.FileName);  // .hcf
          2: FEmrView.SaveToXML(vSaveDlg.FileName, TEncoding.UTF8);  // xml
          3: FEmrView.SaveToDocumentFile(vSaveDlg.FileName, vExt); // docx,rtf
          4: FEmrView.SaveToPDF(vSaveDlg.FileName);  // pdf
          5: FEmrView.SaveToHtml(vSaveDlg.FileName);  // html
        end;
      end;
    end;
  finally
    FreeAndNil(vSaveDlg);
  end;
end;

procedure TfrmRecord.mniSplitColClick(Sender: TObject);
begin
  FEmrView.ActiveTableSplitCurCol;
end;

procedure TfrmRecord.mniSplitRowClick(Sender: TObject);
begin
  FEmrView.ActiveTableSplitCurRow;
end;

procedure TfrmRecord.mniTablePropertyClick(Sender: TObject);
var
  vFrmDeTableProperty: TFrmDeTableProperty;
begin
  vFrmDeTableProperty := TFrmDeTableProperty.Create(Self);
  try
    vFrmDeTableProperty.SetHCView(FEmrView);
  finally
    FreeAndNil(vFrmDeTableProperty);
  end;
end;

procedure TfrmRecord.mniHideTraceClick(Sender: TObject);
begin
  SetHideTrace(not FEmrView.HideTrace);
end;

procedure TfrmRecord.mniSectionClick(Sender: TObject);
begin
  FEmrView.InsertSectionBreak;
end;

procedure TfrmRecord.mniFloatBarCodeClick(Sender: TObject);
var
  vFloatBarCodeItem: TDeFloatBarCodeItem;
begin
  vFloatBarCodeItem := TDeFloatBarCodeItem.Create(FEmrView.ActiveSection.ActiveData);
  FEmrView.InsertFloatItem(vFloatBarCodeItem);
end;

procedure TfrmRecord.mniFloatItemPropertyClick(Sender: TObject);
var
  vActiveFloatItem: THCCustomFloatItem;
  vActiveData: THCCustomData;
  vFrmFloatBarCode: TfrmDeFloatBarCode;
  vfrmFloatItemProperty: TfrmDeFloatItemProperty;
begin
  vActiveData := FEmrView.ActiveSection.ActiveData;
  vActiveFloatItem := (vActiveData as THCSectionData).GetActiveFloatItem;
  if vActiveFloatItem is TDeFloatBarCodeItem then
  begin
    vFrmFloatBarCode := TfrmDeFloatBarCode.Create(nil);
    try
      vFrmFloatBarCode.SetHCView(FEmrView, vActiveFloatItem as TDeFloatBarCodeItem);
    finally
      FreeAndNil(vFrmFloatBarCode);
    end;
  end
  else
  begin
    vfrmFloatItemProperty := TfrmDeFloatItemProperty.Create(nil);
    try
      vfrmFloatItemProperty.SetHCView(FEmrView);
    finally
      FreeAndNil(vfrmFloatItemProperty);
    end;
  end;
end;

procedure TfrmRecord.mniFloatLineClick(Sender: TObject);
var
  vFloatLineItem: THCFloatLineItem;
begin
  vFloatLineItem := THCFloatLineItem.Create(FEmrView.ActiveSection.ActiveData);
  FEmrView.InsertFloatItem(vFloatLineItem);
end;

procedure TfrmRecord.pmFilePopup(Sender: TObject);
begin
  mniHideTrace.Visible := FEmrView.TraceCount > 0;

  if mniHideTrace.Visible then
  begin
    if FEmrView.HideTrace then
      mniHideTrace.Caption := '��ʾ�ۼ�'
    else
      mniHideTrace.Caption := '����ʾ�ۼ�';
  end;

  {$IFDEF VIEWINPUTHELP}
  if FEmrView.InputHelpEnable then
    mniInputHelp.Caption := '�رո�������'
  else
    mniInputHelp.Caption := '������������';
  {$ELSE}
  mniInputHelp.Visible := False;
  {$ENDIF}
end;

procedure TfrmRecord.pmViewPopup(Sender: TObject);
var
  vActiveItem, vTopItem: THCCustomItem;
  vActiveFloatItem: THCCustomFloatItem;
  vTable: TDeTable;
  vActiveData, vTopData: THCCustomData;
  vReadOnly: Boolean;
  i: Integer;
begin
  vActiveData := FEmrView.ActiveSection.ActiveData;
  vActiveFloatItem := (vActiveData as THCSectionData).GetActiveFloatItem;
  vReadOnly := (vActiveData as THCRichData).ReadOnly;

  if Assigned(vActiveFloatItem) then
  begin
    for i := 0 to pmView.Items.Count - 1 do
      pmView.Items[i].Visible := False;

    if not vReadOnly then
    begin
      mniFloatItemProperty.Visible := FEmrView.DesignMode;
      if vActiveFloatItem is TDeFloatBarCodeItem then
        mniFloatItemProperty.Caption := '��������';
    end;

    Exit;
  end
  else
    mniFloatItemProperty.Visible := False;

  vActiveItem := vActiveData.GetActiveItem;

  vTopData := nil;
  vTopItem := vActiveItem;

  while vTopItem is THCCustomRectItem do
  begin
    if (vTopItem as THCCustomRectItem).GetActiveData <> nil then
    begin
      if vTopData <> nil then
      begin
        vActiveData := vTopData;
        vActiveItem := vTopItem;
      end;

      vTopData := (vTopItem as THCCustomRectItem).GetActiveData;
      vTopItem := vTopData.GetActiveItem;
      if (vTopData as THCRichData).ReadOnly then
        vReadOnly := True;
    end
    else
      Break;
  end;

  if vTopData = nil then
    vTopData := vActiveData;

  if vReadOnly then
  begin
    for i := 0 to pmView.Items.Count - 1 do
      pmView.Items[i].Visible := False;

    mniCopy.Visible := vTopData.SelectExists;
    Exit;
  end;

  mniTable.Visible := vActiveItem.StyleNo = THCStyle.Table;
  if mniTable.Visible then
  begin
    vTable := vActiveItem as TDeTable;
    mniInsertRowTop.Enabled := vTable.GetEditCell <> nil;
    mniInsertRowBottom.Enabled := mniInsertRowTop.Enabled;
    mniInsertColLeft.Enabled := mniInsertRowTop.Enabled;
    mniInsertColRight.Enabled := mniInsertRowTop.Enabled;
    mniSplitRow.Enabled := mniInsertRowTop.Enabled;
    mniSplitCol.Enabled := mniInsertRowTop.Enabled;

    mniDeleteCurRow.Enabled := vTable.CurRowCanDelete;
    mniDeleteCurCol.Enabled := vTable.CurColCanDelete;
    mniMerge.Enabled := vTable.SelectedCellCanMerge;
  end;

  mniPara.Enabled := not vReadOnly;
  actCopy.Enabled := vTopData.SelectExists;
  actCut.Enabled := actCopy.Enabled and (not FEmrView.ActiveSection.ReadOnly);
  actPaste.Enabled := (not vReadOnly)  // ��ֻ��
    and (Clipboard.HasFormat(HC_FILEFORMAT)
         or Clipboard.HasFormat(CF_TEXT)
         or Clipboard.HasFormat(CF_BITMAP));

  mniControlItem.Visible := False;
  mniDeItem.Visible := False;
  mniDeleteProtect.Visible := False;
  mniCopyProtect.Visible := False;
  mniDeGroup.Visible := False;
  mniSplit.Visible := False;

  if vReadOnly then Exit;

  if vTopItem is TDeImageItem then
  begin
    mniControlItem.Visible := (not (vTopData as THCRichData).ReadOnly) and vTopItem.Active;
    mniControlItem.Caption := '����(' + vTopItem.ClassName + ')';
  end
  else
  begin
    mniControlItem.Visible := (not (vTopData as THCRichData).ReadOnly) and (not vTopData.SelectExists)
      and (vTopItem is THCControlItem) and vTopItem.Active;
    if mniControlItem.Visible then
      mniControlItem.Caption := '����(' + (vTopItem as THCControlItem).ClassName + ')';
  end;

  if vTopItem is TDeItem then
  begin
    if (vTopItem as TDeItem).IsElement then
    begin
      mniDeItem.Visible := True;
      mniDeItem.Caption := (vTopItem as TDeItem)[TDeProp.Name];
    end;

    if FEmrView.DesignMode then  // ���ģʽ
    begin
      if vTopData.SelectExists then
      begin
        mniDeleteProtect.Caption := '����ʱ��ֹ�༭';
        mniDeleteProtect.Visible := True;

        mniCopyProtect.Caption := '����ʱ��ֹ����';
        mniCopyProtect.Visible := True;
      end
      else
      begin
        if (vTopItem as TDeItem).EditProtect then
          mniDeleteProtect.Caption := '����ʱ����༭'
        else
          mniDeleteProtect.Caption := '����ʱ��ֹ�༭';

        mniDeleteProtect.Visible := True;


        if (vTopItem as TDeItem).CopyProtect then
          mniCopyProtect.Caption := '����ʱ������'
        else
          mniCopyProtect.Caption := '����ʱ��ֹ����';

        mniCopyProtect.Visible := True;
      end;
    end;
  end;

  if (vTopData as THCViewData).ActiveDomain.BeginNo >= 0 then
  begin
    mniDeGroup.Visible := True;
    mniDeGroup.Caption := (vTopData.Items[(vTopData as THCViewData).ActiveDomain.BeginNo] as TDeGroup)[TDeProp.Name];
  end;

  mniSplit.Visible := mniControlItem.Visible or mniDeItem.Visible or mniDeGroup.Visible;  // �˵��ָ���
end;

function TfrmRecord.PopupForm: TfrmRecordPop;
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

procedure TfrmRecord.PopupFormClose;
begin
  if Assigned(FfrmRecordPop) and FfrmRecordPop.Visible then  // ʹ��PopupForm�ᵼ��û��FfrmRecordPopʱ����һ���ٹرգ�������
    FfrmRecordPop.Close;
end;

procedure TfrmRecord.SaveToImage(const APath, APrefix, AImageType: string);
begin
  //if FEmrView.TraceCount > 0 then
  //  Self.HideTrace := True;  // ���غۼ�
  FEmrView.SaveToImage(APath, APrefix, AImageType);
end;

procedure TfrmRecord.SetEditToolVisible(const Value: Boolean);
begin
  tlbTool.Visible := Value;
end;

procedure TfrmRecord.SetHideTrace(const Value: Boolean);
begin
  if FEmrView.HideTrace <> Value then
  begin
    FEmrView.HideTrace := Value;

    if Value then  // ���غۼ�
    begin
      //FEmrView.AnnotatePre.Visible := False;
      TraverseElement(DoHideTraceTraverse, [saPage], TTravTag.HideTrace);
    end
    else  // ��ʾ�ۼ�
    begin
      //if (FEmrView.TraceCount > 0) and (not FEmrView.AnnotatePre.Visible) then
      //  FEmrView.AnnotatePre.Visible := True;

      TraverseElement(DoHideTraceTraverse, [saPage], 0);
    end;

    if Value and (not FEmrView.ReadOnly) then
      FEmrView.ReadOnly := True;
  end;
end;

procedure TfrmRecord.SetOnCopyAsStream(const Value: THCCopyPasteStreamEvent);
begin
  FEmrView.OnCopyAsStream := Value;
end;

procedure TfrmRecord.SetOnCopyRequest(const Value: THCCopyPasteEvent);
begin
  FEmrView.OnCopyRequest := Value;
end;

procedure TfrmRecord.SetOnPasteFromStream(const Value: THCCopyPasteStreamEvent);
begin
  FEmrView.OnPasteFromStream := Value;
end;

procedure TfrmRecord.SetOnPasteRequest(const Value: THCCopyPasteEvent);
begin
  FEmrView.OnPasteRequest := Value;
end;

procedure TfrmRecord.SetOnSyntaxCheck(const Value: TDataDomainItemNoEvent);
begin
  FEmrView.OnSyntaxCheck := Value;
end;

procedure TfrmRecord.SetOnSyntaxPaint(const Value: TSyntaxPaintEvent);
begin
  FEmrView.OnSyntaxPaint := Value;
end;

procedure TfrmRecord.SetPrintToolVisible(const Value: Boolean);
begin
  if Value then
  begin
    cbbPrinter.Items := HCPrinter.Printers;
    cbbPrinter.ItemIndex := HCPrinter.PrinterIndex;
  end;

  pnlPrint.Visible := Value;
end;

procedure TfrmRecord.TraverseElement(const ATravEvent: TTraverseItemEvent;
   const AAreas: TSectionAreas = [saHeader, saPage, saFooter]; const ATag: Integer = 0);
var
  vItemTraverse: THCItemTraverse;
begin
  if not Assigned(ATravEvent) then Exit;

  if AAreas = [] then Exit;

  vItemTraverse := THCItemTraverse.Create;
  try
    vItemTraverse.Tag := ATag;
    vItemTraverse.Areas := [saHeader, saPage, saFooter];
    vItemTraverse.Process := ATravEvent;
    FEmrView.TraverseItem(vItemTraverse);

    FEmrView.FormatData;
  finally
    vItemTraverse.Free;
  end;
end;

procedure TfrmRecord.mniComboboxClick(Sender: TObject);
var
  vDeCombobox: TDeCombobox;
  vS: string;
begin
  vS := 'Ĭ��ֵ';
  if InputQuery('������', '�ı�����', vS) then
  begin
    vDeCombobox := TDeCombobox.Create(FEmrView.ActiveSectionTopLevelData, vS);
    vDeCombobox.SaveItem := False;  // ����ѡ��洢���ĵ���
    vDeCombobox[TDeProp.Index] := '1002';  // �ؼ�������Ԫ����
    {vCombobox.Items.Add('ѡ��1');
    vCombobox.Items.Add('ѡ��2');
    vCombobox.Items.Add('ѡ��3');}
    //vCombobox.ItemIndex := 0;
    FEmrView.InsertItem(vDeCombobox);
  end;
end;

procedure TfrmRecord.mniControlItemClick(Sender: TObject);
var
  vControlItem: THCCustomItem;
  vFrmDeCombobox: TfrmDeCombobox;
  vFrmDeEdit: TfrmDeEdit;
  vFrmDeRadioGroup: TfrmDeRadioGroup;
  vFrmDeCheckBox: TfrmDeCheckBox;
  vFrmDeControlProperty: TfrmDeControlProperty;
  vFrmDeDateTime: TFrmDeDateTime;
  vFrmDeImage: TfrmDeImage;
begin
  vControlItem := FEmrView.ActiveSectionTopLevelData.GetActiveItem;
  if vControlItem is TDeCombobox then  // ComboboxItem
  begin
    vFrmDeCombobox := TfrmDeCombobox.Create(nil);
    try
      vFrmDeCombobox.SetHCView(FEmrView, vControlItem as TDeCombobox);
    finally
      FreeAndNil(vFrmDeCombobox);
    end;
  end
  else
  if vControlItem is TDeEdit then
  begin
    vFrmDeEdit := TfrmDeEdit.Create(nil);
    try
      vFrmDeEdit.SetHCView(FEmrView, vControlItem as TDeEdit);
    finally
      FreeAndNil(vFrmDeEdit);
    end;
  end
  else
  if vControlItem is TDeRadioGroup then
  begin
    vFrmDeRadioGroup := TfrmDeRadioGroup.Create(nil);
    try
      vFrmDeRadioGroup.SetHCView(FEmrView, vControlItem as TDeRadioGroup);
    finally
      FreeAndNil(vFrmDeRadioGroup);
    end;
  end
  else
  if vControlItem is TDeCheckBox then
  begin
    vFrmDeCheckBox := TfrmDeCheckBox.Create(nil);
    try
      vFrmDeCheckBox.SetHCView(FEmrView, vControlItem as TDeCheckBox);
    finally
      FreeAndNil(vFrmDeCheckBox);
    end;
  end
  else
  if vControlItem is TDeDateTimePicker then
  begin
    vFrmDeDateTime := TFrmDeDateTime.Create(nil);
    try
      vFrmDeDateTime.SetHCView(FEmrView, vControlItem as TDeDateTimePicker);
    finally
      FreeAndNil(vFrmDeDateTime);
    end;
  end
  else
  if vControlItem is TDeImageItem then
  begin
    vFrmDeImage := TfrmDeImage.Create(nil);
    try
      vFrmDeImage.SetHCView(FEmrView, vControlItem as TDeImageItem);
    finally
      FreeAndNil(vFrmDeImage);
    end;
  end
  else
  begin
    vFrmDeControlProperty := TfrmDeControlProperty.Create(nil);
    try
      vFrmDeControlProperty.SetHCView(FEmrView);
    finally
      FreeAndNil(vFrmDeControlProperty);
    end;
  end;
end;

procedure TfrmRecord.mniCopyProtectClick(Sender: TObject);
var
  vTopData: THCCustomData;
  vDeItem: TDeItem;
  vS: string;
  i: Integer;
begin
  vTopData := FEmrView.ActiveSectionTopLevelData;

  if vTopData.SelectExists then
  begin
    for i := vTopData.SelectInfo.StartItemNo to vTopData.SelectInfo.EndItemNo do
    begin
      if vTopData.Items[i].StyleNo < THCStyle.Null then
      begin
        ShowMessage('��ֹ�༭ֻ��Ӧ�����ı����ݣ�ѡ�������д��ڷ��ı�����');
        Exit;
      end;
    end;

    if (vTopData.SelectInfo.StartItemNo = vTopData.SelectInfo.EndItemNo)
      and (vTopData.SelectInfo.StartItemOffset = 0)
      and (vTopData.SelectInfo.EndItemOffset = vTopData.GetItemOffsetAfter(vTopData.SelectInfo.StartItemNo))
    then  // ��ͬһ��Item
    begin
      (vTopData.Items[vTopData.SelectInfo.StartItemNo] as TDeItem).CopyProtect := True;
      Exit;
    end;

    for i := vTopData.SelectInfo.StartItemNo to vTopData.SelectInfo.EndItemNo do
      (vTopData.Items[i] as TDeItem).EditProtect := False;

    vS := vTopData.GetSelectText;
    vS := StringReplace(vS, #13#10, '', [rfReplaceAll, rfIgnoreCase]);
    vDeItem := FEmrView.NewDeItem(vS);
    vDeItem.CopyProtect := True;
    FEmrView.InsertDeItem(vDeItem);
  end
  else
  begin
    vDeItem := vTopData.GetActiveItem as TDeItem;
    vDeItem.CopyProtect := not vDeItem.CopyProtect;
    FEmrView.ActiveItemReAdaptEnvironment;
  end;
end;

procedure TfrmRecord.mniDeleteCurColClick(Sender: TObject);
begin
  FEmrView.ActiveTableDeleteCurCol;
end;

procedure TfrmRecord.mniDeleteGroupClick(Sender: TObject);
begin
  FEmrView.DeleteActiveDomain;
end;

procedure TfrmRecord.mniDeleteCurRowClick(Sender: TObject);
begin
  FEmrView.ActiveTableDeleteCurRow;
end;

procedure TfrmRecord.mniDisBorderClick(Sender: TObject);
var
  vTable: TDeTable;
begin
  if FEmrView.ActiveSection.ActiveData.GetActiveItem is TDeTable then
  begin
    vTable := FEmrView.ActiveSection.ActiveData.GetActiveItem as TDeTable;
    vTable.BorderVisible := not vTable.BorderVisible;
    FEmrView.UpdateView;
  end;
end;

procedure TfrmRecord.mniInsertGifClick(Sender: TObject);
var
  vOpenDlg: TOpenDialog;
  vGifItem: THCGifItem;
begin
  vOpenDlg := TOpenDialog.Create(Self);
  try
    vOpenDlg.Filter := 'GIF�����ļ�|*.gif';
    FEmrView.Enabled := False;  // ���˫����ͼƬʱ���¶�λ��������
    try
      if vOpenDlg.Execute then
      begin
        if vOpenDlg.FileName <> '' then
        begin
          vGifItem := THCGifItem.Create(FEmrView.ActiveSectionTopLevelData);
          vGifItem.LoadFromFile(vOpenDlg.FileName);
          Application.ProcessMessages;  // ���˫�����ļ��󣬴����²�ؼ���Mousemove��Mouseup�¼�
          FEmrView.InsertItem(vGifItem);
        end;
      end;
    finally
      FEmrView.Enabled := True;
    end;
  finally
    FreeAndNil(vOpenDlg);
  end;
end;

procedure TfrmRecord.mniInputHelpClick(Sender: TObject);
begin
  {$IFDEF VIEWINPUTHELP}
  FEmrView.InputHelpEnable := not FEmrView.InputHelpEnable;
  {$ENDIF}
end;

procedure TfrmRecord.mniInsertColLeftClick(Sender: TObject);
begin
  FEmrView.ActiveTableInsertColBefor(1);
end;

procedure TfrmRecord.mniInsertColRightClick(Sender: TObject);
begin
  FEmrView.ActiveTableInsertColAfter(1);
end;

procedure TfrmRecord.mniInsertRowBottomClick(Sender: TObject);
begin
  FEmrView.ActiveTableInsertRowAfter(1);
end;

procedure TfrmRecord.mniInsertRowTopClick(Sender: TObject);
begin
  FEmrView.ActiveTableInsertRowBefor(1);
end;

procedure TfrmRecord.mniLineSpace100Click(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    case (Sender as TMenuItem).Tag of
      0: FEmrView.ApplyParaLineSpace(TParaLineSpaceMode.pls100);
      1: FEmrView.ApplyParaLineSpace(TParaLineSpaceMode.pls115);
      2: FEmrView.ApplyParaLineSpace(TParaLineSpaceMode.pls150);
      3: FEmrView.ApplyParaLineSpace(TParaLineSpaceMode.pls200);
    end;
  end;
end;

procedure TfrmRecord.mniPageBreakClick(Sender: TObject);
begin
  FEmrView.InsertPageBreak;
end;

procedure TfrmRecord.mniDateTimeClick(Sender: TObject);
var
  vDateTimePicker: TDeDateTimePicker;
begin
  vDateTimePicker := TDeDateTimePicker.Create(FEmrView.ActiveSectionTopLevelData, Now);
  FEmrView.InsertItem(vDateTimePicker);
end;

procedure TfrmRecord.mniDeItemPropClick(Sender: TObject);
var
  vFrmDeProperty: TfrmDeProperty;
begin
  vFrmDeProperty := TfrmDeProperty.Create(nil);
  try
    vFrmDeProperty.SetHCView(FEmrView);
  finally
    FreeAndNil(vFrmDeProperty);
  end;
end;

procedure TfrmRecord.mniDeleteDeItemClick(Sender: TObject);
var
  vTopData: THCRichData;
begin
  vTopData := FEmrView.ActiveSectionTopLevelData as THCRichData;
  FEmrView.DeleteActiveDataItems(vTopData.SelectInfo.StartItemNo);
end;

procedure TfrmRecord.mniToothClick(Sender: TObject);
var
  vToothItem: TEmrToothItem;
begin
  vToothItem := TEmrToothItem.Create(FEmrView.ActiveSectionTopLevelData,
    'XX', 'XX', 'XX', 'XX');
  FEmrView.InsertItem(vToothItem);
end;

procedure TfrmRecord.mniViewTextClick(Sender: TObject);
begin
  FEmrView.ViewModel := hvmEdit;
end;

procedure TfrmRecord.mniViewFilmClick(Sender: TObject);
begin
  FEmrView.ViewModel := THCViewModel.hvmFilm;
  //FHRuler.Visible := True;
  //FVRuler.Visible := True;
end;

procedure TfrmRecord.mniViewPageClick(Sender: TObject);
begin
  FEmrView.ViewModel := THCViewModel.hvmPage;
  //FHRuler.Visible := False;
  //FVRuler.Visible := False;
end;

procedure TfrmRecord.mniPrintCurLineClick(Sender: TObject);
begin
  FEmrView.PrintCurPageByActiveLine(cbbPrinter.Text, False, False);
end;

procedure TfrmRecord.mniPrintEvenClick(Sender: TObject);
begin
  FEmrView.PrintEven(cbbPrinter.Text);
end;

procedure TfrmRecord.mniPrintOddClick(Sender: TObject);
begin
  FEmrView.PrintOdd(cbbPrinter.Text);
end;

procedure TfrmRecord.mniEditItemClick(Sender: TObject);
var
  vDeEdit: TDeEdit;
  vS: string;
begin
  vS := InputBox('�ı���', '�ı�', '');
  vDeEdit := TDeEdit.Create(FEmrView.ActiveSectionTopLevelData, vS);
  FEmrView.InsertItem(vDeEdit);
end;

procedure TfrmRecord.mniRadioGroupClick(Sender: TObject);
var
  vRadioGroup: TDeRadioGroup;
begin
  vRadioGroup := TDeRadioGroup.Create(FEmrView.ActiveSectionTopLevelData);
  vRadioGroup.AddItem('ѡ��1');
  vRadioGroup.AddItem('ѡ��2');
  vRadioGroup.AddItem('ѡ��3');
  FEmrView.InsertItem(vRadioGroup);
end;

procedure TfrmRecord.mniReSyncDeGroupClick(Sender: TObject);
var
  vTopData, vPage: THCViewData;
  vDomain: THCDomainInfo;
  vText: string;
begin
  vTopData := FEmrView.ActiveSectionTopLevelData as THCViewData;
  vDomain := vTopData.ActiveDomain;
  vPage := FEmrView.ActiveSection.Page;

  if vTopData = vPage then
    vText := FEmrView.GetDataForwardDeGroupText(vPage, vDomain.BeginNo)
  else  {to do: ȡ����е�������}
    vText := '';

  if vText <> '' then
  begin
    FEmrView.BeginUpdate;
    try
      FEmrView.SetDataDeGroupText(vTopData, vDomain.BeginNo, vText);
      FEmrView.FormatSection(FEmrView.ActiveSectionIndex);
    finally
      FEmrView.EndUpdate;
    end;
  end;
end;

procedure TfrmRecord.mniReSyncDeItemClick(Sender: TObject);
var
  vData: THCCustomData;
  vDeItem: TDeItem;
  vValue: string;
  vCancel: Boolean;
begin
  if Assigned(FOnDeItemGetSyncValue) then
  begin
    vData := FEmrView.ActiveSectionTopLevelData;
    vDeItem := vData.GetActiveItem as TDeItem;  // �豣֤�������DeItem
    vValue := FOnDeItemGetSyncValue(TRecordInfo(Self.ObjectData).DesID, vDeItem);  // ȡ����Ԫ��ͬ��ֵ
    if vValue <> '' then
    begin
      vCancel := False;
      Self.DoSetActiveDeItemText(vDeItem, vValue, vCancel);
    end;
  end;
end;

procedure TfrmRecord.mniPrintSelectClick(Sender: TObject);
begin
  FEmrView.PrintCurPageSelected(cbbPrinter.Text, False, False);
end;

procedure TfrmRecord.mniMergeClick(Sender: TObject);
begin
  FEmrView.MergeTableSelectCells;
end;

procedure TfrmRecord.mniInsertDeItemClick(Sender: TObject);
begin
  if not Assigned(FfrmDataElement) then
  begin
    FfrmDataElement := TfrmDataElement.Create(Self);
    //FfrmDataElement.OnInsertAsDE := InsertDeItem;
  end;

  FfrmDataElement.Show;
end;

procedure TfrmRecord.mniClearClick(Sender: TObject);
begin
  FEmrView.Clear;
end;

procedure TfrmRecord.mniFangJiaoClick(Sender: TObject);
var
  vFangJiaoItem: TEmrFangJiaoItem;
begin
  vFangJiaoItem := TEmrFangJiaoItem.Create(FEmrView.ActiveSectionTopLevelData, '', '', '', '');
  FEmrView.InsertItem(vFangJiaoItem);
end;

procedure TfrmRecord.mniSupSubClick(Sender: TObject);
var
  vSupSubScriptItem: THCSupSubScriptItem;
begin
  vSupSubScriptItem := THCSupSubScriptItem.Create(FEmrView.ActiveSectionTopLevelData, '20g', '�ȼ�');
  FEmrView.InsertItem(vSupSubScriptItem);
end;

procedure TfrmRecord.mniSymbolClick(Sender: TObject);
var
  vAppPath: string;
begin
  vAppPath := ExtractFilePath(ParamStr(0));
  if FileExists(vAppPath + '\Symbol.txt') then
  begin
    if not Assigned(FfrmSymbol) then
    begin
      FfrmSymbol := TfrmSymbol.Create(nil);
      FfrmSymbol.HCView := FEmrView;
      FfrmSymbol.AppPath := vAppPath;
    end;

    FfrmSymbol.Show;
  end
  else
    ShowMessage('ʧ�ܣ�û�з��ַ����ļ���' + vAppPath + '\Symbols.txt��');
end;

procedure TfrmRecord.mniSyntaxClick(Sender: TObject);
//var
//  vStream: TMemoryStream;
//  vData: THCViewData;
//  vStartNo, vEndNo: Integer;
begin
  FEmrView.SyntaxCheck;

//  vStartNo := 0;
//  vData := THCViewData(FEmrView.ActiveSection.Page);
//  FEmrView.GetDataDeGroupItemNo(vData, '4', False, vStartNo, vEndNo);
//  if vStartNo >= 0 then
//  begin
//    vStream := TMemoryStream.Create;
//    try
//      // ��Ϊ��
//      FEmrView.GetDataDeGroupToStream(vData, vStartNo, vEndNo, vStream);
//      vStream.SaveToFile('c:\a.temp');
//
//      // ��������
//      vStream.LoadFromFile('c:\a.temp');
//      FEmrView.SetDataDeGroupFromStream(vData, vStartNo, vEndNo, vStream);
//    finally
//      vStream.Free;
//    end;
//  end;
end;

procedure TfrmRecord.mniQRCodeClick(Sender: TObject);
var
  vQRCode: THCQRCodeItem;
  vS: string;
begin
  vS := InputBox('�ı���', '�ı�', 'HCViewʹ����DelphiZXingQRCode��ά��ؼ�');
  vQRCode := THCQRCodeItem.Create(FEmrView.ActiveSectionTopLevelData, vS);
  FEmrView.InsertItem(vQRCode);
end;

procedure TfrmRecord.mniN5Click(Sender: TObject);
var
  vFrmBorderBackColor: TfrmBorderBackColor;
begin
  vFrmBorderBackColor := TfrmBorderBackColor.Create(Self);
  try
    vFrmBorderBackColor.SetView(FEmrView);
  finally
    FreeAndNil(vFrmBorderBackColor);
  end;
end;

procedure TfrmRecord.mniBarCodeClick(Sender: TObject);
var
  vHCBarCode: THCBarCodeItem;
  vS: string;
begin
  vS := InputBox('�ı���', '�ı�', 'HC-' + FormatDateTime('YYYYMMDD', Now));
  vHCBarCode := THCBarCodeItem.Create(FEmrView.ActiveSectionTopLevelData, vS);
  FEmrView.InsertItem(vHCBarCode);
end;

procedure TfrmRecord.mniFastPrintClick(Sender: TObject);
begin
  btnprintClick(Sender);
end;

procedure TfrmRecord.mniSaveStructureClick(Sender: TObject);
begin
  DoSaveStructure;
end;

procedure TfrmRecord.mniDeleteProtectClick(Sender: TObject);
var
  vTopData: THCCustomData;
  vDeItem: TDeItem;
  vS: string;
  i: Integer;
begin
  vTopData := FEmrView.ActiveSectionTopLevelData;

  if vTopData.SelectExists then
  begin
    for i := vTopData.SelectInfo.StartItemNo to vTopData.SelectInfo.EndItemNo do
    begin
      if vTopData.Items[i].StyleNo < THCStyle.Null then
      begin
        ShowMessage('��ֹ�༭ֻ��Ӧ�����ı����ݣ�ѡ�������д��ڷ��ı�����');
        Exit;
      end;
    end;

    if (vTopData.SelectInfo.StartItemNo = vTopData.SelectInfo.EndItemNo)
      and (vTopData.SelectInfo.StartItemOffset = 0)
      and (vTopData.SelectInfo.EndItemOffset = vTopData.GetItemOffsetAfter(vTopData.SelectInfo.StartItemNo))
    then  // ��ͬһ��Item
    begin
      (vTopData.Items[vTopData.SelectInfo.StartItemNo] as TDeItem).EditProtect := True;
      Exit;
    end;

    for i := vTopData.SelectInfo.StartItemNo to vTopData.SelectInfo.EndItemNo do
      (vTopData.Items[i] as TDeItem).EditProtect := False;

    vS := vTopData.GetSelectText;
    vS := StringReplace(vS, #13#10, '', [rfReplaceAll, rfIgnoreCase]);
    vDeItem := FEmrView.NewDeItem(vS);
    vDeItem.EditProtect := True;
    FEmrView.InsertDeItem(vDeItem);
  end
  else
  begin
    vDeItem := vTopData.GetActiveItem as TDeItem;
    vDeItem.EditProtect := not vDeItem.EditProtect;
    FEmrView.ActiveItemReAdaptEnvironment;
  end;
end;

procedure TfrmRecord.mniInsertTableClick(Sender: TObject);
var
  vFrmInsertTable: TfrmInsertTable;
  vTable: TDeTable;
  vTopData: THCRichData;
begin
  vFrmInsertTable := TfrmInsertTable.Create(Self);
  try
    vFrmInsertTable.ShowModal;
    if vFrmInsertTable.ModalResult = mrOk then
    begin
      vTopData := FEmrView.ActiveSectionTopLevelData as THCRichData;
      vTable := TDeTable.Create(vTopData, StrToInt(vFrmInsertTable.edtRows.Text),
        StrToInt(vFrmInsertTable.edtCols.Text), vTopData.Width);
      FEmrView.InsertItem(vTable);
    end;
  finally
    FreeAndNil(vFrmInsertTable);
  end;
end;

procedure TfrmRecord.mniCellVBHLClick(Sender: TObject);
begin
  FEmrView.TableApplyContentAlign(THCContentAlign.tcaBottomLeft);
end;

procedure TfrmRecord.mniCellVBHMClick(Sender: TObject);
begin
  FEmrView.TableApplyContentAlign(THCContentAlign.tcaBottomCenter);
end;

procedure TfrmRecord.mniCellVBHRClick(Sender: TObject);
begin
  FEmrView.TableApplyContentAlign(THCContentAlign.tcaBottomRight);
end;

procedure TfrmRecord.mniCellVMHLClick(Sender: TObject);
begin
  FEmrView.TableApplyContentAlign(THCContentAlign.tcaCenterLeft);
end;

procedure TfrmRecord.mniCellVMHMClick(Sender: TObject);
begin
  FEmrView.TableApplyContentAlign(THCContentAlign.tcaCenterCenter);
end;

procedure TfrmRecord.mniCellVMHRClick(Sender: TObject);
begin
  FEmrView.TableApplyContentAlign(THCContentAlign.tcaCenterRight);
end;

procedure TfrmRecord.mniCellVTHLClick(Sender: TObject);
begin
  FEmrView.TableApplyContentAlign(THCContentAlign.tcaTopLeft);
end;

procedure TfrmRecord.mniCellVTHMClick(Sender: TObject);
begin
  FEmrView.TableApplyContentAlign(THCContentAlign.tcaTopCenter);
end;

procedure TfrmRecord.mniCellVTHRClick(Sender: TObject);
begin
  FEmrView.TableApplyContentAlign(THCContentAlign.tcaTopRight);
end;

procedure TfrmRecord.mniCheckboxClick(Sender: TObject);
var
  vCheckBox: TDeCheckBox;
  vS: string;
begin
  vS := InputBox('��ѡ��', '�ı�', '');
  vCheckBox := TDeCheckBox.Create(FEmrView.ActiveSectionTopLevelData, vS, False);
  FEmrView.InsertItem(vCheckBox);
end;

procedure TfrmRecord.mniInsertImageClick(Sender: TObject);
var
  vOpenDlg: TOpenDialog;
  vImageItem: TDeImageItem;
  vTopData: THCRichData;
begin
  vOpenDlg := TOpenDialog.Create(Self);
  try
    vOpenDlg.Filter := 'ͼ���ļ�|*.bmp; *.jpg; *.jpeg; *.png|Windows Bitmap|*.bmp|JPEG �ļ�|*.jpg; *.jpge|����ֲ����ͼ��|*.png';
    FEmrView.Enabled := False;  // ���˫����ͼƬʱ���¶�λ��������
    try
      if vOpenDlg.Execute then
      begin
        if vOpenDlg.FileName <> '' then
        begin
          vTopData := FEmrView.ActiveSectionTopLevelData as THCRichData;
          vImageItem := TDeImageItem.Create(vTopData);
          vImageItem.LoadGraphicFile(vOpenDlg.FileName);
          vImageItem.RestrainSize(vTopData.Width, vImageItem.Height);
          Application.ProcessMessages;  // ���˫�����ļ��󣬴����²�ؼ���Mousemove��Mouseup�¼�
          FEmrView.InsertItem(vImageItem);
        end;
      end;
    finally
      FEmrView.Enabled := True;
    end;
  finally
    FreeAndNil(vOpenDlg);
  end;
end;

procedure TfrmRecord.mniYueJingClick(Sender: TObject);
var
  vYueJingItem: TEmrYueJingItem;
begin
  vYueJingItem := TEmrYueJingItem.Create(FEmrView.ActiveSectionTopLevelData,
    '12', '5-6', FormatDateTime('YYYY-MM-DD', Now), '28-30');
  FEmrView.InsertItem(vYueJingItem);
end;

procedure TfrmRecord.mniPirntPreviewClick(Sender: TObject);
var
  vFrmPrintView: TfrmPrintView;
begin
  if Assigned(FOnPrintPreview) then
    FOnPrintPreview(Sender)
  else
  begin
    vFrmPrintView := TfrmPrintView.Create(Self);
    try
      vFrmPrintView.SetView(FEmrView);
    finally
      FreeAndNil(vFrmPrintView);
    end;
  end;
end;

procedure TfrmRecord.mniInsertLineClick(Sender: TObject);
begin
  FEmrView.InsertLine(1);
end;

procedure TfrmRecord.mniPageSetClick(Sender: TObject);
var
  vFrmPageSet: TFrmPageSet;
begin
  vFrmPageSet := TFrmPageSet.Create(Self);
  try
    vFrmPageSet.SetHCView(FEmrView);
  finally
    FreeAndNil(vFrmPageSet);
  end;
end;

procedure TfrmRecord.mniParaClick(Sender: TObject);
var
  vFrmParagraph: TfrmParagraph;
begin
  vFrmParagraph := TfrmParagraph.Create(Self);
  try
    vFrmParagraph.SetView(FEmrView);
  finally
    FreeAndNil(vFrmParagraph);
  end;
end;

{ TTravTag }

class function TTravTag.Contains(const ATags, ATag: Integer): Boolean;
begin
  Result := (ATags and ATag) = ATag;
end;

end.
