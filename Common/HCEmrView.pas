{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit HCEmrView;

interface

{$I HCEmrView.inc}

uses
  Windows, Classes, Controls, Graphics, HCView, HCEmrViewIH, HCStyle, HCItem,
  HCTextItem, HCDrawItem, HCCustomData, HCRichData, HCViewData, HCSectionData,
  HCEmrElementItem, HCCommon, HCRectItem, HCEmrGroupItem, HCCustomFloatItem,
  HCImageItem, HCSection, Generics.Collections, Messages, HCXml;

type
  TEmrViewProp = class(TObject)
  public
    const
      /// <summary> 病历ID </summary>
      RecID = 'RecID';
      /// <summary> 创建者 </summary>
      Creator = 'Creator';
      /// <summary> 创建时间 </summary>
      CreateDateTime = 'CreatDT';
  end;

  TSyncDeItemEvent = procedure(const Sender: TObject; const AData: THCCustomData; const AItem: THCCustomItem) of object;
  THCCopyPasteStreamEvent = function(const AStream: TStream): Boolean of object;
  TDrawTraceEvent = procedure(const ADeItem: TDeItem; const ATextRect: TRect; const ACanvas: TCanvas) of object;

  THCEmrView = class({$IFDEF VIEWINPUTHELP}THCEmrViewIH{$ELSE}THCView{$ENDIF})
  private
    FDesignMode,
    FHideTrace,  // 隐藏痕迹
    FTrace,  // 是否处于留痕迹状态
    FSecret: Boolean;  // 是否处于隐私显示状态
    FTraceInfoAnnotate: Boolean;  // 留痕信息以批注形式显示
    FUnAllocWarning: Boolean;  // 没有填写的内容显示警告
    FIgnoreAcceptAction: Boolean;
    FTraceCount: Integer;  // 当前文档痕迹数量

    FInsertTraceStream: Boolean;
    FPrintUnAlloc: Boolean;
    FDeDoneColor, FDeUnDoneColor, FDeHotColor: TColor;
    FPageBlankTip: string;  // 页面空白区域提示
    FPropertyObject: TObject;

    {$IFDEF PROCSERIES}
    FUnEditProcBKColor: TColor;  // 不能编辑的病程区域背景色
    FShowProcSplit: Boolean;  // 绘制2个病程的间隔线
    FCanEditCheckInEditProc: Boolean;
    FProcCount: Integer;  // 当前文档病程数量
    FCaretProcInfo,  // 当前光标处的病程信息
    FEditProcInfo  // 当前正在编辑的病程信息
      : TProcInfo;
    FEditProcIndex: string;
    {$ENDIF}

    FPropertys: TStringList;
    FOnCanNotEdit: TNotifyEvent;
    FOnSyncDeItem: TSyncDeItemEvent;
    // 复制粘贴相关事件
    FOnCopyRequest, FOnPasteRequest: THCCopyPasteEvent;
    FOnCopyAsStream, FOnPasteFromStream: THCCopyPasteStreamEvent;
    // 语法检查相关事件
    FOnSyntaxCheck: TDataDomainItemNoEvent;
    FOnSyntaxPaint: TSyntaxPaintEvent;
    FOnDrawTrace: TDrawTraceEvent;
    FOnSaveItem: TSectionDataItemEvent;
    procedure SetHideTrace(const Value: Boolean);
    procedure SetPageBlankTip(const Value: string);
    procedure DoSyntaxCheck(const AData: THCCustomData; const AItemNo, ATag: Integer;
      const ADomainStack: TDomainStack; var AStop: Boolean);
    procedure DoSyncDeItem(const Sender: TObject; const AData: THCCustomData; const AItem: THCCustomItem);
    procedure InsertEmrTraceItem(const AText: string; const AAdd: Boolean = True);
    procedure MakeSelectTraceIf;
    function CanNotEdit: Boolean;

    function GetValue(const Key: string): string;
    procedure SetValue(const Key, Value: string);

    {$IFDEF PROCSERIES}
    /// <summary> 取光标处病程信息 </summary>
    procedure CheckCaretProcInfo;
    procedure CheckEditProcInfo;
    /// <summary> 取Section光标处病程信息 </summary>
    procedure GetSectionCaretProcInfo(const ASectionIndex: Integer; const AProcInfo: TProcInfo);
    {$ENDIF}
  protected
    /// <summary> 当有新Item创建完成后触发的事件 </summary>
    /// <param name="Sender">Item所属的文档节</param>
    procedure DoSectionCreateItem(Sender: TObject); override;

    /// <summary> 当有新Item创建时触发 </summary>
    /// <param name="AData">创建Item的Data</param>
    /// <param name="AStyleNo">要创建的Item样式</param>
    /// <returns>创建好的Item</returns>
    function DoSectionCreateStyleItem(const AData: THCCustomData;
      const AStyleNo: Integer): THCCustomItem; override;

    procedure DoSectionCaretItemChanged(const Sender: TObject; const AData: THCCustomData;
      const AItem: THCCustomItem); override;

    /// <summary> 当节某Data有Item插入后触发 </summary>
    /// <param name="Sender">在哪个文档节插入</param>
    /// <param name="AData">在哪个Data插入</param>
    /// <param name="AItem">已插入的Item</param>
    procedure DoSectionInsertItem(const Sender: TObject;
      const AData: THCCustomData; const AItem: THCCustomItem); override;

    /// <summary> 当节中某Data有Item删除后触发 </summary>
    /// <param name="Sender">在哪个文档节删除</param>
    /// <param name="AData">在哪个Data删除</param>
    /// <param name="AItem">已删除的Item</param>
    procedure DoSectionRemoveItem(const Sender: TObject;
      const AData: THCCustomData; const AItem: THCCustomItem); override;

    /// <summary> 指定的节当前是否可保存指定的Item </summary>
    function DoSectionSaveItem(const Sender: TObject;
      const AData: THCCustomData; const AItemNo: Integer): Boolean; override;

    function DoSectionPaintDomainRegion(const Sender: TObject; const AData: THCCustomData; const AItemNo: Integer): Boolean; override;

    procedure DoSectionItemMouseDown(const Sender: TObject;
      const AData: THCCustomData; const AItemNo, AOffset: Integer;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    /// <summary> 指定的节当前是否可编辑 </summary>
    /// <param name="Sender">文档节</param>
    /// <returns>True：可编辑，False：不可编辑</returns>
    function DoSectionCanEdit(const Sender: TObject): Boolean; override;

    /// <summary> 指定的节当前是否可删除指定的Item </summary>
    function DoSectionAcceptAction(const Sender: TObject; const AData: THCCustomData;
      const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean; override;

    procedure Clear; override;

    /// <summary> 按键按下 </summary>
    /// <param name="Key">按键值</param>
    /// <param name="Shift">Shift状态</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    /// <summary> 按键按压 </summary>
    /// <param name="Key">按键值</param>
    procedure KeyPress(var Key: Char); override;

    /// <summary> 在当前位置插入文本 </summary>
    /// <param name="AText">要插入的字符串(支持带#13#10的回车换行)</param>
    /// <returns>True：插入成功，False：插入失败</returns>
    function DoInsertText(const AText: string): Boolean; override;

    /// <summary> 复制前，便于控制是否允许复制 </summary>
    function DoCopyRequest(const AFormat: Word): Boolean; override;

    /// <summary> 粘贴前，便于控制是否允许粘贴 </summary>
    function DoPasteRequest(const AFormat: Word): Boolean; override;

    /// <summary> 复制前，便于订制特征数据如内容来源 </summary>
    procedure DoCopyAsStream(const AStream: TStream); override;

    /// <summary> 粘贴前，便于确认订制特征数据如内容来源 </summary>
    function DoPasteFromStream(const AStream: TStream): Boolean; override;

    /// <summary> 保存文档前触发事件，便于订制特征数据 </summary>
    procedure DoSaveStreamBefor(const AStream: TStream); override;

    procedure DoLoadStreamBefor(const AStream: TStream; const AFileVersion: Word); override;

    procedure DoSaveXmlDocument(const AXmlDoc: IHCXMLDocument); override;
    procedure DoLoadXmlDocument(const AXmlDoc: IHCXMLDocument); override;

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

    /// <summary> 文档某节的Item绘制完成 </summary>
    /// <param name="AData">当前绘制的Data</param>
    /// <param name="ADrawItemIndex">Item对应的DrawItem序号</param>
    /// <param name="ADrawRect">Item对应的绘制区域</param>
    /// <param name="ADataDrawLeft">Data绘制时的Left</param>
    /// <param name="ADataDrawBottom">Data绘制时的Bottom</param>
    /// <param name="ADataScreenTop">绘制时呈现Data的Top位置</param>
    /// <param name="ADataScreenBottom">绘制时呈现Data的Bottom位置</param>
    /// <param name="ACanvas">画布</param>
    /// <param name="APaintInfo">绘制时的其它信息</param>
    procedure DoSectionDrawItemPaintAfter(const Sender: TObject;
      const AData: THCCustomData; const AItemNo, ADrawItemNo: Integer; const ADrawRect, AClearRect: TRect;
      const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary> 遍历Item </summary>
    /// <param name="ATraverse">遍历时信息</param>
    procedure TraverseItem(const ATraverse: THCItemTraverse);

    /// <summary> 插入数据组 </summary>
    /// <param name="ADeGroup">数据组信息</param>
    /// <returns>True：成功，False：失败</returns>
    function InsertDeGroup(const ADeGroup: TDeGroup): Boolean;
    function DeleteDeGroup(const ADeIndex: string): Boolean;
    // 只在Section的Page层面取数据组
    function GetDeGroupItemNo(const AIndex: string; var AData: THCViewData; var ASectionIndex, AStartNo, AEndNo: Integer): Boolean;
    function GetCaretDeGroupProperty(const APropName: string): string;
    function SetCaretDeGroupProperty(const APropName, APropValue: string): Boolean;
    function SetDeGroupProperty(const AIndex, APropName, APropValue: string): Boolean;
    function GetDeGroupProperty(const AIndex, APropName: string): string;

    /// <summary> 插入数据元 </summary>
    /// <param name="ADeItem">数据元信息</param>
    /// <returns>True：成功，False：失败</returns>
    function InsertDeItem(const ADeItem: TDeItem): Boolean;

    /// <summary> 新建数据元 </summary>
    /// <param name="AText">数据元文本</param>
    /// <returns>新建好的数据元</returns>
    function NewDeItem(const AText: string): TDeItem;

    /// <summary>
    /// 获取指定数据元的文本内容
    /// </summary>
    /// <param name="aDeIndex"></param>
    /// <returns></returns>
    function GetDeItemText(const ADeIndex: string; var AText: string): Boolean;

    /// <summary>
    /// 获取指定数据元指定属性值
    /// </summary>
    /// <param name="aDeIndex"></param>
    /// <param name="aPropName"></param>
    /// <param name="aPropValue"></param>
    /// <returns></returns>
    function GetDeItemProperty(const ADeIndex, APropName: string; var APropValue: string): Boolean;

    /// <summary>
    /// 设置指定数据元的值
    /// </summary>
    /// <param name="ADeIndex"></param>
    /// <param name="AText"></param>
    /// <returns>是否设置成功</returns>
    function SetDeItemText(const ADeIndex, AText: string): Boolean;

    function SetDeImageGraphic(const ADeIndex: string; const AGraphicStream: TStream): Boolean;
    function SetSignatureGraphic(const ADeIndex: string; const AGraphicStream: TStream): Boolean;

    /// <summary> 同步AStartData的ARefDeItem后面和ArefDeItem相同元素的内容 </summary>
    procedure SyncDeItemAfterRef(const AStartData: THCCustomData; const ARefDeItem: TDeItem);

    /// <summary> 从正文开始找相同的元素 </summary>
    function FindSameDeItem(const ADeItem: TDeItem): TDeItem;

    /// <summary> 处理DLL里钩子传递的方向键和TAB键 </summary>
    procedure KeyDownLib(var AKey: Word);

    /// <summary>
    /// 设置指定数据元指定属性的值
    /// </summary>
    /// <param name="aDeIndex"></param>
    /// <param name="aPropName"></param>
    /// <param name="aPropValue"></param>
    /// <returns>是否设置成功</returns>
    function SetDeObjectProperty(const ADeIndex, APropName, APropValue: string; const AWhich: Integer = 0): Boolean;

    procedure GetDataDeGroupTree(const AIndex: string; const AData: THCViewData;
      const ABeginNo, AEndNo: Integer; const ADomainNode: THCDomainNode);

    {$IFDEF PROCSERIES}
    procedure GetAllProcIndex(const AIndexs: TStrings);
    procedure GetAllProcInfo(const AIndexs, AInfos: TStrings);
    function InsertProc(const AProcIndex, APropertys, ABeforProcIndex: string): Boolean;
    function DeleteProc(const AProcIndex: string): Boolean;
    function GetCaretProcProperty(const APropName: string): string;
    function GetProcProperty(const AProcIndex, APropName: string): string;
    function SetProcProperty(const AProcIndex, APropName, APropValue: string): Boolean;
    function GetProcAsText(const AProcIndex: string; var AText: string): Boolean;
    function SetProcByText(const AProcIndex, AText: string): Boolean;
    function GetProcAsStream(const AProcIndex: string; const AStream: TStream): Boolean;
    /// <summary> 适合书写过程中的替换 </summary>
    function SetProcByStream(const AProcIndex: string; const AStream: TStream): Boolean;
    /// <summary> 从病历文件中设置指定病程的内容(适合第一次写病历时调用模板) </summary>
    function SetProcByFileSteam(const AProcIndex: string; const AStream: TStream): Boolean;
    procedure SetEditProcIndex(const Value: string);
    function ScrollToProc(const AProcIndex: string): Boolean;

    procedure DeleteAllProcMark;
    /// <summary> 取病程的起始结束ItemNo </summary>
    function GetProcItemNo(const AProcIndex: string; var ASectionIndex, AStartNo, AEndNo: Integer): Boolean;
    procedure GetProcInfoAt(const AData: THCSectionData; const AItemNo, AOffset: Integer; const AProcInfo: TProcInfo);
    function SetProcDeGroupByStream(const AProcIndex, AIndex: string; const AStream: TStream; const AWhich: Integer = 0): Boolean;
    function SetProcDeGroupByText(const AProcIndex, AIndex, AText: string; const AWhich: Integer = 0): Boolean;
    {$ENDIF}

    function ScrollToItem(const AItem: THCCustomItem): Boolean;

    /// <summary> 直接设置当前数据元的值为扩展内容 </summary>
  	/// <param name="AStream">扩展内容流</param>
    procedure SetActiveItemExtra(const AStream: TStream);

    function CheckDeGroupStart(const AData: THCViewData; const AItemNo: Integer;
      const ADeIndex: string): Boolean;

    function CheckDeGroupEnd(const AData: THCViewData; const AItemNo: Integer;
      const ADeIndex: string): Boolean;

    procedure GetDataDeGroupItemNo(const AData: THCViewData; const ADeIndex: string;
      const AForward: Boolean; var AStartNo, AEndNo: Integer);

    /// <summary> 获取指定数据组中的文本内容 </summary>
    /// <param name="AData">指定从哪个Data里获取</param>
    /// <param name="ADeGroupStartNo">指定数据组的起始ItemNo</param>
    /// <param name="ADeGroupEndNo">指定数据组的结束ItemNo</param>
    /// <returns>数据组文本内容</returns>
    function GetDataDeGroupText(const AData: THCViewData;
      const ADeGroupStartNo, ADeGroupEndNo: Integer): string;

    function GetDeGroupAsText(const ADeIndex: string): string;

    /// <summary> 从当前数据组起始位置往前找相同数据组的内容 </summary>
    /// <param name="AData">指定从哪个Data里获取</param>
    /// <param name="ADeGroupStartNo">指定从哪个位置开始往前找</param>
    /// <returns>相同数据组文本形式的内容</returns>
    function GetDataForwardDeGroupText(const AData: THCViewData;
      const ADeGroupStartNo: Integer): string;

    function SetDeGroupTreeByStream(const ADomainNode: THCDomainNode; const AStream: TStream;
      const AStyle: THCStyle; const AFileVersion: Word; const AWhich: Integer): Boolean;

    /// <summary> 设置数据组的内容为指定的文本 </summary>
    /// <param name="AData">数据组所在的Data</param>
    /// <param name="ADeGroupNo">数据组的ItemNo</param>
    /// <param name="AText">文本内容</param>
    procedure SetDataDeGroupText(const AData: THCViewData; const ADeGroupStartNo, ADeGroupEndNo: Integer; const AText: string);
    procedure SetDeGroupByText(const ASection: THCSection;
      const AArea: TSectionArea; const ADeIndex, AText: string; const AStartLast: Boolean = True);
    procedure SetDeGroupByFileStream(const ASection: THCSection;
      const AArea: TSectionArea; const ADeIndex: string; const AStream: TStream; const AStartLast: Boolean = True);

    /// <summary> 将指定数据组中的内容写入到流 </summary>
    procedure GetDataDeGroupToStream(const AData: THCViewData;
      const ADeGroupStartNo, ADeGroupEndNo: Integer; const AStream: TStream);

    /// <summary> 将指定数据组中的内容写入到流 </summary>
    procedure SetDataDeGroupFromStream(const AData: THCViewData;
      const ADeGroupStartNo, ADeGroupEndNo: Integer; const AStream: TStream);

    function GetDeGroupAsStream(const AIndex: string; const AStream: TStream): Boolean;
    procedure SetDeGroupByStream(const ASection: THCSection;
      const AArea: TSectionArea; const AIndex: string; const AStream: TStream; const AStartLast: Boolean = True);

    function SetCaretToDeGroupStart(const AIndex: string): Boolean;

    function SaveSelectToText: string;
    /// <summary> 保存选中并抛弃一部分选中的数据组标识符 </summary>
    procedure SaveSelectToStream(const AStream: TStream);

    procedure SaveToLiteStream(const AStream: TStream);

    /// <summary> 语法检测 </summary>
    procedure SyntaxCheck;

    /// <summary> 是否是文档设计模式 </summary>
    property DesignMode: Boolean read FDesignMode write FDesignMode;

    /// <summary> 是否隐藏痕迹 </summary>
    property HideTrace: Boolean read FHideTrace write SetHideTrace;

    /// <summary> 是否处于留痕状态 </summary>
    property Trace: Boolean read FTrace write FTrace;

    /// <summary> 是否处于隐私显示状态 </summary>
    property Secret: Boolean read FSecret write FSecret;

    /// <summary> 留痕信息以批注形式显示（加载文档前设置好此属性） </summary>
    property TraceInfoAnnotate: Boolean read FTraceInfoAnnotate write FTraceInfoAnnotate;

    /// <summary> 文档中有几处痕迹 </summary>
    property TraceCount: Integer read FTraceCount;

    {$IFDEF PROCSERIES}
    property ProcCount: Integer read FProcCount;
    property EditProcIndex: string read FEditProcIndex write SetEditProcIndex;
    property ShowProcSplit: Boolean read FShowProcSplit write FShowProcSplit;
    property CanEditCheckInEditProc: Boolean read FCanEditCheckInEditProc write FCanEditCheckInEditProc;
    property UnEditProcBKColor: TColor read FUnEditProcBKColor write FUnEditProcBKColor;
    {$ENDIF}

    /// <summary> 页面内容不满时底部空白提示 </summary>
    property PageBlankTip: string read FPageBlankTip write SetPageBlankTip;

    property DeDoneColor: TColor read FDeDoneColor write FDeDoneColor;
    property DeUnDoneColor: TColor read FDeUnDoneColor write FDeUnDoneColor;
    property DeHotColor: TColor read FDeHotColor write FDeHotColor;

    /// <summary> 忽略AcceptAction的处理 </summary>
    property IgnoreAcceptAction: Boolean read FIgnoreAcceptAction write FIgnoreAcceptAction;

    property Values[const Key: string]: string read GetValue write SetValue; default;

    /// <summary> 当前文档名称 </summary>
    property FileName;

    /// <summary> 当前文档样式表 </summary>
    property Style;

    /// <summary> 是否对称边距 </summary>
    property SymmetryMargin;

    /// <summary> 当前光标所在页的序号 </summary>
    property ActivePageIndex;

    /// <summary> 当前预览的页序号 </summary>
    property PagePreviewFirst;

    /// <summary> 总页数 </summary>
    property PageCount;

    /// <summary> 当前光标所在节的序号 </summary>
    property ActiveSectionIndex;

    /// <summary> 水平滚动条 </summary>
    property HScrollBar;

    /// <summary> 垂直滚动条 </summary>
    property VScrollBar;

    /// <summary> 缩放值 </summary>
    property Zoom;

    /// <summary> 当前文档所有节 </summary>
    property Sections;

    /// <summary> 是否显示当前行指示符 </summary>
    property ShowLineActiveMark;

    /// <summary> 是否显示行号 </summary>
    property ShowLineNo;

    /// <summary> 是否显示下划线 </summary>
    property ShowUnderLine;

    /// <summary> 当前文档是否有变化 </summary>
    property IsChanged;

    /// <summary> 当编辑只读状态的Data时触发 </summary>
    property OnCanNotEdit: TNotifyEvent read FOnCanNotEdit write FOnCanNotEdit;

    /// <summary> 复制内容前触发 </summary>
    property OnCopyRequest: THCCopyPasteEvent read FOnCopyRequest write FOnCopyRequest;

    /// <summary> 粘贴内容前触发 </summary>
    property OnPasteRequest: THCCopyPasteEvent read FOnPasteRequest write FOnPasteRequest;

    property OnCopyAsStream: THCCopyPasteStreamEvent read FOnCopyAsStream write FOnCopyAsStream;

    property OnPasteFromStream: THCCopyPasteStreamEvent read FOnPasteFromStream write FOnPasteFromStream;

    /// <summary> 数据元需要同步内容时触发 </summary>
    property OnSyncDeItem: TSyncDeItemEvent read FOnSyncDeItem write FOnSyncDeItem;

    /// <summary> 数据元需要用语法检测器来检测时触发 </summary>
    property OnSyntaxCheck: TDataDomainItemNoEvent read FOnSyntaxCheck write FOnSyntaxCheck;

    /// <summary> 数据元绘制语法问题时触发 </summary>
    property OnSyntaxPaint: TSyntaxPaintEvent read FOnSyntaxPaint write FOnSyntaxPaint;
    property OnDrawTrace: TDrawTraceEvent read FOnDrawTrace write FOnDrawTrace;
    property OnSaveItem: TSectionDataItemEvent read FOnSaveItem write FOnSaveItem;
  published
    { Published declarations }

    /// <summary> 节有新的Item创建时触发 </summary>
    property OnSectionCreateItem;

    /// <summary> 节有新的Item插入时触发 </summary>
    property OnSectionItemInsert;

    /// <summary> Item绘制开始前触发 </summary>
    property OnSectionDrawItemPaintBefor;

    /// <summary> Item绘制完成后触发 </summary>
    property OnSectionDrawItemPaintAfter;

    /// <summary> 节页眉绘制时触发 </summary>
    property OnSectionPaintHeaderAfter;

    /// <summary> 节页脚绘制时触发 </summary>
    property OnSectionPaintFooterAfter;

    /// <summary> 节页面绘制时触发 </summary>
    property OnSectionPaintPageAfter;

    /// <summary> 节整页绘制前触发 </summary>
    property OnSectionPaintPaperBefor;

    /// <summary> 节整页绘制后触发 </summary>
    property OnSectionPaintPaperAfter;

    /// <summary> 节只读属性有变化时触发 </summary>
    property OnSectionReadOnlySwitch;

    /// <summary> 界面显示模式：页面、Web </summary>
    property ViewModel;

    /// <summary> 是否根据宽度自动计算缩放比例 </summary>
    property AutoZoom;

    /// <summary> 所有Section是否只读 </summary>
    property ReadOnly;

    /// <summary> 鼠标按下时触发 </summary>
    property OnMouseDown;

    /// <summary> 鼠标弹起时触发 </summary>
    property OnMouseUp;

    /// <summary> 光标位置改变时触发 </summary>
    property OnCaretChange;

    /// <summary> 垂直滚动条滚动时触发 </summary>
    property OnVerScroll;

    /// <summary> 文档内容变化时触发 </summary>
    property OnChange;

    /// <summary> 文档Change状态切换时触发 </summary>
    property OnChangedSwitch;

    /// <summary> 窗口重绘开始时触发 </summary>
    property OnPaintViewBefor;

    /// <summary> 窗口重绘结束后触发 </summary>
    property OnPaintViewAfter;

    property PopupMenu;

    property Align;
  end;

/// <summary> 注册HCEmrView控件到控件面板 </summary>
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
    Result := (vDeGroup.MarkType = TMarkType.cmtEnd) and (vDeGroup[TDeProp.Index] = ADeIndex);
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
  FPropertys.Clear;
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
  FDeDoneColor := clBtnFace;  // 元素填写后背景色
  FDeUnDoneColor := $0080DDFF;  // 元素未填写时背景色
  FDeHotColor := $00F4E0CC;  // 鼠标移动到元素上背景色
  FPageBlankTip := '';  // '--------本页以下空白--------'
  Self.Style.DefaultTextStyle.Size := GetFontSize('小四');
  Self.Style.DefaultTextStyle.Family := '宋体';
  Self.HScrollBar.AddStatus(200);
  FInsertTraceStream := False;
  FPrintUnAlloc := False;
  FUnAllocWarning := True;
  FPropertys := TStringList.Create;
  {$IFDEF PROCSERIES}
  FUnEditProcBKColor := clBtnFace;  // 不能编辑的病程区域背景色
  FShowProcSplit := True;
  FCanEditCheckInEditProc := True;
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
  FreeAndNil(FPropertys);
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
    MakeSelectTraceIf;
    InsertEmrTraceItem(AText);
    Result := True;
  end
  else
    Result := inherited DoInsertText(AText);
end;

procedure THCEmrView.DoLoadStreamBefor(const AStream: TStream; const AFileVersion: Word);
var
  vVersion: Byte;
  vS: string;
begin
  if AFileVersion > 43 then
    AStream.ReadBuffer(vVersion, 1)
  else
    vVersion := 0;

  if vVersion > 0 then
  begin
    HCLoadTextFromStream(AStream, vS, AFileVersion);
    if Self.Style.States.Contain(hosLoading) then  // 加载病历时才处理，插入文件流时不覆盖
      FPropertys.Text := vS;
  end
  else
  if Self.Style.States.Contain(hosLoading) then  // 加载病历时才处理，插入文件流时不覆盖
    FPropertys.Clear;

  inherited DoLoadStreamBefor(AStream, AFileVersion);
end;

procedure THCEmrView.DoLoadXmlDocument(const AXmlDoc: IHCXMLDocument);
begin
  inherited DoLoadXmlDocument(AXmlDoc);
  if AXmlDoc.DocumentElement.HasAttribute('property') then
    FPropertys.Text := GetXmlRN(AXmlDoc.DocumentElement.Attributes['property']);
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
  vData: THCCustomData;
begin
  vData := Self.ActiveSectionTopLevelData;
  vItem := vData.GetActiveItem;
  if (vItem is TDeItem) and (vItem as TDeItem).IsElement then
  begin
    if AFormat <> CF_TEXT then
    begin
      if not vData.SelectStartItemBoundary then  // 选中不在元素边界
      begin
        Result := False;
        Exit;
      end;
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
  if FCanEditCheckInEditProc then
  begin
    if FEditProcIndex <> FCaretProcInfo.Index then  // 光标处和当前允许编辑的不同
    begin
      Result := False;  // 不允许编辑
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
    if (FProcCount > 0) and (AData = Self.ActiveSection.Page) then  // 有病程
    begin
      CheckCaretProcInfo; // 当前位置病程信息
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
      if vDeItem.TraceStyles <> [] then
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
    (Sender as TDeItem).TraceStyles := [TDeTraceStyle.cseAdd];

  inherited DoSectionCreateItem(Sender);
end;

function THCEmrView.DoSectionCreateStyleItem(const AData: THCCustomData;
  const AStyleNo: Integer): THCCustomItem;
begin
  Result := HCEmrElementItem.CreateEmrStyleItem(AData, AStyleNo);
end;

procedure THCEmrView.DoSaveStreamBefor(const AStream: TStream);
var
  vByte: Byte;
begin
  vByte := EmrViewVersion;
  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
  inherited DoSaveStreamBefor(AStream);
end;

procedure THCEmrView.DoSaveXmlDocument(const AXmlDoc: IHCXMLDocument);
begin
  inherited DoSaveXmlDocument(AXmlDoc);
  if FPropertys.Text <> '' then
    AXmlDoc.DocumentElement.Attributes['property'] := FPropertys.Text;
end;

function THCEmrView.DoSectionAcceptAction(const Sender: TObject;
  const AData: THCCustomData; const AItemNo, AOffset: Integer; const AAction: THCAction): Boolean;
var
  vItem: THCCustomItem;
  vDeItem: TDeItem;
begin
  if FIgnoreAcceptAction then Exit(True);

  {$IFDEF PROCSERIES}
  if (AAction = THCAction.actDeleteSelected)
    and (AData = Self.ActiveSection.Page)
    and (FEditProcInfo.EndNo > 0)
  then
  begin
    if (AData.SelectInfo.StartItemNo > FEditProcInfo.BeginNo)
      and (AData.SelectInfo.EndItemNo < FEditProcInfo.EndNo)
    then

    else
    begin
      Result := False;
      Exit;
    end;
  end;

  if FCaretProcInfo.EndNo > 0 then
  begin
    (Self.Sections[FCaretProcInfo.SectionIndex].Page.Items[FCaretProcInfo.BeginNo] as TDeGroup).Changed := True;
    //(Self.Sections[FCaretProcInfo.SectionIndex].Page.Items[FCaretProcInfo.EndNo] as TDeGroup).Changed := True;
  end;
  {$ENDIF}

  Result := inherited DoSectionAcceptAction(Sender, AData, AItemNo, AOffset, AAction);
  if Result then
  begin
    case AAction of
      actBackDeleteText,
      actDeleteText:
        begin
          if AData.Items[AItemNo] is TDeItem then
          begin
            vDeItem := AData.Items[AItemNo] as TDeItem;

            if not FTrace and vDeItem.IsElement and (vDeItem.Length = 1) and not vDeItem.DeleteAllow then
            begin
              if vDeItem[TDeProp.Name] <> '' then
                Self.SetActiveItemText(vDeItem[TDeProp.Name])
              else
                Self.SetActiveItemText('未填写');

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
          //if not FDesignMode then  // 非设计模式不允许直接删除
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
    vTrace := ADeItem[TDeProp.TraceAdd] + ' ' + ADeItem[TDeProp.TraceDel];
    vSize := ACanvas.TextExtent(vTrace);
    vRect := Bounds(AClearRect.Left, AClearRect.Top - vSize.cy - 5, vSize.cx, vSize.cy);
    if vRect.Right > ADataDrawRight then
      OffsetRect(vRect, ADataDrawRight - vRect.Right, 0);

    if TDeTraceStyle.cseDel in ADeItem.TraceStyles then
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
  if APaintInfo.Print and not FPrintUnAlloc then
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

  if (not FHideTrace) and (FTraceCount > 0) then  // 显示痕迹且有痕迹
  begin
    vItem := AData.Items[AItemNo];
    if vItem.StyleNo > THCStyle.Null then
    begin
      vDeItem := vItem as TDeItem;
      if (vDeItem.TraceStyles <> [])
        and ((vDeItem[TDeProp.TraceAdd] <> '') or (vDeItem[TDeProp.TraceDel] <> ''))
      then
      begin
        if FTraceInfoAnnotate then  // 以批注形式显示痕迹
        begin
          vDrawAnnotate := THCDrawAnnotateDynamic.Create;
          vDrawAnnotate.DrawRect := AClearRect;
          vDrawAnnotate.Title := vDeItem.GetHint;
          vDrawAnnotate.Text := AData.GetDrawItemText(ADrawItemNo);

          Self.AnnotatePre.AddDrawAnnotate(vDrawAnnotate);
          //Self.VScrollBar.AddAreaPos(AData.DrawItems[ADrawItemNo].Rect.Top, ADrawRect.Height);
        end
        else
        if (ADrawItemNo = (AData as THCRichData).HotDrawItemNo)
          and (not(AData as THCRichData).MouseMoveRestrain)
        then
          DrawTraceHint_(vDeItem);
      end;
    end;
  end;

  if FSecret then  // 隐私显示状态
  begin
    vItem := AData.Items[AItemNo];
    if (vItem.StyleNo > THCStyle.Null) and ((vItem as TDeItem)[TDeProp.Secret] <> '') then  // 在这里处理其实时间效率不高，可在DeItem的Load后计算出密显的起始和结束保存到字段里，但会增加空间占用，占空间还是占时间呢？
    begin
      TDeItem.GetSecretRange((vItem as TDeItem)[TDeProp.Secret], vSecretLow, vSecretHi);
      if vSecretLow > 0 then
      begin
        if vSecretHi < 0 then
          vSecretHi := vItem.Length;

        vDrawItem := AData.DrawItems[ADrawItemNo];
        if vSecretLow <= vDrawItem.CharOffsetEnd then  // =是处理Low和Hi相同，Hi和OffsetEnd重合时的情况
        begin
          if vSecretLow < vDrawItem.CharOffs then
            vSecretLow := vDrawItem.CharOffs;

          if vSecretHi > vDrawItem.CharOffsetEnd then
            vSecretHi := vDrawItem.CharOffsetEnd;

          vSecretLow := vSecretLow - vDrawItem.CharOffs + 1;
          if vSecretLow > 0 then
            Dec(vSecretLow);  // 转为光标Offset

          vSecretHi := vSecretHi - vDrawItem.CharOffs + 1;  // 不用转光标Offset，因为就是在后面

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
  if (not APaintInfo.Print) and (AData.Items[AItemNo] is TDeGroup) then  // 绘制病程的前后指示箭头
  begin
    vDeGroup := AData.Items[AItemNo] as TDeGroup;
    if vDeGroup.MarkType = TMarkType.cmtBeg then  // 头
    begin
      if vDeGroup[TGroupProp.SubType] = TSubType.Proc then  // 病程头
      begin
        if (AItemNo > 0) and (AData.Items[AItemNo - 1] is TDeGroup)
          and ((AData.Items[AItemNo - 1] as TDeGroup)[TGroupProp.SubType] = TSubType.Proc)  // 上一个是病程尾
        then
          HCDrawArrow(ACanvas, clMedGray, AClearRect.Left - 10, AClearRect.Top, 0);  // 向上箭头

        if FEditProcInfo.BeginNo = AItemNo then
          HCDrawArrow(ACanvas, clBlue, AClearRect.Left - 10, AClearRect.Top + 12, 1)
        else
          HCDrawArrow(ACanvas, clMedGray, AClearRect.Left - 10, AClearRect.Top + 12, 1);  // 向下箭头
      end;
    end
    else  // 尾
    begin
      if vDeGroup[TGroupProp.SubType] = TSubType.Proc then  // 病程尾
      begin
        if (AItemNo < AData.Items.Count - 1) and (AData.Items[AItemNo + 1] is TDeGroup)
          and ((AData.Items[AItemNo + 1] as TDeGroup)[TGroupProp.SubType] = TSubType.Proc)  // 下一个是病程头
        then
          HCDrawArrow(ACanvas, clMedGray, AClearRect.Right + 10, AClearRect.Top + 12, 1);  // 向下箭头

        if FEditProcInfo.EndNo = AItemNo then
          HCDrawArrow(ACanvas, clBlue, AClearRect.Right + 10, AClearRect.Top, 0)
        else
          HCDrawArrow(ACanvas, clMedGray, AClearRect.Right + 10, AClearRect.Top, 0);  // 向上箭头
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
    if (AData is THCPageData) and (AData.Items[AItemNo] is TDeGroup) then  // 病程头绘制前后分隔线
    begin
      vDeGroup := AData.Items[AItemNo] as TDeGroup;
      if vDeGroup.IsProcBegin then  // 是病程
      begin
        ACanvas.Pen.Style := psDashDotDot;
        ACanvas.Pen.Color := clBlue;
        ACanvas.MoveTo(ADataDrawLeft, ADrawRect.Top - 1);
        ACanvas.LineTo(ADataDrawRight, ADrawRect.Top - 1);
      end;
    end;
  end;
  {$ENDIF}

  if FUnAllocWarning and (AData.Items[AItemNo].StyleNo = THCStyle.Domain) then
  begin
    if (AData.Items[AItemNo] as TDeGroup).Empty then
    begin
      ACanvas.Pen.Color := clRed;
      ACanvas.Pen.Width := 1;
      ACanvas.Pen.Style := psSolid;
      HCDrawWave(ACanvas, AClearRect);
    end;
  end;

  if not (AData.Items[AItemNo] is TDeItem) then Exit;

  vDeItem := AData.Items[AItemNo] as TDeItem;
  if not vDeItem.Selected then
  begin
    if vDeItem.IsElement then  // 是数据元
    begin
      if vDeItem.MouseIn or vDeItem.Active then  // 鼠标移入或光标在其中
      begin
        if vDeItem.OutOfRang then
          ACanvas.Brush.Color := clRed
        else
          ACanvas.Brush.Color := FDeHotColor;

        ACanvas.FillRect(ADrawRect);
      end
      else
      if FDesignMode then  // 设计模式
      begin
        if vDeItem.AllocValue then  // 已经填写过了
          ACanvas.Brush.Color := FDeDoneColor
        else  // 没填写过
          ACanvas.Brush.Color := FDeUnDoneColor;

        ACanvas.FillRect(ADrawRect);
      end
      else  // 非设计模式
      begin
        if vDeItem.OutOfRang then  // 超范围
        begin
          ACanvas.Brush.Color := clRed;
          ACanvas.FillRect(ADrawRect);
        end
        else  // 没超范围
        begin
          if vDeItem.AllocValue then  // 已经填写过了
            ACanvas.Brush.Color := FDeDoneColor
          else  // 没填写过
            ACanvas.Brush.Color := FDeUnDoneColor;

          ACanvas.FillRect(ADrawRect);
        end;
      end;

      if (AItemNo < AData.Items.Count - 1)
        and (not AData.Items[AItemNo + 1].ParaFirst)
        and (AData.Items[AItemNo + 1].StyleNo > THCStyle.Null)
        and (AData.Items[AItemNo + 1] as TDeItem).IsElement
      then  // 后面挨着另一个元素
      begin
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Color := Style.BackgroundColor;
        ACanvas.Pen.Style := psSolid;
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
      end;

      if (AItemNo > 0)
        and (not AData.Items[AItemNo].ParaFirst)
        and (AData.Items[AItemNo -1].StyleNo > THCStyle.Null)
        and (AData.Items[AItemNo - 1] as TDeItem).IsElement
      then
      begin
        ACanvas.Pen.Width := 1;
        ACanvas.Pen.Color := Style.BackgroundColor;
        ACanvas.Pen.Style := psSolid;
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
    else  // 不是数据元
    if FDesignMode or vDeItem.MouseIn or vDeItem.Active then
    begin
      if vDeItem.EditProtect or vDeItem.CopyProtect then
      begin
        ACanvas.Brush.Color := clBtnFace;
        ACanvas.FillRect(ADrawRect);
      end;
    end;
  end;

  if (not FHideTrace and (vDeItem.TraceStyles <> [])) then
  begin
    if Assigned(FOnDrawTrace) then
      FOnDrawTrace(vDeItem, AClearRect, ACanvas)
    else
    begin
      if TDeTraceStyle.cseDel in vDeItem.TraceStyles then
      begin
        vTextHeight := Style.TextStyles[vDeItem.StyleNo].FontHeight;
        case Style.ParaStyles[vDeItem.ParaNo].AlignVert of
          pavTop: vTop := ADrawRect.Top + vTextHeight div 2;
          pavCenter: vTop := ADrawRect.Top + (ADrawRect.Bottom - ADrawRect.Top) div 2;
        else
          vTop := ADrawRect.Bottom - vTextHeight div 2;
        end;

        ACanvas.Pen.Style := psSolid;
        ACanvas.Pen.Color := clRed;
        ACanvas.Pen.Width := 1;
        //vTop := vTop + (ADrawRect.Bottom - vTop) div 2;
        ACanvas.MoveTo(ADrawRect.Left, vTop - 1);
        ACanvas.LineTo(ADrawRect.Right, vTop - 1);
        ACanvas.MoveTo(ADrawRect.Left, vTop + 2);
        ACanvas.LineTo(ADrawRect.Right, vTop + 2);
      end;

      if TDeTraceStyle.cseAdd in vDeItem.TraceStyles then
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
  vDrawSyntax: Boolean;
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
      if vSyOffset > vOffsetEnd then  // 语法问题起始在此DrawItem之后
        Continue;

      vSyOffsetEnd := vSyOffset + vDeItem.Syntaxs[i].Length - 1;
      if vSyOffsetEnd < vOffset then  // 语法问题结束在此DrawItem之前
        Continue;

      vDrawSyntax := False;
      if (vSyOffset <= vOffset) and (vSyOffsetEnd >= vOffsetEnd) then  // 问题包含此DrawItem
      begin
        vDrawSyntax := True;
        vRect.Left := AClearRect.Left;
        vRect.Right := AClearRect.Right;
      end
      else
      if vSyOffset >= vOffset then  // 有交集
      begin
        vDrawSyntax := True;
        if vSyOffsetEnd <= vOffsetEnd then  // 问题在DrawItem中间
        begin
          vStart := vSyOffset - vOffset;
          vLen := vDeItem.Syntaxs[i].Length;
          vRect.Left := AClearRect.Left //+ ACanvas.TextWidth(System.Copy(ADrawText, 1, vStart - 1));
            + AData.GetDrawItemOffsetWidth(ADrawItemNo, vStart, ACanvas);
          vRect.Right := AClearRect.Left //+ ACanvas.TextWidth(System.Copy(ADrawText, 1, vStart + vLen - 1));
            + AData.GetDrawItemOffsetWidth(ADrawItemNo, vStart + vLen, ACanvas);
        end
        else  // DrawItem是问题的一部分
        begin
          vRect.Left := AClearRect.Left
            + AData.GetDrawItemOffsetWidth(ADrawItemNo, vSyOffset - vOffset, ACanvas);
          vRect.Right := AClearRect.Right;
        end;
      end
      else  // vSyOffset < vOffset
      if vSyOffsetEnd <= vOffsetEnd then  // 有交集，DrawItem是问题的一部分
      begin
        vDrawSyntax := True;
        vRect.Left := AClearRect.Left;
        vRect.Right := AClearRect.Left //+ ACanvas.TextWidth(System.Copy(ADrawText, 1, vLen));
          + AData.GetDrawItemOffsetWidth(ADrawItemNo, vSyOffsetEnd - vOffset + 1, ACanvas);
      end;

      if vDrawSyntax then  // 此DrawItem中有语法问题
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

          HCDrawWave(ACanvas, vRect);
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
    if FInsertTraceStream and not Style.States.Contain(hosInsertBreakItem) then
      vDeItem.TraceStyles := vDeItem.TraceStyles + [TDeTraceStyle.cseDel];

    if vDeItem.TraceStyles <> [] then
    begin
      Inc(FTraceCount);
      if TDeTraceStyle.cseDel in vDeItem.TraceStyles then
        vDeItem.Visible := not FHideTrace
      else
        vDeItem.Visible := True;

      if FTraceInfoAnnotate and not FHideTrace then
        Self.AnnotatePre.InsertDataAnnotate(nil);
    end;

    if FTrace and (TDeTraceStyle.cseDel in vDeItem.TraceStyles) then

    else
    if not Style.States.Contain(hosInsertBreakItem) then
      DoSyncDeItem(Sender, AData, AItem);
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
  {if AItem is TDeEdit then
    DoSyncDeItem(Sender, AData, AItem)
  else
  if AItem is TDeCombobox then
    DoSyncDeItem(Sender, AData, AItem)
  else
  if AItem is TDeFloatBarCodeItem then
    DoSyncDeItem(Sender, AData, AItem)
  else
  if AItem is TDeImageItem then }
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
    if ((vItem is TDeItem) and AData.SelectInfo.StartRestrain
      and (vItem.Length > 0))
    then
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
    if vPt.Y > ARect.Top then  // 在当前编辑的病程头上面
    begin
      ACanvas.Brush.Color := FUnEditProcBKColor;
      ACanvas.FillRect(Rect(ARect.Left, ARect.Top, ARect.Right, vPt.Y));
    end;

    if FEditProcInfo.EndNo < vData.Items.Count - 1 then  // 当前编辑的病程尾后面有内容
    begin
      vPt := vData.DrawItems[vData.Items[FEditProcInfo.EndNo].FirstDItemNo].Rect.BottomRight;
      vPt := Self.GetFormatPointToViewCoord(vPt);
      if vPt.Y < ARect.Bottom then  // 在当前编辑病程尾下面
      begin
        ACanvas.Brush.Color := FUnEditProcBKColor;
        // 借用vPt.X变量来存放当前页面数据最底部
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

    if vDeItem.TraceStyles <> [] then
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
  if Style.States.Contain(THCState.hosCopying) then  // 复制保存
  begin
    //if (AData.Items[AItemNo] is TDeGroup) and (not FDesignMode) then  // 非设计模式不复制数据组
    //  Result := False
    //else
    if AData.Items[AItemNo] is TDeItem then
      Result := not (AData.Items[AItemNo] as TDeItem).CopyProtect;  // 是否禁止复制
  end;

  {$IFDEF USESAVEITEMEVENT}
  if Style.States.Contain(hosSaving) and Result and Assigned(FOnSaveItem) then
    FOnSaveItem(Sender, AData, AData.Items[AItemNo]);
  {$ENDIF}
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
  //if Assigned(FOnSyntaxCheck) then 调用前已经判断了
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

function THCEmrView.GetDeGroupAsStream(const AIndex: string; const AStream: TStream): Boolean;
var
  vSectionIndex, vStartNo, vEndNo: Integer;
  vData: THCViewData;
begin
  Result := GetDeGroupItemNo(AIndex, vData, vSectionIndex, vStartNo, vEndNo);
  if Result then
    GetDataDeGroupToStream(vData, vStartNo, vEndNo, AStream);
end;

function THCEmrView.GetDeGroupAsText(const ADeIndex: string): string;
var
  vSectionIndex, vStartNo, vEndNo: Integer;
  vData: THCViewData;
begin
  Result := '';
  if GetDeGroupItemNo(ADeIndex, vData, vSectionIndex, vStartNo, vEndNo) then
    Result := GetDataDeGroupText(vData, vStartNo, vEndNo);
end;

function THCEmrView.GetDeGroupItemNo(const AIndex: string; var AData: THCViewData;
  var ASectionIndex, AStartNo, AEndNo: Integer): Boolean;
var
  i: Integer;
  vPageData: THCSectionData;
  vDomainTree: THCDomainNode;
begin
  Result := False;
  ASectionIndex := -1;

  vDomainTree := THCDomainNode.Create;
  try
    for i := 0 to Self.Sections.Count - 1 do
    begin
      AStartNo := -1;
      AEndNo := -1;
      vPageData := Self.Sections[i].Page;

      GetDataDeGroupTree(AIndex, vPageData, 0, vPageData.Items.Count - 1, vDomainTree);
      if vDomainTree.Childs.Count > 0 then
      begin
        ASectionIndex := i;
        AData := vDomainTree.Childs[0].Data as THCViewData;
        AStartNo := vDomainTree.Childs[0].BeginNo;
        AEndNo := vDomainTree.Childs[0].EndNo;
        Result := True;

        Break;
      end;
    end;
  finally
    FreeAndNil(vDomainTree);
  end;
end;

function THCEmrView.GetDeGroupProperty(const AIndex, APropName: string): string;
var
  vSectionIndex, vStartNo, vEndNo: Integer;
  vData: THCViewData;
begin
  if GetDeGroupItemNo(AIndex, vData, vSectionIndex, vStartNo, vEndNo) then
    Result := (vData.Items[vStartNo] as TDeGroup)[APropName]
  else
    Result := '';
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

function THCEmrView.GetValue(const Key: string): string;
begin
  Result := FPropertys.Values[Key];
end;

{$IFDEF PROCSERIES}
procedure THCEmrView.GetAllProcIndex(const AIndexs: TStrings);
var
  i, j: Integer;
  vDeGroup: TDeGroup;
  vData: THCSectionData;
begin
  AIndexs.Clear;

  for i := 0 to Self.Sections.Count - 1 do
  begin
    vData := Self.Sections[i].Page;
    for j := 0 to vData.Items.Count - 1 do
    begin
      if vData.Items[j] is TDeGroup then
      begin
        vDeGroup := vData.Items[j] as TDeGroup;
        if vDeGroup.IsProcBegin then
          AIndexs.Add(vDeGroup.Index);
      end;
    end;
  end;
end;

procedure THCEmrView.GetAllProcInfo(const AIndexs, AInfos: TStrings);
var
  i, j: Integer;
  vDeGroup: TDeGroup;
  vData: THCSectionData;
begin
  AIndexs.Clear;
  AInfos.Clear;
  for i := 0 to Self.Sections.Count - 1 do
  begin
    vData := Self.Sections[i].Page;
    for j := 0 to vData.Items.Count - 1 do
    begin
      if vData.Items[j] is TDeGroup then
      begin
        vDeGroup := vData.Items[j] as TDeGroup;
        if vDeGroup.IsProcBegin then
        begin
          AIndexs.Add(vDeGroup.Index);
          AInfos.Add(vDeGroup.Propertys.Text);
        end;
      end;
    end;
  end;
end;

function THCEmrView.InsertProc(const AProcIndex, APropertys, ABeforProcIndex: string): Boolean;
var
  vPageData: THCViewData;
  vDeGroup: TDeGroup;
  vStrings: TStringList;
  i, vSectionIndex, vStartNo, vEndNo: Integer;
begin
  Result := False;
  if AProcIndex = '' then Exit;

  if ABeforProcIndex <> '' then
  begin
    if GetProcItemNo(ABeforProcIndex, vSectionIndex, vStartNo, vEndNo) then
    begin
      if vSectionIndex <> Self.ActiveSectionIndex then
        Self.ActiveSectionIndex := vSectionIndex;

      vPageData := Self.ActiveSection.Page;
      //vPageData.SetSelectBound(vEndNo, OffsetAfter, vEndNo, OffsetAfter);
      vPageData.SetSelectBound(vStartNo, 0, vStartNo, 0);
    end
    else
      Exit;
  end
  else
  begin
    vPageData := Self.ActiveSectionTopLevelData as THCViewData;
    vPageData.SelectLastItemAfterWithCaret;
  end;

  if vPageData = Self.ActiveSection.Page then
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
        if not vPageData.IsEmptyData then
          Self.InsertBreak;

        if ABeforProcIndex <> '' then
        begin
          vPageData.SetSelectBound(vPageData.SelectInfo.StartItemNo - 1, 0,
            vPageData.SelectInfo.StartItemNo - 1, 0);
        end;

        Self.ApplyParaAlignHorz(TParaAlignHorz.pahLeft);
        Result := Self.InsertDeGroup(vDeGroup);

        vEndNo := vPageData.SelectInfo.StartItemNo;
        vPageData.SetSelectBound(vEndNo, 0, vEndNo, 0);
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
    Self.BeginUpdate;  // 如果删除的是当前编辑的病程，防止重绘时病程起始结束ItemNo没有重新计算引起越界
    try
      FIgnoreAcceptAction := True;
      try
        vPage := Self.Sections[vSectionIndex].Page;
        Result := Self.Sections[vSectionIndex].DataAction(vPage, function(): Boolean
        begin
          vPage.DeleteItems(vStartNo, vEndNo, False);
          Result := True;
        end);
      finally
        FIgnoreAcceptAction := False;
      end;

      Self.ClearUndo;
      CheckCaretProcInfo;
      CheckEditProcInfo;
    finally
      Self.EndUpdate;
    end;
  end;
end;

procedure THCEmrView.GetProcInfoAt(const AData: THCSectionData; const AItemNo: Integer; const AOffset: Integer; const AProcInfo: TProcInfo);
//var
//  vSectionIndex: Integer;
begin
  //vSectionIndex := AProcInfo.SectionIndex;
  AData.GetDomainFrom(AItemNo, AOffset, AProcInfo,
    function(const ADomainItem: THCCustomRectItem): Boolean
    begin
      Result := (ADomainItem as TDeGroup).IsProc;
    end);

  //AProcInfo.SectionIndex := vSectionIndex;
  if AProcInfo.EndNo > 0 then
    AProcInfo.Index := (AData.Items[AProcInfo.BeginNo] as TDeGroup).Index;
end;

function THCEmrView.SetProcDeGroupByStream(const AProcIndex, AIndex: string; const AStream: TStream; const AWhich: Integer = 0): Boolean;
var
  vSectionIndex, vStartNo, vEndNo: Integer;
  vDomainTree: THCDomainNode;
  vRe: Boolean;
begin
  Result := GetProcItemNo(AProcIndex, vSectionIndex, vStartNo, vEndNo);
  if Result then
  begin
    vDomainTree := THCDomainNode.Create;
    try
      GetDataDeGroupTree(AIndex, Self.Sections[vSectionIndex].Page, vStartNo, vEndNo, vDomainTree);
      if vDomainTree.Childs.Count = 0 then Exit(False);

      vRe := False;
      DataLoadLiteStream(AStream, procedure(const AFileVersion: Word; const AStyle: THCStyle)
      begin
        vRe := Self.SetDeGroupTreeByStream(vDomainTree, AStream, AStyle, AFileVersion, AWhich);
      end);
      Result := vRe;

      Self.ClearUndo;
      {$IFDEF PROCSERIES}
      CheckCaretProcInfo;
      {$ENDIF}
    finally
      FreeAndNil(vDomainTree);
    end;
  end;
end;

function THCEmrView.SetProcDeGroupByText(const AProcIndex, AIndex, AText: string; const AWhich: Integer = 0): Boolean;
var
  i, vSectionIndex, vStartNo, vEndNo: Integer;
  vDomainTree: THCDomainNode;
  vDomainInfo: THCDomainInfo;
begin
  Result := GetProcItemNo(AProcIndex, vSectionIndex, vStartNo, vEndNo);
  if Result then
  begin
    vDomainTree := THCDomainNode.Create;
    try
      GetDataDeGroupTree(AIndex, Self.Sections[vSectionIndex].Page, vStartNo, vEndNo, vDomainTree);
      if vDomainTree.Childs.Count = 0 then Exit;

      Self.BeginUpdate;
      try
        FIgnoreAcceptAction := True;
        try
          Self.Style.States.Include(hosDomainWholeReplace);
          try
            if AWhich = 0 then
            begin
              vDomainInfo := vDomainTree.Childs[0];
              (vDomainInfo.Data as THCViewData).SetSelectBound(vDomainInfo.BeginNo, OffsetAfter, vDomainInfo.EndNo, OffsetBefor);
              (vDomainInfo.Data as THCViewData).InsertText(AText);
            end
            else
            if AWhich = 1 then
            begin
              vDomainInfo := vDomainTree.Childs.Last;
              (vDomainInfo.Data as THCViewData).SetSelectBound(vDomainInfo.BeginNo, OffsetAfter, vDomainInfo.EndNo, OffsetBefor);
              (vDomainInfo.Data as THCViewData).InsertText(AText);
            end
            else
            begin
              for i := vDomainTree.Childs.Count - 1 downto 0 do
              begin
                vDomainInfo := vDomainTree.Childs[i];
                (vDomainInfo.Data as THCViewData).SetSelectBound(vDomainInfo.BeginNo, OffsetAfter, vDomainInfo.EndNo, OffsetBefor);
                (vDomainInfo.Data as THCViewData).InsertText(AText);
              end;
            end;
          finally
            Self.Style.States.Exclude(hosDomainWholeReplace);
          end;
        finally
          FIgnoreAcceptAction := False;
        end;

        Self.FormatData;
      finally
        Self.EndUpdate;
      end;

      Self.ClearUndo;

      {$IFDEF PROCSERIES}
      CheckCaretProcInfo;
      {$ENDIF}
    finally
      FreeAndNil(vDomainTree);
    end;
  end;
end;

procedure THCEmrView.CheckCaretProcInfo;
begin
  GetSectionCaretProcInfo(Self.ActiveSectionIndex, FCaretProcInfo);
  if FCaretProcInfo.Index = FEditProcIndex then
    FEditProcInfo.Assign(FCaretProcInfo);
end;

procedure THCEmrView.CheckEditProcInfo;
var
  vSectionIndex, vBeginNo, vEndNo: Integer;
begin
  FEditProcInfo.Clear;
  GetProcItemNo(FEditProcIndex, vSectionIndex, vBeginNo, vEndNo);
  if vEndNo > 0 then
  begin
    if Self.ActiveSectionIndex <> vSectionIndex then
      Self.ActiveSectionIndex := vSectionIndex;

    FEditProcInfo.SectionIndex := vSectionIndex;
    FEditProcInfo.Data := Self.ActiveSection.Page;
    FEditProcInfo.BeginNo := vBeginNo;
    FEditProcInfo.EndNo := vEndNo;
    FEditProcInfo.Index := FEditProcIndex;
  end;
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

    {if APropName = TGroupProp.Name then
      Result := vBeginGroup[TDeProp.Name]
    else}
    if APropName = TGroupProp.Propertys then
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

    //if APropName = TGroupProp.Name then
    //  Result := vBeginGroup[TDeProp.Name]
    //else
    if APropName = TGroupProp.Propertys then
      Result := vBeginGroup.Propertys.Text
    else
      Result := vBeginGroup[APropName];
  end;
end;

procedure THCEmrView.SetEditProcIndex(const Value: string);
begin
  if FEditProcIndex <> Value then
  begin
    FEditProcIndex := Value;
    CheckEditProcInfo;

    Self.ClearUndo;
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

    if (APropName <> '') and (APropValue <> '') then
    begin
      vBeginGroup[APropName] := APropValue;
      vEndGroup[APropName] := APropValue;
    end
    else
    if APropName = TGroupProp.Propertys then
    begin
      vPropertys := TStringList.Create;
      try
        vPropertys.Text := APropValue;
        for i := 0 to vPropertys.Count - 1 do
        begin
          if (vPropertys.Names[i] <> '') and (vPropertys.ValueFromIndex[i] <> '') then
          begin
            vBeginGroup[vPropertys.Names[i]] := vPropertys.ValueFromIndex[i];
            vEndGroup[vPropertys.Names[i]] := vPropertys.ValueFromIndex[i];
          end;
        end;
      finally
        vPropertys.Free;
      end;
    end;

    Result := True;
  end;
end;

function THCEmrView.GetProcAsText(const AProcIndex: string; var AText: string): Boolean;
var
  vSectionIndex, vStartNo, vEndNo: Integer;
begin
  Result := False;
  AText := '';

  if GetProcItemNo(AProcIndex, vSectionIndex, vStartNo, vEndNo) then
  begin
    if vEndNo > vStartNo + 1 then
      AText := GetDataDeGroupText(Sections[vSectionIndex].Page, vStartNo, vEndNo);
    //AText := Sections[vSectionIndex].Page.SaveToText(vStartNo + 1, 0,
    //  vEndNo - 1, Sections[vSectionIndex].Page.GetItemOffsetAfter(vEndNo - 1));
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
        // 选中，使用插入时删除当前数据组中的内容
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
    DataSaveLiteStream(AStream, procedure()
    begin
      vSection := Sections[vSectionIndex];
      vParaFirst := vSection.Page.Items[vStartNo].ParaFirst;
      if not vParaFirst then  // [ 后不是段首(十有八九)
        vSection.Page.Items[vStartNo].ParaFirst := True;  // 保证存的第一个是段首(危险操作通过try买保险)

      try
        vSection.Page.SaveItemToStream(AStream, vStartNo + 1, 0,
          vEndNo - 1, vSection.Page.GetItemOffsetAfter(vEndNo - 1));
      finally
        if not vParaFirst then  // 保险理赔
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
          // 选中，使用插入时删除当前数据组中的内容
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
    // 选中，使用插入时删除当前数据组中的内容
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

function THCEmrView.ScrollToProc(const AProcIndex: string): Boolean;
var
  vItemNo, vSecIndex, vEndNo: Integer;
  vPage: THCPageData;
  vPos: Integer absolute vEndNo;
begin
  Result := False;
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
    vPage.ItemSetCaretRequest(vItemNo, OffsetAfter);
    Result := True;
  end;
end;

procedure THCEmrView.DeleteAllProcMark;
var
  vPageData: THCPageData;
  i: Integer;
  vProcItem: TDeGroup;
begin
  vPageData := Self.ActiveSection.Page;
  Self.BeginUpdate;
  try
    vPageData.BeginFormat;
    try
      for i := vPageData.Items.Count - 1 downto 0 do
      begin
        if vPageData.Items[i] is TDeGroup then
        begin
          vProcItem := vPageData.Items[i] as TDeGroup;
          if vProcItem.IsProc then
          begin
            if vProcItem.IsProcEnd then
              vPageData.Items.Delete(i)
            else
            begin
              if i < vPageData.Items.Count - 1 then
                vPageData.Items[i + 1].ParaFirst := True;

              vPageData.Items.Delete(i);
            end;
          end;
        end;
      end;
    finally
      vPageData.EndFormat(False);
    end;

    Self.FormatData;
  finally
    Self.EndUpdate;
  end;
end;
{$ENDIF}

function THCEmrView.GetCaretDeGroupProperty(const APropName: string): string;
var
  vTopData, vPage: THCViewData;
  vDomain: THCDomainInfo;
begin
  Result := '';
  vTopData := Self.ActiveSectionTopLevelData as THCViewData;
  vDomain := vTopData.ActiveDomain;
  if vDomain.BeginNo >= 0 then
  begin
    if APropName = TGroupProp.Propertys then
      Result := (vTopData.Items[vDomain.BeginNo] as TDeGroup).Propertys.Text
    else
      Result := (vTopData.Items[vDomain.BeginNo] as TDeGroup)[APropName];
  end;
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

  if AForward then  // 从AStartNo往前找
  begin
    for i := AStartNo downto 0 do  // 找结尾ItemNo
    begin
      if CheckDeGroupEnd(AData, i, ADeIndex) then
      begin
        vEndNo := i;
        Break;
      end;
    end;

    if vEndNo >= 0 then  // 再往前找起始ItemNo
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
  else  // 从AStartNo往后找
  begin
    for i := AStartNo to AData.Items.Count - 1 do  // 找起始ItemNo
    begin
      if CheckDeGroupStart(AData, i, ADeIndex) then
      begin
        vBeginNo := i;
        Break;
      end;
    end;

    if vBeginNo >= 0 then  // 找结尾ItemNo
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

procedure THCEmrView.GetDataDeGroupTree(const AIndex: string; const AData: THCViewData;
  const ABeginNo, AEndNo: Integer; const ADomainNode: THCDomainNode);
var
  vItemTraverse: THCItemTraverse;
  i, vStartNo, vEndNo: Integer;
  vDomainStack: TDomainStack;
  vDomainNode: THCDomainNode;
begin
  vDomainStack := TDomainStack.Create;
  vDomainStack.Push(ADomainNode);
  vItemTraverse := THCItemTraverse.Create;
  try
    vItemTraverse.Areas := [saPage];
    vItemTraverse.Process := procedure (const AViewData: THCCustomData; const AItemNo,
      ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean)
    begin
      if (AViewData.Items[AItemNo] is TDeGroup) and ((AViewData.Items[AItemNo] as TDeGroup).Index = AIndex) then
      begin
        if THCDomainItem.IsBeginMark(AViewData.Items[AItemNo]) then
        begin
          vDomainNode := vDomainStack.Peek;
          vDomainNode := vDomainNode.AppendChild;
          vDomainNode.Data := AViewData;
          vDomainNode.BeginNo := AItemNo;
          vDomainStack.Push(vDomainNode);
        end
        else
        begin
          vDomainNode := vDomainStack.Pop;
          vDomainNode.EndNo := AItemNo;
        end;
      end;
    end;

    for i := ABeginNo to AEndNo do
    begin
      vItemTraverse.Process(AData, i, vItemTraverse.Tag, vItemTraverse.DomainStack, vItemTraverse.Stop);
      if not vItemTraverse.Stop then
      begin
        if AData.Items[i].StyleNo < THCStyle.Null then
          (AData.Items[i] as THCCustomRectItem).TraverseItem(vItemTraverse);
      end;
    end;
  finally
    FreeAndNil(vItemTraverse);
    FreeAndNil(vDomainStack);
  end;
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

procedure THCEmrView.InsertEmrTraceItem(const AText: string; const AAdd: Boolean = True);
var
  vEmrTraceItem: TDeItem;
begin
  // 插入添加痕迹元素
  vEmrTraceItem := TDeItem.CreateByText(AText);
  if Self.CurStyleNo < THCStyle.Null then
    vEmrTraceItem.StyleNo := 0
  else
    vEmrTraceItem.StyleNo := Self.CurStyleNo;

  vEmrTraceItem.ParaNo := Self.CurParaNo;
  if AAdd then
    vEmrTraceItem.TraceStyles := [TDeTraceStyle.cseAdd]
  else
    vEmrTraceItem.TraceStyles := [TDeTraceStyle.cseDel];

  Self.InsertItem(vEmrTraceItem);
end;

procedure THCEmrView.KeyDown(var Key: Word; Shift: TShiftState);
var
  vData: THCRichData;
  vText, vCurTraceAdd, vCurTraceAddL, vCurTraceDel, vCurTraceDelL: string;
  vStyleNo, vParaNo: Integer;
  vDeItem: TDeItem;
  vCurItem: THCCustomItem;
  vCurTraceStyles: TDeTraceStyles;
  vStream: TMemoryStream;
begin
  if FTrace then  // 留痕
  begin
    if IsKeyDownEdit(Key) then
    begin
      if CanNotEdit then Exit;

      vText := '';
      vCurTraceAdd := '';
      vCurTraceAddL := '';
      vCurTraceDel := '';
      vCurTraceDelL := '';

      vStyleNo := THCStyle.Null;
      vParaNo := THCStyle.Null;
      vCurTraceStyles := [];

      vData := Self.ActiveSectionTopLevelData as THCRichData;
      if vData.SelectExists then
      begin
        vStream := TMemoryStream.Create;
        try
          Self.SaveSelectToStream(vStream);
          Self.BeginUpdate();
          try
            Self.UndoGroupBegin;
            try
              inherited KeyDown(Key, Shift);
              vStream.Position := 0;
              FInsertTraceStream := True;
              Self.InsertLiteStream(vStream);
            finally
              FInsertTraceStream := False;
              Self.UndoGroupEnd;
            end;
          finally
            Self.EndUpdate;
          end;
        finally
          FreeAndNil(vStream);
        end;

        //Self.DisSelect;
        Exit;
      end;

      if vData.SelectInfo.StartItemNo < 0 then Exit;

      if vData.Items[vData.SelectInfo.StartItemNo].StyleNo < THCStyle.Null then
      begin
        if vData.SelectInfo.StartItemOffset = OffsetBefor then  // 在最前面
        begin
          if Key = VK_BACK then  // 回删
          begin
            if vData.SelectInfo.StartItemNo = 0 then
              Exit  // 第一个最前面则不处理
            else  // 不是第一个最前面
            begin
              vData.SelectInfo.StartItemNo := vData.SelectInfo.StartItemNo - 1;
              vData.SelectInfo.StartItemOffset := vData.Items[vData.SelectInfo.StartItemNo].Length;
              Self.KeyDown(Key, Shift);
            end;
          end
          else
          if Key = VK_DELETE then  // 后删
          begin
            vData.SelectInfo.StartItemOffset := OffsetAfter;
            //Self.KeyDown(Key, Shift);
          end
          else
            inherited KeyDown(Key, Shift);
        end
        else
        if vData.SelectInfo.StartItemOffset = OffsetAfter then  // 在最后面
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

      // 取光标处的文本
      with vData do
      begin
        if Key = VK_BACK then  // 回删
        begin
          if (SelectInfo.StartItemNo = 0) and (SelectInfo.StartItemOffset = 0) then  // 第一个最前面则不处理
            Exit
          else  // 不是第一个最前面
          if SelectInfo.StartItemOffset = 0 then  // 最前面，移动到前一个最后面处理
          begin
            if Items[SelectInfo.StartItemNo].Text <> '' then  // 当前行不是空行
            begin
              SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
              SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;
              Self.KeyDown(Key, Shift);
            end
            else  // 空行不留痕直接默认处理
              inherited KeyDown(Key, Shift);

            Exit;
          end
          else  // 不是第一个Item，也不是在Item最前面
          if Items[SelectInfo.StartItemNo] is TDeItem then  // 文本
          begin
            vDeItem := Items[SelectInfo.StartItemNo] as TDeItem;
            vText := vDeItem.SubString(SelectInfo.StartItemOffset, 1);
            vStyleNo := vDeItem.StyleNo;
            vParaNo := vDeItem.ParaNo;
            vCurTraceStyles := vDeItem.TraceStyles;
            vCurTraceAdd := vDeItem[TDeProp.TraceAdd];
            vCurTraceAddL := vDeItem[TDeProp.TraceAddLevel];
            vCurTraceDel := vDeItem[TDeProp.TraceDel];
            vCurTraceDelL := vDeItem[TDeProp.TraceDelLevel];
          end;
        end
        else
        if Key = VK_DELETE then  // 后删
        begin
          if (SelectInfo.StartItemNo = Items.Count - 1)
            and (SelectInfo.StartItemOffset = Items[Items.Count - 1].Length)
          then  // 最后一个最后面则不处理
            Exit
          else  // 不是最后一个最后面
          if SelectInfo.StartItemOffset = Items[SelectInfo.StartItemNo].Length then  // 最后面，移动到后一个最前面处理
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
            SelectInfo.StartItemOffset := 0;
            Self.KeyDown(Key, Shift);

            Exit;
          end
          else  // 不是最后一个Item，也不是在Item最后面
          if Items[SelectInfo.StartItemNo] is TDeItem then  // 文本
          begin
            vDeItem := Items[SelectInfo.StartItemNo] as TDeItem;
            vText := vDeItem.SubString(SelectInfo.StartItemOffset + 1, 1);
            vStyleNo := vDeItem.StyleNo;
            vParaNo := vDeItem.ParaNo;
            vCurTraceStyles := vDeItem.TraceStyles;
            vCurTraceAdd := vDeItem[TDeProp.TraceAdd];
            vCurTraceAddL := vDeItem[TDeProp.TraceAddLevel];
            vCurTraceDel := vDeItem[TDeProp.TraceDel];
            vCurTraceDelL := vDeItem[TDeProp.TraceDelLevel];
          end;
        end;
      end;

      // 删除掉的内容以痕迹的形式插入
      Self.BeginUpdate;
      try
        inherited KeyDown(Key, Shift);

        if FTrace and (vText <> '') then  // 有删除的内容
        begin
          if (TDeTraceStyle.cseAdd in vCurTraceStyles) and (vCurTraceAdd = '') then Exit;  // 新添加未生效痕迹可以直接删除

          // 创建删除字符对应的Item
          vDeItem := TDeItem.CreateByText(vText);
          vDeItem.StyleNo := vStyleNo;  // Style.CurStyleNo;
          vDeItem.ParaNo := vParaNo;  // Style.CurParaNo;
          vDeItem.TraceStyles := vCurTraceStyles;
          vDeItem[TDeProp.TraceAddLevel] := vCurTraceAddL;
          vDeItem[TDeProp.TraceAdd] := vCurTraceAdd;

          if (TDeTraceStyle.cseDel in vCurTraceStyles) and (vCurTraceDel = '') then  // 原来是删除未生效痕迹
            vDeItem.TraceStyles := vDeItem.TraceStyles - [TDeTraceStyle.cseDel]  // 取消删除痕迹
          else  // 生成删除痕迹
          begin
            vDeItem.TraceStyles := vDeItem.TraceStyles + [TDeTraceStyle.cseDel];
            vDeItem[TDeProp.TraceDelLevel] := vCurTraceDelL;
            vDeItem[TDeProp.TraceDel] := vCurTraceDel;
          end;

          // 插入删除痕迹Item
          vCurItem := vData.Items[vData.SelectInfo.StartItemNo];
          if vData.SelectInfo.StartItemOffset = 0 then  // 在Item最前面
          begin
            if vDeItem.CanConcatItems(vCurItem) then // 可以合并
            begin
              vCurItem.Text := vDeItem.Text + vCurItem.Text;

              if Key = VK_DELETE then  // 后删
                vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset + 1;

              Self.ActiveSection.ReFormatActiveItem;
            end
            else  // 不能合并
            begin
              if (vData.SelectInfo.StartItemNo = 0) and (vCurItem.Length = 0) then  // 当前是空行
                vDeItem.ParaFirst := False
              else
              begin
                vDeItem.ParaFirst := vCurItem.ParaFirst;
                vCurItem.ParaFirst := False;
              end;

              Self.InsertItem(vDeItem);
              if Key = VK_BACK then  // 回删
                vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset - 1;
            end;
          end
          else
          if vData.SelectInfo.StartItemOffset = vCurItem.Length then  // 在Item最后面
          begin
            if vCurItem.CanConcatItems(vDeItem) then // 可以合并
            begin
              vCurItem.Text := vCurItem.Text + vDeItem.Text;

              if Key = VK_DELETE then  // 后删
                vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset + 1;

              Self.ActiveSection.ReFormatActiveItem;
            end
            else  // 不可以合并
            begin
              Self.InsertItem(vDeItem);
              if Key = VK_BACK then  // 回删
                vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset - 1;
            end;
          end
          else  // 在Item中间
          begin
            Self.InsertItem(vDeItem);
            if Key = VK_BACK then  // 回删
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
begin
  if IsKeyPressWant(Key) then
  begin
    if CanNotEdit then Exit;

    if FTrace then
    begin
      MakeSelectTraceIf;
      InsertEmrTraceItem(Key);

      Exit;
    end;

    inherited KeyPress(Key);
  end;
end;

procedure THCEmrView.MakeSelectTraceIf;
var
  vData: THCCustomData;
  vText: string;
begin
  vData := Self.ActiveSectionTopLevelData;
  if vData.SelectInfo.StartItemNo < 0 then Exit;
  if vData.SelectExists then
  begin
    Self.UndoGroupBegin;
    try
      vText := vData.GetSelectText;
      Self.DeleteSelected;
      InsertEmrTraceItem(vText, False);
    finally
      Self.UndoGroupEnd;
    end;
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

    //ASection.DataAction(vData, function(): Boolean
    //begin
      // 选中，使用插入时删除当前数据组中的内容
      vData.SetSelectBound(vStartNo, OffsetAfter, vEndNo, OffsetBefor);
      FIgnoreAcceptAction := True;
      try
        Self.InsertStream(AStream);
        //vData.InsertStream(AStream);
      finally
        FIgnoreAcceptAction := False;
      end;
    //  Result := True;
    //end);

    {$IFDEF PROCSERIES}
    CheckCaretProcInfo;
    {$ENDIF}
  end;
end;

procedure THCEmrView.SetDeGroupByStream(const ASection: THCSection;
  const AArea: TSectionArea; const AIndex: string; const AStream: TStream; const AStartLast: Boolean = True);
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
    GetDataDeGroupItemNo(vData, AIndex, True, vStartNo, vEndNo)
  end
  else
  begin
    {$IFDEF PROCSERIES}
    if FEditProcIndex <> '' then
      vStartNo := FEditProcInfo.BeginNo
    else
    {$ENDIF}
    GetDataDeGroupItemNo(vData, AIndex, False, vStartNo, vEndNo);
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

    DataLoadLiteStream(AStream, procedure(const AFileVersion: Word; const AStyle: THCStyle)
    begin
      Self.BeginUpdate;
      try
        vData.SetSelectBound(vStartNo, OffsetAfter, vEndNo, OffsetBefor);
        FIgnoreAcceptAction := True;
        try
          Self.Style.States.Include(hosDomainWholeReplace);
          try
            ASection.InsertStream(AStream, AStyle, AFileVersion);
          finally
            Self.Style.States.Exclude(hosDomainWholeReplace);
          end;
        finally
          FIgnoreAcceptAction := False;
        end;
      finally
        Self.EndUpdate;
      end;
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
      // 选中，使用插入时删除当前数据组中的内容
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

function THCEmrView.SetDeGroupProperty(const AIndex, APropName,
  APropValue: string): Boolean;
var
  vSectionIndex, vStartNo, vEndNo: Integer;
  vData: THCViewData;
begin
  Result := GetDeGroupItemNo(AIndex, vData, vSectionIndex, vStartNo, vEndNo);
  if Result then
  begin
    (vData.Items[vStartNo] as TDeGroup)[APropName] := APropValue;
    (vData.Items[vEndNo] as TDeGroup)[APropName] := APropValue;
  end;
end;

function THCEmrView.SetDeGroupTreeByStream(const ADomainNode: THCDomainNode;
  const AStream: TStream; const AStyle: THCStyle; const AFileVersion: Word;
  const AWhich: Integer): Boolean;
var
  i, vPosition: Integer;
  vDomainInfo: THCDomainInfo;
begin
  Result := False;
  if ADomainNode.Childs.Count = 0 then Exit;

  Self.BeginUpdate;
  try
    FIgnoreAcceptAction := True;
    try
      Self.Style.States.Include(hosDomainWholeReplace);
      try
        if AWhich = 0 then
        begin
          vDomainInfo := ADomainNode.Childs[0];
          (vDomainInfo.Data as THCViewData).SetSelectBound(vDomainInfo.BeginNo, OffsetAfter, vDomainInfo.EndNo, OffsetBefor);
          vDomainInfo.Data.InsertStream(AStream, AStyle, AFileVersion);
        end
        else
        if AWhich = 1 then
        begin
          vDomainInfo := ADomainNode.Childs.Last;
          (vDomainInfo.Data as THCViewData).SetSelectBound(vDomainInfo.BeginNo, OffsetAfter, vDomainInfo.EndNo, OffsetBefor);
          vDomainInfo.Data.InsertStream(AStream, AStyle, AFileVersion);
        end
        else
        begin
          vPosition := AStream.Position;
          for i := ADomainNode.Childs.Count - 1 downto 0 do
          begin
            AStream.Position := vPosition;
            vDomainInfo := ADomainNode.Childs[i];
            (vDomainInfo.Data as THCViewData).SetSelectBound(vDomainInfo.BeginNo, OffsetAfter, vDomainInfo.EndNo, OffsetBefor);
            vDomainInfo.Data.InsertStream(AStream, AStyle, AFileVersion);
          end;
        end;
      finally
        Self.Style.States.Exclude(hosDomainWholeReplace);
      end;
    finally
      FIgnoreAcceptAction := False;
    end;

    Self.FormatData;
  finally
    Self.EndUpdate;
  end;

  Result := True;
end;

procedure THCEmrView.SaveSelectToStream(const AStream: TStream);
begin
  DataSaveLiteStream(AStream, procedure()
  begin
    Self.Style.States.Include(THCState.hosCopying);  // 去掉复制了一半的数据组
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

procedure THCEmrView.SaveToLiteStream(const AStream: TStream);
begin
  DataSaveLiteStream(AStream, procedure()
  var
    vPageData: THCPageData;
  begin
    vPageData := Self.ActiveSection.Page;
    Self.Style.States.Include(THCState.hosCopying);
    try
      vPageData.SaveItemToStream(AStream, 0, 0, vPageData.Items.Count - 1, vPageData.GetItemOffsetAfter(vPageData.Items.Count - 1));
    finally
      Self.Style.States.Exclude(THCState.hosCopying);
    end;
  end);
end;

function THCEmrView.ScrollToItem(const AItem: THCCustomItem): Boolean;
var
  vItemTraverse: THCItemTraverse;
  vResult: Boolean;
  vTop, vSecIndex: Integer;
begin
  Result := False;
  vTop := -1;

  vItemTraverse := THCItemTraverse.Create;
  try
    vItemTraverse.Tag := 0;
    vItemTraverse.Areas := [saPage];//, saHeader, saFooter];
    vItemTraverse.Process := procedure (const AData: THCCustomData; const AItemNo,
      ATag: Integer; const ADomainStack: TDomainStack; var AStop: Boolean)
    begin
      if Pointer(AData.Items[AItemNo]) = Pointer(AItem) then
      begin
        vTop := (AData as THCRichData).GetDrawItemFormatTop(AData.Items[AItemNo].FirstDItemNo);
        vSecIndex := vItemTraverse.SectionIndex;
        AStop := True;
      end;
    end;

    Self.TraverseItem(vItemTraverse);
  finally
    vItemTraverse.Free;
  end;

  if vTop >= 0 then
  begin
    vTop := Self.Sections[vSecIndex].PageDataFormtToFilmCoord(vTop);
    vTop := vTop + Self.GetSectionTopFilm(vSecIndex);
    Self.VScrollBar.Position := vTop;
    Result := True;
  end;
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

function THCEmrView.SetCaretDeGroupProperty(const APropName,
  APropValue: string): Boolean;
var
  vTopData, vPage: THCViewData;
  vDomain: THCDomainInfo;
begin
  Result := False;
  vTopData := Self.ActiveSectionTopLevelData as THCViewData;
  vDomain := vTopData.ActiveDomain;
  if vDomain.EndNo > 0 then
  begin
    if APropName = TGroupProp.Propertys then
    begin
      (vTopData.Items[vDomain.BeginNo] as TDeGroup).Propertys.Text := APropValue;
      (vTopData.Items[vDomain.EndNo] as TDeGroup).Propertys.Text := APropValue;
    end
    else
    begin
      (vTopData.Items[vDomain.BeginNo] as TDeGroup)[APropName] := APropValue;
      (vTopData.Items[vDomain.EndNo] as TDeGroup)[APropName] := APropValue;
    end;

    Result := True;
  end;
end;

function THCEmrView.SetCaretToDeGroupStart(const AIndex: string): Boolean;
var
  vSectionIndex, vStartNo, vEndNo: Integer;
  vData: THCViewData;
begin
  Result := GetDeGroupItemNo(AIndex, vData, vSectionIndex, vStartNo, vEndNo);
  if Result then
  begin
    vData.ItemSetCaretRequest(vStartNo, OffsetAfter);
    {$IFDEF PROCSERIES}
    CheckCaretProcInfo;
    {$ENDIF}
  end;
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
          if ADeGroupEndNo - ADeGroupStartNo > 1 then  // 中间有内容
            AData.DeleteItems(ADeGroupStartNo + 1,  ADeGroupEndNo - 1, False)
          else
            AData.SetSelectBound(ADeGroupStartNo, OffsetAfter, ADeGroupStartNo, OffsetAfter);
          // 这里参考设置选项内容为流的地方处理格式化，是不是更好
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
  const ADeGroupStartNo, ADeGroupEndNo: Integer; const AText: string);
var
  vGroupBeg, vGroupEnd: Integer;
begin
  if ADeGroupEndNo < 0 then
    vGroupEnd := AData.GetDomainAnother(ADeGroupStartNo)
  else
    vGroupEnd := ADeGroupEndNo;

  if vGroupEnd > ADeGroupStartNo then
    vGroupBeg := ADeGroupStartNo
  else
  begin
    vGroupBeg := vGroupEnd;
    vGroupEnd := ADeGroupStartNo;
  end;

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
  APropValue: string; const AWhich: Integer = 0): Boolean;
var
  vItemTraverse: THCItemTraverse;
  vItem: THCCustomItem;
  vResult, vReformat: Boolean;
  i: Integer;
  vPageData: THCPageData;
  vDomainInfo: THCDomainNode;
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
      if not AData.CanEdit then
      begin
        AStop := True;
        Exit;
      end;

      vItem := AData.Items[AItemNo];
      if (vItem is TDeItem) and ((vItem as TDeItem)[TDeProp.Index] = ADeIndex) then
      begin
        if APropName = 'Text' then
        begin
          if APropValue <> '' then
          begin
            vItem.Text := APropValue;
            (vItem as TDeItem).AllocValue := True;
            AData.Change;
          end;

          vReformat := True;
        end
        else
        if APropName = 'Propertys' then
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
                  AData.Change;
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
        if AWhich = 0 then
          AStop := True;
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
        if AWhich = 0 then
          AStop := True;
      end;
    end;

    {$IFDEF PROCSERIES}
    if FEditProcIndex <> '' then
    begin
      vItemTraverse.SectionIndex := FEditProcInfo.SectionIndex;
      vPageData := Sections[FEditProcInfo.SectionIndex].Page;
      for i := FEditProcInfo.BeginNo to FEditProcInfo.EndNo do
      begin
        if vItemTraverse.Stop then
          Break;

        if vPageData.Items[i] is THCDomainItem then
        begin
          if THCDomainItem.IsBeginMark(vPageData.Items[i]) then
          begin
            vDomainInfo := THCDomainNode.Create;
            vPageData.GetDomainFrom(i, OffsetAfter, vDomainInfo);
            vItemTraverse.DomainStack.Push(vDomainInfo);
          end
          else
            vItemTraverse.DomainStack.Pop;
        end;

        vItemTraverse.Process(vPageData, i, vItemTraverse.Tag, vItemTraverse.DomainStack, vItemTraverse.Stop);
        if not vItemTraverse.Stop then
        begin
          if vPageData.Items[i].StyleNo < THCStyle.Null then
            (vPageData.Items[i] as THCCustomRectItem).TraverseItem(vItemTraverse);
        end;
      end;
    end
    else
    {$ENDIF}
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
          if TDeTraceStyle.cseDel in vDeItem.TraceStyles then
            vDeItem.Visible := not FHideTrace;

          if FTraceInfoAnnotate and (vDeItem.TraceStyles <> []) then
          begin
            if FHideTrace then
              Self.AnnotatePre.RemoveDataAnnotate(nil)
            else
              Self.AnnotatePre.InsertDataAnnotate(nil);
          end;
        end
        else
        if AData.Items[AItemNo] is THCDataItem then
          (AData.Items[AItemNo] as THCDataItem).FormatDirty;
      end;

      Self.TraverseItem(vItemTraverse);

    finally
      vItemTraverse.Free;
    end;

    Self.FormatData;
    {if FHideTrace then  // 隐藏显示痕迹
    begin
      if not Self.ReadOnly then
        Self.ReadOnly := True;
    end;}
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

procedure THCEmrView.SetValue(const Key, Value: string);
begin
  HCSetProperty(FPropertys, Key, Value);
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
        if Application.Handle <> 0 then  // 在exe中运行
        begin
          if Message.WParam <> VK_RETURN then
          begin
            ShiftState := KeyDataToShiftState(TWMKey(Message).KeyData);
            Self.KeyDown(TWMKey(Message).CharCode, ShiftState);
            Exit;
          end;
        end
        else  // 在浏览器中运行
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
