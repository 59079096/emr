unit CFGrid;

interface

uses
  Windows, Classes, Controls, Graphics, Generics.Collections,
  CFControl, CFScrollBar;

type
  TCFRow = class;

  TCFColStates = set of (ccsSelected, ccsFocused);
  TCFColStyle = (ccsText, ccsCheckBox, ccsEdit, ccsCombobox, ccsButtonEdit, ccsGridEdit);

  TCFCol = class(TPersistent)
  private
    FRow: TCFRow;
    FColIndex: Integer;
    FText: string;
    FStates: TCFColStates;

    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);

    procedure SetText(const Value: string);
  public
    constructor Create(const ARow: TCFRow; const AIndex: Integer);
    procedure Draw(const ACanvas: TCanvas; const ALeft, ATop, AHeight, AWidth: Integer;
      const AStyle: TCFColStyle);
    //
    property Text: string read FText write SetText;
    property States: TCFColStates read FStates write FStates default [];
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
  end;

  TCFColInfo = class(TObject)
  private
    FWidth: Integer;
    FReadOnly: Boolean;
    FStyle: TCFColStyle;
  public
    constructor Create;
    property Width: Integer read FWidth write FWidth;
    property Style: TCFColStyle read FStyle write FStyle;
  end;

  TCFTitleRow = class;

  TTitleCol = class(TPersistent)
  private
    FText: string;
  public
    constructor Create(const ATitleRow: TCFTitleRow);
    procedure Draw(const ACanvas: TCanvas; const ALeft, ATop, AHeight, AWidth: Integer);
    property Text: string read FText write FText;
  end;

  TCFGrid = class;

  TCFRow = class(TList)
  private
    FGrid: TCFGrid;
    /// <summary> ��ȡ���е��е����� </summary>
    function Get(Index: Integer): TCFCol;

    /// <summary> ���ø��е��е����� </summary>
    procedure Put(Index: Integer; const Value: TCFCol);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(const AGrid: TCFGrid; const AColCount: Cardinal);

    /// <summary> ������ </summary>
    procedure Draw(const ACanvas: TCanvas; const ALeft, ATop, AStartCol, AEndCol: Integer);
    property Items[Index: Integer]: TCFCol read Get write Put; default;
  end;

  TCFTitleRow = class(TList)
  private
    FGrid: TCFGrid;
    /// <summary> ��ȡ���е��е����� </summary>
    function Get(Index: Integer): TTitleCol;

    /// <summary> ���ø��е��е����� </summary>
    procedure Put(Index: Integer; const Value: TTitleCol);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(const AGrid: TCFGrid; const AColCount: Cardinal);

    /// <summary> ������ </summary>
    procedure Draw(const ACanvas: TCanvas; const ALeft, ATop, AStartCol, AEndCol: Integer);
    property Items[Index: Integer]: TTitleCol read Get write Put; default;
  end;

  TCFRows = class(TObjectList<TCFRow>)
  private
    FOnCountChange: TNotifyEvent;
  protected
    procedure Notify(const Value: TCFRow; Action: TCollectionNotification); override;
  public
    property OnCounthCange: TNotifyEvent read FOnCountChange write FOnCountChange;
  end;

  TCFGridOption = (cgoRowSizing, cgoColSizing, cgoIndicator, cgoRowSelect, cgoShowSelect);
  TCFGridOptions = set of TCFGridOption;

  TOnSelectCell = procedure(Sender: TObject; ARow, ACol: Integer{; var CanSelect: Boolean}) of object;
  TOnTitleClick = procedure(Sender: TObject; ATitleRow, ATitleCol: Integer) of object;
  TOnCellDraw = procedure(Sender: TObject; const ARow, ACol: Cardinal;
    const ACanvas: TCanvas; const ALeft, ATop: Integer) of object;
  TOnCellBeforeEdit = procedure(Sender: TObject; const ARow, ACol: Cardinal; var ACancel: Boolean; var AColStyle: TCFColStyle) of object;
  TOnCellAfterEdit = procedure(Sender: TObject; const ARow, ACol: Cardinal; const ANewValue: string; var ACancel: Boolean) of object;

  TCFGrid = class(TCFTextControl)
  private
    FColInfos: TObjectList<TCFColInfo>;
    FTitleRow: TCFTitleRow;
    FVScrollBar, FHScrollBar: TCFScrollBar;
    FEditControl: TCFTextControl;
    FMouseDownControl: TCFCustomControl;

    FDefaultColWidth,
    FRowHeight: Integer;
    /// <summary> ֻ�� </summary>
    FReadOnly: Boolean;

    // �϶��ı��п�
    FResizeCol,  // �϶��ı��п�ʱ��Ӧ����
    FResizeX     // ��¼�϶�ʱXλ��
      : Integer;
    FMovePt: TPoint;  // ����м�����ƽ��ʱ��ʼ����
    FIndicatorWidth: Integer;
    FColResizing: Boolean;
    FOptions: TCFGridOptions;
    FRowIndex, FColIndex: Integer;

    FOnSelectCell: TOnSelectCell;
    FOnTitleClick: TOnTitleClick;
    FOnCellDraw: TOnCellDraw;
    FOnCellBeforeEdit: TOnCellBeforeEdit;
    FOnCellAfterEdit: TOnCellAfterEdit;
    /// <summary> ��ȡ���ݵ��ܿ�� </summary>
    /// <returns>���ݵ��ܿ��</returns>
    function GetDataWidth: Integer;

    /// <summary> ��ȡչʾ���ݵĿ�� </summary>
    /// <returns>��չʾ���ݵ��ܿ��</returns>
    function GetDataDisplayWidth: Integer;

    /// <summary> ��ȡչʾ���ݵĸ߶� </summary>
    /// <returns>չʾ���ݸ߶�</returns>
    function GetDataDisplayHeight: Integer;

    /// <summary> ��ȡչʾ���ݵ��ұ� </summary>
    /// <returns>���ݵ��ұ�</returns>
    function GetDataDisplayRight: Integer;

    /// <summary> ��ȡչʾ���ݵĵײ� </summary>
    /// <returns>���ݵĵײ�</returns>
    function GetDataDisplayBottom: Integer;

    /// <summary> ��ȡ��Ԫ������� </summary>
    /// <param name="ARow">��</param>
    /// <param name="ACol">��</param>
    /// <returns>��Ԫ�������</returns>
    function GetCells(ARow, ACol: Integer): string;
    function GetRow(ARow: Integer): TCFRow;

    /// <summary> ��ȡ�еı��� </summary>
    /// <param name="ACol">��</param>
    /// <returns>�еı���</returns>
    function GetTitleText(ACol: Integer): string;

    /// <summary> ��ֱ�������¼� </summary>
    procedure OnVScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);

    /// <summary> ˮƽ�������¼� </summary>
    procedure OnHScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);

    /// <summary> ��ȡչʾ���ݵ���ʼ�кͽ����� </summary>
    /// <param name="AStartRow">��ʼ��</param>
    /// <param name="AEndRow">������</param>
    /// <param name="ATopOffset">ƫ����</param>
    procedure GetFirstRowDisplay(var AStartRow, AEndRow, ATopOffset: Integer);

    /// <summary> ��ȡչʾ���ݵ���ʼ�кͽ����� </summary>
    /// <param name="AStartCol">��ʼ��</param>
    /// <param name="AEndCol">������</param>
    /// <param name="ALeftOffset">����ƫ����</param>
    procedure GetFirstColDisplay(var AStartCol, AEndCol, ALeftOffset: Integer);

    /// <summary> ��ȡ����ĸ߶� </summary>
    /// <returns>����߶�</returns>
    function GetTitleHeight: Integer;

    /// <summary> ����Ԫ��ֵ </summary>
    /// <param name="ARow">��Ԫ����</param>
    /// <param name="ACol">��Ԫ����</param>
    /// <param name="Value">��Ӧֵ</param>
    procedure SetCells(ARow, ACol: Integer; const Value: string);

    /// <summary> �����⸳ֵ </summary>
    /// <param name="ACol">��</param>
    /// <param name="Value">�����и�ֵ</param>
    procedure SetTitleText(ATitleCol: Integer; const Value: string);

    procedure SetOptions(Value: TCFGridOptions);

    /// <summary> �����ǹ�������λ�� </summary>
    procedure CalcScrollBarPosition;

    /// <summary> �����Ƿ�Ҫ��ʾ������ </summary>
    procedure CheckScrollBarVisible;

    procedure ShowEditControl(const ARow, ACol: Cardinal; const AColStyle: TCFColStyle);
    procedure DoOnCellBeforeEdit(const ARow, ACol: Cardinal);
    procedure DoOnCellAfterEdit(const ARow, ACol: Cardinal);
    procedure DoOnEditControlKeyPress(Sender: TObject; var Key: Char);
    procedure DoRowCountChange(Sender: TObject);
  protected
    FRows: TCFRows;
    function DoRowDrawBefor(const ARow, ALeft, ATop: Integer; const ACanvas: TCanvas;
      var AUserDrawed: Boolean): Boolean; virtual;
    function DoRowDrawAfter(const ARow, ALeft, ATop: Integer; const ACanvas: TCanvas): Boolean; virtual;
    /// <summary> ���� </summary>
    procedure DrawControl(ACanvas: TCanvas); override;

    procedure AdjustBounds; override;
    /// <summary> ����ֻ������ </summary>
    /// <param name="Value">True��ֻ��</param>
    procedure SetReadOnly(Value: Boolean);

    procedure SizeChange;

    function GetColWidth(ACol: Integer): Integer;

    /// <summary> �����еĿ�� </summary>
    /// <param name="ACol">ָ����</param>
    /// <param name="AWidth">�п��</param>
    procedure SetColWidth(ACol, AWidth: Integer);

    function GetColStyle(ACol: Integer): TCFColStyle;
    procedure SetColStyle(ACol: Integer; const AStyle: TCFColStyle);
    //
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
        MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AOwner: TComponent; const ARowCount, AColCount: Cardinal);
    destructor Destroy; override;
    function GetRowCount: Cardinal;
    procedure SetRowCount(Value: Cardinal);
    function GetColCount: Cardinal;
    procedure SetColCount(Value: Cardinal);

    /// <summary> ������ָ���� </summary>
    /// <param name="ARowIndex">�к�</param>
    procedure ScrollToRow(const ARowIndex: Cardinal);

    /// <summary> ��ȡָ��������к��� </summary>
    /// <param name="DataX">��������X</param>
    /// <param name="DataY">��������Y</param>
    /// <param name="ARow">����������</param>
    /// <param name="ACol">����������</param>
    procedure GetDataCellAt(const ADataX, ADataY: Integer; var ARow, ACol: Integer);

    procedure GetTitleCellAt(const ATitleX, ATitleY: Integer; var ATitleRow, ATitleCol: Integer);

    /// <summary> ��ȡ��Ԫ�������(��Ӧ������) </summary>
    /// <param name="ARow">��Ԫ����</param>
    /// <param name="ACol">��Ԫ����</param>
    /// <returns>��Ԫ������</returns>
    function GetCellClientRect(const ARow, ACol: Integer): TRect;

    /// <summary> ��ȡָ�����е����򣨴ӱ�����ָ���� </summary>
    /// <param name="ACol">������</param>
    /// <returns>������</returns>
    function GetTitleColClientRect(const ATitleCol: Integer): TRect;

    /// <summary> ��ȡ��Ԫ���������ݵ��������� </summary>
    /// <param name="ARow">��Ԫ����</param>
    /// <param name="ACol">��Ԫ����</param>
    /// <returns>��������</returns>
    function GetCellDataBoundRect(const ARow, ACol: Integer): TRect;

    /// <summary> ʹ�õ�Ԫ��Ĭ�ϵĻ��Ʒ�ʽ����ָ����Ԫ�����ݵ�ָ���Ļ����� </summary>
    /// <param name="ARow">��</param>
    /// <param name="ACol">��</param>
    /// <param name="ACanvas">����</param>
    procedure DefaultCellDraw(const ARow, ACol: Cardinal; const ACanvas: TCanvas;
      const ALeft, ATop: Integer);

    property Cells[ARow, ACol: Integer]: string read GetCells write SetCells;
    property Rows[ARow: Integer]: TCFRow read GetRow;
    property TitleText[ACol: Integer]: string read GetTitleText write SetTitleText;
    property ColWidth[ACol: Integer]: Integer read GetColWidth write SetColWidth;
    property ColStyle[ACol: Integer]: TCFColStyle read GetColStyle write SetColStyle;
    //property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
    property RowIndex: Integer read FRowIndex;
    property ColIndex: Integer read FColIndex;
  published
    property VScrollBar: TCFScrollBar read FVScrollBar;
    property HScrollBar: TCFScrollBar read FHScrollBar;
    property DefaultColWidth: Integer read FDefaultColWidth write FDefaultColWidth;
    property RowHeight: Integer read FRowHeight write FRowHeight;
    property RowCount: Cardinal read GetRowCount write SetRowCount;
    property ColCount: Cardinal read GetColCount write SetColCount;
    property Options: TCFGridOptions read FOptions write SetOptions;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property PopupMenu;

    property OnSelectCell: TOnSelectCell read FOnSelectCell write FOnSelectCell;
    property OnTitleClick: TOnTitleClick read FOnTitleClick write FOnTitleClick;
    property OnCellDraw: TOnCellDraw read FOnCellDraw write FOnCellDraw;
    property OnCellBeforeEdit: TOnCellBeforeEdit read FOnCellBeforeEdit write FOnCellBeforeEdit;
    property OnCellAfterEdit: TOnCellAfterEdit read FOnCellAfterEdit write FOnCellAfterEdit;
  end;

implementation

uses
  SysUtils, Math, CFEdit, CFButtonEdit, CFCombobox, CFGridEdit;

{ TCFGrid }

function TCFGrid.GetDataWidth: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to ColCount - 1 do
    Result := Result + FColInfos[i].Width;
end;

procedure TCFGrid.AdjustBounds;
var
  vDC: HDC;
  vNewHeight, vNewWidth: Integer;
begin
  vDC := GetDC(0);
  try
    Canvas.Handle := vDC;
    Canvas.Font := Font;
    vNewHeight := FRowHeight + GetSystemMetrics(SM_CYBORDER) * 4;
    vNewWidth := FDefaultColWidth + GetSystemMetrics(SM_CYBORDER) * 4;
    if vNewWidth < Width then
      vNewWidth := Width;

    if vNewHeight < Height then
      vNewHeight := Height;

    Canvas.Handle := 0;
  finally
    ReleaseDC(0, vDC);
  end;

  SetBounds(Left, Top, vNewWidth, vNewHeight);
end;

procedure TCFGrid.CalcScrollBarPosition;
begin
  if (not Assigned(FVScrollBar)) or (not Assigned(FHScrollBar)) then Exit;  // �����

  // ����λ��
  if BorderVisible then  // ��ʾ�߿�
  begin
    FVScrollBar.Left := Width - FVScrollBar.Width - GBorderWidth;
    FVScrollBar.Top := GBorderWidth;
    FVScrollBar.Height := Height - 2 * GBorderWidth;
    if FHScrollBar.Visible then  // ˮƽ�������
      FVScrollBar.Height := FVScrollBar.Height - FHScrollBar.Height;

    FHScrollBar.Left := GBorderWidth;
    FHScrollBar.Top := Height - FHScrollBar.Height - GBorderWidth;
    FHScrollBar.Width := Width - 2 * GBorderWidth;
    if FVScrollBar.Visible then  // ��ֱ�������
      FHScrollBar.Width := FHScrollBar.Width - FVScrollBar.Width;
  end
  else  // ����ʾ�߿�
  begin
    FVScrollBar.Left := Width - FVScrollBar.Width;
    FVScrollBar.Top := 0;
    FVScrollBar.Height := Height;
    if FHScrollBar.Visible then  // ˮƽ�������
      FVScrollBar.Height := FVScrollBar.Height - FHScrollBar.Height;

    FHScrollBar.Left := 0;
    FHScrollBar.Top := Height - FHScrollBar.Height;
    FHScrollBar.Width := Width;
    if FVScrollBar.Visible then  // ��ֱ�������
      FHScrollBar.Width := FHScrollBar.Width - FVScrollBar.Width;
  end;
end;

procedure TCFGrid.CheckScrollBarVisible;
var
  vWidth, vHeight, vHMax, vVMax: Integer;
  vVVisible, vHVisible: Boolean;
begin
  if (not Assigned(FVScrollBar)) or (not Assigned(FHScrollBar)) then Exit;  // �����

  vHMax := GetDataWidth;
  vVMax := FRows.Count * RowHeight;  // ���ݸ�
  // ��ֱ�������ݿ���ʾ�ĸ߶�
  vHeight := Height - GetTitleHeight;  // ��ȥ������
  // ˮƽ�������ݿ���ʾ�Ŀ��
  vWidth := Width;
  if cgoIndicator in FOptions then
    vWidth := vWidth - FIndicatorWidth;

  if BorderVisible then  // �߿����
  begin
    vHeight := vHeight - 2 * GBorderWidth;  // �߶ȼ�ȥ�߿�
    vWidth := vWidth - 2 * GBorderWidth;
  end;
  // �жϹ������Ƿ���ʾ
  vVVisible := False;
  vHVisible := False;
  if vVMax > vHeight then  // ��ֱ���ڸ߶�
  begin
    vVVisible := True;
    vWidth := vWidth - FVScrollBar.Width;
  end;

  if vHMax > vWidth then  // ˮƽ���ڿ��
  begin
    vHVisible := True;
    vHeight := vHeight - FHScrollBar.Height;
  end;

  // �ٴ��ж���ʾ״̬���Ծ�����2�������������κ�һ������ʾ���ƫ��
  if (not vVVisible) and (vVMax > vHeight) then  // ��ֱ���ڸ߶�
  begin
    vVVisible := True;
    vWidth := vWidth - FVScrollBar.Width;
  end;

  if (not vHVisible) and (vHMax > vWidth) then  // ˮƽ���ڿ��
  begin
    vHVisible := True;
    vHeight := vHeight - FHScrollBar.Height;
  end;

  // ���ù�������Max��Position
  FVScrollBar.Max := vVMax;
  FHScrollBar.Max := vHMax;
  if vHeight > 0 then
    FVScrollBar.PageSize := vHeight
  else
    FVScrollBar.PageSize := 0;

  if vWidth > 0 then
    FHScrollBar.PageSize := vWidth
  else
    FHScrollBar.PageSize := 0;

  // ���ù������Ŀɼ���
  FVScrollBar.Visible := vVVisible;
  FHScrollBar.Visible := vHVisible;
end;

constructor TCFGrid.Create(AOwner: TComponent);
begin
  CreateEx(AOwner, 0, 0);
end;

constructor TCFGrid.CreateEx(AOwner: TComponent; const ARowCount, AColCount: Cardinal);
begin
  inherited Create(AOwner);  // ���� CGrid
  FDefaultColWidth := 70;
  FRowHeight := 20;
  FIndicatorWidth := 20;
  FResizeCol := -1;
  FRowIndex := -1;
  FColIndex := -1;
  FColResizing := False;
  FOptions := [cgoRowSizing, cgoColSizing, cgoIndicator, cgoRowSelect, cgoShowSelect];

  FVScrollBar := TCFScrollBar.Create(Self);  // ������ֱ������
  FVScrollBar.Orientation := coVertical;  // ���ù�����Ϊ��ֱ����
  FVScrollBar.OnScroll := OnVScroll;  // �󶨹����¼�
  FVScrollBar.Width := 10;
  FVScrollBar.Parent := Self;

  FHScrollBar := TCFScrollBar.Create(Self);  // ����ˮƽ������
  FHScrollBar.Orientation := coHorizontal;  // ���ù���������Ϊˮƽ����������
  FHScrollBar.OnScroll := OnHScroll;  // �󶨹����¼�
  FHScrollBar.Height := 10;
  FHScrollBar.Parent := Self;

  FTitleRow := TCFTitleRow.Create(Self, AColCount);  // ����������
  // �����к���
  FRows := TCFRows.Create;
  FRows.OnCounthCange := DoRowCountChange;
  FColInfos := TObjectList<TCFColInfo>.Create;

  if (ARowCount = 0) and (AColCount <> 0) then
    raise Exception.Create('�쳣������Ϊ0ʱ����������Ϊ����0����ֵ��');
  if ARowCount <> 0 then
  begin
    RowCount := ARowCount;
    if AColCount <> 0 then
      ColCount := AColCount;
  end;

  // ���ô��崴��ʱ�ĳ�ʼֵ
  Width := 200;
  Height := 250;
end;

procedure TCFGrid.DefaultCellDraw(const ARow, ACol: Cardinal;
  const ACanvas: TCanvas; const ALeft, ATop: Integer);
begin
  FRows[ARow][ACol].Draw(ACanvas, ALeft, ATop, FRowHeight, FColInfos[ACol].Width,
    FColInfos[ACol].Style);
end;

destructor TCFGrid.Destroy;
begin
  FRows.Free;
  FTitleRow.Free;
  FVScrollBar.Free;
  FHScrollBar.Free;
  inherited;
end;

function TCFGrid.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  inherited;
  if WheelDelta < 0 then
  begin
    if FVScrollBar.Visible then  // ���ڹ���������
    begin
      if FVScrollBar.Position < FVScrollBar.Max - FVScrollBar.PageSize then
      begin
        FVScrollBar.Position := FVScrollBar.Position + WHEEL_DELTA;
        UpdateDirectUI;
        Result := True;
      end;
    end;
  end
  else
  begin
    if FVScrollBar.Visible then
    begin
      if FVScrollBar.Position > FVScrollBar.Min then
      begin
        FVScrollBar.Position := FVScrollBar.Position - WHEEL_DELTA;
        UpdateDirectUI;
        Result := True;
      end;
    end;
  end;
end;

procedure TCFGrid.DoOnCellAfterEdit(const ARow, ACol: Cardinal);
begin
  if FEditControl <> nil then
  begin
    Cells[ARow, ACol] := FEditControl.Text;
    SendMessage(Self.Parent.Handle, WM_CF_REMOVECONTROL, Integer(FEditControl), 0);
    FEditControl.Free;
    FEditControl := nil;
  end;
end;

procedure TCFGrid.DoOnCellBeforeEdit(const ARow, ACol: Cardinal);
var
  vCancel: Boolean;
  vColStyle: TCFColStyle;
begin
  vCancel := False;
  vColStyle := FColInfos[ACol].Style;
  if Assigned(FOnCellBeforeEdit) then
    FOnCellBeforeEdit(Self, ARow, ACol, vCancel, vColStyle);

  if not vCancel then
    ShowEditControl(ARow, ACol, vColStyle);
end;

procedure TCFGrid.DoOnEditControlKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    DoOnCellAfterEdit(FRowIndex, FColIndex);
end;

procedure TCFGrid.DoRowCountChange(Sender: TObject);
begin
  SizeChange;
end;

function TCFGrid.DoRowDrawAfter(const ARow, ALeft, ATop: Integer;
  const ACanvas: TCanvas): Boolean;
begin

end;

function TCFGrid.DoRowDrawBefor(const ARow, ALeft, ATop: Integer;
  const ACanvas: TCanvas; var AUserDrawed: Boolean): Boolean;
begin

end;

procedure TCFGrid.DrawControl(ACanvas: TCanvas);

  procedure DrawTitle(const ADrawLeft, AStartCol, AEndCol: Integer);  // ���Ʊ����б���
  var
    vRight, i: Integer;
  begin
    ACanvas.Brush.Color := GTitleBackColor;
    vRight := Math.Min(GetDataDisplayRight, GetDataWidth);
    if cgoIndicator in FOptions then
      vRight := vRight + FIndicatorWidth;

    ACanvas.FillRect(Bounds(0, 0, vRight, GetTitleHeight));

    // ���Ʊ���������
    vRight := ADrawLeft;
    for i := AStartCol to AEndCol do
    begin
      FTitleRow[i].Draw(ACanvas, vRight, 0, FRowHeight, FColInfos[i].Width);
      vRight := vRight + FColInfos[i].Width;
    end;
  end;

  procedure DrawBorder;  // ���Ʊ߿�
  begin
    if BorderVisible then
    begin
      with ACanvas do
      begin
        Pen.Color := GBorderColor;
        MoveTo(0, 0);
        LineTo(Width - 1, 0);
        LineTo(Width - 1, Height - 1);
        LineTo(0, Height - 1);
        LineTo(0, 0);
      end;
    end;
  end;

var
  i, j, vDrawTop, vDrawLeft, vTop, vLeft, vDisplayRight: Integer;
  vStartRow, vEndRow, vStartCol, vEndCol: Integer;
  vUserDrawed: Boolean;
begin
  inherited DrawControl(ACanvas);

  // ����ؼ���������
  vDisplayRight := Width;
  if FVScrollBar.Visible then  // ��ֱ����������
    vDisplayRight := vDisplayRight - FVScrollBar.Width;  // ��ȥ��ֱ�������Ŀ��

  if BorderVisible then  // �߿����
    vDisplayRight := vDisplayRight - GBorderWidth;  // ��ȥ�߿�

  ACanvas.FillRect(Rect(0, 0, vDisplayRight, GetDataDisplayBottom));

  // ��ȡ����ʾ����
  GetFirstColDisplay(vStartCol, vEndCol, vDrawLeft);  // ���㵱ǰ��ʾ����ʼ�С������к���ʼ����Կؼ�����ƫ����

  if (FRows.Count = 0) or (ColCount = 0) then
  begin
    DrawTitle(vDrawLeft, vStartCol, vEndCol);
    DrawBorder;
    Exit;
  end;

  // ��ȡ����ʾ����
  GetFirstRowDisplay(vStartRow, vEndRow, vDrawTop);  // ���㵱ǰ��ʾ����ʼ�С���������ź���ʼ����Կؼ�������ƫ����

  if ColCount <> 0 then  // ���ƿ���ʾ������������
  begin
    vTop := vDrawTop;
    //vDisplayRight := GetDataDisplayRight;
    for i := vStartRow to vEndRow  do
    begin
      vLeft := vDrawLeft;
      vUserDrawed := False;

      DoRowDrawBefor(i, vLeft, vTop, ACanvas, vUserDrawed);
      if not vUserDrawed then
      begin
        for j := vStartCol to vEndCol do  // ���Ƶ�Ԫ��
        begin
          if Assigned(FOnCellDraw) then
            FOnCellDraw(Self, i, j, ACanvas, vLeft, vTop)
          else
            DefaultCellDraw(i, j, ACanvas, vLeft, vTop);

          vLeft := vLeft + FColInfos[j].Width;
        end;

        DoRowDrawAfter(i, vLeft, vTop, ACanvas);
      end;

      vTop := vTop + FRowHeight;
    end;
  end;

  DrawTitle(vDrawLeft, vStartCol, vEndCol);  // ���Ʊ����б���

  // ����ָʾ��
  if cgoIndicator in FOptions then
  begin
    i := Math.Min(GetDataDisplayBottom, FRows.Count * FRowHeight) + GetTitleHeight;
    ACanvas.FillRect(Rect(0, 0, FIndicatorWidth, i));
    vTop := vDrawTop;
    for i := vStartRow to vEndRow + 1 do  // +1�������ݸ߶�С�ڿؼ��߶�ʱ���һ��ָʾ��û���±���
    begin
      if vTop >= GetTitleHeight then
      begin
        ACanvas.MoveTo(0, vTop);
        ACanvas.LineTo(FIndicatorWidth, vTop);
      end;
      vTop := vTop + FRowHeight;
    end;
  end;

  // ��������
  if FVScrollBar.Visible then  // ��ֱ����������
  begin
    ACanvas.Refresh;
    i := SaveDC(ACanvas.Handle);  // ����(�豸�����Ļ���)�ֳ�
    try
      MoveWindowOrg(ACanvas.Handle, FVScrollBar.Left, FVScrollBar.Top);
      FVScrollBar.DrawTo(ACanvas);
    finally
      RestoreDC(ACanvas.Handle, i);  // �ָ�(�豸�����Ļ���)�ֳ�
    end;
  end;

  if FHScrollBar.Visible then  // ˮƽ����������
  begin
    ACanvas.Refresh;
    i := SaveDC(ACanvas.Handle);  // ����(�豸�����Ļ���)�ֳ�
    try
      MoveWindowOrg(ACanvas.Handle, FHScrollBar.Left, FHScrollBar.Top);
      FHScrollBar.DrawTo(ACanvas);
    finally
      RestoreDC(ACanvas.Handle, i);  // �ָ�(�豸�����Ļ���)�ֳ�
    end;
  end;

  // �������������ཻ�Ľ�
  if (FVScrollBar.Visible) and (FHScrollBar.Visible) then
  begin
    ACanvas.Brush.Color := GTitleBackColor;
    if BorderVisible then  // �߿����
      ACanvas.FillRect(Rect(Width - FVScrollBar.Width - GBorderWidth, Height - FHScrollBar.Height - GBorderWidth, Width - GBorderWidth, Height - GBorderWidth))
    else  // �߿����
      ACanvas.FillRect(Rect(Width - FVScrollBar.Width, Height - FHScrollBar.Height, Width, Height));
  end;

  DrawBorder;
end;

procedure TCFGrid.GetDataCellAt(const ADataX, ADataY: Integer; var ARow, ACol: Integer);
var
  vRang, vRight: Integer;
  vGridPos, vRowSingle: Single;
begin
  ARow := -1;
  ACol := -1;

  if (ADataX < 0) or (ADataY < 0) then Exit;
  if ADataX > GetDataWidth then Exit;
  if ADataY > FRows.Count * FRowHeight then Exit;

  // �ڼ���
  vRang := FVScrollBar.Rang;  // ��Χ
  vGridPos := ADataY
    * (vRang - (GetDataDisplayHeight - FVScrollBar.PageSize))  // ��������
    / vRang;
  vRowSingle := vGridPos / FRowHeight;
  ARow := Trunc(vRowSingle);

  //�ڼ���
  if BorderVisible then
    vRight := GBorderWidth
  else
    vRight := 0;
  if cgoIndicator in FOptions then
    vRight := vRight + FIndicatorWidth;

  for vRang := 0 to ColCount - 1 do  // �����vRang�൱�� i �ã�ʡ������
  begin
    vRight := vRight + FColInfos[vRang].Width;
    if vRight > ADataX then
    begin
      ACol := vRang;
      Break;
    end;
  end;
end;

function TCFGrid.GetCellDataBoundRect(const ARow, ACol: Integer): TRect;
var
  i, vCellTop, vCellLeft: Integer;
begin
  vCellLeft := 0;
  vCellTop := ARow * FRowHeight + GetTitleHeight;
  for i := 0 to ACol - 1 do
    vCellLeft := vCellLeft + FColInfos[i].Width;

  Result := Bounds(vCellLeft, vCellTop, FColInfos[ACol].Width, FRowHeight);
end;

function TCFGrid.GetCellClientRect(const ARow, ACol: Integer): TRect;
begin
  Result := GetCellDataBoundRect(ARow, ACol);
  OffsetRect(Result, -FHScrollBar.Position, -FVScrollBar.Position);
  if cgoIndicator in FOptions then  // ָʾ������
    OffsetRect(Result, FIndicatorWidth, 0);  // ����ָʾ���Ŀ��
end;

function TCFGrid.GetCells(ARow, ACol: Integer): string;
begin
  if (ARow < 0) or (ARow > RowCount - 1) then
    raise Exception.Create('�쳣��GetCells�������� ARow ����������Χ��');
  if (ACol < 0) or (ACol > ColCount - 1) then
    raise Exception.Create('�쳣��GetCells�������� ACol ����������Χ��');

  Result := FRows[ARow].Items[ACol].Text;
end;

function TCFGrid.GetColCount: Cardinal;
begin
  Result := FColInfos.Count;
end;

function TCFGrid.GetColStyle(ACol: Integer): TCFColStyle;
begin
  Result := FColInfos[ACol].Style;
end;

function TCFGrid.GetColWidth(ACol: Integer): Integer;
begin
  Result := FColInfos[ACol].Width;
end;

function TCFGrid.GetDataDisplayBottom: Integer;
begin
  Result := Height;
  if FHScrollBar.Visible then  // ˮƽ������
    Result := Result - FHScrollBar.Height;

  if BorderVisible then  // �߿����
    Result := Result - GBorderWidth;
end;

function TCFGrid.GetDataDisplayHeight: Integer;
begin
  Result := Height - GetTitleHeight;
  if BorderVisible then  // �߿����
    Result := Result - 2 * GBorderWidth;  // ��ȥ�߿�
  if FHScrollBar.Visible then  // ˮƽ����������
    Result := Result - FHScrollBar.Height;  // ˮƽ�������ĸ߶�
end;

function TCFGrid.GetDataDisplayRight: Integer;
begin
  // ���ݿ��
  Result := GetDataWidth;
  if cgoIndicator in FOptions then  // ָʾ������
    Result := Result + FIndicatorWidth;  // ����ָʾ���Ŀ��

  if Result < Width then Exit;

  Result := Width;
  if FVScrollBar.Visible then  // ��ֱ����������
    Result := Result - FVScrollBar.Width;  // ��ȥ��ֱ�������Ŀ��
  if BorderVisible then  // �߿����
    Result := Result - GBorderWidth;  // ��ȥ�߿�
end;

function TCFGrid.GetDataDisplayWidth: Integer;
begin
  Result := Width;
  if FVScrollBar.Visible then  // ��ֱ����������
    Result := Result - FVScrollBar.Width;  // ��ȥ��ֱ�������Ŀ��
  if BorderVisible then  // �߿����
    Result := Result - 2 * GBorderWidth;  // ��ȥ���ߵı߿�

  if cgoIndicator in FOptions then  // ָʾ������
    Result := Result - FIndicatorWidth;  // ��ȥָʾ���Ŀ��
end;

procedure TCFGrid.GetFirstColDisplay(var AStartCol, AEndCol, ALeftOffset: Integer);
var
  vRight, vDspWidth: Integer;
  vGridHPos: Single;
  i: Integer;
begin
  //if RowCount = 0 then Exit;
  // ���ó�ֵ����֤���˳�����ִ�������ѭ��
  AStartCol := -1;
  AEndCol := -2;
  vGridHPos := 0;

  vDspWidth := GetDataDisplayWidth;  // չʾ���ݵĿ��
  if FHScrollBar.Rang > 0 then
    vGridHPos := (FHScrollBar.Position - FHScrollBar.Min)
      * (FHScrollBar.Rang - (vDspWidth - FHScrollBar.PageSize))  // ���ݷ�Χ����Ҫ����������ʾ�Ĵ�С(������ʾ�����Ĳ���)
      / FHScrollBar.Rang;

  vRight := 0;
  for i := 0 to ColCount - 1 do  // ������
  begin
    vRight := vRight + FColInfos[i].Width;  // �ۼ��п�
    if vRight > vGridHPos then  // ��������п��ҵ�
    begin
      AStartCol := i;  // ��ʼ�и�ֵ
      Break;
    end;
  end;

  if AStartCol < 0 then Exit;  // ���õĳ�ʼֵ�ܱ�֤�˳�����ȥ�ҽ�����

  ALeftOffset := -Round(FColInfos[AStartCol].Width - (vRight - vGridHpos));  // ��ƫ����
  if cgoIndicator in FOptions then  // ָʾ������
    ALeftOffset := ALeftOffset + FIndicatorWidth;  // ��ƫ����Ҫ��ָʾ���Ŀ��

  if FHScrollBar.Rang > vDspWidth then  // ���ݿ�ȴ��ڽ������ʾ���ݿ��
  begin
    for i := AStartCol + 1 to ColCount - 1 do  // ��������
    begin
      vRight := vRight + FColInfos[i].Width;  // �ۼ��п�
      if vRight - vGridHPos >= vDspWidth then  // �ҵ�����Ľ�����
      begin
        AEndCol := i;  // ������
        Break;
      end;
    end;
  end
  else  // ���ݿ��С�ڵ��ڽ������ʾ���ݿ��
  begin
    AEndCol := ColCount - 1;
    if AEndCol < 0 then  // ������Ϊ 0 ʱ�����ƽ�����С����ʼ�У����������ݱ�������
      AEndCol := -2;
  end;
end;

procedure TCFGrid.GetFirstRowDisplay(var AStartRow, AEndRow, ATopOffset: Integer);
var
  vGridVPos, vfIndex: Single;
begin
  if RowCount = 0 then Exit;
  // ���ó�ʼ�У���֤���ʼֵ���������˳�����
  AStartRow := -1;
  AEndRow := -2;
  vGridVPos := 0;
  ATopOffset := 0;

  if FVScrollBar.Rang > 0 then
    vGridVPos := FVScrollBar.Position
      * (FVScrollBar.Rang - (GetDataDisplayHeight - FVScrollBar.PageSize))  // ���ݷ�Χ����Ҫ����������ʾ�Ĵ�С(������ʾ�����Ĳ���)
      / FVScrollBar.Rang;

  vfIndex := vGridVPos / FRowHeight;  // ��ʼ�У�ʵ����
  AStartRow := Trunc(vfIndex);  // ��ʼ�У�������
  ATopOffset := GetTitleHeight - Round(Frac(vfIndex) * FRowHeight);  // ��������ƫ�Ʊ����и�

  vfIndex := (vGridVPos + GetDataDisplayHeight) / FRowHeight;  // �����У�ʵ����
  AEndRow := Trunc(vfIndex);  // �����У�������
  {if Frac(vfIndex) > 0 then  // �����������������һ�д���
    Inc(vEndRow);}
  if AEndRow > RowCount - 1 then  // AEndRow��ֵΪ-2ʱ��������
    AEndRow := RowCount - 1;
end;

function TCFGrid.GetRow(ARow: Integer): TCFRow;
begin
  Result := FRows[ARow];
end;

function TCFGrid.GetRowCount: Cardinal;
begin
  Result := FRows.Count;  // ��ȡ����
end;

procedure TCFGrid.GetTitleCellAt(const ATitleX, ATitleY: Integer; var ATitleRow,
  ATitleCol: Integer);
var
  i, vRight: Integer;
begin
  ATitleRow := -1;
  ATitleCol := -1;
  if (ATitleX > 0) and (ATitleX < FHScrollBar.Max)
    and (ATitleY > 0) and (ATitleY < GetTitleHeight)
  then  // �ڱ�����
  begin
    // �ڼ���
    ATitleRow := 0;

    //�ڼ���
    vRight := 0;
    for i := 0 to ColCount - 1 do  // �����vRang�൱�� i �ã�ʡ������
    begin
      vRight := vRight + FColInfos[i].Width;
      if vRight > ATitleX then
      begin
        ATitleCol := i;
        Break;
      end;
    end;
  end;
end;

function TCFGrid.GetTitleColClientRect(const ATitleCol: Integer): TRect;
var
  i, vLeft: Integer;
begin
  vLeft := 0;
  for i := 0 to ATitleCol - 1 do  // ������
    vLeft := vLeft + FColInfos[i].Width;  // �õ��е����
  Result := Bounds(vLeft, 0, FColInfos[ATitleCol].Width, FRowHeight);  // ��ȡ��Ԫ��������
end;

function TCFGrid.GetTitleHeight: Integer;
begin
  Result := FRowHeight;  // ����߶�
end;

function TCFGrid.GetTitleText(ACol: Integer): string;
begin
  if (ACol < 0) or (ACol > ColCount - 1) then
    raise Exception.Create('�쳣��GetTitleText�������� ACol ����������Χ��');
  Result := FTitleRow[ACol].Text;
end;

procedure TCFGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i, vDateX, vDateY, vOldRowIndex, vOldColIndex, vRight, vBottom: Integer;
begin
  inherited;
  if Button = mbMiddle then  // ����м�����ƽ�ƿ�ʼ
  begin
    FMovePt := Point(X, Y);
    Windows.SetCursor(LoadCursor(0, IDC_HAND));
    Exit;
  end;

  vRight := GetDataDisplayRight;
  vBottom := GetDataDisplayBottom;
  if cgoColSizing in FOptions then  // ָʾ������
  begin
    if PtInRect(Bounds(0, 0, vRight, GetTitleHeight), Point(X, Y)) then  // ����ڱ�����
    begin
      if FResizeCol >= 0 then  // Ҫ�ı���д���
      begin
        FResizeX := X;  // �ı�ֵ
        FColResizing := True;  // �������ڸı�
      end;
      Exit;
    end;
  end;

  if FVScrollBar.Visible and (X > vRight) and (Y < vBottom) then  // ������ڴ�ֱ��������
  begin
    FMouseDownControl := FVScrollBar;  // ��ֵ������������
    FVScrollBar.MouseDown(Button, Shift, X - vRight, Y);  // ����
  end
  else
  if FHScrollBar.Visible and (Y > vBottom) and (X < vRight) then  // �������ˮƽ��������
  begin
    FMouseDownControl := FHScrollBar;
    FHScrollBar.MouseDown(Button, Shift, X, Y - vBottom);
  end
  else
  if FRows.Count > 0 then  // �����������
  begin
    // ����ֵ
    vOldRowIndex := FRowIndex;
    vOldColIndex := FColIndex;
    FRowIndex := -1;
    FColIndex := -1;
    vDateX := FHScrollBar.Position + X;  // ʵ������ˮƽ����
    vDateY := FVScrollBar.Position + Y - GetTitleHeight;  // ʵ�����ݵĴ�ֱ���꣨��ȥ��������У�
    GetDataCellAt(vDateX, vDateY, FRowIndex, FColIndex);
    if (FRowIndex <> vOldRowIndex) or (FColIndex <> vOldColIndex) then  // ѡ�����з����仯
    begin
      if (vOldRowIndex >= 0) and (not FReadOnly) then  // ԭѡ�е�Ԫ��
        DoOnCellAfterEdit(vOldRowIndex, vOldColIndex);  // �༭���

      if cgoRowSelect in FOptions then  // ��ѡ
      begin
        if vOldRowIndex >= 0 then  // ԭѡ���д���
        begin
          for i := 0 to ColCount - 1 do  // ���ԭѡ���еĸ���Ԫ���ѡ��״̬
            Exclude(FRows[vOldRowIndex][i].FStates, ccsSelected);
        end;

        if FRowIndex >= 0 then  // ��ѡ���д���
        begin
          for i := 0 to ColCount - 1 do  // ��ѡ���и���Ԫ���ѡ��״̬
            Include(FRows[FRowIndex][i].FStates, ccsSelected);
        end;
      end
      else  // ����ѡ
      begin
        if vOldRowIndex >= 0 then  // ԭѡ�е�Ԫ�����
          Exclude(FRows[vOldRowIndex][vOldColIndex].FStates, ccsSelected);

        if FRowIndex >= 0 then  // ��ѡ�е�Ԫ��
          Include(FRows[FRowIndex][FColIndex].FStates, ccsSelected);
      end;

      UpdateDirectUI;  // �Ż�Ϊֻ���Ʊ䶯����
      if Assigned(FOnSelectCell) then
        FOnSelectCell(Self, FRowIndex, FColIndex);
    end;

    if (FRowIndex >= 0) and (not FReadOnly) then  // ��ѡ�е�Ԫ��
      DoOnCellBeforeEdit(FRowIndex, FColIndex);
  end;
end;

procedure TCFGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vStartCol, vEndCol, vLeftOffset: Integer;
  i, vRight: Integer;
begin
  inherited;

  if Shift = [ssMiddle] then  // ����м�����
  begin
    FHScrollBar.Position := FHScrollBar.Position + (FMovePt.X - X);
    FVScrollBar.Position := FVScrollBar.Position + (FMovePt.Y - Y);
    FMovePt.X := X;
    FMovePt.Y := Y;
    Windows.SetCursor(LoadCursor(0, IDC_HAND));
    UpdateDirectUI;
    Exit;
  end;
  if cgoColSizing in FOptions then  // ָʾ������
  begin
    if FColResizing then  // ����ڱ�������������֮��������ϣ�����ƶ��п�ʵʱ�ı�
    begin
      // �п�ʵʱ�ı�
      SetColWidth(FResizeCol, FColInfos[FResizeCol].Width + X - FResizeX);
      FResizeX := X;
      Exit;
    end;
    Cursor := crDefault;  // ������ʽΪ��ͨ
    FResizeCol := -1;

    if PtInRect(Bounds(0, 0, Width - FVScrollBar.Width, GetTitleHeight), Point(X, Y)) then  // ���û�е����ڱ������ƶ������ʽ��ʾ���Ըı��п�
    begin
      GetFirstColDisplay(vStartCol, vEndCol, vLeftOffset);  // �ͻ������ݵ���ʼ�кͽ������Լ����ݵ�ƫ����
      if vStartCol >= 0 then
      begin
        vRight := vLeftOffset;
        for i := vStartCol to vEndCol do  // �ӿ�ʼ�е�������������
        begin
          vRight := vRight + FColInfos[i].Width;  // �ͻ������ұ߽��λ��
          if (X > vRight - 4) and (X < vRight + 5) then  // ����֮����λ������ƫ�Ƽ�������
          begin
            FResizeCol := i;  // Ҫ�ı��е��п�
            Cursor := crHSplit;  // �ı�������ʽ
            Break;
          end;
        end;
      end;
      Exit;
    end;
  end;

  if FMouseDownControl = FVScrollBar then  // �ڴ�ֱ���������ƶ�
    FVScrollBar.MouseMove(Shift, X + FVScrollBar.Width - Width, Y)
  else
  if FMouseDownControl = FHScrollBar then  // ��ˮƽ���������ƶ�
    FHScrollBar.MouseMove(Shift, X, Y + FHScrollBar.Height - Height)
  else
  if FRows.Count > 0 then
  begin
    if ssLeft in Shift then  // ��ѡ
    begin

    end;
  end;
end;

procedure TCFGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vTitleRow, vTitleCol, vDataX, vDataY, vRow, vCol: Integer;
begin
  inherited;

  if Button = mbMiddle then
  begin
    Windows.SetCursor(LoadCursor(0, IDC_ARROW));
    Exit;
  end;

  FColResizing := False;
  if FResizeCol >= 0 then  // ���иı�ѡ���е��п�
  begin
    //SetColWidth(FResizeCol, FTitleRow[FResizeCol].Width + X - FResizeX);
    FResizeCol := -1;  // �ı��п�֮����Ϊû��Ҫ�ı���е��п�
    Cursor := crDefault;  // �ָ������ʽ
    Exit;
  end;

  if FMouseDownControl = FVScrollBar then  // �ڴ�ֱ��������
    FVScrollBar.MouseUp(Button, Shift, X - GetDataDisplayRight, Y)
  else
  if FMouseDownControl = FHScrollBar then  // ��ˮƽ��������
    FHScrollBar.MouseUp(Button, Shift, X, Y - GetDataDisplayBottom);

  if FMouseDownControl <> nil then  // �����¼�������
    FMouseDownControl := nil
  else  // �ڷǹ�����������
  begin
    if FRows.Count > 0 then  // �����������
    begin
      vRow := -1;
      vCol := -1;
      vDataX := FHScrollBar.Position + X;  // ʵ������ˮƽ����
      vDataY := FVScrollBar.Position + Y - GetTitleHeight;  // ʵ�����ݵĴ�ֱ���꣨��ȥ��������У�
      GetDataCellAt(vDataX, vDataY, vRow, vCol);
      if (vRow = FRowIndex) and (vCol = FColIndex) and (vRow >= 0) and (vCol >= 0) then  // ���µ�����ͬһ����Ԫ��
      begin
        if FColInfos[vCol].Style = TCFColStyle.ccsCheckBox then
        begin
          if FRows[vRow][vCol].Text = '1' then
            FRows[vRow][vCol].Text := '0'
          else
            FRows[vRow][vCol].Text := '1';

          UpdateDirectUI;
        end;

        Exit;
      end;
    end;

    GetTitleCellAt(FHScrollBar.Position + X, Y, vTitleRow, vTitleCol);
    if (vTitleRow >= 0) and (vTitleCol >= 0) then
    begin
      if Assigned(FOnTitleClick) then
        FOnTitleClick(Self, vTitleRow, vTitleCol);
    end;
  end;
end;


procedure TCFGrid.OnHScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  UpdateDirectUI;  // �ػ�����
end;

procedure TCFGrid.OnVScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  UpdateDirectUI;  // ��������
end;

procedure TCFGrid.ScrollToRow(const ARowIndex: Cardinal);
begin
  FVScrollBar.Position := 50;
  UpdateDirectUI;
end;

procedure TCFGrid.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  CheckScrollBarVisible;
  CalcScrollBarPosition;
end;

procedure TCFGrid.SetCells(ARow, ACol: Integer; const Value: string);
var
  vCellRect: TRect;
begin
  if (ARow < 0) or (ARow > RowCount - 1) then
    raise Exception.Create('�쳣��SetCells�������� ARow ����������Χ��');
  if (ACol < 0) or (ACol > ColCount - 1) then
    raise Exception.Create('�쳣��SetCells�������� ACol ����������Χ��');

  if Value <> FRows[ARow].Items[ACol].Text then
  begin
    FRows[ARow].Items[ACol].Text := Value;
    vCellRect := GetCellClientRect(ARow, ACol);
    OffsetRect(vCellRect, Left, Top);

    IntersectRect(vCellRect, vCellRect, ClientRect);  // ���ص�2��3��������ľ��ν���

    if not IsRectEmpty(vCellRect) then  // ��Ԫ������Ϳͻ��������н���
      UpdateDirectUI(vCellRect);
  end;
end;

procedure TCFGrid.SetColCount(Value: Cardinal);
var
  i, j, vColCount: Integer;
  vCol: TCFCol;
  vTitleCol: TTitleCol;
  vColInfo: TCFColInfo;
begin
  if ColCount <> Value then  // ��������иı�
  begin
    vColCount := ColCount;
    if Value > vColCount then  // ������
    begin
      // ���ı����е�����
      for i := vColCount to Value - 1 do
      begin
        vTitleCol := TTitleCol.Create(FTitleRow);  // ���ӱ�����
        //vCol.Text := IntToStr(i) + '-����';
        FTitleRow.Add(vTitleCol);  // ���ӱ�����

        vColInfo := TCFColInfo.Create;
        vColInfo.Width := DefaultColWidth;
        vColInfo.Style := ccsText;
        FColInfos.Add(vColInfo);
      end;
      // ���������е�����
      if RowCount > 0 then
      begin
        for i := 0 to FRows.Count - 1 do  // ������
        begin
          for j := vColCount to Value - 1 do  // �����н��и�������
          begin
            vCol := TCFCol.Create(FRows[i], j);  // ������
            //vCol.Text := IntToStr(i) + '-' + IntToStr(j);
            FRows[i].Add(vCol);  // ������
          end;
        end;
      end;
    end
    else  // ������
    begin
      // ���ı����е�����
      for i := vColCount - 1 downto Value do  // ���±�����ʵ�ּ���
      begin
        FTitleRow.Delete(i);  // ɾ��������
        FColInfos.Delete(i);
      end;

      // ���������е�����
      if RowCount > 0 then
      begin
        for i := 0 to FRows.Count - 1 do  // ������
        begin
          for j := vColCount - 1 downto Value do  // ���±�������������ʵ�ּ���
            FRows[i].Delete(j);  // ����
        end;
      end;
    end;

    SizeChange;
  end;
end;

procedure TCFGrid.SetColStyle(ACol: Integer; const AStyle: TCFColStyle);
begin
  FColInfos[ACol].Style := AStyle;
end;

procedure TCFGrid.SetColWidth(ACol, AWidth: Integer);
var
  i, vW: Integer;
begin
  if AWidth < DefaultColWidth then  // �п�С��Ĭ�ϵ��п�
    vW := DefaultColWidth  // �����п�ΪĬ��ֵ
  else
    vW := AWidth;  // �п�ΪҪ���õ��п�ֵ

  FColInfos[ACol].Width := vW;  // �ı�����п�
  for i := 0 to RowCount - 1 do  // ����������
    FColInfos[ACol].Width := vW;  // �������п���иı�

  SizeChange;
end;

procedure TCFGrid.SetOptions(Value: TCFGridOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    UpdateDirectUI;
  end;
end;

procedure TCFGrid.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    {to do: ����༭������ʾ������Ϊֻ��ʱȡ���༭}
  end;
end;

procedure TCFGrid.SetRowCount(Value: Cardinal);
var
  vRow: TCFRow;
  i, vRowCount: Integer;
begin
  Self.BeginUpdate;
  try
    if FRows.Count <> Value then  // �и��иı�
    begin
      vRowCount := RowCount;  // ����ԭ��������
      if Value > vRowCount then  // Ҫ���õ������ͺ�ԭ�����������бȽϣ�Ҫ������
      begin
        for i := vRowCount to Value - 1 do  // ����������
        begin
          vRow := TCFRow.Create(Self, ColCount);  // ������
          FRows.Add(vRow);  // ������
        end;
      end
      else  // Ҫʵ�ּ�����
      begin
        for i := vRowCount - 1 downto Value do  // ���±�����ʵ�ּ���
          FRows.Delete(i);  // ����
      end;
    end;
  finally
    Self.EndUpdate;
  end;
end;

procedure TCFGrid.SetTitleText(ATitleCol: Integer; const Value: string);
var
  vColRect: TRect;
begin
  if (ATitleCol < 0) or (ATitleCol > ColCount - 1) then
    raise Exception.Create('�쳣��SetTitleText�������� ACol ����������Χ��');
  if Value <> FTitleRow[ATitleCol].Text then
  begin
    FTitleRow[ATitleCol].Text := Value;
    vColRect := GetTitleColClientRect(ATitleCol);
    OffsetRect(vColRect, Left, Top);

    IntersectRect(vColRect, vColRect, ClientRect);  // ���ص�2��3��������ľ��ν���

    if not IsRectEmpty(vColRect) then  // ��Ԫ������Ϳͻ��������н���
      UpdateDirectUI(vColRect);
  end;
end;

procedure TCFGrid.ShowEditControl(const ARow, ACol: Cardinal;
  const AColStyle: TCFColStyle);
var
  vRect: TRect;
begin
  case AColStyle of
    ccsText, ccsCheckBox:
      Exit;

    ccsEdit:
      begin
        FEditControl := TCFEdit.Create(Self);
        with (FEditControl as TCFEdit) do
        begin
          Font := Self.Font;
          Text := Cells[ARow, ACol];
          OnKeyPress := DoOnEditControlKeyPress;
        end;
      end;

    ccsCombobox: FEditControl := TCFCombobox.Create(Self);
    ccsButtonEdit: FEditControl := TCFButtonEdit.Create(Self);
    ccsGridEdit: FEditControl := TCFGridEdit.Create(Self);
  end;

  vRect := GetCellClientRect(ARow, ACol);
  FEditControl.Left := Left + vRect.Left;
  FEditControl.Top := Top + vRect.Top;
  FEditControl.Width := vRect.Right - vRect.Left + 1;
  FEditControl.Height := vRect.Bottom - vRect.Top + 1;

  FEditControl.Parent := Self.Parent;
  FEditControl.BringToFront;
end;

procedure TCFGrid.SizeChange;
begin
  CheckScrollBarVisible;
  UpdateDirectUI;  // ���¿ͻ�������
end;

{ TCFCol }

constructor TCFCol.Create(const ARow: TCFRow; const AIndex: Integer);
begin
  inherited Create;
  FRow := ARow;
  FColIndex := AIndex;
end;

procedure TCFCol.Draw(const ACanvas: TCanvas; const ALeft, ATop, AHeight, AWidth: Integer;
  const AStyle: TCFColStyle);
var
  vRect, vBoxRect: TRect;
begin
  vRect := Rect(ALeft + 1, ATop + 1, ALeft + AWidth, ATop + AHeight);
  if (ccsSelected in FStates) and (cgoShowSelect in FRow.FGrid.FOptions) then  // ��Ԫ��ѡ��
  begin
    ACanvas.Brush.Color := GHightLightColor;  // ������ʾ
    ACanvas.FillRect(vRect);  // �ػ�Ҫ������ʾ������
  end;
  // �����Ԫ�������
  InflateRect(vRect, -2, -1);
  {Windows.ExtTextOut(ACanvas.Handle, ALeft + 3, ATop + 4, 0, nil, FText,
    Length(FText), nil);}

  case AStyle of
    ccsText, ccsEdit:
      begin
        ACanvas.Brush.Style := bsClear;
        ACanvas.TextRect(vRect, FText);  // ʵ�ֲ�������Ԫ��Χ������ʾ�ı�
      end;

    ccsCheckBox:
      begin
        vBoxRect := Classes.Bounds(vRect.Left + (vRect.Width - 14) div 2,
          vRect.Top + (vRect.Height - 14) div 2, 14, 14);

        if FText = '1'then
          DrawFrameControl(ACanvas.Handle, vBoxRect, DFC_MENU, DFCS_CHECKED or DFCS_MENUCHECK);

        ACanvas.Pen.Color := clBlack;
        ACanvas.Brush.Style := bsClear;
        ACanvas.Rectangle(vBoxRect.Left, vBoxRect.Top, vBoxRect.Right, vBoxRect.Bottom);
      end;

    ccsCombobox: ;
    ccsButtonEdit: ;
    ccsGridEdit: ;
  end;

  // �߿�
  ACanvas.Pen.Color := GLineColor;  // ������
  //ACanvas.Rectangle(Rect(ALeft, ATop, ALeft + FWidth, ATop + AHeight));
  // ����Ԫ��ı߿���
  ACanvas.MoveTo(ALeft, ATop + AHeight);  // ���½�
  ACanvas.LineTo(ALeft + AWidth, ATop + AHeight);  // ���½�
  ACanvas.LineTo(ALeft + AWidth, ATop);  // ���Ͻ�
end;

function TCFCol.GetAsBoolean: Boolean;
begin
  Result := FText = '1';
end;

function TCFCol.GetAsInteger: Integer;
begin
  Result := StrToIntDef(FText, -1);
end;

procedure TCFCol.SetAsBoolean(const Value: Boolean);
begin
  if Value then
    FText := '1'
  else
    FText := '0';
end;

procedure TCFCol.SetAsInteger(const Value: Integer);
begin
  FText := IntToStr(Value);
end;

procedure TCFCol.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    if FRow.FGrid.FColInfos[FColIndex].Style = ccsCheckBox then
    begin
      if (Value = 'True') or (Value = 'true') or (Value = '1') then
        FText := '1'
      else
        FText := '0';
    end
    else
      FText := Value;
  end;
end;

{ TCFRow }

constructor TCFRow.Create(const AGrid: TCFGrid; const AColCount: Cardinal);
var
  vCol: TCFCol;
  i: Integer;
begin
  inherited Create;
  FGrid := AGrid;
  for i := 0 to AColCount - 1 do  // ������
  begin
    vCol := TCFCol.Create(Self, i);  // ������
    Add(vCol);  // ������
  end;
end;

procedure TCFRow.Draw(const ACanvas: TCanvas; const ALeft, ATop, AStartCol, AEndCol: Integer);
var
  i, vLeft: Integer;
begin
  vLeft := ALeft;
  for i := AStartCol to AEndCol do  // ������չʾ����
  begin
    Items[i].Draw(ACanvas, vLeft, ATop, FGrid.RowHeight, FGrid.FColInfos[i].Width,
      FGrid.FColInfos[i].Style);  // ����Ԫ�������

    vLeft := vLeft + FGrid.FColInfos[i].Width;  // ��һ����Ԫ������Ҫ���п�
  end;
end;

function TCFRow.Get(Index: Integer): TCFCol;
begin
  Result := TCFCol(inherited Get(Index));
end;

procedure TCFRow.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = TListNotification.lnDeleted then  // ����֪ͨ�ͷ�
    TCFCol(Ptr).Free;
end;

procedure TCFRow.Put(Index: Integer; const Value: TCFCol);
begin
  inherited Put(Index, Value);
end;

{ TCFTitleRow }

constructor TCFTitleRow.Create(const AGrid: TCFGrid; const AColCount: Cardinal);
var
  vTitleCol: TTitleCol;
  i: Integer;
begin
  inherited Create;
  FGrid := AGrid;
  for i := 0 to AColCount - 1 do  // ������
  begin
    vTitleCol := TTitleCol.Create(Self);  // ������
    Add(vTitleCol);  // ������
  end;
end;

procedure TCFTitleRow.Draw(const ACanvas: TCanvas; const ALeft, ATop, AStartCol,
  AEndCol: Integer);
var
  i, vLeft: Integer;
begin
  vLeft := ALeft;
  for i := AStartCol to AEndCol do  // ������չʾ����
  begin
    Items[i].Draw(ACanvas, vLeft, ATop, i, FGrid.RowHeight);  // ����Ԫ�������
    vLeft := vLeft + FGrid.FColInfos[i].Width;  // ��һ����Ԫ������Ҫ���п�
  end;
end;

function TCFTitleRow.Get(Index: Integer): TTitleCol;
begin
  Result := TTitleCol(inherited Get(Index));
end;

procedure TCFTitleRow.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = TListNotification.lnDeleted then  // ����֪ͨ�ͷ�
    TTitleCol(Ptr).Free;
end;

procedure TCFTitleRow.Put(Index: Integer; const Value: TTitleCol);
begin
  inherited Put(Index, Value);
end;

{ TTitleCol }

constructor TTitleCol.Create(const ATitleRow: TCFTitleRow);
begin
  inherited Create;
end;

procedure TTitleCol.Draw(const ACanvas: TCanvas; const ALeft, ATop,
  AHeight, AWidth: Integer);
begin
  ACanvas.FillRect(Rect(ALeft + 1, ATop + 1, ALeft + AWidth, ATop + AHeight));  // �ػ�Ҫ������ʾ������
  ACanvas.Pen.Color := GLineColor;  // ������
  // ����Ԫ��ı߿���
  ACanvas.MoveTo(ALeft, ATop + AHeight);  // ���½�
  ACanvas.LineTo(ALeft + AWidth, ATop + AHeight);  // ���½�
  ACanvas.LineTo(ALeft + AWidth, ATop);  // ���Ͻ�
  // �����Ԫ�������
  Windows.ExtTextOut(ACanvas.Handle, ALeft + 3, ATop + 4, 0, nil, FText,
    Length(FText), nil);
end;

{ TCFColInfo }

constructor TCFColInfo.Create;
begin
  FReadOnly := False;
  FWidth := 10;
  FStyle := ccsText;
end;

{ TCFRows }

procedure TCFRows.Notify(const Value: TCFRow; Action: TCollectionNotification);
begin
  inherited;
  if Action in [cnAdded, cnRemoved] then
  begin
    if Assigned(FOnCountChange) then
      FOnCountChange(Self);
  end;
end;

end.
