unit CFGrid;

interface

uses
  Windows, Classes, Controls, Graphics, Generics.Collections,
  CFControl, CFScrollBar;

type
  TColStates = set of (ccsSelected, ccsFocused);

  TRow = class;

  TCol = class(TPersistent)
  private
    FRow: TRow;
    FText: string;
    FWidth: Integer;
    FStates: TColStates;
  public
    constructor Create(const ARow: TRow);
    procedure Draw(const ACanvas: TCanvas; const ALeft, ATop, AHeight: Integer);
    //
    property Text: string read FText write FText;
    property Width: Integer read FWidth write FWidth;
    property States: TColStates read FStates write FStates default [];
  end;

  TTitleRow = class;

  TTitleCol = class(TPersistent)
  private
    FText: string;
    FWidth: Integer;
  public
    constructor Create(const ATitleRow: TTitleRow);
    procedure Draw(const ACanvas: TCanvas; const ALeft, ATop, AHeight: Integer);
    //
    property Text: string read FText write FText;
    property Width: Integer read FWidth write FWidth;
  end;

  TCFGrid = class;

  TRow = class(TList)
  private
    FGrid: TCFGrid;
    /// <summary> 获取该行的列的内容 </summary>
    function Get(Index: Integer): TCol;

    /// <summary> 设置该行的列的内容 </summary>
    procedure Put(Index: Integer; const Value: TCol);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(const AGrid: TCFGrid; const AColCount: Cardinal);

    /// <summary> 画该行 </summary>
    procedure Draw(const ACanvas: TCanvas; const ALeft, ATop, AStartCol, AEndCol: Integer);
    property Items[Index: Integer]: TCol read Get write Put; default;
  end;

  TTitleRow = class(TList)
  private
    FGrid: TCFGrid;
    /// <summary> 获取该行的列的内容 </summary>
    function Get(Index: Integer): TTitleCol;

    /// <summary> 设置该行的列的内容 </summary>
    procedure Put(Index: Integer; const Value: TTitleCol);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(const AGrid: TCFGrid; const AColCount: Cardinal);

    /// <summary> 画该行 </summary>
    procedure Draw(const ACanvas: TCanvas; const ALeft, ATop, AStartCol, AEndCol: Integer);
    property Items[Index: Integer]: TTitleCol read Get write Put; default;
  end;

  TCGridOption = (cgoRowSizing, cgoColSizing, cgoIndicator, cgoRowSelect);
  TCGridOptions = set of TCGridOption;
  TEditControlType = (ectEdit, ectCombobox, ectButtonEdit, ectGridEdit);

  TOnSelectCell = procedure(Sender: TObject; ARow, ACol: Integer{; var CanSelect: Boolean}) of object;
  TOnTitleClick = procedure(Sender: TObject; ATitleRow, ATitleCol: Integer) of object;
  TOnCellDraw = procedure(Sender: TObject; const ARow, ACol: Cardinal;
    const ACanvas: TCanvas; const ALeft, ATop: Integer) of object;
  TOnCellBeforeEdit = procedure(Sender: TObject; const ARow, ACol: Cardinal; var ACancel: Boolean; var AEditControlType: TEditControlType) of object;
  TOnCellAfterEdit = procedure(Sender: TObject; const ARow, ACol: Cardinal; const ANewValue: string; var ACancel: Boolean) of object;

  TCFGrid = class(TCFTextControl)
  private
    FRows: TObjectList<TRow>;
    FTitleRow: TTitleRow;
    FVScrollBar, FHScrollBar: TCFScrollBar;
    FEditControl: TCFTextControl;
    FMouseDownCtronl: TCFCustomControl;

    FDefaultColWidth,
    FRowHeight: Integer;
    /// <summary> 只读 </summary>
    FReadOnly: Boolean;

    // 拖动改变列宽
    FResizeCol,  // 拖动改变列宽时对应的列
    FResizeX     // 记录拖动时X位置
      : Integer;
    FMovePt: TPoint;  // 鼠标中键按下平移时起始坐标
    FIndicatorWidth: Integer;
    FColResizing: Boolean;
    FOptions: TCGridOptions;
    FRowIndex, FColIndex: Integer;

    FOnSelectCell: TOnSelectCell;
    FOnTitleClick: TOnTitleClick;
    FOnCellDraw: TOnCellDraw;
    FOnCellBeforeEdit: TOnCellBeforeEdit;
    FOnCellAfterEdit: TOnCellAfterEdit;
    /// <summary>
    /// 获取数据的总宽度
    /// </summary>
    /// <returns>数据的总宽度</returns>
    function GetDataWidth: Integer;

    /// <summary>
    /// 获取展示数据的宽度
    /// </summary>
    /// <returns>能展示数据的总宽度</returns>
    function GetDataDisplayWidth: Integer;

    /// <summary>
    /// 获取展示数据的高度
    /// </summary>
    /// <returns>展示数据高度</returns>
    function GetDataDisplayHeight: Integer;

    /// <summary>
    /// 获取展示数据的右边
    /// </summary>
    /// <returns>数据的右边</returns>
    function GetDataDisplayRight: Integer;

    /// <summary>
    /// 获取展示数据的底部
    /// </summary>
    /// <returns>数据的底部</returns>
    function GetDataDisplayBottom: Integer;

    /// <summary>
    /// 获取单元格的内容
    /// </summary>
    /// <param name="ARow">行</param>
    /// <param name="ACol">列</param>
    /// <returns>单元格的内容</returns>
    function GetCells(ARow, ACol: Integer): string;

    /// <summary>
    /// 获取列的标题
    /// </summary>
    /// <param name="ACol">列</param>
    /// <returns>列的标题</returns>
    function GetTitleText(ACol: Integer): string;

    /// <summary> 垂直滚动条事件 </summary>
    procedure OnVScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);

    /// <summary> 水平滚动条事件 </summary>
    procedure OnHScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);

    /// <summary>
    /// 获取展示数据的起始行和结束行
    /// </summary>
    /// <param name="AStartRow">起始行</param>
    /// <param name="AEndRow">结束行</param>
    /// <param name="ATopOffset">偏移量</param>
    procedure GetFirstRowDisplay(var AStartRow, AEndRow, ATopOffset: Integer);

    /// <summary>
    /// 获取展示数据的起始列和结束列
    /// </summary>
    /// <param name="AStartCol">开始列</param>
    /// <param name="AEndCol">结束列</param>
    /// <param name="ALeftOffset">数据偏移量</param>
    procedure GetFirstColDisplay(var AStartCol, AEndCol, ALeftOffset: Integer);

    /// <summary>
    /// 获取标题的高度
    /// </summary>
    /// <returns>标题高度</returns>
    function GetTitleHeight: Integer;

    /// <summary>
    /// 给单元格赋值
    /// </summary>
    /// <param name="ARow">单元格行</param>
    /// <param name="ACol">单元格列</param>
    /// <param name="Value">相应值</param>
    procedure SetCells(ARow, ACol: Integer; const Value: string);

    /// <summary>
    /// 给标题赋值
    /// </summary>
    /// <param name="ACol">列</param>
    /// <param name="Value">标题列赋值</param>
    procedure SetTitleText(ATitleCol: Integer; const Value: string);

    procedure SetOptions(Value: TCGridOptions);

    /// <summary> 计算是滚动条的位置 </summary>
    procedure CalcScrollBarPosition;

    /// <summary> 计算是否要显示滚动条 </summary>
    procedure CheckScrollBarVisible;

    procedure ShowEditControl(const ARow, ACol: Cardinal; const AEditControlType: TEditControlType);
    procedure DoOnCellBeforeEdit(const ARow, ACol: Cardinal);
    procedure DoOnCellAfterEdit(const ARow, ACol: Cardinal);
    procedure DoOnEditControlKeyPress(Sender: TObject; var Key: Char);
  protected
    /// <summary> 绘制 </summary>
    procedure DrawControl(ACanvas: TCanvas); override;
    /// <summary>
    /// 设置只读属性
    /// </summary>
    /// <param name="Value">True：只读</param>
    procedure SetReadOnly(Value: Boolean);

    procedure AdjustBounds; override;
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

    /// <summary>
    /// 滚动到指定行
    /// </summary>
    /// <param name="ARowIndex">行号</param>
    procedure ScrollToRow(const ARowIndex: Cardinal);

    /// <summary>
    /// 获取指定坐标的行和列
    /// </summary>
    /// <param name="DataX">数据坐标X</param>
    /// <param name="DataY">数据坐标Y</param>
    /// <param name="ARow">返回坐标行</param>
    /// <param name="ACol">返回坐标列</param>
    procedure GetDataCellAt(const ADataX, ADataY: Integer; var ARow, ACol: Integer);

    procedure GetTitleCellAt(const ATitleX, ATitleY: Integer; var ATitleRow, ATitleCol: Integer);

    /// <summary>
    /// 获取单元格的区域(相应界面上)
    /// </summary>
    /// <param name="ARow">单元格行</param>
    /// <param name="ACol">单元格列</param>
    /// <returns>单元格区域</returns>
    function GetCellClientRect(const ARow, ACol: Integer): TRect;

    /// <summary>
    /// 获取指定的列的区域（从标题列指定）
    /// </summary>
    /// <param name="ACol">标题列</param>
    /// <returns>列区域</returns>
    function GetTitleColClientRect(const ATitleCol: Integer): TRect;

    /// <summary>
    /// 获取单元格所在数据的虚拟区域
    /// </summary>
    /// <param name="ARow">单元格行</param>
    /// <param name="ACol">单元格列</param>
    /// <returns>数据区域</returns>
    function GetCellDataBoundRect(const ARow, ACol: Integer): TRect;

    /// <summary>
    /// 设置列的宽度
    /// </summary>
    /// <param name="ACol">指定列</param>
    /// <param name="AWidth">列宽度</param>
    procedure SetColWidth(const ACol, AWidth: Integer);

    /// <summary>
    /// 使用单元格默认的绘制方式绘制指定单元格内容到指定的画布上
    /// </summary>
    /// <param name="ARow">行</param>
    /// <param name="ACol">列</param>
    /// <param name="ACanvas">画布</param>
    procedure DefaultCellDraw(const ARow, ACol: Cardinal; const ACanvas: TCanvas;
      const ALeft, ATop: Integer);

    property Cells[ARow, ACol: Integer]: string read GetCells write SetCells;
    property TitleText[ACol: Integer]: string read GetTitleText write SetTitleText;
    //property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
    property RowIndex: Integer read FRowIndex;
    property ColIndex: Integer read FColIndex;
  published
    property DefaultColWidth: Integer read FDefaultColWidth write FDefaultColWidth;
    property RowHeight: Integer read FRowHeight write FRowHeight;
    property RowCount: Cardinal read GetRowCount write SetRowCount;
    property ColCount: Cardinal read GetColCount write SetColCount;
    property Options: TCGridOptions read FOptions write SetOptions;
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
    Result := Result + FTitleRow[i].Width;
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
  if (not Assigned(FVScrollBar)) or (not Assigned(FHScrollBar)) then Exit;  // 设计期

  // 顶点位置
  if BorderVisible then  // 显示边框
  begin
    FVScrollBar.Left := Width - FVScrollBar.Width - GBorderWidth;
    FVScrollBar.Top := GBorderWidth;
    FVScrollBar.Height := Height - 2 * GBorderWidth;
    if FHScrollBar.Visible then  // 水平坐标存在
      FVScrollBar.Height := FVScrollBar.Height - FHScrollBar.Height;

    FHScrollBar.Left := GBorderWidth;
    FHScrollBar.Top := Height - FHScrollBar.Height - GBorderWidth;
    FHScrollBar.Width := Width - 2 * GBorderWidth;
    if FVScrollBar.Visible then  // 垂直坐标存在
      FHScrollBar.Width := FHScrollBar.Width - FVScrollBar.Width;
  end
  else  // 不显示边框
  begin
    FVScrollBar.Left := Width - FVScrollBar.Width;
    FVScrollBar.Top := 0;
    FVScrollBar.Height := Height;
    if FHScrollBar.Visible then  // 水平坐标存在
      FVScrollBar.Height := FVScrollBar.Height - FHScrollBar.Height;

    FHScrollBar.Left := 0;
    FHScrollBar.Top := Height - FHScrollBar.Height;
    FHScrollBar.Width := Width;
    if FVScrollBar.Visible then  // 垂直坐标存在
      FHScrollBar.Width := FHScrollBar.Width - FVScrollBar.Width;
  end;
end;

procedure TCFGrid.CheckScrollBarVisible;
var
  vWidth, vHeight, vHMax, vVMax: Integer;
  vVVisible, vHVisible: Boolean;
begin
  if (not Assigned(FVScrollBar)) or (not Assigned(FHScrollBar)) then Exit;  // 设计期

  vHMax := GetDataWidth;
  vVMax := FRows.Count * RowHeight;  // 数据高
  // 垂直方向数据可显示的高度
  vHeight := Height - GetTitleHeight;  // 减去标题行
  // 水平方向数据可显示的宽度
  vWidth := Width;
  if cgoIndicator in FOptions then
    vWidth := vWidth - FIndicatorWidth;

  if BorderVisible then  // 边框存在
  begin
    vHeight := vHeight - 2 * GBorderWidth;  // 高度减去边框
    vWidth := vWidth - 2 * GBorderWidth;
  end;
  // 判断滚动条是否显示
  vVVisible := False;
  vHVisible := False;
  if vVMax > vHeight then  // 垂直大于高度
  begin
    vVVisible := True;
    vWidth := vWidth - FVScrollBar.Width;
  end;

  if vHMax > vWidth then  // 水平大于宽度
  begin
    vHVisible := True;
    vHeight := vHeight - FHScrollBar.Height;
  end;

  // 再次判断显示状态，以纠正因2个滚动条其中任何一个不显示后的偏差
  if (not vVVisible) and (vVMax > vHeight) then  // 垂直大于高度
  begin
    vVVisible := True;
    vWidth := vWidth - FVScrollBar.Width;
  end;

  if (not vHVisible) and (vHMax > vWidth) then  // 水平大于宽度
  begin
    vHVisible := True;
    vHeight := vHeight - FHScrollBar.Height;
  end;

  // 设置滚动条的Max和Position
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

  // 设置滚动条的可见性
  FVScrollBar.Visible := vVVisible;
  FHScrollBar.Visible := vHVisible;
end;

constructor TCFGrid.Create(AOwner: TComponent);
begin
  CreateEx(AOwner, 0, 0);
end;

constructor TCFGrid.CreateEx(AOwner: TComponent; const ARowCount, AColCount: Cardinal);
begin
  inherited Create(AOwner);  // 创建 CGrid
  FDefaultColWidth := 70;
  FRowHeight := 20;
  FIndicatorWidth := 20;
  FResizeCol := -1;
  FRowIndex := -1;
  FColIndex := -1;
  FColResizing := False;
  FOptions := [cgoRowSizing, cgoColSizing, cgoIndicator, cgoRowSelect];

  FVScrollBar := TCFScrollBar.Create(Self);  // 创建垂直滚动条
  FVScrollBar.Orientation := coVertical;  // 设置滚动条为垂直类型
  FVScrollBar.OnScroll := OnVScroll;  // 绑定滚动事件
  FVScrollBar.Width := 10;
  FVScrollBar.Parent := Self;

  FHScrollBar := TCFScrollBar.Create(Self);  // 创建水平滚动条
  FHScrollBar.Orientation := coHorizontal;  // 设置滚动条类型为水平滚动条类型
  FHScrollBar.OnScroll := OnHScroll;  // 绑定滚动事件
  FHScrollBar.Height := 10;
  FHScrollBar.Parent := Self;

  FTitleRow := TTitleRow.Create(Self, AColCount);  // 创建标题行
  // 设置行和列
  FRows := TObjectList<TRow>.Create;

  if (ARowCount = 0) and (AColCount <> 0) then
    raise Exception.Create('异常：行数为0时，列数不能为大于0的数值！');
  if ARowCount <> 0 then
  begin
    RowCount := ARowCount;
    if AColCount <> 0 then
      ColCount := AColCount;
  end;

  // 设置窗体创建时的初始值
  Width := 200;
  Height := 250;
end;

procedure TCFGrid.DefaultCellDraw(const ARow, ACol: Cardinal;
  const ACanvas: TCanvas; const ALeft, ATop: Integer);
begin
  FRows[ARow][ACol].Draw(ACanvas, ALeft, ATop, FRowHeight);
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
    if FVScrollBar.Visible then  // 存在滚动条存在
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
  vEditControlType: TEditControlType;
begin
  vCancel := False;
  vEditControlType := ectEdit;
  if Assigned(FOnCellBeforeEdit) then
    FOnCellBeforeEdit(Self, ARow, ACol, vCancel, vEditControlType);
  if not vCancel then
    ShowEditControl(ARow, ACol, vEditControlType);
end;

procedure TCFGrid.DoOnEditControlKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    DoOnCellAfterEdit(FRowIndex, FColIndex);
end;

procedure TCFGrid.DrawControl(ACanvas: TCanvas);

  procedure DrawTitle(const ADrawLeft, AStartCol, AEndCol: Integer);  // 绘制标题行背景
  var
    vRight, i: Integer;
  begin
    ACanvas.Brush.Color := GTitleBackColor;
    vRight := Math.Min(GetDataDisplayRight, GetDataWidth);
    if cgoIndicator in FOptions then
      vRight := vRight + FIndicatorWidth;

    ACanvas.FillRect(Bounds(0, 0, vRight, GetTitleHeight));

    // 绘制标题行数据
    vRight := ADrawLeft;
    for i := AStartCol to AEndCol do
    begin
      FTitleRow[i].Draw(ACanvas, vRight, 0, FRowHeight);
      vRight := vRight + FTitleRow[i].Width;
    end;
  end;

  procedure DrawBorder;  // 绘制边框
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
begin
  inherited DrawControl(ACanvas);

  // 计算控件可视区域
  vDisplayRight := Width;
  if FVScrollBar.Visible then  // 垂直滚动条存在
    vDisplayRight := vDisplayRight - FVScrollBar.Width;  // 减去垂直滚动条的宽度

  if BorderVisible then  // 边框存在
    vDisplayRight := vDisplayRight - GBorderWidth;  // 减去边框

  ACanvas.FillRect(Rect(0, 0, vDisplayRight, GetDataDisplayBottom));

  // 获取可显示的列
  GetFirstColDisplay(vStartCol, vEndCol, vDrawLeft);  // 计算当前显示的起始列、结束列和起始列相对控件左侧的偏移量

  if (FRows.Count = 0) or (ColCount = 0) then
  begin
    DrawTitle(vDrawLeft, vStartCol, vEndCol);
    DrawBorder;
    Exit;
  end;

  // 获取可显示的行
  GetFirstRowDisplay(vStartRow, vEndRow, vDrawTop);  // 计算当前显示的起始行、结束行序号和起始行相对控件顶部的偏移量

  if ColCount <> 0 then  // 绘制可显示出来的数据行
  begin
    vTop := vDrawTop;
    //vDisplayRight := GetDataDisplayRight;
    for i := vStartRow to vEndRow  do
    begin
      vLeft := vDrawLeft;
      for j := vStartCol to vEndCol do
      begin
        if Assigned(FOnCellDraw) then
          FOnCellDraw(Self, i, j, ACanvas, vLeft, vTop)
        else
          DefaultCellDraw(i, j, ACanvas, vLeft, vTop);
        vLeft := vLeft + FRows[i][j].Width;
      end;

      {if i = FRowIndex then
      begin
        ACanvas.Brush.Color := GHightLightColor;
        ACanvas.FillRect(Rect(vLeft, vTop, vDisplayRight, vTop + FRowHeight));
      end
      else
        ACanvas.Brush.Color := GThemeColor;
      FRows[i].Draw(ACanvas, vLeft, vTop, vStartCol, vEndCol);}

      vTop := vTop + FRowHeight;
    end;
  end;

  DrawTitle(vDrawLeft, vStartCol, vEndCol);  // 绘制标题行背景

  // 绘制指示区
  if cgoIndicator in FOptions then
  begin
    i := Math.Min(GetDataDisplayBottom, FRows.Count * FRowHeight) + GetTitleHeight;
    ACanvas.FillRect(Rect(0, 0, FIndicatorWidth, i));
    vTop := vDrawTop;
    for i := vStartRow to vEndRow + 1 do  // +1处理当数据高度小于控件高度时最后一行指示器没有下边线
    begin
      if vTop >= GetTitleHeight then
      begin
        ACanvas.MoveTo(0, vTop);
        ACanvas.LineTo(FIndicatorWidth, vTop);
      end;
      vTop := vTop + FRowHeight;
    end;
  end;

  // 画滚动条
  if FVScrollBar.Visible then  // 垂直滚动条存在
  begin
    ACanvas.Refresh;
    i := SaveDC(ACanvas.Handle);  // 保存(设备上下文环境)现场
    try
      MoveWindowOrg(ACanvas.Handle, FVScrollBar.Left, FVScrollBar.Top);
      FVScrollBar.DrawTo(ACanvas);
    finally
      RestoreDC(ACanvas.Handle, i);  // 恢复(设备上下文环境)现场
    end;
  end;

  if FHScrollBar.Visible then  // 水平滚动条存在
  begin
    ACanvas.Refresh;
    i := SaveDC(ACanvas.Handle);  // 保存(设备上下文环境)现场
    try
      MoveWindowOrg(ACanvas.Handle, FHScrollBar.Left, FHScrollBar.Top);
      FHScrollBar.DrawTo(ACanvas);
    finally
      RestoreDC(ACanvas.Handle, i);  // 恢复(设备上下文环境)现场
    end;
  end;

  // 绘制两滚动条相交的角
  if (FVScrollBar.Visible) and (FHScrollBar.Visible) then
  begin
    ACanvas.Brush.Color := GTitleBackColor;
    if BorderVisible then  // 边框存在
      ACanvas.FillRect(Rect(Width - FVScrollBar.Width - GBorderWidth, Height - FHScrollBar.Height - GBorderWidth, Width - GBorderWidth, Height - GBorderWidth))
    else  // 边框存在
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

  // 第几行
  vRang := FVScrollBar.Rang;  // 范围
  vGridPos := ADataY
    * (vRang - (GetDataDisplayHeight - FVScrollBar.PageSize))  // 数据坐标
    / vRang;
  vRowSingle := vGridPos / FRowHeight;
  ARow := Trunc(vRowSingle);

  //第几列
  if BorderVisible then
    vRight := GBorderWidth
  else
    vRight := 0;
  if cgoIndicator in FOptions then
    vRight := vRight + FIndicatorWidth;

  for vRang := 0 to ColCount - 1 do  // 这里的vRang相当于 i 用（省变量）
  begin
    vRight := vRight + FTitleRow[vRang].Width;
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
    vCellLeft := vCellLeft + FTitleRow[i].Width;
  Result := Bounds(vCellLeft, vCellTop, FTitleRow[ACol].Width, FRowHeight);
end;

function TCFGrid.GetCellClientRect(const ARow, ACol: Integer): TRect;
begin
  Result := GetCellDataBoundRect(ARow, ACol);
  OffsetRect(Result, -FHScrollBar.Position, -FVScrollBar.Position);
  if cgoIndicator in FOptions then  // 指示器存在
    OffsetRect(Result, FIndicatorWidth, 0);  // 增加指示器的宽度
end;

function TCFGrid.GetCells(ARow, ACol: Integer): string;
begin
  if (ARow < 0) or (ARow > RowCount - 1) then
    raise Exception.Create('异常：GetCells方法参数 ARow 超出索引范围！');
  if (ACol < 0) or (ACol > ColCount - 1) then
    raise Exception.Create('异常：GetCells方法参数 ACol 超出索引范围！');
    
  Result := FRows[ARow].Items[ACol].Text;
end;

function TCFGrid.GetColCount: Cardinal;
begin
  {if FRows.Count > 0 then  // 行数不为0
    Result := FRows[0].Count
  else}
  if Assigned(FTitleRow) then
    Result := FTitleRow.Count
  else
    Result := 0;
end;

function TCFGrid.GetDataDisplayBottom: Integer;
begin
  Result := Height;
  if FHScrollBar.Visible then  // 水平滚动条
    Result := Result - FHScrollBar.Height;

  if BorderVisible then  // 边框存在
    Result := Result - GBorderWidth;
end;

function TCFGrid.GetDataDisplayHeight: Integer;
begin
  Result := Height - GetTitleHeight;
  if BorderVisible then  // 边框存在
    Result := Result - 2 * GBorderWidth;  // 减去边框
  if FHScrollBar.Visible then  // 水平滚动条存在
    Result := Result - FHScrollBar.Height;  // 水平滚动条的高度
end;

function TCFGrid.GetDataDisplayRight: Integer;
begin
  // 数据宽度
  Result := GetDataWidth;
  if cgoIndicator in FOptions then  // 指示器存在
    Result := Result + FIndicatorWidth;  // 增加指示器的宽度

  if Result < Width then Exit;

  Result := Width;
  if FVScrollBar.Visible then  // 垂直滚动条存在
    Result := Result - FVScrollBar.Width;  // 减去垂直滚动条的宽度
  if BorderVisible then  // 边框存在
    Result := Result - GBorderWidth;  // 减去边框
end;

function TCFGrid.GetDataDisplayWidth: Integer;
begin
  Result := Width;
  if FVScrollBar.Visible then  // 垂直滚动条存在
    Result := Result - FVScrollBar.Width;  // 减去垂直滚动条的宽度
  if BorderVisible then  // 边框存在
    Result := Result - 2 * GBorderWidth;  // 减去两边的边框

  if cgoIndicator in FOptions then  // 指示器存在
    Result := Result - FIndicatorWidth;  // 减去指示器的宽度
end;

procedure TCFGrid.GetFirstColDisplay(var AStartCol, AEndCol, ALeftOffset: Integer);
var
  vRight, vDspWidth: Integer;
  vGridHPos: Single;
  i: Integer;
begin
  //if RowCount = 0 then Exit;
  // 设置初值，保证能退出，不执行下面的循环
  AStartCol := -1;
  AEndCol := -2;
  vGridHPos := 0;

  vDspWidth := GetDataDisplayWidth;  // 展示数据的宽度
  if FHScrollBar.Rang > 0 then
    vGridHPos := (FHScrollBar.Position - FHScrollBar.Min)
      * (FHScrollBar.Rang - (vDspWidth - FHScrollBar.PageSize))  // 数据范围中需要滚动才能显示的大小(不能显示出来的部分)
      / FHScrollBar.Rang;

  vRight := 0;
  for i := 0 to ColCount - 1 do  // 遍历列
  begin
    vRight := vRight + FTitleRow[i].Width;  // 累加列宽
    if vRight > vGridHPos then  // 能满足的列宽找到
    begin
      AStartCol := i;  // 起始列赋值
      Break;
    end;
  end;

  if AStartCol < 0 then Exit;  // 设置的初始值能保证退出，不去找结束列

  ALeftOffset := -Round(FTitleRow[AStartCol].Width - (vRight - vGridHpos));  // 左偏移量
  if cgoIndicator in FOptions then  // 指示器存在
    ALeftOffset := ALeftOffset + FIndicatorWidth;  // 左偏移量要加指示器的宽度

  if FHScrollBar.Rang > vDspWidth then  // 数据宽度大于界面可显示数据宽度
  begin
    for i := AStartCol + 1 to ColCount - 1 do  // 遍历数据
    begin
      vRight := vRight + FTitleRow[i].Width;  // 累加列宽
      if vRight - vGridHPos >= vDspWidth then  // 找到满足的结束列
      begin
        AEndCol := i;  // 结束列
        Break;
      end;
    end;
  end
  else  // 数据宽度小于等于界面可显示数据宽度
  begin
    AEndCol := ColCount - 1;
    if AEndCol < 0 then  // 结束列为 0 时，控制结束列小于起始列，不进行数据遍历操作
      AEndCol := -2;
  end;
end;

procedure TCFGrid.GetFirstRowDisplay(var AStartRow, AEndRow, ATopOffset: Integer);
var
  vGridVPos, vfIndex: Single;
begin
  if RowCount = 0 then Exit;
  // 设置初始行，保证其初始值可以正常退出函数
  AStartRow := -1;
  AEndRow := -2;
  vGridVPos := 0;
  ATopOffset := 0;

  if FVScrollBar.Rang > 0 then
    vGridVPos := FVScrollBar.Position
      * (FVScrollBar.Rang - (GetDataDisplayHeight - FVScrollBar.PageSize))  // 数据范围中需要滚动才能显示的大小(不能显示出来的部分)
      / FVScrollBar.Rang;

  vfIndex := vGridVPos / FRowHeight;  // 开始行（实数）
  AStartRow := Trunc(vfIndex);  // 开始行（整数）
  ATopOffset := GetTitleHeight - Round(Frac(vfIndex) * FRowHeight);  // 数据向下偏移标题行高

  vfIndex := (vGridVPos + GetDataDisplayHeight) / FRowHeight;  // 结束行（实数）
  AEndRow := Trunc(vfIndex);  // 结束行（整数）
  {if Frac(vfIndex) > 0 then  // 如果不是整数，按下一行处理
    Inc(vEndRow);}
  if AEndRow > RowCount - 1 then  // AEndRow赋值为-2时，不满足
    AEndRow := RowCount - 1;
end;

function TCFGrid.GetRowCount: Cardinal;
begin
  Result := FRows.Count;  // 获取行数
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
  then  // 在标题区
  begin
    // 第几行
    ATitleRow := 0;

    //第几列
    vRight := 0;
    for i := 0 to ColCount - 1 do  // 这里的vRang相当于 i 用（省变量）
    begin
      vRight := vRight + FTitleRow[i].Width;
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
  for i := 0 to ATitleCol - 1 do  // 遍历列
    vLeft := vLeft + FTitleRow[i].Width;  // 得到列的左边
  Result := Bounds(vLeft, 0, FTitleRow[ATitleCol].Width, FRowHeight);  // 获取单元格格的区域
end;

function TCFGrid.GetTitleHeight: Integer;
begin
  Result := FRowHeight;  // 标题高度
end;

function TCFGrid.GetTitleText(ACol: Integer): string;
begin
  if (ACol < 0) or (ACol > ColCount - 1) then
    raise Exception.Create('异常：GetTitleText方法参数 ACol 超出索引范围！');
  Result := FTitleRow[ACol].Text;
end;

procedure TCFGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i, vDateX, vDateY, vOldRowIndex, vOldColIndex, vRight, vBottom: Integer;
begin
  inherited;
  if Button = mbMiddle then  // 鼠标中键按下平移开始
  begin
    FMovePt := Point(X, Y);
    Windows.SetCursor(LoadCursor(0, IDC_HAND));
    Exit;
  end;

  vRight := GetDataDisplayRight;
  vBottom := GetDataDisplayBottom;
  if cgoColSizing in FOptions then  // 指示器存在
  begin
    if PtInRect(Bounds(0, 0, vRight, GetTitleHeight), Point(X, Y)) then  // 鼠标在标题区
    begin
      if FResizeCol >= 0 then  // 要改变的列存在
      begin
        FResizeX := X;  // 改变值
        FColResizing := True;  // 此列正在改变
      end;
      Exit;
    end;
  end;

  if FVScrollBar.Visible and (X > vRight) and (Y < vBottom) then  // 鼠标点击在垂直滚动条上
  begin
    FMouseDownCtronl := FVScrollBar;  // 赋值滚动条的类型
    FVScrollBar.MouseDown(Button, Shift, X - vRight, Y);  // 触发
  end
  else
  if FHScrollBar.Visible and (Y > vBottom) and (X < vRight) then  // 鼠标点击在水平滚动条上
  begin
    FMouseDownCtronl := FHScrollBar;
    FHScrollBar.MouseDown(Button, Shift, X, Y - vBottom);
  end
  else
  if FRows.Count > 0 then  // 点击在数据区
  begin
    // 赋初值
    vOldRowIndex := FRowIndex;
    vOldColIndex := FColIndex;
    FRowIndex := -1;
    FColIndex := -1;
    vDateX := FHScrollBar.Position + X;  // 实际数据水平坐标
    vDateY := FVScrollBar.Position + Y - GetTitleHeight;  // 实际数据的垂直坐标（除去界面标题行）
    GetDataCellAt(vDateX, vDateY, FRowIndex, FColIndex);
    if (FRowIndex <> vOldRowIndex) or (FColIndex <> vOldColIndex) then  // 选中行列发生变化
    begin
      if (vOldRowIndex >= 0) and (not FReadOnly) then  // 原选中单元格
        DoOnCellAfterEdit(vOldRowIndex, vOldColIndex);  // 编辑完成
      if cgoRowSelect in FOptions then  // 行选
      begin
        if vOldRowIndex >= 0 then  // 原选中行存在
        begin
          for i := 0 to ColCount - 1 do  // 清除原选中行的各单元格的选中状态
            Exclude(FRows[vOldRowIndex][i].FStates, ccsSelected);
        end;

        if FRowIndex >= 0 then  // 新选中行存在
        begin
          for i := 0 to ColCount - 1 do  // 新选中行各单元格的选中状态
            Include(FRows[FRowIndex][i].FStates, ccsSelected);
        end;
      end
      else  // 非行选
      begin
        if vOldRowIndex >= 0 then  // 原选中单元格存在
          Exclude(FRows[vOldRowIndex][vOldColIndex].FStates, ccsSelected);

        if FRowIndex >= 0 then  // 新选中单元格
          Include(FRows[FRowIndex][FColIndex].FStates, ccsSelected);
      end;

      UpdateDirectUI;  // 优化为只绘制变动部分
      if Assigned(FOnSelectCell) then
        FOnSelectCell(Self, FRowIndex, FColIndex);
    end;

    if (FRowIndex >= 0) and (not FReadOnly) then  // 新选中单元格
      DoOnCellBeforeEdit(FRowIndex, FColIndex);
  end;
end;

procedure TCFGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vStartCol, vEndCol, vLeftOffset: Integer;
  i, vRight: Integer;
begin
  inherited;

  if Shift = [ssMiddle] then  // 鼠标中键按下
  begin
    FHScrollBar.Position := FHScrollBar.Position + (FMovePt.X - X);
    FVScrollBar.Position := FVScrollBar.Position + (FMovePt.Y - Y);
    FMovePt.X := X;
    FMovePt.Y := Y;
    Windows.SetCursor(LoadCursor(0, IDC_HAND));
    UpdateDirectUI;
    Exit;
  end;
  if cgoColSizing in FOptions then  // 指示器存在
  begin
    if FColResizing then  // 鼠标在标题区并且在列之间的线条上，鼠标移动列宽实时改变
    begin
      // 列宽实时改变
      SetColWidth(FResizeCol, FTitleRow[FResizeCol].Width + X - FResizeX);
      FResizeX := X;
      Exit;
    end;
    Cursor := crDefault;  // 鼠标的样式为普通
    FResizeCol := -1;

    if PtInRect(Bounds(0, 0, Width - FVScrollBar.Width, GetTitleHeight), Point(X, Y)) then  // 鼠标没有点下在标题区移动鼠标样式显示可以改变列宽
    begin
      GetFirstColDisplay(vStartCol, vEndCol, vLeftOffset);  // 客户区数据的起始列和结束列以及数据的偏移量
      if vStartCol >= 0 then
      begin
        vRight := vLeftOffset;
        for i := vStartCol to vEndCol do  // 从开始行到结束行找中线
        begin
          vRight := vRight + FTitleRow[i].Width;  // 客户区列右边界的位置
          if (X > vRight - 4) and (X < vRight + 5) then  // 在列之间线位置左右偏移几个像素
          begin
            FResizeCol := i;  // 要改变列的列宽
            Cursor := crHSplit;  // 改变鼠标的样式
            Break;
          end;
        end;
      end;
      Exit;
    end;
  end;

  if FMouseDownCtronl = FVScrollBar then  // 在垂直滚动条中移动
    FVScrollBar.MouseMove(Shift, X + FVScrollBar.Width - Width, Y)
  else
  if FMouseDownCtronl = FHScrollBar then  // 在水平滚动条中移动
    FHScrollBar.MouseMove(Shift, X, Y + FHScrollBar.Height - Height)
  else
  if FRows.Count > 0 then
  begin
    if ssLeft in Shift then  // 划选
    begin

    end;
  end;
end;

procedure TCFGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vTitleRow, vTitleCol: Integer;
begin
  inherited;
  if Button = mbMiddle then
  begin
    Windows.SetCursor(LoadCursor(0, IDC_ARROW));
    Exit;
  end;
  FColResizing := False;
  if FResizeCol >= 0 then  // 进行改变选定列的列宽
  begin
    //SetColWidth(FResizeCol, FTitleRow[FResizeCol].Width + X - FResizeX);
    FResizeCol := -1;  // 改变列宽之后设为没有要改变的列的列宽
    Cursor := crDefault;  // 恢复鼠标样式
    Exit;
  end;

  if FMouseDownCtronl = FVScrollBar then  // 在垂直滚动条上
    FVScrollBar.MouseUp(Button, Shift, X - GetDataDisplayRight, Y)
  else
  if FMouseDownCtronl = FHScrollBar then  // 在水平滚动条上
    FHScrollBar.MouseUp(Button, Shift, X, Y - GetDataDisplayBottom);
  if FMouseDownCtronl <> nil then  // 清除记录鼠标点击处
    FMouseDownCtronl := nil
  else  // 在非滚动条区弹起
  begin
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
  UpdateDirectUI;  // 重绘区域
end;

procedure TCFGrid.OnVScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  UpdateDirectUI;  // 绘制区域
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
    raise Exception.Create('异常：SetCells方法参数 ARow 超出索引范围！');
  if (ACol < 0) or (ACol > ColCount - 1) then
    raise Exception.Create('异常：SetCells方法参数 ACol 超出索引范围！');
    
  if Value <> FRows[ARow].Items[ACol].Text then
  begin
    FRows[ARow].Items[ACol].Text := Value;
    vCellRect := GetCellClientRect(ARow, ACol);
    OffsetRect(vCellRect, Left, Top);

    IntersectRect(vCellRect, vCellRect, ClientRect);  // 返回第2、3参数代表的矩形交集

    if not IsRectEmpty(vCellRect) then  // 单元格区域和客户端区域有交集
      UpdateDirectUI(vCellRect);
  end;
end;

procedure TCFGrid.SetColCount(Value: Cardinal);
var
  i, j, vColCount: Integer;
  vCol: TCol;
  vTitleCol: TTitleCol;
begin
  if ColCount <> Value then  // 如果列数有改变
  begin
    vColCount := ColCount;
    if Value > vColCount then  // 增加列
    begin
      // 更改标题行的列数
      for i := vColCount to Value - 1 do
      begin
        vTitleCol := TTitleCol.Create(FTitleRow);  // 增加标题列
        //vCol.Text := IntToStr(i) + '-标题';
        FTitleRow.Add(vTitleCol);  // 增加标题列
      end;
      // 更改数据行的列数
      if RowCount > 0 then
      begin
        for i := 0 to FRows.Count - 1 do  // 遍历行
        begin
          for j := vColCount to Value - 1 do  // 遍历列进行更改列数
          begin
            vCol := TCol.Create(FRows[i]);  // 创建列
            //vCol.Text := IntToStr(i) + '-' + IntToStr(j);
            FRows[i].Add(vCol);  // 增加列
          end;
        end;
      end;
    end
    else  // 减少列
    begin
      // 更改标题行的列数
      for i := vColCount - 1 downto Value do  // 向下遍历，实现减列
        FTitleRow.Delete(i);  // 删除标题列
      // 更改数据行的列数
      if RowCount > 0 then
      begin
        for i := 0 to FRows.Count - 1 do  // 遍历行
        begin
          for j := vColCount - 1 downto Value do  // 向下遍历，对数据行实现减列
            FRows[i].Delete(j);  // 减行
        end;
      end;
    end;
    CheckScrollBarVisible;
    UpdateDirectUI;
  end;
end;

procedure TCFGrid.SetColWidth(const ACol, AWidth: Integer);
var
  i, vW: Integer;
begin
  if AWidth < DefaultColWidth then  // 列宽小于默认的列宽
    vW := DefaultColWidth  // 设置列宽为默认值
  else
    vW := AWidth;  // 列宽为要设置的列宽值
  FTitleRow[ACol].Width := vW;  // 改变标题列宽
  for i := 0 to RowCount - 1 do  // 遍历数据行
    FRows[i][ACol].Width := vW;  // 数据行列宽进行改变
  CheckScrollBarVisible;
  UpdateDirectUI;  // 更新客户区数据
end;

procedure TCFGrid.SetOptions(Value: TCGridOptions);
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
    {to do: 如果编辑框在显示，设置为只读时取消编辑}
  end;
end;

procedure TCFGrid.SetRowCount(Value: Cardinal);
var
  vRow: TRow;
  i, vRowCount: Integer; 
begin
  if FRows.Count <> Value then  // 行高有改变
  begin
    vRowCount := RowCount;  // 保存原来的行数
    if Value > vRowCount then  // 要设置的行数和和原来的行数进行比较，要增加行
    begin
      for i := vRowCount to Value - 1 do  // 进行增加行
      begin
        vRow := TRow.Create(Self, ColCount);  // 创建行
        FRows.Add(vRow);  // 增加行
      end;
    end
    else  // 要实现减少行
    begin
      for i := vRowCount - 1 downto Value do  // 向下遍历行实现减行
        FRows.Delete(i);  // 减行
    end;                                              
    CheckScrollBarVisible;
    UpdateDirectUI;
  end;
end;

procedure TCFGrid.SetTitleText(ATitleCol: Integer; const Value: string);
var
  vColRect: TRect;
begin
  if (ATitleCol < 0) or (ATitleCol > ColCount - 1) then
    raise Exception.Create('异常：SetTitleText方法参数 ACol 超出索引范围！');
  if Value <> FTitleRow[ATitleCol].Text then
  begin
    FTitleRow[ATitleCol].Text := Value;
    vColRect := GetTitleColClientRect(ATitleCol);
    OffsetRect(vColRect, Left, Top);

    IntersectRect(vColRect, vColRect, ClientRect);  // 返回第2、3参数代表的矩形交集

    if not IsRectEmpty(vColRect) then  // 单元格区域和客户端区域有交集
      UpdateDirectUI(vColRect);
  end;
end;

procedure TCFGrid.ShowEditControl(const ARow, ACol: Cardinal;
  const AEditControlType: TEditControlType);
var
  vRect: TRect;
begin
  case AEditControlType of
    ectEdit:
      begin
        FEditControl := TCFEdit.Create(Self);
        with (FEditControl as TCFEdit) do
        begin
          Font := Self.Font;
          Text := Cells[ARow, ACol];
          OnKeyPress := DoOnEditControlKeyPress;
        end;
      end;

    ectCombobox: FEditControl := TCFCombobox.Create(Self);
    ectButtonEdit: FEditControl := TCFButtonEdit.Create(Self);
    ectGridEdit: FEditControl := TCFGridEdit.Create(Self);
  end;

  vRect := GetCellClientRect(ARow, ACol);
  FEditControl.Left := Left + vRect.Left;
  FEditControl.Top := Top + vRect.Top;
  FEditControl.Width := vRect.Right - vRect.Left + 1;
  FEditControl.Height := vRect.Bottom - vRect.Top + 1;

  FEditControl.Parent := Self.Parent;
  FEditControl.BringToFront;
end;

{ TCol }

constructor TCol.Create(const ARow: TRow);
begin
  inherited Create;
  FRow := ARow;
  FWidth := 70;
end;

procedure TCol.Draw(const ACanvas: TCanvas; const ALeft, ATop, AHeight: Integer);
var
  vRect: TRect;
begin
  if ccsSelected in FStates then  // 单元格被选中
    ACanvas.Brush.Color := GHightLightColor  // 高亮显示
  else
    ACanvas.Brush.Color := GBackColor;

  vRect := Rect(ALeft + 1, ATop + 1, ALeft + FWidth, ATop + AHeight);
  ACanvas.FillRect(vRect);  // 重绘要高亮显示的区域
  // 输出单元格的内容
  InflateRect(vRect, -2, -1);
  {Windows.ExtTextOut(ACanvas.Handle, ALeft + 3, ATop + 4, 0, nil, FText,
    Length(FText), nil);}
  ACanvas.TextRect(vRect, FText);  // 实现不超出单元格范围内容显示文本
  // 边框
  ACanvas.Pen.Color := GLineColor;  // 画笔线
  //ACanvas.Rectangle(Rect(ALeft, ATop, ALeft + FWidth, ATop + AHeight));
  // 画单元格的边框线
  ACanvas.MoveTo(ALeft, ATop + AHeight);  // 左下角
  ACanvas.LineTo(ALeft + FWidth, ATop + AHeight);  // 右下角
  ACanvas.LineTo(ALeft + FWidth, ATop);  // 右上角

//  TextOut(ACanvas.Handle, ALeft + 4, ATop + 4, PChar(FText), Length(FText));
//  ACanvas.TextOut(ALeft + 4, ATop + 4, FText);
//  TextHeight(FText);
//  vWCent := 4;
//  vWCent := ACanvas.TextWidth(FText);
//  vWCent := (FWidth - vWCent) div 2;
//  //InflateRect(vRect, -2, 0);
//  Windows.DrawTextEx(ACanvas.Handle, PChar(FText), -1, vRect, DT_LEFT or DT_VCENTER or DT_SINGLELINE, nil);
end;

{ TRow }

constructor TRow.Create(const AGrid: TCFGrid; const AColCount: Cardinal);
var
  vCol: TCol;
  i: Integer;
begin
  inherited Create;
  FGrid := AGrid;
  for i := 0 to AColCount - 1 do  // 遍历列
  begin
    vCol := TCol.Create(Self);  // 创建列
    Add(vCol);  // 增加列
  end;
end;

procedure TRow.Draw(const ACanvas: TCanvas; const ALeft, ATop, AStartCol, AEndCol: Integer);
var
  i, vLeft: Integer;
begin
  vLeft := ALeft;
  for i := AStartCol to AEndCol do  // 遍历能展示的列
  begin
    Items[i].Draw(ACanvas, vLeft, ATop, FGrid.RowHeight);  // 画单元格的内容
    vLeft := vLeft + Items[i].Width;  // 下一个单元格的左边要加列宽
  end;
end;

function TRow.Get(Index: Integer): TCol;
begin
  Result := TCol(inherited Get(Index));
end;

procedure TRow.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = TListNotification.lnDeleted then  // 进行通知释放
    TCol(Ptr).Free;
end;

procedure TRow.Put(Index: Integer; const Value: TCol);
begin
  inherited Put(Index, Value);
end;

{ TTitleRow }

constructor TTitleRow.Create(const AGrid: TCFGrid; const AColCount: Cardinal);
var
  vTitleCol: TTitleCol;
  i: Integer;
begin
  inherited Create;
  FGrid := AGrid;
  for i := 0 to AColCount - 1 do  // 遍历列
  begin
    vTitleCol := TTitleCol.Create(Self);  // 创建列
    Add(vTitleCol);  // 增加列
  end;
end;

procedure TTitleRow.Draw(const ACanvas: TCanvas; const ALeft, ATop, AStartCol,
  AEndCol: Integer);
var
  i, vLeft: Integer;
begin
  vLeft := ALeft;
  for i := AStartCol to AEndCol do  // 遍历能展示的列
  begin
    Items[i].Draw(ACanvas, vLeft, ATop, FGrid.RowHeight);  // 画单元格的内容
    vLeft := vLeft + Items[i].Width;  // 下一个单元格的左边要加列宽
  end;
end;

function TTitleRow.Get(Index: Integer): TTitleCol;
begin
  Result := TTitleCol(inherited Get(Index));
end;

procedure TTitleRow.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = TListNotification.lnDeleted then  // 进行通知释放
    TTitleCol(Ptr).Free;
end;

procedure TTitleRow.Put(Index: Integer; const Value: TTitleCol);
begin
  inherited Put(Index, Value);
end;

{ TTitleCol }

constructor TTitleCol.Create(const ATitleRow: TTitleRow);
begin
  inherited Create;
  //FRow := ARow;
  FWidth := 70;
end;

procedure TTitleCol.Draw(const ACanvas: TCanvas; const ALeft, ATop,
  AHeight: Integer);
begin
  ACanvas.FillRect(Rect(ALeft + 1, ATop + 1, ALeft + FWidth, ATop + AHeight));  // 重绘要高亮显示的区域
  ACanvas.Pen.Color := GLineColor;  // 画笔线
  // 画单元格的边框线
  ACanvas.MoveTo(ALeft, ATop + AHeight);  // 左下角
  ACanvas.LineTo(ALeft + FWidth, ATop + AHeight);  // 右下角
  ACanvas.LineTo(ALeft + FWidth, ATop);  // 右上角
  // 输出单元格的内容
  Windows.ExtTextOut(ACanvas.Handle, ALeft + 3, ATop + 4, 0, nil, FText,
    Length(FText), nil);
end;

end.
