unit CFListView;

interface

uses
  Windows, Classes, Types, Messages, Graphics, Controls, Generics.Collections,
  pngimage, CFControl, CFScrollBar;

type
  TCFListView = class;
  TListViewGroup = class;

  TCustomListViewItem = class(TObject)
  private
    FListView: TCFListView;
    FGroup: TListViewGroup;
    FParent: TCustomListViewItem;
    FTitle: string;
    FObject: TObject;
    FState: TMouseState;
  protected
    function GetHeight: Integer; virtual;
    /// <summary> 画条目内容 </summary>
    procedure Draw(const ACanvas: TCanvas; const ADspLeft, ATop, ADspRight, ADspBottom: Integer); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Title: string read FTitle write FTitle;
    property ObjectEx: TObject read FObject;
    property Parent: TCustomListViewItem read FParent write FParent;
  end;

  TListViewItem = class;

  TListViewGroup = class(TCustomListViewItem)
  private
    FExpand: Boolean;  // 状态：展开、折叠
    FChilds: TObjectList<TCustomListViewItem>;
    /// <summary> 画条目内容 </summary>
    procedure Draw(const ACanvas: TCanvas; const ADspLeft, ATop, ADspRight, ADspBottom: Integer); override;
    procedure AddChild(const AItem: TCustomListViewItem);
  protected
    function GetHeight: Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Childs: TObjectList<TCustomListViewItem> read FChilds;
  end;

  TListViewItem = class(TCustomListViewItem)
  private
    FImagePng: TPngImage;
    FText: string;
  protected
    /// <summary> 获取条目中的图片 </summary>
    function GetImagePng: TPngImage;

    /// <summary> 设置条目中的内容 </summary>
    procedure SetText(Value: string);
    function GetHeight: Integer; override;
  public
    //constructor Create; virtual;
    destructor Destroy; override;

    /// <summary> 画条目内容 </summary>
    procedure Draw(const ACanvas: TCanvas; const ADspLeft, ATop, ADspRight, ADspBottom: Integer); override;

    property ImagePng: TPngImage read GetImagePng;
    property Text: string read FText write SetText;
  end;

  TOnItemDraw = procedure(Sender: TCustomListViewItem; const ACanvas: TCanvas;
    const ADspLeft, ATop, ADspRight, ADspBottom: Integer; var ADefaultDraw: Boolean) of object;

  TCFListView = class(TCFTextControl)
  private                        
    FItems: TObjectList<TCustomListViewItem>;
    FVScrollBar: TCFScrollBar;

    FGroupHeight,
    FItemHeight: Integer;

    /// <summary> 当前选中的条目 </summary>
    FSelected: TCustomListViewItem;

    /// <summary> 当前鼠标移动到的条目 </summary>
    FMouseMove: TCustomListViewItem;

    /// <summary> 鼠标移动到的条目 </summary>
    //FMouseMoveIndex: Integer;

    /// <summary> 鼠标按下的是哪个滚动条 </summary>
    FMouseDownCtronl: TCFCustomControl;
    
    /// <summary> 鼠标中键按下平移时起始坐标 </summary>
    FMovePt: TPoint;

    FAutoFreeObject: Boolean;  // 自动释放Item包含的对象

    FOnItemDraw: TOnItemDraw;  // 外部控制Item绘制事件(暂时背景还由各Item自行绘制)

    function PtInVScrollBar(const X: Integer): Boolean;

    procedure Changed(Sender: TObject);

    /// <summary> 获取条目内容 </summary>
    function GetItem(Index: Integer): TCustomListViewItem;

    /// <summary> 垂直滚动条 </summary>
    procedure OnVScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);

    /// <summary> 计算滚动条的显示位置 </summary>
    procedure CalcScrollBarDspPosition;

    /// <summary> 获取展示的第一个条目和最后一个条目 </summary>
    procedure GetFirstItemsDisplay(var AStartItem, AEndItem, ADrawTop: Integer);

    /// <summary> 检查滚动条是否显示 </summary>
    procedure CheckScrollBarVisible;

    /// <summary> 获取数据的高度 </summary>
    function GetDataHeight: Integer;

    /// <summary> 获取能显示数据的右边 </summary>
    function GetDataDisplayRight: Integer;

    /// <summary> 获取能显示数据的底部 </summary>
    function GetDataDisplayBottom: Integer;

    /// <summary> 获取能显示数据的高度 </summary>
    function GetDataDisplayHeight: Integer;

    /// <summary>
    /// 获取指定位置的Item
    /// </summary>
    /// <param name="X">整个Item数据区域的X坐标</param>
    /// <param name="Y">整个Item数据区域的Y坐标</param>
    /// <returns></returns>
    function GetItemAt(const X, Y: Integer): TCustomListViewItem;

    /// <summary> 获取指定条目的区域 </summary>
    function GetItemDisplayRect(const AItem: TCustomListViewItem): TRect;
  protected
    /// <summary> 绘制到画布 </summary>
    procedure DrawControl(ACanvas: TCanvas); override;
    procedure AdjustBounds; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    /// <summary> 设置条目的高 </summary>
    procedure SetItemHeight(Value: Integer);

    /// <summary> 获取整的条目 </summary>
    function GetItemCount: Integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
        MousePos: TPoint): Boolean; override;
    procedure CMMouseLeave(var Msg: TMessage ); message CM_MOUSELEAVE;
  public  
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;     
    /// <summary> 添加组 </summary>
    function AddGroup(const ATitle: string): TListViewGroup;
    /// <summary> 添加条目 </summary>
    function AddItem(const ATitle, AText: string; const AGroup: TListViewGroup; AObject: Pointer = nil): TListViewItem;
    procedure Clear;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TCustomListViewItem read GetItem; default;
    property Selected: TCustomListViewItem read FSelected;
  published
    property GroupHeight: Integer read FGroupHeight write FGroupHeight;
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property AutoFreeObject: Boolean read FAutoFreeObject write FAutoFreeObject;
    property OnItemDraw: TOnItemDraw read FOnItemDraw write FOnItemDraw;
    property OnClick;
    property OnDBlClick;
  end;

implementation

{$R CFListView.RES}

{ TListViewItem }

destructor TListViewItem.Destroy;
begin
  if FImagePng <> nil then
    FImagePng.Free;
  inherited Destroy;
end;

procedure TListViewItem.Draw(const ACanvas: TCanvas; const ADspLeft, ATop,
  ADspRight, ADspBottom: Integer);
var
  vLeft, vTop, vFontSize, vTitleHeight, vTextHeight: Integer;
  vDefaultDraw: Boolean;
begin
  { 绘制背景 }
  if cmsMouseDown in FState then  // 如果条目选中
    ACanvas.Brush.Color := GDownColor
  else
  if cmsMouseIn in FState then  // 鼠标移动到的条目
    ACanvas.Brush.Color := GHotColor
  else
    ACanvas.Brush.Color := GThemeColor;
  ACanvas.FillRect(Rect(ADspLeft, ATop, ADspRight, ATop + FListView.ItemHeight));

  { 控件绘制Item事件 }
  if Assigned(FListView.OnItemDraw) then  // ListView控件绘制Item事件
  begin
    vDefaultDraw := True;
    FListView.OnItemDraw(Self, ACanvas, ADspLeft, ATop, ADspRight, ADspBottom, vDefaultDraw);
    if not vDefaultDraw then Exit;
  end;

  { 绘制图片 }
  if FImagePng <> nil then  // 如果有图片
  begin
    ACanvas.Draw(ADspLeft + GRoundSize, ATop + (FListView.ItemHeight - FImagePng.Height) div 2, FImagePng);  // 画图片
    vLeft := ADspLeft + GRoundSize + FImagePng.Width + GPadding;  // 先画图片，之后要进行输出文字
  end
  else
    vLeft := ADspLeft + GRoundSize;

  vTextHeight := ACanvas.TextHeight('字');  // 字体的高度

  vFontSize := ACanvas.Font.Size;  // 字体的大小
  ACanvas.Font.Size := vFontSize + 2;
  vTitleHeight := ACanvas.TextHeight('字');  // 标题的高度

  ACanvas.Font.Color := clBlack;  // 字体的颜色
  vTop := ATop + (FListView.ItemHeight - vTitleHeight - vTextHeight - GPadding) div 2;  // 字体的高度
  ACanvas.TextOut(vLeft, vTop, FTitle);  // 输出字标题

  // 输出内容
  vTop := vTop + vTitleHeight + GPadding;
  ACanvas.Font.Size := vFontSize;
  ACanvas.Font.Color := clMedGray;
  ACanvas.TextOut(vLeft, vTop, FText);
  // 不同项目之间的间隔线
  {vTop := ATop + FListView.ItemHeight;
  ACanvas.Pen.Color := GLineColor;
  ACanvas.MoveTo(vLeft, vTop);
  ACanvas.LineTo(ADspRight, vTop);}  // -1解决向上移动时一个像素的偏差，FillRect会偏差处一个像素
end;

function TListViewItem.GetHeight: Integer;
begin
  Result := FListView.ItemHeight;
end;

function TListViewItem.GetImagePng: TPngImage;
begin
  if FImagePng = nil then  // 图片不为空，进行创建并为条目添加图片
    FImagePng := TPngImage.Create;
  Result := FImagePng;
end;

procedure TListViewItem.SetText(Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    FListView.UpdateDirectUI;  { TODO : 约束绘制当前Item所在区域 }
  end;
end;

{ TCFListView }

function TCFListView.AddGroup(const ATitle: string): TListViewGroup;
begin
  Result := TListViewGroup.Create;
  Result.FListView := Self;
  Result.Title := ATitle;
  FItems.Add(Result);
  CalcScrollBarDspPosition;
  CheckScrollBarVisible;
end;

function TCFListView.AddItem(const ATitle, AText: string; const AGroup: TListViewGroup;
  AObject: Pointer = nil): TListViewItem;
begin
  Result := TListViewItem.Create;
  Result.FListView := Self;
  Result.Title := ATitle;  // 添加条目的标题
  Result.Text := AText;  // 添加条目的内容
  Result.FObject := AObject;
  if AGroup <> nil then
  begin
    AGroup.AddChild(Result);
    Result.Parent := AGroup;
  end
  else
    FItems.Add(Result);
  CalcScrollBarDspPosition;
  CheckScrollBarVisible;
end;

procedure TCFListView.AdjustBounds;
var
  vNewWidth, vNewHeight: Integer;
begin
  if not (csReading in ComponentState) then
  begin
    if Width < 120 then
      vNewWidth := 120
    else
      vNewWidth := Width;
    if Height < 100 then
      vNewHeight := 100
    else
      vNewHeight := Height;
    SetBounds(Left, Top, vNewWidth, vNewHeight);
  end;
end;

procedure TCFListView.CalcScrollBarDspPosition;
begin
  if BorderVisible then  // 边框存在
  begin
    FVScrollBar.Left := Width - FVScrollBar.Width - GBorderWidth;  // 滚动条的左边
    FVScrollBar.Top := GBorderWidth;
    FVScrollBar.Height := Height - 2 * GBorderWidth;
  end
  else  // 边框不存在
  begin
    FVScrollBar.Left := Width - FVScrollBar.Width;
    FVScrollBar.Top := 0;
    FVScrollBar.Height := Height;
  end;
end;

procedure TCFListView.Changed(Sender: TObject);
begin
  CalcScrollBarDspPosition;
  CheckScrollBarVisible;
  UpdateDirectUI;
end;

procedure TCFListView.CheckScrollBarVisible;
var
  vMax,  // 滚动条的最大值
  vHeight  // 能展示数据的高度
    : Integer;
  vVisible: Boolean;
begin
  vVisible := False;
  vMax := GetDataHeight;
  vHeight := Height;
  if BorderVisible then  // 如果边框存在
    vHeight := Height - 2 * GBorderWidth;
  vVisible := vMax > vHeight;  // 如果数据的高度大于能展示数据的高度，显示滚动条
  // 设置滚动条的最大值和翻页的数据值
  FVScrollBar.Max := vMax;
  FVScrollBar.PageSize := vHeight;

  if vVisible then  // 需要显示
  begin
    if FVScrollBar.Visible then  // 当前已经显示，重新处理下位置（处理vMax和调用前有变动但不影响显示状态的情况）
      FVScrollBar.Position := FVScrollBar.Position
    else
      FVScrollBar.Visible := True;
  end
  else
    FVScrollBar.Visible := False;

  UpdateDirectUI;
end;

procedure TCFListView.Clear;
begin
  FItems.Clear;
end;

procedure TCFListView.CMMouseLeave(var Msg: TMessage);
var
  vRect: TRect;
begin
  inherited;
  if FMouseMove <> nil then
  begin
    FMouseMove.FState := FMouseMove.FState - [cmsMouseIn];
    vRect := GetItemDisplayRect(FMouseMove);
    UpdateDirectUI(vRect);
    FMouseMove := nil;
  end;
end;

constructor TCFListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGroupHeight := 20;
  FItemHeight := 50;
  FAutoFreeObject := True;
  //FMouseMoveIndex := -1;
  FItems := TObjectList<TCustomListViewItem>.Create;

  FVScrollBar := TCFScrollBar.Create(Self);  // 创建垂直滚动条
  FVScrollBar.Orientation := coVertical;  // 设置滚动条为垂直类型
  FVScrollBar.OnScroll := OnVScroll;  // 绑定滚动事件
  FVScrollBar.Visible := False;
end;

destructor TCFListView.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TCFListView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
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
  inherited;
end;

procedure TCFListView.DrawControl(ACanvas: TCanvas);
var
  i, vDrawTop, vDspLeft, vDspRight, vDspBottom, vStartItem, vEndItem: Integer;
  //vbGroup: Boolean;
begin
  ACanvas.Brush.Color := GThemeColor;
  vDspRight := GetDataDisplayRight;
  vDspBottom := GetDataDisplayHeight;
  ACanvas.FillRect(Rect(0, 0, vDspRight, GetDataDisplayBottom));  // 填充背景

  // 绘制数据
  if BorderVisible then  // 边框存在
    vDspLeft := GBorderWidth
  else  // 边框不存在
    vDspLeft := 0;

  if FItems.Count <> 0 then  // 有条目存在
  begin
    GetFirstItemsDisplay(vStartItem, vEndItem, vDrawTop);  // 返回屏显第一个和最后一个，如为Group则Group整组算做一个Item
    vDrawTop := vDrawTop - FVScrollBar.Position;
    for i := vStartItem to vEndItem do  // 遍历条目
    begin
      FItems[i].Draw(ACanvas, vDspLeft, vDrawTop, vDspRight, vDspBottom);  // 画条目
      vDrawTop := vDrawTop + FItems[i].GetHeight;
    end;
  end;

  // 画滚动条
  if FVScrollBar.Visible then
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

  // 绘制边框
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

function TCFListView.GetDataDisplayBottom: Integer;
begin
  Result := Height;
  if BorderVisible then  // 如果显示边框
    Result := Result - GBorderWidth;  // 减去下边框
end;

function TCFListView.GetDataDisplayHeight: Integer;
begin
  Result := GetDataDisplayBottom;
  if BorderVisible then  // 如果显示边框
    Result := Result - GBorderWidth;  // 减去上边框
end;

function TCFListView.GetDataDisplayRight: Integer;
begin
  Result := Width;
  if FVScrollBar.Visible then
    Result := Result - FVScrollBar.Width;
  if BorderVisible then  // 如果显示边框
    Result := Result - GBorderWidth;  // 减去下边框
end;

function TCFListView.GetDataHeight: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FItems.Count - 1 do
    Result := Result + FItems[i].GetHeight;  // 数据的高度
end;

procedure TCFListView.GetFirstItemsDisplay(var AStartItem, AEndItem, ADrawTop: Integer);
var
  vVPos: Single;
  i, vHeight: Integer;
begin
  if FItems.Count = 0 then Exit;
  AStartItem := -1;
  AEndItem := -1;
  ADrawTop := 0;
  vVPos := 0;

  if FVScrollBar.Rang > 0 then
    vVPos := FVScrollBar.Position
      * (FVScrollBar.Rang - (GetDataDisplayHeight - FVScrollBar.PageSize))  // 数据范围中需要滚动才能显示的大小(不能显示出来的部分)
      / FVScrollBar.Rang;

  // 获取开始行
  if BorderVisible then  // 如果有边框
    vHeight := GBorderWidth
  else
    vHeight := 0;
  ADrawTop := vHeight;

  for i := 0 to FItems.Count - 1 do
  begin
    vHeight := vHeight + FItems[i].GetHeight;
    if vHeight > vVpos then
    begin
      AStartItem := i;
      Break;
    end;
    ADrawTop := vHeight;  // 计算开始行在整体中的起始位置
  end;

  // 获取结束行
  AEndItem := FItems.Count - 1;
  vVPos := vVPos + GetDataDisplayHeight;
  vHeight := ADrawTop;
  for i := AStartItem to FItems.Count - 1 do
  begin
    vHeight := vHeight + FItems[i].GetHeight;
    if vHeight > vVpos then
    begin
      AEndItem := i;
      Break;
    end;
  end;
end;

function TCFListView.GetItem(Index: Integer): TCustomListViewItem;
begin
  Result := FItems[Index];  // 获取条目
end;

function TCFListView.GetItemAt(const X, Y: Integer): TCustomListViewItem;
var
  vPt: TPoint;
  vWidth: Integer;

  function FindItemAt(var ATop: Integer; const AItems: TObjectList<TCustomListViewItem>): TCustomListViewItem;
  var
    i, vHeight: Integer;
    vGroup: TListViewGroup;
  begin
    Result := nil;
    for i := 0 to AItems.Count - 1 do  // 得到坐标指定的条目
    begin
      vHeight := AItems[i].GetHeight;
      if PtInRect(Bounds(0, ATop, vWidth, vHeight), vPt) then  // 条目在指定的区域
      begin
        Result := AItems[i];
        Break;
      end
      else
        ATop := ATop + vHeight;
    end;

    if (Result <> nil) and (Result is TListViewGroup) then  // 在组的区域内
    begin
      vGroup := Result as TListViewGroup;
      if vGroup.FExpand and  // 不在组Item的区域内，找展开的子Item
        (not PtInRect(Bounds(0, ATop, vWidth, FGroupHeight), vPt))
      then
      begin
        ATop := ATop + FGroupHeight;
        Result := FindItemAt(ATop, vGroup.Childs);
      end;
    end;
  end;

var
  vTop: Integer;

begin
  Result := nil;
  vPt := Point(X, Y);
  vWidth := GetDataDisplayRight;
  {if BorderVisible then  // 边框存在
    vTop := GBorderWidth
  else}  // 边框不存在
  vTop := 0;
  Result := FindItemAt(vTop, FItems);
end;

function TCFListView.GetItemCount: Integer;
begin
  Result := FItems.Count;  // 获取条目的总数目
end;

function TCFListView.GetItemDisplayRect(const AItem: TCustomListViewItem): TRect;

  function GetItemTop(var vTop: Integer; const AItems: TObjectList<TCustomListViewItem>): Boolean;
  var
    i, vHeight: Integer;
    vGroup: TListViewGroup;
  begin
    Result := False;
    for i := 0 to AItems.Count - 1 do  // 得到坐标指定的条目
    begin
      if AItems[i] = AItem then Exit(True);

      if AItems[i] is TListViewGroup then
      begin
        vGroup := AItems[i] as TListViewGroup;
        if vGroup.FExpand then
        begin
          vTop := vTop + FGroupHeight;
          Result := GetItemTop(vTop, vGroup.Childs);
          if Result then Exit;

          Continue;
        end;
      end;
      vTop := vTop + AItems[i].GetHeight;
    end;
  end;

var
  vTop: Integer;
begin
  if BorderVisible then  // 边框存在
    vTop := GBorderWidth
  else
    vTop := 0;

  if GetItemTop(vTop, FItems) then
  begin
    Result := Bounds(0, vTop, GetDataDisplayRight, AItem.GetHeight);  // 返回条目的区域
    if FVScrollBar.Visible then  // 如果边框存在，进行偏移
      OffsetRect(Result, 0, -FVScrollBar.Position);
  end;
end;

procedure TCFListView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vRect: TRect;
  vOldSelected: TCustomListViewItem;
begin
  inherited;
  FMouseDownCtronl := nil;
  if Button = mbMiddle then  // 鼠标中键按下平移开始
  begin
    FMovePt := Point(X, Y);
    Windows.SetCursor(LoadCursor(0, IDC_HAND));
    Exit;
  end;

  if PtInVScrollBar(X) then
  begin
    FVScrollBar.MouseDown(Button, Shift, X + FVScrollBar.Width - Width, Y);
    FMouseDownCtronl := FVScrollBar;
    Exit;
  end;

  vOldSelected := FSelected;
  FSelected := GetItemAt(X, Y + FVScrollBar.Position);  // 如果鼠标点击下选中的条目有改变
  if FSelected <> vOldSelected then  // 原来选中的和现在鼠标点下去的条目不同
  begin
    // 修改原来条目的状态
    if vOldSelected <> nil then
    begin
      vOldSelected.FState := vOldSelected.FState - [cmsMouseDown];
      if vOldSelected is TListViewItem then
      begin
        vRect := GetItemDisplayRect(vOldSelected);
        UpdateDirectUI(vRect);
      end;
    end;
    // 修改新条目的状态
    if FSelected <> nil then
    begin
      FSelected.FState := FSelected.FState + [cmsMouseDown];
      if FSelected is TListViewItem then
      begin
        vRect := GetItemDisplayRect(FSelected);
        UpdateDirectUI(vRect);
      end;
    end;
  end;
  if FSelected = nil then
  begin
    if ssLeft in Shift then  // and (Parent.HandleAllocated)
    Begin
      ReleaseCapture;
      SendMessage(Parent.Handle, WM_SYSCOMMAND, $F011, 0);  {参数3在 $F011-$F01F 之间均可都是移动控件}
    end;
  end;
end;

procedure TCFListView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vOldMoveItem: TCustomListViewItem;
  vRect: TRect;
begin
  inherited;
  if Shift = [ssMiddle] then  // 鼠标中间按下
  begin
    FVScrollBar.Position := FVScrollBar.Position + (FMovePt.Y - Y);
    FMovePt.X := X;
    FMovePt.Y := Y;
    Windows.SetCursor(LoadCursor(0, IDC_HAND));
    UpdateDirectUI;
    Exit;
  end;

  if PtInVScrollBar(X) or (FMouseDownCtronl = FVScrollBar) then
    FVScrollBar.MouseMove(Shift, X + FVScrollBar.Width - Width, Y);

  vOldMoveItem := FMouseMove;
  FMouseMove := GetItemAt(X, Y + FVScrollBar.Position);
  if FMouseMove <> vOldMoveItem then
  begin
    if vOldMoveItem <> nil then
    begin
      vOldMoveItem.FState := vOldMoveItem.FState - [cmsMouseIn];
      vRect := GetItemDisplayRect(vOldMoveItem);
      UpdateDirectUI(vRect);
    end;
    if FMouseMove <> nil then
    begin
      FMouseMove.FState := FMouseMove.FState + [cmsMouseIn];
      vRect := GetItemDisplayRect(FMouseMove);
      UpdateDirectUI(vRect);
    end;
  end;
end;

procedure TCFListView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vUpItem: TCustomListViewItem;
begin
  inherited;
  if Button = mbMiddle then
  begin
    Windows.SetCursor(LoadCursor(0, IDC_ARROW));
    Exit;
  end;

  if FMouseDownCtronl <> nil then
  begin
    FMouseDownCtronl := nil;
    Exit;
  end;

  if PtInVScrollBar(X) then
  begin
    FVScrollBar.MouseUp(Button, Shift, X + FVScrollBar.Width - Width, Y);
    Exit;
  end;

  vUpItem := GetItemAt(X, Y + FVScrollBar.Position);  // 如果鼠标点击下选中的条目有改变
  if FSelected = vUpItem then
  begin
    if FSelected is TListViewGroup then
    begin
      (FSelected as TListViewGroup).FExpand := not (FSelected as TListViewGroup).FExpand;
      CheckScrollBarVisible;
    end;
  end;
end;

procedure TCFListView.OnVScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin 
  UpdateDirectUI;  // 重绘区域
end;

function TCFListView.PtInVScrollBar(const X: Integer): Boolean;
begin
  Result := False;
  if FVScrollBar.Visible then  // 如果滚动条存在
  begin
    if BorderVisible then  // 如果边框存在
    begin
      if (X > Width - GBorderWidth - FVScrollBar.Width) and (X < Width - GBorderWidth)  then  // 鼠标点击在滚动条范围内
        Result := True;
    end
    else  // 边框不存在
    begin
      if (X > Width - FVScrollBar.Width) and (X < Width)  then  // 鼠标点击在滚动条范围内
        Result := True;
    end;
  end;
end;

procedure TCFListView.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if HandleAllocated then
  begin
    CheckScrollBarVisible;
    CalcScrollBarDspPosition;
  end;
end;

procedure TCFListView.SetItemHeight(Value: Integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    UpdateDirectUI;
  end;
end;

{ TCustomListViewItem }

constructor TCustomListViewItem.Create;
begin
end;

destructor TCustomListViewItem.Destroy;
begin
  if (FListView.FAutoFreeObject) and (FObject <> nil) then
    FObject.Free;
  inherited;
end;

procedure TCustomListViewItem.Draw(const ACanvas: TCanvas; const ADspLeft, ATop,
  ADspRight, ADspBottom: Integer);
begin
end;

function TCustomListViewItem.GetHeight: Integer;
begin
end;

{ TListViewGroup }

procedure TListViewGroup.AddChild(const AItem: TCustomListViewItem);
begin
  FChilds.Add(AItem);
  AItem.FGroup := Self;
end;

constructor TListViewGroup.Create;
begin
  inherited;
  FChilds := TObjectList<TCustomListViewItem>.Create;
end;

destructor TListViewGroup.Destroy;
begin
  FChilds.Free;
  inherited;
end;

procedure TListViewGroup.Draw(const ACanvas: TCanvas; const ADspLeft, ATop,
  ADspRight, ADspBottom: Integer);
var
  vRect: TRect;
  vIcon: HICON;
  i, vTop: Integer;
  vDefaultDraw: Boolean;
begin
  if ATop >= ADspBottom then Exit;
  { 绘制背景 }
  vRect := Rect(ADspLeft, ATop, ADspRight, ATop + FListView.GroupHeight);
  {if i = FItemIndex then  // 如果条目选中
    ACanvas.Brush.Color := GDownColor
  else}
  if cmsMouseIn in FState then  // 鼠标移动到的条目
    ACanvas.Brush.Color := GBorderHotColor
  else
    ACanvas.Brush.Color := GThemeColor;
  ACanvas.FillRect(vRect);

  { 控件绘制Item事件 }
  if Assigned(FListView.OnItemDraw) then  // ListView控件绘制Item事件
  begin
    vDefaultDraw := True;
    FListView.OnItemDraw(Self, ACanvas, ADspLeft, ATop, ADspRight, ADspBottom, vDefaultDraw);
    if not vDefaultDraw then Exit;
  end;

  if FExpand then  // 展开
    vIcon := LoadIcon(HInstance, 'EXPAND')
  else
    vIcon := LoadIcon(HInstance, 'UNEXPAND');
  DrawIconEx(ACanvas.Handle, ADspLeft, ATop + (FListView.GroupHeight - GIconWidth) div 2, vIcon, GIconWidth, GIconWidth, 0, 0, DI_NORMAL);

  ACanvas.Font.Color := clBlack;  // 字体的颜色
  vRect := Bounds(ADspLeft + GIconWidth, ATop, FListView.Width - GIconWidth, FListView.GroupHeight);
  ACanvas.TextRect(vRect, FTitle, [tfSingleLine, tfLeft, tfVerticalCenter]);  // 输出标题

  vTop := ATop + FListView.GroupHeight;
  if FExpand then
  begin
    for i := 0 to FChilds.Count - 1 do
    begin
      if vTop >= ADspBottom then
        Break;
      FChilds[i].Draw(ACanvas, ADspLeft, vTop, ADspRight, ADspBottom);
      vTop := vTop + FChilds[i].GetHeight;
    end;
  end;
end;

function TListViewGroup.GetHeight: Integer;
var
  i: Integer;
begin
  Result := FListView.GroupHeight;
  if FExpand then
  begin
    for i := 0 to FChilds.Count - 1 do
      Result := Result + FChilds[i].GetHeight;
  end;
end;

end.
