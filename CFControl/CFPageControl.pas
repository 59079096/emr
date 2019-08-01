unit CFPageControl;

interface

uses
  Windows, Classes, Controls, Messages, Graphics, ImgList, Menus;

type
  TCFButtonState = (cbsDown, cbsMouseIn);
  TCFButtonStates = set of TCFButtonState;

  TCFPageButton = class(TObject)
  strict private
    FRect: TRect;
    FImageIndex, FHotImageIndex, FDownImageIndex: Integer;
    FState: TCFButtonStates;
    FOnClick, FOnUpdateView: TNotifyEvent;
    procedure DoUpdateView;
    procedure SetImageIndex(const Value: Integer);
    procedure SetHotImageIndex(const Value: Integer);
    procedure SetDownImageIndex(const Value: Integer);
  public
    constructor Create;
    procedure MouseDown;
    procedure MouseUp;
    procedure MouseEnter;
    procedure MouseLeave;
    procedure KillFocus;

    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property HotImageIndex: Integer read FHotImageIndex write FHotImageIndex;
    property DownImageIndex: Integer read FDownImageIndex write SetDownImageIndex;
    property Rect: TRect read FRect write FRect;
    property State: TCFButtonStates read FState;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnUpdateView: TNotifyEvent read FOnUpdateView write FOnUpdateView;
  end;

  TCFPageControl = class;
  TCFPage = class;

  TPageButtonEvent = procedure(const APage: TCFPage; const AButton: TCFPageButton) of object;

  TCFPage = class(TObject)
  strict private
    FText: string;
    FWidth: Integer;
    FTextSize: TSize;
    FImageIndex: Integer;
    FMouseIn, FActive: Boolean;
    FButtons: TList;
    FHotButton: TCFPageButton;
    FControl: TWinControl;
    FPageControl: TCFPageControl;
    FOnSetControl: TNotifyEvent;
    FOnUpdateView: TNotifyEvent;
    FOnImageIndexChanged: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnButtonClick: TPageButtonEvent;

    procedure DoButtonClick(Sender: TObject);
    procedure DoButtonUpdateView(Sender: TObject);
    procedure DoSetControl;
    procedure DoUpdateView;
    procedure DoImageIndexChanged;
    procedure DoResize;
    procedure CalcWidth;
    function GetButtonAt(const X, Y: Integer): TCFPageButton;
    procedure SetText(const AText: string);
    procedure SetActive(const Value: Boolean);
    procedure SetControl(const AControl: TWinControl);
    procedure SetImageIndex(const Value: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseEnter;
    procedure MouseLeave;
    procedure PintTo(const ACanvas: TCanvas);
    function AddButton: TCFPageButton;
    procedure KillFocus;

    property Text: string read FText write SetText;
    property Control: TWinControl read FControl write SetControl;
    property Width: Integer read FWidth;
    property Active: Boolean read FActive write SetActive;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property PageControl: TCFPageControl read FPageControl write FPageControl;

    property OnSetControl: TNotifyEvent read FOnSetControl write FOnSetControl;
    property OnUpdateView: TNotifyEvent read FOnUpdateView write FOnUpdateView;
    property OnImageIndexChanged: TNotifyEvent read FOnImageIndexChanged write FOnImageIndexChanged;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnButtonClick: TPageButtonEvent read FOnButtonClick write FOnButtonClick;
  end;

  TPageList = class(TList)
  private
    FOnCountChanged: TNotifyEvent;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    property OnCountChanged: TNotifyEvent read FOnCountChanged write FOnCountChanged;
  end;

  TControlButton = (cbnNone, cbnMenu, cbnLeft, cbnRight);

  TPageControlButtonEvent = procedure(const APageIndex: Integer; const AButton: TCFPageButton) of object;

  TCFPageControl = class(TCustomControl)  // TGraphicControl
  strict private
    FPageHeight, FPagePadding, FPageIndex, FHotPageIndex, FUpdateCount: Integer;
    FOffset, FPageClientRight: Integer;
    FHotControlBtn: TControlButton;
    FScrollBtnVisible, FScrolling, FBorderVisible: Boolean;
    FPages: TPageList;
    FImages: TCustomImageList;
    FPagePopupMenu: TPopupMenu;
    FActivePageColor: TColor;
    FBackGroundText: string;
    FOnChange: TNotifyEvent;
    FOnPageButtonClick: TPageControlButtonEvent;

    procedure DoPageSetControl(Sender: TObject);
    procedure DoPageUpateView(Sender: TObject);
    procedure DoImageIndexChanged(Sender: TObject);
    procedure DoPageResize(Sender: TObject);
    procedure DoPageMenuClick(Sender: TObject);
    procedure DoPageButtonClick(const APage: TCFPage; const AButton: TCFPageButton);
    procedure DoChange;
    procedure DoPageCountChanged(Sender: TObject);
    function GetCount: Integer;
    function GetPageAt(const X, Y: Integer): Integer; overload;
    function GetPageAt(const X, Y: Integer; var ARect: TRect): Integer; overload;
    function GetPageIndex(const APage: TCFPage): Integer;
    function GetPageRect(const APage: TCFPage): TRect; overload;
    function GetPageRect(const APageIndex: Integer): TRect; overload;
    function GetPageMenuIndex(const APageIndex: Integer): Integer;
    function GetButtonsRectWidth: Integer;
    procedure AddPageMenu(const APageIndex: Integer);
    procedure DeletePageMenu(const APageIndex: Integer);
    procedure FreeAllPages;
    procedure CalcPageClientRight;
    procedure ControlButtonMouseMove(const X, Y: Integer);
    procedure SetHotControlBtn(const AControlBtn: TControlButton);
    procedure SetPageMenuImageIndex(const APage: TCFPage);
    procedure SetActivePageIndex(const AIndex: Integer);
    procedure SetPageHeight(const Value: Integer);
    procedure SetBorderVisible(const Value: Boolean);
    procedure SetActivePageColor(const Value: TColor);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetBackGroundText(const Value: string);
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure UpdateView(const ARect: TRect); overload;
    procedure UpdateView; overload;
    procedure DeletePage(const AIndex: Integer); overload;
    procedure DeletePage(const APage: TCFPage); overload;
    function AddPage(const AText: string; const AControl: TWinControl = nil): TCFPage;
    function ActivePage: TCFPage;
    function GetPage(Index: Integer): TCFPage;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Count: Integer read GetCount;
    property PageIndex: Integer read FPageIndex write SetActivePageIndex;
    property Pages[Index: Integer]: TCFPage read GetPage; default;
  published
    property PageHeight: Integer read FPageHeight write SetPageHeight;
    property Images: TCustomImageList read FImages write SetImages;
    property BackGroundText: string read FBackGroundText write SetBackGroundText;
    property ActivePageColor: TColor read FActivePageColor write SetActivePageColor;
    property BorderVisible: Boolean read FBorderVisible write SetBorderVisible;
    property OnPageButtonClick: TPageControlButtonEvent read FOnPageButtonClick write FOnPageButtonClick;
    property Color;
  end;

  function CreateCanvas(const AFontSize: Integer): TCanvas;
  procedure DestroyCanvas(const ACanvas: TCanvas);

implementation

{$R CFPageControl.RES}

const
  CustomPadding = 5;
  ButtonImageWidth = 16;

function CreateCanvas(const AFontSize: Integer): TCanvas;
var
  vDC: HDC;
begin
  vDC := CreateCompatibleDC(0);
  Result := TCanvas.Create;
  Result.Handle := vDC;
  Result.Font.Size := AFontSize;
end;

procedure DestroyCanvas(const ACanvas: TCanvas);
var
  vDC: HDC;
begin
  vDC := ACanvas.Handle;
  ACanvas.Handle := 0;
  ACanvas.Free;
  DeleteDC(vDC);
end;

{ TCFPageControl }

function TCFPageControl.ActivePage: TCFPage;
begin
  Result := nil;
  if (FPageIndex >= 0) then
    Result := TCFPage(FPages[FPageIndex]);
end;

function TCFPageControl.AddPage(const AText: string; const AControl: TWinControl = nil): TCFPage;
var
  vPageIndex: Integer;
begin
  Result := TCFPage.Create;
  Result.PageControl := Self;
  Result.OnSetControl := DoPageSetControl;
  Result.OnButtonClick := DoPageButtonClick;
  Result.OnImageIndexChanged := DoImageIndexChanged;
  Result.OnResize := DoPageResize;
  Result.OnUpdateView := DoPageUpateView;

  Result.Text := AText;
  Result.Control := AControl;

  vPageIndex := FPages.Add(Result);
  SetActivePageIndex(vPageIndex);
  AddPageMenu(vPageIndex);
end;

procedure TCFPageControl.AddPageMenu(const APageIndex: Integer);
var
  vMenuItem: TMenuItem;
begin
  vMenuItem := TMenuItem.Create(FPagePopupMenu);
  vMenuItem.Caption := TCFPage(FPages[APageIndex]).Text;
  vMenuItem.ImageIndex := TCFPage(FPages[APageIndex]).ImageIndex;
  vMenuItem.Tag := APageIndex;
  vMenuItem.OnClick := DoPageMenuClick;

  FPagePopupMenu.Items.Add(vMenuItem);
end;

procedure TCFPageControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCFPageControl.CalcPageClientRight;
var
  i, vWidth: Integer;
  vRect: TRect;
begin
  vWidth := 0;
  for i := 0 to FPages.Count - 1 do
    vWidth := vWidth + TCFPage(FPages[i]).Width + FPagePadding;

  FScrollBtnVisible := vWidth > (Width - ButtonImageWidth);

  FPageClientRight := Width - GetButtonsRectWidth;

  if FPages.Count > 0 then
  begin
    vRect := GetPageRect(FPages.Count - 1);
    if vRect.Right < FPageClientRight then
    begin
      FOffset := FOffset - (FPageClientRight - vRect.Right);
      if FOffset < 0 then
        FOffset := 0;
    end;
  end;
end;

procedure TCFPageControl.Clear;
begin
  FPageIndex := -1;
  FPages.Clear;
end;

procedure TCFPageControl.CMMouseLeave(var Message: TMessage);
begin
  if FHotPageIndex >= 0 then
    TCFPage(FPages[FHotPageIndex]).MouseLeave;

  FHotPageIndex := -1;
  SetHotControlBtn(cbnNone);

  inherited;
end;

procedure TCFPageControl.ControlButtonMouseMove(const X, Y: Integer);
var
  vControlBtn: TControlButton;
  vX: Integer;
begin
  vControlBtn := cbnNone;

  vX := X - FPageClientRight;
  if FScrollBtnVisible then  // 左右切换按钮显示
  begin
    if (vX > ButtonImageWidth * 2) and (vX < ButtonImageWidth * 3) then
      vControlBtn := cbnMenu
    else
    if (vX > ButtonImageWidth) and (vX < ButtonImageWidth * 2) then
      vControlBtn := cbnRight
    else
    if (vX > 0) and (vX < ButtonImageWidth) then
      vControlBtn := cbnLeft;
  end
  else  // 只有菜单按钮显示
  if (vX > 0) and (vX < ButtonImageWidth) then
    vControlBtn := cbnMenu;

  SetHotControlBtn(vControlBtn);
end;

constructor TCFPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageIndex := -1;
  FHotPageIndex := -1;
  FOffset := 0;
  FUpdateCount := 0;
  FScrollBtnVisible := False;
  FBorderVisible := True;
  FScrolling := False;
  FHotControlBtn := cbnNone;
  FPageHeight := 24;
  FPagePadding := 10;
  FPages := TPageList.Create;
  FPages.OnCountChanged := DoPageCountChanged;
  FActivePageColor := clWhite;
  Self.Width := 300;
  Self.Height := FPageHeight;
  Self.DoubleBuffered := True;

  FPagePopupMenu := TPopupMenu.Create(Self);
  //Self.Color := clRed;
end;

procedure TCFPageControl.DeletePage(const APage: TCFPage);
var
  i: Integer;
begin
  for i := 0 to FPages.Count - 1 do
  begin
    if APage = TCFPage(FPages[i]) then
    begin
      DeletePage(i);
      Break;
    end;
  end;
end;

procedure TCFPageControl.DeletePage(const AIndex: Integer);
var
  vPageIndex: Integer;
begin
  //CalcPageClientRight;

  vPageIndex := FPageIndex;
  if vPageIndex >= AIndex then
    vPageIndex := vPageIndex - 1;

  DeletePageMenu(GetPageMenuIndex(AIndex));
  TCFPage(FPages[AIndex]).Free;
  FPages.Delete(AIndex);

  if (vPageIndex < 0) and (FPages.Count > 0) then  // 激活第0个，删除第0个
  begin
    vPageIndex := 0;
    FPageIndex := -1;
  end;

  if vPageIndex <> FPageIndex then
    SetActivePageIndex(vPageIndex);

  FHotPageIndex := vPageIndex;

  UpdateView;
end;

procedure TCFPageControl.DeletePageMenu(const APageIndex: Integer);
var
  i: Integer;
begin
  if (APageIndex >= 0) and (APageIndex < FPagePopupMenu.Items.Count) then
  begin
    FPagePopupMenu.Items.Delete(APageIndex);
    if APageIndex < FPagePopupMenu.Items.Count then
    begin
      for i := APageIndex to FPagePopupMenu.Items.Count - 1 do
        FPagePopupMenu.Items[i].Tag := FPagePopupMenu.Items[i].Tag - 1;
    end;
  end;
end;

destructor TCFPageControl.Destroy;
begin
  FreeAllPages;
  FPages.Free;
  FPagePopupMenu.Free;
  inherited Destroy;
end;

procedure TCFPageControl.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCFPageControl.DoImageIndexChanged(Sender: TObject);
begin
  SetPageMenuImageIndex(Sender as TCFPage);
end;

procedure TCFPageControl.DoPageButtonClick(const APage: TCFPage;
  const AButton: TCFPageButton);
var
  vPageIndex: Integer;
begin
  if Assigned(FOnPageButtonClick) then
  begin
    vPageIndex := GetPageIndex(APage);
    FOnPageButtonClick(vPageIndex, AButton);
  end;
end;

procedure TCFPageControl.DoPageCountChanged(Sender: TObject);
begin
  CalcPageClientRight;
end;

procedure TCFPageControl.DoPageMenuClick(Sender: TObject);
var
  vPageIndex: Integer;
begin
  if Sender is TMenuItem then
  begin
    vPageIndex := (Sender as TMenuItem).Tag;

    if (vPageIndex >= 0) and (vPageIndex <> FPageIndex) then
      SetActivePageIndex(vPageIndex);
  end;
end;

procedure TCFPageControl.DoPageResize(Sender: TObject);
begin
  FOffset := 0;
  SetActivePageIndex(FPageIndex);
  UpdateView;
end;

procedure TCFPageControl.DoPageSetControl(Sender: TObject);
begin
//  if Sender is TWinControl then
//    (Sender as TWinControl).Parent := Self;
end;

procedure TCFPageControl.DoPageUpateView(Sender: TObject);
var
  vRect: TRect;
begin
  vRect := GetPageRect(Sender as TCFPage);
  UpdateView(vRect);
end;

procedure TCFPageControl.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);

   if FUpdateCount = 0 then
     UpdateView;
end;

procedure TCFPageControl.FreeAllPages;
var
  i: Integer;
begin
  for i := 0 to FPages.Count - 1 do
    TCFPage(FPages[i]).Free;
end;

function TCFPageControl.GetPageAt(const X, Y: Integer): Integer;
var
  vRect: TRect;
begin
  Result := GetPageAt(X, Y, vRect);
end;

function TCFPageControl.GetButtonsRectWidth: Integer;
begin
  if FScrollBtnVisible then
    Result := ButtonImageWidth * 3
  else
    Result := ButtonImageWidth;
end;

function TCFPageControl.GetCount: Integer;
begin
  Result := FPages.Count;
end;

function TCFPageControl.GetPage(Index: Integer): TCFPage;
begin
  Result := FPages[Index];
end;

function TCFPageControl.GetPageAt(const X, Y: Integer; var ARect: TRect): Integer;
var
  i, vLeft: Integer;
  vPage: TCFPage;
begin
  Result := -1;

  SetRectEmpty(ARect);
  vLeft := -FOffset;
  for i := 0 to FPages.Count - 1 do
  begin
    vPage := TCFPage(FPages[i]);
    ARect := Bounds(vLeft, 0, vPage.Width, FPageHeight);

    if PtInRect(ARect, Point(X, Y)) then
    begin
      Result := i;
      Break;
    end;

    vLeft := vLeft + vPage.Width + FPagePadding;
  end;
end;

function TCFPageControl.GetPageIndex(const APage: TCFPage): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FPages.Count - 1 do
  begin
    if APage = FPages[i] then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TCFPageControl.GetPageMenuIndex(const APageIndex: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FPagePopupMenu.Items.Count - 1 do
  begin
    if FPagePopupMenu.Items[i].Tag = APageIndex then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TCFPageControl.GetPageRect(const APageIndex: Integer): TRect;
begin
  Result := GetPageRect(TCFPage(FPages[APageIndex]));
end;

function TCFPageControl.GetPageRect(const APage: TCFPage): TRect;
var
  i, vLeft: Integer;
begin
  SetRectEmpty(Result);

  vLeft := -FOffset;
  for i := 0 to FPages.Count - 1 do
  begin
    if TCFPage(FPages[i]) = APage then
    begin
      Result := Bounds(vLeft, 0, APage.Width, FPageHeight);
      Break;
    end;

    vLeft := vLeft + TCFPage(FPages[i]).Width + FPagePadding;
  end;
end;

procedure TCFPageControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vPageIndex: Integer;
  vRect: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);

  FScrolling := False;
  if not Self.Focused then
    Self.SetFocus;

  if PtInRect(Bounds(0, 0, Width, FPageHeight), Point(X, Y)) then
  begin
    vRect := Bounds(FPageClientRight, 0, GetButtonsRectWidth, FPageHeight);  // 菜单按钮区域
    if PtInRect(vRect, Point(X, Y)) then  // 点在了按钮区域
      Exit;

    vPageIndex := GetPageAt(X, Y, vRect);
    if (vPageIndex >= 0) and (vPageIndex <> FPageIndex) then
      SetActivePageIndex(vPageIndex);

    if (not FScrolling) and (vPageIndex >= 0) then
      TCFPage(FPages[vPageIndex]).MouseDown(Button, Shift, X - vRect.Left, Y - vRect.Top);
  end;
end;

procedure TCFPageControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vRect: TRect;
  vPageIndex: Integer;
begin
  if PtInRect(Bounds(0, 0, Width, FPageHeight), Point(X, Y)) then
  begin
    vRect := Bounds(FPageClientRight, 0, GetButtonsRectWidth, FPageHeight);  // 菜单按钮区域
    if PtInRect(vRect, Point(X, Y)) then  // 在按钮区域
    begin
      vPageIndex := -1;
      ControlButtonMouseMove(X, Y);
    end
    else
    begin
      vPageIndex := GetPageAt(X, Y, vRect);
      SetHotControlBtn(cbnNone);
    end;

    if vPageIndex <> FHotPageIndex then  // 鼠标新移入的和原鼠标位置的不是同一个Page
    begin
      if FHotPageIndex >= 0 then  // 如果原鼠标处有Page
        TCFPage(FPages[FHotPageIndex]).MouseLeave;

      FHotPageIndex := vPageIndex;
      if FHotPageIndex >= 0 then  // 当前鼠标处有有效的Page
        TCFPage(FPages[FHotPageIndex]).MouseEnter;
    end;

    if FHotPageIndex >= 0 then  // 当前鼠标处有有效的Page
      TCFPage(FPages[FHotPageIndex]).MouseMove(Shift, X - vRect.Left, Y - vRect.Top);
  end
  else  // 鼠标移出，由CMMouseLeave处理
    FHotPageIndex := -1;

  inherited MouseMove(Shift, X, Y);
end;

procedure TCFPageControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vPageIndex: Integer;
  vRect: TRect;
  vPt: TPoint;
begin
  if PtInRect(Bounds(0, 0, Width, FPageHeight), Point(X, Y)) then
  begin
    vRect := Bounds(FPageClientRight, 0, GetButtonsRectWidth, FPageHeight);  // 菜单按钮区域
    if PtInRect(vRect, Point(X, Y)) then  // 点在了按钮区域
    begin
      case FHotControlBtn of
        cbnNone: ;
        cbnMenu:
          begin
            vPt := Point(vRect.Left, FPageHeight);
            vPt := ClientToScreen(vPt);
            FPagePopupMenu.Popup(vPt.X, vPt.Y);
          end;

        cbnLeft:
          begin
            if FPageIndex > 0 then
              SetActivePageIndex(FPageIndex - 1);
          end;

        cbnRight:
          begin
            if FPageIndex < FPages.Count - 1 then
              SetActivePageIndex(FPageIndex + 1);
          end;
      end;

      Exit;
    end;

    vPageIndex := GetPageAt(X, Y, vRect);
    if (not FScrolling) and (vPageIndex >= 0) then
      TCFPage(FPages[vPageIndex]).MouseUp(Button, Shift, X - vRect.Left, Y - vRect.Top);
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCFPageControl.Paint;
var
  i, vLeft, vSaveIndex: Integer;
  vRect: TRect;
  vPage: TCFPage;
  vBmp: TBitmap;
begin
  inherited Paint;

  Canvas.Brush.Color := Self.Color;
  Canvas.FillRect(ClientRect);
  if FBorderVisible then  // 绘制整个底边框
  begin
    Canvas.Pen.Color := $005A5A5A;
    Canvas.MoveTo(0, FPageHeight - 1);
    Canvas.LineTo(Width, FPageHeight - 1);
  end;

  vLeft := -FOffset;
  for i := 0 to FPages.Count - 1 do
  begin
    if vLeft > FPageClientRight then
      Break;

    vPage := TCFPage(FPages[i]);

    if i = FPageIndex then  // 当前激活的压住底部边框线
      vRect := Bounds(vLeft, 0, vPage.Width, FPageHeight)
    else
      vRect := Bounds(vLeft, 0, vPage.Width, FPageHeight - 1);

    if vRect.Right > 0 then
    begin
      if i = FPageIndex then
        Canvas.Brush.Color := FActivePageColor
      else
        Canvas.Brush.Color := Self.Color;

      Canvas.FillRect(vRect);  // 填充Page标签区域

      vSaveIndex := SaveDC(Canvas.Handle);
      try
        MoveWindowOrg(Canvas.Handle, vLeft, 0);
        IntersectClipRect(Canvas.Handle, 0, 0, vPage.Width, FPageHeight - 1);
        vPage.PintTo(Canvas);
      finally
        RestoreDC(Canvas.Handle, vSaveIndex);
      end;

      if (i = FPageIndex) and FBorderVisible then  // 当前页绘制边框线
      begin
        Canvas.MoveTo(vRect.Left, vRect.Bottom);
        Canvas.LineTo(vRect.Left, vRect.Top);
        Canvas.LineTo(vRect.Right - 1, vRect.Top);
        Canvas.LineTo(vRect.Right - 1, vRect.Bottom);
      end;
    end;

    vLeft := vLeft + vPage.Width + FPagePadding;
  end;

  if FBackGroundText <> '' then
  begin
    vSaveIndex := SaveDC(Canvas.Handle);
    try
      Canvas.Font.Color := clInfoText;
      Canvas.Brush.Style := bsClear;
      Canvas.TextOut(vLeft, (FPageHeight - Canvas.TextHeight('H')) div 2, FBackGroundText);
    finally
      RestoreDC(Canvas.Handle, vSaveIndex);
    end;
  end;

  // 填充切换和菜单按钮背景区域
  Canvas.Brush.Color := Self.Color;
  vRect := Bounds(FPageClientRight, 0, GetButtonsRectWidth, Height - 1);
  Canvas.FillRect(vRect);

  vBmp := TBitmap.Create;
  try
    vBmp.Transparent := True;

    // 绘制切换按钮
    if FScrollBtnVisible then
    begin
      if FHotControlBtn = cbnLeft then
        vBmp.LoadFromResourceName(HInstance, 'LEFTHOT')
      else
        vBmp.LoadFromResourceName(HInstance, 'LEFTNOR');  // 绘制左按钮

      Canvas.Draw(vRect.Left, (FPageHeight - ButtonImageWidth) div 2, vBmp);
      vRect.Left := vRect.Left + ButtonImageWidth;

      if FHotControlBtn = cbnRight then
        vBmp.LoadFromResourceName(HInstance, 'RIGHTHOT')
      else
        vBmp.LoadFromResourceName(HInstance, 'RIGHTNOR');  // 绘制右按钮

      Canvas.Draw(vRect.Left, (FPageHeight - ButtonImageWidth) div 2, vBmp);
      vRect.Left := vRect.Left + ButtonImageWidth;
    end;

    if FHotControlBtn = cbnMenu then
      vBmp.LoadFromResourceName(HInstance, 'DOWNHOT')
    else
      vBmp.LoadFromResourceName(HInstance, 'DOWNNOR');  // 绘制菜单按钮

    Canvas.Draw(vRect.Left, (FPageHeight - ButtonImageWidth) div 2, vBmp);
  finally
    vBmp.Free;
  end;
end;

procedure TCFPageControl.SetActivePageColor(const Value: TColor);
begin
  if FActivePageColor <> Value then
  begin
    FActivePageColor := Value;
    if Self.HandleAllocated then
      UpdateView;
  end;
end;

procedure TCFPageControl.SetActivePageIndex(const AIndex: Integer);

  procedure ResetPage;
  var
    vRect: TRect;
  begin
    if FPageIndex < 0 then Exit;

    vRect := GetPageRect(FPageIndex);
    if vRect.Right > FPageClientRight then
    begin
      FScrolling := True;
      FOffset := vRect.Right + FOffset - FPageClientRight;
      UpdateView;
    end
    else
    if vRect.Left < 0 then
    begin
      FScrolling := True;
      FOffset := FOffset + vRect.Left;
      UpdateView;
    end;
  end;

var
  vOldIndex: Integer;
begin
  if FPageIndex <> AIndex then
  begin
    vOldIndex := FPageIndex;
    FPageIndex := AIndex;

    if (vOldIndex >= 0) and (vOldIndex < FPages.Count) then
      TCFPage(FPages[vOldIndex]).Active := False;

    if FPageIndex >= 0 then
    begin
      TCFPage(FPages[FPageIndex]).Active := True;
      ResetPage;
    end;

    DoChange;
  end
  else
    ResetPage;
end;

procedure TCFPageControl.SetBackGroundText(const Value: string);
begin
  if FBackGroundText <> Value then
  begin
    FBackGroundText := Value;
    if Self.HandleAllocated then
      UpdateView;
  end;
end;

procedure TCFPageControl.SetBorderVisible(const Value: Boolean);
begin
  if FBorderVisible <> Value then
  begin
    FBorderVisible := Value;
    if Self.HandleAllocated then
      UpdateView;
  end;
end;

procedure TCFPageControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  CalcPageClientRight;
end;

procedure TCFPageControl.SetHotControlBtn(const AControlBtn: TControlButton);
begin
  if AControlBtn <> FHotControlBtn then
  begin
    FHotControlBtn := AControlBtn;
    UpdateView(Rect(Width - GetButtonsRectWidth, 0, Width, FPageHeight))
  end;
end;

procedure TCFPageControl.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  FPagePopupMenu.Images := FImages;
end;

procedure TCFPageControl.SetPageHeight(const Value: Integer);
begin
  if FPageHeight <> Value then
  begin
    FPageHeight := Value;
    Self.Height := Value;
    if Self.HandleAllocated then
      UpdateView;
  end;
end;

procedure TCFPageControl.SetPageMenuImageIndex(const APage: TCFPage);
var
  vIndex: Integer;
begin
  vIndex := GetPageIndex(APage);
  if vIndex >= 0 then
  begin
    vIndex := GetPageMenuIndex(vIndex);
    if vIndex >= 0 then
      FPagePopupMenu.Items[vIndex].ImageIndex := APage.ImageIndex;
  end;
end;

procedure TCFPageControl.UpdateView(const ARect: TRect);
begin
  if FUpdateCount = 0 then
  begin
    InvalidateRect(Handle, ARect, False);
    UpdateWindow(Handle);
  end;
end;

procedure TCFPageControl.UpdateView;
begin
  UpdateView(ClientRect);
end;

procedure TCFPageControl.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if (FPageIndex >= 0) then
    ActivePage.KillFocus;
end;

{ TCFPage }

function TCFPage.AddButton: TCFPageButton;
begin
  Result := TCFPageButton.Create;
  Result.OnClick := DoButtonClick;
  Result.OnUpdateView := DoButtonUpdateView;
  FButtons.Add(Result);

  CalcWidth;
end;

procedure TCFPage.CalcWidth;
var
  vCanvas: TCanvas;
  vWidth: Integer;
begin
  vCanvas := CreateCanvas(FPageControl.Font.Size);
  try
    FTextSize := vCanvas.TextExtent(FText);

    vWidth := FTextSize.cx + CustomPadding + CustomPadding;
    vWidth := vWidth + FButtons.Count * (ButtonImageWidth + CustomPadding);

    if FImageIndex >= 0 then
      vWidth := vWidth + FPageControl.Images.Width + CustomPadding;

    if vWidth <> FWidth then
    begin
      FWidth := vWidth;
      DoResize;
    end;
  finally
    DestroyCanvas(vCanvas);
  end;
end;

constructor TCFPage.Create;
begin
  inherited Create;
  FButtons := TList.Create;
  FImageIndex := -1;
  FHotButton := nil;
  FMouseIn := False;
end;

destructor TCFPage.Destroy;
var
  i: Integer;
begin
  for i := 0 to FButtons.Count - 1 do
    TCFPageButton(FButtons[i]).Free;

  inherited Destroy;
end;

procedure TCFPage.DoButtonClick(Sender: TObject);
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self, Sender as TCFPageButton);
end;

procedure TCFPage.DoButtonUpdateView(Sender: TObject);
begin
  DoUpdateView;
end;

procedure TCFPage.DoImageIndexChanged;
begin
  if Assigned(FOnImageIndexChanged) then
    FOnImageIndexChanged(Self);
end;

procedure TCFPage.DoResize;
begin
  if Assigned(FOnResize) then
    FOnResize(Self);
end;

procedure TCFPage.DoSetControl;
begin
  if Assigned(FOnSetControl) then
    FOnSetControl(FControl);
end;

procedure TCFPage.DoUpdateView;
begin
  if Assigned(FOnUpdateView) then
    FOnUpdateView(Self);
end;

function TCFPage.GetButtonAt(const X, Y: Integer): TCFPageButton;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FButtons.Count - 1 do
  begin
    if PtInRect(TCFPageButton(FButtons[i]).Rect, Point(X, Y)) then
    begin
      Result := TCFPageButton(FButtons[i]);
      Break;
    end;
  end;
end;

procedure TCFPage.KillFocus;
begin
  if Assigned(FHotButton) then
  begin
    FHotButton.KillFocus;
    DoUpdateView;
  end;
end;

procedure TCFPage.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vButton: TCFPageButton;
begin
  vButton := GetButtonAt(X, Y);
  if Assigned(vButton) then
  begin
    vButton.MouseDown;
    DoUpdateView;
  end;
end;

procedure TCFPage.MouseEnter;
begin
  FMouseIn := True;
  DoUpdateView;
end;

procedure TCFPage.MouseLeave;
begin
  FMouseIn := False;
  if Assigned(FHotButton) then
  begin
    FHotButton.MouseLeave;
    FHotButton := nil;
  end;

  DoUpdateView;
end;

procedure TCFPage.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vButton: TCFPageButton;
begin
  vButton := GetButtonAt(X, Y);
  if vButton <> FHotButton then
  begin
    if Assigned(FHotButton) then
      FHotButton.MouseLeave;

    FHotButton := vButton;

    if Assigned(FHotButton) then
      FHotButton.MouseEnter;

    DoUpdateView;
  end;
end;

procedure TCFPage.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  vButton: TCFPageButton;
begin
  vButton := GetButtonAt(X, Y);
  if Assigned(vButton) then
  begin
    DoUpdateView;
    if (cbsDown in vButton.State) and Assigned(vButton.OnClick) then
      vButton.OnClick(vButton);

    vButton.MouseUp;
  end;
end;

procedure TCFPage.PintTo(const ACanvas: TCanvas);
var
  i, vLeft: Integer;
  vBtnRect: TRect;
  vButton: TCFPageButton;
begin
  vLeft := CustomPadding;
  if Assigned(FPageControl.Images) and (FImageIndex >= 0) then
  begin
    FPageControl.Images.Draw(ACanvas, vLeft,
      (FPageControl.PageHeight - FPageControl.Images.Height) div 2, FImageIndex);

    vLeft := vLeft + FPageControl.Images.Width + CustomPadding;
  end;

  ACanvas.TextOut(vLeft, (FPageControl.PageHeight - FTextSize.cy) div 2, FText);

  if (not FActive) and (not FMouseIn) then Exit;

  vLeft := vLeft + FTextSize.cx + CustomPadding;
  for i := 0 to FButtons.Count - 1 do
  begin
    vBtnRect := Bounds(vLeft, (FPageControl.PageHeight - ButtonImageWidth) div 2,
      ButtonImageWidth, ButtonImageWidth);

    vButton := TCFPageButton(FButtons[i]);
    vButton.Rect := vBtnRect;

    if Assigned(FPageControl.Images) then
    begin
      if cbsDown in vButton.State then
      begin
        if vButton.DownImageIndex >= 0 then
          FPageControl.Images.Draw(ACanvas, vButton.Rect.Left, vButton.Rect.Top, vButton.DownImageIndex)
        else
        if vButton.HotImageIndex >= 0 then
          FPageControl.Images.Draw(ACanvas, vButton.Rect.Left, vButton.Rect.Top, vButton.HotImageIndex)
        else
        if vButton.ImageIndex >= 0 then
          FPageControl.Images.Draw(ACanvas, vButton.Rect.Left, vButton.Rect.Top, vButton.ImageIndex);
      end
      else
      if cbsMouseIn in vButton.State then
      begin
        if vButton.HotImageIndex >= 0 then
          FPageControl.Images.Draw(ACanvas, vButton.Rect.Left, vButton.Rect.Top, vButton.HotImageIndex)
        else
        if vButton.ImageIndex >= 0 then
          FPageControl.Images.Draw(ACanvas, vButton.Rect.Left, vButton.Rect.Top, vButton.ImageIndex);
      end
      else  // if vButton.State = [] then
      begin
        if vButton.ImageIndex >= 0 then
          FPageControl.Images.Draw(ACanvas, vButton.Rect.Left, vButton.Rect.Top, vButton.ImageIndex);
      end;
    end;

    vLeft := vLeft + CustomPadding + ButtonImageWidth;
  end;
end;

procedure TCFPage.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if Assigned(FControl) then
      FControl.Visible := FActive;

    DoUpdateView;
  end;
end;

procedure TCFPage.SetControl(const AControl: TWinControl);
begin
  if FControl <> AControl then
  begin
    // 原来的控件要在这里释放吗
    FControl := AControl;
    DoSetControl;
  end;
end;

procedure TCFPage.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    CalcWidth;
    DoUpdateView;
    DoImageIndexChanged;
  end;
end;

procedure TCFPage.SetText(const AText: string);
var
  i: Integer;
begin
  if FText <> AText then
  begin
    FText := AText;
    CalcWidth;
  end;
end;

{ TCFPageButton }

constructor TCFPageButton.Create;
begin
  FImageIndex := -1;
  FHotImageIndex := -1;
  FDownImageIndex := -1;
  FState := [];
end;

procedure TCFPageButton.DoUpdateView;
begin
  if Assigned(FOnUpdateView) then
    FOnUpdateView(Self);
end;

procedure TCFPageButton.KillFocus;
begin
  FState := [];
end;

procedure TCFPageButton.MouseDown;
begin
  Include(FState, cbsDown);
end;

procedure TCFPageButton.MouseEnter;
begin
  Include(FState, cbsMouseIn);
end;

procedure TCFPageButton.MouseLeave;
begin
  FState := [];
end;

procedure TCFPageButton.MouseUp;
begin
  Exclude(FState, cbsDown);
end;

procedure TCFPageButton.SetDownImageIndex(const Value: Integer);
begin
  if FDownImageIndex <> Value then
  begin
    FDownImageIndex := Value;
    DoUpdateView;
  end;
end;

procedure TCFPageButton.SetHotImageIndex(const Value: Integer);
begin
  if FHotImageIndex <> Value then
  begin
    FHotImageIndex := Value;
    DoUpdateView;
  end;
end;

procedure TCFPageButton.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    DoUpdateView;
  end;
end;

{ TPageList }

procedure TPageList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);

  if (Action = lnAdded) or (Action = lnDeleted) then
  begin
    if Assigned(FOnCountChanged) then
      FOnCountChanged(Self);
  end;
end;

end.
