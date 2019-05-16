unit CFColorCombobox;

interface

uses
  Windows, Classes, Controls, Graphics, Messages, CFControl, CFEdit, CFPopup,
  CFCombobox, CFListBox;

type
  TCloseUpEvent = procedure(const AItemIndex, AItemX, AItemY: Integer; var ACanCloseUp: Boolean) of Object;

  TCFColorCombobox = class(TCFEdit)
  private
    FButtonRect: TRect;
    FPopup: TCFPopup;
    FOnCloseUp: TCloseUpEvent;
    FBtnMouseState: TMouseState;
    FStyle: TComboBoxStyle;
    FDropDownCount: Byte;

    procedure PopupItem;
    function GetDropHeight: Integer;
    function GetDropDownFont: TFont;
    procedure SetDropDownFont(Value: TFont);
    procedure DoOnPopupClose(Sender: TObject);
    procedure DoOnPopupDrawWindow(const ADC: HDC; const AClentRect: TRect);
    function GetPopupWidth: Integer;
    function GetPopupHeight: Integer;
  protected
    procedure DrawControl(ACanvas: TCanvas); override;
    function GetItems: TStrings;
    procedure SetItems(const Value: TStrings);
    procedure SetDropDownCount(Value: Byte);
    function GetItemHeight: Integer;
    procedure SetItemHeight(Value: Integer);
    procedure SetOnDrawItemEvent(Value: TDrawItemEvent);
    function GetOnDrawItemEvent: TDrawItemEvent;
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    function GetZoomSelected: Boolean;
    procedure SetZoomSelected(Value: Boolean);
    procedure SetStyle(Value: TComboBoxStyle);
    //
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var Msg: TMessage ); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage ); message CM_MOUSELEAVE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    // 支持弹出下拉列表使用的事件和消息
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMCLBUTTONDOWN(var Message: TMessage); message WM_C_LBUTTONDOWN;
    procedure WMCLBUTTONUP(var Message: TMessage); message WM_C_LBUTTONUP;
    procedure WMCMOUSEMOVE(var Message: TMessage); message WM_C_MOUSEMOVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Delete(Index: Integer);
    procedure Clear;
  published
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property Items: TStrings read GetItems write SetItems;
    property DropDownCount: Byte read FDropDownCount write SetDropDownCount default 8;
    property DropDownFont: TFont read GetDropDownFont write SetDropDownFont;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Style: TComboBoxStyle read FStyle write SetStyle default csDropDown;
    property ZoomSelected: Boolean read GetZoomSelected write SetZoomSelected;
    property PopupWidth: Integer read GetPopupWidth;
    property PopupHeight: Integer read GetPopupHeight;

    property OnDrawItem: TDrawItemEvent read GetOnDrawItemEvent write SetOnDrawItemEvent;
    property OnCloseUp: TCloseUpEvent read FOnCloseUp write FOnCloseUp;
  end;

implementation

{ TCFColorCombobox }

procedure TCFColorCombobox.Clear;
begin
  //FListBox.Clear;
end;

procedure TCFColorCombobox.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  UpdateDirectUI;
end;

procedure TCFColorCombobox.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FBtnMouseState := FBtnMouseState - [cmsMouseIn];
  UpdateDirectUI;
end;

constructor TCFColorCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //
  //FListBox := TComListBox.Create(Self);
  //FListBox.BorderVisible := False;
  FDropDownCount := 8;
  FBtnMouseState := [];
  RightPadding := GIconWidth + 2 * GSpace;
  Width := Width + GIconWidth;
end;

procedure TCFColorCombobox.Delete(Index: Integer);
begin
//  if FListBox.Count > 0 then
//  begin
//    FListBox.Delete(Index);
//    FListBox.Height := GetDropHeight;
//    FPopup.SetSize(FListBox.Width, FListBox.Height);
//    FPopup.UpdatePopup;
//  end;
end;

destructor TCFColorCombobox.Destroy;
begin
  //FListBox.Free;
  inherited;
end;

procedure TCFColorCombobox.DoOnPopupDrawWindow(const ADC: HDC;
  const AClentRect: TRect);
var
  vCanvas: TCanvas;
begin
  vCanvas := TCanvas.Create;
  try
    vCanvas.Handle := ADC;
    //FListBox.DrawTo(vCanvas);
    vCanvas.Handle := 0;
  finally
    vCanvas.Free;
  end;
end;

procedure TCFColorCombobox.DoOnPopupClose(Sender: TObject);
begin
//  if FListBox.ItemIndex < 0 then Exit;
//  Text := FListBox.Items[FListBox.ItemIndex];
  UpdateDirectUI;
end;

procedure TCFColorCombobox.DrawControl(ACanvas: TCanvas);

  procedure DrawDownArrow;
  var
    vIcon: HICON;
    vRect: TRect;
    //Details: TThemedElementDetails;  // uses Themes
  begin
    if cmsMouseDown in FBtnMouseState then
    begin
      ACanvas.Pen.Color := GBorderHotColor;
      ACanvas.MoveTo(FButtonRect.Left, FButtonRect.Top);
      ACanvas.LineTo(FButtonRect.Left, FButtonRect.Bottom);

      vRect := FButtonRect;
      InflateRect(vRect, -1, -1);
      ACanvas.Brush.Color := $00E5C27F;
      ACanvas.FillRect(vRect);
    end
    else
    if cmsMouseIn in FBtnMouseState then
    begin
      ACanvas.Pen.Color := GBorderColor;
      ACanvas.MoveTo(FButtonRect.Left, FButtonRect.Top);
      ACanvas.LineTo(FButtonRect.Left, FButtonRect.Bottom);

      {if ThemeServices.ThemesEnabled then
      begin
        Details := ThemeServices.GetElementDetails(tcDropDownButtonHot); // 这里画个按钮处于 Hot 状态下的样子
        //PerformEraseBackground(Self, Canvas.Handle);  // 擦除画按钮时的背景
        ThemeServices.DrawElement(ACanvas.Handle, Details, vRect);
        ThemeServices.DrawText(ACanvas.Handle, Details, Caption, vRect,
          DT_EXPANDTABS or DT_VCENTER or DT_CENTER or DT_SINGLELINE, 0);
      end;}

      vRect := FButtonRect;
      InflateRect(vRect, -1, -1);
      ACanvas.Brush.Color := GHotColor;
      //ACanvas.Brush.Color := $00FCE5BC;
      ACanvas.FillRect(vRect);
    end;
    vIcon := LoadIcon(HInstance, 'DROPDOWN');
    DrawIconEx(ACanvas.Handle, Width - RightPadding + GSpace, (Height - GIconWidth) div 2,
      vIcon, GIconWidth, GIconWidth, 0, 0, DI_NORMAL);
  end;

var
  vRect: TRect;
begin
  if FStyle <> csDropDownList then
    inherited
  else
  begin
    // 外观，圆角矩形
    vRect := Rect(0, 0, Width, Height);
    ACanvas.Brush.Color := GTitleBackColor;
    if Focus or (cmsMouseIn in MouseState) then
      ACanvas.Pen.Color := GBorderHotColor
    else
      ACanvas.Pen.Color := GBorderColor;
    if BorderVisible then
      ACanvas.Pen.Style := psSolid
    else
      ACanvas.Pen.Style := psClear;
    ACanvas.RoundRect(vRect, GRoundSize, GRoundSize);
    if Text <> '' then
    begin
      // 设置可绘制区域
      vRect.Left := vRect.Left + LeftPadding;
      vRect.Right := vRect.Right - RightPadding;
      Windows.DrawText(ACanvas.Handle, Text, -1, vRect, DT_LEFT or DT_VCENTER or DT_SINGLELINE);
    end;
  end;
  DrawDownArrow;
end;

function TCFColorCombobox.GetDropDownFont: TFont;
begin
//  Result := FListBox.Font;
end;

function TCFColorCombobox.GetDropHeight: Integer;
begin
//  if FDropDownCount > FListBox.Count then
//  begin
//    Result := FListBox.Count * FListBox.ItemHeight;
//    if FListBox.ZoomSelected and (FListBox.ItemIndex >= 0) then
//      Result := Result + FListBox.ItemHeight;
//  end
//  else
//    Result := FDropDownCount * FListBox.ItemHeight;
//
//  if FListBox.BorderVisible then
//    Result := Result + GBorderWidth * 2
end;

function TCFColorCombobox.GetItemHeight: Integer;
begin
//  Result := FListBox.ItemHeight;
end;

function TCFColorCombobox.GetItemIndex: Integer;
begin
//  Result := FListBox.ItemIndex;
end;

function TCFColorCombobox.GetItems: TStrings;
begin
//  Result := FListBox.Items;
end;

function TCFColorCombobox.GetOnDrawItemEvent: TDrawItemEvent;
begin
//  Result := FListBox.OnDrawItem;
end;

function TCFColorCombobox.GetPopupHeight: Integer;
begin
//  Result := FListBox.Height;
end;

function TCFColorCombobox.GetPopupWidth: Integer;
begin
//  Result := FListBox.Width;
end;

function TCFColorCombobox.GetZoomSelected: Boolean;
begin
//  Result := FListBox.ZoomSelected;
end;

procedure TCFColorCombobox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if ReadOnly then Exit;

  // 弹出处理放在MouseDown中，实现弹出框显示时再点击按钮执行关闭弹出框
  if PtInRect(FButtonRect, Point(X, Y)) then
  begin
    FBtnMouseState := FBtnMouseState + [cmsMouseDown];
    UpdateDirectUI(FButtonRect);
    PopupItem;
  end
  else
  begin
    if FStyle <> csDropDownList then
      inherited;
  end;
end;

procedure TCFColorCombobox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FStyle <> csDropDownList then
    inherited;
  if PtInRect(FButtonRect, Point(X, Y)) then
  begin
    if not (cmsMouseIn in FBtnMouseState) then
    begin
      FBtnMouseState := FBtnMouseState + [cmsMouseIn];
      UpdateDirectUI(FButtonRect);
    end;
  end
  else
  begin
    if cmsMouseIn in FBtnMouseState then
    begin
      FBtnMouseState := FBtnMouseState - [cmsMouseIn];
      UpdateDirectUI(FButtonRect);
    end;
  end;
end;

procedure TCFColorCombobox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if ReadOnly then Exit;
  if cmsMouseDown in FBtnMouseState then
  begin
    FBtnMouseState := FBtnMouseState - [cmsMouseDown];
    UpdateDirectUI(FButtonRect);
  end
  else
  if FStyle <> csDropDownList then
    inherited;
end;

procedure TCFColorCombobox.PopupItem;
begin
  FPopup := TCFPopup.Create(Self);
  try
    FPopup.PopupControl := Self;
    FPopup.OnDrawWindow := DoOnPopupDrawWindow;
    FPopup.OnPopupClose := DoOnPopupClose;

    //FListBox.Height := GetDropHeight;
    //FPopup.SetSize(FListBox.Width, FListBox.Height);
    FPopup.Popup(Self);
  finally
    FPopup.Free;
  end;
end;

procedure TCFColorCombobox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  FButtonRect := Bounds(Width - RightPadding, 0, RightPadding, Height);
  if BorderVisible then
    InflateRect(FButtonRect, -GBorderWidth, -GBorderWidth);
//  if FListBox <> nil then
//    FListBox.Width := Width - 2;
end;

procedure TCFColorCombobox.SetDropDownCount(Value: Byte);
begin
  if Value < 5 then Exit;
  if FDropDownCount <> Value then
    FDropDownCount := Value;
end;

procedure TCFColorCombobox.SetDropDownFont(Value: TFont);
begin
  //FListBox.Font := Value;
end;

procedure TCFColorCombobox.SetItemHeight(Value: Integer);
begin
//  if FListBox.ItemHeight <> Value then
//    FListBox.ItemHeight := Value;
end;

procedure TCFColorCombobox.SetItemIndex(Value: Integer);
begin
//  FListBox.ItemIndex := Value;
//  if FListBox.ItemIndex >= 0 then
//    Text := FListBox.Items[FListBox.ItemIndex];
end;

procedure TCFColorCombobox.SetItems(const Value: TStrings);
begin
//  FListBox.Items := Value;
end;

procedure TCFColorCombobox.SetOnDrawItemEvent(Value: TDrawItemEvent);
begin
//  FListBox.OnDrawItem := Value;
end;

procedure TCFColorCombobox.SetStyle(Value: TComboBoxStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    UpdateDirectUI;
  end;
end;

procedure TCFColorCombobox.SetZoomSelected(Value: Boolean);
begin
  //FListBox.ZoomSelected := Value;
end;

procedure TCFColorCombobox.WMCLBUTTONDOWN(var Message: TMessage);
begin
  //FListBox.Perform(Message.Msg, Message.WParam, Message.LParam);
end;

procedure TCFColorCombobox.WMCLBUTTONUP(var Message: TMessage);
var
  vCanClose: Boolean;
begin
//  if FListBox.Perform(Message.Msg, Message.WParam, Message.LParam) = 1 then
//  begin
//    vCanClose := True;
//    if Assigned(FOnCloseUp) then
//      FOnCloseUp(ItemIndex, Message.LParam and $FFFF, Message.LParam shr 16, vCanClose);
//    if vCanClose then
//      FPopup.ClosePopup(False);
//  end;
end;

procedure TCFColorCombobox.WMCMOUSEMOVE(var Message: TMessage);
var
  vItemIndex: Integer;
begin
  vItemIndex := ItemIndex;
//  if FListBox.Perform(Message.Msg, Message.WParam, Message.LParam) = 1 then
//  begin
//    if (vItemIndex < 0) or (ItemIndex < 0) then  // 从没选中换到选中，或从选中换到没选中都需要重新计算ListBox高度
//    begin
//      FListBox.Height := GetDropHeight;
//      FPopup.SetSize(FListBox.Width, FListBox.Height);
//    end;
//    FPopup.UpdatePopup;
//  end;
end;

procedure TCFColorCombobox.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if FStyle <> csDropDownList then
    inherited;
end;

procedure TCFColorCombobox.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if FPopup.Opened then
  begin
//    if FListBox.Perform(Message.Msg, Message.WheelDelta, Message.YPos shl 16 + Message.XPos) = 1 then
//      FPopup.UpdatePopup;
  end
  else
   inherited;
end;

end.
