unit CFDateRang;

interface

uses
  Windows, Classes, Controls, Graphics, Messages, CFControl, CFDateTimePicker,
  CFPopupForm, CFCombobox, CFButton;

type
  TRangCombobox = class(TCFCombobox);
  TRangButton = class(TCFButton);

  TDROwnerControl = class(TCFCustomControl)
  protected
    procedure DrawControl(ACanvas: TCanvas); override;
  end;

  TOnSelectDateTime = procedure(const ABeginDT, AEndDT: TDateTime) of object;

  TRangPopup = class(TCFPopupForm)
  private
    FControlOwner: TDROwnerControl;
    FSplitSize: Byte;
    FMouseControl: TCFCustomControl;
    FButton: TRangButton;
    FYearComBox, FMonthComBox: TRangCombobox;
    FOnSelectDateTime: TOnSelectDateTime;
    procedure DoOnButtonClick(Sender: TObject);
    procedure DoOnComboxCloseUp(const AItemIndex, AItemX, AItemY: Integer; var ACanCloseUp: Boolean);
    procedure DoOnPopupClose(Sender: TObject);
  protected
    procedure Popup(const AControl: TControl); overload; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property OnSelectDateTime: TOnSelectDateTime read FOnSelectDateTime write FOnSelectDateTime;
  end;

  TPicker = class(TCFDateTimePicker);

  TBeginPicker = class(TPicker)
  protected
    procedure DrawControl(ACanvas: TCanvas); override;
  end;

  TEndPicker = class(TPicker)
  protected
    procedure DrawControl(ACanvas: TCanvas); override;
  end;

  TCFDateRang = class(TCFTextControl)
  private
    FFocus: Boolean;
    FSplitWidth: Byte;
    FBeginPicker: TBeginPicker;
    FEndPicker: TEndPicker;
    procedure CalcPickerPosition;
    procedure PopupRangSelect;
    procedure DoOnPopupSelectDateTime(const ABeginDT, AEndDT: TDateTime);
  protected
    procedure DrawControl(ACanvas: TCanvas); override;
    procedure AdjustBounds; override;

    function GetBeginDate: TDateTime;
    function GetEndDate: TDateTime;

    function GetFormat: string;
    procedure SetFormat(Value: string);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);

    function GetButtonVisible: Boolean;
    procedure SetButtonVisible(Value: Boolean);

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var Msg: TMessage ); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage ); message CM_MOUSELEAVE;
    //
    procedure WMCLBUTTONDOWN(var Message: TMessage); message WM_CF_LBUTTONDOWN;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BeginDateTime: TDateTime read GetBeginDate;
    property EndDateTime: TDateTime read GetEndDate;
  published
    property Format: string read GetFormat write SetFormat;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property ButtonVisible: Boolean read GetButtonVisible write SetButtonVisible;
  end;

implementation

uses
  SysUtils, DateUtils;

const
  StartYear = 2019;
  EndYear = 2099;

{ TCFDateRang }

procedure TCFDateRang.AdjustBounds;
var
  vDC: HDC;
  vWidth, vHeight: Integer;
begin
  vDC := GetDC(0);
  try
    Canvas.Handle := vDC;
    Canvas.Font := Font;

    vWidth := Canvas.TextWidth(FormatDateTime('YYYY-MM-DD', Now)) * 2 + FSplitWidth + GetSystemMetrics(SM_CYBORDER) * 2;
    if vWidth < Width then
      vWidth := Width;

    vHeight := Canvas.TextHeight('荆') + GetSystemMetrics(SM_CYBORDER) * 4 + GBorderWidth + GBorderWidth;;
    if vHeight < Height then
      vHeight := Height;

    Canvas.Handle := 0;
  finally
    ReleaseDC(0, vDC);
  end;

  SetBounds(Left, Top, vWidth, vHeight);
end;

procedure TCFDateRang.CalcPickerPosition;
begin
  if (not Assigned(FBeginPicker)) or (not Assigned(FEndPicker)) then Exit;

  FBeginPicker.Left := 0;
  FBeginPicker.Top := 0;

  FEndPicker.Left := FBeginPicker.Left + FBeginPicker.Width + GetSystemMetrics(SM_CYBORDER) * 2 + FSplitWidth;
  FEndPicker.Top := 0;
end;

procedure TCFDateRang.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  UpdateDirectUI
end;

procedure TCFDateRang.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  UpdateDirectUI;
end;

constructor TCFDateRang.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSplitWidth := 16;
  FBeginPicker := TBeginPicker.Create(Self);
  FBeginPicker.Parent := Self;

  FEndPicker := TEndPicker.Create(Self);
  FEndPicker.Parent := Self;

  SetFormat('YYYY-MM-DD');
  Width := FBeginPicker.Width * 2 + FSplitWidth + GetSystemMetrics(SM_CYBORDER) * 2;
  Height := FBeginPicker.Height;
end;

destructor TCFDateRang.Destroy;
begin
  FBeginPicker.Free;
  FEndPicker.Free;
  inherited;
end;

procedure TCFDateRang.DoOnPopupSelectDateTime(const ABeginDT, AEndDT: TDateTime);
begin
  BeginUpdate;
  try
    FBeginPicker.DateTime := ABeginDT;
    FEndPicker.DateTime := AEndDT;
  finally
    EndUpdate;
  end;
end;

procedure TCFDateRang.DrawControl(ACanvas: TCanvas);
var
  vRect: TRect;
begin
  inherited DrawControl(ACanvas);

  // 外观，圆角矩形
  vRect := Rect(0, 0, Width, Height);
  ACanvas.Brush.Style := bsSolid;

  if ReadOnly then
    ACanvas.Brush.Color := GReadOlnyBackColor;

  if BorderVisible then
  begin
    if FFocus or (cmsMouseIn in MouseState) then
      ACanvas.Pen.Color := GBorderHotColor
    else
      ACanvas.Pen.Color := GBorderColor;

    ACanvas.Pen.Width := GBorderWidth;
    ACanvas.Pen.Style := psSolid;
  end
  else
    ACanvas.Pen.Style := psClear;

  if RoundCorner > 0 then
    ACanvas.RoundRect(vRect, GRoundSize, GRoundSize)
  else
    ACanvas.Rectangle(vRect);

  ACanvas.Brush.Color := GLineColor;
  if MouseState <> [] then
  begin
    vRect := Bounds(FBeginPicker.Left + FBeginPicker.Width + GetSystemMetrics(SM_CYBORDER) + 4, (Height div 2) - 1,
      2, 2);

    ACanvas.FillRect(vRect);
    vRect.Offset(4, 0);
    ACanvas.FillRect(vRect);
    vRect.Offset(4, 0);
    ACanvas.FillRect(vRect);
  end
  else
    ACanvas.FillRect(Bounds(FBeginPicker.Left + FBeginPicker.Width + GetSystemMetrics(SM_CYBORDER), Height div 2,
      FSplitWidth, 1));
end;

function TCFDateRang.GetBeginDate: TDateTime;
begin
  Result := FBeginPicker.DateTime;
end;

function TCFDateRang.GetButtonVisible: Boolean;
begin
  Result := FBeginPicker.ButtonVisible;
end;

function TCFDateRang.GetEndDate: TDateTime;
begin
  Result := FEndPicker.DateTime;
end;

function TCFDateRang.GetFormat: string;
begin
  Result := FBeginPicker.Format;
end;

function TCFDateRang.GetReadOnly: Boolean;
begin
  Result := FBeginPicker.ReadOnly;
end;

procedure TCFDateRang.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if (X > FBeginPicker.BoundsRect.Right) and (X < FEndPicker.BoundsRect.Left) then
    PopupRangSelect;
end;

procedure TCFDateRang.PopupRangSelect;
var
  vPopup: TRangPopup;
begin
  vPopup := TRangPopup.Create(Self);
  try
    vPopup.OnSelectDateTime := DoOnPopupSelectDateTime;
    vPopup.Alignment := taCenter;
    vPopup.Popup(Self);
  finally
    vPopup.Free;
  end;
end;

procedure TCFDateRang.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  vDC: HDC;
  vWidth, vHeight: Integer;
begin
  vDC := GetDC(0);
  try
    Canvas.Handle := vDC;
    Canvas.Font := Font;

    vWidth := Canvas.TextWidth(FormatDateTime('YYYY-MM-DD', Now)) * 2 + FSplitWidth + GetSystemMetrics(SM_CYBORDER) * 2;
    if vWidth < AWidth then
      vWidth := AWidth;

    vHeight := Canvas.TextHeight('荆') + GetSystemMetrics(SM_CYBORDER) * 4 + GBorderWidth + GBorderWidth;;
    if vHeight < AHeight then
      vHeight := AHeight;

    Canvas.Handle := 0;
  finally
    ReleaseDC(0, vDC);
  end;

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  CalcPickerPosition;
end;

procedure TCFDateRang.SetButtonVisible(Value: Boolean);
begin
  if FBeginPicker.ButtonVisible <> Value then
  begin
    BeginUpdate;
    try
      FBeginPicker.ButtonVisible := Value;
      FEndPicker.ButtonVisible := Value;
      CalcPickerPosition;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCFDateRang.SetFormat(Value: string);
begin
  if FBeginPicker.Format <> Value then
  begin
    FBeginPicker.Format := Value;
    FEndPicker.Format := Value;
    UpdateDirectUI;
  end;
end;

procedure TCFDateRang.SetReadOnly(Value: Boolean);
begin
  if FBeginPicker.ReadOnly <> Value then
  begin
    FBeginPicker.ReadOnly := Value;
    FEndPicker.ReadOnly := Value;
  end;
end;

procedure TCFDateRang.WMCLBUTTONDOWN(var Message: TMessage);
//var
//  vPopup: TCPopup;
begin
//  vPopup := TCPopup.Create(Self);
//  try
//    vPopup.PopupControl := Self;
//    //FPopup.OnDrawWindow := DoOnPopupDrawWindow;
//    vPopup.SetSize(FBeginPicker.Width, 120);
//    vPopup.Popup(Left, Top);
//  finally
//    vPopup.Free;
//  end;
end;

{ TRangPopup }

constructor TRangPopup.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  FSplitSize := 5;
  FControlOwner := TDROwnerControl.Create(nil);

  FYearComBox := TRangCombobox.Create(Self);
  //FYearComBox.Font.Size := 8;
  FYearComBox.Style := csDropDownList;
  FYearComBox.Left := FSplitSize;
  FYearComBox.Top := FSplitSize;
  FYearComBox.OnCloseUp := DoOnComboxCloseUp;
  FYearComBox.Width := 100;
  for i := StartYear to EndYear do
    FYearComBox.Items.Add(IntToStr(i));

  FYearComBox.ItemIndex := 0;
  FYearComBox.Parent := FControlOwner;

  FControlOwner.Width := FYearComBox.Width + FSplitSize * 2;

  FMonthComBox := TRangCombobox.Create(Self);
  //FMonthComBox.Font.Size := 8;
  FMonthComBox.Style := csDropDownList;
  FMonthComBox.Left := FYearComBox.Left;
  FMonthComBox.Top := FYearComBox.Top + FYearComBox.Height + FSplitSize;
  FMonthComBox.OnCloseUp := DoOnComboxCloseUp;
  FMonthComBox.Width := 100;

  FMonthComBox.Items.Add('上半年');
  FMonthComBox.Items.Add('下半年');

  for i := 1 to 4 do
    FMonthComBox.Items.Add(IntToStr(i) + '季度');

  for i := 1 to 12 do
    FMonthComBox.Items.Add(IntToStr(i) + '月');

  FMonthComBox.ItemIndex := 0;
  FMonthComBox.Parent := FControlOwner;

  FButton := TRangButton.Create(Self);
  FButton.ParentFont := False;
  FButton.ModalResult := mrOk;
  FControlOwner.Height := FYearComBox.Height * 2 + FButton.Height + FSplitSize * 4;
  FButton.Left := (FControlOwner.Width - FButton.Width) div 2;
  FButton.Top := FMonthComBox.Top + FMonthComBox.Height + FSplitSize;
  FButton.OnClick := DoOnButtonClick;
  FButton.Parent := FControlOwner;
  //
  OnPopupClose := DoOnPopupClose;

  FControlOwner.Parent := (AOwner as TWinControl);
  Self.PopupControl := FControlOwner;
end;

destructor TRangPopup.Destroy;
begin
  FYearComBox.Free;
  FMonthComBox.Free;
  FButton.Free;
  FControlOwner.Free;
  inherited;
end;

procedure TRangPopup.DoOnButtonClick(Sender: TObject);
begin
  ClosePopup(False);
end;

procedure TRangPopup.DoOnComboxCloseUp(const AItemIndex, AItemX,
  AItemY: Integer; var ACanCloseUp: Boolean);
begin
  UpdatePopup;
end;

procedure TRangPopup.DoOnPopupClose(Sender: TObject);
var
  vBegDT, vEndDT: TDateTime;
  vYear: Word;
begin
  if Assigned(FOnSelectDateTime) then
  begin
    vYear := FYearComBox.ItemIndex + StartYear;

    case FMonthComBox.ItemIndex of
      0:  // 上半年
        begin
          vBegDT := StartOfAMonth(vYear, 1);
          vEndDT := EndOfAMonth(vYear, 6);
        end;

      1:  // 下半年
        begin
          vBegDT := StartOfAMonth(vYear, 7);
          vEndDT := EndOfAMonth(vYear, 12);
        end;

      2:  // 1季度
        begin
          vBegDT := StartOfAMonth(vYear, 1);
          vEndDT := EndOfAMonth(vYear, 3);
        end;

      3:  // 2季度
        begin
          vBegDT := StartOfAMonth(vYear, 4);
          vEndDT := EndOfAMonth(vYear, 6);
        end;

      4:  // 3季度
        begin
          vBegDT := StartOfAMonth(vYear, 7);
          vEndDT := EndOfAMonth(vYear, 9);
        end;

      5:  // 4季度
        begin
          vBegDT := StartOfAMonth(vYear, 10);
          vEndDT := EndOfAMonth(vYear, 12);
        end;

      6..17:  // 1-12月
        begin
          vBegDT := StartOfAMonth(vYear, FMonthComBox.ItemIndex + 1);
          vEndDT := EndOfAMonth(vYear, FMonthComBox.ItemIndex + 1);
        end;
    end;

    FOnSelectDateTime(vBegDT, vEndDT);
  end;
end;

procedure TRangPopup.Popup(const AControl: TControl);
begin
  inherited;
end;

{ TDROwnerControl }

procedure TDROwnerControl.DrawControl(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Pen.Color := GBorderColor;
  ACanvas.Rectangle(ClientRect);
end;

{ TBeginPicker }

procedure TBeginPicker.DrawControl(ACanvas: TCanvas);
begin
  inherited DrawControl(ACanvas);
  if Self.BorderVisible then
  begin
    ACanvas.Pen.Color := GBackColor;
    ACanvas.MoveTo(Width - 1, BorderWidth + 1);
    ACanvas.LineTo(Width - 1, Height - BorderWidth - 1);
    ACanvas.MoveTo(Width - GIconWidth, BorderWidth + 1);
    ACanvas.LineTo(Width - GIconWidth, Height - BorderWidth - 1);
  end;
end;

{ TEndPicker }

procedure TEndPicker.DrawControl(ACanvas: TCanvas);
begin
  inherited DrawControl(ACanvas);
  if Self.BorderVisible then
  begin
    ACanvas.Pen.Color := GBackColor;
    ACanvas.MoveTo(0, BorderWidth + 1);
    ACanvas.LineTo(0, Height - BorderWidth - 1);
    ACanvas.MoveTo(Width - GIconWidth, BorderWidth + 1);
    ACanvas.LineTo(Width - GIconWidth, Height - BorderWidth - 1);
  end;
end;

end.
