unit CFGridEdit;

interface

uses
  Windows, Classes, Controls, Graphics, Messages, CFControl, CFButtonEdit,
  CFDBGrid, CFGrid, CFPopup, DB, Forms;

type
  TEditGrid = class(TCFDBGrid)
  private
    procedure WMCFMOUSEMOVE(var Message: TMessage); message WM_CF_MOUSEMOVE;
    procedure WMCFLBUTTONDOWN(var Message: TMessage); message WM_CF_LBUTTONDOWN;
    procedure WMCFLBUTTONUP(var Message: TMessage); message WM_CF_LBUTTONUP;
  end;

  TCFieldName = type string;

  TCFGridEdit = class(TCFButtonEdit)
  private
    FDropDownCount: Byte;
    FGrid: TEditGrid;
    FPopup: TCFPopup;
    FKeyField, FValueField: TCFieldName;
    FKey: string;
    FValue: string;
    FOnCloseUp: TNotifyEvent;

    procedure PopupGrid;
    function GetDropHeight: Integer;
    procedure DoOnPopupDrawWindow(const ADC: HDC; const AClentRect: TRect);
  protected
    procedure DoButtonClick; override;
    procedure SetDropDownCount(Value: Byte);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function GetFields: TCGridFields;
    procedure SetFields(const Value: TCGridFields);
    function GetPopupWidth: Integer;
    procedure SetPopupWidth(const Value: Integer);
    function GetPopupHeight: Integer;
    procedure SetPopupHeight(const Value: Integer);
    // 支持弹出下拉列表使用的事件和消息
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMCFLBUTTONDOWN(var Message: TMessage); message WM_CF_LBUTTONDOWN;
    procedure WMCFLBUTTONUP(var Message: TMessage); message WM_CF_LBUTTONUP;
    procedure WMCFMOUSEMOVE(var Message: TMessage); message WM_CF_MOUSEMOVE;
    procedure WMCFLBUTTONDBLCLK(var Message: TMessage); message WM_CF_LBUTTONDBLCLK;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromDataSet(const ADataSet: TDataSet);
    function FieldByName(const AFieldName: string): TCField;
    property Key: string read FKey write FKey;
    property Value: string read FValue write FValue;
  published
    property DropDownCount: Byte read FDropDownCount write SetDropDownCount default 1;
    property Fields: TCGridFields read GetFields write SetFields;
    property KeyField: TCFieldName read FKeyField write FKeyField;
    property ValueField: TCFieldName read FValueField write FValueField;
    property PopupWidth: Integer read GetPopupWidth write SetPopupWidth;
    property PopupHeight: Integer read GetPopupHeight write SetPopupHeight;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
  end;

implementation

{ TCFGridEdit }

constructor TCFGridEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropDownCount := 7;
  FGrid := TEditGrid.Create(Self);
  FGrid.ReadOnly := True;
  FGrid.Options := FGrid.Options - [cgoIndicator];
  FGrid.Visible := False;
  FGrid.Parent := Self;
end;

destructor TCFGridEdit.Destroy;
begin
  FGrid.Free;
  inherited;
end;

procedure TCFGridEdit.DoButtonClick;
begin
  inherited DoButtonClick;

  if not (csDesigning in ComponentState) then
  begin
    if ReadOnly then Exit;
    PopupGrid;
  end;
end;

procedure TCFGridEdit.DoOnPopupDrawWindow(const ADC: HDC;
  const AClentRect: TRect);
var
  vCanvas: TCanvas;
begin
  vCanvas := TCanvas.Create;
  try
    vCanvas.Handle := ADC;
    FGrid.DrawTo(vCanvas);
    vCanvas.Handle := 0;
  finally
    vCanvas.Free;
  end;
end;

function TCFGridEdit.FieldByName(const AFieldName: string): TCField;
begin
  Result := FGrid.FieldByName(AFieldName);
end;

function TCFGridEdit.GetFields: TCGridFields;
begin
  Result := FGrid.Fields;
end;

function TCFGridEdit.GetPopupHeight: Integer;
begin
  Result := FGrid.Height;
end;

function TCFGridEdit.GetPopupWidth: Integer;
begin
  Result := FGrid.Width;
end;

function TCFGridEdit.GetDropHeight: Integer;
begin
  if FDropDownCount > FGrid.RowCount then
    Result := FGrid.RowCount * FGrid.RowHeight
  else
    Result := FDropDownCount * FGrid.RowHeight;

  if FGrid.BorderVisible then
    Result := Result + GBorderWidth * 2;

  Result := Result + 2 * FGrid.RowHeight;  // 标题行高度、水平滚动条高度
end;

procedure TCFGridEdit.LoadFromDataSet(const ADataSet: TDataSet);
begin
  FGrid.LoadFromDataSet(ADataSet);
end;

procedure TCFGridEdit.PopupGrid;
begin
  FPopup := TCFPopup.Create(Self);
  try
    FPopup.PopupControl := Self;
    FPopup.OnDrawWindow := DoOnPopupDrawWindow;
    FGrid.Height := GetDropHeight;
    FPopup.SetSize(FGrid.Width, FGrid.Height);
    FPopup.Popup(Self);
  finally
    FPopup.Free;
    FPopup := nil;
  end;
end;

procedure TCFGridEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if FGrid <> nil then
  begin
    if FGrid.Width < AWidth then
      FGrid.Width := AWidth;
  end;
end;

procedure TCFGridEdit.SetFields(const Value: TCGridFields);
begin
  FGrid.Fields := Value;
end;

procedure TCFGridEdit.SetPopupHeight(const Value: Integer);
begin
  FGrid.Height := Value;
end;

procedure TCFGridEdit.SetPopupWidth(const Value: Integer);
begin
  FGrid.Width := Value;
end;

procedure TCFGridEdit.SetDropDownCount(Value: Byte);
begin
  if Value < 5 then Exit;
  if FDropDownCount <> Value then
    FDropDownCount := Value;
end;

procedure TCFGridEdit.WMCFLBUTTONDBLCLK(var Message: TMessage);
begin
  if FGrid.RowIndex >= 0 then
  begin
    if FValueField <> '' then
    begin
      Text := FGrid.FieldByName(FValueField).AsString; //Text := FGrid.Cells[FGrid.RowIndex, FGrid.Fields.Indexof(FValueField)];
      FValue := Text;
    end;

    if FKeyField <> '' then
      FKey := FGrid.FieldByName(FKeyField).AsString;  // FGrid.Cells[FGrid.RowIndex, FGrid.Fields.Indexof(FKeyField)];

    FPopup.ClosePopup(False);
    if Assigned(FOnCloseUp) then
      FOnCloseUp(Self);
  end;
end;

procedure TCFGridEdit.WMCFLBUTTONDOWN(var Message: TMessage);
begin
  if FGrid.Perform(Message.Msg, Message.WParam, Message.LParam) = 1 then
    FPopup.UpdatePopup;
end;

procedure TCFGridEdit.WMCFLBUTTONUP(var Message: TMessage);
begin
  FGrid.Perform(Message.Msg, Message.WParam, Message.LParam);
end;

procedure TCFGridEdit.WMCFMOUSEMOVE(var Message: TMessage);
begin
  if FGrid.Perform(Message.Msg, Message.WParam, Message.LParam) = 1 then
    FPopup.UpdatePopup;
end;

procedure TCFGridEdit.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if (FPopup <> nil) and FPopup.Opened then
  begin
    if FGrid.Perform(Message.Msg, Message.WheelDelta, Message.YPos shl 16 + Message.XPos) = 1 then
      FPopup.UpdatePopup;
  end
  else
   inherited;
end;

{ TEditGrid }

procedure TEditGrid.WMCFLBUTTONDOWN(var Message: TMessage);
//var
//  vOldRowIndex: Integer;
begin
  //vOldRowIndex := RowIndex;
  MouseDown(mbLeft, KeysToShiftState(Message.WParam) + MouseOriginToShiftState, Message.LParam and $FFFF, Message.LParam shr 16);
  //if vOldRowIndex <> RowIndex then
    Message.Result := 1;
end;

procedure TEditGrid.WMCFLBUTTONUP(var Message: TMessage);
var
  vRect: TRect;
  X, Y: Integer;
begin
  vRect := ClientRect;

  X := Message.LParam and $FFFF;
  Y := Message.LParam shr 16;
  if PtInRect(vRect, Point(X, Y)) then  // 在区域
    Message.Result := 1
  else
    Message.Result := 0;
end;

procedure TEditGrid.WMCFMOUSEMOVE(var Message: TMessage);
var
  vShift: TShiftState;
  X, Y: Integer;
  //vItemIndex: Integer;
  vRect: TRect;
begin
  X := Message.LParam and $FFFF;
  Y := Message.LParam shr 16;

  vRect := ClientRect;
  if PtInRect(vRect, Point(X, Y)) then  // 在区域
  begin
    vShift := [];
    if Message.WParam and MK_LBUTTON <> 0 then
    begin
      Include(vShift, ssLeft);
      Message.Result := 1;
    end;

    MouseMove(vShift, X, Y);
  end;
end;

end.
