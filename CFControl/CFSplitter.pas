unit CFSplitter;

interface

uses
  Windows, Classes, Controls, Graphics, CFControl;

type
  CFNaturalNumber = 1..High(Integer);

  TCFSplitter = class(TCFCustomControl)
  private
    FMinSize: CFNaturalNumber;
    FMaxSize, FSplit, FNewSize, FOldSize: Integer;
    FDownPos: TPoint;
    FControl: TControl;
    function FindControl: TControl;
    procedure UpdateControlSize;
    procedure StopSizing;
    procedure CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure UpdateSize(X, Y: Integer);
  protected
    procedure DrawControl(ACanvas: TCanvas); override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure RequestAlign; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  Forms;

{ TCFSplitter }

procedure TCFSplitter.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case Align of
    alLeft: S := FControl.Width + Split;
    alRight: S := FControl.Width - Split;
    alTop: S := FControl.Height + Split;
    alBottom: S := FControl.Height - Split;
  end;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else
  if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if Align in [alRight, alBottom] then
      S := S - NewSize
    else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

function TCFSplitter.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if NewWidth < 3 then
    NewWidth := 3;
  if NewHeight < 3 then
    NewHeight := 3;
end;

constructor TCFSplitter.Create(AOwner: TComponent);
begin
  inherited;
  Height := 100;
  Align := alLeft;
  Width := 6;
end;

procedure TCFSplitter.DrawControl(ACanvas: TCanvas);
const
  PointCount = 12;
  PointSize = 4;
  PointSplit = 8;

  procedure DrawLine;
  begin
    case Align of
      alLeft, alRight:
        begin
          ACanvas.Pen.Color := GLineColor;
          ACanvas.MoveTo(0, 0);
          ACanvas.LineTo(0, Height);
          ACanvas.Pen.Color := GLineColor;
          ACanvas.MoveTo(Width - 1, 0);
          ACanvas.LineTo(Width - 1, Height);
        end;

      alTop, alBottom:
        begin
          ACanvas.Pen.Color := GLineColor;
          ACanvas.MoveTo(0, 0);
          ACanvas.LineTo(Width, 0);
          ACanvas.Pen.Color := GLineColor;
          ACanvas.MoveTo(0, Height - 1);
          ACanvas.LineTo(Width, Height - 1);
        end;
    end;
  end;

var
  vLeft, vTop: Integer;
  i: Integer;
begin
  inherited DrawControl(ACanvas);
  ACanvas.Brush.Color := GBackColor;
  ACanvas.FillRect(ClientRect);

  if BorderVisible then
    DrawLine;

  //  ÌáÊ¾µã
  ACanvas.Brush.Color := GBorderColor;
  case Align of
    alLeft, alRight:
      begin
        vLeft := (Width - PointSize) div 2;
        vTop := PointCount * PointSize + (PointCount - 1) * PointSplit;
        vTop := Round((Height - vTop) div 2);
        //
        for i := 0 to PointCount - 1 do
        begin
          ACanvas.FillRect(Bounds(vLeft, vTop, PointSize, PointSize));
          {ACanvas.Brush.Color := clBtnShadow;
          ACanvas.FillRect(Bounds(vLeft + 1, vTop + 1, 1, 1)); }
          vTop := vTop + PointSize + PointSplit;
        end;
      end;

    alTop, alBottom:
      begin
        vLeft := PointCount * PointSize + (PointCount - 1) * PointSplit;
        vLeft := (Width - vLeft) div 2;
        vTop := Round((Height - PointSize) / 2);
        //
        for i := 0 to PointCount - 1 do
        begin
          ACanvas.FillRect(Bounds(vLeft, vTop, PointSize, PointSize));
          {ACanvas.Brush.Color := clBtnShadow;
          ACanvas.FillRect(Bounds(vLeft + 1, vTop + 1, 1, 1)); }
          vLeft := vLeft + PointSize + PointSplit;
        end;
      end;
  end;
end;

function TCFSplitter.FindControl: TControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  Result := nil;
  P := Point(Left, Top);
  case Align of
    alLeft: Dec(P.X);  // if AlignWithMargins then
    alRight: Inc(P.X, Width);
    alTop: Dec(P.Y);
    alBottom: Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    {if Parent.Controls[i] is TCCustomControl then
    begin
      Result := Parent.Controls[I] as TCCustomControl;}
    Result := Parent.Controls[I];
    if Result.Visible and Result.Enabled then
    begin
      R := Result.BoundsRect;
      {if Result.AlignWithMargins then
      begin
        Inc(R.Right, Result.Margins.Right);
        Dec(R.Left, Result.Margins.Left);
        Inc(R.Bottom, Result.Margins.Bottom);
        Dec(R.Top, Result.Margins.Top);
      end; }
      if (R.Right - R.Left) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);
      if PtInRect(R, P) then Exit;
    end;
    //end;
  end;
  Result := nil;
end;

procedure TCFSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  I: Integer;
begin
  inherited;
  if Button = mbLeft then
  begin
    FControl := FindControl;
    FDownPos := Point(X, Y);
    if Assigned(FControl) then
    begin
      if Align in [alLeft, alRight] then
      begin
        FMaxSize := Parent.ClientWidth - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Visible and (Align in [alLeft, alRight]) then Dec(FMaxSize, Width);
        Inc(FMaxSize, FControl.Width);
      end
      else
      begin
        FMaxSize := Parent.ClientHeight - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alTop, alBottom] then Dec(FMaxSize, Height);
        Inc(FMaxSize, FControl.Height);
      end;
      UpdateSize(X, Y);
      {with TCustomForm(Self.Parent) do
      begin
        if ActiveControl <> nil then
        begin
          FActiveControl := ActiveControl;
          FOldKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
          TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
        end;
      end;}
    end;
  end;
end;

procedure TCFSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vNewSize, vSplit: Integer;
begin
  inherited;
  if (ssLeft in Shift) and Assigned(FControl) then
  begin
    CalcSplitSize(X, Y, vNewSize, vSplit);
    FNewSize := vNewSize;
    FSplit := vSplit;
    UpdateControlSize;
  end;
end;

procedure TCFSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  StopSizing;
end;

procedure TCFSplitter.RequestAlign;
begin
  inherited;
  case Align of
    alLeft, alRight: Cursor := crHSplit;
  else
    Cursor := crVSplit;
  end;
end;

procedure TCFSplitter.StopSizing;
begin
  if Assigned(FControl) then
    FControl := nil;
end;

procedure TCFSplitter.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case Align of
      alLeft: FControl.Width := FNewSize;
      alTop: FControl.Height := FNewSize;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - FNewSize);
            FControl.Width := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - FNewSize);
            FControl.Height := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    Update;
    FOldSize := FNewSize;
  end;
end;

procedure TCFSplitter.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

end.
