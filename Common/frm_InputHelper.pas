unit frm_InputHelper;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs;

type
  TfrmInputHelper = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
    FEnableEx, FSizeChange, FActiveEx, FVisibleEx: Boolean;
    FCompStr, FBeforText, FAfterText: string;
    FBorderColor: TColor;

    procedure UpdateView;
    procedure SetActiveEx(const Value: Boolean);
    procedure WndProc(var Message: TMessage); override;
  public
    { Public declarations }
    /// <summary> ���뷨�Ƿ���Ҫ���µ�����ʾλ�� </summary>
    function ResetImeCompRect(var AImePosition: TPoint): Boolean;
    function GetCandiText(const AIndex: Integer): string;
    procedure SetCompositionString(const S: string);
    procedure SetCaretString(const ABeforText, AAfterText: string);
    procedure CompWndMove(const AHandle: THandle; const ACaretX, ACaretY: Integer);
    procedure ShowEx;
    procedure CloseEx;

    property SizeChange: Boolean read FSizeChange;
    property EnableEx: Boolean read FEnableEx write FEnableEx;
    property ActiveEx: Boolean read FActiveEx write SetActiveEx;
    property VisibleEx: Boolean read FVisibleEx;
  end;

var
  frmInputHelper: TfrmInputHelper;

implementation

{$R *.dfm}

procedure TfrmInputHelper.CloseEx;
begin
  ShowWindow(Handle, SW_HIDE);
  FVisibleEx := False;
end;

procedure TfrmInputHelper.CompWndMove(const AHandle: THandle; const ACaretX,
  ACaretY: Integer);
var
  vPt: TPoint;
begin
  vPt.X := ACaretX;
  vPt.Y := ACaretY;
  Windows.ClientToScreen(AHandle, vPt);
  Left := vPt.X + 2;
  Top := vPt.Y + 4;
  if FCompStr <> '' then  // ��֪ʶ
    ShowEx;
end;

procedure TfrmInputHelper.FormCreate(Sender: TObject);
begin
  Self.FormStyle := fsStayOnTop;
  Self.DoubleBuffered := True;
  FSizeChange := False;
  FEnableEx := False;
  FActiveEx := False;
  FVisibleEx := False;
  FBorderColor := $00D2C5B5;
end;

procedure TfrmInputHelper.FormPaint(Sender: TObject);
var
  vRect: TRect;
  vText: string;
begin
  if not FActiveEx then
    Canvas.Brush.Color := $00FFFEFE
  else
    Canvas.Brush.Color := clBtnFace;

  vRect := ClientRect;
  Canvas.Pen.Color := FBorderColor;
  Canvas.Rectangle(vRect);

  if FCompStr <> '' then
    vText := '1.��1�� 2.��2�� 3.��3�� 4.��4�� 5.��5��'
  else
    vText := '��ã�������ѧϰ���֪ʶ^_^';

  InflateRect(vRect, -5, -5);
  Canvas.TextRect(vRect, vText, [tfVerticalCenter, tfSingleLine]);
end;

function TfrmInputHelper.GetCandiText(const AIndex: Integer): string;
begin
  Result := '��' + IntToStr(AIndex + 1) + '��';
end;

function TfrmInputHelper.ResetImeCompRect(var AImePosition: TPoint): Boolean;
begin
  Result := False;

  if True then  // ��֪ʶ
  begin
    AImePosition.Y := AImePosition.Y + Height + 2;  // ԭ���뷨����

    {CompWndMove(AHandle, ACaretX, ACaretY);
    vPt.X := ACaretX;
    vPt.Y := ACaretY;
    ClientToScreen(AHandle, vPt);
    Show(vPt.X + 2, vPt.Y + 4);}
    Result := True;
  end;
end;

procedure TfrmInputHelper.SetActiveEx(const Value: Boolean);
begin
  if FActiveEx <> Value then
  begin
    FActiveEx := Value;
    UpdateView;
  end;
end;

procedure TfrmInputHelper.SetCaretString(const ABeforText, AAfterText: string);
begin
  FBeforText := ABeforText;
  FAfterText := AAfterText;
  UpdateView;
end;

procedure TfrmInputHelper.SetCompositionString(const S: string);
begin
  FSizeChange := False;

  if FCompStr <> S then
  begin
    FCompStr := S;
    { TODO : �µ����룬����ƥ��֪ʶ���� }

    if FCompStr <> '' then  // ��֪ʶ
    begin
      if not FVisibleEx then
        ShowEx
      else
        UpdateView;
    end
    else  // ��֪ʶ
      CloseEx;
  end;
end;

procedure TfrmInputHelper.ShowEx;
begin
  FActiveEx := False;
  if not FVisibleEx then
  begin
    ShowWindow(Handle, SW_SHOWNOACTIVATE);
    FVisibleEx := True;
  end;
end;

procedure TfrmInputHelper.UpdateView;
begin
  if (Self.FVisibleEx) then
    InvalidateRect(Handle, ClientRect, False);
end;

procedure TfrmInputHelper.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_MOUSEACTIVATE then
  begin
    Message.Result := MA_NOACTIVATE;
    Exit;
  end
  else
  if Message.Msg = WM_NCACTIVATE then
  begin
    if (Message.WParam and $FFFF) <> WA_INACTIVE then
    begin
      if Message.LParam = 0 then
        SetActiveWindow(Message.LParam)
      else
        SetActiveWindow(0);
    end;
  end;

  inherited WndProc(Message);
end;

end.
