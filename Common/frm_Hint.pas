{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_Hint;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.GIFImg,
  Vcl.ExtCtrls, GDIPOBJ, GDIPAPI;

type
  TUpdateThread = class(TThread)
  private
    FOnExecute: TNotifyEvent;
  protected
    procedure DoOnExecute;
    procedure Execute; override;
  public
    constructor Create;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;

  TfrmHint = class(TForm)
    lblHint: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FGPImage: TGPBitmap;
    FGPGraphics: TGPGraphics;
    FFrameCount, FFrameIndex: Integer;
    FFrameTimeArr: array of Cardinal;
    FUpdateThread: TUpdateThread;
    procedure DoUpdateThreadExecute(Sender: TObject);
  public
    { Public declarations }
    procedure UpdateHint(const AHint: string);
  end;

implementation

{$R *.dfm}

{ TfrmHint }

procedure TfrmHint.DoUpdateThreadExecute(Sender: TObject);
var
  vHeight, vTop: Integer;
{  vBitmap: TBitmap;
  SavePal, SourcePal: HPALETTE;}
begin
  if Self.Visible then
  begin
    {// Draw new frame on buffer
    SourcePal := TGIFImage(imgWait.Picture.Graphic).Images[FWaitImageIndex].Palette;
    if (SourcePal = 0) then
      SourcePal := SystemPalette16; // This should never happen

    vBitmap := TBitmap.Create;
    vBitmap.SetSize(imgWait.Width, imgWait.Height);

    vBitmap.Canvas.Brush.Color := TGIFImage(imgWait.Picture.Graphic).BackgroundColor;
    vBitmap.Canvas.Brush.Style := bsSolid;
    vBitmap.Canvas.FillRect(vBitmap.Canvas.ClipRect);

    SavePal := SelectPalette(vBitmap.Handle, SourcePal, False);
    try
      RealizePalette(vBitmap.Canvas.Handle);
      TGIFImage(imgWait.Picture.Graphic).Images[FWaitImageIndex].Draw(
        vBitmap.Canvas, vBitmap.Canvas.ClipRect, True, False);

    finally
      if (SavePal <> 0) then
        SelectPalette(vBitmap.Handle, SavePal, False);
    end;

    Canvas.StretchDraw(imgWait.BoundsRect, vBitmap);

    vBitmap.Free; }

    if Assigned(FGPImage) and HandleAllocated then
    begin
      FGPImage.SelectActiveFrame(FrameDimensionTime, FFrameIndex);
      try
        {Canvas.Brush.Color := Color;
        Canvas.Brush.Style := bsSolid;
        Canvas.FillRect(Bounds(Width - FGPImage.GetWidth, 0, FGPImage.GetWidth, FGPImage.GetHeight));}

        vHeight := FGPImage.GetHeight;
        vTop := (ClientHeight - vHeight) div 2;  // 只考虑小于窗体高度的情况

        FGPGraphics := TGPGraphics.Create(Canvas.Handle);
        FGPGraphics.DrawImage(FGPImage, Width - FGPImage.GetWidth, vTop, FGPImage.GetWidth, vHeight);
      finally
        FreeAndNil(FGPGraphics);
      end;

      Sleep(FFrameTimeArr[FFrameIndex] * 10);

      Inc(FFrameIndex);
      if FFrameIndex > FFrameCount - 1 then
        FFrameIndex := 0;
    end;
  end;
end;

procedure TfrmHint.FormCreate(Sender: TObject);
var
//  vDimensionsCount: Integer;
//  vDimensionsIDs: PGUID;
  vPropertyItem: PPropertyItem;
  vPropertySize: Integer;
{type
  ArrDimensions = array of TGUID;}
begin
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  //SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_TOOLWINDOW or WS_EX_NOACTIVATE);

  if FileExists(ExtractFilePath(ParamStr(0)) + 'image\WAITEGIF.GIF') then
  begin
    FGPImage := TGPBitmap.Create(ExtractFilePath(ParamStr(0)) + 'image\WAITEGIF.GIF');

    {vDimensionsCount := FGPImage.GetFrameDimensionsCount;
    GetMem(vDimensionsIDs, vDimensionsCount * SizeOf(TGUID));
    try
      FGPImage.GetFrameDimensionsList(vDimensionsIDs, vDimensionsCount);
      FFrameCount := FGPImage.GetFrameCount(ArrDimensions(vDimensionsIDs)[0]);
    finally
      FreeMem(vDimensionsIDs);
    end;}

    { 获取 Gif 动画的时间属性, 这是一个 Cardinal 数组 }
    vPropertySize := FGPImage.GetPropertyItemSize(PropertyTagFrameDelay);
    GetMem(vPropertyItem, vPropertySize);
    try
      FGPImage.GetPropertyItem(PropertyTagFrameDelay, vPropertySize, vPropertyItem);

      FFrameCount := vPropertyItem.Length div 4;  // 帧总数

      { 复制到需要的数组 }
      SetLength(FFrameTimeArr, FFrameCount);
      CopyMemory(FFrameTimeArr, vPropertyItem.Value, vPropertyItem.Length);
    finally
      FreeMem(vPropertyItem);
    end;
  end;
end;

procedure TfrmHint.FormDestroy(Sender: TObject);
begin
  Application.ProcessMessages;  // 修正release版本运行时出错(原因待查)

  if Assigned(FUpdateThread) then
  begin
    FUpdateThread.Terminate;  // 告诉线程准备停止
    WaitForSingleObject(FUpdateThread.Handle, INFINITE);  // = WAIT_OBJECT_0  // 等待线程停止
  end;

  FreeAndNil(FGPGraphics);
  FreeAndNil(FGPImage);
end;

procedure TfrmHint.FormShow(Sender: TObject);
begin
  if Assigned(FGPImage) then
  begin
    FUpdateThread := TUpdateThread.Create;  // 创建线程
    FUpdateThread.OnExecute := DoUpdateThreadExecute;
    FUpdateThread.Resume;
  end;

  FFrameIndex := 0;
  Self.Update;
  //Application.ProcessMessages;
end;

procedure TfrmHint.UpdateHint(const AHint: string);
begin
  {lblHint.Caption := AHint;
  //Application.ProcessMessages;
  Self.Update;}

  lblHint.Caption := AHint;
  if HandleAllocated then
    lblHint.Update;
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  //Application.ProcessMessages;
end;

{ TUpdateThread }

constructor TUpdateThread.Create;
begin
  inherited Create;
end;

procedure TUpdateThread.DoOnExecute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

procedure TUpdateThread.Execute;
begin
  inherited;
  while not Terminated do
  begin
    DoOnExecute;  // Synchronize 在主线程中执行DoTimer事件
    Sleep(0);
  end;
end;

end.
