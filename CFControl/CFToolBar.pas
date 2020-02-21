{*******************************************************}
{                                                       }
{               HCView V1.1  作者：荆通                 }
{                                                       }
{      本代码遵循BSD协议，你可以加入QQ群 649023932      }
{            来获取更多的技术交流 2018-5-4              }
{                                                       }
{                  工具条控件实现单元                   }
{                                                       }
{*******************************************************}

unit CFToolBar;

interface

uses
  Windows, Classes, Controls, Graphics, StdCtrls, SysUtils, ImgList, CFToolButton;

type
  TCFToolBar = class(TCustomControl)
  protected
    FImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);
    procedure ResetControlAlign(const AControl: TControl; const AIndex: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InsertToolButton(const AIndex: Integer = -1): TCFToolButton;
    function AddToolButton: TCFToolButton;
    function InsertMenuToolButton(const AIndex: Integer = -1): TCFMenuButton;
    function AddMenuToolButton: TCFMenuButton;
    procedure DoPaintIcon(const AImageIndex: Integer; const ACanvas: TCanvas; const ARect: TRect);
  published
    property Images: TCustomImageList read FImages write SetImages;
  end;

implementation

{ TCFToolBar }

function TCFToolBar.AddMenuToolButton: TCFMenuButton;
begin
  Result := TCFMenuButton.Create(Self);
  Result.OnPaintIcon := DoPaintIcon;
  Result.Align := alLeft;
  Result.Parent := Self;
  Result.Tag := Self.ControlCount - 1;
end;

function TCFToolBar.AddToolButton: TCFToolButton;
begin
  Result := TCFToolButton.Create(Self);
  Result.OnPaintIcon := DoPaintIcon;
  Result.Align := alLeft;
  Result.Parent := Self;
  Result.Tag := Self.ControlCount - 1;
end;

constructor TCFToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCFToolBar.Destroy;
begin
  inherited Destroy;
end;

procedure TCFToolBar.DoPaintIcon(const AImageIndex: Integer; const ACanvas: TCanvas; const ARect: TRect);
begin
  if AImageIndex >= 0 then
    FImages.Draw(ACanvas, ARect.Left + 4, ARect.Top + (ARect.Bottom - ARect.Top - 16) div 2, AImageIndex);
end;

function TCFToolBar.InsertMenuToolButton(const AIndex: Integer = -1): TCFMenuButton;
begin
  Result := AddMenuToolButton;
  if AIndex >= 0 then
    ResetControlAlign(Result, AIndex);
end;

function TCFToolBar.InsertToolButton(const AIndex: Integer = -1): TCFToolButton;
begin
  Result := AddToolButton;
  if AIndex >= 0 then
    ResetControlAlign(Result, AIndex);
end;

procedure TCFToolBar.ResetControlAlign(const AControl: TControl;
  const AIndex: Integer);
var
  i, j: Integer;
begin
  if AIndex >= 0 then
  begin
    Self.Visible := False;
    try
      AControl.Tag := AIndex;
      for i := 0 to Self.ControlCount - 1 do
      begin
        if (Self.Controls[i] <> AControl) and (Self.Controls[i].Tag >= AIndex) then
          Self.Controls[i].Tag := Self.Controls[i].Tag + 1;
      end;

      for i := 0 to Self.ControlCount - 1 do
        Self.Controls[i].Align := alRight;

      for i := 0 to Self.ControlCount - 1 do
      begin
        for j := 0 to Self.ControlCount - 1 do
        begin
          if Self.Controls[j].Tag = i then
          begin
            Self.Controls[j].Align := alLeft;
            Break;
          end;
        end;
      end;
    finally
      Self.Visible := True;
    end;
  end;
end;

procedure TCFToolBar.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

end.
