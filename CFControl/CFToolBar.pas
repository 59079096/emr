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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddToolButton: TCFToolButton;
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
end;

function TCFToolBar.AddToolButton: TCFToolButton;
begin
  Result := TCFToolButton.Create(Self);
  Result.OnPaintIcon := DoPaintIcon;
  Result.Align := alLeft;
  Result.Parent := Self;
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

procedure TCFToolBar.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

end.
