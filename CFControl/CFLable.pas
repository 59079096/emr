unit CFLable;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Graphics;

type
  TCFLable = class(TGraphicControl)
  private
    FAutoSize: Boolean;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Text;
    property Align;
  end;

implementation

{ TCFLable }

constructor TCFLable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSize := True;
end;

procedure TCFLable.Paint;
var
  vRect: TRect;
  vText: string;
begin
  Canvas.Brush.Style := bsClear;
  vRect := ClientRect;
  vText := Self.Text;
  Canvas.TextRect(vRect, vText, [tfSingleLine, tfVerticalCenter]);
end;

procedure TCFLable.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  vWidth: Integer;
begin
  if Self.HasParent and FAutoSize then
    vWidth := Canvas.TextWidth(Text)
  else
    vWidth := AWidth;

  inherited SetBounds(ALeft, ATop, vWidth, AHeight);
end;

end.
