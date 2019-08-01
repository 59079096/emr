unit CFPanel;

interface

uses
  Classes, Graphics, CFControl;

type
  TCFCustomPanel = class(TCFCustomControl)
  protected
    procedure DrawControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color;
  end;

  TCFPanel = class sealed(TCFCustomPanel)  // ²»¿É¼Ì³Ð

  end;

implementation

{ TCFCustomPanel }

constructor TCFCustomPanel.Create(AOwner: TComponent);
begin
  inherited;
  Self.Color := GBackColor;
end;

procedure TCFCustomPanel.DrawControl(ACanvas: TCanvas);
begin
  inherited DrawControl(ACanvas);
  ACanvas.FillRect(ClientRect);
end;

end.
