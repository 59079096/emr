unit CFPopupEdit;

interface

uses
  Windows, Classes, Controls, CFButtonEdit, CFControl, CFPopupForm;

type
  TCFPopupEdit = class(TCFButtonEdit)
  private
    FPopup: TCFPopupForm;
    procedure DoPopupControl;
    function GetPopupControl: TWinControl;
    procedure SetPopupControl(const Value: TWinControl);
  protected
    procedure DoButtonClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup;
    procedure ClosePopup;
  published
    property PopupControl: TWinControl read GetPopupControl write SetPopupControl;
  end;

implementation

{ TCFPopupEdit }

procedure TCFPopupEdit.ClosePopup;
begin
  FPopup.ClosePopup(False);
end;

constructor TCFPopupEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopup := TCFPopupForm.Create(Self);
  Height := 20;
end;

destructor TCFPopupEdit.Destroy;
begin
  FPopup.Free;
  inherited Destroy;
end;

procedure TCFPopupEdit.DoButtonClick;
begin
  inherited DoButtonClick;

  if not (csDesigning in ComponentState) then
  begin
    if ReadOnly then Exit;
    DoPopupControl;
  end;
end;

procedure TCFPopupEdit.DoPopupControl;
begin
  FPopup.Popup(Self);
end;

function TCFPopupEdit.GetPopupControl: TWinControl;
begin
  Result := FPopup.PopupControl;
end;

procedure TCFPopupEdit.Popup;
begin
  DoButtonClick;
end;

procedure TCFPopupEdit.SetPopupControl(const Value: TWinControl);
begin
  FPopup.PopupControl := Value;
end;

end.
