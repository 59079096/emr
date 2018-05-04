unit CFControlReg;

interface

uses
  Classes, DesignIntf, CFScrollBar, CFListView, CFGridEdit;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CFControls', [
    TCFListView,
    TCFScrollBar,
    TCFGridEdit
    ]);
end;

end.
