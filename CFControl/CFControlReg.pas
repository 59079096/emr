unit CFControlReg;

interface

uses
  Classes, CFScrollBar, CFListView, CFGridEdit;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CFControls', [
    TCFListView,
    TCFScrollBar,
    TCFGridEdit,
    TCFColorCombobox
    ]);
end;

end.
