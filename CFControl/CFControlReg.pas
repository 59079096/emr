unit CFControlReg;

interface

uses
  Classes, CFEdit, CFListView, CFDateTimePicker, CFSafeEdit;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CFControls', [
    TCFEdit,
    TCFListView,
    TCFDateTimePicker,
    TCFSafeEdit
    ]);
end;

end.
