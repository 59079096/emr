unit CFControlReg;

interface

uses
  Classes, CFEdit, CFListView, CFDateTimePicker;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CFControls', [
    TCFEdit,
    TCFListView,
    TCFDateTimePicker
    ]);
end;

end.
