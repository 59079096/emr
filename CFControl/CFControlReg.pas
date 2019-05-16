unit CFControlReg;

interface

uses
  Classes, CFListView, CFDateTimePicker;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CFControls', [
    TCFListView,
    TCFDateTimePicker
    ]);
end;

end.
