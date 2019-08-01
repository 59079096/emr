unit CFControlReg;

interface

uses
  Classes, CFButton, CFSplitter, CFEdit, CFListView, CFDateTimePicker, CFSafeEdit,
  CFButtonEdit, CFGridEdit, CFPopupEdit, CFCombobox, CFDateRang, CFDBGrid, CFGrid;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CFControls', [TCFButton, TCFSplitter, TCFEdit, TCFCombobox,
    TCFButtonEdit, TCFPopupEdit, TCFGridEdit, TCFListView, TCFDateTimePicker,
    TCFDateRang, TCFSafeEdit, TCFGrid, TCFDBGrid]);
end;

end.
