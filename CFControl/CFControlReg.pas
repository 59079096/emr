unit CFControlReg;

interface

uses
  Classes, CFButton, CFSplitter, CFEdit, CFListView, CFDateTimePicker, CFSafeEdit,
  CFButtonEdit, CFGridEdit, CFPopupEdit, CFCombobox, CFDateRang, CFDBGrid, CFGrid,
  CFGridTree, CFColorPad, CFColorCombobox, CFLable, CFToolButton;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CFControls', [TCFButton, TCFToolButton, TCFMenuButton,
    TCFSplitter, TCFEdit, TCFCombobox,
    TCFButtonEdit, TCFPopupEdit, TCFGridEdit, TCFListView, TCFDateTimePicker,
    TCFDateRang, TCFSafeEdit, TCFGrid, tCFGridTree, TCFDBGrid, TCFColorPad,
    TCFRichColorPad, TCFColorCombobox, TCFLable]);
end;

end.
