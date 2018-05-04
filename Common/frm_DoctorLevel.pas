unit frm_DoctorLevel;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CFControl, CFButtonEdit,
  CFGridEdit;

type
  TfrmDoctorLevel = class(TForm)
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    btnSave: TButton;
    gde1: TCFGridEdit;
    gde2: TCFGridEdit;
    gde3: TCFGridEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
