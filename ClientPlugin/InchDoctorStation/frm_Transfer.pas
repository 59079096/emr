unit frm_Transfer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CFControl, CFButtonEdit,
  CFGridEdit;

type
  TfrmTransfer = class(TForm)
    btn1: TButton;
    gde1: TCFGridEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTransfer: TfrmTransfer;

implementation

{$R *.dfm}

end.
