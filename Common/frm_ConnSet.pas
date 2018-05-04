unit frm_ConnSet;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmConnSet = class(TForm)
    edtBLLServerIP: TEdit;
    edtBLLServerPort: TEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    edtMsgServerIP: TEdit;
    edtMsgServerPort: TEdit;
    lbl4: TLabel;
    lbl5: TLabel;
    lbl6: TLabel;
    btnSave: TButton;
    edtUpdateServerIP: TEdit;
    edtUpdateServerPort: TEdit;
    lbl7: TLabel;
    lbl8: TLabel;
    lbl9: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
