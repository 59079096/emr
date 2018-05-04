{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

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
