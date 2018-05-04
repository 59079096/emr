{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_Hint;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmHint = class(TForm)
    lblHint: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    procedure UpdateHint(const AHint: string);
  end;

implementation

{$R *.dfm}

{ TfrmHint }

procedure TfrmHint.CreateParams(var Params: TCreateParams);
begin
  //Params.Style := Params.Style or WS_POPUP;
  inherited CreateParams(Params);
end;

procedure TfrmHint.FormCreate(Sender: TObject);
begin
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
  //SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_TOOLWINDOW or WS_EX_NOACTIVATE);
end;

procedure TfrmHint.FormShow(Sender: TObject);
begin
  Self.Update;
  //Application.ProcessMessages;
end;

procedure TfrmHint.UpdateHint(const AHint: string);
begin
  lblHint.Caption := AHint;
  //Application.ProcessMessages;
  Self.Update;
end;

end.
