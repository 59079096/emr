{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_InsertDeTable;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmInsertTable = class(TForm)
    edtRows: TEdit;
    edtCols: TEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    btnOk: TButton;
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TfrmInsertTable.btnOkClick(Sender: TObject);
var
  vRowCount, vColCount: Integer;
begin
  if not TryStrToInt(edtRows.Text, vRowCount) then
    ShowMessage('请输入正确的行数！')
  else
  if not TryStrToInt(edtCols.Text, vColCount) then
    ShowMessage('请输入正确的列数！')
  else
  if vRowCount < 1 then
    ShowMessage('行数至少为1！')
  else
  if vRowCount > 256 then
    ShowMessage('行数不能超过256行！')
  else
  if vColCount < 1 then
    ShowMessage('列数至少为1！')
  else
  if vColCount > 32 then
    ShowMessage('列数不能超过32列！')
  else
    Self.ModalResult := mrOk;
end;

end.
