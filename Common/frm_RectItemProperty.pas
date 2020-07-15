{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_RectItemProperty;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, HCView, HCRectItem,
  ExtCtrls, Grids;

type
  TfrmRectItemProperty = class(TForm)
    pnl1: TPanel;
    btnSave: TButton;
    pnlSize: TPanel;
    lbl1: TLabel;
    edtWidth: TEdit;
    lbl2: TLabel;
    edtHeight: TEdit;
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView);
  end;

implementation

{$R *.dfm}

procedure TfrmRectItemProperty.btnSaveClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmRectItemProperty.SetHCView(const AHCView: THCView);
var
  i: Integer;
  vRectItem: THCCustomRectItem;
begin
  vRectItem := AHCView.ActiveSectionTopLevelData.GetActiveItem as THCCustomRectItem;

  edtWidth.Text := IntToStr(vRectItem.Width);
  edtHeight.Text := IntToStr(vRectItem.Height);

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    vRectItem.Width := StrToIntDef(edtWidth.Text, vRectItem.Width);
    vRectItem.Height := StrToIntDef(edtHeight.Text, vRectItem.Height);

    AHCView.BeginUpdate;
    try
      AHCView.ActiveSection.ReFormatActiveItem;
    finally
      AHCView.EndUpdate;
    end;
  end;
end;

end.
