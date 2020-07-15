{*******************************************************}
{                                                       }
{         ����HCView�ĵ��Ӳ�������  ���ߣ���ͨ          }
{                                                       }
{ �˴������ѧϰ����ʹ�ã�����������ҵĿ�ģ��ɴ�������  }
{ �����ʹ���߳е�������QQȺ 649023932 ����ȡ����ļ��� }
{ ������                                                }
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
