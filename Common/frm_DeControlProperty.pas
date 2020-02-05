{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_DeControlProperty;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, HCView, HCRectItem,
  Vcl.ExtCtrls, Vcl.Grids;

type
  TfrmDeControlProperty = class(TForm)
    pnlSize: TPanel;
    chkAutoSize: TCheckBox;
    edtWidth: TEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    edtHeight: TEdit;
    pnlBorder: TPanel;
    pnl1: TPanel;
    btnSave: TButton;
    chkBorderTop: TCheckBox;
    chkBorderLeft: TCheckBox;
    chkBorderRight: TCheckBox;
    chkBorderBottom: TCheckBox;
    lbl3: TLabel;
    pnlDateTime: TPanel;
    cbbDTFormat: TComboBox;
    lbl4: TLabel;
    pnlEdit: TPanel;
    sgdEdit: TStringGrid;
    lbl8: TLabel;
    btnEditAddProperty: TButton;
    lbl9: TLabel;
    edtText: TEdit;
    procedure btnSaveClick(Sender: TObject);
    procedure chkAutoSizeClick(Sender: TObject);
    procedure btnEditAddPropertyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView);
  end;

implementation

uses
  HCEmrElementItem, HCCommon;

{$R *.dfm}

procedure TfrmDeControlProperty.btnEditAddPropertyClick(Sender: TObject);
begin
  sgdEdit.RowCount := sgdEdit.RowCount + 1;
end;

procedure TfrmDeControlProperty.btnSaveClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmDeControlProperty.chkAutoSizeClick(Sender: TObject);
begin
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;
end;

procedure TfrmDeControlProperty.SetHCView(const AHCView: THCView);
var
  i: Integer;
  vControlItem: THCControlItem;
  vDeDateTimePicker: TDeDateTimePicker;
begin
  vControlItem := AHCView.ActiveSectionTopLevelData.GetActiveItem as THCControlItem;

  chkAutoSize.Checked := vControlItem.AutoSize;
  edtWidth.Text := IntToStr(vControlItem.Width);
  edtHeight.Text := IntToStr(vControlItem.Height);
  edtText.Text := vControlItem.Text;

  if vControlItem is TDeDateTimePicker then  // DateTimePicke
  begin
    Self.Caption := 'TDeDateTime属性';
    vDeDateTimePicker := vControlItem as TDeDateTimePicker;
    chkBorderLeft.Checked := cbsLeft in vDeDateTimePicker.BorderSides;
    chkBorderTop.Checked := cbsTop in vDeDateTimePicker.BorderSides;
    chkBorderRight.Checked := cbsRight in vDeDateTimePicker.BorderSides;
    chkBorderBottom.Checked := cbsBottom in vDeDateTimePicker.BorderSides;
    cbbDTFormat.Text := vDeDateTimePicker.Format;
  end;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    vControlItem.AutoSize := chkAutoSize.Checked;
    if not chkAutoSize.Checked then  // 自定义大小
    begin
      vControlItem.Width := StrToIntDef(edtWidth.Text, vControlItem.Width);
      vControlItem.Height := StrToIntDef(edtHeight.Text, vControlItem.Height);
    end;

    if vDeDateTimePicker <> nil then
    begin
      if chkBorderLeft.Checked then
        vDeDateTimePicker.BorderSides := vDeDateTimePicker.BorderSides + [cbsLeft]
      else
        vDeDateTimePicker.BorderSides := vDeDateTimePicker.BorderSides - [cbsLeft];

      if chkBorderTop.Checked then
        vDeDateTimePicker.BorderSides := vDeDateTimePicker.BorderSides + [cbsTop]
      else
        vDeDateTimePicker.BorderSides := vDeDateTimePicker.BorderSides - [cbsTop];

      if chkBorderRight.Checked then
        vDeDateTimePicker.BorderSides := vDeDateTimePicker.BorderSides + [cbsRight]
      else
        vDeDateTimePicker.BorderSides := vDeDateTimePicker.BorderSides - [cbsRight];

      if chkBorderBottom.Checked then
        vDeDateTimePicker.BorderSides := vDeDateTimePicker.BorderSides + [cbsBottom]
      else
        vDeDateTimePicker.BorderSides := vDeDateTimePicker.BorderSides - [cbsBottom];

      vDeDateTimePicker.Format := cbbDTFormat.Text;
    end;

    AHCView.BeginUpdate;
    try
      AHCView.ActiveSection.ReFormatActiveItem;
    finally
      AHCView.EndUpdate;
    end;
  end;
end;

end.
