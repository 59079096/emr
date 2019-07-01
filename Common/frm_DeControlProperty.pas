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
    btnOk: TButton;
    chkBorderTop: TCheckBox;
    chkBorderLeft: TCheckBox;
    chkBorderRight: TCheckBox;
    chkBorderBottom: TCheckBox;
    lbl3: TLabel;
    pnlCombobox: TPanel;
    edtValue: TEdit;
    lbl5: TLabel;
    btnAdd: TButton;
    btnDelete: TButton;
    btnMod: TButton;
    pnlDateTime: TPanel;
    cbbDTFormat: TComboBox;
    lbl4: TLabel;
    pnlRadioGroup: TPanel;
    lbl6: TLabel;
    edtRadioValue: TEdit;
    btnAddRadioItem: TButton;
    btnDeleteRadioItem: TButton;
    btnModRadioItem: TButton;
    lstRadioItem: TListBox;
    btnComboxAddProperty: TButton;
    lbl7: TLabel;
    pnl2: TPanel;
    sgdCombobox: TStringGrid;
    pnlEdit: TPanel;
    sgdEdit: TStringGrid;
    lbl8: TLabel;
    btnEditAddProperty: TButton;
    lstCombobox: TListBox;
    procedure btnOkClick(Sender: TObject);
    procedure chkAutoSizeClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnModClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lstComboboxClick(Sender: TObject);
    procedure btnAddRadioItemClick(Sender: TObject);
    procedure btnModRadioItemClick(Sender: TObject);
    procedure btnDeleteRadioItemClick(Sender: TObject);
    procedure lstRadioItemClick(Sender: TObject);
    procedure btnComboxAddPropertyClick(Sender: TObject);
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

procedure TfrmDeControlProperty.btnAddClick(Sender: TObject);
begin
  if edtValue.Text <> '' then
  begin
    lstCombobox.Items.Add(edtValue.Text);
    edtValue.Clear;
  end;
end;

procedure TfrmDeControlProperty.btnAddRadioItemClick(Sender: TObject);
begin
  if edtRadioValue.Text <> '' then
  begin
    lstRadioItem.Items.Add(edtRadioValue.Text);
    edtRadioValue.Clear;
  end;
end;

procedure TfrmDeControlProperty.btnComboxAddPropertyClick(Sender: TObject);
begin
  sgdCombobox.RowCount := sgdCombobox.RowCount + 1;
end;

procedure TfrmDeControlProperty.btnDeleteClick(Sender: TObject);
begin
  lstCombobox.DeleteSelected;
end;

procedure TfrmDeControlProperty.btnDeleteRadioItemClick(Sender: TObject);
begin
  lstRadioItem.DeleteSelected;
end;

procedure TfrmDeControlProperty.btnEditAddPropertyClick(Sender: TObject);
begin
  sgdEdit.RowCount := sgdEdit.RowCount + 1;
end;

procedure TfrmDeControlProperty.btnModRadioItemClick(Sender: TObject);
begin
  lstRadioItem.Items[lstRadioItem.ItemIndex] := edtRadioValue.Text;
end;

procedure TfrmDeControlProperty.btnOkClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmDeControlProperty.btnModClick(Sender: TObject);
begin
  lstCombobox.Items[lstCombobox.ItemIndex] := edtValue.Text;
end;

procedure TfrmDeControlProperty.chkAutoSizeClick(Sender: TObject);
begin
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;
end;

procedure TfrmDeControlProperty.lstComboboxClick(Sender: TObject);
begin
  if lstCombobox.ItemIndex >= 0 then
    edtValue.Text := lstCombobox.Items[lstCombobox.ItemIndex];
end;

procedure TfrmDeControlProperty.lstRadioItemClick(Sender: TObject);
begin
  if lstRadioItem.ItemIndex >= 0 then
    edtRadioValue.Text := lstRadioItem.Items[lstRadioItem.ItemIndex];
end;

procedure TfrmDeControlProperty.SetHCView(const AHCView: THCView);
var
  i: Integer;
  vControlItem: THCControlItem;
  vDeCheckBox: TDeCheckBox;
  vDeEdit: TDeEdit;
  vDeCombobox: TDeCombobox;
  vDeDateTimePicker: TDeDateTimePicker;
  vDeRadioGroup: TDeRadioGroup;
begin
  vControlItem := AHCView.ActiveSectionTopLevelData.GetActiveItem as THCControlItem;

  chkAutoSize.Checked := vControlItem.AutoSize;
  edtWidth.Text := IntToStr(vControlItem.Width);
  edtHeight.Text := IntToStr(vControlItem.Height);

  pnlBorder.Visible := False;

  if vControlItem is TDeCheckBox then
  begin
    vDeCheckBox := vControlItem as TDeCheckBox;
    pnlEdit.Visible := False;
  end;

  if vControlItem is TDeEdit then  // EditItem
  begin
    vDeEdit := vControlItem as TDeEdit;
    chkBorderLeft.Checked := cbsLeft in vDeEdit.BorderSides;
    chkBorderTop.Checked := cbsTop in vDeEdit.BorderSides;
    chkBorderRight.Checked := cbsRight in vDeEdit.BorderSides;
    chkBorderBottom.Checked := cbsBottom in vDeEdit.BorderSides;
    pnlBorder.Visible := True;

    sgdEdit.RowCount := vDeEdit.Propertys.Count;
    if sgdEdit.RowCount = 0 then
    begin
      sgdEdit.Cells[0, 0] := '';
      sgdEdit.Cells[1, 0] := '';
    end
    else
    begin
      for i := 0 to vDeEdit.Propertys.Count - 1 do
      begin
        sgdEdit.Cells[0, i] := vDeEdit.Propertys.Names[i];
        sgdEdit.Cells[1, i] := vDeEdit.Propertys.ValueFromIndex[i];
      end;
    end;
  end
  else
  begin
    vDeEdit := nil;
  end;

  if vControlItem is TDeCombobox then  // ComboboxItem
  begin
    vDeCombobox := vControlItem as TDeCombobox;

    chkBorderLeft.Checked := cbsLeft in vDeCombobox.BorderSides;
    chkBorderTop.Checked := cbsTop in vDeCombobox.BorderSides;
    chkBorderRight.Checked := cbsRight in vDeCombobox.BorderSides;
    chkBorderBottom.Checked := cbsBottom in vDeCombobox.BorderSides;
    pnlBorder.Visible := True;

    lstCombobox.Items.Assign(vDeCombobox.Items);
    sgdCombobox.RowCount := vDeCombobox.Propertys.Count;
    if sgdCombobox.RowCount = 0 then
    begin
      sgdCombobox.Cells[0, 0] := '';
      sgdCombobox.Cells[1, 0] := '';
    end
    else
    begin
      for i := 0 to vDeCombobox.Propertys.Count - 1 do
      begin
        sgdCombobox.Cells[0, i] := vDeCombobox.Propertys.Names[i];
        sgdCombobox.Cells[1, i] := vDeCombobox.Propertys.ValueFromIndex[i];
      end;
    end;
  end
  else
  begin
    vDeCombobox := nil;
    pnlCombobox.Visible := False;
  end;

  if vControlItem is TDeDateTimePicker then  // DateTimePicke
  begin
    vDeDateTimePicker := vControlItem as TDeDateTimePicker;

    chkBorderLeft.Checked := cbsLeft in vDeDateTimePicker.BorderSides;
    chkBorderTop.Checked := cbsTop in vDeDateTimePicker.BorderSides;
    chkBorderRight.Checked := cbsRight in vDeDateTimePicker.BorderSides;
    chkBorderBottom.Checked := cbsBottom in vDeDateTimePicker.BorderSides;
    pnlBorder.Visible := True;

    cbbDTFormat.Text := vDeDateTimePicker.Format;
  end
  else
  begin
    vDeDateTimePicker := nil;
    pnlDateTime.Visible := False;
  end;

  if vControlItem is TDeRadioGroup then  // DeRadioGroup
  begin
    vDeRadioGroup := vControlItem as TDeRadioGroup;
    for i := 0 to vDeRadioGroup.Items.Count - 1 do
      lstRadioItem.Items.Add(vDeRadioGroup.Items[i].Text);
  end
  else
  begin
    vDeRadioGroup := nil;
    pnlRadioGroup.Visible := False;
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

    if vDeEdit <> nil then
    begin
      if chkBorderLeft.Checked then
        vDeEdit.BorderSides := vDeEdit.BorderSides + [cbsLeft]
      else
        vDeEdit.BorderSides := vDeEdit.BorderSides - [cbsLeft];

      if chkBorderTop.Checked then
        vDeEdit.BorderSides := vDeEdit.BorderSides + [cbsTop]
      else
        vDeEdit.BorderSides := vDeEdit.BorderSides - [cbsTop];

      if chkBorderRight.Checked then
        vDeEdit.BorderSides := vDeEdit.BorderSides + [cbsRight]
      else
        vDeEdit.BorderSides := vDeEdit.BorderSides - [cbsRight];

      if chkBorderBottom.Checked then
        vDeEdit.BorderSides := vDeEdit.BorderSides + [cbsBottom]
      else
        vDeEdit.BorderSides := vDeEdit.BorderSides - [cbsBottom];

      vDeEdit.Propertys.Clear;
      for i := 0 to sgdEdit.RowCount - 1 do
      begin
        if sgdEdit.Cells[0, i].Trim <> '' then
          vDeEdit.Propertys.Add(sgdEdit.Cells[0, i] + '=' + sgdEdit.Cells[1, i]);
      end;
    end;

    if vDeCombobox <> nil then
    begin
      if chkBorderLeft.Checked then
        vDeCombobox.BorderSides := vDeCombobox.BorderSides + [cbsLeft]
      else
        vDeCombobox.BorderSides := vDeCombobox.BorderSides - [cbsLeft];

      if chkBorderTop.Checked then
        vDeCombobox.BorderSides := vDeCombobox.BorderSides + [cbsTop]
      else
        vDeCombobox.BorderSides := vDeCombobox.BorderSides - [cbsTop];

      if chkBorderRight.Checked then
        vDeCombobox.BorderSides := vDeCombobox.BorderSides + [cbsRight]
      else
        vDeCombobox.BorderSides := vDeCombobox.BorderSides - [cbsRight];

      if chkBorderBottom.Checked then
        vDeCombobox.BorderSides := vDeCombobox.BorderSides + [cbsBottom]
      else
        vDeCombobox.BorderSides := vDeCombobox.BorderSides - [cbsBottom];

      vDeCombobox.Items.Assign(lstCombobox.Items);
      vDeCombobox.Propertys.Clear;
      for i := 0 to sgdCombobox.RowCount - 1 do
      begin
        if sgdCombobox.Cells[0, i].Trim <> '' then
          vDeCombobox.Propertys.Add(sgdCombobox.Cells[0, i] + '=' + sgdCombobox.Cells[1, i]);
      end;
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

    if vDeRadioGroup <> nil then
    begin
      vDeRadioGroup.Items.Clear;

      for i := 0 to lstRadioItem.Items.Count - 1 do
        vDeRadioGroup.AddItem(lstRadioItem.Items[i]);
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
