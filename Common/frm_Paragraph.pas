{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_Paragraph;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, HCEmrView;

type
  TfrmParagraph = class(TForm)
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    lbl6: TLabel;
    lbl7: TLabel;
    lbl8: TLabel;
    lbl9: TLabel;
    lbl10: TLabel;
    btnOk: TButton;
    clrbxBG: TColorBox;
    cbbAlignHorz: TComboBox;
    cbbAlignVert: TComboBox;
    cbbSpaceMode: TComboBox;
    edtFirstIndent: TEdit;
    edtLeftIndent: TEdit;
    edtRightIndent: TEdit;
    edtLineSpace: TEdit;
    lblUnit: TLabel;
    chkBreakRough: TCheckBox;
    procedure btnOkClick(Sender: TObject);
    procedure cbbSpaceModeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetView(const AHCView: THCEmrView);
    procedure SetGridView(const AGridView: THCEmrView);
  end;

var
  frmParagraph: TfrmParagraph;

implementation

uses
  HCParaStyle;

{$R *.dfm}

procedure TfrmParagraph.btnOkClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmParagraph.cbbSpaceModeChange(Sender: TObject);
begin
  if cbbSpaceMode.ItemIndex = 5 then  // 固定值
  begin
    edtLineSpace.Text := '12';
    edtLineSpace.Visible := True;
    lblUnit.Caption := '磅';
    lblUnit.Visible := True;
  end
  else
  if cbbSpaceMode.ItemIndex = 6 then  // 多倍
  begin
    edtLineSpace.Text := '3';
    edtLineSpace.Visible := True;
    lblUnit.Caption := '倍';
    lblUnit.Visible := True;
  end
  else
  begin
    edtLineSpace.Text := '';
    edtLineSpace.Visible := False;
    lblUnit.Visible := False;
  end;
end;

procedure TfrmParagraph.FormShow(Sender: TObject);
begin
  cbbSpaceModeChange(Sender);
end;

procedure TfrmParagraph.SetGridView(const AGridView: THCEmrView);
begin

end;

procedure TfrmParagraph.SetView(const AHCView: THCEmrView);
var
  vFloat: Single;
begin
  cbbSpaceMode.ItemIndex := Ord(AHCView.Style.ParaStyles[AHCView.CurParaNo].LineSpaceMode);
  case AHCView.Style.ParaStyles[aHCView.CurParaNo].LineSpaceMode of
    TParaLineSpaceMode.plsFix:
      edtLineSpace.Text := FormatFloat('0.#', AHCView.Style.ParaStyles[aHCView.CurParaNo].LineSpace);

    TParaLineSpaceMode.plsMult:
      edtLineSpace.Text := FormatFloat('0.#', AHCView.Style.ParaStyles[aHCView.CurParaNo].LineSpace);
  end;
  cbbAlignHorz.ItemIndex := Ord(AHCView.Style.ParaStyles[AHCView.CurParaNo].AlignHorz);
  cbbAlignVert.ItemIndex := Ord(AHCView.Style.ParaStyles[AHCView.CurParaNo].AlignVert);
  clrbxBG.Color := AHCView.Style.ParaStyles[AHCView.CurParaNo].BackColor;
  edtFirstIndent.Text := FormatFloat('0.#', AHCView.Style.ParaStyles[AHCView.CurParaNo].FirstIndent);
  edtLeftIndent.Text := FormatFloat('0.#', AHCView.Style.ParaStyles[AHCView.CurParaNo].LeftIndent);
  edtRightIndent.Text := FormatFloat('0.#', AHCView.Style.ParaStyles[AHCView.CurParaNo].RightIndent);
  chkBreakRough.Checked := AHCView.Style.ParaStyles[AHCView.CurParaNo].BreakRough;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    AHCView.BeginUpdate;
    try
      if cbbSpaceMode.ItemIndex > 4 then
        vFloat := StrToFloatDef(edtLineSpace.Text, 12)
      else
        vFloat := 12;

      AHCView.ApplyParaLineSpace(TParaLineSpaceMode(cbbSpaceMode.ItemIndex), vFloat);
      AHCView.ApplyParaAlignHorz(TParaAlignHorz(cbbAlignHorz.ItemIndex));
      AHCView.ApplyParaAlignVert(TParaAlignVert(cbbAlignVert.ItemIndex));
      AHCView.ApplyParaBackColor(clrbxBG.Color);
      AHCView.ApplyParaFirstIndent(StrToFloatDef(edtFirstIndent.Text, 0));
      AHCView.ApplyParaLeftIndent(StrToFloatDef(edtLeftIndent.Text, 0));
      AHCView.ApplyParaRightIndent(StrToFloatDef(edtRightIndent.Text, 0));
      AHCView.ApplyParaBreakRough(chkBreakRough.Checked);
    finally
      AHCView.EndUpdate;
    end;
  end;
end;

end.
