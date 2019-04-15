{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_ItemContent;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, EmrEdit,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ToolWin, System.ImageList,
  Vcl.ImgList, HCTextStyle, Vcl.Menus, FireDAC.Comp.Client, Vcl.Buttons;

type
  TfrmItemContent = class(TForm)
    pnlEdit: TPanel;
    spl1: TSplitter;
    il1: TImageList;
    tlbFontSize: TToolBar;
    cbbFontSize: TComboBox;
    cbFontColor: TColorBox;
    btnBold: TToolButton;
    btnItalic: TToolButton;
    btnUnderLine: TToolButton;
    btnStrikeOut: TToolButton;
    btnSuperScript: TToolButton;
    btnSubScript: TToolButton;
    btn2: TToolButton;
    btnLineSpace: TToolButton;
    btn9: TToolButton;
    sgdDE: TStringGrid;
    pmLineSpace: TPopupMenu;
    mniLineSpace: TMenuItem;
    mniN17: TMenuItem;
    mniN21: TMenuItem;
    pnl1: TPanel;
    lblDeHint: TLabel;
    edtPY: TEdit;
    cbbFont: TComboBox;
    btnSave: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBoldClick(Sender: TObject);
    procedure cbbFontChange(Sender: TObject);
    procedure cbbFontSizeChange(Sender: TObject);
    procedure cbFontColorChange(Sender: TObject);
    procedure sgdDEDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtPYKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
    FDomainItemID: Integer;
    FEmrEdit: TEmrEdit;
    procedure DoSaveItemContent;
    procedure SetDomainItemID(Value: Integer);
    procedure ShowDataElement;
  public
    { Public declarations }
    property DomainItemID: Integer read FDomainItemID write SetDomainItemID;
  end;

implementation

uses
  HCCommon, EmrElementItem, EmrGroupItem, emr_Common, emr_BLLServerProxy,
  HCTextItem, HCRectItem, HCStyle, Data.DB;

{$R *.dfm}

procedure TfrmItemContent.btnBoldClick(Sender: TObject);
begin
  case (Sender as TToolButton).Tag of
    0: FEmrEdit.ApplyTextStyle(THCFontStyle.tsBold);
    1: FEmrEdit.ApplyTextStyle(THCFontStyle.tsItalic);
    2: FEmrEdit.ApplyTextStyle(THCFontStyle.tsUnderline);
    3: FEmrEdit.ApplyTextStyle(THCFontStyle.tsStrikeOut);
    4: FEmrEdit.ApplyTextStyle(THCFontStyle.tsSuperscript);
    5: FEmrEdit.ApplyTextStyle(THCFontStyle.tsSubscript);
  end;
end;

procedure TfrmItemContent.btnSaveClick(Sender: TObject);
begin
  DoSaveItemContent;
end;

procedure TfrmItemContent.cbbFontChange(Sender: TObject);
begin
  FEmrEdit.ApplyTextFontName(cbbFont.Text);
end;

procedure TfrmItemContent.cbbFontSizeChange(Sender: TObject);
begin
  FEmrEdit.ApplyTextFontSize(GetFontSize(cbbFontSize.Text));
end;

procedure TfrmItemContent.cbFontColorChange(Sender: TObject);
begin
  FEmrEdit.ApplyTextColor(cbFontColor.Selected);
end;

procedure TfrmItemContent.DoSaveItemContent;
var
  vSM: TMemoryStream;
begin
  vSM := TMemoryStream.Create;
  try
    FEmrEdit.SaveToStream(vSM);

    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)  // 获取患者
      begin
        ABLLServerReady.Cmd := BLL_SAVEDOMAINITEMCONTENT;  // 获取模板分组列表
        ABLLServerReady.ExecParam.I['DItemID'] := FDomainItemID;
        ABLLServerReady.ExecParam.ForcePathObject('Content').LoadBinaryFromStream(vSM);
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      begin
        if ABLLServer.MethodRunOk then  // 服务端方法返回执行成功
          ShowMessage('保存成功！')
        else
          ShowMessage(ABLLServer.MethodError);
      end);
  finally
    FreeAndNil(vSM);
  end;
end;

procedure TfrmItemContent.edtPYKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    ClientCache.DataElementDT.FilterOptions := [foCaseInsensitive{不区分大小写, foNoPartialCompare不支持通配符(*)所表示的部分匹配}];
    if edtPY.Text = '' then
      ClientCache.DataElementDT.Filtered := False
    else
    begin
      ClientCache.DataElementDT.Filtered := False;
      if IsPY(edtPY.Text[1]) then
        ClientCache.DataElementDT.Filter := 'py like ''%' + edtPY.Text + '%'''
      else
        ClientCache.DataElementDT.Filter := 'dename like ''%' + edtPY.Text + '%''';
      ClientCache.DataElementDT.Filtered := True;
    end;

    ShowDataElement;
  end;
end;

procedure TfrmItemContent.FormCreate(Sender: TObject);
begin
  FDomainItemID := 0;

  HCDefaultTextItemClass := TDeItem;
  HCDefaultDomainItemClass := TDeGroup;

  FEmrEdit := TEmrEdit.Create(Self);
  FEmrEdit.Parent := Self.pnlEdit;
  FEmrEdit.Align := alClient;
end;

procedure TfrmItemContent.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEmrEdit);
end;

procedure TfrmItemContent.FormShow(Sender: TObject);
begin
  sgdDE.RowCount := 1;
  sgdDE.Cells[0, 0] := '序';
  sgdDE.Cells[1, 0] := '名称';
  sgdDE.Cells[2, 0] := '编码';
  sgdDE.Cells[3, 0] := '拼音';
  sgdDE.Cells[4, 0] := '类型';
  sgdDE.Cells[5, 0] := '值域';

  ClientCache.DataElementDT.Filtered := False;
  ShowDataElement;
end;

procedure TfrmItemContent.SetDomainItemID(Value: Integer);
begin
  FDomainItemID := Value;
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDOMAINITEMCONTENT;  // 获取数据元选项值域对应的内容
      ABLLServerReady.ExecParam.I['DItemID'] := FDomainItemID;
      ABLLServerReady.AddBackField('Content');
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    var
      vSM: TMemoryStream;
    begin
      if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
      begin
        ShowMessage(ABLLServer.MethodError);
        Exit;
      end;

      vSM := TMemoryStream.Create;
      try
        ABLLServer.BackField('Content').SaveBinaryToStream(vSM);
        if vSM.Size > 0 then
          FEmrEdit.LoadFromStream(vSM)
        else
          FEmrEdit.Clear;
      finally
        FreeAndNil(vSM);
      end;
    end);
end;

procedure TfrmItemContent.sgdDEDblClick(Sender: TObject);
var
  vDeItem: TDeItem;
begin
  if sgdDE.Row < 0 then Exit;

  vDeItem := TDeItem.CreateByText(sgdDE.Cells[1, sgdDE.Row]);
  if FEmrEdit.CurStyleNo > THCStyle.Null then
    vDeItem.StyleNo := FEmrEdit.CurStyleNo
  else
    vDeItem.StyleNo := 0;

  vDeItem.ParaNo := FEmrEdit.CurParaNo;

  vDeItem[TDeProp.Name] := sgdDE.Cells[1, sgdDE.Row];
  vDeItem[TDeProp.Index] := sgdDE.Cells[0, sgdDE.Row];

  FEmrEdit.InsertItem(vDeItem);
end;

procedure TfrmItemContent.ShowDataElement;
var
  i: Integer;
begin
  sgdDE.RowCount := ClientCache.DataElementDT.RecordCount + 1;
  i := 1;
  with ClientCache.DataElementDT do
  begin
    First;
    while not Eof do
    begin
      sgdDE.Cells[0, i] := FieldByName('deid').AsString;
      sgdDE.Cells[1, i] := FieldByName('dename').AsString;
      sgdDE.Cells[2, i] := FieldByName('decode').AsString;
      sgdDE.Cells[3, i] := FieldByName('py').AsString;
      sgdDE.Cells[4, i] := FieldByName('frmtp').AsString;
      sgdDE.Cells[5, i] := FieldByName('domainid').AsString;

      Next;
      Inc(i);
    end;
  end;

  if sgdDE.RowCount > 1 then
    sgdDE.FixedRows := 1;
end;

end.
