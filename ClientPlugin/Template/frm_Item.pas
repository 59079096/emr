{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_Item;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, HCEdit,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ToolWin, System.ImageList,
  Vcl.ImgList, HCTextStyle, Vcl.Menus, FireDAC.Comp.Client;

type
  TfrmItem = class(TForm)
    pnlEdit: TPanel;
    spl1: TSplitter;
    il1: TImageList;
    tlbFontSize: TToolBar;
    btnSave: TToolButton;
    btn4: TToolButton;
    cbbFont: TComboBox;
    btn1: TToolButton;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBoldClick(Sender: TObject);
    procedure cbbFontChange(Sender: TObject);
    procedure cbbFontSizeChange(Sender: TObject);
    procedure cbFontColorChange(Sender: TObject);
    procedure sgdDEDblClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
    FDomainID: Integer;
    FHCEdit: THCEdit;
    procedure DoSaveItemContent;
    procedure SetDomainID(Value: Integer);
  public
    { Public declarations }
    property DomainID: Integer read FDomainID write SetDomainID;
  end;

implementation

uses
  HCCommon, EmrElementItem, emr_Common, emr_BLLServerProxy, emr_BLLConst;

{$R *.dfm}

procedure TfrmItem.btnBoldClick(Sender: TObject);
begin
  case (Sender as TToolButton).Tag of
    0: FHCEdit.ApplyTextStyle(TFontStyleEx.tsBold);
    1: FHCEdit.ApplyTextStyle(TFontStyleEx.tsItalic);
    2: FHCEdit.ApplyTextStyle(TFontStyleEx.tsUnderline);
    3: FHCEdit.ApplyTextStyle(TFontStyleEx.tsStrikeOut);
    4: FHCEdit.ApplyTextStyle(TFontStyleEx.tsSuperscript);
    5: FHCEdit.ApplyTextStyle(TFontStyleEx.tsSubscript);
  end;
end;

procedure TfrmItem.btnSaveClick(Sender: TObject);
begin
  DoSaveItemContent;
end;

procedure TfrmItem.cbbFontChange(Sender: TObject);
begin
  FHCEdit.ApplyTextFontName(cbbFont.Text);
end;

procedure TfrmItem.cbbFontSizeChange(Sender: TObject);
begin
  FHCEdit.ApplyTextFontSize(GetFontSize(cbbFontSize.Text));
end;

procedure TfrmItem.cbFontColorChange(Sender: TObject);
begin
  FHCEdit.ApplyTextColor(cbFontColor.Selected);
end;

procedure TfrmItem.DoSaveItemContent;
var
  vSM: TMemoryStream;
begin
  vSM := TMemoryStream.Create;
  try
    FHCEdit.SaveToStream(vSM);

    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)  // 获取患者
      begin
        ABLLServerReady.Cmd := BLL_SAVEDOMAINCONTENT;  // 获取模板分组列表
        ABLLServerReady.ExecParam.I['DomainID'] := FDomainID;
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

procedure TfrmItem.FormCreate(Sender: TObject);
begin
  FDomainID := 0;

  FHCEdit := THCEdit.Create(Self);
  FHCEdit.Parent := Self.pnlEdit;
  FHCEdit.Align := alClient;
end;

procedure TfrmItem.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHCEdit);
end;

procedure TfrmItem.SetDomainID(Value: Integer);
begin
  FDomainID := Value;
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDOMAINCONTENT;  // 获取数据元选项值域对应的内容
      ABLLServerReady.ExecParam.I['DomainID'] := FDomainID;
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
          FHCEdit.LoadFromStream(vSM);
      finally
        FreeAndNil(vSM);
      end;
    end);
end;

procedure TfrmItem.sgdDEDblClick(Sender: TObject);
var
  vDeItem: TEmrTextItem;
begin
  if sgdDE.Row < 0 then Exit;

    vDeItem := TEmrTextItem.CreateByText(sgdDE.Cells[1, sgdDE.Row]);
    vDeItem[TDeProp.Name] := sgdDE.Cells[1, sgdDE.Row];
    vDeItem[TDeProp.Index] := sgdDE.Cells[0, sgdDE.Row];
    vDeItem[TDeProp.Code] := sgdDE.Cells[2, sgdDE.Row];

    FHCEdit.InsertItem(vDeItem);
end;

end.
