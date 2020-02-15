unit frm_Symbol;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, HCView;

type
  TfrmSymbol = class(TForm)
    Label1: TLabel;
    lbl1: TLabel;
    tbcsy: TTabControl;
    sgdSymbol: TStringGrid;
    btnOkAndClose: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sgdSymbolDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sgdSymbolMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure rztcsyChange(Sender: TObject);
    procedure btncancelClick(Sender: TObject);
    procedure btnokClick(Sender: TObject);
    procedure sgdSymbolDblClick(Sender: TObject);
    procedure btnOkAndCloseClick(Sender: TObject);
  private
    FSyList: TStringList;
    FARow, FACol: integer; //Grid的行列值
    procedure IniSpecialSymbols;
    procedure LoadSpecialSymbols(aIndex: Integer);
    { Private declarations }
  public
    AppPath: string;
    HCView: THCView;
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TfrmSymbol.btncancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSymbol.btnOkAndCloseClick(Sender: TObject);
begin
  if sgdSymbol.Cells[FACol, FARow] <> '' then
  begin
    HCView.InsertText(sgdSymbol.Cells[FACol, FARow]);
    Close;
  end;
end;

procedure TfrmSymbol.btnokClick(Sender: TObject);
begin
  if sgdSymbol.Cells[FACol, FARow] <> '' then
    HCView.InsertText(sgdSymbol.Cells[FACol, FARow]);
end;

procedure TfrmSymbol.FormCreate(Sender: TObject);
begin
  FSyList := TStringList.Create;
end;

procedure TfrmSymbol.FormShow(Sender: TObject);
begin
  IniSpecialSymbols;
  LoadSpecialSymbols(0);
  Label1.Caption := sgdSymbol.Cells[0, 0];
end;

procedure TfrmSymbol.IniSpecialSymbols;
var
  i, vTabIndex: Integer;
begin
  if FileExists(AppPath + '\Symbol.txt') then
    FSyList.LoadFromFile(AppPath + '\Symbol.txt');

  tbcsy.Tabs.Clear;
  for i := 0 to FSyList.Count - 1 do
  begin
    vTabIndex := tbcsy.Tabs.Add('');
    tbcsy.Tabs[vTabIndex] := FSyList.Names[i];
  end;
  tbcsy.TabIndex := 0;
end;

procedure TfrmSymbol.sgdSymbolDblClick(Sender: TObject);
begin
  btnOkAndCloseClick(Sender);
end;

procedure TfrmSymbol.sgdSymbolDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  vCellStr: string;
begin
  vCellStr := sgdSymbol.Cells[ACol, ARow];
  with sgdSymbol.Canvas do
  begin
    if (ACol = FACol) and (ARow = FARow) and (vCellStr <> '') then
    begin
      Brush.Color := clSkyBlue;
      FillRect(Rect);
    end
    else
    begin
      Brush.Style := bsClear;
      Brush.Color := clWhite;
      FillRect(Rect);
    end;
    DrawText(Handle, PChar(vCellStr), -1, Rect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);  //水平，垂直，不折行
  end;
end;

procedure TfrmSymbol.sgdSymbolMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  sgdSymbol.MouseToCell(X, Y, FACol, FARow);
  Label1.Caption := sgdSymbol.Cells[FACol, FARow];
end;

procedure TfrmSymbol.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSyList);
  Self := nil;
end;

procedure TfrmSymbol.rztcsyChange(Sender: TObject);
begin
  LoadSpecialSymbols(tbcsy.TabIndex);
  Label1.Caption := sgdSymbol.Cells[FACol, FARow];
end;

procedure TfrmSymbol.LoadSpecialSymbols(aIndex: Integer);
var
  i, j, k: Integer;
  vSyList: TStringList;
begin
  vSyList := TStringList.Create;
  try
    vSyList.DelimitedText := FSyList.ValueFromIndex[aIndex];
    //if FSyList.Count > 120 then
    k := 0;
    for i := 0 to sgdSymbol.RowCount - 1 do
    begin
      for j := 0 to sgdSymbol.ColCount - 1 do
      begin
        if k <= vSyList.Count - 1 then
        begin
          sgdSymbol.Cells[j, i] := vSyList[k];
          Inc(k);
        end
        else
          sgdSymbol.Cells[j, i] := '';
      end;
    end;
  finally
    FreeAndNil(vSyList);
  end;
end;

end.
