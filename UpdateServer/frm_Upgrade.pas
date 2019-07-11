unit frm_Upgrade;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Grids, Vcl.Menus;

type
  TfrmUpgrade = class(TForm)
    pnl2: TPanel;
    lbl1: TLabel;
    lblVersion: TLabel;
    btnOK: TButton;
    edtVersion: TEdit;
    mmo: TMemo;
    sgdFiles: TStringGrid;
    pmFile: TPopupMenu;
    mniAdd: TMenuItem;
    mniDelete: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure mniAddClick(Sender: TObject);
    procedure mniDeleteClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UpgradePath: string;

implementation

uses
  frm_DM, UPCommon, UPMsgPack, Xml.XMLDoc, Xml.XMLIntf;

// 获取版本号
function GetFileVersionStr(const AFileName: string): string;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := '-1';
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
      begin
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
        begin
          Result := IntToStr(HIWORD(FI.dwFileVersionMS)) + '.' +
            IntToStr(LOWORD(FI.dwFileVersionMS)) + '.' +
            IntToStr(HIWORD(FI.dwFileVersionLS)) + '.' +
            IntToStr(LOWORD(FI.dwFileVersionLS));
        end;
      end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

{$R *.dfm}

procedure TfrmUpgrade.btnOKClick(Sender: TObject);
var
  vXmlDoc: IXMLDocument;
  vFileNode: IXMLNode;
  vR: Integer;
begin
  if sgdFiles.RowCount <= 1 then Exit;

  vXmlDoc := TXMLDocument.Create(nil);
  vXmlDoc.Active := True;
  vXmlDoc.Version := '1.0';
  vXmlDoc.DocumentElement := vXmlDoc.CreateNode('HCUpdateFile');
  vXmlDoc.DocumentElement.Attributes['upfver'] := '1';

  for vR := 1 to sgdFiles.RowCount - 1 do
  begin
    vFileNode := vXmlDoc.DocumentElement.AddChild('file' + vR.ToString);
    vFileNode.Text := sgdFiles.Cells[1, vR];  // name
    vFileNode.Attributes['version'] := sgdFiles.Cells[0, vR];
    vFileNode.Attributes['path'] := sgdFiles.Cells[2, vR];
    vFileNode.Attributes['size'] := sgdFiles.Cells[4, vR];
  end;

  if dm.ExecSql(Format('INSERT INTO UpdateInfo (version, memo, files) VALUES (''%s'', ''%s'', ''%s'')',
    [edtVersion.Text, mmo.Text, vXmlDoc.XML.Text]))
  then
  begin
    ShowMessage('提交成功！');
    btnOK.Enabled := False;
  end;
end;

procedure TfrmUpgrade.FormCreate(Sender: TObject);
begin
  sgdFiles.ColCount := 5;
  sgdFiles.ColWidths[0] := 80;
  sgdFiles.ColWidths[1] := 100;
  sgdFiles.ColWidths[2] := 100;
  sgdFiles.ColWidths[3] := 100;
  sgdFiles.ColWidths[4] := 0;
  //
  sgdFiles.Cells[0, 0] := '版本';
  sgdFiles.Cells[1, 0] := '文件名';
  sgdFiles.Cells[2, 0] := '客户端路径';
  sgdFiles.Cells[3, 0] := '大小';
end;

procedure TfrmUpgrade.mniAddClick(Sender: TObject);
var
  vDlg: TOpenDialog;
  vFileName, vPath: string;
  vFileHandle: THandle;
  vFileSize: Cardinal;
  i: Integer;
begin
  vDlg := TOpenDialog.Create(Self);
  try
    vDlg.InitialDir := UpgradePath;
    if vDlg.Execute then
    begin
      if vDlg.FileName <> '' then
      begin
        vFileHandle := FileOpen(vDlg.FileName, 0);
        try
          vFileSize := GetFileSize(vFileHandle, nil);
        finally
          FileClose(vFileHandle);
        end;

        if vFileSize > MAX_OBJECT_SIZE then
        begin
          ShowMessage(Format('文件体积超过允许的最大值 %s！', [BytesToStr(MAX_OBJECT_SIZE)]));
          Exit;
        end;

        vFileName := ExtractFileName(vDlg.FileName);
        vPath := StringReplace(ExtractFilePath(vDlg.FileName), UpgradePath, '', [rfReplaceAll, rfIgnoreCase]);

        for i := 1 to sgdFiles.RowCount - 1 do
        begin
          if (sgdFiles.Cells[1, i] = vFileName) and (sgdFiles.Cells[2, i] = vPath) then
          begin
            ShowMessage('已经存在此文件！');
            Exit;
          end;
        end;


        sgdFiles.RowCount := sgdFiles.RowCount + 1;
        if sgdFiles.RowCount > 1 then
          sgdFiles.FixedRows := 1;

        sgdFiles.Cells[0, sgdFiles.RowCount - 1] := GetFileVersionStr(vDlg.FileName);
        sgdFiles.Cells[1, sgdFiles.RowCount - 1] := vFileName;
        sgdFiles.Cells[2, sgdFiles.RowCount - 1] := vPath;
        sgdFiles.Cells[3, sgdFiles.RowCount - 1] := BytesToStr(vFileSize);
        sgdFiles.Cells[4, sgdFiles.RowCount - 1] := vFileSize.ToString;
      end;
    end;
  finally
    FreeAndNil(vDlg);
  end;
end;

procedure TfrmUpgrade.mniDeleteClick(Sender: TObject);
var
  vR, vC: Integer;
begin
  if sgdFiles.Row < 1 then Exit;

  for vR := sgdFiles.Row to sgdFiles.RowCount - 2 do
  begin
    for vC := 0 to 3 do
      sgdFiles.Cells[vC, vR] := sgdFiles.Cells[vC, vR + 1];
  end;

  sgdFiles.RowCount := sgdFiles.RowCount - 1;
  if sgdFiles.RowCount <= 1 then
    sgdFiles.FixedRows := 0;
end;

end.
