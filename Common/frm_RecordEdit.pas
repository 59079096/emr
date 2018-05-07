{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_RecordEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.XPMan,
  System.ImageList, Vcl.ImgList, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ToolWin,
  System.Generics.Collections, EmrView, HCView, HCCustomRichData, HCItem,
  EmrGroupItem, EmrElementItem, HCDrawItem, frm_RecordPop;

type
  //TItemMouseClickEvent = procedure(const AData: TCustomRichData;
  //  const AGroup: TDeGroup; const AItem: TEmrTextItem;
  //  const ADrawItem: THCCustomDrawItem; const APopupPoint: TPoint) of object;

  TfrmRecordEdit = class(TForm)
    tlbTool: TToolBar;
    btnFile: TToolButton;
    btnSave: TToolButton;
    btnprint: TToolButton;
    btn3: TToolButton;
    cbbZoom: TComboBox;
    btnAnnotation: TToolButton;
    btnSymmetryMargin: TToolButton;
    btn4: TToolButton;
    cbbFont: TComboBox;
    btn1: TToolButton;
    cbbFontSize: TComboBox;
    cbBackColor: TColorBox;
    cbFontColor: TColorBox;
    btnBold: TToolButton;
    btnItalic: TToolButton;
    btnUnderLine: TToolButton;
    btnStrikeOut: TToolButton;
    btnSuperScript: TToolButton;
    btnSubScript: TToolButton;
    btnAlignLeft: TToolButton;
    btnAlignCenter: TToolButton;
    btnAlignRight: TToolButton;
    btnAlignJustify: TToolButton;
    btnAlignScatter: TToolButton;
    btnLineSpace: TToolButton;
    btn9: TToolButton;
    sbStatus: TStatusBar;
    il1: TImageList;
    pmRichEdit: TPopupMenu;
    mniCut: TMenuItem;
    mniCopy: TMenuItem;
    mniPaste: TMenuItem;
    mniTable: TMenuItem;
    mniInsertRowTop: TMenuItem;
    mniInsertRowBottom: TMenuItem;
    mniInsertColLeft: TMenuItem;
    mniInsertColRight: TMenuItem;
    mniDeleteRow: TMenuItem;
    mniDeleteCol: TMenuItem;
    mniN25: TMenuItem;
    mniDisBorder: TMenuItem;
    mniPara: TMenuItem;
    pmLineSpace: TPopupMenu;
    mniLineSpace100: TMenuItem;
    mniLineSpace150: TMenuItem;
    mniLineSpace200: TMenuItem;
    pmFile: TPopupMenu;
    mniOpen: TMenuItem;
    mniSave: TMenuItem;
    mniSaveAs: TMenuItem;
    mniPageSet: TMenuItem;
    mniPrint: TMenuItem;
    mniPrintByLine: TMenuItem;
    btn5: TToolButton;
    mniN1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBoldClick(Sender: TObject);
    procedure btnprintClick(Sender: TObject);
    procedure cbbZoomChange(Sender: TObject);
    procedure btnAnnotationClick(Sender: TObject);
    procedure btnSymmetryMarginClick(Sender: TObject);
    procedure cbbFontChange(Sender: TObject);
    procedure cbbFontSizeChange(Sender: TObject);
    procedure cbBackColorChange(Sender: TObject);
    procedure cbFontColorChange(Sender: TObject);
    procedure btnAlignLeftClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure mniLineSpace100Click(Sender: TObject);
    procedure mniOpenClick(Sender: TObject);
    procedure btn5Click(Sender: TObject);
    procedure mniN1Click(Sender: TObject);
    procedure mniInsertRowTopClick(Sender: TObject);
    procedure mniInsertRowBottomClick(Sender: TObject);
    procedure mniInsertColLeftClick(Sender: TObject);
    procedure mniInsertColRightClick(Sender: TObject);
    procedure mniDeleteRowClick(Sender: TObject);
    procedure mniDeleteColClick(Sender: TObject);
    procedure mniDisBorderClick(Sender: TObject);
    procedure mniCutClick(Sender: TObject);
    procedure mniCopyClick(Sender: TObject);
    procedure mniPasteClick(Sender: TObject);
    procedure mniParaClick(Sender: TObject);
    procedure mniSaveAsClick(Sender: TObject);
    procedure mniPrintByLineClick(Sender: TObject);
    procedure pmRichEditPopup(Sender: TObject);
  private
    { Private declarations }
    FfrmRecordPop: TfrmRecordPop;
    FEmrView: TEmrView;
    FDeGroupStack: TStack<Integer>;

    FOnSave, FOnChangedSwitch, FOnReadOnlySwitch: TNotifyEvent;
    //
    function GetFontSizeStr(AFontSize: Integer): string;
    function GetPaperSizeStr(APaperSize: Integer): string;
    procedure GetPagesAndActive;
    procedure DoCaretChange(Sender: TObject);
    procedure DoChangedSwitch(Sender: TObject);
    procedure DoReadOnlySwitch(Sender: TObject);
    procedure DoVerScroll(Sender: TObject);
    procedure DoActiveItemChange(Sender: TObject);
    procedure CurTextStyleChange(const ANewStyleNo: Integer);
    procedure CurParaStyleChange(const ANewStyleNo: Integer);
    //
    function PopupForm: TfrmRecordPop;
  protected
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoItemLoaded(const AItem: THCCustomItem);
    procedure DoSave;
  public
    { Public declarations }
    ObjectData: Pointer;
    procedure HideToolbar;
    property EmrView: TEmrView read FEmrView;
    property OnSave: TNotifyEvent read FOnSave write FOnSave;

    /// <summary> Changed状态发生切换时触发 </summary>
    property OnChangedSwitch: TNotifyEvent read FOnChangedSwitch write FOnChangedSwitch;
    property OnReadOnlySwitch: TNotifyEvent read FOnReadOnlySwitch write FOnReadOnlySwitch;
  end;

implementation

uses
  Vcl.Clipbrd, HCCommon, HCStyle, HCTextStyle, HCParaStyle, HCBitmapItem, System.DateUtils,
  frm_InsertTable, frm_Paragraph, HCTableItem;

{$R *.dfm}

procedure TfrmRecordEdit.btn5Click(Sender: TObject);
var
  vFrmInsertTable: TfrmInsertTable;
begin
  vFrmInsertTable := TfrmInsertTable.Create(Self);
  try
    vFrmInsertTable.ShowModal;
    if vFrmInsertTable.ModalResult = mrOk then
      FEmrView.InsertTable(StrToInt(vFrmInsertTable.edtRows.Text),
        StrToInt(vFrmInsertTable.edtCols.Text));
  finally
    FreeAndNil(vFrmInsertTable);
  end;
end;

procedure TfrmRecordEdit.btnAlignLeftClick(Sender: TObject);
begin
  case (Sender as TToolButton).Tag of
    0: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahLeft);
    1: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahCenter);
    2: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahRight);
    3: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahJustify);  // 两端
    4: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahScatter);  // 分散
  end;
end;

procedure TfrmRecordEdit.btnAnnotationClick(Sender: TObject);
begin
  FEmrView.ShowAnnotation := not FEmrView.ShowAnnotation;
end;

procedure TfrmRecordEdit.btnBoldClick(Sender: TObject);
begin
  case (Sender as TToolButton).Tag of
    0: FEmrView.ApplyTextStyle(TFontStyleEx.tsBold);
    1: FEmrView.ApplyTextStyle(TFontStyleEx.tsItalic);
    2: FEmrView.ApplyTextStyle(TFontStyleEx.tsUnderline);
    3: FEmrView.ApplyTextStyle(TFontStyleEx.tsStrikeOut);
    4: FEmrView.ApplyTextStyle(TFontStyleEx.tsSuperscript);
    5: FEmrView.ApplyTextStyle(TFontStyleEx.tsSubscript);
  end;
end;

procedure TfrmRecordEdit.btnprintClick(Sender: TObject);
begin
  FEmrView.Print('');
end;

procedure TfrmRecordEdit.btnSaveClick(Sender: TObject);
begin
  DoSave;
end;

procedure TfrmRecordEdit.btnSymmetryMarginClick(Sender: TObject);
begin
  FEmrView.SymmetryMargin := not FEmrView.SymmetryMargin;
end;

procedure TfrmRecordEdit.cbbFontChange(Sender: TObject);
begin
  FEmrView.ApplyTextFontName(cbbFont.Text);
end;

procedure TfrmRecordEdit.cbbFontSizeChange(Sender: TObject);
begin
  FEmrView.ApplyTextFontSize(GetFontSize(cbbFontSize.Text));
end;

procedure TfrmRecordEdit.cbbZoomChange(Sender: TObject);
begin
  FEmrView.Zoom := (StrToInt(cbbZoom.Text) / 100);
end;

procedure TfrmRecordEdit.cbBackColorChange(Sender: TObject);
begin
  FEmrView.ApplyTextBackColor(cbBackColor.Selected);
end;

procedure TfrmRecordEdit.cbFontColorChange(Sender: TObject);
begin
  FEmrView.ApplyTextColor(cbFontColor.Selected);
end;

procedure TfrmRecordEdit.CurParaStyleChange(const ANewStyleNo: Integer);
var
  vAlignHorz: TParaAlignHorz;
begin
  if ANewStyleNo >= 0 then
  begin
    vAlignHorz := FEmrView.Style.ParaStyles[ANewStyleNo].AlignHorz;

    btnAlignLeft.Down := vAlignHorz = TParaAlignHorz.pahLeft;
    btnAlignRight.Down := vAlignHorz = TParaAlignHorz.pahRight;
    btnAlignCenter.Down := vAlignHorz = TParaAlignHorz.pahCenter;
    btnAlignJustify.Down := vAlignHorz = TParaAlignHorz.pahJustify;
    btnAlignScatter.Down := vAlignHorz = TParaAlignHorz.pahScatter;

    case FEmrView.Style.ParaStyles[ANewStyleNo].LineSpace of
      THCStyle.LineSpace100: mniLineSpace100.Checked := True;
      THCStyle.LineSpace150: mniLineSpace150.Checked := True;
      THCStyle.LineSpace200: mniLineSpace200.Checked := True;
    end;
  end;
end;

procedure TfrmRecordEdit.CurTextStyleChange(const ANewStyleNo: Integer);
begin
  if ANewStyleNo >= 0 then
  begin
    cbbFont.ItemIndex := cbbFont.Items.IndexOf(FEmrView.Style.TextStyles[ANewStyleNo].Family);
    cbbFontSize.ItemIndex := cbbFontSize.Items.IndexOf(GetFontSizeStr(FEmrView.Style.TextStyles[ANewStyleNo].Size));
    btnBold.Down := tsBold in FEmrView.Style.TextStyles[ANewStyleNo].FontStyle;
    btnItalic.Down := tsItalic in FEmrView.Style.TextStyles[ANewStyleNo].FontStyle;
    btnUnderline.Down := tsUnderline in FEmrView.Style.TextStyles[ANewStyleNo].FontStyle;
    btnStrikeOut.Down := tsStrikeOut in FEmrView.Style.TextStyles[ANewStyleNo].FontStyle;
    btnSuperscript.Down := tsSuperscript in FEmrView.Style.TextStyles[ANewStyleNo].FontStyle;
    btnSubscript.Down := tsSubscript in FEmrView.Style.TextStyles[ANewStyleNo].FontStyle;
  end;
end;

procedure TfrmRecordEdit.DoActiveItemChange(Sender: TObject);
begin
  FEmrView.ActiveSection.ReFormatActiveItem;
end;

procedure TfrmRecordEdit.DoCaretChange(Sender: TObject);
var
  vStyleNo, vParaNo: Integer;
begin
  GetPagesAndActive;
  FEmrView.GetCurStyle(vStyleNo, vParaNo);
  if vStyleNo < 0 then
    //GStyle.CurStyleNo := 0
  else
    FEmrView.Style.CurStyleNo := vStyleNo;
  FEmrView.Style.CurParaNo := vParaNo;
  CurTextStyleChange(vStyleNo);
  CurParaStyleChange(vParaNo);
end;

procedure TfrmRecordEdit.DoChangedSwitch(Sender: TObject);
begin
  if Assigned(FOnChangedSwitch) then
    FOnChangedSwitch(Self);
end;

procedure TfrmRecordEdit.DoItemLoaded(const AItem: THCCustomItem);
begin
end;

procedure TfrmRecordEdit.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if PopupForm.Visible then
    PopupForm.Close;
end;

procedure TfrmRecordEdit.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vActiveItem: THCCustomItem;
  vEmrTextItem: TEmrTextItem;
  vDeGroup: TDeGroup;
  vActiveDrawItem: THCCustomDrawItem;
  vPt: TPoint;
  vInfo: string;
begin
  vInfo := '';

  if FEmrView.ActiveSection.ActiveData = nil then Exit;

  if FEmrView.ActiveSection.ActiveData.ReadOnly then Exit;

  vActiveItem := FEmrView.GetActiveItem;
  if vActiveItem <> nil then
  begin
    if vActiveItem is TEmrTextItem then
    begin
      vEmrTextItem := vActiveItem as TEmrTextItem;
      if vEmrTextItem[TDeProp.Index] = '' then Exit;

      if FEmrView.ActiveSection.ActiveData.ActiveDomain <> nil then
      begin
        //vDeGroup := FEmrView.GetDeGroupByNo((vActiveItem as TEmrTextItem).DeGroup);
        vDeGroup := FEmrView.ActiveSection.ActiveData.Items[
          FEmrView.ActiveSection.ActiveData.ActiveDomain.BeginNo] as TDeGroup;
        vInfo := vDeGroup[TDeProp.Name];
      end;
      vActiveDrawItem := FEmrView.GetActiveDrawItem;
      if vEmrTextItem.Active then
      begin
        vInfo := vInfo + vEmrTextItem.Text;
        vPt := FEmrView.GetActiveDrawItemCoord;
        vPt.Y := vPt.Y + FEmrView.ZoomIn(vActiveDrawItem.Height);
        PopupForm.Left := vPt.X + FEmrView.Left;
        PopupForm.Top := vPt.Y + FEmrView.Top;

        PopupForm.PopupEmrElement(vEmrTextItem);
      end;
    end;
  end;

  sbStatus.Panels[1].Text := vInfo;
end;

procedure TfrmRecordEdit.DoReadOnlySwitch(Sender: TObject);
begin
  if Assigned(FOnReadOnlySwitch) then
    FOnReadOnlySwitch(Self);
end;

procedure TfrmRecordEdit.DoSave;
begin
  if Assigned(FOnSave) and (not FEmrView.ReadOnly) then
    FOnSave(Self);
end;

procedure TfrmRecordEdit.DoVerScroll(Sender: TObject);
begin
  GetPagesAndActive;
end;

procedure TfrmRecordEdit.FormCreate(Sender: TObject);
begin
  cbbFont.Items := Screen.Fonts;
  cbbFont.ItemIndex := cbbFont.Items.IndexOf('宋体');

  FDeGroupStack := TStack<Integer>.Create;

  FEmrView := TEmrView.Create(Self);
  FEmrView.OnMouseDown := DoMouseDown;
  FEmrView.OnMouseUp := DoMouseUp;
  FEmrView.OnCaretChange := DoCaretChange;
  FEmrView.OnVerScroll := DoVerScroll;
  FEmrView.OnChangedSwitch := DoChangedSwitch;
  FEmrView.OnReadOnlySwitch := DoReadOnlySwitch;
  FEmrView.PopupMenu := pmRichEdit;
  FEmrView.Parent := Self;
  FEmrView.Align := alClient;
end;

procedure TfrmRecordEdit.FormDestroy(Sender: TObject);
begin
  if Assigned(FfrmRecordPop) then
    FreeAndNil(FfrmRecordPop);
  FEmrView.Free;
  FDeGroupStack.Free;
end;

function TfrmRecordEdit.GetFontSizeStr(AFontSize: Integer): string;
begin
  Result := IntToStr(AFontSize);
  if AFontSize = 42 then Result := '初号';
  if AFontSize = 36 then Result := '小初';
  if AFontSize = 26 then Result := '一号';
  if AFontSize = 24 then Result := '小一';
  if AFontSize = 22 then Result := '二号';
  if AFontSize = 18 then Result := '小二';
  if AFontSize = 16 then Result := '三号';
  if AFontSize = 15 then Result := '小三';
  if AFontSize = 14 then Result := '四号';
  if AFontSize = 12 then Result := '小四';
  if AFontSize = 11 then Result := '五号';
  if AFontSize = 9 then Result := '小五';
  if AFontSize = 7 then Result := '六号';
  if AFontSize = 6 then Result := '小六';
  if AFontSize = 5 then Result := '七号';
end;

procedure TfrmRecordEdit.GetPagesAndActive;
begin
  sbStatus.Panels[0].Text := '预览' + IntToStr(FEmrView.PagePreviewFirst + 1)
    + '页 光标' + IntToStr(FEmrView.ActivePageIndex + 1)
    + '页 共' + IntToStr(FEmrView.PageCount) + '页';
end;

function TfrmRecordEdit.GetPaperSizeStr(APaperSize: Integer): string;
begin
  case APaperSize of
    DMPAPER_A3: Result := 'A3';
    DMPAPER_A4: Result := 'A4';
    DMPAPER_A5: Result := 'A5';
    DMPAPER_B5: Result := 'B5';
  else
    Result := '自定义';
  end;
end;

procedure TfrmRecordEdit.HideToolbar;
begin
  tlbTool.Visible := False;
end;

procedure TfrmRecordEdit.mniOpenClick(Sender: TObject);
var
  vOpenDlg: TOpenDialog;
begin
  if FEmrView.ReadOnly then Exit;

  vOpenDlg := TOpenDialog.Create(Self);
  try
    vOpenDlg.Filter := '文件|*.cff';
    if vOpenDlg.Execute then
    begin
      if vOpenDlg.FileName <> '' then
        FEmrView.LoadFromFile(vOpenDlg.FileName);
    end;
  finally
    FreeAndNil(vOpenDlg);
  end;
end;

procedure TfrmRecordEdit.mniPrintByLineClick(Sender: TObject);
begin
  FEmrView.PrintCurPageByActiveLine(False, False);
end;

procedure TfrmRecordEdit.mniSaveAsClick(Sender: TObject);
var
  vSaveDlg: TSaveDialog;
  vFileName: string;
begin
  vSaveDlg := TSaveDialog.Create(nil);
  try
    vSaveDlg.Filter := '文件|*.cff';
    if vSaveDlg.Execute then
    begin
      if vSaveDlg.FileName <> '' then
      begin
        vFileName := vSaveDlg.FileName;
        if ExtractFileExt(vFileName) <> HC_EXT then
          vFileName := vFileName + HC_EXT;

        FEmrView.SaveToFile(vFileName);
      end;
    end;
  finally
    FreeAndNil(vSaveDlg);
  end;
end;

procedure TfrmRecordEdit.pmRichEditPopup(Sender: TObject);
begin
  if FEmrView.ActiveSection.SelectExists then
  begin
    mniCut.Enabled := not FEmrView.ActiveSection.ActiveData.GetTopLevelData.ReadOnly;
    mniCopy.Enabled := True;
  end
  else
  begin
    mniCut.Enabled := False;
    mniCopy.Enabled := False;
  end;

  mniTable.Enabled := FEmrView.ActiveSection.ActiveData.GetCurItem is THCTableItem;  // 不用GetTopLevelData，否则在单元格里时右键不能操作表格了
  mniPaste.Enabled :=
          Clipboard.HasFormat(HC_FILEFORMAT)  // cff格式
          or Clipboard.HasFormat(CF_TEXT);  // 文本格式
end;

function TfrmRecordEdit.PopupForm: TfrmRecordPop;
begin
  if not Assigned(FfrmRecordPop) then
  begin
    FfrmRecordPop := TfrmRecordPop.Create(nil);
    FfrmRecordPop.OnActiveItemChange := DoActiveItemChange;
    FfrmRecordPop.Parent := Self;
  end;

  Result := FfrmRecordPop;
end;

procedure TfrmRecordEdit.mniDeleteColClick(Sender: TObject);
begin
  FEmrView.ActiveTableDeleteCol(1);
end;

procedure TfrmRecordEdit.mniDeleteRowClick(Sender: TObject);
begin
  FEmrView.ActiveTableDeleteRow(1);
end;

procedure TfrmRecordEdit.mniDisBorderClick(Sender: TObject);
var
  vTable: THCTableItem;
begin
  if FEmrView.ActiveSection.ActiveData.GetCurItem is THCTableItem then
  begin
    vTable := FEmrView.ActiveSection.ActiveData.GetCurItem as THCTableItem;
    vTable.BorderVisible := not vTable.BorderVisible;
    FEmrView.UpdateBuffer;
  end;
end;

procedure TfrmRecordEdit.mniInsertColLeftClick(Sender: TObject);
begin
  FEmrView.ActiveTableInsertColBefor(1);
end;

procedure TfrmRecordEdit.mniInsertColRightClick(Sender: TObject);
begin
  FEmrView.ActiveTableInsertColAfter(1);
end;

procedure TfrmRecordEdit.mniInsertRowBottomClick(Sender: TObject);
begin
  FEmrView.ActiveTableInsertRowAfter(1);
end;

procedure TfrmRecordEdit.mniInsertRowTopClick(Sender: TObject);
begin
  FEmrView.ActiveTableInsertRowBefor(1);
end;

procedure TfrmRecordEdit.mniLineSpace100Click(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    case (Sender as TMenuItem).Tag of
      0: FEmrView.ApplyParaLineSpace(THCStyle.LineSpace100);
      1: FEmrView.ApplyParaLineSpace(THCStyle.LineSpace150);
      2: FEmrView.ApplyParaLineSpace(THCStyle.LineSpace200);
    end;
  end;
end;

procedure TfrmRecordEdit.mniN1Click(Sender: TObject);
begin
  FEmrView.MergeTableSelectCells;
end;

procedure TfrmRecordEdit.mniParaClick(Sender: TObject);
var
  vFrmParagraph: TfrmParagraph;
begin
  vFrmParagraph := TfrmParagraph.Create(Self);
  try
    vFrmParagraph.edtLineSpace.Text := IntToStr(FEmrView.Style.ParaStyles[FEmrView.Style.CurParaNo].LineSpace);
    vFrmParagraph.cbbAlignHorz.ItemIndex := Ord(FEmrView.Style.ParaStyles[FEmrView.Style.CurParaNo].AlignHorz);
    vFrmParagraph.cbbAlignVert.ItemIndex := Ord(FEmrView.Style.ParaStyles[FEmrView.Style.CurParaNo].AlignVert);
    vFrmParagraph.clrbxBG.Color := FEmrView.Style.ParaStyles[FEmrView.Style.CurParaNo].BackColor;

    vFrmParagraph.ShowModal;
    if vFrmParagraph.ModalResult = mrOk then
    begin
      FEmrView.BeginUpdate;
      try
        FEmrView.ApplyParaLineSpace(StrToIntDef(vFrmParagraph.edtLineSpace.Text, 8));
        FEmrView.ApplyParaAlignHorz(TParaAlignHorz(vFrmParagraph.cbbAlignHorz.ItemIndex));
        FEmrView.ApplyParaAlignVert(TParaAlignVert(vFrmParagraph.cbbAlignVert.ItemIndex));
        FEmrView.ApplyParaBackColor(vFrmParagraph.clrbxBG.Color);
      finally
        FEmrView.EndUpdate;
      end;
    end;
  finally
    FreeAndNil(vFrmParagraph);
  end;
end;

procedure TfrmRecordEdit.mniCopyClick(Sender: TObject);
begin
  FEmrView.Copy;
end;

procedure TfrmRecordEdit.mniCutClick(Sender: TObject);
begin
  FEmrView.Cut;
end;

procedure TfrmRecordEdit.mniPasteClick(Sender: TObject);
begin
  FEmrView.Paste;
end;

end.
