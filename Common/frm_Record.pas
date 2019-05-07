{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_Record;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.XPMan,
  System.ImageList, Vcl.ImgList, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ToolWin,
  System.Generics.Collections, EmrView, HCView, HCRichData, HCItem, HCCustomData,
  EmrGroupItem, EmrElementItem, HCDrawItem, HCSection, frm_RecordPop, System.Actions,
  Vcl.ActnList, Vcl.Buttons;

type
  TDeItemInsertEvent = procedure(const AEmrView: TEmrView; const ASection: THCSection;
    const AData: THCCustomData; const AItem: THCCustomItem) of object;

  TfrmRecord = class(TForm)
    tlbTool: TToolBar;
    btnFile: TToolButton;
    btnprint: TToolButton;
    btn3: TToolButton;
    cbbZoom: TComboBox;
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
    pmView: TPopupMenu;
    mniCut: TMenuItem;
    mniCopy: TMenuItem;
    mniPaste: TMenuItem;
    mniTable: TMenuItem;
    mniInsertRowTop: TMenuItem;
    mniInsertRowBottom: TMenuItem;
    mniInsertColLeft: TMenuItem;
    mniInsertColRight: TMenuItem;
    mniDeleteCurRow: TMenuItem;
    mniDeleteCurCol: TMenuItem;
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
    mniMerge: TMenuItem;
    mniN2: TMenuItem;
    pmInsert: TPopupMenu;
    mniInsertTable: TMenuItem;
    mniN4: TMenuItem;
    mniCheckbox: TMenuItem;
    mniInsertImage: TMenuItem;
    mniN7: TMenuItem;
    mniN8: TMenuItem;
    mniInsertLine: TMenuItem;
    mniN10: TMenuItem;
    N1: TMenuItem;
    mniInsertGif: TMenuItem;
    actlst: TActionList;
    actSave: TAction;
    mniDeItem: TMenuItem;
    mniDeGroup: TMenuItem;
    mniN11: TMenuItem;
    mniN12: TMenuItem;
    mniDeleteGroup: TMenuItem;
    mniN13: TMenuItem;
    mniN14: TMenuItem;
    mniN15: TMenuItem;
    mniN16: TMenuItem;
    mniEditItem: TMenuItem;
    mniN18: TMenuItem;
    mniLineSpace115: TMenuItem;
    mniControlItem: TMenuItem;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    mniSplitRow: TMenuItem;
    mniSplitCol: TMenuItem;
    mniN21: TMenuItem;
    mniCombobox: TMenuItem;
    mniTableProperty: TMenuItem;
    mniN3: TMenuItem;
    mniN5: TMenuItem;
    btnRightIndent: TToolButton;
    btnLeftIndent: TToolButton;
    btnUndo: TToolButton;
    btnRedo: TToolButton;
    actUndo: TAction;
    actRedo: TAction;
    btnSave: TSpeedButton;
    btnInsert: TSpeedButton;
    mniSplit: TMenuItem;
    mniDeleteProtect: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBoldClick(Sender: TObject);
    procedure btnprintClick(Sender: TObject);
    procedure cbbZoomChange(Sender: TObject);
    procedure btnSymmetryMarginClick(Sender: TObject);
    procedure cbbFontChange(Sender: TObject);
    procedure cbbFontSizeChange(Sender: TObject);
    procedure cbBackColorChange(Sender: TObject);
    procedure cbFontColorChange(Sender: TObject);
    procedure btnAlignLeftClick(Sender: TObject);
    procedure mniLineSpace100Click(Sender: TObject);
    procedure mniOpenClick(Sender: TObject);
    procedure mniMergeClick(Sender: TObject);
    procedure mniInsertRowTopClick(Sender: TObject);
    procedure mniInsertRowBottomClick(Sender: TObject);
    procedure mniInsertColLeftClick(Sender: TObject);
    procedure mniInsertColRightClick(Sender: TObject);
    procedure mniDeleteCurRowClick(Sender: TObject);
    procedure mniDeleteCurColClick(Sender: TObject);
    procedure mniDisBorderClick(Sender: TObject);
    procedure mniParaClick(Sender: TObject);
    procedure mniSaveAsClick(Sender: TObject);
    procedure mniPrintByLineClick(Sender: TObject);
    procedure pmViewPopup(Sender: TObject);
    procedure mniN2Click(Sender: TObject);
    procedure mniInsertTableClick(Sender: TObject);
    procedure mniCheckboxClick(Sender: TObject);
    procedure mniInsertImageClick(Sender: TObject);
    procedure mniN8Click(Sender: TObject);
    procedure mniInsertLineClick(Sender: TObject);
    procedure mniN10Click(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure mniInsertGifClick(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mniDeleteGroupClick(Sender: TObject);
    procedure mniN12Click(Sender: TObject);
    procedure mniPageSetClick(Sender: TObject);
    procedure mniN16Click(Sender: TObject);
    procedure mniEditItemClick(Sender: TObject);
    procedure mniN18Click(Sender: TObject);
    procedure mniSplitRowClick(Sender: TObject);
    procedure mniSplitColClick(Sender: TObject);
    procedure mniComboboxClick(Sender: TObject);
    procedure mniControlItemClick(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure mniTablePropertyClick(Sender: TObject);
    procedure mniN3Click(Sender: TObject);
    procedure mniN5Click(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure mniN11Click(Sender: TObject);
    procedure mniDeleteProtectClick(Sender: TObject);
  private
    { Private declarations }
    FfrmRecordPop: TfrmRecordPop;
    FEmrView: TEmrView;
    FDeGroupStack: TStack<Integer>;

    FOnSave, FOnChangedSwitch, FOnReadOnlySwitch: TNotifyEvent;
    FOnDeItemInsert: TDeItemInsertEvent;
    //
    procedure GetPagesAndActive;
    procedure DoCaretChange(Sender: TObject);
    procedure DoChangedSwitch(Sender: TObject);
    procedure DoCanNotEdit(Sender: TObject);
    procedure DoReadOnlySwitch(Sender: TObject);
    procedure DoVerScroll(Sender: TObject);
    procedure DoPaintPaperBefor(const Sender: TObject; const APageIndex: Integer;
      const ARect: TRect; const ACanvas: TCanvas; const APaintInfo: TSectionPaintInfo);
    procedure DoSetActiveDeItemText(const AText: string);
    procedure DoSetActiveDeItemExtra(const AStream: TStream);
    procedure CurTextStyleChange(const ANewStyleNo: Integer);
    procedure CurParaStyleChange(const ANewStyleNo: Integer);
    //
    function PopupForm: TfrmRecordPop;
    procedure PopupFormClose;
  protected
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoItemInsert(const Sender: TObject; const AData: THCCustomData;
      const AItem: THCCustomItem);
    procedure DoSave;
  public
    { Public declarations }
    ObjectData: Pointer;
    procedure HideToolbar;
    property EmrView: TEmrView read FEmrView;
    procedure InsertDataElementAsDE(const AIndex, AName: string);
    property OnSave: TNotifyEvent read FOnSave write FOnSave;

    /// <summary> Changed状态发生切换时触发 </summary>
    property OnChangedSwitch: TNotifyEvent read FOnChangedSwitch write FOnChangedSwitch;
    property OnReadOnlySwitch: TNotifyEvent read FOnReadOnlySwitch write FOnReadOnlySwitch;
    property OnDeItemInsert: TDeItemInsertEvent read FOnDeItemInsert write FOnDeItemInsert;
  end;

implementation

uses
  Vcl.Clipbrd, HCCommon, HCStyle, HCTextStyle, HCParaStyle, System.DateUtils,
  frm_InsertTable, frm_Paragraph, HCRectItem, HCImageItem, HCGifItem, HCExpressItem,
  HCViewData, EmrToothItem, EmrFangJiaoItem, frm_PageSet, frm_DeControlProperty,
  frm_DeTableProperty, frm_TableBorderBackColor, frm_DeProperty, emr_Common;

{$R *.dfm}

procedure TfrmRecord.actCopyExecute(Sender: TObject);
begin
  FEmrView.Copy;
end;

procedure TfrmRecord.actCutExecute(Sender: TObject);
begin
  FEmrView.Cut;
end;

procedure TfrmRecord.actPasteExecute(Sender: TObject);
begin
  FEmrView.Paste;
end;

procedure TfrmRecord.actRedoExecute(Sender: TObject);
begin
  FEmrView.Redo;
end;

procedure TfrmRecord.actSaveExecute(Sender: TObject);
begin
  DoSave;
end;

procedure TfrmRecord.actUndoExecute(Sender: TObject);
begin
  FEmrView.Undo;
end;

procedure TfrmRecord.btnInsertClick(Sender: TObject);
var
  vPt: TPoint;
begin
  vPt := btnInsert.ClientToScreen(Point(0, btnInsert.Height));
  pmInsert.Popup(vPt.X, vPt.Y);
end;

procedure TfrmRecord.btnAlignLeftClick(Sender: TObject);
begin
  case (Sender as TToolButton).Tag of
    0: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahLeft);
    1: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahCenter);
    2: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahRight);
    3: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahJustify);  // 两端
    4: FEmrView.ApplyParaAlignHorz(TParaAlignHorz.pahScatter);  // 分散
    5: FEmrView.ApplyParaLeftIndent;
    6: FEmrView.ApplyParaLeftIndent(False);
  end;
end;

procedure TfrmRecord.btnBoldClick(Sender: TObject);
begin
  case (Sender as TToolButton).Tag of
    0: FEmrView.ApplyTextStyle(THCFontStyle.tsBold);
    1: FEmrView.ApplyTextStyle(THCFontStyle.tsItalic);
    2: FEmrView.ApplyTextStyle(THCFontStyle.tsUnderline);
    3: FEmrView.ApplyTextStyle(THCFontStyle.tsStrikeOut);
    4: FEmrView.ApplyTextStyle(THCFontStyle.tsSuperscript);
    5: FEmrView.ApplyTextStyle(THCFontStyle.tsSubscript);
  end;
end;

procedure TfrmRecord.btnprintClick(Sender: TObject);
begin
  FEmrView.Print('');
end;

procedure TfrmRecord.btnSymmetryMarginClick(Sender: TObject);
begin
  FEmrView.SymmetryMargin := not FEmrView.SymmetryMargin;
end;

procedure TfrmRecord.cbbFontChange(Sender: TObject);
begin
  FEmrView.ApplyTextFontName(cbbFont.Text);
  if not FEmrView.Focused then
    FEmrView.SetFocus;
end;

procedure TfrmRecord.cbbFontSizeChange(Sender: TObject);
begin
  FEmrView.ApplyTextFontSize(GetFontSize(cbbFontSize.Text));
  if not FEmrView.Focused then
    FEmrView.SetFocus;
end;

procedure TfrmRecord.cbbZoomChange(Sender: TObject);
begin
  FEmrView.Zoom := (StrToInt(cbbZoom.Text) / 100);
end;

procedure TfrmRecord.cbBackColorChange(Sender: TObject);
begin
  FEmrView.ApplyTextBackColor(cbBackColor.Selected);
  if not FEmrView.Focused then
    FEmrView.SetFocus;
end;

procedure TfrmRecord.cbFontColorChange(Sender: TObject);
begin
  FEmrView.ApplyTextColor(cbFontColor.Selected);
  if not FEmrView.Focused then
    FEmrView.SetFocus;
end;

procedure TfrmRecord.CurParaStyleChange(const ANewStyleNo: Integer);
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

    case FEmrView.Style.ParaStyles[ANewStyleNo].LineSpaceMode of
      TParaLineSpaceMode.pls100: mniLineSpace100.Checked := True;
      TParaLineSpaceMode.pls115: mniLineSpace115.Checked := True;
      TParaLineSpaceMode.pls150: mniLineSpace150.Checked := True;
      TParaLineSpaceMode.pls200: mniLineSpace200.Checked := True;
    end;
  end;
end;

procedure TfrmRecord.CurTextStyleChange(const ANewStyleNo: Integer);
begin
  if ANewStyleNo >= 0 then
  begin
    cbbFont.ItemIndex := cbbFont.Items.IndexOf(FEmrView.Style.TextStyles[ANewStyleNo].Family);
    cbbFontSize.ItemIndex := cbbFontSize.Items.IndexOf(GetFontSizeStr(FEmrView.Style.TextStyles[ANewStyleNo].Size));
    btnBold.Down := tsBold in FEmrView.Style.TextStyles[ANewStyleNo].FontStyles;
    btnItalic.Down := tsItalic in FEmrView.Style.TextStyles[ANewStyleNo].FontStyles;
    btnUnderline.Down := tsUnderline in FEmrView.Style.TextStyles[ANewStyleNo].FontStyles;
    btnStrikeOut.Down := tsStrikeOut in FEmrView.Style.TextStyles[ANewStyleNo].FontStyles;
    btnSuperscript.Down := tsSuperscript in FEmrView.Style.TextStyles[ANewStyleNo].FontStyles;
    btnSubscript.Down := tsSubscript in FEmrView.Style.TextStyles[ANewStyleNo].FontStyles;
  end
  else
  begin
    btnBold.Down := False;
    btnItalic.Down := False;
    btnUnderline.Down := False;
    btnStrikeOut.Down := False;
    btnSuperscript.Down := False;
    btnSubscript.Down := False;
  end;
end;

procedure TfrmRecord.DoSetActiveDeItemExtra(const AStream: TStream);
begin
  FEmrView.SetActiveItemExtra(AStream);
end;

procedure TfrmRecord.DoSetActiveDeItemText(const AText: string);
begin
  FEmrView.SetActiveItemText(AText);
  //FEmrView.ActiveSection.ReFormatActiveItem;
end;

procedure TfrmRecord.DoCanNotEdit(Sender: TObject);
begin
  ShowMessage('当前编辑处只读、不可编辑！');
end;

procedure TfrmRecord.DoCaretChange(Sender: TObject);
begin
  GetPagesAndActive;

  CurTextStyleChange(FEmrView.CurStyleNo);
  CurParaStyleChange(FEmrView.CurParaNo);
end;

procedure TfrmRecord.DoChangedSwitch(Sender: TObject);
begin
  if Assigned(FOnChangedSwitch) then
    FOnChangedSwitch(Self);
end;

procedure TfrmRecord.DoItemInsert(const Sender: TObject; const AData: THCCustomData;
  const AItem: THCCustomItem);
begin
  if Assigned(FOnDeItemInsert) then
    FOnDeItemInsert(FEmrView, Sender as THCSection, AData, AItem);
end;

procedure TfrmRecord.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PopupFormClose;
end;

procedure TfrmRecord.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vActiveItem: THCCustomItem;
  vDeItem: TDeItem;
  vDeGroup: TDeGroup;
  vDeEdit: TDeEdit;
  vDeCombobox: TDeCombobox;
  vDeDateTimePicker: TDeDateTimePicker;
  vDeRadioGroup: TDeRadioGroup;
  vActiveDrawItem: THCCustomDrawItem;
  vPt: TPoint;
  vDrawItemRect: TRect;
  vInfo: string;
  vTopData: THCRichData;
begin
  vInfo := '';

  vActiveItem := FEmrView.GetTopLevelItem;
  if vActiveItem <> nil then
  begin
    if FEmrView.ActiveSection.ActiveData.ActiveDomain.BeginNo >= 0 then
    begin
      vDeGroup := FEmrView.ActiveSection.ActiveData.Items[
        FEmrView.ActiveSection.ActiveData.ActiveDomain.BeginNo] as TDeGroup;
      vInfo := vDeGroup[TDeProp.Name];
    end;

    if vActiveItem is TDeItem then
    begin
      vDeItem := vActiveItem as TDeItem;
      if vDeItem.StyleEx <> cseNone  then
        vInfo := vInfo + '-' + vDeItem.GetHint
      else
      if vDeItem.Active
        and (vDeItem[TDeProp.Index] <> '')
        and (not vDeItem.IsSelectComplate)
        and (not vDeItem.IsSelectPart)
      then
      begin
        if ClientCache.FindDataElementByIndex(vDeItem[TDeProp.Index]) then
          vInfo := vInfo + '-' + ClientCache.DataElementDT.FieldByName('dename').AsString + '(' + vDeItem[TDeProp.Index] + ')'
        else
          vInfo := vInfo + '-[缺少Index]';

        if FEmrView.ActiveSection.ActiveData.ReadOnly then Exit;

        vPt := FEmrView.GetActiveDrawItemClientCoord;
        vActiveDrawItem := FEmrView.GetTopLevelDrawItem;
        vDrawItemRect := vActiveDrawItem.Rect;
        vDrawItemRect := Bounds(vPt.X, vPt.Y, vDrawItemRect.Width, vDrawItemRect.Height);

        if PtInRect(vDrawItemRect, Point(X, Y)) then
        begin
          vPt.Y := vPt.Y + FEmrView.ZoomIn(vActiveDrawItem.Height);
          vPt.Offset(FEmrView.Left, FEmrView.Top);
          //PopupForm.Left := vPt.X + FEmrView.Left;
          //PopupForm.Top := vPt.Y + FEmrView.Top;
          vPt := ClientToScreen(vPt);

          PopupForm.PopupDeItem(vDeItem, vPt);
        end;
      end;
    end
    else
    if vActiveItem is TDeEdit then
    begin
      vDeEdit := vActiveItem as TDeEdit;
      if ClientCache.FindDataElementByIndex(vDeEdit[TDeProp.Index]) then
        vInfo := vInfo + '-' + vDeEdit[TDeProp.Name] + '(' + vDeEdit[TDeProp.Index] + ')'
      else
        vInfo := vInfo + '-[缺少Index]';
    end
    else
    if vActiveItem is TDeCombobox then
    begin
      vDeCombobox := vActiveItem as TDeCombobox;
      if ClientCache.FindDataElementByIndex(vDeCombobox[TDeProp.Index]) then
        vInfo := vInfo + '-' + vDeCombobox[TDeProp.Name] + '(' + vDeCombobox[TDeProp.Index] + ')'
      else
        vInfo := vInfo + '-[缺少Index]';
    end
    else
    if vActiveItem is TDeDateTimePicker then
    begin
      vDeDateTimePicker := vActiveItem as TDeDateTimePicker;
      if ClientCache.FindDataElementByIndex(vDeDateTimePicker[TDeProp.Index]) then
        vInfo := vInfo + '-' + vDeDateTimePicker[TDeProp.Name] + '(' + vDeDateTimePicker[TDeProp.Index] + ')'
      else
        vInfo := vInfo + '-[缺少Index]';
    end;
  end;

  sbStatus.Panels[1].Text := vInfo;
end;

procedure TfrmRecord.DoPaintPaperBefor(const Sender: TObject;
  const APageIndex: Integer; const ARect: TRect; const ACanvas: TCanvas;
  const APaintInfo: TSectionPaintInfo);
begin
  if (not APaintInfo.Print) and THCSection(Sender).ReadOnly then
  begin
    ACanvas.Font.Size := 48;
    ACanvas.Font.Color := clMedGray;
    ACanvas.Font.Name := '隶书';
    ACanvas.TextOut(ARect.Left + 10, ARect.Top + 10, '只读');
  end;
end;

procedure TfrmRecord.DoReadOnlySwitch(Sender: TObject);
begin
  if Assigned(FOnReadOnlySwitch) then
    FOnReadOnlySwitch(Self);
end;

procedure TfrmRecord.DoSave;
begin
  if Assigned(FOnSave) and (not FEmrView.ReadOnly) then
    FOnSave(Self);
end;

procedure TfrmRecord.DoVerScroll(Sender: TObject);
begin
  GetPagesAndActive;
  PopupFormClose;
end;

procedure TfrmRecord.FormCreate(Sender: TObject);
begin
  cbbFont.Items := Screen.Fonts;
  cbbFont.ItemIndex := cbbFont.Items.IndexOf('宋体');
  FDeGroupStack := TStack<Integer>.Create;

  FEmrView := TEmrView.Create(Self);
  FEmrView.OnSectionItemInsert := DoItemInsert;
  FEmrView.OnMouseDown := DoMouseDown;
  FEmrView.OnMouseUp := DoMouseUp;
  FEmrView.OnCaretChange := DoCaretChange;
  FEmrView.OnVerScroll := DoVerScroll;
  FEmrView.OnChangedSwitch := DoChangedSwitch;
  FEmrView.OnSectionReadOnlySwitch := DoReadOnlySwitch;
  FEmrView.OnCanNotEdit := DoCanNotEdit;
  FEmrView.OnSectionPaintPaperBefor := DoPaintPaperBefor;
  FEmrView.PopupMenu := pmView;
  FEmrView.Parent := Self;
  FEmrView.Align := alClient;
  //FEmrView.ShowHint := True; 开启后点击元素弹出框会因显示Hint自动消失
end;

procedure TfrmRecord.FormDestroy(Sender: TObject);
begin
  if Assigned(FfrmRecordPop) then
    FreeAndNil(FfrmRecordPop);
  FEmrView.Free;
  FDeGroupStack.Free;
end;

procedure TfrmRecord.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = Ord('S')) then
    actSaveExecute(Sender);
end;

procedure TfrmRecord.GetPagesAndActive;
begin
  sbStatus.Panels[0].Text := '预览' + IntToStr(FEmrView.PagePreviewFirst + 1)
    + '页 光标' + IntToStr(FEmrView.ActivePageIndex + 1)
    + '页 共' + IntToStr(FEmrView.PageCount) + '页';
end;

procedure TfrmRecord.HideToolbar;
begin
  tlbTool.Visible := False;
end;

procedure TfrmRecord.InsertDataElementAsDE(const AIndex, AName: string);
var
  vDeItem: TDeItem;
begin
  vDeItem := FEmrView.NewDeItem(AName);
  vDeItem[TDeProp.Index] := AIndex;
  vDeItem[TDeProp.Name] := AName;
  FEmrView.InsertDeItem(vDeItem);
end;

procedure TfrmRecord.mniOpenClick(Sender: TObject);
var
  vOpenDlg: TOpenDialog;
begin
  if FEmrView.ReadOnly then Exit;

  vOpenDlg := TOpenDialog.Create(Self);
  try
    vOpenDlg.Filter := '文件|*' + HC_EXT;
    if vOpenDlg.Execute then
    begin
      if vOpenDlg.FileName <> '' then
        FEmrView.LoadFromFile(vOpenDlg.FileName);
    end;
  finally
    FreeAndNil(vOpenDlg);
  end;
end;

procedure TfrmRecord.mniPrintByLineClick(Sender: TObject);
begin
  FEmrView.PrintCurPageByActiveLine(False, False);
end;

procedure TfrmRecord.mniSaveAsClick(Sender: TObject);
var
  vSaveDlg: TSaveDialog;
  vFileName: string;
begin
  vSaveDlg := TSaveDialog.Create(nil);
  try
    vSaveDlg.Filter := '文件|*' + HC_EXT;
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

procedure TfrmRecord.mniSplitColClick(Sender: TObject);
begin
  FEmrView.ActiveTableSplitCurCol;
end;

procedure TfrmRecord.mniSplitRowClick(Sender: TObject);
begin
  FEmrView.ActiveTableSplitCurRow;
end;

procedure TfrmRecord.mniTablePropertyClick(Sender: TObject);
var
  vFrmDeTableProperty: TFrmDeTableProperty;
begin
  vFrmDeTableProperty := TFrmDeTableProperty.Create(Self);
  try
    vFrmDeTableProperty.SetHCView(FEmrView);
  finally
    FreeAndNil(vFrmDeTableProperty);
  end;
end;

procedure TfrmRecord.N1Click(Sender: TObject);
begin
  FEmrView.InsertSectionBreak;
end;

procedure TfrmRecord.pmViewPopup(Sender: TObject);
var
  vActiveItem, vTopItem: THCCustomItem;
  vTable: TDeTable;
  vActiveData, vTopData: THCCustomData;
begin
  vActiveData := FEmrView.ActiveSection.ActiveData;
  vActiveItem := vActiveData.GetActiveItem;

  vTopData := nil;
  vTopItem := vActiveItem;

  while vTopItem is THCCustomRectItem do
  begin
    if (vTopItem as THCCustomRectItem).GetActiveData <> nil then
    begin
      if vTopData <> nil then
      begin
        vActiveData := vTopData;
        vActiveItem := vTopItem;
      end;

      vTopData := (vTopItem as THCCustomRectItem).GetActiveData;
      vTopItem := vTopData.GetActiveItem;
    end
    else
      Break;
  end;

  if vTopData = nil then
    vTopData := vActiveData;

  mniTable.Enabled := vActiveItem.StyleNo = THCStyle.Table;
  if mniTable.Enabled then
  begin
    vTable := vActiveItem as TDeTable;
    mniInsertRowTop.Enabled := vTable.GetEditCell <> nil;
    mniInsertRowBottom.Enabled := mniInsertRowTop.Enabled;
    mniInsertColLeft.Enabled := mniInsertRowTop.Enabled;
    mniInsertColRight.Enabled := mniInsertRowTop.Enabled;
    mniSplitRow.Enabled := mniInsertRowTop.Enabled;
    mniSplitCol.Enabled := mniInsertRowTop.Enabled;

    mniDeleteCurRow.Enabled := vTable.CurRowCanDelete;
    mniDeleteCurCol.Enabled := vTable.CurColCanDelete;
    mniMerge.Enabled := vTable.SelectedCellCanMerge;
  end;
  actCut.Enabled := (not FEmrView.ActiveSection.ReadOnly) and vTopData.SelectExists;
  actCopy.Enabled := actCut.Enabled;
  actPaste.Enabled := (not (vTopData as THCRichData).ReadOnly)  // 非只读
    and (Clipboard.HasFormat(HC_FILEFORMAT)
         or Clipboard.HasFormat(CF_TEXT)
         or Clipboard.HasFormat(CF_BITMAP));
  mniControlItem.Visible := (not (vTopData as THCRichData).ReadOnly) and (not vTopData.SelectExists)
    and (vTopItem is THCControlItem) and vTopItem.Active;
  if mniControlItem.Visible then
    mniControlItem.Caption := '属性(' + (vTopItem as THCControlItem).ClassName + ')';

  mniDeItem.Visible := False;
  mniDeleteProtect.Visible := False;

  if vTopItem is TDeItem then
  begin
    if (vTopItem as TDeItem).IsElement then
    begin
      mniDeItem.Visible := True;
      mniDeItem.Caption := (vTopItem as TDeItem)[TDeProp.Name];
    end;

    if FEmrView.DesignMode then  // 设计模式
    begin
      if vTopData.SelectExists then
      begin
        mniDeleteProtect.Caption := '只读';
        mniDeleteProtect.Visible := True;
      end
      else
      if (vTopItem as TDeItem).EditProtect then
      begin
        mniDeleteProtect.Caption := '取消只读';
        mniDeleteProtect.Visible := True;
      end;
    end;
  end;

  if (vTopData as THCViewData).ActiveDomain.BeginNo >= 0 then
  begin
    mniDeGroup.Visible := True;
    mniDeGroup.Caption := (vTopData.Items[(vTopData as THCViewData).ActiveDomain.BeginNo] as TDeGroup)[TDeProp.Name];
  end
  else
    mniDeGroup.Visible := False;

  mniSplit.Visible := mniControlItem.Visible or mniDeItem.Visible or mniDeGroup.Visible;
end;

function TfrmRecord.PopupForm: TfrmRecordPop;
begin
  if not Assigned(FfrmRecordPop) then
  begin
    FfrmRecordPop := TfrmRecordPop.Create(nil);
    FfrmRecordPop.OnSetActiveItemText := DoSetActiveDeItemText;
    FfrmRecordPop.OnSetActiveItemExtra := DoSetActiveDeItemExtra;
    //FfrmRecordPop.Parent := Self;
  end;

  Result := FfrmRecordPop;
end;

procedure TfrmRecord.PopupFormClose;
begin
  if Assigned(FfrmRecordPop) and FfrmRecordPop.Visible then  // 使用PopupForm会导致没有FfrmRecordPop时创建一次再关闭，无意义
    FfrmRecordPop.Close;
end;

procedure TfrmRecord.mniComboboxClick(Sender: TObject);
var
  vCombobox: TDeCombobox;
  vS: string;
begin
  vS := '默认值';
  if InputQuery('下拉框', '文本内容', vS) then
  begin
    vCombobox := TDeCombobox.Create(FEmrView.ActiveSectionTopLevelData, vS);
    vCombobox.SaveItem := False;  // 下拉选项不存储到文档中
    vCombobox[TDeProp.Index] := '1002';  // 控件的数据元属性
    {vCombobox.Items.Add('选项1');
    vCombobox.Items.Add('选项2');
    vCombobox.Items.Add('选项3');}
    //vCombobox.ItemIndex := 0;
    FEmrView.InsertItem(vCombobox);
  end;
end;

procedure TfrmRecord.mniControlItemClick(Sender: TObject);
var
  vFrmDeControlProperty: TfrmDeControlProperty;
begin
  vFrmDeControlProperty := TfrmDeControlProperty.Create(nil);
  try
    vFrmDeControlProperty.SetHCView(FEmrView);
  finally
    FreeAndNil(vFrmDeControlProperty);
  end;
end;

procedure TfrmRecord.mniDeleteCurColClick(Sender: TObject);
begin
  FEmrView.ActiveTableDeleteCurCol;
end;

procedure TfrmRecord.mniDeleteGroupClick(Sender: TObject);
begin
  FEmrView.DeleteActiveDomain;
end;

procedure TfrmRecord.mniDeleteCurRowClick(Sender: TObject);
begin
  FEmrView.ActiveTableDeleteCurRow;
end;

procedure TfrmRecord.mniDisBorderClick(Sender: TObject);
var
  vTable: TDeTable;
begin
  if FEmrView.ActiveSection.ActiveData.GetActiveItem is TDeTable then
  begin
    vTable := FEmrView.ActiveSection.ActiveData.GetActiveItem as TDeTable;
    vTable.BorderVisible := not vTable.BorderVisible;
    FEmrView.UpdateView;
  end;
end;

procedure TfrmRecord.mniInsertGifClick(Sender: TObject);
var
  vOpenDlg: TOpenDialog;
  vGifItem: THCGifItem;
begin
  vOpenDlg := TOpenDialog.Create(Self);
  try
    vOpenDlg.Filter := '图像文件|*.gif';
    if vOpenDlg.Execute then
    begin
      if vOpenDlg.FileName <> '' then
      begin
        vGifItem := THCGifItem.Create(FEmrView.ActiveSectionTopLevelData);
        vGifItem.LoadFromFile(vOpenDlg.FileName);
        Application.ProcessMessages;  // 解决双击打开文件后，触发下层控件的Mousemove，Mouseup事件
        FEmrView.InsertItem(vGifItem);
      end;
    end;
  finally
    FreeAndNil(vOpenDlg);
  end;
end;

procedure TfrmRecord.mniInsertColLeftClick(Sender: TObject);
begin
  FEmrView.ActiveTableInsertColBefor(1);
end;

procedure TfrmRecord.mniInsertColRightClick(Sender: TObject);
begin
  FEmrView.ActiveTableInsertColAfter(1);
end;

procedure TfrmRecord.mniInsertRowBottomClick(Sender: TObject);
begin
  FEmrView.ActiveTableInsertRowAfter(1);
end;

procedure TfrmRecord.mniInsertRowTopClick(Sender: TObject);
begin
  FEmrView.ActiveTableInsertRowBefor(1);
end;

procedure TfrmRecord.mniLineSpace100Click(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    case (Sender as TMenuItem).Tag of
      0: FEmrView.ApplyParaLineSpace(TParaLineSpaceMode.pls100);
      1: FEmrView.ApplyParaLineSpace(TParaLineSpaceMode.pls115);
      2: FEmrView.ApplyParaLineSpace(TParaLineSpaceMode.pls150);
      3: FEmrView.ApplyParaLineSpace(TParaLineSpaceMode.pls200);
    end;
  end;
end;

procedure TfrmRecord.mniN10Click(Sender: TObject);
begin
  FEmrView.InsertPageBreak;
end;

procedure TfrmRecord.mniN11Click(Sender: TObject);
var
  vFrmDeProperty: TfrmDeProperty;
begin
  vFrmDeProperty := TfrmDeProperty.Create(nil);
  try
    vFrmDeProperty.SetHCView(FEmrView);
  finally
    FreeAndNil(vFrmDeProperty);
  end;
end;

procedure TfrmRecord.mniN12Click(Sender: TObject);
var
  vTopData: THCRichData;
begin
  vTopData := FEmrView.ActiveSectionTopLevelData as THCRichData;
  FEmrView.DeleteActiveDataItems(vTopData.SelectInfo.StartItemNo);
end;

procedure TfrmRecord.mniN16Click(Sender: TObject);
var
  vToothItem: TEmrToothItem;
begin
  vToothItem := TEmrToothItem.Create(FEmrView.ActiveSectionTopLevelData,
    'XX', 'XX', 'XX', 'XX');
  FEmrView.InsertItem(vToothItem);
end;

procedure TfrmRecord.mniEditItemClick(Sender: TObject);
var
  vEdit: TDeEdit;
  vS: string;
begin
  vS := InputBox('文本框', '文本', '');
  vEdit := TDeEdit.Create(FEmrView.ActiveSectionTopLevelData, vS);
  FEmrView.InsertItem(vEdit);
end;

procedure TfrmRecord.mniN18Click(Sender: TObject);
var
  vTopData, vPage: THCViewData;
  vDomain: THCDomainInfo;
  vText: string;
begin
  vTopData := FEmrView.ActiveSectionTopLevelData as THCViewData;
  vDomain := vTopData.ActiveDomain;
  vPage := FEmrView.ActiveSection.Page;

  if vTopData = vPage then
    vText := FEmrView.GetDataForwardDeGroupText(vPage, vDomain.BeginNo)
  else  {to do: 取表格中的域内容}
    vText := '';

  if vText <> '' then
  begin
    FEmrView.SetDataDeGroupText(vTopData, vDomain.BeginNo, vText);
    FEmrView.FormatSection(FEmrView.ActiveSectionIndex);
  end;
end;

procedure TfrmRecord.mniMergeClick(Sender: TObject);
begin
  FEmrView.MergeTableSelectCells;
end;

procedure TfrmRecord.mniN2Click(Sender: TObject);
begin
  FEmrView.Clear;
end;

procedure TfrmRecord.mniN3Click(Sender: TObject);
var
  vFangJiaoItem: TEmrFangJiaoItem;
begin
  vFangJiaoItem := TEmrFangJiaoItem.Create(FEmrView.ActiveSectionTopLevelData, '', '', '', '');
  FEmrView.InsertItem(vFangJiaoItem);
end;

procedure TfrmRecord.mniN5Click(Sender: TObject);
var
  vFrmBorderBackColor: TfrmBorderBackColor;
begin
  vFrmBorderBackColor := TfrmBorderBackColor.Create(Self);
  try
    vFrmBorderBackColor.SetView(FEmrView);
  finally
    FreeAndNil(vFrmBorderBackColor);
  end;
end;

procedure TfrmRecord.mniDeleteProtectClick(Sender: TObject);
var
  vTopData: THCCustomData;
  vDeItem: TDeItem;
  vS: string;
begin
  vTopData := FEmrView.ActiveSectionTopLevelData;

  if vTopData.SelectExists then
  begin
    vS := FEmrView.ActiveSectionTopLevelData.GetSelectText;
    vS := StringReplace(vS, #13#10, '', [rfReplaceAll, rfIgnoreCase]);
    vDeItem := FEmrView.NewDeItem(vS);
    vDeItem.EditProtect := True;
    FEmrView.InsertDeItem(vDeItem);
  end
  else
  begin
    vDeItem := vTopData.GetActiveItem as TDeItem;
    if vDeItem.EditProtect then
    begin
      vDeItem.EditProtect := False;
      FEmrView.ReAdaptActiveItem;
    end;
  end;
end;

procedure TfrmRecord.mniInsertTableClick(Sender: TObject);
var
  vFrmInsertTable: TfrmInsertTable;
  vTable: TDeTable;
  vTopData: THCRichData;
begin
  vFrmInsertTable := TfrmInsertTable.Create(Self);
  try
    vFrmInsertTable.ShowModal;
    if vFrmInsertTable.ModalResult = mrOk then
    begin
      vTopData := FEmrView.ActiveSectionTopLevelData as THCRichData;
      vTable := TDeTable.Create(vTopData, StrToInt(vFrmInsertTable.edtRows.Text),
        StrToInt(vFrmInsertTable.edtCols.Text), vTopData.Width);
      FEmrView.InsertItem(vTable);
    end;
  finally
    FreeAndNil(vFrmInsertTable);
  end;
end;

procedure TfrmRecord.mniCheckboxClick(Sender: TObject);
var
  vCheckBox: TDeCheckBox;
  vS: string;
begin
  vS := InputBox('勾选框', '文本', '');
  vCheckBox := TDeCheckBox.Create(FEmrView.ActiveSectionTopLevelData, vS, False);
  FEmrView.InsertItem(vCheckBox);
end;

procedure TfrmRecord.mniInsertImageClick(Sender: TObject);
var
  vOpenDlg: TOpenDialog;
  vImageItem: THCImageItem;
begin
  vOpenDlg := TOpenDialog.Create(Self);
  try
    vOpenDlg.Filter := 'bmp文件|*.bmp';
    if vOpenDlg.Execute then
    begin
      if vOpenDlg.FileName <> '' then
      begin
        vImageItem := THCImageItem.Create(FEmrView.ActiveSectionTopLevelData);
        vImageItem.LoadFromBmpFile(vOpenDlg.FileName);
        Application.ProcessMessages;  // 解决双击打开文件后，触发下层控件的Mousemove，Mouseup事件
        FEmrView.InsertItem(vImageItem);
      end;
    end;
  finally
    FreeAndNil(vOpenDlg);
  end;
end;

procedure TfrmRecord.mniN8Click(Sender: TObject);
var
  vExpressItem: THCExpressItem;
begin
  vExpressItem := THCExpressItem.Create(FEmrView.ActiveSectionTopLevelData,
    '12', '5-6', FormatDateTime('YYYY-MM-DD', Now), '28-30');
  FEmrView.InsertItem(vExpressItem);
end;

procedure TfrmRecord.mniInsertLineClick(Sender: TObject);
begin
  FEmrView.InsertLine(1);
end;

procedure TfrmRecord.mniPageSetClick(Sender: TObject);
var
  vFrmPageSet: TFrmPageSet;
begin
  vFrmPageSet := TFrmPageSet.Create(Self);
  try
    vFrmPageSet.cbbPaper.ItemIndex := vFrmPageSet.cbbPaper.Items.IndexOf(GetPaperSizeStr(FEmrView.ActiveSection.PaperSize));
    if vFrmPageSet.cbbPaper.ItemIndex < 0 then
      vFrmPageSet.cbbPaper.ItemIndex := 0;
    vFrmPageSet.edtWidth.Text := FloatToStr(FEmrView.ActiveSection.PaperWidth);
    vFrmPageSet.edtHeight.Text := FloatToStr(FEmrView.ActiveSection.PaperHeight);

    vFrmPageSet.edtTop.Text := FloatToStr(FEmrView.ActiveSection.PaperMarginTop);
    vFrmPageSet.edtLeft.Text := FloatToStr(FEmrView.ActiveSection.PaperMarginLeft);
    vFrmPageSet.edtRight.Text := FloatToStr(FEmrView.ActiveSection.PaperMarginRight);
    vFrmPageSet.edtBottom.Text := FloatToStr(FEmrView.ActiveSection.PaperMarginBottom);
    vFrmPageSet.chkShowLineNo.Checked := FEmrView.ShowLineNo;
    vFrmPageSet.chkShowLineActiveMark.Checked := FEmrView.ShowLineActiveMark;
    vFrmPageSet.chkShowUnderLine.Checked := FEmrView.ShowUnderLine;
    vFrmPageSet.ShowModal;
    if vFrmPageSet.ModalResult = mrOk then
    begin
      FEmrView.ActiveSection.PaperSize := DMPAPER_A4;
      FEmrView.ActiveSection.PaperWidth := StrToFloat(vFrmPageSet.edtWidth.Text);
      FEmrView.ActiveSection.PaperHeight := StrToFloat(vFrmPageSet.edtHeight.Text);

      FEmrView.ActiveSection.PaperMarginTop := StrToFloat(vFrmPageSet.edtTop.Text);
      FEmrView.ActiveSection.PaperMarginLeft := StrToFloat(vFrmPageSet.edtLeft.Text);
      FEmrView.ActiveSection.PaperMarginRight := StrToFloat(vFrmPageSet.edtRight.Text);
      FEmrView.ActiveSection.PaperMarginBottom := StrToFloat(vFrmPageSet.edtBottom.Text);
      FEmrView.ShowLineNo := vFrmPageSet.chkShowLineNo.Checked;
      FEmrView.ShowLineActiveMark := vFrmPageSet.chkShowLineActiveMark.Checked;
      FEmrView.ShowUnderLine := vFrmPageSet.chkShowUnderLine.Checked;
      FEmrView.ResetActiveSectionMargin;
    end;
  finally
    FreeAndNil(vFrmPageSet);
  end;
end;

procedure TfrmRecord.mniParaClick(Sender: TObject);
var
  vFrmParagraph: TfrmParagraph;
begin
  vFrmParagraph := TfrmParagraph.Create(Self);
  try
    //vFrmParagraph.edtLineSpace.Text := IntToStr(FEmrView.Style.ParaStyles[FEmrView.Style.CurParaNo].LineSpace);
    vFrmParagraph.cbbAlignHorz.ItemIndex := Ord(FEmrView.Style.ParaStyles[FEmrView.CurParaNo].AlignHorz);
    vFrmParagraph.cbbAlignVert.ItemIndex := Ord(FEmrView.Style.ParaStyles[FEmrView.CurParaNo].AlignVert);
    vFrmParagraph.clrbxBG.Color := FEmrView.Style.ParaStyles[FEmrView.CurParaNo].BackColor;

    vFrmParagraph.ShowModal;
    if vFrmParagraph.ModalResult = mrOk then
    begin
      FEmrView.BeginUpdate;
      try
        //FEmrView.ApplyParaLineSpace(StrToIntDef(vFrmParagraph.edtLineSpace.Text, 8));
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

end.
