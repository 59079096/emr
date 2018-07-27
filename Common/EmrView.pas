{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit EmrView;

interface

uses
  Windows, Classes, Controls, Graphics, HCView, HCStyle, HCItem, HCTextItem,
  HCDrawItem, HCCustomData, HCCustomRichData, HCRichData, HCSectionData, EmrElementItem,
  HCCommon, HCRectItem, EmrGroupItem, System.Generics.Collections;

type
  TEmrState = (cesLoading, cesTrace);
  TEmrStates = set of TEmrState;

  TEmrView = class(THCView)
  private
    FStates: TEmrStates;
    procedure DoSectionCreateItem(Sender: TObject);  // Sender为TDeItem
    procedure InsertEmrTraceItem(const AText: string);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function DoInsertText(const AText: string): Boolean; override;
    procedure DoSaveBefor(const AStream: TStream); override;
    procedure DoSaveAfter(const AStream: TStream); override;
    procedure DoLoadBefor(const AStream: TStream; const AFileVersion: Word); override;
    procedure DoLoadAfter(const AStream: TStream; const AFileVersion: Word); override;
    procedure DoSectionItemPaintAfter(const AData: THCCustomData;
      const ADrawItemIndex: Integer; const ADrawRect: TRect;
      const ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    procedure DoUpdateViewBefor(const ACanvas: TCanvas);
    procedure DoUpdateViewAfter(const ACanvas: TCanvas);
//    procedure DoSectionDrawItemPaintAfter(const AData: THCCustomData;
//      const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
//      ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
//      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure LoadFromStream(const AStream: TStream); override;
    //
    function GetTrace: Boolean;
    procedure SetTrace(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetActiveDrawItemCoord: TPoint;

    /// <summary> 从当前行打印当前页 </summary>
    /// <param name="APrintHeader"></param>
    /// <param name="APrintFooter"></param>
    procedure PrintCurPageByActiveLine(const APrintHeader, APrintFooter: Boolean);

    procedure TraverseItem(const ATraverse: TItemTraverse);
    function InsertDeGroup(const ADeGroup: TDeGroup): Boolean;
    function InsertDeItem(const ADeItem: TDeItem): Boolean;
    function NewDeItem(const AText: string): TDeItem;

    /// <summary> 取指定域中的文本内容 </summary>
    function GetDataDomainText(const AData: THCRichData;
      const ADomainStartNo, ADomainEndNo: Integer): string;

    /// <summary> 从当前域起始位置往前找同Index域内容 </summary>
    function GetDataForwardDomainText(const AData: THCRichData;
      const ADomainStartNo: Integer): string;

    /// <summary> 替换指定域的内容 </summary>
    procedure SetDataDomainText(const AData: THCRichData;
      const ADomainStartNo: Integer; const AText: string);

    property Trace: Boolean read GetTrace write SetTrace;
  published
    property Align;
  end;

procedure Register;

implementation

uses
  SysUtils, Forms, Printers, HCTextStyle, HCSection;

procedure Register;
begin
  RegisterComponents('HCEmrViewVCL', [TEmrView]);
end;

{ TEmrView }

constructor TEmrView.Create(AOwner: TComponent);
begin
  HCDefaultTextItemClass := TDeItem;
  HCDefaultDomainItemClass := TDeGroup;
  inherited Create(AOwner);
  Self.Width := 100;
  Self.Height := 100;
  Self.OnSectionCreateItem := DoSectionCreateItem;
  Self.OnUpdateViewBefor := DoUpdateViewBefor;
  Self.OnUpdateViewAfter := DoUpdateViewAfter;
end;

destructor TEmrView.Destroy;
begin
  inherited Destroy;
end;

procedure TEmrView.DoSectionCreateItem(Sender: TObject);
begin
  if (not (cesLoading in FStates)) and (cesTrace in FStates) then
    (Sender as TDeItem).StyleEx := TStyleExtra.cseAdd;
end;

function TEmrView.DoInsertText(const AText: string): Boolean;
begin
  Result := False;
  if cesTrace in FStates then
  begin
    InsertEmrTraceItem(AText);
    Result := True;
  end
  else
    Result := inherited DoInsertText(AText);
end;

procedure TEmrView.DoLoadAfter(const AStream: TStream; const AFileVersion: Word);
begin
  inherited DoLoadAfter(AStream, AFileVersion);;
end;

procedure TEmrView.DoLoadBefor(const AStream: TStream; const AFileVersion: Word);
begin
  inherited DoLoadBefor(AStream, AFileVersion);
end;

procedure TEmrView.DoSaveAfter(const AStream: TStream);
begin
  inherited DoSaveAfter(AStream);
end;

procedure TEmrView.DoSaveBefor(const AStream: TStream);
begin
  inherited DoSaveBefor(AStream);
end;

procedure TEmrView.DoSectionItemPaintAfter(const AData: THCCustomData;
  const ADrawItemIndex: Integer; const ADrawRect: TRect; const ADataDrawLeft,
  ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vItem: THCCustomItem;
  vDeItem: TDeItem;
begin
  if Self.ShowAnnotation then  // 显示批注
  begin
    vItem := AData.Items[AData.DrawItems[ADrawItemIndex].ItemNo];
    if vItem.StyleNo > THCStyle.RsNull then
    begin
      vDeItem := vItem as TDeItem;
      if (vDeItem.StyleEx <> TStyleExtra.cseNone)
        and (vDeItem.FirstDItemNo = ADrawItemIndex)
      then  // 添加批注
      begin
        if vDeItem.StyleEx = TStyleExtra.cseDel then
          Self.Annotates.AddAnnotation(ADrawRect, vDeItem.Text + sLineBreak + vDeItem[TDeProp.Trace])
        else
          Self.Annotates.AddAnnotation(ADrawRect, vDeItem.Text + sLineBreak + vDeItem[TDeProp.Trace]);
      end;
    end;
  end;

  inherited DoSectionItemPaintAfter(AData, ADrawItemIndex, ADrawRect, ADataDrawLeft,
    ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
end;

procedure TEmrView.DoUpdateViewAfter(const ACanvas: TCanvas);
{var
  i: Integer;
  vRect: TRect;
  vS: string;}
begin
  {ACanvas.Brush.Color := clInfoBk;
  for i := 0 to FAreas.Count - 1 do
  begin
    vRect := FAreas[i].Rect;
    ACanvas.FillRect(vRect);
    ACanvas.Pen.Color := clBlack;
    ACanvas.Rectangle(vRect);

    ACanvas.Pen.Color := clMedGray;
    ACanvas.MoveTo(vRect.Left + 2, vRect.Bottom + 1);
    ACanvas.LineTo(vRect.Right, vRect.Bottom + 1);
    ACanvas.LineTo(vRect.Right, vRect.Top + 2);

    vS := '引用';
    ACanvas.TextRect(vRect, vS, [tfSingleLine, tfCenter, tfVerticalCenter]);
  end;}
end;

procedure TEmrView.DoUpdateViewBefor(const ACanvas: TCanvas);
begin
  //FAreas.Clear;
end;

function TEmrView.GetActiveDrawItemCoord: TPoint;
var
  vPageIndex: Integer;
begin
  Result := ActiveSection.GetActiveDrawItemCoord;
  vPageIndex := ActiveSection.ActivePageIndex;

  // 映射到节页面(白色区域)
  Result.X := GetSectionDrawLeft(Self.ActiveSectionIndex)
    + ZoomIn(ActiveSection.GetPageMarginLeft(vPageIndex) + Result.X) - Self.HScrollValue;

  if ActiveSection.ActiveData = ActiveSection.Header then
    Result.Y := ZoomIn(
      GetSectionTopFilm(Self.ActiveSectionIndex)
      + ActiveSection.GetPageTopFilm(vPageIndex)  // 20
      + ActiveSection.GetHeaderPageDrawTop
      + Result.Y
      - ActiveSection.GetPageDataFmtTop(vPageIndex))  // 0
      - Self.VScrollValue
  else
    Result.Y := ZoomIn(
      GetSectionTopFilm(Self.ActiveSectionIndex)
      + ActiveSection.GetPageTopFilm(vPageIndex)  // 20
      + ActiveSection.GetHeaderAreaHeight // 94
      + Result.Y
      - ActiveSection.GetPageDataFmtTop(vPageIndex))  // 0
      - Self.VScrollValue;
end;

function TEmrView.GetDataForwardDomainText(const AData: THCRichData;
  const ADomainStartNo: Integer): string;
var
  i, vBeginNo, vEndNo: Integer;
  vDeGroup: TDeGroup;
  vDeIndex: string;
begin
  Result := '';

  vBeginNo := -1;
  vEndNo := -1;
  vDeIndex := (AData.Items[ADomainStartNo] as TDeGroup)[TDeProp.Index];

  for i := 0 to ADomainStartNo - 1 do  // 找起始
  begin
    if AData.Items[i] is TDeGroup then
    begin
      vDeGroup := AData.Items[i] as TDeGroup;
      if vDeGroup.MarkType = TMarkType.cmtBeg then  // 是域起始
      begin
        if vDeGroup[TDeProp.Index] = vDeIndex then  // 是目标域起始
        begin
          vBeginNo := i;
          Break;
        end;
      end;
    end;
  end;

  if vBeginNo >= 0 then  // 找结束
  begin
    for i := vBeginNo + 1 to ADomainStartNo - 1 do
    begin
      if AData.Items[i] is TDeGroup then
      begin
        vDeGroup := AData.Items[i] as TDeGroup;
        if vDeGroup.MarkType = TMarkType.cmtEnd then  // 是域结束
        begin
          if vDeGroup[TDeProp.Index] = vDeIndex then  // 是目标域结束
          begin
            vEndNo := i;
            Break;
          end;
        end;
      end;
    end;

    if vEndNo > 0 then
      Result := GetDataDomainText(AData, vBeginNo, vEndNo);
  end;
end;

function TEmrView.GetDataDomainText(const AData: THCRichData;
  const ADomainStartNo, ADomainEndNo: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := ADomainStartNo + 1 to ADomainEndNo - 1 do
    Result := Result + AData.Items[i].Text;
end;

function TEmrView.GetTrace: Boolean;
begin
  Result := cesTrace in FStates;
end;

function TEmrView.InsertDeGroup(const ADeGroup: TDeGroup): Boolean;
var
  vGroupItem: TDeGroup;
  vTopData: THCRichData;
  vDomainLevel: Byte;
begin
  Result := False;

  vTopData := Self.ActiveSectionTopData as THCRichData;
  if vTopData <> nil then
  begin
    if vTopData.ActiveDomain <> nil then
    begin
      if (vTopData.Items[vTopData.ActiveDomain.BeginNo] as TDeGroup)[TDeProp.Index] = ADeGroup[TDeProp.Index] then
        Exit;  // 在Index域中不能再插入相同Index的域

      vDomainLevel := (vTopData.Items[vTopData.ActiveDomain.BeginNo] as TDeGroup).Level + 1;
    end
    else
      vDomainLevel := 0;


    Self.BeginUpdate;
    try
      // 头
      vGroupItem := TDeGroup.Create(vTopData);
      vGroupItem.Level := vDomainLevel;
      vGroupItem.Assign(ADeGroup);
      vGroupItem.MarkType := cmtBeg;
      InsertItem(vGroupItem);  // [ 不能使用vTopData直接插入，因其不能触发重新计算页数

      // 尾
      vGroupItem := TDeGroup.Create(vTopData);
      vGroupItem.Level := vDomainLevel;
      vGroupItem.Assign(ADeGroup);
      vGroupItem.MarkType := cmtEnd;
      InsertItem(vGroupItem);  // ]  不能使用vTopData直接插入，因其不能触发重新计算页数

      // 文本内容 先插入[]，再在其中间插入item，防止item和后面的内容合并
      {vInsertIndex := vTopData.SelectInfo.StartItemNo;
      vTextItem := TDeItem.CreateByText(vGroupItem[DEName]);
      vTextItem.StyleNo := Style.CurStyleNo;
      vTextItem.ParaNo := Style.CurParaNo;
      vTopData.InsertItem(vInsertIndex, vTextItem);}
    finally
      Self.EndUpdate;
    end;
  end;
end;

function TEmrView.InsertDeItem(const ADeItem: TDeItem): Boolean;
begin
  Result := Self.InsertItem(ADeItem);
end;

procedure TEmrView.InsertEmrTraceItem(const AText: string);
var
  vEmrTraceItem: TDeItem;
begin
  // 插入添加痕迹元素
  vEmrTraceItem := TDeItem.CreateByText(AText);
  vEmrTraceItem.StyleNo := Style.CurStyleNo;
  vEmrTraceItem.ParaNo := Style.CurParaNo;
  vEmrTraceItem.StyleEx := TStyleExtra.cseAdd;

  Self.InsertItem(vEmrTraceItem);
end;

procedure TEmrView.KeyDown(var Key: Word; Shift: TShiftState);
var
  vData: THCCustomRichData;
  vText, vCurTrace: string;
  vStyleNo, vParaNo: Integer;
  vDeItem: TDeItem;
  vCurItem: THCCustomItem;
  vCurStyleEx: TStyleExtra;
begin
  if cesTrace in FStates then
  begin
    vText := '';
    vCurTrace := '';
    vStyleNo := THCStyle.RsNull;
    vParaNo := THCStyle.RsNull;
    vCurStyleEx := TStyleExtra.cseNone;

    vData := Self.ActiveSection.ActiveData;
    if vData <> nil then
      vData := vData.GetTopLevelData;

    if vData.SelectExists then
    begin
      Self.DisSelect;
      Exit;
    end;

    if vData.SelectInfo.StartItemNo < 0 then Exit;

    if vData.Items[vData.SelectInfo.StartItemNo].StyleNo < THCStyle.RsNull then
    begin
      inherited KeyDown(Key, Shift);
      Exit;
    end;

    // 取光标处的文本
    with vData do
    begin
      if Key = VK_BACK then  // 回删
      begin
        if (SelectInfo.StartItemNo = 0) and (SelectInfo.StartItemOffset = 0) then  // 第一个最前面则不处理
          Exit
        else  // 不是第一个最前面
        if SelectInfo.StartItemOffset = 0 then  // 最前面，移动到前一个最后面处理
        begin
          if Items[SelectInfo.StartItemNo].Text <> '' then  // 当前行不是空行
          begin
            SelectInfo.StartItemNo := SelectInfo.StartItemNo - 1;
            SelectInfo.StartItemOffset := Items[SelectInfo.StartItemNo].Length;
            Self.KeyDown(Key, Shift);
          end
          else  // 空行不留痕直接默认处理
            inherited KeyDown(Key, Shift);

          Exit;
        end
        else  // 不是第一个Item，也不是在Item最前面
        if Items[SelectInfo.StartItemNo] is TDeItem then  // 文本
        begin
          vDeItem := Items[SelectInfo.StartItemNo] as TDeItem;
          vText := vDeItem.GetTextPart(SelectInfo.StartItemOffset, 1);
          vStyleNo := vDeItem.StyleNo;
          vParaNo := vDeItem.ParaNo;
          vCurStyleEx := vDeItem.StyleEx;
          vCurTrace := vDeItem[TDeProp.Trace];
        end;
      end
      else
      if Key = VK_DELETE then  // 后删
      begin
        if (SelectInfo.StartItemNo = Items.Count - 1)
          and (SelectInfo.StartItemOffset = Items[Items.Count - 1].Length)
        then  // 最后一个最后面则不处理
          Exit
        else  // 不是最后一个最后面
        if SelectInfo.StartItemOffset = Items[SelectInfo.StartItemNo].Length then  // 最后面，移动到后一个最前面处理
        begin
          SelectInfo.StartItemNo := SelectInfo.StartItemNo + 1;
          SelectInfo.StartItemOffset := 0;
          Self.KeyDown(Key, Shift);

          Exit;
        end
        else  // 不是最后一个Item，也不是在Item最后面
        if Items[SelectInfo.StartItemNo] is TDeItem then  // 文本
        begin
          vDeItem := Items[SelectInfo.StartItemNo] as TDeItem;
          vText := vDeItem.GetTextPart(SelectInfo.StartItemOffset + 1, 1);
          vStyleNo := vDeItem.StyleNo;
          vParaNo := vDeItem.ParaNo;
          vCurStyleEx := vDeItem.StyleEx;
          vCurTrace := vDeItem[TDeProp.Trace];
        end;
      end;
    end;

    // 删除掉的内容以痕迹的形式插入
    Self.BeginUpdate;
    try
      inherited KeyDown(Key, Shift);

      if (cesTrace in FStates) and (vText <> '') then  // 有删除的内容
      begin
        if (vCurStyleEx = TStyleExtra.cseAdd) and (vCurTrace = '') then Exit;  // 新添加未生效痕迹可以直接删除

        // 创建删除字符对应的Item
        vDeItem := TDeItem.CreateByText(vText);
        vDeItem.StyleNo := vStyleNo;  // Style.CurStyleNo;
        vDeItem.ParaNo := vParaNo;  // Style.CurParaNo;

        if (vCurStyleEx = TStyleExtra.cseDel) and (vCurTrace = '') then  // 原来是删除未生效痕迹
          vDeItem.StyleEx := TStyleExtra.cseNone  // 取消删除痕迹
        else  // 生成删除痕迹
          vDeItem.StyleEx := TStyleExtra.cseDel;

        // 插入删除痕迹Item
        vCurItem := vData.Items[vData.SelectInfo.StartItemNo];
        if (vData.SelectInfo.StartItemOffset = 0) then  // 在Item最前面
        begin
          if vDeItem.CanConcatItems(vCurItem) then // 可以合并
          begin
            vCurItem.Text := vDeItem.Text + vCurItem.Text;

            if Key = VK_DELETE then  // 后删
              vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset + 1;

            Self.ActiveSection.ReFormatActiveItem;
          end
          else  // 不能合并
          begin
            vDeItem.ParaFirst := vCurItem.ParaFirst;
            vCurItem.ParaFirst := False;
            vData.InsertItem(vDeItem);
            if Key = VK_BACK then  // 回删
              vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset - 1;
          end;
        end
        else
        if vData.SelectInfo.StartItemOffset = vCurItem.Length then  // 在Item最后面
        begin
          if vCurItem.CanConcatItems(vDeItem) then // 可以合并
          begin
            vCurItem.Text := vCurItem.Text + vDeItem.Text;

            if Key = VK_DELETE then  // 后删
              vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset + 1;

            Self.ActiveSection.ReFormatActiveItem;
          end
          else  // 不可以合并
          begin
            vData.InsertItem(vDeItem);
            if Key = VK_BACK then  // 回删
              vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset - 1;
          end;
        end
        else  // 在Item中间
        begin
          vData.InsertItem(vDeItem);
          if Key = VK_BACK then  // 回删
            vData.SelectInfo.StartItemOffset := vData.SelectInfo.StartItemOffset - 1;
        end;
      end;
    finally
      Self.EndUpdate;
    end;
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TEmrView.KeyPress(var Key: Char);
var
  vData: THCCustomRichData;
begin
  if cesTrace in FStates then
  begin
    if IsKeyPressWant(Key) then
    begin
      vData := Self.ActiveSectionTopData;

      if vData.SelectInfo.StartItemNo < 0 then Exit;

      if vData.SelectExists then
        Self.DisSelect
      else
        InsertEmrTraceItem(Key);

      Exit;
    end;
  end;
  inherited KeyPress(Key);
end;

procedure TEmrView.LoadFromStream(const AStream: TStream);
begin
  Include(FStates, cesLoading);
  try
    inherited LoadFromStream(AStream);
  finally
    Exclude(FStates, cesLoading);
  end;
end;

function TEmrView.NewDeItem(const AText: string): TDeItem;
begin
  Result := TDeItem.CreateByText(AText);
  if Self.Style.CurStyleNo > THCStyle.RsNull then
    Result.StyleNo := Self.Style.CurStyleNo
  else
    Result.StyleNo := 0;

  Result.ParaNo := Self.Style.CurParaNo;
end;

procedure TEmrView.PrintCurPageByActiveLine(const APrintHeader, APrintFooter: Boolean);
var
  vScaleX, vScaleY: Single;

  {$REGION 'SetPrintBySectionInfo'}
  procedure SetPrintBySectionInfo(const ASectionIndex: Integer);
  var
    vDevice: Array[0..(cchDeviceName - 1)] of Char;
    vDriver: Array[0..(MAX_PATH - 1)] of Char;
    vPort: Array[0..32] of Char;
    vHDMode: THandle;
    vPDMode: PDevMode;
  begin
    Printer.GetPrinter(vDevice, vDriver, vPort, vHDMode);
    if vHDMode <> 0 then
    begin
      // 获取指向DeviceMode的指针
      vPDMode := GlobalLock(vHDMode);
      if vPDMode <> nil then
      begin
        {vOlddmPaperSize := vPDMode^.dmPaperSize;
        vOlddmPaperLength := vPDMode^.dmPaperLength;
        vOlddmPaperWidth := vPDMode^.dmPaperWidth;}
        // 发现常用尺寸和直接设置对应长宽后以B5为例 宽差 0.4cm 故先关了
        vPDMode^.dmPaperSize := Self.Sections[ASectionIndex].PaperSize;
        if vPDMode^.dmPaperSize = DMPAPER_USER then
        begin
          vPDMode^.dmPaperSize := DMPAPER_USER;  // 自定义纸张
          vPDMode^.dmPaperLength := Round(Self.Sections[ASectionIndex].PaperHeight * 10); //纸长你可用变量获得纸张的长、宽。
          vPDMode^.dmPaperWidth := Round(Self.Sections[ASectionIndex].PaperWidth * 10);   //纸宽
          vPDMode^.dmFields := vPDMode^.dmFields or DM_PAPERSIZE or DM_PAPERLENGTH or DM_PAPERWIDTH;
        end
      end;

      ResetDC(Printer.Handle, vPDMode^);
      GlobalUnlock(vHDMode);
      //Printer.SetPrinter(vDevice, vDriver, vPort, vHDMode);
    end;
  end;
  {$ENDREGION}

  procedure ZoomRect(var ARect: TRect);
  begin
    ARect.Left := Round(ARect.Left * vScaleX);
    ARect.Top := Round(ARect.Top * vScaleY);
    ARect.Right := Round(ARect.Right * vScaleX);
    ARect.Bottom := Round(ARect.Bottom * vScaleY);
  end;

var
  vPt: TPoint;
  vPageCanvas: TCanvas;
  vPrintWidth, vPrintHeight, vPrintOffsetX, vPrintOffsetY: Integer;
  vMarginLeft, vMarginRight: Integer;
  vRect: TRect;
  vPaintInfo: TSectionPaintInfo;
begin
  vPrintOffsetX := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);  // 90
  vPrintOffsetY := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);  // 99
  vPrintWidth := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
  vPrintHeight := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
  vScaleX := vPrintWidth / Self.ActiveSection.PageWidthPix;
  vScaleY := vPrintHeight / Self.ActiveSection.PageHeightPix;
  SetPrintBySectionInfo(Self.ActiveSectionIndex);

  Printer.BeginDoc;
  try
    vPaintInfo := TSectionPaintInfo.Create;
    vPaintInfo.Print := True;
    vPaintInfo.SectionIndex := Self.ActiveSectionIndex;
    vPaintInfo.PageIndex := Self.ActiveSection.ActivePageIndex;
    vPaintInfo.ScaleX := vPrintWidth / Self.ActiveSection.PageWidthPix;
    vPaintInfo.ScaleY := vPrintHeight / Self.ActiveSection.PageHeightPix;
    vPaintInfo.WindowWidth := vPrintWidth;  // FSections[vStartSection].PageWidthPix;
    vPaintInfo.WindowHeight := vPrintHeight;  // FSections[vStartSection].PageHeightPix;

    vPageCanvas := TCanvas.Create;
    try
      vPageCanvas.Handle := Printer.Canvas.Handle;  // 为什么不用vPageCanvas中介打印就不行呢？

      Self.ActiveSection.PaintPage(Self.ActiveSection.ActivePageIndex,
        vPrintOffsetX, vPrintOffsetY, vPageCanvas, vPaintInfo);

      if Self.ActiveSection.ActiveData = Self.ActiveSection.PageData then
      begin
        vPt := Self.ActiveSection.GetActiveDrawItemCoord;
        vPt.Y := vPt.Y - ActiveSection.GetPageDataFmtTop(Self.ActiveSection.ActivePageIndex);
      end;
      vPageCanvas.Brush.Color := clRed;
      Self.ActiveSection.GetPageMarginLeftAndRight(Self.ActiveSection.ActivePageIndex, vMarginLeft, vMarginRight);
      if APrintHeader then  // 打印页眉
        vRect := Bounds(vPrintOffsetX + vMarginLeft,
          vPrintOffsetY + Self.ActiveSection.GetHeaderAreaHeight,
          Self.ActiveSection.PageWidthPix - vMarginLeft - vMarginRight, vPt.Y)
      else
        vRect := Bounds(vPrintOffsetX + vMarginLeft, vPrintOffsetY,
          Self.ActiveSection.PageWidthPix - vMarginLeft - vMarginRight,
          Self.ActiveSection.GetHeaderAreaHeight + vPt.Y);

      ZoomRect(vRect);
      vPageCanvas.FillRect(vRect);

      if not APrintFooter then
      begin
        vRect := Bounds(vPrintOffsetX + vMarginLeft,
          vPrintOffsetY + Self.ActiveSection.PageHeightPix - Self.ActiveSection.PageMarginBottomPix,
          Self.ActiveSection.PageWidthPix - vMarginLeft - vMarginRight,
          Self.ActiveSection.PageMarginBottomPix);
        ZoomRect(vRect);
        vPageCanvas.FillRect(vRect);
      end;
    finally
      vPageCanvas.Handle := 0;
      vPageCanvas.Free;
      vPaintInfo.Free;
    end;
  finally
    Printer.EndDoc;
  end;
end;

procedure TEmrView.SetDataDomainText(const AData: THCRichData;
  const ADomainStartNo: Integer; const AText: string);
var
  i, vIgnore, vEndNo: Integer;
begin
  // 找指定的数据组Item范围
  vEndNo := -1;
  vIgnore := 0;

  for i := ADomainStartNo + 1 to AData.Items.Count - 1 do
  begin
    if AData.Items[i] is TDeGroup then
    begin
      if (AData.Items[i] as TDeGroup).MarkType = TMarkType.cmtEnd then
      begin
        if vIgnore = 0 then
        begin
          vEndNo := i;
          Break;
        end
        else
          Dec(vIgnore);
      end
      else
        Inc(vIgnore);
    end;
  end;

  if vEndNo >= 0 then  // 找到了要引用的内容
  begin
    Self.BeginUpdate;
    try
      // 选中，使用插入时删除当前数据组中的内容
      AData.SetSelectBound(ADomainStartNo, OffsetAfter, vEndNo, OffsetBefor);
      AData.InsertText(AText);
    finally
      Self.EndUpdate
    end;
  end;
end;

procedure TEmrView.SetTrace(const Value: Boolean);
begin
  if Value then
    Include(FStates, cesTrace)
  else
    Exclude(FStates, cesTrace);
end;

procedure TEmrView.TraverseItem(const ATraverse: TItemTraverse);
var
  i: Integer;
begin
  for i := 0 to Self.Sections.Count - 1 do
  begin
    with Self.Sections[i] do
    begin
      Header.TraverseItem(ATraverse);
      Footer.TraverseItem(ATraverse);
      PageData.TraverseItem(ATraverse);
    end;
  end;
end;

end.
