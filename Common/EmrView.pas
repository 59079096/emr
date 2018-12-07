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
  Windows, Classes, Controls, Vcl.Graphics, HCView, HCStyle, HCItem, HCTextItem,
  HCDrawItem, HCCustomData, HCCustomRichData, HCViewData, HCSectionData, EmrElementItem,
  HCCommon, HCRectItem, EmrGroupItem, Generics.Collections, Winapi.Messages;

type
  TEmrView = class(THCView)
  private
    FLoading,
    FTrace: Boolean;
    procedure DoSectionCreateItem(Sender: TObject);  // Sender为TDeItem
    procedure InsertEmrTraceItem(const AText: string);
    function DoCreateStyleItem(const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem;
    function DoCanEdit(const Sender: TObject): Boolean;
  protected
    /// <summary> 鼠标按下 </summary>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    /// <summary> 鼠标按压 </summary>
    procedure KeyPress(var Key: Char); override;

    /// <summary> 插入文本 </summary>
    /// <param name="AText">要插入的字符串(支持带#13#10的回车换行)</param>
    /// <returns>True：插入成功</returns>
    function DoInsertText(const AText: string): Boolean; override;

    /// <summary> 开始保存文档 </summary>
    /// <param name="AStream"></param>
    procedure DoSaveBefor(const AStream: TStream); override;

    /// <summary> 文档保存完成 </summary>
    /// <param name="AStream"></param>
    procedure DoSaveAfter(const AStream: TStream); override;

    /// <summary> 开始加载文档 </summary>
    /// <param name="AStream"></param>
    /// <param name="AFileVersion">文件版本号</param>
    procedure DoLoadBefor(const AStream: TStream; const AFileVersion: Word); override;

    /// <summary> 文档加载完成 </summary>
    /// <param name="AStream"></param>
    /// <param name="AFileVersion">文件版本号</param>
    procedure DoLoadAfter(const AStream: TStream; const AFileVersion: Word); override;

    /// <summary> 文档某节的Item绘制完成 </summary>
    /// <param name="AData">当前绘制的Data</param>
    /// <param name="ADrawItemIndex">Item对应的DrawItem序号</param>
    /// <param name="ADrawRect">Item对应的绘制区域</param>
    /// <param name="ADataDrawLeft">Data绘制时的Left</param>
    /// <param name="ADataDrawBottom">Data绘制时的Bottom</param>
    /// <param name="ADataScreenTop">绘制时呈现Data的Top位置</param>
    /// <param name="ADataScreenBottom">绘制时呈现Data的Bottom位置</param>
    /// <param name="ACanvas">画布</param>
    /// <param name="APaintInfo">绘制时的其它信息</param>
    procedure DoSectionDrawItemPaintAfter(const Sender: TObject;
      const AData: THCCustomData; const ADrawItemIndex: Integer; const ADrawRect: TRect;
      const ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;

    /// <summary> 控件绘制文档内容开始 </summary>
    /// <param name="ACanvas">画布</param>
    procedure DoUpdateViewBefor(const ACanvas: TCanvas);

    /// <summary> 控件绘制文档内容结束 </summary>
    /// <param name="ACanvas">画布</param>
    procedure DoUpdateViewAfter(const ACanvas: TCanvas);
//    procedure DoSectionDrawItemPaintAfter(const AData: THCCustomData;
//      const ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
//      ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
//      const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary> 文档保存到流 </summary>
    procedure SaveToStream(const AStream: TStream;
      const ASaveParts: TSaveParts = [saHeader, saPage, saFooter]); override;

    /// <summary> 读取文件流 </summary>
    procedure LoadFromStream(const AStream: TStream); override;

    /// <summary> 遍历Item </summary>
    /// <param name="ATraverse">遍历时信息</param>
    procedure TraverseItem(const ATraverse: TItemTraverse);

    /// <summary> 插入数据组 </summary>
    /// <param name="ADeGroup">数据组信息</param>
    /// <returns>True：插入成功</returns>
    function InsertDeGroup(const ADeGroup: TDeGroup): Boolean;

    /// <summary> 插入数据元 </summary>
    /// <param name="ADeItem">数据元信息</param>
    /// <returns>True：插入成功</returns>
    function InsertDeItem(const ADeItem: TDeItem): Boolean;

    /// <summary> 新建数据元 </summary>
    /// <param name="AText">数据元文本</param>
    /// <returns>新建好的数据元</returns>
    function NewDeItem(const AText: string): TDeItem;

    /// <summary> 获取指定数据组中的文本内容 </summary>
    /// <param name="AData">指定从哪个Data里获取</param>
    /// <param name="ADeGroupStartNo">指定数据组的起始ItemNo</param>
    /// <param name="ADeGroupEndNo">指定数据组的结束ItemNo</param>
    /// <returns>数据组内容</returns>
    function GetDataDeGroupText(const AData: THCViewData;
      const ADeGroupStartNo, ADeGroupEndNo: Integer): string;

    /// <summary> 从当前数据组起始位置往前找相同Index域内容 </summary>
    /// <param name="AData">指定从哪个Data里获取</param>
    /// <param name="ADeGroupStartNo">指定从哪个位置开始往前找</param>
    /// <returns>相同Index的数据组内容</returns>
    function GetDataForwardDeGroupText(const AData: THCViewData;
      const ADeGroupStartNo: Integer): string;

    /// <summary> 替换指定数据组的内容 </summary>
    /// <param name="AData">指定从哪个Data里获取</param>
    /// <param name="ADeGroupStartNo">被替换的数据组起始位置</param>
    /// <param name="AText">要替换的内容</param>
    procedure SetDataDeGroupText(const AData: THCViewData;
      const ADeGroupStartNo: Integer; const AText: string);

    /// <summary> 是否处于留痕状态 </summary>
    property Trace: Boolean read FTrace write FTrace;

    /// <summary> 当前文档名称 </summary>
    property FileName;

    /// <summary> 当前文档样式表 </summary>
    property Style;

    /// <summary> 是否对称边距 </summary>
    property SymmetryMargin;

    /// <summary> 当前光标所在页的序号 </summary>
    property ActivePageIndex;

    /// <summary> 当前预览的页序号 </summary>
    property PagePreviewFirst;

    /// <summary> 总页数 </summary>
    property PageCount;

    /// <summary> 当前光标所在节的序号 </summary>
    property ActiveSectionIndex;

    /// <summary> 水平滚动条的值 </summary>
    property HScrollValue;

    /// <summary> 垂直滚动条的值 </summary>
    property VScrollValue;

    /// <summary> 缩放值 </summary>
    property Zoom;

    /// <summary> 当前文档所有节 </summary>
    property Sections;

    /// <summary> 是否显示当前行指示符 </summary>
    property ShowLineActiveMark;

    /// <summary> 是否显示行号 </summary>
    property ShowLineNo;

    /// <summary> 是否显示下划线 </summary>
    property ShowUnderLine;

    /// <summary> 当前文档是否有变化 </summary>
    property IsChanged;
  published
    { Published declarations }

    /// <summary> 节有新的Item创建时触发 </summary>
    property OnSectionCreateItem;

    /// <summary> 节有新的Item插入时触发 </summary>
    property OnSectionItemInsert;

    /// <summary> Item绘制开始前触发 </summary>
    property OnSectionDrawItemPaintBefor;

    /// <summary> Item绘制完成后触发 </summary>
    property OnSectionDrawItemPaintAfter;

    /// <summary> 节页眉绘制时触发 </summary>
    property OnSectionPaintHeader;

    /// <summary> 节页脚绘制时触发 </summary>
    property OnSectionPaintFooter;

    /// <summary> 节页面绘制时触发 </summary>
    property OnSectionPaintPage;

    /// <summary> 节整页绘制前触发 </summary>
    property OnSectionPaintWholePageBefor;

    /// <summary> 节整页绘制后触发 </summary>
    property OnSectionPaintWholePageAfter;

    /// <summary> 节只读属性有变化时触发 </summary>
    property OnSectionReadOnlySwitch;

    /// <summary> 页面滚动显示模式：纵向、横向 </summary>
    property PageScrollModel;

    /// <summary> 界面显示模式：页面、Web </summary>
    property ViewModel;

    /// <summary> 是否根据宽度自动计算缩放比例 </summary>
    property AutoZoom;

    /// <summary> 所有Section是否只读 </summary>
    property ReadOnly;

    /// <summary> 鼠标按下时触发 </summary>
    property OnMouseDown;

    /// <summary> 鼠标弹起时触发 </summary>
    property OnMouseUp;

    /// <summary> 光标位置改变时触发 </summary>
    property OnCaretChange;

    /// <summary> 垂直滚动条滚动时触发 </summary>
    property OnVerScroll;

    /// <summary> 文档内容变化时触发 </summary>
    property OnChange;

    /// <summary> 文档Change状态切换时触发 </summary>
    property OnChangedSwitch;

    /// <summary> 窗口重绘开始时触发 </summary>
    property OnUpdateViewBefor;

    /// <summary> 窗口重绘结束后触发 </summary>
    property OnUpdateViewAfter;

    property PopupMenu;

    property Align;
  end;

/// <summary> 注册HCEmrView控件到控件面板 </summary>
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
  FLoading := False;
  FTrace := False;
  HCDefaultTextItemClass := TDeItem;
  HCDefaultDomainItemClass := TDeGroup;
  inherited Create(AOwner);
  Self.Width := 100;
  Self.Height := 100;
  Self.OnSectionCreateItem := DoSectionCreateItem;
  Self.OnUpdateViewBefor := DoUpdateViewBefor;
  Self.OnUpdateViewAfter := DoUpdateViewAfter;
  Self.OnSectionCreateStyleItem := DoCreateStyleItem;
  Self.OnSectionCanEdit := DoCanEdit;
end;

destructor TEmrView.Destroy;
begin
  inherited Destroy;
end;

procedure TEmrView.DoSectionCreateItem(Sender: TObject);
begin
  if (not FLoading) and FTrace then
    (Sender as TDeItem).StyleEx := TStyleExtra.cseAdd;
end;

function TEmrView.DoCanEdit(const Sender: TObject): Boolean;
var
  vViewData: THCViewData;
begin
  vViewData := Sender as THCViewData;
  if vViewData.ActiveDomain.BeginNo >= 0 then
    Result := not (vViewData.Items[vViewData.ActiveDomain.BeginNo] as TDeGroup).ReadOnly
  else
    Result := True;
end;

function TEmrView.DoCreateStyleItem(const AData: THCCustomData; const AStyleNo: Integer): THCCustomItem;
begin
  case AStyleNo of
    THCStyle.Table:
      Result := TDeTable.Create(AData, 1, 1, 1);

    THCStyle.CheckBox:
      Result := TDeCheckBox.Create(AData, '勾选框', False);

    THCStyle.Edit:
      Result := TDeEdit.Create(AData, '');

    THCStyle.Combobox:
      Result := TDeCombobox.Create(AData, '');

    THCStyle.DateTimePicker:
      Result := TDeDateTimePicker.Create(AData, Now);

    THCStyle.RadioGroup:
      Result := TDeRadioGroup.Create(AData);
  else
    Result := nil;
  end;
end;

function TEmrView.DoInsertText(const AText: string): Boolean;
begin
  Result := False;
  if FTrace then
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

procedure TEmrView.DoSectionDrawItemPaintAfter(const Sender: TObject;
  const AData: THCCustomData; const ADrawItemIndex: Integer; const ADrawRect: TRect;
  const ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
  const ACanvas: TCanvas; const APaintInfo: TPaintInfo);
var
  vItem: THCCustomItem;
  vDeItem: TDeItem;
begin
//  if Self.ShowAnnotation then  // 显示批注
//  begin
//    vItem := AData.Items[AData.DrawItems[ADrawItemIndex].ItemNo];
//    if vItem.StyleNo > THCStyle.Null then
//    begin
//      vDeItem := vItem as TDeItem;
//      if (vDeItem.StyleEx <> TStyleExtra.cseNone)
//        and (vDeItem.FirstDItemNo = ADrawItemIndex)
//      then  // 添加批注
//      begin
//        if vDeItem.StyleEx = TStyleExtra.cseDel then
//          Self.Annotates.AddAnnotation(ADrawRect, vDeItem.Text + sLineBreak + vDeItem[TDeProp.Trace])
//        else
//          Self.Annotates.AddAnnotation(ADrawRect, vDeItem.Text + sLineBreak + vDeItem[TDeProp.Trace]);
//      end;
//    end;
//  end;

  inherited DoSectionDrawItemPaintAfter(Sender, AData, ADrawItemIndex, ADrawRect,
    ADataDrawLeft, ADataDrawBottom, ADataScreenTop, ADataScreenBottom, ACanvas, APaintInfo);
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

function TEmrView.GetDataForwardDeGroupText(const AData: THCViewData;
  const ADeGroupStartNo: Integer): string;
var
  i, vBeginNo, vEndNo: Integer;
  vDeGroup: TDeGroup;
  vDeIndex: string;
begin
  Result := '';

  vBeginNo := -1;
  vEndNo := -1;
  vDeIndex := (AData.Items[ADeGroupStartNo] as TDeGroup)[TDeProp.Index];

  for i := 0 to ADeGroupStartNo - 1 do  // 找起始
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
    for i := vBeginNo + 1 to ADeGroupStartNo - 1 do
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
      Result := GetDataDeGroupText(AData, vBeginNo, vEndNo);
  end;
end;

function TEmrView.GetDataDeGroupText(const AData: THCViewData;
  const ADeGroupStartNo, ADeGroupEndNo: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := ADeGroupStartNo + 1 to ADeGroupEndNo - 1 do
    Result := Result + AData.Items[i].Text;
end;

function TEmrView.InsertDeGroup(const ADeGroup: TDeGroup): Boolean;
begin
  Result := InsertDomain(ADeGroup);
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
  vData: THCRichData;
  vText, vCurTrace: string;
  vStyleNo, vParaNo: Integer;
  vDeItem: TDeItem;
  vCurItem: THCCustomItem;
  vCurStyleEx: TStyleExtra;
begin
  if FTrace then
  begin
    vText := '';
    vCurTrace := '';
    vStyleNo := THCStyle.Null;
    vParaNo := THCStyle.Null;
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

    if vData.Items[vData.SelectInfo.StartItemNo].StyleNo < THCStyle.Null then
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

      if FTrace and (vText <> '') then  // 有删除的内容
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
  vData: THCRichData;
begin
  if FTrace then
  begin
    if IsKeyPressWant(Key) then
    begin
      vData := Self.ActiveSectionTopLevelData;

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
  FLoading := True;
  try
    inherited LoadFromStream(AStream);
  finally
    FLoading := False;
  end;
end;

function TEmrView.NewDeItem(const AText: string): TDeItem;
begin
  Result := TDeItem.CreateByText(AText);
  if Self.Style.CurStyleNo > THCStyle.Null then
    Result.StyleNo := Self.Style.CurStyleNo
  else
    Result.StyleNo := 0;

  Result.ParaNo := Self.Style.CurParaNo;
end;

procedure TEmrView.SaveToStream(const AStream: TStream;
  const ASaveParts: TSaveParts);
begin
  inherited SaveToStream(AStream, ASaveParts);
end;

procedure TEmrView.SetDataDeGroupText(const AData: THCViewData;
  const ADeGroupStartNo: Integer; const AText: string);
var
  i, vIgnore, vEndNo: Integer;
begin
  // 找指定的数据组Item范围
  vEndNo := -1;
  vIgnore := 0;

  for i := ADeGroupStartNo + 1 to AData.Items.Count - 1 do
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
      AData.SetSelectBound(ADeGroupStartNo, OffsetAfter, vEndNo, OffsetBefor);
      AData.InsertText(AText);
    finally
      Self.EndUpdate
    end;
  end;
end;

procedure TEmrView.TraverseItem(const ATraverse: TItemTraverse);
var
  i: Integer;
begin
  for i := 0 to Self.Sections.Count - 1 do
  begin
    with Self.Sections[i] do
    begin
      case ATraverse.Area of
        saHeader: Header.TraverseItem(ATraverse);
        saPage: PageData.TraverseItem(ATraverse);
        saFooter: Footer.TraverseItem(ATraverse);
      end;
    end;
  end;
end;

procedure TEmrView.WndProc(var Message: TMessage);
var
  Form: TCustomForm;
  ShiftState: TShiftState;
begin
  if (Message.Msg = WM_KEYDOWN) or (Message.Msg = WM_KEYUP) then
  begin
    if message.WParam in [VK_LEFT..VK_DOWN, VK_RETURN, VK_TAB] then
    begin
      Form := GetParentForm(Self);
      if Form = nil then
      begin
        if Application.Handle <> 0 then  // 在exe中运行
        begin
          if Message.WParam <> VK_RETURN then
          begin
            ShiftState := KeyDataToShiftState(TWMKey(Message).KeyData);
            Self.KeyDown(TWMKey(Message).CharCode, ShiftState);
            Exit;
          end;
        end
        else  // 在浏览器中运行
        begin
          if Message.WParam = VK_RETURN then
          begin
            ShiftState := KeyDataToShiftState(TWMKey(Message).KeyData);
            Self.KeyDown(TWMKey(Message).CharCode, ShiftState);

            Exit;
          end;
        end;
      end;
    end;
  end;

  inherited WndProc(Message);
end;

end.
