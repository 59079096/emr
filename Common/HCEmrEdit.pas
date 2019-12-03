{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit HCEmrEdit;

interface

uses
  Windows, Classes, Controls, Vcl.Graphics, HCCommon, HCStyle, HCEdit, HCItem,
  HCCustomData, HCViewData, HCEmrElementItem, HCEmrGroupItem, HCTextItem, HCRectItem;

type
  TEmrEdit = class(THCEdit)
  private
    FDeDoneColor, FDeUnDoneColor: TColor;
    FDesignMode: Boolean;
  protected
    function DoDataCreateStyleItem(const AData: THCCustomData;
      const AStyleNo: Integer): THCCustomItem; override;
    procedure DoDrawItemPaintBefor(const AData: THCCustomData;
      const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect; const ADataDrawLeft,
      ADataDrawRight, ADataDrawBottom, ADataScreenTop, ADataScreenBottom: Integer;
      const ACanvas: TCanvas; const APaintInfo: TPaintInfo); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary> 新建数据元 </summary>
    /// <param name="AText">数据元文本</param>
    /// <returns>新建好的数据元</returns>
    function NewDeItem(const AText: string): TDeItem;

    /// <summary> 插入数据组 </summary>
    /// <param name="ADeGroup">数据组信息</param>
    /// <returns>True：成功，False：失败</returns>
    function InsertDeGroup(const ADeGroup: TDeGroup): Boolean;

    /// <summary> 插入数据元 </summary>
    /// <param name="ADeItem">数据元信息</param>
    /// <returns>True：成功，False：失败</returns>
    function InsertDeItem(const ADeItem: TDeItem): Boolean;

    /// <summary> 直接设置当前数据元的值为扩展内容 </summary>
  	/// <param name="AStream">扩展内容流</param>
    procedure SetActiveItemExtra(const AStream: TStream);
    property DesignMode: Boolean read FDesignMode write FDesignMode;

    /// <summary> 当前文档样式表 </summary>
    property Style;
  published
    { Published declarations }

    /// <summary> 鼠标按下时触发 </summary>
    property OnMouseDown;

    /// <summary> 鼠标弹起时触发 </summary>
    property OnMouseUp;

    /// <summary> 文档内容变化时触发 </summary>
    property OnChange;

    property PopupMenu;

    property Align;
  end;

/// <summary> 注册HCEmrView控件到控件面板 </summary>
procedure Register;

implementation

uses
  SysUtils, Forms, HCPrinters, HCTextStyle;

procedure Register;
begin
  RegisterComponents('HCEmrViewVCL', [TEmrEdit]);
end;

{ TEmrEdit }

constructor TEmrEdit.Create(AOwner: TComponent);
begin
  HCDefaultTextItemClass := TDeItem;
  HCDefaultDomainItemClass := TDeGroup;
  inherited Create(AOwner);
  Self.Width := 100;
  Self.Height := 100;
  FDeDoneColor := clBtnFace;  // 元素填写后背景色
  FDeUnDoneColor := $0080DDFF;  // 元素未填写时背景色
end;

destructor TEmrEdit.Destroy;
begin
  inherited Destroy;
end;

function TEmrEdit.InsertDeGroup(const ADeGroup: TDeGroup): Boolean;
begin
  Result := InsertDomain(ADeGroup);
end;

function TEmrEdit.InsertDeItem(const ADeItem: TDeItem): Boolean;
begin
  Result := Self.InsertItem(ADeItem);
end;

function TEmrEdit.NewDeItem(const AText: string): TDeItem;
begin
  Result := TDeItem.CreateByText(AText);
  if Self.CurStyleNo > THCStyle.Null then
    Result.StyleNo := Self.CurStyleNo
  else
    Result.StyleNo := 0;

  Result.ParaNo := Self.CurParaNo;
end;

procedure TEmrEdit.SetActiveItemExtra(const AStream: TStream);
var
  vFileFormat: string;
  vFileVersion: Word;
  vLang: Byte;
  vStyle: THCStyle;
begin
  _LoadFileFormatAndVersion(AStream, vFileFormat, vFileVersion, vLang);  // 文件格式和版本
  vStyle := THCStyle.Create;
  try
    vStyle.LoadFromStream(AStream, vFileVersion);
    Self.BeginUpdate;
    try
      Self.UndoGroupBegin;
      try
        Self.Data.DeleteActiveDataItems(Self.Data.SelectInfo.StartItemNo,
          Self.Data.SelectInfo.StartItemNo, True);

        Self.Data.InsertStream(AStream, vStyle, vFileVersion);
      finally
        Self.UndoGroupEnd;
      end;
    finally
      Self.EndUpdate;
    end;
  finally
    FreeAndNil(vStyle);
  end;
end;

function TEmrEdit.DoDataCreateStyleItem(const AData: THCCustomData;
  const AStyleNo: Integer): THCCustomItem;
begin
  Result := HCEmrElementItem.CreateEmrStyleItem(AData, AStyleNo);
end;

procedure TEmrEdit.DoDrawItemPaintBefor(const AData: THCCustomData;
  const AItemNo, ADrawItemNo: Integer; const ADrawRect: TRect;
  const ADataDrawLeft, ADataDrawRight, ADataDrawBottom, ADataScreenTop,
  ADataScreenBottom: Integer; const ACanvas: TCanvas;
  const APaintInfo: TPaintInfo);
var
  vDeItem: TDeItem;
begin
  if (not APaintInfo.Print) and (AData.Items[AItemNo] is TDeItem) then
  begin
    vDeItem := AData.Items[AItemNo] as TDeItem;
    if vDeItem.IsElement then  // 是数据元
    begin
      if vDeItem.MouseIn or vDeItem.Active then  // 鼠标移入和光标在其中
      begin
        if vDeItem.IsSelectPart or vDeItem.IsSelectComplate then
        begin

        end
        else
        begin
          if vDeItem[TDeProp.Name] <> vDeItem.Text then  // 已经填写过了
            ACanvas.Brush.Color := FDeDoneColor
          else  // 没填写过
            ACanvas.Brush.Color := FDeUnDoneColor;

          ACanvas.FillRect(ADrawRect);
        end;
      end;
    end
    else  // 不是数据元
    if FDesignMode and vDeItem.EditProtect then
    begin
      ACanvas.Brush.Color := clBtnFace;
      ACanvas.FillRect(ADrawRect);
    end;
  end;
end;

end.
