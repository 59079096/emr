unit HCEmrViewLite;

interface

uses
  Classes, SysUtils, HCViewLite, HCCustomData, HCItem;

type
  THCImportAsTextEvent = procedure (const AText: string) of object;

  THCEmrViewLite = class(THCViewLite)
  private
    FPropertys: TStringList;
  protected
    /// <summary> 当有新Item创建时触发 </summary>
    /// <param name="AData">创建Item的Data</param>
    /// <param name="AStyleNo">要创建的Item样式</param>
    /// <returns>创建好的Item</returns>
    function DoSectionCreateStyleItem(const AData: THCCustomData;
      const AStyleNo: Integer): THCCustomItem; override;

    /// <summary> 读取文档前触发事件，便于确认订制特征数据 </summary>
    procedure DoLoadStreamBefor(const AStream: TStream; const AFileVersion: Word); override;
    /// <summary> 保存文档前触发事件，便于订制特征数据 </summary>
    procedure DoSaveStreamBefor(const AStream: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    /// <summary> 遍历Item </summary>
    /// <param name="ATraverse">遍历时信息</param>
    procedure TraverseItem(const ATraverse: THCItemTraverse);
  end;

implementation

uses
  HCEmrElementItem, HCEmrGroupItem, HCTextItem, HCRectItem, HCCommon, HCSectionData;

{ THCEmrViewLite }

constructor THCEmrViewLite.Create;
begin
  HCDefaultTextItemClass := TDeItem;
  HCDefaultDomainItemClass := TDeGroup;
  inherited Create;
  FPropertys := TStringList.Create;
end;

destructor THCEmrViewLite.Destroy;
begin
  FreeAndNil(FPropertys);
  inherited Destroy;
end;

procedure THCEmrViewLite.DoLoadStreamBefor(const AStream: TStream;
  const AFileVersion: Word);
var
  vVersion: Byte;
  vS: string;
begin
  if AFileVersion > 43 then
    AStream.ReadBuffer(vVersion, 1)
  else
    vVersion := 0;

  if vVersion > 0 then
  begin
    HCLoadTextFromStream(AStream, vS, AFileVersion);
    if Self.Style.States.Contain(hosLoading) then  // 加载病历时才处理，插入文件流时不覆盖
      FPropertys.Text := vS;
  end
  else
  if Self.Style.States.Contain(hosLoading) then  // 加载病历时才处理，插入文件流时不覆盖
    FPropertys.Clear;

  inherited DoLoadStreamBefor(AStream, AFileVersion);
end;

procedure THCEmrViewLite.DoSaveStreamBefor(const AStream: TStream);
var
  vByte: Byte;
begin
  vByte := EmrViewVersion;
  AStream.WriteBuffer(vByte, SizeOf(vByte));
  HCSaveTextToStream(AStream, FPropertys.Text);
  inherited DoSaveStreamBefor(AStream);
end;

function THCEmrViewLite.DoSectionCreateStyleItem(const AData: THCCustomData;
  const AStyleNo: Integer): THCCustomItem;
begin
  Result := HCEmrElementItem.CreateEmrStyleItem(aData, aStyleNo);
end;

procedure THCEmrViewLite.TraverseItem(const ATraverse: THCItemTraverse);
var
  i: Integer;
begin
  if ATraverse.Areas = [] then Exit;

  for i := 0 to Self.Sections.Count - 1 do
  begin
    if not ATraverse.Stop then
    begin
      with Self.Sections[i] do
      begin
        if saHeader in ATraverse.Areas then
          Header.TraverseItem(ATraverse);

        if (not ATraverse.Stop) and (saPage in ATraverse.Areas) then
          Page.TraverseItem(ATraverse);

        if (not ATraverse.Stop) and (saFooter in ATraverse.Areas) then
          Footer.TraverseItem(ATraverse);
      end;
    end;
  end;
end;

end.
