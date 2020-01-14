unit HCEmrViewLite;

interface

uses
  Classes, SysUtils, HCView, HCStyle, HCCustomData, HCCustomFloatItem,
  HCItem, HCTextItem, HCRectItem, HCSectionData;

type
  THCImportAsTextEvent = procedure (const AText: string) of object;

  THCEmrViewLite = class(THCView)
  protected
    /// <summary> 当有新Item创建时触发 </summary>
    /// <param name="AData">创建Item的Data</param>
    /// <param name="AStyleNo">要创建的Item样式</param>
    /// <returns>创建好的Item</returns>
    function DoSectionCreateStyleItem(const AData: THCCustomData;
      const AStyleNo: Integer): THCCustomItem; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  HCEmrElementItem, HCEmrGroupItem, HCEmrYueJingItem, HCEmrFangJiaoItem, HCEmrToothItem;

{ THCEmrViewLite }

constructor THCEmrViewLite.Create(AOwner: TComponent);
begin
  HCDefaultTextItemClass := TDeItem;
  HCDefaultDomainItemClass := TDeGroup;
  inherited Create(AOwner);
end;

function THCEmrViewLite.DoSectionCreateStyleItem(const AData: THCCustomData;
  const AStyleNo: Integer): THCCustomItem;
begin
  Result := CreateEmrStyleItem(aData, aStyleNo);
end;

end.
