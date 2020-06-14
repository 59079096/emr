unit HCEmrViewLite;

interface

uses
  Classes, SysUtils, HCViewLite, HCCustomData, HCItem;

type
  THCImportAsTextEvent = procedure (const AText: string) of object;

  THCEmrViewLite = class(THCViewLite)
  protected
    /// <summary> ������Item����ʱ���� </summary>
    /// <param name="AData">����Item��Data</param>
    /// <param name="AStyleNo">Ҫ������Item��ʽ</param>
    /// <returns>�����õ�Item</returns>
    function DoSectionCreateStyleItem(const AData: THCCustomData;
      const AStyleNo: Integer): THCCustomItem; override;
  public
    constructor Create; override;
  end;

implementation

uses
  HCEmrElementItem, HCEmrGroupItem, HCTextItem, HCRectItem;

{ THCEmrViewLite }

constructor THCEmrViewLite.Create;
begin
  HCDefaultTextItemClass := TDeItem;
  HCDefaultDomainItemClass := TDeGroup;
  inherited Create;
end;

function THCEmrViewLite.DoSectionCreateStyleItem(const AData: THCCustomData;
  const AStyleNo: Integer): THCCustomItem;
begin
  Result := HCEmrElementItem.CreateEmrStyleItem(aData, aStyleNo);
end;

end.
