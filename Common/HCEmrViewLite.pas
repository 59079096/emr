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
    /// <summary> ����Item </summary>
    /// <param name="ATraverse">����ʱ��Ϣ</param>
    procedure TraverseItem(const ATraverse: THCItemTraverse);
  end;

implementation

uses
  HCEmrElementItem, HCEmrGroupItem, HCTextItem, HCRectItem, HCCommon;

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
