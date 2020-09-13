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
    /// <summary> ������Item����ʱ���� </summary>
    /// <param name="AData">����Item��Data</param>
    /// <param name="AStyleNo">Ҫ������Item��ʽ</param>
    /// <returns>�����õ�Item</returns>
    function DoSectionCreateStyleItem(const AData: THCCustomData;
      const AStyleNo: Integer): THCCustomItem; override;

    /// <summary> ��ȡ�ĵ�ǰ�����¼�������ȷ�϶����������� </summary>
    procedure DoLoadStreamBefor(const AStream: TStream; const AFileVersion: Word); override;
    /// <summary> �����ĵ�ǰ�����¼������ڶ����������� </summary>
    procedure DoSaveStreamBefor(const AStream: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    /// <summary> ����Item </summary>
    /// <param name="ATraverse">����ʱ��Ϣ</param>
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
    if Self.Style.States.Contain(hosLoading) then  // ���ز���ʱ�Ŵ��������ļ���ʱ������
      FPropertys.Text := vS;
  end
  else
  if Self.Style.States.Contain(hosLoading) then  // ���ز���ʱ�Ŵ��������ļ���ʱ������
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
