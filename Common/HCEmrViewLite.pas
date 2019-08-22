unit HCEmrViewLite;

interface

uses
  System.Classes, System.SysUtils, HCView, HCStyle, HCCustomData, HCItem, HCTextItem, HCRectItem;

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
    /// <summary> 创建指定样式的Item </summary>
    /// <param name="AData">要创建Item的Data</param>
    /// <param name="AStyleNo">要创建的Item样式</param>
    /// <returns>创建好的Item</returns>
    class function CreateEmrStyleItem(const AData: THCCustomData;
      const AStyleNo: Integer): THCCustomItem;
  end;

implementation

uses
  emr_Common, HCEmrElementItem, HCEmrGroupItem, HCEmrYueJingItem,
  HCEmrFangJiaoItem, HCEmrToothItem;

{ THCEmrViewLite }

constructor THCEmrViewLite.Create(AOwner: TComponent);
begin
  HCDefaultTextItemClass := TDeItem;
  HCDefaultDomainItemClass := TDeGroup;
  inherited Create(AOwner);
end;

class function THCEmrViewLite.CreateEmrStyleItem(const AData: THCCustomData;
  const AStyleNo: Integer): THCCustomItem;
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

    THCStyle.Express, EMRSTYLE_YUEJING:
      Result := TEmrYueJingItem.Create(AData, '', '', '', '');

    EMRSTYLE_TOOTH:
      Result := TEmrToothItem.Create(AData, '', '', '', '');

    EMRSTYLE_FANGJIAO:
      Result := TEMRFangJiaoItem.Create(AData, '', '', '', '');
  else
    Result := nil;
  end;
end;

function THCEmrViewLite.DoSectionCreateStyleItem(const AData: THCCustomData;
  const AStyleNo: Integer): THCCustomItem;
begin
  Result := CreateEmrStyleItem(aData, aStyleNo);
end;

end.
