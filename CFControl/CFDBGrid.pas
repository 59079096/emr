unit CFDBGrid;

interface

uses
  Classes, Graphics, CFGrid, DB;

type
  TCFDBGrid = class;

  TCField = class(TCollectionItem)
  private
    /// <summary> 字段名标题 </summary>
    FTitle,
    /// <summary> 字段名 </summary>
    FFieldName: string;

    /// <summary> 标题字段宽度 </summary>
    FWidth: Integer;
    FDataType: TFieldType;
  protected
    function GetDisplayName: string; override;

    /// <summary>
    /// 设置字段名
    /// </summary>
    /// <param name="Value">字段名</param>
    procedure SetFieldName(const Value: string);
    function GetGrid: TCFDBGrid;

    function GetAsInteger: Longint;
    procedure SetAsInteger(Value: Longint);
    function GetAsString: string;
    procedure SetAsString(Value: string);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
  published
    property Title: string read FTitle write FTitle;
    property FieldName: string read FFieldName write SetFieldName;
    property DataType: TFieldType read FDataType write FDataType;
    property Width: Integer read FWidth write FWidth;
  end;

  TCFieldClass = class of TCField;

  TCGridFields = class(TCollection)
  private
    FGrid: TCFDBGrid;
    /// <summary>
    /// 获取指定字段信息
    /// </summary>
    /// <param name="Index">第几个字段</param>
    /// <returns>字段</returns>
    function GetColumn(Index: Integer): TCField;

    /// <summary>
    /// 设置字段信息
    /// </summary>
    /// <param name="Index">第几个字段</param>
    /// <param name="Value">第几个字段的相关信息</param>
    procedure SetColumn(Index: Integer; Value: TCField);
  protected
    function GetOwner: TPersistent; override;

    procedure Added(var Item: TCollectionItem); override;
    procedure Deleting(Item: TCollectionItem); override;
    /// <summary>
    /// 更新字段信息
    /// </summary>
    /// <param name="Item">字段信息</param>
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AGrid: TCFDBGrid; AItemClass: TCFieldClass);
    destructor Destroy; override;

    /// <summary>
    /// 增加字段
    /// </summary>
    /// <returns></returns>
    function Add: TCField;
    function IndexOf(const AFieldName: string): Integer;

    property Grid: TCFDBGrid read FGrid;
    property Items[Index: Integer]: TCField read GetColumn write SetColumn; default;
  end;

  TCFDBGrid = class(TCFGrid)
  private
    FFields: TCGridFields;

    /// <summary> 当前排序状态 </summary>
    FSortAsc: Boolean;

    /// <summary>
    /// 更新 Grid 的列
    /// </summary>
    /// <param name="Item">更新的列</param>
    procedure UpdateFields(Item: TCField);
  protected
    /// <summary>
    /// 利用画布进行绘制
    /// </summary>
    /// <param name="ACanvas">画布</param>
    procedure DrawControl(ACanvas: TCanvas); override;

    /// <summary>
    /// 添加列数，并添加内容
    /// </summary>
    /// <param name="Value">内容</param>
    procedure SetFields(Value: TCGridFields);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FindField(const AFieldName: string): Integer;
    /// <summary>
    /// 获取数据信息
    /// </summary>
    /// <param name="ADataSet">数据信息</param>
    procedure LoadFromDataSet(const ADataSet: TDataSet);

    function FieldByName(const AFieldName: string): TCField;

    /// <summary> 当前排序状态 </summary>
    property SortAsc: Boolean read FSortAsc write FSortAsc;

    property RowCount;
    property ColCount;
  published
    property Fields: TCGridFields read FFields write SetFields;
  end;

implementation

uses
  SysUtils;

{ TCFDBGrid }

function TCFDBGrid.FieldByName(const AFieldName: string): TCField;
var
  vIndex: Integer;
begin
  vIndex := FindField(AFieldName);
  if vIndex < 0 then
    raise Exception.Create('异常：未发现字段 ' + AFieldName)
  else
    Result := FFields[vIndex];
end;

constructor TCFDBGrid.Create(AOwner: TComponent);
begin
  inherited CreateEx(AOwner, 0, 0);  // 默认创建的 CDBGrid 为一行一列
  FFields := TCGridFields.Create(Self, TCField);
  FSortAsc := False;
end;

destructor TCFDBGrid.Destroy;
begin
  FFields.Free;
  inherited;
end;

procedure TCFDBGrid.DrawControl(ACanvas: TCanvas);
begin
  inherited;

end;

function TCFDBGrid.FindField(const AFieldName: string): Integer;
begin
  Result := FFields.Indexof(AFieldName);
end;

procedure TCFDBGrid.LoadFromDataSet(const ADataSet: TDataSet);
var
  vRow, vCol: Integer;
begin
  //FColumns.Clear;  // 清空 CDGrid 上的字段信息
  BeginUpdate;
  try
    RowCount := ADataSet.RecordCount;  // 数据行赋值为 CDGrid 的行
    if FFields.Count = 0 then
    begin
      for vCol := 0 to ADataSet.Fields.Count - 1 do  // 遍历数据源的列并加载数据字段名
      begin
        with FFields.Add do  // 添加字段，并设置字段宽和名的信息
        begin
          Width := DefaultColWidth;
          FieldName := ADataSet.Fields[vCol].FieldName;
        end;
        //TitleText[vCol] := ADataSet.Fields[vCol].FieldName;
      end;
    end;
    vRow := 0;  // 设置初始行
    if not ADataSet.IsEmpty then  // 加载数据
    begin
      ADataSet.First;
      while not ADataSet.Eof do
      begin
        for vCol := 0 to ColCount - 1 do  // 行中的数据加载
          Cells[vRow, vCol] := ADataSet.FieldByName(FFields[vCol].FieldName).AsString;  // 数据赋值
        Inc(vRow);
        ADataSet.Next;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCFDBGrid.SetFields(Value: TCGridFields);
begin
  FFields.Assign(Value);
end;

procedure TCFDBGrid.UpdateFields(Item: TCField);
var
  vCol: Integer;
begin
  if csLoading in ComponentState then Exit;

  ColCount := FFields.Count;

  if Item = nil then  // 变动字段为空的时候更新所有的字段信息
  begin
    for vCol := 0 to FFields.Count - 1 do
      TitleText[vCol] := FFields.Items[vCol].FieldName;
  end
  else  // 变动字段不为空更新变动字段的信息
    TitleText[Item.Index] := Item.FieldName;
  UpdateDirectUI;
end;

{ TCGridFields }

function TCGridFields.Add: TCField;
begin
  Result := TCField(inherited Add);
end;

procedure TCGridFields.Added(var Item: TCollectionItem);
begin
  inherited;
  FGrid.ColCount := Self.Count;
end;

constructor TCGridFields.Create(AGrid: TCFDBGrid; AItemClass: TCFieldClass);
begin
  inherited Create(AItemClass);
  FGrid := AGrid;
end;

procedure TCGridFields.Deleting(Item: TCollectionItem);
begin
  inherited;
  FGrid.ColCount := Self.Count;
end;

destructor TCGridFields.Destroy;
begin

  inherited;
end;

function TCGridFields.GetColumn(Index: Integer): TCField;
begin
  Result := TCField(inherited Items[Index]);
end;

function TCGridFields.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

function TCGridFields.IndexOf(const AFieldName: string): Integer;
var
  i: Integer;
  vFielName: string;
begin
  Result := -1;

  vFielName := UpperCase(AFieldName);
  for i := 0 to Count - 1 do
  begin
    if UpperCase(Items[i].FieldName) = vFielName then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TCGridFields.SetColumn(Index: Integer; Value: TCField);
begin
  Items[Index].Assign(Value);  // inherited SetItem(Index, Value);
end;

procedure TCGridFields.Update(Item: TCollectionItem);
begin
  inherited;
  FGrid.UpdateFields(TCField(Item));
end;

{ TCField }

procedure TCField.Assign(Source: TPersistent);
begin
  if Source is TCField then  // 设置字段的宽度，字段名和字段标题
  begin
    FWidth := TCField(Source).Width;
    FTitle := TCField(Source).Title;
    FFieldName := TCField(Source).FieldName;
  end
  else
    inherited Assign(Source);
end;

constructor TCField.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FWidth := TCGridFields(Collection).Grid.DefaultColWidth;
end;

function TCField.GetAsInteger: Longint;
var
  vGrid: TCFDBGrid;
begin
  vGrid := GetGrid;
  TryStrToInt(vGrid.Cells[vGrid.RowIndex, vGrid.FindField(Self.FieldName)], Result);
end;

function TCField.GetAsString: string;
var
  vGrid: TCFDBGrid;
begin
  vGrid := GetGrid;
  Result := vGrid.Cells[vGrid.RowIndex, vGrid.FindField(Self.FieldName)];
end;

function TCField.GetDisplayName: string;
begin
  Result := FFieldName;
  if Result = '' then
    Result := inherited GetDisplayName;// + IntToStr(ID);
end;

function TCField.GetGrid: TCFDBGrid;
begin
  if Assigned(Collection) and (Collection is TCGridFields) then
    Result := TCGridFields(Collection).Grid
  else
    Result := nil;
end;

procedure TCField.SetAsInteger(Value: Integer);
var
  vGrid: TCFDBGrid;
begin
  vGrid := GetGrid;
  vGrid.Cells[vGrid.RowIndex, vGrid.FindField(Self.FieldName)] := IntToStr(Value);
  Changed(False);
end;

procedure TCField.SetAsString(Value: string);
var
  vGrid: TCFDBGrid;
begin
  vGrid := GetGrid;
  vGrid.Cells[vGrid.RowIndex, vGrid.FindField(Self.FieldName)] := Value;
  Changed(False);
end;

procedure TCField.SetFieldName(const Value: string);
var
  vGrid: TCFDBGrid;
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;
    if FTitle = '' then  // 如果字段标题为空，设置字段标题和标题名一致
      FTitle := Value;
    vGrid := GetGrid;
    vGrid.TitleText[Index] := FTitle;
    Changed(False);
  end;
end;

end.
