unit PluginImp;

interface

uses
  Classes, PluginIntf, FunctionIntf;

type
  TCustomFunction = class(TInterfacedObject, ICustomFunction)
  private
    FID: ShortString;
  public
    constructor Create; virtual;
    function GetID: ShortString;
    procedure SetID(const Value: ShortString);
    //
    property ID: ShortString read GetID write SetID;
  end;

  TPluginFunction = class(TCustomFunction, IPluginFunction)
  private
    FName: ShortString;
    FShowEntrance: Boolean;  // 在主程序中显示操作入口
  public
    constructor Create; override;
    function GetShowEntrance: Boolean;
    procedure SetShowEntrance(const Value: Boolean);
    function GetName: ShortString;
    procedure SetName(const Value: ShortString);
    //
    property Name: ShortString read GetName write SetName;
    property ShowEntrance: Boolean read GetShowEntrance write SetShowEntrance;
  end;

  // 调用插件提供的方法
  TExecFunctionEvent = procedure(const AICustomFunction: ICustomFunction); stdcall;
  TGetPluginInfoEvent = procedure(const AIPInfo: IPlugin); stdcall;
  TUnLoadPluginEvent = procedure(const AIPInfo: IPlugin); stdcall;

  TPlugin = class(TInterfacedObject, IPlugin)
  private
    FAuthor: ShortString;
    FComment: ShortString;
    FID: ShortString;
    FFileName: string;
    FName: ShortString;
    FVersion: ShortString;
    //
    FFunctions: TList;
    FHandle: THandle;  // 插件打开后的句柄
  public
    constructor Create;
    destructor Destroy; override;
    {IPluginInfo}
    function GetFileName: string;
    procedure SetFileName(const AFileName: string);
    procedure LoadPlugin;
    procedure UnLoadPlugin;
    procedure GetPluginInfo;

    function RegFunction(const AID, AName: ShortString): IPluginFunction;
    procedure ExecFunction(const AIFun: ICustomFunction);
    function GetFunctionCount: Integer;
    function GetFunction(const AIndex: Integer): IPluginFunction; overload;
    function GetFunction(const AID: ShortString): IPluginFunction; overload;

    function GetAuthor: ShortString;
    procedure SetAuthor(const Value: ShortString);
    function GetComment: ShortString;
    procedure SetComment(const Value: ShortString);
    function GetID: ShortString;
    procedure SetID(const Value: ShortString);
    function GetName: ShortString;
    procedure SetName(const Value: ShortString);
    function GetVersion: ShortString;
    procedure SetVersion(const Value: ShortString);
  end;

  TPluginManager = class(TInterfacedObject, IPluginManager)
  private
    FPluginList: TPluginList;
    function GetPlguInIndex(const AFileName: ShortString): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    {IPluginManager}
    function LoadPlugins(const APath, AExt: ShortString): Boolean;
    function LoadPlugin(const AFileName: ShortString): Boolean;
    procedure FunBroadcast(const AFun: ICustomFunction);
    function UnLoadPlugin(const APluginID: ShortString): Boolean;
    function UnLoadAllPlugin: Boolean;
    function GetPlugin(const APluginID: ShortString): IPlugin;
    function PluginList: TPluginList;
    function Count: Integer;
  end;

implementation

uses
  SysUtils, Windows;

{ TPluginManager }

function TPluginManager.Count: Integer;
begin
  Result := FPluginList.Count;
end;

constructor TPluginManager.Create;
begin
  FPluginList := TPluginList.Create;
end;

destructor TPluginManager.Destroy;
begin
  UnLoadAllPlugin;
  FPluginList.Free;
  inherited;
end;

procedure TPluginManager.FunBroadcast(const AFun: ICustomFunction);
var
  i: Integer;
begin
  for i := FPluginList.Count - 1 downto 0 do
    IPlugin(FPluginList[i]).ExecFunction(AFun);
end;

function TPluginManager.GetPlguInIndex(const AFileName: ShortString): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FPluginList.Count - 1 do
  begin  
    if IPlugin(FPluginList[i]).FileName = AFileName then
    begin
      Result := i;
      Break
    end;
  end;
end;

function TPluginManager.GetPlugin(const APluginID: ShortString): IPlugin;
var
  i: Integer;
begin
  for i := 0 to FPluginList.Count - 1 do
  begin
    if IPlugin(FPluginList[i]).ID = APluginID then
    begin
      Result := IPlugin(FPluginList[i]);
      Break;
    end;
  end;
end;

function TPluginManager.LoadPlugin(const AFileName: ShortString): Boolean;
var
  vIPlugin, vIAlivePlugin: IPlugin;
  vIndex: Integer;
begin
  Result := False;

  vIndex := GetPlguInIndex(AFileName);
  if vIndex >= 0 then  // 已经加载了该文件的插件，卸载后重新加载
  begin
    IPlugin(FPluginList[vIndex])._Release;
    FPluginList.Delete(vIndex);
  end;
  vIPlugin := TPlugin.Create;
  vIPlugin.FileName := AFileName;
  vIPlugin.GetPluginInfo;
  if vIPlugin.ID <> '' then
  begin
    vIAlivePlugin := GetPlugin(vIPlugin.ID);
    if vIAlivePlugin <> nil then
      raise Exception.Create('异常：注册插件 ' + AFileName + ' 出错，已经存在ID为' + vIPlugin.ID + '的插件 ' + vIAlivePlugin.FileName);
    vIPlugin._AddRef;
    FPluginList.Add(Pointer(vIPlugin));
  end;
  Result := True;
end;

function TPluginManager.LoadPlugins(const APath, AExt: ShortString): Boolean;
var
  vPath: string;
  vSch: TSearchrec;
begin
  Result := False;
  if Copy(APath, Length(APath), 1) <> '\' then
    vPath := APath + '\'
  else
    vPath := APath;
  if not DirectoryExists(vPath) then
    raise Exception.Create('异常：插件目录 ' + APath + ' 不存在!');
  if FindFirst(vPath + '*', faNormal or faDirectory, vSch) = 0 then
  begin
    repeat
      if ((vSch.Name = '.') or (vSch.Name = '..')) then Continue;// 发现文件夹后不进去找里面的文件

      if (UpperCase(ExtractFileExt(vPath + vSch.Name)) = UpperCase(AExt)) or (AExt = '.*') then
        LoadPlugin(vPath + vSch.Name);

    until FindNext(vSch) <> 0;
    SysUtils.FindClose(vSch);
  end;
end;

function TPluginManager.PluginList: TPluginList;
begin
  Result := FPluginList;
end;

function TPluginManager.UnLoadAllPlugin: Boolean;
var
  i: Integer;
begin
  for i := FPluginList.Count - 1 downto 0 do
  begin
    IPlugin(FPluginList[i])._Release;
    FPluginList.Delete(i);
  end;
end;

function TPluginManager.UnLoadPlugin(const APluginID: ShortString): Boolean;
var
  i: Integer;
begin
  for i := 0 to FPluginList.Count - 1 do
  begin
    if IPlugin(FPluginList[i]).ID = APluginID then
    begin
      IPlugin(FPluginList[i])._Release;
      FPluginList.Delete(i);

      Break;
    end;
  end;
end;

{ TPlugin }

constructor TPlugin.Create;
begin
  FHandle := 0;
  FFunctions := TList.Create;
end;

destructor TPlugin.Destroy;
var
  i: Integer;
begin
  for i := FFunctions.Count - 1 downto 0 do
    ICustomFunction(FFunctions[i])._Release;
  FFunctions.Free;

  UnLoadPlugin;

  inherited Destroy;
end;

procedure TPlugin.ExecFunction(const AIFun: ICustomFunction);
var
  vExecFunction: TExecFunctionEvent;
begin
  LoadPlugin;

  vExecFunction := GetProcAddress(FHandle, 'ExecFunction');
  if Assigned(vExecFunction) then
    vExecFunction(AIFun);


  {
  if FHandle <> 0 then
  begin
    vExecFunction := GetProcAddress(FHandle, 'ExecFunction');
    if Assigned(vExecFunction) then
      vExecFunction(AIFun);
  end
  else
  begin
    if LowerCase(ExtractFileExt(FFileName)) = '.bpl' then
      FHandle := LoadPackage(FFileName)
    else
      FHandle := LoadLibrary(PChar(FFileName));

    if FHandle <> 0 then
    begin
      vExecFunction := GetProcAddress(FHandle, 'ExecFunction');
      if Assigned(vExecFunction) then
        vExecFunction(AIFun);

      if Supports(AIFun, StringToGUID(FUN_PLUGIN)) then
      begin
        if not (AIFun as IPluginFunction).ShowEntrance then
        begin
          UnLoadPlugin;
          if FreeLibrary(FHandle) then
            FHandle := 0;
        end;
      end;
    end
  end; }
end;

function TPlugin.GetFileName: string;
begin
  Result := FFileName;
end;

function TPlugin.GetAuthor: ShortString;
begin
  Result := FAuthor;
end;

function TPlugin.GetComment: ShortString;
begin
  Result := FComment;
end;

function TPlugin.GetID: ShortString;
begin
  Result := FID;
end;

function TPlugin.GetName: ShortString;
begin
  Result := FName;
end;

procedure TPlugin.GetPluginInfo;
var
  vGetPluginInfo: TGetPluginInfoEvent;
begin
  Self.LoadPlugin;
  try
    if FHandle <> 0 then
    begin
      vGetPluginInfo := GetProcAddress(FHandle, 'GetPluginInfo');
      if Assigned(vGetPluginInfo) then
        vGetPluginInfo(Self);
    end;
  finally
    Self.UnLoadPlugin;
  end;
end;

function TPlugin.GetFunction(const AID: ShortString): IPluginFunction;
var
  i: Integer;
begin
  for i := 0 to FFunctions.Count - 1 do
  begin
    if GetFunction(i).ID = AID then
    begin
      Result := IPluginFunction(Pointer(FFunctions[i]));
      Break;
    end;
  end;
end;

function TPlugin.GetFunctionCount: Integer;
begin
  Result := FFunctions.Count;
end;

function TPlugin.GetFunction(const AIndex: Integer): IPluginFunction;
begin
  Result := nil;
  if (AIndex < 0) or (AIndex >= FFunctions.Count) then
    Exit;
  Result := IPluginFunction(Pointer(FFunctions[AIndex]));
end;

function TPlugin.GetVersion: ShortString;
begin
  Result := FVersion;
end;

procedure TPlugin.LoadPlugin;
begin
  if FHandle = 0 then
  begin
    if LowerCase(ExtractFileExt(FFileName)) = '.bpl' then
      FHandle := LoadPackage(FFileName)
    else
      FHandle := LoadLibrary(PChar(FFileName));
  end;
end;

function TPlugin.RegFunction(const AID, AName: ShortString): IPluginFunction;
var
  i: Integer;
  vIPluginFunction: IPluginFunction;
begin
  for i := 0 to FFunctions.Count - 1 do  // 是否已经注册过了
  begin
    vIPluginFunction := IPluginFunction(Pointer(FFunctions[i]));
    if vIPluginFunction.ID = AID then
      Exit;
  end;

  Result := TPluginFunction.Create;
  Result.ID := AID;
  Result.Name := AName;
  Result._AddRef;
  FFunctions.Add(Pointer(Result));
end;

procedure TPlugin.SetAuthor(const Value: ShortString);
begin
  if FAuthor <> Value then
    FAuthor := Value;
end;

procedure TPlugin.SetComment(const Value: ShortString);
begin
  if FComment <> Value then
    FComment := Value;
end;

procedure TPlugin.SetFileName(const AFileName: string);
begin
  FFileName := AFileName;
end;

procedure TPlugin.SetID(const Value: ShortString);
begin
  if FID <> Value then
    FID := Value;
end;

procedure TPlugin.SetName(const Value: ShortString);
begin
  if FName <> Value then
    FName := Value;
end;

procedure TPlugin.SetVersion(const Value: ShortString);
begin
  if FVersion <> Value then
    FVersion := Value;
end;

procedure TPlugin.UnLoadPlugin;
var
  vUnLoadPlugin: TUnLoadPluginEvent;
begin
  if FHandle > 0 then
  begin
    vUnLoadPlugin := GetProcAddress(FHandle, 'UnLoadPlugin');
    if Assigned(vUnLoadPlugin) then
      vUnLoadPlugin(Self);

    if FreeLibrary(FHandle) then
      FHandle := 0;
  end;
end;

{ TCustomFunction }

constructor TCustomFunction.Create;
begin
  FID := FUN_CUSTOM;
end;

function TCustomFunction.GetID: ShortString;
begin
  Result := FID;
end;

procedure TCustomFunction.SetID(const Value: ShortString);
begin
  FID := Value;
end;

{ TPluginFunction }

constructor TPluginFunction.Create;
begin
  ID := FUN_PLUGIN;
end;

function TPluginFunction.GetName: ShortString;
begin
  Result := FName;
end;

function TPluginFunction.GetShowEntrance: Boolean;
begin
  Result := FShowEntrance;
end;

procedure TPluginFunction.SetName(const Value: ShortString);
begin
  FName := Value;
end;

procedure TPluginFunction.SetShowEntrance(const Value: Boolean);
begin
  FShowEntrance := Value;
end;

end.
