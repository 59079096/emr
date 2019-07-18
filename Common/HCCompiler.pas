unit HCCompiler;

interface

uses
  Classes, SysUtils, PaxCompiler, PaxProgram, PaxRunner, PaxJavaScriptLanguage;

type
  TScriptType = (stpNone, stpPascal, stpJavaScript);

  THCCompiler = class(TPaxCompiler)
  private
    FPaxProgram: TPaxProgram;
    FPaxLanguage: TPaxCompilerLanguage;
    FScriptType: TScriptType;

    procedure SetScriptType(const Value: TScriptType);
  protected
    procedure DoMapTableNamespace(Sender: TPaxRunner; const FullName: string;
      Global: Boolean); virtual;
    procedure DoMapTableVarAddress(Sender: TPaxRunner; const FullName: string;
      Global: Boolean; var Address: Pointer); virtual;
    procedure DoMapTableProcAddress(Sender: TPaxRunner; const FullName: string;
      OverCount: Byte; Global: Boolean; var Address: Pointer); virtual;
    procedure DoMapTableClassRef(Sender: TPaxRunner; const FullName: string;
      Global: Boolean; var ClassRef: TClass); virtual;
  public
    constructor CreateByScriptType(AOwner: TComponent; const AScriptType: TScriptType = stpPascal); virtual;
    destructor Destroy; override;
    procedure ResetRegister;
    function RunScript(const ACode: string): Boolean;
    function RunScriptBin(const AStream: TStream): Boolean;
    function CompileScript(const ACode: string): Boolean;
    procedure SaveToStream(const AStream: TStream);
    procedure LoadFromStream(const AStream: TStream);

    property ScriptType: TScriptType read FScriptType write SetScriptType;
  end;

implementation

{ THCCompiler }

function THCCompiler.CompileScript(const ACode: string): Boolean;
begin
  Result := False;

  AddCode('1', ACode);

  if Compile(FPaxProgram) then
    Result := True;
end;

constructor THCCompiler.CreateByScriptType(AOwner: TComponent;
  const AScriptType: TScriptType = stpPascal);
begin
  inherited Create(AOwner);
  FScriptType := stpNone;
  FPaxProgram := TPaxProgram.Create(nil);
  FPaxProgram.OnMapTableNamespace := DoMapTableNamespace;
  FPaxProgram.OnMapTableVarAddress := DoMapTableVarAddress;
  FPaxProgram.OnMapTableProcAddress := DoMapTableProcAddress;
  FPaxProgram.OnMapTableClassRef := DoMapTableClassRef;

  SetScriptType(AScriptType);
end;

destructor THCCompiler.Destroy;
begin
  FPaxProgram.Free;
  if Assigned(FPaxLanguage) then
    FPaxLanguage.Free;
  inherited Destroy;
end;

procedure THCCompiler.DoMapTableClassRef(Sender: TPaxRunner;
  const FullName: string; Global: Boolean; var ClassRef: TClass);
begin
end;

procedure THCCompiler.DoMapTableNamespace(Sender: TPaxRunner;
  const FullName: string; Global: Boolean);
begin
end;

procedure THCCompiler.DoMapTableProcAddress(Sender: TPaxRunner;
  const FullName: string; OverCount: Byte; Global: Boolean;
  var Address: Pointer);
begin
end;

procedure THCCompiler.DoMapTableVarAddress(Sender: TPaxRunner;
  const FullName: string; Global: Boolean; var Address: Pointer);
begin
end;

procedure THCCompiler.LoadFromStream(const AStream: TStream);
begin
  FPaxProgram.LoadFromStream(AStream);
end;

procedure THCCompiler.ResetRegister;
begin
  Reset;
  RegisterLanguage(FPaxLanguage);
  AddModule('1', FPaxLanguage.LanguageName);
end;

function THCCompiler.RunScript(const ACode: string): Boolean;
begin
  Result := False;

  AddCode('1', ACode);

  if Compile(FPaxProgram) then
  begin
    FPaxProgram.Run;
    Result := True;
  end;
end;

function THCCompiler.RunScriptBin(const AStream: TStream): Boolean;
begin
  Result := False;
  AStream.Position := 0;
  FPaxProgram.LoadFromStream(AStream);
  FPaxProgram.MapGlobal;
  FPaxProgram.MapLocal;
  FPaxProgram.Run;
  Result := True;
end;

procedure THCCompiler.SaveToStream(const AStream: TStream);
begin
  FPaxProgram.SaveToStream(AStream);
end;

procedure THCCompiler.SetScriptType(const Value: TScriptType);
begin
  if FScriptType <> Value then
  begin
    if Assigned(FPaxLanguage) then
      FPaxLanguage.Free;

    FScriptType := Value;
    case FScriptType of
      stpPascal: FPaxLanguage := TPaxPascalLanguage.Create(nil);
      stpJavaScript: FPaxLanguage := TPaxJavaScriptLanguage.Create(nil);
    end;

    ResetRegister;
  end;
end;

end.
