unit UPClientContext;

interface

uses
  SysUtils, Classes, Windows, Math, UPMsgPack, diocp_coder_tcpServer;

type
  TUPClientContext = class;

  TOnContextActionEvent = procedure(const AStream: TStream;
    const AContext: TUPClientContext) of object;

  TUPClientContext = class(TIOCPCoderClientContext)
  private
    FOnContextAction: TOnContextActionEvent;
  protected
    procedure DoCleanUp; override;
    procedure OnDisconnected; override;
    procedure OnConnected; override;
    /// <summary> 接收到一个完整的数据包 </summary>
    procedure DoContextAction(const ADataObject: TObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SendMsgPack(const AMsgPack: TUPMsgPack);
    property OnContextAction: TOnContextActionEvent read FOnContextAction write FOnContextAction;
  end;

implementation

uses
  utils_zipTools;

constructor TUPClientContext.Create;
begin
  inherited Create;
end;

destructor TUPClientContext.Destroy;
begin
  inherited Destroy;
end;

procedure TUPClientContext.DoCleanUp;
begin
  inherited DoCleanUp;
end;

procedure TUPClientContext.DoContextAction(const ADataObject: TObject);
begin
  // 此方法被触发时已经由TIOCPCoderClientContext.DoExecuteRequest处理线程同步
  if Assigned(FOnContextAction) then
    FOnContextAction(TMemoryStream(ADataObject), Self);
end;

procedure TUPClientContext.OnConnected;
begin

end;

procedure TUPClientContext.OnDisconnected;
begin

end;

procedure TUPClientContext.SendMsgPack(const AMsgPack: TUPMsgPack);
var
  vStream, vZipStream: TMemoryStream;
begin
  vStream := TMemoryStream.Create;
  try
    AMsgPack.EncodeToStream(vStream);  // 打包
    vStream.Position := 0;

    vZipStream := TMemoryStream.Create;
    try
      TZipTools.ZipStream(vStream, vZipStream);  // 压缩数据
      vZipStream.Position := 0;
      WriteObject(vZipStream);  // 推送到客户端
    finally
      FreeAndNil(vZipStream);
    end;
  finally
    FreeAndNil(vStream);
  end;
end;

end.
