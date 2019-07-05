{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit TemplateCommon;

interface

uses
  emr_Common, emr_BLLServerProxy, FireDAC.Comp.Client;

  function CommonLastError: string;

  // 删除值域选项关联的内容
  function DeleteDomainItemContent(const ADItemID: Integer): Boolean;

  // 删除值域某个选项
  function DeleteDomainItem(const ADItemID: Integer): Boolean;

  // 删除值域所有选项
  function DeleteDomainAllItem(const ADomainID: Integer): Boolean;

implementation

var
  FLastError: string;

function CommonLastError: string;
begin
  Result := FLastError;
end;

function DeleteDomainAllItem(const ADomainID: Integer): Boolean;
var
  vDeleteOk: Boolean;
begin
  Result := False;

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_DELETEDOMAINALLITEM;  // 删除值域对应的所有选项
      ABLLServerReady.ExecParam.I['DomainID'] := ADomainID;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      vDeleteOk := ABLLServer.MethodRunOk;
      if not vDeleteOk then
        FLastError := ABLLServer.MethodError;
    end);

  Result := vDeleteOk;
end;

function DeleteDomainItem(const ADItemID: Integer): Boolean;
var
  vDeleteOk: Boolean;
begin
  Result := False;  // 删除选项

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_DELETEDOMAINITEM;  // 删除值域选项
      ABLLServerReady.ExecParam.I['ID'] := ADItemID;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      vDeleteOk := ABLLServer.MethodRunOk;
      if not vDeleteOk then
        FLastError := ABLLServer.MethodError;
    end);

  Result := vDeleteOk;
end;

function DeleteDomainItemContent(const ADItemID: Integer): Boolean;
var
  vDeleteOk: Boolean;
begin
  Result := False;

  BLLServerExec(procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_DELETEDOMAINITEMCONTENT;  // 删除值域选项关联内容
      ABLLServerReady.ExecParam.I['DItemID'] := ADItemID;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      vDeleteOk := ABLLServer.MethodRunOk;
      if not vDeleteOk then
        FLastError := ABLLServer.MethodError;
    end);

  Result := vDeleteOk;
end;

end.
