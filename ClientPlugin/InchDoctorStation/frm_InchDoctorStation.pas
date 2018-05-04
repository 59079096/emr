{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_InchDoctorStation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FunctionIntf, FunctionImp, emr_Common, StdCtrls, Vcl.Grids, Vcl.Menus,
  System.Generics.Collections, frm_PatientRecord, frm_PatientList, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.AppEvnts;

type
  TfrmInchDoctorStation = class(TForm)
    pnlBar: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FUserInfo: TUserInfo;
    FFrmPatientList: TfrmPatientList;
    FPatientRecords: TObjectList<TfrmPatientRecord>;
    FOnFunctionNotify: TFunctionNotifyEvent;

    procedure DoRecordEditCloseForm(Sender: TObject);
    function GetPatientRecordIndex(const APatID: Integer): Integer;
    function GetPatientRecordFormIndex(const AFormHandle: Integer): Integer;
    function GetToolButtonIndex(const ATag: Integer): Integer;
    procedure AddPatientListForm;
    procedure DoSpeedButtonClick(Sender: TObject);
    procedure AddFormButton(const ACaption: string; const ATag: Integer);
    procedure DoShowPatientRecord(const APatInfo: TPatientInfo);
  public
    { Public declarations }
  end;

  procedure PluginShowInchDoctorStationForm(AIFun: IFunBLLFormShow);
  procedure PluginCloseInchDoctorStationForm;

var
  FrmInchDoctorStation: TfrmInchDoctorStation;
  PluginID: string;

implementation

uses
  PluginConst, FunctionConst, emr_BLLConst, emr_BLLServerProxy, frm_DoctorLevel,
  emr_MsgPack, emr_Entry, emr_PluginObject, FireDAC.Comp.Client;

{$R *.dfm}

procedure PluginShowInchDoctorStationForm(AIFun: IFunBLLFormShow);
begin
  if FrmInchDoctorStation = nil then
    FrmInchDoctorStation := TfrmInchDoctorStation.Create(nil);

  FrmInchDoctorStation.FOnFunctionNotify := AIFun.OnNotifyEvent;
  FrmInchDoctorStation.Show;
end;

procedure PluginCloseInchDoctorStationForm;
begin
  if FrmInchDoctorStation <> nil then
    FreeAndNil(FrmInchDoctorStation);
end;

procedure TfrmInchDoctorStation.AddFormButton(const ACaption: string;
  const ATag: Integer);
var
  vBtn: TSpeedButton;
begin
  if GetToolButtonIndex(ATag) < 0 then
  begin
    vBtn := TSpeedButton.Create(Self);
    vBtn.Width := 64;
    vBtn.Align := alLeft;
    vBtn.Tag := ATag;
    vBtn.GroupIndex := 1;
    vBtn.Flat := True;
    //vBtn.Down := True;
    vBtn.Caption := ACaption;
    vBtn.OnClick := DoSpeedButtonClick;
    vBtn.Parent := pnlBar;
  end;
end;

procedure TfrmInchDoctorStation.AddPatientListForm;
begin
  if FFrmPatientList = nil then
  begin
    FFrmPatientList := TfrmPatientList.Create(Self);
    FFrmPatientList.BorderStyle := bsNone;
    FFrmPatientList.Align := alClient;
    FFrmPatientList.Parent := Self;
    FFrmPatientList.UserInfo := FUserInfo;
    FFrmPatientList.OnShowPatientRecord := DoShowPatientRecord;
    FFrmPatientList.Show;

    AddFormButton('患者列表', FFrmPatientList.Handle);
  end;
end;

procedure TfrmInchDoctorStation.DoSpeedButtonClick(Sender: TObject);
var
  i, vIndex: Integer;
begin
  vIndex := GetPatientRecordFormIndex((Sender as TSpeedButton).Tag);
  if vIndex >= 0 then
  begin
    for i := 0 to pnlBar.ControlCount - 1 do
    begin
      if pnlBar.Controls[i] is TSpeedButton then
        (Sender as TSpeedButton).Down := False;
    end;

    (Sender as TSpeedButton).Down := True;
    Self.Controls[vIndex].BringToFront;
  end;
end;

procedure TfrmInchDoctorStation.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FOnFunctionNotify(PluginID, FUN_BLLFORMDESTROY, nil);  // 释放业务窗体资源
  FOnFunctionNotify(PluginID, FUN_MAINFORMSHOW, nil);  // 显示主窗体
end;

procedure TfrmInchDoctorStation.FormCreate(Sender: TObject);
begin
  PluginID := PLUGIN_INCHDOCTORSTATION;
  GClientParam := TClientParam.Create;
  SetWindowLong(Handle, GWL_EXSTYLE, (GetWindowLong(handle, GWL_EXSTYLE) or WS_EX_APPWINDOW));
  FUserInfo := TUserInfo.Create;
  FPatientRecords := TObjectList<TfrmPatientRecord>.Create;
end;

procedure TfrmInchDoctorStation.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPatientRecords);
  FreeAndNil(FFrmPatientList);
  FreeAndNil(FUserInfo);
  FreeAndNil(GClientParam)
end;

procedure TfrmInchDoctorStation.FormShow(Sender: TObject);
var
  vServerInfo: IServerInfo;
  vUserInfo: IUserInfo;
begin
  // 业务服务端连接参数
  vServerInfo := TServerInfo.Create;
  FOnFunctionNotify(PluginID, FUN_BLLSERVERINFO, vServerInfo);
  GClientParam.BLLServerIP := vServerInfo.Host;
  GClientParam.BLLServerPort := vServerInfo.Port;
  FOnFunctionNotify(PluginID, FUN_MSGSERVERINFO, vServerInfo);
  GClientParam.MsgServerIP := vServerInfo.Host;
  GClientParam.MsgServerPort := vServerInfo.Port;

  // 当前登录用户ID
  vUserInfo := TUserInfoIntf.Create;
  FOnFunctionNotify(PluginID, FUN_USERINFO, vUserInfo);  // 获取主程序登录用户名
  FUserInfo.ID := vUserInfo.UserID;  // 赋值ID后会自动获取其他信息
  //
  FOnFunctionNotify(PluginID, FUN_MAINFORMHIDE, nil);  // 隐藏主窗体

  AddPatientListForm;  // 显示患者列表窗体
end;

function TfrmInchDoctorStation.GetPatientRecordFormIndex(
  const AFormHandle: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Self.ControlCount - 1 do
  begin
    if (Self.Controls[i] is TForm) then
    begin
      if (Self.Controls[i] as TForm).Handle = AFormHandle then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

function TfrmInchDoctorStation.GetPatientRecordIndex(
  const APatID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FPatientRecords.Count - 1 do
  begin
    if FPatientRecords[i].PatientInfo.PatID = APatID then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TfrmInchDoctorStation.GetToolButtonIndex(const ATag: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to pnlBar.ControlCount - 1 do
  begin
    if pnlBar.Controls[i] is TSpeedButton then
    begin
      if (pnlBar.Controls[i] as TSpeedButton).Tag = ATag then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;
end;

procedure TfrmInchDoctorStation.DoRecordEditCloseForm(Sender: TObject);
var
  vIndex: Integer;
  i: Integer;
begin
  vIndex := FPatientRecords.IndexOf(Sender as TfrmPatientRecord);
  if vIndex >= 0 then
  begin
    for i := 0 to pnlBar.ControlCount - 1 do
    begin
      if pnlBar.Controls[i] is TSpeedButton then
      begin
        if (pnlBar.Controls[i] as TSpeedButton).Tag = FPatientRecords[vIndex].Handle then
        begin
          pnlBar.Controls[i].Free;
          Break;
        end;
      end;
    end;
    FPatientRecords.Delete(vIndex);
  end;
end;

procedure TfrmInchDoctorStation.DoShowPatientRecord(const APatInfo: TPatientInfo);
var
  vIndex: Integer;
  vFrmPatientRecord: TfrmPatientRecord;
begin
  vIndex := GetPatientRecordIndex(APatInfo.PatID);
  if vIndex < 0 then
  begin
    vFrmPatientRecord := TfrmPatientRecord.Create(nil);
    vFrmPatientRecord.BorderStyle := bsNone;
    vFrmPatientRecord.Align := alClient;
    vFrmPatientRecord.Parent := Self;
    vFrmPatientRecord.UserInfo := FUserInfo;
    vFrmPatientRecord.OnCloseForm := DoRecordEditCloseForm;
    vFrmPatientRecord.PatientInfo.Assign(APatInfo);

    FPatientRecords.Add(vFrmPatientRecord);

    AddFormButton(APatInfo.NameEx + '-病历', vFrmPatientRecord.Handle);

    vFrmPatientRecord.Show;
  end
  else
    FPatientRecords[vIndex].BringToFront;
end;

end.
