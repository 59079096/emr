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
  System.Generics.Collections, frm_PatientRecord, frm_PatientList, Vcl.Buttons,
  Vcl.ExtCtrls, frm_DataElement;

type
  TfrmInchDoctorStation = class(TForm)
    mmMain: TMainMenu;
    mniN1: TMenuItem;
    mniN2: TMenuItem;
    mniPat: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mniN2Click(Sender: TObject);
  private
    { Private declarations }
    FActivePatRecIndex: Integer;
    FUserInfo: TUserInfo;
    FFrmPatList: TfrmPatientList;
    FfrmDataElement: TfrmDataElement;
    FPatRecordForms: TObjectList<TfrmPatientRecord>;
    FOnFunctionNotify: TFunctionNotifyEvent;
    // DataElement
    procedure DoInsertDataElementAsDE(const AIndex, AName: string);
    // PatientRecord
    function GetPatRecIndexByPatID(const APatID: string): Integer;
    function GetPatRecIndexByHandle(const AFormHandle: Integer): Integer;
    procedure AddPatListForm;
    procedure DoSpeedButtonClick(Sender: TObject);
    procedure AddPatRecMenuItem(const ACaption: string; const AHandle: THandle);
    procedure DoShowPatRecordForm(const APatInfo: TPatientInfo);
    procedure DoCloseChildForm(Sender: TObject; var Action: TCloseAction);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
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
  PluginConst, FunctionConst, emr_BLLServerProxy, frm_DoctorLevel,
  emr_MsgPack, emr_Entry, FireDAC.Comp.Client, frm_DM;

{$R *.dfm}

procedure PluginShowInchDoctorStationForm(AIFun: IFunBLLFormShow);
begin
  if FrmInchDoctorStation = nil then
    Application.CreateForm(TfrmInchDoctorStation, FrmInchDoctorStation);

  FrmInchDoctorStation.FOnFunctionNotify := AIFun.OnNotifyEvent;
  FrmInchDoctorStation.Show;
end;

procedure PluginCloseInchDoctorStationForm;
begin
  if FrmInchDoctorStation <> nil then
    FreeAndNil(FrmInchDoctorStation);
end;

procedure TfrmInchDoctorStation.AddPatRecMenuItem(const ACaption: string;
  const AHandle: THandle);
var
  i, vIndex: Integer;
  vMenuItem: TMenuItem;
begin
  vIndex := -1;
  for i := 0 to mniPat.Count - 1 do
  begin
    if mniPat.Items[i].Tag = AHandle then
    begin
      vIndex := i;
      Break;
    end;
  end;

  if vIndex < 0 then  // 目前没有
  begin
    vMenuItem := TMenuItem.Create(mniPat);
    vMenuItem.Tag := AHandle;
    vMenuItem.GroupIndex := 1;
    vMenuItem.Caption := ACaption;
    vMenuItem.OnClick := DoSpeedButtonClick;
    mniPat.Add(vMenuItem);
  end;
end;

procedure TfrmInchDoctorStation.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  //Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TfrmInchDoctorStation.AddPatListForm;
var
  vMenuItem: TMenuItem;
begin
  if not Assigned(FFrmPatList) then
  begin
    FFrmPatList := TfrmPatientList.Create(Self);
    //FFrmPatList.BorderStyle := bsNone;
    //FFrmPatList.Align := alClient;
    //FFrmPatList.Parent := Self;
    FFrmPatList.WindowState := wsMaximized;
    FFrmPatList.UserInfo := FUserInfo;
    FFrmPatList.OnShowPatientRecord := DoShowPatRecordForm;
    FFrmPatList.OnClose := DoCloseChildForm;
    FFrmPatList.FormStyle := fsMDIChild;
    //FFrmPatList.Show;

    AddPatRecMenuItem('患者列表', FFrmPatList.Handle);

    vMenuItem := TMenuItem.Create(mniPat);
    vMenuItem.Caption := '-';
    mniPat.Add(vMenuItem);
  end;
end;

procedure TfrmInchDoctorStation.DoSpeedButtonClick(Sender: TObject);
var
  i, vIndex, vHandle: Integer;
begin
  FActivePatRecIndex := -1;
  if Sender is TMenuItem then
    vHandle := (Sender as TMenuItem).Tag
  else
    vHandle := 0;

  vIndex := GetPatRecIndexByHandle(vHandle);
  if vIndex >= 0 then
  begin
    if Self.Controls[vIndex] is TfrmPatientRecord then
    begin
      FActivePatRecIndex := vIndex;

      if IsIconic(vHandle) then
        TfrmPatientRecord(Self.Controls[vIndex]).WindowState := wsNormal
      else
        Self.Controls[vIndex].BringToFront;
    end;
  end;
end;

procedure TfrmInchDoctorStation.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  i: Integer;
begin
  for i := FPatRecordForms.Count - 1 downto 0 do
    (FPatRecordForms[i] as TfrmPatientRecord).Close;

  FOnFunctionNotify(PluginID, FUN_MAINFORMSHOW, nil);  // 显示主窗体
  FOnFunctionNotify(PluginID, FUN_BLLFORMDESTROY, nil);  // 释放业务窗体资源
end;

procedure TfrmInchDoctorStation.FormCreate(Sender: TObject);
begin
  FActivePatRecIndex := -1;
  PluginID := PLUGIN_INCHDOCTORSTATION;
  //SetWindowLong(Handle, GWL_EXSTYLE, (GetWindowLong(handle, GWL_EXSTYLE) or WS_EX_APPWINDOW));
  FPatRecordForms := TObjectList<TfrmPatientRecord>.Create;
end;

procedure TfrmInchDoctorStation.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPatRecordForms);

  if Assigned(FFrmPatList) then
    FreeAndNil(FFrmPatList);

  if Assigned(FfrmDataElement) then
    FreeAndNil(FfrmDataElement);
end;

procedure TfrmInchDoctorStation.FormShow(Sender: TObject);
var
  vObjFun: IObjectFunction;
begin
  // 获取客户缓存对象
  vObjFun := TObjectFunction.Create;
  FOnFunctionNotify(PluginID, FUN_CLIENTCACHE, vObjFun);
  ClientCache := TClientCache(vObjFun.&Object);

  // 当前登录用户对象
  FOnFunctionNotify(PluginID, FUN_USERINFO, vObjFun);
  FUserInfo := TUserInfo(vObjFun.&Object);

  // 本地数据库操作对象
  FOnFunctionNotify(PluginID, FUN_LOCALDATAMODULE, vObjFun);
  dm := Tdm(vObjFun.&Object);

  FOnFunctionNotify(PluginID, FUN_MAINFORMHIDE, nil);  // 隐藏主窗体

  AddPatListForm;  // 显示患者列表窗体
end;

function TfrmInchDoctorStation.GetPatRecIndexByHandle(
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

function TfrmInchDoctorStation.GetPatRecIndexByPatID(
  const APatID: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FPatRecordForms.Count - 1 do
  begin
    if FPatRecordForms[i].PatientInfo.PatID = APatID then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TfrmInchDoctorStation.mniN2Click(Sender: TObject);
begin
  if not Assigned(FfrmDataElement) then
  begin
    FfrmDataElement := TfrmDataElement.Create(Self);
    FfrmDataElement.OnInsertAsDE := DoInsertDataElementAsDE;
  end;

  FfrmDataElement.Show;
end;

procedure TfrmInchDoctorStation.DoCloseChildForm(Sender: TObject;
  var Action: TCloseAction);
var
  vIndex: Integer;
  i: Integer;
begin
  if Sender is TfrmPatientRecord then
  begin
    vIndex := FPatRecordForms.IndexOf(Sender as TfrmPatientRecord);
    if vIndex >= 0 then
    begin
      for i := 0 to mniPat.Count - 1 do
      begin
        if mniPat[i].Tag = FPatRecordForms[vIndex].Handle then
        begin
          mniPat.Delete(i);
          Break;
        end;
      end;

      FPatRecordForms.Delete(vIndex);
    end;
  end
  else
  if Sender is TfrmPatientList then
    FreeAndNil(FFrmPatList)
  else
    Action := caFree;
end;

procedure TfrmInchDoctorStation.DoInsertDataElementAsDE(const AIndex, AName: string);
begin
  if FActivePatRecIndex >= 0 then
    FPatRecordForms[FActivePatRecIndex].InsertDataElementAsDE(AIndex, AName);
end;

procedure TfrmInchDoctorStation.DoShowPatRecordForm(const APatInfo: TPatientInfo);
var
  vIndex: Integer;
  vFrmPatRec: TfrmPatientRecord;
begin
  vIndex := GetPatRecIndexByPatID(APatInfo.PatID);
  if vIndex < 0 then
  begin
    vFrmPatRec := TfrmPatientRecord.Create(nil);
    //vFrmPatRec.BorderStyle := bsNone;
    //vFrmPatRec.Align := alClient;
    //vFrmPatRec.Parent := Self;
    vFrmPatRec.UserInfo := FUserInfo;
    vFrmPatRec.OnClose := DoCloseChildForm;
    vFrmPatRec.PatientInfo.Assign(APatInfo);

    vIndex := FPatRecordForms.Add(vFrmPatRec);

    AddPatRecMenuItem(APatInfo.Name + ', ' + APatInfo.BedNo + '床 ' + APatInfo.Sex + ' ' + APatInfo.InpNo, vFrmPatRec.Handle);

    vFrmPatRec.FormStyle := fsMDIChild;
    //vFrmPatRec.Show;
  end
  else
    FPatRecordForms[vIndex].BringToFront;

  FActivePatRecIndex := vIndex;
end;

end.
