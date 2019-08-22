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
  Vcl.ExtCtrls, frm_DataElement, CFPageControl, System.ImageList, Vcl.ImgList;

type
  TfrmInchDoctorStation = class(TForm)
    il: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FUserInfo: TUserInfo;
    FPatientPages: TCFPageControl;
    FOnFunctionNotify: TFunctionNotifyEvent;
    // PatientRecord
    function GetPatientPageIndexByPatID(const APatID: string): Integer;
    procedure AddPatListForm;
    procedure ClearPatientPages;
    procedure DoShowPatRecordForm(const APatInfo: TPatientInfo);
    procedure DoPageButtonClick(const APageIndex: Integer; const AButton: TCFPageButton);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    procedure OnMessage(AIFUN: IObjectFunction);
  end;

  procedure PluginShowInchDoctorStationForm(AIFun: IFunBLLFormShow);
  procedure PluginCloseInchDoctorStationForm;

var
  FrmInchDoctorStation: TfrmInchDoctorStation;
  PluginID: string;

implementation

uses
  PluginConst, FunctionConst, emr_BLLInvoke, frm_DoctorLevel,
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

procedure TfrmInchDoctorStation.ClearPatientPages;
var
  i: Integer;
begin
  for i := FPatientPages.Count - 1 downto 0 do
    (FPatientPages[i].Control as TForm).Free;

  FPatientPages.Clear;
end;

procedure TfrmInchDoctorStation.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  //Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TfrmInchDoctorStation.AddPatListForm;
var
  vFrmPatList: TfrmPatientList;
  vPage: TCFPage;
begin
  vFrmPatList := TfrmPatientList.Create(Self);
  vFrmPatList.BorderStyle := bsNone;
  vFrmPatList.Align := alClient;
  vFrmPatList.Parent := Self;
  vFrmPatList.UserInfo := FUserInfo;
  vFrmPatList.OnShowPatientRecord := DoShowPatRecordForm;
  vFrmPatList.Show;

  vPage := FPatientPages.AddPage('患者列表', vFrmPatList);
  vPage.ImageIndex := 0;
end;

procedure TfrmInchDoctorStation.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  ClearPatientPages;

  FOnFunctionNotify(PluginID, FUN_MAINFORMSHOW, nil);  // 显示主窗体
  FOnFunctionNotify(PluginID, FUN_BLLFORMDESTROY, nil);  // 释放业务窗体资源
end;

procedure TfrmInchDoctorStation.FormCreate(Sender: TObject);
begin
  PluginID := PLUGIN_INCHDOCTORSTATION;
  //SetWindowLong(Handle, GWL_EXSTYLE, (GetWindowLong(handle, GWL_EXSTYLE) or WS_EX_APPWINDOW));
  FPatientPages := TCFPageControl.Create(nil);
  FPatientPages.PageHeight := 28;
  FPatientPages.Images := il;
  FPatientPages.Align := alTop;
  FPatientPages.Parent := Self;
  FPatientPages.OnPageButtonClick := DoPageButtonClick;
end;

procedure TfrmInchDoctorStation.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPatientPages);
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

function TfrmInchDoctorStation.GetPatientPageIndexByPatID(
  const APatID: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FPatientPages.Count - 1 do
  begin
    if FPatientPages[i].Control is TfrmPatientRecord then
    begin
      if (FPatientPages[i].Control as TfrmPatientRecord).PatientInfo.PatID = APatID then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

procedure TfrmInchDoctorStation.OnMessage(AIFUN: IObjectFunction);
var
  vEventMessage: TEventMessage;
  Wnd: HWND;
  Control: TWinControl;
begin
  vEventMessage := TEventMessage(AIFUN.&Object);

  Wnd := GetCapture;
  if Wnd = 0 then
  begin
    Wnd := vEventMessage.Msg.hwnd;
    if (FPatientPages.PageIndex >= 0) and (Wnd = (FPatientPages.ActivePage.Control as TForm).ClientHandle) then
      Control := FPatientPages.ActivePage.Control as TForm
    else
    begin
      Control := FindControl(Wnd);
      while Control = nil do
      begin
        Wnd := GetParent(Wnd);
        if Wnd <> 0 then
          Control := FindControl(Wnd)
        else
          Break;
      end;
    end;
    if Control <> nil then
      vEventMessage.Handled := Control.PreProcessMessage(vEventMessage.Msg);
  end;

  {if not vEventMessage.Handled then
  begin
    TranslateMessage(vEventMessage.Msg);
    DispatchMessage(vEventMessage.Msg);
  end;}
end;

procedure TfrmInchDoctorStation.DoPageButtonClick(const APageIndex: Integer;
  const AButton: TCFPageButton);
begin
  if (FPatientPages[APageIndex].Control is TfrmPatientRecord) then
  begin
    if (FPatientPages[APageIndex].Control as TfrmPatientRecord).CloseAllRecordPage then
    begin
      (FPatientPages[APageIndex].Control as TfrmPatientRecord).Free;
      FPatientPages[APageIndex].Control := nil;
      FPatientPages.DeletePage(APageIndex);
    end;
  end;
end;

procedure TfrmInchDoctorStation.DoShowPatRecordForm(const APatInfo: TPatientInfo);
var
  vIndex: Integer;
  vFrmPatRec: TfrmPatientRecord;
  vPage: TCFPage;
  vPageButton: TCFPageButton;
begin
  vIndex := GetPatientPageIndexByPatID(APatInfo.PatID);
  if vIndex < 0 then
  begin
    vFrmPatRec := TfrmPatientRecord.Create(nil);
    vFrmPatRec.BorderStyle := bsNone;
    vFrmPatRec.Align := alClient;
    vFrmPatRec.Parent := Self;
    vFrmPatRec.UserInfo := FUserInfo;
    vFrmPatRec.PatientInfo.Assign(APatInfo);
    vFrmPatRec.Show;

    vPage := FPatientPages.AddPage(APatInfo.Name + ' ' + APatInfo.BedNo, vFrmPatRec);
    vPage.ImageIndex := 1;
    vPageButton := vPage.AddButton;
    vPageButton.ImageIndex := 3;
    vPageButton.HotImageIndex := 4;
    vPageButton.DownImageIndex := 5;
  end
  else
    FPatientPages.PageIndex := vIndex;
end;

end.
