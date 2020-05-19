{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_RecordPop;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, HCEmrElementItem, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Grids,
  FireDAC.Comp.Client, FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Stan.Intf,
  FireDAC.Comp.UI;

type
  TTextNotifyEvent = procedure(const ADeItem: TDeItem; const AText: string; var ACancel: Boolean) of object;
  TStreamNotifyEvent = procedure(const ADeItem: TDeItem; const AStream: TStream) of object;
  TDeItemSetTextEvent = procedure(Sender: TObject; const ADeItem: TDeItem;
    var AText: string; var ACancel: Boolean) of object;

  TfrmRecordPop = class(TForm)
    pgPop: TPageControl;
    tsDomain: TTabSheet;
    btnDomainOk: TButton;
    tsNumber: TTabSheet;
    tsMemo: TTabSheet;
    tsDateTime: TTabSheet;
    sgdDomain: TStringGrid;
    pnl1: TPanel;
    lbl2: TLabel;
    edtSpliter: TEdit;
    pnl2: TPanel;
    edtvalue: TButtonedEdit;
    chkhideunit: TCheckBox;
    cbbUnit: TComboBox;
    btn1: TButton;
    btn3: TButton;
    btn4: TButton;
    btn5: TButton;
    btn6: TButton;
    btn7: TButton;
    btn8: TButton;
    btn9: TButton;
    btn0: TButton;
    btnDiv: TButton;
    btn2: TButton;
    btnAdd: TButton;
    btnDec: TButton;
    btnMul: TButton;
    btnCE: TButton;
    btnC: TButton;
    btnResult: TButton;
    btnDot: TButton;
    btnNumberOk: TButton;
    pnl3: TPanel;
    pnl4: TPanel;
    btnMemoOk: TButton;
    btnDateTimeOk: TButton;
    mmoMemo: TMemo;
    fdgxwtcrsr: TFDGUIxWaitCursor;
    pnlDate: TPanel;
    pnlTime: TPanel;
    dtpdate: TDateTimePicker;
    cbbdate: TComboBox;
    dtptime: TDateTimePicker;
    cbbtime: TComboBox;
    bvl1: TBevel;
    btnNow: TButton;
    pgQk: TPageControl;
    ts1: TTabSheet;
    btn35: TButton;
    btn36: TButton;
    btn37: TButton;
    btn38: TButton;
    btn42: TButton;
    btn41: TButton;
    btn40: TButton;
    btn39: TButton;
    btnRM: TButton;
    procedure btnDomainOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnDateTimeOkClick(Sender: TObject);
    procedure btnCEClick(Sender: TObject);
    procedure edtvalueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbbUnitCloseUp(Sender: TObject);
    procedure cbbUnitSelect(Sender: TObject);
    procedure btnCClick(Sender: TObject);
    procedure btnNumberOkClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btnDotClick(Sender: TObject);
    procedure btnResultClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btn35Click(Sender: TObject);
    procedure btnMemoOkClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sgdDomainDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnNowClick(Sender: TObject);
    procedure cbbdateChange(Sender: TObject);
    procedure cbbtimeChange(Sender: TObject);
    procedure btnRMClick(Sender: TObject);
    procedure sgdDomainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    // 计算器用
    FSnum1, FSnum2, FSmark, FOldUnit: string;
    FNum1, FNum2{, FMark}: Real;
    FFlag, FSign, FTemp, FTemplate: Boolean;
    FConCalcValue: Boolean;  // True 点击计算器键时在原有字符后增加 False清空原串后增加字符
    FMultSelect: Boolean;  // 多选模式
    //
    FFrmtp: string;
    FDeItem: TDeItem;
    FDBDomain: TFDMemTable;
    FOnSetActiveItemText: TTextNotifyEvent;
    FOnSetActiveItemExtra: TStreamNotifyEvent;
    procedure SetDeItemValue(const AValue: string; var ACancel: Boolean);
    procedure SetDeItemExtraValue(const ACVVID: string);

    procedure SetValueFocus;  // 点击完数据时，焦点返回到数值框
    procedure SetConCalcValue;  // 点击计算器数字键时 处理是否原值串后增加字符
    procedure PutCalcNumber(const ANum: Integer);
    procedure SetMultSelect(const Value: Boolean);
  protected
    //FMMTimerID: Cardinal;
    FPopupWindow: THandle;
    procedure PopupWndProc(var Message: TMessage); virtual;
    procedure RegPopupClass;
    procedure CreatePopupHandle;
    procedure Popup(X, Y: Integer);
    property MultSelect: Boolean read FMultSelect write SetMultSelect;
  public
    { Public declarations }
    procedure PopupDeCombobox(const ADeCombobox: TDeCombobox);
    function PopupDeItem(const ADeItem: TDeItem; const APopupPt: TPoint): Boolean;
    property OnSetActiveItemText: TTextNotifyEvent read FOnSetActiveItemText write FOnSetActiveItemText;
    property OnSetActiveItemExtra: TStreamNotifyEvent read FOnSetActiveItemExtra write FOnSetActiveItemExtra;
  end;

implementation

uses
  emr_Common, emr_BLLInvoke, Winapi.MMSystem, Data.DB;

const
  PopupClassName = 'EMR_PopupClassName';

{$R *.dfm}

function ConversionValueByUnit(const AValue, AOldUnit, ANewUnit: string): string;
var
  vOldUnit, vNewUnit: string;
  viValue{, vReValue}: Single;
begin
  Result := AValue;
  if not TryStrToFloat(AValue, viValue) then  // 值
    Exit;
  vOldUnit := LowerCase(AOldUnit);  // 原始单位
  vNewUnit := LowerCase(ANewUnit);  // 新单位
  if vOldUnit = 'mmhg' then  // 如果是mmHg  100mmHg=13.3kPa
  begin
    if vNewUnit = 'kpa' then
    begin
      viValue := viValue * 133.3 / 1000;
      Result := FormatFloat('#.#', viValue);
    end;
  end
  else
  if vOldUnit = 'kpa' then  // 如果是KPa
  begin
    if vNewUnit = 'mmhg' then
    begin
      viValue := viValue * 1000 / 133.3;
      Result := Format('%d', [Round(viValue)]);
    end;
  end
  else
  if vOldUnit = '℃' then  // 是摄氏  37℃ = 98.6℉
  begin
    if vNewUnit = '℉' then  // 转华氏
    begin
      viValue := (viValue * 9 / 5) + 32;
      Result := FormatFloat('#.#', viValue);
    end;
  end
  else
  if vOldUnit = '℉' then  // 是华氏
  begin
    if vNewUnit = '℃' then  // 转摄氏
    begin
      viValue := (viValue - 32) * 5 / 9;
      Result := FormatFloat('#.#', viValue);
    end;
  end;
end;

function Equal(a: double; m: string; b: double): double;
var
  r: double;
begin
  Result := 0;

  if (m = '+')then
    r := a + b
  else
  if (m = '-')then
    r := a - b
  else
  if (m = '*')then
    r := a * b
  else
  if (m = '/')then
    r := a / b ;

  Result := r;
end;

procedure TfrmRecordPop.btn1Click(Sender: TObject);
begin
  PutCalcNumber((Sender as TButton).Tag);
end;

procedure TfrmRecordPop.btn35Click(Sender: TObject);
begin
  edtValue.Text := (Sender as TButton).Tag.ToString;
  FTemp := False;
  FTemplate := True;
  FConCalcValue := True;
  SetValueFocus;
end;

procedure TfrmRecordPop.btnAddClick(Sender: TObject);
begin
  if FSign then
  begin
    FSnum2 := edtValue.Text;
    if FSnum2 <> '' then
      FNum2 := StrToFloat(edtValue.Text);

    if (FSmark = '/') and (FNum2 = 0) then
    begin
      //lblPop.Caption := '除数不能为0';
      FNum1 := 0;
      FSmark := '';
      FNum2 := 0;
    end
    else
      edtValue.Text := FloatToStr(equal(FNum1, FSmark, FNum2));

    FFlag := True;
  end;

  FSnum1 := edtValue.Text;
  if FSnum1 <> '' then
    FNum1 := StrToFloat(FSnum1)
  else
    FNum1 := 0;

  FSmark := (Sender as TButton).Caption;
  FFlag := False;
  FSign := True;
  FTemp := FSign;
  SetValueFocus;
end;

procedure TfrmRecordPop.btnCClick(Sender: TObject);
var
  vS: string;
begin
  vS := edtValue.Text;
  if FFlag then
    edtValue.Text := ''
  else
  begin
    vS := Copy(vS, 1, Length(vS) - 1);
    edtValue.Text := vS;
  end;
  SetValueFocus;
end;

procedure TfrmRecordPop.btnCEClick(Sender: TObject);
begin
  edtValue.Text := '';
  FNum1 := 0;
  FSmark := '';
  FNum2 := 0;
  FTemplate := False;
  SetValueFocus;
end;

procedure TfrmRecordPop.btnDateTimeOkClick(Sender: TObject);
var
  vText: string;
  vCancel: Boolean;
begin
  if FFrmtp = TDeFrmtp.Date then
  begin
    //FDeItem[TDeProp.PreFormat] := cbbdate.Text;
    //FDeItem[TDeProp.Raw] := DateToStr(dtpdate.Date);
    vText := FormatDateTime(cbbdate.Text, dtpdate.Date);
  end
  else
  if FFrmtp = TDeFrmtp.Time then
  begin
    //FDeItem[TDeProp.PreFormat] := cbbtime.Text;
    //FDeItem[TDeProp.Raw] := TimeToStr(dtptime.Time);
    vText := FormatDateTime(cbbtime.Text, dtptime.Time);
  end
  else
  if FFrmtp = TDeFrmtp.DateTime then
  begin
    dtpdate.Time := dtptime.Time;
    //FDeItem[TDeProp.PreFormat] := cbbdate.Text + ' ' + cbbtime.Text;
    //FDeItem[TDeProp.Raw] := DateTimeToStr(dtpdate.DateTime);
    vText := FormatDateTime(cbbdate.Text + ' ' + cbbtime.Text, dtpdate.DateTime);
  end;

  if vText <> '' then
  begin
    vCancel := False;
    SetDeItemValue(vText, vCancel);
    if not vCancel then
      Close;
  end;
end;

procedure TfrmRecordPop.btnDomainOkClick(Sender: TObject);
var
  vCancel: Boolean;
  vDID, vS: string;
  i: Integer;
begin
  if sgdDomain.Row > 0 then
  begin
    vCancel := False;
    if FMultSelect then  // 多选
    begin
      vS := '';
      vDID := '';
      for i := 0 to sgdDomain.RowCount - 1 do
      begin
        if sgdDomain.Cells[0, i] <> '' then
        begin
          vDID := vDID + sgdDomain.Cells[2, i] + ',';
          vS := vS + sgdDomain.Cells[1, i] + '；';
        end;
      end;

      if vS <> '' then
      begin
        Delete(vS, Length(vS), 1);
        Delete(vDID, Length(vDID), 1);
        FDeItem[TDeProp.CMVVCode] := vDID;
        SetDeItemValue(vS, vCancel);
      end;
    end
    else  // 单选
    begin
      FDeItem[TDeProp.CMVVCode] := sgdDomain.Cells[2, sgdDomain.Row];
      if sgdDomain.Cells[5, sgdDomain.Row] <> '' then  // 有扩展内容
        SetDeItemExtraValue(sgdDomain.Cells[3, sgdDomain.Row])
      else
        SetDeItemValue(sgdDomain.Cells[1, sgdDomain.Row], vCancel);
    end;

    if not vCancel then
      Close;
  end;
end;

procedure TfrmRecordPop.btnDotClick(Sender: TObject);
begin
  edtValue.Text := edtValue.Text + '.';
  SetValueFocus;
end;

procedure TfrmRecordPop.btnMemoOkClick(Sender: TObject);
var
  vCancel: Boolean;
begin
  if mmoMemo.Text <> '' then
  begin
    vCancel := False;
    SetDeItemValue(mmoMemo.Text, vCancel);
    if not vCancel then
      Close;
  end;
end;

procedure TfrmRecordPop.btnNowClick(Sender: TObject);
begin
  dtpdate.DateTime := Now;
  dtptime.DateTime := Now;
end;

procedure TfrmRecordPop.btnNumberOkClick(Sender: TObject);
var
  vText: string;
  vCancel: Boolean;
begin
  if edtvalue.Text <> '' then
  begin
    if chkhideunit.Checked then
      vText := edtValue.Text
    else
      vText := edtValue.Text + cbbUnit.Text;

    vCancel := False;
    SetDeItemValue(vText, vCancel);
    if not vCancel then
    begin
      FDeItem[TDeProp.&Unit] := cbbUnit.Text;
      if chkhideunit.Checked then
        FDeItem[TDeProp.HideUnit] := '1'
      else
        FDeItem.DeleteProperty(TDeProp.HideUnit);

      Close;
    end;
  end;
end;

procedure TfrmRecordPop.btnResultClick(Sender: TObject);
begin
  if FFlag then
    edtValue.Text := FloatToStr(Equal(StrToFloat(edtValue.Text), FSmark, FNum2))
  else
  begin
    if (FSmark = '') then
      //edtValue.text := edtValue.text
    else
    begin
      FSnum2 := edtValue.Text;
      if (FSnum2 <> '') then
        FNum2 := StrToFloat(edtValue.Text)
      else
        FNum2 := FNum1;

      if (FSmark = '/') and (FNum2 = 0) then
      begin
        //lblPop.Caption := '除数不能为0';
        FNum1 := 0;
        FSmark := '';
        FNum2 := 0
      end
      else
        edtValue.Text := FloatToStr(Equal(FNum1, FSmark, FNum2));

      FFlag := True;
      FSign := False;
      FTemp := True;
    end;
  end;

  SetValueFocus;
end;

procedure TfrmRecordPop.btnRMClick(Sender: TObject);
begin
  MultSelect := not FMultSelect;
end;

procedure TfrmRecordPop.cbbdateChange(Sender: TObject);
begin
  dtpdate.Format := cbbdate.Text;
end;

procedure TfrmRecordPop.cbbtimeChange(Sender: TObject);
begin
  dtptime.Format := cbbtime.Text;
end;

procedure TfrmRecordPop.cbbUnitCloseUp(Sender: TObject);
begin
  FOldUnit := cbbUnit.Text;
end;

procedure TfrmRecordPop.cbbUnitSelect(Sender: TObject);
begin
  if Trim(edtvalue.Text) <> '' then
    edtvalue.Text := ConversionValueByUnit(edtvalue.Text, FOldUnit, cbbUnit.Text);
end;

procedure TfrmRecordPop.CreatePopupHandle;
begin
  if not IsWindow(FPopupWindow) then  // 如果提示窗体没有创建
  begin
    FPopupWindow := CreateWindowEx(
        WS_EX_TOOLWINDOW {or WS_EX_TOPMOST},  // 顶层窗口
        PopupClassName,
        nil,
        WS_POPUP,  // 弹出式窗口,支持双击
        0, 0, 100, 100, 0, 0, HInstance, nil);
    SetWindowLong(FPopupWindow, GWL_WNDPROC, Longint(MakeObjectInstance(PopupWndProc)));  // 窗口函数替换为类方法
  end;
end;

procedure TfrmRecordPop.edtvalueKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    btnNumberOk.Click;
    Key := 0;
  end;
end;

procedure TfrmRecordPop.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  {if FMMTimerID > 0 then
  begin
    timeKillEvent(FMMTimerID);
    FMMTimerID := 0;
  end;}
  ShowWindow(FPopupWindow, SW_HIDE);
end;

procedure TfrmRecordPop.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FMultSelect := False;
  //FMMTimerID := 0;
  FPopupWindow := 0;
  RegPopupClass;
  CreatePopupHandle;
  Windows.SetParent(Handle, FPopupWindow);

  FDBDomain := TFDMemTable.Create(Self);

  for i := 0 to pgPop.PageCount - 1 do
    pgPop.Pages[i].TabVisible := False;  // 隐藏标签

  for i := 0 to pgQk.PageCount - 1 do
    pgQk.Pages[i].TabVisible := False;

  sgdDomain.RowCount := 1;
  sgdDomain.ColWidths[0] := 0;
  sgdDomain.ColWidths[1] := 110;
  sgdDomain.ColWidths[2] := 35;
  sgdDomain.ColWidths[3] := 25;
  sgdDomain.ColWidths[4] := 35;
  sgdDomain.ColWidths[5] := 35;

  sgdDomain.Cells[0, 0] := '';
  sgdDomain.Cells[1, 0] := '值';
  sgdDomain.Cells[2, 0] := '编码';
  sgdDomain.Cells[3, 0] := 'ID';
  sgdDomain.Cells[4, 0] := '拼音';
  sgdDomain.Cells[5, 0] := '扩展';
end;

procedure TfrmRecordPop.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TfrmRecordPop.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDBDomain);
  if IsWindow(FPopupWindow) then
    DestroyWindow(FPopupWindow);
end;

procedure TfrmRecordPop.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

{
  ---------------通过控件句柄获取控件实例--------------------------------------------
  ---------------原理详见 Classes.pas 单元，13045行 <Delphi7>------------------------
  ---------------原理详见 Classes.pas 单元，11613行 <Delphi2007>---------------------
  ---------------原理详见 Classes.pas 单元，13045行 <Delphi2010>---------------------
  ---------------原理详见 Classes.pas 单元，13512行 <DelphiXE>-----------------------
}
{function GetInstanceFromhWnd(const hWnd: Cardinal): TWinControl;
type
  PObjectInstance = ^TObjectInstance;

  TObjectInstance = packed record
    Code: Byte;            // 短跳转 $E8
    Offset: Integer;       // CalcJmpOffset(Instance, @Block^.Code);
    Next: PObjectInstance; // MainWndProc 地址
    Self: Pointer;         // 控件对象地址
  end;
var
  wc: PObjectInstance;
begin
  Result := nil;
  wc := Pointer(GetWindowLong(hWnd, GWL_WNDPROC));
  if wc <> nil then
    Result := wc.Self;
end;

procedure MMTimerProc(uTimerID, uMessage: UINT; dwUser, dw1,
  dw2: DWORD); stdcall;
var
  vfrmRecordPop: TfrmRecordPop;
  vActiveWindow: THandle;
  vProcessId: Cardinal;
begin
  vActiveWindow := GetActiveWindow;
  GetWindowThreadProcessId(vActiveWindow, vProcessId);

  if vProcessId <> GetCurrentProcessId then
  begin
    vfrmRecordPop := TfrmRecordPop(GetInstanceFromhWnd(dwUser));
    if vfrmRecordPop <> nil then
      vfrmRecordPop.Close;
  end;
end;}

procedure TfrmRecordPop.Popup(X, Y: Integer);
var
  vMonitor: TMonitor;
  //vMsg: TMsg;
begin
  vMonitor := Screen.MonitorFromPoint(Point(X, Y));

  if vMonitor <> nil then
  begin
    if X + Width > vMonitor.WorkareaRect.Right then
      X := vMonitor.WorkareaRect.Right - Width;
    if Y + Height > vMonitor.WorkareaRect.Bottom then
      Y := vMonitor.WorkareaRect.Bottom - Height;

    if X < vMonitor.WorkareaRect.Left then
      X := vMonitor.WorkareaRect.Left;
    if Y < vMonitor.WorkareaRect.Top then
      Y := vMonitor.WorkareaRect.Top;
  end
  else // Monitor is nil, use Screen object instead
  begin
    if X + Width > Screen.WorkareaRect.Right then
      X := Screen.WorkareaRect.Right - Width;
    if Y + Height > Screen.WorkareaRect.Bottom then
      Y := Screen.WorkareaRect.Bottom - Height;

    if X < Screen.WorkareaRect.Left then
      X := Screen.WorkareaRect.Left;
    if Y < Screen.WorkareaRect.Top then
      Y := Screen.WorkareaRect.Top;
  end;

  MoveWindow(FPopupWindow, X, Y, Width, Height, False);
  MoveWindow(Handle, 0, 0, Width, Height, False);
  ShowWindow(FPopupWindow, SW_SHOW);  //  SW_SHOWNOACTIVATE
  // 创建定时器
  {if FMMTimerID = 0 then
  begin
    FMMTimerID := timeSetEvent(
      100, // 以毫秒指定事件的周期
      1, // 以毫秒指定延时的精度，数值越小定时器事件分辨率越高。缺省值为1ms。
      @MMTimerProc, // 回调函数
      Handle, // 存放用户提供的回调数据
      //定时器事件类型
      TIME_PERIODIC  // 每隔uDelay毫秒周期性地产生事件
      or TIME_CALLBACK_FUNCTION);
  end;}

  {repeat
    PeekMessage(vMsg,0,0,0,PM_REMOVE);

    if Visible and (vMsg.message <> WM_QUIT) then
    begin
      TranslateMessage(vMsg);
      DispatchMessage(vmsg);
    end
    else
      Break;
  until Application.Terminated;}
end;

procedure TfrmRecordPop.PopupDeCombobox(const ADeCombobox: TDeCombobox);
var
  vCMV: Integer;
begin
  ADeCombobox.Items.Clear();
  vCMV := -1;
  TBLLInvoke.GetDeProperty(StrToInt(ADeCombobox[TDeProp.Index]),
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // 服务端方法返回执行成功
      begin
        ShowMessage(ABLLServer.MethodError);
        Exit;
      end;

      vCMV := ABLLServer.BackField('domainid').AsInteger;
    end);

  if vCMV > 0 then  // 有值域
  begin
    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)
      begin
        ABLLServerReady.Cmd := BLL_GETDOMAINITEM;  // 获取值域选项
        ABLLServerReady.ExecParam.I['domainid'] := vCMV;
        ABLLServerReady.BackDataSet := True;
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      begin
        if not ABLLServer.MethodRunOk then  // 服务端方法返回执行成功
        begin
          ShowMessage(ABLLServer.MethodError);
          Exit;
        end;

        if AMemTable <> nil then
        begin
          AMemTable.First;
          while not AMemTable.Eof do
          begin
            ADeCombobox.Items.Add(AMemTable.FieldByName('devalue').AsString);  // FieldByName('code')
            AMemTable.Next;
          end;
        end;
      end);
  end;
end;

function TfrmRecordPop.PopupDeItem(const ADeItem: TDeItem; const APopupPt: TPoint): Boolean;

  {$REGION 'IniDomainUI 显示值域'}
  procedure IniDomainUI;
  var
    vRow: Integer;
  begin
    sgdDomain.RowCount := FDBDomain.RecordCount + 1;

    vRow := 0;
    FDBDomain.First;
    while not FDBDomain.Eof do
    begin
      Inc(vRow);

      sgdDomain.Cells[0, vRow] := '';
      sgdDomain.Cells[1, vRow] := FDBDomain.FieldByName('devalue').AsString;
      sgdDomain.Cells[2, vRow] := FDBDomain.FieldByName('code').AsString;
      sgdDomain.Cells[3, vRow] := FDBDomain.FieldByName('id').AsString;
      sgdDomain.Cells[4, vRow] := FDBDomain.FieldByName('py').AsString;

      if FDBDomain.FieldByName('content').DataType = ftBlob then
      begin
        if (FDBDomain.FieldByName('content') as TBlobField).BlobSize > 0 then
          sgdDomain.Cells[5, vRow] := '...'
        else
          sgdDomain.Cells[5, vRow] := '';  // 设置数据元值时以非空作为有扩展的判断
      end
      else
        sgdDomain.Cells[5, vRow] := '';  // 设置数据元值时以非空作为有扩展的判断

      FDBDomain.Next;
    end;

    if sgdDomain.RowCount > 1 then
      sgdDomain.FixedRows := 1;
  end;
  {$ENDREGION}

var
  vDeUnit: string;
  vCMV: Integer;
  vDT: TDateTime;
begin
  Result := False;

  FFrmtp := '';
  FDeItem := ADeItem;

  TBLLInvoke.GetDeProperty(StrToInt(FDeItem[TDeProp.Index]),
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // 服务端方法返回执行成功
      begin
        ShowMessage(ABLLServer.MethodError);
        Exit;
      end;

      FFrmtp := ABLLServer.BackField('frmtp').AsString;
      vDeUnit := ABLLServer.BackField('deunit').AsString;
      vCMV := ABLLServer.BackField('domainid').AsInteger;
    end);

  // 根据类别展示窗体
  if FFrmtp = TDeFrmtp.Number then  // 数值
  begin
    FTemplate := False;
    FTemp := False;
    FConCalcValue := False;

    if ADeItem[TDeProp.&Unit] <> '' then
      edtvalue.Text := StringReplace(ADeItem.Text, ADeItem[TDeProp.&Unit], '', [rfReplaceAll, rfIgnoreCase])
    else
      edtvalue.Text := ADeItem.Text;

    if edtvalue.Text <> '' then
      edtvalue.SelectAll;

    cbbUnit.Clear;
    cbbUnit.Items.Delimiter := ',';
    cbbUnit.Items.StrictDelimiter := True;
    cbbUnit.Items.DelimitedText := vDeUnit;
    if cbbUnit.Items.Count > 0 then
    begin
      cbbUnit.ItemIndex := cbbUnit.Items.IndexOf(ADeItem[TDeProp.&Unit]);
      if cbbUnit.ItemIndex < 0 then
        cbbUnit.ItemIndex := 0;
    end
    else
      cbbUnit.Text := ADeItem[TDeProp.&Unit];

    chkhideunit.Checked := ADeItem[TDeProp.HideUnit] = '1';

    pgPop.ActivePageIndex := 1;
    Self.Width := 185;

    if ADeItem[TDeProp.Index] = '979' then  // 体温
    begin
      pgQk.ActivePageIndex := 0;
      Self.Height := 285;
    end
    else
    begin
      pgQk.ActivePageIndex := -1;
      Self.Height := 215;
    end;
  end
  else
  if (FFrmtp = TDeFrmtp.Date)
    or (FFrmtp = TDeFrmtp.Time)
    or (FFrmtp = TDeFrmtp.DateTime)
  then  // 日期时间
  begin
    pgPop.ActivePageIndex := 3;
    Self.Width := 260;
    Self.Height := 170;

    pnlDate.Visible := FFrmtp <> TDeFrmtp.Time;
    pnlTime.Visible := FFrmtp <> TDeFrmtp.Date;
  end
  else
  if (FFrmtp = TDeFrmtp.Radio) or (FFrmtp = TDeFrmtp.Multiselect) then  // 单、多选
  begin
    MultSelect := FFrmtp = TDeFrmtp.Multiselect;

    edtSpliter.Clear;

    if FDBDomain.Active then
      FDBDomain.EmptyDataSet;

    sgdDomain.RowCount := 1;
    pgPop.ActivePageIndex := 0;
    Self.Width := 290;
    Self.Height := 300;

    if vCMV > 0 then  // 有值域
    begin
      BLLServerExec(
        procedure(const ABLLServerReady: TBLLServerProxy)
        begin
          ABLLServerReady.Cmd := BLL_GETDOMAINITEM;  // 获取值域选项
          ABLLServerReady.ExecParam.I['domainid'] := vCMV;
          ABLLServerReady.BackDataSet := True;
        end,
        procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
        begin
          if not ABLLServer.MethodRunOk then  // 服务端方法返回执行成功
          begin
            ShowMessage(ABLLServer.MethodError);
            Exit;
          end;

          if AMemTable <> nil then
            FDBDomain.CloneCursor(AMemTable, True);
        end);
    end;

    if FDBDomain.Active then  // 有选项
      IniDomainUI
    else  // 没选项
      Exit(False);
  end
  else
  if FFrmtp = TDeFrmtp.String then
  begin
    Exit(False);  // 文本的不弹了，使用直接在元素上修改的方式
    mmoMemo.Clear;
    pgPop.ActivePageIndex := 2;
    Self.Width := 260;
    Self.Height := 200;
  end
  else
    Exit(False);  // 不认识的类型不弹

  if not Visible then
    Visible := True;;

  Popup(APopupPt.X, APopupPt.Y);

  if FFrmtp = TDeFrmtp.Number then  // 数值
    edtvalue.SetFocus;

  Result := True;
end;

procedure TfrmRecordPop.PopupWndProc(var Message: TMessage);
begin
  if Message.Msg = WM_ACTIVATEAPP then
  begin
    if Message.WParam = 0 then  // 显示
      Close;
  end;
  Message.Result := DefWindowProc(FPopupWindow, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TfrmRecordPop.PutCalcNumber(const ANum: Integer);
begin
  if FTemplate then
  begin
    if ANum <> 0 then
      edtvalue.Text := edtvalue.Text + '.' + ANum.ToString
  end
  else
  begin
    SetConCalcValue;
    if FTemp or FFlag then
    begin
      edtValue.Text := ANum.ToString;
      FTemp := False;
    end
    else
      edtValue.Text := edtValue.text + ANum.ToString;
  end;

  SetValueFocus;
end;

procedure TfrmRecordPop.RegPopupClass;
var
  vWndCls: TWndClassEx;
  vClassName: string;
begin
  vClassName := PopupClassName;
  if not GetClassInfoEx(HInstance, PChar(vClassName), vWndCls) then
  begin
    vWndCls.cbSize        := SizeOf(TWndClassEx);
    vWndCls.lpszClassName := PChar(vClassName);
    vWndCls.style         := CS_VREDRAW or CS_HREDRAW
      or CS_DROPSHADOW or CS_DBLCLKS;  // 通过此样式实现窗口边框阴影效果，只能在注册窗口类时使用此属性，注册后可通过SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) or CS_DROPSHADOW);再增加

    vWndCls.hInstance     := HInstance;
    vWndCls.lpfnWndProc   := @DefWindowProc;
    vWndCls.cbClsExtra    := 0;
    vWndCls.cbWndExtra    := SizeOf(DWord) * 2;
    vWndCls.hIcon         := LoadIcon(hInstance,MakeIntResource('MAINICON'));
    vWndCls.hIconSm       := LoadIcon(hInstance,MakeIntResource('MAINICON'));
    vWndCls.hCursor       := LoadCursor(0, IDC_ARROW);
    vWndCls.hbrBackground := GetStockObject(white_Brush);
    vWndCls.lpszMenuName  := nil;

    if RegisterClassEx(vWndCls) = 0 then
    begin
      //MessageBox(0, '注册TCustomPopup错误!', 'TCustomPopup', MB_OK);
      raise Exception.Create('异常：注册TCustomPopup错误!');
      Exit;
    end;
  end;
end;

procedure TfrmRecordPop.SetConCalcValue;
begin
  if not FConCalcValue then
  begin
    edtValue.Text := '';
    FConCalcValue := True;
  end;
end;

procedure TfrmRecordPop.SetDeItemExtraValue(const ACVVID: string);
var
  vStream: TMemoryStream;
begin
  if Assigned(FOnSetActiveItemExtra) then
  begin
    vStream := TMemoryStream.Create;
    try
      if FDBDomain.Locate('id', ACVVID) then
      begin
        (FDBDomain.FieldByName('content') as TBlobField).SaveToStream(vStream);
        vStream.Position := 0;
        FOnSetActiveItemExtra(FDeItem, vStream);  // 除内容外，其他属性变化不用调用此方法
      end;
    finally
      FreeAndNil(vStream);
    end;
  end;
end;

procedure TfrmRecordPop.SetDeItemValue(const AValue: string; var ACancel: Boolean);
begin
  if Assigned(FOnSetActiveItemText) then
    FOnSetActiveItemText(FDeItem, AValue, ACancel);  // 除内容外，其他属性变化不用调用此方法
end;

procedure TfrmRecordPop.SetMultSelect(const Value: Boolean);
begin
  if FMultSelect <> Value then
  begin
    FMultSelect := Value;
    if FMultSelect then
    begin
      sgdDomain.ColWidths[0] := 20;
      btnRM.Caption := '单选';
    end
    else
    begin
      sgdDomain.ColWidths[0] := 0;
      btnRM.Caption := '多选';
    end;
  end;
end;

procedure TfrmRecordPop.SetValueFocus;
begin
  edtValue.SetFocus;
  edtvalue.SelStart := Length(edtvalue.Text);
  edtvalue.SelLength := 0;
end;

procedure TfrmRecordPop.sgdDomainDblClick(Sender: TObject);
begin
  btnDomainOkClick(Sender);
end;

procedure TfrmRecordPop.sgdDomainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vCol, vRow: Integer;
begin
  if FMultSelect then
  begin
    sgdDomain.MouseToCell(X, Y, vCol, vRow);
    if (vRow > 0) and (vCol = 0) then
    begin
      if sgdDomain.Cells[vCol, vRow] <> '' then
        sgdDomain.Cells[vCol, vRow] := ''
      else
        sgdDomain.Cells[vCol, vRow] := '√';
    end;
  end;
end;

end.
