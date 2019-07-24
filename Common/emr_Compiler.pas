unit emr_Compiler;

interface

uses
  System.Classes, System.SysUtils, HCCompiler, PaxRegister, HCEmrElementItem,
  emr_Common;

type
  TEmrCompiler = class(THCCompiler)
  private
    TDePropTypeID, TDeItemTypeID, TPatientInfoTypeID, TRecordInfoTypeID: Integer;
  public
    constructor CreateByScriptType(AOwner: TComponent; const AScriptType: TScriptType = stpPascal); override;

    /// <summary> 注册 SetDeItemText 需要的类和要设置的字符串 </summary>
    procedure RegClassVariable(const ADeItem, APatientInfo, ARecordInfo, AText: Pointer);
  end;

implementation

function TDeItem_GetValue(Self: TDeItem; const Key: String): String;
begin
  Result := Self[Key];
end;

procedure TDeItem_SetValue(Self: TDeItem; const Key: string; const Value: string);
begin
  Self[Key] := Value;
end;

constructor TEmrCompiler.CreateByScriptType(AOwner: TComponent;
  const AScriptType: TScriptType);
var
  vH, i: Integer;
begin
  inherited CreateByScriptType(AOwner, AScriptType);

  vH := PaxRegister.RegisterNamespace(0, 'HCEmrElementItem');
  TDePropTypeID := PaxRegister.RegisterClassType(vH, TDeProp);

  i := FCompilerConverts.New('Index',
    '\image{0} \column{} const \column{}\style{+B}Index\style{-B}: string;  \color{' + ProposalCommColor + '}  // 唯一索引',
    'Index', 'HCEmrElementItem', 'TDeProp', nil);
  FCompilerConverts[i].Constant := True;
  PaxRegister.RegisterConstant(TDePropTypeID, 'Index', TDeProp.Index);

  i := FCompilerConverts.New('Unit',
    '\image{0} \column{} const \column{}\style{+B}Unit\style{-B}: string;  \color{' + ProposalCommColor + '}  // 单位',
    'Unit', 'HCEmrElementItem', 'TDeProp', nil);
  FCompilerConverts[i].Constant := True;
  PaxRegister.RegisterConstant(TDePropTypeID, 'Unit', TDeProp.&Unit);

  i := FCompilerConverts.New('CMV',
    '\image{0} \column{} const \column{}\style{+B}CMV\style{-B}: string;  \color{' + ProposalCommColor + '}  // 值域代码',
    'CMV', 'HCEmrElementItem', 'TDeProp', nil);
  FCompilerConverts[i].Constant := True;
  PaxRegister.RegisterConstant(TDePropTypeID, 'CMV', TDeProp.CMV);

  i := FCompilerConverts.New('CMVVCode',
    '\image{0} \column{} const \column{}\style{+B}CMVVCode\style{-B}: string;  \color{' + ProposalCommColor + '}  // 值代码',
    'CMVVCode', 'HCEmrElementItem', 'TDeProp', nil);
  FCompilerConverts[i].Constant := True;
  PaxRegister.RegisterConstant(TDePropTypeID, 'CMVVCode', TDeProp.CMVVCode);

//  AInsertList.Add('Index');
//  AItemList.Add('\image{0} \column{} property \column{}\style{+B}Index\style{-B}: string;  // 唯一索引');
//  AInsertList.Add('Code');
//  AItemList.Add('\image{0} \column{} property \column{}\style{+B}Code\style{-B}: string;  // 编码');
//  AInsertList.Add('Name');
//  AItemList.Add('\image{0} \column{} property \column{}\style{+B}Name\style{-B}: string;  // 名称');
//  AInsertList.Add('Frmtp');
//  AItemList.Add('\image{0} \column{} property \column{}\style{+B}Frmtp\style{-B}: string;  // 类别 单选、多选、数值、日期时间等');
//  AInsertList.Add('Unit');
//  AItemList.Add('\image{0} \column{} property \column{}\style{+B}Unit\style{-B}: string;  // 单位');
//  AInsertList.Add('CMV');
//  AItemList.Add('\image{0} \column{} property \column{}\style{+B}CMV\style{-B}: string;  // 受控词汇表(值域代码)');
//  AInsertList.Add('CMVVCode');
//  AItemList.Add('\image{0} \column{} property \column{}\style{+B}CMVVCode\style{-B}: string;  // 受控词汇表(值域代码) ');
//  AInsertList.Add('Trace');
//  AItemList.Add('\image{0} \column{} property \column{}\style{+B}Trace\style{-B}: string;  // 痕迹信息');

  // 注册数据元
  TDeItemTypeID := PaxRegister.RegisterClassType(vH, TDeItem);
  // property Values
  i := FCompilerConverts.New('function TDeItem_GetValue(const Key: String): string;',
    '\image{0} \column{} function \column{}\style{+B}TDeItem_GetValue\style{-B}(const Key: string): string;  \color{' + ProposalCommColor + '}  // ',
    'TDeItem_GetValue', 'HCEmrElementItem', 'TDeItem', @TDeItem_GetValue, True);
  PaxRegister.RegisterFakeHeader(TDeItemTypeID, FCompilerConverts[i].FullName, FCompilerConverts[i].Address);

  i := FCompilerConverts.New('procedure TDeItem_SetValue(const Key: string; const Value: string);',
    '\image{0} \column{} procedure \column{}\style{+B}TDeItem_SetValue\style{-B}(const Key: string; const Value: string);  \color{' + ProposalCommColor + '}  // ',
    'TDeItem_SetValue', 'HCEmrElementItem', 'TDeItem', @TDeItem_SetValue, True);
  PaxRegister.RegisterFakeHeader(TDeItemTypeID, FCompilerConverts[i].FullName, FCompilerConverts[i].Address);

  i := FCompilerConverts.New('property Values[const Key: string]: string read TDeItem_GetValue write TDeItem_SetValue; default',
    '\image{3} \column{} property \column{}\style{+B}Values[const Key: string]\style{-B}: string;  \color{' + ProposalCommColor + '}  // 获取指定属性的值',
    'Values['''']', 'HCEmrElementItem', 'TDeItem', nil);
  PaxRegister.RegisterProperty(TDeItemTypeID, FCompilerConverts[i].FullName);

  // 注册患者
  vH := PaxRegister.RegisterNamespace(0, 'emr_Common');
  TPatientInfoTypeID := PaxRegister.RegisterClassType(vH, TPatientInfo);
  TRecordInfoTypeID := PaxRegister.RegisterClassType(vH, TRecordInfo);
end;

procedure TEmrCompiler.RegClassVariable(const ADeItem, APatientInfo, ARecordInfo, AText: Pointer);
begin
  if FindRegisterVariable(TDeItemTypeID, 'DeItem') then Exit;

  Self.RegisterVariable(0, 'DeItem', TDeItemTypeID, ADeItem);  // 注册数据元
  FCompilerVariables.New(TDeItemTypeID, ADeItem, 'DeItem', '当前数据元');

  Self.RegisterVariable(0, 'PatientInfo', TPatientInfoTypeID, APatientInfo);  // 注册患者
  FCompilerVariables.New(TPatientInfoTypeID, APatientInfo, 'PatientInfo', '当前患者信息');

  Self.RegisterVariable(0, 'RecordInfo', TRecordInfoTypeID, ARecordInfo);  // 注册病历信息
  FCompilerVariables.New(TRecordInfoTypeID, ARecordInfo, 'RecordInfo', '当前病历信息');

  // 注册要设置的文本变量
  Self.RegisterVariable(0, 'Text', _typeSTRING, AText);
  FCompilerVariables.New(_typeSTRING, AText, 'Text', '当前数据将要设置的内容');
end;

end.
