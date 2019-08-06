library InchDoctorStation;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

{ 关闭RTTI反射机制减少EXE文件尺寸 }
{$IF CompilerVersion >= 21.0}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}

uses
  System.ShareMem,
  System.SysUtils,
  System.Classes,
  ExpFun_InchDoctorStation in 'ExpFun_InchDoctorStation.pas',
  frm_InchDoctorStation in 'frm_InchDoctorStation.pas' {frmInchDoctorStation},
  frm_PatientList in 'frm_PatientList.pas' {frmPatientList},
  frm_TemplateList in 'frm_TemplateList.pas' {frmTemplateList},
  frm_PatientRecord in 'frm_PatientRecord.pas' {frmPatientRecord},
  frm_Consultation in 'frm_Consultation.pas' {frmConsultation},
  frm_Transfer in 'frm_Transfer.pas' {frmTransfer},
  frm_RecordOverView in 'frm_RecordOverView.pas' {frmRecordOverView};

{$R *.res}

begin
end.
