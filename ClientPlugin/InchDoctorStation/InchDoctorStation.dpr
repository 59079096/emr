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

uses
  SysUtils,
  Classes,
  ExpFun_InchDoctorStation in 'ExpFun_InchDoctorStation.pas',
  frm_PatientList in 'frm_PatientList.pas' {frmPatientList},
  frm_PatientRecord in 'frm_PatientRecord.pas' {frmPatientRecord},
  frm_TemplateList in 'frm_TemplateList.pas' {frmTemplateList},
  frm_RecordEdit in '..\..\Common\frm_RecordEdit.pas' {frmRecordEdit},
  frm_DoctorLevel in '..\..\Common\frm_DoctorLevel.pas' {frmDoctorLevel},
  frm_InchDoctorStation in 'frm_InchDoctorStation.pas' {frmInchDoctorStation};

{$E cpi}

//{$R *.res}

begin
end.
