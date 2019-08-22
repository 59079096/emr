library Template;

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
{$IFDEF not DEBUG}
  {$IF CompilerVersion >= 21.0}
    {$WEAKLINKRTTI ON}
    {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
  {$IFEND}
{$ENDIF}

uses
  System.ShareMem,
  System.SysUtils,
  System.Classes,
  ExpFun_Template in 'ExpFun_Template.pas',
  frm_Template in 'frm_Template.pas' {frmTemplate},
  frm_ItemContent in 'frm_ItemContent.pas' {frmItemContent},
  frm_TemplateInfo in 'frm_TemplateInfo.pas' {frmTemplateInfo},
  frm_Record in '..\..\Common\frm_Record.pas' {frmRecord},
  frm_DeInfo in 'frm_DeInfo.pas' {frmDeInfo},
  frm_DomainItem in 'frm_DomainItem.pas' {frmDomainItem},
  frm_Domain in 'frm_Domain.pas' {frmDomain},
  frm_DomainOper in 'frm_DomainOper.pas' {frmDomainOper};

{$R *.res}

begin
end.
