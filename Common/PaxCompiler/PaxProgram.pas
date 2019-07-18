////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PaxProgram.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PaxProgram;
interface
uses
  Classes,
  PAXCOMP_BASERUNNER,
  PAXCOMP_PROG,
  PAXCOMP_EMIT,
  PaxRunner;
type
  TPaxProgram = class(TPaxRunner)
  protected
    function GetRunnerClass: TBaseRunnerClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

constructor TPaxProgram.Create(AOwner: TComponent);
begin
  inherited;
  EmitProc := EmitProgProc;
end;

function TPaxProgram.GetRunnerClass: TBaseRunnerClass;
begin
  result := TProgram;
end;

end.
