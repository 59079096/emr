////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PaxBasicLanguage.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PaxBasicLanguage;

interface
uses {$I uses.def}
  Classes,
  PAXCOMP_PARSER,
  PAXCOMP_BASIC_PARSER,
  PaxRegister,
  PaxCompiler;

type
  TPaxBasicLanguage = class(TPaxCompilerLanguage)
  private
    function GetUseFWArrays: Boolean;
    procedure SetUseFWArrays(value: Boolean);
  protected
    function GetParser: TBaseParser; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetCallConv(CallConv: Integer); override;
    procedure SetDeclareCallConv(CallConv: Integer);
    function GetLanguageName: String; override;
  published
    property ExplicitOff;
    property CompleteBooleanEval;
    property UseFWArrays: Boolean read GetUseFWArrays write SetUseFWArrays;
  end;

implementation

constructor TPaxBasicLanguage.Create(AOwner: TComponent);
begin
  inherited;
  P := TBasicParser.Create;
  SetCallConv(_ccREGISTER);
end;

procedure TPaxBasicLanguage.SetCallConv(CallConv: Integer);
begin
  P.CallConv := CallConv;
end;

procedure TPaxBasicLanguage.SetDeclareCallConv(CallConv: Integer);
begin
  P.DeclareCallConv := CallConv;
end;

function TPaxBasicLanguage.GetParser: TBaseParser;
begin
  result := P;
end;

function TPaxBasicLanguage.GetLanguageName: String;
begin
  result := P.LanguageName;
end;

function TPaxBasicLanguage.GetUseFWArrays: Boolean;
begin
  result := P.UseFWArrays;
end;

procedure TPaxBasicLanguage.SetUseFWArrays(value: Boolean);
begin
  P.UseFWArrays := value;
end;


end.
