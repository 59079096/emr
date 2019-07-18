////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PaxJavaScriptLanguage.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PaxJavaScriptLanguage;

interface
uses
  SysUtils,
  Classes,
  PAXCOMP_PARSER,
  PAXCOMP_JS_PARSER,
  PaxRegister,
  PaxCompiler;

type
  TPaxJavaScriptLanguage = class(TPaxCompilerLanguage)
  protected
    function GetParser: TBaseParser; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetCallConv(CallConv: Integer); override;
    function GetLanguageName: String; override;
  published
  end;

implementation

function TPaxJavaScriptLanguage.GetParser: TBaseParser;
begin
  result := P;
end;

function TPaxJavaScriptLanguage.GetLanguageName: String;
begin
  result := P.LanguageName;
end;

constructor TPaxJavaScriptLanguage.Create(AOwner: TComponent);
begin
  inherited;
  P := TJavaScriptParser.Create;
  SetCallConv(_ccSTDCALL);
end;

procedure TPaxJavaScriptLanguage.SetCallConv(CallConv: Integer);
begin
  if CallConv <> _ccSTDCALL then
    raise Exception.Create('Only STDCALL convention is allowed.');

  P.CallConv := CallConv;
end;

end.
