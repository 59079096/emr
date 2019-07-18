{$I PaxCompiler.def}

unit IMPORT_Dialogs;
interface
uses
  SysUtils,
  Classes,
{$IFDEF DPULSAR}
  Winapi.Windows,
  Winapi.Messages,
  Winapi.CommDlg,
  Vcl.Printers,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Dialogs,
{$ELSE}
  CommDlg,
  Windows,
  Messages,
  Printers,
  Graphics,
  Controls,
  Forms,
  StdCtrls,
  Dialogs,
{$ENDIF}
  Variants,
  PaxRegister,
  PaxCompiler;
procedure Register_Dialogs;
implementation
function TOpenDialog_GetFileEditStyle(Self:TOpenDialog):TFileEditStyle;
begin
  result := Self.FileEditStyle;
end;
procedure TOpenDialog_PutFileEditStyle(Self:TOpenDialog;const Value: TFileEditStyle);
begin
  Self.FileEditStyle := Value;
end;
function TFindDialog_GetLeft(Self:TFindDialog):Integer;
begin
  result := Self.Left;
end;
procedure TFindDialog_PutLeft(Self:TFindDialog;const Value: Integer);
begin
  Self.Left := Value;
end;
function TFindDialog_GetPosition(Self:TFindDialog):TPoint;
begin
  result := Self.Position;
end;
procedure TFindDialog_PutPosition(Self:TFindDialog;const Value: TPoint);
begin
  Self.Position := Value;
end;
function TFindDialog_GetTop(Self:TFindDialog):Integer;
begin
  result := Self.Top;
end;
procedure TFindDialog_PutTop(Self:TFindDialog;const Value: Integer);
begin
  Self.Top := Value;
end;
procedure Register_Dialogs;
var G, H: Integer;
begin
  H := RegisterNamespace(0, 'Dialogs');
  RegisterConstant(H, 'MaxCustomColors', 16);
  // Begin of class TCommonDialog
  G := RegisterClassType(H, TCommonDialog);
  RegisterHeader(G, 
       'constructor Create(AOwner: TComponent); override;',
       @TCommonDialog.Create);
  RegisterHeader(G, 
       'destructor Destroy; override;',
       @TCommonDialog.Destroy);
  RegisterHeader(G, 
       'procedure DefaultHandler(var Message); override;',
       @TCommonDialog.DefaultHandler);
  // End of class TCommonDialog
  RegisterRTTIType(H, TypeInfo(TOpenOption));
  RegisterRTTIType(H, TypeInfo(TOpenOptions));
  RegisterRTTIType(H, TypeInfo(TOpenOptionEx));
  RegisterRTTIType(H, TypeInfo(TOpenOptionsEx));
  RegisterRTTIType(H, TypeInfo(TFileEditStyle));
  // Begin of class TOpenDialog
  G := RegisterClassType(H, TOpenDialog);
  RegisterHeader(G, 
       'constructor Create(AOwner: TComponent); override;',
       @TOpenDialog.Create);
  RegisterHeader(G, 
       'destructor Destroy; override;',
       @TOpenDialog.Destroy);
  RegisterHeader(G, 
       'function Execute: Boolean; override;',
       @TOpenDialog.Execute);
  RegisterFakeHeader(G,
       'function TOpenDialog_GetFileEditStyle:TFileEditStyle;',
       @TOpenDialog_GetFileEditStyle);
  RegisterFakeHeader(G,
       'procedure TOpenDialog_PutFileEditStyle(const Value: TFileEditStyle);',
       @TOpenDialog_PutFileEditStyle);
  RegisterProperty(G,
       'property FileEditStyle:TFileEditStyle read TOpenDialog_GetFileEditStyle write TOpenDialog_PutFileEditStyle;');
  // End of class TOpenDialog
  // Begin of class TSaveDialog
  G := RegisterClassType(H, TSaveDialog);
  RegisterHeader(G, 
       'function Execute: Boolean; override;',
       @TSaveDialog.Execute);
  RegisterHeader(G,
       'constructor Create(AOwner: TComponent); override;',
       @TSaveDialog.Create);
  // End of class TSaveDialog
  RegisterRTTIType(H, TypeInfo(TColorDialogOption));
  RegisterRTTIType(H, TypeInfo(TColorDialogOptions));
  // Begin of class TColorDialog
  G := RegisterClassType(H, TColorDialog);
  RegisterHeader(G, 
       'constructor Create(AOwner: TComponent); override;',
       @TColorDialog.Create);
  RegisterHeader(G, 
       'destructor Destroy; override;',
       @TColorDialog.Destroy);
  RegisterHeader(G, 
       'function Execute: Boolean; override;',
       @TColorDialog.Execute);
  // End of class TColorDialog
  RegisterRTTIType(H, TypeInfo(TFontDialogOption));
  RegisterRTTIType(H, TypeInfo(TFontDialogOptions));
  RegisterRTTIType(H, TypeInfo(TFontDialogDevice));
  // Begin of class TFontDialog
  G := RegisterClassType(H, TFontDialog);
  RegisterHeader(G, 
       'constructor Create(AOwner: TComponent); override;',
       @TFontDialog.Create);
  RegisterHeader(G, 
       'destructor Destroy; override;',
       @TFontDialog.Destroy);
  RegisterHeader(G, 
       'function Execute: Boolean; override;',
       @TFontDialog.Execute);
  // End of class TFontDialog
  // Begin of class TPrinterSetupDialog
  G := RegisterClassType(H, TPrinterSetupDialog);
  RegisterHeader(G, 
       'function Execute: Boolean; override;',
       @TPrinterSetupDialog.Execute);
  RegisterHeader(G,
       'constructor Create(AOwner: TComponent); override;',
       @TPrinterSetupDialog.Create);
  // End of class TPrinterSetupDialog
  RegisterRTTIType(H, TypeInfo(TPrintRange));
  RegisterRTTIType(H, TypeInfo(TPrintDialogOption));
  RegisterRTTIType(H, TypeInfo(TPrintDialogOptions));
  // Begin of class TPrintDialog
  G := RegisterClassType(H, TPrintDialog);
  RegisterHeader(G, 
       'function Execute: Boolean; override;',
       @TPrintDialog.Execute);
  RegisterHeader(G,
       'constructor Create(AOwner: TComponent); override;',
       @TPrintDialog.Create);
  // End of class TPrintDialog
  RegisterRTTIType(H, TypeInfo(TPageSetupDialogOption));
  RegisterRTTIType(H, TypeInfo(TPageSetupDialogOptions));
  RegisterRTTIType(H, TypeInfo(TPrinterKind));
  RegisterRTTIType(H, TypeInfo(TPageType));
  RegisterRTTIType(H, TypeInfo(TPageMeasureUnits));
  // Begin of class TPageSetupDialog
  G := RegisterClassType(H, TPageSetupDialog);
  RegisterHeader(G, 
       'constructor Create(AOwner: TComponent); override;',
       @TPageSetupDialog.Create);
  RegisterHeader(G, 
       'function Execute: Boolean; override;',
       @TPageSetupDialog.Execute);
  RegisterHeader(G, 
       'function GetDefaults: Boolean;',
       @TPageSetupDialog.GetDefaults);
  // End of class TPageSetupDialog
  RegisterRTTIType(H, TypeInfo(TFindOption));
  RegisterRTTIType(H, TypeInfo(TFindOptions));
  // Begin of class TFindDialog
  G := RegisterClassType(H, TFindDialog);
  RegisterHeader(G, 
       'constructor Create(AOwner: TComponent); override;',
       @TFindDialog.Create);
  RegisterHeader(G, 
       'destructor Destroy; override;',
       @TFindDialog.Destroy);
  RegisterHeader(G, 
       'procedure CloseDialog;',
       @TFindDialog.CloseDialog);
  RegisterHeader(G, 
       'function Execute: Boolean; override;',
       @TFindDialog.Execute);
  RegisterFakeHeader(G,
       'function TFindDialog_GetLeft:Integer;',
       @TFindDialog_GetLeft);
  RegisterFakeHeader(G,
       'procedure TFindDialog_PutLeft(const Value: Integer);',
       @TFindDialog_PutLeft);
  RegisterProperty(G,
       'property Left:Integer read TFindDialog_GetLeft write TFindDialog_PutLeft;');
  RegisterFakeHeader(G,
       'function TFindDialog_GetPosition:TPoint;',
       @TFindDialog_GetPosition);
  RegisterFakeHeader(G,
       'procedure TFindDialog_PutPosition(const Value: TPoint);',
       @TFindDialog_PutPosition);
  RegisterProperty(G,
       'property Position:TPoint read TFindDialog_GetPosition write TFindDialog_PutPosition;');
  RegisterFakeHeader(G,
       'function TFindDialog_GetTop:Integer;',
       @TFindDialog_GetTop);
  RegisterFakeHeader(G,
       'procedure TFindDialog_PutTop(const Value: Integer);',
       @TFindDialog_PutTop);
  RegisterProperty(G,
       'property Top:Integer read TFindDialog_GetTop write TFindDialog_PutTop;');
  // End of class TFindDialog
  // Begin of class TReplaceDialog
  G := RegisterClassType(H, TReplaceDialog);
  RegisterHeader(G,
       'constructor Create(AOwner: TComponent); override;',
       @TReplaceDialog.Create);
  // End of class TReplaceDialog
  RegisterRTTIType(H, TypeInfo(TMsgDlgType));
  RegisterRTTIType(H, TypeInfo(TMsgDlgBtn));
  RegisterRTTIType(H, TypeInfo(TMsgDlgButtons));
  RegisterHeader(H, 'function MessageDlg(const Msg: string; DlgType: TMsgDlgType;  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;', @MessageDlg);
  RegisterHeader(H, 'function MessageDlgPos(const Msg: string; DlgType: TMsgDlgType;  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer;', @MessageDlgPos);
  RegisterHeader(H, 'function MessageDlgPosHelp(const Msg: string; DlgType: TMsgDlgType;  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;  const HelpFileName: string): Integer;', @MessageDlgPosHelp);
  RegisterHeader(H, 'procedure ShowMessage(const Msg: string);', @ShowMessage);
  RegisterHeader(H, 'procedure ShowMessageFmt(const Msg: string; Params: array of const);', @ShowMessageFmt);
  RegisterHeader(H, 'procedure ShowMessagePos(const Msg: string; X, Y: Integer);', @ShowMessagePos);
  RegisterHeader(H, 'function InputBox(const ACaption, APrompt, ADefault: string): string;', @InputBox);
  RegisterHeader(H, 'function InputQuery(const ACaption, APrompt: string;  var Value: string): Boolean;', @InputQuery);
  RegisterHeader(H, 'function PromptForFileName(var AFileName: string; const AFilter: string = '''';  const ADefaultExt: string = ''''; const ATitle: string = '''';  const AInitialDir: string = ''''; SaveDialog: Boolean = False): Boolean;', @PromptForFileName);
end;
end.
