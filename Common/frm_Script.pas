unit frm_Script;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, HCSynEdit, SynEdit,
  SynHighlighterPas, SynHighlighterJScript, SynCompletionProposal;

type
  TProposalEvent = procedure(const AWord: string; const AInsertList, AItemList: TStrings) of object;

  TfrmScript = class(TForm)
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FProposal: TSynCompletionProposal;  // 代码提示
    FOnProposal: TProposalEvent;

    FPascalSyn: TSynPasSyn;  // Pascal高亮
    //FJScript: TSynJScriptSyn;  // JavaScript高亮
    FSynEdit: THCSynEdit;
    procedure ChangeHighlighter;
    procedure DoEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure DoProposal(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: UnicodeString; var x, y: Integer; var CanExecute: Boolean);
  public
    { Public declarations }
    property SynEdit: THCSynEdit read FSynEdit;
    property OnProposal: TProposalEvent read FOnProposal write FOnProposal;
  end;

implementation

const
  ModifiedStrs: array[boolean] of string = ('', 'Modified');
  InsertModeStrs: array[boolean] of string = ('Overwrite', 'Insert');

{$R *.dfm}

procedure TfrmScript.ChangeHighlighter;
var
  vHighlighterFile: string;
begin
  vHighlighterFile := ExtractFilePath(ParamStr(0)) + FSynEdit.Highlighter.LanguageName + '.ini';
  if FileExists(vHighlighterFile) then
    FSynEdit.Highlighter.LoadFromFile(vHighlighterFile);
end;

procedure TfrmScript.DoEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  vCoord: TBufferCoord;
begin
  if Changes * [scAll, scCaretX, scCaretY] <> [] then
  begin
    vCoord := FSynEdit.CaretXY;
    Statusbar.Panels[0].Text := Format('%6d:%3d', [vCoord.Line, vCoord.Char]);
  end;

  if Changes * [scAll, scInsertMode, scReadOnly] <> [] then
  begin
    if FSynEdit.ReadOnly then
      Statusbar.Panels[2].Text := 'ReadOnly'
    else
      Statusbar.Panels[2].Text := InsertModeStrs[FSynEdit.InsertMode];
  end;

  if Changes * [scAll, scModified] <> [] then
    Statusbar.Panels[1].Text := ModifiedStrs[FSynEdit.Modified];
end;

procedure TfrmScript.DoProposal(Kind: SynCompletionType; Sender: TObject;
  var CurrentInput: UnicodeString; var x, y: Integer; var CanExecute: Boolean);
var
  vCurLine, vsLookup: string;
  vCoorX, vCoorTemp, vParenCounter, vStartX, vSavePos: Integer;
  vFoundMatch: Boolean;
begin
  // 代码提示弹出时
  if not Assigned(FOnProposal) then Exit;

  vCurLine := FSynEdit.LineText;

  vCoorX := FSynEdit.CaretX;
  if vCoorX > length(vCurLine) then
    vCoorX := length(vCurLine)
  else
    Dec(vCoorX);

  vFoundMatch := False;
  vCoorTemp := 0;

  while (vCoorX > 0) and not(vFoundMatch) do
  begin
    if vCurLine[vCoorX] = '.' then
    begin
      //we have a valid open paren, lets see what the word before it is
      vStartX := vCoorX;
      while (vCoorX > 0) and not FSynEdit.IsIdentChar(vCurLine[vCoorX]) do
        Dec(vCoorX);

      if vCoorX > 0 then
      begin
        vSavePos := vCoorX;
        While (vCoorX > 0) and FSynEdit.IsIdentChar(vCurLine[vCoorX]) do
          Dec(vCoorX);

        Inc(vCoorX);
        vsLookup := Uppercase(Copy(vCurLine, vCoorX, vSavePos - vCoorX + 1));
        vFoundMatch := True;
        Break;
        {vFoundMatch := FProposal.LookupList.IndexOf(vsLookup) > -1;
        if vsLookup = 'SELF' then
        begin
          vFoundMatch := True;
          Break;
        end;

        if not(vFoundMatch) then
        begin
          vCoorX := vStartX;
          Dec(vCoorX);
        end;}
      end
      else
      begin
        vsLookup := '.';
        vFoundMatch := True;
        Break;
      end;
    end
    else
    if vCurLine[vCoorX] = ',' then
    begin
      Inc(vCoorTemp);
      Dec(vCoorX);
    end
    else
    if vCurLine[vCoorX] = ')' then
    begin
      //We found a close, go till it's opening paren
      vParenCounter := 1;
      Dec(vCoorX);
      while (vCoorX > 0) and (vParenCounter > 0) do
      begin
        if vCurLine[vCoorX] = ')' then
          Inc(vParenCounter)
        else
        if vCurLine[vCoorX] = '(' then
          Dec(vParenCounter);

        Dec(vCoorX);
      end;

      if vCoorX > 0 then
        Dec(vCoorX);  //eat the open paren
    end
    else
    if vCurLine[vCoorX] = '(' then
    begin
      //we have a valid open paren, lets see what the word before it is
      vStartX := vCoorX;
      while (vCoorX > 0) and not FSynEdit.IsIdentChar(vCurLine[vCoorX]) do
        Dec(vCoorX);

      if vCoorX > 0 then
      begin
        vSavePos := vCoorX;
        While (vCoorX > 0) and FSynEdit.IsIdentChar(vCurLine[vCoorX]) do
          Dec(vCoorX);

        Inc(vCoorX);
        vsLookup := Uppercase(Copy(vCurLine, vCoorX, vSavePos - vCoorX + 1));
        vFoundMatch := True;
        Break;
        {vFoundMatch := LookupList.IndexOf(vsLookup) > -1;
        if not(vFoundMatch) then
        begin
          vCoorX := vStartX;
          Dec(vCoorX);
        end;}
      end;
    end
    else
      Dec(vCoorX)
  end;

  CanExecute := vFoundMatch;

  if CanExecute then
  begin
    TSynCompletionProposal(Sender).Form.CurrentIndex := vCoorTemp;
    //if vsLookup <> TSynCompletionProposal(Sender).PreviousToken then
    begin
      FProposal.InsertList.Clear;
      FProposal.ItemList.Clear;

      FOnProposal(vsLookup, FProposal.InsertList, FProposal.ItemList);
    end;
  end
  else
  begin
    FProposal.InsertList.Clear;
    FProposal.ItemList.Clear;
  end;
end;

procedure TfrmScript.FormCreate(Sender: TObject);
begin
  FPascalSyn := TSynPasSyn.Create(nil);
  //FJScript := TSynJScriptSyn.Create(nil);

  FSynEdit := THCSynEdit.Create(nil);
  FSynEdit.Gutter.ShowLineNumbers := True;
  FSynEdit.Highlighter := FPascalSyn;  // FJScript
  FSynEdit.Highlighter.UseUserSettings(2);
  //FSynEdit.Highlighter.SaveToFile('c:\a.ini');
  ChangeHighlighter;

  FSynEdit.Text := FPascalSyn.SampleSource;

  FSynEdit.OnStatusChange := DoEditStatusChange;
  FSynEdit.Align := alClient;
  FSynEdit.Parent := Self;

  FProposal := TSynCompletionProposal.Create(nil);
  FProposal.Editor := FSynEdit;
  FProposal.Title := '代码提示：';
  FProposal.TimerInterval := 500;
  FProposal.ShortCut := VK_SHIFT or VK_SPACE;
  FProposal.ClSelect := $00A56D53;
  FProposal.Font.Name := 'Tahoma';
  FProposal.Width := 600;
  FProposal.ItemHeight := 16;
  FProposal.Columns.Add.ColumnWidth := 80;
  //FProposal.Margin := 4;
  FProposal.Options := FProposal.Options + [scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer];
  FProposal.OnExecute := DoProposal;
end;

procedure TfrmScript.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSynEdit);
  FreeAndNil(FProposal);
  FreeAndNil(FPascalSyn);
  //FreeAndNil(FJScript);
end;

procedure TfrmScript.FormShow(Sender: TObject);
begin
  DoEditStatusChange(Self, [scAll]);
end;

end.
