unit HCSynEdit;

interface

uses
  Windows, Classes, Graphics, SysUtils, SynEdit, SynHighlighterPas, SynEditHighlighter;

type
  THCSynEdit = class(TSynEdit)
  private
    FDestLine: Integer;
    FBracketFG: TColor;
    FBracketBG: TColor;
    function GetNearWord(const ALine: string; const AChar: Integer): string;
    procedure DoEditPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);
  protected
    procedure DoOnStatusChange(Changes: TSynStatusChanges); override;
    function DoOnSpecialLineColors(Line: Integer;
      var Foreground, Background: TColor): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetErrorLine(const ALine: Integer);
  end;

implementation

const
  BraketString = '([{)]}';
  OpenChars: array[0..2] of Char = ('(', '[', '{');
  CloseChars: array[0..2] of Char = (')', ']', '}');

{ THCSynEdit }

constructor THCSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBracketFG := clRed;
  FBracketBG := $0080DDFF;
  FDestLine := -1;
  Self.OnPaintTransient := DoEditPaintTransient;
end;

destructor THCSynEdit.Destroy;
begin

  inherited Destroy;
end;

procedure THCSynEdit.DoEditPaintTransient(Sender: TObject; Canvas: TCanvas;
  TransientType: TTransientType);

  function CharToPixels(P: TBufferCoord): TPoint;
  begin
    Result := Self.RowColumnToPixels(Self.BufferToDisplayPos(P));
  end;

  function IsCharBracket(AChar: WideChar): Boolean;
  begin
    case AChar of
      '{','[','(','<','}',']',')','>':
        Result := True;
    else
      Result := False;
    end;
  end;

var
  vTmpCharA, vTmpCharB: Char;
  vNearText: string;
  vCaretXY: TBufferCoord;
  vAttri: TSynHighlighterAttributes;

  procedure DrawBracket;
  var
    vPix: TPoint;
    i: Integer;
  begin
    vNearText := vTmpCharB;
    if not IsCharBracket(vTmpCharB) then
    begin
      vCaretXY.Char := vCaretXY.Char - 1;
      vNearText := vTmpCharA;
    end;

    Self.GetHighlighterAttriAtRowCol(vCaretXY, vNearText, vAttri);

    if (Self.Highlighter.SymbolAttribute = vAttri) then
    begin
      for i := low(OpenChars) to High(OpenChars) do
      begin
        if (vNearText = OpenChars[i]) or (vNearText = CloseChars[i]) then
        begin
          vPix := CharToPixels(vCaretXY);

          Canvas.Brush.Style := bsSolid;//Clear;
          Canvas.Font.Assign(Self.Font);
          Canvas.Font.Style := vAttri.Style;

          if (TransientType = ttAfter) then  // 切换到的
          begin
            Canvas.Font.Color := FBracketFG;
            Canvas.Brush.Color := FBracketBG;
          end
          else  // 切换走的
          begin
            Canvas.Font.Color := vAttri.Foreground;
            Canvas.Brush.Color := vAttri.Background;
          end;

          if Canvas.Font.Color = clNone then
            Canvas.Font.Color := Self.Font.Color;

          if Canvas.Brush.Color = clNone then
            Canvas.Brush.Color := Self.Color;

          Canvas.TextOut(vPix.X, vPix.Y, vNearText);
          vCaretXY := Self.GetMatchingBracketEx(vCaretXY);

          if (vCaretXY.Char > 0) and (vCaretXY.Line > 0) then
          begin
            vPix := CharToPixels(vCaretXY);

            if (vPix.X > Self.Gutter.Width) and (vPix.X < Self.Width)
              and (vPix.Y > 0) and (vPix.Y < Self.Height)  // 未约束滚动条
            then
            begin
              if (TransientType = ttAfter) then
              begin
                Canvas.Font.Color := FBracketFG;
                Canvas.Brush.Color := FBracketBG;
              end
              else
              begin
                Canvas.Font.Color := vAttri.Foreground;
                Canvas.Brush.Color := vAttri.Background;
              end;

              if Canvas.Font.Color = clNone then
                Canvas.Font.Color := Self.Font.Color;

              if Canvas.Brush.Color = clNone then
                Canvas.Brush.Color := Self.Color;

              if vNearText = OpenChars[i] then
                Canvas.TextOut(vPix.X, vPix.Y, CloseChars[i])
              else
                Canvas.TextOut(vPix.X, vPix.Y, OpenChars[i]);
            end;
          end;
        end;
      end;

      Canvas.Brush.Style := bsSolid;
    end;
  end;

var
  vLine, vOrgText: string;
  i, vFirstLine, vLastLine, vPos, vNearLen, vTokenType, vNearChar: Integer;
  vBufferCoord: TBufferCoord;
  vDisplayCoord: TDisplayCoord;
  vRect: TRect;
begin
  // [sfCaretChanged,sfLinesChanging,sfIgnoreNextChar]
  if sfLinesChanging in Self.StateFlags then Exit;

  vFirstLine := Self.SelStart;
  vLine := Self.Text;

  if (vFirstLine > 0) and (vFirstLine <= Length(vLine)) then
    vTmpCharA := vLine[vFirstLine]
  else
    vTmpCharA := #0;

  if (vFirstLine < Length(vLine)) then
    vTmpCharB := vLine[vFirstLine + 1]
  else
    vTmpCharB := #0;

  vCaretXY := CaretXY;

  if IsCharBracket(vTmpCharA) or IsCharBracket(vTmpCharB) then
  begin
    DrawBracket;
    Exit;
  end;

  vLine := Self.Lines[vCaretXY.Line - 1];
  if vLine = '' then Exit;

  vNearText := GetNearWord(vLine, vCaretXY.Char);
  //vCaretText := FSynEdit.GetWordAtRowCol(vCaretXY);
  {if vCaretText = '' then
  begin
    vNearXY := FSynEdit.PrevWordPos;
    vCaretText := FSynEdit.GetWordAtRowCol(vNearXY);
  end;

  if vCaretText = '' then
  begin
    vNearXY := FSynEdit.NextWordPos;
    vCaretText := FSynEdit.GetWordAtRowCol(vNearXY);
  end;}

  //vCaretText := FSynEdit.SelText;
  if (vNearText <> '') then  // 不为空
  begin
    if Self.Highlighter.IsKeyword(vNearText) then Exit;  // 关键字
    Self.GetHighlighterAttriAtRowColEx(vCaretXY, vOrgText, vTokenType, vPos, vAttri);
    if Self.Highlighter is TSynPasSyn then
    begin
      if vTokenType = Ord(TtkTokenKind.tkComment) then
        Exit;
    end;

    vFirstLine := Self.TopLine - 1;
    vLastLine := vFirstLine + Self.LinesInWindow + 1;
    if vLastLine > Self.Lines.Count - 1 then
      vLastLine := Self.Lines.Count - 1;

    Canvas.Brush.Color := FBracketBG;
    Canvas.Pen.Color := $00226DA8;
    vNearText := UpperCase(vNearText);
    for i := vFirstLine to vLastLine do
    begin
      vLine := UpperCase(Self.Lines[i]);
      vPos := Pos(vNearText, vLine);
      if vPos > 0 then
      begin
        vBufferCoord.Line := i + 1;
        vBufferCoord.Char := 0;
        vNearLen := Length(vNearText);

        while vPos > 0 do
        begin
          vBufferCoord.Char := vBufferCoord.Char + vPos;

          if (vPos > 1) and Self.IsIdentChar(vLine[vPos - 1]) then  // 前面还是字母

          else
          if (vPos < Length(vLine) - vNearLen) and Self.IsIdentChar(vLine[vPos + vNearLen]) then  // 后面还是字母

          else
          begin
            vNearChar := Self.RowColToCharIndex(vBufferCoord);

            if (vNearChar >= Self.SelStart) and (vNearChar <= Self.SelEnd) then  // 在选中中间

            else
            if Self.SelAvail and (Self.SelStart >= vNearChar) and (Self.SelStart < vNearChar + vNearLen) then  // 是选中一部分

            else
            if Self.SelAvail and (Self.SelEnd > vNearChar) and (Self.SelEnd <= vNearChar + vNearLen) then  // 是选中一部分

            else
            begin
              vDisplayCoord := Self.BufferToDisplayPos(vBufferCoord);
              vRect.Location := Self.RowColumnToPixels(vDisplayCoord);
              vRect.Width := Self.CharWidth * vNearLen;
              vRect.Height := Self.LineHeight;

              vOrgText := Copy(Self.Lines[i], vBufferCoord.Char, vNearLen);
              Canvas.TextOut(vRect.Left, vRect.Top, vOrgText);
              // 边框
              Canvas.MoveTo(vRect.Left, vRect.Top);
              Canvas.LineTo(vRect.Right, vRect.Top);
              Canvas.LineTo(vRect.Right, vRect.Bottom - 1);
              Canvas.LineTo(vRect.Left, vRect.Bottom - 1);
              Canvas.LineTo(vRect.Left, vRect.Top);
            end;
          end;

          vBufferCoord.Char := vBufferCoord.Char + vNearLen - 1;
          vLine := Copy(vLine, vPos + vNearLen, Length(vLine) - (vPos + vNearLen) + 1);
          vPos := Pos(vNearText, vLine);
        end;
      end;

      {vBufferCoord.Char := Pos(vNearText, vLine);
      if vBufferCoord.Char > 0 then
      begin
        if vCaretXY.Line = i + 1 then
          Continue;

        vBufferCoord.Line := i + 1;
        vDisplayCoord := FSynEdit.BufferToDisplayPos(vBufferCoord);
        vRect.Location := FSynEdit.RowColumnToPixels(vDisplayCoord);
        vRect.Width := FSynEdit.CharWidth * Length(vNearText);
        vRect.Height := FSynEdit.LineHeight;

        Canvas.TextOut(vRect.Left, vRect.Top, vNearText);
        // 边框
        Canvas.MoveTo(vRect.Left, vRect.Top);
        Canvas.LineTo(vRect.Right, vRect.Top);
        Canvas.LineTo(vRect.Right, vRect.Bottom - 1);
        Canvas.LineTo(vRect.Left, vRect.Bottom - 1);
        Canvas.LineTo(vRect.Left, vRect.Top);
      end;}
    end;
  end;
end;

function THCSynEdit.DoOnSpecialLineColors(Line: Integer; var Foreground,
  Background: TColor): Boolean;
begin
  Result := inherited DoOnSpecialLineColors(Line, Foreground, Background);
  if Line = FDestLine then
  begin
    Result := True;
    Background := clRed;
  end;
end;

procedure THCSynEdit.DoOnStatusChange(Changes: TSynStatusChanges);
begin
  if Changes * [scSelection, scCaretY] <> [] then  // 高亮或取消高亮视界内的选中内容
    Self.Invalidate;

  if Changes * [scCaretX, scCaretY] <> [] then
    FDestLine := -1;

  inherited DoOnStatusChange(Changes);
end;

function THCSynEdit.GetNearWord(const ALine: string;
  const AChar: Integer): string;
var
  vStart, vStop: Integer;
begin
  Result := '';
  if (Length(ALine) > 0) and ((AChar >= Low(ALine)) and (AChar <= High(ALine))) then
  begin
    if Self.IsIdentChar(ALine[AChar]) then
    begin
      vStart := AChar;
      while (vStart > Low(ALine)) and Self.IsIdentChar(ALine[vStart - 1]) do
        Dec(vStart);

      vStop := AChar + 1;
      while (vStop <= High(ALine)) and Self.IsIdentChar(ALine[vStop]) do
        Inc(vStop);

      Result := Copy(ALine, vStart, vStop - vStart);
    end
    else
    if AChar > 1 then
    begin
      vStart := AChar - 1;
      if not Self.IsIdentChar(ALine[vStart]) then Exit;

      while (vStart > Low(ALine)) and Self.IsIdentChar(ALine[vStart - 1]) do
        Dec(vStart);

      vStop := AChar;
      while (vStop < High(ALine)) and (Self.IsIdentChar(ALine[vStop])) do
        Inc(vStop);

      Result := Copy(ALine, vStart, vStop - vStart);
    end;
  end;
end;

procedure THCSynEdit.SetErrorLine(const ALine: Integer);
begin
  Self.GotoLineAndCenter(ALine);
  FDestLine := ALine;
  Self.InvalidateLine(FDestLine);
end;

end.
