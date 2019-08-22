unit CFGridTree;

interface

uses
  Graphics, CFGrid;

type
  TCFGroupRow = class(TCFRow)
  private
    FText: string;
  public
    property Text: string read FText write FText;
  end;

  TCFGridTree = class(TCFGrid)
  protected
    function DoRowDrawBefor(const ARow, ALeft, ATop: Integer; const ACanvas: TCanvas;
      var AUserDrawed: Boolean): Boolean; override;
  public
    procedure AddGroup(const AText: string);
  end;

implementation

{ TCFGridTree }

procedure TCFGridTree.AddGroup(const AText: string);
var
  vRow: TCFGroupRow;
begin
  vRow := TCFGroupRow.Create(Self, ColCount);
  vRow.Text := AText;
  FRows.Add(vRow);  // Ôö¼ÓÐÐ
end;

function TCFGridTree.DoRowDrawBefor(const ARow, ALeft, ATop: Integer;
  const ACanvas: TCanvas; var AUserDrawed: Boolean): Boolean;
var
  vRow: TCFGroupRow;
begin
  if FRows[ARow] is TCFGroupRow then
  begin
    AUserDrawed := True;

    vRow := FRows[ARow] as TCFGroupRow;
    ACanvas.TextOut(ALeft, ATop + 2, vRow.Text);
  end;
end;

end.
