unit frm_RecordOverView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CFControl, CFGrid,
  CFGridTree;

type
  TfrmRecordOverView = class(TForm)
    sgdRecordView: TCFGridTree;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TfrmRecordOverView.FormCreate(Sender: TObject);
begin
  sgdRecordView.AddGroup('第一组');
  sgdRecordView.RowCount := sgdRecordView.RowCount + 4;
  sgdRecordView.AddGroup('第二组');
  sgdRecordView.RowCount := sgdRecordView.RowCount + 4;
end;

end.
