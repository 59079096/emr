{*******************************************************}
{                                                       }
{               HCView V1.1  ���ߣ���ͨ                 }
{                                                       }
{      ��������ѭBSDЭ�飬����Լ���QQȺ 649023932      }
{            ����ȡ����ļ������� 2020-2-9              }
{                                                       }
{                  �������ؼ�ʵ�ֵ�Ԫ                   }
{                                                       }
{*******************************************************}

unit HCToolBarControl;

interface

uses
  Windows, Classes, Controls, Graphics, StdCtrls, SysUtils, Messages, ExtCtrls,
  CFToolBar, CFToolButton;

type
  THCToolBarControl = class(TCFToolBar)
  private
    FBtnSave, FBtnPrint, FBtnRedo, FBtnUndo, FBtnRightIndent, FBtnLeftIndent,
    FBtnBold, FBtnItalic, FBtnUnderLine, FBtnStrikeOut, FBtnSuperScript, FBtnSubScript,
    FBtnAlignLeft, FBtnAlignCenter, FBtnAlignRight, FBtnAlignJustify, FBtnAlignScatter
      : TCFToolButton;
    FBtnFile, FBtnInsert, FBtnLineSpace
      : TCFMenuButton;
    FOnSaveClick, FOnPrintClick, FOnUndoClick, FOnRedoClick,
    FOnBoldClick, FOnItalicClick, FOnUnderLineClick, FOnStrikeOutClick,
    FOnSuperScriptClick, FOnSubScriptClick, FOnRightIndentClick, FOnLeftIndentClick,
    FOnAlignLeftClick, FOnAlignCenterClick, FOnAlignRightClick,
    FOnAlignJustifyClick, FOnAlignScatterClick,
    FOnFontChange, FOnFontSizeChange, FOnFontColorChange
      : TNotifyEvent;
    FFontCombobox, FFontSizeCombobox: TComboBox;
    FFontColorBox: TColorBox;
    procedure InitImages;
  protected
    procedure DoSaveClick(Sender: TObject);
    procedure DoPrintClick(Sender: TObject);
    procedure DoUndoClick(Sender: TObject);
    procedure DoRedoClick(Sender: TObject);
    procedure DoBoldClick(Sender: TObject);
    procedure DoItalicClick(Sender: TObject);
    procedure DoUnderLineClick(Sender: TObject);
    procedure DoStrikeOutClick(Sender: TObject);
    procedure DoSuperScriptClick(Sender: TObject);
    procedure DoSubScriptClick(Sender: TObject);
    procedure DoRightIndentClick(Sender: TObject);
    procedure DoLeftIndentClick(Sender: TObject);
    procedure DoAlignLeftClick(Sender: TObject);
    procedure DoAlignCenterClick(Sender: TObject);
    procedure DoAlignRightClick(Sender: TObject);
    procedure DoAlignJustifyClick(Sender: TObject);
    procedure DoAlignScatterClick(Sender: TObject);

    procedure DoFontChange(Sender: TObject);
    procedure DoFontSizeChange(Sender: TObject);
    procedure DoFontColorChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FontCombobox: TComboBox read FFontCombobox;
    property FontSizeCombobox: TComboBox read FFontSizeCombobox;
    property FontColorCombobox: TColorBox read FFontColorBox;

    property BtnBold: TCFToolButton read FBtnBold;
    property BtnItalic: TCFToolButton read FBtnItalic;
    property BtnUnderLine: TCFToolButton read FBtnUnderLine;
    property BtnStrikeOut: TCFToolButton read FBtnStrikeOut;
    property BtnSuperScript: TCFToolButton read FBtnSuperScript;
    property BtnSubScript: TCFToolButton read FBtnSubScript;
    property BtnAlignLeft: TCFToolButton read FBtnAlignLeft;
    property BtnAlignCenter: TCFToolButton read FBtnAlignCenter;
    property BtnAlignRight: TCFToolButton read FBtnAlignRight;
    property BtnAlignJustify: TCFToolButton read FBtnAlignJustify;
    property BtnAlignScatter: TCFToolButton read FBtnAlignScatter;

    property BtnFile: TCFMenuButton read FBtnFile;
    property BtnInsert: TCFMenuButton read FBtnInsert;
    property BtnLineSpace: TCFMenuButton read FBtnLineSpace;

    property OnSaveClick: TNotifyEvent read FOnSaveClick write FOnSaveClick;
    property OnPrintClick: TNotifyEvent read FOnPrintClick write FOnPrintClick;
    property OnUndoClick: TNotifyEvent read FOnUndoClick write FOnUndoClick;
    property OnRedoClick: TNotifyEvent read FOnRedoClick write FOnRedoClick;
    property OnBoldClick: TNotifyEvent read FOnBoldClick write FOnBoldClick;
    property OnItalicClikc: TNotifyEvent read FOnItalicClick write FOnItalicClick;
    property OnUnderLineClick: TNotifyEvent read FOnUnderLineClick write FOnUnderLineClick;
    property OnStrikeOutClick: TNotifyEvent read FOnStrikeOutClick write FOnStrikeOutClick;
    property OnSuperScriptClick: TNotifyEvent read FOnSuperScriptClick write FOnSuperScriptClick;
    property OnSubScriptClick: TNotifyEvent read FOnSubScriptClick write FOnSubScriptClick;
    property OnRightIndentClick: TNotifyEvent read FOnRightIndentClick write FOnRightIndentClick;
    property OnLeftIndentClick: TNotifyEvent read FOnLeftIndentClick write FOnLeftIndentClick;
    property OnAlignLeftClick: TNotifyEvent read FOnAlignLeftClick write FOnAlignLeftClick;
    property OnAlignCenterClick: TNotifyEvent read FOnAlignCenterClick write FOnAlignCenterClick;
    property OnAlignRightClick: TNotifyEvent read FOnAlignRightClick write FOnAlignRightClick;
    property OnAlignJustifyClick: TNotifyEvent read FOnAlignJustifyClick write FOnAlignJustifyClick;
    property OnAlignScatterClick: TNotifyEvent read FOnAlignScatterClick write FOnAlignScatterClick;

    property OnFontChange: TNotifyEvent read FOnFontChange write FOnFontChange;
    property OnFontSizeChange: TNotifyEvent read FOnFontSizeChange write FOnFontSizeChange;
    property OnFontColorChange: TNotifyEvent read FOnFontColorChange write FOnFontColorChange;
  end;

  function GetFontSizeStr(AFontSize: Single): string;
  function GetFontSize(const AFontSize: string): Single;

implementation

{$R HCToolBarControl.res}

function GetFontSize(const AFontSize: string): Single;
begin
  if AFontSize = '����' then Result := 42
  else
  if AFontSize = 'С��' then Result := 36
  else
  if AFontSize = 'һ��' then Result := 26
  else
  if AFontSize = 'Сһ' then Result := 24
  else
  if AFontSize = '����' then Result := 22
  else
  if AFontSize = 'С��' then Result := 18
  else
  if AFontSize = '����' then Result := 16
  else
  if AFontSize = 'С��' then Result := 15
  else
  if AFontSize = '�ĺ�' then Result := 14
  else
  if AFontSize = 'С��' then Result := 12
  else
  if AFontSize = '���' then Result := 10.5
  else
  if AFontSize = 'С��' then Result := 9
  else
  if AFontSize = '����' then Result := 7.5
  else
  if AFontSize = 'С��' then Result := 6.5
  else
  if AFontSize = '�ߺ�' then Result := 5.5
  else
  if AFontSize = '�˺�' then Result := 5
  else
  if not TryStrToFloat(AFontSize, Result) then
    raise Exception.Create('�����ֺŴ�С�����޷�ʶ���ֵ��' + AFontSize);
end;

function GetFontSizeStr(AFontSize: Single): string;
begin
  if AFontSize = 42 then Result := '����'
  else
  if AFontSize = 36 then Result := 'С��'
  else
  if AFontSize = 26 then Result := 'һ��'
  else
  if AFontSize = 24 then Result := 'Сһ'
  else
  if AFontSize = 22 then Result := '����'
  else
  if AFontSize = 18 then Result := 'С��'
  else
  if AFontSize = 16 then Result := '����'
  else
  if AFontSize = 15 then Result := 'С��'
  else
  if AFontSize = 14 then Result := '�ĺ�'
  else
  if AFontSize = 12 then Result := 'С��'
  else
  if AFontSize = 10.5 then Result := '���'
  else
  if AFontSize = 9 then Result := 'С��'
  else
  if AFontSize = 7.5 then Result := '����'
  else
  if AFontSize = 6.5 then Result := 'С��'
  else
  if AFontSize = 5.5 then Result := '�ߺ�'
  else
  if AFontSize = 5 then Result := '�˺�'
  else
    Result := FormatFloat('0.#', AFontSize);
end;

{ THCToolBarControl }

constructor THCToolBarControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clBtnFace;
  Width := 100;
  Height := 22;

  FImages := TImageList.Create(Self);
  InitImages;

  FBtnFile := Self.AddMenuToolButton;  // ����
  FBtnFile.Caption := '�ļ�';
  FBtnFile.Name := FBtnFile.Caption;
  FBtnFile.Width := 64;
  FBtnFile.ImageIndex := 18;

  FBtnSave := Self.AddToolButton;
  FBtnSave.Caption := '����';
  FBtnSave.Name := FBtnSave.Caption;
  FBtnSave.Width := 64;
  FBtnSave.ImageIndex := 0;
  FBtnSave.OnClick := DoSaveClick;

  FBtnPrint := Self.AddToolButton;  // ��ӡ
  FBtnPrint.Caption := '��ӡ';
  FBtnPrint.Name := FBtnPrint.Caption;
  FBtnPrint.Width := 64;
  FBtnPrint.ImageIndex := 1;
  FBtnPrint.OnClick := DoPrintClick;

  FBtnInsert := Self.AddMenuToolButton;  // ����
  FBtnInsert.Caption := '����';
  FBtnInsert.Name := FBtnInsert.Caption;
  FBtnInsert.Width := 48;

  FBtnUndo := Self.AddToolButton;  // Undo
  FBtnUndo.Name := '����';
  FBtnUndo.Width := 24;
  FBtnUndo.ImageIndex := 2;
  FBtnUndo.OnClick := DoUndoClick;

  FBtnRedo := Self.AddToolButton;  // Redo
  FBtnRedo.Name := '�ָ�';
  FBtnRedo.Width := 24;
  FBtnRedo.ImageIndex := 3;
  FBtnRedo.OnClick := DoRedoClick;

  FFontCombobox := TComboBox.Create(nil);
  FFontCombobox.Name := '����';
  FFontCombobox.Width := 120;
  FFontCombobox.DropDownCount := 20;
  FFontCombobox.Parent := Self;
  FFontCombobox.Align := alLeft;
  FFontCombobox.Tag := Self.ControlCount - 1;
  FFontCombobox.Style := TComboboxStyle.csDropDownList;
  FFontCombobox.OnChange := DoFontChange;

  FFontSizeCombobox := TComboBox.Create(nil);
  FFontSizeCombobox.Name := '�ֺ�';
  FFontSizeCombobox.Width := 48;
  FFontSizeCombobox.DropDownCount := 20;
  FFontSizeCombobox.Parent := Self;
  FFontSizeCombobox.Align := alLeft;
  FFontSizeCombobox.Tag := Self.ControlCount - 1;
  FFontSizeCombobox.Style := TComboboxStyle.csDropDownList;
  FFontSizeCombobox.OnChange := DoFontSizeChange;

  FFontColorBox := TColorBox.Create(nil);
  FFontColorBox.Parent := Self;
  FFontColorBox.Align := alLeft;
  FFontColorBox.Tag := Self.ControlCount - 1;
  FFontColorBox.Width := 40;
  FFontColorBox.OnChange := DoFontColorChange;

  // ��ʽ
  FBtnBold := Self.AddToolButton;  // �Ӵ�
  FBtnBold.Width := 24;
  FBtnBold.ImageIndex := 4;
  FBtnBold.OnClick := DoBoldClick;

  FBtnItalic := Self.AddToolButton;  // ��б
  FBtnItalic.Width := 24;
  FBtnItalic.ImageIndex := 5;
  FBtnItalic.OnClick := DoItalicClick;

  FBtnUnderLine := Self.AddToolButton;  // �»���
  FBtnUnderLine.Width := 24;
  FBtnUnderLine.ImageIndex := 6;
  FBtnUnderLine.OnClick := DoUnderLineClick;

  FBtnStrikeOut := Self.AddToolButton;  // ɾ����
  FBtnStrikeOut.Width := 24;
  FBtnStrikeOut.ImageIndex := 7;
  FBtnStrikeOut.OnClick := DoStrikeOutClick;

  FBtnSuperScript := Self.AddToolButton;  // �ϱ�
  FBtnSuperScript.Width := 24;
  FBtnSuperScript.ImageIndex := 8;
  FBtnSuperScript.OnClick := DoSuperScriptClick;

  FBtnSubScript := Self.AddToolButton;  // �±�
  FBtnSubScript.Width := 24;
  FBtnSubScript.ImageIndex := 9;
  FBtnSubScript.OnClick := DoSubScriptClick;

  FBtnRightIndent := Self.AddToolButton;  // ������
  FBtnRightIndent.Width := 24;
  FBtnRightIndent.ImageIndex := 10;
  FBtnRightIndent.OnClick := DoRightIndentClick;

  FBtnLeftIndent := Self.AddToolButton;  // ������
  FBtnLeftIndent.Width := 24;
  FBtnLeftIndent.ImageIndex := 11;
  FBtnLeftIndent.OnClick := DoLeftIndentClick;

  FBtnAlignLeft := Self.AddToolButton;  // �����
  FBtnAlignLeft.Width := 24;
  FBtnAlignLeft.ImageIndex := 12;
  FBtnAlignLeft.OnClick := DoAlignLeftClick;

  FBtnAlignCenter := Self.AddToolButton;  // ���ж���
  FBtnAlignCenter.Width := 24;
  FBtnAlignCenter.ImageIndex := 13;
  FBtnAlignCenter.OnClick := DoAlignCenterClick;

  FBtnAlignRight := Self.AddToolButton;  // �Ҷ���
  FBtnAlignRight.Width := 24;
  FBtnAlignRight.ImageIndex := 14;
  FBtnAlignRight.OnClick := DoAlignRightClick;

  FBtnAlignJustify := Self.AddToolButton;  // ��ɢ����
  FBtnAlignJustify.Width := 24;
  FBtnAlignJustify.ImageIndex := 15;
  FBtnAlignJustify.OnClick := DoAlignJustifyClick;

  FBtnAlignScatter := Self.AddToolButton;  // ���˶���
  FBtnAlignScatter.Width := 24;
  FBtnAlignScatter.ImageIndex := 16;
  FBtnAlignScatter.OnClick := DoAlignScatterClick;

  FBtnLineSpace:= Self.AddMenuToolButton;  // �м��
  FBtnLineSpace.Width := 40;
  FBtnLineSpace.ImageIndex := 17;
end;

destructor THCToolBarControl.Destroy;
begin
  FreeAndNil(FFontCombobox);
  FreeAndNil(FFontCombobox);
  FreeAndNil(FImages);
  inherited Destroy;
end;

procedure THCToolBarControl.DoAlignCenterClick(Sender: TObject);
begin
  if Assigned(FOnAlignCenterClick) then
    FOnAlignCenterClick(Sender);
end;

procedure THCToolBarControl.DoAlignJustifyClick(Sender: TObject);
begin
  if Assigned(FOnAlignJustifyClick) then
    FOnAlignJustifyClick(Sender);
end;

procedure THCToolBarControl.DoAlignLeftClick(Sender: TObject);
begin
  if Assigned(FOnAlignLeftClick) then
    FOnAlignLeftClick(Sender);
end;

procedure THCToolBarControl.DoAlignRightClick(Sender: TObject);
begin
  if Assigned(FOnAlignRightClick) then
    FOnAlignRightClick(Sender);
end;

procedure THCToolBarControl.DoAlignScatterClick(Sender: TObject);
begin
  if Assigned(FOnAlignScatterClick) then
    FOnAlignScatterClick(Sender);
end;

procedure THCToolBarControl.DoBoldClick(Sender: TObject);
begin
  if Assigned(FOnBoldClick) then
    FOnBoldClick(Sender);
end;

procedure THCToolBarControl.DoFontChange(Sender: TObject);
begin
  if Assigned(FOnFontChange) then
    FOnFontChange(Sender);
end;

procedure THCToolBarControl.DoFontColorChange(Sender: TObject);
begin
  if Assigned(FOnFontColorChange) then
    FOnFontColorChange(Sender);
end;

procedure THCToolBarControl.DoFontSizeChange(Sender: TObject);
begin
  if Assigned(FOnFontSizeChange) then
    FOnFontSizeChange(Sender);
end;

procedure THCToolBarControl.DoItalicClick(Sender: TObject);
begin
  if Assigned(FOnItalicClick) then
    FOnItalicClick(Sender);
end;

procedure THCToolBarControl.DoLeftIndentClick(Sender: TObject);
begin
  if Assigned(FOnLeftIndentClick) then
    FOnLeftIndentClick(Sender);
end;

procedure THCToolBarControl.DoPrintClick(Sender: TObject);
begin
  if Assigned(FOnPrintClick) then
    FOnPrintClick(Sender);
end;

procedure THCToolBarControl.DoRedoClick(Sender: TObject);
begin
  if Assigned(FOnRedoClick) then
    FOnRedoClick(Sender);
end;

procedure THCToolBarControl.DoRightIndentClick(Sender: TObject);
begin
  if Assigned(FOnRightIndentClick) then
    FOnRightIndentClick(Sender);
end;

procedure THCToolBarControl.DoSaveClick(Sender: TObject);
begin
  if Assigned(FOnSaveClick) then
    FOnSaveClick(Sender);
end;

procedure THCToolBarControl.DoStrikeOutClick(Sender: TObject);
begin
  if Assigned(FOnStrikeOutClick) then
    FOnStrikeOutClick(Sender);
end;

procedure THCToolBarControl.DoSubScriptClick(Sender: TObject);
begin
  if Assigned(FOnSubScriptClick) then
    FOnSubScriptClick(Sender);
end;

procedure THCToolBarControl.DoSuperScriptClick(Sender: TObject);
begin
  if Assigned(FOnSuperScriptClick) then
    FOnSuperScriptClick(Sender);
end;

procedure THCToolBarControl.DoUnderLineClick(Sender: TObject);
begin
  if Assigned(FOnUnderLineClick) then
    FOnUnderLineClick(Sender);
end;

procedure THCToolBarControl.DoUndoClick(Sender: TObject);
begin
  if Assigned(FOnUndoClick) then
   FOnUndoClick(Sender);
end;

procedure THCToolBarControl.InitImages;
var
  vIcon: TIcon;
begin
  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'SAVE');  // 0
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'PRINT');  // 1
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'UNDO');  // 2
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'REDO');  // 3
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'BOLD');  // 4
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'ITALIC');  // 5
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'UNDERLINE');  // 6
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'STRIKE');  // 7
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'SUPERSCRIPT');  // 8
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'SUBSCRIPT');  // 9
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'RIGHTIDENT');  // 10
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'LEFTIDENT');  // 11
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'ALIGNLEFT');  // 12
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'ALIGNCENTER');  // 13
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'ALIGNRIGHT');  // 14
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'ALIGNJUSTIFY');  // 15
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'ALIGNSCATTER');  // 16
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'LINESPACE');  // 17
  FImages.AddIcon(vIcon);

  vIcon := TIcon.Create;
  vIcon.LoadFromResourceName(HInstance, 'FILE');  // 18
  FImages.AddIcon(vIcon);
end;

end.
