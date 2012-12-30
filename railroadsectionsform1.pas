unit railroadsectionsform1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ComCtrls, ActnList, Buttons, ExtCtrls, RailroadBaseForm1;

type

  { TSectionsForm }

  TSectionsForm = class(TRailroadBaseForm)
    RemoveSectionAction: TAction;
    AppendSectionAction: TAction;
    InsertSectionAction: TAction;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    DownAction: TAction;
    ImageList1: TImageList;
    SectionNameEdit: TLabeledEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    UpAction: TAction;
    ActionList1: TActionList;
    StringGrid1: TStringGrid;
    procedure AppendSectionActionExecute(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure DownActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure InsertSectionActionExecute(Sender: TObject);
    procedure SectionNameEditChange(Sender: TObject);
    procedure RemoveSectionActionExecute(Sender: TObject);
    procedure SectionNameEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid1Resize(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure UpActionExecute(Sender: TObject);
  private
    procedure SetSelectedRow(AValue: Integer);
    procedure RefreshEditor;
    procedure EnableControls;
    function  SectionTrackCount : Cardinal;
    function  IsDuplicate( aName : String ) : boolean;
  private
    { private declarations }
    fSelectedRow : Integer;

    property SelectedRow : Integer read fSelectedRow write SetSelectedRow;
    procedure LoadControls; override;
    procedure ExchangeRow( withRowAbove : Boolean );
  public
    { public declarations }
  end;

var
  SectionsForm: TSectionsForm;

implementation

{$R *.lfm}

uses
  StringSubs, RailroadSectionsUnit1;

{ TSectionsForm }

procedure TSectionsForm.AppendSectionActionExecute(Sender: TObject);
var
  RC : Integer;
begin
  StringGrid1.ColCount := 1;
  RC := StringGrid1.RowCount;
  StringGrid1.RowCount := rc + 1;
  SelectedRow := RC;
end;

procedure TSectionsForm.DeleteActionExecute(Sender: TObject);
begin

end;

procedure TSectionsForm.DownActionExecute(Sender: TObject);
begin
  ExchangeRow( false );
end;

procedure TSectionsForm.FormCreate(Sender: TObject);
begin
  inherited;
  fSelectedRow := -1;
  SelectedRow := 0;
  EnableControls;
end;

procedure TSectionsForm.FormHide(Sender: TObject);
var
  I : Integer;
  s : TSection;
begin
  Railroad.SectionList.Clear;
  for I := 0 to pred( StringGrid1.RowCount ) do
    begin
      S := TSection.Create( Railroad.SectionList );
      S.Name := StringGrid1.Cells[0,I];
      Railroad.SectionList.Add( S );
    end;
end;

procedure TSectionsForm.InsertSectionActionExecute(Sender: TObject);
begin
  StringGrid1.InsertColRow( false, SelectedRow );
  SectionNameEdit.Text := StringGrid1.Cells[0, SelectedRow ];
  StringGrid1.Row := SelectedRow;
end;

procedure TSectionsForm.SectionNameEditChange(Sender: TObject);
begin
  StringGrid1.Cells[0,SelectedRow] := SectionNameEdit.Text;
  EnableControls;
end;

procedure TSectionsForm.RemoveSectionActionExecute(Sender: TObject);
begin
  StringGrid1.DeleteColRow( false, SelectedRow );
  RefreshEditor;
  EnableControls;
end;

procedure TSectionsForm.SectionNameEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    if not Empty( SectionNameEdit.Text ) then
      if not IsDuplicate( SectionNameEdit.Text ) then
        AppendSectionActionExecute(Sender);
end;


procedure TSectionsForm.StringGrid1Resize(Sender: TObject);
begin
  StringGrid1.ColWidths[0] := StringGrid1.ClientWidth;
  StringGrid1.Height := ClientHeight - 20 - StringGrid1.Top;
  EnableControls;
end;

procedure TSectionsForm.StringGrid1SelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  SelectedRow := aRow;
end;

procedure TSectionsForm.UpActionExecute(Sender: TObject);
begin
  ExchangeRow( True );
end;

procedure TSectionsForm.SetSelectedRow(AValue: Integer);
begin
  if fSelectedRow=AValue then Exit;
  fSelectedRow:=AValue;
  SectionNameEdit.Text := StringGrid1.Cells[0,fSelectedRow];
  StringGrid1.Row := fSelectedRow;
  RefreshEditor;
  EnableControls;
end;

procedure TSectionsForm.RefreshEditor;
begin
  SectionNameEdit.Text := StringGrid1.Cells[0,fSelectedRow];
  StringGrid1.Row := fSelectedRow;
end;

procedure TSectionsForm.EnableControls;
var
  NullName : Boolean;
begin
  if IsDuplicate( SectionNameEdit.Text ) then
    begin
      UpAction.Enabled := False;
      DownAction.Enabled := False;
      AppendSectionAction.Enabled := False;
      InsertSectionAction.Enabled := False;
      RemoveSectionAction.Enabled := False;
      SectionNameEdit.Color := clRed;
      StringGrid1.Cells[0,fSelectedRow] := '';
    end
  else
    begin
      UpAction.Enabled := SelectedRow > 0;
      DownAction.Enabled := SelectedRow < (StringGrid1.RowCount - 1);
      NullName := Empty(SectionNameEdit.Text);
      if NullName then
        SectionNameEdit.Color := clYellow
      else
        SectionNameEdit.Color := clWhite;
      AppendSectionAction.Enabled := not NullName;
      InsertSectionAction.Enabled := not NullName;
      RemoveSectionAction.Enabled := (StringGrid1.RowCount > 1) and
                                     (SectionTrackCount = 0);
    end;
end;

function TSectionsForm.SectionTrackCount: Cardinal;
begin
{$define PRELIMINARY_TEST}
{$ifdef PRELIMINARY_TEST}
  if fSelectedRow = 3 then
    Result := 3
  else
    Result := 0;
{$else}
// Real code goes here
{$endif}
end;

function TSectionsForm.IsDuplicate(aName: String): boolean;
var
  I : Integer;
begin
  Result := False;
  if Empty( aName ) then exit;
  for I := 0 to pred(StringGrid1.RowCount) do
    if I <> fSelectedRow then
      if StringGrid1.Cells[0,I] = aName then
        begin
          Result := True;
          exit;
        end;
end;

procedure TSectionsForm.LoadControls;
var
  I : Integer;
  C : Integer;
begin
  StringGrid1.Clear;
  C := Railroad.SectionList.Count;
  StringGrid1.RowCount := C;// Railroad.SectionList.Count;
  StringGrid1.ColCount := 1;
  for I := 0 to pred( C ) do
    StringGrid1.Cells[0,I] := Railroad.SectionList.Items[I].Name;
  fSelectedRow := 0;
end;

procedure TSectionsForm.ExchangeRow(withRowAbove: Boolean);
  procedure Xchg( Row0, Row1 : Integer );
  var
    CurrentText : String;
  begin
    CurrentText := StringGrid1.Cells[0,Row0];
    StringGrid1.Cells[0,Row0] := StringGrid1.Cells[0,Row1];
    StringGrid1.Cells[0,Row1] := CurrentText;
  end;
begin
  if withRowAbove then
    begin
      Assert(fSelectedRow > 0);
      Xchg( fSelectedRow, fSelectedRow-1 );
      SelectedRow := fSelectedRow-1;
    end
  else
    begin
      Assert( fSelectedRow < (StringGrid1.RowCount-2) );
      Xchg( fSelectedRow, fSelectedRow+1 );
      SelectedRow := fSelectedRow+1;
    end;
end;

end.

