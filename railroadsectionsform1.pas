//Copyright (c) 2013 by Donald R. Ziesig
//
//Donald.at.Ziesig.org
//
//This file is part of the MagicDispatcher program.
//
//MagicDispatcher is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//MagicDispatcher is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with MagicDispatcher.  If not, see <http://www.gnu.org/licenses/>.

{==============================================================================}
{ This unit implements the GUI for Railroad Sections.                          }
{==============================================================================}

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
    procedure DownActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  StringSubs, RailroadSectionsUnit1, Common1;

{ TSectionsForm }

{.$define DEBUG_INDEXING}

const
  GridColCount = 2; // second column visible during development, hidden when done

procedure TSectionsForm.AppendSectionActionExecute(Sender: TObject);
var
  RC : Integer;
begin
  StringGrid1.ColCount := GridColCount;
  RC := StringGrid1.RowCount;
  StringGrid1.RowCount := rc + 1;
  SelectedRow := RC;
end;

procedure TSectionsForm.DownActionExecute(Sender: TObject);
begin
  ExchangeRow( false );
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
      StringGrid1.Cells[1,fSelectedRow] := '';
      StringGrid1.Objects[0,fSelectedRow] := nil;
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

procedure TSectionsForm.ExchangeRow(withRowAbove: Boolean);
  procedure Xchg( Row0, Row1 : Integer );
  var
    CurrentText : String;
    CurrentSection : TSection;
    I : Integer;
  begin
    for I := 0 to pred(GridColCount) do
      begin
        CurrentText := StringGrid1.Cells[I,Row0];
        StringGrid1.Cells[I,Row0] := StringGrid1.Cells[I,Row1];
        StringGrid1.Cells[I,Row1] := CurrentText;
      end;
    CurrentSection := StringGrid1.Objects[0,Row0] as TSection;
    StringGrid1.Objects[0,Row0] := StringGrid1.Objects[0,Row1];
    StringGrid1.Objects[0,Row1] := CurrentSection;
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

procedure TSectionsForm.FormCreate(Sender: TObject);
begin
  inherited;
  fSelectedRow := -1;
  SelectedRow := 0;
  StringGrid1Resize( Sender );
  EnableControls;
end;

procedure TSectionsForm.FormHide(Sender: TObject);
var
  I : Integer;
//  Index : Integer;
  S : TSection;
begin
  // Check for an empty string grid (unfortunately, it will have a RowCount = 1)
  // Emptyness here means RowCount = 1 and Empty(Cells[0,0]) and Objects[0,0] = nil;
  if (StringGrid1.RowCount = 1) and
     Empty(StringGrid1.Cells[0,0]) and
     (StringGrid1.Objects[0,0] = nil) then exit;

  for I := 0 to pred( StringGrid1.RowCount ) do
    begin
      S := StringGrid1.Objects[0,I] as TSection;
      if S = nil then
        begin
          S := TSection.Create( Railroad.SectionList, StringGrid1.Cells[0,I] );
          Railroad.SectionList.Add( S );
        end;
      S.Name := StringGrid1.Cells[0,I];
      S.Order := I;
    end;
end;

procedure TSectionsForm.FormShow(Sender: TObject);
begin
  StringGrid1Resize( Sender );
  LoadControls;
end;

procedure TSectionsForm.InsertSectionActionExecute(Sender: TObject);
begin
  StringGrid1.InsertColRow( false, SelectedRow );
  SectionNameEdit.Text := StringGrid1.Cells[0, SelectedRow ];
  StringGrid1.Row := SelectedRow;
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
  O : Integer;
  Item : TSection;
begin
  StringGrid1.Clear;
  C := Railroad.SectionList.Count;
  StringGrid1.RowCount := Max(1,C);// Must leave one row in grid.
  StringGrid1Resize( Self );
  for I := 0 to pred( C ) do
    begin
      Item := Railroad.SectionList.Items[I];
      O := Item.Order;
      StringGrid1.Cells[0,O] := Item.Name;
      StringGrid1.Cells[1,O] := IntToStr(Item.Id);
      StringGrid1.Objects[0,O] := Item;
    end;
  fSelectedRow := 0;
  RefreshEditor;
end;

procedure TSectionsForm.RefreshEditor;
begin
  if fSelectedRow >= 0 then
    SectionNameEdit.Text := StringGrid1.Cells[0,fSelectedRow]
  else
    SectionNameEdit.Text := '???????';
  StringGrid1.Row := fSelectedRow;
end;

procedure TSectionsForm.RemoveSectionActionExecute(Sender: TObject);
var
  Item : TSection;
begin
  Item := StringGrid1.Objects[0,SelectedRow] as TSection;
  if Item <> nil then
    Railroad.SectionList.DeleteItem(Item);
  StringGrid1.DeleteColRow( false, SelectedRow );
  RefreshEditor;
  EnableControls;
end;

procedure TSectionsForm.SectionNameEditChange(Sender: TObject);
begin
  StringGrid1.Cells[0,SelectedRow] := SectionNameEdit.Text;
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

procedure TSectionsForm.SetSelectedRow(AValue: Integer);
begin
  if fSelectedRow=AValue then Exit;
  fSelectedRow:=AValue;
  StringGrid1.Row := fSelectedRow;
  RefreshEditor;
  EnableControls;
end;

procedure TSectionsForm.StringGrid1Resize(Sender: TObject);
{$ifdef DEBUG_INDEXING}
const
  IndexColWidth = 36;
{$endif DEBUG_INDEXING}
begin
  {$ifdef DEBUG_INDEXING}
  StringGrid1.ColWidths[0] := StringGrid1.ClientWidth - IndexColWidth;
  StringGrid1.ColWidths[1] := IndexColWidth;
  {$else}
  StringGrid1.ColWidths[0] := StringGrid1.ClientWidth;
  StringGrid1.ColWidths[1] := 0;
  {$endif DEBUG_INDEXING}
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

end.

