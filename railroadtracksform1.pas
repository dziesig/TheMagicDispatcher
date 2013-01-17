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

unit railroadtracksform1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ActnList, RailroadBaseForm1, RailroadTracksUnit1;

type

  { TTrackForm }

  TTrackForm = class(TRailroadBaseForm)
    ActionList1: TActionList;
    AppendTrackAction: TAction;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    DownAction: TAction;
    ImageList1: TImageList;
    InsertTrackAction: TAction;
    Label3: TLabel;
    RemoveTrackAction: TAction;
    TrackNameEdit: TLabeledEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    TrackNameLB: TListBox;
    TrackKindCB: TComboBox;
    Label2: TLabel;
    SectionCB: TComboBox;
    Label1: TLabel;
    UpAction: TAction;
    procedure AppendTrackActionExecute(Sender: TObject);
    procedure DownActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InsertTrackActionExecute(Sender: TObject);
    procedure RemoveTrackActionExecute(Sender: TObject);
    procedure SectionCBChange(Sender: TObject);
    procedure TrackNameEditChange(Sender: TObject);
    procedure TrackNameEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TrackNameLBClick(Sender: TObject);
    procedure TrackNameLBResize(Sender: TObject);
    procedure UpActionExecute(Sender: TObject);
  private
    { private declarations }
    fSelectedRow: Integer;
    procedure SetSelectedRow(AValue: Integer);
    procedure EnableControls;
    procedure ExchangeRow(withRowAbove: Boolean);
    function  IsDuplicate( aName : String ) : boolean;
    procedure RefreshEditor;
  public
    { public declarations }

    procedure LoadControls; override;

    property SelectedRow : Integer read fSelectedRow write SetSelectedRow;
  end;

var
  TrackForm: TTrackForm;

implementation

{$R *.lfm}

uses
  StringSubs, Common1, RailroadSectionsUnit1;

{ TTrackForm }

procedure TTrackForm.AppendTrackActionExecute(Sender: TObject);
var
  RC : Integer;
  T  : TTrack;
begin
  RC := TrackNameLB.Items.Count;
  T := TTrack.Create( Railroad.TrackList, '' );
  TrackNameLB.AddItem('',T);
  SelectedRow := RC;
end;

procedure TTrackForm.DownActionExecute(Sender: TObject);
begin
  ExchangeRow( False );
end;

procedure TTrackForm.EnableControls;
var
  NullName : Boolean;
  C        : Integer;
begin
  NullName := Empty( TrackNameEdit.Text );
  if NullName then
    begin
      TrackNameEdit.Color := clYellow;
      UpAction.Enabled := SelectedRow > 0;
      DownAction.Enabled := SelectedRow < (TrackNameLB.Count - 1);
    end
  else
    if IsDuplicate( TrackNameEdit.Text ) then
      begin
        UpAction.Enabled := False;
        DownAction.Enabled := False;
        AppendTrackAction.Enabled := False;
        InsertTrackAction.Enabled := False;
        RemoveTrackAction.Enabled := False;
        TrackNameEdit.Color := clRed;
        TrackNameLB.Items[SelectedRow] := '';
        TrackNameLB.Items.Objects[SelectedRow] := nil;
      end
    else
      begin
        UpAction.Enabled := SelectedRow > 0;
        DownAction.Enabled := SelectedRow < (TrackNameLB.Count - 1);
        TrackNameEdit.Color := clWhite;
      end;
    AppendTrackAction.Enabled := not NullName;
    InsertTrackAction.Enabled := not NullName;
    C := TrackNameLB.Count;
    RemoveTrackAction.Enabled := (C > 1);
    C := SectionCB.ItemIndex;
    if C < 0 then
      begin
        AppendTrackAction.Enabled := False;
        InsertTrackAction.Enabled := False;
        RemoveTrackAction.Enabled := False;
        UpAction.Enabled := False;
        DownAction.Enabled := False;
      end;
    TrackNameEdit.Enabled := C >= 0;

end;

procedure TTrackForm.ExchangeRow(withRowAbove: Boolean);
  procedure Xchg( Row0, Row1 : Integer );
  var
    CurrentName : String;
    CurrentItem : TTrack;
  begin
    CurrentName := TrackNameLB.Items[Row0];
    CurrentItem := TrackNameLB.Items.Objects[Row0] as TTrack;
    TrackNameLB.Items[Row0] := TrackNameLB.Items[Row1];
    TrackNameLB.Items.Objects[Row0] := TrackNameLb.Items.Objects[Row1];
    TrackNameLB.Items[Row1] := CurrentName;
    TrackNameLB.Items.Objects[Row1] := CurrentItem;
  end;
begin
  if withRowAbove then
    begin
      Xchg( fSelectedRow, fSelectedRow-1 );
      SelectedRow := fSelectedRow-1;
    end
  else
    begin
      Xchg( fSelectedRow, fSelectedRow+1 );
      SelectedRow := fSelectedRow+1;
    end;
end;

procedure TTrackForm.FormCreate(Sender: TObject);
var
  I : TTrackKind;
begin
  inherited;
  for I := tkThrough to pred(tkCount) do
    TrackKindCB.AddItem( TrackKind[I], nil );
  TrackKindCB.ItemIndex := 0;
  fSelectedRow := -1;
  SelectedRow := 0;
//  EnableControls;
end;

procedure TTrackForm.FormHide(Sender: TObject);
var
  I : Integer;
  T : TTrack;
begin
  for I := 0 to pred( TrackNameLB.Count ) do
    begin
      T := TrackNameLB.Items.Objects[I] as TTrack;
      if T = nil then
        begin
          Debug('Unexpected nil Track from ListBox');
        end;
      T.Name := TrackNameLB.Items[I];
      T.Order := I;
    end;
end;

procedure TTrackForm.FormShow(Sender: TObject);
begin
  LoadControls;
end;

procedure TTrackForm.InsertTrackActionExecute(Sender: TObject);
var
  T : TTrack;
begin
  T := TTrack.Create( Railroad.TrackList, '' );
  TrackNameLB.Items.InsertObject( SelectedRow, '', T  );
  TrackNameEdit.Text := TrackNameLB.Items[ SelectedRow ];
  TrackNameLB.ItemIndex := SelectedRow;
end;

function TTrackForm.IsDuplicate(aName: String): boolean;
var
  I : Integer;
  T : TTrack;
  S : TSection;
  C : Integer;
begin
  Result := False;
  if Empty( aName ) then exit;
  I := SectionCB.ItemIndex;
  S := SectionCB.Items.Objects[I] as TSection;
  C := TrackNameLB.Items.Count;
  for I := 0 to pred(C) do
    if I <> fSelectedRow then
      begin
        T := TrackNameLB.Items.Objects[I] as TTrack;
        if (T.Name = aName) and (T.SectionId = S.Id) then
          begin
            Result := True;
            exit;
          end;
      end;
end;

procedure TTrackForm.LoadControls;
var
  I : Integer;
  C : Integer;
  T : TTrack;
  S : TSection;
begin
  SectionCB.Clear;
  C := Railroad.SectionList.Count;
  for I := 0 to pred( C ) do
    begin
      S := Railroad.SectionList.Items[I] as TSection;
      SectionCB.AddItem(S.Name, S );
    end;
  TrackNameLB.Clear;
  C := Railroad.TrackList.Count;
  for I := 0 to pred( C ) do
    TrackNameLB.Items.Add('');
  for I := 0 to pred( C ) do
    begin
      T := Railroad.TrackList.Items[I];
      TrackNameLB.Items[T.Order] := T.Name;
      TrackNameLB.Items.Objects[T.Order] := T;
    end;
  if TrackNameLB.Items.Count < 1 then
    TrackNameLB.AddItem('',nil);
  SectionCB.ItemIndex := 0; // Need to get from the selected track.
  SelectedRow := 0;
end;

procedure TTrackForm.RefreshEditor;
begin
  if fSelectedRow >= 0 then
    TrackNameEdit.Text :=TrackNameLB.Items[fSelectedRow]
  else
    TrackNameEdit.Text := '???????';
  TrackNameLB.ItemIndex := fSelectedRow;
end;

procedure TTrackForm.RemoveTrackActionExecute(Sender: TObject);
var
  Item : TTrack;
begin
  Item := TrackNameLB.Items.Objects[SelectedRow] as TTrack;
  if Item <> nil then
    Railroad.TrackList.DeleteItem(Item);
  TrackNameLB.Items.Delete( SelectedRow );
  RefreshEditor;
  EnableControls;
end;

procedure TTrackForm.SectionCBChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TTrackForm.TrackNameEditChange(Sender: TObject);
var
  C : Integer;
begin
  C := TrackNameLB.Count;
  TrackNameLB.Items[SelectedRow] := TrackNameEdit.Text;
  EnableControls;
end;

procedure TTrackForm.TrackNameEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    if not Empty( TrackNameEdit.Text ) then
      if not IsDuplicate( TrackNameEdit.Text ) then
        AppendTrackActionExecute(Sender);
end;

procedure TTrackForm.TrackNameLBClick(Sender: TObject);
begin
  SelectedRow := TrackNameLB.ItemIndex;
end;

procedure TTrackForm.TrackNameLBResize(Sender: TObject);
begin
  TrackNameLB.Height := ClientHeight - 20 - TrackNameLB.Top;
end;

procedure TTrackForm.UpActionExecute(Sender: TObject);
begin
  ExchangeRow( True );
end;

procedure TTrackForm.SetSelectedRow(AValue: Integer);
var
  TrackName : String;
  Track     : TTrack;
begin
  if SelectedRow >= 0 then
    begin
      Track := TTrack(TrackNameLB.Items.Objects[fSelectedRow]);
      if Track <> nil then
        begin
          Track.TrackKind := TTrackKind( TrackKindCB.ItemIndex );
          Track.SectionId := TSection(SectionCB.Items.Objects[SectionCB.ItemIndex]).Id;
        end;
    end;
  fSelectedRow:=AValue;
  TrackName := '';
  if (fSelectedRow >= 0) and (fSelectedRow < TrackNameLB.Count) then
    begin
      TrackNameLB.ItemIndex := fSelectedRow;
      TrackName := TrackNameLB.Items[fSelectedRow];
      Track := TTrack(TrackNameLB.Items.Objects[fSelectedRow]);
      if Track <> nil then
        begin
          TrackKindCB.ItemIndex := ord( Track.TrackKind );
          SectionCB.ItemIndex := Track.SectionId;
        end;
    end
  else
    begin
//      SectionCB.ItemIndex := -1;
      TrackKindCB.ItemIndex := ord( tkThrough );
    end;

  TrackNameEdit.Text := TrackName;
  EnableControls;
end;

end.

