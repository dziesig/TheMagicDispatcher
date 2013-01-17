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
{ This unit implements the GUI for Railroad Tracks.                            }
{                                                                              }
{ This module is rather complicated so some design info is in order.           }
{                                                                              }
{ If this module is invoked with a Railroad that has an empty list of sections,}
{ it will display a warning and not allow any track-defining operations.       }
{                                                                              }
{ Track names must be unique for any given section.                            }
{                                                                              }
{ Track names may not be entered unless a section is selected from the drop-   }
{ down list.                                                                   }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{==============================================================================}

unit railroadtracksform2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ActnList, ExtCtrls, Buttons,

  RailroadBaseForm1, RailroadTracksUnit1;

type

  { TTrackForm2 }

  TTrackForm2 = class(TRailroadBaseForm)
    ActionList1: TActionList;
    AppendTrackAction: TAction;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    DownAction: TAction;
    ImageList1: TImageList;
    InsertTrackAction: TAction;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    WarningPanel: TPanel;
    RemoveTrackAction: TAction;
    SectionCB: TComboBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    TrackKindCB: TComboBox;
    TrackNameEdit: TLabeledEdit;
    TrackNameLB: TListBox;
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
    fSelectedRow : Integer;
//    fSectionItemIndex : Integer;
    function GetSelectedTrack: TTrack;
    function GetSelectedTrackName: String;
    function GetSelectedTrackSectionID: Integer;
    procedure SetSelectedRow(AValue: Integer);
    procedure EnableControls;
    procedure RefreshEditor;
    function  IsDuplicate( aName : String ) : Boolean;
    procedure ExchangeRow(withRowAbove: Boolean);
    procedure SetSelectedTrack(AValue: TTrack);
    procedure SetSelectedTrackName(AValue: String);
    procedure SetSelectedTrackSectionId(AValue: Integer);
  public
    { public declarations }
    procedure LoadControls; override;

    property SelectedRow : Integer read fSelectedRow write SetSelectedRow;
    property SelectedTrackName : String read GetSelectedTrackName write SetSelectedTrackName;
    property SelectedTrack : TTrack read GetSelectedTrack write SetSelectedTrack;
    property SelectedTrackSectionId : Integer read GetSelectedTrackSectionID write SetSelectedTrackSectionId;
  end;

var
  TrackForm2: TTrackForm2;

implementation

uses
  StringSubs, Common1, RailroadSectionsUnit1;

{$R *.lfm}

{ TTrackForm2 }

procedure TTrackForm2.AppendTrackActionExecute(Sender: TObject);
var
  RC : Integer;
  T  : TTrack;
begin
  SectionCB.ItemIndex := -1;
  RC := TrackNameLB.Items.Count;
  T := TTrack.Create( Railroad.TrackList, '' );
  Railroad.TrackList.Add( T );
  TrackNameLB.AddItem('',T);
  SelectedRow := RC;
end;

procedure TTrackForm2.DownActionExecute(Sender: TObject);
begin
  ExchangeRow( False );
end;

procedure TTrackForm2.EnableControls;
  procedure DisableAll;
  begin
    SectionCB.Enabled := False;
    TrackKindCB.Enabled := False;
    TrackNameEdit.Enabled := False;
    UpAction.Enabled := False;
    DownAction.Enabled := False;
    AppendTrackAction.Enabled := False;
    InsertTrackAction.Enabled := False;
    RemoveTrackAction.Enabled := False;
  end;

var
  SectionCount : Integer;
begin
  // If the section list is empty, warn user and disable everything.
  SectionCount := SectionCB.Items.Count;
  WarningPanel.Visible := SectionCount = 0;
  if SectionCount = 0 then
    DisableAll
  else
    begin
      if TrackNameLB.Items.Count > 0 then
        begin
          DownAction.Enabled := (SelectedRow < pred( TrackNameLB.Count )) and
                                ( SelectedRow >= 0 );
          UpAction.Enabled   := SelectedRow > 0;
          SectionCB.Enabled := True;
          TrackKindCB.Enabled := True;
          if SectionCB.ItemIndex < 0 then
            begin
               TrackNameEdit.Enabled := False;
               TrackNameEdit.Text := 'Select Section';
            end
          else
            begin
              TrackNameEdit.Enabled := True;
              if TracknameEdit.Text = 'Select Section' then
                TrackNameEdit.Text := '';
            end;
          AppendTrackAction.Enabled := True;
          InsertTrackAction.Enabled := SelectedRow > 0;
          RemoveTrackAction.Enabled := SelectedRow >= 0;
        end
      else
        begin
          DisableAll;
          AppendTrackAction.Enabled := True;
        end;
    end;
end;

procedure TTrackForm2.ExchangeRow(withRowAbove: Boolean);
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

procedure TTrackForm2.FormCreate(Sender: TObject);
var
  I : TTrackKind;
begin
  inherited;
  for I := tkThrough to pred(tkCount) do
    TrackKindCB.AddItem( TrackKind[I], nil );
  TrackKindCB.ItemIndex := 0;
  fSelectedRow := -1;
end;

procedure TTrackForm2.FormHide(Sender: TObject);
var
  I : Integer;
  T : TTrack;
  C : Integer;
begin
  C := TrackNameLB.Count;
  for I := 0 to pred( C ) do
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

procedure TTrackForm2.FormShow(Sender: TObject);
begin
  LoadControls;
end;

function TTrackForm2.GetSelectedTrack: TTrack;
begin
  Result := nil;
  if (fSelectedRow >= 0) and (fSelectedRow < TrackNameLB.Items.Count) then
    Result := TrackNameLB.Items.Objects[fSelectedRow] as TTrack
  else
    Debug( 'GetSelectedTrack: fSelectedRow = ' + IntToStr( fSelectedRow ) + #13#10 +
           'TrackNameLB.Items.Count = ' + IntToStr( TrackNameLB.Items.Count ) );
end;

function TTrackForm2.GetSelectedTrackName: String;
begin
  Result := '<ERROR>';
  if (fSelectedRow >= 0) and (fSelectedRow < TrackNameLB.Items.Count) then
    Result := TrackNameLB.Items[fSelectedRow]
  else
    Debug( 'GetSelectedTrackName: fSelectedRow = ' + IntToStr( fSelectedRow ) + #13#10 +
           'TrackNameLB.Items.Count = ' + IntToStr( TrackNameLB.Items.Count ) );

end;

function TTrackForm2.GetSelectedTrackSectionID: Integer;
begin
  Result := SelectedTrack.SectionId;
end;

procedure TTrackForm2.InsertTrackActionExecute(Sender: TObject);
begin
  Stub('InsertTrackActionExecute');
end;

function TTrackForm2.IsDuplicate(aName: String): Boolean;
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

procedure TTrackForm2.LoadControls;
var
  SectionCount : Integer;
  I, C         : Integer;
  S            : TSection;
  T            : TTrack;
begin
  SectionCount := Railroad.SectionList.Count;
  SectionCB.Clear;
  for I := 0 to pred( SectionCount ) do
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
//  if TrackNameLB.Items.Count < 1 then
//    TrackNameLB.AddItem('',nil);
//  SectionCB.ItemIndex := 0; // Need to get from the selected track.
//  SelectedRow := pred( 0 );
  EnableControls;
end;

procedure TTrackForm2.RefreshEditor;
begin
  if fSelectedRow >= 0 then
    TrackNameEdit.Text := SelectedTrackName // TrackNameLB.Items[fSelectedRow]
  else
    TrackNameEdit.Text := '';
  TrackNameLB.ItemIndex := fSelectedRow;
end;

procedure TTrackForm2.RemoveTrackActionExecute(Sender: TObject);
var
  Item : TTrack;
begin
  Item := SelectedTrack; // TrackNameLB.Items.Objects[SelectedRow] as TTrack;
  if Item <> nil then
    Railroad.TrackList.DeleteItem(Item);
  TrackNameLB.Items.Delete( SelectedRow );
  fSelectedRow := -1;
  SelectedRow := TrackNaMeLB.Items.Count - 1;
  RefreshEditor;
  EnableControls;
end;

procedure TTrackForm2.SectionCBChange(Sender: TObject);
begin
  SelectedTrackSectionId := TSection(SectionCB.Items.Objects[SectionCB.ItemIndex]).Id;
  EnableControls;
end;

procedure TTrackForm2.SetSelectedRow(AValue: Integer);
var
  TrackName : String;
  Track     : TTrack;
  I         : Integer;
  S         : TSection;
  Nam       : String;
begin
  if SelectedRow >= 0 then
    begin
      Nam  := SelectedTrackName; //TrackNameLB.Items[SelectedRow];
      Track := SelectedTrack; //TTrack(TrackNameLB.Items.Objects[fSelectedRow]);
      if Empty( Nam  ) or ( Nam  = 'Select Section' ) then
        begin
          if Track <> nil then
            Railroad.TrackList.Remove( Track );
          TrackNameLB.Items.Delete(fSelectedRow)
        end
      else
        begin
          if (Track <> nil) then
            if SelectedTrackName = 'Select Section' then
              TrackNameLB.Items.Delete( fSelectedRow );
            begin
              Track.TrackKind := TTrackKind( TrackKindCB.ItemIndex );
              Track.SectionId := -1;
              if SectionCB.ItemIndex >= 0 then
                Track.SectionId := TSection(SectionCB.Items.Objects[SectionCB.ItemIndex]).Id;
            end;
        end;
    end;
  fSelectedRow:=AValue;
  TrackName := '';
  SectionCB.ItemIndex := -1;
  if (fSelectedRow >= 0) and (fSelectedRow < TrackNameLB.Count) then
    begin
      TrackNameLB.ItemIndex := fSelectedRow;
      TrackName := SelectedTrackName; // TrackNameLB.Items[fSelectedRow];
      Track := SelectedTrack;
//      Track := TTrack(TrackNameLB.Items.Objects[fSelectedRow]);
      if Track <> nil then
        begin
          TrackKindCB.ItemIndex := ord( Track.TrackKind );
          for I := 0 to pred(SectionCB.Items.Count) do
            if TSection(SectionCB.Items.Objects[I]).Id = Track.SectionId then
              begin
                SectionCB.ItemIndex := I;
                break;
              end;
        end;
    end
  else
    begin
      TrackKindCB.ItemIndex := ord( tkThrough );
    end;

  TrackNameEdit.Text := TrackName;
  EnableControls;
end;

procedure TTrackForm2.SetSelectedTrack(AValue: TTrack);
begin
  if (fSelectedRow >= 0) and (fSelectedRow < TrackNameLB.Items.Count) then
    TrackNameLB.Items.Objects[fSelectedRow] := AValue
  else
    Debug( 'SetSelectedTrack: fSelectedRow = ' + IntToStr( fSelectedRow ) + #13#10 +
           'TrackNameLB.Items.Count = ' + IntToStr( TrackNameLB.Items.Count ) );
end;

procedure TTrackForm2.SetSelectedTrackName(AValue: String);
begin
  if (fSelectedRow >= 0) and (fSelectedRow < TrackNameLB.Items.Count) then
    TrackNameLB.Items[fSelectedRow] := AValue
  else
    Debug( 'SetSelectedTrackName: fSelectedRow = ' + IntToStr( fSelectedRow ) + #13#10 +
           'TrackNameLB.Items.Count = ' + IntToStr( TrackNameLB.Items.Count ) );
end;

procedure TTrackForm2.SetSelectedTrackSectionId(AValue: Integer);
begin
  SelectedTrack.SectionId := AValue;
end;

procedure TTrackForm2.TrackNameEditChange(Sender: TObject);
begin
  if SelectedRow >= 0 then
    SelectedTrackName := TrackNameEdit.Text;
  EnableControls;
end;

procedure TTrackForm2.TrackNameEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    if not Empty( TrackNameEdit.Text ) then
      if not IsDuplicate( TrackNameEdit.Text ) then
        AppendTrackActionExecute(Sender);
end;

procedure TTrackForm2.TrackNameLBClick(Sender: TObject);
begin
  SelectedRow := TrackNameLB.ItemIndex;
end;

procedure TTrackForm2.TrackNameLBResize(Sender: TObject);
begin
  TrackNameLB.Height := ClientHeight - 20 - TrackNameLB.Top;
end;

procedure TTrackForm2.UpActionExecute(Sender: TObject);
begin
  ExchangeRow( True );
end;

end.

