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
  StringSubs, Common1;

{ TTrackForm }

procedure TTrackForm.AppendTrackActionExecute(Sender: TObject);
var
  RC : Integer;
begin
  RC := TrackNameLB.Items.Count;
  TrackNameLB.AddItem('',nil);
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
          T := TTrack.Create( Railroad.TrackList, TrackNameLB.Items[I] );
          Railroad.TrackList.Add( T );
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
begin
  TrackNameLB.Items.InsertObject( SelectedRow, '', nil  );
  TrackNameEdit.Text := TrackNameLB.Items[SelectedRow ];
  TrackNameLB.ItemIndex := SelectedRow;
end;

function TTrackForm.IsDuplicate(aName: String): boolean;
var
  I : Integer;
begin
  Result := False;
  if Empty( aName ) then exit;
  for I := 0 to pred(TrackNameLB.Items.Count) do
    if I <> fSelectedRow then
      if TrackNameLB.Items[I] = aName then
        begin
          Result := True;
          exit;
        end;
end;

procedure TTrackForm.LoadControls;
var
  I : Integer;
  C : Integer;
  T : TTrack;
begin
  SectionCB.Clear;
  C := Railroad.SectionList.Count;
  for I := 0 to pred( C ) do
    begin
      SectionCB.AddItem(Railroad.SectionList.Items[I].Name, nil );
    end;
  TrackNameLB.Clear;
  C := Railroad.TrackList.Count;
  for I := 0 to pred( C ) do
    begin
      T := Railroad.TrackList.Items[I];
      TrackNameLB.AddItem( T.Name, T );
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
          Track.SectionId := SectionCB.ItemIndex;
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
      SectionCB.ItemIndex := -1;
      TrackKindCB.ItemIndex := ord( tkThrough );
    end;

  TrackNameEdit.Text := TrackName;
  EnableControls;
end;

end.

