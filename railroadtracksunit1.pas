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

unit railroadtracksunit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Persists1, Generics1, RailroadSectionsUnit1, TextIO1;

type
  TTrackKind = ( tkThrough, tkLeftEntryStub, tkRightEntryStub,
                 tkThroughYardOrStation, tkLeftEntryYardOrStation, tkRightEntryYardOrStation,
                 tkLeftEntryLeftTurnout, tkLeftEntryRightTurnout, tkRightEntryLeftTurnout, tkRightEntryRightTurnout,
                 tkLeftCrossover, tkRightCrossover, tkDoubleCrossover,
                 tkLeftSlip, tkRightSlip, tkDoubleSlip,
                 tkCrossing,
                 tkCount ); // tkCount is just to get the end of the enumeration - there is no track associated with it.

  TTrackInternalConnection = ( ticNone,      // There is no connection
                               ticReverse,   // Use if a reverse move is needed
                               ticForward ); // A normal connection

  TTrackExternalConnection = ( tecUL,    //      +------------+
                               tecCL,    // tecUL|            |tecUR
                               tecLL,    // tecCL|            |tecCR
                               tecUR,    // tecLL|            |tecLR
                               tecCR,    //      +------------+
                               tecLR );

  TTrackConnection = array [TTrackInternalConnection] of TTrackInternalConnection;
  //TTrack = class;
  //
  //TTrackEnd = (Left, Right);
  //TTrackLevel = ( Upper, Middle, Lower );
  //TTrackConnection = array [TTrackEnd, TTrackLevel] of TTrack;
  //TTrackConnections = array [TTrackEnd, TTrackLevel] of TTrackConnection;

  { TTrack }

  TTrack = class( TPersists )
  private

    fRailroad : TPersists;     // Remapped to TRailroad internally
    function GetConnection: TTrackConnection;
    function GetSectionName: String;
//    function GetTrack( I : TTrackEnd; J : TTrackLevel): TTrack;
    procedure SetSectionId(AValue: Integer);
//    procedure SetTrack( I : TTrackEnd; J : TTrackLevel; AValue: TTrack);
    procedure SetTrackKind(AValue: TTrackKind);
  protected
    fSectionId : Integer;
    fTrackKind : TTrackKind;
//    fConnections : TTrackConnections;
  public

    constructor Create( AParent : TPersists );

    procedure Save( TextIO : TTextIO ); override;
    procedure Read( TextIO : TTextIO; Version : Integer ); override;
    procedure MakeNew; override;
    procedure UNMODIFY; override;

    procedure Update( var Data : TTrackKind; NewValue : TTrackKind );  overload;

    property SectionName : String read GetSectionName;
    property SectionId   : Integer read fSectionId write SetSectionId;
    property TrackKind   : TTrackKind read fTrackKind write SetTrackKind;
    property Connection : TTrackConnection read GetConnection;
//    property Connection[ I : TTrackEnd; J : TTrackLevel] : TTrack read GetTrack write SetTrack;
  end;

  TTrackListX = specialize TPersistsList<TTrack>;

  { TTrackList }

  TTrackList = class( TTrackListX )
  private
    fRailroad: TPersists;
   published

    constructor Create( aParent : TPersists ); override;
//    property Railroad : TRailroadX read fRailroad write SetRailroad;
  end;

const
  TrackKind : array [TTrackKind] of String =
    ( 'Through', 'Left Entry Stub', 'Right Entry Stub',
      'Through Yard or Station', 'Left Entry Yard or Station', 'Right Entry Yard or Station',
      'Left Entry Left Turnout', 'Left Entry Right Turnout', 'Right Entry Left Turnout', 'Right Entry Right Turnout',
      'Left Crossover', 'Right Crossover', 'Double Crossover',
      'Left Slip Switch', 'Right Slip Switch', 'Double Slip Switch',
      'Crossing',
      '<<<Error>>>');

implementation

uses
  MagicDispatcherRailroadUnit1, ObjectFactory1;

{ TTrackList }


constructor TTrackList.Create(aParent: TPersists);
begin
  inherited Create(aParent);
  fRailroad := TRailroad(aParent);
end;

{ TTrack }

const
  Version = 2;

constructor TTrack.Create(AParent: TPersists);
begin
  inherited Create( AParent );
  fRailroad := AParent.Parent;
end;

function TTrack.GetConnection: TTrackConnection;
begin

end;

function TTrack.GetSectionName: String;
begin
  if fSectionId >= 0 then
    Result := TSection(TRailroad(fRailroad).SectionList.ItemById[ fSectionId ]).Name
  else
    Result := '<Unspecified>';
end;

//function TTrack.GetTrack( I : TTrackEnd; J : TTrackLevel): TTrack;
//begin
//
//end;
//
procedure TTrack.MakeNew;
begin
  inherited MakeNew;
  fTrackKind := tkThrough;
  fSectionId := -1;
end;

procedure TTrack.Read(TextIO: TTextIO; Version: Integer);
var
  Temp : Integer;
begin
  MakeNew;
  if Version > 1 then
    begin
      TextIO.ReadLn( Temp );
      fTrackKind := TTrackKind( Temp );
      TextIO.ReadLn( fSectionId );
    end;
end;

procedure TTrack.Save(TextIO: TTextIO);
begin
  SaveHeader( TextIO, Version );
  TextIO.WriteLn( ord( fTrackKind ) );
  TextIO.WriteLn( fSectionId );
  SaveTrailer( TextIO );
end;

procedure TTrack.SetSectionId(AValue: Integer);
begin
  Update( fSectionId, AValue );
end;

//procedure TTrack.SetTrack( I : TTrackEnd; J : TTrackLevel; AValue: TTrack);
//begin
//
//end;
//
procedure TTrack.SetTrackKind(AValue: TTrackKind);
begin
  Update( fTrackKind, AValue );
end;

procedure TTrack.UNMODIFY;
begin
  inherited UNMODIFY;
end;

procedure TTrack.Update(var Data: TTrackKind; NewValue: TTrackKind);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

initialization
  ObjectFactory.RegisterClass( TTrack.ClassType );
  ObjectFactory.RegisterClass( TTrackList.ClassType );

end.

