//Copyright (c) 2012 by Donald R. Ziesig
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

unit MagicDispatcherRailroadUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls,

  Persists1, TextIO1, PicturePersists1, RailroadSectionsUnit1,
  RailroadTracksUnit1;

type

  { TRailroad }

  TRailroad = class( TPersists )
  private
    fExtraTrainName: String;
    fSectionList: TSectionList;
    fSuperiorDirection : String;
    fInferiorDirection : String;
    fSuperiorIsOdd     : Boolean;
    fLogo         : TPicturePersists;
    fTrackList: TTrackList;
    function GetLogo: TPicture;
    procedure SetExtraTrainName(AValue: String);
    procedure SetInferiorDirection(AValue: String);
    procedure SetSectionList(AValue: TSectionList);
    procedure SetSuperiorDirection(AValue: String);
    procedure SetSuperiorIsOdd(AValue: Boolean);
    procedure SetLogo(AValue: TPicture);
    procedure SetTrackList(AValue: TTrackList);
  public

    procedure Save( TextIO : TTextIO ); override;
    procedure Read( TextIO : TTextIO; Version : Integer ); override;
    procedure MakeNew; override;
    procedure UNMODIFY; override;

    property Logo         : TPicture read GetLogo       write SetLogo;
    property SuperiorDirection : String  read fSuperiorDirection write SetSuperiorDirection;
    property InferiorDirection : String  read fInferiorDirection write SetInferiorDirection;
    property SuperiorIsOdd     : Boolean read fSuperiorIsOdd     write SetSuperiorIsOdd;
    property ExtraTrainName    : String  read fExtraTrainName    write SetExtraTrainName;
    property SectionList       : TSectionList read fSectionList  write SetSectionList;
    property TrackList         : TTrackList   read fTrackList    write SetTrackList;
end; // TMagicDispatcherRailroad



implementation

uses
  ObjectFactory1;

{ TRailroad }

const
  Version = 4;

function TRailroad.GetLogo: TPicture;
begin
  Result := fLogo.Picture;
end;

procedure TRailroad.SetExtraTrainName(AValue: String);
begin
  Update( fExtraTrainName, AValue);
end;

procedure TRailroad.SetInferiorDirection(AValue: String);
begin
  Update( fInferiorDirection, AValue );
end;

procedure TRailroad.SetSectionList(AValue: TSectionList);
begin
  if fSectionList=AValue then Exit;
  fSectionList:=AValue;
end;

procedure TRailroad.SetSuperiorDirection(AValue: String);
begin
  Update( fSuperiorDirection, AValue );
end;

procedure TRailroad.SetSuperiorIsOdd(AValue: Boolean);
begin
  Update( fSuperiorIsOdd, AValue );
end;

procedure TRailroad.SetLogo(AValue: TPicture);
var
  OldLogo : String;
begin
  OldLogo := fLogo.Pixval;
  fLogo.Picture := AValue;
  if OldLogo <> fLogo.Pixval then
    Modify;
end;

procedure TRailroad.SetTrackList(AValue: TTrackList);
begin
//  if fTrackList=AValue then Exit;
  fTrackList:=AValue;
end;


procedure TRailroad.Save(TextIO: TTextIO);
begin
  SaveHeader( TextIO, Version );
  TextIO.WriteLn( fSuperiorDirection );
  TextIO.WriteLn( fInferiorDirection );
  TextIO.WriteLn( fSuperiorIsOdd );
  fLogo.Save( TextIO );
  TextIO.WriteLn( fExtraTrainName );
  fSectionList.Save( TextIO );
  fTrackList.Save( TextIO );
  SaveTrailer( TextIO );
end;

procedure TRailroad.Read(TextIO: TTextIO; Version: Integer);
begin
  MakeNew;
  if Version >= 1 then
    begin
      TextIO.ReadLn( fSuperiorDirection );
      TextIO.ReadLn( fInferiorDirection );
      TextIO.ReadLn( fSuperiorIsOdd );
      fLogo := TPicturePersists.Load( TextIO ) as TPicturePersists;
    end;
  if Version >= 2 then
    begin
      TextIO.ReadLn( fExtraTrainName );
    end;
  if Version >= 3 then
    begin
      fSectionList.Free;
      fSectionList := TSectionList.Load( TextIO ) as TSectionList;
      fSectionList.Parent := Self;
    end;
  if Version >= 4 then
    begin
      fTrackList.Free;
      fTrackList := TTrackList.Load( TextIO ) as TTrackList;
      fTrackList.Parent := Self;
    end;
end;

procedure TRailroad.MakeNew;
begin
  inherited;
  fSuperiorDirection := 'East';
  fInferiorDirection := 'West';
  fSuperiorIsOdd     := False;
  fExtraTrainName    := 'Extra';
  if Assigned( fLogo ) then fLogo.Free;
  fLogo := TPicturePersists.Create( self );
  if Assigned( fSectionList ) then fSectionList.free;
  fSectionList := TSectionList.Create( self );
  if Assigned( fTrackList ) then fTrackList.Free;
  fTrackList := TTrackList.Create( self );
  UNMODIFY
end;

procedure TRailroad.UNMODIFY;
begin
  inherited UNMODIFY;
  if Assigned( fLogo ) then
    fLogo.UNMODIFY;
  if Assigned( fSectionList ) then
    fSectionList.UNMODIFY;
  if Assigned( fTrackList ) then
    fTrackList.UNMODIFY;
end;

initialization
  ObjectFactory.RegisterClass( TRailroad.ClassType );


end.

