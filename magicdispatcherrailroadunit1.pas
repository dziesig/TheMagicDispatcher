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

{$define USE_GRAPHICS}
interface

uses
  Classes, SysUtils, Graphics,

  Persists1, TextIO1;

type

  { TMagicDispatcherRailroad }

  TMagicDispatcherRailroad = class( TPersists )
  private
  {$ifdef USE_GRAPHICS}
    fLogo         : TBitmap;
  {$endif}
    fNorthSouth   : Boolean; // If False Railroad uses East/West timetable
    fEastPriority : Boolean; // East(North)bound Trains have priority
    procedure SetEastPriority(AValue: Boolean);
    procedure SetLogo(AValue: TBitmap);
    procedure SetNorthSouth(AValue: Boolean);

  public

    procedure Save( TextIO : TTextIO ); override;
    procedure Read( TextIO : TTextIO; Version : Integer ); override;
    procedure MakeNew; override;

  {$ifdef USE_GRAPHICS}
    property Logo : TBitmap read fLogo write SetLogo;
  {$endif}
    property NorthSouth : Boolean   read fNorthSouth   write SetNorthSouth;
    property EastPriority : Boolean read fEastPriority write SetEastPriority;
  end; // TMagicDispatcherRailroad



implementation

uses
  ObjectFactory1;

{ TMagicDispatcherRailroad }

const
  Version = 1;

procedure TMagicDispatcherRailroad.SetEastPriority(AValue: Boolean);
begin
  Update( fEastPriority, AValue );
end;

{$ifdef USE_GRAPHICS}
procedure TMagicDispatcherRailroad.SetLogo(AValue: TBitmap);
begin
  if fLogo=AValue then Exit;
  fLogo:=AValue;
end;
{$endif}

procedure TMagicDispatcherRailroad.SetNorthSouth(AValue: Boolean);
begin
  Update( fNorthSouth, AValue );
end;

procedure TMagicDispatcherRailroad.Save(TextIO: TTextIO);
begin
  SaveHeader( TextIO, Version );
  TextIO.WriteLn( fNorthSouth );
  TextIO.WriteLn( fEastPriority );
  UNMODIFY;
  SaveTrailer( TextIO );
end;

procedure TMagicDispatcherRailroad.Read(TextIO: TTextIO; Version: Integer);
begin
  MakeNew;
  if Version >= 1 then
    begin
      TextIO.ReadLn( fNorthSouth );
      TextIO.ReadLn( fEastPriority );
    end;
end;

procedure TMagicDispatcherRailroad.MakeNew;
begin
  inherited;
  fNorthSouth := False;
  fEastPriority := True;
end;

initialization
  ObjectFactory.RegisterClass( TMagicDispatcherRailroad.ClassType );


end.

