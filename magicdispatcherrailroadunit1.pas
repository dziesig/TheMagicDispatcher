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

    procedure Save( TextIO : TTextIO ); virtual;
    procedure Read( TextIO : TTextIO; Version : Integer ); virtual;
    procedure MakeNew; virtual;

  {$ifdef USE_GRAPHICS}
    property Logo : TBitmap read fLogo write SetLogo;
  {$endif}
    property NorthSouth : Boolean   read fNorthSouth   write SetNorthSouth;
    property EastPriority : Boolean read fEastPriority write SetEastPriority;
  end; // TMagicDispatcherRailroad



implementation

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
  Name := 'UNDEFINED';
  fNorthSouth := False;
  fEastPriority := True;
end;

end.

