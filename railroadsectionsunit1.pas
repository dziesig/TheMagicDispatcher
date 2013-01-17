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

unit railroadsectionsunit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Persists1, Generics1, TextIO1;

type

  { TSection }

  TSection = class(TPersists)
  private

  public
    procedure Save( TextIO : TTextIO ); override;
    procedure Read( TextIO : TTextIO; Version : Integer ); override;
    procedure MakeNew; override;
    procedure UNMODIFY; override;

  end;

  TSectionList = specialize TPersistsList<TSection>;

implementation

uses
  ObjectFactory1;

{ TSection }

const
  Version = 1;

procedure TSection.Save(TextIO: TTextIO);
var
  N : String;
  I, O : Integer;
begin
  N := Name;
  I := Id;
  O := Order;
  SaveHeader( TextIO, Version );
  SaveTrailer( TextIO );
end;

procedure TSection.Read(TextIO: TTextIO; Version: Integer);
begin
  MakeNew;
end;

procedure TSection.MakeNew;
begin
  inherited MakeNew;
end;

procedure TSection.UNMODIFY;
begin
  inherited UNMODIFY;
end;

initialization
ObjectFactory.RegisterClass( TSection.ClassType );
ObjectFactory.RegisterClass( TSectionList.ClassType );

end.

