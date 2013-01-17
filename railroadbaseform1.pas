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

unit railroadbaseform1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  MagicDispatcherRailroadUnit1;

type

  { TRailroadBaseForm }

  TRailroadBaseForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    fRailroad: TRailroad;
    procedure SetRailroad(AValue: TRailroad);
  protected
    procedure LoadControls; virtual; abstract;

  public
    { public declarations }
    property Railroad : TRailroad read fRailroad write SetRailroad;
  end;

var
  RailroadBaseForm: TRailroadBaseForm;

implementation

{$R *.lfm}

{ TRailroadBaseForm }

procedure TRailroadBaseForm.FormCreate(Sender: TObject);
begin
  Visible := False;
end;

procedure TRailroadBaseForm.SetRailroad(AValue: TRailroad);
begin
//  if fRailroad=AValue then Exit;
  fRailroad:=AValue;
  LoadControls;
end;

end.

