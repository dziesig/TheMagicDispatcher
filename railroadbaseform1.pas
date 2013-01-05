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

