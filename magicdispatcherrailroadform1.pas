unit magicdispatcherrailroadform1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, MagicDispatcherRailroadUnit1, railroadbaseform1;

type

  { TRailroadForm1 }

  TRailroadForm1 = class(TRailroadBaseForm)
    Edit1: TEdit;
    LabeledEdit1: TLabeledEdit;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabeledEdit1Exit(Sender: TObject);
  private
    { private declarations }
  protected
    procedure LoadControls; override;
  public
    { public declarations }
  end;

var
  RailroadForm1: TRailroadForm1;

implementation

{$R *.lfm}

{ TRailroadForm1 }

procedure TRailroadForm1.FormShow(Sender: TObject);
begin
  LoadControls;
end;

procedure TRailroadForm1.LabeledEdit1Exit(Sender: TObject);
begin
  Railroad.Name := LabeledEdit1.Text;
end;

procedure TRailroadForm1.LoadControls;
begin
  LabeledEdit1.Text := Railroad.Name;
end;

procedure TRailroadForm1.FormHide(Sender: TObject);
begin
  Railroad.Name := LabeledEdit1.Text;
end;

end.

