unit railroaddefaultsform1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  railroadbaseform1;

type

  { TDefaultsForm }

  TDefaultsForm = class(TRailroadBaseForm)
    Label1: TLabel;
  private
    { private declarations }
  protected
    procedure LoadControls; override;
  public
    { public declarations }
  end;

var
  DefaultsForm: TDefaultsForm;

implementation

{$R *.lfm}

{ TDefaultsForm }

procedure TDefaultsForm.LoadControls;
begin

end;

end.

