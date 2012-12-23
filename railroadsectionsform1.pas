unit railroadsectionsform1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  RailroadBaseForm1;

type

  { TSectionsForm }

  TSectionsForm = class(TRailroadBaseForm)
    Label1: TLabel;
  private
    { private declarations }
  protected
    procedure LoadControls; override;
  public
    { public declarations }
  end;

var
  SectionsForm: TSectionsForm;

implementation

{$R *.lfm}

{ TSectionsForm }

procedure TSectionsForm.LoadControls;
begin

end;

end.

