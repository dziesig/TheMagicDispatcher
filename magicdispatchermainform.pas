unit magicdispatchermainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, PrintersDlgs, Forms, Controls, Graphics, Dialogs,
  ComCtrls, Menus, ActnList, MagicDispatcherRailroadUnit1, magicmainformbase1,
  MagicDispatcherRailroadForm1, RailroadDefaultsForm1, RailroadSectionsForm1,
  RailroadTracksForm1,
  MagicFormFrame1;

type

  { TMainForm }

  TMainForm = class(TMagicMainFormBase)
    MenuItem18: TMenuItem;
    RailroadTracksAction: TAction;
    MenuItem17: TMenuItem;
    RailroadSectionsAction: TAction;
    RailroadDefaultsAction: TAction;
    MenuItem15: TMenuItem;
    RailroadRailroadAction: TAction;
    MenuItem14: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure RailroadDefaultsActionExecute(Sender: TObject);
    procedure RailroadRailroadActionExecute(Sender: TObject);
    procedure RailroadSectionsActionExecute(Sender: TObject);
    procedure RailroadTracksActionExecute(Sender: TObject);
  private
    { private declarations }
    procedure FileNew; override;
    procedure FileOpen( FileName : String ); override;
    procedure FileSave; override;
    function GetRailroad: TRailroad;
    procedure SetRailroad(AValue: TRailroad);

    procedure RRChanged( Sender : TObject ); // Event fired when any component
                                             // of fRailroad changes.
  protected
    RRForm : TRailroadForm1;
    DefaultsForm : TDefaultsForm;
    SectionsForm : TSectionsForm;
    TracksForm   : TTrackForm;

    procedure UpdateData; override;

  public
    { public declarations }
    property Railroad : TRailroad read GetRailroad write SetRailroad;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  TextIO1, CursorStackUnit1;

const
  UntitledRR = 'UNTITLED RR';


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  { Initialize the app-specific data }
  vDefaultExt := 'mdrr';
  vFileFilter := 'Magic Dispatcher Railroad|*.mdrr|Any File|*.*;*'; { For Win and Lin }
  vAppName    := 'The Magic Dispatcher';
  CursorStack := TFormCursorStack.Create( self );
  RRForm := TRailroadForm1.Create( self );
  DefaultsForm := TDefaultsForm.Create( self );
  SectionsForm := TSectionsForm.Create( self );
  TracksForm := TTrackForm.Create( self );
  RRForm.Visible := False;
  Railroad := TRailroad.Create( nil, UntitledRR );
  CurrentFile := NoFile;

  PrimaryFrame.Form := RRForm;

end;

procedure TMainForm.RailroadDefaultsActionExecute(Sender: TObject);
begin
  PrimaryFrame.Form := DefaultsForm;
end;

procedure TMainForm.RailroadRailroadActionExecute(Sender: TObject);
begin
  PrimaryFrame.Form := RRForm;
end;

procedure TMainForm.RailroadSectionsActionExecute(Sender: TObject);
begin
  PrimaryFrame.Form := SectionsForm;
end;

procedure TMainForm.RailroadTracksActionExecute(Sender: TObject);
begin
  PrimaryFrame.Form := TracksForm;
end;

procedure TMainForm.FileNew;
var
  RR : TRailroad; // for debug
begin
  inherited;
  Data.Free;
  RR := TRailroad.Create( nil, UntitledRR );
  Railroad := RR;
  Railroad.UNMODIFY;
end;

procedure TMainForm.FileOpen(FileName: String);
var
  TextIO : TTextIO;
begin
  CursorStack.Push( crHourGlass );
  TextIO := TTextIO.Create( FileName, False );
  Data.Free;
  Railroad := TRailroad.Load( TextIO ) as TRailroad;
  TextIO.Free;
  CursorStack.Pop;
end;

procedure TMainForm.FileSave;
var
  TextIO : TTextIO;
begin
  CursorStack.Push( crHourGlass );
  TextIO := TTextIO.Create( CurrentFile, True );
  Railroad.Save( TextIO );
  TextIO.Free;
  CursorStack.Pop;
end;

function TMainForm.GetRailroad: TRailroad;
begin
  Result := Data as TRailroad;
end;

procedure TMainForm.SetRailroad(AValue: TRailroad);
begin
  Data := aValue;// as TPersists1;
  RRForm.Railroad := Railroad;
  DefaultsForm.Railroad := Railroad;
  SectionsForm.Railroad := Railroad;
  TracksForm.Railroad   := Railroad;
  Railroad.OnChange := @RRChanged;
end;

procedure TMainForm.RRChanged(Sender: TObject);
begin
  Data := (Sender as TRailroad);
end;

procedure TMainForm.UpdateData;
begin
  PrimaryFrame.Form.Hide;   // Hack to copy the controls to the data
  PrimaryFrame.Form.Show;   // Then restore the display
end;

end.

