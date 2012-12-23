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

unit MagicDispatcherMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, StdCtrls,

  MagicDispatcherRailroadUnit1, MagicFormFrame1, MagicDispatcherRailroadForm1,
  RailroadDefaultsForm1, RailroadSectionsForm1;

type

  { TForm1 }

  TForm1 = class(TForm)
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    RailroadSections: TAction;
    RailroadDefaults: TAction;
    MenuItem10: TMenuItem;
    RailroadName: TAction;
    FileExitAction: TAction;
    FileSaveAsAction: TAction;
    FileSaveAction: TAction;
    FileCloseAction: TAction;
    FileOpenAction: TAction;
    FileNewAction: TAction;
    ActionList1: TActionList;
    PrimaryFrame: TFrame1;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    FileNewMenuItem: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    SaveDialog1: TSaveDialog;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    TabSheet2: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure FileCloseActionExecute(Sender: TObject);
    procedure FileExitActionExecute(Sender: TObject);
    procedure FileNewActionExecute(Sender: TObject);
    procedure FileOpenActionExecute(Sender: TObject);
    procedure FileSaveActionExecute(Sender: TObject);
    procedure FileSaveAsActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure RailroadDefaultsExecute(Sender: TObject);
    procedure RailroadNameExecute(Sender: TObject);
    procedure RailroadSectionsExecute(Sender: TObject);
  private
    { private declarations }
    fRailroad: TMagicDispatcherRailroad;

    RRForm : TRailroadForm1;
    DefaultsForm : TDefaultsForm;
    SectionsForm : TSectionsForm;
    procedure SetRailroad(AValue: TMagicDispatcherRailroad);

    function  IfSaveNecessary : Boolean;
    procedure RRChanged( Sender : TObject ); // Event fired when any component
                                             // of fRailroad changes.
  public
    { public declarations }
    property Railroad : TMagicDispatcherRailroad read fRailroad write SetRailroad;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  TextIO1, Common1;

{ TForm1 }

const
  UntitledRR = 'UNTITLED RR';
  NoRRFile   = 'NOFILE.mdrr';
  TitleLeader = 'The Magic Dispatcher - ';

procedure TForm1.FileNewActionExecute(Sender: TObject);
begin
  fRailroad.Free;
  Railroad := TMagicDispatcherRailroad.Create( nil, UntitledRR );
  StatusBar1.Panels[1].Text := NoRRFile;
end;

procedure TForm1.FileCloseActionExecute(Sender: TObject);
begin

end;

procedure TForm1.FileExitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FileOpenActionExecute(Sender: TObject);
begin

end;

procedure TForm1.FileSaveActionExecute(Sender: TObject);
begin

end;

procedure TForm1.FileSaveAsActionExecute(Sender: TObject);
var
  TextIO : TTextIO;
begin
  SaveDialog1.InitialDir := DefaultSaveLocation;
  if SaveDialog1.Execute then
    begin
      TextIO := TTextIO.Create( SaveDialog1.FileName, True );
      fRailroad.Save( TextIO );
      TextIO.Free;
    end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Railroad.Modified then
    if MessageDlg( 'Magic Dispatcher', Railroad.Name + ' has been modified.'#13#10'Do You want to save the file?',
                   mtConfirmation,
                   [mbYes, mbNo, mbCancel],0) = mrYes then
      begin
        CanClose := False;
      end
    else
      CanClose := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RRForm := TRailroadForm1.Create( self );
  DefaultsForm := TDefaultsForm.Create( self );
  SectionsForm := TSectionsForm.Create( self );
  Railroad := TMagicDispatcherRailroad.Create( nil, UntitledRR );
  PrimaryFrame.Form := RRForm;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  StatusBar1.Panels[2].Text := IntToStr( Height );
  StatusBar1.Panels[3].Text := IntToStr( Width );
end;

procedure TForm1.RailroadDefaultsExecute(Sender: TObject);
begin
  PrimaryFrame.Form := DefaultsForm
end;

procedure TForm1.RailroadNameExecute(Sender: TObject);
begin
  PrimaryFrame.Form := RRForm;
end;

procedure TForm1.RailroadSectionsExecute(Sender: TObject);
begin
  PrimaryFrame.Form := SectionsForm;
end;

procedure TForm1.SetRailroad(AValue: TMagicDispatcherRailroad);
begin
  if fRailroad=AValue then Exit;
  fRailroad:=AValue;
  if Assigned( fRailroad ) then
    Caption := TitleLeader + fRailroad.Name
  else
    Caption := TitleLeader + 'nil  This should never happen.';
  RRForm.Railroad := fRailroad;
  DefaultsForm.Railroad := fRailroad;
  SectionsForm.Railroad := fRailroad;
  fRailroad.OnChange := @RRChanged;
end;

function TForm1.IfSaveNecessary: Boolean;
begin

end;

procedure TForm1.RRChanged(Sender: TObject);
begin
  Caption := TitleLeader + (Sender as TMagicDispatcherRailroad).Name;
end;

end.

