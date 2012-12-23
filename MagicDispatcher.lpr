program MagicDispatcher;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, printer4lazarus, MagicDispatcherRailroadUnit1,
  persists1, TextIO1, Common1, MagicFormFrame1, magicdispatcherrailroadform1,
  railroaddefaultsform1, railroadsectionsform1, railroadbaseform1,
  magicmainformbase1, CursorStackUnit1, magicdispatchermainform
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

