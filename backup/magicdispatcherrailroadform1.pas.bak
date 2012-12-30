unit magicdispatcherrailroadform1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ExtDlgs, MagicDispatcherRailroadUnit1, railroadbaseform1;

type

  { TRailroadForm1 }

  TRailroadForm1 = class(TRailroadBaseForm)
    Button1: TButton;
    Button2: TButton;
    ExtraNameCB: TComboBox;
    Label3: TLabel;
    SuperiorIsOddCkB: TCheckBox;
    SuperiorDirectionCB: TComboBox;
    InferiorDirectionCB: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    LabeledEdit1: TLabeledEdit;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  Image1.Picture := Railroad.Logo;
  SuperiorDirectionCB.Text := Railroad.SuperiorDirection;
  InferiorDirectionCB.Text := Railroad.InferiorDirection;
  SuperiorIsOddCkB.Checked := Railroad.SuperiorIsOdd;
  ExtraNameCB.Text         := Railroad.ExtraTrainName;
end;

procedure TRailroadForm1.FormHide(Sender: TObject);
begin
  Railroad.Name         := LabeledEdit1.Text;
  Railroad.Logo         := Image1.Picture;
  Railroad.SuperiorDirection := SuperiorDirectionCB.Text;
  Railroad.InferiorDirection := InferiorDirectionCB.Text;
  Railroad.SuperiorIsOdd     := SuperiorIsOddCkB.Checked;
  Railroad.ExtraTrainName    := ExtraNameCB.Text;
end;

procedure TRailroadForm1.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    begin
      Image1.Picture.LoadFromFile( OpenPictureDialog1.FileName );
    end;
end;

procedure TRailroadForm1.Button2Click(Sender: TObject);
begin
  Image1.Picture.Clear;
end;

procedure TRailroadForm1.FormCreate(Sender: TObject);
  procedure FixComboBox( CB : TComboBox );
  var
    I, J : Integer;
    S : String;
  begin
    for I := 0 To pred(CB.Items.Count) do
      begin
        S := CB.Items[I];
        for J := 1 to Length(S) do
          if S[J] = '~' then
            S[J] := #9;
        CB.Items[I] := S;
      end;
  end;

begin
  inherited;
  FixComboBox( SuperiorDirectionCB );
  FixComboBox( InferiorDirectionCB );
end;

end.

