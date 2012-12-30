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
begin
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

