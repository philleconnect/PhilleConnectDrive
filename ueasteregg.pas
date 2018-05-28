unit UEasterEgg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TEasterEgg }

  TEasterEgg = class(TForm)
    infoLabel1: TLabel;
    infoLabel2: TLabel;
    infoLabel3: TLabel;
    infoLabel4: TLabel;
    ok: TButton;
    kidding: TImage;
    procedure okClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  EasterEgg: TEasterEgg;

implementation

{$R *.lfm}

{ TEasterEgg }

procedure TEasterEgg.okClick(Sender: TObject);
begin
  EasterEgg.close;
end;

end.

