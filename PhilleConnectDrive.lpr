program PhilleConnectStart;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, PCS, USMBShare, UGetMacAdress, UChangePassword,
  UResetStudentPassword, USMBThread, UEasterEgg, URequestThread
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(Twindow, window);
  Application.CreateForm(TCPForm, CPForm);
  Application.CreateForm(TRSPForm, RSPForm);
  Application.CreateForm(TEasterEgg, EasterEgg);
  Application.Run;
end.

