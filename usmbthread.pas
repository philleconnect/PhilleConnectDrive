unit USMBThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

type
  TShowStatusEvent = procedure(Status: String) of Object;

  TSMBThread = class(TThread)
    private
      fStatusText: string;
      FOnShowStatus: TShowStatusEvent;
      output: boolean;
      exe, options: string;
      procedure showStatus;
    protected
      procedure execute; override;
    public
      constructor create(outputC: boolean; exeC, optionsC: string);
      property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;

implementation

constructor TSMBThread.create(outputC: boolean; exeC, optionsC: string);
begin
  output:=outputC;
  exe:=exeC;
  options:=optionsC;
  FreeOnTerminate:=true;
  inherited create(true);
end;

procedure TSMBThread.showStatus;
begin
  if Assigned(FOnShowStatus) then
    begin
      FOnShowStatus(fStatusText);
    end;
end;

procedure TSMBThread.execute;
var
  smbProcess: TProcess;
  response: TStringList;
  error: string;
begin
  smbProcess:=TProcess.create(nil);
  if (output) then begin
    response:=TStringList.create;
  end;
  smbProcess.executable:=exe;
  {$IFDEF LINUX}
  smbProcess.parameters.add('-c');
  {$ENDIF}
  smbProcess.parameters.add(options);
  smbProcess.Options:=smbProcess.Options + [poWaitOnExit, poUsePipes];
  if (exe = 'cmd.exe') or (exe = 'sh') then begin
    smbProcess.ShowWindow:=swoHIDE;
  end;
  smbProcess.execute;
  if (output) then begin
    response.LoadFromStream(smbProcess.stderr);
  end;
  smbProcess.free;
  if (output) then begin
    error:=response.commaText;
    {$IFDEF WINDOWS}
    if (pos('86', error) <> 0) then begin //Passwort falsch
      fStatusText:='1';
    end
    else if (pos('85', error) <> 0) then begin //Laufwerksbuchstabe vergeben
      fStatusText:='2';
    end
    else if (pos('53', error) <> 0) then begin //Share nicht gefunden
      fStatusText:='3';
    end
    else if (pos('1219', error) <> 0) then begin //Mehrere Nutzer auf einem
    //Server (Windows-Bug?)
      fStatusText:='4';
    end
    else if (error = '') then begin //Alles OK
      fStatusText:='0';
    end;
    {$ENDIF}
    {$IFDEF LINUX}
    if (pos('13', error) <> 0) then begin //Passwort falsch
      fStatusText:='1';
    end
    else if (pos('6', error) <> 0) then begin //Share nicht gefunden
      //fStatusText:='3';
      fStatusText:=error;
    end
    else if (error = '') then begin //Alles OK
      fStatusText:='0';
    end;
    {$ENDIF}
    Synchronize(@ShowStatus);
  end;
end;

end.

