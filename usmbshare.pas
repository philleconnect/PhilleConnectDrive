unit USMBShare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, USMBThread;

type
  TSMBShare = class
    private
      path, username, password, SMB, serverpath: string;
      FOnShowStatus: TShowStatusEvent;
      procedure showStatus(status: string);
    public
      constructor create(usernameC, passwordC, pathC, SMBpath, serverpathC: string);
      procedure connect;
      procedure open;
      procedure disconnect;
      property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;

implementation

uses
  PCS;

constructor TSMBShare.create(usernameC, passwordC, pathC, SMBpath, serverpathC: string);
begin
  username:=usernameC;
  password:=passwordC;
  path:=pathC;
  SMB:=SMBpath;
  serverpath:=serverpathC;
end;

procedure TSMBShare.connect;
var
  thread: TSMBThread;
  {$IFDEF LINUX}
    process: TProcess;
    response: TStringList;
    localUserName: string;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
    thread:=TSMBThread.create(true, 'cmd.exe', '/C net use /persistent:no '+path+' \\'+SMB+'\'+serverpath+' '+password+' /user:'+username);
    thread.OnShowStatus:=@ShowStatus;
    thread.resume;
  {$ENDIF}
  {$IFDEF LINUX}
    response:=TStringList.create;
    process:=TProcess.create(nil);
    process.executable:='sh';
    process.parameters.add('-c');
    process.parameters.add('mkdir '+path);
    process.Options:=process.Options + [poWaitOnExit, poUsePipes];
    process.ShowWindow:=swoHIDE;
    process.execute;
    process.free;
    process:=TProcess.create(nil);
    process.executable:='sh';
    process.parameters.add('-c');
    process.parameters.add('logname');
    process.Options:=process.Options + [poWaitOnExit, poUsePipes];
    process.ShowWindow:=swoHIDE;
    process.execute;
    response.LoadFromStream(process.output);
    localUserName:=response.commaText;
    process.free;
    thread:=TSMBThread.create(true, 'sh', 'mount -t cifs -o username='+username+',password='+password+',uid='+localUserName+' //'+SMB+'/'+serverpath+' '+path);
    thread.OnShowStatus:=@ShowStatus;
    thread.resume;
  {$ENDIF}
end;

procedure TSMBShare.open;
var
  thread: TSMBThread;
begin
  {$IFDEF WINDOWS}
    thread:=TSMBThread.create(false, 'explorer.exe', path+'\');
    thread.OnShowStatus:=@ShowStatus;
    thread.resume;
  {$ENDIF}
  {$IFDEF LINUX}
    thread:=TSMBThread.create(false, 'sh', 'dolphin '+path);
    thread.OnShowStatus:=@ShowStatus;
    thread.resume;
  {$ENDIF}
end;

procedure TSMBShare.disconnect;
var
  process: TProcess;
begin
  {$IFDEF WINDOWS}
    process:=TProcess.create(nil);
    process.executable:='cmd';
    process.parameters.add('/C net use '+path+' /delete /y');
    process.Options:=process.Options + [poWaitOnExit, poUsePipes];
    process.ShowWindow:=swoHIDE;
    process.execute;
    process.free;
  {$ENDIF}
  {$IFDEF LINUX}
    process:=TProcess.create(nil);
    process.executable:='sh';
    process.parameters.add('-c');
    process.parameters.add('umount '+path);
    process.Options:=process.Options + [poWaitOnExit, poUsePipes];
    process.ShowWindow:=swoHIDE;
    process.execute;
    process.free;
  {$ENDIF}
end;

procedure TSMBShare.showStatus(status: string);
begin
  if Assigned(FOnShowStatus) then
    begin
      FOnShowStatus(status);
    end;
end;

end.
