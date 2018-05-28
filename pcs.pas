//Copyright 2016-2018 Johannes Kreutz.
//Alle Rechte vorbehalten.
unit PCS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, USMBShare, HTTPSend, synacode, fpjson, jsonparser, LCLType,
  UGetMacAdress, UGetIPAdress, StrUtils, UChangePassword, ssl_openssl,
  UEasterEgg, resolve, UPingThread, lclintf, URequestThread;

type

  { Twindow }

  Twindow = class(TForm)
    actionLabel: TLabel;
    sHelpMenu: TMenuItem;
    onlineInfos: TMenuItem;
    noNetworkSub: TLabel;
    noNetworkHead: TLabel;
    noNetworkInfo: TLabel;
    pwChangeButton: TButton;
    D2L1: TLabel;
    D2L2: TLabel;
    D2L3: TLabel;
    D3L1: TLabel;
    D3L2: TLabel;
    D3L3: TLabel;
    D1logout: TButton;
    D2logout: TButton;
    D3logout: TButton;
    D1open: TButton;
    D2open: TButton;
    D3open: TButton;
    drive2Box: TGroupBox;
    drive3Box: TGroupBox;
    fadingArrow: TImage;
    drive1Box: TGroupBox;
    D1L1: TLabel;
    D1L2: TLabel;
    D1L3: TLabel;
    D1STAT: TLabel;
    D1USER: TLabel;
    D1PATH: TLabel;
    D2STAT: TLabel;
    D2USER: TLabel;
    D2PATH: TLabel;
    D3STAT: TLabel;
    D3USER: TLabel;
    D3PATH: TLabel;
    reloadTimer: TTimer;
    usersBox: TGroupBox;
    newsBox: TGroupBox;
    loginBox: TGroupBox;
    infoLabel: TLabel;
    infoLabel2: TLabel;
    mainMenu: TMainMenu;
    driveMenu: TMenuItem;
    accountMenu: TMenuItem;
    openDrive1: TMenuItem;
    changePassword: TMenuItem;
    openDrive2: TMenuItem;
    openDrive3: TMenuItem;
    news: TMemo;
    search: TEdit;
    arrowTimer: TTimer;
    usernames: TListBox;
    versionLabel: TLabel;
    loginButton: TButton;
    clearButton: TButton;
    passwd: TEdit;
    uname: TEdit;
    headline: TLabel;
    logo: TImage;
    procedure arrowTimerTimer(Sender: TObject);
    procedure changePasswordClick(Sender: TObject);
    procedure clearButtonClick(Sender: TObject);
    procedure D1logoutClick(Sender: TObject);
    procedure D1openClick(Sender: TObject);
    procedure D2logoutClick(Sender: TObject);
    procedure D2openClick(Sender: TObject);
    procedure D3logoutClick(Sender: TObject);
    procedure D3openClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure loginButtonClick(Sender: TObject);
    procedure onlineInfosClick(Sender: TObject);
    procedure reloadTimerTimer(Sender: TObject);
    procedure searchChange(Sender: TObject);
    procedure loadUsers;
    procedure usernamesSelectionChange(Sender: TObject; User: boolean);
  private
    arrowDirection: boolean;
    timerCounter: integer;
    //Netzwerkverbindung prüfen
    procedure checkNetworkConnection;
    procedure networkConnectionResult(result: boolean; return: string);
    procedure trueNetworkResult;
    procedure falseNetworkResult;
    //Konfigurationsdatei / Serverkonfiguration laden
    procedure parseConfigFile;
    procedure loadConfig;
    procedure loadConfigResponse(response: string);
    //Nutzerliste laden
    procedure loadUsersResponse(response: string);
    //Inputs sperren / entsperren
    procedure lockInputs(mode: boolean);
    //Oberfläche sperren / entsperren
    procedure lockUI(mode: boolean);
    //Anmelden und Share einbinden
    procedure doLogin;
    //SMB Thread Rückgabewert für Laufwerk 1 verarbeiten
    procedure handleOne(status: string);
    //SMB Thread Rückgabewert für Laufwerk 2 verarbeiten
    procedure handleTwo(status: string);
    //SMB Thread Rückgabewert für Laufwerk 3 verarbeiten
    procedure handleThree(status: string);
    //Laufwerks-UI sperren / freigeben
    procedure handleConnectResult(share, connect: integer);
    //UI für Laufwerk 1 freigeben / sperren
    procedure controlShareUI(share: integer; state: boolean);
    //Abmelden & Share ausbinden
    procedure doLogout(share: integer);
    //Mit Daten von START anmelden
    procedure doFileLogin;
    //Hinweisfeld laden
    procedure loadTextBox(infotext: string);
    //Gruppenlaufwerke einbinden
    procedure mountGroupFolders(username, pw: string);
    //SMB Thread Rückgabewert für Gruppenlaufwerke verabeiten
    procedure handleGroup(status: string);
    //Zugriff ohne Serververbindung erlauben / sperren
    procedure setAllowOffline(value: string);
    procedure setHelpURL(url: string);
    function sendRequest(url, params: string): string;
    function MemStreamToString(Strm: TMemoryStream): AnsiString;
    function ValidateIP(IP4: string): Boolean;
  public

  end;

var
  window: Twindow;
  d1user, d2user, d3user, serverURL, SMBserverURL, globalPW, login,
  loginPending, loginFailed, wrongCredentials, networkFailed, mac,
  gfdata, ip, cleanServerURL, helpURL: string;
  userdata, fullUserdata, searchUserdata: TStrings;
  shares: array[0..2] of TSMBShare;
  groupfolders: array of TSMBShare;
  drives: array[0..2] of string;
  drivePaths: array[0..2] of string;
  gfcount, actualNetworkRetry, networkRetry: integer;
  groupFoldersMounted, showedGroupfolderWarning, allowOffline, isOnline: boolean;
  pingthread: TPingThread;
  loadUsersThread, loadConfigThread: TRequestThread;

implementation

{$R *.lfm}

{ Twindow }


procedure Twindow.FormCreate(Sender: TObject);
var
   version, build: string;
begin
   version:='1.5.1';
   build:='1F168';
   groupFoldersMounted:=false;
   showedGroupfolderWarning:=false;
   allowOffline:=false;
   gfcount:=0;
   actualNetworkRetry:=1;
   networkRetry:=30;
   isOnline:=false;
   {$IFDEF WIN32}
     versionLabel.Caption:='PhilleConnect Win32 v'+version+' Build '+build+' by Johannes Kreutz';
   {$ENDIF}
   {$IFDEF WIN64}
     versionLabel.Caption:='PhilleConnect Win64 v'+version+' Build '+build+' by Johannes Kreutz';
   {$ENDIF}
   {$IFDEF LINUX}
     versionLabel.Caption:='PhilleConnect Linux v'+version+' Build '+build+' by Johannes Kreutz';
     infoLabel.left:=infoLabel.left-8;
     usernames.height:=usernames.height-8;
     usernames.top:=usernames.top+8;
   {$ENDIF}
   {$IFDEF DARWIN}
     versionLabel.Caption:='PhilleConnect macOS v'+version+' Build '+build+' by Johannes Kreutz';
     infoLabel.left:=infoLabel.left-2;
   {$ENDIF}
   {$IFNDEF WINDOWS}
     d1stat.left:=160;
     d1user.left:=160;
     d1path.left:=160;
     d2stat.left:=160;
     d2user.left:=160;
     d2path.left:=160;
     d3stat.left:=160;
     d3user.left:=160;
     d3path.left:=160;
   {$ENDIF}
   arrowDirection:=true;
   timerCounter:=0;
   parseConfigFile;
end;

procedure Twindow.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (key = 13) then begin
    if (window.activeControl = search) then begin
      if (searchUserdata.count = 1) then begin
        uname.text:=searchUserdata[0];
        passwd.setFocus;
      end;
    end
    else begin
      loginButtonClick(loginButton);
    end;
  end;
end;

procedure Twindow.loginButtonClick(Sender: TObject);
begin
  doLogin;
end;

procedure Twindow.onlineInfosClick(Sender: TObject);
begin
  openURL(helpURL);
end;

procedure Twindow.reloadTimerTimer(Sender: TObject);
begin
  checkNetworkConnection;
end;

procedure Twindow.clearButtonClick(Sender: TObject);
begin
  uname.text:='';
  passwd.text:='';
end;

procedure Twindow.D1logoutClick(Sender: TObject);
begin
  doLogout(0);
end;

procedure Twindow.D1openClick(Sender: TObject);
begin
  shares[0].open;
end;

procedure Twindow.D2logoutClick(Sender: TObject);
begin
  doLogout(1);
end;

procedure Twindow.D2openClick(Sender: TObject);
begin
  shares[1].open;
end;

procedure Twindow.D3logoutClick(Sender: TObject);
begin
  doLogout(2);
end;

procedure Twindow.D3openClick(Sender: TObject);
begin
  shares[2].open;
end;

procedure Twindow.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  c: integer;
begin
  if (drive1Box.enabled = false) and (drive2Box.enabled = false) and (drive3Box.enabled = false) and not(groupFoldersMounted) then begin
    CanClose:=true;
  end
  else begin
    if (messageBox(window.handle, 'Es sind noch (Gruppen)Laufwerke verbunden. Möchtest du sie trennen?', 'PhilleConnect schließen', MB_YESNO) = ID_YES) then begin
      if (drive3Box.enabled = true) then begin
        doLogout(2);
      end;
      if (drive2Box.enabled = true) then begin
        doLogout(1);
      end;
      if (drive1Box.enabled = true) then begin
        doLogout(0);
      end;
      if (groupFoldersMounted) then begin
        c:=0;
        while (c < length(groupfolders)) do begin
          groupfolders[c].disconnect;
          groupfolders[c].free;
          c:=c+1;
        end;
      end;
      CanClose:=true;
    end
    else begin
      CanClose:=false;
    end;
  end;
end;

procedure Twindow.arrowTimerTimer(Sender: TObject);
begin
  if (arrowDirection = false) then begin
    fadingArrow.left:=fadingArrow.left-1;
  end
  else begin
    fadingArrow.left:=fadingArrow.left+1;
  end;
  if (timerCounter < 10) then begin
    arrowTimer.interval:=arrowTimer.interval-6;
  end
  else if (timerCounter >= 20) and (timerCounter < 30) then begin
    arrowTimer.interval:=arrowTimer.interval+6;
  end
  else if (timerCounter = 30) then begin
    arrowDirection:=not(arrowDirection);
    arrowTimer.interval:=arrowTimer.interval-6;
  end
  else if (timerCounter >= 30) and (timerCounter <= 40) then begin
    arrowTimer.interval:=arrowTimer.interval-6;
  end
  else if (timerCounter >= 50) and (timerCounter < 60) then begin
    arrowTimer.interval:=arrowTimer.interval+6;
  end
  else if (timerCounter = 60) then begin
    arrowDirection:=not(arrowDirection);
    timerCounter:=0;
  end;
  timerCounter:=timerCounter+1;
end;

procedure Twindow.changePasswordClick(Sender: TObject);
begin
  CPForm.showModal;
end;

procedure Twindow.loadUsers;
begin
  loadUsersThread:=TRequestThread.create('https://'+serverURL+'/client.php', 'usage=userlist&globalpw='+globalPW+'&machine='+mac+'&ip='+ip);
  loadUsersThread.OnShowStatus:=@loadUsersResponse;
  loadUsersThread.resume;
end;

procedure Twindow.loadUsersResponse(response: string);
var
  c: integer;
  jData: TJSONData;
begin
  if (response = '') then begin
     if (allowOffline) then begin
       halt;
     end
     else begin
       showMessage('Netzwerkfehler. Programm wird beendet.');
       halt;
     end;
   end
   else if (response = '!') then begin
     showMessage('Konfigurationsfehler. Programm wird beendet.');
     halt;
   end
   else begin
     userdata:=TStringList.create;
     fullUserdata:=TStringList.create;
     searchUserdata:=TStringList.create;
     jData:=GetJSON(response);
     c:=0;
     while (c < jData.count) do begin
       userdata.add(jData.FindPath(IntToStr(c)+'[2]').AsString);
       searchUserdata.add(jData.FindPath(IntToStr(c)+'[2]').AsString);
       fullUserdata.add(jData.FindPath(IntToStr(c)+'[0]').AsString+' '+jData.FindPath(IntToStr(c)+'[1]').AsString+' ('+jData.FindPath(IntToStr(c)+'[2]').AsString+')');
       usernames.items.add(jData.FindPath(IntToStr(c)+'[0]').AsString+' '+jData.FindPath(IntToStr(c)+'[1]').AsString+' ('+jData.FindPath(IntToStr(c)+'[2]').AsString+')');
       c:=c+1;
     end;
   end;
end;

procedure Twindow.usernamesSelectionChange(Sender: TObject; User: boolean);
var
   counter: integer;
begin
   counter:=0;
   while (counter < searchUserdata.count) do begin
      if counter = (usernames.ItemIndex) then begin
         uname.Text:=searchUserdata[counter];
         break;
      end;
      counter:=counter+1;
   end;
   passwd.setFocus;
end;

procedure Twindow.searchChange(Sender: TObject);
var
   counter: NativeInt;
   rpl: string;
begin
   usernames.items.clear();
   searchUserdata.clear();
   counter:=0;
   while (counter < userdata.count) do begin
      rpl:=StringReplace(LowerCase(fullUserdata[counter]), LowerCase(search.text), '', [rfReplaceAll]);
      if not(LowerCase(fullUserdata[counter]) = rpl) or (search.Text = '') then begin
         usernames.items.add(fullUserdata[counter]);
         searchUserdata.add(userdata[counter]);
      end;
      counter:=counter+1;
   end;
end;

procedure Twindow.checkNetworkConnection;
var
  noPort: TStringList;
  cache: string;
begin
  if (pos(':', serverURL) > 0) then begin
    noPort:=TStringList.create;
    noPort.delimiter:=':';
    noPort.strictDelimiter:=true;
    noPort.delimitedText:=serverURL;
    cache:=noPort[0];
  end
  else begin
    cache:=serverURL;
  end;
  cleanServerURL:=cache;
  pingthread:=TPingThread.create(cache);
  pingthread.OnShowStatus:=@networkConnectionResult;
  pingthread.resume;
end;

procedure Twindow.networkConnectionResult(result: boolean; return: string);
var
  host: THostResolver;
begin
  if (result) then begin
    if (ValidateIP(cleanServerURL)) then begin
      if (cleanServerURL = return) then begin
        trueNetworkResult;
      end
      else begin
        noNetworkInfo.caption:='PING-Ergebnis: Wrong reply. Versuch: '+IntToStr(actualNetworkRetry);
        falseNetworkResult;
      end;
    end
    else begin
      host:=THostResolver.create(nil);
      host.clearData();
      if (host.NameLookup(cleanServerURL)) then begin
        if (host.AddressAsString = return) then begin
          trueNetworkResult;
        end
        else begin
          noNetworkInfo.caption:='PING-Ergebnis: Wrong reply. Versuch: '+IntToStr(actualNetworkRetry);
          falseNetworkResult;
        end;
      end
      else begin
        noNetworkInfo.caption:='PING-Ergebnis: DNS failed. Versuch: '+IntToStr(actualNetworkRetry);
        falseNetworkResult;
      end;
    end;
  end
  else begin
    noNetworkInfo.caption:='PING-Ergebnis: Host is down. Versuch: '+IntToStr(actualNetworkRetry);
    falseNetworkResult;
  end;
end;

procedure Twindow.trueNetworkResult;
begin
  reloadTimer.enabled:=false;
  lockUI(false);
  if not(isOnline) then begin
    loadConfig;
  end;
  isOnline:=true;
end;

procedure Twindow.falseNetworkResult;
begin
  reloadTimer.enabled:=true;
  lockUI(true);
  if (actualNetworkRetry >= networkRetry) then begin
    reloadTimer.enabled:=false;
    showMessage('Es konnte keine Netzwerkverbindung aufgebaut werden. Programm wird beendet.');
    halt;
  end;
  actualNetworkRetry:=actualNetworkRetry+1;
end;

procedure Twindow.parseConfigFile;
var
  config: TStringList;
  c: integer;
  value: TStringList;
  response, os: string;
  jData: TJSONData;
begin
  config:=TStringList.create;
  {$IFDEF WINDOWS}
    config.loadFromFile('C:\Program Files\PhilleConnect\pcconfig.jkm');
  {$ENDIF}
  {$IFDEF LINUX}
    config.loadFromFile('/etc/pcconfig.jkm');
  {$ENDIF}
  c:=0;
  while (c < config.count) do begin
    if (pos('#', config[c]) = 0) then begin
      value:=TStringList.create;
      value.clear;
      value.strictDelimiter:=true;
      value.delimiter:='=';
      value.delimitedText:=config[c];
      case value[0] of
        'server':
          serverURL:=value[1];
        'global':
          globalPW:=value[1];
        'allowOffline':
          setAllowOffline(value[1]);
        'badNetworkReconnect':
          networkRetry:=StrToInt(value[1]);
      end;
    end;
    c:=c+1;
  end;
  checkNetworkConnection;
end;

procedure Twindow.loadConfig;
var
  os: string;
  MacAddr: TGetMacAdress;
  IPAddr: TGetIPAdress;
begin
  MacAddr:=TGetMacAdress.create;
  mac:=MacAddr.getMac;
  MacAddr.free;
  IPAddr:=TGetIPAdress.create;
  ip:=IPAddr.getIP;
  IPAddr.free;
  {$IFDEF WINDOWS}
    os:='win';
  {$ENDIF}
  {$IFDEF LINUX}
    os:='linux';
  {$ENDIF}
  loadConfigThread:=TRequestThread.create('https://'+serverURL+'/client.php', 'usage=config&globalpw='+globalPW+'&machine='+mac+'&ip='+ip+'&os='+os);
  loadConfigThread.OnShowStatus:=@loadConfigResponse;
  loadConfigThread.resume;
end;

procedure Twindow.loadConfigResponse(response: string);
var
  jData: TJSONData;
  c: integer;
begin
  if (response = '!') then begin
    showMessage('Konfigurationsfehler. Programm wird beendet.');
    halt;
  end
  else if (response = 'nomachine') then begin
    showMessage('Rechner nicht registriert. Programm wird beendet.');
    halt;
  end
  else if (response = 'noconfig') then begin
    showMessage('Rechner nicht fertig eingerichtet. Programm wird beendet.');
    halt;
  end
  else if (response <> '') then begin
    lockInputs(false);
    reloadTimer.enabled:=false;
    jData:=GetJSON(response);
    c:=0;
    while (c < jData.count) do begin
      case jData.FindPath(IntToStr(c)+'[0]').AsString of
        'smbserver':
          SMBServerURL:=Trim(jData.FindPath(IntToStr(c)+'[1]').AsString);
        'driveone':
          drives[0]:=jData.FindPath(IntToStr(c)+'[1]').AsString;
        'drivetwo':
          drives[1]:=jData.FindPath(IntToStr(c)+'[1]').AsString;
        'drivethree':
          drives[2]:=jData.FindPath(IntToStr(c)+'[1]').AsString;
        'pathone':
          drivePaths[0]:=jData.FindPath(IntToStr(c)+'[1]').AsString;
        'pathtwo':
          drivePaths[1]:=jData.FindPath(IntToStr(c)+'[1]').AsString;
        'paththree':
          drivePaths[2]:=jData.FindPath(IntToStr(c)+'[1]').AsString;
        'dologin':
          login:=jData.FindPath(IntToStr(c)+'[1]').AsString;
        'loginpending':
          loginPending:=jData.FindPath(IntToStr(c)+'[1]').AsString;
        'loginfailed':
          loginFailed:=jData.FindPath(IntToStr(c)+'[1]').AsString;
        'wrongcredentials':
          wrongCredentials:=jData.FindPath(IntToStr(c)+'[1]').AsString;
        'networkfailed':
          networkFailed:=jData.FindPath(IntToStr(c)+'[1]').AsString;
        'groupfolders':
          gfdata:=jData.FindPath(IntToStr(c)+'[1]').AsString;
        'infotext':
          loadTextBox(jData.FindPath(IntToStr(c)+'[1]').AsString);
        'helpurl':
          setHelpURL(jData.FindPath(IntToStr(c)+'[1]').AsString);
      end;
      c:=c+1;
    end;
    loadUsers;
    doFileLogin;
  end
  else begin
    lockInputs(false);
    showMessage('Netzwerkfehler. Programm wird beendet.');
    halt;
  end;
end;

procedure Twindow.lockInputs(mode: boolean);
begin
  if (mode) then begin
    actionLabel.caption:='Netzwerkverbindung wird aufgebaut...';
    uname.enabled:=false;
    passwd.enabled:=false;
    clearButton.enabled:=false;
    loginButton.enabled:=false;
    search.enabled:=false;
    usernames.enabled:=false;
  end
  else begin
    actionLabel.caption:='Bitte melde dich mit deinen Zugangsdaten an.';
    uname.enabled:=true;
    passwd.enabled:=true;
    clearButton.enabled:=true;
    loginButton.enabled:=true;
    search.enabled:=true;
    usernames.enabled:=true;
    if (window.visible) then begin
      search.setFocus;
    end;
  end;
end;

procedure Twindow.lockUI(mode: boolean);
begin
  if (mode) then begin
    window.color:=clBlack;
    usersBox.visible:=false;
    loginBox.visible:=false;
    newsBox.visible:=false;
    versionLabel.visible:=false;
    infoLabel.visible:=false;
    infoLabel2.visible:=false;
    pwChangeButton.visible:=false;
    headline.font.color:=clWhite;
    noNetworkInfo.visible:=true;
    noNetworkHead.visible:=true;
    noNetworkSub.visible:=true;
    noNetworkHead.left:=(window.width div 2)-(noNetworkHead.width div 2);
    noNetworkHead.top:=(window.height div 2)-(noNetworkHead.height div 2)-30;
    noNetworkSub.left:=(window.width div 2)-(noNetworkSub.width div 2);
    noNetworkSub.top:=(window.height div 2)-(noNetworkSub.height div 2)+20;
  end
  else begin
    window.color:=clDefault;
    usersBox.visible:=true;
    loginBox.visible:=true;
    newsBox.visible:=true;
    versionLabel.visible:=true;
    infoLabel.visible:=true;
    infoLabel2.visible:=true;
    pwChangeButton.visible:=true;
    headline.font.color:=clDefault;
    noNetworkInfo.visible:=false;
    noNetworkHead.visible:=false;
    noNetworkSub.visible:=false;
    if (window.visible) then begin
      search.setFocus;
    end;
  end;
end;

procedure Twindow.doLogin;
begin
  if (uname.text = '') then begin
    showMessage('Bitte gib einen Nutzernamen ein.');
  end
  else if (passwd.text = '') then begin
    showMessage('Bitte gib ein Passwort ein.');
  end
  else begin
    actionLabel.caption:=loginPending;
    uname.readonly:=true;
    passwd.readonly:=true;
    loginButton.enabled:=false;
    clearButton.enabled:=false;
    if (drive1Box.enabled = false) then begin
      if (groupFoldersMounted = false) then begin
        mountGroupFolders(uname.text, passwd.text);
      end;
      {$IFDEF WINDOWS}
      shares[0]:=TSMBShare.create(uname.text, passwd.text, drives[0], 'driveone.this', uname.text);
      {$ENDIF}
      {$IFDEF LINUX}
      shares[0]:=TSMBShare.create(uname.text, passwd.text, drives[0]+uname.text, SMBServerURL, uname.text);
      {$ENDIF}
      actionLabel.caption:=loginPending;
      shares[0].connect;
      shares[0].onShowStatus:=@handleOne;
      passwd.text:='';
    end
    else if (drive2Box.enabled = false) then begin
      {$IFDEF WINDOWS}
      shares[1]:=TSMBShare.create(uname.text, passwd.text, drives[1], 'drivetwo.this', uname.text);
      {$ENDIF}
      {$IFDEF LINUX}
      shares[1]:=TSMBShare.create(uname.text, passwd.text, drives[1]+uname.text, SMBServerURL, uname.text);
      {$ENDIF}
      actionLabel.caption:=loginPending;
      shares[1].connect;
      shares[1].onShowStatus:=@handleTwo;
      passwd.text:='';
    end
    else if (drive3Box.enabled = false) then begin
      {$IFDEF WINDOWS}
      shares[2]:=TSMBShare.create(uname.text, passwd.text, drives[2], 'drivethree.this', uname.text);
      {$ENDIF}
      {$IFDEF LINUX}
      shares[2]:=TSMBShare.create(uname.text, passwd.text, drives[2]+uname.text, SMBServerURL, uname.text);
      {$ENDIF}
      actionLabel.caption:=loginPending;
      shares[2].connect;
      shares[2].onShowStatus:=@handleThree;
      passwd.text:='';
      loginButton.enabled:=false;
      fadingArrow.visible:=false;
    end
    else begin
      uname.readonly:=false;
      passwd.readonly:=false;
      loginButton.enabled:=true;
      clearButton.enabled:=true;
      passwd.text:='';
      EasterEgg.showModal;
    end;
  end;
end;

procedure Twindow.handleOne(status: string);
begin
  handleConnectResult(0, StrToInt(status));
end;

procedure Twindow.handleTwo(status: string);
begin
  handleConnectResult(1, StrToInt(status));
end;

procedure Twindow.handleThree(status: string);
begin
  handleConnectResult(2, StrToInt(status));
end;

procedure Twindow.controlShareUI(share: integer; state: boolean);
begin
  if (state = true) then begin
    if (share = 0) then begin
      drive1Box.enabled:=true;
      openDrive1.enabled:=true;
      fadingArrow.visible:=false;
      arrowTimer.enabled:=false;
    end
    else if (share = 1) then begin
      drive2Box.enabled:=true;
      openDrive2.enabled:=true;
    end
    else if (share = 2) then begin
      drive3Box.enabled:=true;
      openDrive3.enabled:=true;
    end;
  end
  else begin
    if (share = 0) then begin
      drive1Box.enabled:=false;
      openDrive1.enabled:=false;
    end
    else if (share = 1) then begin
      drive2Box.enabled:=false;
      openDrive2.enabled:=false;
    end
    else if (share = 2) then begin
      drive3Box.enabled:=false;
      openDrive3.enabled:=false;
    end;
  end;
end;

procedure Twindow.handleConnectResult(share, connect: integer);
begin
  //actionLabel.caption:=login;
  if (connect = 0) then begin
    actionLabel.font.color:=clBlack;
    actionLabel.caption:='Erfolgreich angemeldet.';
    controlShareUI(share, true);
    if (share = 0) then begin
      d1stat.caption:='Verbunden';
      d1user.caption:=uname.text;
      {$IFDEF WINDOWS}
        d1path.caption:=drivePaths[share];
      {$ENDIF}
      {$IFDEF LINUX}
        d1path.caption:=drivePaths[share]+uname.text;
      {$ENDIF}
    end
    else if (share = 1) then begin
      d2stat.caption:='Verbunden';
      d2user.caption:=uname.text;
      {$IFDEF WINDOWS}
        d2path.caption:=drivePaths[share];
      {$ENDIF}
      {$IFDEF LINUX}
        d2path.caption:=drivePaths[share]+uname.text;
      {$ENDIF}
    end
    else if (share = 2) then begin
      d3stat.caption:='Verbunden';
      d3user.caption:=uname.text;
      {$IFDEF WINDOWS}
        d3path.caption:=drivePaths[share];
      {$ENDIF}
      {$IFDEF LINUX}
        d3path.caption:=drivePaths[share]+uname.text;
      {$ENDIF}
    end;
    uname.readonly:=false;
    passwd.readonly:=false;
    loginButton.enabled:=true;
    clearButton.enabled:=true;
    uname.text:='';
    passwd.text:='';
  end
  else begin
    shares[share].free;
    uname.readonly:=false;
    passwd.readonly:=false;
    loginButton.enabled:=true;
    clearButton.enabled:=true;
    passwd.text:='';
    if (connect = 1) then begin
      showMessage(wrongCredentials);
    end
    else if (connect = 2) then begin
      showMessage(loginFailed);
    end
    else if (connect = 3) then begin
      showMessage(networkFailed);
    end
    else if (connect = 4) then begin
      showMessage('Es ist ein Systemfehler aufgetreten.');
    end;
    actionLabel.caption:=login;
  end;
end;

procedure Twindow.doLogout(share: integer);
var
  c: integer;
begin
  if (share = 0) then begin
    if (drive2Box.enabled) or (drive3Box.enabled) then begin
      showMessage('Die Gruppen-/Tauschlaufwerke sind mit dem ersten angemeldeten Nutzer verbunden. Um die Laufwerke weiterhin zu nutzen, musst du dich erneut als erster Nutzer (Laufwerk 1) anmelden.');
    end;
    shares[0].disconnect;
    d1stat.caption:='Nicht verbunden';
    d1user.caption:='';
    d1path.caption:='';
    drive1Box.enabled:=false;
    if (groupFoldersMounted) then begin
      c:=0;
      while (c < length(groupfolders)) do begin
        groupfolders[c].disconnect;
        groupfolders[c].free;
        c:=c+1;
      end;
      groupFoldersMounted:=false;
    end;
  end
  else if (share = 1) then begin
    shares[1].disconnect;
    d2stat.caption:='Nicht verbunden';
    d2user.caption:='';
    d2path.caption:='';
    drive2Box.enabled:=false;
  end
  else if (share = 2) then begin
    shares[2].disconnect;
    d3stat.caption:='Nicht verbunden';
    d3user.caption:='';
    d3path.caption:='';
    drive3Box.enabled:=false;
  end;
  if (drive1Box.enabled = false) and (drive2Box.enabled = false) and (drive3Box.enabled = false) then begin
    actionLabel.font.color:=clRed;
    actionLabel.caption:=login;
    fadingArrow.visible:=true;
    arrowTimer.enabled:=true;
  end;
end;

procedure Twindow.doFileLogin;
var
  loginFile: TStringList;
  loginFilePath: string;
begin
  {$IFDEF WINDOWS}
    loginFilePath:=getUserDir+'login.jkm';
  {$ENDIF}
  {$IFDEF LINUX}
    loginFilePath:='/tmp/login.jkm';
  {$ENDIF}
  if (fileExists(loginFilePath)) then begin
    loginFile:=TStringList.create;
    loginFile.loadFromFile(loginFilePath);
    if not(loginFile[0] = '') then begin
      uname.text:=XORDecode(mac, loginFile[0]);
      passwd.text:=XORDecode(mac, loginFile[1]);
      doLogin;
      mountGroupFolders(XORDecode(mac, loginFile[0]), XORDecode(mac, loginFile[1]));
      loginFile[0]:='';
      loginFile[1]:='';
      loginFile.saveToFile(loginFilePath);
    end;
  end;
end;

procedure Twindow.loadTextBox(infotext: string);
var
  content: TStringList;
begin
  content:=TStringList.create;
  content.delimiter:='%';
  content.strictDelimiter:=true;
  content.delimitedText:=infotext;
  news.lines:=content;
end;

procedure Twindow.mountGroupFolders(username, pw: string);
var
  jData: TJSONData;
  c: integer;
  longpath: TStringList;
begin
  if not(gfdata = '') then begin
    jData:=GetJSON(StringReplace(gfdata, '\', '', [rfReplaceAll]));
    c:=0;
    while (c < jData.count) do begin
      setLength(groupfolders, (c+1));
      {$IFDEF WINDOWS}
      groupfolders[c]:=TSMBShare.create(username, pw, jData.FindPath(IntToStr(c)+'[0]').AsString, 'groupfolders.this', StringReplace(jData.FindPath(IntToStr(c)+'[1]').AsString, '/', '\', [rfReplaceAll]));
      {$ENDIF}
      {$IFDEF LINUX}
      longpath:=TStringList.create;
      longpath.delimiter:='/';
      longpath.delimitedText:=jData.FindPath(IntToStr(c)+'[1]').AsString;
      groupfolders[c]:=TSMBShare.create(username, pw, jData.FindPath(IntToStr(c)+'[0]').AsString+longpath[(longpath.count-1)], SMBServerURL, jData.FindPath(IntToStr(c)+'[1]').AsString);
      {$ENDIF}
      groupfolders[c].connect;
      groupfolders[c].onShowStatus:=@handleGroup;
      c:=c+1;
    end;
  end;
end;

procedure Twindow.handleGroup(status: string);
begin
  if (StrToInt(status) <> 0) and (StrToInt(status) <> 2) then begin
    if not(showedGroupfolderWarning) then begin
      showedGroupfolderWarning:=true;
      showMessage('Fehler beim Einbinden der Gruppen-/Tauschlaufwerke.');
    end;
  end
  else begin
    gfcount:=gfcount+1;
    groupFoldersMounted:=true;
  end;
end;

procedure Twindow.setAllowOffline(value: string);
begin
  if (value = '1') then begin
    allowOffline:=true;
  end
  else begin
    allowOffline:=false;
  end;
end;

procedure Twindow.setHelpURL(url: string);
begin
  if (url <> '') then begin
    helpURL:=url;
    sHelpMenu.visible:=true;
  end;
end;

function Twindow.sendRequest(url, params: string): string;
var
   Response: TMemoryStream;
begin
   Response := TMemoryStream.Create;
   try
      if HttpPostURL(url, params, Response) then
         result:=MemStreamToString(Response);
   finally
      Response.Free;
   end;
end;

function Twindow.MemStreamToString(Strm: TMemoryStream): AnsiString;
begin
   if Strm <> nil then begin
      Strm.Position := 0;
      SetString(Result, PChar(Strm.Memory), Strm.Size);
   end;
end;

function Twindow.ValidateIP(IP4: string): Boolean; // Coding by Dave Sonsalla
var
  Octet : String;
  Dots, I : Integer;
begin
  IP4 := IP4+'.'; //add a dot. We use a dot to trigger the Octet check, so need the last one
  Dots := 0;
  Octet := '0';
  for I := 1 to length(IP4) do begin
    if IP4[I] in ['0'..'9','.'] then begin
      if IP4[I] = '.' then begin //found a dot so inc dots and check octet value
        Inc(Dots);
        if (length(Octet) =1) Or (StrToInt(Octet) > 255) then Dots := 5; //Either there's no number or it's higher than 255 so push dots out of range
        Octet := '0'; // Reset to check the next octet
      end // End of IP4[I] is a dot
      else // Else IP4[I] is not a dot so
        Octet := Octet + IP4[I]; // Add the next character to the octet
    end // End of IP4[I] is not a dot
    else // Else IP4[I] Is not in CheckSet so
      Dots := 5; // Push dots out of range
  end;
  result := (Dots = 4) // The only way that Dots will equal 4 is if we passed all the tests
end;

end.

