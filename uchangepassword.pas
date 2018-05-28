unit UChangePassword;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  HTTPSend, ssl_openssl, fpjson, jsonparser;

type

  { TCPForm }

  TCPForm = class(TForm)
    stepTwoLabel: TLabel;
    stepOneLabel: TLabel;
    search: TEdit;
    infoLabel1: TLabel;
    infoLabel2: TLabel;
    cbLabel1: TLabel;
    cbLabel2: TLabel;
    cbLabel3: TLabel;
    cbLabel4: TLabel;
    usernames: TListBox;
    smallLetters: TCheckBox;
    capitalLetters: TCheckBox;
    eightChars: TCheckBox;
    numbers: TCheckBox;
    confirmButton: TButton;
    cancelButton: TButton;
    sameLabel: TLabel;
    username: TEdit;
    oldpw: TEdit;
    newpw: TEdit;
    newpw2: TEdit;
    usernameLabel: TLabel;
    oldPwLabel: TLabel;
    newPwLabel: TLabel;
    newPwTwoLabel: TLabel;
    procedure cancelButtonClick(Sender: TObject);
    procedure capitalLettersChange(Sender: TObject);
    procedure confirmButtonClick(Sender: TObject);
    procedure eightCharsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure newpw2Change(Sender: TObject);
    procedure newpwChange(Sender: TObject);
    procedure numbersChange(Sender: TObject);
    procedure searchChange(Sender: TObject);
    procedure smallLettersChange(Sender: TObject);
    procedure usernamesSelectionChange(Sender: TObject; User: boolean);
  private
    pwsame, eight, small, capital, number, pwfirst: boolean;
    procedure controlConfirmButton;
    procedure loadUsers;
    procedure reset;
    function sendRequest(url, params: string): string;
    function MemStreamToString(Strm: TMemoryStream): AnsiString;
  public
    { public declarations }
  end;

var
  CPForm: TCPForm;
  CPuserdata, CPfullUserdata, CPsearchUserdata: TStrings;

implementation

uses
  PCS;

{$R *.lfm}

{ TCPForm }

procedure TCPForm.newpwChange(Sender: TObject);
var
  i: integer;
  cache: string;
begin
  //Passwort prüfen
  //Groß- und Kleinbuchstaben?
  capital:=newpw.text <> lowercase(newpw.text);
  small:=newpw.text <> uppercase(newpw.text);
  //8 Zeichen?
  if (length(newpw.text) >= 8) then begin
    eight:=true;
  end
  else begin
    eight:=false;
  end;
  //Nummern?
  i:=0;
  number:=false;
  cache:=newpw.text;
  for i:=1 to length(cache) do begin
    if (cache[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then begin
      number:=true;
    end;
  end;
  //UI
  if (capital) then begin
    capitalLetters.checked:=true;
    cbLabel2.font.color:=clDefault;
  end
  else begin
    capitalLetters.checked:=false;
    cbLabel2.font.color:=clRed;
  end;
  if (small) then begin
    smallLetters.checked:=true;
    cbLabel1.font.color:=clDefault;
  end
  else begin
    smallLetters.checked:=false;
    cbLabel1.font.color:=clRed;
  end;
  if (eight) then begin
    eightChars.checked:=true;
    cbLabel3.font.color:=clDefault;
  end
  else begin
    eightChars.checked:=false;
    cbLabel3.font.color:=clRed;
  end;
  if (number) then begin
    numbers.checked:=true;
    cbLabel4.font.color:=clDefault;
  end
  else begin
    numbers.checked:=false;
    cbLabel4.font.color:=clRed;
  end;
  if (capital) and (small) and (eight) and (number) then begin
    pwfirst:=true;
  end
  else begin
    pwfirst:=false;
  end;
  newpw2change(nil);
end;

procedure TCPForm.reset;
begin
  search.text:='';
  username.text:='';
  oldpw.text:='';
  newpw.text:='';
  newpw2.text:='';
  controlConfirmButton;
  newpw2change(nil);
  sameLabel.visible:=false;
end;

procedure TCPForm.newpw2Change(Sender: TObject);
begin
  //Passwörter gleich?
  sameLabel.visible:=true;
  if (newpw.text = newpw2.text) and (newpw.text <> '') then begin
    sameLabel.font.color:=clGreen;
    sameLabel.caption:='Passwörter stimmen überein.';
    pwsame:=true;
  end
  else if (newpw.text = '') then begin
    sameLabel.font.color:=clRed;
    sameLabel.caption:='Achtung: Passwort zu kurz!';
    pwsame:=false;
  end
  else begin
    sameLabel.font.color:=clRed;
    sameLabel.caption:='Achtung: Passwörter stimmen nicht überein!';
    pwsame:=false;
  end;
  controlConfirmButton;
end;

procedure TCPForm.FormCreate(Sender: TObject);
begin
  {$IFDEF LINUX}
  cbLabel1.left:=cbLabel1.left+8;
  cbLabel2.left:=cbLabel2.left+8;
  eightChars.left:=eightChars.left+16;
  numbers.left:=numbers.left+16;
  cbLabel3.left:=cbLabel3.left+24;
  cbLabel4.left:=cbLabel4.left+24;
  {$ENDIF}
  pwsame:=false;
  eight:=false;
  small:=false;
  capital:=false;
  number:=false;
  pwfirst:=false;
  controlConfirmButton;
end;

procedure TCPForm.FormHide(Sender: TObject);
begin
  reset;
end;

procedure TCPForm.cancelButtonClick(Sender: TObject);
begin
  close;
end;

procedure TCPForm.confirmButtonClick(Sender: TObject);
var
  response: string;
begin
  response:=sendRequest('https://'+serverURL+'/client.php', 'usage=pwchange&globalpw='+globalPW+'&machine='+mac+'&ip='+ip+'&uname='+username.text+'&oldpw='+oldpw.text+'&newpw='+newpw.text+'&newpw2='+newpw2.text);
  if (response = 'success') then begin
    showMessage('Passwort erfolgreich geändert.');
    reset;
    close;
  end
  else if (response = 'nouser') or (response = 'wrongold') then begin
    showMessage('Benutzername und / oder Passwort falsch. Bitte erneut versuchen.');
    oldpw.text:='';
    newpw.text:='';
    newpw2.text:='';
  end
  else if (response = 'notsame') then begin
    showMessage('Die eingegebenen Passwörter stimmen nicht überein. Bitte erneut versuchen.');
    oldpw.text:='';
    newpw.text:='';
    newpw2.text:='';
  end
  else begin
    showMessage('Es ist ein Fehler aufgetreten.');
  end;
end;

procedure TCPForm.controlConfirmButton;
begin
  if (pwfirst) and (pwsame) then begin
    confirmButton.enabled:=true;
  end
  else begin
    confirmButton.enabled:=false;
  end;
end;

procedure TCPForm.loadUsers;
var
   response: string;
   c: integer;
   jData: TJSONData;
begin
   response:=sendRequest('https://'+serverURL+'/client.php', 'usage=userlist&globalpw='+globalPW+'&machine='+mac+'&ip='+ip);
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
     CPuserdata:=TStringList.create;
     CPfullUserdata:=TStringList.create;
     CPsearchUserdata:=TStringList.create;
     usernames.items.clear();
     jData:=GetJSON(response);
     c:=0;
     while (c < jData.count) do begin
       CPuserdata.add(jData.FindPath(IntToStr(c)+'[2]').AsString);
       CPsearchUserdata.add(jData.FindPath(IntToStr(c)+'[2]').AsString);
       CPfullUserdata.add(jData.FindPath(IntToStr(c)+'[0]').AsString+' '+jData.FindPath(IntToStr(c)+'[1]').AsString+' ('+jData.FindPath(IntToStr(c)+'[2]').AsString+')');
       usernames.items.add(jData.FindPath(IntToStr(c)+'[0]').AsString+' '+jData.FindPath(IntToStr(c)+'[1]').AsString+' ('+jData.FindPath(IntToStr(c)+'[2]').AsString+')');
       c:=c+1;
     end;
   end;
end;

procedure TCPForm.usernamesSelectionChange(Sender: TObject; User: boolean);
var
   counter: integer;
begin
   counter:=0;
   while (counter < CPsearchUserdata.count) do begin
      if counter = (usernames.ItemIndex) then begin
         username.text:=CPsearchUserdata[counter];
         break;
      end;
      counter:=counter+1;
   end;
   oldpw.setFocus;
end;

procedure TCPForm.searchChange(Sender: TObject);
var
   counter: NativeInt;
   rpl: string;
begin
   usernames.items.clear();
   CPsearchUserdata.clear();
   counter:=0;
   while (counter < CPuserdata.count) do begin
      rpl:=StringReplace(LowerCase(CPfullUserdata[counter]), LowerCase(search.text), '', [rfReplaceAll]);
      if not(LowerCase(CPfullUserdata[counter]) = rpl) or (search.Text = '') then begin
         usernames.items.add(CPfullUserdata[counter]);
         CPsearchUserdata.add(CPuserdata[counter]);
      end;
      counter:=counter+1;
   end;
end;

procedure TCPForm.smallLettersChange(Sender: TObject);
begin
  if (small) then begin
    smallLetters.checked:=true;
  end
  else begin
    smallLetters.checked:=false;
  end;
end;

procedure TCPForm.capitalLettersChange(Sender: TObject);
begin
  if (capital) then begin
    capitalLetters.checked:=true;
  end
  else begin
    capitalLetters.checked:=false;
  end;
end;

procedure TCPForm.eightCharsChange(Sender: TObject);
begin
  if (eight) then begin
    eightChars.checked:=true;
  end
  else begin
    eightChars.checked:=false;
  end;
end;

procedure TCPForm.numbersChange(Sender: TObject);
begin
  if (number) then begin
    numbers.checked:=true;
  end
  else begin
    numbers.checked:=false;
  end;
end;

procedure TCPForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (key = 13) then begin
    if (CPForm.activeControl = search) then begin
      if (CPsearchUserdata.count = 1) then begin
        username.text:=CPsearchUserdata[0];
        oldpw.setFocus;
      end;
    end;
  end;
end;

procedure TCPForm.FormShow(Sender: TObject);
begin
  search.setFocus;
  loadUsers;
end;

function TCPForm.sendRequest(url, params: string): string;
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

function TCPForm.MemStreamToString(Strm: TMemoryStream): AnsiString;
begin
   if Strm <> nil then begin
      Strm.Position := 0;
      SetString(Result, PChar(Strm.Memory), Strm.Size);
   end;
end;

end.

