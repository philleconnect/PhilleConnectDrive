unit UResetStudentPassword;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, HTTPSend, ssl_openssl, fpjson, jsonparser;

type

  { TRSPForm }

  TRSPForm = class(TForm)
    cancelButton: TButton;
    capitalLetters: TCheckBox;
    cbLabel1: TLabel;
    cbLabel2: TLabel;
    cbLabel3: TLabel;
    cbLabel4: TLabel;
    confirmButton: TButton;
    separatorOne: TShape;
    separatorTwo: TShape;
    stepThreeLabel: TLabel;
    stepTwoLabel: TLabel;
    stepOneLabel: TLabel;
    search: TEdit;
    lehrerLabel3: TLabel;
    usernames: TListBox;
    teacherPw: TEdit;
    teacherUname: TEdit;
    eightChars: TCheckBox;
    lehrerLabel2: TLabel;
    lehrerLabel1: TLabel;
    infoLabel1: TLabel;
    infoLabel2: TLabel;
    newpw: TEdit;
    newpw2: TEdit;
    newPwLabel: TLabel;
    newPwTwoLabel: TLabel;
    numbers: TCheckBox;
    sameLabel: TLabel;
    smallLetters: TCheckBox;
    username: TEdit;
    usernameLabel: TLabel;
    procedure cancelButtonClick(Sender: TObject);
    procedure capitalLettersChange(Sender: TObject);
    procedure confirmButtonClick(Sender: TObject);
    procedure eightCharsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure infoLabel3Click(Sender: TObject);
    procedure newpw2Change(Sender: TObject);
    procedure newpwChange(Sender: TObject);
    procedure numbersChange(Sender: TObject);
    procedure searchChange(Sender: TObject);
    procedure smallLettersChange(Sender: TObject);
    procedure teacherPwChange(Sender: TObject);
    procedure usernamesSelectionChange(Sender: TObject; User: boolean);
  private
    pwsame, eight, small, capital, number, pwfirst, pwteacher: boolean;
    procedure controlConfirmButton;
    procedure loadUsers;
    procedure reset;
    function sendRequest(url, params: string): string;
    function MemStreamToString(Strm: TMemoryStream): AnsiString;
  public
    { public declarations }
  end;

var
  RSPForm: TRSPForm;
  RSPuserdata, RSPfullUserdata, RSPsearchUserdata: TStrings;

implementation

uses
  PCS;

{$R *.lfm}

{ TRSPForm }

procedure TRSPForm.newpwChange(Sender: TObject);
var
  i: integer;
  cache: string;
begin
  capital:=newpw.text <> lowercase(newpw.text);
  small:=newpw.text <> uppercase(newpw.text);
  if (length(newpw.text) >= 8) then begin
    eight:=true;
  end
  else begin
    eight:=false;
  end;
  i:=0;
  number:=false;
  cache:=newpw.text;
  for i:=1 to length(cache) do begin
    if (cache[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then begin
      number:=true;
    end;
  end;
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
  controlConfirmButton;
end;

procedure TRSPForm.reset;
begin
  search.text:='';
  teacherUname.text:='';
  teacherPw.text:='';
  newpw.text:='';
  newpw2.text:='';
  username.text:='';
  newpw2change(nil);
  controlConfirmButton;
  sameLabel.visible:=false;
  search.setFocus;
end;

procedure TRSPForm.teacherPwChange(Sender: TObject);
begin
  if (length(teacherPw.text) > 0) then begin
    pwteacher:=true;
  end
  else begin
    pwteacher:=false;
  end;
  controlConfirmButton;
end;

procedure TRSPForm.newpw2Change(Sender: TObject);
begin
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

procedure TRSPForm.FormCreate(Sender: TObject);
begin
  pwsame:=false;
  eight:=false;
  small:=false;
  capital:=false;
  number:=false;
  pwfirst:=false;
  pwteacher:=false;
  controlConfirmButton;
end;

procedure TRSPForm.FormHide(Sender: TObject);
begin
  reset;
end;

procedure TRSPForm.cancelButtonClick(Sender: TObject);
begin
  close;
end;

procedure TRSPForm.confirmButtonClick(Sender: TObject);
var
  response: string;
begin
  response:=sendRequest('https://'+serverURL+'/client.php', 'usage=pwreset&globalpw='+globalPW+'&machine='+mac+'&ip='+ip+'&uname='+username.text+'&newpw='+newpw.text+'&newpw2='+newpw2.text+'&teacheruser='+teacherUname.text+'&teacherpw='+teacherPw.text);
  if (response = 'success') then begin
    showMessage('Passwort erfolgreich geändert.');
    reset;
    close;
  end
  else if (response = 'noteacher') or (response = 'wrongteacher') then begin
    showMessage('Lehrer-Benutzername und / oder Passwort falsch. Bitte erneut versuchen.');
    teacherPw.text:='';
  end
  else if (response = 'nouser') then begin
    showMessage('Benutzername falsch. Bitte erneut versuchen.');
    newpw.text:='';
    newpw2.text:='';
  end
  else if (response = 'notsame') then begin
    showMessage('Die eingegebenen Passwörter stimmen nicht überein. Bitte erneut versuchen.');
    newpw.text:='';
    newpw2.text:='';
  end
  else if (response = 'notallowed') then begin
    showMessage('Es ist nicht möglich, ein Lehrerpasswort zurückzusetzen.');
    newpw.text:='';
    newpw2.text:='';
  end
  else begin
    showMessage('Es ist ein Fehler aufgetreten.');
  end;
end;

procedure TRSPForm.smallLettersChange(Sender: TObject);
begin
  if (small) then begin
    smallLetters.checked:=true;
  end
  else begin
    smallLetters.checked:=false;
  end;
end;

procedure TRSPForm.capitalLettersChange(Sender: TObject);
begin
  if (capital) then begin
    capitalLetters.checked:=true;
  end
  else begin
    capitalLetters.checked:=false;
  end;
end;

procedure TRSPForm.eightCharsChange(Sender: TObject);
begin
  if (eight) then begin
    eightChars.checked:=true;
  end
  else begin
    eightChars.checked:=false;
  end;
end;

procedure TRSPForm.numbersChange(Sender: TObject);
begin
  if (number) then begin
    numbers.checked:=true;
  end
  else begin
    numbers.checked:=false;
  end;
end;

procedure TRSPForm.controlConfirmButton;
begin
  if (pwfirst) and (pwsame) and (pwteacher) then begin
    confirmButton.enabled:=true;
  end
  else begin
    confirmButton.enabled:=false;
  end;
end;

procedure TRSPForm.loadUsers;
var
   response: string;
   c: integer;
   jData: TJSONData;
begin
   response:=sendRequest('https://'+serverURL+'/client.php', 'usage=userlist&globalpw='+globalPW+'&machine='+mac+'&ip='+ip+'&sort=students');
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
     RSPuserdata:=TStringList.create;
     RSPfullUserdata:=TStringList.create;
     RSPsearchUserdata:=TStringList.create;
     usernames.items.clear();
     jData:=GetJSON(response);
     c:=0;
     while (c < jData.count) do begin
       RSPuserdata.add(jData.FindPath(IntToStr(c)+'[2]').AsString);
       RSPsearchUserdata.add(jData.FindPath(IntToStr(c)+'[2]').AsString);
       RSPfullUserdata.add(jData.FindPath(IntToStr(c)+'[0]').AsString+' '+jData.FindPath(IntToStr(c)+'[1]').AsString+' ('+jData.FindPath(IntToStr(c)+'[2]').AsString+')');
       usernames.items.add(jData.FindPath(IntToStr(c)+'[0]').AsString+' '+jData.FindPath(IntToStr(c)+'[1]').AsString+' ('+jData.FindPath(IntToStr(c)+'[2]').AsString+')');
       c:=c+1;
     end;
   end;
end;

procedure TRSPForm.usernamesSelectionChange(Sender: TObject; User: boolean);
var
   counter: integer;
begin
   counter:=0;
   while (counter < RSPsearchUserdata.count) do begin
      if counter = (usernames.ItemIndex) then begin
         username.text:=RSPsearchUserdata[counter];
         break;
      end;
      counter:=counter+1;
   end;
   newpw.setFocus;
end;

procedure TRSPForm.searchChange(Sender: TObject);
var
   counter: NativeInt;
   rpl: string;
begin
   usernames.items.clear();
   RSPsearchUserdata.clear();
   counter:=0;
   while (counter < RSPuserdata.count) do begin
      rpl:=StringReplace(LowerCase(RSPfullUserdata[counter]), LowerCase(search.text), '', [rfReplaceAll]);
      if not(LowerCase(RSPfullUserdata[counter]) = rpl) or (search.Text = '') then begin
         usernames.items.add(RSPfullUserdata[counter]);
         RSPsearchUserdata.add(RSPuserdata[counter]);
      end;
      counter:=counter+1;
   end;
end;

procedure TRSPForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (key = 13) then begin
    if (RSPForm.activeControl = search) then begin
      if (RSPsearchUserdata.count = 1) then begin
        username.text:=RSPsearchUserdata[0];
        newpw.setFocus;
      end;
    end;
  end;
end;

procedure TRSPForm.FormShow(Sender: TObject);
begin
  search.setFocus;
  loadUsers;
end;

procedure TRSPForm.infoLabel3Click(Sender: TObject);
begin

end;

function TRSPForm.sendRequest(url, params: string): string;
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

function TRSPForm.MemStreamToString(Strm: TMemoryStream): AnsiString;
begin
   if Strm <> nil then begin
      Strm.Position := 0;
      SetString(Result, PChar(Strm.Memory), Strm.Size);
   end;
end;

end.

