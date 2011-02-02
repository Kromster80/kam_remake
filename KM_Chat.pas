unit KM_Chat;
{$I KaM_Remake.inc}
interface
uses Classes, KM_Defaults, KromUtils, SysUtils, StrUtils, Math, Windows;

type
  TKMTextMessage = record
                    TimeStamp:string; //HH/MM
                    Nik:string; //PlayerID
                    Msg:string;
                   end;

{Store chat messgaes from lobby and game}
type
  TKMChat = class
    private
      fCount:integer;
      fLastGiven:integer; //Last message UI querried from us
      fMessages:array of TKMTextMessage;
      procedure CheckLength;
    public
      constructor Create;
      destructor Destroy; override;
      procedure AddMessage(aPlayerID:integer; aTime:string; aText:string);
      function GetAllMessages:string;
      function GetNewMessages:string;
    end;


implementation


{ TKMChat }
constructor TKMChat.Create;
begin
  Inherited;
  fCount := 0;
  fLastGiven := 1;
end;


destructor TKMChat.Destroy;
begin
  //
  Inherited;
end;


procedure TKMChat.CheckLength;
begin
  if fCount >= length(fMessages) then
    setlength(fMessages, length(fMessages)+32);
end;


procedure TKMChat.AddMessage(aPlayerID:integer; aTime:string; aText:string);
begin
  CheckLength;
  fMessages[fCount].TimeStamp := aTime;
  fMessages[fCount].Nik := inttostr(aPlayerID);
  fMessages[fCount].Msg := aText;
  inc(fCount);
end;


function TKMChat.GetAllMessages:string;
var i:integer;
begin
  Result := '';
  for i:=0 to fCount-1 do
    Result := Result + fMessages[i].TimeStamp + ' ' + fMessages[i].Nik +': '+ fMessages[i].Msg + '|';

  fLastGiven := fCount-1+1;
end;


function TKMChat.GetNewMessages:string;
var i:integer;
begin
  Result := '';
  for i:=fLastGiven to fCount-1 do
    Result := Result + fMessages[i].TimeStamp + ' ' + fMessages[i].Nik +': '+ fMessages[i].Msg + '|';

  fLastGiven := fCount-1+1;
end;


end.
