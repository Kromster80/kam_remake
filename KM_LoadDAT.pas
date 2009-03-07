unit KM_LoadDAT;
interface
uses
  Windows, Classes, SysUtils, StrUtils, KromUtils, KM_Users;

type
  TMissionPaser = class(TObject)
  private     { Private declarations }
    CurrentPlayerIndex: integer;
    procedure ProcessCommand(CurrentCommandText,FileText:string; i:integer);
    procedure DebugScriptError(ErrorMsg:string);
    procedure UnloadMission;
  public      { Public declarations } 
    constructor Create;
    procedure LoadDATFile(AFileName:string);
end;

var
  fMissionPaser: TMissionPaser;

implementation
uses KM_Defaults;

{ STRING FUNCTIONS }
//Should be in LewinUtils? ;)

function RightStrTo(s : string; SearchPattern : string) : string;
var
  temp : string;
begin
  Result := '';
  temp := s;
  while length(temp) > 1 do
  begin
    if RightStr(temp, length(SearchPattern)) = SearchPattern then
    begin
      Result := RightStr(s, length(s) - length(temp));
      temp := 'aa';
    end;
    temp := LeftStr(temp, length(temp) - 1);
  end;
end;

function LeftStrTo(s : string; SearchPattern : string) : string;
var
  temp : string;
begin
  Result := '';
  temp := s;
  while length(temp) > 1 do
  begin
    if LeftStr(temp, length(SearchPattern)) = SearchPattern then
    begin
      Result := leftstr(s, length(s) - length(temp));
      temp := 'aa';
    end;
    temp := RightStr(temp, length(temp) - 1);
  end;
end;

function GetNextWord(const s : string) : string;
var
  Found : boolean;
  StartedWord : Boolean;
  i, startofword : integer;
begin
  i := 1;
  StartedWord := false;
  Found := false;
  startofword := -1;
  while Found = false do
  begin
    if s[i] = ' ' then
    begin
      if StartedWord then
      begin
        Result := LeftStr(RightStr(s, Length(s) - startofword), i - startofword - 1);
        Found := true;
      end;
    end
    else
    begin
      StartedWord := true;
      //only set this at the start
      if startofword = -1 then
        startofword := i - 1;
    end;
    //stop at the end of the file, if all else fails
    if i >= Length(s) then
    begin
      Result := LeftStr(RightStr(s, Length(s) - startofword), i - startofword - 1);
      Found := true;
    end;
    i := i + 1;
  end;
end;

function GetLengthToEndOfNextWord(const s : string) : integer;
var
  Found : boolean;
  StartedWord : Boolean;
  i, startofword : integer;
begin
  i := 1;
  StartedWord := false;
  Found := false;
  startofword := -1;
  while Found = false do
  begin
    if s[i] = ' ' then
    begin
      if StartedWord then
      begin
        Found := true;
      end;
    end
    else
    begin
      StartedWord := true;
      //only set this at the start
      if startofword = -1 then
        startofword := i - 1;
    end;
    //stop at the end of the file, if all else fails
    if i >= Length(s) then
    begin
      Result := Length(s);
      Found := true;
    end;
    inc(i);
  end;
  Result := i - 1;
end;
{ END STRING FUNCTIONS }


constructor TMissionPaser.Create;
begin
  inherited Create;
end;

procedure TMissionPaser.UnloadMission;
begin
  //fPlayers.Destroy; //@Krom: Is this the correct way of resetting the players?
  //FreeAndNil(fPlayers);
  //@Lewin: I'm not sure which command to use, Free, FreeAndNil or Destroy. Till now I prefered Destroy.
  //fPlayers.Destroy; Probably FreeAndNil is better, but there's an issue:
  //Object should be Freed first and then Niled, where's Delphi does it other way round 
  CurrentPlayerIndex := 0;
end;

procedure TMissionPaser.LoadDATFile(AFileName:string);
var
  FileText, CurrentCommandText: string;
  i, k, FileSize: integer;
  f: file;
  c:array[1..131072] of char;  
  fl: textfile;
begin
  if not CheckFileExists(AFileName) then exit;
  UnloadMission; //Call function which will reset fPlayers and other stuff

  //Load and decode .DAT file into FileText
  FileSize:=GetFileSize(AFileName);
  assignfile(f,AFileName); reset(f,1);
  blockread(f,c,FileSize);
  setlength(FileText,FileSize);
  closefile(f);
  i:=1; k:=1;
  repeat
    FileText[k]:=chr(ord(c[i]) xor 239);
    if (FileText[k]=#9)or(FileText[k]=#10)or(FileText[k]=#13) then FileText[k]:=' ';
    inc(i);
    if (k>1)and(((FileText[k-1]=#32)and(FileText[k]=#32))or((FileText[k-1]='!')and(FileText[k]='!'))) then else
    inc(k);
  until(i>FileSize);
  setlength(FileText,k); //Because some extra characters are removed

  //Temporary debugging
  {assignfile(fl,ExeDir+'Mission.txt');
  rewrite(fl);
  write(fl,FileText);
  closefile(fl);}

  //FileText should now be formatted nicely with 1 space between each parameter/command
  i := 1;
  //go forward until we reach an !
  while i <= Length(FileText) do
  begin
    if FileText[i] = '!' then
    begin
      //i is now at the start of a command. Lets get the whole command text into another string
      CurrentCommandText := LeftStrTo(LeftStr(RightStr(FileText, Length(FileText) - i + 1), 50), ' ');
      //procces this command
      ProcessCommand(CurrentCommandText, FileText, i);
      //move i to the end of this command so that we can continue
      i := i + Length(CurrentCommandText);
    end
    else
      inc(i);
  end;
end;

procedure TMissionPaser.ProcessCommand(CurrentCommandText,FileText:string; i:integer);
var
  ParamList : array[1..8] of string;
  ParamStart : string;
  int: integer;
begin
  //Extract parameters
  ParamStart := RightStr(FileText, Length(FileText) - i - Length(CurrentCommandText));
  for int:=1 to 8 do
    if ParamStart <> '' then
    begin
      ParamList[int] := GetNextWord(ParamStart);
      if LeftStr(ParamList[int],1) = '!' then //Stop at the next command
      begin
        ParamStart := '';
        ParamList[int] := '';
      end
      else
        ParamStart := RightStr(ParamStart, Length(ParamStart) - GetLengthToEndOfNextWord(ParamStart));
    end
    else
      ParamList[int] := '';

  //@Krom: Items bellow are just examples, please feel free to redo them, as I'm not sure if I'm doing it the best way. (am I doing too much range checking/debugging?)
  //Look in my mission editor help file if you want info on the commands. (under Appendix -> List of Commands)
  {case CurrentCommandText of
    '!SET_MAX_PLAYER' :
    begin
      fPlayers:=TKMAllPlayers.Create(EnsureRange(StrToIntDef(ParamList[1],0),1,MAX_PLAYERS)); //Create players
    end;
    '!SET_CURR_PLAYER' :
    begin
      if fPlayers <> nil then
        CurrentPlayerIndex := EnsureRange(StrToIntDef(ParamList[1],0),0,fPlayers.PlayerCount-1)
      else
        DebugScriptError('!SET_CURR_PLAYER before !SET_MAX_PLAYERS');
    end;
    '!SET_HOUSE' :
    begin
      if fPlayers <> nil then
        fPlayers.Player[CurrentPlayerIndex].AddHouse(THouseType(EnsureRange(StrToIntDef(ParamList[1],0),0,High(THouseType))+1), KMPoint(StrToIntDef(ParamList[2],0),StrToIntDef(ParamList[2],0)));
    end;
  end;}
end;

procedure TMissionPaser.DebugScriptError(ErrorMsg:string);
begin
  //Just an idea, a nice way of debugging script errors. Shows the error to the user so they know exactly what they did wrong.
end;


{ Just as idea on how it could be done (copy from KaM Editor), of course there are better ways..
k:=1; s:=''; Row:=1;
//Acquire one command with parameters
repeat
  //append space
  if length(Cmd)-1<Row then
    setlength(Cmd,length(Cmd)+100);

  //clearup
  Cmd[Row].Command:='';
  Cmd[Row].Text:='';
  for i:=1 to 6 do
  Cmd[Row].Numer[i]:=-1;
  Cmd[Row].Comment:='';

  //Acquire command name starting with '!'
  if st[k]='!' then begin

    repeat
      Cmd[Row].Command:=Cmd[Row].Command+st[k];
      inc(k);
    until((st[k]=#32)or(k>=length(st)));
    inc(k); //Skip space


    //Acquire single text parameter
    if not(ord(st[k]) in [48..57])and(st[k]<>'!') then begin//if not a number
    repeat
    Cmd[Row].Text:=Cmd[Row].Text+st[k];
    inc(k);
    until((st[k]=#32)or(k>=length(st)));
    inc(k); //Skip space
    end;

    //Acquire upto 6 numeric parameters
    for i:=1 to 6 do
    if (k<=length(st))and(ord(st[k]) in [48..57]) then
      begin
        Cmd[Row].Numer[i]:=0;
        repeat
          Cmd[Row].Numer[i]:=Cmd[Row].Numer[i]*10+strtoint(st[k]);
          inc(k);
        until((k>length(st))or(not (ord(st[k]) in [48..57])));
        if (k>length(st))or(st[k]=#32) then inc(k); //Skip space
      end;

  end;

  //Skip all comments and disabled commands
  while ((k<length(st))and(st[k]<>'!')) do begin
    Cmd[Row].Comment:=Cmd[Row].Comment+st[k];
    inc(k);
  end;

  inc(Row);

until(k>=length(st));
CMDCount:=Row-1;
}
end.


