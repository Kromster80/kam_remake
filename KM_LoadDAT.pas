unit KM_LoadDAT;
interface
uses KM_Unit1, KM_Defaults, KromUtils, SysUtils, KM_TPlayer, Dialogs, Math;

procedure LoadDAT(filename:string);
procedure AcquireDAT();
procedure ExportText(filename:string);

var
    Cmd:array[1..2000]of record
        Command:string;
        Text:string;
        Numer:array[1..6]of integer;
        Comment:string;
        end;
implementation

uses
  KM_Global_Data;

procedure LoadDAT(filename:string);
var FileSize:integer;
    st:string;         //Decoded file data
    i,k:integer;
    Row:integer;
begin
FileSize:=GetFileSize(filename);
assignfile(f,filename); reset(f,1);
blockread(f,c,FileSize);
setlength(st,FileSize);
for k:=1 to FileSize do st[k]:=chr(ord(c[k]) xor 239);
closefile(f);
//memo1.Lines.Add(st);

st := StringReplace(st, #9, ' ', [rfReplaceAll]);
st := StringReplace(st, #10, ' ', [rfReplaceAll]);
st := StringReplace(st, #13, ' ', [rfReplaceAll]);
for i:=1 to 10 do
st := StringReplace(st, '  ', ' ', [rfReplaceAll]);

//Reset all mission data
Mission:=TMission.Create;

for Row:=1 to length(Cmd) do begin
Cmd[Row].Command:='';
Cmd[Row].Text:='';
for i:=1 to 6 do
Cmd[Row].Numer[i]:=-1;
Cmd[Row].Comment:='';
end;

k:=1; s:=''; Row:=1;
//Acquire one command with parameters
repeat
    //Acquire command name starting with '!'
    if st[k]='!' then begin
    repeat
    Cmd[Row].Command:=Cmd[Row].Command+st[k];
    inc(k);
    until((st[k]=#32)or(k>=length(st)));
    inc(k); //Skip space
    end;

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
      if Row=105 then
      s:=';';
      Cmd[Row].Numer[i]:=0;
      repeat //get 1 param
      Cmd[Row].Numer[i]:=Cmd[Row].Numer[i]*10+strtoint(st[k]);
      inc(k);
      until((k>length(st))or(st[k]=#32));
      inc(k); //Skip space
      end;

    //Skip all comments and disabled commands
    while ((k<length(st))and(st[k]<>'!')) do begin
    Cmd[Row].Comment:=Cmd[Row].Comment+st[k];
    inc(k);
    end;

    inc(Row);

//    if Row>length(Cmd) then ShowMessage('Commands count exceeds limit');
until((k>=length(st))or(Row>length(Cmd)));
//Row:=Row-1;
AcquireDAT();
end;

procedure AcquireDAT();
var i:integer; Row:integer;
begin
for Row:=1 to length(Cmd) do begin
if Cmd[Row].Command='!SET_MAP' then begin
Mission.SetMapFile:=Cmd[Row].Text;
end else
if Cmd[Row].Command='!SET_CURR_PLAYER' then begin
Mission.ActivePlayer:=Cmd[Row].Numer[1]+1;
end else
if Cmd[Row].Command='!SET_HUMAN_PLAYER' then begin
Mission.SetHumanPlayer:=Cmd[Row].Numer[1]+1;
end else
if Cmd[Row].Command='!ENABLE_PLAYER' then begin
Mission.Player[Mission.ActivePlayer].Enabled:=(Cmd[Row].Numer[1]+1=Mission.ActivePlayer);
end else
if Cmd[Row].Command='!CLEAR_UP' then begin
Mission.Player[Mission.ActivePlayer].AddClearUp(Cmd[Row].Numer[1]+1,Cmd[Row].Numer[2]+1,Cmd[Row].Numer[3]+1);
end else
if Cmd[Row].Command='!CENTER_SCREEN' then begin
Mission.Player[Mission.ActivePlayer].CenterScreen.PosX:=Cmd[Row].Numer[1]+1;
Mission.Player[Mission.ActivePlayer].CenterScreen.PosY:=Cmd[Row].Numer[2]+1;
end else
if Cmd[Row].Command='!RELEASE_HOUSE' then begin
Mission.Player[Mission.ActivePlayer].ReleaseHouse[Cmd[Row].Numer[1]+1]:=true;
end else
if Cmd[Row].Command='!BLOCK_HOUSE' then begin
Mission.Player[Mission.ActivePlayer].BlockHouse[Cmd[Row].Numer[1]+1]:=true;
end else
if Cmd[Row].Command='!SET_STREET' then begin
Mission.AddRoad(Cmd[Row].Numer[1]+1,Cmd[Row].Numer[2]+1,Mission.ActivePlayer);
end else
if Cmd[Row].Command='!SET_HOUSE' then begin
Mission.Player[Mission.ActivePlayer].AddHouse(Cmd[Row].Numer[1]+1,Cmd[Row].Numer[2]+1,Cmd[Row].Numer[3]+1);
end else

begin
//if Cmd[Row].Command<>'' then Form1.Memo2.Lines.Add(Cmd[Row].Command);
//if Cmd[Row].Text<>'' then Form1.Memo2.Lines[Form1.Memo2.Lines.Count-1]:=Form1.Memo2.Lines[Form1.Memo2.Lines.Count-1]+' '+Cmd[Row].Text;
//for i:=1 to 6 do if Cmd[Row].Numer[i]<>-1 then
//Form1.Memo2.Lines[Form1.Memo2.Lines.Count-1]:=Form1.Memo2.Lines[Form1.Memo2.Lines.Count-1]+' '+inttostr(Cmd[Row].Numer[i]);
end;
//if Cmd[Row].Comment<>'' then Form1.Memo2.Lines.Add(Cmd[Row].Comment);
end;

Mission.ActivePlayer:=1;
end;

procedure ExportText(filename:string);
var i:integer;
begin
assignfile(ft,filename); rewrite(ft);
writeln(ft,'KaM Editor exported scripts for houses and roads');
writeln(ft,'------------------------------------------------');
writeln(ft);
for i:=1 to 6 do begin
writeln(ft,'Player #'+inttostr(i));
writeln(ft,Mission.Player[i].GetAllHouseStrings);
writeln(ft,Mission.GetAllRoadStrings(i));
writeln(ft);
writeln(ft);
end;
closefile(ft);
end;

end.
