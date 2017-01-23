unit KM_LoadDAT;
{$I ..\KaM_Remake.inc}

interface

uses KM_Unit1, KM_Defaults, KromUtils, SysUtils, KM_TPlayer, Dialogs;

procedure LoadDAT(filename: string);
procedure AcquireDAT;
procedure ExportText(filename: string);

var
  Row: integer;
  CMDCount: integer;
  Cmd: array of record Command: string;
  Text: string;
  Numer: array [1 .. 6] of integer;
  Comment: string;
end;

implementation

procedure LoadDAT(filename: string);
var
  FileSize: integer;
  st: string; //Decoded file data
  i, k: integer;
begin
  FileSize := GetFileSize(filename);
  assignfile(f, filename);
  reset(f, 1);
  blockread(f, c, FileSize);
  setlength(st, FileSize);
  if c[1] <> #206 then
  begin
    ShowMessage('This does not looks like encrypted DAT file, which it should be.');
    closefile(f);
    exit;
  end;
  closefile(f);

  i := 1;
  k := 1;
  repeat
    st[k] := chr(ord(c[i]) xor 239);
    if (st[k] = #9) or (st[k] = #10) or (st[k] = #13) then
      st[k] := ' ';
    inc(i);
    if (k > 1) and (((st[k - 1] = #32) and (st[k] = #32)) or ((st[k - 1] = '!') and (st[k] = '!'))) then
    else
      inc(k);
  until (i >= FileSize);

  //Reset all mission data
  Mission := TMission.Create;
  setlength(Cmd, 0);

  k := 1;
  s := '';
  Row := 1;
  //Acquire one command with parameters
  repeat
    //append space
    if length(Cmd) - 1 < Row then
      setlength(Cmd, length(Cmd) + 100);

    //clearup
    Cmd[Row].Command := '';
    Cmd[Row].Text := '';
    for i := 1 to 6 do
      Cmd[Row].Numer[i] := -1;
    Cmd[Row].Comment := '';

    //Acquire command name starting with '!'
    if st[k] = '!' then
    begin

      repeat
        Cmd[Row].Command := Cmd[Row].Command + st[k];
        inc(k);
      until ((st[k] = #32) or (k >= length(st)));
      inc(k); //Skip space

      //Acquire single text parameter
      if not(ord(st[k]) in [48 .. 57]) and (st[k] <> '!') then
      begin //if not a number
        repeat
          Cmd[Row].Text := Cmd[Row].Text + st[k];
          inc(k);
        until ((st[k] = #32) or (k >= length(st)));
        inc(k); //Skip space
      end;

      //Acquire upto 6 numeric parameters
      for i := 1 to 6 do
        if (k <= length(st)) and (ord(st[k]) in [48 .. 57]) then
        begin
          Cmd[Row].Numer[i] := 0;
          repeat
            Cmd[Row].Numer[i] := Cmd[Row].Numer[i] * 10 + strtoint(st[k]);
            inc(k);
          until ((k > length(st)) or (not(ord(st[k]) in [48 .. 57])));
          if (k > length(st)) or (st[k] = #32) then
            inc(k); //Skip space
        end;

    end;

    //Skip all comments and disabled commands
    while ((k < length(st)) and (st[k] <> '!')) do
    begin
      Cmd[Row].Comment := Cmd[Row].Comment + st[k];
      inc(k);
    end;

    inc(Row);

  until (k >= length(st));
  CMDCount := Row - 1;
  AcquireDAT();
end;

procedure AcquireDAT();
var
  i, Row: integer;
begin
  Form1.Memo2.Clear;
  for Row := 1 to CMDCount do
  begin
    if Cmd[Row].Command = '!SET_MAP' then
    begin
      Mission.SetMapFile := Cmd[Row].Text;
    end
    else if Cmd[Row].Command = '!SET_CURR_PLAYER' then
    begin
      Mission.ActivePlayer := Cmd[Row].Numer[1] + 1;
    end
    else if Cmd[Row].Command = '!SET_HUMAN_PLAYER' then
    begin
      Mission.SetHumanPlayer := Cmd[Row].Numer[1] + 1;
    end
    else if Cmd[Row].Command = '!SET_STREET' then
    begin
      Mission.AddRoad(Cmd[Row].Numer[1] + 1, Cmd[Row].Numer[2] + 1, Mission.ActivePlayer, gpR);
    end
    else if Cmd[Row].Command = '!SET_FIELD' then
    begin
      Mission.AddRoad(Cmd[Row].Numer[1] + 1, Cmd[Row].Numer[2] + 1, Mission.ActivePlayer, gpF);
    end
    else if Cmd[Row].Command = '!SET_WINEFIELD' then
    begin
      Mission.AddRoad(Cmd[Row].Numer[1] + 1, Cmd[Row].Numer[2] + 1, Mission.ActivePlayer, gpW);
    end
    else if Cmd[Row].Command = '!SET_HOUSE' then
    begin
      Mission.Player[Mission.ActivePlayer].AddHouse(Cmd[Row].Numer[1] + 1, Cmd[Row].Numer[2] + 1,
        Cmd[Row].Numer[3] + 1);
    end
    else

    begin
      if Cmd[Row].Command <> '' then
        Form1.Memo2.Lines.Add(Cmd[Row].Command);
      if Cmd[Row].Text <> '' then
        Form1.Memo2.Lines[Form1.Memo2.Lines.Count - 1] := Form1.Memo2.Lines[Form1.Memo2.Lines.Count - 1] + ' ' +
          Cmd[Row].Text;
      for i := 1 to 6 do
        if Cmd[Row].Numer[i] <> -1 then
          Form1.Memo2.Lines[Form1.Memo2.Lines.Count - 1] := Form1.Memo2.Lines[Form1.Memo2.Lines.Count - 1] + ' ' +
            IntToStr(Cmd[Row].Numer[i]);
    end;
    //if Cmd[Row].Comment<>'' then Form1.Memo2.Lines.Add(Cmd[Row].Comment);
  end;

  Mission.ActivePlayer := 1;
  BuildMiniMap();
end;

procedure ExportText(filename: string);
var
  i: integer;
begin
  assignfile(ft, filename);
  rewrite(ft);
  writeln(ft, 'KaM Editor exported scripts for houses and roads');
  writeln(ft, '------------------------------------------------');
  writeln(ft);
  for i := 1 to 8 do
  begin
    writeln(ft, 'Player #' + IntToStr(i));
    writeln(ft, Mission.Player[i].GetAllHouseStrings);
    writeln(ft, Mission.GetAllRoadStrings(i));
    //for k:=1 to Map.Y do for j:=1 to Map.X do
    //if (Roads[j,k].Built)and(Roads[j,k].Owner=i) then
    //writeln(ft,'!SET_STREET '+IntToStr(j-1)+' '+IntToStr(k-1));
    writeln(ft);
    writeln(ft);
  end;
  closefile(ft);
end;

end.
