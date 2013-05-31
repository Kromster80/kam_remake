unit Unit1;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Windows, Classes, Controls, Forms, Math, StdCtrls, SysUtils, StrUtils,
  KM_Defaults, KM_CommonClasses, KM_Points, KromUtils,
  KM_GameApp, KM_Locales, KM_Log, KM_PlayersCollection, KM_TextLibrary,
  KM_Maps, KM_MissionScript_Info, KM_Terrain, KM_Utils;


type
  TForm1 = class(TForm)
    Button3: TButton;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Button4: TButton;
    btnUnXorAll: TButton;
    btnXorAll: TButton;
    Button7: TButton;
    Button8: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btnXorAllClick(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    procedure SetUp;
    procedure TearDown;
    procedure ControlsEnable(aFlag: Boolean);
  end;


var
  Form1: TForm1;


implementation
uses KM_MissionScript;
{$R *.dfm}


type
  TMissionParserPatcher = class(TMissionParserCommon)
  public
    function ReadMissionFile(const aFileName: string): AnsiString;
    procedure SaveToFile(aTxt: AnsiString; const aFileName: string);
  end;


{ TMissionParserPatcher }
function TMissionParserPatcher.ReadMissionFile(const aFileName: string): AnsiString;
var
  I,Num: Cardinal;
  F: TMemoryStream;
begin
  Result := '';

  if not FileExists(aFileName) then Exit;

  //Load and decode .DAT file into FileText
  F := TMemoryStream.Create;
  try
    F.LoadFromFile(aFileName);

    if F.Size = 0 then Exit;

    //Detect whether mission is encoded so we can support decoded/encoded .DAT files
    //We can't test 1st char, it can be any. Instead see how often common chracters meet
    Num := 0;
    for I := 0 to F.Size - 1 do               //tab, eol, 0..9, space, !
      if PByte(Cardinal(F.Memory)+I)^ in [9,10,13,ord('0')..ord('9'),$20,$21] then
        Inc(Num);

    //Usually 30-50% is numerals/spaces, tested on typical KaM maps, take half of that as margin
    if (Num / F.Size < 0.20) then
    for I := 0 to F.Size - 1 do
      PByte(Cardinal(F.Memory)+I)^ := PByte(Cardinal(F.Memory)+I)^ xor 239;

    SetString(Result, PAnsiChar(F.Memory), F.Size div SizeOf(AnsiChar));
  finally
    F.Free;
  end;
end;


procedure TMissionParserPatcher.SaveToFile(aTxt: AnsiString; const aFileName: string);
var
  I: Integer;
  F: TMemoryStream;
begin
  F := TMemoryStream.Create;
  try
    F.Write(Pointer(aTxt)^, Length(aTxt) * SizeOf(AnsiChar));

    for I := 0 to F.Size - 1 do
      PByte(Cardinal(F.Memory)+I)^ := PByte(Cardinal(F.Memory)+I)^ xor 239;

    F.SaveToFile(aFileName);
  finally
    F.Free;
  end;
end;


procedure TForm1.ControlsEnable(aFlag: Boolean);
var I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TButton then
      Controls[I].Enabled := aFlag;
end;


procedure TForm1.SetUp;
begin
  SKIP_RENDER := True;
  SKIP_SOUND := True;
  ExeDir := ExtractFilePath(ParamStr(0)) + '..\..\';
  gLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'temp.log');
  fLocales := TKMLocales.Create(ExeDir+'data\locales.txt');
  fTextLibrary := TKMTextLibrary.Create(ExeDir + 'data\text\', 'eng');
  fGameApp := TKMGameApp.Create(nil, 1024, 768, False, nil, nil, nil, True);
  fGameApp.GameSettings.Autosave := False;
end;


procedure TForm1.TearDown;
begin
  fGameApp.Stop(gr_Silent);
  FreeAndNil(fGameApp);
  FreeAndNil(fTextLibrary);
  FreeAndNil(fLocales);
  FreeAndNil(gLog);
end;


//Rename marketplace sprites from rxRemake scheme to rxHouses library
procedure TForm1.Button1Click(Sender: TObject);
var
  SearchRec: TSearchRec;
  NewName: string;
  B: Boolean;
begin
  begin
    FindFirst('..\..\SpriteResource\9\9*.png', faAnyFile, SearchRec);
    repeat
      NewName := SearchRec.Name;
      NewName := StringReplace(NewName, '9_00', '2_17', [rfReplaceAll, rfIgnoreCase]);
      NewName := StringReplace(NewName, '9_01', '2_18', [rfReplaceAll, rfIgnoreCase]);
      NewName := StringReplace(NewName, '9_02', '2_19', [rfReplaceAll, rfIgnoreCase]);
      NewName := StringReplace(NewName, '9_03', '2_20', [rfReplaceAll, rfIgnoreCase]);
      B := RenameFile(ExtractFilePath(ParamStr(0)) + '..\..\SpriteResource\9\' + SearchRec.Name,
                      ExtractFilePath(ParamStr(0)) + '..\..\SpriteResource\9\' + NewName);
      Assert(B);
    until (FindNext(SearchRec) <> 0);
    FindClose(SearchRec);
  end;
end;


//Export message goals into EVT files to allow to rig them easily
procedure TForm1.Button3Click(Sender: TObject);
var
  I: Integer;
begin
  ControlsEnable(False);
  SetUp;

  for I := 0 to fGameApp.Campaigns.CampaignByTitle('TPR').MapCount - 1 do
  begin
    fGameApp.NewCampaignMap(fGameApp.Campaigns.CampaignByTitle('TPR'), I);

    fPlayers[0].Goals.ExportMessages(ExtractFilePath(ParamStr(0)) + Format('TPR%.2d.evt', [I+1]));

    fGameApp.Stop(gr_Silent);
  end;

  TearDown;
  ControlsEnable(True);
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  I,J,K: Integer;
  PathToMaps: TStringList;
  MapInfo: TKMapInfo;
  WinCond, DefeatCond: array [TGoalCondition] of Word;
  GC: TGoalCondition;
begin
  ControlsEnable(False);
  SetUp;

  FillChar(WinCond, SizeOf(WinCond), #0);
  FillChar(DefeatCond, SizeOf(WinCond), #0);

  PathToMaps := TStringList.Create;
  try
    TKMapsCollection.GetAllMapPaths(ExeDir, PathToMaps);

    for I := 0 to PathToMaps.Count - 1 do
    begin
      MapInfo := TKMapInfo.Create(TruncateExt(ExtractFileName(PathToMaps[I])), False, Pos('MapsMP', PathToMaps[I]) > 0);
      MapInfo.LoadExtra;
      for J := 0 to MapInfo.LocCount - 1 do
      begin
        for K := 0 to MapInfo.GoalsVictoryCount[J] - 1 do
          Inc(WinCond[MapInfo.GoalsVictory[J,K].Cond]);
        for K := 0 to MapInfo.GoalsSurviveCount[J] - 1 do
          Inc(DefeatCond[MapInfo.GoalsSurvive[J,K].Cond]);
      end;
    end;

    //Report results
    Memo1.Clear;
    Memo1.Lines.Append(IntToStr(PathToMaps.Count) + ' maps');
    Memo1.Lines.Append('Win / Def');
    for GC := Low(TGoalCondition) to High(TGoalCondition) do
      Memo1.Lines.Append(Format('%3d / %3d ' + GoalConditionStr[GC], [WinCond[GC], DefeatCond[GC]]));
  finally
    PathToMaps.Free;
  end;

  TearDown;
  ControlsEnable(True);
end;


procedure TForm1.Button4Click(Sender: TObject);
var
  I: Integer;
  PathToMaps: TStringList;
  GoalLoc, GoalEnd, GoalSize: Integer;
  Txt, GoalTxt: AnsiString;
  L,R: AnsiString;
  MP: TMissionParserPatcher;
  Args: TStringList;
  GoalLog: TStringList;
begin
  Memo1.Clear;
  ControlsEnable(False);
  SetUp;

  Args := TStringList.Create;
  //@Krom: StrictDelimiter doesn't exist in D7
  //Args.StrictDelimiter := True;
  Args.Delimiter := ' ';

  GoalLog := TStringList.Create;

  PathToMaps := TStringList.Create;
  try
    TKMapsCollection.GetAllMapPaths(ExeDir, PathToMaps);

    //Intent of this design is to rip the specified lines with least impact
    MP := TMissionParserPatcher.Create(False);

    for I := 0 to PathToMaps.Count - 1 do
    begin
      Txt := MP.ReadMissionFile(PathToMaps[I]);

      //Treat all goals

      //Remove goals solely used to display messages at a time
      GoalLog.Clear;
      GoalLoc := 1;
      repeat
        //ADD_GOAL gcTime *status* MsgId Delay
        GoalLoc := PosEx('!ADD_GOAL 2 ', Txt, GoalLoc);
        if GoalLoc <> 0 then
        begin
          //Many maps have letters aligned in columns, meaning that
          //command length is varying cos of spaces between arguments

          //Look for command end marker (!, eol, /)
          GoalEnd := GoalLoc + 1;
          while (GoalEnd < Length(Txt)) and not (Txt[GoalEnd] in ['!', #13, '/']) do
            Inc(GoalEnd);

          GoalTxt := Copy(Txt, GoalLoc, GoalEnd - GoalLoc);
          GoalTxt := StringReplace(GoalTxt, '    ', ' ', [rfReplaceAll]);
          GoalTxt := StringReplace(GoalTxt, '   ', ' ', [rfReplaceAll]);
          GoalTxt := StringReplace(GoalTxt, '  ', ' ', [rfReplaceAll]);
          Args.DelimitedText := GoalTxt;
          //Is this a gcTime MsgId goal
          if Args[3] <> '0' then
          begin
            Memo1.Lines.Append(TruncateExt(ExtractFileName(PathToMaps[I])) + ' ' + GoalTxt);

            GoalLog.Append('  if States.GameTime = ' + Args[4] + ' then' + eol +
                           '    Actions.ShowMsg( ?, States.Text(' + Args[3] + '));');

            //Cut Goal out
            L := LeftStr(Txt, GoalLoc - 1);
            R := RightStr(Txt, Length(Txt) - GoalEnd + 1);
            Txt := L + R;

            //Keep GoalLoc in place incase there are two consequential goals
          end
          else
            Inc(GoalLoc, Length(GoalTxt));
        end;
      until (GoalLoc = 0);

      if GoalLog.Count > 0 then
        GoalLog.SaveToFile(ChangeFileExt(PathToMaps[I], '.goals.log'));

      MP.SaveToFile(Txt, PathToMaps[I]);
    end;
  finally
    PathToMaps.Free;
  end;

  Memo1.Lines.Append(IntToStr(Memo1.Lines.Count));

  TearDown;
  ControlsEnable(True);
end;


procedure TForm1.Button5Click(Sender: TObject);
var
  I, K: Integer;
  PathToMaps: TStringList;
  CurrLoc, CurrEnd, NextCurrLoc, AILoc, AIEnd: Integer;
  Txt: AnsiString;
  PlayerId, AiId: Integer;
  MP: TMissionParserPatcher;
  PlayersSet: array [0 .. MAX_PLAYERS - 1] of Boolean;
  s: string;
begin
  Memo1.Clear;
  ControlsEnable(False);
  SetUp;

  //Intent of this design is to rip the specified lines with least impact
  MP := TMissionParserPatcher.Create(False);

  PathToMaps := TStringList.Create;
  try
    TKMapsCollection.GetAllMapPaths(ExeDir, PathToMaps);

    //Parse only MP maps
    for I := 0 to PathToMaps.Count - 1 do
    if Pos('\MapsMP\', PathToMaps[I]) <> 0 then
    begin
      Txt := MP.ReadMissionFile(PathToMaps[I]);

      CurrLoc := 1;
      FillChar(PlayersSet, SizeOf(PlayersSet), #0);
      repeat
        //SET_CURR_PLAYER player_id
        CurrLoc := PosEx('!SET_CURR_PLAYER ', Txt, CurrLoc);
        if CurrLoc <> 0 then
        begin
          //Many maps have letters aligned in columns, meaning that
          //command length is varying cos of spaces between arguments
          //Look for command end marker (!, eol, /)
          CurrEnd := CurrLoc + 16;
          while (CurrEnd < Length(Txt)) and not (Txt[CurrEnd] in ['!', #13, '/']) do
            Inc(CurrEnd);
          s := Trim(Copy(Txt, CurrLoc + 16, CurrEnd - (CurrLoc + 16)));
          PlayerId := StrToInt(s);

          NextCurrLoc := PosEx('!SET_CURR_PLAYER ', Txt, CurrLoc+1);
          AILoc := PosEx('!SET_AI_PLAYER', Txt, CurrLoc+1);
          AiId := -1;
          if AILoc <> 0 then
          begin
            //Many maps have letters aligned in columns, meaning that
            //command length is varying cos of spaces between arguments
            //Look for command end marker (!, eol, /)
            AIEnd := AILoc + 14;
            while (AIEnd < Length(Txt)) and not (Txt[AIEnd] in ['!', #13, '/']) do
              Inc(AIEnd);
            s := Trim(Copy(Txt, AILoc + 14, AIEnd - (AILoc + 14)));
            AiId := StrToIntDef(s, -1);
          end;

          //Many times MP maps change CURR player to adjoin similar stuff in sections
          if not PlayersSet[PlayerId] then
            if (AILoc = 0) or ((AILoc > NextCurrLoc) and (NextCurrLoc <> 0)) then
            begin
              //Add from new line
              Insert(eol + '!SET_AI_PLAYER', Txt, CurrEnd);
              if AiId <> -1 then
                PlayersSet[AiId] := True
              else
                PlayersSet[PlayerId] := True;
            end;

          CurrLoc := CurrEnd;
        end;
      until (CurrLoc = 0);

      MP.SaveToFile(Txt, PathToMaps[I]);

      s := '';
      for K := 0 to MAX_PLAYERS - 1 do
        s := s + IfThen(PlayersSet[K], '1', '0');

      Memo1.Lines.Append(s + ' ' + TruncateExt(ExtractFileName(PathToMaps[I])));
    end;
  finally
    PathToMaps.Free;
  end;

  Memo1.Lines.Append(IntToStr(Memo1.Lines.Count));

  TearDown;
  ControlsEnable(True);
end;


procedure TForm1.Button6Click(Sender: TObject);
var
  I, K: Integer;
  PathToMaps: TStringList;
  CurrLoc, CurrEnd: Integer;
  Txt: AnsiString;
  MP: TMissionParserPatcher;
  s: string;
begin
  Memo1.Clear;
  ControlsEnable(False);
  SetUp;

  //Intent of this design is to rip the specified lines with least impact
  MP := TMissionParserPatcher.Create(False);

  PathToMaps := TStringList.Create;
  try
    TKMapsCollection.GetAllMapPaths(ExeDir, PathToMaps);

    //Parse only MP maps
    for I := 0 to PathToMaps.Count - 1 do
    if Pos('\MapsMP\', PathToMaps[I]) <> 0 then
    begin
      Txt := MP.ReadMissionFile(PathToMaps[I]);

      //Show goals which have messages in them
      CurrLoc := 1;
      repeat
        //SET_CURR_PLAYER player_id
        CurrLoc := PosEx('!SET_AI_PLAYER', Txt, CurrLoc);
        if CurrLoc <> 0 then
        begin
          //Many maps have letters aligned in columns, meaning that
          //command length is varying cos of spaces between arguments
          //Look for command end marker (!, eol, /)
          CurrEnd := CurrLoc + 14;
          while (CurrEnd < Length(Txt)) and not (Txt[CurrEnd] in ['!', #13, '/']) do
            Inc(CurrEnd);

            Insert(eol + '!SET_AI_AUTO_DEFENCE', Txt, CurrEnd);

          CurrLoc := CurrEnd;
        end;
      until (CurrLoc = 0);

      MP.SaveToFile(Txt, PathToMaps[I]);

      Memo1.Lines.Append(TruncateExt(ExtractFileName(PathToMaps[I])));
    end;
  finally
    PathToMaps.Free;
  end;

  Memo1.Lines.Append(IntToStr(Memo1.Lines.Count));

  TearDown;
  ControlsEnable(True);
end;


procedure TForm1.Button7Click(Sender: TObject);
var
  I: Integer;
  PathToMaps: TStringList;
  GoalLoc, GoalEnd: Integer;
  Txt, GoalTxt: AnsiString;
  MP: TMissionParserPatcher;
  Args: TStringList;
begin
  Memo1.Clear;
  ControlsEnable(False);
  SetUp;

  Args := TStringList.Create;
  //@Krom: StrictDelimiter doesn't exist in D7
  //Args.StrictDelimiter := True;
  Args.Delimiter := ' ';

  PathToMaps := TStringList.Create;
  try
    TKMapsCollection.GetAllMapPaths(ExeDir, PathToMaps);

    //Intent of this design is to rip the specified lines with least impact
    MP := TMissionParserPatcher.Create(False);

    for I := 0 to PathToMaps.Count - 1 do
    begin
      Txt := MP.ReadMissionFile(PathToMaps[I]);

      //Show goals which have messages in them
      GoalLoc := 1;
      repeat
        //ADD_GOAL *condition* *status* MsgId *player_id*
        GoalLoc := PosEx('!ADD_GOAL ', Txt, GoalLoc);
        if GoalLoc <> 0 then
        begin
          //Many maps have letters aligned in columns, meaning that
          //command length is varying cos of spaces between arguments

          //Look for command end marker (!, eol, /)
          GoalEnd := GoalLoc + 1;
          while (GoalEnd < Length(Txt)) and not (Txt[GoalEnd] in ['!', #13, '/']) do
            Inc(GoalEnd);

          GoalTxt := Copy(Txt, GoalLoc, GoalEnd - GoalLoc);
          GoalTxt := StringReplace(GoalTxt, '    ', ' ', [rfReplaceAll]);
          GoalTxt := StringReplace(GoalTxt, '   ', ' ', [rfReplaceAll]);
          GoalTxt := StringReplace(GoalTxt, '  ', ' ', [rfReplaceAll]);
          Args.DelimitedText := GoalTxt;
          //Is this a gcTime MsgId goal
          if Args[3] <> '0' then
          begin
            Memo1.Lines.Append(TruncateExt(ExtractFileName(PathToMaps[I])) + ' ' + GoalTxt);

            //Found only one case, so it was easier to fix by hand
            Inc(GoalLoc, Length(GoalTxt));
          end
          else
            Inc(GoalLoc, Length(GoalTxt));
        end;
      until (GoalLoc = 0);
    end;
  finally
    PathToMaps.Free;
  end;

  Memo1.Lines.Append(IntToStr(Memo1.Lines.Count));

  TearDown;
  ControlsEnable(True);
end;


procedure TForm1.Button8Click(Sender: TObject);
var
  I, K: Integer;
  PathToMaps: TStringList;
  CurrLoc, CurrEnd: Integer;
  Txt: AnsiString;
  PlayerId: Integer;
  MP: TMissionParserPatcher;
  PlayersSet: array [0 .. MAX_PLAYERS - 1] of Boolean;
  s: string;
begin
  Memo1.Clear;
  ControlsEnable(False);
  SetUp;

  //Intent of this design is to rip the specified lines with least impact
  MP := TMissionParserPatcher.Create(False);

  PathToMaps := TStringList.Create;
  try
    TKMapsCollection.GetAllMapPaths(ExeDir, PathToMaps);

    //Parse only MP maps
    for I := 0 to PathToMaps.Count - 1 do
    if Pos('\MapsMP\', PathToMaps[I]) <> 0 then
    begin
      Txt := MP.ReadMissionFile(PathToMaps[I]);

      //First pass, check if SET_USER_PLAYER is already placed
      //if at least one is there we assume this map should not be fixed
      if Pos('!SET_USER_PLAYER', Txt) <> 0 then Continue;

      //Show goals which have messages in them
      CurrLoc := 1;
      FillChar(PlayersSet, SizeOf(PlayersSet), #0);
      repeat
        //SET_CURR_PLAYER player_id
        CurrLoc := PosEx('!SET_CURR_PLAYER ', Txt, CurrLoc);
        if CurrLoc <> 0 then
        begin
          //Many maps have letters aligned in columns, meaning that
          //command length is varying cos of spaces between arguments
          //Look for command end marker (!, eol, /)
          CurrEnd := CurrLoc + 16;
          while (CurrEnd < Length(Txt)) and not (Txt[CurrEnd] in ['!', #13, '/']) do
            Inc(CurrEnd);

          s := Trim(Copy(Txt, CurrLoc + 16, CurrEnd - (CurrLoc + 16)));
          PlayerId := StrToInt(s);

          //Many times MP maps change CURR player to adjoin similar stuff in sections
          if not PlayersSet[PlayerId] then
          begin
            //Add from new line
            Insert(eol + '!SET_USER_PLAYER ', Txt, CurrEnd);
            PlayersSet[PlayerId] := True;
          end;

          CurrLoc := CurrEnd;
        end;
      until (CurrLoc = 0);

      MP.SaveToFile(Txt, PathToMaps[I]);

      s := '';
      for K := 0 to MAX_PLAYERS - 1 do
        s := s + IfThen(PlayersSet[K], '1', '0');

      Memo1.Lines.Append(s + ' ' + TruncateExt(ExtractFileName(PathToMaps[I])));
    end;
  finally
    PathToMaps.Free;
  end;

  Memo1.Lines.Append(IntToStr(Memo1.Lines.Count));

  TearDown;
  ControlsEnable(True);
end;


procedure TForm1.btnXorAllClick(Sender: TObject);
var
  I, K: Integer;
  PathToMaps: TStringList;
  Num: Integer;
  F: TMemoryStream;
  UnXOR: Boolean;
  MapCount: Integer;
begin
  Memo1.Clear;
  ControlsEnable(False);
  SetUp;

  UnXOR := (Sender = btnUnXorAll);

  PathToMaps := TStringList.Create;
  try
    TKMapsCollection.GetAllMapPaths(ExeDir, PathToMaps);

    MapCount := 0;
    for I := 0 to PathToMaps.Count - 1 do
    begin
      //Load and decode .DAT file into FileText
      F := TMemoryStream.Create;
      try
        F.LoadFromFile(PathToMaps[I]);

        if F.Size > 0 then
        begin
          //Detect whether mission is encoded so we can support decoded/encoded .DAT files
          //We can't test 1st char, it can be any. Instead see how often common chracters meet
          Num := 0;
          for K := 0 to F.Size - 1 do               //tab, eol, 0..9, space, !
          if PByte(Cardinal(F.Memory)+K)^ in [9,10,13,ord('0')..ord('9'),$20,$21] then
            Inc(Num);

          //Usually 30-50% is numerals/spaces, tested on typical KaM maps, take half of that as margin
          if (UnXOR and (Num/F.Size < 0.20))
          or (not UnXOR and (Num/F.Size > 0.20)) then
          begin
            for K := 0 to F.Size - 1 do
              PByte(Cardinal(F.Memory)+K)^ := PByte(Cardinal(F.Memory)+K)^ xor 239;
            Inc(MapCount);
          end;

          F.SaveToFile(PathToMaps[I]);
        end
        else
          Memo1.Lines.Append('Mission file ' + ExtractFileName(PathToMaps[I]) + ' is empty');
      finally
        F.Free;
      end;
    end;
  finally
    PathToMaps.Free;
  end;

  Memo1.Lines.Append(IntToStr(MapCount) + ' maps changed');

  TearDown;
  ControlsEnable(True);
end;


end.
