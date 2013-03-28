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
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
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
  F: TStringStream;
begin
  Result := '';

  if not FileExists(aFileName) then Exit;

  //Load and decode .DAT file into FileText
  F := TStringStream.Create;
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

    Result := F.DataString;
  finally
    F.Free;
  end;
end;


procedure TMissionParserPatcher.SaveToFile(aTxt: AnsiString; const aFileName: string);
var
  I: Integer;
  F: TStringStream;
begin
  F := TStringStream.Create;
  try
    F.WriteString(aTxt);

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
  fLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'temp.log');
  fLocales := TKMLocales.Create(ExeDir+'data\locales.txt');
  fTextLibrary := TTextLibrary.Create(ExeDir + 'data\text\', 'eng');
  fGameApp := TKMGameApp.Create(nil, 1024, 768, False, nil, nil, nil, True);
  fGameApp.GameSettings.Autosave := False;
end;


procedure TForm1.TearDown;
begin
  fGameApp.Stop(gr_Silent);
  FreeAndNil(fGameApp);
  FreeAndNil(fTextLibrary);
  FreeAndNil(fLocales);
  FreeAndNil(fLog);
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
      for J := 0 to MapInfo.PlayerCount - 1 do
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
  Args.StrictDelimiter := True;
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
  Args.StrictDelimiter := True;
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


procedure TForm1.Button5Click(Sender: TObject);
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

  UnXOR := (Sender = Button5);

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
