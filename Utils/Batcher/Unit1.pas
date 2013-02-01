unit Unit1;
interface
uses
  Windows, Classes, Controls, Forms, Math, StdCtrls, SysUtils,
  KM_Points, KM_Defaults, KM_CommonClasses, KromUtils,
  KM_GameApp, KM_Locales, KM_Log, KM_PlayersCollection, KM_TextLibrary,
  KM_Maps, KM_MissionScript_Info, KM_Terrain, KM_Units_Warrior, KM_Utils;


type
  TForm1 = class(TForm)
    Button3: TButton;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure SetUp;
    procedure TearDown;
    procedure ControlsEnable(aFlag: Boolean);
  end;


var
  Form1: TForm1;


implementation
{$R *.dfm}


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
  fGameApp := TKMGameApp.Create(0, 1024, 768, False, nil, nil, nil, True);
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
  Count: Integer;
  SearchRec: TSearchRec;
  PathToMaps: TStringList;
  MissionParser: TMissionParserInfo;
  MapInfo: TKMapInfo;
  WinCond, DefeatCond: array [TGoalCondition] of Word;
  WinStat, DefeatStat: array [TGoalStatus] of Word;
  GC: TGoalCondition;
begin
  ControlsEnable(False);
  SetUp;

  Count := 0;
  MissionParser := TMissionParserInfo.Create(False);
  MapInfo := TKMapInfo.Create;

  PathToMaps := TStringList.Create;
  try
    PathToMaps.Add(ExeDir + 'Maps\');
    PathToMaps.Add(ExeDir + 'MapsMP\');
    PathToMaps.Add(ExeDir + 'Tutorials\');

    //Include all campaigns maps
    FindFirst(ExeDir + 'Campaigns\*', faDirectory, SearchRec);
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        PathToMaps.Add(ExeDir + 'Campaigns\' + SearchRec.Name + '\');
    until (FindNext(SearchRec) <> 0);
    FindClose(SearchRec);

    for I := 0 to PathToMaps.Count - 1 do
      if DirectoryExists(PathToMaps[I]) then
      begin
        FindFirst(PathToMaps[I] + '*', faDirectory, SearchRec);
        repeat
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
          and FileExists(PathToMaps[I] + SearchRec.Name + '\' + SearchRec.Name + '.map') then
          begin
            MissionParser.LoadMission(PathToMaps[I] + SearchRec.Name + '\' + SearchRec.Name + '.dat', MapInfo);
            for J := 0 to MAX_PLAYERS - 1 do
            begin
              for K := 0 to MapInfo.GoalsVictoryCount[J] - 1 do
                Inc(WinCond[MapInfo.GoalsVictory[J,K].Cond]);
              for K := 0 to MapInfo.GoalsSurviveCount[J] - 1 do
                Inc(DefeatCond[MapInfo.GoalsSurvive[J,K].Cond]);
              for K := 0 to MapInfo.GoalsVictoryCount[J] - 1 do
                Inc(WinStat[MapInfo.GoalsVictory[J,K].Stat]);
              for K := 0 to MapInfo.GoalsSurviveCount[J] - 1 do
                Inc(DefeatStat[MapInfo.GoalsSurvive[J,K].Stat]);
            end;
            Inc(Count);
          end;
        until (FindNext(SearchRec) <> 0);
        FindClose(SearchRec);
      end;

  finally
    PathToMaps.Free;
  end;

  Memo1.Clear;
  for GC := Low(TGoalCondition) to High(TGoalCondition) do
  begin
    Memo1.Lines.Append(Format('%d / %d ' + GoalConditionStr[GC], [WinCond[GC], DefeatCond[GC]]))
  end;

  TearDown;
  ControlsEnable(True);
end;

end.
