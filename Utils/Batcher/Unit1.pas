unit Unit1;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Windows, Classes, Controls, Forms, Math, StdCtrls, SysUtils,
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
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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
  I,J,K: Integer;
  PathToMaps: TStringList;
  Edited: Boolean;
begin
  Memo1.Clear;
  ControlsEnable(False);
  SetUp;

  PathToMaps := TStringList.Create;
  try
    TKMapsCollection.GetAllMapPaths(ExeDir, PathToMaps);

    for I := 0 to PathToMaps.Count - 1 do
    begin
      fGameApp.NewMapEditor(PathToMaps[I], 0, 0);

      Edited := False;
      for K := 0 to fPlayers.Count - 1 do
      for J := fPlayers[K].Goals.Count - 1 downto 0 do
      if fPlayers[K].Goals[J].GoalCondition = gc_Time then
      begin
        fPlayers[K].Goals.Delete(J);

        Memo1.Lines.Append(ExtractFileName(PathToMaps[I]));
        Edited := True;
      end;

      if Edited then
        fGameApp.Game.SaveMapEditor(PathToMaps[I]);
    end;

  finally
    PathToMaps.Free;
  end;

  TearDown;
  ControlsEnable(True);
end;


end.
