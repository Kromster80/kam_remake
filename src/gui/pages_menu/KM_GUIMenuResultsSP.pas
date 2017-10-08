unit KM_GUIMenuResultsSP;
{$I KaM_Remake.inc}
interface
uses
  Controls, SysUtils,
  KM_Controls, KM_Defaults, KM_Pics,
  KM_InterfaceDefaults, KM_Campaigns, KM_Game;


type
  TKMMenuResultsSP = class (TKMMenuPageCommon)
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class
    fGameResultMsg: TGameResultMsg; //So we know where to go after results screen
    fGameMode: TGameMode;

    //Story behind these seemingly superflous elements that
    //we need to carry on from previous Game:
    //- Mission script does not knows GameName
    //- Mission does not knows to which CampaignName/Map it belongs
    //- PathName to mission and savegame (incase mission is missing we can load .bas)
    fRepeatGameName: UnicodeString;
    fRepeatMission: UnicodeString;
    fRepeatSave: UnicodeString;
    fRepeatCampName: TKMCampaignId;
    fRepeatCampMap: Byte;
    fRepeatLocation: Byte;
    fRepeatColor: Cardinal;

    fIsStatsRefreshed: Boolean;

    procedure Create_Results(aParent: TKMPanel);
    procedure Refresh;

    procedure GraphToggle(Sender: TObject);

    procedure MoreStatsClick(Sender: TObject);
    procedure BackClick(Sender: TObject);
    procedure ContinueClick(Sender: TObject);
    procedure RepeatClick(Sender: TObject);
  protected
    Panel_Results: TKMPanel;
      Label_Results: TKMLabel;
      Panel_Stats: TKMPanel;
        Label_Stat: array [1..9] of TKMLabel;
      Panel_StatsCharts: TKMPanel;
        Button_ResultsArmy,
        Button_ResultsCitizens,
        Button_ResultsHouses,
        Button_ResultsWares,
        Button_MoreStats: TKMButtonFlat;
        Chart_Army: TKMChart;
        Chart_Citizens: TKMChart;
        Chart_Houses: TKMChart;
        Chart_Wares: TKMChart;
      Button_ResultsBack, Button_ResultsRepeat, Button_ResultsContinue: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);

    procedure Show(aMsg: TGameResultMsg);
  end;


implementation
uses
  KM_ResTexts, KM_GameApp, KM_HandsCollection,
  KM_CommonUtils, KM_Resource, KM_Hand, KM_CommonTypes, KM_RenderUI, KM_ResFonts,
  KM_ResWares, KM_HandStats;


{ TKMGUIMenuResultsSP }
constructor TKMMenuResultsSP.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
begin
  inherited Create;

  fOnPageChange := aOnPageChange;
  Create_Results(aParent);
end;


procedure TKMMenuResultsSP.Refresh;
var
  TempGraphCount: Integer;
  TempGraphs: array [0..MAX_HANDS-1] of record
                                          OwnerName: UnicodeString;
                                          Color: Cardinal;
                                          G: TKMCardinalArray;
                                        end;

  //Temp graphs are used to adjoin same colored AI opponents into one chart
  procedure AddToTempGraph(const aOwnerName: UnicodeString; aColor: Cardinal; aGraph: TKMCardinalArray);
  var
    I, ID: Integer;
  begin
    ID := -1;
    for I := 0 to TempGraphCount - 1 do
      if aColor = TempGraphs[I].Color then
      begin
        ID := I;
        Break;
      end;
    if ID = -1 then
    begin
      ID := TempGraphCount;
      Inc(TempGraphCount);
      TempGraphs[ID].G := aGraph; //Overwrite existing graph
    end
    else
      for I := 0 to Length(aGraph) - 1 do
        Inc(TempGraphs[ID].G[I], aGraph[I]); //Add each element to the existing elements

    TempGraphs[ID].Color := aColor;
    TempGraphs[ID].OwnerName := aOwnerName;
  end;

var
  I: Integer;
  R: TWareType;
  G: TKMCardinalArray;
  HumanId: TKMHandIndex;
  ShowAIResults: Boolean;
begin
  if fIsStatsRefreshed then Exit;

  fGameMode := gGame.GameMode;

  //Remember which map we played so we could restart it
  fRepeatGameName := gGame.GameName;
  fRepeatMission := gGame.GetMissionFile;
  fRepeatSave := gGame.SaveFile;
  fRepeatCampName := gGame.CampaignName;
  fRepeatCampMap := gGame.CampaignMap;
  fRepeatLocation := gGame.PlayerLoc;
  fRepeatColor := gGame.PlayerColor;

  // When exit mission update stats to build actual charts
  // without CHARTS_SAMPLING_FOR_TACTICS or CHARTS_SAMPLING_FOR_ECONOMY delays
  // so measurements for warriors/goods produces will not differ from charts
  for I := 0 to gHands.Count - 1 do
    gHands[I].Stats.UpdateState;

  //If the player canceled mission, hide the AI graph lines so he doesn't see secret info about enemy (e.g. army size)
  //That info should only be visible if the mission was won or a replay
  ShowAIResults := (fGameResultMsg in [gr_Win, gr_ReplayEnd]);

  //Restart button is hidden if you won or if it is a replay
  Button_ResultsRepeat.Visible := not (fGameResultMsg in [gr_ReplayEnd, gr_Win]);

  //Even if the campaign is complete Player can now return to it's screen to replay any of the maps
  Button_ResultsContinue.Visible := (gGameApp.Campaigns.ActiveCampaign <> nil) and (fGameResultMsg <> gr_ReplayEnd);
  Button_ResultsContinue.Enabled := fGameResultMsg = gr_Win;

  //Header
  case fGameResultMsg of
    gr_Win:       Label_Results.Caption := gResTexts[TX_MENU_MISSION_VICTORY];
    gr_Defeat:    Label_Results.Caption := gResTexts[TX_MENU_MISSION_DEFEAT];
    gr_Cancel:    Label_Results.Caption := gResTexts[TX_MENU_MISSION_CANCELED];
    gr_ReplayEnd: Label_Results.Caption := gResTexts[TX_MENU_REPLAY_ENDED];
    else          Label_Results.Caption := NO_TEXT;
  end;

  //Append mission name and time after the result message
  Label_Results.Caption := Label_Results.Caption + ' - ' + gGame.GameName; //Don't show the mission time in SP because it's already shown elsewhere

  //This is SP menu, we are dead sure there's only one Human player
  HumanId := -1;
  for I := 0 to gHands.Count - 1 do
    if gHands[I].HandType = hndHuman then
      HumanId := I;

  //List values (like old KaM did)
  with gHands[HumanId].Stats do
  begin
    Label_Stat[1].Caption := IntToStr(GetCitizensLost + GetWarriorsLost);
    Label_Stat[2].Caption := IntToStr(GetCitizensKilled + GetWarriorsKilled);
    Label_Stat[3].Caption := IntToStr(GetHousesLost);
    Label_Stat[4].Caption := IntToStr(GetHousesDestroyed);
    Label_Stat[5].Caption := IntToStr(GetHousesBuilt);
    Label_Stat[6].Caption := IntToStr(GetCitizensTrained);
    Label_Stat[7].Caption := IntToStr(GetWarfareProduced);
    Label_Stat[8].Caption := IntToStr(GetWarriorsTrained);
    Label_Stat[9].Caption := TimeToString(gGame.MissionTime);
  end;

  //Chart values
  Chart_Army.Clear;
  Chart_Citizens.Clear;
  Chart_Houses.Clear;
  Chart_Wares.Clear;
  Chart_Army.MaxLength      := gHands[HumanId].Stats.ChartCount;
  Chart_Citizens.MaxLength  := gHands[HumanId].Stats.ChartCount;
  Chart_Houses.MaxLength    := gHands[HumanId].Stats.ChartCount;
  Chart_Wares.MaxLength     := gHands[HumanId].Stats.ChartCount;

  Chart_Army.MaxTime      := gGame.GameTickCount div 10;
  Chart_Citizens.MaxTime  := gGame.GameTickCount div 10;
  Chart_Houses.MaxTime    := gGame.GameTickCount div 10;
  Chart_Wares.MaxTime     := gGame.GameTickCount div 10;

  //Citizens
  TempGraphCount := 0; //Reset
  for I := 0 to gHands.Count - 1 do
    with gHands[I] do
      if HandType = hndComputer then
        AddToTempGraph(OwnerName(False), FlagColor, Stats.ChartCitizens)
      else
      begin
        Chart_Citizens.AddLine(OwnerName, FlagColor, Stats.ChartCitizens);
        //Recruits aren't that important, but if we want to include them they should be a separate graph
        //Chart_Citizens.AddAltLine(Stats.ChartRecruits);
      end;

  if ShowAIResults then
    for I := 0 to TempGraphCount - 1 do
      Chart_Citizens.AddLine(TempGraphs[I].OwnerName, TempGraphs[I].Color, TempGraphs[I].G);

  //Houses
  TempGraphCount := 0; //Reset
  for I := 0 to gHands.Count - 1 do
    with gHands[I] do
      if HandType = hndComputer then
        AddToTempGraph(OwnerName(False), FlagColor, Stats.ChartHouses)
      else
        Chart_Houses.AddLine(OwnerName, FlagColor, Stats.ChartHouses);

  if ShowAIResults then
    for I := 0 to TempGraphCount - 1 do
      Chart_Houses.AddLine(TempGraphs[I].OwnerName, TempGraphs[I].Color, TempGraphs[I].G);

  //Wares
  for R := WARE_MIN to WARE_MAX do
  begin
    G := gHands[HumanId].Stats.ChartWares[R];
    for I := 0 to High(G) do
      if G[I] <> 0 then
      begin
        Chart_Wares.AddLine(gRes.Wares[R].Title, gRes.Wares[R].GUIColor or $FF000000, G);
        Break;
      end;
  end;

  //Army
  TempGraphCount := 0; //Reset
  for I := 0 to gHands.Count - 1 do
  with gHands[I] do
    if HandType = hndComputer then
      AddToTempGraph(OwnerName(False), FlagColor, Stats.ChartArmy[cak_Instantaneous, ut_Any])
    else
      Chart_Army.AddLine(OwnerName, FlagColor, Stats.ChartArmy[cak_Instantaneous, ut_Any]);

  if ShowAIResults then
    for I := 0 to TempGraphCount - 1 do
      Chart_Army.AddLine(TempGraphs[I].OwnerName, TempGraphs[I].Color, TempGraphs[I].G);

  Button_ResultsHouses.Enabled := (gGame.MissionMode = mm_Normal);
  Button_ResultsCitizens.Enabled := (gGame.MissionMode = mm_Normal);
  Button_ResultsWares.Enabled := (gGame.MissionMode = mm_Normal);
  GraphToggle(Button_ResultsArmy);
end;


procedure TKMMenuResultsSP.GraphToggle(Sender: TObject);
begin
  Chart_Army.Visible := Sender = Button_ResultsArmy;
  Chart_Citizens.Visible := Sender = Button_ResultsCitizens;
  Chart_Houses.Visible := Sender = Button_ResultsHouses;
  Chart_Wares.Visible := Sender = Button_ResultsWares;

  Button_ResultsArmy.Down := Sender = Button_ResultsArmy;
  Button_ResultsCitizens.Down := Sender = Button_ResultsCitizens;
  Button_ResultsHouses.Down := Sender = Button_ResultsHouses;
  Button_ResultsWares.Down := Sender = Button_ResultsWares;
end;


procedure TKMMenuResultsSP.Show(aMsg: TGameResultMsg);
begin
  if aMsg <> gr_ShowStats then //Do not update game result, if we came back from MP Stats page
    fGameResultMsg := aMsg;

  fIsStatsRefreshed := (aMsg = gr_ShowStats);

  Refresh;
  Panel_Results.Show;
end;


procedure TKMMenuResultsSP.Create_Results(aParent: TKMPanel);
const
  LEGEND_WIDTH = 150;
  StatText: array [1..9] of Word = (
    TX_RESULTS_UNITS_LOST,      TX_RESULTS_UNITS_DEFEATED,  TX_RESULTS_HOUSES_LOST,
    TX_RESULTS_HOUSES_DESTROYED,TX_RESULTS_HOUSES_BUILT,    TX_RESULTS_UNITS_TRAINED,
    TX_RESULTS_WEAPONS_MADE,    TX_RESULTS_SOLDIERS_TRAINED,TX_RESULTS_MISSION_TIME);
var
  I, Adv: Integer;
begin
  Panel_Results := TKMPanel.Create(aParent,0,0,aParent.Width, aParent.Height);
  Panel_Results.AnchorsStretch;
    //Background image
    with TKMImage.Create(Panel_Results,0,0,aParent.Width, aParent.Height,7,rxGuiMain) do
    begin
      ImageStretch;
      AnchorsCenter;
    end;
    //Fade to black by 62.5%
    with TKMShape.Create(Panel_Results,0,0,aParent.Width, aParent.Height) do
    begin
      AnchorsCenter;
      FillColor := $A0000000;
    end;

    Label_Results := TKMLabel.Create(Panel_Results,62,140,900,20,NO_TEXT,fnt_Metal,taCenter);
    Label_Results.Anchors := [anLeft];

    Panel_Stats := TKMPanel.Create(Panel_Results, 30, 216, 360, 354);
    Panel_Stats.Anchors := [anLeft];

      //Backplate for column results
      with TKMImage.Create(Panel_Stats, 0, 0, 360, 354, 3, rxGuiMain) do
      begin
        ImageStretch;
        AnchorsCenter;
      end;

      Adv := 0;
      for I := 1 to 9 do
      begin
        Inc(Adv, 25);
        if I in [3,6,7] then inc(Adv, 15);
        if I = 9 then inc(Adv, 45); //Last one goes right at the bottom of the scroll
        TKMLabel.Create(Panel_Stats,20,Adv,240,20,gResTexts[StatText[I]],fnt_Metal,taLeft);
        Label_Stat[I] := TKMLabel.Create(Panel_Stats,260,Adv,80,20,'00',fnt_Metal,taRight);
      end;

    Panel_StatsCharts := TKMPanel.Create(Panel_Results, 410, 170, 630, 420);
    Panel_StatsCharts.Anchors := [anLeft];

    Button_ResultsArmy := TKMButtonFlat.Create(Panel_StatsCharts, 40, 0, 208, 20, 53, rxGui);
    Button_ResultsArmy.TexOffsetX := -91;
    Button_ResultsArmy.TexOffsetY := 7;
    Button_ResultsArmy.Anchors := [anLeft];
    Button_ResultsArmy.Caption := gResTexts[TX_GRAPH_ARMY];
    Button_ResultsArmy.CapOffsetY := -11;
    Button_ResultsArmy.OnClick := GraphToggle;

    Button_ResultsCitizens := TKMButtonFlat.Create(Panel_StatsCharts, 40, 22, 208, 20, 588, rxGui);
    Button_ResultsCitizens.TexOffsetX := -92;
    Button_ResultsCitizens.TexOffsetY := 6;
    Button_ResultsCitizens.Anchors := [anLeft];
    Button_ResultsCitizens.Caption := gResTexts[TX_GRAPH_CITIZENS];
    Button_ResultsCitizens.CapOffsetY := -11;
    Button_ResultsCitizens.OnClick := GraphToggle;

    Button_ResultsHouses := TKMButtonFlat.Create(Panel_StatsCharts, 252, 0, 208, 20, 587, rxGui);
    Button_ResultsHouses.TexOffsetX := -93;
    Button_ResultsHouses.TexOffsetY := 6;
    Button_ResultsHouses.Anchors := [anLeft];
    Button_ResultsHouses.Caption := gResTexts[TX_GRAPH_HOUSES];
    Button_ResultsHouses.CapOffsetY := -11;
    Button_ResultsHouses.OnClick := GraphToggle;

    Button_ResultsWares := TKMButtonFlat.Create(Panel_StatsCharts, 252, 22, 208, 20, 360, rxGui);
    Button_ResultsWares.TexOffsetX := -93;
    Button_ResultsWares.TexOffsetY := 6;
    Button_ResultsWares.Anchors := [anLeft];
    Button_ResultsWares.Caption := gResTexts[TX_GRAPH_RESOURCES];
    Button_ResultsWares.CapOffsetY := -11;
    Button_ResultsWares.OnClick := GraphToggle;

    Chart_Army := TKMChart.Create(Panel_StatsCharts, 0, 46, 610, 374);
    Chart_Army.LegendWidth := LEGEND_WIDTH;
    Chart_Army.Caption := gResTexts[TX_GRAPH_ARMY];
    Chart_Army.Anchors := [anLeft];

    Chart_Citizens := TKMChart.Create(Panel_StatsCharts, 0, 46, 610, 374);
    Chart_Citizens.LegendWidth := LEGEND_WIDTH;
    Chart_Citizens.Caption := gResTexts[TX_GRAPH_CITIZENS];
    Chart_Citizens.Anchors := [anLeft];

    Chart_Houses := TKMChart.Create(Panel_StatsCharts, 0, 46, 610, 374);
    Chart_Houses.LegendWidth := LEGEND_WIDTH;
    Chart_Houses.Caption := gResTexts[TX_GRAPH_HOUSES];
    Chart_Houses.Anchors := [anLeft];

    Chart_Wares := TKMChart.Create(Panel_StatsCharts, 0, 46, 610, 374);
    Chart_Wares.LegendWidth := LEGEND_WIDTH;
    Chart_Wares.Caption := gResTexts[TX_GRAPH_TITLE_RESOURCES];
    Chart_Wares.Anchors := [anLeft];

    Button_MoreStats := TKMButtonFlat.Create(Panel_StatsCharts, 610 - LEGEND_WIDTH + 2, Chart_Wares.Bottom - 60, LEGEND_WIDTH, 40, 663, rxGui);
    Button_MoreStats.TexOffsetX := -LEGEND_WIDTH div 2 + 14;
    Button_MoreStats.TexOffsetY := 6;
    Button_MoreStats.Anchors := [anLeft];
    Button_MoreStats.Caption := 'More|statistics'; //Todo translate
    Button_MoreStats.CapOffsetX := 12;
    Button_MoreStats.CapOffsetY := -20;
    Button_MoreStats.OnClick := MoreStatsClick;

    Button_ResultsBack := TKMButton.Create(Panel_Results, 30, 610, 220, 30, gResTexts[TX_MENU_BACK], bsMenu);
    Button_ResultsBack.Anchors := [anLeft];
    Button_ResultsBack.OnClick := BackClick;
    Button_ResultsRepeat := TKMButton.Create(Panel_Results, 270, 610, 220, 30, gResTexts[TX_MENU_MISSION_REPEAT], bsMenu);
    Button_ResultsRepeat.Anchors := [anLeft];
    Button_ResultsRepeat.OnClick := RepeatClick;
    Button_ResultsContinue := TKMButton.Create(Panel_Results, 510, 610, 220, 30, gResTexts[TX_MENU_MISSION_NEXT], bsMenu);
    Button_ResultsContinue.Anchors := [anLeft];
    Button_ResultsContinue.OnClick := ContinueClick;
end;


procedure TKMMenuResultsSP.MoreStatsClick(Sender: TObject);
begin
  fOnPageChange(gpResultsMP);
end;


procedure TKMMenuResultsSP.BackClick(Sender: TObject);
begin
  //Depending on where we were created we need to return to a different place
  //Campaign game end     -> ResultsSP -> Main menu
  //Singleplayer game end -> ResultsSP -> Singleplayer
  //Replay end            -> ResultsSP -> Replays

  if fGameResultMsg = gr_ReplayEnd then
    fOnPageChange(gpReplays)
  else if fGameMode = gmSingle then
    fOnPageChange(gpSinglePlayer)
  else
    fOnPageChange(gpMainMenu);
end;


procedure TKMMenuResultsSP.ContinueClick(Sender: TObject);
var TheCampaign: UnicodeString;
begin
  TheCampaign := Char(fRepeatCampName[0]) + Char(fRepeatCampName[1]) + Char(fRepeatCampName[2]);
  fOnPageChange(gpCampaign, TheCampaign);
end;


procedure TKMMenuResultsSP.RepeatClick(Sender: TObject);
begin
  //Means replay last map
  gGameApp.NewRestartLast(fRepeatGameName, fRepeatMission, fRepeatSave, fGameMode, fRepeatCampName, fRepeatCampMap, fRepeatLocation, fRepeatColor);
end;


end.
