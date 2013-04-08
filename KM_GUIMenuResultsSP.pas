unit KM_GUIMenuResultsSP;
{$I KaM_Remake.inc}
interface
uses
  Controls, SysUtils,
  KM_Controls, KM_Defaults, KM_Pics,
  KM_InterfaceDefaults;


type
  TKMGUIMenuResultsSP = class
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class
    fGameResultMsg: TGameResultMsg; //So we know where to go after results screen

    //Story behind these seemingly superflous elements that
    //we need to carry on from previous Game:
    //- Mission script does not knows GameName
    //- Mission does not knows to which CampaignName/Map it belongs
    //- PathName to mission and savegame (incase mission is missing we can load .bas)
    fRepeatGameName: string;
    fRepeatMission: string;
    fRepeatSave: string;
    fRepeatCampName: AnsiString;
    fRepeatCampMap: Byte;
    fRepeatLocation: Byte;
    fRepeatColor: Cardinal;

    procedure Create_Results(aParent: TKMPanel);
    procedure Refresh;

    procedure GraphToggle(Sender: TObject);

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
        Button_ResultsWares: TKMButtonFlat;
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
uses KM_TextLibrary, KM_Game, KM_GameApp, KM_PlayersCollection,
  KM_Utils, KM_Resource, KM_Player, KM_CommonTypes, KM_RenderUI;


{ TKMGUIMenuResultsSP }
constructor TKMGUIMenuResultsSP.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
begin
  inherited Create;

  fOnPageChange := aOnPageChange;
  Create_Results(aParent);
end;


procedure TKMGUIMenuResultsSP.Refresh;
var
  TempGraphCount: Integer;
  TempGraphs: array [0..MAX_PLAYERS-1] of record
                                          Color: Cardinal;
                                          G: TKMCardinalArray;
                                        end;

  //Temp graphs are used to adjoin same colored AI opponents into one chart
  procedure AddToTempGraph(aColor: Cardinal; aGraph: TKMCardinalArray);
  var I, ID: Integer;
  begin
    ID := -1;
    for I := 0 to TempGraphCount - 1 do
      if aColor = TempGraphs[I].Color then
      begin
        ID := I;
        break;
      end;
    if ID = -1 then
    begin
      ID := TempGraphCount;
      inc(TempGraphCount);
      TempGraphs[ID].G := aGraph; //Overwrite existing graph
    end
    else
      for I := 0 to Length(aGraph) - 1 do
        inc(TempGraphs[ID].G[I], aGraph[I]); //Add each element to the existing elements
    TempGraphs[ID].Color := aColor;
  end;

var
  I: Integer;
  R: TWareType;
  G: TKMCardinalArray;
  HumanId: TPlayerIndex;
begin
  //Restart button is hidden if you won or if it is a replay
  Button_ResultsRepeat.Visible := not (fGameResultMsg in [gr_ReplayEnd, gr_Win]);

  //Even if the campaign is complete Player can now return to it's screen to replay any of the maps
  Button_ResultsContinue.Visible := (fGameApp.Campaigns.ActiveCampaign <> nil) and (fGameResultMsg <> gr_ReplayEnd);
  Button_ResultsContinue.Enabled := fGameResultMsg = gr_Win;

  //Header
  case fGameResultMsg of
    gr_Win:       Label_Results.Caption := fTextLibrary[TX_MENU_MISSION_VICTORY];
    gr_Defeat:    Label_Results.Caption := fTextLibrary[TX_MENU_MISSION_DEFEAT];
    gr_Cancel:    Label_Results.Caption := fTextLibrary[TX_MENU_MISSION_CANCELED];
    gr_ReplayEnd: Label_Results.Caption := fTextLibrary[TX_MENU_REPLAY_ENDED];
    else          Label_Results.Caption := NO_TEXT;
  end;

  //Append mission name and time after the result message
  Label_Results.Caption := Label_Results.Caption + ' - ' + fGame.GameName; //Don't show the mission time in SP because it's already shown elsewhere

  //This is SP menu, we are dead sure there's only one Human player
  HumanId := -1;
  for I := 0 to fPlayers.Count - 1 do
  if fPlayers[I].PlayerType = pt_Human then
    HumanId := I;

  //List values (like old KaM did)
  with fPlayers[HumanId].Stats do
  begin
    Label_Stat[1].Caption := IntToStr(GetCitizensLost + GetWarriorsLost);
    Label_Stat[2].Caption := IntToStr(GetCitizensKilled + GetWarriorsKilled);
    Label_Stat[3].Caption := IntToStr(GetHousesLost);
    Label_Stat[4].Caption := IntToStr(GetHousesDestroyed);
    Label_Stat[5].Caption := IntToStr(GetHousesBuilt);
    Label_Stat[6].Caption := IntToStr(GetCitizensTrained);
    Label_Stat[7].Caption := IntToStr(GetWeaponsProduced);
    Label_Stat[8].Caption := IntToStr(GetWarriorsTrained);
    Label_Stat[9].Caption := TimeToString(fGame.MissionTime);
  end;

  //Chart values
  if DISPLAY_CHARTS_RESULT then
  begin
    Chart_Army.Clear;
    Chart_Citizens.Clear;
    Chart_Houses.Clear;
    Chart_Wares.Clear;
    Chart_Army.MaxLength      := fPlayers[HumanId].Stats.ChartCount;
    Chart_Citizens.MaxLength  := fPlayers[HumanId].Stats.ChartCount;
    Chart_Houses.MaxLength    := fPlayers[HumanId].Stats.ChartCount;
    Chart_Wares.MaxLength     := fPlayers[HumanId].Stats.ChartCount;

    Chart_Army.MaxTime      := fGame.GameTickCount div 10;
    Chart_Citizens.MaxTime  := fGame.GameTickCount div 10;
    Chart_Houses.MaxTime    := fGame.GameTickCount div 10;
    Chart_Wares.MaxTime     := fGame.GameTickCount div 10;

    //Army
    TempGraphCount := 0; //Reset
    for I := 0 to fPlayers.Count - 1 do
    with fPlayers[I] do
      if PlayerType = pt_Computer then
        AddToTempGraph(FlagColor, Stats.ChartArmy)
      else
        Chart_Army.AddLine(GetFormattedPlayerName, FlagColor, Stats.ChartArmy);

    for I := 0 to TempGraphCount - 1 do
      Chart_Army.AddLine(Format(fTextLibrary[TX_PLAYER_X], [I+1]), TempGraphs[I].Color, TempGraphs[I].G);

    //Citizens
    TempGraphCount := 0; //Reset
    for I := 0 to fPlayers.Count - 1 do
    with fPlayers[I] do
      if PlayerType = pt_Computer then
        AddToTempGraph(FlagColor, Stats.ChartCitizens)
      else
      begin
        Chart_Citizens.AddLine(GetFormattedPlayerName, FlagColor, Stats.ChartCitizens);
        //Recruits aren't that important, but if we want to include them they should be a separate graph
        //Chart_Citizens.AddAltLine(Stats.ChartRecruits);
      end;

    for I := 0 to TempGraphCount - 1 do
      Chart_Citizens.AddLine(Format(fTextLibrary[TX_PLAYER_X], [I+1]), TempGraphs[I].Color, TempGraphs[I].G);

    //Houses
    TempGraphCount := 0; //Reset
    for I := 0 to fPlayers.Count - 1 do
    with fPlayers[I] do
      if PlayerType = pt_Computer then
        AddToTempGraph(FlagColor, Stats.ChartHouses)
      else
        Chart_Houses.AddLine(GetFormattedPlayerName, FlagColor, Stats.ChartHouses);

    for I := 0 to TempGraphCount - 1 do
      Chart_Houses.AddLine(Format(fTextLibrary[TX_PLAYER_X], [I+1]), TempGraphs[I].Color, TempGraphs[I].G);

    //Wares
    for R := WARE_MIN to WARE_MAX do
    begin
      G := fPlayers[HumanId].Stats.ChartWares[R];
      for I := 0 to High(G) do
        if G[I] <> 0 then
        begin
          Chart_Wares.AddLine(fResource.Wares[R].Title, ResourceColor[R] or $FF000000, G);
          Break;
        end;
    end;

    Button_ResultsHouses.Enabled := (fGame.MissionMode = mm_Normal);
    Button_ResultsCitizens.Enabled := (fGame.MissionMode = mm_Normal);
    Button_ResultsWares.Enabled := (fGame.MissionMode = mm_Normal);
    GraphToggle(Button_ResultsArmy);
  end;
end;


procedure TKMGUIMenuResultsSP.GraphToggle(Sender: TObject);
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


procedure TKMGUIMenuResultsSP.Show(aMsg: TGameResultMsg);
begin
  fGameResultMsg := aMsg;

  //Remember which map we played so we could restart it
  fRepeatGameName := fGame.GameName;
  fRepeatMission := fGame.MissionFile;
  fRepeatSave := fGame.SaveFile;
  fRepeatCampName := fGame.CampaignName;
  fRepeatCampMap := fGame.CampaignMap;
  fRepeatLocation := fGame.PlayerLoc;
  fRepeatColor := fGame.PlayerColor;

  Refresh;
  Panel_Results.Show;
end;


procedure TKMGUIMenuResultsSP.Create_Results(aParent: TKMPanel);
const StatText: array [1..9] of Word = (
    TX_RESULTS_UNITS_LOST,      TX_RESULTS_UNITS_DEFEATED,  TX_RESULTS_HOUSES_LOST,
    TX_RESULTS_HOUSES_DESTROYED,TX_RESULTS_HOUSES_BUILT,    TX_RESULTS_UNITS_TRAINED,
    TX_RESULTS_WEAPONS_MADE,    TX_RESULTS_SOLDIERS_TRAINED,TX_RESULTS_MISSION_TIME);
var I, Adv: Integer;
begin
  Panel_Results := TKMPanel.Create(aParent,0,0,aParent.Width, aParent.Height);
  Panel_Results.Stretch;
    //Background image
    with TKMImage.Create(Panel_Results,0,0,aParent.Width, aParent.Height,7,rxGuiMain) do
    begin
      ImageStretch;
      Center;
    end;
    //Fade to black by 62.5%
    with TKMShape.Create(Panel_Results,0,0,aParent.Width, aParent.Height) do
    begin
      Center;
      FillColor := $A0000000;
    end;

    Label_Results := TKMLabel.Create(Panel_Results,62,140,900,20,NO_TEXT,fnt_Metal,taCenter);
    Label_Results.Anchors := [akLeft];

    Panel_Stats := TKMPanel.Create(Panel_Results, 30, 216, 360, 354);
    Panel_Stats.Anchors := [akLeft];

      //Backplate for column results
      with TKMImage.Create(Panel_Stats, 0, 0, 360, 354, 3, rxGuiMain) do
      begin
        ImageStretch;
        Center;
      end;

      Adv := 0;
      for I := 1 to 9 do
      begin
        inc(Adv, 25);
        if I in [3,6,7] then inc(Adv, 15);
        if I = 9 then inc(Adv, 45); //Last one goes right at the bottom of the scroll
        TKMLabel.Create(Panel_Stats,20,Adv,240,20,fTextLibrary[StatText[I]],fnt_Metal,taLeft);
        Label_Stat[I] := TKMLabel.Create(Panel_Stats,260,Adv,80,20,'00',fnt_Metal,taRight);
      end;

    if DISPLAY_CHARTS_RESULT then
    begin
      Panel_StatsCharts := TKMPanel.Create(Panel_Results, 410, 170, 610, 420);
      Panel_StatsCharts.Anchors := [akLeft];

      Button_ResultsArmy := TKMButtonFlat.Create(Panel_StatsCharts, 40, 0, 208, 20, 53, rxGui);
      Button_ResultsArmy.TexOffsetX := -91;
      Button_ResultsArmy.TexOffsetY := 7;
      Button_ResultsArmy.Anchors := [akLeft];
      Button_ResultsArmy.Caption := fTextLibrary[TX_GRAPH_ARMY];
      Button_ResultsArmy.CapOffsetY := -11;
      Button_ResultsArmy.OnClick := GraphToggle;

      Button_ResultsCitizens := TKMButtonFlat.Create(Panel_StatsCharts, 40, 22, 208, 20, 588, rxGui);
      Button_ResultsCitizens.TexOffsetX := -92;
      Button_ResultsCitizens.TexOffsetY := 6;
      Button_ResultsCitizens.Anchors := [akLeft];
      Button_ResultsCitizens.Caption := fTextLibrary[TX_GRAPH_CITIZENS];
      Button_ResultsCitizens.CapOffsetY := -11;
      Button_ResultsCitizens.OnClick := GraphToggle;

      Button_ResultsHouses := TKMButtonFlat.Create(Panel_StatsCharts, 252, 0, 208, 20, 587, rxGui);
      Button_ResultsHouses.TexOffsetX := -93;
      Button_ResultsHouses.TexOffsetY := 6;
      Button_ResultsHouses.Anchors := [akLeft];
      Button_ResultsHouses.Caption := fTextLibrary[TX_GRAPH_HOUSES];
      Button_ResultsHouses.CapOffsetY := -11;
      Button_ResultsHouses.OnClick := GraphToggle;

      Button_ResultsWares := TKMButtonFlat.Create(Panel_StatsCharts, 252, 22, 208, 20, 360, rxGui);
      Button_ResultsWares.TexOffsetX := -93;
      Button_ResultsWares.TexOffsetY := 6;
      Button_ResultsWares.Anchors := [akLeft];
      Button_ResultsWares.Caption := fTextLibrary[TX_GRAPH_RESOURCES];
      Button_ResultsWares.CapOffsetY := -11;
      Button_ResultsWares.OnClick := GraphToggle;

      Chart_Army := TKMChart.Create(Panel_StatsCharts, 0, 46, 610, 374);
      Chart_Army.Caption := fTextLibrary[TX_GRAPH_ARMY];
      Chart_Army.Anchors := [akLeft];

      Chart_Citizens := TKMChart.Create(Panel_StatsCharts, 0, 46, 610, 374);
      Chart_Citizens.Caption := fTextLibrary[TX_GRAPH_CITIZENS];
      Chart_Citizens.Anchors := [akLeft];

      Chart_Houses := TKMChart.Create(Panel_StatsCharts, 0, 46, 610, 374);
      Chart_Houses.Caption := fTextLibrary[TX_GRAPH_HOUSES];
      Chart_Houses.Anchors := [akLeft];

      Chart_Wares := TKMChart.Create(Panel_StatsCharts, 0, 46, 610, 374);
      Chart_Wares.Caption := fTextLibrary[TX_GRAPH_TITLE_RESOURCES];
      Chart_Wares.Anchors := [akLeft];
    end;

    Button_ResultsBack := TKMButton.Create(Panel_Results,30,610,220,30,fTextLibrary[TX_MENU_BACK],bsMenu);
    Button_ResultsBack.Anchors := [akLeft];
    Button_ResultsBack.OnClick := BackClick;
    Button_ResultsRepeat := TKMButton.Create(Panel_Results,270,610,220,30,fTextLibrary[TX_MENU_MISSION_REPEAT],bsMenu);
    Button_ResultsRepeat.Anchors := [akLeft];
    Button_ResultsRepeat.OnClick := RepeatClick;
    Button_ResultsContinue := TKMButton.Create(Panel_Results,510,610,220,30,fTextLibrary[TX_MENU_MISSION_NEXT],bsMenu);
    Button_ResultsContinue.Anchors := [akLeft];
    Button_ResultsContinue.OnClick := ContinueClick;
end;


procedure TKMGUIMenuResultsSP.BackClick(Sender: TObject);
begin
  //todo:
  //Depending on where we were created we need to return to a different place
  //Campaign game end -> ResultsSP -> Main menu
  //Singleplayer game end -> ResultsSP -> Singleplayer
  //Replay end -> ResultsSP -> Replays

  if fGameResultMsg <> gr_ReplayEnd then
    fOnPageChange(Self, gpMainMenu, '')
  else
    fOnPageChange(Self, gpReplays, '');
end;


procedure TKMGUIMenuResultsSP.ContinueClick(Sender: TObject);
begin
  fOnPageChange(Self, gpCampaign, '');
end;


procedure TKMGUIMenuResultsSP.RepeatClick(Sender: TObject);
begin
  //Means replay last map
  fGameApp.NewRestartLast(fRepeatGameName, fRepeatMission, fRepeatSave, fRepeatCampName, fRepeatCampMap, fRepeatLocation, fRepeatColor);
end;


end.
