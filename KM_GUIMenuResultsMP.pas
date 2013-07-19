unit KM_GUIMenuResultsMP;
{$I KaM_Remake.inc}
interface
uses
  Controls, Math, StrUtils, SysUtils,
  KM_CommonTypes, KM_Controls, KM_Defaults, KM_Pics,
  KM_InterfaceDefaults, KM_ResWares;


type
  TKMGUIMenuResultsMP = class
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class

    fGameResultMsg: TGameResultMsg; //So we know where to go after results screen
    fPlayersVisible: array [0 .. MAX_PLAYERS - 1] of Boolean; //Remember visible players when toggling wares
    fEnabledPlayers: Integer;

    procedure BackClick(Sender: TObject);
    procedure Create_ResultsMP(aParent: TKMPanel);
    procedure CreateBars(aParent: TKMPanel);
    procedure CreateChartWares(aParent: TKMPanel);

    procedure TabChange(Sender: TObject);
    procedure WareChange(Sender: TObject);
    function GetChartWares(aPlayer: TPlayerIndex; aWare: TWareType): TKMCardinalArray;
    procedure Refresh;
    procedure RefreshBars;
    procedure RefreshCharts;
    procedure RefreshChartWares;
  protected
    Panel_ResultsMP: TKMPanel;
      Button_MPResultsBars,
      Button_MPResultsArmy,
      Button_MPResultsEconomy,
      Button_MPResultsWares: TKMButtonFlat;
      Label_ResultsMP: TKMLabel;
      Panel_Bars: TKMPanel;
        Panel_BarsUpper, Panel_BarsLower: TKMPanel;
          Label_ResultsPlayerName1, Label_ResultsPlayerName2: array [0 .. MAX_PLAYERS - 1] of TKMLabel;
          Bar_Results: array [0 .. MAX_PLAYERS - 1, 0 .. 9] of TKMPercentBar;
          Image_ResultsRosette: array [0 .. MAX_PLAYERS - 1, 0 .. 9] of TKMImage;
      Panel_ChartsMP: TKMPanel;
        Chart_MPArmy: TKMChart;
        Chart_MPCitizens: TKMChart;
        Chart_MPHouses: TKMChart;
      Panel_ChartsWares: TKMPanel;
        Columnbox_Wares: TKMColumnBox;
        Chart_MPWares: array [TWareType] of TKMChart; //One for each kind
        Label_NoWareData: TKMLabel;
      Button_ResultsMPBack: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);

    procedure Show(aMsg: TGameResultMsg);
  end;


implementation
uses KM_ResTexts, KM_Game, KM_PlayersCollection, KM_Utils, KM_Resource, KM_ResFonts, KM_RenderUI;


const
  PANES_TOP = 185;
  BAR_ROW_HEIGHT = 22;


{ TKMGUIMenuResultsMP }
constructor TKMGUIMenuResultsMP.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  Create_ResultsMP(aParent);
end;


procedure TKMGUIMenuResultsMP.CreateBars(aParent: TKMPanel);
const
  BarStep = 150;
  BarWidth = BarStep - 10;
  BarHalf = BarWidth div 2;
  Columns1: array[0..4] of Integer = (TX_RESULTS_MP_CITIZENS_TRAINED, TX_RESULTS_MP_CITIZENS_LOST,
                                     TX_RESULTS_MP_SOLDIERS_EQUIPPED, TX_RESULTS_MP_SOLDIERS_LOST,
                                     TX_RESULTS_MP_SOLDIERS_DEFEATED);
  Columns2: array[0..4] of Integer = (TX_RESULTS_MP_BUILDINGS_CONSTRUCTED, TX_RESULTS_MP_BUILDINGS_LOST,
                                     TX_RESULTS_MP_BUILDINGS_DESTROYED,
                                     TX_RESULTS_MP_WARES_PRODUCED, TX_RESULTS_MP_WEAPONS_PRODUCED);
var
  I,K: Integer;
begin
  Panel_Bars := TKMPanel.Create(aParent, 62, PANES_TOP, 900, 435);
  Panel_Bars.Anchors := [];

    //Composed of two sections each on own Panel to position them vertically according to player count

    Panel_BarsUpper := TKMPanel.Create(Panel_Bars, 0, 0, 900, 215);
    Panel_BarsUpper.Anchors := [];

      for I := 0 to MAX_PLAYERS - 1 do
        Label_ResultsPlayerName1[I] := TKMLabel.Create(Panel_BarsUpper, 0, 38+I*BAR_ROW_HEIGHT, 150, 20, '', fnt_Metal, taLeft);

      for K := 0 to 4 do
      begin
        with TKMLabel.Create(Panel_BarsUpper, 160 + BarStep*K, 0, BarWidth+6, 40, gResTexts[Columns1[K]], fnt_Metal, taCenter) do
          AutoWrap := True;
        for I:=0 to MAX_PLAYERS - 1 do
        begin
          Bar_Results[I,K] := TKMPercentBar.Create(Panel_BarsUpper, 160 + K*BarStep, 35+I*BAR_ROW_HEIGHT, BarWidth, 20, fnt_Grey);
          Bar_Results[I,K].TextYOffset := -3;
          Image_ResultsRosette[I,K] := TKMImage.Create(Panel_BarsUpper, 164 + K*BarStep, 38+I*BAR_ROW_HEIGHT, 16, 16, 8, rxGuiMain);
        end;
      end;

    Panel_BarsLower := TKMPanel.Create(Panel_Bars, 0, 220, 900, 180);
    Panel_BarsLower.Anchors := [];

      for I := 0 to MAX_PLAYERS - 1 do
        Label_ResultsPlayerName2[I] := TKMLabel.Create(Panel_BarsLower, 0, 38+I*BAR_ROW_HEIGHT, 150, 20, '', fnt_Metal, taLeft);

      for K := 0 to 4 do
      begin
        with TKMLabel.Create(Panel_BarsLower, 160 + BarStep*K, 0, BarWidth+6, 40, gResTexts[Columns2[K]], fnt_Metal, taCenter) do
          AutoWrap := True;
        for I := 0 to MAX_PLAYERS - 1 do
        begin
          Bar_Results[I,K+5] := TKMPercentBar.Create(Panel_BarsLower, 160 + K*BarStep, 35+I*BAR_ROW_HEIGHT, BarWidth, 20, fnt_Grey);
          Bar_Results[I,K+5].TextYOffset := -3;
          Image_ResultsRosette[I,K+5] := TKMImage.Create(Panel_BarsLower, 164 + K*BarStep, 38+I*BAR_ROW_HEIGHT, 16, 16, 8, rxGuiMain);
        end;
      end;
end;


procedure TKMGUIMenuResultsMP.CreateChartWares(aParent: TKMPanel);
var
  I: TWareType;
begin
  Panel_ChartsWares := TKMPanel.Create(aParent, 62, PANES_TOP, 900, 435);
  Panel_ChartsWares.Anchors := [];

    Columnbox_Wares := TKMColumnBox.Create(Panel_ChartsWares, 0, 0, 140, 435, fnt_Game, bsMenu);
    Columnbox_Wares.SetColumns(fnt_Game, ['', ''], [0, 20]);
    Columnbox_Wares.ShowHeader := False;
    Columnbox_Wares.ShowLines := False;
    Columnbox_Wares.OnChange := WareChange;

    for I := Low(TWareType) to High(TWareType) do
    begin
      Chart_MPWares[I] := TKMChart.Create(Panel_ChartsWares, 140, 0, 900-140, 435);
      Chart_MPWares[I].Caption := gResTexts[TX_GRAPH_TITLE_RESOURCES];
      Chart_MPWares[I].Font := fnt_Metal; //fnt_Outline doesn't work because player names blend badly with yellow
      Chart_MPWares[I].Hide;
    end;

    Label_NoWareData := TKMLabel.Create(Panel_ChartsWares, 450, 215, gResTexts[TX_GRAPH_NO_DATA], fnt_Metal, taCenter);
end;


procedure TKMGUIMenuResultsMP.TabChange(Sender: TObject);
begin
  Button_MPResultsBars.Down := Sender = Button_MPResultsBars;
  Button_MPResultsArmy.Down := Sender = Button_MPResultsArmy;
  Button_MPResultsEconomy.Down := Sender = Button_MPResultsEconomy;
  Button_MPResultsWares.Down := Sender = Button_MPResultsWares;

  Panel_Bars.Visible        := (Sender = Button_MPResultsBars);

  Panel_ChartsMP.Visible    :=(Sender = Button_MPResultsArmy)
                            or (Sender = Button_MPResultsEconomy)
                            or (Sender = Button_MPResultsWares);
  Chart_MPArmy.Visible      := Sender = Button_MPResultsArmy;
  Chart_MPCitizens.Visible  := Sender = Button_MPResultsEconomy;
  Chart_MPHouses.Visible    := Sender = Button_MPResultsEconomy;

  Panel_ChartsWares.Visible := Sender = Button_MPResultsWares;

  if Sender = Button_MPResultsWares then
    WareChange(nil);
end;


procedure TKMGUIMenuResultsMP.WareChange(Sender: TObject);
var
  K: Integer;
  I, R: TWareType;
begin
  if Columnbox_Wares.ItemIndex = -1 then
  begin
    Label_NoWareData.Show;
    Exit;
  end;

  Label_NoWareData.Hide;

  R := TWareType(Columnbox_Wares.Rows[Columnbox_Wares.ItemIndex].Tag);

  //Find and hide old chart
  for I := Low(TWareType) to High(TWareType) do
  begin
    //Remember which lines were visible
    if Chart_MPWares[I].Visible then
    for K := 0 to Chart_MPWares[I].LineCount - 1 do
      fPlayersVisible[Chart_MPWares[I].Lines[K].Tag] := Chart_MPWares[I].Lines[K].Visible;

    Chart_MPWares[I].Visible := False;
  end;

  Chart_MPWares[R].Visible := True;

  //Restore previously visible lines
  for K := 0 to Chart_MPWares[R].LineCount - 1 do
    Chart_MPWares[R].SetLineVisible(K, fPlayersVisible[Chart_MPWares[R].Lines[K].Tag]);
end;


procedure TKMGUIMenuResultsMP.Refresh;
var
  I: Integer;
begin
  case fGameResultMsg of
    gr_Win:       Label_ResultsMP.Caption := gResTexts[TX_MENU_MISSION_VICTORY];
    gr_Defeat:    Label_ResultsMP.Caption := gResTexts[TX_MENU_MISSION_DEFEAT];
    gr_Cancel:    Label_ResultsMP.Caption := gResTexts[TX_MENU_MISSION_CANCELED];
    gr_ReplayEnd: Label_ResultsMP.Caption := gResTexts[TX_MENU_REPLAY_ENDED];
    else          Label_ResultsMP.Caption := NO_TEXT;
  end;
  //Append mission name and time after the result message
  Label_ResultsMP.Caption := Label_ResultsMP.Caption + ' - ' + fGame.GameName + ' - ' + TimeToString(fGame.MissionTime);

  //Get player count to compact their data output
  fEnabledPlayers := 0;
  for I := 0 to gPlayers.Count - 1 do
    if gPlayers[I].Enabled then
      Inc(fEnabledPlayers);

  RefreshBars;
  RefreshCharts;
  RefreshChartWares;

  Button_MPResultsWares.Enabled := (fGame.MissionMode = mm_Normal);
  Button_MPResultsEconomy.Enabled := (fGame.MissionMode = mm_Normal);

  //Back button has different captions depending on where it returns us to
  if fGameResultMsg <> gr_ReplayEnd then
    Button_ResultsMPBack.Caption := gResTexts[TX_RESULTS_BACK_MP]
  else
    Button_ResultsMPBack.Caption := gResTexts[TX_RESULTS_BACK_REPLAYS];

  //Show first tab
  TabChange(Button_MPResultsBars);
end;


procedure TKMGUIMenuResultsMP.RefreshBars;

  procedure SetPlayerControls(aPlayer: Integer; aEnabled: Boolean);
  var I: Integer;
  begin
    Label_ResultsPlayerName1[aPlayer].Visible := aEnabled;
    Label_ResultsPlayerName2[aPlayer].Visible := aEnabled;
    for I := 0 to 9 do
    begin
      Bar_Results[aPlayer,I].Visible := aEnabled;
      Image_ResultsRosette[aPlayer,I].Visible := aEnabled;
    end;
  end;

var
  I,K,Index: Integer;
  UnitsMax, HousesMax, WaresMax, WeaponsMax, MaxValue: Integer;
  Bests: array [0..9] of Cardinal;
  Totals: array [0..9] of Cardinal;
begin
  //Update visibility depending on players count (note, players may be sparsed)
  for I := 0 to MAX_PLAYERS - 1 do
    SetPlayerControls(I, False); //Disable them all to start
  Index := 0;
  for I := 0 to gPlayers.Count - 1 do
    if gPlayers[I].Enabled then
    begin
      SetPlayerControls(Index, True); //Enable used ones
      Label_ResultsPlayerName1[Index].Caption   := gPlayers[I].PlayerName;
      Label_ResultsPlayerName1[Index].FontColor := FlagColorToTextColor(gPlayers[I].FlagColor);
      Label_ResultsPlayerName2[Index].Caption   := gPlayers[I].PlayerName;
      Label_ResultsPlayerName2[Index].FontColor := FlagColorToTextColor(gPlayers[I].FlagColor);
      Inc(Index);
    end;

  //Update positioning
  Panel_BarsUpper.Height := 40 + fEnabledPlayers * BAR_ROW_HEIGHT;
  Panel_BarsLower.Height := 40 + fEnabledPlayers * BAR_ROW_HEIGHT;

  //Second panel does not move from the middle of the screen: results always go above and below the middle
  Panel_BarsUpper.Top := Panel_Bars.Height div 2 - Panel_BarsUpper.Height - 5;
  Panel_BarsLower.Top := Panel_Bars.Height div 2 + 5;

  //Calculate best scores
  FillChar(Bests, SizeOf(Bests), #0);
  //These are a special case: Less is better so we initialize them high
  Bests[1] := High(Cardinal);
  Bests[3] := High(Cardinal);
  Bests[6] := High(Cardinal);
  FillChar(Totals, SizeOf(Totals), #0);

  //Calculate bests for each "section"
  for I := 0 to gPlayers.Count - 1 do
    if gPlayers[I].Enabled then
      with gPlayers[I].Stats do
      begin
        if Bests[0] < GetCitizensTrained then Bests[0] := GetCitizensTrained;
        if Bests[1] > GetCitizensLost    then Bests[1] := GetCitizensLost;
        if Bests[2] < GetWarriorsTrained then Bests[2] := GetWarriorsTrained;
        if Bests[3] > GetWarriorsLost    then Bests[3] := GetWarriorsLost;
        if Bests[4] < GetWarriorsKilled  then Bests[4] := GetWarriorsKilled;
        if Bests[5] < GetHousesBuilt     then Bests[5] := GetHousesBuilt;
        if Bests[6] > GetHousesLost      then Bests[6] := GetHousesLost;
        if Bests[7] < GetHousesDestroyed then Bests[7] := GetHousesDestroyed;
        if Bests[8] < GetCivilProduced   then Bests[8] := GetCivilProduced;
        if Bests[9] < GetWeaponsProduced then Bests[9] := GetWeaponsProduced;

        //If Totals is 0 the category skipped and does not have "Best" icon on it
        Inc(Totals[0], GetCitizensTrained);
        Inc(Totals[1], GetCitizensLost);
        Inc(Totals[2], GetWarriorsTrained);
        Inc(Totals[3], GetWarriorsLost);
        Inc(Totals[4], GetWarriorsKilled);
        Inc(Totals[5], GetHousesBuilt);
        Inc(Totals[6], GetHousesLost);
        Inc(Totals[7], GetHousesDestroyed);
        Inc(Totals[8], GetCivilProduced);
        Inc(Totals[9], GetWeaponsProduced);
      end;

  //Fill in raw values
  Index := 0;
  for I := 0 to gPlayers.Count - 1 do
    if gPlayers[I].Enabled then
    begin

      with gPlayers[I].Stats do
      begin
        //Living things
        Bar_Results[Index,0].Tag := GetCitizensTrained;
        Bar_Results[Index,1].Tag := GetCitizensLost;
        Bar_Results[Index,2].Tag := GetWarriorsTrained;
        Bar_Results[Index,3].Tag := GetWarriorsLost;
        Bar_Results[Index,4].Tag := GetWarriorsKilled;
        Image_ResultsRosette[Index,0].Visible := (GetCitizensTrained >= Bests[0]) and (Totals[0] > 0);
        Image_ResultsRosette[Index,1].Visible := (GetCitizensLost    <= Bests[1]) and (Totals[1] > 0);
        Image_ResultsRosette[Index,2].Visible := (GetWarriorsTrained >= Bests[2]) and (Totals[2] > 0);
        Image_ResultsRosette[Index,3].Visible := (GetWarriorsLost    <= Bests[3]) and (Totals[3] > 0);
        Image_ResultsRosette[Index,4].Visible := (GetWarriorsKilled  >= Bests[4]) and (Totals[4] > 0);
        //Objects
        Bar_Results[Index,5].Tag := GetHousesBuilt;
        Bar_Results[Index,6].Tag := GetHousesLost;
        Bar_Results[Index,7].Tag := GetHousesDestroyed;
        Bar_Results[Index,8].Tag := GetCivilProduced;
        Bar_Results[Index,9].Tag := GetWeaponsProduced;
        Image_ResultsRosette[Index,5].Visible := (GetHousesBuilt     >= Bests[5]) and (Totals[5] > 0);
        Image_ResultsRosette[Index,6].Visible := (GetHousesLost      <= Bests[6]) and (Totals[6] > 0);
        Image_ResultsRosette[Index,7].Visible := (GetHousesDestroyed >= Bests[7]) and (Totals[7] > 0);
        Image_ResultsRosette[Index,8].Visible := (GetCivilProduced   >= Bests[8]) and (Totals[8] > 0);
        Image_ResultsRosette[Index,9].Visible := (GetWeaponsProduced >= Bests[9]) and (Totals[9] > 0);
      end;
      inc(Index);
    end;

  //Update percent bars for each category
  UnitsMax := 0;
  for K := 0 to 4 do for I := 0 to fEnabledPlayers - 1 do
    UnitsMax := Max(Bar_Results[I,K].Tag, UnitsMax);

  HousesMax := 0;
  for K := 5 to 7 do for I := 0 to fEnabledPlayers - 1 do
    HousesMax := Max(Bar_Results[I,K].Tag, HousesMax);

  WaresMax := 0;
  for I := 0 to fEnabledPlayers - 1 do
    WaresMax := Max(Bar_Results[I,8].Tag, WaresMax);

  WeaponsMax := 0;
  for I := 0 to fEnabledPlayers - 1 do
    WeaponsMax := Max(Bar_Results[I,9].Tag, WeaponsMax);

  //Knowing Max in each category we may fill bars properly
  for K := 0 to 9 do
  begin
    case K of
      0..4: MaxValue := UnitsMax;
      5..7: MaxValue := HousesMax;
      8:    MaxValue := WaresMax;
      else  MaxValue := WeaponsMax;
    end;
    for I := 0 to fEnabledPlayers - 1 do
    begin
      if MaxValue <> 0 then
        Bar_Results[I,K].Position := Bar_Results[I,K].Tag / MaxValue
      else
        Bar_Results[I,K].Position := 0;
      Bar_Results[I,K].Caption := IfThen(Bar_Results[I,K].Tag <> 0, IntToStr(Bar_Results[I,K].Tag), '-');
    end;
  end;
end;


procedure TKMGUIMenuResultsMP.RefreshCharts;
var
  I: Integer;
begin
  for I := 0 to MAX_PLAYERS - 1 do
    fPlayersVisible[I] := True;

  Chart_MPArmy.Clear;
  Chart_MPCitizens.Clear;
  Chart_MPHouses.Clear;

  Chart_MPArmy.MaxLength := 0;
  Chart_MPCitizens.MaxLength := 0;
  Chart_MPHouses.MaxLength := 0;
  Chart_MPArmy.MaxTime      := fGame.GameTickCount div 10;
  Chart_MPCitizens.MaxTime  := fGame.GameTickCount div 10;
  Chart_MPHouses.MaxTime    := fGame.GameTickCount div 10;

  for I := 0 to gPlayers.Count - 1 do
  with gPlayers[I] do
    if Enabled then
    begin
      Chart_MPArmy.MaxLength := Max(Chart_MPArmy.MaxLength, Stats.ChartCount);
      Chart_MPArmy.AddLine(PlayerName, FlagColor, Stats.ChartArmy);
    end;

  Chart_MPArmy.TrimToFirstVariation;

  for I := 0 to gPlayers.Count - 1 do
  with gPlayers[I] do
    if Enabled then
    begin
      Chart_MPCitizens.MaxLength := Max(Chart_MPCitizens.MaxLength, Stats.ChartCount);
      Chart_MPCitizens.AddLine(PlayerName, FlagColor, Stats.ChartCitizens);
    end;

  for I := 0 to gPlayers.Count - 1 do
  with gPlayers[I] do
    if Enabled then
    begin
      Chart_MPHouses.MaxLength := Max(Chart_MPHouses.MaxLength, Stats.ChartCount);
      Chart_MPHouses.AddLine(PlayerName, FlagColor, Stats.ChartHouses);
    end;
end;


procedure TKMGUIMenuResultsMP.RefreshChartWares;
const
  Wares: array [0..30] of TWareType = (
    wt_All,     wt_Warfare, wt_Food,
    wt_Trunk,   wt_Stone,   wt_Wood,        wt_IronOre,   wt_GoldOre,
    wt_Coal,    wt_Steel,   wt_Gold,        wt_Wine,      wt_Corn,
    wt_Bread,   wt_Flour,   wt_Leather,     wt_Sausages,  wt_Pig,
    wt_Skin,    wt_Shield,  wt_MetalShield, wt_Armor,     wt_MetalArmor,
    wt_Axe,     wt_Sword,   wt_Pike,        wt_Hallebard, wt_Bow,
    wt_Arbalet, wt_Horse,   wt_Fish);
var
  I,K,J: Integer;
  R: TWareType;
begin
  //Fill in chart values
  Columnbox_Wares.Clear;
  for I := Low(Wares) to High(Wares) do
  begin
    R := Wares[I];
    for K := 0 to gPlayers.Count - 1 do
    if gPlayers[K].Enabled
    and not gPlayers[K].Stats.ChartWaresEmpty(R) then
    begin
      Columnbox_Wares.AddItem(MakeListRow(['', fResource.Wares[R].Title],
                                          [$FFFFFFFF, $FFFFFFFF],
                                          [MakePic(rxGui, fResource.Wares[R].GUIIcon), MakePic(rxGui, 0)],
                                          Byte(R)));
      Break;
    end;
  end;

  for J := 0 to Columnbox_Wares.RowCount - 1 do
  begin
    R := TWareType(Columnbox_Wares.Rows[J].Tag);

    Chart_MPWares[R].Clear;
    Chart_MPWares[R].MaxLength := 0;
    Chart_MPWares[R].MaxTime := fGame.GameTickCount div 10;
    Chart_MPWares[R].Caption := gResTexts[TX_GRAPH_TITLE_RESOURCES] + ' - ' + fResource.Wares[R].Title;

    for I := 0 to gPlayers.Count - 1 do
    with gPlayers[I] do
    if Enabled then
    begin
      Chart_MPWares[R].MaxLength := Max(Chart_MPWares[R].MaxLength, Stats.ChartCount);
      //Do some postprocessing on stats (GDP, food value)
      Chart_MPWares[R].AddLine(PlayerName, FlagColor, GetChartWares(I, R), I);
    end;
  end;

  Columnbox_Wares.ItemIndex := 0;
  Columnbox_Wares.ItemHeight := Min(Columnbox_Wares.Height div 15, 20);
  WareChange(nil);
end;


procedure TKMGUIMenuResultsMP.Create_ResultsMP(aParent: TKMPanel);
begin
  Panel_ResultsMP := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_ResultsMP.Stretch;
    with TKMImage.Create(Panel_ResultsMP,0,0,aParent.Width, aParent.Height,7,rxGuiMain) do
    begin
      ImageStretch;
      Center;
    end;
    with TKMShape.Create(Panel_ResultsMP,0,0,aParent.Width, aParent.Height) do
    begin
      Center;
      FillColor := $A0000000;
    end;

    Label_ResultsMP := TKMLabel.Create(Panel_ResultsMP,62,125,900,20,NO_TEXT,fnt_Metal,taCenter);
    Label_ResultsMP.Anchors := [akLeft];

    Button_MPResultsBars := TKMButtonFlat.Create(Panel_ResultsMP, 160, 155, 176, 20, 8, rxGuiMain);
    Button_MPResultsBars.TexOffsetX := -78;
    Button_MPResultsBars.TexOffsetY := 6;
    Button_MPResultsBars.Anchors := [akLeft];
    Button_MPResultsBars.Caption := gResTexts[TX_RESULTS_STATISTICS];
    Button_MPResultsBars.CapOffsetY := -11;
    Button_MPResultsBars.OnClick := TabChange;

    Button_MPResultsArmy := TKMButtonFlat.Create(Panel_ResultsMP, 340, 155, 176, 20, 53, rxGui);
    Button_MPResultsArmy.TexOffsetX := -76;
    Button_MPResultsArmy.TexOffsetY := 6;
    Button_MPResultsArmy.Anchors := [akLeft];
    Button_MPResultsArmy.Caption := gResTexts[TX_GRAPH_ARMY];
    Button_MPResultsArmy.CapOffsetY := -11;
    Button_MPResultsArmy.OnClick := TabChange;

    Button_MPResultsEconomy := TKMButtonFlat.Create(Panel_ResultsMP, 520, 155, 176, 20, 589, rxGui);
    Button_MPResultsEconomy.TexOffsetX := -72;
    Button_MPResultsEconomy.TexOffsetY := 6;
    Button_MPResultsEconomy.Anchors := [akLeft];
    Button_MPResultsEconomy.Caption := gResTexts[TX_RESULTS_ECONOMY];
    Button_MPResultsEconomy.CapOffsetY := -11;
    Button_MPResultsEconomy.OnClick := TabChange;

    Button_MPResultsWares := TKMButtonFlat.Create(Panel_ResultsMP, 700, 155, 176, 20, 360, rxGui);
    Button_MPResultsWares.TexOffsetX := -77;
    Button_MPResultsWares.TexOffsetY := 6;
    Button_MPResultsWares.Anchors := [akLeft];
    Button_MPResultsWares.Caption := gResTexts[TX_GRAPH_RESOURCES];
    Button_MPResultsWares.CapOffsetY := -11;
    Button_MPResultsWares.OnClick := TabChange;

    CreateBars(Panel_ResultsMP);

    Panel_ChartsMP := TKMPanel.Create(Panel_ResultsMP, 0, PANES_TOP, 1024, 560);
    Panel_ChartsMP.Anchors := [akLeft];

      Chart_MPArmy := TKMChart.Create(Panel_ChartsMP, 62, 0, 900, 435);
      Chart_MPArmy.Caption := gResTexts[TX_GRAPH_ARMY];
      Chart_MPArmy.Anchors := [akLeft];

      Chart_MPCitizens := TKMChart.Create(Panel_ChartsMP, 62, 0, 900, 200);
      Chart_MPCitizens.Caption := gResTexts[TX_GRAPH_CITIZENS];
      Chart_MPCitizens.Anchors := [akLeft];

      Chart_MPHouses := TKMChart.Create(Panel_ChartsMP, 62, 235, 900, 200);
      Chart_MPHouses.Caption := gResTexts[TX_GRAPH_HOUSES];
      Chart_MPHouses.Anchors := [akLeft];

    CreateChartWares(Panel_ResultsMP);

    Button_ResultsMPBack := TKMButton.Create(Panel_ResultsMP, 100, 630, 220, 30, NO_TEXT, bsMenu);
    Button_ResultsMPBack.Anchors := [akLeft];
    Button_ResultsMPBack.OnClick := BackClick;
end;


function TKMGUIMenuResultsMP.GetChartWares(aPlayer: TPlayerIndex; aWare: TWareType): TKMCardinalArray;
var
  RT: TWareType;
  I: Integer;
begin
  with gPlayers[aPlayer].Stats do
  case aWare of
    WARE_MIN..WARE_MAX: Result := ChartWares[aWare];
    wt_All:             begin
                          SetLength(Result, ChartCount);
                          for I := 0 to ChartCount - 1 do
                            Result[I] := 0;
                          for RT := WARE_MIN to WARE_MAX do
                          for I := 0 to ChartCount - 1 do
                            Result[I] := Result[I] + ChartWares[RT][I];
                          //@Lewin: We could show GDP here if we divide by ProductionRate,
                          //so that easy to make Stones do not steal other wares value
                          //@Krom: That sounds worth testing. It might be confusing though.
                        end;
    wt_Warfare:         begin
                          SetLength(Result, ChartCount);
                          for I := 0 to ChartCount - 1 do
                            Result[I] := 0;
                          for RT := WARFARE_MIN to WARFARE_MAX do
                          for I := 0 to ChartCount - 1 do
                            Result[I] := Result[I] + ChartWares[RT][I];
                        end;
    wt_Food:            begin
                          //Compute food value according to food types condition restore
                          SetLength(Result, ChartCount);
                          for I := 0 to ChartCount - 1 do
                            Result[I] := Round(ChartWares[wt_Bread][I] * BREAD_RESTORE) +
                                         Round(ChartWares[wt_Sausages][I] * SAUSAGE_RESTORE) +
                                         Round(ChartWares[wt_Wine][I] * WINE_RESTORE) +
                                         Round(ChartWares[wt_Fish][I] * FISH_RESTORE);
                        end;
  end;
end;


procedure TKMGUIMenuResultsMP.Show(aMsg: TGameResultMsg);
begin
  fGameResultMsg := aMsg;

  Refresh;
  Panel_ResultsMP.Show;
end;


procedure TKMGUIMenuResultsMP.BackClick(Sender: TObject);
begin
  //Depending on where we were created we need to return to a different place
  //Multiplayer game end -> ResultsMP -> Multiplayer
  //Multiplayer replay end -> ResultsMP -> Replays

  if fGameResultMsg <> gr_ReplayEnd then
    fOnPageChange(gpMultiplayer, '')
  else
    fOnPageChange(gpReplays, '');
end;


end.
