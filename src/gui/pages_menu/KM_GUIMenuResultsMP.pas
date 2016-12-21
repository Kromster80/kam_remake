unit KM_GUIMenuResultsMP;
{$I KaM_Remake.inc}
interface
uses
  Controls, Math, StrUtils, SysUtils,
  KM_CommonTypes, KM_Controls, KM_Defaults, KM_Pics,
  KM_InterfaceDefaults, KM_ResWares;


type

  // Army chart types (enum)
  TChartArmyType = (cat_All, cat_ArmyPower,
    cat_Militia,      cat_AxeFighter,   cat_Swordsman,     cat_Bowman,
    cat_Arbaletman,   cat_Pikeman,      cat_Hallebardman,  cat_HorseScout,
    cat_Cavalry,      cat_Barbarian,
    cat_Peasant,      cat_Slingshot,    cat_MetalBarbarian, cat_Horseman);

  // Army chart kind (Instantaneous or Total)
  TChartArmyKind = (cak_Instantaneous, cak_Total);

  // Chart army type class
  TKMChartArmyType = class
  private
    fType: TChartArmyType;
    fUnitType: TUnitType;
    function GetUnitType: TUnitType;
    function GetGUIName: UnicodeString;
    function GetGUIIcon: Word;
  public
    constructor Create(aType: TChartArmyType);
    property UnitType: TUnitType read GetUnitType;
    property GUIName: UnicodeString read GetGUIName;
    property GUIIcon: Word read GetGUIIcon;
    function HasUnitType: Boolean;
  end;

  TKMChartArmyMP = class
  private
    fType: TKMChartArmyType;
    fKind: TChartArmyKind;
    fChart: TKMChart;
    function GetArmyPowerChartData(aPlayer: TKMHandIndex): TKMCardinalArray;
  public
    constructor Create(aType: TChartArmyType; aKind: TChartArmyKind; aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
    destructor Destroy; override;
    procedure AddLine(aPlayer: TKMHandIndex; aTitle: UnicodeString; aColor: Cardinal);
    function IsEmpty(aPlayer: TKMHandIndex): Boolean;
    property Chart: TKMChart read fChart;
    property ChartType: TKMChartArmyType read fType;
  end;

  TKMMenuResultsMP = class
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class

    fGameResultMsg: TGameResultMsg; //So we know where to go after results screen
    fPlayersVisibleWares: array [0 .. MAX_HANDS - 1] of Boolean;  //Remember visible players when toggling wares
    fPlayersVisibleArmy: array [0 .. MAX_HANDS - 1] of Boolean;   //Remember visible players when toggling warriors
    fEnabledPlayers: Integer;

    procedure BackClick(Sender: TObject);
    procedure Create_ResultsMP(aParent: TKMPanel);
    procedure CreateBars(aParent: TKMPanel);
    procedure CreateChartWares(aParent: TKMPanel);
    procedure CreateChartArmy(aParent: TKMPanel);

    procedure TabChange(Sender: TObject);
    procedure WareChange(Sender: TObject);
    procedure ArmyChange(Sender: TObject);
    function GetChartWares(aPlayer: TKMHandIndex; aWare: TWareType): TKMCardinalArray;
    procedure Refresh;
    procedure RefreshBars;
    procedure RefreshCharts;
    procedure RefreshChartWares;
    procedure RefreshChartArmy;
    procedure RadioArmyStyleChange(Sender: TObject);
  protected
    Panel_ResultsMP: TKMPanel;
      Button_MPResultsBars,
      Button_MPResultsArmy,
      Button_MPResultsEconomy,
      Button_MPResultsWares: TKMButtonFlat;
      Label_ResultsMP: TKMLabel;
      Panel_Bars: TKMPanel;
        Panel_BarsUpper, Panel_BarsLower: TKMPanel;
          Label_ResultsPlayerName1, Label_ResultsPlayerName2: array [0 .. MAX_LOBBY_PLAYERS - 1] of TKMLabel;
          Bar_Results: array [0 .. MAX_LOBBY_PLAYERS - 1, 0 .. 9] of TKMPercentBar;
          Image_ResultsRosette: array [0 .. MAX_LOBBY_PLAYERS - 1, 0 .. 9] of TKMImage;
      Panel_ChartsMP: TKMPanel;
        Chart_MPCitizens: TKMChart;
        Chart_MPHouses: TKMChart;
      Panel_ChartsWares: TKMPanel;
        Columnbox_Wares: TKMColumnBox;
        Chart_MPWares: array [TWareType] of TKMChart; //One for each kind
        Label_NoWareData: TKMLabel;
      Panel_ChartsArmy: TKMPanel;
        Columnbox_Army: TKMColumnBox;
        // Charts of in game warriors quantities
        Chart_MPArmy: array [TChartArmyType] of TKMChartArmyMP;      //One for each warrior type;
        // Charts of total army trained (includes initial army)
        Chart_MPArmyTotal: array [TChartArmyType] of TKMChartArmyMP; //One for each warrior type;
        Label_NoArmyData: TKMLabel;
        Radio_ChartArmyStyle: TKMRadioGroup;
      Button_ResultsMPBack: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);

    procedure Show(aMsg: TGameResultMsg);
  end;


implementation
uses
  KM_ResTexts, KM_Game, KM_HandsCollection, KM_Utils, KM_Resource, KM_ResFonts,
  KM_RenderUI, KM_Hand;


const
  PANES_TOP = 185;
  BAR_ROW_HEIGHT = 22;

  WARRIORS_POWER_RATES: array [WARRIOR_MIN..WARRIOR_MAX] of Byte = (
    2, 5, 9,        // ut_Militia, ut_AxeFighter, ut_Swordsman
    4, 7,           // ut_Bowman, ut_Arbaletman
    4, 7,           // ut_Pikeman, ut_Hallebardman
    6, 10,          // ut_HorseScout, ut_Cavalry
    10, 3, 3, 10, 5 // ut_Barbarian, ut_Peasant, ut_Slingshot, ut_MetalBarbarian, ut_Horseman
  );


{TKMChartArmyType}
constructor TKMChartArmyType.Create(aType: TChartArmyType);
begin
  fType := aType;
  case aType of
    cat_All:                    fUnitType := ut_Any;
    cat_Militia..cat_Horseman:  fUnitType := TUnitType(Ord(ut_Militia) + Ord(aType) - Ord(cat_Militia));
  end;
end;


function TKMChartArmyType.GetUnitType: TUnitType;
begin
  Assert(HasUnitType, 'ArmyPower has no UnitType match');
  Result := fUnitType;
end;


function TKMChartArmyType.HasUnitType: Boolean;
begin
  Result := fType <> cat_ArmyPower;
end;


function TKMChartArmyType.GetGUIName: UnicodeString;
begin
  if (HasUnitType) then
    Result := gRes.UnitDat.UnitsDat[UnitType].GUIName
  else
    Result := 'Army power'; //Todo translate
end;


function TKMChartArmyType.GetGUIIcon: Word;
begin
  if (HasUnitType) then
    Result := gRes.UnitDat.UnitsDat[UnitType].GUIIcon
  else
    Result := 53;
end;


{TKMChartArmy}
constructor TKMChartArmyMP.Create(aType: TChartArmyType; aKind: TChartArmyKind; aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
begin
  fType := TKMChartArmyType.Create(aType);
  fKind := aKind;
  fChart := TKMChart.Create(aParent, aLeft, aTop, aWidth, aHeight);
end;


destructor TKMChartArmyMP.Destroy;
begin
  FreeAndNil(fType);
  FreeAndNil(fChart);
  inherited;
end;


procedure TKMChartArmyMP.AddLine(aPlayer: TKMHandIndex; aTitle: UnicodeString; aColor: Cardinal);
var ChartData: TKMCardinalArray;
begin
  if (fType.HasUnitType) then begin
    if (fKind = cak_Instantaneous) then
      ChartData := gHands[aPlayer].Stats.ChartArmy[fType.UnitType]
    else
      ChartData := gHands[aPlayer].Stats.ChartArmyTotal[fType.UnitType];
  end else
    ChartData := GetArmyPowerChartData(aPlayer);
  Chart.AddLine(aTitle, aColor, ChartData, aPlayer);
end;


function TKMChartArmyMP.GetArmyPowerChartData(aPlayer: TKMHandIndex): TKMCardinalArray;
var WT: TUnitType;
    I, ChartCnt: Integer;
begin
  ChartCnt := gHands[aPlayer].Stats.ChartCount;
  //Create new array and fill it (otherwise we assign pointers and corrupt data)
  SetLength(Result, ChartCnt);
  for I := 0 to ChartCnt - 1 do
    Result[I] := 0;
  for WT := WARRIOR_MIN to WARRIOR_MAX do
    for I := 0 to ChartCnt - 1 do
      if (fKind = cak_Instantaneous) then
        Result[I] := Result[I] + gHands[aPlayer].Stats.ChartArmy[WT][I]*WARRIORS_POWER_RATES[WT]
      else
        Result[I] := Result[I] + gHands[aPlayer].Stats.ChartArmyTotal[WT][I]*WARRIORS_POWER_RATES[WT];
end;


function TKMChartArmyMP.IsEmpty(aPlayer: TKMHandIndex): Boolean;
begin
  if (fType.HasUnitType) then
    Result := gHands[aPlayer].Stats.ChartArmyEmpty(fType.UnitType)
  else
    Result := gHands[aPlayer].Stats.ChartArmyEmpty(ut_Any);
end;


{ TKMGUIMenuResultsMP }
constructor TKMMenuResultsMP.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  Create_ResultsMP(aParent);
end;


procedure TKMMenuResultsMP.CreateBars(aParent: TKMPanel);
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

      for I := 0 to MAX_LOBBY_PLAYERS - 1 do
        Label_ResultsPlayerName1[I] := TKMLabel.Create(Panel_BarsUpper, 0, 58+I*BAR_ROW_HEIGHT, 150, 20, '', fnt_Metal, taLeft);

      for K := 0 to 4 do
      begin
        with TKMLabel.Create(Panel_BarsUpper, 160 + BarStep*K, 0, BarWidth+6, 60, gResTexts[Columns1[K]], fnt_Metal, taCenter) do
          AutoWrap := True;
        for I:=0 to MAX_LOBBY_PLAYERS - 1 do
        begin
          Bar_Results[I,K] := TKMPercentBar.Create(Panel_BarsUpper, 160 + K*BarStep, 55+I*BAR_ROW_HEIGHT, BarWidth, 20, fnt_Grey);
          Bar_Results[I,K].TextYOffset := -3;
          Image_ResultsRosette[I,K] := TKMImage.Create(Panel_BarsUpper, 164 + K*BarStep, 58+I*BAR_ROW_HEIGHT, 16, 16, 8, rxGuiMain);
        end;
      end;

    Panel_BarsLower := TKMPanel.Create(Panel_Bars, 0, 220, 900, 180);
    Panel_BarsLower.Anchors := [];

      for I := 0 to MAX_LOBBY_PLAYERS - 1 do
        Label_ResultsPlayerName2[I] := TKMLabel.Create(Panel_BarsLower, 0, 38+I*BAR_ROW_HEIGHT, 150, 20, '', fnt_Metal, taLeft);

      for K := 0 to 4 do
      begin
        with TKMLabel.Create(Panel_BarsLower, 160 + BarStep*K, 0, BarWidth+6, 40, gResTexts[Columns2[K]], fnt_Metal, taCenter) do
          AutoWrap := True;
        for I := 0 to MAX_LOBBY_PLAYERS - 1 do
        begin
          Bar_Results[I,K+5] := TKMPercentBar.Create(Panel_BarsLower, 160 + K*BarStep, 35+I*BAR_ROW_HEIGHT, BarWidth, 20, fnt_Grey);
          Bar_Results[I,K+5].TextYOffset := -3;
          Image_ResultsRosette[I,K+5] := TKMImage.Create(Panel_BarsLower, 164 + K*BarStep, 38+I*BAR_ROW_HEIGHT, 16, 16, 8, rxGuiMain);
        end;
      end;
end;


procedure TKMMenuResultsMP.CreateChartWares(aParent: TKMPanel);
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


procedure TKMMenuResultsMP.CreateChartArmy(aParent: TKMPanel);
var
  WType: TChartArmyType;
begin
  Panel_ChartsArmy := TKMPanel.Create(aParent, 62, PANES_TOP, 900, 435);
  Panel_ChartsArmy.Anchors := [];

    Columnbox_Army := TKMColumnBox.Create(Panel_ChartsArmy, 0, 0, 140, 435, fnt_Game, bsMenu);
    Columnbox_Army.SetColumns(fnt_Game, ['', ''], [0, 30]);
    Columnbox_Army.ShowHeader := False;
    Columnbox_Army.ShowLines := False;
    Columnbox_Army.OnChange := ArmyChange;

    for WType := Low(TChartArmyType) to High(TChartArmyType) do
    begin
      Chart_MPArmy[WType] := TKMChartArmyMP.Create(WType, cak_Instantaneous, Panel_ChartsArmy, 140, 0, 760, 435);
      Chart_MPArmy[WType].Chart.Caption := gResTexts[TX_GRAPH_ARMY];
      Chart_MPArmy[WType].Chart.Font := fnt_Metal; //fnt_Outline doesn't work because player names blend badly with yellow
      Chart_MPArmy[WType].Chart.Hide;

      Chart_MPArmyTotal[WType] := TKMChartArmyMP.Create(WType, cak_Total, Panel_ChartsArmy, 140, 0, 760, 435);
      Chart_MPArmyTotal[WType].Chart.Caption := gResTexts[TX_GRAPH_ARMY];
      Chart_MPArmyTotal[WType].Chart.Font := fnt_Metal; //fnt_Outline doesn't work because player names blend badly with yellow
      Chart_MPArmyTotal[WType].Chart.Hide;
    end;

    Label_NoArmyData := TKMLabel.Create(Panel_ChartsArmy, 450, 215, gResTexts[TX_GRAPH_NO_DATA], fnt_Metal, taCenter);

    TKMLabel.Create(Panel_ChartsArmy, 755, 355, '', fnt_Metal, taLeft); // Todo translate

    Radio_ChartArmyStyle := TKMRadioGroup.Create(Panel_ChartsArmy,755,375,150,40,fnt_Grey);
    Radio_ChartArmyStyle.ItemIndex := 0;
    Radio_ChartArmyStyle.Add('Instantaneous'); // Todo translate
    Radio_ChartArmyStyle.Add('Total Equipped'); // Todo translate
    Radio_ChartArmyStyle.OnChange := RadioArmyStyleChange;
end;


procedure TKMMenuResultsMP.RadioArmyStyleChange(Sender: TObject);
begin
  ArmyChange(nil);
end;


procedure TKMMenuResultsMP.TabChange(Sender: TObject);
begin
  Button_MPResultsBars.Down := Sender = Button_MPResultsBars;
  Button_MPResultsArmy.Down := Sender = Button_MPResultsArmy;
  Button_MPResultsEconomy.Down := Sender = Button_MPResultsEconomy;
  Button_MPResultsWares.Down := Sender = Button_MPResultsWares;

  Panel_Bars.Visible        := (Sender = Button_MPResultsBars);

  Panel_ChartsMP.Visible    := (Sender = Button_MPResultsEconomy) or
                               (Sender = Button_MPResultsWares);
  Chart_MPCitizens.Visible  := Sender = Button_MPResultsEconomy;
  Chart_MPHouses.Visible    := Sender = Button_MPResultsEconomy;

  Panel_ChartsWares.Visible := Sender = Button_MPResultsWares;
  Panel_ChartsArmy.Visible := Sender = Button_MPResultsArmy;

  if Sender = Button_MPResultsWares then
    WareChange(nil);
  if Sender = Button_MPResultsArmy then
    ArmyChange(nil);
end;


procedure TKMMenuResultsMP.WareChange(Sender: TObject);
var
  K: Integer;
  I, R: TWareType;
begin
  if Columnbox_Wares.ItemIndex = -1 then
  begin
    Label_NoWareData.Show;
    Columnbox_Wares.Hide;
    for I := Low(TWareType) to High(TWareType) do
      Chart_MPWares[I].Hide;
    Exit;
  end;

  Label_NoWareData.Hide;
  Columnbox_Wares.Show;

  R := TWareType(Columnbox_Wares.Rows[Columnbox_Wares.ItemIndex].Tag);

  //Find and hide old chart
  for I := Low(TWareType) to High(TWareType) do
  begin
    //Remember which lines were visible
    if Chart_MPWares[I].Visible then
    for K := 0 to Chart_MPWares[I].LineCount - 1 do
      fPlayersVisibleWares[Chart_MPWares[I].Lines[K].Tag] := Chart_MPWares[I].Lines[K].Visible;

    Chart_MPWares[I].Visible := False;
  end;

  Chart_MPWares[R].Visible := True;

  //Restore previously visible lines
  for K := 0 to Chart_MPWares[R].LineCount - 1 do
    Chart_MPWares[R].SetLineVisible(K, fPlayersVisibleWares[Chart_MPWares[R].Lines[K].Tag]);
end;


procedure TKMMenuResultsMP.ArmyChange(Sender: TObject);
var
  K: Integer;
  W, WType: TChartArmyType;
begin
  if Columnbox_Army.ItemIndex = -1 then
  begin
    Label_NoArmyData.Show;
    Columnbox_Army.Hide;
    for WType := Low(TChartArmyType) to High(TChartArmyType) do
    begin
      Chart_MPArmy[WType].Chart.Hide;
      Chart_MPArmyTotal[WType].Chart.Hide;
    end;
    Exit;
  end;

  Label_NoArmyData.Hide;
  Columnbox_Army.Show;

  W := TChartArmyType(Columnbox_Army.Rows[Columnbox_Army.ItemIndex].Tag);

  //Find and hide old chart
  for WType := Low(TChartArmyType) to High(TChartArmyType) do
  begin
    //Remember which lines were visible
    if Chart_MPArmy[WType].Chart.Visible then
      for K := 0 to Chart_MPArmy[WType].Chart.LineCount - 1 do
        fPlayersVisibleArmy[Chart_MPArmy[WType].Chart.Lines[K].Tag] := Chart_MPArmy[WType].Chart.Lines[K].Visible
    else if Chart_MPArmyTotal[WType].Chart.Visible then
      for K := 0 to Chart_MPArmyTotal[WType].Chart.LineCount - 1 do
        fPlayersVisibleArmy[Chart_MPArmyTotal[WType].Chart.Lines[K].Tag] := Chart_MPArmyTotal[WType].Chart.Lines[K].Visible;

    Chart_MPArmy[WType].Chart.Visible := False;
    Chart_MPArmyTotal[WType].Chart.Visible := False;
  end;

  case Radio_ChartArmyStyle.ItemIndex of
    0:  begin
          Chart_MPArmy[W].Chart.Visible := True;
          //Restore previously visible lines
          for K := 0 to Chart_MPArmy[W].Chart.LineCount - 1 do
            Chart_MPArmy[W].Chart.SetLineVisible(K, fPlayersVisibleArmy[Chart_MPArmy[W].Chart.Lines[K].Tag]);
        end;
    1:  begin
          Chart_MPArmyTotal[W].Chart.Visible := True;
          //Restore previously visible lines
          for K := 0 to Chart_MPArmyTotal[W].Chart.LineCount - 1 do
            Chart_MPArmyTotal[W].Chart.SetLineVisible(K, fPlayersVisibleArmy[Chart_MPArmyTotal[W].Chart.Lines[K].Tag]);
        end;
  end;

end;


procedure TKMMenuResultsMP.Refresh;
var
  I: Integer;
begin
  // When exit mission update stats to build actual charts
  // without CHARTS_SAMPLING_FOR_TACTICS or CHARTS_SAMPLING_FOR_ECONOMY delays
  // so measurements for warriors/goods produces will not differ from charts
  for I := 0 to gHands.Count - 1 do
  begin
    gHands[I].Stats.UpdateState;
  end;

  case fGameResultMsg of
    gr_Win:       Label_ResultsMP.Caption := gResTexts[TX_MENU_MISSION_VICTORY];
    gr_Defeat:    Label_ResultsMP.Caption := gResTexts[TX_MENU_MISSION_DEFEAT];
    gr_Cancel:    Label_ResultsMP.Caption := gResTexts[TX_MENU_MISSION_CANCELED];
    gr_ReplayEnd: Label_ResultsMP.Caption := gResTexts[TX_MENU_REPLAY_ENDED];
    else          Label_ResultsMP.Caption := NO_TEXT;
  end;
  //Append mission name and time after the result message
  Label_ResultsMP.Caption := Label_ResultsMP.Caption + ' - ' + gGame.GameName + ' - ' + TimeToString(gGame.MissionTime);

  //Get player count to compact their data output
  fEnabledPlayers := 0;
  for I := 0 to Min(MAX_LOBBY_PLAYERS, gHands.Count) - 1 do
    if gHands[I].Enabled then
      Inc(fEnabledPlayers);

  RefreshBars;
  RefreshCharts;
  RefreshChartWares;
  RefreshChartArmy;

  Button_MPResultsWares.Enabled := (gGame.MissionMode = mm_Normal);
  Button_MPResultsEconomy.Enabled := (gGame.MissionMode = mm_Normal);

  //Back button has different captions depending on where it returns us to
  if fGameResultMsg <> gr_ReplayEnd then
    Button_ResultsMPBack.Caption := gResTexts[TX_RESULTS_BACK_MP]
  else
    Button_ResultsMPBack.Caption := gResTexts[TX_RESULTS_BACK_REPLAYS];

  //Show first tab
  TabChange(Button_MPResultsBars);
end;


procedure TKMMenuResultsMP.RefreshBars;

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
  ValueStr: String;
begin
  //Update visibility depending on players count (note, players may be sparsed)
  for I := 0 to MAX_LOBBY_PLAYERS - 1 do
    SetPlayerControls(I, False); //Disable them all to start
  Index := 0;
  for I := 0 to Min(MAX_LOBBY_PLAYERS, gHands.Count) - 1 do
    if gHands[I].Enabled then
    begin
      SetPlayerControls(Index, True); //Enable used ones
      Label_ResultsPlayerName1[Index].Caption   := gHands[I].OwnerName;
      Label_ResultsPlayerName1[Index].FontColor := FlagColorToTextColor(gHands[I].FlagColor);
      Label_ResultsPlayerName2[Index].Caption   := gHands[I].OwnerName;
      Label_ResultsPlayerName2[Index].FontColor := FlagColorToTextColor(gHands[I].FlagColor);
      Inc(Index);
    end;

  //Update positioning
  Panel_BarsUpper.Height := 60 + fEnabledPlayers * BAR_ROW_HEIGHT;
  Panel_BarsLower.Height := 40 + fEnabledPlayers * BAR_ROW_HEIGHT;

  //Second panel does not move from the middle of the screen: results always go above and below the middle
  Panel_BarsUpper.Top := Max(0, Panel_Bars.Height div 2 - Panel_BarsUpper.Height - 5);
  Panel_BarsLower.Top := Panel_BarsUpper.Top + Panel_BarsUpper.Height + 5;

  //Calculate best scores
  FillChar(Bests, SizeOf(Bests), #0);
  //These are a special case: Less is better so we initialize them high
  Bests[1] := High(Cardinal);
  Bests[3] := High(Cardinal);
  Bests[6] := High(Cardinal);
  FillChar(Totals, SizeOf(Totals), #0);

  //Calculate bests for each "section"
  for I := 0 to Min(MAX_LOBBY_PLAYERS, gHands.Count) - 1 do
    if gHands[I].Enabled then
      with gHands[I].Stats do
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
  for I := 0 to Min(MAX_LOBBY_PLAYERS, gHands.Count) - 1 do
    if gHands[I].Enabled then
    begin

      with gHands[I].Stats do
      begin
        //Living things
        Bar_Results[Index,0].Tag := GetCitizensTrained;
        Bar_Results[Index,1].Tag := GetCitizensLost;
        Bar_Results[Index,1].Caption := IntToStr(GetCitizensDiedOfHunger);
        Bar_Results[Index,2].Tag := GetWarriorsTrained;
        Bar_Results[Index,3].Tag := GetWarriorsLost;
        Bar_Results[Index,3].Caption := IntToStr(GetWarriorsDiedOfHunger);
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

      ValueStr := IntToStr(Bar_Results[I,K].Tag);
      if (K = 1) or (K = 3) then // Citizens or Warriors Lost
        ValueStr := ValueStr + ' / ' + IfThen(Bar_Results[I,K].Caption <> '0', Bar_Results[I,K].Caption, '-'); // add died of starvation info

      Bar_Results[I,K].Caption := IfThen(Bar_Results[I,K].Tag <> 0, ValueStr, '-');
    end;
  end;
end;


procedure TKMMenuResultsMP.RefreshCharts;
var
  I: Integer;
begin
  for I := 0 to MAX_HANDS - 1 do
  begin
    fPlayersVisibleWares[I] := True;
    fPlayersVisibleArmy[I] := True;
  end;

  Chart_MPCitizens.Clear;
  Chart_MPHouses.Clear;

  Chart_MPCitizens.MaxLength := 0;
  Chart_MPHouses.MaxLength := 0;
  Chart_MPCitizens.MaxTime  := gGame.GameTickCount div 10;
  Chart_MPHouses.MaxTime    := gGame.GameTickCount div 10;

  for I := 0 to gHands.Count - 1 do
  with gHands[I] do
    if Enabled then
    begin
      Chart_MPCitizens.MaxLength := Max(Chart_MPCitizens.MaxLength, Stats.ChartCount);
      Chart_MPCitizens.AddLine(OwnerName, FlagColor, Stats.ChartCitizens);
    end;

  for I := 0 to gHands.Count - 1 do
  with gHands[I] do
    if Enabled then
    begin
      Chart_MPHouses.MaxLength := Max(Chart_MPHouses.MaxLength, Stats.ChartCount);
      Chart_MPHouses.AddLine(OwnerName, FlagColor, Stats.ChartHouses);
    end;
end;


procedure TKMMenuResultsMP.RefreshChartWares;
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
    for K := 0 to gHands.Count - 1 do
    if gHands[K].Enabled
      and not gHands[K].Stats.ChartWaresEmpty(R) then
    begin
      Columnbox_Wares.AddItem(MakeListRow(['', gRes.Wares[R].Title],
                                          [$FFFFFFFF, $FFFFFFFF],
                                          [MakePic(rxGui, gRes.Wares[R].GUIIcon), MakePic(rxGui, 0)],
                                          Byte(R)));
      Break;
    end;
  end;

  for J := 0 to Columnbox_Wares.RowCount - 1 do
  begin
    R := TWareType(Columnbox_Wares.Rows[J].Tag);

    Chart_MPWares[R].Clear;
    Chart_MPWares[R].MaxLength := 0;
    Chart_MPWares[R].MaxTime := gGame.GameTickCount div 10;
    Chart_MPWares[R].Caption := gResTexts[TX_GRAPH_TITLE_RESOURCES] + ' - ' + gRes.Wares[R].Title;

    for I := 0 to gHands.Count - 1 do
    with gHands[I] do
    if Enabled then
    begin
      Chart_MPWares[R].MaxLength := Max(Chart_MPWares[R].MaxLength, Stats.ChartCount);
      //Do some postprocessing on stats (GDP, food value)
      Chart_MPWares[R].AddLine(OwnerName, FlagColor, GetChartWares(I, R), I);
    end;
  end;

  Columnbox_Wares.ItemIndex := 0;
  Columnbox_Wares.ItemHeight := Min(Columnbox_Wares.Height div 15, 20);
  WareChange(nil);
end;


procedure TKMMenuResultsMP.RefreshChartArmy;
var
  I,K,J: Integer;
  WType: TChartArmyType;
begin
  //Fill columnbox
  Columnbox_Army.Clear;
  for WType := Low(TChartArmyType) to High(TChartArmyType) do
  begin
    for K := 0 to gHands.Count - 1 do
      if gHands[K].Enabled
        and not Chart_MPArmy[WType].IsEmpty(K) then
      begin
        Columnbox_Army.AddItem(MakeListRow(['', Chart_MPArmy[WType].ChartType.GUIName],
                                            [gMySpectator.Hand.FlagColor, $FFFFFFFF],
                                            [MakePic(rxGui, Chart_MPArmy[WType].ChartType.GUIIcon), MakePic(rxGui, 0)],
                                            Byte(WType)));
        Break;
      end;
  end;

  //Fill in chart values
  for J := 0 to Columnbox_Army.RowCount - 1 do
  begin
    WType := TChartArmyType(Columnbox_Army.Rows[J].Tag);

    Chart_MPArmy[WType].Chart.Clear;
    Chart_MPArmy[WType].Chart.MaxLength := 0;
    Chart_MPArmy[WType].Chart.MaxTime := gGame.GameTickCount div 10;
    Chart_MPArmy[WType].Chart.Caption := 'Instantaneous' + ' - ' + Chart_MPArmy[WType].ChartType.GUIName; // Todo translate

    Chart_MPArmyTotal[WType].Chart.Clear;
    Chart_MPArmyTotal[WType].Chart.MaxLength := 0;
    Chart_MPArmyTotal[WType].Chart.MaxTime := gGame.GameTickCount div 10;
    Chart_MPArmyTotal[WType].Chart.Caption := 'Total Equipped' + ' - ' + Chart_MPArmyTotal[WType].ChartType.GUIName; // Todo translate

    for I := 0 to gHands.Count - 1 do
      with gHands[I] do
        if Enabled then
        begin
          Chart_MPArmy[WType].Chart.MaxLength := Max(Chart_MPArmy[WType].Chart.MaxLength, Stats.ChartCount);
          Chart_MPArmy[WType].AddLine(I, OwnerName, FlagColor);
          Chart_MPArmyTotal[WType].Chart.MaxLength := Max(Chart_MPArmy[WType].Chart.MaxLength, Stats.ChartCount);
          Chart_MPArmyTotal[WType].AddLine(I, OwnerName, FlagColor);
        end;
  end;

  Columnbox_Army.ItemIndex := 0;
  // 33 is a bit more then unit icons max height
  Columnbox_Army.ItemHeight := Min(Columnbox_Army.Height div 13, 33);
  ArmyChange(nil);
end;


procedure TKMMenuResultsMP.Create_ResultsMP(aParent: TKMPanel);
begin
  Panel_ResultsMP := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_ResultsMP.AnchorsStretch;
    with TKMImage.Create(Panel_ResultsMP,0,0,aParent.Width, aParent.Height,7,rxGuiMain) do
    begin
      ImageStretch;
      AnchorsCenter;
    end;
    with TKMShape.Create(Panel_ResultsMP,0,0,aParent.Width, aParent.Height) do
    begin
      AnchorsCenter;
      FillColor := $A0000000;
    end;

    Label_ResultsMP := TKMLabel.Create(Panel_ResultsMP,62,125,900,20,NO_TEXT,fnt_Metal,taCenter);
    Label_ResultsMP.Anchors := [anLeft];

    Button_MPResultsBars := TKMButtonFlat.Create(Panel_ResultsMP, 160, 155, 176, 20, 8, rxGuiMain);
    Button_MPResultsBars.TexOffsetX := -78;
    Button_MPResultsBars.TexOffsetY := 6;
    Button_MPResultsBars.Anchors := [anLeft];
    Button_MPResultsBars.Caption := gResTexts[TX_RESULTS_STATISTICS];
    Button_MPResultsBars.CapOffsetY := -11;
    Button_MPResultsBars.OnClick := TabChange;

    Button_MPResultsArmy := TKMButtonFlat.Create(Panel_ResultsMP, 340, 155, 176, 20, 53, rxGui);
    Button_MPResultsArmy.TexOffsetX := -76;
    Button_MPResultsArmy.TexOffsetY := 6;
    Button_MPResultsArmy.Anchors := [anLeft];
    Button_MPResultsArmy.Caption := gResTexts[TX_GRAPH_ARMY];
    Button_MPResultsArmy.CapOffsetY := -11;
    Button_MPResultsArmy.OnClick := TabChange;

    Button_MPResultsEconomy := TKMButtonFlat.Create(Panel_ResultsMP, 520, 155, 176, 20, 589, rxGui);
    Button_MPResultsEconomy.TexOffsetX := -72;
    Button_MPResultsEconomy.TexOffsetY := 6;
    Button_MPResultsEconomy.Anchors := [anLeft];
    Button_MPResultsEconomy.Caption := gResTexts[TX_RESULTS_ECONOMY];
    Button_MPResultsEconomy.CapOffsetY := -11;
    Button_MPResultsEconomy.OnClick := TabChange;

    Button_MPResultsWares := TKMButtonFlat.Create(Panel_ResultsMP, 700, 155, 176, 20, 360, rxGui);
    Button_MPResultsWares.TexOffsetX := -77;
    Button_MPResultsWares.TexOffsetY := 6;
    Button_MPResultsWares.Anchors := [anLeft];
    Button_MPResultsWares.Caption := gResTexts[TX_GRAPH_RESOURCES];
    Button_MPResultsWares.CapOffsetY := -11;
    Button_MPResultsWares.OnClick := TabChange;

    CreateBars(Panel_ResultsMP);

    Panel_ChartsMP := TKMPanel.Create(Panel_ResultsMP, 0, PANES_TOP, 1024, 560);
    Panel_ChartsMP.Anchors := [anLeft];

      Chart_MPCitizens := TKMChart.Create(Panel_ChartsMP, 62, 0, 900, 200);
      Chart_MPCitizens.Caption := gResTexts[TX_GRAPH_CITIZENS];
      Chart_MPCitizens.Anchors := [anLeft];

      Chart_MPHouses := TKMChart.Create(Panel_ChartsMP, 62, 235, 900, 200);
      Chart_MPHouses.Caption := gResTexts[TX_GRAPH_HOUSES];
      Chart_MPHouses.Anchors := [anLeft];

    CreateChartWares(Panel_ResultsMP);
    CreateChartArmy(Panel_ResultsMP);

    Button_ResultsMPBack := TKMButton.Create(Panel_ResultsMP, 100, 650, 280, 30, NO_TEXT, bsMenu);
    Button_ResultsMPBack.Anchors := [anLeft];
    Button_ResultsMPBack.OnClick := BackClick;
end;


function TKMMenuResultsMP.GetChartWares(aPlayer: TKMHandIndex; aWare: TWareType): TKMCardinalArray;
var
  RT: TWareType;
  I: Integer;
begin
  with gHands[aPlayer].Stats do
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


procedure TKMMenuResultsMP.Show(aMsg: TGameResultMsg);
begin
  fGameResultMsg := aMsg;

  Refresh;
  Panel_ResultsMP.Show;
end;


procedure TKMMenuResultsMP.BackClick(Sender: TObject);
begin
  //Depending on where we were created we need to return to a different place
  //Multiplayer game end -> ResultsMP -> Multiplayer
  //Multiplayer replay end -> ResultsMP -> Replays

  if fGameResultMsg <> gr_ReplayEnd then
    fOnPageChange(gpMultiplayer)
  else
    fOnPageChange(gpReplays);
end;


end.
