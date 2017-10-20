unit KM_GUIMenuResultsMP;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls, Math, StrUtils, SysUtils,
  KM_CommonTypes, KM_Controls, KM_Defaults, KM_Pics,
  KM_InterfaceDefaults, KM_ResWares, KM_HandStats;


type
  TKMStatType = (st_ByPlayers, st_ByTeams);

  TKMEconomyStatKind = (est_Citizens, est_Houses);

  // Army chart types (enum)
  TKMChartWarriorType = (cwt_ArmyPower, cwt_All,
    cwt_Militia,      cwt_AxeFighter,   cwt_Swordsman,     cwt_Bowman,
    cwt_Arbaletman,   cwt_Pikeman,      cwt_Hallebardman,  cwt_HorseScout,
    cwt_Cavalry,      cwt_Barbarian,
    cwt_Peasant,      cwt_Slingshot,    cwt_MetalBarbarian, cwt_Horseman);

  // Chart army type class
  TKMChartWarrior = class
  private
    fType: TKMChartWarriorType;
    fUnitType: TUnitType;
    function GetUnitType: TUnitType;
    function GetGUIName: UnicodeString;
    function GetGUIIcon: Word;
  public
    constructor Create(aType: TKMChartWarriorType);
    property UnitType: TUnitType read GetUnitType;
    property GUIName: UnicodeString read GetGUIName;
    property GUIIcon: Word read GetGUIIcon;
    function HasUnitType: Boolean;
  end;

  TKMChartArmyMP = class
  private
    fType: TKMChartWarrior;
    fKind: TKMChartArmyKind;
    fChart: TKMChart;
    function GetArmyPowerChartData(aPlayer: TKMHandIndex): TKMCardinalArray;
  public
    constructor Create(aType: TKMChartWarriorType; aKind: TKMChartArmyKind; aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
    destructor Destroy; override;
    procedure AddLine(aPlayerId: TKMHandIndex; const aTitle: UnicodeString; aColor: Cardinal);
    function IsEmpty(aPlayer: TKMHandIndex): Boolean;
    function GetChartData(aPlayer: TKMHandIndex): TKMCardinalArray;
    property Chart: TKMChart read fChart;
    property ChartType: TKMChartWarrior read fType;
  end;

  PKMChartArmyMP = ^TKMChartArmyMP;

  TKMStatsValues = array[0..MAX_HANDS-1] of array [0..9] of Cardinal;

  TKMMenuResultsMP = class (TKMMenuPageCommon)
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class

    fGameResultMsg: TGameResultMsg; //So we know where to go after results screen
    fPlayersVisibleWares: array [0 .. MAX_HANDS - 1] of Boolean;  //Remember visible players when toggling wares
    fPlayersVisibleArmy: array [0 .. MAX_HANDS - 1] of Boolean;   //Remember visible players when toggling warriors
    fGameResultCaption: String;

    fStatType: TKMStatType;

    fHandFlagColor: Cardinal;
    fNoEconomyChartData: Boolean;
    fNoArmyChartData: Boolean;
    fColumnBoxArmy_Rows: array[TKMChartArmyKind] of array of TKMChartWarriorType;

    fChartSeparatorsPos: array[TKMStatType] of TStringList;
    fNamesToShow: array[TKMStatType] of array[0 .. MAX_HANDS - 1] of String;
    fColorsToShow: array[TKMStatType] of array[0 .. MAX_HANDS - 1] of Cardinal;
    fListToShow: array[TKMStatType] of TStringList;
    fStatsValues: array[TKMStatType] of TKMStatsValues;

    fIsStatsRefreshed: Boolean;
    fShowAIResults: Boolean;
    procedure RecreateListToShow(aStatType: TKMStatType);
    procedure BackClick(Sender: TObject);
    function DoAdjoinSameColorHand(aHandId: Integer): Boolean;
    function GetSelectedChartArmyKind: TKMChartArmyKind;
    function GetChartWares(aPlayer: TKMHandIndex; aWare: TWareType; aUseGDP: Boolean): TKMCardinalArray;
    function DoShowHandStats(aHandId: Integer): Boolean;

    procedure Create_ResultsMP(aParent: TKMPanel);
    procedure CreateBars(aParent: TKMPanel);
    procedure CreateChartEconomy;
    procedure CreateChartWares(aParent: TKMPanel);
    procedure CreateChartArmy(aParent: TKMPanel);

    procedure UpdateVisibleTab;
    procedure TabChange(Sender: TObject);
    procedure StatTypeChange(Sender: TObject);
    procedure BarsUpdate(aStatType: TKMStatType);
    procedure EconomyUpdate(Sender: TObject);
    procedure WareUpdate(Sender: TObject);
    procedure ArmyUpdate(Sender: TObject);

    procedure RadioEconomyTypeChange(Sender: TObject);
    procedure RadioWareTypeChange(Sender: TObject);
    procedure RadioArmyTypeChange(Sender: TObject);

    procedure Reinit;
    procedure ReinitPlayersToShow;
    procedure ReinitTeamsToShow;
    procedure ReinitBars;
    procedure ReinitChartEconomy;
    procedure ReinitChartWares;
    procedure ReinitChartArmy;
  protected
    Panel_ResultsMP: TKMPanel;
      Button_MPResultsBars,
      Button_MPResultsArmy,
      Button_MPResultsEconomy,
      Button_MPResultsWares,
      Button_Players,Button_Teams: TKMButtonFlat;
      Label_ResultsMP: TKMLabel;
      Panel_Bars: TKMPanel;
        Panel_BarsUpper, Panel_BarsLower: TKMPanel;
          Label_ResultsPlayerName1, Label_ResultsPlayerName2: array [0 .. MAX_LOBBY_PLAYERS - 1] of TKMLabel;
          Bar_Results: array [0 .. MAX_LOBBY_PLAYERS - 1, 0 .. 9] of TKMPercentBar;
          Image_ResultsRosette: array [0 .. MAX_LOBBY_PLAYERS - 1, 0 .. 9] of TKMImage;
      Panel_ChartsEconomy: TKMPanel;
        Chart_Players_Citizens: TKMChart;
        Chart_Players_Houses: TKMChart;
        Chart_Teams_Citizens: TKMChart;
        Chart_Teams_Houses: TKMChart;
        Label_NoEconomyData: TKMLabel;
        Panel_ChartEconomy_Type: TKMPanel;
          Radio_ChartEconomyType: TKMRadioGroup;
      Panel_ChartsWares: TKMPanel;
        Columnbox_Wares: TKMColumnBox;    //columnBox for Quantities charts
        Columnbox_WaresGDP: TKMColumnBox; //columnBox for GDP charts
        Charts_Wares: array[TKMStatType] of array[TWareType] of TKMChart; //One for each kind
        Charts_WaresGDP: array[TKMStatType] of array [0..2] of TKMChart;
        Label_NoWareData: TKMLabel;
        Panel_ChartWare_Type: TKMPanel;
          Radio_ChartWareType: TKMRadioGroup;
      Panel_ChartsArmy: TKMPanel;
        Columnbox_Army: TKMColumnBox;
        Charts_Army: array[TKMStatType] of array[TKMChartArmyKind] of array[TKMChartWarriorType] of TKMChartArmyMP;
        Label_NoArmyData: TKMLabel;
        Panel_ChartArmy_Type: TKMPanel;
          Radio_ChartArmyType: TKMRadioGroup;
      Button_ResultsMPBack: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    destructor Destroy; override;

    procedure Show(aMsg: TGameResultMsg);
  end;


implementation
uses
  KM_ResTexts, KM_Game, KM_HandsCollection, KM_CommonUtils, KM_Resource, KM_ResFonts,
  KM_RenderUI, KM_Hand, KM_ResUnits;


const
  PANES_TOP = 185-80;
  BAR_ROW_HEIGHT = 21;
  CHART_HEIGHT = 595;
  CHART_ECO_HEIGHT = 285;
  BACK_BTN_Y_TO_BOTTOM = 60;

  WARRIORS_POWER_RATES: array [WARRIOR_MIN..WARRIOR_MAX] of Single = (
    1, 2.4, 5.2,    // ut_Militia, ut_AxeFighter, ut_Swordsman
    2.2, 4,         // ut_Bowman, ut_Arbaletman
    2, 4,           // ut_Pikeman, ut_Hallebardman
    3.3, 6,         // ut_HorseScout, ut_Cavalry
    5.3, 1.5, 1.5,  // ut_Barbarian, ut_Peasant, ut_Slingshot
    5.3, 2.1        // ut_MetalBarbarian, ut_Horseman
  );

  GDPWares: array [0..2] of TWareType = (wt_All, wt_Warfare, wt_Food);


function GetWareIdInGDPArr(aWare: TWareType): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to 2 do
    if aWare = GDPWares[I] then
    begin
      Result := I;
      Exit;
    end;
end;


function GetOwnerName(aHandId: Integer): String;
begin
  Result := gHands[aHandId].OwnerName(not (gGame.GameMode in [gmSingle, gmCampaign, gmReplaySingle]));
end;


function GetChartLegendCaption(aStatType: TKMStatType): String;
begin
  Result := '';
  case aStatType of
    st_ByPlayers: Result := 'Players';  //Todo translate
    st_ByTeams:   Result := 'Teams';     //Todo translate
  end;
end;


{TKMChartArmyType}
constructor TKMChartWarrior.Create(aType: TKMChartWarriorType);
begin
  fType := aType;
  case aType of
    cwt_All:                    fUnitType := ut_Any;
    cwt_Militia..cwt_Horseman:  fUnitType := TUnitType(Ord(ut_Militia) + Ord(aType) - Ord(cwt_Militia));
  end;
end;


function TKMChartWarrior.GetUnitType: TUnitType;
begin
  Assert(HasUnitType, 'ArmyPower has no UnitType match');
  Result := fUnitType;
end;


function TKMChartWarrior.HasUnitType: Boolean;
begin
  Result := fType <> cwt_ArmyPower;
end;


function TKMChartWarrior.GetGUIName: UnicodeString;
begin
  case fType of
    cwt_ArmyPower: Result := 'Army power'; //Todo translate
    cwt_All:       Result := 'All soldiers'; //Todo translate
    else           Result := gRes.Units[UnitType].GUIName;
  end;
end;


function TKMChartWarrior.GetGUIIcon: Word;
begin
  case fType of
    cwt_ArmyPower: Result := 53;
    cwt_All:       Result := 665;
    else           Result := gRes.Units[UnitType].GUIIcon;
  end;
end;


{TKMChartArmy}
constructor TKMChartArmyMP.Create(aType: TKMChartWarriorType; aKind: TKMChartArmyKind; aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
begin
  fType := TKMChartWarrior.Create(aType);
  fKind := aKind;
  fChart := TKMChart.Create(aParent, aLeft, aTop, aWidth, aHeight);
end;


destructor TKMChartArmyMP.Destroy;
begin
  FreeAndNil(fType);
  // fChart is freed by GUI (MasterPanel and so on...)
  inherited;
end;


function TKMChartArmyMP.GetChartData(aPlayer: TKMHandIndex): TKMCardinalArray;
begin
  if (fType.HasUnitType) then
    Result := gHands[aPlayer].Stats.ChartArmy[fKind,fType.UnitType]
  else
    Result := GetArmyPowerChartData(aPlayer);
end;


procedure TKMChartArmyMP.AddLine(aPlayerId: TKMHandIndex; const aTitle: UnicodeString; aColor: Cardinal);
begin
  Chart.AddLine(aTitle, aColor, GetChartData(aPlayerId), aPlayerId);
end;


function TKMChartArmyMP.GetArmyPowerChartData(aPlayer: TKMHandIndex): TKMCardinalArray;
var
  WT: TUnitType;
  I, ChartCnt: Integer;
  Value: Single;
begin
  ChartCnt := gHands[aPlayer].Stats.ChartCount;
  //Create new array and fill it (otherwise we assign pointers and corrupt data)
  SetLength(Result, ChartCnt);
  for I := 0 to ChartCnt - 1 do
    Result[I] := 0;

  for I := 0 to ChartCnt - 1 do
  begin
    Value := 0;
    for WT := WARRIOR_MIN to WARRIOR_MAX do
      Value := Value + gHands[aPlayer].Stats.ChartArmy[fKind,WT][I]*WARRIORS_POWER_RATES[WT];
    Result[I] := Result[I] + Round(Value);
  end;
end;


function TKMChartArmyMP.IsEmpty(aPlayer: TKMHandIndex): Boolean;
begin
  if (fType.HasUnitType) then
    Result := gHands[aPlayer].Stats.ChartArmyEmpty(fKind, fType.UnitType)
  else
    Result := gHands[aPlayer].Stats.ChartArmyEmpty(fKind, ut_Any);
end;


{ TKMGUIMenuResultsMP }
constructor TKMMenuResultsMP.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
var
  ST: TKMStatType;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  for ST := Low(TKMStatType) to High(TKMStatType) do
  begin
    fChartSeparatorsPos[ST] := TStringList.Create;
    fChartSeparatorsPos[ST].Sorted := True;
  end;

  Create_ResultsMP(aParent);
end;


destructor TKMMenuResultsMP.Destroy;
var
  WType: TKMChartWarriorType;
  CKind: TKMChartArmyKind;
  ST: TKMStatType;
begin
  for ST := High(TKMStatType) downto Low(TKMStatType) do
  begin
    for CKind := High(TKMChartArmyKind) downto Low(TKMChartArmyKind) do
      for WType := High(TKMChartWarriorType) downto Low(TKMChartWarriorType) do
        FreeAndNil(Charts_Army[ST,CKind,WType]);

    if fListToShow[ST] <> nil then
      FreeAndNil(fListToShow[ST]);
    FreeAndNil(fChartSeparatorsPos[ST]);
  end;
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
  I,K, Middle: Integer;
begin
  Panel_Bars := TKMPanel.Create(aParent, 62, PANES_TOP, 900, aParent.Height - PANES_TOP - BACK_BTN_Y_TO_BOTTOM);
  Panel_Bars.AnchorsCenter;
  Middle := Panel_Bars.Height div 2;

    //Composed of two sections each on own Panel to position them vertically according to player count

    Panel_BarsUpper := TKMPanel.Create(Panel_Bars, 0, 0, 900, Middle - 3);
    Panel_BarsUpper.AnchorsCenter;

      for I := 0 to MAX_LOBBY_PLAYERS - 1 do
        Label_ResultsPlayerName1[I] := TKMLabel.Create(Panel_BarsUpper, 0, 38+I*BAR_ROW_HEIGHT, 150, 20, '', fnt_Metal, taLeft);

      for K := 0 to 4 do
      begin
        with TKMLabel.Create(Panel_BarsUpper, 160 + BarStep*K, 0, BarWidth+6, 40, gResTexts[Columns1[K]], fnt_Metal, taCenter) do
          AutoWrap := True;
        for I:=0 to MAX_LOBBY_PLAYERS - 1 do
        begin
          Bar_Results[I,K] := TKMPercentBar.Create(Panel_BarsUpper, 160 + K*BarStep, 35+I*BAR_ROW_HEIGHT, BarWidth, 20, fnt_Grey);
          Bar_Results[I,K].TextYOffset := -3;
          Image_ResultsRosette[I,K] := TKMImage.Create(Panel_BarsUpper, 164 + K*BarStep, 38+I*BAR_ROW_HEIGHT, 16, 16, 8, rxGuiMain);
        end;
      end;

    Panel_BarsLower := TKMPanel.Create(Panel_Bars, 0, Middle+3, 900, Middle - 10);
    Panel_BarsLower.AnchorsCenter;

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


procedure TKMMenuResultsMP.CreateChartEconomy;
const
  RADIO_ECO_HEIGHT = 80;
begin
  Panel_ChartsEconomy := TKMPanel.Create(Panel_ResultsMP, 0, PANES_TOP, 1024, CHART_HEIGHT);
  Panel_ChartsEconomy.Anchors := [anLeft];
    Chart_Players_Citizens := TKMChart.Create(Panel_ChartsEconomy, 62, 0, 900, CHART_HEIGHT);
    Chart_Players_Citizens.Caption := gResTexts[TX_GRAPH_CITIZENS];
    Chart_Players_Citizens.LegendCaption := 'Players'; //Todo translate
    Chart_Players_Citizens.Anchors := [anLeft];

    Chart_Teams_Citizens := TKMChart.Create(Panel_ChartsEconomy, 62, 0, 900, CHART_HEIGHT);
    Chart_Teams_Citizens.Caption := gResTexts[TX_GRAPH_CITIZENS];
    Chart_Teams_Citizens.LegendCaption := 'Teams'; //Todo translate
    Chart_Teams_Citizens.Anchors := [anLeft];

    Chart_Players_Houses := TKMChart.Create(Panel_ChartsEconomy, 62, 0, 900, CHART_HEIGHT);
    Chart_Players_Houses.Caption := gResTexts[TX_GRAPH_HOUSES];
    Chart_Players_Houses.LegendCaption := 'Players'; //Todo translate
    Chart_Players_Houses.Anchors := [anLeft];
    Chart_Players_Houses.Hide;

    Chart_Teams_Houses := TKMChart.Create(Panel_ChartsEconomy, 62, 0, 900, CHART_HEIGHT);
    Chart_Teams_Houses.Caption := gResTexts[TX_GRAPH_HOUSES];
    Chart_Teams_Houses.LegendCaption := 'Teams'; //Todo translate
    Chart_Teams_Houses.Anchors := [anLeft];
    Chart_Teams_Houses.Hide;

    Label_NoEconomyData := TKMLabel.Create(Panel_ChartsEconomy, 512, CHART_HEIGHT div 2, gResTexts[TX_GRAPH_NO_DATA], fnt_Metal, taCenter);

    Panel_ChartEconomy_Type := TKMPanel.Create(Panel_ChartsEconomy, 817, CHART_HEIGHT - RADIO_ECO_HEIGHT - 20, 150, RADIO_ECO_HEIGHT);
      with TKMShape.Create(Panel_ChartEconomy_Type, 0, 0, 150, RADIO_ECO_HEIGHT) do
      begin
        FillColor := icDarkestGrayTrans;
        LineColor := icGray;
        LineWidth := 1;
      end;

      TKMLabel.Create(Panel_ChartEconomy_Type, 5, 8, 140, 20, 'Chart type', fnt_Metal, taCenter); // Todo translate

      Radio_ChartEconomyType := TKMRadioGroup.Create(Panel_ChartEconomy_Type,5,35,140,RADIO_ECO_HEIGHT - 40,fnt_Grey);
      Radio_ChartEconomyType.DrawChkboxOutline := True;
      Radio_ChartEconomyType.ItemIndex := 0;
      Radio_ChartEconomyType.Add(gResTexts[TX_GRAPH_CITIZENS]);
      Radio_ChartEconomyType.Add(gResTexts[TX_GRAPH_HOUSES]);
      Radio_ChartEconomyType.OnChange := RadioEconomyTypeChange;
end;


procedure TKMMenuResultsMP.RadioEconomyTypeChange(Sender: TObject);
begin
  EconomyUpdate(nil);
end;


procedure TKMMenuResultsMP.CreateChartWares(aParent: TKMPanel);

  procedure SetupWareColumnBox(aColumnBox: TKMColumnBox);
  begin
    aColumnBox.SetColumns(fnt_Game, ['', ''], [0, 20]);
    aColumnBox.ShowHeader := False;
    aColumnBox.ShowLines := False;
    aColumnBox.OnChange := WareUpdate;
  end;

  procedure SetupWareChart(aChart: TKMChart; aStatType: TKMStatType);
  begin
    aChart.Caption := gResTexts[TX_GRAPH_TITLE_RESOURCES];
    aChart.LegendCaption := GetChartLegendCaption(aStatType);
    aChart.Font := fnt_Metal; //fnt_Outline doesn't work because player names blend badly with yellow
    aChart.Hide;
  end;

const
  WARES_TYPE_HEIGHT = 80;
var
  I: Integer;
  W: TWareType;
  ST: TKMStatType;
begin
  Panel_ChartsWares := TKMPanel.Create(aParent, 62, PANES_TOP, 900, CHART_HEIGHT);
  Panel_ChartsWares.AnchorsCenter;

    Columnbox_Wares := TKMColumnBox.Create(Panel_ChartsWares, 0, 0, 145, CHART_HEIGHT, fnt_Game, bsMenu);
    Columnbox_WaresGDP := TKMColumnBox.Create(Panel_ChartsWares, 0, 0, 145, CHART_HEIGHT, fnt_Game, bsMenu);
    SetupWareColumnBox(Columnbox_Wares);
    SetupWareColumnBox(Columnbox_WaresGDP);

    for ST := Low(TKMStatType) to High(TKMStatType) do
    begin
      for W := Low(TWareType) to High(TWareType) do
      begin
        Charts_Wares[ST,W] := TKMChart.Create(Panel_ChartsWares, 140, 0, 900-140, CHART_HEIGHT);
        SetupWareChart(Charts_Wares[ST,W], ST);
      end;

      for I := Low(GDPWares) to High(GDPWares) do
      begin
        Charts_WaresGDP[ST,I] := TKMChart.Create(Panel_ChartsWares, 140, 0, 900-140, CHART_HEIGHT);
        SetupWareChart(Charts_WaresGDP[ST,I], ST);
      end;
    end;

    Label_NoWareData := TKMLabel.Create(Panel_ChartsWares, 450, CHART_HEIGHT div 2, gResTexts[TX_GRAPH_NO_DATA], fnt_Metal, taCenter);

    Panel_ChartWare_Type := TKMPanel.Create(Panel_ChartsWares, 755, CHART_HEIGHT - WARES_TYPE_HEIGHT - 20, 150, WARES_TYPE_HEIGHT);
      with TKMShape.Create(Panel_ChartWare_Type, 0, 0, 150, WARES_TYPE_HEIGHT) do
      begin
        FillColor := icDarkestGrayTrans;
        LineColor := icGray;
        LineWidth := 1;
      end;

      TKMLabel.Create(Panel_ChartWare_Type, 5, 8, 140, 20, 'Chart type', fnt_Metal, taCenter); // Todo translate

      Radio_ChartWareType := TKMRadioGroup.Create(Panel_ChartWare_Type,5,35,140,WARES_TYPE_HEIGHT - 40,fnt_Grey);
      Radio_ChartWareType.DrawChkboxOutline := True;
      Radio_ChartWareType.ItemIndex := 0;
      Radio_ChartWareType.Add('Quantity');  // Todo translate
      Radio_ChartWareType.Add('GDP');       // Todo translate
      Radio_ChartWareType.OnChange := RadioWareTypeChange;
end;


procedure TKMMenuResultsMP.RadioWareTypeChange(Sender: TObject);
begin
  WareUpdate(Radio_ChartWareType);
end;


procedure TKMMenuResultsMP.CreateChartArmy(aParent: TKMPanel);
const
  ARMY_TYPE_HEIGHT = 120;
var
  WType: TKMChartWarriorType;
  CKind: TKMChartArmyKind;
  ST: TKMStatType;
begin
  Panel_ChartsArmy := TKMPanel.Create(aParent, 62, PANES_TOP, 900, CHART_HEIGHT);
  Panel_ChartsArmy.AnchorsCenter;

    Columnbox_Army := TKMColumnBox.Create(Panel_ChartsArmy, 0, 0, 142, CHART_HEIGHT, fnt_Game, bsMenu);
    Columnbox_Army.SetColumns(fnt_Game, ['', ''], [0, 33]);
    Columnbox_Army.ShowHeader := False;
    Columnbox_Army.ShowLines := False;
    Columnbox_Army.OnChange := ArmyUpdate;
    // 33 is a bit more then unit icons max height
    Columnbox_Army.ItemHeight := Min(Columnbox_Army.Height div 13, 33);

    for ST := Low(TKMStatType) to High(TKMStatType) do
      for CKind := Low(TKMChartArmyKind) to High(TKMChartArmyKind) do
        for WType := Low(TKMChartWarriorType) to High(TKMChartWarriorType) do
        begin
          Charts_Army[ST,CKind,WType] := TKMChartArmyMP.Create(WType, CKind, Panel_ChartsArmy, 140, 0, 760, CHART_HEIGHT);
          Charts_Army[ST,CKind,WType].Chart.Caption := gResTexts[TX_GRAPH_ARMY];
          Charts_Army[ST,CKind,WType].Chart.LegendCaption := GetChartLegendCaption(ST); //Todo translate
          Charts_Army[ST,CKind,WType].Chart.Font := fnt_Metal; //fnt_Outline doesn't work because player names blend badly with yellow
          Charts_Army[ST,CKind,WType].Chart.Hide;
        end;

    Label_NoArmyData := TKMLabel.Create(Panel_ChartsArmy, 450, CHART_HEIGHT div 2, gResTexts[TX_GRAPH_NO_DATA], fnt_Metal, taCenter);

    Panel_ChartArmy_Type := TKMPanel.Create(Panel_ChartsArmy, 755, CHART_HEIGHT - ARMY_TYPE_HEIGHT - 20, 150, ARMY_TYPE_HEIGHT);
      with TKMShape.Create(Panel_ChartArmy_Type, 0, 0, 150, ARMY_TYPE_HEIGHT) do
      begin
        FillColor := icDarkestGrayTrans;
        LineColor := icGray;
        LineWidth := 1;
      end;

      TKMLabel.Create(Panel_ChartArmy_Type, 5, 8, 140, 20, 'Chart type', fnt_Metal, taCenter); // Todo translate

      Radio_ChartArmyType := TKMRadioGroup.Create(Panel_ChartArmy_Type,5,35,140,ARMY_TYPE_HEIGHT - 40,fnt_Grey);
      Radio_ChartArmyType.DrawChkboxOutline := True;
      Radio_ChartArmyType.ItemIndex := 0;
      Radio_ChartArmyType.Add('Instantaneous');   // Todo translate
      Radio_ChartArmyType.Add('Total equipped');  // Todo translate
      Radio_ChartArmyType.Add('Defeated');        // Todo translate
      Radio_ChartArmyType.Add('Lost');            // Todo translate
      Radio_ChartArmyType.OnChange := RadioArmyTypeChange;
end;


procedure TKMMenuResultsMP.RadioArmyTypeChange(Sender: TObject);
begin
  ArmyUpdate(nil);
end;


procedure TKMMenuResultsMP.StatTypeChange(Sender: TObject);
begin
  if Sender = Button_Players then
    fStatType := st_ByPlayers;

  if Sender = Button_Teams then
    fStatType := st_ByTeams;

  Button_Players.Down := fStatType = st_ByPlayers;
  Button_Teams.Down   := fStatType = st_ByTeams;

  UpdateVisibleTab;
end;


procedure TKMMenuResultsMP.UpdateVisibleTab;
begin
  if Panel_Bars.Visible then
    BarsUpdate(fStatType);
  if Panel_ChartsEconomy.Visible then
    EconomyUpdate(nil);
  if Panel_ChartsWares.Visible then
    WareUpdate(nil);
  if Panel_ChartsArmy.Visible then
    ArmyUpdate(nil);
end;


procedure TKMMenuResultsMP.TabChange(Sender: TObject);
begin
  Button_MPResultsBars.Down := Sender = Button_MPResultsBars;
  Button_MPResultsArmy.Down := Sender = Button_MPResultsArmy;
  Button_MPResultsEconomy.Down := Sender = Button_MPResultsEconomy;
  Button_MPResultsWares.Down := Sender = Button_MPResultsWares;

  Panel_Bars.Visible          := Sender = Button_MPResultsBars;
  Panel_ChartsEconomy.Visible := Sender = Button_MPResultsEconomy;
  Panel_ChartsWares.Visible   := Sender = Button_MPResultsWares;
  Panel_ChartsArmy.Visible    := Sender = Button_MPResultsArmy;

  StatTypeChange(nil);
end;


procedure TKMMenuResultsMP.BarsUpdate(aStatType: TKMStatType);
const
  STATS_LOWER_IS_BETTER: set of Byte = [1,3,6];

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
  I,J,K: Integer;
  UnitsMax, HousesMax, WaresMax, WeaponsMax, MaxValue: Integer;
  Bests: array [0..9] of Cardinal;
  Totals: array [0..9] of Cardinal;
  StatValue: Cardinal;
  ListToShow, HandsInOne: TStringList;
  StatsValues: TKMStatsValues;
begin
  ListToShow := fListToShow[aStatType];
  StatsValues := fStatsValues[aStatType];

  //Update visibility depending on players count (note, players may be sparsed)
  for I := 0 to MAX_LOBBY_PLAYERS - 1 do
    SetPlayerControls(I, False); //Disable them all to start

  for I := 0 to ListToShow.Count - 1 do
  begin
    SetPlayerControls(I, True); //Enable used ones

    Label_ResultsPlayerName1[I].Caption   := fNamesToShow[fStatType, I];
    Label_ResultsPlayerName1[I].FontColor := FlagColorToTextColor(fColorsToShow[fStatType, I]);
    Label_ResultsPlayerName2[I].Caption   := Label_ResultsPlayerName1[I].Caption;
    Label_ResultsPlayerName2[I].FontColor := Label_ResultsPlayerName1[I].FontColor;
  end;

  //Update positioning
  Panel_BarsUpper.Height := 40 + fListToShow[st_ByPlayers].Count * BAR_ROW_HEIGHT;
  Panel_BarsLower.Height := 40 + fListToShow[st_ByPlayers].Count * BAR_ROW_HEIGHT;

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
  for I := 0 to ListToShow.Count - 1 do
  begin
    K := StrToInt(TStringList(ListToShow.Objects[I])[0]);
    for J := 0 to 9 do
    begin
      StatValue := StatsValues[K,J];
      if J in STATS_LOWER_IS_BETTER then
      begin
        if Bests[J] > StatValue then Bests[J] := StatValue;
      end else
        if Bests[J] < StatValue then Bests[J] := StatValue;

      Inc(Totals[J], StatValue);
    end;
  end;

  //Fill in raw values
  for I := 0 to ListToShow.Count - 1 do
  begin
    K := StrToInt(TStringList(ListToShow.Objects[I])[0]);
    for J := 0 to 9 do
    begin
      StatValue := StatsValues[K,J];
      Bar_Results[I,J].Tag := StatValue;
      if J in STATS_LOWER_IS_BETTER then
      begin
        Image_ResultsRosette[I,J].Visible := (StatValue <= Bests[J]) and (Totals[J] > 0);
      end else
        Image_ResultsRosette[I,J].Visible := (StatValue >= Bests[J]) and (Totals[J] > 0);
    end;
  end;

  //Update percent bars for each category
  UnitsMax := 0;
  for K := 0 to 4 do for I := 0 to ListToShow.Count - 1 do
    UnitsMax := Max(Bar_Results[I,K].Tag, UnitsMax);

  HousesMax := 0;
  for K := 5 to 7 do for I := 0 to ListToShow.Count - 1 do
    HousesMax := Max(Bar_Results[I,K].Tag, HousesMax);

  WaresMax := 0;
  for I := 0 to ListToShow.Count - 1 do
    WaresMax := Max(Bar_Results[I,8].Tag, WaresMax);

  WeaponsMax := 0;
  for I := 0 to ListToShow.Count - 1 do
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
    for I := 0 to ListToShow.Count - 1 do
    begin
      if MaxValue <> 0 then
        Bar_Results[I,K].Position := Bar_Results[I,K].Tag / MaxValue
      else
        Bar_Results[I,K].Position := 0;
      Bar_Results[I,K].Caption := IfThen(Bar_Results[I,K].Tag <> 0, IntToStr(Bar_Results[I,K].Tag), '-');
    end;
  end;
end;


procedure TKMMenuResultsMP.EconomyUpdate(Sender: TObject);
begin
  //Hide everything first
  Chart_Players_Houses.Hide;
  Chart_Players_Citizens.Hide;
  Chart_Teams_Houses.Hide;
  Chart_Teams_Citizens.Hide;

  if fNoEconomyChartData then
  begin
    Label_NoEconomyData.Show;
    Panel_ChartEconomy_Type.Hide;
    Exit;
  end;

  Label_NoEconomyData.Hide;
  Panel_ChartEconomy_Type.Show;

  //Then show what is needed
  case Radio_ChartEconomyType.ItemIndex of
    0:  if fStatType = st_ByPlayers then
          Chart_Players_Citizens.Show
        else
          Chart_Teams_Citizens.Show;
    1:  if fStatType = st_ByPlayers then
          Chart_Players_Houses.Show
        else
          Chart_Teams_Houses.Show;
  end;
end;


procedure TKMMenuResultsMP.WareUpdate(Sender: TObject);

  procedure ChangeWareChart(aChart: TKMChart; aUseGDP: Boolean);
    procedure CheckChart(aChartToFind: TKMChart);
    var
      I: Integer;
    begin
      //Remember which lines were visible
      if aChartToFind.Visible then
        for I := 0 to aChartToFind.LineCount - 1 do
          fPlayersVisibleWares[aChartToFind.Lines[I].Tag] := aChartToFind.Lines[I].Visible;

      aChartToFind.Visible := False;
    end;
  var
    W: TWareType;
    I: Integer;
    ST: TKMStatType;
  begin
    for ST := Low(TKMStatType) to High(TKMStatType) do
    begin
      //Find and hide old chart
      for I := Low(GDPWares) to High(GDPWares) do
        CheckChart(Charts_WaresGDP[ST,I]);

      for W := Low(TWareType) to High(TWareType) do
        CheckChart(Charts_Wares[ST,W]);
    end;

    aChart.Visible := True;

    //Restore previously visible lines
    for I := 0 to aChart.LineCount - 1 do
      aChart.SetLineVisible(I, fPlayersVisibleWares[aChart.Lines[I].Tag]);
  end;

var
  K, WareInGdpI: Integer;
  W: TWareType;
  ST: TKMStatType;
begin
  //Hide everything if no selection in columnbox
  if not Columnbox_Wares.IsSelected
    or not Columnbox_WaresGDP.IsSelected then
  begin
    Label_NoWareData.Show;
    Columnbox_Wares.Hide;
    Columnbox_WaresGDP.Hide;
    Panel_ChartWare_Type.Hide;
    for ST := Low(TKMStatType) to High(TKMStatType) do
    begin
      for W := Low(TWareType) to High(TWareType) do
        Charts_Wares[ST,W].Hide;
      for K := Low(GDPWares) to High(GDPWares) do
        Charts_WaresGDP[ST,K].Hide;
    end;
    Exit;
  end;

  Label_NoWareData.Hide;
  Panel_ChartWare_Type.Show;

  case Radio_ChartWareType.ItemIndex of
    0:  begin // Quantity chart
          if (Sender = Radio_ChartWareType) and Columnbox_WaresGDP.IsSelected then
            Columnbox_Wares.ItemIndex := Columnbox_WaresGDP.ItemIndex;

          W := TWareType(Columnbox_Wares.Rows[Columnbox_Wares.ItemIndex].Tag);
          ChangeWareChart(Charts_Wares[fStatType, W], False);

          Columnbox_WaresGDP.Hide;
          Columnbox_Wares.Show;
        end;
    1:  begin // GDP chart
          if (Sender = Radio_ChartWareType) then
          begin
            W := TWareType(Columnbox_Wares.Rows[Columnbox_Wares.ItemIndex].Tag);
            WareInGdpI := GetWareIdInGDPArr(W);
            if Columnbox_Wares.IsSelected and InRange(WareInGdpI, 0, 2) then
              Columnbox_WaresGDP.ItemIndex := Columnbox_Wares.ItemIndex
            else
              Columnbox_WaresGDP.ItemIndex := 0;
          end;

          ChangeWareChart(Charts_WaresGDP[fStatType, Columnbox_WaresGDP.ItemIndex], True);

          Columnbox_Wares.Hide;
          Columnbox_WaresGDP.Show;
        end;
  end;
end;


function TKMMenuResultsMP.GetSelectedChartArmyKind: TKMChartArmyKind;
begin
  Result := TKMChartArmyKind(Radio_ChartArmyType.ItemIndex);
end;


procedure TKMMenuResultsMP.ArmyUpdate(Sender: TObject);
var
  K: Integer;
  SelectedWType, WType: TKMChartWarriorType;
  SelectedCKind,CKind: TKMChartArmyKind;
  ST: TKMStatType;
  Chart: PKMChart;
  SelectedItemTag: Integer;
begin
  //Hide everything if there is no data
  if fNoArmyChartData then
  begin
    Label_NoArmyData.Show;
    Columnbox_Army.Hide;
    Panel_ChartArmy_Type.Hide;
    for ST := Low(TKMStatType) to High(TKMStatType) do
      for CKind := Low(TKMChartArmyKind) to High(TKMChartArmyKind) do
        for WType := Low(TKMChartWarriorType) to High(TKMChartWarriorType) do
          Charts_Army[ST,CKind,WType].Chart.Hide;
    Exit;
  end;

  SelectedItemTag := -1;
  if Columnbox_Army.IsSelected then
    SelectedItemTag := Columnbox_Army.SelectedItem.Tag;

  SelectedCKind := GetSelectedChartArmyKind;

  //Fill columnbox for selected CKind
  Columnbox_Army.Clear;
  for K := 0 to High(fColumnBoxArmy_Rows[SelectedCKind]) do
  begin
    WType := fColumnBoxArmy_Rows[SelectedCKind,K];
    Columnbox_Army.AddItem(MakeListRow(['', Charts_Army[fStatType,SelectedCKind,WType].ChartType.GUIName],   //Does not matter what chart to use - they all have same GUIName and GUIIcon
                                        [fHandFlagColor, $FFFFFFFF],
                                        [MakePic(rxGui, Charts_Army[fStatType,SelectedCKind,WType].ChartType.GUIIcon), MakePic(rxGui, 0)],
                                        Byte(WType)));
    if SelectedItemTag = Byte(WType) then
      Columnbox_Army.ItemIndex := Columnbox_Army.RowCount - 1;
  end;

  if not Columnbox_Army.IsSelected then
    Columnbox_Army.ItemIndex := 0;  //Select 1st elem in column box, there should be always ArmyPower

  //Show columnbox and chart panel
  Label_NoArmyData.Hide;
  Columnbox_Army.Show;
  Panel_ChartArmy_Type.Show;

  //Find and hide old chart
  for ST := Low(TKMStatType) to High(TKMStatType) do
    for CKind := Low(TKMChartArmyKind) to High(TKMChartArmyKind) do
      //Remember which lines were visible
      for WType := Low(TKMChartWarriorType) to High(TKMChartWarriorType) do
      begin
        Chart := @Charts_Army[ST,CKind,WType].Chart;
        if Chart^.Visible then
          for K := 0 to Chart^.LineCount - 1 do
            fPlayersVisibleArmy[Chart^.Lines[K].Tag] := Chart^.Lines[K].Visible;
        Chart^.Visible := False;
      end;

  SelectedWType := TKMChartWarriorType(Columnbox_Army.Rows[Columnbox_Army.ItemIndex].Tag);

  Chart := @Charts_Army[fStatType,SelectedCKind,SelectedWType].Chart;
  Chart^.Visible := True;
  //Restore previously visible lines
  for K := 0 to Chart^.LineCount - 1 do
    Chart^.SetLineVisible(K, fPlayersVisibleArmy[Chart^.Lines[K].Tag]);

end;


function TKMMenuResultsMP.DoShowHandStats(aHandId: Integer): Boolean;
begin
  Result := gHands[aHandId].Enabled and (fShowAIResults or gHands[aHandId].IsHuman);
end;


procedure TKMMenuResultsMP.RecreateListToShow(aStatType: TKMStatType);
begin
  if fListToShow[aStatType] <> nil then
    FreeAndNil(fListToShow[aStatType]);

  fListToShow[aStatType] := TStringList.Create;
  fListToShow[aStatType].Sorted := False;     //Need to append players to show at the end of list
  fListToShow[aStatType].OwnsObjects := True; //Contains other TStringList'
end;


function TKMMenuResultsMP.DoAdjoinSameColorHand(aHandId: Integer): Boolean;
begin
  Result := gHands[aHandId].IsComputer and (gGame.GameMode in [gmSingle, gmCampaign, gmReplaySingle]); //Adjoin only AI's in SP games
end;


procedure TKMMenuResultsMP.ReinitPlayersToShow;
var
  HandsUniqueColorsCnt: Integer;
  HandsUniqueColors: array[0..MAX_HANDS-1] of record
                                                HandId: Integer;
                                                Color: Cardinal;
                                              end;

  procedure AddOrFindHandColor(aHandId: Integer; aColor: Cardinal; var aOldI: Integer);
  var
    I: Integer;
  begin
    aOldI := -1;
    for I := 0 to HandsUniqueColorsCnt - 1 do
      if HandsUniqueColors[I].Color = aColor then
      begin
        aOldI := HandsUniqueColors[I].HandId;
        Exit;
      end;
    HandsUniqueColors[HandsUniqueColorsCnt].Color := aColor;
    HandsUniqueColors[HandsUniqueColorsCnt].HandId := aHandId;
    Inc(HandsUniqueColorsCnt);
  end;

  procedure TryAddHand(aHandId: TKMHandIndex);
    procedure AddNewHand;
    var PlayersIdList: TStringList;
    begin
      PlayersIdList := TStringList.Create;
      PlayersIdList.Add(IntToStr(aHandId));
      fListToShow[st_ByPlayers].AddObject(IntToStr(aHandId), PlayersIdList);
      fNamesToShow[st_ByPlayers, fListToShow[st_ByPlayers].Count - 1] := GetOwnerName(aHandId);
      fColorsToShow[st_ByPlayers, fListToShow[st_ByPlayers].Count - 1] := gHands[aHandId].FlagColor;
    end;

    procedure AddOldHand(aOldI: Integer);
    var ListI: Integer;
    begin
      ListI := fListToShow[st_ByPlayers].IndexOf(IntToStr(aOldI));
      if ListI > -1 then //should be always true
        TStringList(fListToShow[st_ByPlayers].Objects[ListI]).Add(IntToStr(aHandId)); //Add same color player to list
    end;
  var
    OldI: Integer;
  begin
    if DoShowHandStats(aHandId) then
    begin
      if DoAdjoinSameColorHand(aHandId) then // For SP game we have to adjoin same color hands
      begin
        AddOrFindHandColor(aHandId, gHands[aHandId].FlagColor, OldI); //Same colored players are adjoined into 1
        if OldI = -1 then
          AddNewHand
        else
          AddOldHand(OldI);
      end else
        AddNewHand;
    end;
  end;

var
  I, J, PlayersCntBeforeAdd: Integer;
  Teams: TKMByteSetArray;
  NonTeamHands: set of Byte;
begin
  Teams := gHands.GetTeams;
  NonTeamHands := [0..gHands.Count - 1];

  //Get non team hands
  for I := Low(Teams) to High(Teams) do
    NonTeamHands := NonTeamHands - Teams[I];

  RecreateListToShow(st_ByPlayers);
  HandsUniqueColorsCnt := 0;

  for I in NonTeamHands do
    TryAddHand(I);

  fChartSeparatorsPos[st_ByPlayers].Clear;
  for I := Low(Teams) to High(Teams) do
  begin
    PlayersCntBeforeAdd := fListToShow[st_ByPlayers].Count;
    for J in Teams[I] do
      TryAddHand(J);
    // Add separator position
    if (PlayersCntBeforeAdd > 0)                            // Do not add separator at first pos
      and (PlayersCntBeforeAdd < fListToShow[st_ByPlayers].Count) then // Do not separator if team is 'empty'
      fChartSeparatorsPos[st_ByPlayers].Add(IntToStr(PlayersCntBeforeAdd));
  end;
end;


procedure TKMMenuResultsMP.ReinitTeamsToShow;
  procedure AddToNewTeam(aFirstHandId: Integer; aPlayersList: TStringList);
  var
    PlayersList: TStringList;
  begin
    PlayersList := TStringList.Create;
    PlayersList.AddStrings(aPlayersList);
    fListToShow[st_ByTeams].AddObject(IntToStr(aFirstHandId), PlayersList);
    fNamesToShow[st_ByTeams, fListToShow[st_ByTeams].Count - 1] := gResTexts[TX_LOBBY_HEADER_TEAM] + ' ' + IntToStr(fListToShow[st_ByTeams].Count);
    fColorsToShow[st_ByTeams, fListToShow[st_ByTeams].Count - 1] := gHands[aFirstHandId].FlagColor;
  end;

  procedure AddToTeam(aTeamI: Integer; aPlayersList: TStringList);
  var
    PlayersList: TStringList;
  begin
    PlayersList := TStringList(fListToShow[st_ByTeams].Objects[aTeamI]);
    PlayersList.AddStrings(aPlayersList);
  end;

var
  I,J,HandId,TeamI: Integer;
  PlayersList: TStringList;
  Teams: TKMByteSetArray;
  TeamIsNew: Boolean;
begin
  RecreateListToShow(st_ByTeams);
  fChartSeparatorsPos[st_ByTeams].Clear;
  Teams := gHands.GetFullTeams;
  TeamI := 0;

  for J := Low(Teams) to High(Teams) do
  begin
    TeamIsNew := True;
    for I := 0 to fListToShow[st_ByPlayers].Count - 1 do
    begin
      PlayersList := TStringList(fListToShow[st_ByPlayers].Objects[I]);
      HandId := StrToInt(PlayersList[0]);

      if HandId in Teams[J] then
      begin
        if TeamIsNew then
        begin
          TeamIsNew := False;
          AddToNewTeam(HandId, PlayersList);
        end else
          AddToTeam(TeamI, PlayersList);
      end;
    end;
    if not TeamIsNew then //check if there was empty teams
      Inc(TeamI);

    if J <> Low(Teams) then
      fChartSeparatorsPos[st_ByTeams].Add(IntToStr(J));
  end;
end;


procedure TKMMenuResultsMP.Reinit;
var
  I: Integer;
  BackCaption: String;
begin
  //Back button has different captions depending on where it returns us to
  case fGameResultMsg of
    gr_ReplayEnd: BackCaption := gResTexts[TX_RESULTS_BACK_REPLAYS];
    gr_ShowStats: BackCaption := 'Back to Game results'; //Todo translate
    else          BackCaption := gResTexts[TX_RESULTS_BACK_MP];
  end;
  Button_ResultsMPBack.Caption := BackCaption;

  if fIsStatsRefreshed then Exit;

  fHandFlagColor := gMySpectator.Hand.FlagColor;

  //MP Stats can be shown from SP stats page. We have to hide AI players then, depending on game result
  fShowAIResults := not (gGame.GameMode in [gmSingle, gmCampaign]) or (fGameResultMsg in [gr_Win, gr_ReplayEnd]);

  // When exit mission update stats to build actual charts
  // without CHARTS_SAMPLING_FOR_TACTICS or CHARTS_SAMPLING_FOR_ECONOMY delays
  // so measurements for warriors/goods produces will not differ from charts
  for I := 0 to gHands.Count - 1 do
    if DoShowHandStats(I) then
      gHands[I].Stats.UpdateState;

  case fGameResultMsg of
    gr_Win:       fGameResultCaption := gResTexts[TX_MENU_MISSION_VICTORY];
    gr_Defeat:    fGameResultCaption := gResTexts[TX_MENU_MISSION_DEFEAT];
    gr_Cancel:    fGameResultCaption := gResTexts[TX_MENU_MISSION_CANCELED];
    gr_ReplayEnd: fGameResultCaption := gResTexts[TX_MENU_REPLAY_ENDED];
    gr_ShowStats: ;// Do not change game result caption, as it was set to actual game result already
    else          fGameResultCaption := NO_TEXT;
  end;
  //Append mission name and time after the result message
  Label_ResultsMP.Caption := fGameResultCaption + ' - ' + gGame.GameName + ' - ' + TimeToString(gGame.MissionTime);

  Button_Players.Down := True;
  Button_Teams.Down := False;

  for I := 0 to MAX_HANDS - 1 do
  begin
    fPlayersVisibleWares[I] := True;
    fPlayersVisibleArmy[I] := True;
  end;

  fStatType := st_ByPlayers;

  ReinitPlayersToShow;
  ReinitTeamsToShow; //Shuld be done after ReinitPlayersToShow
  ReinitBars;
  ReinitChartEconomy;
  ReinitChartWares;
  ReinitChartArmy;

  Button_MPResultsWares.Enabled := (gGame.MissionMode = mm_Normal);
  Button_MPResultsEconomy.Enabled := (gGame.MissionMode = mm_Normal);

  Button_Teams.Enabled := fListToShow[st_ByPlayers].Count > fListToShow[st_ByTeams].Count; // Disable 'by Teams' btn for FFA game

  //Show first tab
  TabChange(Button_MPResultsBars);
end;


procedure TKMMenuResultsMP.ReinitBars;

  function GetStatValue(aHandId, aStatId: Integer): Cardinal;
  begin
    with gHands[aHandId].Stats do
      case aStatId of
        0: Result := GetCitizensTrained;
        1: Result := GetCitizensLost;
        2: Result := GetWarriorsTrained;
        3: Result := GetWarriorsLost;
        4: Result := GetWarriorsKilled;
        5: Result := GetHousesBuilt;
        6: Result := GetHousesLost;
        7: Result := GetHousesDestroyed;
        8: Result := GetCivilProduced;
        9: Result := GetWarfareProduced;
        else raise Exception.Create('Unknown stat id = ' + IntToStr(aStatId));
      end;
  end;

  procedure CollectStats(aStatType: TKMStatType);
  var
    I,J,K,HandId: Integer;
    ListToShow,HandsInOne: TStringList;
  begin
    ListToShow := fListToShow[aStatType];
    //Update visibility depending on players count (note, players may be sparsed)
    for I := 0 to MAX_LOBBY_PLAYERS - 1 do
      FillChar(fStatsValues[aStatType,I], SizeOf(fStatsValues[aStatType,I]), #0);

    for I := 0 to ListToShow.Count - 1 do
    begin
      HandsInOne := TStringList(ListToShow.Objects[I]); //hands, that has to be shown as one statistics value

      for K := 0 to HandsInOne.Count - 1 do
      begin
        HandId := StrToInt(HandsInOne[K]);
        for J := 0 to 9 do
          Inc(fStatsValues[aStatType,StrToInt(HandsInOne[0]),J], GetStatValue(HandId,J)); // Adjoin data to 1st Hand in SameColorHands list
      end;
    end;
  end;
var
  ST: TKMStatType;
begin
  for ST := Low(TKMStatType) to High(TKMStatType) do
    CollectStats(ST);
end;


function GetEconomyStatsData(aHandId: Integer; aEcoStatKind: TKMEconomyStatKind): PKMCardinalArray;
begin
  case aEcoStatKind of
    est_Citizens: Result := @gHands[aHandId].Stats.ChartCitizens;
    est_Houses:   Result := @gHands[aHandId].Stats.ChartHouses;
    else          raise Exception.Create('Unknown EconomyStatKind');
  end;
end;


procedure TKMMenuResultsMP.ReinitChartEconomy;

  function GetEconomyChart(aStatType: TKMStatType; aEcoStatKind: TKMEconomyStatKind): PKMChart;
  begin
    case aStatType of
      st_ByPlayers: if aEcoStatKind = est_Citizens then
                      Result := @Chart_Players_Citizens
                    else
                      Result := @Chart_Players_Houses;
      st_ByTeams:   if aEcoStatKind = est_Citizens then
                      Result := @Chart_Teams_Citizens
                    else
                      Result := @Chart_Teams_Houses;
      else          raise Exception.Create('Unknown Economy Chart type');
    end;
  end;

  procedure InitChart(aStatType: TKMStatType; aEcoStatKind: TKMEconomyStatKind);
  var
    Chart: PKMChart;
  begin
    Chart := GetEconomyChart(aStatType, aEcoStatKind);
    Chart^.Clear;
    Chart^.MaxLength := 0;
    Chart^.MaxTime   := gGame.GameTickCount div 10;
    Chart^.Peacetime := 60*gGame.GameOptions.Peacetime;
    Chart^.SetSeparatorPositions(fChartSeparatorsPos[aStatType]);
  end;

  procedure FillChart(aStatType: TKMStatType; aEcoStatKind: TKMEconomyStatKind);
  var
    I, J, HandId: Integer;
    PlayersList, ListToShow: TStringList;
    ChartData: TKMCardinalArray;
    Chart: PKMChart;
  begin
    ListToShow := fListToShow[aStatType];
    Chart := GetEconomyChart(aStatType, aEcoStatKind);
    for I := 0 to ListToShow.Count - 1 do
    begin
      SetLength(ChartData, 0);
      PlayersList := TStringList(ListToShow.Objects[I]);

      for J := 0 to PlayersList.Count - 1 do
      begin
        HandId := StrToInt(PlayersList[J]);
        KMSummAndEnlargeArr(@ChartData, GetEconomyStatsData(HandId, aEcoStatKind));
      end;

      if fNoEconomyChartData then
        for J := 0 to Length(ChartData) - 1 do
          if ChartData[J] <> 0 then
          begin
            fNoEconomyChartData := False;
            Break;
          end;

      HandId := StrToInt(PlayersList[0]);
      Chart^.MaxLength := Max(Chart^.MaxLength, gHands[HandId].Stats.ChartCount);
      Chart^.AddLine(fNamesToShow[aStatType, I], fColorsToShow[aStatType, I], ChartData);
    end;

  end;

var
  ST: TKMStatType;
  ESK: TKMEconomyStatKind;
begin
  fNoEconomyChartData := True;
  for ST := Low(TKMStatType) to High(TKMStatType) do
    for ESK := Low(TKMEconomyStatKind) to High(TKMEconomyStatKind) do
    begin
      InitChart(ST, ESK);
      FillChart(ST, ESK);
    end;
end;


procedure TKMMenuResultsMP.ReinitChartWares;
const
  WARES_CNT = 31;

  Wares: array [0..WARES_CNT-1] of TWareType = (
    wt_All,     wt_Warfare, wt_Food,
    wt_Trunk,   wt_Stone,   wt_Wood,        wt_IronOre,   wt_GoldOre,
    wt_Coal,    wt_Steel,   wt_Gold,        wt_Wine,      wt_Corn,
    wt_Bread,   wt_Flour,   wt_Leather,     wt_Sausages,  wt_Pig,
    wt_Skin,    wt_Shield,  wt_MetalShield, wt_Armor,     wt_MetalArmor,
    wt_Axe,     wt_Sword,   wt_Pike,        wt_Hallebard, wt_Bow,
    wt_Arbalet, wt_Horse,   wt_Fish);

  procedure RefreshChart(aStatType: TKMStatType; W: TWareType; aChart: PKMChart; aUseGDP: Boolean);
  var
    I, K, HandId: Integer;
    PlayersList: TStringList;
    ChartData, ChartWaresData: TKMCardinalArray;
  begin
    aChart^.Clear;
    aChart^.MaxLength := 0;
    aChart^.MaxTime   := gGame.GameTickCount div 10;
    aChart^.Peacetime := 60*gGame.GameOptions.Peacetime;
    aChart^.SetSeparatorPositions(fChartSeparatorsPos[aStatType]);

    if aUseGDP then
      aChart^.Caption   := gRes.Wares[W].Title + ' - ' + 'GDP' //Todo translate
    else
      aChart^.Caption   := gRes.Wares[W].Title + ' - ' + gResTexts[TX_GRAPH_TITLE_RESOURCES];

    for I := 0 to fListToShow[aStatType].Count - 1 do
    begin
      SetLength(ChartData, 0);
      PlayersList := TStringList(fListToShow[aStatType].Objects[I]);

      for K := 0 to PlayersList.Count - 1 do
      begin
        HandId := StrToInt(PlayersList[K]);
        ChartWaresData := GetChartWares(HandId, W, aUseGDP);
        KMSummAndEnlargeArr(@ChartData, @ChartWaresData);
      end;

//      HandId := StrToInt(PlayersList[0]);
//      with gHands[HandId] do
//      begin
//        aChart^.MaxLength := Max(aChart^.MaxLength, Stats.ChartCount);
//        aChart^.AddLine(GetOwnerName(HandId), FlagColor, ChartData, I);
//      end;

      HandId := StrToInt(PlayersList[0]);
      aChart^.MaxLength := Max(aChart^.MaxLength, gHands[HandId].Stats.ChartCount);
      aChart^.AddLine(fNamesToShow[aStatType, I], fColorsToShow[aStatType, I], ChartData, I);
    end;
  end;

var
  I,K,J,WareInGDP: Integer;
  W: TWareType;
  ListRow: TKMListRow;
  ST: TKMStatType;
begin
  Radio_ChartWareType.ItemIndex := 0;

  //Prepare columnboxes
  Columnbox_Wares.Clear;
  Columnbox_WaresGDP.Clear;
  for I := Low(Wares) to High(Wares) do
  begin
    W := Wares[I];
    for K := 0 to gHands.Count - 1 do
    if DoShowHandStats(K)
      and not gHands[K].Stats.ChartWaresEmpty(W) then
    begin
      ListRow := MakeListRow(['', gRes.Wares[W].Title],
                             [$FFFFFFFF, $FFFFFFFF],
                             [MakePic(rxGui, gRes.Wares[W].GUIIcon), MakePic(rxGui, 0)],
                             Byte(W));
      Columnbox_Wares.AddItem(ListRow);
      WareInGDP := GetWareIdInGDPArr(W);
      if WareInGDP <> -1 then
        Columnbox_WaresGDP.AddItem(ListRow);
      Break;
    end;
  end;

  //Fill in chart values
  for ST := Low(TKMStatType) to High(TKMStatType) do
    for J := 0 to Columnbox_Wares.RowCount - 1 do
    begin
      W := TWareType(Columnbox_Wares.Rows[J].Tag);

      RefreshChart(ST, W, @Charts_Wares[ST, W], False);
      WareInGDP := GetWareIdInGDPArr(W);
      if WareInGDP <> -1 then
        RefreshChart(ST, W, @Charts_WaresGDP[ST, WareInGDP], True);
    end;

  Columnbox_Wares.ItemIndex := 0;
  Columnbox_Wares.ItemHeight := Min(Columnbox_Wares.Height div 15, 20);
  Columnbox_WaresGDP.ItemIndex := 0;
  Columnbox_WaresGDP.ItemHeight := Min(Columnbox_WaresGDP.Height div 15, 20);
  WareUpdate(nil);
end;


procedure TKMMenuResultsMP.ReinitChartArmy;
const
  CHART_ARMY_CAPTION: array[TKMChartArmyKind] of String = ('Instantaneous','Total equipped','Defeated','Lost');
var
  I,J, HandId: Integer;
  PlayersList: TStringList;
  ChartData, ChartArmyData: TKMCardinalArray;
  WType: TKMChartWarriorType;
  CKind: TKMChartArmyKind;
  ST: TKMStatType;
  Chart: PKMChart;
  ChartArmy: PKMChartArmyMP;
begin
  fNoArmyChartData := True;
  Radio_ChartArmyType.ItemIndex := 0;

  for CKind := Low(TKMChartArmyKind) to High(TKMChartArmyKind) do
  begin
    SetLength(fColumnBoxArmy_Rows[CKind], Integer(High(TKMChartWarriorType)) + 1); // Set max length for columnbox rows for all chart kinds
    I := 0;
    //Fill columnbox rows for every CKind. We have to do it now, when gHands is not free'd yet
    for WType := Low(TKMChartWarriorType) to High(TKMChartWarriorType) do
    begin
      for J := 0 to gHands.Count - 1 do
        if DoShowHandStats(J)
          and ((WType = cwt_ArmyPower)   // Always add ArmyPower chart, even if its empty
            or not Charts_Army[fStatType,CKind,WType].IsEmpty(J)) then
        begin
          fColumnBoxArmy_Rows[CKind, I] := WType;
          Inc(I);
          if WType <> cwt_ArmyPower then
            fNoArmyChartData := False;
          Break; // Found warriors data for at least 1 hand, that's enought to show warrior type in column box
        end;
    end;
    SetLength(fColumnBoxArmy_Rows[CKind], I); //Cut unused elements, so we will show only needed lines in ArmyChange
  end;

  //Fill in chart values
  for ST := Low(TKMStatType) to High(TKMStatType) do
    for CKind := Low(TKMChartArmyKind) to High(TKMChartArmyKind) do
      for WType := Low(TKMChartWarriorType) to High(TKMChartWarriorType) do
      begin
        ChartArmy := @Charts_Army[ST,CKind,WType];
        Chart := @ChartArmy^.Chart;
        Chart^.Clear;
        Chart^.MaxLength := 0;
        Chart^.MaxTime := gGame.GameTickCount div 10;
        Chart^.Peacetime := 60*gGame.GameOptions.Peacetime;
        Chart^.SetSeparatorPositions(fChartSeparatorsPos[ST]);
        Chart^.Caption := ChartArmy^.ChartType.GUIName + ' - ' + CHART_ARMY_CAPTION[CKind]; // Todo translate

        for I := 0 to fListToShow[ST].Count - 1 do
        begin
          SetLength(ChartData, 0);
          PlayersList := TStringList(fListToShow[ST].Objects[I]);

          for J := 0 to PlayersList.Count - 1 do
          begin
            HandId := StrToInt(PlayersList[J]);
            ChartArmyData := ChartArmy^.GetChartData(HandId);
            KMSummAndEnlargeArr(@ChartData, @ChartArmyData);
          end;

          HandId := StrToInt(PlayersList[0]);
          Chart^.MaxLength := Max(Chart^.MaxLength, gHands[HandId].Stats.ChartCount);
          Chart^.AddLine(fNamesToShow[ST, I], fColorsToShow[ST, I], ChartData, I);
//          HandId := StrToInt(PlayersList[0]);
//          with gHands[HandId] do
//          begin
//            Chart^.MaxLength := Max(Chart^.MaxLength, Stats.ChartCount);
//            Chart^.AddLine(GetOwnerName(HandId), FlagColor, ChartData, HandId);
//          end;
        end;

        Chart^.TrimToFirstVariation; // Trim Army charts, as usually they are same before PeaceTime
      end;

  Columnbox_Army.Clear;
  ArmyUpdate(nil);
end;


procedure TKMMenuResultsMP.Create_ResultsMP(aParent: TKMPanel);
const
  TABS_TOP = 75;
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

    Label_ResultsMP := TKMLabel.Create(Panel_ResultsMP,62,TABS_TOP-30,900,20,NO_TEXT,fnt_Metal,taCenter);
    Label_ResultsMP.Anchors := [anLeft];

    Button_MPResultsBars := TKMButtonFlat.Create(Panel_ResultsMP, 160, TABS_TOP, 176, 20, 8, rxGuiMain);
    Button_MPResultsBars.TexOffsetX := -78;
    Button_MPResultsBars.TexOffsetY := 6;
    Button_MPResultsBars.Anchors := [anLeft];
    Button_MPResultsBars.Caption := gResTexts[TX_RESULTS_STATISTICS];
    Button_MPResultsBars.CapOffsetY := -11;
    Button_MPResultsBars.OnClick := TabChange;

    Button_MPResultsArmy := TKMButtonFlat.Create(Panel_ResultsMP, 340, TABS_TOP, 176, 20, 53, rxGui);
    Button_MPResultsArmy.TexOffsetX := -76;
    Button_MPResultsArmy.TexOffsetY := 6;
    Button_MPResultsArmy.Anchors := [anLeft];
    Button_MPResultsArmy.Caption := gResTexts[TX_GRAPH_ARMY];
    Button_MPResultsArmy.CapOffsetY := -11;
    Button_MPResultsArmy.OnClick := TabChange;

    Button_MPResultsEconomy := TKMButtonFlat.Create(Panel_ResultsMP, 520, TABS_TOP, 176, 20, 589, rxGui);
    Button_MPResultsEconomy.TexOffsetX := -72;
    Button_MPResultsEconomy.TexOffsetY := 6;
    Button_MPResultsEconomy.Anchors := [anLeft];
    Button_MPResultsEconomy.Caption := gResTexts[TX_RESULTS_ECONOMY];
    Button_MPResultsEconomy.CapOffsetY := -11;
    Button_MPResultsEconomy.OnClick := TabChange;

    Button_MPResultsWares := TKMButtonFlat.Create(Panel_ResultsMP, 700, TABS_TOP, 176, 20, 360, rxGui);
    Button_MPResultsWares.TexOffsetX := -77;
    Button_MPResultsWares.TexOffsetY := 6;
    Button_MPResultsWares.Anchors := [anLeft];
    Button_MPResultsWares.Caption := gResTexts[TX_GRAPH_RESOURCES];
    Button_MPResultsWares.CapOffsetY := -11;
    Button_MPResultsWares.OnClick := TabChange;

    Button_Players := TKMButtonFlat.Create(Panel_ResultsMP, 605+62+5, Panel_ResultsMP.Height - BACK_BTN_Y_TO_BOTTOM, 140, 30, 141, rxGui);
    Button_Players.TexOffsetX := -Button_Players.Width div 2 + 12;
    Button_Players.TexOffsetY := 6;
    Button_Players.Anchors := [anLeft];
    Button_Players.Caption := 'by Players'; //Todo translate
    Button_Players.CapOffsetY := -11;
    Button_Players.CapOffsetX := 12;
    Button_Players.OnClick := StatTypeChange;

    Button_Teams := TKMButtonFlat.Create(Panel_ResultsMP, 755+62+5, Panel_ResultsMP.Height - BACK_BTN_Y_TO_BOTTOM, 140, 30, 392, rxGui);
    Button_Teams.TexOffsetX := -Button_Teams.Width div 2 + 20;
    Button_Teams.TexOffsetY := 6;
    Button_Teams.Anchors := [anLeft];
    Button_Teams.Caption := 'by Teams'; //Todo translate
    Button_Teams.CapOffsetY := -11;
    Button_Teams.CapOffsetX := 20;
    Button_Teams.OnClick := StatTypeChange;

    fStatType := st_ByPlayers;

    CreateBars(Panel_ResultsMP);
    CreateChartEconomy;

    CreateChartWares(Panel_ResultsMP);
    CreateChartArmy(Panel_ResultsMP);

    Button_ResultsMPBack := TKMButton.Create(Panel_ResultsMP, 100, Panel_ResultsMP.Height - BACK_BTN_Y_TO_BOTTOM, 280, 30, NO_TEXT, bsMenu);
    Button_ResultsMPBack.Anchors := [anLeft];
    Button_ResultsMPBack.OnClick := BackClick;
end;


function TKMMenuResultsMP.GetChartWares(aPlayer: TKMHandIndex; aWare: TWareType; aUseGDP: Boolean): TKMCardinalArray;
const
  FoodWares: array[0..3] of TWareType = (wt_Bread, wt_Sausages, wt_Wine, wt_Fish);
  FoodWaresRestore: array[0..3] of Single = (BREAD_RESTORE,SAUSAGE_RESTORE,WINE_RESTORE,FISH_RESTORE);
var
  RT: TWareType;
  I,J: Integer;
  TempResult: Single;
begin
  with gHands[aPlayer].Stats do
    case aWare of
      WARE_MIN..WARE_MAX: Result := ChartWares[aWare];
      wt_All:             begin
                            SetLength(Result, ChartCount);
                            for I := 0 to ChartCount - 1 do
                              Result[I] := 0;
                            for I := 0 to ChartCount - 1 do
                            begin
                              TempResult := 0;
                              for RT := WARE_MIN to WARE_MAX do
                                TempResult := TempResult + ChartWares[RT][I] * IfThen(aUseGDP, gRes.Wares[RT].MarketPrice, 1);
                              Result[I] := Round(TempResult);
                            end;
                          end;
      wt_Warfare:         begin
                            SetLength(Result, ChartCount);
                            for I := 0 to ChartCount - 1 do
                              Result[I] := 0;
                            for I := 0 to ChartCount - 1 do
                            begin
                              TempResult := 0;
                              for RT := WARFARE_MIN to WARFARE_MAX do
                                TempResult := TempResult + ChartWares[RT][I] * IfThen(aUseGDP, gRes.Wares[RT].MarketPrice, 1);
                              Result[I] := Round(TempResult);
                            end;
                          end;
      wt_Food:            begin
                            SetLength(Result, ChartCount);
                            for I := 0 to ChartCount - 1 do
                              Result[I] := 0;
                            for I := 0 to ChartCount - 1 do
                            begin
                              TempResult := 0;
                              for J := 0 to 3 do
                              begin
                                RT := FoodWares[J];
                                if aUseGDP then
                                  TempResult := TempResult + ChartWares[RT][I] * gRes.Wares[RT].MarketPrice
                                else
                                  TempResult := TempResult + ChartWares[RT][I] * FoodWaresRestore[J]; //Compute food value according to food types condition restore
                              end;
                              Result[I] := Round(TempResult);
                            end;
                          end;
  end;
end;


procedure TKMMenuResultsMP.Show(aMsg: TGameResultMsg);
begin
  fGameResultMsg := aMsg;

  fIsStatsRefreshed := (aMsg = gr_ShowStats);

  Reinit;
  Panel_ResultsMP.Show;
end;


procedure TKMMenuResultsMP.BackClick(Sender: TObject);
begin
  //Depending on where we were created we need to return to a different place
  //Multiplayer game end   -> ResultsMP -> Multiplayer
  //Multiplayer replay end -> ResultsMP -> Replays
  //Results SP             -> ResultsMP -> ResultsSP
  case fGameResultMsg of
    gr_ReplayEnd: fOnPageChange(gpReplays);
    gr_ShowStats: fOnPageChange(gpResultsSP);
    else          fOnPageChange(gpMultiplayer);
  end;

end;


end.
