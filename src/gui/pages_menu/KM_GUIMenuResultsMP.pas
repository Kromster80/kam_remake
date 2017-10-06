unit KM_GUIMenuResultsMP;
{$I KaM_Remake.inc}
interface
uses
  Controls, Math, StrUtils, SysUtils,
  KM_CommonTypes, KM_Controls, KM_Defaults, KM_Pics,
  KM_InterfaceDefaults, KM_ResWares, KM_HandStats;


type

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
    procedure AddLine(aPlayer: TKMHandIndex; const aTitle: UnicodeString; aColor: Cardinal);
    function IsEmpty(aPlayer: TKMHandIndex): Boolean;
    property Chart: TKMChart read fChart;
    property ChartType: TKMChartWarrior read fType;
  end;

  PKMChartArmyMP = ^TKMChartArmyMP;

  TKMMenuResultsMP = class
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class

    fGameResultMsg: TGameResultMsg; //So we know where to go after results screen
    fPlayersVisibleWares: array [0 .. MAX_HANDS - 1] of Boolean;  //Remember visible players when toggling wares
    fPlayersVisibleArmy: array [0 .. MAX_HANDS - 1] of Boolean;   //Remember visible players when toggling warriors
    fEnabledPlayers: Integer;
    fHandFlagColor: Cardinal;
    fNoArmyChartData: Boolean;
    fColumnBoxArmy_Rows: array[TKMChartArmyKind] of array of TKMChartWarriorType;

    procedure BackClick(Sender: TObject);
    procedure Create_ResultsMP(aParent: TKMPanel);
    procedure CreateBars(aParent: TKMPanel);
    procedure CreateChartWares(aParent: TKMPanel);
    procedure CreateChartArmy(aParent: TKMPanel);

    function GetSelectedChartArmyKind: TKMChartArmyKind;

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
        Chart_MPArmy: array[TKMChartArmyKind] of array[TKMChartWarriorType] of TKMChartArmyMP;
        Label_NoArmyData: TKMLabel;
        Panel_ChartArmy_Type: TKMPanel;
          Radio_ChartArmyStyle: TKMRadioGroup;
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
  if (HasUnitType) then
    Result := gRes.Units[UnitType].GUIName
  else
    Result := 'Army power'; //Todo translate
end;


function TKMChartWarrior.GetGUIIcon: Word;
begin
  if (HasUnitType) then
    Result := gRes.Units[UnitType].GUIIcon
  else
    Result := 53;
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


procedure TKMChartArmyMP.AddLine(aPlayer: TKMHandIndex; const aTitle: UnicodeString; aColor: Cardinal);
var
  ChartData: TKMCardinalArray;
begin
  if (fType.HasUnitType) then
    ChartData := gHands[aPlayer].Stats.ChartArmy[fKind,fType.UnitType]
  else
    ChartData := GetArmyPowerChartData(aPlayer);
  Chart.AddLine(aTitle, aColor, ChartData, aPlayer);
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
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  Create_ResultsMP(aParent);
end;


destructor TKMMenuResultsMP.Destroy;
var
  WType: TKMChartWarriorType;
  CKind: TKMChartArmyKind;
begin
  for CKind := High(TKMChartArmyKind) downto Low(TKMChartArmyKind) do
    for WType := High(TKMChartWarriorType) downto Low(TKMChartWarriorType) do
      FreeAndNil(Chart_MPArmy[CKind,WType]);
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


procedure TKMMenuResultsMP.CreateChartWares(aParent: TKMPanel);
var
  I: TWareType;
begin
  Panel_ChartsWares := TKMPanel.Create(aParent, 62, PANES_TOP, 900, CHART_HEIGHT);
  Panel_ChartsWares.AnchorsCenter;

    Columnbox_Wares := TKMColumnBox.Create(Panel_ChartsWares, 0, 0, 140, CHART_HEIGHT, fnt_Game, bsMenu);
    Columnbox_Wares.SetColumns(fnt_Game, ['', ''], [0, 20]);
    Columnbox_Wares.ShowHeader := False;
    Columnbox_Wares.ShowLines := False;
    Columnbox_Wares.OnChange := WareChange;

    for I := Low(TWareType) to High(TWareType) do
    begin
      Chart_MPWares[I] := TKMChart.Create(Panel_ChartsWares, 140, 0, 900-140, CHART_HEIGHT);
      Chart_MPWares[I].Caption := gResTexts[TX_GRAPH_TITLE_RESOURCES];
      Chart_MPWares[I].Font := fnt_Metal; //fnt_Outline doesn't work because player names blend badly with yellow
      Chart_MPWares[I].Hide;
    end;

    Label_NoWareData := TKMLabel.Create(Panel_ChartsWares, 450, 215, gResTexts[TX_GRAPH_NO_DATA], fnt_Metal, taCenter);
end;


procedure TKMMenuResultsMP.CreateChartArmy(aParent: TKMPanel);
const
  ARMY_TYPE_HEIGHT = 120;
var
  WType: TKMChartWarriorType;
  CKind: TKMChartArmyKind;
begin
  Panel_ChartsArmy := TKMPanel.Create(aParent, 62, PANES_TOP, 900, CHART_HEIGHT);
  Panel_ChartsArmy.AnchorsCenter;

    Columnbox_Army := TKMColumnBox.Create(Panel_ChartsArmy, 0, 0, 140, CHART_HEIGHT, fnt_Game, bsMenu);
    Columnbox_Army.SetColumns(fnt_Game, ['', ''], [0, 30]);
    Columnbox_Army.ShowHeader := False;
    Columnbox_Army.ShowLines := False;
    Columnbox_Army.OnChange := ArmyChange;
    // 33 is a bit more then unit icons max height
    Columnbox_Army.ItemHeight := Min(Columnbox_Army.Height div 13, 33);

    for CKind := Low(TKMChartArmyKind) to High(TKMChartArmyKind) do
      for WType := Low(TKMChartWarriorType) to High(TKMChartWarriorType) do
      begin
        Chart_MPArmy[CKind,WType] := TKMChartArmyMP.Create(WType, CKind, Panel_ChartsArmy, 140, 0, 760, CHART_HEIGHT);
        Chart_MPArmy[CKind,WType].Chart.Caption := gResTexts[TX_GRAPH_ARMY];
        Chart_MPArmy[CKind,WType].Chart.Font := fnt_Metal; //fnt_Outline doesn't work because player names blend badly with yellow
        Chart_MPArmy[CKind,WType].Chart.Hide;
      end;

    Label_NoArmyData := TKMLabel.Create(Panel_ChartsArmy, 450, 215, gResTexts[TX_GRAPH_NO_DATA], fnt_Metal, taCenter);

    Panel_ChartArmy_Type := TKMPanel.Create(Panel_ChartsArmy, 755, CHART_HEIGHT - ARMY_TYPE_HEIGHT - 20, 150, ARMY_TYPE_HEIGHT);
    with TKMShape.Create(Panel_ChartArmy_Type, 0, 0, 150, ARMY_TYPE_HEIGHT) do
    begin
      FillColor := $80303030;
      LineColor := icGray;
      LineWidth := 1;
    end;

    TKMLabel.Create(Panel_ChartArmy_Type, 5, 8, 140, 20, 'Chart type', fnt_Metal, taCenter); // Todo translate

    Radio_ChartArmyStyle := TKMRadioGroup.Create(Panel_ChartArmy_Type,5,35,140,80,fnt_Grey);
    Radio_ChartArmyStyle.DrawChkboxOutline := True;
    Radio_ChartArmyStyle.ItemIndex := 0;
    Radio_ChartArmyStyle.Add('Instantaneous');   // Todo translate
    Radio_ChartArmyStyle.Add('Total equipped');  // Todo translate
    Radio_ChartArmyStyle.Add('Defeated');        // Todo translate
    Radio_ChartArmyStyle.Add('Lost');            // Todo translate
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


function TKMMenuResultsMP.GetSelectedChartArmyKind: TKMChartArmyKind;
begin
  Result := TKMChartArmyKind(Radio_ChartArmyStyle.ItemIndex);
end;


procedure TKMMenuResultsMP.ArmyChange(Sender: TObject);
var
  K: Integer;
  SelectedWType, WType: TKMChartWarriorType;
  SelectedCKind,CKind: TKMChartArmyKind;
  Chart: PKMChart;
  SelectedItemTag: Integer;
begin
  if fNoArmyChartData then
  begin
    Label_NoArmyData.Show;
    Columnbox_Army.Hide;
    Panel_ChartArmy_Type.Hide;
    for CKind := Low(TKMChartArmyKind) to High(TKMChartArmyKind) do
      for WType := Low(TKMChartWarriorType) to High(TKMChartWarriorType) do
        Chart_MPArmy[CKind,WType].Chart.Hide;
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
    Columnbox_Army.AddItem(MakeListRow(['', Chart_MPArmy[SelectedCKind,WType].ChartType.GUIName],   //Does not matter what chart to use - they all have same GUIName and GUIIcon
                                        [fHandFlagColor, $FFFFFFFF],
                                        [MakePic(rxGui, Chart_MPArmy[SelectedCKind,WType].ChartType.GUIIcon), MakePic(rxGui, 0)],
                                        Byte(WType)));
    if SelectedItemTag = Byte(WType) then
      Columnbox_Army.ItemIndex := Columnbox_Army.RowCount - 1;
  end;

  if not Columnbox_Army.IsSelected then
    Columnbox_Army.ItemIndex := 0;

  Label_NoArmyData.Hide;
  Columnbox_Army.Show;
  Panel_ChartArmy_Type.Show;

  //Find and hide old chart
  for CKind := Low(TKMChartArmyKind) to High(TKMChartArmyKind) do
  begin
    //Remember which lines were visible
    for WType := Low(TKMChartWarriorType) to High(TKMChartWarriorType) do
    begin
      Chart := @Chart_MPArmy[CKind,WType].Chart;
      if Chart^.Visible then
        for K := 0 to Chart^.LineCount - 1 do
          fPlayersVisibleArmy[Chart^.Lines[K].Tag] := Chart^.Lines[K].Visible;
      Chart^.Visible := False;
    end;
  end;

  SelectedWType := TKMChartWarriorType(Columnbox_Army.Rows[Columnbox_Army.ItemIndex].Tag);

  Chart := @Chart_MPArmy[SelectedCKind,SelectedWType].Chart;
  Chart^.Visible := True;
  //Restore previously visible lines
  for K := 0 to Chart^.LineCount - 1 do
    Chart^.SetLineVisible(K, fPlayersVisibleArmy[Chart^.Lines[K].Tag]);

end;


procedure TKMMenuResultsMP.Refresh;
var
  I: Integer;
begin
  fHandFlagColor := gMySpectator.Hand.FlagColor;
  // When exit mission update stats to build actual charts
  // without CHARTS_SAMPLING_FOR_TACTICS or CHARTS_SAMPLING_FOR_ECONOMY delays
  // so measurements for warriors/goods produces will not differ from charts
  for I := 0 to gHands.Count - 1 do
    gHands[I].Stats.UpdateState;

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
        if Bests[9] < GetWarfareProduced then Bests[9] := GetWarfareProduced;

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
        Inc(Totals[9], GetWarfareProduced);
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
        Bar_Results[Index,9].Tag := GetWarfareProduced;
        Image_ResultsRosette[Index,5].Visible := (GetHousesBuilt     >= Bests[5]) and (Totals[5] > 0);
        Image_ResultsRosette[Index,6].Visible := (GetHousesLost      <= Bests[6]) and (Totals[6] > 0);
        Image_ResultsRosette[Index,7].Visible := (GetHousesDestroyed >= Bests[7]) and (Totals[7] > 0);
        Image_ResultsRosette[Index,8].Visible := (GetCivilProduced   >= Bests[8]) and (Totals[8] > 0);
        Image_ResultsRosette[Index,9].Visible := (GetWarfareProduced >= Bests[9]) and (Totals[9] > 0);
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
  Chart_MPHouses.MaxLength   := 0;
  Chart_MPCitizens.MaxTime   := gGame.GameTickCount div 10;
  Chart_MPHouses.MaxTime     := gGame.GameTickCount div 10;
  Chart_MPCitizens.Peacetime := 60*gGame.GameOptions.Peacetime;
  Chart_MPHouses.Peacetime   := 60*gGame.GameOptions.Peacetime;

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
    Chart_MPWares[R].MaxTime   := gGame.GameTickCount div 10;
    Chart_MPWares[R].Peacetime := 60*gGame.GameOptions.Peacetime;
    Chart_MPWares[R].Caption   := gResTexts[TX_GRAPH_TITLE_RESOURCES] + ' - ' + gRes.Wares[R].Title;

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
const
  CHART_ARMY_CAPTION: array[TKMChartArmyKind] of String = ('Instantaneous','Total equipped','Defeated','Lost');
var
  I,K,J: Integer;
  WType: TKMChartWarriorType;
  SelectedCKind,CKind: TKMChartArmyKind;
  IsChartEmpty: Boolean;
  Chart: PKMChart;
begin
  fNoArmyChartData := True;
  Radio_ChartArmyStyle.ItemIndex := 0;
  for CKind := Low(TKMChartArmyKind) to High(TKMChartArmyKind) do
  begin
    SetLength(fColumnBoxArmy_Rows[CKind], Integer(High(TKMChartWarriorType)) + 1); // Set max length for columnbox rows for all chart kinds
    I := 0;
    //Fill columnbox rows for every CKind. We have to do it now, when gHands is not free'd yet
    for WType := Low(TKMChartWarriorType) to High(TKMChartWarriorType) do
    begin
      for K := 0 to gHands.Count - 1 do
        if gHands[K].Enabled
          and ((WType = cwt_ArmyPower) or not Chart_MPArmy[CKind,WType].IsEmpty(K)) then // Always add ArmyPower chart, even if its empty
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
  for CKind := Low(TKMChartArmyKind) to High(TKMChartArmyKind) do
    for WType := Low(TKMChartWarriorType) to High(TKMChartWarriorType) do
    begin
      Chart := @Chart_MPArmy[CKind,WType].Chart;
      Chart^.Clear;
      Chart^.MaxLength := 0;
      Chart^.MaxTime := gGame.GameTickCount div 10;
      Chart^.Peacetime := 60*gGame.GameOptions.Peacetime;
      Chart^.Caption := Chart_MPArmy[CKind,WType].ChartType.GUIName + ' - ' + CHART_ARMY_CAPTION[CKind]; // Todo translate

      for I := 0 to gHands.Count - 1 do
        with gHands[I] do
          if Enabled then
          begin
            Chart^.MaxLength := Max(Chart^.MaxLength, Stats.ChartCount);
            Chart_MPArmy[CKind,WType].AddLine(I, OwnerName, FlagColor);
          end;

      Chart^.TrimToFirstVariation;
    end;

  Columnbox_Army.Clear;
  ArmyChange(nil);
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

    CreateBars(Panel_ResultsMP);

    Panel_ChartsMP := TKMPanel.Create(Panel_ResultsMP, 0, PANES_TOP, 1024, 560);
    Panel_ChartsMP.Anchors := [anLeft];

      Chart_MPCitizens := TKMChart.Create(Panel_ChartsMP, 62, 0, 900, CHART_ECO_HEIGHT);
      Chart_MPCitizens.Caption := gResTexts[TX_GRAPH_CITIZENS];
      Chart_MPCitizens.Anchors := [anLeft];

      Chart_MPHouses := TKMChart.Create(Panel_ChartsMP, 62, CHART_ECO_HEIGHT + 25, 900, CHART_ECO_HEIGHT);
      Chart_MPHouses.Caption := gResTexts[TX_GRAPH_HOUSES];
      Chart_MPHouses.Anchors := [anLeft];

    CreateChartWares(Panel_ResultsMP);
    CreateChartArmy(Panel_ResultsMP);

    Button_ResultsMPBack := TKMButton.Create(Panel_ResultsMP, 100, Panel_ResultsMP.Height - BACK_BTN_Y_TO_BOTTOM, 280, 30, NO_TEXT, bsMenu);
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
