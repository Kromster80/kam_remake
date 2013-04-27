unit KM_InterfaceMapEditor;
{$I KaM_Remake.inc}
interface
uses
   {$IFDEF MSWindows} Windows, {$ENDIF}
   {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
   Classes, Controls, KromUtils, Math, StrUtils, SysUtils, KromOGLUtils, TypInfo,
   KM_Controls, KM_Defaults, KM_Pics, KM_Maps, KM_Houses, KM_Units, KM_UnitGroups, KM_MapEditor,
   KM_Points, KM_InterfaceDefaults, KM_AIAttacks, KM_Goals, KM_Terrain;

type
  TKMTerrainTab = (ttBrush, ttHeights, ttTile, ttObject, ttSelection);
  TKMTownTab = (ttHouses, ttUnits, ttScript, ttDefences, ttOffence);
  TKMPlayerTab = (ptGoals, ptColor, ptBlockHouse, ptBlockTrade, ptMarkers);


  TKMapEdInterface = class (TKMUserInterface)
  private
    fPrevHint: TObject;
    fActivePage: TKMPanel;
    fLastTile: Byte;
    fLastObject: Byte;
    fStorehouseItem: Byte; //Selected ware in storehouse
    fBarracksItem: Byte; //Selected ware in barracks
    fTileDirection: Byte;
    fActiveMarker: TKMMapEdMarker;
    fDragScrolling: Boolean;
    fDragScrollingCursorPos: TPoint;
    fDragScrollingViewportPos: TKMPointF;

    fMaps: TKMapsCollection;
    fMapsMP: TKMapsCollection;

    //Objects in MapElem are placed sparsely, so we need to compact them
    //to use in MapEd palette
    fCountCompact: Integer;
    fCompactToMapElem: array [Byte] of Byte; //Pointers to valid MapElem's
    fMapElemToCompact: array [Byte] of Byte; //Pointers of valid MapElem's back to map objects. (reverse lookup to one above) 256 is no object.

    procedure Create_Terrain;
    procedure Create_Town;
    procedure Create_Player;
    procedure Create_Mission;
    procedure Create_Menu;
    procedure Create_MenuSave;
    procedure Create_MenuLoad;
    procedure Create_MenuQuit;
    procedure Create_Extra;
    procedure Create_Message;
    procedure Create_Unit;
    procedure Create_House;
    procedure Create_HouseStore;
    procedure Create_HouseBarracks;
    procedure Create_Marker;
    procedure Create_AttackPopUp;
    procedure Create_FormationsPopUp;
    procedure Create_GoalPopUp;

    procedure Attack_Change(Sender: TObject);
    procedure Attack_Close(Sender: TObject);
    procedure Attack_Refresh(aAttack: TAIAttack);
    procedure Attacks_Add(Sender: TObject);
    procedure Attacks_Del(Sender: TObject);
    procedure Attacks_Edit(aIndex: Integer);
    procedure Attacks_ListClick(Sender: TObject);
    procedure Attacks_ListDoubleClick(Sender: TObject);
    procedure Attacks_Refresh;
    procedure CompactMapElements;
    procedure Extra_Change(Sender: TObject);
    procedure Formations_Show(Sender: TObject);
    procedure Formations_Close(Sender: TObject);
    procedure Goal_Change(Sender: TObject);
    procedure Goal_Close(Sender: TObject);
    procedure Goal_Refresh(aGoal: TKMGoal);
    procedure Goals_Add(Sender: TObject);
    procedure Goals_Del(Sender: TObject);
    procedure Goals_Edit(aIndex: Integer);
    procedure Goals_ListClick(Sender: TObject);
    procedure Goals_ListDoubleClick(Sender: TObject);
    procedure Goals_Refresh;
    procedure House_HealthChange(Sender: TObject; AButton: TMouseButton);
    procedure House_BarracksRefresh(Sender: TObject);
    procedure House_BarracksSelectWare(Sender: TObject);
    procedure House_BarracksEditCount(Sender: TObject; AButton:TMouseButton);
    procedure House_StoreRefresh(Sender: TObject);
    procedure House_StoreSelectWare(Sender: TObject);
    procedure House_StoreEditCount(Sender: TObject; AButton:TMouseButton);
    procedure Layers_UpdateVisibility;
    procedure Marker_Change(Sender: TObject);
    procedure Menu_SaveClick(Sender: TObject);
    procedure Menu_LoadClick(Sender: TObject);
    procedure Menu_QuitClick(Sender: TObject);
    procedure Menu_LoadChange(Sender: TObject);
    procedure Menu_LoadUpdate;
    procedure Menu_LoadUpdateDone(Sender: TObject);
    procedure Minimap_Update(Sender: TObject; const X,Y: Integer);
    procedure Mission_AlliancesChange(Sender: TObject);
    procedure Mission_ModeChange(Sender: TObject);
    procedure Mission_ModeUpdate;
    procedure Mission_PlayerTypesChange(Sender: TObject);
    procedure Mission_PlayerTypesUpdate;
    procedure Player_BlockHouseClick(Sender: TObject);
    procedure Player_BlockHouseRefresh;
    procedure Player_BlockTradeClick(Sender: TObject);
    procedure Player_BlockTradeRefresh;
    procedure Player_ChangeActive(Sender: TObject);
    procedure Player_ColorClick(Sender: TObject);
    procedure Player_MarkerClick(Sender: TObject);
    procedure Terrain_BrushChange(Sender: TObject);
    procedure Terrain_BrushRefresh;
    procedure Terrain_HeightChange(Sender: TObject);
    procedure Terrain_HeightRefresh;
    procedure Terrain_TilesChange(Sender: TObject);
    procedure Terrain_TilesSet(aIndex: Integer);
    procedure Terrain_TilesRefresh(Sender: TObject);
    procedure Terrain_ObjectsChange(Sender: TObject);
    procedure Terrain_ObjectsRefresh(Sender: TObject);
    procedure Terrain_ClipboardChange(Sender: TObject);
    procedure Town_BuildChange(Sender: TObject);
    procedure Town_BuildRefresh;
    procedure Town_DefenceAddClick(Sender: TObject);
    procedure Town_DefenceRefresh;
    procedure Town_DefenceChange(Sender: TObject);
    procedure Town_ScriptRefresh;
    procedure Town_ScriptChange(Sender: TObject);
    procedure Town_UnitChange(Sender: TObject);
    procedure Town_UnitRefresh;
    procedure Unit_ArmyChange1(Sender: TObject); overload;
    procedure Unit_ArmyChange2(Sender: TObject; AButton: TMouseButton); overload;
    procedure ExtraMessage_Switch(Sender: TObject);

    procedure SwitchPage(Sender: TObject);
    procedure DisplayPage(aPage: TKMPanel);
    procedure DisplayHint(Sender: TObject);
    procedure Player_UpdateColors;
    procedure Player_FOWChange(Sender: TObject);
    procedure RightClick_Cancel;
    procedure ShowHouseInfo(Sender: TKMHouse);
    procedure ShowUnitInfo(Sender: TKMUnit);
    procedure ShowGroupInfo(Sender: TKMUnitGroup);
    procedure ShowMarkerInfo(aMarker: TKMMapEdMarker);
    procedure SetActivePlayer(aIndex: TPlayerIndex);
    procedure SetTileDirection(aTileDirection: Byte);
    procedure UpdateAITabsEnabled;
  protected
    Panel_Main:TKMPanel;
      MinimapView: TKMMinimapView;
      Label_Coordinates:TKMLabel;
      Button_PlayerSelect: array [0..MAX_PLAYERS-1] of TKMFlatButtonShape; //Animals are common for all
      Label_Stat,Label_Hint: TKMLabel;
      Bevel_HintBG: TKMBevel;

    Panel_Common: TKMPanel;
      Button_Main: array [1..5] of TKMButton; //5 buttons
      Label_MissionName: TKMLabel;
      Image_Extra: TKMImage;
      Image_Message: TKMImage;

    Panel_Terrain: TKMPanel;
      Button_Terrain: array [TKMTerrainTab] of TKMButton;
      Panel_Brushes: TKMPanel;
        BrushSize: TKMTrackBar;
        BrushCircle,BrushSquare: TKMButtonFlat;
        BrushTable: array [0..6, 0..4] of TKMButtonFlat;
        BrushRandom: TKMCheckBox;
      Panel_Heights: TKMPanel;
        HeightSize, HeightSlope, HeightSpeed: TKMTrackBar;
        HeightShapeLabel: TKMLabel;
        HeightCircle,HeightSquare: TKMButtonFlat;
        HeightElevate, HeightUnequalize: TKMButtonFlat;
      Panel_Tiles: TKMPanel;
        TilesTable: array [0 .. MAPED_TILES_X * MAPED_TILES_Y - 1] of TKMButtonFlat; //how many are visible?
        TilesScroll: TKMScrollBar;
        TilesRandom: TKMCheckBox;
      Panel_Objects: TKMPanel;
        ObjectErase: TKMButtonFlat;
        ObjectBlock: TKMButtonFlat;
        ObjectsTable: array [0..8] of TKMButtonFlat;
        ObjectsScroll: TKMScrollBar;
      Panel_Selection: TKMPanel;
        Button_SelectCopy, Button_SelectPaste: TKMButtonFlat;
        Button_SelectFlipH, Button_SelectFlipV: TKMButtonFlat;


    //How to know where certain page should be?
    //see Docs\Map Editor menu structure.txt

    //Town and Visual stuff
    Panel_Town: TKMPanel;
      Button_Town: array [TKMTownTab] of TKMButton;
      Panel_Build: TKMPanel;
        Button_BuildRoad: TKMButtonFlat;
        Button_BuildField: TKMButtonFlat;
        Button_BuildWine: TKMButtonFlat;
        Button_BuildCancel: TKMButtonFlat;
        Button_Build: array [1..GUI_HOUSE_COUNT] of TKMButtonFlat;
      Panel_Units: TKMPanel;
        Button_UnitCancel: TKMButtonFlat;
        Button_Citizen: array [0..13] of TKMButtonFlat;
        Button_Warriors: array [0..13] of TKMButtonFlat;
        Button_Animals: array [0..7] of TKMButtonFlat;
      Panel_Script: TKMPanel;
        CheckBox_AutoBuild: TKMCheckBox;
        CheckBox_AutoRepair: TKMCheckBox;
        TrackBar_SerfsPer10Houses: TKMTrackBar;
        TrackBar_WorkerCount: TKMTrackBar;
      Panel_Defence: TKMPanel;
        Button_DefencePosAdd: TKMButtonFlat;
        CheckBox_AutoDefence: TKMCheckBox;
        TrackBar_EquipRateLeather: TKMTrackBar;
        TrackBar_EquipRateIron: TKMTrackBar;
        TrackBar_RecruitCount: TKMTrackBar;
        TrackBar_MaxSoldiers: TKMTrackBar;
        CheckBox_MaxSoldiers: TKMCheckBox;
        Button_EditFormations: TKMButton;
      Panel_Offence: TKMPanel;
        CheckBox_AutoAttack: TKMCheckBox;
        ColumnBox_Attacks: TKMColumnBox;
        Button_AttacksAdd: TKMButton;
        Button_AttacksDel: TKMButton;

    //Non-visual stuff per-player
    Panel_Player: TKMPanel;
      Button_Player: array [TKMPlayerTab] of TKMButton;
      Panel_Goals: TKMPanel;
        ColumnBox_Goals: TKMColumnBox;
        Button_GoalsAdd: TKMButton;
        Button_GoalsDel: TKMButton;
      Panel_Color: TKMPanel;
        ColorSwatch_Color: TKMColorSwatch;
      Panel_BlockHouse: TKMPanel;
        Button_BlockHouse: array [1 .. GUI_HOUSE_COUNT] of TKMButtonFlat;
        Image_BlockHouse: array [1 .. GUI_HOUSE_COUNT] of TKMImage;
      Panel_BlockTrade: TKMPanel;
        Button_BlockTrade: array [1 .. STORE_RES_COUNT] of TKMButtonFlat;
        Image_BlockTrade: array [1 .. STORE_RES_COUNT] of TKMImage;
      Panel_Markers: TKMPanel;
        Button_Reveal: TKMButtonFlat;
        TrackBar_RevealNewSize: TKMTrackBar;
        CheckBox_RevealAll: TKMCheckBox;
        Button_CenterScreen: TKMButtonFlat;
        Button_PlayerCenterScreen: TKMButton;

    //Global things
    Panel_Mission: TKMPanel;
      Button_Mission: array [1..3] of TKMButton;
      Panel_Mode: TKMPanel;
        Radio_MissionMode: TKMRadioGroup;
      Panel_Alliances: TKMPanel;
        CheckBox_Alliances: array [0..MAX_PLAYERS-1, 0..MAX_PLAYERS-1] of TKMCheckBox;
        CheckBox_AlliancesSym: TKMCheckBox;
      Panel_PlayerTypes: TKMPanel;
        CheckBox_PlayerTypes: array [0..MAX_PLAYERS-1, 0..2] of TKMCheckBox;

    Panel_Menu: TKMPanel;
      Button_Menu_Save,Button_Menu_Load,Button_Menu_Settings,Button_Menu_Quit: TKMButton;

      Panel_Save:TKMPanel;
        Radio_Save_MapType:TKMRadioGroup;
        Edit_SaveName:TKMEdit;
        Label_SaveExists:TKMLabel;
        CheckBox_SaveExists:TKMCheckBox;
        Button_SaveSave:TKMButton;
        Button_SaveCancel:TKMButton;

      Panel_Load:TKMPanel;
        Radio_Load_MapType:TKMRadioGroup;
        ListBox_Load:TKMListBox;
        Button_LoadLoad:TKMButton;
        Button_LoadCancel:TKMButton;

      Panel_Quit:TKMPanel;
        Button_Quit_Yes,Button_Quit_No:TKMButton;

    Panel_Extra: TKMPanel;
      Image_ExtraClose: TKMImage;
      TrackBar_Passability:TKMTrackBar;
      Label_Passability:TKMLabel;
      CheckBox_ShowObjects: TKMCheckBox;
      CheckBox_ShowHouses: TKMCheckBox;
      CheckBox_ShowUnits: TKMCheckBox;
      CheckBox_ShowDeposits: TKMCheckBox;
      Dropbox_PlayerFOW: TKMDropList;

    Panel_Message: TKMPanel;
      Label_Message: TKMLabel;
      Image_MessageClose: TKMImage;

    Panel_Unit:TKMPanel;
      Label_UnitName:TKMLabel;
      Label_UnitCondition:TKMLabel;
      Label_UnitDescription:TKMLabel;
      KMConditionBar_Unit:TKMPercentBar;
      Image_UnitPic:TKMImage;

      Panel_Army:TKMPanel;
        Button_Army_RotCW,Button_Army_RotCCW: TKMButton;
        Button_Army_ForUp,Button_Army_ForDown: TKMButton;
        ImageStack_Army: TKMImageStack;
        Label_ArmyCount: TKMLabel;
        Button_ArmyDec,Button_ArmyFood,Button_ArmyInc: TKMButton;
        DropBox_ArmyOrder: TKMDropList;
        Edit_ArmyOrderX: TKMNumericEdit;
        Edit_ArmyOrderY: TKMNumericEdit;
        Edit_ArmyOrderDir: TKMNumericEdit;

    Panel_House: TKMPanel;
      Label_House: TKMLabel;
      Image_House_Logo, Image_House_Worker: TKMImage;
      Label_HouseHealth: TKMLabel;
      KMHealthBar_House: TKMPercentBar;
      Button_HouseHealthDec, Button_HouseHealthInc: TKMButton;
      Label_House_Supply: TKMLabel;
      ResRow_Resource: array [0..3] of TKMResourceOrderRow;

      Panel_HouseStore:TKMPanel;
        Button_Store:array[1..STORE_RES_COUNT]of TKMButtonFlat;
        Label_Store_WareCount:TKMLabel;
        Button_StoreDec100,Button_StoreDec:TKMButton;
        Button_StoreInc100,Button_StoreInc:TKMButton;
      Panel_HouseBarracks:TKMPanel;
        Button_Barracks:array[1..BARRACKS_RES_COUNT]of TKMButtonFlat;
        Label_Barracks_WareCount:TKMLabel;
        Button_BarracksDec100,Button_BarracksDec:TKMButton;
        Button_BarracksInc100,Button_BarracksInc:TKMButton;

    Panel_Marker: TKMPanel;
      Label_MarkerType: TKMLabel;
      Image_MarkerPic: TKMImage;

      Panel_MarkerReveal: TKMPanel;
        TrackBar_RevealSize: TKMTrackBar;
        Button_RevealDelete: TKMButton;
        Button_RevealClose: TKMButton;
      Panel_MarkerDefence: TKMPanel;
        DropList_DefenceGroup: TKMDropList;
        DropList_DefenceType: TKMDropList;
        TrackBar_DefenceRad: TKMTrackBar;
        Button_DefenceCW, Button_DefenceCCW: TKMButton;
        Button_DefenceDelete: TKMButton;
        Button_DefenceClose: TKMButton;

    //PopUp panels
    Panel_Formations: TKMPanel;
      Image_FormationsFlag: TKMImage;
      NumEdit_FormationsCount,
      NumEdit_FormationsColumns: array [TGroupType] of TKMNumericEdit;
      Button_Formations_Ok: TKMButton;
      Button_Formations_Cancel: TKMButton;

    Panel_Attack: TKMPanel;
      Radio_AttackType: TKMRadioGroup;
      NumEdit_AttackDelay: TKMNumericEdit;
      NumEdit_AttackMen: TKMNumericEdit;
      NumEdit_AttackAmount: array [TGroupType] of TKMNumericEdit;
      CheckBox_AttackTakeAll: TKMCheckBox;
      Radio_AttackTarget: TKMRadioGroup;
      TrackBar_AttackRange: TKMTrackBar;
      NumEdit_AttackLocX: TKMNumericEdit;
      NumEdit_AttackLocY: TKMNumericEdit;
      Button_AttackOk: TKMButton;
      Button_AttackCancel: TKMButton;

    Panel_Goal: TKMPanel;
      Radio_GoalType: TKMRadioGroup;
      Radio_GoalCondition: TKMRadioGroup;
      NumEdit_GoalTime: TKMNumericEdit;
      NumEdit_GoalMessage: TKMNumericEdit;
      NumEdit_GoalPlayer: TKMNumericEdit;
      Button_GoalOk: TKMButton;
      Button_GoalCancel: TKMButton;
  public
    constructor Create(aScreenX, aScreenY: word);
    destructor Destroy; override;

    function GetShownPage: TKMMapEdShownPage;
    procedure SetLoadMode(aMultiplayer:boolean);
    procedure ShowMessage(aText: string);

    procedure KeyDown(Key:Word; Shift: TShiftState); override;
    procedure KeyUp(Key:Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;

    procedure Resize(X,Y: Word); override;
    procedure SyncUI;
    procedure UpdateState(aTickCount: Cardinal); override;
    procedure Paint; override;
  end;


implementation
uses
  KM_CommonClasses, KM_PlayersCollection, KM_TextLibrary, KM_Game, KM_Main,
  KM_GameApp, KM_Resource, KM_TerrainPainter, KM_ResourceCursors, KM_Utils,
  KM_ResourceMapElements, KM_AIDefensePos, KM_ResourceHouse, KM_RenderUI, KM_Sound;


const
  BIG_TAB_W = 37;
  BIG_PAD_W = 37;
  BIG_TAB_H = 36;
  //BIG_PAD_H = 40;
  SMALL_TAB_W = 32;
  SMALL_PAD_W = SMALL_TAB_W + 0;
  SMALL_TAB_H = 26;
  //SMALL_PAD_H = SMALL_TAB_H + 4;


{Switch between pages}
procedure TKMapEdInterface.SwitchPage(Sender: TObject);
begin
  //Reset cursor mode
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  //If the user clicks on the tab that is open, it closes it (main buttons only)
  if ((Sender = Button_Main[1]) and Panel_Terrain.Visible) or
     ((Sender = Button_Main[2]) and Panel_Town.Visible) or
     ((Sender = Button_Main[3]) and Panel_Player.Visible) or
     ((Sender = Button_Main[4]) and Panel_Mission.Visible) or
     ((Sender = Button_Main[5]) and Panel_Menu.Visible) then
    Sender := nil;

  //Reset shown item if user clicked on any of the main buttons
  if (Sender=Button_Main[1])or(Sender=Button_Main[2])or
     (Sender=Button_Main[3])or(Sender=Button_Main[4])or
     (Sender=Button_Main[5])or
     (Sender=Button_Menu_Settings)or(Sender=Button_Menu_Quit) then
    MySpectator.Selected := nil;

  if (Sender = Button_Main[1]) or (Sender = Button_Terrain[ttBrush]) then
  begin
    Terrain_BrushChange(BrushTable[0,0]);
    DisplayPage(Panel_Brushes);
  end else
  if (Sender = Button_Terrain[ttHeights]) then
  begin
    Terrain_HeightChange(HeightCircle);
    Terrain_HeightChange(HeightElevate);
    DisplayPage(Panel_Heights);
  end else
  if (Sender = Button_Terrain[ttTile]) then
  begin
    Terrain_TilesSet(fLastTile);
    DisplayPage(Panel_Tiles);
  end else
  if (Sender = Button_Terrain[ttObject]) then
  begin
    if fLastObject = 255 then
      Terrain_ObjectsChange(ObjectErase)
    else
      if fLastObject = 61 then
        Terrain_ObjectsChange(ObjectBlock)
      else
        Terrain_ObjectsChange(ObjectsTable[fLastObject]);
    DisplayPage(Panel_Objects);
  end else
  if (Sender = Button_Terrain[ttSelection]) then
  begin
    Terrain_ClipboardChange(Button_SelectCopy);
    DisplayPage(Panel_Selection);
  end else

  if (Sender = Button_Main[2]) or (Sender = Button_Town[ttHouses]) then
  begin
    Town_BuildRefresh;
    DisplayPage(Panel_Build);
  end else
  if (Sender = Button_Town[ttUnits]) then
    DisplayPage(Panel_Units)
  else
  if (Sender = Button_Town[ttScript]) then
    DisplayPage(Panel_Script)
  else
  if (Sender = Button_Town[ttDefences]) then
    DisplayPage(Panel_Defence)
  else
  if (Sender = Button_Town[ttOffence]) then
    DisplayPage(Panel_Offence)
  else

  if (Sender = Button_Main[3])or(Sender = Button_Player[ptGoals]) then
    DisplayPage(Panel_Goals)
  else
  if (Sender = Button_Player[ptColor]) then
    DisplayPage(Panel_Color)
  else
  if (Sender = Button_Player[ptBlockHouse]) then
    DisplayPage(Panel_BlockHouse)
  else
  if (Sender = Button_Player[ptBlockTrade]) then
    DisplayPage(Panel_BlockTrade)
  else
  if (Sender = Button_Player[ptMarkers]) then
    DisplayPage(Panel_Markers)
  else

  if (Sender = Button_Main[4])or(Sender = Button_Mission[1]) then
    DisplayPage(Panel_Mode)
  else
  if (Sender = Button_Mission[2]) then
    DisplayPage(Panel_Alliances)
  else
  if (Sender = Button_Mission[3]) then
    DisplayPage(Panel_PlayerTypes)
  else

  if (Sender = Button_Main[5]) or
     (Sender = Button_Quit_No) or
     (Sender = Button_LoadCancel) or
     (Sender = Button_SaveCancel) then
    DisplayPage(Panel_Menu)
  else
  if Sender = Button_Menu_Quit then
    DisplayPage(Panel_Quit)
  else
  if Sender = Button_Menu_Save then
    DisplayPage(Panel_Save)
  else
  if Sender = Button_Menu_Load then
  begin
    Menu_LoadUpdate;
    DisplayPage(Panel_Load)
  end;
end;


procedure TKMapEdInterface.DisplayPage(aPage: TKMPanel);
var I,K: Integer;
begin
  //Hide all existing pages (2 levels)
  for I := 1 to Panel_Common.ChildCount do
  if Panel_Common.Childs[I] is TKMPanel then
  begin
    Panel_Common.Childs[I].Hide;
    for K := 1 to TKMPanel(Panel_Common.Childs[I]).ChildCount do
    if TKMPanel(Panel_Common.Childs[I]).Childs[K] is TKMPanel then
      TKMPanel(Panel_Common.Childs[I]).Childs[K].Hide;
  end;

  if aPage = Panel_Brushes then
    Terrain_BrushRefresh
  else
  if aPage = Panel_Heights then
    Terrain_HeightRefresh
  else
  if aPage = Panel_Tiles then
  begin
    SetTileDirection(fTileDirection); //ensures tags are in allowed ranges
    Terrain_TilesRefresh(nil);
  end else
  if aPage = Panel_Objects then
    Terrain_ObjectsRefresh(nil)
  else

  if aPage = Panel_Build then
    Town_BuildRefresh
  else
  if aPage = Panel_Units then
    Town_UnitRefresh
  else
  if aPage = Panel_Script then
    Town_ScriptRefresh
  else
  if aPage = Panel_Defence then
  begin
    Town_DefenceAddClick(nil);
    Town_DefenceRefresh;
  end
  else
  if aPage = Panel_Offence then
    Attacks_Refresh
  else

  if aPage = Panel_Goals then
    Goals_Refresh
  else
  if aPage = Panel_Color then

  else
  if aPage = Panel_BlockHouse then
    Player_BlockHouseRefresh
  else
  if aPage = Panel_BlockTrade then
    Player_BlockTradeRefresh
  else
  if aPage = Panel_Markers then
    Player_MarkerClick(nil)
  else

  if aPage = Panel_Mode then
    Mission_ModeUpdate
  else
  if aPage = Panel_Alliances then
    Mission_AlliancesChange(nil)
  else
  if aPage = Panel_PlayerTypes then
    Mission_PlayerTypesUpdate
  else

  if aPage = Panel_Menu then
  else
  if aPage = Panel_Save then
  begin
    Edit_SaveName.Text := fGame.GameName;
    Menu_SaveClick(Edit_SaveName);
  end else
  if aPage = Panel_Load then
    Panel_Load.Show;

  //Display the panel (and its parents)
  fActivePage := aPage;
  if aPage <> nil then
    aPage.Show;

  //Update list of visible layers with regard to active page and checkboxes
  Layers_UpdateVisibility;
end;


procedure TKMapEdInterface.DisplayHint(Sender: TObject);
begin
  if (fPrevHint = Sender) then exit; //Hint didn't changed

  if (Sender = nil) or (TKMControl(Sender).Hint = '') then
  begin
    Label_Hint.Caption := '';
    Bevel_HintBG.Hide;
  end
  else
  begin
    Label_Hint.Caption := TKMControl(Sender).Hint;
    Bevel_HintBG.Show;
    Bevel_HintBG.Width := 8 + fResource.Fonts.GetTextSize(Label_Hint.Caption, Label_Hint.Font).X;
  end;

  fPrevHint := Sender;
end;


procedure TKMapEdInterface.Formations_Show(Sender: TObject);
var
  GT: TGroupType;
begin
  //Fill UI
  Image_FormationsFlag.FlagColor := fPlayers[MySpectator.PlayerIndex].FlagColor;
  for GT := Low(TGroupType) to High(TGroupType) do
  begin
    NumEdit_FormationsCount[GT].Value := fPlayers[MySpectator.PlayerIndex].AI.General.DefencePositions.TroopFormations[GT].NumUnits;
    NumEdit_FormationsColumns[GT].Value := fPlayers[MySpectator.PlayerIndex].AI.General.DefencePositions.TroopFormations[GT].UnitsPerRow;
  end;

  Panel_Formations.Show;
end;


procedure TKMapEdInterface.Formations_Close(Sender: TObject);
var
  GT: TGroupType;
begin
  Assert(Image_FormationsFlag.FlagColor = fPlayers[MySpectator.PlayerIndex].FlagColor, 'Cheap test to see if active player didn''t changed');

  if Sender = Button_Formations_Ok then
    //Save settings
    for GT := Low(TGroupType) to High(TGroupType) do
    begin
      fPlayers[MySpectator.PlayerIndex].AI.General.DefencePositions.TroopFormations[GT].NumUnits := NumEdit_FormationsCount[GT].Value;
      fPlayers[MySpectator.PlayerIndex].AI.General.DefencePositions.TroopFormations[GT].UnitsPerRow := NumEdit_FormationsColumns[GT].Value;
    end;

  Panel_Formations.Hide;
end;


//Update viewport position when user interacts with minimap
procedure TKMapEdInterface.Minimap_Update(Sender: TObject; const X,Y: Integer);
begin
  fGame.Viewport.Position := KMPointF(X,Y);
end;


procedure TKMapEdInterface.CompactMapElements;
var
  I: Integer;
begin
  fCountCompact := 0;
  for I := 0 to fResource.MapElements.Count - 1 do
  if (MapElem[I].Anim.Count > 0) and (MapElem[I].Anim.Step[1] > 0)
  and (MapElem[I].Stump = -1) and (I <> 61) then //Hide falling trees and invisible wall (61)
  begin
    fCompactToMapElem[fCountCompact] := I; //pointer
    fMapElemToCompact[I] := fCountCompact; //Reverse lookup
    Inc(fCountCompact);
  end;
end;


constructor TKMapEdInterface.Create(aScreenX, aScreenY: Word);
var
  I: Integer;
begin
  inherited;

  fBarracksItem   := 1; //First ware selected by default
  fStorehouseItem := 1; //First ware selected by default
  fTileDirection := 0;
  fDragScrolling := False;
  fDragScrollingCursorPos.X := 0;
  fDragScrollingCursorPos.Y := 0;
  fDragScrollingViewportPos.X := 0.0;
  fDragScrollingViewportPos.Y := 0.0;
  fMaps := TKMapsCollection.Create(False);
  fMapsMP := TKMapsCollection.Create(True);

  CompactMapElements;

  //Parent Page for whole toolbar in-game
  Panel_Main := TKMPanel.Create(fMyControls, 0, 0, aScreenX, aScreenY);

    TKMImage.Create(Panel_Main,0,   0,224,200,407); //Minimap place
    TKMImage.Create(Panel_Main,0, 200,224,400,404);
    TKMImage.Create(Panel_Main,0, 600,224,400,404);
    TKMImage.Create(Panel_Main,0,1000,224,400,404); //For 1600x1200 this is needed

    MinimapView := TKMMinimapView.Create(Panel_Main, 10, 10, 176, 176);
    MinimapView.OnChange := Minimap_Update;

    Label_MissionName := TKMLabel.Create(Panel_Main, 230, 10, 184, 10, NO_TEXT, fnt_Grey, taLeft);
    Label_Coordinates := TKMLabel.Create(Panel_Main, 230, 30, 'X: Y:', fnt_Grey, taLeft);
    Label_Stat := TKMLabel.Create(Panel_Main, 230, 50, 0, 0, '', fnt_Outline, taLeft);

    TKMLabel.Create(Panel_Main, TB_PAD, 190, TB_WIDTH, 0, fTextLibrary[TX_MAPED_PLAYERS], fnt_Outline, taLeft);
    for I := 0 to MAX_PLAYERS - 1 do
    begin
      Button_PlayerSelect[I]         := TKMFlatButtonShape.Create(Panel_Main, 8 + I*23, 210, 21, 21, IntToStr(I+1), fnt_Grey, $FF0000FF);
      Button_PlayerSelect[I].Tag     := I;
      Button_PlayerSelect[I].OnClick := Player_ChangeActive;
    end;
    Button_PlayerSelect[0].Down := True; //First player selected by default

  //Must be created before Hint so it goes over them
  Create_Extra;
  Create_Message;

    Bevel_HintBG := TKMBevel.Create(Panel_Main,224+32,Panel_Main.Height-23,300,21);
    Bevel_HintBG.BackAlpha := 0.5;
    Bevel_HintBG.Hide;
    Bevel_HintBG.Anchors := [akLeft, akBottom];

    Label_Hint := TKMLabel.Create(Panel_Main, 224 + 36, Panel_Main.Height - 21, 0, 0, '', fnt_Outline, taLeft);
    Label_Hint.Anchors := [akLeft, akBottom];

  Panel_Common := TKMPanel.Create(Panel_Main,TB_PAD,255,TB_WIDTH,768);

    {5 big tabs}
    Button_Main[1] := TKMButton.Create(Panel_Common, BIG_PAD_W*0, 0, BIG_TAB_W, BIG_TAB_H, 381, rxGui, bsGame);
    Button_Main[2] := TKMButton.Create(Panel_Common, BIG_PAD_W*1, 0, BIG_TAB_W, BIG_TAB_H, 589, rxGui, bsGame);
    Button_Main[3] := TKMButton.Create(Panel_Common, BIG_PAD_W*2, 0, BIG_TAB_W, BIG_TAB_H, 392, rxGui, bsGame);
    Button_Main[4] := TKMButton.Create(Panel_Common, BIG_PAD_W*3, 0, BIG_TAB_W, BIG_TAB_H, 441, rxGui, bsGame);
    Button_Main[5] := TKMButton.Create(Panel_Common, BIG_PAD_W*4, 0, BIG_TAB_W, BIG_TAB_H, 389, rxGui, bsGame);
    Button_Main[1].Hint := fTextLibrary[TX_MAPED_TERRAIN];
    Button_Main[2].Hint := fTextLibrary[TX_MAPED_VILLAGE];
    Button_Main[3].Hint := fTextLibrary[TX_MAPED_SCRIPTS_VISUAL];
    Button_Main[4].Hint := fTextLibrary[TX_MAPED_SCRIPTS_GLOBAL];
    Button_Main[5].Hint := fTextLibrary[TX_MAPED_MENU];
    for I := 1 to 5 do
      Button_Main[I].OnClick := SwitchPage;

{I plan to store all possible layouts on different pages which gets displayed one at a time}
{==========================================================================================}
  Create_Terrain;
  Create_Town;
  Create_Player;
  Create_Mission;

  Create_Menu;
    Create_MenuSave;
    Create_MenuLoad;
    Create_MenuQuit;

  Create_Unit;
  Create_House;
    Create_HouseStore;
    Create_HouseBarracks;
    //Create_HouseTownHall;
  Create_Marker;

  Image_Extra := TKMImage.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - 48, 30, 48, 494);
  Image_Extra.Anchors := [akLeft, akBottom];
  Image_Extra.HighlightOnMouseOver := True;
  Image_Extra.OnClick := ExtraMessage_Switch;

  Image_Message := TKMImage.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - 48*2, 30, 48, 496);
  Image_Message.Anchors := [akLeft, akBottom];
  Image_Message.HighlightOnMouseOver := True;
  Image_Message.OnClick := ExtraMessage_Switch;
  Image_Message.Hide; //Hidden by default, only visible when a message is shown

  //Pages that need to be on top of everything
  Create_AttackPopUp;
  Create_FormationsPopUp;
  Create_GoalPopUp;

  fMyControls.OnHint := DisplayHint;

  DisplayPage(nil); //Update
end;


destructor TKMapEdInterface.Destroy;
begin
  fMaps.Free;
  fMapsMP.Free;
  SHOW_TERRAIN_WIRES := false; //Don't show it in-game if they left it on in MapEd
  SHOW_TERRAIN_PASS := 0; //Don't show it in-game if they left it on in MapEd
  inherited;
end;


//Update Hint position and etc..
procedure TKMapEdInterface.Resize(X,Y: Word);
begin
  Panel_Main.Width := X;
  Panel_Main.Height := Y;
end;


{Terrain page}
procedure TKMapEdInterface.Create_Terrain;
const
  BtnGlyph: array [TKMTerrainTab] of Word = (383, 388, 382, 385, 384);
  BtnHint: array [TKMTerrainTab] of Word = (
    TX_MAPED_TERRAIN_HINTS_BRUSHES,
    TX_MAPED_TERRAIN_HINTS_HEIGHTS,
    TX_MAPED_TERRAIN_HINTS_TILES,
    TX_MAPED_TERRAIN_HINTS_OBJECTS,
    TX_MAPED_TERRAIN_HINTS_OBJECTS);
  Surfaces: array [0 .. 6, 0 .. 4] of TTerrainKind = (
    (tkGrass,       tkMoss,         tkRustyGrass1,  tkRustyGrass2,  tkCustom),
    (tkDirtGrass,   tkDirt,         tkGravel,       tkCobbleStone,  tkCustom),
    (tkGrassSand2,  tkGrassSand1,   tkSand,         tkRichSand,     tkCustom),
    (tkSwamp,       tkGrassyWater,  tkWater,        tkFastWater,    tkCustom),
    (tkShallowSnow, tkSnow,         tkDeepSnow,     tkIce,          tkCustom),
    (tkStoneMount,  tkGoldMount,    tkIronMount,    tkAbyss,        tkCustom),
    (tkCoal,        tkGold,         tkIron,         tkLava,         tkCustom));
var
  I: TKMTerrainTab;
  J,K: Integer;
begin
  Panel_Terrain := TKMPanel.Create(Panel_Common, 0, 45, TB_WIDTH, 28);
    for I := Low(TKMTerrainTab) to High(TKMTerrainTab) do
    begin
      Button_Terrain[I] := TKMButton.Create(Panel_Terrain, SMALL_PAD_W * Byte(I), 0, SMALL_TAB_W, SMALL_TAB_H, BtnGlyph[I], rxGui, bsGame);
      Button_Terrain[I].Hint := fTextLibrary[BtnHint[I]];
      Button_Terrain[I].OnClick := SwitchPage;
    end;

    Panel_Brushes := TKMPanel.Create(Panel_Terrain,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Brushes, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_TERRAIN_BRUSH], fnt_Outline, taCenter);
      BrushSize   := TKMTrackBar.Create(Panel_Brushes, 0, 30, 100, 0, 12);
      BrushSize.OnChange := Terrain_BrushChange;
      BrushCircle := TKMButtonFlat.Create(Panel_Brushes, 106, 28, 24, 24, 592);
      BrushCircle.Hint := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_CIRCLE];
      BrushCircle.OnClick := Terrain_BrushChange;
      BrushSquare := TKMButtonFlat.Create(Panel_Brushes, 134, 28, 24, 24, 593);
      BrushSquare.Hint := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SQUARE];
      BrushSquare.OnClick := Terrain_BrushChange;

      for J := Low(Surfaces) to High(Surfaces) do
      for K := Low(Surfaces[J]) to High(Surfaces[J]) do
      if Surfaces[J,K] <> tkCustom then
      begin
        BrushTable[J,K] := TKMButtonFlat.Create(Panel_Brushes, K * 36, 60 + J * 40, 34, 34, Combo[Surfaces[J,K], Surfaces[J,K], 1] + 1, rxTiles);  // grass
        BrushTable[J,K].Tag := Byte(Surfaces[J,K]);
        BrushTable[J,K].OnClick := Terrain_BrushChange;
      end;

      BrushRandom := TKMCheckBox.Create(Panel_Brushes, 0, 350, TB_WIDTH, 20, fTextLibrary[TX_MAPED_TERRAIN_BRUSH_RANDOM], fnt_Metal);
      BrushRandom.OnClick := Terrain_BrushChange;

    Panel_Heights := TKMPanel.Create(Panel_Terrain,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Heights, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS], fnt_Outline, taCenter);
      HeightShapeLabel := TKMLabel.Create(Panel_Heights, 0, 34, TB_WIDTH, 0, fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SHAPE], fnt_Metal, taLeft);
      HeightCircle := TKMButtonFlat.Create(Panel_Heights, 120, 30, 24, 24, 592);
      HeightCircle.Hint := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_CIRCLE];
      HeightCircle.OnClick  := Terrain_HeightChange;
      HeightSquare := TKMButtonFlat.Create(Panel_Heights, 150, 30, 24, 24, 593);
      HeightSquare.Hint := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SQUARE];
      HeightSquare.OnClick  := Terrain_HeightChange;

      HeightSize          := TKMTrackBar.Create(Panel_Heights, 0, 60, TB_WIDTH, 1, 15); //1..15(4bit) for size
      HeightSize.Caption  := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SIZE];
      HeightSize.Hint     := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SIZE_HINT];
      HeightSize.OnChange := Terrain_HeightChange;
      HeightSlope           := TKMTrackBar.Create(Panel_Heights, 0, 115, TB_WIDTH, 1, 15); //1..15(4bit) for slope shape
      HeightSlope.Caption   := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SLOPE];
      HeightSlope.Hint      := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SLOPE_HINT];
      HeightSlope.OnChange  := Terrain_HeightChange;
      HeightSpeed           := TKMTrackBar.Create(Panel_Heights, 0, 170, TB_WIDTH, 1, 15); //1..15(4bit) for speed
      HeightSpeed.Caption   := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SPEED];
      HeightSpeed.Hint      := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SPEED_HINT];
      HeightSpeed.OnChange  := Terrain_HeightChange;

      HeightElevate             := TKMButtonFlat.Create(Panel_Heights, 0, 225, TB_WIDTH, 20, 0);
      HeightElevate.OnClick     := Terrain_HeightChange;
      HeightElevate.Down        := True;
      HeightElevate.Caption     := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_ELEVATE];
      HeightElevate.CapOffsetY  := -12;
      HeightElevate.Hint        := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_ELEVATE_HINT];
      HeightUnequalize          := TKMButtonFlat.Create(Panel_Heights, 0, 255, TB_WIDTH, 20, 0);
      HeightUnequalize.OnClick  := Terrain_HeightChange;
      HeightUnequalize.Caption  := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_UNEQUALIZE];
      HeightUnequalize.CapOffsetY  := -12;
      HeightUnequalize.Hint      := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_UNEQUALIZE_HINT];

    Panel_Tiles := TKMPanel.Create(Panel_Terrain, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_Tiles, 0, PAGE_TITLE_Y, TB_WIDTH, 0, 'Tiles', fnt_Outline, taCenter);
      TilesRandom := TKMCheckBox.Create(Panel_Tiles, 0, 30, TB_WIDTH, 20, fTextLibrary[TX_MAPED_TERRAIN_TILES_RANDOM], fnt_Metal);
      TilesRandom.Checked := True;
      TilesRandom.OnClick := Terrain_TilesChange;
      TilesRandom.Hint := fTextLibrary[TX_MAPED_TERRAIN_TILES_RANDOM_HINT];

      //Create scroll first to link to its MouseWheel event
      TilesScroll := TKMScrollBar.Create(Panel_Tiles, 2, 50 + 4 + MAPED_TILES_Y * 32, 194, 20, sa_Horizontal, bsGame);
      TilesScroll.MaxValue := 256 div MAPED_TILES_Y - MAPED_TILES_X; // 32 - 6
      TilesScroll.Position := 0;
      TilesScroll.OnChange := Terrain_TilesRefresh;
      for J := 0 to MAPED_TILES_Y - 1 do
      for K := 0 to MAPED_TILES_X - 1 do
      begin
        TilesTable[J * MAPED_TILES_X + K] := TKMButtonFlat.Create(Panel_Tiles, K * 32, 50 + J * 32, 32, 32, 1, rxTiles);
        TilesTable[J * MAPED_TILES_X + K].Tag :=  J * MAPED_TILES_X + K; //Store ID
        TilesTable[J * MAPED_TILES_X + K].OnClick := Terrain_TilesChange;
        TilesTable[J * MAPED_TILES_X + K].OnMouseWheel := TilesScroll.MouseWheel;
        TilesTable[J * MAPED_TILES_X + K].Hint := fTextLibrary[TX_MAPED_TERRAIN_TILES_MAIN_HINT];
      end;

    Panel_Objects := TKMPanel.Create(Panel_Terrain,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Objects, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_OBJECTS], fnt_Outline, taCenter);
      ObjectsScroll := TKMScrollBar.Create(Panel_Objects, 0, 295, TB_WIDTH, 20, sa_Horizontal, bsGame);
      ObjectsScroll.MinValue := 0;
      ObjectsScroll.MaxValue := fCountCompact div 3 - 3;
      ObjectsScroll.Position := 0;
      ObjectsScroll.OnChange := Terrain_ObjectsRefresh;
      for J := 0 to 2 do for K := 0 to 2 do
      begin
        ObjectsTable[J*3+K] := TKMButtonFlat.Create(Panel_Objects, J*65, 40+K*85,64,84,1,rxTrees); //RXid=1  // 1 2
        ObjectsTable[J*3+K].Tag := J*3+K; //Store ID
        ObjectsTable[J*3+K].OnClick := Terrain_ObjectsChange;
        ObjectsTable[J*3+K].OnMouseWheel := ObjectsScroll.MouseWheel;
      end;
      ObjectErase := TKMButtonFlat.Create(Panel_Objects, 0, 8,32,32,340);
      ObjectErase.Hint := fTextLibrary[TX_MAPED_TERRAIN_OBJECTS_REMOVE];
      ObjectErase.Tag := 255; //no object
      ObjectErase.OnClick := Terrain_ObjectsChange;

      ObjectBlock := TKMButtonFlat.Create(Panel_Objects, TB_WIDTH-32, 8,32,32,254,rxTrees);
      ObjectBlock.Hint := fTextLibrary[TX_MAPED_TERRAIN_OBJECTS_BLOCK];
      ObjectBlock.Tag := 61; //no object
      ObjectBlock.OnClick := Terrain_ObjectsChange;

    Panel_Selection := TKMPanel.Create(Panel_Terrain,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Selection, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_COPY_TITLE], fnt_Outline, taCenter);
        Button_SelectCopy := TKMButtonFlat.Create(Panel_Selection, 24, 28, 24, 24, 384);
        Button_SelectCopy.Hint := fTextLibrary[TX_MAPED_COPY_COPY_HINT];
        Button_SelectCopy.OnClick := Terrain_ClipboardChange;
        Button_SelectPaste := TKMButtonFlat.Create(Panel_Selection, 52, 28, 24, 24, 384);
        Button_SelectPaste.Hint := fTextLibrary[TX_MAPED_COPY_PASTE_HINT];
        Button_SelectPaste.OnClick := Terrain_ClipboardChange;
        Button_SelectFlipH := TKMButtonFlat.Create(Panel_Selection, 80, 28, 24, 24, 384);
        Button_SelectFlipH.Hint := fTextLibrary[TX_MAPED_COPY_PASTE_HFLIP_HINT];
        Button_SelectFlipH.OnClick := Terrain_ClipboardChange;
        Button_SelectFlipV := TKMButtonFlat.Create(Panel_Selection, 108, 28, 24, 24, 384);
        Button_SelectFlipV.Hint := fTextLibrary[TX_MAPED_COPY_PASTE_VFLIP_HINT];
        Button_SelectFlipV.OnClick := Terrain_ClipboardChange;
end;


{Build page}
procedure TKMapEdInterface.Create_Town;
const
  TabGlyph: array [TKMTownTab] of Word    = (391,   141,   62,        43,    53);
  TabRXX  : array [TKMTownTab] of TRXType = (rxGui, rxGui, rxGuiMain, rxGui, rxGui);
var
  I: Integer;
  VT: TKMTownTab;
begin
  Panel_Town := TKMPanel.Create(Panel_Common, 0, 45, TB_WIDTH, 28);

    for VT := Low(TKMTownTab) to High(TKMTownTab) do
    begin
      Button_Town[VT] := TKMButton.Create(Panel_Town, SMALL_PAD_W * Byte(VT), 0, SMALL_TAB_W, SMALL_TAB_H, TabGlyph[VT], TabRXX[VT], bsGame);
      Button_Town[VT].OnClick := SwitchPage;
    end;

    //Town placement
    Panel_Build := TKMPanel.Create(Panel_Town,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Build,0,PAGE_TITLE_Y,TB_WIDTH,0,fTextLibrary[TX_MAPED_ROAD_TITLE],fnt_Outline,taCenter);
      Button_BuildRoad   := TKMButtonFlat.Create(Panel_Build,  0,28,33,33,335);
      Button_BuildField  := TKMButtonFlat.Create(Panel_Build, 37,28,33,33,337);
      Button_BuildWine   := TKMButtonFlat.Create(Panel_Build, 74,28,33,33,336);
      Button_BuildCancel := TKMButtonFlat.Create(Panel_Build,148,28,33,33,340);
      Button_BuildRoad.OnClick  := Town_BuildChange;
      Button_BuildField.OnClick := Town_BuildChange;
      Button_BuildWine.OnClick  := Town_BuildChange;
      Button_BuildCancel.OnClick:= Town_BuildChange;
      Button_BuildRoad.Hint     := fTextLibrary[TX_BUILD_ROAD_HINT];
      Button_BuildField.Hint    := fTextLibrary[TX_BUILD_FIELD_HINT];
      Button_BuildWine.Hint     := fTextLibrary[TX_BUILD_WINE_HINT];
      Button_BuildCancel.Hint   := fTextLibrary[TX_BUILD_CANCEL_HINT];

      TKMLabel.Create(Panel_Build,0,65,TB_WIDTH,0,fTextLibrary[TX_MAPED_HOUSES_TITLE],fnt_Outline,taCenter);
      for I:=1 to GUI_HOUSE_COUNT do
        if GUIHouseOrder[I] <> ht_None then begin
          Button_Build[I]:=TKMButtonFlat.Create(Panel_Build, ((I-1) mod 5)*37,83+((I-1) div 5)*37,33,33,fResource.HouseDat[GUIHouseOrder[I]].GUIIcon);
          Button_Build[I].OnClick:=Town_BuildChange;
          Button_Build[I].Hint := fResource.HouseDat[GUIHouseOrder[I]].HouseName;
        end;

    //Units placement
    Panel_Units := TKMPanel.Create(Panel_Town,0,28,TB_WIDTH,400);

      for I := 0 to High(Button_Citizen) do
      begin
        Button_Citizen[I] := TKMButtonFlat.Create(Panel_Units,(I mod 5)*37,8+(I div 5)*37,33,33,fResource.UnitDat[School_Order[I]].GUIIcon); //List of tiles 5x5
        Button_Citizen[I].Hint := fResource.UnitDat[School_Order[I]].UnitName;
        Button_Citizen[I].Tag := Byte(School_Order[I]); //Returns unit ID
        Button_Citizen[I].OnClick := Town_UnitChange;
      end;
      Button_UnitCancel := TKMButtonFlat.Create(Panel_Units,((High(Button_Citizen)+1) mod 5)*37,8+(length(Button_Citizen) div 5)*37,33,33,340);
      Button_UnitCancel.Hint := fTextLibrary[TX_BUILD_CANCEL_HINT];
      Button_UnitCancel.Tag := 255; //Erase
      Button_UnitCancel.OnClick := Town_UnitChange;

      for I := 0 to High(Button_Warriors) do
      begin
        Button_Warriors[I] := TKMButtonFlat.Create(Panel_Units,(I mod 5)*37,124+(I div 5)*37,33,33, MapEd_Icon[I], rxGui);
        Button_Warriors[I].Hint := fResource.UnitDat[MapEd_Order[I]].UnitName;
        Button_Warriors[I].Tag := Byte(MapEd_Order[I]); //Returns unit ID
        Button_Warriors[I].OnClick := Town_UnitChange;
      end;

      for I := 0 to High(Button_Animals) do
      begin
        Button_Animals[I] := TKMButtonFlat.Create(Panel_Units,(I mod 5)*37,240+(I div 5)*37,33,33, Animal_Icon[I], rxGui);
        Button_Animals[I].Hint := fResource.UnitDat[Animal_Order[I]].UnitName;
        Button_Animals[I].Tag := Byte(Animal_Order[I]); //Returns animal ID
        Button_Animals[I].OnClick := Town_UnitChange;
      end;

    //Town settings
    Panel_Script := TKMPanel.Create(Panel_Town, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_Script, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_AI_TITLE], fnt_Outline, taCenter);
      CheckBox_AutoBuild := TKMCheckBox.Create(Panel_Script, 0, 30, TB_WIDTH, 20, fTextLibrary[TX_MAPED_AI_AUTOBUILD], fnt_Metal);
      CheckBox_AutoBuild.OnClick := Town_ScriptChange;
      CheckBox_AutoRepair := TKMCheckBox.Create(Panel_Script, 0, 50, TB_WIDTH, 20, fTextLibrary[TX_MAPED_AI_AUTOREPAIR], fnt_Metal);
      CheckBox_AutoRepair.OnClick := Town_ScriptChange;
      TrackBar_SerfsPer10Houses := TKMTrackBar.Create(Panel_Script, 0, 70, TB_WIDTH, 1, 50);
      TrackBar_SerfsPer10Houses.Caption := fTextLibrary[TX_MAPED_AI_SERFS_PER_10_HOUSES];
      TrackBar_SerfsPer10Houses.OnChange := Town_ScriptChange;
      TrackBar_WorkerCount := TKMTrackBar.Create(Panel_Script, 0, 110, TB_WIDTH, 0, 30);
      TrackBar_WorkerCount.Caption := fTextLibrary[TX_MAPED_AI_WORKERS];
      TrackBar_WorkerCount.OnChange := Town_ScriptChange;

    //Defence settings
    Panel_Defence := TKMPanel.Create(Panel_Town, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_Defence, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_AI_DEFENSE], fnt_Outline, taCenter);
      Button_DefencePosAdd := TKMButtonFlat.Create(Panel_Defence, 0, 30, 33, 33, 338);
      Button_DefencePosAdd.OnClick := Town_DefenceAddClick;
      Button_DefencePosAdd.Hint    := fTextLibrary[TX_MAPED_AI_DEFENSE_HINT];

      TKMLabel.Create(Panel_Defence, 0, 65, TB_WIDTH, 0, 'AI defence', fnt_Outline, taCenter);
      CheckBox_AutoDefence := TKMCheckBox.Create(Panel_Defence, 0, 90, TB_WIDTH, 20, fTextLibrary[TX_MAPED_AI_DEFENSE_AUTO], fnt_Metal);
      CheckBox_AutoDefence.Hint := fTextLibrary[TX_MAPED_AI_DEFENSE_AUTO_HINT];
      CheckBox_AutoDefence.OnClick := Town_DefenceChange;

      TrackBar_EquipRateLeather := TKMTrackBar.Create(Panel_Defence, 0, 120, TB_WIDTH, 10, 300);
      TrackBar_EquipRateLeather.Caption := fTextLibrary[TX_MAPED_AI_DEFENSE_EQUIP_LEATHER];
      TrackBar_EquipRateLeather.Step := 5;
      TrackBar_EquipRateLeather.OnChange := Town_DefenceChange;

      TrackBar_EquipRateIron := TKMTrackBar.Create(Panel_Defence, 0, 164, TB_WIDTH, 10, 300);
      TrackBar_EquipRateIron.Caption := fTextLibrary[TX_MAPED_AI_DEFENSE_EQUIP_IRON];
      TrackBar_EquipRateIron.Step := 5;
      TrackBar_EquipRateIron.OnChange := Town_DefenceChange;

      TrackBar_RecruitCount := TKMTrackBar.Create(Panel_Defence, 0, 208, TB_WIDTH, 1, 20);
      TrackBar_RecruitCount.Caption := fTextLibrary[TX_MAPED_AI_RECRUITS];
      TrackBar_RecruitCount.Hint := fTextLibrary[TX_MAPED_AI_RECRUITS_HINT];
      TrackBar_RecruitCount.OnChange := Town_DefenceChange;

      CheckBox_MaxSoldiers := TKMCheckBox.Create(Panel_Defence, 0, 252, TB_WIDTH, 20, fTextLibrary[TX_MAPED_AI_MAX_SOLDIERS], fnt_Metal);
      CheckBox_MaxSoldiers.Hint := fTextLibrary[TX_MAPED_AI_MAX_SOLDIERS_ENABLE_HINT];
      CheckBox_MaxSoldiers.OnClick := Town_DefenceChange;
      TrackBar_MaxSoldiers := TKMTrackBar.Create(Panel_Defence, 20, 270, TB_WIDTH - 20, 0, 500);
      TrackBar_MaxSoldiers.Caption := '';
      TrackBar_MaxSoldiers.Hint := fTextLibrary[TX_MAPED_AI_MAX_SOLDIERS_HINT];
      TrackBar_MaxSoldiers.Step := 5;
      TrackBar_MaxSoldiers.OnChange := Town_DefenceChange;

      Button_EditFormations := TKMButton.Create(Panel_Defence, 0, 300, TB_WIDTH, 25, fTextLibrary[TX_MAPED_AI_FORMATIONS], bsGame);
      Button_EditFormations.OnClick := Formations_Show;

    //Offence settings
    Panel_Offence := TKMPanel.Create(Panel_Town, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_Offence, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_AI_ATTACK], fnt_Outline, taCenter);

      CheckBox_AutoAttack := TKMCheckBox.Create(Panel_Offence, 0, 30, TB_WIDTH, 20, fTextLibrary[TX_MAPED_AI_ATTACK_AUTO], fnt_Metal);
      CheckBox_AutoAttack.Disable;

      ColumnBox_Attacks := TKMColumnBox.Create(Panel_Offence, 0, 50, TB_WIDTH, 210, fnt_Game, bsGame);
      ColumnBox_Attacks.SetColumns(fnt_Outline,
        [fTextLibrary[TX_MAPED_AI_ATTACK_COL_TYPE],
         fTextLibrary[TX_MAPED_AI_ATTACK_COL_DELAY],
         fTextLibrary[TX_MAPED_AI_ATTACK_COL_MEN],
         fTextLibrary[TX_MAPED_AI_ATTACK_COL_TARGET],
         fTextLibrary[TX_MAPED_AI_ATTACK_COL_LOC]], [0, 20, 60, 100, 130]);
      ColumnBox_Attacks.OnClick := Attacks_ListClick;
      ColumnBox_Attacks.OnDoubleClick := Attacks_ListDoubleClick;

      Button_AttacksAdd := TKMButton.Create(Panel_Offence, 0, 270, 25, 25, '+', bsGame);
      Button_AttacksAdd.OnClick := Attacks_Add;
      Button_AttacksDel := TKMButton.Create(Panel_Offence, 30, 270, 25, 25, 'X', bsGame);
      Button_AttacksDel.OnClick := Attacks_Del;
end;


procedure TKMapEdInterface.Create_Player;
const
  TabGlyph: array [TKMPlayerTab] of Word    = (8,         1159,     38,    327,   393);
  TabRXX  : array [TKMPlayerTab] of TRXType = (rxGuiMain, rxHouses, rxGui, rxGui, rxGui);
var
  I: Integer;
  Col: array [0..255] of TColor4;
  PT: TKMPlayerTab;
begin
  Panel_Player := TKMPanel.Create(Panel_Common,0,45, TB_WIDTH,28);

    for PT := Low(TKMPlayerTab) to High(TKMPlayerTab) do
    begin
      Button_Player[PT] := TKMButton.Create(Panel_Player, SMALL_PAD_W * Byte(PT), 0, SMALL_TAB_W, SMALL_TAB_H,  TabGlyph[PT], TabRXX[PT], bsGame);
      Button_Player[PT].OnClick := SwitchPage;
    end;

    //Goals
    Panel_Goals := TKMPanel.Create(Panel_Player,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Goals, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_GOALS], fnt_Outline, taCenter);
      ColumnBox_Goals := TKMColumnBox.Create(Panel_Goals, 0, 30, TB_WIDTH, 230, fnt_Game, bsGame);
      ColumnBox_Goals.SetColumns(fnt_Outline,
        [fTextLibrary[TX_MAPED_GOALS_TYPE],
         fTextLibrary[TX_MAPED_GOALS_CONDITION],
         fTextLibrary[TX_MAPED_GOALS_PLAYER],
         fTextLibrary[TX_MAPED_GOALS_TIME],
         fTextLibrary[TX_MAPED_GOALS_MESSAGE]], [0, 20, 120, 140, 160]);
      ColumnBox_Goals.OnClick := Goals_ListClick;
      ColumnBox_Goals.OnDoubleClick := Goals_ListDoubleClick;

      Button_GoalsAdd := TKMButton.Create(Panel_Goals, 0, 270, 25, 25, '+', bsGame);
      Button_GoalsAdd.OnClick := Goals_Add;
      Button_GoalsDel := TKMButton.Create(Panel_Goals, 30, 270, 25, 25, 'X', bsGame);
      Button_GoalsDel.OnClick := Goals_Del;

    //Players color
    Panel_Color := TKMPanel.Create(Panel_Player, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_Color, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_PLAYER_COLORS], fnt_Outline, taCenter);
      TKMBevel.Create(Panel_Color, 0, 30, TB_WIDTH, 210);
      ColorSwatch_Color := TKMColorSwatch.Create(Panel_Color, 0, 32, 16, 16, 11);
      for I := 0 to 255 do Col[I] := fResource.Palettes.DefDal.Color32(I);
      ColorSwatch_Color.SetColors(Col);
      ColorSwatch_Color.OnClick := Player_ColorClick;

    //Allow/Block house building
    Panel_BlockHouse := TKMPanel.Create(Panel_Player, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_BlockHouse, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_BLOCK_HOUSES], fnt_Outline, taCenter);
      for I := 1 to GUI_HOUSE_COUNT do
      if GUIHouseOrder[I] <> ht_None then
      begin
        Button_BlockHouse[I] := TKMButtonFlat.Create(Panel_BlockHouse, ((I-1) mod 5)*37, 30 + ((I-1) div 5)*37,33,33,fResource.HouseDat[GUIHouseOrder[I]].GUIIcon);
        Button_BlockHouse[I].Hint := fResource.HouseDat[GUIHouseOrder[I]].HouseName;
        Button_BlockHouse[I].OnClick := Player_BlockHouseClick;
        Button_BlockHouse[I].Tag := I;
        Image_BlockHouse[I] := TKMImage.Create(Panel_BlockHouse, ((I-1) mod 5)*37 + 13, 30 + ((I-1) div 5)*37 + 13, 16, 16, 0, rxGuiMain);
        Image_BlockHouse[I].Hitable := False;
        Image_BlockHouse[I].ImageCenter;
      end;

    //Allow/Block ware trading
    Panel_BlockTrade := TKMPanel.Create(Panel_Player, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_BlockTrade, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_BLOCK_TRADE], fnt_Outline, taCenter);
      for I := 1 to STORE_RES_COUNT do
      begin
        Button_BlockTrade[I] := TKMButtonFlat.Create(Panel_BlockTrade, ((I-1) mod 5)*37, 30 + ((I-1) div 5)*37,33,33, 0);
        Button_BlockTrade[I].TexID := fResource.Wares[StoreResType[I]].GUIIcon;
        Button_BlockTrade[I].Hint := fResource.Wares[StoreResType[I]].Title;
        Button_BlockTrade[I].OnClick := Player_BlockTradeClick;
        Button_BlockTrade[I].Tag := I;
        Image_BlockTrade[I] := TKMImage.Create(Panel_BlockTrade, ((I-1) mod 5)*37 + 13, 30 + ((I-1) div 5)*37 + 13, 16, 16, 0, rxGuiMain);
        Image_BlockTrade[I].Hitable := False;
        Image_BlockTrade[I].ImageCenter;
      end;

    //FOW settings
    Panel_Markers := TKMPanel.Create(Panel_Player, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_Markers, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_FOG], fnt_Outline, taCenter);
      Button_Reveal         := TKMButtonFlat.Create(Panel_Markers, 0, 30, 33, 33, 394);
      Button_Reveal.Hint    := fTextLibrary[TX_MAPED_FOG_HINT];
      Button_Reveal.OnClick := Player_MarkerClick;
      TrackBar_RevealNewSize  := TKMTrackBar.Create(Panel_Markers, 37, 35, 140, 1, 64);
      TrackBar_RevealNewSize.OnChange := Player_MarkerClick;
      TrackBar_RevealNewSize.Position := 8;
      CheckBox_RevealAll          := TKMCheckBox.Create(Panel_Markers, 0, 75, 140, 20, fTextLibrary[TX_MAPED_FOG_ALL], fnt_Metal);
      CheckBox_RevealAll.OnClick  := Player_MarkerClick;
      TKMLabel.Create(Panel_Markers, 0, 100, TB_WIDTH, 0, fTextLibrary[TX_MAPED_FOG_CENTER], fnt_Outline, taCenter);
      Button_CenterScreen         := TKMButtonFlat.Create(Panel_Markers, 0, 120, 33, 33, 391);
      Button_CenterScreen.Hint    := fTextLibrary[TX_MAPED_FOG_CENTER_HINT];
      Button_CenterScreen.OnClick := Player_MarkerClick;
      Button_PlayerCenterScreen    := TKMButton.Create(Panel_Markers, 40, 120, 80, 33, '[X,Y]', bsGame);
      Button_PlayerCenterScreen.OnClick := Player_MarkerClick;
      Button_PlayerCenterScreen.Hint := fTextLibrary[TX_MAPED_FOG_CENTER_JUMP];
end;


procedure TKMapEdInterface.Create_Mission;
var I,K: Integer;
begin
  Panel_Mission := TKMPanel.Create(Panel_Common, 0, 45, TB_WIDTH, 28);
    Button_Mission[1] := TKMButton.Create(Panel_Mission, SMALL_PAD_W * 0, 0, SMALL_TAB_W, SMALL_TAB_H, 41, rxGui, bsGame);
    Button_Mission[2] := TKMButton.Create(Panel_Mission, SMALL_PAD_W * 1, 0, SMALL_TAB_W, SMALL_TAB_H, 386, rxGui, bsGame);
    Button_Mission[3] := TKMButton.Create(Panel_Mission, SMALL_PAD_W * 2, 0, SMALL_TAB_W, SMALL_TAB_H, 656, rxGui, bsGame);
    for I := 1 to 3 do Button_Mission[I].OnClick := SwitchPage;

    Panel_Mode := TKMPanel.Create(Panel_Mission,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Mode, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_MISSION_MODE], fnt_Outline, taCenter);
      Radio_MissionMode := TKMRadioGroup.Create(Panel_Mode, 0, 30, TB_WIDTH, 40, fnt_Metal);
      Radio_MissionMode.Add(fTextLibrary[TX_MAPED_MISSION_NORMAL]);
      Radio_MissionMode.Add(fTextLibrary[TX_MAPED_MISSION_TACTIC]);
      Radio_MissionMode.OnChange := Mission_ModeChange;

    Panel_Alliances := TKMPanel.Create(Panel_Mission,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Alliances, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_ALLIANCE], fnt_Outline, taCenter);
      for I := 0 to MAX_PLAYERS - 1 do
      begin
        TKMLabel.Create(Panel_Alliances,24+I*20+2,30,20,20,inttostr(I+1),fnt_Outline,taLeft);
        TKMLabel.Create(Panel_Alliances,4,50+I*25,20,20,inttostr(I+1),fnt_Outline,taLeft);
        for K := 0 to MAX_PLAYERS - 1 do
        begin
          CheckBox_Alliances[I,K] := TKMCheckBox.Create(Panel_Alliances, 20+K*20, 46+I*25, 20, 20, '', fnt_Metal);
          CheckBox_Alliances[I,K].Tag       := I * MAX_PLAYERS + K;
          CheckBox_Alliances[I,K].OnClick   := Mission_AlliancesChange;
        end;
      end;

      //It does not have OnClick event for a reason:
      // - we don't have a rule to make alliances symmetrical yet
      CheckBox_AlliancesSym := TKMCheckBox.Create(Panel_Alliances, 0, 50+MAX_PLAYERS*25, TB_WIDTH, 20, fTextLibrary[TX_MAPED_ALLIANCE_SYMMETRIC], fnt_Metal);
      CheckBox_AlliancesSym.Checked := true;
      CheckBox_AlliancesSym.Disable;

    Panel_PlayerTypes := TKMPanel.Create(Panel_Mission, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_PlayerTypes, 0, PAGE_TITLE_Y, TB_WIDTH, 0, fTextLibrary[TX_MAPED_PLAYERS_TYPE], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_PlayerTypes,  4, 30, 20, 20, '#',       fnt_Grey, taLeft);
      TKMLabel.Create(Panel_PlayerTypes, 24, 30, 60, 20, fTextLibrary[TX_MAPED_PLAYERS_DEFAULT], fnt_Grey, taLeft);
      TKMImage.Create(Panel_PlayerTypes,104, 30, 60, 20, 588, rxGui);
      TKMImage.Create(Panel_PlayerTypes,164, 30, 20, 20,  62, rxGuiMain);
      for I := 0 to MAX_PLAYERS - 1 do
      begin
        TKMLabel.Create(Panel_PlayerTypes,  4, 50+I*25, 20, 20, IntToStr(I+1), fnt_Outline, taLeft);
        for K := 0 to 2 do
        begin
          CheckBox_PlayerTypes[I,K] := TKMCheckBox.Create(Panel_PlayerTypes, 44+K*60, 48+I*25, 20, 20, '', fnt_Metal);
          CheckBox_PlayerTypes[I,K].Tag       := I;
          CheckBox_PlayerTypes[I,K].OnClick   := Mission_PlayerTypesChange;
        end;
      end;
end;


{Menu page}
procedure TKMapEdInterface.Create_Menu;
begin
  Panel_Menu := TKMPanel.Create(Panel_Common, 0, 45, TB_WIDTH, 400);
    Button_Menu_Load := TKMButton.Create(Panel_Menu, 0, 20, TB_WIDTH, 30, fTextLibrary[TX_MAPED_LOAD_TITLE], bsGame);
    Button_Menu_Load.OnClick := SwitchPage;
    Button_Menu_Load.Hint := fTextLibrary[TX_MAPED_LOAD_TITLE];
    Button_Menu_Save := TKMButton.Create(Panel_Menu, 0, 60, TB_WIDTH, 30, fTextLibrary[TX_MAPED_SAVE_TITLE], bsGame);
    Button_Menu_Save.OnClick := SwitchPage;
    Button_Menu_Save.Hint := fTextLibrary[TX_MAPED_SAVE_TITLE];
    Button_Menu_Settings := TKMButton.Create(Panel_Menu, 0, 100, TB_WIDTH, 30, fTextLibrary[TX_MENU_SETTINGS], bsGame);
    Button_Menu_Settings.Hint := fTextLibrary[TX_MENU_SETTINGS];
    Button_Menu_Settings.Disable;
    Button_Menu_Quit := TKMButton.Create(Panel_Menu, 0, 180, TB_WIDTH, 30, fTextLibrary[TX_MENU_QUIT_MAPED], bsGame);
    Button_Menu_Quit.Hint := fTextLibrary[TX_MENU_QUIT_MAPED];
    Button_Menu_Quit.OnClick := SwitchPage;
end;


{Save page}
procedure TKMapEdInterface.Create_MenuSave;
begin
  Panel_Save := TKMPanel.Create(Panel_Common,0,45,TB_WIDTH,400);
    TKMBevel.Create(Panel_Save, 0, 30, TB_WIDTH, 37);
    Radio_Save_MapType  := TKMRadioGroup.Create(Panel_Save,4,32,TB_WIDTH,35,fnt_Grey);
    Radio_Save_MapType.ItemIndex := 0;
    Radio_Save_MapType.Add(fTextLibrary[TX_MENU_MAPED_SPMAPS]);
    Radio_Save_MapType.Add(fTextLibrary[TX_MENU_MAPED_MPMAPS]);
    Radio_Save_MapType.OnChange := Menu_SaveClick;
    TKMLabel.Create(Panel_Save,0,90,TB_WIDTH,20,fTextLibrary[TX_MAPED_SAVE_TITLE],fnt_Outline,taCenter);
    Edit_SaveName       := TKMEdit.Create(Panel_Save,0,110,TB_WIDTH,20, fnt_Grey);
    Edit_SaveName.AllowedChars := acFileName;
    Label_SaveExists    := TKMLabel.Create(Panel_Save,0,140,TB_WIDTH,0,fTextLibrary[TX_MAPED_SAVE_EXISTS],fnt_Outline,taCenter);
    CheckBox_SaveExists := TKMCheckBox.Create(Panel_Save,0,160,TB_WIDTH,20,fTextLibrary[TX_MAPED_SAVE_OVERWRITE], fnt_Metal);
    Button_SaveSave     := TKMButton.Create(Panel_Save,0,180,TB_WIDTH,30,fTextLibrary[TX_MAPED_SAVE],bsGame);
    Button_SaveCancel   := TKMButton.Create(Panel_Save,0,220,TB_WIDTH,30,fTextLibrary[TX_MAPED_SAVE_CANCEL],bsGame);
    Edit_SaveName.OnChange      := Menu_SaveClick;
    CheckBox_SaveExists.OnClick := Menu_SaveClick;
    Button_SaveSave.OnClick     := Menu_SaveClick;
    Button_SaveCancel.OnClick   := SwitchPage;
end;


{Load page}
procedure TKMapEdInterface.Create_MenuLoad;
begin
  Panel_Load := TKMPanel.Create(Panel_Common,0,45,TB_WIDTH,400);
    TKMLabel.Create(Panel_Load, 0, PAGE_TITLE_Y, TB_WIDTH, 30, fTextLibrary[TX_MAPED_LOAD_TITLE], fnt_Outline, taLeft);
    TKMBevel.Create(Panel_Load, 0, 30, TB_WIDTH, 38);
    Radio_Load_MapType := TKMRadioGroup.Create(Panel_Load,0,32,TB_WIDTH,35,fnt_Grey);
    Radio_Load_MapType.ItemIndex := 0;
    Radio_Load_MapType.Add(fTextLibrary[TX_MENU_MAPED_SPMAPS]);
    Radio_Load_MapType.Add(fTextLibrary[TX_MENU_MAPED_MPMAPS]);
    Radio_Load_MapType.OnChange := Menu_LoadChange;
    ListBox_Load := TKMListBox.Create(Panel_Load, 0, 85, TB_WIDTH, 205, fnt_Grey, bsGame);
    ListBox_Load.ItemHeight := 18;
    ListBox_Load.AutoHideScrollBar := True;
    Button_LoadLoad     := TKMButton.Create(Panel_Load,0,300,TB_WIDTH,30,fTextLibrary[TX_MAPED_LOAD],bsGame);
    Button_LoadCancel   := TKMButton.Create(Panel_Load,0,335,TB_WIDTH,30,fTextLibrary[TX_MAPED_LOAD_CANCEL],bsGame);
    Button_LoadLoad.OnClick     := Menu_LoadClick;
    Button_LoadCancel.OnClick   := SwitchPage;
end;


{Quit page}
procedure TKMapEdInterface.Create_MenuQuit;
begin
  Panel_Quit := TKMPanel.Create(Panel_Common, 0, 45, TB_WIDTH, 400);
    TKMLabel.Create(Panel_Quit, 0, 40, TB_WIDTH, 60, fTextLibrary[TX_MAPED_LOAD_UNSAVED], fnt_Outline, taCenter);
    Button_Quit_Yes := TKMButton.Create(Panel_Quit, 0, 100, TB_WIDTH, 30, fTextLibrary[TX_MENU_QUIT_MAPED], bsGame);
    Button_Quit_No  := TKMButton.Create(Panel_Quit, 0, 140, TB_WIDTH, 30, fTextLibrary[TX_MENU_DONT_QUIT_MISSION], bsGame);
    Button_Quit_Yes.Hint    := fTextLibrary[TX_MENU_QUIT_MAPED];
    Button_Quit_No.Hint     := fTextLibrary[TX_MENU_DONT_QUIT_MISSION];
    Button_Quit_Yes.OnClick := Menu_QuitClick;
    Button_Quit_No.OnClick  := SwitchPage;
end;


procedure TKMapEdInterface.Create_Extra;
begin
  Panel_Extra := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - 190, 600, 190);
  Panel_Extra.Anchors := [akLeft, akBottom];
  Panel_Extra.Hide;

    with TKMImage.Create(Panel_Extra, 0, 0, 600, 190, 409) do
    begin
      Anchors := [akLeft, akTop, akBottom];
      ImageAnchors := [akLeft, akRight, akTop];
    end;

    Image_ExtraClose := TKMImage.Create(Panel_Extra, 600 - 76, 24, 32, 32, 52);
    Image_ExtraClose.Anchors := [akTop, akRight];
    Image_ExtraClose.Hint := fTextLibrary[TX_MSG_CLOSE_HINT];
    Image_ExtraClose.OnClick := ExtraMessage_Switch;
    Image_ExtraClose.HighlightOnMouseOver := True;

    TrackBar_Passability := TKMTrackBar.Create(Panel_Extra, 50, 70, 180, 0, Byte(High(TPassability)));
    TrackBar_Passability.Font := fnt_Antiqua;
    TrackBar_Passability.Caption := fTextLibrary[TX_MAPED_VIEW_PASSABILITY];
    TrackBar_Passability.Position := 0; //Disabled by default
    TrackBar_Passability.OnChange := Extra_Change;
    Label_Passability := TKMLabel.Create(Panel_Extra, 50, 114, 180, 0, 'Off', fnt_Antiqua, taLeft);

    CheckBox_ShowObjects := TKMCheckBox.Create(Panel_Extra, 250, 70, 180, 20, fTextLibrary[TX_MAPED_VIEW_OBJECTS], fnt_Antiqua);
    CheckBox_ShowObjects.Checked := True; //Enabled by default
    CheckBox_ShowObjects.OnClick := Extra_Change;
    CheckBox_ShowHouses := TKMCheckBox.Create(Panel_Extra, 250, 90, 180, 20, fTextLibrary[TX_MAPED_VIEW_HOUSES], fnt_Antiqua);
    CheckBox_ShowHouses.Checked := True; //Enabled by default
    CheckBox_ShowHouses.OnClick := Extra_Change;
    CheckBox_ShowUnits := TKMCheckBox.Create(Panel_Extra, 250, 110, 180, 20, fTextLibrary[TX_MAPED_VIEW_UNITS], fnt_Antiqua);
    CheckBox_ShowUnits.Checked := True; //Enabled by default
    CheckBox_ShowUnits.OnClick := Extra_Change;
    CheckBox_ShowDeposits := TKMCheckBox.Create(Panel_Extra, 250, 130, 180, 20, fTextLibrary[TX_MAPED_VIEW_DEPOSISTS], fnt_Antiqua);
    CheckBox_ShowDeposits.Checked := True; //Enabled by default
    CheckBox_ShowDeposits.OnClick := Extra_Change;

    //dropdown list needs to be ontop other buttons created on Panel_Main
    Dropbox_PlayerFOW := TKMDropList.Create(Panel_Extra, 460, 70, 160, 20, fnt_Metal, '', bsGame);
    Dropbox_PlayerFOW.Hint := fTextLibrary[TX_REPLAY_PLAYER_PERSPECTIVE];
    Dropbox_PlayerFOW.OnChange := Player_FOWChange;
    //todo: This feature isn't working properly yet so it's hidden. FOW should be set by
    //revealers list and current locations of units/houses (must update when they move)
    Dropbox_PlayerFOW.Hide;
end;


procedure TKMapEdInterface.Create_Message;
begin
  Panel_Message := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - 190, Panel_Main.Width - TOOLBAR_WIDTH, 190);
  Panel_Message.Anchors := [akLeft, akBottom];
  Panel_Message.Hide;

    with TKMImage.Create(Panel_Message, 0, 0, 800, 190, 409) do
    begin
      Anchors := [akLeft, akTop, akBottom];
      ImageStretch;
    end;

    Image_MessageClose := TKMImage.Create(Panel_Message, 800-35, 20, 32, 32, 52);
    Image_MessageClose.Anchors := [akTop, akRight];
    Image_MessageClose.Hint := fTextLibrary[TX_MSG_CLOSE_HINT];
    Image_MessageClose.OnClick := ExtraMessage_Switch;
    Image_MessageClose.HighlightOnMouseOver := True;

    Label_Message := TKMLabel.Create(Panel_Message, 40, 50, 7000, 0, '', fnt_Grey, taLeft);
end;


procedure TKMapEdInterface.Create_AttackPopUp;
const
  T: array [TGroupType] of Integer = (TX_MAPED_AI_ATTACK_TYPE_MELEE, TX_MAPED_AI_ATTACK_TYPE_ANTIHORSE, TX_MAPED_AI_ATTACK_TYPE_RANGED, TX_MAPED_AI_ATTACK_TYPE_MOUNTED);
  SIZE_X = 570;
  SIZE_Y = 360;
var
  GT: TGroupType;
begin
  Panel_Attack := TKMPanel.Create(Panel_Main, 362, 250, SIZE_X, SIZE_Y);
  Panel_Attack.Anchors := [];
  Panel_Attack.Hide;

    TKMBevel.Create(Panel_Attack, -1000,  -1000, 4000, 4000);
    with TKMImage.Create(Panel_Attack, -20, -50, SIZE_X+40, SIZE_Y+60, 15, rxGuiMain) do ImageStretch;
    TKMBevel.Create(Panel_Attack,   0,  0, SIZE_X, SIZE_Y);
    TKMLabel.Create(Panel_Attack, SIZE_X div 2, 10, fTextLibrary[TX_MAPED_AI_ATTACK_INFO], fnt_Outline, taCenter);

    TKMLabel.Create(Panel_Attack, 20, 40, fTextLibrary[TX_MAPED_AI_ATTACK_COL_TYPE], fnt_Metal, taLeft);
    Radio_AttackType := TKMRadioGroup.Create(Panel_Attack, 20, 60, 80, 40, fnt_Metal);
    Radio_AttackType.Add(fTextLibrary[TX_MAPED_AI_ATTACK_TYPE_ONCE]);
    Radio_AttackType.Add(fTextLibrary[TX_MAPED_AI_ATTACK_TYPE_REP]);
    Radio_AttackType.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, 130, 40, fTextLibrary[TX_MAPED_AI_ATTACK_DELAY], fnt_Metal, taLeft);
    NumEdit_AttackDelay := TKMNumericEdit.Create(Panel_Attack, 130, 60, 0, High(SmallInt));
    NumEdit_AttackDelay.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, 240, 40, fTextLibrary[TX_MAPED_AI_ATTACK_COL_MEN], fnt_Metal, taLeft);
    NumEdit_AttackMen := TKMNumericEdit.Create(Panel_Attack, 240, 60, 0, 1000);
    NumEdit_AttackMen.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, 340, 40, fTextLibrary[TX_MAPED_AI_ATTACK_COUNT], fnt_Metal, taLeft);
    for GT := Low(TGroupType) to High(TGroupType) do
    begin
      TKMLabel.Create(Panel_Attack, 425, 60 + Byte(GT) * 20, 0, 0, fTextLibrary[T[GT]], fnt_Metal, taLeft);
      NumEdit_AttackAmount[GT] := TKMNumericEdit.Create(Panel_Attack, 340, 60 + Byte(GT) * 20, 0, 255);
      NumEdit_AttackAmount[GT].OnChange := Attack_Change;
    end;

    CheckBox_AttackTakeAll := TKMCheckBox.Create(Panel_Attack, 340, 145, 160, 20, fTextLibrary[TX_MAPED_AI_ATTACK_TAKE_ALL], fnt_Metal);
    CheckBox_AttackTakeAll.OnClick := Attack_Change;

    //Second row

    TKMLabel.Create(Panel_Attack, 20, 170, fTextLibrary[TX_MAPED_AI_ATTACK_COL_TARGET], fnt_Metal, taLeft);
    Radio_AttackTarget := TKMRadioGroup.Create(Panel_Attack, 20, 190, 160, 80, fnt_Metal);
    Radio_AttackTarget.Add(fTextLibrary[TX_MAPED_AI_TARGET_CLOSEST]);
    Radio_AttackTarget.Add(fTextLibrary[TX_MAPED_AI_TARGET_HOUSE1]);
    Radio_AttackTarget.Add(fTextLibrary[TX_MAPED_AI_TARGET_HOUSE2]);
    Radio_AttackTarget.Add(fTextLibrary[TX_MAPED_AI_TARGET_CUSTOM]);
    Radio_AttackTarget.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, 200, 170, fTextLibrary[TX_MAPED_AI_TARGET_POS], fnt_Metal, taLeft);
    NumEdit_AttackLocX := TKMNumericEdit.Create(Panel_Attack, 200, 190, 0, MAX_MAP_SIZE);
    NumEdit_AttackLocX.OnChange := Attack_Change;
    NumEdit_AttackLocY := TKMNumericEdit.Create(Panel_Attack, 200, 210, 0, MAX_MAP_SIZE);
    NumEdit_AttackLocY.OnChange := Attack_Change;

    TKMLabel.Create(Panel_Attack, 200, 240, 'Range (untested)', fnt_Metal, taLeft);
    TrackBar_AttackRange := TKMTrackBar.Create(Panel_Attack, 200, 260, 100, 0, 255);
    TrackBar_AttackRange.Disable;
    TrackBar_AttackRange.OnChange := Attack_Change;

    Button_AttackOk := TKMButton.Create(Panel_Attack, SIZE_X-20-320-10, SIZE_Y - 50, 160, 30, fTextLibrary[TX_MAPED_OK], bsMenu);
    Button_AttackOk.OnClick := Attack_Close;
    Button_AttackCancel := TKMButton.Create(Panel_Attack, SIZE_X-20-160, SIZE_Y - 50, 160, 30, fTextLibrary[TX_MAPED_CANCEL], bsMenu);
    Button_AttackCancel.OnClick := Attack_Close;
end;


procedure TKMapEdInterface.Create_FormationsPopUp;
const
  T: array [TGroupType] of Integer = (TX_MAPED_AI_ATTACK_TYPE_MELEE, TX_MAPED_AI_ATTACK_TYPE_ANTIHORSE, TX_MAPED_AI_ATTACK_TYPE_RANGED, TX_MAPED_AI_ATTACK_TYPE_MOUNTED);  SIZE_X = 570;
  SIZE_Y = 200;
var
  GT: TGroupType;
  Img: TKMImage;
begin
  Panel_Formations := TKMPanel.Create(Panel_Main, 362, 250, SIZE_X, SIZE_Y);
  Panel_Formations.Anchors := [];
  Panel_Formations.Hide;

    TKMBevel.Create(Panel_Formations, -1000,  -1000, 4000, 4000);
    Img := TKMImage.Create(Panel_Formations, -20, -50, SIZE_X+40, SIZE_Y+60, 15, rxGuiMain);
    Img.ImageStretch;
    TKMBevel.Create(Panel_Formations,   0,  0, SIZE_X, SIZE_Y);
    TKMLabel.Create(Panel_Formations, SIZE_X div 2, 10, fTextLibrary[TX_MAPED_AI_FORMATIONS_TITLE], fnt_Outline, taCenter);

    Image_FormationsFlag := TKMImage.Create(Panel_Formations, 10, 10, 0, 0, 30, rxGuiMain);

    TKMLabel.Create(Panel_Formations, 20, 70, 80, 0, fTextLibrary[TX_MAPED_AI_FORMATIONS_COUNT], fnt_Metal, taLeft);
    TKMLabel.Create(Panel_Formations, 20, 95, 80, 0, fTextLibrary[TX_MAPED_AI_FORMATIONS_COLUMNS], fnt_Metal, taLeft);

    for GT := Low(TGroupType) to High(TGroupType) do
    begin
      TKMLabel.Create(Panel_Formations, 130 + Byte(GT) * 110 + 32, 50, 0, 0, fTextLibrary[T[GT]], fnt_Metal, taCenter);
      NumEdit_FormationsCount[GT] := TKMNumericEdit.Create(Panel_Formations, 130 + Byte(GT) * 110, 70, 1, 255);
      NumEdit_FormationsColumns[GT] := TKMNumericEdit.Create(Panel_Formations, 130 + Byte(GT) * 110, 95, 1, 255);
    end;

    Button_Formations_Ok := TKMButton.Create(Panel_Formations, SIZE_X-20-320-10, 150, 160, 30, fTextLibrary[TX_MAPED_OK], bsMenu);
    Button_Formations_Ok.OnClick := Formations_Close;
    Button_Formations_Cancel := TKMButton.Create(Panel_Formations, SIZE_X-20-160, 150, 160, 30, fTextLibrary[TX_MAPED_CANCEL], bsMenu);
    Button_Formations_Cancel.OnClick := Formations_Close;
end;


procedure TKMapEdInterface.Create_GoalPopUp;
const
  SIZE_X = 600;
  SIZE_Y = 300;
var
  Img: TKMImage;
begin
  Panel_Goal := TKMPanel.Create(Panel_Main, 362, 250, SIZE_X, SIZE_Y);
  Panel_Goal.Anchors := [];
  Panel_Goal.Hide;

    TKMBevel.Create(Panel_Goal, -1000,  -1000, 4000, 4000);
    Img := TKMImage.Create(Panel_Goal, -20, -50, SIZE_X+40, SIZE_Y+60, 15, rxGuiMain);
    Img.ImageStretch;
    TKMBevel.Create(Panel_Goal,   0,  0, SIZE_X, SIZE_Y);
    TKMLabel.Create(Panel_Goal, SIZE_X div 2, 10, fTextLibrary[TX_MAPED_GOALS_TITLE], fnt_Outline, taCenter);

    Image_FormationsFlag := TKMImage.Create(Panel_Goal, 10, 10, 0, 0, 30, rxGuiMain);

    TKMLabel.Create(Panel_Goal, 20, 40, 100, 0, fTextLibrary[TX_MAPED_GOALS_TYPE], fnt_Metal, taLeft);
    Radio_GoalType := TKMRadioGroup.Create(Panel_Goal, 20, 60, 100, 60, fnt_Metal);
    Radio_GoalType.Add(fTextLibrary[TX_MAPED_GOALS_TYPE_NONE]);
    Radio_GoalType.Add(fTextLibrary[TX_MAPED_GOALS_TYPE_VICTORY]);
    Radio_GoalType.Add(fTextLibrary[TX_MAPED_GOALS_TYPE_SURVIVE]);
    Radio_GoalType.OnChange := Goal_Change;

    TKMLabel.Create(Panel_Goal, 140, 40, 180, 0, fTextLibrary[TX_MAPED_GOALS_CONDITION], fnt_Metal, taLeft);
    Radio_GoalCondition := TKMRadioGroup.Create(Panel_Goal, 140, 60, 180, 180, fnt_Metal);
    Radio_GoalCondition.Add(fTextLibrary[TX_MAPED_GOALS_CONDITION_NONE], False);
    Radio_GoalCondition.Add(fTextLibrary[TX_MAPED_GOALS_CONDITION_TUTORIAL], False);
    Radio_GoalCondition.Add(fTextLibrary[TX_MAPED_GOALS_CONDITION_TIME], False);
    Radio_GoalCondition.Add(fTextLibrary[TX_MAPED_GOALS_CONDITION_BUILDS]);
    Radio_GoalCondition.Add(fTextLibrary[TX_MAPED_GOALS_CONDITION_TROOPS]);
    Radio_GoalCondition.Add(fTextLibrary[TX_MAPED_GOALS_CONDITION_UNKNOWN], False);
    Radio_GoalCondition.Add(fTextLibrary[TX_MAPED_GOALS_CONDITION_ASSETS]);
    Radio_GoalCondition.Add(fTextLibrary[TX_MAPED_GOALS_CONDITION_SERFS]);
    Radio_GoalCondition.Add(fTextLibrary[TX_MAPED_GOALS_CONDITION_ECONOMY]);
    Radio_GoalCondition.OnChange := Goal_Change;

    TKMLabel.Create(Panel_Goal, 330, 40, fTextLibrary[TX_MAPED_GOALS_PLAYER], fnt_Metal, taLeft);
    NumEdit_GoalPlayer := TKMNumericEdit.Create(Panel_Goal, 330, 60, 1, MAX_PLAYERS);
    NumEdit_GoalPlayer.OnChange := Goal_Change;

    TKMLabel.Create(Panel_Goal, 480, 40, fTextLibrary[TX_MAPED_GOALS_TIME], fnt_Metal, taLeft);
    NumEdit_GoalTime := TKMNumericEdit.Create(Panel_Goal, 480, 60, 0, 32767);
    NumEdit_GoalTime.OnChange := Goal_Change;
    NumEdit_GoalTime.SharedHint := 'This setting is deprecated, use scripts instead';

    TKMLabel.Create(Panel_Goal, 480, 90, fTextLibrary[TX_MAPED_GOALS_MESSAGE], fnt_Metal, taLeft);
    NumEdit_GoalMessage := TKMNumericEdit.Create(Panel_Goal, 480, 110, 0, 0);
    NumEdit_GoalMessage.SharedHint := 'This setting is deprecated, use scripts instead';

    Button_GoalOk := TKMButton.Create(Panel_Goal, SIZE_X-20-320-10, SIZE_Y - 50, 160, 30, fTextLibrary[TX_MAPED_OK], bsMenu);
    Button_GoalOk.OnClick := Goal_Close;
    Button_GoalCancel := TKMButton.Create(Panel_Goal, SIZE_X-20-160, SIZE_Y - 50, 160, 30, fTextLibrary[TX_MAPED_CANCEL], bsMenu);
    Button_GoalCancel.OnClick := Goal_Close;
end;


{Unit page}
procedure TKMapEdInterface.Create_Unit;
begin
  Panel_Unit := TKMPanel.Create(Panel_Common, 0, 45, TB_WIDTH, 400);
    Label_UnitName        := TKMLabel.Create(Panel_Unit,0,16,TB_WIDTH,0,'',fnt_Outline,taCenter);
    Image_UnitPic         := TKMImage.Create(Panel_Unit,0,38,54,100,521);
    Label_UnitCondition   := TKMLabel.Create(Panel_Unit,65,40,116,0,fTextLibrary[TX_UNIT_CONDITION],fnt_Grey,taCenter);
    KMConditionBar_Unit   := TKMPercentBar.Create(Panel_Unit,65,55,116,15);
    Label_UnitDescription := TKMLabel.Create(Panel_Unit,0,152,TB_WIDTH,200,'',fnt_Grey,taLeft); //Taken from LIB resource
    Label_UnitDescription.AutoWrap := True;

    Panel_Army := TKMPanel.Create(Panel_Unit, 0, 160, TB_WIDTH, 400);
    Button_Army_RotCCW  := TKMButton.Create(Panel_Army,       0,  0, 56, 40, 23, rxGui, bsGame);
    Button_Army_RotCW   := TKMButton.Create(Panel_Army,     124,  0, 56, 40, 24, rxGui, bsGame);
    Button_Army_ForUp   := TKMButton.Create(Panel_Army,       0, 46, 56, 40, 33, rxGui, bsGame);
    ImageStack_Army     := TKMImageStack.Create(Panel_Army,  62, 46, 56, 40, 43, 50);
    Label_ArmyCount     := TKMLabel.Create(Panel_Army,       62, 60, 56, 20, '-', fnt_Outline, taCenter);
    Button_Army_ForDown := TKMButton.Create(Panel_Army,     124, 46, 56, 40, 32, rxGui, bsGame);
    Button_Army_RotCW.OnClick   := Unit_ArmyChange1;
    Button_Army_RotCCW.OnClick  := Unit_ArmyChange1;
    Button_Army_ForUp.OnClick   := Unit_ArmyChange1;
    Button_Army_ForDown.OnClick := Unit_ArmyChange1;

    Button_ArmyDec      := TKMButton.Create(Panel_Army,  0,92,56,40,'-', bsGame);
    Button_ArmyFood     := TKMButton.Create(Panel_Army, 62,92,56,40,29, rxGui, bsGame);
    Button_ArmyInc      := TKMButton.Create(Panel_Army,124,92,56,40,'+', bsGame);
    Button_ArmyDec.OnClickEither  := Unit_ArmyChange2;
    Button_ArmyFood.OnClick       := Unit_ArmyChange1;
    Button_ArmyInc.OnClickEither  := Unit_ArmyChange2;

    //Group order
    //todo: Orders should be placed with a cursor (but keep numeric input as well?)
    TKMLabel.Create(Panel_Army, 0, 140, TB_WIDTH, 0, 'Group order', fnt_Outline, taLeft);
    DropBox_ArmyOrder   := TKMDropList.Create(Panel_Army, 0, 160, TB_WIDTH, 20, fnt_Metal, '', bsGame);
    DropBox_ArmyOrder.Add('None');
    DropBox_ArmyOrder.Add('Walk to');
    DropBox_ArmyOrder.Add('Attack position');
    DropBox_ArmyOrder.OnChange := Unit_ArmyChange1;
    TKMLabel.Create(Panel_Army, 0, 185, 'X:', fnt_Grey, taLeft);
    Edit_ArmyOrderX := TKMNumericEdit.Create(Panel_Army, 20, 185, 0, 255);
    Edit_ArmyOrderX.OnChange := Unit_ArmyChange1;
    TKMLabel.Create(Panel_Army, 0, 205, 'Y:', fnt_Grey, taLeft);
    Edit_ArmyOrderY := TKMNumericEdit.Create(Panel_Army, 20, 205, 0, 255);
    Edit_ArmyOrderY.OnChange := Unit_ArmyChange1;
    TKMLabel.Create(Panel_Army, 110, 185, 'Direction', fnt_Grey, taLeft);
    Edit_ArmyOrderDir := TKMNumericEdit.Create(Panel_Army, 110, 205, 0, 7);
    Edit_ArmyOrderDir.OnChange := Unit_ArmyChange1;
end;


{House description page}
procedure TKMapEdInterface.Create_House;
var
  I: Integer;
begin
  Panel_House := TKMPanel.Create(Panel_Common, 0, 45, TB_WIDTH, 400);
    //Thats common things
    Label_House := TKMLabel.Create(Panel_House, 0, 14, TB_WIDTH, 0, '', fnt_Outline, taCenter);
    Image_House_Logo := TKMImage.Create(Panel_House, 0, 41, 32, 32, 338);
    Image_House_Logo.ImageCenter;
    Image_House_Worker := TKMImage.Create(Panel_House, 30, 41, 32, 32, 141);
    Image_House_Worker.ImageCenter;
    Label_HouseHealth := TKMLabel.Create(Panel_House, 100, 41, 60, 20, fTextLibrary[TX_HOUSE_CONDITION], fnt_Mini, taCenter);
    Label_HouseHealth.FontColor := $FFE0E0E0;
    KMHealthBar_House := TKMPercentBar.Create(Panel_House, 100, 53, 60, 20);
    Button_HouseHealthDec := TKMButton.Create(Panel_House, 80, 53, 20, 20, '-', bsGame);
    Button_HouseHealthInc := TKMButton.Create(Panel_House, 160, 53, 20, 20, '+', bsGame);
    Button_HouseHealthDec.OnClickEither := House_HealthChange;
    Button_HouseHealthInc.OnClickEither := House_HealthChange;

    Label_House_Supply := TKMLabel.Create(Panel_House, 0, 85, TB_WIDTH, 0, fTextLibrary[TX_HOUSE_SUPPLIES], fnt_Grey, taCenter);

    for I := 0 to 3 do
    begin
      ResRow_Resource[I] := TKMResourceOrderRow.Create(Panel_House, 0, 105 + I * 25, TB_WIDTH, 20);
      ResRow_Resource[I].RX := rxGui;
      ResRow_Resource[I].OrderAdd.OnClickEither := House_HealthChange;
      ResRow_Resource[I].OrderRem.OnClickEither := House_HealthChange;
    end;
end;


{Store page}
procedure TKMapEdInterface.Create_HouseStore;
var I: Integer;
begin
  Panel_HouseStore := TKMPanel.Create(Panel_House,0,76,TB_WIDTH,400);
    for I := 1 to STORE_RES_COUNT do
    begin
      Button_Store[I] := TKMButtonFlat.Create(Panel_HouseStore, 2 + ((I-1)mod 5)*36,8+((I-1)div 5)*42,32,36,0);
      Button_Store[I].TexID := fResource.Wares[StoreResType[I]].GUIIcon;
      Button_Store[I].Tag := I;
      Button_Store[I].Hint := fResource.Wares[StoreResType[I]].Title;
      Button_Store[I].OnClick := House_StoreSelectWare;
    end;

    Button_StoreDec100      := TKMButton.Create(Panel_HouseStore,108,218,20,20,'<', bsGame);
    Button_StoreDec100.Tag  := 100;
    Button_StoreDec       := TKMButton.Create(Panel_HouseStore,108,238,20,20,'-', bsGame);
    Button_StoreDec.Tag   := 1;
    Label_Store_WareCount:= TKMLabel.Create (Panel_HouseStore,128,230,40,20,'',fnt_Metal,taCenter);
    Button_StoreInc100      := TKMButton.Create(Panel_HouseStore,168,218,20,20,'>', bsGame);
    Button_StoreInc100.Tag  := 100;
    Button_StoreInc       := TKMButton.Create(Panel_HouseStore,168,238,20,20,'+', bsGame);
    Button_StoreInc.Tag   := 1;
    Button_StoreDec100.OnClickEither := House_StoreEditCount;
    Button_StoreDec.OnClickEither    := House_StoreEditCount;
    Button_StoreInc100.OnClickEither := House_StoreEditCount;
    Button_StoreInc.OnClickEither    := House_StoreEditCount;
end;


{Barracks page}
procedure TKMapEdInterface.Create_HouseBarracks;
var i:Integer;
begin
  Panel_HouseBarracks:=TKMPanel.Create(Panel_House,0,76,TB_WIDTH,400);
    for i:=1 to BARRACKS_RES_COUNT do
    begin
      Button_Barracks[i]:=TKMButtonFlat.Create(Panel_HouseBarracks, ((i-1)mod 6)*31,8+((i-1)div 6)*42,28,38,0);
      Button_Barracks[i].Tag := i;
      Button_Barracks[i].TexID := fResource.Wares[BarracksResType[i]].GUIIcon;
      Button_Barracks[i].TexOffsetX := 1;
      Button_Barracks[i].TexOffsetY := 1;
      Button_Barracks[i].CapOffsetY := 2;
      Button_Barracks[i].Hint := fResource.Wares[BarracksResType[i]].Title;
      Button_Barracks[i].OnClick := House_BarracksSelectWare;
    end;
    Button_BarracksDec100     := TKMButton.Create(Panel_HouseBarracks,108,218,20,20,'<', bsGame);
    Button_BarracksDec100.Tag := 100;
    Button_BarracksDec      := TKMButton.Create(Panel_HouseBarracks,108,238,20,20,'-', bsGame);
    Button_BarracksDec.Tag  := 1;
    Label_Barracks_WareCount:= TKMLabel.Create (Panel_HouseBarracks,128,230,40,20,'',fnt_Metal,taCenter);
    Button_BarracksInc100     := TKMButton.Create(Panel_HouseBarracks,168,218,20,20,'>', bsGame);
    Button_BarracksInc100.Tag := 100;
    Button_BarracksInc      := TKMButton.Create(Panel_HouseBarracks,168,238,20,20,'+', bsGame);
    Button_BarracksInc.Tag  := 1;
    Button_BarracksDec100.OnClickEither := House_BarracksEditCount;
    Button_BarracksDec.OnClickEither    := House_BarracksEditCount;
    Button_BarracksInc100.OnClickEither := House_BarracksEditCount;
    Button_BarracksInc.OnClickEither    := House_BarracksEditCount;
end;


procedure TKMapEdInterface.Create_Marker;
begin
  Panel_Marker := TKMPanel.Create(Panel_Common, 0, 50, TB_WIDTH, 400);

  Label_MarkerType := TKMLabel.Create(Panel_Marker, 32, 10, TB_WIDTH, 0, '', fnt_Outline, taLeft);
  Image_MarkerPic := TKMImage.Create(Panel_Marker, 0, 10, 32, 32, 338);

    Panel_MarkerReveal := TKMPanel.Create(Panel_Marker, 0, 46, TB_WIDTH, 400);
      TrackBar_RevealSize := TKMTrackBar.Create(Panel_MarkerReveal, 0, 0, TB_WIDTH, 1, 64);
      TrackBar_RevealSize.Caption := 'Radius';
      TrackBar_RevealSize.OnChange := Marker_Change;
      Button_RevealDelete := TKMButton.Create(Panel_MarkerReveal, 0, 55, 25, 25, 340, rxGui, bsGame);
      Button_RevealDelete.Hint := 'Delete current marker';
      Button_RevealDelete.OnClick := Marker_Change;
      Button_RevealClose := TKMButton.Create(Panel_MarkerReveal, TB_WIDTH-100, 55, 100, 25, 'Close', bsGame);
      Button_RevealClose.Hint := 'Return to the markers page';
      Button_RevealClose.OnClick := Marker_Change;

    Panel_MarkerDefence := TKMPanel.Create(Panel_Marker, 0, 46, TB_WIDTH, 400);
      DropList_DefenceGroup := TKMDropList.Create(Panel_MarkerDefence, 0, 10, TB_WIDTH, 20, fnt_Game, '', bsGame);
      DropList_DefenceGroup.Add(fTextLibrary[TX_MAPED_AI_ATTACK_TYPE_MELEE]);
      DropList_DefenceGroup.Add(fTextLibrary[TX_MAPED_AI_ATTACK_TYPE_ANTIHORSE]);
      DropList_DefenceGroup.Add(fTextLibrary[TX_MAPED_AI_ATTACK_TYPE_RANGED]);
      DropList_DefenceGroup.Add(fTextLibrary[TX_MAPED_AI_ATTACK_TYPE_MOUNTED]);
      DropList_DefenceGroup.OnChange := Marker_Change;
      DropList_DefenceType := TKMDropList.Create(Panel_MarkerDefence, 0, 40, TB_WIDTH, 20, fnt_Game, '', bsGame);
      DropList_DefenceType.Add('FrontLine');
      DropList_DefenceType.Add('BackLine');
      DropList_DefenceType.OnChange := Marker_Change;
      TrackBar_DefenceRad := TKMTrackBar.Create(Panel_MarkerDefence, 0, 70, TB_WIDTH, 1, 128);
      TrackBar_DefenceRad.Caption := 'Defence radius';
      TrackBar_DefenceRad.OnChange := Marker_Change;
      Button_DefenceCCW  := TKMButton.Create(Panel_MarkerDefence, 0, 120, 50, 35, 23, rxGui, bsGame);
      Button_DefenceCCW.OnClick := Marker_Change;
      Button_DefenceCW := TKMButton.Create(Panel_MarkerDefence, 130, 120, 50, 35, 24, rxGui, bsGame);
      Button_DefenceCW.OnClick := Marker_Change;
      Button_DefenceDelete := TKMButton.Create(Panel_MarkerDefence, 0, 165, 25, 25, 340, rxGui, bsGame);
      Button_DefenceDelete.Hint := 'Delete current marker';
      Button_DefenceDelete.OnClick := Marker_Change;
      Button_DefenceClose := TKMButton.Create(Panel_MarkerDefence, TB_WIDTH-100, 165, 100, 25, 'Close', bsGame);
      Button_DefenceClose.Hint := 'Return to the defence page';
      Button_DefenceClose.OnClick := Marker_Change;
end;


//Should update any items changed by game (resource counts, hp, etc..)
procedure TKMapEdInterface.UpdateState(aTickCount: Cardinal);
const
  CAP_COLOR: array [Boolean] of TColor4 = ($80808080, $FFFFFFFF);
var
  I: Integer;
begin
  //Show players without assets in grey
  if aTickCount mod 10 = 0 then
  for I := 0 to MAX_PLAYERS - 1 do
    Button_PlayerSelect[I].FontColor := CAP_COLOR[fPlayers[I].HasAssets];

  if fMaps <> nil then fMaps.UpdateState;
  if fMapsMP <> nil then fMapsMP.UpdateState;
end;


//Update UI state according to game state
procedure TKMapEdInterface.SyncUI;
begin
  Player_UpdateColors;
  UpdateAITabsEnabled;

  Label_MissionName.Caption := fGame.GameName;

  MinimapView.SetMinimap(fGame.Minimap);
  MinimapView.SetViewport(fGame.Viewport);
end;


procedure TKMapEdInterface.Paint;
begin
  fGame.MapEditor.PaintUI;

  inherited;
end;


procedure TKMapEdInterface.Town_DefenceAddClick(Sender: TObject);
begin
  //Press the button
  Button_DefencePosAdd.Down := not Button_DefencePosAdd.Down and (Sender = Button_DefencePosAdd);

  if Button_DefencePosAdd.Down then
  begin
    GameCursor.Mode := cmMarkers;
    GameCursor.Tag1 := MARKER_DEFENCE;
  end
  else
  begin
    GameCursor.Mode := cmNone;
    GameCursor.Tag1 := 0;
  end;
end;


procedure TKMapEdInterface.Town_DefenceChange(Sender: TObject);
begin
  fPlayers[MySpectator.PlayerIndex].AI.Setup.AutoDefend := CheckBox_AutoDefence.Checked;
  fPlayers[MySpectator.PlayerIndex].AI.Setup.EquipRateLeather := TrackBar_EquipRateLeather.Position * 10;
  fPlayers[MySpectator.PlayerIndex].AI.Setup.EquipRateIron := TrackBar_EquipRateIron.Position * 10;
  fPlayers[MySpectator.PlayerIndex].AI.Setup.RecruitCount := TrackBar_RecruitCount.Position;

  if CheckBox_MaxSoldiers.Checked then
    fPlayers[MySpectator.PlayerIndex].AI.Setup.MaxSoldiers := -1
  else
    fPlayers[MySpectator.PlayerIndex].AI.Setup.MaxSoldiers := TrackBar_MaxSoldiers.Position;

  Town_DefenceRefresh;
end;


procedure TKMapEdInterface.Town_DefenceRefresh;
begin
  CheckBox_AutoDefence.Checked := fPlayers[MySpectator.PlayerIndex].AI.Setup.AutoDefend;
  TrackBar_EquipRateLeather.Position := fPlayers[MySpectator.PlayerIndex].AI.Setup.EquipRateLeather div 10;
  TrackBar_EquipRateIron.Position := fPlayers[MySpectator.PlayerIndex].AI.Setup.EquipRateIron div 10;
  TrackBar_RecruitCount.Position := fPlayers[MySpectator.PlayerIndex].AI.Setup.RecruitCount;

  CheckBox_MaxSoldiers.Checked := (fPlayers[MySpectator.PlayerIndex].AI.Setup.MaxSoldiers < 0);
  TrackBar_MaxSoldiers.Enabled := not CheckBox_MaxSoldiers.Checked;
  TrackBar_MaxSoldiers.Position := Max(fPlayers[MySpectator.PlayerIndex].AI.Setup.MaxSoldiers, 0);
end;


procedure TKMapEdInterface.Town_ScriptRefresh;
begin
  CheckBox_AutoBuild.Checked := fPlayers[MySpectator.PlayerIndex].AI.Setup.AutoBuild;
  CheckBox_AutoRepair.Checked := fPlayers[MySpectator.PlayerIndex].AI.Mayor.AutoRepair;
  TrackBar_SerfsPer10Houses.Position := Round(10*fPlayers[MySpectator.PlayerIndex].AI.Setup.SerfsPerHouse);
  TrackBar_WorkerCount.Position := fPlayers[MySpectator.PlayerIndex].AI.Setup.WorkerCount;
end;


procedure TKMapEdInterface.Town_ScriptChange(Sender: TObject);
begin
  fPlayers[MySpectator.PlayerIndex].AI.Setup.AutoBuild := CheckBox_AutoBuild.Checked;
  fPlayers[MySpectator.PlayerIndex].AI.Mayor.AutoRepair := CheckBox_AutoRepair.Checked;
  fPlayers[MySpectator.PlayerIndex].AI.Setup.SerfsPerHouse := TrackBar_SerfsPer10Houses.Position / 10;
  fPlayers[MySpectator.PlayerIndex].AI.Setup.WorkerCount := TrackBar_WorkerCount.Position;
end;


procedure TKMapEdInterface.Player_UpdateColors;
var
  I: Integer;
  PrevIndex: Integer;
begin
  //Set player colors
  for I := 0 to MAX_PLAYERS - 1 do
    Button_PlayerSelect[I].ShapeColor := fPlayers[I].FlagColor;

  //Update pages that have colored elements to match new players color
  Button_Town[ttUnits].FlagColor := fPlayers[MySpectator.PlayerIndex].FlagColor;
  for I := Low(Button_Citizen) to High(Button_Citizen) do
    Button_Citizen[I].FlagColor := fPlayers[MySpectator.PlayerIndex].FlagColor;
  for I := Low(Button_Warriors) to High(Button_Warriors) do
    Button_Warriors[I].FlagColor := fPlayers[MySpectator.PlayerIndex].FlagColor;
  Button_Player[ptColor].FlagColor := fPlayers[MySpectator.PlayerIndex].FlagColor;
  Button_Reveal.FlagColor := fPlayers[MySpectator.PlayerIndex].FlagColor;

  PrevIndex := Dropbox_PlayerFOW.ItemIndex;
  Dropbox_PlayerFOW.Clear;
  Dropbox_PlayerFOW.Add('Show all', -1);
  for I := 0 to MAX_PLAYERS - 1 do
    Dropbox_PlayerFOW.Add('[$' + IntToHex(FlagColorToTextColor(fPlayers[I].FlagColor) and $00FFFFFF, 6) + ']' + fPlayers[I].GetFormattedPlayerName, I);
  if PrevIndex = -1 then
    PrevIndex := 0; //Select Show All
  Dropbox_PlayerFOW.ItemIndex := PrevIndex;
end;


procedure TKMapEdInterface.Player_ChangeActive(Sender: TObject);
begin
  //If we had selected House or Unit - discard them
  if Panel_House.Visible or Panel_Unit.Visible or Panel_Marker.Visible then
    fActivePage := nil;

  if MySpectator.Selected <> nil then
    MySpectator.Selected := nil;

  SetActivePlayer(TKMControl(Sender).Tag);

  //Refresh per-player settings
  DisplayPage(fActivePage);
end;


procedure TKMapEdInterface.SetActivePlayer(aIndex: TPlayerIndex);
var I: Integer;
begin
  MySpectator.PlayerIndex := aIndex;

  for I := 0 to MAX_PLAYERS - 1 do
    Button_PlayerSelect[I].Down := (I = MySpectator.PlayerIndex);

  Player_UpdateColors;
  UpdateAITabsEnabled;
end;


procedure TKMapEdInterface.Terrain_BrushChange(Sender: TObject);
begin
  GameCursor.Mode := cmBrush;
  GameCursor.MapEdSize := BrushSize.Position;
  fTerrainPainter.RandomizeTiling := BrushRandom.Checked;

  if Sender = BrushCircle then
    GameCursor.MapEdShape := hsCircle
  else
  if Sender = BrushSquare then
    GameCursor.MapEdShape := hsSquare
  else
  if Sender is TKMButtonFlat then
    GameCursor.Tag1 := TKMButtonFlat(Sender).Tag;

  Terrain_BrushRefresh;
end;


procedure TKMapEdInterface.Terrain_BrushRefresh;
var
  I,K: Integer;
begin
  BrushCircle.Down := (GameCursor.MapEdShape = hsCircle);
  BrushSquare.Down := (GameCursor.MapEdShape = hsSquare);

  for I := Low(BrushTable) to High(BrushTable) do
  for K := Low(BrushTable[I]) to High(BrushTable[I]) do
  if BrushTable[I,K] <> nil then
    BrushTable[I,K].Down := (BrushTable[I,K].Tag = GameCursor.Tag1);
end;


procedure TKMapEdInterface.Terrain_HeightChange(Sender: TObject);
begin
  GameCursor.MapEdSize := HeightSize.Position;
  GameCursor.MapEdSlope := HeightSlope.Position;
  GameCursor.MapEdSpeed := HeightSpeed.Position;

  if Sender = HeightCircle then
    GameCursor.MapEdShape := hsCircle
  else
  if Sender = HeightSquare then
    GameCursor.MapEdShape := hsSquare
  else
  if Sender = HeightElevate then
    GameCursor.Mode := cmElevate
  else
  if Sender = HeightUnequalize then
    GameCursor.Mode := cmEqualize;

  Terrain_HeightRefresh;
end;


procedure TKMapEdInterface.Terrain_HeightRefresh;
begin
  HeightCircle.Down := (GameCursor.MapEdShape = hsCircle);
  HeightSquare.Down := (GameCursor.MapEdShape = hsSquare);

  HeightElevate.Down := (GameCursor.Mode = cmElevate);
  HeightUnequalize.Down := (GameCursor.Mode = cmEqualize);
end;


procedure TKMapEdInterface.Terrain_TilesChange(Sender: TObject);
begin
  if Sender = TilesRandom then
    GameCursor.MapEdDir := 4 * Byte(TilesRandom.Checked); //Defined=0..3 or Random=4

  if Sender is TKMButtonFlat then
    Terrain_TilesSet(TKMButtonFlat(Sender).TexID);

  Terrain_TilesRefresh(nil);
end;


procedure TKMapEdInterface.Terrain_TilesSet(aIndex: Integer);
begin
  if aIndex <> 0 then
  begin
    GameCursor.Mode := cmTiles;
    GameCursor.Tag1 := aIndex - 1; //MapEdTileRemap is 1 based, tag is 0 based
    if TilesRandom.Checked then
      GameCursor.MapEdDir := 4;

    fLastTile := aIndex;
  end;

  Terrain_TilesRefresh(nil);
end;


procedure TKMapEdInterface.Terrain_TilesRefresh(Sender: TObject);
  function GetTileIDFromTag(aTag: Byte): Byte;
  var X,Y,Tile: Byte;
  begin
    X := aTag mod MAPED_TILES_X + TilesScroll.Position;
    Y := (aTag div MAPED_TILES_X);
    Tile := (256 div MAPED_TILES_Y) * Y + X;
    Result := MapEdTileRemap[Tile + 1];
  end;
var
  I,K,L: Integer;
  TileID: Integer;
begin
  TilesRandom.Checked := (GameCursor.MapEdDir = 4);

  for I := 0 to MAPED_TILES_Y - 1 do
  for K := 0 to MAPED_TILES_X - 1 do
  begin
    L := I * MAPED_TILES_X + K;
    TileID := GetTileIDFromTag(L);
    TilesTable[L].TexID := TileID;
    TilesTable[L].Enabled := TileID <> 0;
    //If cursor has a tile then make sure its properly selected in table as well
    TilesTable[L].Down := (GameCursor.Mode = cmTiles) and (GameCursor.Tag1 = TileID - 1);
  end;
end;


procedure TKMapEdInterface.Terrain_ObjectsChange(Sender: TObject);
var
  ObjID: Integer;
begin
  ObjID := ObjectsScroll.Position * 3 + TKMButtonFlat(Sender).Tag; //0..n-1

  //Skip indexes out of range
  if not InRange(ObjID, 0, fCountCompact - 1)
  and not (TKMButtonFlat(Sender).Tag = 255)
  and not (TKMButtonFlat(Sender).Tag = 61) then
    Exit;

  GameCursor.Mode := cmObjects;
  if TKMButtonFlat(Sender).Tag = 255 then
    //Erase
    GameCursor.Tag1 := 255
  else
  if TKMButtonFlat(Sender).Tag = 61 then
    //Block
    GameCursor.Tag1 := 61
  else
    //Object
    GameCursor.Tag1 := fCompactToMapElem[ObjID]; //0..n-1

  fLastObject := TKMButtonFlat(Sender).Tag;

  Terrain_ObjectsRefresh(nil);
end;


procedure TKMapEdInterface.Terrain_ObjectsRefresh(Sender: TObject);
var
  I: Integer;
  ObjID: Integer;
begin
  for I := 0 to 8 do
  begin
    ObjID := ObjectsScroll.Position * 3 + I;
    if ObjID < fCountCompact then
    begin
      ObjectsTable[I].TexID := MapElem[fCompactToMapElem[ObjID]].Anim.Step[1] + 1;
      ObjectsTable[I].Caption := IntToStr(ObjID);
      ObjectsTable[I].Enable;
    end
    else
    begin
      ObjectsTable[I].TexID := 0;
      ObjectsTable[I].Caption := '';
      ObjectsTable[I].Disable;
    end;
    //Mark the selected one using reverse lookup
    ObjectsTable[I].Down := (GameCursor.Mode = cmObjects) and (ObjID = fMapElemToCompact[GameCursor.Tag1]);
  end;

  ObjectErase.Down := (GameCursor.Mode = cmObjects) and (GameCursor.Tag1 = 255); //or delete button
  ObjectBlock.Down := (GameCursor.Mode = cmObjects) and (GameCursor.Tag1 = 61); //or block button
end;


procedure TKMapEdInterface.Terrain_ClipboardChange(Sender: TObject);
begin
  if Sender = Button_SelectCopy then
  begin

  end;
end;


procedure TKMapEdInterface.Attack_Change(Sender: TObject);
var
  GT: TGroupType;
begin
  //Settings get saved on close, now we just toggle fields
  //because certain combinations can't coexist

  for GT := Low(TGroupType) to High(TGroupType) do
    NumEdit_AttackAmount[GT].Enabled := not CheckBox_AttackTakeAll.Checked;

  NumEdit_AttackLocX.Enabled := (TAIAttackTarget(Radio_AttackTarget.ItemIndex) = att_CustomPosition);
  NumEdit_AttackLocY.Enabled := (TAIAttackTarget(Radio_AttackTarget.ItemIndex) = att_CustomPosition);
end;


procedure TKMapEdInterface.Attack_Close(Sender: TObject);
var
  I: Integer;
  AA: TAIAttack;
  GT: TGroupType;
begin
  if Sender = Button_AttackOk then
  begin
    //Attack we are editing
    I := ColumnBox_Attacks.ItemIndex;

    //Copy attack info from controls to Attacks
    AA.AttackType := TAIAttackType(Radio_AttackType.ItemIndex);
    AA.Delay := NumEdit_AttackDelay.Value * 10;
    AA.TotalMen := NumEdit_AttackMen.Value;
    for GT := Low(TGroupType) to High(TGroupType) do
      AA.GroupAmounts[GT] := NumEdit_AttackAmount[GT].Value;
    AA.TakeAll := CheckBox_AttackTakeAll.Checked;
    AA.Target := TAIAttackTarget(Radio_AttackTarget.ItemIndex);
    AA.Range := TrackBar_AttackRange.Position;
    AA.CustomPosition := KMPoint(NumEdit_AttackLocX.Value, NumEdit_AttackLocY.Value);

    fPlayers[MySpectator.PlayerIndex].AI.General.Attacks[I] := AA;
  end;

  Panel_Attack.Hide;
  Attacks_Refresh;
end;


procedure TKMapEdInterface.Attack_Refresh(aAttack: TAIAttack);
var
  GT: TGroupType;
begin
  //Set attack properties to UI
  Radio_AttackType.ItemIndex := Byte(aAttack.AttackType);
  NumEdit_AttackDelay.Value := aAttack.Delay div 10;
  NumEdit_AttackMen.Value := aAttack.TotalMen;
  for GT := Low(TGroupType) to High(TGroupType) do
    NumEdit_AttackAmount[GT].Value := aAttack.GroupAmounts[GT];
  CheckBox_AttackTakeAll.Checked := aAttack.TakeAll;
  Radio_AttackTarget.ItemIndex := Byte(aAttack.Target);
  TrackBar_AttackRange.Position := aAttack.Range;
  NumEdit_AttackLocX.Value := aAttack.CustomPosition.X;
  NumEdit_AttackLocY.Value := aAttack.CustomPosition.Y;

  //Certain values disable certain controls
  Attack_Change(nil);
end;


//Add a dummy attack and let mapmaker edit it
procedure TKMapEdInterface.Attacks_Add(Sender: TObject);
var
  AA: TAIAttack;
begin
  FillChar(AA, SizeOf(AA), #0);
  fPlayers[MySpectator.PlayerIndex].AI.General.Attacks.AddAttack(AA);

  Attacks_Refresh;
  ColumnBox_Attacks.ItemIndex := fPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Count - 1;

  //Edit the attack we have just appended
  Attacks_Edit(ColumnBox_Attacks.ItemIndex);
end;


procedure TKMapEdInterface.Attacks_Del(Sender: TObject);
var I: Integer;
begin
  I := ColumnBox_Attacks.ItemIndex;
  if InRange(I, 0, fPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Count - 1) then
    fPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Delete(I);

  Attacks_Refresh;
end;


procedure TKMapEdInterface.Attacks_Edit(aIndex: Integer);
begin
  Assert(InRange(aIndex, 0, fPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Count - 1));
  Attack_Refresh(fPlayers[MySpectator.PlayerIndex].AI.General.Attacks[aIndex]);
  Panel_Attack.Show;
end;


procedure TKMapEdInterface.Attacks_ListClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Attacks.ItemIndex;
  Button_AttacksDel.Enabled := InRange(I, 0, fPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Count - 1);
end;


procedure TKMapEdInterface.Attacks_ListDoubleClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Attacks.ItemIndex;

  //Check if user double-clicked on an existing item (not on an empty space)
  if InRange(I, 0, fPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Count - 1) then
    Attacks_Edit(I);
end;


procedure TKMapEdInterface.Attacks_Refresh;
const
  Typ: array [TAIAttackType] of string = ('O', 'R');
  Tgt: array [TAIAttackTarget] of string = ('U', 'H1', 'H2', 'Pos');
var
  I: Integer;
  A: TAIAttack;
begin
  ColumnBox_Attacks.Clear;

  for I := 0 to fPlayers[MySpectator.PlayerIndex].AI.General.Attacks.Count - 1 do
  begin
    A := fPlayers[MySpectator.PlayerIndex].AI.General.Attacks[I];
    ColumnBox_Attacks.AddItem(MakeListRow([Typ[A.AttackType], IntToStr(A.Delay div 10), IntToStr(A.TotalMen), Tgt[A.Target], TypeToString(A.CustomPosition)]));
  end;

  Attacks_ListClick(nil);
end;


procedure TKMapEdInterface.Goal_Change(Sender: TObject);
begin
  //Settings get saved on close, now we just toggle fields
  //because certain combinations can't coexist

  NumEdit_GoalTime.Enabled := TGoalCondition(Radio_GoalCondition.ItemIndex) = gc_Time;
  NumEdit_GoalPlayer.Enabled := TGoalCondition(Radio_GoalCondition.ItemIndex) <> gc_Time;
end;


procedure TKMapEdInterface.Goal_Close(Sender: TObject);
var
  I: Integer;
  G: TKMGoal;
begin
  if Sender = Button_GoalOk then
  begin
    //Goal we are editing
    I := ColumnBox_Goals.ItemIndex;

    //Copy Goal info from controls to Goals
    G.GoalType := TGoalType(Radio_GoalType.ItemIndex);
    G.GoalCondition := TGoalCondition(Radio_GoalCondition.ItemIndex);
    if G.GoalType = glt_Survive then
      G.GoalStatus := gs_True
    else
      G.GoalStatus := gs_False;
    G.GoalTime := NumEdit_GoalTime.Value * 10;
    G.MessageToShow := NumEdit_GoalMessage.Value;
    G.PlayerIndex := NumEdit_GoalPlayer.Value - 1;

    fPlayers[MySpectator.PlayerIndex].Goals[I] := G;
  end;

  Panel_Goal.Hide;
  Goals_Refresh;
end;


procedure TKMapEdInterface.Goal_Refresh(aGoal: TKMGoal);
begin
  Radio_GoalType.ItemIndex := Byte(aGoal.GoalType);
  Radio_GoalCondition.ItemIndex := Byte(aGoal.GoalCondition);
  NumEdit_GoalTime.Value := aGoal.GoalTime div 10;
  NumEdit_GoalMessage.Value := aGoal.MessageToShow;
  NumEdit_GoalPlayer.Value := aGoal.PlayerIndex + 1;

  //Certain values disable certain controls
  Goal_Change(nil);
end;


//Add a dummy goal and let mapmaker edit it
procedure TKMapEdInterface.Goals_Add(Sender: TObject);
var
  G: TKMGoal;
begin
  FillChar(G, SizeOf(G), #0);
  fPlayers[MySpectator.PlayerIndex].Goals.AddGoal(G);

  Goals_Refresh;
  ColumnBox_Goals.ItemIndex := fPlayers[MySpectator.PlayerIndex].Goals.Count - 1;

  //Edit the attack we have just appended
  Goals_Edit(ColumnBox_Goals.ItemIndex);
end;


procedure TKMapEdInterface.Goals_Del(Sender: TObject);
var I: Integer;
begin
  I := ColumnBox_Goals.ItemIndex;
  if InRange(I, 0, fPlayers[MySpectator.PlayerIndex].Goals.Count - 1) then
    fPlayers[MySpectator.PlayerIndex].Goals.Delete(I);
  Goals_Refresh;
end;


procedure TKMapEdInterface.Goals_Edit(aIndex: Integer);
begin
  Assert(InRange(aIndex, 0, fPlayers[MySpectator.PlayerIndex].Goals.Count - 1));
  Goal_Refresh(fPlayers[MySpectator.PlayerIndex].Goals[aIndex]);
  Panel_Goal.Show;
end;


procedure TKMapEdInterface.Goals_ListClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Goals.ItemIndex;
  Button_GoalsDel.Enabled := InRange(I, 0, fPlayers[MySpectator.PlayerIndex].Goals.Count - 1);
end;


procedure TKMapEdInterface.Goals_ListDoubleClick(Sender: TObject);
var
  I: Integer;
begin
  I := ColumnBox_Goals.ItemIndex;

  //Check if user double-clicked on an existing item (not on an empty space)
  if InRange(I, 0, fPlayers[MySpectator.PlayerIndex].Goals.Count - 1) then
    Goals_Edit(I);
end;


procedure TKMapEdInterface.Goals_Refresh;
const
  Typ: array [TGoalType] of string = ('-', 'V', 'S');
  Cnd: array [TGoalCondition] of string = (
    'None', 'BuildTutorial', 'Time', 'Buildings', 'Troops', 'Unknown',
    'MilitaryAssets', 'SerfsAndSchools', 'EconomyBuildings');
var
  I: Integer;
  G: TKMGoal;
begin
  ColumnBox_Goals.Clear;

  for I := 0 to fPlayers[MySpectator.PlayerIndex].Goals.Count - 1 do
  begin
    G := fPlayers[MySpectator.PlayerIndex].Goals[I];
    ColumnBox_Goals.AddItem(MakeListRow([Typ[G.GoalType],
                                    Cnd[G.GoalCondition],
                                    IntToStr(G.PlayerIndex + 1),
                                    IntToStr(G.GoalTime div 10),
                                    IntToStr(G.MessageToShow)]));
  end;

  Goals_ListClick(nil);
end;


procedure TKMapEdInterface.Town_BuildChange(Sender: TObject);
var I: Integer;
begin
  //Reset cursor and see if it needs to be changed
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  if Sender = Button_BuildCancel then
    GameCursor.Mode := cmErase
  else
  if Sender = Button_BuildRoad then
    GameCursor.Mode := cmRoad
  else
  if Sender = Button_BuildField then
    GameCursor.Mode := cmField
  else
  if Sender = Button_BuildWine then
    GameCursor.Mode := cmWine
  else
  //if Button_BuildWall.Down then
  //  GameCursor.Mode:=cm_Wall;
  //else
  for I := 1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[I] <> ht_None then
  if Sender = Button_Build[I] then
  begin
    GameCursor.Mode := cmHouses;
    GameCursor.Tag1 := Byte(GUIHouseOrder[I]);
  end;

  Town_BuildRefresh;
end;


procedure TKMapEdInterface.Town_BuildRefresh;
var
  I: Integer;
begin
  Button_BuildCancel.Down := (GameCursor.Mode = cmErase);
  Button_BuildRoad.Down   := (GameCursor.Mode = cmRoad);
  Button_BuildField.Down  := (GameCursor.Mode = cmField);
  Button_BuildWine.Down   := (GameCursor.Mode = cmWine);
  //Button_BuildWall.Down := (GameCursor.Mode = cm_Wall);

  for I := 1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[I] <> ht_None then
    Button_Build[I].Down := (GameCursor.Mode = cmHouses) and (GameCursor.Tag1 = Byte(GUIHouseOrder[I]));
end;


procedure TKMapEdInterface.Town_UnitChange(Sender: TObject);
begin
  GameCursor.Mode := cmUnits;
  GameCursor.Tag1 := Byte(TKMButtonFlat(Sender).Tag);

  Town_UnitRefresh;
end;


procedure TKMapEdInterface.Town_UnitRefresh;
var
  I: Integer;
  B: TKMButtonFlat;
begin
  for I := 1 to Panel_Units.ChildCount do
  if Panel_Units.Childs[I] is TKMButtonFlat then
  begin
    B := TKMButtonFlat(Panel_Units.Childs[I]);
    B.Down := (GameCursor.Mode = cmUnits) and (GameCursor.Tag1 = B.Tag);
  end;
end;


procedure TKMapEdInterface.Extra_Change(Sender: TObject);
begin
  SHOW_TERRAIN_WIRES := TrackBar_Passability.Position <> 0;
  SHOW_TERRAIN_PASS := TrackBar_Passability.Position;

  if TrackBar_Passability.Position <> 0 then
    Label_Passability.Caption := GetEnumName(TypeInfo(TPassability), SHOW_TERRAIN_PASS)
  else
    Label_Passability.Caption := 'Off';

  Layers_UpdateVisibility;
end;


//Set which layers are visible and which are not
//Layer is always visible if corresponding editing page is active (to see what gets placed)
procedure TKMapEdInterface.Layers_UpdateVisibility;
begin
  if fGame = nil then Exit; //Happens on init

  fGame.MapEditor.VisibleLayers := [];

  if Panel_Markers.Visible or Panel_MarkerReveal.Visible then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlRevealFOW, mlCenterScreen];

  if Panel_Defence.Visible or Panel_MarkerDefence.Visible then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlDefences];

  if CheckBox_ShowObjects.Checked or Panel_Objects.Visible then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlObjects];

  if CheckBox_ShowHouses.Checked or Panel_Build.Visible or Panel_House.Visible then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlHouses];

  if CheckBox_ShowUnits.Checked or Panel_Units.Visible or Panel_Unit.Visible then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlUnits];

  if CheckBox_ShowDeposits.Checked then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlDeposits];
end;


procedure TKMapEdInterface.ShowHouseInfo(Sender: TKMHouse);
var
  HouseDat: TKMHouseDatClass;
  I: Integer;
  Res: TWareType;
begin
  if Sender = nil then
  begin
    DisplayPage(nil);
    exit;
  end;

  SetActivePlayer(Sender.Owner);

  HouseDat := fResource.HouseDat[Sender.HouseType];

  {Common data}
  Label_House.Caption := HouseDat.HouseName;
  Image_House_Logo.TexID := HouseDat.GUIIcon;
  Image_House_Worker.TexID := fResource.UnitDat[HouseDat.OwnerType].GUIIcon;
  Image_House_Worker.FlagColor := fPlayers[Sender.Owner].FlagColor;
  Image_House_Worker.Hint := fResource.UnitDat[HouseDat.OwnerType].UnitName;
  Image_House_Worker.Visible := HouseDat.OwnerType <> ut_None;
  KMHealthBar_House.Caption := IntToStr(Round(Sender.GetHealth)) + '/' + IntToStr(HouseDat.MaxHealth);
  KMHealthBar_House.Position := Sender.GetHealth / HouseDat.MaxHealth;

  Label_House_Supply.Hide;
  for I := 0 to 3 do
  begin
    Res := HouseDat.ResInput[I+1];
    if fResource.Wares[Res].IsValid then
    begin
      ResRow_Resource[I].TexID := fResource.Wares[Res].GUIIcon;
      ResRow_Resource[I].Caption := fResource.Wares[Res].Title;
      ResRow_Resource[I].Hint := fResource.Wares[Res].Title;
      ResRow_Resource[I].ResourceCount := Sender.CheckResIn(Res);
      ResRow_Resource[I].OrderCount := Sender.CheckResIn(Res);
      ResRow_Resource[I].Show;
      Label_House_Supply.Show;
    end
    else
      ResRow_Resource[I].Hide;
  end;

  case Sender.HouseType of
    ht_Store:     begin
                    DisplayPage(Panel_HouseStore); //Must be displayed first
                    House_StoreRefresh(nil);
                    //Reselect the ware so the display is updated
                    House_StoreSelectWare(Button_Store[fStorehouseItem]);
                  end;
    ht_Barracks:  begin
                    DisplayPage(Panel_HouseBarracks); //Must be displayed first
                    House_BarracksRefresh(nil);
                    //In the barrack the recruit icon is always enabled
                    Image_House_Worker.Enable;
                    //Reselect the ware so the display is updated
                    House_BarracksSelectWare(Button_Barracks[fBarracksItem]);
                  end;
    ht_TownHall:;
    else          DisplayPage(Panel_House);
  end;
end;


procedure TKMapEdInterface.ShowUnitInfo(Sender: TKMUnit);
begin
  if Sender = nil then
  begin
    DisplayPage(nil);
    Exit;
  end;

  SetActivePlayer(Sender.Owner);

  DisplayPage(Panel_Unit);
  Label_UnitName.Caption := fResource.UnitDat[Sender.UnitType].UnitName;
  Image_UnitPic.TexID := fResource.UnitDat[Sender.UnitType].GUIScroll;
  Image_UnitPic.FlagColor := fPlayers[Sender.Owner].FlagColor;
  KMConditionBar_Unit.Position := Sender.Condition / UNIT_MAX_CONDITION;

  Label_UnitDescription.Caption := fResource.UnitDat[Sender.UnitType].Description;
  Label_UnitDescription.Show;
end;


procedure TKMapEdInterface.ShowGroupInfo(Sender: TKMUnitGroup);
begin
  if (Sender = nil) or Sender.IsDead then
  begin
    DisplayPage(nil);
    Exit;
  end;

  SetActivePlayer(Sender.Owner);

  DisplayPage(Panel_Unit);
  Label_UnitName.Caption := fResource.UnitDat[Sender.UnitType].UnitName;
  Image_UnitPic.TexID := fResource.UnitDat[Sender.UnitType].GUIScroll;
  Image_UnitPic.FlagColor := fPlayers[Sender.Owner].FlagColor;
  KMConditionBar_Unit.Position := Sender.Condition / UNIT_MAX_CONDITION;

  //Warrior specific
  Label_UnitDescription.Hide;
  ImageStack_Army.SetCount(Sender.MapEdCount, Sender.UnitsPerRow, Sender.UnitsPerRow div 2 + 1);
  Label_ArmyCount.Caption := IntToStr(Sender.MapEdCount);
  DropBox_ArmyOrder.ItemIndex := Byte(Sender.MapEdOrder.Order);
  Edit_ArmyOrderX.Value := Sender.MapEdOrder.Pos.Loc.X;
  Edit_ArmyOrderY.Value := Sender.MapEdOrder.Pos.Loc.Y;
  Edit_ArmyOrderDir.Value := Max(Byte(Sender.MapEdOrder.Pos.Dir) - 1, 0);
  Unit_ArmyChange1(nil);
  Panel_Army.Show;
end;


procedure TKMapEdInterface.ShowMarkerInfo(aMarker: TKMMapEdMarker);
begin
  fActiveMarker := aMarker;

  if (aMarker.MarkerType = mtNone) or (aMarker.Owner = PLAYER_NONE) or (aMarker.Index = -1) then
  begin
    DisplayPage(nil);
    Exit;
  end;

  SetActivePlayer(aMarker.Owner);
  Image_MarkerPic.FlagColor := fPlayers[aMarker.Owner].FlagColor;

  case aMarker.MarkerType of
    mtDefence:    begin
                    Label_MarkerType.Caption := 'Defence position';
                    Image_MarkerPic.TexID := 338;
                    DropList_DefenceGroup.ItemIndex := Byte(fPlayers[aMarker.Owner].AI.General.DefencePositions[aMarker.Index].GroupType);
                    DropList_DefenceType.ItemIndex := Byte(fPlayers[aMarker.Owner].AI.General.DefencePositions[aMarker.Index].DefenceType);
                    TrackBar_DefenceRad.Position := fPlayers[aMarker.Owner].AI.General.DefencePositions[aMarker.Index].Radius;
                    DisplayPage(Panel_MarkerDefence);
                  end;
    mtRevealFOW:  begin
                    Label_MarkerType.Caption := 'Reveal fog';
                    Image_MarkerPic.TexID := 393;
                    TrackBar_RevealSize.Position := fGame.MapEditor.Revealers[aMarker.Owner].Tag[aMarker.Index];
                    DisplayPage(Panel_MarkerReveal);
                  end;
  end;
end;


procedure TKMapEdInterface.ShowMessage(aText: string);
begin
  Label_Message.Caption := aText;
  Panel_Message.Show;
  Image_Message.Show; //Hidden by default, only visible when a message is shown
end;


procedure TKMapEdInterface.Menu_SaveClick(Sender: TObject);
var
  SaveName: string;
begin
  SaveName := TKMapsCollection.FullPath(Trim(Edit_SaveName.Text), '.dat', Radio_Save_MapType.ItemIndex = 1);

  if (Sender = Edit_SaveName) or (Sender = Radio_Save_MapType) then
  begin
    CheckBox_SaveExists.Enabled := FileExists(SaveName);
    Label_SaveExists.Visible := CheckBox_SaveExists.Enabled;
    CheckBox_SaveExists.Checked := False;
    Button_SaveSave.Enabled := not CheckBox_SaveExists.Enabled;
  end;

  if Sender = CheckBox_SaveExists then
    Button_SaveSave.Enabled := CheckBox_SaveExists.Checked;

  if Sender = Button_SaveSave then
  begin
    fGame.SaveMapEditor(SaveName);

    Player_UpdateColors;
    Label_MissionName.Caption := fGame.GameName;

    SwitchPage(Button_SaveCancel); //return to previous menu
  end;
end;


procedure TKMapEdInterface.Marker_Change(Sender: TObject);
var
  DP: TAIDefencePosition;
  Rev: TKMPointTagList;
begin
  case fActiveMarker.MarkerType of
    mtDefence:    begin
                    DP := fPlayers[fActiveMarker.Owner].AI.General.DefencePositions[fActiveMarker.Index];
                    DP.Radius := TrackBar_DefenceRad.Position;
                    DP.DefenceType := TAIDefencePosType(DropList_DefenceType.ItemIndex);
                    DP.GroupType := TGroupType(DropList_DefenceGroup.ItemIndex);

                    if Sender = Button_DefenceCW then
                      DP.Position := KMPointDir(DP.Position.Loc, KMNextDirection(DP.Position.Dir));
                    if Sender = Button_DefenceCCW then
                      DP.Position := KMPointDir(DP.Position.Loc, KMPrevDirection(DP.Position.Dir));

                    if Sender = Button_DefenceDelete then
                    begin
                      fPlayers[fActiveMarker.Owner].AI.General.DefencePositions.Delete(fActiveMarker.Index);
                      SwitchPage(Button_Town[ttDefences]);
                    end;

                    if Sender = Button_DefenceClose then
                      SwitchPage(Button_Town[ttDefences]);
                  end;
    mtRevealFOW:  begin
                    //Shortcut to structure we update
                    Rev := fGame.MapEditor.Revealers[fActiveMarker.Owner];

                    if Sender = TrackBar_RevealSize then
                      Rev.Tag[fActiveMarker.Index] := TrackBar_RevealSize.Position;

                    if Sender = Button_RevealDelete then
                    begin
                      Rev.DeleteEntry(fActiveMarker.Index);
                      SwitchPage(Button_Player[ptMarkers]);
                    end;

                    if Sender = Button_RevealClose then
                      SwitchPage(Button_Player[ptMarkers]);
                  end;
  end;
end;


//Mission loading dialog
procedure TKMapEdInterface.Menu_LoadClick(Sender: TObject);
var
  MapName: string;
  IsMulti: Boolean;
begin
  if ListBox_Load.ItemIndex = -1 then Exit;

  MapName := ListBox_Load.Item[ListBox_Load.ItemIndex];
  IsMulti := Radio_Load_MapType.ItemIndex = 1;
  fGameApp.NewMapEditor(TKMapsCollection.FullPath(MapName, '.dat', IsMulti), 0, 0);

  //Keep MP/SP selected in the new map editor interface
  //this one is destroyed already by `fGameApp.NewMapEditor`
  if (fGame <> nil) and (fGame.MapEditorInterface <> nil) then
    fGame.MapEditorInterface.SetLoadMode(IsMulti);
end;


{Quit the mission and return to main menu}
procedure TKMapEdInterface.Menu_QuitClick(Sender: TObject);
begin
  fGameApp.Stop(gr_MapEdEnd);
end;


procedure TKMapEdInterface.Menu_LoadChange(Sender: TObject);
begin
  Menu_LoadUpdate;
end;


procedure TKMapEdInterface.Menu_LoadUpdate;
begin
  fMaps.TerminateScan;
  fMapsMP.TerminateScan;

  ListBox_Load.Clear;
  ListBox_Load.ItemIndex := -1;

  if Radio_Load_MapType.ItemIndex = 0 then
    fMaps.Refresh(Menu_LoadUpdateDone)
  else
    fMapsMP.Refresh(Menu_LoadUpdateDone);
end;


procedure TKMapEdInterface.Menu_LoadUpdateDone(Sender: TObject);
var
  I: Integer;
  PrevMap: string;
  PrevTop: Integer;
  M: TKMapsCollection;
begin
  if Radio_Load_MapType.ItemIndex = 0 then
    M := fMaps
  else
    M := fMapsMP;

  //Remember previous map
  if ListBox_Load.ItemIndex <> -1 then
    PrevMap := M.Maps[ListBox_Load.ItemIndex].FileName
  else
    PrevMap := '';
  PrevTop := ListBox_Load.TopIndex;

  ListBox_Load.Clear;

  M.Lock;
  try
    for I := 0 to M.Count - 1 do
    begin
      ListBox_Load.Add(M.Maps[I].FileName);
      if M.Maps[I].FileName = PrevMap then
        ListBox_Load.ItemIndex := I;
    end;
  finally
    M.Unlock;
  end;

  ListBox_Load.TopIndex := PrevTop;
end;


//This function will be called if the user right clicks on the screen.
procedure TKMapEdInterface.RightClick_Cancel;
begin
  //We should drop the tool but don't close opened tab. This allows eg:
  //Place a warrior, right click so you are not placing more warriors,
  //select the placed warrior.

  //Terrain height uses both buttons for relief changing, tile rotation etc.
  if Panel_Heights.Visible then Exit;
  //Terrain tiles uses right click for choosing tile rotation
  if Panel_Tiles.Visible then Exit;

  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  //Display page will hide the army panel
  if Panel_Army.Visible then Exit;

  DisplayPage(fActivePage);
end;


procedure TKMapEdInterface.SetTileDirection(aTileDirection: Byte);
begin
  fTileDirection := aTileDirection mod 4; //0..3
  GameCursor.MapEdDir := fTileDirection;
end;


procedure TKMapEdInterface.UpdateAITabsEnabled;
begin
  if fGame.MapEditor.PlayerAI[MySpectator.PlayerIndex] then
  begin
    Button_Town[ttScript].Enable;
    Button_Town[ttDefences].Enable;
    Button_Town[ttOffence].Enable;
  end
  else
  begin
    Button_Town[ttScript].Disable;
    Button_Town[ttDefences].Disable;
    Button_Town[ttOffence].Disable;
    if Panel_Script.Visible or Panel_Defence.Visible or Panel_Offence.Visible then
      Button_Town[ttHouses].Click; //Change back to first tab
  end;
end;


procedure TKMapEdInterface.SetLoadMode(aMultiplayer: Boolean);
begin
  if aMultiplayer then
  begin
    Radio_Load_MapType.ItemIndex := 1;
    Radio_Save_MapType.ItemIndex := 1;
  end
  else
  begin
    Radio_Load_MapType.ItemIndex := 0;
    Radio_Save_MapType.ItemIndex := 0;
  end;
end;


procedure TKMapEdInterface.House_StoreRefresh(Sender: TObject);
var
  I, Tmp: Integer;
begin
  if MySpectator.Selected = nil then exit;
  if not (MySpectator.Selected is TKMHouseStore) then exit;

  for I := 1 to STORE_RES_COUNT do
  begin
    Tmp := TKMHouseStore(MySpectator.Selected).CheckResIn(StoreResType[I]);
    Button_Store[I].Caption := IfThen(Tmp = 0, '-', IntToStr(Tmp));
  end;
end;


procedure TKMapEdInterface.House_BarracksRefresh(Sender: TObject);
var
  I, Tmp: Integer;
begin
  if MySpectator.Selected = nil then exit;
  if not (MySpectator.Selected is TKMHouseBarracks) then exit;

  for I := 1 to BARRACKS_RES_COUNT do
  begin
    Tmp := TKMHouseBarracks(MySpectator.Selected).CheckResIn(BarracksResType[I]);
    Button_Barracks[I].Caption := IfThen(Tmp = 0, '-', IntToStr(Tmp));
  end;
end;


procedure TKMapEdInterface.House_HealthChange(Sender: TObject; AButton: TMouseButton);
var
  H: TKMHouse;
  I: Integer;
  Res: TWareType;
  NewCount: Integer;
begin
  if not (MySpectator.Selected is TKMHouse) then Exit;
  H := TKMHouse(MySpectator.Selected);

  if Sender = Button_HouseHealthDec then H.AddDamage(-1, ORDER_CLICK_AMOUNT[AButton] * 5, True);
  if Sender = Button_HouseHealthInc then H.AddRepair(ORDER_CLICK_AMOUNT[AButton] * 5);

  for I := 0 to 3 do
  begin
    Res := fResource.HouseDat[H.HouseType].ResInput[I+1];

    if Sender = ResRow_Resource[I].OrderAdd then
    begin
      NewCount := Math.Min(ORDER_CLICK_AMOUNT[aButton], MAX_WARES_IN_HOUSE - H.CheckResIn(Res));
      H.ResAddToIn(Res, NewCount);
    end;

    if Sender = ResRow_Resource[I].OrderRem then
    begin
      NewCount := Math.Min(ORDER_CLICK_AMOUNT[aButton], H.CheckResIn(Res));
      H.ResTakeFromIn(Res, NewCount);
    end;
  end;

  if H.IsDestroyed then
    ShowHouseInfo(nil)
  else
    ShowHouseInfo(H);
end;


procedure TKMapEdInterface.Unit_ArmyChange1(Sender: TObject);
var
  Group: TKMUnitGroup;
begin
  if not (MySpectator.Selected is TKMUnitGroup) then Exit;

  Group := TKMUnitGroup(MySpectator.Selected);
  if Sender = Button_Army_ForUp then Group.UnitsPerRow := Group.UnitsPerRow - 1;
  if Sender = Button_Army_ForDown then Group.UnitsPerRow := Group.UnitsPerRow + 1;

  ImageStack_Army.SetCount(Group.MapEdCount, Group.UnitsPerRow, Group.UnitsPerRow div 2 + 1);
  Label_ArmyCount.Caption := IntToStr(Group.MapEdCount);

  if Sender = Button_Army_RotCW then  Group.Direction := KMNextDirection(Group.Direction);
  if Sender = Button_Army_RotCCW then Group.Direction := KMPrevDirection(Group.Direction);
  Group.ResetAnimStep;

  //Toggle between full and half condition
  if Sender = Button_ArmyFood then
  begin
    if Group.Condition = UNIT_MAX_CONDITION then
      Group.Condition := UNIT_MAX_CONDITION div 2
    else
      Group.Condition := UNIT_MAX_CONDITION;
    KMConditionBar_Unit.Position := Group.Condition / UNIT_MAX_CONDITION;
  end;

  Group.MapEdOrder.Order := TKMInitialOrder(DropBox_ArmyOrder.ItemIndex);
  Group.MapEdOrder.Pos.Loc.X := Edit_ArmyOrderX.Value;
  Group.MapEdOrder.Pos.Loc.Y := Edit_ArmyOrderY.Value;
  Group.MapEdOrder.Pos.Dir := TKMDirection(Edit_ArmyOrderDir.Value + 1);

  if DropBox_ArmyOrder.ItemIndex = 0 then
  begin
    Edit_ArmyOrderX.Disable;
    Edit_ArmyOrderY.Disable;
    Edit_ArmyOrderDir.Disable;
  end
  else
    if DropBox_ArmyOrder.ItemIndex = 2 then
    begin
      Edit_ArmyOrderX.Enable;
      Edit_ArmyOrderY.Enable;
      Edit_ArmyOrderDir.Disable; //Attack position doesn't let you set direction
    end
    else
    begin
      Edit_ArmyOrderX.Enable;
      Edit_ArmyOrderY.Enable;
      Edit_ArmyOrderDir.Enable;
    end;
end;


procedure TKMapEdInterface.Unit_ArmyChange2(Sender: TObject; AButton: TMouseButton);
var
  NewCount: Integer;
  Group: TKMUnitGroup;
begin
  if not (MySpectator.Selected is TKMUnitGroup) then Exit;

  Group := TKMUnitGroup(MySpectator.Selected);

  if Sender = Button_ArmyDec then //Decrease
    NewCount := Group.MapEdCount - ORDER_CLICK_AMOUNT[AButton]
  else //Increase
    NewCount := Group.MapEdCount + ORDER_CLICK_AMOUNT[AButton];

  Group.MapEdCount := EnsureRange(NewCount, 1, 200); //Limit max members
  ImageStack_Army.SetCount(Group.MapEdCount, Group.UnitsPerRow, Group.UnitsPerRow div 2 + 1);
  Label_ArmyCount.Caption := IntToStr(Group.MapEdCount);
end;


procedure TKMapEdInterface.ExtraMessage_Switch(Sender: TObject);
begin
  //Don't use DisplayPage because that hides other stuff
  if Sender = Image_Extra then
  begin
    if Panel_Extra.Visible then
    begin
      Panel_Extra.Hide;
      fSoundLib.Play(sfxn_MPChatClose);
    end
    else
    begin
      Panel_Extra.Show;
      Panel_Message.Hide;
      fSoundLib.Play(sfxn_MPChatOpen);
    end;
  end
  else
  if Sender = Image_ExtraClose then
  begin
    Panel_Extra.Hide;
    fSoundLib.Play(sfxn_MPChatClose);
  end;
  if Sender = Image_Message then
  begin
    if Panel_Message.Visible then
    begin
      Panel_Message.Hide;
      fSoundLib.Play(sfxn_MPChatClose);
    end
    else
    begin
      Panel_Message.Show;
      Panel_Extra.Hide;
      fSoundLib.Play(sfxn_MPChatOpen);
    end;
  end
  else
  if Sender = Image_MessageClose then
  begin
    Panel_Message.Hide;
    fSoundLib.Play(sfxn_MPChatClose);
  end;
end;


procedure TKMapEdInterface.House_BarracksSelectWare(Sender: TObject);
var I: Integer;
begin
  if not Panel_HouseBarracks.Visible then exit;
  if not (Sender is TKMButtonFlat) then exit; //Only FlatButtons
  if TKMButtonFlat(Sender).Tag = 0 then exit; //with set Tag

  for I:=1 to BARRACKS_RES_COUNT do
    Button_Barracks[I].Down := False;
  TKMButtonFlat(Sender).Down := True;
  fBarracksItem := TKMButtonFlat(Sender).Tag;
  House_BarracksEditCount(Sender, mbLeft);
end;


procedure TKMapEdInterface.House_StoreSelectWare(Sender: TObject);
var I: Integer;
begin
  if not Panel_HouseStore.Visible then exit;
  if not (Sender is TKMButtonFlat) then exit; //Only FlatButtons
  if TKMButtonFlat(Sender).Tag = 0 then exit; //with set Tag

  for I := 1 to Length(Button_Store) do
    Button_Store[I].Down := False;

  TKMButtonFlat(Sender).Down := True;
  fStorehouseItem := TKMButtonFlat(Sender).Tag;
  House_StoreEditCount(Sender, mbLeft);
end;


procedure TKMapEdInterface.House_BarracksEditCount(Sender: TObject; AButton:TMouseButton);
var
  Res: TWareType;
  Barracks: TKMHouseBarracks;
  NewCount: Word;
begin
  if not Panel_HouseBarracks.Visible or not (MySpectator.Selected is TKMHouseBarracks) then Exit;

  Res := BarracksResType[fBarracksItem];
  Barracks := TKMHouseBarracks(MySpectator.Selected);

  if (Sender = Button_BarracksDec100) or (Sender = Button_BarracksDec) then begin
    NewCount := Math.Min(Barracks.CheckResIn(Res), ORDER_CLICK_AMOUNT[aButton] * TKMButton(Sender).Tag);
    Barracks.ResTakeFromOut(Res, NewCount);
  end;

  if (Sender = Button_BarracksInc100) or (Sender = Button_BarracksInc) then begin
    NewCount := Math.Min(High(Word) - Barracks.CheckResIn(Res), ORDER_CLICK_AMOUNT[aButton] * TKMButton(Sender).Tag);
    Barracks.ResAddToIn(Res, NewCount);
  end;

  Label_Barracks_WareCount.Caption := IntToStr(Barracks.CheckResIn(Res));
  House_BarracksRefresh(nil);
end;


procedure TKMapEdInterface.House_StoreEditCount(Sender: TObject; AButton:TMouseButton);
var
  Res: TWareType;
  Store: TKMHouseStore;
  NewCount: Word;
begin
  if not Panel_HouseStore.Visible or not (MySpectator.Selected is TKMHouseStore) then Exit;

  Res := StoreResType[fStorehouseItem];
  Store := TKMHouseStore(MySpectator.Selected);

  //We need to take no more than it is there, thats part of bugtracking idea
  if (Sender = Button_StoreDec100) or (Sender = Button_StoreDec) then begin
    NewCount := Math.Min(Store.CheckResIn(Res), ORDER_CLICK_AMOUNT[aButton]*TKMButton(Sender).Tag);
    Store.ResTakeFromOut(Res, NewCount);
  end;

  //We can always add any amount of resource, it will be capped by Store
  if (Sender = Button_StoreInc100) or (Sender = Button_StoreInc) then
    Store.ResAddToIn(Res, ORDER_CLICK_AMOUNT[aButton]*TKMButton(Sender).Tag);

  Label_Store_WareCount.Caption := inttostr(Store.CheckResIn(Res));
  House_StoreRefresh(nil);
end;


procedure TKMapEdInterface.Player_ColorClick(Sender: TObject);
begin
  if not (Sender = ColorSwatch_Color) then exit;
  fPlayers[MySpectator.PlayerIndex].FlagColor := ColorSwatch_Color.GetColor;
  Player_UpdateColors;
end;


procedure TKMapEdInterface.Player_FOWChange(Sender: TObject);
begin
  MySpectator.FOWIndex := Dropbox_PlayerFOW.GetTag(Dropbox_PlayerFOW.ItemIndex);
  fGame.Minimap.Update(False); //Force update right now so FOW doesn't appear to lag
end;


procedure TKMapEdInterface.Player_BlockHouseClick(Sender: TObject);
var
  I: Integer;
  H: THouseType;
begin
  I := TKMButtonFlat(Sender).Tag;
  H := GUIHouseOrder[I];

  //Loop through states CanBuild > CantBuild > Released
  if not fPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] and not fPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] then
  begin
    fPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] := True;
    fPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] := False;
  end else
  if fPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] and not fPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] then
  begin
    fPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] := False;
    fPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] := True;
  end else
  begin
    fPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] := False;
    fPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] := False;
  end;

  Player_BlockHouseRefresh;
end;


procedure TKMapEdInterface.Player_BlockHouseRefresh;
var
  I: Integer;
  H: THouseType;
begin
  for I := 1 to GUI_HOUSE_COUNT do
  begin
    H := GUIHouseOrder[I];
    if fPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] and not fPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] then
      Image_BlockHouse[I].TexID := 32
    else
    if fPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] and not fPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] then
      Image_BlockHouse[I].TexID := 33
    else
    if not fPlayers[MySpectator.PlayerIndex].Stats.HouseGranted[H] and not fPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[H] then
      Image_BlockHouse[I].TexID := 0
    else
      Image_BlockHouse[I].TexID := 24; //Some erroneous value
  end;
end;


procedure TKMapEdInterface.Player_BlockTradeClick(Sender: TObject);
var
  I: Integer;
  R: TWareType;
begin
  I := TKMButtonFlat(Sender).Tag;
  R := StoreResType[I];

  fPlayers[MySpectator.PlayerIndex].Stats.AllowToTrade[R] := not fPlayers[MySpectator.PlayerIndex].Stats.AllowToTrade[R];

  Player_BlockTradeRefresh;
end;


procedure TKMapEdInterface.Player_BlockTradeRefresh;
var
  I: Integer;
  R: TWareType;
begin
  for I := 1 to STORE_RES_COUNT do
  begin
    R := StoreResType[I];
    if fPlayers[MySpectator.PlayerIndex].Stats.AllowToTrade[R] then
      Image_BlockTrade[I].TexID := 0
    else
      Image_BlockTrade[I].TexID := 32; //Red cross
  end;
end;


procedure TKMapEdInterface.Player_MarkerClick(Sender: TObject);
begin
  //Press the button
  if Sender = Button_Reveal then
  begin
    Button_Reveal.Down := not Button_Reveal.Down;
    Button_CenterScreen.Down := False;
  end;
  if Sender = Button_CenterScreen then
  begin
    Button_CenterScreen.Down := not Button_CenterScreen.Down;
    Button_Reveal.Down := False;
  end;

  if (Sender = nil) and (GameCursor.Mode = cmNone) then
  begin
    Button_Reveal.Down := False;
    Button_CenterScreen.Down := False;
  end;

  if Button_Reveal.Down then
  begin
    GameCursor.Mode := cmMarkers;
    GameCursor.Tag1 := MARKER_REVEAL;
    GameCursor.MapEdSize := TrackBar_RevealNewSize.Position;
  end
  else
  if Button_CenterScreen.Down then
  begin
    GameCursor.Mode := cmMarkers;
    GameCursor.Tag1 := MARKER_CENTERSCREEN;
  end
  else
  begin
    GameCursor.Mode := cmNone;
    GameCursor.Tag1 := 0;
  end;

  if Sender = CheckBox_RevealAll then
    fGame.MapEditor.RevealAll[MySpectator.PlayerIndex] := CheckBox_RevealAll.Checked
  else
    CheckBox_RevealAll.Checked := fGame.MapEditor.RevealAll[MySpectator.PlayerIndex];

  if Sender = Button_PlayerCenterScreen then
    fGame.Viewport.Position := KMPointF(fPlayers[MySpectator.PlayerIndex].CenterScreen); //Jump to location

  Button_PlayerCenterScreen.Caption := TypeToString(fPlayers[MySpectator.PlayerIndex].CenterScreen);
end;


procedure TKMapEdInterface.Mission_AlliancesChange(Sender: TObject);
var I,K: Integer;
begin
  if Sender = nil then
  begin
    for I:=0 to fPlayers.Count-1 do
    for K:=0 to fPlayers.Count-1 do
      if (fPlayers[I]<>nil)and(fPlayers[K]<>nil) then
        CheckBox_Alliances[I,K].Checked := (fPlayers.CheckAlliance(fPlayers[I].PlayerIndex, fPlayers[K].PlayerIndex)=at_Ally)
      else
        CheckBox_Alliances[I,K].Disable; //Player does not exist?
    exit;
  end;

  I := TKMCheckBox(Sender).Tag div fPlayers.Count;
  K := TKMCheckBox(Sender).Tag mod fPlayers.Count;
  if CheckBox_Alliances[I,K].Checked then fPlayers[I].Alliances[K] := at_Ally
                                     else fPlayers[I].Alliances[K] := at_Enemy;

  //Copy status to symmetrical item
  if CheckBox_AlliancesSym.Checked then
  begin
    CheckBox_Alliances[K,I].Checked := CheckBox_Alliances[I,K].Checked;
    fPlayers[K].Alliances[I] := fPlayers[I].Alliances[K];
  end;
end;


procedure TKMapEdInterface.Mission_ModeChange(Sender: TObject);
begin
  fGame.MissionMode := TKMissionMode(Radio_MissionMode.ItemIndex);
end;


procedure TKMapEdInterface.Mission_ModeUpdate;
begin
  Radio_MissionMode.ItemIndex := Byte(fGame.MissionMode);
end;


procedure TKMapEdInterface.Mission_PlayerTypesUpdate;
var I: Integer;
begin
  for I := 0 to fPlayers.Count - 1 do
  begin
    CheckBox_PlayerTypes[I, 0].Enabled := fPlayers[I].HasAssets;
    CheckBox_PlayerTypes[I, 1].Enabled := fPlayers[I].HasAssets;
    CheckBox_PlayerTypes[I, 2].Enabled := fPlayers[I].HasAssets;

    CheckBox_PlayerTypes[I, 0].Checked := fPlayers[I].HasAssets and (fGame.MapEditor.DefaultHuman = I);
    CheckBox_PlayerTypes[I, 1].Checked := fPlayers[I].HasAssets and fGame.MapEditor.PlayerHuman[I];
    CheckBox_PlayerTypes[I, 2].Checked := fPlayers[I].HasAssets and fGame.MapEditor.PlayerAI[I];
  end;
end;


procedure TKMapEdInterface.Mission_PlayerTypesChange(Sender: TObject);
var PlayerId: Integer;
begin
  PlayerId := TKMCheckBox(Sender).Tag;

  //There should be exactly one default human player
  if Sender = CheckBox_PlayerTypes[PlayerId, 0] then
    fGame.MapEditor.DefaultHuman := PlayerId;

  if Sender = CheckBox_PlayerTypes[PlayerId, 1] then
    fGame.MapEditor.PlayerHuman[PlayerId] := CheckBox_PlayerTypes[PlayerId, 1].Checked;

  if Sender = CheckBox_PlayerTypes[PlayerId, 2] then
    fGame.MapEditor.PlayerAI[PlayerId] := CheckBox_PlayerTypes[PlayerId, 2].Checked;

  Mission_PlayerTypesUpdate;
  UpdateAITabsEnabled;
end;


procedure TKMapEdInterface.KeyDown(Key: Word; Shift: TShiftState);
begin
  if fMyControls.KeyDown(Key, Shift) then
  begin
    fGame.Viewport.ReleaseScrollKeys; //Release the arrow keys when you open a window with an edit to stop them becoming stuck
    Exit; //Handled by Controls
  end;

  //DoPress is not working properly yet. GamePlay only uses DoClick so MapEd can be the same for now.
  //1-5 game menu shortcuts
  //if Key in [49..53] then
  //  Button_Main[Key-48].DoPress;

  //For now enter can open up Extra panel
  if Key = VK_RETURN then
    ExtraMessage_Switch(Image_Extra);

  if Key = VK_ESCAPE then
    if Image_MessageClose.Click
    or Image_ExtraClose.Click then ;

  //Scrolling
  if Key = VK_LEFT  then fGame.Viewport.ScrollKeyLeft  := True;
  if Key = VK_RIGHT then fGame.Viewport.ScrollKeyRight := True;
  if Key = VK_UP    then fGame.Viewport.ScrollKeyUp    := True;
  if Key = VK_DOWN  then fGame.Viewport.ScrollKeyDown  := True;
end;


procedure TKMapEdInterface.KeyUp(Key: Word; Shift: TShiftState);
begin
  if fMyControls.KeyUp(Key, Shift) then Exit; //Handled by Controls

  //1-5 game menu shortcuts
  if Key in [49..53] then
    Button_Main[Key-48].Click;

  //Scrolling
  if Key = VK_LEFT  then fGame.Viewport.ScrollKeyLeft  := False;
  if Key = VK_RIGHT then fGame.Viewport.ScrollKeyRight := False;
  if Key = VK_UP    then fGame.Viewport.ScrollKeyUp    := False;
  if Key = VK_DOWN  then fGame.Viewport.ScrollKeyDown  := False;

  //Backspace resets the zoom and view, similar to other RTS games like Dawn of War.
  //This is useful because it is hard to find default zoom using the scroll wheel, and if not zoomed 100% things can be scaled oddly (like shadows)
  if Key = VK_BACK  then fGame.Viewport.ResetZoom;
end;


procedure TKMapEdInterface.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var MyRect: TRect;
begin
  fMyControls.MouseDown(X,Y,Shift,Button);

  if (Button = mbMiddle) and (fMyControls.CtrlOver = nil) then
  begin
     fDragScrolling := True;
     //Restrict the cursor to the window, for now.
     //TODO: Allow one to drag out of the window, and still capture.
     {$IFDEF MSWindows}
       MyRect := fMain.ClientRect;
       ClipCursor(@MyRect);
     {$ENDIF}
     fDragScrollingCursorPos.X := X;
     fDragScrollingCursorPos.Y := Y;
     fDragScrollingViewportPos.X := fGame.Viewport.Position.X;
     fDragScrollingViewportPos.Y := fGame.Viewport.Position.Y;
     fResource.Cursors.Cursor := kmc_Drag;
     Exit;
  end;

  //So terrain brushes start on mouse down not mouse move
  if fMyControls.CtrlOver = nil then
    fGame.UpdateGameCursor(X, Y, Shift);
end;


procedure TKMapEdInterface.MouseMove(Shift: TShiftState; X,Y: Integer);
var
  P: TKMPoint;
  Marker: TKMMapEdMarker;
  VP: TKMPointF;
begin
  if fDragScrolling then
  begin
    VP.X := fDragScrollingViewportPos.X + (fDragScrollingCursorPos.X - X) / (CELL_SIZE_PX * fGame.Viewport.Zoom);
    VP.Y := fDragScrollingViewportPos.Y + (fDragScrollingCursorPos.Y - Y) / (CELL_SIZE_PX * fGame.Viewport.Zoom);
    fGame.Viewport.Position := VP;
    Exit;
  end;

  fMyControls.MouseMove(X,Y,Shift);

  if fMyControls.CtrlOver <> nil then
  begin
    //kmc_Edit and kmc_DragUp are handled by Controls.MouseMove (it will reset them when required)
    if not fGame.Viewport.Scrolling and not (fResource.Cursors.Cursor in [kmc_Edit,kmc_DragUp]) then
      fResource.Cursors.Cursor := kmc_Default;
    GameCursor.SState := []; //Don't do real-time elevate when the mouse is over controls, only terrain
    Exit;
  end
  else
    DisplayHint(nil); //Clear shown hint

  fGame.UpdateGameCursor(X,Y,Shift);
  if GameCursor.Mode = cmNone then
  begin
    Marker := fGame.MapEditor.HitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
    if Marker.MarkerType <> mtNone then
      fResource.Cursors.Cursor := kmc_Info
    else
    if MySpectator.HitTest(GameCursor.Cell.X, GameCursor.Cell.Y) <> nil then
      fResource.Cursors.Cursor := kmc_Info
    else
      if not fGame.Viewport.Scrolling then
        fResource.Cursors.Cursor := kmc_Default;
  end;

  Label_Coordinates.Caption := Format('X: %d, Y: %d', [GameCursor.Cell.X, GameCursor.Cell.Y]);

  if ssLeft in Shift then //Only allow placing of roads etc. with the left mouse button
  begin
    P := GameCursor.Cell; //Get cursor position tile-wise
    case GameCursor.Mode of
      cmRoad:      if fPlayers[MySpectator.PlayerIndex].CanAddFieldPlan(P, ft_Road) then
                   begin
                     //If there's a field remove it first so we don't get road on top of the field tile (undesired in MapEd)
                     if gTerrain.TileIsCornField(P) or gTerrain.TileIsWineField(P) then
                       gTerrain.RemField(P);
                     fPlayers[MySpectator.PlayerIndex].AddField(P, ft_Road);
                   end;
      cmField:     if fPlayers[MySpectator.PlayerIndex].CanAddFieldPlan(P, ft_Corn) then fPlayers[MySpectator.PlayerIndex].AddField(P, ft_Corn);
      cmWine:      if fPlayers[MySpectator.PlayerIndex].CanAddFieldPlan(P, ft_Wine) then fPlayers[MySpectator.PlayerIndex].AddField(P, ft_Wine);
      //cm_Wall:  if fPlayers[MySpectator.PlayerIndex].CanAddFieldPlan(P, ft_Wall) then fPlayers[MySpectator.PlayerIndex].AddField(P, ft_Wine);
      cmObjects:   if GameCursor.Tag1 = 255 then gTerrain.SetTree(P, 255); //Allow many objects to be deleted at once
      cmUnits:     if GameCursor.Tag1 = 255 then fPlayers.RemAnyUnit(P);
      cmErase:     case GetShownPage of
                      esp_Terrain:    gTerrain.Land[P.Y,P.X].Obj := 255;
                      esp_Buildings:  begin
                                        fPlayers.RemAnyHouse(P);
                                        if gTerrain.Land[P.Y,P.X].TileOverlay = to_Road then
                                          gTerrain.RemRoad(P);
                                        if gTerrain.TileIsCornField(P) or gTerrain.TileIsWineField(P) then
                                          gTerrain.RemField(P);
                                      end;
                    end;
    end;
  end;
end;


procedure TKMapEdInterface.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  P: TKMPoint;
  DP: TAIDefencePosition;
  SelMarker: TKMMapEdMarker;
begin
  if fDragScrolling then
  begin
    if Button = mbMiddle then
    begin
      fDragScrolling := False;
      fResource.Cursors.Cursor := kmc_Default; //Reset cursor
      fMain.ApplyCursorRestriction;
    end;
    Exit;
  end;

  if fMyControls.CtrlOver <> nil then
  begin
    fMyControls.MouseUp(X,Y,Shift,Button);
    Exit; //We could have caused fGame reinit, so exit at once
  end;

  fGame.UpdateGameCursor(X, Y, Shift); //Updates the shift state
  P := GameCursor.Cell; //Get cursor position tile-wise
  if Button = mbRight then
  begin
    RightClick_Cancel;

    //Right click performs some special functions and shortcuts
    case GameCursor.Mode of
      cmTiles:    begin
                    SetTileDirection(GameCursor.MapEdDir+1); //Rotate tile direction
                    TilesRandom.Checked := false; //Reset
                  end;
      cmObjects:  gTerrain.Land[P.Y,P.X].Obj := 255; //Delete object
    end;
    //Move the selected object to the cursor location
    if MySpectator.Selected is TKMHouse then
      TKMHouse(MySpectator.Selected).SetPosition(P); //Can place is checked in SetPosition

    if MySpectator.Selected is TKMUnit then
      TKMUnit(MySpectator.Selected).SetPosition(P);

    if MySpectator.Selected is TKMUnitGroup then
      TKMUnitGroup(MySpectator.Selected).Position := P;

    if Panel_Marker.Visible then
      case fActiveMarker.MarkerType of
        mtDefence:   begin
                       DP := fPlayers[fActiveMarker.Owner].AI.General.DefencePositions[fActiveMarker.Index];
                       DP.Position := KMPointDir(P, DP.Position.Dir);
                     end;
        mtRevealFOW: fGame.MapEditor.Revealers[fActiveMarker.Owner][fActiveMarker.Index] := P;
      end;
  end
  else
  if Button = mbLeft then //Only allow placing of roads etc. with the left mouse button
    case GameCursor.Mode of
      cmNone:     begin
                    //If there are some additional layers we first HitTest them
                    //as they are rendered ontop of Houses/Objects
                    SelMarker := fGame.MapEditor.HitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
                    if SelMarker.MarkerType <> mtNone then
                      ShowMarkerInfo(SelMarker)
                    else
                    begin
                      MySpectator.SelectHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);

                      if MySpectator.Selected is TKMHouse then
                        ShowHouseInfo(TKMHouse(MySpectator.Selected));
                      if MySpectator.Selected is TKMUnit then
                        ShowUnitInfo(TKMUnit(MySpectator.Selected));
                      if MySpectator.Selected is TKMUnitGroup then
                        ShowGroupInfo(TKMUnitGroup(MySpectator.Selected));
                    end;
                  end;
      cmRoad:     if fPlayers[MySpectator.PlayerIndex].CanAddFieldPlan(P, ft_Road) then
                  begin
                    //If there's a field remove it first so we don't get road on top of the field tile (undesired in MapEd)
                    if gTerrain.TileIsCornField(P) or gTerrain.TileIsWineField(P) then
                      gTerrain.RemField(P);
                    fPlayers[MySpectator.PlayerIndex].AddField(P, ft_Road);
                  end;
      cmField:    if fPlayers[MySpectator.PlayerIndex].CanAddFieldPlan(P, ft_Corn) then fPlayers[MySpectator.PlayerIndex].AddField(P, ft_Corn);
      cmWine:     if fPlayers[MySpectator.PlayerIndex].CanAddFieldPlan(P, ft_Wine) then fPlayers[MySpectator.PlayerIndex].AddField(P, ft_Wine);
      //cm_Wall:
      cmHouses:   if fPlayers[MySpectator.PlayerIndex].CanAddHousePlan(P, THouseType(GameCursor.Tag1)) then
                  begin
                    fPlayers[MySpectator.PlayerIndex].AddHouse(THouseType(GameCursor.Tag1), P.X, P.Y, true);
                    if not(ssShift in Shift) then Town_BuildChange(Button_BuildRoad);
                  end;
      cmElevate,
      cmEqualize:; //handled in UpdateStateIdle
      cmObjects:  gTerrain.SetTree(P, GameCursor.Tag1);
      cmUnits:    if GameCursor.Tag1 = 255 then
                    fPlayers.RemAnyUnit(P)
                  else
                  if gTerrain.CanPlaceUnit(P, TUnitType(GameCursor.Tag1)) then
                  begin
                    //Check if we can really add a unit
                    if TUnitType(GameCursor.Tag1) in [CITIZEN_MIN..CITIZEN_MAX] then
                      fPlayers[MySpectator.PlayerIndex].AddUnit(TUnitType(GameCursor.Tag1), P, False)
                    else
                    if TUnitType(GameCursor.Tag1) in [WARRIOR_MIN..WARRIOR_MAX] then
                      fPlayers[MySpectator.PlayerIndex].AddUnitGroup(TUnitType(GameCursor.Tag1), P, dir_S, 1, 1)
                    else
                      fPlayers.PlayerAnimals.AddUnit(TUnitType(GameCursor.Tag1), P);
                  end;
      cmMarkers:  case GameCursor.Tag1 of
                    MARKER_REVEAL:        fGame.MapEditor.Revealers[MySpectator.PlayerIndex].AddEntry(P, TrackBar_RevealNewSize.Position);
                    MARKER_DEFENCE:       fPlayers[MySpectator.PlayerIndex].AI.General.DefencePositions.Add(KMPointDir(P, dir_N), gt_Melee, 10, adt_FrontLine);
                    MARKER_CENTERSCREEN:  begin
                                            fPlayers[MySpectator.PlayerIndex].CenterScreen := P;
                                            Player_MarkerClick(nil); //Update XY display
                                          end;
                  end;
      cmErase:    case GetShownPage of
                    esp_Terrain:    gTerrain.Land[P.Y,P.X].Obj := 255;
                    esp_Buildings:  begin
                                      fPlayers.RemAnyHouse(P);
                                      if gTerrain.Land[P.Y,P.X].TileOverlay = to_Road then
                                        gTerrain.RemRoad(P);
                                      if gTerrain.TileIsCornField(P) or gTerrain.TileIsWineField(P) then
                                        gTerrain.RemField(P);
                                    end;
                  end;
    end;
end;


function TKMapEdInterface.GetShownPage: TKMMapEdShownPage;
begin
  Result := esp_Unknown;
  if Panel_Terrain.Visible then   Result := esp_Terrain;
  if Panel_Build.Visible then     Result := esp_Buildings;
  if Panel_Markers.Visible then Result := esp_Reveal;
end;


end.

