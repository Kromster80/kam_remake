unit KM_InterfaceMapEditor;
{$I KaM_Remake.inc}
interface
uses
     {$IFDEF MSWindows} Windows, {$ENDIF}
     {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
     Classes, Controls, KromUtils, Math, StrUtils, SysUtils, KromOGLUtils, TypInfo,
     KM_Controls, KM_Defaults, KM_Pics, KM_Maps, KM_Houses, KM_Units, KM_UnitGroups, KM_MapEditor,
     KM_Points, KM_InterfaceDefaults, KM_Terrain;

type
  TKMVillageTab = (vtHouses, vtUnits, vtScript, vtDefences);

type
  TKMapEdInterface = class (TKMUserInterface)
  private
    fPrevHint: TObject;
    fStorehouseItem: Byte; //Selected ware in storehouse
    fBarracksItem: Byte; //Selected ware in barracks
    fTileDirection: Byte;
    fActiveMarker: TKMMapEdMarker;

    fMaps: TKMapsCollection;
    fMapsMP: TKMapsCollection;

    procedure Create_Terrain_Page;
    procedure Create_Village_Page;
    procedure Create_Player_Page;
    procedure Create_Mission_Page;
    procedure Create_Menu;
    procedure Create_MenuSave;
    procedure Create_MenuLoad;
    procedure Create_MenuQuit;
    procedure Create_Extra_Page;
    procedure Create_Unit;
    procedure Create_House;
    procedure Create_HouseStore;
    procedure Create_HouseBarracks;
    procedure Create_Markers_Page;
    procedure Create_FormationsPopUp;

    procedure SwitchPage(Sender: TObject);
    procedure DisplayHint(Sender: TObject);
    procedure Minimap_Update(Sender: TObject; const X,Y: Integer);
    procedure ShowHouseInfo(Sender:TKMHouse);
    procedure ShowUnitInfo(Sender:TKMUnit);
    procedure ShowGroupInfo(Sender: TKMUnitGroup);
    procedure ShowMarkerInfo(aMarker: TKMMapEdMarker);

    procedure Menu_Save(Sender: TObject);
    procedure Menu_Load(Sender: TObject);
    procedure Menu_QuitMission(Sender: TObject);
    procedure Load_MapTypeChange(Sender: TObject);
    procedure Load_MapListUpdate;
    procedure Load_MapListUpdateDone(Sender: TObject);
    procedure Terrain_HeightChange(Sender: TObject);
    procedure Terrain_TilesChange(Sender: TObject);
    procedure Terrain_ObjectsChange(Sender: TObject);
    procedure Build_ButtonClick(Sender: TObject);
    procedure Defence_Refresh;
    procedure Defence_Change(Sender: TObject);
    procedure Defence_ListClick(Sender: TObject);
    procedure House_HealthChange(Sender: TObject; AButton: TMouseButton);
    procedure Unit_ButtonClick(Sender: TObject);
    procedure Unit_ArmyChange1(Sender: TObject); overload;
    procedure Unit_ArmyChange2(Sender: TObject; AButton: TMouseButton); overload;
    procedure Barracks_FillValues(Sender: TObject);
    procedure Barracks_SelectWare(Sender: TObject);
    procedure Barracks_EditWareCount(Sender: TObject; AButton:TMouseButton);
    procedure Store_FillValues(Sender: TObject);
    procedure Store_SelectWare(Sender: TObject);
    procedure Store_EditWareCount(Sender: TObject; AButton:TMouseButton);
    procedure Player_ChangeActive(Sender: TObject);
    procedure SetActivePlayer(aIndex: TPlayerIndex);
    procedure SetTileDirection(aTileDirection: byte);
    procedure Village_ScriptRefresh;
    procedure Village_ScriptChange(Sender: TObject);
    procedure Player_BlockClick(Sender: TObject);
    procedure Player_BlockRefresh;
    procedure Player_ColorClick(Sender: TObject);
    procedure Player_RevealClick(Sender: TObject);
    procedure Mission_AlliancesChange(Sender: TObject);
    procedure Mission_PlayerTypesChange(Sender: TObject);
    procedure View_Passability(Sender: TObject);
    procedure Marker_Change(Sender: TObject);
    procedure Formations_Show(Sender: TObject);
    procedure Formations_Close(Sender: TObject);

    function GetSelectedTile: TObject;
    function GetSelectedObject: TObject;
    function GetSelectedUnit: TObject;
  protected
    Panel_Main:TKMPanel;
      MinimapView: TKMMinimapView;
      Label_Coordinates:TKMLabel;
      TrackBar_Passability:TKMTrackBar;
      Label_Passability:TKMLabel;
      Button_PlayerSelect:array[0..MAX_PLAYERS-1]of TKMFlatButtonShape; //Animals are common for all
      Label_Stat,Label_Hint:TKMLabel;
      Bevel_HintBG: TKMBevel;
      Label_MatAmount: TKMLabel;
      Shape_MatAmount: TKMShape;
      Label_DefenceID: TKMLabel;
      Label_DefencePos: TKMLabel;
      Shape_DefencePos: TKMShape;

    Panel_Common:TKMPanel;
      Button_Main:array[1..5]of TKMButton; //5 buttons
      Label_MenuTitle: TKMLabel; //Displays the title of the current menu below
      Label_MissionName: TKMLabel;
      Image_Extra: TKMImage;

    Panel_Terrain:TKMPanel;
      Button_Terrain:array[1..4]of TKMButton;
      Panel_Brushes:TKMPanel;
        BrushSize:TKMTrackBar;
        BrushCircle,BrushSquare:TKMButtonFlat;
        //BrushesTable:array[1..27] of TKMButtonFlat; // todo
      Panel_Heights:TKMPanel;
        HeightSize, HeightSlope, HeightSpeed:TKMTrackBar;
        HeightShapeLabel:TKMLabel;
        HeightCircle,HeightSquare:TKMButtonFlat;
        HeightElevate, HeightUnequalize: TKMButtonFlat;
      Panel_Tiles:TKMPanel;
        TilesTable:array[1..MAPED_TILES_COLS*MAPED_TILES_ROWS] of TKMButtonFlat; //how many are visible?
        TilesScroll:TKMScrollBar;
        TilesRandom:TKMCheckBox;
      Panel_Objects:TKMPanel;
        ObjectErase:TKMButtonFlat;
        ObjectsTable:array[0..8] of TKMButtonFlat;
        ObjectsScroll:TKMScrollBar;

    //todo: How to know where certain page should be?
    //Village - panels that will become mostly obsolete in a battle mission?
    //Player - panels that make sense both in town and battle mode?

    Panel_Village: TKMPanel;
      Button_Village: array [TKMVillageTab] of TKMButton;
      Panel_Build: TKMPanel;
        Button_BuildRoad,Button_BuildField,Button_BuildWine,Button_BuildCancel: TKMButtonFlat;
        Button_Build: array [1..GUI_HOUSE_COUNT] of TKMButtonFlat;
      Panel_Units:TKMPanel;
        Button_UnitCancel:TKMButtonFlat;
        Button_Citizen:array[0..13]of TKMButtonFlat;
        Button_Warriors:array[0..13]of TKMButtonFlat;
        Button_Animals:array[0..7]of TKMButtonFlat;
      Panel_Script: TKMPanel;
        CheckBox_AutoBuild: TKMCheckBox;
        CheckBox_AutoRepair: TKMCheckBox;
        TrackBar_SerfFactor: TKMTrackBar;
        TrackBar_WorkerFactor: TKMTrackBar;
      Panel_Defence: TKMPanel;
        CheckBox_AutoDefence: TKMCheckBox;
        TrackBar_EquipRateLeather: TKMTrackBar;
        TrackBar_EquipRateIron: TKMTrackBar;
        TrackBar_RecruitFactor: TKMTrackBar;
        List_Defences: TKMListBox;
        Button_EditFormations: TKMButton;

    Panel_Formations: TKMPanel;
      Image_FormationsFlag: TKMImage;
      NumEdit_FormationsCount,
      NumEdit_FormationsColumns: array [TGroupType] of TKMNumericEdit;
      Button_Formations_Ok: TKMButton;
      Button_Formations_Cancel: TKMButton;

    Panel_Player:TKMPanel;
      Button_Player:array[1..4]of TKMButton;
      Panel_Goals:TKMPanel;
      Panel_Color:TKMPanel;
        ColorSwatch_Color:TKMColorSwatch;
      Panel_Block: TKMPanel;
        Button_BlockHouse: array [1 .. GUI_HOUSE_COUNT] of TKMButtonFlat;
        Image_BlockHouse: array [1 .. GUI_HOUSE_COUNT] of TKMImage;
      Panel_RevealFOW: TKMPanel;
        Button_Reveal: TKMButtonFlat;
        TrackBar_RevealNewSize: TKMTrackBar;
        CheckBox_RevealAll: TKMCheckBox;

    Panel_Mission:TKMPanel;
      Button_Mission:array[1..2]of TKMButton;
      Panel_Alliances:TKMPanel;
        CheckBox_Alliances: array[0..MAX_PLAYERS-1,0..MAX_PLAYERS-1] of TKMCheckBox;
        CheckBox_AlliancesSym:TKMCheckBox;
      Panel_PlayerTypes:TKMPanel;
        CheckBox_PlayerTypes: array[0..MAX_PLAYERS-1,0..1] of TKMCheckBox;

    Panel_Menu:TKMPanel;
      Button_Menu_Save,Button_Menu_Load,Button_Menu_Settings,Button_Menu_Quit:TKMButton;

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
      Panel_MarkerDefence: TKMPanel;
        DropList_DefenceGroup: TKMDropList;
        DropList_DefenceType: TKMDropList;
        TrackBar_DefenceRad: TKMTrackBar;
        Button_DefenceCW: TKMButton;
        Button_DefenceCCW: TKMButton;


  public
    constructor Create(aScreenX, aScreenY: word);
    destructor Destroy; override;
    procedure Player_UpdateColors;
    procedure SetMinimap;
    procedure SetMapName(const aName:string);
    procedure RightClick_Cancel;
    function GetShownPage: TKMMapEdShownPage;
    procedure SetLoadMode(aMultiplayer:boolean);

    procedure KeyDown(Key:Word; Shift: TShiftState); override;
    procedure KeyUp(Key:Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;

    procedure Resize(X,Y: Word); override;
    procedure UpdateState(aTickCount: Cardinal); override;
    procedure Paint; override;
  end;


implementation

uses
  KM_PlayersCollection, KM_Player, KM_TextLibrary, KM_Game, KM_GameApp, KM_Resource,
  KM_ResourceUnit, KM_ResourceCursors, KM_ResourceMapElements, KM_AIDefensePos, KM_ResourceHouse;


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
var
  I,K: Integer;
begin
  //Reset cursor mode
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  if fGame <> nil then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers - [mlDefences, mlRevealFOW];

  //If the user clicks on the tab that is open, it closes it (main buttons only)
  if ((Sender = Button_Main[1]) and Panel_Terrain.Visible) or
     ((Sender = Button_Main[2]) and Panel_Village.Visible) or
     ((Sender = Button_Main[3]) and Panel_Player.Visible) or
     ((Sender = Button_Main[4]) and Panel_Mission.Visible) or
     ((Sender = Button_Main[5]) and Panel_Menu.Visible) then
     Sender := nil;

  //Reset shown item if user clicked on any of the main buttons
  if (Sender=Button_Main[1])or(Sender=Button_Main[2])or
     (Sender=Button_Main[3])or(Sender=Button_Main[4])or
     (Sender=Button_Main[5])or
     (Sender=Button_Menu_Settings)or(Sender=Button_Menu_Quit) then
  begin
    fPlayers.Selected := nil;
  end;

  Label_MenuTitle.Caption := '';
  //Now hide all existing pages
  for I := 1 to Panel_Common.ChildCount do
    if Panel_Common.Childs[I] is TKMPanel then
    begin
      Panel_Common.Childs[I].Hide;
      for K := 1 to TKMPanel(Panel_Common.Childs[I]).ChildCount do
        if TKMPanel(Panel_Common.Childs[I]).Childs[K] is TKMPanel then
          TKMPanel(Panel_Common.Childs[I]).Childs[K].Hide;
    end;

  if (Sender = Button_Main[1]) or (Sender = Button_Terrain[1]) then begin
    Panel_Terrain.Show;
    Panel_Brushes.Show;
    Label_MenuTitle.Caption:='Terrain - Brushes';
  end else

  if (Sender = Button_Terrain[2]) then
  begin
    Panel_Terrain.Show;
    Panel_Heights.Show;
    Label_MenuTitle.Caption:='Terrain - Heights';
    Terrain_HeightChange(HeightElevate); //Select the default mode
  end else

  if (Sender = Button_Terrain[3]) then
  begin
    Panel_Terrain.Show;
    Panel_Tiles.Show;
    Label_MenuTitle.Caption:='Terrain - Tiles';
    SetTileDirection(fTileDirection); //ensures tags are in allowed ranges
    Terrain_TilesChange(GetSelectedTile);
  end else

  if (Sender = Button_Terrain[4]) then
  begin
    Panel_Terrain.Show;
    Panel_Objects.Show;
    Label_MenuTitle.Caption:='Terrain - Objects';
    Terrain_ObjectsChange(GetSelectedObject);
  end else

  if (Sender = Button_Main[2])or(Sender = Button_Village[vtHouses]) then
  begin
    Panel_Village.Show;
    Panel_Build.Show;
    Label_MenuTitle.Caption := 'Village - Buildings';
    Build_ButtonClick(Button_BuildRoad);
  end else

  if (Sender = Button_Village[vtUnits]) then
  begin
    Panel_Village.Show;
    Panel_Units.Show;
    Label_MenuTitle.Caption := 'Village - Units';
    Unit_ButtonClick(GetSelectedUnit);
  end
  else
  if (Sender = Button_Village[vtScript]) then
  begin
    Village_ScriptRefresh;
    Panel_Village.Show;
    Panel_Script.Show;
    Label_MenuTitle.Caption := 'Village - Script';
  end
  else
  if (Sender = Button_Village[vtDefences]) then
  begin
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlDefences];
    Defence_Refresh;
    Panel_Village.Show;
    Panel_Defence.Show;
    Label_MenuTitle.Caption := 'Village - Defences';
  end else

  if (Sender = Button_Main[3])or(Sender = Button_Player[1]) then
  begin
    Panel_Player.Show;
    Panel_Goals.Show;
    Label_MenuTitle.Caption:='Player - Goals';
  end else

  if (Sender = Button_Player[2]) then
  begin
    Panel_Player.Show;
    Panel_Color.Show;
    Label_MenuTitle.Caption:='Player - Color';
  end else

  if (Sender = Button_Player[3]) then
  begin
    Player_BlockRefresh;
    Panel_Player.Show;
    Panel_Block.Show;
    Label_MenuTitle.Caption:='Player - Block houses';
  end else

  if (Sender = Button_Player[4]) then
  begin
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlRevealFOW];
    Panel_Player.Show;
    Panel_RevealFOW.Show;
    Label_MenuTitle.Caption:='Player - Reveal fog';
  end else

  if (Sender = Button_Main[4])or(Sender = Button_Mission[1]) then
  begin
    Panel_Mission.Show;
    Panel_Alliances.Show;
    Label_MenuTitle.Caption:='Mission - Alliances';
    Mission_AlliancesChange(nil);
  end else

  if (Sender = Button_Mission[2]) then
  begin
    Panel_Mission.Show;
    Panel_PlayerTypes.Show;
    Label_MenuTitle.Caption:='Mission - Player Types';
    Mission_PlayerTypesChange(nil);
  end else

  if (Sender = Button_Main[5]) or
     (Sender = Button_Quit_No) or
     (Sender = Button_LoadCancel) or
     (Sender = Button_SaveCancel) then
  begin
    Panel_Menu.Show;
    Label_MenuTitle.Caption := fTextLibrary[TX_MENU_TAB_OPTIONS];
  end else

  if Sender = Button_Menu_Quit then
  begin
    Panel_Quit.Show;
  end;

  if Sender = Button_Menu_Save then
  begin
    Edit_SaveName.Text := fGame.GameName;
    Menu_Save(Edit_SaveName);
    Panel_Save.Show;
  end;

  if Sender = Button_Menu_Load then
  begin
    Load_MapListUpdate;
    Panel_Load.Show;
  end;

  //Info pages

  if Sender = Panel_Unit then
    TKMPanel(Sender).Show
  else

  if Sender = Panel_House then
    TKMPanel(Sender).Show
  else

  if Sender = Panel_HouseBarracks then
  begin
    TKMPanel(Sender).Parent.Show;
    TKMPanel(Sender).Show;
  end else

  if Sender = Panel_HouseStore then
  begin
    TKMPanel(Sender).Parent.Show;
    TKMPanel(Sender).Show;
  end else

  if Sender = Panel_MarkerReveal then
  begin
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlRevealFOW];
    TKMPanel(Sender).Parent.Show;
    TKMPanel(Sender).Show;
  end else

  if Sender = Panel_MarkerDefence then
  begin
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlDefences];
    TKMPanel(Sender).Parent.Show;
    TKMPanel(Sender).Show;
  end;

  //Additional panels

  if Sender = Image_Extra then
    Panel_Extra.Show
  else

  if Sender = Image_ExtraClose then
    Panel_Extra.Hide;
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
    Bevel_HintBG.Width := 8 + fResource.ResourceFont.GetTextSize(Label_Hint.Caption, Label_Hint.Font).X;
  end;

  fPrevHint := Sender;
end;


procedure TKMapEdInterface.Formations_Show(Sender: TObject);
var
  GT: TGroupType;
begin
  Assert(MyPlayer <> nil);

  Image_FormationsFlag.FlagColor := MyPlayer.FlagColor;
  for GT := Low(TGroupType) to High(TGroupType) do
  begin
    NumEdit_FormationsCount[GT].Value := MyPlayer.AI.General.DefencePositions.TroopFormations[GT].NumUnits;
    NumEdit_FormationsColumns[GT].Value := MyPlayer.AI.General.DefencePositions.TroopFormations[GT].UnitsPerRow;
  end;

  Panel_Formations.Show;
end;


procedure TKMapEdInterface.Formations_Close(Sender: TObject);
var
  GT: TGroupType;
begin
  Assert(Image_FormationsFlag.FlagColor = MyPlayer.FlagColor, 'Cheap test to see if active player didn''t changed');

  if Sender = Button_Formations_Ok then
  for GT := Low(TGroupType) to High(TGroupType) do
  begin
    MyPlayer.AI.General.DefencePositions.TroopFormations[GT].NumUnits := NumEdit_FormationsCount[GT].Value;
    MyPlayer.AI.General.DefencePositions.TroopFormations[GT].UnitsPerRow := NumEdit_FormationsColumns[GT].Value;
  end;

  Panel_Formations.Hide;
end;


//Update viewport position when user interacts with minimap
procedure TKMapEdInterface.Minimap_Update(Sender: TObject; const X,Y: Integer);
begin
  fGame.Viewport.Position := KMPointF(X,Y);
end;


constructor TKMapEdInterface.Create(aScreenX, aScreenY: Word);
var
  I: Integer;
begin
  inherited;

  fBarracksItem   := 1; //First ware selected by default
  fStorehouseItem := 1; //First ware selected by default
  fTileDirection := 0;
  fMaps := TKMapsCollection.Create(False);
  fMapsMP := TKMapsCollection.Create(True);

  //Parent Page for whole toolbar in-game
  Panel_Main := TKMPanel.Create(fMyControls, 0, 0, aScreenX, aScreenY);

    Label_MatAmount := TKMLabel.Create(Panel_Main, 0, 0, '', fnt_Metal, taCenter);
    Shape_MatAmount := TKMShape.Create(Panel_Main,0,0,80,20);
    Shape_MatAmount.LineWidth := 2;
    Shape_MatAmount.LineColor := $F000FF00;
    Shape_MatAmount.FillColor := $80000000;

    Label_DefenceID := TKMLabel.Create(Panel_Main, 0, 0, '', fnt_Metal, taCenter);
    Label_DefencePos := TKMLabel.Create(Panel_Main, 0, 0, '', fnt_Metal, taCenter);
    Shape_DefencePos := TKMShape.Create(Panel_Main,0,0,80,20);
    Shape_DefencePos.LineWidth := 2;
    Shape_DefencePos.LineColor := $F0FF8000;
    Shape_DefencePos.FillColor := $80000000;

    TKMImage.Create(Panel_Main,0,   0,224,200,407); //Minimap place
    TKMImage.Create(Panel_Main,0, 200,224,400,404);
    TKMImage.Create(Panel_Main,0, 600,224,400,404);
    TKMImage.Create(Panel_Main,0,1000,224,400,404); //For 1600x1200 this is needed

    MinimapView := TKMMinimapView.Create(Panel_Main, 10, 10, 176, 176);
    MinimapView.OnChange := Minimap_Update;

    Label_MissionName := TKMLabel.Create(Panel_Main, 230, 10, 184, 10, '<<<LEER>>>', fnt_Grey, taLeft);
    Label_Coordinates := TKMLabel.Create(Panel_Main, 230, 30, 'X: Y:', fnt_Grey, taLeft);
    Label_Stat := TKMLabel.Create(Panel_Main, 230, 50, 0, 0, '', fnt_Outline, taLeft);

    TKMLabel.Create(Panel_Main, TB_PAD, 195, TB_WIDTH, 0, 'Players:', fnt_Outline, taLeft);
    for I := 0 to MAX_PLAYERS - 1 do
    begin
      Button_PlayerSelect[I]         := TKMFlatButtonShape.Create(Panel_Main, 8 + I*23, 215, 21, 21, inttostr(I+1), fnt_Grey, $FF0000FF);
      Button_PlayerSelect[I].Tag     := I;
      Button_PlayerSelect[I].OnClick := Player_ChangeActive;
    end;

    Bevel_HintBG := TKMBevel.Create(Panel_Main,224+32,Panel_Main.Height-23,300,21);
    Bevel_HintBG.BackAlpha := 0.5;
    Bevel_HintBG.Hide;
    Bevel_HintBG.Anchors := [akLeft, akBottom];

    Label_Hint := TKMLabel.Create(Panel_Main, 224 + 36, Panel_Main.Height - 21, 0, 0, '', fnt_Outline, taLeft);
    Label_Hint.Anchors := [akLeft, akBottom];

  Panel_Common := TKMPanel.Create(Panel_Main,TB_PAD,255,TB_WIDTH,768);

    {5 big tabs}
    Button_Main[1] := TKMButton.Create(Panel_Common, BIG_PAD_W*0, 0, BIG_TAB_W, BIG_TAB_H, 381, rxGui, bsGame);
    Button_Main[2] := TKMButton.Create(Panel_Common, BIG_PAD_W*1, 0, BIG_TAB_W, BIG_TAB_H, 368, rxGui, bsGame);
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

    Label_MenuTitle := TKMLabel.Create(Panel_Common,0,40,TB_WIDTH,0,'',fnt_Metal,taLeft);


{I plan to store all possible layouts on different pages which gets displayed one at a time}
{==========================================================================================}
  Create_Terrain_Page;
  Create_Village_Page;
  Create_Player_Page;
  Create_Mission_Page;

  Create_Menu;
    Create_MenuSave;
    Create_MenuLoad;
    Create_MenuQuit;

  Create_Extra_Page;

  Create_Unit;
  Create_House;
    Create_HouseStore;
    Create_HouseBarracks;
    //Create_TownHall_Page;
  Create_Markers_Page;

  Image_Extra := TKMImage.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - 48, 30, 48, 494);
  Image_Extra.Anchors := [akLeft, akBottom];
  Image_Extra.HighlightOnMouseOver := True;
  Image_Extra.OnClick := SwitchPage;

  //Pages that need to be on top of everything
  Create_FormationsPopUp;

  fMyControls.OnHint := DisplayHint;

  SwitchPage(nil); //Update
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
procedure TKMapEdInterface.Create_Terrain_Page;
var i,k:Integer;
begin
  Panel_Terrain := TKMPanel.Create(Panel_Common,0,60,TB_WIDTH,28);
    Button_Terrain[1] := TKMButton.Create(Panel_Terrain, SMALL_PAD_W * 0, 0, SMALL_TAB_W, SMALL_TAB_H, 383, rxGui, bsGame);
    Button_Terrain[1].Hint := fTextLibrary[TX_MAPED_TERRAIN_HINTS_BRUSHES];
    Button_Terrain[2] := TKMButton.Create(Panel_Terrain, SMALL_PAD_W * 1, 0, SMALL_TAB_W, SMALL_TAB_H, 388, rxGui, bsGame);
    Button_Terrain[2].Hint := fTextLibrary[TX_MAPED_TERRAIN_HINTS_HEIGHTS];
    Button_Terrain[3] := TKMButton.Create(Panel_Terrain, SMALL_PAD_W * 2, 0, SMALL_TAB_W, SMALL_TAB_H, 382, rxGui, bsGame);
    Button_Terrain[3].Hint := fTextLibrary[TX_MAPED_TERRAIN_HINTS_TILES];
    Button_Terrain[4] := TKMButton.Create(Panel_Terrain, SMALL_PAD_W * 3, 0, SMALL_TAB_W, SMALL_TAB_H, 385, rxGui, bsGame);
    Button_Terrain[4].Hint := fTextLibrary[TX_MAPED_TERRAIN_HINTS_OBJECTS];
    for i:=1 to 4 do Button_Terrain[i].OnClick := SwitchPage;

    Panel_Brushes := TKMPanel.Create(Panel_Terrain,0,28,TB_WIDTH,400);
      BrushSize   := TKMTrackBar.Create(Panel_Brushes, 0, 10, 100, 1, 12);
      BrushCircle := TKMButtonFlat.Create(Panel_Brushes, 116, 8, 24, 24, 592);
      BrushCircle.Hint := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_CIRCLE];
      BrushSquare := TKMButtonFlat.Create(Panel_Brushes, 134, 8, 24, 24, 593);
      BrushSquare.Hint := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SQUARE];

      TKMButtonFlat.Create(Panel_Brushes, 0, 30, 32, 32, 1, rxTiles);   // grass

      {TKMButtonFlat.Create(Panel_Brushes, 40, 30, 32, 32, 9, rxTiles);  // grass 2
      TKMButtonFlat.Create(Panel_Brushes, 8, 62, 32, 32, 35, rxTiles);  // dirt

      {BrushSize.OnChange   := TerrainBrush_Change;
      BrushCircle.OnChange := TerrainBrush_Change;
      BrushSquare.OnChange := TerrainBrush_Change;}

    Panel_Heights := TKMPanel.Create(Panel_Terrain,0,28,TB_WIDTH,400);
      HeightSize   := TKMTrackBar.Create(Panel_Heights, 0, 40, TB_WIDTH, 1, 15); //1..15(4bit) for size
      HeightSize.Caption := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SIZE];
      HeightSize.Hint :=   fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SIZE_HINT];
      HeightSlope  := TKMTrackBar.Create(Panel_Heights, 0, 94, TB_WIDTH, 1, 15); //1..15(4bit) for slope shape
      HeightSlope.Caption := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SLOPE];
      HeightSlope.Hint := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SLOPE_HINT];
      HeightSpeed  := TKMTrackBar.Create(Panel_Heights, 0, 148, TB_WIDTH, 1, 15); //1..15(4bit) for speed
      HeightSpeed.Caption := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SPEED];
      HeightSpeed.Hint := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SPEED_HINT];

      HeightShapeLabel := TKMLabel.Create(Panel_Heights, 0, 14, TB_WIDTH, 0, fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SHAPE], fnt_Metal, taLeft);
      HeightCircle := TKMButtonFlat.Create(Panel_Heights, 120, 10, 24, 24, 592);
      HeightCircle.Hint := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_CIRCLE];
      HeightSquare := TKMButtonFlat.Create(Panel_Heights, 150, 10, 24, 24, 593);
      HeightSquare.Hint := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_SQUARE];

      HeightElevate             := TKMButtonFlat.Create(Panel_Heights, 0, 204, TB_WIDTH, 20, 0);
      HeightElevate.OnClick     := Terrain_HeightChange;
      HeightElevate.Down        := True;
      HeightElevate.Caption     := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_ELEVATE];
      HeightElevate.CapOffsetY  := -12;
      HeightElevate.Hint        := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_ELEVATE_HINT];
      HeightUnequalize          := TKMButtonFlat.Create(Panel_Heights, 0, 234, TB_WIDTH, 20, 0);
      HeightUnequalize.OnClick  := Terrain_HeightChange;
      HeightUnequalize.Caption  := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_UNEQUALIZE];
      HeightUnequalize.CapOffsetY  := -12;
      HeightUnequalize.Hint      := fTextLibrary[TX_MAPED_TERRAIN_HEIGHTS_UNEQUALIZE_HINT];

      HeightSize.OnChange   := Terrain_HeightChange;
      HeightSlope.OnChange  := Terrain_HeightChange;
      HeightSpeed.OnChange  := Terrain_HeightChange;
      HeightCircle.OnClick  := Terrain_HeightChange;
      HeightSquare.OnClick  := Terrain_HeightChange;

    Panel_Tiles := TKMPanel.Create(Panel_Terrain, 0, 28, TB_WIDTH, 400);
      TilesRandom := TKMCheckBox.Create(Panel_Tiles, 0, 4, TB_WIDTH, 20, fTextLibrary[TX_MAPED_TERRAIN_TILES_RANDOM], fnt_Metal);
      TilesRandom.Checked := True;
      TilesRandom.OnClick := Terrain_TilesChange;
      TilesRandom.Hint := fTextLibrary[TX_MAPED_TERRAIN_TILES_RANDOM_HINT];
      TilesScroll := TKMScrollBar.Create(Panel_Tiles, 2, 30 + 4 + MAPED_TILES_ROWS * 32, 194, 20, sa_Horizontal, bsGame);
      TilesScroll.MaxValue := 256 div MAPED_TILES_ROWS - MAPED_TILES_COLS; // 16 - 6
      TilesScroll.Position := 0;
      TilesScroll.OnChange := Terrain_TilesChange;
      for i:=1 to MAPED_TILES_COLS do for k:=1 to MAPED_TILES_ROWS do begin
        TilesTable[(i-1)*MAPED_TILES_ROWS+k] := TKMButtonFlat.Create(Panel_Tiles,(i-1)*32,30+(k-1)*32,32,32,1,rxTiles); //2..9
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].Tag := (k-1)*MAPED_TILES_COLS+i; //Store ID
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].OnClick := Terrain_TilesChange;
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].OnMouseWheel := TilesScroll.MouseWheel;
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].Hint := fTextLibrary[TX_MAPED_TERRAIN_TILES_MAIN_HINT];
      end;
      Terrain_TilesChange(TilesScroll); //This ensures that the displayed images get updated the first time
      Terrain_TilesChange(TilesTable[1]);

    Panel_Objects := TKMPanel.Create(Panel_Terrain,0,28,TB_WIDTH,400);
      ObjectsScroll := TKMScrollBar.Create(Panel_Objects, 0, 295, TB_WIDTH, 20, sa_Horizontal, bsGame);
      ObjectsScroll.MinValue := 0;
      ObjectsScroll.MaxValue := fResource.MapElements.ValidCount div 3 - 2;
      ObjectsScroll.Position := 0;
      ObjectsScroll.OnChange := Terrain_ObjectsChange;
      ObjectErase := TKMButtonFlat.Create(Panel_Objects, 0, 8,32,32,340);
      ObjectErase.Hint := fTextLibrary[TX_MAPED_TERRAIN_OBJECTS_REMOVE];
      for I := 0 to 2 do for K := 0 to 2 do
      begin
        ObjectsTable[I*3+K] := TKMButtonFlat.Create(Panel_Objects, I*65, 40+K*85,64,84,1,rxTrees); //RXid=1  // 1 2
        ObjectsTable[I*3+K].Tag := I*3+K; //Store ID
        ObjectsTable[I*3+K].OnClick := Terrain_ObjectsChange;
        ObjectsTable[I*3+K].OnMouseWheel := ObjectsScroll.MouseWheel;
      end;
      ObjectErase.Tag := 255; //no object
      ObjectErase.OnClick := Terrain_ObjectsChange;
    Terrain_ObjectsChange(ObjectsScroll); //This ensures that the displayed images get updated the first time
    Terrain_ObjectsChange(ObjectsTable[0]);
end;


{Build page}
procedure TKMapEdInterface.Create_Village_Page;
const
  VillageTabIcon: array [TKMVillageTab] of Word = (391, 141, 327, 43);
var
  I: Integer;
  VT: TKMVillageTab;
begin
  Panel_Village := TKMPanel.Create(Panel_Common, 0, 60, TB_WIDTH, 28);

    for VT := Low(TKMVillageTab) to High(TKMVillageTab) do
    begin
      Button_Village[VT] := TKMButton.Create(Panel_Village, SMALL_PAD_W * Byte(VT), 0, SMALL_TAB_W, SMALL_TAB_H, VillageTabIcon[VT], rxGui, bsGame);
      Button_Village[VT].OnClick := SwitchPage;
    end;

    //Town placement
    Panel_Build := TKMPanel.Create(Panel_Village,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Build,0,10,TB_WIDTH,0,'Roadworks',fnt_Outline,taCenter);
      Button_BuildRoad   := TKMButtonFlat.Create(Panel_Build,  0,28,33,33,335);
      Button_BuildField  := TKMButtonFlat.Create(Panel_Build, 37,28,33,33,337);
      Button_BuildWine   := TKMButtonFlat.Create(Panel_Build, 74,28,33,33,336);
      Button_BuildCancel := TKMButtonFlat.Create(Panel_Build,148,28,33,33,340);
      Button_BuildRoad.OnClick  := Build_ButtonClick;
      Button_BuildField.OnClick := Build_ButtonClick;
      Button_BuildWine.OnClick  := Build_ButtonClick;
      Button_BuildCancel.OnClick:= Build_ButtonClick;
      Button_BuildRoad.Hint     := fTextLibrary[TX_BUILD_ROAD_HINT];
      Button_BuildField.Hint    := fTextLibrary[TX_BUILD_FIELD_HINT];
      Button_BuildWine.Hint     := fTextLibrary[TX_BUILD_WINE_HINT];
      Button_BuildCancel.Hint   := fTextLibrary[TX_BUILD_CANCEL_HINT];

      TKMLabel.Create(Panel_Build,0,65,TB_WIDTH,0,'Houses',fnt_Outline,taCenter);
      for I:=1 to GUI_HOUSE_COUNT do
        if GUIHouseOrder[I] <> ht_None then begin
          Button_Build[I]:=TKMButtonFlat.Create(Panel_Build, ((I-1) mod 5)*37,83+((I-1) div 5)*37,33,33,fResource.HouseDat[GUIHouseOrder[I]].GUIIcon);
          Button_Build[I].OnClick:=Build_ButtonClick;
          Button_Build[I].Hint := fResource.HouseDat[GUIHouseOrder[I]].HouseName;
        end;

    //Units placement
    Panel_Units := TKMPanel.Create(Panel_Village,0,28,TB_WIDTH,400);

      for I:=0 to High(Button_Citizen) do
      begin
        Button_Citizen[I] := TKMButtonFlat.Create(Panel_Units,(I mod 5)*37,8+(I div 5)*37,33,33,fResource.UnitDat[School_Order[I]].GUIIcon); //List of tiles 5x5
        Button_Citizen[I].Hint := fResource.UnitDat[School_Order[I]].UnitName;
        Button_Citizen[I].Tag := byte(School_Order[I]); //Returns unit ID
        Button_Citizen[I].OnClick := Unit_ButtonClick;
      end;
      Button_UnitCancel := TKMButtonFlat.Create(Panel_Units,((High(Button_Citizen)+1) mod 5)*37,8+(length(Button_Citizen) div 5)*37,33,33,340);
      Button_UnitCancel.Hint := fTextLibrary[TX_BUILD_CANCEL_HINT];
      Button_UnitCancel.OnClick := Unit_ButtonClick;

      for I:=0 to High(Button_Warriors) do
      begin
        Button_Warriors[I] := TKMButtonFlat.Create(Panel_Units,(I mod 5)*37,124+(I div 5)*37,33,33, MapEd_Icon[I], rxGui);
        Button_Warriors[I].Hint := fResource.UnitDat[MapEd_Order[I]].UnitName;
        Button_Warriors[I].Tag := byte(MapEd_Order[I]); //Returns unit ID
        Button_Warriors[I].OnClick := Unit_ButtonClick;
      end;

      for I:=0 to High(Button_Animals) do
      begin
        Button_Animals[I] := TKMButtonFlat.Create(Panel_Units,(I mod 5)*37,240+(I div 5)*37,33,33, Animal_Icon[I], rxGui);
        Button_Animals[I].Hint := fResource.UnitDat[Animal_Order[I]].UnitName;
        Button_Animals[I].Tag := byte(Animal_Order[I]); //Returns animal ID
        Button_Animals[I].OnClick := Unit_ButtonClick;
      end;
      Unit_ButtonClick(Button_Citizen[0]); //Select serf as default

    //Town settings
    Panel_Script := TKMPanel.Create(Panel_Village, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_Script, 0, 10, TB_WIDTH, 0, 'Scripts', fnt_Outline, taCenter);
      CheckBox_AutoBuild := TKMCheckBox.Create(Panel_Script, 0, 30, TB_WIDTH, 20, 'Autobuild', fnt_Metal);
      CheckBox_AutoBuild.OnClick := Village_ScriptChange;
      CheckBox_AutoRepair := TKMCheckBox.Create(Panel_Script, 0, 50, TB_WIDTH, 20, 'Autorepair', fnt_Metal);
      CheckBox_AutoRepair.OnClick := Village_ScriptChange;
      TrackBar_SerfFactor := TKMTrackBar.Create(Panel_Script, 0, 70, TB_WIDTH, 1, 20);
      TrackBar_SerfFactor.Caption := 'Serf factor';
      TrackBar_SerfFactor.OnClick := Village_ScriptChange;
      TrackBar_WorkerFactor := TKMTrackBar.Create(Panel_Script, 0, 110, TB_WIDTH, 3, 30);
      TrackBar_WorkerFactor.Caption := 'Workers';
      TrackBar_WorkerFactor.OnClick := Village_ScriptChange;

    //Defence settings
    Panel_Defence := TKMPanel.Create(Panel_Village, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_Defence, 0, 5, TB_WIDTH, 0, 'Defence', fnt_Outline, taCenter);

      CheckBox_AutoDefence := TKMCheckBox.Create(Panel_Defence, 0, 30, TB_WIDTH, 20, 'AutoDefence', fnt_Metal);
      CheckBox_AutoDefence.OnClick := Defence_Change;

      TrackBar_EquipRateLeather := TKMTrackBar.Create(Panel_Defence, 0, 50, TB_WIDTH, 10, 300);
      TrackBar_EquipRateLeather.Caption := 'Equip rate iron';
      TrackBar_EquipRateLeather.Step := 5;
      TrackBar_EquipRateLeather.OnClick := Defence_Change;

      TrackBar_EquipRateIron := TKMTrackBar.Create(Panel_Defence, 0, 90, TB_WIDTH, 10, 300);
      TrackBar_EquipRateIron.Caption := 'Equip rate leather';
      TrackBar_EquipRateIron.Step := 5;
      TrackBar_EquipRateIron.OnClick := Defence_Change;

      TrackBar_RecruitFactor := TKMTrackBar.Create(Panel_Defence, 0, 130, TB_WIDTH, 1, 20);
      TrackBar_RecruitFactor.Caption := 'Recruits per Barracks';
      TrackBar_RecruitFactor.Hint := 'How many recruits AI should have in barracks'; //@Lewin: Please check me on this one
      TrackBar_RecruitFactor.OnClick := Defence_Change;

      List_Defences := TKMListBox.Create(Panel_Defence, 0, 170, TB_WIDTH, 160, fnt_Grey, bsGame);
      List_Defences.OnDoubleClick := Defence_ListClick;

      Button_EditFormations := TKMButton.Create(Panel_Defence, 0, 340, TB_WIDTH, 25, 'Edit formations', bsGame);
      Button_EditFormations.OnClick := Formations_Show;
end;


procedure TKMapEdInterface.Create_Player_Page;
var I: Integer; Col: array [0..255] of TColor4;
begin
  Panel_Player := TKMPanel.Create(Panel_Common,0,60, TB_WIDTH,28);
    Button_Player[1] := TKMButton.Create(Panel_Player, SMALL_PAD_W * 0, 0, SMALL_TAB_W, SMALL_TAB_H,  41, rxGui, bsGame);
    Button_Player[2] := TKMButton.Create(Panel_Player, SMALL_PAD_W * 1, 0, SMALL_TAB_W, SMALL_TAB_H, 382, rxGui, bsGame);
    Button_Player[3] := TKMButton.Create(Panel_Player, SMALL_PAD_W * 2, 0, SMALL_TAB_W, SMALL_TAB_H,  38, rxGui, bsGame);
    Button_Player[4] := TKMButton.Create(Panel_Player, SMALL_PAD_W * 3, 0, SMALL_TAB_W, SMALL_TAB_H, 393, rxGui, bsGame);
    for I := 1 to 4 do Button_Player[I].OnClick := SwitchPage;

    Panel_Goals := TKMPanel.Create(Panel_Player,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Goals,0,10,TB_WIDTH,0,'Goals',fnt_Outline,taCenter);

    //Players color
    Panel_Color := TKMPanel.Create(Panel_Player, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_Color, 0, 10, TB_WIDTH, 0, 'Colors', fnt_Outline, taCenter);
      TKMBevel.Create(Panel_Color, 0, 30, TB_WIDTH, 210);
      ColorSwatch_Color := TKMColorSwatch.Create(Panel_Color, 0, 32, 16, 16, 11);
      for I := 0 to 255 do Col[I] := fResource.Palettes.DefDal.Color32(I);
      ColorSwatch_Color.SetColors(Col);
      ColorSwatch_Color.OnClick := Player_ColorClick;

    //Allow/Block house building
    Panel_Block := TKMPanel.Create(Panel_Player, 0, 28, TB_WIDTH, 400);
      TKMLabel.Create(Panel_Block, 0, 10, TB_WIDTH, 0, 'Block/Release houses', fnt_Outline, taCenter);
      for I := 1 to GUI_HOUSE_COUNT do
      if GUIHouseOrder[I] <> ht_None then begin
        Button_BlockHouse[I] := TKMButtonFlat.Create(Panel_Block, ((I-1) mod 5)*37, 30 + ((I-1) div 5)*37,33,33,fResource.HouseDat[GUIHouseOrder[I]].GUIIcon);
        Button_BlockHouse[I].Hint := fResource.HouseDat[GUIHouseOrder[I]].HouseName;
        Button_BlockHouse[I].OnClick := Player_BlockClick;
        Button_BlockHouse[I].Tag := I;
        Image_BlockHouse[I] := TKMImage.Create(Panel_Block, ((I-1) mod 5)*37 + 13, 30 + ((I-1) div 5)*37 + 13, 16, 16, 0, rxGuiMain);
        Image_BlockHouse[I].Hitable := False;
        Image_BlockHouse[I].ImageCenter;
      end;

    //FOW settings
    Panel_RevealFOW := TKMPanel.Create(Panel_Player,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_RevealFOW, 0, 10, TB_WIDTH, 0, 'Reveal fog', fnt_Outline, taCenter);
      Button_Reveal         := TKMButtonFlat.Create(Panel_RevealFOW, 0, 30, 33, 33, 335);
      Button_Reveal.OnClick := Player_RevealClick;
      Button_Reveal.Hint    := 'Reveal a portion of map';
      TrackBar_RevealNewSize  := TKMTrackBar.Create(Panel_RevealFOW, 37, 35, 140, 1, 50);
      CheckBox_RevealAll      := TKMCheckBox.Create(Panel_RevealFOW, 0, 75, 140, 20, 'Reveal all', fnt_Metal);
      CheckBox_RevealAll.Enabled := False;
end;


procedure TKMapEdInterface.Create_Mission_Page;
var i,k:Integer;
begin
  Panel_Mission := TKMPanel.Create(Panel_Common,0,60,TB_WIDTH,28);
    Button_Mission[1] := TKMButton.Create(Panel_Mission, SMALL_PAD_W * 0, 0, SMALL_TAB_W, SMALL_TAB_H, 41, rxGui, bsGame);
    Button_Mission[2] := TKMButton.Create(Panel_Mission, SMALL_PAD_W * 1, 0, SMALL_TAB_W, SMALL_TAB_H, 41, rxGui, bsGame);
    for i:=1 to 2 do Button_Mission[i].OnClick := SwitchPage;

    Panel_Alliances := TKMPanel.Create(Panel_Mission,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_Alliances,0,10,TB_WIDTH,0,'Alliances',fnt_Outline,taCenter);
      for i:=0 to MAX_PLAYERS-1 do begin
        TKMLabel.Create(Panel_Alliances,24+i*20+2,30,20,20,inttostr(i+1),fnt_Outline,taLeft);
        TKMLabel.Create(Panel_Alliances,4,50+i*25,20,20,inttostr(i+1),fnt_Outline,taLeft);
        for k:=0 to MAX_PLAYERS-1 do begin
          CheckBox_Alliances[i,k] := TKMCheckBox.Create(Panel_Alliances, 20+k*20, 46+i*25, 20, 20, '', fnt_Metal);
          CheckBox_Alliances[i,k].Tag       := i * MAX_PLAYERS + k;
          CheckBox_Alliances[i,k].FlatStyle := true;
          CheckBox_Alliances[i,k].OnClick   := Mission_AlliancesChange;
        end;
      end;

      //It does not have OnClick event for a reason:
      // - we don't have a rule to make alliances symmetrical yet
      CheckBox_AlliancesSym := TKMCheckBox.Create(Panel_Alliances, 0, 50+MAX_PLAYERS*25, TB_WIDTH, 20, 'Symmetrical', fnt_Metal);
      CheckBox_AlliancesSym.Checked := true;
      CheckBox_AlliancesSym.Disable;

    Panel_PlayerTypes := TKMPanel.Create(Panel_Mission,0,28,TB_WIDTH,400);
      TKMLabel.Create(Panel_PlayerTypes,0,10,TB_WIDTH,0,'Player types',fnt_Outline,taCenter);
      for i:=0 to MAX_PLAYERS-1 do begin
        TKMLabel.Create(Panel_PlayerTypes,4,30,20,20,'#',fnt_Grey,taLeft);
        TKMLabel.Create(Panel_PlayerTypes,24,30,100,20,'Human',fnt_Grey,taLeft);
        TKMLabel.Create(Panel_PlayerTypes,94,30,100,20,'Computer',fnt_Grey,taLeft);
        TKMLabel.Create(Panel_PlayerTypes,4,50+i*25,20,20,inttostr(i+1),fnt_Outline,taLeft);
        for k:=0 to 1 do
        begin
          CheckBox_PlayerTypes[i,k] := TKMCheckBox.Create(Panel_PlayerTypes, 44+k*70, 48+i*25, 20, 20, '', fnt_Metal);
          CheckBox_PlayerTypes[i,k].Tag       := i;
          CheckBox_PlayerTypes[i,k].FlatStyle := true;
          CheckBox_PlayerTypes[i,k].OnClick   := Mission_PlayerTypesChange;
        end;
      end;
end;


{Menu page}
procedure TKMapEdInterface.Create_Menu;
begin
  Panel_Menu := TKMPanel.Create(Panel_Common, 0, 128, TB_WIDTH, 400);
    Button_Menu_Save := TKMButton.Create(Panel_Menu, 0, 20, TB_WIDTH, 30, fTextLibrary[TX_MENU_SAVE_GAME], bsGame);
    Button_Menu_Save.OnClick := SwitchPage;
    Button_Menu_Save.Hint := fTextLibrary[TX_MENU_SAVE_GAME];
    Button_Menu_Load := TKMButton.Create(Panel_Menu, 0, 60, TB_WIDTH, 30, fTextLibrary[TX_MENU_LOAD_GAME], bsGame);
    Button_Menu_Load.OnClick := SwitchPage;
    Button_Menu_Load.Hint := fTextLibrary[TX_MENU_LOAD_GAME];
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
  Panel_Save := TKMPanel.Create(Panel_Common,0,128,TB_WIDTH,400);
    TKMBevel.Create(Panel_Save, 0, 30, TB_WIDTH, 37);
    Radio_Save_MapType  := TKMRadioGroup.Create(Panel_Save,4,32,TB_WIDTH,35,fnt_Grey);
    Radio_Save_MapType.ItemIndex := 0;
    Radio_Save_MapType.Items.Add(fTextLibrary[TX_MENU_MAPED_SPMAPS]);
    Radio_Save_MapType.Items.Add(fTextLibrary[TX_MENU_MAPED_MPMAPS]);
    Radio_Save_MapType.OnChange := Menu_Save;
    TKMLabel.Create(Panel_Save,0,90,TB_WIDTH,20,'Save map',fnt_Outline,taCenter);
    Edit_SaveName       := TKMEdit.Create(Panel_Save,0,110,TB_WIDTH,20, fnt_Grey);
    Edit_SaveName.AllowedChars := acFileName;
    Label_SaveExists    := TKMLabel.Create(Panel_Save,0,140,TB_WIDTH,0,'Map already exists',fnt_Outline,taCenter);
    CheckBox_SaveExists := TKMCheckBox.Create(Panel_Save,0,160,TB_WIDTH,20,'Overwrite', fnt_Metal);
    Button_SaveSave     := TKMButton.Create(Panel_Save,0,180,TB_WIDTH,30,'Save',bsGame);
    Button_SaveCancel   := TKMButton.Create(Panel_Save,0,220,TB_WIDTH,30,'Cancel',bsGame);
    Edit_SaveName.OnChange      := Menu_Save;
    CheckBox_SaveExists.OnClick := Menu_Save;
    Button_SaveSave.OnClick     := Menu_Save;
    Button_SaveCancel.OnClick   := SwitchPage;
end;


{Load page}
procedure TKMapEdInterface.Create_MenuLoad;
begin
  Panel_Load := TKMPanel.Create(Panel_Common,0,108,TB_WIDTH,400);
    TKMLabel.Create(Panel_Load, 0, 2, TB_WIDTH, 30, 'Available maps', fnt_Outline, taLeft);
    TKMBevel.Create(Panel_Load, 0, 20, TB_WIDTH, 38);
    Radio_Load_MapType := TKMRadioGroup.Create(Panel_Load,0,22,TB_WIDTH,35,fnt_Grey);
    Radio_Load_MapType.ItemIndex := 0;
    Radio_Load_MapType.Items.Add(fTextLibrary[TX_MENU_MAPED_SPMAPS]);
    Radio_Load_MapType.Items.Add(fTextLibrary[TX_MENU_MAPED_MPMAPS]);
    Radio_Load_MapType.OnChange := Load_MapTypeChange;
    ListBox_Load := TKMListBox.Create(Panel_Load, 0, 75, TB_WIDTH, 205, fnt_Grey, bsGame);
    ListBox_Load.ItemHeight := 18;
    Button_LoadLoad     := TKMButton.Create(Panel_Load,0,290,TB_WIDTH,30,'Load',bsGame);
    Button_LoadCancel   := TKMButton.Create(Panel_Load,0,325,TB_WIDTH,30,'Cancel',bsGame);
    Button_LoadLoad.OnClick     := Menu_Load;
    Button_LoadCancel.OnClick   := SwitchPage;
end;


{Quit page}
procedure TKMapEdInterface.Create_MenuQuit;
begin
  Panel_Quit:=TKMPanel.Create(Panel_Common,0,128,TB_WIDTH,400);
    TKMLabel.Create(Panel_Quit,0,40,TB_WIDTH,60,'Any unsaved|changes will be lost',fnt_Outline,taCenter);
    Button_Quit_Yes   := TKMButton.Create(Panel_Quit,0,100,TB_WIDTH,30,fTextLibrary[TX_MENU_QUIT_MISSION],bsGame);
    Button_Quit_No    := TKMButton.Create(Panel_Quit,0,140,TB_WIDTH,30,fTextLibrary[TX_MENU_DONT_QUIT_MISSION],bsGame);
    Button_Quit_Yes.Hint      := fTextLibrary[TX_MENU_QUIT_MISSION];
    Button_Quit_No.Hint       := fTextLibrary[TX_MENU_DONT_QUIT_MISSION];
    Button_Quit_Yes.OnClick   := Menu_QuitMission;
    Button_Quit_No.OnClick    := SwitchPage;
end;


procedure TKMapEdInterface.Create_Extra_Page;
begin
  Panel_Extra := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - 190, Panel_Main.Width - TOOLBAR_WIDTH, 190);
  Panel_Extra.Anchors := [akLeft, akBottom];
  Panel_Extra.Hide;

    with TKMImage.Create(Panel_Extra, 0, 0, 800, 190, 409) do
    begin
      Anchors := [akLeft,akTop,akBottom];
      ImageStretch;
    end;

    Image_ExtraClose := TKMImage.Create(Panel_Extra, 800-35, 20, 32, 32, 52);
    Image_ExtraClose.Anchors := [akTop, akRight];
    Image_ExtraClose.Hint := fTextLibrary[TX_MSG_CLOSE_HINT];
    Image_ExtraClose.OnClick := SwitchPage;
    Image_ExtraClose.HighlightOnMouseOver := True;

    TrackBar_Passability := TKMTrackBar.Create(Panel_Extra, 18, 50, 184, 0, Byte(High(TPassability)));
    TrackBar_Passability.Caption := 'View passability';
    TrackBar_Passability.Position := 0; //Disabled by default
    TrackBar_Passability.OnChange := View_Passability;
    Label_Passability := TKMLabel.Create(Panel_Extra, 18, 90, 184, 0, 'Off', fnt_Metal, taLeft);
end;


procedure TKMapEdInterface.Create_FormationsPopUp;
const
  T: array [TGroupType] of string = ('Melee', 'AntiHorse', 'Ranged', 'Mounted');
  SIZE_X = 570;
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
    TKMLabel.Create(Panel_Formations, SIZE_X div 2, 10, 'Troop Formations', fnt_Outline, taCenter);

    Image_FormationsFlag := TKMImage.Create(Panel_Formations, 10, 10, 0, 0, 30, rxGuiMain);

    TKMLabel.Create(Panel_Formations, 20, 70, 80, 0, 'Count', fnt_Metal, taLeft);
    TKMLabel.Create(Panel_Formations, 20, 95, 80, 0, 'Columns', fnt_Metal, taLeft);

    for GT := Low(TGroupType) to High(TGroupType) do
    begin
      TKMLabel.Create(Panel_Formations, 130 + Byte(GT) * 110 + 32, 50, 0, 0, T[GT], fnt_Metal, taCenter);
      NumEdit_FormationsCount[GT] := TKMNumericEdit.Create(Panel_Formations, 130 + Byte(GT) * 110, 70);
      NumEdit_FormationsCount[GT].ValueMin := 1;
      NumEdit_FormationsCount[GT].ValueMax := 255;
      NumEdit_FormationsColumns[GT] := TKMNumericEdit.Create(Panel_Formations, 130 + Byte(GT) * 110, 95);
      NumEdit_FormationsColumns[GT].ValueMin := 1;
      NumEdit_FormationsColumns[GT].ValueMax := 255;
    end;

    Button_Formations_Ok := TKMButton.Create(Panel_Formations, SIZE_X-20-320-10, 150, 160, 30, 'Ok', bsMenu);
    Button_Formations_Ok.OnClick := Formations_Close;
    Button_Formations_Cancel := TKMButton.Create(Panel_Formations, SIZE_X-20-160, 150, 160, 30, 'Cancel', bsMenu);
    Button_Formations_Cancel.OnClick := Formations_Close;
end;


{Unit page}
procedure TKMapEdInterface.Create_Unit;
begin
  Panel_Unit:=TKMPanel.Create(Panel_Common,0,112,TB_WIDTH,400);
    Label_UnitName        := TKMLabel.Create(Panel_Unit,0,16,TB_WIDTH,0,'',fnt_Outline,taCenter);
    Image_UnitPic         := TKMImage.Create(Panel_Unit,0,38,54,100,521);
    Label_UnitCondition   := TKMLabel.Create(Panel_Unit,65,40,116,0,fTextLibrary[TX_UNIT_CONDITION],fnt_Grey,taCenter);
    KMConditionBar_Unit   := TKMPercentBar.Create(Panel_Unit,65,55,116,15);
    Label_UnitDescription := TKMLabel.Create(Panel_Unit,0,152,TB_WIDTH,200,'',fnt_Grey,taLeft); //Taken from LIB resource
    Label_UnitDescription.AutoWrap := True;

    Panel_Army := TKMPanel.Create(Panel_Unit, 0, 160, TB_WIDTH, 400);
    Button_Army_RotCCW  := TKMButton.Create(Panel_Army,       0, 0, 56, 40, 23, rxGui, bsGame);
    Button_Army_RotCW   := TKMButton.Create(Panel_Army,     124, 0, 56, 40, 24, rxGui, bsGame);
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
    Button_ArmyDec.OnClickEither := Unit_ArmyChange2;
    Button_ArmyFood.OnClick := Unit_ArmyChange1;
    Button_ArmyInc.OnClickEither := Unit_ArmyChange2;

    //todo: add Group order (see MapEdOrder field)
end;


{House description page}
procedure TKMapEdInterface.Create_House;
var
  I: Integer;
begin
  Panel_House := TKMPanel.Create(Panel_Common, 0, 44, TB_WIDTH, 400);
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
      Button_Store[I].TexID := fResource.Resources[StoreResType[I]].GUIIcon;
      Button_Store[I].Tag := I;
      Button_Store[I].Hint := fResource.Resources[StoreResType[I]].Title;
      Button_Store[I].OnClick := Store_SelectWare;
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
    Button_StoreDec100.OnClickEither := Store_EditWareCount;
    Button_StoreDec.OnClickEither    := Store_EditWareCount;
    Button_StoreInc100.OnClickEither := Store_EditWareCount;
    Button_StoreInc.OnClickEither    := Store_EditWareCount;
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
      Button_Barracks[i].TexID := fResource.Resources[BarracksResType[i]].GUIIcon;
      Button_Barracks[i].TexOffsetX := 1;
      Button_Barracks[i].TexOffsetY := 1;
      Button_Barracks[i].CapOffsetY := 2;
      Button_Barracks[i].Hint := fResource.Resources[BarracksResType[i]].Title;
      Button_Barracks[i].OnClick := Barracks_SelectWare;
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
    Button_BarracksDec100.OnClickEither := Barracks_EditWareCount;
    Button_BarracksDec.OnClickEither    := Barracks_EditWareCount;
    Button_BarracksInc100.OnClickEither := Barracks_EditWareCount;
    Button_BarracksInc.OnClickEither    := Barracks_EditWareCount;
end;


procedure TKMapEdInterface.Create_Markers_Page;
begin
  Panel_Marker := TKMPanel.Create(Panel_Common, 0, 50, TB_WIDTH, 400);

  Label_MarkerType := TKMLabel.Create(Panel_Marker, 0, 10, TB_WIDTH, 0, '', fnt_Outline, taCenter);
  Image_MarkerPic := TKMImage.Create(Panel_Marker, 0, 30, 32, 32, 338);

    Panel_MarkerReveal := TKMPanel.Create(Panel_Marker, 0, 60, TB_WIDTH, 400);
      TrackBar_RevealSize := TKMTrackBar.Create(Panel_MarkerReveal, 0, 10, TB_WIDTH, 1, 128);
      TrackBar_RevealSize.Caption := 'Area';
      TrackBar_RevealSize.OnChange := Marker_Change;

    Panel_MarkerDefence := TKMPanel.Create(Panel_Marker, 0, 60, TB_WIDTH, 400);
      DropList_DefenceGroup := TKMDropList.Create(Panel_MarkerDefence, 0, 10, TB_WIDTH, 20, fnt_Game, '', bsGame);
      DropList_DefenceGroup.SetItems('Melee'+eol+'AntiHorse'+eol+'Ranged'+eol+'Mounted');
      DropList_DefenceGroup.OnChange := Marker_Change;
      DropList_DefenceType := TKMDropList.Create(Panel_MarkerDefence, 0, 40, TB_WIDTH, 20, fnt_Game, '', bsGame);
      DropList_DefenceType.SetItems('FrontLine'+eol+'BackLine');
      DropList_DefenceType.OnChange := Marker_Change;
      TrackBar_DefenceRad := TKMTrackBar.Create(Panel_MarkerDefence, 0, 70, TB_WIDTH, 1, 128);
      TrackBar_DefenceRad.Caption := 'Radius';
      TrackBar_DefenceRad.OnChange := Marker_Change;
      Button_DefenceCCW  := TKMButton.Create(Panel_MarkerDefence, 0, 120, 50, 35, 23, rxGui, bsGame);
      Button_DefenceCCW.OnClick := Marker_Change;
      Button_DefenceCW := TKMButton.Create(Panel_MarkerDefence, 130, 120, 50, 35, 24, rxGui, bsGame);
      Button_DefenceCW.OnClick := Marker_Change;
end;


//Should update any items changed by game (resource counts, hp, etc..)
procedure TKMapEdInterface.UpdateState(aTickCount: Cardinal);
begin
  //
end;


procedure TKMapEdInterface.Paint;
const
  DefColor: array [TAIDefencePosType] of TColor4 = ($FF000080, $FF008000);
var
  I, K: Integer;
  MapLoc: TKMPointF;
  ScreenLoc: TKMPointI;
  R: TRawDeposit;
  Depo: TKMDeposits;
begin
  if mlDeposits in fGame.MapEditor.VisibleLayers then
  begin
    Label_MatAmount.Show; //Only make it visible while we need it
    Shape_MatAmount.Show;
    Depo := fGame.MapEditor.Deposits;
    for R := Low(TRawDeposit) to High(TRawDeposit) do
      for I := 0 to Depo.Count[R] - 1 do
      //Ignore water areas with 0 fish in them
      if Depo.Amount[R, I] > 0 then
      begin
        Label_MatAmount.Caption := IntToStr(Depo.Amount[R, I]);

        MapLoc := fTerrain.FlatToHeight(Depo.Location[R, I]);
        ScreenLoc := fGame.Viewport.MapToScreen(MapLoc);

        //At extreme zoom coords may become out of range of SmallInt used in controls painting
        if KMInRect(ScreenLoc, KMRect(0, 0, Panel_Main.Width, Panel_Main.Height)) then
        begin
          //Paint the background
          Shape_MatAmount.Width := 10 + 10 * Length(Label_MatAmount.Caption);
          Shape_MatAmount.Left := ScreenLoc.X - Shape_MatAmount.Width div 2;
          Shape_MatAmount.Top := ScreenLoc.Y - 10;
          Shape_MatAmount.LineColor := DEPOSIT_COLORS[R];
          Shape_MatAmount.Paint;
          //Paint the label on top of the background
          Label_MatAmount.Left := ScreenLoc.X;
          Label_MatAmount.Top := ScreenLoc.Y - 7;
          Label_MatAmount.Paint;
        end;
      end;
    Label_MatAmount.Hide; //Only make it visible while we need it
    Shape_MatAmount.Hide;
  end;

  if mlDefences in fGame.MapEditor.VisibleLayers then
  begin
    //Only make it visible while we need it
    Label_DefenceID.Show;
    Label_DefencePos.Show;
    Shape_DefencePos.Show;
    for I := 0 to fPlayers.Count - 1 do
      for K := 0 to fPlayers[I].AI.General.DefencePositions.Count - 1 do
      begin
        Label_DefenceID.Caption := IntToStr(K);
        Label_DefencePos.Caption := fPlayers[I].AI.General.DefencePositions[K].UITitle;

        MapLoc := fTerrain.FlatToHeight(KMPointF(fPlayers[I].AI.General.DefencePositions[K].Position.Loc));
        ScreenLoc := fGame.Viewport.MapToScreen(MapLoc);

        if KMInRect(ScreenLoc, KMRect(0, 0, Panel_Main.Width, Panel_Main.Height)) then
        begin
          //Paint the background
          Shape_DefencePos.Width := 10 + 10 * Length(Label_DefencePos.Caption);
          Shape_DefencePos.Left := ScreenLoc.X - Shape_DefencePos.Width div 2;
          Shape_DefencePos.Top := ScreenLoc.Y - 10;
          Shape_DefencePos.Paint;
          //Paint the label on top of the background
          Label_DefenceID.Left := ScreenLoc.X;
          Label_DefenceID.Top := ScreenLoc.Y - 22;
          Label_DefenceID.Paint;
          Label_DefencePos.Left := ScreenLoc.X;
          Label_DefencePos.Top := ScreenLoc.Y - 7;
          Label_DefencePos.Paint;
        end;
      end;
    //Only make it visible while we need it
    Label_DefenceID.Hide;
    Label_DefencePos.Hide;
    Shape_DefencePos.Hide;
  end;

  inherited;
end;


procedure TKMapEdInterface.SetMinimap;
begin
  MinimapView.SetMinimap(fGame.Minimap);
  MinimapView.SetViewport(fGame.Viewport);
end;


procedure TKMapEdInterface.SetMapName(const aName: string);
begin
  Label_MissionName.Caption := aName;
end;


procedure TKMapEdInterface.Defence_Refresh;
var
  I: Integer;
begin
  CheckBox_AutoDefence.Checked := MyPlayer.AI.Setup.AutoDefend;
  TrackBar_EquipRateLeather.Position := MyPlayer.AI.Setup.EquipRateLeather div 10;
  TrackBar_EquipRateIron.Position := MyPlayer.AI.Setup.EquipRateIron div 10;
  TrackBar_RecruitFactor.Position := MyPlayer.AI.Setup.RecruitFactor;

  List_Defences.Clear;

  with MyPlayer.AI.General.DefencePositions do
  for I := 0 to Count - 1 do
    List_Defences.Add(Positions[I].UITitle);
end;


procedure TKMapEdInterface.Defence_Change(Sender: TObject);
begin
  MyPlayer.AI.Setup.AutoDefend := CheckBox_AutoDefence.Checked;
  MyPlayer.AI.Setup.EquipRateLeather := TrackBar_EquipRateLeather.Position * 10;
  MyPlayer.AI.Setup.EquipRateIron := TrackBar_EquipRateIron.Position * 10;
  MyPlayer.AI.Setup.RecruitFactor := TrackBar_RecruitFactor.Position;
end;


procedure TKMapEdInterface.Defence_ListClick(Sender: TObject);
var
  I: Integer;
begin
  I := List_Defences.ItemIndex;
  if I = -1 then Exit;

  fGame.Viewport.Position := KMPointF(MyPlayer.AI.General.DefencePositions[I].Position.Loc);
end;


procedure TKMapEdInterface.Village_ScriptRefresh;
begin
  CheckBox_AutoBuild.Checked := MyPlayer.AI.Setup.AutoBuild;
  CheckBox_AutoRepair.Checked := MyPlayer.AI.Setup.AutoRepair;
  TrackBar_SerfFactor.Position := MyPlayer.AI.Setup.SerfFactor;
  TrackBar_WorkerFactor.Position := MyPlayer.AI.Setup.WorkerFactor;
end;


procedure TKMapEdInterface.Village_ScriptChange(Sender: TObject);
begin
  MyPlayer.AI.Setup.AutoBuild := CheckBox_AutoBuild.Checked;
  MyPlayer.AI.Setup.AutoRepair := CheckBox_AutoRepair.Checked;
  MyPlayer.AI.Setup.SerfFactor := TrackBar_SerfFactor.Position;
  MyPlayer.AI.Setup.WorkerFactor := TrackBar_WorkerFactor.Position;
end;


procedure TKMapEdInterface.Player_UpdateColors;
var I: Integer;
begin
  //Set player colors
  for I := 0 to MAX_PLAYERS - 1 do
    Button_PlayerSelect[I].ShapeColor := fPlayers[I].FlagColor;

  Button_Village[vtUnits].FlagColor := MyPlayer.FlagColor;
  for I := Low(Button_Citizen) to High(Button_Citizen) do
    Button_Citizen[I].FlagColor := MyPlayer.FlagColor;
  for I := Low(Button_Warriors) to High(Button_Warriors) do
    Button_Warriors[I].FlagColor := MyPlayer.FlagColor;
end;


procedure TKMapEdInterface.Player_ChangeActive(Sender: TObject);
begin
  //If we had selected House or Unit - discard them
  if Panel_House.Visible or Panel_Unit.Visible or Panel_Defence.Visible then
    SwitchPage(nil);

  fPlayers.Selected := nil;

  if Sender <> nil then
    SetActivePlayer(TKMControl(Sender).Tag)
  else
    SetActivePlayer(-1);

  //Refresh per-player settings
  Village_ScriptRefresh;
  Player_BlockRefresh;
end;


procedure TKMapEdInterface.SetActivePlayer(aIndex: TPlayerIndex);
var I: Integer;
begin
  if aIndex <> -1 then
    MyPlayer := fPlayers[aIndex]
  else
    MyPlayer := fPlayers[0];

  for I := 0 to MAX_PLAYERS - 1 do
    Button_PlayerSelect[I].Down := (I = MyPlayer.PlayerIndex);

  Player_UpdateColors;
end;


procedure TKMapEdInterface.Terrain_HeightChange(Sender: TObject);
begin
  GameCursor.MapEdSize := HeightSize.Position;
  GameCursor.MapEdSlope := HeightSlope.Position;
  GameCursor.MapEdSpeed := HeightSpeed.Position;

  if Sender = HeightCircle then
  begin
    HeightCircle.Down := true;
    HeightSquare.Down := false;
    GameCursor.MapEdShape := hsCircle;
  end else
  if Sender = HeightSquare then
  begin
    HeightSquare.Down := true;
    HeightCircle.Down := false;
    GameCursor.MapEdShape := hsSquare;
  end else
  if Sender = HeightElevate then
  begin
    HeightElevate.Down := True;
    HeightUnequalize.Down:=false;
    GameCursor.Mode := cmElevate;
  end;
  if Sender = HeightUnequalize then
  begin
    HeightElevate.Down  := false;
    HeightUnequalize.Down := true;
    GameCursor.Mode := cmEqualize;
  end;
end;


procedure TKMapEdInterface.Terrain_TilesChange(Sender: TObject);

  function GetTileIDFromTag(aTag: byte):byte;
  var Tile:byte;
  begin
    Tile := 32*((aTag-1) div MAPED_TILES_COLS) + (aTag-1) mod MAPED_TILES_COLS + TilesScroll.Position;
    Result := MapEdTileRemap[EnsureRange(Tile+1,1,256)];
  end;

var i,k,TileID:Integer;
begin
  if Sender = TilesRandom then
    GameCursor.MapEdDir := 4 * byte(TilesRandom.Checked); //Defined=0..3 or Random=4

  if Sender = TilesScroll then //Shift tiles
    for i:=1 to MAPED_TILES_COLS do
    for k:=1 to MAPED_TILES_ROWS do
    begin
      if GetTileIDFromTag((k-1)*MAPED_TILES_COLS+i) <> 0 then
      begin
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].TexID := GetTileIDFromTag((k-1)*MAPED_TILES_COLS+i); //icons are in 2..9
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].Enable;
      end
      else
      begin
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].TexID := 0;
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].Disable;
      end;
      if GameCursor.Mode = cmTiles then
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].Down := (GameCursor.Tag1+1 = GetTileIDFromTag((k-1)*MAPED_TILES_COLS+i));
    end;
  if Sender is TKMButtonFlat then
  begin
    TileID := GetTileIDFromTag(TKMButtonFlat(Sender).Tag);
    if TileID <> 0 then
    begin
      GameCursor.Mode := cmTiles;
      GameCursor.Tag1 := TileID-1; //MapEdTileRemap is 1 based, tag is 0 based
      if TilesRandom.Checked then
        GameCursor.MapEdDir := 4;
      for i:=1 to MAPED_TILES_COLS do
      for k:=1 to MAPED_TILES_ROWS do
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].Down := (Sender = TilesTable[(i-1)*MAPED_TILES_ROWS+k]);
    end;
  end;
end;


procedure TKMapEdInterface.Terrain_ObjectsChange(Sender: TObject);
var I, ObjID: Integer;
begin
  for I := 0 to 8 do
    ObjectsTable[i].Down := False;

  ObjectErase.Down := False;

  if Sender = ObjectsScroll then
  begin
    for I := 0 to 8 do
    begin
      ObjID := ObjectsScroll.Position * 3 + I;
      if ObjID < fResource.MapElements.ValidCount then
      begin
        ObjectsTable[I].TexID := MapElem[fResource.MapElements.ValidToObject[ObjID]].Anim.Step[1] + 1;
        ObjectsTable[I].Caption := IntToStr(ObjID);
        ObjectsTable[I].Enable;
      end
      else
      begin
        ObjectsTable[I].TexID := 0;
        ObjectsTable[I].Caption := '';
        ObjectsTable[I].Disable;
      end;
      ObjectsTable[I].Down := ObjID = fResource.MapElements.ObjectToValid[GameCursor.Tag1]; //Mark the selected one using reverse lookup
    end;
    ObjectErase.Down := (GameCursor.Tag1 = 255); //or delete button
  end;

  if Sender is TKMButtonFlat then
  begin
    ObjID := ObjectsScroll.Position * 3 + TKMButtonFlat(Sender).Tag; //0..n-1

    if (not InRange(ObjID, 0, fResource.MapElements.ValidCount - 1))
    and not (TKMButtonFlat(Sender).Tag = 255) then
      Exit; //Don't let them click if it is out of range

    GameCursor.Mode := cmObjects;
    if TKMButtonFlat(Sender).Tag = 255 then
      GameCursor.Tag1 := 255 //erase object
    else
      GameCursor.Tag1 := fResource.MapElements.ValidToObject[ObjID]; //0..n-1
    for I := 0 to 8 do
      ObjectsTable[I].Down := (Sender = ObjectsTable[I]); //Mark the selected one
    ObjectErase.Down := (Sender = ObjectErase); //or delete button
  end;
end;


procedure TKMapEdInterface.Build_ButtonClick(Sender: TObject);
var I: Integer;
begin
  //Release all buttons
  for I := 1 to Panel_Build.ChildCount do
    if Panel_Build.Childs[I] is TKMButtonFlat then
      TKMButtonFlat(Panel_Build.Childs[I]).Down := False;

  //Press the button
  TKMButtonFlat(Sender).Down := True;

  //Reset cursor and see if it needs to be changed
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  if Button_BuildCancel.Down then
    GameCursor.Mode := cmErase
  else
  if Button_BuildRoad.Down then
    GameCursor.Mode := cmRoad
  else
  if Button_BuildField.Down then
    GameCursor.Mode := cmField
  else
  if Button_BuildWine.Down then
    GameCursor.Mode := cmWine
  else
  //if Button_BuildWall.Down then
  //  GameCursor.Mode:=cm_Wall;
  //else
  for I := 1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[I] <> ht_None then
  if Button_Build[I].Down then begin
     GameCursor.Mode := cmHouses;
     GameCursor.Tag1 := Byte(GUIHouseOrder[I]);
  end;
end;


procedure TKMapEdInterface.Unit_ButtonClick(Sender: TObject);
var I: Integer;
begin
  //Reset cursor and see if it needs to be changed
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  if Sender = nil then Exit;

  //Release all buttons
  for I := 1 to Panel_Units.ChildCount do
    if Panel_Units.Childs[I] is TKMButtonFlat then
      TKMButtonFlat(Panel_Units.Childs[I]).Down := False;

  //Press the Sender button
  TKMButtonFlat(Sender).Down := True;

  if Button_UnitCancel.Down then
    GameCursor.Mode := cmErase
  else
  if (TKMButtonFlat(Sender).Tag in [byte(UNIT_MIN)..byte(UNIT_MAX)]) then
  begin
    GameCursor.Mode := cmUnits;
    GameCursor.Tag1 := byte(TKMButtonFlat(Sender).Tag);
  end;
end;


procedure TKMapEdInterface.View_Passability(Sender: TObject);
begin
  SHOW_TERRAIN_WIRES := (TKMTrackBar(Sender).Position <> 0);
  SHOW_TERRAIN_PASS := TKMTrackBar(Sender).Position;

  if TKMTrackBar(Sender).Position <> 0 then
    Label_Passability.Caption := GetEnumName(TypeInfo(TPassability), SHOW_TERRAIN_PASS)
  else
    Label_Passability.Caption := 'Off';
end;


procedure TKMapEdInterface.ShowHouseInfo(Sender: TKMHouse);
var
  HouseDat: TKMHouseDatClass;
  I: Integer;
  Res: TResourceType;
begin
  if Sender = nil then
  begin
    SwitchPage(nil);
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
    if fResource.Resources[Res].IsValid then
    begin
      ResRow_Resource[I].TexID := fResource.Resources[Res].GUIIcon;
      ResRow_Resource[I].Caption := fResource.Resources[Res].Title;
      ResRow_Resource[I].Hint := fResource.Resources[Res].Title;
      ResRow_Resource[I].ResourceCount := Sender.CheckResIn(Res);
      ResRow_Resource[I].OrderCount := Sender.CheckResIn(Res);
      ResRow_Resource[I].Show;
      Label_House_Supply.Show;
    end
    else
      ResRow_Resource[I].Hide;
  end;

  case Sender.HouseType of
    ht_Store: begin
          Store_FillValues(nil);
          SwitchPage(Panel_HouseStore);
          Store_SelectWare(Button_Store[fStorehouseItem]); //Reselect the ware so the display is updated
        end;

    ht_Barracks: begin
          Barracks_FillValues(nil);
          Image_House_Worker.Enable; //In the barrack the recruit icon is always enabled
          SwitchPage(Panel_HouseBarracks);
          Barracks_SelectWare(Button_Barracks[fBarracksItem]); //Reselect the ware so the display is updated
        end;
    ht_TownHall:;
    else SwitchPage(Panel_House);
  end;
end;


procedure TKMapEdInterface.ShowUnitInfo(Sender: TKMUnit);
begin
  if Sender = nil then
  begin
    SwitchPage(nil);
    Exit;
  end;

  SetActivePlayer(Sender.Owner);

  SwitchPage(Panel_Unit);
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
    SwitchPage(nil);
    Exit;
  end;

  SetActivePlayer(Sender.Owner);

  SwitchPage(Panel_Unit);
  Label_UnitName.Caption := fResource.UnitDat[Sender.UnitType].UnitName;
  Image_UnitPic.TexID := fResource.UnitDat[Sender.UnitType].GUIScroll;
  Image_UnitPic.FlagColor := fPlayers[Sender.Owner].FlagColor;
  KMConditionBar_Unit.Position := Sender.Condition / UNIT_MAX_CONDITION;

  //Warrior specific
  Label_UnitDescription.Hide;
  ImageStack_Army.SetCount(Sender.MapEdCount, Sender.UnitsPerRow, Sender.UnitsPerRow div 2 + 1);
  Label_ArmyCount.Caption := IntToStr(Sender.MapEdCount);
  Panel_Army.Show;
end;


procedure TKMapEdInterface.ShowMarkerInfo(aMarker: TKMMapEdMarker);
begin
  fActiveMarker := aMarker;

  if (aMarker.MarkerType = mtNone) or (aMarker.Owner = PLAYER_NONE) or (aMarker.Index = -1) then
  begin
    SwitchPage(nil);
    Exit;
  end;

  SetActivePlayer(aMarker.Owner);
  Image_MarkerPic.FlagColor := fPlayers[aMarker.Owner].FlagColor;

  case aMarker.MarkerType of
    mtDefence:    begin
                    Label_MarkerType.Caption := 'Defence position';
                    DropList_DefenceGroup.ItemIndex := Byte(fPlayers[aMarker.Owner].AI.General.DefencePositions[aMarker.Index].GroupType);
                    DropList_DefenceType.ItemIndex := Byte(fPlayers[aMarker.Owner].AI.General.DefencePositions[aMarker.Index].DefenceType);
                    TrackBar_DefenceRad.Position := fPlayers[aMarker.Owner].AI.General.DefencePositions[aMarker.Index].Radius;
                    SwitchPage(Panel_MarkerDefence);
                  end;
    mtRevealFOW:  begin
                    Label_MarkerType.Caption := 'Reveal FOW';
                    TrackBar_RevealSize.Position := fGame.MapEditor.Revealers[aMarker.Owner].Tag[aMarker.Index];
                    SwitchPage(Panel_MarkerReveal);
                  end;
  end;
end;


procedure TKMapEdInterface.Menu_Save(Sender: TObject);
var
  SaveName: string;
begin
  SaveName := Trim(Edit_SaveName.Text);

  if (Sender = Edit_SaveName) or (Sender = Radio_Save_MapType) then
  begin
    CheckBox_SaveExists.Enabled := FileExists(TKMapsCollection.FullPath(SaveName, '.dat', Radio_Save_MapType.ItemIndex = 1));
    Label_SaveExists.Visible := CheckBox_SaveExists.Enabled;
    CheckBox_SaveExists.Checked := False;
    Button_SaveSave.Enabled := not CheckBox_SaveExists.Enabled;
  end;

  if Sender = CheckBox_SaveExists then
    Button_SaveSave.Enabled := CheckBox_SaveExists.Checked;

  if Sender = Button_SaveSave then
  begin
    //Should we expand the path here? It depends.. since we are passing mask for map/dat files/folder
    fGame.SaveMapEditor(SaveName, Radio_Save_MapType.ItemIndex = 1);

    Player_UpdateColors;
    Player_ChangeActive(nil);
    Label_MissionName.Caption := fGame.GameName;

    SwitchPage(Button_SaveCancel); //return to previous menu
  end;
end;


procedure TKMapEdInterface.Marker_Change(Sender: TObject);
var
  DP: TAIDefencePosition;
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

                  end;
    mtRevealFOW: fGame.MapEditor.Revealers[fActiveMarker.Owner].Tag[fActiveMarker.Index] := TrackBar_RevealSize.Position;
  end;
end;


//Mission loading dialog
procedure TKMapEdInterface.Menu_Load(Sender: TObject);
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
procedure TKMapEdInterface.Menu_QuitMission(Sender: TObject);
begin
  fGameApp.Stop(gr_MapEdEnd);
end;


procedure TKMapEdInterface.Load_MapTypeChange(Sender: TObject);
begin
  Load_MapListUpdate;
end;


procedure TKMapEdInterface.Load_MapListUpdate;
begin
  fMaps.TerminateScan;
  fMapsMP.TerminateScan;

  ListBox_Load.SetItems('');
  ListBox_Load.ItemIndex := -1;

  if Radio_Load_MapType.ItemIndex = 0 then
    fMaps.Refresh(Load_MapListUpdateDone)
  else
    fMapsMP.Refresh(Load_MapListUpdateDone);
end;


procedure TKMapEdInterface.Load_MapListUpdateDone(Sender: TObject);
begin
  if Radio_Load_MapType.ItemIndex = 0 then
    ListBox_Load.SetItems(fMaps.MapList)
  else
    ListBox_Load.SetItems(fMapsMP.MapList);

  //Try to select first map by default
  if ListBox_Load.ItemIndex = -1 then
    ListBox_Load.ItemIndex := 0;
end;


//This function will be called if the user right clicks on the screen.
procedure TKMapEdInterface.RightClick_Cancel;
begin
  //We should drop the tool but don't close opened tab. This allows eg: Place a warrior, right click so you are not placing more warriors, select the placed warrior.
  //Before you would have had to close the tab to do this.
  if GetShownPage = esp_Terrain then exit; //Terrain uses both buttons for relief changing, tile rotation etc.
  GameCursor.Mode:=cmNone;
  GameCursor.Tag1:=0;
end;


procedure TKMapEdInterface.SetTileDirection(aTileDirection: byte);
begin
  fTileDirection := aTileDirection mod 4; //0..3
  GameCursor.MapEdDir := fTileDirection;
end;


procedure TKMapEdInterface.SetLoadMode(aMultiplayer:boolean);
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


function TKMapEdInterface.GetSelectedTile: TObject;
var i: byte;
begin
  Result := nil;
  for i:=1 to MAPED_TILES_COLS*MAPED_TILES_ROWS do
    if TilesTable[i].Down then Result := TilesTable[i];
end;


function TKMapEdInterface.GetSelectedObject: TObject;
var i: byte;
begin
  Result := nil;
  for i:=1 to 4 do
    if ObjectsTable[i].Down then Result := ObjectsTable[i];
end;


function TKMapEdInterface.GetSelectedUnit: TObject;
var i: byte;
begin
  Result := nil;
  for i:=0 to High(Button_Citizen) do
    if Button_Citizen[i].Down then Result := Button_Citizen[i];
  for i:=0 to High(Button_Warriors) do
    if Button_Warriors[i].Down then Result := Button_Warriors[i];
  for i:=0 to High(Button_Animals) do
    if Button_Animals[i].Down then Result := Button_Animals[i];
end;


procedure TKMapEdInterface.Store_FillValues(Sender: TObject);
var
  I, Tmp: Integer;
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouseStore) then exit;

  for I := 1 to STORE_RES_COUNT do
  begin
    Tmp := TKMHouseStore(fPlayers.Selected).CheckResIn(StoreResType[I]);
    Button_Store[I].Caption := IfThen(Tmp = 0, '-', IntToStr(Tmp));
  end;
end;


procedure TKMapEdInterface.Barracks_FillValues(Sender: TObject);
var
  I, Tmp: Integer;
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouseBarracks) then exit;

  for I := 1 to BARRACKS_RES_COUNT do
  begin
    Tmp := TKMHouseBarracks(fPlayers.Selected).CheckResIn(BarracksResType[I]);
    Button_Barracks[I].Caption := IfThen(Tmp = 0, '-', IntToStr(Tmp));
  end;
end;


procedure TKMapEdInterface.House_HealthChange(Sender: TObject; AButton: TMouseButton);
var
  H: TKMHouse;
  I: Integer;
  Res: TResourceType;
  NewCount: Integer;
begin
  if not (fPlayers.Selected is TKMHouse) then Exit;
  H := TKMHouse(fPlayers.Selected);

  if Sender = Button_HouseHealthDec then H.AddDamage(ClickAmount[AButton] * 5, True);
  if Sender = Button_HouseHealthInc then H.AddRepair(ClickAmount[AButton] * 5);

  for I := 0 to 3 do
  begin
    Res := fResource.HouseDat[H.HouseType].ResInput[I+1];

    if Sender = ResRow_Resource[I].OrderAdd then
    begin
      NewCount := Math.Min(ClickAmount[aButton], MAX_RES_IN_HOUSE - H.CheckResIn(Res));
      H.ResAddToIn(Res, NewCount);
    end;

    if Sender = ResRow_Resource[I].OrderRem then
    begin
      NewCount := Math.Min(ClickAmount[aButton], H.CheckResIn(Res));
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
  if not (fPlayers.Selected is TKMUnitGroup) then Exit;

  Group := TKMUnitGroup(fPlayers.Selected);
  if Sender = Button_Army_ForUp then Group.UnitsPerRow := Group.UnitsPerRow - 1;
  if Sender = Button_Army_ForDown then Group.UnitsPerRow := Group.UnitsPerRow + 1;

  ImageStack_Army.SetCount(Group.MapEdCount, Group.UnitsPerRow, Group.UnitsPerRow div 2 + 1);
  Label_ArmyCount.Caption := IntToStr(Group.MapEdCount);

  if Sender = Button_Army_RotCW then  Group.Direction := KMNextDirection(Group.Direction);
  if Sender = Button_Army_RotCCW then Group.Direction := KMPrevDirection(Group.Direction);

  //Toggle between full and half condition
  if Sender = Button_ArmyFood then
  begin
    if Group.Condition = UNIT_MAX_CONDITION then
      Group.Condition := UNIT_MAX_CONDITION div 2
    else
      Group.Condition := UNIT_MAX_CONDITION;
    KMConditionBar_Unit.Position := Group.Condition / UNIT_MAX_CONDITION;
  end;
end;


procedure TKMapEdInterface.Unit_ArmyChange2(Sender: TObject; AButton: TMouseButton);
var
  NewCount: Integer;
  Group: TKMUnitGroup;
begin
  if not (fPlayers.Selected is TKMUnitGroup) then Exit;

  Group := TKMUnitGroup(fPlayers.Selected);

  if Sender = Button_ArmyDec then //Decrease
    NewCount := Group.MapEdCount - ClickAmount[AButton]
  else //Increase
    NewCount := Group.MapEdCount + ClickAmount[AButton];

  Group.MapEdCount := EnsureRange(NewCount, 0, 200); //Limit max members
  ImageStack_Army.SetCount(Group.MapEdCount, Group.UnitsPerRow, Group.UnitsPerRow div 2 + 1);
  Label_ArmyCount.Caption := IntToStr(Group.MapEdCount);
end;


procedure TKMapEdInterface.Barracks_SelectWare(Sender: TObject);
var I: Integer;
begin
  if not Panel_HouseBarracks.Visible then exit;
  if not (Sender is TKMButtonFlat) then exit; //Only FlatButtons
  if TKMButtonFlat(Sender).Tag = 0 then exit; //with set Tag

  for I:=1 to BARRACKS_RES_COUNT do
    Button_Barracks[I].Down := False;
  TKMButtonFlat(Sender).Down := True;
  fBarracksItem := TKMButtonFlat(Sender).Tag;
  Barracks_EditWareCount(Sender, mbLeft);
end;


procedure TKMapEdInterface.Store_SelectWare(Sender: TObject);
var i:Integer;
begin
  if not Panel_HouseStore.Visible then exit;
  if not (Sender is TKMButtonFlat) then exit; //Only FlatButtons
  if TKMButtonFlat(Sender).Tag = 0 then exit; //with set Tag
  for i:=1 to length(Button_Store) do
    Button_Store[i].Down := false;
  TKMButtonFlat(Sender).Down := true;
  fStorehouseItem := TKMButtonFlat(Sender).Tag;
  Store_EditWareCount(Sender, mbLeft);
end;


procedure TKMapEdInterface.Barracks_EditWareCount(Sender: TObject; AButton:TMouseButton);
var
  Res: TResourceType;
  Barracks: TKMHouseBarracks;
  NewCount: Word;
begin
  if not Panel_HouseBarracks.Visible or not (fPlayers.Selected is TKMHouseBarracks) then Exit;

  Res := BarracksResType[fBarracksItem];
  Barracks := TKMHouseBarracks(fPlayers.Selected);

  if (Sender = Button_BarracksDec100) or (Sender = Button_BarracksDec) then begin
    NewCount := Math.Min(Barracks.CheckResIn(Res), ClickAmount[aButton] * TKMButton(Sender).Tag);
    Barracks.ResTakeFromOut(Res, NewCount);
  end;

  if (Sender = Button_BarracksInc100) or (Sender = Button_BarracksInc) then begin
    NewCount := Math.Min(High(Word) - Barracks.CheckResIn(Res), ClickAmount[aButton] * TKMButton(Sender).Tag);
    Barracks.ResAddToIn(Res, NewCount);
  end;

  Label_Barracks_WareCount.Caption := IntToStr(Barracks.CheckResIn(Res));
  Barracks_FillValues(nil);
end;


procedure TKMapEdInterface.Store_EditWareCount(Sender: TObject; AButton:TMouseButton);
var
  Res: TResourceType;
  Store: TKMHouseStore;
  NewCount: Word;
begin
  if not Panel_HouseStore.Visible or not (fPlayers.Selected is TKMHouseStore) then Exit;

  Res := StoreResType[fStorehouseItem];
  Store := TKMHouseStore(fPlayers.Selected);

  //We need to take no more than it is there, thats part of bugtracking idea
  if (Sender = Button_StoreDec100) or (Sender = Button_StoreDec) then begin
    NewCount := Math.Min(Store.CheckResIn(Res), ClickAmount[aButton]*TKMButton(Sender).Tag);
    Store.ResTakeFromOut(Res, NewCount);
  end;

  //We can always add any amount of resource, it will be capped by Store
  if (Sender = Button_StoreInc100) or (Sender = Button_StoreInc) then
    Store.ResAddToIn(Res, ClickAmount[aButton]*TKMButton(Sender).Tag);

  Label_Store_WareCount.Caption := inttostr(Store.CheckResIn(Res));
  Store_FillValues(nil);
end;


procedure TKMapEdInterface.Player_ColorClick(Sender: TObject);
begin
  if not (Sender = ColorSwatch_Color) then exit;
  MyPlayer.FlagColor := ColorSwatch_Color.GetColor;
  Player_UpdateColors;
end;


procedure TKMapEdInterface.Player_BlockClick(Sender: TObject);
var
  I: Integer;
  H: THouseType;
begin
  I := TKMButtonFlat(Sender).Tag;
  H := GUIHouseOrder[I];

  //Loop through states CanBuild > CantBuild > Released
  if not MyPlayer.Stats.HouseBlocked[H] and not MyPlayer.Stats.HouseGranted[H] then
  begin
    MyPlayer.Stats.HouseBlocked[H] := True;
    MyPlayer.Stats.HouseGranted[H] := False;
    Image_BlockHouse[I].TexID := 32;
  end else
  if MyPlayer.Stats.HouseBlocked[H] and not MyPlayer.Stats.HouseGranted[H] then
  begin
    MyPlayer.Stats.HouseBlocked[H] := False;
    MyPlayer.Stats.HouseGranted[H] := True;
    Image_BlockHouse[I].TexID := 33;
  end else
  begin
    MyPlayer.Stats.HouseBlocked[H] := False;
    MyPlayer.Stats.HouseGranted[H] := False;
    Image_BlockHouse[I].TexID := 0;
  end;
end;


procedure TKMapEdInterface.Player_BlockRefresh;
var
  I: Integer;
  H: THouseType;
begin
  for I := 1 to GUI_HOUSE_COUNT do
  begin
    H := GUIHouseOrder[I];
    if MyPlayer.Stats.HouseBlocked[H] and not MyPlayer.Stats.HouseGranted[H] then
      Image_BlockHouse[I].TexID := 32
    else
    if MyPlayer.Stats.HouseGranted[H] and not MyPlayer.Stats.HouseBlocked[H] then
      Image_BlockHouse[I].TexID := 33
    else
    if not MyPlayer.Stats.HouseGranted[H] and not MyPlayer.Stats.HouseBlocked[H] then
      Image_BlockHouse[I].TexID := 0
    else
      Image_BlockHouse[I].TexID := 24; //Some erroneous value
  end;
end;


procedure TKMapEdInterface.Player_RevealClick(Sender: TObject);
begin
  //Press the button
  TKMButtonFlat(Sender).Down := True;

  //Reset cursor and see if it needs to be changed
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  if Button_Reveal.Down then
  begin
    GameCursor.Mode := cmMarkers;
    GameCursor.Tag1 := MARKER_REVEAL;
  end;
end;


procedure TKMapEdInterface.Mission_AlliancesChange(Sender: TObject);
var i,k:Integer;
begin
  if Sender = nil then begin
    for i:=0 to fPlayers.Count-1 do
    for k:=0 to fPlayers.Count-1 do
      if (fPlayers[i]<>nil)and(fPlayers[k]<>nil) then
        CheckBox_Alliances[i,k].Checked := (fPlayers.CheckAlliance(fPlayers[i].PlayerIndex, fPlayers[k].PlayerIndex)=at_Ally)
      else
        CheckBox_Alliances[i,k].Disable; //Player does not exist?
    exit;
  end;

  i := TKMCheckBox(Sender).Tag div fPlayers.Count;
  k := TKMCheckBox(Sender).Tag mod fPlayers.Count;
  if CheckBox_Alliances[i,k].Checked then fPlayers[i].Alliances[k] := at_Ally
                                     else fPlayers[i].Alliances[k] := at_Enemy;

  //Copy status to symmetrical item
  if CheckBox_AlliancesSym.Checked then begin
    CheckBox_Alliances[k,i].Checked := CheckBox_Alliances[i,k].Checked;
    fPlayers[k].Alliances[i] := fPlayers[i].Alliances[k];
  end;
end;


procedure TKMapEdInterface.Mission_PlayerTypesChange(Sender: TObject);
var i:Integer;
begin
  if Sender = nil then begin
    for i:=0 to fPlayers.Count-1 do
    begin
      CheckBox_PlayerTypes[i,0].Enabled := fPlayers[i]<>nil;
      CheckBox_PlayerTypes[i,1].Enabled := fPlayers[i]<>nil;
      CheckBox_PlayerTypes[i,0].Checked := (fPlayers[i]<>nil) and (fPlayers[i].PlayerType = pt_Human);
      CheckBox_PlayerTypes[i,1].Checked := (fPlayers[i]<>nil) and (fPlayers[i].PlayerType = pt_Computer);
    end;
    Exit;
  end;

  //@Lewin: Are we allowed to define players freely, e.g. make 5 Human players?
  //How is it working in multiplayer?
  //@Krom: In KaM it works like this: Single player missions have 1 human player and the others computer.
  //       For multiplayer the players are all humans. (although they are not defined in the script with !SET_HUMAN_PLAYER as that is for single missions)
  //       I think we should be a bit more relaxed about it. Here are some cases:
  //       1. Campaign/single missions: Only 1 player is human, the others are computer and this cannot be changed. (usually story based)
  //       2. Tournament missions: (like single missions from TPR but configurable for every game) All players start equal and any can be
  //                               human/AI/not-participating. This means you can chose the number of enemies you wish to fight, and configure alliances.
  //                               e.g. I can chose to fight with me plus 1 computer (team 1) against 3 computers allied together. (team 2)
  //       3. Multiplayer tournament: Same as a single tournament mission, but you can have many humans and many AI. (with configurable teams or deathmatch)
  //       4. Multiplayer cooperative: Similar to a tournament but one or more AI are fixed and cannot be made human. (and the mission is usually story based)
  //                                   This allows a mission to be made where many humans siege AI players in a castle, where the AI have an obvious
  //                                   advantage and the humans must work together to defeat them.
  //       These are just some ideas for the kinds of missions I think we should allow. Note that TPR only allows for types 1 and 3.
  //       I do NOT think that each mission should be given a "type" of the ones mentioned above. That just makes things complicated having 4 mission types.
  //       We do not even need a single/multiplayer distinction. I think we can make this work by having two kinds of players:
  //       a) General: which can be controlled by a human or a computer
  //       b) Fixed AI: which can only be computers, never human controlled.
  //       Therefore both single and multiplayer tournament missions will only use General players, but the other two types will use
  //       some Fixed AI for the players which must always be computer controlled, and some General players which can be either or not-participating.
  //       When you make a mission you can define AI options for the General players if you wish, otherwise they will use defaults. (and figure it out
  //       automatically) Fixed AI will mostly need to be told how to behave, for the story make sense and fit with the circumstances.
  //       (e.g. so they don't try to build a city when it is a siege mission) This allows single player missions to be used for multiplayer and vice versa.
  //       These are just ideas and I think they could be redesigned in a less confusing way for both the players and mission creators.
  //       Let me know what you think. Maybe we should discuss this.
  //@Lewin: Looks like we can't achieve it without changing(adding) mission scripts.. discussed in ICQ.

  //Reset everything
  for i:=0 to fPlayers.Count-1 do
  begin
    CheckBox_PlayerTypes[i,0].Checked := false;
    CheckBox_PlayerTypes[i,1].Checked := true;
    fPlayers[i].PlayerType := pt_Computer;
  end;

  //Define only 1 human player
  i := TKMCheckBox(Sender).Tag;
  if Sender = CheckBox_PlayerTypes[i,0] then
  begin
    CheckBox_PlayerTypes[i,0].Checked := true;
    CheckBox_PlayerTypes[i,1].Checked := false;
    fPlayers[i].PlayerType := pt_Human
  end;
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

  //Scrolling
  if Key = VK_LEFT  then fGame.Viewport.ScrollKeyLeft  := true;
  if Key = VK_RIGHT then fGame.Viewport.ScrollKeyRight := true;
  if Key = VK_UP    then fGame.Viewport.ScrollKeyUp    := true;
  if Key = VK_DOWN  then fGame.Viewport.ScrollKeyDown  := true;
end;


procedure TKMapEdInterface.KeyUp(Key: Word; Shift: TShiftState);
begin
  if fMyControls.KeyUp(Key, Shift) then Exit; //Handled by Controls

  //1-5 game menu shortcuts
  if Key in [49..53] then
    Button_Main[Key-48].Click;

  //Scrolling
  if Key = VK_LEFT  then fGame.Viewport.ScrollKeyLeft  := false;
  if Key = VK_RIGHT then fGame.Viewport.ScrollKeyRight := false;
  if Key = VK_UP    then fGame.Viewport.ScrollKeyUp    := false;
  if Key = VK_DOWN  then fGame.Viewport.ScrollKeyDown  := false;

  //Backspace resets the zoom and view, similar to other RTS games like Dawn of War.
  //This is useful because it is hard to find default zoom using the scroll wheel, and if not zoomed 100% things can be scaled oddly (like shadows)
  if Key = VK_BACK  then fGame.Viewport.ResetZoom;
end;


procedure TKMapEdInterface.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  inherited;

  //So terrain brushes start on mouse down not mouse move
  if fMyControls.CtrlOver = nil then
    fGame.UpdateGameCursor(X,Y,Shift);
end;


procedure TKMapEdInterface.MouseMove(Shift: TShiftState; X,Y: Integer);
var
  P: TKMPoint;
  Marker: TKMMapEdMarker;
begin
  inherited;

  if fMyControls.CtrlOver <> nil then
  begin
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
    if fPlayers.HitTest(GameCursor.Cell.X, GameCursor.Cell.Y, False) <> nil then
      fResource.Cursors.Cursor := kmc_Info
    else
      if not fGame.Viewport.Scrolling then
        fResource.Cursors.Cursor := kmc_Default;
  end;

  Label_Coordinates.Caption := Format('X: %d, Y: %d',[GameCursor.Cell.X,GameCursor.Cell.Y]);

  if ssLeft in Shift then //Only allow placing of roads etc. with the left mouse button
  begin
    P := GameCursor.Cell; //Get cursor position tile-wise
    case GameCursor.Mode of
      cmRoad:      if MyPlayer.CanAddFieldPlan(P, ft_Road) then MyPlayer.AddField(P, ft_Road);
      cmField:     if MyPlayer.CanAddFieldPlan(P, ft_Corn) then MyPlayer.AddField(P, ft_Corn);
      cmWine:      if MyPlayer.CanAddFieldPlan(P, ft_Wine) then MyPlayer.AddField(P, ft_Wine);
      //cm_Wall:  if MyPlayer.CanAddFieldPlan(P, ft_Wall) then MyPlayer.AddField(P, ft_Wine);
      cmObjects:   if GameCursor.Tag1 = 255 then fTerrain.SetTree(P, 255); //Allow many objects to be deleted at once
      cmErase:     case GetShownPage of
                      esp_Terrain:    fTerrain.Land[P.Y,P.X].Obj := 255;
                      esp_Units:      fPlayers.RemAnyUnit(P);
                      esp_Buildings:  begin
                                        fPlayers.RemAnyHouse(P);
                                        if fTerrain.Land[P.Y,P.X].TileOverlay = to_Road then
                                          fTerrain.RemRoad(P);
                                        if fTerrain.TileIsCornField(P) or fTerrain.TileIsWineField(P) then
                                          fTerrain.RemField(P);
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
  inherited;

  if fMyControls.CtrlOver <> nil then begin
    fMyControls.MouseUp(X,Y,Shift,Button);
    exit; //We could have caused fGame reinit, so exit at once
  end;

  fGame.UpdateGameCursor(X, Y, Shift); //Updates the shift state
  P := GameCursor.Cell; //Get cursor position tile-wise
  if Button = mbRight then
  begin
    RightClick_Cancel;

    //Right click performs some special functions and shortcuts
    case GameCursor.Mode of
      cmTiles:   begin
                    SetTileDirection(GameCursor.MapEdDir+1); //Rotate tile direction
                    TilesRandom.Checked := false; //Reset
                  end;
      cmObjects: fTerrain.Land[P.Y,P.X].Obj := 255; //Delete object
    end;
    //Move the selected object to the cursor location
    if fPlayers.Selected is TKMHouse then
      TKMHouse(fPlayers.Selected).SetPosition(P); //Can place is checked in SetPosition

    if fPlayers.Selected is TKMUnit then
      TKMUnit(fPlayers.Selected).SetPosition(P);

    if fPlayers.Selected is TKMUnitGroup then
      TKMUnitGroup(fPlayers.Selected).Position := P;

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
      cmNone:  begin
                  //If there are some additional layers we first HitTest them
                  //as they are rendered ontop of Houses/Objects
                  SelMarker := fGame.MapEditor.HitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
                  if SelMarker.MarkerType <> mtNone then
                    ShowMarkerInfo(SelMarker)
                  else
                  begin
                    fPlayers.SelectHitTest(GameCursor.Cell.X, GameCursor.Cell.Y, False);

                    if fPlayers.Selected is TKMHouse then
                      ShowHouseInfo(TKMHouse(fPlayers.Selected));
                    if fPlayers.Selected is TKMUnit then
                      ShowUnitInfo(TKMUnit(fPlayers.Selected));
                    if fPlayers.Selected is TKMUnitGroup then
                      ShowGroupInfo(TKMUnitGroup(fPlayers.Selected));
                  end;
                end;
      cmRoad:  if MyPlayer.CanAddFieldPlan(P, ft_Road) then MyPlayer.AddField(P, ft_Road);
      cmField: if MyPlayer.CanAddFieldPlan(P, ft_Corn) then MyPlayer.AddField(P, ft_Corn);
      cmWine:  if MyPlayer.CanAddFieldPlan(P, ft_Wine) then MyPlayer.AddField(P, ft_Wine);
      //cm_Wall:
      cmHouses:if MyPlayer.CanAddHousePlan(P, THouseType(GameCursor.Tag1)) then
                begin
                  MyPlayer.AddHouse(THouseType(GameCursor.Tag1), P.X, P.Y, true);
                  if not(ssShift in Shift) then Build_ButtonClick(Button_BuildRoad);
                end;
      cmElevate,
      cmEqualize:; //handled in UpdateStateIdle
      cmObjects: fTerrain.SetTree(P, GameCursor.Tag1);
      cmUnits: if fTerrain.CanPlaceUnit(P, TUnitType(GameCursor.Tag1)) then
                begin //Check if we can really add a unit
                  if TUnitType(GameCursor.Tag1) in [CITIZEN_MIN..CITIZEN_MAX] then
                    MyPlayer.AddUnit(TUnitType(GameCursor.Tag1), P, False)
                  else
                  if TUnitType(GameCursor.Tag1) in [WARRIOR_MIN..WARRIOR_MAX] then
                    MyPlayer.AddUnitGroup(TUnitType(GameCursor.Tag1), P, dir_S, 1, 1)
                  else
                    fPlayers.PlayerAnimals.AddUnit(TUnitType(GameCursor.Tag1), P, false);
                end;
      cmMarkers:
                case GetShownPage of
                  esp_Reveal: fGame.MapEditor.Revealers[MyPlayer.PlayerIndex].AddEntry(P, TrackBar_RevealNewSize.Position);
                end;

      cmErase:
                case GetShownPage of
                  esp_Terrain:    fTerrain.Land[P.Y,P.X].Obj := 255;
                  esp_Units:      fPlayers.RemAnyUnit(P);
                  esp_Buildings:  begin
                                    fPlayers.RemAnyHouse(P);
                                    if fTerrain.Land[P.Y,P.X].TileOverlay = to_Road then
                                      fTerrain.RemRoad(P);
                                    if fTerrain.TileIsCornField(P) or fTerrain.TileIsWineField(P) then
                                      fTerrain.RemField(P);
                                  end;
                  //todo: esp_Reveal:   fGame.MapEditor.Revealers.Remove(P);
                end;
    end;
end;


function TKMapEdInterface.GetShownPage: TKMMapEdShownPage;
begin
  Result := esp_Unknown;
  if Panel_Terrain.Visible then   Result := esp_Terrain;
  if Panel_Build.Visible then     Result := esp_Buildings;
  if Panel_Units.Visible then     Result := esp_Units;
  if Panel_RevealFOW.Visible then Result := esp_Reveal;
end;


end.
