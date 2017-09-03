unit KM_InterfaceMapEditor;
{$I KaM_Remake.inc}
interface
uses
   {$IFDEF MSWindows} Windows, {$ENDIF}
   {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
   Classes, Controls, Math, StrUtils, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics, KM_Points,
   KM_Houses, KM_Units, KM_UnitGroups, KM_MapEditor,
   KM_InterfaceDefaults, KM_InterfaceGame, KM_Terrain, KM_Minimap, KM_Viewport, KM_Render,
   KM_GUIMapEdHouse,
   KM_GUIMapEdGoal,
   KM_GUIMapEdTerrain,
   KM_GUIMapEdTown,
   KM_GUIMapEdPlayer,
   KM_GUIMapEdMission,
   KM_GUIMapEdAttack,
   KM_GUIMapEdExtras,
   KM_GUIMapEdMessage,
   KM_GUIMapEdFormations,
   KM_GUIMapEdMarkerDefence,
   KM_GUIMapEdMarkerReveal,
   KM_GUIMapEdMenu,
   KM_GUIMapEdUnit;

type
  TKMapEdInterface = class (TKMUserInterfaceGame)
  private
    fPrevHint: TObject;
    fMouseDownOnMap: Boolean;

    // Drag object feature fields
    fDragObjectReady: Boolean;   // Ready to start drag object
    fDragingObject: Boolean;     // Flag when drag object is happening
    fDragObject: TObject;        // Object to drag
    fDragHouseOffset: TKMPoint;  // Offset for house position, to let grab house with any of its points

    fGuiHouse: TKMMapEdHouse;
    fGuiUnit: TKMMapEdUnit;
    fGuiTerrain: TKMMapEdTerrain;
    fGuiTown: TKMMapEdTown;
    fGuiPlayer: TKMMapEdPlayer;
    fGuiMission: TKMMapEdMission;
    fGuiAttack: TKMMapEdAttack;
    fGuiGoal: TKMMapEdGoal;
    fGuiFormations: TKMMapEdFormations;
    fGuiExtras: TKMMapEdExtras;
    fGuiMessage: TKMMapEdMessage;
    fGuiMarkerDefence: TKMMapEdMarkerDefence;
    fGuiMarkerReveal: TKMMapEdMarkerReveal;
    fGuiMenu: TKMMapEdMenu;

    procedure Layers_UpdateVisibility;
    procedure Marker_Done(Sender: TObject);
    procedure Minimap_OnUpdate(Sender: TObject; const X,Y: Integer);
    procedure PageChanged(Sender: TObject);
    procedure Player_ActiveClick(Sender: TObject);
    procedure Message_Click(Sender: TObject);
    procedure ChangeOwner_Click(Sender: TObject);
    procedure UniversalEraser_Click(Sender: TObject);

    procedure UpdateCursor(X, Y: Integer; Shift: TShiftState);
    procedure Main_ButtonClick(Sender: TObject);
    procedure HidePages;
    procedure DisplayHint(Sender: TObject);
    procedure RightClick_Cancel;
    procedure ShowMarkerInfo(aMarker: TKMMapEdMarker);
    procedure Player_SetActive(aIndex: TKMHandIndex);
    procedure Player_UpdatePages;
    procedure UpdateStateInternal;
    procedure UpdatePlayerSelectButtons;
    procedure SetPaintBucketMode(aSetPaintBucketMode: Boolean);
    procedure SetUniversalEraserMode(aSetUniversalEraserMode: Boolean);
    procedure MoveObjectToCursorCell(aObjectToMove: TObject);
    procedure UpdateSelection;
    procedure DragHouseModeStart(aHouseNewPos: TKMPoint; aHouseOldPos: TKMPoint);
    procedure DragHouseModeEnd;
    function IsDragHouseModeOn: Boolean;
    procedure ResetDragObject;
    procedure ResetCursorMode;
  protected
    MinimapView: TKMMinimapView;
    Label_Coordinates: TKMLabel;
    Button_PlayerSelect: array [0..MAX_HANDS-1] of TKMFlatButtonShape; //Animals are common for all
    Button_ChangeOwner: TKMButtonFlat;
    Button_UniversalEraser: TKMButtonFlat;
    Label_Stat,Label_Hint: TKMLabel;
    Bevel_HintBG: TKMBevel;

    Panel_Common: TKMPanel;
      Button_Main: array [1..5] of TKMButton; //5 buttons
      Label_MissionName: TKMLabel;
      Image_Extra: TKMImage;
      Image_Message: TKMImage;
  public
    constructor Create(aRender: TRender);
    destructor Destroy; override;

    procedure ShowMessage(aText: string);
    procedure ExportPages(aPath: string); override;
    property Minimap: TKMMinimap read fMinimap;
    property Viewport: TKMViewport read fViewport;
    property GuiTerrain: TKMMapEdTerrain read fGuiTerrain;

    procedure KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean); override;
    procedure KeyUp(Key: Word; Shift: TShiftState; var aHandled: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer; var aHandled: Boolean); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer); override;
    procedure Resize(X,Y: Word); override;
    procedure SetLoadMode(aMultiplayer:boolean);

    procedure SyncUI(aMoveViewport: Boolean = True); override;
    procedure UpdateState(aTickCount: Cardinal); override;
    procedure UpdateStateImmidiately;
    procedure UpdateStateIdle(aFrameTime: Cardinal); override;
    procedure Paint; override;
  end;


implementation
uses
  KM_HandsCollection, KM_ResTexts, KM_Game, KM_Main, KM_GameCursor, KM_RenderPool,
  KM_Resource, KM_TerrainDeposits, KM_ResCursors, KM_ResKeys, KM_GameApp, KM_Utils,
  KM_Hand, KM_AIDefensePos, KM_RenderUI, KM_ResFonts, KM_CommonClasses, KM_Units_Warrior,
  KM_HouseBarracks, KM_ResHouses;

const
  GROUP_IMG: array [TGroupType] of Word = (
    371, 374,
    376, 377);


{ TKMapEdInterface }
constructor TKMapEdInterface.Create(aRender: TRender);
var
  I: Integer;
  S: TKMShape;
begin
  inherited;

  fMinimap.PaintVirtualGroups := True;

  ResetDragObject;

  TKMImage.Create(Panel_Main, 0,    0, 224, 200, 407); //Minimap place
  TKMImage.Create(Panel_Main, 0,  200, 224, 400, 404);
  TKMImage.Create(Panel_Main, 0,  600, 224, 400, 404);
  TKMImage.Create(Panel_Main, 0, 1000, 224, 400, 404); //For 1600x1200 this is needed

  MinimapView := TKMMinimapView.Create(Panel_Main, 10, 10, 176, 176);
  MinimapView.OnChange := Minimap_OnUpdate;

  Label_MissionName := TKMLabel.Create(Panel_Main, 230, 10, 500, 10, NO_TEXT, fnt_Grey, taLeft);
  Label_Coordinates := TKMLabel.Create(Panel_Main, 230, 30, 'X: Y:', fnt_Grey, taLeft);
  Label_Stat := TKMLabel.Create(Panel_Main, 230, 50, 0, 0, '', fnt_Outline, taLeft);

  TKMLabel.Create(Panel_Main, TB_PAD, 190, TB_WIDTH, 0, gResTexts[TX_MAPED_PLAYERS], fnt_Outline, taLeft);
  for I := 0 to MAX_HANDS - 1 do
  begin
    Button_PlayerSelect[I]         := TKMFlatButtonShape.Create(Panel_Main, 6 + (I mod 6)*23, 208 + 23*(I div 6), 21, 21, IntToStr(I+1), fnt_Grey, $FF0000FF);
    Button_PlayerSelect[I].Tag     := I;
    Button_PlayerSelect[I].OnClick := Player_ActiveClick;
  end;
  Button_PlayerSelect[0].Down := True; //First player selected by default

  Button_ChangeOwner := TKMButtonFlat.Create(Panel_Main, 151, 203, 26, 26, 662);
  Button_ChangeOwner.Down := False;
  Button_ChangeOwner.OnClick := ChangeOwner_Click;
  Button_ChangeOwner.Hint := 'Change owner for object'; // Todo Translate

  Button_UniversalEraser := TKMButtonFlat.Create(Panel_Main, 151, 231, 26, 26, 340);
  Button_UniversalEraser.Down := False;
  Button_UniversalEraser.OnClick := UniversalEraser_Click;
  Button_UniversalEraser.Hint := 'Universal eraser'; // Todo Translate

  Image_Extra := TKMImage.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - 48, 30, 48, 494);
  Image_Extra.Anchors := [anLeft, anBottom];
  Image_Extra.HighlightOnMouseOver := True;
  Image_Extra.OnClick := Message_Click;
  Image_Message := TKMImage.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - 48*2, 30, 48, 496);
  Image_Message.Anchors := [anLeft, anBottom];
  Image_Message.HighlightOnMouseOver := True;
  Image_Message.OnClick := Message_Click;
  Image_Message.Hide; //Hidden by default, only visible when a message is shown

  //Must be created before Hint so it goes over them
  fGuiExtras := TKMMapEdExtras.Create(Panel_Main, PageChanged);
  fGuiMessage := TKMMapEdMessage.Create(Panel_Main);

  Panel_Common := TKMPanel.Create(Panel_Main,TB_PAD,262,TB_WIDTH,768);

  {5 big tabs}
  Button_Main[1] := TKMButton.Create(Panel_Common, BIG_PAD_W*0, 0, BIG_TAB_W, BIG_TAB_H, 381, rxGui, bsGame);
  Button_Main[2] := TKMButton.Create(Panel_Common, BIG_PAD_W*1, 0, BIG_TAB_W, BIG_TAB_H, 589, rxGui, bsGame);
  Button_Main[3] := TKMButton.Create(Panel_Common, BIG_PAD_W*2, 0, BIG_TAB_W, BIG_TAB_H, 392, rxGui, bsGame);
  Button_Main[4] := TKMButton.Create(Panel_Common, BIG_PAD_W*3, 0, BIG_TAB_W, BIG_TAB_H, 441, rxGui, bsGame);
  Button_Main[5] := TKMButton.Create(Panel_Common, BIG_PAD_W*4, 0, BIG_TAB_W, BIG_TAB_H, 389, rxGui, bsGame);
  Button_Main[1].Hint := gResTexts[TX_MAPED_TERRAIN];
  Button_Main[2].Hint := gResTexts[TX_MAPED_VILLAGE];
  Button_Main[3].Hint := gResTexts[TX_MAPED_SCRIPTS_VISUAL];
  Button_Main[4].Hint := gResTexts[TX_MAPED_SCRIPTS_GLOBAL];
  Button_Main[5].Hint := gResTexts[TX_MAPED_MENU];
  for I := 1 to 5 do
    Button_Main[I].OnClick := Main_ButtonClick;

  //Editing pages
  fGuiTerrain := TKMMapEdTerrain.Create(Panel_Common, PageChanged);
  fGuiTown := TKMMapEdTown.Create(Panel_Common, PageChanged);
  fGuiPlayer := TKMMapEdPlayer.Create(Panel_Common, PageChanged);
  fGuiMission := TKMMapEdMission.Create(Panel_Common, PageChanged);
  fGuiMenu := TKMMapEdMenu.Create(Panel_Common, PageChanged);

  //Objects pages
  fGuiUnit := TKMMapEdUnit.Create(Panel_Common);
  fGuiHouse := TKMMapEdHouse.Create(Panel_Common);
  fGuiMarkerDefence := TKMMapEdMarkerDefence.Create(Panel_Common, Marker_Done);
  fGuiMarkerReveal := TKMMapEdMarkerReveal.Create(Panel_Common, Marker_Done);

  //Modal pages
  fGuiAttack := TKMMapEdAttack.Create(Panel_Main);
  fGuiFormations := TKMMapEdFormations.Create(Panel_Main);
  fGuiGoal := TKMMapEdGoal.Create(Panel_Main);

  //Pass pop-ups to their dispatchers
  fGuiTown.GuiDefence.FormationsPopUp := fGuiFormations;
  fGuiTown.GuiOffence.AttackPopUp := fGuiAttack;
  fGuiPlayer.GuiPlayerGoals.GoalPopUp := fGuiGoal;

  //Hints go above everything
  Bevel_HintBG := TKMBevel.Create(Panel_Main,224+32,Panel_Main.Height-23,300,21);
  Bevel_HintBG.BackAlpha := 0.5;
  Bevel_HintBG.Hide;
  Bevel_HintBG.Anchors := [anLeft, anBottom];
  Label_Hint := TKMLabel.Create(Panel_Main, 224 + 36, Panel_Main.Height - 21, 0, 0, '', fnt_Outline, taLeft);
  Label_Hint.Anchors := [anLeft, anBottom];

  fMyControls.OnHint := DisplayHint;

  if OVERLAY_RESOLUTIONS then
  begin
    S := TKMShape.Create(Panel_Main, 0, 0, 1024, 576);
    S.LineColor := $FF00FFFF;
    S.LineWidth := 1;
    S.Hitable := False;
    S := TKMShape.Create(Panel_Main, 0, 0, 1024, 768);
    S.LineColor := $FF00FF00;
    S.LineWidth := 1;
    S.Hitable := False;
  end;

  HidePages;
end;


destructor TKMapEdInterface.Destroy;
begin
  fGuiHouse.Free;
  fGuiTerrain.Free;
  fGuiTown.Free;
  fGuiPlayer.Free;
  fGuiMission.Free;
  fGuiAttack.Free;
  fGuiExtras.Free;
  fGuiFormations.Free;
  fGuiGoal.Free;
  fGuiMarkerDefence.Free;
  fGuiMarkerReveal.Free;
  fGuiMenu.Free;
  fGuiMessage.Free;
  fGuiUnit.Free;

  SHOW_TERRAIN_WIRES := false; //Don't show it in-game if they left it on in MapEd
  SHOW_TERRAIN_PASS := 0; //Don't show it in-game if they left it on in MapEd
  inherited;
end;


procedure TKMapEdInterface.Main_ButtonClick(Sender: TObject);
begin
  //Reset cursor mode
  gGameCursor.Mode := cmNone;
  gGameCursor.Tag1 := 0;

  //Reset shown item when user clicks on any of the main buttons
  gMySpectator.Selected := nil;

  HidePages;

  if (Sender = Button_Main[1]) then fGuiTerrain.Show(ttBrush) else
  if (Sender = Button_Main[2]) then
  begin
    fGuiTown.Show(ttHouses);
    fGuiTown.ChangePlayer; //Player's AI status might have changed
  end else
  if (Sender = Button_Main[3]) then fGuiPlayer.Show(ptGoals) else
  if (Sender = Button_Main[4]) then fGuiMission.Show(mtMode) else
  if (Sender = Button_Main[5]) then
  begin
    fGuiMenu.Show;
    //Signal that active page has changed, that may affect layers visibility
    PageChanged(fGuiMenu);
  end;
end;


procedure TKMapEdInterface.HidePages;
var
  I,K: Integer;
begin
  //Hide all existing pages (2 levels)
  for I := 0 to Panel_Common.ChildCount - 1 do
  if Panel_Common.Childs[I] is TKMPanel then
  begin
    Panel_Common.Childs[I].Hide;
    for K := 0 to TKMPanel(Panel_Common.Childs[I]).ChildCount - 1 do
    if TKMPanel(Panel_Common.Childs[I]).Childs[K] is TKMPanel then
      TKMPanel(Panel_Common.Childs[I]).Childs[K].Hide;
  end;
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
    Bevel_HintBG.Width := 8 + gRes.Fonts[Label_Hint.Font].GetTextSize(Label_Hint.Caption).X;
  end;

  fPrevHint := Sender;
end;


procedure TKMapEdInterface.UpdatePlayerSelectButtons;
const
  CAP_COLOR: array [Boolean] of Cardinal = ($80808080, $FFFFFFFF);
var
  I: Integer;
begin
  for I := 0 to MAX_HANDS - 1 do
    Button_PlayerSelect[I].FontColor := CAP_COLOR[gHands[I].HasAssets];
end;


//Should update any items changed by game (resource counts, hp, etc..)
procedure TKMapEdInterface.UpdateState(aTickCount: Cardinal);
begin
  inherited;
  //Update minimap every 1000ms
  if aTickCount mod 10 = 0 then
    fMinimap.Update(False);

  //Show players without assets in grey
  if aTickCount mod 10 = 0 then
    UpdatePlayerSelectButtons;

  UpdateStateInternal;
end;


procedure TKMapEdInterface.UpdateStateInternal;
begin
  fGuiTerrain.UpdateState;
  fGuiHouse.UpdateState;
  fGuiMenu.UpdateState;
  fGuiTown.UpdateState;
  fGuiPlayer.UpdateState;

  Button_ChangeOwner.Down := gGameCursor.Mode = cmPaintBucket;
  Button_UniversalEraser.Down := gGameCursor.Mode = cmUniversalEraser;
end;

  
procedure TKMapEdInterface.UpdateStateImmidiately;
begin
  fMinimap.Update(False);
  UpdatePlayerSelectButtons;
  UpdateStateInternal;
end;


procedure TKMapEdInterface.UpdateStateIdle(aFrameTime: Cardinal);
begin
  //Check to see if we need to scroll
  fViewport.UpdateStateIdle(aFrameTime, False);
  fGuiTown.UpdateStateIdle;
end;


//Update UI state according to game state
procedure TKMapEdInterface.SyncUI(aMoveViewport: Boolean = True);
var
  I: Integer;
begin
  inherited;
  if aMoveViewport then
    fViewport.Position := KMPointF(gTerrain.MapX / 2, gTerrain.MapY / 2);

  MinimapView.SetMinimap(fMinimap);
  MinimapView.SetViewport(fViewport);

  //Set player colors
  for I := 0 to MAX_HANDS - 1 do
    Button_PlayerSelect[I].ShapeColor := gHands[I].FlagColor;

  Player_UpdatePages;

  UpdatePlayerSelectButtons;

  Label_MissionName.Caption := gGame.GameName;
end;


//Active page has changed, that affects layers visibility
procedure TKMapEdInterface.PageChanged(Sender: TObject);
begin
  //Child panels visibility changed, that affects visible layers
  Layers_UpdateVisibility;
end;


//Set which layers are visible and which are not
//Layer is always visible if corresponding editing page is active (to see what gets placed)
procedure TKMapEdInterface.Layers_UpdateVisibility;
begin
  if gGame = nil then Exit; //Happens on init

  gGame.MapEditor.VisibleLayers := [];

  if fGuiPlayer.Visible(ptView) or fGuiMarkerReveal.Visible then
    gGame.MapEditor.VisibleLayers := gGame.MapEditor.VisibleLayers + [mlRevealFOW, mlCenterScreen];

  if fGuiTown.Visible(ttScript) then
    gGame.MapEditor.VisibleLayers := gGame.MapEditor.VisibleLayers + [mlAIStart];

  if fGuiTown.Visible(ttDefences) or fGuiMarkerDefence.Visible then
    gGame.MapEditor.VisibleLayers := gGame.MapEditor.VisibleLayers + [mlDefences];

  if fGuiExtras.CheckBox_ShowObjects.Checked or fGuiTerrain.Visible(ttObject) then
    gGame.MapEditor.VisibleLayers := gGame.MapEditor.VisibleLayers + [mlObjects];

  if fGuiExtras.CheckBox_ShowHouses.Checked or fGuiTown.Visible(ttHouses) or fGuiHouse.Visible then
    gGame.MapEditor.VisibleLayers := gGame.MapEditor.VisibleLayers + [mlHouses];

  if fGuiExtras.CheckBox_ShowUnits.Checked or fGuiTown.Visible(ttUnits) or fGuiUnit.Visible then
    gGame.MapEditor.VisibleLayers := gGame.MapEditor.VisibleLayers + [mlUnits];

  if fGuiTerrain.Visible(ttSelection) then
    gGame.MapEditor.VisibleLayers := gGame.MapEditor.VisibleLayers + [mlSelection];

  if fGuiExtras.CheckBox_ShowDeposits.Checked then
    gGame.MapEditor.VisibleLayers := gGame.MapEditor.VisibleLayers + [mlDeposits];

  if fGuiExtras.CheckBox_ShowTileOwners.Checked then
    gGame.MapEditor.VisibleLayers := gGame.MapEditor.VisibleLayers + [mlTileOwner];

  if fGuiMenu.GuiMenuResize.Visible then
    gGame.MapEditor.VisibleLayers := gGame.MapEditor.VisibleLayers + [mlMapResize];
end;


procedure TKMapEdInterface.Player_ActiveClick(Sender: TObject);
begin
  //Hide player-specific pages
  fGuiHouse.Hide;
  fGuiUnit.Hide;
  fGuiMarkerDefence.Hide;
  fGuiMarkerReveal.Hide;

  if gMySpectator.Selected <> nil then
    gMySpectator.Selected := nil;

  Player_SetActive(TKMControl(Sender).Tag);
end;


procedure TKMapEdInterface.SetPaintBucketMode(aSetPaintBucketMode: Boolean);
begin
  Button_ChangeOwner.Down := aSetPaintBucketMode;
  if aSetPaintBucketMode then
    gGameCursor.Mode := cmPaintBucket
  else
    gGameCursor.Mode := cmNone;
end;


procedure TKMapEdInterface.SetUniversalEraserMode(aSetUniversalEraserMode: Boolean);
begin
  Button_UniversalEraser.Down := aSetUniversalEraserMode;
  if aSetUniversalEraserMode then
    gGameCursor.Mode := cmUniversalEraser
  else
    gGameCursor.Mode := cmNone;
end;


procedure TKMapEdInterface.ChangeOwner_Click(Sender: TObject);
begin
  SetPaintBucketMode(not Button_ChangeOwner.Down);
end;


procedure TKMapEdInterface.UniversalEraser_Click(Sender: TObject);
begin
  SetUniversalEraserMode(not Button_UniversalEraser.Down);
end;


//Active player can be set either from buttons clicked or by selecting a unit or a house
procedure TKMapEdInterface.Player_SetActive(aIndex: TKMHandIndex);
var
  I: Integer;
begin
  gMySpectator.HandIndex := aIndex;

  for I := 0 to MAX_HANDS - 1 do
    Button_PlayerSelect[I].Down := (I = gMySpectator.HandIndex);

  Player_UpdatePages;
end;


procedure TKMapEdInterface.ShowMarkerInfo(aMarker: TKMMapEdMarker);
begin
  gGame.MapEditor.ActiveMarker := aMarker;
  Assert((aMarker.MarkerType <> mtNone) and (aMarker.Owner <> PLAYER_NONE) and (aMarker.Index <> -1));

  Player_SetActive(aMarker.Owner);

  case aMarker.MarkerType of
    mtDefence:    begin
                    HidePages;
                    fGuiMarkerDefence.Show(aMarker.Owner, aMarker.Index);
                  end;
    mtRevealFOW:  begin
                    HidePages;
                    fGuiMarkerReveal.Show(aMarker.Owner, aMarker.Index);
                  end;
  end;
end;


procedure TKMapEdInterface.ShowMessage(aText: string);
begin
  fGuiMessage.Show(aText);
  Image_Message.Show; //Hidden by default, only visible when a message is shown
end;


//When marker page is done we want to return to markers control page
procedure TKMapEdInterface.Marker_Done(Sender: TObject);
begin
  gGame.MapEditor.ActiveMarker.MarkerType := mtNone;
  if Sender = fGuiMarkerReveal then
  begin
    HidePages;
    fGuiPlayer.Show(ptView);
  end;
  if Sender = fGuiMarkerDefence then
  begin
    HidePages;
    fGuiTown.Show(ttDefences);
  end;
end;


//This function will be called if the user right clicks on the screen.
procedure TKMapEdInterface.RightClick_Cancel;
begin
  //We should drop the tool but don't close opened tab. This allows eg:
  //Place a warrior, right click so you are not placing more warriors,
  //select the placed warrior.

  //These pages use RMB
  if fGuiTerrain.Visible(ttHeights) then Exit;
  if fGuiTerrain.Visible(ttTile) then Exit;
  if fGuiUnit.Visible then Exit;
  if fGuiHouse.Visible then Exit;

  fGuiTerrain.RightClickCancel;

  //Reset cursor
  ResetCursorMode;
  //Reset drag object fields
  ResetDragObject;
end;


procedure TKMapEdInterface.Player_UpdatePages;
begin
  //Update players info on pages
  //Colors are updated as well
  //Update regardless of whether the panels are visible, since the user could open then at any time
  fGuiTown.ChangePlayer;
  fGuiPlayer.ChangePlayer;
end;


procedure TKMapEdInterface.Message_Click(Sender: TObject);
begin
  if Sender = Image_Extra then
    if fGuiExtras.Visible then
      fGuiExtras.Hide
    else
    begin
      fGuiMessage.Hide;
      fGuiExtras.Show;
    end;

  if Sender = Image_Message then
    if fGuiMessage.Visible then
      fGuiMessage.Hide
    else
    begin
      fGuiMessage.Show;
      fGuiExtras.Hide;
    end;
end;


//Update viewport position when user interacts with minimap
procedure TKMapEdInterface.Minimap_OnUpdate(Sender: TObject; const X,Y: Integer);
begin
  fViewport.Position := KMPointF(X,Y);
end;


procedure TKMapEdInterface.ExportPages(aPath: string);
var
  path: string;
  I: TKMTerrainTab;
  K: TKMTownTab;
  L: TKMPlayerTab;
  M: TKMMissionTab;
begin
  inherited;

  path := aPath + 'MapEd' + PathDelim;
  ForceDirectories(path);

  for I := Low(TKMTerrainTab) to High(TKMTerrainTab) do
  begin
    HidePages;
    fGuiTerrain.Show(I);
    gGameApp.PrintScreen(path + 'Terrain' + IntToStr(Byte(I)) + '.jpg');
  end;

  for K := Low(TKMTownTab) to High(TKMTownTab) do
  begin
    HidePages;
    fGuiTown.Show(K);
    gGameApp.PrintScreen(path + 'Town' + IntToStr(Byte(K)) + '.jpg');
  end;

  for L := Low(TKMPlayerTab) to High(TKMPlayerTab) do
  begin
    HidePages;
    fGuiPlayer.Show(L);
    gGameApp.PrintScreen(path + 'Player' + IntToStr(Byte(L)) + '.jpg');
  end;

  for M := Low(TKMMissionTab) to High(TKMMissionTab) do
  begin
    HidePages;
    fGuiMission.Show(M);
    gGameApp.PrintScreen(path + 'Mission' + IntToStr(Byte(M)) + '.jpg');
  end;

  HidePages;
  fGuiHouse.Show(nil);
  gGameApp.PrintScreen(path + 'House.jpg');

  HidePages;
  fGuiUnit.Show(TKMUnit(nil));
  gGameApp.PrintScreen(path + 'Unit.jpg');
end;


procedure TKMapEdInterface.KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);
var
  KeyHandled, KeyPassedToModal: Boolean;
begin
  aHandled := True; // assume we handle all keys here

  if fMyControls.KeyDown(Key, Shift) then
  begin
    fViewport.ReleaseScrollKeys; //Release the arrow keys when you open a window with an edit to stop them becoming stuck
    Exit; //Handled by Controls
  end;

  //For Objects Palette
  fGuiTerrain.KeyDown(Key, Shift, KeyHandled);
  if KeyHandled then Exit;

  inherited KeyDown(Key, Shift, KeyHandled);
  if KeyHandled then Exit;

  gGameCursor.SState := Shift; // Update Shift state on KeyDown

  KeyPassedToModal := False;
  //Pass Key to Modal pages first
  if (fGuiAttack.Visible and fGuiAttack.KeyDown(Key, Shift))
    or (fGuiFormations.Visible and fGuiFormations.KeyDown(Key, Shift))
    or (fGuiGoal.Visible and fGuiGoal.KeyDown(Key, Shift)) then
    KeyPassedToModal := True;

  //For now enter can open up Extra panel
  if not KeyPassedToModal and (Key = gResKeys[SC_MAPEDIT_EXTRA].Key) then
    Message_Click(Image_Extra);

  // If modals are closed or they did not handle key
  if not KeyPassedToModal and (Key = gResKeys[SC_CLOSE_MENU].Key) then
  begin
    if fGuiMessage.Visible then fGuiMessage.Hide;
    if fGuiExtras.Visible then fGuiExtras.Hide;
  end;
end;


procedure TKMapEdInterface.KeyUp(Key: Word; Shift: TShiftState; var aHandled: Boolean);
var
  KeyHandled: Boolean;
begin
  aHandled := True; // assume we handle all keys here

  if fMyControls.KeyUp(Key, Shift) then Exit; //Handled by Controls

  inherited KeyUp(Key, Shift, KeyHandled);
  if KeyHandled then Exit;

  //For undo/redo shortcuts and Objects Palette
  fGuiTerrain.KeyUp(Key, Shift, KeyHandled);
  if KeyHandled then Exit;

  //F1-F5 menu shortcuts
  if Key = gResKeys[SC_MAPEDIT_TERRAIN].Key   then Button_Main[1].Click;
  if Key = gResKeys[SC_MAPEDIT_VILLAGE].Key   then Button_Main[2].Click;
  if Key = gResKeys[SC_MAPEDIT_VISUAL].Key    then Button_Main[3].Click;
  if Key = gResKeys[SC_MAPEDIT_GLOBAL].Key    then Button_Main[4].Click;
  if Key = gResKeys[SC_MAPEDIT_MAIN_MANU].Key then Button_Main[5].Click;

  //1-6 submenu shortcuts
  if Key = gResKeys[SC_MAPEDIT_SUB_MENU_1].Key then
    if fGuiTerrain.Visible then fGuiTerrain.ShowIndex(0) else
    if fGuiTown.Visible    then fGuiTown.ShowIndex(0) else
    if fGuiPlayer.Visible  then fGuiPlayer.ShowIndex(0) else
    if fGuiMission.Visible then fGuiMission.ShowIndex(0);
  if Key = gResKeys[SC_MAPEDIT_SUB_MENU_2].Key then
    if fGuiTerrain.Visible then fGuiTerrain.ShowIndex(1) else
    if fGuiTown.Visible    then fGuiTown.ShowIndex(1) else
    if fGuiPlayer.Visible  then fGuiPlayer.ShowIndex(1) else
    if fGuiMission.Visible then fGuiMission.ShowIndex(1);
  if Key = gResKeys[SC_MAPEDIT_SUB_MENU_3].Key then
    if fGuiTerrain.Visible then fGuiTerrain.ShowIndex(2) else
    if fGuiTown.Visible    then fGuiTown.ShowIndex(2) else
    if fGuiPlayer.Visible  then fGuiPlayer.ShowIndex(2) else
    if fGuiMission.Visible then fGuiMission.ShowIndex(2);
  if Key = gResKeys[SC_MAPEDIT_SUB_MENU_4].Key then
    if fGuiTerrain.Visible then fGuiTerrain.ShowIndex(3) else
    if fGuiTown.Visible    then fGuiTown.ShowIndex(3) else
    if fGuiPlayer.Visible  then fGuiPlayer.ShowIndex(3) else
    if fGuiMission.Visible then fGuiMission.ShowIndex(3);
  if Key = gResKeys[SC_MAPEDIT_SUB_MENU_5].Key then
    if fGuiTerrain.Visible then fGuiTerrain.ShowIndex(4) else
    if fGuiTown.Visible    then fGuiTown.ShowIndex(4) else
    if fGuiPlayer.Visible  then fGuiPlayer.ShowIndex(4) else
    if fGuiMission.Visible then fGuiMission.ShowIndex(4);
  if Key = gResKeys[SC_MAPEDIT_SUB_MENU_6].Key then
    if fGuiTerrain.Visible then fGuiTerrain.ShowIndex(5) else
    if fGuiTown.Visible    then fGuiTown.ShowIndex(5) else
    if fGuiPlayer.Visible  then fGuiPlayer.ShowIndex(5) else
    if fGuiMission.Visible then fGuiMission.ShowIndex(5);

  // Build road/field/wine/cancel build
  if Key = gResKeys[SC_PLAN_ROAD].Key then
  begin
    if not fGuiTown.Visible then
      Button_Main[2].Click;
    fGuiTown.GuiHouses.BuildRoad;
  end;
  if Key = gResKeys[SC_PLAN_FIELD].Key then
  begin
    if not fGuiTown.Visible then
      Button_Main[2].Click;
    fGuiTown.GuiHouses.BuildField;
  end;
  if Key = gResKeys[SC_PLAN_WINE].Key then
  begin
    if not fGuiTown.Visible then
      Button_Main[2].Click;
    fGuiTown.GuiHouses.BuildWine;
  end;
  if Key = gResKeys[SC_ERASE_PLAN].Key then
  begin
    if not fGuiTown.Visible then
      Button_Main[2].Click;
    fGuiTown.GuiHouses.BuildCancel;
  end;

  //Universal erasor
  if Key = gResKeys[SC_MAPEDIT_UNIV_ERASOR].Key then
    UniversalEraser_Click(Button_UniversalEraser);

  gGameCursor.SState := Shift; // Update Shift state on KeyUp
end;


procedure TKMapEdInterface.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  Obj: TObject;
begin
  fMyControls.MouseDown(X,Y,Shift,Button);

  if fMyControls.CtrlOver <> nil then
    Exit;

  if (Button = mbLeft) and (gGameCursor.Mode = cmNone) then
  begin
    Obj := gMySpectator.HitTestCursor;
    if Obj <> nil then
    begin
      UpdateSelection;
      fDragObject := Obj;
      if Obj is TKMHouse then
        fDragHouseOffset := KMPointSubtract(TKMHouse(Obj).Entrance, gGameCursor.Cell); //Save drag point adjustement to house position
      fDragObjectReady := True;
    end;
  end;

  if Button = mbRight then
  begin
    RightClick_Cancel;
  end;

  //So terrain brushes start on mouse down not mouse move
  UpdateCursor(X, Y, Shift);

  gGame.MapEditor.MouseDown(Button);
end;


procedure TKMapEdInterface.MouseMove(Shift: TShiftState; X,Y: Integer; var aHandled: Boolean);
begin
  inherited MouseMove(Shift, X, Y, aHandled);
  if aHandled then Exit;

  aHandled := True;

  if fDragObjectReady then
  begin
    if not (ssLeft in Shift) then
    begin
      ResetDragObject;
      Exit;
    end else begin
      gRes.Cursors.Cursor := kmc_Drag;
      fDragingObject := True;
    end;
  end;

  fMyControls.MouseMove(X,Y,Shift);

  if fMyControls.CtrlOver <> nil then
  begin
    //kmc_Edit and kmc_DragUp are handled by Controls.MouseMove (it will reset them when required)
    if not fViewport.Scrolling and not (gRes.Cursors.Cursor in [kmc_Edit,kmc_DragUp]) then
      gRes.Cursors.Cursor := kmc_Default;
    gGameCursor.SState := []; //Don't do real-time elevate when the mouse is over controls, only terrain
    Exit;
  end
  else
    DisplayHint(nil); //Clear shown hint

  if (ssLeft in Shift) or (ssRight in Shift) then
    fMouseDownOnMap := True;

  UpdateCursor(X, Y, Shift);

  Label_Coordinates.Caption := Format('X: %d, Y: %d', [gGameCursor.Cell.X, gGameCursor.Cell.Y]);

  gGame.MapEditor.MouseMove;
end;


procedure TKMapEdInterface.UpdateCursor(X, Y: Integer; Shift: TShiftState);
var
  Marker: TKMMapEdMarker;
begin
  UpdateGameCursor(X, Y, Shift);

  if gGameCursor.Mode = cmPaintBucket then
  begin
    gRes.Cursors.Cursor := kmc_PaintBucket;
    Exit;
  end;

  if fDragingObject and (ssLeft in Shift) then
  begin
    //Cursor can be reset to default, when moved to menu panel while dragging, so set it to drag cursor again
    gRes.Cursors.Cursor := kmc_Drag;
    MoveObjectToCursorCell(fDragObject);
  end else
  if gGameCursor.Mode = cmNone then
  begin
    Marker := gGame.MapEditor.HitTest(gGameCursor.Cell.X, gGameCursor.Cell.Y);
    if Marker.MarkerType <> mtNone then
      gRes.Cursors.Cursor := kmc_Info
    else
    if gMySpectator.HitTestCursor <> nil then
      gRes.Cursors.Cursor := kmc_Info
    else
    if not fViewport.Scrolling then
      gRes.Cursors.Cursor := kmc_Default;
  end;
end;


procedure TKMapEdInterface.ResetCursorMode;
begin
  gGameCursor.Mode := cmNone;
  gGameCursor.Tag1 := 0;
  gGameCursor.DragOffset := KMPOINT_ZERO;
end;


//Start drag house move mode (with cursor mode cmHouse)
procedure TKMapEdInterface.DragHouseModeStart(aHouseNewPos: TKMPoint; aHouseOldPos: TKMPoint);
  procedure SetCursorModeHouse(aHouseType: THouseType);
  begin
    gGameCursor.Mode := cmHouses;
    gGameCursor.Tag1 := Byte(aHouseType);
    //Update cursor DragOffset to render house markups at proper positions
    gGameCursor.DragOffset := fDragHouseOffset;
  end;
var H: TKMHouse;
begin
  if fDragObject is TKMHouse then
  begin
    H := TKMHouse(fDragObject);
    //Temporarily remove house from terrain to render house markups as there is no current house (we want to move it)
    gTerrain.SetHouse(H.GetPosition, H.HouseType, hsNone, H.Owner);
    SetCursorModeHouse(H.HouseType); //Update cursor mode to cmHouse
  end;
end;


//Drag house move mode end (with cursor mode cmHouse)
procedure TKMapEdInterface.DragHouseModeEnd;
var H: TKMHouse;
begin
  if (fDragObject is TKMHouse) then
  begin
    H := TKMHouse(fDragObject);
    H.SetPosition(KMPointAdd(gGameCursor.Cell, fDragHouseOffset));
    ResetCursorMode;
  end;
end;


function TKMapEdInterface.IsDragHouseModeOn: Boolean;
begin
  Result := fDragingObject and (fDragObject is TKMHouse) and (gGameCursor.Mode = cmHouses);
end;


procedure TKMapEdInterface.MoveObjectToCursorCell(aObjectToMove: TObject);
var H: TKMHouse;
    HouseNewPos, HouseOldPos: TKMPoint;
begin
  if aObjectToMove = nil then Exit;

  //House move
  if aObjectToMove is TKMHouse then
  begin
    H := TKMHouse(aObjectToMove);

    HouseOldPos := H.GetPosition;

    HouseNewPos := KMPointAdd(gGameCursor.Cell, fDragHouseOffset);

    if not fDragingObject then
      H.SetPosition(HouseNewPos)  //handles Right click, when house is selected
    else
      if not IsDragHouseModeOn then
        DragHouseModeStart(HouseNewPos, HouseOldPos);
  end;

  //Unit move
  if aObjectToMove is TKMUnit then
    if aObjectToMove is TKMUnitWarrior then
      aObjectToMove := gHands.GetGroupByMember(TKMUnitWarrior(aObjectToMove))
    else
      TKMUnit(aObjectToMove).SetPosition(gGameCursor.Cell);

  //Unit group move
  if aObjectToMove is TKMUnitGroup then
  begin
    //Just move group to specified location
    TKMUnitGroup(aObjectToMove).Position := gGameCursor.Cell;
  end;
end;


procedure TKMapEdInterface.UpdateSelection;
begin
  gMySpectator.UpdateSelect;

  if gMySpectator.Selected is TKMHouse then
  begin
    HidePages;
    Player_SetActive(TKMHouse(gMySpectator.Selected).Owner);
    fGuiHouse.Show(TKMHouse(gMySpectator.Selected));
  end;
  if gMySpectator.Selected is TKMUnit then
  begin
    HidePages;
    Player_SetActive(TKMUnit(gMySpectator.Selected).Owner);
    fGuiUnit.Show(TKMUnit(gMySpectator.Selected));
  end;
  if gMySpectator.Selected is TKMUnitGroup then
  begin
    HidePages;
    Player_SetActive(TKMUnitGroup(gMySpectator.Selected).Owner);
    fGuiUnit.Show(TKMUnitGroup(gMySpectator.Selected));
  end;
end;


procedure TKMapEdInterface.ResetDragObject;
begin
  fDragObjectReady := False;
  fDragingObject := False;
  fDragHouseOffset := KMPOINT_ZERO;
  fDragObject := nil;

  if gRes.Cursors.Cursor = kmc_Drag then
    gRes.Cursors.Cursor := kmc_Default;

  if gGameCursor.Mode = cmHouses then
    ResetCursorMode;
end;


procedure TKMapEdInterface.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DP: TAIDefencePosition;
  Marker: TKMMapEdMarker;
  G: TKMUnitGroup;
  U: TKMUnit;
  H: TKMHouse;
begin
  if fDragingObject then
  begin
    DragHouseModeEnd;
    ResetDragObject;
  end;

  if fMyControls.CtrlOver <> nil then
  begin
    //Still need to make checkpoint if painting and released over controls
    if fMouseDownOnMap then
    begin
      gGame.MapEditor.MouseUp(Button, False);
      fMouseDownOnMap := False;
    end;
    fMyControls.MouseUp(X,Y,Shift,Button);
    Exit; //We could have caused fGame reinit, so exit at once
  end;

  fMouseDownOnMap := False;

  case Button of
    mbLeft:   if gGameCursor.Mode = cmNone then
              begin
                //If there are some additional layers we first HitTest them
                //since they are rendered ontop of Houses/Objects
                Marker := gGame.MapEditor.HitTest(gGameCursor.Cell.X, gGameCursor.Cell.Y);

                if Marker.MarkerType <> mtNone then
                begin
                  ShowMarkerInfo(Marker);
                  gMySpectator.Selected := nil; //We might have had a unit/group/house selected
                end
                else
                  UpdateSelection;
              end;
    mbRight:  begin
                //Right click performs some special functions and shortcuts
                if gGameCursor.Mode = cmTiles then
                  gGameCursor.MapEdDir := (gGameCursor.MapEdDir + 1) mod 4; //Rotate tile direction

                //Check if we are in rally/cutting marker mode
                if (gGameCursor.Mode = cmMarkers)
                  and ((gGameCursor.Tag1 = MARKER_RALLY_POINT) or (gGameCursor.Tag1 = MARKER_CUTTING_POINT)) then
                begin
                  gGameCursor.Mode := cmNone;
                  gGameCursor.Tag1 := 0;
                  Exit;
                end;

                //Move the selected object to the cursor location
                if gMySpectator.Selected is TKMHouse then
                begin
                  if ssShift in Shift then
                  begin
                    if gMySpectator.Selected is TKMHouseBarracks then
                      TKMHouseBarracks(gMySpectator.Selected).RallyPoint := gGameCursor.Cell
                    else if gMySpectator.Selected is TKMHouseWoodcutters then
                      TKMHouseWoodcutters(gMySpectator.Selected).CuttingPoint := gGameCursor.Cell;
                  end else
                    TKMHouse(gMySpectator.Selected).SetPosition(gGameCursor.Cell); //Can place is checked in SetPosition
                end;

                if gMySpectator.Selected is TKMUnitGroup then
                begin
                  G := TKMUnitGroup(gMySpectator.Selected);
                  //Use Shift to set group order
                  if ssShift in gGameCursor.SState then
                  begin
                    U := gTerrain.UnitsHitTest(gGameCursor.Cell.X, gGameCursor.Cell.Y);
                    H := gHands.HousesHitTest(gGameCursor.Cell.X, gGameCursor.Cell.Y);
                    //If there's any enemy unit or house on specified tile - set attack target
                    if ((U <> nil) and (gHands[U.Owner].Alliances[G.Owner] = at_Enemy))
                    or ((H <> nil) and (gHands[H.Owner].Alliances[G.Owner] = at_Enemy)) then
                      G.MapEdOrder.Order := ioAttackPosition
                    //Else order group walk to specified location
                    else
                    if G.CanWalkTo(KMPoint(gGameCursor.Cell.X, gGameCursor.Cell.Y), 0) then
                      G.MapEdOrder.Order := ioSendGroup
                    else
                    //Can't take any orders: f.e. can't walk to unwalkable tile (water, mountain) or attack allied houses
                      G.MapEdOrder.Order := ioNoOrder;
                    //Save target coordinates
                    G.MapEdOrder.Pos.Loc.X := gGameCursor.Cell.X;
                    G.MapEdOrder.Pos.Loc.Y := gGameCursor.Cell.Y;
                    G.MapEdOrder.Pos.Dir := G.Direction;
                    //Update group GUI
                    fGuiUnit.Show(G);
                  end else
                    MoveObjectToCursorCell(gMySpectator.Selected);
                end else
                  MoveObjectToCursorCell(gMySpectator.Selected);

                if fGuiMarkerDefence.Visible then
                begin
                  DP := gHands[fGuiMarkerDefence.Owner].AI.General.DefencePositions[fGuiMarkerDefence.Index];
                  DP.Position := KMPointDir(gGameCursor.Cell, DP.Position.Dir);
                end;

                if fGuiMarkerReveal.Visible then
                  gGame.MapEditor.Revealers[fGuiMarkerReveal.Owner][fGuiMarkerReveal.Index] := gGameCursor.Cell;
              end;
  end;

  UpdateGameCursor(X, Y, Shift); //Updates the shift state

  gGame.MapEditor.MouseUp(Button, True);

  //Update the XY coordinates of the Center Screen button
  if (gGameCursor.Mode = cmMarkers) and (gGameCursor.Tag1 = MARKER_CENTERSCREEN) then
    fGuiPlayer.ChangePlayer; //Forces an update
end;


procedure TKMapEdInterface.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);
begin
  if gGameCursor.Mode in [cmField, cmWine] then
  begin
    if (X < 0) or (Y < 0) then Exit; // This happens when you use the mouse wheel on the window frame

    // Allow to zoom only when cursor is over map. Controls handle zoom on their own
    if (fMyControls.CtrlOver = nil) then
      gGame.MapEditor.MouseWheel(Shift, WheelDelta, X, Y);
  end else
    inherited;
end;


procedure TKMapEdInterface.Resize(X,Y: Word);
begin
  inherited;

  fViewport.Resize(X, Y);
  fGuiTerrain.Resize;
end;


procedure TKMapEdInterface.SetLoadMode(aMultiplayer:boolean);
begin
  fGuiMenu.SetLoadMode(aMultiplayer);
end;


//UI should paint only controls
procedure TKMapEdInterface.Paint;
  procedure PaintTextInShape(aText: string; X,Y: SmallInt; aLineColor: Cardinal; aTextColor: Cardinal);
  var
    W: Integer;
  begin
    //Paint the background
    W := 10 + 10 * Length(aText);
    TKMRenderUI.WriteShape(X - W div 2, Y - 10, W, 20, $80000000);
    TKMRenderUI.WriteOutline(X - W div 2, Y - 10, W, 20, 2, aLineColor);

    //Paint the label on top of the background
    TKMRenderUI.WriteText(X, Y - 7, 0, aText, fnt_Metal, taCenter, aTextColor);
  end;
const
  DefenceLine: array [TAIDefencePosType] of Cardinal = ($FF80FF00, $FFFF8000);
var
  I, K: Integer;
  R: TRawDeposit;
  DP: TAIDefencePosition;
  LocF: TKMPointF;
  ScreenLoc: TKMPoint;
begin
  if mlDeposits in gGame.MapEditor.VisibleLayers then
  begin
    for R := Low(TRawDeposit) to High(TRawDeposit) do
      for I := 0 to gGame.MapEditor.Deposits.Count[R] - 1 do
      //Ignore water areas with 0 fish in them
      if gGame.MapEditor.Deposits.Amount[R, I] > 0 then
      begin
        LocF := gTerrain.FlatToHeight(gGame.MapEditor.Deposits.Location[R, I]);
        ScreenLoc := fViewport.MapToScreen(LocF);

        //At extreme zoom coords may become out of range of SmallInt used in controls painting
        if KMInRect(ScreenLoc, fViewport.ViewRect) then
          PaintTextInShape(IntToStr(gGame.MapEditor.Deposits.Amount[R, I]), ScreenLoc.X, ScreenLoc.Y, DEPOSIT_COLORS[R], $FFFFFFFF);
      end;
  end;

  if mlDefences in gGame.MapEditor.VisibleLayers then
  begin
    for I := 0 to gHands.Count - 1 do
      for K := 0 to gHands[I].AI.General.DefencePositions.Count - 1 do
      begin
        DP := gHands[I].AI.General.DefencePositions[K];
        LocF := gTerrain.FlatToHeight(KMPointF(DP.Position.Loc.X-0.5, DP.Position.Loc.Y-0.5));
        ScreenLoc := fViewport.MapToScreen(LocF);

        if KMInRect(ScreenLoc, fViewport.ViewRect) then
        begin
          PaintTextInShape(IntToStr(K+1), ScreenLoc.X, ScreenLoc.Y - 22, DefenceLine[DP.DefenceType], FlagColorToTextColor(gHands[I].FlagColor));
          TKMRenderUI.WritePicture(ScreenLoc.X, ScreenLoc.Y, 0, 0, [], rxGui, GROUP_IMG[DP.GroupType]);
        end;
      end;
  end;

  inherited;
end;


end.
