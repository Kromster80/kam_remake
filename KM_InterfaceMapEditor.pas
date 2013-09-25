unit KM_InterfaceMapEditor;
{$I KaM_Remake.inc}
interface
uses
   {$IFDEF MSWindows} Windows, {$ENDIF}
   {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
   Classes, Controls, Math, StrUtils, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics, KM_Points,
   KM_Houses, KM_Units, KM_UnitGroups, KM_MapEditor,
   KM_InterfaceDefaults, KM_Terrain,
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
  TKMapEdInterface = class (TKMUserInterface)
  private
    fPrevHint: TObject;
    fDragScrolling: Boolean;
    fDragScrollingCursorPos: TPoint;
    fDragScrollingViewportPos: TKMPointF;

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

    procedure Main_ButtonClick(Sender: TObject);
    procedure HidePages;
    procedure DisplayHint(Sender: TObject);
    procedure RightClick_Cancel;
    procedure ShowMarkerInfo(aMarker: TKMMapEdMarker);
    procedure Player_SetActive(aIndex: TPlayerIndex);
    procedure Player_UpdatePages;
  protected
    Panel_Main: TKMPanel;
      MinimapView: TKMMinimapView;
      Label_Coordinates: TKMLabel;
      Button_PlayerSelect: array [0..MAX_PLAYERS-1] of TKMFlatButtonShape; //Animals are common for all
      Label_Stat,Label_Hint: TKMLabel;
      Bevel_HintBG: TKMBevel;

    Panel_Common: TKMPanel;
      Button_Main: array [1..5] of TKMButton; //5 buttons
      Label_MissionName: TKMLabel;
      Image_Extra: TKMImage;
      Image_Message: TKMImage;
  public
    constructor Create(aScreenX, aScreenY: Word);
    destructor Destroy; override;

    procedure ShowMessage(aText: string);
    procedure ExportPages(aPath: string);

    procedure KeyDown(Key: Word; Shift: TShiftState); override;
    procedure KeyUp(Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;

    procedure SyncUI;
    procedure UpdateState(aTickCount: Cardinal); override;
    procedure Paint; override;
  end;


implementation
uses
  KM_PlayersCollection, KM_ResTexts, KM_Game, KM_Main, KM_GameCursor,
  KM_Resource, KM_TerrainDeposits, KM_ResCursors, KM_GameApp,
  KM_AIDefensePos, KM_RenderUI, KM_ResFonts;

const
  GROUP_IMG: array [TGroupType] of Word = (
    371, 374,
    376, 377);


{ TKMapEdInterface }
constructor TKMapEdInterface.Create(aScreenX, aScreenY: Word);
var
  I: Integer;
  S: TKMShape;
begin
  inherited;

  fDragScrolling := False;
  fDragScrollingCursorPos.X := 0;
  fDragScrollingCursorPos.Y := 0;
  fDragScrollingViewportPos.X := 0;
  fDragScrollingViewportPos.Y := 0;

  TKMImage.Create(Panel_Main, 0,    0, 224, 200, 407); //Minimap place
  TKMImage.Create(Panel_Main, 0,  200, 224, 400, 404);
  TKMImage.Create(Panel_Main, 0,  600, 224, 400, 404);
  TKMImage.Create(Panel_Main, 0, 1000, 224, 400, 404); //For 1600x1200 this is needed

  MinimapView := TKMMinimapView.Create(Panel_Main, 10, 10, 176, 176);
  MinimapView.OnChange := Minimap_OnUpdate;

  Label_MissionName := TKMLabel.Create(Panel_Main, 230, 10, 184, 10, NO_TEXT, fnt_Grey, taLeft);
  Label_Coordinates := TKMLabel.Create(Panel_Main, 230, 30, 'X: Y:', fnt_Grey, taLeft);
  Label_Stat := TKMLabel.Create(Panel_Main, 230, 50, 0, 0, '', fnt_Outline, taLeft);

  TKMLabel.Create(Panel_Main, TB_PAD, 190, TB_WIDTH, 0, gResTexts[TX_MAPED_PLAYERS], fnt_Outline, taLeft);
  for I := 0 to MAX_PLAYERS - 1 do
  begin
    Button_PlayerSelect[I]         := TKMFlatButtonShape.Create(Panel_Main, 8 + I*23, 210, 21, 21, IntToStr(I+1), fnt_Grey, $FF0000FF);
    Button_PlayerSelect[I].Tag     := I;
    Button_PlayerSelect[I].OnClick := Player_ActiveClick;
  end;
  Button_PlayerSelect[0].Down := True; //First player selected by default

  Image_Extra := TKMImage.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - 48, 30, 48, 494);
  Image_Extra.Anchors := [akLeft, akBottom];
  Image_Extra.HighlightOnMouseOver := True;
  Image_Extra.OnClick := Message_Click;
  Image_Message := TKMImage.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - 48*2, 30, 48, 496);
  Image_Message.Anchors := [akLeft, akBottom];
  Image_Message.HighlightOnMouseOver := True;
  Image_Message.OnClick := Message_Click;
  Image_Message.Hide; //Hidden by default, only visible when a message is shown

  //Must be created before Hint so it goes over them
  fGuiExtras := TKMMapEdExtras.Create(Panel_Main, PageChanged);
  fGuiMessage := TKMMapEdMessage.Create(Panel_Main);

  Panel_Common := TKMPanel.Create(Panel_Main,TB_PAD,255,TB_WIDTH,768);

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
  fGuiMenu := TKMMapEdMenu.Create(Panel_Common);

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
  fGuiTown.fGuiDefence.FormationsPopUp := fGuiFormations;
  fGuiTown.fGuiOffence.AttackPopUp := fGuiAttack;
  fGuiPlayer.fGuiPlayerGoals.GoalPopUp := fGuiGoal;

  //Hints go above everything
  Bevel_HintBG := TKMBevel.Create(Panel_Main,224+32,Panel_Main.Height-23,300,21);
  Bevel_HintBG.BackAlpha := 0.5;
  Bevel_HintBG.Hide;
  Bevel_HintBG.Anchors := [akLeft, akBottom];
  Label_Hint := TKMLabel.Create(Panel_Main, 224 + 36, Panel_Main.Height - 21, 0, 0, '', fnt_Outline, taLeft);
  Label_Hint.Anchors := [akLeft, akBottom];

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

  SHOW_TERRAIN_WIRES := false; //Don't show it in-game if they left it on in MapEd
  SHOW_TERRAIN_PASS := 0; //Don't show it in-game if they left it on in MapEd
  inherited;
end;


procedure TKMapEdInterface.Main_ButtonClick(Sender: TObject);
begin
  //Reset cursor mode
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  //Reset shown item when user clicks on any of the main buttons
  MySpectator.Selected := nil;

  HidePages;

  if (Sender = Button_Main[1]) then fGuiTerrain.Show(ttBrush) else
  if (Sender = Button_Main[2]) then fGuiTown.Show(ttHouses) else
  if (Sender = Button_Main[3]) then fGuiPlayer.Show(ptGoals) else
  if (Sender = Button_Main[4]) then fGuiMission.Show(mtMode) else
  if (Sender = Button_Main[5]) then fGuiMenu.Show;
end;


procedure TKMapEdInterface.HidePages;
var I,K: Integer;
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
    Bevel_HintBG.Width := 8 + fResource.Fonts.GetTextSize(Label_Hint.Caption, Label_Hint.Font).X;
  end;

  fPrevHint := Sender;
end;


//Should update any items changed by game (resource counts, hp, etc..)
procedure TKMapEdInterface.UpdateState(aTickCount: Cardinal);
const
  CAP_COLOR: array [Boolean] of Cardinal = ($80808080, $FFFFFFFF);
var
  I: Integer;
begin
  //Show players without assets in grey
  if aTickCount mod 10 = 0 then
  for I := 0 to MAX_PLAYERS - 1 do
    Button_PlayerSelect[I].FontColor := CAP_COLOR[gPlayers[I].HasAssets];

  fGuiTerrain.UpdateState;
  fGuiMenu.UpdateState;
end;


//Update UI state according to game state
procedure TKMapEdInterface.SyncUI;
var
  I: Integer;
begin
  //Set player colors
  for I := 0 to MAX_PLAYERS - 1 do
    Button_PlayerSelect[I].ShapeColor := gPlayers[I].FlagColor;

  Player_UpdatePages;

  Label_MissionName.Caption := fGame.GameName;

  MinimapView.SetMinimap(fGame.Minimap);
  MinimapView.SetViewport(fGame.Viewport);
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
  if fGame = nil then Exit; //Happens on init

  fGame.MapEditor.VisibleLayers := [];

  if fGuiPlayer.Visible(ptView) or fGuiMarkerReveal.Visible then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlRevealFOW, mlCenterScreen];

  if fGuiTown.Visible(ttDefences) or fGuiMarkerDefence.Visible then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlDefences];

  if fGuiExtras.CheckBox_ShowObjects.Checked or fGuiTerrain.Visible(ttObject) then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlObjects];

  if fGuiExtras.CheckBox_ShowHouses.Checked or fGuiTown.Visible(ttHouses) or fGuiHouse.Visible then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlHouses];

  if fGuiExtras.CheckBox_ShowUnits.Checked or fGuiTown.Visible(ttUnits) or fGuiUnit.Visible then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlUnits];

  if fGuiTerrain.Visible(ttSelection) then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlSelection];

  if fGuiExtras.CheckBox_ShowDeposits.Checked then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlDeposits];
end;


procedure TKMapEdInterface.Player_ActiveClick(Sender: TObject);
begin
  //Hide player-specific pages
  fGuiHouse.Hide;
  fGuiUnit.Hide;
  fGuiMarkerDefence.Hide;
  fGuiMarkerReveal.Hide;

  if MySpectator.Selected <> nil then
    MySpectator.Selected := nil;

  Player_SetActive(TKMControl(Sender).Tag);
end;


//Active player can be set either from buttons clicked or by selecting a unit or a house
procedure TKMapEdInterface.Player_SetActive(aIndex: TPlayerIndex);
var
  I: Integer;
begin
  MySpectator.PlayerIndex := aIndex;

  for I := 0 to MAX_PLAYERS - 1 do
    Button_PlayerSelect[I].Down := (I = MySpectator.PlayerIndex);

  Player_UpdatePages;
end;


procedure TKMapEdInterface.ShowMarkerInfo(aMarker: TKMMapEdMarker);
begin
  //fGame.MapEditor.ActiveMarker := aMarker;
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
  if Sender = fGuiMarkerReveal then
  begin
    HidePages;
    fGuiPlayer.Show(ptView);
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

  //Reset cursor
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;
end;


procedure TKMapEdInterface.Player_UpdatePages;
begin
  //Update players info on pages
  //Colors are updated as well

  if fGuiTown.Visible then fGuiTown.ChangePlayer;
  if fGuiPlayer.Visible then fGuiPlayer.ChangePlayer;
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
  fGame.Viewport.Position := KMPointF(X,Y);
end;


procedure TKMapEdInterface.ExportPages(aPath: string);
var
  I: TKMTerrainTab;
  K: TKMTownTab;
  L: TKMPlayerTab;
  M: TKMMissionTab;
begin
  ForceDirectories(aPath);

  for I := Low(TKMTerrainTab) to High(TKMTerrainTab) do
  begin
    HidePages;
    fGuiTerrain.Show(I);
    fGameApp.PrintScreen(aPath + 'Terrain' + IntToStr(Byte(I)) + '.jpg');
  end;

  for K := Low(TKMTownTab) to High(TKMTownTab) do
  begin
    HidePages;
    fGuiTown.Show(K);
    fGameApp.PrintScreen(aPath + 'Town' + IntToStr(Byte(K)) + '.jpg');
  end;

  for L := Low(TKMPlayerTab) to High(TKMPlayerTab) do
  begin
    HidePages;
    fGuiPlayer.Show(L);
    fGameApp.PrintScreen(aPath + 'Player' + IntToStr(Byte(L)) + '.jpg');
  end;

  for M := Low(TKMMissionTab) to High(TKMMissionTab) do
  begin
    HidePages;
    fGuiMission.Show(M);
    fGameApp.PrintScreen(aPath + 'Mission' + IntToStr(Byte(M)) + '.jpg');
  end;

  HidePages;
  fGuiHouse.Show(nil);
  fGameApp.PrintScreen(aPath + 'House.jpg');

  HidePages;
  fGuiUnit.Show(TKMUnit(nil));
  fGameApp.PrintScreen(aPath + 'Unit.jpg');
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
    Message_Click(Image_Extra);

  if Key = VK_ESCAPE then
  begin
    fGuiMessage.Hide;
    fGuiExtras.Hide;
  end;

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

  if fMyControls.CtrlOver <> nil then
    Exit;

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

  if Button = mbRight then
    RightClick_Cancel;

  //So terrain brushes start on mouse down not mouse move
  fGame.UpdateGameCursor(X, Y, Shift);

  fGame.MapEditor.MouseDown(Button);
end;


procedure TKMapEdInterface.MouseMove(Shift: TShiftState; X,Y: Integer);
var
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
    if MySpectator.HitTestCursor <> nil then
      fResource.Cursors.Cursor := kmc_Info
    else
    if not fGame.Viewport.Scrolling then
      fResource.Cursors.Cursor := kmc_Default;
  end;

  Label_Coordinates.Caption := Format('X: %d, Y: %d', [GameCursor.Cell.X, GameCursor.Cell.Y]);

  fGame.MapEditor.MouseMove;
end;


procedure TKMapEdInterface.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  DP: TAIDefencePosition;
  Marker: TKMMapEdMarker;
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

  case Button of
    mbLeft:   if GameCursor.Mode = cmNone then
              begin
                //If there are some additional layers we first HitTest them
                //since they are rendered ontop of Houses/Objects
                Marker := fGame.MapEditor.HitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
                if Marker.MarkerType <> mtNone then
                  ShowMarkerInfo(Marker)
                else
                begin
                  MySpectator.UpdateSelect;

                  if MySpectator.Selected is TKMHouse then
                  begin
                    HidePages;
                    Player_SetActive(TKMHouse(MySpectator.Selected).Owner);
                    fGuiHouse.Show(TKMHouse(MySpectator.Selected));
                  end;
                  if MySpectator.Selected is TKMUnit then
                  begin
                    HidePages;
                    Player_SetActive(TKMUnit(MySpectator.Selected).Owner);
                    fGuiUnit.Show(TKMUnit(MySpectator.Selected));
                  end;
                  if MySpectator.Selected is TKMUnitGroup then
                  begin
                    HidePages;
                    Player_SetActive(TKMUnitGroup(MySpectator.Selected).Owner);
                    fGuiUnit.Show(TKMUnitGroup(MySpectator.Selected));
                  end;
                end;
              end;
    mbRight:  begin
                //Right click performs some special functions and shortcuts
                if GameCursor.Mode = cmTiles then
                  GameCursor.MapEdDir := (GameCursor.MapEdDir + 1) mod 4; //Rotate tile direction

                //Move the selected object to the cursor location
                if MySpectator.Selected is TKMHouse then
                  TKMHouse(MySpectator.Selected).SetPosition(GameCursor.Cell); //Can place is checked in SetPosition

                if MySpectator.Selected is TKMUnit then
                  TKMUnit(MySpectator.Selected).SetPosition(GameCursor.Cell);

                if MySpectator.Selected is TKMUnitGroup then
                  TKMUnitGroup(MySpectator.Selected).Position := GameCursor.Cell;

                if fGuiMarkerDefence.Visible then
                begin
                  DP := gPlayers[fGuiMarkerDefence.Owner].AI.General.DefencePositions[fGuiMarkerDefence.Index];
                  DP.Position := KMPointDir(GameCursor.Cell, DP.Position.Dir);
                end;

                if fGuiMarkerReveal.Visible then
                  fGame.MapEditor.Revealers[fGuiMarkerReveal.Owner][fGuiMarkerReveal.Index] := GameCursor.Cell;
              end;
  end;

  fGame.UpdateGameCursor(X, Y, Shift); //Updates the shift state

  fGame.MapEditor.MouseUp(Button);
end;


//UI should paint only controls
procedure TKMapEdInterface.Paint;
  procedure PaintTextInShape(aText: string; X,Y: SmallInt; aLineColor: Cardinal);
  var
    W: Integer;
  begin
    //Paint the background
    W := 10 + 10 * Length(aText);
    TKMRenderUI.WriteShape(X - W div 2, Y - 10, W, 20, $80000000);
    TKMRenderUI.WriteOutline(X - W div 2, Y - 10, W, 20, 2, aLineColor);

    //Paint the label on top of the background
    TKMRenderUI.WriteText(X, Y - 7, 0, aText, fnt_Metal, taCenter, $FFFFFFFF);
  end;
const
  DefenceLine: array [TAIDefencePosType] of Cardinal = ($FF80FF00, $FFFF8000);
var
  I, K: Integer;
  R: TRawDeposit;
  DP: TAIDefencePosition;
  LocF: TKMPointF;
  ScreenLoc: TKMPointI;
begin
  if mlDeposits in fGame.MapEditor.VisibleLayers then
  begin
    for R := Low(TRawDeposit) to High(TRawDeposit) do
      for I := 0 to fGame.MapEditor.Deposits.Count[R] - 1 do
      //Ignore water areas with 0 fish in them
      if fGame.MapEditor.Deposits.Amount[R, I] > 0 then
      begin
        LocF := gTerrain.FlatToHeight(fGame.MapEditor.Deposits.Location[R, I]);
        ScreenLoc := fGame.Viewport.MapToScreen(LocF);

        //At extreme zoom coords may become out of range of SmallInt used in controls painting
        if KMInRect(ScreenLoc, fGame.Viewport.ViewRect) then
          PaintTextInShape(IntToStr(fGame.MapEditor.Deposits.Amount[R, I]), ScreenLoc.X, ScreenLoc.Y, DEPOSIT_COLORS[R]);
      end;
  end;

  if mlDefences in fGame.MapEditor.VisibleLayers then
  begin
    for I := 0 to gPlayers.Count - 1 do
      for K := 0 to gPlayers[I].AI.General.DefencePositions.Count - 1 do
      begin
        DP := gPlayers[I].AI.General.DefencePositions[K];
        LocF := gTerrain.FlatToHeight(KMPointF(DP.Position.Loc.X-0.5, DP.Position.Loc.Y-0.5));
        ScreenLoc := fGame.Viewport.MapToScreen(LocF);

        if KMInRect(ScreenLoc, fGame.Viewport.ViewRect) then
        begin
          PaintTextInShape(IntToStr(K+1), ScreenLoc.X, ScreenLoc.Y - 15, DefenceLine[DP.DefenceType]);
          TKMRenderUI.WritePicture(ScreenLoc.X, ScreenLoc.Y, 0, 0, [], rxGui, GROUP_IMG[DP.GroupType]);
        end;
      end;
  end;

  inherited;
end;


end.
