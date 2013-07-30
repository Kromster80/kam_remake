unit KM_InterfaceMapEditor;
{$I KaM_Remake.inc}
interface
uses
   {$IFDEF MSWindows} Windows, {$ENDIF}
   {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
   Classes, Controls, KromUtils, Math, StrUtils, SysUtils, KromOGLUtils, TypInfo,
   KM_Controls, KM_Defaults, KM_Pics, KM_Houses, KM_Units, KM_UnitGroups, KM_MapEditor,
   KM_Points, KM_InterfaceDefaults, KM_Terrain,
   KM_GUIMapEdHouse,
   KM_GUIMapEdGoal,
   KM_GUIMapEdTerrain,
   KM_GUIMapEdTown,
   KM_GUIMapEdPlayer,
   KM_GUIMapEdMission,
   KM_GUIMapEdAttack,
   KM_GUIMapEdFormations,
   KM_GUIMapEdMarkerDefence,
   KM_GUIMapEdMarkerReveal,
   KM_GUIMapEdMenu,
   KM_GUIMapEdUnit;

type
  TKMapEdInterface = class (TKMUserInterface)
  private
    fPrevHint: TObject;
    fActivePage: TKMPanel;
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
    fGuiMarkerDefence: TKMMapEdMarkerDefence;
    fGuiMarkerReveal: TKMMapEdMarkerReveal;
    fGuiMenu: TKMMapEdMenu;

    procedure Create_Extra;
    procedure Create_Message;

    procedure Extra_Change(Sender: TObject);
    procedure Layers_UpdateVisibility;
    procedure Marker_Done(Sender: TObject);
    procedure Minimap_Update(Sender: TObject; const X,Y: Integer);
    procedure PageChanged(Sender: TObject);
    procedure Player_ChangeActive(Sender: TObject);
    procedure Player_UpdateColors;
    procedure Player_FOWChange(Sender: TObject);
    procedure ExtraMessage_Switch(Sender: TObject);

    procedure SwitchPage(Sender: TObject);
    procedure HidePages;
    procedure DisplayPage(aPage: TKMPanel);
    procedure DisplayHint(Sender: TObject);
    procedure RightClick_Cancel;
    procedure ShowMarkerInfo(aMarker: TKMMapEdMarker);
    procedure SetActivePlayer(aIndex: TPlayerIndex);
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

    //How to know where certain page should be?
    //see Docs\Map Editor menu structure.txt

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
  public
    constructor Create(aScreenX, aScreenY: word);
    destructor Destroy; override;

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


const
  BIG_TAB_W = 37;
  BIG_PAD_W = 37;
  BIG_TAB_H = 36;
  //BIG_PAD_H = 40;
  SMALL_TAB_W = 32;
  SMALL_PAD_W = SMALL_TAB_W + 0;
  SMALL_TAB_H = 26;
  //SMALL_PAD_H = SMALL_TAB_H + 4;


implementation
uses
  KM_PlayersCollection, KM_ResTexts, KM_Game, KM_Main, KM_GameCursor,
  KM_Resource, KM_TerrainDeposits, KM_ResCursors, KM_Utils,
  KM_AIDefensePos, KM_RenderUI, KM_Sound, KM_ResSound,
  KM_ResFonts;

const
  GROUP_TEXT: array [TGroupType] of Integer = (
    TX_MAPED_AI_ATTACK_TYPE_MELEE, TX_MAPED_AI_ATTACK_TYPE_ANTIHORSE,
    TX_MAPED_AI_ATTACK_TYPE_RANGED, TX_MAPED_AI_ATTACK_TYPE_MOUNTED);

  GROUP_IMG: array [TGroupType] of Word = (
    371, 374,
    376, 377);


{ TKMapEdInterface }
constructor TKMapEdInterface.Create(aScreenX, aScreenY: Word);
var
  I: Integer;
begin
  inherited;

  fDragScrolling := False;
  fDragScrollingCursorPos.X := 0;
  fDragScrollingCursorPos.Y := 0;
  fDragScrollingViewportPos.X := 0;
  fDragScrollingViewportPos.Y := 0;

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

    TKMLabel.Create(Panel_Main, TB_PAD, 190, TB_WIDTH, 0, gResTexts[TX_MAPED_PLAYERS], fnt_Outline, taLeft);
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
    Button_Main[1].Hint := gResTexts[TX_MAPED_TERRAIN];
    Button_Main[2].Hint := gResTexts[TX_MAPED_VILLAGE];
    Button_Main[3].Hint := gResTexts[TX_MAPED_SCRIPTS_VISUAL];
    Button_Main[4].Hint := gResTexts[TX_MAPED_SCRIPTS_GLOBAL];
    Button_Main[5].Hint := gResTexts[TX_MAPED_MENU];
    for I := 1 to 5 do
      Button_Main[I].OnClick := SwitchPage;

{I plan to store all possible layouts on different pages which gets displayed one at a time}
{==========================================================================================}
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
  fGuiAttack := TKMMapEdAttack.Create(Panel_Main);
  fGuiFormations := TKMMapEdFormations.Create(Panel_Main);
  fGuiGoal := TKMMapEdGoal.Create(Panel_Main);

  //Pass pop-ups to their dispatchers
  fGuiTown.fGuiDefence.FormationsPopUp := fGuiFormations;
  fGuiTown.fGuiOffence.AttackPopUp := fGuiAttack;
  fGuiPlayer.fGuiPlayerGoals.GoalPopUp := fGuiGoal;

  fMyControls.OnHint := DisplayHint;

  DisplayPage(nil); //Update
end;


destructor TKMapEdInterface.Destroy;
begin
  fGuiHouse.Free;
  fGuiTerrain.Free;
  fGuiTown.Free;
  fGuiPlayer.Free;
  fGuiMission.Free;
  fGuiAttack.Free;
  fGuiFormations.Free;
  fGuiGoal.Free;
  fGuiMarkerDefence.Free;
  fGuiMarkerReveal.Free;
  fGuiMenu.Free;

  SHOW_TERRAIN_WIRES := false; //Don't show it in-game if they left it on in MapEd
  SHOW_TERRAIN_PASS := 0; //Don't show it in-game if they left it on in MapEd
  inherited;
end;


{Switch between pages}
procedure TKMapEdInterface.SwitchPage(Sender: TObject);
begin
  //Reset cursor mode
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  //If the user clicks on the tab that is open, it closes it (main buttons only)
  if ((Sender = Button_Main[1]) and fGuiTerrain.Visible) or
     ((Sender = Button_Main[2]) and fGuiTown.Visible) or
     ((Sender = Button_Main[3]) and fGuiPlayer.Visible) or
     ((Sender = Button_Main[4]) and fGuiMission.Visible) or
     ((Sender = Button_Main[5]) and fGuiMenu.Visible) then
    Sender := nil;

  //Reset shown item if user clicked on any of the main buttons
  if (Sender=Button_Main[1])or(Sender=Button_Main[2])or
     (Sender=Button_Main[3])or(Sender=Button_Main[4])or
     (Sender=Button_Main[5]) then
    MySpectator.Selected := nil;

  if (Sender = Button_Main[1]) then
  begin
    HidePages;
    fGuiTerrain.Show;
  end
  else

  if (Sender = Button_Main[2]) then
  begin
    HidePages;
    fGuiTown.Show(ttHouses);
  end
  else

  if (Sender = Button_Main[3]) then
  begin
    HidePages;
    fGuiPlayer.Show(ptGoals);
  end
  else

  if (Sender = Button_Main[4]) then
  begin
    HidePages;
    fGuiMission.Show(mtMode);
  end
  else

  if (Sender = Button_Main[5]) then
    fGuiMenu.Show;
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


procedure TKMapEdInterface.DisplayPage(aPage: TKMPanel);
begin
  HidePages;

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


//Update Hint position and etc..
procedure TKMapEdInterface.Resize(X,Y: Word);
begin
  Panel_Main.Width := X;
  Panel_Main.Height := Y;
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
    Image_ExtraClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
    Image_ExtraClose.OnClick := ExtraMessage_Switch;
    Image_ExtraClose.HighlightOnMouseOver := True;

    TrackBar_Passability := TKMTrackBar.Create(Panel_Extra, 50, 70, 180, 0, Byte(High(TPassability)));
    TrackBar_Passability.Font := fnt_Antiqua;
    TrackBar_Passability.Caption := gResTexts[TX_MAPED_VIEW_PASSABILITY];
    TrackBar_Passability.Position := 0; //Disabled by default
    TrackBar_Passability.OnChange := Extra_Change;
    Label_Passability := TKMLabel.Create(Panel_Extra, 50, 114, 180, 0, gResTexts[TX_MAPED_PASSABILITY_OFF], fnt_Antiqua, taLeft);

    CheckBox_ShowObjects := TKMCheckBox.Create(Panel_Extra, 250, 70, 180, 20, gResTexts[TX_MAPED_VIEW_OBJECTS], fnt_Antiqua);
    CheckBox_ShowObjects.Checked := True; //Enabled by default
    CheckBox_ShowObjects.OnClick := Extra_Change;
    CheckBox_ShowHouses := TKMCheckBox.Create(Panel_Extra, 250, 90, 180, 20, gResTexts[TX_MAPED_VIEW_HOUSES], fnt_Antiqua);
    CheckBox_ShowHouses.Checked := True; //Enabled by default
    CheckBox_ShowHouses.OnClick := Extra_Change;
    CheckBox_ShowUnits := TKMCheckBox.Create(Panel_Extra, 250, 110, 180, 20, gResTexts[TX_MAPED_VIEW_UNITS], fnt_Antiqua);
    CheckBox_ShowUnits.Checked := True; //Enabled by default
    CheckBox_ShowUnits.OnClick := Extra_Change;
    CheckBox_ShowDeposits := TKMCheckBox.Create(Panel_Extra, 250, 130, 180, 20, gResTexts[TX_MAPED_VIEW_DEPOSISTS], fnt_Antiqua);
    CheckBox_ShowDeposits.Checked := True; //Enabled by default
    CheckBox_ShowDeposits.OnClick := Extra_Change;

    //dropdown list needs to be ontop other buttons created on Panel_Main
    Dropbox_PlayerFOW := TKMDropList.Create(Panel_Extra, 460, 70, 160, 20, fnt_Metal, '', bsGame);
    Dropbox_PlayerFOW.Hint := gResTexts[TX_REPLAY_PLAYER_PERSPECTIVE];
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
    Image_MessageClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
    Image_MessageClose.OnClick := ExtraMessage_Switch;
    Image_MessageClose.HighlightOnMouseOver := True;

    Label_Message := TKMLabel.Create(Panel_Message, 40, 50, 7000, 0, '', fnt_Grey, taLeft);
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
    Button_PlayerSelect[I].FontColor := CAP_COLOR[gPlayers[I].HasAssets];

  fGuiTerrain.UpdateState;
  fGuiMenu.UpdateState;
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


procedure TKMapEdInterface.PageChanged(Sender: TObject);
begin
  //Child panels visibility changed, that affects visible layers
  Layers_UpdateVisibility;
end;


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


procedure TKMapEdInterface.Player_UpdateColors;
var
  I: Integer;
  PrevIndex: Integer;
begin
  //Set player colors
  for I := 0 to MAX_PLAYERS - 1 do
    Button_PlayerSelect[I].ShapeColor := gPlayers[I].FlagColor;

  //Update pages that have colored elements to match new players color
  fGuiTown.UpdatePlayer(MySpectator.PlayerIndex);

  PrevIndex := Dropbox_PlayerFOW.ItemIndex;
  Dropbox_PlayerFOW.Clear;
  Dropbox_PlayerFOW.Add('Show all', -1);
  for I := 0 to MAX_PLAYERS - 1 do
    Dropbox_PlayerFOW.Add('[$' + IntToHex(FlagColorToTextColor(gPlayers[I].FlagColor) and $00FFFFFF, 6) + ']' + gPlayers[I].GetFormattedPlayerName, I);
  if PrevIndex = -1 then
    PrevIndex := 0; //Select Show All
  Dropbox_PlayerFOW.ItemIndex := PrevIndex;
end;


procedure TKMapEdInterface.Player_ChangeActive(Sender: TObject);
begin
  //If we had selected any player specific page - discard them
  fGuiHouse.Hide;
  fGuiUnit.Hide;
  fGuiMarkerDefence.Hide;
  fGuiMarkerReveal.Hide;

  if MySpectator.Selected <> nil then
    MySpectator.Selected := nil;

  SetActivePlayer(TKMControl(Sender).Tag);

  //Refresh per-player settings
  DisplayPage(fActivePage);
end;


procedure TKMapEdInterface.SetActivePlayer(aIndex: TPlayerIndex);
var
  I: Integer;
begin
  MySpectator.PlayerIndex := aIndex;

  for I := 0 to MAX_PLAYERS - 1 do
    Button_PlayerSelect[I].Down := (I = MySpectator.PlayerIndex);

  Player_UpdateColors;
  UpdateAITabsEnabled;
end;


procedure TKMapEdInterface.Extra_Change(Sender: TObject);
begin
  SHOW_TERRAIN_WIRES := TrackBar_Passability.Position <> 0;
  SHOW_TERRAIN_PASS := TrackBar_Passability.Position;

  if TrackBar_Passability.Position <> 0 then
    Label_Passability.Caption := GetEnumName(TypeInfo(TPassability), SHOW_TERRAIN_PASS)
  else
    Label_Passability.Caption := gResTexts[TX_MAPED_PASSABILITY_OFF];

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

  if CheckBox_ShowObjects.Checked or fGuiTerrain.Visible(ttObject) then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlObjects];

  if CheckBox_ShowHouses.Checked or fGuiTown.Visible(ttHouses) or fGuiHouse.Visible then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlHouses];

  if CheckBox_ShowUnits.Checked or fGuiTown.Visible(ttUnits) or fGuiUnit.Visible then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlUnits];

  if fGuiTerrain.Visible(ttSelection) then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlSelection];

  if CheckBox_ShowDeposits.Checked then
    fGame.MapEditor.VisibleLayers := fGame.MapEditor.VisibleLayers + [mlDeposits];
end;


procedure TKMapEdInterface.ShowMarkerInfo(aMarker: TKMMapEdMarker);
begin
  //fGame.MapEditor.ActiveMarker := aMarker;

  if (aMarker.MarkerType = mtNone) or (aMarker.Owner = PLAYER_NONE) or (aMarker.Index = -1) then
  begin
    DisplayPage(nil);
    Exit;
  end;

  SetActivePlayer(aMarker.Owner);

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
  Label_Message.Caption := aText;
  Panel_Message.Show;
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

  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  DisplayPage(fActivePage);
end;


procedure TKMapEdInterface.UpdateAITabsEnabled;
begin
  fGuiTown.UpdatePlayer(MySpectator.PlayerIndex);
end;


procedure TKMapEdInterface.ExtraMessage_Switch(Sender: TObject);
begin
  //Don't use DisplayPage because that hides other stuff
  if Sender = Image_Extra then
  begin
    if Panel_Extra.Visible then
    begin
      Panel_Extra.Hide;
      gSoundPlayer.Play(sfxn_MPChatClose);
    end
    else
    begin
      Panel_Extra.Show;
      Panel_Message.Hide;
      gSoundPlayer.Play(sfxn_MPChatOpen);
    end;
  end
  else
  if Sender = Image_ExtraClose then
  begin
    Panel_Extra.Hide;
    gSoundPlayer.Play(sfxn_MPChatClose);
  end;
  if Sender = Image_Message then
  begin
    if Panel_Message.Visible then
    begin
      Panel_Message.Hide;
      gSoundPlayer.Play(sfxn_MPChatClose);
    end
    else
    begin
      Panel_Message.Show;
      Panel_Extra.Hide;
      gSoundPlayer.Play(sfxn_MPChatOpen);
    end;
  end
  else
  if Sender = Image_MessageClose then
  begin
    Panel_Message.Hide;
    gSoundPlayer.Play(sfxn_MPChatClose);
  end;
end;


procedure TKMapEdInterface.Player_FOWChange(Sender: TObject);
begin
  MySpectator.FOWIndex := Dropbox_PlayerFOW.GetTag(Dropbox_PlayerFOW.ItemIndex);
  fGame.Minimap.Update(False); //Force update right now so FOW doesn't appear to lag
end;


//Update viewport position when user interacts with minimap
procedure TKMapEdInterface.Minimap_Update(Sender: TObject; const X,Y: Integer);
begin
  fGame.Viewport.Position := KMPointF(X,Y);
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
                    SetActivePlayer(TKMHouse(MySpectator.Selected).Owner);
                    fGuiHouse.Show(TKMHouse(MySpectator.Selected));
                  end;
                  if MySpectator.Selected is TKMUnit then
                  begin
                    HidePages;
                    SetActivePlayer(TKMUnit(MySpectator.Selected).Owner);
                    fGuiUnit.Show(TKMUnit(MySpectator.Selected));
                  end;
                  if MySpectator.Selected is TKMUnitGroup then
                  begin
                    HidePages;
                    SetActivePlayer(TKMUnitGroup(MySpectator.Selected).Owner);
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


end.
