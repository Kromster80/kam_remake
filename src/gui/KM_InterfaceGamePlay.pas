unit KM_InterfaceGamePlay;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  StrUtils, SysUtils, KromUtils, Math, Classes, Controls,
  KM_Controls, KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Pics, KM_Points,
  KM_InterfaceDefaults, KM_InterfaceGame, KM_Terrain, KM_Houses, KM_Units, KM_Minimap, KM_Viewport, KM_Render,
  KM_UnitGroups, KM_Units_Warrior, KM_Saves, KM_MessageStack, KM_ResHouses, KM_Alerts, KM_Networking,
  KM_GUIGameBuild, KM_GUIGameChat, KM_GUIGameHouse, KM_GUIGameRatios, KM_GUIGameStats,KM_GUIGameMenuSettings;


const
  MAX_VISIBLE_MSGS = 32;
  MAX_LOG_MSGS = 8;

type
  TKMTabButtons = (tbBuild, tbRatio, tbStats, tbMenu);
  TUIMode = (umSP, umMP, umReplay, umSpectate);

  TKMGamePlayInterface = class (TKMUserInterfaceGame)
  private
    fAlerts: TKMAlerts;

    fUIMode: TUIMode;
    fSave_Selected: Integer; // Save selected from list (needed because of scanning)

    fGuiGameBuild: TKMGUIGameBuild;
    fGuiGameChat: TKMGUIGameChat;
    fGuiGameHouse: TKMGUIGameHouse;
    fGuiGameRatios: TKMGUIGameRatios;
    fGuiGameStats: TKMGUIGameStats;
    fGuiMenuSettings: TKMGameMenuSettings;

    // Not saved
    fShowTeamNames: Boolean; // True while the SC_SHOW_TEAM key is pressed
    LastDragPoint: TKMPoint; // Last mouse point that we drag placed/removed a road/field
    PrevHint: TObject;
    ShownMessage: Integer;
    PlayMoreMsg: TGameResultMsg; // Remember which message we are showing
    fJoiningGroups, fPlacingBeacon: Boolean;
    fAskDismiss: Boolean;
    fDragScrolling: Boolean;
    fDragScrollingCursorPos: TPoint;
    fDragScrollingViewportPos: TKMPointF;
    fNetWaitDropPlayersDelayStarted: Cardinal;
    SelectedDirection: TKMDirection;
    SelectingTroopDirection: Boolean;
    SelectingDirPosition: TPoint;
    fSaves: TKMSavesCollection;
    fTeamNames: TList;
    Label_TeamName: TKMLabel;
    fLastSyncedMessage: Word; // Last message that we synced with MessageLog
    fAlliesToNetPlayers: array [0..MAX_LOBBY_SLOTS-1] of Integer;

    // Saved (in singleplayer only)
    fLastSaveName: UnicodeString; // The file name we last used to save this file (used as default in Save menu)
    fMessageStack: TKMMessageStack;
    fSelection: array [0..9] of Integer;

    procedure Create_Controls;
    procedure Create_Replay;
    procedure Create_Allies;
    procedure Create_Message;
    procedure Create_MessageLog;
    procedure Create_Pause;
    procedure Create_PlayMore;
    procedure Create_MPPlayMore;
    procedure Create_NetWait;
    procedure Create_MessageStack;
    procedure Create_Menu;
    procedure Create_Save;
    procedure Create_Load;
    procedure Create_Quit;
    procedure Create_Unit;

    procedure Beacon_Cancel;
    procedure Beacon_Place(aLoc: TKMPointF);
    procedure Army_ActivateControls(aGroup: TKMUnitGroup);
    procedure Army_HideJoinMenu(Sender: TObject);
    procedure Army_Issue_Order(Sender: TObject);
    procedure Chat_Click(Sender: TObject);
    procedure House_Demolish;
    procedure Unit_Dismiss(Sender: TObject);
    procedure Menu_QuitMission(Sender: TObject);
    procedure Menu_NextTrack(Sender: TObject);
    procedure Menu_PreviousTrack(Sender: TObject);
    procedure Allies_Click(Sender: TObject);
    procedure Allies_Show(Sender: TObject);
    procedure MessageStack_UpdatePositions;
    procedure Message_Click(Sender: TObject);
    procedure Message_Close(Sender: TObject);
    procedure Message_Delete(Sender: TObject);
    procedure Message_Show(aIndex: Integer);
    procedure Message_GoTo(Sender: TObject);
    procedure Message_UpdateStack;
    procedure MessageLog_Click(Sender: TObject);
    procedure MessageLog_ItemClick(Sender: TObject);
    procedure MessageLog_Close(Sender: TObject);
    procedure MessageLog_Update(aFullRefresh: Boolean);
    procedure Minimap_Update(Sender: TObject; const X,Y:integer);
    procedure Minimap_RightClick(Sender: TObject; const X,Y:integer);
    procedure Minimap_Click(Sender: TObject; const X,Y:integer);

    procedure Menu_Save_RefreshList(Sender: TObject);
    procedure Menu_Save_ListChange(Sender: TObject);
    procedure Menu_Save_EditChange(Sender: TObject);
    procedure Menu_Save_CheckboxChange(Sender: TObject);
    procedure Menu_Save_Click(Sender: TObject);
    procedure Menu_Load_RefreshList(Sender: TObject);
    procedure Menu_Load_ListClick(Sender: TObject);
    procedure Menu_Load_Click(Sender: TObject);
    procedure Selection_Assign(aId: Word; aObject: TObject);
    procedure Selection_Link(aId: Word; aObject: TObject);
    procedure Selection_Select(aId: Word);
    procedure SwitchPage(Sender: TObject);
    procedure DisplayHint(Sender: TObject);
    procedure PlayMoreClick(Sender: TObject);
    procedure MPPlayMoreClick(Sender: TObject);
    procedure NetWaitClick(Sender: TObject);
    procedure ReplayClick(Sender: TObject);
    procedure ReturnToLobbyClick(Sender: TObject);
    procedure Allies_Close(Sender: TObject);
    procedure AlliesUpdateMapping;
    procedure Menu_Update;
    procedure SetPause(aValue:boolean);
    procedure DirectionCursorShow(X,Y: Integer; Dir: TKMDirection);
    procedure DirectionCursorHide;
    function HasLostMPGame:Boolean;
    procedure UpdateDebugInfo;
    procedure HidePages;
    procedure HideOverlay(Sender: TObject);
  protected
    Sidebar_Top: TKMImage;
    Sidebar_Middle: TKMImage;
    Sidebar_Bottom: array of TKMImage;
    MinimapView: TKMMinimapView;
    Label_DebugInfo, Label_Hint: TKMLabel;
    Bevel_HintBG: TKMBevel;

    Image_MPChat, Image_MPAllies: TKMImage; // Multiplayer buttons
    Image_MessageLog: TKMImage;
    Label_MPChatUnread: TKMLabel;
    Image_Message: array[0..MAX_VISIBLE_MSGS]of TKMImage; // Queue of messages covers 32*48=1536px height
    Image_Clock: TKMImage; // Clock displayed when game speed is increased
    Label_Clock: TKMLabel;
    Label_ClockSpeedup: TKMLabel;
    Label_ScriptedOverlay: TKMLabel; // Label that can be set from script
    Button_ScriptedOverlay: TKMButton;
    Label_OverlayShow, Label_OverlayHide: TKMLabel;
    Label_MenuTitle: TKMLabel; // Displays the title of the current menu to the right of return
    Image_DirectionCursor: TKMImage;

    Panel_Controls: TKMPanel;
      Button_Main: array [TKMTabButtons] of TKMButton; // 4 common buttons + Return
      Button_Back: TKMButton;

    Panel_ReplayCtrl: TKMPanel; // Smaller Panel to contain replay controls
      PercentBar_Replay: TKMPercentBar;
      Label_Replay: TKMLabel;
      Button_ReplayRestart: TKMButton;
      Button_ReplayPause: TKMButton;
      Button_ReplayStep: TKMButton;
      Button_ReplayResume: TKMButton;
      Button_ReplayExit: TKMButton;
    Panel_ReplayFOW: TKMPanel;
      Dropbox_ReplayFOW: TKMDropList;
      Checkbox_ReplayFOW: TKMCheckBox;
    Panel_Allies: TKMPanel;
      Label_PeacetimeRemaining: TKMLabel;
      Image_AlliesHostStar: TKMImage;
      Image_AlliesFlag:array [0..MAX_LOBBY_SLOTS-1] of TKMImage;
      Label_AlliesPlayer:array [0..MAX_LOBBY_SLOTS-1] of TKMLabel;
      DropBox_AlliesTeam:array [0..MAX_LOBBY_SLOTS-1] of TKMDropList;
      Label_AlliesTeam:array [0..MAX_LOBBY_SLOTS-1] of TKMLabel;
      Label_AlliesPing:array [0..MAX_LOBBY_SLOTS-1] of TKMLabel;
      Image_AlliesClose: TKMImage;
    Panel_Message: TKMPanel;
      Label_MessageText: TKMLabel;
      Button_MessageGoTo: TKMButton;
      Button_MessageDelete: TKMButton;
      Image_MessageClose: TKMImage;
    Panel_MessageLog: TKMPanel;
      ColumnBox_MessageLog: TKMColumnBox;
      Image_MessageLogClose: TKMImage;
    Panel_Pause: TKMPanel;
      Bevel_Pause: TKMBevel;
      Image_Pause: TKMImage;
      Label_Pause1: TKMLabel;
      Label_Pause2: TKMLabel;
    Panel_PlayMore: TKMPanel;
      Bevel_PlayMore: TKMBevel;
      Panel_PlayMoreMsg: TKMPanel;
        Image_PlayMore: TKMImage;
        Label_PlayMore: TKMLabel;
        Button_PlayMore,Button_PlayQuit: TKMButton;
    Panel_MPPlayMore: TKMPanel;
      Bevel_MPPlayMore: TKMBevel;
      Image_MPPlayMore: TKMImage;
      Label_MPPlayMore: TKMLabel;
      Button_MPPlayMore,Button_MPPlayQuit: TKMButton;
    Panel_NetWait: TKMPanel;
      Bevel_NetWait: TKMBevel;
      Panel_NetWaitMsg: TKMPanel;
        Image_NetWait: TKMImage;
        Label_NetWait,Label_NetDropPlayersDelay: TKMLabel;
        Panel_NetWaitButtons: TKMPanel;
          Button_NetQuit,Button_NetDropPlayers: TKMButton;
        Panel_NetWaitConfirm: TKMPanel;
          Label_NetWaitConfirm: TKMLabel;
          Button_NetConfirmYes,Button_NetConfirmNo: TKMButton;
    Panel_Menu: TKMPanel;
      Button_Menu_Save,Button_Menu_Load,Button_Menu_ReturnLobby,Button_Menu_Settings,Button_Menu_Quit,Button_Menu_TrackUp,Button_Menu_TrackDown: TKMButton;
      Label_Menu_Track, Label_GameTime: TKMLabel;

      Panel_Save: TKMPanel;
        ListBox_Save: TKMListBox;
        Edit_Save: TKMEdit;
        Label_SaveExists: TKMLabel;
        CheckBox_SaveExists: TKMCheckBox;
        Button_Save: TKMButton;

      Panel_Load: TKMPanel;
        ListBox_Load: TKMListBox;
        Label_LoadDescription: TKMLabel;
        Button_Load: TKMButton;

      Panel_Quit: TKMPanel;
        Button_Quit_Yes, Button_Quit_No: TKMButton;

    Panel_Unit: TKMPanel;
      Label_UnitName: TKMLabel;
      Label_UnitCondition: TKMLabel;
      Label_UnitTask: TKMLabel;
      Label_UnitDescription: TKMLabel;
      ConditionBar_Unit: TKMPercentBar;
      Image_UnitPic: TKMImage;
      Button_Unit_Dismiss: TKMButton;

      Panel_Unit_Dismiss: TKMPanel;
         Label_Unit_Dismiss: TKMLabel;
         Button_Unit_DismissYes,Button_Unit_DismissNo: TKMButton;

      Panel_Army: TKMPanel;
        Button_Army_GoTo,Button_Army_Stop,Button_Army_Attack: TKMButton;
        Button_Army_RotCW,Button_Army_Storm,Button_Army_RotCCW: TKMButton;
        Button_Army_ForUp,Button_Army_ForDown: TKMButton;
        ImageStack_Army: TKMImageStack;
        Button_Army_Split,Button_Army_Join,Button_Army_Feed: TKMButton;
        Label_Army_MembersCount: TKMLabel;

      Panel_Army_JoinGroups: TKMPanel;
        Button_Army_Join_Cancel: TKMButton;
        Label_Army_Join_Message: TKMLabel;
  public
    constructor Create(aRender: TRender; aUIMode: TUIMode); reintroduce;
    destructor Destroy; override;
    procedure ShowUnitInfo(Sender: TKMUnit; aAskDismiss:boolean=false);
    procedure ShowGroupInfo(Sender: TKMUnitGroup);
    procedure MessageIssue(aKind: TKMMessageKind; aText: UnicodeString); overload;
    procedure MessageIssue(aKind: TKMMessageKind; aText: UnicodeString; aLoc: TKMPoint); overload;
    procedure SetMenuState(aTactic: Boolean);
    procedure ShowClock(aSpeed: Single);
    procedure ShowPlayMore(DoShow:boolean; Msg: TGameResultMsg);
    procedure ShowMPPlayMore(Msg: TGameResultMsg);
    procedure ShowNetworkLag(aShow: Boolean; aPlayers: TKMByteArray; IsHost: Boolean);
    procedure SetScriptedOverlay(aText: UnicodeString);
    procedure UpdateOverlayControls;
    procedure ReleaseDirectionSelector;
    function GetChatState: TChatState;
    procedure SetChatState(const aChatState: TChatState);
    procedure ChatMessage(const aData: UnicodeString);
    procedure AlliesOnPlayerSetup(Sender: TObject);
    procedure AlliesOnPingInfo(Sender: TObject);
    procedure AlliesTeamChange(Sender: TObject);
    procedure CinematicUpdate;
    procedure LoadHotkeysFromHand;

    property Alerts: TKMAlerts read fAlerts;

    procedure ExportPages(aPath: string); override;

    procedure Save(SaveStream: TKMemoryStream);
    procedure SaveMinimap(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure LoadMinimap(LoadStream: TKMemoryStream);

    procedure KeyDown(Key:Word; Shift: TShiftState); override;
    procedure KeyUp(Key:Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure Resize(X,Y: Word); override;
    procedure SyncUI(aMoveViewport: Boolean = True); override;
    procedure UpdateState(aTickCount: Cardinal); override;
    procedure UpdateStateIdle(aFrameTime: Cardinal); override;
    procedure Paint; override;
  end;


implementation
uses
  KM_Main, KM_GameInputProcess, KM_GameInputProcess_Multi, KM_AI, KM_RenderUI, KM_GameCursor,
  KM_HandsCollection, KM_Hand, KM_RenderPool, KM_ResTexts, KM_Game, KM_GameApp, KM_HouseBarracks,
  KM_Utils, KM_ResLocales, KM_ResSound, KM_Resource, KM_Log, KM_ResCursors, KM_ResFonts, KM_ResKeys,
  KM_ResSprites, KM_ResUnits, KM_ResWares, KM_FogOfWar, KM_Sound, KM_NetPlayersList, KM_MessageLog;


procedure TKMGamePlayInterface.Menu_Save_ListChange(Sender: TObject);
begin
  fSaves.Lock;
    if InRange(TKMListBox(Sender).ItemIndex, 0, fSaves.Count-1) then
    begin
      fSave_Selected := TKMListBox(Sender).ItemIndex;
      Edit_Save.Text := fSaves[ListBox_Save.ItemIndex].FileName;
      // We just selected something from the list so it exists
      CheckBox_SaveExists.Enabled := True;
      CheckBox_SaveExists.Checked := False;
      Label_SaveExists.Visible := True;
      Button_Save.Enabled := False;
    end;
  fSaves.Unlock;
end;


procedure TKMGamePlayInterface.Menu_Save_EditChange(Sender: TObject);
begin
  if (Sender <> fSaves) then
  begin
    ListBox_Save.ItemIndex := -1;
    fSave_Selected := -1;
    CheckBox_SaveExists.Enabled := FileExists(gGame.SaveName(Edit_Save.Text, 'sav', (fUIMode in [umMP, umSpectate])));
    Label_SaveExists.Visible := CheckBox_SaveExists.Enabled;
    CheckBox_SaveExists.Checked := False;
    // we should protect ourselves from empty names and whitespaces at beggining and at end of name
    Button_Save.Enabled := (not CheckBox_SaveExists.Enabled) and (Edit_Save.Text <> '') and
                           not (Edit_Save.Text[1] = ' ') and not (Edit_Save.Text[Length(Edit_Save.Text)] = ' ');
  end;
end;


procedure TKMGamePlayInterface.Menu_Save_CheckboxChange(Sender: TObject);
begin
  // we should protect ourselves from empty names and whitespaces at beggining and at end of name
  Button_Save.Enabled := CheckBox_SaveExists.Checked and (Edit_Save.Text <> '') and
                         not (Edit_Save.Text[1] = ' ') and not (Edit_Save.Text[Length(Edit_Save.Text)] = ' ');
end;


procedure TKMGamePlayInterface.Menu_Save_RefreshList(Sender: TObject);
var I, PrevTop: Integer;
begin
  PrevTop := ListBox_Save.TopIndex;
  ListBox_Save.Clear;

  if (Sender = fSaves) then
    Menu_Save_EditChange(fSaves)
  else
    Menu_Save_EditChange(nil);

  if (Sender = fSaves) then
  begin
    fSaves.Lock;
      for I := 0 to fSaves.Count - 1 do
        ListBox_Save.Add(fSaves[i].FileName);
    fSaves.Unlock;
  end;

  ListBox_Save.ItemIndex := fSave_Selected;
  ListBox_Save.TopIndex := PrevTop;
end;


procedure TKMGamePlayInterface.Menu_Save_Click(Sender: TObject);
var
  SaveName: string;
begin
  SaveName := Trim(Edit_Save.Text);
  // Edit.OnChange event happens on key up, so it's still possible for the user to click save button
  // with an invalid file name entered, if the click while still holding down a key.
  // In general it's bad to rely on events like that to ensure validity, doing check here is a good idea
  if SaveName = '' then Exit;

  fLastSaveName := SaveName; // Do this before saving so it is included in the save
  gGame.Save(SaveName, UTCNow);

  fSaves.TerminateScan; // stop scan as it is no longer needed
  SwitchPage(nil); // Close save menu after saving
end;


procedure TKMGamePlayInterface.Menu_Load_ListClick(Sender: TObject);
begin
  fSaves.Lock;
    Button_Load.Enabled := InRange(ListBox_Load.ItemIndex, 0, fSaves.Count - 1)
                           and fSaves[ListBox_Load.ItemIndex].IsValid;
    if InRange(ListBox_Load.ItemIndex,0,fSaves.Count-1) then
    begin
      Label_LoadDescription.Caption := fSaves[ListBox_Load.ItemIndex].Info.GetTitleWithTime;
      fSave_Selected := ListBox_Load.ItemIndex;
    end;
  fSaves.Unlock;
end;


procedure TKMGamePlayInterface.Menu_Load_Click(Sender: TObject);
begin
  if not InRange(ListBox_Load.ItemIndex, 0, fSaves.Count - 1) then Exit;
  fSaves.TerminateScan; // Stop scan as it is no longer needed
  gGameApp.NewSingleSave(fSaves[ListBox_Load.ItemIndex].FileName);
end;


procedure TKMGamePlayInterface.Menu_Load_RefreshList(Sender: TObject);
var I, PrevTop: Integer;
begin
  PrevTop := ListBox_Load.TopIndex;
  ListBox_Load.Clear;

  if (Sender = fSaves) then
  begin
    fSaves.Lock;
    for I := 0 to fSaves.Count - 1 do
      ListBox_Load.Add(fSaves[I].FileName);
    fSaves.Unlock;
  end;

  ListBox_Load.TopIndex := PrevTop;
  ListBox_Load.ItemIndex := fSave_Selected;

  Menu_Load_ListClick(nil);
end;


procedure TKMGamePlayInterface.HidePages;
var
  I: Integer;
begin
  // Hide all existing pages
  for I := 0 to Panel_Controls.ChildCount - 1 do
    if (Panel_Controls.Childs[I] is TKMPanel) then
      Panel_Controls.Childs[I].Hide;

  fGuiGameBuild.Hide;
  fGuiGameHouse.Hide;
  fGuiGameRatios.Hide;
  fGuiGameStats.Hide;
  fGuiMenuSettings.Hide;
end;


{ Switch between pages }
procedure TKMGamePlayInterface.SwitchPage(Sender: TObject);
var
  LastVisiblePage: TObject;

  procedure Flip4MainButtons(ShowEm: Boolean);
  var T: TKMTabButtons;
  begin
    for T := Low(TKMTabButtons) to High(TKMTabButtons) do Button_Main[T].Visible := ShowEm;
    Button_Back.Visible := not ShowEm;
    Label_MenuTitle.Visible := not ShowEm;
  end;

begin
  if (Sender = Button_Main[tbBuild]) or (Sender = Button_Main[tbRatio])
  or (Sender = Button_Main[tbStats]) or (Sender = Button_Main[tbMenu])
  or (Sender = Button_Menu_Settings) or (Sender = Button_Menu_Quit) then
    gMySpectator.Selected := nil;

  // Set LastVisiblePage to which ever page was last visible, out of the ones needed
  if fGuiMenuSettings.Visible then LastVisiblePage := fGuiMenuSettings else
  if Panel_Save.Visible       then LastVisiblePage := Panel_Save     else
  if Panel_Load.Visible       then LastVisiblePage := Panel_Load     else
    LastVisiblePage := nil;

  // If they just closed settings then we should save them (if something has changed)
  if LastVisiblePage = fGuiMenuSettings then
    gGameApp.GameSettings.SaveSettings;

  // Ensure, that saves scanning will be stopped when user leaves save/load page
  if (LastVisiblePage = Panel_Save) or (LastVisiblePage = Panel_Load) then
    fSaves.TerminateScan;


  HidePages;

  // If Sender is one of 4 main buttons, then open the page, hide the buttons and show Return button
  Flip4MainButtons(false);
  if Sender = Button_Main[tbBuild] then
  begin
    Label_MenuTitle.Caption := gResTexts[TX_MENU_TAB_BUILD];
    fGuiGameBuild.Show;
  end else

  if Sender = Button_Main[tbRatio] then
  begin
    Label_MenuTitle.Caption := gResTexts[TX_MENU_TAB_DISTRIBUTE];
    fGuiGameRatios.Show;
  end else

  if Sender = Button_Main[tbStats] then
  begin
    Label_MenuTitle.Caption := gResTexts[TX_MENU_TAB_STATISTICS];
    fGuiGameStats.Show;
  end else

  if (Sender = Button_Main[tbMenu])
  or (Sender = Button_Quit_No)
  or ((Sender = Button_Back) and ((LastVisiblePage = fGuiMenuSettings)
                               or (LastVisiblePage = Panel_Load)
                               or (LastVisiblePage = Panel_Save))) then begin
    Menu_Update; // Make sure updating happens before it is shown
    Label_MenuTitle.Caption := gResTexts[TX_MENU_TAB_OPTIONS];
    Panel_Menu.Show;
  end else

  if Sender = Button_Menu_Save then
  begin
    fSave_Selected := -1;
    // Stop current now scan so it can't add a save after we clear the list
    fSaves.TerminateScan;
    Menu_Save_RefreshList(nil); // Need to call it at last one time to setup GUI even if there are no saves
    // Initiate refresh and process each new save added
    fSaves.Refresh(Menu_Save_RefreshList, (fUIMode in [umMP, umSpectate]));
    Panel_Save.Show;
    Label_MenuTitle.Caption := gResTexts[TX_MENU_SAVE_GAME];
    if fLastSaveName = '' then
      Edit_Save.Text := gGame.GameName
    else
      Edit_Save.Text := fLastSaveName;
    Menu_Save_EditChange(nil); // Displays "confirm overwrite" message if necessary
  end else

  if Sender = Button_Menu_Load then begin
    fSave_Selected := -1;
    // Stop current now scan so it can't add a save after we clear the list
    fSaves.TerminateScan;
    Menu_Load_RefreshList(nil); // Need to call it at least one time to setup GUI even if there are no saves
    // Initiate refresh and process each new save added
    fSaves.Refresh(Menu_Load_RefreshList, (fUIMode in [umMP, umSpectate]));
    Panel_Load.Show;
    Label_MenuTitle.Caption := gResTexts[TX_MENU_LOAD_GAME];
  end else

  if Sender = Button_Menu_Settings then begin
    fGuiMenuSettings.Menu_Settings_Fill;
    fGuiMenuSettings.Show;
    Label_MenuTitle.Caption := gResTexts[TX_MENU_SETTINGS];
  end else

  if Sender = Button_Menu_Quit then
    Panel_Quit.Show
  else // If Sender is anything else - then show all 4 buttons and hide Return button
    Flip4MainButtons(True);

  // Now process all other kinds of pages
  if (Sender = Panel_Unit) then
    TKMPanel(Sender).Show;
end;


procedure TKMGamePlayInterface.DisplayHint(Sender: TObject);
begin
  if (PrevHint = Sender) then Exit; // Hint didn't change
  if (Sender = nil) or (TKMControl(Sender).Hint = '') then
  begin
    Label_Hint.Caption := '';
    Bevel_HintBG.Hide;
  end
  else
  begin
    Label_Hint.Caption := TKMControl(Sender).Hint;
    Bevel_HintBG.Show;
    Bevel_HintBG.Width := 10 + gRes.Fonts[Label_Hint.Font].GetTextSize(Label_Hint.Caption).X;
  end;
  PrevHint := Sender;
end;


procedure TKMGamePlayInterface.ExportPages(aPath: string);
var
  path: string;
  I, K: Integer;
begin
  inherited;

  path := aPath + 'Gameplay' + PathDelim;
  ForceDirectories(aPath);

  for I := 0 to Panel_Main.ChildCount - 1 do
    if (Panel_Main.Childs[I] is TKMPanel)
    and (Panel_Main.Childs[I].Width > 100) then
    begin
      // Hide all other panels
      for K := 0 to Panel_Main.ChildCount - 1 do
        if Panel_Main.Childs[K] is TKMPanel then
          Panel_Main.Childs[K].Hide;

      Panel_Main.Childs[I].Show;

      gGameApp.PrintScreen(aPath + 'Panel' + int2fix(I, 3) + '.jpg');
    end;
end;


// Update viewport position when user interacts with minimap
procedure TKMGamePlayInterface.Minimap_Update(Sender: TObject; const X,Y: Integer);
begin
  if gMySpectator.Hand.InCinematic then
    Exit;

  fViewport.Position := KMPointF(X,Y);
end;


procedure TKMGamePlayInterface.Minimap_RightClick(Sender: TObject; const X,Y:integer);
var
  Loc: TKMPoint;
  Group: TKMUnitGroup;
begin
  Loc := MinimapView.LocalToMapCoords(X, Y, -1); // Inset by 1 pixel to catch cases "outside of map"
  if not gTerrain.TileInMapCoords(Loc.X, Loc.Y) then Exit; // Must be inside map

  // Send move order, if applicable
  if (gMySpectator.Selected is TKMUnitGroup) and not fPlacingBeacon
  and (fUIMode in [umSP, umMP]) and not HasLostMPGame then
  begin
    Group := TKMUnitGroup(gMySpectator.Selected);
    if Group.CanTakeOrders and (Group.Owner = gMySpectator.HandIndex)
    and Group.CanWalkTo(Loc, 0) then
    begin
      gGame.GameInputProcess.CmdArmy(gic_ArmyWalk, Group, Loc, dir_NA);
      gSoundPlayer.PlayWarrior(Group.UnitType, sp_Move);
    end;
  end;
  if ((gMySpectator.Selected is TKMHouseBarracks) or (gMySpectator.Selected is TKMHouseWoodcutters)) and not fPlacingBeacon
  and (fUIMode in [umSP, umMP]) and not HasLostMPGame then
  begin
    if gTerrain.Route_CanBeMade(KMPointBelow(TKMHouse(gMySpectator.Selected).GetEntrance), Loc, tpWalk, 0) then
    begin
      if gMySpectator.Selected is TKMHouseBarracks then
        gGame.GameInputProcess.CmdHouse(gic_HouseBarracksRally, TKMHouse(gMySpectator.Selected), Loc)
      else
        if gMySpectator.Selected is TKMHouseWoodcutters then
          gGame.GameInputProcess.CmdHouse(gic_HouseWoodcuttersCutting, TKMHouse(gMySpectator.Selected), Loc);
    end
    else
      gSoundPlayer.Play(sfx_CantPlace, Loc, False, 4);
  end;
end;


procedure TKMGamePlayInterface.Minimap_Click(Sender: TObject; const X,Y:integer);
begin
  if fPlacingBeacon then
    Beacon_Place(KMPointF(X,Y));
end;


constructor TKMGamePlayInterface.Create(aRender: TRender; aUIMode: TUIMode);
var S: TKMShape; I: Integer;
begin
  inherited Create(aRender);
  fUIMode := aUIMode;

  fAlerts := TKMAlerts.Create(fViewport);

  // Instruct to use global Terrain
  fLastSaveName := '';
  fJoiningGroups := False;
  fPlacingBeacon := False;
  fDragScrolling := False;
  fDragScrollingCursorPos.X := 0;
  fDragScrollingCursorPos.Y := 0;
  fDragScrollingViewportPos.X := 0;
  fDragScrollingViewportPos.Y := 0;
  SelectingTroopDirection := False;
  SelectingDirPosition.X := 0;
  SelectingDirPosition.Y := 0;
  ShownMessage := -1; // 0 is the first message, -1 is invalid
  for I := 0 to 9 do
    fSelection[I] := -1; // Not set

  fMessageStack := TKMMessageStack.Create;
  fSaves := TKMSavesCollection.Create;

  fTeamNames := TList.Create;

  Label_TeamName := TKMLabel.Create(Panel_Main, 0, 0, '', fnt_Grey, taCenter);

  Sidebar_Top       := TKMImage.Create(Panel_Main, 0,    0, 224, 200, 407);
  Sidebar_Middle    := TKMImage.Create(Panel_Main, 0,  200, 224, 168, 554);

  MinimapView := TKMMinimapView.Create(Panel_Main, 10, 10, 176, 176);
  MinimapView.OnChange := Minimap_Update; // Allow dragging with LMB pressed
  MinimapView.OnClickRight := Minimap_RightClick;
  MinimapView.OnMinimapClick := Minimap_Click; // For placing beacons

  Image_Clock := TKMImage.Create(Panel_Main,232,8,67,65,556);
  Image_Clock.Hide;
  Label_Clock := TKMLabel.Create(Panel_Main,265,80,'mm:ss',fnt_Outline,taCenter);
  Label_Clock.Hide;
  Label_ClockSpeedup := TKMLabel.Create(Panel_Main,265,48,'x1',fnt_Metal,taCenter);
  Label_ClockSpeedup.Hide;

  Label_ScriptedOverlay := TKMLabel.Create(Panel_Main,260,110,'',fnt_Metal,taLeft);

  Button_ScriptedOverlay := TKMButton.Create(Panel_Main, 260, 92, 15, 15, '', bsGame);
  Button_ScriptedOverlay.Hint := gResTexts[TX_GAMEPLAY_OVERLAY_HIDE];
  Button_ScriptedOverlay.Hide;
  Button_ScriptedOverlay.OnClick := HideOverlay;

  Label_OverlayHide := TKMLabel.Create(Panel_Main,263,91,'-',fnt_Metal,taLeft);
  Label_OverlayShow := TKMLabel.Create(Panel_Main,263,93,'+',fnt_Metal,taLeft);
  Label_OverlayHide.Hitable := False;
  Label_OverlayShow.Hitable := False;
  Label_OverlayHide.Hide;
  Label_OverlayShow.Hide;

  Image_DirectionCursor := TKMImage.Create(Panel_Main,0,0,35,36,519);
  Image_DirectionCursor.Hide;

  // Debugging displays
  Label_DebugInfo := TKMLabel.Create(Panel_Main,224+8,106,'',fnt_Outline,taLeft);

{ I plan to store all possible layouts on different pages which gets displayed one at a time }
{ ========================================================================================== }
  Create_Controls; // Includes all the child pages

  Create_NetWait; // Overlay blocking everyhitng but sidestack and messages
  Create_Allies; // MessagePage sibling

  // On top of NetWait to allow players to chat while waiting for late opponents
  fGuiGameChat := TKMGUIGameChat.Create(Panel_Main);

  Create_Message; // Must go bellow message stack
  Create_MessageLog; // Must go bellow message stack
  Create_MessageStack; // Messages, Allies, Chat icons

  Create_Pause;
  Create_Replay; // Replay controls
  Create_PlayMore; // Must be created last, so that all controls behind are blocked
  Create_MPPlayMore;

  Bevel_HintBG := TKMBevel.Create(Panel_Main,224+35,Panel_Main.Height-23,300,21);
  Bevel_HintBG.BackAlpha := 0.5;
  Bevel_HintBG.EdgeAlpha := 0.5;
  Bevel_HintBG.Hide;
  Bevel_HintBG.Anchors := [anLeft, anBottom];
  Label_Hint := TKMLabel.Create(Panel_Main,224+40,Panel_Main.Height-21,0,0,'',fnt_Outline,taLeft);
  Label_Hint.Anchors := [anLeft, anBottom];

  // Controls without a hint will reset the Hint to ''
  fMyControls.OnHint := DisplayHint;

  if OVERLAY_RESOLUTIONS then
  begin
    S := TKMShape.Create(Panel_Main, 0, 96, 1024, 576);
    S.LineColor := $FF00FFFF;
    S.LineWidth := 1;
    S.Hitable := False;
    S := TKMShape.Create(Panel_Main, 0, 0, 1024, 768);
    S.LineColor := $FF00FF00;
    S.LineWidth := 1;
    S.Hitable := False;
  end;

  SwitchPage(nil); // Update
  Resize(aRender.ScreenX, aRender.ScreenY); // Hide/show swords according to player's resolution when game starts
  // Panel_Main.Width := aScreenX;
  // Panel_Main.Height := aScreenY;
  // UpdatePositions; //Reposition messages stack etc.
end;


destructor TKMGamePlayInterface.Destroy;
begin
  ReleaseDirectionSelector; // Make sure we don't exit leaving the cursor restrained

  fGuiGameBuild.Free;
  fGuiGameChat.Free;
  fGuiGameHouse.Free;
  fGuiGameRatios.Free;
  fGuiGameStats.Free;
  fGuiMenuSettings.Free;

  fMessageStack.Free;
  fSaves.Free;
  fTeamNames.Free;
  fAlerts.Free;
  inherited;
end;


procedure TKMGamePlayInterface.Resize(X,Y: Word);
var
  showSwords: Boolean;
begin
  inherited;

  // Show swords filler if screen height allows
  showSwords := (Panel_Main.Height >= 758);
  Sidebar_Middle.Visible := showSwords;

  // Needs to be -10 when the swords are hidden so it fits 1024x576
  Panel_Controls.Top := Sidebar_Top.Height - 10 + (10+Sidebar_Middle.Height) * Byte(showSwords);
  Panel_Controls.Height := Panel_Main.Height - Panel_Controls.Top;

  if fGuiGameStats.Visible then
    fGuiGameStats.Resize;

  fViewport.Resize(X, Y);
end;


{ Pause overlay page }
procedure TKMGamePlayInterface.Create_Pause;
begin
  Panel_Pause := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_Pause.AnchorsStretch;
  Bevel_Pause := TKMBevel.Create(Panel_Pause, -1, -1, Panel_Main.Width + 2, Panel_Main.Height + 2);
  Image_Pause := TKMImage.Create(Panel_Pause, (Panel_Main.Width div 2), (Panel_Main.Height div 2) - 40, 0, 0, 556);
  Label_Pause1 := TKMLabel.Create(Panel_Pause, (Panel_Main.Width div 2), (Panel_Main.Height div 2),
    gResTexts[TX_POPUP_PAUSE], fnt_Antiqua, taCenter);
  Label_Pause2 := TKMLabel.Create(Panel_Pause, (Panel_Main.Width div 2), (Panel_Main.Height div 2) + 20,
    Format(gResTexts[TX_GAMEPLAY_PAUSE_INFO], ['"P"']), fnt_Grey, taCenter);
  Bevel_Pause.AnchorsStretch; // Anchor to all sides
  Image_Pause.ImageCenter;
  Label_Pause1.AnchorsCenter;
  Label_Pause2.AnchorsCenter;
  Image_Pause.AnchorsCenter;
  Panel_Pause.Hide
end;


{ Play More overlay page,
  It's backgrounded with a full-screen bevel area which not only fades image a bit,
  but also blocks all mouse clicks - neat }
procedure TKMGamePlayInterface.Create_PlayMore;
begin
  Panel_PlayMore := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width,Panel_Main.Height);
  Panel_PlayMore.AnchorsStretch;
    Bevel_PlayMore := TKMBevel.Create(Panel_PlayMore,-1,-1,Panel_Main.Width+2,Panel_Main.Height+2);
    Bevel_PlayMore.AnchorsStretch;

    Panel_PlayMoreMsg := TKMPanel.Create(Panel_PlayMore,(Panel_Main.Width div 2)-100,(Panel_Main.Height div 2)-100,200,200);
    Panel_PlayMoreMsg.AnchorsCenter;
      Image_PlayMore := TKMImage.Create(Panel_PlayMoreMsg,100,40,0,0,556);
      Image_PlayMore.ImageCenter;

      Label_PlayMore  := TKMLabel.Create(Panel_PlayMoreMsg,100,80,NO_TEXT,fnt_Outline,taCenter);
      Button_PlayMore := TKMButton.Create(Panel_PlayMoreMsg,0,100,200,30,NO_TEXT,bsGame);
      Button_PlayQuit := TKMButton.Create(Panel_PlayMoreMsg,0,140,200,30,NO_TEXT,bsGame);
      Button_PlayMore.OnClick := PlayMoreClick;
      Button_PlayQuit.OnClick := PlayMoreClick;
    Panel_PlayMore.Hide; // Initially hidden
end;


procedure TKMGamePlayInterface.Create_MPPlayMore;
begin
  Panel_MPPlayMore := TKMPanel.Create(Panel_Main,(Panel_Main.Width div 2)-200,(Panel_Main.Height div 2)-100,400,200);
  Panel_MPPlayMore.AnchorsCenter;
    Bevel_MPPlayMore := TKMBevel.Create(Panel_MPPlayMore,-1,-1,Panel_MPPlayMore.Width+2,Panel_MPPlayMore.Height+2);
    Bevel_MPPlayMore.AnchorsStretch;

      Image_MPPlayMore := TKMImage.Create(Panel_MPPlayMore,200,40,0,0,556);
      Image_MPPlayMore.ImageCenter;

      Label_MPPlayMore  := TKMLabel.Create(Panel_MPPlayMore,200,80,NO_TEXT,fnt_Outline,taCenter);
      Button_MPPlayMore := TKMButton.Create(Panel_MPPlayMore,100,100,200,30,NO_TEXT,bsGame);
      Button_MPPlayQuit := TKMButton.Create(Panel_MPPlayMore,100,140,200,30,NO_TEXT,bsGame);
      Button_MPPlayMore.OnClick := MPPlayMoreClick;
      Button_MPPlayQuit.OnClick := MPPlayMoreClick;
    Panel_MPPlayMore.Hide; // Initially hidden
end;


// Waiting for Net events page, it's similar to PlayMore, but is layered differentlybelow chat panel
procedure TKMGamePlayInterface.Create_NetWait;
begin
  Panel_NetWait := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width,Panel_Main.Height);
  Panel_NetWait.AnchorsStretch;
    Bevel_NetWait := TKMBevel.Create(Panel_NetWait,-1,-1,Panel_Main.Width+2,Panel_Main.Height+2);
    Bevel_NetWait.AnchorsStretch;

    Panel_NetWaitMsg := TKMPanel.Create(Panel_NetWait,0,(Panel_Main.Height div 2)-200,Panel_Main.Width,400);
    Panel_NetWaitMsg.AnchorsCenter;
      Image_NetWait := TKMImage.Create(Panel_NetWaitMsg,Panel_Main.Width div 2,40,0,0,556);
      Image_NetWait.ImageCenter;

      Label_NetWait  := TKMLabel.Create(Panel_NetWaitMsg,Panel_Main.Width div 2,80,NO_TEXT,fnt_Outline,taCenter);
      Label_NetDropPlayersDelay := TKMLabel.Create(Panel_NetWaitMsg,Panel_Main.Width div 2,110,NO_TEXT,fnt_Outline,taCenter);
      Panel_NetWaitButtons := TKMPanel.Create(Panel_NetWaitMsg,0,140,Panel_Main.Width,80);
        Button_NetQuit := TKMButton.Create(Panel_NetWaitButtons,(Panel_Main.Width div 2)-150,0,300,30,gResTexts[TX_GAMEPLAY_QUIT_TO_MENU],bsGame);
        Button_NetQuit.OnClick := NetWaitClick;
        Button_NetDropPlayers := TKMButton.Create(Panel_NetWaitButtons,(Panel_Main.Width div 2)-150,40,300,30,gResTexts[TX_GAMEPLAY_DROP_PLAYERS],bsGame);
        Button_NetDropPlayers.OnClick := NetWaitClick;

      Panel_NetWaitConfirm := TKMPanel.Create(Panel_NetWaitMsg,0,180,Panel_Main.Width,140);
        Label_NetWaitConfirm := TKMLabel.Create(Panel_NetWaitConfirm,(Panel_Main.Width div 2),10,NO_TEXT,fnt_Outline,taCenter);
        Button_NetConfirmYes := TKMButton.Create(Panel_NetWaitConfirm,(Panel_Main.Width div 2)-150,40,300,30,NO_TEXT,bsGame);
        Button_NetConfirmYes.OnClick := NetWaitClick;
        Button_NetConfirmNo := TKMButton.Create(Panel_NetWaitConfirm,(Panel_Main.Width div 2)-150,80,300,30,gResTexts[TX_GAMEPLAY_CONFIRM_CANCEL],bsGame);
        Button_NetConfirmNo.OnClick := NetWaitClick;
      Panel_NetWaitConfirm.Hide;
    Panel_NetWait.Hide; // Initially hidden
end;


procedure TKMGamePlayInterface.Create_MessageStack;
var
  I: Integer;
begin
  Image_MPChat := TKMImage.Create(Panel_Main,TOOLBAR_WIDTH,Panel_Main.Height-48,30,48,494);
  Image_MPChat.Anchors := [anLeft, anBottom];
  Image_MPChat.HighlightOnMouseOver := true;
  Image_MPChat.Hint := gResTexts[TX_GAMEPLAY_CHAT_HINT];
  Image_MPChat.OnClick := Chat_Click;
  Label_MPChatUnread := TKMLabel.Create(Panel_Main,TOOLBAR_WIDTH,Panel_Main.Height-30,30,36,'',fnt_Outline,taCenter);
  Label_MPChatUnread.FontColor := $FF0000FF; // Red
  Label_MPChatUnread.Anchors := [anLeft, anBottom];
  Label_MPChatUnread.Hitable := false; // Clicks should only go to the image, not the flashing label
  Label_MPChatUnread.AutoWrap := true;

  Image_MPAllies := TKMImage.Create(Panel_Main,TOOLBAR_WIDTH,Panel_Main.Height-48*2,30,48,496);
  Image_MPAllies.Anchors := [anLeft, anBottom];
  Image_MPAllies.HighlightOnMouseOver := True;
  Image_MPAllies.Hint := gResTexts[TX_GAMEPLAY_PLAYERS_HINT];
  Image_MPAllies.OnClick := Allies_Click;

  Image_MessageLog := TKMImage.Create(Panel_Main,TOOLBAR_WIDTH,Panel_Main.Height - 48 - IfThen(fUIMode in [umMP, umSpectate], 48*2),30,48,495);
  Image_MessageLog.Anchors := [anLeft, anBottom];
  Image_MessageLog.HighlightOnMouseOver := true;
  Image_MessageLog.Hint := gResTexts[TX_GAME_MESSAGE_LOG];
  Image_MessageLog.OnClick := MessageLog_Click;
  Image_MessageLog.Hide; // Will be shows on first message

  for I := 0 to MAX_VISIBLE_MSGS do
  begin
    Image_Message[I] := TKMImage.Create(Panel_Main, TOOLBAR_WIDTH, 0, 30, 48, 495);
    Image_Message[I].Top := Panel_Main.Height - 48 - I * 48 - IfThen(fUIMode in [umMP, umSpectate], 48 * 2);
    Image_Message[I].Anchors := [anLeft, anBottom];
    Image_Message[I].Disable;
    Image_Message[I].Hide;
    Image_Message[I].HighlightOnMouseOver := True;
    Image_Message[I].Tag := I;
    Image_Message[I].OnClick := Message_Click;
  end;
end;


procedure TKMGamePlayInterface.Create_Replay;
begin
  Panel_ReplayCtrl := TKMPanel.Create(Panel_Main, 320, 8, 160, 60);
    PercentBar_Replay     := TKMPercentBar.Create(Panel_ReplayCtrl, 0, 0, 160, 20);
    Label_Replay          := TKMLabel.Create(Panel_ReplayCtrl,  80,  2, NO_TEXT, fnt_Grey, taCenter);
    Button_ReplayRestart  := TKMButton.Create(Panel_ReplayCtrl,  0, 24, 24, 24, 582, rxGui, bsGame);
    Button_ReplayPause    := TKMButton.Create(Panel_ReplayCtrl, 25, 24, 24, 24, 583, rxGui, bsGame);
    Button_ReplayStep     := TKMButton.Create(Panel_ReplayCtrl, 50, 24, 24, 24, 584, rxGui, bsGame);
    Button_ReplayResume   := TKMButton.Create(Panel_ReplayCtrl, 75, 24, 24, 24, 585, rxGui, bsGame);
    Button_ReplayExit     := TKMButton.Create(Panel_ReplayCtrl,100, 24, 24, 24, 586, rxGui, bsGame);
    //TODO: Button_ReplayFF       := TKMButton.Create(Panel_ReplayCtrl,125, 24, 24, 24, 393, rxGui, bsGame);
    Button_ReplayRestart.OnClick := ReplayClick;
    Button_ReplayPause.OnClick   := ReplayClick;
    Button_ReplayStep.OnClick    := ReplayClick;
    Button_ReplayResume.OnClick  := ReplayClick;
    Button_ReplayExit.OnClick    := ReplayClick;
    Button_ReplayRestart.Hint := gResTexts[TX_REPLAY_RESTART];
    Button_ReplayPause.Hint   := gResTexts[TX_REPLAY_PAUSE];
    Button_ReplayStep.Hint    := gResTexts[TX_REPLAY_STEP];
    Button_ReplayResume.Hint  := gResTexts[TX_REPLAY_RESUME];
    Button_ReplayExit.Hint    := gResTexts[TX_REPLAY_QUIT];

    Button_ReplayStep.Disable; // Initial state
    Button_ReplayResume.Disable; // Initial state

  Panel_ReplayFOW := TKMPanel.Create(Panel_Main, 320, 58, 160, 60);
    Dropbox_ReplayFOW := TKMDropList.Create(Panel_ReplayFOW, 0, 0, 160, 20, fnt_Metal, '', bsGame);
    Dropbox_ReplayFOW.Hint := gResTexts[TX_REPLAY_PLAYER_PERSPECTIVE];
    Dropbox_ReplayFOW.OnChange := ReplayClick;
    Checkbox_ReplayFOW := TKMCheckBox.Create(Panel_ReplayFOW, 0, 25, 220, 20, gResTexts[TX_REPLAY_SHOW_FOG], fnt_Metal);
    Checkbox_ReplayFOW.OnClick := ReplayClick;
end;


// Individual message page
procedure TKMGamePlayInterface.Create_Message;
begin
  Panel_Message := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - MESSAGE_AREA_HEIGHT, 600, MESSAGE_AREA_HEIGHT);
  Panel_Message.Anchors := [anLeft, anBottom];
  Panel_Message.Hide; // Hide it now because it doesn't get hidden by SwitchPage

    TKMImage.Create(Panel_Message, 0, 0, 600, 500, 409);

    Label_MessageText := TKMLabel.Create(Panel_Message, 47, 58, 432, 112, '', fnt_Antiqua, taLeft);
    Label_MessageText.AutoWrap := True;

    Button_MessageGoTo := TKMButton.Create(Panel_Message, 490, 74, 100, 24, gResTexts[TX_MSG_GOTO], bsGame);
    Button_MessageGoTo.Font := fnt_Antiqua;
    Button_MessageGoTo.Hint := gResTexts[TX_MSG_GOTO_HINT];
    Button_MessageGoTo.OnClick := Message_GoTo;

    Button_MessageDelete := TKMButton.Create(Panel_Message, 490, 104, 100, 24, gResTexts[TX_MSG_DELETE], bsGame);
    Button_MessageDelete.Font := fnt_Antiqua;
    Button_MessageDelete.Hint := gResTexts[TX_MSG_DELETE_HINT];
    Button_MessageDelete.OnClick := Message_Delete;
    Button_MessageDelete.MakesSound := False; // Don't play default Click as these buttons use sfx_MessageClose

    Image_MessageClose := TKMImage.Create(Panel_Message, 600 - 76, 24, 32, 32, 52);
    Image_MessageClose.Anchors := [anTop, anLeft];
    Image_MessageClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
    Image_MessageClose.OnClick := Message_Close;
    Image_MessageClose.HighlightOnMouseOver := True;
end;


// Message log page
// there's a queue of not-that-important messages
procedure TKMGamePlayInterface.Create_MessageLog;
var
  I: Integer;
  H: Integer;
begin
  H := 20 * MAX_LOG_MSGS + 2; // +2 for some margin at the bottom

  Panel_MessageLog := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - (H + 65 + 20), 600, H + 65 + 20);
  Panel_MessageLog.Anchors := [anLeft, anBottom];
  Panel_MessageLog.Hide; // Hide it now because it doesn't get hidden by SwitchPage

    TKMImage.Create(Panel_MessageLog, 0, 0, 600, 500, 409);

    Image_MessageLogClose := TKMImage.Create(Panel_MessageLog, 600 - 76, 24, 32, 32, 52);
    Image_MessageLogClose.Anchors := [anTop, anLeft];
    Image_MessageLogClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
    Image_MessageLogClose.OnClick := MessageLog_Close;
    Image_MessageLogClose.HighlightOnMouseOver := True;

    ColumnBox_MessageLog := TKMColumnBox.Create(Panel_MessageLog, 45, 60, 600 - 90, H, fnt_Grey, bsGame);
    ColumnBox_MessageLog.Anchors := [anLeft, anTop, anRight, anBottom];
    ColumnBox_MessageLog.SetColumns(fnt_Outline, ['Icon', 'Message'], [0, 25]);
    ColumnBox_MessageLog.ShowHeader := False;
    ColumnBox_MessageLog.HideSelection := True;
    ColumnBox_MessageLog.HighlightOnMouseOver := True;
    ColumnBox_MessageLog.ItemHeight := 20;
    ColumnBox_MessageLog.BackAlpha := 0;
    ColumnBox_MessageLog.EdgeAlpha := 0;
    ColumnBox_MessageLog.OnClick := MessageLog_ItemClick;
    for I := 0 to MAX_LOG_MSGS - 1 do
      ColumnBox_MessageLog.AddItem(MakeListRow(['', ''], -1));
end;


procedure TKMGamePlayInterface.Create_Controls;
const
  MainHint: array [TKMTabButtons] of Word = (TX_MENU_TAB_HINT_BUILD, TX_MENU_TAB_HINT_DISTRIBUTE,
                                             TX_MENU_TAB_HINT_STATISTICS, TX_MENU_TAB_HINT_OPTIONS);
var
  T: TKMTabButtons;
  I: Integer;
begin
  Panel_Controls := TKMPanel.Create(Panel_Main, 0, 368, 224, 376);
  // Resized manually on .Resize to be most efficient in space management

    // We need several of these to cover max of 1534x2560 (vertically oriented)
    SetLength(Sidebar_Bottom, 6);
    for I := Low(Sidebar_Bottom) to High(Sidebar_Bottom) do
      Sidebar_Bottom[I] := TKMImage.Create(Panel_Controls, 0, 400*I, 224, 400, 404);

    // Main 4 buttons
    for T := Low(TKMTabButtons) to High(TKMTabButtons) do begin
      Button_Main[T] := TKMButton.Create(Panel_Controls,  TB_PAD + 46 * Byte(T), 4, 42, 36, 439 + Byte(T), rxGui, bsGame);
      Button_Main[T].Hint := gResTexts[MainHint[T]];
      Button_Main[T].OnClick := SwitchPage;
    end;
    Button_Back := TKMButton.Create(Panel_Controls, TB_PAD, 4, 42, 36, 443, rxGui, bsGame);
    Button_Back.OnClick := SwitchPage;
    Button_Back.Hint := gResTexts[TX_MENU_TAB_HINT_GO_BACK];

    Label_MenuTitle := TKMLabel.Create(Panel_Controls, 54, 4, 138, 0, '', fnt_Metal, taLeft);
    Label_MenuTitle.AutoWrap := True;

  fGuiGameBuild := TKMGUIGameBuild.Create(Panel_Controls);
  fGuiGameRatios := TKMGUIGameRatios.Create(Panel_Controls, fUIMode in [umSP, umMP]);
  fGuiGameStats := TKMGUIGameStats.Create(Panel_Controls);
  Create_Menu;
    Create_Save;
    Create_Load;
    fGuiMenuSettings := TKMGameMenuSettings.Create(Panel_Controls);
    Create_Quit;

  Create_Unit;

  fGuiGameHouse := TKMGUIGameHouse.Create(Panel_Controls);
  fGuiGameHouse.OnHouseDemolish := House_Demolish;
end;


{ Allies page }
procedure TKMGamePlayInterface.Create_Allies;
var I,K: Integer;
const ROWS = 5;
begin
  Panel_Allies := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - MESSAGE_AREA_HEIGHT, 800, MESSAGE_AREA_HEIGHT);
  Panel_Allies.Anchors := [anLeft, anBottom];
  Panel_Allies.Hide;

    with TKMImage.Create(Panel_Allies,0,0,800,190,409) do ImageAnchors := [anLeft, anRight, anTop];

    Label_PeacetimeRemaining := TKMLabel.Create(Panel_Allies,400,20,'',fnt_Outline,taCenter);
    Image_AlliesHostStar := TKMImage.Create(Panel_Allies, 50, 82, 20, 20, 77, rxGuiMain);
    Image_AlliesHostStar.Hide;

    for I := 0 to MAX_LOBBY_SLOTS - 1 do
    begin
      if (I mod ROWS) = 0 then // Header for each column
      begin
        TKMLabel.Create(Panel_Allies,  70+(I div ROWS)*380, 60, 140, 20, gResTexts[TX_LOBBY_HEADER_PLAYERS], fnt_Outline, taLeft);
        TKMLabel.Create(Panel_Allies, 220+(I div ROWS)*380, 60, 140, 20, gResTexts[TX_LOBBY_HEADER_TEAM], fnt_Outline, taLeft);
        TKMLabel.Create(Panel_Allies, 350+(I div ROWS)*380, 60, gResTexts[TX_LOBBY_HEADER_PINGFPS], fnt_Outline, taCenter);
      end;
      Image_AlliesFlag[I] := TKMImage.Create(Panel_Allies,       50+(I div ROWS)*380, 82+(I mod ROWS)*20, 16,  11,  0, rxGuiMain);
      Label_AlliesPlayer[I] := TKMLabel.Create(Panel_Allies,     70+(I div ROWS)*380, 80+(I mod ROWS)*20, 140, 20, '', fnt_Grey, taLeft);
      Label_AlliesTeam[I]   := TKMLabel.Create(Panel_Allies,    220+(I div ROWS)*380, 80+(I mod ROWS)*20, 120, 20, '', fnt_Grey, taLeft);
      DropBox_AlliesTeam[I] := TKMDropList.Create(Panel_Allies, 220+(I div ROWS)*380, 80+(I mod ROWS)*20, 120, 20, fnt_Grey, '', bsGame);
      DropBox_AlliesTeam[I].Hide; // Use label for demos until we fix exploits
      DropBox_AlliesTeam[I].Add('-');
      for K := 1 to 4 do DropBox_AlliesTeam[I].Add(IntToStr(K));
      DropBox_AlliesTeam[I].OnChange := AlliesTeamChange;
      DropBox_AlliesTeam[I].DropUp := True; // Doesn't fit if it drops down
      Label_AlliesPing[I] := TKMLabel.Create(Panel_Allies,   350+(I div ROWS)*380, 80+(I mod ROWS)*20, '', fnt_Grey, taCenter);
    end;

    Image_AlliesClose:=TKMImage.Create(Panel_Allies,800-97,24,32,32,52,rxGui);
    Image_AlliesClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
    Image_AlliesClose.OnClick := Allies_Close;
    Image_AlliesClose.HighlightOnMouseOver := True;
end;


{ Menu page }
procedure TKMGamePlayInterface.Create_Menu;
begin
  Panel_Menu := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 332);
  Button_Menu_Load := TKMButton.Create(Panel_Menu, 0, 20, TB_WIDTH, 30, gResTexts[TX_MENU_LOAD_GAME], bsGame);
  Button_Menu_Load.OnClick := SwitchPage;
  Button_Menu_Load.Hint := gResTexts[TX_MENU_LOAD_GAME];
  Button_Menu_Load.Visible := not (fUIMode in [umMP, umSpectate]);

  Button_Menu_ReturnLobby := TKMButton.Create(Panel_Menu, 0, 20, TB_WIDTH, 30, gResTexts[TX_MENU_VOTE_RETURN_LOBBY], bsGame);
  Button_Menu_ReturnLobby.OnClick := ReturnToLobbyClick;
  Button_Menu_ReturnLobby.Hint := gResTexts[TX_MENU_VOTE_RETURN_LOBBY_HINT];
  Button_Menu_ReturnLobby.Visible := fUIMode in [umMP, umSpectate];

  Button_Menu_Save := TKMButton.Create(Panel_Menu, 0, 60, TB_WIDTH, 30, gResTexts[TX_MENU_SAVE_GAME], bsGame);
  Button_Menu_Save.OnClick := SwitchPage;
  Button_Menu_Save.Hint := gResTexts[TX_MENU_SAVE_GAME];
  Button_Menu_Settings := TKMButton.Create(Panel_Menu, 0, 100, TB_WIDTH, 30, gResTexts[TX_MENU_SETTINGS], bsGame);
  Button_Menu_Settings.OnClick := SwitchPage;
  Button_Menu_Settings.Hint := gResTexts[TX_MENU_SETTINGS];
  Button_Menu_Quit := TKMButton.Create(Panel_Menu, 0, 170, TB_WIDTH, 30, gResTexts[TX_MENU_QUIT_MISSION], bsGame);
  Button_Menu_Quit.Hint := gResTexts[TX_MENU_QUIT_MISSION];
  Button_Menu_Quit.OnClick := SwitchPage;
  Button_Menu_TrackUp := TKMButton.Create(Panel_Menu, 150, 300, 30, 30, '>', bsGame);
  Button_Menu_TrackDown := TKMButton.Create(Panel_Menu, 0, 300, 30, 30, '<', bsGame);
  Button_Menu_TrackUp.Hint := gResTexts[TX_MUSIC_NEXT_HINT];
  Button_Menu_TrackDown.Hint := gResTexts[TX_MUSIC_PREV_HINT];
  Button_Menu_TrackUp.OnClick := Menu_NextTrack;
  Button_Menu_TrackDown.OnClick := Menu_PreviousTrack;
  TKMLabel.Create(Panel_Menu, 0, 260, TB_WIDTH, 30, gResTexts[TX_MUSIC_PLAYER], fnt_Metal, taCenter);
  Label_Menu_Track := TKMLabel.Create(Panel_Menu, 0, 276, TB_WIDTH, 30, '', fnt_Grey, taCenter);
  Label_Menu_Track.Hitable := False; // It can block hits for the track Up/Down buttons as they overlap
  Label_GameTime := TKMLabel.Create(Panel_Menu, 0, 214, TB_WIDTH, 20, '', fnt_Outline, taCenter);
end;


{ Save page }
procedure TKMGamePlayInterface.Create_Save;
begin
  Panel_Save := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 332);

    // Edit field created first to pick a focus on panel show
    Edit_Save := TKMEdit.Create(Panel_Save, 0, 235, TB_WIDTH, 20, fnt_Metal);
    Edit_Save.AllowedChars := acFileName;
    Edit_Save.MaxLen := MAX_SAVENAME_LENGTH;
    Edit_Save.OnChange := Menu_Save_EditChange;

    ListBox_Save := TKMListBox.Create(Panel_Save, 0, 4, TB_WIDTH, 220, fnt_Metal, bsGame);
    ListBox_Save.AutoHideScrollBar := True;
    ListBox_Save.OnChange := Menu_Save_ListChange;

    Label_SaveExists := TKMLabel.Create(Panel_Save,0,260,TB_WIDTH,30,gResTexts[TX_GAMEPLAY_SAVE_EXISTS],fnt_Outline,taLeft);
    CheckBox_SaveExists := TKMCheckBox.Create(Panel_Save,0,280,TB_WIDTH,20,gResTexts[TX_GAMEPLAY_SAVE_OVERWRITE], fnt_Metal);
    CheckBox_SaveExists.OnClick := Menu_Save_CheckboxChange;

    Button_Save := TKMButton.Create(Panel_Save,0,300,TB_WIDTH,30,gResTexts[TX_GAMEPLAY_SAVE_SAVE], bsGame);
    Button_Save.OnClick := Menu_Save_Click;
end;


{ Load page }
procedure TKMGamePlayInterface.Create_Load;
begin
  Panel_Load := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 332);

    ListBox_Load := TKMListBox.Create(Panel_Load, 0, 2, TB_WIDTH, 260, fnt_Metal, bsGame);
    ListBox_Load.AutoHideScrollBar := True;
    ListBox_Load.OnChange := Menu_Load_ListClick;

    Label_LoadDescription := TKMLabel.Create(Panel_Load,0,265,TB_WIDTH,0,'',fnt_Grey,taLeft);
    Label_LoadDescription.AutoWrap := true;

    Button_Load := TKMButton.Create(Panel_Load,0,300,TB_WIDTH,30,gResTexts[TX_GAMEPLAY_LOAD], bsGame);
    Button_Load.OnClick := Menu_Load_Click;
end;


{ Quit page }
procedure TKMGamePlayInterface.Create_Quit;
begin
  Panel_Quit := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 332);
    with TKMLabel.Create(Panel_Quit, 0, 30, TB_WIDTH, 70, gResTexts[TX_MENU_QUIT_QUESTION], fnt_Outline, taCenter) do
      AutoWrap := True;
    Button_Quit_Yes := TKMButton.Create(Panel_Quit, 0, 100, TB_WIDTH, 30, gResTexts[TX_MENU_QUIT_MISSION], bsGame);
    Button_Quit_No := TKMButton.Create(Panel_Quit, 0, 140, TB_WIDTH, 30, gResTexts[TX_MENU_DONT_QUIT_MISSION], bsGame);
    Button_Quit_Yes.Hint := gResTexts[TX_MENU_QUIT_MISSION];
    Button_Quit_No.Hint := gResTexts[TX_MENU_DONT_QUIT_MISSION];
    Button_Quit_Yes.OnClick := Menu_QuitMission;
    Button_Quit_No.OnClick := SwitchPage;
end;


{ Unit page }
procedure TKMGamePlayInterface.Create_Unit;
begin
  Panel_Unit := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 332);
    Label_UnitName        := TKMLabel.Create(Panel_Unit,0,16,TB_WIDTH,30,'',fnt_Outline,taCenter);
    Image_UnitPic         := TKMImage.Create(Panel_Unit,0,38,54,100,521);
    Label_UnitCondition   := TKMLabel.Create(Panel_Unit,65,40,116,30,gResTexts[TX_UNIT_CONDITION],fnt_Grey,taCenter);
    ConditionBar_Unit     := TKMPercentBar.Create(Panel_Unit,65,55,116,15);
    Label_UnitTask        := TKMLabel.Create(Panel_Unit,65,80,116,60,'',fnt_Grey,taLeft);
    Label_UnitTask.AutoWrap := True;
    Label_UnitDescription := TKMLabel.Create(Panel_Unit,0,152,TB_WIDTH,200,'',fnt_Grey,taLeft); // Taken from LIB resource
    Label_UnitDescription.AutoWrap := True;
    Button_Unit_Dismiss   := TKMButton.Create(Panel_Unit,124,120,56,34,29, rxGui, bsGame);

    Panel_Unit_Dismiss := TKMPanel.Create(Panel_Unit, 0, 160, TB_WIDTH, 182);
    Label_Unit_Dismiss             := TKMLabel.Create(Panel_Unit_Dismiss,100,16,TB_WIDTH,30,'Are you sure?',fnt_Outline,taCenter);
    Button_Unit_DismissYes         := TKMButton.Create(Panel_Unit_Dismiss,50, 50,TB_WIDTH,40,'Dismiss',bsGame);
    Button_Unit_DismissNo          := TKMButton.Create(Panel_Unit_Dismiss,50,100,TB_WIDTH,40,'Cancel',bsGame);
    Button_Unit_DismissYes.OnClick := Unit_Dismiss;
    Button_Unit_DismissNo.OnClick  := Unit_Dismiss;

    Panel_Army := TKMPanel.Create(Panel_Unit, 0, 160, TB_WIDTH, 182);
    // Military buttons start at 8.170 and are 52x38/30 (60x46)
    Button_Army_GoTo   := TKMButton.Create(Panel_Army,  0,  0, 56, 40, 27, rxGui, bsGame);
    Button_Army_Stop   := TKMButton.Create(Panel_Army, 62,  0, 56, 40, 26, rxGui, bsGame);
    Button_Army_Attack := TKMButton.Create(Panel_Army,124,  0, 56, 40, 25, rxGui, bsGame);
    Button_Army_RotCCW := TKMButton.Create(Panel_Army,  0, 46, 56, 40, 23, rxGui, bsGame);
    Button_Army_Storm  := TKMButton.Create(Panel_Army, 62, 46, 56, 40, 28, rxGui, bsGame);
    Button_Army_RotCW  := TKMButton.Create(Panel_Army,124, 46, 56, 40, 24, rxGui, bsGame);
    Button_Army_ForUp  := TKMButton.Create(Panel_Army,  0, 92, 56, 40, 33, rxGui, bsGame);
    ImageStack_Army    := TKMImageStack.Create(Panel_Army, 62, 92, 56, 40, 43, 50);
    Label_Army_MembersCount := TKMLabel.Create(Panel_Army, 62, 106, 56, 20, '', fnt_Outline, taCenter);
    Button_Army_ForDown:= TKMButton.Create(Panel_Army,124, 92, 56, 40, 32, rxGui, bsGame);
    Button_Army_Split  := TKMButton.Create(Panel_Army,  0,138, 56, 34, 31, rxGui, bsGame);
    Button_Army_Join   := TKMButton.Create(Panel_Army, 62,138, 56, 34, 30, rxGui, bsGame);
    Button_Army_Feed   := TKMButton.Create(Panel_Army,124,138, 56, 34, 29, rxGui, bsGame);

    // All one-click-action (i.e. not attack, move, link up) army controls have a single procedure
    // that decides what to do based on Sender
    Button_Army_GoTo.OnClick    := Army_Issue_Order;
    Button_Army_Stop.OnClick    := Army_Issue_Order;
    Button_Army_Attack.OnClick  := Army_Issue_Order;
    Button_Army_RotCW.OnClick   := Army_Issue_Order;
    Button_Army_Storm.OnClick   := Army_Issue_Order;
    Button_Army_RotCCW.OnClick  := Army_Issue_Order;
    Button_Army_ForDown.OnClick := Army_Issue_Order;
    Button_Army_ForUp.OnClick   := Army_Issue_Order;
    Button_Army_Split.OnClick   := Army_Issue_Order;
    Button_Army_Join.OnClick    := Army_Issue_Order;
    Button_Army_Feed.OnClick    := Army_Issue_Order;
    Button_Unit_Dismiss.OnClick := Army_Issue_Order;

    // Disable not working buttons
    Button_Army_GoTo.Hide;
    Button_Army_Attack.Hide;

    // Hints
    Button_Army_GoTo.Hint     := gResTexts[TX_ARMY_GOTO_HINT];
    Button_Army_Stop.Hint     := Format(gResTexts[TX_TROOP_HALT_HINT], [gResKeys.GetKeyNameById(SC_ARMY_HALT)]);
    Button_Army_Attack.Hint   := gResTexts[TX_ARMY_ATTACK_HINT];
    Button_Army_RotCW.Hint    := gResTexts[TX_ARMY_ROTATE_CW_HINT] + ' (''' + gResKeys.GetKeyNameById(SC_ARMY_ROTATE_CW) + ''')';
    Button_Army_Storm.Hint    := gResTexts[TX_ARMY_STORM_HINT] + ' (''' + gResKeys.GetKeyNameById(SC_ARMY_STORM) + ''')';
    Button_Army_RotCCW.Hint   := gResTexts[TX_ARMY_ROTATE_CCW_HINT] + ' (''' + gResKeys.GetKeyNameById(SC_ARMY_ROTATE_CCW) + ''')';
    Button_Army_ForDown.Hint  := gResTexts[TX_ARMY_LINE_ADD_HINT] + ' (''' + gResKeys.GetKeyNameById(SC_ARMY_ADD_LINE) + ''')';
    Button_Army_ForUp.Hint    := gResTexts[TX_ARMY_LINE_REM_HINT] + ' (''' + gResKeys.GetKeyNameById(SC_ARMY_DEL_LINE) + ''')';
    Button_Army_Split.Hint    := Format(gResTexts[TX_TROOP_SPLIT_HINT], [gResKeys.GetKeyNameById(SC_ARMY_SPLIT)]);
    Button_Army_Join.Hint     := Format(gResTexts[TX_TROOP_LINK_HINT], [gResKeys.GetKeyNameById(SC_ARMY_LINK)]);
    Button_Army_Feed.Hint     := gResTexts[TX_ARMY_FEED_HINT] + ' (''' + gResKeys.GetKeyNameById(SC_ARMY_FOOD) + ''')';
    Button_Unit_Dismiss.Hint  := 'Dismiss unit';

    { Army controls...
    Go to     Stop      Attack
    Rotate    Storm     Rotate
    -Column   [Info]    +Column
    Split     Join      Feed }

    Panel_Army_JoinGroups := TKMPanel.Create(Panel_Unit, 0, 160, TB_WIDTH, 182);
    Label_Army_Join_Message := TKMLabel.Create(Panel_Army_JoinGroups, 0, 30, TB_WIDTH, 65, gResTexts[TX_ARMY_JOIN_SELECT], fnt_Outline, taCenter);
    Button_Army_Join_Cancel := TKMButton.Create(Panel_Army_JoinGroups, 0, 95, TB_WIDTH, 30, gResTexts[TX_ARMY_JOIN_CANCEL], bsGame);

  Button_Army_Join_Cancel.OnClick := Army_HideJoinMenu;
end;


procedure TKMGamePlayInterface.Chat_Click(Sender: TObject);
begin
  if fGuiGameChat.Visible then
    fGuiGameChat.Hide
  else
  begin
    Allies_Close(nil);
    Message_Close(nil);
    MessageLog_Close(nil);
    Label_MPChatUnread.Caption := ''; // No unread messages
    fGuiGameChat.Show;
  end;
end;


procedure TKMGamePlayInterface.CinematicUpdate;
var I: Integer;
begin
  if gMySpectator.Hand.InCinematic then
  begin
    gMySpectator.Selected := nil;
    // Close panels unless it is an allowed menu
    if not Panel_Menu.Visible and not Panel_Load.Visible and not Panel_Save.Visible
    and not fGuiMenuSettings.Visible and not Panel_Quit.Visible and not fGuiGameStats.Visible then
      SwitchPage(nil);

    fDragScrolling := False;
    fJoiningGroups := False;
    ReleaseDirectionSelector;
    gRes.Cursors.Cursor := kmc_Default; // Might have been scrolling or joining groups
    SetMenuState(gGame.MissionMode = mm_Tactic); // Disabled main buttons

    MinimapView.Disable;
    Sidebar_Top.Disable;
    Sidebar_Middle.Disable;
    for I := Low(Sidebar_Bottom) to High(Sidebar_Bottom) do
      Sidebar_Bottom[I].Disable;
  end
  else
  begin
    SetMenuState(gGame.MissionMode = mm_Tactic); // Enable main buttons

    MinimapView.Enable;
    Sidebar_Top.Enable;
    Sidebar_Middle.Enable;
    for I := Low(Sidebar_Bottom) to High(Sidebar_Bottom) do
      Sidebar_Bottom[I].Enable;
  end;
end;


// Used when loading MP save since hotkeys must be network synced
procedure TKMGamePlayInterface.LoadHotkeysFromHand;
var I: Integer;
begin
  for I := 0 to 9 do
    fSelection[I] := gMySpectator.Hand.SelectionHotkeys[I];
end;


procedure TKMGamePlayInterface.Allies_Click(Sender: TObject);
begin
  if Panel_Allies.Visible then
    Allies_Close(Sender)
  else
    Allies_Show(Sender);
end;


procedure TKMGamePlayInterface.Allies_Show(Sender: TObject);
begin
  gSoundPlayer.Play(sfxn_MPChatOpen);
  Panel_Allies.Show;
  fGuiGameChat.Hide;
  Message_Close(nil);
  MessageLog_Close(nil);
end;


procedure TKMGamePlayInterface.House_Demolish;
begin
  SwitchPage(Button_Main[tbBuild]);
end;


// Click on the same message again closes it
procedure TKMGamePlayInterface.Message_Click(Sender: TObject);
begin
  if TKMImage(Sender).Tag <> ShownMessage then
    Message_Show(TKMImage(Sender).Tag)
  else
    Message_Close(Sender);
end;


procedure TKMGamePlayInterface.Message_Show(aIndex: Integer);
var
  I: Integer;
begin
  ShownMessage := aIndex;

  // Highlight target message icon
  for I := 0 to MAX_VISIBLE_MSGS do
    Image_Message[I].Highlight := (ShownMessage = I);

  Label_MessageText.Caption := fMessageStack[ShownMessage].Text;
  Button_MessageGoTo.Visible := not KMSamePoint(fMessageStack[ShownMessage].Loc, KMPoint(0,0));

  Allies_Close(nil);
  fGuiGameChat.Hide;
  MessageLog_Close(nil);
  Panel_Message.Show;
  // Must update top AFTER showing panel, otherwise Button_MessageGoTo.Visible will always return false
  Button_MessageDelete.Top := IfThen(Button_MessageGoTo.Visible, 104, 74);
  gSoundPlayer.Play(sfx_MessageOpen); // Play parchment sound when they open the message
end;


// Message has been closed
procedure TKMGamePlayInterface.Message_Close(Sender: TObject);
begin
  // Remove highlight
  if ShownMessage <> -1 then
  begin
    Image_Message[ShownMessage].Highlight := False;

    // Play sound
    if Sender <> nil then
      gSoundPlayer.Play(sfx_MessageClose);
  end;

  ShownMessage := -1;
  Panel_Message.Hide;
end;


procedure TKMGamePlayInterface.Message_Delete(Sender: TObject);
var
  OldMsg: Integer;
begin
  if ShownMessage = -1 then Exit; // Player pressed DEL with no Msg opened

  OldMsg := ShownMessage;

  Message_Close(Sender);
  fMessageStack.RemoveStack(OldMsg);

  Message_UpdateStack;
  DisplayHint(nil);
end;


procedure TKMGamePlayInterface.Message_GoTo(Sender: TObject);
begin
  fViewport.Position := KMPointF(fMessageStack.MessagesStack[ShownMessage].Loc);
end;


procedure TKMGamePlayInterface.Message_UpdateStack;
var
  I: Integer;
begin
  // MessageList is unlimited, while Image_Message has fixed depth and samples data from the list on demand
  for I := 0 to MAX_VISIBLE_MSGS do
  begin
    // Disable and hide at once for safety
    Image_Message[I].Enabled := (I <= fMessageStack.CountStack - 1);
    Image_Message[I].Visible := (I <= fMessageStack.CountStack - 1);
    if I <= fMessageStack.CountStack - 1 then
      Image_Message[i].TexID := fMessageStack.MessagesStack[I].Icon;
  end;
end;


procedure TKMGamePlayInterface.ShowUnitInfo(Sender: TKMUnit; aAskDismiss: Boolean = False);
begin
  Assert(gMySpectator.Selected = Sender);

  fAskDismiss  := aAskDismiss;

  if Sender = nil then
  begin
    SwitchPage(nil);
    Exit;
  end;

  SwitchPage(Panel_Unit);

  // Common properties
  Label_UnitName.Caption      := gRes.UnitDat[Sender.UnitType].GUIName;
  Image_UnitPic.TexID         := gRes.UnitDat[Sender.UnitType].GUIScroll;
  Image_UnitPic.FlagColor     := gHands[Sender.Owner].FlagColor;
  ConditionBar_Unit.Position  := Sender.Condition / UNIT_MAX_CONDITION;
  Label_UnitTask.Caption      := Sender.GetActivityText;

  Label_UnitDescription.Show;
  Button_Unit_Dismiss.Visible := SHOW_DISMISS_BUTTON and not fAskDismiss;
  Panel_Army.Hide;
  Panel_Army_JoinGroups.Hide;
  Panel_Unit_Dismiss.Visible := SHOW_DISMISS_BUTTON and fAskDismiss;

  Label_UnitDescription.Caption := gRes.UnitDat[Sender.UnitType].Description;
end;


procedure TKMGamePlayInterface.ShowGroupInfo(Sender: TKMUnitGroup);
var
  W: TKMUnitWarrior;
begin
  Assert(gMySpectator.Selected = Sender);

  if (Sender = nil) or (Sender.SelectedUnit = nil) then
  begin
    SwitchPage(nil);
    Exit;
  end;

  W := Sender.SelectedUnit;
  SwitchPage(Panel_Unit);

  // Common properties
  Label_UnitName.Caption      := gRes.UnitDat[W.UnitType].GUIName;
  Image_UnitPic.TexID         := gRes.UnitDat[W.UnitType].GUIScroll;
  Image_UnitPic.FlagColor     := gHands[W.Owner].FlagColor;
  ConditionBar_Unit.Position  := W.Condition / UNIT_MAX_CONDITION;
  // We show what this individual is doing, not the whole group.
  // However this can be useful for debugging: Sender.GetOrderText
  Label_UnitTask.Caption      := W.GetWarriorActivityText(Sender.IsAttackingUnit);

  // While selecting target to join we could get attacked
  // Then we must cancel the dialog
  if not Sender.CanTakeOrders then
    Army_HideJoinMenu(nil); // Cannot be joining while in combat/charging

  Label_UnitDescription.Hide;
  Button_Unit_Dismiss.Visible := SHOW_DISMISS_BUTTON and not fAskDismiss and not fJoiningGroups;
  Panel_Army.Visible := not fAskDismiss and not fJoiningGroups;
  Panel_Army_JoinGroups.Visible := not fAskDismiss and fJoiningGroups;
  Panel_Unit_Dismiss.Visible := SHOW_DISMISS_BUTTON and fAskDismiss and not fJoiningGroups;

  // Update army controls if required
  if Panel_Army.Visible then
  begin
    ImageStack_Army.SetCount(Sender.Count, Sender.UnitsPerRow, Sender.UnitsPerRow div 2);
    Army_ActivateControls(Sender);
    Label_Army_MembersCount.Caption := IntToStr(Sender.Count);
  end;
end;


// Quit the mission and return to main menu
procedure TKMGamePlayInterface.Menu_QuitMission(Sender: TObject);
begin
  // Show outcome depending on actual situation.
  // By default PlayOnState is gr_Cancel, if playing on after victory/defeat it changes
  gGameApp.Stop(gGame.PlayOnState);
end;


procedure TKMGamePlayInterface.Menu_NextTrack(Sender: TObject);
begin
  gGameApp.MusicLib.PlayNextTrack;
end;


procedure TKMGamePlayInterface.Menu_PreviousTrack(Sender: TObject);
begin
  gGameApp.MusicLib.PlayPreviousTrack;
end;


procedure TKMGamePlayInterface.Army_Issue_Order(Sender: TObject);
var
  Group: TKMUnitGroup;
begin
  if gMySpectator.Selected = nil then exit;
  if not (gMySpectator.Selected is TKMUnitGroup) then Exit;

  { Not implemented yet
  if Sender = Button_Unit_Dismiss then
  begin
    ShowUnitInfo(TKMUnit(gMySpectator.Selected), true);
  end; }

  Group := TKMUnitGroup(gMySpectator.Selected);

  // if Sender = Button_Army_GoTo    then ; // This command makes no sense unless player has no right-mouse-button
  if Sender = Button_Army_Stop    then
  begin
    gGame.GameInputProcess.CmdArmy(gic_ArmyHalt, Group);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_Halt);
  end;
  // if Sender = Button_Army_Attack  then ; // This command makes no sense unless player has no right-mouse-button
  if Sender = Button_Army_RotCW   then
  begin
    gGame.GameInputProcess.CmdArmy(gic_ArmyFormation, Group, tdCW, 0);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_RotRight);
  end;
  if Sender = Button_Army_Storm   then
  begin
    gGame.GameInputProcess.CmdArmy(gic_ArmyStorm, Group);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_StormAttack);
  end;
  if Sender = Button_Army_RotCCW  then
  begin
    gGame.GameInputProcess.CmdArmy(gic_ArmyFormation, Group, tdCCW, 0);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_RotLeft);
  end;
  if Sender = Button_Army_ForDown then
  begin
    gGame.GameInputProcess.CmdArmy(gic_ArmyFormation, Group, tdNone, 1);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_Formation);
  end;
  if Sender = Button_Army_ForUp   then
  begin
    gGame.GameInputProcess.CmdArmy(gic_ArmyFormation, Group, tdNone, -1);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_Formation);
  end;
  if Sender = Button_Army_Split   then
  begin
    if GetKeyState(VK_CONTROL) < 0 then
      gGame.GameInputProcess.CmdArmy(gic_ArmySplitSingle, Group)
    else
      gGame.GameInputProcess.CmdArmy(gic_ArmySplit, Group);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_Split);
  end;
  if Sender = Button_Army_Join    then
  begin
    Panel_Army.Hide;
    Panel_Army_JoinGroups.Show;
    fJoiningGroups := true;
  end;
  if Sender = Button_Army_Feed    then
  begin
    gGame.GameInputProcess.CmdArmy(gic_ArmyFeed, Group);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_Eat);
  end;
end;


procedure TKMGamePlayInterface.Unit_Dismiss(Sender: TObject);
begin
  if (gMySpectator.Selected = nil)
  or not (gMySpectator.Selected is TKMUnit) then
    Exit;

  if Sender = Button_Unit_DismissYes then
  begin
    // DISMISS UNIT
    fAskDismiss := False;
    ShowUnitInfo(nil, False); // Simpliest way to reset page and ShownUnit
    SwitchPage(nil); // Return to main menu after dismissing
  end
  else
  begin
    fAskDismiss := False;
    ShowUnitInfo(TKMUnit(gMySpectator.Selected), False);  // Cancel and return to selected unit
  end;
end;


procedure TKMGamePlayInterface.Army_HideJoinMenu(Sender: TObject);
begin
  fJoiningGroups := False;
  if gRes.Cursors.Cursor in [kmc_JoinYes, kmc_JoinNo] then // Do not override non-joining cursors
    gRes.Cursors.Cursor := kmc_Default; // In case this is run with keyboard shortcut, mouse move won't happen
  Panel_Army_JoinGroups.Hide;
  if gMySpectator.Selected is TKMUnitWarrior then
    Panel_Army.Show;
end;


procedure TKMGamePlayInterface.Allies_Close(Sender: TObject);
begin
  if Panel_Allies.Visible then gSoundPlayer.Play(sfxn_MPChatClose);
  Panel_Allies.Hide;
end;


procedure TKMGamePlayInterface.AlliesUpdateMapping;
var
  I, K, T: Integer;
begin
  // First empty everything
  for I:=0 to MAX_LOBBY_SLOTS-1 do
    fAlliesToNetPlayers[I] := -1;

  K := 0;
  // Players, sorted by team
  for T := 0 to 4 do
    for I:=1 to gGame.Networking.NetPlayers.Count do
      if not gGame.Networking.NetPlayers[I].IsSpectator and (gGame.Networking.NetPlayers[I].Team = T) then
      begin
        fAlliesToNetPlayers[K] := I;
        Inc(K);
      end;

  // Spectators
  for I:=1 to gGame.Networking.NetPlayers.Count do
    if gGame.Networking.NetPlayers[I].IsSpectator then
    begin
      fAlliesToNetPlayers[K] := I;
      Inc(K);
    end;
end;


procedure TKMGamePlayInterface.ReplayClick(Sender: TObject);
  procedure SetButtons(aPaused: Boolean);
  begin
    Button_ReplayPause.Enabled := aPaused;
    Button_ReplayStep.Enabled := not aPaused;
    Button_ReplayResume.Enabled := not aPaused;
  end;
var
  oldCenter: TKMPointF;
  oldZoom: Single;
begin
  if (Sender = Button_ReplayRestart) then
  begin
    // Restart the replay but keep the viewport position/zoom
    oldCenter := fViewport.Position;
    oldZoom := fViewport.Zoom;

    gGame.RestartReplay; //reload it once again

    // Self is now destroyed, so we must access the NEW fGame object
    gGame.GamePlayInterface.SyncUIView(oldCenter, oldZoom);

    Exit; // Restarting the replay will destroy Self, so exit immediately
  end;

  if (Sender = Button_ReplayPause) then
  begin
    gGame.IsPaused := True;
    SetButtons(False);
  end;

  if (Sender = Button_ReplayStep) then
  begin
    gGame.StepOneFrame;
    gGame.IsPaused := False;
    SetButtons(False);
  end;

  if (Sender = Button_ReplayResume) then
  begin
    gGame.IsPaused := False;
    SetButtons(True);
  end;

  if (Sender = Button_ReplayExit) then
  begin
    gGame.GameHold(True, gr_ReplayEnd);
    SetButtons(True);
  end;

  if (Sender = Dropbox_ReplayFOW) then
  begin
    gMySpectator.HandIndex := Dropbox_ReplayFOW.GetTag(Dropbox_ReplayFOW.ItemIndex);
    if Checkbox_ReplayFOW.Checked then
      gMySpectator.FOWIndex := gMySpectator.HandIndex
    else
      gMySpectator.FOWIndex := -1;
    fMinimap.Update(False); // Force update right now so FOW doesn't appear to lag
    gGame.OverlayUpdate; // Display the overlay seen by the selected player
  end;

  if (Sender = Checkbox_ReplayFOW) then
  begin
    if Checkbox_ReplayFOW.Checked then
      gMySpectator.FOWIndex := gMySpectator.HandIndex
    else
      gMySpectator.FOWIndex := -1;
    fMinimap.Update(False); // Force update right now so FOW doesn't appear to lag
  end;
end;


procedure TKMGamePlayInterface.ReturnToLobbyClick(Sender: TObject);
begin
  gGame.Networking.VoteReturnToLobby;
end;


procedure TKMGamePlayInterface.MessageIssue(aKind: TKMMessageKind; aText: UnicodeString);
begin
  MessageIssue(aKind, aText, KMPoint(0,0));
end;


procedure TKMGamePlayInterface.MessageIssue(aKind: TKMMessageKind; aText: UnicodeString; aLoc: TKMPoint);
begin
  if fUIMode in [umReplay, umSpectate] then Exit; // No message stack in replay/spectate
  fMessageStack.Add(aKind, aText, aLoc);
  Message_UpdateStack;
  gSoundPlayer.Play(sfx_MessageNotice, 4); // Play horn sound on new message if it is the right type
end;


procedure TKMGamePlayInterface.MessageLog_Click(Sender: TObject);
begin
  if Panel_MessageLog.Visible then
  begin
    Panel_MessageLog.Hide;
    gSoundPlayer.Play(sfx_MessageClose);
  end
  else
  begin
    MessageLog_Update(True);

    Allies_Close(nil);
    fGuiGameChat.Hide;
    MessageLog_Close(nil);
    Message_Close(nil);

    Panel_MessageLog.Show;
    ColumnBox_MessageLog.TopIndex := ColumnBox_MessageLog.RowCount;
    gSoundPlayer.Play(sfx_MessageOpen); // Play parchment sound when they open the message
  end;
end;


procedure TKMGamePlayInterface.MessageLog_Close(Sender: TObject);
begin
  Panel_MessageLog.Hide;
  if Sender = Image_MessageLogClose then
    gSoundPlayer.Play(sfx_MessageClose);
end;


procedure TKMGamePlayInterface.MessageLog_ItemClick(Sender: TObject);
var
  ItemId, MessageId: Integer;
  Msg: TKMLogMessage;
  H: TKMHouse;
begin
  ItemId := ColumnBox_MessageLog.ItemIndex;
  if ItemId = -1 then Exit;

  MessageId := ColumnBox_MessageLog.Rows[ItemId].Tag;
  if MessageId = -1 then Exit;

  Msg := gMySpectator.Hand.MessageLog[MessageId];
  Msg.IsReadLocal := True;
  gGame.GameInputProcess.CmdGame(gic_GameMessageLogRead, MessageId);

  // Jump to location
  fViewport.Position := KMPointF(Msg.Loc);

  // Try to highlight the house in question
  H := gHands.HousesHitTest(Msg.Loc.X, Msg.Loc.Y);

  // Do not highlight a house if it is not the one that has issued the notification
  // (happens when note is issues and house is destroyed and a new one is build in the same place)
  // NOTE: It will highlight next house built on the 'ruins' which is unoccupied to be precise
  //       even the NEW message has not been issued yet
  if (H <> nil) then
    if ((Msg.fTextID = TX_MSG_HOUSE_UNOCCUPIED) and not H.GetHasOwner
        and (gRes.HouseDat[H.HouseType].OwnerType <> ut_None) and (H.HouseType <> ht_Barracks))
    or H.ResourceDepletedMsgIssued then
      gMySpectator.Highlight := H;

  MessageLog_Update(True);
end;


// Sync displayed messages with queue
// We show only last 8 messages by design
procedure TKMGamePlayInterface.MessageLog_Update(aFullRefresh: Boolean);
var
  I, K: Integer;
  R: TKMListRow;
begin
  // Exit if synced already
  if not aFullRefresh and (fLastSyncedMessage = gMySpectator.Hand.MessageLog.CountLog) then Exit;

  // Clear the selection if a new item is added so the wrong one is not selected
  if fLastSyncedMessage <> gMySpectator.Hand.MessageLog.CountLog then
    ColumnBox_MessageLog.ItemIndex := -1;

  // Clear all rows in case gMySpectator.HandIndex was changed and MessageLog now contains less items
  for I := 0 to MAX_LOG_MSGS - 1 do
    ColumnBox_MessageLog.Rows[I] := MakeListRow(['', ''], -1);

  K := 0;
  for I := Max(gMySpectator.Hand.MessageLog.CountLog - MAX_LOG_MSGS, 0) to gMySpectator.Hand.MessageLog.CountLog - 1 do
  begin
    R := MakeListRow(['', gMySpectator.Hand.MessageLog[I].Text], I);

    if gMySpectator.Hand.MessageLog[I].Kind = mkUnit then
    begin
      R.Cells[0].Pic := MakePic(rxGui, 588);
      if gMySpectator.Hand.MessageLog[I].IsRead then
      begin
        R.Cells[1].Color := $FF0080B0;
        R.Cells[1].HighlightColor := $FF006797;
      end
      else
      begin
        R.Cells[1].Color := $FF00B0FF;
        R.Cells[1].HighlightColor := $FF4AC7FF;
      end;
    end
    else
    begin
      R.Cells[0].Pic := MakePic(rxGui, 587);
      if gMySpectator.Hand.MessageLog[I].IsRead then
      begin
        R.Cells[1].Color := $FFA0A0A0;
        R.Cells[1].HighlightColor := $FF808080;
      end
      else
      begin
        R.Cells[1].Color := $FFFFFFFF;
        R.Cells[1].HighlightColor := $FFC7C7C7;
      end;
    end;

    ColumnBox_MessageLog.Rows[K] := R;
    Inc(K);
  end;

  fLastSyncedMessage := gMySpectator.Hand.MessageLog.CountLog;
end;


// Update message stack when first log message arrives
procedure TKMGamePlayInterface.MessageStack_UpdatePositions;
var
  I: Integer;
  Pad: Integer;
begin
  Pad := Byte(fUIMode in [umMP, umSpectate]) * 2 +
         Byte(Image_MessageLog.Visible);
  for I := 0 to MAX_VISIBLE_MSGS do
    Image_Message[I].Top := Panel_Main.Height - 48 - (I + Pad) * 48;
end;


procedure TKMGamePlayInterface.Menu_Update;
begin
  if gGameApp.GameSettings.MusicOff then
    Label_Menu_Track.Caption := '-'
  else
    Label_Menu_Track.Caption := gGameApp.MusicLib.GetTrackTitle;

  Label_GameTime.Caption := Format(gResTexts[TX_GAME_TIME], [TimeToString(gGame.MissionTime)]);

  Label_Menu_Track.Enabled      := not gGameApp.GameSettings.MusicOff;
  Button_Menu_TrackUp.Enabled   := not gGameApp.GameSettings.MusicOff;
  Button_Menu_TrackDown.Enabled := not gGameApp.GameSettings.MusicOff;
end;


procedure TKMGamePlayInterface.Beacon_Cancel;
begin
  fPlacingBeacon := False; // Right click cancels it
  MinimapView.ClickableOnce := False;
  if gRes.Cursors.Cursor = kmc_Beacon then
    gRes.Cursors.Cursor := kmc_Default;
end;


procedure TKMGamePlayInterface.Beacon_Place(aLoc: TKMPointF);
begin
  // In replays we show the beacon directly without GIP. In spectator we use -1 for hand index
  case fUIMode of
    umReplay:   Alerts.AddBeacon(aLoc, gMySpectator.HandIndex, gMySpectator.Hand.FlagColor, gGameApp.GlobalTickCount + ALERT_DURATION[atBeacon]);
    umSpectate: gGame.GameInputProcess.CmdGame(gic_GameAlertBeacon, aLoc, PLAYER_NONE, gGame.Networking.NetPlayers[gGame.Networking.MyIndex].FlagColor);
    else        gGame.GameInputProcess.CmdGame(gic_GameAlertBeacon, aLoc, gMySpectator.HandIndex, gMySpectator.Hand.FlagColor);
  end;
  Beacon_Cancel;
end;


procedure TKMGamePlayInterface.Army_ActivateControls(aGroup: TKMUnitGroup);
var AcceptOrders: Boolean;
begin
  AcceptOrders := aGroup.CanTakeOrders and (fUIMode in [umSP, umMP]) and not HasLostMPGame;

  // Button_Army_GoTo.Enabled    := AcceptOrders;
  Button_Army_Stop.Enabled    := AcceptOrders;
  // Button_Army_Attack.Enabled  := AcceptOrders;
  Button_Army_RotCW.Enabled   := AcceptOrders;
  Button_Army_Storm.Enabled   := AcceptOrders and (aGroup.GroupType = gt_Melee);
  Button_Army_RotCCW.Enabled  := AcceptOrders;
  Button_Army_ForUp.Enabled   := AcceptOrders and (aGroup.Count > 1);
  Button_Army_ForDown.Enabled := AcceptOrders and (aGroup.Count > 1);
  Button_Army_Split.Enabled   := AcceptOrders and (aGroup.Count > 1);
  Button_Army_Join.Enabled    := AcceptOrders;
  Button_Army_Feed.Enabled    := AcceptOrders;
end;


procedure TKMGamePlayInterface.SetMenuState(aTactic: Boolean);
var
  I: Integer;
begin
  Button_Main[tbBuild].Enabled := not aTactic and (fUIMode in [umSP, umMP]) and not HasLostMPGame and not gMySpectator.Hand.InCinematic;
  Button_Main[tbRatio].Enabled := not aTactic and ((fUIMode in [umReplay, umSpectate]) or (not HasLostMPGame and not gMySpectator.Hand.InCinematic));
  Button_Main[tbStats].Enabled := not aTactic;

  // No loading during multiplayer games
  Button_Menu_Load.Enabled := fUIMode = umSP;
  Button_Menu_Save.Enabled := fUIMode in [umSP, umMP, umSpectate];
  Button_Menu_Quit.Enabled := fUIMode in [umSP, umMP, umSpectate];

  // Toggle gameplay options
  fGuiMenuSettings.SetAutosaveEnabled(fUIMode in [umSP, umMP, umSpectate]);

  // Chat and Allies setup should be accessible only in Multiplayer
  Image_MPChat.Visible       := fUIMode in [umMP, umSpectate];
  Label_MPChatUnread.Visible := fUIMode in [umMP, umSpectate];
  Image_MPAllies.Visible     := fUIMode in [umMP, umSpectate];

  // Message stack is visible in Replay as it shows which messages player got
  // and does not affect replay consistency

  Panel_ReplayCtrl.Visible := fUIMode = umReplay;
  Panel_ReplayFOW.Visible := fUIMode in [umSpectate, umReplay];
  Panel_ReplayFOW.Top := IfThen(fUIMode = umSpectate, 8, 58);
  if fUIMode in [umSpectate, umReplay] then
  begin
    Checkbox_ReplayFOW.Checked := False;
    Dropbox_ReplayFOW.Clear;
    for I := 0 to gHands.Count - 1 do
    if gHands[I].Enabled then
        Dropbox_ReplayFOW.Add(WrapColor(gHands[I].OwnerName, FlagColorToTextColor(gHands[I].FlagColor)), I);
    Dropbox_ReplayFOW.ItemIndex := 0;
  end;
end;


procedure TKMGamePlayInterface.ShowClock(aSpeed: Single);
begin
  Image_Clock.Visible := aSpeed <> 1;
  Label_Clock.Visible := aSpeed <> 1;
  Label_ClockSpeedup.Visible := aSpeed <> 1;
  Label_ClockSpeedup.Caption := 'x' + FloatToStr(aSpeed);

  // With slow GPUs it will keep old values till next frame, that can take some seconds
  // Thats why we refresh Clock.Caption here
  if aSpeed <> 1 then
    Label_Clock.Caption := TimeToString(gGame.MissionTime);
end;


procedure TKMGamePlayInterface.SetPause(aValue:boolean);
begin
  ReleaseDirectionSelector; // Don't restrict cursor movement to direction selection while paused
  fViewport.ReleaseScrollKeys;
  gGame.IsPaused := aValue;
  Panel_Pause.Visible := aValue;
end;


procedure TKMGamePlayInterface.ShowPlayMore(DoShow:boolean; Msg: TGameResultMsg);
begin
  ReleaseDirectionSelector;
  PlayMoreMsg := Msg;
  case Msg of
    gr_Win:       begin
                    Label_PlayMore.Caption := gResTexts[TX_GAMEPLAY_WON];
                    Button_PlayMore.Caption := gResTexts[TX_GAMEPLAY_CONTINUE_PLAYING];
                    Button_PlayQuit.Caption := gResTexts[TX_GAMEPLAY_VICTORY];
                  end;
    gr_Defeat:    begin
                    Label_PlayMore.Caption := gResTexts[TX_GAMEPLAY_LOST];
                    Button_PlayMore.Caption := gResTexts[TX_GAMEPLAY_DEFEAT_CONTINUEWATCHING];
                    Button_PlayQuit.Caption := gResTexts[TX_GAMEPLAY_DEFEAT];
                  end;
    gr_ReplayEnd: begin
                    Label_PlayMore.Caption := gResTexts[TX_GAMEPLAY_REPLAY_ENDED];
                    Button_PlayMore.Caption := gResTexts[TX_GAMEPLAY_REPLAY_CONTINUEWATCHING];
                    Button_PlayQuit.Caption := gResTexts[TX_GAMEPLAY_QUIT_TO_MENU];
                  end;
    else if DoShow then Assert(false,'Wrong message in ShowPlayMore'); // Can become hidden with any message
  end;
  Panel_PlayMore.Visible := DoShow;
end;


procedure TKMGamePlayInterface.ShowMPPlayMore(Msg: TGameResultMsg);
begin
  ReleaseDirectionSelector;
  PlayMoreMsg := Msg;
  case Msg of
    gr_Win:       begin
                    Label_MPPlayMore.Caption := gResTexts[TX_GAMEPLAY_WON];
                    Button_MPPlayMore.Caption := gResTexts[TX_GAMEPLAY_CONTINUE_PLAYING];
                    Button_MPPlayQuit.Caption := gResTexts[TX_GAMEPLAY_VICTORY];
                  end;
    gr_Defeat:    begin
                    // Refresh it so that menu buttons become disabled
                    SetMenuState(gGame.MissionMode = mm_Tactic);
                    // Close e.g. the build menu if it was open
                    SwitchPage(Button_Back);

                    Label_MPPlayMore.Caption := gResTexts[TX_GAMEPLAY_LOST];
                    Button_MPPlayMore.Caption := gResTexts[TX_GAMEPLAY_DEFEAT_CONTINUEWATCHING];
                    Button_MPPlayQuit.Caption := gResTexts[TX_GAMEPLAY_DEFEAT];
                  end;
    else Assert(false,'Wrong message in ShowMPPlayMore');
  end;
  Panel_MPPlayMore.Visible := true;
end;


procedure TKMGamePlayInterface.PlayMoreClick(Sender: TObject);
begin
  Panel_PlayMore.Hide; // Hide anyways

  if Sender = Button_PlayQuit then
    case PlayMoreMsg of
      gr_Win:       gGameApp.Stop(gr_Win);
      gr_Defeat:    gGameApp.Stop(gr_Defeat);
      gr_ReplayEnd: gGameApp.Stop(gr_ReplayEnd);
    end
  else // GameStop has Destroyed our Sender by now
  if Sender = Button_PlayMore then
    case PlayMoreMsg of
      gr_Win:       begin gGame.GameHold(false, gr_Win); end;
      gr_Defeat:    begin gGame.GameHold(false, gr_Defeat); end;
      gr_ReplayEnd: begin gGame.SkipReplayEndCheck := true; gGame.GameHold(false, gr_ReplayEnd); end;
    end;
end;


procedure TKMGamePlayInterface.MPPlayMoreClick(Sender: TObject);
begin
  Panel_MPPlayMore.Hide;

  if Sender = Button_MPPlayQuit then
    case PlayMoreMsg of
      gr_Win:       gGameApp.Stop(gr_Win);
      gr_Defeat:    gGameApp.Stop(gr_Defeat);
      gr_ReplayEnd: gGameApp.Stop(gr_ReplayEnd);
    end
  // If they click continue no other action is necessary, the game is still running
end;


procedure TKMGamePlayInterface.ShowNetworkLag(aShow: Boolean; aPlayers: TKMByteArray; IsHost: Boolean);
var
  I: Integer;
  txt: UnicodeString;
begin
  if aShow then ReleaseDirectionSelector;
  if not aShow then // Reset the confirm when we hide this screen so it's not on confirm when it reshows
  begin
    Panel_NetWaitConfirm.Hide;
    Panel_NetWaitButtons.Show;
  end;

  if gGame.Networking.IsReconnecting then
  begin
    txt := gResTexts[TX_MULTIPLAYER_ATTEMPT_RECONNECTING];
    Button_NetDropPlayers.Visible := False;
    fNetWaitDropPlayersDelayStarted := 0;
    Label_NetDropPlayersDelay.Caption := '';
  end
  else
  begin
    txt := gResTexts[TX_MULTIPLAYER_WAITING] + ' ';
    for I := Low(aPlayers) to High(aPlayers) do
      txt := txt + UnicodeString(gGame.Networking.NetPlayers[aPlayers[I]].Nikname) + IfThen(I <> High(aPlayers), ', ');

    Button_NetDropPlayers.Visible := IsHost;

    if not aShow then
      fNetWaitDropPlayersDelayStarted := 0
    else
      if fNetWaitDropPlayersDelayStarted = 0 then
      begin
        Label_NetDropPlayersDelay.Caption := '';
        fNetWaitDropPlayersDelayStarted := TimeGet; // Initialise it
        Button_NetDropPlayers.Disable; // Must wait the minimum time before enabling it
      end;
  end;

  Label_NetWait.Caption := txt;
  Panel_NetWait.Visible := aShow;
end;


procedure TKMGamePlayInterface.SetScriptedOverlay(aText: UnicodeString);
begin
  Label_ScriptedOverlay.Caption := aText;
  UpdateOverlayControls;
end;


procedure TKMGamePlayInterface.HideOverlay(Sender: TObject);
begin
  Label_ScriptedOverlay.Visible := not Label_ScriptedOverlay.Visible;
  if not Label_ScriptedOverlay.Visible then
  begin
    Label_OverlayHide.Hide;
    Label_OverlayShow.Show;
    Button_ScriptedOverlay.Hint := gResTexts[TX_GAMEPLAY_OVERLAY_SHOW];
  end
  else
  begin
    Label_OverlayHide.Show;
    Label_OverlayShow.Hide;
    Button_ScriptedOverlay.Hint := gResTexts[TX_GAMEPLAY_OVERLAY_HIDE];
  end;
end;


procedure TKMGamePlayInterface.UpdateOverlayControls;
begin
  Button_ScriptedOverlay.Visible := Label_ScriptedOverlay.Caption <> '';
  Label_OverlayShow.Visible := (Label_ScriptedOverlay.Caption <> '') and not (Label_ScriptedOverlay.Visible);
  Label_OverlayHide.Visible := (Label_ScriptedOverlay.Caption <> '') and (Label_ScriptedOverlay.Visible);
end;


procedure TKMGamePlayInterface.NetWaitClick(Sender: TObject);
begin
  if Sender = Button_NetQuit then
  begin
    Panel_NetWaitButtons.Hide;
    Label_NetWaitConfirm.Caption := gResTexts[TX_GAMEPLAY_CONFIRM_QUIT];
    Button_NetConfirmYes.Caption := gResTexts[TX_GAMEPLAY_QUIT_TO_MENU];
    Panel_NetWaitConfirm.Show;
  end else
  if Sender = Button_NetDropPlayers then
  begin
    Panel_NetWaitButtons.Hide;
    Label_NetWaitConfirm.Caption := gResTexts[TX_GAMEPLAY_CONFIRM_DROP];
    Button_NetConfirmYes.Caption := gResTexts[TX_GAMEPLAY_DROP_PLAYERS];
    Panel_NetWaitConfirm.Show;
  end else
  if Sender = Button_NetConfirmNo then
  begin
    Panel_NetWaitConfirm.Hide;
    Panel_NetWaitButtons.Show;
  end else
  if Sender = Button_NetConfirmYes then
  begin
    Panel_NetWaitConfirm.Hide;
    if Button_NetConfirmYes.Caption = gResTexts[TX_GAMEPLAY_DROP_PLAYERS] then
      gGame.WaitingPlayersDrop else
    if Button_NetConfirmYes.Caption = gResTexts[TX_GAMEPLAY_QUIT_TO_MENU] then
      gGameApp.Stop(gr_Cancel);
  end
  else Assert(false, 'Wrong Sender in NetWaitClick');
end;


procedure TKMGamePlayInterface.DirectionCursorShow(X,Y: Integer; Dir: TKMDirection);
begin
  Image_DirectionCursor.Visible := True;
  Image_DirectionCursor.Left    := X + gRes.Cursors.CursorOffset(Dir).X;
  Image_DirectionCursor.Top     := Y + gRes.Cursors.CursorOffset(Dir).Y;
  Image_DirectionCursor.TexID   := gRes.Cursors.CursorTexID(Dir);
end;


procedure TKMGamePlayInterface.DirectionCursorHide;
begin
  Image_DirectionCursor.Visible := False;
end;


procedure TKMGamePlayInterface.ReleaseDirectionSelector;
begin
  if SelectingTroopDirection then
  begin
    // Reset the cursor position as it will have moved during direction selection
    SetCursorPos(fMain.ClientToScreen(SelectingDirPosition).X, fMain.ClientToScreen(SelectingDirPosition).Y);
    fMain.ApplyCursorRestriction; // Reset the cursor restrictions from selecting direction
    SelectingTroopDirection := False;
    gRes.Cursors.Cursor := kmc_Default; // Reset direction selection cursor when mouse released
    DirectionCursorHide;
  end;
end;


function TKMGamePlayInterface.HasLostMPGame: Boolean;
begin
  Result := (fUIMode = umMP) and (gMySpectator.Hand.AI.WonOrLost = wol_Lost);
end;


// Access chat messages history to copy it over to lobby chat
function TKMGamePlayInterface.GetChatState: TChatState;
begin
  Result := fGuiGameChat.GetChatState;
end;


procedure TKMGamePlayInterface.SetChatState(const aChatState: TChatState);
begin
  fGuiGameChat.SetChatState(aChatState);

  if aChatState.ChatText <> '' then
    fGuiGameChat.Show;
end;


// Assign Object to a Key
// we use ID to avoid use of pointer counter
procedure TKMGamePlayInterface.Selection_Assign(aId: Word; aObject: TObject);
begin
  if not InRange(aId, 0, 9) then Exit;

  if aObject is TKMUnit then
    fSelection[aId] := TKMUnit(aObject).UID
  else
  if aObject is TKMHouse then
    fSelection[aId] := TKMHouse(aObject).UID
  else
  if aObject is TKMUnitGroup then
    fSelection[aId] := TKMUnitGroup(aObject).UID
  else
    fSelection[aId] := -1;

  gGame.GameInputProcess.CmdGame(gic_GameHotkeySet, aId, fSelection[aId]);
end;


procedure TKMGamePlayInterface.Selection_Link(aId: Word; aObject: TObject);
var
  G: TKMUnitGroup;
begin
  G := gHands.GetGroupByUID(fSelection[aId]);
  if (aObject <> G) and (aObject is TKMUnitGroup) and (G is TKMUnitGroup)
  and (TKMUnitGroup(aObject).GroupType = G.GroupType) then
  begin
    gSoundPlayer.PlayWarrior(TKMUnitGroup(aObject).UnitType, sp_Join); // In SP joining is instant, aObject does not exist after that
    gGame.GameInputProcess.CmdArmy(gic_ArmyLink, TKMUnitGroup(aObject), G);
  end;
end;


procedure TKMGamePlayInterface.Selection_Select(aId: Word);
var
  OldSelected: TObject;
begin
  if gMySpectator.Hand.InCinematic then
    Exit;

  if not InRange(aId, 0, 9) then Exit;

  if fSelection[aId] <> -1 then
  begin
    OldSelected := gMySpectator.Selected;
    gMySpectator.Selected := gHands.GetUnitByUID(fSelection[aId]);
    if gMySpectator.Selected <> nil then
    begin
      if TKMUnit(gMySpectator.Selected).IsDeadOrDying then
      begin
        gMySpectator.Selected := nil; // Don't select dead/dying units
        Exit;
      end;
      if (OldSelected <> gMySpectator.Selected) and (fUIMode in [umSP, umMP]) and not HasLostMPGame then
        gSoundPlayer.PlayCitizen(TKMUnit(gMySpectator.Selected).UnitType, sp_Select);
      // Selecting a unit twice is the shortcut to center on that unit
      if OldSelected = gMySpectator.Selected then
        fViewport.Position := TKMUnit(gMySpectator.Selected).PositionF;
    end
    else
    begin
      gMySpectator.Selected := gHands.GetHouseByUID(fSelection[aId]);
      if gMySpectator.Selected <> nil then
      begin
        if TKMHouse(gMySpectator.Selected).IsDestroyed then
        begin
          gMySpectator.Selected := nil; // Don't select destroyed houses
          Exit;
        end;
        // Selecting a house twice is the shortcut to center on that house
        if OldSelected = gMySpectator.Selected then
          fViewport.Position := KMPointF(TKMHouse(gMySpectator.Selected).GetEntrance);
      end
      else
      begin
        gMySpectator.Selected := gHands.GetGroupByUID(fSelection[aId]);
        if (gMySpectator.Selected = nil) or TKMUnitGroup(gMySpectator.Selected).IsDead then
        begin
          gMySpectator.Selected := nil; // Don't select dead groups
          Exit;
        end;
        TKMUnitGroup(gMySpectator.Selected).SelectFlagBearer;
        if (OldSelected <> gMySpectator.Selected) and (fUIMode in [umSP, umMP]) and not HasLostMPGame then
          gSoundPlayer.PlayWarrior(TKMUnitGroup(gMySpectator.Selected).SelectedUnit.UnitType, sp_Select);
        // Selecting a group twice is the shortcut to center on that group
        if OldSelected = gMySpectator.Selected then
          fViewport.Position := TKMUnitGroup(gMySpectator.Selected).SelectedUnit.PositionF;
      end;
    end;

  end
  else
    gMySpectator.Selected := nil;

  // In a replay we want in-game statistics (and other things) to be shown for the owner of the last select object
  if fUIMode in [umReplay, umSpectate] then
  begin
    if gMySpectator.Selected is TKMHouse      then gMySpectator.HandIndex := TKMHouse    (gMySpectator.Selected).Owner;
    if gMySpectator.Selected is TKMUnit       then gMySpectator.HandIndex := TKMUnit     (gMySpectator.Selected).Owner;
    if gMySpectator.Selected is TKMUnitGroup  then gMySpectator.HandIndex := TKMUnitGroup(gMySpectator.Selected).Owner;
    Dropbox_ReplayFOW.SelectByTag(gMySpectator.HandIndex);
    if Checkbox_ReplayFOW.Checked then
      gMySpectator.FOWIndex := gMySpectator.HandIndex
    else
      gMySpectator.FOWIndex := -1;
    fMinimap.Update(False); // Force update right now so FOW doesn't appear to lag
  end;
end;


procedure TKMGamePlayInterface.ChatMessage(const aData: UnicodeString);
begin
  fGuiGameChat.ChatMessage(aData);

  if not fGuiGameChat.Visible then
    Label_MPChatUnread.Caption := IntToStr(StrToIntDef(Label_MPChatUnread.Caption, 0) + 1); // New message
end;


procedure TKMGamePlayInterface.AlliesOnPlayerSetup(Sender: TObject);
var
  I, NetI, LocaleID: Integer;
begin
  Image_AlliesHostStar.Hide;
  // Can't vote if we already have, and spectators don't get to vote unless there's only spectators left
  Button_Menu_ReturnLobby.Enabled := not gGame.Networking.NetPlayers[gGame.Networking.MyIndex].VotedYes
                                     and (gGame.Networking.NetPlayers.HasOnlySpectators
                                          or not gGame.Networking.NetPlayers[gGame.Networking.MyIndex].IsSpectator);

  AlliesUpdateMapping;
  for I := 0 to MAX_LOBBY_SLOTS-1 do
  begin
    NetI := fAlliesToNetPlayers[I];
    if NetI = -1 then
    begin
      Label_AlliesPlayer[I].Hide;
      DropBox_AlliesTeam[I].Hide;
      Label_AlliesTeam[I].Hide;
    end
    else
    begin
      // Show players locale flag
      if gGame.Networking.NetPlayers[NetI].IsComputer then
        Image_AlliesFlag[I].TexID := 62 // PC icon
      else
      begin
        LocaleID := gResLocales.IndexByCode(gGame.Networking.NetPlayers[NetI].LangCode);
        if LocaleID <> -1 then
          Image_AlliesFlag[I].TexID := gResLocales[LocaleID].FlagSpriteID
        else
          Image_AlliesFlag[I].TexID := 0;
      end;
      if gGame.Networking.HostIndex = NetI then
      begin
        Image_AlliesHostStar.Visible := True;
        Image_AlliesHostStar.Left := 190+(I div 5)*380;
        Image_AlliesHostStar.Top := 80+(I mod 5)*20;
      end;

      if gGame.Networking.NetPlayers[NetI].IsHuman then
        Label_AlliesPlayer[I].Caption := UnicodeString(gGame.Networking.NetPlayers[NetI].Nikname)
      else
        Label_AlliesPlayer[I].Caption := gHands[gGame.Networking.NetPlayers[NetI].StartLocation-1].OwnerName;

      if gGame.Networking.NetPlayers[NetI].IsSpectator then
      begin
        Label_AlliesPlayer[I].FontColor := gGame.Networking.NetPlayers[NetI].FlagColor;
        DropBox_AlliesTeam[I].ItemIndex := 0;
        Label_AlliesTeam[I].Caption := gResTexts[TX_LOBBY_SPECTATOR];
      end
      else
      begin
        Label_AlliesPlayer[I].FontColor := gHands[gGame.Networking.NetPlayers[NetI].StartLocation - 1].FlagColor;
        DropBox_AlliesTeam[I].ItemIndex := gGame.Networking.NetPlayers[NetI].Team;
        if gGame.Networking.NetPlayers[NetI].Team = 0 then
          Label_AlliesTeam[I].Caption := '-'
        else
          Label_AlliesTeam[I].Caption := IntToStr(gGame.Networking.NetPlayers[NetI].Team);
      end;
      // Strikethrough for disconnected players
      Image_AlliesFlag[I].Enabled := not gGame.Networking.NetPlayers[NetI].Dropped;
      Label_AlliesPlayer[I].Strikethrough := gGame.Networking.NetPlayers[NetI].Dropped;
      Label_AlliesTeam[I].Strikethrough := gGame.Networking.NetPlayers[NetI].Dropped;
      Label_AlliesPing[I].Strikethrough := gGame.Networking.NetPlayers[NetI].Dropped;
      DropBox_AlliesTeam[I].Enabled := (NetI = gGame.Networking.MyIndex); // Our index
      DropBox_AlliesTeam[I].Hide; // Use label for demos until we fix exploits
    end;
  end;
end;


procedure TKMGamePlayInterface.AlliesOnPingInfo(Sender: TObject);
var
  I, NetI: Integer;
  ping: Word;
  fps: Cardinal;
begin
  AlliesUpdateMapping;
  for I := 0 to MAX_LOBBY_SLOTS - 1 do
  begin
    NetI := fAlliesToNetPlayers[I];
    if (I < gGame.Networking.NetPlayers.Count) and (gGame.Networking.NetPlayers[NetI].IsHuman) then
    begin
      ping := gGame.Networking.NetPlayers[NetI].GetInstantPing;
      fps := gGame.Networking.NetPlayers[NetI].FPS;
      Label_AlliesPing[I].Caption := WrapColor(IntToStr(ping), GetPingColor(ping)) + ' / ' +
                                     WrapColor(IntToStr(fps), GetFPSColor(fps));
    end
    else
      Label_AlliesPing[I].Caption := '';
  end;
end;


procedure TKMGamePlayInterface.AlliesTeamChange(Sender: TObject);
var I: Integer;
begin
  for I := 0 to MAX_LOBBY_SLOTS - 1 do
    if (Sender = DropBox_AlliesTeam[I]) and DropBox_AlliesTeam[I].Enabled then
      gGame.GameInputProcess.CmdGame(gic_GameTeamChange, I+1, DropBox_AlliesTeam[I].ItemIndex);
end;


procedure TKMGamePlayInterface.KeyDown(Key: Word; Shift: TShiftState);
var Rect: TKMRect;
begin
  if gGame.IsPaused and (fUIMode in [umSP, umMP]) then Exit;

  if fMyControls.KeyDown(Key, Shift) then
  begin
    fViewport.ReleaseScrollKeys; // Release the arrow keys when you open a window with an edit to stop them becoming stuck
    Exit;
  end;

  if Key = gResKeys[SC_SCROLL_LEFT].Key  then fViewport.ScrollKeyLeft  := True;
  if Key = gResKeys[SC_SCROLL_RIGHT].Key then fViewport.ScrollKeyRight := True;
  if Key = gResKeys[SC_SCROLL_UP].Key    then fViewport.ScrollKeyUp    := True;
  if Key = gResKeys[SC_SCROLL_DOWN].Key  then fViewport.ScrollKeyDown  := True;
  if Key = gResKeys[SC_ZOOM_IN].Key      then fViewport.ZoomKeyIn      := True;
  if Key = gResKeys[SC_ZOOM_OUT].Key     then fViewport.ZoomKeyOut     := True;
    // As we don't have names for teams in SP we only allow showing team names in MP or MP replays
  if (Key = gResKeys[SC_SHOW_TEAMS].Key) then
    if (fUIMode in [umMP, umSpectate]) or (gGame.GameMode = gmReplayMulti) then //Only MP replays
    begin
      fShowTeamNames := True;
      // Update it immediately so there's no 300ms lag after pressing the key
      fTeamNames.Clear;
      Rect := fViewport.GetMinimapClip;
      gHands.GetUnitsInRect(Rect, fTeamNames);
    end;
end;


// Note: we deliberately don't pass any Keys to MyControls when game is not running
// thats why MyControls.KeyUp is only in gsRunning clause
// Ignore all keys if game is on 'Pause'
procedure TKMGamePlayInterface.KeyUp(Key: Word; Shift: TShiftState);
var
  LastAlert: TKMAlert;
  SelectId: Integer;
begin
  if gGame.IsPaused and (fUIMode = umSP) then
  begin
    if Key = gResKeys[SC_PAUSE].Key then
      SetPause(False);
    Exit;
  end;

  if fMyControls.KeyUp(Key, Shift) then Exit;

  if (fUIMode = umReplay) and (Key = Ord(SC_PAUSE)) then
  begin
    if Button_ReplayPause.Enabled then
      ReplayClick(Button_ReplayPause)
    else if Button_ReplayResume.Enabled then
      ReplayClick(Button_ReplayResume);
  end;

  // These keys are allowed during replays
  // Scrolling
  if Key = gResKeys[SC_SCROLL_LEFT].Key  then fViewport.ScrollKeyLeft  := False;
  if Key = gResKeys[SC_SCROLL_RIGHT].Key then fViewport.ScrollKeyRight := False;
  if Key = gResKeys[SC_SCROLL_UP].Key    then fViewport.ScrollKeyUp    := False;
  if Key = gResKeys[SC_SCROLL_DOWN].Key  then fViewport.ScrollKeyDown  := False;
  if Key = gResKeys[SC_ZOOM_IN].Key      then fViewport.ZoomKeyIn      := False;
  if Key = gResKeys[SC_ZOOM_OUT].Key     then fViewport.ZoomKeyOut     := False;
  if Key = gResKeys[SC_ZOOM_RESET].Key   then fViewport.ResetZoom;
  if Key = gResKeys[SC_SHOW_TEAMS].Key   then fShowTeamNames := False;
  if Key = gResKeys[SC_BEACON].Key then
    if not SelectingTroopDirection then
    begin
      fPlacingBeacon := True;
      MinimapView.ClickableOnce := True;
      gRes.Cursors.Cursor := kmc_Beacon;
    end;
  if Key = gResKeys[SC_CLOSE_MENU].Key then
  begin
    // Progressively hide open elements on Esc
    if fJoiningGroups then
      Army_HideJoinMenu(nil)
    else
    if ShownMessage <> -1 then
      Message_Close(nil)
    else
    if fGuiGameChat.Visible then
      fGuiGameChat.Hide
    else
    if Panel_Allies.Visible then
      Allies_Close(nil)
    else
    if Panel_MessageLog.Visible then
      MessageLog_Close(nil)
    else
    if Button_Back.Visible then
      SwitchPage(Button_Back);
  end;

  // Dynamic key-binding means we cannot use "case of"
  if Key = gResKeys[SC_SELECT_1].Key  then SelectId := 0 else
  if Key = gResKeys[SC_SELECT_2].Key  then SelectId := 1 else
  if Key = gResKeys[SC_SELECT_3].Key  then SelectId := 2 else
  if Key = gResKeys[SC_SELECT_4].Key  then SelectId := 3 else
  if Key = gResKeys[SC_SELECT_5].Key  then SelectId := 4 else
  if Key = gResKeys[SC_SELECT_6].Key  then SelectId := 5 else
  if Key = gResKeys[SC_SELECT_7].Key  then SelectId := 6 else
  if Key = gResKeys[SC_SELECT_8].Key  then SelectId := 7 else
  if Key = gResKeys[SC_SELECT_9].Key  then SelectId := 8 else
  if Key = gResKeys[SC_SELECT_10].Key then SelectId := 9 else
    SelectId := -1;

  if SelectId <> -1 then
    if (ssCtrl in Shift) then
      Selection_Assign(SelectId, gMySpectator.Selected)
    else
    if (ssShift in Shift) and (fUIMode in [umSP, umMP]) then
      Selection_Link(SelectId, gMySpectator.Selected)
    else
      Selection_Select(SelectId);

    // Menu shortcuts
  if Key = gResKeys[SC_MENU_BUILD].Key then
    if Button_Main[tbBuild].Enabled then
      SwitchPage(Button_Main[tbBuild]);

  if Key = gResKeys[SC_MENU_RATIO].Key then
    if Button_Main[tbRatio].Enabled then
      SwitchPage(Button_Main[tbRatio]);

  if Key = gResKeys[SC_MENU_STATS].Key then
    if Button_Main[tbStats].Enabled then
      SwitchPage(Button_Main[tbStats]);

  if Key = gResKeys[SC_MENU_MENU].Key then
    SwitchPage(Button_Main[tbMenu]);

  if (fUIMode in [umSP, umReplay]) or MULTIPLAYER_SPEEDUP then
  begin
    // Game speed/pause: Not available in multiplayer mode
    if Key = gResKeys[SC_SPEEDUP_1].Key then gGame.SetGameSpeed(1, False);
    if Key = gResKeys[SC_SPEEDUP_2].Key then gGame.SetGameSpeed(gGameApp.GameSettings.SpeedMedium, True);
    if Key = gResKeys[SC_SPEEDUP_3].Key then gGame.SetGameSpeed(gGameApp.GameSettings.SpeedFast, True);
    if Key = gResKeys[SC_SPEEDUP_4].Key then gGame.SetGameSpeed(gGameApp.GameSettings.SpeedVeryFast, True);
  end;

  // All the following keys don't work in Replay, because they alter game state
  // which is nonsense
  // thus the easy way to make that is to exit now
  if fUIMode = umReplay then Exit;

  // Messages
  if Key = gResKeys[SC_CENTER_ALERT].Key then
  begin
    // Spacebar centers you on the latest alert
    LastAlert := fAlerts.GetLatestAlert;
    if LastAlert <> nil then
      fViewport.Position := LastAlert.Loc;
  end;
  if Key = gResKeys[SC_DELETE_MSG].Key then Button_MessageDelete.Click;
  if Key = gResKeys[SC_CHAT_MP].Key then            // Enter is the shortcut to bring up chat in multiplayer
    if (fUIMode in [umMP, umSpectate]) and not fGuiGameChat.Visible then
    begin
      Allies_Close(nil);
      Message_Close(nil);
      MessageLog_Close(nil);
      Label_MPChatUnread.Caption := ''; // No unread messages
      fGuiGameChat.Show;
    end;

    // Standard army shortcuts from KaM
  if Key = gResKeys[SC_ARMY_HALT].Key then
    if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_Stop.Click;
  if Key = gResKeys[SC_ARMY_LINK].Key then
    if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_Join.Click;
  if Key = gResKeys[SC_ARMY_SPLIT].Key then
    if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_Split.Click;

    // Additional hotkeys for all group orders
  if Key = gResKeys[SC_ARMY_FOOD].Key then
    if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_Feed.Click;
  if Key = gResKeys[SC_ARMY_STORM].Key then
    if Panel_Army.Visible and Button_Army_Storm.Enabled and not SelectingTroopDirection then Button_Army_Storm.Click;
  if Key = gResKeys[SC_ARMY_ADD_LINE].Key then
    if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_ForDown.Click;
  if Key = gResKeys[SC_ARMY_DEL_LINE].Key then
    if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_ForUp.Click;
  if Key = gResKeys[SC_ARMY_ROTATE_CW].Key then
    if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_RotCW.Click;
  if Key = gResKeys[SC_ARMY_ROTATE_CCW].Key then
    if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_RotCCW.Click;

    // General function keys
  if Key = gResKeys[SC_PAUSE].Key then
    if (fUIMode = umSP) then SetPause(True); // Display pause overlay

  { Temporary cheat codes }
  if DEBUG_CHEATS and (MULTIPLAYER_CHEATS or (fUIMode = umSP)) then
  begin
    if Key = gResKeys[SC_DEBUG_REVEALMAP].Key then gGame.GameInputProcess.CmdTemp(gic_TempRevealMap);
    if Key = gResKeys[SC_DEBUG_VICTORY].Key   then gGame.GameInputProcess.CmdTemp(gic_TempVictory);
    if Key = gResKeys[SC_DEBUG_DEFEAT].Key    then gGame.GameInputProcess.CmdTemp(gic_TempDefeat);
    if Key = gResKeys[SC_DEBUG_ADDSCOUT].Key  then gGame.GameInputProcess.CmdTemp(gic_TempAddScout, gGameCursor.Cell);
  end;
end;


// 1. Process Controls
// 2. Show SelectingTroopDirection
procedure TKMGamePlayInterface.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
  procedure HandleFieldLMBDown(P: TKMPoint; aFieldType: TFieldType);
  begin
    if gMySpectator.Hand.CanAddFakeFieldPlan(P, aFieldType) then
    begin
      gGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, aFieldType);
      LastDragPoint := gGameCursor.Cell;
      gGameCursor.Tag1 := Byte(cfmPlan);
    end else if gMySpectator.Hand.CanRemFakeFieldPlan(P, aFieldType) then
    begin
      gGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, aFieldType);
      LastDragPoint := gGameCursor.Cell;
      // Set cursor into "Erase" mode, so dragging it will erase next tiles with the same field type
      gGameCursor.Tag1 := Byte(cfmErase);
    end;
  end;
var
  Group: TKMUnitGroup;
  Obj: TObject;
  canWalkTo: Boolean;
  MyRect: TRect;
  P: TKMPoint;
begin
  fMyControls.MouseDown(X, Y, Shift, Button);

  if (gGame.IsPaused and (fUIMode in [umSP, umMP])) or (fMyControls.CtrlOver <> nil)
  or gMySpectator.Hand.InCinematic then
    Exit;

  if (Button = mbMiddle) then
  begin
     fDragScrolling := True;
     // Restrict the cursor to the window, for now.
     //TODO: Allow one to drag out of the window, and still capture.
     {$IFDEF MSWindows}
       MyRect := fMain.ClientRect;
       ClipCursor(@MyRect);
     {$ENDIF}
     fDragScrollingCursorPos.X := X;
     fDragScrollingCursorPos.Y := Y;
     fDragScrollingViewportPos.X := fViewport.Position.X;
     fDragScrollingViewportPos.Y := fViewport.Position.Y;
     gRes.Cursors.Cursor := kmc_Drag;
     Exit;
  end;

  if SelectingTroopDirection then
  begin
    fMain.ApplyCursorRestriction; // Reset the cursor restrictions from selecting direction
    SelectingTroopDirection := false;
    DirectionCursorHide;
  end;

  //Handle field planss
  if Button = mbLeft then
  begin
    P := gGameCursor.Cell; // Get cursor position tile-wise
    if gMySpectator.Hand.FogOfWar.CheckTileRevelation(P.X, P.Y) > 0 then
      case gGameCursor.Mode of
        cmRoad:   HandleFieldLMBDown(P, ft_Road);
        cmField:  HandleFieldLMBDown(P, ft_Corn);
        cmWine:   HandleFieldLMBDown(P, ft_Wine);
      end;
  end;

  // See if we can show DirectionSelector
  if (Button = mbRight)
  and (fUIMode in [umSP, umMP])
  and not HasLostMPGame
  and not fJoiningGroups
  and not fPlacingBeacon
  and (gMySpectator.Selected is TKMUnitGroup) then
  begin
    Group := TKMUnitGroup(gMySpectator.Selected);
    Obj := gMySpectator.HitTestCursor;

    canWalkTo := True;

    // Group can walk to allies units place
    if Obj is TKMUnit then
      canWalkTo := (gMySpectator.Hand.Alliances[TKMUnit(Obj).Owner] = at_Ally);

    // Can't walk on to a house
    if Obj is TKMHouse then
      canWalkTo := False;

    if canWalkTo then
    begin
      if Group.CanWalkTo(gGameCursor.Cell, 0) then
      begin
        SelectingTroopDirection := True; // MouseMove will take care of cursor changing
        // Restrict the cursor to inside the main panel so it does not get jammed when used near
        // the edge of the window in windowed mode
        {$IFDEF MSWindows}
        MyRect := fMain.ClientRect;
        ClipCursor(@MyRect);
        {$ENDIF}
        // Now record it as Client XY
        SelectingDirPosition.X := X;
        SelectingDirPosition.Y := Y;
        SelectedDirection := dir_NA;
        DirectionCursorShow(X, Y, SelectedDirection);
        gRes.Cursors.Cursor := kmc_Invisible;
      end
      else
        gSoundPlayer.Play(sfx_CantPlace, gGameCursor.Cell, False, 4);
    end;
  end;
end;


// 1. Process Controls
// 2. Perform SelectingTroopDirection if it is active
// 3. Display various cursors depending on whats below (might be called often)
procedure TKMGamePlayInterface.MouseMove(Shift: TShiftState; X,Y: Integer);
  procedure HandleFieldLMBDrag(P: TKMPoint; aFieldType: TFieldType);
  begin
    if not KMSamePoint(LastDragPoint, P) then
      if (gMySpectator.Hand.CanAddFakeFieldPlan(P, aFieldType)) and (gGameCursor.Tag1 = Byte(cfmPlan)) then
      begin
        gGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, aFieldType);
        LastDragPoint := gGameCursor.Cell;
      end else if (gMySpectator.Hand.CanRemFakeFieldPlan(P, aFieldType)) and (gGameCursor.Tag1 = Byte(cfmErase)) then
      begin
        gGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, aFieldType);
        LastDragPoint := gGameCursor.Cell;
      end;
  end;
var
  DeltaX, DeltaY, DeltaDistanceSqr: integer;
  NewPoint: TPoint;
  Obj: TObject;
  P: TKMPoint;
  VP: TKMPointF;
  Group: TKMUnitGroup;
begin
  if fDragScrolling then
  begin
    VP.X := fDragScrollingViewportPos.X + (fDragScrollingCursorPos.X - X) / (CELL_SIZE_PX * fViewport.Zoom);
    VP.Y := fDragScrollingViewportPos.Y + (fDragScrollingCursorPos.Y - Y) / (CELL_SIZE_PX * fViewport.Zoom);
    fViewport.Position := VP;
    Exit;
  end;

  fMyControls.MouseMove(X,Y,Shift);

  if fPlacingBeacon then
  begin
    // Beacons are a special case, the cursor should be shown over controls to (you can place it on the minimap)
    if fMyControls.CtrlOver = nil then
      UpdateGameCursor(X,Y,Shift); // Keep the game cursor up to date
    gRes.Cursors.Cursor := kmc_Beacon;
    Exit;
  end;

  if (fMyControls.CtrlOver is TKMDragger) or (fMyControls.CtrlDown is TKMDragger) then Exit;

  if (fMyControls.CtrlOver <> nil)
  and (fMyControls.CtrlOver <> Image_DirectionCursor)
  and not SelectingTroopDirection then
  begin
    // kmc_Edit and kmc_DragUp are handled by Controls.MouseMove (it will reset them when required)
    if not fViewport.Scrolling and not (gRes.Cursors.Cursor in [kmc_Edit,kmc_DragUp]) then
      gRes.Cursors.Cursor := kmc_Default;
    Exit;
  end
  else
    DisplayHint(nil); // Clear shown hint

  if gGame.IsPaused and (fUIMode in [umSP, umMP]) then Exit;

  if SelectingTroopDirection then
  begin
    DeltaX := SelectingDirPosition.X - X;
    DeltaY := SelectingDirPosition.Y - Y;
    DeltaDistanceSqr := Sqr(DeltaX)+Sqr(DeltaY);
    // Manually force the cursor to remain within a circle (+2 to avoid infinite loop due to rounding)
    if DeltaDistanceSqr > Sqr(DirCursorCircleRadius+2) then
    begin
      DeltaX := Round(DeltaX / Sqrt(DeltaDistanceSqr) * DirCursorCircleRadius);
      DeltaY := Round(DeltaY / Sqrt(DeltaDistanceSqr) * DirCursorCircleRadius);
      NewPoint := fMain.ClientToScreen(SelectingDirPosition);
      NewPoint.X := NewPoint.X - DeltaX;
      NewPoint.Y := NewPoint.Y - DeltaY;
      SetCursorPos(NewPoint.X, NewPoint.Y);
    end;

    // Compare cursor position and decide which direction it is
    SelectedDirection := KMGetCursorDirection(DeltaX, DeltaY);
    // Update the cursor based on this direction and negate the offset
    DirectionCursorShow(SelectingDirPosition.X, SelectingDirPosition.Y, SelectedDirection);
    gRes.Cursors.Cursor := kmc_Invisible; // Keep it invisible, just in case
    Exit;
  end;

  UpdateGameCursor(X,Y,Shift);

  if ssLeft in Shift then // Only allow placing of roads etc. with the left mouse button
  begin
    P := gGameCursor.Cell; // Get cursor position tile-wise
    if gMySpectator.Hand.FogOfWar.CheckTileRevelation(P.X, P.Y) > 0 then
      case gGameCursor.Mode of
        cmRoad:   HandleFieldLMBDrag(P, ft_Road);
        cmField:  HandleFieldLMBDrag(P, ft_Corn);
        cmWine:   HandleFieldLMBDrag(P, ft_Wine);
        cmErase:  if not KMSamePoint(LastDragPoint, P) then
                  begin
                    if gMySpectator.Hand.BuildList.HousePlanList.HasPlan(P) then
                    begin
                      gGame.GameInputProcess.CmdBuild(gic_BuildRemoveHousePlan, P);
                      LastDragPoint := gGameCursor.Cell;
                    end
                    else
                      if (gMySpectator.Hand.BuildList.FieldworksList.HasFakeField(P) <> ft_None) then
                      begin
                        gGame.GameInputProcess.CmdBuild(gic_BuildRemoveFieldPlan, P); // Remove any plans
                        LastDragPoint := gGameCursor.Cell;
                      end;
                  end;
      end;
  end;

  if gGameCursor.Mode <> cmNone then
  begin
    // Use the default cursor while placing roads, don't become stuck on c_Info or others
    if not fViewport.Scrolling then
      gRes.Cursors.Cursor := kmc_Default;
    Exit;
  end;

  Obj := gMySpectator.HitTestCursor;

  if fJoiningGroups and (gMySpectator.Selected is TKMUnitGroup) then
  begin
    Group := TKMUnitGroup(gMySpectator.Selected);
    if (Obj <> nil)
    and (Obj is TKMUnitWarrior)
    and (TKMUnitWarrior(Obj).Owner = gMySpectator.HandIndex)
    and not Group.HasMember(TKMUnitWarrior(Obj))
    and (UnitGroups[TKMUnitWarrior(Obj).UnitType] = Group.GroupType) then
      gRes.Cursors.Cursor := kmc_JoinYes
    else
      gRes.Cursors.Cursor := kmc_JoinNo;
    Exit;
  end;

  if not gMySpectator.Hand.InCinematic then
    // Only own units can be selected
    if ((Obj is TKMUnit) and ((TKMUnit(Obj).Owner = gMySpectator.HandIndex) or (fUIMode in [umReplay, umSpectate])))
    or ((Obj is TKMHouse) and ((TKMHouse(Obj).Owner = gMySpectator.HandIndex) or (fUIMode in [umReplay, umSpectate]))) then
    begin
      gRes.Cursors.Cursor := kmc_Info;
      Exit;
    end;

  if (gMySpectator.Selected is TKMUnitGroup)
  and (fUIMode in [umSP, umMP]) and not HasLostMPGame
  and not gMySpectator.Hand.InCinematic
  and (gMySpectator.FogOfWar.CheckTileRevelation(gGameCursor.Cell.X, gGameCursor.Cell.Y) > 0) then
  begin
    if ((Obj is TKMUnit) and (gMySpectator.Hand.Alliances[TKMUnit(Obj).Owner] = at_Enemy))
    or ((Obj is TKMHouse) and (gMySpectator.Hand.Alliances[TKMHouse(Obj).Owner] = at_Enemy)) then
      gRes.Cursors.Cursor := kmc_Attack
    else
      if not fViewport.Scrolling then
        gRes.Cursors.Cursor := kmc_Default;
    Exit;
  end;

  if not fViewport.Scrolling then
    gRes.Cursors.Cursor := kmc_Default;
end;


procedure TKMGamePlayInterface.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  P: TKMPoint;
  Obj: TObject;
  H: TKMHouse;
  Group, Group2: TKMUnitGroup;
  OldSelected: TObject;
  OldSelectedUnit: TKMUnitWarrior;
begin
  if fDragScrolling then
  begin
    if Button = mbMiddle then
    begin
      fDragScrolling := False;
      gRes.Cursors.Cursor := kmc_Default; // Reset cursor
      fMain.ApplyCursorRestriction;
    end;
    Exit;
  end;

  if fPlacingBeacon and (Button = mbRight) then
  begin
    Beacon_Cancel;
    if fMyControls.CtrlOver = nil then Exit; // Don't move troops too
  end;

  if (fMyControls.CtrlOver <> nil)
  and (fMyControls.CtrlOver <> Image_DirectionCursor)
  and not SelectingTroopDirection then
  begin
    fMyControls.MouseUp(X,Y,Shift,Button);
    Exit;
  end;

  if gGame.IsPaused and (fUIMode in [umSP, umMP]) then Exit;

  P := gGameCursor.Cell; // It's used in many places here

  case Button of
    mbLeft:
      begin
        // Process groups joining
        if fJoiningGroups and (gMySpectator.Selected is TKMUnitGroup) then
        begin
          Group := TKMUnitGroup(gMySpectator.Selected);
          Obj := gMySpectator.HitTestCursor;

          if (Obj <> nil)
          and (Obj is TKMUnitWarrior)
          and (TKMUnitWarrior(Obj).Owner = gMySpectator.HandIndex)
          and not Group.HasMember(TKMUnitWarrior(Obj))
          and (UnitGroups[TKMUnitWarrior(Obj).UnitType] = Group.GroupType) then
          begin
            Group2 := gMySpectator.Hand.UnitGroups.GetGroupByMember(TKMUnitWarrior(Obj));
            // Warrior might not have a group yet if he's still walking out of the barracks
            if Group2 <> nil then
            begin
              gSoundPlayer.PlayWarrior(Group.UnitType, sp_Join); // In SP joining is instant, Group does not exist after that
              gGame.GameInputProcess.CmdArmy(gic_ArmyLink, Group, Group2);
              Army_HideJoinMenu(nil);
            end;
          end;
          Exit;
        end;

        if fPlacingBeacon then
        begin
          Beacon_Place(gGameCursor.Float);
          Exit;
        end;

        // Only allow placing of roads etc. with the left mouse button
        if gMySpectator.FogOfWar.CheckTileRevelation(P.X, P.Y) = 0 then
        begin
          if gGameCursor.Mode in [cmErase, cmRoad, cmField, cmWine, cmHouses] then
            // Can't place noise when clicking on unexplored areas
            gSoundPlayer.Play(sfx_CantPlace, P, False, 4);
        end
        else
          case gGameCursor.Mode of
            cmNone:
              begin
                // Remember previous selection to play sound if it changes
                OldSelected := gMySpectator.Selected;
                OldSelectedUnit := nil;

                if OldSelected is TKMUnitGroup then
                  OldSelectedUnit := TKMUnitGroup(gMySpectator.Selected).SelectedUnit;

                // Don't allow selecting during a cinematic
                if not gMySpectator.Hand.InCinematic then
                  gMySpectator.UpdateSelect;

                // In a replay we want in-game statistics (and other things) to be shown for the owner of the last select object
                if fUIMode in [umReplay, umSpectate] then
                begin
                  if gMySpectator.Selected is TKMHouse      then gMySpectator.HandIndex := TKMHouse    (gMySpectator.Selected).Owner;
                  if gMySpectator.Selected is TKMUnit       then gMySpectator.HandIndex := TKMUnit     (gMySpectator.Selected).Owner;
                  if gMySpectator.Selected is TKMUnitGroup  then gMySpectator.HandIndex := TKMUnitGroup(gMySpectator.Selected).Owner;
                  Dropbox_ReplayFOW.SelectByTag(gMySpectator.HandIndex);
                  if Checkbox_ReplayFOW.Checked then
                    gMySpectator.FOWIndex := gMySpectator.HandIndex
                  else
                    gMySpectator.FOWIndex := -1;
                  fMinimap.Update(False); // Force update right now so FOW doesn't appear to lag
                end;

                if (gMySpectator.Selected is TKMHouse) then
                begin
                  HidePages;
                  SwitchPage(nil); // Hide main back button if we were in e.g. stats
                  fGuiGameHouse.Show(TKMHouse(gMySpectator.Selected), False);
                end;

                if (gMySpectator.Selected is TKMUnit) then
                begin
                  ShowUnitInfo(TKMUnit(gMySpectator.Selected));
                  if (fUIMode in [umSP, umMP]) and not HasLostMPGame
                  and (OldSelected <> gMySpectator.Selected) then
                    gSoundPlayer.PlayCitizen(TKMUnit(gMySpectator.Selected).UnitType, sp_Select);
                end;

                if (gMySpectator.Selected is TKMUnitGroup) then
                begin
                  Group := TKMUnitGroup(gMySpectator.Selected);
                  ShowGroupInfo(Group);
                  if (fUIMode in [umSP, umMP]) and not HasLostMPGame
                  and ((OldSelected <> Group) or (OldSelectedUnit <> Group.SelectedUnit)) then
                    gSoundPlayer.PlayWarrior(Group.SelectedUnit.UnitType, sp_Select);
                end;
              end;

            cmRoad:  gGameCursor.Tag1 := Ord(cfmNone);
            cmField: gGameCursor.Tag1 := Ord(cfmNone);
            cmWine:  gGameCursor.Tag1 := Ord(cfmNone);

            cmHouses:
              if gMySpectator.Hand.CanAddHousePlan(P, THouseType(gGameCursor.Tag1)) then
              begin
                gGame.GameInputProcess.CmdBuild(gic_BuildHousePlan, P, THouseType(gGameCursor.Tag1));
                // If shift pressed do not reset cursor (keep selected building)
                if not (ssShift in Shift) then
                  fGuiGameBuild.Show;
              end
              else
                gSoundPlayer.Play(sfx_CantPlace, P, False, 4);
            cmErase:
              if KMSamePoint(LastDragPoint, KMPoint(0,0)) then
              begin
                H := gMySpectator.Hand.HousesHitTest(P.X, P.Y);
                // Ask wherever player wants to destroy own house (don't ask about houses that are not started, they are removed below)
                if H <> nil then
                begin
                  gMySpectator.Selected := H; // Select the house irregardless of unit below/above
                  HidePages;
                  SwitchPage(nil); // Hide main back button if we were in e.g. stats
                  fGuiGameHouse.Show(H, True);
                  gSoundPlayer.Play(sfx_Click);
                end
                else
                begin
                  // Now remove houses that are not started
                  if gMySpectator.Hand.BuildList.HousePlanList.HasPlan(P) then
                    gGame.GameInputProcess.CmdBuild(gic_BuildRemoveHousePlan, P)
                  else
                    if gMySpectator.Hand.BuildList.FieldworksList.HasFakeField(P) <> ft_None then
                      gGame.GameInputProcess.CmdBuild(gic_BuildRemoveFieldPlan, P) // Remove plans
                    else
                      gSoundPlayer.Play(sfx_CantPlace, P, False, 4); // Otherwise there is nothing to erase
                end;
              end;
          end
      end;
    mbRight:
      begin
        // Cancel build
        if fGuiGameBuild.Visible then
          SwitchPage(Button_Back);

        // Cancel join
        if fJoiningGroups then
        begin
          Army_HideJoinMenu(nil);
          Exit; // Don't order troops too
        end;

        if ((gMySpectator.Selected is TKMHouseBarracks) or (gMySpectator.Selected is TKMHouseWoodcutters)) and not fPlacingBeacon
        and (fUIMode in [umSP, umMP]) and not HasLostMPGame then
        begin
          if gTerrain.Route_CanBeMade(KMPointBelow(TKMHouse(gMySpectator.Selected).GetEntrance), P, tpWalk, 0) then
          begin
            if gMySpectator.Selected is TKMHouseBarracks then
              gGame.GameInputProcess.CmdHouse(gic_HouseBarracksRally, TKMHouse(gMySpectator.Selected), P)
            else
              if gMySpectator.Selected is TKMHouseWoodcutters then
                gGame.GameInputProcess.CmdHouse(gic_HouseWoodcuttersCutting, TKMHouse(gMySpectator.Selected), P);
          end
          else
            gSoundPlayer.Play(sfx_CantPlace, P, False, 4);
          Exit;
        end;

        // Process warrior commands
        if (fUIMode in [umSP, umMP])
        and not HasLostMPGame
        and not fJoiningGroups
        and not fPlacingBeacon
        and (gMySpectator.Selected is TKMUnitGroup) then
        begin
          Group := TKMUnitGroup(gMySpectator.Selected);

          // Attack or Walk
          if Group.CanTakeOrders and (Group.Owner = gMySpectator.HandIndex) then
          begin
            // Try to Attack unit
            Obj := gMySpectator.HitTestCursor;
            if (Obj is TKMUnit) and (gMySpectator.Hand.Alliances[TKMUnit(Obj).Owner] = at_Enemy) then
            begin
              gGame.GameInputProcess.CmdArmy(gic_ArmyAttackUnit, Group, TKMUnit(Obj));
              gSoundPlayer.PlayWarrior(Group.UnitType, sp_Attack);
            end
            else
            // If there's no unit - try to Attack house
            if (Obj is TKMHouse) and (gMySpectator.Hand.Alliances[TKMHouse(Obj).Owner] = at_Enemy) then
            begin
              gGame.GameInputProcess.CmdArmy(gic_ArmyAttackHouse, Group, TKMHouse(Obj));
              gSoundPlayer.PlayWarrior(Group.UnitType, sp_Attack);
            end
            else
            // Ensure down click was successful (could have been over a mountain, then dragged to a walkable location)
            if SelectingTroopDirection and Group.CanWalkTo(P, 0) then
            begin
              gGame.GameInputProcess.CmdArmy(gic_ArmyWalk, Group, P, SelectedDirection);
              gSoundPlayer.PlayWarrior(Group.UnitType, sp_Move);
            end;
          end;
        end;
        // Not selecting direction now (must do it at the end because SelectingTroopDirection is used for Walk above)
        ReleaseDirectionSelector;
      end;
  end;

  LastDragPoint := KMPoint(0,0);
end;


procedure TKMGamePlayInterface.Save(SaveStream: TKMemoryStream);
begin
  fViewport.Save(SaveStream);

  fGuiGameHouse.Save(SaveStream);
  SaveStream.WriteW(fLastSaveName);
  SaveStream.Write(fSelection, SizeOf(fSelection));
  fMessageStack.Save(SaveStream);
  // Everything else (e.g. ShownUnit or AskDemolish) can't be seen in Save_menu anyways
end;


// Save just the minimap for preview (near the start of the file)
procedure TKMGamePlayInterface.SaveMinimap(SaveStream: TKMemoryStream);
begin
  fMinimap.SaveToStream(SaveStream);
end;


procedure TKMGamePlayInterface.Load(LoadStream: TKMemoryStream);
begin
  fViewport.Load(LoadStream);

  fGuiGameHouse.Load(LoadStream);
  LoadStream.ReadW(fLastSaveName);
  LoadStream.Read(fSelection, SizeOf(fSelection));
  fMessageStack.Load(LoadStream);

  // Everything else (e.g. ShownUnit or AskDemolish) can't be seen in Save_menu anyways
  Message_UpdateStack;
  gLog.AddTime('Interface loaded');
end;


// Load the minimap (saved near start of the file)
procedure TKMGamePlayInterface.LoadMinimap(LoadStream: TKMemoryStream);
begin
  fMinimap.LoadFromStream(LoadStream);
end;


procedure TKMGamePlayInterface.SyncUI(aMoveViewport: Boolean = True);
begin
  inherited;

  fMinimap.Alerts := fAlerts;

  MinimapView.SetMinimap(fMinimap);
  MinimapView.SetViewport(fViewport);

  SetMenuState(gGame.MissionMode = mm_Tactic);
end;


{ Should update any items changed by game (resource counts, hp, etc..) }
{ If it ever gets a bottleneck then some static Controls may be excluded from update }
procedure TKMGamePlayInterface.UpdateState(aTickCount: Cardinal);
var
  I: Integer;
  Rect: TKMRect;
begin
  // Update minimap every 1000ms
  if aTickCount mod 10 = 0 then
    fMinimap.Update(False);

  fAlerts.UpdateState(aTickCount);

  // Update unit/house information
  if gMySpectator.Selected is TKMUnitGroup then
    ShowGroupInfo(TKMUnitGroup(gMySpectator.Selected))
  else
  if gMySpectator.Selected is TKMUnit then
    ShowUnitInfo(TKMUnit(gMySpectator.Selected), fAskDismiss)
  else
  begin
    fJoiningGroups := False;
    if gMySpectator.Selected is TKMHouse then
    begin
      HidePages;
      SwitchPage(nil); // Hide main back button if we were in e.g. stats
      fGuiGameHouse.Show(TKMHouse(gMySpectator.Selected));
    end
    else
      if fGuiGameHouse.Visible then
        fGuiGameHouse.Hide;
      if Panel_Unit.Visible then
        SwitchPage(nil);
  end;

  // Update peacetime counter
  if gGame.GameOptions.Peacetime <> 0 then
    Label_PeacetimeRemaining.Caption := Format(gResTexts[TX_MP_PEACETIME_REMAINING],
                                               [TimeToString(gGame.GetPeacetimeRemaining)])
  else
    Label_PeacetimeRemaining.Caption := '';

  // Update replay counters
  if fUIMode = umReplay then
  begin
    // Replays can continue after end, keep the bar in 0..1 range
    PercentBar_Replay.Seam := Min(gGame.GameOptions.Peacetime * 600 / Max(gGame.GameInputProcess.GetLastTick,1), 1);
    PercentBar_Replay.Position := Min(gGame.GameTickCount / Max(gGame.GameInputProcess.GetLastTick,1), 1);
    Label_Replay.Caption := TimeToString(gGame.MissionTime) + ' / ' +
                            TimeToString(gGame.GameInputProcess.GetLastTick/24/60/60/10);
  end;

  // Update speedup clocks
  if Image_Clock.Visible then
  begin
    Image_Clock.TexID := ((Image_Clock.TexID - 556) + 1) mod 16 + 556;
    Label_Clock.Caption := TimeToString(gGame.MissionTime);
  end;

  // Keep on updating these menu pages as game data keeps on changing
  if fGuiGameBuild.Visible then fGuiGameBuild.UpdateState;
  if fGuiGameRatios.Visible and (fUIMode in [umReplay, umSpectate]) then fGuiGameRatios.UpdateState;
  if fGuiGameStats.Visible then fGuiGameStats.UpdateState;
  if Panel_Menu.Visible then Menu_Update;

  // Update message stack
  // Flash unread message display
  Label_MPChatUnread.Visible := (fUIMode in [umMP, umSpectate]) and (Label_MPChatUnread.Caption <> '') and not (aTickCount mod 10 < 5);
  Image_MPChat.Highlight := fGuiGameChat.Visible or (Label_MPChatUnread.Visible and (Label_MPChatUnread.Caption <> ''));
  Image_MPAllies.Highlight := Panel_Allies.Visible;
  if (fUIMode in [umSP, umMP]) and not Image_MessageLog.Visible and (gMySpectator.Hand.MessageLog.CountLog > 0) then
  begin
    Image_MessageLog.Show;
    MessageStack_UpdatePositions;
  end;
  Image_MessageLog.Highlight := not Panel_MessageLog.Visible and not (aTickCount mod 10 < 5)
                                and (fLastSyncedMessage <> gMySpectator.Hand.MessageLog.CountLog);

  if Panel_MessageLog.Visible then
    MessageLog_Update(False);

  // Update info on awaited players
  if Panel_NetWait.Visible then
  begin
    if gGame.Networking.IsReconnecting then
      Label_NetDropPlayersDelay.Caption := ''
    else
    begin
      i := NET_DROP_PLAYER_MIN_WAIT - EnsureRange(GetTimeSince(fNetWaitDropPlayersDelayStarted) div 1000, 0, NET_DROP_PLAYER_MIN_WAIT);
      if i > 0 then
        Label_NetDropPlayersDelay.Caption := Format(gResTexts[TX_GAMEPLAY_DROP_PLAYERS_DELAY], [i])
      else
        Label_NetDropPlayersDelay.Caption := gResTexts[TX_GAMEPLAY_DROP_PLAYERS_ALLOWED];
      Button_NetDropPlayers.Enabled := i = 0;
    end;
  end;

  // Display team names
  if aTickCount mod 3 = 0 then // Update once every 300ms, player won't notice
  begin
    fTeamNames.Clear;
    if fShowTeamNames then
    begin
      Rect := fViewport.GetMinimapClip;
      gHands.GetUnitsInRect(Rect, fTeamNames);
    end;
  end;

  UpdateDebugInfo;
  if fSaves <> nil then fSaves.UpdateState;
end;


procedure TKMGamePlayInterface.UpdateStateIdle(aFrameTime: Cardinal);
begin
  // Check to see if we need to scroll
  fViewport.UpdateStateIdle(aFrameTime, gMySpectator.Hand.InCinematic);
end;


procedure TKMGamePlayInterface.UpdateDebugInfo;
var
  I: Integer;
  S: string;
begin
  S := '';

  // Debug info
  if SHOW_SPRITE_COUNT then
    S := IntToStr(gHands.UnitCount) + ' units on map|' +
         IntToStr(fRenderPool.RenderList.Stat_Sprites) + '/' +
         IntToStr(fRenderPool.RenderList.Stat_Sprites2) + ' sprites/rendered|' +
         IntToStr(CtrlPaintCount) + ' controls rendered|';

  if SHOW_POINTER_COUNT then
    S := S + Format('Pointers: %d units, %d houses|', [gMySpectator.Hand.Units.GetTotalPointers, gMySpectator.Hand.Houses.GetTotalPointers]);

  if SHOW_CMDQUEUE_COUNT then
    S := S + IntToStr(gGame.GameInputProcess.Count) + ' commands stored|';

  if SHOW_NETWORK_DELAY and (fUIMode in [umMP, umSpectate]) then
    S := S + 'Network delay: ' + IntToStr(TGameInputProcess_Multi(gGame.GameInputProcess).GetNetworkDelay) + '|';

  if DISPLAY_SOUNDS then
    S := S + IntToStr(gSoundPlayer.ActiveCount) + ' sounds playing|';

  // Temporary inteface (by @Crow)
  if SHOW_ARMYEVALS then
    for I := 0 to gHands.Count - 1 do
    if I <> gMySpectator.HandIndex then
      S := S + Format('Enemy %d: %f|', [I, RoundTo(gMySpectator.Hand.ArmyEval.Evaluations[I].Power, -3)]);

  if SHOW_AI_WARE_BALANCE then
    S := S + gMySpectator.Hand.AI.Mayor.BalanceText + '|';

  Label_DebugInfo.Caption := S;
end;


procedure TKMGamePlayInterface.Paint;
var
  I: Integer;
  U: TKMUnit;
  UnitLoc: TKMPointF;
  MapLoc: TKMPointF;
  ScreenLoc: TKMPoint;
begin
  if fShowTeamNames then
  begin
    Label_TeamName.Visible := True; // Only visible while we're using it, otherwise it shows up in other places
    for I := 0 to fTeamNames.Count - 1 do
    begin
      U := TKMUnit(fTeamNames[I]);
      if U.Visible and (gMySpectator.FogOfWar.CheckRevelation(U.PositionF) > FOG_OF_WAR_MIN) then
      begin
        Label_TeamName.Caption := gHands[U.Owner].OwnerName;
        Label_TeamName.FontColor := FlagColorToTextColor(gHands[U.Owner].FlagColor);

        UnitLoc := U.PositionF;
        UnitLoc.X := UnitLoc.X - 0.5;
        UnitLoc.Y := UnitLoc.Y - 1;
        MapLoc := gTerrain.FlatToHeight(UnitLoc);
        ScreenLoc := fViewport.MapToScreen(MapLoc);

        if KMInRect(ScreenLoc, KMRect(0, 0, Panel_Main.Width, Panel_Main.Height)) then
        begin
          Label_TeamName.Left := ScreenLoc.X;
          Label_TeamName.Top := ScreenLoc.Y;
          Label_TeamName.Paint;
        end;
      end;
    end;
  end;
  Label_TeamName.Visible := False; // Only visible while we're using it, otherwise it shows up in other places

  inherited;
end;


end.
