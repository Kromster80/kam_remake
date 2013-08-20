unit KM_InterfaceGamePlay;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  StrUtils, SysUtils, KromUtils, Math, Classes, Controls,
  KM_Controls, KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Pics, KM_Points,
  KM_InterfaceDefaults, KM_Terrain, KM_Houses, KM_Units,
  KM_UnitGroups, KM_Units_Warrior, KM_Saves, KM_MessageStack, KM_ResHouses,
  KM_GUIGameHouse;


const
  MAX_VISIBLE_MSGS = 32;
  MAX_LOG_MSGS = 8;

type
  TKMTabButtons = (tbBuild, tbRatio, tbStats, tbMenu);

  TKMGamePlayInterface = class (TKMUserInterface)
  private
    fMultiplayer: Boolean; //Multiplayer UI has slightly different layout
    fReplay: Boolean; //Replay UI has slightly different layout
    fSave_Selected: Integer; //Save selected from list (needed because of scanning)

    fGuiGameHouse: TKMGUIGameHouse;

    //Not saved
    LastDragPoint: TKMPoint; //Last mouse point that we drag placed/removed a road/field
    PrevHint: TObject;
    ShownMessage: Integer;
    PlayMoreMsg: TGameResultMsg; //Remember which message we are showing
    fJoiningGroups, fPlacingBeacon: Boolean;
    fAskDismiss: Boolean;
    fDragScrolling: Boolean;
    fDragScrollingCursorPos: TPoint;
    fDragScrollingViewportPos: TKMPointF;
    fNetWaitDropPlayersDelayStarted: Cardinal;
    SelectedDirection: TKMDirection;
    SelectingTroopDirection: Boolean;
    SelectingDirPosition: TPoint;
    RatioTab: Byte; //Active resource distribution tab
    fSaves: TKMSavesCollection;
    fTeamNames: TList;
    Label_TeamName: TKMLabel;
    fLastSyncedMessage: Word; //Last message that we synced with MessageLog
    fChatMode: TChatMode;
    fChatWhisperRecipient: Integer; //Server index of the player who will receive the whisper

    //Saved (in singleplayer only)
    fLastSaveName: string; //The file name we last used to save this file (used as default in Save menu)
    fMessageList: TKMMessageList;
    fSelection: array [0..9] of Integer;

    procedure Create_Controls;
    procedure Create_Replay;
    procedure Create_Allies;
    procedure Create_Chat;
    procedure Create_ChatMenu(aParent: TKMPanel);
    procedure Create_Message;
    procedure Create_MessageLog;
    procedure Create_Pause;
    procedure Create_PlayMore;
    procedure Create_MPPlayMore;
    procedure Create_NetWait;
    procedure Create_MessageStack;
    procedure Create_Build;
    procedure Create_Ratios;
    procedure Create_Stats;
    procedure Create_Menu;
    procedure Create_Save;
    procedure Create_Load;
    procedure Create_Settings;
    procedure Create_Quit;
    procedure Create_Unit;

    procedure Beacon_Cancel;
    procedure Army_ActivateControls(aGroup: TKMUnitGroup);
    procedure Army_HideJoinMenu(Sender:TObject);
    procedure Army_Issue_Order(Sender:TObject);
    procedure House_Demolish;
    procedure Unit_Dismiss(Sender:TObject);
    procedure Menu_Settings_Fill;
    procedure Menu_Settings_Change(Sender:TObject);
    procedure Menu_QuitMission(Sender:TObject);
    procedure Menu_NextTrack(Sender:TObject);
    procedure Menu_PreviousTrack(Sender:TObject);
    procedure Chat_Click(Sender: TObject);
    procedure Chat_Show(Sender: TObject);
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
    procedure Selection_Assign(aKey: Word; aObject: TObject);
    procedure Selection_Select(aKey: Word);
    procedure Stats_Resize;
    procedure SwitchPage(Sender: TObject);
    procedure SwitchPage_Ratios(Sender: TObject);
    procedure RatiosChange(Sender: TObject);
    procedure DisplayHint(Sender: TObject);
    procedure PlayMoreClick(Sender:TObject);
    procedure MPPlayMoreClick(Sender:TObject);
    procedure NetWaitClick(Sender:TObject);
    procedure ReplayClick(Sender: TObject);
    procedure Build_ButtonClick(Sender: TObject);
    procedure Build_Update;
    procedure Chat_Close(Sender: TObject);
    procedure Chat_Post(Sender:TObject; Key:word);
    procedure Chat_Resize(Sender: TObject; X,Y: Integer);
    procedure Chat_MenuClick(Sender: TObject);
    procedure Chat_MenuSelect(aItem: Integer);
    procedure Chat_MenuShow(Sender: TObject);
    procedure Allies_Close(Sender: TObject);
    procedure Stats_Update;
    procedure Menu_Update;
    procedure SetPause(aValue:boolean);
    procedure DirectionCursorShow(X,Y: Integer; Dir:TKMDirection);
    procedure DirectionCursorHide;
    function HasLostMPGame:Boolean;
    procedure UpdateDebugInfo;
    procedure HidePages;
  protected
    Panel_Main: TKMPanel;
      Sidebar_Top: TKMImage;
      Sidebar_Middle: TKMImage;
      MinimapView: TKMMinimapView;
      Label_DebugInfo, Label_Hint: TKMLabel;
      Bevel_HintBG: TKMBevel;

      Image_MPChat, Image_MPAllies: TKMImage; //Multiplayer buttons
      Image_MessageLog: TKMImage;
      Label_MPChatUnread: TKMLabel;
      Image_Message: array[0..MAX_VISIBLE_MSGS]of TKMImage; //Queue of messages covers 32*48=1536px height
      Image_Clock:TKMImage; //Clock displayed when game speed is increased
      Label_Clock:TKMLabel;
      Label_ClockSpeedup:TKMLabel;
      Label_ScriptedOverlay:TKMLabel; //Label that can be set from script
      Label_MenuTitle: TKMLabel; //Displays the title of the current menu to the right of return
      Image_DirectionCursor:TKMImage;

    Panel_Controls: TKMPanel;
      Button_Main: array [TKMTabButtons] of TKMButton; //4 common buttons + Return
      Button_Back: TKMButton;

    Panel_ReplayCtrl: TKMPanel; //Smaller Panel to contain replay controls
      PercentBar_Replay: TKMPercentBar;
      Label_Replay: TKMLabel;
      Button_ReplayRestart: TKMButton;
      Button_ReplayPause: TKMButton;
      Button_ReplayStep: TKMButton;
      Button_ReplayResume: TKMButton;
      Button_ReplayExit: TKMButton;
      Dropbox_ReplayFOW: TKMDropList;
    Panel_Allies:TKMPanel;
      Label_PeacetimeRemaining: TKMLabel;
      Image_AlliesFlag:array [0..MAX_PLAYERS-1] of TKMImage;
      Label_AlliesPlayer:array [0..MAX_PLAYERS-1] of TKMLabel;
      DropBox_AlliesTeam:array [0..MAX_PLAYERS-1] of TKMDropList;
      Label_AlliesTeam:array [0..MAX_PLAYERS-1] of TKMLabel;
      Label_AlliesPing:array [0..MAX_PLAYERS-1] of TKMLabel;
      Image_AlliesClose:TKMImage;
    Panel_Chat: TKMPanel; //For multiplayer: Send, reply, text area for typing, etc.
      Dragger_Chat: TKMDragger;
      Image_Chat: TKMImage;
      Memo_ChatText: TKMMemo;
      Edit_ChatMsg: TKMEdit;
      Button_ChatRecipient: TKMButtonFlat;
      Image_ChatClose: TKMImage;
      Menu_Chat: TKMPopUpMenu;
    Panel_Message: TKMPanel;
      Label_MessageText: TKMLabel;
      Button_MessageGoTo: TKMButton;
      Button_MessageDelete: TKMButton;
      Image_MessageClose: TKMImage;
    Panel_MessageLog: TKMPanel;
      ColumnBox_MessageLog: TKMColumnBox;
      Image_MessageLogClose: TKMImage;
    Panel_Pause:TKMPanel;
      Bevel_Pause:TKMBevel;
      Image_Pause:TKMImage;
      Label_Pause1:TKMLabel;
      Label_Pause2:TKMLabel;
    Panel_PlayMore:TKMPanel;
      Bevel_PlayMore:TKMBevel;
      Panel_PlayMoreMsg:TKMPanel;
        Image_PlayMore:TKMImage;
        Label_PlayMore:TKMLabel;
        Button_PlayMore,Button_PlayQuit:TKMButton;
    Panel_MPPlayMore:TKMPanel;
      Bevel_MPPlayMore:TKMBevel;
      Image_MPPlayMore:TKMImage;
      Label_MPPlayMore:TKMLabel;
      Button_MPPlayMore,Button_MPPlayQuit:TKMButton;
    Panel_NetWait:TKMPanel;
      Bevel_NetWait:TKMBevel;
      Panel_NetWaitMsg:TKMPanel;
        Image_NetWait:TKMImage;
        Label_NetWait,Label_NetDropPlayersDelay:TKMLabel;
        Panel_NetWaitButtons:TKMPanel;
          Button_NetQuit,Button_NetDropPlayers:TKMButton;
        Panel_NetWaitConfirm:TKMPanel;
          Label_NetWaitConfirm:TKMLabel;
          Button_NetConfirmYes,Button_NetConfirmNo:TKMButton;
    Panel_Ratios:TKMPanel;
      Button_Ratios:array[1..4]of TKMButton;
      Image_RatioPic0:TKMImage;
      Label_RatioLab0:TKMLabel;
      Image_RatioPic:array[1..4]of TKMImage;
      TrackBar_RatioRat:array[1..4]of TKMTrackBar;
    Panel_Stats:TKMPanel;
      Panel_StatBlock: array [0..12] of TKMPanel;
      Stat_HousePic: array [HOUSE_MIN..HOUSE_MAX] of TKMImage;
      Stat_UnitPic: array [CITIZEN_MIN..CITIZEN_MAX] of TKMImage;
      Stat_HouseQty, Stat_HouseWip: array [HOUSE_MIN..HOUSE_MAX] of TKMLabel;
      Stat_UnitQty, Stat_UnitWip: array [CITIZEN_MIN..CITIZEN_MAX] of TKMLabel;

    Panel_Build:TKMPanel;
      Label_Build:TKMLabel;
      Image_Build_Selected:TKMImage;
      Image_BuildCost_WoodPic:TKMImage;
      Image_BuildCost_StonePic:TKMImage;
      Label_BuildCost_Wood:TKMLabel;
      Label_BuildCost_Stone:TKMLabel;
      Button_BuildRoad,Button_BuildField,Button_BuildWine,Button_BuildCancel:TKMButtonFlat;
      Button_Build:array[1..GUI_HOUSE_COUNT]of TKMButtonFlat;

    Panel_Menu:TKMPanel;
      Button_Menu_Save,Button_Menu_Load,Button_Menu_Settings,Button_Menu_Quit,Button_Menu_TrackUp,Button_Menu_TrackDown:TKMButton;
      Label_Menu_Track, Label_GameTime: TKMLabel;

      Panel_Save:TKMPanel;
        ListBox_Save: TKMListBox;
        Edit_Save: TKMEdit;
        Label_SaveExists: TKMLabel;
        CheckBox_SaveExists: TKMCheckBox;
        Button_Save: TKMButton;

      Panel_Load: TKMPanel;
        ListBox_Load: TKMListBox;
        Label_LoadDescription: TKMLabel;
        Button_Load: TKMButton;

      Panel_Settings: TKMPanel;
        CheckBox_Settings_Autosave: TKMCheckBox;
        TrackBar_Settings_Brightness: TKMTrackBar;
        TrackBar_Settings_SFX: TKMTrackBar;
        TrackBar_Settings_Music: TKMTrackBar;
        TrackBar_Settings_ScrollSpeed: TKMTrackBar;
        CheckBox_Settings_MusicOff: TKMCheckBox;
        CheckBox_Settings_ShuffleOn: TKMCheckBox;

      Panel_Quit: TKMPanel;
        Button_Quit_Yes, Button_Quit_No: TKMButton;

    Panel_Unit:TKMPanel;
      Label_UnitName:TKMLabel;
      Label_UnitCondition:TKMLabel;
      Label_UnitTask:TKMLabel;
      Label_UnitDescription:TKMLabel;
      ConditionBar_Unit:TKMPercentBar;
      Image_UnitPic:TKMImage;
      Button_Unit_Dismiss:TKMButton;

      Panel_Unit_Dismiss:TKMPanel;
         Label_Unit_Dismiss:TKMLabel;
         Button_Unit_DismissYes,Button_Unit_DismissNo:TKMButton;

      Panel_Army:TKMPanel;
        Button_Army_GoTo,Button_Army_Stop,Button_Army_Attack:TKMButton;
        Button_Army_RotCW,Button_Army_Storm,Button_Army_RotCCW:TKMButton;
        Button_Army_ForUp,Button_Army_ForDown:TKMButton;
        ImageStack_Army:TKMImageStack;
        Button_Army_Split,Button_Army_Join,Button_Army_Feed:TKMButton;

      Panel_Army_JoinGroups:TKMPanel;
        Button_Army_Join_Cancel:TKMButton;
        Label_Army_Join_Message:TKMLabel;
  public
    constructor Create(aScreenX, aScreenY: Word; aMultiplayer, aReplay: Boolean); reintroduce;
    destructor Destroy; override;
    procedure ShowUnitInfo(Sender: TKMUnit; aAskDismiss:boolean=false);
    procedure ShowGroupInfo(Sender: TKMUnitGroup);
    procedure MessageIssue(aKind: TKMMessageKind; aText: string); overload;
    procedure MessageIssue(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint); overload;
    procedure SetMenuState(aTactic: Boolean);
    procedure SetMinimap;
    procedure ShowClock(aSpeed: Single);
    procedure ShowPlayMore(DoShow:boolean; Msg:TGameResultMsg);
    procedure ShowMPPlayMore(Msg:TGameResultMsg);
    procedure ShowNetworkLag(DoShow:boolean; aPlayers:TStringList; IsHost:boolean);
    procedure SetScriptedOverlay(aText: string);
    procedure AppendScriptedOverlay(aText: string);
    procedure ReleaseDirectionSelector;
    procedure SetChatText(const aString: string);
    procedure SetChatMessages(const aString: string);
    procedure ChatMessage(const aData: UnicodeString);
    procedure WarriorCommanderDied(DeadID, NewID: Cardinal);
    procedure AlliesOnPlayerSetup(Sender: TObject);
    procedure AlliesOnPingInfo(Sender: TObject);
    procedure AlliesTeamChange(Sender: TObject);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

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
uses KM_Main, KM_GameInputProcess, KM_GameInputProcess_Multi, KM_AI, KM_RenderUI, KM_GameCursor,
  KM_PlayersCollection, KM_Player, KM_RenderPool, KM_ResTexts, KM_Game, KM_GameApp,
  KM_Utils, KM_ResLocales, KM_ResSound, KM_Resource, KM_Log, KM_ResCursors, KM_ResFonts,
  KM_ResSprites, KM_ResUnits, KM_ResWares, KM_FogOfWar, KM_HouseBarracks, KM_Sound;


const
  MESSAGE_AREA_HEIGHT = 173+17; //Image_ChatHead + Image_ChatBody
  MESSAGE_AREA_RESIZE_Y = 200; //How much can we resize it

  ResRatioCount = 4;
  ResRatioType: array[1..ResRatioCount] of TWareType = (wt_Steel, wt_Coal, wt_Wood, wt_Corn);
  ResRatioHint: array[1..ResRatioCount] of Word = (298, 300, 302, 304); //Distribution of rt_***
  ResRatioHouseCount: array[1..ResRatioCount] of Byte = (2, 4, 2, 3);
  ResRatioHouse: array[1..ResRatioCount, 1..4] of THouseType = (
      (ht_WeaponSmithy,   ht_ArmorSmithy,     ht_None,          ht_None),
      (ht_IronSmithy,     ht_Metallurgists,   ht_WeaponSmithy,  ht_ArmorSmithy),
      (ht_ArmorWorkshop,  ht_WeaponWorkshop,  ht_None,          ht_None),
      (ht_Mill,           ht_Swine,           ht_Stables,       ht_None));


{Switch between pages}
procedure TKMGamePlayInterface.SwitchPage_Ratios(Sender: TObject);
var I: Integer; HT:THouseType;
begin
  //Hide everything but the tab buttons
  for I := 0 to Panel_Ratios.ChildCount - 1 do
    if not (Panel_Ratios.Childs[I] is TKMButton) then
      Panel_Ratios.Childs[I].Hide;

  RatioTab := TKMButton(Sender).Tag;

  Image_RatioPic0.TexID := fResource.Wares[ResRatioType[RatioTab]].GUIIcon;//Show resource icon
  Label_RatioLab0.Caption := fResource.Wares[ResRatioType[RatioTab]].Title;
  Image_RatioPic0.Show;
  Label_RatioLab0.Show;

  for i:=1 to ResRatioHouseCount[RatioTab] do
  begin
    HT := ResRatioHouse[RatioTab, i];
    //Do not allow player to see blocked house (never able to build). Though house may be prebuilt and blocked
    if (not gPlayers[MySpectator.PlayerIndex].Stats.HouseBlocked[HT])
    or (gPlayers[MySpectator.PlayerIndex].Stats.GetHouseQty(HT) > 0) then
    begin
      Image_RatioPic[i].TexID := fResource.HouseDat[HT].GUIIcon;
      TrackBar_RatioRat[i].Caption := fResource.HouseDat[HT].HouseName;
      TrackBar_RatioRat[i].Position := gPlayers[MySpectator.PlayerIndex].Stats.Ratio[ResRatioType[RatioTab], HT];
      TrackBar_RatioRat[i].Enable;
    end else begin
      Image_RatioPic[i].TexID := 41; //Question mark
      TrackBar_RatioRat[i].Caption := gResTexts[TX_GAMEPLAY_NOT_AVAILABLE];
      TrackBar_RatioRat[i].Position := 0;
      TrackBar_RatioRat[i].Disable;
    end;

    Image_RatioPic[i].Show;
    TrackBar_RatioRat[i].Show;
  end;
end;


procedure TKMGamePlayInterface.RatiosChange(Sender: TObject);
var RT:TWareType; HT:THouseType;
begin
  RT := ResRatioType[RatioTab];
  HT := ResRatioHouse[RatioTab, TKMTrackBar(Sender).Tag];

  fGame.GameInputProcess.CmdRatio(gic_RatioChange, RT, HT, TKMTrackBar(Sender).Position);
end;


procedure TKMGamePlayInterface.Menu_Save_ListChange(Sender: TObject);
begin
  fSaves.Lock;
    if InRange(TKMListBox(Sender).ItemIndex, 0, fSaves.Count-1) then
    begin
      fSave_Selected := TKMListBox(Sender).ItemIndex;
      Edit_Save.Text := fSaves[ListBox_Save.ItemIndex].FileName;
      //We just selected something from the list so it exists
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
    CheckBox_SaveExists.Enabled := FileExists(fGame.SaveName(Edit_Save.Text, 'sav', fMultiplayer));
    Label_SaveExists.Visible := CheckBox_SaveExists.Enabled;
    CheckBox_SaveExists.Checked := False;
    //we should protect ourselves from empty names and whitespaces at beggining and at end of name
    Button_Save.Enabled := (not CheckBox_SaveExists.Enabled) and (Edit_Save.Text <> '') and
                           not (Edit_Save.Text[1] = ' ') and not (Edit_Save.Text[Length(Edit_Save.Text)] = ' ');
  end;
end;


procedure TKMGamePlayInterface.Menu_Save_CheckboxChange(Sender: TObject);
begin
  //we should protect ourselves from empty names and whitespaces at beggining and at end of name
  Button_Save.Enabled := CheckBox_SaveExists.Checked and (Edit_Save.Text <> '') and
                         not (Edit_Save.Text[1] = ' ') and not (Edit_Save.Text[Length(Edit_Save.Text)] = ' ');
end;


procedure TKMGamePlayInterface.Menu_Save_RefreshList(Sender: TObject);
var I, PrevTop: Integer;
begin
  PrevTop := ListBox_Save.TopIndex;
  ListBox_Save.Clear;

  if fSaves.ScanFinished then
  begin
    if fLastSaveName = '' then
      Edit_Save.Text := fGame.GameName
    else
      Edit_Save.Text := fLastSaveName;
  end;

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
  //Edit.OnChange event happens on key up, so it's still possible for the user to click save button
  //with an invalid file name entered, if the click while still holding down a key.
  //In general it's bad to rely on events like that to ensure validity, doing check here is a good idea
  if SaveName = '' then Exit;

  fLastSaveName := SaveName; //Do this before saving so it is included in the save
  if fMultiplayer then
    //Don't tell everyone in the game that we are saving yet, as the command hasn't been processed
    fGame.GameInputProcess.CmdGame(gic_GameSave, SaveName)
  else
    fGame.Save(SaveName);

  fSaves.TerminateScan; //stop scan as it is no longer needed
  SwitchPage(nil); //Close save menu after saving
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
  fSaves.TerminateScan; //Stop scan as it is no longer needed
  fGameApp.NewSingleSave(fSaves[ListBox_Load.ItemIndex].FileName);
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
  //Hide all existing pages
  for I := 0 to Panel_Controls.ChildCount - 1 do
    if (Panel_Controls.Childs[I] is TKMPanel) then
      Panel_Controls.Childs[I].Hide;

  fGuiGameHouse.Hide;
end;


{Switch between pages}
procedure TKMGamePlayInterface.SwitchPage(Sender: TObject);
var
  LastVisiblePage: TKMPanel;

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
    MySpectator.Selected := nil;

  //Reset the CursorMode, to cm_None
  Build_ButtonClick(nil);

  //Set LastVisiblePage to which ever page was last visible, out of the ones needed
  if Panel_Settings.Visible then LastVisiblePage := Panel_Settings else
  if Panel_Save.Visible     then LastVisiblePage := Panel_Save     else
  if Panel_Load.Visible     then LastVisiblePage := Panel_Load     else
    LastVisiblePage := nil;

  //If they just closed settings then we should save them (if something has changed)
  if LastVisiblePage = Panel_Settings then
    fGameApp.GameSettings.SaveSettings;

  //Ensure, that saves scanning will be stopped when user leaves save/load page
  if (LastVisiblePage = Panel_Save) or (LastVisiblePage = Panel_Load) then
    fSaves.TerminateScan;


  HidePages;

  //If Sender is one of 4 main buttons, then open the page, hide the buttons and show Return button
  Flip4MainButtons(false);
  if Sender = Button_Main[tbBuild] then
  begin
    Build_Update;
    Panel_Build.Show;
    Label_MenuTitle.Caption := gResTexts[TX_MENU_TAB_BUILD];
    Build_ButtonClick(Button_BuildRoad);
  end else

  if Sender = Button_Main[tbRatio] then
  begin
    Panel_Ratios.Show;
    SwitchPage_Ratios(Button_Ratios[1]); //Open 1st tab
    Label_MenuTitle.Caption := gResTexts[TX_MENU_TAB_DISTRIBUTE];
  end else

  if Sender = Button_Main[tbStats] then
  begin
    Stats_Resize;
    Stats_Update;
    Panel_Stats.Show;
    Label_MenuTitle.Caption := gResTexts[TX_MENU_TAB_STATISTICS];
  end else

  if (Sender = Button_Main[tbMenu])
  or (Sender = Button_Quit_No)
  or ((Sender = Button_Back) and ((LastVisiblePage = Panel_Settings)
                               or (LastVisiblePage = Panel_Load)
                               or (LastVisiblePage = Panel_Save))) then begin
    Menu_Update; //Make sure updating happens before it is shown
    Label_MenuTitle.Caption := gResTexts[TX_MENU_TAB_OPTIONS];
    Panel_Menu.Show;
  end else

  if Sender = Button_Menu_Save then
  begin
    fSave_Selected := -1;
    //Stop current now scan so it can't add a save after we clear the list
    fSaves.TerminateScan;
    Menu_Save_RefreshList(nil); //need to call it at last one time to setup GUI even if there are no saves
    //Initiate refresh and process each new save added
    fSaves.Refresh(Menu_Save_RefreshList, fMultiplayer);
    Panel_Save.Show;
    Label_MenuTitle.Caption := gResTexts[TX_MENU_SAVE_GAME];
  end else

  if Sender = Button_Menu_Load then begin
    fSave_Selected := -1;
    //Stop current now scan so it can't add a save after we clear the list
    fSaves.TerminateScan;
    Menu_Load_RefreshList(nil); //need to call it at least one time to setup GUI even if there are no saves
    //Initiate refresh and process each new save added
    fSaves.Refresh(Menu_Load_RefreshList, fMultiplayer);
    Panel_Load.Show;
    Label_MenuTitle.Caption := gResTexts[TX_MENU_LOAD_GAME];
  end else

  if Sender = Button_Menu_Settings then begin
    Menu_Settings_Fill;
    Panel_Settings.Show;
    Label_MenuTitle.Caption := gResTexts[TX_MENU_SETTINGS];
  end else

  if Sender = Button_Menu_Quit then
    Panel_Quit.Show
  else //If Sender is anything else - then show all 4 buttons and hide Return button
    Flip4MainButtons(True);

  //Now process all other kinds of pages
  if (Sender = Panel_Unit) then
    TKMPanel(Sender).Show;
end;


procedure TKMGamePlayInterface.DisplayHint(Sender: TObject);
begin
  if (PrevHint = Sender) then Exit; //Hint didn't changed
  if (Sender = nil) or (TKMControl(Sender).Hint = '') then
  begin
    Label_Hint.Caption := '';
    Bevel_HintBG.Hide;
  end
  else
  begin
    Label_Hint.Caption := TKMControl(Sender).Hint;
    Bevel_HintBG.Show;
    Bevel_HintBG.Width := 10 + fResource.Fonts.GetTextSize(Label_Hint.Caption, Label_Hint.Font).X;
  end;
  PrevHint := Sender;
end;


//Update viewport position when user interacts with minimap
procedure TKMGamePlayInterface.Minimap_Update(Sender: TObject; const X,Y: Integer);
begin
  fGame.Viewport.Position := KMPointF(X,Y);
end;


procedure TKMGamePlayInterface.Minimap_RightClick(Sender: TObject; const X,Y:integer);
var
  Loc: TKMPoint;
  Group: TKMUnitGroup;
begin
  Loc := MinimapView.LocalToMapCoords(X, Y, -1); //Inset by 1 pixel to catch cases "outside of map"
  if not gTerrain.TileInMapCoords(Loc.X, Loc.Y) then Exit; //Must be inside map

  //Send move order, if applicable
  if (MySpectator.Selected is TKMUnitGroup) and not fJoiningGroups and not fPlacingBeacon and not fReplay and not HasLostMPGame then
  begin
    Group := TKMUnitGroup(MySpectator.Selected);
    if Group.CanWalkTo(Loc, 0) then
    begin
      fGame.GameInputProcess.CmdArmy(gic_ArmyWalk, Group, Loc, dir_NA);
      gSoundPlayer.PlayWarrior(Group.UnitType, sp_Move);
    end;
  end;
end;


procedure TKMGamePlayInterface.Minimap_Click(Sender: TObject; const X,Y:integer);
begin
  if not fPlacingBeacon then Exit;
  fGame.GameInputProcess.CmdGame(gic_GameAlertBeacon, KMPointF(X,Y), MySpectator.PlayerIndex);
  Beacon_Cancel;
end;


constructor TKMGamePlayInterface.Create(aScreenX, aScreenY: Word; aMultiplayer, aReplay: Boolean);
var S: TKMShape;
begin
  inherited Create(aScreenX, aScreenY);

  fMultiplayer := aMultiplayer;
  fReplay := aReplay;
  //Instruct to use global Terrain
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
  ShownMessage := -1; //0 is the first message, -1 is invalid

  fMessageList := TKMMessageList.Create;
  fSaves := TKMSavesCollection.Create;

  fTeamNames := TList.Create;

  //Parent Page for whole toolbar in-game
  Panel_Main := TKMPanel.Create(fMyControls, 0, 0, aScreenX, aScreenY);

    Label_TeamName := TKMLabel.Create(Panel_Main, 0, 0, '', fnt_Grey, taCenter);

    Sidebar_Top       := TKMImage.Create(Panel_Main, 0,    0, 224, 200, 407);
    Sidebar_Middle    := TKMImage.Create(Panel_Main, 0,  200, 224, 168, 554);

    MinimapView := TKMMinimapView.Create(Panel_Main, 10, 10, 176, 176);
    MinimapView.OnChange := Minimap_Update; //Allow dragging with LMB pressed
    MinimapView.OnClickRight := Minimap_RightClick;
    MinimapView.OnMinimapClick := Minimap_Click; //For placing beacons

    Image_Clock := TKMImage.Create(Panel_Main,232,8,67,65,556);
    Image_Clock.Hide;
    Label_Clock := TKMLabel.Create(Panel_Main,265,80,'mm:ss',fnt_Outline,taCenter);
    Label_Clock.Hide;
    Label_ClockSpeedup := TKMLabel.Create(Panel_Main,265,48,'x1',fnt_Metal,taCenter);
    Label_ClockSpeedup.Hide;

    Label_ScriptedOverlay := TKMLabel.Create(Panel_Main,260,110,'',fnt_Metal,taLeft);

    Image_DirectionCursor := TKMImage.Create(Panel_Main,0,0,35,36,519);
    Image_DirectionCursor.Hide;

    //Debugging displays
    Label_DebugInfo := TKMLabel.Create(Panel_Main,224+8,106,'',fnt_Outline,taLeft);

{I plan to store all possible layouts on different pages which gets displayed one at a time}
{==========================================================================================}
  Create_Controls; //Includes all the child pages

  Create_NetWait; //Overlay blocking everyhitng but sidestack and messages
  Create_Allies; //MessagePage sibling
  Create_Chat; //On top of NetWait to allow players to chat while waiting for late opponents
  Create_ChatMenu(Panel_Main);
  Create_Message; //Must go bellow message stack
  Create_MessageLog; //Must go bellow message stack
  Create_MessageStack; //Messages, Allies, Chat icons

  Create_Pause;
  Create_Replay; //Replay controls
  Create_PlayMore; //Must be created last, so that all controls behind are blocked
  Create_MPPlayMore;

  Bevel_HintBG := TKMBevel.Create(Panel_Main,224+35,Panel_Main.Height-23,300,21);
  Bevel_HintBG.BackAlpha := 0.5;
  Bevel_HintBG.EdgeAlpha := 0.5;
  Bevel_HintBG.Hide;
  Bevel_HintBG.Anchors := [akLeft, akBottom];
  Label_Hint := TKMLabel.Create(Panel_Main,224+40,Panel_Main.Height-21,0,0,'',fnt_Outline,taLeft);
  Label_Hint.Anchors := [akLeft, akBottom];

  //Controls without a hint will reset the Hint to ''
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

  SwitchPage(nil); //Update
  Resize(aScreenX,aScreenY); //Hide/show swords according to player's resolution when game starts
  //Panel_Main.Width := aScreenX;
  //Panel_Main.Height := aScreenY;
  //UpdatePositions; //Reposition messages stack etc.
end;


destructor TKMGamePlayInterface.Destroy;
begin
  ReleaseDirectionSelector; //Make sure we don't exit leaving the cursor restrained

  fGuiGameHouse.Free;

  fMessageList.Free;
  fSaves.Free;
  fTeamNames.Free;
  inherited;
end;


procedure TKMGamePlayInterface.Resize(X,Y: Word);
var ShowSwords: Boolean;
begin
  Panel_Main.Width := X;
  Panel_Main.Height := Y;

  //Show swords filler if screen height allows
  ShowSwords := (Panel_Main.Height >= 758);
  Sidebar_Middle.Visible := ShowSwords;

  //Needs to be -10 when the swords are hidden so it fits 1024x576
  Panel_Controls.Top := Sidebar_Top.Height - 10 + (10+Sidebar_Middle.Height) * Byte(ShowSwords);
  Panel_Controls.Height := Panel_Main.Height - Panel_Controls.Top;

  if Panel_Stats.Visible then
    Stats_Resize;
end;


{Pause overlay page}
procedure TKMGamePlayInterface.Create_Pause;
begin
  Panel_Pause := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_Pause.Stretch;
  Bevel_Pause := TKMBevel.Create(Panel_Pause, -1, -1, Panel_Main.Width + 2, Panel_Main.Height + 2);
  Image_Pause := TKMImage.Create(Panel_Pause, (Panel_Main.Width div 2), (Panel_Main.Height div 2) - 40, 0, 0, 556);
  Label_Pause1 := TKMLabel.Create(Panel_Pause, (Panel_Main.Width div 2), (Panel_Main.Height div 2),
    gResTexts[TX_POPUP_PAUSE], fnt_Antiqua, taCenter);
  Label_Pause2 := TKMLabel.Create(Panel_Pause, (Panel_Main.Width div 2), (Panel_Main.Height div 2) + 20,
    Format(gResTexts[TX_GAMEPLAY_PAUSE_INFO], ['"P"']), fnt_Grey, taCenter);
  Bevel_Pause.Stretch; //Anchor to all sides
  Image_Pause.ImageCenter;
  Label_Pause1.Center;
  Label_Pause2.Center;
  Image_Pause.Center;
  Panel_Pause.Hide
end;


{ Play More overlay page,
  It's backgrounded with a full-screen bevel area which not only fades image a bit,
  but also blocks all mouse clicks - neat }
procedure TKMGamePlayInterface.Create_PlayMore;
begin
  Panel_PlayMore := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width,Panel_Main.Height);
  Panel_PlayMore.Stretch;
    Bevel_PlayMore := TKMBevel.Create(Panel_PlayMore,-1,-1,Panel_Main.Width+2,Panel_Main.Height+2);
    Bevel_PlayMore.Stretch;

    Panel_PlayMoreMsg := TKMPanel.Create(Panel_PlayMore,(Panel_Main.Width div 2)-100,(Panel_Main.Height div 2)-100,200,200);
    Panel_PlayMoreMsg.Center;
      Image_PlayMore:=TKMImage.Create(Panel_PlayMoreMsg,100,40,0,0,556);
      Image_PlayMore.ImageCenter;

      Label_PlayMore  := TKMLabel.Create(Panel_PlayMoreMsg,100,80,NO_TEXT,fnt_Outline,taCenter);
      Button_PlayMore := TKMButton.Create(Panel_PlayMoreMsg,0,100,200,30,NO_TEXT,bsGame);
      Button_PlayQuit := TKMButton.Create(Panel_PlayMoreMsg,0,140,200,30,NO_TEXT,bsGame);
      Button_PlayMore.OnClick := PlayMoreClick;
      Button_PlayQuit.OnClick := PlayMoreClick;
    Panel_PlayMore.Hide; //Initially hidden
end;


procedure TKMGamePlayInterface.Create_MPPlayMore;
begin
  Panel_MPPlayMore := TKMPanel.Create(Panel_Main,(Panel_Main.Width div 2)-200,(Panel_Main.Height div 2)-100,400,200);
  Panel_MPPlayMore.Center;
    Bevel_MPPlayMore := TKMBevel.Create(Panel_MPPlayMore,-1,-1,Panel_MPPlayMore.Width+2,Panel_MPPlayMore.Height+2);
    Bevel_MPPlayMore.Stretch;

      Image_MPPlayMore:=TKMImage.Create(Panel_MPPlayMore,200,40,0,0,556);
      Image_MPPlayMore.ImageCenter;

      Label_MPPlayMore  := TKMLabel.Create(Panel_MPPlayMore,200,80,NO_TEXT,fnt_Outline,taCenter);
      Button_MPPlayMore := TKMButton.Create(Panel_MPPlayMore,100,100,200,30,NO_TEXT,bsGame);
      Button_MPPlayQuit := TKMButton.Create(Panel_MPPlayMore,100,140,200,30,NO_TEXT,bsGame);
      Button_MPPlayMore.OnClick := MPPlayMoreClick;
      Button_MPPlayQuit.OnClick := MPPlayMoreClick;
    Panel_MPPlayMore.Hide; //Initially hidden
end;


//Waiting for Net events page, it's similar to PlayMore, but is layered differentlybelow chat panel
procedure TKMGamePlayInterface.Create_NetWait;
begin
  Panel_NetWait := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width,Panel_Main.Height);
  Panel_NetWait.Stretch;
    Bevel_NetWait := TKMBevel.Create(Panel_NetWait,-1,-1,Panel_Main.Width+2,Panel_Main.Height+2);
    Bevel_NetWait.Stretch;

    Panel_NetWaitMsg := TKMPanel.Create(Panel_NetWait,0,(Panel_Main.Height div 2)-200,Panel_Main.Width,400);
    Panel_NetWaitMsg.Center;
      Image_NetWait:=TKMImage.Create(Panel_NetWaitMsg,Panel_Main.Width div 2,40,0,0,556);
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
    Panel_NetWait.Hide; //Initially hidden
end;


procedure TKMGamePlayInterface.Create_MessageStack;
var
  I: Integer;
begin
  Image_MPChat := TKMImage.Create(Panel_Main,TOOLBAR_WIDTH,Panel_Main.Height-48,30,48,494);
  Image_MPChat.Anchors := [akLeft, akBottom];
  Image_MPChat.HighlightOnMouseOver := true;
  Image_MPChat.Hint := gResTexts[TX_GAMEPLAY_CHAT_HINT];
  Image_MPChat.OnClick := Chat_Click;
  Label_MPChatUnread := TKMLabel.Create(Panel_Main,TOOLBAR_WIDTH,Panel_Main.Height-30,30,36,'',fnt_Outline,taCenter);
  Label_MPChatUnread.FontColor := $FF0000FF; //Red
  Label_MPChatUnread.Anchors := [akLeft, akBottom];
  Label_MPChatUnread.Hitable := false; //Clicks should only go to the image, not the flashing label
  Label_MPChatUnread.AutoWrap := true;

  Image_MPAllies := TKMImage.Create(Panel_Main,TOOLBAR_WIDTH,Panel_Main.Height-48*2,30,48,496);
  Image_MPAllies.Anchors := [akLeft, akBottom];
  Image_MPAllies.HighlightOnMouseOver := True;
  Image_MPAllies.Hint := gResTexts[TX_GAMEPLAY_PLAYERS_HINT];
  Image_MPAllies.OnClick := Allies_Click;

  Image_MessageLog := TKMImage.Create(Panel_Main,TOOLBAR_WIDTH,Panel_Main.Height - 48 - IfThen(fMultiplayer, 48*2),30,48,495);
  Image_MessageLog.Anchors := [akLeft, akBottom];
  Image_MessageLog.HighlightOnMouseOver := true;
  Image_MessageLog.Hint := gResTexts[TX_GAME_MESSAGE_LOG];
  Image_MessageLog.OnClick := MessageLog_Click;
  Image_MessageLog.Hide; //Will be shows on first message

  for I := 0 to MAX_VISIBLE_MSGS do
  begin
    Image_Message[I] := TKMImage.Create(Panel_Main, TOOLBAR_WIDTH, 0, 30, 48, 495);
    Image_Message[I].Top := Panel_Main.Height - 48 - I * 48 - IfThen(fMultiplayer and not fReplay, 48 * 2);
    Image_Message[I].Anchors := [akLeft, akBottom];
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
    //todo: Button_ReplayFF       := TKMButton.Create(Panel_ReplayCtrl,125, 24, 24, 24, 393, rxGui, bsGame);
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

    Button_ReplayStep.Disable; //Initial state
    Button_ReplayResume.Disable; //Initial state

    Dropbox_ReplayFOW := TKMDropList.Create(Panel_ReplayCtrl, 0, 50, 160, 20, fnt_Metal, '', bsGame);
    Dropbox_ReplayFOW.Hint := gResTexts[TX_REPLAY_PLAYER_PERSPECTIVE];
    Dropbox_ReplayFOW.OnChange := ReplayClick;
end;


//Individual message page
procedure TKMGamePlayInterface.Create_Message;
begin
  Panel_Message := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - MESSAGE_AREA_HEIGHT, 600, MESSAGE_AREA_HEIGHT);
  Panel_Message.Anchors := [akLeft, akBottom];
  Panel_Message.Hide; //Hide it now because it doesn't get hidden by SwitchPage

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
    Button_MessageDelete.MakesSound := False; //Don't play default Click as these buttons use sfx_MessageClose

    Image_MessageClose := TKMImage.Create(Panel_Message, 600 - 76, 24, 32, 32, 52);
    Image_MessageClose.Anchors := [akTop, akLeft];
    Image_MessageClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
    Image_MessageClose.OnClick := Message_Close;
    Image_MessageClose.HighlightOnMouseOver := True;
end;


//Message log page
//there's a queue of not-that-important messages
procedure TKMGamePlayInterface.Create_MessageLog;
var
  I: Integer;
  H: Integer;
begin
  H := 20 * MAX_LOG_MSGS + 2; //+2 for some margin at the bottom

  Panel_MessageLog := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - (H + 65 + 20), 600, H + 65 + 20);
  Panel_MessageLog.Anchors := [akLeft, akBottom];
  Panel_MessageLog.Hide; //Hide it now because it doesn't get hidden by SwitchPage

    TKMImage.Create(Panel_MessageLog, 0, 0, 600, 500, 409);

    Image_MessageLogClose := TKMImage.Create(Panel_MessageLog, 600 - 76, 24, 32, 32, 52);
    Image_MessageLogClose.Anchors := [akTop, akLeft];
    Image_MessageLogClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
    Image_MessageLogClose.OnClick := MessageLog_Close;
    Image_MessageLogClose.HighlightOnMouseOver := True;

    ColumnBox_MessageLog := TKMColumnBox.Create(Panel_MessageLog, 45, 60, 600 - 90, H, fnt_Grey, bsGame);
    ColumnBox_MessageLog.Anchors := [akLeft, akTop, akRight, akBottom];
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


{Chat page}
procedure TKMGamePlayInterface.Create_Chat;
begin
  Panel_Chat := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - MESSAGE_AREA_HEIGHT, 600, MESSAGE_AREA_HEIGHT);
  Panel_Chat.Anchors := [akLeft, akBottom];
  Panel_Chat.Hide;

    Image_Chat := TKMImage.Create(Panel_Chat, 0, 0, 600, 500, 409);
    Image_Chat.Anchors := [akLeft,akTop,akBottom];

    //Allow to resize chat area height
    Dragger_Chat := TKMDragger.Create(Panel_Chat, 45, 36, 600-130, 10);
    Dragger_Chat.Anchors := [akTop];
    Dragger_Chat.SetBounds(0, -MESSAGE_AREA_RESIZE_Y, 0, 0);
    Dragger_Chat.OnMove := Chat_Resize;

    Memo_ChatText := TKMMemo.Create(Panel_Chat,45,50,600-85,101, fnt_Arial, bsGame);
    Memo_ChatText.Anchors := [akLeft, akTop, akRight, akBottom];
    Memo_ChatText.AutoWrap := True;
    Memo_ChatText.IndentAfterNL := True; //Don't let players fake system messages
    Memo_ChatText.ScrollDown := True;

    Edit_ChatMsg := TKMEdit.Create(Panel_Chat, 75, 154, 380, 20, fnt_Arial);
    Edit_ChatMsg.Anchors := [akLeft, akRight, akBottom];
    Edit_ChatMsg.OnKeyDown := Chat_Post;
    Edit_ChatMsg.Text := '';
    Edit_ChatMsg.ShowColors := True;

    Button_ChatRecipient := TKMButtonFlat.Create(Panel_Chat,45,154,132,20,0);
    Button_ChatRecipient.Font := fnt_Grey;
    Button_ChatRecipient.CapOffsetY := -11;
    Button_ChatRecipient.OnClick := Chat_MenuShow;
    Button_ChatRecipient.Anchors := [akRight, akBottom];

    Image_ChatClose := TKMImage.Create(Panel_Chat, 600-80, 18, 32, 32, 52);
    Image_ChatClose.Anchors := [akTop, akRight];
    Image_ChatClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
    Image_ChatClose.OnClick := Chat_Close;
    Image_ChatClose.HighlightOnMouseOver := True;
end;


procedure TKMGamePlayInterface.Create_ChatMenu(aParent: TKMPanel);
begin
  Menu_Chat := TKMPopUpMenu.Create(aParent, 120);
  Menu_Chat.Anchors := [akLeft, akBottom];
  //Menu gets populated right before show
  Menu_Chat.AddItem(NO_TEXT);
  Menu_Chat.OnClick := Chat_MenuClick;
  Chat_MenuSelect(-1); //Initialise it
end;


procedure TKMGamePlayInterface.Create_Controls;
const
  MainHint: array [TKMTabButtons] of Word = (TX_MENU_TAB_HINT_BUILD, TX_MENU_TAB_HINT_DISTRIBUTE,
                                             TX_MENU_TAB_HINT_STATISTICS, TX_MENU_TAB_HINT_OPTIONS);
var
  T: TKMTabButtons;
begin
  Panel_Controls := TKMPanel.Create(Panel_Main, 0, 368, 224, 376);
  //Resized manually on .Resize to be most efficient in space management

    //We need several of these to cover max of 1534x2560 (vertically oriented)
    TKMImage.Create(Panel_Controls, 0,    0, 224, 400, 404);
    TKMImage.Create(Panel_Controls, 0,  400, 224, 400, 404);
    TKMImage.Create(Panel_Controls, 0,  800, 224, 400, 404);
    TKMImage.Create(Panel_Controls, 0, 1200, 224, 400, 404);
    TKMImage.Create(Panel_Controls, 0, 1600, 224, 400, 404);
    TKMImage.Create(Panel_Controls, 0, 2000, 224, 400, 404);

    //Main 4 buttons
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

  Create_Build;
  Create_Ratios;
  Create_Stats;
  Create_Menu;
    Create_Save;
    Create_Load;
    Create_Settings;
    Create_Quit;

  Create_Unit;

  fGuiGameHouse := TKMGUIGameHouse.Create(Panel_Controls);
  fGuiGameHouse.OnHouseDemolish := House_Demolish;
end;


{Allies page}
procedure TKMGamePlayInterface.Create_Allies;
var I,K: Integer;
begin
  Panel_Allies := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - MESSAGE_AREA_HEIGHT, 800, MESSAGE_AREA_HEIGHT);
  Panel_Allies.Anchors := [akLeft, akBottom];
  Panel_Allies.Hide;

    with TKMImage.Create(Panel_Allies,0,0,800,190,409) do ImageAnchors := [akLeft, akRight, akTop];

    Label_PeacetimeRemaining := TKMLabel.Create(Panel_Allies,400,20,'',fnt_Outline,taCenter);

    for I := 0 to MAX_PLAYERS - 1 do
    begin
      if (I mod 4) = 0 then //Header for each column
      begin
        TKMLabel.Create(Panel_Allies,  70+(I div 4)*380, 60, 140, 20, gResTexts[TX_LOBBY_HEADER_PLAYERS], fnt_Outline, taLeft);
        TKMLabel.Create(Panel_Allies, 220+(I div 4)*380, 60, 140, 20, gResTexts[TX_LOBBY_HEADER_TEAM], fnt_Outline, taLeft);
        TKMLabel.Create(Panel_Allies, 350+(I div 4)*380, 60, gResTexts[TX_LOBBY_HEADER_PINGFPS], fnt_Outline, taCenter);
      end;
      Image_AlliesFlag[I] := TKMImage.Create(Panel_Allies,       50+(I div 4)*380, 82+(I mod 4)*24, 16,  11,  0, rxGuiMain);
      Label_AlliesPlayer[I] := TKMLabel.Create(Panel_Allies,     70+(I div 4)*380, 80+(I mod 4)*24, 140, 20, '', fnt_Grey, taLeft);
      Label_AlliesTeam[I]   := TKMLabel.Create(Panel_Allies,    220+(I div 4)*380, 80+(I mod 4)*24, 120, 20, '', fnt_Grey, taLeft);
      DropBox_AlliesTeam[I] := TKMDropList.Create(Panel_Allies, 220+(I div 4)*380, 80+(I mod 4)*24, 120, 20, fnt_Grey, '', bsGame);
      DropBox_AlliesTeam[I].Hide; //Use label for demos until we fix exploits
      DropBox_AlliesTeam[I].Add('-');
      for K := 1 to 4 do DropBox_AlliesTeam[I].Add(IntToStr(K));
      DropBox_AlliesTeam[I].OnChange := AlliesTeamChange;
      DropBox_AlliesTeam[I].DropUp := True; //Doesn't fit if it drops down
      Label_AlliesPing[I]   := TKMLabel.Create(Panel_Allies,   350+(I div 4)*380, 80+(I mod 4)*24, '', fnt_Grey, taCenter);
    end;

    Image_AlliesClose:=TKMImage.Create(Panel_Allies,800-97,24,32,32,52,rxGui);
    Image_AlliesClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
    Image_AlliesClose.OnClick := Allies_Close;
    Image_AlliesClose.HighlightOnMouseOver := True;
end;


{Build page}
procedure TKMGamePlayInterface.Create_Build;
var I: Integer;
begin
  Panel_Build := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 332);
    Label_Build := TKMLabel.Create(Panel_Build, 0, 10, TB_WIDTH, 0, '', fnt_Outline, taCenter);
    Image_Build_Selected := TKMImage.Create(Panel_Build, 0, 40, 32, 32, 335);
    Image_Build_Selected.ImageCenter;
    Image_BuildCost_WoodPic := TKMImage.Create(Panel_Build, 67, 40, 32, 32, 353);
    Image_BuildCost_WoodPic.ImageCenter;
    Image_BuildCost_StonePic := TKMImage.Create(Panel_Build, 122, 40, 32, 32, 352);
    Image_BuildCost_StonePic.ImageCenter;
    Label_BuildCost_Wood  := TKMLabel.Create(Panel_Build,  97, 50, 20, 20, '', fnt_Outline, taLeft);
    Label_BuildCost_Stone := TKMLabel.Create(Panel_Build, 152, 50, 20, 20, '', fnt_Outline, taLeft);

    Button_BuildRoad    := TKMButtonFlat.Create(Panel_Build,   0, 80, 33, 33, 335);
    Button_BuildField   := TKMButtonFlat.Create(Panel_Build,  37, 80, 33, 33, 337);
    Button_BuildWine    := TKMButtonFlat.Create(Panel_Build,  74, 80, 33, 33, 336);
    Button_BuildCancel  := TKMButtonFlat.Create(Panel_Build, 148, 80, 33, 33, 340);
    Button_BuildRoad.OnClick    := Build_ButtonClick;
    Button_BuildField.OnClick   := Build_ButtonClick;
    Button_BuildWine.OnClick    := Build_ButtonClick;
    Button_BuildCancel.OnClick  := Build_ButtonClick;
    Button_BuildRoad.Hint   := gResTexts[TX_BUILD_ROAD_HINT];
    Button_BuildField.Hint  := gResTexts[TX_BUILD_FIELD_HINT];
    Button_BuildWine.Hint   := gResTexts[TX_BUILD_WINE_HINT];
    Button_BuildCancel.Hint := gResTexts[TX_BUILD_CANCEL_HINT];

    for i:=1 to GUI_HOUSE_COUNT do
    if GUIHouseOrder[i] <> ht_None then
    begin
      Button_Build[i] := TKMButtonFlat.Create(Panel_Build, ((i-1) mod 5)*37,120+((i-1) div 5)*37,33,33,
      fResource.HouseDat[GUIHouseOrder[i]].GUIIcon);

      Button_Build[i].OnClick := Build_ButtonClick;
      Button_Build[i].Hint := fResource.HouseDat[GUIHouseOrder[i]].HouseName;
    end;
end;


{Ratios page}
procedure TKMGamePlayInterface.Create_Ratios;
const Res:array[1..4] of TWareType = (wt_Steel,wt_Coal,wt_Wood,wt_Corn);
var I: Integer;
begin
  Panel_Ratios:=TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 332);

  for i:=1 to 4 do begin
    Button_Ratios[i]         := TKMButton.Create(Panel_Ratios, (i-1)*40,20,32,32,0, rxGui, bsGame);
    Button_Ratios[i].TexID   := fResource.Wares[Res[i]].GUIIcon;
    Button_Ratios[i].Hint    := fResource.Wares[Res[i]].Title;
    Button_Ratios[i].Tag     := i;
    Button_Ratios[i].OnClick := SwitchPage_Ratios;
  end;

  Image_RatioPic0 := TKMImage.Create(Panel_Ratios,4,76,32,32,327);
  Label_RatioLab0 := TKMLabel.Create(Panel_Ratios,36,72,148,30,NO_TEXT,fnt_Outline,taLeft);

  for i:=1 to 4 do begin
    Image_RatioPic[i]            := TKMImage.Create(Panel_Ratios,4,124+(i-1)*50,32,32,327);
    TrackBar_RatioRat[i]         := TKMTrackBar.Create(Panel_Ratios,40,116+(i-1)*50,140,0,5);
    TrackBar_RatioRat[i].Font    := fnt_Grey; //fnt_Metal doesn't fit the text
    TrackBar_RatioRat[i].Tag     := i;
    TrackBar_RatioRat[i].OnChange:= RatiosChange;
  end;
end;


{Statistics page}
procedure TKMGamePlayInterface.Create_Stats;
const House_Width = 30; Unit_Width = 26;
var
  I, K: Integer;
  HT: THouseType;
  UT: TUnitType;
  OffX: Integer;
begin
  Panel_Stats := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 332);
  Panel_Stats.Anchors := [akLeft, akTop, akBottom];

  for I := 0 to High(StatPlan) do
  begin
    //Houses block
    Panel_StatBlock[I] := TKMPanel.Create(Panel_Stats, 0, 0, 30, 30);
    with TKMBevel.Create(Panel_StatBlock[I], 0, 0, 30, 30) do Stretch;

    OffX := 0;
    for K := Low(StatPlan[I].HouseType) to High(StatPlan[I].HouseType) do
    if StatPlan[I].HouseType[K] <> ht_None then
    begin
      HT := StatPlan[I].HouseType[K];
      Stat_HousePic[HT] := TKMImage.Create(Panel_StatBlock[I], OffX, 0, House_Width, 30, 41); //Filled with [?] at start
      Stat_HousePic[HT].Hint := fResource.HouseDat[HT].HouseName;
      Stat_HousePic[HT].ImageCenter;
      Stat_HouseWip[HT] := TKMLabel.Create(Panel_StatBlock[I], OffX + House_Width  ,  0,  '', fnt_Grey, taRight);
      Stat_HouseWip[HT].Hitable := False;
      Stat_HouseQty[HT] := TKMLabel.Create(Panel_StatBlock[I], OffX + House_Width-2, 16, '-', fnt_Grey, taRight);
      Stat_HouseQty[HT].Hitable := False;
      Inc(OffX, House_Width);
    end;

    for K := Low(StatPlan[I].UnitType) to High(StatPlan[I].UnitType) do
    if StatPlan[I].UnitType[K] <> ut_None then
    begin
      UT := StatPlan[I].UnitType[K];
      Stat_UnitPic[UT] := TKMImage.Create(Panel_StatBlock[I], OffX, 0, Unit_Width, 30, fResource.UnitDat[UT].GUIIcon);
      Stat_UnitPic[UT].Hint := fResource.UnitDat[UT].GUIName;
      Stat_UnitPic[UT].ImageCenter;
      Stat_UnitWip[UT] := TKMLabel.Create(Panel_StatBlock[I], OffX + Unit_Width  ,  0,  '', fnt_Grey, taRight);
      Stat_UnitWip[UT].Hitable := False;
      Stat_UnitQty[UT] := TKMLabel.Create(Panel_StatBlock[I], OffX + Unit_Width-2, 16, '-', fnt_Grey, taRight);
      Stat_UnitQty[UT].Hitable := False;
      Inc(OffX, Unit_Width);
    end;
    Panel_StatBlock[I].Width := OffX;
  end;
end;


{Menu page}
procedure TKMGamePlayInterface.Create_Menu;
begin
  Panel_Menu := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 332);
  Button_Menu_Load := TKMButton.Create(Panel_Menu, 0, 20, TB_WIDTH, 30, gResTexts[TX_MENU_LOAD_GAME], bsGame);
  Button_Menu_Load.OnClick := SwitchPage;
  Button_Menu_Load.Hint := gResTexts[TX_MENU_LOAD_GAME];
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
  Label_Menu_Track.Hitable := False; //It can block hits for the track Up/Down buttons as they overlap
  Label_GameTime := TKMLabel.Create(Panel_Menu, 0, 214, TB_WIDTH, 20, '', fnt_Outline, taCenter);
end;


{Save page}
procedure TKMGamePlayInterface.Create_Save;
begin
  Panel_Save := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 332);

    //Edit field created first to pick a focus on panel show
    Edit_Save := TKMEdit.Create(Panel_Save, 0, 235, TB_WIDTH, 20, fnt_Metal);
    Edit_Save.AllowedChars := acFileName;
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


{Load page}
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


{Options page}
procedure TKMGamePlayInterface.Create_Settings;
const
  PAD = 10;
  WID = TB_WIDTH - PAD * 2;
begin
  Panel_Settings := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 332);
    CheckBox_Settings_Autosave := TKMCheckBox.Create(Panel_Settings,PAD,15,WID,20,gResTexts[TX_MENU_OPTIONS_AUTOSAVE],fnt_Metal);
    CheckBox_Settings_Autosave.OnClick := Menu_Settings_Change;
    TrackBar_Settings_Brightness := TKMTrackBar.Create(Panel_Settings,PAD,40,WID,0,20);
    TrackBar_Settings_Brightness.Caption := gResTexts[TX_MENU_OPTIONS_BRIGHTNESS];
    TrackBar_Settings_Brightness.OnChange := Menu_Settings_Change;
    TrackBar_Settings_ScrollSpeed := TKMTrackBar.Create(Panel_Settings,PAD,95,WID,0,20);
    TrackBar_Settings_ScrollSpeed.Caption := gResTexts[TX_MENU_OPTIONS_SCROLL_SPEED];
    TrackBar_Settings_ScrollSpeed.OnChange := Menu_Settings_Change;
    TrackBar_Settings_SFX := TKMTrackBar.Create(Panel_Settings,PAD,150,WID,0,20);
    TrackBar_Settings_SFX.Caption := gResTexts[TX_MENU_SFX_VOLUME];
    TrackBar_Settings_SFX.Hint := gResTexts[TX_MENU_SFX_VOLUME_HINT];
    TrackBar_Settings_SFX.OnChange := Menu_Settings_Change;
    TrackBar_Settings_Music := TKMTrackBar.Create(Panel_Settings,PAD,205,WID,0,20);
    TrackBar_Settings_Music.Caption := gResTexts[TX_MENU_MUSIC_VOLUME];
    TrackBar_Settings_Music.Hint := gResTexts[TX_MENU_MUSIC_VOLUME_HINT];
    TrackBar_Settings_Music.OnChange := Menu_Settings_Change;
    CheckBox_Settings_MusicOff := TKMCheckBox.Create(Panel_Settings,PAD,260,WID,20,gResTexts[TX_MENU_OPTIONS_MUSIC_DISABLE],fnt_Metal);
    CheckBox_Settings_MusicOff.Hint := gResTexts[TX_MENU_OPTIONS_MUSIC_DISABLE_HINT];
    CheckBox_Settings_MusicOff.OnClick := Menu_Settings_Change;
    CheckBox_Settings_ShuffleOn := TKMCheckBox.Create(Panel_Settings,PAD,285,WID,20,gResTexts[TX_MENU_OPTIONS_MUSIC_SHUFFLE],fnt_Metal);
    CheckBox_Settings_ShuffleOn.OnClick := Menu_Settings_Change;
end;


{Quit page}
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


{Unit page}
procedure TKMGamePlayInterface.Create_Unit;
begin
  Panel_Unit := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 332);
    Label_UnitName        := TKMLabel.Create(Panel_Unit,0,16,TB_WIDTH,30,'',fnt_Outline,taCenter);
    Image_UnitPic         := TKMImage.Create(Panel_Unit,0,38,54,100,521);
    Label_UnitCondition   := TKMLabel.Create(Panel_Unit,65,40,116,30,gResTexts[TX_UNIT_CONDITION],fnt_Grey,taCenter);
    ConditionBar_Unit     := TKMPercentBar.Create(Panel_Unit,65,55,116,15);
    Label_UnitTask        := TKMLabel.Create(Panel_Unit,65,80,116,60,'',fnt_Grey,taLeft);
    Label_UnitTask.AutoWrap := True;
    Label_UnitDescription := TKMLabel.Create(Panel_Unit,0,152,TB_WIDTH,200,'',fnt_Grey,taLeft); //Taken from LIB resource
    Label_UnitDescription.AutoWrap := True;
    Button_Unit_Dismiss   := TKMButton.Create(Panel_Unit,124,120,56,34,29, rxGui, bsGame);

    Panel_Unit_Dismiss := TKMPanel.Create(Panel_Unit, 0, 160, TB_WIDTH, 182);
    Label_Unit_Dismiss             := TKMLabel.Create(Panel_Unit_Dismiss,100,16,TB_WIDTH,30,'Are you sure?',fnt_Outline,taCenter);
    Button_Unit_DismissYes         := TKMButton.Create(Panel_Unit_Dismiss,50, 50,TB_WIDTH,40,'Dismiss',bsGame);
    Button_Unit_DismissNo          := TKMButton.Create(Panel_Unit_Dismiss,50,100,TB_WIDTH,40,'Cancel',bsGame);
    Button_Unit_DismissYes.OnClick := Unit_Dismiss;
    Button_Unit_DismissNo.OnClick  := Unit_Dismiss;

    Panel_Army := TKMPanel.Create(Panel_Unit, 0, 160, TB_WIDTH, 182);
    //Military buttons start at 8.170 and are 52x38/30 (60x46)
    Button_Army_GoTo   := TKMButton.Create(Panel_Army,  0,  0, 56, 40, 27, rxGui, bsGame);
    Button_Army_Stop   := TKMButton.Create(Panel_Army, 62,  0, 56, 40, 26, rxGui, bsGame);
    Button_Army_Attack := TKMButton.Create(Panel_Army,124,  0, 56, 40, 25, rxGui, bsGame);
    Button_Army_RotCCW := TKMButton.Create(Panel_Army,  0, 46, 56, 40, 23, rxGui, bsGame);
    Button_Army_Storm  := TKMButton.Create(Panel_Army, 62, 46, 56, 40, 28, rxGui, bsGame);
    Button_Army_RotCW  := TKMButton.Create(Panel_Army,124, 46, 56, 40, 24, rxGui, bsGame);
    Button_Army_ForUp  := TKMButton.Create(Panel_Army,  0, 92, 56, 40, 33, rxGui, bsGame);
    ImageStack_Army    := TKMImageStack.Create(Panel_Army, 62, 92, 56, 40, 43, 50);
    Button_Army_ForDown:= TKMButton.Create(Panel_Army,124, 92, 56, 40, 32, rxGui, bsGame);
    Button_Army_Split  := TKMButton.Create(Panel_Army,  0,138, 56, 34, 31, rxGui, bsGame);
    Button_Army_Join   := TKMButton.Create(Panel_Army, 62,138, 56, 34, 30, rxGui, bsGame);
    Button_Army_Feed   := TKMButton.Create(Panel_Army,124,138, 56, 34, 29, rxGui, bsGame);

    //All one-click-action (i.e. not attack, move, link up) army controls have a single procedure that decides what to do based on Sender
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

    //Disable not working buttons
    Button_Army_GoTo.Disable;
    Button_Army_Attack.Disable;

    //Hints
    Button_Army_GoTo.Hint     := gResTexts[TX_ARMY_GOTO_HINT];
    Button_Army_Stop.Hint     := Format(gResTexts[TX_TROOP_HALT_HINT], [SC_ARMY_HALT]);
    Button_Army_Attack.Hint   := gResTexts[TX_ARMY_ATTACK_HINT];
    Button_Army_RotCW.Hint    := gResTexts[TX_ARMY_ROTATE_CW_HINT];
    Button_Army_Storm.Hint    := gResTexts[TX_ARMY_STORM_HINT];
    Button_Army_RotCCW.Hint   := gResTexts[TX_ARMY_ROTATE_CCW_HINT];
    Button_Army_ForDown.Hint  := gResTexts[TX_ARMY_LINE_ADD_HINT];
    Button_Army_ForUp.Hint    := gResTexts[TX_ARMY_LINE_REM_HINT];
    Button_Army_Split.Hint    := Format(gResTexts[TX_TROOP_SPLIT_HINT], [SC_ARMY_SPLIT]);
    Button_Army_Join.Hint     := Format(gResTexts[TX_TROOP_LINK_HINT], [SC_ARMY_LINK]);
    Button_Army_Feed.Hint     := gResTexts[TX_ARMY_FEED_HINT];
    Button_Unit_Dismiss.Hint  := 'Dismiss unit';

    {Army controls...
    Go to     Stop      Attack
    Rotate    Storm     Rotate
    -Column   [Info]    +Column
    Split     Join      Feed}

    Panel_Army_JoinGroups := TKMPanel.Create(Panel_Unit, 0, 160, TB_WIDTH, 182);
    Label_Army_Join_Message := TKMLabel.Create(Panel_Army_JoinGroups, 0, 30, TB_WIDTH, 65, gResTexts[TX_ARMY_JOIN_SELECT], fnt_Outline, taCenter);
    Button_Army_Join_Cancel := TKMButton.Create(Panel_Army_JoinGroups, 0, 95, TB_WIDTH, 30, gResTexts[TX_ARMY_JOIN_CANCEL], bsGame);

  Button_Army_Join_Cancel.OnClick := Army_HideJoinMenu;
end;


procedure TKMGamePlayInterface.Chat_Click(Sender: TObject);
begin
  if Panel_Chat.Visible then
    Chat_Close(Sender)
  else
    Chat_Show(Sender);
end;


procedure TKMGamePlayInterface.Chat_Show(Sender: TObject);
begin
  gSoundPlayer.Play(sfxn_MPChatOpen);
  Allies_Close(nil);
  Panel_Chat.Show;
  Message_Close(nil);
  MessageLog_Close(nil);
  Label_MPChatUnread.Caption := ''; //No unread messages
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
  Chat_Close(nil);
  Message_Close(nil);
  MessageLog_Close(nil);
end;


procedure TKMGamePlayInterface.House_Demolish;
begin
  SwitchPage(Button_Main[tbBuild]);
end;


//Click on the same message again closes it
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

  //Highlight target message icon
  for I := 0 to MAX_VISIBLE_MSGS do
    Image_Message[I].Highlight := (ShownMessage = I);

  Label_MessageText.Caption := fMessageList.MessagesStack[ShownMessage].Text;
  Button_MessageGoTo.Visible := not KMSamePoint(fMessageList.MessagesStack[ShownMessage].Loc, KMPoint(0,0));

  Allies_Close(nil);
  Chat_Close(nil); //Removes focus from Edit_Text
  MessageLog_Close(nil);
  Panel_Message.Show;
  //Must update top AFTER showing panel, otherwise Button_MessageGoTo.Visible will always return false
  Button_MessageDelete.Top := IfThen(Button_MessageGoTo.Visible, 104, 74);
  gSoundPlayer.Play(sfx_MessageOpen); //Play parchment sound when they open the message
end;


//Message has been closed
procedure TKMGamePlayInterface.Message_Close(Sender: TObject);
begin
  //Remove highlight
  if ShownMessage <> -1 then
  begin
    Image_Message[ShownMessage].Highlight := False;

    //Play sound
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
  if ShownMessage = -1 then Exit; //Player pressed DEL with no Msg opened

  OldMsg := ShownMessage;

  Message_Close(Sender);
  fMessageList.RemoveStack(OldMsg);

  Message_UpdateStack;
  DisplayHint(nil);
end;


procedure TKMGamePlayInterface.Message_GoTo(Sender: TObject);
begin
  fGame.Viewport.Position := KMPointF(fMessageList.MessagesStack[ShownMessage].Loc);
end;


procedure TKMGamePlayInterface.Message_UpdateStack;
var
  I: Integer;
begin
  //MessageList is unlimited, while Image_Message has fixed depth and samples data from the list on demand
  for I := 0 to MAX_VISIBLE_MSGS do
  begin
    //Disable and hide at once for safety
    Image_Message[I].Enabled := (I <= fMessageList.CountStack - 1);
    Image_Message[I].Visible := (I <= fMessageList.CountStack - 1);
    if I <= fMessageList.CountStack - 1 then
      Image_Message[i].TexID := fMessageList.MessagesStack[I].Icon;
  end;
end;


procedure TKMGamePlayInterface.Build_ButtonClick(Sender: TObject);
var I: Integer;
begin
  if Sender = nil then
  begin
    GameCursor.Mode := cmNone;
    Exit;
  end;

  //Release all buttons (houses and fields)
  for I := 0 to Panel_Build.ChildCount - 1 do
    if Panel_Build.Childs[I] is TKMButtonFlat then
      TKMButtonFlat(Panel_Build.Childs[I]).Down := False;

  //Press the button
  TKMButtonFlat(Sender).Down := true;

  //Reset building mode
  // and see if it needs to be changed

  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  Label_BuildCost_Wood.Caption  := '-';
  Label_BuildCost_Stone.Caption := '-';
  Label_Build.Caption := '';


  if Button_BuildCancel.Down then begin
    GameCursor.Mode:=cmErase;
    Image_Build_Selected.TexID := 340;
    Label_Build.Caption := gResTexts[TX_BUILD_DEMOLISH];
  end;
  if Button_BuildRoad.Down then begin
    GameCursor.Mode:=cmRoad;
    Image_Build_Selected.TexID := 335;
    Label_BuildCost_Stone.Caption:='1';
    Label_Build.Caption := gResTexts[TX_BUILD_ROAD];
  end;
  if Button_BuildField.Down then begin
    GameCursor.Mode:=cmField;
    Image_Build_Selected.TexID := 337;
    Label_Build.Caption := gResTexts[TX_BUILD_FIELD];
  end;
  if Button_BuildWine.Down then begin
    GameCursor.Mode:=cmWine;
    Image_Build_Selected.TexID := 336;
    Label_BuildCost_Wood.Caption:='1';
    Label_Build.Caption := gResTexts[TX_BUILD_WINE];
  end;

  for i:=1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[i] <> ht_None then
  if Button_Build[i].Down then begin
     GameCursor.Mode:=cmHouses;
     GameCursor.Tag1:=byte(GUIHouseOrder[i]);
     Image_Build_Selected.TexID := fResource.HouseDat[GUIHouseOrder[i]].GUIIcon;
     Label_BuildCost_Wood.Caption:=inttostr(fResource.HouseDat[GUIHouseOrder[i]].WoodCost);
     Label_BuildCost_Stone.Caption:=inttostr(fResource.HouseDat[GUIHouseOrder[i]].StoneCost);
     Label_Build.Caption := fResource.HouseDat[GUIHouseOrder[i]].HouseName;
  end;
end;


procedure TKMGamePlayInterface.ShowUnitInfo(Sender: TKMUnit; aAskDismiss: Boolean = False);
begin
  Assert(MySpectator.Selected = Sender);

  fAskDismiss  := aAskDismiss;

  if Sender = nil then
  begin
    SwitchPage(nil);
    Exit;
  end;

  SwitchPage(Panel_Unit);

  //Common properties
  Label_UnitName.Caption      := fResource.UnitDat[Sender.UnitType].GUIName;
  Image_UnitPic.TexID         := fResource.UnitDat[Sender.UnitType].GUIScroll;
  Image_UnitPic.FlagColor     := gPlayers[Sender.Owner].FlagColor;
  ConditionBar_Unit.Position  := Sender.Condition / UNIT_MAX_CONDITION;
  Label_UnitTask.Caption      := Sender.GetActivityText;

  Label_UnitDescription.Show;
  Button_Unit_Dismiss.Visible := SHOW_DISMISS_BUTTON and not fAskDismiss;
  Panel_Army.Hide;
  Panel_Army_JoinGroups.Hide;
  Panel_Unit_Dismiss.Visible := SHOW_DISMISS_BUTTON and fAskDismiss;

  Label_UnitDescription.Caption := fResource.UnitDat[Sender.UnitType].Description;
end;


procedure TKMGamePlayInterface.ShowGroupInfo(Sender: TKMUnitGroup);
var
  W: TKMUnitWarrior;
begin
  Assert(MySpectator.Selected = Sender);

  if (Sender = nil) or (Sender.SelectedUnit = nil) then
  begin
    SwitchPage(nil);
    Exit;
  end;

  W := Sender.SelectedUnit;
  SwitchPage(Panel_Unit);

  //Common properties
  Label_UnitName.Caption      := fResource.UnitDat[W.UnitType].GUIName;
  Image_UnitPic.TexID         := fResource.UnitDat[W.UnitType].GUIScroll;
  Image_UnitPic.FlagColor     := gPlayers[W.Owner].FlagColor;
  ConditionBar_Unit.Position  := W.Condition / UNIT_MAX_CONDITION;
  //We show what this individual is doing, not the whole group. However this can be useful for debugging: Sender.GetOrderText
  Label_UnitTask.Caption      := W.GetWarriorActivityText(Sender.IsAttackingUnit);

  //While selecting target to join we could get attacked
  //Then we must cancel the dialog
  if not Sender.CanTakeOrders then
    Army_HideJoinMenu(nil); //Cannot be joining while in combat/charging

  Label_UnitDescription.Hide;
  Button_Unit_Dismiss.Visible := SHOW_DISMISS_BUTTON and not fAskDismiss and not fJoiningGroups;
  Panel_Army.Visible := not fAskDismiss and not fJoiningGroups;
  Panel_Army_JoinGroups.Visible := not fAskDismiss and fJoiningGroups;
  Panel_Unit_Dismiss.Visible := SHOW_DISMISS_BUTTON and fAskDismiss and not fJoiningGroups;

  //Update army controls if required
  if Panel_Army.Visible then
  begin
    ImageStack_Army.SetCount(Sender.Count, Sender.UnitsPerRow, Sender.UnitsPerRow div 2);
    Army_ActivateControls(Sender);
  end;
end;


procedure TKMGamePlayInterface.Menu_Settings_Fill;
begin
  TrackBar_Settings_Brightness.Position   := fGameApp.GameSettings.Brightness;
  CheckBox_Settings_Autosave.Checked      := fGameApp.GameSettings.Autosave;
  TrackBar_Settings_ScrollSpeed.Position  := fGameApp.GameSettings.ScrollSpeed;
  TrackBar_Settings_SFX.Position          := Round(fGameApp.GameSettings.SoundFXVolume * TrackBar_Settings_SFX.MaxValue);
  TrackBar_Settings_Music.Position        := Round(fGameApp.GameSettings.MusicVolume * TrackBar_Settings_Music.MaxValue);
  CheckBox_Settings_MusicOff.Checked      := fGameApp.GameSettings.MusicOff;
  CheckBox_Settings_ShuffleOn.Checked     := fGameApp.GameSettings.ShuffleOn;

  TrackBar_Settings_Music.Enabled     := not CheckBox_Settings_MusicOff.Checked;
  CheckBox_Settings_ShuffleOn.Enabled := not CheckBox_Settings_MusicOff.Checked;
end;


procedure TKMGamePlayInterface.Menu_Settings_Change(Sender: TObject);
var
  MusicToggled, ShuffleToggled: Boolean;
begin
  //Change these options only if they changed state since last time
  MusicToggled   := (fGameApp.GameSettings.MusicOff <> CheckBox_Settings_MusicOff.Checked);
  ShuffleToggled := (fGameApp.GameSettings.ShuffleOn <> CheckBox_Settings_ShuffleOn.Checked);

  fGameApp.GameSettings.Brightness    := TrackBar_Settings_Brightness.Position;
  fGameApp.GameSettings.Autosave      := CheckBox_Settings_Autosave.Checked;
  fGameApp.GameSettings.ScrollSpeed   := TrackBar_Settings_ScrollSpeed.Position;
  fGameApp.GameSettings.SoundFXVolume := TrackBar_Settings_SFX.Position / TrackBar_Settings_SFX.MaxValue;
  fGameApp.GameSettings.MusicVolume   := TrackBar_Settings_Music.Position / TrackBar_Settings_Music.MaxValue;
  fGameApp.GameSettings.MusicOff      := CheckBox_Settings_MusicOff.Checked;
  fGameApp.GameSettings.ShuffleOn     := CheckBox_Settings_ShuffleOn.Checked;

  gSoundPlayer.UpdateSoundVolume(fGameApp.GameSettings.SoundFXVolume);
  fGameApp.MusicLib.UpdateMusicVolume(fGameApp.GameSettings.MusicVolume);
  if MusicToggled then
  begin
    fGameApp.MusicLib.ToggleMusic(not fGameApp.GameSettings.MusicOff);
    if not fGameApp.GameSettings.MusicOff then
      ShuffleToggled := True; //Re-shuffle songs if music has been enabled
  end;
  if ShuffleToggled then
    fGameApp.MusicLib.ToggleShuffle(fGameApp.GameSettings.ShuffleOn);

  TrackBar_Settings_Music.Enabled := not CheckBox_Settings_MusicOff.Checked;
  CheckBox_Settings_ShuffleOn.Enabled := not CheckBox_Settings_MusicOff.Checked;
end;


//Quit the mission and return to main menu
procedure TKMGamePlayInterface.Menu_QuitMission(Sender:TObject);
begin
  //Show outcome depending on actual situation. By default PlayOnState is gr_Cancel, if playing on after victory/defeat it changes
  fGameApp.Stop(fGame.PlayOnState);
end;


procedure TKMGamePlayInterface.Menu_NextTrack(Sender:TObject);
begin
  fGameApp.MusicLib.PlayNextTrack;
end;


procedure TKMGamePlayInterface.Menu_PreviousTrack(Sender:TObject);
begin
  fGameApp.MusicLib.PlayPreviousTrack;
end;


procedure TKMGamePlayInterface.Army_Issue_Order(Sender: TObject);
var
  Group: TKMUnitGroup;
begin
  if MySpectator.Selected = nil then exit;
  if not (MySpectator.Selected is TKMUnitGroup) then Exit;

  {Not implemented yet
  if Sender = Button_Unit_Dismiss then
  begin
    ShowUnitInfo(TKMUnit(MySpectator.Selected), true);
  end;}

  Group := TKMUnitGroup(MySpectator.Selected);

  //if Sender = Button_Army_GoTo    then ; //This command makes no sense unless player has no right-mouse-button
  if Sender = Button_Army_Stop    then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyHalt, Group);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_Halt);
  end;
  //if Sender = Button_Army_Attack  then ; //This command makes no sense unless player has no right-mouse-button
  if Sender = Button_Army_RotCW   then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyFormation, Group, tdCW, 0);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_RotRight);
  end;
  if Sender = Button_Army_Storm   then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyStorm, Group);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_StormAttack);
  end;
  if Sender = Button_Army_RotCCW  then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyFormation, Group, tdCCW, 0);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_RotLeft);
  end;
  if Sender = Button_Army_ForDown then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyFormation, Group, tdNone, 1);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_Formation);
  end;
  if Sender = Button_Army_ForUp   then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyFormation, Group, tdNone, -1);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_Formation);
  end;
  if Sender = Button_Army_Split   then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmySplit, Group);
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
    fGame.GameInputProcess.CmdArmy(gic_ArmyFeed, Group);
    gSoundPlayer.PlayWarrior(Group.UnitType, sp_Eat);
  end;
end;


procedure TKMGamePlayInterface.Unit_Dismiss(Sender: TObject);
begin
  if (MySpectator.Selected = nil)
  or not (MySpectator.Selected is TKMUnit) then
    Exit;

  if Sender = Button_Unit_DismissYes then
  begin
    //DISMISS UNIT
    fAskDismiss := False;
    ShowUnitInfo(nil, False); //Simpliest way to reset page and ShownUnit
    SwitchPage(nil); //Return to main menu after dismissing
  end
  else
  begin
    fAskDismiss := False;
    ShowUnitInfo(TKMUnit(MySpectator.Selected), False);  //Cancel and return to selected unit
  end;
end;


procedure TKMGamePlayInterface.Army_HideJoinMenu(Sender: TObject);
begin
  fJoiningGroups := False;
  if fResource.Cursors.Cursor in [kmc_JoinYes, kmc_JoinNo] then //Do not override non-joining cursors
    fResource.Cursors.Cursor := kmc_Default; //In case this is run with keyboard shortcut, mouse move won't happen
  Panel_Army_JoinGroups.Hide;
  if MySpectator.Selected is TKMUnitWarrior then
    Panel_Army.Show;
end;


procedure TKMGamePlayInterface.Build_Update;
var I: Integer;
begin
  for I := 1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[I] <> ht_None then
  if gPlayers[MySpectator.PlayerIndex].Stats.GetCanBuild(GUIHouseOrder[I]) then
  begin
    Button_Build[I].Enable;
    Button_Build[I].TexID := fResource.HouseDat[GUIHouseOrder[I]].GUIIcon;
    Button_Build[I].OnClick := Build_ButtonClick;
    Button_Build[I].Hint := fResource.HouseDat[GUIHouseOrder[I]].HouseName;
  end
  else
  begin
    Button_Build[I].OnClick := nil;
    Button_Build[I].TexID := 41;
    Button_Build[I].Hint := gResTexts[TX_HOUSE_NOT_AVAIABLE]; //Building not available
  end;
end;


procedure TKMGamePlayInterface.Allies_Close(Sender: TObject);
begin
  if Panel_Allies.Visible then gSoundPlayer.Play(sfxn_MPChatClose);
  Panel_Allies.Hide;
end;


procedure TKMGamePlayInterface.Chat_Close(Sender: TObject);
begin
  if Panel_Chat.Visible then gSoundPlayer.Play(sfxn_MPChatClose);
  Panel_Chat.Hide;
end;


procedure TKMGamePlayInterface.Chat_Post(Sender: TObject; Key: Word);
begin
  if (Key = VK_RETURN) and (Trim(Edit_ChatMsg.Text) <> '') and (fGame.Networking <> nil) then
  begin
    if fChatMode in [cmAll, cmTeam] then
      fGame.Networking.PostMessage(Edit_ChatMsg.Text, True, fChatMode = cmTeam);
    if fChatMode = cmWhisper then
      fGame.Networking.PostMessage(Edit_ChatMsg.Text, True, False, fChatWhisperRecipient);
    Edit_ChatMsg.Text := '';
  end;
end;


procedure TKMGamePlayInterface.Chat_Resize(Sender: TObject; X,Y: Integer);
var H: Integer;
begin
  H := EnsureRange(-Y, 0, MESSAGE_AREA_RESIZE_Y);
  Panel_Chat.Top := Panel_Main.Height - (MESSAGE_AREA_HEIGHT + H);
  Panel_Chat.Height := MESSAGE_AREA_HEIGHT + H;
end;


procedure TKMGamePlayInterface.Chat_MenuSelect(aItem: Integer);

  procedure UpdateButtonCaption(aCaption: string; aColor: Cardinal = 0);
  var CapWidth: Integer;
  const MIN_SIZE = 80; //Minimum size for the button
  begin
    //Update button width according to selected item
    CapWidth := fResource.Fonts.GetTextSize(aCaption, Button_ChatRecipient.Font).X;
    CapWidth := Max(MIN_SIZE, CapWidth+10); //Apply minimum size
    if aColor <> 0 then
      aCaption := '[$'+IntToHex(aColor and $00FFFFFF,6)+']'+aCaption;
    Button_ChatRecipient.Caption := aCaption;
    Button_ChatRecipient.Width := CapWidth;

    Edit_ChatMsg.AbsLeft := Button_ChatRecipient.AbsLeft + Button_ChatRecipient.Width + 4;
    Edit_ChatMsg.Width := Memo_ChatText.Width - Button_ChatRecipient.Width - 4;
  end;

var I: Integer;
begin
  //All
  if aItem = -1 then
  begin
    fChatMode := cmAll;
    UpdateButtonCaption(gResTexts[TX_CHAT_ALL]);
    Edit_ChatMsg.DrawOutline := False; //No outline for All
  end
  else
    //Team
    if aItem = -2 then
    begin
      fChatMode := cmTeam;
      UpdateButtonCaption(gResTexts[TX_CHAT_TEAM], $FF66FF66);
      Edit_ChatMsg.DrawOutline := True;
      Edit_ChatMsg.OutlineColor := $FF66FF66;
    end
    else
    //Whisper
    begin
      I := fGame.Networking.NetPlayers.ServerToLocal(aItem);
      if I <> -1 then
      begin
        fChatMode := cmWhisper;
        Edit_ChatMsg.DrawOutline := True;
        Edit_ChatMsg.OutlineColor := $FF00B9FF;
        with fGame.Networking.NetPlayers[I] do
        begin
          fChatWhisperRecipient := IndexOnServer;
          UpdateButtonCaption(Nikname, IfThen(FlagColorID <> 0, FlagColorToTextColor(FlagColor), 0));
        end;
      end;
    end;
end;


procedure TKMGamePlayInterface.Chat_MenuClick(Sender: TObject);
begin
  if Menu_Chat.ItemIndex <> -1 then
    Chat_MenuSelect(Menu_Chat.ItemTags[Menu_Chat.ItemIndex]);
end;


procedure TKMGamePlayInterface.Chat_MenuShow(Sender: TObject);
var C: TKMControl; I: Integer;
begin
  //First populate the list
  Menu_Chat.Clear;
  Menu_Chat.AddItem(gResTexts[TX_CHAT_ALL], -1);
  //Only show "Team" if the player is on a team
  if fGame.Networking.NetPlayers[fGame.Networking.MyIndex].Team <> 0 then
    Menu_Chat.AddItem('[$66FF66]'+gResTexts[TX_CHAT_TEAM], -2);

  for I := 1 to fGame.Networking.NetPlayers.Count do
    if I <> fGame.Networking.MyIndex then //Can't whisper to yourself
      with fGame.Networking.NetPlayers[I] do
        if IsHuman and Connected and not Dropped then
          if FlagColor <> 0 then
            Menu_Chat.AddItem('[$'+IntToHex(FlagColorToTextColor(FlagColor) and $00FFFFFF,6)+']' + Nikname, IndexOnServer)
          else
            Menu_Chat.AddItem(Nikname, IndexOnServer);

  C := TKMControl(Sender);
  //Position the menu next to the icon, but do not overlap players name
  Menu_Chat.ShowAt(C.AbsLeft, C.AbsTop - Menu_Chat.Height);
end;


procedure TKMGamePlayInterface.ReplayClick(Sender: TObject);
  procedure SetButtons(aPaused: Boolean);
  begin
    Button_ReplayPause.Enabled := aPaused;
    Button_ReplayStep.Enabled := not aPaused;
    Button_ReplayResume.Enabled := not aPaused;
  end;
begin
  if (Sender = Button_ReplayRestart) then
  begin
    fGame.RestartReplay; //reload it once again
    Exit; //Restarting the replay will destroy Self, so exit immediately
  end;

  if (Sender = Button_ReplayPause) then
  begin
    fGame.IsPaused := True;
    SetButtons(False);
  end;

  if (Sender = Button_ReplayStep) then
  begin
    fGame.StepOneFrame;
    fGame.IsPaused := False;
    SetButtons(False);
  end;

  if (Sender = Button_ReplayResume) then
  begin
    fGame.IsPaused := False;
    SetButtons(True);
  end;

  if (Sender = Button_ReplayExit) then
  begin
    fGame.GameHold(True, gr_ReplayEnd);
    SetButtons(True);
  end;

  if (Sender = Dropbox_ReplayFOW) then
  begin
    MySpectator.FOWIndex := Dropbox_ReplayFOW.GetTag(Dropbox_ReplayFOW.ItemIndex);
    fGame.Minimap.Update(False); //Force update right now so FOW doesn't appear to lag
  end;
end;


procedure TKMGamePlayInterface.MessageIssue(aKind: TKMMessageKind; aText: string);
begin
  fMessageList.Add(aKind, aText, KMPoint(0,0));
  Message_UpdateStack;
  gSoundPlayer.Play(sfx_MessageNotice, 4); //Play horn sound on new message if it is the right type
end;


procedure TKMGamePlayInterface.MessageIssue(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
begin
  fMessageList.Add(aKind, aText, aLoc);
  Message_UpdateStack;
  gSoundPlayer.Play(sfx_MessageNotice, 4); //Play horn sound on new message if it is the right type
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
    Chat_Close(nil); //Removes focus from Edit_Text
    MessageLog_Close(nil);
    Message_Close(nil);

    Panel_MessageLog.Show;
    ColumnBox_MessageLog.TopIndex := ColumnBox_MessageLog.RowCount;
    gSoundPlayer.Play(sfx_MessageOpen); //Play parchment sound when they open the message
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
  Msg: TKMMessage;
  H: TKMHouse;
begin
  ItemId := ColumnBox_MessageLog.ItemIndex;
  if ItemId = -1 then Exit;

  MessageId := ColumnBox_MessageLog.Rows[ItemId].Tag;
  if MessageId = -1 then Exit;

  Msg := fMessageList.MessagesLog[MessageId];
  Msg.IsRead := True;

  //Jump to location
  fGame.Viewport.Position := KMPointF(Msg.Loc);

  //Try to highlight the house in question
  H := gPlayers.HousesHitTest(Msg.Loc.X, Msg.Loc.Y);
  if H <> nil then
    MySpectator.Highlight := H;

  MessageLog_Update(True);
end;


//Sync displayed messages with queue
//We show only last 8 messages by design
procedure TKMGamePlayInterface.MessageLog_Update(aFullRefresh: Boolean);
var
  I, K: Integer;
  R: TKMListRow;
begin
  //Exit if synced already
  if not aFullRefresh and (fLastSyncedMessage = fMessageList.CountLog) then Exit;

  //Clear the selection if a new item is added so the wrong one is not selected
  if fLastSyncedMessage <> fMessageList.CountLog then
    ColumnBox_MessageLog.ItemIndex := -1;

  K := 0;
  for I := Max(fMessageList.CountLog - MAX_LOG_MSGS, 0) to fMessageList.CountLog - 1 do
  begin
    R := MakeListRow(['', fMessageList.MessagesLog[I].Text], I);

    if fMessageList.MessagesLog[I].Kind = mkUnit then
    begin
      R.Cells[0].Pic := MakePic(rxGui, 588);
      if fMessageList.MessagesLog[I].IsRead then
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
      if fMessageList.MessagesLog[I].IsRead then
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

  fLastSyncedMessage := fMessageList.CountLog;
end;


//Update message stack when first log message arrives
procedure TKMGamePlayInterface.MessageStack_UpdatePositions;
var
  I: Integer;
  Pad: Integer;
begin
  Pad := Byte(fMultiplayer and not fReplay) * 2 +
         Byte(Image_MessageLog.Visible);
  for I := 0 to MAX_VISIBLE_MSGS do
    Image_Message[I].Top := Panel_Main.Height - 48 - (I + Pad) * 48;
end;


procedure TKMGamePlayInterface.Menu_Update;
begin
  if fGameApp.GameSettings.MusicOff then
    Label_Menu_Track.Caption := '-'
  else
    Label_Menu_Track.Caption := fGameApp.MusicLib.GetTrackTitle;

  Label_GameTime.Caption := Format(gResTexts[TX_GAME_TIME], [TimeToString(fGame.MissionTime)]);

  Label_Menu_Track.Enabled      := not fGameApp.GameSettings.MusicOff;
  Button_Menu_TrackUp.Enabled   := not fGameApp.GameSettings.MusicOff;
  Button_Menu_TrackDown.Enabled := not fGameApp.GameSettings.MusicOff;
end;


//Resize stats page in a way to display data in more readable form
//Try to keep items in corresponding pairs and stack them when dont fit otherwise
procedure TKMGamePlayInterface.Stats_Resize;
const PAD_X = 4; PAD_Y = 4;
var
  Rows: Integer;
  I, K: Integer;
  OffX, NextWidth: Integer;
  NeedToCompact: Boolean;
begin
  //How many rows could fit
  Rows := Panel_Stats.Height div (Panel_StatBlock[0].Height + PAD_Y);


  //Adjoin rows till they fit
  K := 0;
  OffX := 0;
  for I := 0 to High(StatPlan) do
  begin
    Panel_StatBlock[I].Left := OffX;
    Panel_StatBlock[I].Top := K * (Panel_StatBlock[0].Height + PAD_Y);

    Inc(OffX, PAD_X + Panel_StatBlock[I].Width);

    //Return caret
    if I <> High(StatPlan) then
    begin
      NeedToCompact := (Length(StatPlan) - I) > (Rows - K);
      NextWidth := Panel_StatBlock[I].Width + PAD_X;
      if not NeedToCompact or (OffX + NextWidth > TB_WIDTH) then
      begin
        OffX := 0;
        Inc(K);
      end;
    end;
  end;
end;


procedure TKMGamePlayInterface.Stats_Update;
var
  HT: THouseType;
  UT: TUnitType;
  Tmp, Tmp2: Integer;
  I,K: Integer;
begin
  //Update display values
  for I := 0 to High(StatPlan) do
  for K := Low(StatPlan[I].HouseType) to High(StatPlan[I].HouseType) do
  if StatPlan[I].HouseType[K] <> ht_None then
  begin
    HT := StatPlan[I].HouseType[K];
    Tmp := gPlayers[MySpectator.PlayerIndex].Stats.GetHouseQty(HT);
    Tmp2 := gPlayers[MySpectator.PlayerIndex].Stats.GetHouseWip(HT);
    Stat_HouseQty[HT].Caption := IfThen(Tmp  = 0, '-', IntToStr(Tmp));
    Stat_HouseWip[HT].Caption := IfThen(Tmp2 = 0, '', '+' + IntToStr(Tmp2));
    if gPlayers[MySpectator.PlayerIndex].Stats.GetCanBuild(HT) or (Tmp > 0) then
    begin
      Stat_HousePic[HT].TexID := fResource.HouseDat[HT].GUIIcon;
      Stat_HousePic[HT].Hint := fResource.HouseDat[HT].HouseName;
    end
    else
    begin
      Stat_HousePic[HT].TexID := 41;
      Stat_HousePic[HT].Hint := gResTexts[TX_HOUSE_NOT_AVAIABLE]; //Building not available
    end;
  end;

  for UT := CITIZEN_MIN to CITIZEN_MAX do
  begin
    Tmp := gPlayers[MySpectator.PlayerIndex].Stats.GetUnitQty(UT);
    Tmp2 := 0;//fPlayers[MySpectator.PlayerIndex].Stats.GetUnitWip(UT);
    Stat_UnitQty[UT].Caption := IfThen(Tmp  = 0, '-', IntToStr(Tmp));
    Stat_UnitWip[UT].Caption := IfThen(Tmp2 = 0, '', '+' + IntToStr(Tmp2));
    Stat_UnitPic[UT].Hint := fResource.UnitDat[UT].GUIName;
    Stat_UnitPic[UT].FlagColor := gPlayers[MySpectator.PlayerIndex].FlagColor;
  end;
end;


procedure TKMGamePlayInterface.Beacon_Cancel;
begin
  fPlacingBeacon := False; //Right click cancels it
  MinimapView.ClickableOnce := False;
  if fResource.Cursors.Cursor = kmc_Beacon then
    fResource.Cursors.Cursor := kmc_Default;
end;


procedure TKMGamePlayInterface.Army_ActivateControls(aGroup: TKMUnitGroup);
var AcceptOrders: Boolean;
begin
  AcceptOrders := aGroup.CanTakeOrders and not fReplay and not HasLostMPGame;

  //Button_Army_GoTo.Enabled    := AcceptOrders;
  Button_Army_Stop.Enabled    := AcceptOrders;
  //Button_Army_Attack.Enabled  := AcceptOrders;
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
  Button_Main[tbBuild].Enabled := not aTactic and not fReplay and not HasLostMPGame;
  Button_Main[tbRatio].Enabled := not aTactic and not fReplay and not HasLostMPGame;
  Button_Main[tbStats].Enabled := not aTactic;

  //No loading during multiplayer games
  Button_Menu_Load.Enabled := not fMultiplayer and not fReplay;
  Button_Menu_Save.Enabled := not fReplay;
  Button_Menu_Quit.Enabled := not fReplay;

  //Toggle gameplay options
  CheckBox_Settings_Autosave.Enabled := not fReplay;

  //Chat and Allies setup should be accessible only in Multiplayer
  Image_MPChat.Visible       := fMultiplayer and not fReplay;
  Label_MPChatUnread.Visible := fMultiplayer and not fReplay;
  Image_MPAllies.Visible     := fMultiplayer and not fReplay;

  //Message stack is visible in Replay as it shows which messages player got
  //and does not affect replay consistency

  Panel_ReplayCtrl.Visible := fReplay;
  if fReplay then
  begin
    Dropbox_ReplayFOW.Clear;
    Dropbox_ReplayFOW.Add(gResTexts[TX_REPLAY_SHOW_ALL], -1);
    for I := 0 to gPlayers.Count - 1 do
    if gPlayers[I].Enabled and (gPlayers[I].PlayerType = pt_Human) then
        Dropbox_ReplayFOW.Add('[$' + IntToHex(FlagColorToTextColor(gPlayers[I].FlagColor) and $00FFFFFF, 6) + ']' + gPlayers[I].GetFormattedPlayerName, I);
    Dropbox_ReplayFOW.ItemIndex := 0;
  end;
end;


procedure TKMGamePlayInterface.ShowClock(aSpeed: Single);
begin
  Image_Clock.Visible := aSpeed <> 1;
  Label_Clock.Visible := aSpeed <> 1;
  Label_ClockSpeedup.Visible := aSpeed <> 1;
  Label_ClockSpeedup.Caption := 'x' + FloatToStr(aSpeed);

  //With slow GPUs it will keep old values till next frame, that can take some seconds
  //Thats why we refresh Clock.Caption here
  if aSpeed <> 1 then
    Label_Clock.Caption := TimeToString(fGame.MissionTime);
end;


procedure TKMGamePlayInterface.SetPause(aValue:boolean);
begin
  ReleaseDirectionSelector; //Don't restrict cursor movement to direction selection while paused
  fGame.Viewport.ReleaseScrollKeys;
  fGame.IsPaused := aValue;
  Panel_Pause.Visible := aValue;
end;


procedure TKMGamePlayInterface.ShowPlayMore(DoShow:boolean; Msg:TGameResultMsg);
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
    else if DoShow then Assert(false,'Wrong message in ShowPlayMore'); //Can become hidden with any message
  end;
  Panel_PlayMore.Visible := DoShow;
end;


procedure TKMGamePlayInterface.ShowMPPlayMore(Msg:TGameResultMsg);
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
                    //Refresh it so that menu buttons become disabled
                    SetMenuState(fGame.MissionMode = mm_Tactic);
                    //Close e.g. the build menu if it was open
                    SwitchPage(Button_Back);

                    Label_MPPlayMore.Caption := gResTexts[TX_GAMEPLAY_LOST];
                    Button_MPPlayMore.Caption := gResTexts[TX_GAMEPLAY_DEFEAT_CONTINUEWATCHING];
                    Button_MPPlayQuit.Caption := gResTexts[TX_GAMEPLAY_DEFEAT];
                  end;
    else Assert(false,'Wrong message in ShowMPPlayMore');
  end;
  Panel_MPPlayMore.Visible := true;
end;


procedure TKMGamePlayInterface.PlayMoreClick(Sender:TObject);
begin
  Panel_PlayMore.Hide; //Hide anyways

  if Sender = Button_PlayQuit then
    case PlayMoreMsg of
      gr_Win:       fGameApp.Stop(gr_Win);
      gr_Defeat:    fGameApp.Stop(gr_Defeat);
      gr_ReplayEnd: fGameApp.Stop(gr_ReplayEnd);
    end
  else //GameStop has Destroyed our Sender by now
  if Sender = Button_PlayMore then
    case PlayMoreMsg of
      gr_Win:       begin fGame.GameHold(false, gr_Win); end;
      gr_Defeat:    begin fGame.GameHold(false, gr_Defeat); end;
      gr_ReplayEnd: begin fGame.SkipReplayEndCheck := true; fGame.GameHold(false, gr_ReplayEnd); end;
    end;
end;


procedure TKMGamePlayInterface.MPPlayMoreClick(Sender:TObject);
begin
  Panel_MPPlayMore.Hide;

  if Sender = Button_MPPlayQuit then
    case PlayMoreMsg of
      gr_Win:       fGameApp.Stop(gr_Win);
      gr_Defeat:    fGameApp.Stop(gr_Defeat);
      gr_ReplayEnd: fGameApp.Stop(gr_ReplayEnd);
    end
  //If they click continue no other action is necessary, the game is still running
end;


procedure TKMGamePlayInterface.ShowNetworkLag(DoShow:boolean; aPlayers:TStringList; IsHost:boolean);
var I: Integer; S:String;
begin
  if DoShow then ReleaseDirectionSelector;
  if not DoShow then //Reset the confirm when we hide this screen so it's not on confirm when it reshows
  begin
    Panel_NetWaitConfirm.Hide;
    Panel_NetWaitButtons.Show;
  end;
  if fGame.Networking.IsReconnecting then
  begin
    S := gResTexts[TX_MULTIPLAYER_ATTEMPT_RECONNECTING];
    Button_NetDropPlayers.Visible := false;
    fNetWaitDropPlayersDelayStarted := 0;
    Label_NetDropPlayersDelay.Caption := '';
  end
  else
  begin
    S := gResTexts[TX_MULTIPLAYER_WAITING]+' ';
    for i:=0 to aPlayers.Count-1 do
      S := S + aPlayers.Strings[i] + IfThen(i<>aPlayers.Count-1, ', ');

    Button_NetDropPlayers.Visible := IsHost;

    if not DoShow then
      fNetWaitDropPlayersDelayStarted := 0
    else
      if fNetWaitDropPlayersDelayStarted = 0 then
      begin
        Label_NetDropPlayersDelay.Caption := '';
        fNetWaitDropPlayersDelayStarted := TimeGet; //Initialise it
        Button_NetDropPlayers.Disable; //Must wait the minimum time before enabling it
      end;
  end;

  Label_NetWait.Caption := S;
  Panel_NetWait.Visible := DoShow;
end;


procedure TKMGamePlayInterface.SetScriptedOverlay(aText: string);
begin
  Label_ScriptedOverlay.Caption := aText;
end;


procedure TKMGamePlayInterface.AppendScriptedOverlay(aText: string);
begin
  Label_ScriptedOverlay.Caption := Label_ScriptedOverlay.Caption + aText;
end;


procedure TKMGamePlayInterface.NetWaitClick(Sender:TObject);
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
      fGame.GameDropWaitingPlayers else
    if Button_NetConfirmYes.Caption = gResTexts[TX_GAMEPLAY_QUIT_TO_MENU] then
      fGameApp.Stop(gr_Cancel);
  end
  else Assert(false, 'Wrong Sender in NetWaitClick');
end;


procedure TKMGamePlayInterface.DirectionCursorShow(X,Y: Integer; Dir:TKMDirection);
begin
  Image_DirectionCursor.Visible := True;
  Image_DirectionCursor.Left    := X + fResource.Cursors.CursorOffset(Dir).X;
  Image_DirectionCursor.Top     := Y + fResource.Cursors.CursorOffset(Dir).Y;
  Image_DirectionCursor.TexID   := fResource.Cursors.CursorTexID(Dir);
end;


procedure TKMGamePlayInterface.DirectionCursorHide;
begin
  Image_DirectionCursor.Visible := False;
end;


procedure TKMGamePlayInterface.ReleaseDirectionSelector;
begin
  if SelectingTroopDirection then
  begin
    //Reset the cursor position as it will have moved during direction selection
    SetCursorPos(fMain.ClientToScreen(SelectingDirPosition).X, fMain.ClientToScreen(SelectingDirPosition).Y);
    fMain.ApplyCursorRestriction; //Reset the cursor restrictions from selecting direction
    SelectingTroopDirection := False;
    fResource.Cursors.Cursor := kmc_Default; //Reset direction selection cursor when mouse released
    DirectionCursorHide;
  end;
end;


function TKMGamePlayInterface.HasLostMPGame:Boolean;
begin
  Result := fMultiplayer and (gPlayers[MySpectator.PlayerIndex].AI.WonOrLost = wol_Lost);
end;


procedure TKMGamePlayInterface.SetChatText(const aString: string);
begin
  Edit_ChatMsg.Text := aString;
  if aString <> '' then Chat_Show(nil);
end;


//Assign Object to a Key
//we use ID to avoid use of pointer counter
procedure TKMGamePlayInterface.Selection_Assign(aKey: Word; aObject: TObject);
var Key: Integer;
begin
  Key := aKey - Ord('0');
  if not InRange(Key, 0, 9) then Exit;

  if aObject is TKMUnit then
    fSelection[Key] := TKMUnit(aObject).UID
  else
  if aObject is TKMHouse then
    fSelection[Key] := TKMHouse(aObject).UID
  else
  if aObject is TKMUnitGroup then
    fSelection[Key] := TKMUnitGroup(aObject).UID
  else
    fSelection[Key] := -1;
end;


procedure TKMGamePlayInterface.Selection_Select(aKey: Word);
var Key: Integer; OldSelected: TObject;
begin
  Key := aKey - Ord('0');
  if not InRange(Key, 0, 9) then Exit;

  if fSelection[Key] <> -1 then
  begin
    OldSelected := MySpectator.Selected;
    MySpectator.Selected := gPlayers.GetUnitByUID(fSelection[Key]);
    if MySpectator.Selected <> nil then
    begin
      if TKMUnit(MySpectator.Selected).IsDeadOrDying then
      begin
        MySpectator.Selected := nil; //Don't select dead/dying units
        Exit;
      end;
      if (OldSelected <> MySpectator.Selected) and not fReplay and not HasLostMPGame then
        gSoundPlayer.PlayCitizen(TKMUnit(MySpectator.Selected).UnitType, sp_Select);
      //Selecting a unit twice is the shortcut to center on that unit
      if OldSelected = MySpectator.Selected then
        fGame.Viewport.Position := TKMUnit(MySpectator.Selected).PositionF;
    end
    else
    begin
      MySpectator.Selected := gPlayers.GetHouseByUID(fSelection[Key]);
      if MySpectator.Selected <> nil then
      begin
        if TKMHouse(MySpectator.Selected).IsDestroyed then
        begin
          MySpectator.Selected := nil; //Don't select destroyed houses
          Exit;
        end;
        //Selecting a house twice is the shortcut to center on that house
        if OldSelected = MySpectator.Selected then
          fGame.Viewport.Position := KMPointF(TKMHouse(MySpectator.Selected).GetEntrance);
      end
      else
      begin
        MySpectator.Selected := gPlayers.GetGroupByUID(fSelection[Key]);
        if (MySpectator.Selected = nil) or TKMUnitGroup(MySpectator.Selected).IsDead then
        begin
          MySpectator.Selected := nil; //Don't select dead groups
          Exit;
        end;
        TKMUnitGroup(MySpectator.Selected).SelectFlagBearer;
        if (OldSelected <> MySpectator.Selected) and not fReplay and not HasLostMPGame then
          gSoundPlayer.PlayWarrior(TKMUnitGroup(MySpectator.Selected).SelectedUnit.UnitType, sp_Select);
        //Selecting a group twice is the shortcut to center on that group
        if OldSelected = MySpectator.Selected then
          fGame.Viewport.Position := TKMUnitGroup(MySpectator.Selected).SelectedUnit.PositionF;
      end;
    end;

  end
  else
    MySpectator.Selected := nil;
end;


procedure TKMGamePlayInterface.SetChatMessages(const aString: string);
begin
  Memo_ChatText.Text := aString;
  Memo_ChatText.ScrollToBottom;
end;


procedure TKMGamePlayInterface.ChatMessage(const aData: UnicodeString);
begin
  Memo_ChatText.Add(aData);

  if not Panel_Chat.Visible then
    Label_MPChatUnread.Caption := IntToStr(StrToIntDef(Label_MPChatUnread.Caption, 0) + 1); //New message
end;


procedure TKMGamePlayInterface.WarriorCommanderDied(DeadID, NewID: Cardinal);
var I: Integer;
begin
  for I := 0 to 9 do
    if fSelection[I] = DeadID then
      fSelection[I] := NewID; //If the commander dies select the new commander
end;


procedure TKMGamePlayInterface.AlliesOnPlayerSetup(Sender: TObject);
var
  I, LocaleID: Integer;
begin
  for I := 0 to fGame.Networking.NetPlayers.Count - 1 do
  begin
    //Show players locale flag
    if fGame.Networking.NetPlayers[I+1].IsComputer then
      Image_AlliesFlag[I].TexID := 62 //PC icon
    else
    begin
      LocaleID := gResLocales.IndexByCode(fGame.Networking.NetPlayers[I+1].LangCode);
      if LocaleID <> -1 then
        Image_AlliesFlag[I].TexID := gResLocales[LocaleID].FlagSpriteID
      else
        Image_AlliesFlag[I].TexID := 0;
    end;

    Label_AlliesPlayer[I].Caption := fGame.Networking.NetPlayers[I+1].Nikname;
    Label_AlliesPlayer[I].FontColor := gPlayers[fGame.Networking.NetPlayers[I+1].StartLocation - 1].FlagColor;
    DropBox_AlliesTeam[I].ItemIndex := fGame.Networking.NetPlayers[I+1].Team;
    //Strikethrough for disconnected players
    Image_AlliesFlag[I].Enabled := not fGame.Networking.NetPlayers[I+1].Dropped;
    Label_AlliesPlayer[I].Strikethrough := fGame.Networking.NetPlayers[I+1].Dropped;
    Label_AlliesTeam[I].Strikethrough := fGame.Networking.NetPlayers[I+1].Dropped;
    Label_AlliesPing[I].Strikethrough := fGame.Networking.NetPlayers[I+1].Dropped;
    if fGame.Networking.NetPlayers[I+1].Team = 0 then
      Label_AlliesTeam[I].Caption := '-'
    else
      Label_AlliesTeam[I].Caption := IntToStr(fGame.Networking.NetPlayers[I+1].Team);
    DropBox_AlliesTeam[I].Enabled := (I+1 = fGame.Networking.MyIndex); //Our index
    DropBox_AlliesTeam[I].Hide; //Use label for demos until we fix exploits
  end;

  for I := fGame.Networking.NetPlayers.Count to MAX_PLAYERS - 1 do
  begin
    Label_AlliesPlayer[I].Hide;
    DropBox_AlliesTeam[I].Hide;
    Label_AlliesTeam[I].Hide;
  end;
end;


procedure TKMGamePlayInterface.AlliesOnPingInfo(Sender: TObject);
var I: Integer;
begin
  for I := 0 to MAX_PLAYERS - 1 do
    if (I < fGame.Networking.NetPlayers.Count) and (fGame.Networking.NetPlayers[I+1].IsHuman) then
    begin
      Label_AlliesPing[I].Caption := Format('[$%.6x]', [GetPingColor(fGame.Networking.NetPlayers[I+1].GetInstantPing) and $FFFFFF])+
                                     IntToStr(fGame.Networking.NetPlayers[I+1].GetInstantPing)+'[] / '+
                                     Format('[$%.6x]', [GetFPSColor(fGame.Networking.NetPlayers[I+1].FPS) and $FFFFFF])+
                                     IntToStr(fGame.Networking.NetPlayers[I+1].FPS)+'[]';
    end
    else
      Label_AlliesPing[I].Caption := '';
end;


procedure TKMGamePlayInterface.AlliesTeamChange(Sender: TObject);
var I: Integer;
begin
  for I := 0 to MAX_PLAYERS - 1 do
    if (Sender = DropBox_AlliesTeam[I]) and DropBox_AlliesTeam[I].Enabled then
      fGame.GameInputProcess.CmdGame(gic_GameTeamChange, I+1, DropBox_AlliesTeam[I].ItemIndex);
end;


procedure TKMGamePlayInterface.KeyDown(Key: Word; Shift: TShiftState);
var Rect: TKMRect;
begin
  if fGame.IsPaused and not fReplay then Exit;

  if fMyControls.KeyDown(Key, Shift) then
  begin
    fGame.Viewport.ReleaseScrollKeys; //Release the arrow keys when you open a window with an edit to stop them becoming stuck
    Exit;
  end;

  case Key of
    VK_LEFT:  fGame.Viewport.ScrollKeyLeft  := True;
    VK_RIGHT: fGame.Viewport.ScrollKeyRight := True;
    VK_UP:    fGame.Viewport.ScrollKeyUp    := True;
    VK_DOWN:  fGame.Viewport.ScrollKeyDown  := True;
    //As we don't have names for teams in SP we only allow showing team names in MP or MP replays
    Ord(SC_SHOW_TEAMS): if fMultiplayer or (fGame.GameMode = gmReplayMulti) then //Only MP replays
    begin
      fGame.ShowTeamNames := True;
      //Update it immediately so there's no 300ms lag after pressing the key
      fTeamNames.Clear;
      Rect := fGame.Viewport.GetMinimapClip;
      gPlayers.GetUnitsInRect(Rect, fTeamNames);
    end;
  end;
end;


//Note: we deliberately don't pass any Keys to MyControls when game is not running
//thats why MyControls.KeyUp is only in gsRunning clause
//Ignore all keys if game is on 'Pause'
procedure TKMGamePlayInterface.KeyUp(Key: Word; Shift: TShiftState);
begin
  if fGame.IsPaused and not fMultiplayer and not fReplay then
  begin
    if Key = Ord(SC_PAUSE) then
      SetPause(False);
    Exit;
  end;

  if fMyControls.KeyUp(Key, Shift) then Exit;

  if fReplay and (Key = Ord(SC_PAUSE)) then
  begin
    if Button_ReplayPause.Enabled then
      ReplayClick(Button_ReplayPause)
    else if Button_ReplayResume.Enabled then
      ReplayClick(Button_ReplayResume);
  end;

  case Key of
    //Scrolling
    VK_LEFT:  fGame.Viewport.ScrollKeyLeft  := False;
    VK_RIGHT: fGame.Viewport.ScrollKeyRight := False;
    VK_UP:    fGame.Viewport.ScrollKeyUp    := False;
    VK_DOWN:  fGame.Viewport.ScrollKeyDown  := False;
    VK_BACK:  fGame.Viewport.ResetZoom;

    Ord(SC_SHOW_TEAMS):  fGame.ShowTeamNames := False;
  end;

  if not fGame.IsMultiplayer or MULTIPLAYER_SPEEDUP then
  case Key of
    //Game speed/pause: Not available in multiplayer mode
    VK_F5:    fGame.SetGameSpeed(1, False);
    VK_F6:    fGame.SetGameSpeed(fGameApp.GameSettings.SpeedMedium, True);
    VK_F7:    fGame.SetGameSpeed(fGameApp.GameSettings.SpeedFast, True);
    VK_F8:    fGame.SetGameSpeed(fGameApp.GameSettings.SpeedVeryFast, True);
  end;

  //All the following keys don't work in Replay, because they alter game state
  //which is nonsense
  //thus the easy way to make that is to exit now
  if fReplay then Exit;

  case Key of
    //Messages
    VK_SPACE:             //In KaM spacebar centers you on the message
                          //Button_MessageGoTo.Click
                          ;
    VK_DELETE:            Button_MessageDelete.Click;
    VK_RETURN:            //Enter is the shortcut to bring up chat in multiplayer
                          if fMultiplayer and not Panel_Chat.Visible then
                            Chat_Show(Self);
    VK_ESCAPE:            //'or' allows us to go through Clicks one by one
                          if Button_Army_Join_Cancel.Click
                            or Image_MessageClose.Click
                            or Image_ChatClose.Click
                            or Image_AlliesClose.Click
                            or Image_MessageLogClose.Click
                            or Button_Back.Click then ;
    //Menu shortcuts
    SC_MENU_BUILD:  Button_Main[tbBuild].Click;
    SC_MENU_RATIO:  Button_Main[tbRatio].Click;
    SC_MENU_STATS:  Button_Main[tbStats].Click;
    SC_MENU_MENU:   Button_Main[tbMenu].Click;

    Ord(SC_SELECT_LOW)..Ord(SC_SELECT_HIGH):
                    if (ssCtrl in Shift) then
                      Selection_Assign(Key, MySpectator.Selected)
                    else
                      Selection_Select(Key);

    // Army shortcuts from KaM
    Ord(SC_ARMY_HALT):   if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_Stop.Click;
    Ord(SC_ARMY_LINK):   if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_Join.Click;
    Ord(SC_ARMY_SPLIT):  if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_Split.Click;

    //General function keys
    Ord(SC_PAUSE):  if not fMultiplayer then SetPause(True); //Display pause overlay
    Ord(SC_BEACON): if not SelectingTroopDirection then
                    begin
                      fPlacingBeacon := True;
                      MinimapView.ClickableOnce := True;
                      fResource.Cursors.Cursor := kmc_Beacon;
                    end;
  end;

  {Temporary cheat codes}
  if DEBUG_CHEATS and (MULTIPLAYER_CHEATS or not fMultiplayer) then
  case Key of
    Ord(SC_DEBUG_REVEALMAP): fGame.GameInputProcess.CmdTemp(gic_TempRevealMap);
    Ord(SC_DEBUG_VICTORY):   begin fGame.PlayerVictory(MySpectator.PlayerIndex); Exit; end;
    Ord(SC_DEBUG_DEFEAT):    begin fGame.PlayerDefeat (MySpectator.PlayerIndex); Exit; end;
    Ord(SC_DEBUG_ADDSCOUT):  fGame.GameInputProcess.CmdTemp(gic_TempAddScout, GameCursor.Cell);
  end;
end;


//1. Process Controls
//2. Show SelectingTroopDirection
procedure TKMGamePlayInterface.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  Group: TKMUnitGroup;
  Obj: TObject;
  canWalkTo: Boolean;
  MyRect: TRect;
begin
  fMyControls.MouseDown(X, Y, Shift, Button);

  if (fGame.IsPaused and not fReplay) or (fMyControls.CtrlOver <> nil) then
    Exit;

  if (Button = mbMiddle) then
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

  if SelectingTroopDirection then
  begin
    fMain.ApplyCursorRestriction; //Reset the cursor restrictions from selecting direction
    SelectingTroopDirection := false;
    DirectionCursorHide;
  end;

  //See if we can show DirectionSelector
  if (Button = mbRight)
  and not fReplay
  and not HasLostMPGame
  and not fJoiningGroups
  and not fPlacingBeacon
  and (MySpectator.Selected is TKMUnitGroup) then
  begin
    Group := TKMUnitGroup(MySpectator.Selected);
    Obj := MySpectator.HitTestCursor;

    canWalkTo := True;

    //Group can walk to allies units place
    if Obj is TKMUnit then
      canWalkTo := (gPlayers.CheckAlliance(MySpectator.PlayerIndex, TKMUnit(Obj).Owner) = at_Ally);

    //Can't walk on to a house
    if Obj is TKMHouse then
      canWalkTo := False;

    if canWalkTo then
    begin
      if Group.CanWalkTo(GameCursor.Cell, 0) then
      begin
        SelectingTroopDirection := True; //MouseMove will take care of cursor changing
        //Restrict the cursor to inside the main panel so it does not get jammed when used near the edge of the window in windowed mode
        {$IFDEF MSWindows}
        MyRect := fMain.ClientRect;
        ClipCursor(@MyRect);
        {$ENDIF}
        //Now record it as Client XY
        SelectingDirPosition.X := X;
        SelectingDirPosition.Y := Y;
        SelectedDirection := dir_NA;
        DirectionCursorShow(X, Y, SelectedDirection);
        fResource.Cursors.Cursor := kmc_Invisible;
      end
      else
        gSoundPlayer.Play(sfx_CantPlace, GameCursor.Cell, False, 4);
    end;
  end;
end;


//1. Process Controls
//2. Perform SelectingTroopDirection if it is active
//3. Display various cursors depending on whats below (might be called often)
procedure TKMGamePlayInterface.MouseMove(Shift: TShiftState; X,Y: Integer);
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
    VP.X := fDragScrollingViewportPos.X + (fDragScrollingCursorPos.X - X) / (CELL_SIZE_PX * fGame.Viewport.Zoom);
    VP.Y := fDragScrollingViewportPos.Y + (fDragScrollingCursorPos.Y - Y) / (CELL_SIZE_PX * fGame.Viewport.Zoom);
    fGame.Viewport.Position := VP;
    Exit;
  end;

  fMyControls.MouseMove(X,Y,Shift);

  if fPlacingBeacon then
  begin
    //Beacons are a special case, the cursor should be shown over controls to (you can place it on the minimap)
    if fMyControls.CtrlOver = nil then
      fGame.UpdateGameCursor(X,Y,Shift); //Keep the game cursor up to date
    fResource.Cursors.Cursor := kmc_Beacon;
    Exit;
  end;

  if (fMyControls.CtrlOver is TKMDragger) or (fMyControls.CtrlDown is TKMDragger) then Exit;

  if (fMyControls.CtrlOver <> nil)
  and (fMyControls.CtrlOver <> Image_DirectionCursor)
  and not SelectingTroopDirection then
  begin
    //kmc_Edit and kmc_DragUp are handled by Controls.MouseMove (it will reset them when required)
    if not fGame.Viewport.Scrolling and not (fResource.Cursors.Cursor in [kmc_Edit,kmc_DragUp]) then
      fResource.Cursors.Cursor := kmc_Default;
    Exit;
  end
  else
    DisplayHint(nil); //Clear shown hint

  if fGame.IsPaused and not fReplay then Exit;

  if SelectingTroopDirection then
  begin
    DeltaX := SelectingDirPosition.X - X;
    DeltaY := SelectingDirPosition.Y - Y;
    DeltaDistanceSqr := Sqr(DeltaX)+Sqr(DeltaY);
    //Manually force the cursor to remain within a circle (+2 to avoid infinite loop due to rounding)
    if DeltaDistanceSqr > Sqr(DirCursorCircleRadius+2) then
    begin
      DeltaX := Round(DeltaX / Sqrt(DeltaDistanceSqr) * DirCursorCircleRadius);
      DeltaY := Round(DeltaY / Sqrt(DeltaDistanceSqr) * DirCursorCircleRadius);
      NewPoint := fMain.ClientToScreen(SelectingDirPosition);
      NewPoint.X := NewPoint.X - DeltaX;
      NewPoint.Y := NewPoint.Y - DeltaY;
      SetCursorPos(NewPoint.X, NewPoint.Y);
    end;

    //Compare cursor position and decide which direction it is
    SelectedDirection := KMGetCursorDirection(DeltaX, DeltaY);
    //Update the cursor based on this direction and negate the offset
    DirectionCursorShow(SelectingDirPosition.X, SelectingDirPosition.Y, SelectedDirection);
    fResource.Cursors.Cursor := kmc_Invisible; //Keep it invisible, just in case
    Exit;
  end;

  fGame.UpdateGameCursor(X,Y,Shift);

  if ssLeft in Shift then //Only allow placing of roads etc. with the left mouse button
  begin
    P := GameCursor.Cell; //Get cursor position tile-wise
    if gPlayers[MySpectator.PlayerIndex].FogOfWar.CheckTileRevelation(P.X, P.Y) > 0 then
    case GameCursor.Mode of
      cmRoad:  if gPlayers[MySpectator.PlayerIndex].CanAddFakeFieldPlan(P, ft_Road) and not KMSamePoint(LastDragPoint, P) then
                begin
                  fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Road);
                  LastDragPoint := GameCursor.Cell;
                end;
      cmField: if gPlayers[MySpectator.PlayerIndex].CanAddFakeFieldPlan(P, ft_Corn) and not KMSamePoint(LastDragPoint, P) then
                begin
                  fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Corn);
                  LastDragPoint := GameCursor.Cell;
                end;
      cmWine:  if gPlayers[MySpectator.PlayerIndex].CanAddFakeFieldPlan(P, ft_Wine) and not KMSamePoint(LastDragPoint, P) then
                begin
                  fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Wine);
                  LastDragPoint := GameCursor.Cell;
                end;
      cmErase: if not KMSamePoint(LastDragPoint, P) then
                begin
                  if gPlayers[MySpectator.PlayerIndex].BuildList.HousePlanList.HasPlan(P) then
                  begin
                    fGame.GameInputProcess.CmdBuild(gic_BuildRemoveHousePlan, P);
                    LastDragPoint := GameCursor.Cell;
                  end
                  else
                    if (gPlayers[MySpectator.PlayerIndex].BuildList.FieldworksList.HasFakeField(P) <> ft_None) then
                    begin
                      fGame.GameInputProcess.CmdBuild(gic_BuildRemoveFieldPlan, P); //Remove any plans
                      LastDragPoint := GameCursor.Cell;
                    end;
                end;
    end;
  end;

  if GameCursor.Mode <> cmNone then
  begin
    //Use the default cursor while placing roads, don't become stuck on c_Info or others
    if not fGame.Viewport.Scrolling then
      fResource.Cursors.Cursor := kmc_Default;
    Exit;
  end;

  Obj := MySpectator.HitTestCursor;

  if fJoiningGroups and (MySpectator.Selected is TKMUnitGroup) then
  begin
    Group := TKMUnitGroup(MySpectator.Selected);
    if (Obj <> nil)
    and (Obj is TKMUnitWarrior)
    and (TKMUnitWarrior(Obj).Owner = MySpectator.PlayerIndex)
    and not Group.HasMember(TKMUnitWarrior(Obj))
    and (UnitGroups[TKMUnitWarrior(Obj).UnitType] = Group.GroupType) then
      fResource.Cursors.Cursor := kmc_JoinYes
    else
      fResource.Cursors.Cursor := kmc_JoinNo;
    Exit;
  end;

  //Only own units can be selected
  if ((Obj is TKMUnit) and (TKMUnit(Obj).Owner = MySpectator.PlayerIndex))
  or ((Obj is TKMHouse) and (TKMHouse(Obj).Owner = MySpectator.PlayerIndex)) then
  begin
    fResource.Cursors.Cursor := kmc_Info;
    Exit;
  end;

  if (MySpectator.Selected is TKMUnitGroup)
  and not fReplay and not HasLostMPGame
  and (MySpectator.FogOfWar.CheckTileRevelation(GameCursor.Cell.X, GameCursor.Cell.Y) > 0) then
  begin
    if ((Obj is TKMUnit) and (gPlayers.CheckAlliance(MySpectator.PlayerIndex, TKMUnit(Obj).Owner) = at_Enemy))
    or ((Obj is TKMHouse) and (gPlayers.CheckAlliance(MySpectator.PlayerIndex, TKMHouse(Obj).Owner) = at_Enemy)) then
      fResource.Cursors.Cursor := kmc_Attack
    else
      if not fGame.Viewport.Scrolling then
        fResource.Cursors.Cursor := kmc_Default;
    Exit;
  end;

  if not fGame.Viewport.Scrolling then
    fResource.Cursors.Cursor := kmc_Default;
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
      fResource.Cursors.Cursor := kmc_Default; //Reset cursor
      fMain.ApplyCursorRestriction;
    end;
    Exit;
  end;

  if fPlacingBeacon and (Button = mbRight) then
  begin
    Beacon_Cancel;
    if fMyControls.CtrlOver = nil then Exit; //Don't move troops too
  end;

  if (fMyControls.CtrlOver <> nil)
  and (fMyControls.CtrlOver <> Image_DirectionCursor)
  and not SelectingTroopDirection then
  begin
    fMyControls.MouseUp(X,Y,Shift,Button);
    Exit;
  end;

  if fGame.IsPaused and not fReplay then Exit;

  P := GameCursor.Cell; //It's used in many places here

  case Button of
    mbLeft:
      begin
        //Process groups joining
        if fJoiningGroups and (MySpectator.Selected is TKMUnitGroup) then
        begin
          Group := TKMUnitGroup(MySpectator.Selected);
          Obj := MySpectator.HitTestCursor;

          if (Obj <> nil)
          and (Obj is TKMUnitWarrior)
          and (TKMUnitWarrior(Obj).Owner = MySpectator.PlayerIndex)
          and not Group.HasMember(TKMUnitWarrior(Obj))
          and (UnitGroups[TKMUnitWarrior(Obj).UnitType] = Group.GroupType) then
          begin
            Group2 := gPlayers[MySpectator.PlayerIndex].UnitGroups.GetGroupByMember(TKMUnitWarrior(Obj));
            //Warrior might not have a group yet if he's still walking out of the barracks
            if Group2 <> nil then
            begin
              gSoundPlayer.PlayWarrior(Group.UnitType, sp_Join); //In SP joining is instant, Group does not exist after that
              fGame.GameInputProcess.CmdArmy(gic_ArmyLink, Group, Group2);
              Army_HideJoinMenu(nil);
            end;
          end;
          Exit;
        end;

        if fPlacingBeacon then
        begin
          fGame.GameInputProcess.CmdGame(gic_GameAlertBeacon, GameCursor.Float, MySpectator.PlayerIndex);
          Beacon_Cancel;
          Exit;
        end;

        //Only allow placing of roads etc. with the left mouse button
        if MySpectator.FogOfWar.CheckTileRevelation(P.X, P.Y) = 0 then
        begin
          if GameCursor.Mode in [cmErase, cmRoad, cmField, cmWine, cmHouses] then
            //Can't place noise when clicking on unexplored areas
            gSoundPlayer.Play(sfx_CantPlace, P, False, 4);
        end
        else
          case GameCursor.Mode of
            cmNone:
              begin
                //Remember previous selection to play sound if it changes
                OldSelected := MySpectator.Selected;
                OldSelectedUnit := nil;

                if OldSelected is TKMUnitGroup then
                  OldSelectedUnit := TKMUnitGroup(MySpectator.Selected).SelectedUnit;

                MySpectator.UpdateSelect;

                //In a replay we want in-game statistics (and other things) to be shown for the owner of the last select object
                if fReplay then
                begin
                  if MySpectator.Selected is TKMHouse then MySpectator.PlayerIndex := TKMHouse(MySpectator.Selected).Owner;
                  if MySpectator.Selected is TKMUnit  then MySpectator.PlayerIndex := TKMUnit (MySpectator.Selected).Owner;
                end;

                if (MySpectator.Selected is TKMHouse) then
                begin
                  HidePages;
                  fGuiGameHouse.Show(TKMHouse(MySpectator.Selected), False);
                end;

                if (MySpectator.Selected is TKMUnit) then
                begin
                  ShowUnitInfo(TKMUnit(MySpectator.Selected));
                  if not fReplay and not HasLostMPGame
                  and (OldSelected <> MySpectator.Selected) then
                    gSoundPlayer.PlayCitizen(TKMUnit(MySpectator.Selected).UnitType, sp_Select);
                end;

                if (MySpectator.Selected is TKMUnitGroup) then
                begin
                  Group := TKMUnitGroup(MySpectator.Selected);
                  ShowGroupInfo(Group);
                  if not fReplay and not HasLostMPGame
                  and ((OldSelected <> Group) or (OldSelectedUnit <> Group.SelectedUnit)) then
                    gSoundPlayer.PlayWarrior(Group.SelectedUnit.UnitType, sp_Select);
                end;
              end;

            cmRoad:
              if KMSamePoint(LastDragPoint, KMPoint(0,0)) then fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Road);

            cmField:
              if KMSamePoint(LastDragPoint, KMPoint(0,0)) then fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Corn);

            cmWine:
              if KMSamePoint(LastDragPoint, KMPoint(0,0)) then fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Wine);

            cmHouses:
              if gPlayers[MySpectator.PlayerIndex].CanAddHousePlan(P, THouseType(GameCursor.Tag1)) then
              begin
                fGame.GameInputProcess.CmdBuild(gic_BuildHousePlan, P,
                  THouseType(GameCursor.Tag1));
                if not (ssShift in Shift) then Build_ButtonClick(Button_BuildRoad); //If shift pressed do not reset cursor(keep selected building)
              end
              else
                gSoundPlayer.Play(sfx_CantPlace, P, False, 4);
            cmErase:
              if KMSamePoint(LastDragPoint, KMPoint(0,0)) then
              begin
                H := gPlayers[MySpectator.PlayerIndex].HousesHitTest(P.X, P.Y);
                //Ask wherever player wants to destroy own house (don't ask about houses that are not started, they are removed below)
                if H <> nil then
                begin
                  MySpectator.Selected := H; //Select the house irregardless of unit below/above
                  HidePages;
                  fGuiGameHouse.Show(H, True);
                  gSoundPlayer.Play(sfx_Click);
                end
                else
                begin
                  //Now remove houses that are not started
                  if gPlayers[MySpectator.PlayerIndex].BuildList.HousePlanList.HasPlan(P) then
                    fGame.GameInputProcess.CmdBuild(gic_BuildRemoveHousePlan, P)
                  else
                    if gPlayers[MySpectator.PlayerIndex].BuildList.FieldworksList.HasFakeField(P) <> ft_None then
                      fGame.GameInputProcess.CmdBuild(gic_BuildRemoveFieldPlan, P) //Remove plans
                    else
                      gSoundPlayer.Play(sfx_CantPlace, P, False, 4); //Otherwise there is nothing to erase
                end;
              end;
          end
      end;
    mbRight:
      begin
        //Cancel build/join
        if Panel_Build.Visible then
          SwitchPage(Button_Back);
        if fJoiningGroups then
        begin
          Army_HideJoinMenu(nil);
          Exit; //Don't order troops too
        end;

        //Process warrior commands
        if not fReplay
        and not HasLostMPGame
        and not fJoiningGroups
        and not fPlacingBeacon
        and (MySpectator.Selected is TKMUnitGroup) then
        begin
          Group := TKMUnitGroup(MySpectator.Selected);

          //Attack or Walk
          if Group.CanTakeOrders and (Group.Owner = MySpectator.PlayerIndex) then
          begin
            //Try to Attack unit
            Obj := MySpectator.HitTestCursor;
            if (Obj is TKMUnit) and (gPlayers.CheckAlliance(MySpectator.PlayerIndex, TKMUnit(Obj).Owner) = at_Enemy) then
            begin
              fGame.GameInputProcess.CmdArmy(gic_ArmyAttackUnit, Group, TKMUnit(Obj));
              gSoundPlayer.PlayWarrior(Group.UnitType, sp_Attack);
            end
            else
            //If there's no unit - try to Attack house
            if (Obj is TKMHouse) and (gPlayers.CheckAlliance(MySpectator.PlayerIndex, TKMHouse(Obj).Owner) = at_Enemy) then
            begin
              fGame.GameInputProcess.CmdArmy(gic_ArmyAttackHouse, Group, TKMHouse(Obj));
              gSoundPlayer.PlayWarrior(Group.UnitType, sp_Attack);
            end
            else
            //Ensure down click was successful (could have been over a mountain, then dragged to a walkable location)
            if SelectingTroopDirection and Group.CanWalkTo(P, 0) then
            begin
              fGame.GameInputProcess.CmdArmy(gic_ArmyWalk, Group, P, SelectedDirection);
              gSoundPlayer.PlayWarrior(Group.UnitType, sp_Move);
            end;
          end;
        end;
        //Not selecting direction now (must do it at the end because SelectingTroopDirection is used for Walk above)
        ReleaseDirectionSelector;
      end;
  end;

  LastDragPoint := KMPoint(0,0);
end;


procedure TKMGamePlayInterface.Save(SaveStream: TKMemoryStream);
begin
  fGuiGameHouse.Save(SaveStream);

  SaveStream.WriteW(fLastSaveName);
  SaveStream.Write(fSelection, SizeOf(fSelection));
  fMessageList.Save(SaveStream);
  //Everything else (e.g. ShownUnit or AskDemolish) can't be seen in Save_menu anyways
end;


procedure TKMGamePlayInterface.Load(LoadStream: TKMemoryStream);
begin
  fGuiGameHouse.Load(LoadStream);

  LoadStream.ReadW(fLastSaveName);
  LoadStream.Read(fSelection, SizeOf(fSelection));
  fMessageList.Load(LoadStream);
  //Everything else (e.g. ShownUnit or AskDemolish) can't be seen in Save_menu anyways
  Message_UpdateStack;
  gLog.AddTime('Interface loaded');
end;


procedure TKMGamePlayInterface.SetMinimap;
begin
  MinimapView.SetMinimap(fGame.Minimap);
  MinimapView.SetViewport(fGame.Viewport);
end;


{Should update any items changed by game (resource counts, hp, etc..)}
{If it ever gets a bottleneck then some static Controls may be excluded from update}
procedure TKMGamePlayInterface.UpdateState(aTickCount: Cardinal);
var
  I: Integer;
  Rect: TKMRect;
begin
  //Update unit/house information
  if MySpectator.Selected is TKMUnitGroup then
    ShowGroupInfo(TKMUnitGroup(MySpectator.Selected))
  else
  if MySpectator.Selected is TKMUnit then
    ShowUnitInfo(TKMUnit(MySpectator.Selected), fAskDismiss)
  else
  begin
    fJoiningGroups := False;
    if MySpectator.Selected is TKMHouse then
    begin
      HidePages;
      fGuiGameHouse.Show(TKMHouse(MySpectator.Selected));
    end
    else
      if fGuiGameHouse.Visible then
        fGuiGameHouse.Hide;
      if Panel_Unit.Visible then
        SwitchPage(nil);
  end;

  //Update peacetime counter
  if fGame.GameOptions.Peacetime <> 0 then
    Label_PeacetimeRemaining.Caption := Format(gResTexts[TX_MP_PEACETIME_REMAINING],
                                               [TimeToString(fGame.GetPeacetimeRemaining)])
  else
    Label_PeacetimeRemaining.Caption := '';

  //Update replay counters
  if fReplay then
  begin
    //Replays can continue after end, keep the bar in 0..1 range
    PercentBar_Replay.Seam := Min(fGame.GameOptions.Peacetime * 600 / Max(fGame.GameInputProcess.GetLastTick,1), 1);
    PercentBar_Replay.Position := Min(fGame.GameTickCount / Max(fGame.GameInputProcess.GetLastTick,1), 1);
    Label_Replay.Caption := TimeToString(fGame.MissionTime) + ' / ' +
                            TimeToString(fGame.GameInputProcess.GetLastTick/24/60/60/10);
  end;

  //Update speedup clocks
  if Image_Clock.Visible then
  begin
    Image_Clock.TexID := ((Image_Clock.TexID - 556) + 1) mod 16 + 556;
    Label_Clock.Caption := TimeToString(fGame.MissionTime);
  end;

  //Keep on updating these menu pages as game data keeps on changing
  if Panel_Build.Visible then Build_Update;
  if Panel_Stats.Visible then Stats_Update;
  if Panel_Menu.Visible then Menu_Update;

  //Update message stack
  //Flash unread message display
  Label_MPChatUnread.Visible := fMultiplayer and (Label_MPChatUnread.Caption <> '') and not (aTickCount mod 10 < 5);
  Image_MPChat.Highlight := Panel_Chat.Visible or (Label_MPChatUnread.Visible and (Label_MPChatUnread.Caption <> ''));
  Image_MPAllies.Highlight := Panel_Allies.Visible;
  if not fReplay and not Image_MessageLog.Visible and (fMessageList.CountLog > 0) then
  begin
    Image_MessageLog.Show;
    MessageStack_UpdatePositions;
  end;
  Image_MessageLog.Highlight := not Panel_MessageLog.Visible and (fLastSyncedMessage <> fMessageList.CountLog) and not (aTickCount mod 10 < 5);

  if Panel_MessageLog.Visible then
    MessageLog_Update(False);

  //Update info on awaited players
  if Panel_NetWait.Visible then
  begin
    if fGame.Networking.IsReconnecting then
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

  //Display team names
  if aTickCount mod 3 = 0 then //Update once every 300ms, player won't notice
  begin
    fTeamNames.Clear;
    if fGame.ShowTeamNames then
    begin
      Rect := fGame.Viewport.GetMinimapClip;
      gPlayers.GetUnitsInRect(Rect, fTeamNames);
    end;
  end;

  UpdateDebugInfo;
  if fSaves <> nil then fSaves.UpdateState;
end;


procedure TKMGamePlayInterface.UpdateDebugInfo;
var
  I: Integer;
  S: string;
begin
  S := '';

  //Debug info
  if SHOW_SPRITE_COUNT then
    S := IntToStr(gPlayers.UnitCount) + ' units on map|' +
         IntToStr(fRenderPool.RenderList.Stat_Sprites) + '/' +
         IntToStr(fRenderPool.RenderList.Stat_Sprites2) + ' sprites/rendered|' +
         IntToStr(CtrlPaintCount) + ' controls rendered|';

  if SHOW_POINTER_COUNT then
    S := S + Format('Pointers: %d units, %d houses|', [gPlayers[MySpectator.PlayerIndex].Units.GetTotalPointers, gPlayers[MySpectator.PlayerIndex].Houses.GetTotalPointers]);

  if SHOW_CMDQUEUE_COUNT then
    S := S + IntToStr(fGame.GameInputProcess.Count) + ' commands stored|';

  if SHOW_NETWORK_DELAY and fMultiplayer then
    S := S + 'Network delay: ' + IntToStr(TGameInputProcess_Multi(fGame.GameInputProcess).GetNetworkDelay) + '|';

  if DISPLAY_SOUNDS then
    S := S + IntToStr(gSoundPlayer.ActiveCount) + ' sounds playing|';

  //Temporary inteface (by @Crow)
  if SHOW_ARMYEVALS then
    for I := 0 to gPlayers.Count - 1 do
    if I <> MySpectator.PlayerIndex then
      S := S + Format('Enemy %d: %f|', [I, RoundTo(gPlayers[MySpectator.PlayerIndex].ArmyEval.Evaluations[I].Power, -3)]);

  if SHOW_AI_WARE_BALANCE then
    S := S + gPlayers[MySpectator.PlayerIndex].AI.Mayor.BalanceText + '|';

  Label_DebugInfo.Caption := S;
end;


procedure TKMGamePlayInterface.Paint;
var
  I: Integer;
  U: TKMUnit;
  UnitLoc: TKMPointF;
  MapLoc: TKMPointF;
  ScreenLoc: TKMPointI;
begin
  if fGame.ShowTeamNames then
  begin
    Label_TeamName.Visible := True; //Only visible while we're using it, otherwise it shows up in other places
    for I := 0 to fTeamNames.Count - 1 do
    begin
      U := TKMUnit(fTeamNames[I]);
      if U.Visible and (MySpectator.FogOfWar.CheckRevelation(U.PositionF) > FOG_OF_WAR_MIN) then
      begin
        Label_TeamName.Caption := gPlayers[U.Owner].PlayerName;
        Label_TeamName.FontColor := FlagColorToTextColor(gPlayers[U.Owner].FlagColor);

        UnitLoc := U.PositionF;
        UnitLoc.X := UnitLoc.X - 0.5;
        UnitLoc.Y := UnitLoc.Y - 1;
        MapLoc := gTerrain.FlatToHeight(UnitLoc);
        ScreenLoc := fGame.Viewport.MapToScreen(MapLoc);

        if KMInRect(ScreenLoc, KMRect(0, 0, Panel_Main.Width, Panel_Main.Height)) then
        begin
          Label_TeamName.Left := ScreenLoc.X;
          Label_TeamName.Top := ScreenLoc.Y;
          Label_TeamName.Paint;
        end;
      end;
    end;
  end;
  Label_TeamName.Visible := False; //Only visible while we're using it, otherwise it shows up in other places

  inherited;
end;


end.
