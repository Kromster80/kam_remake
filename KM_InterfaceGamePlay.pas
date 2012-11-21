unit KM_InterfaceGamePlay;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  StrUtils, SysUtils, KromUtils, Math, Classes, Controls,
  KM_CommonTypes,
  KM_InterfaceDefaults, KM_Terrain, KM_Pics,
  KM_Controls, KM_Houses, KM_Units, KM_UnitGroups, KM_Units_Warrior, KM_Saves, KM_Defaults, KM_MessageStack, KM_CommonClasses, KM_Points;


const MAX_VISIBLE_MSGS = 32;

type
  TKMTabButtons = (tbBuild, tbRatio, tbStats, tbMenu);

  TKMGamePlayInterface = class (TKMUserInterface)
  private
    fMultiplayer: Boolean; //Multiplayer UI has slightly different layout
    fReplay: Boolean; //Replay UI has slightly different layout
    fSave_Selected: Integer; //Save selected from list (needed because of scanning)

    //Not saved
    LastDragPoint: TKMPoint; //Last mouse point that we drag placed/removed a road/field
    PrevHint: TObject;
    ShownMessage: Integer;
    PlayMoreMsg: TGameResultMsg; //Remember which message we are showing
    fJoiningGroups, fPlacingBeacon: Boolean;
    fAskDemolish, fAskDismiss: Boolean;
    fNetWaitDropPlayersDelayStarted: Cardinal;
    SelectedDirection: TKMDirection;
    SelectingTroopDirection: Boolean;
    SelectingDirPosition: TPoint;
    RatioTab: Byte; //Active resource distribution tab
    fSaves: TKMSavesCollection;
    fTeamNames: TList;
    Label_TeamName: TKMLabel;

    //Saved
    fLastSaveName: AnsiString; //The file name we last used to save this file (used as default in Save menu)
    LastSchoolUnit: Byte;  //Last unit that was selected in School, global for all schools player owns
    LastBarracksUnit: Byte; //Last unit that was selected in Barracks, global for all barracks player owns
    fMessageList: TKMMessageList;
    fSelection: array [0..9] of Integer;

    procedure Create_Controls_Page;
    procedure Create_Replay_Page;
    procedure Create_Allies_Page;
    procedure Create_Chat_Page;
    procedure Create_Message_Page;
    procedure Create_Pause_Page;
    procedure Create_PlayMore_Page;
    procedure Create_MPPlayMore_Page;
    procedure Create_NetWait_Page;
    procedure Create_SideStack;
    procedure Create_Build_Page;
    procedure Create_Ratios_Page;
    procedure Create_Stats_Page;
    procedure Create_Menu_Page;
    procedure Create_Save_Page;
    procedure Create_Load_Page;
    procedure Create_Settings_Page;
    procedure Create_Quit_Page;
    procedure Create_Unit_Page;
    procedure Create_House_Page;
    procedure Create_Market_Page;
    procedure Create_Store_Page;
    procedure Create_School_Page;
    procedure Create_Barracks_Page;
    procedure Create_Woodcutter_Page;

    procedure Beacon_Cancel;
    procedure Army_ActivateControls(aGroup: TKMUnitGroup);
    procedure Army_HideJoinMenu(Sender:TObject);
    procedure Army_Issue_Order(Sender:TObject);
    procedure Unit_Dismiss(Sender:TObject);
    procedure House_WoodcutterChange(Sender:TObject);
    procedure House_BarracksUnitChange(Sender:TObject; AButton:TMouseButton);
    procedure House_Demolish(Sender:TObject);
    procedure House_RepairToggle(Sender:TObject);
    procedure House_WareDeliveryToggle(Sender:TObject);
    procedure House_OrderClick(Sender:TObject; AButton:TMouseButton);
    procedure House_MarketFill(aMarket: TKMHouseMarket);
    procedure House_MarketOrderClick(Sender:TObject; AButton:TMouseButton);
    procedure House_MarketSelect(Sender:TObject; AButton:TMouseButton);
    procedure House_SchoolUnitChange(Sender:TObject; AButton:TMouseButton);
    procedure House_SchoolUnitRemove(Sender: TObject; AButton: TMouseButton);
    procedure House_StoreAcceptFlag(Sender:TObject);
    procedure House_StoreFill;
    procedure Menu_Settings_Fill;
    procedure Menu_Settings_Change(Sender:TObject);
    procedure Menu_QuitMission(Sender:TObject);
    procedure Menu_NextTrack(Sender:TObject);
    procedure Menu_PreviousTrack(Sender:TObject);
    procedure Chat_Click(Sender: TObject);
    procedure Chat_Show(Sender: TObject);
    procedure Allies_Click(Sender: TObject);
    procedure Allies_Show(Sender: TObject);
    procedure Message_Click(Sender: TObject);
    procedure Message_Close(Sender: TObject);
    procedure Message_Delete(Sender: TObject);
    procedure Message_Display(aIndex: Integer);
    procedure Message_GoTo(Sender: TObject);
    procedure Message_UpdateStack;
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
    procedure SwitchPage(Sender: TObject);
    procedure SwitchPage_Ratios(Sender: TObject);
    procedure RatiosChange(Sender: TObject);
    procedure DisplayHint(Sender: TObject);
    procedure PlayMoreClick(Sender:TObject);
    procedure MPPlayMoreClick(Sender:TObject);
    procedure NetWaitClick(Sender:TObject);
    procedure ReplayClick(Sender: TObject);
    procedure Build_ButtonClick(Sender: TObject);
    procedure Build_Fill(Sender:TObject);
    procedure Chat_Close(Sender: TObject);
    procedure Chat_Post(Sender:TObject; Key:word);
    procedure Chat_Resize(Sender: TObject; X,Y: Integer);
    procedure Allies_Close(Sender: TObject);
    procedure Stats_Fill(Sender:TObject);
    procedure Menu_Fill(Sender:TObject);
    procedure SetPause(aValue:boolean);
    procedure DirectionCursorShow(X,Y: Integer; Dir:TKMDirection);
    procedure DirectionCursorHide;
    function HasLostMPGame:Boolean;
  protected
    Panel_Main: TKMPanel;
      Sidebar_Top: TKMImage;
      Sidebar_Middle: TKMImage;
      MinimapView: TKMMinimapView;
      Label_DebugInfo, Label_Hint: TKMLabel;
      Bevel_HintBG: TKMBevel;

      Image_MPChat, Image_MPAllies: TKMImage; //Multiplayer buttons
      Label_MPChatUnread: TKMLabel;
      Image_Message: array[0..MAX_VISIBLE_MSGS]of TKMImage; //Queue of messages covers 32*48=1536px height
      Image_Clock:TKMImage; //Clock displayed when game speed is increased
      Label_Clock:TKMLabel;
      Label_ClockSpeedup:TKMLabel;
      Label_MenuTitle: TKMLabel; //Displays the title of the current menu to the right of return
      Image_DirectionCursor:TKMImage;

    Panel_Controls: TKMPanel;
      Button_Main: array [TKMTabButtons] of TKMButton; //4 common buttons + Return
      Button_Back: TKMButton;

    Panel_Replay:TKMPanel; //Bigger Panel to contain Shapes to block all interface below
    Panel_ReplayCtrl:TKMPanel; //Smaller Panel to contain replay controls
      PercentBar_Replay: TKMPercentBar;
      Label_Replay:TKMLabel;
      Button_ReplayRestart:TKMButton;
      Button_ReplayPause:TKMButton;
      Button_ReplayStep:TKMButton;
      Button_ReplayResume:TKMButton;
      Button_ReplayExit:TKMButton;
    Panel_Allies:TKMPanel;
      Label_PeacetimeRemaining: TKMLabel;
      Image_AlliesLang:array [0..MAX_PLAYERS-1] of TKMImage;
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
      CheckBox_SendToAllies: TKMCheckBox;
      Image_ChatClose: TKMImage;
    Panel_Message: TKMPanel;
      Image_Scroll: TKMImage;
      Label_MessageText: TKMLabel;
      Button_MessageGoTo: TKMButton;
      Button_MessageDelete: TKMButton;
      Button_MessageClose: TKMButton;
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
      Stat_HousePic,Stat_UnitPic:array[1..32]of TKMImage;
      Stat_HouseQty,Stat_HouseWip,Stat_UnitQty:array[1..32]of TKMLabel;

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
        List_Save: TKMListBox;
        Edit_Save: TKMEdit;
        Label_SaveExists: TKMLabel;
        CheckBox_SaveExists: TKMCheckBox;
        Button_Save: TKMButton;

      Panel_Load:TKMPanel;
        List_Load: TKMListBox;
        Label_Load_Description: TKMLabel;
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

    Panel_House:TKMPanel;
      Label_House:TKMLabel;
      Button_House_Goods,Button_House_Repair:TKMButton;
      Image_House_Logo,Image_House_Worker:TKMImage;
      HealthBar_House:TKMPercentBar;
      Label_HouseHealth:TKMLabel;

    Panel_House_Common:TKMPanel;
      Label_Common_Demand,Label_Common_Offer,Label_Common_Costs,
      Label_House_UnderConstruction,Label_House_Demolish:TKMLabel;
      Image_HouseConstructionWood, Image_HouseConstructionStone: TKMImage;
      Label_HouseConstructionWood, Label_HouseConstructionStone: TKMLabel;
      Button_House_DemolishYes,Button_House_DemolishNo:TKMButton;
      ResRow_Common_Resource:array[1..4]of TKMResourceRow; //4 bars is the maximum
      ResRow_Order:array[1..4]of TKMResourceOrderRow; //3 bars is the maximum
      ResRow_Costs:array[1..4]of TKMCostsRow; //3 bars is the maximum
    Panel_HouseMarket:TKMPanel;
      Button_Market:array[0..STORE_RES_COUNT-1]of TKMButtonFlat;
      Shape_Market_From, Shape_Market_To: TKMShape;
      Label_Market_In, Label_Market_Out: TKMLabel;
      Button_Market_In, Button_Market_Out: TKMButtonFlat;
      Button_Market_Add,Button_Market_Remove: TKMButton;
      Label_Market_FromAmount,Label_Market_ToAmount:TKMLabel;
    Panel_HouseStore:TKMPanel;
      Button_Store:array[1..STORE_RES_COUNT]of TKMButtonFlat;
      Image_Store_Accept:array[1..STORE_RES_COUNT]of TKMImage;
    Panel_House_School:TKMPanel;
      ResRow_School_Resource:TKMResourceRow;
      Button_School_UnitWIP:TKMButton;
      Button_School_UnitWIPBar:TKMPercentBar;
      Button_School_UnitPlan:array[1..5]of TKMButtonFlat;
      Label_School_Unit:TKMLabel;
      Image_School_Right,Image_School_Train,Image_School_Left:TKMImage;
      Button_School_Right,Button_School_Train,Button_School_Left:TKMButton;
    Panel_HouseBarracks:TKMPanel;
      Button_Barracks:array[1..BARRACKS_RES_COUNT]of TKMButtonFlat;
      Button_BarracksRecruit: TKMButtonFlat;
      Label_Barracks_Unit:TKMLabel;
      Image_Barracks_Right,Image_Barracks_Train,Image_Barracks_Left:TKMImage;
      Button_Barracks_Right,Button_Barracks_Train,Button_Barracks_Left:TKMButton;
    Panel_HouseWoodcutter:TKMPanel;
      Radio_Woodcutter:TKMRadioGroup;
      Button_Woodcutter:TKMButtonFlat;

  public
    constructor Create(aScreenX, aScreenY: Word; aMultiplayer, aReplay: Boolean); reintroduce;
    destructor Destroy; override;
    procedure ShowHouseInfo(Sender: TKMHouse; aAskDemolish: Boolean = False);
    procedure ShowUnitInfo(Sender: TKMUnit; aAskDismiss:boolean=false);
    procedure ShowGroupInfo(Sender: TKMUnitGroup);
    procedure MessageIssue(aKind: TKMMessageKind; aText: string); overload;
    procedure MessageIssue(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint); overload;
    procedure SetMenuState(aTactic: Boolean);
    procedure ClearOpenMenu;
    procedure SetMinimap;
    procedure ShowClock(aSpeed: Word);
    procedure ShowPlayMore(DoShow:boolean; Msg:TGameResultMsg);
    procedure ShowMPPlayMore(Msg:TGameResultMsg);
    procedure ShowNetworkLag(DoShow:boolean; aPlayers:TStringList; IsHost:boolean);
    property LastSaveName: AnsiString read fLastSaveName write fLastSaveName;
    procedure ReleaseDirectionSelector;
    procedure SetChatText(const aString: string);
    procedure SetChatMessages(const aString: string);
    procedure ChatMessage(const aData: string);
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
uses KM_Main, KM_GameInputProcess, KM_GameInputProcess_Multi, KM_AI,
  KM_PlayersCollection, KM_RenderPool, KM_TextLibrary, KM_Game,  KM_GameApp, KM_Utils, KM_Locales,
  KM_Sound, KM_Resource, KM_Log, KM_ResourceUnit, KM_ResourceCursors, KM_ResourceSprites;


const
  MESSAGE_AREA_HEIGHT = 173+17; //Image_ChatHead + Image_ChatBody
  MESSAGE_AREA_RESIZE_Y = 200; //How much can we resize it

  ResRatioCount = 4;
  ResRatioType:array[1..ResRatioCount] of TResourceType = (rt_Steel, rt_Coal, rt_Wood, rt_Corn);
  ResRatioHint:array[1..ResRatioCount] of word = (298, 300, 302, 304); //Distribution of rt_***
  ResRatioHouseCount:array[1..ResRatioCount] of byte = (2, 4, 2, 3);
  ResRatioHouse:array[1..ResRatioCount, 1..4] of THouseType = (
      (ht_WeaponSmithy,   ht_ArmorSmithy,     ht_None,          ht_None),
      (ht_IronSmithy,     ht_Metallurgists,   ht_WeaponSmithy,  ht_ArmorSmithy),
      (ht_ArmorWorkshop,  ht_WeaponWorkshop,  ht_None,          ht_None),
      (ht_Mill,           ht_Swine,           ht_Stables,       ht_None));


{Switch between pages}
procedure TKMGamePlayInterface.SwitchPage_Ratios(Sender: TObject);
var I: Integer; HT:THouseType;
begin
  if (MyPlayer=nil) or (MyPlayer.Stats=nil) then Exit; //We need to be able to access these

  //Hide everything but the tab buttons
  for i:=1 to Panel_Ratios.ChildCount do
    if not (Panel_Ratios.Childs[i] is TKMButton) then
      Panel_Ratios.Childs[i].Hide;

  RatioTab := TKMButton(Sender).Tag;

  Image_RatioPic0.TexID := fResource.Resources[ResRatioType[RatioTab]].GUIIcon;//Show resource icon
  Label_RatioLab0.Caption := fResource.Resources[ResRatioType[RatioTab]].Title;
  Image_RatioPic0.Show;
  Label_RatioLab0.Show;

  for i:=1 to ResRatioHouseCount[RatioTab] do
  begin
    HT := ResRatioHouse[RatioTab, i];
    //Do not allow player to see blocked house (never able to build). Though house may be prebuilt and blocked
    if (not MyPlayer.Stats.HouseBlocked[HT]) or (MyPlayer.Stats.GetHouseQty(HT) > 0) then
    begin
      Image_RatioPic[i].TexID := fResource.HouseDat[HT].GUIIcon;
      TrackBar_RatioRat[i].Caption := fResource.HouseDat[HT].HouseName;
      TrackBar_RatioRat[i].Position := MyPlayer.Stats.Ratio[ResRatioType[RatioTab], HT];
      TrackBar_RatioRat[i].Enable;
    end else begin
      Image_RatioPic[i].TexID := 41; //Question mark
      TrackBar_RatioRat[i].Caption := fTextLibrary[TX_GAMEPLAY_NOT_AVAILABLE];
      TrackBar_RatioRat[i].Position := 0;
      TrackBar_RatioRat[i].Disable;
    end;

    Image_RatioPic[i].Show;
    TrackBar_RatioRat[i].Show;
  end;
end;


procedure TKMGamePlayInterface.RatiosChange(Sender: TObject);
var RT:TResourceType; HT:THouseType;
begin
  RT := ResRatioType[RatioTab];
  HT := ResRatioHouse[RatioTab, TKMTrackBar(Sender).Tag];

  fGame.GameInputProcess.CmdRatio(gic_RatioChange, RT, HT, TKMTrackBar(Sender).Position);
end;


procedure TKMGamePlayInterface.Menu_Save_ListChange(Sender: TObject);
begin
  if InRange(TKMListBox(Sender).ItemIndex, 0, fSaves.Count-1) then
  begin
    fSave_Selected := TKMListBox(Sender).ItemIndex;
    Edit_Save.Text := fSaves[List_Save.ItemIndex].FileName;
    CheckBox_SaveExists.Enabled := False;
    Label_SaveExists.Visible := False;
    CheckBox_SaveExists.Checked := False;
    Button_Save.Enabled := True;
  end;
end;


procedure TKMGamePlayInterface.Menu_Save_EditChange(Sender: TObject);
begin
  if (Sender <> fSaves) then
  begin
    List_Save.ItemIndex := -1;
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
  Button_Save.Enabled := CheckBox_SaveExists.Checked;
end;


procedure TKMGamePlayInterface.Menu_Save_RefreshList(Sender: TObject);
var I, OldTopIndex: Integer;
begin
  OldTopIndex := List_Save.TopIndex;
  List_Save.Clear;

  if fSaves.ScanFinished then
  begin
    if LastSaveName = '' then
      Edit_Save.Text := fGame.GameName
    else
      Edit_Save.Text := LastSaveName;
  end;

  if (Sender = fSaves) then
    Menu_Save_EditChange(fSaves)
  else
    Menu_Save_EditChange(nil);

  if (Sender = fSaves) then
    for I := 0 to fSaves.Count - 1 do
      List_Save.Add(fSaves[i].FileName);

  List_Save.ItemIndex := fSave_Selected;
  List_Save.TopIndex := OldTopIndex;
end;


procedure TKMGamePlayInterface.Menu_Save_Click(Sender: TObject);
var
  SaveName: string;
begin
  SaveName := Trim(Edit_Save.Text);
  LastSaveName := SaveName; //Do this before saving so it is included in the save
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
  Button_Load.Enabled := InRange(List_Load.ItemIndex, 0, fSaves.Count - 1)
                         and fSaves[List_Load.ItemIndex].IsValid;
  if InRange(List_Load.ItemIndex,0,fSaves.Count-1) then
  begin
    Label_Load_Description.Caption := fSaves[List_Load.ItemIndex].Info.GetTitleWithTime;
    fSave_Selected := List_Load.ItemIndex;
  end;
end;


procedure TKMGamePlayInterface.Menu_Load_Click(Sender: TObject);
begin
  if not InRange(List_Load.ItemIndex, 0, fSaves.Count - 1) then Exit;
  fSaves.TerminateScan; //Stop scan as it is no longer needed
  fGameApp.NewSingleSave(fSaves[List_Load.ItemIndex].FileName);
end;


procedure TKMGamePlayInterface.Menu_Load_RefreshList(Sender: TObject);
var I, OldTopIndex: Integer;
begin
  OldTopIndex := List_Load.TopIndex;
  List_Load.Clear;

  if (Sender = fSaves) then
  begin
    for i:=0 to fSaves.Count-1 do
      List_Load.Add(fSaves[i].FileName);
  end;

  List_Load.TopIndex := OldTopIndex;
  List_Load.ItemIndex := fSave_Selected;

  Menu_Load_ListClick(nil);
end;


{Switch between pages}
procedure TKMGamePlayInterface.SwitchPage(Sender: TObject);
var I: Integer; LastVisiblePage: TKMPanel;

  procedure Flip4MainButtons(ShowEm: Boolean);
  var T: TKMTabButtons;
  begin
    for T := Low(TKMTabButtons) to High(TKMTabButtons) do Button_Main[T].Visible := ShowEm;
    Button_Back.Visible := not ShowEm;
    Label_MenuTitle.Visible := not ShowEm;

  end;

begin
  fMyControls.CtrlFocus := nil; //Panels that require control focus should set it themselves

  if (Sender = Button_Main[tbBuild]) or (Sender = Button_Main[tbRatio])
  or (Sender = Button_Main[tbStats]) or (Sender = Button_Main[tbMenu])
  or (Sender = Button_Menu_Settings) or (Sender = Button_Menu_Quit) then
    fPlayers.Selected := nil;

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

  //Hide all existing pages
  for I := 1 to Panel_Controls.ChildCount do
    if (Panel_Controls.Childs[I] is TKMPanel) then
      Panel_Controls.Childs[I].Hide;

  //Hide all House sub-pages
  for I := 1 to Panel_House.ChildCount do
    if Panel_House.Childs[I] is TKMPanel then
      Panel_House.Childs[I].Hide;

  //If Sender is one of 4 main buttons, then open the page, hide the buttons and show Return button
  Flip4MainButtons(false);
  if Sender = Button_Main[tbBuild] then begin
    Build_Fill(nil);
    Panel_Build.Show;
    Label_MenuTitle.Caption := fTextLibrary[TX_MENU_TAB_BUILD];
    Build_ButtonClick(Button_BuildRoad);
  end else

  if Sender = Button_Main[tbRatio] then begin
    Panel_Ratios.Show;
    SwitchPage_Ratios(Button_Ratios[1]); //Open 1st tab
    Label_MenuTitle.Caption := fTextLibrary[TX_MENU_TAB_DISTRIBUTE];
  end else

  if Sender = Button_Main[tbStats] then begin
    Stats_Fill(nil);
    Panel_Stats.Show;
    Label_MenuTitle.Caption := fTextLibrary[TX_MENU_TAB_STATISTICS];
  end else

  if (Sender = Button_Main[tbMenu])
  or (Sender = Button_Quit_No)
  or ((Sender = Button_Back) and ((LastVisiblePage = Panel_Settings)
                               or (LastVisiblePage = Panel_Load)
                               or (LastVisiblePage = Panel_Save))) then begin
    Menu_Fill(Sender); //Make sure updating happens before it is shown
    Label_MenuTitle.Caption := fTextLibrary[TX_MENU_TAB_OPTIONS];
    Panel_Menu.Show;
  end else

  if Sender = Button_Menu_Save then begin
    fSave_Selected := -1;
    //Stop current now scan so it can't add a save after we clear the list
    fSaves.TerminateScan;
    Menu_Save_RefreshList(nil); //need to call it at last one time to setup GUI even if there are no saves
    //Initiate refresh and process each new save added
    fSaves.Refresh(Menu_Save_RefreshList, fMultiplayer);
    Panel_Save.Show;
    fMyControls.CtrlFocus := Edit_Save;
    Label_MenuTitle.Caption := fTextLibrary[TX_MENU_SAVE_GAME];
  end else

  if Sender = Button_Menu_Load then begin
    fSave_Selected := -1;
    //Stop current now scan so it can't add a save after we clear the list
    fSaves.TerminateScan;
    Menu_Load_RefreshList(nil); //need to call it at least one time to setup GUI even if there are no saves
    //Initiate refresh and process each new save added
    fSaves.Refresh(Menu_Load_RefreshList, fMultiplayer);
    Panel_Load.Show;
    Label_MenuTitle.Caption := fTextLibrary[TX_MENU_LOAD_GAME];
  end else

  if Sender = Button_Menu_Settings then begin
    Menu_Settings_Fill;
    Panel_Settings.Show;
    Label_MenuTitle.Caption := fTextLibrary[TX_MENU_SETTINGS];
  end else

  if Sender = Button_Menu_Quit then
    Panel_Quit.Show
  else //If Sender is anything else - then show all 4 buttons and hide Return button
    Flip4MainButtons(True);

  //Now process all other kinds of pages
  if (Sender = Panel_Unit) or (Sender = Panel_House)
  or (Sender = Panel_House_Common) or (Sender = Panel_House_School)
  or (Sender = Panel_HouseMarket) or (Sender = Panel_HouseBarracks)
  or (Sender = Panel_HouseStore) or (Sender = Panel_HouseWoodcutter) then
    TKMPanel(Sender).Show;

  //Place the cursor in the chatbox if it is open and nothing else has taken focus
  if (Panel_Chat.Visible) and (fMyControls.CtrlFocus = nil) then
    fMyControls.CtrlFocus := Edit_ChatMsg;
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
    Bevel_HintBG.Width := 8+fResource.ResourceFont.GetTextSize(Label_Hint.Caption, Label_Hint.Font).X;
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
  if not fTerrain.TileInMapCoords(Loc.X, Loc.Y) then Exit; //Must be inside map

  //Send move order, if applicable
  if (fPlayers.Selected is TKMUnitGroup) and not fJoiningGroups and not fPlacingBeacon and not fReplay and not HasLostMPGame then
  begin
    Group := TKMUnitGroup(fPlayers.Selected);
    if Group.CanWalkTo(Loc, 0) then
    begin
      fGame.GameInputProcess.CmdArmy(gic_ArmyWalk, Group, Loc, dir_NA);
      fSoundLib.PlayWarrior(Group.UnitType, sp_Move);
    end;
  end;
end;


procedure TKMGamePlayInterface.Minimap_Click(Sender: TObject; const X,Y:integer);
begin
  if not fPlacingBeacon then Exit;
  fGame.GameInputProcess.CmdGame(gic_GameAlertBeacon, KMPointF(X,Y), MyPlayer.PlayerIndex);
  Beacon_Cancel;
end;


constructor TKMGamePlayInterface.Create(aScreenX, aScreenY: Word; aMultiplayer, aReplay: Boolean);
var S: TKMShape;
begin
  inherited Create(aScreenX, aScreenY);

  fMultiplayer := aMultiplayer;
  fReplay := aReplay;
  //Instruct to use global Terrain
  LastSaveName := '';
  fJoiningGroups := False;
  fPlacingBeacon := False;
  SelectingTroopDirection := false;
  SelectingDirPosition.X := 0;
  SelectingDirPosition.Y := 0;
  ShownMessage := -1; //0 is the first message, -1 is invalid

  LastSchoolUnit   := 0;
  LastBarracksUnit := 0;
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

    Image_DirectionCursor := TKMImage.Create(Panel_Main,0,0,35,36,519);
    Image_DirectionCursor.Hide;

    //Debugging displays
    Label_DebugInfo := TKMLabel.Create(Panel_Main,224+8,106,'',fnt_Outline,taLeft);

{I plan to store all possible layouts on different pages which gets displayed one at a time}
{==========================================================================================}
  Create_Controls_Page; //Includes all the child pages

  Create_NetWait_Page; //Overlay blocking everyhitng but sidestack and messages
  Create_Allies_Page; //MessagePage sibling
  Create_Chat_Page; //On top of NetWait to allow players to chat while waiting for late opponents
  Create_Message_Page; //Must go bellow message stack
  Create_SideStack; //Messages, Allies, Chat icons

  Create_Pause_Page;
  Create_Replay_Page; //Replay controls
  Create_PlayMore_Page; //Must be created last, so that all controls behind are blocked
  Create_MPPlayMore_Page;

  Bevel_HintBG := TKMBevel.Create(Panel_Main,224+32,Panel_Main.Height-23,300,21);
  Bevel_HintBG.BackAlpha := 0.5;
  Bevel_HintBG.Hide;
  Bevel_HintBG.Anchors := [akLeft, akBottom];
  Label_Hint := TKMLabel.Create(Panel_Main,224+36,Panel_Main.Height-21,0,0,'',fnt_Outline,taLeft);
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
  ShowSwords := (Panel_Main.Height >= Sidebar_Top.Height + Sidebar_Middle.Height + 10 + Panel_Controls.Height);
  Sidebar_Middle.Visible := ShowSwords;
  //Needs to be -10 when the swords are hidden so it fits 1024x576
  Panel_Controls.Top := Sidebar_Top.Height - 10 + (10+Sidebar_Middle.Height) * Byte(ShowSwords);
end;


{Pause overlay page}
procedure TKMGamePlayInterface.Create_Pause_Page;
begin
  Panel_Pause := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_Pause.Stretch;
  Bevel_Pause := TKMBevel.Create(Panel_Pause, -1, -1, Panel_Main.Width + 2, Panel_Main.Height + 2);
  Image_Pause := TKMImage.Create(Panel_Pause, (Panel_Main.Width div 2), (Panel_Main.Height div 2) - 40, 0, 0, 556);
  Label_Pause1 := TKMLabel.Create(Panel_Pause, (Panel_Main.Width div 2), (Panel_Main.Height div 2),
    fTextLibrary[TX_POPUP_PAUSE], fnt_Antiqua, taCenter);
  Label_Pause2 := TKMLabel.Create(Panel_Pause, (Panel_Main.Width div 2), (Panel_Main.Height div 2) + 20,
    Format(fTextLibrary[TX_GAMEPLAY_PAUSE_INFO], ['"P"']), fnt_Grey, taCenter);
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
procedure TKMGamePlayInterface.Create_PlayMore_Page;
begin
  Panel_PlayMore := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width,Panel_Main.Height);
  Panel_PlayMore.Stretch;
    Bevel_PlayMore := TKMBevel.Create(Panel_PlayMore,-1,-1,Panel_Main.Width+2,Panel_Main.Height+2);
    Bevel_PlayMore.Stretch;

    Panel_PlayMoreMsg := TKMPanel.Create(Panel_PlayMore,(Panel_Main.Width div 2)-100,(Panel_Main.Height div 2)-100,200,200);
    Panel_PlayMoreMsg.Center;
      Image_PlayMore:=TKMImage.Create(Panel_PlayMoreMsg,100,40,0,0,556);
      Image_PlayMore.ImageCenter;

      Label_PlayMore  := TKMLabel.Create(Panel_PlayMoreMsg,100,80,'<<<LEER>>>',fnt_Outline,taCenter);
      Button_PlayMore := TKMButton.Create(Panel_PlayMoreMsg,0,100,200,30,'<<<LEER>>>',bsGame);
      Button_PlayQuit := TKMButton.Create(Panel_PlayMoreMsg,0,140,200,30,'<<<LEER>>>',bsGame);
      Button_PlayMore.OnClick := PlayMoreClick;
      Button_PlayQuit.OnClick := PlayMoreClick;
    Panel_PlayMore.Hide; //Initially hidden
end;


procedure TKMGamePlayInterface.Create_MPPlayMore_Page;
begin
  Panel_MPPlayMore := TKMPanel.Create(Panel_Main,(Panel_Main.Width div 2)-200,(Panel_Main.Height div 2)-100,400,200);
  Panel_MPPlayMore.Center;
    Bevel_MPPlayMore := TKMBevel.Create(Panel_MPPlayMore,-1,-1,Panel_MPPlayMore.Width+2,Panel_MPPlayMore.Height+2);
    Bevel_MPPlayMore.Stretch;

      Image_MPPlayMore:=TKMImage.Create(Panel_MPPlayMore,200,40,0,0,556);
      Image_MPPlayMore.ImageCenter;

      Label_MPPlayMore  := TKMLabel.Create(Panel_MPPlayMore,200,80,'<<<LEER>>>',fnt_Outline,taCenter);
      Button_MPPlayMore := TKMButton.Create(Panel_MPPlayMore,100,100,200,30,'<<<LEER>>>',bsGame);
      Button_MPPlayQuit := TKMButton.Create(Panel_MPPlayMore,100,140,200,30,'<<<LEER>>>',bsGame);
      Button_MPPlayMore.OnClick := MPPlayMoreClick;
      Button_MPPlayQuit.OnClick := MPPlayMoreClick;
    Panel_MPPlayMore.Hide; //Initially hidden
end;


//Waiting for Net events page, it's similar to PlayMore, but is layered differentlybelow chat panel
procedure TKMGamePlayInterface.Create_NetWait_Page;
begin
  Panel_NetWait := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width,Panel_Main.Height);
  Panel_NetWait.Stretch;
    Bevel_NetWait := TKMBevel.Create(Panel_NetWait,-1,-1,Panel_Main.Width+2,Panel_Main.Height+2);
    Bevel_NetWait.Stretch;

    Panel_NetWaitMsg := TKMPanel.Create(Panel_NetWait,0,(Panel_Main.Height div 2)-200,Panel_Main.Width,400);
    Panel_NetWaitMsg.Center;
      Image_NetWait:=TKMImage.Create(Panel_NetWaitMsg,Panel_Main.Width div 2,40,0,0,556);
      Image_NetWait.ImageCenter;

      Label_NetWait  := TKMLabel.Create(Panel_NetWaitMsg,Panel_Main.Width div 2,80,'<<<LEER>>>',fnt_Outline,taCenter);
      Label_NetDropPlayersDelay := TKMLabel.Create(Panel_NetWaitMsg,Panel_Main.Width div 2,110,'<<<LEER>>>',fnt_Outline,taCenter);
      Panel_NetWaitButtons := TKMPanel.Create(Panel_NetWaitMsg,0,140,Panel_Main.Width,80);
        Button_NetQuit := TKMButton.Create(Panel_NetWaitButtons,(Panel_Main.Width div 2)-150,0,300,30,fTextLibrary[TX_GAMEPLAY_QUIT_TO_MENU],bsGame);
        Button_NetQuit.OnClick := NetWaitClick;
        Button_NetDropPlayers := TKMButton.Create(Panel_NetWaitButtons,(Panel_Main.Width div 2)-150,40,300,30,fTextLibrary[TX_GAMEPLAY_DROP_PLAYERS],bsGame);
        Button_NetDropPlayers.OnClick := NetWaitClick;

      Panel_NetWaitConfirm := TKMPanel.Create(Panel_NetWaitMsg,0,180,Panel_Main.Width,140);
        Label_NetWaitConfirm := TKMLabel.Create(Panel_NetWaitConfirm,(Panel_Main.Width div 2),10,'<<<LEER>>>',fnt_Outline,taCenter);
        Button_NetConfirmYes := TKMButton.Create(Panel_NetWaitConfirm,(Panel_Main.Width div 2)-150,40,300,30,'<<<LEER>>>',bsGame);
        Button_NetConfirmYes.OnClick := NetWaitClick;
        Button_NetConfirmNo := TKMButton.Create(Panel_NetWaitConfirm,(Panel_Main.Width div 2)-150,80,300,30,fTextLibrary[TX_GAMEPLAY_CONFIRM_CANCEL],bsGame);
        Button_NetConfirmNo.OnClick := NetWaitClick;
      Panel_NetWaitConfirm.Hide;
    Panel_NetWait.Hide; //Initially hidden
end;


procedure TKMGamePlayInterface.Create_SideStack;
var I: Integer;
begin
  Image_MPChat := TKMImage.Create(Panel_Main,TOOLBAR_WIDTH,Panel_Main.Height-48,30,48,494);
  Image_MPChat.Anchors := [akLeft, akBottom];
  Image_MPChat.HighlightOnMouseOver := true;
  Image_MPChat.Hint := fTextLibrary[TX_GAMEPLAY_CHAT_HINT];
  Image_MPChat.OnClick := Chat_Click;
  Label_MPChatUnread := TKMLabel.Create(Panel_Main,TOOLBAR_WIDTH,Panel_Main.Height-30,30,36,'',fnt_Outline,taCenter);
  Label_MPChatUnread.FontColor := $FF0000FF; //Red
  Label_MPChatUnread.Anchors := [akLeft, akBottom];
  Label_MPChatUnread.Hitable := false; //Clicks should only go to the image, not the flashing label
  Label_MPChatUnread.AutoWrap := true;

  Image_MPAllies := TKMImage.Create(Panel_Main,TOOLBAR_WIDTH,Panel_Main.Height-48*2,30,48,496);
  Image_MPAllies.Anchors := [akLeft, akBottom];
  Image_MPAllies.HighlightOnMouseOver := true;
  Image_MPAllies.Hint := fTextLibrary[TX_GAMEPLAY_PLAYERS_HINT];
  Image_MPAllies.OnClick := Allies_Click;

  for I := 0 to MAX_VISIBLE_MSGS do
  begin
    Image_Message[I] := TKMImage.Create(Panel_Main,TOOLBAR_WIDTH,0,30,48,495);
    Image_Message[I].Top := Panel_Main.Height - (I+1)*48 - IfThen(fMultiplayer, 48*2);
    Image_Message[I].Anchors := [akLeft, akBottom];
    Image_Message[I].Disable;
    Image_Message[I].Hide;
    Image_Message[I].HighlightOnMouseOver := True;
    Image_Message[I].Tag := I;
    Image_Message[I].OnClick := Message_Click;
  end;
end;


procedure TKMGamePlayInterface.Create_Replay_Page;
begin
  Panel_Replay := TKMPanel.Create(Panel_Main, 0, 0, 1024, 768);
  Panel_Replay.Stretch;

    Panel_ReplayCtrl := TKMPanel.Create(Panel_Replay, 320, 8, 160, 60);
      PercentBar_Replay     := TKMPercentBar.Create(Panel_ReplayCtrl, 0, 0, 160, 20);
      Label_Replay          := TKMLabel.Create(Panel_ReplayCtrl,  80,  2, '<<<LEER>>>', fnt_Grey, taCenter);
      Button_ReplayRestart  := TKMButton.Create(Panel_ReplayCtrl,  0, 24, 24, 24, 582, rxGui, bsGame);
      Button_ReplayPause    := TKMButton.Create(Panel_ReplayCtrl, 25, 24, 24, 24, 583, rxGui, bsGame);
      Button_ReplayStep     := TKMButton.Create(Panel_ReplayCtrl, 50, 24, 24, 24, 584, rxGui, bsGame);
      Button_ReplayResume   := TKMButton.Create(Panel_ReplayCtrl, 75, 24, 24, 24, 585, rxGui, bsGame);
      Button_ReplayExit     := TKMButton.Create(Panel_ReplayCtrl,100, 24, 24, 24, 586, rxGui, bsGame);
      Button_ReplayRestart.OnClick := ReplayClick;
      Button_ReplayPause.OnClick   := ReplayClick;
      Button_ReplayStep.OnClick    := ReplayClick;
      Button_ReplayResume.OnClick  := ReplayClick;
      Button_ReplayExit.OnClick    := ReplayClick;
      Button_ReplayRestart.Hint := fTextLibrary[TX_REPLAY_RESTART];
      Button_ReplayPause.Hint   := fTextLibrary[TX_REPLAY_PAUSE];
      Button_ReplayStep.Hint    := fTextLibrary[TX_REPLAY_STEP];
      Button_ReplayResume.Hint  := fTextLibrary[TX_REPLAY_RESUME];
      Button_ReplayExit.Hint    := fTextLibrary[TX_REPLAY_QUIT];
      Button_ReplayStep.Disable; //Initial state
      Button_ReplayResume.Disable; //Initial state
end;


{Message page}
procedure TKMGamePlayInterface.Create_Message_Page;
begin
  Panel_Message:=TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - MESSAGE_AREA_HEIGHT, Panel_Main.Width - TOOLBAR_WIDTH, MESSAGE_AREA_HEIGHT);
  Panel_Message.Anchors := [akLeft, akRight, akBottom];
  Panel_Message.Hide; //Hide it now because it doesn't get hidden by SwitchPage

    Image_Scroll := TKMImage.Create(Panel_Message,0,0,600,190,409);
    Image_Scroll.ImageStretch;

    Label_MessageText:=TKMLabel.Create(Panel_Message,47,67,432,122,'',fnt_Antiqua,taLeft);
    Label_MessageText.AutoWrap := True;

    Button_MessageGoTo:=TKMButton.Create(Panel_Message,490,74,100,24,fTextLibrary[TX_MSG_GOTO], bsGame);
    Button_MessageGoTo.Font := fnt_Antiqua;
    Button_MessageGoTo.Hint := fTextLibrary[TX_MSG_GOTO_HINT];
    Button_MessageGoTo.OnClick := Message_GoTo;

    Button_MessageDelete:=TKMButton.Create(Panel_Message,490,104,100,24,fTextLibrary[TX_MSG_DELETE], bsGame);
    Button_MessageDelete.Font := fnt_Antiqua;
    Button_MessageDelete.Hint := fTextLibrary[TX_MSG_DELETE_HINT];
    Button_MessageDelete.OnClick := Message_Delete;
    Button_MessageDelete.MakesSound := False; //Don't play default Click as these buttons use sfx_MessageClose

    Button_MessageClose:=TKMButton.Create(Panel_Message,490,134,100,24,fTextLibrary[TX_MSG_CLOSE], bsGame);
    Button_MessageClose.Font := fnt_Antiqua;
    Button_MessageClose.Hint := fTextLibrary[TX_MSG_CLOSE_HINT];
    Button_MessageClose.OnClick := Message_Close;
    Button_MessageClose.MakesSound := False; //Don't play default Click as these buttons use sfx_MessageClose
end;


{Chat page}
procedure TKMGamePlayInterface.Create_Chat_Page;
begin
  Panel_Chat := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - MESSAGE_AREA_HEIGHT, Panel_Main.Width - TOOLBAR_WIDTH, MESSAGE_AREA_HEIGHT);
  Panel_Chat.Anchors := [akLeft, akBottom];
  Panel_Chat.Hide;

    Image_Chat := TKMImage.Create(Panel_Chat,0,0,800,190,409);
    Image_Chat.Anchors := [akLeft,akTop,akBottom];
    Image_Chat.ImageStretch;

    //Allow to resize chat area height
    Dragger_Chat := TKMDragger.Create(Panel_Chat, 45, 36, 800-85, 10);
    Dragger_Chat.Anchors := [akTop];
    Dragger_Chat.SetBounds(0, -MESSAGE_AREA_RESIZE_Y, 0, 0);
    Dragger_Chat.OnMove := Chat_Resize;

    Memo_ChatText := TKMMemo.Create(Panel_Chat,45,50,800-85,101, fnt_Metal, bsGame);
    Memo_ChatText.ScrollDown := True;
    Memo_ChatText.AutoWrap := True;
    Memo_ChatText.Anchors := [akLeft, akTop, akRight, akBottom];

    Edit_ChatMsg := TKMEdit.Create(Panel_Chat, 45, 151, 680-85, 20, fnt_Metal);
    Edit_ChatMsg.Anchors := [akLeft, akRight, akBottom];
    Edit_ChatMsg.OnKeyDown := Chat_Post;
    Edit_ChatMsg.Text := '';
    Edit_ChatMsg.ShowColors := True;

    CheckBox_SendToAllies := TKMCheckBox.Create(Panel_Chat,645,154,155,20, fTextLibrary[TX_GAMEPLAY_CHAT_TOTEAM], fnt_Outline);
    CheckBox_SendToAllies.Checked := True;
    CheckBox_SendToAllies.Anchors := [akRight, akBottom];

    Image_ChatClose := TKMImage.Create(Panel_Chat, 800-35, 20, 32, 32, 52);
    Image_ChatClose.Anchors := [akTop, akRight];
    Image_ChatClose.Hint := fTextLibrary[TX_MSG_CLOSE_HINT];
    Image_ChatClose.OnClick := Chat_Close;
    Image_ChatClose.HighlightOnMouseOver := True;
end;


procedure TKMGamePlayInterface.Create_Controls_Page;
const
  MainHint: array [TKMTabButtons] of Word = (TX_MENU_TAB_HINT_BUILD, TX_MENU_TAB_HINT_DISTRIBUTE,
                                             TX_MENU_TAB_HINT_STATISTICS, TX_MENU_TAB_HINT_OPTIONS);
var
  T: TKMTabButtons;
begin
  Panel_Controls := TKMPanel.Create(Panel_Main, 0, 368, 224, 376);
  Panel_Controls.Anchors := [akLeft, akTop];

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
      Button_Main[T].Hint := fTextLibrary[MainHint[T]];
      Button_Main[T].OnClick := SwitchPage;
    end;
    Button_Back := TKMButton.Create(Panel_Controls, TB_PAD, 4, 42, 36, 443, rxGui, bsGame);
    Button_Back.OnClick := SwitchPage;
    Button_Back.Hint := fTextLibrary[TX_MENU_TAB_HINT_GO_BACK];

    Label_MenuTitle := TKMLabel.Create(Panel_Controls, 54, 4, 138, 0, '', fnt_Metal, taLeft);
    Label_MenuTitle.AutoWrap := True;

  Create_Build_Page;
  Create_Ratios_Page;
  Create_Stats_Page;
  Create_Menu_Page;
    Create_Save_Page;
    Create_Load_Page;
    Create_Settings_Page;
    Create_Quit_Page;

  Create_Unit_Page;

  Create_House_Page;
    Create_Market_Page;
    Create_Store_Page;
    Create_School_Page;
    Create_Barracks_Page;
    Create_Woodcutter_Page;
    //Create_TownHall_Page; //I don't want to make it at all yet
end;


{Allies page}
procedure TKMGamePlayInterface.Create_Allies_Page;
var I,K: Integer;
begin
  Panel_Allies := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - MESSAGE_AREA_HEIGHT, Panel_Main.Width - TOOLBAR_WIDTH, MESSAGE_AREA_HEIGHT);
  Panel_Allies.Anchors := [akLeft, akRight, akBottom];
  Panel_Allies.Hide;

    with TKMImage.Create(Panel_Allies,0,0,800,190,409) do ImageStretch;

    Label_PeacetimeRemaining := TKMLabel.Create(Panel_Allies,400,20,'',fnt_Outline,taCenter);

    for I := 0 to MAX_PLAYERS - 1 do
    begin
      if (I mod 4) = 0 then //Header for each column
      begin
        TKMLabel.Create(Panel_Allies,  70+(I div 4)*380, 60, 140, 20, fTextLibrary[TX_LOBBY_HEADER_PLAYERS], fnt_Outline, taLeft);
        TKMLabel.Create(Panel_Allies, 220+(I div 4)*380, 60, 140, 20, fTextLibrary[TX_LOBBY_HEADER_TEAM], fnt_Outline, taLeft);
        TKMLabel.Create(Panel_Allies, 350+(I div 4)*380, 60, fTextLibrary[TX_LOBBY_HEADER_PING], fnt_Outline, taCenter);
      end;
      Image_AlliesLang[I] := TKMImage.Create(Panel_Allies,       50+(I div 4)*380, 82+(I mod 4)*24, 16,  11,  0, rxGuiMain);
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

    Image_AlliesClose:=TKMImage.Create(Panel_Allies,800-35,20,32,32,52,rxGui);
    Image_AlliesClose.Hint := fTextLibrary[TX_MSG_CLOSE_HINT];
    Image_AlliesClose.OnClick := Allies_Close;
    Image_AlliesClose.HighlightOnMouseOver := True;
end;


{Build page}
procedure TKMGamePlayInterface.Create_Build_Page;
var I: Integer;
begin
  Panel_Build := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 342);
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
    Button_BuildRoad.Hint   := fTextLibrary[TX_BUILD_ROAD_HINT];
    Button_BuildField.Hint  := fTextLibrary[TX_BUILD_FIELD_HINT];
    Button_BuildWine.Hint   := fTextLibrary[TX_BUILD_WINE_HINT];
    Button_BuildCancel.Hint := fTextLibrary[TX_BUILD_CANCEL_HINT];

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
procedure TKMGamePlayInterface.Create_Ratios_Page;
const Res:array[1..4] of TResourceType = (rt_Steel,rt_Coal,rt_Wood,rt_Corn);
var I: Integer;
begin
  Panel_Ratios:=TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 342);

  for i:=1 to 4 do begin
    Button_Ratios[i]         := TKMButton.Create(Panel_Ratios, (i-1)*40,20,32,32,0, rxGui, bsGame);
    Button_Ratios[i].TexID   := fResource.Resources[Res[i]].GUIIcon;
    Button_Ratios[i].Hint    := fResource.Resources[Res[i]].Title;
    Button_Ratios[i].Tag     := i;
    Button_Ratios[i].OnClick := SwitchPage_Ratios;
  end;

  Image_RatioPic0 := TKMImage.Create(Panel_Ratios,4,76,32,32,327);
  Label_RatioLab0 := TKMLabel.Create(Panel_Ratios,36,72,148,30,'<<<LEER>>>',fnt_Outline,taLeft);

  for i:=1 to 4 do begin
    Image_RatioPic[i]            := TKMImage.Create(Panel_Ratios,4,124+(i-1)*50,32,32,327);
    TrackBar_RatioRat[i]         := TKMTrackBar.Create(Panel_Ratios,40,116+(i-1)*50,140,0,5);
    TrackBar_RatioRat[i].Font    := fnt_Grey; //fnt_Metal doesn't fit the text
    TrackBar_RatioRat[i].Tag     := i;
    TrackBar_RatioRat[i].OnChange:= RatiosChange;
  end;
end;


{Statistics page}
procedure TKMGamePlayInterface.Create_Stats_Page;
const LineHeight=34; Nil_Width=10; House_Width=30; Unit_Width=26;
var i,k:integer; hc,uc,off:integer;
  LineBase:integer;
begin
  Panel_Stats:=TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 342);

  hc:=1; uc:=1;
  for i:=1 to 8 do begin
    LineBase := (i-1)*LineHeight;
    case i of
    1: begin
          TKMBevel.Create(Panel_Stats,  0,LineBase, 56,30);
          TKMBevel.Create(Panel_Stats, 63,LineBase, 56,30);
          TKMBevel.Create(Panel_Stats,126,LineBase, 56,30);
       end;
    2: begin
          TKMBevel.Create(Panel_Stats,  0,LineBase, 86,30);
          TKMBevel.Create(Panel_Stats, 96,LineBase, 86,30);
       end;
    3: begin
          TKMBevel.Create(Panel_Stats,  0,LineBase, 86,30);
          TKMBevel.Create(Panel_Stats,96,LineBase, 86,30);
       end;
    4: begin
          TKMBevel.Create(Panel_Stats,  0,LineBase, 86,30);
          TKMBevel.Create(Panel_Stats, 96,LineBase, 86,30);
       end;
    5:    TKMBevel.Create(Panel_Stats,  0,LineBase,116,30);
    6:    TKMBevel.Create(Panel_Stats,  0,LineBase,116,30);
    7: begin
         TKMBevel.Create(Panel_Stats,   0,LineBase, 32,30);
         TKMBevel.Create(Panel_Stats,  70,LineBase, 86,30);
       end;
    8: begin
          TKMBevel.Create(Panel_Stats,  0,LineBase, 90,30);
          TKMBevel.Create(Panel_Stats,112,LineBase, 52,30);
       end;
    end;

    off:=0;
    for k:=1 to 8 do
    case StatCount[i,k] of
      0: if i=1 then
           inc(off,Nil_Width-3) //Special fix to fit first row of 3x2 items
         else
           inc(off,Nil_Width);
      1: begin
          Stat_HousePic[hc] := TKMImage.Create(Panel_Stats,off,LineBase,House_Width,30,41); //Filled with [?] at start
          Stat_HouseWip[hc] := TKMLabel.Create(Panel_Stats,off+House_Width  ,LineBase   ,'',fnt_Grey,taRight);
          Stat_HouseQty[hc] := TKMLabel.Create(Panel_Stats,off+House_Width-2,LineBase+16,'-',fnt_Grey,taRight);
          Stat_HousePic[hc].Hint := fResource.HouseDat[StatHouse[hc]].HouseName;
          Stat_HouseQty[hc].Hitable := False;
          Stat_HouseWip[hc].Hitable := False;
          Stat_HousePic[hc].ImageCenter;
          inc(hc);
          inc(off,House_Width);
         end;
      2: begin
          Stat_UnitPic[uc] := TKMImage.Create(Panel_Stats,off,LineBase,Unit_Width,30, fResource.UnitDat[StatUnit[uc]].GUIIcon);
          Stat_UnitQty[uc] := TKMLabel.Create(Panel_Stats,off+Unit_Width-2,LineBase+16,'-',fnt_Grey,taRight);
          Stat_UnitPic[uc].Hint := fResource.UnitDat[StatUnit[uc]].UnitName;
          Stat_UnitQty[uc].Hitable := False;
          Stat_UnitPic[uc].ImageCenter;
          inc(uc);
          inc(off,Unit_Width);
         end;
    end;
  end;
end;


{Menu page}
procedure TKMGamePlayInterface.Create_Menu_Page;
begin
  Panel_Menu := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 342);
  Button_Menu_Load := TKMButton.Create(Panel_Menu, 0, 20, TB_WIDTH, 30, fTextLibrary[TX_MENU_LOAD_GAME], bsGame);
  Button_Menu_Load.OnClick := SwitchPage;
  Button_Menu_Load.Hint := fTextLibrary[TX_MENU_LOAD_GAME];
  Button_Menu_Save := TKMButton.Create(Panel_Menu, 0, 60, TB_WIDTH, 30, fTextLibrary[TX_MENU_SAVE_GAME], bsGame);
  Button_Menu_Save.OnClick := SwitchPage;
  Button_Menu_Save.Hint := fTextLibrary[TX_MENU_SAVE_GAME];
  Button_Menu_Settings := TKMButton.Create(Panel_Menu, 0, 100, TB_WIDTH, 30, fTextLibrary[TX_MENU_SETTINGS], bsGame);
  Button_Menu_Settings.OnClick := SwitchPage;
  Button_Menu_Settings.Hint := fTextLibrary[TX_MENU_SETTINGS];
  Button_Menu_Quit := TKMButton.Create(Panel_Menu, 0, 170, TB_WIDTH, 30, fTextLibrary[TX_MENU_QUIT_MISSION], bsGame);
  Button_Menu_Quit.Hint := fTextLibrary[TX_MENU_QUIT_MISSION];
  Button_Menu_Quit.OnClick := SwitchPage;
  Button_Menu_TrackUp := TKMButton.Create(Panel_Menu, 150, 300, 30, 30, '>', bsGame);
  Button_Menu_TrackDown := TKMButton.Create(Panel_Menu, 0, 300, 30, 30, '<', bsGame);
  Button_Menu_TrackUp.Hint := fTextLibrary[TX_MUSIC_NEXT_HINT];
  Button_Menu_TrackDown.Hint := fTextLibrary[TX_MUSIC_PREV_HINT];
  Button_Menu_TrackUp.OnClick := Menu_NextTrack;
  Button_Menu_TrackDown.OnClick := Menu_PreviousTrack;
  TKMLabel.Create(Panel_Menu, 0, 260, TB_WIDTH, 30, fTextLibrary[TX_MUSIC_PLAYER], fnt_Metal, taCenter);
  Label_Menu_Track := TKMLabel.Create(Panel_Menu, 0, 276, TB_WIDTH, 30, '', fnt_Grey, taCenter);
  Label_Menu_Track.Hitable := False; //It can block hits for the track Up/Down buttons as they overlap
  Label_GameTime := TKMLabel.Create(Panel_Menu, 0, 214, TB_WIDTH, 20, '', fnt_Outline, taCenter);
end;


{Save page}
procedure TKMGamePlayInterface.Create_Save_Page;
begin
  Panel_Save := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 342);

    List_Save := TKMListBox.Create(Panel_Save, 0, 4, TB_WIDTH, 220, fnt_Metal, bsGame);
    List_Save.OnChange := Menu_Save_ListChange;

    Edit_Save := TKMEdit.Create(Panel_Save, 0, 235, TB_WIDTH, 20, fnt_Metal);
    Edit_Save.AllowedChars := acFileName;
    Edit_Save.OnChange := Menu_Save_EditChange;
    Label_SaveExists := TKMLabel.Create(Panel_Save,0,260,TB_WIDTH,30,fTextLibrary[TX_GAMEPLAY_SAVE_EXISTS],fnt_Outline,taLeft);
    CheckBox_SaveExists := TKMCheckBox.Create(Panel_Save,0,280,TB_WIDTH,20,fTextLibrary[TX_GAMEPLAY_SAVE_OVERWRITE], fnt_Metal);
    CheckBox_SaveExists.OnClick := Menu_Save_CheckboxChange;

    Button_Save := TKMButton.Create(Panel_Save,0,300,TB_WIDTH,30,fTextLibrary[TX_GAMEPLAY_SAVE_SAVE], bsGame);
    Button_Save.OnClick := Menu_Save_Click;
end;


{Load page}
procedure TKMGamePlayInterface.Create_Load_Page;
begin
  Panel_Load := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 342);

    List_Load := TKMListBox.Create(Panel_Load, 0, 2, TB_WIDTH, 260, fnt_Metal, bsGame);
    List_Load.OnChange := Menu_Load_ListClick;

    Label_Load_Description := TKMLabel.Create(Panel_Load,0,265,TB_WIDTH,0,'',fnt_Grey,taLeft);
    Label_Load_Description.AutoWrap := true;

    Button_Load := TKMButton.Create(Panel_Load,0,300,TB_WIDTH,30,fTextLibrary[TX_GAMEPLAY_LOAD], bsGame);
    Button_Load.OnClick := Menu_Load_Click;
end;


{Options page}
procedure TKMGamePlayInterface.Create_Settings_Page;
const
  PAD = 10;
  WID = TB_WIDTH - PAD * 2;
begin
  Panel_Settings := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 342);
    CheckBox_Settings_Autosave := TKMCheckBox.Create(Panel_Settings,PAD,15,WID,20,fTextLibrary[TX_MENU_OPTIONS_AUTOSAVE],fnt_Metal);
    CheckBox_Settings_Autosave.OnClick := Menu_Settings_Change;
    TrackBar_Settings_Brightness := TKMTrackBar.Create(Panel_Settings,PAD,40,WID,0,20);
    TrackBar_Settings_Brightness.Caption := fTextLibrary[TX_MENU_OPTIONS_BRIGHTNESS];
    TrackBar_Settings_Brightness.OnChange := Menu_Settings_Change;
    TrackBar_Settings_ScrollSpeed := TKMTrackBar.Create(Panel_Settings,PAD,95,WID,0,20);
    TrackBar_Settings_ScrollSpeed.Caption := fTextLibrary[TX_MENU_OPTIONS_SCROLL_SPEED];
    TrackBar_Settings_ScrollSpeed.OnChange := Menu_Settings_Change;
    TrackBar_Settings_SFX := TKMTrackBar.Create(Panel_Settings,PAD,150,WID,0,20);
    TrackBar_Settings_SFX.Caption := fTextLibrary[TX_MENU_SFX_VOLUME];
    TrackBar_Settings_SFX.Hint := fTextLibrary[TX_MENU_SFX_VOLUME_HINT];
    TrackBar_Settings_SFX.OnChange := Menu_Settings_Change;
    TrackBar_Settings_Music := TKMTrackBar.Create(Panel_Settings,PAD,205,WID,0,20);
    TrackBar_Settings_Music.Caption := fTextLibrary[TX_MENU_MUSIC_VOLUME];
    TrackBar_Settings_Music.Hint := fTextLibrary[TX_MENU_MUSIC_VOLUME_HINT];
    TrackBar_Settings_Music.OnChange := Menu_Settings_Change;
    CheckBox_Settings_MusicOff := TKMCheckBox.Create(Panel_Settings,PAD,260,WID,20,fTextLibrary[TX_MENU_OPTIONS_MUSIC_DISABLE],fnt_Metal);
    CheckBox_Settings_MusicOff.Hint := fTextLibrary[TX_MENU_OPTIONS_MUSIC_DISABLE_HINT];
    CheckBox_Settings_MusicOff.OnClick := Menu_Settings_Change;
    CheckBox_Settings_ShuffleOn := TKMCheckBox.Create(Panel_Settings,PAD,285,WID,20,fTextLibrary[TX_MENU_OPTIONS_MUSIC_SHUFFLE],fnt_Metal);
    CheckBox_Settings_ShuffleOn.OnClick := Menu_Settings_Change;
end;


{Quit page}
procedure TKMGamePlayInterface.Create_Quit_Page;
begin
  Panel_Quit := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 342);
    with TKMLabel.Create(Panel_Quit, 0, 30, TB_WIDTH, 70, fTextLibrary[TX_MENU_QUIT_QUESTION], fnt_Outline, taCenter) do
      AutoWrap := True;
    Button_Quit_Yes := TKMButton.Create(Panel_Quit, 0, 100, TB_WIDTH, 30, fTextLibrary[TX_MENU_QUIT_MISSION], bsGame);
    Button_Quit_No := TKMButton.Create(Panel_Quit, 0, 140, TB_WIDTH, 30, fTextLibrary[TX_MENU_DONT_QUIT_MISSION], bsGame);
    Button_Quit_Yes.Hint := fTextLibrary[TX_MENU_QUIT_MISSION];
    Button_Quit_No.Hint := fTextLibrary[TX_MENU_DONT_QUIT_MISSION];
    Button_Quit_Yes.OnClick := Menu_QuitMission;
    Button_Quit_No.OnClick := SwitchPage;
end;


{Unit page}
procedure TKMGamePlayInterface.Create_Unit_Page;
begin
  Panel_Unit := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 342);
    Label_UnitName        := TKMLabel.Create(Panel_Unit,0,16,TB_WIDTH,30,'',fnt_Outline,taCenter);
    Image_UnitPic         := TKMImage.Create(Panel_Unit,0,38,54,100,521);
    Label_UnitCondition   := TKMLabel.Create(Panel_Unit,65,40,116,30,fTextLibrary[TX_UNIT_CONDITION],fnt_Grey,taCenter);
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
    //@Lewin: I suggest we check other games, but I have a feeling that using same shortcuts for every version would be better
    //@Krom: Yes we should check other games. I did it this way to match KaM TSK/TPR.
    Button_Army_GoTo.Hint   := fTextLibrary[TX_ARMY_GOTO_HINT];
    Button_Army_Stop.Hint   := Format(fTextLibrary[TX_TROOP_HALT_HINT], [SC_ARMY_HALT]);
    Button_Army_Attack.Hint := fTextLibrary[TX_ARMY_ATTACK_HINT];
    Button_Army_RotCW.Hint  := fTextLibrary[TX_ARMY_ROTATE_CW_HINT];
    Button_Army_Storm.Hint  := fTextLibrary[TX_ARMY_STORM_HINT];
    Button_Army_RotCCW.Hint := fTextLibrary[TX_ARMY_ROTATE_CCW_HINT];
    Button_Army_ForDown.Hint:= fTextLibrary[TX_ARMY_LINE_ADD_HINT];
    Button_Army_ForUp.Hint  := fTextLibrary[TX_ARMY_LINE_REM_HINT];
    Button_Army_Split.Hint  := Format(fTextLibrary[TX_TROOP_SPLIT_HINT], [SC_ARMY_SPLIT]);
    Button_Army_Join.Hint   := Format(fTextLibrary[TX_TROOP_LINK_HINT], [SC_ARMY_LINK]);
    Button_Army_Feed.Hint   := fTextLibrary[TX_ARMY_FEED_HINT];
    Button_Unit_Dismiss.Hint:= 'Dismiss unit';

    {Army controls...
    Go to     Stop      Attack
    Rotate    Storm     Rotate
    -Column   [Info]    +Column
    Split     Join      Feed}

    Panel_Army_JoinGroups := TKMPanel.Create(Panel_Unit, 0, 160, TB_WIDTH, 182);
    Label_Army_Join_Message := TKMLabel.Create(Panel_Army_JoinGroups, 0, 30, TB_WIDTH, 65, fTextLibrary[TX_ARMY_JOIN_SELECT], fnt_Outline, taCenter);
    Button_Army_Join_Cancel := TKMButton.Create(Panel_Army_JoinGroups, 0, 95, TB_WIDTH, 30, fTextLibrary[TX_ARMY_JOIN_CANCEL], bsGame);

  Button_Army_Join_Cancel.OnClick := Army_HideJoinMenu;
end;


{House description page}
procedure TKMGamePlayInterface.Create_House_Page;
var I: Integer;
begin
  Panel_House := TKMPanel.Create(Panel_Controls, TB_PAD, 44, TB_WIDTH, 342);
    //Thats common things
    //Custom things come in fixed size blocks (more smaller Panels?), and to be shown upon need
    Label_House := TKMLabel.Create(Panel_House, 0, 14, TB_WIDTH, 0, '', fnt_Outline, taCenter);
    Button_House_Goods := TKMButton.Create(Panel_House,0,42,30,30,37, rxGui, bsGame);
    Button_House_Goods.Hint := fTextLibrary[TX_HOUSE_TOGGLE_DELIVERS_HINT];
    Button_House_Goods.OnClick := House_WareDeliveryToggle;
    Button_House_Repair := TKMButton.Create(Panel_House,30,42,30,30,40, rxGui, bsGame);
    Button_House_Repair.Hint := fTextLibrary[TX_HOUSE_TOGGLE_REPAIR_HINT];
    Button_House_Repair.OnClick := House_RepairToggle;
    Image_House_Logo := TKMImage.Create(Panel_House,60,41,32,32,338);
    Image_House_Logo.ImageCenter;
    Image_House_Worker := TKMImage.Create(Panel_House,90,41,32,32,141);
    Image_House_Worker.ImageCenter;
    Label_HouseHealth := TKMLabel.Create(Panel_House,120,45,55,15,fTextLibrary[TX_HOUSE_CONDITION],fnt_Mini,taCenter);
    Label_HouseHealth.FontColor := $FFE0E0E0;

    HealthBar_House := TKMPercentBar.Create(Panel_House,120,57,55,15);
    Label_House_UnderConstruction := TKMLabel.Create(Panel_House,0,110,TB_WIDTH,0,fTextLibrary[TX_HOUSE_UNDER_CONSTRUCTION],fnt_Grey,taCenter);

    Image_HouseConstructionWood  := TKMImage.Create(Panel_House,40,170,40,40,655);
    Image_HouseConstructionWood.ImageCenter;
    Image_HouseConstructionStone := TKMImage.Create(Panel_House,100,170,40,40,654);
    Image_HouseConstructionStone.ImageCenter;
    Label_HouseConstructionWood  := TKMLabel.Create(Panel_House,60,210,fResource.Resources[rt_Wood].Title,fnt_Grey,taCenter);
    Label_HouseConstructionStone := TKMLabel.Create(Panel_House,120,210,fResource.Resources[rt_Stone].Title,fnt_Grey,taCenter);

    Label_House_Demolish := TKMLabel.Create(Panel_House,0,130,TB_WIDTH,0,fTextLibrary[TX_HOUSE_DEMOLISH],fnt_Grey,taCenter);
    Label_House_Demolish.AutoWrap := True;
    Button_House_DemolishYes := TKMButton.Create(Panel_House,0,185,TB_WIDTH,30,fTextLibrary[TX_HOUSE_DEMOLISH_YES],bsGame);
    Button_House_DemolishNo  := TKMButton.Create(Panel_House,0,220,TB_WIDTH,30,fTextLibrary[TX_HOUSE_DEMOLISH_NO],bsGame);
    Button_House_DemolishYes.Hint := fTextLibrary[TX_HOUSE_DEMOLISH_YES_HINT];
    Button_House_DemolishNo.Hint  := fTextLibrary[TX_HOUSE_DEMOLISH_NO];
    Button_House_DemolishYes.OnClick := House_Demolish;
    Button_House_DemolishNo.OnClick  := House_Demolish;

    Panel_House_Common := TKMPanel.Create(Panel_House,0,76,200,310);
      Label_Common_Demand := TKMLabel.Create(Panel_House_Common,0,2,TB_WIDTH,0,fTextLibrary[TX_HOUSE_NEEDS],fnt_Grey,taCenter);
      Label_Common_Offer  := TKMLabel.Create(Panel_House_Common,0,2,TB_WIDTH,0,'',fnt_Grey,taCenter);
      Label_Common_Costs  := TKMLabel.Create(Panel_House_Common,0,2,TB_WIDTH,0,fTextLibrary[TX_HOUSE_GOOD_COST],fnt_Grey,taCenter);

      //They get repositioned on display
      for i:=1 to 4 do
      begin
        ResRow_Common_Resource[i] := TKMResourceRow.Create(Panel_House_Common, 0,22,TB_WIDTH,20);
        ResRow_Common_Resource[i].RX := rxGui;

        ResRow_Order[i] := TKMResourceOrderRow.Create(Panel_House_Common, 0,22,TB_WIDTH,20);
        ResRow_Order[i].RX := rxGui;
        ResRow_Order[i].OrderRem.OnClickEither := House_OrderClick;
        ResRow_Order[i].OrderAdd.OnClickEither := House_OrderClick;
        ResRow_Order[i].OrderRem.Hint          := fTextLibrary[TX_HOUSE_ORDER_DEC_HINT];
        ResRow_Order[i].OrderAdd.Hint          := fTextLibrary[TX_HOUSE_ORDER_INC_HINT];

        ResRow_Costs[i] := TKMCostsRow.Create(Panel_House_Common, 0,22,TB_WIDTH,20);
        ResRow_Costs[i].RX := rxGui;
      end;
end;


{Market page}
procedure TKMGamePlayInterface.Create_Market_Page;
var
  I: Integer;
  LineH: Integer;
begin
  Panel_HouseMarket := TKMPanel.Create(Panel_House, 0, 76, TB_WIDTH, 266);

  for I := 0 to STORE_RES_COUNT - 1 do
  begin
    Button_Market[I] := TKMButtonFlat.Create(Panel_HouseMarket, (I mod 6)*31, 12 + (I div 6) * MARKET_RES_HEIGHT, 26, 31, 0);
    Button_Market[I].TexOffsetY := 1;
    Button_Market[I].TexID := fResource.Resources[StoreResType[I+1]].GUIIcon;
    Button_Market[I].Hint := fResource.Resources[StoreResType[I+1]].Title;
    Button_Market[I].Tag := Byte(StoreResType[I+1]);
    Button_Market[I].OnClickEither := House_MarketSelect;
  end;

  Shape_Market_From := TKMShape.Create(Panel_HouseMarket, 0, 0, 26, 30);
  Shape_Market_From.LineColor := $FF00B000;
  Shape_Market_From.LineWidth := 2;
  Shape_Market_From.Hitable := False;
  Shape_Market_From.Hide;
  Shape_Market_To := TKMShape.Create(Panel_HouseMarket, 0, 0, 26, 30);
  Shape_Market_To.LineColor := $FF0000B0;
  Shape_Market_To.LineWidth := 2;
  Shape_Market_To.Hitable := False;
  Shape_Market_To.Hide;

  LineH := 12 + ((STORE_RES_COUNT - 1) div 6 + 1) * MARKET_RES_HEIGHT;
  Label_Market_In  := TKMLabel.Create(Panel_HouseMarket, 0,LineH,85,0,'',fnt_Grey,taLeft);
  Label_Market_Out := TKMLabel.Create(Panel_HouseMarket, TB_WIDTH - 85,LineH,85,0,'',fnt_Grey,taRight);

  Inc(LineH, 20);
  Button_Market_In  := TKMButtonFlat.Create(Panel_HouseMarket,  0, LineH, 36, 40, 0);
  Button_Market_In.HideHighlight := True;
  Button_Market_In.Disable;
  Button_Market_In.Hint := fTextLibrary[TX_HOUSES_MARKET_SELECT_LEFT];
  Button_Market_Out := TKMButtonFlat.Create(Panel_HouseMarket, TB_WIDTH - 36, LineH, 36, 40, 0);
  Button_Market_Out.Disable;
  Button_Market_Out.Hint := fTextLibrary[TX_HOUSES_MARKET_SELECT_RIGHT];

  with TKMShape.Create(Panel_HouseMarket,  0, LineH, 36, 40) do
  begin
    LineColor := $FF00B000;
    LineWidth := 2;
    Hitable := False;
  end;
  with TKMShape.Create(Panel_HouseMarket, TB_WIDTH - 36, LineH, 36, 40) do
  begin
    LineColor := $FF0000B0;
    LineWidth := 2;
    Hitable := False;
  end;

  Inc(LineH, 10);

  Label_Market_FromAmount := TKMLabel.Create(Panel_HouseMarket, 53, LineH, '', fnt_Grey, taCenter);
  Button_Market_Remove := TKMButton.Create(Panel_HouseMarket, TB_WIDTH div 2 - 20, LineH, 20, 20, '-', bsGame);
  Button_Market_Remove.OnClickEither := House_MarketOrderClick;
  Button_Market_Remove.Hint := fTextLibrary[TX_HOUSES_MARKET_HINT_REM];
  Button_Market_Add := TKMButton.Create(Panel_HouseMarket, TB_WIDTH div 2, LineH, 20, 20, '+', bsGame);
  Button_Market_Add.Hint := fTextLibrary[TX_HOUSES_MARKET_HINT_ADD];
  Button_Market_Add.OnClickEither := House_MarketOrderClick;
  Label_Market_ToAmount := TKMLabel.Create(Panel_HouseMarket, 127, LineH, '', fnt_Grey, taCenter);
end;


{Store page}
procedure TKMGamePlayInterface.Create_Store_Page;
var I: Integer;
begin
  Panel_HouseStore := TKMPanel.Create(Panel_House, 0, 76, TB_WIDTH, 266);
  for I := 1 to STORE_RES_COUNT do
    begin
      Button_Store[I] := TKMButtonFlat.Create(Panel_HouseStore, 2 + ((I-1)mod 5)*36, 19+((I-1)div 5)*42, 32, 36, 0);
      Button_Store[I].TexID := fResource.Resources[StoreResType[I]].GUIIcon;
      Button_Store[I].Tag := I;
      Button_Store[I].Hint := fResource.Resources[StoreResType[I]].Title;
      Button_Store[I].OnClick := House_StoreAcceptFlag;

      Image_Store_Accept[I] := TKMImage.Create(Panel_HouseStore, 2 + ((I-1)mod 5)*36+20,18+((I-1)div 5)*42+1,12,12,49);
      Image_Store_Accept[I].Hitable := False;
    end;
end;


{School page}
procedure TKMGamePlayInterface.Create_School_Page;
var I: Integer;
begin
  Panel_House_School := TKMPanel.Create(Panel_House, 0, 76, TB_WIDTH, 266);

    TKMLabel.Create(Panel_House_School,0,2,TB_WIDTH,30,fTextLibrary[TX_HOUSE_NEEDS],fnt_Grey,taCenter);

    ResRow_School_Resource := TKMResourceRow.Create(Panel_House_School, 0,22,TB_WIDTH,20);
    ResRow_School_Resource.RX := rxGui;
    ResRow_School_Resource.TexID := fResource.Resources[rt_Gold].GUIIcon;
    ResRow_School_Resource.Caption := fResource.Resources[rt_Gold].Title;
    ResRow_School_Resource.Hint := fResource.Resources[rt_Gold].Title;

    Button_School_UnitWIP := TKMButton.Create(Panel_House_School,  0,48,32,32,0, rxGui, bsGame);
    Button_School_UnitWIP.Hint := fTextLibrary[TX_HOUSE_SCHOOL_WIP_HINT];
    Button_School_UnitWIP.Tag := 0;
    Button_School_UnitWIP.OnClickEither := House_SchoolUnitRemove;
    Button_School_UnitWIPBar := TKMPercentBar.Create(Panel_House_School,34,54,146,20);
    for I := 1 to 5 do
    begin
      Button_School_UnitPlan[i] := TKMButtonFlat.Create(Panel_House_School, (I-1) * 36, 80, 32, 32, 0);
      Button_School_UnitPlan[i].Tag := I;
      Button_School_UnitPlan[i].OnClickEither := House_SchoolUnitRemove;
    end;

    Label_School_Unit := TKMLabel.Create(Panel_House_School,   0,116,TB_WIDTH,30,'',fnt_Outline,taCenter);
    Image_School_Left := TKMImage.Create(Panel_House_School,   0,136,54,80,521);
    Image_School_Train := TKMImage.Create(Panel_House_School, 62,136,54,80,522);
    Image_School_Right := TKMImage.Create(Panel_House_School,124,136,54,80,523);
    Image_School_Left.Disable;
    Image_School_Right.Disable;
    Button_School_Left  := TKMButton.Create(Panel_House_School,  0,222,54,40,35, rxGui, bsGame);
    Button_School_Train := TKMButton.Create(Panel_House_School, 62,222,54,40,42, rxGui, bsGame);
    Button_School_Right := TKMButton.Create(Panel_House_School,124,222,54,40,36, rxGui, bsGame);
    Button_School_Left.OnClickEither:=House_SchoolUnitChange;
    Button_School_Train.OnClickEither:=House_SchoolUnitChange;
    Button_School_Right.OnClickEither:=House_SchoolUnitChange;
    Button_School_Left.Hint :=fTextLibrary[TX_HOUSE_SCHOOL_PREV_HINT];
    Button_School_Train.Hint:=fTextLibrary[TX_HOUSE_SCHOOL_TRAIN_HINT];
    Button_School_Right.Hint:=fTextLibrary[TX_HOUSE_SCHOOL_NEXT_HINT];
end;


{Barracks page}
procedure TKMGamePlayInterface.Create_Barracks_Page;
var I: Integer;
begin
  Panel_HouseBarracks := TKMPanel.Create(Panel_House, 0, 76, TB_WIDTH, 266);
    for I := 1 to BARRACKS_RES_COUNT do
    begin
      Button_Barracks[I] := TKMButtonFlat.Create(Panel_HouseBarracks, ((I-1)mod 6)*31, 8+((I-1)div 6)*42, 28, 38, 0);
      Button_Barracks[I].TexOffsetX := 1;
      Button_Barracks[I].TexOffsetY := 1;
      Button_Barracks[I].CapOffsetY := 2;
      Button_Barracks[I].HideHighlight := True;
      Button_Barracks[I].TexID := fResource.Resources[BarracksResType[I]].GUIIcon;
      Button_Barracks[I].Hint := fResource.Resources[BarracksResType[I]].Title;
    end;

    Button_BarracksRecruit := TKMButtonFlat.Create(Panel_HouseBarracks, ((BARRACKS_RES_COUNT)mod 6)*31,8+((BARRACKS_RES_COUNT)div 6)*42,28,38,0);
    Button_BarracksRecruit.TexOffsetX := 1;
    Button_BarracksRecruit.TexOffsetY := 1;
    Button_BarracksRecruit.CapOffsetY := 2;
    Button_BarracksRecruit.HideHighlight := True;
    Button_BarracksRecruit.TexID := fResource.UnitDat[ut_Recruit].GUIIcon;
    Button_BarracksRecruit.Hint := fResource.UnitDat[ut_Recruit].UnitName;

    Label_Barracks_Unit:=TKMLabel.Create(Panel_HouseBarracks, 0, 96, TB_WIDTH, 0, '', fnt_Outline, taCenter);

    Image_Barracks_Left :=TKMImage.Create(Panel_HouseBarracks,  0,116,54,80,535);
    Image_Barracks_Left.Disable;
    Image_Barracks_Train:=TKMImage.Create(Panel_HouseBarracks, 62,116,54,80,536);
    Image_Barracks_Right:=TKMImage.Create(Panel_HouseBarracks,124,116,54,80,537);
    Image_Barracks_Right.Disable;

    Button_Barracks_Left :=TKMButton.Create(Panel_HouseBarracks,  0,222,54,40,35, rxGui, bsGame);
    Button_Barracks_Train:=TKMButton.Create(Panel_HouseBarracks, 62,222,54,40,42, rxGui, bsGame);
    Button_Barracks_Right:=TKMButton.Create(Panel_HouseBarracks,124,222,54,40,36, rxGui, bsGame);
    Button_Barracks_Left.OnClickEither:=House_BarracksUnitChange;
    Button_Barracks_Train.OnClickEither:=House_BarracksUnitChange;
    Button_Barracks_Right.OnClickEither:=House_BarracksUnitChange;
    Button_Barracks_Left.Hint :=fTextLibrary[TX_HOUSE_BARRACKS_PREV_HINT];
    Button_Barracks_Train.Hint:=fTextLibrary[TX_HOUSE_BARRACKS_TRAIN_HINT];
    Button_Barracks_Right.Hint:=fTextLibrary[TX_HOUSE_BARRACKS_NEXT_HINT];
    Button_Barracks_Train.Disable;
end;


{Woodcutter page}
procedure TKMGamePlayInterface.Create_Woodcutter_Page;
begin
  Panel_HouseWoodcutter:=TKMPanel.Create(Panel_House,TB_PAD,76,TB_WIDTH,266);
    Button_Woodcutter := TKMButtonFlat.Create(Panel_HouseWoodcutter,0,64,32,32,51,rxGui);
    Button_Woodcutter.OnClick := House_WoodcutterChange; //Clicking the button cycles it

    Radio_Woodcutter := TKMRadioGroup.Create(Panel_HouseWoodcutter,38,64,TB_WIDTH-38,32,fnt_Grey);
    Radio_Woodcutter.ItemIndex := 0;
    Radio_Woodcutter.Items.Add(fTextLibrary[TX_HOUSES_WOODCUTTER_PLANT_CHOP]);
    Radio_Woodcutter.Items.Add(fTextLibrary[TX_HOUSES_WOODCUTTER_CHOP_ONLY]);
    Radio_Woodcutter.OnChange := House_WoodcutterChange;
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
  fSoundLib.Play(sfxn_MPChatOpen);
  fMyControls.CtrlFocus := Edit_ChatMsg;
  Allies_Close(nil);
  Panel_Chat.Show;
  Message_Close(nil);
  if fGame.Networking.NetPlayers[fGame.Networking.MyIndex].Team = 0 then
  begin
    CheckBox_SendToAllies.Checked := false;
    CheckBox_SendToAllies.Enabled := false;
  end
  else
    CheckBox_SendToAllies.Enabled := true;
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
  fSoundLib.Play(sfxn_MPChatOpen);
  Panel_Allies.Show;
  Chat_Close(nil);
  Message_Close(nil);
end;


//Click on the same message again closes it
procedure TKMGamePlayInterface.Message_Click(Sender: TObject);
begin
  if TKMImage(Sender).Tag <> ShownMessage then
    Message_Display(TKMImage(Sender).Tag)
  else
    Message_Close(Sender);
end;


procedure TKMGamePlayInterface.Message_Display(aIndex: Integer);
var I: Integer;
begin
  ShownMessage := aIndex;

  for I := 0 to MAX_VISIBLE_MSGS do
    Image_Message[I].Highlight := (ShownMessage = I);

  Label_MessageText.Caption := fMessageList[ShownMessage].Text;
  Button_MessageGoTo.Enabled := fMessageList[ShownMessage].IsGoto;

  Allies_Close(nil);
  Chat_Close(nil); //Removes focus from Edit_Text
  Panel_Message.Show;
  fSoundLib.Play(sfx_MessageOpen); //Play parchment sound when they open the message
end;


procedure TKMGamePlayInterface.Message_Close(Sender: TObject);
begin
  if ShownMessage <> -1 then
  begin
    Image_Message[ShownMessage].Highlight := False;
    fSoundLib.Play(sfx_MessageClose);
  end;
  ShownMessage := -1;
  Panel_Message.Hide;
end;


procedure TKMGamePlayInterface.Message_Delete(Sender: TObject);
begin
  if ShownMessage = -1 then Exit; //Player pressed DEL with no Msg opened

  fMessageList.RemoveEntry(ShownMessage);
  Message_Close(Sender);
  Message_UpdateStack;
  DisplayHint(nil);
end;


procedure TKMGamePlayInterface.Message_GoTo(Sender: TObject);
begin
  fGame.Viewport.Position := KMPointF(fMessageList[ShownMessage].Loc);
end;


procedure TKMGamePlayInterface.Message_UpdateStack;
var I: Integer;
begin
  //MassageList is unlimited, while Image_Message has fixed depth and samples data from the list on demand
  for I := 0 to MAX_VISIBLE_MSGS do
  begin
    //Disable and hide at once for safety
    Image_Message[I].Enabled := (I <= fMessageList.Count - 1);
    Image_Message[I].Visible := (I <= fMessageList.Count - 1);
    if I <= fMessageList.Count - 1 then
      Image_Message[i].TexID := fMessageList[I].Icon;
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
  for i:=1 to Panel_Build.ChildCount do
    if Panel_Build.Childs[i] is TKMButtonFlat then
      TKMButtonFlat(Panel_Build.Childs[i]).Down := false;

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
    Label_Build.Caption := fTextLibrary[TX_BUILD_DEMOLISH];
  end;
  if Button_BuildRoad.Down then begin
    GameCursor.Mode:=cmRoad;
    Image_Build_Selected.TexID := 335;
    Label_BuildCost_Stone.Caption:='1';
    Label_Build.Caption := fTextLibrary[TX_BUILD_ROAD];
  end;
  if Button_BuildField.Down then begin
    GameCursor.Mode:=cmField;
    Image_Build_Selected.TexID := 337;
    Label_Build.Caption := fTextLibrary[TX_BUILD_FIELD];
  end;
  if Button_BuildWine.Down then begin
    GameCursor.Mode:=cmWine;
    Image_Build_Selected.TexID := 336;
    Label_BuildCost_Wood.Caption:='1';
    Label_Build.Caption := fTextLibrary[TX_BUILD_WINE];
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


procedure TKMGamePlayInterface.ShowHouseInfo(Sender: TKMHouse; aAskDemolish: Boolean = False);
const LineAdv = 25; //Each new Line is placed ## pixels after previous
var i,RowRes,Base,Line:integer; Res:TResourceType;
begin
  Assert(fPlayers.Selected = Sender);
  fAskDemolish := aAskDemolish;

  if Sender = nil then
  begin
    SwitchPage(nil);
    Exit;
  end;

  {Common data}
  Label_House.Caption       := fResource.HouseDat[Sender.HouseType].HouseName;
  Image_House_Logo.TexID    := fResource.HouseDat[Sender.HouseType].GUIIcon;
  Image_House_Worker.TexID  := fResource.UnitDat[fResource.HouseDat[Sender.HouseType].OwnerType].GUIIcon;
  Image_House_Worker.Hint   := fResource.UnitDat[fResource.HouseDat[Sender.HouseType].OwnerType].UnitName;
  Image_House_Worker.FlagColor := fPlayers[Sender.Owner].FlagColor;
  HealthBar_House.Caption   := inttostr(round(Sender.GetHealth))+'/'+inttostr(fResource.HouseDat[Sender.HouseType].MaxHealth);
  HealthBar_House.Position  := Sender.GetHealth / fResource.HouseDat[Sender.HouseType].MaxHealth;

  if fAskDemolish then
  begin
    for i:=1 to Panel_House.ChildCount do
      Panel_House.Childs[i].Hide; //hide all
    Label_House_Demolish.Show;
    Button_House_DemolishYes.Show;
    Button_House_DemolishNo.Show;
    Label_House.Show;
    Image_House_Logo.Show;
    Image_House_Worker.Show;
    Image_House_Worker.Enable;
    HealthBar_House.Show;
    Label_HouseHealth.Show;
    SwitchPage(Panel_House);
    exit;
  end;

  if not Sender.IsComplete then
  begin
    for i:=1 to Panel_House.ChildCount do
      Panel_House.Childs[i].Hide; //hide all
    Label_House_UnderConstruction.Show;
    Image_HouseConstructionWood.Show;
    Image_HouseConstructionStone.Show;
    Label_HouseConstructionWood.Show;
    Label_HouseConstructionStone.Show;
    Label_HouseConstructionWood.Caption := IntToStr(Sender.GetBuildWoodDelivered)+' / '+IntToStr(fResource.HouseDat[Sender.HouseType].WoodCost);
    Label_HouseConstructionStone.Caption := IntToStr(Sender.GetBuildStoneDelivered)+' / '+IntToStr(fResource.HouseDat[Sender.HouseType].StoneCost);
    Label_House.Show;
    Image_House_Logo.Show;
    Image_House_Worker.Show;
    Image_House_Worker.Enable;
    HealthBar_House.Show;
    Label_HouseHealth.Show;
    SwitchPage(Panel_House);
    exit;
  end;


  for i:=1 to Panel_House.ChildCount do
    Panel_House.Childs[i].Show; //show all

  Image_House_Worker.Enabled := Sender.GetHasOwner;
  Image_House_Worker.Visible := fResource.HouseDat[Sender.HouseType].OwnerType <> ut_None;
  Button_House_Goods.Enabled := fResource.HouseDat[Sender.HouseType].AcceptsGoods;
  if Sender.BuildingRepair then Button_House_Repair.TexID:=39 else Button_House_Repair.TexID:=40;
  if Sender.WareDelivery then Button_House_Goods.TexID:=37 else Button_House_Goods.TexID:=38;
  Label_House_UnderConstruction.Hide;
  Image_HouseConstructionWood.Hide;
  Image_HouseConstructionStone.Hide;
  Label_HouseConstructionWood.Hide;
  Label_HouseConstructionStone.Hide;
  Label_House_Demolish.Hide;
  Button_House_DemolishYes.Hide;
  Button_House_DemolishNo.Hide;
  SwitchPage(Panel_House);

  case Sender.HouseType of
    ht_Marketplace:
        begin
          House_MarketFill(TKMHouseMarket(Sender));
          SwitchPage(Panel_HouseMarket);
        end;

    ht_Store:
        begin
          House_StoreFill;
          SwitchPage(Panel_HouseStore);
        end;

    ht_School:
        begin
          ResRow_School_Resource.ResourceCount := Sender.CheckResIn(rt_Gold) - byte(TKMHouseSchool(Sender).HideOneGold);
          Button_School_UnitWIP.FlagColor := fPlayers[Sender.Owner].FlagColor;
          for I := 1 to 5 do
            Button_School_UnitPlan[I].FlagColor := fPlayers[Sender.Owner].FlagColor;
          Image_School_Left.FlagColor  := fPlayers[Sender.Owner].FlagColor;
          Image_School_Right.FlagColor := fPlayers[Sender.Owner].FlagColor;
          Image_School_Train.FlagColor := fPlayers[Sender.Owner].FlagColor;
          House_SchoolUnitChange(nil, mbLeft);
          SwitchPage(Panel_House_School);
        end;

    ht_Barracks:
        begin
          Image_House_Worker.Enable; //In the barrack the recruit icon is always enabled
          Image_Barracks_Left.FlagColor := fPlayers[Sender.Owner].FlagColor;
          Image_Barracks_Right.FlagColor := fPlayers[Sender.Owner].FlagColor;
          Image_Barracks_Train.FlagColor := fPlayers[Sender.Owner].FlagColor;
          Button_BarracksRecruit.FlagColor := fPlayers[Sender.Owner].FlagColor;
          House_BarracksUnitChange(nil, mbLeft);
          SwitchPage(Panel_HouseBarracks);
        end;

    ht_Woodcutters:
        begin
          House_WoodcutterChange(nil);
          SwitchPage(Panel_HouseWoodcutter);

          //First thing - hide everything
          for i:=1 to Panel_House_Common.ChildCount do
            Panel_House_Common.Childs[i].Hide;

          Label_Common_Offer.Show;
          Label_Common_Offer.Caption := fTextLibrary[TX_HOUSE_DELIVERS]+'(x'+inttostr(fResource.HouseDat[Sender.HouseType].ResProductionX)+'):';
          Label_Common_Offer.Top := 8;

          ResRow_Common_Resource[1].TexID := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[1]].GUIIcon;
          ResRow_Common_Resource[1].ResourceCount := Sender.CheckResOut(fResource.HouseDat[Sender.HouseType].ResOutput[1]);
          ResRow_Common_Resource[1].Caption := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[1]].Title;
          ResRow_Common_Resource[1].Hint := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[1]].Title;
          ResRow_Common_Resource[1].Show;
          ResRow_Common_Resource[1].Top := 2+LineAdv;
        end;
    ht_TownHall:;
    else
        begin
          //First thing - hide everything
          for i:=1 to Panel_House_Common.ChildCount do
            Panel_House_Common.Childs[i].Hide;

          //Now show only what we need
          RowRes := 1; Line := 0; Base := 2;

          //Show Demand
          if fResource.HouseDat[Sender.HouseType].AcceptsGoods then
          begin
            Label_Common_Demand.Show;
            Label_Common_Demand.Top := Base+Line*LineAdv+6;
            inc(Line);

            for i:=1 to 4 do
            if fResource.Resources[fResource.HouseDat[Sender.HouseType].ResInput[i]].IsValid then
            begin
              ResRow_Common_Resource[RowRes].TexID := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResInput[i]].GUIIcon;
              ResRow_Common_Resource[RowRes].Caption := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResInput[i]].Title;
              ResRow_Common_Resource[RowRes].Hint := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResInput[i]].Title;
              ResRow_Common_Resource[RowRes].ResourceCount := Sender.CheckResIn(fResource.HouseDat[Sender.HouseType].ResInput[i]);
              ResRow_Common_Resource[RowRes].Top := Base+Line*LineAdv;
              ResRow_Common_Resource[RowRes].Show;
              inc(Line);
              inc(RowRes);
            end;
          end;

          //Show Output
          if not fResource.HouseDat[Sender.HouseType].DoesOrders then
          if fResource.HouseDat[Sender.HouseType].ProducesGoods then begin
            Label_Common_Offer.Show;
            Label_Common_Offer.Caption := fTextLibrary[TX_HOUSE_DELIVERS]+'(x'+inttostr(fResource.HouseDat[Sender.HouseType].ResProductionX)+'):';
            Label_Common_Offer.Top := Base+Line*LineAdv+6;
            inc(Line);

            for i:=1 to 4 do
            if fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[i]].IsValid then
            begin
              ResRow_Common_Resource[RowRes].TexID := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[i]].GUIIcon;
              ResRow_Common_Resource[RowRes].ResourceCount := Sender.CheckResOut(fResource.HouseDat[Sender.HouseType].ResOutput[i]);
              ResRow_Common_Resource[RowRes].Caption := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[i]].Title;
              ResRow_Common_Resource[RowRes].Hint := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[i]].Title;
              ResRow_Common_Resource[RowRes].Show;
              ResRow_Common_Resource[RowRes].Top := Base+Line*LineAdv;
              inc(Line);
              inc(RowRes);
            end;
          end;

          //Show Orders
          if fResource.HouseDat[Sender.HouseType].DoesOrders then
          begin
            Label_Common_Offer.Show;
            Label_Common_Offer.Caption:=fTextLibrary[TX_HOUSE_DELIVERS]+'(x'+inttostr(fResource.HouseDat[Sender.HouseType].ResProductionX)+'):';
            Label_Common_Offer.Top:=Base+Line*LineAdv+6;
            inc(Line);
            for i:=1 to 4 do //Orders
            if fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[i]].IsValid then
            begin
              ResRow_Order[i].TexID := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[i]].GUIIcon;
              ResRow_Order[i].Caption := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[i]].Title;
              ResRow_Order[i].Hint := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[i]].Title;
              ResRow_Order[i].ResourceCount := Sender.CheckResOut(fResource.HouseDat[Sender.HouseType].ResOutput[i]);
              ResRow_Order[i].OrderCount := Sender.CheckResOrder(i);
              ResRow_Order[i].Show;
              ResRow_Order[i].Top := Base+Line*LineAdv;
              inc(Line);
            end;
            Label_Common_Costs.Show;
            Label_Common_Costs.Top:=Base+Line*LineAdv+2;
            inc(Line);
            for i:=1 to 4 do //Costs
            begin
              Res := fResource.HouseDat[Sender.HouseType].ResOutput[i];
              if fResource.Resources[Res].IsValid then
              begin
                ResRow_Costs[i].Caption := fResource.Resources[Res].Title;
                ResRow_Costs[i].RX := rxGui;
                //Hide the icons when they are not used
                if WarfareCosts[Res, 1] = rt_None then ResRow_Costs[i].TexID1 := 0
                else ResRow_Costs[i].TexID1 := fResource.Resources[WarfareCosts[Res, 1]].GUIIcon;
                if WarfareCosts[Res, 2] = rt_None then ResRow_Costs[i].TexID2 := 0
                else ResRow_Costs[i].TexID2 := fResource.Resources[WarfareCosts[Res, 2]].GUIIcon;

                ResRow_Costs[i].Show;
                ResRow_Costs[i].Top := Base + Line * LineAdv - 2*i - 6; //Pack them closer so they fit on 1024x576
                inc(Line);
              end;
            end;
          end;
          SwitchPage(Panel_House_Common);
        end;
  end;
end;


procedure TKMGamePlayInterface.ShowUnitInfo(Sender: TKMUnit; aAskDismiss: Boolean = False);
begin
  Assert(fPlayers.Selected = Sender);

  fAskDismiss  := aAskDismiss;

  if Sender = nil then
  begin
    SwitchPage(nil);
    Exit;
  end;

  SwitchPage(Panel_Unit);

  //Common properties
  Label_UnitName.Caption      := fResource.UnitDat[Sender.UnitType].UnitName;
  Image_UnitPic.TexID         := fResource.UnitDat[Sender.UnitType].GUIScroll;
  Image_UnitPic.FlagColor     := fPlayers[Sender.Owner].FlagColor;
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
  Assert(fPlayers.Selected = Sender);

  if (Sender = nil) or (Sender.SelectedUnit = nil) then
  begin
    SwitchPage(nil);
    Exit;
  end;

  W := Sender.SelectedUnit;
  SwitchPage(Panel_Unit);

  //Common properties
  Label_UnitName.Caption      := fResource.UnitDat[W.UnitType].UnitName;
  Image_UnitPic.TexID         := fResource.UnitDat[W.UnitType].GUIScroll;
  Image_UnitPic.FlagColor     := fPlayers[W.Owner].FlagColor;
  ConditionBar_Unit.Position  := W.Condition / UNIT_MAX_CONDITION;
  Label_UnitTask.Caption      := Sender.GetOrderText + '|' + W.GetActivityText;

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
    ImageStack_Army.SetCount(Sender.Count, Sender.UnitsPerRow, Sender.UnitsPerRow div 2 + 1);
    Army_ActivateControls(Sender);
  end;
end;


procedure TKMGamePlayInterface.House_Demolish(Sender: TObject);
begin
  if (fPlayers.Selected = nil) or not (fPlayers.Selected is TKMHouse) then Exit;

  if Sender = Button_House_DemolishYes then
  begin
    fGame.GameInputProcess.CmdBuild(gic_BuildRemoveHouse, TKMHouse(fPlayers.Selected).GetPosition);
    fPlayers.Selected := nil; //fPlayers.Selected MUST be reset before calling ShowHouseInfo
    ShowHouseInfo(nil, False); //Simpliest way to reset page and ShownHouse
  end else
    fAskDemolish := False;

  SwitchPage(Button_Main[tbBuild]); //Return to build menu
end;


procedure TKMGamePlayInterface.House_RepairToggle(Sender: TObject);
begin
  if (fPlayers.Selected = nil) or not (fPlayers.Selected is TKMHouse) then Exit;

  fGame.GameInputProcess.CmdHouse(gic_HouseRepairToggle, TKMHouse(fPlayers.Selected));
  Button_House_Repair.TexID := IfThen(TKMHouse(fPlayers.Selected).BuildingRepair, 39, 40);
end;


procedure TKMGamePlayInterface.House_WareDeliveryToggle(Sender: TObject);
begin
  if (fPlayers.Selected = nil) or not (fPlayers.Selected is TKMHouse) then Exit;

  fGame.GameInputProcess.CmdHouse(gic_HouseDeliveryToggle, TKMHouse(fPlayers.Selected));
  Button_House_Goods.TexID := IfThen(TKMHouse(fPlayers.Selected).WareDelivery, 37, 38);
end;


procedure TKMGamePlayInterface.House_OrderClick(Sender: TObject; AButton: TMouseButton);
var
  I: Integer;
  H: TKMHouse;
begin
  if not (fPlayers.Selected is TKMHouse) then Exit;

  H := TKMHouse(fPlayers.Selected);

  for i:=1 to 4 do begin
    if Sender = ResRow_Order[i].OrderRem then
      fGame.GameInputProcess.CmdHouse(gic_HouseOrderProduct, H, i, -ClickAmount[AButton]);
    if Sender = ResRow_Order[i].OrderAdd then
      fGame.GameInputProcess.CmdHouse(gic_HouseOrderProduct, H, i, ClickAmount[AButton]);
  end;
end;


procedure TKMGamePlayInterface.House_WoodcutterChange(Sender:TObject);
var
  W: TKMHouseWoodcutters;
  WMode: TWoodcutterMode;
begin
  W := TKMHouseWoodcutters(fPlayers.Selected);
  if Sender = Button_Woodcutter then
    Radio_Woodcutter.ItemIndex := (Radio_Woodcutter.ItemIndex + 1) mod 2; //Cycle

  if (Sender = Button_Woodcutter) or (Sender = Radio_Woodcutter) then
  begin
    if Radio_Woodcutter.ItemIndex = 0 then
      WMode := wcm_ChopAndPlant
    else
      WMode := wcm_Chop;
    fGame.GameInputProcess.CmdHouse(gic_HouseWoodcutterMode, W, WMode);
  end;

  case W.WoodcutterMode of
    wcm_ChopAndPlant: begin
                        Button_Woodcutter.TexID := 310;
                        Button_Woodcutter.RX := rxGui;
                        Radio_Woodcutter.ItemIndex := 0;
                      end;
    wcm_Chop:         begin
                        Button_Woodcutter.TexID := 51;
                        Button_Woodcutter.RX := rxGui;
                        Radio_Woodcutter.ItemIndex := 1;
                      end;
  end;
end;


procedure TKMGamePlayInterface.House_BarracksUnitChange(Sender:TObject; AButton:TMouseButton);
var i, k, Tmp: integer; Barracks:TKMHouseBarracks;
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouseBarracks) then exit;

  Barracks:=TKMHouseBarracks(fPlayers.Selected);

  if (Sender=Button_Barracks_Left) and (AButton = mbRight) then LastBarracksUnit := 0;
  if (Sender=Button_Barracks_Right) and (AButton = mbRight) then LastBarracksUnit := High(Barracks_Order);

  if (Sender=Button_Barracks_Left)and(LastBarracksUnit > 0) then dec(LastBarracksUnit);
  if (Sender=Button_Barracks_Right)and(LastBarracksUnit < High(Barracks_Order)) then inc(LastBarracksUnit);

  if Sender=Button_Barracks_Train then //Equip unit
    if AButton = mbLeft then
      fGame.GameInputProcess.CmdHouse(gic_HouseBarracksEquip, Barracks, Barracks_Order[LastBarracksUnit], 1)
    else if AButton = mbRight then
      fGame.GameInputProcess.CmdHouse(gic_HouseBarracksEquip, Barracks, Barracks_Order[LastBarracksUnit], 10);

  for i:=1 to BARRACKS_RES_COUNT do begin
    Tmp := Barracks.CheckResIn(BarracksResType[i]);
    Button_Barracks[i].Caption := IfThen(Tmp = 0, '-', inttostr(Tmp));
    //Set highlights
    Button_Barracks[i].Down := False;
    for k:=1 to 4 do
      if BarracksResType[i] = TroopCost[Barracks_Order[LastBarracksUnit],k] then
        Button_Barracks[i].Down := True;
  end;

  Tmp := Barracks.RecruitsList.Count;
  Button_BarracksRecruit.Caption := IfThen(Tmp = 0, '-', inttostr(Tmp));
  Button_BarracksRecruit.Down := True; //Recruit is always enabled, all troops require one

  Button_Barracks_Train.Enabled := Barracks.CanEquip(Barracks_Order[LastBarracksUnit]);
  Button_Barracks_Left.Enabled := LastBarracksUnit > 0;
  Button_Barracks_Right.Enabled := LastBarracksUnit < High(Barracks_Order);
  Image_Barracks_Left.Visible:= Button_Barracks_Left.Enabled;
  Image_Barracks_Right.Visible:= Button_Barracks_Right.Enabled;

  if LastBarracksUnit > 0 then
    Image_Barracks_Left.TexID := fResource.UnitDat[Barracks_Order[LastBarracksUnit-1]].GUIScroll;

  Label_Barracks_Unit.Caption := fResource.UnitDat[Barracks_Order[LastBarracksUnit]].UnitName;
  Image_Barracks_Train.TexID := fResource.UnitDat[Barracks_Order[LastBarracksUnit]].GUIScroll;

  if LastBarracksUnit < High(Barracks_Order) then
    Image_Barracks_Right.TexID := fResource.UnitDat[Barracks_Order[LastBarracksUnit+1]].GUIScroll;
end;


{Process click on Left-Train-Right buttons of School}
procedure TKMGamePlayInterface.House_SchoolUnitChange(Sender:TObject; AButton:TMouseButton);
var
  I: Byte;
  School: TKMHouseSchool;
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouseSchool) then exit;
  School := TKMHouseSchool(fPlayers.Selected);

  if (AButton = mbRight) and (Sender=Button_School_Left) then LastSchoolUnit := 0;
  if (AButton = mbRight) and (Sender=Button_School_Right) then LastSchoolUnit := High(School_Order);

  if (Sender=Button_School_Left)and(LastSchoolUnit > 0) then dec(LastSchoolUnit);
  if (Sender=Button_School_Right)and(LastSchoolUnit < High(School_Order)) then inc(LastSchoolUnit);

  if Sender=Button_School_Train then //Add unit to training queue
    if AButton = mbLeft then
      fGame.GameInputProcess.CmdHouse(gic_HouseSchoolTrain, School, School_Order[LastSchoolUnit], 1)
    else if AButton = mbRight then
      fGame.GameInputProcess.CmdHouse(gic_HouseSchoolTrain, School, School_Order[LastSchoolUnit], 6);

  if School.Queue[0] <> ut_None then
    Button_School_UnitWIP.TexID := fResource.UnitDat[School.Queue[0]].GUIIcon
  else
    Button_School_UnitWIP.TexID := 41; //Question mark

  Button_School_UnitWIPBar.Position := School.GetTrainingProgress;

  for I := 1 to 5 do
    if School.Queue[I] <> ut_None then
    begin
      Button_School_UnitPlan[I].TexID := fResource.UnitDat[School.Queue[I]].GUIIcon;
      Button_School_UnitPlan[I].Hint := fResource.UnitDat[School.Queue[I]].UnitName;
    end
    else
    begin
      Button_School_UnitPlan[I].TexID:=0;
      Button_School_UnitPlan[I].Hint:='';
    end;

  Button_School_Train.Enabled := School.Queue[High(School.Queue)] = ut_None;
  Button_School_Left.Enabled := LastSchoolUnit > 0;
  Button_School_Right.Enabled := LastSchoolUnit < High(School_Order);
  Image_School_Left.Visible:= Button_School_Left.Enabled;
  Image_School_Right.Visible:= Button_School_Right.Enabled;

  if LastSchoolUnit > 0 then
    Image_School_Left.TexID:= fResource.UnitDat[School_Order[LastSchoolUnit-1]].GUIScroll;

  Label_School_Unit.Caption:=fResource.UnitDat[School_Order[LastSchoolUnit]].UnitName;
  Image_School_Train.TexID:=fResource.UnitDat[School_Order[LastSchoolUnit]].GUIScroll;

  if LastSchoolUnit < High(School_Order) then
    Image_School_Right.TexID:=fResource.UnitDat[School_Order[LastSchoolUnit+1]].GUIScroll;
end;


{Process click on Remove-from-queue buttons of School}
procedure TKMGamePlayInterface.House_SchoolUnitRemove(Sender: TObject; AButton: TMouseButton);
var
  School: TKMHouseSchool;
  i, ID:Integer;
begin
  School := TKMHouseSchool(fPlayers.Selected);
  ID := TKMControl(Sender).Tag; //Item number that was clicked from the school queue

  if not (ID in [0..High(School.Queue)]) then Exit;

  //Right click clears entire queue after this item.
  //In that case we remove the same ID repeatedly because they're automatically move along
  case AButton of
    mbLeft:  fGame.GameInputProcess.CmdHouse(gic_HouseRemoveTrain, School, ID);
    mbRight: for i:=ID to High(School.Queue) do
               fGame.GameInputProcess.CmdHouse(gic_HouseRemoveTrain, School, ID);
  end;
  House_SchoolUnitChange(nil, mbLeft);
end;


{That small red triangle blocking delivery of goods to Storehouse}
{Resource determined by Button.Tag property}
procedure TKMGamePlayInterface.House_StoreAcceptFlag(Sender:TObject);
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouseStore) then exit;
  fGame.GameInputProcess.CmdHouse(gic_HouseStoreAcceptFlag, TKMHouse(fPlayers.Selected), StoreResType[(Sender as TKMControl).Tag]);
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

  fSoundLib.UpdateSoundVolume(fGameApp.GameSettings.SoundFXVolume);
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
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMUnitGroup) then Exit;

  {Not implemented yet
  if Sender = Button_Unit_Dismiss then
  begin
    ShowUnitInfo(TKMUnit(fPlayers.Selected), true);
  end;}

  Group := TKMUnitGroup(fPlayers.Selected);

  //if Sender = Button_Army_GoTo    then ; //This command makes no sense unless player has no right-mouse-button
  if Sender = Button_Army_Stop    then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyHalt, Group);
    fSoundLib.PlayWarrior(Group.UnitType, sp_Halt);
  end;
  //if Sender = Button_Army_Attack  then ; //This command makes no sense unless player has no right-mouse-button
  if Sender = Button_Army_RotCW   then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyFormation, Group, tdCW, 0);
    fSoundLib.PlayWarrior(Group.UnitType, sp_RotRight);
  end;
  if Sender = Button_Army_Storm   then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyStorm, Group);
    fSoundLib.PlayWarrior(Group.UnitType, sp_StormAttack);
  end;
  if Sender = Button_Army_RotCCW  then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyFormation, Group, tdCCW, 0);
    fSoundLib.PlayWarrior(Group.UnitType, sp_RotLeft);
  end;
  if Sender = Button_Army_ForDown then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyFormation, Group, tdNone, 1);
    fSoundLib.PlayWarrior(Group.UnitType, sp_Formation);
  end;
  if Sender = Button_Army_ForUp   then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyFormation, Group, tdNone, -1);
    fSoundLib.PlayWarrior(Group.UnitType, sp_Formation);
  end;
  if Sender = Button_Army_Split   then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmySplit, Group);
    fSoundLib.PlayWarrior(Group.UnitType, sp_Split);
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
    fSoundLib.PlayWarrior(Group.UnitType, sp_Eat);
  end;
end;


procedure TKMGamePlayInterface.Unit_Dismiss(Sender:TObject);
begin
  if fPlayers.Selected = nil then exit;
    if not (fPlayers.Selected is TKMUnit) then exit;

    if Sender=Button_Unit_DismissYes then begin
      //DISMISS UNIT
      fAskDismiss:=false;
      ShowUnitInfo(nil, false); //Simpliest way to reset page and ShownUnit
      SwitchPage(nil); //Return to main menu after dismissing
    end else begin
      fAskDismiss:=false;
      ShowUnitInfo(TKMUnit(fPlayers.Selected), false);  //Cancel and return to selected unit
    end;
end;


procedure TKMGamePlayInterface.Army_HideJoinMenu(Sender: TObject);
begin
  fJoiningGroups := False;
  if fResource.Cursors.Cursor in [kmc_JoinYes, kmc_JoinNo] then //Do not override non-joining cursors
    fResource.Cursors.Cursor := kmc_Default; //In case this is run with keyboard shortcut, mouse move won't happen
  Panel_Army_JoinGroups.Hide;
  if fPlayers.Selected is TKMUnitWarrior then
    Panel_Army.Show;
end;


procedure TKMGamePlayInterface.Build_Fill(Sender:TObject);
var I: Integer;
begin
  for i:=1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[i] <> ht_None then
  if MyPlayer.Stats.GetCanBuild(GUIHouseOrder[i]) then begin
    Button_Build[i].Enable;
    Button_Build[i].TexID:=fResource.HouseDat[GUIHouseOrder[i]].GUIIcon;
    Button_Build[i].OnClick:=Build_ButtonClick;
    Button_Build[i].Hint:=fResource.HouseDat[GUIHouseOrder[i]].HouseName;
  end else begin
    Button_Build[i].OnClick:=nil;
    Button_Build[i].TexID:=41;
    Button_Build[i].Hint:=fTextLibrary[TX_HOUSE_NOT_AVAIABLE]; //Building not available
  end;
end;


procedure TKMGamePlayInterface.Allies_Close(Sender: TObject);
begin
  if Panel_Allies.Visible then fSoundLib.Play(sfxn_MPChatClose);
  Panel_Allies.Hide;
end;


procedure TKMGamePlayInterface.Chat_Close(Sender: TObject);
begin
  if Panel_Chat.Visible then fSoundLib.Play(sfxn_MPChatClose);
  Panel_Chat.Hide;
  if fMyControls.CtrlFocus = Edit_ChatMsg then
    fMyControls.CtrlFocus := nil; //Lose focus so you can't type messages with the panel hidden
end;


procedure TKMGamePlayInterface.Chat_Post(Sender: TObject; Key: Word);
begin
  if (Key = VK_RETURN) and (Trim(Edit_ChatMsg.Text) <> '') and (fGame.Networking <> nil) then
  begin
    fGame.Networking.PostMessage(Edit_ChatMsg.Text, True, CheckBox_SendToAllies.Checked);
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


procedure TKMGamePlayInterface.ReplayClick;
  procedure SetButtons(aPaused:boolean);
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

  if (Sender = Button_ReplayPause) then begin
    fGame.IsPaused := True;
    SetButtons(False);
  end;

  if (Sender = Button_ReplayStep) then begin
    fGame.StepOneFrame;
    fGame.IsPaused := False;
    SetButtons(False);
  end;

  if (Sender = Button_ReplayResume) then begin
    fGame.IsPaused := False;
    SetButtons(True);
  end;

  if (Sender = Button_ReplayExit) then begin
    fGame.GameHold(true, gr_ReplayEnd);
    SetButtons(True);
  end;
end;


procedure TKMGamePlayInterface.MessageIssue(aKind: TKMMessageKind; aText: string);
begin
  fMessageList.AddEntry(aKind, aText, KMPoint(0,0));
  Message_UpdateStack;
  fSoundLib.Play(sfx_MessageNotice, 4); //Play horn sound on new message if it is the right type
end;


procedure TKMGamePlayInterface.MessageIssue(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
begin
  fMessageList.AddEntry(aKind, aText, aLoc);
  Message_UpdateStack;
  fSoundLib.Play(sfx_MessageNotice, 4); //Play horn sound on new message if it is the right type
end;


procedure TKMGamePlayInterface.House_MarketFill(aMarket: TKMHouseMarket);
var
  R: TResourceType;
  I, Tmp: Integer;
begin
  for I := 0 to STORE_RES_COUNT - 1 do
  begin
    R := TResourceType(Button_Market[I].Tag);
    if aMarket.AllowedToTrade(R) then
    begin
      Button_Market[I].TexID := fResource.Resources[R].GUIIcon;
      Button_Market[I].Hint := fResource.Resources[R].Title;
      Tmp := aMarket.GetResTotal(TResourceType(Button_Market[I].Tag));
      Button_Market[I].Caption := IfThen(Tmp = 0, '-', IntToStr(Tmp));
    end
    else
    begin
      Button_Market[I].TexID := 41;
      Button_Market[I].Hint := fTextLibrary[TX_HOUSES_MARKET_HINT_BLOCKED];
      Button_Market[I].Caption := '-';
    end;
  end;

  Shape_Market_From.Visible := aMarket.ResFrom <> rt_None;
  if aMarket.ResFrom <> rt_None then
  begin
    Shape_Market_From.Left := ((Byte(aMarket.ResFrom)-1) mod 6) * 31;
    Shape_Market_From.Top := 12 + ((Byte(aMarket.ResFrom)-1) div 6) * MARKET_RES_HEIGHT;
    Label_Market_In.Caption := Format(fTextLibrary[TX_HOUSES_MARKET_FROM],[aMarket.RatioFrom]) + ':';
    Button_Market_In.TexID := fResource.Resources[aMarket.ResFrom].GUIIcon;
    Button_Market_In.Caption := IntToStr(aMarket.GetResTotal(aMarket.ResFrom));
  end else begin
    Label_Market_In.Caption := Format(fTextLibrary[TX_HOUSES_MARKET_FROM],[0]) + ':';
    Button_Market_In.TexID := fResource.Resources[rt_None].GUIIcon;
    Button_Market_In.Caption := '-';
  end;

  Shape_Market_To.Visible := aMarket.ResTo <> rt_None;
  if aMarket.ResTo <> rt_None then
  begin
    Shape_Market_To.Left := ((Byte(aMarket.ResTo)-1) mod 6) * 31;
    Shape_Market_To.Top := 12 + ((Byte(aMarket.ResTo)-1) div 6) * MARKET_RES_HEIGHT;
    Label_Market_Out.Caption := Format(fTextLibrary[TX_HOUSES_MARKET_TO],[aMarket.RatioTo]) + ':';
    Button_Market_Out.Caption := IntToStr(aMarket.GetResTotal(aMarket.ResTo));
    Button_Market_Out.TexID := fResource.Resources[aMarket.ResTo].GUIIcon;
  end else begin
    Label_Market_Out.Caption := Format(fTextLibrary[TX_HOUSES_MARKET_TO],[0]) + ':';
    Button_Market_Out.TexID := fResource.Resources[rt_None].GUIIcon;
    Button_Market_Out.Caption := '-';
  end;

  Button_Market_Remove.Enabled := (aMarket.ResFrom <> rt_None) and (aMarket.ResTo <> rt_None);
  Button_Market_Add.Enabled := Button_Market_Remove.Enabled;
  Label_Market_FromAmount.Caption := IntToStr(aMarket.RatioFrom * aMarket.CheckResOrder(1));
  Label_Market_ToAmount.Caption := IntToStr(aMarket.RatioTo * aMarket.CheckResOrder(1));
end;


procedure TKMGamePlayInterface.House_MarketOrderClick(Sender:TObject; AButton:TMouseButton);
var M: TKMHouseMarket;
begin
  if not (fPlayers.Selected is TKMHouseMarket) then Exit;

  M := TKMHouseMarket(fPlayers.Selected);

  if Sender = Button_Market_Remove then
    fGame.GameInputProcess.CmdHouse(gic_HouseOrderProduct, M, 1, -ClickAmount[AButton]);
  if Sender = Button_Market_Add then
    fGame.GameInputProcess.CmdHouse(gic_HouseOrderProduct, M, 1, ClickAmount[AButton]);
end;


procedure TKMGamePlayInterface.House_MarketSelect(Sender: TObject; AButton: TMouseButton);
var M: TKMHouseMarket;
begin
  if not (fPlayers.Selected is TKMHouseMarket) then Exit;

  M := TKMHouseMarket(fPlayers.Selected);

  //todo: We need to tell player that he must cancel previous order first instead of silently refusing

  if aButton = mbLeft then
    fGame.GameInputProcess.CmdHouse(gic_HouseMarketFrom, M, TResourceType(TKMButtonFlat(Sender).Tag));
  if aButton = mbRight then
    fGame.GameInputProcess.CmdHouse(gic_HouseMarketTo, M, TResourceType(TKMButtonFlat(Sender).Tag));

  House_MarketFill(M); //Update costs and order count
end;


procedure TKMGamePlayInterface.House_StoreFill;
var i,Tmp:integer;
begin
  if fPlayers.Selected=nil then exit;
  if not (fPlayers.Selected is TKMHouseStore) then exit;

  for i:=1 to STORE_RES_COUNT do begin
    Tmp:=TKMHouseStore(fPlayers.Selected).CheckResIn(StoreResType[i]);
    Button_Store[i].Caption := IfThen(Tmp=0, '-', inttostr(Tmp));
    Image_Store_Accept[i].Visible := TKMHouseStore(fPlayers.Selected).NotAcceptFlag[StoreResType[i]];
  end;
end;


procedure TKMGamePlayInterface.Menu_Fill(Sender:TObject);
begin
  if fGameApp.GameSettings.MusicOff then
    Label_Menu_Track.Caption := '-'
  else
    Label_Menu_Track.Caption := fGameApp.MusicLib.GetTrackTitle;

  Label_GameTime.Caption := Format(fTextLibrary[TX_GAME_TIME], [TimeToString(fGame.MissionTime)]);

  Label_Menu_Track.Enabled      := not fGameApp.GameSettings.MusicOff;
  Button_Menu_TrackUp.Enabled   := not fGameApp.GameSettings.MusicOff;
  Button_Menu_TrackDown.Enabled := not fGameApp.GameSettings.MusicOff;
end;


procedure TKMGamePlayInterface.Stats_Fill(Sender: TObject);
var I, Tmp, Tmp2: Integer;
begin
  for I := Low(StatHouse) to High(StatHouse) do
  begin
    Tmp := MyPlayer.Stats.GetHouseQty(StatHouse[I]);
    Tmp2 := MyPlayer.Stats.GetHouseWip(StatHouse[I]);
    Stat_HouseQty[I].Caption := IfThen(Tmp =0, '-', inttostr(Tmp));
    Stat_HouseWip[I].Caption := IfThen(Tmp2=0, '', '+'+inttostr(Tmp2));
    if MyPlayer.Stats.GetCanBuild(StatHouse[I]) or (Tmp > 0) then
    begin
      Stat_HousePic[I].TexID := fResource.HouseDat[StatHouse[I]].GUIIcon;
      Stat_HousePic[I].Hint := fResource.HouseDat[StatHouse[I]].HouseName;
      Stat_HouseQty[I].Hint := fResource.HouseDat[StatHouse[I]].HouseName;
      Stat_HouseWip[I].Hint := fResource.HouseDat[StatHouse[I]].HouseName;
    end
    else
    begin
      Stat_HousePic[I].TexID := 41;
      Stat_HousePic[I].Hint := fTextLibrary[TX_HOUSE_NOT_AVAIABLE]; //Building not available
    end;
  end;
  for I := Low(StatUnit) to High(StatUnit) do
  begin
    Tmp := MyPlayer.Stats.GetUnitQty(StatUnit[I]);
    Stat_UnitQty[I].Caption := IfThen(Tmp = 0, '-', inttostr(Tmp));
    Stat_UnitPic[I].Hint := fResource.UnitDat[StatUnit[I]].UnitName;
    Stat_UnitPic[I].FlagColor := MyPlayer.FlagColor;
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

  Panel_Replay.Visible := fReplay;
end;


procedure TKMGamePlayInterface.ClearOpenMenu;
begin
  SwitchPage(Button_Back);
end;


procedure TKMGamePlayInterface.ShowClock(aSpeed: Word);
begin
  Image_Clock.Visible := aSpeed <> 1;
  Label_Clock.Visible := aSpeed <> 1;
  Label_ClockSpeedup.Visible := aSpeed <> 1;
  Label_ClockSpeedup.Caption := 'x' + IntToStr(aSpeed);

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
                    Label_PlayMore.Caption := fTextLibrary[TX_GAMEPLAY_WON];
                    Button_PlayMore.Caption := fTextLibrary[TX_GAMEPLAY_CONTINUE_PLAYING];
                    Button_PlayQuit.Caption := fTextLibrary[TX_GAMEPLAY_VICTORY];
                  end;
    gr_Defeat:    begin
                    Label_PlayMore.Caption := fTextLibrary[TX_GAMEPLAY_LOST];
                    Button_PlayMore.Caption := fTextLibrary[TX_GAMEPLAY_DEFEAT_CONTINUEWATCHING];
                    Button_PlayQuit.Caption := fTextLibrary[TX_GAMEPLAY_DEFEAT];
                  end;
    gr_ReplayEnd: begin
                    Label_PlayMore.Caption := fTextLibrary[TX_GAMEPLAY_REPLAY_ENDED];
                    Button_PlayMore.Caption := fTextLibrary[TX_GAMEPLAY_REPLAY_CONTINUEWATCHING];
                    Button_PlayQuit.Caption := fTextLibrary[TX_GAMEPLAY_QUIT_TO_MENU];
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
                    Label_MPPlayMore.Caption := fTextLibrary[TX_GAMEPLAY_WON];
                    Button_MPPlayMore.Caption := fTextLibrary[TX_GAMEPLAY_CONTINUE_PLAYING];
                    Button_MPPlayQuit.Caption := fTextLibrary[TX_GAMEPLAY_VICTORY];
                  end;
    gr_Defeat:    begin
                    Label_MPPlayMore.Caption := fTextLibrary[TX_GAMEPLAY_LOST];
                    Button_MPPlayMore.Caption := fTextLibrary[TX_GAMEPLAY_DEFEAT_CONTINUEWATCHING];
                    Button_MPPlayQuit.Caption := fTextLibrary[TX_GAMEPLAY_DEFEAT];
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
    S := fTextLibrary[TX_MULTIPLAYER_ATTEMPT_RECONNECTING];
    Button_NetDropPlayers.Visible := false;
    fNetWaitDropPlayersDelayStarted := 0;
    Label_NetDropPlayersDelay.Caption := '';
  end
  else
  begin
    S := fTextLibrary[TX_MULTIPLAYER_WAITING]+' ';
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


procedure TKMGamePlayInterface.NetWaitClick(Sender:TObject);
begin
  if Sender = Button_NetQuit then
  begin
    Panel_NetWaitButtons.Hide;
    Label_NetWaitConfirm.Caption := fTextLibrary[TX_GAMEPLAY_CONFIRM_QUIT];
    Button_NetConfirmYes.Caption := fTextLibrary[TX_GAMEPLAY_QUIT_TO_MENU];
    Panel_NetWaitConfirm.Show;
  end else
  if Sender = Button_NetDropPlayers then
  begin
    Panel_NetWaitButtons.Hide;
    Label_NetWaitConfirm.Caption := fTextLibrary[TX_GAMEPLAY_CONFIRM_DROP];
    Button_NetConfirmYes.Caption := fTextLibrary[TX_GAMEPLAY_DROP_PLAYERS];
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
    if Button_NetConfirmYes.Caption = fTextLibrary[TX_GAMEPLAY_DROP_PLAYERS] then
      fGame.GameDropWaitingPlayers else
    if Button_NetConfirmYes.Caption = fTextLibrary[TX_GAMEPLAY_QUIT_TO_MENU] then
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
  Result := fMultiplayer and (MyPlayer.AI.WonOrLost = wol_Lost);
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
    fSelection[Key] := TKMUnit(aObject).ID
  else
  if aObject is TKMHouse then
    fSelection[Key] := TKMHouse(aObject).ID
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
    OldSelected := fPlayers.Selected;
    fPlayers.Selected := fPlayers.GetUnitByID(fSelection[Key]);
    if fPlayers.Selected <> nil then
    begin
      if (OldSelected <> fPlayers.Selected) and not fReplay and not HasLostMPGame then
      begin
        if fPlayers.Selected is TKMUnitWarrior then
          fSoundLib.PlayWarrior(TKMUnit(fPlayers.Selected).UnitType, sp_Select)
        else
          fSoundLib.PlayCitizen(TKMUnit(fPlayers.Selected).UnitType, sp_Select);
      end;
      //Selecting a unit twice is the shortcut to center on that unit
      if OldSelected = fPlayers.Selected then
        fGame.Viewport.Position := TKMUnit(fPlayers.Selected).PositionF;
    end
    else
    begin
      fPlayers.Selected := fPlayers.GetHouseByID(fSelection[Key]);
      if (fPlayers.Selected <> nil) and TKMHouse(fPlayers.Selected).IsDestroyed then
        fPlayers.Selected := nil; //Don't select destroyed houses
      //Selecting a house twice is the shortcut to center on that house
      if (fPlayers.Selected <> nil) and (OldSelected = fPlayers.Selected) then
        fGame.Viewport.Position := KMPointF(TKMHouse(fPlayers.Selected).GetEntrance);
    end;

  end
  else
    fPlayers.Selected := nil;
end;


procedure TKMGamePlayInterface.SetChatMessages(const aString: string);
begin
  Memo_ChatText.Text := aString;
  Memo_ChatText.ScrollToBottom;
end;


procedure TKMGamePlayInterface.ChatMessage(const aData: string);
begin
  Memo_ChatText.Add(aData);

  if not Panel_Chat.Visible then
    Label_MPChatUnread.Caption := IntToStr(StrToIntDef(Label_MPChatUnread.Caption, 0) + 1); //New message
end;


procedure TKMGamePlayInterface.WarriorCommanderDied(DeadID, NewID: Cardinal);
var I: Integer;
begin
  for I:=0 to 9 do
    if fSelection[I] = DeadID then
      fSelection[I] := NewID; //If the commander dies select the new commander
end;


procedure TKMGamePlayInterface.AlliesOnPlayerSetup(Sender: TObject);
var
  I,LocaleID: Integer;
begin
  for I := 0 to fGame.Networking.NetPlayers.Count - 1 do
  begin
    //Show players locale flag
    LocaleID := fLocales.GetIDFromCode(fGame.Networking.NetPlayers[I+1].LangCode);
    if LocaleID <> -1 then
      Image_AlliesLang[I].TexID := fLocales[LocaleID].FlagSpriteID
    else
      if fGame.Networking.NetPlayers[I+1].IsComputer then
        Image_AlliesLang[I].TexID := 62 //PC icon
      else
        Image_AlliesLang[I].TexID := 0;

    Label_AlliesPlayer[I].Caption := fGame.Networking.NetPlayers[I+1].Nikname;
    Label_AlliesPlayer[I].FontColor := fPlayers[fGame.Networking.NetPlayers[I+1].PlayerIndex.PlayerIndex].FlagColor;
    DropBox_AlliesTeam[I].ItemIndex := fGame.Networking.NetPlayers[I+1].Team;
    //Strikethrough for disconnected players
    Image_AlliesLang[I].Enabled := not fGame.Networking.NetPlayers[I+1].Dropped;
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
      Label_AlliesPing[I].Caption := IntToStr(fGame.Networking.NetPlayers[I+1].GetInstantPing);
      Label_AlliesPing[I].FontColor := GetPingColor(fGame.Networking.NetPlayers[I+1].GetInstantPing);
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
    SC_SHOW_TEAMS: if fMultiplayer or (fGame.GameMode = gmReplayMulti) then //Only MP replays
    begin
      fGame.ShowTeamNames := True;
      //Update it immediately so there's no 300ms lag after pressing the key
      fTeamNames.Clear;
      Rect := fGame.Viewport.GetMinimapClip;
      fPlayers.GetUnitsInRect(Rect, fTeamNames);
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
    if (Key = SC_PAUSE) then
      SetPause(False);
    Exit;
  end;

  if fMyControls.KeyUp(Key, Shift) then Exit;

  case Key of
    //Scrolling
    VK_LEFT:  fGame.Viewport.ScrollKeyLeft  := False;
    VK_RIGHT: fGame.Viewport.ScrollKeyRight := False;
    VK_UP:    fGame.Viewport.ScrollKeyUp    := False;
    VK_DOWN:  fGame.Viewport.ScrollKeyDown  := False;
    VK_BACK:  fGame.Viewport.ResetZoom;

    //Game speed/pause: Not available in multiplayer mode yet (handled in fGame)
    VK_F5:    fGame.SetGameSpeed(1);
    VK_F6:    fGame.SetGameSpeed(fGameApp.GameSettings.SpeedMedium);
    VK_F7:    fGame.SetGameSpeed(fGameApp.GameSettings.SpeedFast);
    VK_F8:    fGame.SetGameSpeed(fGameApp.GameSettings.SpeedVeryFast);

    SC_SHOW_TEAMS:  fGame.ShowTeamNames := False;
  end;

  //All the following keys don't work in Replay,
  //thus the easy way to make that is to exit now
  if fReplay then Exit;

  case Key of
    //Messages
    VK_SPACE:             //In KaM spacebar centers you on the message
                          Button_MessageGoTo.Click;
    VK_DELETE:            Button_MessageDelete.Click;
    VK_RETURN:            //Enter is the shortcut to bring up chat in multiplayer
                          if fMultiplayer and not Panel_Chat.Visible then
                            Chat_Show(Self);
    VK_ESCAPE:            //'or' allows us to go through Clicks one by one
                          if Button_Army_Join_Cancel.Click
                            or Button_MessageClose.Click
                            or Image_ChatClose.Click
                            or Image_AlliesClose.Click
                            or Button_Back.Click then ;
    //Menu shortcuts
    SC_MENU_BUILD:  Button_Main[tbBuild].Click;
    SC_MENU_RATIO:  Button_Main[tbRatio].Click;
    SC_MENU_STATS:  Button_Main[tbStats].Click;
    SC_MENU_MENU:   Button_Main[tbMenu].Click;

    SC_SELECT_LOW..SC_SELECT_HIGH:
                    if (ssCtrl in Shift) then
                      Selection_Assign(Key, fPlayers.Selected)
                    else
                      Selection_Select(Key);

    // Army shortcuts from KaM
    Ord(SC_ARMY_HALT):   if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_Stop.Click;
    Ord(SC_ARMY_LINK):   if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_Join.Click;
    Ord(SC_ARMY_SPLIT):  if Panel_Army.Visible and not SelectingTroopDirection then Button_Army_Split.Click;

    //General function keys
    SC_PAUSE:       if not fMultiplayer then SetPause(True); //Display pause overlay
    SC_BEACON:      if not SelectingTroopDirection then
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
    Ord(SC_DEBUG_VICTORY):   begin fGame.PlayerVictory(MyPlayer.PlayerIndex); Exit; end;
    Ord(SC_DEBUG_DEFEAT):    begin fGame.PlayerDefeat (MyPlayer.PlayerIndex); Exit; end;
    Ord(SC_DEBUG_ADDSCOUT):  fGame.GameInputProcess.CmdTemp(gic_TempAddScout, GameCursor.Cell);
  end;
end;


//1. Process Controls
//2. Show SelectingTroopDirection
procedure TKMGamePlayInterface.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  Group: TKMUnitGroup;
  U: TKMUnit;
  H: TKMHouse;
  MyRect: TRect;
begin
  inherited;

  if (fGame.IsPaused and not fReplay) or (fMyControls.CtrlOver <> nil) then
    Exit;

  if SelectingTroopDirection then
  begin
    fMain.ApplyCursorRestriction; //Reset the cursor restrictions from selecting direction
    SelectingTroopDirection := false;
    DirectionCursorHide;
  end;

  //See if we can show DirectionSelector
  //Can walk to ally units place, can't walk to house place anyway, unless it's a markup and allied
  if (Button = mbRight)
  and not fReplay
  and not HasLostMPGame
  and not fJoiningGroups
  and not fPlacingBeacon
  and (fPlayers.Selected is TKMUnitGroup) then
  begin
    Group := TKMUnitGroup(fPlayers.Selected);
    if Group.Owner = MyPlayer.PlayerIndex then
    begin
      U := fTerrain.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
      H := fPlayers.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
      if ((U = nil) or U.IsDeadOrDying or (fPlayers.CheckAlliance(MyPlayer.PlayerIndex, U.Owner) = at_Ally)) and
         ((H = nil) or (fPlayers.CheckAlliance(MyPlayer.PlayerIndex, H.Owner) = at_Ally)) and
        Group.CanWalkTo(GameCursor.Cell, 0) then
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
      end;
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
  U: TKMUnit;
  H: TKMHouse;
  P: TKMPoint;
  Group: TKMUnitGroup;
begin
  inherited;

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
    if MyPlayer.FogOfWar.CheckTileRevelation(P.X, P.Y, False) > 0 then
    case GameCursor.Mode of
      cmRoad:  if MyPlayer.CanAddFakeFieldPlan(P, ft_Road) and not KMSamePoint(LastDragPoint, P) then
                begin
                  fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Road);
                  LastDragPoint := GameCursor.Cell;
                end;
      cmField: if MyPlayer.CanAddFakeFieldPlan(P, ft_Corn) and not KMSamePoint(LastDragPoint, P) then
                begin
                  fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Corn);
                  LastDragPoint := GameCursor.Cell;
                end;
      cmWine:  if MyPlayer.CanAddFakeFieldPlan(P, ft_Wine) and not KMSamePoint(LastDragPoint, P) then
                begin
                  fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Wine);
                  LastDragPoint := GameCursor.Cell;
                end;
      cmErase: if not KMSamePoint(LastDragPoint, P) then
                begin
                  if MyPlayer.BuildList.HousePlanList.HasPlan(P) then
                  begin
                    fGame.GameInputProcess.CmdBuild(gic_BuildRemoveHousePlan, P);
                    LastDragPoint := GameCursor.Cell;
                  end
                  else
                    if (MyPlayer.BuildList.FieldworksList.HasFakeField(P) <> ft_None) then
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

  if fJoiningGroups and (fPlayers.Selected is TKMUnitGroup) then
  begin
    Group := TKMUnitGroup(fPlayers.Selected);
    U := fTerrain.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
    if (U <> nil)
    and (U is TKMUnitWarrior)
    and (U.Owner = MyPlayer.PlayerIndex)
    and not U.IsDeadOrDying
    and not Group.HasMember(TKMUnitWarrior(U))
    and (UnitGroups[U.UnitType] = Group.GroupType) then
      fResource.Cursors.Cursor := kmc_JoinYes
    else
      fResource.Cursors.Cursor := kmc_JoinNo;
    Exit;
  end;

  if fPlayers.HitTest(GameCursor.Cell.X, GameCursor.Cell.Y, not fReplay) <> nil then
  begin
    fResource.Cursors.Cursor := kmc_Info;
    Exit;
  end;

  if (fPlayers.Selected is TKMUnitGroup) and not fReplay and not HasLostMPGame
  and (MyPlayer.FogOfWar.CheckTileRevelation(GameCursor.Cell.X, GameCursor.Cell.Y, false) > 0) then
  begin
    U := fTerrain.UnitsHitTest (GameCursor.Cell.X, GameCursor.Cell.Y);
    H := fPlayers.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
    if ((U <> nil) and (not U.IsDeadOrDying) and (fPlayers.CheckAlliance(MyPlayer.PlayerIndex, U.Owner) = at_Enemy)) or
       ((H <> nil) and (fPlayers.CheckAlliance(MyPlayer.PlayerIndex, H.Owner) = at_Enemy)) then
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
  U: TKMUnit;
  H: TKMHouse;
  Group, Group2: TKMUnitGroup;
  OldSelected: TObject;
begin
  inherited;

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
    mbLeft:   begin
                //Process groups joining
                if fJoiningGroups and (fPlayers.Selected is TKMUnitGroup) then
                begin
                  Group := TKMUnitGroup(fPlayers.Selected);
                  U := MyPlayer.UnitsHitTest(P.X, P.Y); //Scan only teammates
                  if (U is TKMUnitWarrior)
                  and not U.IsDeadOrDying
                  and not Group.HasMember(U)
                  and (Group.GroupType = UnitGroups[U.UnitType]) then
                  begin
                    Group2 := MyPlayer.UnitGroups.GetGroupByMember(TKMUnitWarrior(U));
                    fSoundLib.PlayWarrior(Group.UnitType, sp_Join); //In SP joining is instant, Group does not exist after that
                    fGame.GameInputProcess.CmdArmy(gic_ArmyLink, Group, Group2);
                    Army_HideJoinMenu(nil);
                  end;
                  Exit;
                end;

                if fPlacingBeacon then
                begin
                  fGame.GameInputProcess.CmdGame(gic_GameAlertBeacon, GameCursor.Float, MyPlayer.PlayerIndex);
                  Beacon_Cancel;
                  Exit;
                end;

                //Only allow placing of roads etc. with the left mouse button
                if MyPlayer.FogOfWar.CheckTileRevelation(P.X, P.Y, True) = 0 then
                begin
                  if GameCursor.Mode in [cmErase, cmRoad, cmField, cmWine, cmWall, cmHouses] then
                    fSoundLib.Play(sfx_CantPlace,P,false,4.0); //Can't place noise when clicking on unexplored areas
                end
                else
                  case GameCursor.Mode of
                    cmNone:  begin
                                //Remember previous selection to play sound if it changes
                                OldSelected := fPlayers.Selected;

                                //Allow to select any players assets in replay
                                fPlayers.SelectHitTest(P.X, P.Y, not fReplay);

                                //In a replay we want in-game statistics (and other things) to be shown for the owner of the last select object
                                if fReplay then
                                begin
                                  if fPlayers.Selected is TKMHouse then MyPlayer := fPlayers[TKMHouse(fPlayers.Selected).Owner];
                                  if fPlayers.Selected is TKMUnit  then MyPlayer := fPlayers[TKMUnit (fPlayers.Selected).Owner];
                                end;

                                if (fPlayers.Selected is TKMHouse) then
                                  ShowHouseInfo(TKMHouse(fPlayers.Selected));

                                if (fPlayers.Selected is TKMUnit) then
                                begin
                                  ShowUnitInfo(TKMUnit(fPlayers.Selected));
                                  if (OldSelected <> fPlayers.Selected) and not fReplay and not HasLostMPGame then
                                    fSoundLib.PlayCitizen(TKMUnit(fPlayers.Selected).UnitType, sp_Select);
                                end;

                                if (fPlayers.Selected is TKMUnitGroup) then
                                begin
                                  Group := TKMUnitGroup(fPlayers.Selected);
                                  ShowGroupInfo(Group);
                                  if (OldSelected <> fPlayers.Selected) and not fReplay and not HasLostMPGame then
                                    fSoundLib.PlayWarrior(Group.SelectedUnit.UnitType, sp_Select);
                                end;
                              end;
                    cmRoad:  if KMSamePoint(LastDragPoint,KMPoint(0,0)) then fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Road);
                    cmField: if KMSamePoint(LastDragPoint,KMPoint(0,0)) then fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Corn);
                    cmWine:  if KMSamePoint(LastDragPoint,KMPoint(0,0)) then fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Wine);
                    cmWall:  fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Wall);
                    cmHouses:if MyPlayer.CanAddHousePlan(P, THouseType(GameCursor.Tag1)) then
                      begin
                        fGame.GameInputProcess.CmdBuild(gic_BuildHousePlan, P,
                          THouseType(GameCursor.Tag1));
                        if not (ssShift in Shift) then Build_ButtonClick(Button_BuildRoad); //If shift pressed do not reset cursor(keep selected building)
                      end
                      else
                        fSoundLib.Play(sfx_CantPlace,P,false,4.0);
                    cmErase: if KMSamePoint(LastDragPoint,KMPoint(0,0)) then
                              begin
                                H := MyPlayer.HousesHitTest(P.X, P.Y);
                                //Ask wherever player wants to destroy own house (don't ask about houses that are not started, they are removed below)
                                if H <> nil then
                                begin
                                  fPlayers.Selected := H; //Select the house irregardless of unit below/above
                                  ShowHouseInfo(H, True);
                                  fSoundLib.Play(sfx_Click);
                                end
                                else
                                begin
                                  //Now remove houses that are not started
                                  if MyPlayer.BuildList.HousePlanList.HasPlan(P) then
                                    fGame.GameInputProcess.CmdBuild(gic_BuildRemoveHousePlan, P)
                                  else
                                    if MyPlayer.BuildList.FieldworksList.HasFakeField(P) <> ft_None then
                                      fGame.GameInputProcess.CmdBuild(gic_BuildRemoveFieldPlan, P) //Remove plans
                                    else
                                      fSoundLib.Play(sfx_CantPlace,P,false,4.0); //Otherwise there is nothing to erase
                                end;
                              end;
                  end
              end;
    mbRight:  begin
                //Select direction
                ReleaseDirectionSelector;

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
                and (fPlayers.Selected is TKMUnitGroup) then
                begin
                  Group := TKMUnitGroup(fPlayers.Selected);

                  //Attack or Walk
                  if Group.CanTakeOrders and (Group.Owner = MyPlayer.PlayerIndex) then
                  begin
                    //Try to Attack unit
                    U := fTerrain.UnitsHitTest(P.X, P.Y);
                    if (U <> nil) and not U.IsDeadOrDying
                    and (fPlayers.CheckAlliance(MyPlayer.PlayerIndex, U.Owner) = at_Enemy) then
                    begin
                      fGame.GameInputProcess.CmdArmy(gic_ArmyAttackUnit, Group, U);
                      fSoundLib.PlayWarrior(Group.UnitType, sp_Attack);
                    end
                    else
                    begin //If there's no unit - try to Attack house
                      H := fPlayers.HousesHitTest(P.X, P.Y);
                      if (H <> nil) and not H.IsDestroyed
                      and (fPlayers.CheckAlliance(MyPlayer.PlayerIndex, H.Owner) = at_Enemy) then
                      begin
                        fGame.GameInputProcess.CmdArmy(gic_ArmyAttackHouse, Group, H);
                        fSoundLib.PlayWarrior(Group.UnitType, sp_Attack);
                      end
                      else //If there's no house - Walk to spot
                        if Group.CanWalkTo(P, 0) then
                        begin
                          fGame.GameInputProcess.CmdArmy(gic_ArmyWalk, Group, P, SelectedDirection);
                          fSoundLib.PlayWarrior(Group.UnitType, sp_Move);
                        end;
                    end;
                  end;
                end;

              end;
    mbMiddle: if DEBUG_CHEATS and (MULTIPLAYER_CHEATS or not fMultiplayer) then
                fGame.GameInputProcess.CmdTemp(gic_TempAddScout, P);
  end;

  LastDragPoint := KMPoint(0,0);
end;


procedure TKMGamePlayInterface.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fLastSaveName);
  SaveStream.Write(LastSchoolUnit);
  SaveStream.Write(LastBarracksUnit);
  SaveStream.Write(fSelection, SizeOf(fSelection));
  fMessageList.Save(SaveStream);
  //Everything else (e.g. ShownUnit or AskDemolish) can't be seen in Save_menu anyways
end;


procedure TKMGamePlayInterface.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fLastSaveName);
  LoadStream.Read(LastSchoolUnit);
  LoadStream.Read(LastBarracksUnit);
  LoadStream.Read(fSelection, SizeOf(fSelection));
  fMessageList.Load(LoadStream);
  //Everything else (e.g. ShownUnit or AskDemolish) can't be seen in Save_menu anyways
  Message_UpdateStack;
  fLog.AddTime('Interface loaded');
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
  S: string;
  Rect: TKMRect;
begin
  //Update unit/house information
  if fPlayers.Selected is TKMUnitGroup then
    ShowGroupInfo(TKMUnitGroup(fPlayers.Selected))
  else
  if fPlayers.Selected is TKMUnit then
    ShowUnitInfo(TKMUnit(fPlayers.Selected), fAskDismiss)
  else
  begin
    fJoiningGroups := False;
    if fPlayers.Selected is TKMHouse then
      ShowHouseInfo(TKMHouse(fPlayers.Selected), fAskDemolish)
    else
      if Panel_House.Visible or Panel_Unit.Visible then
        SwitchPage(nil);
  end;

  //Update peacetime counter
  if fGame.GameOptions.Peacetime <> 0 then
    Label_PeacetimeRemaining.Caption := Format(fTextLibrary[TX_MP_PEACETIME_REMAINING],
                                               [TimeToString(fGame.GetPeacetimeRemaining)])
  else Label_PeacetimeRemaining.Caption := '';

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
  if Image_Clock.Visible then begin
    Image_Clock.TexID := ((Image_Clock.TexID - 556) + 1) mod 16 + 556;
    Label_Clock.Caption := TimeToString(fGame.MissionTime);
  end;

  //Keep on updating these menu pages as game data keeps on changing
  if Panel_Build.Visible then Build_Fill(nil);
  if Panel_Stats.Visible then Stats_Fill(nil);
  if Panel_Menu.Visible then Menu_Fill(nil);

  //Flash unread message display
  Label_MPChatUnread.Visible := fMultiplayer and (Label_MPChatUnread.Caption <> '') and not (aTickCount mod 10 < 5);
  Image_MPChat.Highlight := Panel_Chat.Visible or (Label_MPChatUnread.Visible and (Label_MPChatUnread.Caption <> ''));
  Image_MPAllies.Highlight := Panel_Allies.Visible;

  //Update info on awaited players
  if Panel_NetWait.Visible then
  begin
    if fGame.Networking.IsReconnecting then
      Label_NetDropPlayersDelay.Caption := ''
    else
    begin
      i := NET_DROP_PLAYER_MIN_WAIT - EnsureRange(GetTimeSince(fNetWaitDropPlayersDelayStarted) div 1000, 0, NET_DROP_PLAYER_MIN_WAIT);
      if i > 0 then
        Label_NetDropPlayersDelay.Caption := Format(fTextLibrary[TX_GAMEPLAY_DROP_PLAYERS_DELAY], [i])
      else
        Label_NetDropPlayersDelay.Caption := fTextLibrary[TX_GAMEPLAY_DROP_PLAYERS_ALLOWED];
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
      fPlayers.GetUnitsInRect(Rect, fTeamNames);
    end;
  end;

  S := '';

  //Debug info
  if SHOW_SPRITE_COUNT then
    S := IntToStr(fPlayers.GetUnitCount) + ' units on map|' +
         IntToStr(fRenderPool.RenderList.Stat_Sprites) + '/' +
         IntToStr(fRenderPool.RenderList.Stat_Sprites2) + ' sprites/rendered|' +
         IntToStr(CtrlPaintCount) + ' controls rendered|';

  if SHOW_POINTER_COUNT then
    S := S + Format('Pointers: %d units, %d houses|', [MyPlayer.Units.GetTotalPointers, MyPlayer.Houses.GetTotalPointers]);

  if SHOW_CMDQUEUE_COUNT then
    S := S + IntToStr(fGame.GameInputProcess.Count) + ' commands stored|';

  if SHOW_NETWORK_DELAY and fMultiplayer then
    S := S + 'Network delay: ' + IntToStr(TGameInputProcess_Multi(fGame.GameInputProcess).GetNetworkDelay) + '|';

  if DISPLAY_SOUNDS then
    S := S + IntToStr(fSoundLib.ActiveCount) + ' sounds playing|';

  //Temporary inteface (by @Crow)
  if SHOW_ARMYEVALS then
    for I := 0 to fPlayers.Count - 1 do
    if I <> MyPlayer.PlayerIndex then
      S := S + Format('Enemy %d: %f|', [I, RoundTo(MyPlayer.ArmyEval.Evaluations[I].fPower, -3)]);

  if SHOW_AI_WARE_BALANCE then
    S := S + MyPlayer.AI.Mayor.BalanceText + '|';

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
      Label_TeamName.Caption := fPlayers[U.Owner].PlayerName;
      Label_TeamName.FontColor := FlagColorToTextColor(fPlayers[U.Owner].FlagColor);

      UnitLoc := U.PositionF;
      UnitLoc.X := UnitLoc.X - 0.5;
      UnitLoc.Y := UnitLoc.Y - 1;
      MapLoc := fTerrain.FlatToHeight(UnitLoc);
      ScreenLoc := fGame.Viewport.MapToScreen(MapLoc);

      if KMInRect(ScreenLoc, KMRect(0, 0, Panel_Main.Width, Panel_Main.Height)) then
      begin
        Label_TeamName.Left := ScreenLoc.X;
        Label_TeamName.Top := ScreenLoc.Y;
        Label_TeamName.Paint;
      end;
    end;
  end;
  Label_TeamName.Visible := False; //Only visible while we're using it, otherwise it shows up in other places

  inherited;
end;


end.
