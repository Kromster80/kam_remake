unit KM_InterfaceGamePlay;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  StrUtils, SysUtils, KromUtils, Math, Classes, Controls,
  KM_InterfaceDefaults, KM_MapView, KM_Terrain,
  KM_Controls, KM_Houses, KM_Units, KM_Saves, KM_Defaults, KM_MessageStack, KM_CommonClasses, KM_Points;


const MAX_VISIBLE_MSGS = 32;

type
  TKMGamePlayInterface = class (TKMUserInterface)
  private
    fMapView: TKMMapView;

    //Not saved
    fShownUnit:TKMUnit;
    fShownHouse:TKMHouse;
    LastDragPoint:TKMPoint; //Last mouse point that we drag placed/removed a road/field
    PrevHint:TObject;
    ShownMessage: Integer;
    PlayMoreMsg:TGameResultMsg; //Remember which message we are showing
    fJoiningGroups: boolean;
    fAskDemolish, fAskDismiss: Boolean;
    fNetWaitDropPlayersDelayStarted:cardinal;
    SelectedDirection: TKMDirection;
    SelectingTroopDirection:boolean;
    SelectingDirPosition: TPoint;
    RatioTab:byte; //Active resource distribution tab
    fSaves: TKMSavesCollection;

    //Saved
    fLastSaveName:string; //The file name we last used to save this file (used as default in Save menu)
    fSave_Selected:integer; //Save selected from list (needed because of scanning)
    LastSchoolUnit:byte;  //Last unit that was selected in School, global for all schools player owns
    LastBarracksUnit:byte; //Last unit that was selected in Barracks, global for all barracks player owns
    fMessageList:TKMMessageList;

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

    procedure Army_ActivateControls(aActive:boolean);
    procedure Army_HideJoinMenu(Sender:TObject);
    procedure Army_Issue_Order(Sender:TObject);
    procedure Unit_Dismiss(Sender:TObject);
    procedure House_WoodcutterChange(Sender:TObject);
    procedure House_BarracksUnitChange(Sender:TObject; AButton:TMouseButton);
    procedure House_Demolish(Sender:TObject);
    procedure House_RepairToggle(Sender:TObject);
    procedure House_WareDeliveryToggle(Sender:TObject);
    procedure House_OrderClick(Sender:TObject; AButton:TMouseButton);
    procedure House_MarketFill;
    procedure House_MarketOrderClick(Sender:TObject; AButton:TMouseButton);
    procedure House_MarketSelect(Sender:TObject; AButton:TMouseButton);
    procedure House_SchoolUnitChange(Sender:TObject; AButton:TMouseButton);
    procedure House_SchoolUnitRemove(Sender:TObject);
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

    procedure Save_RefreshList(Sender: TObject);
    procedure Save_ListChange(Sender: TObject);
    procedure Save_EditChange(Sender: TObject);
    procedure Save_CheckboxChange(Sender: TObject);
    procedure Save_Click(Sender: TObject);
    procedure Load_RefreshList(Sender: TObject);
    procedure Load_ListClick(Sender: TObject);
    procedure Load_Click(Sender: TObject);
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
  protected
    Panel_Main: TKMPanel;
      Sidebar_Top: TKMImage;
      Sidebar_Middle: TKMImage;
      Minimap: TKMMinimap;
      Label_Stat, Label_PointerCount, Label_CmdQueueCount, Label_SoundsCount, Label_NetworkDelay, Label_Hint:TKMLabel;

      Image_MPChat, Image_MPAllies: TKMImage; //Multiplayer buttons
      Label_MPChatUnread: TKMLabel;
      Image_Message: array[0..MAX_VISIBLE_MSGS]of TKMImage; //Queue of messages covers 32*48=1536px height
      Image_Clock:TKMImage; //Clock displayed when game speed is increased
      Label_Clock:TKMLabel;
      Label_ClockSpeedup:TKMLabel;
      Label_MenuTitle: TKMLabel; //Displays the title of the current menu to the right of return
      Image_DirectionCursor:TKMImage;

      Label_VictoryChance : TKMLabel;

    Panel_Controls: TKMPanel;
      Button_Main: array[1..5]of TKMButton; //4 common buttons + Return

    Panel_Replay:TKMPanel; //Bigger Panel to contain Shapes to block all interface below
    Panel_ReplayCtrl:TKMPanel; //Smaller Panel to contain replay controls
      PercentBar_Replay:TKMPercentBar;
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
      DropBox_AlliesTeam:array [0..MAX_PLAYERS-1] of TKMDropBox;
      Label_AlliesTeam:array [0..MAX_PLAYERS-1] of TKMLabel;
      Label_AlliesPing:array [0..MAX_PLAYERS-1] of TKMLabel;
      Image_AlliesClose:TKMImage;
    Panel_Chat: TKMPanel; //For multiplayer: Send, reply, text area for typing, etc.
      Dragger_Chat: TKMDragger;
      Image_ChatHead, Image_ChatBody: TKMImage;
      Memo_ChatText: TKMMemo;
      Edit_ChatMsg: TKMEdit;
      CheckBox_SendToAllies: TKMCheckBox;
      Image_ChatClose: TKMImage;
    Panel_Message:TKMPanel;
      Label_MessageText:TKMLabel;
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
      Label_School_Res:TKMLabel;
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
    constructor Create(aScreenX, aScreenY: word); reintroduce;
    destructor Destroy; override;
    procedure Resize(X,Y: Word);
    procedure ShowHouseInfo(Sender:TKMHouse; aAskDemolish:boolean=false);
    procedure ShowUnitInfo(Sender:TKMUnit; aAskDismiss:boolean=false);
    procedure MessageIssue(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
    procedure MenuIconsEnabled(NewValue:boolean);
    procedure UpdateMapSize(X,Y:integer);
    procedure ShowClock(aSpeed: Word);
    procedure ShowPlayMore(DoShow:boolean; Msg:TGameResultMsg);
    procedure ShowMPPlayMore(Msg:TGameResultMsg);
    procedure ShowNetworkLag(DoShow:boolean; aPlayers:TStringList; IsHost:boolean);
    property ShownUnit: TKMUnit read fShownUnit;
    property ShownHouse: TKMHouse read fShownHouse;
    property LastSaveName:string read fLastSaveName write fLastSaveName;
    procedure ClearShownUnit;
    procedure ClearSelectedUnitOrHouse;
    procedure ReleaseDirectionSelector;
    procedure SetChatText(const aString: string);
    procedure SetChatMessages(const aString: string);
    procedure ChatMessage(const aData: string);
    procedure AlliesOnPlayerSetup(Sender: TObject);
    procedure AlliesOnPingInfo(Sender: TObject);
    procedure AlliesTeamChange(Sender: TObject);

    procedure KeyDown(Key:Word; Shift: TShiftState);
    procedure KeyUp(Key:Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
    procedure SaveMapview(SaveStream:TKMemoryStream);
    procedure LoadMapview(LoadStream:TKMemoryStream);
    procedure UpdateState; override;
  end;


implementation
uses KM_Main, KM_Units_Warrior, KM_GameInputProcess, KM_GameInputProcess_Multi,
KM_PlayersCollection, KM_RenderPool, KM_TextLibrary, KM_Game, KM_Utils, KM_Locales,
KM_Sound, Forms, KM_Resource, KM_Log, KM_ResourceUnit, KM_ResourceCursors, KM_ResourceSprites;

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
var i:integer; HT:THouseType;
begin
  if (MyPlayer=nil) or (MyPlayer.Stats=nil) then Exit; //We need to be able to access these

  //Hide everything but the tab buttons
  for i:=1 to Panel_Ratios.ChildCount do
    if not (Panel_Ratios.Childs[i] is TKMButton) then
      Panel_Ratios.Childs[i].Hide;

  RatioTab := TKMButton(Sender).Tag;

  Image_RatioPic0.TexID := fResource.Resources[ResRatioType[RatioTab]].GUIIcon;//Show resource icon
  Label_RatioLab0.Caption := fResource.Resources[ResRatioType[RatioTab]].Name;
  Image_RatioPic0.Show;
  Label_RatioLab0.Show;

  for i:=1 to ResRatioHouseCount[RatioTab] do
  begin
    HT := ResRatioHouse[RatioTab, i];
    //Do not allow player to see yet unreleased houses. Though house may be prebuilt and unreleased
    if MyPlayer.Stats.HouseReleased[HT] or (MyPlayer.Stats.GetHouseQty(HT)>0) then
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


procedure TKMGamePlayInterface.Save_ListChange(Sender: TObject);
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


procedure TKMGamePlayInterface.Save_EditChange(Sender: TObject);
begin
  if (Sender <> fSaves) then
  begin
    List_Save.ItemIndex := -1;
    fSave_Selected := -1;
    CheckBox_SaveExists.Enabled := FileExists(fGame.SaveName(Edit_Save.Text,'sav'));
    Label_SaveExists.Visible := CheckBox_SaveExists.Enabled;
    CheckBox_SaveExists.Checked := False;
    //we should protect ourselves from empty names and whitespaces at beggining and at end of name
    Button_Save.Enabled := (not CheckBox_SaveExists.Enabled) and (Edit_Save.Text <> '') and
                           not (Edit_Save.Text[1] = ' ') and not (Edit_Save.Text[Length(Edit_Save.Text)] = ' ');
  end;
end;


procedure TKMGamePlayInterface.Save_CheckboxChange(Sender: TObject);
begin
  Button_Save.Enabled := CheckBox_SaveExists.Checked;
end;


procedure TKMGamePlayInterface.Save_RefreshList(Sender: TObject);
var i:integer;
begin
  List_Save.Clear;

  if fSaves.ScanFinished then
  begin
    if LastSaveName = '' then
      Edit_Save.Text := fGame.GameName
    else
      Edit_Save.Text := LastSaveName;
  end;

  if (Sender = fSaves) then
    Save_EditChange(fSaves)
  else
    Save_EditChange(nil);

  if (Sender = fSaves) then
    for i:=0 to fSaves.Count-1 do
      List_Save.Add(fSaves[i].FileName);

  List_Save.ItemIndex := fSave_Selected;
end;


procedure TKMGamePlayInterface.Save_Click(Sender: TObject);
begin
  LastSaveName := Edit_Save.Text; //Do this before saving so it is included in the save
  if fGame.MultiplayerMode then
    //Don't tell everyone in the game that we are saving yet, as the command hasn't been processed
    fGame.GameInputProcess.CmdGame(gic_GameSave, Edit_Save.Text)
  else
    fGame.Save(Edit_Save.Text);

  fSaves.TerminateScan; //stop scan as it is no longer needed
  SwitchPage(nil); //Close save menu after saving
end;


procedure TKMGamePlayInterface.Load_ListClick(Sender: TObject);
begin
  Button_Load.Enabled := InRange(List_Load.ItemIndex, 0, fSaves.Count-1)
                         and fSaves[List_Load.ItemIndex].IsValid;
  if InRange(List_Load.ItemIndex,0,fSaves.Count-1) then
  begin
    Label_Load_Description.Caption := fSaves[List_Load.ItemIndex].Info.GetTitleWithTime;
    fSave_Selected := List_Load.ItemIndex;
  end;
end;


procedure TKMGamePlayInterface.Load_Click(Sender: TObject);
begin
  if fGame.MultiplayerMode then Exit; //Loading disabled during multiplayer gameplay. It is done from the lobby

  if not InRange(List_Load.ItemIndex,0,fSaves.Count-1) then exit;
  fSaves.TerminateScan; //stop scan as it is no longer needed
  fGame.StartSingleSave(fSaves[List_Load.ItemIndex].FileName);
end;


procedure TKMGamePlayInterface.Load_RefreshList(Sender: TObject);
var i:integer;
begin
  List_Load.Clear;

  if (Sender = fSaves) then
  begin
    for i:=0 to fSaves.Count-1 do
      List_Load.Add(fSaves[i].FileName);
  end;

  List_Load.ItemIndex := fSave_Selected;

  Load_ListClick(nil);
end;


{Switch between pages}
procedure TKMGamePlayInterface.SwitchPage(Sender: TObject);
var I: Integer; LastVisiblePage: TKMPanel;

  procedure Flip4MainButtons(ShowEm: Boolean);
  var K: Integer;
  begin
    for K := 1 to 4 do Button_Main[K].Visible := ShowEm;
    Button_Main[5].Visible := not ShowEm;
    Label_MenuTitle.Visible := not ShowEm;
  end;

begin
  fMyControls.CtrlFocus := nil; //Panels that require control focus should set it themselves

  if (Sender = Button_Main[1]) or (Sender = Button_Main[2])
  or (Sender = Button_Main[3]) or (Sender = Button_Main[4])
  or (Sender = Button_Menu_Settings) or (Sender = Button_Menu_Quit) then
  begin
    fShownHouse := nil;
    fShownUnit := nil;
    fPlayers.Selected := nil;
  end;

  //Reset the CursorMode, to cm_None
  Build_ButtonClick(nil);

  //Set LastVisiblePage to which ever page was last visible, out of the ones needed
  if Panel_Settings.Visible then LastVisiblePage := Panel_Settings else
  if Panel_Save.Visible     then LastVisiblePage := Panel_Save     else
  if Panel_Load.Visible     then LastVisiblePage := Panel_Load     else
    LastVisiblePage := nil;

  //If they just closed settings then we should save them (if something has changed)
  if LastVisiblePage = Panel_Settings then
    fGame.GlobalSettings.SaveSettings;

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
  if Sender = Button_Main[1] then begin
    Build_Fill(nil);
    Panel_Build.Show;
    Label_MenuTitle.Caption := fTextLibrary[TX_MENU_TAB_BUILD];
    Build_ButtonClick(Button_BuildRoad);
  end else

  if Sender = Button_Main[2] then begin
    Panel_Ratios.Show;
    SwitchPage_Ratios(Button_Ratios[1]); //Open 1st tab
    Label_MenuTitle.Caption := fTextLibrary[TX_MENU_TAB_DISTRIBUTE];
  end else

  if Sender = Button_Main[3] then begin
    Stats_Fill(nil);
    Panel_Stats.Show;
    Label_MenuTitle.Caption := fTextLibrary[TX_MENU_TAB_STATISTICS];
  end else

  if (Sender = Button_Main[4]) or (Sender = Button_Quit_No) or
     ((Sender = Button_Main[5]) and (LastVisiblePage = Panel_Settings)) or
     ((Sender = Button_Main[5]) and (LastVisiblePage = Panel_Load)) or
     ((Sender = Button_Main[5]) and (LastVisiblePage = Panel_Save)) then begin
    Menu_Fill(Sender); //Make sure updating happens before it is shown
    Label_MenuTitle.Caption := fTextLibrary[TX_MENU_TAB_OPTIONS];
    Panel_Menu.Show;
    Button_Menu_Load.Enabled := not fGame.MultiplayerMode; //No loading during multiplayer games
  end else

  if Sender = Button_Menu_Save then begin
    fSave_Selected := -1;
    //Stop current now scan so it can't add a save after we clear the list
    fSaves.TerminateScan;
    Save_RefreshList(nil); //need to call it at last one time to setup GUI even if there are no saves
    //Initiate refresh and process each new save added
    fSaves.Refresh(Save_RefreshList, fGame.MultiplayerMode);
    Panel_Save.Show;
    fMyControls.CtrlFocus := Edit_Save;
    Label_MenuTitle.Caption := fTextLibrary[TX_MENU_SAVE_GAME];
  end else

  if Sender = Button_Menu_Load then begin
    fSave_Selected := -1;
    //Stop current now scan so it can't add a save after we clear the list
    fSaves.TerminateScan;
    Load_RefreshList(nil); //need to call it at least one time to setup GUI even if there are no saves
    //Initiate refresh and process each new save added
    fSaves.Refresh(Load_RefreshList, fGame.MultiplayerMode);
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
  if Sender = nil then Label_Hint.Caption := ''
                else Label_Hint.Caption := TKMControl(Sender).Hint;
  PrevHint := Sender;
end;


{Update minimap data}
procedure TKMGamePlayInterface.Minimap_Update(Sender: TObject; const X,Y:integer);
begin
  fGame.Viewport.Position := KMPointF(X,Y);
  Minimap.ViewArea := fGame.Viewport.GetMinimapClip;
end;


procedure TKMGamePlayInterface.Minimap_RightClick(Sender: TObject; const X,Y:integer);
var
  KMP: TKMPoint;
begin
  KMP := Minimap.LocalToMapCoords(X, Y, -1); //Inset by 1 pixel to catch cases "outside of map"
  if not fTerrain.TileInMapCoords(KMP.X, KMP.Y) then Exit; //Must be inside map

  //Send move order, if applicable
  if (fShownUnit is TKMUnitWarrior) and (not fJoiningGroups)
  and fShownUnit.CanWalkTo(KMP, 0) then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyWalk, TKMUnitWarrior(fShownUnit), KMP, TKMUnitWarrior(fShownUnit).Direction);
    fSoundLib.PlayWarrior(fShownUnit.UnitType, sp_Move);
  end;
end;


constructor TKMGamePlayInterface.Create(aScreenX, aScreenY: word);
var i:integer;
begin
  inherited Create(aScreenX, aScreenY);

  //Instruct to use global Terrain
  fMapView := TKMMapView.Create(fTerrain, False, False);
  LastSaveName := '';
  fShownUnit:=nil;
  fShownHouse:=nil;
  fJoiningGroups := false;
  SelectingTroopDirection := false;
  SelectingDirPosition.X := 0;
  SelectingDirPosition.Y := 0;
  ShownMessage := -1; //0 is the first message, -1 is invalid

  LastSchoolUnit   := 0;
  LastBarracksUnit := 0;
  fMessageList := TKMMessageList.Create;
  fSaves := TKMSavesCollection.Create(fGame.MultiplayerMode);

{Parent Page for whole toolbar in-game}
  Panel_Main := TKMPanel.Create(fMyControls, 0, 0, aScreenX, aScreenY);

    Sidebar_Top       := TKMImage.Create(Panel_Main, 0,    0, 224, 200, 407);
    Sidebar_Middle    := TKMImage.Create(Panel_Main, 0,  200, 224, 168, 554);

    Minimap := TKMMinimap.Create(Panel_Main,10,10,176,176);
    Minimap.OnChange := Minimap_Update; //Allow dragging with LMB pressed
    Minimap.OnClickRight := Minimap_RightClick;

    Image_Clock := TKMImage.Create(Panel_Main,232,8,67,65,556);
    Image_Clock.Hide;
    Label_Clock := TKMLabel.Create(Panel_Main,265,80,0,0,'mm:ss',fnt_Outline,taCenter);
    Label_Clock.Hide;
    Label_ClockSpeedup := TKMLabel.Create(Panel_Main,265,48,0,0,'x1',fnt_Metal,taCenter);
    Label_ClockSpeedup.Hide;

    Image_DirectionCursor := TKMImage.Create(Panel_Main,0,0,35,36,519);
    Image_DirectionCursor.Hide;

    //Debugging displays
    Label_Stat := TKMLabel.Create(Panel_Main,224+80,16,0,0,'',fnt_Outline,taLeft);
    Label_Stat.Visible := SHOW_SPRITE_COUNT;
    Label_PointerCount := TKMLabel.Create(Panel_Main,224+80,80,0,0,'',fnt_Outline,taLeft);
    Label_PointerCount.Visible := SHOW_POINTER_COUNT;
    Label_CmdQueueCount := TKMLabel.Create(Panel_Main,224+80,110,0,0,'',fnt_Outline,taLeft);
    Label_CmdQueueCount.Visible := SHOW_CMDQUEUE_COUNT;
    Label_SoundsCount := TKMLabel.Create(Panel_Main,224+80,140,0,0,'',fnt_Outline,taLeft);
    Label_SoundsCount.Visible := DISPLAY_SOUNDS;
    Label_NetworkDelay := TKMLabel.Create(Panel_Main,224+80,140,0,0,'',fnt_Outline,taLeft);
    Label_NetworkDelay.Visible := SHOW_NETWORK_DELAY;
    Label_VictoryChance := TKMLabel.Create(Panel_Main, Panel_Main.Width - 150, 20, 0, 0, '', fnt_Outline, taLeft);

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

  Label_Hint := TKMLabel.Create(Panel_Main,224+32,Panel_Main.Height-16,0,0,'',fnt_Outline,taLeft);
  Label_Hint.Anchors := [akLeft, akBottom];

  //Controls without a hint will reset the Hint to ''
  fMyControls.OnHint := DisplayHint;

  if OVERLAY_RESOLUTIONS then
  begin
    with TKMShape.Create(Panel_Main, 0, 0, 800, 600, $FF00FFFF) do Hitable := False;
    with TKMShape.Create(Panel_Main, 0, 0, 1024, 768, $FF00FF00) do Hitable := False;
  end;

  SwitchPage(nil); //Update
  //Panel_Main.Width := aScreenX;
  //Panel_Main.Height := aScreenY;
  //UpdatePositions; //Reposition messages stack etc.
end;


destructor TKMGamePlayInterface.Destroy;
begin
  ReleaseDirectionSelector; //Make sure we don't exit leaving the cursor restrained
  fMessageList.Free;
  fSaves.Free;
  fMapView.Free;
  inherited;
end;


procedure TKMGamePlayInterface.Resize(X,Y: Word);
var ShowSwords: Boolean;
begin
  Panel_Main.Width := X;
  Panel_Main.Height := Y;

  //Show swords filler if screen height allows
  ShowSwords := (Panel_Main.Height >= Sidebar_Top.Height + Sidebar_Middle.Height + Panel_Controls.Height);
  Sidebar_Middle.Visible := ShowSwords;
  Panel_Controls.Top := Sidebar_Top.Height + Sidebar_Middle.Height * Byte(ShowSwords);
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
      Button_PlayMore := TKMButton.Create(Panel_PlayMoreMsg,0,100,200,30,'<<<LEER>>>',fnt_Metal);
      Button_PlayQuit := TKMButton.Create(Panel_PlayMoreMsg,0,140,200,30,'<<<LEER>>>',fnt_Metal);
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
      Button_MPPlayMore := TKMButton.Create(Panel_MPPlayMore,100,100,200,30,'<<<LEER>>>',fnt_Metal);
      Button_MPPlayQuit := TKMButton.Create(Panel_MPPlayMore,100,140,200,30,'<<<LEER>>>',fnt_Metal);
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
        Button_NetQuit := TKMButton.Create(Panel_NetWaitButtons,(Panel_Main.Width div 2)-150,0,300,30,fTextLibrary[TX_GAMEPLAY_QUIT_TO_MENU],fnt_Metal);
        Button_NetQuit.OnClick := NetWaitClick;
        Button_NetDropPlayers := TKMButton.Create(Panel_NetWaitButtons,(Panel_Main.Width div 2)-150,40,300,30,fTextLibrary[TX_GAMEPLAY_DROP_PLAYERS],fnt_Metal);
        Button_NetDropPlayers.OnClick := NetWaitClick;

      Panel_NetWaitConfirm := TKMPanel.Create(Panel_NetWaitMsg,0,180,Panel_Main.Width,140);
        Label_NetWaitConfirm := TKMLabel.Create(Panel_NetWaitConfirm,(Panel_Main.Width div 2),10,'<<<LEER>>>',fnt_Outline,taCenter);
        Button_NetConfirmYes := TKMButton.Create(Panel_NetWaitConfirm,(Panel_Main.Width div 2)-150,40,300,30,'<<<LEER>>>',fnt_Metal);
        Button_NetConfirmYes.OnClick := NetWaitClick;
        Button_NetConfirmNo := TKMButton.Create(Panel_NetWaitConfirm,(Panel_Main.Width div 2)-150,80,300,30,fTextLibrary[TX_GAMEPLAY_CONFIRM_CANCEL],fnt_Metal);
        Button_NetConfirmNo.OnClick := NetWaitClick;
      Panel_NetWaitConfirm.Hide;
    Panel_NetWait.Hide; //Initially hidden
end;


procedure TKMGamePlayInterface.Create_SideStack;
var i:integer;
begin
  Image_MPChat := TKMImage.Create(Panel_Main,TOOLBAR_WIDTH,Panel_Main.Height-48,30,48,494);
  Image_MPChat.Anchors := [akLeft, akBottom];
  Image_MPChat.HighlightOnMouseOver := true;
  Image_MPChat.Hint := fTextLibrary[TX_GAMEPLAY_CHAT_HINT];
  Image_MPChat.OnClick := Chat_Click;
  Label_MPChatUnread := TKMLabel.Create(Panel_Main,TOOLBAR_WIDTH+15,Panel_Main.Height-30,30,36,'',fnt_Outline,taCenter);
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
    Image_Message[I].Top := Panel_Main.Height - (I+1)*48 - IfThen(fGame.MultiplayerMode, 48*2);
    Image_Message[I].Anchors := [akLeft, akBottom];
    Image_Message[I].Disable;
    Image_Message[I].Hide;
    Image_Message[I].HighlightOnMouseOver := True;
    Image_Message[I].Tag := I;
    Image_Message[I].OnClick := Message_Click;
  end;

  //Chat and Allies setup should be accessible only in Multiplayer
  if not fGame.MultiplayerMode then begin
    Image_MPChat.Hide;
    Label_MPChatUnread.Hide;
    Image_MPAllies.Hide;
  end;
end;


procedure TKMGamePlayInterface.Create_Replay_Page;
begin
  Panel_Replay := TKMPanel.Create(Panel_Main, 0, 0, 1024, 768);
  Panel_Replay.Stretch;

    //Block all clicks except MinimapArea
    //We add one pixel to each side to make sure there are no gaps
    with TKMShape.Create(Panel_Replay, -1+196, -1, Panel_Main.Width+2-196, 196+2, $00000000) do
      Anchors := [akLeft, akTop, akRight];
    with TKMShape.Create(Panel_Replay, -1, -1+196, Panel_Main.Width+2, Panel_Main.Height+2-196, $00000000) do
      Anchors := [akLeft, akTop, akRight, akBottom];

    Panel_ReplayCtrl := TKMPanel.Create(Panel_Replay, 320, 8, 160, 60);
      PercentBar_Replay     := TKMPercentBar.Create(Panel_ReplayCtrl, 0, 0, 160, 20);
      Label_Replay          := TKMLabel.Create(Panel_ReplayCtrl,  80,  2, 160, 0, '<<<LEER>>>', fnt_Grey, taCenter);
      Button_ReplayRestart  := TKMButton.Create(Panel_ReplayCtrl,  0, 24, 24, 24, 'I<', fnt_Metal);
      Button_ReplayPause    := TKMButton.Create(Panel_ReplayCtrl, 25, 24, 24, 24, 'II', fnt_Metal);
      Button_ReplayStep     := TKMButton.Create(Panel_ReplayCtrl, 50, 24, 24, 24, '\\', fnt_Metal);
      Button_ReplayResume   := TKMButton.Create(Panel_ReplayCtrl, 75, 24, 24, 24, 'I>', fnt_Metal);
      Button_ReplayExit     := TKMButton.Create(Panel_ReplayCtrl,100, 24, 24, 24, 'X',  fnt_Metal);
      Button_ReplayRestart.OnClick := ReplayClick;
      Button_ReplayPause.OnClick   := ReplayClick;
      Button_ReplayStep.OnClick    := ReplayClick;
      Button_ReplayResume.OnClick  := ReplayClick;
      Button_ReplayExit.OnClick    := ReplayClick;
      Button_ReplayStep.Disable; //Initial state
      Button_ReplayResume.Disable; //Initial state
  Panel_Replay.Hide; //Initially hidden
end;


{Message page}
procedure TKMGamePlayInterface.Create_Message_Page;
begin
  Panel_Message:=TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - MESSAGE_AREA_HEIGHT, Panel_Main.Width - TOOLBAR_WIDTH, MESSAGE_AREA_HEIGHT);
  Panel_Message.Anchors := [akLeft, akRight, akBottom];
  Panel_Message.Hide; //Hide it now because it doesn't get hidden by SwitchPage

    TKMImage.Create(Panel_Message,0, 0,600, 20,551);
    TKMImage.Create(Panel_Message,0,20,600,170,409);

    Label_MessageText:=TKMLabel.Create(Panel_Message,47,67,432,122,'',fnt_Antiqua,taLeft);
    Label_MessageText.AutoWrap := true;

    Button_MessageGoTo:=TKMButton.Create(Panel_Message,490,74,100,24,fTextLibrary[TX_MSG_GOTO],fnt_Antiqua);
    Button_MessageGoTo.Hint := fTextLibrary[TX_MSG_GOTO_HINT];
    Button_MessageGoTo.OnClick := Message_GoTo;

    Button_MessageDelete:=TKMButton.Create(Panel_Message,490,104,100,24,fTextLibrary[TX_MSG_DELETE],fnt_Antiqua);
    Button_MessageDelete.Hint := fTextLibrary[TX_MSG_DELETE_HINT];
    Button_MessageDelete.OnClick := Message_Delete;
    Button_MessageDelete.MakesSound := false; //Don't play default Click as these buttons use sfx_MessageClose

    Button_MessageClose:=TKMButton.Create(Panel_Message,490,134,100,24,fTextLibrary[TX_MSG_CLOSE],fnt_Antiqua);
    Button_MessageClose.Hint := fTextLibrary[TX_MSG_CLOSE_HINT];
    Button_MessageClose.OnClick := Message_Close;
    Button_MessageClose.MakesSound := false; //Don't play default Click as these buttons use sfx_MessageClose
end;


{Chat page}
procedure TKMGamePlayInterface.Create_Chat_Page;
begin
  Panel_Chat := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - MESSAGE_AREA_HEIGHT, Panel_Main.Width - TOOLBAR_WIDTH, MESSAGE_AREA_HEIGHT);
  Panel_Chat.Anchors := [akLeft, akBottom];
  Panel_Chat.Hide;

    Image_ChatHead := TKMImage.Create(Panel_Chat,0,0,800,17,552);
    Image_ChatHead.Anchors := [akLeft, akTop, akRight];
    Image_ChatHead.ImageStretch;
    Image_ChatBody := TKMImage.Create(Panel_Chat,0,17,800,173,410);
    Image_ChatBody.Anchors := [akLeft, akTop, akRight, akBottom];
    Image_ChatBody.ImageStretch;

    //Allow to resize chat area height
    Dragger_Chat := TKMDragger.Create(Panel_Chat, 45, 36, 800-85, 10);
    Dragger_Chat.Anchors := [akTop];
    Dragger_Chat.SetBounds(0, -MESSAGE_AREA_RESIZE_Y, 0, 0);
    Dragger_Chat.OnMove := Chat_Resize;

    Memo_ChatText := TKMMemo.Create(Panel_Chat,45,50,800-85,101,fnt_Metal);
    Memo_ChatText.ScrollDown := True;
    Memo_ChatText.AutoWrap := True;
    Memo_ChatText.Anchors := [akLeft, akTop, akRight, akBottom];

    Edit_ChatMsg := TKMEdit.Create(Panel_Chat, 45, 151, 680-85, 20, fnt_Metal);
    Edit_ChatMsg.Anchors := [akLeft, akRight, akBottom];
    Edit_ChatMsg.OnKeyDown := Chat_Post;
    Edit_ChatMsg.Text := '';

    CheckBox_SendToAllies := TKMCheckBox.Create(Panel_Chat,645,154,155,20,fTextLibrary[TX_GAMEPLAY_CHAT_TOTEAM],fnt_Outline);
    CheckBox_SendToAllies.Checked := true;
    CheckBox_SendToAllies.Anchors := [akRight, akBottom];

    Image_ChatClose:=TKMImage.Create(Panel_Chat,800-35,20,32,32,24,rxGame);
    Image_ChatClose.Anchors := [akTop, akRight];
    Image_ChatClose.Hint := fTextLibrary[TX_MSG_CLOSE_HINT];
    Image_ChatClose.OnClick := Chat_Close;
    Image_ChatClose.HighlightOnMouseOver := true;
end;


procedure TKMGamePlayInterface.Create_Controls_Page;
var I: Integer;
begin
  Panel_Controls := TKMPanel.Create(Panel_Main, 0, 368, 224, 400);
  Panel_Controls.Anchors := [akLeft, akTop];

    //We need several of these to cover max of 1534x2560 (vertically oriented)
    TKMImage.Create(Panel_Controls, 0,    0, 224, 400, 404);
    TKMImage.Create(Panel_Controls, 0,  400, 224, 400, 404);
    TKMImage.Create(Panel_Controls, 0,  800, 224, 400, 404);
    TKMImage.Create(Panel_Controls, 0, 1200, 224, 400, 404);
    TKMImage.Create(Panel_Controls, 0, 1600, 224, 400, 404);
    TKMImage.Create(Panel_Controls, 0, 2000, 224, 400, 404);

    //Main 4 buttons
    for i := 0 to 3 do begin
      Button_Main[i+1] := TKMButton.Create(Panel_Controls,  8+46*i, 4, 42, 36, 439+i);
      Button_Main[i+1].OnClick := SwitchPage;
    end;
    Button_Main[1].Hint := fTextLibrary[TX_MENU_TAB_HINT_BUILD];
    Button_Main[2].Hint := fTextLibrary[TX_MENU_TAB_HINT_DISTRIBUTE];
    Button_Main[3].Hint := fTextLibrary[TX_MENU_TAB_HINT_STATISTICS];
    Button_Main[4].Hint := fTextLibrary[TX_MENU_TAB_HINT_OPTIONS];

    Button_Main[5] := TKMButton.Create(Panel_Controls, 8, 4, 42, 36, 443);
    Button_Main[5].OnClick := SwitchPage;
    Button_Main[5].Hint := fTextLibrary[TX_MENU_TAB_HINT_GO_BACK];
    Label_MenuTitle := TKMLabel.Create(Panel_Controls, 54, 4, 138, 36, '', fnt_Metal, taLeft);

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
var i,k:integer;
begin
  Panel_Allies := TKMPanel.Create(Panel_Main, TOOLBAR_WIDTH, Panel_Main.Height - MESSAGE_AREA_HEIGHT, Panel_Main.Width - TOOLBAR_WIDTH, MESSAGE_AREA_HEIGHT);
  Panel_Allies.Anchors := [akLeft, akRight, akBottom];
  Panel_Allies.Hide;

    TKMImage.Create(Panel_Allies,0,0,800,17,552);
    TKMImage.Create(Panel_Allies,0,17,800,173,410);

    Label_PeacetimeRemaining := TKMLabel.Create(Panel_Allies,400,20,800,20,'',fnt_Outline,taCenter);

    for i:=0 to MAX_PLAYERS-1 do
    begin
      if (i mod 4) = 0 then //Header for each column
      begin
        TKMLabel.Create(Panel_Allies,  70+(i div 4)*380, 60, 140, 20, fTextLibrary[TX_LOBBY_HEADER_PLAYERS], fnt_Outline, taLeft);
        TKMLabel.Create(Panel_Allies, 220+(i div 4)*380, 60, 140, 20, fTextLibrary[TX_LOBBY_HEADER_TEAM], fnt_Outline, taLeft);
        TKMLabel.Create(Panel_Allies, 350+(i div 4)*380, 60, 140, 20, fTextLibrary[TX_LOBBY_HEADER_PING], fnt_Outline, taCenter);
      end;
      Image_AlliesLang[i] := TKMImage.Create(Panel_Allies,      50+(i div 4)*380, 82+(i mod 4)*24, 16,  11,  0, rxMenu);
      Label_AlliesPlayer[i] := TKMLabel.Create(Panel_Allies,    70+(i div 4)*380, 80+(i mod 4)*24, 140, 20, '', fnt_Grey, taLeft);
      Label_AlliesTeam[i]   := TKMLabel.Create(Panel_Allies,   220+(i div 4)*380, 80+(i mod 4)*24, 120, 20, '', fnt_Grey, taLeft);
      DropBox_AlliesTeam[i] := TKMDropBox.Create(Panel_Allies, 220+(i div 4)*380, 80+(i mod 4)*24, 120, 20, fnt_Grey, '');
      DropBox_AlliesTeam[i].Hide; //Use label for demos until we fix exploits
      DropBox_AlliesTeam[i].Add(fTextLibrary[TX_LOBBY_NONE]);
      for k:=1 to 4 do DropBox_AlliesTeam[i].Add(Format(fTextLibrary[TX_LOBBY_TEAM_X],[k]));
      DropBox_AlliesTeam[i].OnChange := AlliesTeamChange;
      DropBox_AlliesTeam[i].DropUp := true; //Doesn't fit if it drops down
      Label_AlliesPing[i]   := TKMLabel.Create(Panel_Allies,   350+(i div 4)*380, 80+(i mod 4)*24, 60, 20, '', fnt_Grey, taCenter);
    end;

    Image_AlliesClose:=TKMImage.Create(Panel_Allies,800-35,20,32,32,24,rxGame);
    Image_AlliesClose.Hint := fTextLibrary[TX_MSG_CLOSE_HINT];
    Image_AlliesClose.OnClick := Allies_Close;
    Image_AlliesClose.HighlightOnMouseOver := true;
end;


{Build page}
procedure TKMGamePlayInterface.Create_Build_Page;
var I: Integer;
begin
  Panel_Build := TKMPanel.Create(Panel_Controls, 0, 44, 196, 400);
    Label_Build := TKMLabel.Create(Panel_Build, 100, 10, 184, 30, '', fnt_Outline, taCenter);
    Image_Build_Selected := TKMImage.Create(Panel_Build, 8, 40, 32, 32, 335);
    Image_Build_Selected.ImageCenter;
    Image_BuildCost_WoodPic := TKMImage.Create(Panel_Build, 75, 40, 32, 32, 353);
    Image_BuildCost_WoodPic.ImageCenter;
    Image_BuildCost_StonePic := TKMImage.Create(Panel_Build, 130, 40, 32, 32, 352);
    Image_BuildCost_StonePic.ImageCenter;
    Label_BuildCost_Wood  := TKMLabel.Create(Panel_Build, 105, 50, 20, 20, '', fnt_Outline, taLeft);
    Label_BuildCost_Stone := TKMLabel.Create(Panel_Build, 160, 50, 20, 20, '', fnt_Outline, taLeft);

    Button_BuildRoad    := TKMButtonFlat.Create(Panel_Build, 8, 80, 33, 33, 335);
    Button_BuildField   := TKMButtonFlat.Create(Panel_Build, 45, 80, 33, 33, 337);
    Button_BuildWine    := TKMButtonFlat.Create(Panel_Build, 82, 80, 33, 33, 336);
    Button_BuildCancel  := TKMButtonFlat.Create(Panel_Build, 156, 80, 33, 33, 340);
    Button_BuildRoad.OnClick    := Build_ButtonClick;
    Button_BuildField.OnClick   := Build_ButtonClick;
    Button_BuildWine.OnClick    := Build_ButtonClick;
    Button_BuildCancel.OnClick  := Build_ButtonClick;
    Button_BuildRoad.Hint   := fTextLibrary[TX_BUILD_ROAD_HINT];
    Button_BuildField.Hint  := fTextLibrary[TX_BUILD_FIELD_HINT];
    Button_BuildWine.Hint   := fTextLibrary[TX_BUILD_WINE_HINT];
    Button_BuildCancel.Hint := fTextLibrary[TX_BUILD_CANCEL_HINT];

    for i:=1 to GUI_HOUSE_COUNT do
      if GUIHouseOrder[i] <> ht_None then begin
        Button_Build[i] := TKMButtonFlat.Create(Panel_Build, 8+((i-1) mod 5)*37,120+((i-1) div 5)*37,33,33,
        fResource.HouseDat[GUIHouseOrder[i]].GUIIcon);

        Button_Build[i].OnClick:=Build_ButtonClick;
        Button_Build[i].Hint := fResource.HouseDat[GUIHouseOrder[i]].HouseName;
      end;
end;


{Ratios page}
procedure TKMGamePlayInterface.Create_Ratios_Page;
const Res:array[1..4] of TResourceType = (rt_Steel,rt_Coal,rt_Wood,rt_Corn);
var i:integer;
begin
  Panel_Ratios:=TKMPanel.Create(Panel_Controls, 0, 44, 196, 400);

  for i:=1 to 4 do begin
    Button_Ratios[i]         := TKMButton.Create(Panel_Ratios, 8+(i-1)*40,20,32,32,0);
    Button_Ratios[i].TexID   := fResource.Resources[Res[i]].GUIIcon;
    Button_Ratios[i].Hint    := fResource.Resources[Res[i]].Name;
    Button_Ratios[i].Tag     := i;
    Button_Ratios[i].OnClick := SwitchPage_Ratios;
  end;

  Image_RatioPic0 := TKMImage.Create(Panel_Ratios,12,76,32,32,327);
  Label_RatioLab0 := TKMLabel.Create(Panel_Ratios,44,72,148,30,'<<<LEER>>>',fnt_Outline,taLeft);

  for i:=1 to 4 do begin
    Image_RatioPic[i]         := TKMImage.Create(Panel_Ratios,12,124+(i-1)*50,32,32,327);
    //Label_RatioLab[i]         := TKMLabel.Create(Panel_Ratios,50,116+(i-1)*50,148,20,'<<<LEER>>>',fnt_Grey,taLeft);
    TrackBar_RatioRat[i]         := TKMTrackBar.Create(Panel_Ratios,48,116+(i-1)*50,140,0,5);
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
  Panel_Stats:=TKMPanel.Create(Panel_Controls, 0, 44, 196, 400);

  hc:=1; uc:=1;
  for i:=1 to 8 do begin
    LineBase := (i-1)*LineHeight;
    case i of
    1: begin
          TKMBevel.Create(Panel_Stats,  8,LineBase, 56,30);
          TKMBevel.Create(Panel_Stats, 71,LineBase, 56,30);
          TKMBevel.Create(Panel_Stats,134,LineBase, 56,30);
       end;
    2: begin
          TKMBevel.Create(Panel_Stats,  8,LineBase, 86,30);
          TKMBevel.Create(Panel_Stats,104,LineBase, 86,30);
       end;
    3: begin
          TKMBevel.Create(Panel_Stats,  8,LineBase, 86,30);
          TKMBevel.Create(Panel_Stats,104,LineBase, 86,30);
       end;
    4: begin
          TKMBevel.Create(Panel_Stats,  8,LineBase, 86,30);
          TKMBevel.Create(Panel_Stats,104,LineBase, 86,30);
       end;
    5:    TKMBevel.Create(Panel_Stats,  8,LineBase,116,30);
    6:    TKMBevel.Create(Panel_Stats,  8,LineBase,116,30);
    7: begin
         TKMBevel.Create(Panel_Stats,  8,LineBase, 32,30);
         TKMBevel.Create(Panel_Stats,  78,LineBase, 86,30);
       end;
    8: begin
          TKMBevel.Create(Panel_Stats,  8,LineBase, 90,30);
          TKMBevel.Create(Panel_Stats,120,LineBase, 52,30);
       end;
    end;

    off:=8;
    for k:=1 to 8 do
    case StatCount[i,k] of
      0: if i=1 then
           inc(off,Nil_Width-3) //Special fix to fit first row of 3x2 items
         else
           inc(off,Nil_Width);
      1: begin
          Stat_HousePic[hc] := TKMImage.Create(Panel_Stats,off,LineBase,House_Width,30,41); //Filled with [?] at start
          Stat_HouseWip[hc] := TKMLabel.Create(Panel_Stats,off+House_Width  ,LineBase   ,30,15,'',fnt_Grey,taRight);
          Stat_HouseQty[hc] := TKMLabel.Create(Panel_Stats,off+House_Width-2,LineBase+16,30,15,'-',fnt_Grey,taRight);
          Stat_HousePic[hc].Hint := fResource.HouseDat[StatHouse[hc]].HouseName;
          Stat_HouseQty[hc].Hitable := False;
          Stat_HouseWip[hc].Hitable := False;
          Stat_HousePic[hc].ImageCenter;
          inc(hc);
          inc(off,House_Width);
         end;
      2: begin
          Stat_UnitPic[uc] := TKMImage.Create(Panel_Stats,off,LineBase,Unit_Width,30, fResource.UnitDat[StatUnit[uc]].GUIIcon);
          Stat_UnitQty[uc] := TKMLabel.Create(Panel_Stats,off+Unit_Width-2,LineBase+16,33,15,'-',fnt_Grey,taRight);
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
  Panel_Menu:=TKMPanel.Create(Panel_Controls, 0, 44, 196, 400);
    Button_Menu_Load:=TKMButton.Create(Panel_Menu,8,20,180,30,fTextLibrary[TX_MENU_LOAD_GAME],fnt_Metal);
    Button_Menu_Load.OnClick:=SwitchPage;
    Button_Menu_Load.Hint:=fTextLibrary[TX_MENU_LOAD_GAME];
    Button_Menu_Save:=TKMButton.Create(Panel_Menu,8,60,180,30,fTextLibrary[TX_MENU_SAVE_GAME],fnt_Metal);
    Button_Menu_Save.OnClick:=SwitchPage;
    Button_Menu_Save.Hint:=fTextLibrary[TX_MENU_SAVE_GAME];
    Button_Menu_Settings:=TKMButton.Create(Panel_Menu,8,100,180,30,fTextLibrary[TX_MENU_SETTINGS],fnt_Metal);
    Button_Menu_Settings.OnClick:=SwitchPage;
    Button_Menu_Settings.Hint:=fTextLibrary[TX_MENU_SETTINGS];
    Button_Menu_Quit:=TKMButton.Create(Panel_Menu,8,180,180,30,fTextLibrary[TX_MENU_QUIT_MISSION],fnt_Metal);
    Button_Menu_Quit.Hint:=fTextLibrary[TX_MENU_QUIT_MISSION];
    Button_Menu_Quit.OnClick:=SwitchPage;
    Button_Menu_TrackUp   := TKMButton.Create(Panel_Menu,158,320,30,30, '>', fnt_Metal);
    Button_Menu_TrackDown := TKMButton.Create(Panel_Menu,  8,320,30,30, '<', fnt_Metal);
    Button_Menu_TrackUp.Hint    := fTextLibrary[TX_MUSIC_NEXT_HINT];
    Button_Menu_TrackDown.Hint  := fTextLibrary[TX_MUSIC_PREV_HINT];
    Button_Menu_TrackUp.OnClick   := Menu_NextTrack;
    Button_Menu_TrackDown.OnClick := Menu_PreviousTrack;
    TKMLabel.Create(Panel_Menu,100,276,184,30,fTextLibrary[TX_MUSIC_PLAYER],fnt_Metal,taCenter);
    Label_Menu_Track:=TKMLabel.Create(Panel_Menu,100,296,184,30,'',fnt_Grey,taCenter);
    Label_Menu_Track.Hitable := false; //It can block hits for the track Up/Down buttons as they overlap
    Label_GameTime := TKMLabel.Create(Panel_Menu,100,228,184,20,'',fnt_Outline,taCenter);
end;


{Save page}
procedure TKMGamePlayInterface.Create_Save_Page;
begin
  Panel_Save := TKMPanel.Create(Panel_Controls, 0, 44, 196, 400);

    List_Save := TKMListBox.Create(Panel_Save, 12, 4, 170, 240, fnt_Metal);
    List_Save.OnChange := Save_ListChange;

    Edit_Save := TKMEdit.Create(Panel_Save, 12, 255, 170, 20, fnt_Metal);
    Edit_Save.AllowedChars := acFileName;
    Edit_Save.OnChange := Save_EditChange;
    Label_SaveExists := TKMLabel.Create(Panel_Save,12,280,170,30,fTextLibrary[TX_GAMEPLAY_SAVE_EXISTS],fnt_Outline,taLeft);
    CheckBox_SaveExists := TKMCheckBox.Create(Panel_Save,12,300,170,20,fTextLibrary[TX_GAMEPLAY_SAVE_OVERWRITE], fnt_Metal);
    CheckBox_SaveExists.OnClick := Save_CheckboxChange;

    Button_Save := TKMButton.Create(Panel_Save,12,320,170,30,fTextLibrary[TX_GAMEPLAY_SAVE_SAVE],fnt_Metal, bsMenu);
    Button_Save.OnClick := Save_Click;
end;


{Load page}
procedure TKMGamePlayInterface.Create_Load_Page;
begin
  Panel_Load := TKMPanel.Create(Panel_Controls, 0, 44, 196, 400);

    List_Load := TKMListBox.Create(Panel_Load, 12, 2, 170, 280, fnt_Metal);
    List_Load.OnChange := Load_ListClick;

    Label_Load_Description := TKMLabel.Create(Panel_Load,12,285,170,40,'',fnt_Grey,taLeft);
    Label_Load_Description.AutoWrap := true;

    Button_Load := TKMButton.Create(Panel_Load,12,320,170,30,fTextLibrary[TX_GAMEPLAY_LOAD],fnt_Metal, bsMenu);
    Button_Load.OnClick := Load_Click;
end;


{Options page}
procedure TKMGamePlayInterface.Create_Settings_Page;
begin
  Panel_Settings := TKMPanel.Create(Panel_Controls, 0, 44, 196, 400);
    CheckBox_Settings_Autosave := TKMCheckBox.Create(Panel_Settings,18,15,180,20,fTextLibrary[TX_MENU_OPTIONS_AUTOSAVE],fnt_Metal);
    CheckBox_Settings_Autosave.OnClick := Menu_Settings_Change;
    TrackBar_Settings_Brightness := TKMTrackBar.Create(Panel_Settings,18,40,160,0,20);
    TrackBar_Settings_Brightness.Caption := fTextLibrary[TX_MENU_OPTIONS_BRIGHTNESS];
    TrackBar_Settings_Brightness.OnChange := Menu_Settings_Change;
    TrackBar_Settings_ScrollSpeed := TKMTrackBar.Create(Panel_Settings,18,95,160,0,20);
    TrackBar_Settings_ScrollSpeed.Caption := fTextLibrary[TX_MENU_OPTIONS_SCROLL_SPEED];
    TrackBar_Settings_ScrollSpeed.OnChange := Menu_Settings_Change;
    TrackBar_Settings_SFX := TKMTrackBar.Create(Panel_Settings,18,150,160,0,20);
    TrackBar_Settings_SFX.Caption := fTextLibrary[TX_MENU_SFX_VOLUME];
    TrackBar_Settings_SFX.Hint := fTextLibrary[TX_MENU_SFX_VOLUME_HINT];
    TrackBar_Settings_SFX.OnChange := Menu_Settings_Change;
    TrackBar_Settings_Music := TKMTrackBar.Create(Panel_Settings,18,205,160,0,20);
    TrackBar_Settings_Music.Caption := fTextLibrary[TX_MENU_MUSIC_VOLUME];
    TrackBar_Settings_Music.Hint := fTextLibrary[TX_MENU_MUSIC_VOLUME_HINT];
    TrackBar_Settings_Music.OnChange := Menu_Settings_Change;
    CheckBox_Settings_MusicOff := TKMCheckBox.Create(Panel_Settings,18,260,180,20,fTextLibrary[TX_MENU_OPTIONS_MUSIC_DISABLE],fnt_Metal);
    CheckBox_Settings_MusicOff.Hint := fTextLibrary[TX_MENU_OPTIONS_MUSIC_DISABLE_HINT];
    CheckBox_Settings_MusicOff.OnClick := Menu_Settings_Change;
    CheckBox_Settings_ShuffleOn := TKMCheckBox.Create(Panel_Settings,18,285,180,20,fTextLibrary[TX_MENU_OPTIONS_MUSIC_SHUFFLE],fnt_Metal);
    CheckBox_Settings_ShuffleOn.OnClick := Menu_Settings_Change;
end;


{Quit page}
procedure TKMGamePlayInterface.Create_Quit_Page;
begin
  Panel_Quit := TKMPanel.Create(Panel_Controls, 0, 44, 196, 400);
    TKMLabel.Create(Panel_Quit, 100, 30, 180, 70, fTextLibrary[TX_MENU_QUIT_QUESTION], fnt_Outline, taCenter);
    Button_Quit_Yes := TKMButton.Create(Panel_Quit, 8, 100, 180, 30, fTextLibrary[TX_MENU_QUIT_MISSION], fnt_Metal);
    Button_Quit_No := TKMButton.Create(Panel_Quit, 8, 140, 180, 30, fTextLibrary[TX_MENU_DONT_QUIT_MISSION], fnt_Metal);
    Button_Quit_Yes.Hint := fTextLibrary[TX_MENU_QUIT_MISSION];
    Button_Quit_No.Hint := fTextLibrary[TX_MENU_DONT_QUIT_MISSION];
    Button_Quit_Yes.OnClick := Menu_QuitMission;
    Button_Quit_No.OnClick := SwitchPage;
end;


{Unit page}
procedure TKMGamePlayInterface.Create_Unit_Page;
begin
  Panel_Unit := TKMPanel.Create(Panel_Controls, 0, 44, 196, 400);
    Label_UnitName        := TKMLabel.Create(Panel_Unit,100,16,184,30,'',fnt_Outline,taCenter);
    Image_UnitPic         := TKMImage.Create(Panel_Unit,8,38,54,100,521);
    Label_UnitCondition   := TKMLabel.Create(Panel_Unit,132,40,116,30,fTextLibrary[TX_UNIT_CONDITION],fnt_Grey,taCenter);
    ConditionBar_Unit     := TKMPercentBar.Create(Panel_Unit,73,55,116,15);
    Label_UnitTask        := TKMLabel.Create(Panel_Unit,73,80,116,30,'',fnt_Grey,taLeft);
    Label_UnitTask.AutoWrap := true;
    Label_UnitDescription := TKMLabel.Create(Panel_Unit,8,152,184,200,'',fnt_Grey,taLeft); //Taken from LIB resource
    Button_Unit_Dismiss   := TKMButton.Create(Panel_Unit,132,120,56,34,29);

  Panel_Unit_Dismiss := TKMPanel.Create(Panel_Unit,0,160,200,400);
    Label_Unit_Dismiss             := TKMLabel.Create(Panel_Unit_Dismiss,100,16,184,30,'Are you sure?',fnt_Outline,taCenter);
    Button_Unit_DismissYes         := TKMButton.Create(Panel_Unit_Dismiss,50, 50,100,40,'Dismiss',fnt_Metal);
    Button_Unit_DismissNo          := TKMButton.Create(Panel_Unit_Dismiss,50,100,100,40,'Cancel',fnt_Metal);
    Button_Unit_DismissYes.OnClick := Unit_Dismiss;
    Button_Unit_DismissNo.OnClick  := Unit_Dismiss;

  Panel_Army := TKMPanel.Create(Panel_Unit,0,160,200,400);
    //Military buttons start at 8.170 and are 52x38/30 (60x46)
    Button_Army_GoTo   := TKMButton.Create(Panel_Army,  8,  0, 56, 40, 27);
    Button_Army_Stop   := TKMButton.Create(Panel_Army, 70,  0, 56, 40, 26);
    Button_Army_Attack := TKMButton.Create(Panel_Army,132,  0, 56, 40, 25);
    Button_Army_RotCCW := TKMButton.Create(Panel_Army,  8, 46, 56, 40, 23);
    Button_Army_Storm  := TKMButton.Create(Panel_Army, 70, 46, 56, 40, 28);
    Button_Army_RotCW  := TKMButton.Create(Panel_Army,132, 46, 56, 40, 24);
    Button_Army_ForUp  := TKMButton.Create(Panel_Army,  8, 92, 56, 40, 33);
    ImageStack_Army    := TKMImageStack.Create(Panel_Army, 70, 92, 56, 40, 43);
    Button_Army_ForDown:= TKMButton.Create(Panel_Army,132, 92, 56, 40, 32);
    Button_Army_Split  := TKMButton.Create(Panel_Army,  8,138, 56, 34, 31);
    Button_Army_Join   := TKMButton.Create(Panel_Army, 70,138, 56, 34, 30);
    Button_Army_Feed   := TKMButton.Create(Panel_Army,132,138, 56, 34, 29);

    //All one-click-action (i.e. not attack, move, link up) army controls have a single procedure that decides what to do based on Sender
    Button_Army_GoTo.OnClick   := Army_Issue_Order;
    Button_Army_Stop.OnClick   := Army_Issue_Order;
    Button_Army_Attack.OnClick := Army_Issue_Order;
    Button_Army_RotCW.OnClick  := Army_Issue_Order;
    Button_Army_Storm.OnClick  := Army_Issue_Order;
    Button_Army_RotCCW.OnClick := Army_Issue_Order;
    Button_Army_ForDown.OnClick:= Army_Issue_Order;
    Button_Army_ForUp.OnClick  := Army_Issue_Order;
    Button_Army_Split.OnClick  := Army_Issue_Order;
    Button_Army_Join.OnClick   := Army_Issue_Order;
    Button_Army_Feed.OnClick   := Army_Issue_Order;
    Button_Unit_Dismiss.OnClick:= Army_Issue_Order;

    //Disable not working buttons
    Button_Army_GoTo.Disable;
    Button_Army_Attack.Disable;

    //Hints
    //@Lewin: I suggest we check other games, but I have a feeling that using same shortcuts for every version would be better
    //@Krom: Yes we should check other games. I did it this way to match KaM TSK/TPR.
    Button_Army_GoTo.Hint   := fTextLibrary[TX_ARMY_GOTO_HINT];
    Button_Army_Stop.Hint   := Format(fTextLibrary[TX_TROOP_HALT_HINT], [fTextLibrary[TX_SHORTCUT_KEY_TROOP_HALT]]);
    Button_Army_Attack.Hint := fTextLibrary[TX_ARMY_ATTACK_HINT];
    Button_Army_RotCW.Hint  := fTextLibrary[TX_ARMY_ROTATE_CW_HINT];
    Button_Army_Storm.Hint  := fTextLibrary[TX_ARMY_STORM_HINT];
    Button_Army_RotCCW.Hint := fTextLibrary[TX_ARMY_ROTATE_CCW_HINT];
    Button_Army_ForDown.Hint:= fTextLibrary[TX_ARMY_LINE_ADD_HINT];
    Button_Army_ForUp.Hint  := fTextLibrary[TX_ARMY_LINE_REM_HINT];
    Button_Army_Split.Hint  := Format(fTextLibrary[TX_TROOP_SPLIT_HINT], [fTextLibrary[TX_SHORTCUT_KEY_TROOP_SPLIT]]);
    Button_Army_Join.Hint   := Format(fTextLibrary[TX_TROOP_LINK_HINT], [fTextLibrary[TX_SHORTCUT_KEY_TROOP_LINK]]);
    Button_Army_Feed.Hint   := fTextLibrary[TX_ARMY_FEED_HINT];
    Button_Unit_Dismiss.Hint:= 'Dismiss unit';

    {Army controls...
    Go to     Stop      Attack
    Rotate    Storm     Rotate
    -Column   [Info]    +Column
    Split     Join      Feed}

  Panel_Army_JoinGroups:=TKMPanel.Create(Panel_Unit,0,160,200,400);
    Label_Army_Join_Message := TKMLabel.Create(Panel_Army_JoinGroups, 98, 30, 188, 65, fTextLibrary[TX_ARMY_JOIN_SELECT], fnt_Outline, taCenter);
    Button_Army_Join_Cancel := TKMButton.Create(Panel_Army_JoinGroups, 8, 95, 180, 30, fTextLibrary[TX_ARMY_JOIN_CANCEL], fnt_Metal);

  Button_Army_Join_Cancel.OnClick := Army_HideJoinMenu;
end;


{House description page}
procedure TKMGamePlayInterface.Create_House_Page;
var i:integer;
begin
  Panel_House := TKMPanel.Create(Panel_Controls, 0, 44, 196, 400);
    //Thats common things
    //Custom things come in fixed size blocks (more smaller Panels?), and to be shown upon need
    Label_House := TKMLabel.Create(Panel_House,100,14,184,20,'',fnt_Outline,taCenter);
    Button_House_Goods := TKMButton.Create(Panel_House,8,42,30,30,37);
    Button_House_Goods.Hint := fTextLibrary[TX_HOUSE_TOGGLE_DELIVERS_HINT];
    Button_House_Goods.OnClick := House_WareDeliveryToggle;
    Button_House_Repair := TKMButton.Create(Panel_House,38,42,30,30,40);
    Button_House_Repair.Hint := fTextLibrary[TX_HOUSE_TOGGLE_REPAIR_HINT];
    Button_House_Repair.OnClick := House_RepairToggle;
    Image_House_Logo := TKMImage.Create(Panel_House,68,41,32,32,338);
    Image_House_Logo.ImageCenter;
    Image_House_Worker := TKMImage.Create(Panel_House,98,41,32,32,141);
    Image_House_Worker.ImageCenter;
    Label_HouseHealth := TKMLabel.Create(Panel_House,156,45,55,15,fTextLibrary[TX_HOUSE_CONDITION],fnt_Mini,taCenter);
    Label_HouseHealth.FontColor := $FFE0E0E0;

    HealthBar_House := TKMPercentBar.Create(Panel_House,129,57,55,15);
    Label_House_UnderConstruction := TKMLabel.Create(Panel_House,100,170,184,100,fTextLibrary[TX_HOUSE_UNDER_CONSTRUCTION],fnt_Grey,taCenter);

    Label_House_Demolish := TKMLabel.Create(Panel_House,100,130,184,55,fTextLibrary[TX_HOUSE_DEMOLISH],fnt_Grey,taCenter);
    Button_House_DemolishYes := TKMButton.Create(Panel_House,8,185,180,30,fTextLibrary[TX_HOUSE_DEMOLISH_YES],fnt_Metal);
    Button_House_DemolishNo  := TKMButton.Create(Panel_House,8,220,180,30,fTextLibrary[TX_HOUSE_DEMOLISH_NO],fnt_Metal);
    Button_House_DemolishYes.Hint := fTextLibrary[TX_HOUSE_DEMOLISH_YES_HINT];
    Button_House_DemolishNo.Hint  := fTextLibrary[TX_HOUSE_DEMOLISH_NO];
    Button_House_DemolishYes.OnClick := House_Demolish;
    Button_House_DemolishNo.OnClick  := House_Demolish;

    Panel_House_Common := TKMPanel.Create(Panel_House,0,76,200,400);
      Label_Common_Demand := TKMLabel.Create(Panel_House_Common,100,2,184,30,fTextLibrary[TX_HOUSE_NEEDS],fnt_Grey,taCenter);
      Label_Common_Offer  := TKMLabel.Create(Panel_House_Common,100,2,184,30,'',fnt_Grey,taCenter);
      Label_Common_Costs  := TKMLabel.Create(Panel_House_Common,100,2,184,30,fTextLibrary[TX_HOUSE_GOOD_COST],fnt_Grey,taCenter);

      //They get repositioned on display
      for i:=1 to 4 do
      begin
        ResRow_Common_Resource[i] := TKMResourceRow.Create(Panel_House_Common, 8,22,180,20);
        ResRow_Common_Resource[i].RX := rxGui;

        ResRow_Order[i] := TKMResourceOrderRow.Create(Panel_House_Common, 8,22,180,20);
        ResRow_Order[i].RX := rxGui;
        ResRow_Order[i].OrderRem.OnClickEither := House_OrderClick;
        ResRow_Order[i].OrderAdd.OnClickEither := House_OrderClick;
        ResRow_Order[i].OrderRem.Hint          := fTextLibrary[TX_HOUSE_ORDER_DEC_HINT];
        ResRow_Order[i].OrderAdd.Hint          := fTextLibrary[TX_HOUSE_ORDER_INC_HINT];

        ResRow_Costs[i] := TKMCostsRow.Create(Panel_House_Common, 8,22,180,20);
        ResRow_Costs[i].RX := rxGui;
      end;
end;


{Market page}
procedure TKMGamePlayInterface.Create_Market_Page;
var i:integer; LineH:integer;
begin
  Panel_HouseMarket := TKMPanel.Create(Panel_House,0,76,200,400);
    for i:=0 to STORE_RES_COUNT-1 do
    begin
      Button_Market[i] := TKMButtonFlat.Create(Panel_HouseMarket, 8+(i mod 6)*30,12+(i div 6)*34,26,30,0);
      Button_Market[i].TexID := fResource.Resources[StoreResType[i+1]].GUIIcon;
      Button_Market[i].Hint := fResource.Resources[StoreResType[i+1]].Name;
      Button_Market[i].Tag := Byte(StoreResType[i+1]);
      Button_Market[i].OnClickEither := House_MarketSelect;
    end;
  Shape_Market_From := TKMShape.Create(Panel_HouseMarket, 0, 0, 26, 30, $FF00B000);
  Shape_Market_From.LineWidth := 2;
  Shape_Market_From.Hitable := False;
  Shape_Market_From.Hide;
  Shape_Market_To := TKMShape.Create(Panel_HouseMarket, 0, 0, 26, 30, $FF0000B0);
  Shape_Market_To.LineWidth := 2;
  Shape_Market_To.Hitable := False;
  Shape_Market_To.Hide;

  LineH := 12+((STORE_RES_COUNT-1) div 6 + 1)*34;
  Label_Market_In  := TKMLabel.Create(Panel_HouseMarket, 8,LineH,85,30,'',fnt_Grey,taLeft);
  Label_Market_Out := TKMLabel.Create(Panel_HouseMarket,184,LineH,85,30,'',fnt_Grey,taRight);

  inc(LineH, 20);
  Button_Market_In  := TKMButtonFlat.Create(Panel_HouseMarket,  8, LineH, 36, 40, 0);
  Button_Market_In.HideHighlight := True;
  Button_Market_In.Disable;
  Button_Market_In.Hint := fTextLibrary[TX_HOUSES_MARKET_SELECT_LEFT];
  Button_Market_Out := TKMButtonFlat.Create(Panel_HouseMarket, 148, LineH, 36, 40, 0);
  Button_Market_Out.Disable;
  Button_Market_Out.Hint := fTextLibrary[TX_HOUSES_MARKET_SELECT_RIGHT];

  with TKMShape.Create(Panel_HouseMarket,  8, LineH, 36, 40, $FF00B000) do
  begin
    LineWidth := 2;
    Hitable := False;
  end;
  with TKMShape.Create(Panel_HouseMarket, 148, LineH, 36, 40, $FF0000B0) do
  begin
    LineWidth := 2;
    Hitable := False;
  end;

  inc(LineH, 10);

  Label_Market_FromAmount := TKMLabel.Create(Panel_HouseMarket,60,LineH,31,30,'',fnt_Grey,taCenter);
  Button_Market_Remove := TKMButton.Create(Panel_HouseMarket,76,LineH,20,20,'-',fnt_Metal,bsGame);
  Button_Market_Remove.OnClickEither := House_MarketOrderClick;
  Button_Market_Remove.Hint := fTextLibrary[TX_HOUSES_MARKET_HINT_REM];
  Button_Market_Add := TKMButton.Create(Panel_HouseMarket,97,LineH,20,20,'+',fnt_Metal,bsGame);
  Button_Market_Add.Hint := fTextLibrary[TX_HOUSES_MARKET_HINT_ADD];
  Button_Market_Add.OnClickEither := House_MarketOrderClick;
  Label_Market_ToAmount := TKMLabel.Create(Panel_HouseMarket, 132,LineH,31,30,'',fnt_Grey,taCenter);
end;


{Store page}
procedure TKMGamePlayInterface.Create_Store_Page;
var i:integer;
begin
  Panel_HouseStore:=TKMPanel.Create(Panel_House,0,76,200,400);
    for i:=1 to STORE_RES_COUNT do
    begin
      Button_Store[i] := TKMButtonFlat.Create(Panel_HouseStore, 8+((i-1)mod 5)*36,19+((i-1)div 5)*42,32,36,0);
      Button_Store[i].TexID := fResource.Resources[StoreResType[i]].GUIIcon;
      Button_Store[i].Tag := i;
      Button_Store[i].Hint := fResource.Resources[StoreResType[i]].Name;
      Button_Store[i].OnClick := House_StoreAcceptFlag;

      Image_Store_Accept[i] := TKMImage.Create(Panel_HouseStore, 8+((i-1)mod 5)*36+20,18+((i-1)div 5)*42+1,12,12,49);
      Image_Store_Accept[i].Tag := i;
      Image_Store_Accept[i].Hint := fResource.Resources[StoreResType[i]].Name;
      Image_Store_Accept[i].OnClick := House_StoreAcceptFlag;
    end;
end;


{School page}
procedure TKMGamePlayInterface.Create_School_Page;
var i:integer;
begin
  Panel_House_School:=TKMPanel.Create(Panel_House,0,76,200,400);
    Label_School_Res:=TKMLabel.Create(Panel_House_School,100,2,184,30,fTextLibrary[TX_HOUSE_NEEDS],fnt_Grey,taCenter);
    ResRow_School_Resource := TKMResourceRow.Create(Panel_House_School,  8,22,180,20);
    ResRow_School_Resource.RX := rxGui;
    ResRow_School_Resource.TexID := fResource.Resources[rt_Gold].GUIIcon;
    ResRow_School_Resource.Caption := fResource.Resources[rt_Gold].Name;
    ResRow_School_Resource.Hint := fResource.Resources[rt_Gold].Name;
    Button_School_UnitWIPBar :=TKMPercentBar.Create(Panel_House_School,42,54,138,20);
    Button_School_UnitWIP := TKMButton.Create(Panel_House_School,  8,48,32,32,0);
    Button_School_UnitWIP.Hint := fTextLibrary[TX_HOUSE_SCHOOL_WIP_HINT];
    Button_School_UnitWIP.Tag := 1;
    Button_School_UnitWIP.OnClick := House_SchoolUnitRemove;
    for i:=1 to 5 do begin
      Button_School_UnitPlan[i] := TKMButtonFlat.Create(Panel_House_School, 8+(i-1)*36,80,32,32,0);
      Button_School_UnitPlan[i].Tag := i+1;
      Button_School_UnitPlan[i].OnClick := House_SchoolUnitRemove;
    end;
    Label_School_Unit:=TKMLabel.Create(Panel_House_School,100,116,184,30,'',fnt_Outline,taCenter);
    Image_School_Left :=TKMImage.Create(Panel_House_School,  8,136,54,80,521);
    Image_School_Left.Disable;
    Image_School_Train:=TKMImage.Create(Panel_House_School, 70,136,54,80,522);
    Image_School_Right:=TKMImage.Create(Panel_House_School,132,136,54,80,523);
    Image_School_Right.Disable;
    Button_School_Left :=TKMButton.Create(Panel_House_School,  8,226,54,40,35);
    Button_School_Train:=TKMButton.Create(Panel_House_School, 70,226,54,40,42);
    Button_School_Right:=TKMButton.Create(Panel_House_School,132,226,54,40,36);
    Button_School_Left.OnClickEither:=House_SchoolUnitChange;
    Button_School_Train.OnClickEither:=House_SchoolUnitChange;
    Button_School_Right.OnClickEither:=House_SchoolUnitChange;
    Button_School_Left.Hint :=fTextLibrary[TX_HOUSE_SCHOOL_PREV_HINT];
    Button_School_Train.Hint:=fTextLibrary[TX_HOUSE_SCHOOL_TRAIN_HINT];
    Button_School_Right.Hint:=fTextLibrary[TX_HOUSE_SCHOOL_NEXT_HINT];
end;


{Barracks page}
procedure TKMGamePlayInterface.Create_Barracks_Page;
var i:integer;
begin
  Panel_HouseBarracks:=TKMPanel.Create(Panel_House,0,76,200,400);
    for i:=1 to BARRACKS_RES_COUNT do
    begin
      Button_Barracks[i] := TKMButtonFlat.Create(Panel_HouseBarracks, 8+((i-1)mod 6)*31,8+((i-1)div 6)*42,28,38,0);
      Button_Barracks[i].TexOffsetX := 1;
      Button_Barracks[i].TexOffsetY := 1;
      Button_Barracks[i].CapOffsetY := 2;
      Button_Barracks[i].HideHighlight := True;
      Button_Barracks[i].TexID := fResource.Resources[BarracksResType[i]].GUIIcon;
      Button_Barracks[i].Hint := fResource.Resources[BarracksResType[i]].Name;
    end;

    Button_BarracksRecruit := TKMButtonFlat.Create(Panel_HouseBarracks, 8+((BARRACKS_RES_COUNT)mod 6)*31,8+((BARRACKS_RES_COUNT)div 6)*42,28,38,0);
    Button_BarracksRecruit.TexOffsetX := 1;
    Button_BarracksRecruit.TexOffsetY := 1;
    Button_BarracksRecruit.CapOffsetY := 2;
    Button_BarracksRecruit.HideHighlight := True;
    Button_BarracksRecruit.TexID := fResource.UnitDat[ut_Recruit].GUIIcon;
    Button_BarracksRecruit.Hint := fResource.UnitDat[ut_Recruit].UnitName;

    Label_Barracks_Unit:=TKMLabel.Create(Panel_HouseBarracks,100,96,184,30,'',fnt_Outline,taCenter);

    Image_Barracks_Left :=TKMImage.Create(Panel_HouseBarracks,  8,116,54,80,535);
    Image_Barracks_Left.Disable;
    Image_Barracks_Train:=TKMImage.Create(Panel_HouseBarracks, 70,116,54,80,536);
    Image_Barracks_Right:=TKMImage.Create(Panel_HouseBarracks,132,116,54,80,537);
    Image_Barracks_Right.Disable;

    Button_Barracks_Left :=TKMButton.Create(Panel_HouseBarracks,  8,226,54,40,35);
    Button_Barracks_Train:=TKMButton.Create(Panel_HouseBarracks, 70,226,54,40,42);
    Button_Barracks_Right:=TKMButton.Create(Panel_HouseBarracks,132,226,54,40,36);
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
  Panel_HouseWoodcutter:=TKMPanel.Create(Panel_House,0,76,200,400);
    Radio_Woodcutter := TKMRadioGroup.Create(Panel_HouseWoodcutter,46,64,160,32,fnt_Grey);
    Radio_Woodcutter.ItemIndex := 0;
    Radio_Woodcutter.Items.Add(fTextLibrary[TX_HOUSES_WOODCUTTER_PLANT_CHOP]);
    Radio_Woodcutter.Items.Add(fTextLibrary[TX_HOUSES_WOODCUTTER_CHOP_ONLY]);
    Radio_Woodcutter.OnChange := House_WoodcutterChange;

    Button_Woodcutter := TKMButtonFlat.Create(Panel_HouseWoodcutter,8,64,32,32,23,rxGame);
    Button_Woodcutter.OnClick := House_WoodcutterChange; //Clicking the button cycles it
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
var i:integer;
begin
  if Sender=nil then begin GameCursor.Mode:=cm_None; exit; end;

  //Release all buttons (houses and fields)
  for i:=1 to Panel_Build.ChildCount do
    if Panel_Build.Childs[i] is TKMButtonFlat then
      TKMButtonFlat(Panel_Build.Childs[i]).Down := false;

  //Press the button
  TKMButtonFlat(Sender).Down := true;

  //Reset cursor and see if it needs to be changed
  GameCursor.Mode := cm_None;
  GameCursor.Tag1 := 0;
  GameCursor.Tag2 := 0;
  Label_BuildCost_Wood.Caption  := '-';
  Label_BuildCost_Stone.Caption := '-';
  Label_Build.Caption := '';

  if Button_BuildCancel.Down then begin
    GameCursor.Mode:=cm_Erase;
    Image_Build_Selected.TexID := 340;
    Label_Build.Caption := fTextLibrary[TX_BUILD_DEMOLISH];
  end;
  if Button_BuildRoad.Down then begin
    GameCursor.Mode:=cm_Road;
    Image_Build_Selected.TexID := 335;
    Label_BuildCost_Stone.Caption:='1';
    Label_Build.Caption := fTextLibrary[TX_BUILD_ROAD];
  end;
  if Button_BuildField.Down then begin
    GameCursor.Mode:=cm_Field;
    Image_Build_Selected.TexID := 337;
    Label_Build.Caption := fTextLibrary[TX_BUILD_FIELD];
  end;
  if Button_BuildWine.Down then begin
    GameCursor.Mode:=cm_Wine;
    Image_Build_Selected.TexID := 336;
    Label_BuildCost_Wood.Caption:='1';
    Label_Build.Caption := fTextLibrary[TX_BUILD_WINE];
  end;

  for i:=1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[i] <> ht_None then
  if Button_Build[i].Down then begin
     GameCursor.Mode:=cm_Houses;
     GameCursor.Tag1:=byte(GUIHouseOrder[i]);
     Image_Build_Selected.TexID := fResource.HouseDat[GUIHouseOrder[i]].GUIIcon;
     Label_BuildCost_Wood.Caption:=inttostr(fResource.HouseDat[GUIHouseOrder[i]].WoodCost);
     Label_BuildCost_Stone.Caption:=inttostr(fResource.HouseDat[GUIHouseOrder[i]].StoneCost);
     Label_Build.Caption := fResource.HouseDat[GUIHouseOrder[i]].HouseName;
  end;
end;


procedure TKMGamePlayInterface.ShowHouseInfo(Sender:TKMHouse; aAskDemolish:boolean=false);
const LineAdv = 25; //Each new Line is placed ## pixels after previous
var i,RowRes,Base,Line:integer; Res:TResourceType;
begin
  fShownUnit  := nil;
  fShownHouse := Sender;
  fAskDemolish := aAskDemolish;

  if not Assigned(Sender) then begin //=nil produces wrong result when there's no object at all
    SwitchPage(nil);
    exit;
  end;

  {Common data}
  Label_House.Caption       := fResource.HouseDat[Sender.HouseType].HouseName;
  Image_House_Logo.TexID    := fResource.HouseDat[Sender.HouseType].GUIIcon;
  Image_House_Worker.TexID  := fResource.UnitDat[fResource.HouseDat[Sender.HouseType].OwnerType].GUIIcon;
  Image_House_Worker.Hint   := fResource.UnitDat[fResource.HouseDat[Sender.HouseType].OwnerType].UnitName;
  HealthBar_House.Caption   := inttostr(round(Sender.GetHealth))+'/'+inttostr(fResource.HouseDat[Sender.HouseType].MaxHealth);
  HealthBar_House.Position  := round( Sender.GetHealth / fResource.HouseDat[Sender.HouseType].MaxHealth * 100 );

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
  Label_House_Demolish.Hide;
  Button_House_DemolishYes.Hide;
  Button_House_DemolishNo.Hide;
  SwitchPage(Panel_House);

  case Sender.HouseType of
    ht_Marketplace:
        begin
          House_MarketFill;
          SwitchPage(Panel_HouseMarket);
        end;

    ht_Store:
        begin
          House_StoreFill;
          SwitchPage(Panel_HouseStore);
        end;

    ht_School:
        begin
          ResRow_School_Resource.ResourceCount:=Sender.CheckResIn(rt_Gold) - byte(TKMHouseSchool(Sender).HideOneGold);
          House_SchoolUnitChange(nil, mbLeft);
          SwitchPage(Panel_House_School);
        end;

    ht_Barracks:
        begin
          Image_House_Worker.Enable; //In the barrack the recruit icon is always enabled
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
          ResRow_Common_Resource[1].Caption := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[1]].Name;
          ResRow_Common_Resource[1].Hint := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[1]].Name;
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
              ResRow_Common_Resource[RowRes].Caption := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResInput[i]].Name;
              ResRow_Common_Resource[RowRes].Hint := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResInput[i]].Name;
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
              ResRow_Common_Resource[RowRes].Caption := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[i]].Name;
              ResRow_Common_Resource[RowRes].Hint := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[i]].Name;
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
              ResRow_Order[i].Caption := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[i]].Name;
              ResRow_Order[i].Hint := fResource.Resources[fResource.HouseDat[Sender.HouseType].ResOutput[i]].Name;
              ResRow_Order[i].ResourceCount := Sender.CheckResOut(fResource.HouseDat[Sender.HouseType].ResOutput[i]);
              ResRow_Order[i].OrderCount := Sender.CheckResOrder(i);
              ResRow_Order[i].Show;
              ResRow_Order[i].Top := Base+Line*LineAdv;
              inc(Line);
            end;
            Label_Common_Costs.Show;
            Label_Common_Costs.Top:=Base+Line*LineAdv+6;
            inc(Line);
            for i:=1 to 4 do //Costs
            begin
              Res := fResource.HouseDat[Sender.HouseType].ResOutput[i];
              if fResource.Resources[Res].IsValid then
              begin
                ResRow_Costs[i].Caption := fResource.Resources[Res].Name;
                ResRow_Costs[i].RX := rxGui;
                //Hide the icons when they are not used
                if WarfareCosts[Res, 1] = rt_None then ResRow_Costs[i].TexID1 := 0
                else ResRow_Costs[i].TexID1 := fResource.Resources[WarfareCosts[Res, 1]].GUIIcon;
                if WarfareCosts[Res, 2] = rt_None then ResRow_Costs[i].TexID2 := 0
                else ResRow_Costs[i].TexID2 := fResource.Resources[WarfareCosts[Res, 2]].GUIIcon;

                ResRow_Costs[i].Show;
                ResRow_Costs[i].Top := Base + Line * LineAdv;
                inc(Line);
              end;
            end;
          end;
          SwitchPage(Panel_House_Common);
        end;
  end;
end;


procedure TKMGamePlayInterface.ShowUnitInfo(Sender:TKMUnit; aAskDismiss:boolean=false);
var Commander: TKMUnitWarrior;
begin
  fShownUnit  := Sender;
  fShownHouse := nil;
  fAskDismiss  := aAskDismiss;

  if (fShownUnit=nil) or (not fShownUnit.Visible) or (fShownUnit.IsDeadOrDying) then begin
    SwitchPage(nil);
    exit;
  end;

  SwitchPage(Panel_Unit);
  Label_UnitName.Caption:= fResource.UnitDat[Sender.UnitType].UnitName;
  Image_UnitPic.TexID:=fResource.UnitDat[Sender.UnitType].GUIScroll;
  ConditionBar_Unit.Position:=EnsureRange(round(Sender.Condition / UNIT_MAX_CONDITION * 100),-10,110);
  Label_UnitTask.Caption:='Task: '+Sender.GetUnitTaskText;

  if Sender is TKMUnitWarrior then
  begin
    Label_UnitDescription.Hide;
    if fAskDismiss then
    begin
      Panel_Army.Hide;
      Panel_Army_JoinGroups.Hide;
      Panel_Unit_Dismiss.Show;
      Button_Unit_Dismiss.Hide;
      exit;
    end;
    Panel_Unit_Dismiss.Hide;
    Button_Unit_Dismiss.Show;
    Commander := TKMUnitWarrior(Sender).GetCommander;
    if not Commander.ArmyCanTakeOrders then
      Army_HideJoinMenu(nil); //Cannot be joining while in combat/charging
    if fJoiningGroups then
    begin
      Panel_Army_JoinGroups.Show;
      Panel_Army.Hide;
    end
    else
    begin
      Panel_Army.Show;
      ImageStack_Army.SetCount(Commander.GetMemberCount + 1, Commander.UnitsPerRow, Commander.UnitsPerRow div 2 + 1); //Count+commander, Columns
      Panel_Army_JoinGroups.Hide;
      Army_ActivateControls(Commander.ArmyCanTakeOrders);
    end;
    Button_Army_Storm.Enabled := (UnitGroups[Sender.UnitType] = gt_Melee) and Commander.ArmyCanTakeOrders; //Only melee groups may charge
    Button_Army_Split.Enabled := (Commander.GetMemberCount > 0) and Commander.ArmyCanTakeOrders;
  end
  else
  begin //Citizen specific
    Panel_Army.Hide;
    Panel_Army_JoinGroups.Hide;
    if fAskDismiss then
    begin
      Panel_Unit_Dismiss.Show;
      Button_Unit_Dismiss.Hide;
      Label_UnitDescription.Hide;
      exit;
    end;
    Panel_Unit_Dismiss.Hide;
    Button_Unit_Dismiss.Show;
    Label_UnitDescription.Caption := fResource.UnitDat[Sender.UnitType].Description;
    Label_UnitDescription.Show;
  end;
end;


procedure TKMGamePlayInterface.House_Demolish(Sender:TObject);
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouse) then exit;

  if Sender=Button_House_DemolishYes then begin
    fGame.GameInputProcess.CmdBuild(gic_BuildRemoveHouse, TKMHouse(fPlayers.Selected).GetPosition);
    ShowHouseInfo(nil, false); //Simpliest way to reset page and ShownHouse
    SwitchPage(Button_Main[1]); //Return to build menu after demolishing
  end else begin
    fAskDemolish:=false;
    SwitchPage(Button_Main[1]); //Cancel and return to build menu
  end;
end;


procedure TKMGamePlayInterface.House_RepairToggle(Sender:TObject);
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouse) then exit;
  fGame.GameInputProcess.CmdHouse(gic_HouseRepairToggle, TKMHouse(fPlayers.Selected));
  case TKMHouse(fPlayers.Selected).BuildingRepair of
    true:   Button_House_Repair.TexID := 39;
    false:  Button_House_Repair.TexID := 40;
  end;
end;


procedure TKMGamePlayInterface.House_WareDeliveryToggle(Sender:TObject);
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouse) then exit;

  fGame.GameInputProcess.CmdHouse(gic_HouseDeliveryToggle, TKMHouse(fPlayers.Selected));
  case TKMHouse(fPlayers.Selected).WareDelivery of
    true:   Button_House_Goods.TexID := 37;
    false:  Button_House_Goods.TexID := 38;
  end;
end;


procedure TKMGamePlayInterface.House_OrderClick(Sender:TObject; AButton:TMouseButton);
var i:integer; Amt:byte;
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouse) then exit;

  Amt := 0;
  if AButton = mbLeft then Amt := 1;
  if AButton = mbRight then Amt := 10;

  for i:=1 to 4 do begin
    if Sender = ResRow_Order[i].OrderRem then
      fGame.GameInputProcess.CmdHouse(gic_HouseOrderProduct, TKMHouse(fPlayers.Selected), i, -Amt);
    if Sender = ResRow_Order[i].OrderAdd then
      fGame.GameInputProcess.CmdHouse(gic_HouseOrderProduct, TKMHouse(fPlayers.Selected), i, Amt);
  end;
end;


procedure TKMGamePlayInterface.House_WoodcutterChange(Sender:TObject);
var Woodcutters: TKMHouseWoodcutters; WoodcutterMode: TWoodcutterMode;
begin
  Woodcutters := TKMHouseWoodcutters(fPlayers.Selected);
  if Sender = Button_Woodcutter then
    Radio_Woodcutter.ItemIndex := (Radio_Woodcutter.ItemIndex+1) mod 2; //Cycle

  if (Sender = Button_Woodcutter) or (Sender = Radio_Woodcutter) then
  begin
    if Radio_Woodcutter.ItemIndex = 0 then
         WoodcutterMode := wcm_ChopAndPlant
    else WoodcutterMode := wcm_Chop;
    fGame.GameInputProcess.CmdHouse(gic_HouseWoodcutterMode, Woodcutters, WoodcutterMode);
  end;

  if Woodcutters.WoodcutterMode = wcm_ChopAndPlant then
  begin
    Button_Woodcutter.TexID := 310;
    Button_Woodcutter.RX := rxGui;
    Radio_Woodcutter.ItemIndex := 0;
  end;
  if Woodcutters.WoodcutterMode = wcm_Chop then
  begin
    Button_Woodcutter.TexID := 23;
    Button_Woodcutter.RX := rxGame;
    Radio_Woodcutter.ItemIndex := 1;
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
var i:byte; School:TKMHouseSchool;
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouseSchool) then exit;
  School:=TKMHouseSchool(fPlayers.Selected);

  if (AButton = mbRight) and (Sender=Button_School_Left) then LastSchoolUnit := 0;
  if (AButton = mbRight) and (Sender=Button_School_Right) then LastSchoolUnit := High(School_Order);

  if (Sender=Button_School_Left)and(LastSchoolUnit > 0) then dec(LastSchoolUnit);
  if (Sender=Button_School_Right)and(LastSchoolUnit < High(School_Order)) then inc(LastSchoolUnit);

  if Sender=Button_School_Train then //Add unit to training queue
    if AButton = mbLeft then
      fGame.GameInputProcess.CmdHouse(gic_HouseSchoolTrain, School, School_Order[LastSchoolUnit], 1)
    else if AButton = mbRight then
      fGame.GameInputProcess.CmdHouse(gic_HouseSchoolTrain, School, School_Order[LastSchoolUnit], 6);

  if School.UnitQueue[1]<>ut_None then
    Button_School_UnitWIP.TexID := fResource.UnitDat[School.UnitQueue[1]].GUIIcon
  else
    Button_School_UnitWIP.TexID := 41; //Question mark

  Button_School_UnitWIPBar.Position:=School.GetTrainingProgress;

  for i:=1 to 5 do
    if School.UnitQueue[i+1]<>ut_None then
    begin
      Button_School_UnitPlan[i].TexID := fResource.UnitDat[School.UnitQueue[i+1]].GUIIcon;
      Button_School_UnitPlan[i].Hint := fResource.UnitDat[School.UnitQueue[i+1]].UnitName;
    end
    else
    begin
      Button_School_UnitPlan[i].TexID:=0;
      Button_School_UnitPlan[i].Hint:='';
    end;

  Button_School_Train.Enabled := School.UnitQueue[length(School.UnitQueue)]=ut_None;
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
procedure TKMGamePlayInterface.House_SchoolUnitRemove(Sender:TObject);
begin
  if not (TKMControl(Sender).Tag in [1..6]) then exit;
  fGame.GameInputProcess.CmdHouse(gic_HouseRemoveTrain, TKMHouseSchool(fPlayers.Selected), TKMControl(Sender).Tag);
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
  TrackBar_Settings_Brightness.Position   := fGame.GlobalSettings.Brightness;
  CheckBox_Settings_Autosave.Checked      := fGame.GlobalSettings.Autosave;
  TrackBar_Settings_ScrollSpeed.Position  := fGame.GlobalSettings.ScrollSpeed;
  TrackBar_Settings_SFX.Position          := Round(fGame.GlobalSettings.SoundFXVolume * TrackBar_Settings_SFX.MaxValue);
  TrackBar_Settings_Music.Position        := Round(fGame.GlobalSettings.MusicVolume * TrackBar_Settings_Music.MaxValue);
  CheckBox_Settings_MusicOff.Checked      := fGame.GlobalSettings.MusicOff;
  CheckBox_Settings_ShuffleOn.Checked     := fGame.GlobalSettings.ShuffleOn;

  TrackBar_Settings_Music.Enabled     := not CheckBox_Settings_MusicOff.Checked;
  CheckBox_Settings_ShuffleOn.Enabled := not CheckBox_Settings_MusicOff.Checked;
end;


procedure TKMGamePlayInterface.Menu_Settings_Change(Sender: TObject);
var
  MusicToggled, ShuffleToggled: Boolean;
begin
  //Change these options only if they changed state since last time
  MusicToggled   := (fGame.GlobalSettings.MusicOff <> CheckBox_Settings_MusicOff.Checked);
  ShuffleToggled := (fGame.GlobalSettings.ShuffleOn <> CheckBox_Settings_ShuffleOn.Checked);

  fGame.GlobalSettings.Brightness    := TrackBar_Settings_Brightness.Position;
  fGame.GlobalSettings.Autosave      := CheckBox_Settings_Autosave.Checked;
  fGame.GlobalSettings.ScrollSpeed   := TrackBar_Settings_ScrollSpeed.Position;
  fGame.GlobalSettings.SoundFXVolume := TrackBar_Settings_SFX.Position / TrackBar_Settings_SFX.MaxValue;
  fGame.GlobalSettings.MusicVolume   := TrackBar_Settings_Music.Position / TrackBar_Settings_Music.MaxValue;
  fGame.GlobalSettings.MusicOff      := CheckBox_Settings_MusicOff.Checked;
  fGame.GlobalSettings.ShuffleOn     := CheckBox_Settings_ShuffleOn.Checked;

  fSoundLib.UpdateSoundVolume(fGame.GlobalSettings.SoundFXVolume);
  fGame.MusicLib.UpdateMusicVolume(fGame.GlobalSettings.MusicVolume);
  if MusicToggled then
  begin
    fGame.MusicLib.ToggleMusic(not fGame.GlobalSettings.MusicOff);
    if not fGame.GlobalSettings.MusicOff then
      ShuffleToggled := True; //Re-shuffle songs if music has been enabled
  end;
  if ShuffleToggled then
    fGame.MusicLib.ToggleShuffle(fGame.GlobalSettings.ShuffleOn);

  TrackBar_Settings_Music.Enabled := not CheckBox_Settings_MusicOff.Checked;
  CheckBox_Settings_ShuffleOn.Enabled := not CheckBox_Settings_MusicOff.Checked;
end;


{Quit the mission and return to main menu}
procedure TKMGamePlayInterface.Menu_QuitMission(Sender:TObject);
begin
  //Show outcome depending on actual situation. By default PlayOnState is gr_Cancel, if playing on after victory/defeat it changes
  fGame.Stop(fGame.PlayOnState);
end;


procedure TKMGamePlayInterface.Menu_NextTrack(Sender:TObject); begin fGame.MusicLib.PlayNextTrack; end;
procedure TKMGamePlayInterface.Menu_PreviousTrack(Sender:TObject); begin fGame.MusicLib.PlayPreviousTrack; end;


procedure TKMGamePlayInterface.Army_Issue_Order(Sender:TObject);
var Commander: TKMUnitWarrior;
begin
  if fPlayers.Selected = nil then exit;

  if Sender = Button_Unit_Dismiss then
  begin
    ShowUnitInfo(TKMUnit(fPlayers.Selected), true);
  end;

  if not (fPlayers.Selected is TKMUnitWarrior) then exit;

  Commander := TKMUnitWarrior(fPlayers.Selected).GetCommander;

  //if Sender = Button_Army_GoTo    then ; //This command makes no sense unless player has no right-mouse-button
  if Sender = Button_Army_Stop    then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyHalt, Commander, tdNone, 0);
    fSoundLib.PlayWarrior(Commander.UnitType, sp_Halt);
  end;
  //if Sender = Button_Army_Attack  then ; //This command makes no sense unless player has no right-mouse-button
  if Sender = Button_Army_RotCW   then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyHalt, Commander, tdCW, 0);
    fSoundLib.PlayWarrior(Commander.UnitType, sp_RotRight);
  end;
  if Sender = Button_Army_Storm   then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyStorm, Commander);
    fSoundLib.PlayWarrior(Commander.UnitType, sp_StormAttack);
  end;
  if Sender = Button_Army_RotCCW  then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyHalt, Commander, tdCCW, 0);
    fSoundLib.PlayWarrior(Commander.UnitType, sp_RotLeft);
  end;
  if Sender = Button_Army_ForDown then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyHalt, Commander, tdNone, 1);
    fSoundLib.PlayWarrior(Commander.UnitType, sp_Formation);
  end;
  if Sender = Button_Army_ForUp   then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyHalt, Commander, tdNone, -1);
    fSoundLib.PlayWarrior(Commander.UnitType, sp_Formation);
  end;
  if Sender = Button_Army_Split   then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmySplit, Commander);
    fSoundLib.PlayWarrior(Commander.UnitType, sp_Split);
  end;
  if Sender = Button_Army_Join    then
  begin
    Panel_Army.Hide;
    Panel_Army_JoinGroups.Show;
    fJoiningGroups := true;
  end;
  if Sender = Button_Army_Feed    then
  begin
    fGame.GameInputProcess.CmdArmy(gic_ArmyFeed, Commander);
    fSoundLib.PlayWarrior(Commander.UnitType, sp_Eat);
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

procedure TKMGamePlayInterface.Army_HideJoinMenu(Sender:TObject);
begin
  fJoiningGroups := false;
  if fResource.Cursors.Cursor in [kmc_JoinYes, kmc_JoinNo] then //Do not override non-joining cursors
    fResource.Cursors.Cursor := kmc_Default; //In case this is run with keyboard shortcut, mouse move won't happen
  Panel_Army_JoinGroups.Hide;
  if fShownUnit <> nil then
    Panel_Army.Show;
end;


procedure TKMGamePlayInterface.Build_Fill(Sender:TObject);
var i:integer;
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


procedure TKMGamePlayInterface.Chat_Post(Sender:TObject; Key:word);
begin
  if (Key = VK_RETURN) and (Trim(Edit_ChatMsg.Text) <> '') and (fGame.Networking <> nil) then
  begin
    fGame.Networking.PostMessage(Edit_ChatMsg.Text, true, CheckBox_SendToAllies.Checked);
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
    fGame.RestartReplay; //reload it once again

  if (Sender = Button_ReplayPause) then begin
    fGame.SetGameState(gsPaused);
    SetButtons(false);
  end;

  if (Sender = Button_ReplayStep) then begin
    fGame.StepOneFrame;
    fGame.SetGameState(gsReplay);
    SetButtons(false);
  end;

  if (Sender = Button_ReplayResume) then begin
    fGame.SetGameState(gsReplay);
    SetButtons(true);
  end;

  if (Sender = Button_ReplayExit) then
    fGame.GameHold(true, gr_ReplayEnd);
end;


procedure TKMGamePlayInterface.MessageIssue(aKind: TKMMessageKind; aText: string; aLoc: TKMPoint);
begin
  fMessageList.AddEntry(aKind, aText, aLoc);
  Message_UpdateStack;
  fSoundLib.Play(sfx_MessageNotice, 4); //Play horn sound on new message if it is the right type
end;


procedure TKMGamePlayInterface.House_MarketFill;
var M: TKMHouseMarket; R:TResourceType; i,Tmp:integer;
begin
  if (fShownHouse = nil) or not (fShownHouse is TKMHouseMarket) then Exit;

  M := TKMHouseMarket(fShownHouse);

  for i:=0 to STORE_RES_COUNT-1 do
  begin
    R := TResourceType(Button_Market[i].Tag);
    if M.AllowedToTrade(R) then
    begin
      Button_Market[i].TexID := fResource.Resources[R].GUIIcon;
      Button_Market[i].Hint := fResource.Resources[R].Name;
      Tmp := M.GetResTotal(TResourceType(Button_Market[i].Tag));
      Button_Market[i].Caption := IfThen(Tmp=0, '-', IntToStr(Tmp));
    end
    else
    begin
      Button_Market[i].TexID := 41;
      Button_Market[i].Hint := fTextLibrary[TX_HOUSES_MARKET_HINT_BLOCKED];
      Button_Market[i].Caption := '-';
    end;
  end;

  Shape_Market_From.Visible := M.ResFrom <> rt_None;
  if M.ResFrom <> rt_None then
  begin
    Shape_Market_From.Left := 8 + ((Byte(M.ResFrom)-1) mod 6) * 30;
    Shape_Market_From.Top := 12 + ((Byte(M.ResFrom)-1) div 6) * 34;
    Label_Market_In.Caption := Format(fTextLibrary[TX_HOUSES_MARKET_FROM],[M.RatioFrom]) + ':';
    Button_Market_In.TexID := fResource.Resources[M.ResFrom].GUIIcon;
    Button_Market_In.Caption := IntToStr(M.GetResTotal(M.ResFrom));
  end else begin
    Label_Market_In.Caption := Format(fTextLibrary[TX_HOUSES_MARKET_FROM],[0]) + ':';
    Button_Market_In.TexID := fResource.Resources[rt_None].GUIIcon;
    Button_Market_In.Caption := '-';
  end;

  Shape_Market_To.Visible := M.ResTo <> rt_None;
  if M.ResTo <> rt_None then
  begin
    Shape_Market_To.Left := 8 + ((Byte(M.ResTo)-1) mod 6) * 30;
    Shape_Market_To.Top := 12 + ((Byte(M.ResTo)-1) div 6) * 34;
    Label_Market_Out.Caption := Format(fTextLibrary[TX_HOUSES_MARKET_TO],[M.RatioTo]) + ':';
    Button_Market_Out.Caption := IntToStr(M.GetResTotal(M.ResTo));
    Button_Market_Out.TexID := fResource.Resources[M.ResTo].GUIIcon;
  end else begin
    Label_Market_Out.Caption := Format(fTextLibrary[TX_HOUSES_MARKET_TO],[0]) + ':';
    Button_Market_Out.TexID := fResource.Resources[rt_None].GUIIcon;
    Button_Market_Out.Caption := '-';
  end;

  Button_Market_Remove.Enabled := (M.ResFrom <> rt_None) and (M.ResTo <> rt_None);
  Button_Market_Add.Enabled := Button_Market_Remove.Enabled;
  Label_Market_FromAmount.Caption := IntToStr(M.RatioFrom * M.CheckResOrder(1));
  Label_Market_ToAmount.Caption := IntToStr(M.RatioTo * M.CheckResOrder(1));
end;


procedure TKMGamePlayInterface.House_MarketOrderClick(Sender:TObject; AButton:TMouseButton);
var Amt:byte; M: TKMHouseMarket;
begin
  if fPlayers.Selected = nil then exit;
  if not (fPlayers.Selected is TKMHouseMarket) then exit;

  M := TKMHouseMarket(fPlayers.Selected);

  Amt := 0;
  if AButton = mbLeft then Amt := 1;
  if AButton = mbRight then Amt := 10;

  if Sender = Button_Market_Remove then
    fGame.GameInputProcess.CmdHouse(gic_HouseOrderProduct, M, 1, -Amt);
  if Sender = Button_Market_Add then
    fGame.GameInputProcess.CmdHouse(gic_HouseOrderProduct, M, 1, Amt);
end;


procedure TKMGamePlayInterface.House_MarketSelect(Sender: TObject; AButton:TMouseButton);
var M: TKMHouseMarket;
begin
  if (fPlayers.Selected = nil) then Exit;
  if not (fPlayers.Selected is TKMHouseMarket) then Exit;

  M := TKMHouseMarket(fPlayers.Selected);

  //todo: We need to tell player that he must cancel previous order first instead of silently refusing

  if aButton = mbLeft then
    fGame.GameInputProcess.CmdHouse(gic_HouseMarketFrom, M, TResourceType(TKMButtonFlat(Sender).Tag));
  if aButton = mbRight then
    fGame.GameInputProcess.CmdHouse(gic_HouseMarketTo, M, TResourceType(TKMButtonFlat(Sender).Tag));

  House_MarketFill; //Update costs and order count
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
  if fGame.GlobalSettings.MusicOff then
    Label_Menu_Track.Caption := '-'
  else
    Label_Menu_Track.Caption := fGame.MusicLib.GetTrackTitle;

  Label_Menu_Track.Enabled      := not fGame.GlobalSettings.MusicOff;
  Button_Menu_TrackUp.Enabled   := not fGame.GlobalSettings.MusicOff;
  Button_Menu_TrackDown.Enabled := not fGame.GlobalSettings.MusicOff;
end;


procedure TKMGamePlayInterface.Stats_Fill(Sender:TObject);
var i,Tmp,Tmp2:integer;
begin
  for i:=low(StatHouse) to high(StatHouse) do
  begin
    Tmp := MyPlayer.Stats.GetHouseQty(StatHouse[i]);
    Tmp2 := MyPlayer.Stats.GetHouseWip(StatHouse[i]);
    Stat_HouseQty[i].Caption := IfThen(Tmp =0, '-', inttostr(Tmp));
    Stat_HouseWip[i].Caption := IfThen(Tmp2=0, '', '+'+inttostr(Tmp2));
    if MyPlayer.Stats.GetCanBuild(StatHouse[i]) or (Tmp>0) then
    begin
      Stat_HousePic[i].TexID := fResource.HouseDat[StatHouse[i]].GUIIcon;
      Stat_HousePic[i].Hint := fResource.HouseDat[StatHouse[i]].HouseName;
      Stat_HouseQty[i].Hint := fResource.HouseDat[StatHouse[i]].HouseName;
      Stat_HouseWip[i].Hint := fResource.HouseDat[StatHouse[i]].HouseName;
    end
    else
    begin
      Stat_HousePic[i].TexID := 41;
      Stat_HousePic[i].Hint := fTextLibrary[TX_HOUSE_NOT_AVAIABLE]; //Building not available
    end;
  end;
  for i:=low(StatUnit) to high(StatUnit) do
  begin
    Tmp := MyPlayer.Stats.GetUnitQty(StatUnit[i]);
    Stat_UnitQty[i].Caption := IfThen(Tmp = 0, '-', inttostr(Tmp));
    Stat_UnitPic[i].Hint := fResource.UnitDat[StatUnit[i]].UnitName;
  end;
end;


procedure TKMGamePlayInterface.Army_ActivateControls(aActive:boolean);
begin
  //Button_Army_GoTo.Enabled := aActive;
  Button_Army_Stop.Enabled := aActive;
  //Button_Army_Attack.Enabled := aActive;
  Button_Army_RotCW.Enabled := aActive;
  Button_Army_Storm.Enabled := aActive;
  Button_Army_RotCCW.Enabled := aActive;
  Button_Army_ForUp.Enabled := aActive;
  Button_Army_ForDown.Enabled := aActive;
  Button_Army_Split.Enabled := aActive;
  Button_Army_Join.Enabled := aActive;
  Button_Army_Feed.Enabled := aActive;
end;


procedure TKMGamePlayInterface.MenuIconsEnabled(NewValue:boolean);
begin
  Button_Main[1].Enabled := NewValue;
  Button_Main[2].Enabled := NewValue;
  Button_Main[3].Enabled := NewValue;
end;


procedure TKMGamePlayInterface.ShowClock(aSpeed: word);
begin
  Image_Clock.Visible := aSpeed <> 1;
  Label_Clock.Visible := aSpeed <> 1;
  Label_ClockSpeedup.Visible := aSpeed <> 1;
  Label_ClockSpeedup.Caption := 'x' + IntToStr(aSpeed);

  //With slow GPUs it will keep old values till next frame, that can take some seconds
  //Thats why we refresh Clock.Caption here
  if aSpeed <> 1 then
    Label_Clock.Caption := FormatDateTime('hh:nn:ss', fGame.GetMissionTime);
end;


procedure TKMGamePlayInterface.SetPause(aValue:boolean);
begin
  ReleaseDirectionSelector; //Don't restrict cursor movement to direction selection while paused
  fGame.Viewport.ReleaseScrollKeys;
  if aValue then fGame.SetGameState(gsPaused)
            else fGame.SetGameState(gsRunning);
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
      gr_Win:       fGame.Stop(gr_Win);
      gr_Defeat:    fGame.Stop(gr_Defeat);
      gr_ReplayEnd: fGame.Stop(gr_ReplayEnd);
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
      gr_Win:       fGame.Stop(gr_Win);
      gr_Defeat:    fGame.Stop(gr_Defeat);
      gr_ReplayEnd: fGame.Stop(gr_ReplayEnd);
    end
  //If they click continue no other action is necessary, the game is still running
end;


procedure TKMGamePlayInterface.ShowNetworkLag(DoShow:boolean; aPlayers:TStringList; IsHost:boolean);
var i:integer; S:String;
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
    Button_NetDropPlayers.Disable; //Must wait the minimum time before enabling it

    if not DoShow then
      fNetWaitDropPlayersDelayStarted := 0
    else
      if fNetWaitDropPlayersDelayStarted = 0 then
      begin
        Label_NetDropPlayersDelay.Caption := '';
        fNetWaitDropPlayersDelayStarted := GetTickCount; //Initialise it
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
      fGame.Stop(gr_Cancel);
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


procedure TKMGamePlayInterface.ClearShownUnit;
begin
  fShownUnit := nil;
  SwitchPage(nil);
end;


procedure TKMGamePlayInterface.ClearSelectedUnitOrHouse;
begin
  fShownUnit := nil;
  fShownHouse := nil;
  fPlayers.Selected := nil;
  SwitchPage(nil);
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


procedure TKMGamePlayInterface.SetChatText(const aString: string);
begin
  Edit_ChatMsg.Text := aString;
  if aString <> '' then Chat_Show(nil);
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


procedure TKMGamePlayInterface.AlliesOnPlayerSetup(Sender: TObject);
var i:integer;
begin
  for i:=0 to fGame.Networking.NetPlayers.Count - 1 do
  begin
    //Show players locale flag
    if fGame.Networking.NetPlayers[i+1].LangCode <> '' then
      Image_AlliesLang[i].TexID := fLocales.GetLocale(fGame.Networking.NetPlayers[i+1].LangCode).FlagSpriteID
    else
      Image_AlliesLang[i].TexID := 0;

    Label_AlliesPlayer[i].Caption := fGame.Networking.NetPlayers[i+1].Nikname;
    Label_AlliesPlayer[i].FontColor := fPlayers[fGame.Networking.NetPlayers[i+1].PlayerIndex.PlayerIndex].FlagColor;
    DropBox_AlliesTeam[i].ItemIndex := fGame.Networking.NetPlayers[i+1].Team;
    //Strikethrough for disconnected players
    Image_AlliesLang[i].Enabled := not fGame.Networking.NetPlayers[i+1].Dropped;
    Label_AlliesPlayer[i].Strikethrough := fGame.Networking.NetPlayers[i+1].Dropped;
    Label_AlliesTeam[i].Strikethrough := fGame.Networking.NetPlayers[i+1].Dropped;
    Label_AlliesPing[i].Strikethrough := fGame.Networking.NetPlayers[i+1].Dropped;
    if fGame.Networking.NetPlayers[i+1].Team = 0 then
      Label_AlliesTeam[i].Caption := fTextLibrary[TX_LOBBY_NONE]
    else
      Label_AlliesTeam[i].Caption := Format(fTextLibrary[TX_LOBBY_TEAM_X],[fGame.Networking.NetPlayers[i+1].Team]);
    DropBox_AlliesTeam[i].Enabled := (i+1 = fGame.Networking.MyIndex); //Our index
    DropBox_AlliesTeam[i].Hide; //Use label for demos until we fix exploits
  end;

  for i:=fGame.Networking.NetPlayers.Count to MAX_PLAYERS-1 do
  begin
    Label_AlliesPlayer[i].Hide;
    DropBox_AlliesTeam[i].Hide;
    Label_AlliesTeam[i].Hide;
  end;
end;


procedure TKMGamePlayInterface.AlliesOnPingInfo(Sender: TObject);
var i:integer;
begin
  for i:=0 to MAX_PLAYERS-1 do
    if (i < fGame.Networking.NetPlayers.Count) and (fGame.Networking.NetPlayers[i+1].IsHuman) then
    begin
      Label_AlliesPing[i].Caption := inttostr(fGame.Networking.NetPlayers[i+1].GetInstantPing);
      Label_AlliesPing[i].FontColor := GetPingColor(fGame.Networking.NetPlayers[i+1].GetInstantPing);
    end
    else
      Label_AlliesPing[i].Caption := '';
end;


procedure TKMGamePlayInterface.AlliesTeamChange(Sender: TObject);
var i:integer;
begin
  for i:=0 to MAX_PLAYERS-1 do
    if (Sender = DropBox_AlliesTeam[i]) and DropBox_AlliesTeam[i].Enabled then
      fGame.GameInputProcess.CmdGame(gic_GameTeamChange, i+1, DropBox_AlliesTeam[i].ItemIndex);
end;


procedure TKMGamePlayInterface.KeyDown(Key:Word; Shift: TShiftState);
begin
  if fGame.GameState in [gsRunning, gsReplay] then
  begin
    if (fGame.GameState = gsRunning) and fMyControls.KeyDown(Key, Shift) then
    begin
      fGame.Viewport.ReleaseScrollKeys; //Release the arrow keys when you open a window with an edit to stop them becoming stuck
      Exit;
    end;
    if Key = VK_LEFT  then fGame.Viewport.ScrollKeyLeft  := true;
    if Key = VK_RIGHT then fGame.Viewport.ScrollKeyRight := true;
    if Key = VK_UP    then fGame.Viewport.ScrollKeyUp    := true;
    if Key = VK_DOWN  then fGame.Viewport.ScrollKeyDown  := true;
  end;
end;


//Note: we deliberately don't pass any Keys to MyControls when game is not running
//thats why MyControls.KeyUp is only in gsRunning clause
//Ignore all keys if game is on 'Pause'
procedure TKMGamePlayInterface.KeyUp(Key:Word; Shift: TShiftState);
begin
  case fGame.GameState of
    gsPaused:   if (Key = ord('P')) and not fGame.MultiplayerMode then SetPause(false);
    gsOnHold:   ; //Ignore all keys if game is on victory 'Hold', only accept mouse clicks
    gsRunning:  begin //Game is running normally
                  if (Key=VK_ESCAPE) and (Image_ChatClose.Click or Image_AlliesClose.Click) then exit; //Escape from chat/allies page
                  if fMyControls.KeyUp(Key, Shift) then Exit;

                  //Scrolling
                  if Key = VK_LEFT  then fGame.Viewport.ScrollKeyLeft  := False;
                  if Key = VK_RIGHT then fGame.Viewport.ScrollKeyRight := False;
                  if Key = VK_UP    then fGame.Viewport.ScrollKeyUp    := False;
                  if Key = VK_DOWN  then fGame.Viewport.ScrollKeyDown  := False;

                  if Key = VK_BACK then  fGame.Viewport.ResetZoom;

                  //Game speed/pause: Not available in multiplayer mode yet
                  if not fGame.MultiplayerMode then
                  begin
                    if (Key = VK_F5) then fGame.SetGameSpeed(1);
                    if (Key = VK_F6) then fGame.SetGameSpeed(fGame.GlobalSettings.SpeedMedium);
                    if (Key = VK_F7) then fGame.SetGameSpeed(fGame.GlobalSettings.SpeedFast);
                    if (Key = VK_F8) then fGame.SetGameSpeed(fGame.GlobalSettings.SpeedVeryFast);
                    if (Key = ord('P')) then SetPause(true); //Display pause overlay
                  end;

                  //Menu shortcuts
                  if Key in [ord('1')..ord('4')] then Button_Main[Key-48].Click;
                  if Key=VK_ESCAPE then if Button_Army_Join_Cancel.Click then exit
                                        else if Button_MessageClose.Click then exit
                                        else if Image_ChatClose.Click then exit
                                        else if Image_AlliesClose.Click then exit
                                        else if Button_Main[5].Click then exit;
                  //Messages
                  if Key=VK_SPACE  then Button_MessageGoTo.Click; //In KaM spacebar centers you on the message
                  if Key=VK_DELETE then Button_MessageDelete.Click;
                  if (Key=VK_RETURN) and fGame.MultiplayerMode and not Panel_Chat.Visible then
                    Chat_Show(Self); //Enter is the shortcut to bring up chat in multiplayer

                  //Army shortcuts from KaM. (these are also in hints) Can be improved/changed later if we want to
                  if (Key = ord(fTextLibrary[TX_SHORTCUT_KEY_TROOP_HALT][1])) and (Panel_Army.Visible) then Button_Army_Stop.Click;
                  if (Key = ord(fTextLibrary[TX_SHORTCUT_KEY_TROOP_LINK][1])) and (Panel_Army.Visible) then Button_Army_Join.Click;
                  if (Key = ord(fTextLibrary[TX_SHORTCUT_KEY_TROOP_SPLIT][1])) and (Panel_Army.Visible) then Button_Army_Split.Click;

                  {Temporary cheat codes}
                  if DEBUG_CHEATS and (MULTIPLAYER_CHEATS or not fGame.MultiplayerMode) then
                  begin
                    if Key=ord('5') then MessageIssue(mkText, '123', KMPoint(0,0));
                    if Key=ord('6') then MessageIssue(mkHouse,'123', KMPointRound(fGame.Viewport.Position));
                    if Key=ord('7') then MessageIssue(mkUnit, '123', KMPoint(0,0));
                    if Key=ord('8') then MessageIssue(mkQuill,'123', KMPoint(0,0));

                    if Key=ord('W') then fGame.GameInputProcess.CmdTemp(gic_TempRevealMap);
                    if Key=ord('V') then begin fGame.GameHold(true, gr_Win); exit; end; //Instant victory
                    if Key=ord('D') then begin fGame.GameHold(true, gr_Defeat); exit; end; //Instant defeat
                    if Key=ord('Q') then fGame.GameInputProcess.CmdTemp(gic_TempAddScout, GameCursor.Cell); //Usefull when mouse has no middle-button
                  end;

                end;
    gsReplay:   begin
                  //Scrolling
                  if Key = VK_LEFT  then fGame.Viewport.ScrollKeyLeft  := false;
                  if Key = VK_RIGHT then fGame.Viewport.ScrollKeyRight := false;
                  if Key = VK_UP    then fGame.Viewport.ScrollKeyUp    := false;
                  if Key = VK_DOWN  then fGame.Viewport.ScrollKeyDown  := false;

                  if Key = VK_BACK then fGame.Viewport.ResetZoom;

                  if Key = VK_F5 then fGame.SetGameSpeed(1);
                  if Key = VK_F6 then fGame.SetGameSpeed(fGame.GlobalSettings.SpeedMedium);
                  if Key = VK_F7 then fGame.SetGameSpeed(fGame.GlobalSettings.SpeedFast);
                  if Key = VK_F8 then fGame.SetGameSpeed(fGame.GlobalSettings.SpeedVeryFast);
                end;
   end;
end;


//1. Process Controls
//2. Show SelectingTroopDirection
procedure TKMGamePlayInterface.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var U:TKMUnit; H:TKMHouse; MyRect:TRect;
begin
  inherited;

  if (fGame.GameState <> gsRunning) or (fMyControls.CtrlOver <> nil) then exit;

  if SelectingTroopDirection then
  begin
    fMain.ApplyCursorRestriction; //Reset the cursor restrictions from selecting direction
    SelectingTroopDirection := false;
    DirectionCursorHide;
  end;

  //See if we can show DirectionSelector
  //Can walk to ally units place, can't walk to house place anyway, unless it's a markup and allied
  if (Button = mbRight) and (not fJoiningGroups) and(fShownUnit is TKMUnitWarrior)
    and(fShownUnit.GetOwner = MyPlayer.PlayerIndex) then
  begin
    U := fTerrain.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
    H := fPlayers.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
    if ((U = nil) or U.IsDeadOrDying or (fPlayers.CheckAlliance(MyPlayer.PlayerIndex, U.GetOwner) = at_Ally)) and
       ((H = nil) or (fPlayers.CheckAlliance(MyPlayer.PlayerIndex, H.GetOwner) = at_Ally)) and
      fShownUnit.CanWalkTo(GameCursor.Cell, 0) then
    begin
      SelectingTroopDirection := true; //MouseMove will take care of cursor changing
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
begin
  inherited;

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

  if fGame.GameState = gsReplay then
    fGame.UpdateGameCursor(X,Y,Shift); //To show coords in status bar

  if (fGame.GameState <> gsRunning) then exit;

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
      cm_Road:  if MyPlayer.CanAddFakeFieldPlan(P, ft_Road) and not KMSamePoint(LastDragPoint, P) then
                begin
                  fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Road);
                  LastDragPoint := GameCursor.Cell;
                end;
      cm_Field: if MyPlayer.CanAddFakeFieldPlan(P, ft_Corn) and not KMSamePoint(LastDragPoint, P) then
                begin
                  fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Corn);
                  LastDragPoint := GameCursor.Cell;
                end;
      cm_Wine:  if MyPlayer.CanAddFakeFieldPlan(P, ft_Wine) and not KMSamePoint(LastDragPoint, P) then
                begin
                  fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Wine);
                  LastDragPoint := GameCursor.Cell;
                end;
      cm_Erase: if not KMSamePoint(LastDragPoint, P) then
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

  if GameCursor.Mode<>cm_None then
  begin
    //Use the default cursor while placing roads, don't become stuck on c_Info or others
    if not fGame.Viewport.Scrolling then
      fResource.Cursors.Cursor := kmc_Default;
    Exit;
  end;

  if fJoiningGroups and (fShownUnit is TKMUnitWarrior) then
  begin
    U := fTerrain.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
    if (U <> nil)
    and (U.GetOwner = MyPlayer.PlayerIndex)
    and (U is TKMUnitWarrior)
    and (not U.IsDeadOrDying)
    and (not TKMUnitWarrior(U).IsSameGroup(TKMUnitWarrior(fShownUnit)))
    and (UnitGroups[U.UnitType] = UnitGroups[fShownUnit.UnitType]) then
      fResource.Cursors.Cursor := kmc_JoinYes
    else
      fResource.Cursors.Cursor := kmc_JoinNo;
    Exit;
  end;

  if (MyPlayer.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y)<>nil)or
     (MyPlayer.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y)<>nil) then begin
    fResource.Cursors.Cursor := kmc_Info;
    Exit;
  end;

  if fShownUnit is TKMUnitWarrior then
  begin
    if (MyPlayer.FogOfWar.CheckTileRevelation(GameCursor.Cell.X, GameCursor.Cell.Y, false)>0) then
    begin
      U := fTerrain.UnitsHitTest (GameCursor.Cell.X, GameCursor.Cell.Y);
      H := fPlayers.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
      if ((U<>nil) and (not U.IsDeadOrDying) and (fPlayers.CheckAlliance(MyPlayer.PlayerIndex, U.GetOwner) = at_Enemy)) or
         ((H<>nil) and (fPlayers.CheckAlliance(MyPlayer.PlayerIndex, H.GetOwner) = at_Enemy)) then
        fResource.Cursors.Cursor := kmc_Attack
      else
      if not fGame.Viewport.Scrolling then
        fResource.Cursors.Cursor := kmc_Default;
      Exit;
    end;
  end;

  if not fGame.Viewport.Scrolling then
    fResource.Cursors.Cursor := kmc_Default;
end;


procedure TKMGamePlayInterface.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var P:TKMPoint; U:TKMUnit; H:TKMHouse; OldSelected: TObject;
begin
  fMyControls.MouseMove(X,Y,Shift);
  if (fMyControls.CtrlOver <> nil) and (fMyControls.CtrlOver <> Image_DirectionCursor) and
      not SelectingTroopDirection then begin
    fMyControls.MouseUp(X,Y,Shift,Button);
    exit;
  end;

  if fGame.GameState <> gsRunning then exit;

  P := GameCursor.Cell; //It's used in many places here

  if (Button = mbMiddle) and DEBUG_CHEATS and (MULTIPLAYER_CHEATS or not fGame.MultiplayerMode) then
    fGame.GameInputProcess.CmdTemp(gic_TempAddScout, P);

  //Select direction
  if Button = mbRight then
    ReleaseDirectionSelector;

  //Attack or Walk
  if (Button = mbRight) and (not fJoiningGroups) and(fShownUnit is TKMUnitWarrior)
    and TKMUnitWarrior(fShownUnit).GetCommander.ArmyCanTakeOrders //Can't give orders to busy warriors
    and(fShownUnit.GetOwner = MyPlayer.PlayerIndex) then
  begin
    //Try to Attack unit
    U := fTerrain.UnitsHitTest(P.X, P.Y);
    if (U <> nil) and (not U.IsDeadOrDying) and
    (fPlayers.CheckAlliance(MyPlayer.PlayerIndex, U.GetOwner) = at_Enemy) then
    begin
      fGame.GameInputProcess.CmdArmy(gic_ArmyAttackUnit, TKMUnitWarrior(fShownUnit).GetCommander, U);
      fSoundLib.PlayWarrior(fShownUnit.UnitType, sp_Attack);
    end
    else
    begin //If there's no unit - try to Attack house
      H := fPlayers.HousesHitTest(P.X, P.Y);
      if (H <> nil) and (not H.IsDestroyed) and
      (fPlayers.CheckAlliance(MyPlayer.PlayerIndex, H.GetOwner) = at_Enemy) then
      begin
        fGame.GameInputProcess.CmdArmy(gic_ArmyAttackHouse, TKMUnitWarrior(fShownUnit).GetCommander, H);
        fSoundLib.PlayWarrior(fShownUnit.UnitType, sp_Attack);
      end
      else //If there's no house - Walk to spot
        if fShownUnit.CanWalkTo(P, 0) then
        begin
          fGame.GameInputProcess.CmdArmy(gic_ArmyWalk, TKMUnitWarrior(fShownUnit), P, SelectedDirection);
          fSoundLib.PlayWarrior(fShownUnit.UnitType, sp_Move);
        end;
    end;
  end;

  //Cancel join
  if (Button = mbRight) then begin
    if Panel_Build.Visible then
      SwitchPage(Button_Main[5]);
    if fJoiningGroups then
      Army_HideJoinMenu(nil);
  end;

  if Button = mbLeft then
  if fJoiningGroups and (fShownUnit <> nil) and (fShownUnit is TKMUnitWarrior) then
  begin
    U  := MyPlayer.UnitsHitTest(P.X, P.Y); //Scan only teammates
    if (U is TKMUnitWarrior) and (not U.IsDeadOrDying) and
       (not TKMUnitWarrior(U).IsSameGroup(TKMUnitWarrior(fShownUnit))) and
       (UnitGroups[U.UnitType] = UnitGroups[fShownUnit.UnitType]) then
    begin
      fGame.GameInputProcess.CmdArmy(gic_ArmyLink, TKMUnitWarrior(fShownUnit), U);
      fSoundLib.PlayWarrior(fShownUnit.UnitType, sp_Join);
      Army_HideJoinMenu(nil);
    end;
    exit;
  end;

  if Button = mbLeft then //Only allow placing of roads etc. with the left mouse button
  if MyPlayer.FogOfWar.CheckTileRevelation(P.X, P.Y, False) > 0 then
  case GameCursor.Mode of
    cm_None:  begin
                //You cannot select nil (or unit/house from other team) simply by clicking on the terrain
                OldSelected := fPlayers.Selected;
                if (not fPlayers.HitTest(P.X, P.Y)) or
                  ((fPlayers.Selected is TKMHouse) and (TKMHouse(fPlayers.Selected).GetOwner <> MyPlayer.PlayerIndex))or
                  ((fPlayers.Selected is TKMUnit) and (TKMUnit(fPlayers.Selected).GetOwner <> MyPlayer.PlayerIndex)) then
                  fPlayers.Selected := OldSelected;

                if (fPlayers.Selected is TKMHouse) then
                  ShowHouseInfo(TKMHouse(fPlayers.Selected));

                if (fPlayers.Selected is TKMUnit) then
                begin
                  ShowUnitInfo(TKMUnit(fPlayers.Selected));
                  if OldSelected <> fPlayers.Selected then
                  begin
                    if fPlayers.Selected is TKMUnitWarrior then
                      fSoundLib.PlayWarrior(TKMUnit(fPlayers.Selected).UnitType, sp_Select)
                    else
                      fSoundLib.PlayCitizen(TKMUnit(fPlayers.Selected).UnitType, sp_Select);
                  end;
                end;
              end;
    cm_Road:  if KMSamePoint(LastDragPoint,KMPoint(0,0)) then fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Road);
    cm_Field: if KMSamePoint(LastDragPoint,KMPoint(0,0)) then fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Corn);
    cm_Wine:  if KMSamePoint(LastDragPoint,KMPoint(0,0)) then fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Wine);
    cm_Wall:  fGame.GameInputProcess.CmdBuild(gic_BuildAddFieldPlan, P, ft_Wall);
    cm_Houses:if MyPlayer.CanAddHousePlan(P, THouseType(GameCursor.Tag1)) then
              begin
                fGame.GameInputProcess.CmdBuild(gic_BuildHousePlan, P, THouseType(GameCursor.Tag1));
                Build_ButtonClick(Button_BuildRoad);
              end
              else
                fSoundLib.Play(sfx_CantPlace,P,false,4.0);
    cm_Erase: if KMSamePoint(LastDragPoint,KMPoint(0,0)) then
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
  end;
  LastDragPoint := KMPoint(0,0);
end;


//e.g. if we're over a scrollbar it shouldn't zoom map,
//but this can apply for all controls (i.e. only zoom when over the map not controls)
procedure TKMGamePlayInterface.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);
var PrevCursor: TKMPointF;
begin
  fMyControls.MouseWheel(X, Y, WheelDelta);
  if (X < 0) or (Y < 0) then exit; //This occours when you use the mouse wheel on the window frame
  if MOUSEWHEEL_ZOOM_ENABLE and ((fMyControls.CtrlOver = nil) or fGame.ReplayMode) and
     (fGame.GameState in [gsReplay,gsRunning]) then
  begin
    fGame.UpdateGameCursor(X, Y, Shift); //Make sure we have the correct cursor position to begin with
    PrevCursor := GameCursor.Float;
    fGame.Viewport.Zoom := fGame.Viewport.Zoom + WheelDelta/2000;
    fGame.UpdateGameCursor(X, Y, Shift); //Zooming changes the cursor position
    //Move the center of the screen so the cursor stays on the same tile, thus pivoting the zoom around the cursor
    fGame.Viewport.Position := KMPointF(fGame.Viewport.Position.X + PrevCursor.X-GameCursor.Float.X,
                                   fGame.Viewport.Position.Y + PrevCursor.Y-GameCursor.Float.Y);
    fGame.UpdateGameCursor(X, Y, Shift); //Recentering the map changes the cursor position
  end;
end;


procedure TKMGamePlayInterface.Save(SaveStream:TKMemoryStream);
begin
  SaveStream.Write(fLastSaveName);
  SaveStream.Write(LastSchoolUnit);
  SaveStream.Write(LastBarracksUnit);
  fMessageList.Save(SaveStream);
  //Everything else (e.g. ShownUnit or AskDemolish) can't be seen in Save_menu anyways
end;


procedure TKMGamePlayInterface.Load(LoadStream:TKMemoryStream);
begin
  LoadStream.Read(fLastSaveName);
  LoadStream.Read(LastSchoolUnit);
  LoadStream.Read(LastBarracksUnit);
  fMessageList.Load(LoadStream);
  //Everything else (e.g. ShownUnit or AskDemolish) can't be seen in Save_menu anyways
  Message_UpdateStack;
  fLog.AppendLog('Interface loaded');
end;


procedure TKMGamePlayInterface.SaveMapview(SaveStream:TKMemoryStream);
begin
  fMapView.Save(SaveStream);
end;


procedure TKMGamePlayInterface.LoadMapview(LoadStream:TKMemoryStream);
begin
  fMapView.Load(LoadStream);
end;


procedure TKMGamePlayInterface.UpdateMapSize(X, Y: Integer);
begin
  fMapView.UpdateMapSize(X,Y);
  fMapView.Update(False);
  Minimap.UpdateFrom(fMapView);
  Minimap.MapSize := KMPoint(X, Y);
  Minimap.ViewArea := fGame.Viewport.GetMinimapClip;
end;


{Should update any items changed by game (resource counts, hp, etc..)}
{If it ever gets a bottleneck then some static Controls may be excluded from update}
procedure TKMGamePlayInterface.UpdateState;
var i:Integer; S:String;
begin
  //Every 1000ms
  if fGame.GlobalTickCount mod 10 = 0 then
    if (fGame.GameState in [gsRunning, gsReplay]) then
      fMapView.Update(False);

  Minimap.ViewArea := fGame.Viewport.GetMinimapClip;

  if fShownUnit<>nil then ShowUnitInfo(fShownUnit,fAskDismiss) else
  if fShownHouse<>nil then ShowHouseInfo(fShownHouse,fAskDemolish);

  Label_GameTime.Caption := Format(fTextLibrary[TX_GAME_TIME],[FormatDateTime('h:nn:ss', fGame.GetMissionTime)]);
  if fGame.GameOptions.Peacetime <> 0 then
    Label_PeacetimeRemaining.Caption := Format(fTextLibrary[TX_MP_PEACETIME_REMAINING],
                                               [FormatDateTime('h:nn:ss', fGame.GetPeacetimeRemaining)])
  else Label_PeacetimeRemaining.Caption := '';

  if fShownUnit=nil then fJoiningGroups := false;

  if fGame.GameInputProcess.ReplayState = gipReplaying then
  begin
    Panel_Replay.Show;
    PercentBar_Replay.Position := EnsureRange(Round(fGame.GameTickCount / fGame.GameInputProcess.GetLastTick * 100), 0, 100);
    Label_Replay.Caption := FormatDateTime('hh:nn:ss', fGame.GetMissionTime) + ' / ' +
                            FormatDateTime('hh:nn:ss', fGame.GameInputProcess.GetLastTick/24/60/60/10);
  end else
    Panel_Replay.Hide;

  if Image_Clock.Visible then begin
    Image_Clock.TexID := ((Image_Clock.TexID - 556) + 1) mod 16 + 556;
    Label_Clock.Caption := FormatDateTime('hh:nn:ss', fGame.GetMissionTime);
  end;

  if Panel_Build.Visible then Build_Fill(nil);
  if Panel_Stats.Visible then Stats_Fill(nil);
  if Panel_Menu.Visible then Menu_Fill(nil);

  //Debug info
  if SHOW_SPRITE_COUNT then
    Label_Stat.Caption:=
        inttostr(fPlayers.GetUnitCount)+' units on map'+#124+
        inttostr(fRenderPool.RenderList.Stat_Sprites)+'/'+inttostr(fRenderPool.RenderList.Stat_Sprites2)+' sprites/rendered'+#124+
        inttostr(CtrlPaintCount)+' controls rendered';

  if SHOW_POINTER_COUNT then
    Label_PointerCount.Caption := Format('Pointers: %d units, %d houses', [MyPlayer.Units.GetTotalPointers, MyPlayer.Houses.GetTotalPointers]);

  if SHOW_CMDQUEUE_COUNT then
    Label_CmdQueueCount.Caption := inttostr(fGame.GameInputProcess.Count)+' commands stored';

  if SHOW_NETWORK_DELAY and fGame.MultiplayerMode then
    Label_NetworkDelay.Caption := 'Network delay: '+inttostr(TGameInputProcess_Multi(fGame.GameInputProcess).GetNetworkDelay);

  if DISPLAY_SOUNDS then
    Label_SoundsCount.Caption := inttostr(fSoundLib.ActiveCount)+' sounds playing';

  // Temporary inteface (By @Crow)
  if SHOW_ARMYEVALS then begin
    S := '';
    for i := 0 to fPlayers.Count-1 do
    if i <> MyPlayer.PlayerIndex then
      S := S+Format('Enemy %d: %f|', [i, RoundTo(MyPlayer.ArmyEval.Evaluations[i].fPower,-3)]);
    Label_VictoryChance.Caption := S;
  end;

  //Flash unread message display
  Label_MPChatUnread.Visible := fGame.MultiplayerMode and (Label_MPChatUnread.Caption <> '') and not (fGame.GlobalTickCount mod 10 < 5);
  Image_MPChat.Highlight := Panel_Chat.Visible or (Label_MPChatUnread.Visible and (Label_MPChatUnread.Caption <> ''));
  Image_MPAllies.Highlight := Panel_Allies.Visible;

  if Panel_NetWait.Visible then
  begin
    if fGame.Networking.IsReconnecting then
      Label_NetDropPlayersDelay.Caption := ''
    else
    begin
      i := NET_DROP_PLAYER_MIN_WAIT - EnsureRange((GetTickCount-fNetWaitDropPlayersDelayStarted) div 1000, 0, NET_DROP_PLAYER_MIN_WAIT);
      if i > 0 then
        Label_NetDropPlayersDelay.Caption := Format(fTextLibrary[TX_GAMEPLAY_DROP_PLAYERS_DELAY], [i])
      else
        Label_NetDropPlayersDelay.Caption := fTextLibrary[TX_GAMEPLAY_DROP_PLAYERS_ALLOWED];
      Button_NetDropPlayers.Enabled := i = 0;
    end;
  end;
end;


end.
