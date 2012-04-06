unit KM_InterfaceMainMenu;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  StrUtils, SysUtils, KromUtils, KromOGLUtils, Math, Classes, Controls,
  KM_Controls, KM_Defaults, KM_Settings, KM_Maps, KM_Campaigns, KM_Saves, KM_Pics,
  KM_InterfaceDefaults, KM_MapView, KM_ServerQuery;


type
  TMenuScreen = (msError, msLoading, msMain, msOptions, msResults, msResultsMP);


  TKMMainMenuInterface = class (TKMUserInterface)
  private
    Campaign_Selected:TKMCampaign;
    Campaign_MapIndex:byte;

    fServerSelected: Boolean;
    fSelectedRoomInfo: TKMRoomInfo;
    fSelectedServerInfo: TKMServerInfo;

    fMapView: TKMMapView;

    fJumpToSelectedMap: Boolean;
    fJumpToSelectedSave: Boolean;
    fJumpToSelectedServer: Boolean;

    fMap_Selected: Integer; //Selected map
    fMapCRC_Selected: Cardinal; //CRC of selected map
    fMaps: TKMapsCollection;
    fMapsMP: TKMapsCollection;
    fSave_Selected: Integer; //Selected save
    fSaveCRC_Selected: Cardinal; //CRC of selected save
    fSaves: TKMSavesCollection;
    fSavesMP: TKMSavesCollection;
    MapEdSizeX,MapEdSizeY:integer; //Map Editor map size
    //We remember old values to enable "Apply" button dynamicaly
    OldFullScreen: Boolean;
    OldResolutionID: TResIndex;

    procedure Create_MainMenu_Page;
    procedure Create_SinglePlayer_Page;
    procedure Create_Campaign_Page;
    procedure Create_Single_Page;
    procedure Create_Load_Page;
    procedure Create_MultiPlayer_Page;
    procedure Create_Lobby_Page;
    procedure Create_MapEditor_Page;
    procedure Create_Replays_Page;
    procedure Create_Options_Page;
    procedure Create_Credits_Page;
    procedure Create_Loading_Page;
    procedure Create_Error_Page;
    procedure Create_Results_Page;
    procedure Create_ResultsMP_Page;
    procedure SwitchMenuPage(Sender: TObject);
    procedure MainMenu_PlayTutorial(Sender: TObject);
    procedure MainMenu_PlayBattle(Sender: TObject);
    procedure MainMenu_ReplayLastMap(Sender: TObject);
    procedure Campaign_Set(aCampaign:TKMCampaign);
    procedure Campaign_SelectMap(Sender: TObject);
    procedure Campaign_StartMap(Sender: TObject);

    procedure SingleMap_Clear;
    procedure SingleMap_RefreshList(Sender: TObject);
    procedure SingleMap_ScrollChange(Sender: TObject);
    procedure SingleMap_MapClick(Sender: TObject);
    procedure SingleMap_SelectMap(aIndex: Integer);
    procedure SingleMap_Start(Sender: TObject);
    procedure SingleMap_Sort(Sender: TObject);

    procedure MP_Init(Sender: TObject);
    procedure MP_BindEvents;
    procedure MP_SaveSettings;
    procedure MP_Update(const aStatus:string; aColor:TColor4; aBusy:boolean);
    procedure MP_ServersUpdateList(Sender: TObject);
    procedure MP_AnnouncementsUpdated(const S: string);
    procedure MP_CreateServerClick(Sender: TObject);
    procedure MP_FindServerClick(Sender: TObject);
    procedure MP_CreateServerCancelClick(Sender: TObject);
    procedure MP_FindServerIPClick(Sender: TObject);
    procedure MP_FindServerCancelClick(Sender: TObject);
    procedure MP_ServersRefresh(Sender: TObject);
    procedure MP_ServersSort(aIndex: Integer);
    procedure MP_ServersClick(Sender: TObject);
    procedure MP_ServersDoubleClick(Sender: TObject);
    procedure MP_GetInClick(Sender: TObject);
    procedure MP_Join(aServerAddress, aPort: string; aRoom: Integer);
    procedure MP_JoinSuccess(Sender: TObject);
    procedure MP_JoinFail(const aData:string);
    procedure MP_JoinAssignedHost(Sender: TObject);
    procedure MP_HostClick(Sender: TObject);
    procedure MP_HostFail(const aData:string);
    procedure MP_BackClick(Sender: TObject);

    procedure Lobby_Reset(Sender: TObject; aPreserveMessage:boolean=false);
    procedure Lobby_GameOptionsChange(Sender: TObject);
    procedure Lobby_OnGameOptions(Sender: TObject);
    procedure Lobby_PlayersSetupChange(Sender: TObject);
    procedure Lobby_OnPlayersSetup(Sender: TObject);
    procedure Lobby_OnPingInfo(Sender: TObject);
    procedure Lobby_MapTypeSelect(Sender: TObject);
    procedure Lobby_MapTypeRefreshDone(Sender: TObject);
    procedure Lobby_MapSelect(Sender: TObject);
    procedure Lobby_OnMapName(const aData:string);
    procedure Lobby_SaveRefreshDone(Sender: TObject);
    procedure Lobby_OnReassignedToHost(Sender: TObject);
    procedure Lobby_PostKey(Sender: TObject; Key: Word);
    procedure Lobby_OnMessage(const aData:string);
    procedure Lobby_OnDisconnect(const aData:string);
    procedure Lobby_BackClick(Sender: TObject);
    procedure Lobby_StartClick(Sender: TObject);

    procedure Load_Click(Sender: TObject);
    procedure Load_Delete_Click(Sender: TObject);
    procedure Load_ListClick(Sender: TObject);
    procedure Load_RefreshList(Sender: TObject);
    procedure Load_SavesSort(aIndex: Integer);
    procedure Load_DeleteConfirmation(aVisible:boolean);
    procedure Replays_ListClick(Sender: TObject);
    procedure Replay_TypeChange(Sender: TObject);
    procedure Replays_RefreshList(Sender: TObject);
    procedure Replays_SavesSort(aIndex: Integer);
    procedure Replays_Play(Sender: TObject);
    procedure MapEditor_Start(Sender: TObject);
    procedure MapEditor_SizeChange(Sender: TObject);
    procedure MapEditor_MapTypeChange(Sender: TObject);
    procedure MapEditor_ListUpdate;
    procedure MapEditor_ListUpdateDone(Sender: TObject);
    procedure MapEditor_SelectMap(Sender: TObject);
    procedure Options_Fill(aMainSettings: TMainSettings; aGameSettings: TGameSettings);
    procedure Options_Change(Sender: TObject);
    procedure Options_ChangeRes(Sender: TObject);
    procedure Options_ApplyRes(Sender: TObject);
    procedure Options_FlagClick(Sender: TObject);
    procedure Options_Refresh_DropBoxes;
  protected
    Panel_Main:TKMPanel;
      Label_Version:TKMLabel;
    Panel_MainMenu:TKMPanel;
      Panel_MMButtons:TKMPanel;
      Button_MM_SinglePlayer,
      Button_MM_MultiPlayer,
      Button_MM_MapEd,
      Button_MM_Replays,
      Button_MM_Options,
      Button_MM_Credits,
      Button_MM_Quit: TKMButton;
    Panel_SinglePlayer:TKMPanel;
      Panel_SPButtons:TKMPanel;
      Button_SP_Tutor,
      Button_SP_Fight,
      Button_SP_TSK,
      Button_SP_TPR,
      Button_SP_Single,
      Button_SP_Load: TKMButton;
      Button_SP_Back: TKMButton;
    Panel_MultiPlayer: TKMPanel;
      Panel_MPAnnouncement: TKMPanel;
        Memo_MP_Announcement: TKMMemo;
      ColList_Servers: TKMColumnListBox;
      Label_Servers_Status: TKMLabel;
      Button_MP_Back: TKMButton;
      Button_MP_Refresh: TKMButton;
      Button_MP_GetIn: TKMButton;

      Panel_MPPlayerName: TKMPanel;
        Edit_MP_PlayerName: TKMEdit;
        Label_MP_Status: TKMLabel;
      Button_MP_CreateServer: TKMButton;
      Button_MP_FindServer: TKMButton;
      Panel_MPServerDetails:TKMPanel;
        Label_MP_Players:TKMLabel;
        Label_MP_Map:TKMLabel;
        Label_MP_GameTime:TKMLabel;

      //PopUps
      Panel_MPCreateServer: TKMPanel;
        Edit_MP_ServerName,
        Edit_MP_ServerPort: TKMEdit;
        Button_MP_CreateLAN,
        Button_MP_CreateWAN: TKMButton;
        Button_MP_CreateServerCancel: TKMButton;
      Panel_MPFindServer: TKMPanel;
        Button_MP_FindServerIP: TKMButton;
        Button_MP_FindCancel: TKMButton;
        Edit_MP_FindIP,
        Edit_MP_FindPort,
        Edit_MP_FindRoom: TKMEdit;

    Panel_Lobby:TKMPanel;
      Panel_LobbyServerName:TKMPanel;
        Label_LobbyServerName:TKMLabel;

      Panel_LobbyPlayers:TKMPanel;
        Image_LobbyFlag:array [0..MAX_PLAYERS-1] of TKMImage;
        DropBox_LobbyPlayerSlot:array [0..MAX_PLAYERS-1] of TKMDropList;
        Label_LobbyPlayer:array [0..MAX_PLAYERS-1] of TKMLabel;
        DropBox_LobbyLoc:array [0..MAX_PLAYERS-1] of TKMDropList;
        DropBox_LobbyTeam:array [0..MAX_PLAYERS-1] of TKMDropList;
        Drop_LobbyColors:array [0..MAX_PLAYERS-1] of TKMDropColumns;
        CheckBox_LobbyReady:array [0..MAX_PLAYERS-1] of TKMCheckBox;
        Label_LobbyPing:array [0..MAX_PLAYERS-1] of TKMLabel;

      Panel_LobbySetup:TKMPanel;
        CheckBox_LobbyHostControl: TKMCheckBox;
        Label_LobbyChooseMap: TKMLabel;
        Radio_LobbyMapType:TKMRadioGroup;
        List_Lobby:TKMDropList;
        Label_LobbyMapName:TKMLabel;
        Memo_LobbyMapDesc: TKMMemo;
        Label_LobbyMapMode:TKMLabel;
        Label_LobbyMapCond:TKMLabel;
        Label_LobbyMapSize:TKMLabel;
        TrackBar_LobbyPeacetime: TKMTrackBar;
        Minimap_LobbyPreview: TKMMinimap;

      Memo_LobbyPosts:TKMMemo;
      Label_LobbyPost:TKMLabel;
      Edit_LobbyPost:TKMEdit;

      Button_LobbyBack:TKMButton;
      Button_LobbyStart:TKMButton;

    Panel_Campaign:TKMPanel;
      Image_CampaignBG:TKMImage;
      Image_CampaignFlags:array[0..MAX_CAMP_MAPS - 1] of TKMImage;
      Image_CampaignSubNode:array[0..MAX_CAMP_NODES - 1] of TKMImage;
      Panel_CampScroll:TKMPanel;
        Image_ScrollTop,Image_Scroll:TKMImage;
        Label_CampaignTitle,Label_CampaignText:TKMLabel;
      Button_CampaignStart,Button_CampaignBack:TKMButton;
    Panel_Single:TKMPanel;
      Minimap_SinglePreview: TKMMinimap;
      Panel_SingleList,Panel_SingleDesc:TKMPanel;
      Button_SingleHeadMode,Button_SingleHeadTeams,Button_SingleHeadTitle,Button_SingleHeadSize:TKMButton;
      Bevel_SingleBG: array of array[1..4]of TKMBevel;
      Image_SingleMode: array of TKMImage;
      Label_SinglePlayers,Label_SingleSize,
      Label_SingleTitle1,Label_SingleTitle2: array of TKMLabel;
      Shape_SingleOverlay: array of TKMShape;
      ScrollBar_SingleMaps:TKMScrollBar;
      Shape_SingleMap:TKMShape;
      Label_SingleTitle:TKMLabel;
      Memo_SingleDesc:TKMMemo;
      Label_SingleCondTyp,Label_SingleCondWin,Label_SingleCondDef:TKMLabel;
      Label_SingleAllies,Label_SingleEnemies:TKMLabel;
      Button_SingleBack,Button_SingleStart:TKMButton;
    Panel_Load:TKMPanel;
      List_Load: TKMColumnListBox;
      Button_Load: TKMButton;
      Button_Delete: TKMButton;
      Label_DeleteConfirm: TKMLabel;
      Button_DeleteYes, Button_DeleteNo: TKMButton;
      Button_LoadBack:TKMButton;
      Minimap_LoadPreview: TKMMinimap;
    Panel_Replays:TKMPanel;
      Radio_Replays_Type:TKMRadioGroup;
      List_Replays: TKMColumnListBox;
      Button_ReplaysPlay: TKMButton;
      Button_ReplaysBack:TKMButton;
      Minimap_ReplayPreview: TKMMinimap;
    Panel_MapEd:TKMPanel;
      Panel_MapEd_SizeXY:TKMPanel;
      Radio_MapEd_SizeX,Radio_MapEd_SizeY:TKMRadioGroup;
      Panel_MapEd_Load:TKMPanel;
      List_MapEd:TKMListBox;
      Radio_MapEd_MapType:TKMRadioGroup;
      Minimap_MapEd:TKMMinimap;
      Button_MapEdBack,Button_MapEd_Create,Button_MapEd_Load:TKMButton;
    Panel_Options:TKMPanel;
      Panel_Options_GFX:TKMPanel;
        TrackBar_Options_Brightness:TKMTrackBar;
        CheckBox_Options_Shadows:TKMCheckBox;
      Panel_Options_Ctrl:TKMPanel;
        TrackBar_Options_ScrollSpeed:TKMTrackBar;
      Panel_Options_Game:TKMPanel;
        CheckBox_Options_Autosave:TKMCheckBox;
      Panel_Options_Sound:TKMPanel;
        Label_Options_MusicOff: TKMLabel;
        TrackBar_Options_SFX,TrackBar_Options_Music:TKMTrackBar;
        CheckBox_Options_MusicOff:TKMCheckBox;
        CheckBox_Options_ShuffleOn:TKMCheckBox;
      Panel_Options_Lang:TKMPanel;
        Radio_Options_Lang:TKMRadioGroup;
        Image_Options_Lang_Flags:array of TKMImage;
      Panel_Options_Res:TKMPanel;
        CheckBox_Options_FullScreen:TKMCheckBox;
        DropBox_Options_Resolution:TKMDropList;
        DropBox_Options_RefreshRate:TKMDropList;
        Button_Options_ResApply:TKMButton;
      Button_Options_Back:TKMButton;
    Panel_Credits:TKMPanel;
      Label_Credits_KaM:TKMLabelScroll;
      Label_Credits_Remake:TKMLabelScroll;
      Button_CreditsBack:TKMButton;
    Panel_Loading:TKMPanel;
      Label_Loading:TKMLabel;
    Panel_Error:TKMPanel;
      Label_Error:TKMLabel;
      Button_ErrorBack:TKMButton;
    Panel_Results:TKMPanel;
      Label_Results:TKMLabel;
      Panel_Stats: TKMPanel;
      Label_Stat:array[1..9]of TKMLabel;
      Button_ResultsBack,Button_ResultsRepeat,Button_ResultsContinue:TKMButton;
    Panel_ResultsMP:TKMPanel;
      Label_ResultsMP, Label_ResultsMPTime: TKMLabel;
      Panel_StatsMP1, Panel_StatsMP2: TKMPanel;
      Label_ResultsPlayerName1, Label_ResultsPlayerName2:array[0..MAX_PLAYERS-1] of TKMLabel;
      Bar_Results:array[0..MAX_PLAYERS-1, 0..9] of TKMPercentBar;
      Button_ResultsMPBack:TKMButton;
  public
    constructor Create(X,Y:word);
    destructor Destroy; override;
    procedure Resize(X,Y:word);
    procedure ShowScreen(aScreen: TMenuScreen; const aText: string=''; aMsg: TGameResultMsg=gr_Silent);
    procedure AppendLoadingText(const aText:string);
    procedure Fill_Results;
    procedure Fill_ResultsMP;
    function GetChatText:string;
    function GetChatMessages:string;

    procedure KeyDown(Key:Word; Shift: TShiftState);
    procedure KeyUp(Key:Word; Shift: TShiftState);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);

    procedure UpdateState; override;
  end;


implementation
uses KM_Main, KM_NetworkTypes, KM_TextLibrary, KM_Game, KM_PlayersCollection, KM_Locales,
  KM_Utils, KM_Log, KM_Sound, KM_Networking, KM_ResourceSprites;

const
  MENU_SP_MAPS_COUNT    = 14;           //Number of single player maps to display in menu
  MENU_SP_MAPS_HEIGHT   = 40;


{ TKMMainMenuInterface }
constructor TKMMainMenuInterface.Create(X,Y: Word);
var S: TKMShape;
begin
  inherited;
  Assert(fTextLibrary <> nil, 'fTextLibrary should be initialized before MainMenuInterface');

  Campaign_MapIndex := 1;

  fMapView := TKMMapView.Create(nil, False, True);

  fMaps := TKMapsCollection.Create(False);
  fMapsMP := TKMapsCollection.Create(True);
  fMap_Selected := -1; //None
  fSaves := TKMSavesCollection.Create(False);
  fSavesMP := TKMSavesCollection.Create(True);

  Panel_Main := TKMPanel.Create(fMyControls, 0,
                                             0,
                                             MENU_DESIGN_X,
                                             MENU_DESIGN_Y); //Parent Panel for whole menu

  //Background is the same for all pages, except Results/Campaign, which will render ontop
  with TKMImage.Create(Panel_Main,-448,-216,960,600,1,rxMenu) do Anchors := [];
  with TKMImage.Create(Panel_Main, 512,-216,960,600,2,rxMenu) do Anchors := [];
  with TKMImage.Create(Panel_Main,-448, 384,960,600,3,rxMenu) do Anchors := [];
  with TKMImage.Create(Panel_Main, 512, 384,960,600,4,rxMenu) do Anchors := [];

  Create_MainMenu_Page;
  Create_SinglePlayer_Page;
    Create_Campaign_Page;
    Create_Single_Page;
    Create_Load_Page;
  Create_MultiPlayer_Page;
    Create_Lobby_Page;
  Create_MapEditor_Page;
  Create_Replays_Page;
  Create_Options_Page;
  Create_Credits_Page;
  Create_Loading_Page;
  Create_Error_Page;
  Create_Results_Page;
  Create_ResultsMP_Page;


    {for i:=1 to length(FontFiles) do L[i]:=TKMLabel.Create(Panel_Main1,550,280+i*20,160,30,'This is a test string for KaM Remake ('+FontFiles[i],TKMFont(i),taLeft);//}
    //MyControls.AddTextEdit(Panel_Main, 32, 32, 200, 20, fnt_Grey);

  //Show version info on every page
  Label_Version := TKMLabel.Create(Panel_Main, 8, 8, 0, 0, '', fnt_Antiqua, taLeft);

  if OVERLAY_RESOLUTIONS then
  begin
    S := TKMShape.Create(Panel_Main, 0, 96, 1024, 576, $FF00FFFF);
    S.Anchors := [];
    S.LineWidth := 1;
    S.Hitable := False;
    S := TKMShape.Create(Panel_Main, 0, 0, 1024, 768, $FF00FF00);
    S.Anchors := [];
    S.LineWidth := 1;
    S.Hitable := False;
  end;

  fLog.AppendLog('Main menu init done');
//Use ShowScreen to select properscreen after fGame init is done
end;


destructor TKMMainMenuInterface.Destroy;
begin
  fMaps.Free;
  fMapsMP.Free;
  fSaves.Free;
  fSavesMP.Free;
  fMapView.Free;
  inherited;
end;


//Keep Panel_Main centered
procedure TKMMainMenuInterface.Resize(X, Y: Word);
begin
  Panel_Main.Width  := Min(X, MENU_DESIGN_X);
  Panel_Main.Height := Min(Y, MENU_DESIGN_Y);

  Panel_Main.Left := (X - Panel_Main.Width) div 2;
  Panel_Main.Top  := (Y - Panel_Main.Height) div 2;
end;


procedure TKMMainMenuInterface.ShowScreen(aScreen: TMenuScreen; const aText: string=''; aMsg: TGameResultMsg=gr_Silent);
begin
  case aScreen of
    msError:    begin
                  Label_Error.Caption := aText;
                  SwitchMenuPage(Panel_Error);
                end;
    msLoading:  begin
                  Label_Loading.Caption := aText;
                  SwitchMenuPage(Panel_Loading);
                end;
    msMain:     SwitchMenuPage(nil);
    msOptions:  SwitchMenuPage(Button_MM_Options);
    msResults:  begin
                  case aMsg of
                    gr_Win:       Label_Results.Caption := fTextLibrary[TX_MENU_MISSION_VICTORY];
                    gr_Defeat:    Label_Results.Caption := fTextLibrary[TX_MENU_MISSION_DEFEAT];
                    gr_Cancel:    Label_Results.Caption := fTextLibrary[TX_MENU_MISSION_CANCELED];
                    else          Label_Results.Caption := '<<<LEER>>>'; //Thats string used in all Synetic games for missing texts =)
                  end;

                  Button_ResultsRepeat.Enabled := aMsg in [gr_Defeat, gr_Cancel];

                  //Even if the campaign is complete Player can now return to it's screen to replay any of the maps
                  Button_ResultsContinue.Visible := fGame.Campaigns.ActiveCampaign <> nil;
                  Button_ResultsContinue.Enabled := aMsg = gr_Win;

                  SwitchMenuPage(Panel_Results);
                end;
    msResultsMP:begin
                  case aMsg of
                    gr_Win:       Label_ResultsMP.Caption := fTextLibrary[TX_MENU_MISSION_VICTORY];
                    gr_Defeat:    Label_ResultsMP.Caption := fTextLibrary[TX_MENU_MISSION_DEFEAT];
                    gr_Cancel:    Label_ResultsMP.Caption := fTextLibrary[TX_MENU_MISSION_CANCELED];
                    else          Label_ResultsMP.Caption := '<<<LEER>>>'; //Thats string used in all Synetic games for missing texts =)
                  end;
                  SwitchMenuPage(Panel_ResultsMP);
                end;
  end;
end;


procedure TKMMainMenuInterface.AppendLoadingText(const aText: string);
begin
  Label_Loading.Caption := Label_Loading.Caption + aText + '|';
end;


function TKMMainMenuInterface.GetChatText:string;
begin
  Result := Edit_LobbyPost.Text;
end;


function TKMMainMenuInterface.GetChatMessages:string;
begin
  Result := Memo_LobbyPosts.Text;
end;


procedure TKMMainMenuInterface.Fill_Results;
begin
  if (MyPlayer=nil) or (MyPlayer.Stats=nil) then exit;

  with MyPlayer.Stats do
  begin
    Label_Stat[1].Caption := inttostr(GetCitizensLost + GetWarriorsLost);
    Label_Stat[2].Caption := inttostr(GetCitizensKilled + GetWarriorsKilled);
    Label_Stat[3].Caption := inttostr(GetHousesLost);
    Label_Stat[4].Caption := inttostr(GetHousesDestroyed);
    Label_Stat[5].Caption := inttostr(GetHousesBuilt);
    Label_Stat[6].Caption := inttostr(GetCitizensTrained);
    Label_Stat[7].Caption := inttostr(GetWeaponsProduced);
    Label_Stat[8].Caption := inttostr(GetWarriorsTrained);
    Label_Stat[9].Caption := FormatDateTime('hh:nn:ss', fGame.GetMissionTime);
  end;
end;


procedure TKMMainMenuInterface.Fill_ResultsMP;
var
  i,k: Integer;
  UnitsMax, HousesMax, GoodsMax, WeaponsMax, MaxValue: Integer;
begin
  Label_ResultsMPTime.Caption := FormatDateTime('hh:nn:ss', fGame.GetMissionTime);

  //Update visibility depending on players count
  for i:=0 to MAX_PLAYERS-1 do
  begin
    Label_ResultsPlayerName1[i].Visible := (i <= fPlayers.Count-1);
    Label_ResultsPlayerName2[i].Visible := (i <= fPlayers.Count-1);
    for k:=0 to 9 do
      Bar_Results[i,k].Visible := (i <= fPlayers.Count-1);
  end;

  //Update positioning
  Panel_StatsMP1.Height := 40 + fPlayers.Count * 25 + 20;
  Panel_StatsMP2.Height := 40 + fPlayers.Count * 25 + 20;

  Panel_StatsMP1.Top := 140 + (520 - Panel_StatsMP1.Height * 2) div 2;
  //Second panel does not move from the middle of the screen: results always go above and below the middle

  //Fill in raw values
  for i:=0 to fPlayers.Count-1 do
  begin
    Label_ResultsPlayerName1[i].Caption := fPlayers[i].PlayerName;
    Label_ResultsPlayerName2[i].Caption := fPlayers[i].PlayerName;

    with fPlayers[i].Stats do
    begin
      //Living things
      Bar_Results[i,0].Tag := GetCitizensTrained;
      Bar_Results[i,1].Tag := GetCitizensLost;
      Bar_Results[i,2].Tag := GetWarriorsTrained;
      Bar_Results[i,3].Tag := GetWarriorsLost;
      Bar_Results[i,4].Tag := GetCitizensKilled + GetWarriorsKilled;
      //Objects
      Bar_Results[i,5].Tag := GetHousesBuilt;
      Bar_Results[i,6].Tag := GetHousesLost;
      Bar_Results[i,7].Tag := GetHousesDestroyed;
      Bar_Results[i,8].Tag := GetGoodsProduced;
      Bar_Results[i,9].Tag := GetWeaponsProduced;
    end;
  end;

  //Update percent bars
  UnitsMax := 0;
  for k:=0 to 4 do for i:=0 to fPlayers.Count-1 do
    UnitsMax := Max(Bar_Results[i,k].Tag, UnitsMax);

  HousesMax := 0;
  for k:=5 to 7 do for i:=0 to fPlayers.Count-1 do
    HousesMax := Max(Bar_Results[i,k].Tag, HousesMax);

  GoodsMax := 0;
  for i:=0 to fPlayers.Count-1 do
    GoodsMax := Max(Bar_Results[i,8].Tag, GoodsMax);

  WeaponsMax := 0;
  for i:=0 to fPlayers.Count-1 do
    WeaponsMax := Max(Bar_Results[i,9].Tag, WeaponsMax);

  for k:=0 to 9 do
  begin
    case k of
      0..4: MaxValue := UnitsMax;
      5..7: MaxValue := HousesMax;
      8:    MaxValue := GoodsMax;
      else  MaxValue := WeaponsMax;
    end;
    for i:=0 to fPlayers.Count-1 do
    begin
      if MaxValue <> 0 then
        Bar_Results[i,k].Position := Round(Bar_Results[i,k].Tag / MaxValue * 100)
      else
        Bar_Results[i,k].Position := 0;
      Bar_Results[i,k].Caption := IfThen(Bar_Results[i,k].Tag <> 0, IntToStr(Bar_Results[i,k].Tag), '-');
    end;
  end;
end;


procedure TKMMainMenuInterface.Create_MainMenu_Page;
begin
  //Without anchors this page is centered on resize
  Panel_MainMenu := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_MainMenu.Anchors := [];
    TKMImage.Create(Panel_MainMenu, 300,  60, 423, 164, 4, rxGuiMain);
    TKMLabel.Create(Panel_MainMenu, 512, 240,   0,   0,
    '[$4040F0]R[$40F0F0]e[$40F040]m[$F0F040]a[$F04040]k[$F040F0]e', fnt_Metal, taCenter);

    with TKMImage.Create(Panel_MainMenu,  50, 220, round(218*1.3), round(291*1.3), 5, rxGuiMainH) do ImageStretch;
    with TKMImage.Create(Panel_MainMenu, 705, 220, round(207*1.3), round(295*1.3), 6, rxGuiMainH) do ImageStretch;

    Panel_MMButtons := TKMPanel.Create(Panel_MainMenu, 337, 290, 350, 400);
      Button_MM_SinglePlayer := TKMButton.Create(Panel_MMButtons,0,  0,350,30,fTextLibrary[TX_MENU_SINGLEPLAYER],fnt_Metal,bsMenu);
      Button_MM_MultiPlayer  := TKMButton.Create(Panel_MMButtons,0, 40,350,30,fTextLibrary[TX_MENU_MULTIPLAYER],fnt_Metal,bsMenu);
      Button_MM_MapEd        := TKMButton.Create(Panel_MMButtons,0, 80,350,30,fTextLibrary[TX_MENU_MAP_EDITOR],fnt_Metal,bsMenu);
      Button_MM_Replays      := TKMButton.Create(Panel_MMButtons,0,120,350,30,fTextLibrary[TX_MENU_REPLAYS],fnt_Metal,bsMenu);
      Button_MM_Options      := TKMButton.Create(Panel_MMButtons,0,160,350,30,fTextLibrary[TX_MENU_OPTIONS],fnt_Metal,bsMenu);
      Button_MM_Credits      := TKMButton.Create(Panel_MMButtons,0,200,350,30,fTextLibrary[TX_MENU_CREDITS],fnt_Metal,bsMenu);
      Button_MM_Quit         := TKMButton.Create(Panel_MMButtons,0,320,350,30,fTextLibrary[TX_MENU_QUIT],fnt_Metal,bsMenu);
      Button_MM_SinglePlayer.OnClick := SwitchMenuPage;
      Button_MM_MultiPlayer.OnClick  := SwitchMenuPage;
      Button_MM_MapEd.OnClick        := SwitchMenuPage;
      Button_MM_Replays.OnClick      := SwitchMenuPage;
      Button_MM_Options.OnClick      := SwitchMenuPage;
      Button_MM_Credits.OnClick      := SwitchMenuPage;
      Button_MM_Quit.OnClick         := fMain.Stop;
end;


procedure TKMMainMenuInterface.Create_SinglePlayer_Page;
begin
  //Without anchors this page is centered on resize
  Panel_SinglePlayer := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_SinglePlayer.Anchors := [];
    TKMImage.Create(Panel_SinglePlayer, 300, 60, 423, 164, 4, rxGuiMain);
    TKMLabel.Create(Panel_SinglePlayer, 512, 240, 0, 0, 'Remake', fnt_Metal, taCenter);
    with TKMImage.Create(Panel_SinglePlayer, 50, 220, Round(218 * 1.3), Round(291 * 1.3), 5, rxGuiMainH) do ImageStretch;
    with TKMImage.Create(Panel_SinglePlayer, 705, 220, Round(207 * 1.3), Round(295 * 1.3), 6, rxGuiMainH) do ImageStretch;

    Panel_SPButtons := TKMPanel.Create(Panel_SinglePlayer,337,290,350,400);
      Button_SP_Tutor  := TKMButton.Create(Panel_SPButtons,0,  0,350,30,fTextLibrary[TX_MENU_TUTORIAL_TOWN],fnt_Metal,bsMenu);
      Button_SP_Fight  := TKMButton.Create(Panel_SPButtons,0, 40,350,30,fTextLibrary[TX_MENU_TUTORIAL_BATTLE],fnt_Metal,bsMenu);
      Button_SP_TSK    := TKMButton.Create(Panel_SPButtons,0, 80,350,30,fTextLibrary[TX_MENU_CAMP_TSK],fnt_Metal,bsMenu);
      Button_SP_TPR    := TKMButton.Create(Panel_SPButtons,0,120,350,30,fTextLibrary[TX_MENU_CAMP_TPR],fnt_Metal,bsMenu);
      Button_SP_Single := TKMButton.Create(Panel_SPButtons,0,160,350,30,fTextLibrary[TX_MENU_SINGLE_MAP],fnt_Metal,bsMenu);
      Button_SP_Load   := TKMButton.Create(Panel_SPButtons,0,200,350,30,fTextLibrary[TX_MENU_LOAD_SAVEGAME],fnt_Metal,bsMenu);
      Button_SP_Back   := TKMButton.Create(Panel_SPButtons,0,320,350,30,fTextLibrary[TX_MENU_BACK],fnt_Metal,bsMenu);

      Button_SP_Tutor.OnClick  := MainMenu_PlayTutorial;
      Button_SP_Fight.OnClick  := MainMenu_PlayBattle;
      Button_SP_TSK.OnClick    := SwitchMenuPage;
      Button_SP_TPR.OnClick    := SwitchMenuPage;
      Button_SP_Single.OnClick := SwitchMenuPage;
      Button_SP_Load.OnClick   := SwitchMenuPage;
      Button_SP_Back.OnClick   := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_MultiPlayer_Page;
  procedure CreateServerPopUp;
  begin
    Panel_MPCreateServer := TKMPanel.Create(Panel_Main, 362, 250, 320, 300);
    Panel_MPCreateServer.Anchors := [];
      TKMBevel.Create(Panel_MPCreateServer, -1000,  -1000, 2000, 2000);
      TKMImage.Create(Panel_MPCreateServer, -20, -75, 340, 310, 15, rxGuiMain);
      TKMBevel.Create(Panel_MPCreateServer,   0,  0, 320, 300);
      TKMLabel.Create(Panel_MPCreateServer, 160, 10, 280, 20, fTextLibrary[TX_MP_MENU_CREATE_SERVER_HEADER], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_MPCreateServer, 20, 50, 280, 20, fTextLibrary[TX_MP_MENU_CREATE_SERVER_NAME], fnt_Outline, taLeft);
      Edit_MP_ServerName := TKMEdit.Create(Panel_MPCreateServer, 20, 70, 280, 20, fnt_Grey);
      TKMLabel.Create(Panel_MPCreateServer, 20, 100, 284, 20, fTextLibrary[TX_MP_MENU_CREATE_SERVER_PORT], fnt_Outline, taLeft);
      Edit_MP_ServerPort := TKMEdit.Create(Panel_MPCreateServer, 20, 120, 100, 20, fnt_Grey);
      Edit_MP_ServerPort.AllowedChars := acDigits;
      Button_MP_CreateLAN  := TKMButton.Create(Panel_MPCreateServer, 20, 170, 280, 30, fTextLibrary[TX_MP_MENU_CREATE_SERVER_LOCAL], fnt_Metal, bsMenu);
      Button_MP_CreateWAN  := TKMButton.Create(Panel_MPCreateServer, 20, 210, 280, 30, fTextLibrary[TX_MP_MENU_CREATE_SERVER_INTERNET], fnt_Metal, bsMenu);
      Button_MP_CreateServerCancel := TKMButton.Create(Panel_MPCreateServer, 20, 250, 280, 30, fTextLibrary[TX_MP_MENU_CREATE_SERVER_CANCEL], fnt_Metal, bsMenu);
      Button_MP_CreateLAN.OnClick := MP_HostClick;
      Button_MP_CreateWAN.OnClick := MP_HostClick;
      Button_MP_CreateServerCancel.OnClick := MP_CreateServerCancelClick;
  end;
  procedure FindServerPopUp;
  begin
    Panel_MPFindServer := TKMPanel.Create(Panel_Main, 362, 250, 320, 300);
    Panel_MPFindServer.Anchors := [];
      TKMBevel.Create(Panel_MPFindServer, -1000,  -1000, 2000, 2000);
      TKMImage.Create(Panel_MPFindServer, -20, -75, 340, 310, 15, rxGuiMain);
      TKMBevel.Create(Panel_MPFindServer,   0,  0, 320, 300);
      TKMLabel.Create(Panel_MPFindServer, 160, 10, 280, 20, fTextLibrary[TX_MP_MENU_FIND_SERVER_HEADER], fnt_Outline, taCenter);

      TKMLabel.Create(Panel_MPFindServer, 20, 50, 156, 20, fTextLibrary[TX_MP_MENU_FIND_SERVER_ADDRESS], fnt_Outline, taLeft);
      Edit_MP_FindIP := TKMEdit.Create(Panel_MPFindServer, 20, 70, 152, 20, fnt_Grey);
      Edit_MP_FindIP.AllowedChars := acText; //Server name could be "localhost"
      TKMLabel.Create(Panel_MPFindServer, 172, 50, 60, 20, fTextLibrary[TX_MP_MENU_FIND_SERVER_PORT], fnt_Outline, taLeft);
      Edit_MP_FindPort := TKMEdit.Create(Panel_MPFindServer, 172, 70, 60, 20, fnt_Grey);
      Edit_MP_FindPort.AllowedChars := acDigits;
      TKMLabel.Create(Panel_MPFindServer, 232, 50, 60, 20, fTextLibrary[TX_MP_MENU_FIND_SERVER_ROOM], fnt_Outline, taLeft);
      Edit_MP_FindRoom := TKMEdit.Create(Panel_MPFindServer, 232, 70, 60, 20, fnt_Grey);
      Edit_MP_FindRoom.AllowedChars := acDigits;
      Button_MP_FindServerIP := TKMButton.Create(Panel_MPFindServer, 20, 110, 280, 30, fTextLibrary[TX_MP_MENU_FIND_SERVER_FIND], fnt_Metal, bsMenu);
      Button_MP_FindServerIP.OnClick := MP_FindServerIPClick;
      Button_MP_FindCancel := TKMButton.Create(Panel_MPFindServer, 20, 150, 280, 30, fTextLibrary[TX_MP_MENU_FIND_SERVER_CANCEL], fnt_Metal, bsMenu);
      Button_MP_FindCancel.OnClick := MP_FindServerCancelClick;
  end;
begin
  Panel_MultiPlayer := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_MultiPlayer.Stretch;

    //Top area
    Panel_MPPlayerName := TKMPanel.Create(Panel_MultiPlayer, 675, 45, 320, 120);
      TKMBevel.Create(Panel_MPPlayerName, 0, 0, 320, 120);
      TKMLabel.Create(Panel_MPPlayerName, 8, 10, 304, 20, fTextLibrary[TX_MP_MENU_PLAYERNAME], fnt_Outline, taLeft);
      Edit_MP_PlayerName := TKMEdit.Create(Panel_MPPlayerName, 8, 30, 120, 20, fnt_Grey);
      TKMLabel.Create(Panel_MPPlayerName, 8, 60, 304, 20, fTextLibrary[TX_MP_MENU_STATUS], fnt_Outline, taLeft);
      Label_MP_Status := TKMLabel.Create(Panel_MPPlayerName, 8, 80, 304, 36, '', fnt_Grey, taLeft);
      Label_MP_Status.AutoWrap := True;

    Button_MP_CreateServer := TKMButton.Create(Panel_MultiPlayer, 675, 170, 320, 30, fTextLibrary[TX_MP_MENU_CREATE_SERVER], fnt_Metal, bsMenu);
    Button_MP_CreateServer.OnClick := MP_CreateServerClick;

    CreateServerPopUp;

    Button_MP_FindServer := TKMButton.Create(Panel_MultiPlayer, 675, 204, 320, 30, fTextLibrary[TX_MP_MENU_FIND_SERVER], fnt_Metal, bsMenu);
    Button_MP_FindServer.OnClick := MP_FindServerClick;

    FindServerPopUp;

    //Master server announcement
    Memo_MP_Announcement := TKMMemo.Create(Panel_MultiPlayer, 45, 45, 620, 189, fnt_Grey);
    Memo_MP_Announcement.Anchors := [akLeft, akTop];
    Memo_MP_Announcement.AutoWrap := True;
    Memo_MP_Announcement.ItemHeight := 16;

    //List of available servers
    ColList_Servers := TKMColumnListBox.Create(Panel_MultiPlayer,45,240,620,465,fnt_Metal);
    ColList_Servers.Anchors := [akLeft, akTop, akBottom];
    ColList_Servers.SetColumns(fnt_Outline, [fTextLibrary[TX_MP_MENU_SERVERLIST_NAME],fTextLibrary[TX_MP_MENU_SERVERLIST_STATE],fTextLibrary[TX_MP_MENU_SERVERLIST_PLAYERS],fTextLibrary[TX_MP_MENU_SERVERLIST_PING]],[0,300,430,525]);
    ColList_Servers.OnColumnClick := MP_ServersSort;
    ColList_Servers.OnChange := MP_ServersClick;
    ColList_Servers.OnDoubleClick := MP_ServersDoubleClick;
    Label_Servers_Status := TKMLabel.Create(Panel_MultiPlayer, 45+310, 240+230, 0, 0, '', fnt_Grey, taCenter);
    Label_Servers_Status.Anchors := [akLeft];
    Label_Servers_Status.Hide;

    //Server details area
    Panel_MPServerDetails := TKMPanel.Create(Panel_MultiPlayer, 675, 240, 320, 465);
    Panel_MPServerDetails.Anchors := [akLeft, akTop, akBottom];
      with TKMBevel.Create(Panel_MPServerDetails, 0, 0, 320, 465) do Stretch;
      TKMLabel.Create(Panel_MPServerDetails, 160, 6, 304, 20, fTextLibrary[TX_MP_MENU_HEADER_SERVER_DETAILS], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_MPServerDetails, 8, 30, 304, 20, fTextLibrary[TX_MP_MENU_GAME_INFORMATION], fnt_Outline, taLeft);
      Label_MP_Map := TKMLabel.Create(Panel_MPServerDetails, 8, 50, 304, 30, '', fnt_Metal, taLeft);
      Label_MP_GameTime := TKMLabel.Create(Panel_MPServerDetails, 8, 70, 304, 30, '--:--:--', fnt_Metal, taLeft);
      TKMLabel.Create(Panel_MPServerDetails, 8, 100, 304, 20, fTextLibrary[TX_MP_MENU_PLAYER_LIST], fnt_Outline, taLeft);
      Label_MP_Players := TKMLabel.Create(Panel_MPServerDetails, 8, 120, 304, 340, '', fnt_Metal, taLeft);
      Label_MP_Players.Anchors := [akLeft, akTop, akBottom];

    Button_MP_Back    := TKMButton.Create(Panel_MultiPlayer,  45, 720, 220, 30, fTextLibrary[TX_MENU_BACK], fnt_Metal, bsMenu);
    Button_MP_Refresh := TKMButton.Create(Panel_MultiPlayer, 275, 720, 390, 30,fTextLibrary[TX_MP_MENU_REFRESH_SERVER_LIST],fnt_Metal,bsMenu);
    Button_MP_GetIn   := TKMButton.Create(Panel_MultiPlayer, 675, 720, 320, 30,fTextLibrary[TX_MP_MENU_SERVER_JOIN], fnt_Metal, bsMenu);
    Button_MP_Back.Anchors    := [akLeft, akBottom];
    Button_MP_Refresh.Anchors := [akLeft, akBottom];
    Button_MP_GetIn.Anchors   := [akLeft, akBottom];
    Button_MP_Back.OnClick    := MP_BackClick;
    Button_MP_Refresh.OnClick := MP_ServersRefresh;
    Button_MP_GetIn.OnClick   := MP_GetInClick;
end;


procedure TKMMainMenuInterface.Create_Lobby_Page;
const CW = 630; C1 = 35; C2 = 215; C3 = 315; C4 = 415; C5 = 540; C6 = 590;
var i,k,top:integer;
begin
  Panel_Lobby := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_Lobby.Stretch;

    //Server Name
    Panel_LobbyServerName := TKMPanel.Create(Panel_Lobby, 40, 30, CW, 30);
      TKMBevel.Create(Panel_LobbyServerName,   0,  0, CW, 30);
      Label_LobbyServerName := TKMLabel.Create(Panel_LobbyServerName, 10, 10, CW-20, 20, '', fnt_Metal, taLeft);

    //Players
    Panel_LobbyPlayers := TKMPanel.Create(Panel_Lobby, 40, 65, CW, 240);
      TKMBevel.Create(Panel_LobbyPlayers,  0,  0, CW, 240);
      TKMLabel.Create(Panel_LobbyPlayers, C1, 10, 170,  20, fTextLibrary[TX_LOBBY_HEADER_PLAYERS], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, C2, 10,  90,  20, fTextLibrary[TX_LOBBY_HEADER_STARTLOCATION], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, C3, 10,  90,  20, fTextLibrary[TX_LOBBY_HEADER_TEAM], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, C4, 10,  90,  20, fTextLibrary[TX_LOBBY_HEADER_FLAGCOLOR], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, C5, 10,  90,  20, fTextLibrary[TX_LOBBY_HEADER_READY], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_LobbyPlayers, C6, 10,  70,  20, fTextLibrary[TX_LOBBY_HEADER_PING], fnt_Outline, taCenter);

      for i:=0 to MAX_PLAYERS-1 do begin
        top := 30+i*24;
        Image_LobbyFlag[i] := TKMImage.Create(Panel_LobbyPlayers, 10, top+3, 16, 11, 0, rxMenu);

        Label_LobbyPlayer[i] := TKMLabel.Create(Panel_LobbyPlayers, C1, top+2, 170, 20, '', fnt_Metal, taLeft);
        Label_LobbyPlayer[i].Hide;

        DropBox_LobbyPlayerSlot[i] := TKMDropList.Create(Panel_LobbyPlayers, C1, top, 170, 20, fnt_Metal, '');
        DropBox_LobbyPlayerSlot[i].Add(fTextLibrary[TX_LOBBY_SLOT_OPEN]); //Player can join into this slot
        DropBox_LobbyPlayerSlot[i].Add(fTextLibrary[TX_LOBBY_SLOT_CLOSED]); //Closed, nobody can join it
        DropBox_LobbyPlayerSlot[i].Add(fTextLibrary[TX_LOBBY_SLOT_AI_PLAYER]); //This slot is an AI player
        DropBox_LobbyPlayerSlot[i].ItemIndex := 0; //Open
        DropBox_LobbyPlayerSlot[i].OnChange := Lobby_PlayersSetupChange;

        DropBox_LobbyLoc[i] := TKMDropList.Create(Panel_LobbyPlayers, C2, top, 90, 20, fnt_Metal, '');
        DropBox_LobbyLoc[i].Add(fTextLibrary[TX_LOBBY_RANDOM]);
        DropBox_LobbyLoc[i].OnChange := Lobby_PlayersSetupChange;

        DropBox_LobbyTeam[i] := TKMDropList.Create(Panel_LobbyPlayers, C3, top, 90, 20, fnt_Metal, '');
        DropBox_LobbyTeam[i].Add(fTextLibrary[TX_LOBBY_NONE]);
        for k:=1 to 4 do DropBox_LobbyTeam[i].Add(Format(fTextLibrary[TX_LOBBY_TEAM_X],[k]));
        DropBox_LobbyTeam[i].OnChange := Lobby_PlayersSetupChange;

        Drop_LobbyColors[i] := TKMDropColumns.Create(Panel_LobbyPlayers, C4, top, 90, 20, fnt_Grey);
        Drop_LobbyColors[i].SetColumns(fnt_Outline, [''], [0]);
        Drop_LobbyColors[i].ShowHeader := False;
        Drop_LobbyColors[i].Add(MakeListRow([''], [$FFFFFFFF], [MakePic(rxMenu, 21)], 0));
        for K := Low(MP_TEAM_COLORS) to High(MP_TEAM_COLORS) do
          Drop_LobbyColors[i].Add(MakeListRow([''], [MP_TEAM_COLORS[K]], [MakePic(rxMenu, 20)]));
        Drop_LobbyColors[i].OnChange := Lobby_PlayersSetupChange;

        CheckBox_LobbyReady[i] := TKMCheckBox.Create(Panel_LobbyPlayers, C5-10, top, 20, 20, '', fnt_Metal);
        CheckBox_LobbyReady[I].Disable; //Read-only, just for info (perhaps we will replace it with an icon)

        Label_LobbyPing[i] := TKMLabel.Create(Panel_LobbyPlayers, C6, top, 50, 20, '', fnt_Metal, taCenter);
      end;

    //Chat
    Memo_LobbyPosts := TKMMemo.Create(Panel_Lobby, 40, 310, CW, 340, fnt_Metal);
    Memo_LobbyPosts.AutoWrap := True;
    Memo_LobbyPosts.ScrollDown := True;
    Memo_LobbyPosts.Anchors := [akLeft, akTop, akBottom];
    Label_LobbyPost := TKMLabel.Create(Panel_Lobby, 40, 655, CW, 20, fTextLibrary[TX_LOBBY_POST_WRITE], fnt_Outline, taLeft);
    Label_LobbyPost.Anchors := [akLeft, akBottom];
    Edit_LobbyPost := TKMEdit.Create(Panel_Lobby, 40, 675, CW, 20, fnt_Metal);
    Edit_LobbyPost.OnKeyDown := Lobby_PostKey;
    Edit_LobbyPost.Anchors := [akLeft, akBottom];

    //Setup
    Panel_LobbySetup := TKMPanel.Create(Panel_Lobby, 680, 30, 300, 620);
    Panel_LobbySetup.Anchors := [akLeft, akTop, akBottom];
      with TKMBevel.Create(Panel_LobbySetup,  0,  0, 300, 620) do Stretch;
      CheckBox_LobbyHostControl := TKMCheckBox.Create(Panel_LobbySetup, 10, 10, 280, 20, fTextLibrary[TX_LOBBY_HOST_DOES_SETUP], fnt_Metal);
      CheckBox_LobbyHostControl.OnClick := Lobby_PlayersSetupChange;
      Label_LobbyChooseMap := TKMLabel.Create(Panel_LobbySetup, 10, 36, 280, 20, fTextLibrary[TX_LOBBY_MAP_TYPE], fnt_Outline, taLeft);
      Radio_LobbyMapType := TKMRadioGroup.Create(Panel_LobbySetup, 10, 55, 280, 60, fnt_Metal);
      Radio_LobbyMapType.Items.Add(fTextLibrary[TX_LOBBY_MAP_BUILD]);
      Radio_LobbyMapType.Items.Add(fTextLibrary[TX_LOBBY_MAP_FIGHT]);
      Radio_LobbyMapType.Items.Add(fTextLibrary[TX_LOBBY_MAP_COOP]);
      Radio_LobbyMapType.Items.Add(fTextLibrary[TX_LOBBY_MAP_SAVED]);
      Radio_LobbyMapType.ItemIndex := 0;
      Radio_LobbyMapType.OnChange := Lobby_MapTypeSelect;

      //@DanJB: These two occupy the same place and are visible for Host/Join correspondingly, right?
      List_Lobby := TKMDropList.Create(Panel_LobbySetup, 10, 125, 280, 20, fnt_Metal, fTextLibrary[TX_LOBBY_MAP_SELECT]);
      List_Lobby.OnChange := Lobby_MapSelect;
      Label_LobbyMapName := TKMLabel.Create(Panel_LobbySetup, 10, 125, 280, 20, '', fnt_Metal, taLeft);

      TKMBevel.Create(Panel_LobbySetup, 50, 150, 199, 199);
      Minimap_LobbyPreview := TKMMinimap.Create(Panel_LobbySetup, 54, 154, 191, 191);
      Minimap_LobbyPreview.ShowLocs := True; //In the minimap we want player locations to be shown

      Memo_LobbyMapDesc := TKMMemo.Create(Panel_LobbySetup, 10, 350, 280, 100, fnt_Game);
      Memo_LobbyMapDesc.AutoWrap := True;
      Memo_LobbyMapDesc.ItemHeight := 16;

      Label_LobbyMapMode := TKMLabel.Create(Panel_LobbySetup, 10, 460, 280, 20, '', fnt_Metal, taLeft);
      Label_LobbyMapSize := TKMLabel.Create(Panel_LobbySetup, 10, 480, 280, 20, '', fnt_Metal, taLeft);
      Label_LobbyMapCond := TKMLabel.Create(Panel_LobbySetup, 10, 500, 280, 20, '', fnt_Metal, taLeft);

      TKMLabel.Create(Panel_LobbySetup, 10, 530, 280, 20, fTextLibrary[TX_LOBBY_GAME_OPTIONS], fnt_Outline, taLeft);
      TrackBar_LobbyPeacetime := TKMTrackBar.Create(Panel_LobbySetup, 10, 560, 280, 0, 120);
      TrackBar_LobbyPeacetime.Caption := fTextLibrary[TX_LOBBY_PEACETIME];
      TrackBar_LobbyPeacetime.Step := 5; //Round to 5min steps
      TrackBar_LobbyPeacetime.OnChange := Lobby_GameOptionsChange;

    Button_LobbyBack := TKMButton.Create(Panel_Lobby, 40, 712, 230, 30, fTextLibrary[TX_LOBBY_QUIT], fnt_Metal, bsMenu);
    Button_LobbyBack.Anchors := [akLeft, akBottom];
    Button_LobbyBack.OnClick := Lobby_BackClick;
    Button_LobbyStart := TKMButton.Create(Panel_Lobby, 285, 712, 230, 30, '<<<LEER>>>', fnt_Metal, bsMenu);
    Button_LobbyStart.Anchors := [akLeft, akBottom];
    Button_LobbyStart.OnClick := Lobby_StartClick;
end;


procedure TKMMainMenuInterface.Create_Campaign_Page;
var I: Integer;
begin
  Panel_Campaign:=TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
    Image_CampaignBG := TKMImage.Create(Panel_Campaign,0,0,Panel_Main.Width, Panel_Main.Height,0,rxGuiMain);
    Image_CampaignBG.ImageStretch;

    for I := 0 to High(Image_CampaignFlags) do
    begin
      Image_CampaignFlags[I] := TKMImage.Create(Panel_Campaign, Panel_Main.Width, Panel_Main.Height, 23, 29, 10, rxGuiMain);
      Image_CampaignFlags[I].OnClick := Campaign_SelectMap;
      Image_CampaignFlags[I].Tag := I;
    end;
    for I := 0 to High(Image_CampaignSubNode) do
      Image_CampaignSubNode[I] := TKMImage.Create(Panel_Campaign, Panel_Main.Width, Panel_Main.Height, 0, 0, 16, rxGuiMain);

  Panel_CampScroll := TKMPanel.Create(Panel_Campaign,Panel_Main.Width-360,Panel_Main.Height-430,360,430);

    Image_Scroll := TKMImage.Create(Panel_CampScroll, 0, 0,360,430,{15}2,rxGuiMainH);
    Image_Scroll.ImageStretch;
    Label_CampaignTitle := TKMLabel.Create(Panel_CampScroll, 180, 18,100,20, '', fnt_Outline, taCenter);

    Label_CampaignText := TKMLabel.Create(Panel_CampScroll, 20, 50, 325, 310, '', fnt_Briefing, taLeft);
    Label_CampaignText.AutoWrap := true;

  Button_CampaignStart := TKMButton.Create(Panel_Campaign, Panel_Main.Width-220-20, Panel_Main.Height-50, 220, 30, fTextLibrary[TX_MENU_START_MISSION], fnt_Metal, bsMenu);
  Button_CampaignStart.OnClick := Campaign_StartMap;

  Button_CampaignBack := TKMButton.Create(Panel_Campaign, 20, Panel_Main.Height-50, 220, 30, fTextLibrary[TX_MENU_BACK], fnt_Metal, bsMenu);
  Button_CampaignBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Single_Page;
var i:integer;
begin
  Panel_Single:=TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
    Panel_SingleList:=TKMPanel.Create(Panel_Single,512+22,84,445,600);

      ScrollBar_SingleMaps := TKMScrollBar.Create(Panel_SingleList,420,40,25,MENU_SP_MAPS_COUNT*40, sa_Vertical, bsMenu);
      ScrollBar_SingleMaps.OnChange := SingleMap_ScrollChange;

      Button_SingleHeadMode  := TKMButton.Create(Panel_SingleList,  0,0, 40,40,42,rxGui,bsMenu);
      Button_SingleHeadMode.OnClick := SingleMap_Sort;
      Button_SingleHeadTeams := TKMButton.Create(Panel_SingleList, 40,0, 40,40,31,rxGui,bsMenu);
      Button_SingleHeadTeams.OnClick := SingleMap_Sort;
      Button_SingleHeadTitle := TKMButton.Create(Panel_SingleList, 80,0,300,40,fTextLibrary[TX_MENU_TITLE],fnt_Metal,bsMenu);
      Button_SingleHeadTitle.OnClick := SingleMap_Sort;
      Button_SingleHeadSize  := TKMButton.Create(Panel_SingleList,380,0, 40,40,fTextLibrary[TX_MENU_SIZE],fnt_Metal,bsMenu);
      Button_SingleHeadSize.OnClick := SingleMap_Sort;
      with TKMButton.Create(Panel_SingleList,420,0, 25,40,'',fnt_Metal,bsMenu) do Disable;

      SetLength(Bevel_SingleBG, MENU_SP_MAPS_COUNT);
      SetLength(Image_SingleMode, MENU_SP_MAPS_COUNT);
      SetLength(Label_SinglePlayers, MENU_SP_MAPS_COUNT);
      SetLength(Label_SingleTitle1, MENU_SP_MAPS_COUNT);
      SetLength(Label_SingleTitle2, MENU_SP_MAPS_COUNT);
      SetLength(Label_SingleSize, MENU_SP_MAPS_COUNT);
      SetLength(Shape_SingleOverlay, MENU_SP_MAPS_COUNT);

      for i := 0 to MENU_SP_MAPS_COUNT - 1 do
      begin
        Bevel_SingleBG[i,1] := TKMBevel.Create(Panel_SingleList,0,  40+i*40, 40,40);
        Bevel_SingleBG[i,2] := TKMBevel.Create(Panel_SingleList,40, 40+i*40, 40,40);
        Bevel_SingleBG[i,3] := TKMBevel.Create(Panel_SingleList,80, 40+i*40,300,40);
        Bevel_SingleBG[i,4] := TKMBevel.Create(Panel_SingleList,380,40+i*40, 40,40);

        Image_SingleMode[i]    := TKMImage.Create(Panel_SingleList,  0   ,40+i*40,40,40,28);
        Image_SingleMode[i].ImageCenter;
        Label_SinglePlayers[i] := TKMLabel.Create(Panel_SingleList, 40+20, 40+i*40+14, 40,20,'0',fnt_Metal, taCenter);
        Label_SingleTitle1[i]  := TKMLabel.Create(Panel_SingleList, 80+ 6, 40+i*40+ 5, 288,0,'<<<LEER>>>',fnt_Metal, taLeft);
        Label_SingleTitle2[i]  := TKMLabel.Create(Panel_SingleList, 80+ 6, 40+i*40+22, 288,0,'<<<LEER>>>',fnt_Game, taLeft);
        Label_SingleTitle2[i].FontColor := $FFD0D0D0; //Grey for minor description
        Label_SingleSize[i]    := TKMLabel.Create(Panel_SingleList,380+20, 40+i*40+14, 40,20,'0',fnt_Metal, taCenter);

        Shape_SingleOverlay[i] := TKMShape.Create(Panel_SingleList, 0, 40+i*40, 420, 40, $00000000);
        Shape_SingleOverlay[i].LineWidth := 0;
        Shape_SingleOverlay[i].Tag := i;
        Shape_SingleOverlay[i].OnClick := SingleMap_MapClick;
        Shape_SingleOverlay[i].OnDoubleClick := SingleMap_Start;
        Shape_SingleOverlay[i].OnMouseWheel := ScrollBar_SingleMaps.MouseWheel;
      end;

      Shape_SingleMap:=TKMShape.Create(Panel_SingleList,0,40,420,40, $FFFFFF00);
      Shape_SingleMap.Hitable := false; //All hits should go to Shape_SingleOverlay[i] not this

    Panel_SingleDesc:=TKMPanel.Create(Panel_Single,45,84,445,600);

      Label_SingleTitle := TKMLabel.Create(Panel_SingleDesc,445 div 2,0,433,20,'',fnt_Outline, taCenter);
      Memo_SingleDesc  := TKMMemo.Create(Panel_SingleDesc,15,25,415,189,fnt_Metal);
      Memo_SingleDesc.AutoWrap := True;

      TKMBevel.Create(Panel_SingleDesc, 121, 220, 199, 199);
      Minimap_SinglePreview := TKMMinimap.Create(Panel_SingleDesc, 125, 224, 191, 191);

      TKMBevel.Create(Panel_SingleDesc,0,428,445,20);
      Label_SingleCondTyp:=TKMLabel.Create(Panel_SingleDesc,8,431,429,20,fTextLibrary[TX_MENU_MISSION_TYPE],fnt_Metal, taLeft);
      TKMBevel.Create(Panel_SingleDesc,0,450,445,20);
      Label_SingleCondWin:=TKMLabel.Create(Panel_SingleDesc,8,453,429,20,fTextLibrary[TX_MENU_WIN_CONDITION],fnt_Metal, taLeft);
      TKMBevel.Create(Panel_SingleDesc,0,472,445,20);
      Label_SingleCondDef:=TKMLabel.Create(Panel_SingleDesc,8,475,429,20,fTextLibrary[TX_MENU_DEFEAT_CONDITION],fnt_Metal, taLeft);
      TKMBevel.Create(Panel_SingleDesc,0,494,445,20);
      Label_SingleAllies:=TKMLabel.Create(Panel_SingleDesc,8,497,429,20,fTextLibrary[TX_MENU_ALLIES],fnt_Metal, taLeft);
      TKMBevel.Create(Panel_SingleDesc,0,516,445,20);
      Label_SingleEnemies:=TKMLabel.Create(Panel_SingleDesc,8,519,429,20,fTextLibrary[TX_MENU_ENEMIES],fnt_Metal, taLeft);

    Button_SingleBack := TKMButton.Create(Panel_Single, 45, 650, 220, 30, fTextLibrary[TX_MENU_BACK], fnt_Metal, bsMenu);
    Button_SingleBack.OnClick := SwitchMenuPage;
    Button_SingleStart := TKMButton.Create(Panel_Single, 270, 650, 220, 30, fTextLibrary[TX_MENU_SINGLE_START_MAP], fnt_Metal, bsMenu);
    Button_SingleStart.OnClick := SingleMap_Start;
end;


procedure TKMMainMenuInterface.Create_Load_Page;
begin
  Panel_Load:=TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);

    TKMLabel.Create(Panel_Load, Panel_Main.Width div 2, 60, 700, 20, fTextLibrary[TX_MENU_LOAD_LIST], fnt_Metal, taCenter);

    List_Load := TKMColumnListBox.Create(Panel_Load, 62, 85, 700, 468, fnt_Metal);
    List_Load.SetColumns(fnt_Outline, [fTextLibrary[TX_MENU_LOAD_FILE], fTextLibrary[TX_MENU_LOAD_DESCRIPTION]], [0, 300]);
    List_Load.OnColumnClick := Load_SavesSort;
    List_Load.OnChange := Load_ListClick;
    List_Load.OnDoubleClick := Load_Click;

    Button_Load := TKMButton.Create(Panel_Load,337,560,350,30,fTextLibrary[TX_MENU_LOAD_LOAD],fnt_Metal, bsMenu);
    Button_Load.OnClick := Load_Click;

    Button_Delete := TKMButton.Create(Panel_Load, 337, 594, 350, 30, fTextLibrary[TX_MENU_LOAD_DELETE], fnt_Metal, bsMenu);
    Button_Delete.OnClick := Load_Delete_Click;

    Label_DeleteConfirm := TKMLabel.Create(Panel_Load, Panel_Main.Width div 2, 604, 550, 30, fTextLibrary[TX_MENU_LOAD_DELETE_CONFIRM], fnt_Outline, taCenter);
    Button_DeleteYes := TKMButton.Create(Panel_Load, 337, 630, 170, 30, fTextLibrary[TX_MENU_LOAD_DELETE_DELETE], fnt_Metal, bsMenu);
    Button_DeleteYes.OnClick := Load_Delete_Click;
    Button_DeleteNo  := TKMButton.Create(Panel_Load, 517, 630, 170, 30, fTextLibrary[TX_MENU_LOAD_DELETE_CANCEL], fnt_Metal, bsMenu);
    Button_DeleteNo.OnClick := Load_Delete_Click;

    Button_LoadBack := TKMButton.Create(Panel_Load, 337, 670, 350, 30, fTextLibrary[TX_MENU_BACK], fnt_Metal, bsMenu);
    Button_LoadBack.OnClick := SwitchMenuPage;

    TKMBevel.Create(Panel_Load, 785, 220, 199, 199);
    Minimap_LoadPreview := TKMMinimap.Create(Panel_Load,789,224,191,191);
end;


//Should contain options to make a map from scratch, load map from file, generate new one
procedure TKMMainMenuInterface.Create_MapEditor_Page;
var i:integer;
begin
  Panel_MapEd:=TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
    Panel_MapEd_SizeXY := TKMPanel.Create(Panel_MapEd, 120, 200, 200, 400);
      TKMLabel.Create(Panel_MapEd_SizeXY, 6, 0, 188, 20, fTextLibrary[TX_MENU_MAP_SIZE], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_MapEd_SizeXY, 0, 20, 200, 20 + MAPSIZES_COUNT*26);
      TKMLabel.Create(Panel_MapEd_SizeXY, 8, 27, 88, 20, fTextLibrary[TX_MENU_MAP_WIDTH], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_MapEd_SizeXY, 108, 27, 88, 20, fTextLibrary[TX_MENU_MAP_HEIGHT], fnt_Outline, taLeft);

      Radio_MapEd_SizeX := TKMRadioGroup.Create(Panel_MapEd_SizeXY, 8, 52, 88, 260, fnt_Metal);
      Radio_MapEd_SizeY := TKMRadioGroup.Create(Panel_MapEd_SizeXY, 108, 52, 88, 260, fnt_Metal);
      Radio_MapEd_SizeX.ItemIndex := 2; //64
      Radio_MapEd_SizeY.ItemIndex := 2; //64
      Radio_MapEd_SizeX.OnChange := MapEditor_SizeChange;
      Radio_MapEd_SizeY.OnChange := MapEditor_SizeChange;

      for i:=1 to MAPSIZES_COUNT do begin
        Radio_MapEd_SizeX.Items.Add(inttostr(MapSize[i]));
        Radio_MapEd_SizeY.Items.Add(inttostr(MapSize[i]));
      end;

      Button_MapEd_Create := TKMButton.Create(Panel_MapEd_SizeXY, 0, 335, 200, 30, fTextLibrary[TX_MENU_MAP_CREATE_NEW_MAP], fnt_Metal, bsMenu);
      Button_MapEd_Create.OnClick := MapEditor_Start;

    Panel_MapEd_Load := TKMPanel.Create(Panel_MapEd, 340, 200, 520, 400);
      TKMLabel.Create(Panel_MapEd_Load, 6, 0, 288, 20, fTextLibrary[TX_MENU_MAP_AVAILABLE], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_MapEd_Load, 0, 20, 300, 50);
      Radio_MapEd_MapType := TKMRadioGroup.Create(Panel_MapEd_Load,8,28,286,40,fnt_Grey);
      Radio_MapEd_MapType.ItemIndex := 0;
      Radio_MapEd_MapType.Items.Add(fTextLibrary[TX_MENU_MAPED_SPMAPS]);
      Radio_MapEd_MapType.Items.Add(fTextLibrary[TX_MENU_MAPED_MPMAPS]);
      Radio_MapEd_MapType.OnChange := MapEditor_MapTypeChange;
      List_MapEd := TKMListBox.Create(Panel_MapEd_Load, 0, 80, 300, 240, fnt_Metal);
      List_MapEd.OnChange := MapEditor_SelectMap;
      List_MapEd.OnDoubleClick := MapEditor_Start;
      Button_MapEd_Load := TKMButton.Create(Panel_MapEd_Load, 0, 335, 300, 30, fTextLibrary[TX_MENU_MAP_LOAD_EXISTING], fnt_Metal, bsMenu);
      Button_MapEd_Load.OnClick := MapEditor_Start;
      TKMBevel.Create(Panel_MapEd_Load, 308, 80, 199, 199);
      Minimap_MapEd := TKMMinimap.Create(Panel_MapEd_Load, 312, 84, 191, 191);

    Button_MapEdBack := TKMButton.Create(Panel_MapEd, 120, 650, 220, 30, fTextLibrary[TX_MENU_BACK], fnt_Metal, bsMenu);
    Button_MapEdBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Replays_Page;
begin
  Panel_Replays := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);

    TKMLabel.Create(Panel_Replays, Panel_Main.Width div 2, 60, 900, 20, fTextLibrary[TX_MENU_LOAD_LIST], fnt_Outline, taCenter);

    TKMBevel.Create(Panel_Replays, 62, 100, 900, 50);
    Radio_Replays_Type := TKMRadioGroup.Create(Panel_Replays,70,108,300,40,fnt_Grey);
    Radio_Replays_Type.ItemIndex := 0;
    Radio_Replays_Type.Items.Add(fTextLibrary[TX_MENU_MAPED_SPMAPS]);
    Radio_Replays_Type.Items.Add(fTextLibrary[TX_MENU_MAPED_MPMAPS]);
    Radio_Replays_Type.OnChange := Replay_TypeChange;

    List_Replays := TKMColumnListBox.Create(Panel_Replays, 62, 200, 700, 400, fnt_Metal);
    List_Replays.SetColumns(fnt_Outline, [fTextLibrary[TX_MENU_LOAD_FILE], fTextLibrary[TX_MENU_LOAD_DESCRIPTION]], [0, 300]);
    List_Replays.OnChange := Replays_ListClick;
    List_Replays.OnColumnClick := Replays_SavesSort;
    List_Replays.OnDoubleClick := Replays_Play;

    Button_ReplaysPlay := TKMButton.Create(Panel_Replays,337,630,350,30,fTextLibrary[TX_MENU_VIEW_REPLAY],fnt_Metal, bsMenu);
    Button_ReplaysPlay.OnClick := Replays_Play;

    Button_ReplaysBack := TKMButton.Create(Panel_Replays, 337, 670, 350, 30, fTextLibrary[TX_MENU_BACK], fnt_Metal, bsMenu);
    Button_ReplaysBack.OnClick := SwitchMenuPage;

    TKMBevel.Create(Panel_Replays, 785, 300, 199, 199);
    Minimap_ReplayPreview := TKMMinimap.Create(Panel_Replays,789,304,191,191);
end;


procedure TKMMainMenuInterface.Create_Options_Page;
var I: Integer;
begin
  Panel_Options:=TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
    with TKMImage.Create(Panel_Options,705,220,round(207*1.3),round(295*1.3),6,rxGuiMainH) do ImageStretch;

    //Controls section
    Panel_Options_Ctrl:=TKMPanel.Create(Panel_Options,120,130,220,80);
      TKMLabel.Create(Panel_Options_Ctrl,6,0,288,20,fTextLibrary[TX_MENU_OPTIONS_CONTROLS],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Ctrl,0,20,220,60);

      TrackBar_Options_ScrollSpeed := TKMTrackBar.Create(Panel_Options_Ctrl,10,27,180,OPT_SLIDER_MIN,OPT_SLIDER_MAX);
      TrackBar_Options_ScrollSpeed.Caption := fTextLibrary[TX_MENU_OPTIONS_SCROLL_SPEED];
      TrackBar_Options_ScrollSpeed.OnChange := Options_Change;

    //Gameplay section
    Panel_Options_Game:=TKMPanel.Create(Panel_Options,120,230,220,50);
      TKMLabel.Create(Panel_Options_Game,6,0,188,20,fTextLibrary[TX_MENU_OPTIONS_GAMEPLAY],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Game,0,20,220,30);

      CheckBox_Options_Autosave := TKMCheckBox.Create(Panel_Options_Game,12,27,176,20,fTextLibrary[TX_MENU_OPTIONS_AUTOSAVE], fnt_Metal);
      CheckBox_Options_Autosave.OnClick := Options_Change;

    //Graphics section
    Panel_Options_GFX:=TKMPanel.Create(Panel_Options,120,300,220,100);
      TKMLabel.Create(Panel_Options_GFX,6,0,188,20,fTextLibrary[TX_MENU_OPTIONS_GRAPHICS],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_GFX,0,20,220,80);
      TrackBar_Options_Brightness:=TKMTrackBar.Create(Panel_Options_GFX,10,27,180,OPT_SLIDER_MIN,OPT_SLIDER_MAX);
      TrackBar_Options_Brightness.Caption := fTextLibrary[TX_MENU_OPTIONS_BRIGHTNESS];
      TrackBar_Options_Brightness.OnChange:=Options_Change;
      CheckBox_Options_Shadows := TKMCheckBox.Create(Panel_Options_GFX,10,74,180,20,fTextLibrary[TX_MENU_OPTIONS_SOFT_SHADOWS], fnt_Metal);
      CheckBox_Options_Shadows.OnClick := Options_Change;

    //SFX section
    Panel_Options_Sound:=TKMPanel.Create(Panel_Options,120,420,220,167);
      TKMLabel.Create(Panel_Options_Sound,6,0,188,20,fTextLibrary[TX_MENU_OPTIONS_SOUND],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Sound,0,20,220,147);

      TrackBar_Options_SFX       := TKMTrackBar.Create(Panel_Options_Sound, 10, 27, 180, OPT_SLIDER_MIN, OPT_SLIDER_MAX);
      TrackBar_Options_Music     := TKMTrackBar.Create(Panel_Options_Sound, 10, 77, 180, OPT_SLIDER_MIN, OPT_SLIDER_MAX);
      CheckBox_Options_MusicOff  := TKMCheckBox.Create(Panel_Options_Sound, 12, 127, 176, 20, fTextLibrary[TX_MENU_OPTIONS_MUSIC_DISABLE], fnt_Metal);
      CheckBox_Options_ShuffleOn := TKMCheckBox.Create(Panel_Options_Sound, 12, 147, 184, 20, fTextLibrary[TX_MENU_OPTIONS_MUSIC_SHUFFLE], fnt_Metal);
      TrackBar_Options_SFX.Caption   := fTextLibrary[TX_MENU_SFX_VOLUME];
      TrackBar_Options_Music.Caption := fTextLibrary[TX_MENU_MUSIC_VOLUME];
      TrackBar_Options_SFX.OnChange      := Options_Change;
      TrackBar_Options_Music.OnChange    := Options_Change;
      CheckBox_Options_MusicOff.OnClick  := Options_Change;
      CheckBox_Options_ShuffleOn.OnClick := Options_Change;

    //Resolutions section
    Panel_Options_Res := TKMPanel.Create(Panel_Options, 360, 130, 210, 160);
      TKMLabel.Create(Panel_Options_Res, 6, 0, 188, 20, fTextLibrary[TX_MENU_OPTIONS_RESOLUTION], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_Options_Res, 0, 20, 220, 140);

      CheckBox_Options_FullScreen := TKMCheckBox.Create(Panel_Options_Res, 12, 30, 176, 20, fTextLibrary[TX_MENU_OPTIONS_FULLSCREEN], fnt_Metal);
      CheckBox_Options_FullScreen.OnClick := Options_ChangeRes;

      DropBox_Options_Resolution := TKMDropList.Create(Panel_Options_Res, 10, 50, 180, 20, fnt_Metal, '');
      DropBox_Options_Resolution.OnChange := Options_ChangeRes;

      DropBox_Options_RefreshRate := TKMDropList.Create(Panel_Options_Res, 10, 85, 180, 20, fnt_Metal, '');
      DropBox_Options_RefreshRate.OnChange := Options_ChangeRes;

      Button_Options_ResApply := TKMButton.Create(Panel_Options_Res, 10, 120, 180, 30, fTextLibrary[TX_MENU_OPTIONS_APPLY], fnt_Metal, bsMenu);
      Button_Options_ResApply.OnClick := Options_ApplyRes;

    //Language section
    Panel_Options_Lang:=TKMPanel.Create(Panel_Options,600,130,240,30+fLocales.Count*20);
      TKMLabel.Create(Panel_Options_Lang,6,0,242,20,fTextLibrary[TX_MENU_OPTIONS_LANGUAGE],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Lang,0,20,260,10+fLocales.Count*20);

      Radio_Options_Lang := TKMRadioGroup.Create(Panel_Options_Lang, 28, 27, 220, 20*fLocales.Count, fnt_Metal);
      SetLength(Image_Options_Lang_Flags,fLocales.Count);
      for i := 0 to fLocales.Count - 1 do
      begin
        Radio_Options_Lang.Items.Add(fLocales[i].Title);
        Image_Options_Lang_Flags[i] := TKMImage.Create(Panel_Options_Lang,6,28+(i*20),16,11,fLocales[i].FlagSpriteID,rxMenu);
        Image_Options_Lang_Flags[i].Tag := i;
        Image_Options_Lang_Flags[i].OnClick := Options_FlagClick;
      end;
      Radio_Options_Lang.OnChange := Options_Change;

    //Back button
    Button_Options_Back:=TKMButton.Create(Panel_Options,120,650,220,30,fTextLibrary[TX_MENU_BACK],fnt_Metal,bsMenu);
    Button_Options_Back.OnClick:=SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Credits_Page;
const OFFSET = 312;
begin
  Panel_Credits:=TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);

    TKMLabel.Create(Panel_Credits, Panel_Main.Width div 2 - OFFSET,100,400,20,fTextLibrary[TX_CREDITS],fnt_Outline,taCenter);
    Label_Credits_Remake := TKMLabelScroll.Create(Panel_Credits, Panel_Main.Width div 2 - OFFSET, 140, 400, Panel_Main.Height - 160,
    fTextLibrary[TX_CREDITS_PROGRAMMING]+'|Krom|Lewin||'+
    fTextLibrary[TX_CREDITS_ADDITIONAL_PROGRAMMING]+'|Alex|Danjb||'+
    fTextLibrary[TX_CREDITS_ADDITIONAL_GRAPHICS]+'|StarGazer|Malin||'+
    fTextLibrary[TX_CREDITS_ADDITIONAL_SOUNDS]+'|trb1914||'+
    fTextLibrary[TX_CREDITS_ADDITIONAL_TRANSLATIONS]+'|'+fLocales.GetTranslatorCredits+'|'+
    fTextLibrary[TX_CREDITS_SPECIAL]+'|KaM Community members'
    ,fnt_Grey,taCenter);

    TKMLabel.Create(Panel_Credits, Panel_Main.Width div 2 + OFFSET, 100, 400, 20, fTextLibrary[TX_CREDITS_ORIGINAL], fnt_Outline, taCenter);
    Label_Credits_KaM := TKMLabelScroll.Create(Panel_Credits, Panel_Main.Width div 2 + OFFSET, 140, 400, Panel_Main.Height - 160, fTextLibrary[TX_CREDITS_TEXT], fnt_Grey, taCenter);

    Button_CreditsBack:=TKMButton.Create(Panel_Credits,400,700,224,30,fTextLibrary[TX_MENU_BACK],fnt_Metal,bsMenu);
    Button_CreditsBack.OnClick:=SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Loading_Page;
begin
  Panel_Loading:=TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
    TKMLabel.Create(Panel_Loading, Panel_Main.Width div 2, Panel_Main.Height div 2 - 20, 0,0,fTextLibrary[TX_MENU_LOADING],fnt_Outline,taCenter);
    Label_Loading := TKMLabel.Create(Panel_Loading, Panel_Main.Width div 2, Panel_Main.Height div 2+10, 0, 0,'...',fnt_Grey,taCenter);
end;


procedure TKMMainMenuInterface.Create_Error_Page;
begin
  Panel_Error := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
    TKMLabel.Create(Panel_Error, Panel_Main.Width div 2,Panel_Main.Height div 2 - 20, 0, 30, fTextLibrary[TX_MENU_ERROR], fnt_Antiqua, taCenter);
    Label_Error := TKMLabel.Create(Panel_Error, Panel_Main.Width div 2, Panel_Main.Height div 2+10, Panel_Main.Width-16, 200, '...', fnt_Grey, taCenter);
    Label_Error.AutoWrap := True;
    Button_ErrorBack := TKMButton.Create(Panel_Error,100,640,224,30,fTextLibrary[TX_MENU_BACK],fnt_Metal,bsMenu);
    Button_ErrorBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Results_Page;
const StatText: array [1..9] of Word = (
    TX_RESULTS_UNITS_LOST,      TX_RESULTS_UNITS_DEFEATED,  TX_RESULTS_HOUSES_LOST,
    TX_RESULTS_HOUSES_DESTROYED,TX_RESULTS_HOUSES_BUILT,    TX_RESULTS_UNITS_TRAINED,
    TX_RESULTS_WEAPONS_MADE,    TX_RESULTS_SOLDIERS_TRAINED,TX_RESULTS_MISSION_TIME);
var i, Adv: Integer;
begin
  Panel_Results := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
    with TKMImage.Create(Panel_Results,0,0,Panel_Main.Width, Panel_Main.Height,7,rxGuiMain) do ImageStretch;

    Label_Results := TKMLabel.Create(Panel_Results,512,200,300,20,'<<<LEER>>>',fnt_Metal,taCenter);

    Panel_Stats := TKMPanel.Create(Panel_Results,80,240,400,400);
    Adv := 0;
    for i:=1 to 9 do
    begin
      inc(Adv, 25);
      if i in [3,6,7,9] then inc(Adv, 15);
      TKMLabel.Create(Panel_Stats,0,Adv,232,20,fTextLibrary[StatText[i]],fnt_Metal,taLeft);
      Label_Stat[i] := TKMLabel.Create(Panel_Stats,340,Adv,100,20,'00',fnt_Metal,taRight);
    end;

    Button_ResultsBack := TKMButton.Create(Panel_Results,100,650,220,30,fTextLibrary[TX_MENU_BACK],fnt_Metal,bsMenu);
    Button_ResultsBack.OnClick := SwitchMenuPage;
    Button_ResultsRepeat := TKMButton.Create(Panel_Results,340,650,220,30,fTextLibrary[TX_MENU_MISSION_REPEAT],fnt_Metal,bsMenu);
    Button_ResultsRepeat.OnClick := MainMenu_ReplayLastMap;
    Button_ResultsContinue := TKMButton.Create(Panel_Results,580,650,220,30,fTextLibrary[TX_MENU_MISSION_NEXT],fnt_Metal,bsMenu);
    Button_ResultsContinue.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_ResultsMP_Page;
const
  BarStep = 130;
  BarWidth = BarStep - 10;
  BarHalf = BarWidth div 2;
  Columns1: array[0..4] of integer = (TX_RESULTS_MP_CITIZENS_TRAINED, TX_RESULTS_MP_CITIZENS_LOST,
                                     TX_RESULTS_MP_SOLDIERS_EQUIPPED, TX_RESULTS_MP_SOLDIERS_LOST,
                                     TX_RESULTS_MP_UNITS_DEFEATED);
  Columns2: array[0..4] of integer = (TX_RESULTS_MP_BUILDINGS_CONSTRUCTED, TX_RESULTS_MP_BUILDINGS_LOST,
                                     TX_RESULTS_MP_BUILDINGS_DESTROYED,
                                     TX_RESULTS_MP_WARES_PRODUCED, TX_RESULTS_MP_WEAPONS_PRODUCED);
var i,k: Integer;
begin
  Panel_ResultsMP := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
    with TKMImage.Create(Panel_ResultsMP,0,0,Panel_Main.Width, Panel_Main.Height,7,rxGuiMain) do ImageStretch;

    Label_ResultsMP := TKMLabel.Create(Panel_ResultsMP,512,90,300,20,'<<<LEER>>>',fnt_Metal,taCenter);
    Label_ResultsMPTime := TKMLabel.Create(Panel_ResultsMP,512,120,300,20,'<<<LEER>>>',fnt_Metal,taCenter);

    Panel_StatsMP1 := TKMPanel.Create(Panel_ResultsMP, 112, 140, 800, 260);

      for i:=0 to 7 do
        Label_ResultsPlayerName1[i] := TKMLabel.Create(Panel_StatsMP1, 0, 43+i*25, 140, 20, '', fnt_Metal, taLeft);

      for k:=0 to 4 do
      begin
        with TKMLabel.Create(Panel_StatsMP1, 150 + BarHalf+BarStep*k, 0, BarWidth+6, 40, fTextLibrary[Columns1[k]], fnt_Metal, taCenter) do
          AutoWrap := true;
        for i:=0 to 7 do
          Bar_Results[i,k] := TKMPercentBar.Create(Panel_StatsMP1, 150 + k*BarStep, 40+i*25, BarWidth, 20, fnt_Metal);
      end;

    Panel_StatsMP2 := TKMPanel.Create(Panel_ResultsMP, 112, 400, 800, 260);

      for i:=0 to 7 do
        Label_ResultsPlayerName2[i] := TKMLabel.Create(Panel_StatsMP2, 0, 43+i*25, 140, 20, '', fnt_Metal, taLeft);

      for k:=0 to 4 do
      begin
        with TKMLabel.Create(Panel_StatsMP2, 150 + BarHalf+BarStep*k, 0, BarWidth+6, 40, fTextLibrary[Columns2[k]], fnt_Metal, taCenter) do
          AutoWrap := true;
        for i:=0 to 7 do
          Bar_Results[i,k+5] := TKMPercentBar.Create(Panel_StatsMP2, 150 + k*BarStep, 40+i*25, BarWidth, 20, fnt_Metal);
      end;

    Button_ResultsMPBack := TKMButton.Create(Panel_ResultsMP,100,650,220,30,fTextLibrary[TX_MENU_BACK],fnt_Metal,bsMenu);
    Button_ResultsMPBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.SwitchMenuPage(Sender: TObject);
var i:integer;
begin
  if fGame <> nil then
    Label_Version.Caption := GAME_VERSION + ' / ' + fGame.RenderVersion;

  //First thing - hide all existing pages
  for i:=1 to Panel_Main.ChildCount do
    if Panel_Main.Childs[i] is TKMPanel then
      Panel_Main.Childs[i].Hide;

  {Return to MainMenu}
  if (Sender = nil) or
     (Sender = Button_SP_Back) or
     (Sender = Button_MP_Back) or
     (Sender = Button_CreditsBack) or
     (Sender = Button_MapEdBack) or
     (Sender = Button_ErrorBack) or
     (Sender = Button_ResultsBack) or
     (Sender = Button_ReplaysBack) then begin
       if Sender = Button_ReplaysBack then
         //scan should be terminated, it is no longer needed
         fSaves.TerminateScan;
       Panel_MainMenu.Show;
     end;

  {Player leaves lobby (LAN text is updated)}
  if Sender=Button_LobbyBack then
  begin
    Panel_MultiPlayer.Show;
    MP_ServersRefresh(Sender);
  end;

  {Return to MainMenu and restore resolution changes}
  if Sender=Button_Options_Back then
  begin
    fMain.Settings.SaveSettings;
    Panel_MainMenu.Show;
  end;

  {Show SinglePlayer menu}
  {Return to SinglePlayerMenu}
  if (Sender=Button_MM_SinglePlayer)or
     (Sender=Button_CampaignBack)or
     (Sender=Button_SingleBack)or
     (Sender=Button_LoadBack) then begin
       if (Sender=Button_SingleBack) then
         //scan should be terminated, it is no longer needed
         fMaps.TerminateScan;
       if (Sender=Button_LoadBack) then
         //scan should be terminated, it is no longer needed
         fSaves.TerminateScan;
       Panel_SinglePlayer.Show;
  end;

  {Show campaign menu}
  if (Sender = Button_SP_TSK)
  or (Sender = Button_SP_TPR)
  or (Sender = Button_ResultsContinue) then
  begin
    if (Sender = Button_SP_TPR) then
      Campaign_Set(fGame.Campaigns.CampaignByTitle('TPR'))
    else
    if (Sender = Button_SP_TSK) then
      Campaign_Set(fGame.Campaigns.CampaignByTitle('TSK'))
    else
      Campaign_Set(fGame.Campaigns.ActiveCampaign);
    Panel_Campaign.Show;
  end;

  {Show SingleMap menu}
  if Sender=Button_SP_Single then
  begin
    //Stop current now scan so it can't add a map after we clear the list
    fMaps.TerminateScan;
    //Remove any old entries from UI
    SingleMap_Clear;
    //Initiate refresh and process each new map added
    fMaps.Refresh(SingleMap_RefreshList);
    Panel_Single.Show;
  end;

  {Show Load menu}
  if Sender=Button_SP_Load then begin

    //Stop current scan so it can't add a save after we clear the list
    fSaves.TerminateScan;
    fSave_Selected := -1;
    fSaveCRC_Selected := 0;
    Load_RefreshList(nil); //call it at least one time, so GUI is prepared even if there are no saves
    //Initiate refresh and process each new save added
    fSaves.Refresh(Load_RefreshList, False);
    Panel_Load.Show;
  end;

  {Show replays menu}
  if Sender=Button_MM_Replays then begin

    Radio_Replays_Type.ItemIndex := 0; //we always show SP replays on start
    //Stop current scan so it can't add a replay after we clear the list
    fSaves.TerminateScan;
    fSave_Selected := -1;
    fSaveCRC_Selected := 0;
    Replays_RefreshList(nil); //call it at least one time, so GUI is prepared even if there are no replays
    //Initiate refresh and process each new replay added
    fSaves.Refresh(Replays_RefreshList, False);
    Panel_Replays.Show;
  end;

  {Show MultiPlayer menu}
  if (Sender=Button_MM_MultiPlayer) or (Sender=Button_ResultsMPBack) then
  begin
    fGame.NetworkInit;
    MP_Init(Sender);
    MP_Update(fTextLibrary[TX_MP_MENU_STATUS_READY],$FF00FF00,false);
    Panel_MultiPlayer.Show;
  end;

  { Lobby }
  if (Sender=Button_MP_FindServerIP)
  or (Sender=Button_MP_GetIn)
  or (Sender=Button_MP_CreateLAN)
  or (Sender=Button_MP_CreateWAN) then
  begin
    Lobby_Reset(Sender);
    fMyControls.CtrlFocus := Edit_LobbyPost;
    Panel_Lobby.Show;
  end;

  {Show MapEditor menu}
  if Sender=Button_MM_MapEd then begin
    MapEditor_ListUpdate;
    MapEditor_SizeChange(nil);
    Panel_MapEd.Show;
  end;

  {Show Options menu}
  if Sender=Button_MM_Options then
  begin
    Options_Fill(fMain.Settings, fGame.GlobalSettings);
    Panel_Options.Show;
  end;

  {Show Credits}
  if Sender=Button_MM_Credits then begin
    Panel_Credits.Show;
    Label_Credits_KaM.SmoothScrollToTop := TimeGet; //Set initial position
    Label_Credits_Remake.SmoothScrollToTop := TimeGet; //Set initial position
  end;

  {Show Loading... screen}
  if Sender=Panel_Loading then
    Panel_Loading.Show;

  {Show Error... screen}
  if Sender=Panel_Error then
    Panel_Error.Show;

  {Show Results screen}
  if Sender=Panel_Results then //This page can be accessed only by itself
    Panel_Results.Show;

  {Show ResultsMP screen}
  if Sender=Panel_ResultsMP then //This page can be accessed only by itself
    Panel_ResultsMP.Show;
end;


procedure TKMMainMenuInterface.MainMenu_PlayTutorial(Sender: TObject);
begin
  fGame.StartSingleMap(ExeDir + 'Tutorials\Town Tutorial\Town Tutorial.dat', fTextLibrary[TX_MENU_TUTORIAL_TOWN]);
end;


procedure TKMMainMenuInterface.MainMenu_PlayBattle(Sender: TObject);
begin
  fGame.StartSingleMap(ExeDir + 'Tutorials\Battle Tutorial\Battle Tutorial.dat', fTextLibrary[TX_MENU_TUTORIAL_BATTLE]);
end;


procedure TKMMainMenuInterface.MainMenu_ReplayLastMap(Sender: TObject);
begin
  fGame.StartLastMap; //Means replay last map
end;


procedure TKMMainMenuInterface.Campaign_Set(aCampaign: TKMCampaign);
const MapPic: array [Boolean] of byte = (10, 11);
var I: Integer;
begin
  Campaign_Selected := aCampaign;

  //Choose background
  Image_CampaignBG.RX := Campaign_Selected.BackGroundPic.RX;
  Image_CampaignBG.TexID := Campaign_Selected.BackGroundPic.ID;

  //Setup sites
  for I := 0 to High(Image_CampaignFlags) do
  begin
    Image_CampaignFlags[I].Visible   := I < Campaign_Selected.MapCount;
    Image_CampaignFlags[I].TexID     := MapPic[I <= Campaign_Selected.UnlockedMap];
    Image_CampaignFlags[I].HighlightOnMouseOver := I <= Campaign_Selected.UnlockedMap;
  end;

  //Place sites
  for I := 0 to Campaign_Selected.MapCount - 1 do
  begin
    Image_CampaignFlags[I].Left := Campaign_Selected.Maps[I].Flag.X;
    Image_CampaignFlags[I].Top  := Campaign_Selected.Maps[I].Flag.Y;
  end;

  //Select last map to play by 'clicking' last node
  Campaign_SelectMap(Image_CampaignFlags[Campaign_Selected.UnlockedMap]);
end;


procedure TKMMainMenuInterface.Campaign_SelectMap(Sender:TObject);
var i:integer;
begin
  if not (Sender is TKMImage) then exit;
  if not TKMImage(Sender).HighlightOnMouseOver then exit; //Skip closed maps

  Campaign_MapIndex := TKMImage(Sender).Tag;

  //Place highlight
  for i := 0 to High(Image_CampaignFlags) do
    Image_CampaignFlags[i].Highlight := (Campaign_MapIndex = i);

  //Connect by sub-nodes
  for i := 0 to High(Image_CampaignSubNode) do
  begin
    Image_CampaignSubNode[i].Visible := InRange(i, 0, Campaign_Selected.Maps[Campaign_MapIndex].NodeCount-1);
    Image_CampaignSubNode[i].Left := Campaign_Selected.Maps[Campaign_MapIndex].Nodes[i].X;
    Image_CampaignSubNode[i].Top  := Campaign_Selected.Maps[Campaign_MapIndex].Nodes[i].Y;
  end;

  Label_CampaignTitle.Caption := Format(fTextLibrary[TX_GAME_MISSION], [Campaign_MapIndex+1]);
  Label_CampaignText.Caption := Campaign_Selected.MissionText(Campaign_MapIndex);

  Panel_CampScroll.Height := 50 + Label_CampaignText.TextSize.Y + 70; //Add offset from top and space on bottom
  Panel_CampScroll.Top := Panel_Main.Height - Panel_CampScroll.Height;
  Image_Scroll.Height := Panel_CampScroll.Height;
end;


procedure TKMMainMenuInterface.Campaign_StartMap(Sender: TObject);
begin
  fGame.StartCampaignMap(Campaign_Selected, Campaign_MapIndex);
end;


procedure TKMMainMenuInterface.SingleMap_Clear;
var I: Integer;
begin
  for I := 0 to MENU_SP_MAPS_COUNT - 1 do
  begin
    Image_SingleMode[I].TexID       := 0;
    Label_SinglePlayers[I].Caption  := '';
    Label_SingleTitle1[I].Caption   := '';
    Label_SingleTitle2[I].Caption   := '';
    Label_SingleSize[I].Caption     := '';
  end;

  ScrollBar_SingleMaps.MaxValue := 0;
  SingleMap_SelectMap(-1);
  fMapCRC_Selected := 0;
end;


procedure TKMMainMenuInterface.SingleMap_RefreshList(Sender: TObject);
var
  I, MapID: Integer;
begin
  //Updating MaxValue may change Position
  ScrollBar_SingleMaps.MaxValue := Max(0, fMaps.Count - MENU_SP_MAPS_COUNT);

  //IDs of maps could changed, so use CRC to check
  //which one was selected
  for I := 0 to fMaps.Count-1 do
    if (fMaps[I].CRC = fMapCRC_Selected) then
        fMap_Selected := I;

  if not InRange(fMap_Selected - ScrollBar_SingleMaps.Position, 0, MENU_SP_MAPS_COUNT - 1) and
         (fJumpToSelectedMap) then begin
            if fMap_Selected < ScrollBar_SingleMaps.Position + MENU_SP_MAPS_COUNT - 1 then
              ScrollBar_SingleMaps.Position := fMap_Selected
            else if fMap_Selected > ScrollBar_SingleMaps.Position + MENU_SP_MAPS_COUNT - 1 then
              ScrollBar_SingleMaps.Position := fMap_Selected-MENU_SP_MAPS_COUNT + 1;
            fJumpToSelectedMap := False;
         end;

  for I := 0 to MENU_SP_MAPS_COUNT - 1 do
  begin
    MapID := ScrollBar_SingleMaps.Position + I;
    if MapID <= fMaps.Count - 1 then
    begin
      Image_SingleMode[I].TexID       := 28 + Byte(fMaps[MapID].Info.MissionMode <> mm_Tactic)*14;  //28 or 42
      Label_SinglePlayers[I].Caption  := IntToStr(fMaps[MapID].Info.PlayerCount);
      Label_SingleTitle1[I].Caption   := fMaps[MapID].FileName;
      Label_SingleTitle2[I].Caption   := fMaps[MapID].SmallDesc;
      Label_SingleSize[I].Caption     := fMaps[MapID].Info.MapSizeText;
    end;
  end;

  Shape_SingleMap.Visible := InRange(fMap_Selected - ScrollBar_SingleMaps.Position, 0, MENU_SP_MAPS_COUNT - 1);
  Shape_SingleMap.Top     := MENU_SP_MAPS_HEIGHT * (fMap_Selected - ScrollBar_SingleMaps.Position + 1); // Including header height
end;


procedure TKMMainMenuInterface.SingleMap_ScrollChange(Sender: TObject);
begin
  SingleMap_RefreshList(nil);
end;


procedure TKMMainMenuInterface.SingleMap_MapClick(Sender: TObject);
begin
  SingleMap_SelectMap(ScrollBar_SingleMaps.Position + TKMControl(Sender).Tag);
end;


procedure TKMMainMenuInterface.SingleMap_SelectMap(aIndex: Integer);
begin
  //User could have clicked on empty space in list and we get -1 or unused id
  if not InRange(aIndex, 0, fMaps.Count - 1) then
  begin
    fMap_Selected := -1;
    Label_SingleTitle.Caption   := '';
    Memo_SingleDesc.Text        := '';
    Label_SingleCondTyp.Caption := '';
    Label_SingleCondWin.Caption := '';
    Label_SingleCondDef.Caption := '';

    Minimap_SinglePreview.Hide;
  end
  else
  begin
    fMap_Selected := aIndex;
    fMapCRC_Selected := fMaps[fMap_Selected].CRC;
    Label_SingleTitle.Caption   := fMaps[fMap_Selected].FileName;
    Memo_SingleDesc.Text        := fMaps[fMap_Selected].BigDesc;
    Label_SingleCondTyp.Caption := Format(fTextLibrary[TX_MENU_MISSION_TYPE], [fMaps[fMap_Selected].Info.MissionModeText]);
    Label_SingleCondWin.Caption := Format(fTextLibrary[TX_MENU_WIN_CONDITION], [fMaps[fMap_Selected].Info.VictoryCondition]);
    Label_SingleCondDef.Caption := Format(fTextLibrary[TX_MENU_DEFEAT_CONDITION], [fMaps[fMap_Selected].Info.DefeatCondition]);

    Minimap_SinglePreview.Show;
    fMapView.LoadTerrain(MapNameToPath(fMaps[fMap_Selected].FileName, 'dat', False));
    fMapView.Update(False);
    Minimap_SinglePreview.UpdateFrom(fMapView);
  end;

  Button_SingleStart.Enabled := fMap_Selected <> -1;
  Shape_SingleMap.Visible := InRange(fMap_Selected - ScrollBar_SingleMaps.Position, 0, MENU_SP_MAPS_COUNT - 1);
  Shape_SingleMap.Top     := MENU_SP_MAPS_HEIGHT * (fMap_Selected - ScrollBar_SingleMaps.Position + 1); // Including header height
end;


procedure TKMMainMenuInterface.SingleMap_Start(Sender: TObject);
begin
  //This is also called by double clicking on a list entry
  if not Button_SingleStart.Enabled then
    Exit;

  if not InRange(fMap_Selected, 0, fMaps.Count-1) then exit; //Some odd index
  //scan should be terminated, as it is no longer needed
  fMaps.TerminateScan;
  fGame.StartSingleMap(MapNameToPath(fMaps[fMap_Selected].FileName,'dat',false),fMaps[fMap_Selected].FileName); //Provide mission FileName mask and title here
end;


procedure TKMMainMenuInterface.SingleMap_Sort(Sender: TObject);
var
  Method: TMapsSortMethod;
begin
  //Set Descending order by default and invert it if same column selected again
  if Sender = Button_SingleHeadTitle then
    if fMaps.SortMethod = smByNameDesc then
      Method := smByNameAsc
    else
      Method := smByNameDesc
  else
  if Sender = Button_SingleHeadSize then
    if fMaps.SortMethod = smBySizeDesc then
      Method := smBySizeAsc
    else
      Method := smBySizeDesc
  else
  if Sender = Button_SingleHeadTeams then
    if fMaps.SortMethod = smByPlayersDesc then
      Method := smByPlayersAsc
    else
      Method := smByPlayersDesc
  else
  if Sender = Button_SingleHeadMode then
    if fMaps.SortMethod = smByModeDesc then
      Method := smByModeAsc
    else
      Method := smByModeDesc
  else
    Method := smByNameAsc; //Default

  //scroll to selected map, so it will be shown on the screen
  fJumpToSelectedMap := True;

  //Start sorting and wait for SortComplete event
  fMaps.Sort(Method, SingleMap_RefreshList);
end;


procedure TKMMainMenuInterface.MP_Init(Sender: TObject);
begin
  fServerSelected := False;

  //Refresh the list when they first open the multiplayer page
  MP_ServersRefresh(Sender);

  Edit_MP_PlayerName.Text := fGame.GlobalSettings.MultiplayerName;

  Edit_MP_ServerName.Text := fGame.GlobalSettings.ServerName;
  Edit_MP_ServerPort.Text := fGame.GlobalSettings.ServerPort;

  Edit_MP_FindIP.Text := fGame.GlobalSettings.LastIP;
  Edit_MP_FindPort.Text := fGame.GlobalSettings.LastPort;
  Edit_MP_FindRoom.Text := fGame.GlobalSettings.LastRoom;

  Button_MP_GetIn.Disable;

  //Fetch the announcements display
  fGame.Networking.ServerQuery.OnAnnouncements := MP_AnnouncementsUpdated;
  fGame.Networking.ServerQuery.FetchAnnouncements(fGame.GlobalSettings.Locale);
  Memo_MP_Announcement.Clear;
  Memo_MP_Announcement.Add(fTextLibrary[TX_MP_MENU_LOADING_ANNOUNCEMENTS]);
end;


//Events binding is the same for Host and Joiner because of stand-alone Server
//E.g. If Server fails, Host can be disconnected from it as well as a Joiner
procedure TKMMainMenuInterface.MP_BindEvents;
begin
  fGame.Networking.OnTextMessage  := Lobby_OnMessage;
  fGame.Networking.OnPlayersSetup := Lobby_OnPlayersSetup;
  fGame.Networking.OnGameOptions  := Lobby_OnGameOptions;
  fGame.Networking.OnMapName      := Lobby_OnMapName;
  fGame.Networking.OnPingInfo     := Lobby_OnPingInfo;
  fGame.Networking.OnStartMap     := fGame.StartMultiplayerMap;
  fGame.Networking.OnStartSave    := fGame.StartMultiplayerSave;
  fGame.Networking.OnDisconnect   := Lobby_OnDisconnect;
  fGame.Networking.OnReassignedHost := Lobby_OnReassignedToHost;
end;


procedure TKMMainMenuInterface.MP_CreateServerCancelClick(Sender: TObject);
begin
  Panel_MPCreateServer.Hide;
end;


procedure TKMMainMenuInterface.MP_CreateServerClick(Sender: TObject);
begin
  Panel_MPCreateServer.Show;
end;


procedure TKMMainMenuInterface.MP_FindServerCancelClick(Sender: TObject);
begin
  Panel_MPFindServer.Hide;
end;


procedure TKMMainMenuInterface.MP_FindServerClick(Sender: TObject);
begin
  Panel_MPFindServer.Show;
end;


procedure TKMMainMenuInterface.MP_FindServerIPClick(Sender: TObject);
begin
  MP_Join(Edit_MP_FindIP.Text, Edit_MP_FindPort.Text, StrToIntDef(Edit_MP_FindRoom.Text, -1));
end;


//Save the Player and IP name so it is not lost inbetween activities
procedure TKMMainMenuInterface.MP_SaveSettings;
begin
  //Player name
  fGame.GlobalSettings.MultiplayerName := Edit_MP_PlayerName.Text;

  //Create Server popup
  fGame.GlobalSettings.ServerName := Edit_MP_ServerName.Text;
  fGame.GlobalSettings.ServerPort := Edit_MP_ServerPort.Text;

  //Join server popup
  fGame.GlobalSettings.LastPort := Edit_MP_FindPort.Text;
  fGame.GlobalSettings.LastRoom := Edit_MP_FindRoom.Text;
  fGame.GlobalSettings.LastIP   := Edit_MP_FindIP.Text;
end;


//Update status line
//todo: replace Color with enum
//When user tries to Join some server disable joining controls for that time
procedure TKMMainMenuInterface.MP_Update(const aStatus: string; aColor: TColor4; aBusy: Boolean);
begin
  //Toggle server creation
  Button_MP_CreateServer.Enabled := not aBusy;
  Button_MP_CreateLAN.Enabled := not aBusy;
  Button_MP_CreateWAN.Enabled := not aBusy;

  //Toggle server joining
  Button_MP_FindServerIP.Enabled := not aBusy;
  Button_MP_FindCancel.Enabled := not aBusy;
  //Button_MP_GetIn.Enabled := not aBusy;

  Label_MP_Status.Caption := aStatus;
  Label_MP_Status.FontColor := aColor;
end;


procedure TKMMainMenuInterface.MP_ServersRefresh(Sender: TObject);
begin
  fGame.Networking.ServerQuery.OnListUpdated := MP_ServersUpdateList;
  fGame.Networking.ServerQuery.RefreshList;
  ColList_Servers.Clear;
  Label_MP_Players.Caption := '';
  Label_MP_GameTime.Caption := '';
  Label_MP_Map.Caption := '';

  //Do not use 'Show' here as it will also make the parent panel visible
  //which could be already hidden if player switched pages
  Label_Servers_Status.Caption := fTextLibrary[TX_MP_MENU_REFRESHING];
  Label_Servers_Status.Visible := True;
  Button_MP_GetIn.Disable;
end;


//Refresh the display for the list of servers
procedure TKMMainMenuInterface.MP_ServersUpdateList(Sender: TObject);
const
  GameStateTextIDs: array [TMPGameState] of Integer = (TX_MP_STATE_NONE, TX_MP_STATE_LOBBY, TX_MP_STATE_LOADING, TX_MP_STATE_GAME);
var
  I: Integer;
  DisplayName: string;
  S: TKMServerInfo;
  R: TKMRoomInfo;
begin
  ColList_Servers.Clear;

  if fGame.Networking.ServerQuery.Rooms.Count = 0 then
  begin
    //Do not use 'Show' here as it will also make the parent panel visible
    //which could be already hidden if player switched pages
    Label_Servers_Status.Caption := fTextLibrary[TX_MP_MENU_NO_SERVERS];
    Label_Servers_Status.Visible := True;
  end
  else
  begin
    Label_Servers_Status.Hide;
    for I := 0 to fGame.Networking.ServerQuery.Rooms.Count - 1 do
    begin
      R := fGame.Networking.ServerQuery.Rooms[I];
      S := fGame.Networking.ServerQuery.Servers[R.ServerIndex];

      //Only show # if Server has more than 1 Room
      DisplayName := IfThen(R.OnlyRoom, S.Name, S.Name + ' #' + IntToStr(R.RoomID + 1));
      ColList_Servers.AddItem(
      MakeListRow([DisplayName, fTextLibrary[GameStateTextIDs[R.GameInfo.GameState]], IntToStr(R.GameInfo.PlayerCount), IntToStr(S.Ping)],
                  [$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, GetPingColor(S.Ping)], I));

      //if server was selected, we need to select it again, because TKMColumnListBox was cleared
      if fServerSelected
      and (R.RoomID = fSelectedRoomInfo.RoomID)
      and (S.IP = fSelectedServerInfo.IP)
      and (S.Port = fSelectedServerInfo.Port) then
      begin
        ColList_Servers.ItemIndex := I;
        MP_ServersClick(nil); //Shows info about this selected server
        if fJumpToSelectedServer
        and not InRange(I - ColList_Servers.TopIndex, 0, (ColList_Servers.Height div ColList_Servers.ItemHeight) - 1) then
        begin
          if I < ColList_Servers.TopIndex + (ColList_Servers.Height div ColList_Servers.ItemHeight) - 1 then
            ColList_Servers.TopIndex := I
          else
          if I > ColList_Servers.TopIndex + (ColList_Servers.Height div ColList_Servers.ItemHeight) - 1 then
            ColList_Servers.TopIndex := I - (ColList_Servers.Height div ColList_Servers.ItemHeight) + 1;
          fJumpToSelectedServer := False;
        end;
      end;
    end;
  end;
end;


procedure TKMMainMenuInterface.MP_AnnouncementsUpdated(const S: string);
begin
  Memo_MP_Announcement.Clear;
  Memo_MP_Announcement.Add(S);
end;


//Sort the servers list by said column ID
procedure TKMMainMenuInterface.MP_ServersSort(aIndex: Integer);
begin
  case ColList_Servers.SortIndex of
    //Sorting by name goes A..Z by default
    0:  if ColList_Servers.SortDirection = sdDown then
          fGame.Networking.ServerQuery.SortMethod := ssmByNameDesc
        else
          fGame.Networking.ServerQuery.SortMethod := ssmByNameAsc;
    //Sorting by state goes Lobby,Loading,Game,None by default
    1:  if ColList_Servers.SortDirection = sdDown then
          fGame.Networking.ServerQuery.SortMethod := ssmByStateDesc
        else
          fGame.Networking.ServerQuery.SortMethod := ssmByStateAsc;
    //Sorting by player count goes 8..0 by default
    2:  if ColList_Servers.SortDirection = sdDown then
          fGame.Networking.ServerQuery.SortMethod := ssmByPlayersAsc
        else
          fGame.Networking.ServerQuery.SortMethod := ssmByPlayersDesc;
    //Sorting by ping goes 0 ... 1000 by default
    3:  if ColList_Servers.SortDirection = sdDown then
          fGame.Networking.ServerQuery.SortMethod := ssmByPingDesc
        else
          fGame.Networking.ServerQuery.SortMethod := ssmByPingAsc;
  end;

  //scroll to selected server, so it will be shown on the screen
  fJumpToSelectedServer := True;

  //Refresh the display only if there are rooms to be sorted (otherwise it shows "no servers found" immediately)
  if fGame.Networking.ServerQuery.Rooms.Count > 0 then
    MP_ServersUpdateList(nil);
end;


procedure TKMMainMenuInterface.MP_ServersClick(Sender: TObject);
var ID: Integer;
begin
  ID := ColList_Servers.ItemIndex;
  if (ID = -1) or (ColList_Servers.Rows[ID].Tag = -1) then
  begin
    fServerSelected := False;
    Button_MP_GetIn.Disable;
    Label_MP_Players.Caption := '';
    Label_MP_GameTime.Caption := '';
    Label_MP_Map.Caption := '';
    Exit;
  end;

  fServerSelected := True;
  Button_MP_GetIn.Enable;

  fSelectedRoomInfo := fGame.Networking.ServerQuery.Rooms[ColList_Servers.Rows[ID].Tag];
  fSelectedServerInfo := fGame.Networking.ServerQuery.Servers[fSelectedRoomInfo.ServerIndex];

  Label_MP_Players.Caption := fSelectedRoomInfo.GameInfo.Players;
  Label_MP_GameTime.Caption := fSelectedRoomInfo.GameInfo.GetFormattedTime;
  Label_MP_Map.Caption := fSelectedRoomInfo.GameInfo.Map;
end;


procedure TKMMainMenuInterface.MP_ServersDoubleClick(Sender: TObject);
begin
  //MP_SelectServer gets called by first Click
  if Button_MP_GetIn.Enabled and (ColList_Servers.ItemIndex <> -1)
  and InRange(ColList_Servers.Rows[ColList_Servers.ItemIndex].Tag, 0, fGame.Networking.ServerQuery.Rooms.Count-1) then
    MP_GetInClick(Sender);
end;


procedure TKMMainMenuInterface.MP_HostClick(Sender: TObject);
begin
  //Save the player and IP name so it is not lost if something fails
  MP_SaveSettings;

  //Verify PlayerName;
  if Trim(Edit_MP_PlayerName.Text) = '' then
  begin
    MP_Update(fTextLibrary[TX_GAME_ERROR_BLANK_PLAYERNAME],$FF007FFF,false);
    fSoundLib.Play(sfxn_Error2);
    Exit;
  end;
  SwitchMenuPage(Sender); //Open lobby page

  MP_BindEvents;
  fGame.Networking.OnHostFail := MP_HostFail;
  fGame.Networking.Host(Edit_MP_PlayerName.Text, Edit_MP_ServerName.Text, Edit_MP_ServerPort.Text, (Sender = Button_MP_CreateWAN));
end;


procedure TKMMainMenuInterface.MP_GetInClick(Sender: TObject);
begin
  MP_Join(fSelectedServerInfo.IP, fSelectedServerInfo.Port, fSelectedRoomInfo.RoomID);
end;


procedure TKMMainMenuInterface.MP_Join(aServerAddress, aPort: string; aRoom: Integer);
begin
  //Save the player and IP name so it is not lost if the connection fails
  MP_SaveSettings;

  if Trim(Edit_MP_PlayerName.Text) = '' then
  begin
    MP_Update(fTextLibrary[TX_GAME_ERROR_BLANK_PLAYERNAME],$FF007FFF,false);
    fSoundLib.Play(sfxn_Error2);
    Exit;
  end;

  //Disable buttons to prevent multiple clicks while connection process is in progress
  MP_Update(fTextLibrary[TX_MP_MENU_STATUS_CONNECTING],$FF00FF00, True);

  //Send request to join
  fGame.Networking.OnJoinSucc := MP_JoinSuccess;
  fGame.Networking.OnJoinFail := MP_JoinFail;
  fGame.Networking.OnJoinAssignedHost := MP_JoinAssignedHost;
  fGame.Networking.Join(aServerAddress, aPort, Edit_MP_PlayerName.Text, aRoom); //Init lobby
end;


//We had recieved permission to join
procedure TKMMainMenuInterface.MP_JoinSuccess(Sender: TObject);
begin
  SwitchMenuPage(Button_MP_GetIn); //Open lobby page

  fGame.Networking.OnJoinSucc := nil;
  fGame.Networking.OnJoinFail := nil;
  fGame.Networking.OnJoinAssignedHost := nil;
  MP_BindEvents;
end;


procedure TKMMainMenuInterface.MP_JoinFail(const aData:string);
begin
  fGame.Networking.Disconnect;
  MP_Update(Format(fTextLibrary[TX_GAME_ERROR_CONNECTION_FAILED],[aData]),$FF007FFF,false);
  fSoundLib.Play(sfxn_Error2);
end;


procedure TKMMainMenuInterface.MP_JoinAssignedHost(Sender: TObject);
begin
  //We were joining a game and the server assigned hosting rights to us
  SwitchMenuPage(Button_MP_CreateLAN); //Open lobby page in host mode

  fGame.Networking.OnJoinSucc := nil;
  fGame.Networking.OnJoinFail := nil;
  fGame.Networking.OnJoinAssignedHost := nil;
  fGame.Networking.OnHostFail := MP_HostFail;
  MP_BindEvents;
end;


procedure TKMMainMenuInterface.MP_BackClick(Sender: TObject);
begin
  fGame.Networking.Disconnect;
  MP_SaveSettings;
  SwitchMenuPage(Sender);
end;


procedure TKMMainMenuInterface.MP_HostFail(const aData: string);
begin
  fGame.Networking.Disconnect;
  SwitchMenuPage(Button_LobbyBack);
  MP_Update(aData, $FF007FFF, False);
  fSoundLib.Play(sfxn_Error2);
end;


//Reset everything to it's defaults depending on users role (Host/Joiner/Reassigned)
procedure TKMMainMenuInterface.Lobby_Reset(Sender: TObject; aPreserveMessage: Boolean = False);
var I: Integer;
begin
  Label_LobbyServerName.Caption := '';

  for I := 0 to MAX_PLAYERS - 1 do
  begin
    Label_LobbyPlayer[I].Caption := '.';
    Image_LobbyFlag[I].TexID := 0;
    Label_LobbyPlayer[I].Hide;
    DropBox_LobbyPlayerSlot[I].Show;
    DropBox_LobbyPlayerSlot[I].Disable;
    DropBox_LobbyLoc[I].ItemIndex := 0;
    DropBox_LobbyLoc[I].Disable;
    DropBox_LobbyTeam[I].Disable;
    DropBox_LobbyTeam[I].ItemIndex := 0;
    Drop_LobbyColors[I].Disable;
    Drop_LobbyColors[I].ItemIndex := 0;
    DropBox_LobbyPlayerSlot[I].ItemIndex := 0; //Open
    Label_LobbyPing[I].Caption := '';
  end;

  if not aPreserveMessage then Memo_LobbyPosts.Clear;
  Edit_LobbyPost.Text := '';

  Label_LobbyMapName.Caption := '';
  Memo_LobbyMapDesc.Clear;

  TrackBar_LobbyPeacetime.Position := 0; //Default peacetime = 0

  Lobby_OnMapName('');

  //Setup for Host
  if (Sender = Button_MP_CreateWAN) or (Sender = Button_MP_CreateLAN) then
  begin
    Radio_LobbyMapType.Enable;
    Radio_LobbyMapType.ItemIndex := 0;
    Lobby_MapTypeSelect(nil);
    List_Lobby.Show;
    Label_LobbyMapName.Hide;
    Label_LobbyChooseMap.Show;
    Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_START]; //Start
    Button_LobbyStart.Disable;
    TrackBar_LobbyPeacetime.Enable;
    CheckBox_LobbyHostControl.Enable;
  end
  else //Setup for Joiner
  begin
    Radio_LobbyMapType.Disable;
    Radio_LobbyMapType.ItemIndex := 0;
    List_Lobby.Hide;
    Label_LobbyMapName.Show;
    Label_LobbyChooseMap.Hide;
    Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_READY]; //Ready
    Button_LobbyStart.Enable;
    TrackBar_LobbyPeacetime.Disable;
    CheckBox_LobbyHostControl.Disable;
  end;
end;


procedure TKMMainMenuInterface.Lobby_GameOptionsChange(Sender: TObject);
begin
  //Set the peacetime
  fGame.Networking.NetGameOptions.Peacetime := EnsureRange(TrackBar_LobbyPeacetime.Position, 0, 300);
  fGame.Networking.SendGameOptions;

  //Refresh the data to controls
  Lobby_OnGameOptions(nil);
end;


procedure TKMMainMenuInterface.Lobby_OnGameOptions(Sender: TObject);
begin
  TrackBar_LobbyPeacetime.Position := fGame.Networking.NetGameOptions.Peacetime;
end;


//Try to change players setup, Networking will check if it can be done under current
//conditions immediately and reverts the change without disturbing Host.
//If the change is possible Networking will send query to the Host.
//Host will reply with OnPlayersSetup event and data will be actualized.
procedure TKMMainMenuInterface.Lobby_PlayersSetupChange(Sender: TObject);
var i:integer;
begin
  //Host control toggle
  if Sender = CheckBox_LobbyHostControl then
  begin
    fGame.Networking.NetPlayers.HostDoesSetup := CheckBox_LobbyHostControl.Checked;
    fGame.Networking.SendPlayerListAndRefreshPlayersSetup;
  end;

  for i:=0 to MAX_PLAYERS-1 do
  begin
    //Starting location
    if (Sender = DropBox_LobbyLoc[i]) and DropBox_LobbyLoc[i].Enabled then
    begin
      fGame.Networking.SelectLoc(DropBox_LobbyLoc[i].ItemIndex, i+1);
      DropBox_LobbyLoc[i].ItemIndex := fGame.Networking.NetPlayers[i+1].StartLocation;
    end;

    //Team
    if (Sender = DropBox_LobbyTeam[i]) and DropBox_LobbyTeam[i].Enabled then
      fGame.Networking.SelectTeam(DropBox_LobbyTeam[i].ItemIndex, i+1);

    //Color
    if (Sender = Drop_LobbyColors[i]) and Drop_LobbyColors[i].Enabled then
    begin
      fGame.Networking.SelectColor(Drop_LobbyColors[i].ItemIndex, i+1);
      Drop_LobbyColors[i].ItemIndex := fGame.Networking.NetPlayers[i+1].FlagColorID;
    end;

    if Sender = DropBox_LobbyPlayerSlot[i] then
    begin
      //Modify an existing player
      if (i < fGame.Networking.NetPlayers.Count) then
      begin
        case DropBox_LobbyPlayerSlot[i].ItemIndex of
          0: //Open
            begin
              if fGame.Networking.NetPlayers[i+1].IsComputer then
                fGame.Networking.NetPlayers.RemAIPlayer(i+1)
              else if fGame.Networking.NetPlayers[i+1].IsClosed then
                fGame.Networking.NetPlayers.RemClosedPlayer(i+1);
            end;
          1: //Closed
            fGame.Networking.NetPlayers.AddClosedPlayer(i+1); //Replace it
          2: //AI
            fGame.Networking.NetPlayers.AddAIPlayer(i+1); //Replace it
        end;
      end
      else
      begin
        //Add a new player
        if DropBox_LobbyPlayerSlot[i].ItemIndex = 1 then //Closed
          fGame.Networking.NetPlayers.AddClosedPlayer;
        if DropBox_LobbyPlayerSlot[i].ItemIndex = 2 then //AI
        begin
          fGame.Networking.NetPlayers.AddAIPlayer;
          if fGame.Networking.SelectGameKind = ngk_Save then
            fGame.Networking.MatchPlayersToSave(fGame.Networking.NetPlayers.Count); //Match new AI player in save
        end;
      end;
      fGame.Networking.SendPlayerListAndRefreshPlayersSetup;
    end;
  end;
end;


//Players list has been updated
//We should reflect it to UI
//todo: Refactor formatting
procedure TKMMainMenuInterface.Lobby_OnPlayersSetup(Sender: TObject);
var i:integer; MyNik, CanEdit, HostCanEdit, IsSave, IsValid:boolean;
begin
  IsSave := fGame.Networking.SelectGameKind = ngk_Save;
  for i:=0 to fGame.Networking.NetPlayers.Count - 1 do
  begin
    Label_LobbyPlayer[i].Caption := fGame.Networking.NetPlayers[i+1].GetNickname;
    if fGame.Networking.NetPlayers[i+1].LangCode <> '' then
         Image_LobbyFlag[i].TexID := fLocales.GetLocale(fGame.Networking.NetPlayers[i+1].LangCode).FlagSpriteID
    else Image_LobbyFlag[i].TexID := 0;
    if fGame.Networking.IsHost and (not fGame.Networking.NetPlayers[i+1].IsHuman) then
    begin
      Label_LobbyPlayer[i].Hide;
      DropBox_LobbyPlayerSlot[i].Show;
      if fGame.Networking.NetPlayers[i+1].IsComputer then
        DropBox_LobbyPlayerSlot[i].ItemIndex := 2 //AI
      else
        DropBox_LobbyPlayerSlot[i].ItemIndex := 1; //Closed
      DropBox_LobbyPlayerSlot[i].Enabled := fGame.Networking.IsHost;
    end
    else
    begin
      Label_LobbyPlayer[i].Show;
      DropBox_LobbyPlayerSlot[i].Hide;
      DropBox_LobbyPlayerSlot[i].ItemIndex := 0; //Open
    end;
    //If we can't load the map, don't attempt to show starting locations
    IsValid := false;
    if fGame.Networking.SelectGameKind = ngk_Save then
      IsValid := fGame.Networking.SaveInfo.IsValid;
    if fGame.Networking.SelectGameKind = ngk_Map then
      IsValid := fGame.Networking.MapInfo.IsValid;
    if IsValid then
      DropBox_LobbyLoc[i].ItemIndex := fGame.Networking.NetPlayers[i+1].StartLocation
    else
      DropBox_LobbyLoc[i].ItemIndex := 0;
    DropBox_LobbyTeam[i].ItemIndex := fGame.Networking.NetPlayers[i+1].Team;
    Drop_LobbyColors[i].ItemIndex := fGame.Networking.NetPlayers[i+1].FlagColorID;
    CheckBox_LobbyReady[i].Checked := fGame.Networking.NetPlayers[i+1].ReadyToStart;

    MyNik := (i+1 = fGame.Networking.MyIndex); //Our index
    //We are allowed to edit if it is our nickname and we are set as NOT ready,
    //or we are the host and this player is an AI
    CanEdit := (MyNik and (fGame.Networking.IsHost or not fGame.Networking.NetPlayers.HostDoesSetup) and
                          (fGame.Networking.IsHost or not fGame.Networking.NetPlayers[i+1].ReadyToStart)) or
               (fGame.Networking.IsHost and fGame.Networking.NetPlayers[i+1].IsComputer);
    HostCanEdit := (fGame.Networking.IsHost and fGame.Networking.NetPlayers.HostDoesSetup and
                    not fGame.Networking.NetPlayers[i+1].IsClosed);
    DropBox_LobbyLoc[i].Enabled := (CanEdit or HostCanEdit);
    DropBox_LobbyTeam[i].Enabled := (CanEdit or HostCanEdit) and not IsSave; //Can't change color or teams in a loaded save
    Drop_LobbyColors[i].Enabled := (CanEdit or (MyNik and not fGame.Networking.NetPlayers[i+1].ReadyToStart)) and not IsSave;
    if MyNik and not fGame.Networking.IsHost then
    begin
      if fGame.Networking.NetPlayers[i+1].ReadyToStart then
        Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_NOT_READY]
      else
        Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_READY];
    end
  end;

  //Disable rest of the players
  for I := fGame.Networking.NetPlayers.Count to MAX_PLAYERS - 1 do
  begin
    Label_LobbyPlayer[I].Caption := '';
    Image_LobbyFlag[I].TexID := 0;
    Label_LobbyPlayer[I].Hide;
    DropBox_LobbyPlayerSlot[I].Show;
    DropBox_LobbyPlayerSlot[I].ItemIndex := 0; //Open
    DropBox_LobbyLoc[I].ItemIndex := 0;
    DropBox_LobbyTeam[I].ItemIndex := 0;
    Drop_LobbyColors[I].ItemIndex := 0;
    //Only host may change player slots, and only the first unused slot may be changed (so there are no gaps in net players list)
    DropBox_LobbyPlayerSlot[I].Enabled := fGame.Networking.IsHost and (I = fGame.Networking.NetPlayers.Count);
    CheckBox_LobbyReady[I].Checked := False;
    DropBox_LobbyLoc[I].Disable;
    DropBox_LobbyTeam[I].Disable;
    Drop_LobbyColors[I].Disable;
  end;

  CheckBox_LobbyHostControl.Checked := fGame.Networking.NetPlayers.HostDoesSetup;
  if fGame.Networking.IsHost then
    Button_LobbyStart.Enabled := fGame.Networking.CanStart;
  //If the game can't be started the text message with explanation will appear in chat area
end;


procedure TKMMainMenuInterface.Lobby_OnPingInfo(Sender: TObject);
var i:integer;
begin
  for i:=0 to MAX_PLAYERS-1 do
  if (fGame.Networking.Connected) and (i < fGame.Networking.NetPlayers.Count) and
     (fGame.Networking.NetPlayers[i+1].IsHuman) then
  begin
    Label_LobbyPing[i].Caption := inttostr(fGame.Networking.NetPlayers[i+1].GetInstantPing);
    Label_LobbyPing[i].FontColor := GetPingColor(fGame.Networking.NetPlayers[i+1].GetInstantPing);
  end
  else
    Label_LobbyPing[i].Caption := '';
  Label_LobbyServerName.Caption := fGame.Networking.ServerName+' #'+IntToStr(fGame.Networking.ServerRoom+1)+
                                   '  '+fGame.Networking.ServerAddress+' : '+fGame.Networking.ServerPort;
end;


procedure TKMMainMenuInterface.Lobby_MapTypeSelect(Sender: TObject);
begin
  case Radio_LobbyMapType.ItemIndex of
    0:  //Build Map
        begin
          fMapsMP.Refresh(Lobby_MapTypeRefreshDone);
          List_Lobby.DefaultCaption := fTextLibrary[TX_LOBBY_MAP_SELECT];
        end;
    1:  //Fight Map
        begin
          fMapsMP.Refresh(Lobby_MapTypeRefreshDone);
          List_Lobby.DefaultCaption := fTextLibrary[TX_LOBBY_MAP_SELECT];
        end;
    2:  //Co-op Map
        begin
          fMapsMP.Refresh(Lobby_MapTypeRefreshDone);
          List_Lobby.DefaultCaption := fTextLibrary[TX_LOBBY_MAP_SELECT];
        end;
    3:  //Saved Game
        begin
          fSavesMP.Refresh(Lobby_SaveRefreshDone, True);
          List_Lobby.DefaultCaption := fTextLibrary[TX_LOBBY_MAP_SELECT_SAVED];
        end;
    else
        begin
          List_Lobby.DefaultCaption := '<<<LEER>>>';
          List_Lobby.SetItems('');
        end;
  end;
  List_Lobby.ItemIndex := -1; //Clear previously selected item

  //The Sender is nil in Reset_Lobby when we are not connected
  if Sender <> nil then
    fGame.Networking.SelectNoMap(fTextLibrary[TX_LOBBY_MAP_NONE]);
end;


procedure TKMMainMenuInterface.Lobby_MapTypeRefreshDone(Sender: TObject);
begin
  case Radio_LobbyMapType.ItemIndex of
    0:  List_Lobby.SetItems(fMapsMP.MapListBuild); //Build Map
    1:  List_Lobby.SetItems(fMapsMP.MapListFight); //Fight Map
    2:  List_Lobby.SetItems(fMapsMP.MapListCoop); //Co-op Map
    //Other cases are already handled in Lobby_MapTypeSelect
  end;
end;


//Just pass FileName to Networking, it will check validity itself
procedure TKMMainMenuInterface.Lobby_MapSelect(Sender: TObject);
begin
  if Radio_LobbyMapType.ItemIndex < 3 then
    fGame.Networking.SelectMap(List_Lobby.Item[List_Lobby.ItemIndex])
  else
    fGame.Networking.SelectSave(List_Lobby.Item[List_Lobby.ItemIndex]);
end;


procedure TKMMainMenuInterface.Lobby_OnMapName(const aData:string);
var i:Integer; DropText:string;
begin
  //todo: It will be better to rework this code into CASE, moving common part into internal procedure
  Minimap_LobbyPreview.Visible := False; //Hide unless correct options are selected (e.g. there's no preview for multiplayer saves)
  if fGame.Networking.SelectGameKind <> ngk_None then
  begin
    if fGame.Networking.SelectGameKind = ngk_Save then
    begin
      Label_LobbyMapName.Caption := fGame.Networking.SaveInfo.FileName;
      Memo_LobbyMapDesc.Clear;
      Memo_LobbyMapDesc.Text := fGame.Networking.GameInfo.GetTitleWithTime;
      TrackBar_LobbyPeacetime.Disable;
      if not fGame.Networking.IsHost then Radio_LobbyMapType.ItemIndex := 3;
    end
    else
    begin
      fMapView.LoadTerrain(MapNameToPath(fGame.Networking.MapInfo.FileName, 'dat', True));
      fMapView.Update(True);
      Minimap_LobbyPreview.UpdateFrom(fMapView);
      Minimap_LobbyPreview.Visible := true;

      Label_LobbyMapName.Caption := fGame.Networking.GameInfo.Title;
      Memo_LobbyMapDesc.Text := fGame.Networking.MapInfo.BigDesc;
      if fGame.Networking.IsHost then
        TrackBar_LobbyPeacetime.Enable
      else
      begin
        if fGame.Networking.MapInfo.IsCoop then
          Radio_LobbyMapType.ItemIndex := 2
        else
          if fGame.Networking.MapInfo.Info.MissionMode = mm_Tactic then
            Radio_LobbyMapType.ItemIndex := 1
          else
            Radio_LobbyMapType.ItemIndex := 0;
      end;
    end;

    Label_LobbyMapMode.Caption := fTextLibrary[TX_LOBBY_MAP_MODE]+' '+fGame.Networking.GameInfo.MissionModeText;
    //Label_LobbyMapCond.Caption :=
    Label_LobbyMapSize.Caption := fTextLibrary[TX_LOBBY_MAP_SIZE]+' '+fGame.Networking.GameInfo.MapSizeText;

    //Update starting locations
    if fGame.Networking.SelectGameKind = ngk_Save then
      DropText := fTextLibrary[TX_LOBBY_SELECT] + eol
    else
      DropText := fTextLibrary[TX_LOBBY_RANDOM] + eol;

    for i:=1 to fGame.Networking.GameInfo.PlayerCount do
      DropText := DropText + fGame.Networking.GameInfo.LocationName[i-1] + eol;

    for i:=0 to MAX_PLAYERS-1 do
      DropBox_LobbyLoc[i].SetItems(DropText);
  end
  else
  begin
    Label_LobbyMapName.Caption := aData; //aData is some error message
    Memo_LobbyMapDesc.Clear;
    if aData <> fTextLibrary[TX_LOBBY_MAP_NONE] then
      Memo_LobbyMapDesc.Text := aData;
    Label_LobbyMapMode.Caption := fTextLibrary[TX_LOBBY_MAP_MODE];
    Label_LobbyMapSize.Caption := fTextLibrary[TX_LOBBY_MAP_SIZE];

    //Update starting locations
    DropText := fTextLibrary[TX_LOBBY_RANDOM] + eol;

    for i:=0 to MAX_PLAYERS-1 do
      DropBox_LobbyLoc[i].SetItems(DropText);
  end;
end;


procedure TKMMainMenuInterface.Lobby_SaveRefreshDone(Sender: TObject);
begin
  List_Lobby.SetItems(fSavesMP.SavesList);
end;


//We have been assigned to be the host of the game because the host disconnected. Reopen lobby page in correct mode.
procedure TKMMainMenuInterface.Lobby_OnReassignedToHost(Sender: TObject);
begin
  Lobby_Reset(Button_MP_CreateLAN, True); //Will reset the lobby page into host mode, preserving messages
  if fGame.Networking.SelectGameKind = ngk_None then
    Radio_LobbyMapType.ItemIndex := 0 //Default
  else
    if fGame.Networking.SelectGameKind = ngk_Save then
      Radio_LobbyMapType.ItemIndex := 3
    else
      if fGame.Networking.MapInfo.IsCoop then
        Radio_LobbyMapType.ItemIndex := 2
      else
        if fGame.Networking.MapInfo.Info.MissionMode = mm_Tactic then
          Radio_LobbyMapType.ItemIndex := 1
        else
          Radio_LobbyMapType.ItemIndex := 0;


  Lobby_MapTypeSelect(nil);
  if fGame.Networking.SelectGameKind = ngk_Save then
    List_Lobby.SelectByName(fGame.Networking.SaveInfo.FileName) //Select the map
  else
    if fGame.Networking.SelectGameKind = ngk_Map then
      List_Lobby.SelectByName(fGame.Networking.MapInfo.FileName); //Select the map

  Lobby_OnGameOptions(nil);
  if fGame.Networking.SelectGameKind = ngk_Save then
    Lobby_OnMapName(fGame.Networking.SaveInfo.FileName)
  else
    if fGame.Networking.SelectGameKind = ngk_Map then
      Lobby_OnMapName(fGame.Networking.MapInfo.FileName);
end;


//Post what user has typed
procedure TKMMainMenuInterface.Lobby_PostKey(Sender: TObject; Key: Word);
var PlayerID: Integer;
begin
  if (Key <> VK_RETURN) or (Trim(Edit_LobbyPost.Text) = '') then exit;

  //Check for console commands
  if fGame.Networking.IsHost
  and (Length(Edit_LobbyPost.Text) = 7)
  and SameText(LeftStr(Edit_LobbyPost.Text, 6), '/kick ')
  and TryStrToInt(Edit_LobbyPost.Text[7], PlayerID)
  and InRange(PlayerID, 1, fGame.Networking.NetPlayers.Count)
  and fGame.Networking.NetPlayers[PlayerID].IsHuman
  and (PlayerID <> fGame.Networking.MyIndex) then
    fGame.Networking.KickPlayer(PlayerID)
  else
    fGame.Networking.PostMessage(Edit_LobbyPost.Text, True);

  Edit_LobbyPost.Text := '';
end;


procedure TKMMainMenuInterface.Lobby_OnMessage(const aData:string);
begin
  Memo_LobbyPosts.Add(aData);
end;


//We were disconnected from Server. Either we were kicked, or connection broke down
procedure TKMMainMenuInterface.Lobby_OnDisconnect(const aData:string);
begin
  fGame.Networking.Disconnect;
  MP_Update(aData,$FF00FFFF,false);
  fSoundLib.Play(sfxn_Error2);
  SwitchMenuPage(Button_LobbyBack);
end;


procedure TKMMainMenuInterface.Lobby_BackClick(Sender: TObject);
begin
  fGame.Networking.AnnounceDisconnect;
  fGame.Networking.Disconnect;
  MP_Update(fTextLibrary[TX_GAME_ERROR_DISCONNECT],$FF00FFFF,false);
  SwitchMenuPage(Button_LobbyBack);
end;


procedure TKMMainMenuInterface.Lobby_StartClick(Sender: TObject);
begin
  if fGame.Networking.IsHost then
    fGame.Networking.StartClick
  else
  begin
    if fGame.Networking.ReadyToStart then
      Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_NOT_READY]
    else
      Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_READY];
  end;
end;


procedure TKMMainMenuInterface.Load_ListClick(Sender: TObject);
begin
  //Hide delete confirmation if player has selected a different savegame item
  if (Sender = List_Load) then
    Load_DeleteConfirmation(false);
  Button_Delete.Enabled := InRange(List_Load.ItemIndex, 0, fSaves.Count-1);
  Button_Load.Enabled := InRange(List_Load.ItemIndex, 0, fSaves.Count-1)
                         and fSaves[List_Load.ItemIndex].IsValid;
  if Button_Load.Enabled then
  begin
    fSave_Selected := List_Load.ItemIndex;
    fSaveCRC_Selected := fSaves[List_Load.ItemIndex].CRC;
    fSaves[List_Load.ItemIndex].LoadMinimap(fMapView);
    Minimap_LoadPreview.UpdateFrom(fMapView);
    Minimap_LoadPreview.Show;
  end
  else
    Minimap_LoadPreview.Hide;
end;


procedure TKMMainMenuInterface.Load_Click(Sender: TObject);
begin
  if not Button_Load.Enabled then exit; //This is also called by double clicking
  if not InRange(List_Load.ItemIndex, 0, fSaves.Count-1) then Exit;
  fSaves.TerminateScan; //stop scan as it is no longer needed
  fGame.StartSingleSave(fSaves[List_Load.ItemIndex].FileName);
end;


procedure TKMMainMenuInterface.Load_Delete_Click(Sender: TObject);
var
  PreviouslySelected: Integer;
begin
  if List_Load.ItemIndex = -1 then Exit;

  if Sender = Button_Delete then
    Load_DeleteConfirmation(true);

  if (Sender = Button_DeleteYes) or (Sender = Button_DeleteNo) then
    Load_DeleteConfirmation(false); //Hide confirmation anyways

  //Delete the savegame
  if Sender = Button_DeleteYes then
  begin
    PreviouslySelected := List_Load.ItemIndex;
    fSaves.DeleteSave(List_Load.ItemIndex);
    Load_RefreshList(fSaves);
    if List_Load.RowCount > 0 then
      List_Load.ItemIndex := EnsureRange(PreviouslySelected, 0, List_Load.RowCount - 1)
    else
      List_Load.ItemIndex := -1; //there are no saves, nothing to select
    Load_ListClick(List_Load);
  end;
end;


procedure TKMMainMenuInterface.Load_RefreshList(Sender: TObject);
var i:integer;
begin
  List_Load.Clear;

  if (Sender = fSaves) then
  begin
    for I:=0 to fSaves.Count - 1 do
      List_Load.AddItem(MakeListRow(
                          [fSaves[i].FileName, fSaves[i].Info.GetTitleWithTime],
                          [$FFFFFFFF, $FFFFFFFF]));

    //IDs of saves could changed, so use CRC to check
    //which one was selected
    for I := 0 to fSaves.Count - 1 do
      if (fSaves[I].CRC = fSaveCRC_Selected) then
      begin
          fSave_Selected := I;
          List_Load.ItemIndex := I;
      end;
    end;

  List_Load.ItemIndex := fSave_Selected;

  if not InRange(fSave_Selected - List_Load.TopIndex, 0, (List_Load.Height div List_Load.ItemHeight) - 1) and
         (fJumpToSelectedSave) then begin
            if fSave_Selected < List_Load.TopIndex + (List_Load.Height div List_Load.ItemHeight) - 1 then
              List_Load.TopIndex := fSave_Selected
            else if fSave_Selected > List_Load.TopIndex + (List_Load.Height div List_Load.ItemHeight) - 1 then
              List_Load.TopIndex := fSave_Selected - (List_Load.Height div List_Load.ItemHeight) + 1;
            fJumpToSelectedSave := False;
         end;

  Load_ListClick(nil);

  if fSaves.ScanFinished then  //do only if we don't scan
    Load_DeleteConfirmation(false);
end;


procedure TKMMainMenuInterface.Load_SavesSort(aIndex: Integer);
begin
  case List_Load.SortIndex of
    //Sorting by filename goes A..Z by default
    0:  if List_Load.SortDirection = sdDown then
          fSaves.Sort(smByFileNameDesc, Load_RefreshList)
        else
          fSaves.Sort(smByFileNameAsc, Load_RefreshList);
    //Sorting by description goes A..Z by default
    1:  if List_Load.SortDirection = sdDown then
          fSaves.Sort(smByDescriptionDesc, Load_RefreshList)
        else
          fSaves.Sort(smByDescriptionAsc, Load_RefreshList);
  end;

  //scroll to selected save, so it will be shown on the screen
  fJumpToSelectedSave := True;

  //Refresh the display only if there are saves to be sorted
  if fSaves.Count > 0 then
    Load_RefreshList(fSaves);
end;



//Shortcut to choose if DeleteConfirmation should be displayed or hid
procedure TKMMainMenuInterface.Load_DeleteConfirmation(aVisible:boolean);
begin
  Label_DeleteConfirm.Visible := aVisible;
  Button_DeleteYes.Visible := aVisible;
  Button_DeleteNo.Visible := aVisible;
  Button_Delete.Visible := not aVisible;
end;


procedure TKMMainMenuInterface.Replays_ListClick(Sender: TObject);
begin
  Button_ReplaysPlay.Enabled := InRange(List_Replays.ItemIndex, 0, fSaves.Count-1)
                                and fSaves[List_Replays.ItemIndex].IsValid
                                and fGame.ReplayExists(fSaves[List_Replays.ItemIndex].FileName, (Radio_Replays_Type.ItemIndex = 1));
  if Button_ReplaysPlay.Enabled then
  begin
    fSave_Selected := List_Replays.ItemIndex;
    fSaveCRC_Selected := fSaves[List_Replays.ItemIndex].CRC;
    fSaves[List_Replays.ItemIndex].LoadMinimap(fMapView);
    Minimap_ReplayPreview.UpdateFrom(fMapView);
    Minimap_ReplayPreview.Show;
  end
  else
    Minimap_ReplayPreview.Hide;
end;


procedure TKMMainMenuInterface.Replay_TypeChange(Sender: TObject);
begin
  fSaves.TerminateScan;
  fSave_Selected := -1;
  fSaveCRC_Selected := 0;
  fSaves.Refresh(Replays_RefreshList, (Radio_Replays_Type.ItemIndex = 1));
  Replays_RefreshList(nil);  //we need to refresh immediately
end;


procedure TKMMainMenuInterface.Replays_RefreshList(Sender: TObject);
var I: Integer;
begin
  List_Replays.Clear;

  if (Sender = fSaves) then
  begin
    for i:=0 to fSaves.Count-1 do
      List_Replays.AddItem(MakeListRow(
                            [fSaves[i].FileName, fSaves[i].Info.GetTitleWithTime],
                            [$FFFFFFFF, $FFFFFFFF]));

    for I := 0 to fSaves.Count - 1 do
      if (fSaves[I].CRC = fSaveCRC_Selected) then
      begin
          fSave_Selected := I;
          List_Replays.ItemIndex := I;
      end;
  end;

  List_Replays.ItemIndex := fSave_Selected;

  if not InRange(fSave_Selected - List_Replays.TopIndex, 0, (List_Replays.Height div List_Replays.ItemHeight) - 1) and
         (fJumpToSelectedSave) then begin
            if fSave_Selected < List_Replays.TopIndex + (List_Replays.Height div List_Replays.ItemHeight) - 1 then
              List_Replays.TopIndex := fSave_Selected
            else if fSave_Selected > List_Replays.TopIndex + (List_Replays.Height div List_Replays.ItemHeight) - 1 then
              List_Replays.TopIndex := fSave_Selected - (List_Replays.Height div List_Replays.ItemHeight) + 1;
            fJumpToSelectedSave := False;
         end;

  Replays_ListClick(List_Replays);
end;


procedure TKMMainMenuInterface.Replays_SavesSort(aIndex: Integer);
begin
  case List_Replays.SortIndex of
    //Sorting by filename goes A..Z by default
    0:  if List_Replays.SortDirection = sdDown then
          fSaves.Sort(smByFileNameDesc, Replays_RefreshList)
        else
          fSaves.Sort(smByFileNameAsc, Replays_RefreshList);
    //Sorting by description goes A..Z by default
    1:  if List_Replays.SortDirection = sdDown then
          fSaves.Sort(smByDescriptionDesc, Replays_RefreshList)
        else
          fSaves.Sort(smByDescriptionAsc, Replays_RefreshList);
  end;

  //scroll to selected save, so it will be shown on the screen
  fJumpToSelectedSave := True;

  //Refresh the display only if there are saves to be sorted
  if fSaves.Count > 0 then
    Replays_RefreshList(fSaves);
end;


procedure TKMMainMenuInterface.Replays_Play(Sender: TObject);
begin
  if not Button_ReplaysPlay.Enabled then exit; //This is also called by double clicking
  if not InRange(List_Replays.ItemIndex, 0, fSaves.Count-1) then Exit;
  fSaves.TerminateScan; //stop scan as it is no longer needed
  fGame.StartReplay(fSaves[List_Replays.ItemIndex].FileName,(Radio_Replays_Type.ItemIndex = 1));
end;


procedure TKMMainMenuInterface.MapEditor_Start(Sender: TObject);
begin
  if Sender = Button_MapEd_Create then
    fGame.StartMapEditor('', false, MapEdSizeX, MapEdSizeY); //Provide mission FileName here, Mapsize will be ignored if map exists
  //This is also called by double clicking on a map in the list
  if ((Sender = Button_MapEd_Load) or (Sender = List_MapEd)) and
     Button_MapEd_Load.Enabled and (List_MapEd.ItemIndex <> -1) then
    fGame.StartMapEditor(MapNameToPath(List_MapEd.Item[List_MapEd.ItemIndex], 'dat', Radio_MapEd_MapType.ItemIndex = 1), Radio_MapEd_MapType.ItemIndex = 1, 0, 0); //Provide mission FileName here, Mapsize will be ignored if map exists
end;


procedure TKMMainMenuInterface.MapEditor_SizeChange(Sender: TObject);
begin
  //Find out new map dimensions
  MapEdSizeX := MapSize[Radio_MapEd_SizeX.ItemIndex+1];
  MapEdSizeY := MapSize[Radio_MapEd_SizeY.ItemIndex+1];
end;


procedure TKMMainMenuInterface.MapEditor_MapTypeChange(Sender: TObject);
begin
  MapEditor_ListUpdate;
end;


//Clear the list and initiate refresh
procedure TKMMainMenuInterface.MapEditor_ListUpdate;
begin
  fMaps.TerminateScan;
  fMapsMP.TerminateScan;

  List_MapEd.SetItems('');

  //If both Maps and MapsMP are scanning at once ListUpdateDone can be called from either one
  //meaning we can access inconsistent and trigger assertion
  if Radio_MapEd_MapType.ItemIndex = 0 then
    fMaps.Refresh(MapEditor_ListUpdateDone)
  else
    fMapsMP.Refresh(MapEditor_ListUpdateDone);
end;


procedure TKMMainMenuInterface.MapEditor_ListUpdateDone(Sender: TObject);
begin
  if Radio_MapEd_MapType.ItemIndex = 0 then
    List_MapEd.SetItems(fMaps.MapList)
  else
    List_MapEd.SetItems(fMapsMP.MapList);
end;


procedure TKMMainMenuInterface.MapEditor_SelectMap(Sender: TObject);
begin
  if (not Button_MapEd_Load.Enabled) or (List_MapEd.ItemIndex = -1) then exit;
  fMapView.LoadTerrain(MapNameToPath(List_MapEd.Item[List_MapEd.ItemIndex], 'dat', Radio_MapEd_MapType.ItemIndex = 1));
  fMapView.Update(True);
  Minimap_MapEd.UpdateFrom(fMapView);
end;


//This is called when the options page is shown, so update all the values
//Note: Options can be required to fill before fGame is completely initialized, hence we need to pass either fGame.Settings or a direct Settings link
procedure TKMMainMenuInterface.Options_Fill(aMainSettings: TMainSettings; aGameSettings: TGameSettings);
begin
  CheckBox_Options_Autosave.Checked     := aGameSettings.Autosave;
  TrackBar_Options_Brightness.Position  := aGameSettings.Brightness;
  CheckBox_Options_Shadows.Checked      := aGameSettings.AlphaShadows;
  TrackBar_Options_ScrollSpeed.Position := aGameSettings.ScrollSpeed;
  TrackBar_Options_SFX.Position         := Round(aGameSettings.SoundFXVolume * TrackBar_Options_SFX.MaxValue);
  TrackBar_Options_Music.Position       := Round(aGameSettings.MusicVolume * TrackBar_Options_Music.MaxValue);
  CheckBox_Options_MusicOff.Checked     := aGameSettings.MusicOff;
  TrackBar_Options_Music.Enabled        := not CheckBox_Options_MusicOff.Checked;
  CheckBox_Options_ShuffleOn.Checked    := aGameSettings.ShuffleOn;
  CheckBox_Options_ShuffleOn.Enabled    := not CheckBox_Options_MusicOff.Checked;

  Radio_Options_Lang.ItemIndex := fLocales.GetIDFromCode(aGameSettings.Locale);

  //we need to reset dropboxes every time we enter Options page
  Options_Refresh_DropBoxes;

end;


procedure TKMMainMenuInterface.Options_Change(Sender: TObject);
var
  MusicToggled, ShuffleToggled: Boolean;
begin
  //Change these options only if they changed state since last time
  MusicToggled := (fGame.GlobalSettings.MusicOff <> CheckBox_Options_MusicOff.Checked);
  ShuffleToggled := (fGame.GlobalSettings.ShuffleOn <> CheckBox_Options_ShuffleOn.Checked);

  fGame.GlobalSettings.Autosave         := CheckBox_Options_Autosave.Checked;
  fGame.GlobalSettings.Brightness       := TrackBar_Options_Brightness.Position;
  fGame.GlobalSettings.AlphaShadows     := CheckBox_Options_Shadows.Checked;
  fGame.GlobalSettings.ScrollSpeed      := TrackBar_Options_ScrollSpeed.Position;
  fGame.GlobalSettings.SoundFXVolume    := TrackBar_Options_SFX.Position / TrackBar_Options_SFX.MaxValue;
  fGame.GlobalSettings.MusicVolume      := TrackBar_Options_Music.Position / TrackBar_Options_Music.MaxValue;
  fGame.GlobalSettings.MusicOff         := CheckBox_Options_MusicOff.Checked;
  fGame.GlobalSettings.ShuffleOn        := CheckBox_Options_ShuffleOn.Checked;
  TrackBar_Options_Music.Enabled        := not CheckBox_Options_MusicOff.Checked;
  CheckBox_Options_ShuffleOn.Enabled    := not CheckBox_Options_MusicOff.Checked;

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

  if Sender = Radio_Options_Lang then
  begin
    ShowScreen(msLoading, fTextLibrary[TX_MENU_NEW_LOCALE]);
    fGame.Render; //Force to repaint loading screen
    fGame.ToggleLocale(fLocales[Radio_Options_Lang.ItemIndex].Code);
    exit; //Whole interface will be recreated
  end;
end;


//Apply resolution changes
procedure TKMMainMenuInterface.Options_ChangeRes(Sender: TObject);
var
  I: Integer;
  RefRate: Integer;
  ResID, RefID: Integer;
begin
  if fMain.Resolutions.Count = 0 then Exit;

  DropBox_Options_Resolution.Enabled := CheckBox_Options_FullScreen.Checked;
  DropBox_Options_RefreshRate.Enabled := CheckBox_Options_FullScreen.Checked;

  //Repopulate RefreshRates list
  if Sender = DropBox_Options_Resolution then
  begin
    ResID := DropBox_Options_Resolution.ItemIndex;
    RefID := DropBox_Options_RefreshRate.ItemIndex;

    //Remember refresh rate (we will try to reuse it below)
    RefRate := fMain.Resolutions.Items[ResID].RefRate[RefID];

    //Reset refresh rates, because they are different for each resolution
    DropBox_Options_RefreshRate.Clear;
    for I := 0 to fMain.Resolutions.Items[ResID].RefRateCount - 1 do
    begin
      DropBox_Options_RefreshRate.Add(Format('%d Hz', [fMain.Resolutions.Items[ResID].RefRate[I]]));
      //Make sure to select something
      if (I = 0) or (fMain.Resolutions.Items[ResID].RefRate[I] = RefRate) then
        DropBox_Options_RefreshRate.ItemIndex := I;
    end;
  end;

  //Make button enabled only if new resolution/mode differs from old
  ResID := DropBox_Options_Resolution.ItemIndex;
  RefID := DropBox_Options_RefreshRate.ItemIndex;
  Button_Options_ResApply.Enabled :=
      (OldFullScreen <> fMain.Settings.FullScreen) or
      (fMain.Settings.FullScreen and ((OldResolutionID.ResID <> ResID) or
                                      (OldResolutionID.RefID <> RefID)));
end;


procedure TKMMainMenuInterface.Options_ApplyRes(Sender: TObject);
var
  ResID, RefID: Integer;
  NewResolution: TScreenRes;
begin
  if fMain.Resolutions.Count = 0 then Exit;

  fMain.Settings.FullScreen := CheckBox_Options_FullScreen.Checked;

  ResID := DropBox_Options_Resolution.ItemIndex;
  RefID := DropBox_Options_RefreshRate.ItemIndex;
  NewResolution.Width := fMain.Resolutions.Items[ResID].Width;
  NewResolution.Height := fMain.Resolutions.Items[ResID].Height;
  NewResolution.RefRate := fMain.Resolutions.Items[ResID].RefRate[RefID];

  fMain.Settings.Resolution := NewResolution;
  fMain.ReinitRender(True);
end;


procedure TKMMainMenuInterface.Options_FlagClick(Sender: TObject);
begin
  Assert(Sender is TKMImage);
  Radio_Options_Lang.ItemIndex := TKMImage(Sender).Tag;
  Options_Change(Radio_Options_Lang);
end;


//Resets dropboxes, they will have correct values
procedure TKMMainMenuInterface.Options_Refresh_DropBoxes;
var I: Integer; R: TResIndex;
begin
  DropBox_Options_Resolution.Clear;
  DropBox_Options_RefreshRate.Clear;

  R := fMain.Resolutions.GetResolutionIDs(fMain.Settings.Resolution);

  if fMain.Resolutions.Count > 0 then
  begin
    for I := 0 to fMain.Resolutions.Count - 1 do
    begin
      DropBox_Options_Resolution.Add(Format('%dx%d', [fMain.Resolutions.Items[I].Width, fMain.Resolutions.Items[I].Height]));
      if (I = 0) or (I = R.ResID) then
        DropBox_Options_Resolution.ItemIndex := I;
    end;

    for I := 0 to fMain.Resolutions.Items[R.ResID].RefRateCount - 1 do
    begin
      DropBox_Options_RefreshRate.Add(Format('%d Hz', [fMain.Resolutions.Items[R.ResID].RefRate[I]]));
      if (I = 0) or (I = R.RefID) then
        DropBox_Options_RefreshRate.ItemIndex := I;
    end;
  end
  else
  begin
    //no supported resolutions
    //TODO: String "Not supported" should be moved to text library
    //and translated to all languages
    DropBox_Options_Resolution.Add('Not supported');
    DropBox_Options_RefreshRate.Add('Not supported');
    DropBox_Options_Resolution.ItemIndex := 0;
    DropBox_Options_RefreshRate.ItemIndex := 0;
  end;

  CheckBox_Options_FullScreen.Checked := fMain.Settings.FullScreen;
  //Controls should be disabled, when there is no resolution to choose
  CheckBox_Options_FullScreen.Enabled := fMain.Resolutions.Count > 0;
  DropBox_Options_Resolution.Enabled  := (fMain.Settings.FullScreen) and (fMain.Resolutions.Count > 0);
  DropBox_Options_RefreshRate.Enabled := (fMain.Settings.FullScreen) and (fMain.Resolutions.Count > 0);

  OldFullScreen   := fMain.Settings.FullScreen;
  OldResolutionID := R;
  Button_Options_ResApply.Disable;
end;


procedure TKMMainMenuInterface.KeyDown(Key:Word; Shift: TShiftState);
begin
  if fMyControls.KeyDown(Key, Shift) then exit; //Handled by Controls
end;


procedure TKMMainMenuInterface.KeyUp(Key:Word; Shift: TShiftState);
begin
  if fMyControls.KeyUp(Key, Shift) then exit; //Handled by Controls
end;


//Do something related to mouse movement in menu
procedure TKMMainMenuInterface.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  inherited;

  if (Panel_Campaign.Visible)
  and (Y > Panel_Campaign.Top + Panel_Campaign.Height - Panel_CampScroll.Height) then
    if X < Panel_Campaign.Left +  Panel_CampScroll.Width then
      Panel_CampScroll.Left := Panel_Campaign.Width - Panel_CampScroll.Width
    else
    if X > Panel_Campaign.Left +  Panel_Campaign.Width - Panel_CampScroll.Width then
      Panel_CampScroll.Left := 0;
end;


procedure TKMMainMenuInterface.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  fMyControls.MouseUp(X,Y,Shift,Button);
  exit; //We could have caused fGame reinit, so exit at once
end;


procedure TKMMainMenuInterface.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);
begin
  fMyControls.MouseWheel(X, Y, WheelDelta);
end;


{Should update anything we want to be updated, obviously}
procedure TKMMainMenuInterface.UpdateState;
begin
  //
end;


end.
