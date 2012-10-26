unit KM_InterfaceMainMenu;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  {$IFDEF WDC} ShellAPI, {$ENDIF} //Required for OpenURL in Delphi
  {$IFDEF FPC} LCLIntf, {$ENDIF} //Required for OpenURL in Lazarus
  StrUtils, SysUtils, KromUtils, KromOGLUtils, Math, Classes, Forms, Controls,
  KM_Controls, KM_Defaults, KM_Settings, KM_Maps, KM_Campaigns, KM_Saves, KM_Pics,
  KM_InterfaceDefaults, KM_Minimap, KM_ServerQuery;


type
  TMenuScreen = (msError, msLoading, msMain, msOptions, msResults, msResultsMP);


  TKMMainMenuInterface = class (TKMUserInterface)
  private
    Campaign_Selected: TKMCampaign;
    Campaign_MapIndex: Byte;

    fServerSelected: Boolean;
    fSelectedRoomInfo: TKMRoomInfo;
    fSelectedServerInfo: TKMServerInfo;

    fMaps: TKMapsCollection;
    fMapsMP: TKMapsCollection;
    fSaves: TKMSavesCollection;
    fSavesMP: TKMSavesCollection;
    fMinimap: TKMMinimap;

    fLobbyBusy: Boolean;
    fGameResultMsg: TGameResultMsg; //So we know where to go after results screen
    fWaresVisible:array[WARE_MIN..WARE_MAX] of Boolean; //For MP results page

    fLastMapCRC: Cardinal; //CRC of selected map
    fLastSaveCRC: Cardinal; //CRC of selected save

    //We remember old values to enable "Apply" button dynamicaly
    OldResolutionID: TResIndex;
    SelectedRefRate: Integer;

    procedure Create_MainMenu_Page;
    procedure Create_SinglePlayer_Page;
    procedure Create_CampSelect_Page;
    procedure Create_Campaign_Page;
    procedure Create_SingleMap_Page;
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
    procedure MainMenu_MultiplayerClick(Sender: TObject);
    procedure MainMenu_PlayTutorial(Sender: TObject);
    procedure MainMenu_PlayBattle(Sender: TObject);
    procedure Results_RepeatLastMap(Sender: TObject);
    procedure Campaign_FillList;
    procedure Campaign_ListChange(Sender: TObject);
    procedure Campaign_Set(aCampaign: TKMCampaign);
    procedure Campaign_SelectMap(Sender: TObject);
    procedure Campaign_StartMap(Sender: TObject);
    procedure Credits_LinkClick(Sender: TObject);

    procedure SingleMap_Clear;
    procedure SingleMap_ScanUpdate(Sender: TObject);
    procedure SingleMap_SortUpdate(Sender: TObject);
    procedure SingleMap_RefreshList(aJumpToSelected:Boolean);
    procedure SingleMap_ListClick(Sender: TObject);
    procedure SingleMap_Start(Sender: TObject);
    procedure SingleMap_Sort(aColumn: Integer);

    procedure MP_Init(Sender: TObject);
    procedure MP_BindEvents;
    procedure MP_SaveSettings;
    procedure MP_Update(const aStatus: string; aColor: TColor4; aBusy: Boolean);
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
    function MP_ValidatePlayerName(const aName: string): Boolean;
    function MP_GetInEnabled: Boolean;
    procedure MP_Join(aServerAddress, aPort: string; aRoom: Integer);
    procedure MP_JoinSuccess(Sender: TObject);
    procedure MP_JoinFail(const aData: string);
    procedure MP_JoinAssignedHost(Sender: TObject);
    procedure MP_HostClick(Sender: TObject);
    procedure MP_HostFail(const aData: string);
    procedure MP_BackClick(Sender: TObject);

    procedure Lobby_Reset(Sender: TObject; aPreserveMessage: Boolean = False; aPreserveMaps: Boolean = False);
    procedure Lobby_GameOptionsChange(Sender: TObject);
    procedure Lobby_OnGameOptions(Sender: TObject);
    procedure Lobby_PlayersSetupChange(Sender: TObject);
    procedure Lobby_OnPlayersSetup(Sender: TObject);
    procedure Lobby_OnPingInfo(Sender: TObject);
    procedure Lobby_MapColumnClick(aValue: Integer);
    procedure Lobby_MapTypeSelect(Sender: TObject);
    procedure Lobby_SortUpdate(Sender: TObject);
    procedure Lobby_ScanUpdate(Sender: TObject);
    procedure Lobby_RefreshMapList(aJumpToSelected:Boolean);
    procedure Lobby_RefreshSaveList(aJumpToSelected:Boolean);
    procedure Lobby_MapSelect(Sender: TObject);
    procedure Lobby_OnMapName(const aData:string);
    procedure Lobby_OnReassignedToHost(Sender: TObject);
    procedure Lobby_PostKey(Sender: TObject; Key: Word);
    procedure Lobby_OnMessage(const aData:string);
    procedure Lobby_OnDisconnect(const aData:string);
    procedure Lobby_BackClick(Sender: TObject);
    procedure Lobby_StartClick(Sender: TObject);

    procedure Load_Click(Sender: TObject);
    procedure Load_Delete_Click(Sender: TObject);
    procedure Load_ListClick(Sender: TObject);
    procedure Load_ScanUpdate(Sender: TObject);
    procedure Load_SortUpdate(Sender: TObject);
    procedure Load_RefreshList(aJumpToSelected:Boolean);
    procedure Load_Sort(aIndex: Integer);
    procedure Load_DeleteConfirmation(aVisible:boolean);

    procedure Replays_ListClick(Sender: TObject);
    procedure Replay_TypeChange(Sender: TObject);
    procedure Replays_ScanUpdate(Sender: TObject);
    procedure Replays_SortUpdate(Sender: TObject);
    procedure Replays_RefreshList(aJumpToSelected:Boolean);
    procedure Replays_Sort(aIndex: Integer);
    procedure Replays_Play(Sender: TObject);

    procedure MapEditor_Start(Sender: TObject);
    procedure MapEditor_MapTypeChange(Sender: TObject);
    procedure MapEditor_ListUpdate;
    procedure MapEditor_ScanUpdate(Sender: TObject);
    procedure MapEditor_SortUpdate(Sender: TObject);
    procedure MapEditor_RefreshList(aJumpToSelected:Boolean);
    procedure MapEditor_ColumnClick(aValue: Integer);
    procedure MapEditor_SelectMap(Sender: TObject);

    procedure Options_Fill(aMainSettings: TMainSettings; aGameSettings: TGameSettings);
    procedure Options_Change(Sender: TObject);
    procedure Options_ChangeRes(Sender: TObject);
    procedure Options_ApplyRes(Sender: TObject);
    procedure Options_FlagClick(Sender: TObject);
    procedure Options_Refresh_DropBoxes;

    procedure Results_GraphToggle(Sender: TObject);
    procedure ResultsMP_Toggle(Sender: TObject);
    procedure ResultsMP_PlayerSelect(Sender: TObject);
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
      Button_SP_Camp,
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
        Image_LobbyReady:array [0..MAX_PLAYERS-1] of TKMImage;
        Label_LobbyPing:array [0..MAX_PLAYERS-1] of TKMLabel;

      Panel_LobbySetup:TKMPanel;
        CheckBox_LobbyHostControl: TKMCheckBox;
        Label_LobbyChooseMap: TKMLabel;
        Radio_LobbyMapType: TKMRadioGroup;
        DropCol_LobbyMaps: TKMDropColumns;
        Label_LobbyMapName: TKMLabel;
        Memo_LobbyMapDesc: TKMMemo;
        TrackBar_LobbyPeacetime: TKMTrackBar;
        MinimapView_Lobby: TKMMinimapView;

      Memo_LobbyPosts:TKMMemo;
      Label_LobbyPost:TKMLabel;
      Edit_LobbyPost:TKMEdit;

      Button_LobbyBack:TKMButton;
      Button_LobbyStart:TKMButton;

    Panel_CampSelect: TKMPanel;
      List_Camps: TKMColumnListBox;
      Image_CampsPreview: TKMImage;
      Button_Camp_Start, Button_Camp_Back: TKMButton;

    Panel_Campaign:TKMPanel;
      Image_CampaignBG:TKMImage;
      Panel_Campaign_Flags:TKMPanel;
        Image_CampaignFlags:array[0..MAX_CAMP_MAPS - 1] of TKMImage;
        Image_CampaignSubNode:array[0..MAX_CAMP_NODES - 1] of TKMImage;
      Panel_CampScroll:TKMPanel;
        Image_ScrollTop,Image_Scroll:TKMImage;
        Label_CampaignTitle,Label_CampaignText:TKMLabel;
      Button_CampaignStart,Button_CampaignBack:TKMButton;
    Panel_Single:TKMPanel;
      Panel_SingleDesc: TKMPanel;
        Label_SingleTitle: TKMLabel;
        Memo_SingleDesc: TKMMemo;
        MinimapView_Single: TKMMinimapView;
        Label_SingleCondTyp,Label_SingleCondWin,Label_SingleCondDef: TKMLabel;
        Label_SingleAllies,Label_SingleEnemies: TKMLabel;
      ColList_SingleMaps: TKMColumnListBox;
      {Panel_SingleList: TKMPanel;
        Button_SingleHeadMode,Button_SingleHeadTeams,Button_SingleHeadTitle,Button_SingleHeadSize:TKMButton;
        Bevel_SingleBG: array of array[1..4]of TKMBevel;
        Image_SingleMode: array of TKMImage;
        Label_SinglePlayers,Label_SingleSize,
        Label_SingleTitle1,Label_SingleTitle2: array of TKMLabel;
        Shape_SingleOverlay: array of TKMShape;
        ScrollBar_SingleMaps:TKMScrollBar;
        Shape_SingleMap:TKMShape;}
      Button_SingleBack,Button_SingleStart:TKMButton;
    Panel_Load:TKMPanel;
      List_Load: TKMColumnListBox;
      Button_Load: TKMButton;
      Button_Delete: TKMButton;
      Label_DeleteConfirm: TKMLabel;
      Button_DeleteYes, Button_DeleteNo: TKMButton;
      Button_LoadBack:TKMButton;
      MinimapView_Load: TKMMinimapView;
    Panel_Replays:TKMPanel;
      Radio_Replays_Type:TKMRadioGroup;
      List_Replays: TKMColumnListBox;
      Button_ReplaysPlay: TKMButton;
      Button_ReplaysBack:TKMButton;
      MinimapView_Replay: TKMMinimapView;
    Panel_MapEd: TKMPanel;
      Panel_MapEd_SizeXY: TKMPanel;
      Radio_MapEd_SizeX,Radio_MapEd_SizeY: TKMRadioGroup;
      Panel_MapEd_Load: TKMPanel;
      List_MapEd: TKMColumnListBox;
      Radio_MapEd_MapType: TKMRadioGroup;
      MinimapView_MapEd: TKMMinimapView;
      Button_MapEdBack,Button_MapEd_Create,Button_MapEd_Load: TKMButton;
    Panel_Options:TKMPanel;
      Panel_Options_GFX:TKMPanel;
        TrackBar_Options_Brightness:TKMTrackBar;
        RadioGroup_Options_Shadows:TKMRadioGroup;
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
      Button_CreditsHomepage,
      Button_CreditsFacebook,
      Button_CreditsBack:TKMButton;
    Panel_Loading:TKMPanel;
      Label_Loading:TKMLabel;
    Panel_Error:TKMPanel;
      Label_Error:TKMLabel;
      Button_ErrorBack:TKMButton;
    Panel_Results: TKMPanel;
      Label_Results: TKMLabel;
      Panel_Stats: TKMPanel;
        Label_Stat: array[1..9]of TKMLabel;
      Panel_StatsCharts: TKMPanel;
        Button_ResultsArmy,
        Button_ResultsCitizens,
        Button_ResultsHouses,
        Button_ResultsWares: TKMButtonFlat;
        Graph_Army: TKMGraph;
        Graph_Citizens: TKMGraph;
        Graph_Houses: TKMGraph;
        Graph_Wares: TKMGraph;
      Button_ResultsBack,Button_ResultsRepeat,Button_ResultsContinue: TKMButton;
    Panel_ResultsMP:TKMPanel;
      Button_MPResultsStats,
      Button_MPResultsArmy,
      Button_MPResultsEconomy,
      Button_MPResultsWares: TKMButtonFlat;
      Label_ResultsMP: TKMLabel;
      Panel_StatsMP1, Panel_StatsMP2: TKMPanel;
        Label_ResultsPlayerName1, Label_ResultsPlayerName2:array[0..MAX_PLAYERS-1] of TKMLabel;
        Bar_Results:array[0..MAX_PLAYERS-1, 0..9] of TKMPercentBar;
        Image_ResultsRosette:array[0..MAX_PLAYERS-1, 0..9] of TKMImage;
      Panel_GraphsMP: TKMPanel;
        Graph_MPArmy: TKMGraph;
        Graph_MPCitizens: TKMGraph;
        Graph_MPHouses: TKMGraph;
        Graph_MPWares: array[0..MAX_PLAYERS-1] of TKMGraph; //One for each player
        Image_MPResultsBackplate: TKMImage;
        Radio_MPResultsWarePlayer: TKMRadioGroup;
      Button_ResultsMPBack:TKMButton;
  public
    constructor Create(X,Y: Word);
    destructor Destroy; override;
    procedure ShowScreen(aScreen: TMenuScreen; const aText: string = ''; aMsg: TGameResultMsg=gr_Silent);
    procedure AppendLoadingText(const aText: string);
    procedure Results_Fill(aMsg: TGameResultMsg=gr_Silent);
    procedure ResultsMP_Fill(aMsg: TGameResultMsg=gr_Silent);
    function GetChatText: string;
    function GetChatMessages: string;

    procedure KeyDown(Key:Word; Shift: TShiftState); override;
    procedure KeyUp(Key:Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;

    procedure Resize(X,Y: Word); override;
    procedure UpdateState(aTickCount: Cardinal); override;
  end;


implementation
uses KM_Main, KM_NetworkTypes, KM_TextLibrary, KM_Game, KM_GameApp, KM_PlayersCollection, KM_Locales,
  KM_Utils, KM_Log, KM_Sound, KM_Networking, KM_Resource, KM_Player, KM_CommonTypes;

const
  MENU_SP_MAPS_COUNT    = 12;           //Number of single player maps to display in menu
  MENU_SP_MAPS_HEIGHT   = 40;

  MAPSIZES_COUNT = 15;
  MapSize: array [1..MAPSIZES_COUNT] of Word = (32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 240, 256);


{ TKMMainMenuInterface }
constructor TKMMainMenuInterface.Create(X,Y: Word);
var S: TKMShape;
begin
  inherited;
  Assert(fTextLibrary <> nil, 'fTextLibrary should be initialized before MainMenuInterface');

  Campaign_MapIndex := 1;

  fMinimap := TKMMinimap.Create(True, False, True);

  fMaps := TKMapsCollection.Create(False);
  fMapsMP := TKMapsCollection.Create(True);
  fSaves := TKMSavesCollection.Create;
  fSavesMP := TKMSavesCollection.Create;

  Panel_Main := TKMPanel.Create(fMyControls, 0,
                                             0,
                                             MENU_DESIGN_X,
                                             MENU_DESIGN_Y); //Parent Panel for whole menu

  //Background is the same for all pages, except Results/Campaign, which will render ontop
  with TKMImage.Create(Panel_Main,-448,-216,960,600,17,rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Main, 512,-216,960,600,18,rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Main,-448, 384,960,600,19,rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Main, 512, 384,960,600,20,rxGuiMain) do Anchors := [];

  Create_MainMenu_Page;
  Create_SinglePlayer_Page;
    Create_CampSelect_Page;
      Create_Campaign_Page;
    Create_SingleMap_Page;
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
    S := TKMShape.Create(Panel_Main, 0, 96, 1024, 576);
    S.LineColor := $FF00FFFF;
    S.LineWidth := 1;
    S.Hitable := False;
    S := TKMShape.Create(Panel_Main, 0, 0, 1024, 768);
    S.LineColor := $FF00FF00;
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
  fMinimap.Free;
  inherited;
end;


//Keep Panel_Main centered
procedure TKMMainMenuInterface.Resize(X, Y: Word);
var I: Integer;
begin
  Panel_Main.Width  := Min(X, MENU_DESIGN_X);
  Panel_Main.Height := Min(Y, MENU_DESIGN_Y);

  Panel_Main.Left := (X - Panel_Main.Width) div 2;
  Panel_Main.Top  := (Y - Panel_Main.Height) div 2;

  //Special rules for resizing the campaigns panel
  Panel_Campaign_Flags.Scale := Min(768,Y) / 768;
  Panel_Campaign_Flags.Left := Round(1024*(1-Panel_Campaign_Flags.Scale) / 2);
  Image_CampaignBG.Left := Round(1024*(1-Panel_Campaign_Flags.Scale) / 2);
  Image_CampaignBG.Height := Min(768,Y);
  Image_CampaignBG.Width := Round(1024*Panel_Campaign_Flags.Scale);
  //Special rule to keep campaign flags pivoted at the right place (so the flagpole doesn't move when you resize)
  if Campaign_Selected <> nil then
    for I := 0 to Campaign_Selected.MapCount - 1 do
      with Image_CampaignFlags[I] do
      begin
        //Pivot flags around Y=bottom X=middle, that's where the flag pole is
        Left := Campaign_Selected.Maps[I].Flag.X - Round((Width/2)*(1-Panel_Campaign_Flags.Scale));
        Top  := Campaign_Selected.Maps[I].Flag.Y - Round(Height   *(1-Panel_Campaign_Flags.Scale));
      end;
end;


procedure TKMMainMenuInterface.ShowScreen(aScreen: TMenuScreen; const aText: string=''; aMsg: TGameResultMsg=gr_Silent);
begin
  case aScreen of
    msError:     begin
                   Label_Error.Caption := aText;
                   SwitchMenuPage(Panel_Error);
                 end;
    msLoading:   begin
                   Label_Loading.Caption := aText;
                   SwitchMenuPage(Panel_Loading);
                 end;
    msMain:      SwitchMenuPage(nil);
    msOptions:   SwitchMenuPage(Button_MM_Options);
    msResults:   begin
                   //Restart button is hidden if you won or if it is a replay
                   Button_ResultsRepeat.Visible := not (aMsg in [gr_ReplayEnd, gr_Win]);

                   //Even if the campaign is complete Player can now return to it's screen to replay any of the maps
                   Button_ResultsContinue.Visible := (fGameApp.Campaigns.ActiveCampaign <> nil) and (aMsg <> gr_ReplayEnd);
                   Button_ResultsContinue.Enabled := aMsg = gr_Win;

                   SwitchMenuPage(Panel_Results);
                 end;
    msResultsMP: SwitchMenuPage(Panel_ResultsMP);
  end;
end;


procedure TKMMainMenuInterface.AppendLoadingText(const aText: string);
begin
  Label_Loading.Caption := Label_Loading.Caption + aText + '|';
end;


function TKMMainMenuInterface.GetChatText: string;
begin
  Result := Edit_LobbyPost.Text;
end;


function TKMMainMenuInterface.GetChatMessages: string;
begin
  Result := Memo_LobbyPosts.Text;
end;


procedure TKMMainMenuInterface.Results_Fill(aMsg: TGameResultMsg=gr_Silent);
var
  TempGraphCount: Integer;
  TempGraphs:array[0..MAX_PLAYERS-1] of record
                                          Color: Cardinal;
                                          G: TCardinalArray;
                                        end;

  procedure AddToTempGraph(aColor:Cardinal; aGraph:TCardinalArray);
  var I, ID: Integer;
  begin
    ID := -1;
    for I:=0 to TempGraphCount-1 do
      if aColor = TempGraphs[I].Color then
      begin
        ID := I;
        break;
      end;
    if ID = -1 then
    begin
      ID := TempGraphCount;
      inc(TempGraphCount);
      TempGraphs[ID].G := aGraph; //Overwrite existing graph
    end
    else
      for I:=0 to Length(aGraph)-1 do
        inc(TempGraphs[ID].G[I], aGraph[I]); //Add each element to the existing elements
    TempGraphs[ID].Color := aColor;
  end;

var
  I: Integer;
  R: TResourceType;
  G: TCardinalArray;
begin
  fGameResultMsg := aMsg;
  case aMsg of
    gr_Win:       Label_Results.Caption := fTextLibrary[TX_MENU_MISSION_VICTORY];
    gr_Defeat:    Label_Results.Caption := fTextLibrary[TX_MENU_MISSION_DEFEAT];
    gr_Cancel:    Label_Results.Caption := fTextLibrary[TX_MENU_MISSION_CANCELED];
    gr_ReplayEnd: Label_Results.Caption := fTextLibrary[TX_MENU_REPLAY_ENDED];
    else          Label_Results.Caption := '<<<LEER>>>'; //Thats string used in all Synetic games for missing texts =)
  end;
  //Append mission name and time after the result message
  Label_Results.Caption := Label_Results.Caption + ' - ' + fGame.GameName; //Don't show the mission time in SP because it's already shown elsewhere

  if (MyPlayer = nil) or (MyPlayer.Stats = nil) then Exit;

  //Fill in table values (like old KaM did)
  with MyPlayer.Stats do
  begin
    Label_Stat[1].Caption := IntToStr(GetCitizensLost + GetWarriorsLost);
    Label_Stat[2].Caption := IntToStr(GetCitizensKilled + GetWarriorsKilled);
    Label_Stat[3].Caption := IntToStr(GetHousesLost);
    Label_Stat[4].Caption := IntToStr(GetHousesDestroyed);
    Label_Stat[5].Caption := IntToStr(GetHousesBuilt);
    Label_Stat[6].Caption := IntToStr(GetCitizensTrained);
    Label_Stat[7].Caption := IntToStr(GetWeaponsProduced);
    Label_Stat[8].Caption := IntToStr(GetWarriorsTrained);
    Label_Stat[9].Caption := TimeToString(fGame.MissionTime);
  end;

  //Fill in chart values
  if DISPLAY_CHARTS_RESULT then
  begin
    Graph_Army.Clear;
    Graph_Citizens.Clear;
    Graph_Houses.Clear;
    Graph_Wares.Clear;
    Graph_Army.MaxLength      := MyPlayer.Stats.GraphCount;
    Graph_Citizens.MaxLength  := MyPlayer.Stats.GraphCount;
    Graph_Houses.MaxLength    := MyPlayer.Stats.GraphCount;
    Graph_Wares.MaxLength     := MyPlayer.Stats.GraphCount;

    Graph_Army.MaxTime      := fGame.GameTickCount div 10;
    Graph_Citizens.MaxTime  := fGame.GameTickCount div 10;
    Graph_Houses.MaxTime    := fGame.GameTickCount div 10;
    Graph_Wares.MaxTime     := fGame.GameTickCount div 10;

    //Army
    TempGraphCount := 0; //Reset
    for I := 0 to fPlayers.Count - 1 do
    with fPlayers[I] do
      if PlayerType = pt_Computer then
        AddToTempGraph(FlagColor, Stats.GraphArmy)
      else
        Graph_Army.AddLine(GetFormattedPlayerName, FlagColor, Stats.GraphArmy);

    for I := 0 to TempGraphCount - 1 do
      Graph_Army.AddLine(Format(fTextLibrary[TX_PLAYER_X], [I+1]), TempGraphs[I].Color, TempGraphs[I].G);

    //Citizens
    TempGraphCount := 0; //Reset
    for I := 0 to fPlayers.Count - 1 do
    with fPlayers[I] do
      if PlayerType = pt_Computer then
        AddToTempGraph(FlagColor, Stats.GraphCitizens)
      else
        Graph_Citizens.AddLine(GetFormattedPlayerName, FlagColor, Stats.GraphCitizens);

    for I := 0 to TempGraphCount - 1 do
      Graph_Citizens.AddLine(Format(fTextLibrary[TX_PLAYER_X], [I+1]), TempGraphs[I].Color, TempGraphs[I].G);

    //Houses
    TempGraphCount := 0; //Reset
    for I := 0 to fPlayers.Count - 1 do
    with fPlayers[I] do
      if PlayerType = pt_Computer then
        AddToTempGraph(FlagColor, Stats.GraphHouses)
      else
        Graph_Houses.AddLine(GetFormattedPlayerName, FlagColor, Stats.GraphHouses);

    for I := 0 to TempGraphCount - 1 do
      Graph_Houses.AddLine(Format(fTextLibrary[TX_PLAYER_X], [I+1]), TempGraphs[I].Color, TempGraphs[I].G);

    for R := WARE_MIN to WARE_MAX do
    begin
      G := MyPlayer.Stats.GraphGoods[R];
      for I := 0 to High(G) do
        if G[I] <> 0 then
        begin
          Graph_Wares.AddLine(fResource.Resources[R].Title, ResourceColor[R] or $FF000000, G);
          Break;
        end;
    end;

    Button_ResultsHouses.Enabled := (fGame.MissionMode = mm_Normal);
    Button_ResultsCitizens.Enabled := (fGame.MissionMode = mm_Normal);
    Button_ResultsWares.Enabled := (fGame.MissionMode = mm_Normal);
    Results_GraphToggle(Button_ResultsArmy);
  end;
end;


procedure TKMMainMenuInterface.Results_GraphToggle(Sender: TObject);
begin
  Graph_Army.Visible := Sender = Button_ResultsArmy;
  Graph_Citizens.Visible := Sender = Button_ResultsCitizens;
  Graph_Houses.Visible := Sender = Button_ResultsHouses;
  Graph_Wares.Visible := Sender = Button_ResultsWares;

  Button_ResultsArmy.Down := Sender = Button_ResultsArmy;
  Button_ResultsCitizens.Down := Sender = Button_ResultsCitizens;
  Button_ResultsHouses.Down := Sender = Button_ResultsHouses;
  Button_ResultsWares.Down := Sender = Button_ResultsWares;
end;


procedure TKMMainMenuInterface.ResultsMP_Toggle(Sender: TObject);
var I: Integer;
begin
  Panel_StatsMP1.Visible := Sender = Button_MPResultsStats;
  Panel_StatsMP2.Visible := Sender = Button_MPResultsStats;

  Panel_GraphsMP.Visible   :=(Sender = Button_MPResultsArmy)
                          or (Sender = Button_MPResultsEconomy)
                          or (Sender = Button_MPResultsWares);
  Graph_MPArmy.Visible     := Sender = Button_MPResultsArmy;
  Graph_MPCitizens.Visible := Sender = Button_MPResultsEconomy;
  Graph_MPHouses.Visible   := Sender = Button_MPResultsEconomy;
  for I:=0 to MAX_PLAYERS-1 do
    Graph_MPWares[I].Visible := (Sender = Button_MPResultsWares) and (Radio_MPResultsWarePlayer.ItemIndex = I);
  Radio_MPResultsWarePlayer.Visible := Sender = Button_MPResultsWares;
  Image_MPResultsBackplate.Visible  := Sender = Button_MPResultsWares;

  Button_MPResultsStats.Down := Sender = Button_MPResultsStats;
  Button_MPResultsArmy.Down := Sender = Button_MPResultsArmy;
  Button_MPResultsEconomy.Down := Sender = Button_MPResultsEconomy;
  Button_MPResultsWares.Down := Sender = Button_MPResultsWares;
end;


procedure TKMMainMenuInterface.ResultsMP_PlayerSelect(Sender: TObject);
var ID, I, K: Integer;
begin
  ID := Radio_MPResultsWarePlayer.ItemIndex;
  Assert(ID in [0..MAX_PLAYERS-1]);

  for I:=0 to MAX_PLAYERS-1 do
    if Graph_MPWares[I].Visible then
    begin
      Graph_MPWares[I].Visible := False; //Hide the old one
      //Update the values of which lines are visible in our internal record
      for K := 0 to Graph_MPWares[I].LineCount-1 do
        fWaresVisible[TResourceType(Graph_MPWares[I].Lines[K].Tag)] := Graph_MPWares[I].Lines[K].Visible;
    end;

  Graph_MPWares[ID].Visible := True;
  //Show only the line that are visible in our internal record
  for K := 0 to Graph_MPWares[ID].LineCount-1 do
    Graph_MPWares[ID].SetLineVisible(K,fWaresVisible[TResourceType(Graph_MPWares[ID].Lines[K].Tag)]);
end;


procedure TKMMainMenuInterface.ResultsMP_Fill(aMsg: TGameResultMsg=gr_Silent);
var
  I,K: Integer;
  UnitsMax, HousesMax, GoodsMax, WeaponsMax, MaxValue: Integer;
  Bests: array [0..9] of Cardinal;
  Totals: array [0..9] of Cardinal;
  R: TResourceType;
  G: TCardinalArray;
begin
  fGameResultMsg := aMsg;
  case aMsg of
    gr_Win:       Label_ResultsMP.Caption := fTextLibrary[TX_MENU_MISSION_VICTORY];
    gr_Defeat:    Label_ResultsMP.Caption := fTextLibrary[TX_MENU_MISSION_DEFEAT];
    gr_Cancel:    Label_ResultsMP.Caption := fTextLibrary[TX_MENU_MISSION_CANCELED];
    gr_ReplayEnd: Label_ResultsMP.Caption := fTextLibrary[TX_MENU_REPLAY_ENDED];
    else          Label_ResultsMP.Caption := '<<<LEER>>>'; //Thats string used in all Synetic games for missing texts =)
  end;
  //Append mission name and time after the result message
  Label_ResultsMP.Caption := Label_ResultsMP.Caption + ' - ' + fGame.GameName + ' - ' + TimeToString(fGame.MissionTime);

  //Update visibility depending on players count
  for I := 0 to MAX_PLAYERS - 1 do
  begin
    Label_ResultsPlayerName1[I].Visible := (I < fPlayers.Count);
    Label_ResultsPlayerName2[I].Visible := (I < fPlayers.Count);
    for K := 0 to 9 do
    begin
      Bar_Results[I,K].Visible := (I < fPlayers.Count);
      Image_ResultsRosette[I,K].Visible := (I < fPlayers.Count);
    end;
  end;

  //Update positioning
  Panel_StatsMP1.Height := 40 + fPlayers.Count * 22;
  Panel_StatsMP2.Height := 40 + fPlayers.Count * 22;

  Panel_StatsMP1.Top := 144 + (520 - Panel_StatsMP1.Height * 2) div 2 -
                        (768 - Min(Panel_ResultsMP.Height,768)) div 2; //Manually apply anchoring
  //Second panel does not move from the middle of the screen: results always go above and below the middle

  //Calculate best scores
  ZeroMemory(@Bests, SizeOf(Bests));
  //These are a special case: Less is better so we initialized them high
  Bests[1] := High(Cardinal);
  Bests[3] := High(Cardinal);
  Bests[6] := High(Cardinal);
  ZeroMemory(@Totals, SizeOf(Totals));

  //Calculate bests for each "section"
  for I := 0 to fPlayers.Count - 1 do
    with fPlayers[I].Stats do
    begin
      if Bests[0] < GetCitizensTrained then Bests[0] := GetCitizensTrained;
      if Bests[1] > GetCitizensLost    then Bests[1] := GetCitizensLost;
      if Bests[2] < GetWarriorsTrained then Bests[2] := GetWarriorsTrained;
      if Bests[3] > GetWarriorsLost    then Bests[3] := GetWarriorsLost;
      if Bests[4] < GetWarriorsKilled  then Bests[4] := GetWarriorsKilled;
      if Bests[5] < GetHousesBuilt     then Bests[5] := GetHousesBuilt;
      if Bests[6] > GetHousesLost      then Bests[6] := GetHousesLost;
      if Bests[7] < GetHousesDestroyed then Bests[7] := GetHousesDestroyed;
      if Bests[8] < GetGoodsProduced   then Bests[8] := GetGoodsProduced;
      if Bests[9] < GetWeaponsProduced then Bests[9] := GetWeaponsProduced;

      //If Totals is 0 the category skipped and does not have "Best" icon on it
      inc(Totals[0], GetCitizensTrained);
      inc(Totals[1], GetCitizensLost);
      inc(Totals[2], GetWarriorsTrained);
      inc(Totals[3], GetWarriorsLost);
      inc(Totals[4], GetWarriorsKilled);
      inc(Totals[5], GetHousesBuilt);
      inc(Totals[6], GetHousesLost);
      inc(Totals[7], GetHousesDestroyed);
      inc(Totals[8], GetGoodsProduced);
      inc(Totals[9], GetWeaponsProduced);
    end;

  //Fill in raw values
  for I := 0 to fPlayers.Count - 1 do
  begin
    Label_ResultsPlayerName1[I].Caption   := fPlayers[I].PlayerName;
    Label_ResultsPlayerName1[I].FontColor := FlagColorToTextColor(fPlayers[I].FlagColor);
    Label_ResultsPlayerName2[I].Caption   := fPlayers[I].PlayerName;
    Label_ResultsPlayerName2[I].FontColor := FlagColorToTextColor(fPlayers[I].FlagColor);

    with fPlayers[I].Stats do
    begin
      //Living things
      Bar_Results[I,0].Tag := GetCitizensTrained;
      Bar_Results[I,1].Tag := GetCitizensLost;
      Bar_Results[I,2].Tag := GetWarriorsTrained;
      Bar_Results[I,3].Tag := GetWarriorsLost;
      Bar_Results[I,4].Tag := GetWarriorsKilled;
      Image_ResultsRosette[I,0].Visible := (GetCitizensTrained >= Bests[0]) and (Totals[0] > 0);
      Image_ResultsRosette[I,1].Visible := (GetCitizensLost    <= Bests[1]) and (Totals[1] > 0);
      Image_ResultsRosette[I,2].Visible := (GetWarriorsTrained >= Bests[2]) and (Totals[2] > 0);
      Image_ResultsRosette[I,3].Visible := (GetWarriorsLost    <= Bests[3]) and (Totals[3] > 0);
      Image_ResultsRosette[I,4].Visible := (GetWarriorsKilled >= Bests[4]) and (Totals[4] > 0);
      //Objects
      Bar_Results[I,5].Tag := GetHousesBuilt;
      Bar_Results[I,6].Tag := GetHousesLost;
      Bar_Results[I,7].Tag := GetHousesDestroyed;
      Bar_Results[I,8].Tag := GetGoodsProduced;
      Bar_Results[I,9].Tag := GetWeaponsProduced;
      Image_ResultsRosette[I,5].Visible := (GetHousesBuilt     >= Bests[5]) and (Totals[5] > 0);
      Image_ResultsRosette[I,6].Visible := (GetHousesLost      <= Bests[6]) and (Totals[6] > 0);
      Image_ResultsRosette[I,7].Visible := (GetHousesDestroyed >= Bests[7]) and (Totals[7] > 0);
      Image_ResultsRosette[I,8].Visible := (GetGoodsProduced   >= Bests[8]) and (Totals[8] > 0);
      Image_ResultsRosette[I,9].Visible := (GetWeaponsProduced >= Bests[9]) and (Totals[9] > 0);
    end;
  end;

  //Update percent bars for each category
  UnitsMax := 0;
  for K := 0 to 4 do for I := 0 to fPlayers.Count - 1 do
    UnitsMax := Max(Bar_Results[I,K].Tag, UnitsMax);

  HousesMax := 0;
  for K := 5 to 7 do for I := 0 to fPlayers.Count - 1 do
    HousesMax := Max(Bar_Results[I,K].Tag, HousesMax);

  GoodsMax := 0;
  for I := 0 to fPlayers.Count - 1 do
    GoodsMax := Max(Bar_Results[I,8].Tag, GoodsMax);

  WeaponsMax := 0;
  for I := 0 to fPlayers.Count - 1 do
    WeaponsMax := Max(Bar_Results[I,9].Tag, WeaponsMax);

  //Knowing Max in each category we may fill bars properly
  for K := 0 to 9 do
  begin
    case K of
      0..4: MaxValue := UnitsMax;
      5..7: MaxValue := HousesMax;
      8:    MaxValue := GoodsMax;
      else  MaxValue := WeaponsMax;
    end;
    for I := 0 to fPlayers.Count - 1 do
    begin
      if MaxValue <> 0 then
        Bar_Results[I,K].Position := Bar_Results[I,K].Tag / MaxValue
      else
        Bar_Results[I,K].Position := 0;
      Bar_Results[I,K].Caption := IfThen(Bar_Results[I,K].Tag <> 0, IntToStr(Bar_Results[I,K].Tag), '-');
    end;
  end;

  //Fill in chart values
  if DISPLAY_CHARTS_RESULT then
  begin
    Radio_MPResultsWarePlayer.Items.Clear;
    for I := 0 to fPlayers.Count - 1 do
      Radio_MPResultsWarePlayer.Items.Add('[$'+IntToHex(FlagColorToTextColor(fPlayers[I].FlagColor) and $00FFFFFF,6)+']'+fPlayers[I].PlayerName+'[]');

    Radio_MPResultsWarePlayer.ItemIndex := 0;
    Radio_MPResultsWarePlayer.Height := 25*fPlayers.Count;
    Image_MPResultsBackplate.Height := 24 + 25*fPlayers.Count;

    for R := WARE_MIN to WARE_MAX do
      fWaresVisible[R] := True; //All are visible by default

    Graph_MPArmy.Clear;
    Graph_MPCitizens.Clear;
    Graph_MPHouses.Clear;
    Graph_MPArmy.MaxLength      := MyPlayer.Stats.GraphCount;
    Graph_MPCitizens.MaxLength  := MyPlayer.Stats.GraphCount;
    Graph_MPHouses.MaxLength    := MyPlayer.Stats.GraphCount;

    Graph_MPArmy.MaxTime      := fGame.GameTickCount div 10;
    Graph_MPCitizens.MaxTime  := fGame.GameTickCount div 10;
    Graph_MPHouses.MaxTime    := fGame.GameTickCount div 10;

    for I := 0 to fPlayers.Count - 1 do
    with fPlayers[I] do
      Graph_MPArmy.AddLine(PlayerName, FlagColor, Stats.GraphArmy);

    for I := 0 to fPlayers.Count - 1 do
    with fPlayers[I] do
      Graph_MPCitizens.AddLine(PlayerName, FlagColor, Stats.GraphCitizens);

    for I := 0 to fPlayers.Count - 1 do
    with fPlayers[I] do
      Graph_MPHouses.AddLine(PlayerName, FlagColor, Stats.GraphHouses);

    for I := 0 to fPlayers.Count - 1 do
    begin
      Graph_MPWares[I].Clear;
      Graph_MPWares[I].MaxLength := MyPlayer.Stats.GraphCount;
      Graph_MPWares[I].MaxTime := fGame.GameTickCount div 10;
      Graph_MPWares[I].Caption := fTextLibrary[TX_GRAPH_TITLE_RESOURCES]+' - [$'+IntToHex(FlagColorToTextColor(fPlayers[I].FlagColor) and $00FFFFFF,6)+']'+fPlayers[I].PlayerName+'[]';
      for R := WARE_MIN to WARE_MAX do
      begin
        G := fPlayers[I].Stats.GraphGoods[R];
        for K := 0 to High(G) do
          if G[K] <> 0 then
          begin
            Graph_MPWares[I].AddLine(fResource.Resources[R].Title, ResourceColor[R] or $FF000000, G, Byte(R));
            Break;
          end;
      end;
    end;

    Button_MPResultsWares.Enabled := (fGame.MissionMode = mm_Normal);
    Button_MPResultsEconomy.Enabled := (fGame.MissionMode = mm_Normal);
    ResultsMP_Toggle(Button_MPResultsStats); //Statistics (not graphs) page shown by default every time
  end;
end;


procedure TKMMainMenuInterface.Create_MainMenu_Page;
begin
  //Without anchors this page is centered on resize
  Panel_MainMenu := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_MainMenu.Anchors := [];
    TKMImage.Create(Panel_MainMenu, 300, 120, 423, 164, 4, rxGuiMain);
    TKMLabel.Create(Panel_MainMenu, 512, 300, 'Remake', fnt_Metal, taCenter);

    with TKMImage.Create(Panel_MainMenu,  50, 220, round(218*1.3), round(291*1.3), 5, rxGuiMain) do
      ImageStretch;
    with TKMImage.Create(Panel_MainMenu, 705, 220, round(207*1.3), round(295*1.3), 6, rxGuiMain) do
      ImageStretch;

    Panel_MMButtons := TKMPanel.Create(Panel_MainMenu, 337, 340, 350, 400);
      Button_MM_SinglePlayer := TKMButton.Create(Panel_MMButtons,0,  0,350,30,fTextLibrary[TX_MENU_SINGLEPLAYER],bsMenu);
      Button_MM_MultiPlayer  := TKMButton.Create(Panel_MMButtons,0, 40,350,30,fTextLibrary[TX_MENU_MULTIPLAYER],bsMenu);
      Button_MM_MapEd        := TKMButton.Create(Panel_MMButtons,0, 80,350,30,fTextLibrary[TX_MENU_MAP_EDITOR],bsMenu);
      Button_MM_Replays      := TKMButton.Create(Panel_MMButtons,0,120,350,30,fTextLibrary[TX_MENU_REPLAYS],bsMenu);
      Button_MM_Options      := TKMButton.Create(Panel_MMButtons,0,160,350,30,fTextLibrary[TX_MENU_OPTIONS],bsMenu);
      Button_MM_Credits      := TKMButton.Create(Panel_MMButtons,0,200,350,30,fTextLibrary[TX_MENU_CREDITS],bsMenu);
      Button_MM_Quit         := TKMButton.Create(Panel_MMButtons,0,290,350,30,fTextLibrary[TX_MENU_QUIT],bsMenu);
      Button_MM_SinglePlayer.OnClick := SwitchMenuPage;
      Button_MM_MultiPlayer.OnClick  := MainMenu_MultiplayerClick;
      Button_MM_MapEd.OnClick        := SwitchMenuPage;
      Button_MM_Replays.OnClick      := SwitchMenuPage;
      Button_MM_Options.OnClick      := SwitchMenuPage;
      Button_MM_Credits.OnClick      := SwitchMenuPage;
      Button_MM_Quit.OnClick         := fMain.Stop;
end;


//Single player menu
procedure TKMMainMenuInterface.Create_SinglePlayer_Page;
begin
  //Without anchors this page is centered on resize
  Panel_SinglePlayer := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_SinglePlayer.Anchors := [];
    TKMImage.Create(Panel_SinglePlayer, 300, 120, 423, 164, 4, rxGuiMain);
    TKMLabel.Create(Panel_SinglePlayer, 512, 300, 'Remake', fnt_Metal, taCenter);
    with TKMImage.Create(Panel_SinglePlayer, 50, 220, Round(218 * 1.3), Round(291 * 1.3), 5, rxGuiMain) do ImageStretch;
    with TKMImage.Create(Panel_SinglePlayer, 705, 220, Round(207 * 1.3), Round(295 * 1.3), 6, rxGuiMain) do ImageStretch;

    Panel_SPButtons := TKMPanel.Create(Panel_SinglePlayer,337,340,350,400);
      Button_SP_Tutor  := TKMButton.Create(Panel_SPButtons,0,  0,350,30,fTextLibrary[TX_MENU_TUTORIAL_TOWN],bsMenu);
      Button_SP_Fight  := TKMButton.Create(Panel_SPButtons,0, 40,350,30,fTextLibrary[TX_MENU_TUTORIAL_BATTLE],bsMenu);
      Button_SP_Camp   := TKMButton.Create(Panel_SPButtons,0,100,350,30,fTextLibrary[TX_MENU_CAMPAIGNS],bsMenu);
      Button_SP_Single := TKMButton.Create(Panel_SPButtons,0,160,350,30,fTextLibrary[TX_MENU_SINGLE_MAP],bsMenu);
      Button_SP_Load   := TKMButton.Create(Panel_SPButtons,0,200,350,30,fTextLibrary[TX_MENU_LOAD_SAVEGAME],bsMenu);
      Button_SP_Back   := TKMButton.Create(Panel_SPButtons,0,290,350,30,fTextLibrary[TX_MENU_BACK],bsMenu);

      Button_SP_Tutor.OnClick  := MainMenu_PlayTutorial;
      Button_SP_Fight.OnClick  := MainMenu_PlayBattle;
      Button_SP_Camp.OnClick   := SwitchMenuPage;
      Button_SP_Single.OnClick := SwitchMenuPage;
      Button_SP_Load.OnClick   := SwitchMenuPage;
      Button_SP_Back.OnClick   := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_MultiPlayer_Page;
  procedure CreateServerPopUp;
  begin
    Panel_MPCreateServer := TKMPanel.Create(Panel_Main, 362, 250, 320, 300);
    Panel_MPCreateServer.Anchors := [];
      TKMBevel.Create(Panel_MPCreateServer, -1000,  -1000, 4000, 4000);
      TKMImage.Create(Panel_MPCreateServer, -20, -75, 340, 310, 15, rxGuiMain);
      TKMBevel.Create(Panel_MPCreateServer,   0,  0, 320, 300);
      TKMLabel.Create(Panel_MPCreateServer, 20, 10, 280, 20, fTextLibrary[TX_MP_MENU_CREATE_SERVER_HEADER], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_MPCreateServer, 20, 50, 280, 20, fTextLibrary[TX_MP_MENU_CREATE_SERVER_NAME], fnt_Outline, taLeft);
      Edit_MP_ServerName := TKMEdit.Create(Panel_MPCreateServer, 20, 70, 280, 20, fnt_Grey);
      TKMLabel.Create(Panel_MPCreateServer, 20, 100, 284, 20, fTextLibrary[TX_MP_MENU_CREATE_SERVER_PORT], fnt_Outline, taLeft);
      Edit_MP_ServerPort := TKMEdit.Create(Panel_MPCreateServer, 20, 120, 100, 20, fnt_Grey);
      Edit_MP_ServerPort.AllowedChars := acDigits;
      Button_MP_CreateLAN  := TKMButton.Create(Panel_MPCreateServer, 20, 170, 280, 30, fTextLibrary[TX_MP_MENU_CREATE_SERVER_LOCAL],  bsMenu);
      Button_MP_CreateWAN  := TKMButton.Create(Panel_MPCreateServer, 20, 210, 280, 30, fTextLibrary[TX_MP_MENU_CREATE_SERVER_INTERNET],  bsMenu);
      Button_MP_CreateServerCancel := TKMButton.Create(Panel_MPCreateServer, 20, 250, 280, 30, fTextLibrary[TX_MP_MENU_CREATE_SERVER_CANCEL],  bsMenu);
      Button_MP_CreateLAN.OnClick := MP_HostClick;
      Button_MP_CreateWAN.OnClick := MP_HostClick;
      Button_MP_CreateServerCancel.OnClick := MP_CreateServerCancelClick;
  end;
  procedure FindServerPopUp;
  begin
    Panel_MPFindServer := TKMPanel.Create(Panel_Main, 362, 250, 320, 300);
    Panel_MPFindServer.Anchors := [];
      TKMBevel.Create(Panel_MPFindServer, -1000,  -1000, 4000, 4000);
      TKMImage.Create(Panel_MPFindServer, -20, -75, 340, 310, 15, rxGuiMain);
      TKMBevel.Create(Panel_MPFindServer,   0,  0, 320, 300);
      TKMLabel.Create(Panel_MPFindServer,  20, 10, 280, 20, fTextLibrary[TX_MP_MENU_FIND_SERVER_HEADER], fnt_Outline, taCenter);

      TKMLabel.Create(Panel_MPFindServer, 20, 50, 156, 20, fTextLibrary[TX_MP_MENU_FIND_SERVER_ADDRESS], fnt_Outline, taLeft);
      Edit_MP_FindIP := TKMEdit.Create(Panel_MPFindServer, 20, 70, 152, 20, fnt_Grey);
      Edit_MP_FindIP.AllowedChars := acText; //Server name could be "localhost"
      TKMLabel.Create(Panel_MPFindServer, 172, 50, 60, 20, fTextLibrary[TX_MP_MENU_FIND_SERVER_PORT], fnt_Outline, taLeft);
      Edit_MP_FindPort := TKMEdit.Create(Panel_MPFindServer, 172, 70, 60, 20, fnt_Grey);
      Edit_MP_FindPort.AllowedChars := acDigits;
      TKMLabel.Create(Panel_MPFindServer, 232, 50, 60, 20, fTextLibrary[TX_MP_MENU_FIND_SERVER_ROOM], fnt_Outline, taLeft);
      Edit_MP_FindRoom := TKMEdit.Create(Panel_MPFindServer, 232, 70, 60, 20, fnt_Grey);
      Edit_MP_FindRoom.AllowedChars := acDigits;
      Button_MP_FindServerIP := TKMButton.Create(Panel_MPFindServer, 20, 110, 280, 30, fTextLibrary[TX_MP_MENU_FIND_SERVER_FIND], bsMenu);
      Button_MP_FindServerIP.OnClick := MP_FindServerIPClick;
      Button_MP_FindCancel := TKMButton.Create(Panel_MPFindServer, 20, 150, 280, 30, fTextLibrary[TX_MP_MENU_FIND_SERVER_CANCEL], bsMenu);
      Button_MP_FindCancel.OnClick := MP_FindServerCancelClick;
  end;
begin
  Panel_MultiPlayer := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_MultiPlayer.Stretch;

    //Top area
    Panel_MPPlayerName := TKMPanel.Create(Panel_MultiPlayer, 675, 45, 320, 120);
      TKMBevel.Create(Panel_MPPlayerName, 0, 0, 320, 120);
      TKMLabel.Create(Panel_MPPlayerName, 8, 10, 304, 20, fTextLibrary[TX_MP_MENU_PLAYERNAME], fnt_Outline, taLeft);
      Edit_MP_PlayerName := TKMEdit.Create(Panel_MPPlayerName, 8, 30, 140, 20, fnt_Grey);
      TKMLabel.Create(Panel_MPPlayerName, 8, 60, 304, 20, fTextLibrary[TX_MP_MENU_STATUS], fnt_Outline, taLeft);
      Label_MP_Status := TKMLabel.Create(Panel_MPPlayerName, 8, 80, 304, 36, '', fnt_Grey, taLeft);
      Label_MP_Status.AutoWrap := True;

    Button_MP_CreateServer := TKMButton.Create(Panel_MultiPlayer, 675, 170, 320, 30, fTextLibrary[TX_MP_MENU_CREATE_SERVER], bsMenu);
    Button_MP_CreateServer.OnClick := MP_CreateServerClick;

    CreateServerPopUp;

    Button_MP_FindServer := TKMButton.Create(Panel_MultiPlayer, 675, 204, 320, 30, fTextLibrary[TX_MP_MENU_FIND_SERVER], bsMenu);
    Button_MP_FindServer.OnClick := MP_FindServerClick;

    FindServerPopUp;

    //Master server announcement
    Memo_MP_Announcement := TKMMemo.Create(Panel_MultiPlayer, 45, 45, 620, 189, fnt_Grey, bsMenu);
    Memo_MP_Announcement.Anchors := [akLeft, akTop];
    Memo_MP_Announcement.AutoWrap := True;
    Memo_MP_Announcement.ItemHeight := 16;

    //List of available servers
    ColList_Servers := TKMColumnListBox.Create(Panel_MultiPlayer,45,240,620,465,fnt_Metal, bsMenu);
    ColList_Servers.Anchors := [akLeft, akTop, akBottom];
    ColList_Servers.SetColumns(fnt_Outline, [fTextLibrary[TX_MP_MENU_SERVERLIST_NAME],fTextLibrary[TX_MP_MENU_SERVERLIST_STATE],fTextLibrary[TX_MP_MENU_SERVERLIST_PLAYERS],fTextLibrary[TX_MP_MENU_SERVERLIST_PING]],[0,300,430,525]);
    ColList_Servers.OnColumnClick := MP_ServersSort;
    ColList_Servers.OnChange := MP_ServersClick;
    ColList_Servers.OnDoubleClick := MP_ServersDoubleClick;
    Label_Servers_Status := TKMLabel.Create(Panel_MultiPlayer, 45+310, 240+230, '', fnt_Grey, taCenter);
    Label_Servers_Status.Anchors := [akLeft];
    Label_Servers_Status.Hide;

    //Server details area
    Panel_MPServerDetails := TKMPanel.Create(Panel_MultiPlayer, 675, 240, 320, 465);
    Panel_MPServerDetails.Anchors := [akLeft, akTop, akBottom];
      with TKMBevel.Create(Panel_MPServerDetails, 0, 0, 320, 465) do Stretch;
      TKMLabel.Create(Panel_MPServerDetails, 8, 6, 304, 20, fTextLibrary[TX_MP_MENU_HEADER_SERVER_DETAILS], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_MPServerDetails, 8, 30, 304, 20, fTextLibrary[TX_MP_MENU_GAME_INFORMATION], fnt_Outline, taLeft);
      Label_MP_Map := TKMLabel.Create(Panel_MPServerDetails, 8, 50, 304, 30, '', fnt_Metal, taLeft);
      Label_MP_GameTime := TKMLabel.Create(Panel_MPServerDetails, 8, 70, 304, 30, '--:--:--', fnt_Metal, taLeft);
      TKMLabel.Create(Panel_MPServerDetails, 8, 100, 304, 20, fTextLibrary[TX_MP_MENU_PLAYER_LIST], fnt_Outline, taLeft);
      Label_MP_Players := TKMLabel.Create(Panel_MPServerDetails, 8, 120, 304, 340, '', fnt_Metal, taLeft);
      Label_MP_Players.Anchors := [akLeft, akTop, akBottom];

    Button_MP_Back    := TKMButton.Create(Panel_MultiPlayer,  45, 720, 220, 30, fTextLibrary[TX_MENU_BACK], bsMenu);
    Button_MP_Refresh := TKMButton.Create(Panel_MultiPlayer, 275, 720, 390, 30,fTextLibrary[TX_MP_MENU_REFRESH_SERVER_LIST], bsMenu);
    Button_MP_GetIn   := TKMButton.Create(Panel_MultiPlayer, 675, 720, 320, 30,fTextLibrary[TX_MP_MENU_SERVER_JOIN],  bsMenu);
    Button_MP_Back.Anchors    := [akLeft, akBottom];
    Button_MP_Refresh.Anchors := [akLeft, akBottom];
    Button_MP_GetIn.Anchors   := [akLeft, akBottom];
    Button_MP_Back.OnClick    := MP_BackClick;
    Button_MP_Refresh.OnClick := MP_ServersRefresh;
    Button_MP_GetIn.OnClick   := MP_GetInClick;
end;


procedure TKMMainMenuInterface.Create_Lobby_Page;
const CW = 690; C1 = 35; C2 = 195; C3 = 355; C4 = 445; C5 = 570; C6 = 650;
var i,k,top:integer;
begin
  Panel_Lobby := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_Lobby.Stretch;

    //Server Name
    Panel_LobbyServerName := TKMPanel.Create(Panel_Lobby, 30, 30, CW, 30);
      TKMBevel.Create(Panel_LobbyServerName,   0,  0, CW, 30);
      Label_LobbyServerName := TKMLabel.Create(Panel_LobbyServerName, 10, 10, CW-20, 20, '', fnt_Metal, taLeft);

    //Players
    Panel_LobbyPlayers := TKMPanel.Create(Panel_Lobby, 30, 65, CW, 260);
      TKMBevel.Create(Panel_LobbyPlayers,  0,  0, CW, 260);

      CheckBox_LobbyHostControl := TKMCheckBox.Create(Panel_LobbyPlayers, 10, 10, 450, 20, fTextLibrary[TX_LOBBY_HOST_DOES_SETUP], fnt_Metal);
      CheckBox_LobbyHostControl.OnClick := Lobby_PlayersSetupChange;

      TKMLabel.Create(Panel_LobbyPlayers, C1, 40, 150,  20, fTextLibrary[TX_LOBBY_HEADER_PLAYERS], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, C2, 40, 150,  20, fTextLibrary[TX_LOBBY_HEADER_STARTLOCATION], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, C3, 40,  80,  20, fTextLibrary[TX_LOBBY_HEADER_TEAM], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, C4, 40,  80,  20, fTextLibrary[TX_LOBBY_HEADER_FLAGCOLOR], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, C5, 40, fTextLibrary[TX_LOBBY_HEADER_READY], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_LobbyPlayers, C6, 40, fTextLibrary[TX_LOBBY_HEADER_PING], fnt_Outline, taCenter);

      for i:=0 to MAX_PLAYERS-1 do begin
        top := 60+i*24;
        Image_LobbyFlag[i] := TKMImage.Create(Panel_LobbyPlayers, 10, top+3, 16, 11, 0, rxGuiMain);

        Label_LobbyPlayer[i] := TKMLabel.Create(Panel_LobbyPlayers, C1, top+2, 150, 20, '', fnt_Grey, taLeft);
        Label_LobbyPlayer[i].Hide;

        DropBox_LobbyPlayerSlot[i] := TKMDropList.Create(Panel_LobbyPlayers, C1, top, 150, 20, fnt_Grey, '', bsMenu);
        DropBox_LobbyPlayerSlot[i].Add(fTextLibrary[TX_LOBBY_SLOT_OPEN]); //Player can join into this slot
        DropBox_LobbyPlayerSlot[i].Add(fTextLibrary[TX_LOBBY_SLOT_CLOSED]); //Closed, nobody can join it
        DropBox_LobbyPlayerSlot[i].Add(fTextLibrary[TX_LOBBY_SLOT_AI_PLAYER]); //This slot is an AI player
        DropBox_LobbyPlayerSlot[i].ItemIndex := 0; //Open
        DropBox_LobbyPlayerSlot[i].OnChange := Lobby_PlayersSetupChange;

        DropBox_LobbyLoc[i] := TKMDropList.Create(Panel_LobbyPlayers, C2, top, 150, 20, fnt_Grey, '', bsMenu);
        DropBox_LobbyLoc[i].Add(fTextLibrary[TX_LOBBY_RANDOM]);
        DropBox_LobbyLoc[i].OnChange := Lobby_PlayersSetupChange;

        DropBox_LobbyTeam[i] := TKMDropList.Create(Panel_LobbyPlayers, C3, top, 80, 20, fnt_Grey, '', bsMenu);
        DropBox_LobbyTeam[i].Add('-');
        for k:=1 to 4 do DropBox_LobbyTeam[i].Add(IntToStr(k));
        DropBox_LobbyTeam[i].OnChange := Lobby_PlayersSetupChange;

        Drop_LobbyColors[i] := TKMDropColumns.Create(Panel_LobbyPlayers, C4, top, 80, 20, fnt_Grey, '', bsMenu);
        Drop_LobbyColors[i].SetColumns(fnt_Outline, [''], [0]);
        Drop_LobbyColors[i].List.ShowHeader := False;
        Drop_LobbyColors[i].FadeImageWhenDisabled := False;
        Drop_LobbyColors[i].Add(MakeListRow([''], [$FFFFFFFF], [MakePic(rxGuiMain, 31)], 0));
        for K := Low(MP_TEAM_COLORS) to High(MP_TEAM_COLORS) do
          Drop_LobbyColors[i].Add(MakeListRow([''], [MP_TEAM_COLORS[K]], [MakePic(rxGuiMain, 30)]));
        Drop_LobbyColors[i].OnChange := Lobby_PlayersSetupChange;

        Image_LobbyReady[i] := TKMImage.Create(Panel_LobbyPlayers, C5-8, top, 16, 16, 32, rxGuiMain);
        Label_LobbyPing[i] := TKMLabel.Create(Panel_LobbyPlayers, C6, top, '', fnt_Metal, taCenter);
      end;

    //Chat
    Memo_LobbyPosts := TKMMemo.Create(Panel_Lobby, 30, 330, CW, 320, fnt_Metal, bsMenu);
    Memo_LobbyPosts.AutoWrap := True;
    Memo_LobbyPosts.ScrollDown := True;
    Memo_LobbyPosts.Anchors := [akLeft, akTop, akBottom];
    Label_LobbyPost := TKMLabel.Create(Panel_Lobby, 30, 655, CW, 20, fTextLibrary[TX_LOBBY_POST_WRITE], fnt_Outline, taLeft);
    Label_LobbyPost.Anchors := [akLeft, akBottom];
    Edit_LobbyPost := TKMEdit.Create(Panel_Lobby, 30, 675, CW, 20, fnt_Metal);
    Edit_LobbyPost.OnKeyDown := Lobby_PostKey;
    Edit_LobbyPost.Anchors := [akLeft, akBottom];
    Edit_LobbyPost.ShowColors := True;

    //Setup
    Panel_LobbySetup := TKMPanel.Create(Panel_Lobby, 725, 30, 270, 712);
    Panel_LobbySetup.Anchors := [akLeft, akTop, akBottom];
      with TKMBevel.Create(Panel_LobbySetup,  0,  0, 270, 712) do Stretch;
      Label_LobbyChooseMap := TKMLabel.Create(Panel_LobbySetup, 10, 10, 250, 20, fTextLibrary[TX_LOBBY_MAP_TYPE], fnt_Outline, taLeft);
      Radio_LobbyMapType := TKMRadioGroup.Create(Panel_LobbySetup, 10, 29, 250, 60, fnt_Metal);
      Radio_LobbyMapType.Items.Add(fTextLibrary[TX_LOBBY_MAP_BUILD]);
      Radio_LobbyMapType.Items.Add(fTextLibrary[TX_LOBBY_MAP_FIGHT]);
      Radio_LobbyMapType.Items.Add(fTextLibrary[TX_LOBBY_MAP_COOP]);
      Radio_LobbyMapType.Items.Add(fTextLibrary[TX_LOBBY_MAP_SAVED]);
      Radio_LobbyMapType.ItemIndex := 0;
      Radio_LobbyMapType.OnChange := Lobby_MapTypeSelect;

      DropCol_LobbyMaps := TKMDropColumns.Create(Panel_LobbySetup, 10, 99, 250, 20, fnt_Metal, fTextLibrary[TX_LOBBY_MAP_SELECT], bsMenu);
      DropCol_LobbyMaps.DropCount := 19;
      DropCol_LobbyMaps.DropWidth := 430; //180 extra width
      DropCol_LobbyMaps.SetColumns(fnt_Outline, [fTextLibrary[TX_MENU_MAP_TITLE], '#', fTextLibrary[TX_MENU_MAP_SIZE]], [0, 290, 320]);
      DropCol_LobbyMaps.List.OnColumnClick := Lobby_MapColumnClick;
      DropCol_LobbyMaps.OnChange := Lobby_MapSelect;
      Label_LobbyMapName := TKMLabel.Create(Panel_LobbySetup, 10, 99, 250, 20, '', fnt_Metal, taLeft);

      TKMBevel.Create(Panel_LobbySetup, 35, 124, 199, 199);
      MinimapView_Lobby := TKMMinimapView.Create(Panel_LobbySetup, 39, 128, 191, 191);
      MinimapView_Lobby.ShowLocs := True; //In the minimap we want player locations to be shown

      Memo_LobbyMapDesc := TKMMemo.Create(Panel_LobbySetup, 10, 328, 250, 294, fnt_Game, bsMenu);
      Memo_LobbyMapDesc.Anchors := [akLeft,akTop,akBottom];
      Memo_LobbyMapDesc.AutoWrap := True;
      Memo_LobbyMapDesc.ItemHeight := 16;

      with TKMLabel.Create(Panel_LobbySetup, 10, 626, 250, 20, fTextLibrary[TX_LOBBY_GAME_OPTIONS], fnt_Outline, taLeft) do Anchors := [akLeft,akBottom];
      TrackBar_LobbyPeacetime := TKMTrackBar.Create(Panel_LobbySetup, 10, 648, 250, 0, 120);
      TrackBar_LobbyPeacetime.Anchors := [akLeft,akBottom];
      TrackBar_LobbyPeacetime.Caption := fTextLibrary[TX_LOBBY_PEACETIME];
      TrackBar_LobbyPeacetime.Step := 5; //Round to 5min steps
      TrackBar_LobbyPeacetime.OnChange := Lobby_GameOptionsChange;

    Button_LobbyBack := TKMButton.Create(Panel_Lobby, 30, 712, 230, 30, fTextLibrary[TX_LOBBY_QUIT], bsMenu);
    Button_LobbyBack.Anchors := [akLeft, akBottom];
    Button_LobbyBack.OnClick := Lobby_BackClick;
    Button_LobbyStart := TKMButton.Create(Panel_Lobby, 285, 712, 230, 30, '<<<LEER>>>', bsMenu);
    Button_LobbyStart.Anchors := [akLeft, akBottom];
    Button_LobbyStart.OnClick := Lobby_StartClick;
end;


procedure TKMMainMenuInterface.Create_CampSelect_Page;
var L: TKMLabel;
begin
  Panel_CampSelect := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_CampSelect.Stretch;

    L := TKMLabel.Create(Panel_CampSelect, Panel_Main.Width div 2, 230, fTextLibrary[TX_MENU_CAMP_CUSTOM], fnt_Outline, taCenter);
    L.Anchors := [];
    List_Camps := TKMColumnListBox.Create(Panel_CampSelect, 80, 260, 600, 300, fnt_Grey, bsMenu);
    List_Camps.SetColumns(fnt_Outline, [fTextLibrary[TX_MENU_CAMPAIGNS_TITLE],
                                        fTextLibrary[TX_MENU_CAMPAIGNS_MAPS_COUNT],
                                        fTextLibrary[TX_MENU_CAMPAIGNS_MAPS_UNLOCKED], ''],
                                        [0, 320, 460, 600]);
    List_Camps.Anchors := [];
    List_Camps.OnChange := Campaign_ListChange;
    List_Camps.OnDoubleClick := SwitchMenuPage;

    with TKMBevel.Create(Panel_CampSelect, 696, 306, 275, 208) do Anchors := [];
    Image_CampsPreview := TKMImage.Create(Panel_CampSelect, 700, 310, 267, 200, 0, rxGuiMain);
    Image_CampsPreview.ImageStretch;
    Image_CampsPreview.Anchors := [];

    Button_Camp_Start := TKMButton.Create(Panel_CampSelect, 362, 570, 300, 30, fTextLibrary[TX_MENU_CAMP_START], bsMenu);
    Button_Camp_Start.Anchors := [];
    Button_Camp_Start.OnClick := SwitchMenuPage;

    Button_Camp_Back := TKMButton.Create(Panel_CampSelect, 362, 615, 300, 30, fTextLibrary[TX_MENU_BACK], bsMenu);
    Button_Camp_Back.Anchors := [];
    Button_Camp_Back.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Campaign_Page;
var I: Integer;
begin
  Panel_Campaign := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_Campaign.Stretch;
    Image_CampaignBG := TKMImage.Create(Panel_Campaign,0,0,Panel_Main.Width, Panel_Main.Height,0,rxGuiMain);
    Image_CampaignBG.ImageStretch;

    Panel_Campaign_Flags:=TKMPanel.Create(Panel_Campaign,0,0,Panel_Main.Width, Panel_Main.Height);
    Panel_Campaign_Flags.Stretch;
    for I := 0 to High(Image_CampaignFlags) do
    begin
      Image_CampaignFlags[I] := TKMImage.Create(Panel_Campaign_Flags, Panel_Main.Width, Panel_Main.Height, 23, 29, 10, rxGuiMain);
      Image_CampaignFlags[I].OnClick := Campaign_SelectMap;
      Image_CampaignFlags[I].Tag := I;
    end;
    for I := 0 to High(Image_CampaignSubNode) do
    begin
      Image_CampaignSubNode[I] := TKMImage.Create(Panel_Campaign_Flags, Panel_Main.Width, Panel_Main.Height, 0, 0, 16, rxGuiMain);
      Image_CampaignSubNode[I].ImageCenter; //Pivot at the center of the dot (width/height = 0)
    end;

  Panel_CampScroll := TKMPanel.Create(Panel_Campaign,Panel_Main.Width-360,Panel_Main.Height-430,360,430);
  Panel_CampScroll.Anchors := [akLeft,akBottom];

    Image_Scroll := TKMImage.Create(Panel_CampScroll, 0, 0,360,430,{15}3,rxGuiMain);
    Image_Scroll.ImageStretch;
    Label_CampaignTitle := TKMLabel.Create(Panel_CampScroll, 130, 18,100,20, '<<<LEER>>>', fnt_Outline, taCenter);

    Label_CampaignText := TKMLabel.Create(Panel_CampScroll, 20, 50, 325, 310, '<<<LEER>>>', fnt_Briefing, taLeft);
    Label_CampaignText.AutoWrap := true;

  Button_CampaignStart := TKMButton.Create(Panel_Campaign, Panel_Main.Width-220-20, Panel_Main.Height-50, 220, 30, fTextLibrary[TX_MENU_START_MISSION], bsMenu);
  Button_CampaignStart.Anchors := [akLeft,akBottom];
  Button_CampaignStart.OnClick := Campaign_StartMap;

  Button_CampaignBack := TKMButton.Create(Panel_Campaign, 20, Panel_Main.Height-50, 220, 30, fTextLibrary[TX_MENU_BACK], bsMenu);
  Button_CampaignBack.Anchors := [akLeft,akBottom];
  Button_CampaignBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_SingleMap_Page;
begin
  Panel_Single := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_Single.Stretch;

    ColList_SingleMaps := TKMColumnListBox.Create(Panel_Single, 524, 135, 465, 520, fnt_MainMapGold, bsMenu);
    ColList_SingleMaps.SetColumns(fnt_Outline, ['', '', 'Title', 'Size'], [0, 50, 100, 380]);
    ColList_SingleMaps.Columns[2].Font := fnt_Metal;
    ColList_SingleMaps.Columns[2].HintFont := fnt_Grey;
    ColList_SingleMaps.Columns[1].TextAlign := taCenter;
    ColList_SingleMaps.Columns[3].TextAlign := taCenter;
    ColList_SingleMaps.ItemHeight := 40;
    ColList_SingleMaps.ShowLines := True;
    ColList_SingleMaps.Header.Height := 40;
    ColList_SingleMaps.Header.TextAlign := taCenter;
    ColList_SingleMaps.Header.Columns[0].Glyph := MakePic(rxGui, 42);
    ColList_SingleMaps.Header.Columns[1].Glyph := MakePic(rxGui, 31);

    //ColList_SingleMaps.

    ColList_SingleMaps.OnColumnClick := SingleMap_Sort;
    ColList_SingleMaps.OnChange := SingleMap_ListClick;

    Panel_SingleDesc := TKMPanel.Create(Panel_Single, 45, 135, 445, 520);
    Panel_SingleDesc.Anchors := [];

      Label_SingleTitle := TKMLabel.Create(Panel_SingleDesc,445 div 2,0,'',fnt_Outline, taCenter);
      Memo_SingleDesc  := TKMMemo.Create(Panel_SingleDesc,15,25,415,129,fnt_Metal, bsMenu);
      Memo_SingleDesc.AutoWrap := True;

      TKMBevel.Create(Panel_SingleDesc, 121, 160, 199, 199);
      MinimapView_Single := TKMMinimapView.Create(Panel_SingleDesc, 125, 164, 191, 191);

      TKMBevel.Create(Panel_SingleDesc,0,368,445,20);
      Label_SingleCondTyp:=TKMLabel.Create(Panel_SingleDesc,8,371,429,20,fTextLibrary[TX_MENU_MISSION_TYPE],fnt_Metal, taLeft);
      TKMBevel.Create(Panel_SingleDesc,0,390,445,20);
      Label_SingleCondWin:=TKMLabel.Create(Panel_SingleDesc,8,393,429,20,fTextLibrary[TX_MENU_WIN_CONDITION],fnt_Metal, taLeft);
      TKMBevel.Create(Panel_SingleDesc,0,412,445,20);
      Label_SingleCondDef:=TKMLabel.Create(Panel_SingleDesc,8,415,429,20,fTextLibrary[TX_MENU_DEFEAT_CONDITION],fnt_Metal, taLeft);
      TKMBevel.Create(Panel_SingleDesc,0,434,445,20);
      Label_SingleAllies:=TKMLabel.Create(Panel_SingleDesc,8,437,429,20,fTextLibrary[TX_MENU_ALLIES],fnt_Metal, taLeft);
      TKMBevel.Create(Panel_SingleDesc,0,456,445,20);
      Label_SingleEnemies:=TKMLabel.Create(Panel_SingleDesc,8,459,429,20,fTextLibrary[TX_MENU_ENEMIES],fnt_Metal, taLeft);

    Button_SingleBack := TKMButton.Create(Panel_Single, 45, 625, 220, 30, fTextLibrary[TX_MENU_BACK], bsMenu);
    Button_SingleBack.Anchors := [];
    Button_SingleBack.OnClick := SwitchMenuPage;
    Button_SingleStart := TKMButton.Create(Panel_Single, 270, 625, 220, 30, fTextLibrary[TX_MENU_SINGLE_START_MAP], bsMenu);
    Button_SingleStart.Anchors := [];
    Button_SingleStart.OnClick := SingleMap_Start;
end;


procedure TKMMainMenuInterface.Create_Load_Page;
begin
  Panel_Load := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_Load.Stretch;

    TKMLabel.Create(Panel_Load, Panel_Main.Width div 2, 50, fTextLibrary[TX_MENU_LOAD_LIST], fnt_Outline, taCenter);

    List_Load := TKMColumnListBox.Create(Panel_Load, 62, 86, 700, 485, fnt_Metal, bsMenu);
    List_Load.Anchors := [akLeft,akTop,akBottom];
    List_Load.SetColumns(fnt_Outline, [fTextLibrary[TX_MENU_LOAD_FILE], fTextLibrary[TX_MENU_LOAD_DESCRIPTION]], [0, 300]);
    List_Load.OnColumnClick := Load_Sort;
    List_Load.OnChange := Load_ListClick;
    List_Load.OnDoubleClick := Load_Click;

    Button_Load := TKMButton.Create(Panel_Load,337,590,350,30,fTextLibrary[TX_MENU_LOAD_LOAD], bsMenu);
    Button_Load.Anchors := [akLeft,akBottom];
    Button_Load.OnClick := Load_Click;

    Button_Delete := TKMButton.Create(Panel_Load, 337, 624, 350, 30, fTextLibrary[TX_MENU_LOAD_DELETE], bsMenu);
    Button_Delete.Anchors := [akLeft,akBottom];
    Button_Delete.OnClick := Load_Delete_Click;

    Label_DeleteConfirm := TKMLabel.Create(Panel_Load, Panel_Main.Width div 2, 634, fTextLibrary[TX_MENU_LOAD_DELETE_CONFIRM], fnt_Outline, taCenter);
    Label_DeleteConfirm.Anchors := [akLeft,akBottom];
    Button_DeleteYes := TKMButton.Create(Panel_Load, 337, 660, 170, 30, fTextLibrary[TX_MENU_LOAD_DELETE_DELETE], bsMenu);
    Button_DeleteYes.Anchors := [akLeft,akBottom];
    Button_DeleteYes.OnClick := Load_Delete_Click;
    Button_DeleteNo  := TKMButton.Create(Panel_Load, 517, 660, 170, 30, fTextLibrary[TX_MENU_LOAD_DELETE_CANCEL], bsMenu);
    Button_DeleteNo.Anchors := [akLeft,akBottom];
    Button_DeleteNo.OnClick := Load_Delete_Click;

    Button_LoadBack := TKMButton.Create(Panel_Load, 337, 700, 350, 30, fTextLibrary[TX_MENU_BACK], bsMenu);
    Button_LoadBack.Anchors := [akLeft,akBottom];
    Button_LoadBack.OnClick := SwitchMenuPage;

    with TKMBevel.Create(Panel_Load, 785, 226, 199, 199) do Anchors := [akLeft];
    MinimapView_Load := TKMMinimapView.Create(Panel_Load,789,230,191,191);
    MinimapView_Load.Anchors := [akLeft];
end;


//Should contain options to make a map from scratch, load map from file, generate new one
procedure TKMMainMenuInterface.Create_MapEditor_Page;
var I: Integer;
begin
  Panel_MapEd:=TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_MapEd.Stretch;
    Panel_MapEd_SizeXY := TKMPanel.Create(Panel_MapEd, 80, 160, 200, 400);
    Panel_MapEd_SizeXY.Anchors := [akLeft];
      TKMLabel.Create(Panel_MapEd_SizeXY, 6, 0, 188, 20, fTextLibrary[TX_MENU_NEW_MAP_SIZE], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_MapEd_SizeXY, 0, 20, 200, 370);
      TKMLabel.Create(Panel_MapEd_SizeXY, 8, 27, 88, 20, fTextLibrary[TX_MENU_MAP_WIDTH], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_MapEd_SizeXY, 108, 27, 88, 20, fTextLibrary[TX_MENU_MAP_HEIGHT], fnt_Outline, taLeft);

      Radio_MapEd_SizeX := TKMRadioGroup.Create(Panel_MapEd_SizeXY, 10, 52, 88, 332, fnt_Metal);
      Radio_MapEd_SizeY := TKMRadioGroup.Create(Panel_MapEd_SizeXY, 110, 52, 88, 332, fnt_Metal);
      for I := 1 to MAPSIZES_COUNT do begin
        Radio_MapEd_SizeX.Items.Add(IntToStr(MapSize[I]));
        Radio_MapEd_SizeY.Items.Add(IntToStr(MapSize[I]));
      end;
      Radio_MapEd_SizeX.ItemIndex := 2; //64
      Radio_MapEd_SizeY.ItemIndex := 2; //64

      Button_MapEd_Create := TKMButton.Create(Panel_MapEd_SizeXY, 0, 400, 200, 30, fTextLibrary[TX_MENU_MAP_CREATE_NEW_MAP], bsMenu);
      Button_MapEd_Create.OnClick := MapEditor_Start;

    Panel_MapEd_Load := TKMPanel.Create(Panel_MapEd, 300, 160, 620, 500);
    Panel_MapEd_Load.Anchors := [akLeft];
      TKMLabel.Create(Panel_MapEd_Load, 6, 0, 288, 20, fTextLibrary[TX_MENU_MAP_AVAILABLE], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_MapEd_Load, 0, 20, 300, 50);
      Radio_MapEd_MapType := TKMRadioGroup.Create(Panel_MapEd_Load,8,28,286,40,fnt_Grey);
      Radio_MapEd_MapType.ItemIndex := 0;
      Radio_MapEd_MapType.Items.Add(fTextLibrary[TX_MENU_MAPED_SPMAPS]);
      Radio_MapEd_MapType.Items.Add(fTextLibrary[TX_MENU_MAPED_MPMAPS]);
      Radio_MapEd_MapType.OnChange := MapEditor_MapTypeChange;
      List_MapEd := TKMColumnListBox.Create(Panel_MapEd_Load, 0, 80, 440, 310, fnt_Metal,  bsMenu);
      List_MapEd.SetColumns(fnt_Outline, [fTextLibrary[TX_MENU_MAP_TITLE], '#', fTextLibrary[TX_MENU_MAP_SIZE]], [0, 310, 340]);
      List_MapEd.OnColumnClick := MapEditor_ColumnClick;
      List_MapEd.OnChange := MapEditor_SelectMap;
      List_MapEd.OnDoubleClick := MapEditor_Start;
      Button_MapEd_Load := TKMButton.Create(Panel_MapEd_Load, 0, 400, 300, 30, fTextLibrary[TX_MENU_MAP_LOAD_EXISTING], bsMenu);
      Button_MapEd_Load.OnClick := MapEditor_Start;
      TKMBevel.Create(Panel_MapEd_Load, 448, 80, 199, 199);
      MinimapView_MapEd := TKMMinimapView.Create(Panel_MapEd_Load, 452, 84, 191, 191);

    Button_MapEdBack := TKMButton.Create(Panel_MapEd, 80, 620, 220, 30, fTextLibrary[TX_MENU_BACK], bsMenu);
    Button_MapEdBack.Anchors := [akLeft];
    Button_MapEdBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Replays_Page;
begin
  Panel_Replays := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_Replays.Stretch;

    TKMLabel.Create(Panel_Replays, Panel_Main.Width div 2, 50, fTextLibrary[TX_MENU_LOAD_LIST], fnt_Outline, taCenter);

    TKMBevel.Create(Panel_Replays, 62, 86, 900, 50);
    Radio_Replays_Type := TKMRadioGroup.Create(Panel_Replays,70,94,300,40,fnt_Grey);
    Radio_Replays_Type.ItemIndex := 0;
    Radio_Replays_Type.Items.Add(fTextLibrary[TX_MENU_MAPED_SPMAPS]);
    Radio_Replays_Type.Items.Add(fTextLibrary[TX_MENU_MAPED_MPMAPS]);
    Radio_Replays_Type.OnChange := Replay_TypeChange;

    List_Replays := TKMColumnListBox.Create(Panel_Replays, 62, 150, 700, 485, fnt_Metal, bsMenu);
    List_Replays.SetColumns(fnt_Outline, [fTextLibrary[TX_MENU_LOAD_FILE], fTextLibrary[TX_MENU_LOAD_DESCRIPTION]], [0, 300]);
    List_Replays.Anchors := [akLeft,akTop,akBottom];
    List_Replays.OnChange := Replays_ListClick;
    List_Replays.OnColumnClick := Replays_Sort;
    List_Replays.OnDoubleClick := Replays_Play;

    Button_ReplaysPlay := TKMButton.Create(Panel_Replays,337,660,350,30,fTextLibrary[TX_MENU_VIEW_REPLAY], bsMenu);
    Button_ReplaysPlay.Anchors := [akLeft,akBottom];
    Button_ReplaysPlay.OnClick := Replays_Play;

    Button_ReplaysBack := TKMButton.Create(Panel_Replays, 337, 700, 350, 30, fTextLibrary[TX_MENU_BACK], bsMenu);
    Button_ReplaysBack.Anchors := [akLeft,akBottom];
    Button_ReplaysBack.OnClick := SwitchMenuPage;

    with TKMBevel.Create(Panel_Replays, 785, 290, 199, 199) do Anchors := [akLeft];
    MinimapView_Replay := TKMMinimapView.Create(Panel_Replays,789,294,191,191);
    MinimapView_Replay.Anchors := [akLeft];
end;


procedure TKMMainMenuInterface.Create_Options_Page;
var I: Integer;
begin
  Panel_Options:=TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_Options.Stretch;
    with TKMImage.Create(Panel_Options,705,220,round(207*1.3),round(295*1.3),6,rxGuiMain) do
    begin
      ImageStretch;
      Anchors := [akLeft];
    end;

    //Controls section
    Panel_Options_Ctrl:=TKMPanel.Create(Panel_Options,120,130,220,80);
    Panel_Options_Ctrl.Anchors := [akLeft];
      TKMLabel.Create(Panel_Options_Ctrl,6,0,288,20,fTextLibrary[TX_MENU_OPTIONS_CONTROLS],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Ctrl,0,20,220,60);

      TrackBar_Options_ScrollSpeed := TKMTrackBar.Create(Panel_Options_Ctrl,10,27,180,OPT_SLIDER_MIN,OPT_SLIDER_MAX);
      TrackBar_Options_ScrollSpeed.Caption := fTextLibrary[TX_MENU_OPTIONS_SCROLL_SPEED];
      TrackBar_Options_ScrollSpeed.OnChange := Options_Change;

    //Gameplay section
    Panel_Options_Game:=TKMPanel.Create(Panel_Options,120,230,220,50);
    Panel_Options_Game.Anchors := [akLeft];
      TKMLabel.Create(Panel_Options_Game,6,0,188,20,fTextLibrary[TX_MENU_OPTIONS_GAMEPLAY],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Game,0,20,220,30);

      CheckBox_Options_Autosave := TKMCheckBox.Create(Panel_Options_Game,12,27,196,20,fTextLibrary[TX_MENU_OPTIONS_AUTOSAVE], fnt_Metal);
      CheckBox_Options_Autosave.OnClick := Options_Change;

    //Graphics section
    Panel_Options_GFX:=TKMPanel.Create(Panel_Options,360,300,220,140);
    Panel_Options_GFX.Anchors := [akLeft];
      TKMLabel.Create(Panel_Options_GFX,6,0,188,20,fTextLibrary[TX_MENU_OPTIONS_GRAPHICS],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_GFX,0,20,220,120);
      TrackBar_Options_Brightness:=TKMTrackBar.Create(Panel_Options_GFX,10,27,180,OPT_SLIDER_MIN,OPT_SLIDER_MAX);
      TrackBar_Options_Brightness.Caption := fTextLibrary[TX_MENU_OPTIONS_BRIGHTNESS];
      TrackBar_Options_Brightness.OnChange:=Options_Change;
      TKMLabel.Create(Panel_Options_GFX,10,82,200,20,fTextLibrary[TX_MENU_OPTIONS_SHADOW_QUALITY],fnt_Metal,taLeft);
      RadioGroup_Options_Shadows := TKMRadioGroup.Create(Panel_Options_GFX,10,100,200,32, fnt_Metal);
      RadioGroup_Options_Shadows.Items.Add(fTextLibrary[TX_MENU_OPTIONS_SHADOW_QUALITY_LOW]);
      RadioGroup_Options_Shadows.Items.Add(fTextLibrary[TX_MENU_OPTIONS_SHADOW_QUALITY_HIGH]);
      RadioGroup_Options_Shadows.OnChange := Options_Change;

    //SFX section
    Panel_Options_Sound:=TKMPanel.Create(Panel_Options,120,300,220,167);
    Panel_Options_Sound.Anchors := [akLeft];
      TKMLabel.Create(Panel_Options_Sound,6,0,188,20,fTextLibrary[TX_MENU_OPTIONS_SOUND],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Sound,0,20,220,147);

      TrackBar_Options_SFX       := TKMTrackBar.Create(Panel_Options_Sound, 10, 27, 180, OPT_SLIDER_MIN, OPT_SLIDER_MAX);
      TrackBar_Options_Music     := TKMTrackBar.Create(Panel_Options_Sound, 10, 77, 180, OPT_SLIDER_MIN, OPT_SLIDER_MAX);
      CheckBox_Options_MusicOff  := TKMCheckBox.Create(Panel_Options_Sound, 12, 127, 196, 20, fTextLibrary[TX_MENU_OPTIONS_MUSIC_DISABLE], fnt_Metal);
      CheckBox_Options_ShuffleOn := TKMCheckBox.Create(Panel_Options_Sound, 12, 147, 196, 20, fTextLibrary[TX_MENU_OPTIONS_MUSIC_SHUFFLE], fnt_Metal);
      TrackBar_Options_SFX.Caption   := fTextLibrary[TX_MENU_SFX_VOLUME];
      TrackBar_Options_Music.Caption := fTextLibrary[TX_MENU_MUSIC_VOLUME];
      TrackBar_Options_SFX.OnChange      := Options_Change;
      TrackBar_Options_Music.OnChange    := Options_Change;
      CheckBox_Options_MusicOff.OnClick  := Options_Change;
      CheckBox_Options_ShuffleOn.OnClick := Options_Change;

    //Resolutions section
    Panel_Options_Res := TKMPanel.Create(Panel_Options, 360, 130, 210, 160);
    Panel_Options_Res.Anchors := [akLeft];
      TKMLabel.Create(Panel_Options_Res, 6, 0, 188, 20, fTextLibrary[TX_MENU_OPTIONS_RESOLUTION], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_Options_Res, 0, 20, 220, 140);

      CheckBox_Options_FullScreen := TKMCheckBox.Create(Panel_Options_Res, 12, 30, 176, 20, fTextLibrary[TX_MENU_OPTIONS_FULLSCREEN], fnt_Metal);
      CheckBox_Options_FullScreen.OnClick := Options_ChangeRes;

      DropBox_Options_Resolution := TKMDropList.Create(Panel_Options_Res, 10, 50, 180, 20, fnt_Metal, '', bsMenu);
      DropBox_Options_Resolution.OnChange := Options_ChangeRes;

      DropBox_Options_RefreshRate := TKMDropList.Create(Panel_Options_Res, 10, 85, 180, 20, fnt_Metal, '', bsMenu);
      DropBox_Options_RefreshRate.OnChange := Options_ChangeRes;

      Button_Options_ResApply := TKMButton.Create(Panel_Options_Res, 10, 120, 180, 30, fTextLibrary[TX_MENU_OPTIONS_APPLY], bsMenu);
      Button_Options_ResApply.OnClick := Options_ApplyRes;

    //Language section
    Panel_Options_Lang:=TKMPanel.Create(Panel_Options,600,130,240,30+fLocales.Count*20);
    Panel_Options_Lang.Anchors := [akLeft];
      TKMLabel.Create(Panel_Options_Lang,6,0,242,20,fTextLibrary[TX_MENU_OPTIONS_LANGUAGE],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Lang,0,20,260,10+fLocales.Count*20);

      Radio_Options_Lang := TKMRadioGroup.Create(Panel_Options_Lang, 28, 27, 220, 20*fLocales.Count, fnt_Metal);
      SetLength(Image_Options_Lang_Flags,fLocales.Count);
      for i := 0 to fLocales.Count - 1 do
      begin
        Radio_Options_Lang.Items.Add(fLocales[i].Title);
        Image_Options_Lang_Flags[i] := TKMImage.Create(Panel_Options_Lang,6,28+(i*20),16,11,fLocales[i].FlagSpriteID,rxGuiMain);
        Image_Options_Lang_Flags[i].Tag := i;
        Image_Options_Lang_Flags[i].OnClick := Options_FlagClick;
      end;
      Radio_Options_Lang.OnChange := Options_Change;

    //Back button
    Button_Options_Back:=TKMButton.Create(Panel_Options,120,630,220,30,fTextLibrary[TX_MENU_BACK],bsMenu);
    Button_Options_Back.Anchors := [akLeft];
    Button_Options_Back.OnClick:=SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Credits_Page;
const OFFSET = 312;
begin
  Panel_Credits := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_Credits.Stretch;

    TKMLabel.Create(Panel_Credits, Panel_Main.Width div 2 - OFFSET, 70, fTextLibrary[TX_CREDITS],fnt_Outline,taCenter);
    Label_Credits_Remake := TKMLabelScroll.Create(Panel_Credits, Panel_Main.Width div 2 - OFFSET, 110, 0, Panel_Main.Height - 130,
    fTextLibrary[TX_CREDITS_PROGRAMMING]+'|Krom|Lewin||'+
    fTextLibrary[TX_CREDITS_ADDITIONAL_PROGRAMMING]+'|Alex|Danjb||'+
    fTextLibrary[TX_CREDITS_ADDITIONAL_GRAPHICS]+'|StarGazer|Malin||'+
    fTextLibrary[TX_CREDITS_ADDITIONAL_MUSIC]+'|Andre Sklenar - www.juicelab.cz||'+
    fTextLibrary[TX_CREDITS_ADDITIONAL_SOUNDS]+'|trb1914||'+
    fTextLibrary[TX_CREDITS_ADDITIONAL_TRANSLATIONS]+'|'+fLocales.GetTranslatorCredits+'|'+
    fTextLibrary[TX_CREDITS_SPECIAL]+'|KaM Community members'
    ,fnt_Grey,taCenter);
    Label_Credits_Remake.Anchors := [akLeft,akTop,akBottom];

    TKMLabel.Create(Panel_Credits, Panel_Main.Width div 2 + OFFSET, 70, fTextLibrary[TX_CREDITS_ORIGINAL], fnt_Outline, taCenter);
    Label_Credits_KaM := TKMLabelScroll.Create(Panel_Credits, Panel_Main.Width div 2 + OFFSET, 110, 0, Panel_Main.Height - 130, fTextLibrary[TX_CREDITS_TEXT], fnt_Grey, taCenter);
    Label_Credits_KaM.Anchors := [akLeft,akTop,akBottom];

    Button_CreditsHomepage:=TKMButton.Create(Panel_Credits,400,610,224,30,'[$F8A070]www.kamremake.com[]',bsMenu);
    Button_CreditsHomepage.Anchors := [akLeft,akBottom];
    Button_CreditsHomepage.OnClick:=Credits_LinkClick;

    Button_CreditsFacebook:=TKMButton.Create(Panel_Credits,400,646,224,30,'[$F8A070]Facebook[]',bsMenu);
    Button_CreditsFacebook.Anchors := [akLeft,akBottom];
    Button_CreditsFacebook.OnClick:=Credits_LinkClick;

    Button_CreditsBack:=TKMButton.Create(Panel_Credits,400,700,224,30,fTextLibrary[TX_MENU_BACK],bsMenu);
    Button_CreditsBack.Anchors := [akLeft,akBottom];
    Button_CreditsBack.OnClick:=SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Loading_Page;
begin
  Panel_Loading:=TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_Loading.Stretch;
    with TKMLabel.Create(Panel_Loading, Panel_Main.Width div 2, Panel_Main.Height div 2 - 20, fTextLibrary[TX_MENU_LOADING], fnt_Outline, taCenter) do
      Center;
    Label_Loading := TKMLabel.Create(Panel_Loading, Panel_Main.Width div 2, Panel_Main.Height div 2+10, '...', fnt_Grey, taCenter);
    Label_Loading.Center;
end;


procedure TKMMainMenuInterface.Create_Error_Page;
begin
  Panel_Error := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_Error.Stretch;
    with TKMLabel.Create(Panel_Error, Panel_Main.Width div 2, Panel_Main.Height div 2 - 20, fTextLibrary[TX_MENU_ERROR], fnt_Antiqua, taCenter) do
      Center;
    Label_Error := TKMLabel.Create(Panel_Error, 8, Panel_Main.Height div 2+10, Panel_Main.Width-16, 200, '...', fnt_Grey, taCenter);
    Label_Error.Center;
    Label_Error.AutoWrap := True;
    Button_ErrorBack := TKMButton.Create(Panel_Error,100,630,224,30,fTextLibrary[TX_MENU_BACK],bsMenu);
    Button_ErrorBack.Center;
    Button_ErrorBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Results_Page;
const StatText: array [1..9] of Word = (
    TX_RESULTS_UNITS_LOST,      TX_RESULTS_UNITS_DEFEATED,  TX_RESULTS_HOUSES_LOST,
    TX_RESULTS_HOUSES_DESTROYED,TX_RESULTS_HOUSES_BUILT,    TX_RESULTS_UNITS_TRAINED,
    TX_RESULTS_WEAPONS_MADE,    TX_RESULTS_SOLDIERS_TRAINED,TX_RESULTS_MISSION_TIME);
var I, Adv: Integer;
begin
  Panel_Results := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_Results.Stretch;
    //Background image
    with TKMImage.Create(Panel_Results,0,0,Panel_Main.Width, Panel_Main.Height,7,rxGuiMain) do
    begin
      ImageStretch;
      Center;
    end;
    //Fade to black by 62.5%
    with TKMShape.Create(Panel_Results,0,0,Panel_Main.Width+1, Panel_Main.Height+1) do
    begin
      Center;
      FillColor := $A0000000;
    end;

    Label_Results := TKMLabel.Create(Panel_Results,62,140,900,20,'<<<LEER>>>',fnt_Metal,taCenter);
    Label_Results.Anchors := [akLeft];

    Panel_Stats := TKMPanel.Create(Panel_Results, 30, 216, 360, 354);
    Panel_Stats.Anchors := [akLeft];

      //Backplate for column results
      with TKMImage.Create(Panel_Stats, 0, 0, 360, 354, 3, rxGuiMain) do
      begin
        ImageStretch;
        Center;
      end;

      Adv := 0;
      for I := 1 to 9 do
      begin
        inc(Adv, 25);
        if I in [3,6,7] then inc(Adv, 15);
        if I = 9 then inc(Adv, 45); //Last one goes right at the bottom of the scroll
        TKMLabel.Create(Panel_Stats,20,Adv,240,20,fTextLibrary[StatText[I]],fnt_Metal,taLeft);
        Label_Stat[I] := TKMLabel.Create(Panel_Stats,260,Adv,80,20,'00',fnt_Metal,taRight);
      end;

    if DISPLAY_CHARTS_RESULT then
    begin
      Panel_StatsCharts := TKMPanel.Create(Panel_Results, 410, 170, 610, 420);
      Panel_StatsCharts.Anchors := [akLeft];

      Button_ResultsArmy := TKMButtonFlat.Create(Panel_StatsCharts, 40, 0, 208, 20, 53, rxGui);
      Button_ResultsArmy.TexOffsetX := -91;
      Button_ResultsArmy.TexOffsetY := 7;
      Button_ResultsArmy.Anchors := [akLeft];
      Button_ResultsArmy.Caption := fTextLibrary[TX_GRAPH_ARMY];
      Button_ResultsArmy.CapOffsetY := -11;
      Button_ResultsArmy.OnClick := Results_GraphToggle;

      Button_ResultsCitizens := TKMButtonFlat.Create(Panel_StatsCharts, 40, 22, 208, 20, 588, rxGui);
      Button_ResultsCitizens.TexOffsetX := -92;
      Button_ResultsCitizens.TexOffsetY := 6;
      Button_ResultsCitizens.Anchors := [akLeft];
      Button_ResultsCitizens.Caption := fTextLibrary[TX_GRAPH_CITIZENS];
      Button_ResultsCitizens.CapOffsetY := -11;
      Button_ResultsCitizens.OnClick := Results_GraphToggle;

      Button_ResultsHouses := TKMButtonFlat.Create(Panel_StatsCharts, 252, 0, 208, 20, 587, rxGui);
      Button_ResultsHouses.TexOffsetX := -93;
      Button_ResultsHouses.TexOffsetY := 6;
      Button_ResultsHouses.Anchors := [akLeft];
      Button_ResultsHouses.Caption := fTextLibrary[TX_GRAPH_HOUSES];
      Button_ResultsHouses.CapOffsetY := -11;
      Button_ResultsHouses.OnClick := Results_GraphToggle;

      Button_ResultsWares := TKMButtonFlat.Create(Panel_StatsCharts, 252, 22, 208, 20, 360, rxGui);
      Button_ResultsWares.TexOffsetX := -93;
      Button_ResultsWares.TexOffsetY := 6;
      Button_ResultsWares.Anchors := [akLeft];
      Button_ResultsWares.Caption := fTextLibrary[TX_GRAPH_RESOURCES];
      Button_ResultsWares.CapOffsetY := -11;
      Button_ResultsWares.OnClick := Results_GraphToggle;

      Graph_Army := TKMGraph.Create(Panel_StatsCharts, 0, 46, 610, 374);
      Graph_Army.Caption := fTextLibrary[TX_GRAPH_ARMY];
      Graph_Army.Anchors := [akLeft];

      Graph_Citizens := TKMGraph.Create(Panel_StatsCharts, 0, 46, 610, 374);
      Graph_Citizens.Caption := fTextLibrary[TX_GRAPH_CITIZENS];
      Graph_Citizens.Anchors := [akLeft];

      Graph_Houses := TKMGraph.Create(Panel_StatsCharts, 0, 46, 610, 374);
      Graph_Houses.Caption := fTextLibrary[TX_GRAPH_HOUSES];
      Graph_Houses.Anchors := [akLeft];

      Graph_Wares := TKMGraph.Create(Panel_StatsCharts, 0, 46, 610, 374);
      Graph_Wares.Caption := fTextLibrary[TX_GRAPH_TITLE_RESOURCES];
      Graph_Wares.Anchors := [akLeft];
    end;

    Button_ResultsBack := TKMButton.Create(Panel_Results,30,610,220,30,fTextLibrary[TX_MENU_BACK],bsMenu);
    Button_ResultsBack.Anchors := [akLeft];
    Button_ResultsBack.OnClick := SwitchMenuPage;
    Button_ResultsRepeat := TKMButton.Create(Panel_Results,270,610,220,30,fTextLibrary[TX_MENU_MISSION_REPEAT],bsMenu);
    Button_ResultsRepeat.Anchors := [akLeft];
    Button_ResultsRepeat.OnClick := Results_RepeatLastMap;
    Button_ResultsContinue := TKMButton.Create(Panel_Results,510,610,220,30,fTextLibrary[TX_MENU_MISSION_NEXT],bsMenu);
    Button_ResultsContinue.Anchors := [akLeft];
    Button_ResultsContinue.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_ResultsMP_Page;
const
  BarStep = 150;
  RowHeight = 22;
  BarWidth = BarStep - 10;
  BarHalf = BarWidth div 2;
  Columns1: array[0..4] of integer = (TX_RESULTS_MP_CITIZENS_TRAINED, TX_RESULTS_MP_CITIZENS_LOST,
                                     TX_RESULTS_MP_SOLDIERS_EQUIPPED, TX_RESULTS_MP_SOLDIERS_LOST,
                                     TX_RESULTS_MP_SOLDIERS_DEFEATED);
  Columns2: array[0..4] of integer = (TX_RESULTS_MP_BUILDINGS_CONSTRUCTED, TX_RESULTS_MP_BUILDINGS_LOST,
                                     TX_RESULTS_MP_BUILDINGS_DESTROYED,
                                     TX_RESULTS_MP_WARES_PRODUCED, TX_RESULTS_MP_WEAPONS_PRODUCED);
var i,k: Integer;
begin
  Panel_ResultsMP := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_ResultsMP.Stretch;
    with TKMImage.Create(Panel_ResultsMP,0,0,Panel_Main.Width, Panel_Main.Height,7,rxGuiMain) do
    begin
      ImageStretch;
      Center;
    end;
    with TKMShape.Create(Panel_ResultsMP,0,0,Panel_Main.Width+1, Panel_Main.Height+1) do
    begin
      Center;
      FillColor := $A0000000;
    end;

    Label_ResultsMP := TKMLabel.Create(Panel_ResultsMP,62,125,900,20,'<<<LEER>>>',fnt_Metal,taCenter);
    Label_ResultsMP.Anchors := [akLeft];

    Button_MPResultsStats := TKMButtonFlat.Create(Panel_ResultsMP, 160, 155, 176, 20, 8, rxGuiMain);
    Button_MPResultsStats.TexOffsetX := -78;
    Button_MPResultsStats.TexOffsetY := 6;
    Button_MPResultsStats.Anchors := [akLeft];
    Button_MPResultsStats.Caption := fTextLibrary[TX_RESULTS_STATISTICS];
    Button_MPResultsStats.CapOffsetY := -11;
    Button_MPResultsStats.OnClick := ResultsMP_Toggle;

    Button_MPResultsArmy := TKMButtonFlat.Create(Panel_ResultsMP, 340, 155, 176, 20, 53, rxGui);
    Button_MPResultsArmy.TexOffsetX := -76;
    Button_MPResultsArmy.TexOffsetY := 6;
    Button_MPResultsArmy.Anchors := [akLeft];
    Button_MPResultsArmy.Caption := fTextLibrary[TX_GRAPH_ARMY];
    Button_MPResultsArmy.CapOffsetY := -11;
    Button_MPResultsArmy.OnClick := ResultsMP_Toggle;

    Button_MPResultsEconomy := TKMButtonFlat.Create(Panel_ResultsMP, 520, 155, 176, 20, 589, rxGui);
    Button_MPResultsEconomy.TexOffsetX := -72;
    Button_MPResultsEconomy.TexOffsetY := 6;
    Button_MPResultsEconomy.Anchors := [akLeft];
    Button_MPResultsEconomy.Caption := fTextLibrary[TX_RESULTS_ECONOMY];
    Button_MPResultsEconomy.CapOffsetY := -11;
    Button_MPResultsEconomy.OnClick := ResultsMP_Toggle;

    Button_MPResultsWares := TKMButtonFlat.Create(Panel_ResultsMP, 700, 155, 176, 20, 360, rxGui);
    Button_MPResultsWares.TexOffsetX := -77;
    Button_MPResultsWares.TexOffsetY := 6;
    Button_MPResultsWares.Anchors := [akLeft];
    Button_MPResultsWares.Caption := fTextLibrary[TX_GRAPH_RESOURCES];
    Button_MPResultsWares.CapOffsetY := -11;
    Button_MPResultsWares.OnClick := ResultsMP_Toggle;

    Panel_StatsMP1 := TKMPanel.Create(Panel_ResultsMP, 62, 240, 900, 180);
    Panel_StatsMP1.Anchors := [akLeft];

      for i:=0 to 7 do
        Label_ResultsPlayerName1[i] := TKMLabel.Create(Panel_StatsMP1, 0, 38+i*RowHeight, 150, 20, '', fnt_Metal, taLeft);

      for k:=0 to 4 do
      begin
        with TKMLabel.Create(Panel_StatsMP1, 160 + BarStep*k, 0, BarWidth+6, 40, fTextLibrary[Columns1[k]], fnt_Metal, taCenter) do
          AutoWrap := true;
        for i:=0 to 7 do
        begin
          Bar_Results[i,k] := TKMPercentBar.Create(Panel_StatsMP1, 160 + k*BarStep, 35+i*RowHeight, BarWidth, 20, fnt_Grey);
          Bar_Results[i,k].TextYOffset := -3;
          Image_ResultsRosette[i,k] := TKMImage.Create(Panel_StatsMP1, 164 + k*BarStep, 38+i*RowHeight, 16, 16, 8, rxGuiMain);
        end;
      end;

    Panel_StatsMP2 := TKMPanel.Create(Panel_ResultsMP, 62, 411, 900, 180);
    Panel_StatsMP2.Anchors := [akLeft];

      for i:=0 to 7 do
        Label_ResultsPlayerName2[i] := TKMLabel.Create(Panel_StatsMP2, 0, 38+i*RowHeight, 150, 20, '', fnt_Metal, taLeft);

      for k:=0 to 4 do
      begin
        with TKMLabel.Create(Panel_StatsMP2, 160 + BarStep*k, 0, BarWidth+6, 40, fTextLibrary[Columns2[k]], fnt_Metal, taCenter) do
          AutoWrap := true;
        for i:=0 to 7 do
        begin
          Bar_Results[i,k+5] := TKMPercentBar.Create(Panel_StatsMP2, 160 + k*BarStep, 35+i*RowHeight, BarWidth, 20, fnt_Grey);
          Bar_Results[i,k+5].TextYOffset := -3;
          Image_ResultsRosette[i,k+5] := TKMImage.Create(Panel_StatsMP2, 164 + k*BarStep, 38+i*RowHeight, 16, 16, 8, rxGuiMain);
        end;
      end;

    Panel_GraphsMP := TKMPanel.Create(Panel_ResultsMP, 0, 185, 1024, 560);
    Panel_GraphsMP.Anchors := [akLeft];

      Graph_MPArmy := TKMGraph.Create(Panel_GraphsMP, 12, 0, 1000, 435);
      Graph_MPArmy.Caption := fTextLibrary[TX_GRAPH_ARMY];
      Graph_MPArmy.Anchors := [akLeft];

      Graph_MPCitizens := TKMGraph.Create(Panel_GraphsMP, 62, 0, 900, 200);
      Graph_MPCitizens.Caption := fTextLibrary[TX_GRAPH_CITIZENS];
      Graph_MPCitizens.Anchors := [akLeft];

      Graph_MPHouses := TKMGraph.Create(Panel_GraphsMP, 62, 235, 900, 200);
      Graph_MPHouses.Caption := fTextLibrary[TX_GRAPH_HOUSES];
      Graph_MPHouses.Anchors := [akLeft];

      Image_MPResultsBackplate := TKMImage.Create(Panel_GraphsMP, 12, 56, 178, 224, 3, rxGuiMain);
      Image_MPResultsBackplate.ImageStretch;
      Image_MPResultsBackplate.Center;

      Radio_MPResultsWarePlayer := TKMRadioGroup.Create(Panel_GraphsMP, 26, 70, 150, 200, fnt_Metal);
      Radio_MPResultsWarePlayer.Anchors := [akLeft];
      Radio_MPResultsWarePlayer.OnChange := ResultsMP_PlayerSelect;

      for I:=0 to MAX_PLAYERS-1 do
      begin
        Graph_MPWares[I] := TKMGraph.Create(Panel_GraphsMP, 190, 0, 822, 435);
        Graph_MPWares[I].Caption := fTextLibrary[TX_GRAPH_TITLE_RESOURCES];
        Graph_MPWares[I].Font := fnt_Metal; //fnt_Outline doesn't work because player names blend badly with yellow
        Graph_MPWares[I].Anchors := [akLeft];
      end;

    Button_ResultsMPBack := TKMButton.Create(Panel_ResultsMP,100,630,220,30,fTextLibrary[TX_MENU_BACK],bsMenu);
    Button_ResultsMPBack.Anchors := [akLeft];
    Button_ResultsMPBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.SwitchMenuPage(Sender: TObject);
var I: Integer;
begin
  Label_Version.Caption := GAME_VERSION + ' / ' + fGameApp.RenderVersion;

  //First thing - hide all existing pages
  for I := 1 to Panel_Main.ChildCount do
    if Panel_Main.Childs[i] is TKMPanel then
      Panel_Main.Childs[i].Hide;

  {Return to MainMenu}
  if (Sender = nil)
  or (Sender = Button_SP_Back)
  or (Sender = Button_MP_Back)
  or (Sender = Button_CreditsBack)
  or (Sender = Button_MapEdBack)
  or (Sender = Button_ErrorBack)
  or (Sender = Button_ReplaysBack)
  or (((Sender = Button_ResultsBack)or(Sender = Button_ResultsMPBack))and(fGameResultMsg = gr_ReplayEnd)) then
  begin
    //Scan should be terminated, it is no longer needed
    if Sender = Button_ReplaysBack then
      fSaves.TerminateScan;
    if (Sender = Button_MP_Back) or (Sender = Button_ErrorBack) then
      fMain.UnlockMutex; //Leaving MP areas
    Panel_MainMenu.Show;
  end;

  {Player leaves lobby (LAN text is updated)}
  if Sender = Button_LobbyBack then
  begin
    Panel_MultiPlayer.Show;
    MP_ServersRefresh(Sender);
  end;

  {Return to MainMenu and restore resolution changes}
  if Sender = Button_Options_Back then
  begin
    fMain.Settings.SaveSettings;
    Panel_MainMenu.Show;
  end;

  {Show SinglePlayer menu}
  {Return to SinglePlayerMenu}
  if (Sender = Button_MM_SinglePlayer)
  or (Sender = Button_Camp_Back)
  or (Sender = Button_SingleBack)
  or (Sender = Button_LoadBack)
  or ((Sender = Button_ResultsBack)and(fGameResultMsg <> gr_ReplayEnd)) then
  begin
    if (Sender = Button_SingleBack) then
      //scan should be terminated, it is no longer needed
      fMaps.TerminateScan;
    if (Sender = Button_LoadBack) then
      //scan should be terminated, it is no longer needed
      fSaves.TerminateScan;
    Panel_SinglePlayer.Show;
  end;

  {Show campaign selection menu}
  if (Sender = Button_SP_Camp)
  or (Sender = Button_CampaignBack) then
  begin
    fGameApp.MusicLib.StopPlayingOtherFile; //Cancel briefing if it was playing
    Campaign_FillList;
    Panel_CampSelect.Show;
  end;

  {Show campaign screen}
  if (Sender = Button_Camp_Start) or (Sender = List_Camps) then
  begin
    Campaign_Set(fGameApp.Campaigns.CampaignByTitle(List_Camps.Rows[List_Camps.ItemIndex].Cells[3].Caption));
    Panel_Campaign.Show;
  end;
  if (Sender = Button_ResultsContinue) then
  begin
    Campaign_Set(fGameApp.Campaigns.ActiveCampaign);
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
    fMaps.Refresh(SingleMap_ScanUpdate);
    Panel_Single.Show;
  end;

  {Show Load menu}
  if Sender=Button_SP_Load then begin
    //Stop current scan so it can't add a save after we clear the list
    fSaves.TerminateScan;
    fLastSaveCRC := 0;
    List_Load.Clear; //clear the list
    Load_DeleteConfirmation(False);
    Load_ListClick(nil);
    //Initiate refresh and process each new save added
    fSaves.Refresh(Load_ScanUpdate, False);
    Load_Sort(List_Load.SortIndex); //Apply sorting from last time we were on this page
    Panel_Load.Show;
  end;

  {Show replays menu}
  if Sender=Button_MM_Replays then begin
    fLastSaveCRC := 0;
    Radio_Replays_Type.ItemIndex := 0; //we always show SP replays on start
    Replay_TypeChange(nil); //Select SP as this will refresh everything
    Replays_Sort(List_Replays.SortIndex); //Apply sorting from last time we were on this page
    Panel_Replays.Show;
  end;

  {Show MultiPlayer menu}
  if (Sender=Button_MM_MultiPlayer)
  or ((Sender=Button_ResultsMPBack)and(fGameResultMsg <> gr_ReplayEnd)) then
  begin
    fGameApp.NetworkInit;
    MP_Init(Sender);
    MP_Update(fTextLibrary[TX_MP_MENU_STATUS_READY],icGreen,false);
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
    Panel_MapEd.Show;
  end;

  {Show Options menu}
  if Sender=Button_MM_Options then
  begin
    Options_Fill(fMain.Settings, fGameApp.GameSettings);
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
  if Sender = Panel_Error then
    Panel_Error.Show;

  {Show Results screen}
  if Sender = Panel_Results then //This page can be accessed only by itself
  begin
    Results_GraphToggle(Button_ResultsArmy);
    Panel_Results.Show;
  end;

  {Show ResultsMP screen}
  if Sender=Panel_ResultsMP then //This page can be accessed only by itself
    Panel_ResultsMP.Show;
end;


procedure TKMMainMenuInterface.MainMenu_MultiplayerClick(Sender: TObject);
begin
  if fMain.LockMutex then
  begin
    if not fGameApp.CheckDATConsistency then
      ShowScreen(msError, fTextLibrary[TX_ERROR_MODS])
    else
      SwitchMenuPage(Sender);
  end
  else
    ShowScreen(msError, fTextLibrary[TX_MULTIPLE_INSTANCES]);
end;


procedure TKMMainMenuInterface.MainMenu_PlayTutorial(Sender: TObject);
begin
  fGameApp.NewSingleMap(ExeDir + 'Tutorials\Town Tutorial\Town Tutorial.dat', fTextLibrary[TX_MENU_TUTORIAL_TOWN]);
end;


procedure TKMMainMenuInterface.MainMenu_PlayBattle(Sender: TObject);
begin
  fGameApp.NewSingleMap(ExeDir + 'Tutorials\Battle Tutorial\Battle Tutorial.dat', fTextLibrary[TX_MENU_TUTORIAL_BATTLE]);
end;


procedure TKMMainMenuInterface.Results_RepeatLastMap(Sender: TObject);
begin
  fGameApp.NewRestartLast; //Means replay last map
end;


procedure TKMMainMenuInterface.Campaign_FillList;
var
  I: Integer;
  Camps: TKMCampaignsCollection;
begin
  Camps := fGameApp.Campaigns;

  Image_CampsPreview.TexID := 0; //Clear preview image
  List_Camps.Clear;
  for I := 0 to Camps.Count - 1 do
  with Camps[I] do
    List_Camps.AddItem(MakeListRow(
                        [CampaignTitle, IntToStr(MapCount), IntToStr(UnlockedMap+1), ShortTitle],
                        [$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $00FFFFFF]));

  Button_Camp_Start.Disable;
end;


procedure TKMMainMenuInterface.Campaign_ListChange(Sender: TObject);
var Camp: TKMCampaign;
begin
  Button_Camp_Start.Enable;
  Camp := fGameApp.Campaigns.CampaignByTitle(List_Camps.Rows[List_Camps.ItemIndex].Cells[3].Caption);

  Image_CampsPreview.RX := Camp.BackGroundPic.RX;
  Image_CampsPreview.TexID := Camp.BackGroundPic.ID;
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
    Image_CampaignFlags[I].Visible := I < Campaign_Selected.MapCount;
    Image_CampaignFlags[I].TexID   := MapPic[I <= Campaign_Selected.UnlockedMap];
    Image_CampaignFlags[I].HighlightOnMouseOver := I <= Campaign_Selected.UnlockedMap;
  end;

  //Place sites
  for I := 0 to Campaign_Selected.MapCount - 1 do
  begin
    //Pivot flags around Y=bottom X=middle, that's where the flag pole is
    Image_CampaignFlags[I].Left := Campaign_Selected.Maps[I].Flag.X - Round((Image_CampaignFlags[I].Width/2)*(1-Panel_Campaign_Flags.Scale));
    Image_CampaignFlags[I].Top  := Campaign_Selected.Maps[I].Flag.Y - Round(Image_CampaignFlags[I].Height   *(1-Panel_Campaign_Flags.Scale));
  end;

  //Select last map to play by 'clicking' last node
  Campaign_SelectMap(Image_CampaignFlags[Campaign_Selected.UnlockedMap]);

  //When opening campaign screen set the scroll initial position properly
  //Player can move it later (to allow to select previous maps and look at camp map)
  Panel_CampScroll.Left := IfThen(Campaign_Selected.Maps[Campaign_MapIndex].TextPos = cBottomRight, Panel_Campaign.Width - Panel_CampScroll.Width, 0);
end;


procedure TKMMainMenuInterface.Campaign_SelectMap(Sender: TObject);
var I: Integer;
begin
  if not (Sender is TKMImage) then exit;
  if not TKMImage(Sender).HighlightOnMouseOver then exit; //Skip closed maps

  Campaign_MapIndex := TKMImage(Sender).Tag;

  //Place highlight
  for I := 0 to High(Image_CampaignFlags) do
    Image_CampaignFlags[I].Highlight := (Campaign_MapIndex = I);

  //Connect by sub-nodes
  for I := 0 to High(Image_CampaignSubNode) do
  begin
    Image_CampaignSubNode[I].Visible := InRange(I, 0, Campaign_Selected.Maps[Campaign_MapIndex].NodeCount-1);
    Image_CampaignSubNode[I].Left := Campaign_Selected.Maps[Campaign_MapIndex].Nodes[I].X;
    Image_CampaignSubNode[I].Top  := Campaign_Selected.Maps[Campaign_MapIndex].Nodes[I].Y;
  end;

  Label_CampaignTitle.Caption := Format(fTextLibrary[TX_GAME_MISSION], [Campaign_MapIndex+1]);
  Label_CampaignText.Caption := Campaign_Selected.MissionText(Campaign_MapIndex);

  Panel_CampScroll.Height := 50 + Label_CampaignText.TextSize.Y + 70; //Add offset from top and space on bottom
  Panel_CampScroll.Top := Panel_Main.Height - Panel_CampScroll.Height;
  Image_Scroll.Height := Panel_CampScroll.Height;

  fGameApp.MusicLib.StopPlayingOtherFile; //Stop playing the previous breifing even if this one doesn't exist
  fGameApp.PauseMusicToPlayFile(Campaign_Selected.BreifingAudioFile(Campaign_MapIndex, fGameApp.GameSettings.Locale));
end;


procedure TKMMainMenuInterface.Campaign_StartMap(Sender: TObject);
begin
  fGameApp.MusicLib.StopPlayingOtherFile;
  fGameApp.NewCampaignMap(Campaign_Selected, Campaign_MapIndex);
end;


procedure TKMMainMenuInterface.Credits_LinkClick(Sender: TObject);

  //This can't be moved to e.g. KM_Utils because the dedicated server needs that, and it must be Linux compatible
  procedure GoToURL(aUrl: string);
  begin
    {$IFDEF WDC}
    ShellExecute(Application.Handle, 'open', PChar(aUrl), nil, nil, SW_SHOWNORMAL);
    {$ENDIF}
    {$IFDEF FPC}
    OpenURL(aUrl);
    {$ENDIF}
  end;

begin
  if Sender = Button_CreditsHomepage then GoToURL('http://www.kamremake.com/redirect.php?page=homepage&rev='+GAME_REVISION);
  if Sender = Button_CreditsFacebook then GoToURL('http://www.kamremake.com/redirect.php?page=facebook&rev='+GAME_REVISION);
end;


procedure TKMMainMenuInterface.SingleMap_Clear;
begin
  ColList_SingleMaps.Clear;
  SingleMap_ListClick(nil);
  fLastMapCRC := 0;
end;


procedure TKMMainMenuInterface.SingleMap_ScanUpdate(Sender: TObject);
begin
  SingleMap_RefreshList(False); //Don't jump to selected with each scan update
end;


procedure TKMMainMenuInterface.SingleMap_SortUpdate(Sender: TObject);
begin
  SingleMap_RefreshList(True); //After sorting jump to the selected item
end;


procedure TKMMainMenuInterface.SingleMap_RefreshList(aJumpToSelected: Boolean);
var
  I, MapIndex: Integer;
begin
  ColList_SingleMaps.Clear;

  //IDs of maps could changed, so use CRC to check which one was selected
  MapIndex := -1;
  for I := 0 to fMaps.Count-1 do
    if (fMaps[I].CRC = fLastMapCRC) then
      MapIndex := I;

  if aJumpToSelected
  and not InRange(MapIndex - ColList_SingleMaps.TopIndex, 0, MENU_SP_MAPS_COUNT - 1)
  then
    if MapIndex < ColList_SingleMaps.TopIndex + MENU_SP_MAPS_COUNT - 1 then
      ColList_SingleMaps.TopIndex := MapIndex
    else
    if MapIndex > ColList_SingleMaps.TopIndex + MENU_SP_MAPS_COUNT - 1 then
      ColList_SingleMaps.TopIndex := MapIndex - MENU_SP_MAPS_COUNT + 1;

  for I := 0 to fMaps.Count - 1 do
  begin
    ColList_SingleMaps.AddItem(MakeListRow(['', IntToStr(fMaps[I].Info.PlayerCount), fMaps[I].FileName, fMaps[I].Info.MapSizeText]));
    ColList_SingleMaps.Rows[I].Cells[2].Hint := fMaps[I].SmallDesc;
    ColList_SingleMaps.Rows[I].Cells[0].Pic := MakePic(rxGui, 28 + Byte(fMaps[I].Info.MissionMode <> mm_Tactic)*14);
  end;
end;


procedure TKMMainMenuInterface.SingleMap_ListClick(Sender: TObject);
var
  ID: Integer;
begin
  ID := ColList_SingleMaps.ItemIndex;

  //User could have clicked on empty space in list and we get -1 or unused id
  if not InRange(ID, 0, fMaps.Count - 1) then
  begin
    fLastMapCRC := 0;
    Label_SingleTitle.Caption   := '';
    Memo_SingleDesc.Text        := '';
    Label_SingleCondTyp.Caption := '';
    Label_SingleCondWin.Caption := '';
    Label_SingleCondDef.Caption := '';

    MinimapView_Single.Hide;
  end
  else
  begin
    fLastMapCRC := fMaps[ID].CRC;
    Label_SingleTitle.Caption   := fMaps[ID].FileName;
    Memo_SingleDesc.Text        := fMaps[ID].BigDesc;
    Label_SingleCondTyp.Caption := Format(fTextLibrary[TX_MENU_MISSION_TYPE], [fMaps[ID].Info.MissionModeText]);
    Label_SingleCondWin.Caption := Format(fTextLibrary[TX_MENU_WIN_CONDITION], [fMaps[ID].Info.VictoryCondition]);
    Label_SingleCondDef.Caption := Format(fTextLibrary[TX_MENU_DEFEAT_CONDITION], [fMaps[ID].Info.DefeatCondition]);

    MinimapView_Single.Show;
    fMinimap.LoadFromMission(fMaps[ID].FullPath('.dat'));
    fMinimap.Update(False);
    MinimapView_Single.SetMinimap(fMinimap);
  end;

  Button_SingleStart.Enabled := ID <> -1;
end;


procedure TKMMainMenuInterface.SingleMap_Start(Sender: TObject);
var
  I: Integer;
begin
  //This is also called by double clicking on a list entry
  if not Button_SingleStart.Enabled then
    Exit;

  for I := 0 to fMaps.Count - 1 do
    if fLastMapCRC = fMaps[I].CRC then
    begin
      //Scan should be terminated, as it is no longer needed
      fMaps.TerminateScan;
      fGameApp.NewSingleMap(fMaps[I].FullPath('.dat'), fMaps[I].FileName); //Provide mission FileName mask and title here
      Break;
    end;
end;


procedure TKMMainMenuInterface.SingleMap_Sort(aColumn: Integer);
var
  Method: TMapsSortMethod;
begin
  //Set Descending order by default and invert it if same column selected again
  case aColumn of
    0:  if fMaps.SortMethod = smByModeDesc then
          Method := smByModeAsc
        else
          Method := smByModeDesc;
    1:
        if fMaps.SortMethod = smByPlayersDesc then
          Method := smByPlayersAsc
        else
          Method := smByPlayersDesc;
    2:  if fMaps.SortMethod = smByNameDesc then
          Method := smByNameAsc
        else
          Method := smByNameDesc;
    3:  if fMaps.SortMethod = smBySizeDesc then
          Method := smBySizeAsc
        else
          Method := smBySizeDesc;
    else
        Method := smByNameAsc; //Default
  end;

  //Start sorting and wait for SortComplete event
  fMaps.Sort(Method, SingleMap_SortUpdate);
end;


procedure TKMMainMenuInterface.MP_Init(Sender: TObject);
begin
  fServerSelected := False;

  //Refresh the list when they first open the multiplayer page
  MP_ServersRefresh(Sender);

  Edit_MP_PlayerName.Text := fGameApp.GameSettings.MultiplayerName;

  Edit_MP_ServerName.Text := fGameApp.GameSettings.ServerName;
  Edit_MP_ServerPort.Text := fGameApp.GameSettings.ServerPort;

  Edit_MP_FindIP.Text := fGameApp.GameSettings.LastIP;
  Edit_MP_FindPort.Text := fGameApp.GameSettings.LastPort;
  Edit_MP_FindRoom.Text := fGameApp.GameSettings.LastRoom;

  Button_MP_GetIn.Disable;

  //Fetch the announcements display
  fGameApp.Networking.ServerQuery.OnAnnouncements := MP_AnnouncementsUpdated;
  fGameApp.Networking.ServerQuery.FetchAnnouncements(fGameApp.GameSettings.Locale);
  Memo_MP_Announcement.Clear;
  Memo_MP_Announcement.Add(fTextLibrary[TX_MP_MENU_LOADING_ANNOUNCEMENTS]);
end;


//Events binding is the same for Host and Joiner because of stand-alone Server
//E.g. If Server fails, Host can be disconnected from it as well as a Joiner
procedure TKMMainMenuInterface.MP_BindEvents;
begin
  fGameApp.Networking.OnTextMessage  := Lobby_OnMessage;
  fGameApp.Networking.OnPlayersSetup := Lobby_OnPlayersSetup;
  fGameApp.Networking.OnGameOptions  := Lobby_OnGameOptions;
  fGameApp.Networking.OnMapName      := Lobby_OnMapName;
  fGameApp.Networking.OnPingInfo     := Lobby_OnPingInfo;
  fGameApp.Networking.OnStartMap     := fGameApp.NewMultiplayerMap;
  fGameApp.Networking.OnStartSave    := fGameApp.NewMultiplayerSave;
  fGameApp.Networking.OnDisconnect   := Lobby_OnDisconnect;
  fGameApp.Networking.OnReassignedHost := Lobby_OnReassignedToHost;
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
  fGameApp.GameSettings.MultiplayerName := Edit_MP_PlayerName.Text;

  //Create Server popup
  fGameApp.GameSettings.ServerName := Edit_MP_ServerName.Text;
  fGameApp.GameSettings.ServerPort := Edit_MP_ServerPort.Text;

  //Join server popup
  fGameApp.GameSettings.LastPort := Edit_MP_FindPort.Text;
  fGameApp.GameSettings.LastRoom := Edit_MP_FindRoom.Text;
  fGameApp.GameSettings.LastIP   := Edit_MP_FindIP.Text;
end;


//Update status line
//When user tries to Join some server disable joining controls for that time
procedure TKMMainMenuInterface.MP_Update(const aStatus: string; aColor: TColor4; aBusy: Boolean);
begin
  fLobbyBusy := aBusy;

  //Toggle server creation
  Button_MP_CreateServer.Enabled := not aBusy;
  Button_MP_CreateLAN.Enabled := not aBusy;
  Button_MP_CreateWAN.Enabled := not aBusy;

  //Toggle server joining
  Button_MP_FindServer.Enabled := not aBusy;
  Button_MP_FindServerIP.Enabled := not aBusy;
  Button_MP_FindCancel.Enabled := not aBusy;
  Button_MP_GetIn.Enabled := MP_GetInEnabled;

  Label_MP_Status.Caption := aStatus;
  Label_MP_Status.FontColor := aColor;
end;


procedure TKMMainMenuInterface.MP_ServersRefresh(Sender: TObject);
begin
  fGameApp.Networking.ServerQuery.OnListUpdated := MP_ServersUpdateList;
  fGameApp.Networking.ServerQuery.RefreshList;
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
  I, OldTopIndex: Integer;
  DisplayName: string;
  S: TKMServerInfo;
  R: TKMRoomInfo;
begin
  OldTopIndex := ColList_Servers.TopIndex;
  ColList_Servers.Clear;
  ColList_Servers.ItemIndex := -1;

  if fGameApp.Networking.ServerQuery.Rooms.Count = 0 then
  begin
    //Do not use 'Show' here as it will also make the parent panel visible
    //which could be already hidden if player switched pages
    Label_Servers_Status.Caption := fTextLibrary[TX_MP_MENU_NO_SERVERS];
    Label_Servers_Status.Visible := True;
  end
  else
  begin
    Label_Servers_Status.Hide;
    for I := 0 to fGameApp.Networking.ServerQuery.Rooms.Count - 1 do
    begin
      R := fGameApp.Networking.ServerQuery.Rooms[I];
      S := fGameApp.Networking.ServerQuery.Servers[R.ServerIndex];

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
      end;
    end;

    ColList_Servers.TopIndex := OldTopIndex;
    if (ColList_Servers.ItemIndex <> -1)
    and not InRange(ColList_Servers.ItemIndex - ColList_Servers.TopIndex, 0, ColList_Servers.GetVisibleRows - 1) then
    begin
      if ColList_Servers.ItemIndex < ColList_Servers.TopIndex + ColList_Servers.GetVisibleRows - 1 then
        ColList_Servers.TopIndex := ColList_Servers.ItemIndex
      else
      if ColList_Servers.ItemIndex > ColList_Servers.TopIndex + ColList_Servers.GetVisibleRows - 1 then
        ColList_Servers.TopIndex := ColList_Servers.ItemIndex - ColList_Servers.GetVisibleRows + 1;
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
          fGameApp.Networking.ServerQuery.SortMethod := ssmByNameAsc
        else
          fGameApp.Networking.ServerQuery.SortMethod := ssmByNameDesc;
    //Sorting by state goes Lobby,Loading,Game,None by default
    1:  if ColList_Servers.SortDirection = sdDown then
          fGameApp.Networking.ServerQuery.SortMethod := ssmByStateAsc
        else
          fGameApp.Networking.ServerQuery.SortMethod := ssmByStateDesc;
    //Sorting by player count goes 8..0 by default
    2:  if ColList_Servers.SortDirection = sdDown then
          fGameApp.Networking.ServerQuery.SortMethod := ssmByPlayersDesc
        else
          fGameApp.Networking.ServerQuery.SortMethod := ssmByPlayersAsc;
    //Sorting by ping goes 0 ... 1000 by default
    3:  if ColList_Servers.SortDirection = sdDown then
          fGameApp.Networking.ServerQuery.SortMethod := ssmByPingAsc
        else
          fGameApp.Networking.ServerQuery.SortMethod := ssmByPingDesc;
  end;

  //Refresh the display only if there are rooms to be sorted (otherwise it shows "no servers found" immediately)
  if fGameApp.Networking.ServerQuery.Rooms.Count > 0 then
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
  Button_MP_GetIn.Enabled := MP_GetInEnabled;

  fSelectedRoomInfo := fGameApp.Networking.ServerQuery.Rooms[ColList_Servers.Rows[ID].Tag];
  fSelectedServerInfo := fGameApp.Networking.ServerQuery.Servers[fSelectedRoomInfo.ServerIndex];

  Label_MP_Players.Caption := fSelectedRoomInfo.GameInfo.Players;
  Label_MP_GameTime.Caption := fSelectedRoomInfo.GameInfo.GetFormattedTime;
  Label_MP_Map.Caption := fSelectedRoomInfo.GameInfo.Map;
end;


procedure TKMMainMenuInterface.MP_ServersDoubleClick(Sender: TObject);
begin
  //MP_SelectServer gets called by first Click
  if Button_MP_GetIn.Enabled and (ColList_Servers.ItemIndex <> -1)
  and InRange(ColList_Servers.Rows[ColList_Servers.ItemIndex].Tag, 0, fGameApp.Networking.ServerQuery.Rooms.Count-1) then
    MP_GetInClick(Sender);
end;


procedure TKMMainMenuInterface.MP_HostClick(Sender: TObject);
begin
  //Save the player and IP name so it is not lost if something fails
  MP_SaveSettings;

  Panel_MPCreateServer.Hide; //Hide the panel so if it fails the error message will be easy to see (e.g. name too long)
  if not MP_ValidatePlayerName(Edit_MP_PlayerName.Text) then Exit;

  SwitchMenuPage(Sender); //Open lobby page

  MP_BindEvents;
  fGameApp.Networking.OnHostFail := MP_HostFail;
  fGameApp.Networking.Host(Edit_MP_PlayerName.Text, Edit_MP_ServerName.Text, Edit_MP_ServerPort.Text, (Sender = Button_MP_CreateWAN));
end;


procedure TKMMainMenuInterface.MP_GetInClick(Sender: TObject);
begin
  MP_Join(fSelectedServerInfo.IP, fSelectedServerInfo.Port, fSelectedRoomInfo.RoomID);
end;


function TKMMainMenuInterface.MP_ValidatePlayerName(const aName: string): Boolean;
const MAX_NAME_LENGTH = 16;
begin
  Result := False;

  if Trim(aName) = '' then
  begin
    MP_Update(fTextLibrary[TX_GAME_ERROR_BLANK_PLAYERNAME], icYellow, false);
    fSoundLib.Play(sfxn_Error);
  end
  else if Length(aName) > MAX_NAME_LENGTH then
  begin
    MP_Update(Format(fTextLibrary[TX_GAME_ERROR_LONG_PLAYERNAME], [MAX_NAME_LENGTH]), icYellow, false);
    fSoundLib.Play(sfxn_Error);
  end
  else if (Pos('|', aName) <> 0) or (Pos('[$', aName) <> 0) or (Pos('[]', aName) <> 0) then
  begin
    MP_Update(fTextLibrary[TX_GAME_ERROR_ILLEGAL_PLAYERNAME], icYellow, false);
    fSoundLib.Play(sfxn_Error);
  end
  else
    Result := True;
end;


//Join button is enabled if valid server is selected and the lobby is not busy
function TKMMainMenuInterface.MP_GetInEnabled:Boolean;
var ID: Integer;
begin
  ID := ColList_Servers.ItemIndex;
  Result := (not fLobbyBusy) and (ID <> -1) and (ColList_Servers.Rows[ID].Tag <> -1);
end;


procedure TKMMainMenuInterface.MP_Join(aServerAddress, aPort: string; aRoom: Integer);
begin
  //Save the player and IP name so it is not lost if the connection fails
  MP_SaveSettings;

  if not MP_ValidatePlayerName(Edit_MP_PlayerName.Text) then Exit;

  //Disable buttons to prevent multiple clicks while connection process is in progress
  MP_Update(fTextLibrary[TX_MP_MENU_STATUS_CONNECTING],icGreen, True);

  //Send request to join
  fGameApp.Networking.OnJoinSucc := MP_JoinSuccess;
  fGameApp.Networking.OnJoinFail := MP_JoinFail;
  fGameApp.Networking.OnJoinAssignedHost := MP_JoinAssignedHost;
  fGameApp.Networking.Join(aServerAddress, aPort, Edit_MP_PlayerName.Text, aRoom); //Init lobby
end;


//We had recieved permission to join
procedure TKMMainMenuInterface.MP_JoinSuccess(Sender: TObject);
begin
  SwitchMenuPage(Button_MP_GetIn); //Open lobby page

  fGameApp.Networking.OnJoinSucc := nil;
  fGameApp.Networking.OnJoinFail := nil;
  fGameApp.Networking.OnJoinAssignedHost := nil;
  MP_BindEvents;
end;


procedure TKMMainMenuInterface.MP_JoinFail(const aData: string);
begin
  fGameApp.Networking.Disconnect;
  MP_Update(Format(fTextLibrary[TX_GAME_ERROR_CONNECTION_FAILED],[aData]),icYellow,false);
  fSoundLib.Play(sfxn_Error);
end;


procedure TKMMainMenuInterface.MP_JoinAssignedHost(Sender: TObject);
begin
  //We were joining a game and the server assigned hosting rights to us
  SwitchMenuPage(Button_MP_CreateLAN); //Open lobby page in host mode

  fGameApp.Networking.OnJoinSucc := nil;
  fGameApp.Networking.OnJoinFail := nil;
  fGameApp.Networking.OnJoinAssignedHost := nil;
  fGameApp.Networking.OnHostFail := MP_HostFail;
  MP_BindEvents;
end;


procedure TKMMainMenuInterface.MP_BackClick(Sender: TObject);
begin
  fGameApp.Networking.Disconnect;
  MP_SaveSettings;
  SwitchMenuPage(Sender);
end;


procedure TKMMainMenuInterface.MP_HostFail(const aData: string);
begin
  fGameApp.Networking.Disconnect;
  SwitchMenuPage(Button_LobbyBack);
  MP_Update(aData, icYellow, False);
  fSoundLib.Play(sfxn_Error);
end;


//Reset everything to it's defaults depending on users role (Host/Joiner/Reassigned)
procedure TKMMainMenuInterface.Lobby_Reset(Sender: TObject; aPreserveMessage: Boolean = False; aPreserveMaps: Boolean = False);
var I: Integer;
begin
  Label_LobbyServerName.Caption := '';

  for I := 0 to MAX_PLAYERS - 1 do
  begin
    Label_LobbyPlayer[I].Caption := '.';
    Label_LobbyPlayer[I].FontColor := $FFFFFFFF;
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
    Image_LobbyReady[I].TexID := 0;
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
    if not aPreserveMaps then Lobby_MapTypeSelect(nil);
    DropCol_LobbyMaps.Show;
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
    DropCol_LobbyMaps.Hide;
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
  fGameApp.Networking.NetGameOptions.Peacetime := EnsureRange(TrackBar_LobbyPeacetime.Position, 0, 300);
  fGameApp.Networking.SendGameOptions;

  //Refresh the data to controls
  Lobby_OnGameOptions(nil);
end;


procedure TKMMainMenuInterface.Lobby_OnGameOptions(Sender: TObject);
begin
  TrackBar_LobbyPeacetime.Position := fGameApp.Networking.NetGameOptions.Peacetime;
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
    fGameApp.Networking.NetPlayers.HostDoesSetup := CheckBox_LobbyHostControl.Checked;
    fGameApp.Networking.SendPlayerListAndRefreshPlayersSetup;
  end;

  for i:=0 to MAX_PLAYERS-1 do
  begin
    //Starting location
    if (Sender = DropBox_LobbyLoc[i]) and DropBox_LobbyLoc[i].Enabled then
    begin
      fGameApp.Networking.SelectLoc(DropBox_LobbyLoc[i].ItemIndex, i+1);
      //Host with HostDoesSetup could have given us some location we don't know about from a map/save we don't have
      if fGameApp.Networking.SelectGameKind <> ngk_None then
        DropBox_LobbyLoc[i].ItemIndex := fGameApp.Networking.NetPlayers[i+1].StartLocation;
    end;

    //Team
    if (Sender = DropBox_LobbyTeam[i]) and DropBox_LobbyTeam[i].Enabled then
      fGameApp.Networking.SelectTeam(DropBox_LobbyTeam[i].ItemIndex, i+1);

    //Color
    if (Sender = Drop_LobbyColors[i]) and Drop_LobbyColors[i].Enabled then
    begin
      fGameApp.Networking.SelectColor(Drop_LobbyColors[i].ItemIndex, i+1);
      Drop_LobbyColors[i].ItemIndex := fGameApp.Networking.NetPlayers[i+1].FlagColorID;
    end;

    if Sender = DropBox_LobbyPlayerSlot[i] then
    begin
      //Modify an existing player
      if (i < fGameApp.Networking.NetPlayers.Count) then
      begin
        case DropBox_LobbyPlayerSlot[i].ItemIndex of
          0: //Open
            begin
              if fGameApp.Networking.NetPlayers[i+1].IsComputer then
                fGameApp.Networking.NetPlayers.RemAIPlayer(i+1)
              else if fGameApp.Networking.NetPlayers[i+1].IsClosed then
                fGameApp.Networking.NetPlayers.RemClosedPlayer(i+1);
            end;
          1: //Closed
            fGameApp.Networking.NetPlayers.AddClosedPlayer(i+1); //Replace it
          2: //AI
            fGameApp.Networking.NetPlayers.AddAIPlayer(i+1); //Replace it
        end;
      end
      else
      begin
        //Add a new player
        if DropBox_LobbyPlayerSlot[i].ItemIndex = 1 then //Closed
          fGameApp.Networking.NetPlayers.AddClosedPlayer;
        if DropBox_LobbyPlayerSlot[i].ItemIndex = 2 then //AI
        begin
          fGameApp.Networking.NetPlayers.AddAIPlayer;
          if fGameApp.Networking.SelectGameKind = ngk_Save then
            fGameApp.Networking.MatchPlayersToSave(fGameApp.Networking.NetPlayers.Count); //Match new AI player in save
        end;
      end;
      fGameApp.Networking.SendPlayerListAndRefreshPlayersSetup;
    end;
  end;
end;


//Players list has been updated
//We should reflect it to UI
procedure TKMMainMenuInterface.Lobby_OnPlayersSetup(Sender: TObject);
var
  I,ID,LocaleID: Integer;
  MyNik, CanEdit, HostCanEdit, IsSave, IsValid: Boolean;
begin
  IsSave := fGameApp.Networking.SelectGameKind = ngk_Save;

  //Go through active players first
  for I:=0 to fGameApp.Networking.NetPlayers.Count - 1 do
  begin
    //Flag icon
    LocaleID := fLocales.GetIDFromCode(fGameApp.Networking.NetPlayers[I+1].LangCode);
    if LocaleID <> -1 then
      Image_LobbyFlag[I].TexID := fLocales[LocaleID].FlagSpriteID
    else
      if fGameApp.Networking.NetPlayers[I+1].IsComputer then
        Image_LobbyFlag[I].TexID := 62 //PC icon
      else
        Image_LobbyFlag[I].TexID := 0;

    //Players list
    if fGameApp.Networking.IsHost and (not fGameApp.Networking.NetPlayers[I+1].IsHuman) then
    begin
      Label_LobbyPlayer[I].Hide;
      DropBox_LobbyPlayerSlot[I].Enable;
      DropBox_LobbyPlayerSlot[I].Show;
      if fGameApp.Networking.NetPlayers[I+1].IsComputer then
        DropBox_LobbyPlayerSlot[I].ItemIndex := 2 //AI
      else
        DropBox_LobbyPlayerSlot[I].ItemIndex := 1; //Closed
    end
    else
    begin
      Label_LobbyPlayer[I].Caption := fGameApp.Networking.NetPlayers[I+1].GetNickname;
      if fGameApp.Networking.NetPlayers[I+1].FlagColorID = 0 then
        Label_LobbyPlayer[I].FontColor := $FFFFFFFF
      else
        Label_LobbyPlayer[I].FontColor := FlagColorToTextColor(fGameApp.Networking.NetPlayers[I+1].FlagColor);
      Label_LobbyPlayer[I].Show;
      DropBox_LobbyPlayerSlot[I].Disable;
      DropBox_LobbyPlayerSlot[I].Hide;
      DropBox_LobbyPlayerSlot[I].ItemIndex := 0; //Open
    end;

    //If we can't load the map, don't attempt to show starting locations
    IsValid := false;
    if fGameApp.Networking.SelectGameKind = ngk_Save then
      IsValid := fGameApp.Networking.SaveInfo.IsValid;
    if fGameApp.Networking.SelectGameKind = ngk_Map then
      IsValid := fGameApp.Networking.MapInfo.IsValid;
    if IsValid then
      DropBox_LobbyLoc[I].ItemIndex := fGameApp.Networking.NetPlayers[I+1].StartLocation
    else
      DropBox_LobbyLoc[I].ItemIndex := 0;

    DropBox_LobbyTeam[I].ItemIndex := fGameApp.Networking.NetPlayers[I+1].Team;
    Drop_LobbyColors[I].ItemIndex := fGameApp.Networking.NetPlayers[I+1].FlagColorID;
    if fGameApp.Networking.NetPlayers[I+1].IsClosed then
      Image_LobbyReady[I].TexID := 0
    else
      Image_LobbyReady[I].TexID := 32+Byte(fGameApp.Networking.NetPlayers[I+1].ReadyToStart);

    MyNik := (I+1 = fGameApp.Networking.MyIndex); //Our index
    //We are allowed to edit if it is our nickname and we are set as NOT ready,
    //or we are the host and this player is an AI
    CanEdit := (MyNik and (fGameApp.Networking.IsHost or not fGameApp.Networking.NetPlayers.HostDoesSetup) and
                          (fGameApp.Networking.IsHost or not fGameApp.Networking.NetPlayers[I+1].ReadyToStart)) or
               (fGameApp.Networking.IsHost and fGameApp.Networking.NetPlayers[I+1].IsComputer);
    HostCanEdit := (fGameApp.Networking.IsHost and fGameApp.Networking.NetPlayers.HostDoesSetup and
                    not fGameApp.Networking.NetPlayers[I+1].IsClosed);
    DropBox_LobbyLoc[I].Enabled := (CanEdit or HostCanEdit);
    DropBox_LobbyTeam[I].Enabled := (CanEdit or HostCanEdit) and not IsSave; //Can't change color or teams in a loaded save
    Drop_LobbyColors[I].Enabled := (CanEdit or (MyNik and not fGameApp.Networking.NetPlayers[I+1].ReadyToStart)) and not IsSave;
    if MyNik and not fGameApp.Networking.IsHost then
    begin
      if fGameApp.Networking.NetPlayers[I+1].ReadyToStart then
        Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_NOT_READY]
      else
        Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_READY];
    end
  end;

  //Disable rest of the players
  for I := fGameApp.Networking.NetPlayers.Count to MAX_PLAYERS - 1 do
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
    DropBox_LobbyPlayerSlot[I].Enabled := fGameApp.Networking.IsHost and (I = fGameApp.Networking.NetPlayers.Count);
    Image_LobbyReady[I].TexID := 0; //Hidden
    DropBox_LobbyLoc[I].Disable;
    DropBox_LobbyTeam[I].Disable;
    Drop_LobbyColors[I].Disable;
  end;

  //Update the minimap preivew with player colors
  for I := 1 to MAX_PLAYERS do
  begin
    ID := fGameApp.Networking.NetPlayers.StartingLocToLocal(I);
    if ID <> -1 then
      fMinimap.PlayerColors[I] := fGameApp.Networking.NetPlayers[ID].FlagColor
    else
      fMinimap.PlayerColors[I] := $7F000000; //Semi-transparent when not selected
  end;
  //If we have a map selected update the preview
  if (fGameApp.Networking.SelectGameKind = ngk_Map) and fGameApp.Networking.MapInfo.IsValid then
  begin
    fMinimap.Update(True);
    MinimapView_Lobby.SetMinimap(fMinimap);
  end;

  CheckBox_LobbyHostControl.Checked := fGameApp.Networking.NetPlayers.HostDoesSetup;
  if fGameApp.Networking.IsHost then
    Button_LobbyStart.Enabled := fGameApp.Networking.CanStart;
  //If the game can't be started the text message with explanation will appear in chat area
end;


procedure TKMMainMenuInterface.Lobby_OnPingInfo(Sender: TObject);
var i:integer;
begin
  for i:=0 to MAX_PLAYERS-1 do
  if (fGameApp.Networking.Connected) and (i < fGameApp.Networking.NetPlayers.Count) and
     (fGameApp.Networking.NetPlayers[i+1].IsHuman) then
  begin
    Label_LobbyPing[i].Caption := IntToStr(fGameApp.Networking.NetPlayers[i+1].GetInstantPing);
    Label_LobbyPing[i].FontColor := GetPingColor(fGameApp.Networking.NetPlayers[i+1].GetInstantPing);
  end
  else
    Label_LobbyPing[i].Caption := '';
  Label_LobbyServerName.Caption := fGameApp.Networking.ServerName+' #'+IntToStr(fGameApp.Networking.ServerRoom+1)+
                                   '  '+fGameApp.Networking.ServerAddress+' : '+fGameApp.Networking.ServerPort;
end;


procedure TKMMainMenuInterface.Lobby_MapTypeSelect(Sender: TObject);
begin
  //Terminate any running scans otherwise they will continue to fill the drop box in the background
  fMapsMP.TerminateScan;
  fSavesMP.TerminateScan;
  DropCol_LobbyMaps.Clear; //Clear previous items in case scanning finds no maps/saves
  case Radio_LobbyMapType.ItemIndex of
    0:  //Build Map
        begin
          fMapsMP.Refresh(Lobby_ScanUpdate);
          DropCol_LobbyMaps.DefaultCaption := fTextLibrary[TX_LOBBY_MAP_SELECT];
          DropCol_LobbyMaps.List.Header.Columns[0].Caption := fTextLibrary[TX_MENU_MAP_TITLE];
          DropCol_LobbyMaps.List.Header.Columns[2].Caption := fTextLibrary[TX_MENU_MAP_SIZE];
        end;
    1:  //Fight Map
        begin
          fMapsMP.Refresh(Lobby_ScanUpdate);
          DropCol_LobbyMaps.DefaultCaption := fTextLibrary[TX_LOBBY_MAP_SELECT];
          DropCol_LobbyMaps.List.Header.Columns[0].Caption := fTextLibrary[TX_MENU_MAP_TITLE];
          DropCol_LobbyMaps.List.Header.Columns[2].Caption := fTextLibrary[TX_MENU_MAP_SIZE];
        end;
    2:  //Co-op Map
        begin
          fMapsMP.Refresh(Lobby_ScanUpdate);
          DropCol_LobbyMaps.DefaultCaption := fTextLibrary[TX_LOBBY_MAP_SELECT];
          DropCol_LobbyMaps.List.Header.Columns[0].Caption := fTextLibrary[TX_MENU_MAP_TITLE];
          DropCol_LobbyMaps.List.Header.Columns[2].Caption := fTextLibrary[TX_MENU_MAP_SIZE];
        end;
    3:  //Saved Game
        begin
          fMapsMP.TerminateScan;
          fSavesMP.Refresh(Lobby_ScanUpdate, True);
          DropCol_LobbyMaps.DefaultCaption := fTextLibrary[TX_LOBBY_MAP_SELECT_SAVED];
          DropCol_LobbyMaps.List.Header.Columns[0].Caption := fTextLibrary[TX_MENU_LOAD_FILE];
          DropCol_LobbyMaps.List.Header.Columns[2].Caption := fTextLibrary[TX_MENU_SAVE_TIME];
        end;
    else
        begin
          DropCol_LobbyMaps.DefaultCaption := '<<<LEER>>>';
        end;
  end;
  DropCol_LobbyMaps.ItemIndex := -1; //Clear previously selected item

  //The Sender is nil in Reset_Lobby when we are not connected
  if Sender <> nil then
    fGameApp.Networking.SelectNoMap(fTextLibrary[TX_LOBBY_MAP_NONE]);
end;


procedure TKMMainMenuInterface.Lobby_SortUpdate(Sender: TObject);
begin
  //After sorting jump to the selected item
  if Sender = fSavesMP then
    Lobby_RefreshSaveList(True);
  if Sender = fMapsMP then
    Lobby_RefreshMapList(True);
end;


procedure TKMMainMenuInterface.Lobby_ScanUpdate(Sender: TObject);
begin
  //Don't jump to selected with each scan update
  if Sender = fSavesMP then
    Lobby_RefreshSaveList(False);
  if Sender = fMapsMP then
    Lobby_RefreshMapList(False);
end;


procedure TKMMainMenuInterface.Lobby_RefreshMapList(aJumpToSelected:Boolean);
var
  I, OldTopIndex: Integer;
  PrevMap: string;
  AddMap: Boolean;
begin
  //Remember previous map selected
  if DropCol_LobbyMaps.ItemIndex <> -1 then
    PrevMap := DropCol_LobbyMaps.Item[DropCol_LobbyMaps.ItemIndex].Cells[0].Caption
  else
    PrevMap := '';

  OldTopIndex := DropCol_LobbyMaps.List.TopIndex;
  DropCol_LobbyMaps.Clear;

  for I := 0 to fMapsMP.Count - 1 do
  begin
    //Different modes allow different maps
    case Radio_LobbyMapType.ItemIndex of
      0:    AddMap := (fMapsMP[I].Info.MissionMode = mm_Normal) and not fMapsMP[I].IsCoop; //BuildMap
      1:    AddMap := (fMapsMP[I].Info.MissionMode = mm_Tactic) and not fMapsMP[I].IsCoop; //FightMap
      2:    AddMap := fMapsMP[I].IsCoop; //CoopMap
      else  AddMap := False; //Other cases are already handled in Lobby_MapTypeSelect
    end;

    if AddMap then
      DropCol_LobbyMaps.Add(MakeListRow([fMapsMP[I].Info.Title,
                                         IntToStr(fMapsMP[I].Info.PlayerCount),
                                         fMapsMP[I].Info.MapSizeText], I));
  end;

  //Restore previously selected map
  if PrevMap <> '' then
    for I := 0 to DropCol_LobbyMaps.Count - 1 do
      if DropCol_LobbyMaps.Item[I].Cells[0].Caption = PrevMap then
        DropCol_LobbyMaps.ItemIndex := I;

  //Restore the top index
  DropCol_LobbyMaps.List.TopIndex := OldTopIndex;
  if aJumpToSelected and (DropCol_LobbyMaps.List.ItemIndex <> -1)
  and not InRange(DropCol_LobbyMaps.List.ItemIndex - DropCol_LobbyMaps.List.TopIndex, 0, DropCol_LobbyMaps.List.GetVisibleRows - 1) then
  begin
    if DropCol_LobbyMaps.List.ItemIndex < DropCol_LobbyMaps.List.TopIndex + DropCol_LobbyMaps.List.GetVisibleRows - 1 then
      DropCol_LobbyMaps.List.TopIndex := DropCol_LobbyMaps.List.ItemIndex
    else
    if DropCol_LobbyMaps.List.ItemIndex > DropCol_LobbyMaps.List.TopIndex + DropCol_LobbyMaps.List.GetVisibleRows - 1 then
      DropCol_LobbyMaps.List.TopIndex := DropCol_LobbyMaps.List.ItemIndex - DropCol_LobbyMaps.List.GetVisibleRows + 1;
  end;
end;


procedure TKMMainMenuInterface.Lobby_RefreshSaveList(aJumpToSelected:Boolean);
var I, OldTopIndex: Integer; PrevSave: string;
begin
  //Remember previous save selected
  if DropCol_LobbyMaps.ItemIndex <> -1 then
    PrevSave := DropCol_LobbyMaps.Item[DropCol_LobbyMaps.ItemIndex].Cells[0].Caption
  else
    PrevSave := '';

  OldTopIndex := DropCol_LobbyMaps.List.TopIndex;
  DropCol_LobbyMaps.Clear;
  for I := 0 to fSavesMP.Count - 1 do
    if fSavesMP[I].IsValid then
      DropCol_LobbyMaps.Add(MakeListRow([fSavesMP[I].FileName,
                                         IntToStr(fSavesMP[I].Info.PlayerCount),
                                         fSavesMP[I].Info.GetTimeText], I))
    else
      DropCol_LobbyMaps.Add(MakeListRow([fSavesMP[I].FileName, '', ''], I));

  //Restore previously selected save
  if PrevSave <> '' then
    for I := 0 to DropCol_LobbyMaps.Count - 1 do
      if DropCol_LobbyMaps.Item[I].Cells[0].Caption = PrevSave then
        DropCol_LobbyMaps.ItemIndex := I;

  //Restore the top index
  DropCol_LobbyMaps.List.TopIndex := OldTopIndex;
  if aJumpToSelected and (DropCol_LobbyMaps.List.ItemIndex <> -1)
  and not InRange(DropCol_LobbyMaps.List.ItemIndex - DropCol_LobbyMaps.List.TopIndex, 0, DropCol_LobbyMaps.List.GetVisibleRows - 1) then
  begin
    if DropCol_LobbyMaps.List.ItemIndex < DropCol_LobbyMaps.List.TopIndex + DropCol_LobbyMaps.List.GetVisibleRows - 1 then
      DropCol_LobbyMaps.List.TopIndex := DropCol_LobbyMaps.List.ItemIndex
    else
    if DropCol_LobbyMaps.List.ItemIndex > DropCol_LobbyMaps.List.TopIndex + DropCol_LobbyMaps.List.GetVisibleRows - 1 then
      DropCol_LobbyMaps.List.TopIndex := DropCol_LobbyMaps.List.ItemIndex - DropCol_LobbyMaps.List.GetVisibleRows + 1;
  end;
end;


procedure TKMMainMenuInterface.Lobby_MapColumnClick(aValue: Integer);
var
  SM: TMapsSortMethod;
  SSM: TSavesSortMethod;
begin

  if Radio_LobbyMapType.ItemIndex < 3 then
  begin
    //Determine Sort method depending on which column user clicked
    with DropCol_LobbyMaps.List do
    case SortIndex of
      0:  if SortDirection = sdDown then
            SM := smByNameDesc
          else
            SM := smByNameAsc;
      1:  if SortDirection = sdDown then
            SM := smByPlayersDesc
          else
            SM := smByPlayersAsc;
      2:  if SortDirection = sdDown then
            SM := smBySizeDesc
          else
            SM := smBySizeAsc;
      else SM := smByNameAsc;
    end;
    fMapsMP.Sort(SM, Lobby_SortUpdate);
  end
  else
  begin
    //Determine Sort method depending on which column user clicked
    with DropCol_LobbyMaps.List do
    case SortIndex of
      0:  if SortDirection = sdDown then
            SSM := smByFileNameDesc
          else
            SSM := smByFileNameAsc;
      1:  if SortDirection = sdDown then
            SSM := smByPlayerCountDesc
          else
            SSM := smByPlayerCountAsc;
      2:  if SortDirection = sdDown then
            SSM := smByTimeDesc
          else
            SSM := smByTimeAsc;
      else SSM := smByFileNameAsc;
    end;
    fSavesMP.Sort(SSM, Lobby_SortUpdate);
  end;
end;


//Just pass FileName to Networking, it will check validity itself
procedure TKMMainMenuInterface.Lobby_MapSelect(Sender: TObject);
begin
  if Radio_LobbyMapType.ItemIndex < 3 then
    fGameApp.Networking.SelectMap(fMapsMP[DropCol_LobbyMaps.Item[DropCol_LobbyMaps.ItemIndex].Tag].Info.Title)
  else
    fGameApp.Networking.SelectSave(fSavesMP[DropCol_LobbyMaps.Item[DropCol_LobbyMaps.ItemIndex].Tag].FileName);
end;


procedure TKMMainMenuInterface.Lobby_OnMapName(const aData: string);
var
  I: Integer;
  DropText: string;
  M: TKMapInfo;
begin
  //Common settings
  MinimapView_Lobby.Visible := (fGameApp.Networking.SelectGameKind = ngk_Map) and fGameApp.Networking.MapInfo.IsValid;
  TrackBar_LobbyPeacetime.Enabled := fGameApp.Networking.IsHost and (fGameApp.Networking.SelectGameKind = ngk_Map) and fGameApp.Networking.MapInfo.IsValid;

  case  fGameApp.Networking.SelectGameKind of
    ngk_None: begin
                Memo_LobbyMapDesc.Clear;
                if aData = fTextLibrary[TX_LOBBY_MAP_NONE] then
                  Label_LobbyMapName.Caption := aData
                else
                begin
                  Label_LobbyMapName.Caption := '';
                  Memo_LobbyMapDesc.Text := aData; //aData is some error message
                end;

                //Starting locations text
                DropText := fTextLibrary[TX_LOBBY_RANDOM] + eol;
              end;
    ngk_Save: begin
                if not fGameApp.Networking.IsHost then
                  Radio_LobbyMapType.ItemIndex := 3;

                Label_LobbyMapName.Caption := fGameApp.Networking.SaveInfo.FileName;
                Memo_LobbyMapDesc.Text := fGameApp.Networking.GameInfo.GetTitleWithTime;

                //Starting locations text
                DropText := fTextLibrary[TX_LOBBY_SELECT] + eol;
                for I := 0 to fGameApp.Networking.GameInfo.PlayerCount - 1 do
                  DropText := DropText + fGameApp.Networking.GameInfo.LocationName[I] + eol;
              end;
    ngk_Map:  begin
                M := fGameApp.Networking.MapInfo;
                if not fGameApp.Networking.IsHost then
                begin
                  if M.IsCoop then
                    Radio_LobbyMapType.ItemIndex := 2
                  else
                    if M.Info.MissionMode = mm_Tactic then
                      Radio_LobbyMapType.ItemIndex := 1
                    else
                      Radio_LobbyMapType.ItemIndex := 0;
                end;

                //Only load the minimap preview if the map is valid
                if M.IsValid then
                begin
                  fMinimap.LoadFromMission(M.FullPath('.dat'));
                  fMinimap.Update(True);
                  MinimapView_Lobby.SetMinimap(fMinimap);
                end;
                Label_LobbyMapName.Caption := fGameApp.Networking.GameInfo.Title;
                Memo_LobbyMapDesc.Text := M.BigDesc;

              //Starting locations text
              DropText := fTextLibrary[TX_LOBBY_RANDOM] + eol;
              for I := 0 to fGameApp.Networking.GameInfo.PlayerCount - 1 do
                DropText := DropText + fGameApp.Networking.GameInfo.LocationName[I] + eol;
            end;
  end;

  for I := 0 to MAX_PLAYERS - 1 do
    DropBox_LobbyLoc[I].SetItems(DropText);
end;


//We have been assigned to be the host of the game because the host disconnected. Reopen lobby page in correct mode.
procedure TKMMainMenuInterface.Lobby_OnReassignedToHost(Sender: TObject);
  procedure SelectByName(aName: string);
  var I: Integer;
  begin
    DropCol_LobbyMaps.ItemIndex := -1;
    for I := 0 to DropCol_LobbyMaps.Count - 1 do
      if DropCol_LobbyMaps.Item[I].Cells[0].Caption = aName then
      begin
        DropCol_LobbyMaps.ItemIndex := I;
        Break;
      end;
  end;
var OldMapType: byte;
begin
  Lobby_Reset(Button_MP_CreateLAN, True, True); //Will reset the lobby page into host mode, preserving messages/maps
  OldMapType := Radio_LobbyMapType.ItemIndex;
  if fGameApp.Networking.SelectGameKind = ngk_None then
    Radio_LobbyMapType.ItemIndex := 0 //Default
  else
    if fGameApp.Networking.SelectGameKind = ngk_Save then
      Radio_LobbyMapType.ItemIndex := 3
    else
      if fGameApp.Networking.MapInfo.IsCoop then
        Radio_LobbyMapType.ItemIndex := 2
      else
        if fGameApp.Networking.MapInfo.Info.MissionMode = mm_Tactic then
          Radio_LobbyMapType.ItemIndex := 1
        else
          Radio_LobbyMapType.ItemIndex := 0;

  //Don't force rescanning all the maps unless the map type changed or no map was selected
  if (Radio_LobbyMapType.ItemIndex <> OldMapType) or (DropCol_LobbyMaps.ItemIndex = -1) then
    Lobby_MapTypeSelect(nil)
  else
    Lobby_RefreshMapList(False); //Just fill the list from fMapMP

  if fGameApp.Networking.SelectGameKind = ngk_Save then
    SelectByName(fGameApp.Networking.SaveInfo.FileName) //Select the map
  else
    if fGameApp.Networking.SelectGameKind = ngk_Map then
      SelectByName(fGameApp.Networking.MapInfo.FileName); //Select the map

  Lobby_OnGameOptions(nil);
  if fGameApp.Networking.SelectGameKind = ngk_Save then
    Lobby_OnMapName(fGameApp.Networking.SaveInfo.FileName)
  else
    if fGameApp.Networking.SelectGameKind = ngk_Map then
      Lobby_OnMapName(fGameApp.Networking.MapInfo.FileName);
end;


//Post what user has typed
procedure TKMMainMenuInterface.Lobby_PostKey(Sender: TObject; Key: Word);
var ChatMessage: string;
begin
  if (Key <> VK_RETURN) or (Trim(Edit_LobbyPost.Text) = '') then exit;
  ChatMessage := Edit_LobbyPost.Text;
  //Check for console commands
  if (Length(ChatMessage) > 1) and (ChatMessage[1] = '/')
  and (ChatMessage[2] <> '/') then //double slash is the escape to place a slash at the start of a sentence
    fGameApp.Networking.ConsoleCommand(ChatMessage)
  else
  begin
    if (Length(ChatMessage) > 1) and (ChatMessage[1] = '/') and (ChatMessage[2] = '/') then
      Delete(ChatMessage, 1, 1); //Remove one of the /'s
    fGameApp.Networking.PostMessage(ChatMessage, True);
  end;

  Edit_LobbyPost.Text := '';
end;


procedure TKMMainMenuInterface.Lobby_OnMessage(const aData:string);
begin
  Memo_LobbyPosts.Add(aData);
end;


//We were disconnected from Server. Either we were kicked, or connection broke down
procedure TKMMainMenuInterface.Lobby_OnDisconnect(const aData:string);
begin
  fGameApp.Networking.Disconnect;
  MP_Update(aData,icYellow,false);
  fSoundLib.Play(sfxn_Error);
  SwitchMenuPage(Button_LobbyBack);
end;


procedure TKMMainMenuInterface.Lobby_BackClick(Sender: TObject);
begin
  fGameApp.Networking.AnnounceDisconnect;
  fGameApp.Networking.Disconnect;
  MP_Update(fTextLibrary[TX_GAME_ERROR_DISCONNECT],icYellow,false);
  SwitchMenuPage(Button_LobbyBack);
end;


procedure TKMMainMenuInterface.Lobby_StartClick(Sender: TObject);
begin
  if fGameApp.Networking.IsHost then
    fGameApp.Networking.StartClick
  else
  begin
    if fGameApp.Networking.ReadyToStart then
      Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_NOT_READY]
    else
      Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_READY];
  end;
end;


procedure TKMMainMenuInterface.Load_ListClick(Sender: TObject);
begin
  //Hide delete confirmation if player has selected a different savegame item
  if Sender = List_Load then
    Load_DeleteConfirmation(False);

  Button_Delete.Enabled := InRange(List_Load.ItemIndex, 0, fSaves.Count-1);
  Button_Load.Enabled := InRange(List_Load.ItemIndex, 0, fSaves.Count-1)
                         and fSaves[List_Load.ItemIndex].IsValid;

  if InRange(List_Load.ItemIndex, 0, fSaves.Count-1) then
    fLastSaveCRC := fSaves[List_Load.ItemIndex].CRC
  else
    fLastSaveCRC := 0;

  MinimapView_Load.Hide; //Hide by default, then show it if we load the map successfully
  if Button_Load.Enabled and fSaves[List_Load.ItemIndex].LoadMinimap(fMinimap) then
  begin
    MinimapView_Load.SetMinimap(fMinimap);
    MinimapView_Load.Show;
  end;
end;


procedure TKMMainMenuInterface.Load_Click(Sender: TObject);
begin
  if not Button_Load.Enabled then exit; //This is also called by double clicking
  if not InRange(List_Load.ItemIndex, 0, fSaves.Count-1) then Exit;
  fSaves.TerminateScan; //stop scan as it is no longer needed
  fGameApp.NewSingleSave(fSaves[List_Load.ItemIndex].FileName);
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
    Load_RefreshList(False);
    if List_Load.RowCount > 0 then
      List_Load.ItemIndex := EnsureRange(PreviouslySelected, 0, List_Load.RowCount - 1)
    else
      List_Load.ItemIndex := -1; //there are no saves, nothing to select
    Load_ListClick(List_Load);
  end;
end;


procedure TKMMainMenuInterface.Load_ScanUpdate(Sender: TObject);
begin
  Load_RefreshList(False); //Don't jump to selected with each scan update
end;


procedure TKMMainMenuInterface.Load_SortUpdate(Sender: TObject);
begin
  Load_RefreshList(True); //After sorting jump to the selected item
end;


procedure TKMMainMenuInterface.Load_RefreshList(aJumpToSelected:Boolean);
var I, OldTopIndex: Integer;
begin
  OldTopIndex := List_Load.TopIndex;
  List_Load.Clear;

  for I := 0 to fSaves.Count - 1 do
    List_Load.AddItem(MakeListRow(
                      [fSaves[i].FileName, fSaves[I].Info.GetTitleWithTime],
                      [$FFFFFFFF, $FFFFFFFF]));

  //IDs of saves could changed, so use CRC to check which one was selected
  for I := 0 to fSaves.Count - 1 do
    if (fSaves[I].CRC = fLastSaveCRC) then
      List_Load.ItemIndex := I;

  List_Load.TopIndex := OldTopIndex;

  if aJumpToSelected and (List_Load.ItemIndex <> -1)
  and not InRange(List_Load.ItemIndex - List_Load.TopIndex, 0, List_Load.GetVisibleRows - 1)
  then
  begin
    if List_Load.ItemIndex < List_Load.TopIndex + List_Load.GetVisibleRows - 1 then
      List_Load.TopIndex := List_Load.ItemIndex
    else
      if List_Load.ItemIndex > List_Load.TopIndex + List_Load.GetVisibleRows - 1 then
        List_Load.TopIndex := List_Load.ItemIndex - List_Load.GetVisibleRows + 1;
  end;

  Load_ListClick(nil);
end;


procedure TKMMainMenuInterface.Load_Sort(aIndex: Integer);
begin
  case List_Load.SortIndex of
    //Sorting by filename goes A..Z by default
    0:  if List_Load.SortDirection = sdDown then
          fSaves.Sort(smByFileNameDesc, Load_SortUpdate)
        else
          fSaves.Sort(smByFileNameAsc, Load_SortUpdate);
    //Sorting by description goes A..Z by default
    1:  if List_Load.SortDirection = sdDown then
          fSaves.Sort(smByDescriptionDesc, Load_SortUpdate)
        else
          fSaves.Sort(smByDescriptionAsc, Load_SortUpdate);
  end;
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
var ID: Integer;
begin
  ID := List_Replays.ItemIndex;

  Button_ReplaysPlay.Enabled := InRange(ID, 0, fSaves.Count-1)
                                and fSaves[ID].IsValid
                                and fSaves[ID].IsReplayValid;

  if InRange(ID, 0, fSaves.Count-1) then
    fLastSaveCRC := fSaves[ID].CRC
  else
    fLastSaveCRC := 0;

  MinimapView_Replay.Hide; //Hide by default, then show it if we load the map successfully
  if Button_ReplaysPlay.Enabled and fSaves[ID].LoadMinimap(fMinimap) then
  begin
    MinimapView_Replay.SetMinimap(fMinimap);
    MinimapView_Replay.Show;
  end;
end;


procedure TKMMainMenuInterface.Replay_TypeChange(Sender: TObject);
begin
  fSaves.TerminateScan;
  fLastSaveCRC := 0;
  List_Replays.Clear;
  Replays_ListClick(nil);
  fSaves.Refresh(Replays_ScanUpdate, (Radio_Replays_Type.ItemIndex = 1));
end;


procedure TKMMainMenuInterface.Replays_ScanUpdate(Sender: TObject);
begin
  Replays_RefreshList(False); //Don't jump to selected with each scan update
end;


procedure TKMMainMenuInterface.Replays_SortUpdate(Sender: TObject);
begin
  Replays_RefreshList(True); //After sorting jump to the selected item
end;


procedure TKMMainMenuInterface.Replays_RefreshList(aJumpToSelected:Boolean);
var
  I, OldTopIndex: Integer;
begin
  OldTopIndex := List_Replays.TopIndex;
  List_Replays.Clear;

  for I := 0 to fSaves.Count - 1 do
    List_Replays.AddItem(MakeListRow(
                         [fSaves[I].FileName, fSaves[I].Info.GetTitleWithTime],
                         [$FFFFFFFF, $FFFFFFFF]));

  for I := 0 to fSaves.Count - 1 do
    if (fSaves[I].CRC = fLastSaveCRC) then
      List_Replays.ItemIndex := I;

  List_Replays.TopIndex := OldTopIndex;

  if aJumpToSelected and (List_Replays.ItemIndex <> -1)
  and not InRange(List_Replays.ItemIndex - List_Replays.TopIndex, 0, List_Replays.GetVisibleRows-1)
  then
    if List_Replays.ItemIndex < List_Replays.TopIndex then
      List_Replays.TopIndex := List_Replays.ItemIndex
    else
    if List_Replays.ItemIndex > List_Replays.TopIndex + List_Replays.GetVisibleRows - 1 then
      List_Replays.TopIndex := List_Replays.ItemIndex - List_Replays.GetVisibleRows + 1;
end;


procedure TKMMainMenuInterface.Replays_Sort(aIndex: Integer);
begin
  case List_Replays.SortIndex of
    //Sorting by filename goes A..Z by default
    0:  if List_Replays.SortDirection = sdDown then
          fSaves.Sort(smByFileNameDesc, Replays_SortUpdate)
        else
          fSaves.Sort(smByFileNameAsc, Replays_SortUpdate);
    //Sorting by description goes A..Z by default
    1:  if List_Replays.SortDirection = sdDown then
          fSaves.Sort(smByDescriptionDesc, Replays_SortUpdate)
        else
          fSaves.Sort(smByDescriptionAsc, Replays_SortUpdate);
  end;
end;


procedure TKMMainMenuInterface.Replays_Play(Sender: TObject);
var
  ID: Integer;
begin
  if not Button_ReplaysPlay.Enabled then exit; //This is also called by double clicking

  ID := List_Replays.ItemIndex;
  if not InRange(ID, 0, fSaves.Count-1) then Exit;
  fSaves.TerminateScan; //stop scan as it is no longer needed
  fGameApp.NewReplay(fSaves[ID].Path + fSaves[ID].FileName + '.bas');
end;


procedure TKMMainMenuInterface.MapEditor_Start(Sender: TObject);
var
  MapEdSizeX, MapEdSizeY: Integer;
  ID: Integer;
  Maps: TKMapsCollection;
begin
  if Sender = Button_MapEd_Create then
  begin
    MapEdSizeX := MapSize[Radio_MapEd_SizeX.ItemIndex+1];
    MapEdSizeY := MapSize[Radio_MapEd_SizeY.ItemIndex+1];
    fGameApp.NewMapEditor('', MapEdSizeX, MapEdSizeY);
  end;

  //This is also called by double clicking on a map in the list
  if ((Sender = Button_MapEd_Load) or (Sender = List_MapEd)) and
     Button_MapEd_Load.Enabled and (List_MapEd.ItemIndex <> -1) then
  begin
    ID := List_MapEd.Rows[List_MapEd.ItemIndex].Tag;
    if Radio_MapEd_MapType.ItemIndex = 0 then Maps := fMaps else Maps := fMapsMP;

    fGameApp.NewMapEditor(Maps[ID].FullPath('.dat'), 0, 0);
    //Keep MP/SP selected in the map editor interface
    //(if mission failed to load we would have fGame = nil)
    if (fGame <> nil) and (fGame.MapEditorInterface <> nil) then
      fGame.MapEditorInterface.SetLoadMode(Radio_MapEd_MapType.ItemIndex = 1);
  end;
end;


procedure TKMMainMenuInterface.MapEditor_MapTypeChange(Sender: TObject);
begin
  MapEditor_ListUpdate;
end;


//Clear the list and initiate refresh
procedure TKMMainMenuInterface.MapEditor_ListUpdate;
var
  Maps: TKMapsCollection;
begin
  //Terminate both
  fMaps.TerminateScan;
  fMapsMP.TerminateScan;

  List_MapEd.Clear;
  fLastMapCRC := 0;
  MapEditor_SelectMap(nil);

  //If both Maps and MapsMP are scanning at once ListUpdateDone can be called from either one
  //meaning we can access inconsistent and trigger assertion
  if Radio_MapEd_MapType.ItemIndex = 0 then Maps := fMaps else Maps := fMapsMP;

  Maps.Refresh(MapEditor_ScanUpdate);
end;


procedure TKMMainMenuInterface.MapEditor_ScanUpdate(Sender: TObject);
begin
  MapEditor_RefreshList(False); //Don't jump to selected with each scan update
end;


procedure TKMMainMenuInterface.MapEditor_SortUpdate(Sender: TObject);
begin
  MapEditor_RefreshList(True); //After sorting jump to the selected item
end;


procedure TKMMainMenuInterface.MapEditor_RefreshList(aJumpToSelected:Boolean);
var
  I, OldTopIndex: Integer;
  Maps: TKMapsCollection;
begin
  OldTopIndex := List_MapEd.TopIndex;
  List_MapEd.Clear;

  if Radio_MapEd_MapType.ItemIndex = 0 then Maps := fMaps else Maps := fMapsMP;

  for I := 0 to Maps.Count - 1 do
    List_MapEd.AddItem(MakeListRow([Maps[I].Info.Title, IntToStr(Maps[I].Info.PlayerCount), Maps[I].Info.MapSizeText], I));

  for I := 0 to Maps.Count - 1 do
    if (Maps[I].CRC = fLastMapCRC) then
      List_MapEd.ItemIndex := I;

  List_MapEd.TopIndex := OldTopIndex;

  if aJumpToSelected and (List_MapEd.ItemIndex <> -1)
  and not InRange(List_MapEd.ItemIndex - List_MapEd.TopIndex, 0, List_MapEd.GetVisibleRows-1)
  then
    if List_MapEd.ItemIndex < List_MapEd.TopIndex then
      List_MapEd.TopIndex := List_MapEd.ItemIndex
    else
    if List_MapEd.ItemIndex > List_MapEd.TopIndex + List_MapEd.GetVisibleRows - 1 then
      List_MapEd.TopIndex := List_MapEd.ItemIndex - List_MapEd.GetVisibleRows + 1;
end;


procedure TKMMainMenuInterface.MapEditor_ColumnClick(aValue: Integer);
var
  SM: TMapsSortMethod;
begin
  //Determine Sort method depending on which column user clicked
  with List_MapEd do
  case SortIndex of
    0:  if SortDirection = sdDown then
          SM := smByNameDesc
        else
          SM := smByNameAsc;
    1:  if SortDirection = sdDown then
          SM := smByPlayersDesc
        else
          SM := smByPlayersAsc;
    2:  if SortDirection = sdDown then
          SM := smBySizeDesc
        else
          SM := smBySizeAsc;
    else SM := smByNameAsc;
  end;

  //Keep both lists in sync incase user switches between them
  fMaps.Sort(SM, MapEditor_SortUpdate);
  fMapsMP.Sort(SM, MapEditor_SortUpdate);
end;


procedure TKMMainMenuInterface.MapEditor_SelectMap(Sender: TObject);
var
  ID: Integer;
  Maps: TKMapsCollection;
begin
  Button_MapEd_Load.Enabled := (List_MapEd.ItemIndex <> -1);

  if Button_MapEd_Load.Enabled then
  begin
    ID := List_MapEd.Rows[List_MapEd.ItemIndex].Tag;
    if Radio_MapEd_MapType.ItemIndex = 0 then Maps := fMaps else Maps := fMapsMP;

    fLastMapCRC := Maps[ID].CRC;

    fMinimap.LoadFromMission(Maps[ID].FullPath('.dat'));
    fMinimap.Update(True);
    MinimapView_MapEd.SetMinimap(fMinimap);
    MinimapView_MapEd.Show;
  end
  else
  begin
    MinimapView_MapEd.Hide;
    fLastMapCRC := 0;
  end;
end;


//This is called when the options page is shown, so update all the values
//Note: Options can be required to fill before fGameApp is completely initialized,
//hence we need to pass either fGameApp.Settings or a direct Settings link
procedure TKMMainMenuInterface.Options_Fill(aMainSettings: TMainSettings; aGameSettings: TGameSettings);
begin
  CheckBox_Options_Autosave.Checked     := aGameSettings.Autosave;
  TrackBar_Options_Brightness.Position  := aGameSettings.Brightness;
  RadioGroup_Options_Shadows.ItemIndex  := Byte(aGameSettings.AlphaShadows);
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
  MusicToggled := (fGameApp.GameSettings.MusicOff <> CheckBox_Options_MusicOff.Checked);
  ShuffleToggled := (fGameApp.GameSettings.ShuffleOn <> CheckBox_Options_ShuffleOn.Checked);

  fGameApp.GameSettings.Autosave         := CheckBox_Options_Autosave.Checked;
  fGameApp.GameSettings.Brightness       := TrackBar_Options_Brightness.Position;
  fGameApp.GameSettings.AlphaShadows     := RadioGroup_Options_Shadows.ItemIndex = 1;
  fGameApp.GameSettings.ScrollSpeed      := TrackBar_Options_ScrollSpeed.Position;
  fGameApp.GameSettings.SoundFXVolume    := TrackBar_Options_SFX.Position / TrackBar_Options_SFX.MaxValue;
  fGameApp.GameSettings.MusicVolume      := TrackBar_Options_Music.Position / TrackBar_Options_Music.MaxValue;
  fGameApp.GameSettings.MusicOff         := CheckBox_Options_MusicOff.Checked;
  fGameApp.GameSettings.ShuffleOn        := CheckBox_Options_ShuffleOn.Checked;
  TrackBar_Options_Music.Enabled        := not CheckBox_Options_MusicOff.Checked;
  CheckBox_Options_ShuffleOn.Enabled    := not CheckBox_Options_MusicOff.Checked;

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

  if Sender = Radio_Options_Lang then
  begin
    ShowScreen(msLoading, fTextLibrary[TX_MENU_NEW_LOCALE]);
    fGameApp.Render; //Force to repaint loading screen
    fGameApp.ToggleLocale(fLocales[Radio_Options_Lang.ItemIndex].Code);
    exit; //Whole interface will be recreated
  end;
end;


//Apply resolution changes
procedure TKMMainMenuInterface.Options_ChangeRes(Sender: TObject);
var
  I: Integer;
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

    //Reset refresh rates, because they are different for each resolution
    DropBox_Options_RefreshRate.Clear;
    for I := 0 to fMain.Resolutions.Items[ResID].RefRateCount - 1 do
    begin
      DropBox_Options_RefreshRate.Add(Format('%d Hz', [fMain.Resolutions.Items[ResID].RefRate[I]]));
      //Make sure to select something. SelectedRefRate is prefered, otherwise select first
      if (I = 0) or (fMain.Resolutions.Items[ResID].RefRate[I] = SelectedRefRate) then
        DropBox_Options_RefreshRate.ItemIndex := I;
    end;
  end;

  //Make button enabled only if new resolution/mode differs from old
  ResID := DropBox_Options_Resolution.ItemIndex;
  RefID := DropBox_Options_RefreshRate.ItemIndex;
  Button_Options_ResApply.Enabled :=
      (fMain.Settings.FullScreen <> CheckBox_Options_FullScreen.Checked) or
      (CheckBox_Options_FullScreen.Checked and ((OldResolutionID.ResID <> ResID) or
                                                (OldResolutionID.RefID <> RefID)));
  //Remember which one we have selected so we can reselect it if the user changes resolution
  SelectedRefRate := fMain.Resolutions.Items[ResID].RefRate[RefID];
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
      begin
        DropBox_Options_RefreshRate.ItemIndex := I;
        SelectedRefRate := fMain.Resolutions.Items[R.ResID].RefRate[I];
      end;
    end;
  end
  else
  begin
    //no supported resolutions
    DropBox_Options_Resolution.Add(fTextLibrary[TX_MENU_OPTIONS_RESOLUTION_NOT_SUPPORTED]);
    DropBox_Options_RefreshRate.Add(fTextLibrary[TX_MENU_OPTIONS_REFRESH_RATE_NOT_SUPPORTED]);
    DropBox_Options_Resolution.ItemIndex := 0;
    DropBox_Options_RefreshRate.ItemIndex := 0;
  end;

  CheckBox_Options_FullScreen.Checked := fMain.Settings.FullScreen;
  //Controls should be disabled, when there is no resolution to choose
  CheckBox_Options_FullScreen.Enabled := fMain.Resolutions.Count > 0;
  DropBox_Options_Resolution.Enabled  := (fMain.Settings.FullScreen) and (fMain.Resolutions.Count > 0);
  DropBox_Options_RefreshRate.Enabled := (fMain.Settings.FullScreen) and (fMain.Resolutions.Count > 0);

  OldResolutionID := R;
  Button_Options_ResApply.Disable;
end;


procedure TKMMainMenuInterface.KeyDown(Key:Word; Shift: TShiftState);
begin
  if fMyControls.KeyDown(Key, Shift) then Exit; //Handled by Controls
end;


procedure TKMMainMenuInterface.KeyUp(Key:Word; Shift: TShiftState);
begin
  if fMyControls.KeyUp(Key, Shift) then Exit; //Handled by Controls
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
  inherited;

  fMyControls.MouseUp(X,Y,Shift,Button);
  Exit; //We could have caused fGameApp reinit, so exit at once
end;


//Should update anything we want to be updated, obviously
procedure TKMMainMenuInterface.UpdateState(aTickCount: Cardinal);
begin
  //
end;


end.
