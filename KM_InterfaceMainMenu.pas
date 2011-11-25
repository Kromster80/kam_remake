unit KM_InterfaceMainMenu;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  StrUtils, SysUtils, KromUtils, KromOGLUtils, Math, Classes, Controls,
  KM_Controls, KM_Defaults, KM_Settings, KM_MapInfo, KM_Campaigns, KM_Saves;


type
  TMenuScreen = (msError, msLoading, msMain, msOptions, msResults, msResultsMP);


  TKMMainMenuInterface = class
  private
    MyControls: TKMMasterControl;
    ScreenX,ScreenY:word;

    Campaign_Selected:TKMCampaign;
    Campaign_MapIndex:byte;

    fMaps_Top:integer; //Top map in list
    fMap_Selected:integer; //Selected map
    fMaps: TKMapsCollection;
    fMapsMP: TKMapsCollection;
    fSaves: TKMSavesCollection;
    fSavesMP: TKMSavesCollection;
    MapEdSizeX,MapEdSizeY:integer; //Map Editor map size
    OldFullScreen:boolean;
    OldResolution:word;

    procedure Create_MainMenu_Page;
    procedure Create_SinglePlayer_Page;
    procedure Create_Campaign_Page;
    procedure Create_Single_Page;
    procedure Create_Load_Page;
    procedure Create_MultiPlayer_Page;
    procedure Create_Lobby_Page;
    procedure Create_MapEditor_Page;
    procedure Create_Replays_Page;
    procedure Create_Options_Page(aGameSettings:TGlobalSettings);
    procedure Create_Credits_Page;
    procedure Create_Loading_Page;
    procedure Create_Error_Page;
    procedure Create_Results_Page;
    procedure Create_ResultsMP_Page;
    procedure SwitchMenuPage(Sender: TObject);
    procedure MainMenu_PlayTutorial(Sender: TObject);
    procedure MainMenu_PlayBattle(Sender: TObject);
    procedure MainMenu_StartReplay(Sender: TObject);
    procedure MainMenu_ReplayLastMap(Sender: TObject);
    procedure Campaign_Set(aCampaign:TKMCampaign);
    procedure Campaign_SelectMap(Sender: TObject);
    procedure Campaign_StartMap(Sender: TObject);

    procedure SingleMap_RefreshList;
    procedure SingleMap_ScrollChange(Sender: TObject);
    procedure SingleMap_SelectMap(Sender: TObject);
    procedure SingleMap_Start(Sender: TObject);

    procedure MP_Init(Sender: TObject);
    procedure MP_BindEvents;
    procedure MP_Save_Settings;
    procedure MP_Update(const aStatus:string; aColor:TColor4; aBusy:boolean);
    procedure MP_RefreshClick(Sender: TObject);
    procedure MP_ListUpdated(Sender: TObject);
    procedure MP_AnnouncementsUpdated(const S: string);
    procedure MP_SelectServer(Sender: TObject);
    procedure MP_Host_Click(Sender: TObject);
    procedure MP_JoinClick(Sender: TObject);
    procedure MP_JoinSuccess(Sender: TObject);
    procedure MP_JoinFail(const aData:string);
    procedure MP_JoinAssignedHost(Sender: TObject);
    procedure MP_HostFail(const aData:string);
    procedure MP_Back_Click(Sender: TObject);

    procedure Lobby_Reset(Sender: TObject; aPreserveMessage:boolean=false);
    procedure Lobby_GameOptionsChange(Sender: TObject);
    procedure Lobby_OnGameOptions(Sender: TObject);
    procedure Lobby_PlayersSetupChange(Sender: TObject);
    procedure Lobby_OnPlayersSetup(Sender: TObject);
    procedure Lobby_OnPingInfo(Sender: TObject);
    procedure Lobby_MapTypeSelect(Sender: TObject);
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
    procedure Load_RefreshList;
    procedure Load_DeleteConfirmation(aVisible:boolean);
    procedure Replays_ListClick(Sender: TObject);
    procedure Replay_TypeChange(Sender: TObject);
    procedure Replays_RefreshList;
    procedure Replays_Play(Sender: TObject);
    procedure MapEditor_Start(Sender: TObject);
    procedure MapEditor_SizeChange(Sender: TObject);
    procedure MapEditor_MapTypeChange(Sender: TObject);
    procedure MapEditor_ListUpdate(aUseMP:boolean);
    procedure Options_Fill(aGlobalSettings:TGlobalSettings);
    procedure Options_Change(Sender: TObject);
    procedure Options_FlagClick(Sender: TObject);
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
      Button_MM_Quit:TKMButton;
    Panel_SinglePlayer:TKMPanel;
      Panel_SPButtons:TKMPanel;
      Button_SP_Tutor,
      Button_SP_Fight,
      Button_SP_TSK,
      Button_SP_TPR,
      Button_SP_Single,
      Button_SP_Load,
      Button_SP_Replay:TKMButton;
      Button_SP_Back:TKMButton;
    Panel_MultiPlayer:TKMPanel;
      Panel_MPPlayerName:TKMPanel;
        Edit_MP_PlayerName: TKMEdit;
        Label_MP_Status:TKMLabel;
      Panel_MPAnnouncement:TKMPanel;
        Memo_MP_Announcement:TKMMemo;
      Panel_MPCreateServer:TKMPanel;
        Edit_MP_ServerName,
        Edit_MP_ServerPort: TKMEdit;
        Button_MP_CreateLAN,
        Button_MP_CreateWAN: TKMButton;
      Panel_MPJoinServer:TKMPanel;
        Button_MP_Join: TKMButton;
        Edit_MP_IP,
        Edit_MP_Port,
        Edit_MP_Room: TKMEdit;
      Panel_MPServerDetails:TKMPanel;
        Label_MP_Players:TKMLabel;
        Label_MP_Map:TKMLabel;
        Label_MP_GameTime:TKMLabel;
      Button_MP_Refresh,
      Button_MP_Back:TKMButton;
      ColList_Servers: TKMColumnListBox;

    Panel_Lobby:TKMPanel;
      Panel_LobbyPlayers:TKMPanel;
        Button_LobbyKick:array [0..MAX_PLAYERS-1] of TKMButton;
        Image_LobbyFlag:array [0..MAX_PLAYERS-1] of TKMImage;
        DropBox_LobbyPlayerSlot:array [0..MAX_PLAYERS-1] of TKMDropBox;
        Label_LobbyPlayer:array [0..MAX_PLAYERS-1] of TKMLabel;
        DropBox_LobbyLoc:array [0..MAX_PLAYERS-1] of TKMDropBox;
        DropBox_LobbyTeam:array [0..MAX_PLAYERS-1] of TKMDropBox;
        DropColorBox_Lobby:array [0..MAX_PLAYERS-1] of TKMDropColorBox;
        CheckBox_LobbyReady:array [0..MAX_PLAYERS-1] of TKMCheckBox;
        Label_LobbyPing:array [0..MAX_PLAYERS-1] of TKMLabel;

      Panel_LobbySetup:TKMPanel;
        Label_LobbyChooseMap: TKMLabel;
        Radio_LobbyMapType:TKMRadioGroup;
        List_Lobby:TKMDropBox;
        Label_LobbyMapName:TKMLabel;
        Memo_LobbyMapDesc: TKMMemo;
        Label_LobbyMapCount:TKMLabel;
        Label_LobbyMapMode:TKMLabel;
        Label_LobbyMapCond:TKMLabel;
        Label_LobbyMapSize:TKMLabel;

      Button_LobbyBack:TKMButton;
      Button_LobbyStart:TKMButton;
      Memo_LobbyPosts:TKMMemo;
      Edit_LobbyPost:TKMEdit;


    Panel_Campaign:TKMPanel;
      Image_CampaignBG:TKMImage;
      Image_CampaignNodes:array[0..MAX_CMP_MAPS-1] of TKMImage;
      Image_CampaignSubNode:array[0..MAX_CMP_SUBNODES-1] of TKMImage;
      Panel_CampScroll:TKMPanel;
        Image_ScrollTop,Image_Scroll:TKMImage;
        Label_CampaignTitle,Label_CampaignText:TKMLabel;
      Button_CampaignStart,Button_CampaignBack:TKMButton;
    Panel_Single:TKMPanel;
      Panel_SingleList,Panel_SingleDesc:TKMPanel;
      Button_SingleHeadMode,Button_SingleHeadTeams,Button_SingleHeadTitle,Button_SingleHeadSize:TKMButton;
      Bevel_SingleBG:array[0..MENU_SP_MAPS_COUNT-1,1..4]of TKMBevel;
      Image_SingleMode:array[0..MENU_SP_MAPS_COUNT-1]of TKMImage;
      Label_SinglePlayers,Label_SingleSize,
      Label_SingleTitle1,Label_SingleTitle2:array[0..MENU_SP_MAPS_COUNT-1]of TKMLabel;
      Shape_SingleOverlay:array[0..MENU_SP_MAPS_COUNT-1]of TKMShape;
      ScrollBar_SingleMaps:TKMScrollBar;
      Shape_SingleMap:TKMShape;
      Label_SingleTitle,Label_SingleDesc:TKMLabel;
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
    Panel_Replays:TKMPanel;
      Radio_Replays_Type:TKMRadioGroup;
      List_Replays: TKMColumnListBox;
      Button_ReplaysPlay: TKMButton;
      Button_ReplaysBack:TKMButton;
    Panel_MapEd:TKMPanel;
      Panel_MapEd_SizeXY:TKMPanel;
      Radio_MapEd_SizeX,Radio_MapEd_SizeY:TKMRadioGroup;
      Panel_MapEd_Load:TKMPanel;
      List_MapEd:TKMListBox;
      Radio_MapEd_MapType:TKMRadioGroup;
      Button_MapEdBack,Button_MapEd_Create,Button_MapEd_Load:TKMButton;
    Panel_Options:TKMPanel;
      Panel_Options_GFX:TKMPanel;
        Ratio_Options_Brightness:TKMRatioRow;
      Panel_Options_Ctrl:TKMPanel;
        Label_Options_MouseSpeed:TKMLabel;
        Ratio_Options_Mouse:TKMRatioRow;
        Label_Options_ScrollSpeed:TKMLabel;
        Ratio_Options_ScrollSpeed:TKMRatioRow;
      Panel_Options_Game:TKMPanel;
        CheckBox_Options_Autosave:TKMCheckBox;
      Panel_Options_Sound:TKMPanel;
        Label_Options_SFX,Label_Options_Music,Label_Options_MusicOn:TKMLabel;
        Ratio_Options_SFX,Ratio_Options_Music:TKMRatioRow;
        CheckBox_Options_MusicOn:TKMCheckBox;
        CheckBox_Options_ShuffleOn:TKMCheckBox;
      Panel_Options_Lang:TKMPanel;
        Radio_Options_Lang:TKMRadioGroup;
        Image_Options_Lang_Flags:array[1..LOCALES_COUNT] of TKMImage;
      Panel_Options_Res:TKMPanel;
        CheckBox_Options_FullScreen:TKMCheckBox;
        CheckBox_Options_Resolution:array[1..RESOLUTION_COUNT] of TKMCheckBox;
        Button_Options_ResApply:TKMButton;
      Button_Options_Back:TKMButton;
    Panel_Credits:TKMPanel;
      Label_Credits:TKMLabelScroll;
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
    constructor Create(X,Y:word; aGameSettings:TGlobalSettings; aReturnToOptions: Boolean);
    destructor Destroy; override;
    procedure Resize(X,Y:word);
    procedure ShowScreen(aScreen:TMenuScreen; const aText:string=''; aMsg:TGameResultMsg=gr_Silent);
    procedure Fill_Results;
    procedure Fill_ResultsMP;
    function GetChatText:string;
    function GetChatMessages:string;

    procedure KeyDown(Key:Word; Shift: TShiftState);
    procedure KeyPress(Key: Char);
    procedure KeyUp(Key:Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);
    procedure UpdateState;
    procedure Paint;
end;


implementation
uses KM_Unit1, KM_Render, KM_TextLibrary, KM_Game, KM_PlayersCollection, Forms, KM_Utils, KM_Player, KM_Log, KM_Sound, KM_Networking, KM_CommonTypes,
  KM_ServerQuery;


constructor TKMMainMenuInterface.Create(X,Y:word; aGameSettings:TGlobalSettings; aReturnToOptions: Boolean);
begin
  Inherited Create;

  Assert(fTextLibrary<>nil, 'fTextLibrary should be initialized before MainMenuInterface');

  ScreenX := min(X, MENU_DESIGN_X);
  ScreenY := min(Y, MENU_DESIGN_Y);
  Campaign_MapIndex := 1;

  fMaps := TKMapsCollection.Create(false);
  fMapsMP := TKMapsCollection.Create(true);
  fMaps_Top := 0;
  fMap_Selected := 0;
  fSaves := TKMSavesCollection.Create;
  fSavesMP := TKMSavesCollection.Create;

  MyControls := TKMMasterControl.Create;
  Panel_Main := TKMPanel.Create(MyControls, (X - MENU_DESIGN_X) div 2,
                                            (Y - MENU_DESIGN_Y) div 2,
                                            ScreenX, ScreenY); //Parent Panel for whole menu

  //Background is the same for all pages, except Results/Campaign, which will render ontop
  TKMImage.Create(Panel_Main,-448,-216,960,600,1,7);
  TKMImage.Create(Panel_Main, 512,-216,960,600,2,7);
  TKMImage.Create(Panel_Main,-448, 384,960,600,3,7);
  TKMImage.Create(Panel_Main, 512, 384,960,600,4,7);

  Create_MainMenu_Page;
  Create_SinglePlayer_Page;
    Create_Campaign_Page;
    Create_Single_Page;
    Create_Load_Page;
  Create_MultiPlayer_Page;
    Create_Lobby_Page;
  Create_MapEditor_Page;
  Create_Replays_Page;
  Create_Options_Page(aGameSettings);
  Create_Credits_Page;
  Create_Loading_Page;
  Create_Error_Page;
  Create_Results_Page;
  Create_ResultsMP_Page;

    {for i:=1 to length(FontFiles) do L[i]:=TKMLabel.Create(Panel_Main1,550,280+i*20,160,30,'This is a test string for KaM Remake ('+FontFiles[i],TKMFont(i),taLeft);//}
    //MyControls.AddTextEdit(Panel_Main, 32, 32, 200, 20, fnt_Grey);

  //Show version info on every page
  Label_Version := TKMLabel.Create(Panel_Main,8,8,0,0,GAME_VERSION+' / OpenGL '+fRender.RendererVersion,fnt_Antiqua,taLeft);

  if OVERLAY_RESOLUTIONS then
  begin
    with TKMShape.Create(Panel_Main, 112, 84, 800, 600, $FF00FFFF) do Hitable := False;
    with TKMShape.Create(Panel_Main, 0, 0, 1024, 768, $FF00FF00) do Hitable := False;
  end;

  if aReturnToOptions then
  begin
    Options_Fill(aGameSettings); //Use these settings link since fGame is not initialized yet
    SwitchMenuPage(Button_MM_Options); //Don't call Render afterwards for same reason
  end
  else
    SwitchMenuPage(nil);

  //SwitchMenuPage(Panel_ResultsMP); //Put here page you would like to debug
  fLog.AppendLog('Main menu init done');
end;


destructor TKMMainMenuInterface.Destroy;
begin
  fMaps.Free;
  fMapsMP.Free;
  fSaves.Free;
  fSavesMP.Free;
  MyControls.Free;
  Inherited;
end;


//Keep Panel_Main centered
procedure TKMMainMenuInterface.Resize(X, Y:word);
begin
  ScreenX := min(X, MENU_DESIGN_X);
  ScreenY := min(Y, MENU_DESIGN_Y);
  Panel_Main.Left := (X - MENU_DESIGN_X) div 2;
  Panel_Main.Top  := (Y - MENU_DESIGN_Y) div 2;
end;


procedure TKMMainMenuInterface.ShowScreen(aScreen:TMenuScreen; const aText:string=''; aMsg:TGameResultMsg=gr_Silent);
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
                    gr_Win:       Label_Results.Caption := fTextLibrary.GetSetupString(111);
                    gr_Defeat:    Label_Results.Caption := fTextLibrary.GetSetupString(112);
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
                    gr_Win:       Label_ResultsMP.Caption := fTextLibrary.GetSetupString(111);
                    gr_Defeat:    Label_ResultsMP.Caption := fTextLibrary.GetSetupString(112);
                    gr_Cancel:    Label_ResultsMP.Caption := fTextLibrary[TX_MENU_MISSION_CANCELED];
                    else          Label_ResultsMP.Caption := '<<<LEER>>>'; //Thats string used in all Synetic games for missing texts =)
                  end;
                  SwitchMenuPage(Panel_ResultsMP);
                end;
  end;

  fRender.Render;
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
  Panel_MainMenu := TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);
    TKMImage.Create(Panel_MainMenu, 300,  60, 423, 164, 4, 5);
    TKMLabel.Create(Panel_MainMenu, 512, 240,   0,   0, 'Remake', fnt_Metal, taCenter);
    with TKMImage.Create(Panel_MainMenu, 50,220,round(218*1.3),round(291*1.3),5,6) do ImageStretch;
    with TKMImage.Create(Panel_MainMenu,705,220,round(207*1.3),round(295*1.3),6,6) do ImageStretch;

    Panel_MMButtons := TKMPanel.Create(Panel_MainMenu,337,290,350,400);

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
      Button_MM_Quit.OnClick         := Form1.Exit1.OnClick;
end;


procedure TKMMainMenuInterface.Create_SinglePlayer_Page;
begin
  Panel_SinglePlayer:=TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);
    TKMImage.Create(Panel_SinglePlayer,300,60,423,164,4,5);
    TKMLabel.Create(Panel_SinglePlayer, 512, 240, 0, 0, 'Remake', fnt_Metal, taCenter);
    with TKMImage.Create(Panel_SinglePlayer,50,220,round(218*1.3),round(291*1.3),5,6) do ImageStretch;
    with TKMImage.Create(Panel_SinglePlayer,705,220,round(207*1.3),round(295*1.3),6,6) do ImageStretch;

    Panel_SPButtons:=TKMPanel.Create(Panel_SinglePlayer,337,290,350,400);
      Button_SP_Tutor  :=TKMButton.Create(Panel_SPButtons,0,  0,350,30,fTextLibrary[TX_MENU_TUTORIAL_TOWN],fnt_Metal,bsMenu);
      Button_SP_Fight  :=TKMButton.Create(Panel_SPButtons,0, 40,350,30,fTextLibrary[TX_MENU_TUTORIAL_BATTLE],fnt_Metal,bsMenu);
      Button_SP_TSK    :=TKMButton.Create(Panel_SPButtons,0, 80,350,30,fTextLibrary.GetSetupString( 1),fnt_Metal,bsMenu);
      Button_SP_TPR    :=TKMButton.Create(Panel_SPButtons,0,120,350,30,fTextLibrary.GetSetupString( 2),fnt_Metal,bsMenu);
      Button_SP_Single :=TKMButton.Create(Panel_SPButtons,0,160,350,30,fTextLibrary.GetSetupString( 4),fnt_Metal,bsMenu);
      Button_SP_Load   :=TKMButton.Create(Panel_SPButtons,0,200,350,30,fTextLibrary.GetSetupString(10),fnt_Metal,bsMenu);
      Button_SP_Replay :=TKMButton.Create(Panel_SPButtons,0,240,350,30,fTextLibrary[TX_MENU_VIEW_LAST_REPLAY],fnt_Metal,bsMenu);
      Button_SP_Back   :=TKMButton.Create(Panel_SPButtons,0,320,350,30,fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);

      Button_SP_Tutor.OnClick    := MainMenu_PlayTutorial;
      Button_SP_Fight.OnClick    := MainMenu_PlayBattle;
      Button_SP_TSK.OnClick      := SwitchMenuPage;
      Button_SP_TPR.OnClick      := SwitchMenuPage;
      Button_SP_Single.OnClick   := SwitchMenuPage;
      Button_SP_Load.OnClick     := SwitchMenuPage;
      Button_SP_Replay.OnClick   := MainMenu_StartReplay; //Reroute since fGame is not initialized yet
      Button_SP_Back.OnClick     := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_MultiPlayer_Page;
begin
  Panel_MultiPlayer := TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);

    //Top area
    Panel_MPPlayerName := TKMPanel.Create(Panel_MultiPlayer, 45, 42, 620, 72);
      TKMBevel.Create(Panel_MPPlayerName, 0, 0, 620, 72);
      TKMLabel.Create(Panel_MPPlayerName, 8, 6, 136, 20, fTextLibrary[TX_MP_MENU_PLAYERNAME], fnt_Outline, taLeft);
      Edit_MP_PlayerName := TKMEdit.Create(Panel_MPPlayerName, 8, 26, 120, 20, fnt_Grey);
      TKMLabel.Create(Panel_MPPlayerName, 150, 6, 460, 10, fTextLibrary[TX_MP_MENU_STATUS], fnt_Outline, taLeft);
      Label_MP_Status := TKMLabel.Create(Panel_MPPlayerName, 150, 26, 470, 46, '', fnt_Grey, taLeft);
      Label_MP_Status.AutoWrap := true;

    Panel_MPAnnouncement := TKMPanel.Create(Panel_MultiPlayer, 45, 120, 620, 158);
      Memo_MP_Announcement := TKMMemo.Create(Panel_MPAnnouncement,0,0,620,158,fnt_Grey);
      Memo_MP_Announcement.ItemHeight := 16;
      Memo_MP_Announcement.AutoWrap := true;

    //Create server area
    Panel_MPCreateServer := TKMPanel.Create(Panel_MultiPlayer, 673, 42, 300, 236);
      TKMBevel.Create(Panel_MPCreateServer,   0,  0, 300, 236);
      TKMLabel.Create(Panel_MPCreateServer, 150, 6, 284, 20, fTextLibrary[TX_MP_MENU_HEADER_CREATE_SERVER], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_MPCreateServer, 8, 42, 284, 20, fTextLibrary[TX_MP_MENU_CREATE_SERVER_NAME], fnt_Outline, taLeft);
      Edit_MP_ServerName := TKMEdit.Create(Panel_MPCreateServer, 8, 58, 286, 20, fnt_Grey);
      TKMLabel.Create(Panel_MPCreateServer, 8, 88, 284, 20, fTextLibrary[TX_MP_MENU_SERVER_PORT], fnt_Outline, taLeft);
      Edit_MP_ServerPort := TKMEdit.Create(Panel_MPCreateServer, 8, 104, 100, 20, fnt_Grey);
      Button_MP_CreateLAN  := TKMButton.Create(Panel_MPCreateServer,8, 155,286,30,fTextLibrary[TX_MP_MENU_CREATE_LOCAL],fnt_Metal,bsMenu);
      Button_MP_CreateWAN  := TKMButton.Create(Panel_MPCreateServer,8, 195,286,30,fTextLibrary[TX_MP_MENU_CREATE_INTERNET],fnt_Metal,bsMenu);
      Button_MP_CreateLAN.OnClick := MP_Host_Click;
      Button_MP_CreateWAN.OnClick := MP_Host_Click;

    //Server list area
    ColList_Servers := TKMColumnListBox.Create(Panel_MultiPlayer,45,300,620,392,fnt_Metal,fnt_Outline,[fTextLibrary[TX_MP_MENU_SERVER_NAME],fTextLibrary[TX_MP_MENU_SERVER_STATE],fTextLibrary[TX_MP_MENU_SERVER_PLAYERS],fTextLibrary[TX_MP_MENU_SERVER_PING]],[0,300,430,525]);
    ColList_Servers.OnChange := MP_SelectServer;
    Button_MP_Refresh := TKMButton.Create(Panel_MultiPlayer,275,700,390,30,fTextLibrary[TX_MP_MENU_REFRESH_LIST],fnt_Metal,bsMenu);
    Button_MP_Refresh.OnClick := MP_RefreshClick;

    //Server details area
    Panel_MPServerDetails := TKMPanel.Create(Panel_MultiPlayer, 673, 300, 300, 292);
      TKMBevel.Create(Panel_MPServerDetails, 0, 0, 300, 300);
      TKMLabel.Create(Panel_MPServerDetails, 150, 6, 284, 20, fTextLibrary[TX_MP_MENU_HEADER_SERVER_DETAILS], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_MPServerDetails, 8, 30, 284, 20, fTextLibrary[TX_MP_MENU_GAME_INFORMATION], fnt_Outline, taLeft);
      Label_MP_Map := TKMLabel.Create(Panel_MPServerDetails, 8, 50, 284, 242, '', fnt_Metal, taLeft);
      Label_MP_GameTime := TKMLabel.Create(Panel_MPServerDetails, 8, 70, 284, 242, '', fnt_Metal, taLeft);
      TKMLabel.Create(Panel_MPServerDetails, 8, 130, 284, 20, fTextLibrary[TX_MP_MENU_PLAYER_LIST], fnt_Outline, taLeft);
      Label_MP_Players := TKMLabel.Create(Panel_MPServerDetails, 8, 150, 284, 242, '', fnt_Metal, taLeft);

    //Join server area
    Panel_MPJoinServer := TKMPanel.Create(Panel_MultiPlayer, 673, 602, 300, 90);
      TKMBevel.Create(Panel_MPJoinServer, 0, 0, 300, 90);
      TKMLabel.Create(Panel_MPJoinServer, 8, 8, 156, 20, fTextLibrary[TX_MP_MENU_JOIN_ADDRESS], fnt_Outline, taLeft);
      Edit_MP_IP := TKMEdit.Create(Panel_MPJoinServer, 8, 24, 162, 20, fnt_Grey);
      TKMLabel.Create(Panel_MPJoinServer, 172, 8, 60, 20, fTextLibrary[TX_MP_MENU_JOIN_PORT], fnt_Outline, taLeft);
      Edit_MP_Port := TKMEdit.Create(Panel_MPJoinServer, 172, 24, 60, 20, fnt_Grey);
      TKMLabel.Create(Panel_MPJoinServer, 232, 8, 60, 20, fTextLibrary[TX_MP_MENU_JOIN_ROOM], fnt_Outline, taLeft);
      Edit_MP_Room := TKMEdit.Create(Panel_MPJoinServer, 232, 24, 60, 20, fnt_Grey);
      Button_MP_Join := TKMButton.Create(Panel_MPJoinServer,8, 52,284,30,fTextLibrary[TX_MP_MENU_SERVER_JOIN],fnt_Metal,bsMenu);
      Button_MP_Join.OnClick := MP_JoinClick;

    Button_MP_Back := TKMButton.Create(Panel_MultiPlayer, 45, 700, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_MP_Back.OnClick := MP_Back_Click;
end;


procedure TKMMainMenuInterface.Create_Lobby_Page;
var i,k,top:integer;
begin
  Panel_Lobby := TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);

    //Players
    Panel_LobbyPlayers := TKMPanel.Create(Panel_Lobby,20,42,985,240);
      TKMBevel.Create(Panel_LobbyPlayers,   0,  0, 985, 240);
      TKMLabel.Create(Panel_LobbyPlayers, 60, 10, 210, 20, fTextLibrary[TX_LOBBY_HEADER_PLAYERS], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, 280, 10, 200, 20, fTextLibrary[TX_LOBBY_HEADER_STARTLOCATION], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, 490, 10, 150, 20, fTextLibrary[TX_LOBBY_HEADER_TEAM], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, 650, 10, 150, 20, fTextLibrary[TX_LOBBY_HEADER_FLAGCOLOR], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, 836, 10,  80, 20, fTextLibrary[TX_LOBBY_HEADER_READY], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_LobbyPlayers, 903, 10, 50, 20, fTextLibrary[TX_LOBBY_HEADER_PING], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_LobbyPlayers, 955, 10, 50, 20, fTextLibrary[TX_LOBBY_HEADER_KICK], fnt_Outline, taCenter);

      for i:=0 to MAX_PLAYERS-1 do begin
        top := 30+i*25;
        Image_LobbyFlag[i] := TKMImage.Create(Panel_LobbyPlayers, 20, top+3, 16, 11, 0, 7);

        Label_LobbyPlayer[i] := TKMLabel.Create(Panel_LobbyPlayers, 60, top+2, 140, 20, '', fnt_Metal, taLeft);
        Label_LobbyPlayer[i].Hide;

        DropBox_LobbyPlayerSlot[i] := TKMDropBox.Create(Panel_LobbyPlayers, 60, top, 210, 20, fnt_Metal, '');
        DropBox_LobbyPlayerSlot[i].Add(fTextLibrary[TX_LOBBY_SLOT_OPEN]); //Player can join into this slot
        DropBox_LobbyPlayerSlot[i].Add(fTextLibrary[TX_LOBBY_SLOT_AI_PLAYER]); //This slot is an AI player
        DropBox_LobbyPlayerSlot[i].ItemIndex := 0; //Open
        DropBox_LobbyPlayerSlot[i].OnChange := Lobby_PlayersSetupChange;

        DropBox_LobbyLoc[i] := TKMDropBox.Create(Panel_LobbyPlayers, 280, top, 200, 20, fnt_Metal, '');
        DropBox_LobbyLoc[i].Add(fTextLibrary[TX_LOBBY_RANDOM]);
        DropBox_LobbyLoc[i].OnChange := Lobby_PlayersSetupChange;

        DropBox_LobbyTeam[i] := TKMDropBox.Create(Panel_LobbyPlayers, 490, top, 150, 20, fnt_Metal, '');
        DropBox_LobbyTeam[i].Add(fTextLibrary[TX_LOBBY_NONE]);
        for k:=1 to 4 do DropBox_LobbyTeam[i].Add(Format(fTextLibrary[TX_LOBBY_TEAM_X],[k]));
        DropBox_LobbyTeam[i].OnChange := Lobby_PlayersSetupChange;

        DropColorBox_Lobby[i] := TKMDropColorBox.Create(Panel_LobbyPlayers, 650, top, 150, 20, MP_COLOR_COUNT);
        DropColorBox_Lobby[i].SetColors(MP_TEAM_COLORS, fTextLibrary[TX_LOBBY_RANDOM]);
        DropColorBox_Lobby[i].OnChange := Lobby_PlayersSetupChange;

        CheckBox_LobbyReady[i] := TKMCheckBox.Create(Panel_LobbyPlayers, 829, top, 20, 20, '', fnt_Metal);

        Label_LobbyPing[i] := TKMLabel.Create(Panel_LobbyPlayers, 903, top, 50, 20, '', fnt_Metal, taCenter);

        Button_LobbyKick[i] := TKMButton.Create(Panel_LobbyPlayers, 948, top, 16, 16, 'X', fnt_Metal);
        Button_LobbyKick[i].OnClick := Lobby_PlayersSetupChange;
      end;

    //Chat
    TKMLabel.Create(Panel_Lobby, 20, 292, 475, 20, fTextLibrary[TX_LOBBY_POST_LIST], fnt_Outline, taLeft);
    Memo_LobbyPosts := TKMMemo.Create(Panel_Lobby, 20, 312, 475, 300, fnt_Metal);
    Memo_LobbyPosts.AutoWrap := True;
    Memo_LobbyPosts.ScrollDown := True;
    TKMLabel.Create(Panel_Lobby, 20, 622, 625, 20, fTextLibrary[TX_LOBBY_POST_WRITE], fnt_Outline, taLeft);
    Edit_LobbyPost := TKMEdit.Create(Panel_Lobby, 20, 642, 475, 20, fnt_Metal);
    Edit_LobbyPost.OnKeyDown := Lobby_PostKey;

    //Setup
    Panel_LobbySetup := TKMPanel.Create(Panel_Lobby,510,292,495,430);
      TKMBevel.Create(Panel_LobbySetup,  0,  0, 495, 430);
      Label_LobbyChooseMap := TKMLabel.Create(Panel_LobbySetup, 10, 10, 220, 20, fTextLibrary[TX_LOBBY_MAP_CHOOSE], fnt_Outline, taLeft);
      Radio_LobbyMapType := TKMRadioGroup.Create(Panel_LobbySetup, 10, 35, 220, 60, fnt_Metal);
      Radio_LobbyMapType.Items.Add(fTextLibrary[TX_LOBBY_MAP_BUILD]);
      Radio_LobbyMapType.Items.Add(fTextLibrary[TX_LOBBY_MAP_FIGHT]);
      Radio_LobbyMapType.Items.Add(fTextLibrary[TX_LOBBY_MAP_COOP]);
      Radio_LobbyMapType.Items.Add(fTextLibrary[TX_LOBBY_MAP_SAVED]);
      Radio_LobbyMapType.ItemIndex := 0;
      Radio_LobbyMapType.OnChange := Lobby_MapTypeSelect;
      List_Lobby := TKMDropBox.Create(Panel_LobbySetup, 10, 105, 220, 20, fnt_Metal, fTextLibrary[TX_LOBBY_MAP_SELECT]);
      List_Lobby.OnChange := Lobby_MapSelect;
      Label_LobbyMapName := TKMLabel.Create(Panel_LobbySetup, 10, 130, 220, 20, '', fnt_Metal, taLeft);

      Memo_LobbyMapDesc := TKMMemo.Create(Panel_LobbySetup, 10, 155, 220, 180, fnt_Game);
      Memo_LobbyMapDesc.AutoWrap := True;
      Memo_LobbyMapDesc.ItemHeight := 16;

      //TKMLabel.Create(Panel_LobbySetup, 10, 340, 220, 20, fTextLibrary[TX_LOBBY_MAP_INFO], fnt_Outline, taLeft);
      Label_LobbyMapCount := TKMLabel.Create(Panel_LobbySetup, 10, 345, 220, 20, '', fnt_Metal, taLeft);
      Label_LobbyMapMode := TKMLabel.Create(Panel_LobbySetup, 10, 365, 220, 20, '', fnt_Metal, taLeft);
      Label_LobbyMapCond := TKMLabel.Create(Panel_LobbySetup, 10, 385, 220, 20, '', fnt_Metal, taLeft);
      Label_LobbyMapSize := TKMLabel.Create(Panel_LobbySetup, 10, 405, 220, 20, '', fnt_Metal, taLeft);

      Label_LobbyChooseMap := TKMLabel.Create(Panel_LobbySetup, 265, 10, 220, 20, fTextLibrary[TX_LOBBY_GAME_OPTIONS], fnt_Outline, taLeft);

    Button_LobbyBack := TKMButton.Create(Panel_Lobby, 35, 680, 190, 30, fTextLibrary[TX_LOBBY_QUIT], fnt_Metal, bsMenu);
    Button_LobbyBack.OnClick := Lobby_BackClick;
    Button_LobbyStart := TKMButton.Create(Panel_Lobby, 240, 680, 240, 30, '<<<LEER>>>', fnt_Metal, bsMenu);
    Button_LobbyStart.OnClick := Lobby_StartClick;
end;


procedure TKMMainMenuInterface.Create_Campaign_Page;
var i:integer;
begin
  Panel_Campaign:=TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);
    Image_CampaignBG := TKMImage.Create(Panel_Campaign,0,0,MENU_DESIGN_X,MENU_DESIGN_Y,12,5);
    Image_CampaignBG.ImageStretch;

    for i:=0 to High(Image_CampaignNodes) do begin
      Image_CampaignNodes[i] := TKMImage.Create(Panel_Campaign, MENU_DESIGN_X,MENU_DESIGN_Y, 23, 29, 10, 5);
      Image_CampaignNodes[i].OnClick := Campaign_SelectMap;
      Image_CampaignNodes[i].Tag := i;
    end;
    for i:=0 to High(Image_CampaignSubNode) do
    begin
      Image_CampaignSubNode[i] := TKMImage.Create(Panel_Campaign, MENU_DESIGN_X,MENU_DESIGN_Y, 0, 0, 16, 5);
      Image_CampaignSubNode[i].ImageCenter;
    end;
  Panel_CampScroll:=TKMPanel.Create(Panel_Campaign,MENU_DESIGN_X-360,MENU_DESIGN_Y-430,360,430);

  //@Krom: When this page is finalised the labels will need to have their width/heights checked with the overlay debug on
    Image_Scroll := TKMImage.Create(Panel_CampScroll, 0, 0,360,430,{15}2,6);
    Image_Scroll.ImageStretch;
    Label_CampaignTitle := TKMLabel.Create(Panel_CampScroll, 180, 18,100,20, '', fnt_Outline, taCenter);

    Label_CampaignText := TKMLabel.Create(Panel_CampScroll, 20, 50, 325, 310, '', fnt_Briefing, taLeft);
    Label_CampaignText.AutoWrap := true;

  Button_CampaignStart := TKMButton.Create(Panel_Campaign, MENU_DESIGN_X-220-20, MENU_DESIGN_Y-50, 220, 30, fTextLibrary[TX_MENU_START_MISSION], fnt_Metal, bsMenu);
  Button_CampaignStart.OnClick := Campaign_StartMap;

  Button_CampaignBack := TKMButton.Create(Panel_Campaign, 20, MENU_DESIGN_Y-50, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
  Button_CampaignBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Single_Page;
var i:integer;
begin
  Panel_Single:=TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);
    Panel_SingleList:=TKMPanel.Create(Panel_Single,512+22,84,445,600);

      ScrollBar_SingleMaps := TKMScrollBar.Create(Panel_SingleList,420,40,25,MENU_SP_MAPS_COUNT*40, sa_Vertical, bsMenu);
      ScrollBar_SingleMaps.OnChange := SingleMap_ScrollChange;

      Button_SingleHeadMode  := TKMButton.Create(Panel_SingleList,  0,0, 40,40,42,4,bsMenu);
      Button_SingleHeadTeams := TKMButton.Create(Panel_SingleList, 40,0, 40,40,31,4,bsMenu);
      Button_SingleHeadTitle := TKMButton.Create(Panel_SingleList, 80,0,300,40,fTextLibrary[TX_MENU_TITLE],fnt_Metal,bsMenu);
      Button_SingleHeadSize  := TKMButton.Create(Panel_SingleList,380,0, 40,40,fTextLibrary[TX_MENU_SIZE],fnt_Metal,bsMenu);
      with TKMButton.Create(Panel_SingleList,420,0, 25,40,'',fnt_Metal,bsMenu) do Disable;

      for i:=0 to MENU_SP_MAPS_COUNT-1 do
      begin
        Bevel_SingleBG[i,1] := TKMBevel.Create(Panel_SingleList,0,  40+i*40, 40,40);
        Bevel_SingleBG[i,2] := TKMBevel.Create(Panel_SingleList,40, 40+i*40, 40,40);
        Bevel_SingleBG[i,3] := TKMBevel.Create(Panel_SingleList,80, 40+i*40,300,40);
        Bevel_SingleBG[i,4] := TKMBevel.Create(Panel_SingleList,380,40+i*40, 40,40);

        Image_SingleMode[i]    := TKMImage.Create(Panel_SingleList,  0   ,40+i*40,40,40,28);
        Image_SingleMode[i].ImageCenter;
        Label_SinglePlayers[i] := TKMLabel.Create(Panel_SingleList, 40+20, 40+i*40+14, 40,20,'0',fnt_Metal, taCenter);
        Label_SingleTitle1[i]  := TKMLabel.Create(Panel_SingleList, 80+ 6, 40+i*40+ 5, 288,0,'<<<LEER>>>',fnt_Metal, taLeft);
        Label_SingleTitle2[i]  := TKMLabel.Create(Panel_SingleList, 80+ 6, 40+i*40+22, 288,0,'<<<LEER>>>',fnt_Game, taLeft, $FFD0D0D0);
        Label_SingleSize[i]    := TKMLabel.Create(Panel_SingleList,380+20, 40+i*40+14, 40,20,'0',fnt_Metal, taCenter);

        Shape_SingleOverlay[i] := TKMShape.Create(Panel_SingleList, 0, 40+i*40, 420, 40, $00000000);
        Shape_SingleOverlay[i].LineWidth := 0;
        Shape_SingleOverlay[i].Tag := i;
        Shape_SingleOverlay[i].OnClick := SingleMap_SelectMap;
        Shape_SingleOverlay[i].OnMouseWheel := ScrollBar_SingleMaps.MouseWheel;
      end;

      Shape_SingleMap:=TKMShape.Create(Panel_SingleList,0,40,420,40, $FFFFFF00);
      Shape_SingleMap.OnMouseWheel := ScrollBar_SingleMaps.MouseWheel;

    Panel_SingleDesc:=TKMPanel.Create(Panel_Single,45,84,445,600);

      TKMBevel.Create(Panel_SingleDesc,0,0,445,220);

      Label_SingleTitle := TKMLabel.Create(Panel_SingleDesc,445 div 2,35,433,20,'',fnt_Outline, taCenter);
      Label_SingleDesc  := TKMLabel.Create(Panel_SingleDesc,15,60,415,154,'',fnt_Metal, taLeft);
      Label_SingleDesc.AutoWrap:=true;

      TKMBevel.Create(Panel_SingleDesc,125,230,192,192);

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

    Button_SingleBack := TKMButton.Create(Panel_Single, 45, 650, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_SingleBack.OnClick := SwitchMenuPage;
    Button_SingleStart := TKMButton.Create(Panel_Single, 270, 650, 220, 30, fTextLibrary.GetSetupString(8), fnt_Metal, bsMenu);
    Button_SingleStart.OnClick := SingleMap_Start;
end;


procedure TKMMainMenuInterface.Create_Load_Page;
begin
  Panel_Load:=TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);

    TKMLabel.Create(Panel_Load, MENU_DESIGN_X div 2, 60, 900, 20, fTextLibrary[TX_MENU_LOAD_LIST], fnt_Metal, taCenter);

    List_Load := TKMColumnListBox.Create(Panel_Load, 62, 85, 900, 468, fnt_Metal, fnt_Outline, [fTextLibrary[TX_MENU_LOAD_FILE], fTextLibrary[TX_MENU_LOAD_DESCRIPTION]], [0, 300]);
    List_Load.OnChange := Load_ListClick;

    Button_Load := TKMButton.Create(Panel_Load,337,560,350,30,fTextLibrary[TX_MENU_LOAD_LOAD],fnt_Metal, bsMenu);
    Button_Load.OnClick := Load_Click;

    Button_Delete := TKMButton.Create(Panel_Load, 337, 594, 350, 30, fTextLibrary[TX_MENU_LOAD_DELETE], fnt_Metal, bsMenu);
    Button_Delete.OnClick := Load_Delete_Click;

    Label_DeleteConfirm := TKMLabel.Create(Panel_Load, MENU_DESIGN_X div 2, 604, 550, 30, fTextLibrary[TX_MENU_LOAD_DELETE_CONFIRM], fnt_Outline, taCenter);
    Button_DeleteYes := TKMButton.Create(Panel_Load, 337, 630, 170, 30, fTextLibrary[TX_MENU_LOAD_DELETE_DELETE], fnt_Metal, bsMenu);
    Button_DeleteYes.OnClick := Load_Delete_Click;
    Button_DeleteNo  := TKMButton.Create(Panel_Load, 517, 630, 170, 30, fTextLibrary[TX_MENU_LOAD_DELETE_CANCEL], fnt_Metal, bsMenu);
    Button_DeleteNo.OnClick := Load_Delete_Click;

    Button_LoadBack := TKMButton.Create(Panel_Load, 337, 670, 350, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_LoadBack.OnClick := SwitchMenuPage;
end;


//Should contain options to make a map from scratch, load map from file, generate new one
procedure TKMMainMenuInterface.Create_MapEditor_Page;
var i:integer;
begin
  Panel_MapEd:=TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);
    Panel_MapEd_SizeXY := TKMPanel.Create(Panel_MapEd, 462-210, 200, 200, 400);
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

    Panel_MapEd_Load := TKMPanel.Create(Panel_MapEd, 462+10, 200, 300, 400);
      TKMLabel.Create(Panel_MapEd_Load, 6, 0, 288, 20, fTextLibrary[TX_MENU_MAP_AVAILABLE], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_MapEd_Load, 0, 20, 300, 50);
      Radio_MapEd_MapType := TKMRadioGroup.Create(Panel_MapEd_Load,8,28,286,40,fnt_Grey);
      Radio_MapEd_MapType.ItemIndex := 0;
      Radio_MapEd_MapType.Items.Add(fTextLibrary[TX_MENU_MAPED_SPMAPS]);
      Radio_MapEd_MapType.Items.Add(fTextLibrary[TX_MENU_MAPED_MPMAPS]);
      Radio_MapEd_MapType.OnChange := MapEditor_MapTypeChange;
      List_MapEd := TKMListBox.Create(Panel_MapEd_Load, 0, 80, 300, 240, fnt_Metal);
      Button_MapEd_Load := TKMButton.Create(Panel_MapEd_Load, 0, 335, 300, 30, fTextLibrary[TX_MENU_MAP_LOAD_EXISTING], fnt_Metal, bsMenu);
      Button_MapEd_Load.OnClick := MapEditor_Start;

    Button_MapEdBack := TKMButton.Create(Panel_MapEd, 120, 650, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_MapEdBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Replays_Page;
begin
  Panel_Replays := TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);

    TKMLabel.Create(Panel_Replays, MENU_DESIGN_X div 2, 60, 900, 20, fTextLibrary[TX_MENU_LOAD_LIST], fnt_Outline, taCenter);

    TKMBevel.Create(Panel_Replays, 62, 100, 900, 50);
    Radio_Replays_Type := TKMRadioGroup.Create(Panel_Replays,70,108,300,40,fnt_Grey);
    Radio_Replays_Type.ItemIndex := 0;
    Radio_Replays_Type.Items.Add(fTextLibrary[TX_MENU_MAPED_SPMAPS]);
    Radio_Replays_Type.Items.Add(fTextLibrary[TX_MENU_MAPED_MPMAPS]);
    Radio_Replays_Type.OnChange := Replay_TypeChange;

    List_Replays := TKMColumnListBox.Create(Panel_Replays, 62, 200, 900, 350, fnt_Metal, fnt_Outline, [fTextLibrary[TX_MENU_LOAD_FILE], fTextLibrary[TX_MENU_LOAD_DESCRIPTION]], [0, 300]);
    List_Replays.OnChange := Replays_ListClick;

    Button_ReplaysPlay := TKMButton.Create(Panel_Replays,337,630,350,30,fTextLibrary[TX_MENU_VIEW_REPLAY],fnt_Metal, bsMenu);
    Button_ReplaysPlay.OnClick := Replays_Play;

    Button_ReplaysBack := TKMButton.Create(Panel_Replays, 337, 670, 350, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_ReplaysBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Options_Page(aGameSettings:TGlobalSettings);
var i:integer;
begin
  Panel_Options:=TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);
    with TKMImage.Create(Panel_Options,705,220,round(207*1.3),round(295*1.3),6,6) do ImageStretch;

    Panel_Options_Ctrl:=TKMPanel.Create(Panel_Options,120,130,200,130);
      TKMLabel.Create(Panel_Options_Ctrl,6,0,288,20,fTextLibrary[TX_MENU_OPTIONS_CONTROLS],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Ctrl,0,20,200,110);

      Label_Options_MouseSpeed:=TKMLabel.Create(Panel_Options_Ctrl,18,27,164,20,fTextLibrary.GetTextString(192),fnt_Metal,taLeft);
      Label_Options_MouseSpeed.Disable;
      Ratio_Options_Mouse:=TKMRatioRow.Create(Panel_Options_Ctrl,10,47,180,20,aGameSettings.SlidersMin,aGameSettings.SlidersMax);
      Ratio_Options_Mouse.Disable;

      Label_Options_ScrollSpeed:=TKMLabel.Create(Panel_Options_Ctrl,18,77,164,20,fTextLibrary[TX_MENU_OPTIONS_SCROLL_SPEED],fnt_Metal,taLeft);
      Ratio_Options_ScrollSpeed:=TKMRatioRow.Create(Panel_Options_Ctrl,10,97,180,20,aGameSettings.SlidersMin,aGameSettings.SlidersMax);
      Ratio_Options_ScrollSpeed.OnChange:=Options_Change;

    Panel_Options_Game:=TKMPanel.Create(Panel_Options,120,280,200,50);
      TKMLabel.Create(Panel_Options_Game,6,0,188,20,fTextLibrary[TX_MENU_OPTIONS_GAMEPLAY],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Game,0,20,200,30);

      CheckBox_Options_Autosave := TKMCheckBox.Create(Panel_Options_Game,12,27,176,20,fTextLibrary.GetTextString(203), fnt_Metal);
      CheckBox_Options_Autosave.OnClick := Options_Change;

    Panel_Options_Sound:=TKMPanel.Create(Panel_Options,120,350,200,167);
      TKMLabel.Create(Panel_Options_Sound,6,0,188,20,fTextLibrary[TX_MENU_OPTIONS_SOUND],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Sound,0,20,200,147);

      Label_Options_SFX:=TKMLabel.Create(Panel_Options_Sound,18,27,164,20,fTextLibrary.GetTextString(194),fnt_Metal,taLeft);
      Ratio_Options_SFX:=TKMRatioRow.Create(Panel_Options_Sound,10,47,180,20,aGameSettings.SlidersMin,aGameSettings.SlidersMax);
      Ratio_Options_SFX.OnChange:=Options_Change;
      Label_Options_Music:=TKMLabel.Create(Panel_Options_Sound,18,77,164,20,fTextLibrary.GetTextString(196),fnt_Metal,taLeft);
      Ratio_Options_Music:=TKMRatioRow.Create(Panel_Options_Sound,10,97,180,20,aGameSettings.SlidersMin,aGameSettings.SlidersMax);
      Ratio_Options_Music.OnChange:=Options_Change;
      CheckBox_Options_MusicOn := TKMCheckBox.Create(Panel_Options_Sound,12,127,176,20,fTextLibrary[TX_MENU_OPTIONS_MUSIC_DISABLE], fnt_Metal);
      CheckBox_Options_MusicOn.OnClick := Options_Change;
      CheckBox_Options_ShuffleOn := TKMCheckBox.Create(Panel_Options_Sound,12,147,176,20,fTextLibrary[TX_MENU_OPTIONS_MUSIC_SHUFFLE], fnt_Metal);
      CheckBox_Options_ShuffleOn.OnClick := Options_Change;

    Panel_Options_GFX:=TKMPanel.Create(Panel_Options,340,130,200,80);
      TKMLabel.Create(Panel_Options_GFX,6,0,188,20,fTextLibrary[TX_MENU_OPTIONS_GRAPHICS],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_GFX,0,20,200,60);
      TKMLabel.Create(Panel_Options_GFX,18,27,164,20,fTextLibrary[TX_MENU_OPTIONS_BRIGHTNESS],fnt_Metal,taLeft);
      Ratio_Options_Brightness:=TKMRatioRow.Create(Panel_Options_GFX,10,47,180,20,aGameSettings.SlidersMin,aGameSettings.SlidersMax);
      Ratio_Options_Brightness.OnChange:=Options_Change;

    Panel_Options_Res:=TKMPanel.Create(Panel_Options,340,230,200,30+RESOLUTION_COUNT*20);
      //Resolution selector
      TKMLabel.Create(Panel_Options_Res,6,0,188,20,fTextLibrary.GetSetupString(20),fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Res,0,20,200,10+RESOLUTION_COUNT*20);
      for i:=1 to RESOLUTION_COUNT do
      begin
        CheckBox_Options_Resolution[i]:=TKMCheckBox.Create(Panel_Options_Res,12,27+(i-1)*20,176,20,Format('%dx%d',[SupportedResolutions[i,1],SupportedResolutions[i,2],SupportedRefreshRates[i]]),fnt_Metal);
        CheckBox_Options_Resolution[i].Enabled:=(SupportedRefreshRates[i] > 0);
        CheckBox_Options_Resolution[i].OnClick:=Options_Change;
      end;

      CheckBox_Options_FullScreen:=TKMCheckBox.Create(Panel_Options_Res,12,38+RESOLUTION_COUNT*20,176,20,fTextLibrary[TX_MENU_OPTIONS_FULLSCREEN],fnt_Metal);
      CheckBox_Options_FullScreen.OnClick:=Options_Change;

      Button_Options_ResApply:=TKMButton.Create(Panel_Options_Res,10,58+RESOLUTION_COUNT*20,180,30,fTextLibrary[TX_MENU_OPTIONS_APPLY],fnt_Metal, bsMenu);
      Button_Options_ResApply.OnClick:=Options_Change;

    Panel_Options_Lang:=TKMPanel.Create(Panel_Options,560,130,220,30+LOCALES_COUNT*20);
      TKMLabel.Create(Panel_Options_Lang,6,0,242,20,fTextLibrary[TX_MENU_OPTIONS_LANGUAGE],fnt_Outline,taLeft);
      TKMBevel.Create(Panel_Options_Lang,0,20,246,10+LOCALES_COUNT*20);

      Radio_Options_Lang := TKMRadioGroup.Create(Panel_Options_Lang, 28, 27, 220, 20*LOCALES_COUNT, fnt_Metal);
      for i:=1 to LOCALES_COUNT do
      begin
        Radio_Options_Lang.Items.Add(Locales[i,4]);
        Image_Options_Lang_Flags[i] := TKMImage.Create(Panel_Options_Lang,6,28+((i-1)*20),16,11,StrToInt(Locales[i,3]),7);
        Image_Options_Lang_Flags[i].Tag := i-1;
        Image_Options_Lang_Flags[i].OnClick := Options_FlagClick;
      end;
      Radio_Options_Lang.OnChange := Options_Change;

    Button_Options_Back:=TKMButton.Create(Panel_Options,120,650,220,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_Options_Back.OnClick:=SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Credits_Page;
begin
  Panel_Credits:=TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);

    TKMLabel.Create(Panel_Credits,232,100,400,20,fTextLibrary[TX_CREDITS],fnt_Outline,taCenter);
    TKMLabel.Create(Panel_Credits,232,140,400,492,
    fTextLibrary[TX_CREDITS_PROGRAMMING]+'|Krom|Lewin||'+
    fTextLibrary[TX_CREDITS_ADDITIONAL_PROGRAMMING]+'|Alex||'+
    fTextLibrary[TX_CREDITS_ADDITIONAL_GRAPHICS]+'|StarGazer||'+
    fTextLibrary[TX_CREDITS_ADDITIONAL_SOUNDS]+'|trb1914||'+
    fTextLibrary[TX_CREDITS_ADDITIONAL_TRANSLATIONS]+TRANSLATOR_CREDITS+
    fTextLibrary[TX_CREDITS_SPECIAL]+'|KaM Community members'
    ,fnt_Grey,taCenter);

    TKMLabel.Create(Panel_Credits,MENU_DESIGN_X div 2 + 200, 100, 500, 20, fTextLibrary[TX_CREDITS_ORIGINAL], fnt_Outline, taCenter);
    Label_Credits := TKMLabelScroll.Create(Panel_Credits, MENU_DESIGN_X div 2 + 200, 140, 500, MENU_DESIGN_Y - 160, fTextLibrary.GetSetupString(300), fnt_Grey, taCenter);

    Button_CreditsBack:=TKMButton.Create(Panel_Credits,120,640,224,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_CreditsBack.OnClick:=SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Loading_Page;
begin
  Panel_Loading:=TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);
    TKMLabel.Create(Panel_Loading, MENU_DESIGN_X div 2, MENU_DESIGN_Y div 2 - 20, 0,0,fTextLibrary[TX_MENU_LOADING],fnt_Outline,taCenter);
    Label_Loading := TKMLabel.Create(Panel_Loading, MENU_DESIGN_X div 2, MENU_DESIGN_Y div 2+10, 0, 0,'...',fnt_Grey,taCenter);
end;


procedure TKMMainMenuInterface.Create_Error_Page;
begin
  Panel_Error := TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);
    TKMLabel.Create(Panel_Error,MENU_DESIGN_X div 2,MENU_DESIGN_Y div 2 - 20,MENU_DESIGN_X-16,30,fTextLibrary[TX_MENU_ERROR],fnt_Antiqua,taCenter);
    Label_Error := TKMLabel.Create(Panel_Error,MENU_DESIGN_X div 2,MENU_DESIGN_Y div 2+10,MENU_DESIGN_X-16,200,'...',fnt_Grey,taCenter);
    Label_Error.AutoWrap := true;
    Button_ErrorBack := TKMButton.Create(Panel_Error,100,640,224,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_ErrorBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Results_Page;
var i, Adv: Integer;
begin
  Panel_Results := TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);
    with TKMImage.Create(Panel_Results,0,0,MENU_DESIGN_X,MENU_DESIGN_Y,7,5) do ImageStretch;

    Label_Results := TKMLabel.Create(Panel_Results,512,200,300,20,'<<<LEER>>>',fnt_Metal,taCenter);

    Panel_Stats := TKMPanel.Create(Panel_Results,80,240,400,400);
    Adv := 0;
    for i:=1 to 9 do
    begin
      inc(Adv, 25);
      if i in [3,6,7,9] then inc(Adv, 15);
      TKMLabel.Create(Panel_Stats,0,Adv,232,20,fTextLibrary.GetSetupString(112+i),fnt_Metal,taLeft);
      Label_Stat[i] := TKMLabel.Create(Panel_Stats,340,Adv,100,20,'00',fnt_Metal,taRight);
    end;

    Button_ResultsBack := TKMButton.Create(Panel_Results,100,650,220,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_ResultsBack.OnClick := SwitchMenuPage;
    Button_ResultsRepeat := TKMButton.Create(Panel_Results,340,650,220,30,fTextLibrary.GetSetupString(18),fnt_Metal,bsMenu);
    Button_ResultsRepeat.OnClick := MainMenu_ReplayLastMap;
    Button_ResultsContinue := TKMButton.Create(Panel_Results,580,650,220,30,fTextLibrary.GetSetupString(17),fnt_Metal,bsMenu);
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
  Panel_ResultsMP := TKMPanel.Create(Panel_Main,0,0,MENU_DESIGN_X,MENU_DESIGN_Y);
    with TKMImage.Create(Panel_ResultsMP,0,0,MENU_DESIGN_X,MENU_DESIGN_Y,7,5) do ImageStretch;

    Label_ResultsMP := TKMLabel.Create(Panel_ResultsMP,512,90,300,20,'<<<LEER>>>',fnt_Metal,taCenter);
    Label_ResultsMPTime := TKMLabel.Create(Panel_ResultsMP,512,120,300,20,'<<<LEER>>>',fnt_Metal,taCenter);

    Panel_StatsMP1 := TKMPanel.Create(Panel_ResultsMP, 112, 140, 800, 260);

      for i:=0 to 7 do
        Label_ResultsPlayerName1[i] := TKMLabel.Create(Panel_StatsMP1, 0, 43+i*25, 140, 20, '', fnt_Metal, taLeft);

      for k:=0 to 4 do
      begin
        with TKMLabel.Create(Panel_StatsMP1, 150 + BarHalf+BarStep*k, 0, BarWidth, 20, fTextLibrary[Columns1[k]], fnt_Metal, taCenter) do
          AutoWrap := true;
        for i:=0 to 7 do
          Bar_Results[i,k] := TKMPercentBar.Create(Panel_StatsMP1, 150 + k*BarStep, 40+i*25, BarWidth, 20, fnt_Metal);
      end;

    Panel_StatsMP2 := TKMPanel.Create(Panel_ResultsMP, 112, 400, 800, 260);

      for i:=0 to 7 do
        Label_ResultsPlayerName2[i] := TKMLabel.Create(Panel_StatsMP2, 0, 43+i*25, 140, 20, '', fnt_Metal, taLeft);

      for k:=0 to 4 do
      begin
        with TKMLabel.Create(Panel_StatsMP2, 150 + BarHalf+BarStep*k, 0, BarWidth, 20, fTextLibrary[Columns2[k]], fnt_Metal, taCenter) do
          AutoWrap := true;
        for i:=0 to 7 do
          Bar_Results[i,k+5] := TKMPercentBar.Create(Panel_StatsMP2, 150 + k*BarStep, 40+i*25, BarWidth, 20, fnt_Metal);
      end;

    Button_ResultsMPBack := TKMButton.Create(Panel_ResultsMP,100,650,220,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_ResultsMPBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.SwitchMenuPage(Sender: TObject);
var i:integer;
begin
  //First thing - hide all existing pages
  for i:=1 to Panel_Main.ChildCount do
    if Panel_Main.Childs[i] is TKMPanel then
      Panel_Main.Childs[i].Hide;

  {Return to MainMenu if Sender unspecified}
  if Sender=nil then Panel_MainMenu.Show;

  {Return to MainMenu}
  if (Sender=Button_SP_Back)or
     (Sender=Button_MP_Back)or
     (Sender=Button_CreditsBack)or
     (Sender=Button_MapEdBack)or
     (Sender=Button_ErrorBack)or
     (Sender=Button_ResultsBack)or
     (Sender=Button_ReplaysBack) then
    Panel_MainMenu.Show;

  {Player leaves lobby (LAN text is updated)}
  if Sender=Button_LobbyBack then
  begin
    Panel_MultiPlayer.Show;
    MP_RefreshClick(Sender);
  end;

  {Return to MainMenu and restore resolution changes}
  if Sender=Button_Options_Back then begin
    fGame.GlobalSettings.FullScreen := OldFullScreen;
    fGame.GlobalSettings.ResolutionID := OldResolution;
    fGame.GlobalSettings.SaveSettings;
    Panel_MainMenu.Show;
  end;

  {Show SinglePlayer menu}
  {Return to SinglePlayerMenu}
  if (Sender=Button_MM_SinglePlayer)or
     (Sender=Button_CampaignBack)or
     (Sender=Button_SingleBack)or
     (Sender=Button_LoadBack) then begin
    Panel_SinglePlayer.Show;
    Button_SP_Replay.Enabled := fGame.ReplayExists('basesave',false);
  end;

  {Show TSK campaign menu}
  if (Sender=Button_SP_TSK) or (Sender=Button_SP_TPR) or (Sender=Button_ResultsContinue) then begin
    if (Sender=Button_SP_TPR) then
      Campaign_Set(fGame.Campaigns.CampaignByTitle('TPR'))
    else
    if (Sender=Button_SP_TSK) then
      Campaign_Set(fGame.Campaigns.CampaignByTitle('TSK'))
    else
      Campaign_Set(fGame.Campaigns.ActiveCampaign);
    Panel_Campaign.Show;
  end;

  {Show SingleMap menu}
  if Sender=Button_SP_Single then
  begin
    fMaps.ScanMapsFolder;
    SingleMap_RefreshList;
    fMap_Selected := EnsureRange(fMap_Selected, 0, fMaps.Count-1);
    ScrollBar_SingleMaps.Position := EnsureRange(ScrollBar_SingleMaps.Position, fMap_Selected-MENU_SP_MAPS_COUNT+1, fMap_Selected);
    SingleMap_ScrollChange(ScrollBar_SingleMaps);
    SingleMap_SelectMap(Shape_SingleOverlay[fMap_Selected-fMaps_Top]);
    Panel_Single.Show;
  end;

  {Show Load menu}
  if Sender=Button_SP_Load then begin
    Load_RefreshList;
    Panel_Load.Show;
  end;

  {Show replays menu}
  if Sender=Button_MM_Replays then begin
    Replays_RefreshList;
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
  if (Sender=Button_MP_Join) or (Sender=Button_MP_CreateLAN) or (Sender=Button_MP_CreateWAN) then begin
    MP_Save_Settings;
    Lobby_Reset(Sender);
    MyControls.CtrlFocus := Edit_LobbyPost;
    Panel_Lobby.Show;
  end;

  {Show MapEditor menu}
  if Sender=Button_MM_MapEd then begin
    MapEditor_ListUpdate(Radio_MapEd_MapType.ItemIndex = 1);
    MapEditor_SizeChange(nil);
    Panel_MapEd.Show;
  end;

  {Show Options menu}
  if Sender=Button_MM_Options then begin
    if fGame <> nil then
      Options_Fill(fGame.GlobalSettings);
    Panel_Options.Show;
  end;

  {Show Credits}
  if Sender=Button_MM_Credits then begin
    Panel_Credits.Show;
    Label_Credits.SmoothScrollToTop := TimeGet; //Set initial position
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
  fGame.StartSingleMap(ExeDir+'data\mission\mission0.dat', fTextLibrary[TX_MENU_TUTORIAL_TOWN]);
end;


procedure TKMMainMenuInterface.MainMenu_PlayBattle(Sender: TObject);
begin
  fGame.StartSingleMap(ExeDir+'data\mission\mission99.dat', fTextLibrary[TX_MENU_TUTORIAL_BATTLE]);
end;


//Should be done this way since fGame is NIL on UI creation
procedure TKMMainMenuInterface.MainMenu_StartReplay(Sender: TObject);
begin
  fGame.StartReplay('basesave',false);
end;


procedure TKMMainMenuInterface.MainMenu_ReplayLastMap(Sender: TObject);
begin
  fGame.StartLastMap; //Means replay last map
end;


procedure TKMMainMenuInterface.Campaign_Set(aCampaign:TKMCampaign);
const MapPic:array[boolean]of byte = (10,11);
var i:integer;
begin
  Campaign_Selected := aCampaign;

  //Choose background
  Image_CampaignBG.RXid := Campaign_Selected.BackGroundPicRX;
  Image_CampaignBG.TexID := Campaign_Selected.BackGroundPicID;

  //Setup sites
  for i:=0 to High(Image_CampaignNodes) do
  begin
    Image_CampaignNodes[i].Visible   := i < Campaign_Selected.MapCount;
    Image_CampaignNodes[i].TexID     := MapPic[i<Campaign_Selected.UnlockedMaps];
    Image_CampaignNodes[i].HighlightOnMouseOver := i<Campaign_Selected.UnlockedMaps;
  end;

  //Place sites
  for i:=0 to Campaign_Selected.MapCount-1 do
  begin
    Image_CampaignNodes[i].Left := Campaign_Selected.Maps[i].Node.X - Image_CampaignNodes[i].Width div 2;
    Image_CampaignNodes[i].Top  := Campaign_Selected.Maps[i].Node.Y - Image_CampaignNodes[i].Height div 2;
  end;

  //Select last map to play by 'clicking' last node
  Campaign_SelectMap(Image_CampaignNodes[Campaign_Selected.UnlockedMaps-1]);
end;


procedure TKMMainMenuInterface.Campaign_SelectMap(Sender:TObject);
var i:integer;
begin
  if not (Sender is TKMImage) then exit;
  if not TKMImage(Sender).HighlightOnMouseOver then exit; //Skip closed maps

  Campaign_MapIndex := TKMImage(Sender).Tag;

  //Place highlight
  for i:=0 to High(Image_CampaignNodes) do
    Image_CampaignNodes[i].Highlight := (Campaign_MapIndex = i);

  //Connect by sub-nodes
  for i:=0 to High(Image_CampaignSubNode) do
  begin
    Image_CampaignSubNode[i].Visible := InRange(i, 1, Campaign_Selected.SubNodesCount(Campaign_MapIndex)-1);
    Image_CampaignSubNode[i].Left := Campaign_Selected.SubNodesPos(Campaign_MapIndex, i).X;
    Image_CampaignSubNode[i].Top  := Campaign_Selected.SubNodesPos(Campaign_MapIndex, i).Y;
  end;

  Label_CampaignTitle.Caption := Format(fTextLibrary[TX_GAME_MISSION], [Campaign_MapIndex+1]);
  Label_CampaignText.Caption := Campaign_Selected.MissionText(Campaign_MapIndex);

  Panel_CampScroll.Height := 50 + Label_CampaignText.TextSize.Y + 70; //Add offset from top and space on bottom
  Panel_CampScroll.Top := MENU_DESIGN_Y - Panel_CampScroll.Height;
  Image_Scroll.Height := Panel_CampScroll.Height;
end;


procedure TKMMainMenuInterface.Campaign_StartMap(Sender: TObject);
begin
  fGame.StartCampaignMap(Campaign_Selected, Campaign_MapIndex);
end;


procedure TKMMainMenuInterface.SingleMap_RefreshList;
var i,MapID:integer;
begin
  for i:=0 to MENU_SP_MAPS_COUNT-1 do
  begin
    MapID := fMaps_Top + i;
    if MapID > fMaps.Count-1 then begin
      Image_SingleMode[i].TexID       := 0;
      Label_SinglePlayers[i].Caption  := '';
      Label_SingleTitle1[i].Caption   := '';
      Label_SingleTitle2[i].Caption   := '';
      Label_SingleSize[i].Caption     := '';
    end else begin
      Image_SingleMode[i].TexID       := 28+byte(fMaps[MapID].Info.MissionMode <> mm_Tactic)*14;  //28 or 42
      Label_SinglePlayers[i].Caption  := inttostr(fMaps[MapID].Info.PlayerCount);
      Label_SingleTitle1[i].Caption   := fMaps[MapID].Filename;
      Label_SingleTitle2[i].Caption   := fMaps[MapID].SmallDesc;
      Label_SingleSize[i].Caption     := fMaps[MapID].Info.MapSizeText;
    end;
  end;

  ScrollBar_SingleMaps.MaxValue := max(0, fMaps.Count - MENU_SP_MAPS_COUNT);
  ScrollBar_SingleMaps.Position := EnsureRange(ScrollBar_SingleMaps.Position,ScrollBar_SingleMaps.MinValue,ScrollBar_SingleMaps.MaxValue);
end;


procedure TKMMainMenuInterface.SingleMap_ScrollChange(Sender: TObject);
begin
  fMaps_Top := ScrollBar_SingleMaps.Position;
  if InRange(fMap_Selected-fMaps_Top,0,MENU_SP_MAPS_COUNT-1) then
    SingleMap_SelectMap(Shape_SingleOverlay[fMap_Selected-fMaps_Top])
  else
    SingleMap_SelectMap(nil); //Means it is off visible area
  SingleMap_RefreshList;
end;


procedure TKMMainMenuInterface.SingleMap_SelectMap(Sender: TObject);
var i:integer;
begin
  if Sender = nil then
    Shape_SingleMap.Hide //Off visible list
  else
  begin
    i := TKMControl(Sender).Tag;
    if not InRange(fMaps_Top+i, 0, fMaps.Count-1) then exit; //Less items than list space

    Shape_SingleMap.Show;
    Shape_SingleMap.Top := Bevel_SingleBG[i,3].Height * (i+1); // Including header height

    fMap_Selected        := fMaps_Top+i;
    Label_SingleTitle.Caption := fMaps[fMap_Selected].Filename;
    Label_SingleDesc.Caption  := fMaps[fMap_Selected].BigDesc;

    Label_SingleCondTyp.Caption := Format(fTextLibrary[TX_MENU_MISSION_TYPE], [fMaps[fMap_Selected].Info.MissionModeText]);
    Label_SingleCondWin.Caption := Format(fTextLibrary[TX_MENU_WIN_CONDITION], [fMaps[fMap_Selected].Info.VictoryCondition]);
    Label_SingleCondDef.Caption := Format(fTextLibrary[TX_MENU_DEFEAT_CONDITION], [fMaps[fMap_Selected].Info.DefeatCondition]);
  end;
end;


procedure TKMMainMenuInterface.SingleMap_Start(Sender: TObject);
begin
  if not InRange(fMap_Selected, 0, fMaps.Count-1) then exit; //Some odd index
    fGame.StartSingleMap(MapNameToPath(fMaps[fMap_Selected].Filename,'dat',false),fMaps[fMap_Selected].Filename); //Provide mission filename mask and title here
end;


procedure TKMMainMenuInterface.MP_Init(Sender: TObject);
begin
  MP_RefreshClick(Sender); //Refresh the list when they first open the multiplayer page
  Edit_MP_PlayerName.Text := fGame.GlobalSettings.MultiplayerName;
  Edit_MP_IP.Text := fGame.GlobalSettings.MultiplayerIP;
  Edit_MP_Port.Text := fGame.GlobalSettings.LastPort;
  Edit_MP_Room.Text := fGame.GlobalSettings.LastRoom;
  Edit_MP_ServerName.Text := fGame.GlobalSettings.ServerName;
  Edit_MP_ServerPort.Text := fGame.GlobalSettings.ServerPort;

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
  fGame.Networking.OnGameOptions := Lobby_OnGameOptions;
  fGame.Networking.OnMapName      := Lobby_OnMapName;
  fGame.Networking.OnPingInfo     := Lobby_OnPingInfo;
  fGame.Networking.OnStartMap     := fGame.StartMultiplayerMap;
  fGame.Networking.OnStartSave    := fGame.StartMultiplayerSave;
  fGame.Networking.OnDisconnect   := Lobby_OnDisconnect;
  fGame.Networking.OnReassignedHost := Lobby_OnReassignedToHost;
end;


procedure TKMMainMenuInterface.MP_Save_Settings;
begin
  fGame.GlobalSettings.ServerName := Edit_MP_ServerName.Text;
  fGame.GlobalSettings.LastPort := Edit_MP_Port.Text;
  fGame.GlobalSettings.LastRoom := Edit_MP_Room.Text;
  fGame.GlobalSettings.MultiplayerIP := Edit_MP_IP.Text;
  fGame.GlobalSettings.MultiplayerName := Edit_MP_PlayerName.Text;
  fGame.GlobalSettings.ServerPort := Edit_MP_ServerPort.Text;
end;


procedure TKMMainMenuInterface.MP_Update(const aStatus:string; aColor:TColor4; aBusy:boolean);
begin
  Button_MP_CreateLAN.Enabled := not aBusy;
  Button_MP_CreateWAN.Enabled := not aBusy;
  Button_MP_Join.Enabled := not aBusy;
  Label_MP_Status.Caption := aStatus;
  Label_MP_Status.FontColor := aColor;
end;


procedure TKMMainMenuInterface.MP_RefreshClick(Sender: TObject);
begin
  fGame.Networking.ServerQuery.OnListUpdated := MP_ListUpdated;
  fGame.Networking.ServerQuery.RefreshList;
  ColList_Servers.Clear;
  Label_MP_Players.Caption := '';
  Label_MP_GameTime.Caption := '';
  Label_MP_Map.Caption := '';
  ColList_Servers.AddItem([fTextLibrary[TX_MP_MENU_REFRESHING],'','',''],[$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF],-1,-1);
end;


procedure TKMMainMenuInterface.MP_ListUpdated(Sender: TObject);
var i,k:integer; DisplayName:string; ServerFound:boolean;
begin
  //Refresh the display for the list of servers
  ServerFound := false;
  ColList_Servers.Clear;
  for i:=0 to fGame.Networking.ServerQuery.Count-1 do
    with fGame.Networking.ServerQuery.GetServer(i) do
      for k:=0 to RoomCount-1 do
      begin
        ServerFound := true;
        if RoomCount > 1 then DisplayName := Name+' #'+IntToStr(k+1) else DisplayName := Name; //Only show # when needed
        ColList_Servers.AddItem([DisplayName,fTextLibrary[GameStateTextIDs[Rooms[k].GameInfo.GameState]],IntToStr(Rooms[k].PlayerCount),IntToStr(Ping)],
                                [$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,GetPingColor(Ping)], i, k);
      end;
  if not ServerFound then
    ColList_Servers.AddItem([fTextLibrary[TX_MP_MENU_NO_SERVERS],'','',''],[$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF],-1,-1);
end;


procedure TKMMainMenuInterface.MP_AnnouncementsUpdated(const S: string);
begin
  Memo_MP_Announcement.Clear;
  Memo_MP_Announcement.Add(S);
end;


procedure TKMMainMenuInterface.MP_SelectServer(Sender: TObject);
begin
  if not InRange(ColList_Servers.GetItemTag, 0, fGame.Networking.ServerQuery.Count-1) then exit;
    with fGame.Networking.ServerQuery.GetServer(ColList_Servers.GetItemTag) do
    begin
      Edit_MP_IP.Text := IP;
      Edit_MP_Port.Text := Port;
      Edit_MP_Room.Text := IntToStr(Rooms[ColList_Servers.GetItemTag2].RoomID);
      Label_MP_Players.Caption := Rooms[ColList_Servers.GetItemTag2].GameInfo.Players;
      Label_MP_GameTime.Caption := Rooms[ColList_Servers.GetItemTag2].GameInfo.GetFormattedTime;
      Label_MP_Map.Caption := Rooms[ColList_Servers.GetItemTag2].GameInfo.Map;
    end;
end;


procedure TKMMainMenuInterface.MP_Host_Click(Sender: TObject);
begin
  MP_Save_Settings; //Save the player and IP name so it is not lost if something fails
  if Trim(Edit_MP_PlayerName.Text) = '' then
  begin
    MP_Update(fTextLibrary[TX_GAME_ERROR_BLANK_PLAYERNAME],$FF007FFF,false);
    fSoundLib.Play(sfxn_Error2);
    exit;
  end;
  SwitchMenuPage(Sender); //Open lobby page

  MP_BindEvents;
  fGame.Networking.OnHostFail := MP_HostFail;
  fGame.Networking.Host(Edit_MP_PlayerName.Text, Edit_MP_ServerName.Text, Edit_MP_ServerPort.Text, (Sender = Button_MP_CreateWAN));
end;

procedure TKMMainMenuInterface.MP_JoinClick(Sender: TObject);
begin
  MP_Save_Settings; //Save the player and IP name so it is not lost if the connection fails
  if Trim(Edit_MP_PlayerName.Text) = '' then
  begin
    MP_Update(fTextLibrary[TX_GAME_ERROR_BLANK_PLAYERNAME],$FF007FFF,false);
    fSoundLib.Play(sfxn_Error2);
    exit;
  end;
  //Disable buttons to prevent multiple clicks while connection process is in progress
  Button_MP_CreateLAN.Disable;
  Button_MP_CreateWAN.Disable;
  Button_MP_Join.Disable;
  MP_Update(fTextLibrary[TX_MP_MENU_STATUS_CONNECTING],$FF00FF00,true);

  //Send request to join
  fGame.Networking.OnJoinSucc := MP_JoinSuccess;
  fGame.Networking.OnJoinFail := MP_JoinFail;
  fGame.Networking.OnJoinAssignedHost := MP_JoinAssignedHost;
  fGame.Networking.Join(Edit_MP_IP.Text, Edit_MP_Port.Text, Edit_MP_PlayerName.Text, StrToIntDef(Edit_MP_Room.Text,-1)); //Init lobby
end;


//We had recieved permission to join
procedure TKMMainMenuInterface.MP_JoinSuccess(Sender: TObject);
begin
  SwitchMenuPage(Button_MP_Join); //Open lobby page

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


procedure TKMMainMenuInterface.MP_Back_Click(Sender: TObject);
begin
  fGame.Networking.Disconnect;
  MP_Save_Settings;
  SwitchMenuPage(Sender);
end;


procedure TKMMainMenuInterface.MP_HostFail(const aData:string);
begin
  fGame.Networking.Disconnect;
  SwitchMenuPage(Button_LobbyBack);
  MP_Update(aData,$FF007FFF,false);
  fSoundLib.Play(sfxn_Error2);
end;


//Reset everything to it's defaults depending on users role (Host/Joiner/Reassigned)
procedure TKMMainMenuInterface.Lobby_Reset(Sender: TObject; aPreserveMessage:boolean=false);
var i:integer;
begin
  for i:=0 to MAX_PLAYERS-1 do
  begin
    Label_LobbyPlayer[i].Caption := '.';
    Image_LobbyFlag[i].TexID := 0;
    Label_LobbyPlayer[i].Hide;
    DropBox_LobbyPlayerSlot[i].Show;
    DropBox_LobbyPlayerSlot[i].Disable;
    DropBox_LobbyLoc[i].ItemIndex := 0;
    DropBox_LobbyLoc[i].Disable;
    DropBox_LobbyTeam[i].Disable;
    DropBox_LobbyTeam[i].ItemIndex := 0;
    DropColorBox_Lobby[i].Disable;
    DropColorBox_Lobby[i].ColorIndex := 0;
    DropBox_LobbyPlayerSlot[i].ItemIndex := 0; //Open
    Button_LobbyKick[i].Disable;
    Label_LobbyPing[i].Caption := '';
  end;

  if not aPreserveMessage then Memo_LobbyPosts.Clear;
  Edit_LobbyPost.Text := '';

  Label_LobbyMapName.Caption := '';
  Memo_LobbyMapDesc.Clear;
  Label_LobbyMapCount.Caption := fTextLibrary[TX_LOBBY_MAP_PLAYERS];
  Label_LobbyMapMode.Caption := fTextLibrary[TX_LOBBY_MAP_MODE];
  Label_LobbyMapCond.Caption := fTextLibrary[TX_LOBBY_MAP_CONDITIONS];
  Label_LobbyMapSize.Caption := fTextLibrary[TX_LOBBY_MAP_SIZE];

  Lobby_OnMapName('');
  if (Sender = Button_MP_CreateWAN) or (Sender = Button_MP_CreateLAN) then begin
    Radio_LobbyMapType.Show;
    Radio_LobbyMapType.ItemIndex := 0;
    Lobby_MapTypeSelect(nil);
    List_Lobby.Show;
    Label_LobbyMapName.Hide;
    Label_LobbyChooseMap.Show;
    Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_START]; //Start
    Button_LobbyStart.Disable;
  end else begin
    Radio_LobbyMapType.Hide;
    List_Lobby.Hide;
    Label_LobbyMapName.Show;
    Label_LobbyChooseMap.Hide;
    Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_READY]; //Ready
    Button_LobbyStart.Enable;
  end;
end;


procedure TKMMainMenuInterface.Lobby_GameOptionsChange(Sender: TObject);
begin
  //fGame.Networking.NetGameOptions.Peacetime :=
  fGame.Networking.SendGameOptions;
end;


procedure TKMMainMenuInterface.Lobby_OnGameOptions(Sender: TObject);
begin
  //fGame.Networking.NetGameOptions.Peacetime
end;


//Try to change players setup, Networking will check if it can be done under current
//conditions immediately and reverts the change without disturbing Host.
//If the change is possible Networking will send query to the Host.
//Host will reply with OnPlayersSetup event and data will be actualized.
procedure TKMMainMenuInterface.Lobby_PlayersSetupChange(Sender: TObject);
var i:integer;
begin
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
    if (Sender = DropColorBox_Lobby[i]) and DropColorBox_Lobby[i].Enabled then
    begin
      fGame.Networking.SelectColor(DropColorBox_Lobby[i].ColorIndex, i+1);
      DropColorBox_Lobby[i].ColorIndex := fGame.Networking.NetPlayers[i+1].FlagColorID;
    end;

    //Kick
    if (Sender = Button_LobbyKick[i]) and Button_LobbyKick[i].Enabled then
    begin
      fGame.Networking.KickPlayer(i+1);
    end;

    if Sender = DropBox_LobbyPlayerSlot[i] then
    begin
      if i < fGame.Networking.NetPlayers.Count then
      begin
        if (fGame.Networking.NetPlayers[i+1].PlayerType = pt_Computer) and (DropBox_LobbyPlayerSlot[i].ItemIndex = 0) then
          fGame.Networking.NetPlayers.RemAIPlayer(i+1);
      end
      else
        if DropBox_LobbyPlayerSlot[i].ItemIndex = 1 then
        begin
          fGame.Networking.NetPlayers.AddAIPlayer;
          if fGame.Networking.SelectGameKind = ngk_Save then
            fGame.Networking.MatchPlayersToSave(fGame.Networking.NetPlayers.Count); //Match new AI player in save
        end;
      fGame.Networking.SendPlayerListAndRefreshPlayersSetup;
    end;
  end;
end;


//Players list has been updated
//We should reflect it to UI
procedure TKMMainMenuInterface.Lobby_OnPlayersSetup(Sender: TObject);
var i:integer; MyNik, CanEdit, IsSave, IsValid:boolean;
begin
  IsSave := fGame.Networking.SelectGameKind = ngk_Save;
  for i:=0 to fGame.Networking.NetPlayers.Count - 1 do
  begin
    Label_LobbyPlayer[i].Caption := fGame.Networking.NetPlayers[i+1].Nikname;
    if fGame.Networking.NetPlayers[i+1].LangID <> 0 then
         Image_LobbyFlag[i].TexID := StrToInt(Locales[fGame.Networking.NetPlayers[i+1].LangID,3])
    else Image_LobbyFlag[i].TexID := 0;
    if (fGame.Networking.NetPlayers[i+1].PlayerType = pt_Computer) and fGame.Networking.IsHost then
    begin
      Label_LobbyPlayer[i].Hide;
      DropBox_LobbyPlayerSlot[i].Show;
      DropBox_LobbyPlayerSlot[i].ItemIndex := 1; //AI
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
    DropColorBox_Lobby[i].ColorIndex := fGame.Networking.NetPlayers[i+1].FlagColorID;
    CheckBox_LobbyReady[i].Checked := fGame.Networking.NetPlayers[i+1].ReadyToStart;

    MyNik := (i+1 = fGame.Networking.MyIndex); //Our index
    //We are allowed to edit if it is our nickname and we are set as NOT ready,
    //or we are the host and this player is an AI
    CanEdit := (MyNik and (fGame.Networking.IsHost or not fGame.Networking.NetPlayers[i+1].ReadyToStart)) or
               (fGame.Networking.IsHost and (fGame.Networking.NetPlayers[i+1].PlayerType = pt_Computer));
    DropBox_LobbyLoc[i].Enabled := CanEdit;
    DropBox_LobbyTeam[i].Enabled := CanEdit and not IsSave; //Can't change color or teams in a loaded save
    DropColorBox_Lobby[i].Enabled := CanEdit and not IsSave;
    CheckBox_LobbyReady[i].Enabled := false; //Read-only, just for info (perhaps we will replace it with an icon)
    Button_LobbyKick[i].Enabled := (fGame.Networking.NetPlayers[i+1].PlayerType = pt_Human) and
                                    fGame.Networking.IsHost and not MyNik; //Can't kick self
    if MyNik and not fGame.Networking.IsHost then
    begin
      if fGame.Networking.NetPlayers[i+1].ReadyToStart then
        Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_NOT_READY]
      else
        Button_LobbyStart.Caption := fTextLibrary[TX_LOBBY_READY];
    end
  end;

  for i:=fGame.Networking.NetPlayers.Count to MAX_PLAYERS-1 do
  begin
    Label_LobbyPlayer[i].Caption := '';
    Image_LobbyFlag[i].TexID := 0;
    Label_LobbyPlayer[i].Hide;
    DropBox_LobbyPlayerSlot[i].Show;
    DropBox_LobbyPlayerSlot[i].ItemIndex := 0; //Open
    DropBox_LobbyLoc[i].ItemIndex := 0;
    DropBox_LobbyTeam[i].ItemIndex := 0;
    DropColorBox_Lobby[i].ColorIndex := 0;
    //Only host may change player slots, and only the first unused slot may be changed (so there are no gaps in net players list)
    DropBox_LobbyPlayerSlot[i].Enabled := fGame.Networking.IsHost and (i = fGame.Networking.NetPlayers.Count);
    CheckBox_LobbyReady[i].Checked := false;
    DropBox_LobbyLoc[i].Disable;
    DropBox_LobbyTeam[i].Disable;
    DropColorBox_Lobby[i].Disable;
    Button_LobbyKick[i].Disable;
    CheckBox_LobbyReady[i].Disable; //Read-only, just for info (perhaps we will replace it with an icon)
  end;

  if fGame.Networking.IsHost then
    Button_LobbyStart.Enabled := fGame.Networking.CanStart;
  //If the game can't be started the text message with explanation will appear in chat area
end;


procedure TKMMainMenuInterface.Lobby_OnPingInfo(Sender: TObject);
var i:integer;
begin
  for i:=0 to MAX_PLAYERS-1 do
  if (fGame.Networking.Connected) and (i < fGame.Networking.NetPlayers.Count) and
     (fGame.Networking.NetPlayers[i+1].PlayerType <> pt_Computer) then
  begin
    Label_LobbyPing[i].Caption := inttostr(fGame.Networking.NetPlayers[i+1].GetInstantPing);
    Label_LobbyPing[i].FontColor := GetPingColor(fGame.Networking.NetPlayers[i+1].GetInstantPing);
  end
  else
    Label_LobbyPing[i].Caption := '';
end;


procedure TKMMainMenuInterface.Lobby_MapTypeSelect(Sender: TObject);
begin

  if Radio_LobbyMapType.ItemIndex = 0 then //Build Map
  begin
    fMapsMP.ScanMapsFolder;
    List_Lobby.DefaultCaption := fTextLibrary[TX_LOBBY_MAP_SELECT];
    List_Lobby.SetItems(fMapsMP.MapListBuild);
  end
  else if Radio_LobbyMapType.ItemIndex = 1 then //Fight Map
  begin
    fMapsMP.ScanMapsFolder;
    List_Lobby.DefaultCaption := fTextLibrary[TX_LOBBY_MAP_SELECT];
    List_Lobby.SetItems(fMapsMP.MapListFight);
  end
  else if Radio_LobbyMapType.ItemIndex = 2 then //Co-op Map
  begin
    fMapsMP.ScanMapsFolder;
    List_Lobby.DefaultCaption := fTextLibrary[TX_LOBBY_MAP_SELECT];
    List_Lobby.SetItems(fMapsMP.MapListCoop);
  end
  else if Radio_LobbyMapType.ItemIndex = 3 then //Saved Game
  begin
    fSavesMP.ScanSavesFolder(true);
    List_Lobby.DefaultCaption := fTextLibrary[TX_LOBBY_MAP_SELECT_SAVED];
    List_Lobby.SetItems(fSavesMP.SavesList);
  end;
  if Sender <> nil then //This is used in Reset_Lobby when we are not connected
    fGame.Networking.SelectNoMap(fTextLibrary[TX_LOBBY_MAP_NONE]);
end;


//Just pass Filename to Networking, it will check validity itself
procedure TKMMainMenuInterface.Lobby_MapSelect(Sender: TObject);
begin
  if Radio_LobbyMapType.ItemIndex < 3 then
    fGame.Networking.SelectMap(List_Lobby.Item[List_Lobby.ItemIndex])
  else
    fGame.Networking.SelectSave(List_Lobby.Item[List_Lobby.ItemIndex]);
end;


//todo: Fill in map info
procedure TKMMainMenuInterface.Lobby_OnMapName(const aData:string);
var i:Integer; DropText:string;
begin
  if fGame.Networking.SelectGameKind <> ngk_None then
  begin
    if fGame.Networking.SelectGameKind = ngk_Save then
    begin
      Label_LobbyMapName.Caption := fGame.Networking.SaveInfo.Filename;
      Memo_LobbyMapDesc.Clear;
      Memo_LobbyMapDesc.Text := fGame.Networking.GameInfo.GetTitleWithTime;
    end
    else
    begin
      Label_LobbyMapName.Caption := fGame.Networking.GameInfo.Title;
      Memo_LobbyMapDesc.Text := fGame.Networking.MapInfo.BigDesc;
    end;

    Label_LobbyMapCount.Caption := Format(fTextLibrary[TX_LOBBY_MAP_PLAYERS],[fGame.Networking.GameInfo.PlayerCount]);
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
    Label_LobbyMapCount.Caption := Format(fTextLibrary[TX_LOBBY_MAP_PLAYERS],[0]);
    Label_LobbyMapMode.Caption := fTextLibrary[TX_LOBBY_MAP_MODE];
    //Label_LobbyMapCond.Caption :=
    Label_LobbyMapSize.Caption := fTextLibrary[TX_LOBBY_MAP_SIZE];

    //Update starting locations
    if fGame.Networking.SelectGameKind = ngk_Save then
      DropText := fTextLibrary[TX_LOBBY_SELECT] + eol
    else
      DropText := fTextLibrary[TX_LOBBY_RANDOM] + eol;

    for i:=0 to MAX_PLAYERS-1 do
      DropBox_LobbyLoc[i].SetItems(DropText);
  end;
end;


//We have been assigned to be the host of the game because the host disconnected. Reopen lobby page in correct mode.
procedure TKMMainMenuInterface.Lobby_OnReassignedToHost(Sender: TObject);
begin
  Lobby_Reset(Button_MP_CreateLAN,true); //Will reset the lobby page into host mode, preserving messages
  if fGame.Networking.SelectGameKind = ngk_Save then
    Radio_LobbyMapType.ItemIndex := 1
  else
    Radio_LobbyMapType.ItemIndex := 0;

  Lobby_MapTypeSelect(nil);
  if fGame.Networking.SelectGameKind = ngk_Save then
    List_Lobby.SelectByName(fGame.Networking.SaveInfo.Filename) //Select the map
  else
    if fGame.Networking.SelectGameKind = ngk_Map then
      List_Lobby.SelectByName(fGame.Networking.MapInfo.Filename); //Select the map
end;


//Post what user has typed
procedure TKMMainMenuInterface.Lobby_PostKey(Sender: TObject; Key: Word);
begin
  if (Key <> VK_RETURN) or (Trim(Edit_LobbyPost.Text) = '') then exit;
  fGame.Networking.PostMessage(Edit_LobbyPost.Text, true);
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
  Load_DeleteConfirmation(false);
  Button_Delete.Enabled := InRange(List_Load.ItemIndex, 0, fSaves.Count-1);
  Button_Load.Enabled := InRange(List_Load.ItemIndex, 0, fSaves.Count-1)
                         and fSaves[List_Load.ItemIndex].IsValid;
end;


procedure TKMMainMenuInterface.Load_Click(Sender: TObject);
begin
  if not InRange(List_Load.ItemIndex, 0, fSaves.Count-1) then Exit;
  fGame.StartSingleSave(fSaves[List_Load.ItemIndex].Filename);
end;


procedure TKMMainMenuInterface.Load_Delete_Click(Sender: TObject);
var PreviouslySelected:integer;
begin
  if not InRange(List_Load.ItemIndex, 0, List_Load.Count-1) then Exit;
  
  if Sender = Button_Delete then
    Load_DeleteConfirmation(true);

  if (Sender = Button_DeleteYes) or (Sender = Button_DeleteNo) then
    Load_DeleteConfirmation(false); //Hide confirmation anyways

  //Delete the savegame
  if Sender = Button_DeleteYes then
  begin
    PreviouslySelected := List_Load.ItemIndex;
    fSaves.DeleteSave(List_Load.ItemIndex);
    Load_RefreshList;
    if List_Load.Count > 0 then
      List_Load.ItemIndex := EnsureRange(PreviouslySelected, 0, List_Load.Count-1);
    Load_ListClick(List_Load);
  end;
end;


procedure TKMMainMenuInterface.Load_RefreshList;
var i:integer;
begin
  fSaves.ScanSavesFolder(false);
  List_Load.Clear;

  for i:=0 to fSaves.Count-1 do
    List_Load.AddItem([fSaves[i].Filename, fSaves[i].Info.GetTitleWithTime], [$FFFFFFFF, $FFFFFFFF]);

  //Select first Save by default
  if List_Load.Count > 0 then
    List_Load.ItemIndex := 0;

  Load_ListClick(List_Load);

  Load_DeleteConfirmation(false);
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
                                and fGame.ReplayExists(fSaves[List_Replays.ItemIndex].Filename, (Radio_Replays_Type.ItemIndex = 1));
end;


procedure TKMMainMenuInterface.Replay_TypeChange(Sender: TObject);
begin
  Replays_RefreshList;
end;


procedure TKMMainMenuInterface.Replays_RefreshList;
var i:integer;
begin
  fSaves.ScanSavesFolder((Radio_Replays_Type.ItemIndex = 1));
  List_Replays.Clear;

  for i:=0 to fSaves.Count-1 do
    List_Replays.AddItem([fSaves[i].Filename, fSaves[i].Info.GetTitleWithTime], [$FFFFFFFF, $FFFFFFFF]);

  //Select first Save by default
  if List_Replays.Count > 0 then
    List_Replays.ItemIndex := 0;

  Replays_ListClick(List_Replays);
end;


procedure TKMMainMenuInterface.Replays_Play(Sender: TObject);
begin
  if not InRange(List_Replays.ItemIndex, 0, fSaves.Count-1) then Exit;
  fGame.StartReplay(fSaves[List_Replays.ItemIndex].Filename,(Radio_Replays_Type.ItemIndex = 1));
end;


procedure TKMMainMenuInterface.MapEditor_Start(Sender: TObject);
begin
  if Sender = Button_MapEd_Create then
    fGame.StartMapEditor('', false, MapEdSizeX, MapEdSizeY); //Provide mission filename here, Mapsize will be ignored if map exists
  if (Sender = Button_MapEd_Load) and (List_MapEd.ItemIndex <> -1) then
    fGame.StartMapEditor(MapNameToPath(List_MapEd.Item[List_MapEd.ItemIndex], 'dat', Radio_MapEd_MapType.ItemIndex = 1), Radio_MapEd_MapType.ItemIndex = 1, 0, 0); //Provide mission filename here, Mapsize will be ignored if map exists
end;


procedure TKMMainMenuInterface.MapEditor_SizeChange(Sender: TObject);
begin
  //Find out new map dimensions
  MapEdSizeX := MapSize[Radio_MapEd_SizeX.ItemIndex+1];
  MapEdSizeY := MapSize[Radio_MapEd_SizeY.ItemIndex+1];
end;


procedure TKMMainMenuInterface.MapEditor_MapTypeChange(Sender: TObject);
begin
  MapEditor_ListUpdate(Radio_MapEd_MapType.ItemIndex = 1);
  List_MapEd.ItemIndex := 0;
end;


procedure TKMMainMenuInterface.MapEditor_ListUpdate(aUseMP:boolean);
begin
  if aUseMP then
  begin
    fMapsMP.ScanMapsFolder;
    List_MapEd.SetItems(fMapsMP.MapList);
  end
  else
  begin
    fMaps.ScanMapsFolder;
    List_MapEd.SetItems(fMaps.MapList);
  end;
  List_MapEd.ItemIndex := 0; //Select first map by default, otherwise there could be an invalid map selected (if items have been removed since we last updated)
end;


//This is called when the options page is shown, so update all the values
//Note: Options can be required to fill before fGame is completely initialized, hence we need to pass either fGame.Settings or a direct Settings link
procedure TKMMainMenuInterface.Options_Fill(aGlobalSettings:TGlobalSettings);
var i:cardinal;
begin
  CheckBox_Options_Autosave.Checked   := aGlobalSettings.Autosave;
  Ratio_Options_Brightness.Position   := aGlobalSettings.Brightness;
  Ratio_Options_Mouse.Position        := aGlobalSettings.MouseSpeed;
  Ratio_Options_ScrollSpeed.Position  := aGlobalSettings.ScrollSpeed;
  Ratio_Options_SFX.Position          := aGlobalSettings.SoundFXVolume;
  Ratio_Options_Music.Position        := aGlobalSettings.MusicVolume;
  CheckBox_Options_MusicOn.Checked    := not aGlobalSettings.MusicOn;
  Ratio_Options_Music.Enabled         := not CheckBox_Options_MusicOn.Checked;
  CheckBox_Options_ShuffleOn.Checked  := aGlobalSettings.ShuffleOn;
  CheckBox_Options_ShuffleOn.Enabled  := not CheckBox_Options_MusicOn.Checked;

  for i:=1 to LOCALES_COUNT do
    if SameText(aGlobalSettings.Locale, Locales[i,1]) then
      Radio_Options_Lang.ItemIndex := i-1;

  CheckBox_Options_FullScreen.Checked := aGlobalSettings.FullScreen;
  for i:=1 to RESOLUTION_COUNT do begin
    CheckBox_Options_Resolution[i].Checked := (i = aGlobalSettings.ResolutionID);
    CheckBox_Options_Resolution[i].Enabled := (SupportedRefreshRates[i] > 0) AND aGlobalSettings.FullScreen;
  end;

  OldFullScreen := aGlobalSettings.FullScreen;
  OldResolution := aGlobalSettings.ResolutionID;
  Button_Options_ResApply.Disable;
end;


procedure TKMMainMenuInterface.Options_Change(Sender: TObject);
var i:cardinal; MusicToggled, ShuffleToggled: boolean;
begin
  MusicToggled := (fGame.GlobalSettings.MusicOn = CheckBox_Options_MusicOn.Checked);
  ShuffleToggled := (not fGame.GlobalSettings.ShuffleOn = CheckBox_Options_ShuffleOn.Checked);

  fGame.GlobalSettings.Autosave         := CheckBox_Options_Autosave.Checked;
  fGame.GlobalSettings.Brightness       := Ratio_Options_Brightness.Position;
  fGame.GlobalSettings.MouseSpeed       := Ratio_Options_Mouse.Position;
  fGame.GlobalSettings.ScrollSpeed      := Ratio_Options_ScrollSpeed.Position;
  fGame.GlobalSettings.SoundFXVolume    := Ratio_Options_SFX.Position;
  fGame.GlobalSettings.MusicVolume      := Ratio_Options_Music.Position;
  fGame.GlobalSettings.MusicOn          := not CheckBox_Options_MusicOn.Checked;
  fGame.GlobalSettings.ShuffleOn        := CheckBox_Options_ShuffleOn.Checked;
  fGame.GlobalSettings.FullScreen       := CheckBox_Options_FullScreen.Checked;
  Ratio_Options_Music.Enabled           := not CheckBox_Options_MusicOn.Checked;
  CheckBox_Options_ShuffleOn.Enabled    := not CheckBox_Options_MusicOn.Checked;

  fSoundLib.UpdateSoundVolume(fGame.GlobalSettings.SoundFXVolume / fGame.GlobalSettings.SlidersMax);
  fGame.MusicLib.UpdateMusicVolume(fGame.GlobalSettings.MusicVolume / fGame.GlobalSettings.SlidersMax);
  if MusicToggled then
  begin
    fGame.MusicLib.ToggleMusic(fGame.GlobalSettings.MusicOn);
    if fGame.GlobalSettings.MusicOn then
      ShuffleToggled := true; //Re-shuffle songs if music has been enabled
  end;
  if ShuffleToggled then
    fGame.MusicLib.ToggleShuffle(fGame.GlobalSettings.ShuffleOn);

  if Sender = Radio_Options_Lang then begin
    ShowScreen(msLoading, fTextLibrary[TX_MENU_NEWLOCAL]);
    fRender.Render; //Force to repaint loading screen
    fGame.ToggleLocale(Locales[Radio_Options_Lang.ItemIndex+1,1]);
    exit; //Whole interface will be recreated
  end;

  if Sender = Button_Options_ResApply then begin //Apply resolution changes
    OldFullScreen := fGame.GlobalSettings.FullScreen; //memorize (it will be niled on re-init anyway, but we might change that in future)
    OldResolution := fGame.GlobalSettings.ResolutionID;
    fGame.ToggleFullScreen(fGame.GlobalSettings.FullScreen, true);
    exit; //Whole interface will be recreated
  end;

  for i:=1 to RESOLUTION_COUNT do
    if Sender = CheckBox_Options_Resolution[i] then
      fGame.GlobalSettings.ResolutionID := i;

  for i:=1 to RESOLUTION_COUNT do begin
    CheckBox_Options_Resolution[i].Checked := (i = fGame.GlobalSettings.ResolutionID);
    CheckBox_Options_Resolution[i].Enabled := (SupportedRefreshRates[i] > 0) AND fGame.GlobalSettings.FullScreen;
  end;

  //Make button enabled only if new resolution/mode differs from old
  Button_Options_ResApply.Enabled := (OldFullScreen <> fGame.GlobalSettings.FullScreen) or
                                     (fGame.GlobalSettings.FullScreen and (OldResolution <> fGame.GlobalSettings.ResolutionID));
end;


procedure TKMMainMenuInterface.Options_FlagClick(Sender: TObject);
begin
  Assert(Sender is TKMImage);
  Radio_Options_Lang.ItemIndex := TKMImage(Sender).Tag;
  Options_Change(Radio_Options_Lang);
end;


procedure TKMMainMenuInterface.KeyDown(Key:Word; Shift: TShiftState);
begin
  if MyControls.KeyDown(Key, Shift) then exit; //Handled by Controls
end;


procedure TKMMainMenuInterface.KeyPress(Key: Char);
begin
  MyControls.KeyPress(Key);
end;


procedure TKMMainMenuInterface.KeyUp(Key:Word; Shift: TShiftState);
begin
  if MyControls.KeyUp(Key, Shift) then exit; //Handled by Controls
end;


procedure TKMMainMenuInterface.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  MyControls.MouseDown(X,Y,Shift,Button);
end;


//Do something related to mouse movement in menu
procedure TKMMainMenuInterface.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  MyControls.MouseMove(X,Y,Shift);

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
  MyControls.MouseUp(X,Y,Shift,Button);
  exit; //We could have caused fGame reinit, so exit at once
end;


procedure TKMMainMenuInterface.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);
begin
  MyControls.MouseWheel(X, Y, WheelDelta);
end;


{Should update anything we want to be updated, obviously}
procedure TKMMainMenuInterface.UpdateState;
begin
  //
end;


procedure TKMMainMenuInterface.Paint;
begin
  MyControls.Paint;
end;


end.
