unit KM_InterfaceMainMenu;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLType, {$ENDIF}
  {$IFDEF WDC} ShellAPI, Windows, {$ENDIF} //Required for OpenURL in Delphi
  {$IFDEF FPC} LCLIntf, {$ENDIF} //Required for OpenURL in Lazarus
  StrUtils, SysUtils, KromUtils, KromOGLUtils, Math, Classes, Forms, Controls,
  KM_Controls, KM_Defaults, KM_Maps, KM_Campaigns, KM_Saves, KM_Pics,
  KM_InterfaceDefaults, KM_Minimap, KM_ServerQuery,
  KM_GUIMenuCampaign,
  KM_GUIMenuLobby,
  KM_GUIMenuOptions,
  KM_GUIMenuResultsMP,
  KM_GUIMenuSingleMap;


type
  TMenuScreen = (msError, msLoading, msMain, msOptions, msResults, msResultsMP);

  TKMMainMenuInterface = class (TKMUserInterface)
  private
    fServerSelected: Boolean;
    fSelectedRoomInfo: TKMRoomInfo;
    fSelectedServerInfo: TKMServerInfo;

    fMaps: TKMapsCollection;
    fMapsMP: TKMapsCollection;
    fSaves: TKMSavesCollection;
    fMinimap: TKMMinimap;

    fLobbyBusy: Boolean;
    fGameResultMsg: TGameResultMsg; //So we know where to go after results screen
    fWaresVisible: array [WARE_MIN..WARE_MAX] of Boolean; //For MP results page

    fLastMapCRC: Cardinal; //CRC of selected map
    fLastSaveCRC: Cardinal; //CRC of selected save

    fLobby: TKMGUIMenuLobby;
    fOptions: TKMGUIMainOptions;
    fGuiCampaign: TKMGUIMainCampaign;
    fSingleMap: TKMGUIMenuSingleMap;
    fResultsMP: TKMGUIMenuResultsMP;
    //fPages: array [TGUIPage] of TKMGUIPage;

    procedure Create_MainMenu;
    procedure Create_SinglePlayer;
    procedure Create_CampSelect;
    procedure Create_Load;
    procedure Create_MultiPlayer;
    procedure Create_MapEditor;
    procedure Create_Replays;
    procedure Create_Credits;
    procedure Create_Loading;
    procedure Create_Error;
    procedure Create_Results;
    procedure PageChange(Sender: TObject; Dest: TGUIPage; aText: string);
    procedure SwitchMenuPage(Sender: TObject);
    procedure MainMenu_MultiplayerClick(Sender: TObject);
    procedure MainMenu_PlayTutorial(Sender: TObject);
    procedure MainMenu_PlayBattle(Sender: TObject);
    procedure Campaign_FillList;
    procedure Campaign_ListChange(Sender: TObject);
    procedure Credits_LinkClick(Sender: TObject);

    procedure MP_Init(Sender: TObject);
    procedure MP_SaveSettings;
    procedure MP_Update(const aStatus: string; aColor: TColor4; aBusy: Boolean);
    procedure MP_ServersUpdateList(Sender: TObject);
    procedure MP_AnnouncementsUpdated(const S: string);
    procedure MP_CreateServerClick(Sender: TObject);
    procedure MP_FindServerClick(Sender: TObject);
    procedure MP_CreateServerCancelClick(Sender: TObject);
    procedure MP_FindServerIPClick(Sender: TObject);
    procedure MP_PasswordClick(Sender: TObject);
    procedure MP_FindServerCancelClick(Sender: TObject);
    procedure MP_ServersRefresh(Sender: TObject);
    procedure MP_ServersSort(aIndex: Integer);
    procedure MP_ServersClick(Sender: TObject);
    procedure MP_ServersDoubleClick(Sender: TObject);
    procedure MP_GetInClick(Sender: TObject);
    function MP_ValidatePlayerName(const aName: string): Boolean;
    function MP_GetInEnabled: Boolean;
    procedure MP_Join(aServerAddress, aPort: string; aRoom: Integer);
    procedure MP_JoinPassword(Sender: TObject);
    procedure MP_JoinSuccess(Sender: TObject);
    procedure MP_JoinFail(const aData: string);
    procedure MP_JoinAssignedHost(Sender: TObject);
    procedure MP_HostClick(Sender: TObject);
    procedure MP_HostFail(const aData: string);
    procedure MP_BackClick(Sender: TObject);

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

    procedure Results_GraphToggle(Sender: TObject);
    procedure Results_RepeatLastMap(Sender: TObject);
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
      ColumnBox_Servers: TKMColumnBox;
      Label_Servers_Status: TKMLabel;
      Button_MP_Back: TKMButton;
      Button_MP_Refresh: TKMButton;
      Button_MP_GetIn: TKMButton;

      Panel_MPPlayerName: TKMPanel;
        Edit_MP_PlayerName: TKMEdit;
        Label_MP_Status: TKMLabel;
      Button_MP_CreateServer: TKMButton;
      Button_MP_FindServer: TKMButton;
      Panel_MPServerDetails: TKMPanel;
        Label_MP_Desc: TKMLabel;
        Label_MP_Players: TKMLabel;

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
      Panel_MPPassword: TKMPanel;
        Edit_MP_Password: TKMEdit;
        Button_MP_PasswordOk: TKMButton;
        Button_MP_PasswordCancel: TKMButton;

    Panel_CampSelect: TKMPanel;
      ColumnBox_Camps: TKMColumnBox;
      Image_CampsPreview: TKMImage;
      Button_Camp_Start, Button_Camp_Back: TKMButton;

    Panel_Load:TKMPanel;
      ColumnBox_Load: TKMColumnBox;
      Button_Load: TKMButton;
      Button_Delete: TKMButton;
      Label_DeleteConfirm: TKMLabel;
      Button_DeleteYes, Button_DeleteNo: TKMButton;
      Button_LoadBack:TKMButton;
      MinimapView_Load: TKMMinimapView;
    Panel_Replays:TKMPanel;
      Radio_Replays_Type:TKMRadioGroup;
      ColumnBox_Replays: TKMColumnBox;
      Button_ReplaysPlay: TKMButton;
      Button_ReplaysBack:TKMButton;
      MinimapView_Replay: TKMMinimapView;
    Panel_MapEd: TKMPanel;
      Panel_MapEdSizeXY: TKMPanel;
      Radio_MapEdSizeX, Radio_MapEdSizeY: TKMRadioGroup;
      Panel_MapEdLoad: TKMPanel;
      ColumnBox_MapEd: TKMColumnBox;
      Radio_MapEd_MapType: TKMRadioGroup;
      MinimapView_MapEd: TKMMinimapView;
      Button_MapEdBack,Button_MapEd_Create,Button_MapEd_Load: TKMButton;
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
        Chart_Army: TKMChart;
        Chart_Citizens: TKMChart;
        Chart_Houses: TKMChart;
        Chart_Wares: TKMChart;
      Button_ResultsBack,Button_ResultsRepeat,Button_ResultsContinue: TKMButton;
  public
    constructor Create(X,Y: Word);
    destructor Destroy; override;
    procedure ShowScreen(aScreen: TMenuScreen; const aText: string = ''; aMsg: TGameResultMsg=gr_Silent);
    procedure AppendLoadingText(const aText: string);
    procedure Results_Fill(aMsg: TGameResultMsg=gr_Silent);
    procedure ShowResultsMP(aMsg: TGameResultMsg);
    function GetChatText: string;
    function GetChatMessages: string;

    procedure KeyDown(Key:Word; Shift: TShiftState); override;
    procedure KeyUp(Key:Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;

    procedure Resize(X,Y: Word); override;
    procedure UpdateState(aTickCount: Cardinal); override;
  end;


implementation
uses KM_Main, KM_NetworkTypes, KM_TextLibrary, KM_Game, KM_GameApp, KM_PlayersCollection, KM_Locales,
  KM_Utils, KM_Log, KM_Sound, KM_Networking, KM_Resource, KM_Player, KM_CommonTypes, KM_RenderUI;

const
  MAPSIZES_COUNT = 15;
  MapSize: array [1..MAPSIZES_COUNT] of Word = (32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 240, 256);
  MAX_NAME_LENGTH = 16;


{ TKMMainMenuInterface }
constructor TKMMainMenuInterface.Create(X,Y: Word);
var S: TKMShape;
begin
  inherited;
  Assert(fTextLibrary <> nil, 'fTextLibrary should be initialized before MainMenuInterface');

  fMinimap := TKMMinimap.Create(True, False, True);

  fMaps := TKMapsCollection.Create(False);
  fMapsMP := TKMapsCollection.Create(True);
  fSaves := TKMSavesCollection.Create;

  Panel_Main := TKMPanel.Create(fMyControls, 0,
                                             0,
                                             MENU_DESIGN_X,
                                             MENU_DESIGN_Y); //Parent Panel for whole menu

  //Background is the same for all pages, except Results/Campaign, which will render ontop
  with TKMImage.Create(Panel_Main,-448,-216,960,600,17,rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Main, 512,-216,960,600,18,rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Main,-448, 384,960,600,19,rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Main, 512, 384,960,600,20,rxGuiMain) do Anchors := [];

  Create_MainMenu;
  Create_SinglePlayer;
    Create_CampSelect;
      fGuiCampaign := TKMGUIMainCampaign.Create(Panel_Main, PageChange);
  fSingleMap := TKMGUIMenuSingleMap.Create(Panel_Main, PageChange);
    Create_Load;
  Create_MultiPlayer;
  fLobby := TKMGUIMenuLobby.Create(Panel_Main, PageChange);
  Create_MapEditor;
  Create_Replays;
  fOptions := TKMGUIMainOptions.Create(Panel_Main, PageChange);
  Create_Credits;
  Create_Loading;
  Create_Error;
  Create_Results;
  fResultsMP := TKMGUIMenuResultsMP.Create(Panel_Main, PageChange);


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

  fLog.AddTime('Main menu init done');
//Use ShowScreen to select properscreen after fGame init is done
end;


destructor TKMMainMenuInterface.Destroy;
begin
  fMaps.Free;
  fMapsMP.Free;
  fSaves.Free;
  fMinimap.Free;

  fLobby.Free;
  fOptions.Free;
  fSingleMap.Free;
  fGuiCampaign.Free;
  fResultsMP.Free;

  inherited;
end;


//Keep Panel_Main centered
procedure TKMMainMenuInterface.Resize(X, Y: Word);
begin
  Panel_Main.Width  := Min(X, MENU_DESIGN_X);
  Panel_Main.Height := Min(Y, MENU_DESIGN_Y);

  Panel_Main.Left := (X - Panel_Main.Width) div 2;
  Panel_Main.Top  := (Y - Panel_Main.Height) div 2;

  //Needs to resize the map and move flag positions accordingly
  fGuiCampaign.Resize(X, Y);

  //Needs to swap map description / game settings on low resolution displays
  fLobby.Lobby_Resize(Panel_Main.Height);
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
    msResultsMP: PageChange(nil, gpMultiplayer, '');
  end;
end;


procedure TKMMainMenuInterface.AppendLoadingText(const aText: string);
begin
  Label_Loading.Caption := Label_Loading.Caption + aText + '|';
end;


function TKMMainMenuInterface.GetChatText: string;
begin
  Result := fLobby.GetChatText;
end;


//Access chat messages history to copy it over to gameplay chat
function TKMMainMenuInterface.GetChatMessages: string;
begin
  Result := fLobby.GetChatMessages;
end;


procedure TKMMainMenuInterface.Results_Fill(aMsg: TGameResultMsg=gr_Silent);
var
  TempGraphCount: Integer;
  TempGraphs:array[0..MAX_PLAYERS-1] of record
                                          Color: Cardinal;
                                          G: TKMCardinalArray;
                                        end;

  //Temp graphs are used to adjoin same colored AI opponents into one chart
  procedure AddToTempGraph(aColor:Cardinal; aGraph:TKMCardinalArray);
  var I, ID: Integer;
  begin
    ID := -1;
    for I := 0 to TempGraphCount - 1 do
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
  R: TWareType;
  G: TKMCardinalArray;
begin
  //Header
  fGameResultMsg := aMsg;
  case aMsg of
    gr_Win:       Label_Results.Caption := fTextLibrary[TX_MENU_MISSION_VICTORY];
    gr_Defeat:    Label_Results.Caption := fTextLibrary[TX_MENU_MISSION_DEFEAT];
    gr_Cancel:    Label_Results.Caption := fTextLibrary[TX_MENU_MISSION_CANCELED];
    gr_ReplayEnd: Label_Results.Caption := fTextLibrary[TX_MENU_REPLAY_ENDED];
    else          Label_Results.Caption := NO_TEXT;
  end;
  //Append mission name and time after the result message
  Label_Results.Caption := Label_Results.Caption + ' - ' + fGame.GameName; //Don't show the mission time in SP because it's already shown elsewhere

  //List values (like old KaM did)
  with fPlayers[MySpectator.PlayerIndex].Stats do
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

  //Chart values
  if DISPLAY_CHARTS_RESULT then
  begin
    Chart_Army.Clear;
    Chart_Citizens.Clear;
    Chart_Houses.Clear;
    Chart_Wares.Clear;
    Chart_Army.MaxLength      := fPlayers[MySpectator.PlayerIndex].Stats.ChartCount;
    Chart_Citizens.MaxLength  := fPlayers[MySpectator.PlayerIndex].Stats.ChartCount;
    Chart_Houses.MaxLength    := fPlayers[MySpectator.PlayerIndex].Stats.ChartCount;
    Chart_Wares.MaxLength     := fPlayers[MySpectator.PlayerIndex].Stats.ChartCount;

    Chart_Army.MaxTime      := fGame.GameTickCount div 10;
    Chart_Citizens.MaxTime  := fGame.GameTickCount div 10;
    Chart_Houses.MaxTime    := fGame.GameTickCount div 10;
    Chart_Wares.MaxTime     := fGame.GameTickCount div 10;

    //Army
    TempGraphCount := 0; //Reset
    for I := 0 to fPlayers.Count - 1 do
    with fPlayers[I] do
      if PlayerType = pt_Computer then
        AddToTempGraph(FlagColor, Stats.ChartArmy)
      else
        Chart_Army.AddLine(GetFormattedPlayerName, FlagColor, Stats.ChartArmy);

    for I := 0 to TempGraphCount - 1 do
      Chart_Army.AddLine(Format(fTextLibrary[TX_PLAYER_X], [I+1]), TempGraphs[I].Color, TempGraphs[I].G);

    //Citizens
    TempGraphCount := 0; //Reset
    for I := 0 to fPlayers.Count - 1 do
    with fPlayers[I] do
      if PlayerType = pt_Computer then
        AddToTempGraph(FlagColor, Stats.ChartCitizens)
      else
      begin
        Chart_Citizens.AddLine(GetFormattedPlayerName, FlagColor, Stats.ChartCitizens);
        //Recruits aren't that important, but if we want to include them they should be a separate graph
        //Chart_Citizens.AddAltLine(Stats.ChartRecruits);
      end;

    for I := 0 to TempGraphCount - 1 do
      Chart_Citizens.AddLine(Format(fTextLibrary[TX_PLAYER_X], [I+1]), TempGraphs[I].Color, TempGraphs[I].G);

    //Houses
    TempGraphCount := 0; //Reset
    for I := 0 to fPlayers.Count - 1 do
    with fPlayers[I] do
      if PlayerType = pt_Computer then
        AddToTempGraph(FlagColor, Stats.ChartHouses)
      else
        Chart_Houses.AddLine(GetFormattedPlayerName, FlagColor, Stats.ChartHouses);

    for I := 0 to TempGraphCount - 1 do
      Chart_Houses.AddLine(Format(fTextLibrary[TX_PLAYER_X], [I+1]), TempGraphs[I].Color, TempGraphs[I].G);

    //Wares
    for R := WARE_MIN to WARE_MAX do
    begin
      G := fPlayers[MySpectator.PlayerIndex].Stats.ChartWares[R];
      for I := 0 to High(G) do
        if G[I] <> 0 then
        begin
          Chart_Wares.AddLine(fResource.Wares[R].Title, ResourceColor[R] or $FF000000, G);
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
  Chart_Army.Visible := Sender = Button_ResultsArmy;
  Chart_Citizens.Visible := Sender = Button_ResultsCitizens;
  Chart_Houses.Visible := Sender = Button_ResultsHouses;
  Chart_Wares.Visible := Sender = Button_ResultsWares;

  Button_ResultsArmy.Down := Sender = Button_ResultsArmy;
  Button_ResultsCitizens.Down := Sender = Button_ResultsCitizens;
  Button_ResultsHouses.Down := Sender = Button_ResultsHouses;
  Button_ResultsWares.Down := Sender = Button_ResultsWares;
end;


procedure TKMMainMenuInterface.ShowResultsMP(aMsg: TGameResultMsg);
begin
  fResultsMP.Show(aMsg);
end;


procedure TKMMainMenuInterface.Create_MainMenu;
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
procedure TKMMainMenuInterface.Create_SinglePlayer;
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


procedure TKMMainMenuInterface.Create_MultiPlayer;
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
  procedure PasswordPopUp;
  begin
    Panel_MPPassword := TKMPanel.Create(Panel_Main, 362, 250, 320, 300);
    Panel_MPPassword.Anchors := [];
      TKMBevel.Create(Panel_MPPassword, -1000,  -1000, 4000, 4000);
      TKMImage.Create(Panel_MPPassword, -20, -75, 340, 310, 15, rxGuiMain);
      TKMBevel.Create(Panel_MPPassword,   0,  0, 320, 300);
      TKMLabel.Create(Panel_MPPassword,  20, 10, 280, 20, fTextLibrary[TX_MP_MENU_PASSWORD_HEADER], fnt_Outline, taCenter);

      TKMLabel.Create(Panel_MPPassword, 20, 50, 156, 20, fTextLibrary[TX_MP_MENU_PASSWORD], fnt_Outline, taLeft);
      Edit_MP_Password := TKMEdit.Create(Panel_MPPassword, 20, 70, 152, 20, fnt_Grey);
      Edit_MP_Password.AllowedChars := acText;
      Button_MP_PasswordOk := TKMButton.Create(Panel_MPPassword, 20, 110, 280, 30, fTextLibrary[TX_MP_MENU_SERVER_JOIN], bsMenu);
      Button_MP_PasswordOk.OnClick := MP_PasswordClick;
      Button_MP_PasswordCancel := TKMButton.Create(Panel_MPPassword, 20, 150, 280, 30, fTextLibrary[TX_MP_MENU_FIND_SERVER_CANCEL], bsMenu);
      Button_MP_PasswordCancel.OnClick := MP_PasswordClick;
  end;
begin
  Panel_MultiPlayer := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_MultiPlayer.Stretch;

    //Top area
    Panel_MPPlayerName := TKMPanel.Create(Panel_MultiPlayer, 675, 45, 320, 120);
      TKMBevel.Create(Panel_MPPlayerName, 0, 0, 320, 120);
      TKMLabel.Create(Panel_MPPlayerName, 8, 10, 304, 20, fTextLibrary[TX_MP_MENU_PLAYERNAME], fnt_Outline, taLeft);
      Edit_MP_PlayerName := TKMEdit.Create(Panel_MPPlayerName, 8, 30, 140, 20, fnt_Grey);
      Edit_MP_PlayerName.MaxLen := MAX_NAME_LENGTH;
      TKMLabel.Create(Panel_MPPlayerName, 8, 60, 304, 20, fTextLibrary[TX_MP_MENU_STATUS], fnt_Outline, taLeft);
      Label_MP_Status := TKMLabel.Create(Panel_MPPlayerName, 8, 80, 304, 36, '', fnt_Grey, taLeft);
      Label_MP_Status.AutoWrap := True;

    Button_MP_CreateServer := TKMButton.Create(Panel_MultiPlayer, 675, 170, 320, 30, fTextLibrary[TX_MP_MENU_CREATE_SERVER], bsMenu);
    Button_MP_CreateServer.OnClick := MP_CreateServerClick;

    CreateServerPopUp;

    Button_MP_FindServer := TKMButton.Create(Panel_MultiPlayer, 675, 204, 320, 30, fTextLibrary[TX_MP_MENU_FIND_SERVER], bsMenu);
    Button_MP_FindServer.OnClick := MP_FindServerClick;

    FindServerPopUp;

    PasswordPopUp;

    //Master server announcement
    Memo_MP_Announcement := TKMMemo.Create(Panel_MultiPlayer, 45, 45, 620, 189, fnt_Grey, bsMenu);
    Memo_MP_Announcement.Anchors := [akLeft, akTop];
    Memo_MP_Announcement.AutoWrap := True;
    Memo_MP_Announcement.ItemHeight := 16;

    //List of available servers
    ColumnBox_Servers := TKMColumnBox.Create(Panel_MultiPlayer,45,240,620,465,fnt_Metal, bsMenu);
    ColumnBox_Servers.Anchors := [akLeft, akTop, akBottom];
    ColumnBox_Servers.SetColumns(fnt_Outline, ['','',fTextLibrary[TX_MP_MENU_SERVERLIST_NAME],fTextLibrary[TX_MP_MENU_SERVERLIST_STATE],fTextLibrary[TX_MP_MENU_SERVERLIST_PLAYERS],fTextLibrary[TX_MP_MENU_SERVERLIST_PING]],[0,20,40,300,430,525]);
    ColumnBox_Servers.OnColumnClick := MP_ServersSort;
    ColumnBox_Servers.OnChange := MP_ServersClick;
    ColumnBox_Servers.OnDoubleClick := MP_ServersDoubleClick;
    Label_Servers_Status := TKMLabel.Create(Panel_MultiPlayer, 45+310, 240+230, '', fnt_Grey, taCenter);
    Label_Servers_Status.Anchors := [akLeft];
    Label_Servers_Status.Hide;

    //Server details area
    Panel_MPServerDetails := TKMPanel.Create(Panel_MultiPlayer, 675, 240, 320, 465);
    Panel_MPServerDetails.Anchors := [akLeft, akTop, akBottom];
      with TKMBevel.Create(Panel_MPServerDetails, 0, 0, 320, 465) do Stretch;
      TKMLabel.Create(Panel_MPServerDetails, 8, 6, 304, 20, fTextLibrary[TX_MP_MENU_HEADER_SERVER_DETAILS], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_MPServerDetails, 8, 30, 304, 20, fTextLibrary[TX_MP_MENU_GAME_INFORMATION], fnt_Outline, taLeft);
      Label_MP_Desc := TKMLabel.Create(Panel_MPServerDetails, 8, 50, 304, 80, '', fnt_Metal, taLeft);
      TKMLabel.Create(Panel_MPServerDetails, 8, 110, 304, 20, fTextLibrary[TX_MP_MENU_PLAYER_LIST], fnt_Outline, taLeft);
      Label_MP_Players := TKMLabel.Create(Panel_MPServerDetails, 8, 130, 304, 340, '', fnt_Metal, taLeft);
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


procedure TKMMainMenuInterface.Create_CampSelect;
var L: TKMLabel;
begin
  Panel_CampSelect := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_CampSelect.Stretch;

    L := TKMLabel.Create(Panel_CampSelect, Panel_Main.Width div 2, 230, fTextLibrary[TX_MENU_CAMP_HEADER], fnt_Outline, taCenter);
    L.Anchors := [];
    ColumnBox_Camps := TKMColumnBox.Create(Panel_CampSelect, 80, 260, 600, 300, fnt_Grey, bsMenu);
    ColumnBox_Camps.SetColumns(fnt_Outline, [fTextLibrary[TX_MENU_CAMPAIGNS_TITLE],
                                        fTextLibrary[TX_MENU_CAMPAIGNS_MAPS_COUNT],
                                        fTextLibrary[TX_MENU_CAMPAIGNS_MAPS_UNLOCKED], ''],
                                        [0, 320, 460, 600]);
    ColumnBox_Camps.Anchors := [];
    ColumnBox_Camps.Header.Anchors := [];
    ColumnBox_Camps.SearchColumn := 0;
    ColumnBox_Camps.OnChange := Campaign_ListChange;
    ColumnBox_Camps.OnDoubleClick := SwitchMenuPage;

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


procedure TKMMainMenuInterface.Create_Load;
begin
  Panel_Load := TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_Load.Stretch;

    TKMLabel.Create(Panel_Load, Panel_Main.Width div 2, 50, fTextLibrary[TX_MENU_LOAD_LIST], fnt_Outline, taCenter);

    ColumnBox_Load := TKMColumnBox.Create(Panel_Load, 62, 86, 700, 485, fnt_Metal, bsMenu);
    ColumnBox_Load.Anchors := [akLeft,akTop,akBottom];
    ColumnBox_Load.SetColumns(fnt_Outline, [fTextLibrary[TX_MENU_LOAD_FILE], fTextLibrary[TX_MENU_LOAD_DESCRIPTION]], [0, 300]);
    ColumnBox_Load.SearchColumn := 0;
    ColumnBox_Load.OnColumnClick := Load_Sort;
    ColumnBox_Load.OnChange := Load_ListClick;
    ColumnBox_Load.OnDoubleClick := Load_Click;

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
procedure TKMMainMenuInterface.Create_MapEditor;
var I: Integer;
begin
  Panel_MapEd:=TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_MapEd.Stretch;
    Panel_MapEdSizeXY := TKMPanel.Create(Panel_MapEd, 80, 160, 200, 400);
    Panel_MapEdSizeXY.Anchors := [akLeft];
      TKMLabel.Create(Panel_MapEdSizeXY, 6, 0, 188, 20, fTextLibrary[TX_MENU_NEW_MAP_SIZE], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_MapEdSizeXY, 0, 20, 200, 370);
      TKMLabel.Create(Panel_MapEdSizeXY, 8, 27, 88, 20, fTextLibrary[TX_MENU_MAP_WIDTH], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_MapEdSizeXY, 108, 27, 88, 20, fTextLibrary[TX_MENU_MAP_HEIGHT], fnt_Outline, taLeft);

      Radio_MapEdSizeX := TKMRadioGroup.Create(Panel_MapEdSizeXY, 10, 52, 88, 332, fnt_Metal);
      Radio_MapEdSizeY := TKMRadioGroup.Create(Panel_MapEdSizeXY, 110, 52, 88, 332, fnt_Metal);
      for I := 1 to MAPSIZES_COUNT do begin
        Radio_MapEdSizeX.Add(IntToStr(MapSize[I]));
        Radio_MapEdSizeY.Add(IntToStr(MapSize[I]));
      end;
      Radio_MapEdSizeX.ItemIndex := 2; //64
      Radio_MapEdSizeY.ItemIndex := 2; //64

      Button_MapEd_Create := TKMButton.Create(Panel_MapEdSizeXY, 0, 400, 200, 30, fTextLibrary[TX_MENU_MAP_CREATE_NEW_MAP], bsMenu);
      Button_MapEd_Create.OnClick := MapEditor_Start;

    Panel_MapEdLoad := TKMPanel.Create(Panel_MapEd, 300, 160, 620, 500);
    Panel_MapEdLoad.Anchors := [akLeft];
      TKMLabel.Create(Panel_MapEdLoad, 6, 0, 288, 20, fTextLibrary[TX_MENU_MAP_AVAILABLE], fnt_Outline, taLeft);
      TKMBevel.Create(Panel_MapEdLoad, 0, 20, 300, 50);
      Radio_MapEd_MapType := TKMRadioGroup.Create(Panel_MapEdLoad,8,28,286,40,fnt_Grey);
      Radio_MapEd_MapType.ItemIndex := 0;
      Radio_MapEd_MapType.Add(fTextLibrary[TX_MENU_MAPED_SPMAPS]);
      Radio_MapEd_MapType.Add(fTextLibrary[TX_MENU_MAPED_MPMAPS]);
      Radio_MapEd_MapType.OnChange := MapEditor_MapTypeChange;
      ColumnBox_MapEd := TKMColumnBox.Create(Panel_MapEdLoad, 0, 80, 440, 310, fnt_Metal,  bsMenu);
      ColumnBox_MapEd.SetColumns(fnt_Outline, [fTextLibrary[TX_MENU_MAP_TITLE], '#', fTextLibrary[TX_MENU_MAP_SIZE]], [0, 310, 340]);
      ColumnBox_MapEd.SearchColumn := 0;
      ColumnBox_MapEd.OnColumnClick := MapEditor_ColumnClick;
      ColumnBox_MapEd.OnChange := MapEditor_SelectMap;
      ColumnBox_MapEd.OnDoubleClick := MapEditor_Start;
      Button_MapEd_Load := TKMButton.Create(Panel_MapEdLoad, 0, 400, 300, 30, fTextLibrary[TX_MENU_MAP_LOAD_EXISTING], bsMenu);
      Button_MapEd_Load.OnClick := MapEditor_Start;
      TKMBevel.Create(Panel_MapEdLoad, 448, 80, 199, 199);
      MinimapView_MapEd := TKMMinimapView.Create(Panel_MapEdLoad, 452, 84, 191, 191);

    Button_MapEdBack := TKMButton.Create(Panel_MapEd, 80, 620, 220, 30, fTextLibrary[TX_MENU_BACK], bsMenu);
    Button_MapEdBack.Anchors := [akLeft];
    Button_MapEdBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Replays;
begin
  Panel_Replays := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_Replays.Stretch;

    TKMLabel.Create(Panel_Replays, Panel_Main.Width div 2, 50, fTextLibrary[TX_MENU_LOAD_LIST], fnt_Outline, taCenter);

    TKMBevel.Create(Panel_Replays, 62, 86, 900, 50);
    Radio_Replays_Type := TKMRadioGroup.Create(Panel_Replays,70,94,300,40,fnt_Grey);
    Radio_Replays_Type.ItemIndex := 0;
    Radio_Replays_Type.Add(fTextLibrary[TX_MENU_MAPED_SPMAPS]);
    Radio_Replays_Type.Add(fTextLibrary[TX_MENU_MAPED_MPMAPS]);
    Radio_Replays_Type.OnChange := Replay_TypeChange;

    ColumnBox_Replays := TKMColumnBox.Create(Panel_Replays, 62, 150, 700, 485, fnt_Metal, bsMenu);
    ColumnBox_Replays.SetColumns(fnt_Outline, [fTextLibrary[TX_MENU_LOAD_FILE], fTextLibrary[TX_MENU_LOAD_DESCRIPTION]], [0, 300]);
    ColumnBox_Replays.Anchors := [akLeft,akTop,akBottom];
    ColumnBox_Replays.SearchColumn := 0;
    ColumnBox_Replays.OnChange := Replays_ListClick;
    ColumnBox_Replays.OnColumnClick := Replays_Sort;
    ColumnBox_Replays.OnDoubleClick := Replays_Play;

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


procedure TKMMainMenuInterface.Create_Credits;
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


procedure TKMMainMenuInterface.Create_Loading;
begin
  Panel_Loading:=TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_Loading.Stretch;
    with TKMLabel.Create(Panel_Loading, Panel_Main.Width div 2, Panel_Main.Height div 2 - 20, fTextLibrary[TX_MENU_LOADING], fnt_Outline, taCenter) do
      Center;
    Label_Loading := TKMLabel.Create(Panel_Loading, Panel_Main.Width div 2, Panel_Main.Height div 2+10, '...', fnt_Grey, taCenter);
    Label_Loading.Center;
end;


procedure TKMMainMenuInterface.Create_Error;
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


procedure TKMMainMenuInterface.Create_Results;
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
    with TKMShape.Create(Panel_Results,0,0,Panel_Main.Width, Panel_Main.Height) do
    begin
      Center;
      FillColor := $A0000000;
    end;

    Label_Results := TKMLabel.Create(Panel_Results,62,140,900,20,NO_TEXT,fnt_Metal,taCenter);
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

      Chart_Army := TKMChart.Create(Panel_StatsCharts, 0, 46, 610, 374);
      Chart_Army.Caption := fTextLibrary[TX_GRAPH_ARMY];
      Chart_Army.Anchors := [akLeft];

      Chart_Citizens := TKMChart.Create(Panel_StatsCharts, 0, 46, 610, 374);
      Chart_Citizens.Caption := fTextLibrary[TX_GRAPH_CITIZENS];
      Chart_Citizens.Anchors := [akLeft];

      Chart_Houses := TKMChart.Create(Panel_StatsCharts, 0, 46, 610, 374);
      Chart_Houses.Caption := fTextLibrary[TX_GRAPH_HOUSES];
      Chart_Houses.Anchors := [akLeft];

      Chart_Wares := TKMChart.Create(Panel_StatsCharts, 0, 46, 610, 374);
      Chart_Wares.Caption := fTextLibrary[TX_GRAPH_TITLE_RESOURCES];
      Chart_Wares.Anchors := [akLeft];
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


procedure TKMMainMenuInterface.PageChange(Sender: TObject; Dest: TGUIPage; aText: string);
var
  I: Integer;
begin
  //Hide all other pages
  for I := 1 to Panel_Main.ChildCount do
    if Panel_Main.Childs[I] is TKMPanel then
      Panel_Main.Childs[I].Hide;

  case Dest of
    gpMainMenu:     Panel_MainMenu.Show;
    gpSingleplayer: Panel_SinglePlayer.Show;
    gpMultiplayer:  begin
                      fGameApp.NetworkInit;
                      MP_Init(Sender);

                      if aText = '' then
                        //Entering MP anew
                        MP_Update(fTextLibrary[TX_MP_MENU_STATUS_READY],icGreen,false)
                      else
                        //We are in event handler of Lobby.BackClick (show status warning)
                        MP_Update(aText, icYellow, False);

                      Panel_MultiPlayer.Show;
                    end;
    gpOptions:      ;
    gpReplays:      begin
                      //Copy/Pasted from SwitchPage for now (needed that for ResultsMP BackClick)
                      //Probably needs some cleanup when we have GUIMenuReplays
                      fLastSaveCRC := 0;
                      Radio_Replays_Type.ItemIndex := 0; //we always show SP replays on start
                      Replay_TypeChange(nil); //Select SP as this will refresh everything
                      Replays_Sort(ColumnBox_Replays.SortIndex); //Apply sorting from last time we were on this page
                      Panel_Replays.Show;
                    end;
    gmCampSelect:   begin
                      Campaign_FillList;
                      Panel_CampSelect.Show;
                    end;
  end;

end;


procedure TKMMainMenuInterface.SwitchMenuPage(Sender: TObject);
var
  I: Integer;
  CmpName: string;
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
  or ((Sender = Button_ResultsBack)and(fGameResultMsg = gr_ReplayEnd)) then
  begin
    //Scan should be terminated, it is no longer needed
    if Sender = Button_ReplaysBack then
      fSaves.TerminateScan;
    if (Sender = Button_MP_Back) or (Sender = Button_ErrorBack) then
      fMain.UnlockMutex; //Leaving MP areas
    Panel_MainMenu.Show;
  end;

  {Show SinglePlayer menu}
  {Return to SinglePlayerMenu}
  if (Sender = Button_MM_SinglePlayer)
  or (Sender = Button_Camp_Back)
  or (Sender = Button_LoadBack)
  or ((Sender = Button_ResultsBack)and(fGameResultMsg <> gr_ReplayEnd)) then
  begin
    if (Sender = Button_LoadBack) then
      //scan should be terminated, it is no longer needed
      fSaves.TerminateScan;
    Panel_SinglePlayer.Show;
  end;

  {Show campaign selection menu}
  if (Sender = Button_SP_Camp) then
  begin
    Campaign_FillList;
    Panel_CampSelect.Show;
  end;

  {Show campaign screen}
  if (Sender = Button_Camp_Start) or (Sender = ColumnBox_Camps) then
  begin
    CmpName := ColumnBox_Camps.Rows[ColumnBox_Camps.ItemIndex].Cells[3].Caption;
    fGuiCampaign.Show(fGameApp.Campaigns.CampaignByTitle(CmpName));
  end;

  if (Sender = Button_ResultsContinue) then
    fGuiCampaign.Show(fGameApp.Campaigns.ActiveCampaign);

  {Show SingleMap menu}
  if Sender = Button_SP_Single then
    fSingleMap.Show;

  {Show Load menu}
  if Sender=Button_SP_Load then
  begin
    //Stop current scan so it can't add a save after we clear the list
    fSaves.TerminateScan;
    fLastSaveCRC := 0;
    ColumnBox_Load.Clear; //clear the list
    Load_DeleteConfirmation(False);
    Load_ListClick(nil);
    //Initiate refresh and process each new save added
    fSaves.Refresh(Load_ScanUpdate, False);
    Load_Sort(ColumnBox_Load.SortIndex); //Apply sorting from last time we were on this page
    Panel_Load.Show;
  end;

  {Show replays menu}
  if Sender=Button_MM_Replays then begin
    fLastSaveCRC := 0;
    Radio_Replays_Type.ItemIndex := 0; //we always show SP replays on start
    Replay_TypeChange(nil); //Select SP as this will refresh everything
    Replays_Sort(ColumnBox_Replays.SortIndex); //Apply sorting from last time we were on this page
    Panel_Replays.Show;
  end;

  {Show MultiPlayer menu}
  if (Sender=Button_MM_MultiPlayer) then
  begin
    fGameApp.NetworkInit;
    MP_Init(Sender);
    MP_Update(fTextLibrary[TX_MP_MENU_STATUS_READY],icGreen,false);
    Panel_MultiPlayer.Show;
  end;

  { Lobby }
  if (Sender = Button_MP_FindServerIP) or (Sender = Button_MP_GetIn) then
    fLobby.Show(lpk_Joiner, fGameApp.Networking, Panel_Main.Height);

  if (Sender = Button_MP_CreateLAN) or (Sender = Button_MP_CreateWAN) then
    fLobby.Show(lpk_Host, fGameApp.Networking, Panel_Main.Height);

  {Show MapEditor menu}
  if Sender=Button_MM_MapEd then begin
    MapEditor_ListUpdate;
    Panel_MapEd.Show;
  end;

  {Show Options menu}
  if Sender = Button_MM_Options then
    fOptions.Show;

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
  fGameApp.NewSingleMap(ExeDir + 'Tutorials'+PathDelim+'Town Tutorial'+PathDelim+'Town Tutorial.dat', fTextLibrary[TX_MENU_TUTORIAL_TOWN]);
end;


procedure TKMMainMenuInterface.MainMenu_PlayBattle(Sender: TObject);
begin
  fGameApp.NewSingleMap(ExeDir + 'Tutorials'+PathDelim+'Battle Tutorial'+PathDelim+'Battle Tutorial.dat', fTextLibrary[TX_MENU_TUTORIAL_BATTLE]);
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
  ColumnBox_Camps.Clear;
  for I := 0 to Camps.Count - 1 do
  with Camps[I] do
    ColumnBox_Camps.AddItem(MakeListRow(
                        [CampaignTitle, IntToStr(MapCount), IntToStr(UnlockedMap+1), ShortTitle],
                        [$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $00FFFFFF]));

  Button_Camp_Start.Disable;
end;


procedure TKMMainMenuInterface.Campaign_ListChange(Sender: TObject);
var Camp: TKMCampaign;
begin
  Button_Camp_Start.Enable;
  Camp := fGameApp.Campaigns.CampaignByTitle(ColumnBox_Camps.Rows[ColumnBox_Camps.ItemIndex].Cells[3].Caption);

  Image_CampsPreview.RX := Camp.BackGroundPic.RX;
  Image_CampsPreview.TexID := Camp.BackGroundPic.ID;
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


procedure TKMMainMenuInterface.MP_PasswordClick(Sender: TObject);
begin
  if Sender = Button_MP_PasswordOk then
  begin
    Panel_MPPassword.Hide;
    fGameApp.Networking.SendPassword(Edit_MP_Password.Text);
  end;
  if Sender = Button_MP_PasswordCancel then
  begin
    fGameApp.Networking.Disconnect;
    Panel_MPPassword.Hide;
    MP_Update(fTextLibrary[TX_MP_MENU_STATUS_READY], icGreen, False);
  end;
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
  ColumnBox_Servers.Clear;
  Label_MP_Desc.Caption := '';
  Label_MP_Players.Caption := '';

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
  OldTopIndex := ColumnBox_Servers.TopIndex;
  ColumnBox_Servers.Clear;
  ColumnBox_Servers.ItemIndex := -1;

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
      ColumnBox_Servers.AddItem(
      MakeListRow(['', '', DisplayName, fTextLibrary[GameStateTextIDs[R.GameInfo.GameState]], IntToStr(R.GameInfo.PlayerCount), IntToStr(S.Ping)],
                  [$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, GetPingColor(S.Ping)],
                  [ServerTypePic[S.ServerType],MakePic(rxGuiMain,IfThen(R.GameInfo.PasswordLocked, 73, 0)),MakePic(rxGuiMain,0),MakePic(rxGuiMain,0),MakePic(rxGuiMain,0),MakePic(rxGuiMain,0)], I));

      //if server was selected, we need to select it again, because TKMColumnListBox was cleared
      if fServerSelected
      and (R.RoomID = fSelectedRoomInfo.RoomID)
      and (S.IP = fSelectedServerInfo.IP)
      and (S.Port = fSelectedServerInfo.Port) then
      begin
        ColumnBox_Servers.ItemIndex := I;
        MP_ServersClick(nil); //Shows info about this selected server
      end;
    end;

    ColumnBox_Servers.TopIndex := OldTopIndex;
    if (ColumnBox_Servers.ItemIndex <> -1)
    and not InRange(ColumnBox_Servers.ItemIndex - ColumnBox_Servers.TopIndex, 0, ColumnBox_Servers.GetVisibleRows - 1) then
    begin
      if ColumnBox_Servers.ItemIndex < ColumnBox_Servers.TopIndex + ColumnBox_Servers.GetVisibleRows - 1 then
        ColumnBox_Servers.TopIndex := ColumnBox_Servers.ItemIndex
      else
      if ColumnBox_Servers.ItemIndex > ColumnBox_Servers.TopIndex + ColumnBox_Servers.GetVisibleRows - 1 then
        ColumnBox_Servers.TopIndex := ColumnBox_Servers.ItemIndex - ColumnBox_Servers.GetVisibleRows + 1;
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
  case ColumnBox_Servers.SortIndex of
    0:  if ColumnBox_Servers.SortDirection = sdDown then
          fGameApp.Networking.ServerQuery.SortMethod := ssmByTypeAsc
        else
          fGameApp.Networking.ServerQuery.SortMethod := ssmByTypeDesc;
    1:  if ColumnBox_Servers.SortDirection = sdDown then
          fGameApp.Networking.ServerQuery.SortMethod := ssmByPasswordAsc
        else
          fGameApp.Networking.ServerQuery.SortMethod := ssmByPasswordDesc;
    //Sorting by name goes A..Z by default
    2:  if ColumnBox_Servers.SortDirection = sdDown then
          fGameApp.Networking.ServerQuery.SortMethod := ssmByNameAsc
        else
          fGameApp.Networking.ServerQuery.SortMethod := ssmByNameDesc;
    //Sorting by state goes Lobby,Loading,Game,None by default
    3:  if ColumnBox_Servers.SortDirection = sdDown then
          fGameApp.Networking.ServerQuery.SortMethod := ssmByStateAsc
        else
          fGameApp.Networking.ServerQuery.SortMethod := ssmByStateDesc;
    //Sorting by player count goes 8..0 by default
    4:  if ColumnBox_Servers.SortDirection = sdDown then
          fGameApp.Networking.ServerQuery.SortMethod := ssmByPlayersDesc
        else
          fGameApp.Networking.ServerQuery.SortMethod := ssmByPlayersAsc;
    //Sorting by ping goes 0 ... 1000 by default
    5:  if ColumnBox_Servers.SortDirection = sdDown then
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
  ID := ColumnBox_Servers.ItemIndex;
  if (ID = -1) or (ColumnBox_Servers.Rows[ID].Tag = -1) then
  begin
    fServerSelected := False;
    Button_MP_GetIn.Disable;
    Label_MP_Desc.Caption := '';
    Label_MP_Players.Caption := '';
    Exit;
  end;

  fServerSelected := True;
  Button_MP_GetIn.Enabled := MP_GetInEnabled;

  fSelectedRoomInfo := fGameApp.Networking.ServerQuery.Rooms[ColumnBox_Servers.Rows[ID].Tag];
  fSelectedServerInfo := fGameApp.Networking.ServerQuery.Servers[fSelectedRoomInfo.ServerIndex];

  Label_MP_Desc.Caption := '';
  //Description might be blank, so don't output a blank line in that case
  if fSelectedRoomInfo.GameInfo.Description <> '' then
    Label_MP_Desc.Caption := Label_MP_Desc.Caption + fSelectedRoomInfo.GameInfo.Description + '|';
  //Append map and time on lines below the description
  Label_MP_Desc.Caption := Label_MP_Desc.Caption + fSelectedRoomInfo.GameInfo.Map + '|';
  Label_MP_Desc.Caption := Label_MP_Desc.Caption + fSelectedRoomInfo.GameInfo.GetFormattedTime;
  Label_MP_Players.Caption := fSelectedRoomInfo.GameInfo.Players;
end;


procedure TKMMainMenuInterface.MP_ServersDoubleClick(Sender: TObject);
begin
  //MP_SelectServer gets called by first Click
  if Button_MP_GetIn.Enabled and (ColumnBox_Servers.ItemIndex <> -1)
  and InRange(ColumnBox_Servers.Rows[ColumnBox_Servers.ItemIndex].Tag, 0, fGameApp.Networking.ServerQuery.Rooms.Count-1) then
    MP_GetInClick(Sender);
end;


procedure TKMMainMenuInterface.MP_HostClick(Sender: TObject);
begin
  //Save the player and IP name so it is not lost if something fails
  MP_SaveSettings;

  Panel_MPCreateServer.Hide; //Hide the panel so if it fails the error message will be easy to see (e.g. name too long)
  if not MP_ValidatePlayerName(Edit_MP_PlayerName.Text) then Exit;

  SwitchMenuPage(Sender); //Open lobby page

  fGameApp.Networking.OnHostFail := MP_HostFail;
  fGameApp.Networking.Host(Edit_MP_PlayerName.Text, Edit_MP_ServerName.Text, Edit_MP_ServerPort.Text, (Sender = Button_MP_CreateWAN));
end;


procedure TKMMainMenuInterface.MP_GetInClick(Sender: TObject);
begin
  MP_Join(fSelectedServerInfo.IP, fSelectedServerInfo.Port, fSelectedRoomInfo.RoomID);
end;


function TKMMainMenuInterface.MP_ValidatePlayerName(const aName: string): Boolean;
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
function TKMMainMenuInterface.MP_GetInEnabled: Boolean;
var ID: Integer;
begin
  ID := ColumnBox_Servers.ItemIndex;
  Result := (not fLobbyBusy) and (ID <> -1) and (ColumnBox_Servers.Rows[ID].Tag <> -1);
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
  fGameApp.Networking.OnJoinPassword := MP_JoinPassword;
  fGameApp.Networking.OnJoinAssignedHost := MP_JoinAssignedHost;
  fGameApp.Networking.Join(aServerAddress, aPort, Edit_MP_PlayerName.Text, aRoom); //Init lobby
end;


procedure TKMMainMenuInterface.MP_JoinPassword(Sender: TObject);
begin
  Panel_MPFindServer.Hide;
  Edit_MP_Password.Text := '';
  Panel_MPPassword.Show;
end;


//We had recieved permission to join
procedure TKMMainMenuInterface.MP_JoinSuccess(Sender: TObject);
begin
  fGameApp.Networking.OnJoinSucc := nil;
  fGameApp.Networking.OnJoinFail := nil;
  fGameApp.Networking.OnJoinAssignedHost := nil;

  SwitchMenuPage(Button_MP_GetIn); //Open lobby page
end;


procedure TKMMainMenuInterface.MP_JoinFail(const aData: string);
begin
  fGameApp.Networking.Disconnect;
  MP_Update(Format(fTextLibrary[TX_GAME_ERROR_CONNECTION_FAILED],[aData]),icYellow,false);
  fSoundLib.Play(sfxn_Error);
end;


procedure TKMMainMenuInterface.MP_JoinAssignedHost(Sender: TObject);
begin
  fGameApp.Networking.OnJoinSucc := nil;
  fGameApp.Networking.OnJoinFail := nil;
  fGameApp.Networking.OnJoinAssignedHost := nil;
  fGameApp.Networking.OnHostFail := MP_HostFail;

  //We were joining a game and the server assigned hosting rights to us
  SwitchMenuPage(Button_MP_CreateLAN); //Open lobby page in host mode
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
  fSoundLib.Play(sfxn_Error);

  PageChange(Self, gpMultiplayer, aData);
end;


procedure TKMMainMenuInterface.Load_ListClick(Sender: TObject);
begin
  fSaves.Lock;
    //Hide delete confirmation if player has selected a different savegame item
    if Sender = ColumnBox_Load then
      Load_DeleteConfirmation(False);

    Button_Delete.Enabled := InRange(ColumnBox_Load.ItemIndex, 0, fSaves.Count-1);
    Button_Load.Enabled := InRange(ColumnBox_Load.ItemIndex, 0, fSaves.Count-1)
                           and fSaves[ColumnBox_Load.ItemIndex].IsValid;

    if InRange(ColumnBox_Load.ItemIndex, 0, fSaves.Count-1) then
      fLastSaveCRC := fSaves[ColumnBox_Load.ItemIndex].CRC
    else
      fLastSaveCRC := 0;

    MinimapView_Load.Hide; //Hide by default, then show it if we load the map successfully
    if Button_Load.Enabled and fSaves[ColumnBox_Load.ItemIndex].LoadMinimap(fMinimap) then
    begin
      MinimapView_Load.SetMinimap(fMinimap);
      MinimapView_Load.Show;
    end;
  fSaves.Unlock;
end;


procedure TKMMainMenuInterface.Load_Click(Sender: TObject);
begin
  if not Button_Load.Enabled then exit; //This is also called by double clicking
  if not InRange(ColumnBox_Load.ItemIndex, 0, fSaves.Count-1) then Exit;
  fSaves.TerminateScan; //stop scan as it is no longer needed
  fGameApp.NewSingleSave(fSaves[ColumnBox_Load.ItemIndex].FileName);
end;


procedure TKMMainMenuInterface.Load_Delete_Click(Sender: TObject);
var
  PreviouslySelected: Integer;
begin
  if ColumnBox_Load.ItemIndex = -1 then Exit;

  if Sender = Button_Delete then
    Load_DeleteConfirmation(true);

  if (Sender = Button_DeleteYes) or (Sender = Button_DeleteNo) then
    Load_DeleteConfirmation(false); //Hide confirmation anyways

  //Delete the savegame
  if Sender = Button_DeleteYes then
  begin
    PreviouslySelected := ColumnBox_Load.ItemIndex;
    fSaves.DeleteSave(ColumnBox_Load.ItemIndex);
    Load_RefreshList(False);
    if ColumnBox_Load.RowCount > 0 then
      ColumnBox_Load.ItemIndex := EnsureRange(PreviouslySelected, 0, ColumnBox_Load.RowCount - 1)
    else
      ColumnBox_Load.ItemIndex := -1; //there are no saves, nothing to select
    Load_ListClick(ColumnBox_Load);
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
  fSaves.Lock;
    OldTopIndex := ColumnBox_Load.TopIndex;
    ColumnBox_Load.Clear;

    for I := 0 to fSaves.Count - 1 do
      ColumnBox_Load.AddItem(MakeListRow(
                        [fSaves[i].FileName, fSaves[I].Info.GetTitleWithTime],
                        [$FFFFFFFF, $FFFFFFFF]));

    //IDs of saves could changed, so use CRC to check which one was selected
    for I := 0 to fSaves.Count - 1 do
      if (fSaves[I].CRC = fLastSaveCRC) then
        ColumnBox_Load.ItemIndex := I;

    ColumnBox_Load.TopIndex := OldTopIndex;

    if aJumpToSelected and (ColumnBox_Load.ItemIndex <> -1)
    and not InRange(ColumnBox_Load.ItemIndex - ColumnBox_Load.TopIndex, 0, ColumnBox_Load.GetVisibleRows - 1)
    then
    begin
      if ColumnBox_Load.ItemIndex < ColumnBox_Load.TopIndex + ColumnBox_Load.GetVisibleRows - 1 then
        ColumnBox_Load.TopIndex := ColumnBox_Load.ItemIndex
      else
        if ColumnBox_Load.ItemIndex > ColumnBox_Load.TopIndex + ColumnBox_Load.GetVisibleRows - 1 then
          ColumnBox_Load.TopIndex := ColumnBox_Load.ItemIndex - ColumnBox_Load.GetVisibleRows + 1;
    end;

    Load_ListClick(nil);
  fSaves.Unlock;
end;


procedure TKMMainMenuInterface.Load_Sort(aIndex: Integer);
begin
  case ColumnBox_Load.SortIndex of
    //Sorting by filename goes A..Z by default
    0:  if ColumnBox_Load.SortDirection = sdDown then
          fSaves.Sort(smByFileNameDesc, Load_SortUpdate)
        else
          fSaves.Sort(smByFileNameAsc, Load_SortUpdate);
    //Sorting by description goes A..Z by default
    1:  if ColumnBox_Load.SortDirection = sdDown then
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
  fSaves.Lock;
    ID := ColumnBox_Replays.ItemIndex;

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
  fSaves.Unlock;
end;


procedure TKMMainMenuInterface.Replay_TypeChange(Sender: TObject);
begin
  fSaves.TerminateScan;
  fLastSaveCRC := 0;
  ColumnBox_Replays.Clear;
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
  fSaves.Lock;
    OldTopIndex := ColumnBox_Replays.TopIndex;
    ColumnBox_Replays.Clear;

    for I := 0 to fSaves.Count - 1 do
      ColumnBox_Replays.AddItem(MakeListRow(
                           [fSaves[I].FileName, fSaves[I].Info.GetTitleWithTime],
                           [$FFFFFFFF, $FFFFFFFF]));

    for I := 0 to fSaves.Count - 1 do
      if (fSaves[I].CRC = fLastSaveCRC) then
        ColumnBox_Replays.ItemIndex := I;

    ColumnBox_Replays.TopIndex := OldTopIndex;

    if aJumpToSelected and (ColumnBox_Replays.ItemIndex <> -1)
    and not InRange(ColumnBox_Replays.ItemIndex - ColumnBox_Replays.TopIndex, 0, ColumnBox_Replays.GetVisibleRows-1)
    then
      if ColumnBox_Replays.ItemIndex < ColumnBox_Replays.TopIndex then
        ColumnBox_Replays.TopIndex := ColumnBox_Replays.ItemIndex
      else
      if ColumnBox_Replays.ItemIndex > ColumnBox_Replays.TopIndex + ColumnBox_Replays.GetVisibleRows - 1 then
        ColumnBox_Replays.TopIndex := ColumnBox_Replays.ItemIndex - ColumnBox_Replays.GetVisibleRows + 1;
  fSaves.Unlock;
end;


procedure TKMMainMenuInterface.Replays_Sort(aIndex: Integer);
begin
  case ColumnBox_Replays.SortIndex of
    //Sorting by filename goes A..Z by default
    0:  if ColumnBox_Replays.SortDirection = sdDown then
          fSaves.Sort(smByFileNameDesc, Replays_SortUpdate)
        else
          fSaves.Sort(smByFileNameAsc, Replays_SortUpdate);
    //Sorting by description goes A..Z by default
    1:  if ColumnBox_Replays.SortDirection = sdDown then
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

  ID := ColumnBox_Replays.ItemIndex;
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
    MapEdSizeX := MapSize[Radio_MapEdSizeX.ItemIndex+1];
    MapEdSizeY := MapSize[Radio_MapEdSizeY.ItemIndex+1];
    fGameApp.NewMapEditor('', MapEdSizeX, MapEdSizeY);
  end;

  //This is also called by double clicking on a map in the list
  if ((Sender = Button_MapEd_Load) or (Sender = ColumnBox_MapEd)) and
     Button_MapEd_Load.Enabled and (ColumnBox_MapEd.ItemIndex <> -1) then
  begin
    ID := ColumnBox_MapEd.Rows[ColumnBox_MapEd.ItemIndex].Tag;
    if Radio_MapEd_MapType.ItemIndex = 0 then
      Maps := fMaps
    else
      Maps := fMapsMP;

    Maps.Lock;
      fGameApp.NewMapEditor(Maps[ID].FullPath('.dat'), 0, 0);
    Maps.Unlock;

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

  ColumnBox_MapEd.Clear;
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
  OldTopIndex := ColumnBox_MapEd.TopIndex;
  ColumnBox_MapEd.Clear;

  if Radio_MapEd_MapType.ItemIndex = 0 then
    Maps := fMaps
  else
    Maps := fMapsMP;

  Maps.Lock;
    for I := 0 to Maps.Count - 1 do
      ColumnBox_MapEd.AddItem(MakeListRow([Maps[I].FileName, IntToStr(Maps[I].PlayerCount), Maps[I].SizeText], I));

    for I := 0 to Maps.Count - 1 do
      if (Maps[I].CRC = fLastMapCRC) then
        ColumnBox_MapEd.ItemIndex := I;
  Maps.Unlock;

  ColumnBox_MapEd.TopIndex := OldTopIndex;

  if aJumpToSelected and (ColumnBox_MapEd.ItemIndex <> -1)
  and not InRange(ColumnBox_MapEd.ItemIndex - ColumnBox_MapEd.TopIndex, 0, ColumnBox_MapEd.GetVisibleRows-1)
  then
    if ColumnBox_MapEd.ItemIndex < ColumnBox_MapEd.TopIndex then
      ColumnBox_MapEd.TopIndex := ColumnBox_MapEd.ItemIndex
    else
    if ColumnBox_MapEd.ItemIndex > ColumnBox_MapEd.TopIndex + ColumnBox_MapEd.GetVisibleRows - 1 then
      ColumnBox_MapEd.TopIndex := ColumnBox_MapEd.ItemIndex - ColumnBox_MapEd.GetVisibleRows + 1;
end;


procedure TKMMainMenuInterface.MapEditor_ColumnClick(aValue: Integer);
var
  SM: TMapsSortMethod;
begin
  //Determine Sort method depending on which column user clicked
  with ColumnBox_MapEd do
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
  Button_MapEd_Load.Enabled := (ColumnBox_MapEd.ItemIndex <> -1);

  if Button_MapEd_Load.Enabled then
  begin
    ID := ColumnBox_MapEd.Rows[ColumnBox_MapEd.ItemIndex].Tag;
    if Radio_MapEd_MapType.ItemIndex = 0 then
      Maps := fMaps
    else
      Maps := fMapsMP;

    Maps.Lock;
      fLastMapCRC := Maps[ID].CRC;
      fMinimap.LoadFromMission(Maps[ID].FullPath('.dat'), []);
    Maps.Unlock;

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


procedure TKMMainMenuInterface.KeyDown(Key:Word; Shift: TShiftState);
begin
  if fMyControls.KeyDown(Key, Shift) then Exit; //Handled by Controls
end;


procedure TKMMainMenuInterface.KeyUp(Key:Word; Shift: TShiftState);
begin
  if fMyControls.KeyUp(Key, Shift) then Exit; //Handled by Controls
end;


procedure TKMMainMenuInterface.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  fMyControls.MouseDown(X,Y,Shift,Button);
end;


//Do something related to mouse movement in menu
procedure TKMMainMenuInterface.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  fMyControls.MouseMove(X,Y,Shift);

  fGuiCampaign.MouseMove(Shift, X, Y);
end;


procedure TKMMainMenuInterface.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  fMyControls.MouseUp(X,Y,Shift,Button);
  Exit; //We could have caused fGameApp reinit (i.e. resolution change), so exit at once
end;


//Should update anything we want to be updated, obviously
procedure TKMMainMenuInterface.UpdateState(aTickCount: Cardinal);
begin
  fLobby.UpdateState(aTickCount);
  fSingleMap.UpdateState(aTickCount);

  if fMaps <> nil then fMaps.UpdateState;
  if fMapsMP <> nil then fMapsMP.UpdateState;
  if fSaves <> nil then fSaves.UpdateState;
end;


end.
