unit KM_GUIMenuLobby;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Controls, Math, SysUtils,
  KM_Defaults,
  KM_Controls, KM_Maps, KM_Saves, KM_Pics, KM_InterfaceDefaults, KM_Minimap, KM_Networking;


type
  TLobbyTab = (ltDesc, ltOptions);

  TKMMenuLobby = class
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class

    fMapsMP: TKMapsCollection;
    fSavesMP: TKMSavesCollection;
    fMinimap: TKMMinimap;
    fNetworking: TKMNetworking;

    fLobbyTab: TLobbyTab;
    fChatMode: TChatMode;
    fChatWhisperRecipient: Integer; //Server index of the player who will receive the whisper

    procedure CreateControls(aParent: TKMPanel);
    procedure CreateChatMenu(aParent: TKMPanel);
    procedure CreatePlayerMenu(aParent: TKMPanel);
    procedure CreateSettingsPopUp(aParent: TKMPanel);

    procedure Reset(aKind: TNetPlayerKind; aPreserveMessage: Boolean = False; aPreserveMaps: Boolean = False);
    procedure GameOptionsTabSwitch(Sender: TObject);
    procedure GameOptionsChange(Sender: TObject);
    procedure FileDownloadClick(Sender: TObject);

    procedure ChatMenuSelect(aItem: Integer);
    procedure ChatMenuClick(Sender: TObject);
    procedure ChatMenuShow(Sender: TObject);

    procedure PlayerMenuClick(Sender: TObject);
    procedure PlayerMenuShow(Sender: TObject);

    procedure PlayersSetupChange(Sender: TObject);
    procedure MapColumnClick(aValue: Integer);
    procedure MapTypeChange(Sender: TObject);
    procedure MapList_SortUpdate(Sender: TObject);
    procedure MapList_ScanUpdate(Sender: TObject);
    procedure RefreshMapList(aJumpToSelected: Boolean);
    procedure RefreshSaveList(aJumpToSelected: Boolean);
    procedure MapChange(Sender: TObject);
    procedure PostKeyDown(Sender: TObject; Key: Word);

    procedure MinimapLocClick(aValue: Integer);

    procedure Lobby_OnDisconnect(const aData: UnicodeString);
    procedure Lobby_OnGameOptions(Sender: TObject);
    procedure Lobby_OnMapName(const aData: UnicodeString);
    procedure Lobby_OnMessage(const aText: UnicodeString);
    procedure Lobby_OnPingInfo(Sender: TObject);
    procedure Lobby_OnPlayersSetup(Sender: TObject);
    procedure Lobby_OnReassignedToHost(Sender: TObject);
    procedure Lobby_OnFileTransferProgress(aTotal, aProgress: Cardinal);

    function DetectMapType: Integer;
    procedure BackClick(Sender: TObject);
    procedure SettingsClick(Sender: TObject);
    procedure StartClick(Sender: TObject);
  protected
    Panel_Lobby: TKMPanel;
      Panel_LobbySettings: TKMPanel;
        Edit_LobbyDescription: TKMEdit;
        Edit_LobbyPassword: TKMEdit;
        Button_LobbySettingsSave: TKMButton;
        Button_LobbySettingsCancel: TKMButton;

      Menu_Chat: TKMPopUpMenu;
      Menu_Host: TKMPopUpMenu;

      Panel_LobbyServerName: TKMPanel;
        Label_LobbyServerName: TKMLabel;

      Panel_LobbyPlayers: TKMPanel;
        CheckBox_LobbyHostControl: TKMCheckBox;
        CheckBox_LobbyRandomizeTeamLocations: TKMCheckBox;
        Image_LobbyFlag: array [0..MAX_HANDS-1] of TKMImage;
        DropBox_LobbyPlayerSlot: array [0..MAX_HANDS-1] of TKMDropList;
        Label_LobbyPlayer: array [0..MAX_HANDS-1] of TKMLabel;
        DropBox_LobbyLoc: array [0..MAX_HANDS-1] of TKMDropList;
        DropBox_LobbyTeam: array [0..MAX_HANDS-1] of TKMDropList;
        DropBox_LobbyColors: array [0..MAX_HANDS-1] of TKMDropColumns;
        Image_LobbyReady: array [0..MAX_HANDS-1] of TKMImage;
        Label_LobbyPing: array [0..MAX_HANDS-1] of TKMLabel;

      Panel_LobbySetup: TKMPanel;
        Radio_LobbyMapType: TKMRadioGroup;
        DropCol_LobbyMaps: TKMDropColumns;
        Label_LobbyMapName: TKMLabel;
        Panel_LobbySetupTransfer: TKMPanel;
          Button_LobbySetupDownload: TKMButton;
          PercentBar_LobbySetupProgress: TKMPercentBar;
          CheckBox_LobbySetupOverwrite: TKMCheckbox;
        Panel_LobbySetupMinimap: TKMPanel;
          MinimapView_Lobby: TKMMinimapView;
        Button_LobbyTabDesc, Button_LobbyTabOptions: TKMButton;
        Panel_LobbySetupDesc: TKMPanel;
          Memo_LobbyMapDesc: TKMMemo;
        Panel_LobbySetupOptions: TKMPanel;
          TrackBar_LobbyPeacetime: TKMTrackBar;
          TrackBar_LobbySpeedPT, TrackBar_LobbySpeedAfterPT: TKMTrackBar;

      Memo_LobbyPosts: TKMMemo;
      Button_LobbyPost: TKMButtonFlat;
      Edit_LobbyPost: TKMEdit;

      Button_LobbyBack: TKMButton;
      Button_LobbyChangeSettings: TKMButton;
      Button_LobbyStart: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    destructor Destroy; override;

    function GetChatText: string;
    function GetChatMessages: string;
    procedure Show(aKind: TNetPlayerKind; aNetworking: TKMNetworking; aMainHeight: Word);
    procedure Lobby_Resize(aMainHeight: Word);
    procedure UpdateState;
  end;


implementation
uses KM_ResTexts, KM_ResLocales, KM_Utils, KM_Sound, KM_ResSound, KM_RenderUI, KM_Resource, KM_ResFonts, KM_NetPlayersList, KM_Main, KM_GameApp;


{ TKMGUIMenuLobby }
constructor TKMMenuLobby.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  fMinimap := TKMMinimap.Create(True, True);

  fMapsMP := TKMapsCollection.Create(True);
  fSavesMP := TKMSavesCollection.Create;

  CreateControls(aParent);
  CreateChatMenu(aParent);
  CreatePlayerMenu(aParent);
  CreateSettingsPopUp(aParent);
end;


destructor TKMMenuLobby.Destroy;
begin
  fMapsMP.Free;
  fSavesMP.Free;
  fMinimap.Free;

  inherited;
end;


procedure TKMMenuLobby.CreateControls(aParent: TKMPanel);
const
  CW = 690; C1 = 35; C2 = 195; C3 = 355; C4 = 445; C5 = 570; C6 = 650;
var
  I, K, OffY: Integer;
begin
  Panel_Lobby := TKMPanel.Create(aParent,0,0,aParent.Width, aParent.Height);
  Panel_Lobby.AnchorsStretch;

    //Server Name
    Panel_LobbyServerName := TKMPanel.Create(Panel_Lobby, 30, 30, CW, 30);
      TKMBevel.Create(Panel_LobbyServerName,   0,  0, CW, 30);
      Label_LobbyServerName := TKMLabel.Create(Panel_LobbyServerName, 10, 10, CW-20, 20, '', fnt_Metal, taLeft);

    //Players
    Panel_LobbyPlayers := TKMPanel.Create(Panel_Lobby, 30, 65, CW, 268);
      TKMBevel.Create(Panel_LobbyPlayers,  0,  0, CW, 268);

      CheckBox_LobbyHostControl := TKMCheckBox.Create(Panel_LobbyPlayers, 10, 10, 450, 20, gResTexts[TX_LOBBY_HOST_DOES_SETUP], fnt_Metal);
      CheckBox_LobbyHostControl.OnClick := PlayersSetupChange;

      CheckBox_LobbyRandomizeTeamLocations := TKMCheckBox.Create(Panel_LobbyPlayers, 10, 28, 450, 20, gResTexts[TX_LOBBY_RANDOMIZE_LOCATIONS], fnt_Metal);
      CheckBox_LobbyRandomizeTeamLocations.OnClick := PlayersSetupChange;

      //Column titles
      TKMLabel.Create(Panel_LobbyPlayers, C1, 50, 150,  20, gResTexts[TX_LOBBY_HEADER_PLAYERS], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, C2, 50, 150,  20, gResTexts[TX_LOBBY_HEADER_STARTLOCATION], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, C3, 50,  80,  20, gResTexts[TX_LOBBY_HEADER_TEAM], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, C4, 50,  80,  20, gResTexts[TX_LOBBY_HEADER_FLAGCOLOR], fnt_Outline, taLeft);
      TKMLabel.Create(Panel_LobbyPlayers, C5, 50, gResTexts[TX_LOBBY_HEADER_READY], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_LobbyPlayers, C6, 50, gResTexts[TX_LOBBY_HEADER_PING], fnt_Outline, taCenter);

      for I := 0 to MAX_HANDS - 1 do
      begin
        OffY := 70 + I * 24;
        Image_LobbyFlag[I] := TKMImage.Create(Panel_LobbyPlayers, 10, OffY, 20, 20, 0, rxGuiMain);
        Image_LobbyFlag[I].ImageCenter;
        Image_LobbyFlag[I].Tag := I+1; //Required for PlayerMenuShow
        Image_LobbyFlag[I].OnClick := PlayerMenuShow;

        Label_LobbyPlayer[I] := TKMLabel.Create(Panel_LobbyPlayers, C1, OffY+2, 150, 20, '', fnt_Grey, taLeft);
        Label_LobbyPlayer[I].Hide;

        DropBox_LobbyPlayerSlot[I] := TKMDropList.Create(Panel_LobbyPlayers, C1, OffY, 150, 20, fnt_Grey, '', bsMenu);
        DropBox_LobbyPlayerSlot[I].Add(gResTexts[TX_LOBBY_SLOT_OPEN]); //Player can join into this slot
        DropBox_LobbyPlayerSlot[I].Add(gResTexts[TX_LOBBY_SLOT_CLOSED]); //Closed, nobody can join it
        DropBox_LobbyPlayerSlot[I].Add(gResTexts[TX_LOBBY_SLOT_AI_PLAYER]); //This slot is an AI player
        DropBox_LobbyPlayerSlot[I].ItemIndex := 0; //Open
        DropBox_LobbyPlayerSlot[I].OnChange := PlayersSetupChange;

        DropBox_LobbyLoc[I] := TKMDropList.Create(Panel_LobbyPlayers, C2, OffY, 150, 20, fnt_Grey, '', bsMenu);
        DropBox_LobbyLoc[I].Add(gResTexts[TX_LOBBY_RANDOM], 0);
        DropBox_LobbyLoc[I].OnChange := PlayersSetupChange;

        DropBox_LobbyTeam[I] := TKMDropList.Create(Panel_LobbyPlayers, C3, OffY, 80, 20, fnt_Grey, '', bsMenu);
        DropBox_LobbyTeam[I].Add('-');
        for K := 1 to 4 do DropBox_LobbyTeam[I].Add(IntToStr(K));
        DropBox_LobbyTeam[I].OnChange := PlayersSetupChange;

        DropBox_LobbyColors[I] := TKMDropColumns.Create(Panel_LobbyPlayers, C4, OffY, 80, 20, fnt_Grey, '', bsMenu);
        DropBox_LobbyColors[I].SetColumns(fnt_Outline, [''], [0]);
        DropBox_LobbyColors[I].List.ShowHeader := False;
        DropBox_LobbyColors[I].FadeImageWhenDisabled := False;
        DropBox_LobbyColors[I].Add(MakeListRow([''], [$FFFFFFFF], [MakePic(rxGuiMain, 31)], 0));
        for K := Low(MP_TEAM_COLORS) to High(MP_TEAM_COLORS) do
          DropBox_LobbyColors[I].Add(MakeListRow([''], [MP_TEAM_COLORS[K]], [MakePic(rxGuiMain, 30)]));
        DropBox_LobbyColors[I].OnChange := PlayersSetupChange;

        Image_LobbyReady[I] := TKMImage.Create(Panel_LobbyPlayers, C5-8, OffY, 16, 16, 32, rxGuiMain);
        Label_LobbyPing[I] := TKMLabel.Create(Panel_LobbyPlayers, C6, OffY, '', fnt_Metal, taCenter);
      end;

    //Chat area
    Memo_LobbyPosts := TKMMemo.Create(Panel_Lobby, 30, 338, CW, 342, fnt_Arial, bsMenu);
    Memo_LobbyPosts.Anchors := [anLeft, anTop, anBottom];
    Memo_LobbyPosts.AutoWrap := True;
    Memo_LobbyPosts.IndentAfterNL := True; //Don't let players fake system messages
    Memo_LobbyPosts.ScrollDown := True;
    Button_LobbyPost := TKMButtonFlat.Create(Panel_Lobby, 30, 683, 30, 20, 0);
    Button_LobbyPost.CapOffsetY := -11;
    Button_LobbyPost.Font := fnt_Grey;
    Button_LobbyPost.OnClick := ChatMenuShow;
    Button_LobbyPost.Anchors := [anLeft, anBottom];

    Edit_LobbyPost := TKMEdit.Create(Panel_Lobby, 60, 683, CW, 20, fnt_Arial);
    Edit_LobbyPost.OnKeyDown := PostKeyDown;
    Edit_LobbyPost.Anchors := [anLeft, anBottom];
    Edit_LobbyPost.ShowColors := True;

    //Setup
    Panel_LobbySetup := TKMPanel.Create(Panel_Lobby, 725, 30, 270, 712);
    Panel_LobbySetup.Anchors := [anLeft, anTop, anBottom];
      with TKMBevel.Create(Panel_LobbySetup,  0,  0, 270, 712) do AnchorsStretch;
      Radio_LobbyMapType := TKMRadioGroup.Create(Panel_LobbySetup, 10, 10, 250, 80, fnt_Metal);
      Radio_LobbyMapType.Add(gResTexts[TX_LOBBY_MAP_BUILD]);
      Radio_LobbyMapType.Add(gResTexts[TX_LOBBY_MAP_FIGHT]);
      Radio_LobbyMapType.Add(gResTexts[TX_LOBBY_MAP_COOP]);
      Radio_LobbyMapType.Add(gResTexts[TX_LOBBY_MAP_SPECIAL]);
      Radio_LobbyMapType.Add(gResTexts[TX_LOBBY_MAP_SAVED]);
      Radio_LobbyMapType.ItemIndex := 0;
      Radio_LobbyMapType.OnChange := MapTypeChange;

      DropCol_LobbyMaps := TKMDropColumns.Create(Panel_LobbySetup, 10, 95, 250, 20, fnt_Metal, gResTexts[TX_LOBBY_MAP_SELECT], bsMenu);
      DropCol_LobbyMaps.DropCount := 19;
      DropCol_LobbyMaps.DropWidth := 430; //Wider to fit mapnames well
      DropCol_LobbyMaps.SetColumns(fnt_Outline, [gResTexts[TX_MENU_MAP_TITLE], '#', gResTexts[TX_MENU_MAP_SIZE]], [0, 290, 320]);
      DropCol_LobbyMaps.List.OnColumnClick := MapColumnClick;
      DropCol_LobbyMaps.List.SearchColumn := 0;
      DropCol_LobbyMaps.OnChange := MapChange;
      Label_LobbyMapName := TKMLabel.Create(Panel_LobbySetup, 10, 95, 250, 20, '', fnt_Metal, taLeft);

      Panel_LobbySetupMinimap := TKMPanel.Create(Panel_LobbySetup, 0, 120, 270, 200);
        TKMBevel.Create(Panel_LobbySetupMinimap, 35, 0, 199, 199);
        MinimapView_Lobby := TKMMinimapView.Create(Panel_LobbySetupMinimap, 39, 4, 191, 191);
        MinimapView_Lobby.ShowLocs := True; //In the minimap we want player locations to be shown
        MinimapView_Lobby.OnLocClick := MinimapLocClick;

      Panel_LobbySetupTransfer := TKMPanel.Create(Panel_LobbySetup, 0, 120, 270, 200);
        CheckBox_LobbySetupOverwrite := TKMCheckbox.Create(Panel_LobbySetupTransfer, 10, 0, 250, 16, gResTexts[TX_LOBBY_DOWNLOAD_OVERWRITE], fnt_Game);
        CheckBox_LobbySetupOverwrite.OnClick := FileDownloadClick;
        Button_LobbySetupDownload := TKMButton.Create(Panel_LobbySetupTransfer, 10, 0, 250, 30, gResTexts[TX_LOBBY_DOWNLOAD], bsMenu);
        Button_LobbySetupDownload.OnClick := FileDownloadClick;
        PercentBar_LobbySetupProgress := TKMPercentBar.Create(Panel_LobbySetupTransfer, 10, 0, 250, 24, fnt_Game);
      Panel_LobbySetupTransfer.Hide;

      Button_LobbyTabDesc := TKMButton.Create(Panel_LobbySetup, 10, 324, 125, 20, gResTexts[TX_LOBBY_MAP_DESCRIPTION], bsMenu);
      Button_LobbyTabDesc.OnClick := GameOptionsTabSwitch;
      Button_LobbyTabDesc.Hide;
      Button_LobbyTabOptions := TKMButton.Create(Panel_LobbySetup, 10+125, 324, 125, 20, gResTexts[TX_LOBBY_OPTIONS], bsMenu);
      Button_LobbyTabOptions.OnClick := GameOptionsTabSwitch;
      Button_LobbyTabOptions.Hide;

      Panel_LobbySetupDesc := TKMPanel.Create(Panel_LobbySetup, 0, 324, 270, 218);
      Panel_LobbySetupDesc.Anchors := [anLeft, anTop, anBottom];
        Memo_LobbyMapDesc := TKMMemo.Create(Panel_LobbySetupDesc, 10, 0, 250, 218, fnt_Game, bsMenu);
        Memo_LobbyMapDesc.Anchors := [anLeft,anTop,anBottom];
        Memo_LobbyMapDesc.AutoWrap := True;
        Memo_LobbyMapDesc.ItemHeight := 16;

      Panel_LobbySetupOptions := TKMPanel.Create(Panel_LobbySetup, 0, 542, 270, 170);
      Panel_LobbySetupOptions.Anchors := [anLeft,anBottom];
        with TKMLabel.Create(Panel_LobbySetupOptions, 10, 4, 250, 20, gResTexts[TX_LOBBY_GAME_OPTIONS], fnt_Outline, taLeft) do Anchors := [anLeft,anBottom];
        TrackBar_LobbyPeacetime := TKMTrackBar.Create(Panel_LobbySetupOptions, 10, 26, 250, 0, 120);
        TrackBar_LobbyPeacetime.Anchors := [anLeft,anBottom];
        TrackBar_LobbyPeacetime.Caption := gResTexts[TX_LOBBY_PEACETIME];
        TrackBar_LobbyPeacetime.Step := 5; //Round to 5min steps
        TrackBar_LobbyPeacetime.OnChange := GameOptionsChange;

        TrackBar_LobbySpeedPT := TKMTrackBar.Create(Panel_LobbySetupOptions, 10, 72, 250, 1, 5);
        TrackBar_LobbySpeedPT.Anchors := [anLeft,anBottom];
        TrackBar_LobbySpeedPT.Caption := gResTexts[TX_LOBBY_GAMESPEED_PEACETIME];
        TrackBar_LobbySpeedPT.ThumbWidth := 45; //Enough to fit 'x2.5'
        TrackBar_LobbySpeedPT.OnChange := GameOptionsChange;

        TrackBar_LobbySpeedAfterPT := TKMTrackBar.Create(Panel_LobbySetupOptions, 10, 116, 250, 1, 5);
        TrackBar_LobbySpeedAfterPT.Anchors := [anLeft,anBottom];
        TrackBar_LobbySpeedAfterPT.Caption := gResTexts[TX_LOBBY_GAMESPEED];
        TrackBar_LobbySpeedAfterPT.ThumbWidth := 45; //Enough to fit 'x2.5'
        TrackBar_LobbySpeedAfterPT.OnChange := GameOptionsChange;

    Button_LobbyBack := TKMButton.Create(Panel_Lobby, 30, 712, 220, 30, gResTexts[TX_LOBBY_QUIT], bsMenu);
    Button_LobbyBack.Anchors := [anLeft, anBottom];
    Button_LobbyBack.OnClick := BackClick;

    Button_LobbyChangeSettings := TKMButton.Create(Panel_Lobby, 265, 712, 220, 30, gResTexts[TX_LOBBY_ROOMSETTINGS], bsMenu);
    Button_LobbyChangeSettings.Anchors := [anLeft, anBottom];
    Button_LobbyChangeSettings.OnClick := SettingsClick;

    Button_LobbyStart := TKMButton.Create(Panel_Lobby, 500, 712, 220, 30, NO_TEXT, bsMenu);
    Button_LobbyStart.Anchors := [anLeft, anBottom];
    Button_LobbyStart.OnClick := StartClick;
end;


procedure TKMMenuLobby.CreateChatMenu(aParent: TKMPanel);
begin
  Menu_Chat := TKMPopUpMenu.Create(aParent, 120);
  Menu_Chat.Anchors := [anLeft, anBottom];
  //Menu gets populated right before show
  Menu_Chat.AddItem(NO_TEXT);
  Menu_Chat.OnClick := ChatMenuClick;
end;


procedure TKMMenuLobby.CreatePlayerMenu(aParent: TKMPanel);
begin
  Menu_Host := TKMPopUpMenu.Create(aParent, 120);
  Menu_Host.AddItem(gResTexts[TX_LOBBY_PLAYER_KICK]);
  //Menu_Host.AddItem(fTextMain[TX_LOBBY_PLAYER_BAN]);
  //Menu_Host.AddItem(fTextMain[TX_LOBBY_PLAYER_SETHOST]);
  Menu_Host.OnClick := PlayerMenuClick;
end;


procedure TKMMenuLobby.CreateSettingsPopUp(aParent: TKMPanel);
begin
  Panel_LobbySettings := TKMPanel.Create(aParent, 362, 250, 320, 300);
  Panel_LobbySettings.Anchors := [];
    TKMBevel.Create(Panel_LobbySettings, -1000,  -1000, 4000, 4000);
    TKMImage.Create(Panel_LobbySettings, -20, -75, 340, 310, 15, rxGuiMain);
    TKMBevel.Create(Panel_LobbySettings,   0,  0, 320, 300);
    TKMLabel.Create(Panel_LobbySettings,  20, 10, 280, 20, gResTexts[TX_LOBBY_ROOMSETTINGS], fnt_Outline, taCenter);

    TKMLabel.Create(Panel_LobbySettings, 20, 50, 156, 20, gResTexts[TX_LOBBY_ROOM_DESCRIPTION], fnt_Outline, taLeft);
    Edit_LobbyDescription := TKMEdit.Create(Panel_LobbySettings, 20, 70, 152, 20, fnt_Grey);
    Edit_LobbyDescription.AllowedChars := acText;

    TKMLabel.Create(Panel_LobbySettings, 20, 100, 156, 20, gResTexts[TX_LOBBY_ROOM_PASSWORD], fnt_Outline, taLeft);
    Edit_LobbyPassword := TKMEdit.Create(Panel_LobbySettings, 20, 120, 152, 20, fnt_Grey);
    Edit_LobbyPassword.AllowedChars := acText;

    Button_LobbySettingsSave := TKMButton.Create(Panel_LobbySettings, 20, 160, 280, 30, gResTexts[TX_LOBBY_ROOM_OK], bsMenu);
    Button_LobbySettingsSave.OnClick := SettingsClick;
    Button_LobbySettingsCancel := TKMButton.Create(Panel_LobbySettings, 20, 200, 280, 30, gResTexts[TX_LOBBY_ROOM_CANCEL], bsMenu);
    Button_LobbySettingsCancel.OnClick := SettingsClick;
end;


procedure TKMMenuLobby.ChatMenuSelect(aItem: Integer);

  procedure UpdateButtonCaption(aCaption: UnicodeString; aColor: Cardinal = 0);
  var CapWidth: Integer;
  const MIN_SIZE = 80; //Minimum size for the button
  begin
    //Update button width according to selected item
    CapWidth := gResource.Fonts.GetTextSize(aCaption, Button_LobbyPost.Font).X;
    CapWidth := Max(MIN_SIZE, CapWidth+10); //Apply minimum size
    if aColor <> 0 then
      aCaption := WrapColor(aCaption, aColor);
    Button_LobbyPost.Caption := aCaption;
    Button_LobbyPost.Width := CapWidth;

    Edit_LobbyPost.AbsLeft := Button_LobbyPost.AbsLeft + Button_LobbyPost.Width + 4;
    Edit_LobbyPost.Width := Memo_LobbyPosts.Width - Button_LobbyPost.Width - 4;
  end;

var I: Integer;
begin
  //All
  if aItem = -1 then
  begin
    fChatMode := cmAll;
    UpdateButtonCaption(gResTexts[TX_CHAT_ALL]);
    Edit_LobbyPost.DrawOutline := False; //No outline for All
  end
  else
    //Team
    if aItem = -2 then
    begin
      fChatMode := cmTeam;
      UpdateButtonCaption(gResTexts[TX_CHAT_TEAM], $FF66FF66);
      Edit_LobbyPost.DrawOutline := True;
      Edit_LobbyPost.OutlineColor := $FF66FF66;
    end
    else
    //Whisper
    begin
      I := fNetworking.NetPlayers.ServerToLocal(aItem);
      if I <> -1 then
      begin
        fChatMode := cmWhisper;
        Edit_LobbyPost.DrawOutline := True;
        Edit_LobbyPost.OutlineColor := $FF00B9FF;
        with fNetworking.NetPlayers[I] do
        begin
          fChatWhisperRecipient := IndexOnServer;
          UpdateButtonCaption(UnicodeString(Nikname), IfThen(FlagColorID <> 0, FlagColorToTextColor(FlagColor), 0));
        end;
      end;
    end;
end;


procedure TKMMenuLobby.ChatMenuClick(Sender: TObject);
begin
  if Menu_Chat.ItemIndex <> -1 then
    ChatMenuSelect(Menu_Chat.ItemTags[Menu_Chat.ItemIndex]);
end;


procedure TKMMenuLobby.ChatMenuShow(Sender: TObject);
var
  C: TKMControl;
  I: Integer;
  n: TKMNetPlayerInfo;
begin
  //Populate menu with right options
  Menu_Chat.Clear;

  Menu_Chat.AddItem(gResTexts[TX_CHAT_ALL], -1);
  //Only show "Team" if the player is on a team
  if fNetworking.NetPlayers[fNetworking.MyIndex].Team <> 0 then
    Menu_Chat.AddItem('[$66FF66]' + gResTexts[TX_CHAT_TEAM], -2);

  for I := 1 to fNetworking.NetPlayers.Count do
  if I <> fNetworking.MyIndex then //Can't whisper to yourself
  begin
    n := fNetworking.NetPlayers[I];

    if n.IsHuman and n.Connected and not n.Dropped then
      Menu_Chat.AddItem(UnicodeString(n.NiknameColored), n.IndexOnServer);
  end;

  C := TKMControl(Sender);
  //Position the menu next to the icon, but do not overlap players name
  Menu_Chat.ShowAt(C.AbsLeft, C.AbsTop - Menu_Chat.Height);
end;


//Try to detect which kind it is
function TKMMenuLobby.DetectMapType: Integer;
begin
  //Default
  Result := 0;

  case fNetworking.SelectGameKind of
    ngk_Map:  if fNetworking.MapInfo.IsCoop then
                Result := 2
              else
              if fNetworking.MapInfo.IsSpecial then
                Result := 3
              else
              if fNetworking.MapInfo.MissionMode = mm_Tactic then
                Result := 1;
    ngk_Save: Result := 4;
  end;
end;


//Access text that user was typing to copy it over to gameplay chat
function TKMMenuLobby.GetChatText: string;
begin
  Result := Edit_LobbyPost.Text;
end;


//Access chat messages history to copy it over to gameplay chat
function TKMMenuLobby.GetChatMessages: string;
begin
  Result := Memo_LobbyPosts.Text;
end;


procedure TKMMenuLobby.Show(aKind: TNetPlayerKind; aNetworking: TKMNetworking; aMainHeight: Word);
begin
  fNetworking := aNetworking;

  Reset(aKind);

  //Events binding is the same for Host and Joiner because of stand-alone Server
  //E.g. If Server fails, Host can be disconnected from it as well as a Joiner
  fNetworking.OnTextMessage  := Lobby_OnMessage;
  fNetworking.OnPlayersSetup := Lobby_OnPlayersSetup;
  fNetworking.OnGameOptions  := Lobby_OnGameOptions;
  fNetworking.OnMapName      := Lobby_OnMapName;
  fNetworking.OnPingInfo     := Lobby_OnPingInfo;
  //fNetworking.OnStartMap - already assigned in fGameApp when Net is created
  //fNetworking.OnStartSave - already assigned in fGameApp when Net is created
  fNetworking.OnDisconnect   := Lobby_OnDisconnect;
  fNetworking.OnReassignedHost := Lobby_OnReassignedToHost;
  fNetworking.OnFileTransferProgress := Lobby_OnFileTransferProgress;

  ChatMenuSelect(-1); //All

  Panel_Lobby.Show;
  Lobby_Resize(aMainHeight);
end;


procedure TKMMenuLobby.Lobby_Resize(aMainHeight: Word);
begin
  if not Panel_Lobby.Visible then Exit;
  //If the vertical screen height goes below a certain amount we need to switch to "compact" mode
  if aMainHeight >= 660 then
  begin
    //We have enough space, so stack Options below Desc
    Panel_LobbySetupDesc.Top := 324;
    Panel_LobbySetupDesc.Height := aMainHeight-550;
    Panel_LobbySetupOptions.Top := 324 + Panel_LobbySetupDesc.Height;
    Button_LobbyTabDesc.Hide;
    Button_LobbyTabOptions.Hide;
    Panel_LobbySetupDesc.Show;
    Panel_LobbySetupOptions.Show;
  end
  else
  begin
    //Not enough space, so enabled tabbed view
    Panel_LobbySetupDesc.Top := 350;
    Panel_LobbySetupDesc.Height := aMainHeight-420;
    Panel_LobbySetupOptions.Top := 350;
    Button_LobbyTabDesc.Show;
    Button_LobbyTabOptions.Show;
    GameOptionsTabSwitch(nil);
  end;
end;


procedure TKMMenuLobby.GameOptionsTabSwitch(Sender: TObject);
begin
  if Sender = Button_LobbyTabDesc then
    fLobbyTab := ltDesc;
  if Sender = Button_LobbyTabOptions then
    fLobbyTab := ltOptions;

  case fLobbyTab of
    ltDesc:    begin
                 Panel_LobbySetupDesc.Show;
                 Panel_LobbySetupOptions.Hide;
               end;
    ltOptions: begin
                 Panel_LobbySetupDesc.Hide;
                 Panel_LobbySetupOptions.Show;
               end;
  end;
end;


procedure TKMMenuLobby.BackClick(Sender: TObject);
begin
  //Scan should be terminated, it is no longer needed
  fMapsMP.TerminateScan;

  fNetworking.AnnounceDisconnect;
  fNetworking.Disconnect;

  fOnPageChange(gpMultiplayer, gResTexts[TX_GAME_ERROR_DISCONNECT]);
end;


//Reset everything to it's defaults depending on users role (Host/Joiner/Reassigned)
procedure TKMMenuLobby.Reset(aKind: TNetPlayerKind; aPreserveMessage: Boolean = False; aPreserveMaps: Boolean = False);
var I: Integer;
begin
  Label_LobbyServerName.Caption := '';

  for I := 0 to MAX_HANDS - 1 do
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
    DropBox_LobbyColors[I].Disable;
    DropBox_LobbyColors[I].ItemIndex := 0;
    DropBox_LobbyPlayerSlot[I].ItemIndex := 0; //Open
    Image_LobbyReady[I].TexID := 0;
    Label_LobbyPing[I].Caption := '';
  end;

  if not aPreserveMessage then Memo_LobbyPosts.Clear;
  Edit_LobbyPost.Text := '';
  ChatMenuSelect(-1); //All

  Label_LobbyMapName.Caption := '';
  Memo_LobbyMapDesc.Clear;

  TrackBar_LobbyPeacetime.Position := 0; //Default peacetime = 0
  TrackBar_LobbySpeedPT.Position := 1; //Default speed = 1
  TrackBar_LobbySpeedPT.ThumbText := 'x1';
  TrackBar_LobbySpeedAfterPT.Position := 1; //Default speed = 1
  TrackBar_LobbySpeedAfterPT.ThumbText := 'x1';

  Lobby_OnMapName('');

  //Setup for Host
  if aKind = lpk_Host then
  begin
    Radio_LobbyMapType.Enable;
    Radio_LobbyMapType.ItemIndex := 0;
    if not aPreserveMaps then MapTypeChange(nil);
    DropCol_LobbyMaps.Show;
    Label_LobbyMapName.Hide;
    Button_LobbyStart.Caption := gResTexts[TX_LOBBY_START]; //Start
    Button_LobbyStart.Disable;
    TrackBar_LobbyPeacetime.Disable;
    TrackBar_LobbySpeedPT.Disable;
    TrackBar_LobbySpeedAfterPT.Disable;
    CheckBox_LobbyHostControl.Enable;
    CheckBox_LobbyRandomizeTeamLocations.Enable;
    Button_LobbyChangeSettings.Show;
  end
  else //Setup for Joiner
  begin
    Radio_LobbyMapType.Disable;
    Radio_LobbyMapType.ItemIndex := 0;
    DropCol_LobbyMaps.Hide;
    Label_LobbyMapName.Show;
    Button_LobbyStart.Caption := gResTexts[TX_LOBBY_READY]; //Ready
    Button_LobbyStart.Enable;
    TrackBar_LobbyPeacetime.Disable;
    TrackBar_LobbySpeedPT.Disable;
    TrackBar_LobbySpeedAfterPT.Disable;
    CheckBox_LobbyHostControl.Disable;
    CheckBox_LobbyRandomizeTeamLocations.Disable;
    Button_LobbyChangeSettings.Hide;
  end;
end;


procedure TKMMenuLobby.GameOptionsChange(Sender: TObject);
begin
  //Set the peacetime length (up to 300min)
  fNetworking.NetGameOptions.Peacetime := EnsureRange(TrackBar_LobbyPeacetime.Position, 0, 300);
  fNetworking.NetGameOptions.SpeedPT := (TrackBar_LobbySpeedPT.Position - 1) / 2 + 1;
  fNetworking.NetGameOptions.SpeedAfterPT := (TrackBar_LobbySpeedAfterPT.Position - 1) / 2 + 1;
  fNetworking.SendGameOptions;

  //Refresh the data to controls
  Lobby_OnGameOptions(nil);
end;


procedure TKMMenuLobby.FileDownloadClick(Sender: TObject);
begin
  if Sender = Button_LobbySetupDownload then
  begin
    fNetworking.RequestFileTransfer;
    Button_LobbySetupDownload.Hide;
    CheckBox_LobbySetupOverwrite.Hide;
    Lobby_OnFileTransferProgress(1, 0);
    PercentBar_LobbySetupProgress.Caption := gResTexts[TX_LOBBY_DOWNLOADING];
  end;
  if Sender = CheckBox_LobbySetupOverwrite then
    Button_LobbySetupDownload.Enabled := CheckBox_LobbySetupOverwrite.Checked;
end;


procedure TKMMenuLobby.Lobby_OnGameOptions(Sender: TObject);
begin
  TrackBar_LobbyPeacetime.Position := fNetworking.NetGameOptions.Peacetime;

  TrackBar_LobbySpeedPT.Enabled   := (TrackBar_LobbyPeacetime.Position > 0) and TrackBar_LobbySpeedAfterPT.Enabled;
  TrackBar_LobbySpeedPT.Position  := Round((fNetworking.NetGameOptions.SpeedPT - 1) * 2 + 1);
  TrackBar_LobbySpeedPT.ThumbText := 'x' + FloatToStr(fNetworking.NetGameOptions.SpeedPT);

  TrackBar_LobbySpeedAfterPT.Position  := Round((fNetworking.NetGameOptions.SpeedAfterPT - 1) * 2 + 1);
  TrackBar_LobbySpeedAfterPT.ThumbText := 'x' + FloatToStr(fNetworking.NetGameOptions.SpeedAfterPT);
end;


procedure TKMMenuLobby.PlayerMenuClick(Sender: TObject);
var
  id: Integer;
begin
  //We can't really do global bans because player's IP addresses change all the time (and we have no other way to identify someone).
  //My idea was for bans to be managed completely by the server, since player's don't actually know each other's IPs.
  //So the host says "please ban client 3257" and the server adds his IP to the ban list for this room. The ban list
  //is then reset when the room becomes empty. Maybe we need to call the button "ban from this lobby" instead.
  //In any way banlist should be editable from within the lobby, so we will need methods to get the list
  //from the server and allow to remove items from it.

  id := fNetworking.NetPlayers.ServerToLocal(TKMControl(Sender).Tag);
  if id = -1 then Exit; //Player has quit the lobby

  //Kick
  if (Sender = Menu_Host) and (Menu_Host.ItemIndex = 0) then
    fNetworking.KickPlayer(id);

  //todo: Ban
  //if (Sender = Menu_Host) and (ListBox_PlayerMenuHost.ItemIndex = 1) then

  //todo: Set to host
  //if (Sender = Menu_Host) and (ListBox_PlayerMenuHost.ItemIndex = 2) then
end;


procedure TKMMenuLobby.PlayerMenuShow(Sender: TObject);
var
  ctrl: TKMControl;
begin
  ctrl := TKMControl(Sender);

  //Only human players (excluding ourselves) have the player menu
  if not fNetworking.NetPlayers[ctrl.Tag].IsHuman //No menu for AI players
  or (fNetworking.MyIndex = ctrl.Tag) //No menu for ourselves
  or not fNetworking.IsHost //Only host gets to use the menu (for now)
  or not fNetworking.NetPlayers[ctrl.Tag].Connected then //Don't show menu for empty slots
    Exit;

  //Remember which player it is by his server index
  //since order of players can change. If someone above leaves we still have the proper Id
  Menu_Host.Tag := fNetworking.NetPlayers[ctrl.Tag].IndexOnServer;

  //Position the menu next to the icon, but do not overlap players name
  Menu_Host.ShowAt(ctrl.AbsLeft, ctrl.AbsTop + ctrl.Height);
end;


//Try to change players setup, Networking will check if it can be done under current
//conditions immediately and reverts the change without disturbing Host.
//If the change is possible Networking will send query to the Host.
//Host will reply with OnPlayersSetup event and data will be actualized.
procedure TKMMenuLobby.PlayersSetupChange(Sender: TObject);
var
  I: Integer;
begin
  //Host control toggle
  if Sender = CheckBox_LobbyHostControl then
  begin
    fNetworking.NetPlayers.HostDoesSetup := CheckBox_LobbyHostControl.Checked;
    fNetworking.SendPlayerListAndRefreshPlayersSetup;
  end;

  if Sender = CheckBox_LobbyRandomizeTeamLocations then
  begin
    fNetworking.NetPlayers.RandomizeTeamLocations := CheckBox_LobbyRandomizeTeamLocations.Checked;
    fNetworking.SendPlayerListAndRefreshPlayersSetup;
  end;

  for I := 0 to MAX_HANDS - 1 do
  begin
    //Starting location
    if (Sender = DropBox_LobbyLoc[I]) and DropBox_LobbyLoc[I].Enabled then
    begin
      fNetworking.SelectLoc(DropBox_LobbyLoc[I].GetSelectedTag, I+1);
      //Host with HostDoesSetup could have given us some location we don't know about from a map/save we don't have
      if fNetworking.SelectGameKind <> ngk_None then
        DropBox_LobbyLoc[I].SelectByTag(fNetworking.NetPlayers[I+1].StartLocation);
    end;

    //Team
    if (Sender = DropBox_LobbyTeam[I]) and DropBox_LobbyTeam[I].Enabled then
      fNetworking.SelectTeam(DropBox_LobbyTeam[I].ItemIndex, I+1);

    //Color
    if (Sender = DropBox_LobbyColors[I]) and DropBox_LobbyColors[I].Enabled then
    begin
      fNetworking.SelectColor(DropBox_LobbyColors[I].ItemIndex, I+1);
      DropBox_LobbyColors[I].ItemIndex := fNetworking.NetPlayers[I+1].FlagColorID;
    end;

    if Sender = DropBox_LobbyPlayerSlot[I] then
    begin
      //Modify an existing player
      if (I < fNetworking.NetPlayers.Count) then
      begin
        case DropBox_LobbyPlayerSlot[I].ItemIndex of
          0: //Open
            begin
              if fNetworking.NetPlayers[I+1].IsComputer
              or fNetworking.NetPlayers[I+1].IsClosed then
                fNetworking.NetPlayers.RemPlayer(I+1);
            end;
          1: //Closed
            fNetworking.NetPlayers.AddClosedPlayer(I+1); //Replace it
          2: //AI
            fNetworking.NetPlayers.AddAIPlayer(I+1); //Replace it
        end;
      end
      else
      begin
        //Add a new player
        if DropBox_LobbyPlayerSlot[I].ItemIndex = 1 then //Closed
          fNetworking.NetPlayers.AddClosedPlayer;
        if DropBox_LobbyPlayerSlot[I].ItemIndex = 2 then //AI
        begin
          fNetworking.NetPlayers.AddAIPlayer;
          if fNetworking.SelectGameKind = ngk_Save then
            fNetworking.MatchPlayersToSave(fNetworking.NetPlayers.Count); //Match new AI player in save
        end;
      end;

      fNetworking.SendPlayerListAndRefreshPlayersSetup;
    end;
  end;
end;


//Players list has been updated
//We should reflect it to UI
procedure TKMMenuLobby.Lobby_OnPlayersSetup(Sender: TObject);
var
  I,K,ID,LocaleID: Integer;
  MyNik, CanEdit, HostCanEdit, IsSave, IsCoop, IsValid: Boolean;
  CurPlayer: TKMNetPlayerInfo;
  LocationName: string;
begin
  IsSave := fNetworking.SelectGameKind = ngk_Save;
  IsCoop := (fNetworking.SelectGameKind = ngk_Map) and (fNetworking.MapInfo.IsCoop);

  //Go through active players first
  for I := 0 to fNetworking.NetPlayers.Count - 1 do
  begin
    CurPlayer := fNetworking.NetPlayers[I+1];

    //Flag icon
    if CurPlayer.IsComputer then
      Image_LobbyFlag[I].TexID := 62 //PC icon
    else
    begin
      LocaleID := gResLocales.IndexByCode(CurPlayer.LangCode);
      if LocaleID <> -1 then
        Image_LobbyFlag[I].TexID := gResLocales[LocaleID].FlagSpriteID
      else
        Image_LobbyFlag[I].TexID := 0;
    end;

    //Players list
    if fNetworking.IsHost and (not CurPlayer.IsHuman) then
    begin
      Label_LobbyPlayer[I].Hide;
      DropBox_LobbyPlayerSlot[I].Enable;
      DropBox_LobbyPlayerSlot[I].Show;
      if CurPlayer.IsComputer then
        DropBox_LobbyPlayerSlot[I].ItemIndex := 2 //AI
      else
        DropBox_LobbyPlayerSlot[I].ItemIndex := 1; //Closed
    end
    else
    begin
      Label_LobbyPlayer[I].Caption := CurPlayer.SlotName;
      if CurPlayer.FlagColorID = 0 then
        Label_LobbyPlayer[I].FontColor := $FFFFFFFF
      else
        Label_LobbyPlayer[I].FontColor := FlagColorToTextColor(CurPlayer.FlagColor);
      Label_LobbyPlayer[I].Show;
      DropBox_LobbyPlayerSlot[I].Disable;
      DropBox_LobbyPlayerSlot[I].Hide;
      DropBox_LobbyPlayerSlot[I].ItemIndex := 0; //Open
    end;

    //Starting locations
    //If we can't load the map, don't attempt to show starting locations
    IsValid := False;
    DropBox_LobbyLoc[I].Clear;
    case fNetworking.SelectGameKind of
      ngk_None: DropBox_LobbyLoc[I].Add(gResTexts[TX_LOBBY_RANDOM], 0);
      ngk_Save: begin
                  IsValid := fNetworking.SaveInfo.IsValid;
                  DropBox_LobbyLoc[I].Add(gResTexts[TX_LOBBY_SELECT], 0);
                  if CurPlayer.IsHuman then //Cannot add AIs to MP save, they are filled automatically
                    for K := 0 to fNetworking.SaveInfo.Info.PlayerCount - 1 do
                      if fNetworking.SaveInfo.Info.Enabled[K]
                      and (fNetworking.SaveInfo.Info.CanBeHuman[K] or ALLOW_TAKE_AI_PLAYERS) then
                        DropBox_LobbyLoc[I].Add(UnicodeString(fNetworking.SaveInfo.Info.OwnerNikname[K]), K+1);
                end;
      ngk_Map:  begin
                  IsValid := fNetworking.MapInfo.IsValid;
                  DropBox_LobbyLoc[I].Add(gResTexts[TX_LOBBY_RANDOM], 0);

                  for K := 0 to fNetworking.MapInfo.LocCount - 1 do
                    //AI-only locations should not be listed for AIs in lobby, since those ones are
                    //automatically added when the game starts (so AI checks CanBeHuman too)
                    if (CurPlayer.IsHuman and (fNetworking.MapInfo.CanBeHuman[K] or ALLOW_TAKE_AI_PLAYERS))
                    or (CurPlayer.IsComputer and fNetworking.MapInfo.CanBeHuman[K] and fNetworking.MapInfo.CanBeAI[K]) then
                    begin
                      LocationName := fNetworking.MapInfo.LocationName(K);
                      //Disable taken locations
                      if not fNetworking.NetPlayers.LocAvailable(K+1)
                      and (fNetworking.NetPlayers[I+1].StartLocation <> K+1) then
                        LocationName := '[$707070]' + LocationName + '[]';
                      DropBox_LobbyLoc[I].Add(LocationName, K+1);
                    end;
                end;
    end;

    if IsValid then
      DropBox_LobbyLoc[I].SelectByTag(CurPlayer.StartLocation)
    else
      DropBox_LobbyLoc[I].ItemIndex := 0;

    //Teams
    if IsCoop then
      DropBox_LobbyTeam[I].ItemIndex := 0 //No teams in coop maps, it's done for you
    else
      DropBox_LobbyTeam[I].ItemIndex := CurPlayer.Team;

    DropBox_LobbyColors[I].ItemIndex := CurPlayer.FlagColorID;
    if CurPlayer.IsClosed then
      Image_LobbyReady[I].TexID := 0
    else
      Image_LobbyReady[I].TexID := 32+Byte(CurPlayer.ReadyToStart);

    MyNik := (I+1 = fNetworking.MyIndex); //Our index
    //We are allowed to edit if it is our nickname and we are set as NOT ready,
    //or we are the host and this player is an AI
    CanEdit := (MyNik and (fNetworking.IsHost or not fNetworking.NetPlayers.HostDoesSetup) and
                          (fNetworking.IsHost or not CurPlayer.ReadyToStart)) or
               (fNetworking.IsHost and CurPlayer.IsComputer);
    HostCanEdit := (fNetworking.IsHost and fNetworking.NetPlayers.HostDoesSetup and
                    not CurPlayer.IsClosed);
    DropBox_LobbyLoc[I].Enabled := (CanEdit or HostCanEdit);
    //Can't change color or teams in a loaded save
    DropBox_LobbyTeam[I].Enabled := (CanEdit or HostCanEdit) and not IsSave and not IsCoop;
    DropBox_LobbyColors[I].Enabled := (CanEdit or (MyNik and not CurPlayer.ReadyToStart)) and not IsSave;
    if MyNik and not fNetworking.IsHost then
    begin
      if CurPlayer.ReadyToStart then
        Button_LobbyStart.Caption := gResTexts[TX_LOBBY_NOT_READY]
      else
        Button_LobbyStart.Caption := gResTexts[TX_LOBBY_READY];
    end
  end;

  //Disable rest of the players
  for I := fNetworking.NetPlayers.Count to MAX_HANDS - 1 do
  begin
    Label_LobbyPlayer[I].Caption := '';
    Image_LobbyFlag[I].TexID := 0;
    Label_LobbyPlayer[I].Hide;
    DropBox_LobbyPlayerSlot[I].Show;
    DropBox_LobbyPlayerSlot[I].ItemIndex := 0; //Open
    DropBox_LobbyLoc[I].ItemIndex := 0;
    DropBox_LobbyTeam[I].ItemIndex := 0;
    DropBox_LobbyColors[I].ItemIndex := 0;
    //Only host may change player slots, and only the first unused slot may be changed (so there are no gaps in net players list)
    DropBox_LobbyPlayerSlot[I].Enabled := fNetworking.IsHost and (I = fNetworking.NetPlayers.Count);
    Image_LobbyReady[I].TexID := 0; //Hidden
    DropBox_LobbyLoc[I].Disable;
    DropBox_LobbyTeam[I].Disable;
    DropBox_LobbyColors[I].Disable;
  end;

  //Update the minimap preivew with player colors
  for I := 0 to MAX_HANDS - 1 do
  begin
    ID := fNetworking.NetPlayers.StartingLocToLocal(I+1);
    if ID <> -1 then
      fMinimap.HandColors[I] := fNetworking.NetPlayers[ID].FlagColor
    else
      fMinimap.HandColors[I] := $7F000000; //Semi-transparent when not selected
  end;

  //If we have a map selected update the preview
  if (fNetworking.SelectGameKind = ngk_Map) and fNetworking.MapInfo.IsValid then
  begin
    fMinimap.Update(not fNetworking.MapInfo.IsCoop);
    MinimapView_Lobby.SetMinimap(fMinimap);
    for I := 0 to MAX_HANDS - 1 do
    begin
      ID := fNetworking.NetPlayers.StartingLocToLocal(I+1);
      if ID <> -1 then
        fMinimap.HandTeam[I] := fNetworking.NetPlayers[ID].Team
      else
        fMinimap.HandTeam[I] := 0;
    end;
  end;

  //If we are in team chat mode and find ourselves not on a team (player went back to no team), switch back to all
  if (fChatMode = cmTeam) and (fNetworking.NetPlayers[fNetworking.MyIndex].Team = 0) then
    ChatMenuSelect(-1);

  //If we are in whisper chat mode and find the player has left, switch back to all
  if fChatMode = cmWhisper then
  begin
    if fNetworking.NetPlayers.ServerToLocal(fChatWhisperRecipient) = -1 then
      ChatMenuSelect(-1)
    else
      ChatMenuSelect(fChatWhisperRecipient); //In case that player changed his color
  end;

  CheckBox_LobbyHostControl.Checked := fNetworking.NetPlayers.HostDoesSetup;
  CheckBox_LobbyRandomizeTeamLocations.Checked := fNetworking.NetPlayers.RandomizeTeamLocations;
  if fNetworking.IsHost then
    Button_LobbyStart.Enabled := fNetworking.CanStart;
  //If the game can't be started the text message with explanation will appear in chat area
end;


procedure TKMMenuLobby.Lobby_OnPingInfo(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to MAX_HANDS - 1 do
    if (fNetworking.Connected) and (I < fNetworking.NetPlayers.Count) and
       (fNetworking.NetPlayers[I+1].IsHuman) then
    begin
      Label_LobbyPing[I].Caption := IntToStr(fNetworking.NetPlayers[I+1].GetInstantPing);
      Label_LobbyPing[I].FontColor := GetPingColor(fNetworking.NetPlayers[I+1].GetInstantPing);
    end
    else
      Label_LobbyPing[I].Caption := '';

  Label_LobbyServerName.Caption := fNetworking.ServerName + ' #' + IntToStr(fNetworking.ServerRoom+1) +
                                   '  ' + fNetworking.ServerAddress + ' : ' + fNetworking.ServerPort;
end;


procedure TKMMenuLobby.MapTypeChange(Sender: TObject);
begin
  //Terminate any running scans otherwise they will continue to fill the drop box in the background
  fMapsMP.TerminateScan;
  fSavesMP.TerminateScan;
  DropCol_LobbyMaps.Clear; //Clear previous items in case scanning finds no maps/saves
  case Radio_LobbyMapType.ItemIndex of
    0,  //Build Map
    1,  //Fight Map
    2,  //Co-op Map
    3:  //Special map Map
        begin
          fMapsMP.Refresh(MapList_ScanUpdate);
          DropCol_LobbyMaps.DefaultCaption := gResTexts[TX_LOBBY_MAP_SELECT];
          DropCol_LobbyMaps.List.Header.Columns[0].Caption := gResTexts[TX_MENU_MAP_TITLE];
          DropCol_LobbyMaps.List.Header.Columns[2].Caption := gResTexts[TX_MENU_MAP_SIZE];
        end;
    4:  //Saved Game
        begin
          fSavesMP.Refresh(MapList_ScanUpdate, True);
          DropCol_LobbyMaps.DefaultCaption := gResTexts[TX_LOBBY_MAP_SELECT_SAVED];
          DropCol_LobbyMaps.List.Header.Columns[0].Caption := gResTexts[TX_MENU_LOAD_FILE];
          DropCol_LobbyMaps.List.Header.Columns[2].Caption := gResTexts[TX_MENU_SAVE_TIME];
        end;
    else
        begin
          DropCol_LobbyMaps.DefaultCaption := NO_TEXT;
        end;
  end;
  DropCol_LobbyMaps.ItemIndex := -1; //Clear previously selected item

  //The Sender is nil in Reset_Lobby when we are not connected
  if Sender <> nil then
    fNetworking.SelectNoMap('');
end;


//Change starting location
procedure TKMMenuLobby.MinimapLocClick(aValue: Integer);
var
  I: Integer;
  CanEdit: Boolean;
begin
  I := fNetworking.MyIndex;

  CanEdit := ((fNetworking.IsHost or not fNetworking.NetPlayers.HostDoesSetup) and
              (fNetworking.IsHost or not fNetworking.NetPlayers[I].ReadyToStart));

  if CanEdit then
  begin
    fNetworking.SelectLoc(aValue+1, I);
    //Host with HostDoesSetup could have given us some location we don't know about from a map/save we don't have
    if fNetworking.SelectGameKind <> ngk_None then
      DropBox_LobbyLoc[I-1].SelectByTag(fNetworking.NetPlayers[I].StartLocation);
  end;
end;


procedure TKMMenuLobby.MapList_SortUpdate(Sender: TObject);
begin
  //After sorting jump to the selected item
  if Sender = fSavesMP then
    RefreshSaveList(True);
  if Sender = fMapsMP then
    RefreshMapList(True);
end;


procedure TKMMenuLobby.MapList_ScanUpdate(Sender: TObject);
begin
  //Don't jump to selected with each scan update
  if Sender = fSavesMP then
    RefreshSaveList(False);
  if Sender = fMapsMP then
    RefreshMapList(False);
end;


procedure TKMMenuLobby.RefreshMapList(aJumpToSelected:Boolean);
var
  I, PrevTop: Integer;
  PrevMap: string;
  AddMap: Boolean;
begin
  //Remember previous map selected
  if DropCol_LobbyMaps.ItemIndex <> -1 then
    PrevMap := DropCol_LobbyMaps.Item[DropCol_LobbyMaps.ItemIndex].Cells[0].Caption
  else
    PrevMap := '';

  PrevTop := DropCol_LobbyMaps.List.TopIndex;
  DropCol_LobbyMaps.Clear;

  fMapsMP.Lock;
  try
    for I := 0 to fMapsMP.Count - 1 do
    begin
      //Different modes allow different maps
      case Radio_LobbyMapType.ItemIndex of
        0:    AddMap := (fMapsMP[I].MissionMode = mm_Normal) and not fMapsMP[I].IsCoop and not fMapsMP[I].IsSpecial; //BuildMap
        1:    AddMap := (fMapsMP[I].MissionMode = mm_Tactic) and not fMapsMP[I].IsCoop and not fMapsMP[I].IsSpecial; //FightMap
        2:    AddMap := fMapsMP[I].IsCoop; //CoopMap
        3:    AddMap := fMapsMP[I].IsSpecial; //Special map
        else  AddMap := False; //Other cases are already handled in Lobby_MapTypeSelect
      end;

      if AddMap then
        DropCol_LobbyMaps.Add(MakeListRow([fMapsMP[I].FileName,
                                           IntToStr(fMapsMP[I].HumanPlayerCount),
                                           fMapsMP[I].SizeText], I));
    end;
  finally
    fMapsMP.Unlock;
  end;

  //Restore previously selected map
  if PrevMap <> '' then
  for I := 0 to DropCol_LobbyMaps.Count - 1 do
  if DropCol_LobbyMaps.Item[I].Cells[0].Caption = PrevMap then
    DropCol_LobbyMaps.ItemIndex := I;

  //Restore the top index
  DropCol_LobbyMaps.List.TopIndex := PrevTop;
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


procedure TKMMenuLobby.RefreshSaveList(aJumpToSelected: Boolean);
var
  I, PrevTop: Integer;
  PrevSave: string;
begin
  //Remember previous save selected
  if DropCol_LobbyMaps.ItemIndex <> -1 then
    PrevSave := DropCol_LobbyMaps.Item[DropCol_LobbyMaps.ItemIndex].Cells[0].Caption
  else
    PrevSave := '';

  PrevTop := DropCol_LobbyMaps.List.TopIndex;

  DropCol_LobbyMaps.Clear;

  fSavesMP.Lock;
  try
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
  finally
    fSavesMP.Unlock;
  end;

  //Restore the top index
  DropCol_LobbyMaps.List.TopIndex := PrevTop;
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


procedure TKMMenuLobby.MapColumnClick(aValue: Integer);
var
  SM: TMapsSortMethod;
  SSM: TSavesSortMethod;
begin
  if Radio_LobbyMapType.ItemIndex < 4 then
  begin
    //Determine Sort method depending on which column user clicked
    with DropCol_LobbyMaps.List do
    case SortIndex of
      0:  if SortDirection = sdDown then
            SM := smByNameDesc
          else
            SM := smByNameAsc;
      1:  if SortDirection = sdDown then
            SM := smByHumanPlayersDesc
          else
            SM := smByHumanPlayersAsc;
      2:  if SortDirection = sdDown then
            SM := smBySizeDesc
          else
            SM := smBySizeAsc;
      else SM := smByNameAsc;
    end;
    fMapsMP.Sort(SM, MapList_SortUpdate);
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
    fSavesMP.Sort(SSM, MapList_SortUpdate);
  end;
end;


//Just pass FileName to Networking, it will check validity itself
procedure TKMMenuLobby.MapChange(Sender: TObject);
begin
  if Radio_LobbyMapType.ItemIndex < 4 then
  begin
    fMapsMP.Lock;
      fNetworking.SelectMap(fMapsMP[DropCol_LobbyMaps.Item[DropCol_LobbyMaps.ItemIndex].Tag].FileName);
    fMapsMP.Unlock;
  end
  else
  begin
    fSavesMP.Lock;
      fNetworking.SelectSave(fSavesMP[DropCol_LobbyMaps.Item[DropCol_LobbyMaps.ItemIndex].Tag].FileName);
    fSavesMP.Unlock;
  end;
end;


//We have received MapName
//Update UI to show it
procedure TKMMenuLobby.Lobby_OnMapName(const aData: UnicodeString);
var
  M: TKMapInfo;
  S: TKMSaveInfo;
begin
  //Common settings
  MinimapView_Lobby.Visible := (fNetworking.SelectGameKind = ngk_Map) and fNetworking.MapInfo.IsValid;
  TrackBar_LobbyPeacetime.Enabled := fNetworking.IsHost and (fNetworking.SelectGameKind = ngk_Map) and fNetworking.MapInfo.IsValid and not fNetworking.MapInfo.IsCoop;
  TrackBar_LobbySpeedPT.Enabled := (TrackBar_LobbyPeacetime.Position > 0) and fNetworking.IsHost and (fNetworking.SelectGameKind = ngk_Map) and fNetworking.MapInfo.IsValid;
  TrackBar_LobbySpeedAfterPT.Enabled := fNetworking.IsHost and (fNetworking.SelectGameKind = ngk_Map) and fNetworking.MapInfo.IsValid;
  CheckBox_LobbyRandomizeTeamLocations.Enabled := fNetworking.IsHost and (fNetworking.SelectGameKind <> ngk_Save);

  //In case it was hidden during file transfer
  Panel_LobbySetupTransfer.Hide;
  Panel_LobbySetupMinimap.Show;

  //Don't reset the selection if no map is selected
  if ((fNetworking.SelectGameKind = ngk_Map) and fNetworking.MapInfo.IsValid)
  or ((fNetworking.SelectGameKind = ngk_Save) and fNetworking.SaveInfo.IsValid) then
    Radio_LobbyMapType.ItemIndex := DetectMapType;

  case fNetworking.SelectGameKind of
    ngk_None: begin
                Memo_LobbyMapDesc.Clear;
                if aData = '' then
                  Label_LobbyMapName.Caption := gResTexts[TX_LOBBY_MAP_NONE]
                else
                begin
                  Label_LobbyMapName.Caption := fNetworking.MissingFileName;
                  Memo_LobbyMapDesc.Text := aData; //aData is some error message
                  if fNetworking.MissingFileType = ngk_Save then
                    Radio_LobbyMapType.ItemIndex := 4
                  else
                    Radio_LobbyMapType.ItemIndex := 0;
                  Panel_LobbySetupMinimap.Hide;
                  Panel_LobbySetupTransfer.Show;
                  Button_LobbySetupDownload.Show;
                  Lobby_OnFileTransferProgress(0, 0); //Reset progress bar
                  if fNetworking.MissingFileExists then
                  begin
                    CheckBox_LobbySetupOverwrite.Show;
                    CheckBox_LobbySetupOverwrite.Checked := False;
                    Button_LobbySetupDownload.Disable;
                    Button_LobbySetupDownload.Top := 20;
                  end
                  else
                  begin
                    CheckBox_LobbySetupOverwrite.Hide;
                    Button_LobbySetupDownload.Enable;
                    Button_LobbySetupDownload.Top := 0;
                  end;
                end;
              end;
    ngk_Save: begin
                S := fNetworking.SaveInfo;
                Label_LobbyMapName.Caption := S.FileName;
                Memo_LobbyMapDesc.Text := S.Info.GetTitleWithTime;
              end;
    ngk_Map:  begin
                M := fNetworking.MapInfo;

                //Only load the minimap preview if the map is valid
                if M.IsValid then
                begin
                  fMinimap.LoadFromMission(M.FullPath('.dat'), M.HumanUsableLocations);
                  fMinimap.Update(not M.IsCoop);
                  MinimapView_Lobby.SetMinimap(fMinimap);

                  if fNetworking.MapInfo.IsCoop then
                    TrackBar_LobbyPeacetime.Position := 0; //No peacetime in coop (trackbar gets disabled above)
                end;
                Label_LobbyMapName.Caption := M.FileName;
                Memo_LobbyMapDesc.Text := M.BigDesc;
            end;
  end;
end;


//We have been assigned to be the host of the game because the host disconnected. Reopen lobby page in correct mode.
procedure TKMMenuLobby.Lobby_OnReassignedToHost(Sender: TObject);
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
var
  OldMapType: Integer;
begin
  Reset(lpk_Host, True, True); //Will reset the lobby page into host mode, preserving messages/maps
  OldMapType := Radio_LobbyMapType.ItemIndex;

  //Pick correct position of map type selector
  Radio_LobbyMapType.ItemIndex := DetectMapType;

  //Don't force rescanning all the maps unless the map type changed or no map was selected
  if (Radio_LobbyMapType.ItemIndex <> OldMapType) or (DropCol_LobbyMaps.ItemIndex = -1) then
    MapTypeChange(nil)
  else
    RefreshMapList(False); //Just fill the list from fMapMP

  case fNetworking.SelectGameKind of
    ngk_Map:  SelectByName(fNetworking.MapInfo.FileName);
    ngk_Save: SelectByName(fNetworking.SaveInfo.FileName);
  end;

  Lobby_OnGameOptions(nil);

  case fNetworking.SelectGameKind of
    ngk_Map:  Lobby_OnMapName(fNetworking.MapInfo.FileName);
    ngk_Save: Lobby_OnMapName(fNetworking.SaveInfo.FileName);
  end;
end;


//Post what user has typed
procedure TKMMenuLobby.PostKeyDown(Sender: TObject; Key: Word);
var
  ChatMessage: UnicodeString;
begin
  if (Key <> VK_RETURN) or (Trim(Edit_LobbyPost.Text) = '') then Exit;

  ChatMessage := Edit_LobbyPost.Text;

  //Console commands are disabled for now, maybe we'll reuse them later
  //Check for console commands
  {if (Length(ChatMessage) > 1) and (ChatMessage[1] = '/')
  and (ChatMessage[2] <> '/') then //double slash is the escape to place a slash at the start of a sentence
    fNetworking.ConsoleCommand(ChatMessage)
  else
  begin
    if (Length(ChatMessage) > 1) and (ChatMessage[1] = '/') and (ChatMessage[2] = '/') then
      Delete(ChatMessage, 1, 1); //Remove one of the /'s
  end;}

  if fChatMode in [cmAll, cmTeam] then
    fNetworking.PostMessage(ChatMessage, True, fChatMode = cmTeam);
  if fChatMode = cmWhisper then
    fNetworking.PostMessage(ChatMessage, True, False, fChatWhisperRecipient);

  Edit_LobbyPost.Text := '';
end;


procedure TKMMenuLobby.Lobby_OnMessage(const aText: UnicodeString);
begin
  if fGameApp.GameSettings.FlashOnMessage then
    fMain.FlashingStart;

  Memo_LobbyPosts.Add(aText);
end;


//We were disconnected from Server. Either we were kicked, or connection broke down
procedure TKMMenuLobby.Lobby_OnDisconnect(const aData: UnicodeString);
begin
  fNetworking.Disconnect;
  gSoundPlayer.Play(sfxn_Error);

  fOnPageChange(gpMultiplayer, aData);
end;


procedure TKMMenuLobby.Lobby_OnFileTransferProgress(aTotal, aProgress: Cardinal);
begin
  if aTotal = 0 then
    PercentBar_LobbySetupProgress.Hide
  else
  begin
    PercentBar_LobbySetupProgress.Show;
    PercentBar_LobbySetupProgress.Position := aProgress / aTotal;
    PercentBar_LobbySetupProgress.Caption := IntToStr(aProgress div 1024) + 'kb / ' + IntToStr(aTotal div 1024) + 'kb';
  end;
end;


procedure TKMMenuLobby.StartClick(Sender: TObject);
begin
  if fNetworking.IsHost then
    fNetworking.StartClick
  else
  begin
    if fNetworking.ReadyToStart then
      Button_LobbyStart.Caption := gResTexts[TX_LOBBY_NOT_READY]
    else
      Button_LobbyStart.Caption := gResTexts[TX_LOBBY_READY];
  end;
end;


procedure TKMMenuLobby.SettingsClick(Sender: TObject);
begin
  if Sender = Button_LobbyChangeSettings then
  begin
    Edit_LobbyDescription.Text := fNetworking.Description;
    Edit_LobbyPassword.Text := UnicodeString(fNetworking.Password);
    Panel_LobbySettings.Show;
  end;

  if Sender = Button_LobbySettingsCancel then
  begin
    Panel_LobbySettings.Hide;
  end;

  if Sender = Button_LobbySettingsSave then
  begin
    Panel_LobbySettings.Hide;
    fNetworking.Description := Edit_LobbyDescription.Text;
    fNetworking.SetPassword(AnsiString(Edit_LobbyPassword.Text));
  end;
end;


//Should update anything we want to be updated, obviously
procedure TKMMenuLobby.UpdateState;
begin
  if fMapsMP <> nil then fMapsMP.UpdateState;
  if fSavesMP <> nil then fSavesMP.UpdateState;
end;


end.