unit KM_GUIMenuMultiplayer;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  StrUtils, SysUtils, KromUtils, KromOGLUtils, Math, Classes, Controls,
  KM_Controls, KM_Defaults, KM_Pics,
  KM_InterfaceDefaults, KM_ServerQuery;


type
  TKMMenuMultiplayer = class (TKMMenuPageCommon)
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class

    fServerSelected: Boolean;
    fSelectedRoomInfo: TKMRoomInfo;
    fSelectedServerInfo: TKMServerInfo;
    fLobbyBusy: Boolean;

    procedure MP_Init;
    procedure MP_SaveSettings;
    procedure MP_Update(const aStatus: string; aColor: TColor4; aBusy: Boolean);
    procedure MP_ServersUpdateList(Sender: TObject);
    procedure MP_AnnouncementsUpdated(const S: UnicodeString);
    procedure MP_CreateServerClick(Sender: TObject);
    procedure MP_FindServerClick(Sender: TObject);
    procedure MP_CreateServerCancelClick(Sender: TObject);
    procedure MP_FindServerIPClick(Sender: TObject);
    procedure MP_PasswordClick(Sender: TObject);
    procedure MP_FindServerCancelClick(Sender: TObject);
    procedure MP_ClearServerDetailsPanel;
    procedure MP_ServersRefresh(Sender: TObject);
    procedure MP_ServersSort(aIndex: Integer);
    procedure MP_ServersClick(Sender: TObject);
    procedure MP_ServersDoubleClick(Sender: TObject);
    procedure MP_GetInClick(Sender: TObject);
    function MP_GetInEnabled: Boolean;
    procedure MP_Join(const aServerAddress: string; aPort: Word; aRoom: Integer);
    procedure MP_JoinPassword(Sender: TObject);
    procedure MP_JoinSuccess(Sender: TObject);
    procedure MP_JoinFail(const aData: UnicodeString);
    procedure MP_JoinAssignedHost(Sender: TObject);
    procedure MP_HostClick(Sender: TObject);
    procedure MP_HostFail(const aData: UnicodeString);
    procedure BackClick(Sender: TObject);
    function ValidatePlayerName(const aName: UnicodeString): Boolean;
    procedure EscKeyDown(Sender: TObject);
    procedure KeyDown(Key: Word; Shift: TShiftState);
    procedure UpdateServerDetailsUI;
  protected
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
        Label_MP_ServerDetails_Header, Label_MP_GameInfo_Header, Label_MP_Map_Header,
        Label_MP_PlayerList_Header, Label_MP_Team_Header,
        Label_MP_Desc, Label_MP_PT_Times, Label_MP_GameTime, Label_MP_MapName: TKMLabel;
        Label_MP_PlayersNames: array[1..MAX_LOBBY_SLOTS] of TKMLabel;
        Label_MP_PlayersTeams: array[1..MAX_LOBBY_SLOTS] of TKMLabel;
        Image_MP_PlayerIcons: array[1..MAX_LOBBY_SLOTS] of TKMImage;
        Image_MP_PlayerSpecIcons: array[1..MAX_LOBBY_SLOTS] of TKMImage;
        Image_MP_Host: TKMImage;

      //PopUps
      Panel_MPCreateServer: TKMPanel;
        Edit_MP_ServerName: TKMEdit;
        Edit_MP_ServerPort: TKMEdit;
        Button_MP_CreateLAN: TKMButton;
        Button_MP_CreateWAN: TKMButton;
        Button_MP_CreateServerCancel: TKMButton;

      Panel_MPFindServer: TKMPanel;
        Button_MP_FindServerIP: TKMButton;
        Button_MP_FindCancel: TKMButton;
        Edit_MP_FindIP: TKMEdit;
        Edit_MP_FindPort: TKMEdit;
        Edit_MP_FindRoom: TKMEdit;

      Panel_MPPassword: TKMPanel;
        Edit_MP_Password: TKMEdit;
        Button_MP_PasswordOk: TKMButton;
        Button_MP_PasswordCancel: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);

    procedure Show(const aText: UnicodeString);
    procedure Resize(X, Y: Word);
  end;


implementation
uses
  KM_Main, KM_NetworkTypes, KM_ResTexts, KM_GameApp, KM_ResLocales,
  KM_CommonUtils, KM_Sound, KM_ResSound, KM_RenderUI, KM_ResFonts, KM_Resource;


const
  MAX_NIKNAME_LENGTH = 16;


{ TKMGUIMainMultiplayer }
constructor TKMMenuMultiplayer.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
  procedure CreateServerPopUp;
  begin
    Panel_MPCreateServer := TKMPanel.Create(aParent, 362, 250, 320, 300);
    Panel_MPCreateServer.AnchorsCenter;
      TKMBevel.Create(Panel_MPCreateServer, -1000,  -1000, 4000, 4000);
      TKMImage.Create(Panel_MPCreateServer, -20, -75, 340, 310, 15, rxGuiMain);
      TKMBevel.Create(Panel_MPCreateServer,   0,  0, 320, 300);
      TKMLabel.Create(Panel_MPCreateServer, 20, 10, 280, 20, gResTexts[TX_MP_MENU_CREATE_SERVER_HEADER], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_MPCreateServer, 20, 50, 280, 20, gResTexts[TX_MP_MENU_CREATE_SERVER_NAME], fnt_Outline, taLeft);
      Edit_MP_ServerName := TKMEdit.Create(Panel_MPCreateServer, 20, 70, 280, 20, fnt_Grey);
      Edit_MP_ServerName.AllowedChars := acANSI7;
      TKMLabel.Create(Panel_MPCreateServer, 20, 100, 284, 20, gResTexts[TX_MP_MENU_CREATE_SERVER_PORT], fnt_Outline, taLeft);
      Edit_MP_ServerPort := TKMEdit.Create(Panel_MPCreateServer, 20, 120, 100, 20, fnt_Grey);
      Edit_MP_ServerPort.AllowedChars := acDigits;
      Button_MP_CreateLAN  := TKMButton.Create(Panel_MPCreateServer, 20, 170, 280, 30, gResTexts[TX_MP_MENU_CREATE_SERVER_LOCAL],  bsMenu);
      Button_MP_CreateWAN  := TKMButton.Create(Panel_MPCreateServer, 20, 210, 280, 30, gResTexts[TX_MP_MENU_CREATE_SERVER_INTERNET],  bsMenu);
      Button_MP_CreateServerCancel := TKMButton.Create(Panel_MPCreateServer, 20, 250, 280, 30, gResTexts[TX_MP_MENU_CREATE_SERVER_CANCEL],  bsMenu);
      Button_MP_CreateLAN.OnClick := MP_HostClick;
      Button_MP_CreateWAN.OnClick := MP_HostClick;
      Button_MP_CreateServerCancel.OnClick := MP_CreateServerCancelClick;
  end;
  procedure FindServerPopUp;
  begin
    Panel_MPFindServer := TKMPanel.Create(aParent, 362, 250, 320, 300);
    Panel_MPFindServer.AnchorsCenter;
      TKMBevel.Create(Panel_MPFindServer, -1000,  -1000, 4000, 4000);
      TKMImage.Create(Panel_MPFindServer, -20, -75, 340, 310, 15, rxGuiMain);
      TKMBevel.Create(Panel_MPFindServer,   0,  0, 320, 300);
      TKMLabel.Create(Panel_MPFindServer,  20, 10, 280, 20, gResTexts[TX_MP_MENU_FIND_SERVER_HEADER], fnt_Outline, taCenter);

      TKMLabel.Create(Panel_MPFindServer, 20, 50, 156, 20, gResTexts[TX_MP_MENU_FIND_SERVER_ADDRESS], fnt_Outline, taLeft);
      Edit_MP_FindIP := TKMEdit.Create(Panel_MPFindServer, 20, 70, 152, 20, fnt_Grey);
      Edit_MP_FindIP.AllowedChars := acText; //Server name could be "localhost"
      TKMLabel.Create(Panel_MPFindServer, 172, 50, 60, 20, gResTexts[TX_MP_MENU_FIND_SERVER_PORT], fnt_Outline, taLeft);
      Edit_MP_FindPort := TKMEdit.Create(Panel_MPFindServer, 172, 70, 60, 20, fnt_Grey);
      Edit_MP_FindPort.AllowedChars := acDigits;
      TKMLabel.Create(Panel_MPFindServer, 232, 50, 60, 20, gResTexts[TX_MP_MENU_FIND_SERVER_ROOM], fnt_Outline, taLeft);
      Edit_MP_FindRoom := TKMEdit.Create(Panel_MPFindServer, 232, 70, 60, 20, fnt_Grey);
      Edit_MP_FindRoom.AllowedChars := acDigits;
      Button_MP_FindServerIP := TKMButton.Create(Panel_MPFindServer, 20, 110, 280, 30, gResTexts[TX_MP_MENU_FIND_SERVER_FIND], bsMenu);
      Button_MP_FindServerIP.OnClick := MP_FindServerIPClick;
      Button_MP_FindCancel := TKMButton.Create(Panel_MPFindServer, 20, 150, 280, 30, gResTexts[TX_MP_MENU_FIND_SERVER_CANCEL], bsMenu);
      Button_MP_FindCancel.OnClick := MP_FindServerCancelClick;
  end;
  procedure PasswordPopUp;
  begin
    Panel_MPPassword := TKMPanel.Create(aParent, 362, 250, 320, 300);
    Panel_MPPassword.AnchorsCenter;
      TKMBevel.Create(Panel_MPPassword, -1000,  -1000, 4000, 4000);
      TKMImage.Create(Panel_MPPassword, -20, -75, 340, 310, 15, rxGuiMain);
      TKMBevel.Create(Panel_MPPassword,   0,  0, 320, 300);
      TKMLabel.Create(Panel_MPPassword,  20, 10, 280, 20, gResTexts[TX_MP_MENU_PASSWORD_HEADER], fnt_Outline, taCenter);

      TKMLabel.Create(Panel_MPPassword, 20, 50, 156, 20, gResTexts[TX_MP_MENU_PASSWORD], fnt_Outline, taLeft);
      Edit_MP_Password := TKMEdit.Create(Panel_MPPassword, 20, 70, 152, 20, fnt_Grey);
      Edit_MP_Password.AllowedChars := acANSI7; //Passwords are basic ANSI so everyone can type them
      Button_MP_PasswordOk := TKMButton.Create(Panel_MPPassword, 20, 110, 280, 30, gResTexts[TX_MP_MENU_SERVER_JOIN], bsMenu);
      Button_MP_PasswordOk.OnClick := MP_PasswordClick;
      Button_MP_PasswordCancel := TKMButton.Create(Panel_MPPassword, 20, 150, 280, 30, gResTexts[TX_MP_MENU_FIND_SERVER_CANCEL], bsMenu);
      Button_MP_PasswordCancel.OnClick := MP_PasswordClick;
  end;
var
  I: Integer;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;
  OnEscKeyDown := EscKeyDown;
  OnKeyDown := KeyDown;

  Panel_MultiPlayer := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_MultiPlayer.AnchorsStretch;

    //Top area
    Panel_MPPlayerName := TKMPanel.Create(Panel_MultiPlayer, 675, 45, 320, 120);
      TKMBevel.Create(Panel_MPPlayerName, 0, 0, 320, 120);
      TKMLabel.Create(Panel_MPPlayerName, 8, 10, 304, 20, gResTexts[TX_MP_MENU_PLAYERNAME], fnt_Outline, taLeft);
      Edit_MP_PlayerName := TKMEdit.Create(Panel_MPPlayerName, 8, 30, 140, 20, fnt_Grey);
      Edit_MP_PlayerName.MaxLen := MAX_NIKNAME_LENGTH;
      Edit_MP_PlayerName.AllowedChars := acANSI7;
      TKMLabel.Create(Panel_MPPlayerName, 8, 60, 304, 20, gResTexts[TX_MP_MENU_STATUS], fnt_Outline, taLeft);
      Label_MP_Status := TKMLabel.Create(Panel_MPPlayerName, 8, 80, 304, 36, '', fnt_Grey, taLeft);
      Label_MP_Status.AutoWrap := True;

    Button_MP_CreateServer := TKMButton.Create(Panel_MultiPlayer, 675, 170, 320, 30, gResTexts[TX_MP_MENU_CREATE_SERVER], bsMenu);
    Button_MP_CreateServer.OnClick := MP_CreateServerClick;


    Button_MP_FindServer := TKMButton.Create(Panel_MultiPlayer, 675, 204, 320, 30, gResTexts[TX_MP_MENU_FIND_SERVER], bsMenu);
    Button_MP_FindServer.OnClick := MP_FindServerClick;

    //Master server announcement
    Memo_MP_Announcement := TKMMemo.Create(Panel_MultiPlayer, 45, 45, 620, 189, fnt_Grey, bsMenu);
    Memo_MP_Announcement.Anchors := [anLeft, anTop];
    Memo_MP_Announcement.AutoWrap := True;
    Memo_MP_Announcement.ItemHeight := 16;

    //List of available servers
    ColumnBox_Servers := TKMColumnBox.Create(Panel_MultiPlayer,45,240,620,465,fnt_Metal, bsMenu);
    ColumnBox_Servers.Anchors := [anLeft, anTop, anBottom];
    ColumnBox_Servers.Focusable := True;
    ColumnBox_Servers.SetColumns(fnt_Outline, ['','',gResTexts[TX_MP_MENU_SERVERLIST_NAME],gResTexts[TX_MP_MENU_SERVERLIST_STATE],gResTexts[TX_MP_MENU_SERVERLIST_PLAYERS],gResTexts[TX_MP_MENU_SERVERLIST_PING]],[0,20,40,300,430,525]);
    ColumnBox_Servers.OnColumnClick := MP_ServersSort;
    ColumnBox_Servers.OnChange := MP_ServersClick;
    ColumnBox_Servers.OnDoubleClick := MP_ServersDoubleClick;
    Label_Servers_Status := TKMLabel.Create(Panel_MultiPlayer, 45+310, 240+230, '', fnt_Grey, taCenter);
    Label_Servers_Status.Anchors := [anLeft];
    Label_Servers_Status.Hide;

    //Server details area
    Panel_MPServerDetails := TKMPanel.Create(Panel_MultiPlayer, 675, 240, 320, 465);
    Panel_MPServerDetails.Anchors := [anLeft, anTop, anBottom];
      with TKMBevel.Create(Panel_MPServerDetails, 0, 0, 320, 465) do AnchorsStretch;
      Label_MP_ServerDetails_Header := TKMLabel.Create(Panel_MPServerDetails, 8, 6, 304, 20, gResTexts[TX_MP_MENU_HEADER_SERVER_DETAILS], fnt_Outline, taCenter);
      Label_MP_GameInfo_Header := TKMLabel.Create(Panel_MPServerDetails, 8, 30, 304, 20, gResTexts[TX_MP_MENU_GAME_INFORMATION], fnt_Outline, taLeft);
      Label_MP_Desc := TKMLabel.Create(Panel_MPServerDetails, 8, 50, 304, 40, '', fnt_Metal, taLeft);
      Label_MP_Desc.AutoWrap := True;
      Label_MP_Desc.AutoCut := True;  //Automatically cut text, if it's too long
      Label_MP_PT_Times := TKMLabel.Create(Panel_MPServerDetails, 8, 90, 304, 20, '', fnt_Metal, taLeft);
      Label_MP_PT_Times.FontColor := clMPSrvDetailsGameInfoFont;
      Label_MP_GameTime := TKMLabel.Create(Panel_MPServerDetails, 8, 90, 304, 20, '', fnt_Metal, taRight);
      Label_MP_GameTime.FontColor := clMPSrvDetailsGameInfoFont;
      Label_MP_Map_Header := TKMLabel.Create(Panel_MPServerDetails, 8, 110, 304, 20, 'Map:', fnt_Outline, taLeft); //Todo: translate
      Label_MP_MapName := TKMLabel.Create(Panel_MPServerDetails, 8, 130, 304, 20, '', fnt_Metal, taLeft);
      Label_MP_PlayerList_Header := TKMLabel.Create(Panel_MPServerDetails, 8, 150, 304, 20, gResTexts[TX_MP_MENU_PLAYER_LIST], fnt_Outline, taLeft);

      Label_MP_Team_Header := TKMLabel.Create(Panel_MPServerDetails, 8 + 22 + 156, 150, 150, 20, 'Team', fnt_Outline, taLeft);
      Label_MP_Team_Header.Visible := False;

      Image_MP_Host := TKMImage.Create(Panel_MPServerDetails, 8 + 22 + 156 + 35, 148, 14, 15, 77, rxGuiMain);
      Image_MP_Host.Visible := False;
      for I := 1 to MAX_LOBBY_SLOTS do
      begin
        Label_MP_PlayersNames[I] := TKMLabel.Create(Panel_MPServerDetails, 8 + 22 + 156*((I-1) div 8), 170 + 20*((I-1) mod 8), 130, 20, '', fnt_Metal, taLeft);
        Label_MP_PlayersNames[I].Anchors := [anLeft, anTop, anBottom];
        Label_MP_PlayersTeams[I] := TKMLabel.Create(Panel_MPServerDetails, 8 + 22 + 166, 170 + 20*(I-1), 20, 20, '', fnt_Metal, taLeft);
        Image_MP_PlayerIcons[I] := TKMImage.Create(Panel_MPServerDetails, 8, 170 + 20*(I-1), 16, 11, 0, rxGuiMain);
        Image_MP_PlayerSpecIcons[I] := TKMImage.Create(Panel_MPServerDetails, 8 + 22 + 160, 170 + 20*(I-1), 16, 11, 0, rxGuiMain);
        Image_MP_PlayerSpecIcons[I].Visible := False;
      end;

    Button_MP_Back    := TKMButton.Create(Panel_MultiPlayer,  45, 720, 220, 30, gResTexts[TX_MENU_BACK], bsMenu);
    Button_MP_Refresh := TKMButton.Create(Panel_MultiPlayer, 275, 720, 390, 30,gResTexts[TX_MP_MENU_REFRESH_SERVER_LIST], bsMenu);
    Button_MP_GetIn   := TKMButton.Create(Panel_MultiPlayer, 675, 720, 320, 30,gResTexts[TX_MP_MENU_SERVER_JOIN],  bsMenu);
    Button_MP_Back.Anchors    := [anLeft, anBottom];
    Button_MP_Refresh.Anchors := [anLeft, anBottom];
    Button_MP_GetIn.Anchors   := [anLeft, anBottom];
    Button_MP_Back.OnClick    := BackClick;
    Button_MP_Refresh.OnClick := MP_ServersRefresh;
    Button_MP_GetIn.OnClick   := MP_GetInClick;

  CreateServerPopUp;
  FindServerPopUp;
  PasswordPopUp;
end;


procedure TKMMenuMultiplayer.KeyDown(Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:  if Panel_MPPassword.Visible then
                  MP_PasswordClick(Button_MP_PasswordOk)
                else if Panel_MPFindServer.Visible then
                  MP_FindServerIPClick(Button_MP_FindServerIP);
    // Refresh server list on F5
    VK_F5:      if not Panel_MPPassword.Visible
                  and not Panel_MPCreateServer.Visible
                  and not Panel_MPFindServer.Visible
                  and Button_MP_Refresh.IsClickable then
                  MP_ServersRefresh(Button_MP_Refresh);
  end;
end;


procedure TKMMenuMultiplayer.UpdateServerDetailsUI;
var
  Rows, I, PlayersCnt: Integer;
  ShowExtraInfo: Boolean;
begin
  //How many rows could fit
  Rows := (Panel_MPServerDetails.Height - Label_MP_PlayersNames[1].Top) div 20;
  if fServerSelected and (fSelectedRoomInfo.GameInfo <> nil) then
    PlayersCnt := fSelectedRoomInfo.GameInfo.PlayerCount
  else
    PlayersCnt := MAX_LOBBY_SLOTS;

  ShowExtraInfo := Rows >= PlayersCnt;
  for I := 1 to MAX_LOBBY_SLOTS do
  begin
    Label_MP_PlayersNames[I].Left := 8 + 22*Byte(Rows >= PlayersCnt) + 156*((I-1) div Rows);
    Label_MP_PlayersNames[I].Top := 170 + 20*((I-1) mod Rows);
    Label_MP_PlayersNames[I].Width := IfThen(ShowExtraInfo, 304 - 22, 150);

    // Show team section and icons only when player list has 1 column
    Image_MP_PlayerIcons[I].Visible := ShowExtraInfo;
    Label_MP_PlayersTeams[I].Visible := ShowExtraInfo;

    Image_MP_PlayerSpecIcons[I].Visible := ShowExtraInfo;
  end;
  Label_MP_Team_Header.Visible := ShowExtraInfo;
  Image_MP_Host.Visible := ShowExtraInfo;
end;


procedure TKMMenuMultiplayer.Resize(X, Y: Word);
begin
  UpdateServerDetailsUI;
end;


procedure TKMMenuMultiplayer.MP_Init;
begin
  fServerSelected := False;

  //Refresh the list when they first open the multiplayer page
  MP_ServersRefresh(nil);

  Edit_MP_PlayerName.Text := UnicodeString(gGameApp.GameSettings.MultiplayerName);

  Edit_MP_ServerName.Text := UnicodeString(gGameApp.GameSettings.ServerName);
  Edit_MP_ServerPort.Text := gGameApp.GameSettings.ServerPort;

  Edit_MP_FindIP.Text := gGameApp.GameSettings.LastIP;
  Edit_MP_FindPort.Text := gGameApp.GameSettings.LastPort;
  Edit_MP_FindRoom.Text := gGameApp.GameSettings.LastRoom;

  Button_MP_GetIn.Disable;

  //Fetch the announcements display
  gGameApp.Networking.ServerQuery.OnAnnouncements := MP_AnnouncementsUpdated;
  gGameApp.Networking.ServerQuery.FetchAnnouncements(gResLocales.UserLocale);
  Memo_MP_Announcement.Clear;
  Memo_MP_Announcement.Add(gResTexts[TX_MP_MENU_LOADING_ANNOUNCEMENTS]);
end;


procedure TKMMenuMultiplayer.MP_CreateServerCancelClick(Sender: TObject);
begin
  Panel_MPCreateServer.Hide;
end;


procedure TKMMenuMultiplayer.MP_CreateServerClick(Sender: TObject);
begin
  Panel_MPCreateServer.Show;
end;


procedure TKMMenuMultiplayer.MP_FindServerCancelClick(Sender: TObject);
begin
  Panel_MPFindServer.Hide;
end;


procedure TKMMenuMultiplayer.MP_FindServerClick(Sender: TObject);
begin
  Panel_MPFindServer.Show;
end;


procedure TKMMenuMultiplayer.MP_FindServerIPClick(Sender: TObject);
var
  serverPortStr: string;
  serverPort: Word;
begin
  serverPortStr := Trim(Edit_MP_FindPort.Text);
  serverPort    := StrToInt(serverPortStr);
  MP_Join(Edit_MP_FindIP.Text, serverPort, StrToIntDef(Edit_MP_FindRoom.Text, -1));
end;


procedure TKMMenuMultiplayer.MP_PasswordClick(Sender: TObject);
begin
  if Sender = Button_MP_PasswordOk then
  begin
    Panel_MPPassword.Hide;
    gGameApp.Networking.SendPassword(AnsiString(Edit_MP_Password.Text));
  end;
  if Sender = Button_MP_PasswordCancel then
  begin
    gGameApp.Networking.Disconnect;
    Panel_MPPassword.Hide;
    MP_Update(gResTexts[TX_MP_MENU_STATUS_READY], icGreen, False);
  end;
end;


//Save the Player and IP name so it is not lost inbetween activities
procedure TKMMenuMultiplayer.MP_SaveSettings;
begin
  //Player name
  gGameApp.GameSettings.MultiplayerName := AnsiString(Edit_MP_PlayerName.Text);

  //Create Server popup
  gGameApp.GameSettings.ServerName := AnsiString(Edit_MP_ServerName.Text);
  gGameApp.GameSettings.ServerPort := Edit_MP_ServerPort.Text;

  //Join server popup
  gGameApp.GameSettings.LastPort := Edit_MP_FindPort.Text;
  gGameApp.GameSettings.LastRoom := Edit_MP_FindRoom.Text;
  gGameApp.GameSettings.LastIP   := Edit_MP_FindIP.Text;
end;


//Update status line
//When user tries to Join some server disable joining controls for that time
procedure TKMMenuMultiplayer.MP_Update(const aStatus: string; aColor: TColor4; aBusy: Boolean);
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


procedure TKMMenuMultiplayer.MP_ClearServerDetailsPanel;
var I: Integer;
begin
  Label_MP_ServerDetails_Header.Visible := False;
  Label_MP_GameInfo_Header.Visible := False;
  Label_MP_Map_Header.Visible := False;
  Label_MP_PlayerList_Header.Visible := False;
  Label_MP_Team_Header.Visible := False;
  Image_MP_Host.Visible := False;
  Label_MP_Desc.Caption := '';
  Label_MP_PT_Times.Caption := '';
  Label_MP_GameTime.Caption := '';
  Label_MP_MapName.Caption := '';
  for I := 1 to MAX_LOBBY_SLOTS do
  begin
    Label_MP_PlayersNames[I].Caption := '';
    Label_MP_PlayersNames[I].Strikethrough := False;
    Label_MP_PlayersTeams[I].Caption := '';
    Label_MP_PlayersTeams[I].Strikethrough := False;
    Label_MP_PlayersTeams[I].Visible := False;
    Image_MP_PlayerSpecIcons[I].Visible := False;
    Image_MP_PlayerIcons[I].TexID := 0;
    Image_MP_PlayerIcons[I].Lightness := 0;
  end;
end;


procedure TKMMenuMultiplayer.MP_ServersRefresh(Sender: TObject);
begin
  gGameApp.Networking.ServerQuery.OnListUpdated := MP_ServersUpdateList;
  gGameApp.Networking.ServerQuery.RefreshList;
  ColumnBox_Servers.Clear;
  MP_ClearServerDetailsPanel;

  //Do not use 'Show' here as it will also make the parent panel visible
  //which could be already hidden if player switched pages
  Label_Servers_Status.Caption := gResTexts[TX_MP_MENU_REFRESHING];
  Label_Servers_Status.Visible := True;
  Button_MP_GetIn.Disable;
end;


//Refresh the display for the list of servers
procedure TKMMenuMultiplayer.MP_ServersUpdateList(Sender: TObject);
const
  GameStateTextIDs: array [TMPGameState] of Integer = (TX_MP_STATE_NONE, TX_MP_STATE_LOBBY, TX_MP_STATE_LOADING, TX_MP_STATE_GAME);
var
  I, PrevTop: Integer;
  DisplayName: string;
  S: TKMServerInfo;
  R: TKMRoomInfo;
begin
  PrevTop := ColumnBox_Servers.TopIndex;
  ColumnBox_Servers.Clear;

  if gGameApp.Networking.ServerQuery.Rooms.Count = 0 then
  begin
    //Do not use 'Show' here as it will also make the parent panel visible
    //which could be already hidden if player switched pages
    Label_Servers_Status.Caption := gResTexts[TX_MP_MENU_NO_SERVERS];
    Label_Servers_Status.Visible := True;
  end
  else
  begin
    Label_Servers_Status.Hide;
    for I := 0 to gGameApp.Networking.ServerQuery.Rooms.Count - 1 do
    begin
      R := gGameApp.Networking.ServerQuery.Rooms[I];
      S := gGameApp.Networking.ServerQuery.Servers[R.ServerIndex];

      //Only show # if Server has more than 1 Room
      DisplayName := IfThen(R.OnlyRoom, S.Name, S.Name + ' #' + IntToStr(R.RoomID + 1));
      ColumnBox_Servers.AddItem(
      MakeListRow(['', '', DisplayName, gResTexts[GameStateTextIDs[R.GameInfo.GameState]], IntToStr(R.GameInfo.ConnectedPlayerCount), IntToStr(S.Ping)],
                  [$FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, GetPingColor(S.Ping)],
                  [MakePic(rxGuiMain, ServerTypePic[S.ServerType]), MakePic(rxGuiMain, IfThen(R.GameInfo.PasswordLocked, 73, 0)), MakePic(rxGuiMain,0), MakePic(rxGuiMain,0), MakePic(rxGuiMain,0), MakePic(rxGuiMain,0)],
                  I));

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

    ColumnBox_Servers.TopIndex := PrevTop;
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


procedure TKMMenuMultiplayer.MP_AnnouncementsUpdated(const S: UnicodeString);
begin
  Memo_MP_Announcement.Clear;
  Memo_MP_Announcement.Add(S);
end;


//Sort the servers list by said column ID
procedure TKMMenuMultiplayer.MP_ServersSort(aIndex: Integer);
begin
  case ColumnBox_Servers.SortIndex of
    0:  if ColumnBox_Servers.SortDirection = sdDown then
          gGameApp.Networking.ServerQuery.SortMethod := ssmByTypeAsc
        else
          gGameApp.Networking.ServerQuery.SortMethod := ssmByTypeDesc;
    1:  if ColumnBox_Servers.SortDirection = sdDown then
          gGameApp.Networking.ServerQuery.SortMethod := ssmByPasswordAsc
        else
          gGameApp.Networking.ServerQuery.SortMethod := ssmByPasswordDesc;
    //Sorting by name goes A..Z by default
    2:  if ColumnBox_Servers.SortDirection = sdDown then
          gGameApp.Networking.ServerQuery.SortMethod := ssmByNameAsc
        else
          gGameApp.Networking.ServerQuery.SortMethod := ssmByNameDesc;
    //Sorting by state goes Lobby,Loading,Game,None by default
    3:  if ColumnBox_Servers.SortDirection = sdDown then
          gGameApp.Networking.ServerQuery.SortMethod := ssmByStateAsc
        else
          gGameApp.Networking.ServerQuery.SortMethod := ssmByStateDesc;
    //Sorting by player count goes 8..0 by default
    4:  if ColumnBox_Servers.SortDirection = sdDown then
          gGameApp.Networking.ServerQuery.SortMethod := ssmByPlayersDesc
        else
          gGameApp.Networking.ServerQuery.SortMethod := ssmByPlayersAsc;
    //Sorting by ping goes 0 ... 1000 by default
    5:  if ColumnBox_Servers.SortDirection = sdDown then
          gGameApp.Networking.ServerQuery.SortMethod := ssmByPingAsc
        else
          gGameApp.Networking.ServerQuery.SortMethod := ssmByPingDesc;
  end;

  //Refresh the display only if there are rooms to be sorted (otherwise it shows "no servers found" immediately)
  if gGameApp.Networking.ServerQuery.Rooms.Count > 0 then
    MP_ServersUpdateList(nil);
end;


procedure TKMMenuMultiplayer.MP_ServersClick(Sender: TObject);
var SortedNetPlayersIndexes: array [1..MAX_LOBBY_SLOTS] of Integer;
  function GetTeamStr(aTeam: Integer; aIsSpectator: Boolean): String;
  begin
    if aIsSpectator then
      Result := ''
    else if aTeam = 0 then
      Result := '-'
    else
      Result := IntToStr(aTeam);
  end;

  procedure SortPlayersByTeam;
  var
    I, K, T: Integer;
  begin
    // First empty everything
    for I := 1 to MAX_LOBBY_SLOTS do
      SortedNetPlayersIndexes[I] := -1;

    K := 1;
    // Players, sorted by team
    for T := 0 to MAX_TEAMS do
      for I := 1 to fSelectedRoomInfo.GameInfo.PlayerCount do
        if not fSelectedRoomInfo.GameInfo.Players[I].IsSpectator and (fSelectedRoomInfo.GameInfo.Players[I].Team = T) then
        begin
          SortedNetPlayersIndexes[K] := I;
          Inc(K);
        end;

    // Spectators
    for I := 1 to fSelectedRoomInfo.GameInfo.PlayerCount do
      if fSelectedRoomInfo.GameInfo.Players[I].IsSpectator then
      begin
        SortedNetPlayersIndexes[K] := I;
        Inc(K);
      end;
  end;

var K, I, ID, LocaleID: Integer;
begin
  ID := ColumnBox_Servers.ItemIndex;
  if (ID = -1) or (ColumnBox_Servers.Rows[ID].Tag = -1) then
  begin
    fServerSelected := False;
    Button_MP_GetIn.Disable;
    MP_ClearServerDetailsPanel;
    Exit;
  end;

  fServerSelected := True;
  Button_MP_GetIn.Enabled := MP_GetInEnabled;

  fSelectedRoomInfo := gGameApp.Networking.ServerQuery.Rooms[ColumnBox_Servers.Rows[ID].Tag];
  fSelectedServerInfo := gGameApp.Networking.ServerQuery.Servers[fSelectedRoomInfo.ServerIndex];

  if fSelectedRoomInfo.GameInfo.PlayerCount = 0 then
  begin
    MP_ClearServerDetailsPanel;
    Exit;
  end;

  Label_MP_ServerDetails_Header.Visible := True;
  Label_MP_GameInfo_Header.Visible := True;
  Label_MP_Map_Header.Visible := True;
  Label_MP_PlayerList_Header.Visible := True;

  Label_MP_Desc.Caption := fSelectedRoomInfo.GameInfo.Description;

  //Game options (Peacetime duration, speed before and after PT)
  Label_MP_PT_Times.Caption := IntToStr(fSelectedRoomInfo.GameInfo.GameOptions.Peacetime) + 'pt' +
                               ' x' + FormatFloat('#.#', fSelectedRoomInfo.GameInfo.GameOptions.SpeedPT) +
                               ' x' + FormatFloat('#.#', fSelectedRoomInfo.GameInfo.GameOptions.SpeedAfterPT);
  Label_MP_GameTime.Caption := fSelectedRoomInfo.GameInfo.GetFormattedTime;
  Label_MP_MapName.Caption := fSelectedRoomInfo.GameInfo.Map;

  SortPlayersByTeam;

  for I := 1 to MAX_LOBBY_SLOTS do
    if I <= fSelectedRoomInfo.GameInfo.PlayerCount then
    begin
      K := SortedNetPlayersIndexes[I];
      if K = -1 then raise Exception.Create('Unexpected sorted value'); ;
      case fSelectedRoomInfo.GameInfo.Players[K].PlayerType of
        nptHuman:     begin
                        Label_MP_PlayersNames[I].Caption := UnicodeString(fSelectedRoomInfo.GameInfo.Players[K].Name);
                        Label_MP_PlayersTeams[I].Caption := GetTeamStr(fSelectedRoomInfo.GameInfo.Players[K].Team, fSelectedRoomInfo.GameInfo.Players[K].IsSpectator);
                        Image_MP_PlayerSpecIcons[I].TexId := IfThen(fSelectedRoomInfo.GameInfo.Players[K].IsSpectator, 86, 0); //spectator eye icon
                        if fSelectedRoomInfo.GameInfo.Players[K].IsHost then
                          Image_MP_Host.Top := Label_MP_PlayersNames[1].Top + 20*(I-1) - 2;
                        LocaleID := gResLocales.IndexByCode(fSelectedRoomInfo.GameInfo.Players[K].LangCode);
                        if LocaleID <> -1 then
                          Image_MP_PlayerIcons[I].TexID := gResLocales[LocaleID].FlagSpriteID
                        else
                          Image_MP_PlayerIcons[I].TexID := 0;
                      end;
        nptComputer:  begin
                        Label_MP_PlayersNames[I].Caption := gResTexts[TX_LOBBY_SLOT_AI_PLAYER];
                        Label_MP_PlayersTeams[I].Caption := GetTeamStr(fSelectedRoomInfo.GameInfo.Players[K].Team, fSelectedRoomInfo.GameInfo.Players[K].IsSpectator);
                        Image_MP_PlayerSpecIcons[I].TexId := 0;
                        Image_MP_PlayerIcons[I].TexID := 62; //PC Icon
                      end;
        nptClosed:    begin
                        Label_MP_PlayersNames[I].Caption := gResTexts[TX_LOBBY_SLOT_CLOSED];
                        Label_MP_PlayersTeams[I].Caption := '';
                        Image_MP_PlayerSpecIcons[I].TexId := 0;
                        Image_MP_PlayerIcons[I].TexID := 0;
                      end;
      end;
      Label_MP_PlayersNames[I].FontColor := FlagColorToTextColor(fSelectedRoomInfo.GameInfo.Players[K].Color);
      Label_MP_PlayersNames[I].Strikethrough := not fSelectedRoomInfo.GameInfo.Players[K].Connected;
      Image_MP_PlayerIcons[I].Lightness := IfThen(fSelectedRoomInfo.GameInfo.Players[K].Connected, 0, -0.66);
    end
    else
    begin
      Label_MP_PlayersNames[I].Caption := '';
      Label_MP_PlayersNames[I].Strikethrough := False;
      Label_MP_PlayersTeams[I].Caption := '';
      Label_MP_PlayersTeams[I].Strikethrough := False;
      Image_MP_PlayerSpecIcons[I].TexId := 0;
      Image_MP_PlayerIcons[I].TexId := 0;
      Image_MP_PlayerIcons[I].Lightness := 0;
    end;
    UpdateServerDetailsUI;
end;


procedure TKMMenuMultiplayer.MP_ServersDoubleClick(Sender: TObject);
begin
  //MP_SelectServer gets called by first Click
  if Button_MP_GetIn.Enabled and (ColumnBox_Servers.ItemIndex <> -1)
  and InRange(ColumnBox_Servers.Rows[ColumnBox_Servers.ItemIndex].Tag, 0, gGameApp.Networking.ServerQuery.Rooms.Count-1) then
    MP_GetInClick(Sender);
end;


procedure TKMMenuMultiplayer.MP_HostClick(Sender: TObject);
var
  serverPortStr: string;
  serverPort: Word;
begin
  //Save the player and IP name so it is not lost if something fails
  MP_SaveSettings;
  serverPortStr := Trim(Edit_MP_ServerPort.Text);
  serverPort    := StrToInt(serverPortStr);

  //Hide the panel so if it fails the error message will be easy to see (e.g. name too long)
  Panel_MPCreateServer.Hide;

  if not ValidatePlayerName(Edit_MP_PlayerName.Text) then
    Exit;

  fOnPageChange(gpLobby, 'HOST');

  gGameApp.Networking.OnHostFail := MP_HostFail;
  gGameApp.Networking.Host(AnsiString(Edit_MP_ServerName.Text), serverPort,
                           AnsiString(Edit_MP_PlayerName.Text), (Sender = Button_MP_CreateWAN));
end;


procedure TKMMenuMultiplayer.MP_GetInClick(Sender: TObject);
begin
  MP_Join(fSelectedServerInfo.IP, fSelectedServerInfo.Port, fSelectedRoomInfo.RoomID);
end;


//Make sure that the nikname as a whole is valid (checks that TKMEdit can not always perform)
function TKMMenuMultiplayer.ValidatePlayerName(const aName: UnicodeString): Boolean;
var
  err: UnicodeString;
  I: Integer;
begin
  err := '';

  if (aName = '') or (aName <> Trim(aName)) then
    err := gResTexts[TX_GAME_ERROR_BLANK_PLAYERNAME]
  else
  if Length(aName) > MAX_NIKNAME_LENGTH then
    err := Format(gResTexts[TX_GAME_ERROR_LONG_PLAYERNAME], [MAX_NIKNAME_LENGTH])
  else
  if (Pos('|', aName) <> 0) or (Pos('[$', aName) <> 0) or (Pos('[]', aName) <> 0) or (Pos('<$', aName) <> 0) then
    err := gResTexts[TX_GAME_ERROR_ILLEGAL_PLAYERNAME]
  else
  for I := 1 to Length(aName) do
    if not InRange(Ord(aName[I]), 32, 126) then
      err := gResTexts[TX_GAME_ERROR_ILLEGAL_PLAYERNAME];

  Result := (err = '');

  if not Result then
  begin
    MP_Update(err, icYellow, False);
    gSoundPlayer.Play(sfxn_Error);
  end
end;


//Join button is enabled if valid server is selected and the lobby is not busy
function TKMMenuMultiplayer.MP_GetInEnabled: Boolean;
var ID: Integer;
begin
  ID := ColumnBox_Servers.ItemIndex;
  Result := (not fLobbyBusy) and (ID <> -1) and (ColumnBox_Servers.Rows[ID].Tag <> -1);
end;


procedure TKMMenuMultiplayer.MP_Join(const aServerAddress: string; aPort: Word; aRoom: Integer);
begin
  //Save the player and IP name so it is not lost if the connection fails
  MP_SaveSettings;

  if not ValidatePlayerName(Edit_MP_PlayerName.Text) then
    Exit;

  //Disable buttons to prevent multiple clicks while connection process is in progress
  MP_Update(gResTexts[TX_MP_MENU_STATUS_CONNECTING], icGreen, True);

  //Send request to join
  gGameApp.Networking.OnJoinSucc := MP_JoinSuccess;
  gGameApp.Networking.OnJoinFail := MP_JoinFail;
  gGameApp.Networking.OnJoinPassword := MP_JoinPassword;
  gGameApp.Networking.OnJoinAssignedHost := MP_JoinAssignedHost;
  gGameApp.Networking.Join(aServerAddress, aPort, AnsiString(Edit_MP_PlayerName.Text), aRoom); //Init lobby
end;


procedure TKMMenuMultiplayer.MP_JoinPassword(Sender: TObject);
begin
  Panel_MPFindServer.Hide;
  Edit_MP_Password.Text := '';
  Panel_MPPassword.Show;
end;


//We had recieved permission to join
procedure TKMMenuMultiplayer.MP_JoinSuccess(Sender: TObject);
begin
  gGameApp.Networking.OnJoinSucc := nil;
  gGameApp.Networking.OnJoinFail := nil;
  gGameApp.Networking.OnJoinAssignedHost := nil;

  fOnPageChange(gpLobby, 'JOIN');
end;


procedure TKMMenuMultiplayer.MP_JoinFail(const aData: UnicodeString);
begin
  gGameApp.Networking.Disconnect;
  MP_Update(Format(gResTexts[TX_GAME_ERROR_CONNECTION_FAILED], [aData]), icYellow, False);
  gSoundPlayer.Play(sfxn_Error);
end;


procedure TKMMenuMultiplayer.MP_JoinAssignedHost(Sender: TObject);
begin
  gGameApp.Networking.OnJoinSucc := nil;
  gGameApp.Networking.OnJoinFail := nil;
  gGameApp.Networking.OnJoinAssignedHost := nil;
  gGameApp.Networking.OnHostFail := MP_HostFail;

  //We were joining a game and the server assigned hosting rights to us
  fOnPageChange(gpLobby, 'HOST'); //Open lobby page in host mode
end;


procedure TKMMenuMultiplayer.EscKeyDown(Sender: TObject);
begin
  if Button_MP_CreateServerCancel.IsClickable then
    MP_CreateServerCancelClick(nil)
  else if Button_MP_FindCancel.IsClickable then
    MP_FindServerCancelClick(nil)
  else if Button_MP_PasswordCancel.IsClickable then
    MP_PasswordClick(Button_MP_PasswordCancel)
  else begin
    BackClick(nil);
  end;
end;


procedure TKMMenuMultiplayer.BackClick(Sender: TObject);
begin
  gGameApp.Networking.Disconnect;
  MP_SaveSettings;

  gMain.UnlockMutex; //Leaving MP areas

  fOnPageChange(gpMainMenu);
end;


procedure TKMMenuMultiplayer.MP_HostFail(const aData: UnicodeString);
begin
  gGameApp.Networking.Disconnect;
  gSoundPlayer.Play(sfxn_Error);

  fOnPageChange(gpMultiplayer, aData);
end;


procedure TKMMenuMultiplayer.Show(const aText: UnicodeString);
begin
  gGameApp.NetworkInit;
  MP_Init;

  if aText = '' then
    //Entering MP anew
    MP_Update(gResTexts[TX_MP_MENU_STATUS_READY],icGreen,false)
  else
    //We are in event handler of Lobby.BackClick (show status warning)
    MP_Update(aText, icYellow, False);

  Panel_MultiPlayer.Show;
end;


end.
