unit KM_GUIMenuMultiplayer;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, KromUtils, KromOGLUtils, Math, Classes, Controls,
  KM_Controls, KM_Defaults, KM_Pics,
  KM_InterfaceDefaults, KM_ServerQuery;


type
  TKMMenuMultiplayer = class {(TKMGUIPage)}
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
    procedure MP_JoinFail(const aData: UnicodeString);
    procedure MP_JoinAssignedHost(Sender: TObject);
    procedure MP_HostClick(Sender: TObject);
    procedure MP_HostFail(const aData: UnicodeString);
    procedure BackClick(Sender: TObject);
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
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);

    procedure Show(aText: UnicodeString);
  end;


implementation
uses KM_Main, KM_NetworkTypes, KM_ResTexts, KM_GameApp, KM_ResLocales,
  KM_Utils, KM_Sound, KM_ResSound, KM_RenderUI, KM_ResFonts;


const
  MAX_NAME_LENGTH = 16;


{ TKMGUIMainMultiplayer }
constructor TKMMenuMultiplayer.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
  procedure CreateServerPopUp;
  begin
    Panel_MPCreateServer := TKMPanel.Create(aParent, 362, 250, 320, 300);
    Panel_MPCreateServer.Anchors := [];
      TKMBevel.Create(Panel_MPCreateServer, -1000,  -1000, 4000, 4000);
      TKMImage.Create(Panel_MPCreateServer, -20, -75, 340, 310, 15, rxGuiMain);
      TKMBevel.Create(Panel_MPCreateServer,   0,  0, 320, 300);
      TKMLabel.Create(Panel_MPCreateServer, 20, 10, 280, 20, gResTexts[TX_MP_MENU_CREATE_SERVER_HEADER], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_MPCreateServer, 20, 50, 280, 20, gResTexts[TX_MP_MENU_CREATE_SERVER_NAME], fnt_Outline, taLeft);
      Edit_MP_ServerName := TKMEdit.Create(Panel_MPCreateServer, 20, 70, 280, 20, fnt_Grey);
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
    Panel_MPFindServer.Anchors := [];
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
    Panel_MPPassword.Anchors := [];
      TKMBevel.Create(Panel_MPPassword, -1000,  -1000, 4000, 4000);
      TKMImage.Create(Panel_MPPassword, -20, -75, 340, 310, 15, rxGuiMain);
      TKMBevel.Create(Panel_MPPassword,   0,  0, 320, 300);
      TKMLabel.Create(Panel_MPPassword,  20, 10, 280, 20, gResTexts[TX_MP_MENU_PASSWORD_HEADER], fnt_Outline, taCenter);

      TKMLabel.Create(Panel_MPPassword, 20, 50, 156, 20, gResTexts[TX_MP_MENU_PASSWORD], fnt_Outline, taLeft);
      Edit_MP_Password := TKMEdit.Create(Panel_MPPassword, 20, 70, 152, 20, fnt_Grey);
      Edit_MP_Password.AllowedChars := acText;
      Button_MP_PasswordOk := TKMButton.Create(Panel_MPPassword, 20, 110, 280, 30, gResTexts[TX_MP_MENU_SERVER_JOIN], bsMenu);
      Button_MP_PasswordOk.OnClick := MP_PasswordClick;
      Button_MP_PasswordCancel := TKMButton.Create(Panel_MPPassword, 20, 150, 280, 30, gResTexts[TX_MP_MENU_FIND_SERVER_CANCEL], bsMenu);
      Button_MP_PasswordCancel.OnClick := MP_PasswordClick;
  end;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  Panel_MultiPlayer := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_MultiPlayer.Stretch;

    //Top area
    Panel_MPPlayerName := TKMPanel.Create(Panel_MultiPlayer, 675, 45, 320, 120);
      TKMBevel.Create(Panel_MPPlayerName, 0, 0, 320, 120);
      TKMLabel.Create(Panel_MPPlayerName, 8, 10, 304, 20, gResTexts[TX_MP_MENU_PLAYERNAME], fnt_Outline, taLeft);
      Edit_MP_PlayerName := TKMEdit.Create(Panel_MPPlayerName, 8, 30, 140, 20, fnt_Grey);
      Edit_MP_PlayerName.MaxLen := MAX_NAME_LENGTH;
      TKMLabel.Create(Panel_MPPlayerName, 8, 60, 304, 20, gResTexts[TX_MP_MENU_STATUS], fnt_Outline, taLeft);
      Label_MP_Status := TKMLabel.Create(Panel_MPPlayerName, 8, 80, 304, 36, '', fnt_Grey, taLeft);
      Label_MP_Status.AutoWrap := True;

    Button_MP_CreateServer := TKMButton.Create(Panel_MultiPlayer, 675, 170, 320, 30, gResTexts[TX_MP_MENU_CREATE_SERVER], bsMenu);
    Button_MP_CreateServer.OnClick := MP_CreateServerClick;

    CreateServerPopUp;

    Button_MP_FindServer := TKMButton.Create(Panel_MultiPlayer, 675, 204, 320, 30, gResTexts[TX_MP_MENU_FIND_SERVER], bsMenu);
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
    ColumnBox_Servers.Focusable := True;
    ColumnBox_Servers.SetColumns(fnt_Outline, ['','',gResTexts[TX_MP_MENU_SERVERLIST_NAME],gResTexts[TX_MP_MENU_SERVERLIST_STATE],gResTexts[TX_MP_MENU_SERVERLIST_PLAYERS],gResTexts[TX_MP_MENU_SERVERLIST_PING]],[0,20,40,300,430,525]);
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
      TKMLabel.Create(Panel_MPServerDetails, 8, 6, 304, 20, gResTexts[TX_MP_MENU_HEADER_SERVER_DETAILS], fnt_Outline, taCenter);
      TKMLabel.Create(Panel_MPServerDetails, 8, 30, 304, 20, gResTexts[TX_MP_MENU_GAME_INFORMATION], fnt_Outline, taLeft);
      Label_MP_Desc := TKMLabel.Create(Panel_MPServerDetails, 8, 50, 304, 80, '', fnt_Metal, taLeft);
      TKMLabel.Create(Panel_MPServerDetails, 8, 110, 304, 20, gResTexts[TX_MP_MENU_PLAYER_LIST], fnt_Outline, taLeft);
      Label_MP_Players := TKMLabel.Create(Panel_MPServerDetails, 8, 130, 304, 340, '', fnt_Metal, taLeft);
      Label_MP_Players.Anchors := [akLeft, akTop, akBottom];

    Button_MP_Back    := TKMButton.Create(Panel_MultiPlayer,  45, 720, 220, 30, gResTexts[TX_MENU_BACK], bsMenu);
    Button_MP_Refresh := TKMButton.Create(Panel_MultiPlayer, 275, 720, 390, 30,gResTexts[TX_MP_MENU_REFRESH_SERVER_LIST], bsMenu);
    Button_MP_GetIn   := TKMButton.Create(Panel_MultiPlayer, 675, 720, 320, 30,gResTexts[TX_MP_MENU_SERVER_JOIN],  bsMenu);
    Button_MP_Back.Anchors    := [akLeft, akBottom];
    Button_MP_Refresh.Anchors := [akLeft, akBottom];
    Button_MP_GetIn.Anchors   := [akLeft, akBottom];
    Button_MP_Back.OnClick    := BackClick;
    Button_MP_Refresh.OnClick := MP_ServersRefresh;
    Button_MP_GetIn.OnClick   := MP_GetInClick;
end;


procedure TKMMenuMultiplayer.MP_Init;
begin
  fServerSelected := False;

  //Refresh the list when they first open the multiplayer page
  MP_ServersRefresh(nil);

  Edit_MP_PlayerName.Text := fGameApp.GameSettings.MultiplayerName;

  Edit_MP_ServerName.Text := fGameApp.GameSettings.ServerName;
  Edit_MP_ServerPort.Text := fGameApp.GameSettings.ServerPort;

  Edit_MP_FindIP.Text := fGameApp.GameSettings.LastIP;
  Edit_MP_FindPort.Text := fGameApp.GameSettings.LastPort;
  Edit_MP_FindRoom.Text := fGameApp.GameSettings.LastRoom;

  Button_MP_GetIn.Disable;

  //Fetch the announcements display
  fGameApp.Networking.ServerQuery.OnAnnouncements := MP_AnnouncementsUpdated;
  fGameApp.Networking.ServerQuery.FetchAnnouncements(gResLocales.UserLocale);
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
begin
  MP_Join(Edit_MP_FindIP.Text, Edit_MP_FindPort.Text, StrToIntDef(Edit_MP_FindRoom.Text, -1));
end;


procedure TKMMenuMultiplayer.MP_PasswordClick(Sender: TObject);
begin
  if Sender = Button_MP_PasswordOk then
  begin
    Panel_MPPassword.Hide;
    fGameApp.Networking.SendPassword(AnsiString(Edit_MP_Password.Text));
  end;
  if Sender = Button_MP_PasswordCancel then
  begin
    fGameApp.Networking.Disconnect;
    Panel_MPPassword.Hide;
    MP_Update(gResTexts[TX_MP_MENU_STATUS_READY], icGreen, False);
  end;
end;


//Save the Player and IP name so it is not lost inbetween activities
procedure TKMMenuMultiplayer.MP_SaveSettings;
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


procedure TKMMenuMultiplayer.MP_ServersRefresh(Sender: TObject);
begin
  fGameApp.Networking.ServerQuery.OnListUpdated := MP_ServersUpdateList;
  fGameApp.Networking.ServerQuery.RefreshList;
  ColumnBox_Servers.Clear;
  Label_MP_Desc.Caption := '';
  Label_MP_Players.Caption := '';

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

  if fGameApp.Networking.ServerQuery.Rooms.Count = 0 then
  begin
    //Do not use 'Show' here as it will also make the parent panel visible
    //which could be already hidden if player switched pages
    Label_Servers_Status.Caption := gResTexts[TX_MP_MENU_NO_SERVERS];
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
      MakeListRow(['', '', DisplayName, gResTexts[GameStateTextIDs[R.GameInfo.GameState]], IntToStr(R.GameInfo.PlayerCount), IntToStr(S.Ping)],
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


procedure TKMMenuMultiplayer.MP_ServersClick(Sender: TObject);
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


procedure TKMMenuMultiplayer.MP_ServersDoubleClick(Sender: TObject);
begin
  //MP_SelectServer gets called by first Click
  if Button_MP_GetIn.Enabled and (ColumnBox_Servers.ItemIndex <> -1)
  and InRange(ColumnBox_Servers.Rows[ColumnBox_Servers.ItemIndex].Tag, 0, fGameApp.Networking.ServerQuery.Rooms.Count-1) then
    MP_GetInClick(Sender);
end;


procedure TKMMenuMultiplayer.MP_HostClick(Sender: TObject);
begin
  //Save the player and IP name so it is not lost if something fails
  MP_SaveSettings;

  Panel_MPCreateServer.Hide; //Hide the panel so if it fails the error message will be easy to see (e.g. name too long)
  if not MP_ValidatePlayerName(Edit_MP_PlayerName.Text) then Exit;

  fOnPageChange(gpLobby, 'HOST');

  fGameApp.Networking.OnHostFail := MP_HostFail;
  fGameApp.Networking.Host(Edit_MP_ServerName.Text, Edit_MP_ServerPort.Text, AnsiString(Edit_MP_PlayerName.Text), (Sender = Button_MP_CreateWAN));
end;


procedure TKMMenuMultiplayer.MP_GetInClick(Sender: TObject);
begin
  MP_Join(fSelectedServerInfo.IP, fSelectedServerInfo.Port, fSelectedRoomInfo.RoomID);
end;


function TKMMenuMultiplayer.MP_ValidatePlayerName(const aName: string): Boolean;
begin
  Result := False;

  if Trim(aName) = '' then
  begin
    MP_Update(gResTexts[TX_GAME_ERROR_BLANK_PLAYERNAME], icYellow, false);
    gSoundPlayer.Play(sfxn_Error);
  end
  else if Length(aName) > MAX_NAME_LENGTH then
  begin
    MP_Update(Format(gResTexts[TX_GAME_ERROR_LONG_PLAYERNAME], [MAX_NAME_LENGTH]), icYellow, false);
    gSoundPlayer.Play(sfxn_Error);
  end
  else if (Pos('|', aName) <> 0) or (Pos('[$', aName) <> 0) or (Pos('[]', aName) <> 0) or (Pos('<$', aName) <> 0) then
  begin
    MP_Update(gResTexts[TX_GAME_ERROR_ILLEGAL_PLAYERNAME], icYellow, false);
    gSoundPlayer.Play(sfxn_Error);
  end
  else
    Result := True;
end;


//Join button is enabled if valid server is selected and the lobby is not busy
function TKMMenuMultiplayer.MP_GetInEnabled: Boolean;
var ID: Integer;
begin
  ID := ColumnBox_Servers.ItemIndex;
  Result := (not fLobbyBusy) and (ID <> -1) and (ColumnBox_Servers.Rows[ID].Tag <> -1);
end;


procedure TKMMenuMultiplayer.MP_Join(aServerAddress, aPort: string; aRoom: Integer);
begin
  //Save the player and IP name so it is not lost if the connection fails
  MP_SaveSettings;

  if not MP_ValidatePlayerName(Edit_MP_PlayerName.Text) then Exit;

  //Disable buttons to prevent multiple clicks while connection process is in progress
  MP_Update(gResTexts[TX_MP_MENU_STATUS_CONNECTING],icGreen, True);

  //Send request to join
  fGameApp.Networking.OnJoinSucc := MP_JoinSuccess;
  fGameApp.Networking.OnJoinFail := MP_JoinFail;
  fGameApp.Networking.OnJoinPassword := MP_JoinPassword;
  fGameApp.Networking.OnJoinAssignedHost := MP_JoinAssignedHost;
  fGameApp.Networking.Join(aServerAddress, aPort, AnsiString(Edit_MP_PlayerName.Text), aRoom); //Init lobby
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
  fGameApp.Networking.OnJoinSucc := nil;
  fGameApp.Networking.OnJoinFail := nil;
  fGameApp.Networking.OnJoinAssignedHost := nil;

  fOnPageChange(gpLobby, 'JOIN');
end;


procedure TKMMenuMultiplayer.MP_JoinFail(const aData: UnicodeString);
begin
  fGameApp.Networking.Disconnect;
  MP_Update(Format(gResTexts[TX_GAME_ERROR_CONNECTION_FAILED],[aData]),icYellow,false);
  gSoundPlayer.Play(sfxn_Error);
end;


procedure TKMMenuMultiplayer.MP_JoinAssignedHost(Sender: TObject);
begin
  fGameApp.Networking.OnJoinSucc := nil;
  fGameApp.Networking.OnJoinFail := nil;
  fGameApp.Networking.OnJoinAssignedHost := nil;
  fGameApp.Networking.OnHostFail := MP_HostFail;

  //We were joining a game and the server assigned hosting rights to us
  fOnPageChange(gpLobby, 'HOST'); //Open lobby page in host mode
end;


procedure TKMMenuMultiplayer.BackClick(Sender: TObject);
begin
  fGameApp.Networking.Disconnect;
  MP_SaveSettings;

  fMain.UnlockMutex; //Leaving MP areas

  fOnPageChange(gpMainMenu);
end;


procedure TKMMenuMultiplayer.MP_HostFail(const aData: UnicodeString);
begin
  fGameApp.Networking.Disconnect;
  gSoundPlayer.Play(sfxn_Error);

  fOnPageChange(gpMultiplayer, aData);
end;


procedure TKMMenuMultiplayer.Show(aText: UnicodeString);
begin
  fGameApp.NetworkInit;
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
