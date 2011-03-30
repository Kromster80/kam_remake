unit KM_InterfaceMainMenu;
{$I KaM_Remake.inc}
interface
uses MMSystem, SysUtils, KromUtils, KromOGLUtils, Math, Classes, Controls,
  {$IFDEF WDC} OpenGL, {$ENDIF}
  {$IFDEF FPC} GL, {$ENDIF}
  KM_Controls, KM_Defaults, KM_CommonTypes, Windows, KM_Settings, KM_MapInfo, KM_Networking;


type TMenuScreen = (msError, msLoading, msMain, msOptions, msResults);


type
  TKMMainMenuInterface = class
  private
    MyControls: TKMMasterControl;
    ScreenX,ScreenY:word;

    Campaign_Selected:TCampaign;
    Campaign_Mission_Choice:integer;

    SingleMap_Top:integer; //Top map in list
    SingleMap_Selected:integer; //Selected map
    SingleMapsInfo:TKMMapsInfo;
    MapEdSizeX,MapEdSizeY:integer; //Map Editor map size
    OldFullScreen:boolean;
    OldResolution:word;
  protected
    Panel_Main:TKMPanel;
      Label_Version:TKMLabel;
    Panel_MainMenu:TKMPanel;
      Panel_MMButtons:TKMPanel;
      Button_MM_SinglePlayer,
      Button_MM_MultiPlayer,
      Button_MM_MapEd,
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
      Panel_MPButtons:TKMPanel;
      Button_MP_LAN,
      Button_MP_WWW,
      Button_MP_Back:TKMButton;

      Panel_LANLogin:TKMPanel;
        Panel_LANLogin2:TKMPanel;
          Label_LAN_IP:TKMLabel;
          Button_LAN_Host:TKMButton;
          Edit_LAN_IP:TKMEdit;
          Button_LAN_Join:TKMButton;
          Label_LAN_Status:TKMLabel;
          Button_LAN_LoginBack:TKMButton;

    Panel_Lobby:TKMPanel;
      Panel_LobbyPlayers:TKMPanel;
        Label_LobbyPlayer:array [0..MAX_PLAYERS-1] of TKMLabel;
        DropBox_LobbyLoc:array [0..MAX_PLAYERS-1] of TKMDropBox;
        DropBox_LobbyColor:array [0..MAX_PLAYERS-1] of TKMDropBox;
        CheckBox_LobbyReady:array [0..MAX_PLAYERS-1] of TKMCheckBox;

      Panel_LobbySetup:TKMPanel;
        FileList_Lobby:TKMFileList;
        Label_LobbyMapName:TKMLabel;
        Label_LobbyMapCount:TKMLabel;
        Label_LobbyMapMode:TKMLabel;
        Label_LobbyMapCond:TKMLabel;

      Button_LobbyBack:TKMButton;
      Button_LobbyReady:TKMButton;
      Button_LobbyStart:TKMButton;
      ListBox_LobbyPosts:TKMListBox;
      Edit_LobbyPost:TKMEdit;


    Panel_Campaign:TKMPanel;
      Image_CampaignBG:TKMImage;
      Image_CampaignNodes:array[1..MAX_MAPS] of TKMImage;
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
      Button_Load:array[1..SAVEGAME_COUNT] of TKMButton;
      Button_LoadBack:TKMButton;
    Panel_MapEd:TKMPanel;
      Panel_MapEd_SizeXY:TKMPanel;
      CheckBox_MapEd_SizeX,CheckBox_MapEd_SizeY:array[1..MAPSIZES_COUNT] of TKMCheckBox; //todo: Replace with RadioGroup
      Panel_MapEd_Load:TKMPanel;
      FileList_MapEd:TKMFileList;
      Button_MapEdBack,Button_MapEd_Create,Button_MapEd_Load:TKMButton;
    Panel_Options:TKMPanel;
      Panel_Options_GFX:TKMPanel;
        Ratio_Options_Brightness:TKMRatioRow;
      Panel_Options_Ctrl:TKMPanel;
        Label_Options_MouseSpeed:TKMLabel;
        Ratio_Options_Mouse:TKMRatioRow;
      Panel_Options_Game:TKMPanel;
        CheckBox_Options_Autosave:TKMCheckBox;
      Panel_Options_Sound:TKMPanel;
        Label_Options_SFX,Label_Options_Music,Label_Options_MusicOn:TKMLabel;
        Ratio_Options_SFX,Ratio_Options_Music:TKMRatioRow;
        CheckBox_Options_MusicOn:TKMCheckBox;
      Panel_Options_Lang:TKMPanel;
        Radio_Options_Lang:TKMRadioGroup;
      Panel_Options_Res:TKMPanel;
        CheckBox_Options_FullScreen:TKMCheckBox;
        CheckBox_Options_Resolution:array[1..RESOLUTION_COUNT] of TKMCheckBox; //todo: Replace with RadioGroup
        Button_Options_ResApply:TKMButton;
      Button_Options_Back:TKMButton;
    Panel_Credits:TKMPanel;
      Label_Credits:TKMLabel;
      Button_CreditsBack:TKMButton;
    Panel_Loading:TKMPanel;
      Label_Loading:TKMLabel;
    Panel_Error:TKMPanel;
      Label_Error:TKMLabel;
      Button_ErrorBack:TKMButton;
    Panel_Results:TKMPanel;
      Label_Results_Result:TKMLabel;
      Panel_Stats:TKMPanel;
      Label_Stat:array[1..9]of TKMLabel;
      Button_ResultsBack,Button_ResultsRepeat,Button_ResultsContinue:TKMButton;
  private
    procedure Create_MainMenu_Page;
    procedure Create_SinglePlayer_Page;
    procedure Create_Campaign_Page;
    procedure Create_Single_Page;
    procedure Create_Load_Page;
    procedure Create_MultiPlayer_Page;
    procedure Create_LANLogin_Page;
    procedure Create_Lobby_Page;
    procedure Create_MapEditor_Page;
    procedure Create_Options_Page(aGameSettings:TGlobalSettings);
    procedure Create_Credits_Page;
    procedure Create_Loading_Page;
    procedure Create_Error_Page;
    procedure Create_Results_Page;
    procedure SwitchMenuPage(Sender: TObject);
    procedure MainMenu_PlayTutorial(Sender: TObject);
    procedure MainMenu_PlayBattle(Sender: TObject);
    procedure MainMenu_ReplayView(Sender: TObject);
    procedure MainMenu_ReplayLastMap(Sender: TObject);
    procedure Campaign_Set(aCampaign:TCampaign);
    procedure Campaign_SelectMap(Sender: TObject);
    procedure Campaign_StartMap(Sender: TObject);

    procedure SingleMap_PopulateList;
    procedure SingleMap_RefreshList;
    procedure SingleMap_ScrollChange(Sender: TObject);
    procedure SingleMap_SelectMap(Sender: TObject);
    procedure SingleMap_Start(Sender: TObject);

    procedure LAN_Host(Sender: TObject);
    procedure LAN_Join(Sender: TObject);
    procedure LAN_JoinSucc(Sender: TObject);
    procedure LAN_JoinFail(const aData:string);
    procedure LAN_Update;
    procedure LAN_QuitLobby;

    procedure Lobby_Reset(Sender: TObject);
    procedure Lobby_PostKey(Sender: TObject; Key: Word);
    procedure Lobby_MapSelect(Sender: TObject);
    procedure Lobby_PlayersSetupChange(Sender: TObject);
    procedure Lobby_OnMessage(const aData:string);
    procedure Lobby_OnPlayersSetup(Sender: TObject);
    procedure Lobby_OnMapName(const aData:string);
    procedure Lobby_OnAllReady(Sender: TObject);
    procedure Lobby_ReadyClick(Sender: TObject);
    procedure Lobby_StartClick(Sender: TObject);

    procedure Load_Click(Sender: TObject);
    procedure Load_RefreshList;
    procedure MapEditor_Start(Sender: TObject);
    procedure MapEditor_Change(Sender: TObject);
    procedure Options_Fill;
    procedure Options_Change(Sender: TObject);
  public
    constructor Create(X,Y:word; aGameSettings:TGlobalSettings);
    destructor Destroy; override;
    procedure ResizeGameArea(X,Y:word);
    procedure ShowScreen(aScreen:TMenuScreen; const aText:string=''; aMsg:TGameResultMsg=gr_Silent);
    procedure Fill_Results;

    procedure KeyDown(Key:Word; Shift: TShiftState);
    procedure KeyUp(Key:Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);
    procedure UpdateState;
    procedure Paint;
end;


implementation
uses KM_Unit1, KM_Render, KM_TextLibrary, KM_Game, KM_PlayersCollection, Forms, KM_Utils;


constructor TKMMainMenuInterface.Create(X,Y:word; aGameSettings:TGlobalSettings);
begin
  Inherited Create;

  fLog.AssertToLog(fTextLibrary<>nil, 'fTextLibrary should be initialized before MainMenuInterface');

  ScreenX := min(X,MENU_DESIGN_X);
  ScreenY := min(Y,MENU_DESIGN_Y);
  Campaign_Mission_Choice := 1;
  SingleMap_Top := 0;
  SingleMap_Selected := 0;
  MapEdSizeX := 64;
  MapEdSizeY := 64;

  MyControls := TKMMasterControl.Create;
  Panel_Main := TKMPanel.Create(MyControls, (X-MENU_DESIGN_X) div 2,
                                         (Y-MENU_DESIGN_Y) div 2,
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
    Create_LANLogin_Page;
    Create_Lobby_Page;
  Create_MapEditor_Page;
  Create_Options_Page(aGameSettings);
  Create_Credits_Page;
  Create_Loading_Page;
  Create_Error_Page;
  Create_Results_Page;

    {for i:=1 to length(FontFiles) do L[i]:=TKMLabel.Create(Panel_Main1,550,280+i*20,160,30,'This is a test string for KaM Remake ('+FontFiles[i],TKMFont(i),kaLeft);//}
    //MyControls.AddTextEdit(Panel_Main, 32, 32, 200, 20, fnt_Grey);
    //FL := MyControls.AddFileList(Panel_Main1, 550, 300, 320, 220);
    //FL.RefreshList(ExeDir+'Maps\','dat',true);


  //Show version info on every page
  Label_Version := TKMLabel.Create(Panel_Main,8,8,100,30,GAME_VERSION+' / OpenGL '+fRender.RendererVersion,fnt_Antiqua,kaLeft);

  if SHOW_1024_768_OVERLAY then with TKMShape.Create(Panel_Main, 0, 0, 1024, 768, $FF00FF00) do Hitable:=false;

  SwitchMenuPage(nil);
  //ShowScreen_Results; //Put here page you would like to debug
  fLog.AppendLog('Main menu init done');
end;


destructor TKMMainMenuInterface.Destroy;
begin
  SingleMapsInfo.Free;
  MyControls.Free;
  Inherited;
end;


procedure TKMMainMenuInterface.ResizeGameArea(X, Y:word);
begin
  ScreenX := min(X,MENU_DESIGN_X);
  ScreenY := min(Y,MENU_DESIGN_Y);
  Panel_Main.Left := (X-MENU_DESIGN_X) div 2;
  Panel_Main.Top  := (Y-MENU_DESIGN_Y) div 2;
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
                    gr_Win:    Label_Results_Result.Caption := fTextLibrary.GetSetupString(111);
                    gr_Defeat: Label_Results_Result.Caption := fTextLibrary.GetSetupString(112);
                    gr_Cancel: Label_Results_Result.Caption := fTextLibrary.GetRemakeString(1);
                    else       Label_Results_Result.Caption := '<<<LEER>>>'; //Thats string used in all Synetic games for missing texts =)
                  end;

                  Button_ResultsRepeat.Enabled := aMsg in [gr_Defeat, gr_Cancel];

                  //Even if the campaign is complete Player can now return to it's screen to replay any of the maps
                  Button_ResultsContinue.Visible := fGame.GetCampaign in [cmp_TSK, cmp_TPR];
                  Button_ResultsContinue.Enabled := aMsg = gr_Win;

                  SwitchMenuPage(Panel_Results);
                end;
  end;
end;


procedure TKMMainMenuInterface.Fill_Results;
begin
  if (MyPlayer=nil) or (MyPlayer.Stats=nil) then exit;

  Label_Stat[1].Caption := inttostr(MyPlayer.Stats.GetUnitsLost);
  Label_Stat[2].Caption := inttostr(MyPlayer.Stats.GetUnitsKilled);
  Label_Stat[3].Caption := inttostr(MyPlayer.Stats.GetHousesLost);
  Label_Stat[4].Caption := inttostr(MyPlayer.Stats.GetHousesDestroyed);
  Label_Stat[5].Caption := inttostr(MyPlayer.Stats.GetHousesBuilt);
  Label_Stat[6].Caption := inttostr(MyPlayer.Stats.GetUnitsTrained);
  Label_Stat[7].Caption := inttostr(MyPlayer.Stats.GetWeaponsProduced);
  Label_Stat[8].Caption := inttostr(MyPlayer.Stats.GetSoldiersTrained);
  Label_Stat[9].Caption := int2time(fGame.GetMissionTime);
end;


procedure TKMMainMenuInterface.Create_MainMenu_Page;
begin
  Panel_MainMenu:=TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);
    TKMImage.Create(Panel_MainMenu,300,60,423,164,4,5);
    TKMLabel.Create(Panel_MainMenu, 512, 240, 100, 20, 'Remake', fnt_Metal, kaCenter);
    with TKMImage.Create(Panel_MainMenu,50,220,round(218*1.3),round(291*1.3),5,6) do ImageStretch;
    with TKMImage.Create(Panel_MainMenu,705,220,round(207*1.3),round(295*1.3),6,6) do ImageStretch;

    with TKMDropBox.Create(Panel_MainMenu, 500, 260, 150, 20, fnt_Metal) do
    begin
      Items.Add('Undefined');
      Items.Add('Undefined');
      Items.Add('Undefined');
      Items.Add('Undefined');
      Items.Add('Undefined');
    end;


    Panel_MMButtons:=TKMPanel.Create(Panel_MainMenu,337,290,350,400);
      Button_MM_SinglePlayer := TKMButton.Create(Panel_MMButtons,0,  0,350,30,fTextLibrary.GetRemakeString(4),fnt_Metal,bsMenu);
      Button_MM_MultiPlayer  := TKMButton.Create(Panel_MMButtons,0, 40,350,30,fTextLibrary.GetSetupString(11),fnt_Metal,bsMenu);
      Button_MM_MapEd        := TKMButton.Create(Panel_MMButtons,0, 80,350,30,fTextLibrary.GetRemakeString(5),fnt_Metal,bsMenu);
      Button_MM_Options      := TKMButton.Create(Panel_MMButtons,0,120,350,30,fTextLibrary.GetSetupString(12),fnt_Metal,bsMenu);
      Button_MM_Credits      := TKMButton.Create(Panel_MMButtons,0,160,350,30,fTextLibrary.GetSetupString(13),fnt_Metal,bsMenu);
      Button_MM_Quit         := TKMButton.Create(Panel_MMButtons,0,320,350,30,fTextLibrary.GetSetupString(14),fnt_Metal,bsMenu);
      Button_MM_SinglePlayer.OnClick := SwitchMenuPage;
      Button_MM_MultiPlayer.OnClick  := SwitchMenuPage;
      Button_MM_MapEd.OnClick        := SwitchMenuPage;
      Button_MM_Options.OnClick      := SwitchMenuPage;
      Button_MM_Credits.OnClick      := SwitchMenuPage;
      Button_MM_Quit.OnClick         := Form1.Exit1.OnClick;


      Button_MM_MapEd.Visible        := SHOW_MAPED_IN_MENU; //Let it be created, but hidden, I guess there's no need to seriously block it
      Button_MM_MultiPlayer.Enabled  :=  ENABLE_MP_IN_MENU;
end;


procedure TKMMainMenuInterface.Create_SinglePlayer_Page;
begin
  Panel_SinglePlayer:=TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);
    TKMImage.Create(Panel_SinglePlayer,300,60,423,164,4,5);
    TKMLabel.Create(Panel_SinglePlayer, 512, 240, 100, 20, 'Remake', fnt_Metal, kaCenter);
    with TKMImage.Create(Panel_SinglePlayer,50,220,round(218*1.3),round(291*1.3),5,6) do ImageStretch;
    with TKMImage.Create(Panel_SinglePlayer,705,220,round(207*1.3),round(295*1.3),6,6) do ImageStretch;

    Panel_SPButtons:=TKMPanel.Create(Panel_SinglePlayer,337,290,350,400);
      Button_SP_Tutor  :=TKMButton.Create(Panel_SPButtons,0,  0,350,30,fTextLibrary.GetRemakeString(2),fnt_Metal,bsMenu);
      Button_SP_Fight  :=TKMButton.Create(Panel_SPButtons,0, 40,350,30,fTextLibrary.GetRemakeString(3),fnt_Metal,bsMenu);
      Button_SP_TSK    :=TKMButton.Create(Panel_SPButtons,0, 80,350,30,fTextLibrary.GetSetupString( 1),fnt_Metal,bsMenu);
      Button_SP_TPR    :=TKMButton.Create(Panel_SPButtons,0,120,350,30,fTextLibrary.GetSetupString( 2),fnt_Metal,bsMenu);
      Button_SP_Single :=TKMButton.Create(Panel_SPButtons,0,160,350,30,fTextLibrary.GetSetupString( 4),fnt_Metal,bsMenu);
      Button_SP_Load   :=TKMButton.Create(Panel_SPButtons,0,200,350,30,fTextLibrary.GetSetupString(10),fnt_Metal,bsMenu);
      Button_SP_Replay :=TKMButton.Create(Panel_SPButtons,0,240,350,30,fTextLibrary.GetRemakeString(6),fnt_Metal,bsMenu);
      Button_SP_Back   :=TKMButton.Create(Panel_SPButtons,0,320,350,30,fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);

      Button_SP_Tutor.OnClick    := MainMenu_PlayTutorial;
      Button_SP_Fight.OnClick    := MainMenu_PlayBattle;
      Button_SP_TSK.OnClick      := SwitchMenuPage;
      Button_SP_TPR.OnClick      := SwitchMenuPage;
      Button_SP_Single.OnClick   := SwitchMenuPage;
      Button_SP_Load.OnClick     := SwitchMenuPage;
      Button_SP_Replay.OnClick   := MainMenu_ReplayView; //Reroute since fGame is not initialized yet
      Button_SP_Back.OnClick     := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_MultiPlayer_Page;
begin
  Panel_MultiPlayer := TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);
    with TKMImage.Create(Panel_MultiPlayer,635,220,round(207*1.3),round(295*1.3),6,6) do ImageStretch;

    Panel_MPButtons:=TKMPanel.Create(Panel_MultiPlayer,155,280,350,400);
      Button_MP_LAN  := TKMButton.Create(Panel_MPButtons,0,  0,350,30,fTextLibrary.GetRemakeString(7),fnt_Metal,bsMenu);
      Button_MP_WWW  := TKMButton.Create(Panel_MPButtons,0, 40,350,30,fTextLibrary.GetRemakeString(8),fnt_Metal,bsMenu);
      Button_MP_LAN.OnClick := SwitchMenuPage;
      Button_MP_WWW.OnClick := SwitchMenuPage;
      Button_MP_WWW.Disable;

    Button_MP_Back := TKMButton.Create(Panel_MultiPlayer, 45, 650, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_MP_Back.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_LANLogin_Page;
begin
  Panel_LANLogin := TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);
    Panel_LANLogin2 := TKMPanel.Create(Panel_LANLogin,312,280,400,400);

      TKMLabel.Create(Panel_LANLogin2, 100, 0, 100, 20, 'Your IP address is:', fnt_Metal, kaCenter);
      Label_LAN_IP := TKMLabel.Create(Panel_LANLogin2, 100, 25, 100, 20, '0.0.0.0', fnt_Outline, kaCenter);
      Button_LAN_Host := TKMButton.Create(Panel_LANLogin2, 50, 60, 100, 30, 'Host', fnt_Metal, bsMenu);
      Button_LAN_Host.OnClick := LAN_Host;

      TKMLabel.Create(Panel_LANLogin2, 300, 0, 100, 20, 'Set partners IP address:', fnt_Metal, kaCenter);
      Edit_LAN_IP := TKMEdit.Create(Panel_LANLogin2, 250, 25, 100, 20, fnt_Grey);
      Edit_LAN_IP.Text := '127.0.0.1';
      Button_LAN_Join := TKMButton.Create(Panel_LANLogin2, 250, 60, 100, 30, 'Join', fnt_Metal, bsMenu);
      Button_LAN_Join.OnClick := LAN_Join;

      Label_LAN_Status := TKMLabel.Create(Panel_LANLogin2, 200, 140, 100, 20, ' ... ', fnt_Outline, kaCenter);

    Button_LAN_LoginBack := TKMButton.Create(Panel_LANLogin2, 45, 300, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_LAN_LoginBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Lobby_Page;
var i,k:integer;
begin
  Panel_Lobby := TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);

    //Players
    Panel_LobbyPlayers := TKMPanel.Create(Panel_Lobby,80,100,580,240);
      TKMBevel.Create(Panel_LobbyPlayers,   0,  0, 580, 240);
      TKMLabel.Create(Panel_LobbyPlayers,  10, 10, 150, 20, 'Players list:', fnt_Outline, kaLeft);
      TKMLabel.Create(Panel_LobbyPlayers, 170, 10, 150, 20, 'Start location:', fnt_Outline, kaLeft);
      TKMLabel.Create(Panel_LobbyPlayers, 340, 10, 140, 20, 'Flag colors:', fnt_Outline, kaLeft);
      TKMLabel.Create(Panel_LobbyPlayers, 510, 10, 40, 20, 'Ready:', fnt_Outline, kaLeft);

      for i:=0 to MAX_PLAYERS-1 do begin
        Label_LobbyPlayer[i] := TKMLabel.Create(Panel_LobbyPlayers, 10, 30+i*25, 150, 20, '. ', fnt_Metal, kaLeft);

        DropBox_LobbyLoc[i] := TKMDropBox.Create(Panel_LobbyPlayers, 170, 30+i*25, 150, 20, fnt_Metal);
        DropBox_LobbyLoc[i].Items.Add('Undefined');
        for k:=1 to MAX_PLAYERS do
          DropBox_LobbyLoc[i].Items.Add('Location ' + inttostr(k));
        DropBox_LobbyLoc[i].OnChange := Lobby_PlayersSetupChange;

        DropBox_LobbyColor[i] := TKMDropBox.Create(Panel_LobbyPlayers, 340, 30+i*25, 150, 20, fnt_Metal);
        DropBox_LobbyColor[i].Items.Add('Undefined');
        DropBox_LobbyColor[i].Items.Add('Red');
        DropBox_LobbyColor[i].Items.Add('Cyan');
        DropBox_LobbyColor[i].Items.Add('Green');
        DropBox_LobbyColor[i].Items.Add('Magenta');
        DropBox_LobbyColor[i].Items.Add('Yellow');
        DropBox_LobbyColor[i].Items.Add('Grey');
        DropBox_LobbyColor[i].Items.Add('Black');
        DropBox_LobbyColor[i].Items.Add('Black');
        DropBox_LobbyColor[i].OnChange := Lobby_PlayersSetupChange;

        CheckBox_LobbyReady[i] := TKMCheckBox.Create(Panel_LobbyPlayers, 510, 30+i*25, 40, 20, '', fnt_Metal);
      end;

    //Chat
                          TKMLabel.Create  (Panel_Lobby, 80, 350, 100, 20, 'Posts list:', fnt_Outline, kaLeft);
    ListBox_LobbyPosts := TKMListBox.Create(Panel_Lobby, 80, 370, 580, 200);
                          TKMLabel.Create  (Panel_Lobby, 80, 580, 100, 20, 'Post message:', fnt_Outline, kaLeft);
    Edit_LobbyPost :=     TKMEdit.Create   (Panel_Lobby, 80, 600, 580, 20, fnt_Metal);
    Edit_LobbyPost.OnKeyDown := Lobby_PostKey;


    //Setup
    Panel_LobbySetup := TKMPanel.Create(Panel_Lobby,700,100,240,400);
      TKMBevel.Create(Panel_LobbySetup,  0,  0, 240, 520);
      TKMLabel.Create(Panel_LobbySetup, 10, 10, 100, 20, 'Available maps:', fnt_Outline, kaLeft);
      FileList_Lobby := TKMFileList.Create(Panel_LobbySetup, 10, 30, 220, 300);
      FileList_Lobby.OnChange := Lobby_MapSelect;
      TKMLabel.Create(Panel_LobbySetup, 10, 360, 100, 20, 'Map info:', fnt_Outline, kaLeft);
      Label_LobbyMapName := TKMLabel.Create(Panel_LobbySetup, 10, 380, 220, 20, '', fnt_Metal, kaLeft);
      Label_LobbyMapCount := TKMLabel.Create(Panel_LobbySetup, 10, 400, 220, 20, 'Players: 4/6', fnt_Metal, kaLeft);
      Label_LobbyMapMode := TKMLabel.Create(Panel_LobbySetup, 10, 420, 220, 20, 'Mode: Building', fnt_Metal, kaLeft);
      Label_LobbyMapCond := TKMLabel.Create(Panel_LobbySetup, 10, 440, 220, 20, 'Conditions: Town', fnt_Metal, kaLeft);

    Button_LobbyBack := TKMButton.Create(Panel_Lobby, 80, 650, 190, 30, 'Quit lobby', fnt_Metal, bsMenu);
    Button_LobbyBack.OnClick := SwitchMenuPage;
    Button_LobbyReady := TKMButton.Create(Panel_Lobby, 700, 650, 120, 30, 'Ready', fnt_Metal, bsMenu);
    Button_LobbyReady.OnClick := Lobby_ReadyClick;
    Button_LobbyStart := TKMButton.Create(Panel_Lobby, 830, 650, 120, 30, 'Start!', fnt_Metal, bsMenu);
    Button_LobbyStart.OnClick := Lobby_StartClick;
end;


procedure TKMMainMenuInterface.Create_Campaign_Page;
var i:integer;
begin
  Panel_Campaign:=TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);
    Image_CampaignBG := TKMImage.Create(Panel_Campaign,0,0,ScreenX,ScreenY,12,5);
    Image_CampaignBG.ImageStretch;

    for i:=1 to length(Image_CampaignNodes) do begin
      Image_CampaignNodes[i] := TKMImage.Create(Panel_Campaign, ScreenX, ScreenY, 23, 29, 10, 5);
      Image_CampaignNodes[i].OnClick := Campaign_SelectMap;
      Image_CampaignNodes[i].Tag := i;
    end;

  Panel_CampScroll:=TKMPanel.Create(Panel_Campaign,ScreenX-360,ScreenY-430,360,430);

    Image_Scroll := TKMImage.Create(Panel_CampScroll, 0, 0,360,430,{15}2,6);
    Image_Scroll.ImageStretch;
    Label_CampaignTitle := TKMLabel.Create(Panel_CampScroll, 180, 18,100,20, '', fnt_Outline, kaCenter);

    Label_CampaignText := TKMLabel.Create(Panel_CampScroll, 20, 50, 325, 310, '', fnt_Briefing, kaLeft);
    Label_CampaignText.AutoWrap := true;

  Button_CampaignStart := TKMButton.Create(Panel_Campaign, ScreenX-220-20, ScreenY-50, 220, 30, fTextLibrary.GetRemakeString(11), fnt_Metal, bsMenu);
  Button_CampaignStart.OnClick := Campaign_StartMap;

  Button_CampaignBack := TKMButton.Create(Panel_Campaign, 20, ScreenY-50, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
  Button_CampaignBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Single_Page;
var i:integer;
begin
  SingleMapsInfo:=TKMMapsInfo.Create;

  Panel_Single:=TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);
    Panel_SingleList:=TKMPanel.Create(Panel_Single,512+22,84,445,600);

      ScrollBar_SingleMaps := TKMScrollBar.Create(Panel_SingleList,420,40,25,MENU_SP_MAPS_COUNT*40, sa_Vertical, bsMenu);
      ScrollBar_SingleMaps.OnChange := SingleMap_ScrollChange;

      Button_SingleHeadMode  := TKMButton.Create(Panel_SingleList,  0,0, 40,40,42,4,bsMenu);
      Button_SingleHeadTeams := TKMButton.Create(Panel_SingleList, 40,0, 40,40,31,4,bsMenu);
      Button_SingleHeadTitle := TKMButton.Create(Panel_SingleList, 80,0,300,40,fTextLibrary.GetRemakeString(12),fnt_Metal,bsMenu);
      Button_SingleHeadSize  := TKMButton.Create(Panel_SingleList,380,0, 40,40,fTextLibrary.GetRemakeString(13),fnt_Metal,bsMenu);
      with TKMButton.Create(Panel_SingleList,420,0, 25,40,'',fnt_Metal,bsMenu) do Disable;

      for i:=0 to MENU_SP_MAPS_COUNT-1 do
      begin
        Bevel_SingleBG[i,1] := TKMBevel.Create(Panel_SingleList,0,  40+i*40, 40,40);
        Bevel_SingleBG[i,2] := TKMBevel.Create(Panel_SingleList,40, 40+i*40, 40,40);
        Bevel_SingleBG[i,3] := TKMBevel.Create(Panel_SingleList,80, 40+i*40,300,40);
        Bevel_SingleBG[i,4] := TKMBevel.Create(Panel_SingleList,380,40+i*40, 40,40);

        Image_SingleMode[i]    := TKMImage.Create(Panel_SingleList,  0   ,40+i*40,40,40,28);
        Image_SingleMode[i].ImageCenter;
        Label_SinglePlayers[i] := TKMLabel.Create(Panel_SingleList, 40+20, 40+i*40+14, 40,40,'0',fnt_Metal, kaCenter);
        Label_SingleTitle1[i]  := TKMLabel.Create(Panel_SingleList, 80+ 6, 40+i*40+ 5, 40,40,'<<<LEER>>>',fnt_Metal, kaLeft);
        Label_SingleTitle2[i]  := TKMLabel.Create(Panel_SingleList, 80+ 6, 40+i*40+22, 40,40,'<<<LEER>>>',fnt_Game, kaLeft, $FFD0D0D0);
        Label_SingleSize[i]    := TKMLabel.Create(Panel_SingleList,380+20, 40+i*40+14, 40,40,'0',fnt_Metal, kaCenter);

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

      Label_SingleTitle:=TKMLabel.Create(Panel_SingleDesc,445 div 2,35,420,180,'',fnt_Outline, kaCenter);
      Label_SingleTitle.AutoWrap:=true;

      Label_SingleDesc:=TKMLabel.Create(Panel_SingleDesc,15,60,420,160,'',fnt_Metal, kaLeft);
      Label_SingleDesc.AutoWrap:=true;

      TKMBevel.Create(Panel_SingleDesc,125,230,192,192);

      TKMBevel.Create(Panel_SingleDesc,0,428,445,20);
      Label_SingleCondTyp:=TKMLabel.Create(Panel_SingleDesc,8,431,445,20,fTextLibrary.GetRemakeString(14),fnt_Metal, kaLeft);
      TKMBevel.Create(Panel_SingleDesc,0,450,445,20);
      Label_SingleCondWin:=TKMLabel.Create(Panel_SingleDesc,8,453,445,20,fTextLibrary.GetRemakeString(15),fnt_Metal, kaLeft);
      TKMBevel.Create(Panel_SingleDesc,0,472,445,20);
      Label_SingleCondDef:=TKMLabel.Create(Panel_SingleDesc,8,475,445,20,fTextLibrary.GetRemakeString(16),fnt_Metal, kaLeft);
      TKMBevel.Create(Panel_SingleDesc,0,494,445,20);
      Label_SingleAllies:=TKMLabel.Create(Panel_SingleDesc,8,497,445,20,fTextLibrary.GetRemakeString(17),fnt_Metal, kaLeft);
      TKMBevel.Create(Panel_SingleDesc,0,516,445,20);
      Label_SingleEnemies:=TKMLabel.Create(Panel_SingleDesc,8,519,445,20,fTextLibrary.GetRemakeString(18),fnt_Metal, kaLeft);

    Button_SingleBack := TKMButton.Create(Panel_Single, 45, 650, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_SingleBack.OnClick := SwitchMenuPage;
    Button_SingleStart := TKMButton.Create(Panel_Single, 270, 650, 220, 30, fTextLibrary.GetSetupString(8), fnt_Metal, bsMenu);
    Button_SingleStart.OnClick := SingleMap_Start;
end;


procedure TKMMainMenuInterface.Create_Load_Page;
var i:integer;
begin
  Panel_Load:=TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);
    with TKMImage.Create(Panel_Load,50,220,round(218*1.3),round(291*1.3),5,6) do ImageStretch;
    with TKMImage.Create(Panel_Load,705,220,round(207*1.3),round(295*1.3),6,6) do ImageStretch;

    for i:=1 to SAVEGAME_COUNT do
    begin
      Button_Load[i] := TKMButton.Create(Panel_Load,337,110+i*40,350,30,fTextLibrary.GetRemakeString(19)+inttostr(i),fnt_Metal, bsMenu);
      Button_Load[i].Tag := i; //To simplify usage
      Button_Load[i].OnClick := Load_Click;
    end;

    Button_LoadBack := TKMButton.Create(Panel_Load, 337, 650, 350, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_LoadBack.OnClick := SwitchMenuPage;
end;


//Should contain options to make a map from scratch, load map from file, generate new one
procedure TKMMainMenuInterface.Create_MapEditor_Page;
var i:integer;
begin
  Panel_MapEd:=TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);
    Panel_MapEd_SizeXY := TKMPanel.Create(Panel_MapEd, 462-210, 200, 200, 300);
      TKMLabel.Create(Panel_MapEd_SizeXY, 6, 0, 100, 30, fTextLibrary.GetRemakeString(20), fnt_Outline, kaLeft);
      TKMBevel.Create(Panel_MapEd_SizeXY, 0, 20, 200, 40 + MAPSIZES_COUNT*20);
      TKMLabel.Create(Panel_MapEd_SizeXY, 8, 27, 100, 30, fTextLibrary.GetRemakeString(21), fnt_Outline, kaLeft);
      TKMLabel.Create(Panel_MapEd_SizeXY, 108, 27, 100, 30, fTextLibrary.GetRemakeString(22), fnt_Outline, kaLeft);
      for i:=1 to MAPSIZES_COUNT do
      begin
        CheckBox_MapEd_SizeX[i] := TKMCheckBox.Create(Panel_MapEd_SizeXY, 8, 52+(i-1)*20, 100, 30, inttostr(MapSize[i]),fnt_Metal);
        CheckBox_MapEd_SizeY[i] := TKMCheckBox.Create(Panel_MapEd_SizeXY, 108, 52+(i-1)*20, 100, 30, inttostr(MapSize[i]),fnt_Metal);
        CheckBox_MapEd_SizeX[i].OnClick := MapEditor_Change;
        CheckBox_MapEd_SizeY[i].OnClick := MapEditor_Change;
      end;
      Button_MapEd_Create := TKMButton.Create(Panel_MapEd_SizeXY, 0, 285, 200, 30, fTextLibrary.GetRemakeString(23), fnt_Metal, bsMenu);
      Button_MapEd_Create.OnClick := MapEditor_Start;

    Panel_MapEd_Load := TKMPanel.Create(Panel_MapEd, 462+10, 200, 300, 300);
      TKMLabel.Create(Panel_MapEd_Load, 6, 0, 100, 30, fTextLibrary.GetRemakeString(24), fnt_Outline, kaLeft);
      FileList_MapEd := TKMFileList.Create(Panel_MapEd_Load, 0, 20, 300, 240);
      Button_MapEd_Load := TKMButton.Create(Panel_MapEd_Load, 0, 285, 300, 30, fTextLibrary.GetRemakeString(25), fnt_Metal, bsMenu);
      Button_MapEd_Load.OnClick := MapEditor_Start;

    Button_MapEdBack := TKMButton.Create(Panel_MapEd, 120, 650, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_MapEdBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Options_Page(aGameSettings:TGlobalSettings);
var i:integer;
begin
  Panel_Options:=TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);
    with TKMImage.Create(Panel_Options,705,220,round(207*1.3),round(295*1.3),6,6) do ImageStretch;

    Panel_Options_Ctrl:=TKMPanel.Create(Panel_Options,120,130,200,80);
      TKMLabel.Create(Panel_Options_Ctrl,6,0,100,30,fTextLibrary.GetRemakeString(26),fnt_Outline,kaLeft);
      TKMBevel.Create(Panel_Options_Ctrl,0,20,200,60);

      Label_Options_MouseSpeed:=TKMLabel.Create(Panel_Options_Ctrl,18,27,100,30,fTextLibrary.GetTextString(192),fnt_Metal,kaLeft);
      Label_Options_MouseSpeed.Disable;
      Ratio_Options_Mouse:=TKMRatioRow.Create(Panel_Options_Ctrl,10,47,180,20,aGameSettings.SlidersMin,aGameSettings.SlidersMax);
      Ratio_Options_Mouse.Disable;

    Panel_Options_Game:=TKMPanel.Create(Panel_Options,120,230,200,50);
      TKMLabel.Create(Panel_Options_Game,6,0,100,30,fTextLibrary.GetRemakeString(27),fnt_Outline,kaLeft);
      TKMBevel.Create(Panel_Options_Game,0,20,200,30);

      CheckBox_Options_Autosave := TKMCheckBox.Create(Panel_Options_Game,12,27,100,30,fTextLibrary.GetTextString(203), fnt_Metal);
      CheckBox_Options_Autosave.OnClick := Options_Change;

    Panel_Options_Sound:=TKMPanel.Create(Panel_Options,120,300,200,150);
      TKMLabel.Create(Panel_Options_Sound,6,0,100,30,fTextLibrary.GetRemakeString(28),fnt_Outline,kaLeft);
      TKMBevel.Create(Panel_Options_Sound,0,20,200,130);

      Label_Options_SFX:=TKMLabel.Create(Panel_Options_Sound,18,27,100,30,fTextLibrary.GetTextString(194),fnt_Metal,kaLeft);
      Ratio_Options_SFX:=TKMRatioRow.Create(Panel_Options_Sound,10,47,180,20,aGameSettings.SlidersMin,aGameSettings.SlidersMax);
      Ratio_Options_SFX.OnChange:=Options_Change;
      Label_Options_Music:=TKMLabel.Create(Panel_Options_Sound,18,77,100,30,fTextLibrary.GetTextString(196),fnt_Metal,kaLeft);
      Ratio_Options_Music:=TKMRatioRow.Create(Panel_Options_Sound,10,97,180,20,aGameSettings.SlidersMin,aGameSettings.SlidersMax);
      Ratio_Options_Music.OnChange:=Options_Change;
      CheckBox_Options_MusicOn := TKMCheckBox.Create(Panel_Options_Sound,12,127,100,30,'Disable', fnt_Metal);
      CheckBox_Options_MusicOn.OnClick := Options_Change;

    Panel_Options_GFX:=TKMPanel.Create(Panel_Options,340,130,200,80);
      TKMLabel.Create(Panel_Options_GFX,6,0,100,30,fTextLibrary.GetRemakeString(29),fnt_Outline,kaLeft);
      TKMBevel.Create(Panel_Options_GFX,0,20,200,60);
      TKMLabel.Create(Panel_Options_GFX,18,27,100,30,fTextLibrary.GetRemakeString(30),fnt_Metal,kaLeft);
      Ratio_Options_Brightness:=TKMRatioRow.Create(Panel_Options_GFX,10,47,180,20,aGameSettings.SlidersMin,aGameSettings.SlidersMax);
      Ratio_Options_Brightness.OnChange:=Options_Change;

    Panel_Options_Res:=TKMPanel.Create(Panel_Options,340,230,200,30+RESOLUTION_COUNT*20);
      //Resolution selector
      TKMLabel.Create(Panel_Options_Res,6,0,100,30,fTextLibrary.GetSetupString(20),fnt_Outline,kaLeft);
      TKMBevel.Create(Panel_Options_Res,0,20,200,10+RESOLUTION_COUNT*20);
      for i:=1 to RESOLUTION_COUNT do
      begin
        CheckBox_Options_Resolution[i]:=TKMCheckBox.Create(Panel_Options_Res,12,27+(i-1)*20,100,30,Format('%dx%d',[SupportedResolutions[i,1],SupportedResolutions[i,2],SupportedRefreshRates[i]]),fnt_Metal);
        CheckBox_Options_Resolution[i].Enabled:=(SupportedRefreshRates[i] > 0);
        CheckBox_Options_Resolution[i].OnClick:=Options_Change;
      end;

      CheckBox_Options_FullScreen:=TKMCheckBox.Create(Panel_Options_Res,12,38+RESOLUTION_COUNT*20,100,30,fTextLibrary.GetRemakeString(31),fnt_Metal);
      CheckBox_Options_FullScreen.OnClick:=Options_Change;

      Button_Options_ResApply:=TKMButton.Create(Panel_Options_Res,10,58+RESOLUTION_COUNT*20,180,30,fTextLibrary.GetRemakeString(32),fnt_Metal, bsMenu);
      Button_Options_ResApply.OnClick:=Options_Change;
      Button_Options_ResApply.Disable;


    Panel_Options_Lang:=TKMPanel.Create(Panel_Options,560,130,200,30+LOCALES_COUNT*20);
      TKMLabel.Create(Panel_Options_Lang,6,0,100,30,fTextLibrary.GetRemakeString(33),fnt_Outline,kaLeft);
      TKMBevel.Create(Panel_Options_Lang,0,20,200,10+LOCALES_COUNT*20);

      Radio_Options_Lang := TKMRadioGroup.Create(Panel_Options_Lang, 12, 27, 100, 20*LOCALES_COUNT, fnt_Metal);
      for i:=1 to LOCALES_COUNT do Radio_Options_Lang.Items.Add(Locales[i,2]);
      Radio_Options_Lang.OnClick := Options_Change;

    Button_Options_Back:=TKMButton.Create(Panel_Options,120,650,220,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_Options_Back.OnClick:=SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Credits_Page;
begin
  Panel_Credits:=TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);

    TKMLabel.Create(Panel_Credits,200,100,100,30,'KaM Remake Credits',fnt_Outline,kaCenter);
    TKMLabel.Create(Panel_Credits,200,140,100,30,
    'PROGRAMMING:|Krom|Lewin||'+
    'ADDITIONAL PROGRAMMING:|Alex||'+
    'FRENCH TRANSLATION:|Sylvain Domange||'+
    'SLOVAK TRANSLATION:|Robert Marko||'+
    'SPECIAL THANKS:|KaM Community members'
    ,fnt_Grey,kaCenter);

    TKMLabel.Create(Panel_Credits,ScreenX div 2+150,100,100,30,'Knights & Merchants Credits',fnt_Outline,kaCenter);
    Label_Credits:=TKMLabel.Create(Panel_Credits,ScreenX div 2+150,140,200,ScreenY-140,fTextLibrary.GetSetupString(300),fnt_Grey,kaCenter);

    Button_CreditsBack:=TKMButton.Create(Panel_Credits,120,640,224,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_CreditsBack.OnClick:=SwitchMenuPage;
end;
                      

procedure TKMMainMenuInterface.Create_Loading_Page;
begin
  Panel_Loading:=TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);
    TKMLabel.Create(Panel_Loading,ScreenX div 2,ScreenY div 2 - 20,100,30,fTextLibrary.GetRemakeString(34),fnt_Outline,kaCenter);
    Label_Loading:=TKMLabel.Create(Panel_Loading,ScreenX div 2,ScreenY div 2+10,100,30,'...',fnt_Grey,kaCenter);
end;


procedure TKMMainMenuInterface.Create_Error_Page;
begin
  Panel_Error := TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);
    TKMLabel.Create(Panel_Error,ScreenX div 2,ScreenY div 2 - 20,100,30,fTextLibrary.GetRemakeString(35),fnt_Antiqua,kaCenter);
    Label_Error := TKMLabel.Create(Panel_Error,ScreenX div 2,ScreenY div 2+10,100,30,'...',fnt_Grey,kaCenter);
    Button_ErrorBack := TKMButton.Create(Panel_Error,100,640,224,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_ErrorBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Results_Page;
var i:integer; Adv:integer;
begin
  Panel_Results:=TKMPanel.Create(Panel_Main,0,0,ScreenX,ScreenY);
    with TKMImage.Create(Panel_Results,0,0,ScreenX,ScreenY,7,5) do ImageStretch;

    Label_Results_Result:=TKMLabel.Create(Panel_Results,512,200,100,30,'<<<LEER>>>',fnt_Metal,kaCenter);

    Panel_Stats:=TKMPanel.Create(Panel_Results,80,240,400,400);
    Adv:=0;
    for i:=1 to 9 do
    begin
      inc(Adv,25);
      if i in [3,6,7,9] then inc(Adv,15);
      TKMLabel.Create(Panel_Stats,0,Adv,100,30,fTextLibrary.GetSetupString(112+i),fnt_Metal,kaLeft);
      Label_Stat[i]:=TKMLabel.Create(Panel_Stats,340,Adv,100,30,'00',fnt_Metal,kaRight);
    end;

    Button_ResultsBack:=TKMButton.Create(Panel_Results,100,640,220,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_ResultsBack.OnClick:=SwitchMenuPage;
    Button_ResultsRepeat:=TKMButton.Create(Panel_Results,340,640,220,30,fTextLibrary.GetSetupString(18),fnt_Metal,bsMenu);
    Button_ResultsRepeat.OnClick:=MainMenu_ReplayLastMap;
    Button_ResultsContinue:=TKMButton.Create(Panel_Results,580,640,220,30,fTextLibrary.GetSetupString(17),fnt_Metal,bsMenu);
    Button_ResultsContinue.OnClick:=SwitchMenuPage;
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
     (Sender=Button_ResultsBack) then
    Panel_MainMenu.Show;

  {Stop the network when the player exits the lobby screen}
  if Sender=Button_LobbyBack then
  begin
    LAN_QuitLobby;
    LAN_Update;
    Panel_LANLogin.Show;
  end;

  {Return to MultiPlayerMenu}
  if Sender=Button_LAN_LoginBack then
    Panel_MultiPlayer.Show;

  {Return to MainMenu and restore resolution changes}
  if Sender=Button_Options_Back then begin
    fGame.GlobalSettings.FullScreen := OldFullScreen;
    fGame.GlobalSettings.ResolutionID := OldResolution;
    Panel_MainMenu.Show;
  end;

  {Show SinglePlayer menu}
  {Return to SinglePlayerMenu}
  if (Sender=Button_MM_SinglePlayer)or
     (Sender=Button_CampaignBack)or
     (Sender=Button_SingleBack)or
     (Sender=Button_LoadBack) then begin
    Panel_SinglePlayer.Show;
    Button_SP_Replay.Enabled := fGame.ReplayExists;
  end;

  {Show TSK campaign menu}
  if (Sender=Button_SP_TSK) or ((Sender=Button_ResultsContinue) and (fGame.GetCampaign=cmp_TSK)) then begin
    Campaign_Set(cmp_TSK);
    Panel_Campaign.Show;
  end;

  {Show TSK campaign menu}
  if (Sender=Button_SP_TPR) or ((Sender=Button_ResultsContinue) and (fGame.GetCampaign=cmp_TPR)) then begin
    Campaign_Set(cmp_TPR);
    Panel_Campaign.Show;
  end;

  {Show SingleMap menu}
  if Sender=Button_SP_Single then begin
    SingleMap_PopulateList;
    SingleMap_RefreshList;
    SingleMap_SelectMap(Shape_SingleOverlay[0]); //Select first entry
    Panel_Single.Show;
  end;

  {Show Load menu}
  if Sender=Button_SP_Load then begin
    Load_RefreshList;
    Panel_Load.Show;
  end;

  {Show MultiPlayer menu}
  if Sender=Button_MM_MultiPlayer then begin
    Panel_MultiPlayer.Show;
  end;

  {Show LAN login}
  if Sender=Button_MP_LAN then begin
    LAN_Update;
    Panel_LANLogin.Show;
  end;

  { Lobby }
  if (Sender=Button_LAN_Host) or (Sender=Button_LAN_Join) then begin
    Lobby_Reset(Sender);
    MyControls.CtrlFocus := Edit_LobbyPost;
    Panel_Lobby.Show;
  end;

  {Show MapEditor menu}
  if Sender=Button_MM_MapEd then begin
    FileList_MapEd.RefreshList(ExeDir+'Maps\', 'dat', true); //Refresh each time we go here
    if FileList_MapEd.fFiles.Count > 0 then
      FileList_MapEd.ItemIndex := 0; //Select first map by default
    MapEditor_Change(nil);
    Panel_MapEd.Show;
  end;

  {Show Options menu}
  if Sender=Button_MM_Options then begin
    OldFullScreen := fGame.GlobalSettings.FullScreen;
    OldResolution := fGame.GlobalSettings.ResolutionID;
    Options_Fill;
    Panel_Options.Show;
  end;

  {Show Credits}
  if Sender=Button_MM_Credits then begin
    Panel_Credits.Show;
    Label_Credits.SmoothScrollToTop := TimeGetTime; //Set initial position
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

  { Save settings when leaving options, if needed }
  if Sender=Button_Options_Back then
    fGame.GlobalSettings.SaveSettings;
end;


procedure TKMMainMenuInterface.MainMenu_PlayTutorial(Sender: TObject);
begin
  fGame.GameStart(ExeDir+'data\mission\mission0.dat', 'Town Tutorial');
end;


procedure TKMMainMenuInterface.MainMenu_PlayBattle(Sender: TObject);
begin
  fGame.GameStart(ExeDir+'data\mission\mission99.dat', 'Battle Tutorial');
end;


//Should be done this way since fGame is NIL on UI creation
procedure TKMMainMenuInterface.MainMenu_ReplayView(Sender: TObject);
begin
  fGame.ReplayView;
end;


procedure TKMMainMenuInterface.MainMenu_ReplayLastMap(Sender: TObject);
begin
  fGame.GameStart('', ''); //Means replay last map
end;


procedure TKMMainMenuInterface.Campaign_Set(aCampaign:TCampaign);
var i,Top,Revealed:integer;
begin
  Campaign_Selected := aCampaign;
  Top := fGame.CampaignSettings.GetMapsCount(Campaign_Selected);
  Revealed := min(fGame.CampaignSettings.GetUnlockedMaps(Campaign_Selected), Top); //INI could be wrong

  //Choose background
  case Campaign_Selected of
    cmp_TSK: Image_CampaignBG.TexID := 12;
    cmp_TPR: Image_CampaignBG.TexID := 20;
  end;

  //Setup sites
  for i:=1 to length(Image_CampaignNodes) do begin
    Image_CampaignNodes[i].Visible   := i <= Top;
    Image_CampaignNodes[i].TexID     := 10 + byte(i<=Revealed);
    Image_CampaignNodes[i].HighlightOnMouseOver := i <= Revealed;
  end;

  //Place sites
  for i:=1 to Top do
  case Campaign_Selected of
    cmp_TSK:  begin
                Image_CampaignNodes[i].Left := TSK_Campaign_Maps[i,1];
                Image_CampaignNodes[i].Top  := TSK_Campaign_Maps[i,2];
              end;
    cmp_TPR:  begin
                Image_CampaignNodes[i].Left := TPR_Campaign_Maps[i,1];
                Image_CampaignNodes[i].Top  := TPR_Campaign_Maps[i,2];
              end;
  end;

  //todo: Place intermediate nodes between previous and selected mission nodes

  //Select last map to play by 'clicking' last node
  Campaign_SelectMap(Image_CampaignNodes[Revealed]);
end;


procedure TKMMainMenuInterface.Campaign_SelectMap(Sender:TObject);
var i:integer;
begin
  if not (Sender is TKMImage) then exit;
  if not TKMImage(Sender).HighlightOnMouseOver then exit; //Skip closed maps

   //Place highlight
  for i:=1 to length(Image_CampaignNodes) do
    Image_CampaignNodes[i].Highlight := false;

  TKMImage(Sender).Highlight := true;

  Label_CampaignTitle.Caption := 'Mission '+inttostr(TKMImage(Sender).Tag);
  Label_CampaignText.Caption := fGame.CampaignSettings.GetMapText(Campaign_Selected, TKMImage(Sender).Tag);

  Panel_CampScroll.Height := 50 + Label_CampaignText.TextHeight + 70; //Add offset from top and space on bottom
  Panel_CampScroll.Top := ScreenY - Panel_CampScroll.Height;
  Image_Scroll.Height := Panel_CampScroll.Height;

  Campaign_Mission_Choice := TKMImage(Sender).Tag;
end;


procedure TKMMainMenuInterface.Campaign_StartMap(Sender: TObject);
var MissString,NameString:string;
begin
  fLog.AssertToLog(Sender=Button_CampaignStart,'not Button_CampaignStart');
  case Campaign_Selected of
    cmp_TSK: begin
      MissString := ExeDir+'Data\mission\mission'+inttostr(Campaign_Mission_Choice)+'.dat';
      NameString := 'TSK mission '+inttostr(Campaign_Mission_Choice);
    end;
    cmp_TPR: begin
      MissString := ExeDir+'Data\mission\dmission'+inttostr(Campaign_Mission_Choice)+'.dat';
      NameString := 'TPR mission '+inttostr(Campaign_Mission_Choice);
    end;
    else Assert(false,'Unknown Campaign');
  end;
  fGame.GameStart(MissString, NameString, Campaign_Selected, Campaign_Mission_Choice);
end;


procedure TKMMainMenuInterface.SingleMap_PopulateList;
begin
  SingleMapsInfo.ScanSingleMapsFolder;
end;


procedure TKMMainMenuInterface.SingleMap_RefreshList;
var i,MapID:integer;
begin
  for i:=0 to MENU_SP_MAPS_COUNT-1 do begin
    MapID := SingleMap_Top + i;
    if MapID > SingleMapsInfo.Count-1 then begin
      Image_SingleMode[i].TexID       := 0;
      Label_SinglePlayers[i].Caption  := '';
      Label_SingleTitle1[i].Caption   := '';
      Label_SingleTitle2[i].Caption   := '';
      Label_SingleSize[i].Caption     := '';
    end else begin
      Image_SingleMode[i].TexID       := 28+byte(not SingleMapsInfo[MapID].IsFight)*14;  //28 or 42
      Label_SinglePlayers[i].Caption  := inttostr(SingleMapsInfo[MapID].PlayerCount);
      Label_SingleTitle1[i].Caption   := SingleMapsInfo[MapID].Folder;
      Label_SingleTitle2[i].Caption   := SingleMapsInfo[MapID].SmallDesc;
      Label_SingleSize[i].Caption     := SingleMapsInfo[MapID].MapSize;
    end;
  end;

  ScrollBar_SingleMaps.MaxValue := max(0, SingleMapsInfo.Count - MENU_SP_MAPS_COUNT);
  ScrollBar_SingleMaps.Position := EnsureRange(ScrollBar_SingleMaps.Position,ScrollBar_SingleMaps.MinValue,ScrollBar_SingleMaps.MaxValue);
end;


procedure TKMMainMenuInterface.SingleMap_ScrollChange(Sender: TObject);
begin
  SingleMap_Top := ScrollBar_SingleMaps.Position;
  if InRange(SingleMap_Selected-SingleMap_Top,0,MENU_SP_MAPS_COUNT-1) then
    SingleMap_SelectMap(Shape_SingleOverlay[SingleMap_Selected-SingleMap_Top])
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
    Shape_SingleMap.Show;
    i := TKMControl(Sender).Tag;

    Shape_SingleMap.Top := Bevel_SingleBG[i,3].Height * (i+1); // Including header height

    SingleMap_Selected        := SingleMap_Top+i;
    Label_SingleTitle.Caption := SingleMapsInfo[SingleMap_Selected].Folder;
    Label_SingleDesc.Caption  := SingleMapsInfo[SingleMap_Selected].BigDesc;

    Label_SingleCondTyp.Caption := fTextLibrary.GetRemakeString(14)+SingleMapsInfo[SingleMap_Selected].MissionMode;
    Label_SingleCondWin.Caption := fTextLibrary.GetRemakeString(15)+SingleMapsInfo[SingleMap_Selected].VictoryCondition;
    Label_SingleCondDef.Caption := fTextLibrary.GetRemakeString(16)+SingleMapsInfo[SingleMap_Selected].DefeatCondition;
  end;
end;


procedure TKMMainMenuInterface.SingleMap_Start(Sender: TObject);
begin
  fLog.AssertToLog(Sender=Button_SingleStart,'not Button_SingleStart');
  if not InRange(SingleMap_Selected, 1, SingleMapsInfo.Count) then exit;
  fGame.GameStart(KMMapNameToPath(SingleMapsInfo[SingleMap_Selected].Folder,'dat'),SingleMapsInfo[SingleMap_Selected].Folder); //Provide mission filename mask and title here
end;


procedure TKMMainMenuInterface.LAN_Update;
var s:string;
begin
  s := fGame.Networking.MyIPString;

  Button_LAN_Host.Enabled := s<>'';
  Button_LAN_Join.Enable;

  if s <> '' then Label_LAN_IP.Caption := s
             else Label_LAN_IP.Caption := 'Unknown';
end;


procedure TKMMainMenuInterface.LAN_Host(Sender: TObject);
begin
  SwitchMenuPage(Sender); //Open lobby page

  fGame.Networking.OnTextMessage := Lobby_OnMessage;
  fGame.Networking.OnPlayersSetup := Lobby_OnPlayersSetup;
  fGame.Networking.OnMapName := Lobby_OnMapName;
  fGame.Networking.OnAllReady := Lobby_OnAllReady;
  fGame.Networking.Host('Host');
  fGame.Networking.PostMessage(fGame.Networking.MyIPStringAndPort);
end;


procedure TKMMainMenuInterface.LAN_Join(Sender: TObject);
begin
  //Disable buttons to prevent multiple clicks while connection process is in progress
  Button_LAN_Host.Disable;
  Button_LAN_Join.Disable;
  Label_LAN_Status.Caption := 'Connecting, please wait ...';

  //Send request to join
  fGame.Networking.Join(Edit_LAN_IP.Text, 'Joiner'); //Init lobby
  fGame.Networking.OnJoinSucc := LAN_JoinSucc;
  fGame.Networking.OnJoinFail := LAN_JoinFail;
end;


//We had recieved permission to join
procedure TKMMainMenuInterface.LAN_JoinSucc(Sender: TObject);
begin
  SwitchMenuPage(Button_LAN_Join); //Open lobby page

  fGame.Networking.OnTextMessage := Lobby_OnMessage;
  fGame.Networking.OnPlayersSetup := Lobby_OnPlayersSetup;
  fGame.Networking.OnMapName := Lobby_OnMapName;
  fGame.Networking.PostMessage(fGame.Networking.MyIPStringAndPort);
end;


procedure TKMMainMenuInterface.LAN_JoinFail(const aData:string);
begin
  LAN_Update; //Reset buttons
  Label_LAN_Status.Caption := 'Connection failed: '+aData;
end;


procedure TKMMainMenuInterface.LAN_QuitLobby;
begin
  fGame.Networking.Disconnect;
  LAN_Update; //Reset buttons
  Label_LAN_Status.Caption := 'Disconnected';
end;


procedure TKMMainMenuInterface.Lobby_Reset(Sender: TObject);
var i:integer;
begin
  for i:=0 to MAX_PLAYERS-1 do
    Label_LobbyPlayer[i].Caption := '.';

  ListBox_LobbyPosts.Items.Clear;
  Edit_LobbyPost.Text := '';
  //Button_LobbyStart.Disable;

  if Sender = Button_LAN_Host then begin
    FileList_Lobby.RefreshList(ExeDir+'Maps\', 'dat', true); //Refresh each time we go here
    if FileList_Lobby.fFiles.Count > 0 then
      FileList_Lobby.ItemIndex := 0; //Select first map by default
    FileList_Lobby.Show;
    Button_LobbyReady.Disable;
  end else begin
    FileList_Lobby.Hide;
    Button_LobbyReady.Enable;
  end;
end;


procedure TKMMainMenuInterface.Lobby_PostKey(Sender: TObject; Key: Word);
begin
  if (Key <> VK_RETURN) or (Trim(Edit_LobbyPost.Text) = '') then exit;
  fGame.Networking.PostMessage(Edit_LobbyPost.Text);
  Edit_LobbyPost.Text := '';
end;


procedure TKMMainMenuInterface.Lobby_MapSelect(Sender: TObject);
begin
  fGame.Networking.MapSelect(TruncateExt(ExtractFileName(FileList_Lobby.FileName)));
end;


//Try to change players setup, Networking will check if it can be done under current
//conditions immediately and reverts the change without disturbing Host.
//If the change is possible Networking will send query to the Host.
//Host will reply with OnPlayersSetup event and data will be actual.
procedure TKMMainMenuInterface.Lobby_PlayersSetupChange(Sender: TObject);
var i:integer;
begin
  i := fGame.Networking.MyIndex-1; //0..n Matches index of editable fields

  //Starting location
  if Sender = DropBox_LobbyLoc[i] then
  begin
    fGame.Networking.SelectLoc(DropBox_LobbyLoc[i].ItemIndex);
    DropBox_LobbyLoc[i].ItemIndex := fGame.Networking.NetPlayers[i+1].StartLocID;
  end;

  if Sender = DropBox_LobbyColor[i] then
  begin
    fGame.Networking.SelectColor(DropBox_LobbyColor[i].ItemIndex);
    DropBox_LobbyColor[i].ItemIndex := fGame.Networking.NetPlayers[i+1].FlagColorID;
  end;
end;


//Players list has been updated
//We should reflect it to UI
procedure TKMMainMenuInterface.Lobby_OnPlayersSetup(Sender: TObject);
var i:integer; MyNik:boolean;
begin
  for i:=0 to MAX_PLAYERS-1 do
  begin
    Label_LobbyPlayer[i].Caption := fGame.Networking.NetPlayers[i+1].Nikname;
    DropBox_LobbyLoc[i].ItemIndex := fGame.Networking.NetPlayers[i+1].StartLocID;
    DropBox_LobbyColor[i].ItemIndex := fGame.Networking.NetPlayers[i+1].FlagColorID;
    CheckBox_LobbyReady[i].Checked := fGame.Networking.NetPlayers[i+1].ReadyToStart;

    MyNik := (i+1 = fGame.Networking.MyIndex); //Our index
    DropBox_LobbyLoc[i].Enabled := MyNik;
    DropBox_LobbyColor[i].Enabled := MyNik;
    CheckBox_LobbyReady[i].Enabled := false; //Read-only, just for info (perhaps we will need to replace it with an icon)
    Button_LobbyReady.Enabled := MyNik and fGame.Networking.NetPlayers[i+1].ReadyToStart;
  end;
end;


procedure TKMMainMenuInterface.Lobby_OnMapName(const aData:string);
begin
  //Fill in map info
  Label_LobbyMapName.Caption := aData;

  //Cut DropBox_LobbyLoc according to map max players

  //todo: Keep disabled if Map does not matches Hosts or missing
  Button_LobbyReady.Enabled := true;
end;


procedure TKMMainMenuInterface.Lobby_OnMessage(const aData:string);
begin
  ListBox_LobbyPosts.Items.Add(aData);
end;


procedure TKMMainMenuInterface.Lobby_OnAllReady(Sender: TObject);
begin
  Button_LobbyStart.Enable;
end;


procedure TKMMainMenuInterface.Lobby_ReadyClick(Sender: TObject);
begin
  fGame.Networking.ReadyToStart;
  Button_LobbyReady.Enabled := not fGame.Networking.NetPlayers[fGame.Networking.MyIndex].ReadyToStart;
end;


procedure TKMMainMenuInterface.Lobby_StartClick(Sender: TObject);
begin
  fGame.Networking.StartClick;
end;


procedure TKMMainMenuInterface.Load_Click(Sender: TObject);
var LoadError: string;
begin
  LoadError := fGame.Load(TKMControl(Sender).Tag);
  if LoadError <> '' then ShowScreen(msError, LoadError); //This will show an option to return back to menu
end;


procedure TKMMainMenuInterface.Load_RefreshList;
var i:integer;
begin
  for i:=1 to SAVEGAME_COUNT do
    Button_Load[i].Caption := fGame.SavegameTitle(i);
end;


procedure TKMMainMenuInterface.MapEditor_Start(Sender: TObject);
begin
  if Sender = Button_MapEd_Create then
    fGame.MapEditorStart('', MapEdSizeX, MapEdSizeY); //Provide mission filename here, Mapsize will be ignored if map exists
  if Sender = Button_MapEd_Load then
    fGame.MapEditorStart(FileList_MapEd.FileName, 0, 0); //Provide mission filename here, Mapsize will be ignored if map exists
end;


procedure TKMMainMenuInterface.MapEditor_Change(Sender: TObject);
var i:integer;
begin
  //Find out new map dimensions
  for i:=1 to MAPSIZES_COUNT do
  begin
    if Sender = CheckBox_MapEd_SizeX[i] then MapEdSizeX := MapSize[i];
    if Sender = CheckBox_MapEd_SizeY[i] then MapEdSizeY := MapSize[i];
  end;
  //Put checkmarks
  for i:=1 to MAPSIZES_COUNT do
  begin
    CheckBox_MapEd_SizeX[i].Checked := MapEdSizeX = MapSize[i];
    CheckBox_MapEd_SizeY[i].Checked := MapEdSizeY = MapSize[i];
  end;
end;


//This is called when the options page is shown, so update all the values
procedure TKMMainMenuInterface.Options_Fill;
var i:cardinal;
begin
  CheckBox_Options_Autosave.Checked := fGame.GlobalSettings.Autosave;
  Ratio_Options_Brightness.Position := fGame.GlobalSettings.Brightness;
  Ratio_Options_Mouse.Position      := fGame.GlobalSettings.MouseSpeed;
  Ratio_Options_SFX.Position        := fGame.GlobalSettings.SoundFXVolume;
  Ratio_Options_Music.Position      := fGame.GlobalSettings.MusicVolume;
  CheckBox_Options_MusicOn.Checked  := not fGame.GlobalSettings.MusicOn;

  Ratio_Options_Music.Enabled := not CheckBox_Options_MusicOn.Checked;

  for i:=1 to LOCALES_COUNT do
    if SameText(fGame.GlobalSettings.Locale, Locales[i,1]) then
      Radio_Options_Lang.ItemIndex := i-1;

  CheckBox_Options_FullScreen.Checked := fGame.GlobalSettings.FullScreen;
  for i:=1 to RESOLUTION_COUNT do begin
    CheckBox_Options_Resolution[i].Checked := (i = fGame.GlobalSettings.ResolutionID);
    CheckBox_Options_Resolution[i].Enabled := (SupportedRefreshRates[i] > 0) AND fGame.GlobalSettings.FullScreen;
  end;
end;


procedure TKMMainMenuInterface.Options_Change(Sender: TObject);
var i:cardinal;
begin
  fGame.GlobalSettings.Autosave        := CheckBox_Options_Autosave.Checked;
  fGame.GlobalSettings.Brightness      := Ratio_Options_Brightness.Position;
  fGame.GlobalSettings.MouseSpeed      := Ratio_Options_Mouse.Position;
  fGame.GlobalSettings.SoundFXVolume   := Ratio_Options_SFX.Position;
  fGame.GlobalSettings.MusicVolume     := Ratio_Options_Music.Position;
  fGame.GlobalSettings.MusicOn         := not CheckBox_Options_MusicOn.Checked;
  fGame.GlobalSettings.FullScreen      := CheckBox_Options_FullScreen.Checked;

  Ratio_Options_Music.Enabled := not CheckBox_Options_MusicOn.Checked;

  if Sender = Radio_Options_Lang then begin
    ShowScreen(msLoading, 'Loading new locale');
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

  //Make button enabled only if new resolution/mode differs from old
  Button_Options_ResApply.Enabled := (OldFullScreen <> fGame.GlobalSettings.FullScreen) or (OldResolution <> fGame.GlobalSettings.ResolutionID);
end;


procedure TKMMainMenuInterface.KeyDown(Key:Word; Shift: TShiftState);
begin
  if MyControls.KeyDown(Key, Shift) then exit; //Handled by Controls
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
  if MyControls.CtrlOver is TKMEdit then // Show "CanEdit" cursor
    Screen.Cursor := c_Edit  //todo: Make our own 'Writing' cursor using textures from other cursors
  else
    Screen.Cursor := c_Default;

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
