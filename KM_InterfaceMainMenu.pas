unit KM_InterfaceMainMenu;
{$I KaM_Remake.inc}
interface
uses MMSystem, SysUtils, KromUtils, KromOGLUtils, Math, Classes, Controls,
  {$IFDEF WDC} OpenGL, {$ENDIF}
  {$IFDEF FPC} GL, {$ENDIF}
  KM_Controls, KM_Defaults, KM_CommonTypes, Windows, KM_Settings, KM_MapInfo, KM_Lobby;


type TKMMainMenuInterface = class
  private
    fLobby:TKMLobby;
    ScreenX,ScreenY:word;

    Campaign_Selected:TCampaign;
    Campaign_Mission_Choice:integer;

    SingleMap_Top:integer; //Top map in list
    SingleMap_Selected:integer; //Selected map
    SingleMapsInfo:TKMMapsInfo;
    MapEdSizeX,MapEdSizeY:integer; //Map Editor map size
    OldFullScreen, OldVSync:boolean;
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

    Panel_WWWLogin:TKMPanel;
      Button_WWW_LoginBack:TKMButton;
      Panel_WWWLogin2:TKMPanel;
        Edit_WWW_Login:TKMEdit;
        Edit_WWW_Pass:TKMEdit;
        Label_WWW_IP:TKMLabel;
        Button_WWW_Login:TKMButton;
        Label_WWW_Status:TKMLabel;

    Panel_Lobby:TKMPanel;
      Button_LobbyBack:TKMButton;
      ListBox_LobbyPlayers:TKMListBox;
      //ListBox_LobbyRooms:TKMListBox;
      ListBox_LobbyPosts:TKMListBox;
      Edit_LobbyPost:TKMEdit;
      Button_LobbyPost:TKMButton;


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
      Bevel_SingleBG:array[1..MENU_SP_MAPS_COUNT,1..4]of TKMBevel;
      Image_SingleMode:array[1..MENU_SP_MAPS_COUNT]of TKMImage;
      Label_SinglePlayers,Label_SingleSize,
      Label_SingleTitle1,Label_SingleTitle2:array[1..MENU_SP_MAPS_COUNT]of TKMLabel;
      Shape_SingleOverlay:array[1..MENU_SP_MAPS_COUNT]of TKMShape;
      ScrollBar_SingleMaps:TKMScrollBar;
      Shape_SingleMap:TKMShape;
      Image_SingleScroll1:TKMImage;
      Label_SingleTitle,Label_SingleDesc:TKMLabel;
      Label_SingleCondTyp,Label_SingleCondWin,Label_SingleCondDef:TKMLabel;
      Label_SingleAllies,Label_SingleEnemies:TKMLabel;
      Button_SingleBack,Button_SingleStart:TKMButton;
    Panel_Load:TKMPanel;
      Button_Load:array[1..SAVEGAME_COUNT] of TKMButton;
      Button_LoadBack:TKMButton;
    Panel_MapEd:TKMPanel;
      Panel_MapEd_SizeXY:TKMPanel;
      CheckBox_MapEd_SizeX,CheckBox_MapEd_SizeY:array[1..MAPSIZES_COUNT] of TKMCheckBox;
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
        Button_Options_MusicOn:TKMButton;
      Panel_Options_Lang:TKMPanel;
        CheckBox_Options_Lang:array[1..LOCALES_COUNT] of TKMCheckBox;
      Panel_Options_Res:TKMPanel;
        CheckBox_Options_FullScreen, CheckBox_Options_VSync:TKMCheckBox;
        CheckBox_Options_Resolution:array[1..RESOLUTION_COUNT] of TKMCheckBox;
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
    procedure Create_WWWLogin_Page;
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
    procedure SingleMap_PopulateList();
    procedure SingleMap_RefreshList();
    procedure SingleMap_ScrollChange(Sender: TObject);
    procedure SingleMap_SelectMap(Sender: TObject);
    procedure SingleMap_Start(Sender: TObject);
    procedure MultiPlayer_ShowLogin();
    procedure MultiPlayer_ShowLoginResult(Sender: TObject);
    procedure MultiPlayer_LoginQuery(Sender: TObject);
    procedure MultiPlayer_LoginResult(Sender: TObject);
    procedure MultiPlayer_LobbyPost(Sender: TObject);
    procedure MultiPlayer_RefreshLobby();
    procedure Load_Click(Sender: TObject);
    procedure Load_PopulateList();
    procedure MapEditor_Start(Sender: TObject);
    procedure MapEditor_Change(Sender: TObject);
    procedure Options_Change(Sender: TObject);
  public
    MyControls: TKMControlsCollection;
    constructor Create(X,Y:word; aGameSettings:TGlobalSettings);
    destructor Destroy; override;
    procedure SetScreenSize(X,Y:word);
    procedure ShowScreen_Loading(Text:string);
    procedure ShowScreen_Error(Text:string);
    procedure ShowScreen_Main();
    procedure ShowScreen_Options();
    procedure ShowScreen_Results(Msg:gr_Message);
    procedure Fill_Results();

    procedure MouseMove(X,Y:integer);
    procedure UpdateState;
    procedure Paint;
end;


implementation
uses KM_Unit1, KM_Render, KM_LoadLib, KM_Game, KM_PlayersCollection, Forms, KM_Utils;


constructor TKMMainMenuInterface.Create(X,Y:word; aGameSettings:TGlobalSettings);
begin
  Inherited Create;

  fLog.AssertToLog(fTextLibrary<>nil, 'fTextLibrary should be initialized before MainMenuInterface');

  MyControls := TKMControlsCollection.Create;
  ScreenX := min(X,MENU_DESIGN_X);
  ScreenY := min(Y,MENU_DESIGN_Y);
  Campaign_Mission_Choice := 1;
  SingleMap_Top := 1;
  SingleMap_Selected := 1;
  MapEdSizeX := 64;
  MapEdSizeY := 64;

  Panel_Main := MyControls.AddPanel(nil, (X-MENU_DESIGN_X) div 2,
                                         (Y-MENU_DESIGN_Y) div 2,
                                         ScreenX, ScreenY); //Parent Panel for whole menu

  Create_MainMenu_Page;
  Create_SinglePlayer_Page;
    Create_Campaign_Page;
    Create_Single_Page;
    Create_Load_Page;
  Create_MultiPlayer_Page;
    Create_WWWLogin_Page;
    Create_Lobby_Page;
  Create_MapEditor_Page;
  Create_Options_Page(aGameSettings);
  Create_Credits_Page;
  Create_Loading_Page;
  Create_Error_Page;
  Create_Results_Page;

    {for i:=1 to length(FontFiles) do L[i]:=MyControls.AddLabel(Panel_Main1,550,280+i*20,160,30,'This is a test string for KaM Remake ('+FontFiles[i],TKMFont(i),kaLeft);//}
    //MyControls.AddTextEdit(Panel_Main, 32, 32, 200, 20, fnt_Grey);
    //FL := MyControls.AddFileList(Panel_Main1, 550, 300, 320, 220);
    //FL.RefreshList(ExeDir+'Maps\','dat',true);


  //Show version info on every page
  Label_Version := MyControls.AddLabel(Panel_Main,8,8,100,30,GAME_VERSION+' / OpenGL '+fRender.GetRendererVersion,fnt_Antiqua,kaLeft);

  if SHOW_1024_768_OVERLAY then with MyControls.AddShape(Panel_Main, 0, 0, 1024, 768, $FF00FF00) do Hitable:=false;

  SwitchMenuPage(nil);
  //ShowScreen_Results(); //Put here page you would like to debug
  fLog.AppendLog('Main menu init done');
end;


destructor TKMMainMenuInterface.Destroy;
begin
  if fLobby<>nil then FreeAndNil(fLobby); //If user never went to MP area it will be NIL
  FreeAndNil(SingleMapsInfo);
  FreeAndNil(MyControls);
  Inherited;
end;


procedure TKMMainMenuInterface.SetScreenSize(X, Y:word);
begin
  ScreenX := min(X,MENU_DESIGN_X);
  ScreenY := min(Y,MENU_DESIGN_Y);
  Panel_Main.Left := (X-MENU_DESIGN_X) div 2;
  Panel_Main.Top  := (Y-MENU_DESIGN_Y) div 2;
end;


procedure TKMMainMenuInterface.ShowScreen_Loading(Text:string);
begin
  Label_Loading.Caption := Text;
  SwitchMenuPage(Panel_Loading);
end;


procedure TKMMainMenuInterface.ShowScreen_Error(Text:string);
begin
  Label_Error.Caption := Text;
  SwitchMenuPage(Panel_Error);
end;


procedure TKMMainMenuInterface.ShowScreen_Main();
begin
  SwitchMenuPage(nil);
end;


procedure TKMMainMenuInterface.ShowScreen_Options();
begin
  SwitchMenuPage(Button_MM_Options);
end;


procedure TKMMainMenuInterface.ShowScreen_Results(Msg:gr_Message);
begin
  case Msg of
    gr_Win:    Label_Results_Result.Caption := fTextLibrary.GetSetupString(111);
    gr_Defeat: Label_Results_Result.Caption := fTextLibrary.GetSetupString(112);
    gr_Cancel: Label_Results_Result.Caption := fTextLibrary.GetRemakeString(1);
    else       Label_Results_Result.Caption := '<<<LEER>>>'; //Thats string used in all Synetic games for missing texts =)
  end;

  Button_ResultsRepeat.Enabled := Msg in [gr_Defeat, gr_Cancel];

  //Even if the campaign is complete Player can now return to it's screen to replay any of the maps
  Button_ResultsContinue.Visible := fGame.GetCampaign in [cmp_TSK, cmp_TPR];
  Button_ResultsContinue.Enabled := Msg = gr_Win;

  SwitchMenuPage(Panel_Results);
end;


procedure TKMMainMenuInterface.Fill_Results();
begin
  if (MyPlayer=nil) or (MyPlayer.fMissionSettings=nil) then exit;

  Label_Stat[1].Caption := inttostr(MyPlayer.fMissionSettings.GetUnitsLost);
  Label_Stat[2].Caption := inttostr(MyPlayer.fMissionSettings.GetUnitsKilled);
  Label_Stat[3].Caption := inttostr(MyPlayer.fMissionSettings.GetHousesLost);
  Label_Stat[4].Caption := inttostr(MyPlayer.fMissionSettings.GetHousesDestroyed);
  Label_Stat[5].Caption := inttostr(MyPlayer.fMissionSettings.GetHousesConstructed);
  Label_Stat[6].Caption := inttostr(MyPlayer.fMissionSettings.GetUnitsTrained);
  Label_Stat[7].Caption := inttostr(MyPlayer.fMissionSettings.GetWeaponsProduced);
  Label_Stat[8].Caption := inttostr(MyPlayer.fMissionSettings.GetSoldiersTrained);
  Label_Stat[9].Caption := int2time(fGame.GetMissionTime);
end;


procedure TKMMainMenuInterface.Create_MainMenu_Page;
begin
  Panel_MainMenu:=MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);
    with MyControls.AddImage(Panel_MainMenu,0,0,ScreenX,ScreenY,2,6) do ImageStretch;
    MyControls.AddImage(Panel_MainMenu,300,60,423,164,4,5);
    MyControls.AddLabel(Panel_MainMenu, 512, 240, 100, 20, 'Remake', fnt_Metal, kaCenter);
    with MyControls.AddImage(Panel_MainMenu,50,220,round(218*1.3),round(291*1.3),5,6) do ImageStretch;
    with MyControls.AddImage(Panel_MainMenu,705,220,round(207*1.3),round(295*1.3),6,6) do ImageStretch;

    Panel_MMButtons:=MyControls.AddPanel(Panel_MainMenu,337,290,350,400);
      Button_MM_SinglePlayer := MyControls.AddButton(Panel_MMButtons,0,  0,350,30,fTextLibrary.GetRemakeString(4),fnt_Metal,bsMenu);
      Button_MM_MultiPlayer  := MyControls.AddButton(Panel_MMButtons,0, 40,350,30,fTextLibrary.GetSetupString(11),fnt_Metal,bsMenu);
      Button_MM_MapEd        := MyControls.AddButton(Panel_MMButtons,0, 80,350,30,fTextLibrary.GetRemakeString(5),fnt_Metal,bsMenu);
      Button_MM_Options      := MyControls.AddButton(Panel_MMButtons,0,120,350,30,fTextLibrary.GetSetupString(12),fnt_Metal,bsMenu);
      Button_MM_Credits      := MyControls.AddButton(Panel_MMButtons,0,160,350,30,fTextLibrary.GetSetupString(13),fnt_Metal,bsMenu);
      Button_MM_Quit         := MyControls.AddButton(Panel_MMButtons,0,320,350,30,fTextLibrary.GetSetupString(14),fnt_Metal,bsMenu);
      Button_MM_SinglePlayer.OnClick := SwitchMenuPage;
      Button_MM_MultiPlayer.OnClick  := SwitchMenuPage;
      Button_MM_MapEd.OnClick        := SwitchMenuPage;
      Button_MM_Options.OnClick      := SwitchMenuPage;
      Button_MM_Credits.OnClick      := SwitchMenuPage;
      Button_MM_Quit.OnClick         := Form1.Exit1.OnClick;

      Button_MM_MapEd.Visible := SHOW_MAPED_IN_MENU; //Let it be created, but hidden, I guess there's no need to seriously block it
      Button_MM_MultiPlayer.Enabled :=  ENABLE_MP_IN_MENU
end;


procedure TKMMainMenuInterface.Create_SinglePlayer_Page;
begin
  Panel_SinglePlayer:=MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);
    with MyControls.AddImage(Panel_SinglePlayer,0,0,ScreenX,ScreenY,2,6) do ImageStretch;
    MyControls.AddImage(Panel_SinglePlayer,300,60,423,164,4,5);
    MyControls.AddLabel(Panel_SinglePlayer, 512, 240, 100, 20, 'Remake', fnt_Metal, kaCenter);
    with MyControls.AddImage(Panel_SinglePlayer,50,220,round(218*1.3),round(291*1.3),5,6) do ImageStretch;
    with MyControls.AddImage(Panel_SinglePlayer,705,220,round(207*1.3),round(295*1.3),6,6) do ImageStretch;

    Panel_SPButtons:=MyControls.AddPanel(Panel_SinglePlayer,337,290,350,400);
      Button_SP_Tutor  :=MyControls.AddButton(Panel_SPButtons,0,  0,350,30,fTextLibrary.GetRemakeString(2),fnt_Metal,bsMenu);
      Button_SP_Fight  :=MyControls.AddButton(Panel_SPButtons,0, 40,350,30,fTextLibrary.GetRemakeString(3),fnt_Metal,bsMenu);
      Button_SP_TSK    :=MyControls.AddButton(Panel_SPButtons,0, 80,350,30,fTextLibrary.GetSetupString( 1),fnt_Metal,bsMenu);
      Button_SP_TPR    :=MyControls.AddButton(Panel_SPButtons,0,120,350,30,fTextLibrary.GetSetupString( 2),fnt_Metal,bsMenu);
      Button_SP_Single :=MyControls.AddButton(Panel_SPButtons,0,160,350,30,fTextLibrary.GetSetupString( 4),fnt_Metal,bsMenu);
      Button_SP_Load   :=MyControls.AddButton(Panel_SPButtons,0,200,350,30,fTextLibrary.GetSetupString(10),fnt_Metal,bsMenu);
      Button_SP_Replay :=MyControls.AddButton(Panel_SPButtons,0,240,350,30,fTextLibrary.GetRemakeString(6),fnt_Metal,bsMenu);
      Button_SP_Back   :=MyControls.AddButton(Panel_SPButtons,0,320,350,30,fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);

      Button_SP_Tutor.OnClick    := MainMenu_PlayTutorial;
      Button_SP_Fight.OnClick    := MainMenu_PlayBattle;
      Button_SP_TSK.OnClick      := SwitchMenuPage;
      Button_SP_TPR.OnClick      := SwitchMenuPage;
      Button_SP_Single.OnClick   := SwitchMenuPage;
      Button_SP_Load.OnClick     := SwitchMenuPage;
      Button_SP_Replay.OnClick   := MainMenu_ReplayView; //Should be done this way since fGame is NIL yet
      Button_SP_Back.OnClick     := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_MultiPlayer_Page;
begin
  Panel_MultiPlayer := MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);
    with MyControls.AddImage(Panel_MultiPlayer,0,0,ScreenX,ScreenY,2,6) do ImageStretch;
    with MyControls.AddImage(Panel_MultiPlayer,635,220,round(207*1.3),round(295*1.3),6,6) do ImageStretch;

    Panel_MPButtons:=MyControls.AddPanel(Panel_MultiPlayer,155,280,350,400);
      Button_MP_LAN  :=MyControls.AddButton(Panel_MPButtons,0,  0,350,30,fTextLibrary.GetRemakeString(7),fnt_Metal,bsMenu);
      Button_MP_WWW  :=MyControls.AddButton(Panel_MPButtons,0, 40,350,30,fTextLibrary.GetRemakeString(8),fnt_Metal,bsMenu);
      Button_MP_LAN.Disable;
      Button_MP_LAN.OnClick      := SwitchMenuPage;
      Button_MP_WWW.OnClick      := SwitchMenuPage;

    Button_MP_Back := MyControls.AddButton(Panel_MultiPlayer, 45, 650, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_MP_Back.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_WWWLogin_Page;
begin
  Panel_WWWLogin := MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);
    with MyControls.AddImage(Panel_WWWLogin,0,0,ScreenX,ScreenY,2,6) do ImageStretch;

    Panel_WWWLogin2 := MyControls.AddPanel(Panel_WWWLogin,312,280,400,400);

      MyControls.AddLabel(Panel_WWWLogin2, 200, -50, 100, 20, 'Your IP address is:', fnt_Metal, kaCenter);
      Label_WWW_IP := MyControls.AddLabel(Panel_WWWLogin2, 200, -25, 100, 20, '0.0.0.0', fnt_Outline, kaCenter);

      MyControls.AddLabel(Panel_WWWLogin2, 108, 0, 100, 20, fTextLibrary.GetRemakeString(9), fnt_Metal, kaLeft);
      Edit_WWW_Login := MyControls.AddEdit(Panel_WWWLogin2,100,20,200,20,fnt_Grey);
      Edit_WWW_Login.Text := '';
      MyControls.AddLabel(Panel_WWWLogin2, 108, 50, 100, 20, fTextLibrary.GetRemakeString(10), fnt_Metal, kaLeft);
      Edit_WWW_Pass  := MyControls.AddEdit(Panel_WWWLogin2,100,70,200,20,fnt_Grey,true);
      Edit_WWW_Pass.Text := '';
      Button_WWW_Login := MyControls.AddButton(Panel_WWWLogin2, 100, 100, 200, 30, 'Login', fnt_Metal, bsMenu);
      Button_WWW_Login.OnClick := MultiPlayer_LoginQuery;

      Label_WWW_Status := MyControls.AddLabel(Panel_WWWLogin2, 200, 140, 100, 20, ' ... ', fnt_Outline, kaCenter);

    Button_WWW_LoginBack := MyControls.AddButton(Panel_WWWLogin, 45, 650, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_WWW_LoginBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Lobby_Page;
begin
  Panel_Lobby := MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);
    with MyControls.AddImage(Panel_Lobby,0,0,ScreenX,ScreenY,2,6) do ImageStretch;

    //MyControls.AddLabel(Panel_Lobby, 100, 200, 100, 20, 'Rooms list:', fnt_Outline, kaLeft);
    //ListBox_LobbyRooms := MyControls.AddListBox(Panel_Lobby, 100, 220, 200, 150);

                          MyControls.AddLabel  (Panel_Lobby, 100, 200, 100, 20, 'Posts list:', fnt_Outline, kaLeft);
    ListBox_LobbyPosts := MyControls.AddListBox(Panel_Lobby, 100, 220, 600, 300);
                          MyControls.AddLabel  (Panel_Lobby, 100, 530, 100, 20, 'Post message:', fnt_Outline, kaLeft);
    Button_LobbyPost :=   MyControls.AddButton (Panel_Lobby, 100, 550, 30, 20, '>>', fnt_Metal);
    Edit_LobbyPost :=     MyControls.AddEdit   (Panel_Lobby, 130, 550, 570, 20, fnt_Metal);
    Button_LobbyPost.OnClick := MultiPlayer_LobbyPost;


                            MyControls.AddLabel  (Panel_Lobby, 720, 200, 100, 20, 'Players list:', fnt_Outline, kaLeft);
    ListBox_LobbyPlayers := MyControls.AddListBox(Panel_Lobby, 720, 220, 200, 250);

    Button_LobbyBack := MyControls.AddButton(Panel_Lobby, 45, 650, 220, 30, 'Quit lobby', fnt_Metal, bsMenu);
    Button_LobbyBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Campaign_Page;
var i:integer;
begin
  Panel_Campaign:=MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);
    Image_CampaignBG:=MyControls.AddImage(Panel_Campaign,0,0,ScreenX,ScreenY,12,5);
    Image_CampaignBG.ImageStretch;

    for i:=1 to length(Image_CampaignNodes) do begin
      Image_CampaignNodes[i] := MyControls.AddImage(Panel_Campaign, ScreenX div 2, ScreenY div 2,23*2,29*2,10,5);
      Image_CampaignNodes[i].ImageCenter; //I guess it's easier to position them this way
      Image_CampaignNodes[i].OnClick := Campaign_SelectMap;
      Image_CampaignNodes[i].Tag := i;
    end;

  Panel_CampScroll:=MyControls.AddPanel(Panel_Campaign,ScreenX-360,ScreenY-397,360,397);

    Image_Scroll := MyControls.AddImage(Panel_CampScroll, 0, 0,360,397,{15}2,6);
    Image_Scroll.ImageStretch;
    Label_CampaignTitle := MyControls.AddLabel(Panel_CampScroll, 180, 18,100,20, '', fnt_Outline, kaCenter);

    Label_CampaignText := MyControls.AddLabel(Panel_CampScroll, 20, 65, 320, 310, '', fnt_Briefing, kaLeft);
    Label_CampaignText.AutoWrap := true;

  Button_CampaignStart := MyControls.AddButton(Panel_Campaign, ScreenX-220-20, ScreenY-50, 220, 30, fTextLibrary.GetRemakeString(11), fnt_Metal, bsMenu);
  Button_CampaignStart.OnClick := Campaign_StartMap;

  Button_CampaignBack := MyControls.AddButton(Panel_Campaign, 20, ScreenY-50, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
  Button_CampaignBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Single_Page;
var i:integer;
begin
  SingleMapsInfo:=TKMMapsInfo.Create;

  Panel_Single:=MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);

    with MyControls.AddImage(Panel_Single,0,0,ScreenX,ScreenY,2,6) do ImageStretch;

    Panel_SingleList:=MyControls.AddPanel(Panel_Single,512+22,84,445,600);

      ScrollBar_SingleMaps := MyControls.AddScrollBar(Panel_SingleList,420,40,25,MENU_SP_MAPS_COUNT*40, sa_Vertical, bsMenu);
      ScrollBar_SingleMaps.OnChange := SingleMap_ScrollChange;

      Button_SingleHeadMode  := MyControls.AddButton(Panel_SingleList,  0,0, 40,40,42,4,bsMenu);
      Button_SingleHeadTeams := MyControls.AddButton(Panel_SingleList, 40,0, 40,40,31,4,bsMenu);
      Button_SingleHeadTitle := MyControls.AddButton(Panel_SingleList, 80,0,300,40,fTextLibrary.GetRemakeString(12),fnt_Metal,bsMenu);
      Button_SingleHeadSize  := MyControls.AddButton(Panel_SingleList,380,0, 40,40,fTextLibrary.GetRemakeString(13),fnt_Metal,bsMenu);
      with MyControls.AddButton(Panel_SingleList,420,0, 25,40,'',fnt_Metal,bsMenu) do Disable;

      for i:=1 to MENU_SP_MAPS_COUNT do
      begin
        Bevel_SingleBG[i,1] := MyControls.AddBevel(Panel_SingleList,0,  40+(i-1)*40,40,40);
        Bevel_SingleBG[i,2] := MyControls.AddBevel(Panel_SingleList,40, 40+(i-1)*40,40,40);
        Bevel_SingleBG[i,3] := MyControls.AddBevel(Panel_SingleList,80, 40+(i-1)*40,300,40);
        Bevel_SingleBG[i,4] := MyControls.AddBevel(Panel_SingleList,380,40+(i-1)*40,40,40);

        Image_SingleMode[i]    := MyControls.AddImage(Panel_SingleList,  0   ,40+(i-1)*40,40,40,28);
        Image_SingleMode[i].ImageCenter;
        Label_SinglePlayers[i] := MyControls.AddLabel(Panel_SingleList, 40+20,40+(i-1)*40+14,40,40,'0',fnt_Metal, kaCenter);
        Label_SingleTitle1[i]  := MyControls.AddLabel(Panel_SingleList, 80+6 ,40+5+(i-1)*40,40,40,'<<<LEER>>>',fnt_Metal, kaLeft);
        Label_SingleTitle2[i]  := MyControls.AddLabel(Panel_SingleList, 80+6 ,40+22+(i-1)*40,40,40,'<<<LEER>>>',fnt_Game, kaLeft, $FFD0D0D0);
        Label_SingleSize[i]    := MyControls.AddLabel(Panel_SingleList,380+20,40+(i-1)*40+14,40,40,'0',fnt_Metal, kaCenter);

        Shape_SingleOverlay[i] := MyControls.AddShape(Panel_SingleList, 0, 40+(i-1)*40, 420, 40, $00000000);
        Shape_SingleOverlay[i].LineWidth := 0;
        Shape_SingleOverlay[i].Tag := i;
        Shape_SingleOverlay[i].OnClick := SingleMap_SelectMap;
        Shape_SingleOverlay[i].OnMouseWheel := ScrollBar_SingleMaps.MouseWheel;
      end;

      Shape_SingleMap:=MyControls.AddShape(Panel_SingleList,0,40,420,40, $FFFFFF00);
      Shape_SingleMap.OnMouseWheel := ScrollBar_SingleMaps.MouseWheel;

    Panel_SingleDesc:=MyControls.AddPanel(Panel_Single,45,84,445,600);

      MyControls.AddBevel(Panel_SingleDesc,0,0,445,220);

      //Image_SingleScroll1:=MyControls.AddImage(Panel_SingleDesc,0,0,445,220,15,5);
      //Image_SingleScroll1.StretchImage:=true;
      //Image_SingleScroll1.Height:=220; //Need to reset it after stretching is enabled, cos it can't stretch down by default

      Label_SingleTitle:=MyControls.AddLabel(Panel_SingleDesc,445 div 2,35,420,180,'',fnt_Outline, kaCenter);
      Label_SingleTitle.AutoWrap:=true;

      Label_SingleDesc:=MyControls.AddLabel(Panel_SingleDesc,15,60,420,160,'',fnt_Metal, kaLeft);
      Label_SingleDesc.AutoWrap:=true;

      MyControls.AddBevel(Panel_SingleDesc,125,230,192,192);

      MyControls.AddBevel(Panel_SingleDesc,0,428,445,20);
      Label_SingleCondTyp:=MyControls.AddLabel(Panel_SingleDesc,8,431,445,20,fTextLibrary.GetRemakeString(14),fnt_Metal, kaLeft);
      MyControls.AddBevel(Panel_SingleDesc,0,450,445,20);
      Label_SingleCondWin:=MyControls.AddLabel(Panel_SingleDesc,8,453,445,20,fTextLibrary.GetRemakeString(15),fnt_Metal, kaLeft);
      MyControls.AddBevel(Panel_SingleDesc,0,472,445,20);
      Label_SingleCondDef:=MyControls.AddLabel(Panel_SingleDesc,8,475,445,20,fTextLibrary.GetRemakeString(16),fnt_Metal, kaLeft);
      MyControls.AddBevel(Panel_SingleDesc,0,494,445,20);
      Label_SingleAllies:=MyControls.AddLabel(Panel_SingleDesc,8,497,445,20,fTextLibrary.GetRemakeString(17),fnt_Metal, kaLeft);
      MyControls.AddBevel(Panel_SingleDesc,0,516,445,20);
      Label_SingleEnemies:=MyControls.AddLabel(Panel_SingleDesc,8,519,445,20,fTextLibrary.GetRemakeString(18),fnt_Metal, kaLeft);

    Button_SingleBack := MyControls.AddButton(Panel_Single, 45, 650, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_SingleBack.OnClick := SwitchMenuPage;
    Button_SingleStart := MyControls.AddButton(Panel_Single, 270, 650, 220, 30, fTextLibrary.GetSetupString(8), fnt_Metal, bsMenu);
    Button_SingleStart.OnClick := SingleMap_Start;
end;


procedure TKMMainMenuInterface.Create_Load_Page;
var i:integer;
begin
  Panel_Load:=MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);
    with MyControls.AddImage(Panel_Load,0,0,ScreenX,ScreenY,2,6) do ImageStretch;
    with MyControls.AddImage(Panel_Load,50,220,round(218*1.3),round(291*1.3),5,6) do ImageStretch;
    with MyControls.AddImage(Panel_Load,705,220,round(207*1.3),round(295*1.3),6,6) do ImageStretch;

    for i:=1 to SAVEGAME_COUNT do
    begin
      Button_Load[i] := MyControls.AddButton(Panel_Load,337,110+i*40,350,30,fTextLibrary.GetRemakeString(19)+inttostr(i),fnt_Metal, bsMenu);
      Button_Load[i].Tag := i; //To simplify usage
      Button_Load[i].OnClick := Load_Click;
    end;

    Button_LoadBack := MyControls.AddButton(Panel_Load, 337, 650, 350, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_LoadBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_MapEditor_Page;
var i:integer;
begin
  Panel_MapEd:=MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);
    with MyControls.AddImage(Panel_MapEd,0,0,ScreenX,ScreenY,2,6) do ImageStretch;

    //Should contain options to make a map from scratch, load map from file, generate new one

    Panel_MapEd_SizeXY := MyControls.AddPanel(Panel_MapEd, 462-210, 200, 200, 300);
      MyControls.AddLabel(Panel_MapEd_SizeXY, 6, 0, 100, 30, fTextLibrary.GetRemakeString(20), fnt_Outline, kaLeft);
      MyControls.AddBevel(Panel_MapEd_SizeXY, 0, 20, 200, 40 + MAPSIZES_COUNT*20);
      MyControls.AddLabel(Panel_MapEd_SizeXY, 8, 27, 100, 30, fTextLibrary.GetRemakeString(21), fnt_Outline, kaLeft);
      MyControls.AddLabel(Panel_MapEd_SizeXY, 108, 27, 100, 30, fTextLibrary.GetRemakeString(22), fnt_Outline, kaLeft);
      for i:=1 to MAPSIZES_COUNT do
      begin
        CheckBox_MapEd_SizeX[i] := MyControls.AddCheckBox(Panel_MapEd_SizeXY, 8, 52+(i-1)*20, 100, 30, inttostr(MapSize[i]),fnt_Metal);
        CheckBox_MapEd_SizeY[i] := MyControls.AddCheckBox(Panel_MapEd_SizeXY, 108, 52+(i-1)*20, 100, 30, inttostr(MapSize[i]),fnt_Metal);
        CheckBox_MapEd_SizeX[i].OnClick := MapEditor_Change;
        CheckBox_MapEd_SizeY[i].OnClick := MapEditor_Change;
      end;
      Button_MapEd_Create := MyControls.AddButton(Panel_MapEd_SizeXY, 0, 285, 200, 30, fTextLibrary.GetRemakeString(23), fnt_Metal, bsMenu);
      Button_MapEd_Create.OnClick := MapEditor_Start;

    Panel_MapEd_Load := MyControls.AddPanel(Panel_MapEd, 462+10, 200, 300, 300);
      MyControls.AddLabel(Panel_MapEd_Load, 6, 0, 100, 30, fTextLibrary.GetRemakeString(24), fnt_Outline, kaLeft);
      FileList_MapEd := MyControls.AddFileList(Panel_MapEd_Load, 0, 20, 300, 240);
      Button_MapEd_Load := MyControls.AddButton(Panel_MapEd_Load, 0, 285, 300, 30, fTextLibrary.GetRemakeString(25), fnt_Metal, bsMenu);
      Button_MapEd_Load.OnClick := MapEditor_Start;

    Button_MapEdBack := MyControls.AddButton(Panel_MapEd, 120, 650, 220, 30, fTextLibrary.GetSetupString(9), fnt_Metal, bsMenu);
    Button_MapEdBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Options_Page(aGameSettings:TGlobalSettings);
var i:integer;
begin
  Panel_Options:=MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);
    with MyControls.AddImage(Panel_Options,0,0,ScreenX,ScreenY,2,6) do ImageStretch;
    with MyControls.AddImage(Panel_Options,705,220,round(207*1.3),round(295*1.3),6,6) do ImageStretch;

    Panel_Options_Ctrl:=MyControls.AddPanel(Panel_Options,120,130,200,80);
      MyControls.AddLabel(Panel_Options_Ctrl,6,0,100,30,fTextLibrary.GetRemakeString(26),fnt_Outline,kaLeft);
      MyControls.AddBevel(Panel_Options_Ctrl,0,20,200,60);

      Label_Options_MouseSpeed:=MyControls.AddLabel(Panel_Options_Ctrl,18,27,100,30,fTextLibrary.GetTextString(192),fnt_Metal,kaLeft);
      Label_Options_MouseSpeed.Disable;
      Ratio_Options_Mouse:=MyControls.AddRatioRow(Panel_Options_Ctrl,10,47,180,20,aGameSettings.GetSlidersMin,aGameSettings.GetSlidersMax);
      Ratio_Options_Mouse.Disable;

    Panel_Options_Game:=MyControls.AddPanel(Panel_Options,120,230,200,50);
      MyControls.AddLabel(Panel_Options_Game,6,0,100,30,fTextLibrary.GetRemakeString(27),fnt_Outline,kaLeft);
      MyControls.AddBevel(Panel_Options_Game,0,20,200,30);

      CheckBox_Options_Autosave := MyControls.AddCheckBox(Panel_Options_Game,12,27,100,30,fTextLibrary.GetTextString(203), fnt_Metal);
      CheckBox_Options_Autosave.OnClick := Options_Change;

    Panel_Options_Sound:=MyControls.AddPanel(Panel_Options,120,300,200,130);
      MyControls.AddLabel(Panel_Options_Sound,6,0,100,30,fTextLibrary.GetRemakeString(28),fnt_Outline,kaLeft);
      MyControls.AddBevel(Panel_Options_Sound,0,20,200,110);

      Label_Options_SFX:=MyControls.AddLabel(Panel_Options_Sound,18,27,100,30,fTextLibrary.GetTextString(194),fnt_Metal,kaLeft);
      Ratio_Options_SFX:=MyControls.AddRatioRow(Panel_Options_Sound,10,47,180,20,aGameSettings.GetSlidersMin,aGameSettings.GetSlidersMax);
      Ratio_Options_SFX.OnChange:=Options_Change;
      Label_Options_Music:=MyControls.AddLabel(Panel_Options_Sound,18,77,100,30,fTextLibrary.GetTextString(196),fnt_Metal,kaLeft);
      Ratio_Options_Music:=MyControls.AddRatioRow(Panel_Options_Sound,10,97,180,20,aGameSettings.GetSlidersMin,aGameSettings.GetSlidersMax);
      Ratio_Options_Music.OnChange:=Options_Change;

      Label_Options_MusicOn:=MyControls.AddLabel(Panel_Options_Sound,18,140,100,20,fTextLibrary.GetTextString(197),fnt_Outline,kaLeft);
      Button_Options_MusicOn:=MyControls.AddButton(Panel_Options_Sound,10,160,180,30,'',fnt_Metal, bsMenu);
      Button_Options_MusicOn.OnClick:=Options_Change;


    Panel_Options_GFX:=MyControls.AddPanel(Panel_Options,340,130,200,80);
      MyControls.AddLabel(Panel_Options_GFX,6,0,100,30,fTextLibrary.GetRemakeString(29),fnt_Outline,kaLeft);
      MyControls.AddBevel(Panel_Options_GFX,0,20,200,60);
      MyControls.AddLabel(Panel_Options_GFX,18,27,100,30,fTextLibrary.GetRemakeString(30),fnt_Metal,kaLeft);
      Ratio_Options_Brightness:=MyControls.AddRatioRow(Panel_Options_GFX,10,47,180,20,aGameSettings.GetSlidersMin,aGameSettings.GetSlidersMax);
      Ratio_Options_Brightness.OnChange:=Options_Change;

    Panel_Options_Res:=MyControls.AddPanel(Panel_Options,340,230,200,30+RESOLUTION_COUNT*20);
      //Resolution selector
      MyControls.AddLabel(Panel_Options_Res,6,0,100,30,fTextLibrary.GetSetupString(20),fnt_Outline,kaLeft);
      MyControls.AddBevel(Panel_Options_Res,0,20,200,10+RESOLUTION_COUNT*20);
      for i:=1 to RESOLUTION_COUNT do
      begin
        CheckBox_Options_Resolution[i]:=MyControls.AddCheckBox(Panel_Options_Res,12,27+(i-1)*20,100,30,Format('%dx%d',[SupportedResolutions[i,1],SupportedResolutions[i,2],SupportedRefreshRates[i]]),fnt_Metal);
        CheckBox_Options_Resolution[i].Enabled:=(SupportedRefreshRates[i] > 0);
        CheckBox_Options_Resolution[i].OnClick:=Options_Change;
      end;

      CheckBox_Options_FullScreen:=MyControls.AddCheckBox(Panel_Options_Res,12,38+RESOLUTION_COUNT*20,100,30,fTextLibrary.GetRemakeString(31),fnt_Metal);
      CheckBox_Options_FullScreen.OnClick:=Options_Change;

      CheckBox_Options_VSync:=MyControls.AddCheckBox(Panel_Options_Res,12,58+RESOLUTION_COUNT*20,100,30,'VSync',fnt_Metal);
      CheckBox_Options_VSync.OnClick:=Options_Change;

      Button_Options_ResApply:=MyControls.AddButton(Panel_Options_Res,10,88+RESOLUTION_COUNT*20,180,30,fTextLibrary.GetRemakeString(32),fnt_Metal, bsMenu);
      Button_Options_ResApply.OnClick:=Options_Change;
      Button_Options_ResApply.Disable;


    Panel_Options_Lang:=MyControls.AddPanel(Panel_Options,560,130,200,30+LOCALES_COUNT*20);
      MyControls.AddLabel(Panel_Options_Lang,6,0,100,30,fTextLibrary.GetRemakeString(33),fnt_Outline,kaLeft);
      MyControls.AddBevel(Panel_Options_Lang,0,20,200,10+LOCALES_COUNT*20);

      for i:=1 to LOCALES_COUNT do
      begin
        CheckBox_Options_Lang[i]:=MyControls.AddCheckBox(Panel_Options_Lang,12,27+(i-1)*20,100,30,Locales[i,2],fnt_Metal);
        CheckBox_Options_Lang[i].OnClick:=Options_Change;
      end;


    CheckBox_Options_Autosave.Checked := aGameSettings.IsAutosave;
    Ratio_Options_Brightness.Position := aGameSettings.GetBrightness;
    Ratio_Options_Mouse.Position      := aGameSettings.GetMouseSpeed;
    Ratio_Options_SFX.Position        := aGameSettings.GetSoundFXVolume;
    Ratio_Options_Music.Position      := aGameSettings.GetMusicVolume;

    if aGameSettings.IsMusic then Button_Options_MusicOn.Caption:=fTextLibrary.GetTextString(201)
                             else Button_Options_MusicOn.Caption:=fTextLibrary.GetTextString(199);

    Button_Options_Back:=MyControls.AddButton(Panel_Options,120,650,220,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_Options_Back.OnClick:=SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Credits_Page;
begin
  Panel_Credits:=MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);
    with MyControls.AddImage(Panel_Credits,0,0,ScreenX,ScreenY,2,6) do ImageStretch;

    MyControls.AddLabel(Panel_Credits,200,100,100,30,'KaM Remake Credits',fnt_Outline,kaCenter);
    MyControls.AddLabel(Panel_Credits,200,140,100,30,
    'PROGRAMMING:|Krom|Lewin||'+
    'ADDITIONAL PROGRAMMING:|Alex||'+
    'FRENCH TRANSLATION:|Sylvain Domange||'+
    'SLOVAK TRANSLATION:|Robert Marko||'+
    'SPECIAL THANKS:|KaM Community members'
    ,fnt_Grey,kaCenter);

    MyControls.AddLabel(Panel_Credits,ScreenX div 2+150,100,100,30,'Knights & Merchants Credits',fnt_Outline,kaCenter);
    Label_Credits:=MyControls.AddLabel(Panel_Credits,ScreenX div 2+150,140,200,ScreenY-140,fTextLibrary.GetSetupString(300),fnt_Grey,kaCenter);

    Button_CreditsBack:=MyControls.AddButton(Panel_Credits,120,640,224,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_CreditsBack.OnClick:=SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Loading_Page;
begin
  Panel_Loading:=MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);
    with MyControls.AddImage(Panel_Loading,0,0,ScreenX,ScreenY,2,6) do ImageStretch;
    MyControls.AddLabel(Panel_Loading,ScreenX div 2,ScreenY div 2 - 20,100,30,fTextLibrary.GetRemakeString(34),fnt_Outline,kaCenter);
    Label_Loading:=MyControls.AddLabel(Panel_Loading,ScreenX div 2,ScreenY div 2+10,100,30,'...',fnt_Grey,kaCenter);
end;


procedure TKMMainMenuInterface.Create_Error_Page;
begin
  Panel_Error := MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);
    with MyControls.AddImage(Panel_Error,0,0,ScreenX,ScreenY,2,6) do ImageStretch;
    MyControls.AddLabel(Panel_Error,ScreenX div 2,ScreenY div 2 - 20,100,30,fTextLibrary.GetRemakeString(35),fnt_Antiqua,kaCenter);
    Label_Error := MyControls.AddLabel(Panel_Error,ScreenX div 2,ScreenY div 2+10,100,30,'...',fnt_Grey,kaCenter);
    Button_ErrorBack := MyControls.AddButton(Panel_Error,100,640,224,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_ErrorBack.OnClick := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Results_Page;
var i:integer; Adv:integer;
begin
  Panel_Results:=MyControls.AddPanel(Panel_Main,0,0,ScreenX,ScreenY);
    with MyControls.AddImage(Panel_Results,0,0,ScreenX,ScreenY,7,5) do ImageStretch;

    Label_Results_Result:=MyControls.AddLabel(Panel_Results,512,200,100,30,'<<<LEER>>>',fnt_Metal,kaCenter);

    Panel_Stats:=MyControls.AddPanel(Panel_Results,80,240,400,400);
    Adv:=0;
    for i:=1 to 9 do
    begin
      inc(Adv,25);
      if i in [3,6,7,9] then inc(Adv,15);
      MyControls.AddLabel(Panel_Stats,0,Adv,100,30,fTextLibrary.GetSetupString(112+i),fnt_Metal,kaLeft);
      Label_Stat[i]:=MyControls.AddLabel(Panel_Stats,340,Adv,100,30,'00',fnt_Metal,kaRight);
    end;

    Button_ResultsBack:=MyControls.AddButton(Panel_Results,100,640,200,30,fTextLibrary.GetSetupString(9),fnt_Metal,bsMenu);
    Button_ResultsBack.OnClick:=SwitchMenuPage;
    Button_ResultsRepeat:=MyControls.AddButton(Panel_Results,320,640,200,30,fTextLibrary.GetSetupString(18),fnt_Metal,bsMenu);
    Button_ResultsRepeat.OnClick:=MainMenu_ReplayLastMap;
    Button_ResultsContinue:=MyControls.AddButton(Panel_Results,540,640,200,30,fTextLibrary.GetSetupString(17),fnt_Metal,bsMenu);
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

  {Return to MultiPlayerMenu}
  if (Sender=Button_WWW_LoginBack)or
     (Sender=Button_LobbyBack) then
    Panel_MultiPlayer.Show;

  if (Sender=Button_WWW_Login) then begin
    MultiPlayer_RefreshLobby();
    Panel_Lobby.Show;
  end;

  {Return to MainMenu and restore resolution changes}
  if Sender=Button_Options_Back then begin
    fGame.fGlobalSettings.IsFullScreen := OldFullScreen;
    fGame.fGlobalSettings.SetResolutionID := OldResolution;
    fGame.fGlobalSettings.IsVSync := OldVSync;
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
    SingleMap_PopulateList();
    SingleMap_RefreshList();
    SingleMap_SelectMap(Shape_SingleOverlay[1]);
    Panel_Single.Show;
  end;

  {Show Load menu}
  if Sender=Button_SP_Load then begin
    Load_PopulateList();
    Panel_Load.Show;
  end;

  {Show MultiPlayer menu}
  if Sender=Button_MM_MultiPlayer then begin
    Panel_MultiPlayer.Show;
  end;

  {Show MultiPlayer menu}
  if Sender=Button_MP_WWW then begin
    MultiPlayer_ShowLogin();
    Panel_WWWLogin.Show;
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
    OldFullScreen := fGame.fGlobalSettings.IsFullScreen;
    OldResolution := fGame.fGlobalSettings.GetResolutionID;
    OldVSync := fGame.fGlobalSettings.IsVSync;
    Options_Change(nil);
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
    if fGame.fGlobalSettings.GetNeedsSave then
      fGame.fGlobalSettings.SaveSettings;
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
  fGame.ReplayView(nil);
end;


procedure TKMMainMenuInterface.MainMenu_ReplayLastMap(Sender: TObject);
begin
  fGame.GameStart('', ''); //Means replay last map
end;


procedure TKMMainMenuInterface.Campaign_Set(aCampaign:TCampaign);
var i,Top,Revealed:integer;
begin
  Campaign_Selected := aCampaign;
  Top := fGame.fCampaignSettings.GetMapsCount(Campaign_Selected);
  Revealed := min(fGame.fCampaignSettings.GetUnlockedMaps(Campaign_Selected), Top); //INI could be wrong

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
  for i:=1 to length(Image_CampaignNodes) do begin
    Image_CampaignNodes[i].Highlight := false;
    Image_CampaignNodes[i].ImageCenter;
  end;

  TKMImage(Sender).Highlight := true;
  TKMImage(Sender).ImageStretch;

  Label_CampaignTitle.Caption := 'Mission '+inttostr(TKMImage(Sender).Tag);

  Label_CampaignText.Caption := fGame.fCampaignSettings.GetMapText(Campaign_Selected, TKMImage(Sender).Tag);
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


procedure TKMMainMenuInterface.SingleMap_PopulateList();
begin
  SingleMapsInfo.ScanSingleMapsFolder();
end;


procedure TKMMainMenuInterface.SingleMap_RefreshList();
var i,ci:integer;
begin
  for i:=1 to MENU_SP_MAPS_COUNT do begin
    ci:=SingleMap_Top+i-1;
    if ci>SingleMapsInfo.GetMapCount then begin
      Image_SingleMode[i].TexID       := 0;
      Label_SinglePlayers[i].Caption  := '';
      Label_SingleTitle1[i].Caption   := '';
      Label_SingleTitle2[i].Caption   := '';
      Label_SingleSize[i].Caption     := '';
    end else begin
      Image_SingleMode[i].TexID       := 28+byte(not SingleMapsInfo.IsFight(ci))*14;  //28 or 42
      Label_SinglePlayers[i].Caption  := inttostr(SingleMapsInfo.GetPlayerCount(ci));
      Label_SingleTitle1[i].Caption   := SingleMapsInfo.GetFolder(ci);
      Label_SingleTitle2[i].Caption   := SingleMapsInfo.GetSmallDesc(ci);
      Label_SingleSize[i].Caption     := SingleMapsInfo.GetMapSize(ci);
    end;
  end;

  ScrollBar_SingleMaps.MinValue := 1;
  ScrollBar_SingleMaps.MaxValue := max(1, SingleMapsInfo.GetMapCount - MENU_SP_MAPS_COUNT + 1);
  ScrollBar_SingleMaps.Position := EnsureRange(ScrollBar_SingleMaps.Position,ScrollBar_SingleMaps.MinValue,ScrollBar_SingleMaps.MaxValue);
end;


procedure TKMMainMenuInterface.SingleMap_ScrollChange(Sender: TObject);
begin
  SingleMap_Top := ScrollBar_SingleMaps.Position;
  if InRange(1+SingleMap_Selected-SingleMap_Top,1,MENU_SP_MAPS_COUNT) then
    SingleMap_SelectMap(Shape_SingleOverlay[1+SingleMap_Selected-SingleMap_Top])
  else
    SingleMap_SelectMap(nil); //Means it is off visible area
  SingleMap_RefreshList();
end;


procedure TKMMainMenuInterface.SingleMap_SelectMap(Sender: TObject);
var i:integer;
begin
  if Sender = nil then
    Shape_SingleMap.Visible := false //Off visible list
  else
  begin
    Shape_SingleMap.Visible := true;
    i := TKMControl(Sender).Tag;

    Shape_SingleMap.Top := Bevel_SingleBG[i,3].Height * i; // All heights are equal in fact..

    SingleMap_Selected        := SingleMap_Top+i-1;
    Label_SingleTitle.Caption := SingleMapsInfo.GetFolder(SingleMap_Selected);
    Label_SingleDesc.Caption  := SingleMapsInfo.GetBigDesc(SingleMap_Selected);

    Label_SingleCondTyp.Caption := fTextLibrary.GetRemakeString(14)+SingleMapsInfo.GetTyp(SingleMap_Selected);
    Label_SingleCondWin.Caption := fTextLibrary.GetRemakeString(15);//+SingleMapsInfo.GetWin(SingleMap_Selected);
    Label_SingleCondDef.Caption := fTextLibrary.GetRemakeString(16);//+SingleMapsInfo.GetDefeat(SingleMap_Selected);
  end;
end;


procedure TKMMainMenuInterface.SingleMap_Start(Sender: TObject);
begin
  fLog.AssertToLog(Sender=Button_SingleStart,'not Button_SingleStart');
  if not InRange(SingleMap_Selected, 1, SingleMapsInfo.GetMapCount) then exit;
  fGame.GameStart(KMMapNameToPath(SingleMapsInfo.GetFolder(SingleMap_Selected),'dat'),SingleMapsInfo.GetFolder(SingleMap_Selected)); //Provide mission filename mask and title here
end;


{ First page actually related to LobbyChat - create fLobby here}
procedure TKMMainMenuInterface.MultiPlayer_ShowLogin();
begin
  with THTTPPostThread.Create('http://www.whatismyip.com/automation/n09230945.asp','',nil) do begin
    FreeOnTerminate := true;
    OnTerminate := MultiPlayer_ShowLoginResult; //Will get our IP address asynchronously
  end;

  Button_WWW_Login.Disable; //Until after we resolve our IP
end;


procedure TKMMainMenuInterface.MultiPlayer_ShowLoginResult(Sender: TObject);
begin
  Label_WWW_IP.Caption := THTTPPostThread(Sender).ResultMsg;
  THTTPPostThread(Sender).Terminate;

  Edit_WWW_Login.Text := 'Player';
  Button_WWW_Login.Enable;
end;


procedure TKMMainMenuInterface.MultiPlayer_LoginQuery(Sender: TObject);
begin
  fLobby := TKMLobby.Create('http://www.assoft.ru/chat/',
                            Edit_WWW_Login.Text, //Username
                            Edit_WWW_Pass.Text, //Password, ignored
                            '', //was IP address, now ignored by PHP
                            MultiPlayer_LoginResult);

  Button_WWW_Login.Disable; //Should block duplicate clicks
end;


procedure TKMMainMenuInterface.MultiPlayer_LoginResult(Sender: TObject);
begin
  SwitchMenuPage(Button_WWW_Login);
  Button_WWW_Login.Enable;
end;


procedure TKMMainMenuInterface.MultiPlayer_LobbyPost(Sender: TObject);
begin
  fLobby.PostMessage(Edit_LobbyPost.Text);
end;


procedure TKMMainMenuInterface.MultiPlayer_RefreshLobby();
begin
  ListBox_LobbyPlayers.Items.Text := fLobby.PlayersList;
  //ListBox_LobbyRooms.Items.Text := fLobby.RoomsList;
  ListBox_LobbyPosts.Items.Text := fLobby.PostsList;
end;


procedure TKMMainMenuInterface.Load_Click(Sender: TObject);
var LoadError: string;
begin
  LoadError := fGame.Load(TKMControl(Sender).Tag);
  if LoadError <> '' then ShowScreen_Error(LoadError); //This will show an option to return back to menu
end;


procedure TKMMainMenuInterface.Load_PopulateList();
var i:integer;
begin
  for i:=1 to SAVEGAME_COUNT do
    Button_Load[i].Caption := fGame.LoadName(i);
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


procedure TKMMainMenuInterface.Options_Change(Sender: TObject);
var i:integer;
begin
  if Sender = CheckBox_Options_Autosave then fGame.fGlobalSettings.IsAutosave := not CheckBox_Options_Autosave.Checked;
  CheckBox_Options_Autosave.Checked := fGame.fGlobalSettings.IsAutosave;

  if Sender = Ratio_Options_Brightness then fGame.fGlobalSettings.SetBrightness(Ratio_Options_Brightness.Position);
  if Sender = Ratio_Options_Mouse then fGame.fGlobalSettings.SetMouseSpeed(Ratio_Options_Mouse.Position);
  if Sender = Ratio_Options_SFX   then fGame.fGlobalSettings.SetSoundFXVolume(Ratio_Options_SFX.Position);
  if Sender = Ratio_Options_Music then fGame.fGlobalSettings.SetMusicVolume(Ratio_Options_Music.Position);
  if Sender = Button_Options_MusicOn then fGame.fGlobalSettings.IsMusic := not fGame.fGlobalSettings.IsMusic;

  //This is called when the options page is shown, so update all the values
  CheckBox_Options_Autosave.Checked := fGame.fGlobalSettings.IsAutosave;
  Ratio_Options_Brightness.Position := fGame.fGlobalSettings.GetBrightness;
  Ratio_Options_Mouse.Position      := fGame.fGlobalSettings.GetMouseSpeed;
  Ratio_Options_SFX.Position        := fGame.fGlobalSettings.GetSoundFXVolume;
  Ratio_Options_Music.Position      := fGame.fGlobalSettings.GetMusicVolume;
  if fGame.fGlobalSettings.IsMusic then Button_Options_MusicOn.Caption:=fTextLibrary.GetTextString(201)
                                 else Button_Options_MusicOn.Caption:=fTextLibrary.GetTextString(199);

  for i:=1 to LOCALES_COUNT do
    if Sender = CheckBox_Options_Lang[i] then begin
      fGame.fGlobalSettings.SetLocale := Locales[i,1];
      ShowScreen_Loading('Loading new locale');
      fRender.Render; //Force to repaint loading screen
      fGame.ToggleLocale;
      exit; //Whole interface will be recreated
    end;

  for i:=1 to LOCALES_COUNT do
    CheckBox_Options_Lang[i].Checked := LowerCase(fGame.fGlobalSettings.GetLocale) = LowerCase(Locales[i,1]);

  if Sender = Button_Options_ResApply then begin //Apply resolution changes
    OldFullScreen := fGame.fGlobalSettings.IsFullScreen; //memorize (it will be niled on re-init anyway, but we might change that in future)
    OldResolution := fGame.fGlobalSettings.GetResolutionID;
    OldVSync := fGame.fGlobalSettings.IsVSync;
    fGame.ToggleFullScreen(fGame.fGlobalSettings.IsFullScreen, true);
    exit;
  end;

  if Sender = CheckBox_Options_FullScreen then
    fGame.fGlobalSettings.IsFullScreen := not fGame.fGlobalSettings.IsFullScreen;

  if Sender = CheckBox_Options_VSync then
    fGame.fGlobalSettings.IsVSync := not fGame.fGlobalSettings.IsVSync;


  for i:=1 to RESOLUTION_COUNT do
    if Sender = CheckBox_Options_Resolution[i] then
      fGame.fGlobalSettings.SetResolutionID := i;

  CheckBox_Options_FullScreen.Checked := fGame.fGlobalSettings.IsFullScreen;
  CheckBox_Options_VSync.Checked := fGame.fGlobalSettings.IsVSync;
  for i:=1 to RESOLUTION_COUNT do begin
    CheckBox_Options_Resolution[i].Checked := (i = fGame.fGlobalSettings.GetResolutionID);
    CheckBox_Options_Resolution[i].Enabled := (SupportedRefreshRates[i] > 0) AND fGame.fGlobalSettings.IsFullScreen;
  end;

  //Make button enabled only if new resolution/mode differs from old
  Button_Options_ResApply.Enabled := (OldFullScreen <> fGame.fGlobalSettings.IsFullScreen) or (OldVSync <> fGame.fGlobalSettings.IsVSync) or (OldResolution <> fGame.fGlobalSettings.GetResolutionID);

end;


//Do something related to mouse movement in menu
procedure TKMMainMenuInterface.MouseMove(X,Y:integer);
begin
  if (Panel_Campaign.Visible)
 and (Y > Panel_Campaign.Height - Panel_CampScroll.Height) then
      if X < Panel_CampScroll.Width then
        Panel_CampScroll.Left := Panel_Campaign.Width - Panel_CampScroll.Width
      else
      if X > Panel_Campaign.Width - Panel_CampScroll.Width then
        Panel_CampScroll.Left := 0;
end;


{Should update anything we want to be updated, obviously}
procedure TKMMainMenuInterface.UpdateState;
begin
  if fLobby<>nil then begin
    fLobby.UpdateState;
    MultiPlayer_RefreshLobby;
  end;
end;


procedure TKMMainMenuInterface.Paint;
begin
  MyControls.Paint;
end;



end.
