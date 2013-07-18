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
  KM_GUIMenuCampaigns,
  KM_GUIMenuLoad,
  KM_GUIMenuLobby,
  KM_GUIMenuMapEditor,
  KM_GUIMenuMultiplayer,
  KM_GUIMenuOptions,
  KM_GUIMenuResultsMP,
  KM_GUIMenuResultsSP,
  KM_GUIMenuSingleMap;


type
  TMenuScreen = (msError, msLoading, msMain, msOptions);

  TKMMainMenuInterface = class (TKMUserInterface)
  private
    fMaps: TKMapsCollection;
    fMapsMP: TKMapsCollection;
    fSaves: TKMSavesCollection;
    fMinimap: TKMMinimap;

    fLastSaveCRC: Cardinal; //CRC of selected save

    fGuiCampaign: TKMGUIMainCampaign;
    fGuiCampaigns: TKMGUIMainCampaigns;
    fGuiLoad: TKMGUIMenuLoad;
    fGuiLobby: TKMGUIMenuLobby;
    fGuiMapEditor: TKMGUIMainMapEditor;
    fGuiMultiplayer: TKMGUIMainMultiplayer;
    fGuiOptions: TKMGUIMainOptions;
    fGuiSingleMap: TKMGUIMenuSingleMap;
    fGuiResultsMP: TKMGUIMenuResultsMP;
    fGuiResultsSP: TKMGUIMenuResultsSP;
    //fPages: array [TGUIPage] of TKMGUIPage;

    procedure Create_MainMenu;
    procedure Create_SinglePlayer;
    procedure Create_Replays;
    procedure Create_Credits;
    procedure Create_Loading;
    procedure Create_Error;
    procedure PageChange(Sender: TObject; Dest: TGUIPage; aText: string);
    procedure SwitchMenuPage(Sender: TObject);
    procedure MainMenu_MultiplayerClick(Sender: TObject);
    procedure MainMenu_PlayTutorial(Sender: TObject);
    procedure MainMenu_PlayBattle(Sender: TObject);
    procedure Credits_LinkClick(Sender: TObject);

    procedure Replays_ListClick(Sender: TObject);
    procedure Replay_TypeChange(Sender: TObject);
    procedure Replays_ScanUpdate(Sender: TObject);
    procedure Replays_SortUpdate(Sender: TObject);
    procedure Replays_RefreshList(aJumpToSelected:Boolean);
    procedure Replays_Sort(aIndex: Integer);
    procedure Replays_Play(Sender: TObject);
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
    Panel_Replays:TKMPanel;
      Radio_Replays_Type:TKMRadioGroup;
      ColumnBox_Replays: TKMColumnBox;
      Button_ReplaysPlay: TKMButton;
      Button_ReplaysBack:TKMButton;
      MinimapView_Replay: TKMMinimapView;
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
  public
    constructor Create(X,Y: Word);
    destructor Destroy; override;
    procedure ShowScreen(aScreen: TMenuScreen; const aText: string = ''; aMsg: TGameResultMsg=gr_Silent);
    procedure AppendLoadingText(const aText: string);
    procedure ShowResultsMP(aMsg: TGameResultMsg);
    procedure ShowResultsSP(aMsg: TGameResultMsg);
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
uses KM_Main, KM_NetworkTypes, KM_ResTexts, KM_Game, KM_GameApp, KM_ResLocales,
  KM_Utils, KM_Log, KM_Sound, KM_ResSound, KM_Networking, KM_RenderUI, KM_ResFonts;


{ TKMMainMenuInterface }
constructor TKMMainMenuInterface.Create(X,Y: Word);
var S: TKMShape;
begin
  inherited;
  Assert(gResTexts <> nil, 'fTextMain should be initialized before MainMenuInterface');

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
    fGuiCampaigns := TKMGUIMainCampaigns.Create(Panel_Main, PageChange);
      fGuiCampaign := TKMGUIMainCampaign.Create(Panel_Main, PageChange);
    fGuiSingleMap := TKMGUIMenuSingleMap.Create(Panel_Main, PageChange);
    fGuiLoad := TKMGUIMenuLoad.Create(Panel_Main, PageChange);
  fGuiMultiplayer := TKMGUIMainMultiplayer.Create(Panel_Main, PageChange);
    fGuiLobby := TKMGUIMenuLobby.Create(Panel_Main, PageChange);
  fGuiMapEditor := TKMGUIMainMapEditor.Create(Panel_Main, PageChange);
  Create_Replays;
  fGuiOptions := TKMGUIMainOptions.Create(Panel_Main, PageChange);
  Create_Credits;
  Create_Loading;
  Create_Error;
  fGuiResultsMP := TKMGUIMenuResultsMP.Create(Panel_Main, PageChange);
  fGuiResultsSP := TKMGUIMenuResultsSP.Create(Panel_Main, PageChange);


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

  gLog.AddTime('Main menu init done');
//Use ShowScreen to select properscreen after fGame init is done
end;


destructor TKMMainMenuInterface.Destroy;
begin
  fMaps.Free;
  fMapsMP.Free;
  fSaves.Free;
  fMinimap.Free;

  fGuiLobby.Free;
  fGuiLoad.Free;
  fGuiMultiplayer.Free;
  fGuiOptions.Free;
  fGuiSingleMap.Free;
  fGuiCampaign.Free;
  fGuiCampaigns.Free;
  fGuiResultsMP.Free;
  fGuiResultsSP.Free;

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
  fGuiLobby.Lobby_Resize(Panel_Main.Height);
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
  end;
end;


procedure TKMMainMenuInterface.AppendLoadingText(const aText: string);
begin
  Label_Loading.Caption := Label_Loading.Caption + aText + '|';
end;


function TKMMainMenuInterface.GetChatText: string;
begin
  Result := fGuiLobby.GetChatText;
end;


//Access chat messages history to copy it over to gameplay chat
function TKMMainMenuInterface.GetChatMessages: string;
begin
  Result := fGuiLobby.GetChatMessages;
end;


procedure TKMMainMenuInterface.ShowResultsMP(aMsg: TGameResultMsg);
begin
  fGuiResultsMP.Show(aMsg);
end;


procedure TKMMainMenuInterface.ShowResultsSP(aMsg: TGameResultMsg);
begin
  fGuiResultsSP.Show(aMsg);
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
      Button_MM_SinglePlayer := TKMButton.Create(Panel_MMButtons,0,  0,350,30,gResTexts[TX_MENU_SINGLEPLAYER],bsMenu);
      Button_MM_MultiPlayer  := TKMButton.Create(Panel_MMButtons,0, 40,350,30,gResTexts[TX_MENU_MULTIPLAYER],bsMenu);
      Button_MM_MapEd        := TKMButton.Create(Panel_MMButtons,0, 80,350,30,gResTexts[TX_MENU_MAP_EDITOR],bsMenu);
      Button_MM_Replays      := TKMButton.Create(Panel_MMButtons,0,120,350,30,gResTexts[TX_MENU_REPLAYS],bsMenu);
      Button_MM_Options      := TKMButton.Create(Panel_MMButtons,0,160,350,30,gResTexts[TX_MENU_OPTIONS],bsMenu);
      Button_MM_Credits      := TKMButton.Create(Panel_MMButtons,0,200,350,30,gResTexts[TX_MENU_CREDITS],bsMenu);
      Button_MM_Quit         := TKMButton.Create(Panel_MMButtons,0,290,350,30,gResTexts[TX_MENU_QUIT],bsMenu);
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
      Button_SP_Tutor  := TKMButton.Create(Panel_SPButtons,0,  0,350,30,gResTexts[TX_MENU_TUTORIAL_TOWN],bsMenu);
      Button_SP_Fight  := TKMButton.Create(Panel_SPButtons,0, 40,350,30,gResTexts[TX_MENU_TUTORIAL_BATTLE],bsMenu);
      Button_SP_Camp   := TKMButton.Create(Panel_SPButtons,0,100,350,30,gResTexts[TX_MENU_CAMPAIGNS],bsMenu);
      Button_SP_Single := TKMButton.Create(Panel_SPButtons,0,160,350,30,gResTexts[TX_MENU_SINGLE_MAP],bsMenu);
      Button_SP_Load   := TKMButton.Create(Panel_SPButtons,0,200,350,30,gResTexts[TX_MENU_LOAD_SAVEGAME],bsMenu);
      Button_SP_Back   := TKMButton.Create(Panel_SPButtons,0,290,350,30,gResTexts[TX_MENU_BACK],bsMenu);

      Button_SP_Tutor.OnClick  := MainMenu_PlayTutorial;
      Button_SP_Fight.OnClick  := MainMenu_PlayBattle;
      Button_SP_Camp.OnClick   := SwitchMenuPage;
      Button_SP_Single.OnClick := SwitchMenuPage;
      Button_SP_Load.OnClick   := SwitchMenuPage;
      Button_SP_Back.OnClick   := SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Replays;
begin
  Panel_Replays := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_Replays.Stretch;

    TKMLabel.Create(Panel_Replays, Panel_Main.Width div 2, 50, gResTexts[TX_MENU_LOAD_LIST], fnt_Outline, taCenter);

    TKMBevel.Create(Panel_Replays, 62, 86, 900, 50);
    Radio_Replays_Type := TKMRadioGroup.Create(Panel_Replays,70,94,300,40,fnt_Grey);
    Radio_Replays_Type.ItemIndex := 0;
    Radio_Replays_Type.Add(gResTexts[TX_MENU_MAPED_SPMAPS]);
    Radio_Replays_Type.Add(gResTexts[TX_MENU_MAPED_MPMAPS]);
    Radio_Replays_Type.OnChange := Replay_TypeChange;

    ColumnBox_Replays := TKMColumnBox.Create(Panel_Replays, 62, 150, 700, 485, fnt_Metal, bsMenu);
    ColumnBox_Replays.SetColumns(fnt_Outline, [gResTexts[TX_MENU_LOAD_FILE], gResTexts[TX_MENU_LOAD_DESCRIPTION]], [0, 300]);
    ColumnBox_Replays.Anchors := [akLeft,akTop,akBottom];
    ColumnBox_Replays.SearchColumn := 0;
    ColumnBox_Replays.OnChange := Replays_ListClick;
    ColumnBox_Replays.OnColumnClick := Replays_Sort;
    ColumnBox_Replays.OnDoubleClick := Replays_Play;

    Button_ReplaysPlay := TKMButton.Create(Panel_Replays,337,660,350,30,gResTexts[TX_MENU_VIEW_REPLAY], bsMenu);
    Button_ReplaysPlay.Anchors := [akLeft,akBottom];
    Button_ReplaysPlay.OnClick := Replays_Play;

    Button_ReplaysBack := TKMButton.Create(Panel_Replays, 337, 700, 350, 30, gResTexts[TX_MENU_BACK], bsMenu);
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

    TKMLabel.Create(Panel_Credits, Panel_Main.Width div 2 - OFFSET, 70, gResTexts[TX_CREDITS],fnt_Outline,taCenter);
    Label_Credits_Remake := TKMLabelScroll.Create(Panel_Credits, Panel_Main.Width div 2 - OFFSET, 110, 0, Panel_Main.Height - 130,
    gResTexts[TX_CREDITS_PROGRAMMING]+'|Krom|Lewin||'+
    gResTexts[TX_CREDITS_ADDITIONAL_PROGRAMMING]+'|Alex|Danjb||'+
    gResTexts[TX_CREDITS_ADDITIONAL_GRAPHICS]+'|StarGazer|Malin|H.A.H.||'+
    gResTexts[TX_CREDITS_ADDITIONAL_MUSIC]+'|Andre Sklenar - www.juicelab.cz||'+
    gResTexts[TX_CREDITS_ADDITIONAL_SOUNDS]+'|trb1914||'+
    gResTexts[TX_CREDITS_ADDITIONAL_TRANSLATIONS]+'|'+gResLocales.TranslatorCredits+'|'+
    gResTexts[TX_CREDITS_SPECIAL]+'|KaM Community members'
    ,fnt_Grey,taCenter);
    Label_Credits_Remake.Anchors := [akLeft,akTop,akBottom];

    TKMLabel.Create(Panel_Credits, Panel_Main.Width div 2 + OFFSET, 70, gResTexts[TX_CREDITS_ORIGINAL], fnt_Outline, taCenter);
    Label_Credits_KaM := TKMLabelScroll.Create(Panel_Credits, Panel_Main.Width div 2 + OFFSET, 110, 0, Panel_Main.Height - 130, gResTexts[TX_CREDITS_TEXT], fnt_Grey, taCenter);
    Label_Credits_KaM.Anchors := [akLeft,akTop,akBottom];

    Button_CreditsHomepage:=TKMButton.Create(Panel_Credits,400,610,224,30,'[$F8A070]www.kamremake.com[]',bsMenu);
    Button_CreditsHomepage.Anchors := [akLeft,akBottom];
    Button_CreditsHomepage.OnClick:=Credits_LinkClick;

    Button_CreditsFacebook:=TKMButton.Create(Panel_Credits,400,646,224,30,'[$F8A070]Facebook[]',bsMenu);
    Button_CreditsFacebook.Anchors := [akLeft,akBottom];
    Button_CreditsFacebook.OnClick:=Credits_LinkClick;

    Button_CreditsBack:=TKMButton.Create(Panel_Credits,400,700,224,30,gResTexts[TX_MENU_BACK],bsMenu);
    Button_CreditsBack.Anchors := [akLeft,akBottom];
    Button_CreditsBack.OnClick:=SwitchMenuPage;
end;


procedure TKMMainMenuInterface.Create_Loading;
begin
  Panel_Loading:=TKMPanel.Create(Panel_Main,0,0,Panel_Main.Width, Panel_Main.Height);
  Panel_Loading.Stretch;
    with TKMLabel.Create(Panel_Loading, Panel_Main.Width div 2, Panel_Main.Height div 2 - 20, gResTexts[TX_MENU_LOADING], fnt_Outline, taCenter) do
      Center;
    Label_Loading := TKMLabel.Create(Panel_Loading, Panel_Main.Width div 2, Panel_Main.Height div 2+10, '...', fnt_Grey, taCenter);
    Label_Loading.Center;
end;


procedure TKMMainMenuInterface.Create_Error;
begin
  Panel_Error := TKMPanel.Create(Panel_Main, 0, 0, Panel_Main.Width, Panel_Main.Height);
  Panel_Error.Stretch;
    with TKMLabel.Create(Panel_Error, Panel_Main.Width div 2, Panel_Main.Height div 2 - 20, gResTexts[TX_MENU_ERROR], fnt_Antiqua, taCenter) do
      Center;
    Label_Error := TKMLabel.Create(Panel_Error, 8, Panel_Main.Height div 2+10, Panel_Main.Width-16, 200, '...', fnt_Grey, taCenter);
    Label_Error.Center;
    Label_Error.AutoWrap := True;
    Button_ErrorBack := TKMButton.Create(Panel_Error,100,630,224,30,gResTexts[TX_MENU_BACK],bsMenu);
    Button_ErrorBack.Center;
    Button_ErrorBack.OnClick := SwitchMenuPage;
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
    gpMultiplayer:  fGuiMultiplayer.Show(aText);
    gpLobby:        begin
                      if aText = 'HOST' then
                        fGuiLobby.Show(lpk_Host, fGameApp.Networking, Panel_Main.Height)
                      else
                      if aText = 'JOIN' then
                        fGuiLobby.Show(lpk_Joiner, fGameApp.Networking, Panel_Main.Height)
                      else
                        Assert(False);
                    end;
    gpCampaign:     fGuiCampaign.Show(aText);
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
    gpCampSelect:   fGuiCampaigns.Show;
  end;
end;


procedure TKMMainMenuInterface.SwitchMenuPage(Sender: TObject);
var
  I: Integer;
begin
  Label_Version.Caption := GAME_VERSION + ' / ' + fGameApp.RenderVersion;

  //First thing - hide all existing pages
  for I := 1 to Panel_Main.ChildCount do
    if Panel_Main.Childs[i] is TKMPanel then
      Panel_Main.Childs[i].Hide;

  {Return to MainMenu}
  if (Sender = nil)
  or (Sender = Button_SP_Back)
  or (Sender = Button_CreditsBack)
  or (Sender = Button_ErrorBack)
  or (Sender = Button_ReplaysBack) then
  begin
    //Scan should be terminated, it is no longer needed
    if Sender = Button_ReplaysBack then
      fSaves.TerminateScan;
    Panel_MainMenu.Show;
  end;

  {Show SinglePlayer menu}
  {Return to SinglePlayerMenu}
  if (Sender = Button_MM_SinglePlayer) then
  begin
    Panel_SinglePlayer.Show;
  end;

  {Show campaign selection menu}
  if (Sender = Button_SP_Camp) then
    fGuiCampaigns.Show;

  {Show SingleMap menu}
  if Sender = Button_SP_Single then
    fGuiSingleMap.Show;

  {Show Load menu}
  if Sender=Button_SP_Load then
    fGuiLoad.Show;

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
    fGuiMultiplayer.Show('');

  {Show MapEditor menu}
  if Sender=Button_MM_MapEd then
    fGuiMapEditor.Show;

  {Show Options menu}
  if Sender = Button_MM_Options then
    fGuiOptions.Show;

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
end;


procedure TKMMainMenuInterface.MainMenu_MultiplayerClick(Sender: TObject);
begin
  if fMain.LockMutex then
  begin
    if not fGameApp.CheckDATConsistency then
      ShowScreen(msError, gResTexts[TX_ERROR_MODS])
    else
      SwitchMenuPage(Sender);
  end
  else
    ShowScreen(msError, gResTexts[TX_MULTIPLE_INSTANCES]);
end;


procedure TKMMainMenuInterface.MainMenu_PlayTutorial(Sender: TObject);
begin
  fGameApp.NewSingleMap(ExeDir + 'Tutorials'+PathDelim+'Town Tutorial'+PathDelim+'Town Tutorial.dat', gResTexts[TX_MENU_TUTORIAL_TOWN]);
end;


procedure TKMMainMenuInterface.MainMenu_PlayBattle(Sender: TObject);
begin
  fGameApp.NewSingleMap(ExeDir + 'Tutorials'+PathDelim+'Battle Tutorial'+PathDelim+'Battle Tutorial.dat', gResTexts[TX_MENU_TUTORIAL_BATTLE]);
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


procedure TKMMainMenuInterface.Replays_RefreshList(aJumpToSelected: Boolean);
var
  I, PrevTop: Integer;
begin
  PrevTop := ColumnBox_Replays.TopIndex;
  ColumnBox_Replays.Clear;

  fSaves.Lock;
  try
    for I := 0 to fSaves.Count - 1 do
      ColumnBox_Replays.AddItem(MakeListRow(
                           [fSaves[I].FileName, fSaves[I].Info.GetTitleWithTime],
                           [$FFFFFFFF, $FFFFFFFF]));

    for I := 0 to fSaves.Count - 1 do
      if (fSaves[I].CRC = fLastSaveCRC) then
        ColumnBox_Replays.ItemIndex := I;

  finally
    fSaves.Unlock;
  end;

  ColumnBox_Replays.TopIndex := PrevTop;

  if aJumpToSelected and (ColumnBox_Replays.ItemIndex <> -1)
  and not InRange(ColumnBox_Replays.ItemIndex - ColumnBox_Replays.TopIndex, 0, ColumnBox_Replays.GetVisibleRows-1)
  then
    if ColumnBox_Replays.ItemIndex < ColumnBox_Replays.TopIndex then
      ColumnBox_Replays.TopIndex := ColumnBox_Replays.ItemIndex
    else
    if ColumnBox_Replays.ItemIndex > ColumnBox_Replays.TopIndex + ColumnBox_Replays.GetVisibleRows - 1 then
      ColumnBox_Replays.TopIndex := ColumnBox_Replays.ItemIndex - ColumnBox_Replays.GetVisibleRows + 1;
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
  fGuiLobby.UpdateState(aTickCount);
  fGuiMapEditor.UpdateState;
  fGuiLoad.UpdateState;
  fGuiSingleMap.UpdateState;

  if fMaps <> nil then fMaps.UpdateState;
  if fMapsMP <> nil then fMapsMP.UpdateState;
  if fSaves <> nil then fSaves.UpdateState;
end;


end.
