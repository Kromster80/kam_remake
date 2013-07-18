unit KM_InterfaceMainMenu;
{$I KaM_Remake.inc}
interface
uses
  StrUtils, SysUtils, KromUtils, Math, Classes, Controls,
  KM_Controls, KM_Defaults, KM_Pics,
  KM_InterfaceDefaults,
  KM_GUIMenuCampaign,
  KM_GUIMenuCampaigns,
  KM_GUIMenuCredits,
  KM_GUIMenuLoad,
  KM_GUIMenuLobby,
  KM_GUIMenuMapEditor,
  KM_GUIMenuMultiplayer,
  KM_GUIMenuOptions,
  KM_GUIMenuReplays,
  KM_GUIMenuResultsMP,
  KM_GUIMenuResultsSP,
  KM_GUIMenuSingleMap;


type
  TMenuScreen = (msError, msLoading, msMain, msOptions);

  TKMMainMenuInterface = class (TKMUserInterface)
  private
    fGuiCampaign: TKMGUIMainCampaign;
    fGuiCampaigns: TKMGUIMainCampaigns;
    fGuiCredits: TKMGUIMainCredits;
    fGuiLoad: TKMGUIMenuLoad;
    fGuiLobby: TKMGUIMenuLobby;
    fGuiMapEditor: TKMGUIMainMapEditor;
    fGuiMultiplayer: TKMGUIMainMultiplayer;
    fGuiOptions: TKMGUIMainOptions;
    fGuiReplays: TKMGUIMenuReplays;
    fGuiResultsMP: TKMGUIMenuResultsMP;
    fGuiResultsSP: TKMGUIMenuResultsSP;
    fGuiSingleMap: TKMGUIMenuSingleMap;
    //fPages: array [TGUIPage] of TKMGUIPage;

    procedure Create_MainMenu;
    procedure Create_SinglePlayer;
    procedure Create_Loading;
    procedure Create_Error;
    procedure PageChange(Sender: TObject; Dest: TGUIPage; aText: string);
    procedure SwitchMenuPage(Sender: TObject);
    procedure MainMenu_MultiplayerClick(Sender: TObject);
    procedure MainMenu_PlayTutorial(Sender: TObject);
    procedure MainMenu_PlayBattle(Sender: TObject);
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
uses KM_Main, KM_ResTexts, KM_GameApp, KM_ResLocales,
  KM_Utils, KM_Log, KM_Networking, KM_RenderUI, KM_ResFonts;


{ TKMMainMenuInterface }
constructor TKMMainMenuInterface.Create(X,Y: Word);
var S: TKMShape;
begin
  inherited;
  Assert(gResTexts <> nil, 'fTextMain should be initialized before MainMenuInterface');

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
  fGuiReplays := TKMGUIMenuReplays.Create(Panel_Main, PageChange);
  fGuiOptions := TKMGUIMainOptions.Create(Panel_Main, PageChange);
  fGuiCredits := TKMGUIMainCredits.Create(Panel_Main, PageChange);
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
  fGuiLobby.Free;
  fGuiLoad.Free;
  fGuiMultiplayer.Free;
  fGuiOptions.Free;
  fGuiSingleMap.Free;
  fGuiCampaign.Free;
  fGuiCampaigns.Free;
  fGuiCredits.Free;
  fGuiReplays.Free;
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
    gpReplays:      fGuiReplays.Show;
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
  or (Sender = Button_ErrorBack) then
    Panel_MainMenu.Show;

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
  if Sender=Button_MM_Replays then
    fGuiReplays.Show;

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
  if Sender=Button_MM_Credits then
    fGuiCredits.Show;

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
  fGuiLobby.UpdateState;
  fGuiMapEditor.UpdateState;
  fGuiLoad.UpdateState;
  fGuiReplays.UpdateState;
  fGuiSingleMap.UpdateState;
end;


end.
