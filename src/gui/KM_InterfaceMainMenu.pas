unit KM_InterfaceMainMenu;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls, Math, SysUtils, KromUtils,
  KM_Controls, KM_Defaults, KM_Pics, KM_Networking,
  KM_InterfaceDefaults,
  KM_GUIMenuCampaign,
  KM_GUIMenuCampaigns,
  KM_GUIMenuCredits,
  KM_GUIMenuError,
  KM_GUIMenuLoad,
  KM_GUIMenuLoading,
  KM_GUIMenuLobby,
  KM_GUIMenuMain,
  KM_GUIMenuMapEditor,
  KM_GUIMenuMultiplayer,
  KM_GUIMenuOptions,
  KM_GUIMenuReplays,
  KM_GUIMenuResultsMP,
  KM_GUIMenuResultsSP,
  KM_GUIMenuSingleMap,
  KM_GUIMenuSinglePlayer;


type
  TKMMainMenuInterface = class(TKMUserInterfaceCommon)
  private
    fMenuCampaign: TKMMenuCampaign;
    fMenuCampaigns: TKMMenuCampaigns;
    fMenuCredits: TKMMenuCredits;
    fMenuError: TKMMenuError;
    fMenuLoad: TKMMenuLoad;
    fMenuLoading: TKMMenuLoading;
    fMenuLobby: TKMMenuLobby;
    fMenuMain: TKMMenuMain;
    fMenuMapEditor: TKMMenuMapEditor;
    fMenuMultiplayer: TKMMenuMultiplayer;
    fMenuOptions: TKMMenuOptions;
    fMenuReplays: TKMMenuReplays;
    fMenuResultsMP: TKMMenuResultsMP;
    fMenuResultsSP: TKMMenuResultsSP;
    fMenuSingleMap: TKMMenuSingleMap;
    fMenuSinglePlayer: TKMMenuSinglePlayer;
  protected
    Panel_Menu: TKMPanel;
    Label_Version: TKMLabel;
  public
    constructor Create(X,Y: Word);
    destructor Destroy; override;
    procedure PageChange(Dest: TKMMenuPage; aText: UnicodeString = '');
    procedure AppendLoadingText(const aText: string);
    procedure ShowResultsMP(aMsg: TGameResultMsg);
    procedure ShowResultsSP(aMsg: TGameResultMsg);
    function GetChatState: TChatState;
    procedure SetChatState(const aChatState: TChatState);
    procedure ExportPages(aPath: string); override;
    procedure ReturnToLobby(const aSaveName: UnicodeString);

    procedure KeyDown(Key:Word; Shift: TShiftState); override;
    procedure KeyUp(Key:Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;

    procedure Resize(X,Y: Word); override;
    procedure UpdateState(aTickCount: Cardinal); override;
  end;


implementation
uses
  KM_ResTexts, KM_Campaigns, KM_GameApp, KM_Game, KM_Log, KM_RenderUI, KM_ResFonts;


{ TKMMainMenuInterface }
constructor TKMMainMenuInterface.Create(X,Y: Word);
var
  S: TKMShape;
  //F: TKMForm;
begin
  inherited;
  Assert(gResTexts <> nil, 'fTextMain should be initialized before MainMenuInterface');

  //Fixed-size and centered Panel for menu
  Panel_Menu := TKMPanel.Create(Panel_Main, (X - MENU_DESIGN_X) div 2, (Y - MENU_DESIGN_Y) div 2, MENU_DESIGN_X, MENU_DESIGN_Y);
  Panel_Menu.AnchorsCenter;

  //Background is the same for all pages, except Results/Campaign, which will render ontop
  with TKMImage.Create(Panel_Menu,-448,-216, 960, 600, 17, rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Menu, 512,-216, 960, 600, 18, rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Menu,-448, 384, 960, 600, 19, rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Menu, 512, 384, 960, 600, 20, rxGuiMain) do Anchors := [];

  fMenuMain          := TKMMenuMain.Create(Panel_Menu, PageChange);
  fMenuSinglePlayer  := TKMMenuSinglePlayer.Create(Panel_Menu, PageChange);
  fMenuCampaigns     := TKMMenuCampaigns.Create(Panel_Menu, PageChange);
  fMenuCampaign      := TKMMenuCampaign.Create(Panel_Menu, PageChange);
  fMenuSingleMap     := TKMMenuSingleMap.Create(Panel_Menu, PageChange);
  fMenuLoad          := TKMMenuLoad.Create(Panel_Menu, PageChange);
  fMenuMultiplayer   := TKMMenuMultiplayer.Create(Panel_Menu, PageChange);
  fMenuLobby         := TKMMenuLobby.Create(Panel_Menu, PageChange);
  fMenuMapEditor     := TKMMenuMapEditor.Create(Panel_Menu, PageChange);
  fMenuReplays       := TKMMenuReplays.Create(Panel_Menu, PageChange);
  fMenuOptions       := TKMMenuOptions.Create(Panel_Menu, PageChange);
  fMenuCredits       := TKMMenuCredits.Create(Panel_Menu, PageChange);
  fMenuError         := TKMMenuError.Create(Panel_Menu, PageChange);
  fMenuLoading       := TKMMenuLoading.Create(Panel_Menu, PageChange);
  fMenuResultsMP     := TKMMenuResultsMP.Create(Panel_Menu, PageChange);
  fMenuResultsSP     := TKMMenuResultsSP.Create(Panel_Menu, PageChange);

    {for i:=1 to length(FontFiles) do L[i]:=TKMLabel.Create(Panel_Main1,550,280+i*20,160,30,'This is a test string for KaM Remake ('+FontFiles[i],TKMFont(i),taLeft);//}
    //MyControls.AddTextEdit(Panel_Main, 32, 32, 200, 20, fnt_Grey);

  //Show version info on every page
  Label_Version := TKMLabel.Create(Panel_Main, 8, 8, 0, 0, '', fnt_Antiqua, taLeft);

  if OVERLAY_RESOLUTIONS then
  begin
    S := TKMShape.Create(Panel_Menu, 0, 96, 1024, 576);
    S.LineColor := $FF00FFFF;
    S.LineWidth := 1;
    S.Hitable := False;
    S := TKMShape.Create(Panel_Menu, 0, 0, 1024, 768);
    S.LineColor := $FF00FF00;
    S.LineWidth := 1;
    S.Hitable := False;
  end;

  //F := TKMForm.Create(Panel_Main, 100, 100, 200, 160);
  //F.Caption := 'Some Form';
  //F.Show;

  gLog.AddTime('Main menu init done');
end;


destructor TKMMainMenuInterface.Destroy;
begin
  fMenuCampaign.Free;
  fMenuCampaigns.Free;
  fMenuCredits.Free;
  fMenuError.Free;
  fMenuLoad.Free;
  fMenuLoading.Free;
  fMenuLobby.Free;
  fMenuMain.Free;
  fMenuMapEditor.Free;
  fMenuMultiplayer.Free;
  fMenuOptions.Free;
  fMenuReplays.Free;
  fMenuResultsMP.Free;
  fMenuResultsSP.Free;
  fMenuSingleMap.Free;
  fMenuSinglePlayer.Free;

  inherited;
end;


//Keep Panel_Main centered
procedure TKMMainMenuInterface.Resize(X, Y: Word);
begin
  inherited;
  Panel_Menu.Height := Min(Panel_Main.Height, MENU_DESIGN_Y);
  Panel_Menu.Top := (Panel_Main.Height - Panel_Menu.Height) div 2;

  //Needs to resize the map and move flag positions accordingly
  fMenuCampaign.Resize(X, Y);
  fMenuMultiplayer.Resize(X, Y);

  //Needs to swap map description / game settings on low resolution displays
  fMenuLobby.Lobby_Resize(Panel_Menu.Height);
end;


procedure TKMMainMenuInterface.AppendLoadingText(const aText: string);
begin
  fMenuLoading.AppendText(aText);
end;


function TKMMainMenuInterface.GetChatState: TChatState;
begin
  Result := fMenuLobby.GetChatState;
end;


procedure TKMMainMenuInterface.SetChatState(const aChatState: TChatState);
begin
  fMenuLobby.SetChatState(aChatState);
end;


procedure TKMMainMenuInterface.ShowResultsMP(aMsg: TGameResultMsg);
begin
  fMenuResultsMP.Show(aMsg);
end;


procedure TKMMainMenuInterface.ShowResultsSP(aMsg: TGameResultMsg);
begin
  fMenuResultsSP.Show(aMsg);
end;


procedure TKMMainMenuInterface.PageChange(Dest: TKMMenuPage; aText: UnicodeString = '');
var
  I: Integer;
  cmp: TKMCampaignId;
begin
  Label_Version.Caption := 'KaM Remake - ' + GAME_VERSION + ' / ' + gGameApp.RenderVersion;

  //Hide all other pages
  for I := 0 to Panel_Menu.ChildCount - 1 do
    if Panel_Menu.Childs[I] is TKMPanel then
      Panel_Menu.Childs[I].Hide;

  case Dest of
    gpMainMenu:     fMenuMain.Show;
    gpSingleplayer: fMenuSinglePlayer.Show;
    gpLoad:         fMenuLoad.Show;
    gpSingleMap:    fMenuSingleMap.Show;
    gpMultiplayer:  fMenuMultiplayer.Show(aText);
    gpLobby:        begin
                      if aText = 'HOST' then
                        fMenuLobby.Show(lpk_Host, gGameApp.Networking, Panel_Menu.Height)
                      else
                      if aText = 'JOIN' then
                        fMenuLobby.Show(lpk_Joiner, gGameApp.Networking, Panel_Menu.Height)
                      else
                        Assert(False);
                    end;
    gpCampaign:     begin
                      cmp[0] := Ord(aText[1]);
                      cmp[1] := Ord(aText[2]);
                      cmp[2] := Ord(aText[3]);
                      fMenuCampaign.Show(cmp);
                    end;
    gpCampSelect:   fMenuCampaigns.Show;
    gpCredits:      fMenuCredits.Show;
    gpOptions:      fMenuOptions.Show;
    gpMapEditor:    fMenuMapEditor.Show;
    gpReplays:      fMenuReplays.Show;
    gpError:        fMenuError.Show(aText);
    gpLoading:      fMenuLoading.Show(aText);
  end;
end;


procedure TKMMainMenuInterface.ExportPages(aPath: string);
var
  path: string;
  I, K: Integer;
begin
  inherited;

  path := aPath + 'Menu' + PathDelim;
  ForceDirectories(path);

  for I := 0 to Panel_Menu.ChildCount - 1 do
    if (Panel_Menu.Childs[I] is TKMPanel)
    and (Panel_Menu.Childs[I].Width > 100) then
    begin
      //Hide all other panels
      for K := 0 to Panel_Menu.ChildCount - 1 do
        if Panel_Menu.Childs[K] is TKMPanel then
          Panel_Menu.Childs[K].Hide;

      Panel_Menu.Childs[I].Show;

      gGameApp.PrintScreen(path + 'Panel' + int2fix(I, 3) + '.jpg');
    end;
end;


procedure TKMMainMenuInterface.ReturnToLobby(const aSaveName: UnicodeString);
begin
  if gGameApp.Networking.IsHost then
    PageChange(gpLobby, 'HOST')
  else
    PageChange(gpLobby, 'JOIN');

  fMenuLobby.ReturnToLobby(aSaveName);
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

  fMenuCampaign.MouseMove(Shift, X, Y);
end;


procedure TKMMainMenuInterface.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  fMyControls.MouseUp(X,Y,Shift,Button);
  Exit; //We could have caused gGameApp reinit (i.e. resolution change), so exit at once
end;


//Should update anything we want to be updated, obviously
procedure TKMMainMenuInterface.UpdateState(aTickCount: Cardinal);
begin
  fMenuLobby.UpdateState;
  fMenuMapEditor.UpdateState;
  fMenuLoad.UpdateState;
  fMenuReplays.UpdateState;
  fMenuSingleMap.UpdateState;
end;


end.
