unit KM_InterfaceMainMenu;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls, Math,
  KM_Controls, KM_Defaults, KM_Pics,
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
  TKMMainMenuInterface = class(TKMUserInterface)
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
    Panel_Main: TKMPanel;
    Label_Version: TKMLabel;
  public
    constructor Create(X,Y: Word);
    destructor Destroy; override;
    procedure PageChange(Dest: TKMMenuPage; aText: string = '');
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
uses KM_ResTexts, KM_GameApp, KM_Log, KM_Networking, KM_RenderUI, KM_ResFonts;


{ TKMMainMenuInterface }
constructor TKMMainMenuInterface.Create(X,Y: Word);
var S: TKMShape;
begin
  inherited;
  Assert(gResTexts <> nil, 'fTextMain should be initialized before MainMenuInterface');

  //Parent Panel for whole menu
  Panel_Main := TKMPanel.Create(fMyControls, 0, 0, MENU_DESIGN_X, MENU_DESIGN_Y);

  //Background is the same for all pages, except Results/Campaign, which will render ontop
  with TKMImage.Create(Panel_Main,-448,-216, 960, 600, 17, rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Main, 512,-216, 960, 600, 18, rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Main,-448, 384, 960, 600, 19, rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Main, 512, 384, 960, 600, 20, rxGuiMain) do Anchors := [];

  fMenuMain          := TKMMenuMain.Create(Panel_Main, PageChange);
  fMenuSinglePlayer  := TKMMenuSinglePlayer.Create(Panel_Main, PageChange);
  fMenuCampaigns     := TKMMenuCampaigns.Create(Panel_Main, PageChange);
  fMenuCampaign      := TKMMenuCampaign.Create(Panel_Main, PageChange);
  fMenuSingleMap     := TKMMenuSingleMap.Create(Panel_Main, PageChange);
  fMenuLoad          := TKMMenuLoad.Create(Panel_Main, PageChange);
  fMenuMultiplayer   := TKMMenuMultiplayer.Create(Panel_Main, PageChange);
  fMenuLobby         := TKMMenuLobby.Create(Panel_Main, PageChange);
  fMenuMapEditor     := TKMMenuMapEditor.Create(Panel_Main, PageChange);
  fMenuReplays       := TKMMenuReplays.Create(Panel_Main, PageChange);
  fMenuOptions       := TKMMenuOptions.Create(Panel_Main, PageChange);
  fMenuCredits       := TKMMenuCredits.Create(Panel_Main, PageChange);
  fMenuError         := TKMMenuError.Create(Panel_Main, PageChange);
  fMenuLoading       := TKMMenuLoading.Create(Panel_Main, PageChange);
  fMenuResultsMP     := TKMMenuResultsMP.Create(Panel_Main, PageChange);
  fMenuResultsSP     := TKMMenuResultsSP.Create(Panel_Main, PageChange);

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
  Panel_Main.Width  := Min(X, MENU_DESIGN_X);
  Panel_Main.Height := Min(Y, MENU_DESIGN_Y);

  Panel_Main.Left := (X - Panel_Main.Width) div 2;
  Panel_Main.Top  := (Y - Panel_Main.Height) div 2;

  //Needs to resize the map and move flag positions accordingly
  fMenuCampaign.Resize(X, Y);

  //Needs to swap map description / game settings on low resolution displays
  fMenuLobby.Lobby_Resize(Panel_Main.Height);
end;


procedure TKMMainMenuInterface.AppendLoadingText(const aText: string);
begin
  fMenuLoading.AppendText(aText);
end;


function TKMMainMenuInterface.GetChatText: string;
begin
  Result := fMenuLobby.GetChatText;
end;


//Access chat messages history to copy it over to gameplay chat
function TKMMainMenuInterface.GetChatMessages: string;
begin
  Result := fMenuLobby.GetChatMessages;
end;


procedure TKMMainMenuInterface.ShowResultsMP(aMsg: TGameResultMsg);
begin
  fMenuResultsMP.Show(aMsg);
end;


procedure TKMMainMenuInterface.ShowResultsSP(aMsg: TGameResultMsg);
begin
  fMenuResultsSP.Show(aMsg);
end;


procedure TKMMainMenuInterface.PageChange(Dest: TKMMenuPage; aText: string = '');
var
  I: Integer;
begin
  Label_Version.Caption := GAME_VERSION + ' / ' + fGameApp.RenderVersion;

  //Hide all other pages
  for I := 1 to Panel_Main.ChildCount do
    if Panel_Main.Childs[I] is TKMPanel then
      Panel_Main.Childs[I].Hide;

  case Dest of
    gpMainMenu:     fMenuMain.Show;
    gpSingleplayer: fMenuSinglePlayer.Show;
    gpLoad:         fMenuLoad.Show;
    gpSingleMap:    fMenuSingleMap.Show;
    gpMultiplayer:  fMenuMultiplayer.Show(aText);
    gpLobby:        begin
                      if aText = 'HOST' then
                        fMenuLobby.Show(lpk_Host, fGameApp.Networking, Panel_Main.Height)
                      else
                      if aText = 'JOIN' then
                        fMenuLobby.Show(lpk_Joiner, fGameApp.Networking, Panel_Main.Height)
                      else
                        Assert(False);
                    end;
    gpCampaign:     fMenuCampaign.Show(aText);
    gpCampSelect:   fMenuCampaigns.Show;
    gpCredits:      fMenuCredits.Show;
    gpOptions:      fMenuOptions.Show;
    gpMapEditor:    fMenuMapEditor.Show;
    gpReplays:      fMenuReplays.Show;
    gpError:        fMenuError.Show(aText);
    gpLoading:      fMenuLoading.Show(aText);
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

  fMenuCampaign.MouseMove(Shift, X, Y);
end;


procedure TKMMainMenuInterface.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  fMyControls.MouseUp(X,Y,Shift,Button);
  Exit; //We could have caused fGameApp reinit (i.e. resolution change), so exit at once
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
