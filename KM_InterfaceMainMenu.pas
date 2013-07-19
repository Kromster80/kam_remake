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
  TKMMainMenuInterface = class (TKMUserInterface)
  private
    fGuiCampaign: TKMGUIMainCampaign;
    fGuiCampaigns: TKMGUIMainCampaigns;
    fGuiCredits: TKMGUIMainCredits;
    fGuiError: TKMGUIMenuError;
    fGuiLoad: TKMGUIMenuLoad;
    fGuiLoading: TKMGUIMenuLoading;
    fGuiLobby: TKMGUIMenuLobby;
    fGuiMain: TKMGUIMenuMain;
    fGuiMapEditor: TKMGUIMainMapEditor;
    fGuiMultiplayer: TKMGUIMainMultiplayer;
    fGuiOptions: TKMGUIMainOptions;
    fGuiReplays: TKMGUIMenuReplays;
    fGuiResultsMP: TKMGUIMenuResultsMP;
    fGuiResultsSP: TKMGUIMenuResultsSP;
    fGuiSingleMap: TKMGUIMenuSingleMap;
    fGuiSinglePlayer: TKMGUIMenuSinglePlayer;
    //fPages: array [TGUIPage] of TKMGUIPage;
  protected
    Panel_Main: TKMPanel;
      Label_Version: TKMLabel;
  public
    constructor Create(X,Y: Word);
    destructor Destroy; override;
    procedure PageChange(Dest: TGUIPage; aText: string = '');
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

  Panel_Main := TKMPanel.Create(fMyControls, 0,
                                             0,
                                             MENU_DESIGN_X,
                                             MENU_DESIGN_Y); //Parent Panel for whole menu

  //Background is the same for all pages, except Results/Campaign, which will render ontop
  with TKMImage.Create(Panel_Main,-448,-216,960,600,17,rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Main, 512,-216,960,600,18,rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Main,-448, 384,960,600,19,rxGuiMain) do Anchors := [];
  with TKMImage.Create(Panel_Main, 512, 384,960,600,20,rxGuiMain) do Anchors := [];

  fGuiMain := TKMGUIMenuMain.Create(Panel_Main, PageChange);
    fGuiSinglePlayer := TKMGUIMenuSinglePlayer.Create(Panel_Main, PageChange);
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
    fGuiError := TKMGUIMenuError.Create(Panel_Main, PageChange);
    fGuiLoading := TKMGUIMenuLoading.Create(Panel_Main, PageChange);
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
  fGuiCampaign.Free;
  fGuiCampaigns.Free;
  fGuiCredits.Free;
  fGuiError.Free;
  fGuiLoad.Free;
  fGuiLoading.Free;
  fGuiLobby.Free;
  fGuiMain.Free;
  fGuiMapEditor.Free;
  fGuiMultiplayer.Free;
  fGuiOptions.Free;
  fGuiReplays.Free;
  fGuiResultsMP.Free;
  fGuiResultsSP.Free;
  fGuiSingleMap.Free;
  fGuiSinglePlayer.Free;

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


procedure TKMMainMenuInterface.AppendLoadingText(const aText: string);
begin
  fGuiLoading.AppendText(aText);
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


procedure TKMMainMenuInterface.PageChange(Dest: TGUIPage; aText: string = '');
var
  I: Integer;
begin
  Label_Version.Caption := GAME_VERSION + ' / ' + fGameApp.RenderVersion;

  //Hide all other pages
  for I := 1 to Panel_Main.ChildCount do
    if Panel_Main.Childs[I] is TKMPanel then
      Panel_Main.Childs[I].Hide;

  case Dest of
    gpMainMenu:     fGuiMain.Show;
    gpSingleplayer: fGuiSinglePlayer.Show;
    gpLoad:         fGuiLoad.Show;
    gpSingleMap:    fGuiSingleMap.Show;
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
    gpCampSelect:   fGuiCampaigns.Show;
    gpCredits:      fGuiCredits.Show;
    gpOptions:      fGuiOptions.Show;
    gpMapEditor:    fGuiMapEditor.Show;
    gpReplays:      fGuiReplays.Show;
    gpError:        fGuiError.Show(aText);
    gpLoading:      fGuiLoading.Show(aText);
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
  fGuiLobby.UpdateState;
  fGuiMapEditor.UpdateState;
  fGuiLoad.UpdateState;
  fGuiReplays.UpdateState;
  fGuiSingleMap.UpdateState;
end;


end.
