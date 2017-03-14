unit KM_GUIMenuMain;
{$I KaM_Remake.inc}
interface
uses
  Controls, Math, SysUtils,
  KM_Controls, KM_Pics, KM_InterfaceDefaults;


type
  TKMMenuMain = class (TKMMenuPageCommon)
  private
    fOnPageChange: TGUIEventText;
    procedure ButtonClick(Sender: TObject);
  protected
    Panel_MainMenu: TKMPanel;
    Panel_MMButtons: TKMPanel;
    Button_MM_SinglePlayer: TKMButton;
    Button_MM_MultiPlayer: TKMButton;
    Button_MM_MapEd: TKMButton;
    Button_MM_Replays: TKMButton;
    Button_MM_Options: TKMButton;
    Button_MM_Credits: TKMButton;
    Button_MM_Quit: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    procedure Show;
  end;


implementation
uses
  KM_Main, KM_ResTexts, KM_GameApp, KM_RenderUI, KM_ResFonts;


{ TKMGUIMenuMain }
constructor TKMMenuMain.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  //Without anchors this page is centered on resize
  Panel_MainMenu := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_MainMenu.AnchorsCenter;
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
      Button_MM_SinglePlayer.OnClick := ButtonClick;
      Button_MM_MultiPlayer.OnClick  := ButtonClick;
      Button_MM_MapEd.OnClick        := ButtonClick;
      Button_MM_Replays.OnClick      := ButtonClick;
      Button_MM_Options.OnClick      := ButtonClick;
      Button_MM_Credits.OnClick      := ButtonClick;
      Button_MM_Quit.OnClick         := ButtonClick;
end;


procedure TKMMenuMain.ButtonClick(Sender: TObject);
begin
  if Sender = Button_MM_SinglePlayer then
    fOnPageChange(gpSinglePlayer);

  if Sender = Button_MM_MultiPlayer then
  begin
    if gMain.LockMutex then
    begin
      if not gGameApp.CheckDATConsistency then
        fOnPageChange(gpError, gResTexts[TX_ERROR_MODS])
      else
        fOnPageChange(gpMultiplayer);
    end
    else
      fOnPageChange(gpError, gResTexts[TX_MULTIPLE_INSTANCES]);
  end;

  if Sender = Button_MM_MapEd then
    fOnPageChange(gpMapEditor);

  if Sender = Button_MM_Replays then
    fOnPageChange(gpReplays);

  if Sender = Button_MM_Options then
    fOnPageChange(gpOptions);

  if Sender = Button_MM_Credits then
    fOnPageChange(gpCredits);

  if Sender = Button_MM_Quit then
    gMain.Stop(Self);
end;


procedure TKMMenuMain.Show;
begin
  Panel_MainMenu.Show;
end;


end.
