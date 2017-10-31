unit KM_GUIMapEdMenu;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, SysUtils,
   KM_Controls, KM_InterfaceGame,
   KM_GUIMapEdMenuResize,
   KM_GUIMapEdMenuQuickPlay,
   KM_GUIMapEdMenuLoad,
   KM_GUIMapEdMenuSave,
   KM_GUIMapEdMenuQuit,
   KM_GUIMapEdMenuSettings;

type
  TKMMapEdMenu = class
  private
    fGuiMenuResize: TKMMapEdMenuResize;
    fGuiMenuQuickPlay: TKMMapEdMenuQuickPlay;
    fGuiMenuLoad: TKMMapEdMenuLoad;
    fGuiMenuSave: TKMMapEdMenuSave;
    fGuiMenuSettings: TKMMapEdMenuSettings;
    fGuiMenuQuit: TKMMapEdMenuQuit;
    procedure MenuClick(Sender: TObject);
    procedure MenuDone(Sender: TObject);
    procedure MapTypeChange(aIsMultiplayer: Boolean);
  protected
    Panel_Menu: TKMPanel;
    Button_Resize: TKMButton;
    Button_Menu_Save: TKMButton;
    Button_Menu_Load: TKMButton;
    Button_QuickPlay: TKMButton;
    Button_Menu_Settings: TKMButton;
    Button_Menu_Quit: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
    destructor Destroy; override;

    property GuiMenuResize: TKMMapEdMenuResize read fGuiMenuResize;
    property GuiMenuQuickPlay: TKMMapEdMenuQuickPlay read fGuiMenuQuickPlay write fGuiMenuQuickPlay;
    procedure SetLoadMode(aMultiplayer:boolean);
    procedure Show;
    procedure Hide;
    function Visible: Boolean;
    procedure UpdateState;
  end;


implementation
uses
  KM_ResTexts, KM_RenderUI;


{ TKMapEdInterface }
constructor TKMMapEdMenu.Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
begin
  inherited Create;

  fGuiMenuResize := TKMMapEdMenuResize.Create(aParent, MenuDone, aOnPageChange);
  fGuiMenuLoad := TKMMapEdMenuLoad.Create(aParent, MenuDone);
  fGuiMenuSave := TKMMapEdMenuSave.Create(aParent, MenuDone);
  fGuiMenuSave.OnChangeMapType := MapTypeChange;
  fGuiMenuQuit := TKMMapEdMenuQuit.Create(aParent, MenuDone);
  fGuiMenuSettings := TKMMapEdMenuSettings.Create(aParent);

  Panel_Menu := TKMPanel.Create(aParent, 0, 45, TB_WIDTH, 400);

  Button_Resize := TKMButton.Create(Panel_Menu, 0, 20, TB_WIDTH, 30, 'Resize Map', bsGame); //Todo translate
  Button_Resize.Hint := 'Resize map'; //Todo translate
  Button_Resize.OnClick := MenuClick;

  Button_QuickPlay := TKMButton.Create(Panel_Menu, 0, 60, TB_WIDTH, 30, 'Quick Play', bsGame); //Todo translate
  Button_QuickPlay.Hint := 'Start quick game with current map'; //Todo translate
  Button_QuickPlay.OnClick := MenuClick;

  Button_Menu_Load := TKMButton.Create(Panel_Menu, 0, 130, TB_WIDTH, 30, gResTexts[TX_MAPED_LOAD_TITLE], bsGame);
  Button_Menu_Load.OnClick := MenuClick;
  Button_Menu_Load.Hint := gResTexts[TX_MAPED_LOAD_TITLE];
  Button_Menu_Save := TKMButton.Create(Panel_Menu, 0, 170, TB_WIDTH, 30, gResTexts[TX_MAPED_SAVE_TITLE], bsGame);
  Button_Menu_Save.OnClick := MenuClick;
  Button_Menu_Save.Hint := gResTexts[TX_MAPED_SAVE_TITLE];
  Button_Menu_Settings := TKMButton.Create(Panel_Menu, 0, 210, TB_WIDTH, 30, gResTexts[TX_MENU_SETTINGS], bsGame);
  Button_Menu_Settings.Hint := gResTexts[TX_MENU_SETTINGS];
  Button_Menu_Settings.OnClick := MenuClick;

  Button_Menu_Quit := TKMButton.Create(Panel_Menu, 0, 280, TB_WIDTH, 30, gResTexts[TX_MENU_QUIT_MAPED], bsGame);
  Button_Menu_Quit.Hint := gResTexts[TX_MENU_QUIT_MAPED];
  Button_Menu_Quit.OnClick := MenuClick;
end;


destructor TKMMapEdMenu.Destroy;
begin
  fGuiMenuResize.Free;
  fGuiMenuLoad.Free;
  fGuiMenuSave.Free;
  fGuiMenuQuit.Free;
  fGuiMenuSettings.Free;

  inherited;
end;


procedure TKMMapEdMenu.MapTypeChange(aIsMultiplayer: Boolean);
begin
  fGuiMenuQuickPlay.MapTypeChanged(aIsMultiplayer);
end;


procedure TKMMapEdMenu.MenuClick(Sender: TObject);
begin
  Hide;

  if Sender = Button_Resize then
    fGuiMenuResize.Show
  else
  if Sender = Button_QuickPlay then
    fGuiMenuQuickPlay.Show
  else
  if Sender = Button_Menu_Quit then
    fGuiMenuQuit.Show
  else
  if Sender = Button_Menu_Save then
    fGuiMenuSave.Show
  else
  if Sender = Button_Menu_Load then
    fGuiMenuLoad.Show;
  if Sender = Button_Menu_Settings then
  begin
    fGuiMenuSettings.Menu_Settings_Fill;
    fGuiMenuSettings.Show;
  end;
end;


procedure TKMMapEdMenu.MenuDone(Sender: TObject);
begin
  fGuiMenuResize.Hide;
  fGuiMenuQuickPlay.Hide;
  fGuiMenuLoad.Hide;
  fGuiMenuSave.Hide;
  fGuiMenuQuit.Hide;
  fGuiMenuSettings.Hide;

  Show;
end;


procedure TKMMapEdMenu.Hide;
begin
  Panel_Menu.Hide;
end;


procedure TKMMapEdMenu.Show;
begin
  Panel_Menu.Show;
end;


function TKMMapEdMenu.Visible: Boolean;
begin
  Result := Panel_Menu.Visible;
end;


procedure TKMMapEdMenu.UpdateState;
begin
  fGuiMenuLoad.UpdateState;
end;


procedure TKMMapEdMenu.SetLoadMode(aMultiplayer:boolean);
begin
  fGuiMenuResize.SetLoadMode(aMultiplayer);
  fGuiMenuQuickPlay.SetLoadMode(aMultiplayer);
  fGuiMenuLoad.SetLoadMode(aMultiplayer);
  fGuiMenuSave.SetLoadMode(aMultiplayer);
end;


end.
