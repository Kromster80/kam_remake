unit KM_GUIMapEdPlayer;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, Math, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics,
   KM_GUIMapEdPlayerBlockHouse,
   KM_GUIMapEdPlayerBlockTrade,
   KM_GUIMapEdPlayerBlockUnit,
   KM_GUIMapEdPlayerColors,
   KM_GUIMapEdPlayerGoals,
   KM_GUIMapEdPlayerView;

type
  TKMPlayerTab = (ptGoals, ptColor, ptBlockHouse, ptBlockTrade, ptBlockUnit, ptView);

  TKMMapEdPlayer = class
  private
    fOnPageChange: TNotifyEvent;

    fGuiPlayerBlockHouse: TKMMapEdPlayerBlockHouse;
    fGuiPlayerBlockTrade: TKMMapEdPlayerBlockTrade;
    fGuiPlayerBlockUnit: TKMMapEdPlayerBlockUnit;
    fGuiPlayerColors: TKMMapEdPlayerColors;
    fGuiPlayerView: TKMMapEdPlayerView;

    procedure PageChange(Sender: TObject);
  protected
    Panel_Player: TKMPanel;
    Button_Player: array [TKMPlayerTab] of TKMButton;
  public
    GuiPlayerGoals: TKMMapEdPlayerGoals;
    constructor Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
    destructor Destroy; override;

    procedure Show(aPage: TKMPlayerTab);
    procedure ShowIndex(aIndex: Byte);
    function Visible(aPage: TKMPlayerTab): Boolean; overload;
    function Visible: Boolean; overload;
    procedure ChangePlayer;
    procedure UpdatePlayerColor;
    procedure UpdateState;
  end;


implementation
uses
  KM_Hand, KM_HandsCollection, KM_ResTexts, KM_GameCursor,
  KM_RenderUI, KM_InterfaceGame;


{ TKMMapEdPlayer }
constructor TKMMapEdPlayer.Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
const
  TabGlyph: array [TKMPlayerTab] of Word    = (8,         1159,     38,    327,   141,   393);
  TabRXX  : array [TKMPlayerTab] of TRXType = (rxGuiMain, rxHouses, rxGui, rxGui, rxGui, rxGui);
  TabHint : array [TKMPlayerTab] of Word = (
    TX_MAPED_GOALS,
    TX_MAPED_PLAYER_COLORS,
    TX_MAPED_BLOCK_HOUSES,
    TX_MAPED_BLOCK_TRADE,
    TX_MAPED_BLOCK_UNITS,
    TX_MAPED_FOG);
var
  PT: TKMPlayerTab;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  Panel_Player := TKMPanel.Create(aParent, 0, 45, TB_WIDTH, 28);

  for PT := Low(TKMPlayerTab) to High(TKMPlayerTab) do
  begin
    Button_Player[PT] := TKMButton.Create(Panel_Player, SMALL_PAD_W * Byte(PT), 0, SMALL_TAB_W, SMALL_TAB_H,  TabGlyph[PT], TabRXX[PT], bsGame);
    Button_Player[PT].Hint := gResTexts[TabHint[PT]];
    Button_Player[PT].OnClick := PageChange;
  end;

  GuiPlayerGoals := TKMMapEdPlayerGoals.Create(Panel_Player);
  fGuiPlayerColors := TKMMapEdPlayerColors.Create(Panel_Player);
  fGuiPlayerBlockHouse := TKMMapEdPlayerBlockHouse.Create(Panel_Player);
  fGuiPlayerBlockTrade := TKMMapEdPlayerBlockTrade.Create(Panel_Player);
  fGuiPlayerBlockUnit := TKMMapEdPlayerBlockUnit.Create(Panel_Player);
  fGuiPlayerView := TKMMapEdPlayerView.Create(Panel_Player);
end;


destructor TKMMapEdPlayer.Destroy;
begin
  GuiPlayerGoals.Free;
  fGuiPlayerColors.Free;
  fGuiPlayerBlockHouse.Free;
  fGuiPlayerBlockTrade.Free;
  fGuiPlayerBlockUnit.Free;
  fGuiPlayerView.Free;

  inherited;
end;


procedure TKMMapEdPlayer.PageChange(Sender: TObject);
begin
  //Reset cursor mode
  gGameCursor.Mode := cmNone;

  //Hide existing pages
  GuiPlayerGoals.Hide;
  fGuiPlayerColors.Hide;
  fGuiPlayerBlockHouse.Hide;
  fGuiPlayerBlockTrade.Hide;
  fGuiPlayerBlockUnit.Hide;
  fGuiPlayerView.Hide;

  if (Sender = Button_Player[ptGoals]) then
    GuiPlayerGoals.Show
  else
  if (Sender = Button_Player[ptColor]) then
    fGuiPlayerColors.Show
  else
  if (Sender = Button_Player[ptBlockHouse]) then
    fGuiPlayerBlockHouse.Show
  else
  if (Sender = Button_Player[ptBlockTrade]) then
    fGuiPlayerBlockTrade.Show
  else
  if (Sender = Button_Player[ptBlockUnit]) then
    fGuiPlayerBlockUnit.Show
  else
  if (Sender = Button_Player[ptView]) then
    fGuiPlayerView.Show;

  //Signal that active page has changed, that may affect layers visibility
  fOnPageChange(Self);
end;


procedure TKMMapEdPlayer.Show(aPage: TKMPlayerTab);
begin
  case aPage of
    ptGoals:      GuiPlayerGoals.Show;
    ptColor:      fGuiPlayerColors.Show;
    ptBlockHouse: fGuiPlayerBlockHouse.Show;
    ptBlockTrade: fGuiPlayerBlockTrade.Show;
    ptBlockUnit:  fGuiPlayerBlockUnit.Show;
    ptView:       fGuiPlayerView.Show;
  end;

  //Signal that active page has changed, that may affect layers visibility
  fOnPageChange(Self);
end;


procedure TKMMapEdPlayer.ShowIndex(aIndex: Byte);
begin
  if aIndex in [Byte(Low(TKMPlayerTab))..Byte(High(TKMPlayerTab))] then
  begin
    PageChange(nil); //Hide existing pages
    Show(TKMPlayerTab(aIndex));
  end;
end;


function TKMMapEdPlayer.Visible: Boolean;
begin
  Result := Panel_Player.Visible;
end;


function TKMMapEdPlayer.Visible(aPage: TKMPlayerTab): Boolean;
begin
  case aPage of
    ptGoals:      Result := GuiPlayerGoals.Visible;
    ptColor:      Result := fGuiPlayerColors.Visible;
    ptBlockHouse: Result := fGuiPlayerBlockHouse.Visible;
    ptBlockTrade: Result := fGuiPlayerBlockTrade.Visible;
    ptBlockUnit:  Result := fGuiPlayerBlockUnit.Visible;
    ptView:       Result := fGuiPlayerView.Visible;
    else          Result := False;
  end;
end;


procedure TKMMapEdPlayer.ChangePlayer;
begin
  if GuiPlayerGoals.Visible then GuiPlayerGoals.Show;
  if fGuiPlayerColors.Visible then fGuiPlayerColors.Show;
  if fGuiPlayerBlockHouse.Visible then fGuiPlayerBlockHouse.Show;
  if fGuiPlayerBlockTrade.Visible then fGuiPlayerBlockTrade.Show;
  if fGuiPlayerBlockUnit.Visible then fGuiPlayerBlockUnit.Show;
  if fGuiPlayerView.Visible then fGuiPlayerView.Show;

  UpdatePlayerColor;
end;


procedure TKMMapEdPlayer.UpdatePlayerColor;
begin
  Button_Player[ptColor].FlagColor := gMySpectator.Hand.FlagColor;
  Button_Player[ptBlockUnit].FlagColor := gMySpectator.Hand.FlagColor;

  fGuiPlayerView.UpdatePlayerColor;
  fGuiPlayerBlockUnit.UpdatePlayerColor;
end;


procedure TKMMapEdPlayer.UpdateState;
begin
  fGuiPlayerView.UpdateState;
end;


end.
