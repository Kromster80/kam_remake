unit KM_GUIMapEdPlayer;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, Math, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics,
   KM_GUIMapEdPlayerBlockHouse,
   KM_GUIMapEdPlayerBlockTrade,
   KM_GUIMapEdPlayerColors,
   KM_GUIMapEdPlayerGoals,
   KM_GUIMapEdPlayerView;

type
  TKMPlayerTab = (ptGoals, ptColor, ptBlockHouse, ptBlockTrade, ptView);

  TKMMapEdPlayer = class
  private
    fOnPageChange: TNotifyEvent;

    fGuiPlayerBlockHouse: TKMMapEdPlayerBlockHouse;
    fGuiPlayerBlockTrade: TKMMapEdPlayerBlockTrade;
    fGuiPlayerColors: TKMMapEdPlayerColors;
    fGuiPlayerView: TKMMapEdPlayerView;

    procedure PageChange(Sender: TObject);
  protected
    Panel_Player: TKMPanel;
    Button_Player: array [TKMPlayerTab] of TKMButton;
  public
    fGuiPlayerGoals: TKMMapEdPlayerGoals;
    constructor Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
    destructor Destroy; override;

    procedure Show(aPage: TKMPlayerTab);
    function Visible(aPage: TKMPlayerTab): Boolean; overload;
    function Visible: Boolean; overload;
    procedure UpdatePlayer(aPlayerIndex: TPlayerIndex);
  end;


implementation
uses
  KM_Player, KM_PlayersCollection, KM_ResTexts, KM_GameCursor,
  KM_RenderUI, KM_InterfaceDefaults;


{ TKMMapEdPlayer }
constructor TKMMapEdPlayer.Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
const
  TabGlyph: array [TKMPlayerTab] of Word    = (8,         1159,     38,    327,   393);
  TabRXX  : array [TKMPlayerTab] of TRXType = (rxGuiMain, rxHouses, rxGui, rxGui, rxGui);
  TabHint : array [TKMPlayerTab] of Word = (
    TX_MAPED_GOALS,
    TX_MAPED_PLAYER_COLORS,
    TX_MAPED_BLOCK_HOUSES,
    TX_MAPED_BLOCK_TRADE,
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

  fGuiPlayerGoals := TKMMapEdPlayerGoals.Create(Panel_Player);
  fGuiPlayerColors := TKMMapEdPlayerColors.Create(Panel_Player);
  fGuiPlayerBlockHouse := TKMMapEdPlayerBlockHouse.Create(Panel_Player);
  fGuiPlayerBlockTrade := TKMMapEdPlayerBlockTrade.Create(Panel_Player);
  fGuiPlayerView := TKMMapEdPlayerView.Create(Panel_Player);
end;


destructor TKMMapEdPlayer.Destroy;
begin
  fGuiPlayerGoals.Free;
  fGuiPlayerColors.Free;
  fGuiPlayerBlockHouse.Free;
  fGuiPlayerBlockTrade.Free;
  fGuiPlayerView.Free;

  inherited;
end;


procedure TKMMapEdPlayer.PageChange(Sender: TObject);
begin
  //Reset cursor mode
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  //Hide existing pages
  fGuiPlayerGoals.Hide;
  fGuiPlayerColors.Hide;
  fGuiPlayerBlockHouse.Hide;
  fGuiPlayerBlockTrade.Hide;
  fGuiPlayerView.Hide;

  if (Sender = Button_Player[ptGoals]) then
    fGuiPlayerGoals.Show
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
  if (Sender = Button_Player[ptView]) then
    fGuiPlayerView.Show;

  //Signal that active page has changed, that may affect layers visibility
  fOnPageChange(Self);
end;


procedure TKMMapEdPlayer.Show(aPage: TKMPlayerTab);
begin
  case aPage of
    ptGoals:      fGuiPlayerGoals.Show;
    ptColor:      fGuiPlayerColors.Show;
    ptBlockHouse: fGuiPlayerBlockHouse.Show;
    ptBlockTrade: fGuiPlayerBlockTrade.Show;
    ptView:       fGuiPlayerView.Show;
  end;
end;


function TKMMapEdPlayer.Visible: Boolean;
begin
  Result := Panel_Player.Visible;
end;


function TKMMapEdPlayer.Visible(aPage: TKMPlayerTab): Boolean;
begin
  case aPage of
    ptGoals:      Result := fGuiPlayerGoals.Visible;
    ptColor:      Result := fGuiPlayerColors.Visible;
    ptBlockHouse: Result := fGuiPlayerBlockHouse.Visible;
    ptBlockTrade: Result := fGuiPlayerBlockTrade.Visible;
    ptView:       Result := fGuiPlayerView.Visible;
    else          Result := False;
  end;
end;


procedure TKMMapEdPlayer.UpdatePlayer(aPlayerIndex: TPlayerIndex);
begin
  Button_Player[ptColor].FlagColor := gPlayers[aPlayerIndex].FlagColor;

  fGuiPlayerView.UpdatePlayer(aPlayerIndex);
end;


end.
