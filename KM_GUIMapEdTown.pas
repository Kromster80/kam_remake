unit KM_GUIMapEdTown;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls, Math, SysUtils,
  KM_Controls, KM_Defaults, KM_Pics,
  KM_GUIMapEdTownHouses,
  KM_GUIMapEdTownUnits,
  KM_GUIMapEdTownScript,
  KM_GUIMapEdTownDefence,
  KM_GUIMapEdTownOffence;

type
  TKMTownTab = (ttHouses, ttUnits, ttScript, ttDefences, ttOffence);

  TKMMapEdTown = class
  private
    fOnPageChange: TNotifyEvent;

    fGuiHouses: TKMMapEdTownHouses;
    fGuiUnits: TKMMapEdTownUnits;
    fGuiScript: TKMMapEdTownScript;

    procedure PageChange(Sender: TObject);
  protected
    Panel_Town: TKMPanel;
    Button_Town: array [TKMTownTab] of TKMButton;
  public
    fGuiDefence: TKMMapEdTownDefence;
    fGuiOffence: TKMMapEdTownOffence;
    constructor Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
    destructor Destroy; override;

    procedure Show(aPage: TKMTownTab);
    function Visible(aPage: TKMTownTab): Boolean; overload;
    function Visible: Boolean; overload;
    procedure UpdatePlayer(aPlayerIndex: TPlayerIndex);
  end;


implementation
uses
  KM_Player, KM_PlayersCollection, KM_ResTexts, KM_GameCursor,
  KM_InterfaceDefaults, KM_RenderUI;


{ TKMMapEdTown }
constructor TKMMapEdTown.Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
const
  TabGlyph: array [TKMTownTab] of Word    = (391,   141,   62,        43,    53);
  TabRXX  : array [TKMTownTab] of TRXType = (rxGui, rxGui, rxGuiMain, rxGui, rxGui);
  TabHint : array [TKMTownTab] of Word = (
    TX_MAPED_VILLAGE,
    TX_MAPED_UNITS,
    TX_MAPED_AI_TITLE,
    TX_MAPED_AI_DEFENSE_OPTIONS,
    TX_MAPED_AI_ATTACK);
var
  I: TKMTownTab;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  Panel_Town := TKMPanel.Create(aParent, 0, 45, TB_WIDTH, 28);

  for I := Low(TKMTownTab) to High(TKMTownTab) do
  begin
    Button_Town[I] := TKMButton.Create(Panel_Town, SMALL_PAD_W * Byte(I), 0, SMALL_TAB_W, SMALL_TAB_H, TabGlyph[I], TabRXX[I], bsGame);
    Button_Town[I].Hint := gResTexts[TabHint[I]];
    Button_Town[I].OnClick := PageChange;
  end;

  fGuiHouses := TKMMapEdTownHouses.Create(Panel_Town);
  fGuiUnits := TKMMapEdTownUnits.Create(Panel_Town);
  fGuiScript := TKMMapEdTownScript.Create(Panel_Town);
  fGuiDefence := TKMMapEdTownDefence.Create(Panel_Town);
  fGuiOffence := TKMMapEdTownOffence.Create(Panel_Town);
end;


destructor TKMMapEdTown.Destroy;
begin
  fGuiHouses.Free;
  fGuiUnits.Free;
  fGuiScript.Free;
  fGuiDefence.Free;
  fGuiOffence.Free;

  inherited;
end;


procedure TKMMapEdTown.PageChange(Sender: TObject);
begin
  //Reset cursor mode
  GameCursor.Mode := cmNone;
  GameCursor.Tag1 := 0;

  //Hide existing pages
  fGuiHouses.Hide;
  fGuiUnits.Hide;
  fGuiScript.Hide;
  fGuiDefence.Hide;
  fGuiOffence.Hide;

  if (Sender = Button_Town[ttHouses]) then
    fGuiHouses.Show
  else
  if (Sender = Button_Town[ttUnits]) then
    fGuiUnits.Show
  else
  if (Sender = Button_Town[ttScript]) then
    fGuiScript.Show
  else
  if (Sender = Button_Town[ttDefences]) then
    fGuiDefence.Show
  else
  if (Sender = Button_Town[ttOffence]) then
    fGuiOffence.Show;

  //Signal that active page has changed, that may affect layers visibility
  fOnPageChange(Self);
end;


procedure TKMMapEdTown.Show(aPage: TKMTownTab);
begin
  case aPage of
    ttHouses:   fGuiHouses.Show;
    ttUnits:    fGuiUnits.Show;
    ttScript:   fGuiScript.Show;
    ttDefences: fGuiDefence.Show;
    ttOffence:  fGuiOffence.Show;
  end;
end;


function TKMMapEdTown.Visible: Boolean;
begin
  Result := Panel_Town.Visible;
end;


function TKMMapEdTown.Visible(aPage: TKMTownTab): Boolean;
begin
  case aPage of
    ttHouses:   Result := fGuiHouses.Visible;
    ttUnits:    Result := fGuiUnits.Visible;
    ttScript:   Result := fGuiScript.Visible;
    ttDefences: Result := fGuiDefence.Visible;
    ttOffence:  Result := fGuiOffence.Visible;
    else        Result := False;
  end;
end;


procedure TKMMapEdTown.UpdatePlayer(aPlayerIndex: TPlayerIndex);
var
  isAI: Boolean;
begin
  //Update colors
  Button_Town[ttUnits].FlagColor := gPlayers[aPlayerIndex].FlagColor;
  fGuiUnits.UpdateColors(gPlayers[aPlayerIndex].FlagColor);

  isAI := (gPlayers[aPlayerIndex].PlayerType = pt_Computer);

  Button_Town[ttScript].Enabled := isAI;
  Button_Town[ttDefences].Enabled := isAI;
  Button_Town[ttOffence].Enabled := isAI;

  if fGuiScript.Visible or fGuiDefence.Visible or fGuiOffence.Visible then
    Button_Town[ttHouses].Click;
end;


end.
