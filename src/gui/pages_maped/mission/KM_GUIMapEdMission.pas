unit KM_GUIMapEdMission;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, Math, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics,
   KM_GUIMapEdMissionMode,
   KM_GUIMapEdMissionAlliances,
   KM_GUIMapEdMissionPlayers;

type
  TKMMissionTab = (mtMode, mtAlliances, mtPlayers);

  TKMMapEdMission = class
  private
    fOnPageChange: TNotifyEvent;

    fGuiMissionMode: TKMMapEdMissionMode;
    fGuiMissionAlliances: TKMMapEdMissionAlliances;
    fGuiMissionPlayers: TKMMapEdMissionPlayers;

    procedure PageChange(Sender: TObject);
  protected
    Panel_Mission: TKMPanel;
    Button_Mission: array [TKMMissionTab] of TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
    destructor Destroy; override;

    procedure Show(aPage: TKMMissionTab);
    procedure ShowIndex(aIndex: Byte);
    function Visible(aPage: TKMMissionTab): Boolean; overload;
    function Visible: Boolean; overload;
  end;


implementation
uses
  KM_ResTexts, KM_GameCursor, KM_RenderUI, KM_InterfaceGame;


{ TKMMapEdMission }
constructor TKMMapEdMission.Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent);
const
  TabGlyph: array [TKMMissionTab] of Word    = (41, 386, 656);
  TabHint : array [TKMMissionTab] of Word = (
    TX_MAPED_MISSION_MODE,
    TX_MAPED_ALLIANCE,
    TX_MAPED_PLAYERS_TYPE);
var
  MT: TKMMissionTab;
begin
  inherited Create;

  fOnPageChange := aOnPageChange;

  Panel_Mission := TKMPanel.Create(aParent, 0, 45, TB_WIDTH, 28);

  for MT := Low(TKMMissionTab) to High(TKMMissionTab) do
  begin
    Button_Mission[MT] := TKMButton.Create(Panel_Mission, SMALL_PAD_W * Byte(MT), 0, SMALL_TAB_W, SMALL_TAB_H,  TabGlyph[MT], rxGui, bsGame);
    Button_Mission[MT].Hint := gResTexts[TabHint[MT]];
    Button_Mission[MT].OnClick := PageChange;
  end;

  fGuiMissionMode := TKMMapEdMissionMode.Create(Panel_Mission);
  fGuiMissionAlliances := TKMMapEdMissionAlliances.Create(Panel_Mission);
  fGuiMissionPlayers := TKMMapEdMissionPlayers.Create(Panel_Mission);
end;


destructor TKMMapEdMission.Destroy;
begin
  fGuiMissionMode.Free;
  fGuiMissionAlliances.Free;
  fGuiMissionPlayers.Free;

  inherited;
end;


procedure TKMMapEdMission.PageChange(Sender: TObject);
begin
  //Reset cursor mode
  gGameCursor.Mode := cmNone;

  //Hide existing pages
  fGuiMissionMode.Hide;
  fGuiMissionAlliances.Hide;
  fGuiMissionPlayers.Hide;

  if (Sender = Button_Mission[mtMode]) then
    fGuiMissionMode.Show
  else
  if (Sender = Button_Mission[mtAlliances]) then
    fGuiMissionAlliances.Show
  else
  if (Sender = Button_Mission[mtPlayers]) then
    fGuiMissionPlayers.Show;

  //Signal that active page has changed, that may affect layers visibility
  fOnPageChange(Self);
end;


procedure TKMMapEdMission.Show(aPage: TKMMissionTab);
begin
  case aPage of
    mtMode:       fGuiMissionMode.Show;
    mtAlliances:  fGuiMissionAlliances.Show;
    mtPlayers:    fGuiMissionPlayers.Show;
  end;

  //Signal that active page has changed, that may affect layers visibility
  fOnPageChange(Self);
end;


procedure TKMMapEdMission.ShowIndex(aIndex: Byte);
begin
  if aIndex in [Byte(Low(TKMMissionTab))..Byte(High(TKMMissionTab))] then
  begin
    PageChange(nil); //Hide existing pages
    Show(TKMMissionTab(aIndex));
  end;
end;


function TKMMapEdMission.Visible: Boolean;
begin
  Result := Panel_Mission.Visible;
end;


function TKMMapEdMission.Visible(aPage: TKMMissionTab): Boolean;
begin
  case aPage of
    mtMode:       Result := fGuiMissionMode.Visible;
    mtAlliances:  Result := fGuiMissionAlliances.Visible;
    mtPlayers:    Result := fGuiMissionPlayers.Visible;
    else          Result := False;
  end;
end;


end.
