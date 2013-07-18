unit KM_PlayerSpectator;
{$I KaM_Remake.inc}
interface
uses KromUtils,
  KM_CommonClasses, KM_Defaults,
  KM_FogOfWar;


type
  //Wrap to let choose FOW player sees (and let 1 player control several towns
  //or several players to control 1 town in future)
  TKMSpectator = class
  private
    fPlayerIndex: TPlayerIndex;
    fHighlight: TObject; //Unit/House/Group that is shown highlighted to draw players attention
    fHighlightEnd: Cardinal; //Highlight has a short time to live
    fSelected: TObject;
    fFOWIndex: TPlayerIndex; //Unit/House/Group selected by player and shown in UI
    fFogOfWar: TKMFogOfWarOpen; //Stub for MapEd
    procedure SetHighlight(Value: TObject);
    procedure SetSelected(Value: TObject);
    procedure SetPlayerIndex(const Value: TPlayerIndex);
    procedure SetFOWIndex(const Value: TPlayerIndex);
  public
    constructor Create(aPlayerIndex: TPlayerIndex);
    destructor Destroy; override;
    property Highlight: TObject read fHighlight write SetHighlight;
    property Selected: TObject read fSelected write SetSelected;
    property PlayerIndex: TPlayerIndex read fPlayerIndex write SetPlayerIndex;
    property FOWIndex: TPlayerIndex read fFOWIndex write SetFOWIndex;
    function FogOfWar: TKMFogOfWarCommon; //Which FOW we want to see
    function HitTestCursor: TObject;
    procedure UpdateSelect;
    procedure Load(LoadStream: TKMemoryStream);
    procedure Save(SaveStream: TKMemoryStream);
    procedure UpdateState(aTick: Cardinal);
  end;

  //todo: Spectator for Replays and MapEd that sees everything and allows to toggle FOW
  TKMSpectatorReplay = class(TKMSpectator)

  end;


implementation
uses KM_PlayersCollection, KM_Game, KM_UnitGroups, KM_GameCursor, KM_Units_Warrior;


{ TKMSpectator }
constructor TKMSpectator.Create(aPlayerIndex: TPlayerIndex);
begin
  inherited Create;

  fPlayerIndex := aPlayerIndex;

  //Stub that always returns REVEALED
  fFogOfWar := TKMFogOfWarOpen.Create;
end;


destructor TKMSpectator.Destroy;
begin
  Highlight := nil;
  Selected := nil;
  fFogOfWar.Free;
  inherited;
end;


function TKMSpectator.FogOfWar: TKMFogOfWarCommon;
begin
  //fGame = nil in Tests
  if (fGame <> nil) and (fGame.IsReplay or fGame.IsMapEditor) then
    if FOWIndex = -1 then
      Result := fFogOfWar
    else
      Result := gPlayers[FOWIndex].FogOfWar
  else
    Result := gPlayers[PlayerIndex].FogOfWar;
end;


procedure TKMSpectator.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fPlayerIndex);
end;


procedure TKMSpectator.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fPlayerIndex);
end;


//Test if there's object below that player can interact with
//Units and Houses, not Groups
function TKMSpectator.HitTestCursor: TObject;
begin
  Result := gPlayers.GetUnitByUID(GameCursor.ObjectUID);

  //If there's no unit try pick a house on the Cell below
  if Result = nil then
    Result := gPlayers.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
end;


//Select anything player CAN select below cursor
procedure TKMSpectator.UpdateSelect;
var
  G: TKMUnitGroup;
begin
  //In-game player can select only own Units
  if fGame.IsReplay or fGame.IsMapEditor then
    Selected := gPlayers.GetUnitByUID(GameCursor.ObjectUID)
  else
    Selected := gPlayers[fPlayerIndex].Units.GetUnitByUID(GameCursor.ObjectUID);

  //If Id belongs to some Warrior, try to select his group instead
  if Selected is TKMUnitWarrior then
  begin
    if fGame.IsReplay or fGame.IsMapEditor then
      G := gPlayers.GetGroupByMember(TKMUnitWarrior(Selected))
    else
      G := gPlayers[fPlayerIndex].UnitGroups.GetGroupByMember(TKMUnitWarrior(Selected));

    //Warrior might not be assigned to a group while walking out of the Barracks
    if G <> nil then
      Selected := G;
  end;

  //Update selected groups selected unit
  if Selected is TKMUnitGroup then
    TKMUnitGroup(Selected).SelectedUnit := TKMUnitGroup(Selected).MemberByUID(GameCursor.ObjectUID);

  //If there's no unit try pick a house on the Cell below
  if Selected = nil then
    if fGame.IsReplay or fGame.IsMapEditor then
      Selected := gPlayers.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y)
    else
      Selected := gPlayers[fPlayerIndex].HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
end;


procedure TKMSpectator.SetFOWIndex(const Value: TPlayerIndex);
begin
  fFOWIndex := Value;
end;


procedure TKMSpectator.SetHighlight(Value: TObject);
begin
  //We don't increase PointersCount of object because of savegames identicality over MP
  //Objects report on their destruction and set it to nil
  fHighlight := Value;
  fHighlightEnd := TimeGet + 3000;
end;


procedure TKMSpectator.SetPlayerIndex(const Value: TPlayerIndex);
begin
  Assert((MULTIPLAYER_CHEATS or not fGame.IsMultiplayer) or fGame.IsReplay or fGame.IsMapEditor);
  fPlayerIndex := Value;

  if not fGame.IsReplay and not fGame.IsMapEditor then
    Selected := nil;
end;


procedure TKMSpectator.SetSelected(Value: TObject);
begin
  //We don't increase PointersCount of object because of savegames identicality over MP
  //Objects report on their destruction and set it to nil
  fSelected := Value;
end;


procedure TKMSpectator.UpdateState(aTick: Cardinal);
begin
  //Hide the highlight
  if TimeGet > fHighlightEnd then
    fHighlight := nil;
end;


end.
