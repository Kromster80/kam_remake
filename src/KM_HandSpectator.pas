unit KM_HandSpectator;
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
    fHandIndex: THandIndex;
    fHighlight: TObject; //Unit/House/Group that is shown highlighted to draw players attention
    fHighlightEnd: Cardinal; //Highlight has a short time to live
    fSelected: TObject;
    fFOWIndex: THandIndex; //Unit/House/Group selected by player and shown in UI
    fFogOfWar: TKMFogOfWarOpen; //Stub for MapEd
    procedure SetHighlight(Value: TObject);
    procedure SetSelected(Value: TObject);
    procedure SetHandIndex(const Value: THandIndex);
    procedure SetFOWIndex(const Value: THandIndex);
  public
    constructor Create(aHandIndex: THandIndex);
    destructor Destroy; override;
    property Highlight: TObject read fHighlight write SetHighlight;
    property Selected: TObject read fSelected write SetSelected;
    property HandIndex: THandIndex read fHandIndex write SetHandIndex;
    property FOWIndex: THandIndex read fFOWIndex write SetFOWIndex;
    function FogOfWar: TKMFogOfWarCommon; //Which FOW we want to see
    function HitTestCursor: TObject;
    procedure UpdateSelect;
    procedure Load(LoadStream: TKMemoryStream);
    procedure Save(SaveStream: TKMemoryStream);
    procedure UpdateState(aTick: Cardinal);
  end;


implementation
uses KM_HandsCollection, KM_Game, KM_Houses, KM_Units, KM_UnitGroups, KM_GameCursor, KM_Units_Warrior;


{ TKMSpectator }
constructor TKMSpectator.Create(aHandIndex: THandIndex);
begin
  inherited Create;

  fHandIndex := aHandIndex;

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
  if (gGame <> nil) and (gGame.IsReplay or gGame.IsMapEditor) then
    if FOWIndex = -1 then
      Result := fFogOfWar
    else
      Result := gHands[FOWIndex].FogOfWar
  else
    Result := gHands[HandIndex].FogOfWar;
end;


procedure TKMSpectator.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fHandIndex);
end;


procedure TKMSpectator.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fHandIndex);
end;


//Test if there's object below that player can interact with
//Units and Houses, not Groups
function TKMSpectator.HitTestCursor: TObject;
begin
  Result := gHands.GetUnitByUID(GameCursor.ObjectUID);
  if (Result is TKMUnit) and TKMUnit(Result).IsDeadOrDying then
    Result := nil;

  //If there's no unit try pick a house on the Cell below
  if Result = nil then
  begin
    Result := gHands.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
    if (Result is TKMHouse) and TKMHouse(Result).IsDestroyed then
      Result := nil;
  end;
end;


//Select anything player CAN select below cursor
procedure TKMSpectator.UpdateSelect;
var
  G: TKMUnitGroup;
  NewSelected: TObject;
begin
  //In-game player can select only own Units
  if gGame.IsReplay or gGame.IsMapEditor then
    NewSelected := gHands.GetUnitByUID(GameCursor.ObjectUID)
  else
    NewSelected := gHands[fHandIndex].Units.GetUnitByUID(GameCursor.ObjectUID);

  //Don't allow the player to select dead units
  if (NewSelected is TKMUnit) and TKMUnit(NewSelected).IsDeadOrDying then
    NewSelected := nil;

  //If Id belongs to some Warrior, try to select his group instead
  if NewSelected is TKMUnitWarrior then
  begin
    if gGame.IsReplay or gGame.IsMapEditor then
      G := gHands.GetGroupByMember(TKMUnitWarrior(NewSelected))
    else
      G := gHands[fHandIndex].UnitGroups.GetGroupByMember(TKMUnitWarrior(NewSelected));

    //Warrior might not be assigned to a group while walking out of the Barracks
    if G <> nil then
      NewSelected := G
    else
      NewSelected := nil; //Can't select warriors until they have been assigned a group
  end;

  //Update selected groups selected unit
  if NewSelected is TKMUnitGroup then
    TKMUnitGroup(NewSelected).SelectedUnit := TKMUnitGroup(NewSelected).MemberByUID(GameCursor.ObjectUID);

  //If there's no unit try pick a house on the Cell below
  if NewSelected = nil then
  begin
    if gGame.IsReplay or gGame.IsMapEditor then
      NewSelected := gHands.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y)
    else
      NewSelected := gHands[fHandIndex].HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);

    //Don't allow the player to select destroyed houses
    if (NewSelected is TKMHouse) and TKMHouse(NewSelected).IsDestroyed then
      NewSelected := nil;
  end;

  //Don't clear the old selection unless we found something new
  if NewSelected <> nil then
    Selected := NewSelected;
end;


procedure TKMSpectator.SetFOWIndex(const Value: THandIndex);
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


procedure TKMSpectator.SetHandIndex(const Value: THandIndex);
begin
  Assert((MULTIPLAYER_CHEATS or not gGame.IsMultiplayer) or gGame.IsReplay or gGame.IsMapEditor);
  fHandIndex := Value;

  if not gGame.IsReplay and not gGame.IsMapEditor then
    Selected := nil;
end;


procedure TKMSpectator.SetSelected(Value: TObject);
begin
  //We don't increase PointersCount of object because of savegames identicality over MP
  //Objects report on their destruction and set it to nil
  fSelected := Value;
end;


procedure TKMSpectator.UpdateState;
begin
  //Hide the highlight
  if TimeGet > fHighlightEnd then
    fHighlight := nil;
end;


end.
