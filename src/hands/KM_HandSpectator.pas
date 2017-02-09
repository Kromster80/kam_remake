unit KM_HandSpectator;
{$I KaM_Remake.inc}
interface
uses
  KromUtils,
  KM_CommonClasses, KM_Defaults,
  KM_FogOfWar, KM_Hand;


type
  //Wrap to let choose FOW player sees (and let 1 player control several towns
  //or several players to control 1 town in future)
  TKMSpectator = class
  private
    fHandIndex: TKMHandIndex;
    fHighlight: TObject; //Unit/House/Group that is shown highlighted to draw players attention
    fHighlightEnd: Cardinal; //Highlight has a short time to live
    fSelected: TObject;
    fFOWIndex: TKMHandIndex; //Unit/House/Group selected by player and shown in UI
    fFogOfWarOpen: TKMFogOfWarOpen; //Stub for MapEd
    fFogOfWar: TKMFogOfWarCommon; //Pointer to current FOW view, updated by UpdateFogOfWarIndex
    procedure SetHighlight(Value: TObject);
    procedure SetSelected(Value: TObject);
    procedure SeTKMHandIndex(const Value: TKMHandIndex);
    procedure SetFOWIndex(const Value: TKMHandIndex);
    procedure UpdateFogOfWarIndex;
  public
    constructor Create(aHandIndex: TKMHandIndex);
    destructor Destroy; override;
    property Highlight: TObject read fHighlight write SetHighlight;
    property Selected: TObject read fSelected write SetSelected;
    function Hand: TKMHand;
    property HandIndex: TKMHandIndex read fHandIndex write SeTKMHandIndex;
    property FOWIndex: TKMHandIndex read fFOWIndex write SetFOWIndex;
    property FogOfWar: TKMFogOfWarCommon read fFogOfWar;
    function HitTestCursor: TObject;
    function HitTestCursorWGroup: TObject;
    procedure UpdateSelect;
    procedure Load(LoadStream: TKMemoryStream);
    procedure Save(SaveStream: TKMemoryStream);
    procedure UpdateState(aTick: Cardinal);
  end;


implementation
uses
  KM_HandsCollection, KM_Game, KM_Houses, KM_Units, KM_UnitGroups, KM_GameCursor,
  KM_Units_Warrior;


{ TKMSpectator }
constructor TKMSpectator.Create(aHandIndex: TKMHandIndex);
begin
  inherited Create;

  fHandIndex := aHandIndex;

  //Stub that always returns REVEALED
  fFogOfWarOpen := TKMFogOfWarOpen.Create;
  UpdateFogOfWarIndex;
end;


destructor TKMSpectator.Destroy;
begin
  Highlight := nil;
  Selected := nil;
  fFogOfWarOpen.Free;
  inherited;
end;


procedure TKMSpectator.UpdateFogOfWarIndex;
begin
  //fGame = nil in Tests
  if (gGame <> nil) and (gGame.GameMode in [gmMultiSpectate, gmMapEd, gmReplaySingle, gmReplayMulti]) then
    if FOWIndex = -1 then
      fFogOfWar := fFogOfWarOpen
    else
      fFogOfWar := gHands[FOWIndex].FogOfWar
  else
    fFogOfWar := gHands[HandIndex].FogOfWar;
end;


procedure TKMSpectator.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fHandIndex);
  UpdateFogOfWarIndex;
end;


procedure TKMSpectator.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fHandIndex);
end;


function TKMSpectator.Hand: TKMHand;
begin
  Result := gHands[fHandIndex];
end;


//Test if there's object below that player can interact with
//Units and Houses, not Groups
function TKMSpectator.HitTestCursor: TObject;
begin
  Result := gHands.GetUnitByUID(gGameCursor.ObjectUID);
  if ((Result is TKMUnit) and TKMUnit(Result).IsDeadOrDying)
  or (Result is TKMUnitAnimal) then
    Result := nil;

  //If there's no unit try pick a house on the Cell below
  if Result = nil then
  begin
    Result := gHands.HousesHitTest(gGameCursor.Cell.X, gGameCursor.Cell.Y);
    if (Result is TKMHouse) and TKMHouse(Result).IsDestroyed then
      Result := nil;
  end;
end;


//Test if there's object below that player can interact with
//Units and Houses and Groups
function TKMSpectator.HitTestCursorWGroup: TObject;
var G: TKMUnitGroup;
begin
  Result := HitTestCursor;
  if Result is TKMUnitWarrior then
  begin
    if gGame.GameMode in [gmMultiSpectate, gmMapEd, gmReplaySingle, gmReplayMulti]  then
      G := gHands.GetGroupByMember(TKMUnitWarrior(Result))
    else
      G := gHands[fHandIndex].UnitGroups.GetGroupByMember(TKMUnitWarrior(Result));

    //Warrior might not be assigned to a group while walking out of the Barracks
    if G <> nil then
      Result := G
    else
      Result := nil; //Can't select warriors until they have been assigned a group
  end;
end;


//Select anything player CAN select below cursor
procedure TKMSpectator.UpdateSelect;
var
  G: TKMUnitGroup;
  NewSelected: TObject;
begin
  //In-game player can select only own Units
  if gGame.GameMode in [gmMultiSpectate, gmMapEd, gmReplaySingle, gmReplayMulti] then
    NewSelected := gHands.GetUnitByUID(gGameCursor.ObjectUID)
  else
    NewSelected := gHands[fHandIndex].Units.GetUnitByUID(gGameCursor.ObjectUID);

  //Don't allow the player to select dead units
  if ((NewSelected is TKMUnit) and TKMUnit(NewSelected).IsDeadOrDying)
  or (NewSelected is TKMUnitAnimal) then //...or animals
    NewSelected := nil;

  //If Id belongs to some Warrior, try to select his group instead
  if NewSelected is TKMUnitWarrior then
  begin
    if gGame.GameMode in [gmMultiSpectate, gmMapEd, gmReplaySingle, gmReplayMulti]  then
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
    TKMUnitGroup(NewSelected).SelectedUnit := TKMUnitGroup(NewSelected).MemberByUID(gGameCursor.ObjectUID);

  //If there's no unit try pick a house on the Cell below
  if NewSelected = nil then
  begin
    if gGame.GameMode in [gmMultiSpectate, gmMapEd, gmReplaySingle, gmReplayMulti]  then
      NewSelected := gHands.HousesHitTest(gGameCursor.Cell.X, gGameCursor.Cell.Y)
    else
      NewSelected := gHands[fHandIndex].HousesHitTest(gGameCursor.Cell.X, gGameCursor.Cell.Y);

    //Don't allow the player to select destroyed houses
    if (NewSelected is TKMHouse) and TKMHouse(NewSelected).IsDestroyed then
      NewSelected := nil;
  end;

  //Don't clear the old selection unless we found something new
  if NewSelected <> nil then
    Selected := NewSelected;
end;


procedure TKMSpectator.SetFOWIndex(const Value: TKMHandIndex);
begin
  fFOWIndex := Value;
  UpdateFogOfWarIndex;
end;


procedure TKMSpectator.SetHighlight(Value: TObject);
begin
  //We don't increase PointersCount of object because of savegames identicality over MP
  //Objects report on their destruction and set it to nil
  fHighlight := Value;
  fHighlightEnd := TimeGet + 3000;
end;


procedure TKMSpectator.SeTKMHandIndex(const Value: TKMHandIndex);
begin
  Assert(MULTIPLAYER_CHEATS or (gGame.GameMode <> gmMulti));
  fHandIndex := Value;

  if not (gGame.GameMode in [gmMultiSpectate, gmMapEd, gmReplaySingle, gmReplayMulti]) then
    Selected := nil;

  UpdateFogOfWarIndex;
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
  //Units should be deselected when they go inside a house
  if Selected is TKMUnit then
    if not TKMUnit(Selected).Visible then
      Selected := nil;
end;


end.
