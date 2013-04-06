unit KM_PlayerSpectator;
{$I KaM_Remake.inc}
interface
uses KromUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_FogOfWar;


type
  TKMSpectator = class
  private
    fPlayerIndex: TPlayerIndex;
    fHighlight: TObject; //Unit/House/Group that is shown highlighted to draw players attention
    fHighlightEnd: Cardinal; //Highlight has a short time to live
    fSelected: TObject;
    fFOWIndex: TPlayerIndex; //Unit/House/Group selected by player and shown in UI
    fFogOfWar: TKMFogOfWarOpen;
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
    procedure SelectHitTest(X, Y: Integer);
    function HitTest(X, Y: Integer): TObject;
    procedure Load(LoadStream: TKMemoryStream);
    procedure Save(SaveStream: TKMemoryStream);
    procedure UpdateState(aTick: Cardinal);
  end;


implementation
uses KM_PlayersCollection, KM_Game, KM_Units_Warrior, KM_Terrain, KM_UnitGroups;


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
  if fGame.IsReplay or fGame.IsMapEditor then
    if FOWIndex = -1 then
      Result := fFogOfWar
    else
      Result := fPlayers[FOWIndex].FogOfWar
  else
    Result := fPlayers[PlayerIndex].FogOfWar;
end;


procedure TKMSpectator.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fPlayerIndex);
end;


procedure TKMSpectator.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fPlayerIndex);
end;


function TKMSpectator.HitTest(X, Y: Integer): TObject;
begin
  if fGame.IsReplay or fGame.IsMapEditor then
    Result := fPlayers.HitTest(X, Y)
  else
    Result := fPlayers[fPlayerIndex].HitTest(X, Y);
end;


procedure TKMSpectator.SelectHitTest(X,Y: Integer);
var
  Obj: TObject;
begin
  Obj := HitTest(X, Y);

  //Don't select a warrior directly, only select his group.
  //A warrior can be incorrectly selected while walking out of the barracks (before he has a group)
  if Obj is TKMUnitWarrior then Exit;

  if Obj <> nil then
  begin
    Selected := Obj;
    //Update selected unit within a group
    if Selected is TKMUnitGroup then
      TKMUnitGroup(Selected).SelectHitTest(X,Y);
  end;
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
