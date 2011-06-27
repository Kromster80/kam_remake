unit KM_Projectiles;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KromUtils, KM_Utils, KM_Defaults, KM_CommonTypes, KM_Units, KM_Houses;

//todo: Make projectiles take into account targets speed/direction and aim for predicted position

{Projectiles in-game: arrows, bolts, rocks, etc..}
//Once launched they are on their own
type
  TKMProjectiles = class
  private
    fItems:array of record //1..n
      fScreenStart:TKMPointF; //Screen-space trajectory start
      fScreenEnd:TKMPointF;   //Screen-space trajectory end

      fTarget:TKMPointF;  //Tile-space target before jitter (used for debug and house hits)
      fTargetJ:TKMPointF; //Tile-space target

      fType:TProjectileType; //type of projectile (arrow, bolt, rocks, etc..)
      fOwner:TPlayerIndex; //The ID of the player who launched the projectile, used for kill statistics
      fSpeed:single; //Each projectile speed may vary a little bit
      fArc:single; //Thats how high projectile will go along parabola (varies a little more)
      fPosition:single; //Projectiles position along the route Start>>End
      fLength:single; //Route length to look-up for hit
    end;

    function AddItem(I:integer; aStart,aEnd:TKMPointF; aProjType:TProjectileType; aOwner:TPlayerIndex):word;
    function AddNewProjectile(aProjType:TProjectileType):integer;
    function ProjectileVisible(aIndex:integer):boolean;
  public
    constructor Create;
    function AimTarget(aStart:TKMPointF; aTarget:TKMUnit; aProjType:TProjectileType; aOwner:TPlayerIndex):word; overload;
    function AimTarget(aStart:TKMPointF; aTarget:TKMHouse; aProjType:TProjectileType; aOwner:TPlayerIndex):word; overload;

    procedure UpdateState;
    procedure Paint;

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
  end;


implementation
uses KM_Sound, KM_Render, KM_PlayersCollection, KM_Terrain;


{ TKMProjectiles }
constructor TKMProjectiles.Create;
begin
  Inherited;
  //Nothing here yet
end;


//Find empty spot or add one
function TKMProjectiles.AddNewProjectile(aProjType:TProjectileType):integer;
begin
  Result := -1;
  repeat
    inc(Result);
    if Result >= length(fItems) then
      SetLength(fItems, Result+8); //Add new
  until(fItems[Result].fSpeed = 0);

  //Fill in basic info
  fItems[Result].fType  := aProjType;
  fItems[Result].fSpeed := ProjectileSpeeds[aProjType] + RandomS(0.05);
  fItems[Result].fArc   := ProjectileArcs[aProjType, 1] + RandomS(ProjectileArcs[aProjType, 2]);
end;


function TKMProjectiles.AimTarget(aStart:TKMPointF; aTarget:TKMUnit; aProjType:TProjectileType; aOwner:TPlayerIndex):word;
var Target:TKMPointF; i:integer;
begin
  i := AddNewProjectile(aProjType);

  //Now we know projectiles speed and aim, we can predict where target will be at the time projectile hits it

  Target := aTarget.PositionF;

  Result := AddItem(i, aStart, Target, aProjType, aOwner);
end;


function TKMProjectiles.AimTarget(aStart:TKMPointF; aTarget:TKMHouse; aProjType:TProjectileType; aOwner:TPlayerIndex):word;
var i:integer;
begin
  i := AddNewProjectile(aProjType);

  Result := AddItem(i, aStart, KMPointF(aTarget.GetRandomCellWithin), aProjType, aOwner);
end;


{ Return flight time (archers like to know when they hit target before firing again) }
function TKMProjectiles.AddItem(I:integer; aStart,aEnd:TKMPointF; aProjType:TProjectileType; aOwner:TPlayerIndex):word;
const //TowerRock position is a bit different for said reasons
  OffsetX:array[TProjectileType] of single = (0.5,0.5,-0.25); //Recruit stands in entrance, Tower middleline is X-0.75
  OffsetY:array[TProjectileType] of single = (0.2,0.2,-0.5); //Add towers height
var
  Jitter:single;
begin
  fItems[i].fOwner  := aOwner;

  Jitter := GetLength(aStart, aEnd) * ProjectileJitter[aProjType];

  fItems[i].fTarget.X   := aEnd.X; //Thats logical target in tile-coords
  fItems[i].fTarget.Y   := aEnd.Y;
  fItems[i].fTargetJ.X  := aEnd.X + RandomS(Jitter); //Thats logical target in tile-coords
  fItems[i].fTargetJ.Y  := aEnd.Y + RandomS(Jitter);

  fItems[i].fScreenStart.X := aStart.X + OffsetX[aProjType];
  fItems[i].fScreenStart.Y := aStart.Y - fTerrain.InterpolateLandHeight(aStart)/CELL_HEIGHT_DIV + OffsetX[aProjType];
  fItems[i].fScreenEnd.X := fItems[i].fTargetJ.X + 0.5; //projectile hits on Unit's chest height
  fItems[i].fScreenEnd.Y := fItems[i].fTargetJ.Y + 0.5 - fTerrain.InterpolateLandHeight(aEnd)/CELL_HEIGHT_DIV;

  fItems[i].fPosition := 0; //projectile position on its route
  fItems[i].fLength   := GetLength(fItems[i].fScreenStart.X - fItems[i].fScreenEnd.X, fItems[i].fScreenStart.Y - fItems[i].fScreenEnd.Y); //route length

  if (MyPlayer.FogOfWar.CheckTileRevelation(KMPointRound(aStart).X, KMPointRound(aStart).Y) >= 255) then
    fSoundLib.Play(ProjectileSounds[pt_Arrow], KMPointRound(aStart), true);

  Result := round(fItems[i].fLength / fItems[i].fSpeed);
end;


//Update all items positions and kill some targets
procedure TKMProjectiles.UpdateState;
const HTicks = 6; //The number of ticks before hitting that an arrow will make the hit noise
var i:integer; U:TKMUnit; H:TKMHouse;
    Damage : Smallint;
begin
  for i:=0 to length(fItems)-1 do
    with fItems[i] do
      if fSpeed <> 0 then
      begin
        fPosition := fPosition + fSpeed;

        //Will hit the target in X..X-1 ticks (this ensures it only happens once)
        //Can't use InRange cos it might get called twice due to <= X <= comparison
        if (fLength - HTicks*fSpeed <= fPosition) and (fPosition < fLength - (HTicks-1)*fSpeed)
        and (fType in [pt_Arrow, pt_Bolt]) then //These projectiles make the sound
          fSoundLib.Play(sfx_ArrowHit, KMPointRound(fTargetJ));

        if fPosition >= fLength then begin
          fSpeed := 0; //remove projectile
          U := fPlayers.UnitsHitTestF(fTargetJ, false);
          case fType of
            pt_Arrow,
            pt_Bolt:      if (U <> nil)and(not U.IsDeadOrDying)and(U.Visible)and(not (U is TKMUnitAnimal)) then
                          begin
                            Damage := 0;
                            if fType = pt_Arrow then Damage := UnitStat[byte(ut_Bowman)].Attack;
                            if fType = pt_Bolt then Damage := UnitStat[byte(ut_Arbaletman)].Attack;
                            //Arrows are more likely to cause damage when the unit is closer
                            if Random(Round(8 * GetLength(U.PositionF,fTargetJ))) = 0 then
                              if U.HitPointsDecrease(Damage) then
                                fPlayers.Player[fOwner].Stats.UnitKilled(U.UnitType);
                          end
                          else
                          begin
                            //Stray arrows do not damage houses, they are only hit when directly aimed at. Hence use fTarget not fTargetJ
                            H := fPlayers.HousesHitTest(round(fTarget.X), round(fTarget.Y));
                            if (H <> nil) then
                              if H.AddDamage(1) then //House was destroyed
                                fPlayers.Player[fOwner].Stats.HouseDestroyed(H.GetHouseType);
                          end;
            pt_TowerRock: if (U <> nil)and(not U.IsDeadOrDying)and(U.Visible)and(not (U is TKMUnitAnimal)) then
                            if U.HitPointsDecrease(10) then //Instant death
                              fPlayers.Player[fOwner].Stats.UnitKilled(U.UnitType);
          end;
        end;
      end;
end;


//Test wherever projectile is visible (used by rocks thrown from Towers mostly)
function TKMProjectiles.ProjectileVisible(aIndex:integer):boolean;
begin
  case fItems[aIndex].fType of
    pt_Arrow:      Result := true;
    pt_Bolt:       Result := true;
    pt_TowerRock:  if (fItems[aIndex].fScreenEnd.Y - fItems[aIndex].fScreenStart.Y) < 0 then
                     Result := fItems[aIndex].fPosition >= 0.2 //fly behind a Tower
                   else
                     Result := true;
    else           Result := true;
  end;
end;


procedure TKMProjectiles.Paint;
var
  i:integer;
  MixValue:single;
  MixArc:single; //mix Arc shape
  P1,P2:TKMPointF; //Arrows and bolts send 2 points for head and tail
  Dir:TKMDirection;
begin
  for i:=0 to length(fItems)-1 do
    if (fItems[i].fSpeed<>0) and ProjectileVisible(i) then
    begin

      MixValue := fItems[i].fPosition / fItems[i].fLength; // 0 >> 1
      case fItems[i].fType of
        pt_Arrow, pt_Bolt:
        begin
          MixArc := sin(MixValue*pi);   // 0 >> 1 >> 0 Parabola
          P1 := mix(fItems[i].fScreenEnd, fItems[i].fScreenStart, MixValue);
          P1.Y := P1.Y - fItems[i].fArc * MixArc;
          P2.X := P1.X+3; P2.Y := P2.Y+1;
          Dir := KMGetDirection(fItems[i].fScreenStart, fItems[i].fScreenEnd);
          fRender.RenderProjectile(fItems[i].fType, P1.X, P1.Y, MixValue, Dir);
        end;

        pt_TowerRock:
        begin
          MixArc := cos(MixValue*pi/2); // 1 >> 0      Half-parabola
          P1 := mix(fItems[i].fScreenEnd, fItems[i].fScreenStart, MixValue);
          P1.Y := P1.Y - fItems[i].fArc * MixArc;
          P2.X := 0; P2.Y := 0;
          fRender.RenderProjectile(fItems[i].fType, P1.X, P1.Y, MixValue);
        end;
        else
      end;

      if SHOW_PROJECTILES then begin
        fRender.RenderDebugProjectile(fItems[i].fScreenStart.X,
                                      fItems[i].fScreenStart.Y,
                                      fItems[i].fScreenEnd.X,
                                      fItems[i].fScreenEnd.Y);

        fRender.RenderDebugProjectile(fItems[i].fTarget.X,
                                      fItems[i].fTarget.Y,
                                      fItems[i].fTargetJ.X,
                                      fItems[i].fTargetJ.Y);
      end;
    end;
end;


procedure TKMProjectiles.Save(SaveStream:TKMemoryStream);
var i, Count:integer;
begin
  SaveStream.Write('Projectiles');

  Count := 0;
  for i:=0 to length(fItems)-1 do
    if fItems[i].fSpeed <> 0 then inc(Count);
  SaveStream.Write(Count);

  for i:=0 to length(fItems)-1 do
    if fItems[i].fSpeed <> 0 then
      SaveStream.Write(fItems[i], SizeOf(fItems[i]));
end;


procedure TKMProjectiles.Load(LoadStream:TKMemoryStream);
var s:string;
  i,Count:integer;
begin
  LoadStream.Read(s);
  Assert(s = 'Projectiles', 'Projectiles not found');

  LoadStream.Read(Count);
  SetLength(fItems, Count);

  for i:=0 to Count-1 do
    LoadStream.Read(fItems[i], SizeOf(fItems[i]));
end;


end.
