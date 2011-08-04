unit KM_Projectiles;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, Math, KromUtils, KM_Utils, KM_Defaults, KM_CommonTypes, KM_Units, KM_Houses, KM_Points;


{Projectiles in-game: arrows, bolts, rocks, etc..}
//Once launched they are on their own
type
  TKMProjectiles = class
  private
    fItems:array of record //1..n
      fScreenStart:TKMPointF; //Screen-space trajectory start
      fScreenEnd:TKMPointF;   //Screen-space trajectory end

      fAim:TKMPointF;  //Where we were aiming to hit
      fTarget:TKMPointF; //Where projectile will hit

      fType:TProjectileType; //type of projectile (arrow, bolt, rocks, etc..)
      fOwner:TPlayerIndex; //The ID of the player who launched the projectile, used for kill statistics
      fSpeed:single; //Each projectile speed may vary a little bit
      fArc:single; //Thats how high projectile will go along parabola (varies a little more)
      fPosition:single; //Projectiles position along the route Start>>End
      fLength:single; //Route length to look-up for hit
    end;

    function AddItem(aStart,aAim,aEnd:TKMPointF; aSpeed:single; aProjType:TProjectileType; aOwner:TPlayerIndex):word;
    procedure RemItem(aIndex: integer);
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
uses KM_Sound, KM_Render, KM_PlayersCollection, KM_Terrain, KM_ResourceGFX;


{ TKMProjectiles }
constructor TKMProjectiles.Create;
begin
  Inherited;
  //Nothing here yet
end;


procedure TKMProjectiles.RemItem(aIndex: integer);
begin
  fItems[aIndex].fSpeed := 0;
end;


function TKMProjectiles.AimTarget(aStart:TKMPointF; aTarget:TKMUnit; aProjType:TProjectileType; aOwner:TPlayerIndex):word;
var
  TargetVector,Target,TargetPosition:TKMPointF;
  A,B,C,D:single;
  TimeToHit, Time1, Time2: single;
  Jitter, Speed:single;
begin
  //Now we know projectiles speed and aim, we can predict where target will be at the time projectile hits it

  //I wonder if medieval archers knew about vectors and quadratic equations

  TargetPosition.X := (aTarget.PositionF.X - aStart.X);
  TargetPosition.Y := (aTarget.PositionF.Y - aStart.Y);
  TargetVector := aTarget.GetMovementVector;

  { Target.X := TargetPosition.X + TargetVector.X * Time;
    Target.Y := TargetPosition.Y + TargetVector.Y * Time;

    FlightDistance := ArrowSpeed * Time;

    sqr(Target.X)+sqr(Target.Y) = sqr(FlightDistance);

    sqr(TargetPosition.X + TargetVector.X * Time) + sqr(TargetPosition.Y + TargetVector.Y * Time) = sqr(ArrowSpeed * Time)

    sqr(TargetPosition.X) + 2 * Time * TargetPosition.X * TargetVector.X + sqr(Time) * sqr(TargetVector.X) +
    sqr(TargetPosition.Y) + 2 * Time * TargetPosition.Y * TargetVector.Y + sqr(Time) * sqr(TargetVector.Y) =
    sqr(ArrowSpeed) * sqr(Time)

    sqr(Time) * (sqr(TargetVector.X) + sqr(TargetVector.Y) - sqr(ArrowSpeed)) +
    2 * Time * (TargetPosition.X * TargetVector.X + TargetPosition.Y * TargetVector.Y) +
    sqr(TargetPosition.X) + sqr(TargetPosition.Y) = 0

    //Lets try to solve this quadratic equation
    //ATT + BT + C = 0
    //by using formulae X = (-B +- sqrt(B*B - 4*A*C)) / 2*A
    A = sqr(TargetVector.X) + sqr(TargetVector.Y) - sqr(ArrowSpeed)
    B = 2 * (TargetPosition.X * TargetVector.X + TargetPosition.Y * TargetVector.Y)
    C = sqr(TargetPosition.X) + sqr(TargetPosition.Y) }

  Speed := ProjectileSpeeds[aProjType] + KaMRandomS(0.05);

  A := sqr(TargetVector.X) + sqr(TargetVector.Y) - sqr(Speed);
  B := 2 * (TargetPosition.X * TargetVector.X + TargetPosition.Y * TargetVector.Y);
  C := sqr(TargetPosition.X) + sqr(TargetPosition.Y);

  D := sqr(B) - 4 * A * C;

  if (D >= 0) and (A <> 0) then
  begin
    Time1 := (-B + sqrt(D)) / (2 * A);
    Time2 := (-B - sqrt(D)) / (2 * A);

    //Choose smallest positive time
    if (Time1 > 0) and (Time2 > 0) then
      TimeToHit := Math.min(Time1, Time2)
    else
    if (Time1 < 0) and (Time2 < 0) then
      TimeToHit := 0
    else
      TimeToHit := Math.max(Time1, Time2);
  end
  else
    TimeToHit := 0;

  if TimeToHit <> 0 then
  begin
    Jitter := GetLength(aStart, aTarget.PositionF) * ProjectileJitter[aProjType];

    Target.X := aTarget.PositionF.X + TargetVector.X*TimeToHit + KaMRandomS(Jitter);
    Target.Y := aTarget.PositionF.Y + TargetVector.Y*TimeToHit + KaMRandomS(Jitter);
    Result := AddItem(aStart, aTarget.PositionF, Target, Speed, aProjType, aOwner);
  end else
    Result := 0;
end;


function TKMProjectiles.AimTarget(aStart:TKMPointF; aTarget:TKMHouse; aProjType:TProjectileType; aOwner:TPlayerIndex):word;
var
  Speed: single;
  Aim, Target: TKMPointF;
begin
  Speed := ProjectileSpeeds[aProjType] + KaMRandomS(0.05);

  Aim := KMPointF(aTarget.GetRandomCellWithin);
  Target.X := Aim.X + KaMRandom; //So that arrows were within house area, without attitude to tile corners
  Target.Y := Aim.Y + KaMRandom;

  Result := AddItem(aStart, Aim, Target, Speed, aProjType, aOwner);
end;


{ Return flight time (archers like to know when they hit target before firing again) }
function TKMProjectiles.AddItem(aStart,aAim,aEnd:TKMPointF; aSpeed:single; aProjType:TProjectileType; aOwner:TPlayerIndex):word;
const //TowerRock position is a bit different for reasons said below
  OffsetX:array[TProjectileType] of single = (0.5,0.5,-0.25); //Recruit stands in entrance, Tower middleline is X-0.75
  OffsetY:array[TProjectileType] of single = (0.2,0.2,-0.5); //Add towers height
var
  i:integer;
begin
  i := -1;
  repeat
    inc(i);
    if i >= length(fItems) then
      SetLength(fItems, i+8); //Add new
  until(fItems[i].fSpeed = 0);

  //Fill in basic info
  fItems[i].fType   := aProjType;
  fItems[i].fSpeed  := aSpeed;
  fItems[i].fArc    := ProjectileArcs[aProjType, 1] + KaMRandomS(ProjectileArcs[aProjType, 2]);
  fItems[i].fOwner  := aOwner;
  fItems[i].fAim    := aAim;
  fItems[i].fTarget := aEnd;

  fItems[i].fScreenStart.X := aStart.X + OffsetX[aProjType];
  fItems[i].fScreenStart.Y := aStart.Y - fTerrain.InterpolateLandHeight(aStart)/CELL_HEIGHT_DIV + OffsetX[aProjType];
  fItems[i].fScreenEnd.X := fItems[i].fTarget.X + 0.5; //projectile hits on Unit's chest height
  fItems[i].fScreenEnd.Y := fItems[i].fTarget.Y + 0.5 - fTerrain.InterpolateLandHeight(aEnd)/CELL_HEIGHT_DIV;

  fItems[i].fPosition := 0; //projectile position on its route
  fItems[i].fLength   := GetLength(fItems[i].fScreenStart.X - fItems[i].fScreenEnd.X, fItems[i].fScreenStart.Y - fItems[i].fScreenEnd.Y); //route length

  if (MyPlayer.FogOfWar.CheckTileRevelation(KMPointRound(aStart).X, KMPointRound(aStart).Y) >= 255) then
    fSoundLib.Play(ProjectileLaunchSounds[aProjType], KMPointRound(aStart), true);

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
        if (fLength - HTicks*fSpeed <= fPosition) and (fPosition < fLength - (HTicks-1)*fSpeed) then
          fSoundLib.Play(ProjectileHitSounds[fType], KMPointRound(fTarget));

        if fPosition >= fLength then begin
          if KaMRandom >= ProjectileMissChance[fType] then
          begin
            U := fPlayers.UnitsHitTestF(fTarget, false);
            case fType of
              pt_Arrow,
              pt_Bolt:      if (U <> nil)and(not U.IsDeadOrDying)and(U.Visible)and(not (U is TKMUnitAnimal)) then
                            begin
                              Damage := 0;
                              if fType = pt_Arrow then Damage := fResource.UnitDat[ut_Bowman].Attack;
                              if fType = pt_Bolt then Damage := fResource.UnitDat[ut_Arbaletman].Attack;
                              //Arrows are more likely to cause damage when the unit is closer
                              Damage := Round(Damage * (1-Math.min(GetLength(U.PositionF,fTarget),1)));
                              if FRIENDLY_FIRE or (fPlayers.CheckAlliance(fOwner, U.GetOwner)= at_Enemy) then
                                if U.HitPointsDecrease(Damage,false) then
                                  fPlayers.Player[fOwner].Stats.UnitKilled(U.UnitType);
                            end
                            else
                            begin
                              H := fPlayers.HousesHitTest(round(fTarget.X), round(fTarget.Y));
                              if (H <> nil) and (FRIENDLY_FIRE or (fPlayers.CheckAlliance(fOwner, H.GetOwner)= at_Enemy)) then
                                if H.AddDamage(1) then //House was destroyed
                                  fPlayers.Player[fOwner].Stats.HouseDestroyed(H.GetHouseType);
                            end;
              pt_TowerRock: if (U <> nil)and(not U.IsDeadOrDying)and(U.Visible)and(not (U is TKMUnitAnimal)) then
                              if FRIENDLY_FIRE or (fPlayers.CheckAlliance(fOwner, U.GetOwner)= at_Enemy) then
                                if U.HitPointsDecrease(U.GetMaxHitPoints,true) then //Instant death
                                  fPlayers.Player[fOwner].Stats.UnitKilled(U.UnitType);
            end;
          end;
          RemItem(i);
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
          fRender.RenderProjectile(fItems[i].fType, P1.X, P1.Y, MixValue, dir_N); //Direction will be ignored
        end;
        else
      end;

      if SHOW_PROJECTILES then begin
        fRender.RenderDebugProjectile(fItems[i].fScreenStart.X,
                                      fItems[i].fScreenStart.Y,
                                      fItems[i].fScreenEnd.X,
                                      fItems[i].fScreenEnd.Y);

        fRender.RenderDebugProjectile(fItems[i].fAim.X,
                                      fItems[i].fAim.Y,
                                      fItems[i].fTarget.X,
                                      fItems[i].fTarget.Y);
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
