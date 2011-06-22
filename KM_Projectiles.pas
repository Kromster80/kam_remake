unit KM_Projectiles;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KromUtils, KM_Utils, KM_Defaults, KM_CommonTypes;


{Projectiles in-game: arrows, bolts, rocks, etc..}
//Once launched they are on their own
type
  TKMProjectiles = class
  private
    fItems:array of record //1..n
      fScreenStart:TKMPointF; //Screen-space trajectory start
      fScreenEnd:TKMPointF; //Screen-space trajectory end

      fTarget:TKMPointF; //Logical tile-coords target before jitter (used for debug only)
      fTargetJ:TKMPointF; //Logical tile-coords target

      fProjType:TProjectileType; //type of projectile (arrow, bolt, rocks, etc..)
      fOwner:shortint; //The ID of the player who launched the projectile, used for kill statistics
      fSpeed:single; //Each projectile speed may vary a little bit
      fArc:single; //Thats how high projectile will go along parabola (varies a little more)
      fPosition:single; //Projectiles position along the route Start>>End
      fLength:single; //Route length to look-up for hit
    end;

  public
    constructor Create;
    function AddItem(aStart,aEnd:TKMPointF; aProjType:TProjectileType; aOwner:shortint; MakeSound:boolean):word;

    function ProjectileVisible(aIndex:integer):boolean;

    procedure UpdateState;
    procedure Paint;

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
  end;


implementation
uses KM_Sound, KM_Render, KM_PlayersCollection, KM_Houses, KM_Units, KM_Terrain;


{ TKMProjectiles }
constructor TKMProjectiles.Create;
begin
  Inherited;
  SetLength(fItems, 2); //Reserve some space already
end;


{ Return flight time (archers like to know when they hit target before firing again) }
function TKMProjectiles.AddItem(aStart,aEnd:TKMPointF; aProjType:TProjectileType; aOwner:shortint; MakeSound:boolean):word;
var i:integer; Jitter:single;
begin
  MakeSound := MakeSound and (MyPlayer.FogOfWar.CheckTileRevelation(KMPointRound(aStart).X, KMPointRound(aStart).Y) >= 255);
  //Find empty spot or add one
  i := 0;
  repeat
    inc(i);
    if i>=length(fItems) then SetLength(fItems,i+10); //Add new
  until(fItems[i].fSpeed=0);

  fItems[i].fProjType := aProjType;
  fItems[i].fOwner := aOwner;

  case aProjType of
    pt_Arrow:     begin
                    if MakeSound then fSoundLib.Play(sfx_BowShoot,KMPointRound(aStart),true);
                    fItems[i].fSpeed    := 0.45 + randomS(0.05);
                    fItems[i].fArc      := 1.5 + randomS(0.25);
                    Jitter := GetLength(aStart.X - aEnd.X, aStart.Y - aEnd.Y) / RANGE_BOWMAN_MAX / 2;
                  end;
    pt_Bolt:      begin
                    if MakeSound then fSoundLib.Play(sfx_CrossbowShoot,KMPointRound(aStart),true);
                    fItems[i].fSpeed    := 0.5 + randomS(0.05);
                    fItems[i].fArc      := 1 + randomS(0.2);
                    Jitter := GetLength(aStart.X - aEnd.X, aStart.Y - aEnd.Y) / RANGE_ARBALETMAN_MAX / 2.5;
                  end;
    pt_TowerRock: begin
                    fItems[i].fSpeed    := 0.6 + randomS(0.05);
                    fItems[i].fArc      := 1.25;
                    Jitter := GetLength(aStart.X - aEnd.X, aStart.Y - aEnd.Y) / RANGE_WATCHTOWER_MAX / 1.5;
                  end;
    else          Jitter := 0; //
  end;

  fItems[i].fTarget.X := aEnd.X; //Thats logical target in tile-coords
  fItems[i].fTarget.Y := aEnd.Y;
  fItems[i].fTargetJ.X := aEnd.X + RandomS(Jitter); //Thats logical target in tile-coords
  fItems[i].fTargetJ.Y := aEnd.Y + RandomS(Jitter);

  //Converting tile-coords into screen coords
  case aProjType of
    pt_Arrow,
    pt_Bolt:      begin
                    fItems[i].fScreenStart.X := aStart.X + 0.5; //
                    fItems[i].fScreenStart.Y := aStart.Y - fTerrain.InterpolateLandHeight(aStart.X,aStart.Y)/CELL_HEIGHT_DIV + 0.2;
                  end;
    pt_TowerRock: begin
                    fItems[i].fScreenStart.X := aStart.X - 0.25; //Recruit stands in entrance, Tower middleline is X-0.75
                    fItems[i].fScreenStart.Y := aStart.Y - fTerrain.InterpolateLandHeight(aStart.X,aStart.Y)/CELL_HEIGHT_DIV - 0.5 {1.75}; //Add towers height
                  end;
  end;

  fItems[i].fScreenEnd.X    := fItems[i].fTargetJ.X + 0.5; //projectile hits on Unit's chest height
  fItems[i].fScreenEnd.Y    := fItems[i].fTargetJ.Y + 0.5 - fTerrain.InterpolateLandHeight(aEnd.X,aEnd.Y)/CELL_HEIGHT_DIV;

  fItems[i].fPosition := 0; //projectile position on its route
  fItems[i].fLength   := GetLength(fItems[i].fScreenStart.X - fItems[i].fScreenEnd.X, fItems[i].fScreenStart.Y - fItems[i].fScreenEnd.Y); //route length
  Result := round(fItems[i].fLength / fItems[i].fSpeed);
end;


//Update all items positions and kill some targets
procedure TKMProjectiles.UpdateState;
const HTicks = 6; //The number of ticks before hitting that an arrow will make the hit noise
var i:integer; U:TKMUnit; H:TKMHouse;
begin
  for i:=1 to length(fItems)-1 do
    with fItems[i] do
      if fSpeed <> 0 then
      begin
        fPosition := fPosition + fSpeed;

        //Will hit the target in X..X-1 ticks (this ensures it only happens once)
        //Can't use InRange cos it might get called twice due to <= X <= comparison
        if (fLength - HTicks*fSpeed <= fPosition) and (fPosition < fLength - (HTicks-1)*fSpeed)
        and (fProjType in [pt_Arrow, pt_Bolt]) then //These projectiles make the sound
          fSoundLib.Play(sfx_ArrowHit, KMPointRound(fTargetJ));

        if fPosition >= fLength then begin
          fSpeed := 0; //remove projectile
          U := fPlayers.UnitsHitTestF(fTargetJ);
          case fProjType of
            pt_Arrow,
            pt_Bolt:      if (U <> nil)and(not U.IsDeadOrDying)and(U.Visible)and(not (U is TKMUnitAnimal)) then
                          begin
                            //Arrows are more likely to cause damage when the unit is closer
                            if Random(Round(8 * GetLength(U.PositionF,fTargetJ))) = 0 then
                              if  U.HitPointsDecrease(1) then
                                if (fPlayers <> nil) then
                                  fPlayers.Player[fOwner].Stats.UnitKilled(U.UnitType);
                          end
                          else
                          begin
                            //Stray arrows do not damage houses, they are only hit when directly aimed at. Hence use fTarget not fTargetJ
                            H := fPlayers.HousesHitTest(round(fTarget.X), round(fTarget.Y));
                            if (H <> nil) then
                              if H.AddDamage(1) then //House was destroyed
                                if (fPlayers <> nil) then
                                  fPlayers.Player[fOwner].Stats.HouseDestroyed(H.GetHouseType);
                          end;
            pt_TowerRock: if (U <> nil)and(not U.IsDeadOrDying)and(U.Visible)and(not (U is TKMUnitAnimal)) then
                            if U.HitPointsDecrease(10)then //Instant death
                              if (fPlayers <> nil) then
                                fPlayers.Player[fOwner].Stats.UnitKilled(U.UnitType);
          end;
        end;
      end;
end;


//Test wherever projectile is visible (used by rocks thrown from Towers mostly)
function TKMProjectiles.ProjectileVisible(aIndex:integer):boolean;
begin
  case fItems[aIndex].fProjType of
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
  for i:=1 to length(fItems)-1 do
    if (fItems[i].fSpeed<>0) and ProjectileVisible(i) then
    begin

      MixValue := fItems[i].fPosition / fItems[i].fLength; // 0 >> 1
      case fItems[i].fProjType of
        pt_Arrow, pt_Bolt:
        begin
          MixArc := sin(MixValue*pi);   // 0 >> 1 >> 0 Parabola
          P1 := mix(fItems[i].fScreenEnd, fItems[i].fScreenStart, MixValue);
          P1.Y := P1.Y - fItems[i].fArc * MixArc;
          P2.X := P1.X+3; P2.Y := P2.Y+1;
          Dir := KMGetDirection(fItems[i].fScreenStart, fItems[i].fScreenEnd);
          fRender.RenderProjectile(fItems[i].fProjType, P1.X, P1.Y, MixValue, Dir);
        end;

        pt_TowerRock:
        begin
          MixArc := cos(MixValue*pi/2); // 1 >> 0      Half-parabola
          P1 := mix(fItems[i].fScreenEnd, fItems[i].fScreenStart, MixValue);
          P1.Y := P1.Y - fItems[i].fArc * MixArc;
          P2.X := 0; P2.Y := 0;
          fRender.RenderProjectile(fItems[i].fProjType, P1.X, P1.Y, MixValue);
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
  for i:=1 to length(fItems)-1 do
    if fItems[i].fSpeed <> 0 then inc(Count);
  SaveStream.Write(Count);

  for i:=1 to length(fItems)-1 do
    if fItems[i].fSpeed <> 0 then
      with fItems[i] do
      begin
        SaveStream.Write(fScreenStart);
        SaveStream.Write(fScreenEnd);
        SaveStream.Write(fTarget);
        SaveStream.Write(fTargetJ);
        SaveStream.Write(fProjType, SizeOf(fProjType));
        SaveStream.Write(fOwner, SizeOf(fOwner));
        SaveStream.Write(fSpeed);
        SaveStream.Write(fArc);
        SaveStream.Write(fPosition);
        SaveStream.Write(fLength);
      end;
end;


procedure TKMProjectiles.Load(LoadStream:TKMemoryStream);
var s:string; i, Count: integer;
begin
  LoadStream.Read(s);
  Assert(s = 'Projectiles', 'Projectiles not found');

  LoadStream.Read(Count);
  SetLength(fItems,Count+2); //Reserve extra space
     
  for i:=1 to Count do
    with fItems[i] do
    begin
      LoadStream.Read(fScreenStart);
      LoadStream.Read(fScreenEnd);
      LoadStream.Read(fTarget);
      LoadStream.Read(fTargetJ);
      LoadStream.Read(fProjType, SizeOf(fProjType));
      LoadStream.Read(fOwner, SizeOf(fOwner));
      LoadStream.Read(fSpeed);
      LoadStream.Read(fArc);
      LoadStream.Read(fPosition);
      LoadStream.Read(fLength);
    end;
end;


end.
