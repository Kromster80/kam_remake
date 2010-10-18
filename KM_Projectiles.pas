unit KM_Projectiles;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KromUtils, Math, KM_Utils, KM_Defaults, KM_CommonTypes;


{Projectiles in-game: arrows, bolts, rocks, etc..}
//Once launched they are on their own
type
  TKMProjectiles = class
  private
    fItems:array of record
      fScreenStart:TKMPointF; //Screen-space trajectory start
      fScreenEnd:TKMPointF; //Screen-space trajectory end

      fTarget:TKMPointF; //Logical tile-coords target
      fTargetJ:TKMPointF; //Logical tile-coords target

      fProjType:TProjectileType;
      fSpeed:single; //Projectile speed may vary a little bit
      fArc:single; //Thats how high projectile will go along parabola
      fPosition:single; //Projectiles position along the route Start--End
      fLength:single; //Route length to look-up for hit
    end;

  public
    constructor Create;
    function AddItem(aStart,aEnd:TKMPointF; aProjType:TProjectileType):word;

    function ProjectileVisible(aIndex:integer):boolean;

    procedure UpdateState;
    procedure Paint;

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
  end;


implementation
uses KM_Sound, KM_Render, KM_PlayersCollection, KM_Houses, KM_Units, KM_Terrain;


{ TMissionSettings }
constructor TKMProjectiles.Create;
begin
  Inherited;
  SetLength(fItems, 2); //Reserve some space already
end;


function TKMProjectiles.AddItem(aStart,aEnd:TKMPointF; aProjType:TProjectileType):word;
var i:integer; Jitter:single;
begin
  //Find empty spot or add one
  i := 0;
  repeat
    inc(i);
    if i>=length(fItems) then setlength(fItems,i+10); //Add new
  until(fItems[i].fSpeed=0);

  //todo: Speed, Arc, Target jitter depends on projectile type
  //Route for Arrow is parabola of some kind
  //Route for TowerRock is 2nd half of that parabola

  Jitter              := GetLength(aStart.X - aEnd.X, aStart.Y - aEnd.Y) / RANGE_WATCHTOWER / 1.5;
  fItems[i].fTarget.X := aEnd.X; //Thats logical target in tile-coords
  fItems[i].fTarget.Y := aEnd.Y;
  fItems[i].fTargetJ.X := aEnd.X + RandomS(Jitter); //Thats logical target in tile-coords
  fItems[i].fTargetJ.Y := aEnd.Y + RandomS(Jitter);

  //Converting tile-coords into screen coords
  case aProjType of
    pt_Arrow, pt_Bolt: begin
                    fItems[i].fScreenStart.X := aStart.X + 0.5; //
                    fItems[i].fScreenStart.Y := aStart.Y - fTerrain.InterpolateLandHeight(aStart.X,aStart.Y)/CELL_HEIGHT_DIV - 0.5; //
                  end;
    pt_TowerRock: begin
                    fItems[i].fScreenStart.X := aStart.X - 0.25; //Recruit stands in entrance, Tower middleline is X-0.75
                    fItems[i].fScreenStart.Y := aStart.Y - fTerrain.InterpolateLandHeight(aStart.X,aStart.Y)/CELL_HEIGHT_DIV - 1.75; //Add towers height
                  end;
  end;
  fItems[i].fScreenEnd.X    := fItems[i].fTargetJ.X + 0.5;
  fItems[i].fScreenEnd.Y    := fItems[i].fTargetJ.Y + 0.5 - fTerrain.InterpolateLandHeight(aEnd.X,aEnd.Y)/CELL_HEIGHT_DIV; //projectile hits on Unit's chest height

  fItems[i].fProjType := aProjType;
  fItems[i].fSpeed    := 0.3 + randomS(0.05);
  fItems[i].fArc      := 1 + randomS(0.3);
  fItems[i].fPosition := 0; //projectile position on its route
  fItems[i].fLength   := GetLength(fItems[i].fScreenStart.X - fItems[i].fScreenEnd.X, fItems[i].fScreenStart.Y - fItems[i].fScreenEnd.Y); //route length

  Result := round(fItems[i].fLength / fItems[i].fSpeed);
end;


//Update all items positions and kill some targets
procedure TKMProjectiles.UpdateState;
var i:integer; U:TKMUnit; H:TKMHouse;
begin
  for i:=1 to length(fItems)-1 do
    if fItems[i].fSpeed <> 0 then
    begin
      fItems[i].fPosition := fItems[i].fPosition + fItems[i].fSpeed;

      if fItems[i].fPosition >= fItems[i].fLength then begin
        fItems[i].fSpeed := 0; //remove projectile

        U := fPlayers.UnitsHitTestF(fItems[i].fTargetJ);

        case fItems[i].fProjType of
          pt_Arrow,
          pt_Bolt:      begin
                          if U <> nil then
                            U.HitPointsDecrease(1)
                          else begin
                            H := fPlayers.HousesHitTest(round(fItems[i].fTargetJ.X), round(fItems[i].fTargetJ.Y));
                            if H <> nil then H.AddDamage(1);
                          end;
                        end;

          pt_TowerRock: if U <> nil then U.KillUnit;
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
begin
  for i:=1 to length(fItems)-1 do
    if (fItems[i].fSpeed<>0) and ProjectileVisible(i) then
    begin
      MixValue := 1 - fItems[i].fPosition / fItems[i].fLength;
      fRender.RenderProjectile(
                                 fItems[i].fProjType,
                                 0,
                                 mix(fItems[i].fScreenStart.X, fItems[i].fScreenEnd.X, MixValue),
                                 mix(fItems[i].fScreenStart.Y, fItems[i].fScreenEnd.Y, MixValue)
                                 );
      fRender.RenderDebugProjectile(
                                    fItems[i].fScreenStart.X,
                                    fItems[i].fScreenStart.Y,
                                    fItems[i].fScreenEnd.X,
                                    fItems[i].fScreenEnd.Y);
      fRender.RenderDebugProjectile(
                                    fItems[i].fTarget.X,
                                    fItems[i].fTarget.Y,
                                    fItems[i].fTargetJ.X,
                                    fItems[i].fTargetJ.Y);
    end;
end;


procedure TKMProjectiles.Save(SaveStream:TKMemoryStream);
//var i,Count:integer;
begin
{  SaveStream.Write('Projectiles');

  Count := 0;
  for i:=1 to length(fItems) do
    if fItems[i].fSpeed <> 0 then inc(Count);
  SaveStream.Write(Count);

  for i:=1 to length(fItems) do
    if fItems[i].fSpeed <> 0 then
    begin
      SaveStream.Write(fItems[i].fStart.X);
      SaveStream.Write(fItems[i].fStart.Y);
      SaveStream.Write(fItems[i].fStart.Z);
      SaveStream.Write(fItems[i].fEnd.X);
      SaveStream.Write(fItems[i].fEnd.Y);
      SaveStream.Write(fItems[i].fEnd.Z);

      //wip

    end;   }
end;


procedure TKMProjectiles.Load(LoadStream:TKMemoryStream);
//var i:integer;
begin
{  for i:=1 to HOUSE_COUNT do LoadStream.Read(HouseTotalCount[i]);
  for i:=1 to HOUSE_COUNT do LoadStream.Read(HouseBuiltCount[i]);
  for i:=1 to HOUSE_COUNT do LoadStream.Read(HouseLostCount[i]);
  for i:=1 to 40 do LoadStream.Read(UnitTotalCount[i]);
  for i:=1 to 40 do LoadStream.Read(UnitTrainedCount[i]);
  for i:=1 to 40 do LoadStream.Read(UnitLostCount[i]);
  for i:=1 to 4 do for k:=1 to 4 do LoadStream.Read(ResourceRatios[i,k]);
  LoadStream.Read(fMissionTimeInSec);
  for i:=1 to HOUSE_COUNT do LoadStream.Read(AllowToBuild[i]);
  for i:=1 to HOUSE_COUNT do LoadStream.Read(BuildReqDone[i]);}
end;


end.
