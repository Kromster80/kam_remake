unit KM_Projectiles;
interface
uses Classes, SysUtils, KromUtils, Math, KM_Utils, KM_Defaults, KM_CommonTypes;


{Projectiles in-game: arrows, bolts, rocks, etc..}
//Once launched they are on their own
type
  TKMProjectiles = class
  private
    fItems:array of record
      fStart:TKMPointF; //Screen-space trajectory start
      fEnd:TKMPointF; //Screen-space trajectory end
      fTarget:TKMPointF; //Logical tile-coords target
      fProjType:TProjectileType;
      fSpeed:single; //Projectile speed may vary a little bit
      fArc:single; //Thats how high projectile will go along parabola
      fPosition:single; //Projectiles position along the route Start--End
      fLength:single; //Route length to look-up for hit
    end;

  public
    constructor Create;
    procedure AddItem(aStart,aEnd:TKMPointF; aProjType:TProjectileType);

    procedure UpdateState;
    procedure Paint;

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
  end;


implementation
uses KM_Sound, KM_Game, KM_Render, KM_PlayersCollection, KM_Units, KM_Terrain;


{ TMissionSettings }
constructor TKMProjectiles.Create;
begin
  Inherited;
  SetLength(fItems, 2); //Reserve some space already
end;


procedure TKMProjectiles.AddItem(aStart,aEnd:TKMPointF; aProjType:TProjectileType);
var i:integer;
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


  //Converting tile-coords into screen coords
  case aProjType of
    pt_Arrow, pt_Bolt: begin
                    fItems[i].fStart.X := aStart.X; //Recruit stands in entrance, Tower middleline is X-0.75
                    fItems[i].fStart.Y := aStart.Y - fTerrain.InterpolateLandHeight(aStart.X,aStart.Y)/CELL_HEIGHT_DIV - 0.5; //Recruit stands in entrance, Tower middleline is X-0.5
                  end;
    pt_TowerRock: begin
                    fItems[i].fStart.X := aStart.X - 0.75; //Recruit stands in entrance, Tower middleline is X-0.75
                    fItems[i].fStart.Y := aStart.Y - fTerrain.InterpolateLandHeight(aStart.X,aStart.Y)/CELL_HEIGHT_DIV - 1.75; //Recruit stands in entrance, Tower middleline is X-0.5
                  end;
  end;
  fItems[i].fEnd.X      := aEnd.X;
  fItems[i].fEnd.Y      := aEnd.Y + 0.5; //projectile hits on Unit's chest height

  fItems[i].fTarget   := aEnd; //Thats logical target in tile-coords

  fItems[i].fProjType := aProjType;
  fItems[i].fSpeed    := 0.3 + randomS(0.05);
  fItems[i].fArc      := 1 + randomS(0.3);
  fItems[i].fPosition := 0; //projectile position on its route
  fItems[i].fLength   := GetLength(fItems[i].fStart.X - fItems[i].fEnd.X, fItems[i].fStart.Y - fItems[i].fEnd.Y); //route length
end;


//Update all items positions and kill some targets
procedure TKMProjectiles.UpdateState;
var i:integer; U:TKMUnit;
begin
  for i:=1 to length(fItems)-1 do
    if fItems[i].fSpeed <> 0 then
    begin
      fItems[i].fPosition := fItems[i].fPosition + fItems[i].fSpeed;

      if fItems[i].fPosition >= fItems[i].fLength then begin
        fItems[i].fSpeed := 0; //remove projectile
        U := fPlayers.UnitsHitTest(round(fItems[i].fEnd.X), round(fItems[i].fEnd.Y));
        if U <> nil then
          U.KillUnit;
      end;
    end;
end;


procedure TKMProjectiles.Paint;
var
  i:integer;
  MixValue:single;
begin
  for i:=1 to length(fItems)-1 do
    if fItems[i].fSpeed <> 0 then
    begin
      MixValue := 1 - fItems[i].fPosition / fItems[i].fLength;
      fRender.RenderProjectile(
                                 fItems[i].fProjType,
                                 0,
                                 mix(fItems[i].fStart.X, fItems[i].fEnd.X, MixValue) + 0.5,
                                 mix(fItems[i].fStart.Y, fItems[i].fEnd.Y, MixValue) + 0.5
                                 );
      fRender.RenderDebugLine(
                                 fItems[i].fStart.X + 0.5,
                                 fItems[i].fStart.Y + 0.5,
                                 fItems[i].fEnd.X + 0.5,
                                 fItems[i].fEnd.Y + 0.5);
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
