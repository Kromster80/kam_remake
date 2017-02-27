unit KM_TerrainWalkConnect;
{$I KaM_Remake.inc}
interface
uses
  KM_Terrain, KM_Points, KM_Defaults;

type
  //We use class procedues/functions here because we don't need to store anything,
  //it's just a nice way to group the procedures together.
  TKMTerrainWalkConnect = class
  private
    //Two different methods for doing full map walk connect update
    class procedure FloodFill(aWC: TWalkConnect; aPass: TKMTerrainPassability; aAllowDiag: Boolean);
    class procedure CCLFind(aWC: TWalkConnect; aPass: TKMTerrainPassability; aAllowDiag: Boolean);

    //Check whether passability was unchanged, if so we can completely skip the update
    class function CheckCanSkip(aWorkRect: TKMRect; aWC: TWalkConnect; aPass: TKMTerrainPassability; aDiagObjectsEffected: Boolean): Boolean;

    //Helpful functions used to determine when it's ok to use LocalUpdate instead of slower GlobalUpdate
    class function ExactlyOneAreaIDInRect_Current(aRect: TKMRect; aWC: TWalkConnect): Boolean;
    class function ExactlyOneAreaIDInRect_New(aRect: TKMRect; aPass: TKMTerrainPassability; aAllowDiag: Boolean): Boolean;

    //GlobalUpdate rebuilds the entire map
    class procedure GlobalUpdate(aWC: TWalkConnect; aPass: TKMTerrainPassability; aAllowDiag: Boolean);
    //LocalUpdate just updates changes in aRect for much better performance, used under special conditions
    class procedure LocalUpdate(aRect: TKMRect; aWC: TWalkConnect; aPass: TKMTerrainPassability; aAllowDiag: Boolean);
  public
    class procedure DoUpdate(aAreaAffected: TKMRect; aWC:TWalkConnect; aPass: TKMTerrainPassability; aAllowDiag: Boolean; aDiagObjectsEffected: Boolean);
  end;

implementation
uses
  Math, KM_ResMapElements;

{ TKMTerrainWalkConnect }
class procedure TKMTerrainWalkConnect.DoUpdate(aAreaAffected:TKMRect; aWC:TWalkConnect; aPass:TKMTerrainPassability; aAllowDiag: Boolean; aDiagObjectsEffected: Boolean);
var LocalArea: TKMRect;
begin
  //If passability is unchanged we can completely skip the update
  if CheckCanSkip(aAreaAffected, aWC, aPass, aDiagObjectsEffected) then
    Exit;

  LocalArea := KMRectGrow(aAreaAffected, 1);
  LocalArea := KMClipRect(LocalArea, 1, 1, gTerrain.MapX-1, gTerrain.MapY-1);

  //If the Rect area grown by 1 contains exactly one unique WalkConnect ID both:
  // - Before the the update (current values in Land.WalkConnect leftover from last update)
  // - After the update (recalculate a local flood fill in this Rect only)
  //Then logically there is NO possible way that this change affected ANY other part of
  //the map (by blocking or allowing access to some other area), so we can just update the
  //local area based on current passabilities (doing global update is a waste of time).
  //NOTE: This logic works because we grew aAreaAffected by 1.

  //Here are some examples: Lets say 1 tile was changed, so we get a 3x3 area for LocalArea.

  //   EXAMPLE 1
  //Say current Land.WalkConnect (out of date) shows this for our area:
  //  101
  //  111
  //  101
  //So ExactlyOneAreaIDInRect_Current returns True (there is exactly 1 area).
  //ExactlyOneAreaIDInRect_New does a local floodfill just on LocalArea, and we get this result:
  //  102
  //  102
  //  102
  //So we can see now that the affected tile (in the middle) became unwalkable.
  //This means ExactlyOneAreaIDInRect_New returns false (there are exactly 2 areas, not 1)
  //and we do a global update.

  //   EXAMPLE 2
  //Say current Land.WalkConnect (out of date) shows this for our area:
  //  102
  //  102
  //  102
  //And the new walk connect should look like this: (middle tile became walkable)
  //  101
  //  111
  //  101
  //In this case ExactlyOneAreaIDInRect_Current returns False (there are exactly 2 areas, not 1)
  //and we do a global update.

  //   EXAMPLE 3
  //Say current Land.WalkConnect (out of date) shows this for our area:
  //  111
  //  101
  //  101
  //And the new walk connect should look like this: (middle tile became walkable)
  //  111
  //  111
  //  101
  //In this case ExactlyOneAreaIDInRect_Current returns True (there is exactly 1 area)
  //and ExactlyOneAreaIDInRect_New returns True (there is exactly 1 area there too)
  //This means we do a local update, which updates Land.WalkConnect within this area

  if (KMRectArea(LocalArea) < 64) //If the area is large the test takes too long, better to just do global update
  and ExactlyOneAreaIDInRect_Current(LocalArea, aWC)
  and ExactlyOneAreaIDInRect_New(LocalArea, aPass, aAllowDiag) then
    LocalUpdate(LocalArea, aWC, aPass, aAllowDiag)
  else
    GlobalUpdate(aWC, aPass, aAllowDiag);
end;


class function TKMTerrainWalkConnect.CheckCanSkip(aWorkRect:TKMRect; aWC:TWalkConnect; aPass:TKMTerrainPassability; aDiagObjectsEffected: Boolean):Boolean;
var I,K: Integer; AllPass, AllFail: Boolean;
begin
  //If objects were effected we must reprocess because a tree could block the connection
  //between two areas. Also skip this check if the area is too large because it takes too long
  if (KMRectArea(aWorkRect) > 100) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  AllPass := True;
  AllFail := True;
  with gTerrain do
  for I := aWorkRect.Top to aWorkRect.Bottom do
    for K := aWorkRect.Left to aWorkRect.Right do
    begin
      if aDiagObjectsEffected then
      begin
        AllPass := AllPass and ((Land[I,K].WalkConnect[aWC] <> 0) and (aPass in Land[I,K].Passability));
        AllFail := AllFail and ((Land[I,K].WalkConnect[aWC] = 0) and not (aPass in Land[I,K].Passability));
        //If all tiles that changed are walkable or not walkable currently and in our last UpdateWalkConnect, it's safe to skip
        Result := AllPass or AllFail;
      end else begin
        Result := Result and
                  //First case: Last time we did WalkConnect the tile WASN'T walkable,
                  //and Passability confirms this has not changed (tile still not walkable)
                 (((Land[I,K].WalkConnect[aWC] = 0) and not (aPass in Land[I,K].Passability)) or
                  //Second case: Last time we did WalkConnect the tile WAS walkable,
                  //and Passability confirms this has not changed (tile still walkable)
                  ((Land[I,K].WalkConnect[aWC] <> 0) and (aPass in Land[I,K].Passability)));
      end;
      if not Result then Exit; //If one tile has changed, we need to do the whole thing
    end;
end;


//Returns true if there is exactly one walkable area within Rect in Land.WalkConnect (from last time we updated WalkConnect)
class function TKMTerrainWalkConnect.ExactlyOneAreaIDInRect_Current(aRect:TKMRect; aWC: TWalkConnect): Boolean;
var
  AreaID: Byte;
  X, Y: Word;
begin
  with gTerrain do
  begin
    //First find out the AreaID for this rect area BEFORE the change (must only be one!)
    AreaID := 0;
    for X := aRect.Left to aRect.Right do
      for Y := aRect.Top to aRect.Bottom do
        if (Land[Y,X].WalkConnect[aWC] <> 0) and (Land[Y,X].WalkConnect[aWC] <> AreaID) then
        begin
          //If we already found a different AreaID then there's more than one, so we can exit immediately
          if (AreaID <> 0) then
          begin
            Result := False;
            Exit;
          end;
          AreaID := Land[Y,X].WalkConnect[aWC];
        end;
    Result := (AreaID <> 0); //If we haven't exited yet and AreaID <> 0 then there's exactly one
  end;
end;


//Do a local floodfill and check that there's exactly one area that matches passability
class function TKMTerrainWalkConnect.ExactlyOneAreaIDInRect_New(aRect:TKMRect; aPass: TKMTerrainPassability; aAllowDiag: Boolean): Boolean;
var
  LocalWalkConnect: array of array of Boolean; //We can use Boolean instead of byte since we're only looking for one area

  procedure LocalFillArea(X,Y: Word);
  begin
    with gTerrain do
      if KMInRect(KMPoint(X,Y), aRect) //Within rectangle
      and (not LocalWalkConnect[Y - aRect.Top, X - aRect.Left]) //Untested area
      and (aPass in Land[Y,X].Passability) then //Matches passability
      begin
        LocalWalkConnect[Y - aRect.Top, X - aRect.Left] := True;
        //Using custom TileInMapCoords replacement gives ~40% speed improvement
        //Using custom CanWalkDiagonally is also much faster
        if X-1 >= 1 then
        begin
          if aAllowDiag and (Y-1 >= 1) and not gMapElements[Land[Y,X].Obj].DiagonalBlocked then
            LocalFillArea(X-1, Y-1);
          LocalFillArea(X-1, Y);
          if aAllowDiag and (Y+1 <= MapY) and not gMapElements[Land[Y+1,X].Obj].DiagonalBlocked then
            LocalFillArea(X-1,Y+1);
        end;

        if Y-1 >= 1 then    LocalFillArea(X, Y-1);
        if Y+1 <= MapY then LocalFillArea(X, Y+1);

        if X+1 <= MapX then
        begin
          if aAllowDiag and (Y-1 >= 1) and not gMapElements[Land[Y,X+1].Obj].DiagonalBlocked then
            LocalFillArea(X+1, Y-1);
          LocalFillArea(X+1, Y);
          if aAllowDiag and (Y+1 <= MapY) and not gMapElements[Land[Y+1,X+1].Obj].DiagonalBlocked then
            LocalFillArea(X+1, Y+1);
        end;
      end;
  end;

var
  X,Y: Word;
  FoundAnArea: Boolean;
begin
  SetLength(LocalWalkConnect, (aRect.Bottom-aRect.Top)+1, (aRect.Right-aRect.Left)+1);

  FoundAnArea := False;
  for X := aRect.Left to aRect.Right do
    for Y := aRect.Top to aRect.Bottom do
      if not LocalWalkConnect[Y - aRect.Top, X - aRect.Left] //Untested area
      and (aPass in gTerrain.Land[Y,X].Passability) then //Passability matches
      begin
        if FoundAnArea then
        begin
          Result := False; //We've found two walkable areas
          Exit;
        end
        else
        begin
          LocalFillArea(X,Y); //Floodfill this area
          FoundAnArea := True; //We have now found an area
        end;
      end;

  Result := FoundAnArea; //If we haven't exited yet and we found an area then there's exactly one
end;


class procedure TKMTerrainWalkConnect.FloodFill(aWC: TWalkConnect; aPass: TKMTerrainPassability; aAllowDiag: Boolean);
var
  AreaID: Byte;
  Count: Integer;

  procedure FillArea(X,Y: Word);
  begin
    with gTerrain do
      if (Land[Y,X].WalkConnect[aWC] = 0) //Untested area
      and (aPass in Land[Y,X].Passability) then //Matches passability
      begin
        Land[Y,X].WalkConnect[aWC] := AreaID;
        Inc(Count);
        //Using custom TileInMapCoords replacement gives ~40% speed improvement
        //Using custom CanWalkDiagonally is also much faster
        if X-1 >= 1 then
        begin
          if aAllowDiag and (Y-1 >= 1) and not gMapElements[Land[Y,X].Obj].DiagonalBlocked then
            FillArea(X-1, Y-1);
          FillArea(X-1, Y);
          if aAllowDiag and (Y+1 <= MapY) and not gMapElements[Land[Y+1,X].Obj].DiagonalBlocked then
            FillArea(X-1,Y+1);
        end;

        if Y-1 >= 1 then    FillArea(X, Y-1);
        if Y+1 <= MapY then FillArea(X, Y+1);

        if X+1 <= MapX then
        begin
          if aAllowDiag and (Y-1 >= 1) and not gMapElements[Land[Y,X+1].Obj].DiagonalBlocked then
            FillArea(X+1, Y-1);
          FillArea(X+1, Y);
          if aAllowDiag and (Y+1 <= MapY) and not gMapElements[Land[Y+1,X+1].Obj].DiagonalBlocked then
            FillArea(X+1, Y+1);
        end;
      end;
  end;
//const MinSize = 1; //Minimum size that is treated as new area
var I,K: Integer;
begin
  with gTerrain do
  begin
    //Reset everything
    for I := 1 to MapY do for K := 1 to MapX do
      Land[I,K].WalkConnect[aWC] := 0;

    AreaID := 0;
    for I := 1 to MapY do for K := 1 to MapX do
    if (Land[I,K].WalkConnect[aWC] = 0)
    and (aPass in Land[I,K].Passability) then
    begin
      Inc(AreaID);
      Count := 0;
      FillArea(K,I);

      if Count <= 1 then //Revert
      begin
        Dec(AreaID);
        Count := 0;
        Land[I,K].WalkConnect[aWC] := 0;
      end;

      Assert(AreaID < 255, 'UpdateWalkConnect failed due too many unconnected areas');
    end;
  end;
end;


class procedure TKMTerrainWalkConnect.CCLFind(aWC: TWalkConnect; aPass: TKMTerrainPassability; aAllowDiag: Boolean);
var
  Parent: array [0..512] of Word;

  function TopParent(const Area: Word): Word;
  begin
    Result := Area;
    while Parent[Result] <> Result do
      Result := Parent[Result];
  end;

  procedure AddAlias(const Area1, Area2: Word);
  begin
    //See if there are common parents
    if Area2 <> Area1 then
      Parent[Area2] := Area1;
  end;
const Samples: array [0..3, 0..1] of ShortInt = ((-1,-1),(0,-1),(1,-1),(-1,0));
var
  I,K,H: Word;
  X,Y: Smallint;
  AreaID: Word;
  NCount: Byte;
begin
  with gTerrain do
  begin
    //Reset everything
    for I := 1 to MapY do for K := 1 to MapX do
      Land[I,K].WalkConnect[aWC] := 0;

    FillChar(Parent, SizeOf(Parent), #0);

    AreaID := 1;
    for I := 1 to MapY do
    for K := 1 to MapX do
    if (aPass in Land[I,K].Passability) then
    begin

      //Check 4 preceeding neighbors, if there is ID we will take it
      NCount := 0;
      for H := 0 to 3 do
      begin
        X := K + Samples[H,0];
        Y := I + Samples[H,1];

        if (Y >= 1) and InRange(X, 1, MapX) and (aPass in Land[Y,X].Passability) then
        if (H = 1) or (H = 3) or (aAllowDiag and (
                                   ((H = 0) and not gMapElements[Land[I,K].Obj].DiagonalBlocked) or
                                   ((H = 2) and not gMapElements[Land[I,K+1].Obj].DiagonalBlocked)))
        then
        begin
          if (NCount = 0) then
            Land[I,K].WalkConnect[aWC] := Land[Y,X].WalkConnect[aWC]
          else
            //Remember alias
            if (Parent[Land[Y,X].WalkConnect[aWC]] <> Parent[Land[I,K].WalkConnect[aWC]]) then
              AddAlias(TopParent(Land[Y,X].WalkConnect[aWC]), TopParent(Land[I,K].WalkConnect[aWC]));

          Inc(NCount);
        end;
      end;

      //If there's no Area we create new one
      if NCount = 0 then
      begin
        Land[I,K].WalkConnect[aWC] := AreaID;
        Parent[AreaID] := AreaID;
        Inc(AreaID);
        Assert(AreaID < 32767, 'UpdateWalkConnect failed due too many unconnected areas');
      end;
    end;

    //1 -> 2    1 -> 2
    //2 -> 2    2 -> 2
    //3 -> 4    3 -> 5
    //4 -> 5    4 -> 5
    //5 -> 5    5 -> 5
    //Merge parents
    for I := 1 to AreaID - 1 do
      while Parent[I] <> Parent[Parent[I]] do
        Parent[I] := Parent[Parent[I]];

    //Merge areas
    for I := 1 to MapY do
    for K := 1 to MapX do
    if (Land[I,K].WalkConnect[aWC] <> 0) then
      Land[I,K].WalkConnect[aWC] := Parent[Land[I,K].WalkConnect[aWC]];
  end;
end;


class procedure TKMTerrainWalkConnect.GlobalUpdate(aWC: TWalkConnect; aPass: TKMTerrainPassability; aAllowDiag: Boolean);
begin
  if USE_CCL_WALKCONNECT then
    CCLFind(aWC, aPass, aAllowDiag)
  else
    FloodFill(aWC, aPass, aAllowDiag);
end;


//LocalUpdate just updates changes in aRect for much better performance, used under special conditions:
//The Rect area must contain exactly one unique WalkConnect ID, before AND after the change
//(must be checked before running this procedure)
//See comments in TKMTerrainWalkConnect.DoUpdate for a full explanation of the logic.
class procedure TKMTerrainWalkConnect.LocalUpdate(aRect:TKMRect; aWC: TWalkConnect; aPass: TKMTerrainPassability; aAllowDiag: Boolean);
var
  AreaID: Byte;
  X, Y: Word;
begin
  with gTerrain do
  begin
    //First find out the AreaID for this rect area BEFORE the change (must only be one!)
    AreaID := 0;
    for X := aRect.Left to aRect.Right do
      for Y := aRect.Top to aRect.Bottom do
        if (Land[Y,X].WalkConnect[aWC] <> 0) and (Land[Y,X].WalkConnect[aWC] <> AreaID) then
        begin
          Assert(AreaID = 0, 'Must not do local walk connect update with multiple AreaIDs in Rect');
          AreaID := Land[Y,X].WalkConnect[aWC];
        end;
    Assert(AreaID <> 0, 'Must not do local walk connect update with zero AreaIDs in Rect');

    //Now update WalkConnect based on passability, setting it to either AreaID or 0
    for X := aRect.Left to aRect.Right do
      for Y := aRect.Top to aRect.Bottom do
        if aPass in Land[Y,X].Passability then
          Land[Y,X].WalkConnect[aWC] := AreaID //Walkable
        else
          Land[Y,X].WalkConnect[aWC] := 0; //Unwalkable
  end;
end;


end.
