unit KM_TerrainPainter;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_Points,
  KM_Terrain;


type
  TTerrainPainter = class
  private
    //Fraction part of height, for smooth height editing
    HeightAdd: array [1..MAX_MAP_SIZE, 1..MAX_MAP_SIZE] of Byte;
  public
    procedure MapEdHeight;
    procedure MapEdTile(aLoc:TKMPoint; aTile,aRotation: Byte);
    procedure UpdateStateIdle;
  end;


var
  fTerrainPainter: TTerrainPainter;


implementation
uses KM_Utils;


procedure TTerrainPainter.MapEdHeight;
var
  I, K: Integer;
  Rad, Slope, Speed: Byte;
  Tmp: Single;
  R: TKMRect;
  aLoc : TKMPointF;
  aRaise: Boolean;
begin
  aLoc    := KMPointF(GameCursor.Float.X+1, GameCursor.Float.Y+1); // Mouse point
  aRaise  := ssLeft in GameCursor.SState;         // Raise or Lowered (Left or Right mousebtn)
  Rad     := GameCursor.MapEdSize;                // Radius basing on brush size
  Slope   := GameCursor.MapEdSlope;               // Elevation slope
  Speed   := GameCursor.MapEdSpeed;               // Elvation speed
  for I := Max((round(aLoc.Y) - Rad), 1) to Min((round(aLoc.Y) + Rad), fTerrain.MapY) do
  for K := Max((round(aLoc.X) - Rad), 1) to Min((round(aLoc.X) + Rad), fTerrain.MapX) do
  begin
  // We have square area basing on mouse point +/- radius
  // Now we need to check whether point is inside brush type area(circle etc.)
  // Every MapEdShape case has it's own check routine
    case GameCursor.MapEdShape of
        hsCircle:
            Tmp := Max((1 - GetLength(I - round(aLoc.Y), round(K - aLoc.X)) / Rad), 0);   // Negative number means that point is outside circle
        hsSquare:
          Tmp := 1 - Max(Abs(I - round(aLoc.Y)), Abs(K - round(aLoc.X))) / Rad;
      else
        Tmp := 0;
      end;
  // Default cursor mode is elevate/decrease
    if GameCursor.Mode = cmEqualize then
    begin // START Unequalize
      if aRaise then
      begin
        if (i > 1) and (k >1) and (i < fTerrain.MapY - 1) and (k < fTerrain.MapX - 1) then
        begin
        // Unequalize compares heights of adjacent tiles and increases differences
          if (fTerrain.Land[I,K].Height < fTerrain.Land[I-1,K+1].Height) then
            Tmp := -Min(fTerrain.Land[I-1,K+1].Height - fTerrain.Land[I,K].Height, Tmp)
          else
          if (fTerrain.Land[I,K].Height > fTerrain.Land[I-1,K+1].Height) then
            Tmp := Min(fTerrain.Land[I,K].Height - fTerrain.Land[I-1,K+1].Height, Tmp)
          else
            Tmp := 0;
        end
        else
          Tmp := 0;
       //END Unequalize
      end else
      // START Flatten
      begin
      //Flatten compares heights of mouse click and active tile then it increases/decreases height of active tile
        if (fTerrain.Land[I,K].Height < fTerrain.Land[Max(trunc(aLoc.Y), 1), Max(trunc(aLoc.X), 1)].Height) then
          Tmp := - Min(fTerrain.Land[Max(trunc(aLoc.Y), 1), Max(trunc(aLoc.X), 1)].Height - fTerrain.Land[I,K].Height, Tmp)
        else
          if (fTerrain.Land[I,K].Height > fTerrain.Land[Max(trunc(aLoc.Y), 1), Max(trunc(aLoc.X), 1)].Height) then
            Tmp := Min(fTerrain.Land[I,K].Height - fTerrain.Land[Max(trunc(aLoc.Y), 1), Max(trunc(aLoc.X), 1)].Height, Tmp)
          else
            Tmp := 0;
      end;
      //END Flatten
    end;
    //COMMON PART FOR Elevate/Lower and Unequalize/Flatten
    //Compute resulting floating-point height
    Tmp := power(abs(Tmp),(Slope+1)/6)*sign(Tmp); //Modify slopes curve
    Tmp := Tmp * (4.75/14*(Speed - 1) + 0.25);
    Tmp := EnsureRange(fTerrain.Land[I,K].Height + HeightAdd[I,K]/255 + Tmp * (Byte(aRaise)*2 - 1), 0, 100); // (Byte(aRaise)*2 - 1) - LeftButton pressed it equals 1, otherwise equals -1
    fTerrain.Land[I,K].Height := trunc(Tmp);
    HeightAdd[I,K] := round(frac(Tmp)*255); //write fractional part in 0..255 range (1Byte) to save us mem
  end;

  R := KMRectGrow(KMRect(aLoc), Rad);
  fTerrain.UpdateLighting(R);
  fTerrain.UpdatePassability(R);
end;


procedure TTerrainPainter.MapEdTile(aLoc: TKMPoint; aTile, aRotation: Byte);
begin
  if fTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then
  begin
    fTerrain.Land[aLoc.Y, aLoc.X].Terrain := aTile;
    fTerrain.Land[aLoc.Y, aLoc.X].Rotation := aRotation;
    fTerrain.UpdatePassability(aLoc);
  end;
end;


//Only MapEd accesses it
procedure TTerrainPainter.UpdateStateIdle;
begin
  case GameCursor.Mode of
    cmElevate,
    cmEqualize:  if (ssLeft in GameCursor.SState) or (ssRight in GameCursor.SState) then
                    MapEdHeight;
    cmTiles:     if (ssLeft in GameCursor.SState) then
                    if GameCursor.MapEdDir in [0..3] then //Defined direction
                      MapEdTile(GameCursor.Cell, GameCursor.Tag1, GameCursor.MapEdDir)
                    else //Random direction
                      MapEdTile(GameCursor.Cell, GameCursor.Tag1, KaMRandom(4));
  end;
end;


end.
