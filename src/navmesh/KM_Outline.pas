unit KM_Outline;
{$I KaM_Remake.inc}
interface
uses
  KM_PolySimplify,
  KM_CommonTypes;


  //Procedure that converts 2D Byte array into outlines
  //aMap:   Values are 0/1,
  //        Note that procedure stores internal data in same array as 255
  //aTrim:  Skip area smaller than this size in tiles (1 is buggy and misfires within obstacles)
  procedure GenerateOutline(aMap: TKMByte2Array; aTrim: Byte; out aOutlines: TKMShapesArray);


implementation
uses
  Math,
  KM_Points;


procedure GenerateOutline(aMap: TKMByte2Array; aTrim: Byte; out aOutlines: TKMShapesArray);
type
  TStepDirection = (sdNone, sdUp, sdRight, sdDown, sdLeft);
var
  NextStep: TStepDirection;
  SizeX, SizeY: Word;

  procedure Step(X,Y: SmallInt);
    function IsTilePassable(aX, aY: SmallInt): Boolean;
    begin
      Result := InRange(aY, 0, SizeY-1)
            and InRange(aX, 0, SizeX-1)
            and (aMap[aY,aX] > 0);
      //Mark tiles we've been on, so they do not trigger new duplicate contour
      if Result then
        aMap[aY,aX] := 255; //Mark unpassable but visited
    end;
  var
    PrevStep: TStepDirection;
    State: Byte;
  begin
    //Remember previous step for convenience of use
    PrevStep := NextStep;

    //Assemble bitmask
    State :=  Byte(IsTilePassable(X  ,Y  )) +
              Byte(IsTilePassable(X+1,Y  )) * 2 +
              Byte(IsTilePassable(X  ,Y+1)) * 4 +
              Byte(IsTilePassable(X+1,Y+1)) * 8;

    //Where do we go from here
    case State of
      1:  NextStep := sdUp;
      2:  NextStep := sdRight;
      3:  NextStep := sdRight;
      4:  NextStep := sdLeft;
      5:  NextStep := sdUp;
      6:  if (PrevStep = sdUp) then
            NextStep := sdLeft
          else
            NextStep := sdRight;
      7:  NextStep := sdRight;
      8:  NextStep := sdDown;
      9:  if (PrevStep = sdRight) then
            NextStep := sdUp
          else
            NextStep := sdDown;
      10: NextStep := sdDown;
      11: NextStep := sdDown;
      12: NextStep := sdLeft;
      13: NextStep := sdUp;
      14: NextStep := sdLeft;
      else NextStep := sdNone;
    end;
  end;

  procedure WalkPerimeter(aStartX, aStartY: SmallInt);
  var
    X, Y: Integer;
  begin
    X := aStartX;
    Y := aStartY;
    NextStep := sdNone;

    SetLength(aOutlines.Shape, aOutlines.Count + 1);
    aOutlines.Shape[aOutlines.Count].Count := 0;

    repeat
      Step(X, Y);

      case NextStep of
        sdUp:     Dec(Y);
        sdRight:  Inc(X);
        sdDown:   Inc(Y);
        sdLeft:   Dec(X);
        else
      end;

      //Append new node vertice
      with aOutlines.Shape[aOutlines.Count] do
      begin
        if Length(Nodes) <= Count then
          SetLength(Nodes, Count + 32);
        Nodes[Count] := KMPoint(X, Y);
        Inc(Count);
      end;
    until((X = aStartX) and (Y = aStartY));

    //Do not include too small regions
    if aOutlines.Shape[aOutlines.Count].Count >= aTrim then
      Inc(aOutlines.Count);
  end;

var
  I, K: Integer;
  C1, C2, C3, C4: Boolean;
begin
  SizeX := Length(aMap[0]);
  SizeY := Length(aMap);

  aOutlines.Count := 0;
  for I := 0 to SizeY - 2 do
  for K := 0 to SizeX - 2 do
  begin
    //Find new seed among unparsed obstacles
    //C1-C2
    //C3-C4
    C1 := (aMap[I,K] = 1);
    C2 := (aMap[I,K+1] = 1);
    C3 := (aMap[I+1,K] = 1);
    C4 := (aMap[I+1,K+1] = 1);

    //Maybe skip cases where C1..C4 are all having value of 1-2
    //but I'm not sure this is going to get us any improvements
    if (C1 or C2 or C3 or C4) <> (C1 and C2 and C3 and C4) then
      WalkPerimeter(K,I);
  end;
end;


end.
