unit KM_FogOfWar;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points;


{ FOW state for each player }
type
  TKMFogOfWarCommon = class
  public
    function CheckVerticeRevelation(const X,Y: Word): Byte; virtual; abstract;
    function CheckTileRevelation(const X,Y: Word): Byte; virtual; abstract;
    function CheckRevelation(const aPoint: TKMPointF): Byte; virtual; abstract;
  end;

  TKMFogOfWar = class(TKMFogOfWarCommon)
  private
    fAnimStep: Cardinal;
    MapX: Word;
    MapY: Word;

    // Used to optimize RevealCircle
    // It doesn't work if a cover function is called
    // No need to save/load it, it's just an optimisation
    RevealedRadius: array [0..MAX_MAP_SIZE-1, 0..MAX_MAP_SIZE-1] of Word;
    CoverHasBeenCalled: Boolean;

    (*Revelation: array of array of packed record
      //Lies within range 0, TERRAIN_FOG_OF_WAR_MIN..TERRAIN_FOG_OF_WAR_MAX.
      Visibility: Byte;
      {LastTerrain: Byte;
      LastHeight: Byte;
      LastObj: Byte;
      LastHouse: THouseType;}
    end;*)
    procedure SetMapSize(X,Y: Word);
  public
    Revelation: TKMByte2Array; //Public for faster access from Render
    constructor Create(X,Y: Word);
    procedure RevealCircle(Pos: TKMPoint; Radius,Amount: Word);
    procedure CoverCircle(Pos: TKMPoint; Radius: Word);
    procedure RevealRect(TL, BR: TKMPoint; Amount: Word);
    procedure CoverRect(TL, BR: TKMPoint);
    procedure RevealEverything;
    procedure CoverEverything;
    function CheckVerticeRevelation(const X,Y: Word): Byte; override;
    function CheckTileRevelation(const X,Y: Word): Byte; override;
    function CheckRevelation(const aPoint: TKMPointF): Byte; override;

    procedure SyncFOW(aFOW: TKMFogOfWar);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure UpdateState;
  end;

  //FOW that is always revealed (used by MapEd, Replays)
  TKMFogOfWarOpen = class(TKMFogOfWarCommon)
  public
    function CheckVerticeRevelation(const X,Y: Word): Byte; override;
    function CheckTileRevelation(const X,Y: Word): Byte; override;
    function CheckRevelation(const aPoint: TKMPointF): Byte; override;
  end;


const
  FOG_OF_WAR_MIN  = 80;           //Minimum value for explored but FOW terrain, MIN/ACT determines FOW darkness
  FOG_OF_WAR_ACT  = 160;          //Until this value FOW is not rendered at all
  FOG_OF_WAR_MAX  = 255;          //This is max value that FOW can be, MAX-ACT determines how long until FOW appears
  FOG_OF_WAR_INC  = 128;          //Increment for FOW
  FOG_OF_WAR_DEC  = 12;           //Decrement for FOW


implementation
uses
  Math;


{ TKMFogOfWar }
//Init with Terrain size only once on creation as terrain size never change during the game
constructor TKMFogOfWar.Create(X,Y: Word);
begin
  inherited Create;
  SetMapSize(X,Y);
end;


procedure TKMFogOfWar.SetMapSize(X,Y: Word);
begin
  MapX := X;
  MapY := Y;
  SetLength(Revelation, Y, X);
end;


{Reveal circle on map}
{Amount controls how "strong" terrain is revealed, almost instantly or slowly frame-by-frame in multiple calls}
procedure TKMFogOfWar.RevealCircle(Pos: TKMPoint; Radius, Amount: Word);
var
  I, K: Word;
  I1, I2, K1, K2: Word;
  SqrRadius: Integer;
begin
  if not CoverHasBeenCalled then
  begin
    if RevealedRadius[Pos.Y, Pos.X] >= Radius then Exit;
    RevealedRadius[Pos.Y, Pos.X] := Radius;
  end;

  //Avoid repeated computing (+2% performance)
  I1 := max(Pos.Y-Radius, 0);
  I2 := min(Pos.Y+Radius, MapY-1);
  K1 := max(Pos.X-Radius, 0);
  K2 := min(Pos.X+Radius, MapX-1);
  SqrRadius := sqr(Radius);

  //Inline maths here to gain performance
  if Amount >= FOG_OF_WAR_MAX then
  begin
    for I := I1 to I2 do for K := K1 to K2 do
    if (sqr(Pos.X - K) + sqr(Pos.Y - I)) <= SqrRadius then
      Revelation[I, K] := FOG_OF_WAR_MAX;
  end
  else
  begin
    for I := I1 to I2 do for K := K1 to K2 do
    if (sqr(Pos.X - K) + sqr(Pos.Y - I)) <= SqrRadius then
      Revelation[I, K] := min(Revelation[I, K] + Amount, FOG_OF_WAR_MAX);
  end;
end;


procedure TKMFogOfWar.CoverCircle(Pos: TKMPoint; Radius: Word);
var
  I, K: Word;
  I1, I2, K1, K2: Word;
  SqrRadius: Integer;
begin
  //Avoid repeated computing (+2% performance)
  I1 := max(Pos.Y-Radius, 0);
  I2 := min(Pos.Y+Radius, MapY-1);
  K1 := max(Pos.X-Radius, 0);
  K2 := min(Pos.X+Radius, MapX-1);
  SqrRadius := sqr(Radius);

  //Inline maths here to gain performance
  for I := I1 to I2 do for K := K1 to K2 do
  if (sqr(Pos.X - K) + sqr(Pos.Y - I)) <= SqrRadius then
    Revelation[I,K] := 0;

  CoverHasBeenCalled := True;
end;


procedure TKMFogOfWar.RevealRect(TL, BR: TKMPoint; Amount: Word);
var
  I, K: Word;
begin
  for I := TL.Y to BR.Y do for K := TL.X to BR.X do
    Revelation[I,K] := Min(Revelation[I,K] + Amount, FOG_OF_WAR_MAX);
end;


procedure TKMFogOfWar.CoverRect(TL, BR: TKMPoint);
var
  I, K: Word;
begin
  for I := TL.Y to BR.Y do for K := TL.X to BR.X do
    Revelation[I,K] := 0;

  CoverHasBeenCalled := True;
end;


{Reveal whole map to max value}
procedure TKMFogOfWar.RevealEverything;
var I,K: Word;
begin
  for I := 0 to MapY - 1 do
    for K := 0 to MapX - 1 do
      Revelation[I, K] := FOG_OF_WAR_MAX;
end;


procedure TKMFogOfWar.CoverEverything;
var I,K: Word;
begin
  for I := 0 to MapY - 1 do
    for K := 0 to MapX - 1 do
      Revelation[I, K] := 0;

  CoverHasBeenCalled := True;
end;


//Check if requested vertice is revealed for given player
//Return value of revelation is 0..255
//0 unrevealed, 255 revealed completely
//but false in cases where it will effect the gameplay (e.g. unit hit test)
function TKMFogOfWar.CheckVerticeRevelation(const X,Y: Word): Byte;
begin
  //I like how "alive" the fog looks with some tweaks
  //pulsating around units and slowly thickening when they leave :)
  if DYNAMIC_FOG_OF_WAR then
    if (Revelation[Y,X] >= FOG_OF_WAR_ACT) then
      Result := 255
    else
      Result := (Revelation[Y,X] shl 8) div FOG_OF_WAR_ACT
  else
    if (Revelation[Y,X] >= FOG_OF_WAR_MIN) then
      Result := 255
    else
      Result := 0;
end;


//Check if requested tile is revealed for given player
//Input values for tiles (X,Y) are in 1..N range
//Return value of revelation within 0..255 (0 unrevealed, 255 fully revealed)
//but false in cases where it will effect the gameplay (e.g. unit hit test)
function TKMFogOfWar.CheckTileRevelation(const X,Y: Word): Byte;
begin
  if (X <= 0) or (X >= MapX)
  or (Y <= 0) or (Y >= MapY) then
  begin
    Result := 0;
    Exit;
  end;

  //Check all four corners and choose max
  Result := CheckVerticeRevelation(X-1,Y-1);
  if Result = 255 then exit;
  if X <= MapX-1 then Result := max(Result, CheckVerticeRevelation(X,Y-1));
  if Result = 255 then exit;
  if (X <= MapX-1) and (Y <= MapY-1) then Result := max(Result, CheckVerticeRevelation(X,Y));
  if Result = 255 then exit;
  if Y <= MapY-1 then Result := max(Result, CheckVerticeRevelation(X-1,Y));
end;


//Check exact revelation of the point (interpolate between vertices)
function TKMFogOfWar.CheckRevelation(const aPoint: TKMPointF): Byte;
var A, B, C, D, Y1, Y2: Byte;
begin
  if (aPoint.X <= 0) or (aPoint.X >= MapX - 1)
  or (aPoint.Y <= 0) or (aPoint.Y >= MapY - 1) then
  begin
    Result := 0;
    Exit;
  end;

  //Interpolate as follows:
  //A-B
  //C-D
  A := CheckVerticeRevelation(Trunc(aPoint.X),   Trunc(aPoint.Y)   );
  B := CheckVerticeRevelation(Trunc(aPoint.X)+1, Trunc(aPoint.Y)   );
  C := CheckVerticeRevelation(Trunc(aPoint.X),   Trunc(aPoint.Y)+1 );
  D := CheckVerticeRevelation(Trunc(aPoint.X)+1, Trunc(aPoint.Y)+1 );

  Y1 := Round(A + (B - A) * Frac(aPoint.X));
  Y2 := Round(C + (D - C) * Frac(aPoint.X));

  Result := Round(Y1 + (Y2 - Y1) * Frac(aPoint.Y));
end;


//Synchronize FOW revelation between players
procedure TKMFogOfWar.SyncFOW(aFOW: TKMFogOfWar);
var I,K: Word;
begin
  for I := 0 to MapY - 1 do
    for K := 0 to MapX - 1 do
      Revelation[I, K] := Math.max(Revelation[I, K], aFOW.Revelation[I, K]);
end;


procedure TKMFogOfWar.Save(SaveStream: TKMemoryStream);
var
  I: Word;
begin
  SaveStream.WriteA('FOW');
  SaveStream.Write(fAnimStep);
  //Because each player has FOW it can become a bottleneck (8.7ms per run) due to autosaving (e.g. on Paradise Island)
  //so save it out 1 row at a time (due to 2D arrays not being continguous we can't save it all at once)
  for I := 0 to MapY - 1 do
    SaveStream.Write(Revelation[I, 0], MapX * SizeOf(Revelation[I, 0]));
end;


procedure TKMFogOfWar.Load(LoadStream: TKMemoryStream);
var
  I: Word;
begin
  LoadStream.ReadAssert('FOW');
  LoadStream.Read(fAnimStep);
  SetMapSize(MapX, MapY);
  for I := 0 to MapY - 1 do
    LoadStream.Read(Revelation[I, 0], MapX * SizeOf(Revelation[I, 0]));
end;


//Decrease FOW revelation as time goes
procedure TKMFogOfWar.UpdateState;
var
  I, K: Word;
begin
  if not DYNAMIC_FOG_OF_WAR then Exit;

  Inc(fAnimStep);

  for I := 0 to MapY - 1 do
  for K := 0 to MapX - 1 do
  if (Revelation[I, K] > FOG_OF_WAR_MIN)
  and ((I * MapX + K + fAnimStep) mod FOW_PACE = 0) then
  begin
    Dec(Revelation[I, K], FOG_OF_WAR_DEC);

    {//Remember waht we have seen last
    if Revelation[I, K].Visibility <= FOG_OF_WAR_MIN then
    begin
      Revelation[I, K].LastTerrain := gTerrain.Land[I, K].Terrain;

    end;}
  end;
end;


{ TKMFogOfWarOpen }
function TKMFogOfWarOpen.CheckRevelation(const aPoint: TKMPointF): Byte;
begin
  Result := 255;
end;


function TKMFogOfWarOpen.CheckTileRevelation(const X, Y: Word): Byte;
begin
  Result := 255;
end;


function TKMFogOfWarOpen.CheckVerticeRevelation(const X, Y: Word): Byte;
begin
  Result := 255;
end;


end.
