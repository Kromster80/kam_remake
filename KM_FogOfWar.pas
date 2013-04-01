unit KM_FogOfWar;
{$I KaM_Remake.inc}
interface
uses Classes, Math,
  KM_CommonClasses, KM_Points;


{ FOW state for each player }
type
  TKMFogOfWar = class
  private
    fAnimStep: Cardinal;
    MapX: Word;
    MapY: Word;
    Revelation: array of array of record
      //Lies within range 0, TERRAIN_FOG_OF_WAR_MIN..TERRAIN_FOG_OF_WAR_MAX.
      Visibility: Byte;
      {LastTerrain: Byte;
      LastHeight: Byte;
      LastTree: Byte;
      LastHouse: THouseType;}
    end;
    procedure SetMapSize(X,Y: Word);
  public
    constructor Create(X,Y: Word);
    procedure RevealCircle(Pos: TKMPoint; Radius,Amount: Word);
    procedure RevealEverything;
    function CheckVerticeRevelation(const X,Y: Word; aSkipForReplay: Boolean): Byte;
    function CheckTileRevelation(const X,Y: Word; aSkipForReplay: Boolean): Byte;
    function CheckRevelation(const aPoint: TKMPointF; aSkipForReplay: Boolean): Byte;

    procedure SyncFOW(aFOW: TKMFogOfWar);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure UpdateState;
  end;


implementation
uses KM_Defaults, KM_Game;


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
var I,K: Integer;
begin
  //We inline maths here to gain performance
  for I := max(Pos.Y-Radius, 0) to min(Pos.Y+Radius, MapY-1) do
  for K := max(Pos.X-Radius, 0) to min(Pos.X+Radius, MapX-1) do
  if (sqr(Pos.x-K) + sqr(Pos.y-I)) <= sqr(Radius) then
    Revelation[I,K].Visibility := min(Revelation[I,K].Visibility + Amount, FOG_OF_WAR_MAX);
end;


{Reveal whole map to max value}
procedure TKMFogOfWar.RevealEverything;
var I,K: Word;
begin
  for I := 0 to MapY - 1 do
    for K := 0 to MapX - 1 do
      Revelation[I, K].Visibility := FOG_OF_WAR_MAX;
end;


//Check if requested vertice is revealed for given player
//Return value of revelation is 0..255
//0 unrevealed, 255 revealed completely
//aSkipForReplay should be true in cases where replay should always return revealed (e.g. sounds, render)
//but false in cases where it will effect the gameplay (e.g. unit hit test)
function TKMFogOfWar.CheckVerticeRevelation(const X,Y: Word; aSkipForReplay: Boolean): Byte;
begin
  if aSkipForReplay then
  begin
    Result := 255;
    Exit;
  end;

  //I like how "alive" the fog looks with some tweaks
  //pulsating around units and slowly thickening when they leave :)
  if FOG_OF_WAR_ENABLE then
    if (Revelation[Y,X].Visibility >= FOG_OF_WAR_ACT) then
      Result := 255
    else
      Result := (Revelation[Y,X].Visibility shl 8) div FOG_OF_WAR_ACT
  else
    if (Revelation[Y,X].Visibility >= FOG_OF_WAR_MIN) then
      Result := 255
    else
      Result := 0;
end;


//Check if requested tile is revealed for given player
//Input values for tiles (X,Y) are in 1..N range
//Return value of revelation within 0..255 (0 unrevealed, 255 fully revealed)
//aSkipForReplay should be true in cases where replay should always return revealed (e.g. sounds, render)
//but false in cases where it will effect the gameplay (e.g. unit hit test)
function TKMFogOfWar.CheckTileRevelation(const X,Y: Word; aSkipForReplay: Boolean): Byte;
begin
  if aSkipForReplay then
  begin
    Result := 255;
    Exit;
  end;

  if (X <= 0) or (X >= MapX)
  or (Y <= 0) or (Y >= MapY) then
  begin
    Result := 0;
    Exit;
  end;

  //Check all four corners and choose max
  Result := CheckVerticeRevelation(X-1,Y-1,aSkipForReplay);
  if Result = 255 then exit;
  if X <= MapX-1 then Result := max(Result, CheckVerticeRevelation(X,Y-1,aSkipForReplay));
  if Result = 255 then exit;
  if (X <= MapX-1) and (Y <= MapY-1) then Result := max(Result, CheckVerticeRevelation(X,Y,aSkipForReplay));
  if Result = 255 then exit;
  if Y <= MapY-1 then Result := max(Result, CheckVerticeRevelation(X-1,Y,aSkipForReplay));
end;


//Check exact revelation of the point (interpolate between vertices)
function TKMFogOfWar.CheckRevelation(const aPoint: TKMPointF; aSkipForReplay: Boolean): Byte;
var A, B, C, D, Y1, Y2: Byte;
begin
  if aSkipForReplay then
  begin
    Result := 255;
    Exit;
  end;

  if (aPoint.X <= 0) or (aPoint.X >= MapX - 1)
  or (aPoint.Y <= 0) or (aPoint.Y >= MapY - 1) then
  begin
    Result := 0;
    Exit;
  end;

  //Interpolate as follows:
  //A-B
  //C-D
  A := CheckVerticeRevelation(Trunc(aPoint.X),   Trunc(aPoint.Y),   aSkipForReplay);
  B := CheckVerticeRevelation(Trunc(aPoint.X)+1, Trunc(aPoint.Y),   aSkipForReplay);
  C := CheckVerticeRevelation(Trunc(aPoint.X),   Trunc(aPoint.Y)+1, aSkipForReplay);
  D := CheckVerticeRevelation(Trunc(aPoint.X)+1, Trunc(aPoint.Y)+1, aSkipForReplay);

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
      Revelation[I, K].Visibility := Math.max(Revelation[I, K].Visibility, aFOW.Revelation[I, K].Visibility);
end;


procedure TKMFogOfWar.Save(SaveStream: TKMemoryStream);
var
  I: Word;
begin
  SaveStream.Write('FOW');
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
  if not FOG_OF_WAR_ENABLE then Exit;

  Inc(fAnimStep);

  for I := 0 to MapY - 1 do
    for K := 0 to MapX - 1 do
      if (Revelation[I, K].Visibility > FOG_OF_WAR_MIN)
      and ((I * MapX + K + fAnimStep) mod FOW_PACE = 0) then
          Dec(Revelation[I, K].Visibility, FOG_OF_WAR_DEC);
end;


end.
