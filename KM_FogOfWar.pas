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
  Inherited Create;
  SetMapSize(X,Y);
end;


procedure TKMFogOfWar.SetMapSize(X,Y: Word);
begin
  MapX := X;
  MapY := Y;
  SetLength(Revelation, Y+1, X+1);
end;


{Reveal circle on map}
{Amount controls how "strong" terrain is revealed, almost instantly or slowly frame-by-frame in multiple calls}
procedure TKMFogOfWar.RevealCircle(Pos: TKMPoint; Radius, Amount: Word);
var I,K: Integer;
begin
  //We inline maths here to gain performance
  for I := max(Pos.Y-Radius, 2) to min(Pos.Y+Radius, MapY-1) do //Keep map edges unrevealed
  for K := max(Pos.X-Radius, 2) to min(Pos.X+Radius, MapX-1) do
  if (sqr(Pos.x-K) + sqr(Pos.y-I)) <= sqr(Radius) then
    Revelation[I,K].Visibility := min(Revelation[I,K].Visibility + Amount, FOG_OF_WAR_MAX);
end;


{Reveal whole map to max value}
procedure TKMFogOfWar.RevealEverything;
var I,K: Integer;
begin
  for I := 1 to MapY do
    for K := 1 to MapX do
      Revelation[I, K].Visibility := FOG_OF_WAR_MAX;
end;


{Check if requested vertice is revealed for given player}
{Return value of revelation is 0..255}
//0 unrevealed, 255 revealed completely
//aSkipForReplay should be true in cases where replay should always return revealed (e.g. sounds, render)
//but false in cases where it will effect the gameplay (e.g. unit hit test)
function TKMFogOfWar.CheckVerticeRevelation(const X,Y: Word; aSkipForReplay:boolean):byte;
begin
  if aSkipForReplay and fGame.ReplayMode then
  begin
    Result := 255;
    exit;
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


{Check if requested tile is revealed for given player}
{Return value of revelation is 0..255}
//0 unrevealed, 255 revealed completely
//aSkipForReplay should be true in cases where replay should always return revealed (e.g. sounds, render)
//but false in cases where it will effect the gameplay (e.g. unit hit test)
function TKMFogOfWar.CheckTileRevelation(const X,Y: Word; aSkipForReplay:boolean):byte;
begin
  if aSkipForReplay and fGame.ReplayMode then
  begin
    Result := 255;
    exit;
  end;
  //Check all four corners and choose max
  Result := CheckVerticeRevelation(X,Y,aSkipForReplay);
  if Result = 255 then exit;
  if X+1 <= MapX-1 then Result := max(Result, CheckVerticeRevelation(X+1,Y,aSkipForReplay));
  if Result = 255 then exit;
  if (X+1 <= MapX-1) and (Y+1 <= MapY-1) then Result := max(Result, CheckVerticeRevelation(X+1,Y+1,aSkipForReplay));
  if Result = 255 then exit;
  if Y+1 <= MapY-1 then Result := max(Result, CheckVerticeRevelation(X,Y+1,aSkipForReplay));
end;


//
function TKMFogOfWar.CheckRevelation(const aPoint: TKMPointF; aSkipForReplay: Boolean): Byte;
var A, B, C, D, Y1, Y2: Byte;
begin
  if aSkipForReplay and fGame.ReplayMode then
  begin
    Result := 255;
    Exit;
  end;

  if (aPoint.X < 0) or (aPoint.X > MapX - 1)
  or (aPoint.Y < 0) or (aPoint.Y > MapY - 1) then
  begin
    Result := 0;
    Exit;
  end;


  //Interpolate as follows:
  //A-B
  //C-D
  A := CheckVerticeRevelation(Trunc(aPoint.X), Trunc(aPoint.Y), aSkipForReplay);
  B := CheckVerticeRevelation(Trunc(aPoint.X)+1, Trunc(aPoint.Y), aSkipForReplay);
  C := CheckVerticeRevelation(Trunc(aPoint.X), Trunc(aPoint.Y)+1, aSkipForReplay);
  D := CheckVerticeRevelation(Trunc(aPoint.X)+1, Trunc(aPoint.Y)+1, aSkipForReplay);

  Y1 := Round(A + (B - A) * Frac(aPoint.X));
  Y2 := Round(C + (D - C) * Frac(aPoint.X));

  Result := Round(Y1 + (Y2 - Y1) * Frac(aPoint.Y));
end;


procedure TKMFogOfWar.SyncFOW(aFOW: TKMFogOfWar);
var I,K: Integer;
begin
  for I := 1 to MapY do
    for K := 1 to MapX do
      Revelation[I, K].Visibility := Math.max(Revelation[I, K].Visibility, aFOW.Revelation[I, K].Visibility);
end;


procedure TKMFogOfWar.Save(SaveStream: TKMemoryStream);
var
  I, K: integer;
begin
  SaveStream.Write('FOW');
  SaveStream.Write(MapX);
  SaveStream.Write(MapY);
  SaveStream.Write(fAnimStep);
  for I := 1 to MapY do
    for K := 1 to MapX do
      SaveStream.Write(Revelation[I, K], SizeOf(Revelation[I, K]));
end;


procedure TKMFogOfWar.Load(LoadStream: TKMemoryStream);
var
  I, K: integer;
begin
  LoadStream.ReadAssert('FOW');
  LoadStream.Read(MapX);
  LoadStream.Read(MapY);
  LoadStream.Read(fAnimStep);
  SetMapSize(MapX, MapY);
  for I := 1 to MapY do
    for K := 1 to MapX do
      LoadStream.Read(Revelation[I, K], SizeOf(Revelation[I, K]));
end;


procedure TKMFogOfWar.UpdateState;
var
  I, K: Word;
begin
  Inc(fAnimStep);

  if FOG_OF_WAR_ENABLE then
    for I := 1 to MapY do
      for K := 1 to MapX do
        if (I * MapX + K + fAnimStep) mod FOW_PACE = 0 then
          if Revelation[I, K].Visibility > FOG_OF_WAR_MIN then
            Dec(Revelation[I, K].Visibility, FOG_OF_WAR_DEC);
end;


end.
