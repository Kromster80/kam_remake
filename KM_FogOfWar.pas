unit KM_FogOfWar;
{$I KaM_Remake.inc}
interface
uses Classes, Math,
  KM_CommonClasses, KM_Points;


{ FOW state for each player }
type
  TKMFogOfWar = class
  private
    fAnimStep:cardinal;
    MapX: word;
    MapY: word;
    Revelation:array of array of record
      //Lies within range 0, TERRAIN_FOG_OF_WAR_MIN..TERRAIN_FOG_OF_WAR_MAX.
      Visibility:byte;
      {LastTerrain:byte;
      LastHeight:byte;
      LastTree:byte;
      LastHouse:THouseType;}
    end;
  public
    constructor Create(X,Y:integer);
    procedure SetMapSize(X,Y:integer);
    procedure RevealCircle(Pos:TKMPoint; Radius,Amount:word);
    procedure RevealEverything;
    function CheckVerticeRevelation(const X,Y: Word; aSkipForReplay:boolean):byte;
    function CheckTileRevelation(const X,Y: Word; aSkipForReplay:boolean):byte;
    procedure SyncFOW(aFOW: TKMFogOfWar);

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);

    procedure UpdateState;
  end;


implementation
uses KM_Defaults, KM_Game;


{ TKMFogOfWar }
constructor TKMFogOfWar.Create(X,Y:integer);
begin
  Inherited Create;
  SetMapSize(X,Y);
end;


procedure TKMFogOfWar.SetMapSize(X,Y:integer);
begin
  MapX := X;
  MapY := Y;
  SetLength(Revelation, Y+1, X+1);
end;


{Reveal circle on map}
{Amount controls how "strong" terrain is revealed, almost instantly or slowly frame-by-frame in multiple calls}
procedure TKMFogOfWar.RevealCircle(Pos:TKMPoint; Radius,Amount:word);
var i,k:integer;
begin
  //We inline maths here to gain performance
  for i:=max(Pos.Y-Radius,2) to min(Pos.Y+Radius,MapY-1) do //Keep map edges unrevealed
  for k:=max(Pos.X-Radius,2) to min(Pos.X+Radius,MapX-1) do
  if (sqr(Pos.x-k) + sqr(Pos.y-i)) <= sqr(Radius) then
    Revelation[i,k].Visibility := min(Revelation[i,k].Visibility + Amount, FOG_OF_WAR_MAX);
end;


{Reveal whole map to max value}
procedure TKMFogOfWar.RevealEverything;
var i,k:integer;
begin
  for i:=1 to MapY do
    for k:=1 to MapX do
      Revelation[i,k].Visibility := FOG_OF_WAR_MAX;
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
  //I like how "alive" fog looks with some tweaks
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


procedure TKMFogOfWar.SyncFOW(aFOW: TKMFogOfWar);
var i,k:integer;
begin
  for i:=1 to MapY do
    for k:=1 to MapX do
      Revelation[i,k].Visibility := Math.Max(Revelation[i,k].Visibility, aFOW.Revelation[i,k].Visibility);
end;


procedure TKMFogOfWar.Save(SaveStream: TKMemoryStream);
var i,k:integer;
begin
  SaveStream.Write('FOW');
  SaveStream.Write(MapX);
  SaveStream.Write(MapY);
  SaveStream.Write(fAnimStep);
  for i:=1 to MapY do
  for k:=1 to MapX do
    SaveStream.Write(Revelation[i,k], SizeOf(Revelation[i,k]));
end;


procedure TKMFogOfWar.Load(LoadStream: TKMemoryStream);
var i,k:integer;
begin
  LoadStream.ReadAssert('FOW');
  LoadStream.Read(MapX);
  LoadStream.Read(MapY);
  LoadStream.Read(fAnimStep);
  SetMapSize(MapX, MapY);
  for i:=1 to MapY do
  for k:=1 to MapX do
    LoadStream.Read(Revelation[i,k], SizeOf(Revelation[i,k]));
end;


procedure TKMFogOfWar.UpdateState;
var i,k:word;
begin
  inc(fAnimStep);

  if FOG_OF_WAR_ENABLE then
  for i:=1 to MapY do
  for k:=1 to MapX do
  if (i*MapX+k+fAnimStep) mod TERRAIN_PACE = 0 then //All those global things can be performed once a sec, or even less frequent
    if Revelation[i,k].Visibility > FOG_OF_WAR_MIN then dec(Revelation[i,k].Visibility, FOG_OF_WAR_DEC);
end;


end.
