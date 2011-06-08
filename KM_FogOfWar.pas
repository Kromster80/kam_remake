unit KM_FogOfWar;
{$I KaM_Remake.inc}
interface
uses Classes, Math, KM_Defaults, KM_CommonTypes;

{These are mission specific settings and stats for each player}
type
  TKMFogOfWar = class
  private
    fAnimStep:cardinal;
    MapX:byte;
    MapY:byte;
    Revelation:array of array of record
      //Lies within range 0, TERRAIN_FOG_OF_WAR_MIN..TERRAIN_FOG_OF_WAR_MAX.
      Visibility:byte;
      {LastTerrain:byte;
      LastHeight:byte;
      LastTree:byte;
      LastHouse:THouseType;}
    end;
  public
    procedure SetMapSize(X,Y:integer);
    procedure RevealCircle(Pos:TKMPoint; Radius,Amount:word);
    procedure RevealEverything;
    function CheckVerticeRevelation(const X,Y:word):byte;
    function CheckTileRevelation(const X,Y:word):byte;

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);

    procedure UpdateState;
  end;


implementation


{ TKMFogOfWar }
procedure TKMFogOfWar.SetMapSize(X,Y:integer);
var i:integer;
begin
  MapX := X;
  MapY := Y;
  SetLength(Revelation, Y+1);
  for i:=1 to Y do
    SetLength(Revelation[i], X+1);
end;


{Reveal circle on map}
{Amount controls how "strong" terrain is revealed, almost instantly or slowly frame-by-frame in multiple calls}
procedure TKMFogOfWar.RevealCircle(Pos:TKMPoint; Radius,Amount:word);
var i,k:integer;
begin
  for i:=max(Pos.Y-Radius,2) to min(Pos.Y+Radius,MapY-1) do //Keep map edges unrevealed
  for k:=max(Pos.X-Radius,2) to min(Pos.X+Radius,MapX-1) do
  if sqrt(sqr(Pos.x-k) + sqr(Pos.y-i)) <= Radius then
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
function TKMFogOfWar.CheckVerticeRevelation(const X,Y:word):byte;
begin
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
function TKMFogOfWar.CheckTileRevelation(const X,Y:word):byte;
begin
  //Check all four corners and choose max
  Result := CheckVerticeRevelation(X,Y);
  if Result = 255 then exit;
  if X+1 <= MapX-1 then Result := max(Result, CheckVerticeRevelation(X+1,Y));
  if Result = 255 then exit;
  if (X+1 <= MapX-1) and (Y+1 <= MapY-1) then Result := max(Result, CheckVerticeRevelation(X+1,Y+1));
  if Result = 255 then exit;
  if Y+1 <= MapY-1 then Result := max(Result, CheckVerticeRevelation(X,Y+1));
end;


procedure TKMFogOfWar.Save(SaveStream:TKMemoryStream);
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


procedure TKMFogOfWar.Load(LoadStream:TKMemoryStream);
var i,k:integer; s:string;
begin
  LoadStream.Read(s);
  Assert(s = 'FOW');
  LoadStream.Read(MapX);
  LoadStream.Read(MapY);
  LoadStream.Read(fAnimStep);
  SetMapSize(MapX, MapY);
  for i:=1 to MapY do
  for k:=1 to MapX do
    LoadStream.Read(Revelation[i,k], SizeOf(Revelation[i,k]));
end;


procedure TKMFogOfWar.UpdateState;
var i,k:cardinal;
begin
  inc(fAnimStep);

  if FOG_OF_WAR_ENABLE then
  for i:=1 to MapY do
  for k:=1 to MapX do
  if (i*MapX+k+fAnimStep) mod TERRAIN_PACE = 0 then //All those global things can be performed once a sec, or even less frequent
    if Revelation[i,k].Visibility > FOG_OF_WAR_MIN then dec(Revelation[i,k].Visibility, FOG_OF_WAR_DEC);
end;


end.
