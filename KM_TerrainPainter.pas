unit KM_TerrainPainter;
{$I KaM_Remake.inc}
interface
uses Classes, KromUtils, Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Terrain;


type
  TTerrainPainter = class
  private
    //Fraction part of height, for smooth height editing
    HeightAdd: array [1 .. MAX_MAP_SIZE, 1 .. MAX_MAP_SIZE] of Byte;

    Land2: array[1 .. MAX_MAP_SIZE, 1 .. MAX_MAP_SIZE] of record
      TerType: ShortInt; //Stores terrain type per node
      Tiles: SmallInt;  //Stores kind of transition tile used, no need to save into MAP footer
    end;

    MapXn,MapYn:integer; //Cursor position node
    MapXc,MapYc:integer; //Cursor position cell
    MapXn2,MapYn2:integer; //keeps previous node position
    MapXc2,MapYc2:integer; //keeps previous cell position

    procedure BrushTerrainTile(Y, X, Index: integer);
    procedure RebuildMap(X,Y,Rad:integer);
    procedure EditBrush(aLoc: TKMPoint; aTile: Byte);
    procedure EditHeight;
    procedure EditTile(aLoc: TKMPoint; aTile,aRotation: Byte);
  public
    RandomizeTiling: Boolean;
    constructor Create;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure UpdateStateIdle;
  end;


var
  fTerrainPainter: TTerrainPainter;


const
  Combo:array[1..26,1..26,1..3]of SmallInt = (
   //Grass        //Moss  //GrassRed    //GrassRed2//GrassGrnd//Sand           //OrangeSand  //Ground         //Cobblest       //SandGrass   //GrassSand      //Swamp       //GrndSwmp //Ice         //SnowLittle   //SnowMedium  //SnowBig  //StoneMount     //GreyMount      //RedMount       //Black        //GroundStoned   //Water          //Coal    //Gold    //Iron
  ((  0,   0,  0),(8,0,0),( 17,  0,  0),(26, 0, 0),(34, 0, 0),(  32,   0,   0),( 29,  0,  0),(  35,   0,   0),( 215,   0,   0),( 28,  0,  0),(  27,   0,   0),( 48,  0,  0),(40, 0, 0),( 44,  0,  0),( 47,  0,   0),( 46,  0,  0),(45, 0, 0),( 132,   0,   0),( 159,   0,   0),( 164,   0,   0),(245, 0,    0),( 20,  0,  0),( 192,   0,   0),(155,0,0),(147,0,0),(151,0,0)), //Grass
  ((-19, -18,  9),(8,8,8),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //Moss
  (( 66,  67, 68),(0,0,0),( 17, 17, 17),( 0, 0, 0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //GrassRed
  (( 72,  73, 74),(0,0,0),(  0,  0,  0),(26,26,26),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //GrassRed2
  (( 84,  85, 86),(0,0,0),(-98,-97,-96),( 0, 0, 0),(34,34,34),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //GrassGround
  (( 69,  70, 71),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(  32,  32,  32),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //Sand
  ((  0,   0,  0),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(  99, 100, 101),( 29, 29, 29),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //RoughSand
  (( 56,  57, 58),(0,0,0),(  0,  0,  0),( 0, 0, 0),(87,88,89),(-113,-112,-111),(  0,  0,  0),(  35,  35,  35),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),( 21, 21, 21),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //Ground
  ((  0,   0,  0),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(  38,  39, 215),( 215, 215, 215),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //Cobblestones
  (( 93,  94, 95),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(   0,   0,   0),(-83,-82,-81),(   0,   0,   0),(   0,   0,   0),( 28, 28, 28),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //SandGrass
  ((  0,   0,  0),(0,0,0),(  0,  0,  0),(75,76,77),( 0, 0, 0),( 102, 103, 104),(-83,-82,-81),(   0,   0,   0),(   0,   0,   0),(-80,-79,-78),(  27,  27,  27),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //GrassSand
  ((120, 121,122),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),( 48, 48, 48),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //Swamp
  (( 90,  91, 92),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),(40,40,40),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //GroundSwamp
  ((  0,   0,  0),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),( 44, 44, 44),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //Ice
  ((  0,   0,  0),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),( 247,  64,  65),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),( 47, 47,  47),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //SnowLittle
  ((  0,   0,  0),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),( 44, -4,-10),(220,212, 213),( 46, 46, 46),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //SnowMedium
  ((  0,   0,  0),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(203,204,205),(45,45,45),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //SnowBig
  ((  0, 139,138),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),( 132, 132, 132),(   0,   0,   0),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //Moss
  ((180, 172,176),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),( 181, 173, 177),(  0,  0,  0),( 183, 175, 179),(   0,   0,   0),(  0,  0,  0),( 182, 174, 178),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),( 49,171,  51),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),( 159, 159, 159),(   0,   0,   0),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //Grey Mountains
  ((188, 168,184),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),( 189, 169, 185),(  0,  0,  0),( 191, 167, 187),(   0,   0,   0),(  0,  0,  0),( 190, 170, 186),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),( 52,166, 54),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),( 164, 164, 164),(  0, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //Red Mountains
  ((  0,   0,  0),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),( -53, -50,-165),(245, 0,    0),(  0,  0,  0),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //Black
  ((  0,   0,  0),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),(-113,-112,-111),(  0,  0,  0),(  21,  21,  20),( -38, -39, -38),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(-65,-64,-247),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(-179,-175,-183),(-187,-167,-191),(  0, 0,    0),( 20, 20, 20),(   0,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //GroundStoned
  ((123,-125,127),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),( 116,-117, 118),(  0,  0,  0),(-107,-106,-105),(-107,-106,-105),(  0,  0,  0),(-243,-242,-241),(114,115,119),( 0, 0, 0),(-22,-12,-23),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(-143,-200,-236),(-237,-200,-236),(-239,-200,-236),(245, 0,    0),(  0,  0,  0),( 192,   0,   0),(0,0,0),(0,0,0),(0,0,0)), //Water
  (( 56,  57, 58),(0,0,0),(  0,  0,  0),( 0, 0, 0),(87,88,89),(-113,-112,-111),(  0,  0,  0),( 152, 153, 154),(   0,   0,   0),(  0,  0,  0),(   0,   0,   0),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),(   0,   0,   0),(  0,  0,   0),(  0,  0,  0),(   0,   0,   0),(155,0,0),(0,0,0),(0,0,0)), //Coal
  ((180, 172,176),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),( 181, 173, 177),(  0,  0,  0),( 183, 175, 179),(   0,   0,   0),(  0,  0,  0),( 182, 174, 178),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),( 49,171,  51),(  0,  0,  0),( 0, 0, 0),(   0,   0,   0),( 144, 145, 146),(   0,   0,   0),(  0,  0,   0),(183,175,179),( 236, 200, 237),(0,0,0),(147,0,0),(0,0,0)), //Gold
  ((188, 168,184),(0,0,0),(  0,  0,  0),( 0, 0, 0),( 0, 0, 0),( 189, 169, 185),(  0,  0,  0),( 191, 167, 187),(   0,   0,   0),(  0,  0,  0),( 190, 170, 186),(  0,  0,  0),( 0, 0, 0),(  0,  0,  0),(  0,  0,   0),( 52,166, 54),( 0, 0, 0),(   0,   0,   0),(   0,   0,   0),( 148, 149, 150),(-53,-50,-165),(191,167,187),( 236, 200, 239),(0,0,0),(0,0,0),(151,0,0))  //Iron
  );

  //0     number of variants (1..X)
  //1..X  tile variants
  //
  RandomTiling:array[1..26,0..15]of byte = (
    (15,1,1,1,2,2,2,3,3,3,5,5,5,11,13,14), //reduced chance for "eye-catching" tiles
    (1,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (1,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,26,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,34,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (1,33,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,29,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (2,36,37,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,215,0,0,0,0,0,0,0,0,0,0,0,0,0,0),//Cobblestone
    (0,28,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,27,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,48,0,0,0,0,0,0,0,0,0,0,0,0,0,0), //swamp
    (3,41,42,43,0,0,0,0,0,0,0,0,0,0,0,0),//brownswamp
    (0,44,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,47,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,46,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,45,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (8,129,130,131,132,134,135,136,137,0,0,0,0,0,0,0),    //Stone
    (5,156,157,158,159,201{?},0,0,0,0,0,0,0,0,0,0),       //Grey
    (5,160,161,162,163,164,0,0,0,0,0,0,0,0,0,0),          //Rusty
    (0,245,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (1,196,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (9,152,153,154,154,154,155,155,155,155,0,0,0,0,0,0), //Coal //enriched pattern
    (9,144,145,146,146,146,147,147,147,147,0,0,0,0,0,0), //Gold
    (9,148,149,150,150,150,151,151,151,151,0,0,0,0,0,0)  //Iron
  );


implementation
uses KM_Log, KM_Utils;


constructor TTerrainPainter.Create;
var
  I,K: Integer;
begin
  inherited;

  for I := 1 to MAX_MAP_SIZE do
  for K := 1 to MAX_MAP_SIZE do
  begin
    Land2[I,K].TerType := 1; //Grass
  end;
end;


procedure TTerrainPainter.BrushTerrainTile(Y, X, Index: integer);
var
  xx, yy, T: integer;
begin
  if not fTerrain.TileInMapCoords(X, Y) then
    Exit;

  fTerrain.Land[Y, X].Terrain := Index;
  xx := EnsureRange(X, 1, fTerrain.MapX - 1);
  yy := EnsureRange(Y, 1, fTerrain.MapY - 1);
  Land2[yy, xx].TerType := Index;
  Land2[yy, xx + 1].TerType := Index;
  Land2[yy + 1, xx + 1].TerType := Index;
  Land2[yy + 1, xx].TerType := Index;

  T := abs(Combo[Index, Index, 1]); //Pick a tile ID from table
  if (RandomizeTiling) then
    case GameCursor.Tag1 of
      1 .. 17, 21 .. 23:
        if random(6) = 1 then
          T := RandomTiling[Index, random(RandomTiling[Index, 0]) + 1]; // chance of 1/6
      18 .. 20, 24 .. 26:
        T := RandomTiling[Index, random(RandomTiling[Index, 0]) + 1]; //equal chance
    end;
  fTerrain.Land[yy, xx].Terrain := T;
  fTerrain.Land[yy, xx].Rotation := random(4); //random direction for all plain tiles
end;


procedure TTerrainPainter.RebuildMap(X,Y,Rad:integer);
var i,k,pY,pX,Nodes,Rot,Ter1,Ter2,T,A,B,C,D:integer;
begin
  for i:=-Rad to Rad do for k:=-Rad to Rad do if sqrt(sqr(i)+sqr(k))<Rad then
  begin
  pX:=EnsureRange(X+k,1,fTerrain.MapX);
  pY:=EnsureRange(Y+i,1,fTerrain.MapY);
  if (Land2[pY  ,pX].TerType>0)and(Land2[pY  ,pX+1].TerType>0)
  and(Land2[pY+1,pX].TerType>0)and(Land2[pY+1,pX+1].TerType>0) then //don't touch custom placed tiles (with negative values)
    begin
    A:=abs(Land2[pY  ,pX].TerType); B:=abs(Land2[pY  ,pX+1].TerType);
    C:=abs(Land2[pY+1,pX].TerType); D:=abs(Land2[pY+1,pX+1].TerType);
    Rot:=0; Nodes:=1;// Ter1:=1; Ter2:=1;

    if (A=B)or(C=D)  then begin Ter1:=A; Ter2:=C; Nodes:=2; if A<C then Rot:=2 else Rot:=0; end;
    if (A=C)or(B=D)  then begin Ter1:=A; Ter2:=B; Nodes:=2; if A<B then Rot:=1 else Rot:=3; end;

    if A=D then begin Ter1:=A; Ter2:=B; Nodes:=4+1; Rot:=1; end; //special case \
    if B=C then begin Ter1:=A; Ter2:=B; Nodes:=4+2; Rot:=0; end; //special case /

    if (A=B)and(C=D) then begin Ter1:=A; Ter2:=C; Nodes:=2; if A<C then Rot:=2 else Rot:=0; end;
    if (A=C)and(B=D) then begin Ter1:=A; Ter2:=B; Nodes:=2; if A<B then Rot:=1 else Rot:=3; end;

    if (B=C)and(C=D) then begin Ter1:=C; Ter2:=A; Nodes:=3; if C<A then Rot:=3 else Rot:=1; end;
    if (A=C)and(C=D) then begin Ter1:=A; Ter2:=B; Nodes:=3; if A<B then Rot:=0 else Rot:=2; end;
    if (A=B)and(B=D) then begin Ter1:=A; Ter2:=C; Nodes:=3; if A<C then Rot:=2 else Rot:=0; end;
    if (A=B)and(B=C) then begin Ter1:=A; Ter2:=D; Nodes:=3; if A<D then Rot:=1 else Rot:=3; end;

    if (A=B)and(B=C)and(C=D) then begin Ter1:=A; Ter2:=A; Nodes:=4; Rot:=0; end;

      //Terrain table has only half filled, so make sure first comes bigger ID
      if Ter1<Ter2 then
        begin T:=Ter1; Ter1:=Ter2; Ter2:=T;
          case Nodes of
          1..3: Nodes:=4-Nodes;  //hence invert nodes count
          5..6: Rot:=1;
          end;
        end;

      //Some tiles placed upside and should be rotated 180`
      if Nodes<4 then
      if Combo[Ter1,Ter2,Nodes]<0 then case Rot of //Flip direction
      0: Rot:=2; 1: Rot:=3; 2: Rot:=0; 3: Rot:=1; end;

      if Nodes<4 then T:=abs(Combo[Ter1,Ter2,Nodes]);     //transition tiles
      if Nodes=4 then T:=abs(Combo[Ter1,Ter2,1]);         //no transition
      if Nodes>4 then T:=abs(Combo[Ter1,Ter2,3]);         //transition use 1 or 3

      if (RandomizeTiling)and(Ter1=Ter2) then //for plain tiles only
        case Ter1 of
        1..17,21..23: if random(6)=1 then T:=RandomTiling[Ter1,random(RandomTiling[Ter1,0])+1]; // chance of 1/6
        18..20,24..26: T:=RandomTiling[Ter1,random(RandomTiling[Ter1,0])+1]; //equal chance
        end;

      if Ter1=Ter2 then Rot:=random(4); //random direction for all plain tiles

      //Need to check if this tile was already smart-painted, "4-Nodes" hence default value is 0
      if Land2[pY,pX].Tiles<>Ter1*Ter2*(4-Nodes) then
      begin
        Land2[pY,pX].Tiles:=Ter1*Ter2*(4-Nodes);//store not only nodes info, but also terrain type used
        fTerrain.Land[pY,pX].Terrain:=T;
        fTerrain.Land[pY,pX].Rotation:=Rot mod 4;
      end;
    end;
  end;
end;


procedure TTerrainPainter.EditBrush(aLoc: TKMPoint; aTile: Byte);
var
  I,K,Rad: Integer;
begin
  //Cell below cursor
  MapXc := EnsureRange(round(GameCursor.Float.X+0.5),1,fTerrain.MapX);
  MapYc := EnsureRange(round(GameCursor.Float.Y+0.5),1,fTerrain.MapY);

  //Node below cursor
  MapXn := EnsureRange(round(GameCursor.Float.X+1),1,fTerrain.MapX);
  MapYn := EnsureRange(round(GameCursor.Float.Y+1),1,fTerrain.MapY);

  Rad := GameCursor.MapEdSize;
  if (MapXn2 <> MapXn) or (MapYn2 <> MapYn) then
    if Rad = 0 then
      Land2[MapYn, MapXn].TerType := GameCursor.Tag1
    else
    if Rad mod 2 = 1 then
    begin                                               //There are two brush types here, even and odd size
      Rad := Rad div 2;                                     //first comes odd sizes 1,3,5..
      for I:=-Rad to Rad do for K:=-Rad to Rad do
      if (GameCursor.MapEdShape = hsSquare)or(sqrt(sqr(I)+sqr(K))<Rad+0.5) then               //Rounding corners in a nice way
      BrushTerrainTile(MapYc+I,MapXc+K,GameCursor.Tag1);
    end
    else
    begin
      Rad := Rad div 2;                                     //even sizes 2,4,6..
      for I:=-Rad to Rad-1 do for K:=-Rad to Rad-1 do
      if (GameCursor.MapEdShape = hsSquare)or(sqrt(sqr(I+0.5)+sqr(K+0.5))<Rad) then           //Rounding corners in a nice way
      BrushTerrainTile(MapYc+I,MapXc+K,GameCursor.Tag1);
    end;
  RebuildMap(MapXc, MapYc, Rad + 5);

  MapXn2 := MapXn;
  MapYn2 := MapYn;
  MapXc2 := MapXc;
  MapYc2 := MapYc;
end;


procedure TTerrainPainter.EditHeight;
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


procedure TTerrainPainter.EditTile(aLoc: TKMPoint; aTile, aRotation: Byte);
begin
  if fTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then
  begin
    fTerrain.Land[aLoc.Y, aLoc.X].Terrain := aTile;
    fTerrain.Land[aLoc.Y, aLoc.X].Rotation := aRotation;
    fTerrain.UpdatePassability(aLoc);
  end;
end;


//Skip the KaM data and load MapEd vertice info
procedure TTerrainPainter.LoadFromFile(FileName: string);
var
  I,K: Integer;
  S: TKMemoryStream;
  NewX,NewY: Integer;
  ResHead: packed record x1:word; Allocated,Qty1,Qty2,x5,Len17:integer; end;
  Chunk: AnsiString;
begin
  if not CheckFileExists(FileName) then Exit;

  S := TKMemoryStream.Create;
  try
    S.LoadFromFile(FileName);
    S.Read(NewX); //We read header to new variables to avoid damage to existing map if header is wrong
    S.Read(NewY);
    Assert((NewX = fTerrain.MapX) and (NewY = fTerrain.MapY), 'Map size does not match map size');

    //Skip terrain data
    S.Seek(23 * NewX * NewY, soFromCurrent);

    //For now we just throw away the resource footer because we don't understand it (and save a blank one)
    S.Read(ResHead, 22);
    S.Seek(17 * ResHead.Allocated, soFromCurrent);

    //ADDN
    if S.Position < S.Size then
    begin
      Chunk := '    ';
      S.Read(Chunk[1], 4);
      if Chunk = 'ADDN' then
      begin
        S.Read(Chunk[1], 4);
        if Chunk = 'TILE' then
        begin
          S.Read(I, 4); //Chunk size
          S.Read(I, 4); //Cypher - ommited
          for I := 1 to NewY do
          for K := 1 to NewX do
            S.Read(Land2[I,K].TerType, 1);
        end
        else
          fLog.AddNoTime(FileName + ' has no MapEd.TILE chunk');
      end
      else
        fLog.AddNoTime(FileName + ' has no MapEd.ADDN chunk');
    end
    else
      fLog.AddNoTime(FileName + ' has no MapEd chunk');
  finally
    S.Free;
  end;
end;


procedure TTerrainPainter.SaveToFile(FileName: string);
var
  I,K: Integer;
  S: TKMemoryStream;
  NewX,NewY: Integer;
  ResHead: packed record x1:word; Allocated,Qty1,Qty2,x5,Len17:integer; end;
begin
  if not CheckFileExists(FileName) then Exit;

  S := TKMemoryStream.Create;
  try
    S.LoadFromFile(FileName);
    S.Read(NewX); //We read header to new variables to avoid damage to existing map if header is wrong
    S.Read(NewY);
    Assert((NewX = fTerrain.MapX) and (NewY = fTerrain.MapY), 'Map size does not match map size');

    //Skip terrain data
    S.Seek(23 * NewX * NewY, soFromCurrent);

    //For now we just throw away the resource footer because we don't understand it (and save a blank one)
    S.Read(ResHead, 22);
    S.Seek(17 * ResHead.Allocated, soFromCurrent);

    S.Write(AnsiString('ADDN')[1], 4);
    S.Write(AnsiString('TILE')[1], 4);

    S.Write(Integer(NewX * NewY)); //Chunk size
    S.Write(Integer(0)); //Cypher - ommited
    for I := 1 to NewY do
    for K := 1 to NewX do
      S.Write(Land2[I,K].TerType, 1);

    S.SaveToFile(FileName);
  finally
    S.Free;
  end;
end;


//Only MapEd accesses it
procedure TTerrainPainter.UpdateStateIdle;
begin
  case GameCursor.Mode of
    cmElevate,
    cmEqualize:  if (ssLeft in GameCursor.SState) or (ssRight in GameCursor.SState) then
                    EditHeight;
    cmBrush:     if (ssLeft in GameCursor.SState) then
                    EditBrush(GameCursor.Cell, GameCursor.Tag1);
    cmTiles:     if (ssLeft in GameCursor.SState) then
                    if GameCursor.MapEdDir in [0..3] then //Defined direction
                      EditTile(GameCursor.Cell, GameCursor.Tag1, GameCursor.MapEdDir)
                    else //Random direction
                      EditTile(GameCursor.Cell, GameCursor.Tag1, KaMRandom(4));
  end;
end;


end.
