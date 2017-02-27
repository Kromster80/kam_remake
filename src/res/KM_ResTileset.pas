unit KM_ResTileset;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KromUtils,
  KM_Defaults, KM_CommonTypes;


type
  //TKMTileProperty = set of (tpWalkable, tpRoadable);

  TKMResTileset = class
  private
    fCRC: Cardinal;
    TileTable: array [1 .. 30, 1 .. 30] of packed record
      Tile1, Tile2, Tile3: byte;
      b1, b2, b3, b4, b5, b6, b7: boolean;
    end;

    function LoadPatternDAT(const FileName: string): Boolean;
  public
    PatternDAT: array [1..256] of packed record
      MinimapColor: byte;
      Walkable: byte;  //This looks like a bitfield, but everything besides <>0 seems to have no logical explanation
      Buildable: byte; //This looks like a bitfield, but everything besides <>0 seems to have no logical explanation
      u1: byte; // 1/2/4/8/16 bitfield, seems to have no logical explanation
      u2: byte; // 0/1 Boolean? seems to have no logical explanation
      u3: byte; // 1/2/4/8 bitfield, seems to have no logical explanation
    end;

    TileColor: TRGBArray;

    constructor Create(const aPatternPath: string);

    property CRC: Cardinal read fCRC;

    procedure ExportPatternDat(const aFilename: string);

    function TileIsWater(aTile: Byte): Boolean;
    //function TileHasWater(aTile: Byte): Boolean;
    function TileIsIce(aTile: Byte): Boolean;
    function TileIsSand(aTile: Byte): Boolean;
    function TileIsStone(aTile: Byte): Byte;
    function TileIsSnow(aTile: Byte): Boolean;
    function TileIsCoal(aTile: Byte): Byte;
    function TileIsIron(aTile: Byte): Byte;
    function TileIsGold(aTile: Byte): Byte;
    function TileIsSoil(aTile: Byte): Boolean;
    function TileIsWalkable(aTile: Byte): Boolean;
    function TileIsRoadable(aTile: Byte): Boolean;
    function TileIsCornField(aTile: Byte): Boolean;
    function TileIsWineField(aTile: Byte): Boolean;
    function TileIsFactorable(aTile: Byte): Boolean;
  end;


implementation


{ TKMResTileset }
constructor TKMResTileset.Create(const aPatternPath: string);
begin
  inherited Create;

  LoadPatternDAT(aPatternPath);
end;


//Reading pattern data (tile info)
function TKMResTileset.LoadPatternDAT(const FileName: string): Boolean;
var
  I: Integer;
  f: file;
  s: byte;
begin
  Result := false;
  if not FileExists(FileName) then
    Exit;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f, 1);
  BlockRead(f, PatternDAT[1], 6 * 256);
  for I := 1 to 30 do
  begin
    BlockRead(f, TileTable[I, 1], 30 * 10);
    BlockRead(f, s, 1);
  end;

  CloseFile(f);
  fCRC := Adler32CRC(FileName);

  if WriteResourceInfoToTXT then
    ExportPatternDat(ExeDir + 'Export'+PathDelim+'Pattern.csv');

  Result := true;
end;


procedure TKMResTileset.ExportPatternDat(const aFileName: string);
var
  I, K: Integer;
  ft: TextFile;
begin
  AssignFile(ft, ExeDir + 'Pattern.csv');
  Rewrite(ft);
  Writeln(ft, 'PatternDAT');
  for I := 0 to 15 do
  begin
    for K := 1 to 16 do
      write(ft, inttostr(I * 16 + K), ' ', PatternDAT[I * 16 + K].u1, ';');
    writeln(ft);
  end;
  writeln(ft, 'TileTable');
  for I := 1 to 30 do
  begin
    for K := 1 to 30 do
    begin
      write(ft, inttostr(TileTable[I, K].Tile1) + '_' + inttostr(TileTable[I, K].Tile2) + '_' +
        inttostr(TileTable[I, K].Tile3) + ' ');
      write(ft, inttostr(byte(TileTable[I, K].b1)));
      write(ft, inttostr(byte(TileTable[I, K].b2)));
      write(ft, inttostr(byte(TileTable[I, K].b3)));
      write(ft, inttostr(byte(TileTable[I, K].b4)));
      write(ft, inttostr(byte(TileTable[I, K].b5)));
      write(ft, inttostr(byte(TileTable[I, K].b6)));
      write(ft, inttostr(byte(TileTable[I, K].b7)));
      write(ft, ';');
    end;

    writeln(ft);
  end;
  closefile(ft);
end;


{Check if requested tile is water suitable for fish and/or sail. No waterfalls, but swamps/shallow water allowed}
function TKMResTileset.TileIsWater(aTile: Byte): Boolean;
begin
  Result := aTile in [48,114,115,119,192,193,194,196, 200, 208..211, 235,236, 240,244];
end;


//Check if requested tile has ice
function TKMResTileset.TileIsIce(aTile: Byte): Boolean;
begin
  Result := aTile in [4, 10, 12, 22, 23, 44];
end;


{//Check if requested tile has any water, including ground-water transitions
function TKMResTileset.TileHasWater(aTile: Byte): Boolean;
begin
  Result := aTile in [4,10,12,22,23,44,48,105..107,114..127,142,143,192..194,196,198..200,208..211,230,232..244];
end;}


{Check if requested tile is sand suitable for crabs}
function TKMResTileset.TileIsSand(aTile: Byte): Boolean;
begin
  Result := aTile in [31..33, 70,71, 99,100,102,103, 108,109, 112,113, 116,117, 169, 173, 181, 189];
end;


{Check if requested tile is Stone and returns Stone deposit}
function TKMResTileset.TileIsStone(aTile: Byte): Byte;
begin
  case aTile of
    132,137: Result := 5;
    131,136: Result := 4;
    130,135: Result := 3;
    129,134: Result := 2;
    128,133: Result := 1;
    else     Result := 0;
  end;
end;


{Check if requested tile is sand suitable for crabs}
function TKMResTileset.TileIsSnow(aTile: Byte): Boolean;
begin
  Result := aTile in [45, 46, 47, 49, 52, 64, 65, 166, 171, 203, 204, 205, 212, 213, 220];
end;


function TKMResTileset.TileIsCoal(aTile: Byte): Byte;
begin
  Result := 0;
  if aTile > 151 then
    if aTile < 156 then
      Result := aTile - 151;
end;


function TKMResTileset.TileIsIron(aTile: Byte): Byte;
begin
  Result := 0;
  if aTile > 147 then
    if aTile < 152 then
      Result := aTile - 147;
end;


function TKMResTileset.TileIsGold(aTile: Byte): Byte;
begin
  Result := 0;
  if aTile > 143 then
    if aTile < 148 then
      Result := aTile - 143;
end;


{Check if requested tile is soil suitable for fields and trees}
function TKMResTileset.TileIsSoil(aTile: Byte): Boolean;
begin
  Result := aTile in [0..3,5,6, 8,9,11,13,14, 16..21, 26..28, 34..39, 47, 49, 55..58, 64..69, 72..80, 84..87, 88,89, 93..98,180,182..183,188,190..191,220,247];
end;


{Check if requested tile is generally walkable}
function TKMResTileset.TileIsWalkable(aTile: Byte): Boolean;
begin
  //Includes 1/2 and 3/4 walkable as walkable
  //Result := Land[Loc.Y,Loc.X].Terrain in [0..6, 8..11,13,14, 16..22, 25..31, 32..39, 44..47, 49,52,55, 56..63,
  //                                        64..71, 72..79, 80..87, 88..95, 96..103, 104,106..109,111, 112,113,116,117, 123..125,
  //                                        138..139, 152..155, 166,167, 168..175, 180..183, 188..191,
  //                                        197, 203..205,207, 212..215, 220..223, 242,243,247];
  //Values can be 1 or 2, What 2 does is unknown
  Result := PatternDAT[aTile+1].Walkable <> 0;
end;


{Check if requested tile is generally suitable for road building}
function TKMResTileset.TileIsRoadable(aTile: Byte): Boolean;
begin
  //Do not include 1/2 and 1/4 walkable as roadable
  //Result := Land[Loc.Y,Loc.X].Terrain in [0..3,5,6, 8,9,11,13,14, 16..21, 26..31, 32..39, 45..47, 49, 52, 55, 56..63,
  //                                        64..71, 72..79, 80..87, 88..95, 96..103, 104,108,111, 112,113,
  //                                        152..155,180..183,188..191,
  //                                        203..205, 212,213,215, 220, 247];
  Result := PatternDAT[aTile+1].Buildable <> 0;
end;


function TKMResTileset.TileIsCornField(aTile: Byte): Boolean;
begin
  Result := aTile in [59..63];
end;


function TKMResTileset.TileIsWineField(aTile: Byte): Boolean;
begin
  Result := aTile = 55;
end;


function TKMResTileset.TileIsFactorable(aTile: Byte): Boolean;
begin
  //List of tiles that cannot be factored (coordinates outside the map return true)
  Result := not (aTile in [7,15,24,50,53,144..151,156..165,198,199,202,206]);
end;


end.
