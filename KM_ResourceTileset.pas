unit KM_ResourceTileset;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, dglOpenGL,
  {$IFDEF WDC} ZLibEx, {$ENDIF}
  {$IFDEF FPC} ZStream, {$ENDIF}
  KM_Defaults, KM_Pics, KM_Points, KM_ResourceSprites;


type
  //TKMTileProperty = set of (tpWalkable, tpRoadable);

  TKMTileset = class
  private
    TileTable: array [1 .. 30, 1 .. 30] of packed record
      Tile1, Tile2, Tile3: byte;
      b1, b2, b3, b4, b5, b6, b7: boolean;
    end;

    function LoadPatternDAT(const FileName: string): Boolean;
    procedure LoadTextures(const aPath: string);
    procedure GenerateGFX;
    procedure MakeMiniMapColors(const FileName: string);
  public
    PatternDAT: array [1..256] of packed record
      MinimapColor: byte;
      Walkable: byte;  //This looks like a bitfield, but everything besides <>0 seems to have no logical explanation
      Buildable: byte; //This looks like a bitfield, but everything besides <>0 seems to have no logical explanation
      u1: byte; // 1/2/4/8/16 bitfield, seems to have no logical explanation
      u2: byte; // 0/1 Boolean? seems to have no logical explanation
      u3: byte; // 1/2/4/8 bitfield, seems to have no logical explanation
    end;

    TextL: Cardinal; //Shading gradient for lighting
    TextD: Cardinal; //Shading gradient for darkening (same as light but reversed)
    TextT: Cardinal; //Tiles
    TextW: array [1..8] of Cardinal; //Water
    TextS: array [1..3] of Cardinal; //Swamps
    TextF: array [1..5] of Cardinal; //WaterFalls
    TileColor: array [Byte] of record R,G,B: Byte end;

    constructor Create(const aPath, aPatternPath: string; aSprites: TKMSpritePack);

    procedure ExportPatternDat(const aFilename: string);

    function TileIsWater(aTile: Byte): Boolean;
    //function TileHasWater(aTile: Byte): Boolean;
    function TileIsSand(aTile: Byte): Boolean;
    function TileIsStone(aTile: Byte): Byte;
    function TileIsSoil(aTile: Byte): Boolean;
    function TileIsWalkable(aTile: Byte): Boolean;
    function TileIsRoadable(aTile: Byte): Boolean;
    function TileIsCornField(aTile: Byte): Boolean;
    function TileIsWineField(aTile: Byte): Boolean;
    function TileIsFactorable(aTile: Byte): Boolean;
  end;


implementation
uses KM_TGATexture;


{ TKMTileset }
constructor TKMTileset.Create(const aPath, aPatternPath: string; aSprites: TKMSpritePack);
begin
  inherited Create;

  LoadPatternDAT(aPatternPath);

  LoadTextures(aPath);
  GenerateGFX;

  MakeMiniMapColors(aPath + 'Tiles1.tga');
end;


// Load the Textures
procedure TKMTileset.LoadTextures(const aPath: string);
var
  I: Integer;
  pData: array [0..255] of Cardinal;
begin
  if aPath = '' then Exit;

  //Generate gradients programmatically

  //KaM uses [0..255] gradients
  //We use slightly smoothed gradients [16..255] for Remake cos it shows much more of terrain on screen and it looks too contrast
  TextL := GenerateTextureCommon;
  if TextL <> 0 then
  begin
    for I := 0 to 255 do
      pData[I] := EnsureRange(Round(I * 1.0625 - 16), 0, 255) * 65793 or $FF000000;
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 256, 1, 0, GL_RGBA, GL_UNSIGNED_BYTE, @pData[0]);
  end;

  TextD := GenerateTextureCommon;
  if TextD <> 0 then
  begin
    for I := 0 to 255 do
      pData[I] := EnsureRange(Round(I * 1.0625 - 16), 0, 255) * 65793 or $FF000000;
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 256, 1, 0, GL_RGBA, GL_UNSIGNED_BYTE, @pData[0]);
  end;

  LoadTextureTGA(aPath + 'Tiles1.tga', TextT);
  //LoadTextureTGA(aPath + 'gradient.tga', TextL);
  //LoadTextureTGA(aPath + 'gradient.tga', TextD);

  if MAKE_ANIM_TERRAIN then begin
    for i:=1 to 8 do LoadTextureTGA(aPath + 'Water'+inttostr(i)+'.tga', TextW[i]);
    for i:=1 to 3 do LoadTextureTGA(aPath + 'Swamp'+inttostr(i)+'.tga', TextS[i]);
    for i:=1 to 5 do LoadTextureTGA(aPath + 'Falls'+inttostr(i)+'.tga', TextF[i]);
  end;
end;


//Reading pattern data (tile info)
function TKMTileset.LoadPatternDAT(const FileName: string): Boolean;
var
  I: Integer;
  f: file;
  s: byte;
begin
  Result := false;
  if not FileExists(FileName) then
    Exit;
  assignfile(f, FileName);
  reset(f, 1);
  blockread(f, PatternDAT[1], 6 * 256);
  for I := 1 to 30 do
  begin
    blockread(f, TileTable[I, 1], 30 * 10);
    blockread(f, s, 1);
  end;

  closefile(f);

  if WriteResourceInfoToTXT then
    ExportPatternDat(ExeDir + 'Export\Pattern.csv');

  Result := true;
end;


procedure TKMTileset.ExportPatternDat(const aFileName: string);
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


procedure TKMTileset.GenerateGFX;
var I: Integer;
begin
  SetLength(GFXData[rxTiles], 256 + 1);
  //Generate UV coords
  for I := 0 to 255 do
  with GFXData[rxTiles, I+1] do
  begin
    TexID := TextT;
    v1 := (I div 16  ) / 16; //There are 16 tiles across the line
    u1 := (I mod 16  ) / 16;
    v2 := (I div 16+1) / 16;
    u2 := (I mod 16+1) / 16;
    PxWidth := 32;
    PxHeight := 32;
  end;
end;

{Tile textures aren't always the same, e.g. if someone makes a mod they will be different,
thus it's better to spend few ms and generate minimap colors from actual data}
procedure TKMTileset.MakeMiniMapColors(const FileName: string);
var
  ii,kk,h,j,pX: Integer;
  c:array of byte;
  R,G,B,SizeX,SizeY: Integer;
  f: file;
  InputStream: TFileStream;
  OutputStream: TMemoryStream;
  {$IFDEF WDC}
  DecompressionStream: TZDecompressionStream;
  {$ENDIF}
  {$IFDEF FPC}
  DecompressionStream: TDecompressionStream;
  i: Integer;
  Buf: array[0..1023]of Byte;
  {$ENDIF}
begin
  if not FileExists(FileName) then exit;
  AssignFile(f, FileName);
  FileMode:=0; Reset(f,1); FileMode:=2; //Open ReadOnly

  SetLength(c,18+1);
  blockread(f,c[1],18); //SizeOf(TGAHeader)
  SizeX := c[13]+c[14]*256;
  SizeY := c[15]+c[16]*256;

  if c[1]=120 then
  begin
    closefile(f);
    InputStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    OutputStream := TMemoryStream.Create;
    {$IFDEF WDC}
     DecompressionStream := TZDecompressionStream.Create(InputStream);
     OutputStream.CopyFrom(DecompressionStream, 0);
    {$ENDIF}
    {$IFDEF FPC}
     DecompressionStream := TDecompressionStream.Create(InputStream);
     repeat
       i:=DecompressionStream.Read(Buf, SizeOf(Buf));
       if i <> 0 then OutputStream.Write(Buf, i);
     until i <= 0;
    {$ENDIF}
    OutputStream.Position := 0;
    OutputStream.ReadBuffer(c[1], 18); //SizeOf(TGAHeader)
    SizeX := c[13]+c[14]*256;
    SizeY := c[15]+c[16]*256;
    SetLength(c,SizeX*SizeY*4+1);
    OutputStream.ReadBuffer(c[1], SizeX*SizeY*4);
    InputStream.Free;
    OutputStream.Free;
    DecompressionStream.Free;
  end
  else
  begin
    SetLength(c,SizeX*SizeY*4+1);
    blockread(f,c[1],SizeX*SizeY*4);
    closefile(f);
  end;

  for ii:=0 to 15 do for kk:=0 to 15 do
  begin

    R:=0; G:=0; B:=0;

    for j:=0 to (SizeY div 16 - 1) do
    for h:=0 to (SizeX div 16 - 1) do
    begin
      pX := (((SizeX-1)-(ii*(SizeY div 16)+j))*SizeX+kk*(SizeX div 16)+h)*4; //TGA comes flipped upside down
      inc(B, c[pX+1]);
      inc(G, c[pX+2]);
      inc(R, c[pX+3]);
    end;

    pX := ii*16+kk;

    TileColor[pX].R := round(R / (SizeX*SizeY div 256)); //each tile is 32x32 px
    TileColor[pX].G := round(G / (SizeX*SizeY div 256));
    TileColor[pX].B := round(B / (SizeX*SizeY div 256));
  end;

end;


{Check if requested tile is water suitable for fish and/or sail. No waterfalls, but swamps/shallow water allowed}
function TKMTileset.TileIsWater(aTile: Byte): Boolean;
begin
  Result := aTile in [48,114,115,119,192,193,194,196, 200, 208..211, 235,236, 240,244];
end;


{//Check if requested tile has any water, including ground-water transitions
function TKMTileset.TileHasWater(aTile: Byte): Boolean;
begin
  Result := aTile in [4,10,12,22,23,44,48,105..107,114..127,142,143,192..194,196,198..200,208..211,230,232..244];
end;}


{Check if requested tile is sand suitable for crabs}
function TKMTileset.TileIsSand(aTile: Byte): Boolean;
begin
  Result := aTile in [31..33, 70,71, 99,100,102,103, 108,109, 112,113, 116,117, 169, 173, 181, 189];
end;


{Check if requested tile is Stone and returns Stone deposit}
function TKMTileset.TileIsStone(aTile: Byte): Byte;
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


{Check if requested tile is soil suitable for fields and trees}
function TKMTileset.TileIsSoil(aTile: Byte): Boolean;
begin
  Result := aTile in [0..3,5,6, 8,9,11,13,14, 16..21, 26..28, 34..39, 47, 49, 55..58, 64..69, 72..80, 84..87, 88,89, 93..98,180,182..183,188,190..191,220,247];
end;


{Check if requested tile is generally walkable}
function TKMTileset.TileIsWalkable(aTile: Byte): Boolean;
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
function TKMTileset.TileIsRoadable(aTile: Byte): Boolean;
begin
  //Do not include 1/2 and 1/4 walkable as roadable
  //Result := Land[Loc.Y,Loc.X].Terrain in [0..3,5,6, 8,9,11,13,14, 16..21, 26..31, 32..39, 45..47, 49, 52, 55, 56..63,
  //                                        64..71, 72..79, 80..87, 88..95, 96..103, 104,108,111, 112,113,
  //                                        152..155,180..183,188..191,
  //                                        203..205, 212,213,215, 220, 247];
  Result := PatternDAT[aTile+1].Buildable <> 0;
end;


function TKMTileset.TileIsCornField(aTile: Byte): Boolean;
begin
  Result := aTile in [59..63];
end;


function TKMTileset.TileIsWineField(aTile: Byte): Boolean;
begin
  Result := aTile = 55;
end;


function TKMTileset.TileIsFactorable(aTile: Byte): Boolean;
begin
  //List of tiles that cannot be factored (coordinates outside the map return true)
  Result := not (aTile in [7,15,24,50,53,144..151,156..165,198,199,202,206]);
end;


end.
