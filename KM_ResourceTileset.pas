unit KM_ResourceTileset;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, dglOpenGL,
  {$IFDEF WDC} ZLibEx, {$ENDIF}
  {$IFDEF FPC} ZStream, {$ENDIF}
  KM_Defaults, KM_Pics;


type
  TKMTileset = class
  private
    procedure LoadTileSet(const aPath: string);
    procedure MakeMiniMapColors(const FileName: string);
  public
    TextL: Cardinal; //Shading gradient for lighting
    TextD: Cardinal; //Shading gradient for darkening (same as light but reversed)
    TextT: Cardinal; //Tiles
    TextW: array[1..8]of Cardinal; //Water
    TextS: array[1..3]of Cardinal; //Swamps
    TextF: array[1..5]of Cardinal; //WaterFalls
    TileColor: array[0..255] of record R,G,B:Byte end;
    constructor Create(const aPath: string);
  end;


implementation
uses KM_TGATexture, KM_ResourceSprites;


{ TKMTileset }
constructor TKMTileset.Create(const aPath: string);
var I: Integer;
begin
  Inherited Create;

  LoadTileSet(aPath);

  //Special case for Tileset for MapEd menu
  RXData[rxTiles].Count := 256;
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

  MakeMiniMapColors(aPath + 'Tiles1.tga');
end;


// Load the Textures
procedure TKMTileset.LoadTileSet(const aPath: string);
var
  I: Integer;
  pData: array [0..255] of Cardinal;
begin
  //Generate gradients programmatically
  //todo: Compare gradients with KaM
  //[16-gradient]
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
      pData[I] := EnsureRange(Round((255 - I) * 1.0625 - 16), 0, 255) * 65793 or $FF000000;
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 256, 1, 0, GL_RGBA, GL_UNSIGNED_BYTE, @pData[0]);
  end;

  LoadTextureTGA(aPath + 'Tiles1.tga', TextT);
  LoadTextureTGA(aPath + 'gradient.tga', TextL);
  LoadTextureTGA(aPath + 'gradient.tga', TextD);

  if MAKE_ANIM_TERRAIN then begin
    for i:=1 to 8 do LoadTextureTGA(aPath + 'Water'+inttostr(i)+'.tga', TextW[i]);
    for i:=1 to 3 do LoadTextureTGA(aPath + 'Swamp'+inttostr(i)+'.tga', TextS[i]);
    for i:=1 to 5 do LoadTextureTGA(aPath + 'Falls'+inttostr(i)+'.tga', TextF[i]);
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


end.
