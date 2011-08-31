unit KM_ResourceTileset;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  {$IFDEF WDC} ZLibEx, {$ENDIF}
  {$IFDEF FPC} ZStream, {$ENDIF}
  KM_CommonTypes, KM_Defaults;


type
  TKMTileset = class
  private
    procedure LoadTileSet(const aPath:string);
    procedure MakeMiniMapColors(FileName:string);
  public
    TextG: Cardinal; //Shading gradient
    TextT: Cardinal; //Tiles
    TextW:array[1..8]of Cardinal; //Water
    TextS:array[1..3]of Cardinal; //Swamps
    TextF:array[1..5]of Cardinal; //WaterFalls
    TileColor:array[0..255] of record R,G,B:Byte end;
    constructor Create(const aPath:string);
  end;


implementation
uses KM_Render, KM_TGATexture;


{ TKMTileset }
constructor TKMTileset.Create(const aPath:string);
begin
  Inherited Create;

  LoadTileSet(aPath);
  MakeMiniMapColors(aPath+'Tiles1.tga');
end;


// Load the Textures
procedure TKMTileset.LoadTileSet(const aPath:string);
var i:integer;
begin
  LoadTexture(aPath + 'gradient.tga', TextG);
  LoadTexture(aPath + 'Tiles1.tga', TextT);

  //Generate UV coords
  for i:=0 to 255 do
    with GFXData[8, i+1] do
    begin
      TexID := TextT;
      v1 := (i div 16  ) / 16; //There are 16 tiles across the line
      u1 := (i mod 16  ) / 16;
      v2 := (i div 16+1) / 16;
      u2 := (i mod 16+1) / 16;
      PxWidth := 32;
      PxHeight := 32;
    end;

  if MAKE_ANIM_TERRAIN then begin
    for i:=1 to 8 do LoadTexture(aPath + 'Water'+inttostr(i)+'.tga', TextW[i]);
    for i:=1 to 3 do LoadTexture(aPath + 'Swamp'+inttostr(i)+'.tga', TextS[i]);
    for i:=1 to 5 do LoadTexture(aPath + 'Falls'+inttostr(i)+'.tga', TextF[i]);
  end;
end;


{Tile textures aren't always the same, e.g. if someone makes a mod they will be different,
thus it's better to spend few ms and generate minimap colors from actual data}
procedure TKMTileset.MakeMiniMapColors(FileName:string);
var ii,kk,h,j,pX:integer; c:array of byte; R,G,B,SizeX,SizeY:integer; f:file; {ft:textfile;}
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
