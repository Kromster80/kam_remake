unit KM_RenderTerrain;
{$I KaM_Remake.inc}
interface
uses
  dglOpenGL, SysUtils, KromUtils, Math,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_FogOfWar, KM_Pics, KM_ResSprites, KM_Points, KM_Terrain;

type
  TVBOArrayType = (vat_None, vat_Tile, vat_AnimTile);

  TUVRect = array [1 .. 4, 1 .. 2] of Single; // Texture UV coordinates

  TTileVertice = record
    X, Y, Z, UTile, VTile, ULit, UShd, UFow: Single;
  end;

  TAnimTileVertice = record
    X, Y, Z, UAnim, VAnim: Single;
  end;

  //Render terrain without sprites
  TRenderTerrain = class
  private
    fClipRect: TKMRect;
    fTextG: GLuint; //Shading gradient for lighting
    fTextB: GLuint; //Contrast BW for FOW over color-coder
    fUseVBO: Boolean; //Wherever to render terrain through VBO (faster but needs GL1.5) or DrawCalls (slower but needs only GL1.1)
    fTilesVtx: array of TTileVertice; //Vertice cache for tiles
    fTilesInd: array of Integer;      //Indexes for tiles array
    fAnimTilesVtx: array of TAnimTileVertice; //Vertice cache for tiles animations (water/falls/swamp)
    fAnimTilesInd: array of Integer;          //Indexes for array tiles animation array
    fVtxTilesShd: GLUint;
    fIndTilesShd: GLUint;
    fVtxAnimTilesShd: GLUint;
    fIndAnimTilesShd: GLUint;
    fTileUVLookup: array [0..255, 0..3] of TUVRect;
    fLastBindVBOArrayType: TVBOArrayType;
    function GetTileUV(Index: Word; Rot: Byte): TUVRect;
    procedure BindVBOArray(aVBOArrayType: TVBOArrayType);
    procedure UpdateVBO(aAnimStep: Integer; aFOW: TKMFogOfWarCommon);
    procedure DoTiles;
    procedure DoOverlays;
    procedure DoLighting;
    procedure DoWater(aAnimStep: Integer; aFOW: TKMFogOfWarCommon);
    procedure DoShadows;
    function VBOSupported: Boolean;
    procedure RenderFence(aFence: TFenceType; Pos: TKMDirection; pX,pY: Integer);
    procedure RenderMarkup(pX, pY: Word; aFieldType: TFieldType);
  public
    constructor Create;
    destructor Destroy; override;
    property ClipRect: TKMRect read fClipRect write fClipRect;
    procedure RenderBase(aAnimStep: Integer; aFOW: TKMFogOfWarCommon);
    procedure RenderFences;
    procedure RenderPlayerPlans(aFieldsList: TKMPointTagList; aHousePlansList: TKMPointDirList);
    procedure RenderFOW(aFOW: TKMFogOfWarCommon; aUseContrast: Boolean);
    procedure RenderTile(Index: Byte; pX,pY,Rot: Integer; DoHighlight: Boolean = False; HighlightColor: Cardinal = 0);
    procedure RenderTileOverlay(pX, pY: Integer; DoHighlight: Boolean = False; HighlightColor: Cardinal = 0);
  end;


implementation
uses
  KM_Render;

type
  TAnimLayer = (alWater, alFalls, alSwamp);


constructor TRenderTerrain.Create;
var
  I, K: Integer;
  pData: array [0..255] of Cardinal;
begin
  inherited;
  if SKIP_RENDER then Exit;

  //Tiles UV lookup for faster access. Only base tileset for smaller size
  for I := 0 to 255 do
    for K := 0 to 3 do
      fTileUVLookup[I, K] := GetTileUV(I, K);

  //Generate gradient programmatically
  //KaM uses [0..255] gradients
  //We use slightly smoothed gradients [16..255] for Remake
  //cos it shows much more of terrain on screen and it looks too contrast
  for I := 0 to 255 do
    pData[I] := EnsureRange(Round(I * 1.0625 - 16), 0, 255) * 65793 or $FF000000;

  fTextG := TRender.GenTexture(256, 1, @pData[0], tf_RGBA8);

  //Sharp transition between black and white
  pData[0] := $FF000000;
  pData[1] := $00000000;
  pData[2] := $00000000;
  pData[3] := $00000000;
  fTextB := TRender.GenTexture(4, 1, @pData[0], tf_RGBA8);

  fUseVBO := VBOSupported and not RENDER_3D;

  if fUseVBO then
  begin
    glGenBuffers(1, @fVtxTilesShd);
    glGenBuffers(1, @fIndTilesShd);
    glGenBuffers(1, @fVtxAnimTilesShd);
    glGenBuffers(1, @fIndAnimTilesShd);
  end;
end;


destructor TRenderTerrain.Destroy;
begin
//  fUseVBO := VBOSupported; //Could have been set to false if 3D rendering is enabled, so reset it
  if fUseVBO then
  begin
    //Since RenderTerrain is created fresh everytime fGame is created, we should clear
    //the buffers to avoid memory leaks.
    glDeleteBuffers(1, @fVtxTilesShd);
    glDeleteBuffers(1, @fIndTilesShd);
    glDeleteBuffers(1, @fVtxAnimTilesShd);
    glDeleteBuffers(1, @fIndAnimTilesShd);
  end;
  inherited;
end;


function TRenderTerrain.VBOSupported:Boolean;
begin
  //Some GPUs don't comply with OpenGL 1.5 spec on VBOs, so check Assigned instead of GL_VERSION_1_5
  Result := Assigned(glGenBuffers)        and Assigned(glBindBuffer)    and Assigned(glBufferData) and
            Assigned(glEnableClientState) and Assigned(glVertexPointer) and Assigned(glClientActiveTexture) and
            Assigned(glTexCoordPointer)   and Assigned(glDrawElements)  and Assigned(glDisableClientState) and
            Assigned(glDeleteBuffers);
end;


function TRenderTerrain.GetTileUV(Index: Word; Rot: Byte): TUVRect;
var
  TexO: array [1 .. 4] of Byte; // order of UV coordinates, for rotations
  A: Byte;
begin
  TexO[1] := 1;
  TexO[2] := 2;
  TexO[3] := 3;
  TexO[4] := 4;

  // Rotate by 90 degrees: 4-1-2-3
  if Rot and 1 = 1 then
  begin
    A := TexO[4];
    TexO[4] := TexO[3];
    TexO[3] := TexO[2];
    TexO[2] := TexO[1];
    TexO[1] := A;
  end;

  //Rotate by 180 degrees: 3-4-1-2
  if Rot and 2 = 2 then
  begin
    SwapInt(TexO[1], TexO[3]);
    SwapInt(TexO[2], TexO[4]);
  end;

  // Rotate by 270 degrees = 90 + 180

  //Apply rotation
  with GFXData[rxTiles, Index+1] do
  begin
    Result[TexO[1], 1] := Tex.u1; Result[TexO[1], 2] := Tex.v1;
    Result[TexO[2], 1] := Tex.u1; Result[TexO[2], 2] := Tex.v2;
    Result[TexO[3], 1] := Tex.u2; Result[TexO[3], 2] := Tex.v2;
    Result[TexO[4], 1] := Tex.u2; Result[TexO[4], 2] := Tex.v1;
  end;
end;


procedure TRenderTerrain.UpdateVBO(aAnimStep: Integer; aFOW: TKMFogOfWarCommon);
var
  Fog: PKMByte2Array;

  procedure SetTileVertex(aH: Integer; aTX, aTY: Word; aIsBottomRow: Boolean; aUTile, aVTile: Single);
  begin
    with gTerrain do
    begin
      fTilesVtx[aH].X := aTX;
      fTilesVtx[aH].Y := aTY - Land[aTY+1, aTX+1].Height / CELL_HEIGHT_DIV;
      fTilesVtx[aH].Z := aTY - Byte(aIsBottomRow);
      fTilesVtx[aH].UTile := aUTile;
      fTilesVtx[aH].VTile := aVTile;
      fTilesVtx[aH].ULit := Land[aTY+1, aTX+1].Light;
      fTilesVtx[aH].UShd := -Land[aTY+1, aTX+1].Light;
      if Fog <> nil then
        fTilesVtx[aH].UFow := Fog^[aTY, aTX] / 256
      else
        fTilesVtx[aH].UFow := 255;
    end;
  end;

  procedure SetAnimTileVertex(aQ: Integer; aTX, aTY: Word; aIsBottomRow: Boolean; aUAnimTile, aVAnimTile: Single);
  begin
    with gTerrain do
    begin
      fAnimTilesVtx[aQ].X := aTX;
      fAnimTilesVtx[aQ].Y := aTY - Land[aTY+1, aTX+1].Height / CELL_HEIGHT_DIV;
      fAnimTilesVtx[aQ].Z := aTY - Byte(aIsBottomRow);
      fAnimTilesVtx[aQ].UAnim := aUAnimTile;
      fAnimTilesVtx[aQ].VAnim := aVAnimTile;
    end;
  end;

  function TryAddAnimTex(var aQ: Integer; aTX, aTY, aTexOffset: Word): Boolean;
  var TexAnimC: TUVRect;
  begin
    Result := False;
    with gTerrain do
      if (InRange(aTexOffset + Land[aTY,aTX].Terrain + 1, 305, High(GFXData[rxTiles])))
         and (GFXData[rxTiles, aTexOffset + Land[aTY,aTX].Terrain + 1].Tex.ID <> 0)
         and (aFOW.CheckTileRevelation(aTX,aTY) > FOG_OF_WAR_ACT) then
      begin
        TexAnimC := GetTileUV(aTexOffset + Land[aTY,aTX].Terrain, Land[aTY,aTX].Rotation mod 4);

        SetAnimTileVertex(aQ,   aTX-1, aTY-1, False, TexAnimC[1][1], TexAnimC[1][2]);
        SetAnimTileVertex(aQ+1, aTX-1, aTY,   True,  TexAnimC[2][1], TexAnimC[2][2]);
        SetAnimTileVertex(aQ+2, aTX,   aTY,   True,  TexAnimC[3][1], TexAnimC[3][2]);
        SetAnimTileVertex(aQ+3, aTX,   aTY-1, False, TexAnimC[4][1], TexAnimC[4][2]);

        aQ := aQ + 4;
        Result := True;
      end;
  end;

var
  I,K,H,Q,AnimCnt: Integer;
  SizeX, SizeY: Word;
  tX, tY: Word;
  Row: Integer;
  TexTileC: TUVRect;
  AL: TAnimLayer;
  TexOffsetWater, TexOffsetFalls, TexOffsetSwamp: Word;
begin
  if not fUseVBO then Exit;

  fLastBindVBOArrayType := vat_None;

  if aFOW is TKMFogOfWar then
    Fog := @TKMFogOfWar(aFOW).Revelation
  else
    Fog := nil;

  SizeX := Max(fClipRect.Right - fClipRect.Left, 0);
  SizeY := Max(fClipRect.Bottom - fClipRect.Top, 0);

  H := 0;
  Q := 0;

  TexOffsetWater := 0;
  TexOffsetFalls := 0;
  TexOffsetSwamp := 0;

  for AL := Low(TAnimLayer) to High(TAnimLayer) do
    case AL of
      alWater: TexOffsetWater := 300 * (aAnimStep mod 8 + 1);       // 300..2400
      alFalls: TexOffsetFalls := 300 * (aAnimStep mod 5 + 1 + 8);   // 2700..3900
      alSwamp: TexOffsetSwamp := 300 * ((aAnimStep mod 24) div 8 + 1 + 8 + 5); // 4200..4800
    end;

  SetLength(fTilesVtx, (SizeX + 1) * 4 * (SizeY + 1));
  SetLength(fAnimTilesVtx, (SizeX + 1) * 4 * (SizeY + 1));
  with gTerrain do
    if (MapX > 0) and (MapY > 0) then
      for I := 0 to SizeY do
        for K := 0 to SizeX do
        begin
          tX := K + fClipRect.Left;
          tY := I + fClipRect.Top;
          TexTileC := fTileUVLookup[Land[tY, tX].Terrain, Land[tY, tX].Rotation mod 4];

          //Fill Tile vertices array
          SetTileVertex(H,   tX-1, tY-1, False, TexTileC[1][1], TexTileC[1][2]);
          SetTileVertex(H+1, tX-1, tY,   True,  TexTileC[2][1], TexTileC[2][2]);
          SetTileVertex(H+2, tX,   tY,   True,  TexTileC[3][1], TexTileC[3][2]);
          SetTileVertex(H+3, tX,   tY-1, False, TexTileC[4][1], TexTileC[4][2]);

          //Fill tiles animation vertices array
          if not TryAddAnimTex(Q, tX, tY, TexOffsetWater) then  //every tile can have only 1 animation
            if not TryAddAnimTex(Q, tX, tY, TexOffsetFalls) then
              TryAddAnimTex(Q, tX, tY, TexOffsetSwamp);

          H := H + 4;
        end;


  //Cut animation vertices array to actual size
  SetLength(fAnimTilesVtx, Q);

  //Fill indexes array for tiles vertices array
  H := 0;
  SetLength(fTilesInd, (SizeX+1) * (SizeY+1) * 6);
  for I := 0 to SizeY do
    for K := 0 to SizeX do
    begin
      Row := (I * (SizeX + 1)) shl 2;
      fTilesInd[H+0] := Row + K shl 2; // shl 2 = *4
      fTilesInd[H+1] := Row + (K shl 2) + 1;
      fTilesInd[H+2] := Row + (K shl 2) + 2;
      fTilesInd[H+3] := Row + (K shl 2);
      fTilesInd[H+4] := Row + (K shl 2) + 3;
      fTilesInd[H+5] := Row + (K shl 2) + 2;
      H := H + 6;
    end;

  AnimCnt := Q div 4;
  //Fill indexes array for tiles animation vertices array
  H := 0;
  I := 0;
  SetLength(fAnimTilesInd, AnimCnt*6);
  while I < AnimCnt do
  begin
    fAnimTilesInd[H+0] := I shl 2; // shl 2 = *4
    fAnimTilesInd[H+1] := (I shl 2) + 1;
    fAnimTilesInd[H+2] := (I shl 2) + 2;
    fAnimTilesInd[H+3] := (I shl 2);
    fAnimTilesInd[H+4] := (I shl 2) + 3;
    fAnimTilesInd[H+5] := (I shl 2) + 2;
    H := H + 6;
    Inc(I);
  end;
end;


procedure TRenderTerrain.DoTiles;
var
  TexC: TUVRect;
  I,K: Integer;
begin
  //First we render base layer, then we do animated layers for Water/Swamps/Waterfalls
  //They all run at different speeds so we can't adjoin them in one layer
  glColor4f(1,1,1,1);
  //Draw with VBO only if all tiles are on the same texture
  if fUseVBO and TKMResSprites.AllTilesInOneAtlas then
  begin
    BindVBOArray(vat_Tile);
    //Bind to tiles texture. All tiles should be places in 1 atlas,
    //so to get TexId we can use any of terrain tile Id (f.e. 1st)
    TRender.BindTexture(GFXData[rxTiles, 1].Tex.ID);

    //Setup vertex and UV layout and offsets
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_FLOAT, SizeOf(TTileVertice), Pointer(0));
    glClientActiveTexture(GL_TEXTURE0);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glTexCoordPointer(2, GL_FLOAT, SizeOf(TTileVertice), Pointer(12));

    //Here and above OGL requests Pointer, but in fact it's just a number (offset within Array)
    glDrawElements(GL_TRIANGLES, Length(fTilesInd), GL_UNSIGNED_INT, Pointer(0));

    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  end
  else
  begin
    with gTerrain do
    for I := fClipRect.Top to fClipRect.Bottom do
    for K := fClipRect.Left to fClipRect.Right do
    begin
      with Land[I,K] do
      begin
        TRender.BindTexture(GFXData[rxTiles, Terrain+1].Tex.ID);
        glBegin(GL_TRIANGLE_FAN);
        TexC := fTileUVLookup[Terrain, Rotation mod 4];
      end;

      if RENDER_3D then
      begin
        glTexCoord2fv(@TexC[1]); glVertex3f(K-1,I-1,-Land[I,K].Height/CELL_HEIGHT_DIV);
        glTexCoord2fv(@TexC[2]); glVertex3f(K-1,I  ,-Land[I+1,K].Height/CELL_HEIGHT_DIV);
        glTexCoord2fv(@TexC[3]); glVertex3f(K  ,I  ,-Land[I+1,K+1].Height/CELL_HEIGHT_DIV);
        glTexCoord2fv(@TexC[4]); glVertex3f(K  ,I-1,-Land[I,K+1].Height/CELL_HEIGHT_DIV);
      end else begin
        glTexCoord2fv(@TexC[1]); glVertex3f(K-1,I-1-Land[I,K].Height / CELL_HEIGHT_DIV, I-1);
        glTexCoord2fv(@TexC[2]); glVertex3f(K-1,I  -Land[I+1,K].Height / CELL_HEIGHT_DIV, I-1);
        glTexCoord2fv(@TexC[3]); glVertex3f(K  ,I  -Land[I+1,K+1].Height / CELL_HEIGHT_DIV, I-1);
        glTexCoord2fv(@TexC[4]); glVertex3f(K  ,I-1-Land[I,K+1].Height / CELL_HEIGHT_DIV, I-1);
      end;
      glEnd;
    end;
  end;
end;


procedure TRenderTerrain.DoWater(aAnimStep: Integer; aFOW: TKMFogOfWarCommon);
var
  AL: TAnimLayer;
  I,K: Integer;
  TexC: TUVRect;
  TexOffset: Word;
begin
  //First we render base layer, then we do animated layers for Water/Swamps/Waterfalls
  //They all run at different speeds so we can't adjoin them in one layer
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  if fUseVBO and TKMResSprites.AllTilesInOneAtlas then
  begin
    if Length(fAnimTilesVtx) = 0 then Exit; //There is no animation on map
    BindVBOArray(vat_AnimTile);
    //Bind to tiles texture. All tiles should be placed in 1 atlas,
    //so to get TexId we can use any of terrain tile Id (f.e. 1st)
    TRender.BindTexture(GFXData[rxTiles, 1].Tex.ID);

    //Setup vertex and UV layout and offsets
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_FLOAT, SizeOf(TAnimTileVertice), Pointer(0));
    glClientActiveTexture(GL_TEXTURE0);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glTexCoordPointer(2, GL_FLOAT, SizeOf(TAnimTileVertice), Pointer(12));

    //Here and above OGL requests Pointer, but in fact it's just a number (offset within Array)
    glDrawElements(GL_TRIANGLES, Length(fAnimTilesInd), GL_UNSIGNED_INT, Pointer(0));

    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  end else begin
    //Each new layer inflicts 10% fps drop
    for AL := Low(TAnimLayer) to High(TAnimLayer) do
    begin
      case AL of
        alWater: TexOffset := 300 * (aAnimStep mod 8 + 1);       // 300..2400
        alFalls: TexOffset := 300 * (aAnimStep mod 5 + 1 + 8);   // 2700..3900
        alSwamp: TexOffset := 300 * ((aAnimStep mod 24) div 8 + 1 + 8 + 5); // 4200..4800
        else     TexOffset := 0;
      end;

      with gTerrain do
      for I := fClipRect.Top to fClipRect.Bottom do
      for K := fClipRect.Left to fClipRect.Right do
      if (TexOffset + Land[I,K].Terrain + 1 <= High(GFXData[rxTiles]))
      and (GFXData[rxTiles, TexOffset + Land[I,K].Terrain + 1].Tex.ID <> 0)
      and (aFOW.CheckTileRevelation(K,I) > FOG_OF_WAR_ACT) then //No animation in FOW
      begin
        TRender.BindTexture(GFXData[rxTiles, TexOffset + Land[I,K].Terrain + 1].Tex.ID);
        TexC := GetTileUV(TexOffset + Land[I,K].Terrain, Land[I,K].Rotation);

        glBegin(GL_TRIANGLE_FAN);
          glColor4f(1,1,1,1);
          if RENDER_3D then
          begin
            glTexCoord2fv(@TexC[1]); glVertex3f(K-1,I-1,-Land[I,K].Height/CELL_HEIGHT_DIV);
            glTexCoord2fv(@TexC[2]); glVertex3f(K-1,I  ,-Land[I+1,K].Height/CELL_HEIGHT_DIV);
            glTexCoord2fv(@TexC[3]); glVertex3f(K  ,I  ,-Land[I+1,K+1].Height/CELL_HEIGHT_DIV);
            glTexCoord2fv(@TexC[4]); glVertex3f(K  ,I-1,-Land[I,K+1].Height/CELL_HEIGHT_DIV);
          end
          else
          begin
            glTexCoord2fv(@TexC[1]); glVertex3f(K-1,I-1-Land[I,K].Height/CELL_HEIGHT_DIV, I-1);
            glTexCoord2fv(@TexC[2]); glVertex3f(K-1,I  -Land[I+1,K].Height/CELL_HEIGHT_DIV, I-1);
            glTexCoord2fv(@TexC[3]); glVertex3f(K  ,I  -Land[I+1,K+1].Height/CELL_HEIGHT_DIV, I-1);
            glTexCoord2fv(@TexC[4]); glVertex3f(K  ,I-1-Land[I,K+1].Height/CELL_HEIGHT_DIV, I-1);
          end;
        glEnd;
      end;
    end;
  end;
end;


//Render single tile overlay
procedure TRenderTerrain.RenderTileOverlay(pX, pY: Integer; DoHighlight: Boolean = False; HighlightColor: Cardinal = 0);
//   1      //Select road tile and rotation
//  8*2     //depending on surrounding tiles
//   4      //Bitfield
const
  RoadsConnectivity: array [0..15, 1..2] of Byte = (
    (248,0), (248,0), (248,1), (250,3),
    (248,0), (248,0), (250,0), (252,0),
    (248,1), (250,2), (248,1), (252,3),
    (250,1), (252,2), (252,1), (254,0));
var Road, ID, Rot: Byte;
begin
  case gTerrain.Land[pY, pX].TileOverlay of
    to_Dig1:  RenderTile(249, pX, pY, 0, DoHighlight, HighlightColor);
    to_Dig2:  RenderTile(251, pX, pY, 0, DoHighlight, HighlightColor);
    to_Dig3:  RenderTile(253, pX, pY, 0, DoHighlight, HighlightColor);
    to_Dig4:  RenderTile(255, pX, pY, 0, DoHighlight, HighlightColor);
    to_Road:  begin
                Road := 0;
                if (pY - 1 >= 1) then
                  Road := Road + byte(gTerrain.Land[pY - 1, pX].TileOverlay = to_Road) shl 0;
                if (pX + 1 <= gTerrain.MapX - 1) then
                  Road := Road + byte(gTerrain.Land[pY, pX + 1].TileOverlay = to_Road) shl 1;
                if (pY + 1 <= gTerrain.MapY - 1) then
                  Road := Road + byte(gTerrain.Land[pY + 1, pX].TileOverlay = to_Road) shl 2;
                if (pX - 1 >= 1) then
                  Road := Road + byte(gTerrain.Land[pY, pX - 1].TileOverlay = to_Road) shl 3;
                ID := RoadsConnectivity[Road, 1];
                Rot := RoadsConnectivity[Road, 2];
                RenderTile(ID, pX, pY, Rot, DoHighlight, HighlightColor);
              end;
   end;

   //Fake tiles for MapEd fields
   case gTerrain.Land[pY, pX].CornOrWine of
     1: RenderTile(gTerrain.Land[pY, pX].CornOrWineTerrain, pX, pY, 0, DoHighlight, HighlightColor);
     2: RenderTile(55, pX, pY, 0, DoHighlight, HighlightColor);
   end;
end;


procedure TRenderTerrain.DoOverlays;
var
  I, K: Integer;
begin
  for I := fClipRect.Top to fClipRect.Bottom do
    for K := fClipRect.Left to fClipRect.Right do
      RenderTileOverlay(K, I);
end;


procedure TRenderTerrain.RenderFences;
var
  I,K: Integer;
begin
  with gTerrain do
  for I := fClipRect.Top to fClipRect.Bottom do
  for K := fClipRect.Left to fClipRect.Right do
  begin
    if Land[I,K].FenceSide and 1 = 1 then RenderFence(Land[I,K].Fence, dir_N, K, I);
    if Land[I,K].FenceSide and 2 = 2 then RenderFence(Land[I,K].Fence, dir_E, K, I);
    if Land[I,K].FenceSide and 4 = 4 then RenderFence(Land[I,K].Fence, dir_W, K, I);
    if Land[I,K].FenceSide and 8 = 8 then RenderFence(Land[I,K].Fence, dir_S, K, I);
  end;
end;


//Player markings should be always clearly visible to the player (thats why we render them ontop FOW)
procedure TRenderTerrain.RenderPlayerPlans(aFieldsList: TKMPointTagList; aHousePlansList: TKMPointDirList);
var
  I: Integer;
begin
  //Rope field marks
  for I := 0 to aFieldsList.Count - 1 do
    RenderMarkup(aFieldsList[I].X, aFieldsList[I].Y, TFieldType(aFieldsList.Tag[I]));

  //Rope outlines
  for I := 0 to aHousePlansList.Count - 1 do
    RenderFence(fncHousePlan, aHousePlansList[I].Dir, aHousePlansList[I].Loc.X, aHousePlansList[I].Loc.Y);
end;


procedure TRenderTerrain.DoLighting;
var
  I, K: Integer;
begin
  glColor4f(1, 1, 1, 1);
  //Render highlights
  glBlendFunc(GL_DST_COLOR, GL_ONE);
  TRender.BindTexture(fTextG);

  if fUseVBO then
  begin
    BindVBOArray(vat_Tile);
    //Setup vertex and UV layout and offsets
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_FLOAT, SizeOf(TTileVertice), Pointer(0));
    glClientActiveTexture(GL_TEXTURE0);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glTexCoordPointer(1, GL_FLOAT, SizeOf(TTileVertice), Pointer(20));

    //Here and above OGL requests Pointer, but in fact it's just a number (offset within Array)
    glDrawElements(GL_TRIANGLES, Length(fTilesInd), GL_UNSIGNED_INT, Pointer(0));

    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  end
  else
  begin
    with gTerrain do
    if RENDER_3D then
      for I := fClipRect.Top to fClipRect.Bottom do
      for K := fClipRect.Left to fClipRect.Right do
      begin
        glBegin(GL_TRIANGLE_FAN);
          glTexCoord1f(Land[  I,   K].Light); glVertex3f(K-1, I-1, -Land[  I,   K].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Land[I+1,   K].Light); glVertex3f(K-1,   I, -Land[I+1,   K].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Land[I+1, K+1].Light); glVertex3f(  K,   I, -Land[I+1, K+1].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Land[  I, K+1].Light); glVertex3f(  K, I-1, -Land[  I, K+1].Height / CELL_HEIGHT_DIV);
        glEnd;
      end
    else
      for I := fClipRect.Top to fClipRect.Bottom do
      for K := fClipRect.Left to fClipRect.Right do
      begin
        glBegin(GL_TRIANGLE_FAN);
          glTexCoord1f(Land[  I,   K].Light); glVertex3f(K-1, I-1 - Land[  I,   K].Height / CELL_HEIGHT_DIV, I-1);
          glTexCoord1f(Land[I+1,   K].Light); glVertex3f(K-1,   I - Land[I+1,   K].Height / CELL_HEIGHT_DIV, I-1);
          glTexCoord1f(Land[I+1, K+1].Light); glVertex3f(  K,   I - Land[I+1, K+1].Height / CELL_HEIGHT_DIV, I-1);
          glTexCoord1f(Land[  I, K+1].Light); glVertex3f(  K, I-1 - Land[  I, K+1].Height / CELL_HEIGHT_DIV, I-1);
        glEnd;
      end;
  end;
end;


//Render shadows and FOW at once
procedure TRenderTerrain.DoShadows;
var
  I,K: Integer;
begin
  glColor4f(1, 1, 1, 1);
  glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_COLOR);
  TRender.BindTexture(fTextG);

  if fUseVBO then
  begin
    BindVBOArray(vat_Tile);
    //Setup vertex and UV layout and offsets
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_FLOAT, SizeOf(TTileVertice), Pointer(0));
    glClientActiveTexture(GL_TEXTURE0);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glTexCoordPointer(1, GL_FLOAT, SizeOf(TTileVertice), Pointer(24));

    //Here and above OGL requests Pointer, but in fact it's just a number (offset within Array)
    glDrawElements(GL_TRIANGLES, Length(fTilesInd), GL_UNSIGNED_INT, Pointer(0));

    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  end
  else
  begin
    with gTerrain do
    if RENDER_3D then
      for I := fClipRect.Top to fClipRect.Bottom do
      for K := fClipRect.Left to fClipRect.Right do
      begin
        glBegin(GL_TRIANGLE_FAN);
          glTexCoord1f(-Land[I, K].Light);
          glVertex3f(K - 1, I - 1, -Land[I, K].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(-Land[I + 1, K].Light);
          glVertex3f(K - 1, I, -Land[I + 1, K].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(-Land[I + 1, K + 1].Light);
          glVertex3f(K, I, -Land[I + 1, K + 1].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(-Land[I, K + 1].Light);
          glVertex3f(K, I - 1, -Land[I, K + 1].Height / CELL_HEIGHT_DIV);
        glEnd;
      end
    else
      for I := fClipRect.Top to fClipRect.Bottom do
      for K := fClipRect.Left to fClipRect.Right do
      begin
        glBegin(GL_TRIANGLE_FAN);
          glTexCoord1f(-Land[I, K].Light);
          glVertex3f(K - 1, I - 1 - Land[I, K].Height / CELL_HEIGHT_DIV, I-1);
          glTexCoord1f(-Land[I + 1, K].Light);
          glVertex3f(K - 1, I - Land[I + 1, K].Height / CELL_HEIGHT_DIV, I-1);
          glTexCoord1f(-Land[I + 1, K + 1].Light);
          glVertex3f(K, I - Land[I + 1, K + 1].Height / CELL_HEIGHT_DIV, I-1);
          glTexCoord1f(-Land[I, K + 1].Light);
          glVertex3f(K, I - 1 - Land[I, K + 1].Height / CELL_HEIGHT_DIV, I-1);
        glEnd;
      end;
  end;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  TRender.BindTexture(0);
end;


//Render FOW at once
procedure TRenderTerrain.RenderFOW(aFOW: TKMFogOfWarCommon; aUseContrast: Boolean);
var
  I,K: Integer;
  Fog: PKMByte2Array;
begin
  if aFOW is TKMFogOfWarOpen then Exit;

  glColor4f(1, 1, 1, 1);

  if aUseContrast then
  begin
    //Hide everything behind FOW with a sharp transition
    glColor4f(0, 0, 0, 1);
    TRender.BindTexture(fTextB);
  end
  else
  begin
    glBlendFunc(GL_ZERO, GL_SRC_COLOR);
    TRender.BindTexture(fTextG);
  end;

  Fog := @TKMFogOfWar(aFOW).Revelation;
  if fUseVBO then
  begin
    BindVBOArray(vat_Tile);
    //Setup vertex and UV layout and offsets
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_FLOAT, SizeOf(TTileVertice), Pointer(0));
    glClientActiveTexture(GL_TEXTURE0);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glTexCoordPointer(1, GL_FLOAT, SizeOf(TTileVertice), Pointer(28));

    //Here and above OGL requests Pointer, but in fact it's just a number (offset within Array)
    glDrawElements(GL_TRIANGLES, Length(fTilesInd), GL_UNSIGNED_INT, Pointer(0));

    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  end
  else
  begin
    with gTerrain do
    if RENDER_3D then
      for I := fClipRect.Top to fClipRect.Bottom do
      for K := fClipRect.Left to fClipRect.Right do
      begin
        glBegin(GL_TRIANGLE_FAN);
          glTexCoord1f(Fog^[I - 1, K - 1] / 255);
          glVertex3f(K - 1, I - 1, -Land[I, K].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Fog^[I, K - 1] / 255);
          glVertex3f(K - 1, I, -Land[I + 1, K].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Fog^[I, K] / 255);
          glVertex3f(K, I, -Land[I + 1, K + 1].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Fog^[I - 1, K] / 255);
          glVertex3f(K, I - 1, -Land[I, K + 1].Height / CELL_HEIGHT_DIV);
        glEnd;
      end
    else
      for I := fClipRect.Top to fClipRect.Bottom do
      for K := fClipRect.Left to fClipRect.Right do
      begin
        glBegin(GL_TRIANGLE_FAN);
          glTexCoord1f(Fog^[I - 1, K - 1] / 255);
          glVertex2f(K - 1, I - 1 - Land[I, K].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Fog^[I, K - 1] / 255);
          glVertex2f(K - 1, I - Land[I + 1, K].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Fog^[I, K] / 255);
          glVertex2f(K, I - Land[I + 1, K + 1].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Fog^[I - 1, K] / 255);
          glVertex2f(K, I - 1 - Land[I, K + 1].Height / CELL_HEIGHT_DIV);
        glEnd;
      end;
  end;

  //Sprites (trees) can extend beyond the top edge of the map, so draw extra rows of fog to cover them
  //@Krom: If you know a neater/faster way to solve this problem please feel free to change it.
  //@Krom: Side note: Is it ok to not use VBOs in this case? (does mixing VBO with non-VBO code cause any problems?)
  with gTerrain do
    if fClipRect.Top <= 1 then
    begin
      //3 tiles is enough to cover the tallest tree with highest elevation on top row
      for I := -2 to 0 do
      for K := fClipRect.Left to fClipRect.Right do
      begin
        glBegin(GL_TRIANGLE_FAN);
          glTexCoord1f(Fog^[0, K - 1] / 255);
          glVertex2f(K - 1, I - 1 - Land[1, K].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Fog^[0, K - 1] / 255);
          glVertex2f(K - 1, I - Land[1, K].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Fog^[0, K] / 255);
          glVertex2f(K, I - Land[1, K + 1].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Fog^[0, K] / 255);
          glVertex2f(K, I - 1 - Land[1, K + 1].Height / CELL_HEIGHT_DIV);
        glEnd;
      end;
    end;
  //Similar thing for the bottom of the map (field borders can overhang)
  with gTerrain do
    if fClipRect.Bottom >= MapY-1 then
    begin
      //1 tile is enough to cover field borders
      for K := fClipRect.Left to fClipRect.Right do
      begin
        glBegin(GL_TRIANGLE_FAN);
          glTexCoord1f(Fog^[MapY-1, K - 1] / 255);
          glVertex2f(K - 1, MapY-1 - Land[MapY, K].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Fog^[MapY-1, K - 1] / 255);
          glVertex2f(K - 1, MapY - Land[MapY, K].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Fog^[MapY-1, K] / 255);
          glVertex2f(K, MapY-1 - Land[MapY, K + 1].Height / CELL_HEIGHT_DIV);
          glTexCoord1f(Fog^[MapY-1, K] / 255);
          glVertex2f(K, MapY - Land[MapY, K + 1].Height / CELL_HEIGHT_DIV);
        glEnd;
      end;
    end;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  TRender.BindTexture(0);
end;


procedure TRenderTerrain.BindVBOArray(aVBOArrayType: TVBOArrayType);
begin
  if fLastBindVBOArrayType = aVBOArrayType then Exit; // Do not to rebind for same tyle type

  case aVBOArrayType of
    vat_Tile:      begin
                    glBindBuffer(GL_ARRAY_BUFFER, fVtxTilesShd);
                    glBufferData(GL_ARRAY_BUFFER, Length(fTilesVtx) * SizeOf(TTileVertice), @fTilesVtx[0].X, GL_STREAM_DRAW);

                    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, fIndTilesShd);
                    glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(fTilesInd) * SizeOf(fTilesInd[0]), @fTilesInd[0], GL_STREAM_DRAW);
                  end;
    vat_AnimTile: if Length(fAnimTilesVtx) > 0 then
                  begin
                    glBindBuffer(GL_ARRAY_BUFFER, fVtxAnimTilesShd);
                    glBufferData(GL_ARRAY_BUFFER, Length(fAnimTilesVtx) * SizeOf(TAnimTileVertice), @fAnimTilesVtx[0].X, GL_STREAM_DRAW);

                    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, fIndAnimTilesShd);
                    glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(fAnimTilesInd) * SizeOf(fAnimTilesInd[0]), @fAnimTilesInd[0], GL_STREAM_DRAW);
                  end;
  end;
  fLastBindVBOArrayType := aVBOArrayType;
end;


//AnimStep - animation step for terrain (water/etc)
//aFOW - whose players FOW to apply
procedure TRenderTerrain.RenderBase(aAnimStep: Integer; aFOW: TKMFogOfWarCommon);
begin
  //VBO has proper vertice coords only for Light/Shadow
  //it cant handle 3D yet and because of FOW leaves terrain revealed, which is an exploit in MP
  //Thus we allow VBO only in 2D
  fUseVBO := VBOSupported and not RENDER_3D;

  UpdateVBO(aAnimStep, aFOW);

  DoTiles;
  DoOverlays;
  DoLighting;
  DoWater(aAnimStep, aFOW); //Unlit water goes above lit sand
  DoShadows;
end;


//Render single terrain cell
procedure TRenderTerrain.RenderTile(Index: Byte; pX,pY,Rot: Integer; DoHighlight: Boolean = False; HighlightColor: Cardinal = 0);
var
  K, I: Integer;
  TexC: TUVRect; // Texture UV coordinates
begin
  if not gTerrain.TileInMapCoords(pX,pY) then Exit;

  K := pX;
  I := pY;
  if DoHighlight then
    glColor4ub(HighlightColor AND $FF, HighlightColor SHR 8 AND $FF, HighlightColor SHR 16 AND $FF, $FF)
  else
    glColor4f(1, 1, 1, 1);

  TRender.BindTexture(GFXData[rxTiles, Index + 1].Tex.ID);
  TexC := fTileUVLookup[Index, Rot mod 4];

  glBegin(GL_TRIANGLE_FAN);
    with gTerrain do
    if RENDER_3D then
    begin
      glTexCoord2fv(@TexC[1]); glVertex3f(K-1,I-1,-Land[I,K].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[2]); glVertex3f(K-1,I  ,-Land[I+1,K].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[3]); glVertex3f(K  ,I  ,-Land[I+1,K+1].Height/CELL_HEIGHT_DIV);
      glTexCoord2fv(@TexC[4]); glVertex3f(K  ,I-1,-Land[I,K+1].Height/CELL_HEIGHT_DIV);
    end
    else
    begin
      glTexCoord2fv(@TexC[1]); glVertex3f(K-1,I-1-Land[I,K].Height/CELL_HEIGHT_DIV, I-1);
      glTexCoord2fv(@TexC[2]); glVertex3f(K-1,I  -Land[I+1,K].Height/CELL_HEIGHT_DIV, I-1);
      glTexCoord2fv(@TexC[3]); glVertex3f(K  ,I  -Land[I+1,K+1].Height/CELL_HEIGHT_DIV, I-1);
      glTexCoord2fv(@TexC[4]); glVertex3f(K  ,I-1-Land[I,K+1].Height/CELL_HEIGHT_DIV, I-1);
    end;
  glEnd;
end;


procedure TRenderTerrain.RenderFence(aFence: TFenceType; Pos: TKMDirection; pX,pY: Integer);
const
  FO = 4; //Fence overlap
  VO = -4; //Move fences a little down to avoid visible overlap when unit stands behind fence, but is rendered ontop of it, due to Z sorting algo we use
var
  UVa, UVb: TKMPointF;
  TexID: Integer;
  x1,y1,y2,FenceX, FenceY: Single;
  HeightInPx: Integer;
begin
  case aFence of
    fncHouseFence: if Pos in [dir_N,dir_S] then TexID:=463 else TexID:=467; //WIP (Wood planks)
    fncHousePlan:  if Pos in [dir_N,dir_S] then TexID:=105 else TexID:=117; //Plan (Ropes)
    fncWine:       if Pos in [dir_N,dir_S] then TexID:=462 else TexID:=466; //Fence (Wood)
    fncCorn:       if Pos in [dir_N,dir_S] then TexID:=461 else TexID:=465; //Fence (Stones)
    else          TexID := 0;
  end;

  //With these directions render fences on next tile
  if Pos = dir_S then Inc(pY);
  if Pos = dir_W then Inc(pX);

  if Pos in [dir_N, dir_S] then
  begin //Horizontal
    TRender.BindTexture(GFXData[rxGui,TexID].Tex.ID);
    UVa.X := GFXData[rxGui, TexID].Tex.u1;
    UVa.Y := GFXData[rxGui, TexID].Tex.v1;
    UVb.X := GFXData[rxGui, TexID].Tex.u2;
    UVb.Y := GFXData[rxGui, TexID].Tex.v2;

    y1 := pY - 1 - (gTerrain.Land[pY, pX].Height + VO) / CELL_HEIGHT_DIV;
    y2 := pY - 1 - (gTerrain.Land[pY, pX + 1].Height + VO) / CELL_HEIGHT_DIV;

    FenceY := GFXData[rxGui,TexID].PxWidth / CELL_SIZE_PX;
    glBegin(GL_QUADS);
      glTexCoord2f(UVb.x, UVa.y); glVertex2f(pX-1 -3/ CELL_SIZE_PX, y1);
      glTexCoord2f(UVa.x, UVa.y); glVertex2f(pX-1 -3/ CELL_SIZE_PX, y1 - FenceY);
      glTexCoord2f(UVa.x, UVb.y); glVertex2f(pX   +3/ CELL_SIZE_PX, y2 - FenceY);
      glTexCoord2f(UVb.x, UVb.y); glVertex2f(pX   +3/ CELL_SIZE_PX, y2);
    glEnd;
  end
  else
  begin //Vertical
    TRender.BindTexture(GFXData[rxGui,TexID].Tex.ID);
    HeightInPx := Round(CELL_SIZE_PX * (1 + (gTerrain.Land[pY,pX].Height - gTerrain.Land[pY+1,pX].Height)/CELL_HEIGHT_DIV)+FO);
    UVa.X := GFXData[rxGui, TexID].Tex.u1;
    UVa.Y := GFXData[rxGui, TexID].Tex.v1;
    UVb.X := GFXData[rxGui, TexID].Tex.u2;
    UVb.Y := Mix(GFXData[rxGui, TexID].Tex.v2, GFXData[rxGui, TexID].Tex.v1, HeightInPx / GFXData[rxGui, TexID].pxHeight);

    y1 := pY - 1 - (gTerrain.Land[pY, pX].Height + FO + VO) / CELL_HEIGHT_DIV;
    y2 := pY - (gTerrain.Land[pY + 1, pX].Height + VO) / CELL_HEIGHT_DIV;

    FenceX := GFXData[rxGui,TexID].PxWidth / CELL_SIZE_PX;

    case Pos of
      dir_W:  x1 := pX - 1 - 3 / CELL_SIZE_PX;
      dir_E:  x1 := pX - 1 + 3 / CELL_SIZE_PX - FenceX;
      else    x1 := pX - 1; //Should never happen
    end;

    glBegin(GL_QUADS);
      glTexCoord2f(UVa.x, UVa.y); glVertex2f(x1, y1);
      glTexCoord2f(UVb.x, UVa.y); glVertex2f(x1+ FenceX, y1);
      glTexCoord2f(UVb.x, UVb.y); glVertex2f(x1+ FenceX, y2);
      glTexCoord2f(UVa.x, UVb.y); glVertex2f(x1, y2);
    glEnd;
  end;
end;


procedure TRenderTerrain.RenderMarkup(pX, pY: Word; aFieldType: TFieldType);
const
  MarkupTex: array [TFieldType] of Word = (0, 105, 107, 0, 108);
var
  ID: Integer;
  UVa,UVb: TKMPointF;
begin
  ID := MarkupTex[aFieldType];

  TRender.BindTexture(GFXData[rxGui, ID].Tex.ID);

  UVa.X := GFXData[rxGui, ID].Tex.u1;
  UVa.Y := GFXData[rxGui, ID].Tex.v1;
  UVb.X := GFXData[rxGui, ID].Tex.u2;
  UVb.Y := GFXData[rxGui, ID].Tex.v2;

  glBegin(GL_QUADS);
    glTexCoord2f(UVb.x, UVa.y); glVertex2f(pX-1, pY-1 - gTerrain.Land[pY  ,pX  ].Height/CELL_HEIGHT_DIV+0.10);
    glTexCoord2f(UVa.x, UVa.y); glVertex2f(pX-1, pY-1 - gTerrain.Land[pY  ,pX  ].Height/CELL_HEIGHT_DIV-0.15);
    glTexCoord2f(UVa.x, UVb.y); glVertex2f(pX  , pY   - gTerrain.Land[pY+1,pX+1].Height/CELL_HEIGHT_DIV-0.25);
    glTexCoord2f(UVb.x, UVb.y); glVertex2f(pX  , pY   - gTerrain.Land[pY+1,pX+1].Height/CELL_HEIGHT_DIV);

    glTexCoord2f(UVb.x, UVa.y); glVertex2f(pX-1, pY   - gTerrain.Land[pY+1,pX  ].Height/CELL_HEIGHT_DIV);
    glTexCoord2f(UVa.x, UVa.y); glVertex2f(pX-1, pY   - gTerrain.Land[pY+1,pX  ].Height/CELL_HEIGHT_DIV-0.25);
    glTexCoord2f(UVa.x, UVb.y); glVertex2f(pX  , pY-1 - gTerrain.Land[pY  ,pX+1].Height/CELL_HEIGHT_DIV-0.15);
    glTexCoord2f(UVb.x, UVb.y); glVertex2f(pX  , pY-1 - gTerrain.Land[pY  ,pX+1].Height/CELL_HEIGHT_DIV+0.10);
  glEnd;
end;


end.
