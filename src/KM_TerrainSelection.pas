unit KM_TerrainSelection;
{$I KaM_Remake.inc}
interface
uses Classes, Math, Clipbrd, KromUtils,
  {$IFDEF MSWindows} Windows, {$ENDIF}
  KM_CommonClasses, KM_Points, KM_Terrain, KM_TerrainPainter, KM_RenderPool;


type
  TKMSelectionEdit = (seNone, seNewRect, seResizeX1, seResizeY1, seResizeX2, seResizeY2, seMove);
  TKMSelectionMode = (smSelecting, smPasting);
  TKMFlipAxis = (fa_Horizontal, fa_Vertical);

  TKMBufferData = record
                    Terrain: Byte;
                    Height: Byte;
                    Rotation: Byte;
                    Obj: Byte;
                    TerKind: TKMTerrainKind; //Used for brushes
                  end;

  TKMSelection = class
  private
    fTerrainPainter: TKMTerrainPainter;
    fSelectionEdit: TKMSelectionEdit;
    fSelPrevX, fSelPrevY: Integer;

    fSelectionRectF: TKMRectF; //Cursor selection bounds (can have inverted bounds)
    fSelectionRect: TKMRect; //Tile-space selection, at least 1 tile
    fSelectionMode: TKMSelectionMode;
    fSelectionBuffer: array of array of TKMBufferData;
    procedure Selection_SyncCellRect;
  public
    constructor Create(aTerrainPainter: TKMTerrainPainter);
    procedure Selection_Resize;
    procedure Selection_Start;
    function Selection_DataInBuffer: Boolean;
    procedure Selection_Copy; //Copies the selected are into buffer
    procedure Selection_PasteBegin; //Pastes the area from buffer and lets move it with cursor
    procedure Selection_PasteApply; //Do the actual paste from buffer to terrain
    procedure Selection_PasteCancel;
    procedure Selection_Flip(aAxis: TKMFlipAxis);

    function TileWithinPastePreview(aX, aY: Word): Boolean;
    procedure Paint(aLayer: TKMPaintLayer; aClipRect: TKMRect);
  end;


var
  CF_MAPDATA: Word; //Our own custom clipboard format


implementation
uses KM_GameCursor, KM_RenderAux;


{ TKMSelection }
constructor TKMSelection.Create(aTerrainPainter: TKMTerrainPainter);
begin
  inherited Create;

  fTerrainPainter := aTerrainPainter;
end;


procedure TKMSelection.Selection_SyncCellRect;
begin
  //Convert RawRect values that can be inverted to tilespace Rect
  fSelectionRect.Left   := Trunc(Math.Min(fSelectionRectF.Left, fSelectionRectF.Right));
  fSelectionRect.Top    := Trunc(Math.Min(fSelectionRectF.Top, fSelectionRectF.Bottom));
  fSelectionRect.Right  := Ceil(Math.Max(fSelectionRectF.Left, fSelectionRectF.Right));
  fSelectionRect.Bottom := Ceil(Math.Max(fSelectionRectF.Top, fSelectionRectF.Bottom));
  //Selection must be at least one tile
  if fSelectionRect.Left = fSelectionRect.Right then Inc(fSelectionRect.Right);
  if fSelectionRect.Top = fSelectionRect.Bottom then Inc(fSelectionRect.Bottom);
end;


procedure TKMSelection.Selection_Resize;
var
  RectO: TKMRect;
  CursorFloat: TKMPointF;
  CursorCell: TKMPoint;
  MoveX, MoveY: Integer;
begin
  //Last row/col of the map is not visible or selectable
  CursorFloat.X := EnsureRange(GameCursor.Float.X, 0.1, gTerrain.MapX-1 - 0.1);
  CursorFloat.Y := EnsureRange(GameCursor.Float.Y, 0.1, gTerrain.MapY-1 - 0.1);
  CursorCell.X := EnsureRange(GameCursor.Cell.X, 1, gTerrain.MapX-1);
  CursorCell.Y := EnsureRange(GameCursor.Cell.Y, 1, gTerrain.MapY-1);

  case fSelectionEdit of
    seNone:       ;
    seNewRect:    begin
                    fSelectionRectF.Right := CursorFloat.X;
                    fSelectionRectF.Bottom := CursorFloat.Y;
                  end;
    seResizeX1:   fSelectionRectF.Left := CursorFloat.X;
    seResizeY1:   fSelectionRectF.Top := CursorFloat.Y;
    seResizeX2:   fSelectionRectF.Right := CursorFloat.X;
    seResizeY2:   fSelectionRectF.Bottom := CursorFloat.Y;
    seMove:       begin
                    MoveX := CursorCell.X - fSelPrevX;
                    MoveY := CursorCell.Y - fSelPrevY;
                    //Don't allow the selection to be moved out of the map bounds
                    MoveX := EnsureRange(MoveX, -fSelectionRect.Left, gTerrain.MapX-1-fSelectionRect.Right);
                    MoveY := EnsureRange(MoveY, -fSelectionRect.Top, gTerrain.MapY-1-fSelectionRect.Bottom);
                    RectO := KMRectMove(fSelectionRect, MoveX, MoveY);
                    fSelectionRectF := KMRectF(RectO);

                    fSelPrevX := CursorCell.X;
                    fSelPrevY := CursorCell.Y;
                  end;
  end;

  Selection_SyncCellRect;
end;


procedure TKMSelection.Selection_Start;
const
  EDGE = 0.25;
var
  CursorFloat: TKMPointF;
  CursorCell: TKMPoint;
begin
  //Last row/col of the map is not visible or selectable
  CursorFloat.X := EnsureRange(GameCursor.Float.X, 0.1, gTerrain.MapX-1 - 0.1);
  CursorFloat.Y := EnsureRange(GameCursor.Float.Y, 0.1, gTerrain.MapY-1 - 0.1);
  CursorCell.X := EnsureRange(GameCursor.Cell.X, 1, gTerrain.MapX-1);
  CursorCell.Y := EnsureRange(GameCursor.Cell.Y, 1, gTerrain.MapY-1);

  if fSelectionMode = smSelecting then
  begin
    if InRange(CursorFloat.Y, fSelectionRect.Top, fSelectionRect.Bottom)
    and (Abs(CursorFloat.X - fSelectionRect.Left) < EDGE) then
      fSelectionEdit := seResizeX1
    else
    if InRange(CursorFloat.Y, fSelectionRect.Top, fSelectionRect.Bottom)
    and (Abs(CursorFloat.X - fSelectionRect.Right) < EDGE) then
      fSelectionEdit := seResizeX2
    else
    if InRange(CursorFloat.X, fSelectionRect.Left, fSelectionRect.Right)
    and (Abs(CursorFloat.Y - fSelectionRect.Top) < EDGE) then
      fSelectionEdit := seResizeY1
    else
    if InRange(CursorFloat.X, fSelectionRect.Left, fSelectionRect.Right)
    and (Abs(CursorFloat.Y - fSelectionRect.Bottom) < EDGE) then
      fSelectionEdit := seResizeY2
    else
    if KMInRect(CursorFloat, fSelectionRect) then
    begin
      fSelectionEdit := seMove;
      fSelPrevX := CursorCell.X;
      fSelPrevY := CursorCell.Y;
    end
    else
    begin
      fSelectionEdit := seNewRect;
      fSelectionRectF := KMRectF(CursorFloat);
      Selection_SyncCellRect;
    end;
  end
  else
  begin
    if KMInRect(CursorFloat, fSelectionRect) then
    begin
      fSelectionEdit := seMove;
      //Grab and move
      fSelPrevX := CursorCell.X;
      fSelPrevY := CursorCell.Y;
    end
    else
    begin
      fSelectionEdit := seMove;
      //Selection edge will jump to under cursor
      fSelPrevX := EnsureRange(CursorCell.X, fSelectionRect.Left + 1, fSelectionRect.Right);
      fSelPrevY := EnsureRange(CursorCell.Y, fSelectionRect.Top + 1, fSelectionRect.Bottom);
    end;
  end;
end;


function TKMSelection.Selection_DataInBuffer: Boolean;
begin
  Result := Clipboard.HasFormat(CF_MAPDATA);
end;


//Copy terrain section into buffer
procedure TKMSelection.Selection_Copy;
var
  I, K: Integer;
  Sx, Sy: Word;
  Bx, By: Word;
  hMem: THandle;
  BufPtr: Pointer;
  BufferStream: TKMemoryStream;
begin
  Sx := fSelectionRect.Right - fSelectionRect.Left;
  Sy := fSelectionRect.Bottom - fSelectionRect.Top;
  SetLength(fSelectionBuffer, Sy, Sx);

  BufferStream := TKMemoryStream.Create;
  BufferStream.Write(Sx);
  BufferStream.Write(Sy);

  for I := fSelectionRect.Top to fSelectionRect.Bottom - 1 do
  for K := fSelectionRect.Left to fSelectionRect.Right - 1 do
  if gTerrain.TileInMapCoords(K+1, I+1, 0) then
  begin
    Bx := K - fSelectionRect.Left;
    By := I - fSelectionRect.Top;
    fSelectionBuffer[By,Bx].Terrain   := gTerrain.Land[I+1, K+1].Terrain;
    fSelectionBuffer[By,Bx].Height    := gTerrain.Land[I+1, K+1].Height;
    fSelectionBuffer[By,Bx].Rotation  := gTerrain.Land[I+1, K+1].Rotation;
    fSelectionBuffer[By,Bx].Obj       := gTerrain.Land[I+1, K+1].Obj;
    fSelectionBuffer[By,Bx].TerKind   := fTerrainPainter.Land2[I+1, K+1].TerKind;

    BufferStream.Write(fSelectionBuffer[By,Bx], SizeOf(fSelectionBuffer[By,Bx]));
  end;

  if Sx*Sy <> 0 then
  begin
    {$IFDEF WDC}
    hMem := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, BufferStream.Size);
    BufPtr := GlobalLock(hMem);
    Move(BufferStream.Memory^, BufPtr^, BufferStream.Size);
    Clipboard.SetAsHandle(CF_MAPDATA, hMem);
    GlobalUnlock(hMem);
    {$ENDIF}
    {$IFDEF FPC}
    Clipboard.SetFormat(CF_MAPDATA, BufferStream);
    {$ENDIF}
  end;
  BufferStream.Free;
end;


procedure TKMSelection.Selection_PasteBegin;
var
  I, K: Integer;
  Sx, Sy: Word;
  hMem: THandle;
  BufPtr: Pointer;
  BufferStream: TKMemoryStream;
begin
  BufferStream := TKMemoryStream.Create;
  {$IFDEF WDC}
  hMem := Clipboard.GetAsHandle(CF_MAPDATA);
  if hMem = 0 then Exit;
  BufPtr := GlobalLock(hMem);
  if BufPtr = nil then Exit;
  BufferStream.WriteBuffer(BufPtr^, GlobalSize(hMem));
  GlobalUnlock(hMem);
  {$ENDIF}
  {$IFDEF FPC}
  if not Clipboard.GetFormat(CF_MAPDATA, BufferStream) then Exit;
  {$ENDIF}
  BufferStream.Position := 0;
  BufferStream.Read(Sx);
  BufferStream.Read(Sy);
  SetLength(fSelectionBuffer, Sy, Sx);
  for I:=0 to Sy-1 do
    for K:=0 to Sx-1 do
      BufferStream.Read(fSelectionBuffer[I,K], SizeOf(fSelectionBuffer[I,K]));
  BufferStream.Free;

  //Mapmaker could have changed selection rect, sync it with Buffer size
  fSelectionRect.Right := fSelectionRect.Left + Length(fSelectionBuffer[0]);
  fSelectionRect.Bottom := fSelectionRect.Top + Length(fSelectionBuffer);

  fSelectionMode := smPasting;
end;


procedure TKMSelection.Selection_PasteApply;
var
  I, K: Integer;
  Bx, By: Word;
begin
  for I := fSelectionRect.Top to fSelectionRect.Bottom - 1 do
  for K := fSelectionRect.Left to fSelectionRect.Right - 1 do
  if gTerrain.TileInMapCoords(K+1, I+1, 0) then
  begin
    Bx := K - fSelectionRect.Left;
    By := I - fSelectionRect.Top;
    gTerrain.Land[I+1, K+1].Terrain     := fSelectionBuffer[By,Bx].Terrain;
    gTerrain.Land[I+1, K+1].Height      := fSelectionBuffer[By,Bx].Height;
    gTerrain.Land[I+1, K+1].Rotation    := fSelectionBuffer[By,Bx].Rotation;
    gTerrain.Land[I+1, K+1].Obj         := fSelectionBuffer[By,Bx].Obj;
    fTerrainPainter.Land2[I+1, K+1].TerKind := fSelectionBuffer[By,Bx].TerKind;
  end;

  gTerrain.UpdateLighting(fSelectionRect);
  gTerrain.UpdatePassability(fSelectionRect);

  fSelectionMode := smSelecting;
end;


procedure TKMSelection.Selection_PasteCancel;
begin
  fSelectionMode := smSelecting;
end;


procedure TKMSelection.Selection_Flip(aAxis: TKMFlipAxis);

  procedure SwapTiles(X1, Y1, X2, Y2: Word);
  var Tmp: TKMTerrainKind;
  begin
    SwapInt(gTerrain.Land[Y1,X1].Terrain, gTerrain.Land[Y2,X2].Terrain);
    //Heights are vertex based not tile based, so it gets flipped slightly differently
    case aAxis of
      fa_Horizontal: SwapInt(gTerrain.Land[Y1,X1].Height, gTerrain.Land[Y2  ,X2+1].Height);
      fa_Vertical:   SwapInt(gTerrain.Land[Y1,X1].Height, gTerrain.Land[Y2+1,X2  ].Height);
    end;
    SwapInt(gTerrain.Land[Y1,X1].Rotation, gTerrain.Land[Y2,X2].Rotation);
    SwapInt(gTerrain.Land[Y1,X1].Obj, gTerrain.Land[Y2,X2].Obj);
    Tmp := fTerrainPainter.Land2[Y1, X1].TerKind;
    fTerrainPainter.Land2[Y1, X1].TerKind := fTerrainPainter.Land2[Y2, X2].TerKind;
    fTerrainPainter.Land2[Y2, X2].TerKind := Tmp;
  end;

  procedure FixTerrain(X, Y: Integer);
  const
    CORNERS = [10,15,18,21..23,25,38,49,51..54,56,58,65,66,68..69,71,72,74,78,80,81,83,84,86..87,89,90,92,93,95,96,98,99,101,102,104,105,107..108,110..111,113,114,116,118,119,120,122,123,126..127,138,142,143,165,176..193,196,202,203,205,213,220,234..241,243,247];
    CORNERS_REVERSED = [15,21,142,234,235,238];
    EDGES = [4,12,19,39,50,57,64,67,70,73,76,79,82,85,88,91,94,97,100,103,106,109,112,115,117,121,124..125,139,141,166..175,194,198..200,204,206..212,216..219,223,224..233,242,244];
    OBJ_MIDDLE_X = [8,9,54..61,80,81,212,213,215];
    OBJ_MIDDLE_Y = [8,9,54..61,80,81,212,213,215,  1..5,10..12,17..19,21..24,63,126,210,211,249..253];
  var
    Ter, Rot: Byte;
  begin
    Ter := gTerrain.Land[Y,X].Terrain;
    Rot := gTerrain.Land[Y,X].Rotation mod 4; //Some KaM maps contain rotations > 3 which must be fixed by modding

    //Edges
    if (Ter in EDGES) and ((Rot in [1,3]) xor (aAxis = fa_Vertical)) then
      gTerrain.Land[Y,X].Rotation := (Rot+2) mod 4;

    //Corners
    if Ter in CORNERS then
    begin
      if (Rot in [1,3]) xor (Ter in CORNERS_REVERSED) xor (aAxis = fa_Vertical) then
        gTerrain.Land[Y,X].Rotation := (Rot+1) mod 4
      else
        gTerrain.Land[Y,X].Rotation := (Rot+3) mod 4;
    end;

    //Horizontal flip: Vertex (not middle) objects must be moved right by 1
    if (aAxis = fa_Horizontal) and (X < fSelectionRect.Right)
    and (gTerrain.Land[Y,X+1].Obj = 255) and not (gTerrain.Land[Y,X].Obj in OBJ_MIDDLE_X) then
    begin
      gTerrain.Land[Y,X+1].Obj := gTerrain.Land[Y,X].Obj;
      gTerrain.Land[Y,X].Obj := 255;
    end;

    //Vertical flip: Vertex (not middle) objects must be moved down by 1
    if (aAxis = fa_Vertical) and (Y < fSelectionRect.Bottom)
    and (gTerrain.Land[Y+1,X].Obj = 255) and not (gTerrain.Land[Y,X].Obj in OBJ_MIDDLE_Y) then
    begin
      gTerrain.Land[Y+1,X].Obj := gTerrain.Land[Y,X].Obj;
      gTerrain.Land[Y,X].Obj := 255;
    end;
  end;

var
  I,K: Integer;
  SX, SY: Word;
begin
  SX := (fSelectionRect.Right - fSelectionRect.Left);
  SY := (fSelectionRect.Bottom - fSelectionRect.Top);

  case aAxis of
    fa_Horizontal:  for I := 1 to SY do
                    for K := 1 to SX div 2 do
                      SwapTiles(fSelectionRect.Left + K, fSelectionRect.Top + I,
                                fSelectionRect.Right - K + 1, fSelectionRect.Top + I);
    fa_Vertical:    for I := 1 to SY div 2 do
                    for K := 1 to SX do
                      SwapTiles(fSelectionRect.Left + K, fSelectionRect.Top + I,
                                fSelectionRect.Left + K, fSelectionRect.Bottom - I + 1);
  end;

  //Must loop backwards for object fixing
  for I := SY downto 1 do
  for K := SX downto 1 do
    FixTerrain(fSelectionRect.Left + K, fSelectionRect.Top + I);

  gTerrain.UpdateLighting(fSelectionRect);
  gTerrain.UpdatePassability(fSelectionRect);
end;


function TKMSelection.TileWithinPastePreview(aX, aY: Word): Boolean;
begin
  Result := (fSelectionMode = smPasting) and KMInRect(KMPoint(aX, aY), KMRectShinkTopLeft(fSelectionRect));
end;


procedure TKMSelection.Paint(aLayer: TKMPaintLayer; aClipRect: TKMRect);
var
  Sx, Sy: Word;
  I, K: Integer;
begin
  Sx := fSelectionRect.Right - fSelectionRect.Left;
  Sy := fSelectionRect.Bottom - fSelectionRect.Top;

  if aLayer = plTerrain then
    case fSelectionMode of
      smSelecting:  begin
                      //fRenderAux.SquareOnTerrain(RawRect.Left, RawRect.Top, RawRect.Right, RawRect.Bottom, $40FFFF00);
                      gRenderAux.SquareOnTerrain(fSelectionRect.Left, fSelectionRect.Top, fSelectionRect.Right, fSelectionRect.Bottom, $FFFFFF00);
                    end;
      smPasting:    begin
                      for I := 0 to Sy - 1 do
                      for K := 0 to Sx - 1 do
                       //Check TileInMapCoords first since KMInRect can't handle negative coordinates
                      if gTerrain.TileInMapCoords(fSelectionRect.Left+K+1, fSelectionRect.Top+I+1)
                      and KMInRect(KMPoint(fSelectionRect.Left+K+1, fSelectionRect.Top+I+1), aClipRect) then
                        fRenderPool.RenderTerrain.RenderTile(fSelectionBuffer[I,K].Terrain, fSelectionRect.Left+K+1, fSelectionRect.Top+I+1, fSelectionBuffer[I,K].Rotation);

                      gRenderAux.SquareOnTerrain(fSelectionRect.Left, fSelectionRect.Top, fSelectionRect.Right, fSelectionRect.Bottom, $FF0000FF);
                    end;
    end;

  if aLayer = plObjects then
    if fSelectionMode = smPasting then
    begin
      for I := 0 to Sy - 1 do
      for K := 0 to Sx - 1 do
        //Check TileInMapCoords first since KMInRect can't handle negative coordinates
        if (fSelectionBuffer[I,K].Obj <> 255) and gTerrain.TileInMapCoords(fSelectionRect.Left+K+1, fSelectionRect.Top+I+1)
        and KMInRect(KMPoint(fSelectionRect.Left+K+1, fSelectionRect.Top+I+1), aClipRect) then
          fRenderPool.RenderMapElement(fSelectionBuffer[I,K].Obj, 0, fSelectionRect.Left+K+1, fSelectionRect.Top+I+1, True);
    end;
end;


initialization
  CF_MAPDATA := RegisterClipboardFormat('KaM Remake Map Data');


end.
