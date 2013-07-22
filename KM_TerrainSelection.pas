unit KM_TerrainSelection;
{$I KaM_Remake.inc}
interface
uses Classes, Math,
  KM_Points, KM_Terrain, KM_TerrainPainter;


type
  TSelectionManipulation = (smNone, smNewRect, smResizeX1, smResizeY1, smResizeX2, smResizeY2, smMove);
  TKMSelectionMode = (smSelecting, smPasting);
  TKMSelection = class
  private
    fSelectionMan: TSelectionManipulation;
    fPrevX, fPrevY: Integer;

    fRawRect: TKMRectF; //Cursor selection bounds (can have inverted bounds)
    fCellRect: TKMRect; //Tile-space selection, at least 1 tile
    fSelectionMode: TKMSelectionMode;
    fBuffer: array of array of record
      Terrain: Byte;
      Height: Byte;
      Rotation: Byte;
      Obj: Byte;
      TerrainKind: TTerrainKind; //Used for brushes
    end;
    procedure SyncCellRect;
  public
    procedure Selection_Resize;
    procedure Selection_Start;
    function IsBufferHasData: Boolean;
    procedure Copy; //Copies the selected are into buffer
    procedure PasteBegin; //Pastes the area from buffer and lets move it with cursor
    procedure PasteApply; //Do the actual paste from buffer to terrain
    procedure PasteCancel;
    //procedure Transform; //Transforms the buffer data ?
    procedure Paint;
  end;


implementation
uses KM_GameCursor, KM_RenderAux, KM_RenderPool;


{ TKMSelection }
procedure TKMSelection.SyncCellRect;
begin
  //Convert RawRect values that can be inverted to tilespace Rect
  fCellRect.Left   := Trunc(Min(fRawRect.Left, fRawRect.Right));
  fCellRect.Top    := Trunc(Min(fRawRect.Top, fRawRect.Bottom));
  fCellRect.Right  := Ceil(Max(fRawRect.Left, fRawRect.Right));
  fCellRect.Bottom := Ceil(Max(fRawRect.Top, fRawRect.Bottom));
end;


procedure TKMSelection.Selection_Resize;
var
  RectO: TKMRect;
begin
  case fSelectionMan of
    smNone:       ;
    smNewRect:    begin
                    fRawRect.Right := GameCursor.Float.X;
                    fRawRect.Bottom := GameCursor.Float.Y;
                  end;
    smResizeX1:   fRawRect.Left := GameCursor.Float.X;
    smResizeY1:   fRawRect.Top := GameCursor.Float.Y;
    smResizeX2:   fRawRect.Right := GameCursor.Float.X;
    smResizeY2:   fRawRect.Bottom := GameCursor.Float.Y;
    smMove:       begin
                    RectO := KMRectMove(fCellRect, GameCursor.Cell.X - fPrevX, GameCursor.Cell.Y - fPrevY);
                    fRawRect := KMRectF(RectO);

                    fPrevX := GameCursor.Cell.X;
                    fPrevY := GameCursor.Cell.Y;
                  end;
  end;

  SyncCellRect;
end;


procedure TKMSelection.Selection_Start;
const
  EDGE = 0.25;
begin
  if fSelectionMode = smSelecting then
  begin
    if InRange(GameCursor.Float.Y, fCellRect.Top, fCellRect.Bottom)
    and (Abs(GameCursor.Float.X - fCellRect.Left) < EDGE) then
      fSelectionMan := smResizeX1
    else
    if InRange(GameCursor.Float.Y, fCellRect.Top, fCellRect.Bottom)
    and (Abs(GameCursor.Float.X - fCellRect.Right) < EDGE) then
      fSelectionMan := smResizeX2
    else
    if InRange(GameCursor.Float.X, fCellRect.Left, fCellRect.Right)
    and (Abs(GameCursor.Float.Y - fCellRect.Top) < EDGE) then
      fSelectionMan := smResizeY1
    else
    if InRange(GameCursor.Float.X, fCellRect.Left, fCellRect.Right)
    and (Abs(GameCursor.Float.Y - fCellRect.Bottom) < EDGE) then
      fSelectionMan := smResizeY2
    else
    if KMInRect(GameCursor.Float, fCellRect) then
    begin
      fSelectionMan := smMove;
      fPrevX := GameCursor.Cell.X;
      fPrevY := GameCursor.Cell.Y;
    end
    else
    begin
      fSelectionMan := smNewRect;
      fRawRect := KMRectF(GameCursor.Float);
      SyncCellRect;
    end;
  end
  else
  begin
    if KMInRect(GameCursor.Float, fCellRect) then
    begin
      fSelectionMan := smMove;
      //Grab and move
      fPrevX := GameCursor.Cell.X;
      fPrevY := GameCursor.Cell.Y;
    end
    else
    begin
      fSelectionMan := smMove;
      //Selection edge will jump to under cursor
      fPrevX := EnsureRange(GameCursor.Cell.X, fCellRect.Left + 1, fCellRect.Right);
      fPrevY := EnsureRange(GameCursor.Cell.Y, fCellRect.Top + 1, fCellRect.Bottom);
    end;
  end;
end;


function TKMSelection.IsBufferHasData: Boolean;
begin
  Result := Length(fBuffer) > 0;
end;


//Copy terrain section into buffer
procedure TKMSelection.Copy;
var
  I, K: Integer;
  Sx, Sy: Word;
  Bx, By: Word;
begin
  Sx := fCellRect.Right - fCellRect.Left;
  Sy := fCellRect.Bottom - fCellRect.Top;
  SetLength(fBuffer, Sy, Sx);

  for I := fCellRect.Top to fCellRect.Bottom - 1 do
  for K := fCellRect.Left to fCellRect.Right - 1 do
  if gTerrain.TileInMapCoords(K+1, I+1, 0) then
  begin
    Bx := K - fCellRect.Left;
    By := I - fCellRect.Top;
    fBuffer[By,Bx].Terrain     := gTerrain.Land[I+1, K+1].Terrain;
    fBuffer[By,Bx].Height      := gTerrain.Land[I+1, K+1].Height;
    fBuffer[By,Bx].Rotation    := gTerrain.Land[I+1, K+1].Rotation;
    fBuffer[By,Bx].Obj         := gTerrain.Land[I+1, K+1].Obj;
//TODO: Move to TerrainPainter    fBuffer[By,Bx].TerrainKind := fTerrainPainter.TerrainKind[I+1, K+1];
  end;
end;


procedure TKMSelection.PasteBegin;
begin
  //Mapmaker could have changed selection rect, sync it with Buffer size
  fCellRect.Right := fCellRect.Left + Length(fBuffer[0]);
  fCellRect.Bottom := fCellRect.Top + Length(fBuffer);

  fSelectionMode := smPasting;
end;


procedure TKMSelection.PasteApply;
var
  I, K: Integer;
  Bx, By: Word;
begin
  for I := fCellRect.Top to fCellRect.Bottom - 1 do
  for K := fCellRect.Left to fCellRect.Right - 1 do
  if gTerrain.TileInMapCoords(K+1, I+1, 0) then
  begin
    Bx := K - fCellRect.Left;
    By := I - fCellRect.Top;
    gTerrain.Land[I+1, K+1].Terrain     := fBuffer[By,Bx].Terrain;
    gTerrain.Land[I+1, K+1].Height      := fBuffer[By,Bx].Height;
    gTerrain.Land[I+1, K+1].Rotation    := fBuffer[By,Bx].Rotation;
    gTerrain.Land[I+1, K+1].Obj         := fBuffer[By,Bx].Obj;
//TODO: Move to TerrainPainter    fTerrainPainter.TerrainKind[I+1, K+1] := fBuffer[By,Bx].TerrainKind;
  end;

  gTerrain.UpdateLighting(fCellRect);
  gTerrain.UpdatePassability(fCellRect);

  fSelectionMode := smSelecting;
end;


procedure TKMSelection.PasteCancel;
begin
  fSelectionMode := smSelecting;
end;


procedure TKMSelection.Paint;
var
  Sx, Sy: Word;
  I, K: Integer;
begin
  Sx := fCellRect.Right - fCellRect.Left;
  Sy := fCellRect.Bottom - fCellRect.Top;

  case fSelectionMode of
    smSelecting:  begin
                    //fRenderAux.SquareOnTerrain(RawRect.Left, RawRect.Top, RawRect.Right, RawRect.Bottom, $40FFFF00);
                    fRenderAux.SquareOnTerrain(fCellRect.Left, fCellRect.Top, fCellRect.Right, fCellRect.Bottom, $FFFFFF00);
                  end;
    smPasting:    begin
                    for I := 0 to Sy - 1 do
                    for K := 0 to Sx - 1 do
                      fRenderPool.RenderTerrain.RenderTile(fBuffer[I,K].Terrain, fCellRect.Left+K+1, fCellRect.Top+I+1, fBuffer[I,K].Rotation);

                    fRenderAux.SquareOnTerrain(fCellRect.Left, fCellRect.Top, fCellRect.Right, fCellRect.Bottom, $FF0000FF);
                  end;
  end;
end;


end.
