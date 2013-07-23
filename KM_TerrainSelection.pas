unit KM_TerrainSelection;
{$I KaM_Remake.inc}
interface
uses Classes, Math, Clipbrd,
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
                    TerrainKind: TTerrainKind; //Used for brushes
                  end;

  TKMSelection = class
  private
    fSelectionEdit: TKMSelectionEdit;
    fSelPrevX, fSelPrevY: Integer;

    fSelectionRectF: TKMRectF; //Cursor selection bounds (can have inverted bounds)
    fSelectionRect: TKMRect; //Tile-space selection, at least 1 tile
    fSelectionMode: TKMSelectionMode;
    fSelectionBuffer: array of array of TKMBufferData;
    procedure Selection_SyncCellRect;
  public
    procedure Selection_Resize;
    procedure Selection_Start;
    function Selection_DataInBuffer: Boolean;
    procedure Selection_Copy; //Copies the selected are into buffer
    procedure Selection_PasteBegin; //Pastes the area from buffer and lets move it with cursor
    procedure Selection_PasteApply; //Do the actual paste from buffer to terrain
    procedure Selection_PasteCancel;
    procedure Selection_Flip(aAxis: TKMFlipAxis);
    //procedure Transform; //Transforms the buffer data ?
    procedure Paint(aLayer: TKMPaintLayer; aClipRect: TKMRect);
  end;


var
  CF_MAPDATA: Word; //Our own custom clipboard format


implementation
uses KM_GameCursor, KM_RenderAux;


{ TKMSelection }
procedure TKMSelection.Selection_SyncCellRect;
begin
  //Convert RawRect values that can be inverted to tilespace Rect
  fSelectionRect.Left   := Trunc(Min(fSelectionRectF.Left, fSelectionRectF.Right));
  fSelectionRect.Top    := Trunc(Min(fSelectionRectF.Top, fSelectionRectF.Bottom));
  fSelectionRect.Right  := Ceil(Max(fSelectionRectF.Left, fSelectionRectF.Right));
  fSelectionRect.Bottom := Ceil(Max(fSelectionRectF.Top, fSelectionRectF.Bottom));
end;


procedure TKMSelection.Selection_Resize;
var
  RectO: TKMRect;
begin
  case fSelectionEdit of
    seNone:       ;
    seNewRect:    begin
                    fSelectionRectF.Right := GameCursor.Float.X;
                    fSelectionRectF.Bottom := GameCursor.Float.Y;
                  end;
    seResizeX1:   fSelectionRectF.Left := GameCursor.Float.X;
    seResizeY1:   fSelectionRectF.Top := GameCursor.Float.Y;
    seResizeX2:   fSelectionRectF.Right := GameCursor.Float.X;
    seResizeY2:   fSelectionRectF.Bottom := GameCursor.Float.Y;
    seMove:       begin
                    RectO := KMRectMove(fSelectionRect, GameCursor.Cell.X - fSelPrevX, GameCursor.Cell.Y - fSelPrevY);
                    fSelectionRectF := KMRectF(RectO);

                    fSelPrevX := GameCursor.Cell.X;
                    fSelPrevY := GameCursor.Cell.Y;
                  end;
  end;

  Selection_SyncCellRect;
end;


procedure TKMSelection.Selection_Start;
const
  EDGE = 0.25;
begin
  if fSelectionMode = smSelecting then
  begin
    if InRange(GameCursor.Float.Y, fSelectionRect.Top, fSelectionRect.Bottom)
    and (Abs(GameCursor.Float.X - fSelectionRect.Left) < EDGE) then
      fSelectionEdit := seResizeX1
    else
    if InRange(GameCursor.Float.Y, fSelectionRect.Top, fSelectionRect.Bottom)
    and (Abs(GameCursor.Float.X - fSelectionRect.Right) < EDGE) then
      fSelectionEdit := seResizeX2
    else
    if InRange(GameCursor.Float.X, fSelectionRect.Left, fSelectionRect.Right)
    and (Abs(GameCursor.Float.Y - fSelectionRect.Top) < EDGE) then
      fSelectionEdit := seResizeY1
    else
    if InRange(GameCursor.Float.X, fSelectionRect.Left, fSelectionRect.Right)
    and (Abs(GameCursor.Float.Y - fSelectionRect.Bottom) < EDGE) then
      fSelectionEdit := seResizeY2
    else
    if KMInRect(GameCursor.Float, fSelectionRect) then
    begin
      fSelectionEdit := seMove;
      fSelPrevX := GameCursor.Cell.X;
      fSelPrevY := GameCursor.Cell.Y;
    end
    else
    begin
      fSelectionEdit := seNewRect;
      fSelectionRectF := KMRectF(GameCursor.Float);
      Selection_SyncCellRect;
    end;
  end
  else
  begin
    if KMInRect(GameCursor.Float, fSelectionRect) then
    begin
      fSelectionEdit := seMove;
      //Grab and move
      fSelPrevX := GameCursor.Cell.X;
      fSelPrevY := GameCursor.Cell.Y;
    end
    else
    begin
      fSelectionEdit := seMove;
      //Selection edge will jump to under cursor
      fSelPrevX := EnsureRange(GameCursor.Cell.X, fSelectionRect.Left + 1, fSelectionRect.Right);
      fSelPrevY := EnsureRange(GameCursor.Cell.Y, fSelectionRect.Top + 1, fSelectionRect.Bottom);
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
    fSelectionBuffer[By,Bx].Terrain     := gTerrain.Land[I+1, K+1].Terrain;
    fSelectionBuffer[By,Bx].Height      := gTerrain.Land[I+1, K+1].Height;
    fSelectionBuffer[By,Bx].Rotation    := gTerrain.Land[I+1, K+1].Rotation;
    fSelectionBuffer[By,Bx].Obj         := gTerrain.Land[I+1, K+1].Obj;
//TODO: Move to TerrainPainter    fBuffer[By,Bx].TerrainKind := fTerrainPainter.TerrainKind[I+1, K+1];

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
//TODO: Move to TerrainPainter    fTerrainPainter.TerrainKind[I+1, K+1] := fBuffer[By,Bx].TerrainKind;
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
const
  CORNERS = [10,15,18,21..23,25,38,49,51..54,56,58,65,66,68..69,71,72,74,78,80,81,83,84,86..87,89,90,92,93,95,96,98,99,101,102,104,105,107..108,110..111,113,114,116,118,119,120,122,123,126..127,138,142,143,165,176..193,196,202,203,205,213,220,234..241,243,247];
  CORNERS_REVERSED = [15,21,142,234,235,238];
  EDGES = [4,12,19,39,50,57,64,67,70,73,76,79,82,85,88,91,94,97,100,103,106,109,112,115,117,121,124..125,139,141,166..175,194,198..200,204,206..212,216..219,223,224..233,242,244];
  //objMiddle = [8,9,54..61,80,81,212,213,215];
  //objMiddleVert = [8,9,54..61,80,81,212,213,215,  1..5,10..12,17..19,21..24,126,210,211,249..253];

  procedure FixTerrain(X, Y: Integer);
  var Ter, Rot:byte;
  begin
    Ter := fSelectionBuffer[Y,X].Terrain;
    Rot := fSelectionBuffer[Y,X].Rotation mod 4; //Some KaM maps contain rotations > 3 which must be fixed by modding

    //Edges
    if (Ter in EDGES) and ((Rot in [1,3]) xor (aAxis = fa_Vertical)) then
      fSelectionBuffer[Y,X].Rotation := (Rot+2) mod 4;

    //Corners
    if Ter in CORNERS then
    begin
      if (Rot in [1,3]) xor (Ter in CORNERS_REVERSED) xor (aAxis = fa_Vertical) then
        fSelectionBuffer[Y,X].Rotation := (Rot+1) mod 4
      else
        fSelectionBuffer[Y,X].Rotation := (Rot+3) mod 4;
    end;
  end;

var X,Y, MaxX, MaxY:integer; Temp: TKMBufferData;
begin
  MaxY := Length(fSelectionBuffer)-1;
  if MaxY < 0 then Exit;
  MaxX := Length(fSelectionBuffer[0])-1;

  case aAxis of
    fa_Horizontal:
      for Y:=0 to MaxY do
        for X:=0 to Ceil((MaxX+1) / 2)-1 do
        begin
          Temp := fSelectionBuffer[Y, X];
          fSelectionBuffer[Y, X] := fSelectionBuffer[Y, MaxX-X];
          fSelectionBuffer[Y, MaxX-X] := Temp;

          FixTerrain(X, Y);
          if MaxX-X <> X then
            FixTerrain(MaxX-X, Y);
        end;

    fa_Vertical:
      for Y:=0 to Ceil((MaxY+1) / 2)-1 do
        for X:=0 to MaxX do
        begin
          Temp := fSelectionBuffer[Y, X];
          fSelectionBuffer[Y, X] := fSelectionBuffer[MaxY-Y, X];
          fSelectionBuffer[MaxY-Y, X] := Temp;

          FixTerrain(X, Y);
          if MaxY-Y <> Y then
            FixTerrain(X, MaxY-Y);
        end;
  end;
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
                      fRenderAux.SquareOnTerrain(fSelectionRect.Left, fSelectionRect.Top, fSelectionRect.Right, fSelectionRect.Bottom, $FFFFFF00);
                    end;
      smPasting:    begin
                      for I := 0 to Sy - 1 do
                      for K := 0 to Sx - 1 do
                      if KMInRect(KMPoint(aClipRect.Left+K+1, aClipRect.Top+I+1), aClipRect) then
                        fRenderPool.RenderTerrain.RenderTile(fSelectionBuffer[I,K].Terrain, fSelectionRect.Left+K+1, fSelectionRect.Top+I+1, fSelectionBuffer[I,K].Rotation);

                      fRenderAux.SquareOnTerrain(fSelectionRect.Left, fSelectionRect.Top, fSelectionRect.Right, fSelectionRect.Bottom, $FF0000FF);
                    end;
    end;

  if aLayer = plObjects then
    if fSelectionMode = smPasting then
    begin
      for I := 0 to Sy - 1 do
      for K := 0 to Sx - 1 do
        if (fSelectionBuffer[I,K].Obj <> 255)
        and KMInRect(KMPoint(aClipRect.Left+K+1, aClipRect.Top+I+1), aClipRect) then
          fRenderPool.RenderMapElement(fSelectionBuffer[I,K].Obj, 0, fSelectionRect.Left+K+1, fSelectionRect.Top+I+1, True);
    end;
end;


initialization
  CF_MAPDATA := RegisterClipboardFormat('KaM Remake Map Data');


end.
