unit KM_TerrainSelection;
{$I KaM_Remake.inc}
interface
uses Classes, Math,
  KM_Points, KM_Terrain, KM_TerrainPainter;


type
  TKMSelectionMode = (smSelecting, smPasting);
  TKMSelection = class
  private
    fRawRect: TKMRectF; //Cursor selection bounds (can have inverted bounds)
    fRect: TKMRect; //Tile-space selection, at least 1 tile
    fSelectionMode: TKMSelectionMode;
    fBuffer: array of array of record
      Terrain: Byte;
      Height: Byte;
      Rotation: Byte;
      Obj: Byte;
      OldTerrain, OldRotation: Byte; //Only used for map editor
      TerrainKind: TTerrainKind; //Used for brushes
    end;
    procedure SetRawRect(const aValue: TKMRectF);
  public
    property RawRect: TKMRectF read fRawRect write SetRawRect;
    property Rect: TKMRect read fRect write fRect;
    property SelectionMode: TKMSelectionMode read fSelectionMode;
    function IsBufferHasData: Boolean;
    procedure Copy; //Copies the selected are into buffer
    procedure PasteBegin; //Pastes the area from buffer and lets move it with cursor
    procedure PasteApply; //Do the actual paste from buffer to terrain
    procedure PasteCancel;
    //procedure Transform; //Transforms the buffer data ?
    procedure Paint;
  end;


implementation
uses KM_RenderAux, KM_RenderPool;


{ TKMSelection }
procedure TKMSelection.SetRawRect(const aValue: TKMRectF);
begin
  fRawRect := aValue;

  //Convert RawRect values that can be inverted to tilespace Rect
  fRect.Left   := Trunc(Min(fRawRect.Left, fRawRect.Right));
  fRect.Top    := Trunc(Min(fRawRect.Top, fRawRect.Bottom));
  fRect.Right  := Ceil(Max(fRawRect.Left, fRawRect.Right));
  fRect.Bottom := Ceil(Max(fRawRect.Top, fRawRect.Bottom));
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
  Sx := fRect.Right - fRect.Left;
  Sy := fRect.Bottom - fRect.Top;
  SetLength(fBuffer, Sy, Sx);

  for I := fRect.Top to fRect.Bottom - 1 do
  for K := fRect.Left to fRect.Right - 1 do
  if gTerrain.TileInMapCoords(K+1, I+1, 0) then
  begin
    Bx := K - fRect.Left;
    By := I - fRect.Top;
    fBuffer[By,Bx].Terrain     := gTerrain.Land[I+1, K+1].Terrain;
    fBuffer[By,Bx].Height      := gTerrain.Land[I+1, K+1].Height;
    fBuffer[By,Bx].Rotation    := gTerrain.Land[I+1, K+1].Rotation;
    fBuffer[By,Bx].Obj         := gTerrain.Land[I+1, K+1].Obj;
    fBuffer[By,Bx].OldTerrain  := gTerrain.Land[I+1, K+1].OldTerrain;
    fBuffer[By,Bx].OldRotation := gTerrain.Land[I+1, K+1].OldRotation;
//TODO: Move to TerrainPainter    fBuffer[By,Bx].TerrainKind := fTerrainPainter.TerrainKind[I+1, K+1];
  end;
end;


procedure TKMSelection.PasteBegin;
begin
  //Mapmaker could have changed selection rect, sync it with Buffer size
  fRect.Right := fRect.Left + Length(fBuffer[0]);
  fRect.Bottom := fRect.Top + Length(fBuffer);

  fSelectionMode := smPasting;
end;


procedure TKMSelection.PasteApply;
var
  I, K: Integer;
  Bx, By: Word;
begin
  for I := fRect.Top to fRect.Bottom - 1 do
  for K := fRect.Left to fRect.Right - 1 do
  if gTerrain.TileInMapCoords(K+1, I+1, 0) then
  begin
    Bx := K - fRect.Left;
    By := I - fRect.Top;
    gTerrain.Land[I+1, K+1].Terrain     := fBuffer[By,Bx].Terrain;
    gTerrain.Land[I+1, K+1].Height      := fBuffer[By,Bx].Height;
    gTerrain.Land[I+1, K+1].Rotation    := fBuffer[By,Bx].Rotation;
    gTerrain.Land[I+1, K+1].Obj         := fBuffer[By,Bx].Obj;
    gTerrain.Land[I+1, K+1].OldTerrain  := fBuffer[By,Bx].OldTerrain;
    gTerrain.Land[I+1, K+1].OldRotation := fBuffer[By,Bx].OldRotation;
//TODO: Move to TerrainPainter    fTerrainPainter.TerrainKind[I+1, K+1] := fBuffer[By,Bx].TerrainKind;
  end;

  gTerrain.UpdateLighting(fRect);
  gTerrain.UpdatePassability(fRect);

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
  Sx := Rect.Right - Rect.Left;
  Sy := Rect.Bottom - Rect.Top;

  case fSelectionMode of
    smSelecting:  begin
                    //fRenderAux.SquareOnTerrain(RawRect.Left, RawRect.Top, RawRect.Right, RawRect.Bottom, $40FFFF00);
                    fRenderAux.SquareOnTerrain(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, $FFFFFF00);
                  end;
    smPasting:    begin
                    for I := 0 to Sy - 1 do
                    for K := 0 to Sx - 1 do
                      fRenderPool.RenderTerrain.RenderTile(fBuffer[I,K].Terrain, Rect.Left+K+1, Rect.Top+I+1, fBuffer[I,K].Rotation);

                    fRenderAux.SquareOnTerrain(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, $FF0000FF);
                  end;
  end;
end;


end.
