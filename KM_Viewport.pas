unit KM_Viewport;
{$I KaM_Remake.inc}
interface
uses Math, Controls, Forms, Windows, KM_CommonClasses, KM_Points;

type
  { Here should be viewport routines }
  TViewport = class
  private
    fMapX, fMapY: Word;
    fPosition: TKMPointF;
    fScrolling: Boolean;
    PrevScrollAdv: array [1..24] of Single;
    PrevScrollPos: Byte;
    fViewportClip: TPoint;
    fViewRect: TRect;
    fZoom: Single;
    function GetPosition: TKMPointF;
    procedure SetPosition(Value: TKMPointF);
    procedure SetZoom(aZoom: Single);
  public
    ScrollKeyLeft, ScrollKeyRight, ScrollKeyUp, ScrollKeyDown: boolean;
    constructor Create(aWidth, aHeight: Integer);

    property Position: TKMPointF read GetPosition write SetPosition;
    property Scrolling: Boolean read fScrolling;
    property ViewportClip: TPoint read fViewportClip;
    property ViewRect: TRect read fViewRect;
    property Zoom: Single read fZoom write SetZoom;

    procedure ResetZoom;
    procedure Resize(NewWidth, NewHeight: Integer);
    procedure ResizeMap(aMapX, aMapY: Integer);
    function GetClip: TRect; //returns visible area dimensions in map space
    function GetMinimapClip: TRect;
    procedure ReleaseScrollKeys;

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure UpdateStateIdle(aFrameTime: Cardinal);
  end;


implementation
uses KM_Defaults, KM_Sound, KM_Game, KM_Unit1, KM_Resource, KM_ResourceCursors;


constructor TViewport.Create(aWidth, aHeight: Integer);
begin
  Inherited Create;
  fMapX := 1; //Avoid division by 0
  fMapY := 1; //Avoid division by 0

  FillChar(PrevScrollAdv, SizeOf(PrevScrollAdv), #0);
  PrevScrollPos := 0;
  fZoom := 1;
  ReleaseScrollKeys;
  fSoundLib.UpdateListener(fPosition.X, fPosition.Y);
  Resize(aWidth, aHeight);
end;


procedure TViewport.SetZoom(aZoom:single);
begin
  fZoom := EnsureRange(aZoom, 0.1, 8);
  //Limit the zoom to within the map boundaries
  if fViewportClip.X/CELL_SIZE_PX/fZoom > fMapX then fZoom := fViewportClip.X/CELL_SIZE_PX/(fMapX-1);
  if fViewportClip.Y/CELL_SIZE_PX/fZoom > fMapY then fZoom := fViewportClip.Y/CELL_SIZE_PX/ fMapY;
  SetPosition(fPosition); //To ensure it sets the limits smoothly
end;


procedure TViewport.ResetZoom;
begin
  Zoom := 1;
end;


procedure TViewport.Resize(NewWidth, NewHeight: Integer);
begin
  fViewRect.Left   := TOOLBAR_WIDTH;
  fViewRect.Top    := 0;
  fViewRect.Right  := NewWidth;
  fViewRect.Bottom := NewHeight;
  
  fViewportClip.X := fViewRect.Right-fViewRect.Left;
  fViewportClip.Y := fViewRect.Bottom-fViewRect.Top;

  SetZoom(fZoom); //View size has changed and that affects Zoom restrictions
end;


procedure TViewport.ResizeMap(aMapX, aMapY: Integer);
begin
  fMapX := aMapX;
  fMapY := aMapY;
end;


function TViewport.GetPosition:TKMPointF;
begin
  Result.X := EnsureRange(fPosition.X, 1, fMapX);
  Result.Y := EnsureRange(fPosition.Y, 1, fMapY);
  if not SMOOTH_SCROLLING then Result.X := round(Result.X);
  if not SMOOTH_SCROLLING then Result.Y := round(Result.Y);
end;


procedure TViewport.SetPosition(Value: TKMPointF);
begin
  fPosition.X := EnsureRange(Value.X, 0 + fViewportClip.X/2/CELL_SIZE_PX/fZoom, fMapX - fViewportClip.X/2/CELL_SIZE_PX/fZoom - 1);
  fPosition.Y := EnsureRange(Value.Y,-1 + fViewportClip.Y/2/CELL_SIZE_PX/fZoom, fMapY - fViewportClip.Y/2/CELL_SIZE_PX/fZoom); //Top row should be visible
  fSoundLib.UpdateListener(fPosition.X, fPosition.Y);
end;


//Acquire boundaries of area visible to user (including mountain tops from the lower tiles)
//TestViewportClipInset is for debug, allows to see if all gets clipped well
function TViewport.GetClip:TRect;
begin
  Result.Left   := Math.max(round(fPosition.X-(fViewportClip.X/2-fViewRect.Left+TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom),1);
  Result.Right  := Math.min(round(fPosition.X+(fViewportClip.X/2+fViewRect.Left-TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom)+1,fMapX-1);
  Result.Top    := Math.max(round(fPosition.Y-fViewportClip.Y/2/CELL_SIZE_PX/fZoom),1);
  Result.Bottom := Math.min(round(fPosition.Y+fViewportClip.Y/2/CELL_SIZE_PX/fZoom)+4,fMapY-1);
  if not TEST_VIEW_CLIP_INSET then exit;
  inc(Result.Left,4);
  dec(Result.Right,4);
  inc(Result.Top,4);
  dec(Result.Bottom,7);
end;


//Same as above function but with some values changed to suit minimap
function TViewport.GetMinimapClip:TRect;
begin
  Result.Left   := Math.max(round(fPosition.X-(fViewportClip.X/2-fViewRect.Left+TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom)+1,1);
  Result.Right  := Math.min(round(fPosition.X+(fViewportClip.X/2+fViewRect.Left-TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom)+1,fMapX);
  Result.Top    := Math.max(round(fPosition.Y-fViewportClip.Y/2/CELL_SIZE_PX/fZoom)+2,1);
  Result.Bottom := Math.min(round(fPosition.Y+fViewportClip.Y/2/CELL_SIZE_PX/fZoom),fMapY);
end;


procedure TViewport.ReleaseScrollKeys;
begin
  ScrollKeyLeft  := false;
  ScrollKeyRight := false;
  ScrollKeyUp    := false;
  ScrollKeyDown  := false;
end;


//Here we must test each edge to see if we need to scroll in that direction
//We scroll at SCROLLSPEED per 100 ms. That constant is defined in KM_Defaults
procedure TViewport.UpdateStateIdle(aFrameTime: Cardinal);
const
  DirectionsBitfield: array [0..15] of TKMCursor = (
    kmc_Default, kmc_Scroll6, kmc_Scroll0, kmc_Scroll7,
    kmc_Scroll2, kmc_Default, kmc_Scroll1, kmc_Default,
    kmc_Scroll4, kmc_Scroll5, kmc_Default, kmc_Default,
    kmc_Scroll3, kmc_Default, kmc_Default, kmc_Default);
var
  ScrollAdv: Single;
  CursorPoint: TKMPoint;
  ScreenBounds: TRect;
  Temp:byte;
begin
  ScreenBounds := Form1.GetScreenBounds;
  //With multiple monitors the cursor position can be outside of this screen, which makes scrolling too fast
  CursorPoint.X := EnsureRange(Mouse.CursorPos.X, ScreenBounds.Left, ScreenBounds.Right );
  CursorPoint.Y := EnsureRange(Mouse.CursorPos.Y, ScreenBounds.Top , ScreenBounds.Bottom);

  //Do not do scrolling when the form is not focused (player has switched to another application)
  if not Form1.Active or
    (not ScrollKeyLeft  and
     not ScrollKeyUp    and
     not ScrollKeyRight and
     not ScrollKeyDown  and
     not (CursorPoint.X <= ScreenBounds.Left + SCROLLFLEX) and
     not (CursorPoint.Y <= ScreenBounds.Top + SCROLLFLEX) and
     not (CursorPoint.X >= ScreenBounds.Right -1-SCROLLFLEX) and
     not (CursorPoint.Y >= ScreenBounds.Bottom-1-SCROLLFLEX)) then
  begin
    //Stop the scrolling (e.g. if the form loses focus due to other application popping up)
    ReleaseScrollKeys;
    fScrolling := False;

    if (fResource.Cursors.Cursor in [kmc_Scroll0 .. kmc_Scroll7]) then
      fResource.Cursors.Cursor := kmc_Default;

    Exit;
  end;

  ScrollAdv := (SCROLLSPEED + fGame.GlobalSettings.ScrollSpeed / 5) * aFrameTime / 100; //1-5 tiles per second

  PrevScrollPos := (PrevScrollPos + 1) mod length(PrevScrollAdv) + 1; //Position in ring-buffer
  PrevScrollAdv[PrevScrollPos] := ScrollAdv; //Replace oldest value
  for Temp := 1 to length(PrevScrollAdv) do //Compute average
    ScrollAdv := ScrollAdv + PrevScrollAdv[Temp];
  ScrollAdv := ScrollAdv / Length(PrevScrollAdv);

  Temp := 0; //That is our bitfield variable for directions, 0..12 range
  //    3 2 6  These are directions
  //    1 * 4  They are converted from bitfield to actual cursor constants, see Arr array
  //    9 8 12

  //Keys
  if ScrollKeyLeft  then fPosition.X := fPosition.X - ScrollAdv;
  if ScrollKeyUp    then fPosition.Y := fPosition.Y - ScrollAdv;
  if ScrollKeyRight then fPosition.X := fPosition.X + ScrollAdv;
  if ScrollKeyDown  then fPosition.Y := fPosition.Y + ScrollAdv;
  //Mouse
  if CursorPoint.X <= ScreenBounds.Left   + SCROLLFLEX then begin inc(Temp,1); fPosition.X := fPosition.X - ScrollAdv*(1+(ScreenBounds.Left   - CursorPoint.X)/SCROLLFLEX); end;
  if CursorPoint.Y <= ScreenBounds.Top    + SCROLLFLEX then begin inc(Temp,2); fPosition.Y := fPosition.Y - ScrollAdv*(1+(ScreenBounds.Top    - CursorPoint.Y)/SCROLLFLEX); end;
  if CursorPoint.X >= ScreenBounds.Right -1-SCROLLFLEX then begin inc(Temp,4); fPosition.X := fPosition.X + ScrollAdv*(1-(ScreenBounds.Right -1-CursorPoint.X)/SCROLLFLEX); end;
  if CursorPoint.Y >= ScreenBounds.Bottom-1-SCROLLFLEX then begin inc(Temp,8); fPosition.Y := fPosition.Y + ScrollAdv*(1-(ScreenBounds.Bottom-1-CursorPoint.Y)/SCROLLFLEX); end;

  //Now do actual the scrolling, if needed
  fScrolling := Temp <> 0;
  if fScrolling then
    fResource.Cursors.Cursor := DirectionsBitfield[Temp] //Sample cursor type from bitfield value
  else
    if (fResource.Cursors.Cursor in [kmc_Scroll0 .. kmc_Scroll7]) then
      fResource.Cursors.Cursor := kmc_Default;

  SetPosition(fPosition); //EnsureRanges
end;


procedure TViewport.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write('Viewport');
  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
  SaveStream.Write(fPosition);
  SaveStream.Write(fZoom);
end;


procedure TViewport.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('Viewport');
  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);
  LoadStream.Read(fPosition);
  LoadStream.Read(fZoom);
  fSoundLib.UpdateListener(fPosition.X, fPosition.Y);
end;


end.

