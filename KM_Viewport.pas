unit KM_Viewport;
{$I KaM_Remake.inc}
interface
uses Math, Controls, Forms, Windows, KM_CommonTypes, KM_Points;

type
  { Here should be viewport routines }
  TViewport = class
  private
    fPosition:TKMPointF;
    fScrolling: boolean;
    PrevScrollAdv:array [1..24]of single;
    PrevScrollPos:byte;
    fViewportClip:TPoint;
    fViewRect:TRect;
    fZoom:single;
    function GetPosition:TKMPointF;
    procedure SetPosition(Value: TKMPointF);
    procedure SetZoom(aZoom:single);
  public
    ScrollKeyLeft, ScrollKeyRight, ScrollKeyUp, ScrollKeyDown: boolean;
    constructor Create;

    property Position:TKMPointF read GetPosition write SetPosition;
    property Scrolling:boolean read fScrolling;
    property ViewportClip:TPoint read fViewportClip;
    property ViewRect:TRect read fViewRect;
    property Zoom:single read fZoom write SetZoom;

    procedure ResetZoom;
    procedure Resize(NewWidth,NewHeight:integer);
    function GetClip:TRect; //returns visible area dimensions in map space
    function GetMinimapClip:TRect;
    procedure ReleaseScrollKeys;

    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);

    procedure UpdateStateIdle(aFrameTime:cardinal);
  end;

var
  fViewport: TViewport;

implementation
uses KM_Defaults, KM_Terrain, KM_Sound, KM_Game, KM_Unit1, KM_Log;


constructor TViewport.Create;
begin
  Inherited;
  FillChar(PrevScrollAdv, SizeOf(PrevScrollAdv), #0);
  PrevScrollPos := 0;
  fZoom := 1;
  ReleaseScrollKeys;
  fSoundLib.UpdateListener(fPosition.X, fPosition.Y);
end;


procedure TViewport.SetZoom(aZoom:single);
begin
  fZoom := EnsureRange(aZoom, 0.1, 8);
  //Limit the zoom to within the map boundaries
  if fViewportClip.X/CELL_SIZE_PX/fZoom > fTerrain.MapX then fZoom := fViewportClip.X/CELL_SIZE_PX/(fTerrain.MapX-1);
  if fViewportClip.Y/CELL_SIZE_PX/fZoom > fTerrain.MapY then fZoom := fViewportClip.Y/CELL_SIZE_PX/ fTerrain.MapY;
  SetPosition(fPosition); //To ensure it sets the limits smoothly
end;


procedure TViewport.ResetZoom;
begin
  Zoom := 1;
end;


procedure TViewport.Resize(NewWidth,NewHeight:integer);
begin
  fViewRect.Left   := TOOLBAR_WIDTH;
  fViewRect.Top    := 0;
  fViewRect.Right  := NewWidth;
  fViewRect.Bottom := NewHeight;
  
  fViewportClip.X := fViewRect.Right-fViewRect.Left;
  fViewportClip.Y := fViewRect.Bottom-fViewRect.Top;

  SetZoom(fZoom); //View size has changed and that affects Zoom restrictions
end;


function TViewport.GetPosition:TKMPointF;
begin
  Result.X := EnsureRange(fPosition.X, 1, fTerrain.MapX);
  Result.Y := EnsureRange(fPosition.Y, 1, fTerrain.MapY);
  if not SMOOTH_SCROLLING then Result.X := round(Result.X);
  if not SMOOTH_SCROLLING then Result.Y := round(Result.Y);
  fSoundLib.UpdateListener(Result.X, Result.Y);
end;


procedure TViewport.SetPosition(Value: TKMPointF);
begin
  fPosition.X := EnsureRange(Value.X, 0 + fViewportClip.X/2/CELL_SIZE_PX/fZoom, fTerrain.MapX - fViewportClip.X/2/CELL_SIZE_PX/fZoom - 1);
  fPosition.Y := EnsureRange(Value.Y,-1 + fViewportClip.Y/2/CELL_SIZE_PX/fZoom, fTerrain.MapY - fViewportClip.Y/2/CELL_SIZE_PX/fZoom); //Top row should be visible
  fSoundLib.UpdateListener(fPosition.X, fPosition.Y);
end;


//Acquire boundaries of area visible to user (including mountain tops from the lower tiles)
//TestViewportClipInset is for debug, allows to see if all gets clipped well
function TViewport.GetClip:TRect;
begin
  Result.Left   := Math.max(round(fPosition.X-(fViewportClip.X/2-fViewRect.Left+TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom),1);
  Result.Right  := Math.min(round(fPosition.X+(fViewportClip.X/2+fViewRect.Left-TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom)+1,fTerrain.MapX-1);
  Result.Top    := Math.max(round(fPosition.Y-fViewportClip.Y/2/CELL_SIZE_PX/fZoom),1);
  Result.Bottom := Math.min(round(fPosition.Y+fViewportClip.Y/2/CELL_SIZE_PX/fZoom)+4,fTerrain.MapY-1);
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
  Result.Right  := Math.min(round(fPosition.X+(fViewportClip.X/2+fViewRect.Left-TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom)+1,fTerrain.MapX);
  Result.Top    := Math.max(round(fPosition.Y-fViewportClip.Y/2/CELL_SIZE_PX/fZoom)+2,1);
  Result.Bottom := Math.min(round(fPosition.Y+fViewportClip.Y/2/CELL_SIZE_PX/fZoom),fTerrain.MapY);
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
procedure TViewport.UpdateStateIdle(aFrameTime:cardinal);
const DirectionsBitfield:array[0..12]of byte = (0,c_Scroll6,c_Scroll0,c_Scroll7,c_Scroll2,0,c_Scroll1,0,c_Scroll4,c_Scroll5,0,0,c_Scroll3);
var
  ScrollAdv:single;
  CursorPoint: TKMPoint;
  ScreenBounds: TRect;
  Temp:byte;
begin
  ScreenBounds := Form1.GetScreenBounds;
  //With multiple monitors the cursor position can be outside of this screen, which makes scrolling too fast
  CursorPoint.X := EnsureRange(Mouse.CursorPos.X, ScreenBounds.Left, ScreenBounds.Right );
  CursorPoint.Y := EnsureRange(Mouse.CursorPos.Y, ScreenBounds.Top , ScreenBounds.Bottom);
  if(Form1.Handle <> GetForegroundWindow) or //Do not do scrolling when the form is not focused (player has switched to another application)
    (not ScrollKeyLeft  and
     not ScrollKeyUp    and
     not ScrollKeyRight and
     not ScrollKeyDown  and
     not (CursorPoint.X <= ScreenBounds.Left + SCROLLFLEX) and
     not (CursorPoint.Y <= ScreenBounds.Top + SCROLLFLEX) and
     not (CursorPoint.X >= ScreenBounds.Right -1-SCROLLFLEX) and
     not (CursorPoint.Y >= ScreenBounds.Bottom-1-SCROLLFLEX)) then
  begin
    ReleaseScrollKeys; //Release scroll keys when we are no longer scrolling (required if the form loses focus)
    fScrolling := false;
    if (Screen.Cursor in [c_Scroll6..c_Scroll3]) then //Which is 2..9, since directions are not incremental
      Screen.Cursor := c_Default;
    exit;
  end;

  ScrollAdv := (SCROLLSPEED + fGame.GlobalSettings.ScrollSpeed/5)*aFrameTime/100; //1 vs 4 tiles per second

  PrevScrollPos := (PrevScrollPos + 1) mod length(PrevScrollAdv) + 1; //Position in ring-buffer
  PrevScrollAdv[PrevScrollPos] := ScrollAdv; //Replace oldest value
  for Temp := 1 to length(PrevScrollAdv) do //Compute average
    ScrollAdv := ScrollAdv + PrevScrollAdv[Temp];
  ScrollAdv := ScrollAdv / length(PrevScrollAdv);//}

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
  fScrolling := Temp<>0;
  if fScrolling then
    Screen.Cursor := DirectionsBitfield[Temp] //Sample cursor type from bitfield value
  else
    if (Screen.Cursor in [c_Scroll6..c_Scroll3]) then //Which is 2..9, since directions are not incremental
      Screen.Cursor := c_Default;

  SetPosition(fPosition); //EnsureRanges
end;


procedure TViewport.Save(SaveStream:TKMemoryStream);
begin
  SaveStream.Write('Viewport');
  SaveStream.Write(fPosition);
  SaveStream.Write(fZoom);
end;


procedure TViewport.Load(LoadStream:TKMemoryStream);
var s:string;
begin
  LoadStream.Read(s);
  Assert(s = 'Viewport');
  LoadStream.Read(fPosition);
  LoadStream.Read(fZoom);
  fSoundLib.UpdateListener(fPosition.X, fPosition.Y);
  fLog.AppendLog('Viewport loaded');
end;


end.

