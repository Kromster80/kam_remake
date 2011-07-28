unit KM_Viewport;
{$I KaM_Remake.inc}
interface
uses Math, Types, Controls, Forms, KM_CommonTypes, KM_Points;

type
{ Here should be viewport routines }
TViewport = class
  private
    XCoord,YCoord:single;
    PrevScrollAdv:array [1..24]of single;
    PrevScrollPos:byte;
    fZoom:single;
    procedure SetZoom(aZoom:single);
  public
    ViewRect:TRect;
    ViewWidth, ViewHeight:integer;
    ScrollKeyLeft, ScrollKeyRight, ScrollKeyUp, ScrollKeyDown: boolean;
    Scrolling: boolean;
    constructor Create;

    property Zoom:single read fZoom write SetZoom;
    procedure ResetZoom;
    procedure Resize(NewWidth,NewHeight:integer);
    function GetCenter:TKMPointF;
    procedure SetCenter(NewX,NewY:single);
    function GetClip:TRect; //returns visible area dimensions in map space
    function GetMinimapClip:TRect;
    procedure ReleaseScrollKeys;
    procedure DoScrolling(aFrameTime:cardinal);
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
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
  XCoord := 0;
  YCoord := 0;
  fSoundLib.UpdateListener(XCoord, YCoord);
end;


procedure TViewport.SetZoom(aZoom:single);
begin
  fZoom := EnsureRange(aZoom, 0.1, 8);
  //Limit the zoom to within the map boundaries
  if ViewWidth /CELL_SIZE_PX/fZoom > fTerrain.MapX then fZoom := ViewWidth /CELL_SIZE_PX/(fTerrain.MapX-1);
  if ViewHeight/CELL_SIZE_PX/fZoom > fTerrain.MapY then fZoom := ViewHeight/CELL_SIZE_PX/ fTerrain.MapY;
  SetCenter(XCoord, YCoord); //To ensure it sets the limits smoothly
end;


procedure TViewport.ResetZoom;
begin
  Zoom := 1;
end;


procedure TViewport.Resize(NewWidth,NewHeight:integer);
begin
  ViewRect.Left   := TOOLBAR_WIDTH;
  ViewRect.Top    := 0;
  ViewRect.Right  := NewWidth;
  ViewRect.Bottom := NewHeight;
  
  ViewWidth       := ViewRect.Right-ViewRect.Left;
  ViewHeight      := ViewRect.Bottom-ViewRect.Top;

  SetZoom(fZoom); //View size has changed and that affects Zoom restrictions
end;


function TViewport.GetCenter:TKMPointF;
begin
  Result.X := EnsureRange(XCoord, 1, fTerrain.MapX);
  Result.Y := EnsureRange(YCoord, 1, fTerrain.MapY);
  if not SMOOTH_SCROLLING then Result.X := round(Result.X);
  if not SMOOTH_SCROLLING then Result.Y := round(Result.Y);
  fSoundLib.UpdateListener(Result.X, Result.Y);
end;


procedure TViewport.SetCenter(NewX,NewY:single);
begin
  XCoord := EnsureRange(NewX, 0 + ViewWidth/2/CELL_SIZE_PX/fZoom,  fTerrain.MapX - ViewWidth /2/CELL_SIZE_PX/fZoom - 1);
  YCoord := EnsureRange(NewY,-1 + ViewHeight/2/CELL_SIZE_PX/fZoom, fTerrain.MapY - ViewHeight/2/CELL_SIZE_PX/fZoom); //Top row should be visible
  fSoundLib.UpdateListener(XCoord, YCoord);
end;


//Acquire boundaries of area visible to user (including mountain tops from the lower tiles)
//TestViewportClipInset is for debug, allows to see if all gets clipped well
function TViewport.GetClip:TRect;
begin
  Result.Left  :=Math.max(round(XCoord-(ViewWidth/2-ViewRect.Left+TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom),1);
  Result.Right :=Math.min(round(XCoord+(ViewWidth/2+ViewRect.Left-TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom)+1,fTerrain.MapX-1);
  Result.Top   :=Math.max(round(YCoord-ViewHeight/2/CELL_SIZE_PX/fZoom),1);
  Result.Bottom:=Math.min(round(YCoord+ViewHeight/2/CELL_SIZE_PX/fZoom)+4,fTerrain.MapY-1);
  if not TEST_VIEW_CLIP_INSET then exit;
  inc(Result.Left,4);
  dec(Result.Right,4);
  inc(Result.Top,4);
  dec(Result.Bottom,7);
end;


//Same as above function but with some values changed to suit minimap
function TViewport.GetMinimapClip:TRect;
begin
  Result.Left  :=Math.max(round(XCoord-(ViewWidth/2-ViewRect.Left+TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom)+1,1);
  Result.Right :=Math.min(round(XCoord+(ViewWidth/2+ViewRect.Left-TOOLBAR_WIDTH)/CELL_SIZE_PX/fZoom)+1,fTerrain.MapX);
  Result.Top   :=Math.max(round(YCoord-ViewHeight/2/CELL_SIZE_PX/fZoom)+2,1);
  Result.Bottom:=Math.min(round(YCoord+ViewHeight/2/CELL_SIZE_PX/fZoom),fTerrain.MapY);
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
procedure TViewport.DoScrolling(aFrameTime:cardinal);
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
  if not ScrollKeyLeft  and
     not ScrollKeyUp    and
     not ScrollKeyRight and
     not ScrollKeyDown  and
     not (CursorPoint.X <= ScreenBounds.Left + SCROLLFLEX) and
     not (CursorPoint.Y <= ScreenBounds.Top + SCROLLFLEX) and
     not (CursorPoint.X >= ScreenBounds.Right -1-SCROLLFLEX) and
     not (CursorPoint.Y >= ScreenBounds.Bottom-1-SCROLLFLEX) then
  begin
    Scrolling := false;
    if (Screen.Cursor in [c_Scroll6..c_Scroll5]) then //Which is 2..8, since directions are not incremental
      Screen.Cursor := c_Default;
    exit;
  end;

  ScrollAdv := (SCROLLSPEED + byte(fGame.GlobalSettings.FastScroll)*3)*aFrameTime/100; //1 vs 4 tiles per second

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
  if ScrollKeyLeft  then XCoord := XCoord - ScrollAdv;
  if ScrollKeyUp    then YCoord := YCoord - ScrollAdv;
  if ScrollKeyRight then XCoord := XCoord + ScrollAdv;
  if ScrollKeyDown  then YCoord := YCoord + ScrollAdv;
  //Mouse
  if CursorPoint.X <= ScreenBounds.Left   + SCROLLFLEX then begin inc(Temp,1); XCoord := XCoord - ScrollAdv*(1-(ScreenBounds.Left   - CursorPoint.X)/SCROLLFLEX); end;
  if CursorPoint.Y <= ScreenBounds.Top    + SCROLLFLEX then begin inc(Temp,2); YCoord := YCoord - ScrollAdv*(1-(ScreenBounds.Top    - CursorPoint.Y)/SCROLLFLEX); end;
  if CursorPoint.X >= ScreenBounds.Right -1-SCROLLFLEX then begin inc(Temp,4); XCoord := XCoord + ScrollAdv*(1-(ScreenBounds.Right -1-CursorPoint.X)/SCROLLFLEX); end;
  if CursorPoint.Y >= ScreenBounds.Bottom-1-SCROLLFLEX then begin inc(Temp,8); YCoord := YCoord + ScrollAdv*(1-(ScreenBounds.Bottom-1-CursorPoint.Y)/SCROLLFLEX); end;

  //Now do actual the scrolling, if needed
  Scrolling := Temp<>0;
  if Scrolling then
    Screen.Cursor :=DirectionsBitfield[Temp] //Sample cursor type from bitfield value
  else
    if (Screen.Cursor in [c_Scroll6..c_Scroll5]) then //Which is 2..8, since directions are not incremental
      Screen.Cursor := c_Default;

  SetCenter(XCoord,YCoord); //EnsureRanges
end;


procedure TViewport.Save(SaveStream:TKMemoryStream);
begin
  SaveStream.Write('Viewport');
  SaveStream.Write(XCoord);
  SaveStream.Write(YCoord);
  SaveStream.Write(fZoom);
end;


procedure TViewport.Load(LoadStream:TKMemoryStream);
var s:string;
begin
  LoadStream.Read(s);
  Assert(s = 'Viewport');
  LoadStream.Read(XCoord);
  LoadStream.Read(YCoord);
  LoadStream.Read(fZoom);
  fSoundLib.UpdateListener(XCoord, YCoord);
  fLog.AppendLog('Viewport loaded');
end;


end.

