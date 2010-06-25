unit KM_Viewport;
{$I KaM_Remake.inc}
interface
uses ExtCtrls, SysUtils, Math, Types, Graphics, Controls, Forms, KromUtils, KromOGLUtils,
  {$IFDEF WDC} OpenGL, {$ENDIF}
  {$IFDEF FPC} GL, {$ENDIF}
  KM_Utils, KM_CommonTypes;

type
{ Here should be viewport routines }
TViewport = class
  private
    XCoord,YCoord:single;
    PrevScrollAdv:array [1..24]of single;
    PrevScrollPos:byte;
  public
    Zoom:single;
    ViewRect:TRect;
    ViewWidth, ViewHeight:integer;
    ScrollKeyLeft, ScrollKeyRight, ScrollKeyUp, ScrollKeyDown: boolean;
    Scrolling: boolean;
    constructor Create;
    procedure SetZoom(aZoom:single);
    procedure SetVisibleScreenArea(NewWidth,NewHeight:integer);
    function GetCenter():TKMPointF;
    procedure SetCenter(NewX,NewY:single);
    function GetClip():TRect; //returns visible area dimensions in map space
    function GetMinimapClip():TRect;
    procedure DoScrolling(aFrameTime:cardinal);
    procedure Save(SaveStream:TKMemoryStream);
    procedure Load(LoadStream:TKMemoryStream);
end;

var
  fViewport: TViewport;

implementation
uses KM_Defaults, KM_Terrain, KM_Unit1, KM_Sound, KM_Game;

constructor TViewport.Create;
begin
  FillChar(PrevScrollAdv, SizeOf(PrevScrollAdv), #0);
  PrevScrollPos := 0;
  Zoom := 1;
  ScrollKeyLeft  := false;
  ScrollKeyRight := false;
  ScrollKeyUp    := false;
  ScrollKeyDown  := false;
end;


procedure TViewport.SetZoom(aZoom:single);
begin
  aZoom := EnsureRange(aZoom, 0.1, 8);
  //Limit the zoom to within the map boundaries
  if ViewWidth /CELL_SIZE_PX/aZoom > fTerrain.MapX then aZoom := ViewWidth /CELL_SIZE_PX/(fTerrain.MapX-1);
  if ViewHeight/CELL_SIZE_PX/aZoom > fTerrain.MapY then aZoom := ViewHeight/CELL_SIZE_PX/ fTerrain.MapY;
  Zoom := aZoom;
  SetCenter(XCoord, YCoord); //To ensure it sets the limits smoothly
end;


procedure TViewport.SetVisibleScreenArea(NewWidth,NewHeight:integer);
begin
  ViewRect.Left   := ToolBarWidth;
  ViewRect.Top    := 0;
  ViewRect.Right  := NewWidth;
  ViewRect.Bottom := NewHeight;
  
  ViewWidth       := ViewRect.Right-ViewRect.Left;
  ViewHeight      := ViewRect.Bottom-ViewRect.Top;
end;

function TViewport.GetCenter():TKMPointF;
begin
  Result.X := EnsureRange(XCoord, 1, fTerrain.MapX);
  Result.Y := EnsureRange(YCoord, 1, fTerrain.MapY);
  if not SMOOTH_SCROLLING then Result.X := round(Result.X);
  if not SMOOTH_SCROLLING then Result.Y := round(Result.Y);
  fSoundLib.UpdateListener(Result);
end;

procedure TViewport.SetCenter(NewX,NewY:single);
begin
  XCoord:=EnsureRange(NewX, 0 + round(ViewWidth/2/CELL_SIZE_PX/Zoom),  fTerrain.MapX - round(ViewWidth /2/CELL_SIZE_PX/Zoom) - 1);
  YCoord:=EnsureRange(NewY,-1 + round(ViewHeight/2/CELL_SIZE_PX/Zoom), fTerrain.MapY - round(ViewHeight/2/CELL_SIZE_PX/Zoom)); //Top row should be visible
  fSoundLib.UpdateListener(KMPointF(XCoord,YCoord));
end;

//Acquire boundaries of area visible to user
//TestViewportClipInset is for debug, allows to see if all gets clipped well
function TViewport.GetClip():TRect;
begin
  Result.Left  :=Math.max(round(XCoord-(ViewWidth/2-ViewRect.Left+ToolBarWidth)/CELL_SIZE_PX/Zoom),1);
  Result.Right :=Math.min(round(XCoord+(ViewWidth/2+ViewRect.Left-ToolBarWidth)/CELL_SIZE_PX/Zoom)+1,fTerrain.MapX-1);
  Result.Top   :=Math.max(round(YCoord-ViewHeight/2/CELL_SIZE_PX/Zoom),1);
  Result.Bottom:=Math.min(round(YCoord+ViewHeight/2/CELL_SIZE_PX/Zoom)+4,fTerrain.MapY-1);
  if not TestViewportClipInset then exit;
  inc(Result.Left,4);
  dec(Result.Right,4);
  inc(Result.Top,4);
  dec(Result.Bottom,7);
end;

//Same as above function but with some values changed to suit minimap
function TViewport.GetMinimapClip():TRect;
begin
  Result.Left  :=Math.max(round(XCoord-(ViewWidth/2-ViewRect.Left+ToolBarWidth)/CELL_SIZE_PX/Zoom)+1,1);
  Result.Right :=Math.min(round(XCoord+(ViewWidth/2+ViewRect.Left-ToolBarWidth)/CELL_SIZE_PX/Zoom)+1,fTerrain.MapX);
  Result.Top   :=Math.max(round(YCoord-ViewHeight/2/CELL_SIZE_PX/Zoom)+2,1);
  Result.Bottom:=Math.min(round(YCoord+ViewHeight/2/CELL_SIZE_PX/Zoom),fTerrain.MapY);
  if not TestViewportClipInset then exit;
  inc(Result.Left,4);
  dec(Result.Right,4);
  inc(Result.Top,4);
  dec(Result.Bottom,7);
end;

//Here we must test each edge to see if we need to scroll in that direction
//We scroll at SCROLLSPEED per 100 ms. That constant is defined in KM_Defaults
procedure TViewport.DoScrolling(aFrameTime:cardinal);
const DirectionsBitfield:array[0..12]of byte = (0,c_Scroll6,c_Scroll0,c_Scroll7,c_Scroll2,0,c_Scroll1,0,c_Scroll4,c_Scroll5,0,0,c_Scroll3);
var
  ScrollAdv:single;
  Temp:byte;
begin
  if not ScrollKeyLeft  and
     not ScrollKeyUp    and
     not ScrollKeyRight and
     not ScrollKeyDown  and
     not (Mouse.CursorPos.X <= SCROLLFLEX) and
     not (Mouse.CursorPos.Y <= SCROLLFLEX) and
     not (Mouse.CursorPos.X >= Screen.Width -1-SCROLLFLEX) and
     not (Mouse.CursorPos.Y >= Screen.Height-1-SCROLLFLEX) then
  begin
    Scrolling := false;
    if (Screen.Cursor in [c_Scroll6..c_Scroll5]) then //Which is 2..8, since directions are not incremental
      Screen.Cursor := c_Default;
    exit;
  end;

  ScrollAdv := (SCROLLSPEED + byte(fGame.fGlobalSettings.IsFastScroll)*3)*aFrameTime/100; //1 vs 4 tiles per second

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
  //@Krom: Smooth scrolling isn't actually that smooth, it sort of hurts my eyes. Do we need vsync or something?
  //@Lewin: I did smoothed it by averaging from previous frames times.
  if ScrollKeyLeft  then XCoord := XCoord - ScrollAdv;
  if ScrollKeyUp    then YCoord := YCoord - ScrollAdv;
  if ScrollKeyRight then XCoord := XCoord + ScrollAdv;
  if ScrollKeyDown  then YCoord := YCoord + ScrollAdv;
  //Mouse
  if Mouse.CursorPos.X <= SCROLLFLEX then begin inc(Temp,1); XCoord := XCoord - ScrollAdv*(1-Mouse.CursorPos.X/SCROLLFLEX); end;
  if Mouse.CursorPos.Y <= SCROLLFLEX then begin inc(Temp,2); YCoord := YCoord - ScrollAdv*(1-Mouse.CursorPos.Y/SCROLLFLEX); end;
  if Mouse.CursorPos.X >= Screen.Width -1-SCROLLFLEX then begin inc(Temp,4); XCoord := XCoord + ScrollAdv*(1-(Screen.Width -1-Mouse.CursorPos.X)/SCROLLFLEX); end;
  if Mouse.CursorPos.Y >= Screen.Height-1-SCROLLFLEX then begin inc(Temp,8); YCoord := YCoord + ScrollAdv*(1-(Screen.Height-1-Mouse.CursorPos.Y)/SCROLLFLEX); end;

  //Now do actual the scrolling, if needed
  if Temp<>0 then begin
    Screen.Cursor :=DirectionsBitfield[Temp]; //Sample cursor type from bitfield value
    Scrolling := true; //Stop OnMouseOver from overriding my cursor changes
  end else begin
    Scrolling := false; //Allow cursor changes to be overriden and reset if still on a scrolling cursor
    if (Screen.Cursor in [c_Scroll6..c_Scroll5]) then //Which is 2..8, since directions are not incremental
      Screen.Cursor := c_Default;
  end;

  SetCenter(XCoord,YCoord); //EnsureRanges
end;


procedure TViewport.Save(SaveStream:TKMemoryStream);
begin
  SaveStream.Write('Viewport');
  SaveStream.Write(XCoord);
  SaveStream.Write(YCoord);
  SaveStream.Write(Zoom);
end;


procedure TViewport.Load(LoadStream:TKMemoryStream);
var s:string;
begin
  LoadStream.Read(s); if s <> 'Viewport' then exit;
  LoadStream.Read(XCoord);
  LoadStream.Read(YCoord);
  LoadStream.Read(Zoom);
  fLog.AppendLog('Viewport loaded');
end;


end.

