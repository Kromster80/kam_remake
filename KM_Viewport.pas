unit KM_Viewport;
interface            
uses StdCtrls, ExtCtrls, SysUtils, Math, Types, Graphics, Controls, Forms, KromUtils, KromOGLUtils, OpenGL;

type

{ Here should be viewport routines }
TViewport = class
private
XCoord,YCoord:integer;
protected
public
Zoom:single;
ViewRect:TRect;
ViewWidth,ViewHeight:integer;
  constructor Create;
  procedure SetZoom(aZoom:single);
  procedure SetArea(NewWidth,NewHeight:integer);
  function GetCenter():TKMPoint;
  procedure SetCenter(NewX,NewY:integer);
  function GetClip():TRect; //returns visible area dimensions in map space
  procedure DoScrolling;
published
end;

var
  fViewport: TViewport;

implementation
uses KM_Defaults, KM_Terrain, KM_Unit1, KM_Users, KM_LoadSFX, KM_Settings;

constructor TViewport.Create;
begin
  Zoom:=1;
end;


procedure TViewport.SetZoom(aZoom:single);
begin
  Zoom:=EnsureRange(aZoom,0.1,8);
end;


procedure TViewport.SetArea(NewWidth,NewHeight:integer);
begin
  ViewRect.Left:=ToolBarWidth;
  ViewRect.Top:=0;
  ViewRect.Right:=NewWidth;
  ViewRect.Bottom:=NewHeight;
  ViewWidth:=ViewRect.Right-ViewRect.Left;
  ViewHeight:=ViewRect.Bottom-ViewRect.Top;
end;

function TViewport.GetCenter():TKMPoint;
begin
  Result.X:=EnsureRange(XCoord,1,fTerrain.MapX);
  Result.Y:=EnsureRange(YCoord,1,fTerrain.MapY);
  fSoundLib.UpdateListener(KMPoint(XCoord,YCoord));
end;

procedure TViewport.SetCenter(NewX,NewY:integer);
begin
  XCoord:=EnsureRange(NewX,1,fTerrain.MapX);
  YCoord:=EnsureRange(NewY,1,fTerrain.MapY);
  fSoundLib.UpdateListener(KMPoint(XCoord,YCoord));
end;

//Acquire boundaries of area visible to user
//TestViewportClipInset is for debug, allows to see if all gets clipped well
function TViewport.GetClip():TRect;
begin
  Result.Left  :=max(round(XCoord-(ViewWidth/2-ViewRect.Left+ToolBarWidth)/CELL_SIZE_PX/Zoom),1);
  Result.Right :=min(round(XCoord+(ViewWidth/2+ViewRect.Left-ToolBarWidth)/CELL_SIZE_PX/Zoom)+1,fTerrain.MapX-1);
  Result.Top   :=max(round(YCoord-ViewHeight/2/CELL_SIZE_PX/Zoom),1);
  Result.Bottom:=min(round(YCoord+ViewHeight/2/CELL_SIZE_PX/Zoom)+4,fTerrain.MapY-1);
  if not TestViewportClipInset then exit;
  inc(Result.Left,4);
  dec(Result.Right,4);
  inc(Result.Top,4);
  dec(Result.Bottom,7);
end;

//Here we must test each edge to see if we need to scroll in that direction
//We scroll at SCROLLSPEED per 100 ms. That constant is defined in KM_Global_Data
procedure TViewport.DoScrolling;
const DirectionsBitfield:array[0..12]of byte = (0,c_Scroll6,c_Scroll0,c_Scroll7,c_Scroll2,0,c_Scroll1,0,c_Scroll4,c_Scroll5,0,0,c_Scroll3);
var ScrollAdv: integer; Temp:byte;
begin
  Temp:=0; //That is our bitfield variable for directions, 0..12 range
  //    3 2 6  These are directions
  //    1 * 4  They are converted from bitfield to actual cursor constants, see Arr array
  //    9 8 12

  ScrollAdv := SCROLLSPEED + byte(fGameSettings.IsFastScroll)*3; //3 times faster

  //Left, Top, Right, Bottom
  if Mouse.CursorPos.X < SCROLLFLEX then begin inc(Temp,1); dec(XCoord,ScrollAdv); end;
  if Mouse.CursorPos.Y < SCROLLFLEX then begin inc(Temp,2); dec(YCoord,ScrollAdv); end;
  if Mouse.CursorPos.X > Screen.Width -1-SCROLLFLEX then begin inc(Temp,4); inc(XCoord,ScrollAdv); end;
  if Mouse.CursorPos.Y > Screen.Height-1-SCROLLFLEX then begin inc(Temp,8); inc(YCoord,ScrollAdv); end;

  //Now do actual the scrolling, if needed
  if Temp<>0 then
  begin
    SetCenter(XCoord,YCoord); //EnsureRanges
    Screen.Cursor :=DirectionsBitfield[Temp]; //Sample cursor type from bitfield value
    Scrolling := true; //Stop OnMouseOver from overriding my cursor changes
  end else begin
    Scrolling := false; //Allow cursor changes to be overriden and reset if still on a scrolling cursor
    if (Screen.Cursor in [c_Scroll6..c_Scroll5]) then //Which is 2..8, since directions are not incremental
      Screen.Cursor := c_Default;
  end;
end;

end.

