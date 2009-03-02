unit KM_Viewport;
interface            
uses StdCtrls, ExtCtrls, SysUtils, Math, Types, Graphics, KromUtils, KromOGLUtils, OpenGL;

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
  property SetZoom:single write Zoom;
  procedure SetArea(NewWidth,NewHeight:integer);
  function GetCenter():TKMPoint;
  procedure SetCenter(NewX,NewY:integer);
  function GetClip():TRect; //returns visible area dimensions in map space
published
end;

var
  fViewport: TViewport;

implementation
uses KM_Defaults, KM_Terrain, KM_Unit1, KM_Users;

constructor TViewport.Create;
begin
  Zoom:=1;
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
end;

procedure TViewport.SetCenter(NewX,NewY:integer);
begin
  XCoord:=EnsureRange(NewX,1,fTerrain.MapX);
  YCoord:=EnsureRange(NewY,1,fTerrain.MapY);
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

end.

