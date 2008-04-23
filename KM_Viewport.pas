unit KM_Viewport;
interface

uses StdCtrls, ExtCtrls, SysUtils, Math, Types;

type

{ Here should be viewport routines }
//I guess they should be combined with TMinimap somehow..? 
TViewport = class
private
protected
public
Zoom:single;
XCoord,YCoord:integer;
ViewWidth,ViewHeight:integer;
  constructor Create;
  procedure SetZoom(NewZoom:single);
  procedure SetArea(NewWidth,NewHeight:integer);
  procedure SetCenter(NewX,NewY:integer);
  function GetClip():TRect; //returns visible are dimensions in map space
published
end;

{ Here should be Minimap routines }
TMiniMap = class
private
mmShape:TShape; //This is rectangle representing view area of map
mmLabel:TLabel; //This is text label with zoom level in %%
mmMiniMap:TImage; //This is where actual minimap image is drawn
protected
public
  constructor Create(inShape:TShape; inMiniMap:TImage; inLabel:TLabel); //Provide three key elements
  procedure SetRect(Viewport:TViewport); //Update view area position and size, use TViewport info
  procedure Repaint(); //Repaint minimap
published
end;


implementation
uses KM_Global_Data, KM_Defaults, KM_Terrain, KM_Unit1;

constructor TViewport.Create;
begin
Zoom:=10;
end;

procedure TViewport.SetZoom(NewZoom:single);
begin
Zoom:=NewZoom;
end;

procedure TViewport.SetArea(NewWidth,NewHeight:integer);
begin
ViewWidth:=NewWidth;
ViewHeight:=NewHeight;
end;

procedure TViewport.SetCenter(NewX,NewY:integer);
begin
XCoord:=EnsureRange(NewX,1,Map.X);
YCoord:=EnsureRange(NewY,1,Map.Y);
end;

function TViewport.GetClip():TRect;
begin
Result.Left  :=max(round(XCoord-ViewWidth/CellSize/2/Zoom*10),1);
Result.Right :=min(round(XCoord+ViewWidth/CellSize/2/Zoom*10)+1,Map.X-1);
Result.Top   :=max(round(YCoord-ViewHeight/CellSize/2/Zoom*10),1);
Result.Bottom:=min(round(YCoord+ViewHeight/CellSize/2/Zoom*10)+4,Map.Y-1); 
end;

constructor TMiniMap.Create(inShape:TShape; inMiniMap:TImage; inLabel:TLabel);
begin
mmShape:=inShape;
mmLabel:=inLabel;
mmMiniMap:=inMiniMap;
end;

procedure TMiniMap.SetRect(Viewport:TViewport);
begin
  with Viewport do
    begin
      mmShape.Width:=round(ViewWidth/CellSize/Zoom*10);
      mmShape.Height:=round(ViewHeight/CellSize/Zoom*10);
      mmLabel.Caption:=inttostr(round(Zoom*10))+'%';
      mmShape.Left:=XCoord+mmMiniMap.Left-mmShape.Width  div 2;
      mmShape.Top :=YCoord+mmMiniMap.Top -mmShape.Height div 2;
      mmShape.Refresh;
    end;
end;

procedure TMiniMap.Repaint();
var i,k:integer;
begin
for i:=1 to Map.Y do for k:=1 to Map.X do
mmMiniMap.Canvas.Pixels[k-1,i-1]:=TileMMColor[fTerrain.Land[i,k].Terrain+1];

if Mission<>nil then
for i:=1 to Map.Y do for k:=1 to Map.X do
if Mission.Roads[k,i] then
mmMiniMap.Canvas.Pixels[k-1,i-1]:=R[Mission.Owner[k,i]]+G[Mission.Owner[k,i]]*256+B[Mission.Owner[k,i]]*65536;

mmMiniMap.Left:=(MaxMapSize-Map.X) div 2;
mmMiniMap.Top:=(MaxMapSize-Map.Y) div 2;
mmMiniMap.Width:=Map.X;
mmMiniMap.Height:=Map.Y;
end;

end.
