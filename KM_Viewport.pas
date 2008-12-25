unit KM_Viewport;
interface

uses StdCtrls, ExtCtrls, SysUtils, Math, Types, Graphics;

type

{ Here should be viewport routines }
//I guess they should be combined with TMinimap somehow..? 
TViewport = class
private
protected
public
Zoom:single;
XCoord,YCoord:integer;
ViewRect:TRect;
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
  procedure ReSize(X,Y:word);
  procedure Repaint(); //Repaint minimap
published
end;


implementation
uses KM_Global_Data, KM_Defaults, KM_Terrain, KM_Unit1;

constructor TViewport.Create;
begin
Zoom:=1;
end;

procedure TViewport.SetZoom(NewZoom:single);
begin
Zoom:=NewZoom;
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

procedure TViewport.SetCenter(NewX,NewY:integer);
begin
XCoord:=EnsureRange(NewX,1,fTerrain.MapX);
YCoord:=EnsureRange(NewY,1,fTerrain.MapY);
end;

//Acquire boundaries of area visible to user
function TViewport.GetClip():TRect;
begin
Result.Left  :=max(round(XCoord-(ViewWidth/2-ViewRect.Left)/CellSize/Zoom),1);
Result.Right :=min(round(XCoord+(ViewWidth/2+ViewRect.Left)/CellSize/Zoom)+1,fTerrain.MapX-1);
Result.Top   :=max(round(YCoord-ViewHeight/CellSize/2/Zoom),1);
Result.Bottom:=min(round(YCoord+ViewHeight/CellSize/2/Zoom)+4,fTerrain.MapY-1);
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
      mmShape.Width:=round(ViewWidth/CellSize/Zoom);
      mmShape.Height:=round(ViewHeight/CellSize/Zoom);
      mmLabel.Caption:=inttostr(round(Zoom*100))+'%';
      mmShape.Left:=round(XCoord+mmMiniMap.Left +ViewRect.Left/CellSize/Zoom -mmShape.Width  div 2);
      mmShape.Top :=YCoord+mmMiniMap.Top -mmShape.Height div 2;
      mmShape.Refresh;
    end;
end;

procedure TMiniMap.Repaint();
var i,k:integer; bm:TBitmap; ID,Light,Team:integer; Loc:TKMPointList;
begin
  bm:=TBitmap.Create;
  bm.Width:=fTerrain.MapX;
  bm.Height:=fTerrain.MapY;

  for i:=1 to fTerrain.MapY do for k:=1 to fTerrain.MapX do begin
    ID:=fTerrain.Land[i,k].Terrain+1;
    Light:=round(fTerrain.Land[i,k].Light*64); //Originally it's -1..1 range
    Team:=byte(fTerrain.Land[i,k].TileOwner);
    if fTerrain.Land[i,k].TileOwner=play_none then
      bm.Canvas.Pixels[k-1,i-1]:=EnsureRange(TileMMColor[ID].R+Light,0,255)+
                                 EnsureRange(TileMMColor[ID].G+Light,0,255)*256+
                                 EnsureRange(TileMMColor[ID].B+Light,0,255)*65536
    else
      bm.Canvas.Pixels[k-1,i-1]:=TeamColors[Team];
  //bm.Canvas.Pixels[k-1,i-1]:=random(16777214);
  end;

  Loc:=TKMPointList.Create;
  for i:=1 to MaxPlayers do begin
    ControlList.GetUnitLocations(TPlayerID(i),Loc);
    for k:=1 to Loc.Count do
      bm.Canvas.Pixels[Loc.List[k].X-1,Loc.List[k].Y-1]:=TeamColors[i];
  end;

mmMiniMap.Canvas.StretchDraw(mmMiniMap.Canvas.ClipRect,bm);
end;

procedure TMiniMap.ReSize(X,Y:word);
begin
mmMiniMap.Left:=(MaxMapSize-X) div 2;
mmMiniMap.Top:=(MaxMapSize-Y) div 2;
mmMiniMap.Width:=X;
mmMiniMap.Height:=Y;
end;

end.

