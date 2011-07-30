unit BGRAGradients;

{$mode objfpc}{$H+}

interface

{ Here are various functions that draw gradients, shadow and lighting }

uses
  Graphics, Classes, BGRABitmap, BGRABitmapTypes, BGRABlend
  {$ifdef CPUI386},mmx{$endif};

{ Creates a bitmap with the specified text horizontally centered and with a shadow }
function TextShadow(AWidth,AHeight: Integer; AText: String; AFontHeight: Integer; ATextColor,AShadowColor: TBGRAPixel;
  AOffSetX,AOffSetY: Integer; ARadius: Integer = 0; AFontStyle: TFontStyles = []; AFontName: String = 'Default'; AShowText: Boolean = True): TBGRABitmap;

{----------------------------------------------------------------------}
{ Functions to draw multiple gradients.
  See : http://wiki.lazarus.freepascal.org/Double_Gradient#nGradient }
type
  TnGradientInfo = record
    StartColor,StopColor: TBGRAPixel;
    Direction: TGradientDirection;
    EndPercent : single; // Position from 0 to 1
  end;

function nGradientInfo(StartColor, StopColor: TBGRAPixel; Direction: TGradientDirection; EndPercent: Single): TnGradientInfo;

function nGradientAlphaFill(ARect: TRect; ADir: TGradientDirection; const AGradient: array of TnGradientInfo): TBGRABitmap;
function nGradientAlphaFill(AWidth, AHeight: Integer; ADir: TGradientDirection; const AGradient: array of TnGradientInfo): TBGRABitmap;
procedure nGradientAlphaFill(ACanvas: TCanvas; ARect: TRect; ADir: TGradientDirection; const AGradient: array of TnGradientInfo);
procedure nGradientAlphaFill(ABitmap: TBGRABitmap; ARect: TRect; ADir: TGradientDirection; const AGradient: array of TnGradientInfo);

function DoubleGradientAlphaFill(ARect: TRect; AStart1,AStop1,AStart2,AStop2: TBGRAPixel;
                                 ADirection1,ADirection2,ADir: TGradientDirection; AValue: Single): TBGRABitmap;
function DoubleGradientAlphaFill(AWidth,AHeight: Integer; AStart1,AStop1,AStart2,AStop2: TBGRAPixel;
                                 ADirection1,ADirection2,ADir: TGradientDirection; AValue: Single): TBGRABitmap;
procedure DoubleGradientAlphaFill(ACanvas: TCanvas; ARect: TRect; AStart1,AStop1,AStart2,AStop2: TBGRAPixel;
                                 ADirection1,ADirection2,ADir: TGradientDirection; AValue: Single);
procedure DoubleGradientAlphaFill(ABitmap: TBGRABitmap; ARect: TRect; AStart1,AStop1,AStart2,AStop2: TBGRAPixel;
                                 ADirection1,ADirection2,ADir: TGradientDirection; AValue: Single);

{----------------------------------------------------------------------}
{ Phong shading functions. Use a height map (grayscale image or a precise map filled with MapHeightToBGRA)
  to determine orientation and position of the surface.

  Phong shading consist in adding an ambiant light, a diffuse light (angle between light and object),
  and a specular light (angle between light, object and observer, i.e. reflected light) }

type
  TRectangleMapOption = (rmoNoLeftBorder,rmoNoTopBorder,rmoNoRightBorder,rmoNoBottomBorder,rmoLinearBorder);
  TRectangleMapOptions = set of TRectangleMapOption;

  { TPhongShading }

  TPhongShading = class
    LightSourceIntensity : Single; //global intensity of the light

    LightSourceDistanceTerm,       //minimum distance always added (positive value)
    LightSourceDistanceFactor,     //how much actual distance is taken into account (usually 0 or 1)
    LightDestFactor : Single;      //how much the location of the lightened pixel is taken into account (usually 0 or 1)

    LightPosition : TPoint;
    LightPositionZ : Integer;
    LightColor: TBGRAPixel;        //color of the light reflection

    SpecularFactor,                //how much light is reflected (0..1)
    SpecularIndex : Single;        //how concentrated reflected light is (positive value)

    AmbientFactor,                 //ambiant lighting whereever the point is (0..1)
    DiffusionFactor,               //diffusion, i.e. how much pixels are lightened by light source (0..1)
    NegativeDiffusionFactor : Single; //how much hidden surface are darkened (0..1)
    DiffuseSaturation: Boolean;    //when diffusion saturates, use light color to show it

    constructor Create;

    { Render the specified map on the destination bitmap with one solid color. Map altitude
      indicate the global height of the map. }
    procedure Draw(dest: TBGRABitmap; map: TBGRABitmap; mapAltitude: integer; ofsX,ofsY: integer;
                   Color : TBGRAPixel);

    { Render with a color map of the same size as the height map. Map altitude
      indicate the global height of the map. }
    procedure Draw(dest: TBGRABitmap; map: TBGRABitmap; mapAltitude: integer; ofsX,ofsY: integer;
                   ColorMap : TBGRABitmap);

    { Draw a cone of the specified color }
    procedure DrawCone(dest: TBGRABitmap; X,Y,Size,Altitude: Integer; Color: TBGRAPixel);

    { Draw a hemisphere of the specified color }
    procedure DrawSphere(dest: TBGRABitmap; bounds: TRect; Altitude: Integer; Color: TBGRAPixel);

    { Draw a rectangle of the specified color }
    procedure DrawRectangle(dest: TBGRABitmap; bounds: TRect; Border,Altitude: Integer; Color: TBGRAPixel; RoundCorners: Boolean; Options: TRectangleMapOptions);
  protected

    procedure DrawMapNormal(dest: TBGRABitmap; map: TBGRABitmap; mapAltitude: integer; ofsX,ofsY: integer;
                   ColorMap : TBGRABitmap);
    procedure DrawColorNormal(dest: TBGRABitmap; map: TBGRABitmap; mapAltitude: integer; ofsX,ofsY: integer;
                   Color : TBGRAPixel);

    {$ifdef CPUI386}
    procedure DrawMapSSE(dest: TBGRABitmap; map: TBGRABitmap; mapAltitude: integer; ofsX,ofsY: integer;
                   ColorMap : TBGRABitmap);
    procedure DrawColorSSE(dest: TBGRABitmap; map: TBGRABitmap; mapAltitude: integer; ofsX,ofsY: integer;
                   Color : TBGRAPixel);
    {$endif}

  end;

{ Create a grayscale height map for a cone }
function CreateConeMap(size: integer): TBGRABitmap;

{ Create a grayscale height map for a sphere (may not be precise enough) }
function CreateSphereMap(width,height: integer): TBGRABitmap;

{ Create a precise height map for a sphere (not grayscale anymore but more precise) }
function CreateSpherePreciseMap(width,height: integer): TBGRABitmap;

{ Create a rectangle height map with a border }
function CreateRectangleMap(width,height,border: integer; options: TRectangleMapOptions = []): TBGRABitmap;

{ Create a round rectangle height map with a border }
function CreateRoundRectangleMap(width,height,border: integer; options: TRectangleMapOptions = []): TBGRABitmap;

{ Get height [0..1] stored in a TBGRAPixel }
function MapHeight(Color: TBGRAPixel): Single;

{ Get TBGRAPixel to store height [0..1] }
function MapHeightToBGRA(Height: Single; Alpha: Byte): TBGRAPixel;

{---------- Perlin Noise -------------}
{ Random image using a superposition of interpolated random values.
  See : http://wiki.lazarus.freepascal.org/Perlin_Noise
        http://freespace.virgin.net/hugo.elias/models/m_perlin.htm }

{ Creates a non-tilable random grayscale image }
function CreatePerlinNoiseMap(AWidth, AHeight: integer; HorizontalPeriod: Single = 1;
  VerticalPeriod: Single = 1; Exponent: Double = 1; ResampleFilter: TResampleFilter = rfCosine): TBGRABitmap;

{ Creates a tilable random grayscale image }
function CreateCyclicPerlinNoiseMap(AWidth, AHeight: integer; HorizontalPeriod: Single = 1;
  VerticalPeriod: Single = 1; Exponent: Double = 1; ResampleFilter: TResampleFilter = rfCosine): TBGRABitmap;

implementation

uses Types, SysUtils;

function TextShadow(AWidth,AHeight: Integer; AText: String; AFontHeight: Integer; ATextColor,AShadowColor: TBGRAPixel;
  AOffSetX,AOffSetY: Integer; ARadius: Integer = 0; AFontStyle: TFontStyles = []; AFontName: String = 'Default'; AShowText: Boolean = True): TBGRABitmap;
var
  bmpOut,bmpSdw: TBGRABitmap; OutTxtSize: TSize; OutX,OutY: Integer;
begin
  bmpOut:= TBGRABitmap.Create(AWidth,AHeight);
  bmpOut.FontAntialias:= True;
  bmpOut.FontHeight:= AFontHeight;
  bmpOut.FontStyle:= AFontStyle;
  bmpOut.FontName:= AFontName;

  OutTxtSize:= bmpOut.TextSize(AText);
  OutX:= Round(AWidth/2) - Round(OutTxtSize.cx/2);
  OutY:= Round(AHeight/2) - Round(OutTxtSize.cy/2);

  bmpSdw:= TBGRABitmap.Create(OutTxtSize.cx+2*ARadius,OutTxtSize.cy+2*ARadius);
  bmpSdw.FontAntialias:= True;
  bmpSdw.FontHeight:= AFontHeight;
  bmpSdw.FontStyle:= AFontStyle;
  bmpSdw.FontName:= AFontName;

  bmpSdw.TextOut(ARadius,ARadius,AText,AShadowColor);
  BGRAReplace(bmpSdw,bmpSdw.FilterBlurRadial(ARadius,rbFast));
  bmpOut.PutImage(OutX+AOffSetX-ARadius,OutY+AOffSetY-ARadius,bmpSdw,dmDrawWithTransparency);
  bmpSdw.Free;

  if AShowText = True then bmpOut.TextOut(OutX,OutY,AText,ATextColor);

  Result:= bmpOut;
end;

function nGradientInfo(StartColor, StopColor: TBGRAPixel;
  Direction: TGradientDirection; EndPercent: Single): TnGradientInfo;
begin
  result.StartColor := StartColor;
  result.StopColor := StopColor;
  result.Direction := Direction;
  result.EndPercent := EndPercent;
end;

function DoubleGradientAlphaFill(ARect: TRect; AStart1,AStop1,AStart2,AStop2: TBGRAPixel;
  ADirection1,ADirection2,ADir: TGradientDirection; AValue: Single): TBGRABitmap;
var
  ABitmap: TBGRABitmap;
  ARect1,ARect2: TRect;
  APoint1,APoint2,APoint3,APoint4: TPointF;
begin
  Dec(ARect.Right, ARect.Left);
  ARect.Left := 0;
  Dec(ARect.Bottom,ARect.Top);
  ARect.Top := 0;

  ABitmap := TBGRABitmap.Create(ARect.Right,ARect.Bottom);

  if AValue <> 0 then ARect1:=ARect;
  if AValue <> 1 then ARect2:=ARect;

  if ADir = gdVertical then begin
    ARect1.Bottom:=Round(ARect1.Bottom * AValue);
    ARect2.Top:=ARect1.Bottom;
  end
  else if ADir = gdHorizontal then begin
    ARect1.Right:=Round(ARect1.Right * AValue);
    ARect2.Left:=ARect1.Right;
  end;
  if ADirection1 = gdVertical then begin
    APoint1:=PointF(ARect1.Left,ARect1.Top);
    APoint2:=PointF(ARect1.Left,ARect1.Bottom);
  end
  else if ADirection1 = gdHorizontal then begin
    APoint1:=PointF(ARect1.Left,ARect1.Top);
    APoint2:=PointF(ARect1.Right,ARect1.Top);
  end;
  if ADirection2 = gdVertical then begin
    APoint3:=PointF(ARect2.Left,ARect2.Top);
    APoint4:=PointF(ARect2.Left,ARect2.Bottom);
  end
  else if ADirection2 = gdHorizontal then begin
    APoint3:=PointF(ARect2.Left,ARect2.Top);
    APoint4:=PointF(ARect2.Right,ARect2.Top);
  end;

  if AValue <> 0 then
    ABitmap.GradientFill(ARect1.Left,ARect1.Top,ARect1.Right,ARect1.Bottom,
    AStart1,AStop1,gtLinear,APoint1,APoint2,dmSet,True);
  if AValue <> 1 then
    ABitmap.GradientFill( ARect2.Left,ARect2.Top,ARect2.Right,ARect2.Bottom,
    AStart2,AStop2,gtLinear,APoint3,APoint4,dmSet,True);

  Result:=ABitmap;
end;

function DoubleGradientAlphaFill(AWidth, AHeight: Integer; AStart1, AStop1,
  AStart2, AStop2: TBGRAPixel; ADirection1, ADirection2,
  ADir: TGradientDirection; AValue: Single): TBGRABitmap;
begin
  result := DoubleGradientAlphaFill(Rect(0,0,AWidth,AHeight),
    AStart1,AStop1,AStart2,AStop2,
    ADirection1,ADirection2, ADir, AValue);
end;

procedure DoubleGradientAlphaFill(ACanvas: TCanvas; ARect: TRect; AStart1,
  AStop1, AStart2, AStop2: TBGRAPixel; ADirection1, ADirection2,
  ADir: TGradientDirection; AValue: Single);
var
  bmp: TBGRABitmap;
begin
  bmp := DoubleGradientAlphaFill(ARect,AStart1,AStop1,AStart2,AStop2,ADirection1,ADirection2,ADir,AValue);
  bmp.Draw(ACanvas,ARect.Left,ARect.Top,not bmp.HasTransparentPixels);
  bmp.Free;
end;

procedure DoubleGradientAlphaFill(ABitmap: TBGRABitmap; ARect: TRect; AStart1,
  AStop1, AStart2, AStop2: TBGRAPixel; ADirection1, ADirection2,
  ADir: TGradientDirection; AValue: Single);
var
  bmp: TBGRABitmap;
begin
  bmp := DoubleGradientAlphaFill(ARect,AStart1,AStop1,AStart2,AStop2,ADirection1,ADirection2,ADir,AValue);
  ABitmap.PutImage(ARect.Left,ARect.Top,bmp,dmDrawWithTransparency);
  bmp.Free;
end;

function nGradientAlphaFill(ARect: TRect; ADir: TGradientDirection;
  const AGradient: array of TnGradientInfo): TBGRABitmap;
var
  i:integer;
  AnRect, OldRect: TRect;
  Point1, Point2: TPointF;
begin
  Result := TBGRABitmap.Create(ARect.Right-ARect.Left,ARect.Bottom-ARect.Top);
  Dec(ARect.Right, ARect.Left);
  ARect.Left := 0;
  Dec(ARect.Bottom,ARect.Top);
  ARect.Top := 0;

  OldRect := ARect;

  if ADir = gdVertical then
    OldRect.Bottom := ARect.Top
  else
    OldRect.Right := ARect.Left;

  for i := 0 to high(AGradient) do
  begin
    AnRect:=OldRect;
    if ADir = gdVertical then
    begin
      AnRect.Bottom:=Round((ARect.Bottom-ARect.Top) * AGradient[i].endPercent + ARect.Top);
      AnRect.Top:=OldRect.Bottom;
      Point1:=PointF(AnRect.Left,AnRect.Top);
      Point2:=PointF(AnRect.Left,AnRect.Bottom);
    end
    else
    begin
     AnRect.Right:=Round((ARect.Right-ARect.Left) * AGradient[i].endPercent + ARect.Left);
     AnRect.Left:=OldRect.Right;
     Point1:=PointF(AnRect.Left,AnRect.Top);
     Point2:=PointF(AnRect.Right,AnRect.Top);
    end;
    Result.GradientFill(AnRect.Left,AnRect.Top,AnRect.Right,AnRect.Bottom,
      AGradient[i].StartColor,AGradient[i].StopColor,gtLinear,Point1,Point2,dmSet,True);
    OldRect := AnRect;
  end;
end;

function nGradientAlphaFill(AWidth, AHeight: Integer; ADir: TGradientDirection;
  const AGradient: array of TnGradientInfo): TBGRABitmap;
begin
  result := nGradientAlphaFill(Rect(0,0,AWidth,AHeight),ADir,AGradient);
end;

procedure nGradientAlphaFill(ACanvas: TCanvas; ARect: TRect;
  ADir: TGradientDirection; const AGradient: array of TnGradientInfo);
var
  bmp: TBGRABitmap;
begin
  bmp := nGradientAlphaFill(ARect, ADir, AGradient);
  bmp.Draw(ACanvas,ARect.Left,ARect.Top,not bmp.HasTransparentPixels);
  bmp.Free;
end;

procedure nGradientAlphaFill(ABitmap: TBGRABitmap; ARect: TRect;
  ADir: TGradientDirection; const AGradient: array of TnGradientInfo);
var
  bmp: TBGRABitmap;
begin
  bmp := nGradientAlphaFill(ARect, ADir, AGradient);
  ABitmap.PutImage(ARect.Left,ARect.Top,bmp,dmDrawWithTransparency);
  bmp.Free;
end;

{ TPhongShading }

constructor TPhongShading.Create;
begin
  //set default values
  LightSourceIntensity := 500;
  LightSourceDistanceTerm := 150;
  LightSourceDistanceFactor := 1;
  LightDestFactor := 1;
  LightColor := BGRAWhite;
  AmbientFactor := 0.3;
  DiffusionFactor := 0.9;
  DiffuseSaturation:= False;
  NegativeDiffusionFactor := 0.1;
  SpecularFactor := 0.6;
  SpecularIndex := 10;
  LightPosition := Point(-100,-100);
  LightPositionZ := 100;
end;

type
  TVector3D = record x,y,z,t: single; end;

function Vector3D(x,y,z: single): TVector3D; inline;
begin
  result.x := x;
  result.y := y;
  result.z := z;
  result.t := 0;
end;

function Vector3D(x,y,z,t: single): TVector3D; inline; overload;
begin
  result.x := x;
  result.y := y;
  result.z := z;
  result.t := t;
end;

operator + (const v1,v2: TVector3D): TVector3D; inline;
begin
  result.x := v1.x+v2.x;
  result.y := v1.y+v2.y;
  result.z := v1.z+v2.z;
end;

operator - (const v1,v2: TVector3D): TVector3D; inline;
begin
  result.x := v1.x-v2.x;
  result.y := v1.y-v2.y;
  result.z := v1.z-v2.z;
end;

operator * (const v1: TVector3D; const factor: single): TVector3D; inline;
begin
  result.x := v1.x*factor;
  result.y := v1.y*factor;
  result.z := v1.z*factor;
end;

operator * (const v1,v2: TVector3D): single; inline;
begin
  result := v1.x*v2.x + v1.y*v2.y + v1.z*v2.z;
end;

procedure normalize(var v: TVector3D); inline;
var len: double;
begin
  len := v*v;
  if len = 0 then exit;
  len := sqrt(len);
  v.x /= len;
  v.y /= len;
  v.z /= len;
end;

procedure vectproduct(u,v: TVector3D; out w: TVector3D); overload;
begin
  w.x := u.y*v.z-u.z*v.y;
  w.y := u.z*v.x-u.x*v.z;
  w.z := u.x*v.Y-u.y*v.x;
end;

Const
  PhongLightPrecisionSh = 12;
  PhongLightPrecision = 1 shl PhongLightPrecisionSh;
  PhongLightPrecisionDiv2 = PhongLightPrecision shr 1;

{------------------ Phong drawing ----------------}
{ Look for the fastest method available }
procedure TPhongShading.Draw(dest: TBGRABitmap; map: TBGRABitmap; mapAltitude: integer; ofsX,ofsY: integer;
                             Color : TBGRAPixel);
begin
  {$ifdef CPUI386}
    if is_sse_cpu then
      DrawColorSSE(dest,map,mapAltitude,ofsX,ofsY,Color)
    else
      DrawColorNormal(dest,map,mapAltitude,ofsX,ofsY,Color);
  {$else}
    DrawColorNormal(dest,map,mapAltitude,ofsX,ofsY,Color);
  {$endif}
end;

procedure TPhongShading.Draw(dest: TBGRABitmap; map: TBGRABitmap;
            mapAltitude: integer; ofsX, ofsY: integer; ColorMap: TBGRABitmap);
begin
  {$ifdef CPUI386}
    if is_sse_cpu then
      DrawMapSSE(dest,map,mapAltitude,ofsX,ofsY,ColorMap)
    else
      DrawMapNormal(dest,map,mapAltitude,ofsX,ofsY,ColorMap);
  {$else}
    DrawMapNormal(dest,map,mapAltitude,ofsX,ofsY,ColorMap);
  {$endif}
end;

  {------------------ End of phong drawing ----------------}

procedure TPhongShading.DrawCone(dest: TBGRABitmap; X, Y, Size,
  Altitude: Integer; Color: TBGRAPixel);
var map: TBGRABitmap;
begin
  map := CreateConeMap(Size);
  Draw(dest,map,Altitude,X,Y,Color);
  map.Free;
end;

procedure TPhongShading.DrawSphere(dest: TBGRABitmap; bounds: TRect;
  Altitude: Integer; Color: TBGRAPixel);
var map: TBGRABitmap;
    temp: integer;
begin
  if Bounds.Right < Bounds.Left then
  begin
    temp := Bounds.Left;
    bounds.Left := bounds.Right;
    Bounds.Right := temp;
  end;
  if Bounds.Bottom < Bounds.Top then
  begin
    temp := Bounds.Bottom;
    bounds.Bottom := bounds.Top;
    Bounds.Top := temp;
  end;
  map := CreateSpherePreciseMap(Bounds.Right-Bounds.Left,Bounds.Bottom-Bounds.Top);
  Draw(dest,map,Altitude,bounds.Left,bounds.Top,Color);
  map.Free;
end;

procedure TPhongShading.DrawRectangle(dest: TBGRABitmap; bounds: TRect;
  Border,Altitude: Integer; Color: TBGRAPixel; RoundCorners: Boolean; Options: TRectangleMapOptions);
var map: TBGRABitmap;
    temp: integer;
begin
  if Bounds.Right < Bounds.Left then
  begin
    temp := Bounds.Left;
    bounds.Left := bounds.Right;
    Bounds.Right := temp;
  end;
  if Bounds.Bottom < Bounds.Top then
  begin
    temp := Bounds.Bottom;
    bounds.Bottom := bounds.Top;
    Bounds.Top := temp;
  end;
  if RoundCorners then
    map := CreateRoundRectangleMap(Bounds.Right-Bounds.Left,Bounds.Bottom-Bounds.Top,Border,Options)
  else
    map := CreateRectangleMap(Bounds.Right-Bounds.Left,Bounds.Bottom-Bounds.Top,Border,Options);
  Draw(dest,map,Altitude,bounds.Left,bounds.Top,Color);
  map.Free;
end;

procedure TPhongShading.DrawMapNormal(dest: TBGRABitmap; map: TBGRABitmap;
  mapAltitude: integer; ofsX, ofsY: integer; ColorMap: TBGRABitmap);
  {$I phongdraw.inc }

procedure TPhongShading.DrawColorNormal(dest: TBGRABitmap; map: TBGRABitmap;
  mapAltitude: integer; ofsX, ofsY: integer; Color: TBGRAPixel);
  {$define PARAM_SIMPLECOLOR}
  {$I phongdraw.inc }

{$ifdef CPUI386}
procedure TPhongShading.DrawMapSSE(dest: TBGRABitmap; map: TBGRABitmap;
  mapAltitude: integer; ofsX, ofsY: integer; ColorMap: TBGRABitmap);
  {$define PARAM_PHONGSSE}
  {$I phongdraw.inc }

procedure TPhongShading.DrawColorSSE(dest: TBGRABitmap; map: TBGRABitmap;
  mapAltitude: integer; ofsX, ofsY: integer; Color: TBGRAPixel);
  {$define PARAM_PHONGSSE}
  {$define PARAM_SIMPLECOLOR}
  {$I phongdraw.inc }

{$endif}

{************************ maps ***********************************}

function CreateConeMap(size: integer): TBGRABitmap;
var cx,cy,r: single;
    mask: TBGRABitmap;
begin
  cx := (size-1)/2;
  cy := (size-1)/2;
  r := (size-1)/2;
  result := TBGRABitmap.Create(size,size);
  result.GradientFill(0,0,size,size,BGRAWhite,BGRABlack,gtRadial,PointF(cx,cy),PointF(cx+r,cy),dmSet,False);

  mask := TBGRABitmap.Create(size,size,BGRABlack);
  mask.FillEllipseAntialias(cx,cy,r,r,BGRAWhite);
  result.ApplyMask(mask);
  mask.Free;
end;

function CreateSphereMap(width,height: integer): TBGRABitmap;
var cx,cy,rx,ry,d: single;
    xb,yb: integer;
    p: PBGRAPixel;
    h: integer;
    mask: TBGRABitmap;
begin
  result := TBGRABitmap.Create(width,height);
  cx := (width-1)/2;
  cy := (height-1)/2;
  rx := (width-1)/2;
  ry := (height-1)/2;
  for yb := 0 to height-1 do
  begin
   p := result.scanline[yb];
   for xb := 0 to width-1 do
   begin
     d := sqr((xb-cx)/(rx+1))+sqr((yb-cy)/(ry+1));
     if d >= 1 then
       p^ := BGRAPixelTransparent else
     begin
       h := round(sqrt(1-d)*255);
       p^.red := h;
       p^.green := h;
       p^.blue := h;
       p^.alpha := 255;
     end;
     inc(p);
   end;
  end;
  //antialiased border
  mask := TBGRABitmap.Create(width,height,BGRABlack);
  mask.FillEllipseAntialias(cx,cy,rx,ry,BGRAWhite);
  result.ApplyMask(mask);
  mask.Free;
end;

procedure MapBorderLimit(width,height: integer; options: TRectangleMapOptions; var border: integer);
var maxHoriz,maxVert: integer;
begin
  if [rmoNoLeftBorder,rmoNoRightBorder] <= options then maxHoriz := border else
  if [rmoNoLeftBorder,rmoNoRightBorder] * options = [] then maxHoriz := width div 2 else
    maxHoriz := width;
  if border > maxHoriz then border := maxHoriz;

  if [rmoNoTopBorder,rmoNoBottomBorder] <= options then maxVert := border else
  if [rmoNoTopBorder,rmoNoBottomBorder] * options = [] then maxVert := height div 2 else
    maxVert := height;
  if border > maxVert then border := maxVert;
end;

function CreateSpherePreciseMap(width, height: integer): TBGRABitmap;
var cx,cy,rx,ry,d: single;
    xb,yb: integer;
    p: PBGRAPixel;
    mask: TBGRABitmap;
begin
  result := TBGRABitmap.Create(width,height);
  cx := (width-1)/2;
  cy := (height-1)/2;
  rx := (width-1)/2;
  ry := (height-1)/2;
  for yb := 0 to height-1 do
  begin
   p := result.scanline[yb];
   for xb := 0 to width-1 do
   begin
     d := sqr((xb-cx)/(rx+1))+sqr((yb-cy)/(ry+1));
     if d >= 1 then
       p^ := BGRAPixelTransparent else
       p^ := MapHeightToBGRA(sqrt(1-d),255);
     inc(p);
   end;
  end;
  //antialiased border
  mask := TBGRABitmap.Create(width,height,BGRABlack);
  mask.FillEllipseAntialias(cx,cy,rx,ry,BGRAWhite);
  result.ApplyMask(mask);
  mask.Free;
end;

function CreateRectangleMap(width,height,border: integer; options: TRectangleMapOptions = []): TBGRABitmap;
var xb,yb: integer;
    p: PBGRAPixel;
    h: integer;
begin
  MapBorderLimit(width,height,options,border);

  result := TBGRABitmap.Create(width,height);
  for yb := 0 to height-1 do
  begin
   p := result.scanline[yb];
   for xb := 0 to width-1 do
   begin
     if not (rmoNoLeftBorder in options) and (xb < border) and (yb > xb) and (yb < height-1-xb) then h := xb else
     if not (rmoNoRightBorder in options) and (xb > width-1-border) and (yb > width-1-xb) and (yb < height-1-(width-1-xb)) then h := width-1-xb else
     if not (rmoNoTopBorder in options) and (yb < border) and (xb > yb) and (xb < width-1-yb) then h := yb else
     if not (rmoNoBottomBorder in options) and (yb > height-1-border) and (xb > height-1-yb) and (xb < width-1-(height-1-yb)) then h := height-1-yb else
     if not (rmoNoLeftBorder in options) and (xb < border) then h := xb else
     if not (rmoNoRightBorder in options) and (xb > width-1-border) then h := width-1-xb else
     if not (rmoNoTopBorder in options) and (yb < border) then h := yb else
     if not (rmoNoBottomBorder in options) and (yb > height-1-border) then h := height-1-yb else
     begin
       p^ := BGRAWhite;
       inc(p);
       Continue;
     end;

     if rmoLinearBorder in options then h := h*256 div border else
       h := round(sin((h+1/2)/border*Pi/2)*255);
     p^.red := h;
     p^.green := h;
     p^.blue := h;
     p^.alpha := 255;
     inc(p);
   end;
  end;

  if [rmoNoLeftBorder,rmoNoTopBorder]*Options = [] then
  begin
    result.SetPixel(0,0,BGRAPixelTransparent);
    result.ErasePixel(1,0,128);
    result.ErasePixel(0,1,128);
  end;

  if [rmoNoRightBorder,rmoNoTopBorder]*Options = [] then
  begin
    result.SetPixel(width-1,0,BGRAPixelTransparent);
    result.ErasePixel(width-2,0,128);
    result.ErasePixel(width-1,1,128);
  end;

  if [rmoNoRightBorder,rmoNoBottomBorder]*Options = [] then
  begin
    result.SetPixel(width-1,height-1,BGRAPixelTransparent);
    result.ErasePixel(width-2,height-1,128);
    result.ErasePixel(width-1,height-2,128);
  end;

  if [rmoNoLeftBorder,rmoNoBottomBorder]*Options = [] then
  begin
    result.SetPixel(0,height-1,BGRAPixelTransparent);
    result.ErasePixel(1,height-1,128);
    result.ErasePixel(0,height-2,128);
  end;
end;

function CreateRoundRectangleMap(width,height,border: integer; options: TRectangleMapOptions = []): TBGRABitmap;
var d: single;
    xb,yb: integer;
    p: PBGRAPixel;
    h: integer;
begin
  MapBorderLimit(width,height,options,border);

  result := TBGRABitmap.Create(width,height);
  for yb := 0 to height-1 do
  begin
   p := result.scanline[yb];
   for xb := 0 to width-1 do
   begin
     if not (rmoNoLeftBorder in options) and not (rmoNoTopBorder in options) and (xb < border) and (yb < border) then d := border-sqrt(sqr(border-xb)+sqr(border-yb)) else
     if not (rmoNoLeftBorder in options) and not (rmoNoBottomBorder in options) and (xb < border) and (yb > height-1-border) then d := border-sqrt(sqr(border-xb)+sqr(border-(height-1-yb))) else
     if not (rmoNoRightBorder in options) and not (rmoNoTopBorder in options) and (xb > width-1-border) and (yb < border) then d := border-sqrt(sqr(border-(width-1-xb))+sqr(border-yb)) else
     if not (rmoNoRightBorder in options) and not (rmoNoBottomBorder in options) and (xb > width-1-border) and (yb > height-1-border) then d := border-sqrt(sqr(border-(width-1-xb))+sqr(border-(height-1-yb))) else
     if not (rmoNoLeftBorder in options) and (xb < border) then d := xb else
     if not (rmoNoRightBorder in options) and (xb > width-1-border) then d := width-1-xb else
     if not (rmoNoTopBorder in options) and (yb < border) then d := yb else
     if not (rmoNoBottomBorder in options) and (yb > height-1-border) then d := height-1-yb else
     begin
       p^ := BGRAWhite;
       inc(p);
       Continue;
     end;

     if d < 0 then
       p^ := BGRAPixelTransparent else
     begin
       if rmoLinearBorder in options then h := trunc(d*256/border) else
         h := round(sin((d+1/2)/border*Pi/2)*255);

       p^.red := h;
       p^.green := h;
       p^.blue := h;
       if d < 1 then p^.alpha := round(d*255) else
         p^.alpha := 255;
     end;
     inc(p);
   end;
  end;
end;

function CreatePerlinNoiseMap(AWidth, AHeight: integer; HorizontalPeriod: Single;
  VerticalPeriod: Single; Exponent: Double = 1; ResampleFilter: TResampleFilter = rfCosine): TBGRABitmap;

  procedure AddNoise(frequencyH, frequencyV: integer; amplitude: byte; dest: TBGRABitmap);
  var small,resampled: TBGRABitmap;
      p: PBGRAPixel;
      i: Integer;
  begin
    if (frequencyH = 0) or (frequencyV = 0) then exit;
    small := TBGRABitmap.Create(frequencyH,frequencyV);
    p := small.data;
    for i := 0 to small.NbPixels-1 do
    begin
      p^.red := random(amplitude);
      p^.green := p^.red;
      p^.blue := p^.green;
      p^.alpha := 255;
      inc(p);
    end;
    small.ResampleFilter := ResampleFilter;
    resampled := small.Resample(dest.Width,dest.Height) as TBGRABitmap;
    dest.BlendImage(0,0,resampled,boAdditive);
    resampled.Free;
    small.Free;
  end;

var
  i: Integer;
  temp: TBGRABitmap;

begin
  result := TBGRABitmap.Create(AWidth,AHeight);
  for i := 0 to 5 do
    AddNoise(round(AWidth / HorizontalPeriod / (32 shr i)),round(AHeight / VerticalPeriod / (32 shr i)), round(exp(ln((128 shr i)/128)*Exponent)*128),result);

  temp := result.FilterNormalize(False) as TBGRABitmap;
  result.Free;
  result := temp;

  temp := result.FilterBlurRadial(1,rbNormal) as TBGRABitmap;
  result.Free;
  result := temp;
end;

function CreateCyclicPerlinNoiseMap(AWidth, AHeight: integer; HorizontalPeriod: Single = 1;
  VerticalPeriod: Single = 1; Exponent: Double = 1; ResampleFilter: TResampleFilter = rfCosine): TBGRABitmap;

  procedure AddNoise(frequencyH, frequencyV: integer; amplitude: byte; dest: TBGRABitmap);
  var small,cycled,resampled: TBGRABitmap;
      p: PBGRAPixel;
      i: Integer;
  begin
    if (frequencyH = 0) or (frequencyV = 0) then exit;
    small := TBGRABitmap.Create(frequencyH,frequencyV);
    p := small.data;
    for i := 0 to small.NbPixels-1 do
    begin
      p^.red := random(amplitude);
      p^.green := p^.red;
      p^.blue := p^.green;
      p^.alpha := 255;
      inc(p);
    end;
    cycled := small.GetPart(rect(-2,-2,small.Width+2,small.Height+2)) as TBGRABitmap;
    cycled.ResampleFilter := ResampleFilter;
    resampled := cycled.Resample(round((cycled.Width-1)*(dest.Width/frequencyH)),round((cycled.Height-1)*(dest.Height/frequencyV))) as TBGRABitmap;
    dest.BlendImage(round(-2*(dest.Width/frequencyH)),round(-2*(dest.Height/frequencyV)),resampled,boAdditive);
    resampled.Free;
    cycled.Free;
    small.Free;
  end;

var
  i: Integer;
  temp: TBGRABitmap;

begin
  result := TBGRABitmap.Create(AWidth,AHeight);
  for i := 0 to 5 do
    AddNoise(round(AWidth / HorizontalPeriod / (32 shr i)),round(AHeight / VerticalPeriod / (32 shr i)), round(exp(ln((128 shr i)/128)*Exponent)*128),result);

  temp := result.FilterNormalize(False) as TBGRABitmap;
  result.Free;
  result := temp;

  temp := result.FilterBlurRadial(1,rbNormal) as TBGRABitmap;
  result.Free;
  result := temp;
end;

function MapHeight(Color: TBGRAPixel): Single;
var intval: integer;
begin
  intval := color.Green shl 16 + color.red shl 8 + color.blue;
  result := intval/16777215;
end;

function MapHeightToBGRA(Height: Single; Alpha: Byte): TBGRAPixel;
var intval: integer;
begin
  if Height >= 1 then result := BGRA(255,255,255,alpha) else
  if Height <= 0 then result := BGRA(0,0,0,alpha) else
  begin
    intval := round(Height*16777215);
    result := BGRA(intval shr 8,intval shr 16,intval,alpha);
  end;
end;

end.

