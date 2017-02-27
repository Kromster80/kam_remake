unit BGRAPolygon;

{$mode objfpc}{$H+}

{ This unit contains polygon drawing functions and spline functions.

  Shapes are drawn using a TFillShapeInfo object, which calculates the
  intersection of an horizontal line and the polygon.

  Various shapes are handled :
  - TFillPolyInfo : polygon
  - TFillEllipseInfo : ellipse
  - TFillBorderEllipseInfo : ellipse border
  - TFillRoundRectangleInfo : round rectangle (or other corners)
  - TFillBorderRoundRectInfo : round rectangle border

  Various fill modes :
  - Alternate : each time there is an intersection, it enters or go out of the polygon
  - Winding : filled when the sum of ascending and descending intersection is non zero
  - Color : fill with a color defined as a TBGRAPixel argument
  - Erase : erase with an alpha in the TBGRAPixel argument
  - Texture : draws a texture with the IBGRAScanner argument

  Various border handling :
  - aliased : one horizontal line intersection is calculated per pixel in the vertical loop
  - antialiased : more lines are calculated and a density is computed by adding them together
  - multi-polygon antialiasing and superposition (TBGRAMultiShapeFiller) : same as above but
    by combining multiple polygons at the same time, and optionally subtracting top polygons
  }

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRAFillInfo, Graphics;

procedure FillShapeAntialias(bmp: TBGRACustomBitmap; shapeInfo: TFillShapeInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean);
procedure FillShapeAntialiasWithTexture(bmp: TBGRACustomBitmap; shapeInfo: TFillShapeInfo;
  scan: IBGRAScanner; NonZeroWinding: boolean);
procedure FillShapeAliased(bmp: TBGRACustomBitmap; shapeInfo: TFillShapeInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode; AliasingIncludeBottomRight: Boolean= false);

type

  { TBGRAMultishapeFiller }

  TBGRAMultishapeFiller = class
  protected
    nbShapes: integer;
    shapes: array of record
        info: TFillShapeInfo;
        internalInfo: boolean;
        texture: IBGRAScanner;
        internalTexture: TObject;
        color: TExpandedPixel;
        bounds: TRect;
      end;
    procedure AddShape(AInfo: TFillShapeInfo; AInternalInfo: boolean; ATexture: IBGRAScanner; AInternalTexture: TObject; AColor: TBGRAPixel);
    function CheckRectangleBorderBounds(var x1, y1, x2, y2: single; w: single): boolean;
  public
    FillMode : TFillMode;
    PolygonOrder: TPolygonOrder;
    Antialiasing: Boolean;
    AliasingIncludeBottomRight: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure AddShape(AShape: TFillShapeInfo; AColor: TBGRAPixel);
    procedure AddShape(AShape: TFillShapeInfo; ATexture: IBGRAScanner);
    procedure AddPolygon(const points: array of TPointF; AColor: TBGRAPixel);
    procedure AddPolygon(const points: array of TPointF; ATexture: IBGRAScanner);
    procedure AddTriangleLinearColor(pt1, pt2, pt3: TPointF; c1, c2, c3: TBGRAPixel);
    procedure AddTriangleLinearMapping(pt1, pt2, pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF);
    procedure AddQuadLinearColor(pt1, pt2, pt3, pt4: TPointF; c1, c2, c3, c4: TBGRAPixel);
    procedure AddQuadLinearMapping(pt1, pt2, pt3, pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF);
    procedure AddQuadPerspectiveMapping(pt1, pt2, pt3, pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF);
    procedure AddEllipse(x, y, rx, ry: single; AColor: TBGRAPixel);
    procedure AddEllipse(x, y, rx, ry: single; ATexture: IBGRAScanner);
    procedure AddEllipseBorder(x, y, rx, ry, w: single; AColor: TBGRAPixel);
    procedure AddEllipseBorder(x, y, rx, ry, w: single; ATexture: IBGRAScanner);
    procedure AddRoundRectangle(x1, y1, x2, y2, rx, ry: single; AColor: TBGRAPixel; options: TRoundRectangleOptions= []);
    procedure AddRoundRectangle(x1, y1, x2, y2, rx, ry: single; ATexture: IBGRAScanner; options: TRoundRectangleOptions= []);
    procedure AddRoundRectangleBorder(x1, y1, x2, y2, rx, ry, w: single; AColor: TBGRAPixel; options: TRoundRectangleOptions= []);
    procedure AddRoundRectangleBorder(x1, y1, x2, y2, rx, ry, w: single; ATexture: IBGRAScanner; options: TRoundRectangleOptions= []);
    procedure AddRectangle(x1, y1, x2, y2: single; AColor: TBGRAPixel);
    procedure AddRectangle(x1, y1, x2, y2: single; ATexture: IBGRAScanner);
    procedure AddRectangleBorder(x1, y1, x2, y2, w: single; AColor: TBGRAPixel);
    procedure AddRectangleBorder(x1, y1, x2, y2, w: single; ATexture: IBGRAScanner);
    procedure Draw(dest: TBGRACustomBitmap);
  end;

procedure FillPolyAliased(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean; drawmode: TDrawMode);
procedure FillPolyAliasedWithTexture(bmp: TBGRACustomBitmap; points: array of TPointF;
  scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode);
procedure FillPolyAntialias(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean);
procedure FillPolyAntialiasWithTexture(bmp: TBGRACustomBitmap; points: array of TPointF;
  scan: IBGRAScanner; NonZeroWinding: boolean);

procedure FillEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry: single;
  c: TBGRAPixel; EraseMode: boolean);
procedure FillEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx, ry: single;
  scan: IBGRAScanner);

procedure BorderEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  c: TBGRAPixel; EraseMode: boolean);
procedure BorderEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  scan: IBGRAScanner);

procedure FillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry: single;
  options: TRoundRectangleOptions; c: TBGRAPixel; EraseMode: boolean);
procedure FillRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry: single;
  options: TRoundRectangleOptions; scan: IBGRAScanner);

procedure BorderRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry, w: single;
  options: TRoundRectangleOptions; c: TBGRAPixel; EraseMode: boolean);
procedure BorderRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry, w: single;
  options: TRoundRectangleOptions; scan: IBGRAScanner);

procedure BorderAndFillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry, w: single;
  options: TRoundRectangleOptions; bordercolor,fillcolor: TBGRAPixel; bordertexture,filltexture: IBGRAScanner; EraseMode: boolean);

{----------------------- Spline ------------------}

function SplineVertexToSide(y0, y1, y2, y3: single; t: single): single;
function ComputeBezierCurve(const curve: TCubicBezierCurve): ArrayOfTPointF; overload;
function ComputeBezierCurve(const curve: TQuadraticBezierCurve): ArrayOfTPointF; overload;
function ComputeBezierSpline(const spline: array of TCubicBezierCurve): ArrayOfTPointF; overload;
function ComputeBezierSpline(const spline: array of TQuadraticBezierCurve): ArrayOfTPointF; overload;
function ComputeClosedSpline(const points: array of TPointF; Style: TSplineStyle): ArrayOfTPointF;
function ComputeOpenedSpline(const points: array of TPointF; Style: TSplineStyle; EndCoeff: single = 0.25): ArrayOfTPointF;

implementation

uses Math, BGRABlend, BGRAGradientScanner, BGRATransform, BGRAResample;

procedure FillShapeAntialias(bmp: TBGRACustomBitmap; shapeInfo: TFillShapeInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean);
var
  inter:   array of TIntersectionInfo;
  nbInter: integer;

  firstScan, lastScan: record
    inter:   array of TIntersectionInfo;
    nbInter: integer;
  end;

  miny, maxy, minx, maxx,
  densMinX, densMaxX: integer;

  density: PDensity;

  xb, yb, yc, i, j: integer;

  x1, x2, x1b,x2b: single;
  ix1, ix2: integer;
  pdest:    PBGRAPixel;
  pdens:    PDensity;

  curvedSeg,optimised: boolean;
  ec: TExpandedPixel;
  c2:TBGRAPixel;
  ScanNextFunc: function():TBGRAPixel of object;
  temp: Single;

  function GetYScan(num: integer): single; inline;
  begin
    result := yb + (num * 2 + 1) / (AntialiasPrecision * 2);
  end;

  procedure SubTriangleDensity(x1,density1, x2, density2: single);
  var ix1,ix2,n: integer;
      slope: single;
    function densityAt(x: single): single; inline;
    begin
      result := (x-x1)*slope+density1;
    end;
  var
      curdens: single;
      pdens: pdensity;
  begin
    if (x1 <> x2) and (x1 < maxx + 1) and (x2 >= minx) then
    begin
      slope := (density2-density1)/(x2-x1);
      if x1 < minx then
      begin
        density1 := densityAt(minx);
        x1 := minx;
      end;
      if x2 >= maxx + 1 then
      begin
        density2 := densityAt(maxx+1);
        x2 := maxx + 1;
      end;
      ix1  := floor(x1);
      ix2  := floor(x2);

      if ix1 = ix2 then
        (density + (ix1 - minx))^ -= round((x2 - x1)*(density1+density2)/2)
      else
      begin
        (density + (ix1 - minx))^ := max(0, (density + (ix1 - minx))^ - round((1 - (x1 - ix1))*(density1+densityAt(ix1+1))/2) );
        if (ix2 <= maxx) then
          (density + (ix2 - minx))^ := max(0, (density + (ix2 - minx))^ - round((x2 - ix2)*(density2+densityAt(ix2))/2) );
      end;
      if ix2 > ix1 + 1 then
      begin
        curdens := densityAt(ix1+1.5);
        pdens := density + (ix1+1 - minx);
        for n := ix2-1-(ix1+1) downto 0 do
        begin
          pdens^ -= round(curdens);
          curdens += slope;
          inc(pdens);
        end;
      end;
    end;
  end;

begin
  if (scan=nil) and (c.alpha=0) then exit;
  If not shapeInfo.ComputeMinMax(minx,miny,maxx,maxy,bmp) then exit;

  inter := shapeInfo.CreateIntersectionArray;
  getmem(density, (maxx - minx + 2)*sizeof(TDensity)); //more for safety
  ec := GammaExpansion(c);
  c2 := c;

  if scan <> nil then ScanNextFunc := @scan.ScanNextPixel;
  curvedSeg := shapeInfo.SegmentsCurved;
  if not curvedSeg then
  begin
    firstScan.inter := shapeInfo.CreateIntersectionArray;
    lastScan.inter := shapeInfo.CreateIntersectionArray;
  end;

  //vertical scan
  for yb := miny to maxy do
  begin
    //mean density
    fillchar(density^,(maxx-minx+1)*sizeof(TDensity),0);

    densMinX := maxx+1;
    densMaxX := minx-1;

    if not curvedSeg then
    begin
      with firstScan do
        shapeInfo.ComputeAndSort(yb+1/256,inter,nbInter,NonZeroWinding);
      with lastScan do
        shapeInfo.ComputeAndSort(yb+255/256,inter,nbInter,NonZeroWinding);
      if firstScan.nbInter = lastScan.nbInter then
      begin
        optimised := firstScan.nbInter >= 2;
        for i := 0 to firstScan.nbInter-1 do
          if firstScan.inter[i].numSegment <> lastScan.inter[i].numSegment then
          begin
            optimised := false;
            break;
          end;
      end else
        optimised := false;

      if optimised then
      begin
        for i := 0 to firstScan.nbinter div 2 - 1 do
        begin
          x1 := firstScan.inter[i+i].interX;
          x1b := lastScan.inter[i+i].interX;
          if (x1 > x1b) then
          begin
            temp := x1;
            x1 := x1b;
            x1b := temp;
          end;
          x2 := firstScan.inter[i+i+1].interX;
          x2b := lastScan.inter[i+i+1].interX;
          if (x2 < x2b) then
          begin
            temp := x2;
            x2 := x2b;
            x2b := temp;
          end;
          {$i filldensitysegment256.inc}
          SubTriangleDensity(x1,256,x1b,0);
          SubTriangleDensity(x2b,0,x2,256);
        end;
      end else
      begin
        for yc := 0 to AntialiasPrecision - 1 do
        begin
          //find intersections
          shapeInfo.ComputeAndSort(GetYScan(yc),inter,nbInter,NonZeroWinding);

          {$i filldensity256.inc}
        end;
      end;
    end else
    begin
      optimised := false;
      //precision scan
      for yc := 0 to AntialiasPrecision - 1 do
      begin
        //find intersections
        shapeInfo.ComputeAndSort(GetYScan(yc),inter,nbInter,NonZeroWinding);

        {$i filldensity256.inc}
      end;
    end;

    if optimised then
      {$i renderdensity256.inc}
    else
      {$define PARAM_ANTIALIASINGFACTOR}
      {$i renderdensity256.inc}
  end;

  for i := 0 to high(inter) do
    inter[i].free;
  if not curvedSeg then
  begin
    with firstScan do
    begin
      for i := 0 to high(inter) do
        inter[i].free;
    end;
    with lastScan do
    begin
      for i := 0 to high(inter) do
        inter[i].free;
    end;
  end;
  freemem(density);

  bmp.InvalidateBitmap;
end;

procedure FillShapeAliased(bmp: TBGRACustomBitmap; shapeInfo: TFillShapeInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode; AliasingIncludeBottomRight: Boolean= false);
var
  inter:    array of TIntersectionInfo;
  nbInter:  integer;

  miny, maxy, minx, maxx: integer;
  xb,yb, i: integer;
  x1, x2: single;
  ix1, ix2: integer;
  pdest: PBGRAPixel;
  AliasingOfs: TPointF;
  ec: TExpandedPixel;

begin
  if (scan=nil) and (c.alpha=0) then exit;
  If not shapeInfo.ComputeMinMax(minx,miny,maxx,maxy,bmp) then exit;
  inter := shapeInfo.CreateIntersectionArray;

  if AliasingIncludeBottomRight then
    AliasingOfs := PointF(0,0) else
    AliasingOfs := PointF(-0.0001,-0.0001);

  ec := GammaExpansion(c);
  if (scan = nil) and (c.alpha = 255) then drawmode := dmSet;

  //vertical scan
  for yb := miny to maxy do
  begin
    //find intersections
    shapeInfo.ComputeAndSort( yb+0.5-AliasingOfs.Y, inter, nbInter, NonZeroWinding);

    for i := 0 to nbinter div 2 - 1 do
    begin
      x1 := inter[i + i].interX-AliasingOfs.X;
      x2 := inter[i + i+ 1].interX-AliasingOfs.X;

      if x1 <> x2 then
      begin
        ComputeAliasedRowBounds(x1,x2, minx,maxx, ix1,ix2);
        if ix1 <= ix2 then
        begin
          //render scanline
          if scan <> nil then //with texture scan
          begin
            pdest := bmp.ScanLine[yb] + ix1;
            scan.ScanMoveTo(ix1,yb);
            ScannerPutPixels(scan,pdest,ix2-ix1+1,drawmode);
          end else
          if EraseMode then //erase with alpha
          begin
            pdest := bmp.ScanLine[yb] + ix1;
            for xb := ix1 to ix2 do
            begin
              ErasePixelInline(pdest, c.alpha);
              Inc(pdest);
            end;
          end
          else
          begin
            case drawmode of
              dmFastBlend: bmp.FastBlendHorizLine(ix1,yb,ix2, c);
              dmDrawWithTransparency: bmp.DrawHorizLine(ix1,yb,ix2, ec);
              dmSet: bmp.SetHorizLine(ix1,yb,ix2, c);
            end;
          end;
        end;
      end;
    end;
  end;

  for i := 0 to high(inter) do
    inter[i].free;

  bmp.InvalidateBitmap;
end;

procedure FillShapeAntialiasWithTexture(bmp: TBGRACustomBitmap;
  shapeInfo: TFillShapeInfo; scan: IBGRAScanner; NonZeroWinding: boolean);
begin
  FillShapeAntialias(bmp,shapeInfo,BGRAPixelTransparent,False,scan,NonZeroWinding);
end;

procedure FillPolyAliased(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean; drawmode: TDrawMode);
var
  info: TFillPolyInfo;
begin
  if length(points) < 3 then
    exit;

  info := TFillPolyInfo.Create(points);
  FillShapeAliased(bmp, info, c, EraseMode, nil, NonZeroWinding, drawmode);
  info.Free;
end;

procedure FillPolyAliasedWithTexture(bmp: TBGRACustomBitmap;
  points: array of TPointF; scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode);
var
  info: TFillPolyInfo;
begin
  if length(points) < 3 then
    exit;

  info := TFillPolyInfo.Create(points);
  FillShapeAliased(bmp, info, BGRAPixelTransparent,False,scan, NonZeroWinding, drawmode);
  info.Free;
end;

procedure FillPolyAntialias(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean);
var
  info: TFillPolyInfo;
begin
  if length(points) < 3 then
    exit;

  info := TFillPolyInfo.Create(points);
  FillShapeAntialias(bmp, info, c, EraseMode, nil, NonZeroWinding);
  info.Free;
end;

procedure FillPolyAntialiasWithTexture(bmp: TBGRACustomBitmap;
  points: array of TPointF; scan: IBGRAScanner; NonZeroWinding: boolean
  );
var
  info: TFillPolyInfo;
begin
  if length(points) < 3 then
    exit;

  info := TFillPolyInfo.Create(points);
  FillShapeAntialiasWithTexture(bmp, info, scan, NonZeroWinding);
  info.Free;
end;

procedure FillEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry: single;
  c: TBGRAPixel; EraseMode: boolean);
var
  info: TFillEllipseInfo;
begin
  if (rx = 0) or (ry = 0) then
    exit;

  info := TFillEllipseInfo.Create(x, y, rx, ry);
  FillShapeAntialias(bmp, info, c, EraseMode, nil, False);
  info.Free;
end;

procedure FillEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx,
  ry: single; scan: IBGRAScanner);
var
  info: TFillEllipseInfo;
begin
  if (rx = 0) or (ry = 0) then
    exit;

  info := TFillEllipseInfo.Create(x, y, rx, ry);
  FillShapeAntialiasWithTexture(bmp, info, scan, False);
  info.Free;
end;

procedure BorderEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  c: TBGRAPixel; EraseMode: boolean);
var
  info: TFillBorderEllipseInfo;
begin
  if (rx = 0) or (ry = 0) or (w=0) then
    exit;
  info := TFillBorderEllipseInfo.Create(x, y, rx, ry, w);
  FillShapeAntialias(bmp, info, c, EraseMode, nil, False);
  info.Free;
end;

procedure BorderEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx,
  ry, w: single; scan: IBGRAScanner);
var
  info: TFillBorderEllipseInfo;
begin
  if (rx = 0) or (ry = 0) or (w=0) then
    exit;
  info := TFillBorderEllipseInfo.Create(x, y, rx, ry, w);
  FillShapeAntialiasWithTexture(bmp, info, scan, False);
  info.Free;
end;

{ TBGRAMultishapeFiller }

procedure TBGRAMultishapeFiller.AddShape(AInfo: TFillShapeInfo; AInternalInfo: boolean; ATexture: IBGRAScanner; AInternalTexture: TObject; AColor: TBGRAPixel);
begin
  if length(shapes) = nbShapes then
    setlength(shapes, (length(shapes)+1)*2);
  with shapes[nbShapes] do
  begin
    info := AInfo;
    internalInfo:= AInternalInfo;
    texture := ATexture;
    internalTexture:= AInternalTexture;
    color := GammaExpansion(AColor);
  end;
  inc(nbShapes);
end;

function TBGRAMultishapeFiller.CheckRectangleBorderBounds(var x1, y1, x2,
  y2: single; w: single): boolean;
var temp: single;
begin
  if x1 > x2 then
  begin
    temp := x1;
    x1 := x2;
    x2 := temp;
  end;
  if y1 > y2 then
  begin
    temp := y1;
    y1 := y2;
    y2 := temp;
  end;
  result := (x2-x1 > w) and (y2-y1 > w);
end;

constructor TBGRAMultishapeFiller.Create;
begin
  nbShapes := 0;
  shapes := nil;
  PolygonOrder := poNone;
  Antialiasing := True;
  AliasingIncludeBottomRight := False;
end;

destructor TBGRAMultishapeFiller.Destroy;
var
  i: Integer;
begin
  for i := 0 to nbShapes-1 do
  begin
    if shapes[i].internalInfo then shapes[i].info.free;
    shapes[i].texture := nil;
    if shapes[i].internalTexture <> nil then shapes[i].internalTexture.Free;
  end;
  shapes := nil;
  inherited Destroy;
end;

procedure TBGRAMultishapeFiller.AddShape(AShape: TFillShapeInfo; AColor: TBGRAPixel);
begin
  AddShape(AShape,False,nil,nil,AColor);
end;

procedure TBGRAMultishapeFiller.AddShape(AShape: TFillShapeInfo;
  ATexture: IBGRAScanner);
begin
  AddShape(AShape,False,ATexture,nil,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddPolygon(const points: array of TPointF;
  AColor: TBGRAPixel);
begin
  if length(points) <= 2 then exit;
  AddShape(TFillPolyInfo.Create(points),True,nil,nil,AColor);
end;

procedure TBGRAMultishapeFiller.AddPolygon(const points: array of TPointF;
  ATexture: IBGRAScanner);
begin
  if length(points) <= 2 then exit;
  AddShape(TFillPolyInfo.Create(points),True,ATexture,nil,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddTriangleLinearColor(pt1, pt2, pt3: TPointF; c1, c2,
  c3: TBGRAPixel);
var
  grad: TBGRAGradientTriangleScanner;
begin
  grad := TBGRAGradientTriangleScanner.Create(pt1,pt2,pt3, c1,c2,c3);
  AddShape(TFillPolyInfo.Create([pt1,pt2,pt3]),True,grad,grad,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddTriangleLinearMapping(pt1, pt2,
  pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF);
var
  mapping: TBGRATriangleLinearMapping;
begin
  mapping := TBGRATriangleLinearMapping.Create(texture, pt1,pt2,pt3, tex1, tex2, tex3);
  AddShape(TFillPolyInfo.Create([pt1,pt2,pt3]),True,mapping,mapping,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddQuadLinearColor(pt1, pt2, pt3, pt4: TPointF;
  c1, c2, c3, c4: TBGRAPixel);
var
  center: TPointF;
  centerColor: TBGRAPixel;
begin
  center := (pt1+pt2+pt3+pt4)*(1/4);
  centerColor := GammaCompression( MergeBGRA(MergeBGRA(GammaExpansion(c1),GammaExpansion(c2)),
                    MergeBGRA(GammaExpansion(c3),GammaExpansion(c4))) );
  AddTriangleLinearColor(pt1,pt2,center, c1,c2,centerColor);
  AddTriangleLinearColor(pt2,pt3,center, c2,c3,centerColor);
  AddTriangleLinearColor(pt3,pt4,center, c3,c4,centerColor);
  AddTriangleLinearColor(pt4,pt1,center, c4,c1,centerColor);
end;

procedure TBGRAMultishapeFiller.AddQuadLinearMapping(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF);
var
  center: TPointF;
  centerTex: TPointF;
begin
  center := (pt1+pt2+pt3+pt4)*(1/4);
  centerTex := (tex1+tex2+tex3+tex4)*(1/4);
  AddTriangleLinearMapping(pt1,pt2,center, texture,tex1,tex2,centerTex);
  AddTriangleLinearMapping(pt2,pt3,center, texture,tex2,tex3,centerTex);
  AddTriangleLinearMapping(pt3,pt4,center, texture,tex3,tex4,centerTex);
  AddTriangleLinearMapping(pt4,pt1,center, texture,tex4,tex1,centerTex);
end;

procedure TBGRAMultishapeFiller.AddQuadPerspectiveMapping(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF);
var persp: TBGRAPerspectiveScannerTransform;
begin
  persp := TBGRAPerspectiveScannerTransform.Create(texture,[tex1,tex2,tex3,tex4],[pt1,pt2,pt3,pt4]);
  AddShape(TFillPolyInfo.Create([pt1,pt2,pt3,pt4]),True,persp,persp,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddEllipse(x, y, rx, ry: single; AColor: TBGRAPixel
  );
begin
  AddShape(TFillEllipseInfo.Create(x,y,rx,ry),True,nil,nil,AColor);
end;

procedure TBGRAMultishapeFiller.AddEllipse(x, y, rx, ry: single;
  ATexture: IBGRAScanner);
begin
  AddShape(TFillEllipseInfo.Create(x,y,rx,ry),True,ATexture,nil,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddEllipseBorder(x, y, rx, ry, w: single;
  AColor: TBGRAPixel);
begin
  AddShape(TFillBorderEllipseInfo.Create(x,y,rx,ry,w),True,nil,nil,AColor);
end;

procedure TBGRAMultishapeFiller.AddEllipseBorder(x, y, rx, ry, w: single;
  ATexture: IBGRAScanner);
begin
  AddShape(TFillBorderEllipseInfo.Create(x,y,rx,ry,w),True,ATexture,nil,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddRoundRectangle(x1, y1, x2, y2, rx, ry: single;
  AColor: TBGRAPixel; options: TRoundRectangleOptions);
begin
  AddShape(TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry,options),True,nil,nil,AColor);
end;

procedure TBGRAMultishapeFiller.AddRoundRectangle(x1, y1, x2, y2, rx, ry: single;
  ATexture: IBGRAScanner; options: TRoundRectangleOptions);
begin
  AddShape(TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry,options),True,
     ATexture,nil,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddRoundRectangleBorder(x1, y1, x2, y2, rx,
  ry, w: single; AColor: TBGRAPixel; options: TRoundRectangleOptions);
begin
  AddShape(TFillBorderRoundRectInfo.Create(x1, y1, x2, y2, rx, ry,w,options),True,
    nil,nil,AColor);
end;

procedure TBGRAMultishapeFiller.AddRoundRectangleBorder(x1, y1, x2, y2, rx, ry,
  w: single; ATexture: IBGRAScanner; options: TRoundRectangleOptions);
begin
  AddShape(TFillBorderRoundRectInfo.Create(x1, y1, x2, y2, rx, ry,w,options),True,
    ATexture,nil,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddRectangle(x1, y1, x2, y2: single;
  AColor: TBGRAPixel);
begin
  AddPolygon([PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)],AColor);
end;

procedure TBGRAMultishapeFiller.AddRectangle(x1, y1, x2, y2: single;
  ATexture: IBGRAScanner);
begin
  AddPolygon([PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)],ATexture);
end;

procedure TBGRAMultishapeFiller.AddRectangleBorder(x1, y1, x2, y2,
  w: single; AColor: TBGRAPixel);
var hw : single;
begin
  hw := w/2;
  if not CheckRectangleBorderBounds(x1,y1,x2,y2,w) then
    AddRectangle(x1-hw,y1-hw,x2+hw,y2+hw,AColor) else
    AddPolygon([PointF(x1-hw,y1-hw),PointF(x2+hw,y1-hw),PointF(x2+hw,y2+hw),PointF(x1-hw,y2+hw),EmptyPointF,
                PointF(x1+hw,y2-hw),PointF(x2-hw,y2-hw),PointF(x2-hw,y1+hw),PointF(x1+hw,y1+hw)],AColor);
end;

procedure TBGRAMultishapeFiller.AddRectangleBorder(x1, y1, x2, y2,
  w: single; ATexture: IBGRAScanner);
var hw : single;
begin
  hw := w/2;
  if not CheckRectangleBorderBounds(x1,y1,x2,y2,w) then
    AddRectangle(x1-hw,y1-hw,x2+hw,y2+hw,ATexture) else
    AddPolygon([PointF(x1-hw,y1-hw),PointF(x2+hw,y1-hw),PointF(x2+hw,y2+hw),PointF(x1-hw,y2+hw),EmptyPointF,
                PointF(x1+hw,y2-hw),PointF(x2-hw,y2-hw),PointF(x2-hw,y1+hw),PointF(x1+hw,y1+hw)],ATexture);
end;

procedure TBGRAMultishapeFiller.Draw(dest: TBGRACustomBitmap);
var
  shapeRow: array of record
    density: PDensity;
    densMinx,densMaxx: integer;
    nbInter: integer;
    inter: array of TIntersectionInfo;
  end;
  shapeRowsList: array of integer;
  NbShapeRows: integer;
  miny, maxy, minx, maxx,
  rowminx, rowmaxx: integer;

  procedure SubstractScanlines(src,dest: integer);
  var i: integer;

    procedure SubstractSegment(srcseg: integer);
    var x1,x2, x3,x4: single;
      j: integer;

      procedure AddSegment(xa,xb: single);
      var nb: PInteger;
          prevNb,k: integer;
      begin
        nb := @shapeRow[dest].nbinter;
        if length(shapeRow[dest].inter) < nb^+2 then
        begin
          prevNb := length(shapeRow[dest].inter);
          setlength(shapeRow[dest].inter, nb^*2+2);
          for k := prevNb to high(shapeRow[dest].inter) do
            shapeRow[dest].inter[k] := shapes[dest].info.CreateIntersectionInfo;
        end;
        shapeRow[dest].inter[nb^].interX := xa;
        shapeRow[dest].inter[nb^+1].interX := xb;
        inc(nb^,2);
      end;

    begin
      x1 := shapeRow[src].inter[(srcseg-1)*2].interX;
      x2 := shapeRow[src].inter[srcseg*2-1].interX;
      for j := shapeRow[dest].nbInter div 2 downto 1 do
      begin
        x3 := shapeRow[dest].inter[(j-1)*2].interX;
        x4 := shapeRow[dest].inter[j*2-1].interX;
        if (x2 <= x3) or (x1 >= x4) then continue; //not overlapping
        if (x1 <= x3) and (x2 >= x4) then
          shapeRow[dest].inter[j*2-1].interX := x3 //empty
        else
        if (x1 <= x3) and (x2 < x4) then
          shapeRow[dest].inter[(j-1)*2].interX := x2 //remove left part
        else
        if (x1 > x3) and (x2 >= x4) then
          shapeRow[dest].inter[j*2-1].interX := x1 else //remove right part
        begin
          //[x1,x2] is inside [x3,x4]
          shapeRow[dest].inter[j*2-1].interX := x1; //left part
          AddSegment(x2,x4);
        end;
      end;
    end;

  begin
    for i := 1 to shapeRow[src].nbInter div 2 do
      SubstractSegment(i);
  end;

var
    AliasingOfs: TPointF;

  procedure AddOneLineDensity(cury: single);
  var
    i,k: integer;
    ix1,ix2: integer;
    x1,x2: single;
  begin
    for k := 0 to NbShapeRows-1 do
      with shapeRow[shapeRowsList[k]], shapes[shapeRowsList[k]] do
      begin
        //find intersections
        info.ComputeAndSort(cury, inter, nbInter, FillMode=fmWinding);
        nbInter := nbInter and not 1; //even
      end;

      case PolygonOrder of
        poLastOnTop: begin
          for k := 1 to NbShapeRows-1 do
            if shapeRow[shapeRowsList[k]].nbInter > 0 then
              for i := 0 to k-1 do
                SubstractScanlines(shapeRowsList[k],shapeRowsList[i]);
        end;
        poFirstOnTop: begin
          for k := 0 to NbShapeRows-2 do
            if shapeRow[shapeRowsList[k]].nbInter > 0 then
              for i := k+1 to NbShapeRows-1 do
                SubstractScanlines(shapeRowsList[k],shapeRowsList[i]);
        end;
      end;

      for k := 0 to NbShapeRows-1 do
      with shapeRow[shapeRowsList[k]] do
      begin
        //fill density
        if not Antialiasing then
        begin
          for i := 0 to nbinter div 2 - 1 do
          begin
            x1 := inter[i + i].interX;
            x2 := inter[i + i + 1].interX;
            ComputeAliasedRowBounds(x1+AliasingOfs.X,x2+AliasingOfs.X,minx,maxx,ix1,ix2);

            if ix1 < densMinx then densMinx := ix1;
            if ix2 > densMaxx then densMaxx := ix2;

            FillWord(density[ix1-minx],ix2-ix1+1,256);
          end;
        end else
          {$I filldensity256.inc}
      end;

      for k := 0 to NbShapeRows-1 do
      with shapeRow[shapeRowsList[k]] do
      begin
        if densMinX < rowminx then rowminx := densMinX;
        if densMaxX > rowmaxx then rowmaxx := densMaxX;
      end;
  end;

type
    TCardinalSum = record
          sumR,sumG,sumB,sumA: cardinal;
        end;

var
  MultiEmpty: boolean;
  bounds: TRect;

  xb, yb, yc, i, j,k: integer;
  pdest:    PBGRAPixel;

  curSum,nextSum: ^TCardinalSum;
  sums: array of TCardinalSum;

  pdens: PDensity;
  w: cardinal;
  ec: TExpandedPixel;
  count: integer;
  ScanNextFunc: function: TBGRAPixel of object;

begin
  if nbShapes = 0 then exit;
  if nbShapes = 1 then
  begin
    if Antialiasing then
      FillShapeAntialias(dest,shapes[0].info,GammaCompression(shapes[0].color),False,shapes[0].texture,FillMode = fmWinding) else
      FillShapeAliased(dest,shapes[0].info,GammaCompression(shapes[0].color),False,shapes[0].texture,FillMode = fmWinding, dmDrawWithTransparency,
        AliasingIncludeBottomRight);
    exit;
  end;
  bounds := Rect(0,0,0,0);
  MultiEmpty := True;
  for k := 0 to nbShapes-1 do
  begin
    If shapes[k].info.ComputeMinMax(minx,miny,maxx,maxy,dest) then
    begin
      shapes[k].bounds := rect(minx,miny,maxx+1,maxy+1);
      if MultiEmpty then
      begin
        MultiEmpty := False;
        bounds := shapes[k].bounds;
      end else
      begin
        if minx < bounds.left then bounds.left := minx;
        if miny < bounds.top then bounds.top := miny;
        if maxx >= bounds.right then bounds.right := maxx+1;
        if maxy >= bounds.bottom then bounds.bottom := maxy+1;
      end;
    end else
      shapes[k].bounds := rect(0,0,0,0);
  end;
  if MultiEmpty then exit;
  minx := bounds.left;
  miny := bounds.top;
  maxx := bounds.right-1;
  maxy := bounds.bottom-1;

  setlength(shapeRow, nbShapes);
  for k := 0 to nbShapes-1 do
  begin
    shapeRow[k].inter := shapes[k].info.CreateIntersectionArray;
    getmem(shapeRow[k].density, (maxx - minx + 2)*sizeof(TDensity)); //more for safety
  end;

  if AliasingIncludeBottomRight then
    AliasingOfs := PointF(0,0) else
    AliasingOfs := PointF(-0.0001,-0.0001);

  setlength(sums,maxx-minx+2); //more for safety
  setlength(shapeRowsList, nbShapes);

  //vertical scan
  for yb := miny to maxy do
  begin
    rowminx := maxx+1;
    rowmaxx := minx-1;

    //init shape rows
    NbShapeRows := 0;
    for k := 0 to nbShapes-1 do
    if (yb >= shapes[k].bounds.top) and (yb < shapes[k].bounds.Bottom) then
    begin
      shapeRowsList[NbShapeRows] := k;
      inc(NbShapeRows);

      fillchar(shapeRow[k].density^,(maxx-minx+1)*sizeof(TDensity),0);
      shapeRow[k].densMinx := maxx+1;
      shapeRow[k].densMaxx := minx-1;
    end;

    If Antialiasing then
    begin
      //precision scan
      for yc := 0 to AntialiasPrecision - 1 do
        AddOneLineDensity( yb + (yc * 2 + 1) / (AntialiasPrecision * 2) );
    end else
    begin
      AddOneLineDensity( yb + 0.5 - AliasingOfs.Y );
    end;

    rowminx := minx;
    rowmaxx := maxx;
    if rowminx <= rowmaxx then
    begin
      if rowminx < minx then rowminx := minx;
      if rowmaxx > maxx then rowmaxx := maxx;

      FillChar(sums[rowminx-minx],(rowmaxx-rowminx+1)*sizeof(sums[0]),0);

      if Antialiasing then
        {$define PARAM_ANTIALIASINGFACTOR}
        {$i multishapeline.inc}
      else
        {$i multishapeline.inc};

      pdest := dest.ScanLine[yb] + rowminx;
      xb := rowminx;
      nextSum := @sums[xb-minx];
      while xb <= rowmaxx do
      begin
        curSum := nextSum;
        inc(nextSum);
        with curSum^ do
        begin
          if sumA <> 0 then
          begin
            ec.red := (sumR+sumA shr 1) div sumA;
            ec.green := (sumG+sumA shr 1) div sumA;
            ec.blue := (sumB+sumA shr 1) div sumA;
            if sumA > 255 then sumA := 255;
            ec.alpha := sumA shl 8 + sumA;
            count := 1;
            while (xb < rowmaxx) and (nextSum^.sumA = sumA) and (nextSum^.sumB = sumB)
              and (nextSum^.sumG = sumG) and (nextSum^.sumR = sumR) do
            begin
              inc(xb);
              inc(nextSum);
              inc(count);
            end;
            if count = 1 then
              DrawExpandedPixelInlineWithAlphaCheck(pdest,ec) else
               DrawExpandedPixelsInline(pdest, ec, count );
            inc(pdest,count-1);
          end;
        end;
        inc(xb);
        inc(pdest);
      end;
    end;

  end;

  for k := 0 to nbShapes-1 do
  begin
    freemem(shapeRow[k].density);
    for i := 0 to high(shapeRow[k].inter) do
      shapeRow[k].inter[i].Free;
  end;

  dest.InvalidateBitmap;
end;

procedure FillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2,
  rx, ry: single; options: TRoundRectangleOptions; c: TBGRAPixel; EraseMode: boolean);
var
  info: TFillRoundRectangleInfo;
begin
  if (x1 = x2) or (y1 = y2) then exit;
  info := TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry, options);
  FillShapeAntialias(bmp, info, c, EraseMode,nil, False);
  info.Free;
end;

procedure FillRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1,
  y1, x2, y2, rx, ry: single; options: TRoundRectangleOptions;
  scan: IBGRAScanner);
var
  info: TFillRoundRectangleInfo;
begin
  if (x1 = x2) or (y1 = y2) then exit;
  info := TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry, options);
  FillShapeAntialiasWithTexture(bmp, info, scan, False);
  info.Free;
end;

procedure BorderRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2,
  y2, rx, ry, w: single; options: TRoundRectangleOptions; c: TBGRAPixel;
  EraseMode: boolean);
var
  info: TFillBorderRoundRectInfo;
begin
  if (rx = 0) or (ry = 0) or (w=0) then exit;
  info := TFillBorderRoundRectInfo.Create(x1, y1, x2,y2, rx, ry, w, options);
  FillShapeAntialias(bmp, info, c, EraseMode, nil, False);
  info.Free;
end;

procedure BorderRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1,
  y1, x2, y2, rx, ry, w: single; options: TRoundRectangleOptions;
  scan: IBGRAScanner);
var
  info: TFillBorderRoundRectInfo;
begin
  if (rx = 0) or (ry = 0) or (w=0) then exit;
  info := TFillBorderRoundRectInfo.Create(x1, y1, x2,y2, rx, ry, w, options);
  FillShapeAntialiasWithTexture(bmp, info, scan, False);
  info.Free;
end;

procedure BorderAndFillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1,
  x2, y2, rx, ry, w: single; options: TRoundRectangleOptions; bordercolor,
  fillcolor: TBGRAPixel; bordertexture,filltexture: IBGRAScanner; EraseMode: boolean);
var
  info: TFillBorderRoundRectInfo;
  multi: TBGRAMultishapeFiller;
begin
  if (rx = 0) or (ry = 0) then exit;
  info := TFillBorderRoundRectInfo.Create(x1, y1, x2,y2, rx, ry, w, options);
  if not EraseMode then
  begin
    multi := TBGRAMultishapeFiller.Create;
    if filltexture <> nil then
      multi.AddShape(info.innerBorder, filltexture) else
      multi.AddShape(info.innerBorder, fillcolor);
    if w<>0 then
    begin
      if bordertexture <> nil then
        multi.AddShape(info, bordertexture) else
        multi.AddShape(info, bordercolor);
    end;
    multi.Draw(bmp);
    multi.Free;
  end else
  begin
    FillShapeAntialias(bmp, info.innerBorder, fillcolor, EraseMode, nil, False);
    FillShapeAntialias(bmp, info, bordercolor, EraseMode, nil, False);
  end;
  info.Free;
end;

function SplineVertexToSide(y0, y1, y2, y3: single; t: single): single;
var
  a0, a1, a2, a3: single;
  t2: single;
begin
  t2     := t * t;
  a0     := y3 - y2 - y0 + y1;
  a1     := y0 - y1 - a0;
  a2     := y2 - y0;
  a3     := y1;
  Result := a0 * t * t2 + a1 * t2 + a2 * t + a3;
end;

function ComputeCurvePrecision(pt1, pt2, pt3, pt4: TPointF): integer;
var
  len: single;
begin
  len    := sqr(pt1.x - pt2.x) + sqr(pt1.y - pt2.y);
  len    := max(len, sqr(pt3.x - pt2.x) + sqr(pt3.y - pt2.y));
  len    := max(len, sqr(pt3.x - pt4.x) + sqr(pt3.y - pt4.y));
  Result := round(sqrt(sqrt(len)) * 2);
end;

function ComputeBezierCurve(const curve: TCubicBezierCurve): ArrayOfTPointF; overload;
var
  t,f1,f2,f3,f4: single;
  i,nb: Integer;
begin
  nb := ComputeCurvePrecision(curve.p1,curve.c1,curve.c2,curve.p2);
  if nb <= 1 then nb := 2;
  setlength(result,nb);
  result[0] := curve.p1;
  result[nb-1] := curve.p2;
  for i := 1 to nb-2 do
  begin
    t := i/(nb-1);
    f1 := (1-t);
    f2 := f1*f1;
    f1 *= f2;
    f2 *= t*3;
    f4 := t*t;
    f3 := f4*(1-t)*3;
    f4 *= t;

    result[i] := PointF(f1*curve.p1.x + f2*curve.c1.x +
                  f3*curve.c2.x + f4*curve.p2.x,
                  f1*curve.p1.y + f2*curve.c1.y +
                  f3*curve.c2.y + f4*curve.p2.y);
  end;
end;

function ComputeBezierCurve(const curve: TQuadraticBezierCurve): ArrayOfTPointF; overload;
var
  t,f1,f2,f3: single;
  i,nb: Integer;
begin
  nb := ComputeCurvePrecision(curve.p1,curve.c,curve.c,curve.p2);
  if nb <= 1 then nb := 2;
  setlength(result,nb);
  result[0] := curve.p1;
  result[nb-1] := curve.p2;
  for i := 1 to nb-2 do
  begin
    t := i/(nb-1);
    f1 := (1-t);
    f3 := t;
    f2 := f1*f3*2;
    f1 *= f1;
    f3 *= f3;
    result[i] := PointF(f1*curve.p1.x + f2*curve.c.x + f3*curve.p2.x,
                  f1*curve.p1.y + f2*curve.c.y + f3*curve.p2.y);
  end;
end;

function ComputeBezierSpline(const spline: array of TCubicBezierCurve): ArrayOfTPointF;
var
  curves: array of array of TPointF;
  nb: integer;
  lastPt: TPointF;
  i: Integer;
  j: Integer;

  procedure AddPt(pt: TPointF); inline;
  begin
    result[nb]:= pt;
    inc(nb);
    lastPt := pt;
  end;

  function EqLast(pt: TPointF): boolean;
  begin
    result := (pt.x = lastPt.x) and (pt.y = lastPt.y);
  end;

begin
  if length(spline)= 0 then
  begin
    setlength(result,0);
    exit;
  end;
  setlength(curves, length(spline));
  for i := 0 to high(spline) do
    curves[i] := ComputeBezierCurve(spline[i]);
  nb := length(curves[0]);
  lastPt := curves[0][high(curves[0])];
  for i := 1 to high(curves) do
  begin
    inc(nb,length(curves[i]));
    if EqLast(curves[i][0]) then dec(nb);
    lastPt := curves[i][high(curves[i])];
  end;
  setlength(result,nb);
  nb := 0;
  for j := 0 to high(curves[0]) do
    AddPt(curves[0][j]);
  for i := 1 to high(curves) do
  begin
    if not EqLast(curves[i][0]) then AddPt(curves[i][0]);
    for j := 1 to high(curves[i]) do
      AddPt(curves[i][j]);
  end;
end;

function ComputeBezierSpline(const spline: array of TQuadraticBezierCurve
  ): ArrayOfTPointF;
var
  curves: array of array of TPointF;
  nb: integer;
  lastPt: TPointF;
  i: Integer;
  j: Integer;

  procedure AddPt(pt: TPointF); inline;
  begin
    result[nb]:= pt;
    inc(nb);
    lastPt := pt;
  end;

  function EqLast(pt: TPointF): boolean;
  begin
    result := (pt.x = lastPt.x) and (pt.y = lastPt.y);
  end;

begin
  if length(spline)= 0 then
  begin
    setlength(result,0);
    exit;
  end;
  setlength(curves, length(spline));
  for i := 0 to high(spline) do
    curves[i] := ComputeBezierCurve(spline[i]);
  nb := length(curves[0]);
  lastPt := curves[0][high(curves[0])];
  for i := 1 to high(curves) do
  begin
    inc(nb,length(curves[i]));
    if EqLast(curves[i][0]) then dec(nb);
    lastPt := curves[i][high(curves[i])];
  end;
  setlength(result,nb);
  nb := 0;
  for j := 0 to high(curves[0]) do
    AddPt(curves[0][j]);
  for i := 1 to high(curves) do
  begin
    if not EqLast(curves[i][0]) then AddPt(curves[i][0]);
    for j := 1 to high(curves[i]) do
      AddPt(curves[i][j]);
  end;
end;

function ComputeClosedSpline(const points: array of TPointF; Style: TSplineStyle): ArrayOfTPointF;
var
  i, j, nb, idx, pre: integer;
  ptPrev, ptPrev2, ptNext, ptNext2: TPointF;
  t: single;
  kernel: TWideKernelFilter;

begin
  if length(points) <= 2 then
  begin
    setlength(result,length(points));
    for i := 0 to high(result) do
      result[i] := points[i];
    exit;
  end;

  nb := 1;
  for i := 0 to high(points) do
  begin
    ptPrev2 := points[(i + length(points) - 1) mod length(points)];
    ptPrev  := points[i];
    ptNext  := points[(i + 1) mod length(points)];
    ptNext2 := points[(i + 2) mod length(points)];
    nb      += ComputeCurvePrecision(ptPrev2, ptPrev, ptNext, ptNext2);
  end;

  kernel := CreateInterpolator(style);
  setlength(Result, nb);
  for i := 0 to high(points) do
  begin
    ptPrev2 := points[(i + length(points) - 1) mod length(points)];
    ptPrev  := points[i];
    ptNext  := points[(i + 1) mod length(points)];
    ptNext2 := points[(i + 2) mod length(points)];
    pre     := ComputeCurvePrecision(ptPrev2, ptPrev, ptNext, ptNext2);
    if i=0 then
    begin
      j := 0;
      idx := 0;
    end else j := 1;
    while j <= pre do
    begin
      t := j/pre;
      result[idx] := ptPrev2*kernel.Interpolation(t+1) + ptPrev*kernel.Interpolation(t) +
                     ptNext*kernel.Interpolation(t-1)  + ptNext2*kernel.Interpolation(t-2);
      Inc(idx);
      inc(j);
    end;
  end;
  kernel.Free;
end;

function ComputeOpenedSpline(const points: array of TPointF; Style: TSplineStyle; EndCoeff: single): ArrayOfTPointF;
var
  i, j, nb, idx, pre: integer;
  ptPrev, ptPrev2, ptNext, ptNext2: TPointF;
  t: single;
  kernel: TWideKernelFilter;
begin
  if length(points) <= 2 then
  begin
    setlength(result,length(points));
    for i := 0 to high(result) do
      result[i] := points[i];
    exit;
  end;
  if EndCoeff < -0.3 then EndCoeff := -0.3;

  nb := 1;
  for i := 0 to high(points) - 1 do
  begin
    ptPrev  := points[i];
    ptNext  := points[i + 1];
    if i=0 then
      ptPrev2 := (ptPrev+(ptNext+points[i + 2])*EndCoeff)*(1/(1+2*EndCoeff))
    else
      ptPrev2 := points[i - 1];
    if i = high(points)-1 then
      ptNext2 := (ptNext+(ptPrev+points[i - 1])*EndCoeff)*(1/(1+2*EndCoeff))
    else
      ptNext2 := points[i + 2];
    nb      += ComputeCurvePrecision(ptPrev2, ptPrev, ptNext, ptNext2);
  end;

  kernel := CreateInterpolator(style);
  setlength(Result, nb);
  for i := 0 to high(points) - 1 do
  begin
    ptPrev  := points[i];
    ptNext  := points[i + 1];
    if i=0 then
      ptPrev2 := (ptPrev+(ptNext+points[i + 2])*EndCoeff)*(1/(1+2*EndCoeff))
    else
      ptPrev2 := points[i - 1];
    if i = high(points)-1 then
      ptNext2 := (ptNext+(ptPrev+points[i - 1])*EndCoeff)*(1/(1+2*EndCoeff))
    else
      ptNext2 := points[i + 2];
    pre     := ComputeCurvePrecision(ptPrev2, ptPrev, ptNext, ptNext2);
    if i=0 then
    begin
      j := 0;
      idx := 0;
    end else j := 1;
    while j <= pre do
    begin
      t := j/pre;
      result[idx] := ptPrev2*kernel.Interpolation(t+1) + ptPrev*kernel.Interpolation(t) +
                     ptNext*kernel.Interpolation(t-1)  + ptNext2*kernel.Interpolation(t-2);
      Inc(idx);
      inc(j);
    end;
  end;
  kernel.Free;
end;

initialization

  Randomize;

end.
