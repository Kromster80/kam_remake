unit BGRAPolygonAliased;

{$mode objfpc}{$H+}

interface

{ This unit provides fast aliased polygon routines.

  To do aliased drawing, only one line is intersected with polygons for each output scanline.
  Along with intersection coordinates, color and texture coordinates are computed using
  linear interpolation. Inverse values are used for projective transform. }

uses
  Classes, SysUtils, BGRAPolygon, BGRAFillInfo, BGRABitmapTypes;

type
  //segment information for linear color
  TLinearColorInfo = record
    Color, ColorSlopes: TColorF;
  end;
  PLinearColorInfo = ^TLinearColorInfo;
  ArrayOfTColorF = array of TColorF;

  //add a color information to intersection info
  TLinearColorGradientIntersectionInfo = class(TIntersectionInfo)
    Color: TColorF;
  end;

  { TPolygonLinearColorGradientInfo }

  TPolygonLinearColorGradientInfo = class(TFillPolyInfo)
  protected
    FColors: array of TColorF;
  public
    constructor Create(const points: array of TPointF; const Colors: array of TBGRAPixel);
    function CreateSegmentData(numPt,nextPt: integer; x,y: single): pointer; override;
    function CreateIntersectionInfo: TIntersectionInfo; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  end;

procedure PolygonLinearColorGradientAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonLinearColorGradientInfo;
  NonZeroWinding: boolean); overload;
procedure PolygonLinearColorGradientAliased(bmp: TBGRACustomBitmap; const points: array of TPointF;
  const Colors: array of TBGRAPixel; NonZeroWinding: boolean); overload;

type
  //segment information for linear texture
  TLinearTextureInfo = record
    TexCoord: TPointF;
    TexCoordSlopes: TPointF;
    lightness: single;
    lightnessSlope: single;
  end;
  PLinearTextureInfo = ^TLinearTextureInfo;

  //add a texture coordinate to intersection info
  TLinearTextureMappingIntersectionInfo = class(TIntersectionInfo)
    texCoord: TPointF;
    lightness: word;
  end;

  { TPolygonLinearTextureMappingInfo }

  TPolygonLinearTextureMappingInfo = class(TFillPolyInfo)
  protected
    FTexCoords: array of TPointF;
    FLightnesses: array of Word;
  public
    constructor Create(const points: array of TPointF; const texCoords: array of TPointF);
    constructor Create(const points: array of TPointF; const texCoords: array of TPointF; const lightnesses: array of word);
    function CreateSegmentData(numPt,nextPt: integer; x,y: single): pointer; override;
    function CreateIntersectionInfo: TIntersectionInfo; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  end;

procedure PolygonLinearTextureMappingAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonLinearTextureMappingInfo;
  texture: IBGRAScanner; TextureInterpolation: Boolean; NonZeroWinding: boolean); overload;

procedure PolygonLinearTextureMappingAliased(bmp: TBGRACustomBitmap; const points: array of TPointF; texture: IBGRAScanner;
  const texCoords: array of TPointF; TextureInterpolation: Boolean; NonZeroWinding: boolean); overload;
procedure PolygonLinearTextureMappingAliasedWithLightness(bmp: TBGRACustomBitmap; const points: array of TPointF; texture: IBGRAScanner;
  const texCoords: array of TPointF; TextureInterpolation: Boolean; lightnesses: array of word; NonZeroWinding: boolean); overload;


type
  //segment information for perspective texture. Use inverse Z and slopes.
  TPerspectiveTextureInfo = record
    InvZ,InvZSlope: Single;
    TexCoordDivByZ: TPointF;
    TexCoordDivByZSlopes: TPointF;
    lightness: single;
    lightnessSlope: single;
  end;
  PPerspectiveTextureInfo = ^TPerspectiveTextureInfo;

  //add a texture coordinate and depth to intersection info (stored as inverse)
  TPerspectiveTextureMappingIntersectionInfo = class(TIntersectionInfo)
    texCoordDivByZ: TPointF;
    coordInvZ: single;
    lightness: word;
  end;

  { TPolygonPerspectiveTextureMappingInfo }

  TPolygonPerspectiveTextureMappingInfo = class(TFillPolyInfo)
  protected
    FTexCoords: array of TPointF;
    FPointsZ: array of single;
    FLightnesses: array of Word;
  public
    constructor Create(const points: array of TPointF; const pointsZ: array of single; const texCoords: array of TPointF);
    constructor Create(const points: array of TPointF; const pointsZ: array of single; const texCoords: array of TPointF; const lightnesses: array of word);
    function CreateSegmentData(numPt,nextPt: integer; x,y: single): pointer; override;
    function CreateIntersectionInfo: TIntersectionInfo; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  end;

procedure PolygonPerspectiveTextureMappingAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonPerspectiveTextureMappingInfo;
         texture: IBGRAScanner; TextureInterpolation: Boolean; NonZeroWinding: boolean); overload;
procedure PolygonPerspectiveTextureMappingAliased(bmp: TBGRACustomBitmap; const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner;
           const texCoords: array of TPointF; TextureInterpolation: Boolean; NonZeroWinding: boolean); overload;
procedure PolygonPerspectiveTextureMappingAliasedWithLightness(bmp: TBGRACustomBitmap; const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner;
           const texCoords: array of TPointF; TextureInterpolation: Boolean; lightnesses: array of word; NonZeroWinding: boolean); overload;

{ Aliased round rectangle }
procedure BGRARoundRectAliased(dest: TBGRACustomBitmap; X1, Y1, X2, Y2: integer;
  DX, DY: integer; BorderColor, FillColor: TBGRAPixel; FillTexture: IBGRAScanner = nil);

implementation

uses Math, BGRABlend;

{From LazRGBGraphics}
procedure BGRARoundRectAliased(dest: TBGRACustomBitmap; X1, Y1, X2, Y2: integer;
  DX, DY: integer; BorderColor, FillColor: TBGRAPixel; FillTexture: IBGRAScanner = nil);
var
  CX, CY, CX1, CY1, A, B, NX, NY: single;
  X, Y, EX, EY: integer;
  LX1, LY1: integer;
  LX2, LY2: integer;
  DivSqrA, DivSqrB: single;
  I, J, S: integer;
  EdgeList: array of TPoint;
  temp:   integer;
  LX, LY: integer;
  RowStart,RowEnd: integer;
  eBorderColor,eFillColor: TExpandedPixel;

  procedure AddEdge(X, Y: integer);
  begin
    if (EdgeList[Y].X = -1) or (X < EdgeList[Y].X) then
      EdgeList[Y].X := X;
    if (EdgeList[Y].Y = -1) or (X > EdgeList[Y].Y) then
      EdgeList[Y].Y := X;
  end;

begin
  if (x1 > x2) then
  begin
    temp := x1;
    x1   := x2;
    x2   := temp;
  end;
  if (y1 > y2) then
  begin
    temp := y1;
    y1   := y2;
    y2   := temp;
  end;
  if (x2 - x1 <= 0) or (y2 - y1 <= 0) then
    exit;
  LX := x2 - x1 - DX;
  LY := y2 - y1 - DY;
  if LX < 0 then LX := 0;
  if LY < 0 then LY := 0;
  Dec(x2);
  Dec(y2);

  eBorderColor := GammaExpansion(BorderColor);
  eFillColor := GammaExpansion(FillColor);

  if (X1 = X2) and (Y1 = Y2) then
  begin
    dest.DrawPixel(X1, Y1, eBorderColor);
    Exit;
  end;

  if (X2 - X1 = 1) or (Y2 - Y1 = 1) then
  begin
    dest.FillRect(X1, Y1, X2 + 1, Y2 + 1, BorderColor, dmDrawWithTransparency);
    Exit;
  end;

  if (LX > X2 - X1) or (LY > Y2 - Y1) then
  begin
    dest.Rectangle(X1, Y1, X2 + 1, Y2 + 1, BorderColor, dmDrawWithTransparency);
    if FillTexture <> nil then
      dest.FillRect(X1 + 1, Y1 + 1, X2, Y2, FillTexture, dmDrawWithTransparency) else
      dest.FillRect(X1 + 1, Y1 + 1, X2, Y2, FillColor, dmDrawWithTransparency);
    Exit;
  end;

  SetLength(EdgeList, Ceil((Y2 - Y1 + 1) / 2));
  for I := 0 to Pred(High(EdgeList)) do
    EdgeList[I] := Point(-1, -1);
  EdgeList[High(EdgeList)] := Point(0, 0);

  A  := (X2 - X1 + 1 - LX) / 2;
  B  := (Y2 - Y1 + 1 - LY) / 2;
  CX := (X2 + X1 + 1) / 2;
  CY := (Y2 + Y1 + 1) / 2;

  CX1 := X2 + 1 - A - Floor(CX);
  CY1 := Y2 + 1 - B - Floor(CY);

  EX := Floor(Sqr(A) / Sqrt(Sqr(A) + Sqr(B)) + Frac(A));
  EY := Floor(Sqr(B) / Sqrt(Sqr(A) + Sqr(B)) + Frac(B));

  DivSqrA := 1 / Sqr(A);
  DivSqrB := 1 / Sqr(B);

  NY := B;
  AddEdge(Floor(CX1), Round(CY1 + B) - 1);
  for X := 1 to Pred(EX) do
  begin
    NY := B * Sqrt(1 - Sqr(X + 0.5 - Frac(A)) * DivSqrA);

    AddEdge(Floor(CX1) + X, Round(CY1 + NY) - 1);
  end;

  LX1 := Floor(CX1) + Pred(EX);
  LY1 := Round(CY1 + NY) - 1;

  NX := A;
  AddEdge(Round(CX1 + A) - 1, Floor(CY1));
  for Y := 1 to Pred(EY) do
  begin
    NX := A * Sqrt(1 - Sqr(Y + 0.5 - Frac(B)) * DivSqrB);

    AddEdge(Round(CX1 + NX) - 1, Floor(CY1) + Y);
  end;

  LX2 := Round(CX1 + NX) - 1;
  LY2 := Floor(CY1) + Pred(EY);

  if Abs(LX1 - LX2) > 1 then
  begin
    if Abs(LY1 - LY2) > 1 then
      AddEdge(LX1 + 1, LY1 - 1)
    else
      AddEdge(LX1 + 1, LY1);
  end
  else
  if Abs(LY1 - LY2) > 1 then
    AddEdge(LX2, LY1 - 1);

  for I := 0 to High(EdgeList) do
  begin
    if EdgeList[I].X = -1 then
      EdgeList[I] := Point(Round(CX1 + A) - 1, Round(CX1 + A) - 1)
    else
      Break;
  end;

  for J := 0 to High(EdgeList) do
  begin
    if (J = 0) and (Frac(CY) > 0) then
    begin
      for I := EdgeList[J].X to EdgeList[J].Y do
      begin
        dest.DrawPixel(Floor(CX) + I, Floor(CY) + J, eBorderColor);
        dest.DrawPixel(Ceil(CX) - Succ(I), Floor(CY) + J, eBorderColor);
      end;

      if FillTexture <> nil then
        dest.DrawHorizLine(Ceil(CX) - EdgeList[J].X, Floor(CY) + J, Floor(CX) +
          Pred(EdgeList[J].X), FillTexture) else
        dest.DrawHorizLine(Ceil(CX) - EdgeList[J].X, Floor(CY) + J, Floor(CX) +
          Pred(EdgeList[J].X), eFillColor);
    end
    else
    if (J = High(EdgeList)) then
    begin
      if Frac(CX) > 0 then
        S := -EdgeList[J].Y
      else
        S := -Succ(EdgeList[J].Y);

      for I := S to EdgeList[J].Y do
      begin
        dest.DrawPixel(Floor(CX) + I, Floor(CY) + J, eBorderColor);
        dest.DrawPixel(Floor(CX) + I, Ceil(CY) - Succ(J), eBorderColor);
      end;
    end
    else
    begin
      for I := EdgeList[J].X to EdgeList[J].Y do
      begin
        dest.DrawPixel(Floor(CX) + I, Floor(CY) + J, eBorderColor);
        dest.DrawPixel(Floor(CX) + I, Ceil(CY) - Succ(J), eBorderColor);
        if Floor(CX) + I <> Ceil(CX) - Succ(I) then
        begin
          dest.DrawPixel(Ceil(CX) - Succ(I), Floor(CY) + J, eBorderColor);
          dest.DrawPixel(Ceil(CX) - Succ(I), Ceil(CY) - Succ(J), eBorderColor);
        end;
      end;

      RowStart := Ceil(CX) - EdgeList[J].X;
      RowEnd := Floor(CX) + Pred(EdgeList[J].X);
      if RowEnd >= RowStart then
      begin
        if FillTexture <> nil then
        begin
          dest.DrawHorizLine(RowStart, Floor(CY) + J,
            RowEnd, FillTexture);
          dest.DrawHorizLine(RowStart, Ceil(CY) - Succ(J),
            RowEnd, FillTexture);
        end else
        begin
          dest.DrawHorizLine(RowStart, Floor(CY) + J,
            RowEnd, eFillColor);
          dest.DrawHorizLine(RowStart, Ceil(CY) - Succ(J),
            RowEnd, eFillColor);
        end;
      end;
    end;
  end;
end;

{ TPolygonLinearColorGradientInfo }

constructor TPolygonLinearColorGradientInfo.Create(
  const points: array of TPointF; const Colors: array of TBGRAPixel);
var
  i: Integer;
  lPoints: array of TPointF;
  nbP: integer;
  ec: TExpandedPixel;
begin
  if length(Colors) <> length(points) then
    raise Exception.Create('Dimensions mismatch');

  setlength(lPoints, length(points));
  SetLength(FColors, length(points));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i]<>points[i-1]) then
  begin
    lPoints[nbP] := points[i];
    ec := GammaExpansion(Colors[i]);
    FColors[nbP] := ColorF(ec.red,ec.green,ec.blue,ec.alpha);
    inc(nbP);
  end;
  if (nbP>0) and (lPoints[nbP-1] = lPoints[0]) then dec(NbP);
  setlength(lPoints, nbP);
  SetLength(FColors, nbP);

  inherited Create(lPoints);
end;

{$hints off}
function TPolygonLinearColorGradientInfo.CreateSegmentData(numPt, nextPt: integer; x,
  y: single): pointer;
var
  info: PLinearColorInfo;
  ty,dy: single;
begin
  New(info);
  ty := FPoints[nextPt].y-FPoints[numPt].y;
  info^.ColorSlopes := (FColors[nextPt] - FColors[numPt])*(1/ty);
  dy := y-FPoints[numPt].y;
  info^.Color := FColors[numPt] + info^.ColorSlopes*dy;
  Result:= info;
end;
{$hints on}

function TPolygonLinearColorGradientInfo.CreateIntersectionInfo: TIntersectionInfo;
begin
  Result:= TLinearColorGradientIntersectionInfo.Create;
end;

procedure TPolygonLinearColorGradientInfo.ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var
  j: integer;
  dy: single;
  info: PLinearColorInfo;
begin
  if length(FSlices)=0 then exit;

  while (cury < FSlices[FCurSlice].y1) and (FCurSlice > 0) do dec(FCurSlice);
  while (cury > FSlices[FCurSlice].y2) and (FCurSlice < high(FSlices)) do inc(FCurSlice);
  with FSlices[FCurSlice] do
  if (cury >= y1) and (cury <= y2) then
  begin
    for j := 0 to nbSegments-1 do
    begin
      dy := cury - segments[j].y1;
      inter[nbinter].interX := dy * segments[j].slope + segments[j].x1;
      inter[nbinter].winding := segments[j].winding;
      info := PLinearColorInfo(segments[j].data);
      TLinearColorGradientIntersectionInfo(inter[nbinter]).color := info^.Color + info^.ColorSlopes*dy;
      Inc(nbinter);
    end;
  end;
end;

procedure PolygonLinearColorGradientAliased(bmp: TBGRACustomBitmap;
  polyInfo: TPolygonLinearColorGradientInfo; NonZeroWinding: boolean);
var
  inter:    array of TIntersectionInfo;
  nbInter:  integer;

  procedure DrawGradientLine(yb: integer; ix1: integer; ix2: integer;
    x1: Single; c1: TColorF; x2: Single; c2: TColorF);
  var
    colorPos: TColorF;
    colorStep: TColorF;
    t: single;
    pdest: PBGRAPixel;
    i: LongInt;
    ec: TExpandedPixel;
  begin
    t := ((ix1+0.5)-x1)/(x2-x1);
    colorPos := c1 + (c2-c1)*t;
    colorStep := (c2-c1)*(1/(x2-x1));
    pdest := bmp.ScanLine[yb]+ix1;
    for i := ix1 to ix2 do
    begin
      ec.red := round(colorPos[1]);
      ec.green := round(colorPos[2]);
      ec.blue := round(colorPos[3]);
      ec.alpha := round(colorPos[4]);
      DrawPixelInlineWithAlphaCheck(pdest, GammaCompression(ec));
      colorPos += colorStep;
      inc(pdest);
    end;
  end;

var
  miny, maxy, minx, maxx: integer;

  yb, i: integer;
  x1, x2: single;

  ix1, ix2: integer;

begin
  If not polyInfo.ComputeMinMax(minx,miny,maxx,maxy,bmp) then exit;
  inter := polyInfo.CreateIntersectionArray;

  //vertical scan
  for yb := miny to maxy do
  begin
    //find intersections
    polyInfo.ComputeAndSort(yb+0.5001,inter,nbInter,NonZeroWinding);

    for i := 0 to nbinter div 2 - 1 do
    begin
      x1 := inter[i + i].interX;
      x2 := inter[i + i+ 1].interX;

      if x1 <> x2 then
      begin
        ComputeAliasedRowBounds(x1,x2, minx,maxx, ix1,ix2);
        if ix1 <= ix2 then
          DrawGradientLine(yb,ix1,ix2,
            x1,TLinearColorGradientIntersectionInfo(inter[i+i]).Color,
            x2,TLinearColorGradientIntersectionInfo(inter[i+i+1]).Color);
      end;
    end;
  end;

  for i := 0 to high(inter) do
    inter[i].free;

  bmp.InvalidateBitmap;
end;

procedure PolygonLinearColorGradientAliased(bmp: TBGRACustomBitmap;
  const points: array of TPointF; const Colors: array of TBGRAPixel;
  NonZeroWinding: boolean);
var polyInfo: TPolygonLinearColorGradientInfo;
begin
  polyInfo := TPolygonLinearColorGradientInfo.Create(points,Colors);
  PolygonLinearColorGradientAliased(bmp,polyInfo,NonZeroWinding);
  polyInfo.Free;
end;

{ TPolygonLinearTextureMappingInfo }

constructor TPolygonLinearTextureMappingInfo.Create(const points: array of TPointF;
  const texCoords: array of TPointF);
var
  i: Integer;
  lPoints: array of TPointF;
  nbP: integer;
begin
  if length(texCoords) <> length(points) then
    raise Exception.Create('Dimensions mismatch');

  setlength(lPoints, length(points));
  SetLength(FTexCoords, length(points));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i]<>points[i-1]) then
  begin
    lPoints[nbP] := points[i];
    FTexCoords[nbP] := texCoords[i];
    inc(nbP);
  end;
  if (nbP>0) and (lPoints[nbP-1] = lPoints[0]) then dec(NbP);
  setlength(lPoints, nbP);
  SetLength(FTexCoords, nbP);

  inherited Create(lPoints);
end;

constructor TPolygonLinearTextureMappingInfo.Create(
  const points: array of TPointF; const texCoords: array of TPointF;
  const lightnesses: array of word);
var
  i: Integer;
  lPoints: array of TPointF;
  nbP: integer;
begin
  if (length(texCoords) <> length(points)) or (length(lightnesses) <> length(points)) then
    raise Exception.Create('Dimensions mismatch');

  setlength(lPoints, length(points));
  SetLength(FTexCoords, length(points));
  setlength(FLightnesses, length(lightnesses));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i]<>points[i-1]) then
  begin
    lPoints[nbP] := points[i];
    FTexCoords[nbP] := texCoords[i];
    FLightnesses[nbP] := lightnesses[i];
    inc(nbP);
  end;
  if (nbP>0) and (lPoints[nbP-1] = lPoints[0]) then dec(NbP);
  setlength(lPoints, nbP);
  SetLength(FTexCoords, nbP);
  SetLength(FLightnesses, nbP);

  inherited Create(lPoints);
end;

{$hints off}
function TPolygonLinearTextureMappingInfo.CreateSegmentData(numPt, nextPt: integer; x,
  y: single): pointer;
var
  info: PLinearTextureInfo;
  ty,dy: single;
begin
  New(info);
  ty := FPoints[nextPt].y-FPoints[numPt].y;
  dy := y-FPoints[numPt].y;
  info^.TexCoordSlopes := (FTexCoords[nextPt] - FTexCoords[numPt])*(1/ty);
  info^.TexCoord := FTexCoords[numPt] + info^.TexCoordSlopes*dy;
  if FLightnesses <> nil then
  begin
    info^.lightnessSlope := (FLightnesses[nextPt] - FLightnesses[numPt])*(1/ty);
    info^.lightness := FLightnesses[numPt] + info^.lightnessSlope*dy;
  end else
  begin
    info^.lightness := 32768;
    info^.lightnessSlope := 0;
  end;
  Result:= info;
end;
{$hints on}

function TPolygonLinearTextureMappingInfo.CreateIntersectionInfo: TIntersectionInfo;
begin
  result := TLinearTextureMappingIntersectionInfo.Create;
end;

procedure TPolygonLinearTextureMappingInfo.ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var
  j: integer;
  dy: single;
  info: PLinearTextureInfo;
begin
  if length(FSlices)=0 then exit;

  while (cury < FSlices[FCurSlice].y1) and (FCurSlice > 0) do dec(FCurSlice);
  while (cury > FSlices[FCurSlice].y2) and (FCurSlice < high(FSlices)) do inc(FCurSlice);
  with FSlices[FCurSlice] do
  if (cury >= y1) and (cury <= y2) then
  begin
    for j := 0 to nbSegments-1 do
    begin
      dy := cury - segments[j].y1;
      inter[nbinter].interX := dy * segments[j].slope + segments[j].x1;
      inter[nbinter].winding := segments[j].winding;
      info := PLinearTextureInfo(segments[j].data);
      TLinearTextureMappingIntersectionInfo(inter[nbinter]).texCoord := info^.TexCoord + info^.TexCoordSlopes*dy;
      if FLightnesses<>nil then
        TLinearTextureMappingIntersectionInfo(inter[nbinter]).lightness := round(info^.lightness + info^.lightnessSlope*dy)
      else
        TLinearTextureMappingIntersectionInfo(inter[nbinter]).lightness := 32768;
      Inc(nbinter);
    end;
  end;
end;

{$hints off}
procedure PolygonLinearTextureMappingAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonLinearTextureMappingInfo;
  texture: IBGRAScanner; TextureInterpolation: Boolean; NonZeroWinding: boolean);
var
  inter:    array of TIntersectionInfo;
  nbInter:  integer;
  scanAtFunc: function(X,Y: Single): TBGRAPixel of object;

  procedure DrawTextureLineWithoutLight(yb: integer; ix1: integer; ix2: integer;
    info1,info2: TLinearTextureMappingIntersectionInfo;
    WithInterpolation: boolean);
    {$i lineartexscan.inc}

  procedure DrawTextureLineWithLight(yb: integer; ix1: integer; ix2: integer;
    info1,info2: TLinearTextureMappingIntersectionInfo;
    WithInterpolation: boolean);
    {$define PARAM_USELIGHTING}
    {$i lineartexscan.inc}

var
  miny, maxy, minx, maxx: integer;

  yb, i: integer;
  x1, x2: single;

  ix1, ix2: integer;

begin
  If not polyInfo.ComputeMinMax(minx,miny,maxx,maxy,bmp) then exit;

  scanAtFunc := @texture.ScanAt;

  inter := polyInfo.CreateIntersectionArray;

  //vertical scan
  for yb := miny to maxy do
  begin
    //find intersections
    polyInfo.ComputeAndSort(yb+0.5001,inter,nbInter,NonZeroWinding);
    for i := 0 to nbinter div 2 - 1 do
    begin
      x1 := inter[i + i].interX;
      x2 := inter[i + i+ 1].interX;

      if x1 <> x2 then
      begin
        ComputeAliasedRowBounds(x1,x2, minx,maxx, ix1,ix2);
        if ix1 <= ix2 then
        begin
          if (TLinearTextureMappingIntersectionInfo(inter[i+i]).lightness = 32768) and
             (TLinearTextureMappingIntersectionInfo(inter[i+i+1]).lightness = 32768) then
            DrawTextureLineWithoutLight(yb,ix1,ix2,
               TLinearTextureMappingIntersectionInfo(inter[i+i]),
               TLinearTextureMappingIntersectionInfo(inter[i+i+1]),
               TextureInterpolation)
          else
            DrawTextureLineWithLight(yb,ix1,ix2,
               TLinearTextureMappingIntersectionInfo(inter[i+i]),
               TLinearTextureMappingIntersectionInfo(inter[i+i+1]),
               TextureInterpolation);
        end;
      end;
    end;
  end;

  for i := 0 to high(inter) do
    inter[i].free;

  bmp.InvalidateBitmap;
end;
{$hints on}

procedure PolygonLinearTextureMappingAliased(bmp: TBGRACustomBitmap;
  const points: array of TPointF; texture: IBGRAScanner;
  const texCoords: array of TPointF; TextureInterpolation: Boolean; NonZeroWinding: boolean);
var polyInfo: TPolygonLinearTextureMappingInfo;
begin
  polyInfo := TPolygonLinearTextureMappingInfo.Create(points,texCoords);
  PolygonLinearTextureMappingAliased(bmp,polyInfo,texture,TextureInterpolation,NonZeroWinding);
  polyInfo.Free;
end;

procedure PolygonLinearTextureMappingAliasedWithLightness(
  bmp: TBGRACustomBitmap; const points: array of TPointF;
  texture: IBGRAScanner; const texCoords: array of TPointF;
  TextureInterpolation: Boolean; lightnesses: array of word;
  NonZeroWinding: boolean);
var polyInfo: TPolygonLinearTextureMappingInfo;
begin
  polyInfo := TPolygonLinearTextureMappingInfo.Create(points,texCoords,lightnesses);
  PolygonLinearTextureMappingAliased(bmp,polyInfo,texture,TextureInterpolation,NonZeroWinding);
  polyInfo.Free;
end;

{ TPolygonPerspectiveTextureMappingInfo }

constructor TPolygonPerspectiveTextureMappingInfo.Create(
  const points: array of TPointF; const pointsZ: array of single;
  const texCoords: array of TPointF);
var
  i: Integer;
  lPoints: array of TPointF;
  nbP: integer;
begin
  if (length(texCoords) <> length(points)) or (length(pointsZ) <> length(points)) then
    raise Exception.Create('Dimensions mismatch');

  setlength(lPoints, length(points));
  SetLength(FTexCoords, length(points));
  SetLength(FPointsZ, length(points));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i].x<>points[i-1].X) or (points[i].y<>points[i-1].y) then
  begin
    lPoints[nbP] := points[i];
    FTexCoords[nbP] := texCoords[i];
    FPointsZ[nbP] := abs(pointsZ[i]);
    inc(nbP);
  end;
  if (nbP>0) and (lPoints[nbP-1].X = lPoints[0].X) and (lPoints[nbP-1].Y = lPoints[0].Y) then dec(NbP);
  setlength(lPoints, nbP);
  SetLength(FTexCoords, nbP);
  SetLength(FPointsZ, nbP);

  inherited Create(lPoints);
end;

constructor TPolygonPerspectiveTextureMappingInfo.Create(
  const points: array of TPointF; const pointsZ: array of single;
  const texCoords: array of TPointF; const lightnesses: array of word);
var
  i: Integer;
  lPoints: array of TPointF;
  nbP: integer;
begin
  if (length(texCoords) <> length(points)) or (length(pointsZ) <> length(points)) or
     (length(lightnesses) <> length(points)) then
    raise Exception.Create('Dimensions mismatch');

  setlength(lPoints, length(points));
  SetLength(FTexCoords, length(points));
  SetLength(FPointsZ, length(points));
  setLength(FLightnesses, length(points));
  nbP := 0;
  for i := 0 to high(points) do
  if (i=0) or (points[i].x<>points[i-1].X) or (points[i].y<>points[i-1].y) then
  begin
    lPoints[nbP] := points[i];
    FTexCoords[nbP] := texCoords[i];
    FPointsZ[nbP] := abs(pointsZ[i]);
    FLightnesses[nbP] := lightnesses[i];
    inc(nbP);
  end;
  if (nbP>0) and (lPoints[nbP-1].X = lPoints[0].X) and (lPoints[nbP-1].Y = lPoints[0].Y) then dec(NbP);
  setlength(lPoints, nbP);
  SetLength(FTexCoords, nbP);
  SetLength(FPointsZ, nbP);
  SetLength(FLightnesses, nbP);

  inherited Create(lPoints);
end;

{$hints off}
function TPolygonPerspectiveTextureMappingInfo.CreateSegmentData(numPt,
  nextPt: integer; x, y: single): pointer;
var
  info: PPerspectiveTextureInfo;
  ty,dy: single;
  CurInvZ,NextInvZ: single;
  CurTexCoordDivByZ: TPointF;
  NextTexCoordDivByZ: TPointF;
begin
  New(info);
  CurInvZ := 1/FPointsZ[numPt];
  CurTexCoordDivByZ := FTexCoords[numPt]*CurInvZ;
  NextInvZ := 1/FPointsZ[nextPt];
  NextTexCoordDivByZ := FTexCoords[nextPt]*NextInvZ;
  ty := FPoints[nextPt].y-FPoints[numPt].y;
  info^.TexCoordDivByZSlopes := (NextTexCoordDivByZ - CurTexCoordDivByZ)*(1/ty);
  dy := y-FPoints[numPt].y;
  info^.TexCoordDivByZ := CurTexCoordDivByZ + info^.TexCoordDivByZSlopes*dy;
  info^.InvZSlope := (NextInvZ-CurInvZ)/ty;
  info^.InvZ := CurInvZ+dy*info^.InvZSlope;
  if FLightnesses <> nil then
  begin
    info^.lightnessSlope := (FLightnesses[nextPt] - FLightnesses[numPt])*(1/ty);
    info^.lightness := FLightnesses[numPt] + info^.lightnessSlope*dy;
  end else
  begin
    info^.lightness := 32768;
    info^.lightnessSlope := 0;
  end;
  Result:= info;
end;
{$hints on}

function TPolygonPerspectiveTextureMappingInfo.CreateIntersectionInfo: TIntersectionInfo;
begin
  Result:= TPerspectiveTextureMappingIntersectionInfo.Create;
end;

procedure TPolygonPerspectiveTextureMappingInfo.ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var
  j: integer;
  dy: single;
  info: PPerspectiveTextureInfo;
begin
  if length(FSlices)=0 then exit;

  while (cury < FSlices[FCurSlice].y1) and (FCurSlice > 0) do dec(FCurSlice);
  while (cury > FSlices[FCurSlice].y2) and (FCurSlice < high(FSlices)) do inc(FCurSlice);
  with FSlices[FCurSlice] do
  if (cury >= y1) and (cury <= y2) then
  begin
    for j := 0 to nbSegments-1 do
    begin
      dy := cury - segments[j].y1;
      inter[nbinter].interX := dy * segments[j].slope + segments[j].x1;
      inter[nbinter].winding := segments[j].winding;
      info := PPerspectiveTextureInfo(segments[j].data);
      TPerspectiveTextureMappingIntersectionInfo(inter[nbinter]).coordInvZ := dy*info^.InvZSlope + info^.InvZ;
      TPerspectiveTextureMappingIntersectionInfo(inter[nbinter]).texCoordDivByZ := info^.TexCoordDivByZ + info^.TexCoordDivByZSlopes*dy;
      if FLightnesses<>nil then
        TPerspectiveTextureMappingIntersectionInfo(inter[nbinter]).lightness := round(info^.lightness + info^.lightnessSlope*dy)
      else
        TPerspectiveTextureMappingIntersectionInfo(inter[nbinter]).lightness := 32768;
      Inc(nbinter);
    end;
  end;
end;

{$hints off}
procedure PolygonPerspectiveTextureMappingAliased(bmp: TBGRACustomBitmap;
  polyInfo: TPolygonPerspectiveTextureMappingInfo; texture: IBGRAScanner;
  TextureInterpolation: Boolean; NonZeroWinding: boolean);
var
  inter:    array of TIntersectionInfo;
  nbInter:  integer;

  scanAtFunc: function(X,Y: Single): TBGRAPixel of object;

  procedure DrawTextureLineWithoutLight(yb: integer; ix1: integer; ix2: integer;
      info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
    {$i perspectivescan.inc}

  procedure DrawTextureLineWithLight(yb: integer; ix1: integer; ix2: integer;
      info1, info2 : TPerspectiveTextureMappingIntersectionInfo; WithInterpolation: boolean);
    {$define PARAM_USELIGHTING}
    {$i perspectivescan.inc}

var
  miny, maxy, minx, maxx: integer;

  yb, i : integer;
  x1, x2: single;

  ix1, ix2: integer;

begin
  If not polyInfo.ComputeMinMax(minx,miny,maxx,maxy,bmp) then exit;

  inter := polyInfo.CreateIntersectionArray;
  scanAtFunc := @texture.ScanAt;

  //vertical scan
  for yb := miny to maxy do
  begin
    //find intersections
    polyInfo.ComputeAndSort(yb+0.5001,inter,nbInter,NonZeroWinding);

    for i := 0 to nbinter div 2 - 1 do
    begin
      x1 := inter[i + i].interX;
      x2 := inter[i + i+ 1].interX;

      if x1 <> x2 then
      begin
        ComputeAliasedRowBounds(x1,x2, minx,maxx, ix1,ix2);
        if ix1 <= ix2 then
        begin
          if (TPerspectiveTextureMappingIntersectionInfo(inter[i+i]).lightness = 32768) and
             (TPerspectiveTextureMappingIntersectionInfo(inter[i+i+1]).lightness = 32768) then
            DrawTextureLineWithoutLight(yb,ix1,ix2,
              TPerspectiveTextureMappingIntersectionInfo(inter[i+i]),
              TPerspectiveTextureMappingIntersectionInfo(inter[i+i+1]),
              TextureInterpolation)
          else
            DrawTextureLineWithLight(yb,ix1,ix2,
              TPerspectiveTextureMappingIntersectionInfo(inter[i+i]),
              TPerspectiveTextureMappingIntersectionInfo(inter[i+i+1]),
              TextureInterpolation);
        end;
      end;
    end;
  end;

  for i := 0 to high(inter) do
    inter[i].free;

  bmp.InvalidateBitmap;
end;
{$hints on}

procedure PolygonPerspectiveTextureMappingAliased(bmp: TBGRACustomBitmap;
  const points: array of TPointF; const pointsZ: array of single;
  texture: IBGRAScanner; const texCoords: array of TPointF;
  TextureInterpolation: Boolean; NonZeroWinding: boolean);
var polyInfo: TPolygonPerspectiveTextureMappingInfo;
begin
  polyInfo := TPolygonPerspectiveTextureMappingInfo.Create(points,pointsZ,texCoords);
  PolygonPerspectiveTextureMappingAliased(bmp,polyInfo,texture,TextureInterpolation, NonZeroWinding);
  polyInfo.Free;
end;

procedure PolygonPerspectiveTextureMappingAliasedWithLightness(
  bmp: TBGRACustomBitmap; const points: array of TPointF;
  const pointsZ: array of single; texture: IBGRAScanner;
  const texCoords: array of TPointF; TextureInterpolation: Boolean;
  lightnesses: array of word; NonZeroWinding: boolean);
var polyInfo: TPolygonPerspectiveTextureMappingInfo;
begin
  polyInfo := TPolygonPerspectiveTextureMappingInfo.Create(points,pointsZ,texCoords,lightnesses);
  PolygonPerspectiveTextureMappingAliased(bmp,polyInfo,texture,TextureInterpolation, NonZeroWinding);
  polyInfo.Free;
end;

end.

