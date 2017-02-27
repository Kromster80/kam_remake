unit BGRATransform;

{$mode objfpc}

interface

{ This unit contains bitmap transformations as classes and the TAffineMatrix record and functions. }

uses
  Classes, SysUtils, BGRABitmapTypes;

type
  { Contains an affine matrix, i.e. a matrix to transform linearly and translate TPointF coordinates }
  TAffineMatrix = array[1..2,1..3] of single;

  { TBGRAAffineScannerTransform allow to transform any scanner. To use it,
    create this object with a scanner as parameter, call transformation
    procedures, and finally, use the newly created object as a scanner.

    You can transform a gradient or a bitmap. See TBGRAAffineBitmapTransform
    for bitmap specific transformation. }

  { TBGRAAffineScannerTransform }

  TBGRAAffineScannerTransform = class(TBGRACustomScanner)
  protected
    FScanner: IBGRAScanner;
    FScanAtFunc: TScanAtFunction;
    FCurX,FCurY: Single;
    FEmptyMatrix: Boolean;
    FMatrix: TAffineMatrix;
    procedure SetMatrix(AMatrix: TAffineMatrix);
    function InternalScanCurrentPixel: TBGRAPixel; virtual;
  public
    constructor Create(AScanner: IBGRAScanner);
    procedure Reset;
    procedure Invert;
    procedure Translate(OfsX,OfsY: Single);
    procedure RotateDeg(Angle: Single);
    procedure RotateRad(Angle: Single);
    procedure MultiplyBy(AMatrix: TAffineMatrix);
    procedure Fit(Origin,HAxis,VAxis: TPointF); virtual;
    procedure Scale(sx,sy: single); overload;
    procedure Scale(factor: single); overload;
    procedure ScanMoveTo(X, Y: Integer); override;
    procedure ScanMoveToF(X, Y: single); inline;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    property Matrix: TAffineMatrix read FMatrix write SetMatrix;
  end;

  { If you don't want the bitmap to repeats itself, or want to specify the
    resample filter, or want to fit easily the bitmap on axes,
    use TBGRAAffineBitmapTransform instead of TBGRAAffineScannerTransform }

  { TBGRAAffineBitmapTransform }

  TBGRAAffineBitmapTransform = class(TBGRAAffineScannerTransform)
  protected
    FBitmap: TBGRACustomBitmap;
    FRepeatImage: boolean;
    FResampleFilter : TResampleFilter;
  public
    constructor Create(ABitmap: TBGRACustomBitmap; ARepeatImage: Boolean= false; AResampleFilter: TResampleFilter = rfLinear);
    function InternalScanCurrentPixel: TBGRAPixel; override;
    procedure Fit(Origin, HAxis, VAxis: TPointF); override;
  end;

{---------------------- Affine matrix functions -------------------}
//fill a matrix
function AffineMatrix(m11,m12,m13,m21,m22,m23: single): TAffineMatrix;

//matrix multiplication
operator *(M,N: TAffineMatrix): TAffineMatrix;

//matrix multiplication by a vector (apply transformation to that vector)
operator *(M: TAffineMatrix; V: TPointF): TPointF;

//check if matrix is inversible
function IsAffineMatrixInversible(M: TAffineMatrix): boolean;

//compute inverse (check if inversible before)
function AffineMatrixInverse(M: TAffineMatrix): TAffineMatrix;

//define a translation matrix
function AffineMatrixTranslation(OfsX,OfsY: Single): TAffineMatrix;

//define a scaling matrix
function AffineMatrixScale(sx,sy: single): TAffineMatrix;

//define a rotation matrix (positive radians are counter clock wise)
function AffineMatrixRotationRad(Angle: Single): TAffineMatrix;

//Positive degrees are clock wise
function AffineMatrixRotationDeg(Angle: Single): TAffineMatrix;

//define the identity matrix (that do nothing)
function AffineMatrixIdentity: TAffineMatrix;

type
  { TBGRATriangleLinearMapping is a scanner that provides
    an optimized transformation for linear texture mapping
    on triangles }

  { TBGRATriangleLinearMapping }

  TBGRATriangleLinearMapping = class(TBGRACustomScanner)
  protected
    FScanner: IBGRAScanner;
    FMatrix: TAffineMatrix;
    FTexCoord1,FDiff2,FDiff3,FStep: TPointF;
    FCurTexCoord: TPointF;
    FScanAtFunc: TScanAtFunction;
  public
    constructor Create(AScanner: IBGRAScanner; pt1,pt2,pt3: TPointF; tex1,tex2,tex3: TPointF);
    procedure ScanMoveTo(X,Y: Integer); override;
    procedure ScanMoveToF(X,Y: Single);
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
  end;

type
  TPerspectiveTransform = class;

  { TBGRAPerspectiveScannerTransform }

  TBGRAPerspectiveScannerTransform = class(TBGRACustomScanner)
  private
    FTexture: IBGRAScanner;
    FMatrix: TPerspectiveTransform;
    FScanAtProc: TScanAtFunction;
  public
    constructor Create(texture: IBGRAScanner; texCoord1,texCoord2: TPointF; const quad: array of TPointF);
    constructor Create(texture: IBGRAScanner; const texCoordsQuad: array of TPointF; const quad: array of TPointF);
    destructor Destroy; override;
    procedure ScanMoveTo(X, Y: Integer); override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
  end;

  { TPerspectiveTransform }

  TPerspectiveTransform = class
  private
    sx ,shy ,w0 ,shx ,sy ,w1 ,tx ,ty ,w2 : single;
    scanDenom,scanNumX,scanNumY: single;
  public
    constructor Create; overload;
    constructor Create(x1,y1,x2,y2: single; const quad: array of TPointF);
    constructor Create(const quad: array of TPointF; x1,y1,x2,y2: single);
    constructor Create(const srcQuad,destQuad: array of TPointF);
    function MapQuadToQuad(const srcQuad,destQuad: array of TPointF): boolean;
    function MapRectToQuad(x1,y1,x2,y2: single; const quad: array of TPointF): boolean;
    function MapQuadToRect(const quad: array of TPointF; x1,y1,x2,y2: single): boolean;
    function MapSquareToQuad(const quad: array of TPointF): boolean;
    function MapQuadToSquare(const quad: array of TPointF): boolean;
    procedure AssignIdentity;
    function Invert: boolean;
    procedure Translate(dx,dy: single);
    procedure MultiplyBy(a: TPerspectiveTransform);
    procedure PremultiplyBy(b: TPerspectiveTransform);
    function Duplicate: TPerspectiveTransform;
    function Apply(pt: TPointF): TPointF;
    procedure ScanMoveTo(x,y:single);
    function ScanNext: TPointF;
  end;

type
  { TBGRATwirlScanner applies a twirl transformation.

    Note : this scanner handles integer coordinates only, so
    any further transformation applied after this one may not
    render correctly. }

  { TBGRATwirlScanner }

  TBGRATwirlScanner = Class(TBGRACustomScanner)
  protected
    FScanner: IBGRAScanner;
    FScanAtFunc: TScanAtFunction;
    FCenter: TPoint;
    FTurn, FRadius, FExponent: Single;
  public
    constructor Create(AScanner: IBGRAScanner; ACenter: TPoint; ARadius: single; ATurn: single = 1; AExponent: single = 3);
    function ScanAt(X, Y: Single): TBGRAPixel; override;
    property Radius: Single read FRadius;
    property Center: TPoint read FCenter;
    property Exponent: Single read FExponent;
  end;

implementation

function AffineMatrix(m11, m12, m13, m21, m22, m23: single): TAffineMatrix;
begin
  result[1,1] := m11;
  result[1,2] := m12;
  result[1,3] := m13;
  result[2,1] := m21;
  result[2,2] := m22;
  result[2,3] := m23;
end;

operator *(M, N: TAffineMatrix): TAffineMatrix;
begin
  result[1,1] := M[1,1]*N[1,1] + M[1,2]*N[2,1];
  result[1,2] := M[1,1]*N[1,2] + M[1,2]*N[2,2];
  result[1,3] := M[1,1]*N[1,3] + M[1,2]*N[2,3] + M[1,3];

  result[2,1] := M[2,1]*N[1,1] + M[2,2]*N[2,1];
  result[2,2] := M[2,1]*N[1,2] + M[2,2]*N[2,2];
  result[2,3] := M[2,1]*N[1,3] + M[2,2]*N[2,3] + M[2,3];
end;

operator*(M: TAffineMatrix; V: TPointF): TPointF;
begin
  result.X := V.X*M[1,1]+V.Y*M[1,2]+M[1,3];
  result.Y := V.X*M[2,1]+V.Y*M[2,2]+M[2,3];
end;

function IsAffineMatrixInversible(M: TAffineMatrix): boolean;
begin
  result := M[1,1]*M[2,2]-M[1,2]*M[2,1] <> 0;
end;

function AffineMatrixInverse(M: TAffineMatrix): TAffineMatrix;
var det,f: single;
    linearInverse: TAffineMatrix;
begin
  det := M[1,1]*M[2,2]-M[1,2]*M[2,1];
  if det = 0 then
    raise Exception.Create('Not inversible');
  f := 1/det;
  linearInverse := AffineMatrix(M[2,2]*f,-M[1,2]*f,0,
                         -M[2,1]*f,M[1,1]*f,0);
  result := linearInverse * AffineMatrixTranslation(-M[1,3],-M[2,3]);
end;

function AffineMatrixTranslation(OfsX, OfsY: Single): TAffineMatrix;
begin
  result := AffineMatrix(1, 0, OfsX,
                         0, 1, OfsY);
end;

function AffineMatrixScale(sx, sy: single): TAffineMatrix;
begin
  result := AffineMatrix(sx, 0,    0,
                         0,  sy, 0);
end;

function AffineMatrixRotationRad(Angle: Single): TAffineMatrix;
begin
  result := AffineMatrix(cos(Angle),  sin(Angle), 0,
                         -sin(Angle), cos(Angle), 0);
end;

function AffineMatrixRotationDeg(Angle: Single): TAffineMatrix;
begin
  result := AffineMatrixRotationRad(-Angle*Pi/180);
end;

function AffineMatrixIdentity: TAffineMatrix;
begin
  result := AffineMatrix(1, 0, 0,
                         0, 1, 0);
end;

{ TBGRATriangleLinearMapping }

constructor TBGRATriangleLinearMapping.Create(AScanner: IBGRAScanner; pt1, pt2,
  pt3: TPointF; tex1, tex2, tex3: TPointF);
begin
  FScanner := AScanner;
  FScanAtFunc := @FScanner.ScanAt;

  FMatrix := AffineMatrix(pt2.X-pt1.X, pt3.X-pt1.X, 0,
                          pt2.Y-pt1.Y, pt3.Y-pt1.Y, 0);
  if not IsAffineMatrixInversible(FMatrix) then
    FMatrix := AffineMatrix(0,0,0,0,0,0)
  else
    FMatrix := AffineMatrixInverse(FMatrix) * AffineMatrixTranslation(-pt1.x,-pt1.y);

  FTexCoord1 := tex1;
  FDiff2 := tex2-tex1;
  FDiff3 := tex3-tex1;
  FStep := FDiff2*FMatrix[1,1]+FDiff3*FMatrix[2,1];
end;

procedure TBGRATriangleLinearMapping.ScanMoveTo(X, Y: Integer);
begin
  ScanMoveToF(X, Y);
end;

procedure TBGRATriangleLinearMapping.ScanMoveToF(X, Y: Single);
var
  Cur: TPointF;
begin
  Cur := FMatrix*PointF(X,Y);
  FCurTexCoord := FTexCoord1+FDiff2*Cur.X+FDiff3*Cur.Y;
end;

function TBGRATriangleLinearMapping.ScanAt(X, Y: Single): TBGRAPixel;
begin
  ScanMoveToF(X,Y);
  result := ScanNextPixel;
end;

function TBGRATriangleLinearMapping.ScanNextPixel: TBGRAPixel;
begin
  result := FScanAtFunc(FCurTexCoord.X,FCurTexCoord.Y);
  FCurTexCoord += FStep;
end;

{ TBGRAAffineScannerTransform }

constructor TBGRAAffineScannerTransform.Create(AScanner: IBGRAScanner);
begin
  FScanner := AScanner;
  FScanAtFunc := @FScanner.ScanAt;
  Reset;
end;

procedure TBGRAAffineScannerTransform.Reset;
begin
  FMatrix := AffineMatrixIdentity;
  FEmptyMatrix := False;
end;

procedure TBGRAAffineScannerTransform.Invert;
begin
  if not FEmptyMatrix and IsAffineMatrixInversible(FMatrix) then
    FMatrix := AffineMatrixInverse(FMatrix) else
      FEmptyMatrix := True;
end;

procedure TBGRAAffineScannerTransform.SetMatrix(AMatrix: TAffineMatrix);
begin
  FEmptyMatrix := False;
  FMatrix := AMatrix;
end;

procedure TBGRAAffineScannerTransform.Translate(OfsX, OfsY: Single);
begin
  MultiplyBy(AffineMatrixTranslation(-OfsX,-OfsY));
end;

procedure TBGRAAffineScannerTransform.RotateDeg(Angle: Single);
begin
  MultiplyBy(AffineMatrixRotationDeg(-Angle));
end;

procedure TBGRAAffineScannerTransform.RotateRad(Angle: Single);
begin
  MultiplyBy(AffineMatrixRotationRad(-Angle));
end;

procedure TBGRAAffineScannerTransform.MultiplyBy(AMatrix: TAffineMatrix);
begin
  FMatrix *= AMatrix;
end;

procedure TBGRAAffineScannerTransform.Fit(Origin, HAxis, VAxis: TPointF);
begin
  SetMatrix(AffineMatrix(HAxis.X-Origin.X, VAxis.X-Origin.X, 0,
                         HAxis.Y-Origin.Y, VAxis.Y-Origin.Y, 0));
  Invert;
  Translate(Origin.X,Origin.Y);
end;

procedure TBGRAAffineScannerTransform.Scale(sx, sy: single);
begin
  if (sx=0) or (sy=0) then
  begin
    FEmptyMatrix := True;
    exit;
  end;

  MultiplyBy(AffineMatrixScale(1/sx,1/sy));
end;

procedure TBGRAAffineScannerTransform.Scale(factor: single);
begin
  Scale(factor,factor);
end;

procedure TBGRAAffineScannerTransform.ScanMoveTo(X, Y: Integer);
begin
  ScanMoveToF(X,Y);
end;

procedure TBGRAAffineScannerTransform.ScanMoveToF(X, Y: single);
Var Cur: TPointF;
begin
  Cur := FMatrix * PointF(X,Y);
  FCurX := Cur.X;
  FCurY := Cur.Y;
end;

function TBGRAAffineScannerTransform.InternalScanCurrentPixel: TBGRAPixel;
begin
  if FEmptyMatrix then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  result := FScanAtFunc(FCurX,FCurY);
end;

function TBGRAAffineScannerTransform.ScanNextPixel: TBGRAPixel;
begin
  result := InternalScanCurrentPixel;
  FCurX += FMatrix[1,1];
  FCurY += FMatrix[2,1];
end;

function TBGRAAffineScannerTransform.ScanAt(X, Y: Single): TBGRAPixel;
begin
  ScanMoveToF(X,Y);
  result := InternalScanCurrentPixel;
end;

{ TBGRAAffineBitmapTransform }

constructor TBGRAAffineBitmapTransform.Create(ABitmap: TBGRACustomBitmap;
  ARepeatImage: Boolean; AResampleFilter: TResampleFilter = rfLinear);
begin
  if (ABitmap.Width = 0) or (ABitmap.Height = 0) then
    raise Exception.Create('Empty image');
  inherited Create(ABitmap);
  FBitmap := ABitmap;
  FRepeatImage := ARepeatImage;
  FResampleFilter:= AResampleFilter;
end;

function TBGRAAffineBitmapTransform.InternalScanCurrentPixel: TBGRAPixel;
begin
  if FRepeatImage then
    result := FBitmap.GetPixelCycle(FCurX,FCurY,FResampleFilter) else
    result := FBitmap.GetPixel(FCurX,FCurY,FResampleFilter);
end;

procedure TBGRAAffineBitmapTransform.Fit(Origin, HAxis, VAxis: TPointF);
begin
  SetMatrix(AffineMatrix((HAxis.X-Origin.X)/FBitmap.Width, (VAxis.X-Origin.X)/FBitmap.Height, 0,
                         (HAxis.Y-Origin.Y)/FBitmap.Width, (VAxis.Y-Origin.Y)/FBitmap.Height, 0));
  Invert;
  Translate(Origin.X,Origin.Y);
end;

{ TBGRAPerspectiveScannerTransform }

constructor TBGRAPerspectiveScannerTransform.Create(texture: IBGRAScanner; texCoord1,texCoord2: TPointF; const quad: array of TPointF);
begin
  if DoesQuadIntersect(quad[0],quad[1],quad[2],quad[3]) or not IsConvex(quad,False) or (texCoord1.x = texCoord2.x) or (texCoord1.y = texCoord2.y) then
    FMatrix := nil
  else
    FMatrix := TPerspectiveTransform.Create(quad,texCoord1.x,texCoord1.y,texCoord2.x,texCoord2.y);
  FTexture := texture;
  FScanAtProc:= @FTexture.ScanAt;
end;

constructor TBGRAPerspectiveScannerTransform.Create(texture: IBGRAScanner;
  const texCoordsQuad: array of TPointF; const quad: array of TPointF);
begin
  if DoesQuadIntersect(quad[0],quad[1],quad[2],quad[3]) or not IsConvex(quad,False) or
     DoesQuadIntersect(texCoordsQuad[0],texCoordsQuad[1],texCoordsQuad[2],texCoordsQuad[3]) or not IsConvex(texCoordsQuad,False) then
    FMatrix := nil
  else
    FMatrix := TPerspectiveTransform.Create(quad,texCoordsQuad);
  FTexture := texture;
  FScanAtProc:= @FTexture.ScanAt;
end;

destructor TBGRAPerspectiveScannerTransform.Destroy;
begin
  FMatrix.free;
  inherited Destroy;
end;

procedure TBGRAPerspectiveScannerTransform.ScanMoveTo(X, Y: Integer);
begin
  if FMatrix = nil then exit;
  FMatrix.ScanMoveTo(X,Y);
end;

function TBGRAPerspectiveScannerTransform.ScanAt(X, Y: Single): TBGRAPixel;
var ptSource: TPointF;
begin
  if FMatrix = nil then
    result := BGRAPixelTransparent else
  begin
    ptSource := FMatrix.Apply(PointF(X,Y));
    Result:= FScanAtProc(ptSource.X, ptSource.Y);
  end;
end;

function TBGRAPerspectiveScannerTransform.ScanNextPixel: TBGRAPixel;
var ptSource: TPointF;
begin
  if FMatrix = nil then
    result := BGRAPixelTransparent else
  begin
    ptSource := FMatrix.ScanNext;
    Result:= FScanAtProc(ptSource.X, ptSource.Y);
  end;
end;

{ TPerspectiveTransform }

constructor TPerspectiveTransform.Create;
begin
  AssignIdentity;
end;

constructor TPerspectiveTransform.Create(x1, y1, x2, y2: single;
  const quad: array of TPointF);
begin
  MapRectToQuad(x1 ,y1 ,x2 ,y2 ,quad );
end;

constructor TPerspectiveTransform.Create(const quad: array of TPointF; x1, y1,
  x2, y2: single);
begin
  MapQuadToRect(quad, x1,y1,x2,y2);
end;

constructor TPerspectiveTransform.Create(const srcQuad,
  destQuad: array of TPointF);
begin
  MapQuadToQuad(srcQuad,destQuad);
end;

{ Map a quad to quad. First compute quad to square, and then square to quad. }
function TPerspectiveTransform.MapQuadToQuad(const srcQuad,
  destQuad: array of TPointF): boolean;
var
  p : TPerspectiveTransform;
begin
  if not MapQuadToSquare(srcQuad ) then
  begin
    result:=false;
    exit;
  end;

  p := TPerspectiveTransform.Create;
  if not p.MapSquareToQuad(destQuad) then
  begin
    p.Free;
    result:=false;
    exit;
  end;

  //combine both transformations
  MultiplyBy(p);
  p.Free;
  result:=true;
end;

//Map a rectangle to a quad. Make a polygon for the rectangle, and map it.
function TPerspectiveTransform.MapRectToQuad(x1, y1, x2, y2: single;
  const quad: array of TPointF): boolean;
begin
  result := MapQuadToQuad([PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)], quad);
end;

//Map a quad to a rectangle. Make a polygon for the rectangle, and map the quad into it.
function TPerspectiveTransform.MapQuadToRect(const quad: array of TPointF; x1,
  y1, x2, y2: single): boolean;
begin
 result := MapQuadToQuad(quad, [PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)]);
end;

//Map a square to a quad
function TPerspectiveTransform.MapSquareToQuad(const quad: array of TPointF): boolean;
var
 d,d1,d2: TPointF;
 den ,u ,v : double;

begin
 d := quad[0]-quad[1]+quad[2]-quad[3];

  if (d.x = 0.0 ) and
    (d.y = 0.0 ) then
  begin
  // Affine case (parallelogram)
   sx :=quad[1].x - quad[0].x;
   shy:=quad[1].y - quad[0].y;
   w0 :=0.0;
   shx:=quad[2].x - quad[1].x;
   sy :=quad[2].y - quad[1].y;
   w1 :=0.0;
   tx :=quad[0].x;
   ty :=quad[0].y;
   w2 :=1.0;

  end
 else
  begin
   d1 := quad[1]-quad[2];
   d2 := quad[3]-quad[2];
   den:=d1.x * d2.y - d2.x * d1.y;

   if den = 0.0 then
   begin
    // Singular case
     sx :=0.0;
     shy:=0.0;
     w0 :=0.0;
     shx:=0.0;
     sy :=0.0;
     w1 :=0.0;
     tx :=0.0;
     ty :=0.0;
     w2 :=0.0;
     result:=false;
     exit;
   end;

  // General case
   u:=(d.x * d2.y - d.y * d2.x ) / den;
   v:=(d.y * d1.x - d.x * d1.y ) / den;

   sx :=quad[1].x - quad[0].x + u * quad[1].x;
   shy:=quad[1].y - quad[0].y + u * quad[1].y;
   w0 :=u;
   shx:=quad[3].x - quad[0].x + v * quad[3].x;
   sy :=quad[3].y - quad[0].y + v * quad[3].y;
   w1 :=v;
   tx :=quad[0].x;
   ty :=quad[0].y;
   w2 :=1.0;

  end;

 result:=true;

end;

//Map a quad to a square. Compute mapping from square to quad, then invert.
function TPerspectiveTransform.MapQuadToSquare(const quad: array of TPointF): boolean;
begin
 if not MapSquareToQuad(quad ) then
   result:=false
 else
  result := Invert;
end;

procedure TPerspectiveTransform.AssignIdentity;
begin
 sx :=1;
 shy:=0;
 w0 :=0;
 shx:=0;
 sy :=1;
 w1 :=0;
 tx :=0;
 ty :=0;
 w2 :=1;
end;

function TPerspectiveTransform.Invert: boolean;
var
 d0, d1, d2, d : double;
 copy : TPerspectiveTransform;

begin
 d0:= sy  * w2 - w1  * ty;
 d1:= w0  * ty - shy * w2;
 d2:= shy * w1 - w0  * sy;
 d := sx  * d0 + shx * d1 + tx * d2;

 if d = 0.0 then
 begin
   sx := 0.0;
   shy:= 0.0;
   w0 := 0.0;
   shx:= 0.0;
   sy := 0.0;
   w1 := 0.0;
   tx := 0.0;
   ty := 0.0;
   w2 := 0.0;
   result:= false;
   exit;
 end;

 d:= 1.0 / d;

 copy := Duplicate;

 sx :=d * d0;
 shy:=d * d1;
 w0 :=d * d2;
 shx:=d * (copy.w1  * copy.tx  - copy.shx * copy.w2 );
 sy :=d * (copy.sx  * copy.w2  - copy.w0  * copy.tx );
 w1 :=d * (copy.w0  * copy.shx - copy.sx  * copy.w1 );
 tx :=d * (copy.shx * copy.ty  - copy.sy  * copy.tx );
 ty :=d * (copy.shy * copy.tx  - copy.sx  * copy.ty );
 w2 :=d * (copy.sx  * copy.sy  - copy.shy * copy.shx );

 copy.free;

 result:=true;
end;

procedure TPerspectiveTransform.Translate(dx, dy: single);
begin
 tx:=tx + dx;
 ty:=ty + dy;
end;

procedure TPerspectiveTransform.MultiplyBy(a: TPerspectiveTransform);
var b: TPerspectiveTransform;
begin
  b := Duplicate;
  sx :=a.sx  * b.sx  + a.shx * b.shy + a.tx * b.w0;
  shx:=a.sx  * b.shx + a.shx * b.sy  + a.tx * b.w1;
  tx :=a.sx  * b.tx  + a.shx * b.ty  + a.tx * b.w2;
  shy:=a.shy * b.sx  + a.sy  * b.shy + a.ty * b.w0;
  sy :=a.shy * b.shx + a.sy  * b.sy  + a.ty * b.w1;
  ty :=a.shy * b.tx  + a.sy  * b.ty  + a.ty * b.w2;
  w0 :=a.w0  * b.sx  + a.w1  * b.shy + a.w2 * b.w0;
  w1 :=a.w0  * b.shx + a.w1  * b.sy  + a.w2 * b.w1;
  w2 :=a.w0  * b.tx  + a.w1  * b.ty  + a.w2 * b.w2;
  b.Free;
end;

procedure TPerspectiveTransform.PremultiplyBy(b: TPerspectiveTransform);
var
  a : TPerspectiveTransform;
 begin
  a := Duplicate;
  sx :=a.sx  * b.sx  + a.shx * b.shy + a.tx * b.w0;
  shx:=a.sx  * b.shx + a.shx * b.sy  + a.tx * b.w1;
  tx :=a.sx  * b.tx  + a.shx * b.ty  + a.tx * b.w2;
  shy:=a.shy * b.sx  + a.sy  * b.shy + a.ty * b.w0;
  sy :=a.shy * b.shx + a.sy  * b.sy  + a.ty * b.w1;
  ty :=a.shy * b.tx  + a.sy  * b.ty  + a.ty * b.w2;
  w0 :=a.w0  * b.sx  + a.w1  * b.shy + a.w2 * b.w0;
  w1 :=a.w0  * b.shx + a.w1  * b.sy  + a.w2 * b.w1;
  w2 :=a.w0  * b.tx  + a.w1  * b.ty  + a.w2 * b.w2;
  a.Free;
end;

function TPerspectiveTransform.Duplicate: TPerspectiveTransform;
begin
  result := TPerspectiveTransform.Create;
  result.sx :=sx;
  result.shy:=shy;
  result.w0 :=w0;
  result.shx:=shx;
  result.sy :=sy;
  result.w1 :=w1;
  result.tx :=tx;
  result.ty :=ty;
  result.w2 :=w2;
end;

function TPerspectiveTransform.Apply(pt: TPointF): TPointF;
var
  m : single;
begin
  m:= pt.x * w0 + pt.y * w1 + w2 ;
  if m=0 then
  begin
    result.x := 0;
    result.y := 0;
  end else
  begin
   m := 1/m;
   result.x := m * (pt.x * sx  + pt.y * shx + tx );
   result.y := m * (pt.x * shy + pt.y * sy  + ty );
  end;
end;

procedure TPerspectiveTransform.ScanMoveTo(x, y: single);
begin
  ScanDenom := x * w0 + y * w1 + w2;
  ScanNumX := x * sx  + y * shx + tx;
  scanNumY := x * shy + y * sy  + ty;
end;

function TPerspectiveTransform.ScanNext: TPointF;
var m: single;
begin
  if ScanDenom = 0 then
  begin
    result.x := 0;
    result.y := 0;
  end else
  begin
   m := 1/scanDenom;
   result.x := m * ScanNumX;
   result.y := m * scanNumY;
  end;
  ScanDenom += w0;
  ScanNumX += sx;
  scanNumY += shy;
end;

{ TBGRATwirlScanner }

constructor TBGRATwirlScanner.Create(AScanner: IBGRAScanner; ACenter: TPoint; ARadius: single; ATurn: single = 1; AExponent: single = 3);
begin
  FScanner := AScanner;
  FScanAtFunc := @FScanner.ScanAt;
  FCenter := ACenter;
  FTurn := ATurn;
  FRadius := ARadius;
  FExponent := AExponent;
end;

function TBGRATwirlScanner.ScanAt(X, Y: Single): TBGRAPixel;
var p: TPoint;
    d: single;
    a,cosa,sina: integer;
begin
  p := Point(Round(X)-FCenter.X,Round(Y)-FCenter.Y);
  if (abs(p.x) < FRadius) and (abs(p.Y) < FRadius) then
  begin
    d := sqrt(p.x*p.x+p.y*p.y);
    if d < FRadius then
    begin
      d := (FRadius-d)/FRadius;
      if FExponent <> 1 then d := exp(ln(d)*FExponent);
      a := round(d*FTurn*65536);
      cosa := Cos65536(a)-32768;
      sina := Sin65536(a)-32768;
      result := FScanner.ScanAt((p.x*cosa+p.y*sina)/32768 + FCenter.X,
                                (-p.x*sina+p.y*cosa)/32768 + FCenter.Y);
      exit;
    end;
  end;
  result := FScanAtFunc(X,Y);
end;

end.

