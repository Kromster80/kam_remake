unit BGRAGradientScanner;

{$mode objfpc}{$H+}

interface

{ This unit contains scanners that generate gradients }

uses
  Classes, SysUtils, BGRABitmapTypes, BGRATransform;

type
  { TBGRASimpleGradientWithoutGammaCorrection }

  TBGRASimpleGradientWithoutGammaCorrection = class(TBGRACustomGradient)
  private
    FColor1,FColor2: TBGRAPixel;
  public
    constructor Create(Color1,Color2: TBGRAPixel);
    function GetColorAt(position: integer): TBGRAPixel; override;
    function GetAverageColor: TBGRAPixel; override;
  end;

  { TBGRASimpleGradientWithGammaCorrection }

  TBGRASimpleGradientWithGammaCorrection = class(TBGRACustomGradient)
  private
    FColor1,FColor2: TBGRAPixel;
    ec1,ec2: TExpandedPixel;
  public
    constructor Create(Color1,Color2: TBGRAPixel);
    function GetColorAt(position: integer): TBGRAPixel; override;
    function GetAverageColor: TBGRAPixel; override;
  end;

  { TBGRAMultiGradient }

  TBGRAMultiGradient = class(TBGRACustomGradient)
  private
    FColors: array of TBGRAPixel;
    FPositions: array of integer;
    FEColors: array of TExpandedPixel;
    FCycle: Boolean;
    procedure Init(Colors: array of TBGRAPixel; Positions: array of Integer; AGammaCorrection, ACycle: boolean);
  public
    GammaCorrection: boolean;
    constructor Create(Colors: array of TBGRAPixel; Positions0To1: array of single; AGammaCorrection: boolean; ACycle: boolean = false);
    function GetColorAt(position: integer): TBGRAPixel; override;
    function GetAverageColor: TBGRAPixel; override;
  end;

  { TBGRAGradientScanner }

  TBGRAGradientScanner = class(TBGRACustomScanner)
    FCurX,FCurY: integer;
    FGradientType: TGradientType;
    FOrigin1,FOrigin2: TPointF;
    FSinus: Boolean;
    u: TPointF;
    len: single;
    mergedColor: TBGRAPixel;
    FGradient: TBGRACustomGradient;
    FGradientOwner: boolean;
    procedure Init(gtype: TGradientType; o1, o2: TPointF; Sinus: Boolean=False);
  public
    constructor Create(c1, c2: TBGRAPixel; gtype: TGradientType; o1, o2: TPointF;
                       gammaColorCorrection: boolean = True; Sinus: Boolean=False);
    constructor Create(gradient: TBGRACustomGradient; gtype: TGradientType; o1, o2: TPointF; Sinus: Boolean=False);
    destructor Destroy; override;
    procedure ScanMoveTo(X, Y: Integer); override;
    function ScanNextPixel: TBGRAPixel; override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
  end;

  { TBGRAGradientTriangleScanner }

  TBGRAGradientTriangleScanner= class(TBGRACustomScanner)
  protected
    FMatrix: TAffineMatrix;
    FColor1,FDiff2,FDiff3,FStep: TColorF;
    FCurColor: TColorF;
  public
    constructor Create(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel);
    procedure ScanMoveTo(X,Y: Integer); override;
    procedure ScanMoveToF(X,Y: Single);
    function ScanAt(X,Y: Single): TBGRAPixel; override;
    function ScanNextPixel: TBGRAPixel; override;
  end;

implementation

{ TBGRAMultiGradient }

procedure TBGRAMultiGradient.Init(Colors: array of TBGRAPixel;
  Positions: array of Integer; AGammaCorrection, ACycle: boolean);
var
  i: Integer;
begin
  if length(positions) <> length(colors) then
    raise Exception.Create('Dimension mismatch');
  if length(positions) = 0 then
    raise Exception.Create('Empty gradient');
  setlength(FColors,length(Colors));
  setlength(FPositions,length(Positions));
  setlength(FEColors,length(Colors));
  for i := 0 to high(colors) do
  begin
    FColors[i]:= colors[i];
    FPositions[i]:= Positions[i];
    FEColors[i]:= GammaExpansion(colors[i]);
  end;
  GammaCorrection := AGammaCorrection;
  FCycle := ACycle;
  if FPositions[high(FPositions)] = FPositions[0] then FCycle := false;
end;

constructor TBGRAMultiGradient.Create(Colors: array of TBGRAPixel;
  Positions0To1: array of single; AGammaCorrection: boolean; ACycle: boolean);
var intPositions: array of integer;
  i: Integer;
begin
  setlength(intPositions, length(positions0to1));
  for i := 0 to High(Positions0To1) do
    intPositions[i] := round(Positions0To1[i]*65536);
  Init(Colors,intPositions,AGammaCorrection, ACycle);
end;

function TBGRAMultiGradient.GetColorAt(position: integer): TBGRAPixel;
var i: integer;
    ec: TExpandedPixel;
begin
  if FCycle then
    position := (position-FPositions[0]) mod (FPositions[high(FPositions)] - FPositions[0]) + FPositions[0];
  if position <= FPositions[0] then
    result := FColors[0] else
  if position >= FPositions[high(FPositions)] then
    result := FColors[high(FColors)] else
  begin
    i := 0;
    while (i < high(FPositions)) and (position > FPositions[i+1]) do
      inc(i);

    if Position = FPositions[i+1] then
      result := FColors[i+1]
    else
    if GammaCorrection then
    begin
      ec.red := FEColors[i].red + (position-FPositions[i])*(FEColors[i+1].red-FEColors[i].red) div (FPositions[i+1]-FPositions[i]);
      ec.green := FEColors[i].green + (position-FPositions[i])*(FEColors[i+1].green-FEColors[i].green) div (FPositions[i+1]-FPositions[i]);
      ec.blue := FEColors[i].blue + (position-FPositions[i])*(FEColors[i+1].blue-FEColors[i].blue) div (FPositions[i+1]-FPositions[i]);
      ec.alpha := FEColors[i].alpha + (position-FPositions[i])*(FEColors[i+1].alpha-FEColors[i].alpha) div (FPositions[i+1]-FPositions[i]);
      result := GammaCompression(ec);
    end else
    begin
      result.red := FColors[i].red + (position-FPositions[i])*(FColors[i+1].red-FColors[i].red) div (FPositions[i+1]-FPositions[i]);
      result.green := FColors[i].green + (position-FPositions[i])*(FColors[i+1].green-FColors[i].green) div (FPositions[i+1]-FPositions[i]);
      result.blue := FColors[i].blue + (position-FPositions[i])*(FColors[i+1].blue-FColors[i].blue) div (FPositions[i+1]-FPositions[i]);
      result.alpha := FColors[i].alpha + (position-FPositions[i])*(FColors[i+1].alpha-FColors[i].alpha) div (FPositions[i+1]-FPositions[i]);
    end;
  end;
end;

function TBGRAMultiGradient.GetAverageColor: TBGRAPixel;
var sumR,sumG,sumB,sumA: integer;
  i: Integer;
begin
  sumR := 0;
  sumG := 0;
  sumB := 0;
  sumA := 0;
  for i := 0 to high(FColors) do
  begin
    sumR += FColors[i].red;
    sumG += FColors[i].green;
    sumB += FColors[i].blue;
    sumA += FColors[i].alpha;
  end;
  result := BGRA(sumR div length(FColors),sumG div length(FColors),
    sumB div length(FColors),sumA div length(FColors));
end;

{ TBGRASimpleGradientWithGammaCorrection }

constructor TBGRASimpleGradientWithGammaCorrection.Create(Color1,
  Color2: TBGRAPixel);
begin
  FColor1 := Color1;
  FColor2 := Color2;
  ec1 := GammaExpansion(Color1);
  ec2 := GammaExpansion(Color2);
end;

function TBGRASimpleGradientWithGammaCorrection.GetColorAt(position: integer
  ): TBGRAPixel;
var b,b2: cardinal;
    ec: TExpandedPixel;
begin
  if position < 0 then
    result := FColor1 else
  if position >= 65536 then
    result := FColor2 else
  begin
    b      := position;
    b2     := 65536-b;
    ec.red := (ec1.red * b2 + ec2.red * b + 32767) shr 16;
    ec.green := (ec1.green * b2 + ec2.green * b + 32767) shr 16;
    ec.blue := (ec1.blue * b2 + ec2.blue * b + 32767) shr 16;
    ec.alpha := (ec1.alpha * b2 + ec2.alpha * b + 32767) shr 16;
    result := GammaCompression(ec);
  end;
end;

function TBGRASimpleGradientWithGammaCorrection.GetAverageColor: TBGRAPixel;
begin
  result := GammaCompression(MergeBGRA(ec1,ec2));
end;

{ TBGRASimpleGradientWithoutGammaCorrection }

constructor TBGRASimpleGradientWithoutGammaCorrection.Create(Color1,
  Color2: TBGRAPixel);
begin
  FColor1 := Color1;
  FColor2 := Color2;
end;

function TBGRASimpleGradientWithoutGammaCorrection.GetColorAt(position: integer
  ): TBGRAPixel;
var b,b2: cardinal;
begin
  if position < 0 then
    result := FColor1 else
  if position >= 65536 then
    result := FColor2 else
  begin
    b      := position shr 6;
    b2     := 1024-b;
    result.red  := (FColor1.red * b2 + FColor2.red * b + 511) shr 10;
    result.green := (FColor1.green * b2 + FColor2.green * b + 511) shr 10;
    result.blue := (FColor1.blue * b2 + FColor2.blue * b + 511) shr 10;
    result.alpha := (FColor1.alpha * b2 + FColor2.alpha * b + 511) shr 10;
  end;
end;

function TBGRASimpleGradientWithoutGammaCorrection.GetAverageColor: TBGRAPixel;
begin
  result := MergeBGRA(FColor1,FColor2);
end;

{ TBGRAGradientTriangleScanner }

constructor TBGRAGradientTriangleScanner.Create(pt1, pt2, pt3: TPointF; c1, c2,
  c3: TBGRAPixel);
var ec1,ec2,ec3: TExpandedPixel;
begin
  FMatrix := AffineMatrix(pt2.X-pt1.X, pt3.X-pt1.X, 0,
                          pt2.Y-pt1.Y, pt3.Y-pt1.Y, 0);
  if not IsAffineMatrixInversible(FMatrix) then
    FMatrix := AffineMatrix(0,0,0,0,0,0)
  else
    FMatrix := AffineMatrixInverse(FMatrix) * AffineMatrixTranslation(-pt1.x,-pt1.y);

  ec1 := GammaExpansion(c1);
  ec2 := GammaExpansion(c2);
  ec3 := GammaExpansion(c3);
  FColor1[1] := ec1.red;
  FColor1[2] := ec1.green;
  FColor1[3] := ec1.blue;
  FColor1[4] := ec1.alpha;
  FDiff2[1] := ec2.red - ec1.red;
  FDiff2[2] := ec2.green - ec1.green;
  FDiff2[3] := ec2.blue - ec1.blue;
  FDiff2[4] := ec2.alpha - ec1.alpha;
  FDiff3[1] := ec3.red - ec1.red;
  FDiff3[2] := ec3.green - ec1.green;
  FDiff3[3] := ec3.blue - ec1.blue;
  FDiff3[4] := ec3.alpha - ec1.alpha;
  FStep := FDiff2*FMatrix[1,1]+FDiff3*FMatrix[2,1];
end;

procedure TBGRAGradientTriangleScanner.ScanMoveTo(X, Y: Integer);
begin
  ScanMoveToF(X, Y);
end;

procedure TBGRAGradientTriangleScanner.ScanMoveToF(X, Y: Single);
var
  Cur: TPointF;
begin
  Cur := FMatrix*PointF(X,Y);
  FCurColor := FColor1+FDiff2*Cur.X+FDiff3*Cur.Y;
end;

function TBGRAGradientTriangleScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  ScanMoveToF(X,Y);
  result := ScanNextPixel;
end;

function TBGRAGradientTriangleScanner.ScanNextPixel: TBGRAPixel;
var r,g,b,a: int64;
begin
  r := round(FCurColor[1]);
  g := round(FCurColor[2]);
  b := round(FCurColor[3]);
  a := round(FCurColor[4]);
  if r > 65535 then r := 65535 else
  if r < 0 then r := 0;
  if g > 65535 then g := 65535 else
  if g < 0 then g := 0;
  if b > 65535 then b := 65535 else
  if b < 0 then b := 0;
  if a > 65535 then a := 65535 else
  if a < 0 then a := 0;
  result.red := GammaCompressionTab[r];
  result.green := GammaCompressionTab[g];
  result.blue := GammaCompressionTab[b];
  result.alpha := a shr 8;
  FCurColor += FStep;
end;

{ TBGRAGradientScanner }

procedure TBGRAGradientScanner.Init(gtype: TGradientType; o1, o2: TPointF;
  Sinus: Boolean);
begin
  FGradientType:= gtype;
  FOrigin1 := o1;
  FOrigin2 := o2;
  FSinus := Sinus;

  //compute vector
  u.x := o2.x - o1.x;
  u.y := o2.y - o1.y;
  len := sqrt(sqr(u.x) + sqr(u.y));
  if len <> 0 then
  begin
    u.x /= len;
    u.y /= len;
  end;

  mergedColor := FGradient.GetAverageColor;
end;

constructor TBGRAGradientScanner.Create(c1, c2: TBGRAPixel;
  gtype: TGradientType; o1, o2: TPointF; gammaColorCorrection: boolean;
  Sinus: Boolean);
begin
  //transparent pixels have no color so
  //take it from other color
  if c1.alpha = 0 then
  begin
    c1.red   := c2.red;
    c1.green := c2.green;
    c1.blue  := c2.blue;
  end
  else
  if c2.alpha = 0 then
  begin
    c2.red   := c1.red;
    c2.green := c1.green;
    c2.blue  := c1.blue;
  end;

  if gammaColorCorrection then
  begin
    FGradient := TBGRASimpleGradientWithGammaCorrection.Create(c1,c2);
    FGradientOwner := true;
  end else
  begin
    FGradient := TBGRASimpleGradientWithoutGammaCorrection.Create(c1,c2);
    FGradientOwner := true;
  end;
  Init(gtype,o1,o2,Sinus);
end;

constructor TBGRAGradientScanner.Create(gradient: TBGRACustomGradient;
  gtype: TGradientType; o1, o2: TPointF; Sinus: Boolean);
begin
  FGradient := gradient;
  FGradientOwner := false;
  Init(gtype,o1,o2,Sinus);
end;

destructor TBGRAGradientScanner.Destroy;
begin
  if FGradientOwner then
    FGradient.Free;
  inherited Destroy;
end;

procedure TBGRAGradientScanner.ScanMoveTo(X, Y: Integer);
begin
  FCurX := X;
  FCurY := Y;
end;

function TBGRAGradientScanner.ScanNextPixel: TBGRAPixel;
begin
  result := ScanAt(FCurX,FCurY);
  Inc(FCurX);
end;

function TBGRAGradientScanner.ScanAt(X, Y: Single): TBGRAPixel;
var p: TPointF;
    a,a2: single;
    ai: int64;
begin
  if len = 0 then
  begin
    result := mergedColor;
    exit;
  end;

  p.x := X - FOrigin1.x;
  p.y := Y - FOrigin1.y;
  case FGradientType of
    gtLinear:    a := p.x * u.x + p.y * u.y;
    gtReflected: a := abs(p.x * u.x + p.y * u.y);
    gtDiamond:
        begin
          a   := abs(p.x * u.x + p.y * u.y);
          a2  := abs(p.x * u.y - p.y * u.x);
          if a2 > a then a := a2;
        end;
    gtRadial:    a := sqrt(sqr(p.x * u.x + p.y * u.y) + sqr(p.x * u.y - p.y * u.x));
  end;
  ai := round(a/len*65536);
  if FSinus then ai := Sin65536(ai);

  if ai <= low(integer) then
    result := FGradient.GetColorAt(low(integer))
  else
  if ai >= high(integer) then
    result := FGradient.GetColorAt(high(integer))
  else
    result := FGradient.GetColorAt(ai);
end;

end.

