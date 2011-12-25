{
 /**************************************************************************\
                                bgrabitmaptypes.pas
                                -------------------
                   This unit defines basic types and it must be
                   included in the 'uses' clause.

       --> Include BGRABitmap and BGRABitmapTypes in the 'uses' clause.

 ****************************************************************************
 *                                                                          *
 *  This file is part of BGRABitmap library which is distributed under the  *
 *  modified LGPL.                                                          *
 *                                                                          *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,   *
 *  for details about the copyright.                                        *
 *                                                                          *
 *  This program is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    *
 *                                                                          *
 ****************************************************************************
}

unit BGRABitmapTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, Graphics, FPImage, FPImgCanv, GraphType;

type
  //pointer for direct pixel access
  PBGRAPixel = ^TBGRAPixel;

  //pixel structure
  TBGRAPixel = packed record
    blue, green, red, alpha: byte;
  end;

  //gamma expanded values
  TExpandedPixel = packed record
    red, green, blue, alpha: word;
  end;

  //pixel color defined in HSL colorspace
  THSLAPixel = packed record
    hue, saturation, lightness, alpha: word;
  end;

  //general purpose color variable with floating point values
  TColorF = array[1..4] of single;
  
  { These types are used as parameters }

  TDrawMode = (dmSet,                   //replace pixels
               dmSetExceptTransparent,  //draw pixels with alpha=255
               dmLinearBlend,           //blend without gamma correction
               dmDrawWithTransparency); //normal blending with gamma correction
  TChannel = (cRed, cGreen, cBlue, cAlpha);
               
  //floodfill option
  TFloodfillMode = (fmSet,                   //set pixels
                    fmDrawWithTransparency,  //draw fill color with transparency
                    fmProgressive);          //draw fill color with transparency according to similarity with start color

  TResampleMode = (rmSimpleStretch,   //low quality resample
                   rmFineResample);   //use resample filters 
  TResampleFilter = (rfLinear,        //linear interpolation
                     rfHalfCosine,    //mix of rfLinear and rfCosine
                     rfCosine,        //cosine-like interpolation
                     rfBicubic,       //simple bi-cubic filter (blur)
                     rfMitchell,      //downsizing interpolation
                     rfSpline,        //upsizing interpolation
                     rfBestQuality);  //mix of rfMitchell and rfSpline

  TMedianOption = (moNone, moLowSmooth, moMediumSmooth, moHighSmooth);
  TRadialBlurType = (rbNormal, rbDisk, rbCorona, rbPrecise, rbFast);
  TSplineStyle = (ssInside, ssCrossing, ssOutside, ssRoundOutside, ssVertexToSide);
  
  //Advanced blending modes
  //see : http://www.brighthub.com/multimedia/photography/articles/18301.aspx
  //and : http://www.pegtop.net/delphi/articles/blendmodes/  
  TBlendOperation = (boLinearBlend, boTransparent, boMultiply,
    boLinearMultiply, boAdditive, boLinearAdd, boColorBurn, boColorDodge, boReflect,
    boGlow, boOverlay, boDifference, boLinearDifference, boNegation,
    boLinearNegation, boLighten, boDarken, boScreen, boXor);

  TGradientType = (gtLinear, gtReflected, gtDiamond, gtRadial);
const
  GradientTypeStr : array[TGradientType] of string =
  ('Linear','Reflected','Diamond','Radial');
function StrToGradientType(str: string): TGradientType;
 
type
  { A pen style is defined as a list of floating number. The first number is the length of the first dash,
    the second number is the length of the first gap, the third number is the length of the second dash... 
    It must have an even number of values. }
  TBGRAPenStyle = Array Of Single;
  TRoundRectangleOption = (rrTopLeftSquare,rrTopRightSquare,rrBottomRightSquare,rrBottomLeftSquare,
                           rrTopLeftBevel,rrTopRightBevel,rrBottomRightBevel,rrBottomLeftBevel);
  TRoundRectangleOptions = set of TRoundRectangleOption;
  TPolygonOrder = (poNone, poFirstOnTop, poLastOnTop); //see TBGRAMultiShapeFiller in BGRAPolygon
  
function BGRAPenStyle(dash1, space1: single; dash2: single=0; space2: single = 0; dash3: single=0; space3: single = 0; dash4 : single = 0; space4 : single = 0): TBGRAPenStyle;  
  
{ Point, polygon and curve structures }
type
  PPointF = ^TPointF;
  TPointF = record
    x, y: single;
  end;
  ArrayOfTPointF = array of TPointF;
  TArcOption = (aoClosePath, aoPie, aoFillPath);
  TArcOptions = set of TArcOption;

  TCubicBezierCurve = record
    p1,c1,c2,p2: TPointF;
  end;
  TQuadraticBezierCurve = record
    p1,c,p2: TPointF;
  end;

function ConcatPointsF(const APolylines: array of ArrayOfTPointF): ArrayOfTPointF;

function BezierCurve(origin, control1, control2, destination: TPointF) : TCubicBezierCurve; overload;
function BezierCurve(origin, control, destination: TPointF) : TQuadraticBezierCurve; overload;

{ Useful constants }
const
  dmFastBlend = dmLinearBlend;
  EmptySingle: single = -3.402823e38;                        //used as a separator in floating point lists
  EmptyPointF: TPointF = (x: -3.402823e38; y: -3.402823e38); //used as a separator in TPointF lists
  BGRAPixelTransparent: TBGRAPixel = (blue: 0; green: 0; red: 0; alpha: 0);
  BGRAWhite: TBGRAPixel = (blue: 255; green: 255; red: 255; alpha: 255);
  BGRABlack: TBGRAPixel = (blue: 0; green: 0; red: 0; alpha: 255);
  
  { This color is needed for drawing black shapes on the standard TCanvas, because
    when drawing with pure black, there is no way to know if something has been
    drawn or if it is transparent }
  clBlackOpaque = TColor($010000);

function isEmptyPointF(pt: TPointF): boolean;

type
  { A scanner is like an image, but its content has no limit and can be calculated on the fly }
  IBGRAScanner = interface
    procedure ScanMoveTo(X,Y: Integer);
    function ScanNextPixel: TBGRAPixel;
    function ScanAt(X,Y: Single): TBGRAPixel;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode);
    function IsScanPutPixelsDefined: boolean;
  end;

  TScanAtFunction = function (X,Y: Single): TBGRAPixel of object;
  TScanNextPixelFunction = function: TBGRAPixel of object;
  TBGRACustomGradient = class;

  { TBGRACustomBitmap }

  TBGRACustomBitmap = class(TFPCustomImage,IBGRAScanner) // a bitmap can be used as a scanner
  protected
     { accessors to properies }
     function GetHeight: integer; virtual; abstract;
     function GetWidth: integer; virtual; abstract;
     function GetDataPtr: PBGRAPixel; virtual; abstract;
     function GetNbPixels: integer; virtual; abstract;
     function CheckEmpty: boolean; virtual; abstract;
     function GetHasTransparentPixels: boolean; virtual; abstract;
     function GetAverageColor: TColor; virtual; abstract;
     function GetAveragePixel: TBGRAPixel; virtual; abstract;
     procedure SetCanvasOpacity(AValue: byte); virtual; abstract;
     function GetScanLine(y: integer): PBGRAPixel; virtual; abstract;
     function GetRefCount: integer; virtual; abstract;
     function GetBitmap: TBitmap; virtual; abstract;
     function GetLineOrder: TRawImageLineOrder; virtual; abstract;
     function GetCanvasFP: TFPImageCanvas; virtual; abstract;
     function GetCanvasDrawModeFP: TDrawMode; virtual; abstract;
     procedure SetCanvasDrawModeFP(const AValue: TDrawMode); virtual; abstract;
     function GetCanvas: TCanvas; virtual; abstract;
     function GetCanvasOpacity: byte; virtual; abstract;
     function GetCanvasAlphaCorrection: boolean; virtual; abstract;
     procedure SetCanvasAlphaCorrection(const AValue: boolean); virtual; abstract;
     function GetFontHeight: integer; virtual; abstract;
     procedure SetFontHeight(AHeight: integer); virtual; abstract;
     function GetPenStyle: TPenStyle; virtual; abstract;
     procedure SetPenStyle(const AValue: TPenStyle); virtual; abstract;
     function GetCustomPenStyle: TBGRAPenStyle; virtual; abstract;
     procedure SetCustomPenStyle(const AValue: TBGRAPenStyle); virtual; abstract;
     function GetClipRect: TRect; virtual; abstract;
     procedure SetClipRect(const AValue: TRect); virtual; abstract;

  public
     Caption:   string;  //user defined caption

     //font style
     FontName:  string;
     FontStyle: TFontStyles;
     FontAntialias:   Boolean; //antialiasing (it's different from TFont antialiasing mode)
     FontOrientation: integer;

     //line style
     LineCap:   TPenEndCap;
     JoinStyle: TPenJoinStyle;

     FillMode:  TFillMode;  //winding or alternate

     { The resample filter is used when resizing the bitmap, and
       scan interpolation filter is used when the bitmap is used
       as a scanner (IBGRAScanner) }
     ResampleFilter,
     ScanInterpolationFilter: TResampleFilter;
     ScanOffset: TPoint;

     function NewBitmap(AWidth, AHeight: integer): TBGRACustomBitmap; virtual; abstract;
     function NewBitmap(Filename: string): TBGRACustomBitmap; virtual; abstract;

     procedure LoadFromFile(const filename: string); virtual;
     procedure LoadFromStream(Str: TStream); virtual;
     procedure LoadFromStream(Str: TStream; Handler: TFPCustomImageReader); virtual;
     procedure SaveToFile(const filename: string); virtual;
     procedure SaveToFile(const filename: string; Handler:TFPCustomImageWriter); virtual;
     procedure Assign(ABitmap: TBitmap); virtual; abstract; overload;
     procedure Assign(MemBitmap: TBGRACustomBitmap); virtual; abstract; overload;

     {Pixel functions}
     procedure SetPixel(x, y: integer; c: TColor); virtual; abstract; overload;
     procedure XorPixel(x, y: integer; c: TBGRAPixel); virtual; abstract; overload;
     procedure SetPixel(x, y: integer; c: TBGRAPixel); virtual; abstract; overload;
     procedure DrawPixel(x, y: integer; c: TBGRAPixel); virtual; abstract; overload;
     procedure DrawPixel(x, y: integer; ec: TExpandedPixel); virtual; abstract; overload;
     procedure FastBlendPixel(x, y: integer; c: TBGRAPixel); virtual; abstract;
     procedure ErasePixel(x, y: integer; alpha: byte); virtual; abstract;
     procedure AlphaPixel(x, y: integer; alpha: byte); virtual; abstract;
     function GetPixel(x, y: integer): TBGRAPixel; virtual; abstract; overload;
     function GetPixel(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; virtual; abstract; overload;
     function GetPixelCycle(x, y: integer): TBGRAPixel; virtual;
     function GetPixelCycle(x, y: single; AResampleFilter: TResampleFilter = rfLinear): TBGRAPixel; virtual; abstract; overload;

     {Line primitives}
     procedure SetHorizLine(x, y, x2: integer; c: TBGRAPixel); virtual; abstract;
     procedure DrawHorizLine(x, y, x2: integer; c: TBGRAPixel); virtual; abstract; overload;
     procedure DrawHorizLine(x, y, x2: integer; ec: TExpandedPixel); virtual; abstract; overload;
     procedure DrawHorizLine(x, y, x2: integer; texture: IBGRAScanner); virtual; abstract; overload;
     procedure FastBlendHorizLine(x, y, x2: integer; c: TBGRAPixel); virtual; abstract;
     procedure AlphaHorizLine(x, y, x2: integer; alpha: byte); virtual; abstract;
     procedure SetVertLine(x, y, y2: integer; c: TBGRAPixel); virtual; abstract;
     procedure DrawVertLine(x, y, y2: integer; c: TBGRAPixel); virtual; abstract;
     procedure AlphaVertLine(x, y, y2: integer; alpha: byte); virtual; abstract;
     procedure FastBlendVertLine(x, y, y2: integer; c: TBGRAPixel); virtual; abstract;
     procedure DrawHorizLineDiff(x, y, x2: integer; c, compare: TBGRAPixel;
       maxDiff: byte); virtual; abstract;

     {Shapes}
     procedure DrawLine(x1, y1, x2, y2: integer; c: TBGRAPixel; DrawLastPixel: boolean); virtual; abstract;
     procedure DrawLineAntialias(x1, y1, x2, y2: integer; c: TBGRAPixel; DrawLastPixel: boolean); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: integer; c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: single; c: TBGRAPixel; w: single); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: single; texture: IBGRAScanner; w: single); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: single; c: TBGRAPixel; w: single; Closed: boolean); virtual; abstract; overload;
     procedure DrawLineAntialias(x1, y1, x2, y2: single; texture: IBGRAScanner; w: single; Closed: boolean); virtual; abstract; overload;

     procedure DrawPolyLineAntialias(const points: array of TPoint; c: TBGRAPixel; DrawLastPixel: boolean); virtual; overload;
     procedure DrawPolyLineAntialias(const points: array of TPoint; c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean); virtual; overload;
     procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single); virtual; abstract; overload;
     procedure DrawPolyLineAntialias(const points: array of TPointF; texture: IBGRAScanner; w: single); virtual; abstract; overload;
     procedure DrawPolyLineAntialias(const points: array of TPointF; c: TBGRAPixel; w: single; Closed: boolean); virtual; abstract; overload;
     procedure DrawPolygonAntialias(const points: array of TPointF; c: TBGRAPixel; w: single); virtual; abstract; overload;
     procedure DrawPolygonAntialias(const points: array of TPointF; texture: IBGRAScanner; w: single); virtual; abstract; overload;
     procedure EraseLineAntialias(x1, y1, x2, y2: single; alpha: byte; w: single); virtual; abstract; overload;
     procedure EraseLineAntialias(x1, y1, x2, y2: single; alpha: byte; w: single; Closed: boolean); virtual; abstract; overload;
     procedure ErasePolyLineAntialias(const points: array of TPointF; alpha: byte; w: single); virtual; abstract; overload;

     procedure FillTriangleLinearColor(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); virtual; abstract; overload;
     procedure FillTriangleLinearColorAntialias(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel); virtual; abstract; overload;
     procedure FillTriangleLinearMapping(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; TextureInterpolation: Boolean= True); virtual; abstract; overload;
     procedure FillTriangleLinearMappingLightness(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF; light1,light2,light3: word; TextureInterpolation: Boolean= True); virtual; abstract; overload;
     procedure FillTriangleLinearMappingAntialias(pt1,pt2,pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF); virtual; abstract; overload;

     procedure FillQuadLinearColor(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); virtual; abstract; overload;
     procedure FillQuadLinearColorAntialias(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel); virtual; abstract; overload;
     procedure FillQuadLinearMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; TextureInterpolation: Boolean= True);  virtual; abstract; overload;
     procedure FillQuadLinearMappingLightness(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF; light1,light2,light3,light4: word; TextureInterpolation: Boolean= True); virtual; abstract; overload;
     procedure FillQuadLinearMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); virtual; abstract; overload;
     procedure FillQuadPerspectiveMapping(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); virtual; abstract; overload;
     procedure FillQuadPerspectiveMappingAntialias(pt1,pt2,pt3,pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF); virtual; abstract; overload;

     procedure FillPolyLinearColor(const points: array of TPointF; AColors: array of TBGRAPixel);  virtual; abstract; overload;
     procedure FillPolyLinearMapping(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean); virtual; abstract; overload;
     procedure FillPolyLinearMappingLightness(const points: array of TPointF; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean); virtual; abstract; overload;
     procedure FillPolyPerspectiveMapping(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; TextureInterpolation: Boolean); virtual; abstract; overload;
     procedure FillPolyPerspectiveMappingLightness(const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner; texCoords: array of TPointF; lightnesses: array of word; TextureInterpolation: Boolean); virtual; abstract; overload;

     procedure FillPoly(const points: array of TPointF; c: TBGRAPixel; drawmode: TDrawMode); virtual; abstract;
     procedure FillPoly(const points: array of TPointF; texture: IBGRAScanner; drawmode: TDrawMode); virtual; abstract;
     procedure FillPolyAntialias(const points: array of TPointF; c: TBGRAPixel); virtual; abstract;
     procedure FillPolyAntialias(const points: array of TPointF; texture: IBGRAScanner); virtual; abstract;
     procedure ErasePoly(const points: array of TPointF; alpha: byte); virtual; abstract;
     procedure ErasePolyAntialias(const points: array of TPointF; alpha: byte); virtual; abstract;

     procedure EllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel; w: single); virtual; abstract;
     procedure EllipseAntialias(x, y, rx, ry: single; texture: IBGRAScanner; w: single); virtual; abstract;
     procedure EllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel; w: single; back: TBGRAPixel); virtual; abstract;
     procedure FillEllipseAntialias(x, y, rx, ry: single; c: TBGRAPixel); virtual; abstract;
     procedure FillEllipseAntialias(x, y, rx, ry: single; texture: IBGRAScanner); virtual; abstract;
     procedure FillEllipseLinearColorAntialias(x, y, rx, ry: single; outercolor, innercolor: TBGRAPixel); virtual; abstract;
     procedure EraseEllipseAntialias(x, y, rx, ry: single; alpha: byte); virtual; abstract;

     procedure Rectangle(x, y, x2, y2: integer; c: TBGRAPixel; mode: TDrawMode); virtual; abstract; overload;
     procedure Rectangle(x, y, x2, y2: integer; BorderColor, FillColor: TBGRAPixel; mode: TDrawMode); virtual; abstract; overload;
     procedure Rectangle(x, y, x2, y2: integer; c: TColor); virtual; overload;
     procedure Rectangle(r: TRect; c: TBGRAPixel; mode: TDrawMode); virtual; overload;
     procedure Rectangle(r: TRect; BorderColor, FillColor: TBGRAPixel; mode: TDrawMode); virtual;overload;
     procedure Rectangle(r: TRect; c: TColor); virtual; overload;
     procedure RectangleAntialias(x, y, x2, y2: single; c: TBGRAPixel; w: single); virtual; overload;
     procedure RectangleAntialias(x, y, x2, y2: single; c: TBGRAPixel; w: single; back: TBGRAPixel); virtual; abstract; overload;
     procedure RectangleAntialias(x, y, x2, y2: single; texture: IBGRAScanner; w: single); virtual; abstract; overload;

     procedure RoundRect(X1, Y1, X2, Y2: integer; DX, DY: integer; BorderColor, FillColor: TBGRAPixel); virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; w: single; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; pencolor: TBGRAPixel; w: single; fillcolor: TBGRAPixel; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; penTexture: IBGRAScanner; w: single; fillTexture: IBGRAScanner; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure RoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; w: single; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure FillRoundRectAntialias(x,y,x2,y2,rx,ry: single; c: TBGRAPixel; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure FillRoundRectAntialias(x,y,x2,y2,rx,ry: single; texture: IBGRAScanner; options: TRoundRectangleOptions = []); virtual; abstract;
     procedure EraseRoundRectAntialias(x,y,x2,y2,rx,ry: single; alpha: byte; options: TRoundRectangleOptions = []); virtual; abstract;

     procedure FillRect(r: TRect; c: TColor); virtual;
     procedure FillRect(r: TRect; c: TBGRAPixel; mode: TDrawMode); virtual;
     procedure FillRect(x, y, x2, y2: integer; c: TColor); virtual;
     procedure FillRect(x, y, x2, y2: integer; c: TBGRAPixel; mode: TDrawMode); virtual; abstract;
     procedure FillRect(x, y, x2, y2: integer; texture: IBGRAScanner; mode: TDrawMode); virtual; abstract;
     procedure FillRectAntialias(x, y, x2, y2: single; c: TBGRAPixel); virtual; abstract;
     procedure FillRectAntialias(x, y, x2, y2: single; texture: IBGRAScanner); virtual; abstract;
     procedure EraseRectAntialias(x, y, x2, y2: single; alpha: byte); virtual; abstract;
     procedure AlphaFillRect(x, y, x2, y2: integer; alpha: byte); virtual; abstract;

     procedure TextOut(x, y: integer; s: string; c: TBGRAPixel; align: TAlignment); virtual; abstract; overload;
     procedure TextOut(x, y: integer; s: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract; overload;
     procedure TextOutAngle(x, y, orientation: integer; s: string; c: TBGRAPixel; align: TAlignment); virtual; abstract;
     procedure TextOutAngle(x, y, orientation: integer; s: string; texture: IBGRAScanner; align: TAlignment); virtual; abstract;
     procedure TextOut(x, y: integer; s: string; c: TBGRAPixel); virtual; overload;
     procedure TextOut(x, y: integer; s: string; c: TColor); virtual; overload;
     procedure TextOut(x, y: integer; s: string; texture: IBGRAScanner); virtual; overload;
     procedure TextRect(ARect: TRect; x, y: integer; s: string; style: TTextStyle; c: TBGRAPixel); virtual; abstract; overload;
     procedure TextRect(ARect: TRect; x, y: integer; s: string; style: TTextStyle; texture: IBGRAScanner); virtual; abstract; overload;
     procedure TextRect(ARect: TRect; s: string; halign: TAlignment; valign: TTextLayout; c: TBGRAPixel); virtual; overload;
     procedure TextRect(ARect: TRect; s: string; halign: TAlignment; valign: TTextLayout; texture: IBGRAScanner); virtual; overload;
     function TextSize(s: string): TSize; virtual; abstract;

     {Spline}
     function ComputeClosedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF; virtual; abstract;
     function ComputeOpenedSpline(const APoints: array of TPointF; AStyle: TSplineStyle): ArrayOfTPointF; virtual; abstract;
     function ComputeBezierCurve(const curve: TCubicBezierCurve): ArrayOfTPointF; virtual; abstract;
     function ComputeBezierCurve(const curve: TQuadraticBezierCurve): ArrayOfTPointF; virtual; abstract;
     function ComputeBezierSpline(const spline: array of TCubicBezierCurve): ArrayOfTPointF; virtual; abstract;
     function ComputeBezierSpline(const spline: array of TQuadraticBezierCurve): ArrayOfTPointF; virtual; abstract;

     function ComputeWidePolyline(const points: array of TPointF; w: single): ArrayOfTPointF; virtual; abstract;
     function ComputeWidePolyline(const points: array of TPointF; w: single; Closed: boolean): ArrayOfTPointF; virtual; abstract;
     function ComputeWidePolygon(const points: array of TPointF; w: single): ArrayOfTPointF; virtual; abstract;

     {Filling}
     procedure FillTransparent; virtual;
     procedure NoClip; virtual; abstract;
     procedure ApplyGlobalOpacity(alpha: byte); virtual; abstract;
     procedure Fill(c: TColor); virtual; overload;
     procedure Fill(c: TBGRAPixel); virtual; overload;
     procedure Fill(texture: IBGRAScanner); virtual; abstract; overload;
     procedure Fill(c: TBGRAPixel; start, Count: integer); virtual; abstract; overload;
     procedure DrawPixels(c: TBGRAPixel; start, Count: integer); virtual; abstract;
     procedure AlphaFill(alpha: byte); virtual; overload;
     procedure AlphaFill(alpha: byte; start, Count: integer); virtual; abstract; overload;
     procedure ReplaceColor(before, after: TColor); virtual; abstract; overload;
     procedure ReplaceColor(before, after: TBGRAPixel); virtual; abstract; overload;
     procedure ReplaceTransparent(after: TBGRAPixel); virtual; abstract; overload;
     procedure FloodFill(X, Y: integer; Color: TBGRAPixel;
       mode: TFloodfillMode; Tolerance: byte = 0); virtual;
     procedure ParallelFloodFill(X, Y: integer; Dest: TBGRACustomBitmap; Color: TBGRAPixel;
       mode: TFloodfillMode; Tolerance: byte = 0); virtual; abstract;
     procedure GradientFill(x, y, x2, y2: integer; c1, c2: TBGRAPixel;
       gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
       gammaColorCorrection: boolean = True; Sinus: Boolean=False); virtual; abstract;
     procedure GradientFill(x, y, x2, y2: integer; gradient: TBGRACustomGradient;
       gtype: TGradientType; o1, o2: TPointF; mode: TDrawMode;
       Sinus: Boolean=False); virtual; abstract;
     function CreateBrushTexture(ABrushStyle: TBrushStyle; APatternColor, ABackgroundColor: TBGRAPixel;
                AWidth: integer = 8; AHeight: integer = 8; APenWidth: single = 1): TBGRACustomBitmap; virtual; abstract;

     {Canvas drawing functions}
     procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
       AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); virtual; abstract;
     procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
       ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); virtual; abstract;
     procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); virtual; abstract;
     procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean = True); virtual; abstract;
     procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); virtual; abstract;
     procedure DrawPart(ARect: TRect; Canvas: TCanvas; x, y: integer; Opaque: boolean); virtual;
     function GetPart(ARect: TRect): TBGRACustomBitmap; virtual; abstract;
     procedure InvalidateBitmap; virtual; abstract;         //call if you modify with Scanline
     procedure LoadFromBitmapIfNeeded; virtual; abstract;   //call to ensure that bitmap data is up to date

     {BGRA bitmap functions}
     procedure PutImage(x, y: integer; Source: TBGRACustomBitmap; mode: TDrawMode); virtual; abstract;
     procedure PutImageSubpixel(x, y: single; Source: TBGRACustomBitmap);
     procedure PutImageAffine(Origin,HAxis,VAxis: TPointF; Source: TBGRACustomBitmap); virtual; abstract;
     procedure PutImageAngle(x,y: single; Source: TBGRACustomBitmap; angle: single; imageCenterX: single = 0; imageCenterY: single = 0); virtual; abstract;
     procedure BlendImage(x, y: integer; Source: TBGRACustomBitmap;
       operation: TBlendOperation); virtual; abstract;
     function Duplicate(DuplicateProperties: Boolean = False): TBGRACustomBitmap; virtual; abstract;
     function Equals(comp: TBGRACustomBitmap): boolean; virtual; abstract;
     function Equals(comp: TBGRAPixel): boolean; virtual; abstract;
     function Resample(newWidth, newHeight: integer;
       mode: TResampleMode = rmFineResample): TBGRACustomBitmap; virtual; abstract;
     procedure VerticalFlip; virtual; abstract;
     procedure HorizontalFlip; virtual; abstract;
     function RotateCW: TBGRACustomBitmap; virtual; abstract;
     function RotateCCW: TBGRACustomBitmap; virtual; abstract;
     procedure Negative; virtual; abstract;
     procedure LinearNegative; virtual; abstract;
     procedure SwapRedBlue; virtual; abstract;
     procedure GrayscaleToAlpha; virtual; abstract;
     procedure AlphaToGrayscale; virtual; abstract;
     procedure ApplyMask(mask: TBGRACustomBitmap); virtual; abstract;
     function GetImageBounds(Channel: TChannel = cAlpha): TRect; virtual; abstract;
     function MakeBitmapCopy(BackgroundColor: TColor): TBitmap; virtual; abstract;

     {Filters}
     function FilterSmartZoom3(Option: TMedianOption): TBGRACustomBitmap; virtual; abstract;
     function FilterMedian(Option: TMedianOption): TBGRACustomBitmap; virtual; abstract;
     function FilterSmooth: TBGRACustomBitmap; virtual; abstract;
     function FilterSharpen: TBGRACustomBitmap; virtual; abstract;
     function FilterContour: TBGRACustomBitmap; virtual; abstract;
     function FilterBlurRadial(radius: integer;
       blurType: TRadialBlurType): TBGRACustomBitmap; virtual; abstract;
     function FilterBlurMotion(distance: integer; angle: single;
       oriented: boolean): TBGRACustomBitmap; virtual; abstract;
     function FilterCustomBlur(mask: TBGRACustomBitmap): TBGRACustomBitmap; virtual; abstract;
     function FilterEmboss(angle: single): TBGRACustomBitmap; virtual; abstract;
     function FilterEmbossHighlight(FillSelection: boolean): TBGRACustomBitmap; virtual; abstract;
     function FilterGrayscale: TBGRACustomBitmap; virtual; abstract;
     function FilterNormalize(eachChannel: boolean = True): TBGRACustomBitmap; virtual; abstract;
     function FilterRotate(origin: TPointF; angle: single): TBGRACustomBitmap; virtual; abstract;
     function FilterSphere: TBGRACustomBitmap; virtual; abstract;
     function FilterTwirl(ACenter: TPoint; ARadius: Single; ATurn: Single=1; AExponent: Single=3): TBGRACustomBitmap; virtual; abstract;
     function FilterCylinder: TBGRACustomBitmap; virtual; abstract;
     function FilterPlane: TBGRACustomBitmap; virtual; abstract;

     property Data: PBGRAPixel Read GetDataPtr;
     property Width: integer Read GetWidth;
     property Height: integer Read GetHeight;
     property NbPixels: integer Read GetNbPixels;
     property Empty: boolean Read CheckEmpty;

     property ScanLine[y: integer]: PBGRAPixel Read GetScanLine;
     property RefCount: integer Read GetRefCount;
     property Bitmap: TBitmap Read GetBitmap; //don't forget to call InvalidateBitmap before if you changed something with Scanline
     property HasTransparentPixels: boolean Read GetHasTransparentPixels;
     property AverageColor: TColor Read GetAverageColor;
     property AveragePixel: TBGRAPixel Read GetAveragePixel;
     property LineOrder: TRawImageLineOrder Read GetLineOrder;
     property CanvasFP: TFPImageCanvas read GetCanvasFP;
     property CanvasDrawModeFP: TDrawMode read GetCanvasDrawModeFP write SetCanvasDrawModeFP;
     property Canvas: TCanvas Read GetCanvas;
     property CanvasOpacity: byte Read GetCanvasOpacity Write SetCanvasOpacity;
     property CanvasAlphaCorrection: boolean
       Read GetCanvasAlphaCorrection Write SetCanvasAlphaCorrection;

     property FontHeight: integer Read GetFontHeight Write SetFontHeight;
     property PenStyle: TPenStyle read GetPenStyle Write SetPenStyle;
     property CustomPenStyle: TBGRAPenStyle read GetCustomPenStyle write SetCustomPenStyle;
     property ClipRect: TRect read GetClipRect write SetClipRect;

     //interface
     function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
     function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
     function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};

     //IBGRAScanner
     procedure ScanMoveTo(X,Y: Integer); virtual; abstract;
     function ScanNextPixel: TBGRAPixel; virtual; abstract;
     function ScanAt(X,Y: Single): TBGRAPixel; virtual; abstract;
     procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); virtual;
     function IsScanPutPixelsDefined: boolean; virtual;
  end;

  { TBGRACustomScanner }

  TBGRACustomScanner = class(IBGRAScanner)
  private
    FCurX,FCurY: integer;
  public
    procedure ScanMoveTo(X,Y: Integer); virtual;
    function ScanNextPixel: TBGRAPixel; virtual;
    function ScanAt(X,Y: Single): TBGRAPixel; virtual; abstract;
    procedure ScanPutPixels(pdest: PBGRAPixel; count: integer; mode: TDrawMode); virtual;
    function IsScanPutPixelsDefined: boolean; virtual;
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
    function _Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
  end;

  TBGRACustomGradient = class
  public
    function GetColorAt(position: integer): TBGRAPixel; virtual; abstract;
    function GetAverageColor: TBGRAPixel; virtual; abstract;
  end;

{ Color functions }
function GetIntensity(c: TExpandedPixel): word; inline;
function SetIntensity(c: TExpandedPixel; intensity: word): TExpandedPixel;
function GetLightness(c: TExpandedPixel): word; inline;
function SetLightness(c: TExpandedPixel; lightness: word): TExpandedPixel;
function ApplyLightnessFast(color: TBGRAPixel; lightness: word): TBGRAPixel; inline;
function BGRAToHSLA(c: TBGRAPixel): THSLAPixel;
function HSLAToBGRA(c: THSLAPixel): TBGRAPixel;
function GammaExpansion(c: TBGRAPixel): TExpandedPixel; inline;
function GammaCompression(ec: TExpandedPixel): TBGRAPixel; inline;
function GammaCompression(red,green,blue,alpha: word): TBGRAPixel; inline;
function BGRAToGrayscale(c: TBGRAPixel): TBGRAPixel;
function MergeBGRA(c1, c2: TBGRAPixel): TBGRAPixel; overload;
function MergeBGRA(ec1, ec2: TExpandedPixel): TExpandedPixel; overload;
function BGRA(red, green, blue, alpha: byte): TBGRAPixel; overload; inline;
function BGRA(red, green, blue: byte): TBGRAPixel; overload; inline;
function ColorToBGRA(color: TColor): TBGRAPixel; overload;
function ColorToBGRA(color: TColor; opacity: byte): TBGRAPixel; overload;
function BGRAToFPColor(AValue: TBGRAPixel): TFPColor; inline;
function FPColorToBGRA(AValue: TFPColor): TBGRAPixel;
function BGRAToColor(c: TBGRAPixel): TColor;
operator = (const c1, c2: TBGRAPixel): boolean; inline;
function BGRADiff(c1, c2: TBGRAPixel): byte;
operator - (const c1, c2: TColorF): TColorF; inline;
operator + (const c1, c2: TColorF): TColorF; inline;
operator * (const c1: TColorF; factor: single): TColorF; inline;
function ColorF(red,green,blue,alpha: single): TColorF;
function BGRAToStr(c: TBGRAPixel): string;
function StrToBGRA(str: string): TBGRAPixel;

{ Gamma conversion arrays. Should be used as readonly }
var
  // TBGRAPixel -> TExpandedPixel
  GammaExpansionTab:   packed array[0..255] of word;
  
  // TExpandedPixel -> TBGRAPixel
  GammaCompressionTab: packed array[0..65535] of byte;

{ Point functions }
function PointF(x, y: single): TPointF;
function PointsF(const pts: array of TPointF): ArrayOfTPointF;
operator = (const pt1, pt2: TPointF): boolean; inline;
operator - (const pt1, pt2: TPointF): TPointF; inline;
operator - (const pt2: TPointF): TPointF; inline;
operator + (const pt1, pt2: TPointF): TPointF; inline;
operator * (const pt1, pt2: TPointF): single; inline; //scalar product
operator * (const pt1: TPointF; factor: single): TPointF; inline;
operator * (factor: single; const pt1: TPointF): TPointF; inline;
function PtInRect(pt: TPoint; r: TRect): boolean;

{ Line and polygon functions }
type
    TLineDef = record
       origin, dir: TPointF;
    end;

function IntersectLine(line1, line2: TLineDef): TPointF;
function IntersectLine(line1, line2: TLineDef; out parallel: boolean): TPointF;
function IsConvex(const pts: array of TPointF; IgnoreAlign: boolean = true): boolean;
function DoesQuadIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
function DoesSegmentIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;

{ Cyclic functions }
function PositiveMod(value, cycle: integer): integer; inline;

{ Sin65536 and Cos65536 are fast routines to compute sine and cosine as integer values.
  They use a table to store already computed values. The return value is an integer
  ranging from 0 to 65536, so the mean value is 32768 and the half amplitude is
  32768 instead of 1. The input has a period of 65536, so you can supply any integer
  without applying a modulo. }
procedure PrecalcSin65536; // compute all values now
function Sin65536(value: word): integer; inline;
function Cos65536(value: word): integer; inline;

implementation

uses Math, SysUtils;

function StrToGradientType(str: string): TGradientType;
var gt: TGradientType;
begin
  result := gtLinear;
  str := LowerCase(str);
  for gt := low(TGradientType) to high(TGradientType) do
    if str = LowerCase(GradientTypeStr[gt]) then
    begin
      result := gt;
      exit;
    end;
end;

{ Make a pen style. Need an even number of values. See TBGRAPenStyle }
function BGRAPenStyle(dash1, space1: single; dash2: single; space2: single;
  dash3: single; space3: single; dash4: single; space4: single): TBGRAPenStyle;
var
  i: Integer;
begin
  if dash4 <> 0 then
  begin
    setlength(result,8);
    result[6] := dash4;
    result[7] := space4;
    result[4] := dash3;
    result[5] := space3;
    result[2] := dash2;
    result[3] := space2;
  end else
  if dash3 <> 0 then
  begin
    setlength(result,6);
    result[4] := dash3;
    result[5] := space3;
    result[2] := dash2;
    result[3] := space2;
  end else
  if dash2 <> 0 then
  begin
    setlength(result,4);
    result[2] := dash2;
    result[3] := space2;
  end else
  begin
    setlength(result,2);
  end;
  result[0] := dash1;
  result[1] := space1;
  for i := 0 to high(result) do
    if result[i]=0 then
      raise exception.Create('Zero is not a valid value');
end;

{ Bézier curves definitions. See : http://en.wikipedia.org/wiki/B%C3%A9zier_curve }

function ConcatPointsF(const APolylines: array of ArrayOfTPointF
  ): ArrayOfTPointF;
var
  i,pos,count:integer;
  j: Integer;
begin
  count := 0;
  for i := 0 to high(APolylines) do
    inc(count,length(APolylines[i]));
  setlength(result,count);
  pos := 0;
  for i := 0 to high(APolylines) do
    for j := 0 to high(APolylines[i]) do
    begin
      result[pos] := APolylines[i][j];
      inc(pos);
    end;
end;

// Define a Bézier curve with two control points.
function BezierCurve(origin, control1, control2, destination: TPointF): TCubicBezierCurve;
begin
  result.p1 := origin;
  result.c1 := control1;
  result.c2 := control2;
  result.p2 := destination;
end;

// Define a Bézier curve with one control point.
function BezierCurve(origin, control, destination: TPointF
  ): TQuadraticBezierCurve;
begin
  result.p1 := origin;
  result.c := control;
  result.p2 := destination;
end;

{ Check if a PointF structure is empty or should be treated as a list separator }
function isEmptyPointF(pt: TPointF): boolean;
begin
  Result := (pt.x = EmptySingle) and (pt.y = EmptySingle);
end;

{ TBGRACustomBitmap }

{ These declaration make sure that these methods are virtual }
procedure TBGRACustomBitmap.LoadFromFile(const filename: string);
begin
  inherited LoadFromFile(filename);
end;

procedure TBGRACustomBitmap.SaveToFile(const filename: string);
begin
  inherited SaveToFile(filename);
end;

procedure TBGRACustomBitmap.SaveToFile(const filename: string;
  Handler: TFPCustomImageWriter);
begin
  inherited SaveToFile(filename, Handler);
end;

{ LoadFromStream uses TFPCustomImage routine, which uses
  Colors property to access pixels. That's why the
  FP drawing mode is temporarily changed to load
  bitmaps properly }
procedure TBGRACustomBitmap.LoadFromStream(Str: TStream);
var
  OldDrawMode: TDrawMode;
begin
  OldDrawMode := CanvasDrawModeFP;
  CanvasDrawModeFP := dmSet;
  try
    inherited LoadFromStream(Str);
  finally
    CanvasDrawModeFP := OldDrawMode;
  end;
end;

{ See above }
procedure TBGRACustomBitmap.LoadFromStream(Str: TStream;
  Handler: TFPCustomImageReader);
var
  OldDrawMode: TDrawMode;
begin
  OldDrawMode := CanvasDrawModeFP;
  CanvasDrawModeFP := dmSet;
  try
    inherited LoadFromStream(Str, Handler);
  finally
    CanvasDrawModeFP := OldDrawMode;
  end;
end;

{ Look for a pixel considering the bitmap is repeated in both directions }
function TBGRACustomBitmap.GetPixelCycle(x, y: integer): TBGRAPixel;
begin
  if (Width = 0) or (Height = 0) then
    Result := BGRAPixelTransparent
  else
    Result := (Scanline[PositiveMod(y,Height)] + PositiveMod(x,Width))^;
end;

{ Pixel polylines are constructed by concatenation }
procedure TBGRACustomBitmap.DrawPolyLineAntialias(const points: array of TPoint;
  c: TBGRAPixel; DrawLastPixel: boolean);
var i: integer;
begin
   if length(points) = 1 then
   begin
     if DrawLastPixel then DrawPixel(points[0].x,points[0].y,c);
   end
   else
     for i := 0 to high(points)-1 do
       DrawLineAntialias(points[i].x,points[i].Y,points[i+1].x,points[i+1].y,c,DrawLastPixel and (i=high(points)-1));
end;

procedure TBGRACustomBitmap.DrawPolyLineAntialias(const points: array of TPoint; c1,
  c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean);
var i: integer;
begin
   if length(points) = 1 then
   begin
     if DrawLastPixel then DrawPixel(points[0].x,points[0].y,c1);
   end
   else
     for i := 0 to high(points)-1 do
       DrawLineAntialias(points[i].x,points[i].Y,points[i+1].x,points[i+1].y,c1,c2,dashLen,DrawLastPixel and (i=high(points)-1));
end;

{ Following functions are defined for convenience }
procedure TBGRACustomBitmap.Rectangle(x, y, x2, y2: integer; c: TColor);
begin
  Rectangle(x, y, x2, y2, ColorToBGRA(c), dmSet);
end;

procedure TBGRACustomBitmap.Rectangle(r: TRect; c: TBGRAPixel; mode: TDrawMode
  );
begin
  Rectangle(r.left, r.top, r.right, r.bottom, c, mode);
end;

procedure TBGRACustomBitmap.Rectangle(r: TRect; BorderColor,
  FillColor: TBGRAPixel; mode: TDrawMode);
begin
  Rectangle(r.left, r.top, r.right, r.bottom, BorderColor, FillColor, mode);
end;

procedure TBGRACustomBitmap.Rectangle(r: TRect; c: TColor);
begin
  Rectangle(r.left, r.top, r.right, r.bottom, c);
end;

procedure TBGRACustomBitmap.RectangleAntialias(x, y, x2, y2: single;
  c: TBGRAPixel; w: single);
begin
  RectangleAntialias(x, y, x2, y2, c, w, BGRAPixelTransparent);
end;

procedure TBGRACustomBitmap.FillRect(r: TRect; c: TColor);
begin
  FillRect(r.Left, r.top, r.right, r.bottom, c);
end;

procedure TBGRACustomBitmap.FillRect(r: TRect; c: TBGRAPixel; mode: TDrawMode);
begin
  FillRect(r.Left, r.top, r.right, r.bottom, c, mode);
end;

procedure TBGRACustomBitmap.FillRect(x, y, x2, y2: integer; c: TColor);
begin
  FillRect(x, y, x2, y2, ColorToBGRA(c), dmSet);
end;

procedure TBGRACustomBitmap.TextOut(x, y: integer; s: string; c: TBGRAPixel);
begin
  TextOut(x, y, s, c, taLeftJustify);
end;

procedure TBGRACustomBitmap.TextOut(x, y: integer; s: string; c: TColor);
begin
  TextOut(x, y, s, ColorToBGRA(c));
end;

procedure TBGRACustomBitmap.TextOut(x, y: integer; s: string;
  texture: IBGRAScanner);
begin
  TextOut(x, y, s, texture, taLeftJustify);
end;

procedure TBGRACustomBitmap.TextRect(ARect: TRect; s: string;
  halign: TAlignment; valign: TTextLayout; c: TBGRAPixel);
var
  style: TTextStyle;
begin
  {$hints off}
  FillChar(style,sizeof(style),0);
  {$hints on}
  style.Alignment := halign;
  style.Layout := valign;
  style.Wordbreak := true;
  style.ShowPrefix := false;
  style.Clipping := false;
  TextRect(ARect,ARect.Left,ARect.Top,s,style,c);
end;

procedure TBGRACustomBitmap.TextRect(ARect: TRect; s: string;
  halign: TAlignment; valign: TTextLayout; texture: IBGRAScanner);
var
  style: TTextStyle;
begin
  {$hints off}
  FillChar(style,sizeof(style),0);
  {$hints on}
  style.Alignment := halign;
  style.Layout := valign;
  style.Wordbreak := true;
  style.ShowPrefix := false;
  style.Clipping := false;
  TextRect(ARect,ARect.Left,ARect.Top,s,style,texture);
end;

procedure TBGRACustomBitmap.FillTransparent;
begin
  Fill(BGRAPixelTransparent);
end;

procedure TBGRACustomBitmap.Fill(c: TColor);
begin
  Fill(ColorToBGRA(c));
end;

procedure TBGRACustomBitmap.Fill(c: TBGRAPixel);
begin
  Fill(c, 0, NbPixels);
end;

procedure TBGRACustomBitmap.AlphaFill(alpha: byte);
begin
  AlphaFill(alpha, 0, NbPixels);
end;

procedure TBGRACustomBitmap.FloodFill(X, Y: integer; Color: TBGRAPixel;
  mode: TFloodfillMode; Tolerance: byte);
begin
  ParallelFloodFill(X,Y,Self,Color,mode,Tolerance);
end;

procedure TBGRACustomBitmap.DrawPart(ARect: TRect; Canvas: TCanvas; x,
  y: integer; Opaque: boolean);
var
  partial: TBGRACustomBitmap;
begin
  partial := GetPart(ARect);
  if partial <> nil then
  begin
    partial.Draw(Canvas, x, y, Opaque);
    partial.Free;
  end;
end;

procedure TBGRACustomBitmap.PutImageSubpixel(x, y: single; Source: TBGRACustomBitmap);
begin
  PutImageAngle(x,y,source,0);
end;

{ Interface gateway }
function TBGRACustomBitmap.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

{ There is no automatic reference counting, but it is compulsory to define these functions }
function TBGRACustomBitmap._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TBGRACustomBitmap._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

{$hints off}
procedure TBGRACustomBitmap.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
begin
  //do nothing
end;
{$hints on}

function TBGRACustomBitmap.IsScanPutPixelsDefined: boolean;
begin
  result := False;
end;

{********************** End of TBGRACustomBitmap **************************}

{ TBGRACustomScanner }
{ The abstract class record the position so that a derived class
  need only to redefine ScanAt }

procedure TBGRACustomScanner.ScanMoveTo(X, Y: Integer);
begin
  FCurX := X;
  FCurY := Y;
end;

{ Call ScanAt to determine pixel value }
function TBGRACustomScanner.ScanNextPixel: TBGRAPixel;
begin
  result := ScanAt(FCurX,FCurY);
  Inc(FCurX);
end;

{$hints off}
procedure TBGRACustomScanner.ScanPutPixels(pdest: PBGRAPixel; count: integer;
  mode: TDrawMode);
begin
  //do nothing
end;
{$hints on}

function TBGRACustomScanner.IsScanPutPixelsDefined: boolean;
begin
  result := false;
end;

{ Interface gateway }
function TBGRACustomScanner.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := longint(E_NOINTERFACE);
end;

{ There is no automatic reference counting, but it is compulsory to define these functions }
function TBGRACustomScanner._AddRef: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

function TBGRACustomScanner._Release: Integer; {$IF (not defined(WINDOWS)) AND (FPC_FULLVERSION>=20501)}cdecl{$ELSE}stdcall{$IFEND};
begin
  result := 0;
end;

{********************** End of TBGRACustomScanner **************************}

{ The gamma correction is approximated here by a power function }
const
  GammaExpFactor   = 1.7; //exponent
  redWeightShl10   = 306; // = 0.299
  greenWeightShl10 = 601; // = 0.587
  blueWeightShl10  = 117; // = 0.114

var
  GammaLinearFactor: single;

procedure InitGamma;
var
  i: integer;
begin
  //the linear factor is used to normalize expanded values in the range 0..65535
  GammaLinearFactor := 65535 / power(255, GammaExpFactor);
  for i := 0 to 255 do
    GammaExpansionTab[i] := round(power(i, GammaExpFactor) * GammaLinearFactor);

  for i := 0 to 65535 do
    GammaCompressionTab[i] := round(power(i / GammaLinearFactor, 1 / GammaExpFactor));

  GammaExpansionTab[1]   := 1; //to avoid information loss
  GammaCompressionTab[1] := 1;
end;

{************************** Color functions **************************}

{ The intensity is defined here as the maximum value of any color component }
function GetIntensity(c: TExpandedPixel): word; inline;
begin
  Result := c.red;
  if c.green > Result then
    Result := c.green;
  if c.blue > Result then
    Result := c.blue;
end;

function SetIntensity(c: TExpandedPixel; intensity: word): TExpandedPixel;
var
  curIntensity: word;
begin
  curIntensity := GetIntensity(c);
  if curIntensity = 0 then //suppose it's gray if there is no color information
    Result := c
  else
  begin
    //linear interpolation to reached wanted intensity
    Result.red   := (c.red * intensity + (curIntensity shr 1)) div curIntensity;
    Result.green := (c.green * intensity + (curIntensity shr 1)) div curIntensity;
    Result.blue  := (c.blue * intensity + (curIntensity shr 1)) div curIntensity;
    Result.alpha := c.alpha;
  end;
end;

{ The lightness here is defined as the subjective sensation of luminosity, where
  blue is the darkest component and green the lightest }
function GetLightness(c: TExpandedPixel): word; inline;
begin
  Result := (c.red * redWeightShl10 + c.green * greenWeightShl10 +
    c.blue * blueWeightShl10 + 512) shr 10;
end;

function SetLightness(c: TExpandedPixel; lightness: word): TExpandedPixel;
var
  curLightness: word;
  AddedWhiteness, maxBeforeWhite: word;
  clip: boolean;
begin
  curLightness := GetLightness(c);
  if lightness = curLightness then
  begin //no change
    Result := c;
    exit;
  end;
  if lightness = 65535 then //set to white
  begin
    Result.red   := 65535;
    Result.green := 65535;
    Result.blue  := 65535;
    Result.alpha := c.alpha;
    exit;
  end;
  if lightness = 0 then  //set to black
  begin
    Result.red   := 0;
    Result.green := 0;
    Result.blue  := 0;
    Result.alpha := c.alpha;
    exit;
  end;
  if curLightness = 0 then  //set from black
  begin
    Result.red   := lightness;
    Result.green := lightness;
    Result.blue  := lightness;
    Result.alpha := c.alpha;
    exit;
  end;
  if lightness < curLightness then //darker is easy
  begin
    Result := SetIntensity(c, (GetIntensity(c) * lightness + (curLightness shr 1)) div
      curLightness);
    exit;
  end;
  //lighter and grayer
  Result := c;
  AddedWhiteness := lightness - curLightness;
  maxBeforeWhite := 65535 - AddedWhiteness;
  clip   := False;
  if Result.red <= maxBeforeWhite then
    Inc(Result.red, AddedWhiteness)
  else
  begin
    Result.red := 65535;
    clip := True;
  end;
  if Result.green <= maxBeforeWhite then
    Inc(Result.green, AddedWhiteness)
  else
  begin
    Result.green := 65535;
    clip := True;
  end;
  if Result.blue <= maxBeforeWhite then
    Inc(Result.blue, AddedWhiteness)
  else
  begin
    Result.blue := 65535;
    clip := True;
  end;

  if clip then //light and whiter
  begin
    curLightness   := GetLightness(Result);
    addedWhiteness := lightness - curLightness;
    maxBeforeWhite := 65535 - curlightness;
    Result.red     := Result.red + addedWhiteness * (65535 - Result.red) div
      maxBeforeWhite;
    Result.green   := Result.green + addedWhiteness * (65535 - Result.green) div
      maxBeforeWhite;
    Result.blue    := Result.blue + addedWhiteness * (65535 - Result.blue) div
      maxBeforeWhite;
  end;
end;

function ApplyLightnessFast(color: TBGRAPixel; lightness: word): TBGRAPixel; inline;
var lightness256: byte;
begin
  if lightness <= 32768 then
  begin
    if lightness = 32768 then
      result := color else
    begin
      lightness256 := GammaCompressionTab[lightness shl 1];
      result := BGRA(color.red * lightness256 shr 8, color.green*lightness256 shr 8,
                     color.blue * lightness256 shr 8, color.alpha);
    end;
  end else
  begin
    if lightness = 65535 then
      result := BGRAWhite else
    begin
      lightness256 := GammaCompressionTab[(lightness-32767) shl 1];
      result := BGRA(color.red + (255-color.red)*lightness256 shr 8,
                     color.green + (255-color.green)*lightness256 shr 8,
                     color.blue + (255-color.blue)*lightness256 shr 8,
                     color.alpha);
    end;
  end;
end;

{ Conversion from RGB value to HSL colorspace. See : http://en.wikipedia.org/wiki/HSL_color_space }
function BGRAToHSLA(c: TBGRAPixel): THSLAPixel;
const
  deg60  = 8192;
  deg120 = deg60 * 2;
  deg240 = deg60 * 4;
  deg360 = deg60 * 6;
var
  ec: TExpandedPixel;
  min, max, minMax: integer;
  twiceLightness: integer;
  r,g,b: integer;
begin
  ec  := GammaExpansion(c);
  r := ec.red;
  g := ec.green;
  b := ec.blue;
  min := r;
  max := r;
  if g > max then
    max := g
  else
  if g < min then
    min := g;
  if b > max then
    max := b
  else
  if b < min then
    min  := b;
  minMax := max - min;

  if minMax = 0 then
    Result.hue := 0
  else
  if max = r then
    Result.hue := (((g - b) * deg60) div
      minMax + deg360) mod deg360
  else
  if max = g then
    Result.hue := ((b - r) * deg60) div minMax + deg120
  else
    {max = b} Result.hue :=
      ((r - g) * deg60) div minMax + deg240;
  twiceLightness := max + min;
  if min = max then
    Result.saturation := 0
  else
  {$hints off}
  if twiceLightness < 65536 then
    Result.saturation := (int64(minMax) shl 16) div (twiceLightness + 1)
  else
    Result.saturation := (int64(minMax) shl 16) div (131072 - twiceLightness);
  {$hints on}
  Result.lightness := twiceLightness shr 1;
  Result.alpha := ec.alpha;
  Result.hue   := (Result.hue shl 16) div deg360;
end;

{ Conversion from HSL colorspace to RGB. See : http://en.wikipedia.org/wiki/HSL_color_space }
function HSLAToBGRA(c: THSLAPixel): TBGRAPixel;
const
  deg30  = 4096;
  deg60  = 8192;
  deg120 = deg60 * 2;
  deg180 = deg60 * 3;
  deg240 = deg60 * 4;
  deg360 = deg60 * 6;

  function ComputeColor(p, q: integer; h: integer): word; inline;
  begin
    if h > deg360 then
      Dec(h, deg360);
    if h < deg60 then
      Result := p + ((q - p) * h + deg30) div deg60
    else
    if h < deg180 then
      Result := q
    else
    if h < deg240 then
      Result := p + ((q - p) * (deg240 - h) + deg30) div deg60
    else
      Result := p;
  end;

var
  q, p: integer;
  ec:   TExpandedPixel;
begin
  c.hue := c.hue * deg360 shr 16;
  if c.saturation = 0 then  //gray
  begin
    ec.red   := c.lightness;
    ec.green := c.lightness;
    ec.blue  := c.lightness;
    ec.alpha := c.alpha;
    Result   := GammaCompression(ec);
    exit;
  end;
  {$hints off}
  if c.lightness < 32768 then
    q := (c.lightness shr 1) * ((65535 + c.saturation) shr 1) shr 14
  else
    q := c.lightness + c.saturation - ((c.lightness shr 1) *
      (c.saturation shr 1) shr 14);
  {$hints on}
  if q > 65535 then
    q := 65535;
  p   := c.lightness * 2 - q;
  if p > 65535 then
    p      := 65535;
  ec.red   := ComputeColor(p, q, c.hue + deg120);
  ec.green := ComputeColor(p, q, c.hue);
  ec.blue  := ComputeColor(p, q, c.hue + deg240);
  ec.alpha := c.alpha;
  Result   := GammaCompression(ec);
end;

{ Apply gamma correction using conversion tables }
function GammaExpansion(c: TBGRAPixel): TExpandedPixel;
begin
  Result.red   := GammaExpansionTab[c.red];
  Result.green := GammaExpansionTab[c.green];
  Result.blue  := GammaExpansionTab[c.blue];
  Result.alpha := c.alpha shl 8 + c.alpha;
end;

function GammaCompression(ec: TExpandedPixel): TBGRAPixel;
begin
  Result.red   := GammaCompressionTab[ec.red];
  Result.green := GammaCompressionTab[ec.green];
  Result.blue  := GammaCompressionTab[ec.blue];
  Result.alpha := ec.alpha shr 8;
end;

function GammaCompression(red, green, blue, alpha: word): TBGRAPixel;
begin
  Result.red   := GammaCompressionTab[red];
  Result.green := GammaCompressionTab[green];
  Result.blue  := GammaCompressionTab[blue];
  Result.alpha := alpha shr 8;
end;

// Conversion to grayscale by taking into account
// different color weights
function BGRAToGrayscale(c: TBGRAPixel): TBGRAPixel;
var
  ec:    TExpandedPixel;
  gray:  word;
  cgray: byte;
begin
  //gamma expansion
  ec    := GammaExpansion(c);
  //gray composition
  gray  := (ec.red * redWeightShl10 + ec.green * greenWeightShl10 +
    ec.blue * blueWeightShl10 + 512) shr 10;
  //gamma compression
  cgray := GammaCompressionTab[gray];
  Result.red := cgray;
  Result.green := cgray;
  Result.blue := cgray;
  Result.alpha := c.alpha;
end;

{ Merge linearly two colors of same importance }
function MergeBGRA(c1, c2: TBGRAPixel): TBGRAPixel;
var c12: cardinal;
begin
  if (c1.alpha = 0) then
    Result := c2
  else
  if (c2.alpha = 0) then
    Result := c1
  else
  begin
    c12 := c1.alpha + c2.alpha;
    Result.red   := (c1.red * c1.alpha + c2.red * c2.alpha + c12 shr 1) div c12;
    Result.green := (c1.green * c1.alpha + c2.green * c2.alpha + c12 shr 1) div c12;
    Result.blue  := (c1.blue * c1.alpha + c2.blue * c2.alpha + c12 shr 1) div c12;
    Result.alpha := (c12 + 1) shr 1;
  end;
end;

{ Merge two colors of same importance }
function MergeBGRA(ec1, ec2: TExpandedPixel): TExpandedPixel;
var c12: cardinal;
begin
  if (ec1.alpha = 0) then
    Result := ec2
  else
  if (ec2.alpha = 0) then
    Result := ec1
  else
  begin
    c12 := ec1.alpha + ec2.alpha;
    Result.red   := (int64(ec1.red) * ec1.alpha + int64(ec2.red) * ec2.alpha + c12 shr 1) div c12;
    Result.green := (int64(ec1.green) * ec1.alpha + int64(ec2.green) * ec2.alpha + c12 shr 1) div c12;
    Result.blue  := (int64(ec1.blue) * ec1.alpha + int64(ec2.blue) * ec2.alpha + c12 shr 1) div c12;
    Result.alpha := (c12 + 1) shr 1;
  end;
end;

function BGRA(red, green, blue, alpha: byte): TBGRAPixel;
begin
  Result.red   := red;
  Result.green := green;
  Result.blue  := blue;
  Result.alpha := alpha;
end;

function BGRA(red, green, blue: byte): TBGRAPixel; overload;
begin
  Result.red   := red;
  Result.green := green;
  Result.blue  := blue;
  Result.alpha := 255;
end;

{ Convert a TColor value to a TBGRAPixel value. Note that
  you need to call ColorToRGB first if you use a system
  color identifier like clWindow. }
{$PUSH}{$R-}
function ColorToBGRA(color: TColor): TBGRAPixel; overload;
begin
  Result.red   := color;
  Result.green := color shr 8;
  Result.blue  := color shr 16;
  Result.alpha := 255;
end;

function ColorToBGRA(color: TColor; opacity: byte): TBGRAPixel; overload;
begin
  Result.red   := color;
  Result.green := color shr 8;
  Result.blue  := color shr 16;
  Result.alpha := opacity;
end;
{$POP}

{ Conversion from TFPColor to TBGRAPixel assuming TFPColor
  is already gamma compressed }
function FPColorToBGRA(AValue: TFPColor): TBGRAPixel;
begin
  with AValue do
    Result := BGRA(red shr 8, green shr 8, blue shr 8, alpha shr 8);
end;

function BGRAToFPColor(AValue: TBGRAPixel): TFPColor; inline;
begin
  result.red := AValue.red shl 8 + AValue.red;
  result.green := AValue.green shl 8 + AValue.green;
  result.blue := AValue.blue shl 8 + AValue.blue;
  result.alpha := AValue.alpha shl 8 + AValue.alpha;
end;

function BGRAToColor(c: TBGRAPixel): TColor;
begin
  Result := c.red + (c.green shl 8) + (c.blue shl 16);
end;

operator = (const c1, c2: TBGRAPixel): boolean;
begin
  if (c1.alpha = 0) and (c2.alpha = 0) then
    Result := True
  else
    Result := (c1.alpha = c2.alpha) and (c1.red = c2.red) and
      (c1.green = c2.green) and (c1.blue = c2.blue);
end;

function BGRADiff(c1, c2: TBGRAPixel): byte;
var
  CompRedAlpha1, CompGreenAlpha1, CompBlueAlpha1, CompRedAlpha2,
  CompGreenAlpha2, CompBlueAlpha2: integer;
  DiffAlpha: byte;
begin
    {$hints off}
  CompRedAlpha1 := c1.red * c1.alpha;
  CompGreenAlpha1 := c1.green * c1.alpha;
  CompBlueAlpha1 := c1.blue * c1.alpha;
  CompRedAlpha2 := c2.red * c2.alpha;
  CompGreenAlpha2 := c2.green * c2.alpha;
  CompBlueAlpha2 := c2.blue * c2.alpha;
    {$hints on}
  Result    := (Abs(CompRedAlpha2 - CompRedAlpha1) +
    Abs(CompBlueAlpha2 - CompBlueAlpha1) + Abs(CompGreenAlpha2 - CompGreenAlpha1)) div
    (3 * 255);
  DiffAlpha := Abs(c2.Alpha - c1.Alpha) * 3 shr 2;
  if DiffAlpha > Result then
    Result := DiffAlpha;
end;

operator-(const c1, c2: TColorF): TColorF;
begin
  result[1] := c1[1]-c2[1];
  result[2] := c1[2]-c2[2];
  result[3] := c1[3]-c2[3];
  result[4] := c1[4]-c2[4];
end;

operator+(const c1, c2: TColorF): TColorF;
begin
  result[1] := c1[1]+c2[1];
  result[2] := c1[2]+c2[2];
  result[3] := c1[3]+c2[3];
  result[4] := c1[4]+c2[4];
end;

operator*(const c1: TColorF; factor: single): TColorF;
begin
  result[1] := c1[1]*factor;
  result[2] := c1[2]*factor;
  result[3] := c1[3]*factor;
  result[4] := c1[4]*factor;
end;

function ColorF(red, green, blue, alpha: single): TColorF;
begin
  result[1] := red;
  result[2] := green;
  result[3] := blue;
  result[4] := alpha;
end;

{ Write a color in hexadecimal format RRGGBBAA }
function BGRAToStr(c: TBGRAPixel): string;
begin
  result := IntToHex(c.red,2)+IntToHex(c.green,2)+IntToHex(c.Blue,2)+IntToHex(c.Alpha,2);
end;

{ Read a color in hexadecimal format RRGGBB(AA) or RGB(A) }
function StrToBGRA(str: string): TBGRAPixel;
var errPos: integer;
begin
  if length(str)=6 then str += 'FF';
  if length(str)=3 then str += 'F';
  if length(str)=8 then
  begin
    val('$'+copy(str,1,2),result.red,errPos);
    val('$'+copy(str,3,2),result.green,errPos);
    val('$'+copy(str,5,2),result.blue,errPos);
    val('$'+copy(str,7,2),result.alpha,errPos);
  end else
  if length(str)=4 then
  begin
    val('$'+copy(str,1,1),result.red,errPos);
    val('$'+copy(str,2,1),result.green,errPos);
    val('$'+copy(str,3,1),result.blue,errPos);
    val('$'+copy(str,4,1),result.alpha,errPos);
    result.red *= $11;
    result.green *= $11;
    result.blue *= $11;
    result.alpha *= $11;
  end else
    result := BGRAPixelTransparent;
end;

{********************** Point functions **************************}

function PointF(x, y: single): TPointF;
begin
  Result.x := x;
  Result.y := y;
end;

function PointsF(const pts: array of TPointF): ArrayOfTPointF;
var
  i: Integer;
begin
  setlength(result, length(pts));
  for i := 0 to high(pts) do result[i] := pts[i];
end;

operator =(const pt1, pt2: TPointF): boolean;
begin
  result := (pt1.x = pt2.x) and (pt1.y = pt2.y);
end;

operator-(const pt1, pt2: TPointF): TPointF;
begin
  result.x := pt1.x-pt2.x;
  result.y := pt1.y-pt2.y;
end;

operator-(const pt2: TPointF): TPointF;
begin
  result.x := -pt2.x;
  result.y := -pt2.y;
end;

operator+(const pt1, pt2: TPointF): TPointF;
begin
  result.x := pt1.x+pt2.x;
  result.y := pt1.y+pt2.y;
end;

operator*(const pt1, pt2: TPointF): single;
begin
  result := pt1.x*pt2.x + pt1.y*pt2.y;
end;

operator*(const pt1: TPointF; factor: single): TPointF;
begin
  result.x := pt1.x*factor;
  result.y := pt1.y*factor;
end;

operator*(factor: single; const pt1: TPointF): TPointF;
begin
  result.x := pt1.x*factor;
  result.y := pt1.y*factor;
end;

function PtInRect(pt: TPoint; r: TRect): boolean;
var
  temp: integer;
begin
  if r.right < r.left then
  begin
    temp    := r.left;
    r.left  := r.right;
    r.Right := temp;
  end;
  if r.bottom < r.top then
  begin
    temp     := r.top;
    r.top    := r.bottom;
    r.bottom := temp;
  end;
  Result := (pt.X >= r.left) and (pt.Y >= r.top) and (pt.X < r.right) and
    (pt.y < r.bottom);
end;

function IntersectLine(line1, line2: TLineDef): TPointF;
var parallel: boolean;
begin
  result := IntersectLine(line1,line2,parallel);
end;

function IntersectLine(line1, line2: TLineDef; out parallel: boolean): TPointF;
var divFactor: double;
begin
  parallel := false;
  //if lines are parallel
  if ((line1.dir.x = line2.dir.x) and (line1.dir.y = line2.dir.y)) or
     ((line1.dir.y=0) and (line2.dir.y=0)) then
  begin
       parallel := true;
       //return the center of the segment between line origins
       result.x := (line1.origin.x+line2.origin.x)/2;
       result.y := (line1.origin.y+line2.origin.y)/2;
  end else
  if line1.dir.y=0 then //line1 is horizontal
  begin
       result.y := line1.origin.y;
       result.x := line2.origin.x + (result.y - line2.origin.y)
               /line2.dir.y*line2.dir.x;
  end else
  if line2.dir.y=0 then //line2 is horizontal
  begin
       result.y := line2.origin.y;
       result.x := line1.origin.x + (result.y - line1.origin.y)
               /line1.dir.y*line1.dir.x;
  end else
  begin
       divFactor := line1.dir.x/line1.dir.y - line2.dir.x/line2.dir.y;
       if abs(divFactor) < 1e-6 then //almost parallel
       begin
            parallel := true;
            //return the center of the segment between line origins
            result.x := (line1.origin.x+line2.origin.x)/2;
            result.y := (line1.origin.y+line2.origin.y)/2;
       end else
       begin
         result.y := (line2.origin.x - line1.origin.x +
                  line1.origin.y*line1.dir.x/line1.dir.y -
                  line2.origin.y*line2.dir.x/line2.dir.y)
                  / divFactor;
         result.x := line1.origin.x + (result.y - line1.origin.y)
                 /line1.dir.y*line1.dir.x;
       end;
  end;
end;

{ Check if a polygon is convex, i.e. it always turns in the same direction }
function IsConvex(const pts: array of TPointF; IgnoreAlign: boolean = true): boolean;
var
  positive,negative,zero: boolean;
  product: single;
  i: Integer;
begin
  positive := false;
  negative := false;
  zero := false;
  for i := 0 to high(pts) do
  begin
    product := (pts[(i+1) mod length(pts)].x-pts[i].x)*(pts[(i+2) mod length(pts)].y-pts[i].y) -
               (pts[(i+1) mod length(pts)].y-pts[i].y)*(pts[(i+2) mod length(pts)].x-pts[i].x);
    if product > 0 then
    begin
      if negative then
      begin
        result := false;
        exit;
      end;
      positive := true;
    end else
    if product < 0 then
    begin
      if positive then
      begin
        result := false;
        exit;
      end;
      negative := true;
    end else
      zero := true;
  end;
  if not IgnoreAlign and zero then
    result := false
  else
    result := true;
end;

{ Check if two segments intersect }
function DoesSegmentIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
var
  seg1: TLineDef;
  seg1len: single;
  seg2: TLineDef;
  seg2len: single;
  inter: TPointF;
  pos1,pos2: single;
  para: boolean;

begin
  { Determine line definitions }
  seg1.origin := pt1;
  seg1.dir := pt2-pt1;
  seg1len := sqrt(sqr(seg1.dir.X)+sqr(seg1.dir.Y));
  if seg1len = 0 then
  begin
    result := false;
    exit;
  end;
  seg1.dir *= 1/seg1len;

  seg2.origin := pt3;
  seg2.dir := pt4-pt3;
  seg2len := sqrt(sqr(seg2.dir.X)+sqr(seg2.dir.Y));
  if seg2len = 0 then
  begin
    result := false;
    exit;
  end;
  seg2.dir *= 1/seg2len;

  //obviously parallel
  if seg1.dir = seg2.dir then
    result := false
  else
  begin
    //try to compute intersection
    inter := IntersectLine(seg1,seg2,para);
    if para then
      result := false
    else
    begin
      //check if intersections are inside the segments
      pos1 := (inter-seg1.origin)*seg1.dir;
      pos2 := (inter-seg2.origin)*seg2.dir;
      if (pos1 >= 0) and (pos1 <= seg1len) and
         (pos2 >= 0) and (pos2 <= seg2len) then
        result := true
      else
        result := false;
    end;
  end;
end;

{ Check if a quaduadrilateral intersects itself }
function DoesQuadIntersect(pt1,pt2,pt3,pt4: TPointF): boolean;
begin
  result := DoesSegmentIntersect(pt1,pt2,pt3,pt4) or DoesSegmentIntersect(pt2,pt3,pt4,pt1);
end;

{************************** Cyclic functions *******************}

// Get the cyclic value in the range [0..cycle-1]
function PositiveMod(value, cycle: integer): integer; inline;
begin
  result := value mod cycle;
  if result < 0 then //modulo can be negative
    Inc(result, cycle);
end;

{ Table of precalc values. Note : the value is stored for
  the first half of the cycle, and values are stored 'minus 1'
  in order to stay in the range 0..65535 }
var
  sinTab65536: packed array of word;

function Sin65536(value: word): integer;
var b: integer;
begin
  //allocate array
  if sinTab65536 = nil then
    setlength(sinTab65536,32768);

  if value >= 32768 then //function is upside down after half-period
  begin
    b := value xor 32768;
    if sinTab65536[b] = 0 then //precalc
      sinTab65536[b] := round((sin(b*2*Pi/65536)+1)*65536/2)-1;
    result := not sinTab65536[b];
  end else
  begin
    b := value;
    if sinTab65536[b] = 0 then //precalc
      sinTab65536[b] := round((sin(b*2*Pi/65536)+1)*65536/2)-1;
    {$hints off}
    result := sinTab65536[b]+1;
    {$hints on}
  end;
end;

function Cos65536(value: word): integer;
begin
  result := Sin65536(value+16384); //cosine is translated
end;

procedure PrecalcSin65536;
var
  i: Integer;
begin
  for i := 0 to 32767 do Sin65536(i);
end;

initialization

  InitGamma;

end.

