{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit bgrabitmappack; 

interface

uses
    BGRAAnimatedGif, BGRABitmap, BGRABitmapTypes, BGRABlend, 
  bgracompressablebitmap, BGRADefaultBitmap, BGRADNetDeserial, BGRAFilters, 
  BGRAGradients, BGRAPaintNet, BGRAPolygon, BGRAResample, BGRAPen, 
  BGRATransform, BGRAGradientScanner, BGRAText, BGRAPolygonAliased, 
  BGRACanvas, BGRAFillInfo, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('bgrabitmappack', @Register); 
end.
