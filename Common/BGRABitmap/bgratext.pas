unit BGRAText;

{$mode objfpc}{$H+}

interface

{ Text functions use a temporary bitmap where the operating system text drawing is used.
  Then it is scaled down (if antialiasing is activated), and colored.

  These routines are rather slow. }

uses
  Classes, SysUtils, BGRABitmapTypes, Graphics, Types;

procedure BGRATextOut(bmp: TBGRACustomBitmap; Font: TFont; Antialiasing: boolean; x, y: integer; s: string;
  c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment);

procedure BGRATextOutAngle(bmp: TBGRACustomBitmap; Font: TFont; Antialiasing: boolean; x, y, orientation: integer;
  s: string; c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment);

procedure BGRATextRect(bmp: TBGRACustomBitmap; Font: TFont; Antialiasing: boolean; ARect: TRect; x, y: integer;
  s: string; style: TTextStyle; c: TBGRAPixel; tex: IBGRAScanner);

function BGRATextSize(Font: TFont; Antialiasing: boolean; s: string): TSize;

implementation

uses Math;

var
  TempBmp: TBitmap;

const FontAntialiasingLevel = 6;

function OriginalTextSize(Font: TFont; Antialiasing: boolean; s: string): TSize;
begin
  if tempBmp = nil then tempBmp := TBitmap.Create;
  tempBmp.Canvas.Font := Font;
  if Antialiasing then tempBmp.Canvas.Font.Height := Font.Height*FontAntialiasingLevel else
    tempBmp.Canvas.Font.Height := Font.Height;
  tempBmp.Canvas.Font.GetTextSize(s, Result.cx, Result.cy);
end;

function BGRATextSize(Font: TFont; Antialiasing: boolean; s: string): TSize;
begin
  result := OriginalTextSize(Font, Antialiasing, s);
  if Antialiasing then
  begin
    result.cx := ceil(Result.cx/FontAntialiasingLevel);
    result.cy := ceil(Result.cy/FontAntialiasingLevel);
  end;
end;

procedure FilterOriginalText(Antialiasing: boolean; var temp: TBGRACustomBitmap;
  c: TBGRAPixel; tex: IBGRAScanner);
var
  resampled: TBGRACustomBitmap;
  P:       PBGRAPixel;
  n,xb,yb: integer;
  alpha, maxAlpha: integer;
begin
  if Antialiasing then
  begin
    resampled := temp.Resample(round(temp.width/FontAntialiasingLevel),round(temp.Height/FontAntialiasingLevel),rmSimpleStretch);

    maxAlpha := 0;
    if tex = nil then
    begin
      p := resampled.Data;
      for n := resampled.NbPixels - 1 downto 0 do
      begin
        alpha    := P^.green;
        if alpha > maxAlpha then maxAlpha := alpha;
        if alpha = 0 then
          p^:= BGRAPixelTransparent else
        begin
          p^.red   := c.red;
          p^.green := c.green;
          p^.blue  := c.blue;
          p^.alpha := alpha;
        end;
        Inc(p);
      end;
    end else
    begin
      for yb := 0 to resampled.Height-1 do
      begin
        p := resampled.ScanLine[yb];
        tex.ScanMoveTo(0,yb);
        for xb := 0 to resampled.Width-1 do
        begin
          c := tex.ScanNextPixel;
          alpha    := P^.green;
          if alpha > maxAlpha then maxAlpha := alpha;
          if alpha = 0 then
            p^:= BGRAPixelTransparent else
          begin
            p^.red   := c.red;
            p^.green := c.green;
            p^.blue  := c.blue;
            p^.alpha := alpha;
          end;
          Inc(p);
        end;
      end;
    end;
    if maxAlpha <> 0 then
    begin
      p := resampled.Data;
      for n := resampled.NbPixels - 1 downto 0 do
      begin
        p^.alpha := integer(p^.alpha * c.alpha) div maxAlpha;
        Inc(p);
      end;
    end;
    temp.Free;
    temp := resampled;
  end else
  begin
    if tex = nil then
    begin
      p := temp.Data;
      for n := temp.NbPixels - 1 downto 0 do
      begin
        alpha    := GammaExpansionTab[P^.green] shr 8;
        alpha    := (c.alpha * alpha) div (255);
        if alpha = 0 then p^:= BGRAPixelTransparent else
        begin
          p^.red   := c.red;
          p^.green := c.green;
          p^.blue  := c.blue;
          p^.alpha := alpha;
        end;
        Inc(p);
      end;
    end else
    begin
      for yb := 0 to temp.Height-1 do
      begin
        p := temp.Scanline[yb];
        tex.ScanMoveTo(0,yb);
        for xb := 0 to temp.Width-1 do
        begin
          c := tex.ScanNextPixel;
          alpha    := GammaExpansionTab[P^.green] shr 8;
          alpha    := (c.alpha * alpha) div (255);
          if alpha = 0 then p^:= BGRAPixelTransparent else
          begin
            p^.red   := c.red;
            p^.green := c.green;
            p^.blue  := c.blue;
            p^.alpha := alpha;
          end;
          Inc(p);
        end;
      end;
    end;
  end;
end;

procedure BGRATextOut(bmp: TBGRACustomBitmap; Font: TFont; Antialiasing: Boolean; x, y: integer; s: string;
  c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment);
var
  size: TSize;
  temp: TBGRACustomBitmap;
begin
  if Font.Orientation <> 0 then
  begin
    BGRATextOutAngle(bmp,Font,Antialiasing,x,y,Font.Orientation,s,c,tex,align);
    exit;
  end;

  size := OriginalTextSize(Font,Antialiasing,s);
  if (size.cx = 0) or (size.cy = 0) then
    exit;

  temp := bmp.NewBitmap(size.cx, size.cy);
  temp.Fill(clBlack);
  temp.Canvas.Font := Font;
  if Antialiasing then temp.Canvas.Font.Height := Font.Height*FontAntialiasingLevel
   else temp.Canvas.Font.Height := Font.Height;
  temp.Canvas.Font.Color := clWhite;
  temp.Canvas.Brush.Style := bsClear;
  temp.Canvas.TextOut(0, 0, s);

  FilterOriginalText(Antialiasing,temp,c,tex);

  case align of
    taLeftJustify: ;
    taCenter: Dec(x, temp.width div 2);
    taRightJustify: Dec(x, temp.width);
  end;
  bmp.PutImage(x, y, temp, dmDrawWithTransparency);
  temp.Free;
end;

procedure BGRATextOutAngle(bmp: TBGRACustomBitmap; Font: TFont; Antialiasing: boolean; x, y, orientation: integer;
  s: string; c: TBGRAPixel; tex: IBGRAScanner; align: TAlignment);
var
  size: TSize;
  temp: TBGRACustomBitmap;
  TopRight,BottomRight,BottomLeft: TPointF;
  cosA,sinA: single;
  rotBounds: TRect;
  sizeFactor: integer;
  TempFont: TFont;

  procedure rotBoundsAdd(pt: TPointF);
  begin
    if floor(pt.X) < rotBounds.Left then rotBounds.Left := floor(pt.X/sizeFactor)*sizeFactor;
    if floor(pt.Y) < rotBounds.Top then rotBounds.Top := floor(pt.Y/sizeFactor)*sizeFactor;
    if ceil(pt.X) > rotBounds.Right then rotBounds.Right := ceil(pt.X/sizeFactor)*sizeFactor;
    if ceil(pt.Y) > rotBounds.Bottom then rotBounds.Bottom := ceil(pt.Y/sizeFactor)*sizeFactor;
  end;

begin
  TempFont := TFont.Create;
  TempFont.Assign(Font);
  TempFont.Orientation := orientation;
  TempFont.Height := Font.Height;
  size := OriginalTextSize(TempFont,Antialiasing,s);
  if (size.cx = 0) or (size.cy = 0) then
  begin
    tempFont.Free;
    exit;
  end;
  if Antialiasing then
    sizeFactor := FontAntialiasingLevel
  else
    sizeFactor := 1;

  cosA := cos(orientation*Pi/1800);
  sinA := sin(orientation*Pi/1800);
  TopRight := PointF(cosA*size.cx,-sinA*size.cx);
  BottomRight := PointF(cosA*size.cx+sinA*size.cy,cosA*size.cy-sinA*size.cx);
  BottomLeft := PointF(sinA*size.cy,cosA*size.cy);
  rotBounds := rect(0,0,0,0);
  rotBoundsAdd(TopRight);
  rotBoundsAdd(BottomRight);
  rotBoundsAdd(BottomLeft);
  inc(rotBounds.Right);
  inc(rotBounds.Bottom);

  temp := bmp.NewBitmap(rotBounds.Right-rotBounds.Left,rotBounds.Bottom-rotBounds.Top);
  temp.Fill(clBlack);
  temp.Canvas.Font := Font;
  temp.Canvas.Font.Color := clWhite;
  temp.Canvas.Font.Orientation := orientation;
  if Antialiasing then temp.Canvas.Font.Height := Font.Height*FontAntialiasingLevel
     else temp.Canvas.Font.Height := Font.Height;
  temp.Canvas.Brush.Style := bsClear;
  temp.Canvas.TextOut(-rotBounds.Left, -rotBounds.Top, s);

  FilterOriginalText(Antialiasing,temp,c,tex);

  inc(x,round(rotBounds.Left/sizeFactor));
  inc(y,round(rotBounds.Top/sizeFactor));
  case align of
    taLeftJustify: ;
    taCenter:
      begin
        Dec(x, round(TopRight.x/2/sizeFactor));
        Dec(y, round(TopRight.y/2/sizeFactor));
      end;
    taRightJustify:
      begin
        Dec(x, round(TopRight.x/sizeFactor));
        Dec(y, round(TopRight.y/sizeFactor));
      end;
  end;
  bmp.PutImage(x, y, temp, dmDrawWithTransparency);
  temp.Free;
  tempFont.Free;
end;

procedure BGRATextRect(bmp: TBGRACustomBitmap; Font: TFont; Antialiasing: boolean; ARect: TRect; x, y: integer;
  s: string; style: TTextStyle; c: TBGRAPixel; tex: IBGRAScanner);
var
  lim: TRect;
  tx, ty: integer;
  temp:   TBGRACustomBitmap;
  sizeFactor: integer;
  cr: TRect;
begin
  cr := bmp.ClipRect;
  if ARect.Left < cr.Left then
    lim.Left := cr.Left else lim.Left := ARect.Left;
  if ARect.Top < cr.Top then
    lim.Top := cr.Top else lim.Top := ARect.Top;
  if ARect.Right > cr.Right then
    lim.Right := cr.Right else lim.Right := ARect.Right;
  if ARect.Bottom > cr.Bottom then
    lim.Bottom := cr.Bottom else lim.Bottom := ARect.Bottom;

  tx := lim.Right - lim.Left;
  ty := lim.Bottom - lim.Top;
  if (tx <= 0) or (ty <= 0) then
    exit;

  if Antialiasing then
    sizeFactor := FontAntialiasingLevel
  else
    sizeFactor := 1;

  temp := bmp.NewBitmap(tx*sizeFactor, ty*sizeFactor);
  temp.Fill(clBlack);
  temp.Canvas.Font := Font;
  temp.Canvas.Font.Orientation := 0;
  if Antialiasing then temp.Canvas.Font.Height := Font.Height*FontAntialiasingLevel
     else temp.Canvas.Font.Height := Font.Height;
  temp.Canvas.Font.Color := clWhite;
  temp.Canvas.Brush.Style := bsClear;
  temp.Canvas.TextRect(rect(lim.Left-ARect.Left, lim.Top-ARect.Top, (ARect.Right-ARect.Left)*sizeFactor, (ARect.Bottom-ARect.Top)*sizeFactor), (x - lim.Left)*sizeFactor, (y - lim.Top)*sizeFactor, s, style);

  FilterOriginalText(Antialiasing,temp,c,tex);

  bmp.PutImage(lim.Left, lim.Top, temp, dmDrawWithTransparency);
  temp.Free;
end;

initialization

  tempBmp := nil;

finalization

  tempBmp.Free;

end.

