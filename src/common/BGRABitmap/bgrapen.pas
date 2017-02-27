unit BGRAPen;

{$mode objfpc}{$H+}

interface

{ This unit handles pen style and width, as well as line caps and join styles.

  A line consists in two points.
  A polyline consists in one or more lines, defined by two points or more than two points
  A poly-polyline consists in a series of polylines, defined by polyline points separated by empty points (see EmptyPointF) }

uses
  Classes, SysUtils, Graphics, BGRABitmapTypes;

var   //predefined pen styles
  SolidPenStyle, DashPenStyle, DotPenStyle, DashDotPenStyle, DashDotDotPenStyle, ClearPenStyle: TBGRAPenStyle;

type
  TBGRAPolyLineOption = (plRoundCapOpen, //specifies that the line ending is opened
                         plCycle);       //specifies that it is a polygon
  TBGRAPolyLineOptions = set of TBGRAPolyLineOption;

{ Draw a polyline with specified parameters. If a scanner is specified, it is used as a texture.
  Else the pencolor parameter is used as a solid color. }
procedure BGRAPolyLine(bmp: TBGRACustomBitmap; const linepts: array of TPointF;
     width: single; pencolor: TBGRAPixel; linecap: TPenEndCap; joinstyle: TPenJoinStyle; const penstyle: TBGRAPenStyle;
     options: TBGRAPolyLineOptions; scan: IBGRAScanner= nil);

{ Compute the path for a polyline }
function ComputeWidePolylinePoints(const linepts: array of TPointF; width: single;
          pencolor: TBGRAPixel; linecap: TPenEndCap; joinstyle: TPenJoinStyle; const penstyle: TBGRAPenStyle;
          options: TBGRAPolyLineOptions): ArrayOfTPointF;

{ Compute the path for a poly-polyline }
function ComputeWidePolyPolylinePoints(const linepts: array of TPointF; width: single;
          pencolor: TBGRAPixel; linecap: TPenEndCap; joinstyle: TPenJoinStyle; const penstyle: TBGRAPenStyle;
          options: TBGRAPolyLineOptions): ArrayOfTPointF;

{ Compute points to draw an antialiased ellipse }
function ComputeEllipse(x,y,rx,ry: single): ArrayOfTPointF;
function ComputeArc65536(x, y, rx, ry: single; start65536,end65536: word): ArrayOfTPointF;
function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single): ArrayOfTPointF;

{--------------------- Pixel line procedures --------------------------}
{ These procedures take integer coordinates as parameters and do not handle pen styles and width.
  They are faster and can be useful for drawing a simple frame }

//aliased version
procedure BGRADrawLineAliased(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer; c: TBGRAPixel; DrawLastPixel: boolean);

//antialiased version
procedure BGRADrawLineAntialias(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  c: TBGRAPixel; DrawLastPixel: boolean);

//antialiased version with bicolor dashes (to draw a frame)
procedure BGRADrawLineAntialias(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean);

//length added to ensure accepable alpha join (using TBGRAMultishapeFiller is still better)
function GetAlphaJoinFactor(alpha: byte): single;

//create standard brush texture
function CreateBrushTexture(prototype: TBGRACustomBitmap; brushstyle: TBrushStyle; PatternColor, BackgroundColor: TBGRAPixel;
    width: integer = 8; height: integer = 8; penwidth: single = 1): TBGRACustomBitmap;

//check special pen styles
function IsSolidPenStyle(ACustomPenStyle: TBGRAPenStyle): boolean;
function IsClearPenStyle(ACustomPenStyle: TBGRAPenStyle): boolean;

implementation

uses math;

procedure BGRADrawLineAliased(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  c: TBGRAPixel; DrawLastPixel: boolean);
var
  Y, X: integer;
  DX, DY, SX, SY, E: integer;
begin

  if (Y1 = Y2) and (X1 = X2) then
  begin
    if DrawLastPixel then
      dest.DrawPixel(X1, Y1, c);
    Exit;
  end;

  DX := X2 - X1;
  DY := Y2 - Y1;

  if DX < 0 then
  begin
    SX := -1;
    DX := -DX;
  end
  else
    SX := 1;

  if DY < 0 then
  begin
    SY := -1;
    DY := -DY;
  end
  else
    SY := 1;

  DX := DX shl 1;
  DY := DY shl 1;

  X := X1;
  Y := Y1;
  if DX > DY then
  begin
    E := DY - DX shr 1;

    while X <> X2 do
    begin
      dest.DrawPixel(X, Y, c);
      if E >= 0 then
      begin
        Inc(Y, SY);
        Dec(E, DX);
      end;
      Inc(X, SX);
      Inc(E, DY);
    end;
  end
  else
  begin
    E := DX - DY shr 1;

    while Y <> Y2 do
    begin
      dest.DrawPixel(X, Y, c);
      if E >= 0 then
      begin
        Inc(X, SX);
        Dec(E, DY);
      end;
      Inc(Y, SY);
      Inc(E, DX);
    end;
  end;

  if DrawLastPixel then
    dest.DrawPixel(X2, Y2, c);
end;

procedure BGRADrawLineAntialias(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  c: TBGRAPixel; DrawLastPixel: boolean);
var
  Y, X:  integer;
  DX, DY, SX, SY, E: integer;
  alpha: single;
begin

  if (Y1 = Y2) and (X1 = X2) then
  begin
    if DrawLastPixel then
      dest.DrawPixel(X1, Y1, c);
    Exit;
  end;

  DX := X2 - X1;
  DY := Y2 - Y1;

  if DX < 0 then
  begin
    SX := -1;
    DX := -DX;
  end
  else
    SX := 1;

  if DY < 0 then
  begin
    SY := -1;
    DY := -DY;
  end
  else
    SY := 1;

  DX := DX shl 1;
  DY := DY shl 1;

  X := X1;
  Y := Y1;

  if DX > DY then
  begin
    E := 0;

    while X <> X2 do
    begin
      alpha := 1 - E / DX;
      dest.DrawPixel(X, Y, BGRA(c.red, c.green, c.blue, round(c.alpha * sqrt(alpha))));
      dest.DrawPixel(X, Y + SY, BGRA(c.red, c.green, c.blue,
        round(c.alpha * sqrt(1 - alpha))));
      Inc(E, DY);
      if E >= DX then
      begin
        Inc(Y, SY);
        Dec(E, DX);
      end;
      Inc(X, SX);
    end;
  end
  else
  begin
    E := 0;

    while Y <> Y2 do
    begin
      alpha := 1 - E / DY;
      dest.DrawPixel(X, Y, BGRA(c.red, c.green, c.blue, round(c.alpha * sqrt(alpha))));
      dest.DrawPixel(X + SX, Y, BGRA(c.red, c.green, c.blue,
        round(c.alpha * sqrt(1 - alpha))));
      Inc(E, DX);
      if E >= DY then
      begin
        Inc(X, SX);
        Dec(E, DY);
      end;
      Inc(Y, SY);
    end;
  end;
  if DrawLastPixel then
    dest.DrawPixel(X2, Y2, c);
end;

procedure BGRADrawLineAntialias(dest: TBGRACustomBitmap; x1, y1, x2, y2: integer;
  c1, c2: TBGRAPixel; dashLen: integer; DrawLastPixel: boolean);
var
  Y, X:  integer;
  DX, DY, SX, SY, E: integer;
  alpha: single;
  c:     TBGRAPixel;
  DashPos: integer;
begin
  if (c1.alpha=0) and (c2.alpha=0) then exit;

  c := c1;
  DashPos := 0;

  if (Y1 = Y2) and (X1 = X2) then
  begin
    if DrawLastPixel then
      dest.DrawPixel(X1, Y1, c);
    Exit;
  end;

  DX := X2 - X1;
  DY := Y2 - Y1;

  if DX < 0 then
  begin
    SX := -1;
    DX := -DX;
  end
  else
    SX := 1;

  if DY < 0 then
  begin
    SY := -1;
    DY := -DY;
  end
  else
    SY := 1;

  DX := DX shl 1;
  DY := DY shl 1;

  X := X1;
  Y := Y1;

  if DX > DY then
  begin
    E := 0;

    while X <> X2 do
    begin
      alpha := 1 - E / DX;
      dest.DrawPixel(X, Y, BGRA(c.red, c.green, c.blue, round(c.alpha * sqrt(alpha))));
      dest.DrawPixel(X, Y + SY, BGRA(c.red, c.green, c.blue,
        round(c.alpha * sqrt(1 - alpha))));
      Inc(E, DY);
      if E >= DX then
      begin
        Inc(Y, SY);
        Dec(E, DX);
      end;
      Inc(X, SX);

      Inc(DashPos);
      if DashPos = DashLen then
        c := c2
      else
      if DashPos = DashLen + DashLen then
      begin
        c := c1;
        DashPos := 0;
      end;
    end;
  end
  else
  begin
    E := 0;

    while Y <> Y2 do
    begin
      alpha := 1 - E / DY;
      dest.DrawPixel(X, Y, BGRA(c.red, c.green, c.blue, round(c.alpha * sqrt(alpha))));
      dest.DrawPixel(X + SX, Y, BGRA(c.red, c.green, c.blue,
        round(c.alpha * sqrt(1 - alpha))));
      Inc(E, DX);
      if E >= DY then
      begin
        Inc(X, SX);
        Dec(E, DY);
      end;
      Inc(Y, SY);

      Inc(DashPos);
      if DashPos = DashLen then
        c := c2
      else
      if DashPos = DashLen + DashLen then
      begin
        c := c1;
        DashPos := 0;
      end;
    end;
  end;
  if DrawLastPixel then
    dest.DrawPixel(X2, Y2, c);
end;

function GetAlphaJoinFactor(alpha: byte): single;
var t: single;
begin
  if alpha = 255 then result := 1 else
  begin
    result := (power(20,alpha/255)-1)/19*0.5;
    t := power(alpha/255,40);
    result := result*(1-t)+t*0.82;
  end;
end;

function CreateBrushTexture(prototype: TBGRACustomBitmap; brushstyle: TBrushStyle;
  PatternColor, BackgroundColor: TBGRAPixel; width: integer = 8; height: integer = 8; penwidth: single = 1): TBGRACustomBitmap;
begin
  result := prototype.NewBitmap(width,height);
  if brushstyle <> bsClear then
  begin
    result.Fill(BackgroundColor);
    if brushstyle in[bsDiagCross,bsBDiagonal] then
    begin
      result.DrawLineAntialias(-1,height,width,-1,PatternColor,penwidth);
      result.DrawLineAntialias(-1-penwidth,0+penwidth,0+penwidth,-1-penwidth,PatternColor,penwidth);
      result.DrawLineAntialias(width-1-penwidth,height+penwidth,width+penwidth,height-1-penwidth,PatternColor,penwidth);
    end;
    if brushstyle in[bsDiagCross,bsFDiagonal] then
    begin
      result.DrawLineAntialias(-1,-1,width,height,PatternColor,penwidth);
      result.DrawLineAntialias(width-1-penwidth,-1-penwidth,width+penwidth,0+penwidth,PatternColor,penwidth);
      result.DrawLineAntialias(-1-penwidth,height-1-penwidth,0+penwidth,height+penwidth,PatternColor,penwidth);
    end;
    if brushstyle in[bsHorizontal,bsCross] then
      result.DrawLineAntialias(-1,height div 2,width,height div 2,PatternColor,penwidth);
    if brushstyle in[bsVertical,bsCross] then
      result.DrawLineAntialias(width div 2,-1,width div 2,height,PatternColor,penwidth);
  end;
end;

function IsSolidPenStyle(ACustomPenStyle: TBGRAPenStyle): boolean;
begin
  result := ACustomPenStyle = nil;
end;

function IsClearPenStyle(ACustomPenStyle: TBGRAPenStyle): boolean;
begin
  if (length(ACustomPenStyle)=1) and (ACustomPenStyle[0]=0) then
    result := true
  else
    result := false;
end;

procedure ApplyPenStyle(const leftPts, rightPts: array of TPointF; const penstyle: TBGRAPenStyle;
    width: single; var posstyle: single; out styledPts: ArrayOfTPointF);
var
  styleIndex :integer;
  remainingDash: single;

  procedure NextStyleIndex;
  begin
    inc(styleIndex);
    if styleIndex = length(penstyle) then
      styleIndex := 0;
    remainingDash += penstyle[styleindex];
  end;

var
  dashStartIndex: integer;
  dashLeftStartPos,dashRightStartPos : TPointF;
  betweenDash: boolean;

  procedure StartDash(index: integer; t: single);
  begin
    dashStartIndex := index;
    dashLeftStartPos := leftPts[index] + (leftPts[index+1]-leftPts[index])*t;
    dashRightStartPos := rightPts[index] + (rightPts[index+1]-rightPts[index])*t;
    betweenDash := false;
  end;

var
  nbStyled: integer;

  procedure AddPt(pt: TPointF);
  begin
    if nbStyled = length(styledPts) then
      setlength(styledPts,nbStyled*2+4);
    styledPts[nbStyled] := pt;
    inc(nbStyled);
  end;

  procedure StartPolygon;
  begin
    if nbStyled > 0 then AddPt(EmptyPointF);
  end;

  procedure EndDash(index: integer; t: single);
  var dashLeftEndPos,dashRightEndPos: TPointF;
    i: Integer;
  begin
    if t=0 then
    begin
      dashLeftEndPos := leftPts[index];
      dashRightEndPos := rightPts[index];
    end else
    begin
      dashLeftEndPos := leftPts[index] + (leftPts[index+1]-leftPts[index])*t;
      dashRightEndPos := rightPts[index] + (rightPts[index+1]-rightPts[index])*t;
    end;
    StartPolygon;
    AddPt(dashLeftStartPos);
    for i := dashStartIndex+1 to index do
      AddPt(leftPts[i]);
    AddPt(dashLeftEndPos);
    AddPt(dashRightEndPos);
    for i := index downto dashStartIndex+1 do
      AddPt(rightPts[i]);
    AddPt(dashRightStartPos);
    betweenDash := true;
  end;

var
  i,nb: integer;
  styleLength: single;
  len,lenDone: single;

begin
  nbStyled := 0;
  styledPts := nil;
  if IsClearPenStyle(penstyle) then exit;
  if IsSolidPenStyle(penstyle) then
  begin
    for i := 0 to high(leftPts) do AddPt(leftPts[i]);
    for i := high(rightPts) downto 0 do AddPt(rightPts[i]);
    setlength(styledPts,nbStyled);
    exit;
  end;
  if length(leftPts) <> length(rightPts) then
    raise Exception.Create('Dimension mismatch');
  nb := length(leftPts);
  if length(penstyle) mod 2 <> 0 then
    raise Exception.Create('Pen style must contain an even number of values');
  styleLength := 0;
  styleIndex := -1;
  for i := 0 to high(penstyle) do
    if penstyle[i] <= 0 then
      raise Exception.Create('Invalid pen dash length')
    else
    begin
      styleLength += penstyle[i];
      if styleLength >= posstyle then
      begin
        styleIndex := i;
        remainingDash := styleLength-posstyle;
        break;
      end;
    end;
  if styleIndex = -1 then
  begin
    styleIndex := 0;
    remainingDash := penstyle[0];
  end;

  if styleIndex mod 2 = 0 then
    StartDash(0, 0) else
      betweenDash := true;
  for i := 0 to nb-2 do
  begin
    len := (sqrt(sqr(leftPts[i+1].x-leftPts[i].x) + sqr(leftPts[i+1].y-leftPts[i].y))+
           sqrt(sqr(rightPts[i+1].x-rightPts[i].x) + sqr(rightPts[i+1].y-rightPts[i].y)))/(2*width);
    lenDone := 0;
    while lenDone < len do
    begin
      if len-lenDone < remainingDash then
      begin
        remainingDash -= len-lenDone;
        if remainingDash = 0 then NextStyleIndex;
        lenDone := len;
      end else
      if betweenDash then
      begin
        lenDone += remainingDash;
        StartDash(i, lenDone/len);
        remainingDash := 0;
        NextStyleIndex;
      end else
      begin
        lenDone += remainingDash;
        EndDash(i, lenDone/len);
        remainingDash := 0;
        NextStyleIndex;
      end;
    end;
  end;
  if not betweenDash then
    EndDash(nb-1,0);
  setlength(styledPts,nbStyled);
end;

procedure BGRAPolyLine(bmp: TBGRACustomBitmap; const linepts: array of TPointF; width: single;
          pencolor: TBGRAPixel; linecap: TPenEndCap; joinstyle: TPenJoinStyle; const penstyle: TBGRAPenStyle;
          options: TBGRAPolyLineOptions; scan: IBGRAScanner= nil);
var
  widePolylinePoints: ArrayOfTPointF;
begin
  widePolylinePoints := ComputeWidePolylinePoints(linepts,width,pencolor,linecap,joinstyle,penstyle,options);
  if scan <> nil then
    bmp.FillPolyAntialias(widePolylinePoints,scan)
  else
    bmp.FillPolyAntialias(widePolylinePoints,pencolor);
end;

function ComputeWidePolylinePoints(const linepts: array of TPointF; width: single;
          pencolor: TBGRAPixel; linecap: TPenEndCap; joinstyle: TPenJoinStyle; const penstyle: TBGRAPenStyle;
          options: TBGRAPolyLineOptions): ArrayOfTPointF;
var
  borders : array of record
              leftSide,rightSide: TLineDef;
              len: single;
              leftDir: TPointF;
            end;
  compPts: array of TPointF;
  nbCompPts: integer;
  revCompPts: array of TPointF;
  nbRevCompPts: integer;
  pts: array of TPointF;
  roundPrecision: integer;
  hw: single; //half-width

  procedure AddPt(normal,rev: TPointF); overload;
  begin
    if (nbCompPts > 0) and (compPts[nbCompPts-1]=normal) and
       (nbRevCompPts > 0) and (revCompPts[nbRevCompPts-1]=rev) then exit;

    if nbCompPts = length(compPts) then
     setlength(compPts, length(compPts)*2);
    compPts[nbCompPts] := normal;
    inc(nbCompPts);

    if nbRevCompPts = length(revCompPts) then
     setlength(revCompPts, length(revCompPts)*2);
    revCompPts[nbRevCompPts] := rev;
    inc(nbRevCompPts);
  end;

  procedure AddPt(xnormal,ynormal: single; xrev,yrev: single); overload;
  begin
    AddPt(PointF(xnormal,ynormal),PointF(xrev,yrev));
  end;

  procedure AddRoundCap(origin: TPointF; dir: TPointF; fromCenter: boolean; flipped: boolean= false);
  var i: integer;
      a,s,c: single;
      offset,flipvalue: single;
  begin
    if fromCenter then offset := 0 else offset := -Pi/2;
    if flipped then flipvalue := -1 else flipvalue := 1;
    for i := 1 to RoundPrecision do
    begin
      a := i/(RoundPrecision+1)*Pi/2 + offset;
      s := sin(a)*hw*flipvalue;
      c := cos(a)*hw;
      AddPt( PointF(origin.x+ dir.x*c - dir.y*s, origin.y + dir.y*c + dir.x*s),
             PointF(origin.x+ dir.x*c + dir.y*s, origin.y + dir.y*c - dir.x*s) );
    end;
  end;

  procedure AddRoundCapAlphaJoin(origin: TPointF; dir: TPointF; fromCenter: boolean; flipped: boolean= false);
  var i: integer;
      a,s,c: single;
      offset,flipvalue: single;
      t,alphaFactor: single; //antialiasing join
  begin
    if fromCenter then offset := 0 else offset := -Pi/2;
    if flipped then flipvalue := -1 else flipvalue := 1;

    alphaFactor := GetAlphaJoinFactor(pencolor.alpha);

    for i := 1 to RoundPrecision do
    begin
      a := i/(RoundPrecision+1)*Pi/2 + offset;
      s := sin(a)*hw*flipvalue;
      c := cos(a);
      t := (1 - c) * (0.2 + alphaFactor*0.3) + alphaFactor;
      c *= hw;
      AddPt( PointF(origin.x+ dir.x*(c-t) - dir.y*s, origin.y + dir.y*(c-t) + dir.x*s),
             PointF(origin.x+ dir.x*(c-t) + dir.y*s, origin.y + dir.y*(c-t) - dir.x*s) );
    end;
  end;

  function ComputeRoundJoin(origin, pt1,pt2: TPointF): ArrayOfTPointF;
  var a1,a2: single;
      da: single;
      precision,i: integer;
  begin
    a1 := arctan2(pt1.y-origin.y,pt1.x-origin.x);
    a2 := arctan2(pt2.y-origin.y,pt2.x-origin.x);
    if a2-a1 > Pi then a2 -= 2*Pi;
    if a1-a2 > Pi then a1 -= 2*Pi;
    if a2=a1 then
    begin
      setlength(result,1);
      result[0] := pt1;
      exit;
    end;
    da := a2-a1;
    precision := round( sqrt( sqr(pt2.x-pt1.x)+sqr(pt2.y-pt1.y) ) ) +2;
    setlength(result,precision);
    for i := 0 to precision-1 do
      result[i] := origin + PointF( cos(a1+i/(precision-1)*da)*hw,
                                    sin(a1+i/(precision-1)*da)*hw );
  end;

var
  joinLeft,joinRight: array of TPointF;
  nbJoinLeft,nbJoinRight: integer;

  procedure SetJoinLeft(joinpts: array of TPointF);
  var i: integer;
  begin
    nbJoinLeft := length(joinpts);
    if length(joinLeft) < nbJoinLeft then setlength(joinLeft,length(joinLeft)+nbJoinLeft+2);
    for i := 0 to nbJoinLeft-1 do
      joinLeft[i] := joinpts[i];
  end;

  procedure SetJoinRight(joinpts: array of TPointF);
  var i: integer;
  begin
    nbJoinRight := length(joinpts);
    if length(joinRight) < nbJoinRight then setlength(joinRight,length(joinRight)+nbJoinRight+2);
    for i := 0 to nbJoinRight-1 do
      joinRight[i] := joinpts[i];
  end;

  procedure AddJoin(index: integer);
  var len,i: integer;
  begin
    len := nbJoinLeft;
    if nbJoinRight > len then
      len := nbJoinRight;
    if len = 0 then exit;
    if (len > 1) and (index <> -1) then
    begin
      if nbJoinLeft=1 then
        AddPt(joinLeft[0], joinLeft[0] - 2*borders[Index].leftDir) else
      if nbJoinRight=1 then
        AddPt( joinRight[0] + 2* borders[index].leftDir, joinRight[0]);
    end;
    for i := 0 to len-1 do
    begin
      AddPt(joinLeft[i*nbJoinLeft div len],
            joinRight[i*nbJoinRight div len]);
    end;
    if (len > 1) and (index <> -1) then
    begin
      if nbJoinLeft=1 then
        AddPt(joinLeft[0], joinLeft[0] - 2*borders[index+1].leftDir) else
      if nbJoinRight=1 then
        AddPt(joinRight[0]+2*borders[index+1].leftDir, joinRight[0]);
    end;
  end;

var
  NbPolyAcc: integer;

  procedure FlushLine(lastPointIndex: integer);
  var
    enveloppe: arrayOfTPointF;
    posstyle: single;
    i,idxInsert: Integer;
  begin
    if lastPointIndex <> -1 then
       AddPt( pts[lastPointIndex] + borders[lastPointIndex-1].leftDir,
              pts[lastPointIndex] - borders[lastPointIndex-1].leftDir);

    if (lastPointIndex = high(pts)) and (linecap = pecRound) then
    begin
      if not (plRoundCapOpen in options) then
        AddRoundCap(pts[high(pts)],borders[high(pts)-1].leftSide.dir,false)
      else
       AddRoundCapAlphaJoin(pts[high(pts)],
            -borders[high(pts)-1].leftSide.dir, false,true);
    end;
    posstyle := 0;
    ApplyPenStyle(slice(compPts,nbCompPts),slice(revCompPts,nbRevCompPts),penstyle,width,posstyle,enveloppe);

    if Result=nil then
    begin
      Result := enveloppe;
      NbPolyAcc := length(enveloppe);
    end
      else
    if enveloppe <> nil then
    begin
      if NbPolyAcc +1+length(enveloppe) > length(Result) then
        setlength(Result, length(Result)*2+1+length(enveloppe));

      idxInsert := NbPolyAcc+1;
      Result[idxInsert-1] := EmptyPointF;
      for i := 0 to high(enveloppe) do
        Result[idxInsert+i]:= enveloppe[i];
      inc(NbPolyAcc, length(enveloppe)+1);
    end;

    nbCompPts := 0;
    nbRevCompPts := 0;
  end;

  procedure CycleFlush;
  var idx: integer;
  begin
    if Result = nil then
    begin
      if (nbCompPts > 1) and (nbRevCompPts > 1) then
      begin
        compPts[0] := compPts[nbCompPts-1];
        revCompPts[0] := revCompPts[nbRevCompPts-1];
      end;
      FlushLine(-1);
    end else
    begin
      if (nbCompPts >= 1) and (nbRevCompPts >= 1) and (NbPolyAcc >= 2) then
      begin
        Result[0] := compPts[nbCompPts-1];
        idx := 0;
        while (idx < high(Result)) and (not isEmptyPointF(Result[idx+1])) do inc(idx);
        Result[idx] := revCompPts[nbRevCompPts-1];
      end;
      FlushLine(-1);
    end;
  end;

var
  i: integer;
  dir: TPointF;
  leftInter,rightInter,diff: TPointF;
  len,maxMiter: single;
  littleBorder: TLineDef;
  turn,maxDiff: single;
  nbPts: integer;
  ShouldFlushLine, HasLittleBorder, NormalRestart: Boolean;
  pt1,pt2,pt3,pt4: TPointF;

begin
  Result := nil;

  if length(linepts)=0 then exit;
  if IsClearPenStyle(penstyle) then exit;
  for i := 0 to high(linepts) do
    if isEmptyPointF(linepts[i]) then
    begin
      result := ComputeWidePolyPolylinePoints(linepts,width,pencolor,linecap,joinstyle,penstyle,options);
      exit;
    end;

  hw := width / 2;
  case joinstyle of
  pjsBevel,pjsRound: maxMiter := hw*1.001;
  pjsMiter: maxMiter := hw*2;
  end;

  roundPrecision := round(hw)+2;

  nbPts := 0;
  setlength(pts, length(linepts)+2);
  for i := 0 to high(linepts) do
    if (nbPts = 0) or (linepts[i] <> pts[nbPts-1]) then
    begin
      pts[nbPts]:= linePts[i];
      inc(nbPts);
    end;
  if (nbPts > 1) and
      (pts[nbPts-1] = pts[0]) then dec(nbPts);
  if (plCycle in options) and (nbPts > 2) then
  begin
    pts[nbPts] := pts[0];
    inc(nbPts);
    pts[nbPts] := pts[1];
    inc(nbPts);
    linecap := pecRound;
  end else
    options -= [plCycle];

  setlength(pts,nbPts);

  if nbPts = 1 then
  begin
    if (linecap <> pecFlat) and ((linecap <> pecRound) or not (plRoundCapOpen in options)) then
      result := ComputeEllipse(pts[0].x,pts[0].y,hw,hw);
    exit;
  end;

  //init computed points arrays
  setlength(compPts, length(pts)*2+4);
  setlength(revCompPts, length(pts)*2+4); //reverse order array
  nbCompPts := 0;
  nbRevCompPts := 0;
  NbPolyAcc := 0;

  //compute borders
  setlength(borders, length(pts)-1);
  for i := 0 to high(pts)-1 do
  begin
    dir := pts[i+1]-pts[i];
    len := sqrt(dir*dir);
    dir *= 1/len;

    if (linecap = pecSquare) and ((i=0) or (i=high(pts)-1)) then //for square cap, just start and end further
    begin
      if i=0 then
        pts[0] -= dir*hw;

      if (i=high(pts)-1) then
        pts[high(pts)] += dir*hw;

      //length changed
      dir := pts[i+1]-pts[i];
      len := sqrt(dir*dir);
      dir *= 1/len;
    end else
    if (linecap = pecRound) and (i=0) and not (plCycle in options) then
      AddRoundCap(pts[0], -dir ,true);

    borders[i].len := len;
    borders[i].leftDir := PointF(dir.y*hw,-dir.x*hw);
    borders[i].leftSide.origin := pts[i] + borders[i].leftDir;
    borders[i].leftSide.dir := dir;
    borders[i].rightSide.origin := pts[i] - borders[i].leftDir;
    borders[i].rightSide.dir := dir;
  end;

  //first points
  AddPt( pts[0] + borders[0].leftDir,
         pts[0] - borders[0].leftDir );

  setlength(joinLeft,1);
  setlength(joinRight,1);
  ShouldFlushLine := False;
  //between first and last points
  for i := 0 to high(pts)-2 do
  begin
    HasLittleBorder := false;

    //determine u-turn
    turn := borders[i].leftSide.dir * borders[i+1].leftSide.dir;
    if turn < -0.99999 then
    begin
      if joinstyle <> pjsRound then
      begin
        littleBorder.origin := pts[i+1] + borders[i].leftSide.dir*maxMiter;
        littleBorder.dir := borders[i].leftDir;
        HasLittleBorder := true;
      end;

      nbJoinLeft := 0;
      nbJoinRight:= 0;

      ShouldFlushLine := True;
    end else
    if turn > 0.99999 then //straight line
    begin
      pt1 := pts[i+1] + borders[i].leftDir;
      pt2 := pts[i+2] + borders[i+1].leftDir;
      SetJoinLeft([pt1, (pt1+pt2)*(1/2),pt2]);

      pt1 := pts[i+1] - borders[i].leftDir;
      pt2 := pts[i+2] - borders[i+1].leftDir;
      SetJoinRight([pt1,(pt1+pt2)*(1/2),pt2]);
    end else
    begin
      //determine turning left or right
      turn := borders[i].leftSide.dir.x*borders[i+1].leftSide.dir.y - borders[i].leftSide.dir.y*borders[i+1].leftSide.dir.x;

      maxDiff := borders[i].len;
      if borders[i+1].len < maxDiff then
        maxDiff := borders[i+1].len;
      if penstyle <> nil then
        if maxDiff > 2*width then maxDiff := 2*width;
      maxDiff := sqrt(sqr(maxDiff)+sqr(hw));

      //leftside join
      leftInter := IntersectLine( borders[i].leftSide, borders[i+1].leftSide );
      diff := leftInter-pts[i+1];
      len := sqrt(diff*diff);
      if (len > maxMiter) and (turn >= 0) then //if miter too far
      begin
        diff.x /= len;
        diff.y /= len;
        if joinstyle <> pjsRound then
        begin
          //compute little border
          littleBorder.origin := pts[i+1]+diff*maxMiter;
          littleBorder.dir := PointF(diff.y,-diff.x);
          HasLittleBorder := true;

          //intersect with each border
          pt1 := IntersectLine(borders[i].leftSide, littleBorder);
          pt2 := IntersectLine(borders[i+1].leftSide, littleBorder);
          SetJoinLeft( [pt1, pt2] );
        end else
        begin
          //perpendicular
          pt1 := PointF(pts[i+1].x+borders[i].leftSide.dir.y*hw,
                        pts[i+1].y-borders[i].leftSide.dir.x*hw);
          pt2 := PointF(pts[i+1].x+borders[i+1].leftSide.dir.y*hw,
                        pts[i+1].y-borders[i+1].leftSide.dir.x*hw);
          SetJoinLeft(ComputeRoundJoin(pts[i+1],pt1,pt2));
        end;
      end else
      if (len > maxDiff) and (turn <= 0) then //if inner intersection too far
      begin
        ShouldFlushLine := True;
        nbJoinLeft := 0;
      end else
      begin
        if (turn > 0) and (len > 1.0001*hw) then
          SetJoinLeft([leftInter,leftInter]) else
        begin
          nbJoinLeft := 1;
          joinLeft[0] := leftInter;
        end;
      end;

      //rightside join
      rightInter := IntersectLine( borders[i].rightSide, borders[i+1].rightSide );
      diff := rightInter-pts[i+1];
      len := sqrt(diff*diff);
      if (len > maxMiter) and (turn <= 0) then //if miter too far
      begin
        diff *= 1/len;

        if joinstyle <> pjsRound then
        begin
          //compute little border
          littleBorder.origin := pts[i+1] + diff*maxMiter;
          littleBorder.dir := PointF(diff.y, -diff.x);
          HasLittleBorder := true;

          //intersect with each border
          pt1 := IntersectLine(borders[i].rightSide, littleBorder);
          pt2 := IntersectLine(borders[i+1].rightSide, littleBorder);
          SetJoinRight( [pt1, pt2] );
        end else
        begin
          //perpendicular
          pt1 := PointF(pts[i+1].x-borders[i].rightSide.dir.y*hw,
                        pts[i+1].y+borders[i].rightSide.dir.x*hw);
          pt2 := PointF(pts[i+1].x-borders[i+1].rightSide.dir.y*hw,
                        pts[i+1].y+borders[i+1].rightSide.dir.x*hw);
          SetJoinRight(ComputeRoundJoin(pts[i+1],pt1,pt2));
        end;
      end else
      if (len > maxDiff) and (turn >= 0) then //if inner intersection too far
      begin
        ShouldFlushLine := True;
        nbJoinRight := 0;
      end else
      begin
        if (turn < 0) and (len > 1.0001*hw) then
          SetJoinRight([rightInter,rightInter]) else
        begin
          nbJoinRight := 1;
          joinRight[0] := rightInter;
        end;
      end;
    end;

    if ShouldFlushLine then
    begin
      NormalRestart := True;
      if HasLittleBorder then
      begin
        if turn >= 0 then
        begin
          //intersect with each border
          pt1 := IntersectLine(borders[i].leftSide, littleBorder);
          pt2 := IntersectLine(borders[i+1].leftSide, littleBorder);
          pt3 := pts[i+1] - borders[i].leftDir;
          pt4 := pts[i+1] + borders[i].leftDir;

          AddPt(pt4,pt3);
          AddPt(pt1,pt2);
        end else
        begin
          //intersect with each border
          pt1 := IntersectLine(borders[i+1].rightSide, littleBorder);
          pt2 := IntersectLine(borders[i].rightSide, littleBorder);
          pt3 := pts[i+1] + borders[i].leftDir;
          pt4 := pts[i+1] - borders[i].leftDir;

          AddPt(pt3,pt4);
          AddPt(pt1,pt2);
        end;

        FlushLine(-1);

        AddPt(pt2,pt1);
      end else
      if joinstyle = pjsRound then
      begin

        if (penstyle= nil) and (turn > 0) then
        begin
          pt1 := pts[i+1] + borders[i].leftDir;
          pt2 := pts[i+1] + borders[i+1].leftDir;
          SetJoinLeft(ComputeRoundJoin(pts[i+1],pt1,pt2));
          nbJoinRight := 1;
          joinRight[0] := pts[i+1] - borders[i].leftDir;
          AddJoin(-1);
          FlushLine(-1);
        end else
        if (penstyle= nil) and (turn < 0) then
        begin
          pt1 := pts[i+1] - borders[i].leftDir;
          pt2 := pts[i+1] - borders[i+1].leftDir;
          SetJoinRight(ComputeRoundJoin(pts[i+1],pt1,pt2));
          nbJoinLeft := 1;
          joinLeft[0] := pts[i+1] + borders[i].leftDir;
          AddJoin(-1);
          FlushLine(-1);
        end else
        if (nbCompPts > 1) and (nbRevCompPts > 1) then
        begin
          pt1 := pts[i+1]+borders[i].leftDir;
          pt2 := pts[i+1]-borders[i].leftDir;
          AddPt( pt1, pt2 );
          FlushLine(-1);
        end else
        begin
          FlushLine(i+1);
        end;
      end else
      begin
        FlushLine(i+1);
        if turn > 0 then
          AddPt( leftInter, pts[i+1]+borders[i].leftDir ) else
        if turn < 0 then
          AddPt( pts[i+1] - borders[i].leftDir, rightInter );
      end;

      If NormalRestart then
        AddPt(pts[i+1]+borders[i+1].leftDir,
              pts[i+1]-borders[i+1].leftDir);

      ShouldFlushLine := false;
    end else
      AddJoin(i);
  end;

  if plCycle in options then
    CycleFlush
  else
    FlushLine(high(pts));

  SetLength(Result, NbPolyAcc);
end;

function ComputeWidePolyPolylinePoints(const linepts: array of TPointF;
  width: single; pencolor: TBGRAPixel; linecap: TPenEndCap;
  joinstyle: TPenJoinStyle; const penstyle: TBGRAPenStyle;
  options: TBGRAPolyLineOptions): ArrayOfTPointF;

var
  results: array of array of TPointF;
  nbResults,nbTotalPts: integer;

  procedure AddWidePolyline(startIndex,endIndexP1: integer);
  var
    tempWidePolyline: array of TPointF;
    subPts: array of TPointF;
    j : integer;
  begin
    if endIndexP1 > startIndex then
    begin
      setlength(subPts,endIndexP1-startIndex);
      for j := startIndex to endIndexP1-1 do
        subPts[j-startIndex] := linepts[j];
      tempWidePolyline := ComputeWidePolylinePoints(subPts,width,pencolor,linecap,joinstyle,penstyle,options);
      if length(results) = nbresults then
        setlength(results,(nbresults+1)*2);
      results[nbResults] := tempWidePolyline;
      if nbResults <> 0 then inc(nbTotalPts);
      inc(nbResults);
      inc(nbTotalPts,length(tempWidePolyline));
    end;
  end;

var
  start,i,j: integer;

begin
  start := 0;
  nbResults := 0;
  nbTotalPts := 0;
  for i := 0 to high(linepts) do
    if isEmptyPointF(linepts[i]) then
    begin
      AddWidePolyline(start,i);
      start := i+1;
    end;
  AddWidePolyline(start,length(linepts));

  setlength(result, nbTotalPts);
  start := 0;
  for i := 0 to nbResults-1 do
  begin
    if i <> 0 then
    begin
      result[start] := EmptyPointF;
      inc(start);
    end;
    for j := 0 to high(results[i]) do
    begin
      result[start] := results[i][j];
      inc(start);
    end;
  end;
end;

{$PUSH}{$R-}
function ComputeArc65536(x, y, rx, ry: single; start65536,end65536: word): ArrayOfTPointF;
var i,nb: integer;
    arclen: integer;
    pos: word;
begin
  if end65536 > start65536 then
    arclen := end65536-start65536 else
    arclen := 65536-(start65536-end65536);

  nb := round(((rx+ry)*2+8)*arclen/65536) and not 3;
  if nb < 2 then nb := 2;
  if nb > arclen+1 then nb := arclen+1;

  setlength(result,nb);
  for i := 0 to nb-1 do
  begin
    pos := start65536+int64(i)*arclen div (int64(nb)-1);
    result[i] := PointF(x+rx*(Cos65536(pos)-32768)/32768,
                        y-ry*(Sin65536(pos)-32768)/32768);
  end;
end;
{$R+}

function ComputeEllipse(x, y, rx, ry: single): ArrayOfTPointF;
begin
  result := ComputeArc65536(x,y,rx,ry,0,0);
end;

function ComputeRoundRect(x1,y1,x2,y2,rx,ry: single): ArrayOfTPointF;
var q1,q2,q3,q4: array of TPointF;
  temp: Single;
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
  rx := abs(rx);
  ry := abs(ry);
  if 2*rx > x2-x1 then
    rx := (x2-x1)/2;
  if 2*ry > y2-y1 then
    ry := (y2-y1)/2;
  q1 := ComputeArc65536(x2-rx,y1+ry,rx,ry,0,16384);
  q2 := ComputeArc65536(x1+rx,y1+ry,rx,ry,16384,32768);
  q3 := ComputeArc65536(x1+rx,y2-ry,rx,ry,32768,32768+16384);
  q4 := ComputeArc65536(x2-rx,y2-ry,rx,ry,32768+16384,0);
  result := ConcatPointsF([q1,q2,q3,q4]);
end;

initialization

  //special pen styles
  SolidPenStyle := nil;

  setlength(ClearPenStyle,1);
  ClearPenStyle[0] := 0;

  DashPenStyle := BGRAPenStyle(3,1);
  DotPenStyle := BGRAPenStyle(1,1);
  DashDotPenStyle := BGRAPenStyle(3,1,1,1);
  DashDotDotPenStyle := BGRAPenStyle(3,1,1,1,1,1);

end.

