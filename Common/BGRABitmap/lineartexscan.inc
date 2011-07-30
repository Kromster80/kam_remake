  var
    xLen: single;       //horizontal length in pixels
    t: single;          //initial horizontal position in [0..1]
    texVect: TPointF;   //texture vector between start and end of line
    texPos: TPointF;    //texture start position
    texStep: TPointF;   //texture step

    //loop variables
    pdest: PBGRAPixel;
    i: LongInt;

  {$IFDEF PARAM_USELIGHTING}
    light,lightStep,lightDiff,lightAcc,lightMod: word;
    lightLen: integer;

    procedure NextLight; inline;
    begin
      inc(light,lightStep);
      inc(lightAcc,lightDiff);
      if lightAcc >= lightMod then
      begin
        dec(lightAcc,lightMod);
        inc(light);
      end;
    end;
  {$ENDIF}

  begin
    xLen := info2.interX - info1.interX;
    t := ((ix1+0.5)-info1.interX)/xLen;

    texVect := info2.texCoord-info1.texCoord;
    texPos := info1.texCoord + texVect*t;
    texStep := texVect*(1/xLen);

    pdest := bmp.ScanLine[yb]+ix1;

  {$IFDEF PARAM_USELIGHTING}
    if ix2 = ix1 then
    begin
      light := (info1.lightness+info2.lightness) div 2;
      lightStep := 0;
      lightDiff := 0;
      lightMod := 1;
    end
    else
    begin
      light := info1.lightness;
      lightLen := info2.lightness-info1.lightness;
      lightStep := lightLen div (ix2-ix1);
      lightMod := ix2-ix1;
      lightDiff := lightLen - lightStep*(ix2-ix1);
    end;
    lightAcc := lightDiff div 2;
  {$ENDIF}

    if WithInterpolation then
    begin
      for i := ix1 to ix2 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest,
          {$IFDEF PARAM_USELIGHTING} ApplyLightnessFast( {$ENDIF}
          scanAtFunc(texPos.x,texPos.y)
          {$IFDEF PARAM_USELIGHTING} ,light) {$ENDIF}
          );
        texPos += texStep;
        {$IFDEF PARAM_USELIGHTING}
          NextLight;
        {$ENDIF}
        inc(pdest);
      end;
    end else
    begin
      for i := ix1 to ix2 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest,
          {$IFDEF PARAM_USELIGHTING} ApplyLightnessFast( {$ENDIF}
          scanAtFunc(round(texPos.x),round(texPos.y))
          {$IFDEF PARAM_USELIGHTING} ,light) {$ENDIF}
          );
        texPos += texStep;
        {$IFDEF PARAM_USELIGHTING}
          NextLight;
        {$ENDIF}
        inc(pdest);
      end;
    end;
  end;
{$undef PARAM_USELIGHTING}
