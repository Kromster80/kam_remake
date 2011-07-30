{$ifdef PARAM_PHONGSSE}
  {$asmmode intel}
  //SSE rotate singles
  const Shift231 = 1 + 8;
        Shift312 = 2 + 16;
{$endif}

var
  //Light source normal.
  vL: TVector3D;  {xmm0}
  //Light source position.
  vLS: TVector3D; {xmm1}
  //Vector H is the unit normal to the hypothetical surface oriented
  //halfway between the light direction vector (L) and the viewing vector (V).
  vH: TVector3D;  {xmm2}

  vN: TVector3D;  {xmm3}        // surface normal
  vP: TVector3D;  {xmm4}        // position of lighted pixel
  vV: TVector3D;                // viewer direction
{$ifdef PARAM_PHONGSSE}
  LightDestFactor4: TVector3D;  // for multiplication
{$endif}

  //Calculate LdotN and NnH
  NH: Single;
{$ifndef PARAM_PHONGSSE}
  vD: TVector3D;
{$endif}

  Iw, Ic: integer;    // Iw: specular intensity, Ic: ambient+diffuse intensity
  sIw: single;        // floating point value for Iw

  z, LdotN, NnH,
  dist, distfactor, diffuseterm, specularterm: single;
  eLight: TExpandedPixel;
  mc,mcLeft,mcRight,mcTop,mcBottom: TBGRAPixel; ///map values

{$ifdef PARAM_SIMPLECOLOR}
  eColor: TExpandedPixel;
{$else}
  pcolormap: PBGRAPixel;
{$endif}

  {$hints off}
  function ComputePixel(x,y: integer; DiffuseLight, SpecularLight: Word; Alpha: Byte): TBGRAPixel; inline;
  var ec: TExpandedPixel;
    {$ifndef PARAM_SIMPLECOLOR}
      eColor: TExpandedPixel;
    {$endif}
  begin
    {$ifndef PARAM_SIMPLECOLOR}
      eColor := GammaExpansion(pcolormap^);
    {$endif}
    ec.red := (eColor.Red*DiffuseLight+eLight.Red*SpecularLight+PhongLightPrecisionDiv2) shr PhongLightPrecisionSh;
    ec.green := (eColor.Green*DiffuseLight+eLight.Green*SpecularLight+PhongLightPrecisionDiv2) shr PhongLightPrecisionSh;
    ec.blue := (eColor.Blue*DiffuseLight+eLight.Blue*SpecularLight+PhongLightPrecisionDiv2) shr PhongLightPrecisionSh;
    ec.alpha := Alpha shl 8+Alpha;
    result := GammaCompression(ec);
  end;
  {$hints on}

var
  minx,miny,maxx,maxy: integer;
  pmap: PBGRAPixel;
  pdest: PBGRAPixel;
  x,y : integer;      // Coordinates of point in height map.
  vS1,vS2: TVector3D; // surface vectors (plane)

begin
  {$ifndef PARAM_SIMPLECOLOR}
    if (colorMap.Width < map.width) or (colorMap.Height < map.height) then
      raise Exception.Create('Dimension mismatch');
  {$endif}

  if (map.width = 0) or (map.Height = 0) then exit;
  if ofsX >= dest.ClipRect.Right then exit;
  if ofsY >= dest.ClipRect.Bottom then exit;
  if ofsX <= dest.ClipRect.Left-map.Width then exit;
  if ofsY <= dest.ClipRect.Top-map.Height then exit;

  minx := 0;
  miny := 0;
  maxx := map.Width-1;
  maxy := map.Height-1;
  if ofsX < dest.clipRect.Left then minx := dest.clipRect.Left-ofsX;
  if ofsY < dest.clipRect.Top then miny := dest.clipRect.Top-ofsY;
  if OfsX+maxx > dest.ClipRect.Right-1 then maxx := dest.ClipRect.Right-1-ofsX;
  if OfsY+maxy > dest.ClipRect.Bottom-1 then maxy := dest.ClipRect.Bottom-1-ofsY;

  eLight := GammaExpansion(LightColor);
  {$ifdef PARAM_SIMPLECOLOR}
    eColor := GammaExpansion(color);
  {$endif}

  //light origin
  vLS := Vector3D(LightPosition.X-ofsX,
                  LightPosition.Y-ofsY,
                  LightPositionZ);
  {$ifdef PARAM_PHONGSSE}
  asm
    movups xmm1, vLS
  end;
  LightDestFactor4 := Vector3D(LightDestFactor,LightDestFactor,LightDestFactor,LightDestFactor);
  {$endif}

  //surface vectors
  vS1 := Vector3D(1,0,0);
  vS2 := Vector3D(0,1,0);

  vV := Vector3D(0,0,1);


  dist := 0;
  LdotN := 0;
  NnH := 0;

  for y := miny to maxy do
  begin
    //read map values
    pmap := map.ScanLine[y]+minx;
    mc := BGRAPixelTransparent;
    mcRight := pmap^;
    pdest := dest.ScanLine[y+ofsY]+ofsX+minx;
    {$ifndef PARAM_SIMPLECOLOR}
      pcolormap := ColorMap.ScanLine[y];
    {$endif}
    for x := minx to maxx do
    begin
      mcLeft := mc;
      mc := mcRight;
      inc(pmap);
      if x < map.width-1 then
        mcRight := pmap^ else
        mcRight := BGRAPixelTransparent;
      if mc.alpha = 0 then
      begin
        {$ifndef PARAM_SIMPLECOLOR}
        inc(pcolormap);
        {$endif}
        inc(pdest);
        continue;
      end;

      //compute surface vectors
      mcTop := map.GetPixel(x,y-1);
      mcBottom := map.GetPixel(x,y+1);
      z := MapHeight(mc)*mapAltitude;
      if mcLeft.alpha = 0 then
      begin
        if mcRight.alpha = 0 then
          vS1.z := 0
        else
          vS1.z := (MapHeight(mcRight)-MapHeight(mc))*mapAltitude*2;
      end else
      begin
        if mcRight.alpha = 0 then
          vS1.z := (MapHeight(mc)-MapHeight(mcLeft))*mapAltitude*2
        else
          vS1.z := (MapHeight(mcRight)-MapHeight(mcLeft))*mapAltitude;
      end;
      if mcTop.alpha = 0 then
      begin
        if mcBottom.alpha = 0 then
          vS2.z := 0
        else
          vS2.z := (MapHeight(mcBottom)-MapHeight(mc))*mapAltitude*2;
      end else
      begin
        if mcBottom.alpha = 0 then
          vS2.z := (MapHeight(mc)-MapHeight(mcTop))*mapAltitude*2
        else
          vS2.z := (MapHeight(mcBottom)-MapHeight(mcTop))*mapAltitude;
      end;

      //position vector
      vP := Vector3D(x, y, z);
      {$ifdef PARAM_PHONGSSE}
      asm
        //vL := vLS- vP*LightDestFactor;
        movups xmm4, vP
        movups xmm6,LightDestFactor4
        shufps xmm6,xmm6, 0
        mulps xmm6, xmm4
        movaps xmm0, xmm1
        subps xmm0, xmm6

        //normalize(vL);
        movaps xmm6, xmm0   //save
        mulps xmm0, xmm0
        //mix1
        movaps xmm7, xmm0
        shufps xmm7, xmm7, $4e
        addps xmm0, xmm7
        //mix2
        movaps xmm7, xmm0
        shufps xmm7, xmm7, $11
        addps xmm0, xmm7
        //1/sqrt
        rsqrtps xmm0, xmm0
        mulps xmm0, xmm6  //apply

        //vH := vL + vV;
        movaps xmm2, xmm0
        movups xmm6, vV
        addps xmm2, xmm6

        //normalize(vH);
        movaps xmm6, xmm2   //save
        mulps xmm2, xmm2
        //mix1
        movaps xmm7, xmm2
        shufps xmm7, xmm7, $4e
        addps xmm2, xmm7
        //mix2
        movaps xmm7, xmm2
        shufps xmm7, xmm7, $11
        addps xmm2, xmm7
        //1/sqrt
        rsqrtps xmm2, xmm2
        mulps xmm2, xmm6  //apply

        //vectproduct(vS1,vS2,vN);
        movups xmm6, vS1
        shufps xmm6, xmm6, Shift231

        movups xmm7, vS2
        shufps xmm7, xmm7, Shift312

        movaps xmm3,xmm6
        mulps xmm3,xmm7

        movups xmm6, vS1
        shufps xmm6, xmm6, Shift312

        movups xmm7, vS2
        shufps xmm7, xmm7, Shift231

        mulps xmm7,xmm6
        subps xmm3,xmm7
        //end of vectproduct

        //normalize(vN);
        movaps xmm6, xmm3   //save
        mulps xmm3, xmm3
        //mix1
        movaps xmm7, xmm3
        shufps xmm7, xmm7, $4e
        addps xmm3, xmm7
        //mix2
        movaps xmm7, xmm3
        shufps xmm7, xmm7, $11
        addps xmm3, xmm7
        //1/sqrt
        rsqrtps xmm3, xmm3
        mulps xmm3, xmm6  //apply

        //LdotN := vN * vL;
        movups xmm5, xmm3
        mulps xmm5, xmm0
        //mix1
        movaps xmm7, xmm5
        shufps xmm7, xmm7, $4e
        addps xmm5, xmm7
        //mix2
        movaps xmm7, xmm5
        shufps xmm7, xmm7, $11
        addps xmm5, xmm7
        //:=
        movss LdotN, xmm5

        //vD := vLS-vP;
        movaps xmm5, xmm1
        subps xmm5, xmm4

        //dist := sqrt(vD*vD);
        mulps xmm5, xmm5
        //mix1
        movaps xmm7, xmm5
        shufps xmm7, xmm7, $4e
        addps xmm5, xmm7
        //mix2
        movaps xmm7, xmm5
        shufps xmm7, xmm7, $11
        addps xmm5, xmm7
        //:= sqrt
        sqrtss xmm5,xmm5
        movss dist, xmm5

        //NH := vH * vN;
        movups xmm5, xmm2
        mulps xmm5, xmm3
        //mix1
        movaps xmm7, xmm5
        shufps xmm7, xmm7, $4e
        addps xmm5, xmm7
        //mix2
        movaps xmm7, xmm5
        shufps xmm7, xmm7, $11
        addps xmm5, xmm7
        //:=
        movss NH, xmm5
      end;
      {$else}
        vP := Vector3D(x, y, z);
        vL := vLS- vP*LightDestFactor;
        normalize(vL);

        //compute bisector of angle between light and observer
        vH := vL + vV;
        normalize(vH);

        // compute normal vector to the surface
        vectproduct(vS1,vS2,vN);
        normalize(vN);

        //Calculate LdotN and NnH
        LdotN := vN * vL;
        vD := vLS-vP;
        dist := sqrt(vD*vD);

        NH := vH * vN;
        if NH <= 0 then
          NnH := 0
        else
          NnH := exp(SpecularIndex*ln(NH));
      {$endif}

      if NH <= 0 then
        NnH := 0
      else
        NnH := exp(SpecularIndex*ln(NH));

      distfactor := LightSourceIntensity / (dist*LightSourceDistanceFactor + LightSourceDistanceTerm);

      if (LdotN <= 0) then //Point is not illuminated by light source.
        //Use negative diffuse for contrast
        diffuseterm := distfactor * NegativeDiffusionFactor * LdotN
      else
        diffuseterm := distfactor * DiffusionFactor * LdotN;
      Ic := round((AmbientFactor + diffuseterm)*PhongLightPrecision);

      //specular (reflection)
      specularterm := distfactor * SpecularFactor * NnH;
      sIw := specularterm*PhongLightPrecision;
      if sIw > PhongLightPrecision then Iw := PhongLightPrecision else
        Iw := round(sIw);

      //intensity bounds (0..PhongLightPrecision)
      If Ic < 0 then Ic := 0;
      If Ic > PhongLightPrecision then
      begin
        If DiffuseSaturation then
        begin
          Iw := Iw+(Ic-PhongLightPrecision);
          if Iw > PhongLightPrecision then Iw := PhongLightPrecision;
        end;
        Ic := PhongLightPrecision;
      end;
      Ic := Ic*(PhongLightPrecision-Iw) shr PhongLightPrecisionSh;

      DrawPixelInlineWithAlphaCheck(pdest, ComputePixel(x,y,Ic,Iw,mc.alpha));
      {$ifndef PARAM_SIMPLECOLOR}
        inc(pcolormap);
      {$endif}
      inc(pdest); //go to next pixel
    end;
  end;
end;

{$undef PARAM_PHONGSSE}
{$undef PARAM_SIMPLECOLOR}

