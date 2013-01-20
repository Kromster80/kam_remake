unit KM_RenderUI;
{$I KaM_Remake.inc}
interface
uses dglOpenGL, Controls, Graphics, Math, KromOGLUtils, StrUtils, SysUtils,
  KM_Defaults, KM_CommonTypes, KM_Points, KM_Pics, KM_ResourceSprites;

type
  TTextAlign = (taLeft, taCenter, taRight);
  TButtonStateSet = set of (bsOver, bsDown, bsDisabled);
  TButtonStyle = (bsMenu, bsGame); //Menu buttons are metal, game buttons are stone

  //Dont do taps and fit because pixel graphics aren't supposed to be stretched
  //paStretch used only a couple of time when we need to scale large menu elements
  TKMRenderUI = class
  public
    class procedure SetupClipX        (X1,X2: SmallInt);
    class procedure SetupClipY        (Y1,Y2: SmallInt);
    class procedure ReleaseClip;
    class procedure Write3DButton     (PosX,PosY,SizeX,SizeY: SmallInt; aRX: TRXType; aID: Word; aFlagColor: TColor4; State: TButtonStateSet; aStyle: TButtonStyle);
    class procedure WriteBevel        (X, Y, W, H: SmallInt; aEdgeAlpha: Single = 1; aBackAlpha: Single = 0.5);
    class procedure WritePercentBar   (PosX,PosY,SizeX,SizeY:SmallInt; aSeam: Single; aPos: Single);
    class procedure WritePicture      (PosX,PosY,SizeX,SizeY: SmallInt; aAnchors: TAnchors; aRX: TRXType; aID: Word; Enabled: Boolean = True; aColor: TColor4 = $FFFF00FF; aLightness: Single = 0);
    class procedure WritePlot         (PosX,PosY,SizeX,SizeY: SmallInt; aValues: TKMCardinalArray; aMaxValue: Cardinal; aColor: TColor4; LineWidth: Byte);
    class procedure WriteOutline      (PosX,PosY,SizeX,SizeY,LineWidth:smallint; Col:TColor4);
    class procedure WriteShape        (PosX,PosY,SizeX,SizeY:smallint; Col:TColor4; Outline: TColor4 = $00000000);
    class procedure WriteLine         (X1,Y1,X2,Y2: Single; aCol: TColor4; aPattern: Word = $FFFF);
    class procedure WriteText         (X,Y,W: smallint; aText: AnsiString; aFont: TKMFont; aAlign: TTextAlign; aColor: TColor4 = $FFFFFFFF; aIgnoreMarkup:Boolean = False; aShowMarkup:Boolean=False);
    class procedure WriteTexture      (PosX,PosY,SizeX,SizeY:smallint; aTexture: TTexture; aCol: TColor4);
    class procedure WriteCircle       (PosX,PosY: SmallInt; Rad: Byte; aFillCol: TColor4);
  end;


implementation
uses KM_Resource, KM_ResourceFonts;


//X axis uses planes 0,1 and Y axis uses planes 2,3, so that they don't interfere when both axis are
//clipped from both sides
class procedure TKMRenderUI.SetupClipX(X1,X2:smallint);
var cp:array[0..3]of real; //Function uses 8byte floats //ClipPlane X+Y+Z=-D
begin
  glEnable(GL_CLIP_PLANE0);
  glEnable(GL_CLIP_PLANE1);
  FillChar(cp, SizeOf(cp), 0);
  cp[0] := 1; cp[3] := -X1; //Upper edge
  glClipPlane(GL_CLIP_PLANE0, @cp);
  cp[0] := -1; cp[3] := X2; //Lower edge
  glClipPlane(GL_CLIP_PLANE1, @cp);
end;


class procedure TKMRenderUI.SetupClipY(Y1,Y2:smallint);
var cp:array[0..3]of real; //Function uses 8byte floats //ClipPlane X+Y+Z=-D
begin
  glEnable(GL_CLIP_PLANE2);
  glEnable(GL_CLIP_PLANE3);
  FillChar(cp, SizeOf(cp), 0);
  cp[1] := 1; cp[3] := -Y1; //Upper edge
  glClipPlane(GL_CLIP_PLANE2, @cp);
  cp[1] := -1; cp[3] := Y2; //Lower edge
  glClipPlane(GL_CLIP_PLANE3, @cp);
end;


//Release all clipping planes
class procedure TKMRenderUI.ReleaseClip;
begin
  glDisable(GL_CLIP_PLANE0);
  glDisable(GL_CLIP_PLANE1);
  glDisable(GL_CLIP_PLANE2);
  glDisable(GL_CLIP_PLANE3);
end;


class procedure TKMRenderUI.Write3DButton(PosX,PosY,SizeX,SizeY: SmallInt; aRX: TRXType; aID: Word; aFlagColor: TColor4; State: TButtonStateSet; aStyle: TButtonStyle);
var
  Down: Byte;
  Chamfer: Byte;
  A,B: TKMPointF;
  InsetX,InsetY: Single;
  c1,c2: Byte;
  BackRX: TRXType;
  BackID: Word;
begin
  if aStyle = bsMenu then
  begin
    BackRX := rxGuiMain;
    BackID := 9; //GuiMain-3 is a metal background used in main menu
  end else
  begin
    BackRX := rxGui;
    BackID := 402; //Gui-402 is a stone background
  end;

  Down := Byte(bsDown in State);

  with GFXData[BackRX, BackID] do
  with GFXData[BackRX, BackID].Tex do
  if PxWidth * PxHeight <> 0 then //Make sure data was loaded properly
  begin
    A.X := u1 + (u2 - u1) * (PosX - Down) / 2 / PxWidth;
    B.X := u1 + (u2 - u1) * (PosX + SizeX - Down) / 2 / PxWidth;
    A.Y := v1 + (v2 - v1) * (PosY - Down) / 2 / PxHeight;
    B.Y := v1 + (v2 - v1) * (PosY + SizeY - Down) / 2 / PxHeight;
    A.X := A.X - (u2 - u1) * ((PosX + SizeX div 2) div PxWidth) / 2;
    B.X := B.X - (u2 - u1) * ((PosX + SizeX div 2) div PxWidth) / 2;
    A.Y := A.Y - (v2 - v1) * ((PosY + SizeY div 2) div PxHeight) / 2;
    B.Y := B.Y - (v2 - v1) * ((PosY + SizeY div 2) div PxHeight) / 2;
    A.X := EnsureRange(A.X, u1, u2);
    B.X := EnsureRange(B.X, u1, u2);
    A.Y := EnsureRange(A.Y, v1, v2);
    B.Y := EnsureRange(B.Y, v1, v2);
  end;

  glPushMatrix;
    glTranslatef(PosX, PosY, 0);

      //Background
      glColor4f(1, 1, 1, 1);
      glBindTexture(GL_TEXTURE_2D, GFXData[BackRX, BackID].Tex.ID);
      glBegin(GL_QUADS);
        glTexCoord2f(A.x,A.y); glVertex2f(0,0);
        glTexCoord2f(B.x,A.y); glVertex2f(SizeX,0);
        glTexCoord2f(B.x,B.y); glVertex2f(SizeX,SizeY);
        glTexCoord2f(A.x,B.y); glVertex2f(0,SizeY);
      glEnd;

      //Render beveled edges
      glBindTexture(GL_TEXTURE_2D, 0);

      c1 := 1 - Down;
      c2 := Down;
      Chamfer := 2 + Byte(Min(SizeX, SizeY) > 25);

      glPushMatrix;
        //Scale to save on XY+/-Inset coordinates calculations
        glScalef(SizeX, SizeY, 0);
        InsetX := Chamfer / SizeX;
        InsetY := Chamfer / SizeY;
        glBegin(GL_QUADS);
          glColor4f(c1,c1,c1,0.7); glkQuad(0, 0, 1,        0,        1-InsetX, 0+InsetY, 0+InsetX, 0+InsetY);
          glColor4f(c1,c1,c1,0.6); glkQuad(0, 0, 0+InsetX, 0+InsetY, 0+InsetX, 1-InsetY, 0,        1       );
          glColor4f(c2,c2,c2,0.5); glkQuad(1, 0, 1,        1,        1-InsetX, 1-InsetY, 1-InsetX, 0+InsetY);
          glColor4f(c2,c2,c2,0.4); glkQuad(0, 1, 0+InsetX, 1-InsetY, 1-InsetX, 1-InsetY, 1,        1       );
        glEnd;
      glPopMatrix;

    //Render a pic ontop
    if aID <> 0 then
    begin
      glColor4f(1, 1, 1, 1);
      WritePicture(Down, Down, SizeX, SizeY, [], aRX, aID, True, aFlagColor);
    end;

    //Render MouseOver highlight
    if bsOver in State then
    begin
      glColor4f(1, 1, 1, 0.15);
      glBegin(GL_QUADS);
        glkRect(0, 0, SizeX, SizeY);
      glEnd;
    end;

    //Render darklight when Disabled
    if bsDisabled in State then
    begin
      glColor4f(0, 0, 0, 0.5);
      glBegin(GL_QUADS);
        glkRect(0, 0, SizeX, SizeY);
      glEnd;
    end;

  glPopMatrix;
end;


class procedure TKMRenderUI.WriteBevel(X, Y, W, H: SmallInt; aEdgeAlpha: Single = 1; aBackAlpha: Single = 0.5);
begin
  if (W < 0) or (H < 0) then Exit;
  glPushMatrix;
    glTranslatef(X, Y, 0);

    //Background
    glColor4f(0, 0, 0, aBackAlpha);
    glBegin(GL_QUADS);
      glkRect(1, 1, W-1, H-1);
    glEnd;

    //2 Thin outlines rendered on top of background to avoid inset calculations
    if aEdgeAlpha > 0 then
    begin
      //Bright edge
      glBlendFunc(GL_DST_COLOR, GL_ONE);
      glColor3f(0.75 * aEdgeAlpha, 0.75 * aEdgeAlpha, 0.75 * aEdgeAlpha);
      glBegin(GL_LINE_STRIP);
        glVertex2f(W-0.5, 0.5);
        glVertex2f(W-0.5, H-0.5);
        glVertex2f(0.5, H-0.5);
      glEnd;

      //Dark edge
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glColor4f(0, 0, 0, aEdgeAlpha);
      glBegin(GL_LINE_STRIP);
        glVertex2f(0.5, H-0.5);
        glVertex2f(0.5, 0.5);
        glVertex2f(W-0.5, 0.5);
      glEnd;
    end;
  glPopMatrix;
end;


class procedure TKMRenderUI.WritePercentBar(PosX,PosY,SizeX,SizeY:SmallInt; aSeam: Single; aPos: Single);
const
  BAR_COLOR_GREEN: TColor4 = $FF00AA26;
  BAR_COLOR_BLUE: TColor4 = $FFBBAA00;
var
  BarWidth: Word;
begin
  glPushMatrix;
    glTranslatef(PosX, PosY, 0);

    WriteBevel(0, 0, SizeX, SizeY);

    //At least 2px wide to show up from under the shadow
    BarWidth := Round((SizeX - 2) * (aPos)) + 2;
    glColor4ubv(@BAR_COLOR_GREEN);
    glBegin(GL_QUADS);
      glkRect(1, 1, BarWidth-1, SizeY-1);
    glEnd;

    if (aSeam > 0) then
    begin
      //At least 2px wide to show up from under the shadow
      BarWidth := Round((SizeX - 2) * Min(aPos, aSeam)) + 2;
      glColor4ubv(@BAR_COLOR_BLUE);
      glBegin(GL_QUADS);
        glkRect(1, 1, BarWidth-1, SizeY-1);
      glEnd;

      //Skip the seam if it matches high border
      if (aSeam < 1) then
        WriteOutline(Round(aSeam * (SizeX - 2)) + 1, 1, 1, SizeY-2, 1, $FFFFFFFF);
    end;

    //Draw shadow on top and left of the bar, just like real one
    glColor4f(0,0,0,0.5); //Set semi-transparent black
    glBegin(GL_LINE_STRIP); //List vertices, order is important
      glVertex2f(1.5,SizeY-1.5);
      glVertex2f(1.5,1.5);
      glVertex2f(SizeX-1.5,1.5);
      glVertex2f(SizeX-1.5,2.5);
      glVertex2f(2.5,2.5);
      glVertex2f(2.5,SizeY-1.5);
    glEnd;
  glPopMatrix;
end;


class procedure TKMRenderUI.WritePicture(PosX,PosY,SizeX,SizeY: SmallInt; aAnchors: TAnchors; aRX: TRXType; aID: Word; Enabled: Boolean = True; aColor: TColor4 = $FFFF00FF; aLightness: Single = 0);
var
  OffX, OffY: Integer;
  DrawWidth, DrawHeight: Integer;
begin
  if aID = 0 then Exit;

  OffX  := 0;
  OffY  := 0;
  DrawWidth   := GFXData[aRX, aID].PxWidth;
  DrawHeight  := GFXData[aRX, aID].PxHeight;

  //Both aAnchors means that we will need to stretch the image
  if (akLeft in aAnchors) and (akRight in aAnchors) then
    DrawWidth := SizeX
  else
  if akLeft in aAnchors then
    //Use defaults
  else
  if akRight in aAnchors then
    OffX := SizeX - DrawWidth
  else
    //No aAnchors means: draw the image in center
    OffX := (SizeX - DrawWidth) div 2;

  if (akTop in aAnchors) and (akBottom in aAnchors) then
    DrawHeight  := SizeY
  else
  if akTop in aAnchors then
    //Use defaults
  else
  if akBottom in aAnchors then
    OffY := SizeY - DrawHeight
  else
    OffY := (SizeY - DrawHeight) div 2;

  with GFXData[aRX, aID] do
  begin
    glPushMatrix;
      glTranslatef(PosX + OffX, PosY + OffY, 0);

      //Base layer
      glBindTexture(GL_TEXTURE_2D, Tex.ID);
      if Enabled then glColor3f(1,1,1) else glColor3f(0.33,0.33,0.33);
      glBegin(GL_QUADS);
        glTexCoord2f(Tex.u1,Tex.v1); glVertex2f(0            , 0             );
        glTexCoord2f(Tex.u2,Tex.v1); glVertex2f(0 + DrawWidth, 0             );
        glTexCoord2f(Tex.u2,Tex.v2); glVertex2f(0 + DrawWidth, 0 + DrawHeight);
        glTexCoord2f(Tex.u1,Tex.v2); glVertex2f(0            , 0 + DrawHeight);
      glEnd;

      //Color overlay for unit icons and scrolls
      if Alt.ID <> 0 then
      begin
        glBindTexture(GL_TEXTURE_2D, Alt.ID);
        if Enabled then
          glColor3ub(aColor AND $FF, aColor SHR 8 AND $FF, aColor SHR 16 AND $FF)
        else
          glColor3f(aColor AND $FF / 768, aColor SHR 8 AND $FF / 768, aColor SHR 16 AND $FF / 768);
        glBegin(GL_QUADS);
          glTexCoord2f(Alt.u1,Alt.v1); glVertex2f(0            , 0             );
          glTexCoord2f(Alt.u2,Alt.v1); glVertex2f(0 + DrawWidth, 0             );
          glTexCoord2f(Alt.u2,Alt.v2); glVertex2f(0 + DrawWidth, 0 + DrawHeight);
          glTexCoord2f(Alt.u1,Alt.v2); glVertex2f(0            , 0 + DrawHeight);
        glEnd;
      end;

      //Highlight for active/focused/mouseOver images
      if aLightness <> 0 then
      begin
        glBindTexture(GL_TEXTURE_2D, Tex.ID); //Replace AltID if it was used
        if aLightness > 0 then
          glBlendFunc(GL_SRC_ALPHA, GL_ONE)
        else
          glBlendFunc(GL_SRC_ALPHA, GL_ZERO);
        glColor3f(aLightness, aLightness, aLightness);
        glBegin(GL_QUADS);
          glTexCoord2f(Tex.u1,Tex.v1); glVertex2f(0            , 0             );
          glTexCoord2f(Tex.u2,Tex.v1); glVertex2f(0 + DrawWidth, 0             );
          glTexCoord2f(Tex.u2,Tex.v2); glVertex2f(0 + DrawWidth, 0 + DrawHeight);
          glTexCoord2f(Tex.u1,Tex.v2); glVertex2f(0            , 0 + DrawHeight);
        glEnd;
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      end;

    glPopMatrix;
  end;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


class procedure TKMRenderUI.WritePlot(PosX,PosY,SizeX,SizeY: SmallInt; aValues: TKMCardinalArray; aMaxValue: Cardinal; aColor: TColor4; LineWidth: Byte);
var
  I: Integer;
begin
  glPushAttrib(GL_LINE_BIT);
  glPushMatrix;
    //glEnable(GL_LINE_SMOOTH); //Smooth lines actually look odd in KaM
    glTranslatef(PosX, PosY, 0);
    glLineWidth(LineWidth);
    glColor4ubv(@aColor);
    glBegin(GL_LINE_STRIP);
      for I := 0 to High(aValues) do
        glVertex2f(I / High(aValues) * SizeX, SizeY - aValues[I] / aMaxValue * SizeY);
    glEnd;
  glPopAttrib;
  glPopMatrix;
end;


class procedure TKMRenderUI.WriteOutline(PosX,PosY,SizeX,SizeY,LineWidth:smallint; Col:TColor4);
begin
  if LineWidth = 0 then Exit;
  glPushAttrib(GL_LINE_BIT);
    glLineWidth(LineWidth);
    glColor4ubv(@Col);
    glBegin(GL_LINE_LOOP);
      glkRect(PosX+LineWidth/2, PosY+LineWidth/2, PosX+SizeX-LineWidth/2, PosY+SizeY-LineWidth/2);
    glEnd;
  glPopAttrib;
end;


//Renders plane with given color and optional 1px outline
class procedure TKMRenderUI.WriteShape(PosX,PosY,SizeX,SizeY:smallint; Col:TColor4; Outline: TColor4 = $00000000);
begin
  glPushAttrib(GL_LINE_BIT);
    glColor4ubv(@Col);
    glBegin(GL_QUADS);
      glkRect(PosX, PosY, PosX+SizeX, PosY+SizeY);
    glEnd;
    glLineWidth(1);
    glColor4ubv(@Outline);
    glBegin(GL_LINE_LOOP);
      glkRect(PosX+0.5,PosY+0.5,PosX+SizeX-0.5,PosY+SizeY-0.5);
    glEnd;
  glPopAttrib;
end;


class procedure TKMRenderUI.WriteLine(X1,Y1,X2,Y2: Single; aCol: TColor4; aPattern: Word = $FFFF);
begin
  glColor4ubv(@aCol);

  glEnable(GL_LINE_STIPPLE);
  glLineStipple(2, aPattern);

  glBegin(GL_LINES);
    glVertex2f(x1, y1);
    glVertex2f(x2, y2);
  glEnd;
  glDisable(GL_LINE_STIPPLE);
end;


{Renders a line of text}
{By default color must be non-transparent white}
class procedure TKMRenderUI.WriteText(X,Y,W: smallint; aText: AnsiString; aFont: TKMFont; aAlign: TTextAlign; aColor: TColor4 = $FFFFFFFF; aIgnoreMarkup:Boolean = False; aShowMarkup:Boolean = False);
var
  I, K: Integer;
  LineCount,AdvX,AdvY,LineHeight,BlockWidth: Integer;
  LineWidth: array of Integer; //Use signed format since some fonts may have negative CharSpacing
  FontData: TKMFontData;
  TmpColor: Integer;
  Colors: array of record
    FirstChar: Word;
    Color: TColor4;
  end;
begin
  if (aText = '') or (aColor = $00000000) then Exit;

  if W <> 0 then
    SetupClipX(X, X + W);

  //Look for [$FFFFFF][] patterns that markup text color
  I := 0;
  if not aIgnoreMarkup then
  repeat
    I := PosEx('[', aText, I+1);

    //Check for reset
    if (I <> 0) and (I+1 <= Length(aText)) and (aText[I+1] = ']') then
    begin
      SetLength(Colors, Length(Colors) + 1);
      Colors[High(Colors)].FirstChar := I;
      Colors[High(Colors)].Color := 0;
      if not aShowMarkup then Delete(aText, I, 2);
    end;

    //Check for new color
    if (I <> 0) and (I+8 <= Length(aText))
    and (aText[I+1] = '$') and (aText[I+8] = ']')
    and TryStrToInt(Copy(aText, I+1, 7), TmpColor) then
    begin
      SetLength(Colors, Length(Colors) + 1);
      Colors[High(Colors)].FirstChar := I;
      if aShowMarkup then inc(Colors[High(Colors)].FirstChar, 9); //Don't color the markup itself
      Colors[High(Colors)].Color := Abs(TmpColor) or $FF000000;
      if not aShowMarkup then Delete(aText, I, 9);
    end;
  until(I = 0);


  FontData := fResource.ResourceFont.FontData[aFont]; //Shortcut

  //Calculate line count and each lines width to be able to properly aAlign them
  LineCount := 1;
  for I:=1 to length(aText) do
    if aText[I]=#124 then inc(LineCount);

  SetLength(LineWidth, LineCount+2); //1..n+1 (for last line)

  LineCount := 1;

  for I:=1 to length(aText) do
  begin
    if aText[I]<>#124 then
      if aText[I]=#32 then
        inc(LineWidth[LineCount], FontData.WordSpacing)
      else
        inc(LineWidth[LineCount], FontData.Letters[byte(aText[I])].Width + FontData.CharSpacing);

    //If EOL or aText end
    if (aText[I]=#124)or(I=length(aText)) then
    begin
      LineWidth[LineCount] := Math.max(0, LineWidth[LineCount] - FontData.CharSpacing); //Remove last interletter space and negate double EOLs
      inc(LineCount);
    end;
  end;

  LineHeight := FontData.Unk1 + FontData.LineSpacing;

  dec(LineCount);
  BlockWidth := 0;
  for I := 1 to LineCount do
    BlockWidth := Math.max(BlockWidth, LineWidth[I]);

  case aAlign of
    taLeft:   AdvX := X;
    taCenter: AdvX := X + (W - LineWidth[1]) div 2;
    taRight:  AdvX := X + W - LineWidth[1];
  end;
  AdvY := Y;
  LineCount := 1;

  glPushMatrix;
    glBindTexture(GL_TEXTURE_2D, FontData.TexID);
    glColor4ubv(@aColor);

    glBegin(GL_QUADS);
      K := 0;
      for I := 1 to Length(aText) do
      begin

        //Loop as there might be adjoined tags on same position
        while (K < Length(Colors)) and (I = Colors[K].FirstChar) do
        begin
          if Colors[K].Color = 0 then
            glColor4ubv(@aColor)
          else
            glColor4ubv(@Colors[K].Color);
          Inc(K);
        end;

        if aText[I] = #32 then
          Inc(AdvX, FontData.WordSpacing)
        else
        if aText[I] = #124 then
        begin
          //KaM uses #124 or vertical bar (|) for new lines in the LIB files,
          //so lets do the same here. Saves complex conversions...
          Inc(AdvY, LineHeight);
          Inc(LineCount);
          case aAlign of
            taLeft:   AdvX := X;
            taCenter: AdvX := X + (W - LineWidth[LineCount]) div 2;
            taRight:  AdvX := X + W - LineWidth[LineCount];
          end;
        end else
        with FontData.Letters[byte(aText[I])] do
        begin
          glTexCoord2f(u1,v1); glVertex2f(AdvX       ,AdvY       +YOffset);
          glTexCoord2f(u2,v1); glVertex2f(AdvX+Width ,AdvY       +YOffset);
          glTexCoord2f(u2,v2); glVertex2f(AdvX+Width ,AdvY+Height+YOffset);
          glTexCoord2f(u1,v2); glVertex2f(AdvX       ,AdvY+Height+YOffset);
          Inc(AdvX, Width + FontData.CharSpacing);
        end;
      end;
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
  glPopMatrix;

  if SHOW_TEXT_OUTLINES then
  begin
    glPushMatrix;
      case aAlign of
        taLeft:   glTranslatef(X,                          Y, 0);
        taCenter: glTranslatef(X + (W - BlockWidth) div 2, Y, 0);
        taRight:  glTranslatef(X + (W - BlockWidth),       Y, 0);
      end;

      glColor4f(1,0,0,0.5);
      glBegin(GL_LINE_LOOP);
        glVertex2f(0.5           , 0.5       );
        glVertex2f(BlockWidth+0.5, 0.5       );
        glVertex2f(BlockWidth+0.5, LineHeight*LineCount+0.5);
        glVertex2f(0.5           , LineHeight*LineCount+0.5);
      glEnd;

      glBegin(GL_LINE_LOOP);
        glVertex2f(0.5           , 0.5       );
        glVertex2f(BlockWidth+0.5, 0.5       );
        glVertex2f(BlockWidth+0.5, LineHeight+0.5);
        glVertex2f(0.5           , LineHeight+0.5);
      glEnd;
    glPopMatrix;
  end;

  ReleaseClip;
end;


class procedure TKMRenderUI.WriteTexture(PosX,PosY,SizeX,SizeY:smallint; aTexture: TTexture; aCol: TColor4);
begin
  glBindTexture(GL_TEXTURE_2D, aTexture.Tex);

  glColor4ubv(@aCol);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);                   glVertex2f(PosX, PosY);
    glTexCoord2f(aTexture.U, 0);          glVertex2f(PosX+SizeX, PosY);
    glTexCoord2f(aTexture.U, aTexture.V); glVertex2f(PosX+SizeX, PosY+SizeY);
    glTexCoord2f(0, aTexture.V);          glVertex2f(PosX, PosY+SizeY);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, 0);
end;


class procedure TKMRenderUI.WriteCircle(PosX,PosY: SmallInt; Rad: Byte; aFillCol: TColor4);
var
  Ang: Single;
  I: Byte;
begin
  if Rad = 0 then Exit;
  glColor4ubv(@aFillCol);
  glBegin(GL_POLYGON);
    for I := 0 to 15 do
    begin
      Ang := I / 8 * Pi;
      glVertex2f(PosX + Sin(Ang) * Rad, PosY + Cos(Ang) * Rad);
    end;
  glEnd;
end;


end.
