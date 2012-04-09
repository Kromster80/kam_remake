unit KM_RenderUI;
{$I KaM_Remake.inc}
interface
uses dglOpenGL, Graphics, Math, KromOGLUtils, StrUtils, SysUtils,
  KM_Defaults, KM_Controls, KM_Points, KM_Pics, KM_ResourceSprites;

type
  TRenderUI = class
  public
    procedure SetupClipX        (X1,X2:smallint);
    procedure SetupClipY        (Y1,Y2:smallint);
    procedure ReleaseClip;
    procedure Write3DButton     (PosX,PosY,SizeX,SizeY: SmallInt; aRX: TRXType; aID: Word; aFlagColor: TColor4; State: TButtonStateSet; aStyle: TButtonStyle);
    procedure WriteFlatButton   (PosX,PosY,SizeX,SizeY: SmallInt; aRX: TRXType; aID: Word; aColor: TColor4; TexOffsetX,TexOffsetY,CapOffsetY:smallint; const Caption:string; State: TButtonStateSet);
    procedure WriteBevel        (PosX,PosY,SizeX,SizeY:smallint; HalfBright:boolean=false; BackAlpha:single=0.5);
    procedure WritePercentBar   (PosX,PosY,SizeX,SizeY,Pos:smallint);
    procedure WritePicture      (PosX,PosY: SmallInt; aRX: TRXType; aID: Word; aColor: TColor4; Enabled: Boolean = True; Highlight: Boolean = False); overload;
    procedure WritePicture      (PosX,PosY,SizeX,SizeY: SmallInt; aRX: TRXType; aID: Word; Enabled:boolean=true; Highlight:boolean=false); overload;
    procedure WriteRect         (PosX,PosY,SizeX,SizeY,LineWidth:smallint; Col:TColor4);
    procedure WriteLayer        (PosX,PosY,SizeX,SizeY:smallint; Col:TColor4; Outline: TColor4);
    procedure WriteText         (X,Y,W,H: smallint; aText: string; aFont: TKMFont; aAlign: TTextAlign; aColor: TColor4 = $FFFFFFFF; aIgnoreMarkup:Boolean = False; aShowMarkup:Boolean=False);
    procedure WriteTexture      (PosX,PosY,SizeX,SizeY:smallint; aTexture: TTexture; aCol: TColor4);
  end;


implementation
uses KM_Resource, KM_ResourceFonts;


//X axis uses planes 0,1 and Y axis uses planes 2,3, so that they don't interfere when both axis are
//clipped from both sides
procedure TRenderUI.SetupClipX(X1,X2:smallint);
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


procedure TRenderUI.SetupClipY(Y1,Y2:smallint);
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
procedure TRenderUI.ReleaseClip;
begin
  glDisable(GL_CLIP_PLANE0);
  glDisable(GL_CLIP_PLANE1);
  glDisable(GL_CLIP_PLANE2);
  glDisable(GL_CLIP_PLANE3);
end;


procedure TRenderUI.Write3DButton(PosX,PosY,SizeX,SizeY: SmallInt; aRX: TRXType; aID: Word; aFlagColor: TColor4; State: TButtonStateSet; aStyle: TButtonStyle);
var
  a,b:TKMPointF;
  InsetX,InsetY:single;
  c1,c2:byte;
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

  with GFXData[BackRX,BackID] do
  if PxWidth*PxHeight<>0 then //Make sure data was loaded properly
  begin
    a.x := u1 + (u2-u1) * (PosX         - byte(bsDown in State)) /2/ PxWidth;
    b.x := u1 + (u2-u1) * (PosX + SizeX - byte(bsDown in State)) /2/ PxWidth;
    a.y := v1 + (v2-v1) * (PosY         - byte(bsDown in State)) /2/ PxHeight;
    b.y := v1 + (v2-v1) * (PosY + SizeY - byte(bsDown in State)) /2/ PxHeight;
    a.x := a.x-(u2-u1)*((PosX+SizeX div 2) div PxWidth )/2; b.x := b.x-(u2-u1)*((PosX+SizeX div 2) div PxWidth )/2;
    a.y := a.y-(v2-v1)*((PosY+SizeY div 2) div PxHeight)/2; b.y := b.y-(v2-v1)*((PosY+SizeY div 2) div PxHeight)/2;
    a.x := EnsureRange(a.x,u1,u2); b.x := EnsureRange(b.x,u1,u2);
    a.y := EnsureRange(a.y,v1,v2); b.y := EnsureRange(b.y,v1,v2);
  end;

  glPushMatrix;
    glTranslatef(PosX, PosY, 0);

      //Background
      glColor4f(1,1,1,1);
      glBindTexture(GL_TEXTURE_2D, GFXData[BackRX,BackID].TexID);
      glBegin(GL_QUADS);
        glTexCoord2f(a.x,a.y); glVertex2f(0,0);
        glTexCoord2f(b.x,a.y); glVertex2f(SizeX,0);
        glTexCoord2f(b.x,b.y); glVertex2f(SizeX,SizeY);
        glTexCoord2f(a.x,b.y); glVertex2f(0,SizeY);
      glEnd;

      //Render beveled edges
      glBindTexture(GL_TEXTURE_2D, 0);
      if bsDown in State then begin
        c1:=0; c2:=1; //Quick way to invert bevel lighting
      end else begin
        c1:=1; c2:=0;
      end;
      glPushMatrix;
        //Scale to save on XY+/-Inset coordinates calculations
        glScalef(SizeX,SizeY,0);
        InsetX := 3/SizeX; //3px
        InsetY := 3/SizeY; //3px
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
      glColor4f(1,1,1,1);
      WritePicture((SizeX-GFXData[aRX,aID].PxWidth ) div 2 +byte(bsDown in State),
                   (SizeY-GFXData[aRX,aID].PxHeight) div 2 +byte(bsDown in State), aRX, aID, aFlagColor);
    end;

    //Render MouseOver highlight
    if bsOver in State then
    begin
      glColor4f(1,1,1,0.15);
      glBegin(GL_QUADS);
        glkRect(0,0,SizeX,SizeY);
      glEnd;
    end;

    //Render darklight when Disabled
    if bsDisabled in State then
    begin
      glColor4f(0,0,0,0.5);
      glBegin(GL_QUADS);
        glkRect(0,0,SizeX,SizeY);
      glEnd;
    end;

  glPopMatrix;
end;


procedure TRenderUI.WriteFlatButton(PosX,PosY,SizeX,SizeY: SmallInt; aRX: TRXType; aID: Word; aColor: TColor4; TexOffsetX,TexOffsetY,CapOffsetY:smallint; const Caption:string; State: TButtonStateSet);
begin
  WriteBevel(PosX, PosY, SizeX, SizeY);

  glPushMatrix;
    glTranslatef(PosX, PosY, 0);

    if aID <> 0 then
    begin
      TexOffsetY := TexOffsetY - 6 * byte(Caption <> '');
      WritePicture((SizeX-GFXData[aRX,aID].PxWidth) div 2 + TexOffsetX,
                   (SizeY-GFXData[aRX,aID].PxHeight) div 2 + TexOffsetY, aRX, aID, aColor, True);
    end;

    if bsDisabled in State then
      WriteText(SizeX div 2, (SizeY div 2)+4+CapOffsetY, SizeX, 0, Caption, fnt_Game, taCenter, $FF808080)
    else
      WriteText(SizeX div 2, (SizeY div 2)+4+CapOffsetY, SizeX, 0, Caption, fnt_Game, taCenter, $FFE0E0E0);

    if bsOver in State then
    begin
      glColor4f(1,1,1,0.25);
      glBegin(GL_QUADS);
        glkRect(0,0,SizeX-1,SizeY-1);
      glEnd;
    end;

    {if bsDisabled in State then
    begin
      glColor4f(0,0,0,0.5);
      glBegin (GL_QUADS);
        glkRect(0,0,SizeX-1,SizeY-1);
      glEnd;
    end;}

    if bsDown in State then
    begin
      glColor4f(1,1,1,1);
      glBegin(GL_LINE_LOOP);
        glkRect(0.5,0.5,SizeX-0.5,SizeY-0.5);
      glEnd;
    end;

  glPopMatrix;
end;


procedure TRenderUI.WriteBevel(PosX,PosY,SizeX,SizeY:smallint; HalfBright:boolean=false; BackAlpha:single=0.5);
begin
  if (SizeX < 0) or (SizeY < 0) then Exit;
  glPushMatrix;
    glTranslatef(PosX, PosY, 0);

    //Background
    glColor4f(0,0,0,BackAlpha);
    glBegin(GL_QUADS);
      glkRect(1,1,SizeX-1,SizeY-1);
    glEnd;

    //2 Thin outlines rendered on top of background to avoid inset calculations

    //Bright edge
    glBlendFunc(GL_DST_COLOR, GL_ONE);
    glColor3f(1-byte(HalfBright)/2, 1-byte(HalfBright)/2, 1-byte(HalfBright)/2);
    glBegin(GL_LINE_STRIP);
      glVertex2f(SizeX-0.5,0.5);
      glVertex2f(SizeX-0.5,SizeY-0.5);
      glVertex2f(0.5,SizeY-0.5);
    glEnd;

    //Dark edge
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glColor4f(0, 0, 0, 0.75 - byte(HalfBright)/6);
    glBegin(GL_LINE_STRIP);
      glVertex2f(0.5,SizeY-0.5);
      glVertex2f(0.5,0.5);
      glVertex2f(SizeX-0.5,0.5);
    glEnd;
  glPopMatrix;
end;


procedure TRenderUI.WritePercentBar(PosX,PosY,SizeX,SizeY,Pos:smallint);
const BarColor: TColor4=$FF00AA26;
var BarWidth: Word;
begin
  WriteBevel(PosX,PosY,SizeX,SizeY);

  glPushMatrix;
    glTranslatef(PosX, PosY, 0);

    //Draw the bar itself, as long as it is wider than 0
    if Pos > 0 then
    begin
      BarWidth := Round((SizeX - 2) * Pos / 100) + 2; //At least 2px wide to show up from under the shadow
      glColor4ubv(@BarColor);
      glBegin(GL_QUADS);
        glkRect(1, 1, BarWidth-1, SizeY-1);
      glEnd;
      //Draw shadow on top and left of the bar, just like real one
      glColor4f(0,0,0,0.5); //Set semi-transparent black
      glBegin(GL_LINE_STRIP); //List vertices, order is important
        glVertex2f(1.5,SizeY-1.5);
        glVertex2f(1.5,1.5);
        glVertex2f(BarWidth-0.5,1.5);
        glVertex2f(BarWidth-0.5,2.5);
        glVertex2f(2.5,2.5);
        glVertex2f(2.5,SizeY-1.5);
      glEnd;
    end;

  glPopMatrix;
end;


procedure TRenderUI.WritePicture(PosX,PosY: SmallInt; aRX: TRXType; aID: Word; aColor: TColor4; Enabled: Boolean = True; Highlight: Boolean = False);
begin
  if aID = 0 then Exit;

  with GFXData[aRX, aID] do
  begin
    glPushMatrix;
      glTranslatef(PosX, PosY, 0);

      //Base layer
      glBindTexture(GL_TEXTURE_2D, TexID);
      if Enabled then glColor3f(1,1,1) else glColor3f(0.33,0.33,0.33);
      glBegin(GL_QUADS);
        glTexCoord2f(u1,v1); glVertex2f(0        , 0         );
        glTexCoord2f(u2,v1); glVertex2f(0+PxWidth, 0         );
        glTexCoord2f(u2,v2); glVertex2f(0+PxWidth, 0+PxHeight);
        glTexCoord2f(u1,v2); glVertex2f(0        , 0+PxHeight);
      glEnd;

      //Color overlay for unit icons and scrolls
      if AltID <> 0 then
      begin
        glBindTexture(GL_TEXTURE_2D, AltID);
        if Enabled then
          glColor3ub(aColor AND $FF, aColor SHR 8 AND $FF, aColor SHR 16 AND $FF)
        else
          glColor3f(aColor AND $FF / 768, aColor SHR 8 AND $FF / 768, aColor SHR 16 AND $FF / 768);
        glBegin(GL_QUADS);
          glTexCoord2f(u1,v1); glVertex2f(0        , 0         );
          glTexCoord2f(u2,v1); glVertex2f(0+PxWidth, 0         );
          glTexCoord2f(u2,v2); glVertex2f(0+PxWidth, 0+PxHeight);
          glTexCoord2f(u1,v2); glVertex2f(0        , 0+PxHeight);
        glEnd;
      end;

      //Highlight for active/focused/mouseOver images
      if Highlight then
      begin
        glBindTexture(GL_TEXTURE_2D, TexID); //Replace AltID if it was used
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
        glColor3f(0.5, 0.5, 0.5);
        glBegin(GL_QUADS);
          glTexCoord2f(u1,v1); glVertex2f(0         ,0         );
          glTexCoord2f(u2,v1); glVertex2f(0+PxWidth ,0         );
          glTexCoord2f(u2,v2); glVertex2f(0+PxWidth ,0+PxHeight);
          glTexCoord2f(u1,v2); glVertex2f(0         ,0+PxHeight);
        glEnd;
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      end;

    glPopMatrix;
  end;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


{Stretched pic}
procedure TRenderUI.WritePicture(PosX,PosY,SizeX,SizeY: SmallInt; aRX: TRXType; aID: Word; Enabled:boolean=true; Highlight:boolean=false);
begin
  if aID <> 0 then
  with GFXData[aRX, aID] do
  begin
    glBindTexture(GL_TEXTURE_2D, TexID);
    glPushMatrix;
      glTranslatef(PosX, PosY, 0);
      if Enabled then glColor4f(1,1,1,1) else glColor4f(0.33,0.33,0.33,1);

      glBegin(GL_QUADS);
        glTexCoord2f(u1,v1); glVertex2f(0      , 0      );
        glTexCoord2f(u2,v1); glVertex2f(0+SizeX, 0      );
        glTexCoord2f(u2,v2); glVertex2f(0+SizeX, 0+SizeY);
        glTexCoord2f(u1,v2); glVertex2f(0      , 0+SizeY);
      glEnd;

      if Highlight then
      begin
        glBlendFunc(GL_DST_COLOR,GL_ONE);
        glColor4f(0.5, 0.5, 0.5, 0.5);
        glBegin(GL_QUADS);
          glTexCoord2f(u1,v1); glVertex2f(0      , 0      );
          glTexCoord2f(u2,v1); glVertex2f(0+SizeX, 0      );
          glTexCoord2f(u2,v2); glVertex2f(0+SizeX, 0+SizeY);
          glTexCoord2f(u1,v2); glVertex2f(0      , 0+SizeY);
        glEnd;
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      end;
    glPopMatrix;
  end;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


procedure TRenderUI.WriteRect(PosX,PosY,SizeX,SizeY,LineWidth:smallint; Col:TColor4);
begin
  if LineWidth=0 then exit;
  glPushAttrib(GL_LINE_BIT);
    glLineWidth(LineWidth);
    glColor4ubv(@Col);
    glBegin(GL_LINE_LOOP);
      glkRect(PosX+0.5, PosY+0.5, PosX+SizeX-0.5, PosY+SizeY-0.5);
    glEnd;
  glPopAttrib;
end;


{Renders plane with given color}
procedure TRenderUI.WriteLayer(PosX,PosY,SizeX,SizeY:smallint; Col:TColor4; Outline: TColor4);
begin
  glPushAttrib(GL_LINE_BIT);
  glLineWidth(1);
  glColor4ubv(@Col);
  glBegin(GL_QUADS);
    glkRect(PosX,PosY,PosX+SizeX-1,PosY+SizeY-1);
  glEnd;
  glColor4ubv(@Outline);
  glBegin(GL_LINE_LOOP);
    glkRect(PosX+0.5,PosY+0.5,PosX+SizeX-0.5,PosY+SizeY-0.5);
  glEnd;
  glPopAttrib;
end;


{Renders a line of text}
{By default color must be non-transparent white}
procedure TRenderUI.WriteText(X,Y,W,H: smallint; aText: string; aFont: TKMFont; aAlign: TTextAlign; aColor: TColor4 = $FFFFFFFF; aIgnoreMarkup:Boolean = False; aShowMarkup:Boolean = False);
var
  I, K: Integer;
  LineCount,AdvX,LineHeight,BlockWidth: Integer;
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
    case aAlign of
      taLeft:   SetupClipX(X, X+W);
      taCenter: SetupClipX(X-W div 2, X+W div 2);
      taRight:  SetupClipX(X-W, X+W);
    end;

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
  for I:=1 to LineCount do
    BlockWidth := Math.max(BlockWidth, LineWidth[I]);

  AdvX := 0;
  LineCount := 1;

  glPushMatrix;
    glBindTexture(GL_TEXTURE_2D, FontData.TexID);
    glColor4ubv(@aColor);

    case aAlign of
      taLeft:   glTranslatef(X,                      Y, 0);
      taCenter: glTranslatef(X - LineWidth[1] div 2, Y, 0);
      taRight:  glTranslatef(X - LineWidth[1],       Y, 0);
    end;

    glBegin(GL_QUADS);
      K := 0;
      for I:=1 to length(aText) do
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

        //Switch line if needed
        //Actually KaM uses #124 or vertical bar (|) for new lines in the LIB files,
        //so lets do the same here. Saves complex conversions...
        if aText[I]=#124 then begin
          glEnd;
          inc(LineCount);
          case aAlign of
            taLeft:   glTranslatef(0, LineHeight, 0); //Negate previous line length
            taCenter: glTranslatef(-(LineWidth[LineCount]-LineWidth[LineCount-1]) div 2, LineHeight, 0);
            taRight:  glTranslatef(-LineWidth[LineCount]+LineWidth[LineCount-1], LineHeight, 0);
          end;
          AdvX := 0;
          glBegin(GL_QUADS);
        end else
        if aText[I]=#32 then
          inc(AdvX, FontData.WordSpacing)
        else
        with FontData.Letters[byte(aText[I])] do begin
          glTexCoord2f(u1,v1); glVertex2f(AdvX       ,0       +YOffset);
          glTexCoord2f(u2,v1); glVertex2f(AdvX+Width ,0       +YOffset);
          glTexCoord2f(u2,v2); glVertex2f(AdvX+Width ,0+Height+YOffset);
          glTexCoord2f(u1,v2); glVertex2f(AdvX       ,0+Height+YOffset);
          inc(AdvX, Width + FontData.CharSpacing);
        end;
      end;
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
  glPopMatrix;

  if SHOW_TEXT_OUTLINES then
  begin
    glPushMatrix;
      case aAlign of
        taLeft:   glTranslatef(X,                      Y, 0);
        taCenter: glTranslatef(X - LineWidth[1] div 2, Y, 0);
        taRight:  glTranslatef(X - LineWidth[1],       Y, 0);
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


procedure TRenderUI.WriteTexture(PosX,PosY,SizeX,SizeY:smallint; aTexture: TTexture; aCol: TColor4);
begin
  glBindTexture(GL_TEXTURE_2D, aTexture.Tex);

  glColor4ubv(@aCol);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(PosX, PosY);
    glTexCoord2f(aTexture.U, 0); glVertex2f(PosX+SizeX,PosY);
    glTexCoord2f(aTexture.U, aTexture.V); glVertex2f(PosX+SizeX,PosY+SizeY);
    glTexCoord2f(0, aTexture.V); glVertex2f(PosX,PosY+SizeY);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, 0);
end;


end.
