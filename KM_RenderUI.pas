unit KM_RenderUI;
{$I KaM_Remake.inc}
interface
uses dglOpenGL,
  {$IFDEF FPC} GL, {$ENDIF}
  Math, KromOGLUtils, SysUtils, KM_Defaults, Graphics, KM_Points;

type
  TRenderUI = class
  public
    constructor Create;
    procedure SetupClipX        (X1,X2:smallint);
    procedure SetupClipY        (Y1,Y2:smallint);
    procedure ReleaseClip;
    procedure Write3DButton     (PosX,PosY,SizeX,SizeY,RXid,ID:smallint; State:T3DButtonStateSet; aStyle:TButtonStyle);
    procedure WriteFlatButton   (PosX,PosY,SizeX,SizeY,RXid,ID,TexOffsetX,TexOffsetY,CapOffsetY:smallint; const Caption:string; State:TFlatButtonStateSet);
    procedure WriteBevel        (PosX,PosY,SizeX,SizeY:smallint; HalfBright:boolean=false; BackAlpha:single=0.5);
    procedure WritePercentBar   (PosX,PosY,SizeX,SizeY,Pos:smallint);
    procedure WritePicture      (PosX,PosY,RXid,ID:smallint; Enabled:boolean=true; Highlight:boolean=false); overload;
    procedure WritePicture      (PosX,PosY,SizeX,SizeY,RXid,ID:smallint; Enabled:boolean=true; Highlight:boolean=false); overload;
    procedure WriteRect         (PosX,PosY,SizeX,SizeY,LineWidth:smallint; Col:TColor4);
    procedure WriteLayer        (PosX,PosY,SizeX,SizeY:smallint; Col:TColor4; Outline:TColor4=$FFFFFFFF);
    procedure WriteText         (X,Y,W,H:smallint; Text:string; Fnt:TKMFont; Align:KAlign; Color:TColor4);
    procedure RenderMinimap     (PosX,PosY,SizeX,SizeY:smallint);
  end;

var
  fRenderUI: TRenderUI;

implementation
uses KM_Terrain, KM_PlayersCollection, KM_ResourceGFX, KM_ResourceFonts;


constructor TRenderUI.Create;
begin
  Inherited;
end;


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


procedure TRenderUI.Write3DButton(PosX,PosY,SizeX,SizeY,RXid,ID:smallint; State:T3DButtonStateSet; aStyle:TButtonStyle);
var a,b:TKMPointF; InsetX,InsetY:single; c1,c2:byte; BackRX,BackID:word;
begin
  BackRX:=4; BackID:=402; //4-402 is a stone background
  if aStyle=bsMenu then begin
    BackRX:=5; BackID:=9; //5-3 is a metal background used in main menu
  end;

  with GFXData[BackRX,BackID] do
  if PxWidth*PxHeight<>0 then //Make sure data was loaded properly
  begin
    a.x := u1 + (u2-u1) * (PosX         - byte(bs_Down in State)) /2/ PxWidth;
    b.x := u1 + (u2-u1) * (PosX + SizeX - byte(bs_Down in State)) /2/ PxWidth;
    a.y := v1 + (v2-v1) * (PosY         - byte(bs_Down in State)) /2/ PxHeight;
    b.y := v1 + (v2-v1) * (PosY + SizeY - byte(bs_Down in State)) /2/ PxHeight;
    a.x := a.x-(u2-u1)*((PosX+SizeX div 2) div PxWidth )/2; b.x := b.x-(u2-u1)*((PosX+SizeX div 2) div PxWidth )/2;
    a.y := a.y-(v2-v1)*((PosY+SizeY div 2) div PxHeight)/2; b.y := b.y-(v2-v1)*((PosY+SizeY div 2) div PxHeight)/2;
    a.x := EnsureRange(a.x,u1,u2); b.x := EnsureRange(b.x,u1,u2);
    a.y := EnsureRange(a.y,v1,v2); b.y := EnsureRange(b.y,v1,v2);
  end;

  InsetX := 3/SizeX; //3px
  InsetY := 3/SizeY; //3px

  glPushMatrix;
    glTranslatef(PosX,PosY,0);

    {//Thin black outline outside the button
    //I know, but it's the first thing I'll do when we reach TSK status - add this thin outline, it make buttons look much nicer ;-)
    glColor4f(0,0,0,0.5);
    glBegin (GL_LINE_LOOP);
      glkRect(-1,-1,SizeX,SizeY);
    glEnd;}

    glPushMatrix;
      glkMoveAALines(false);

      //Background
      glColor4f(1,1,1,1);
      glBindTexture(GL_TEXTURE_2D, GFXData[BackRX,BackID].TexID);
      glBegin (GL_QUADS);
        glTexCoord2f(a.x,a.y); glvertex2f(0,0);
        glTexCoord2f(b.x,a.y); glvertex2f(SizeX,0);
        glTexCoord2f(b.x,b.y); glvertex2f(SizeX,SizeY);
        glTexCoord2f(a.x,b.y); glvertex2f(0,SizeY);
      glEnd;

      //Render beveled edges
      glBindTexture(GL_TEXTURE_2D, 0);
      if bs_Down in State then begin
        c1:=0; c2:=1; //Quick way to invert bevel lighting
      end else begin
        c1:=1; c2:=0;
      end;
      glScalef(SizeX,SizeY,0);
      glBegin (GL_QUADS);
        glColor4f(c1,c1,c1,0.7); glkQuad(0, 0, 1,        0,        1-InsetX, 0+InsetY, 0+InsetX, 0+InsetY);
        glColor4f(c1,c1,c1,0.6); glkQuad(0, 0, 0+InsetX, 0+InsetY, 0+InsetX, 1-InsetY, 0,        1       );
        glColor4f(c2,c2,c2,0.5); glkQuad(1, 0, 1,        1,        1-InsetX, 1-InsetY, 1-InsetX, 0+InsetY);
        glColor4f(c2,c2,c2,0.4); glkQuad(0, 1, 0+InsetX, 1-InsetY, 1-InsetX, 1-InsetY, 1,        1       );
      glEnd;
    glPopMatrix;

    //Render a pic ontop
    if ID<>0 then begin
      glColor4f(1,1,1,1);
      WritePicture((SizeX-GFXData[RXid,ID].PxWidth ) div 2 +byte(bs_Down in State),
                   (SizeY-GFXData[RXid,ID].PxHeight) div 2 +byte(bs_Down in State),RXid,ID);
    end;

    //Render highlight
    glkMoveAALines(false);
    if bs_Highlight in State then begin
      glColor4f(1,1,1,0.15);
      glBegin (GL_QUADS);
        glkRect(0,0,SizeX,SizeY);
      glEnd;
    end;

    //Render darklight
    if bs_Disabled in State then begin
      glColor4f(0,0,0,0.5);
      glBegin (GL_QUADS);
        glkRect(0,0,SizeX,SizeY);
      glEnd;
    end;
    glkMoveAALines(true);

  glPopMatrix;
end;


procedure TRenderUI.WriteFlatButton(PosX,PosY,SizeX,SizeY,RXid,ID,TexOffsetX,TexOffsetY,CapOffsetY:smallint; const Caption:string; State:TFlatButtonStateSet);
begin
  glPushMatrix;
    glTranslatef(PosX,PosY,0);

    //Background
    glColor4f(0,0,0,0.5);
    glBegin (GL_QUADS);
      glkRect(0,0,SizeX-1,SizeY-1);
    glEnd;

    //Thin outline rendered on top of background to avoid inset calculations
    glBlendFunc(GL_DST_COLOR,GL_ONE);
    glColor4f(1,1,1,1);
    glBegin (GL_LINE_STRIP);
      glvertex2f(SizeX-1,0);
      glvertex2f(SizeX-1,SizeY-1);
      glvertex2f(0,SizeY-1);
    glEnd;
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glColor4f(0,0,0,0.75);
    glBegin (GL_LINE_STRIP);
      glvertex2f(0,SizeY-1);
      glvertex2f(0,0);
      glvertex2f(SizeX-1,0);
    glEnd;

    if ID<>0 then begin
      TexOffsetY:=TexOffsetY-6*byte(Caption<>'');
      fRenderUI.WritePicture((SizeX-GFXData[RXid,ID].PxWidth) div 2 + TexOffsetX,
                             (SizeY-GFXData[RXid,ID].PxHeight) div 2 + TexOffsetY,RXid,ID, true);
    end;

    if fbs_Disabled in State then
      fRenderUI.WriteText(SizeX div 2, (SizeY div 2)+4+CapOffsetY, SizeX, 0, Caption, fnt_Game, kaCenter, $FF808080)
    else
      fRenderUI.WriteText(SizeX div 2, (SizeY div 2)+4+CapOffsetY, SizeX, 0, Caption, fnt_Game, kaCenter, $FFE0E0E0);

    if fbs_Highlight in State then begin
      glColor4f(1,1,1,0.25);
      glBegin (GL_QUADS);
        glkRect(0,0,SizeX-1,SizeY-1);
      glEnd;
    end;

    {if fbs_Disabled in State then begin
      glColor4f(0,0,0,0.5);
      glBegin (GL_QUADS);
        glkRect(0,0,SizeX-1,SizeY-1);
      glEnd;
    end;}

    if fbs_Selected in State then begin
      glColor4f(1,1,1,1);
      glBegin (GL_LINE_LOOP);
        glkRect(0,0,SizeX-1,SizeY-1);
      glEnd;
    end;

  glPopMatrix;
end;


procedure TRenderUI.WriteBevel(PosX,PosY,SizeX,SizeY:smallint; HalfBright:boolean=false; BackAlpha:single=0.5);
begin
  glPushMatrix;
    glTranslatef(PosX,PosY,0);

    //Background
    glColor4f(0,0,0,BackAlpha);
    glBegin(GL_QUADS);
      glkRect(0,0,SizeX-1,SizeY-1);
    glEnd;

    //Thin outline rendered on top of background to avoid inset calculations
    glBlendFunc(GL_DST_COLOR,GL_ONE);
    glColor4f(1-byte(HalfBright)/2,1-byte(HalfBright)/2,1-byte(HalfBright)/2,1);
    glBegin(GL_LINE_STRIP);
      glvertex2f(SizeX-1,0);
      glvertex2f(SizeX-1,SizeY-1);
      glvertex2f(0,SizeY-1);
    glEnd;
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glColor4f(0,0,0,0.75-byte(HalfBright)/2);
    glBegin(GL_LINE_STRIP);
      glvertex2f(0,SizeY-1);
      glvertex2f(0,0);
      glvertex2f(SizeX-1,0);
    glEnd;
  glPopMatrix;
end;


procedure TRenderUI.WritePercentBar(PosX,PosY,SizeX,SizeY,Pos:smallint);
const BarColor:TColor4=$FF00AA26;
var BarWidth:word;
begin
  glPushMatrix;
    glTranslatef(PosX,PosY,0);

    //Background
    glColor4f(0,0,0,0.5);
    glBegin (GL_QUADS);
      glkRect(1,1,SizeX-1,SizeY-1);
    glEnd;

    //Thin outline rendered on top of background to avoid inset calculations
    glBlendFunc(GL_DST_COLOR,GL_ONE); //Switch BlendFunc, that allows us to make nice beveled edge
    glColor4f(1,1,1,0.5);
    glBegin (GL_LINE_STRIP);
      glvertex2f(SizeX-1,0);
      glvertex2f(SizeX-1,SizeY-1);
      glvertex2f(0,SizeY-1);
    glEnd;
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); //Restore default BlendFunc
    glColor4f(0,0,0,0.5);
    glBegin (GL_LINE_STRIP);
      glvertex2f(0,SizeY-1);
      glvertex2f(0,0);
      glvertex2f(SizeX-1,0);
    glEnd;

    //Draw the bar itself, so long as it is above 0 position
    if Pos > 0 then
    begin
      BarWidth:=round((SizeX-4)*Pos/100);
      glColor4ubv(@BarColor);
      glBegin (GL_QUADS);
        glkRect(1,1,BarWidth+3,SizeY-1);
      glEnd;
      //Draw shadow on top and left of the bar, just like real one
      glColor4f(0,0,0,0.5); //Set semi-transparent black
      glBegin (GL_LINE_STRIP); //List vertices, order is important
        glvertex2f(1,SizeY-2);
        glvertex2f(1,1);
        glvertex2f(BarWidth+3,1);
        glvertex2f(BarWidth+3,2);
        glvertex2f(2,2);
        glvertex2f(2,SizeY-2);
      glEnd;
    end;
    
  glPopMatrix;

end;


procedure TRenderUI.WritePicture(PosX,PosY,RXid,ID:smallint;Enabled:boolean=true; Highlight:boolean=false);
var Col:TColor4;
begin
  if ID<>0 then with GFXData[RXid,ID] do begin
    glBindTexture(GL_TEXTURE_2D,TexID);
    glPushMatrix;
      glkMoveAALines(false);
      glTranslatef(PosX,PosY,0);
      if Enabled then glColor4f(1,1,1,1) else glColor4f(0.33,0.33,0.33,1);
      glBegin(GL_QUADS);
        glTexCoord2f(u1,v1); glVertex2f(0         ,0         );
        glTexCoord2f(u2,v1); glVertex2f(0+PxWidth ,0         );
        glTexCoord2f(u2,v2); glVertex2f(0+PxWidth ,0+PxHeight);
        glTexCoord2f(u1,v2); glVertex2f(0         ,0+PxHeight);
      glEnd;
      if (AltID<>0)and(MyPlayer<>nil) then begin
        glBindTexture(GL_TEXTURE_2D, AltID);
        Col := MyPlayer.FlagColor;
        if Enabled then
          glColor3ub(Col AND $FF, Col SHR 8 AND $FF, Col SHR 16 AND $FF)
        else
          glColor3f(Col AND $FF / 768, Col SHR 8 AND $FF / 768, Col SHR 16 AND $FF / 768);
        glBegin(GL_QUADS);
          glTexCoord2f(u1,v1); glVertex2f(0         ,0         );
          glTexCoord2f(u2,v1); glVertex2f(0+PxWidth ,0         );
          glTexCoord2f(u2,v2); glVertex2f(0+PxWidth ,0+PxHeight);
          glTexCoord2f(u1,v2); glVertex2f(0         ,0+PxHeight);
        glEnd;
      end;
      if Highlight then begin
        glBindTexture(GL_TEXTURE_2D, TexID); //Replace AltID if it was used
        glBlendFunc(GL_DST_COLOR,GL_ONE);
        glColor4f(0.5, 0.5, 0.5, 0.5);
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
  glBindTexture(GL_TEXTURE_2D,0);
end;


{Stretched pic}
procedure TRenderUI.WritePicture(PosX,PosY,SizeX,SizeY,RXid,ID:smallint; Enabled:boolean=true; Highlight:boolean=false);
begin
  if ID<>0 then with GFXData[RXid,ID] do begin
    glBindTexture(GL_TEXTURE_2D,TexID);
    glPushMatrix;
      glkMoveAALines(false);
      glTranslatef(PosX,PosY,0);
      if Enabled then glColor4f(1,1,1,1) else glColor4f(0.33,0.33,0.33,1);
      glBegin(GL_QUADS);
        glTexCoord2f(u1,v1); glVertex2f(0       ,0      );
        glTexCoord2f(u2,v1); glVertex2f(0+SizeX ,0      );
        glTexCoord2f(u2,v2); glVertex2f(0+SizeX ,0+SizeY);
        glTexCoord2f(u1,v2); glVertex2f(0       ,0+SizeY);
      glEnd;
      if Highlight then begin
        glBlendFunc(GL_DST_COLOR,GL_ONE);
        glColor4f(0.5, 0.5, 0.5, 0.5);
        glBegin(GL_QUADS);
          glTexCoord2f(u1,v1); glVertex2f(0       ,0      );
          glTexCoord2f(u2,v1); glVertex2f(0+SizeX ,0      );
          glTexCoord2f(u2,v2); glVertex2f(0+SizeX ,0+SizeY);
          glTexCoord2f(u1,v2); glVertex2f(0       ,0+SizeY);
        glEnd;
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      end;
    glPopMatrix;
  end;
  glBindTexture(GL_TEXTURE_2D,0);
end;


procedure TRenderUI.WriteRect(PosX,PosY,SizeX,SizeY,LineWidth:smallint; Col:TColor4);
begin
  if LineWidth=0 then exit;
  glPushAttrib(GL_LINE_BIT);
    glLineWidth(LineWidth);
    glColor4ubv(@Col);
    glBegin(GL_LINE_LOOP);
      glkRect(PosX,PosY,PosX+SizeX-1,PosY+SizeY-1);
    glEnd;
  glPopAttrib;
end;


{Renders plane with given color}
procedure TRenderUI.WriteLayer(PosX,PosY,SizeX,SizeY:smallint; Col:TColor4; Outline:TColor4=$FFFFFFFF);
begin
  glPushAttrib(GL_LINE_BIT);
  glLineWidth(1);
  glColor4ubv(@Col);
  glBegin(GL_QUADS);
    glkRect(PosX,PosY,PosX+SizeX-1,PosY+SizeY-1);
  glEnd;
  glColor4ubv(@Outline);
  glBegin(GL_LINE_LOOP);
    glkRect(PosX,PosY,PosX+SizeX-1,PosY+SizeY-1);
  glEnd;
  glPopAttrib;
end;


{Renders a line of text}
{By default color must be non-transparent white}
procedure TRenderUI.WriteText(X,Y,W,H:smallint; Text:string; Fnt:TKMFont; Align:KAlign; Color:TColor4);
var
  i:integer;
  LineCount,AdvX,LineHeight,BlockWidth:integer;
  LineWidth:array of integer; //Use signed format since some fonts may have negative CharSpacing
  FD: TKMFontData;
begin
  if (Text = '') or (Color = $00000000) then Exit;

  if W <> 0 then
    case Align of
      kaLeft:   SetupClipX(X, X+W);
      kaCenter: SetupClipX(X-W div 2, X+W);
      kaRight:  SetupClipX(X-W, X+W);
    end;

  FD := fResource.ResourceFont.FontData[Fnt]; //Shortcut

  //Calculate line count and each lines width to be able to properly align them
  LineCount := 1;
  for i:=1 to length(Text) do
    if Text[i]=#124 then inc(LineCount);

  SetLength(LineWidth, LineCount+2); //1..n+1 (for last line)

  LineCount := 1;

  for i:=1 to length(Text) do begin
    if Text[i]<>#124 then
      if Text[i]=#32 then inc(LineWidth[LineCount], FD.WordSpacing)
                     else inc(LineWidth[LineCount], FD.Letters[byte(Text[i])].Width + FD.CharSpacing);
    if (Text[i]=#124)or(i=length(Text)) then begin //If EOL or text end
      LineWidth[LineCount] := Math.max(0, LineWidth[LineCount] - FD.CharSpacing); //Remove last interletter space and negate double EOLs
      inc(LineCount);
    end;
  end;

  LineHeight := FD.Unk1 + FONT_INTERLINE;

  dec(LineCount);
  BlockWidth := 0;
  for i:=1 to LineCount do
    BlockWidth := Math.max(BlockWidth, LineWidth[i]);

  AdvX := 0;
  LineCount := 1;

  glPushMatrix;
    glBindTexture(GL_TEXTURE_2D, FD.TexID);
    glColor4ubv(@Color);
    glkMoveAALines(false);

    case Align of
      kaLeft:   glTranslatef(X,                      Y, 0);
      kaCenter: glTranslatef(X - LineWidth[1] div 2, Y, 0);
      kaRight:  glTranslatef(X - LineWidth[1],       Y, 0);
    end;

    glBegin(GL_QUADS);
      for i:=1 to length(Text) do
      //Switch line if needed
      //Actually KaM uses #124 or vertical bar (|) for new lines in the LIB files,
      //so lets do the same here. Saves complex conversions...
      if Text[i]=#124 then begin
        glEnd;
        inc(LineCount);
        case Align of
          kaLeft:   glTranslatef(0, LineHeight, 0); //Negate previous line length
          kaCenter: glTranslatef(-(LineWidth[LineCount]-LineWidth[LineCount-1]) div 2, LineHeight, 0);
          kaRight:  glTranslatef(-LineWidth[LineCount]+LineWidth[LineCount-1], LineHeight, 0);
        end;
        AdvX := 0;
        glBegin(GL_QUADS);
      end else
      if Text[i]=#32 then
        inc(AdvX, FD.WordSpacing)
      else
      with FD.Letters[byte(Text[i])] do begin
        glTexCoord2f(u1,v1); glVertex2f(AdvX       ,0       +YOffset);
        glTexCoord2f(u2,v1); glVertex2f(AdvX+Width ,0       +YOffset);
        glTexCoord2f(u2,v2); glVertex2f(AdvX+Width ,0+Height+YOffset);
        glTexCoord2f(u1,v2); glVertex2f(AdvX       ,0+Height+YOffset);
        inc(AdvX, Width + FD.CharSpacing);
      end;
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
  glPopMatrix;

  if SHOW_TEXT_OUTLINES then
  begin
    glPushMatrix;
      case Align of
        kaLeft:   glTranslatef(X,                      Y, 0);
        kaCenter: glTranslatef(X - LineWidth[1] div 2, Y, 0);
        kaRight:  glTranslatef(X - LineWidth[1],       Y, 0);
      end;

      glColor4f(1,0,0,0.5);
      glBegin(GL_LINE_LOOP);
        glVertex2f(0         , 0       );
        glVertex2f(BlockWidth, 0       );
        glVertex2f(BlockWidth, LineHeight*LineCount);
        glVertex2f(0         , LineHeight*LineCount);
      glEnd;

      glBegin(GL_LINE_LOOP);
        glVertex2f(0         , 0       );
        glVertex2f(BlockWidth, 0       );
        glVertex2f(BlockWidth, LineHeight);
        glVertex2f(0         , LineHeight);
      glEnd;
    glPopMatrix;
  end;

  ReleaseClip;
end;


procedure TRenderUI.RenderMinimap(PosX,PosY,SizeX,SizeY:smallint);
var i,k:integer; Scale:single;
begin
  glPushMatrix;

    Scale:=1;//min(SizeX/MapX,SizeY/MapY);
    glTranslatef(PosX + round((SizeX-fTerrain.MapX*Scale)/2), PosY + round((SizeY-fTerrain.MapY*Scale)/2),0);
    glkScale(Scale);
    glPointSize(ceil(Scale));

    glBegin(GL_POINTS);
      for i:=1 to fTerrain.MapY-1 do for k:=1 to fTerrain.MapX-1 do begin
        glColor3ubv(@fTerrain.MiniMapRGB[i,k]);
        glVertex2f(k,i);
      end;
    glEnd;

  glPopMatrix;
end;


end.
