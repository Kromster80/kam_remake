unit KM_RenderUI;
interface
uses dglOpenGL,
  {$IFDEF VER140} OpenGL, {$ENDIF}
  {$IFDEF FPC} GL, {$ENDIF}
  Math, KromUtils, KromOGLUtils, SysUtils, KM_CommonTypes, KM_Defaults, Graphics;

type
TRenderUI = class
  public
    constructor Create;
    procedure Write3DButton     (PosX,PosY,SizeX,SizeY,RXid,ID:smallint; State:T3DButtonStateSet; aStyle:TButtonStyle);
    procedure WriteFlatButton   (PosX,PosY,SizeX,SizeY,RXid,ID,TexOffsetX,TexOffsetY,CapOffsetY:smallint; Caption:string; State:TFlatButtonStateSet);
    procedure WriteBevel        (PosX,PosY,SizeX,SizeY:smallint; const HalfContrast:boolean=false);
    procedure WritePercentBar   (PosX,PosY,SizeX,SizeY,Pos:smallint);
    procedure WritePicture      (PosX,PosY,RXid,ID:smallint; Enabled:boolean=true; Highlight:boolean=false); overload;
    procedure WritePicture      (PosX,PosY,SizeX,SizeY,RXid,ID:smallint; Enabled:boolean=true; Highlight:boolean=false); overload;
    procedure WriteRect         (PosX,PosY,SizeX,SizeY,LineWidth:smallint; Col:TColor4);
    procedure WriteLayer        (PosX,PosY,SizeX,SizeY:smallint; Col:TColor4);
    function  WriteText         (PosX,PosY,SizeX:smallint; Text:string; Fnt:TKMFont; Align:KAlign; Wrap:boolean; Color:TColor4):TKMPoint; //Should return text width in px
    procedure RenderMinimap     (PosX,PosY,SizeX,SizeY:smallint);
  end;

var
  fRenderUI: TRenderUI;

implementation
uses KM_Unit1, KM_Terrain, KM_PlayersCollection, KM_Game;


constructor TRenderUI.Create;
begin
  Inherited;
end;


procedure TRenderUI.Write3DButton(PosX,PosY,SizeX,SizeY,RXid,ID:smallint; State:T3DButtonStateSet; aStyle:TButtonStyle);
var a,b:TKMPointF; InsetX,InsetY:single; c1,c2:byte; bRX,bID:word;
begin
  bRX:=4; bID:=402; //4-402 is a stone background
  if aStyle=bsMenu then begin
    bRX:=5; bID:=9; //5-3 is a metal background used in main menu
  end;

  with GFXData[bRX,bID] do
  if PxWidth*PxHeight<>0 then //Incase data wasn't loaded properly
  begin
    a.x := u1 + (u2-u1) * (PosX         - byte(bs_Down in State)) /2/ PxWidth;
    b.x := u1 + (u2-u1) * (PosX + SizeX - byte(bs_Down in State)) /2/ PxWidth;
    a.y := v1 + (v2-v1) * (PosY         - byte(bs_Down in State)) /2/ PxHeight;
    b.y := v1 + (v2-v1) * (PosY + SizeY - byte(bs_Down in State)) /2/ PxHeight;
    a.x:=a.x-(u2-u1)*((PosX+SizeX div 2) div PxWidth )/2; b.x:=b.x-(u2-u1)*((PosX+SizeX div 2) div PxWidth )/2;
    a.y:=a.y-(v2-v1)*((PosY+SizeY div 2) div PxHeight)/2; b.y:=b.y-(v2-v1)*((PosY+SizeY div 2) div PxHeight)/2;
    a.x:=EnsureRange(a.x,u1,u2); b.x:=EnsureRange(b.x,u1,u2);
    a.y:=EnsureRange(a.y,v1,v2); b.y:=EnsureRange(b.y,v1,v2);
  end;

  InsetX:=3/SizeX; //3px
  InsetY:=3/SizeY; //3px

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
      glBindTexture(GL_TEXTURE_2D, GFXData[bRX,bID].TexID);
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


procedure TRenderUI.WriteFlatButton(PosX,PosY,SizeX,SizeY,RXid,ID,TexOffsetX,TexOffsetY,CapOffsetY:smallint; Caption:string; State:TFlatButtonStateSet);
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
      fRenderUI.WriteText(SizeX div 2, (SizeY div 2)+4+CapOffsetY, SizeX, Caption, fnt_Game, kaCenter, false, $FF888888)
    else
      fRenderUI.WriteText(SizeX div 2, (SizeY div 2)+4+CapOffsetY, SizeX, Caption, fnt_Game, kaCenter, false, $FFFFFFFF);

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


procedure TRenderUI.WriteBevel(PosX,PosY,SizeX,SizeY:smallint; const HalfContrast:boolean=false);
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
    glColor4f(1-byte(HalfContrast)/2,1-byte(HalfContrast)/2,1-byte(HalfContrast)/2,1);
    glBegin (GL_LINE_STRIP);
      glvertex2f(SizeX-1,0);
      glvertex2f(SizeX-1,SizeY-1);
      glvertex2f(0,SizeY-1);
    glEnd;
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glColor4f(0,0,0,0.75-byte(HalfContrast)/2);
    glBegin (GL_LINE_STRIP);
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
        if fGame.GameState in [gsPaused, gsRunning] then //Was causing a crash if you went to options on main menu after quitting a mission
          Col:=TeamColors[byte(MyPlayer.PlayerID)]
        else Col:=TeamColors[1]; //Use default player 1
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
var i:single;
begin
  //@Lewin: I know it looks a bit wrong on corners and if coupled with AA turned On, I'll fix it later
  glGetFloatv(GL_LINE_WIDTH,@i); //Memorize
  glLineWidth(LineWidth);
  glColor4ubv(@Col);
  glBegin(GL_LINE_LOOP);
    glkRect(PosX,PosY,PosX+SizeX-1,PosY+SizeY-1);
  glEnd;
  glLineWidth(i); //Restore
end;


{Renders plane with given color}
procedure TRenderUI.WriteLayer(PosX,PosY,SizeX,SizeY:smallint; Col:TColor4);
begin
  glColor4ubv(@Col);
  glBegin(GL_QUADS);
    glkRect(PosX,PosY,PosX+SizeX-1,PosY+SizeY-1);
  glEnd;
  glColor4f(1,1,1,1);
  glBegin(GL_LINE_LOOP);
    glkRect(PosX,PosY,PosX+SizeX-1,PosY+SizeY-1);
  glEnd;
end;


{Renders a line of text and returns text width and height in px}
{By default color must be non-transparent white}
function TRenderUI.WriteText(PosX,PosY,SizeX:smallint; Text:string; Fnt:TKMFont; Align:KAlign; Wrap:boolean; Color:TColor4):TKMPoint;
var
  i:integer;
  InterLetter,LineCount,AdvX,PrevX,LastSpace:integer;
  LineWidth:array[1..256] of word; //Lets hope 256 lines will be enough
begin
  InterLetter := FontData[byte(Fnt)].CharOffset;// CharSpacing[Fnt]; //Spacing between letters, this varies between fonts
  Result.X := 0;
  Result.Y := 0;

  if Wrap then //Reposition EOLs
  begin

    AdvX := 0;
    PrevX := 0;
    LastSpace := -1; //Used as line width

    for i:=1 to length(Text) do //Existing EOLs should be preserved, and new ones added where needed.
      begin
      if (Text[i]=#32) or (Text[i]=#124) then begin
        LastSpace := i;
        PrevX := AdvX;
      end;

      if Text[i]=#32 then
        inc(AdvX, FontData[byte(Fnt)].WordSpacing)
      else
        inc(AdvX, FontData[byte(Fnt)].Letters[ord(Text[i])].Width + InterLetter);

      //This algorithm is not perfect, somehow line width is not within SizeX, but very rare
      if ((AdvX>SizeX)and(LastSpace<>-1))or(Text[i]=#124) then
      begin
        Text[LastSpace] := #124; //Place EOL instead of last whitespace
        dec(AdvX, PrevX); //Should subtract replaced whitespace
      end;
    end;
  end;

  LineCount := 0;
  for i:=1 to length(Text) do begin
    if Text[i]<>#124 then begin
      if Text[i]=#32 then
        Result.X := Result.X+FontData[byte(Fnt)].WordSpacing
      else
        Result.X := Result.X+FontData[byte(Fnt)].Letters[ord(Text[i])].Width+InterLetter;
      Result.Y := Math.max(Result.Y,FontData[byte(Fnt)].Letters[ord(Text[i])].Height);
    end;
    if (Text[i]=#124)or(i=length(Text)) then begin //If EOL or text end
      inc(LineCount);
      Assert(LineCount <= Length(LineWidth), 'Line count exceeded');
      LineWidth[LineCount]:=Math.max(0,Result.X-InterLetter); //Remove last interletter space and negate double EOLs
      Result.X := 0;
    end;
  end;

  for i:=1 to LineCount do
    Result.X := Math.max(Result.X,LineWidth[i]);

  AdvX := 0;
  LineCount := 1;

  glPushMatrix;
    glBindTexture(GL_TEXTURE_2D, FontData[byte(Fnt)].TexID);
    glColor4ubv(@Color);
    glkMoveAALines(false);

    if Align=kaLeft   then glTranslatef(PosX,                      PosY, 0);
    if Align=kaCenter then glTranslatef(PosX - LineWidth[1] div 2, PosY, 0);
    if Align=kaRight  then glTranslatef(PosX - LineWidth[1],       PosY, 0);

    glBegin(GL_QUADS);
      for i:=1 to length(Text) do
      //Switch line if needed
      //Actually KaM uses #124 or vertical bar (|) for new lines in the LIB files,
      //so lets do the same here. Saves complex conversions...
      if Text[i]=#124 then begin
        glEnd;
        inc(LineCount);
        if Align=kaLeft   then glTranslatef(0, Result.Y, 0); //Negate previous line length
        if Align=kaCenter then glTranslatef(-(LineWidth[LineCount]-LineWidth[LineCount-1])div 2, Result.Y, 0);
        if Align=kaRight  then glTranslatef(-LineWidth[LineCount]+LineWidth[LineCount-1], Result.Y, 0);
        AdvX:=0;
        glBegin(GL_QUADS);
      end else
      if Text[i]=#32 then
        inc(AdvX, FontData[byte(Fnt)].WordSpacing)
      else begin
        with FontData[byte(Fnt)].Letters[ord(Text[i])] do begin
          glTexCoord2f(u1,v1); glVertex2f(AdvX       ,0       );
          glTexCoord2f(u2,v1); glVertex2f(AdvX+Width ,0       );
          glTexCoord2f(u2,v2); glVertex2f(AdvX+Width ,0+Height);
          glTexCoord2f(u1,v2); glVertex2f(AdvX       ,0+Height);
        end;
        inc(AdvX, FontData[byte(Fnt)].Letters[ord(Text[i])].Width + InterLetter);
      end;
    glEnd;
    glBindTexture(GL_TEXTURE_2D,0);
  glPopMatrix;
  Result.Y := Result.Y*LineCount;
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
        glColor3ubv(@fTerrain.MM[i,k].R);
        glVertex2f(k,i);
      end;
    glEnd;

  glPopMatrix;
end;

end.
