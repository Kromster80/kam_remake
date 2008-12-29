unit KM_RenderUI;
interface
uses dglOpenGL, OpenGL, Math, KromUtils, KromOGLUtils, SysUtils, KM_Defaults, Graphics;

type
TRenderUI = class
  private
  protected
  public
    constructor Create;
    procedure Write3DButton(ID,PosX,PosY,SizeX,SizeY:integer; State:T3DButtonStateSet);
    procedure WriteFlatButton(ID,PosX,PosY,SizeX,SizeY:integer; State:T3DButtonStateSet);
    procedure WritePercentBar(PosX,PosY,SizeX,SizeY,Pos:integer);
    procedure WritePic(ID,PosX,PosY:integer;Enabled:boolean=true);
    procedure WriteLayer(Col:cardinal; PosX,PosY,Width,Height:integer);
    function WriteText(PosX,PosY:integer; Align:KAlign; Text:string; Fnt:TKMFont; Color:TColor=$00000000):TKMPoint; //Should return text width in px
  end;

var
  fRenderUI: TRenderUI;

implementation
uses KM_Unit1, KM_Global_Data;

constructor TRenderUI.Create;
begin
//
end;


procedure TRenderUI.Write3DButton(ID,PosX,PosY,SizeX,SizeY:integer; State:T3DButtonStateSet);
var a,b:TKMPointF; InsetX,InsetY:single; c1,c2:byte;
begin
//402 is a stone background
with GFXData[4,402] do begin
  a.x := u1 + (u2-u1) * (PosX/PxWidth) ;
  b.x := u1 + (u2-u1) * ((PosX+SizeX)/PxWidth) ;
  a.y := v1 + (v2-v1) * (PosY/PxHeight) ;
  b.y := v1 + (v2-v1) * ((PosY+SizeY)/PxHeight) ;
  if PosX+SizeX>PxWidth  then begin a.x:=a.x-(u2-u1); b.x:=b.x-(u2-u1); end;
  if PosY+SizeY>PxHeight then begin a.y:=a.y-(v2-v1); b.y:=b.y-(v2-v1); end;
end;
  InsetX:=3/SizeX; //3px
  InsetY:=3/SizeY; //3px

  glPushMatrix;
    glTranslate(PosX,PosY,0);

    {//Thin black outline outside the button
    //Actually, if you look at KaM there are NO "thin black outlines" on buttons
    //Delete on reading ;)

    glColor4f(0,0,0,0.75);
    glBegin (GL_LINE_LOOP);
      glkRect(-1,-1,SizeX,SizeY);
    glEnd;}

    glPushMatrix;
      glkMoveAALines(false);
      glScale(SizeX,SizeY,0);
      glColor4f(1,1,1,1);
      glBindTexture(GL_TEXTURE_2D, GFXData[4,402].TexID);
      glBegin (GL_QUADS);
        glTexCoord2f(a.x,a.y); glvertex2f(0,0);
        glTexCoord2f(b.x,a.y); glvertex2f(1,0);
        glTexCoord2f(b.x,b.y); glvertex2f(1,1);
        glTexCoord2f(a.x,b.y); glvertex2f(0,1);
      glEnd;
      glBindTexture(GL_TEXTURE_2D, 0);
      if bs_Down in State then begin
        c1:=0; c2:=1;
      end else begin
        c1:=1; c2:=0;
      end;
      glBegin (GL_QUADS); //Render beveled edges
        glColor4f(c1,c1,c1,0.75); glkQuad(0,0, 1,0, 1-InsetX,0+InsetY, 0+InsetX,0+InsetY);
        glColor4f(c1,c1,c1,0.65); glkQuad(0,0, 0+InsetX,0+InsetY, 0+InsetX,1-InsetY, 0,1);
        glColor4f(c2,c2,c2,0.55); glkQuad(1,0, 1,1, 1-InsetX,1-InsetY, 1-InsetX,0+InsetY);
        glColor4f(c2,c2,c2,0.45); glkQuad(0,1, 0+InsetX,1-InsetY, 1-InsetX,1-InsetY, 1,1);
      glEnd;
    glPopMatrix;

    if ID<>0 then begin
      glColor4f(1,1,1,1);
      WritePic(ID,round((SizeX-GFXData[4,ID].PxWidth)/2),
                  round((SizeY-GFXData[4,ID].PxHeight)/2));
    end;

    if bs_Highlight in State then begin
      glColor4f(1,1,1,0.15);
      glBegin (GL_QUADS);
        glkRect(0,0,SizeX-1,SizeY-1);
      glEnd;
    end;

    if bs_Disabled in State then begin
      glColor4f(0,0,0,0.5);
      glBegin (GL_QUADS);
        glkRect(0,0,SizeX-1,SizeY-1);
      glEnd;
    end;

  glPopMatrix;

end;


procedure TRenderUI.WriteFlatButton(ID,PosX,PosY,SizeX,SizeY:integer; State:T3DButtonStateSet);
begin
  glPushMatrix;
    glTranslate(PosX,PosY,0);

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

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    if ID<>0 then begin
      glColor4f(1,1,1,1);
      WritePic(ID,round((SizeX-GFXData[4,ID].PxWidth)/2),
                  round((SizeY-GFXData[4,ID].PxHeight)/2));
    end;

    if bs_Highlight in State then begin
      glColor4f(1,1,1,1);
      glBegin (GL_LINE_LOOP);
        glkRect(0,0,SizeX-1,SizeY-1);
      glEnd;
    end;

    if bs_Disabled in State then begin
      glColor4f(0,0,0,0.5);
      glBegin (GL_QUADS);
        glkRect(0,0,SizeX-1,SizeY-1);
      glEnd;
    end;

  glPopMatrix;

end;


procedure TRenderUI.WritePercentBar(PosX,PosY,SizeX,SizeY,Pos:integer);
var BarWidth:word;
begin
  glPushMatrix;
    glTranslate(PosX,PosY,0);

    //Background
    glColor4f(0,0,0,0.5);
    glBegin (GL_QUADS);
      glkRect(1,1,SizeX-1,SizeY-1);
    glEnd;

    //Thin outline rendered on top of background to avoid inset calculations
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glColor4f(0,0,0,0.5);
    glBegin (GL_LINE_STRIP);
      glvertex2f(SizeX-1,0);
      glvertex2f(SizeX-1,SizeY-1);
      glvertex2f(0,SizeY-1);
    glEnd;
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glColor4f(0,0,0,0.5);
    glBegin (GL_LINE_STRIP);
      glvertex2f(0,SizeY-1);
      glvertex2f(0,0);
      glvertex2f(SizeX-1,0);
    glEnd;

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    BarWidth:=round((SizeX-4)*Pos/100);
      glColor4f(0,0.67,0.15,1);
      glBegin (GL_QUADS);
        glkRect(1,1,BarWidth+3,SizeY-1);
      glEnd;  
    //Draw shadow on top and left of the bar, just like real one
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glColor4f(0,0.37,0.09,1);
    glBegin (GL_LINE_STRIP);
      glvertex2f(1,SizeY-2);
      glvertex2f(1,1);
      glvertex2f(BarWidth+3,1);
    glEnd;
    //@Krom: Repeating above function offset by 1,1 as I don't know how to draw a 2px thick line. Clean up if you like
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glColor4f(0,0.37,0.09,1);
    glBegin (GL_LINE_STRIP);
      glvertex2f(2,SizeY-2);
      glvertex2f(2,2);
      glvertex2f(BarWidth+3,2);
    glEnd;

  glPopMatrix;

end;


procedure TRenderUI.WritePic(ID,PosX,PosY:integer;Enabled:boolean=true);
begin
  //Temporaraly using semi transparency if image disabled, because I don't know how to darken each pixel but not the transperent ones. (like disabled images/buttons in KaM) If you could do that it would be great!
  if Enabled = true then glColor4f(1,1,1,1) else glColor4f(1,1,1,0.4);
  if ID<>0 then with GFXData[4,ID] do begin
    glBindTexture(GL_TEXTURE_2D,TexID);
    glPushMatrix;
      glkMoveAALines(false);
      glTranslate(PosX,PosY,0);
      glBegin(GL_QUADS);
        glTexCoord2f(u1,v1); glVertex2f(0         ,0         );
        glTexCoord2f(u2,v1); glVertex2f(0+PxWidth ,0         );
        glTexCoord2f(u2,v2); glVertex2f(0+PxWidth ,0+PxHeight);
        glTexCoord2f(u1,v2); glVertex2f(0         ,0+PxHeight);
      glEnd;
    glPopMatrix;
  end;
  glBindTexture(GL_TEXTURE_2D,0);
end;


{Renders plane with given color}
procedure TRenderUI.WriteLayer(Col:cardinal; PosX,PosY,Width,Height:integer);
begin
  glColor4ubv(@Col);
  glBegin(GL_QUADS);
    glkRect(PosX,PosY,PosX+Width-1,PosY+Height-1);
  glEnd;
end;


{Renders a line of text and returns text width in px}
function TRenderUI.WriteText(PosX,PosY:integer; Align:KAlign; Text:string; Fnt:TKMFont; Color:TColor=$00000000):TKMPoint;
const InterLetter=1; //Spacing between letters
var i,Num:integer; TextWidth:integer;
begin
  //@Krom: Could you please make it so that all the pixels that are not transperent will be re-rendered with the Color parameter? The alpha will be 00 if the real colors are to be used. (therefore making it transperent) This is required for stuff like Condition on the building page.
  TextWidth:=0;
  Result.Y:=0;
  for i:=1 to length(Text) do
    if Text[i] in [' '..'z'] then begin
      TextWidth:=TextWidth+FontData[byte(Fnt)].Letters[ord(Text[i])].Width+InterLetter;
      Result.Y:=max(Result.Y,FontData[byte(Fnt)].Letters[ord(Text[i])].Height);
    end;

  Result.X:=TextWidth;

  glPushMatrix;
    glkMoveAALines(false);
    if Align=kaLeft   then glTranslate(PosX,PosY,0);
    if Align=kaCenter then glTranslate(PosX-(TextWidth div 2),PosY,0);
    if Align=kaRight  then glTranslate(PosX-TextWidth,PosY,0);
    glColor4f(1,1,1,1);
    //glkScale(2);
    for i:=1 to length(Text) do begin
      Num:=ord(Text[i]);
      glBindTexture(GL_TEXTURE_2D,FontData[byte(Fnt)].TexID);
      glBegin(GL_QUADS);
        with FontData[byte(Fnt)].Letters[Num] do begin
          glTexCoord2f(u1,v1); glVertex2f(0       ,0       );
          glTexCoord2f(u2,v1); glVertex2f(0+Width ,0       );
          glTexCoord2f(u2,v2); glVertex2f(0+Width ,0+Height);
          glTexCoord2f(u1,v2); glVertex2f(0       ,0+Height);
        end;
      glEnd;
      //glCallList(coChar[ EnsureRange(Num-32,0,96) ]);
      glTranslate(FontData[byte(Fnt)].Letters[Num].Width+InterLetter,0,0);
    end;
    glBindTexture(GL_TEXTURE_2D,0);
  glPopMatrix;
end;

end.
 