unit KM_RenderUI;
interface
uses dglOpenGL, OpenGL, KromUtils, KromOGLUtils, SysUtils, KM_Defaults;

type
TRenderUI = class
  private
  protected
  public
    constructor Create;
    procedure Write3DButton(ID,PosX,PosY,SizeX,SizeY:integer; State:T3DButtonStateSet);
    procedure WriteFlatButton(ID,PosX,PosY,SizeX,SizeY:integer; State:T3DButtonStateSet);
    procedure WritePic(ID,PosX,PosY:integer);
    function WriteText(PosX,PosY:integer; Align:KAlign; Text:string; Fnt:TKMFont):integer; //Should return text width in px
  end;

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
  InsetX:=3/SizeX; //4px
  InsetY:=3/SizeY; //4px

  glPushMatrix;
    glTranslate(PosX,PosY,0);

    //Thin black outline
    glColor4f(0,0,0,0.5);
    glBegin (GL_LINE_LOOP);
      glkQuad(0,0,SizeX,0,SizeX,SizeY,0,SizeY);
    glEnd;

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
    WritePic(ID,round(+(SizeX-GFXData[4,ID].PxWidth)/2),
                round(+(SizeY-GFXData[4,ID].PxHeight)/2));
  end;

  if bs_Highlight in State then begin
    glColor4f(1,1,1,0.15);
    glBegin (GL_QUADS);
      glkQuad(0,0,SizeX,0,SizeX,SizeY,0,SizeY);
    glEnd;
  end;

  if bs_Disabled in State then begin
    glColor4f(0.5,0.5,0.5,0.5);
    glBegin (GL_QUADS);
      glkQuad(0,0,SizeX,0,SizeX,SizeY,0,SizeY);
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
      glkQuad(0,0,SizeX,0,SizeX,SizeY,0,SizeY);
    glEnd;

    //Thin outline rendered on top of background to avoid inset calculations
    glBlendFunc(GL_DST_COLOR,GL_ONE);
    glBegin (GL_LINE_STRIP);
      glColor4f(1,1,1,1);
      glvertex2f(SizeX,0);
      glvertex2f(SizeX,SizeY);
      glvertex2f(0,SizeY);
      glColor4f(0,0,0,1);
      glvertex2f(0,SizeY);
      glvertex2f(0,0);
      glvertex2f(SizeX,0);
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
        glkQuad(0,0,SizeX,0,SizeX,SizeY,0,SizeY);
      glEnd;
    end;

    if bs_Disabled in State then begin
      glColor4f(0,0,0,0.5);
      glBegin (GL_QUADS);
        glkQuad(0,0,SizeX,0,SizeX,SizeY,0,SizeY);
      glEnd;
    end;

  glPopMatrix;

end;


procedure TRenderUI.WritePic(ID,PosX,PosY:integer);
begin                          
  glColor4f(1,1,1,1);
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

{Renders a line of text and returns text width in px}
function TRenderUI.WriteText(PosX,PosY:integer; Align:KAlign; Text:string; Fnt:TKMFont):integer;
var i,Num:integer; TextWidth:single;
begin
  TextWidth:=0;
  for i:=1 to length(Text) do
    if Text[i] in [' '..'z'] then
    TextWidth:=TextWidth+FontData[byte(Fnt)].Letters[ord(Text[i])].Width+1.5;
  Result:=round(TextWidth);

  glColor4f(1,1,1,1);

  glPushMatrix;
  glkMoveAALines(false);
  if Align=kaLeft   then glTranslate(PosX,PosY,0);
  if Align=kaCenter then glTranslate(PosX-TextWidth/2,PosY,0);
  if Align=kaRight  then glTranslate(PosX-TextWidth,PosY,0);

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
    glTranslate(FontData[byte(Fnt)].Letters[Num].Width+1,0,0);
  end;
  glBindTexture(GL_TEXTURE_2D,0);
  glPopMatrix;
end;

end.
 