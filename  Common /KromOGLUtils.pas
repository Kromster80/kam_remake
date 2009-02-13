unit KromOGLUtils;
interface
uses dglOpenGL, OpenGL, sysutils, windows, Forms;

type KCode = (kNil=0,kPoint=1,kSpline=2,kSplineAnchor=3,kSplineAnchorLength=4,
kPoly=5,kSurface=6,kObject=7,kButton=8);  //1..31 are ok
type Vector = record x,y,z:single; end;

KAlign = (kaLeft, kaCenter, kaRight);

TColor4 = cardinal;

procedure SetRenderFrame(const RenderFrame:HWND; out h_DC: HDC; out h_RC: HGLRC);
procedure SetRenderDefaults();
function SetDCPixelFormat(h_DC:HDC):boolean;
procedure CheckGLSLError(FormHandle:hWND; Handle: GLhandleARB; Param: GLenum; ShowWarnings:boolean; Text:string);
procedure BuildFont(h_DC:HDC;FontSize:integer);
procedure glPrint(text: string);
function ReadClick(X, Y: word): Vector;
procedure glkScale(x:single);
procedure glkQuad(Ax,Ay,Bx,By,Cx,Cy,Dx,Dy:single);
procedure glkRect(Ax,Ay,Bx,By:single);
procedure glkMoveAALines(DoShift:boolean);
procedure kSetColorCode(TypeOfValue:KCode;IndexNum:integer);
procedure kGetColorCode(RGBColor:Pointer;var TypeOfValue:KCode;var IndexNum:integer);

const
MatModeDefaultV:string=
'varying vec3 kBlend;'+#10+#13+
'void main(void)'+#10+#13+
'{ '+#10+#13+
'kBlend = gl_SecondaryColor.rgb;'+#10+#13+
'gl_Position = ftransform();'+#10+#13+
'}';

MatModeDefaultF:string=
'varying vec3 kBlend;'+#10+#13+
'void main(void)'+#10+#13+
'{ '+#10+#13+
'vec3 kColor = smoothstep(0.4375,.5625,kBlend.rgb);'+#10+#13+
'gl_FragColor = vec4(kColor.rgb,1);'+#10+#13+
'}';

implementation

procedure SetRenderFrame(const RenderFrame:HWND; out h_DC: HDC; out h_RC: HGLRC);
begin
  InitOpenGL;
  h_DC := GetDC(RenderFrame);
  if h_DC=0 then
  begin
    MessageBox(Application.Handle, 'Unable to get a device context', 'Error', MB_OK or MB_ICONERROR);
    exit;
  end;
  if not SetDCPixelFormat(h_DC) then
    exit;
  h_RC := wglCreateContext(h_DC);
  if h_RC=0 then
  begin
    MessageBox(Application.Handle, 'Unable to create an OpenGL rendering context', 'Error', MB_OK or MB_ICONERROR);
    exit;
  end;
  if not wglMakeCurrent(h_DC, h_RC) then
  begin
    MessageBox(Application.Handle, 'Unable to activate OpenGL rendering context', 'Error', MB_OK or MB_ICONERROR);
    exit;
  end;
  ReadExtensions;
  ReadImplementationProperties;
end;

procedure SetRenderDefaults();
begin
  glClearColor(0, 0, 0, 0); 	   //Background
  glClear (GL_COLOR_BUFFER_BIT);
  glShadeModel(GL_SMOOTH);                 //Enables Smooth Color Shading
  glPolygonMode(GL_FRONT,GL_FILL);
  glEnable(GL_NORMALIZE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); //Set alpha mode
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_COLOR_MATERIAL);                 //Enable Materials
  glEnable(GL_TEXTURE_2D);                     // Enable Texture Mapping
end;

function SetDCPixelFormat(h_DC:HDC):boolean;
var
  nPixelFormat: Integer;
  PixelDepth:integer;
  pfd: TPixelFormatDescriptor;
begin
PixelDepth:=32;
  with pfd do begin
    nSize           := SizeOf(TPIXELFORMATDESCRIPTOR); // Size Of This Pixel Format Descriptor
    nVersion        := 1;                    // The version of this data structure
    dwFlags         := PFD_DRAW_TO_WINDOW    // Buffer supports drawing to window
                       or PFD_SUPPORT_OPENGL // Buffer supports OpenGL drawing
                       or PFD_DOUBLEBUFFER;  // Supports double buffering
    iPixelType      := PFD_TYPE_RGBA;        // RGBA color format
    cColorBits      := PixelDepth;           // OpenGL color depth
    cRedBits        := 0;                    // Number of red bitplanes
    cRedShift       := 0;                    // Shift count for red bitplanes
    cGreenBits      := 0;                    // Number of green bitplanes
    cGreenShift     := 0;                    // Shift count for green bitplanes
    cBlueBits       := 0;                    // Number of blue bitplanes
    cBlueShift      := 0;                    // Shift count for blue bitplanes
    cAlphaBits      := 0;                    // Not supported
    cAlphaShift     := 0;                    // Not supported
    cAccumBits      := 0;                    // No accumulation buffer
    cAccumRedBits   := 0;                    // Number of red bits in a-buffer
    cAccumGreenBits := 0;                    // Number of green bits in a-buffer
    cAccumBlueBits  := 0;                    // Number of blue bits in a-buffer
    cAccumAlphaBits := 0;                    // Number of alpha bits in a-buffer
    cDepthBits      := 16;                   // Specifies the depth of the depth buffer
    cStencilBits    := 0;                    // Turn off stencil buffer
    cAuxBuffers     := 0;                    // Not supported
    iLayerType      := PFD_MAIN_PLANE;       // Ignored
    bReserved       := 0;                    // Number of overlay and underlay planes
    dwLayerMask     := 0;                    // Ignored
    dwVisibleMask   := 0;                    // Transparent color of underlay plane
    dwDamageMask    := 0;                     // Ignored
  end;
  nPixelFormat:=ChoosePixelFormat(h_DC, @pfd);
  if nPixelFormat=0 then begin
  MessageBox(0, 'Unable to find a suitable pixel format', 'Error', MB_OK or MB_ICONERROR);
  Result:=false; exit; end;
  if not SetPixelFormat(h_DC, nPixelFormat, @pfd) then begin
  MessageBox(0, 'Unable to set the pixel format', 'Error', MB_OK or MB_ICONERROR);
  Result:=false; exit;
  end;
Result:=true;
end;

procedure CheckGLSLError(FormHandle:hWND; Handle: GLhandleARB; Param: GLenum; ShowWarnings:boolean; Text:string);
var l,glsl_ok:GLint; s:PChar; i:integer; ShowMessage:boolean;
begin
  glGetObjectParameterivARB(Handle, Param, @glsl_ok);
  s := StrAlloc(1000);
  glGetInfoLogARB(Handle, StrBufSize(s), l, PGLcharARB(s));
//Intent to hide all Warning messages
  ShowMessage:=ShowWarnings;
  for i:=1 to length(s) do
  if (s[i]=#13)and(i+1<length(s)) then
  if (s[i+1]<>'W')and(s[i+1]<>'L')and(not ShowMessage) then ShowMessage:=true;
if ShowMessage then
if s[0]<>'' then MessageBox(FormHandle, @(Text+s)[1],'GLSL Log', MB_OK);
end;

procedure BuildFont(h_DC:HDC;FontSize:integer);
var Font: HFONT;
begin
//New parameter FontSize=16
  font:=CreateFont(-abs(FontSize),0,0,0,FW_NORMAL,0,0,0,ANSI_CHARSET,
  OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,
  ANTIALIASED_QUALITY,FF_DONTCARE or DEFAULT_PITCH,
  'Terminal');
  SelectObject(h_dc,font);
  wglUseFontBitmaps(h_dc,0,128,20000);
end;

procedure glPrint(text: string);
begin
  if text = '' then exit;
  glPushAttrib(GL_LIST_BIT);
  glListBase(20000);
  glCallLists(length(text),GL_UNSIGNED_BYTE,Pchar(text));
  glPopAttrib;
end;

function ReadClick(X, Y: word): Vector;
var viewport:array[0..3]of GLInt;
    projection:array[0..15]of GLDouble;
    modelview:array[0..15]of GLdouble;
    vx,vy:integer;
    vz:single; //required to match GL_FLOAT - single
    wx,wy,wz:GLdouble;
begin
  glGetIntegerv(GL_VIEWPORT,@viewport);
  glGetDoublev(GL_PROJECTION_MATRIX,@projection);
  glGetDoublev(GL_MODELVIEW_MATRIX,@modelview);

  vx := x;
  vy := y;

  glReadPixels(vx, vy, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @vz);

  if vz=1 then begin
  Result.x:=0;
  Result.y:=0;
  Result.z:=0;
  end else begin
  //This function uses OpenGL parameters, not dglOpenGL
  gluUnProject(vx, vy, vz, @modelview, @projection, @viewport, wx, wy, wz);
  Result.x:=wx;
  Result.y:=wy;
  Result.z:=wz;
  end;
end;

procedure kSetColorCode(TypeOfValue:KCode;IndexNum:integer);
begin
glColor4ub(IndexNum mod 256,
          (IndexNum mod 65536) div 256,    // 1,2,4(524288) 8,16,32,64,128 //0..31
          (IndexNum mod 524288) div 65536+byte(TypeOfValue)*8,255);
end;

procedure kGetColorCode(RGBColor:Pointer;var TypeOfValue:KCode;var IndexNum:integer);
begin
IndexNum:=pword(integer(RGBColor))^+((pbyte(integer(RGBColor)+2)^)mod 8)*65536;
TypeOfValue:=KCode((pbyte(integer(RGBColor)+2)^)div 8);
end;

procedure glkScale(x:single);
begin
  glScale(x,x,x);
end;

procedure glkQuad(Ax,Ay,Bx,By,Cx,Cy,Dx,Dy:single);
begin
  glvertex2f(Ax,Ay);
  glvertex2f(Bx,By);
  glvertex2f(Cx,Cy);
  glvertex2f(Dx,Dy);
end;

{Same as glkQuad, but requires on TopLeft and BottomRight coords}
procedure glkRect(Ax,Ay,Bx,By:single);
begin
  glvertex2f(Ax,Ay);
  glvertex2f(Bx,Ay);
  glvertex2f(Bx,By);
  glvertex2f(Ax,By);
end;

{Lines are drawn between pixels, thus when AA turned on they get blurred.
We can negate this by using 0.5 offset
Still it's unclear if that works on all GPUs the same..}
procedure glkMoveAALines(DoShift:boolean);
const Value=0.5;
begin
if DoShift then
  glTranslate(Value,Value,0)
else
  glTranslate(-Value,-Value,0);
end;



end.
