unit KromOGLUtils;
{$IFDEF VER140} {$DEFINE WDC} {$ENDIF}  // Delphi 6
{$IFDEF VER150} {$DEFINE WDC} {$ENDIF}  // Delphi 7
{$IFDEF VER220} {$DEFINE WDC} {$ENDIF}  // Delphi XE
{$IFDEF VER310} {$DEFINE WDC} {$ENDIF}  // Delphi 10.1 Berlin
interface
uses
  dglOpenGL,
  {$IFDEF FPC} GL, LCLIntf, {$ENDIF}
  {$IFDEF MSWindows} Windows {$ENDIF}
  {$IFDEF Unix} LCLType, glx, x, xlib, xutil {$ENDIF}
  ;

{$IFDEF Unix}
type HGLRC = Integer;
type PFD_DRAW_TO_WINDOW = Integer;
type PFD_SUPPORT_OPENGL = Integer;
type PFD_DOUBLEBUFFER = Integer;
type PFD_TYPE_RGBA = Integer;
type PFD_MAIN_PLANE = Integer;
{$ENDIF}

type
  TColor4 = Cardinal;
  TTexture = record Tex: Cardinal; U, V: Single; end;

  procedure SetRenderFrame(RenderFrame: HWND; out h_DC: HDC; out h_RC: HGLRC);
  procedure SetRenderFrameAA(DummyFrame, RenderFrame: HWND; AntiAliasing: Byte; out h_DC: HDC; out h_RC: HGLRC);

  procedure BuildFont(h_DC: HDC; FontSize: Integer; FontWeight: Word = FW_NORMAL);
  procedure glPrint(const text: AnsiString);
  procedure glkScale(x: Single);
  procedure glkQuad(Ax, Ay, Bx, By, Cx, Cy, Dx, Dy: Single);
  procedure glkRect(Ax, Ay, Bx, By: Single);
  procedure SetupVSync(aVSync: Boolean);


implementation


function SetDCPixelFormat(h_DC: HDC; PixelFormat: Integer): Boolean;
var
  nPixelFormat: Integer;
  PixelDepth: Integer;
  {$IFDEF MSWINDOWS}
  pfd: TPixelFormatDescriptor;
  {$ENDIF}
begin
  PixelDepth := 32; //32bpp is common
  {$IFDEF MSWINDOWS}
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
    cStencilBits    := 8;                    // Turn off stencil buffer
    cAuxBuffers     := 0;                    // Not supported
    iLayerType      := PFD_MAIN_PLANE;       // Ignored
    bReserved       := 0;                    // Number of overlay and underlay planes
    dwLayerMask     := 0;                    // Ignored
    dwVisibleMask   := 0;                    // Transparent color of underlay plane
    dwDamageMask    := 0;                    // Ignored
  end;

  if PixelFormat = 0 then
    nPixelFormat := ChoosePixelFormat(h_DC, @pfd)
  else
    nPixelFormat := PixelFormat;

  if nPixelFormat = 0 then begin
    MessageBox(0, 'Unable to find a suitable pixel format', 'Error', MB_OK or MB_ICONERROR);
    Result := false;
    Exit;
  end;

  //Even with known pixel format we still need to supply some PFD structure
  if not SetPixelFormat(h_DC, nPixelFormat, @pfd) then begin
    MessageBox(0, 'Unable to set the pixel format', 'Error', MB_OK or MB_ICONERROR);
    Result := false;
    Exit;
  end;

  Result := true;
  {$ENDIF}
  //glXChooseVisual() should be on Uni insted of ChoosePixelFormatx
end;


function GetMultisamplePixelFormat(h_DC: HDC; AntiAliasing: Byte): Integer;
var
  pixelFormat: Integer;
  ValidFormat: Boolean;
  NumFormats: GLUint;
  iAttributes: array of GLint;
begin
  Result := 0;
  {$IFDEF MSWINDOWS}
  if not WGL_ARB_multisample or not Assigned(wglChoosePixelFormatARB) then
    Exit;
 {$ENDIF}
  SetLength(iAttributes,21);
  iAttributes[0] := WGL_DRAW_TO_WINDOW_ARB;
  iAttributes[1] := 1;
  iAttributes[2] := WGL_SUPPORT_OPENGL_ARB;
  iAttributes[3] := 1;
  iAttributes[4] := WGL_ACCELERATION_ARB;
  iAttributes[5] := WGL_FULL_ACCELERATION_ARB;
  iAttributes[6] := WGL_COLOR_BITS_ARB;
  iAttributes[7] := 24;
  iAttributes[8] := WGL_ALPHA_BITS_ARB;
  iAttributes[9] := 8;
  iAttributes[10] := WGL_DEPTH_BITS_ARB;
  iAttributes[11] := 16;
  iAttributes[12] := WGL_STENCIL_BITS_ARB;
  iAttributes[13] := 0;
  iAttributes[14] := WGL_DOUBLE_BUFFER_ARB;
  iAttributes[15] := 1;
  iAttributes[16] := WGL_SAMPLE_BUFFERS_ARB;
  iAttributes[17] := 1;
  iAttributes[18] := WGL_SAMPLES_ARB;
  iAttributes[19] := AntiAliasing;
  iAttributes[20] := 0;

  //Try to find mode with slightly worse AA before giving up
  repeat
    iAttributes[19] := AntiAliasing;
    {$IFDEF MSWINDOWS}
    ValidFormat := wglChoosePixelFormatARB(h_dc, @iAttributes[0], nil, 1, @pixelFormat, @NumFormats);
    {$ENDIF}
    if ValidFormat and (NumFormats >= 1) then
    begin
      Result := pixelFormat;
      Exit;
    end;
    AntiAliasing := AntiAliasing div 2;
  until(AntiAliasing < 2);
end;


procedure SetContexts(RenderFrame: HWND; PixelFormat: Integer; out h_DC: HDC; out h_RC: HGLRC);
begin
  {$IFDEF MSWINDOWS}
  h_DC := GetDC(RenderFrame);

  if h_DC = 0 then
  begin
    MessageBox(HWND(nil), 'Unable to get a device context', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  if not SetDCPixelFormat(h_DC, PixelFormat) then
    Exit;

  h_RC := wglCreateContext(h_DC);
  if h_RC = 0 then
  begin
    MessageBox(HWND(nil), 'Unable to create an OpenGL rendering context', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  if not wglMakeCurrent(h_DC, h_RC) then

  begin
    MessageBox(HWND(nil), 'Unable to activate OpenGL rendering context', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;
  {$ENDIF}
end;


procedure SetRenderFrame(RenderFrame: HWND; out h_DC: HDC; out h_RC: HGLRC);
begin
  InitOpenGL;
  SetContexts(RenderFrame, 0, h_DC, h_RC);
  ReadExtensions;
  ReadImplementationProperties;
end;


{The key problem is this: the function we use to get WGL extensions is, itself, an OpenGL extension.
Thus like any OpenGL function, it requires an OpenGL context to call it. So in order to get the
functions we need to create a context, we have to... create a context.

Fortunately, this context does not need to be our final context. All we need to do is create a dummy
context to get function pointers, then use those functions directly. Unfortunately, Windows does not
allow recreation of a rendering context within a single HWND. We must destroy previous HWND context
and create final HWND context after we are finished with the dummy context.}
procedure SetRenderFrameAA(DummyFrame, RenderFrame: HWND; AntiAliasing: Byte; out h_DC: HDC; out h_RC: HGLRC);
var
  PixelFormat: Integer;
begin
  InitOpenGL;
  SetContexts(DummyFrame, 0, h_DC, h_RC);
  ReadExtensions;
  ReadImplementationProperties;

  PixelFormat := GetMultisamplePixelFormat(h_DC, AntiAliasing);
  {$IFDEF MSWINDOWS}
  wglMakeCurrent(h_DC, 0);
  wglDeleteContext(h_RC);
  {$ENDIF}
  SetContexts(RenderFrame, PixelFormat, h_DC, h_RC);
  ReadExtensions;
  ReadImplementationProperties;
end;


procedure BuildFont(h_DC: HDC; FontSize: Integer; FontWeight: Word = FW_NORMAL);
var
  Font: HFONT;
begin
//New parameter FontSize=16
  font:=CreateFont(-abs(FontSize),0,0,0,FontWeight,0,0,0,ANSI_CHARSET,
  OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,
  ANTIALIASED_QUALITY,FF_DONTCARE or DEFAULT_PITCH,
  'Terminal');
  SelectObject(h_dc,font);
  {$IFDEF MSWindows}
  wglUseFontBitmaps(h_dc,0,128,20000);
  {$ENDIF}
  {$IFDEF Unix}
  //TODO need to emulate these functions (with freetype you should be able to code a multi platform solution).
  {$ENDIF}
end;


procedure glPrint(const text: AnsiString);
begin
  if text = '' then Exit;
  glPushAttrib(GL_LIST_BIT);
  glListBase(20000);
  glCallLists(length(text),GL_UNSIGNED_BYTE,Pchar(@text[1]));
  glPopAttrib;
end;


procedure glkScale(x: Single);
begin
  glScalef(x, x, x);
end;


procedure glkQuad(Ax, Ay, Bx, By, Cx, Cy, Dx, Dy: Single);
begin
  glvertex2f(Ax, Ay);
  glvertex2f(Bx, By);
  glvertex2f(Cx, Cy);
  glvertex2f(Dx, Dy);
end;


//Same as glkQuad, but requires on TopLeft and BottomRight coords
procedure glkRect(Ax, Ay, Bx, By: Single);
begin
  glvertex2f(Ax, Ay);
  glvertex2f(Bx, Ay);
  glvertex2f(Bx, By);
  glvertex2f(Ax, By);
end;


//This function is used to enable/disable V-Sync
procedure SetupVSync(aVSync: Boolean);
begin
  {$IFDEF MSWindows}
  if WGL_EXT_swap_control then
    wglSwapIntervalEXT(byte(aVSync));
  {$ENDIF}
end;


end.
