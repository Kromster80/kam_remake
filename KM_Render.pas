unit KM_Render;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  dglOpenGL, KromOGLUtils, KromUtils, Math, KM_TGATexture;

type
  TCardinalArray = array of Cardinal;
  TTexFormat = (tf_Normal, tf_NormalAlpha, tf_AltID, tf_AlphaTest);

  TRenderMode = (rm2D, rm3D);

  //General OpenGL handling
  TRender = class
  private
    h_DC: HDC;
    h_RC: HGLRC;
    fOpenGL_Vendor, fOpenGL_Renderer, fOpenGL_Version: AnsiString;
    fScreenX, fScreenY: Word;
  public
    constructor Create(RenderFrame: HWND; ScreenX,ScreenY: Integer; aVSync: Boolean);
    destructor Destroy; override;

    procedure SetRenderMode(aRenderMode: TRenderMode); //Switch between 2D and 3D perspectives

    function GenTexture(DestX, DestY: Word; const Data: TCardinalArray; Mode: TTexFormat): GLUint;

    property RendererVersion: AnsiString read fOpenGL_Version;
    procedure Resize(Width,Height: Integer);

    property ScreenX: Word read fScreenX;
    property ScreenY: Word read fScreenY;

    procedure BeginFrame;
    procedure RenderBrightness(Value: Byte);
    procedure EndFrame;
  end;


implementation
uses KM_Log;


constructor TRender.Create(RenderFrame: HWND; ScreenX,ScreenY: Integer; aVSync: Boolean);
begin
  Inherited Create;

  SetRenderFrame(RenderFrame, h_DC, h_RC);
  SetRenderDefaults;
  glDisable(GL_LIGHTING); //We don't need it

  fOpenGL_Vendor   := glGetString(GL_VENDOR);   fLog.AddToLog('OpenGL Vendor: '   + String(fOpenGL_Vendor));
  fOpenGL_Renderer := glGetString(GL_RENDERER); fLog.AddToLog('OpenGL Renderer: ' + String(fOpenGL_Renderer));
  fOpenGL_Version  := glGetString(GL_VERSION);  fLog.AddToLog('OpenGL Version: '  + String(fOpenGL_Version));

  SetupVSync(aVSync);
  BuildFont(h_DC, 16, FW_BOLD);

  Resize(ScreenX, ScreenY);
end;


destructor TRender.Destroy;
begin
  {$IFDEF MSWindows}
  wglMakeCurrent(h_DC, 0);
  wglDeleteContext(h_RC);
  {$ENDIF}
  {$IFDEF Unix}
    //?
  {$ENDIF}
  inherited;
end;


procedure TRender.Resize(Width, Height: Integer);
begin
  fScreenX := max(Width, 1);
  fScreenY := max(Height, 1);
  glViewport(0, 0, fScreenX, fScreenY);
end;


procedure TRender.SetRenderMode(aRenderMode: TRenderMode);
begin
  glMatrixMode(GL_PROJECTION); //Change Matrix Mode to Projection
  glLoadIdentity; //Reset View
  case aRenderMode of
    rm2D: gluOrtho2D(0, fScreenX, fScreenY, 0);
    rm3D: gluPerspective(80, -fScreenX/fScreenY, 0.1, 5000.0);
  end;
  glMatrixMode(GL_MODELVIEW); //Return to the modelview matrix
  glLoadIdentity; //Reset View
end;


//Generate texture out of TCardinalArray
function TRender.GenTexture(DestX, DestY: Word; const Data: TCardinalArray; Mode: TTexFormat): GLUint;
begin
  Result := 0;

  DestX := MakePOT(DestX);
  DestY := MakePOT(DestY);
  if DestX*DestY = 0 then exit; //Do not generate zeroed textures

  Result := GenerateTextureCommon; //Should be called prior to glTexImage2D or gluBuild2DMipmaps

  //@Krom: Make textures support an alpha channel for nice shadows. How does it work for houses on top of AlphaTest?
  //@Lewin: AlphaTest does not supports semitransparency, because it uses A chanel to do the testing
  //we will need to use something else

  //GL_ALPHA   (0-0-0-8 bit) - used only for flags, but may bring unexpected bugs
  //GL_RGB5_A1 (5-5-5-1 bit) - uses 185mb GPURAM
  //GL_RGBA    (8-8-8-8 bit) - uses 323mb GPURAM (but allows fuzzy shadows)
  //Figures are before trimming - only ratio matters
  case Mode of
    //Houses under construction
    tf_AlphaTest:   glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA,    DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
    //Base layer
    tf_Normal:      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB5_A1, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
    //Base layer with alpha channel for shadows
    tf_NormalAlpha: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA,    DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
    //Team color layer
    tf_AltID:       glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA4,  DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
  end;
end;


procedure TRender.BeginFrame;
begin
  glClear(GL_COLOR_BUFFER_BIT); //Clear The Screen, can save some FPS on this one

  //RC.Activate for OSX
end;




//Render highlight overlay to make whole picture look brighter (more saturated)
procedure TRender.RenderBrightness(Value: Byte);
begin
  //There will be no change to image anyway
  if Value = 0 then Exit;

  glLoadIdentity;
  glBlendFunc(GL_DST_COLOR, GL_ONE);
  glColor4f(Value/20, Value/20, Value/20, Value/20);
  glBegin(GL_QUADS);
    glkRect(0, 0, ScreenX, ScreenY);
  glEnd;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
end;

procedure TRender.EndFrame;
begin
  glFinish;
  {$IFDEF MSWindows}
  SwapBuffers(h_DC);
  {$ENDIF}
  {$IFDEF Unix}
  glXSwapBuffers(FDisplay, FDC);
  {$ENDIF}
end;


end.
