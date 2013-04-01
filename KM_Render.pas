unit KM_Render;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF WDC} Windows, Graphics, JPEG, {$ENDIF} //Lazarus doesn't have JPEG library yet -> FPReadJPEG?
  {$IFDEF Unix} LCLIntf, LCLType, OpenGLContext, {$ENDIF}
  Math, dglOpenGL, KromOGLUtils, KromUtils, KM_RenderControl;


type
  TTexFormat = (
    tf_RGB5A1,
    tf_RGBA8,
    tf_Alpha8 //Mask used for team colors and house construction steps (GL_ALPHA)
    );
  const
  TexFormatSize: array [TTexFormat] of Byte = (2, 4, 1);

type
  TRenderMode = (rm2D, rm3D);

  //General OpenGL handling
  TRender = class
  private
    fRenderControl: TKMRenderControl;
    fOpenGL_Vendor, fOpenGL_Renderer, fOpenGL_Version: AnsiString;
    fScreenX, fScreenY: Word;
    fBlind: Boolean;
  public
    constructor Create(aRenderControl: TKMRenderControl; ScreenX,ScreenY: Integer; aVSync: Boolean);
    destructor Destroy; override;

    procedure SetRenderMode(aRenderMode: TRenderMode); //Switch between 2D and 3D perspectives

    class function GetMaxTexSize: Integer;
    class function GenerateTextureCommon: GLuint;
    class function GenTexture(DestX, DestY: Word; const Data: Pointer; Mode: TTexFormat): GLUint;
    class procedure UpdateTexture(aTexture: GLuint; DestX, DestY: Word; Mode: TTexFormat; const Data: Pointer);

    property RendererVersion: AnsiString read fOpenGL_Version;
    function IsOldGLVersion: Boolean;
    procedure DoPrintScreen(FileName: string);
    procedure Resize(Width,Height: Integer);

    property ScreenX: Word read fScreenX;
    property ScreenY: Word read fScreenY;
    property Blind: Boolean read fBlind;

    procedure BeginFrame;
    procedure RenderBrightness(Value: Byte);
    procedure EndFrame;
  end;


implementation
uses KM_Log;


constructor TRender.Create(aRenderControl: TKMRenderControl; ScreenX,ScreenY: Integer; aVSync: Boolean);
begin
  inherited Create;

  fBlind := aRenderControl = nil;
  fRenderControl := aRenderControl;

  if not fBlind then
  begin
    fRenderControl.CreateRenderContext;

    glDisable(GL_LIGHTING); //We don't need it
    //glEnable(GL_CULL_FACE);
    //glCullFace(GL_FRONT);

    fOpenGL_Vendor   := glGetString(GL_VENDOR);   fLog.AddNoTime('OpenGL Vendor: '   + string(fOpenGL_Vendor));
    fOpenGL_Renderer := glGetString(GL_RENDERER); fLog.AddNoTime('OpenGL Renderer: ' + string(fOpenGL_Renderer));
    fOpenGL_Version  := glGetString(GL_VERSION);  fLog.AddNoTime('OpenGL Version: '  + string(fOpenGL_Version));

    SetupVSync(aVSync);
    //todo: Is this needed? Make it multiplatform
    //BuildFont(h_DC, 16, FW_BOLD);

    Resize(ScreenX, ScreenY);
  end;
end;


destructor TRender.Destroy;
begin
  if not fBlind then
    fRenderControl.DestroyRenderContext;
  inherited;
end;


procedure TRender.Resize(Width, Height: Integer);
begin
  if fBlind then Exit;

  fScreenX := max(Width, 1);
  fScreenY := max(Height, 1);
  glViewport(0, 0, fScreenX, fScreenY);
end;


procedure TRender.SetRenderMode(aRenderMode: TRenderMode);
begin
  if fBlind then Exit;

  glMatrixMode(GL_PROJECTION); //Change Matrix Mode to Projection
  glLoadIdentity; //Reset View

  //In 2D mode we use Z-test to clip terrain shadows behind mountains
  //1 unit for each tile strip. 512 means we can handle up to 512x512 maps
  case aRenderMode of
    rm2D: glOrtho(0, fScreenX, fScreenY, 0, 512, 0);
    rm3D: gluPerspective(80, -fScreenX/fScreenY, 0.1, 5000.0);
  end;
  glMatrixMode(GL_MODELVIEW); //Return to the modelview matrix
  glLoadIdentity; //Reset View
end;


class function TRender.GenerateTextureCommon: GLuint;
var Texture: GLuint;
begin
  Result := 0;
  if not Assigned(glGenTextures) then Exit;

  glGenTextures(1, @Texture);
  glBindTexture(GL_TEXTURE_2D, Texture);

  {Enable color blending into texture}
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  //GL_MODULATE is our choice
  //GL_REPLACE is also available since version 1.1
  //can't use GL_REPLACE cos it disallows blending of texture with custom color (e.g. trees in FOW)

  {Keep original KaM grainy look}
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);

  {Clamping UVs solves edge artifacts}
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  Result := Texture;
end;


//Generate texture out of TCardinalArray
class function TRender.GenTexture(DestX, DestY: Word; const Data: Pointer; Mode: TTexFormat): GLUint;
begin
  Result := GenerateTextureCommon;
  UpdateTexture(Result, DestX, DestY, Mode, Data);
end;


class function TRender.GetMaxTexSize: Integer;
begin
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @Result);
end;


//Update texture with TCardinalArray
class procedure TRender.UpdateTexture(aTexture: GLuint; DestX, DestY: Word; Mode: TTexFormat; const Data: Pointer);
begin
  if not Assigned(glTexImage2D) then Exit;
  Assert((DestX * DestY > 0) and (DestX = MakePOT(DestX)) and (DestY = MakePOT(DestY)), 'Game designed to handle only POT textures');

  glBindTexture(GL_TEXTURE_2D, aTexture);

  //GL_ALPHA   (0-0-0-8 bit) - 
  //GL_RGB5_A1 (5-5-5-1 bit) - 
  //GL_RGBA    (8-8-8-8 bit) - allows fuzzy shadows
  //Figures are before trimming - only ratio matters
  case Mode of
    //Base layer
    tf_RGB5A1:  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB5_A1, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
    //Base layer with alpha channel for shadows
    tf_RGBA8:   glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
    //Team color layer (4 bit would be okay), but house construction steps need 8bit resolution
    tf_Alpha8:  glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA,  DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
  end;
  glBindTexture(GL_TEXTURE_2D, 0);
end;


//1.4 is considered to be our minimal requirement
function TRender.IsOldGLVersion: Boolean;
begin
  Result := not fBlind and not GL_VERSION_1_4;
end;


procedure TRender.DoPrintScreen(FileName: string);
{$IFDEF WDC}
var
  i, k, W, H: integer;
  jpg: TJpegImage;
  mkbmp: TBitMap;
  bmp: array of Cardinal;
{$ENDIF}
begin
{$IFDEF WDC}
  W := ScreenX;
  H := ScreenY;

  SetLength(bmp, W * H + 1);
  glReadPixels(0, 0, W, H, GL_BGRA, GL_UNSIGNED_BYTE, @bmp[0]);

  //Mirror verticaly
  for i := 0 to (H div 2) - 1 do
    for k := 0 to W - 1 do
      SwapInt(bmp[i * W + k], bmp[((H - 1) - i) * W + k]);

  mkbmp := TBitmap.Create;
  mkbmp.Handle := CreateBitmap(W, H, 1, 32, @bmp[0]);

  jpg := TJpegImage.Create;
  jpg.assign(mkbmp);
  jpg.ProgressiveEncoding := true;
  jpg.ProgressiveDisplay  := true;
  jpg.Performance         := jpBestQuality;
  jpg.CompressionQuality  := 90;
  jpg.Compress;
  jpg.SaveToFile(FileName);

  jpg.Free;
  mkbmp.Free;
{$ENDIF}
end;


procedure TRender.BeginFrame;
begin
  if fBlind then Exit;

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  SetRenderMode(rm2D);

  //RC.Activate for OSX
end;


//Render highlight overlay to make whole picture look brighter (more saturated)
procedure TRender.RenderBrightness(Value: Byte);
begin
  if fBlind then Exit;

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
  if fBlind then Exit;

  glFinish;
  fRenderControl.SwapBuffers;
end;


end.
