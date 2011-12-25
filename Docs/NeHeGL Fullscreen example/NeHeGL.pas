unit NeHeGL;

interface

uses windows, messages, opengl, ARB_multisample;

type
  Keys = record
    keyDown: array [0..255] of boolean;
    end;

  PKeys = ^Keys;

  Application = record
    hInstance: HINST;
    className: string;
    end;

  GL_WindowInit = record
    application: ^Application;
    title: string;
    width: integer;
    height: integer;
    bitsPerPixel: integer;
    isFullScreen: boolean;
    end;

  GL_Window = record
    keys: PKeys;
    hWnd: HWND;                      // Obsahuje Handle našeho okna
    hDc: HDC;                        // Privátní GDI Device Context
    hRc: HGLRC;		                  // Trvalý Rendering Context
    init: GL_WindowInit;
    isVisible: boolean;
    lastTickCount: DWORD;
    end;

  PGL_Window = ^GL_Window;

procedure TerminateApplication(window: GL_Window);
procedure ToggleFullscreen(window: GL_Window);
procedure ReshapeGL(Width, Height: GLsizei);
function CreateWindowGL(var window: GL_Window): boolean;
function DestroyWindowGL(var window: GL_Window): boolean;  

const
  WM_TOGGLEFULLSCREEN = WM_USER + 1;

var
  g_isProgramLooping: boolean;
  g_createFullScreen: boolean;

implementation

procedure TerminateApplication(window: GL_Window);
begin
  PostMessage(window.hWnd,WM_QUIT,0,0);
  g_isProgramLooping := false;
end;

procedure ToggleFullscreen(window: GL_Window);
begin
  PostMessage(window.hWnd,WM_TOGGLEFULLSCREEN,0,0);
end;

procedure ReshapeGL(Width, Height: GLsizei); // Zmìna velikosti a inicializace OpenGL okna
begin
  if Height = 0 then		                                  // Zabezpeèení proti dìlení nulou
     Height := 1;                                           // Nastaví výšku na jedna
  glViewport(0,0,Width,Height);                        // Resetuje aktuální nastavení
  glMatrixMode(GL_PROJECTION);                            // Zvolí projekèní matici
  glLoadIdentity;                                       // Reset matice
  gluPerspective(50.0,Width/Height,5.0,2000.0);            // Výpoèet perspektivy
  glMatrixMode(GL_MODELVIEW);                             // Zvolí matici Modelview
  glLoadIdentity;                                         // Reset matice
end;

function ChangeScreenResolution(width, height, bitsPerPixel: integer): boolean;
var
  dmScreenSettings: DEVMODE;      // Mód zaøízení
begin
  ZeroMemory(@dmScreenSettings,sizeof(dmScreenSettings));  // Vynulování pamìti
  with dmScreensettings do
    begin
    dmSize := sizeof(dmScreenSettings);         // Velikost struktury Devmode
    dmPelsWidth := width;	                    // Šíøka okna
    dmPelsHeight := height;                     // Výška okna
    dmBitsPerPel := bitsPerPixel;                       // Barevná hloubka
    dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
    end;
  // Pokusí se použít právì definované nastavení
  if ChangeDisplaySettings(dmScreenSettings,CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL then
    begin
    Result := false;
    exit;
    end;
  Result := true;
end;

function CreateWindowGL(var window: GL_Window): boolean;
var
  windowStyle: DWORD;                  // Styl okna
  windowExtendedStyle: DWORD;                // Rozšíøený styl okna
  pfd: PIXELFORMATDESCRIPTOR;     // Nastavení formátu pixelù
  WindowRect: TRect;              // Obdélník okna
  Pixelformat: GLuint;            // Ukládá formát pixelù
begin
  windowStyle := WS_OVERLAPPEDWINDOW;
  windowExtendedStyle := WS_EX_APPWINDOW;
  with pfd do                                         // Oznámíme Windows jak chceme vše nastavit
    begin
    nSize := SizeOf(PIXELFORMATDESCRIPTOR);        // Velikost struktury
    nVersion := 1;                                   // Èíslo verze
    dwFlags := PFD_DRAW_TO_WINDOW                    // Podpora okna
            or PFD_SUPPORT_OPENGL                         // Podpora OpenGL
            or PFD_DOUBLEBUFFER;                          // Podpora Double Bufferingu
    iPixelType := PFD_TYPE_RGBA;                     // RGBA Format
    cColorBits := window.init.bitsPerPixel;                              // Zvolí barevnou hloubku
    cRedBits := 0;                                   // Bity barev ignorovány
    cRedShift := 0;
    cGreenBits := 0;
    cGreenShift := 0;
    cBlueBits := 0;
    cBlueShift := 0;
    cAlphaBits := 0;                                 // Žádný alpha buffer
    cAlphaShift := 0;                                // Ignorován Shift bit
    cAccumBits := 0;                                 // Žádný akumulaèní buffer
    cAccumRedBits := 0;                              // Akumulaèní bity ignorovány
    cAccumGreenBits := 0;
    cAccumBlueBits := 0;
    cAccumAlphaBits := 0;
    cDepthBits := 16;                                // 16-bitový hloubkový buffer (Z-Buffer)
    cStencilBits := 0;                               // Žádný Stencil Buffer
    cAuxBuffers := 0;                                // Žádný Auxiliary Buffer
    iLayerType := PFD_MAIN_PLANE;                    // Hlavní vykreslovací vrstva
    bReserved := 0;                                  // Rezervováno
    dwLayerMask := 0;                                // Maska vrstvy ignorována
    dwVisibleMask := 0;
    dwDamageMask := 0;
    end;
  WindowRect.Left := 0;                               // Nastaví levý okraj na nulu
  WindowRect.Top := 0;                                // Nastaví horní okraj na nulu
  WindowRect.Right := window.init.width;                          // Nastaví pravý okraj na zadanou hodnotu
  WindowRect.Bottom := window.init.height;                        // Nastaví spodní okraj na zadanou hodnotu
  if window.init.isFullScreen then
    if not ChangeScreenResolution(window.init.width,window.init.height,window.init.bitsPerPixel) then
      begin
      MessageBox(HWND_DESKTOP,'Mode Switch Failed.\nRunning In Windowed Mode.','Error',MB_OK or MB_ICONEXCLAMATION);
			window.init.isFullScreen := false;
      end
      else
      begin
      ShowCursor(false);
      windowStyle := WS_POPUP;
      windowExtendedStyle := windowExtendedStyle or WS_EX_TOPMOST;
      end
    else
    AdjustWindowRectEx(WindowRect,windowStyle,false,windowExtendedStyle); // Pøizpùsobení velikosti okna
  // Vytvoøení okna
  window.hWnd := CreateWindowEx(windowExtendedStyle,                    // Rozšíøený styl
                               PChar(window.init.application^.className),              // Jméno tøídy
                               PChar(window.init.title),                 // Titulek
                               windowStyle,               // Definovaný styl
                               0,0,                   // Pozice
                               WindowRect.Right-WindowRect.Left,  // Výpoèet šíøky
                               WindowRect.Bottom-WindowRect.Top,  // Výpoèet výšky
                               HWND_DESKTOP,                     // Rodièovské okno
                               0,                     // Bez menu
                               window.init.application^.hInstance,             // Instance
                               @window);                  // Do WM_CREATE
  if window.hWnd = 0 then                                     // Pokud se okno nepodaøilo vytvoøit
    begin
      Result := false;                                  // Vrátí chybu
      exit;
    end;
  window.hDc := GetDC(window.hWnd);                               // Zkusí pøipojit kontext zaøízení
  if window.hDc = 0 then                                      // Podaøilo se pøipojit kontext zaøízení?
    begin
      DestroyWindow(window.hWnd);                                 // Zavøe okno
      window.hWnd := 0;
      Result := false;                                  // Ukonèí program
      exit;
    end;
  if not arbMultisampleSupported then                       // Multisampling není podporován
    begin
    // Vytvoøení normálního okna
    PixelFormat := ChoosePixelFormat(window.hDC,@pfd);      // Získá kompatibilní pixel formát
    if PixelFormat = 0 then                                 // Podaøilo se ho získat?
      begin
      ReleaseDC(window.hWnd,window.hDC);                    // Uvolnìní kontextu zaøízení
      window.hDC := 0;                                      // Nulování promìnné
      DestroyWindow(window.hWnd);                           // Zrušení okna
      window.hWnd := 0;                                     // Nulování handle
      Result := false;                                      // Neúspìch
      exit;
      end;
    end
    else                                                    // Multisampling je podporován
    PixelFormat := arbMultisampleFormat;
  if not SetPixelFormat(window.hDc,PixelFormat,@pfd) then  // Podaøilo se nastavit Pixel Format?
    begin
      ReleaseDC(window.hWnd,window.hDc);
      window.hDc := 0;
      DestroyWindow(window.hWnd);                               // Zavøe okno
      window.hWnd := 0;
      Result := false;                                  // Ukonèí program
      exit;
    end;
  window.hRc := wglCreateContext(window.hDc);                     // Podaøilo se vytvoøit Rendering Context?
  if window.hRc = 0 then
    begin
      ReleaseDC(window.hWnd,window.hDc);
      window.hDc := 0;
      DestroyWindow(window.hWnd);                               // Zavøe okno
      window.hWnd := 0;
      Result := false;                                  // Ukonèí program
      exit;
    end;
  if not wglMakeCurrent(window.hDc,window.hRc) then            // Podaøilo se aktivovat Rendering Context?
    begin
      wglDeleteContext(window.hRc);
      window.hRc := 0;
      ReleaseDC(window.hWnd,window.hDc);
      window.hDc := 0;
      DestroyWindow(window.hWnd);                               // Zavøe okno
      window.hWnd := 0;
      Result := false;                                  // Ukonèí program
      exit;
    end;
  if (not arbMultisampleSupported) and CHECK_FOR_MULTISAMPLE then   // Je multisampling dostupný?
    if InitMultisample(window.init.application.hInstance,window.hWnd,pfd) then // Inicializace multisamplingu
      begin
      DestroyWindowGL(window);
      Result := CreateWindowGL(window);
      end;
  ShowWindow(window.hWnd,SW_NORMAL);                          // Zobrazení okna
  window.isVisible := true;
  ReshapeGL(window.init.width,window.init.height);                        // Nastavení perspektivy OpenGL scény
  ZeroMemory(window.keys,Sizeof(Keys));
  window.lastTickCount := GetTickCount;
  Result := true;                                       // Vše probìhlo v poøádku
end;

function DestroyWindowGL(var window: GL_Window): boolean;                                 // Zavírání okna
begin
  if window.hWnd <> 0 then
    begin
    if window.hDc <> 0 then
      begin
      wglMakeCurrent(window.hDc,0);
      if window.hRc <> 0 then
        begin
        wglDeleteContext(window.hRc);
        window.hRc := 0;
        end;
      ReleaseDC(window.hWnd,window.hDc);
      window.hDc := 0;
      end;
    DestroyWindow(window.hWnd);
    window.hWnd := 0;
    end;
  if window.init.isFullScreen then                                    // Jsme ve fullscreenu?
    ChangeDisplaySettings(DEVMODE(nil^),0);           // Pøepnutí do systému
  ShowCursor(true);
  Result := true;
end;

end.
