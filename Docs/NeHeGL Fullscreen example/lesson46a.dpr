program lesson46a;

{   kód pro Delphi 7}

uses
  Windows,
  Messages,
  OpenGL,
  NeHeGL in 'NeHeGL.pas',
  ARB_multisample in 'ARB_multisample.pas';


const
  GL_MULTISAMPLE_ARB = $809D;

var
  g_window: PGL_Window;                                         // Okno
  g_keys: PKeys;                                                // Klávesy
  domulti: boolean = true;
  doangle: boolean = true;
  angle: GLfloat = 0.0;
  mp: boolean = false;
  sp: boolean = false;


function Initialize(window: PGL_Window; key: PKeys): boolean;	                  // Inicializace OpenGL
begin
  g_window := window;                                                           // Okno
  g_keys := key;                                                                // Klávesnice
  angle := 0.0;
  glViewport(0,0,window.init.width,window.init.height);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity;
	gluPerspective(50,window.init.width / window.init.height,5,2000);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity;
	glEnable(GL_DEPTH_TEST);
	glShadeModel(GL_SMOOTH);
	glClearColor(0.0,0.0,0.0,0.5);
  Result := true;                                                               // OK
end;

procedure Deinitialize;                                                         // Deinicializace
begin

end;

procedure Update(milliseconds: DWORD);                                // Aktualizace pohybù ve scénì a stisk kláves
begin
  if g_keys.keyDown[VK_SPACE] and not sp then
    begin
    sp := true;
     domulti := not domulti;
    end;
  if not g_keys.keyDown[VK_SPACE] then sp := false;
  if g_keys.keyDown[Ord('M')] and not mp then
    begin
    mp := true;
    doangle := not doangle;
    end;
  if not g_keys.keyDown[Ord('M')] then mp := false;
  if g_keys.keyDown[VK_ESCAPE] then                                   // Klávesa ESC?
    TerminateApplication(g_window^);                                  // Ukonèení programu
  if g_keys.keyDown[VK_F1] then                                       // Klávesa F1?
    ToggleFullscreen(g_window^);                                      // Pøepnutí fullscreen/okno
end;

procedure Draw;                                                       // Vykreslení scény
var
  i, j: integer;
begin
  if domulti then glEnable(GL_MULTISAMPLE_ARB);
  glClearColor(0.0,0.0,0.0,0.5);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);                // Smaže obrazovku a hloubkový buffer
  glLoadIdentity;	                                                    // Reset matice
  for i := -10 to 9 do
		for j := -10 to 9 do
		  begin
			glPushMatrix;
			glTranslatef(i*2.0,j*2.0,-5.0);
			glRotatef(angle,0.0,0.0,1.0);
				glBegin(GL_QUADS);
				glColor3f(1.0,0.0,0.0);glVertex3f(i,j,0.0);
				glColor3f(0.0,1.0,0.0);glVertex3f(i + 2.0,j,0.0);
				glColor3f(0.0,0.0,1.0);glVertex3f(i + 2.0,j + 2.0,0.0);
				glColor3f(1.0,1.0,1.0);glVertex3f(i,j + 2.0,0.0);
				glEnd;
			glPopMatrix;
		  end;
	if doangle then	angle := angle + 0.05;
  glFlush;                                                            // Vyprázdní OpenGL renderovací pipeline
	if domulti then	glDisable(GL_MULTISAMPLE_ARB);
end;

function WindowProc(hWnd: HWND;                                       // Handle okna
                 uMsg: UINT;                                          // Zpráva pro okno
                 wParam: WPARAM;                                      // Doplòkové informace
                 lParam: LPARAM):                                     // Doplòkové informace
                                  LRESULT; stdcall;
var
  window: ^GL_Window;
  creation: ^CREATESTRUCT;
begin
  if uMsg = WM_SYSCOMMAND then                                        // Systémový pøíkaz
      case wParam of                                                  // Typ systémového pøíkazu
        SC_SCREENSAVE,SC_MONITORPOWER:                                // Pokus o zapnutí šetøièe obrazovky, Pokus o pøechod do úsporného režimu?
          begin
            Result := 0;                                              // Zabrání obojímu
            exit;
          end;
      end;
  window := Pointer(GetWindowLong(hWnd,GWL_USERDATA));
  case uMsg of                                                        // Vìtvení podle pøíchozí zprávy
    WM_CREATE:
      begin
      creation := Pointer(lParam);
      window := Pointer(creation.lpCreateParams);
      SetWindowLong(hWnd,GWL_USERDATA,Integer(window));
      Result := 0;
      end;
    WM_CLOSE:                                                         // Povel k ukonèení programu
      begin
      TerminateApplication(window^);                                  // Pošle zprávu o ukonèení
      Result := 0                                                     // Návrat do hlavního cyklu programu
      end;
    WM_SIZE:                                                          // Zmìna velikosti okna
      begin
      case wParam of
        SIZE_MINIMIZED:
          begin
          window.isVisible := false;
          Result := 0;
          end;
        SIZE_MAXIMIZED,
        SIZE_RESTORED:
          begin
          window.isVisible := true;
          ReshapeGL(LOWORD(lParam),HIWORD(lParam));                 // LoWord=Šíøka, HiWord=Výška
          Result := 0;                                              // Návrat do hlavního cyklu programu
          end;
      end;
     // Result := 0;
      end;
    WM_KEYDOWN:                                                     // Stisk klávesy
      begin
      if (wParam >= 0) and (wParam <= 255) then
        begin
        window^.keys^.keyDown[wParam] := true;                      // Oznámí to programu
        Result := 0;
        end;
      //Result := 0;                                                // Návrat do hlavního cyklu programu
      end;
    WM_KEYUP:                                                       // Uvolnìní klávesy
      begin
      if (wParam >= 0) and (wParam <= 255) then
        begin
    	  window^.keys^.keyDown[wParam] := false;                     // Oznámí to programu
        Result := 0;                                                // Návrat do hlavního cyklu programu
        end;
      //exit;
      end;
    WM_TOGGLEFULLSCREEN:
      begin
      g_createFullScreen := not g_createFullScreen;
      PostMessage(hWnd,WM_QUIT,0,0);
      Result := 0;
      end;
    else
      // Pøedání ostatních zpráv systému
      begin
      	Result := DefWindowProc(hWnd,uMsg,wParam,lParam);
      end;
    end;
end;

function RegisterWindowClass(application: Application): boolean;
var
  windowClass: WNDCLASSEX;
begin
  ZeroMemory(@windowClass,Sizeof(windowClass));
  with windowClass do
    begin
    cbSize := Sizeof(windowClass);
    style := CS_HREDRAW or CS_VREDRAW or CS_OWNDC;                  // Pøekreslení pøi zmìnì velikosti a vlastní DC
    lpfnWndProc := @WindowProc;                                     // Definuje proceduru okna
    hInstance := application.hInstance;                             // Instance
    hbrBackground := COLOR_APPWORKSPACE;                            // Pozadí není nutné
    hCursor := LoadCursor(0,IDC_ARROW);                             // Standardní kurzor myši
    lpszClassName := PChar(application.className);                  // Jméno tøídy okna
    end;
  if RegisterClassEx(windowClass) = 0 then                          // Registruje tøídu okna
    begin
    MessageBox(HWND_DESKTOP,'RegisterClassEx Failed!','Error',MB_OK or MB_ICONEXCLAMATION);
    Result := false;                                                // Pøi chybì vrátí false
    exit;
    end;
  Result := true;
end;

function WinMain(hInstance: HINST;                                  // Instance
		 hPrevInstance: HINST;                                          // Pøedchozí instance
		 lpCmdLine: PChar;                                              // Parametry pøíkazové øádky
		 nCmdShow: integer):                                            // Stav zobrazení okna
                        integer; stdcall;
var
  app: Application;
  window: GL_Window;
  key: Keys;
  isMessagePumpActive: boolean;
  msg: TMsg;                                                        // Struktura zpráv systému
  tickCount: DWORD;
begin
  app.className := 'OpenGL';
  app.hInstance := hInstance;
  ZeroMemory(@window,Sizeof(window));
  with window do
    begin
    keys := @key;
    init.application := @app;
    init.title := 'Lesson 46: NeHe & MainRoach''s FSAA Tutorial';
    init.width := 640;
    init.height := 480;
    init.bitsPerPixel := 32;
    init.isFullScreen := true;
    end;
  ZeroMemory(@key,Sizeof(key));
  // Dotaz na uživatele pro fullscreen/okno
  if MessageBox(HWND_DESKTOP,'Would You Like To Run In FullScreen Mode?','Start FullScreen',
                MB_YESNO or MB_ICONQUESTION) = IDNO then
    window.init.isFullScreen := false;                                 // Bìh v oknì
  if not RegisterWindowClass(app) then
    begin
    MessageBox(HWND_DESKTOP,'Error Registering Window Class!','Error',MB_OK or MB_ICONEXCLAMATION);
    Result := -1;
    exit;
    end;
  g_isProgramLooping := true;
  g_createFullScreen := window.init.isFullScreen;
  while g_isProgramLooping do
    begin
    window.init.isFullScreen := g_createFullScreen;
    if CreateWindowGL(window) then
      begin
      if not Initialize(@window,@key) then
        TerminateApplication(window)
        else
        begin
        isMessagePumpActive := true;
        while isMessagePumpActive do
          if PeekMessage(msg,0,0,0,PM_REMOVE) then                  // Pøišla zpráva?
            if msg.message <> WM_QUIT then                          // Obdrželi jsme zprávu pro ukonèení?
              DispatchMessage(msg)                                
              else
              isMessagePumpActive := false                          // Konec programu
            else
            if not window.isVisible then
              WaitMessage
              else
              begin
              tickCount := GetTickCount;
              Update(tickCount - window.lastTickCount);
              window.lastTickCount := tickCount;
              Draw;
              SwapBuffers(window.hDc);
              end;
        end;
      Deinitialize;
      DestroyWindowGL(window);
      end
      else
      begin
      MessageBox(HWND_DESKTOP,'Error Creating OpenGL Window','Error',MB_OK or MB_ICONEXCLAMATION);
      g_isProgramLooping := false;
      end;
    end;
  UnregisterClass(PChar(app.className),app.hInstance);
  Result := 0;
end;

begin
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );                  // Start programu
end.

