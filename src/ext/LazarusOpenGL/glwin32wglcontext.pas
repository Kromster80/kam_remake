{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

}
unit GLWin32WGLContext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages, Windows, LCLProc, LCLType, gl, Forms, Controls, 
  Win32Int, WSLCLClasses, WSControls, Win32WSControls, Win32Proc, LCLMessageGlue;

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
procedure LOpenGLSwapBuffers(Handle: HWND);
function LOpenGLMakeCurrent(Handle: HWND): boolean;
function LOpenGLCreateContext(AWinControl: TWinControl;
                    WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
                    DoubleBuffered, RGBA: boolean;
                    const AParams: TCreateParams): HWND;
procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);

procedure InitWGL;
procedure InitOpenGLContextGLWindowClass;


type
  TWGLControlInfo = record
    Window: HWND;
    DC: HDC;
    PixelFormat: GLUInt;
    WGLContext: HGLRC;
  end;
  PWGLControlInfo = ^TWGLControlInfo;

var
  WGLControlInfoAtom: ATOM = 0;

function AllocWGLControlInfo(Window: HWND): PWGLControlInfo;
function DisposeWGLControlInfo(Window: HWND): boolean;
function GetWGLControlInfo(Window: HWND): PWGLControlInfo;


const
  WGL_SAMPLE_BUFFERS_ARB                           = $2041;
  WGL_SAMPLES_ARB                                  = $2042;

  // WGL_ARB_pixel_format
  WGL_NUMBER_PIXEL_FORMATS_ARB                     = $2000;
  WGL_DRAW_TO_WINDOW_ARB                           = $2001;
  WGL_DRAW_TO_BITMAP_ARB                           = $2002;
  WGL_ACCELERATION_ARB                             = $2003;
  WGL_NEED_PALETTE_ARB                             = $2004;
  WGL_NEED_SYSTEM_PALETTE_ARB                      = $2005;
  WGL_SWAP_LAYER_BUFFERS_ARB                       = $2006;
  WGL_SWAP_METHOD_ARB                              = $2007;
  WGL_NUMBER_OVERLAYS_ARB                          = $2008;
  WGL_NUMBER_UNDERLAYS_ARB                         = $2009;
  WGL_TRANSPARENT_ARB                              = $200A;
  WGL_TRANSPARENT_RED_VALUE_ARB                    = $2037;
  WGL_TRANSPARENT_GREEN_VALUE_ARB                  = $2038;
  WGL_TRANSPARENT_BLUE_VALUE_ARB                   = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE_ARB                  = $203A;
  WGL_TRANSPARENT_INDEX_VALUE_ARB                  = $203B;
  WGL_SHARE_DEPTH_ARB                              = $200C;
  WGL_SHARE_STENCIL_ARB                            = $200D;
  WGL_SHARE_ACCUM_ARB                              = $200E;
  WGL_SUPPORT_GDI_ARB                              = $200F;
  WGL_SUPPORT_OPENGL_ARB                           = $2010;
  WGL_DOUBLE_BUFFER_ARB                            = $2011;
  WGL_STEREO_ARB                                   = $2012;
  WGL_PIXEL_TYPE_ARB                               = $2013;
  WGL_COLOR_BITS_ARB                               = $2014;
  WGL_RED_BITS_ARB                                 = $2015;
  WGL_RED_SHIFT_ARB                                = $2016;
  WGL_GREEN_BITS_ARB                               = $2017;
  WGL_GREEN_SHIFT_ARB                              = $2018;
  WGL_BLUE_BITS_ARB                                = $2019;
  WGL_BLUE_SHIFT_ARB                               = $201A;
  WGL_ALPHA_BITS_ARB                               = $201B;
  WGL_ALPHA_SHIFT_ARB                              = $201C;
  WGL_ACCUM_BITS_ARB                               = $201D;
  WGL_ACCUM_RED_BITS_ARB                           = $201E;
  WGL_ACCUM_GREEN_BITS_ARB                         = $201F;
  WGL_ACCUM_BLUE_BITS_ARB                          = $2020;
  WGL_ACCUM_ALPHA_BITS_ARB                         = $2021;
  WGL_DEPTH_BITS_ARB                               = $2022;
  WGL_STENCIL_BITS_ARB                             = $2023;
  WGL_AUX_BUFFERS_ARB                              = $2024;
  WGL_NO_ACCELERATION_ARB                          = $2025;
  WGL_GENERIC_ACCELERATION_ARB                     = $2026;
  WGL_FULL_ACCELERATION_ARB                        = $2027;
  WGL_SWAP_EXCHANGE_ARB                            = $2028;
  WGL_SWAP_COPY_ARB                                = $2029;
  WGL_SWAP_UNDEFINED_ARB                           = $202A;
  WGL_TYPE_RGBA_ARB                                = $202B;
  WGL_TYPE_COLORINDEX_ARB                          = $202C;

  // WGL_NV_float_buffer
  WGL_FLOAT_COMPONENTS_NV                          = $20B0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_R_NV         = $20B1;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RG_NV        = $20B2;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGB_NV       = $20B3;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGBA_NV      = $20B4;
  WGL_TEXTURE_FLOAT_R_NV                           = $20B5;
  WGL_TEXTURE_FLOAT_RG_NV                          = $20B6;
  WGL_TEXTURE_FLOAT_RGB_NV                         = $20B7;
  WGL_TEXTURE_FLOAT_RGBA_NV                        = $20B8;

  // WGL_ARB_pbuffer
type
  HPBUFFERARB = Integer;
  TGLenum = uint;

const
  WGL_DRAW_TO_PBUFFER_ARB                          = $202D;
  WGL_MAX_PBUFFER_PIXELS_ARB                       = $202E;
  WGL_MAX_PBUFFER_WIDTH_ARB                        = $202F;
  WGL_MAX_PBUFFER_HEIGHT_ARB                       = $2030;
  WGL_PBUFFER_LARGEST_ARB                          = $2033;
  WGL_PBUFFER_WIDTH_ARB                            = $2034;
  WGL_PBUFFER_HEIGHT_ARB                           = $2035;
  WGL_PBUFFER_LOST_ARB                             = $2036;

  // WGL_ARB_buffer_region
  WGL_FRONT_COLOR_BUFFER_BIT_ARB                   = $00000001;
  WGL_BACK_COLOR_BUFFER_BIT_ARB                    = $00000002;
  WGL_DEPTH_BUFFER_BIT_ARB                         = $00000004;
  WGL_STENCIL_BUFFER_BIT_ARB                       = $00000008;

const
  opengl32 = 'OpenGL32.dll';
  glu32 = 'GLU32.dll';

type
  PWGLSwap = ^TWGLSwap;
  _WGLSWAP = packed record
    hdc: HDC;
    uiFlags: UINT;
  end;
  TWGLSwap = _WGLSWAP;
  WGLSWAP = _WGLSWAP;

  function wglGetProcAddress(ProcName: PChar): Pointer; stdcall; external opengl32;
  function wglCopyContext(p1: HGLRC; p2: HGLRC; p3: Cardinal): BOOL; stdcall; external opengl32;
  function wglCreateContext(DC: HDC): HGLRC; stdcall; external opengl32;
  function wglCreateLayerContext(p1: HDC; p2: Integer): HGLRC; stdcall; external opengl32;
  function wglDeleteContext(p1: HGLRC): BOOL; stdcall; external opengl32;
  function wglDescribeLayerPlane(p1: HDC; p2, p3: Integer; p4: Cardinal; var p5: TLayerPlaneDescriptor): BOOL; stdcall; external opengl32;
  function wglGetCurrentContext: HGLRC; stdcall; external opengl32;
  function wglGetCurrentDC: HDC; stdcall; external opengl32;
  function wglGetLayerPaletteEntries(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall; external opengl32;
  function wglMakeCurrent(DC: HDC; p2: HGLRC): BOOL; stdcall; external opengl32;
  function wglRealizeLayerPalette(p1: HDC; p2: Integer; p3: BOOL): BOOL; stdcall; external opengl32;
  function wglSetLayerPaletteEntries(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall; external opengl32;
  function wglShareLists(p1, p2: HGLRC): BOOL; stdcall; external opengl32;
  function wglSwapLayerBuffers(p1: HDC; p2: Cardinal): BOOL; stdcall; external opengl32;
  function wglUseFontBitmapsA(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32;
  function wglUseFontOutlinesA (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
  function wglUseFontBitmapsW(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32;
  function wglUseFontOutlinesW (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
  function wglUseFontBitmaps(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall; external opengl32 name 'wglUseFontBitmapsA';
  function wglUseFontOutlines(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32 name 'wglUseFontOutlinesA';

var
  // WGL Extensions ----------------------------
  WGL_EXT_swap_control: boolean;
  WGL_ARB_multisample: boolean;
  WGL_ARB_extensions_string: boolean;
  WGL_ARB_pixel_format: boolean;
  WGL_ARB_pbuffer: boolean;
  WGL_ARB_buffer_region: boolean;
  WGL_ATI_pixel_format_float: boolean;


  // ARB wgl extensions
  wglGetExtensionsStringARB: function(DC: HDC): PChar; stdcall;
  wglGetPixelFormatAttribivARB: function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: TGLenum;
    const piAttributes: PGLint; piValues : PGLint) : BOOL; stdcall;
  wglGetPixelFormatAttribfvARB: function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: TGLenum;
    const piAttributes: PGLint; piValues: PGLFloat) : BOOL; stdcall;
  wglChoosePixelFormatARB: function(DC: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLFloat;
    nMaxFormats: GLint; piFormats: PGLint; nNumFormats: PGLenum) : BOOL; stdcall;
  wglCreatePbufferARB: function(DC: HDC; iPixelFormat: Integer; iWidth, iHeight : Integer;
    const piAttribList: PGLint) : HPBUFFERARB; stdcall;
  wglGetPbufferDCARB: function(hPbuffer: HPBUFFERARB) : HDC; stdcall;
  wglReleasePbufferDCARB: function(hPbuffer: HPBUFFERARB; DC: HDC) : Integer; stdcall;
  wglDestroyPbufferARB: function(hPbuffer: HPBUFFERARB): BOOL; stdcall;
  wglQueryPbufferARB: function(hPbuffer: HPBUFFERARB; iAttribute : Integer;
    piValue: PGLint) : BOOL; stdcall;

  wglCreateBufferRegionARB: function(DC: HDC; iLayerPlane: Integer; uType: TGLenum) : Integer; stdcall;
  wglDeleteBufferRegionARB: procedure(hRegion: Integer); stdcall;
  wglSaveBufferRegionARB: function(hRegion: Integer; x, y, width, height: Integer): BOOL; stdcall;
  wglRestoreBufferRegionARB: function(hRegion: Integer; x, y, width, height: Integer;
    xSrc, ySrc: Integer): BOOL; stdcall;

  // non-ARB wgl extensions
  wglSwapIntervalEXT: function(interval : Integer) : BOOL; stdcall;
  wglGetSwapIntervalEXT: function : Integer; stdcall;

var
  WGLInitialized: boolean = false;
  OpenGLContextWindowClassInitialized: boolean = false;
  OpenGLContextWindowClass: WNDCLASS;

const
  DefaultOpenGLContextInitAttrList: array [0..0] of LongInt = (
    0
    );

implementation

function GLGetProcAddress(ProcName: PChar):Pointer;
begin
  Result := wglGetProcAddress(ProcName);
end;

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
begin
  glViewport(Left,Top,Width,Height);
end;

procedure LOpenGLSwapBuffers(Handle: HWND);
var
  Info: PWGLControlInfo;
begin
  Info:=GetWGLControlInfo(Handle);
  // don't use wglSwapLayerBuffers or wglSwapBuffers!
  SwapBuffers(Info^.DC);
end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
var
  Info: PWGLControlInfo;
begin
  Info:=GetWGLControlInfo(Handle);
  Result:=wglMakeCurrent(Info^.DC,Info^.WGLContext);
end;

function GlWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  PaintMsg   : TLMPaint;
  winctrl    : TWinControl;
begin
  case Msg of 
    WM_ERASEBKGND: begin
      Result:=0;
    end;
    WM_PAINT: begin 
      winctrl := GetWin32WindowInfo(Window)^.WinControl;
      if Assigned(winctrl) then begin
        FillChar(PaintMsg, SizeOf(PaintMsg), 0);
        PaintMsg.Msg := LM_PAINT;
        PaintMsg.DC := WParam;
        DeliverMessage(winctrl, PaintMsg);
        Result:=PaintMsg.Result;
      end else 
        Result:=WindowProc(Window, Msg, WParam, LParam);
    end;
  else
    Result:=WindowProc(Window, Msg, WParam, LParam);
  end;
end;

function LOpenGLCreateContext(AWinControl: TWinControl;
  WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
  DoubleBuffered, RGBA: boolean; const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  pfd: PIXELFORMATDESCRIPTOR;
  Info: PWGLControlInfo;
begin
  InitWGL;
  //InitOpenGLContextGLWindowClass;
  
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do begin
    pClassName := @ClsName;
    WindowTitle := StrCaption;
    SubClassWndProc := @GlWindowProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
  
  // create info
  Info:=AllocWGLControlInfo(Result);

  // create device context
  Info^.DC := GetDC(Result);
  if Info^.DC=0 then
    raise Exception.Create('LOpenGLCreateContext GetDC failed');

  // get pixelformat
  FillChar(pfd,SizeOf(pfd),0);
  with pfd do begin
    nSize:=sizeOf(pfd);
    nVersion:=1;
    dwFlags:=PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL;
    if DoubleBuffered then
      dwFlags:=dwFlags or PFD_DOUBLEBUFFER;
    if RGBA then
      iPixelType:=PFD_TYPE_RGBA
    else
      iPixelType:=PFD_TYPE_COLORINDEX;
    cColorBits:=24; // color depth
    cDepthBits:=16; // Z-Buffer
    iLayerType:=PFD_MAIN_PLANE;
  end;
  
  Info^.PixelFormat:=ChoosePixelFormat(Info^.DC,@pfd);
  if Info^.PixelFormat=0 then
    raise Exception.Create('LOpenGLCreateContext ChoosePixelFormat failed');

  // set pixel format in device context
  if not SetPixelFormat(Info^.DC,Info^.PixelFormat,@pfd) then
    raise Exception.Create('LOpenGLCreateContext SetPixelFormat failed');

  // create WGL context
  Info^.WGLContext:=wglCreateContext(Info^.DC);
  if Info^.WGLContext=0 then
    raise Exception.Create('LOpenGLCreateContext wglCreateContext failed');
end;

procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
var
  Info: PWGLControlInfo;
begin
  if not AWinControl.HandleAllocated then exit;
  Info:=GetWGLControlInfo(AWinControl.Handle);
  if Info=nil then exit;
  if wglMakeCurrent(Info^.DC,Info^.WGLContext) then begin
    wglDeleteContext(Info^.WGLContext);
    Info^.WGLContext:=0;
  end;
  if (Info^.DC<>0) then begin
    ReleaseDC(Info^.Window,Info^.DC);
  end;
  DisposeWGLControlInfo(Info^.Window);
end;

procedure InitWGL;
var
  Buffer: string;

  // Checks if the given Extension string is in Buffer.
  function CheckExtension(const extension : String) : Boolean;
  begin
    Result:=(Pos(extension, Buffer)>0);
  end;

begin
  if WGLInitialized then exit;
  WGLInitialized:=true;

  try
    // ARB wgl extensions
    Pointer(wglGetExtensionsStringARB) := GLGetProcAddress('wglGetExtensionsStringARB');
    Pointer(wglGetPixelFormatAttribivARB) := GLGetProcAddress('wglGetPixelFormatAttribivARB');
    Pointer(wglGetPixelFormatAttribfvARB) := GLGetProcAddress('wglGetPixelFormatAttribfvARB');
    Pointer(wglChoosePixelFormatARB) := GLGetProcAddress('wglChoosePixelFormatARB');

    Pointer(wglCreatePbufferARB) := GLGetProcAddress('wglCreatePbufferARB');
    Pointer(wglGetPbufferDCARB) := GLGetProcAddress('wglGetPbufferDCARB');
    Pointer(wglReleasePbufferDCARB) := GLGetProcAddress('wglReleasePbufferDCARB');
    Pointer(wglDestroyPbufferARB) := GLGetProcAddress('wglDestroyPbufferARB');
    Pointer(wglQueryPbufferARB) := GLGetProcAddress('wglQueryPbufferARB');

    Pointer(wglCreateBufferRegionARB) := GLGetProcAddress('wglCreateBufferRegionARB');
    Pointer(wglDeleteBufferRegionARB) := GLGetProcAddress('wglDeleteBufferRegionARB');
    Pointer(wglSaveBufferRegionARB) := GLGetProcAddress('wglSaveBufferRegionARB');
    Pointer(wglRestoreBufferRegionARB) := GLGetProcAddress('wglRestoreBufferRegionARB');

    // -EGG- ----------------------------
    Pointer(wglSwapIntervalEXT) := GLGetProcAddress('wglSwapIntervalEXT');
    Pointer(wglGetSwapIntervalEXT) := GLGetProcAddress('wglGetSwapIntervalEXT');

    // ARB wgl extensions
    if Assigned(wglGetExtensionsStringARB) then
      Buffer:=wglGetExtensionsStringARB(wglGetCurrentDC)
    else
      Buffer:='';
    WGL_ARB_multisample:=CheckExtension('WGL_ARB_multisample');
    WGL_EXT_swap_control:=CheckExtension('WGL_EXT_swap_control');
    WGL_ARB_buffer_region:=CheckExtension('WGL_ARB_buffer_region');
    WGL_ARB_extensions_string:=CheckExtension('WGL_ARB_extensions_string');
    WGL_ARB_pbuffer:=CheckExtension('WGL_ARB_pbuffer ');
    WGL_ARB_pixel_format:=CheckExtension('WGL_ARB_pixel_format');
    WGL_ATI_pixel_format_float:=CheckExtension('WGL_ATI_pixel_format_float');
  except
    on E: Exception do begin
      DebugLn('InitWGL ',E.Message);
    end;
  end;
end;

procedure InitOpenGLContextGLWindowClass;
begin
  if OpenGLContextWindowClassInitialized then exit;
  OpenGLContextWindowClassInitialized:=true;
  with OpenGLContextWindowClass do begin
    style:=CS_HREDRAW or CS_VREDRAW or CS_OWNDC;// Redraw On Move, And Own DC For Window
    lpfnWndProc  := @WindowProc;                // WndProc Handles Messages
    cbClsExtra   := 0;                          // No Extra Window Data
    cbWndExtra   := 0;                          // No Extra Window Data
    hInstance    := System.HInstance;           // Set The Instance
    hIcon        := LoadIcon(NULL, IDI_WINLOGO);// Load The Default Icon
    hCursor      := LoadCursor(NULL, IDC_ARROW);// Load The Arrow Pointer
    hbrBackground:= NULL;                       // No Background Required For GL
    lpszMenuName := nil;                       // We Don't Want A Menu
    lpszClassName:= 'LazOpenGLContext';         // Set The Class Name
  end;
  if RegisterClass(@OpenGLContextWindowClass)=0 then
    raise Exception.Create('registering OpenGLContextWindowClass failed');
end;

function AllocWGLControlInfo(Window: HWND): PWGLControlInfo;
begin
  New(Result);
  FillChar(Result^, sizeof(Result^), 0);
  Result^.Window := Window;
  if WGLControlInfoAtom=0 then
    WGLControlInfoAtom := Windows.GlobalAddAtom('WGLControlInfo');
  Windows.SetProp(Window, PChar(PtrUInt(WGLControlInfoAtom)), PtrUInt(Result));
end;

function DisposeWGLControlInfo(Window: HWND): boolean;
var
  Info: PWGLControlInfo;
begin
  Info := PWGLControlInfo(Windows.GetProp(Window,
                                          PChar(PtrUInt(WGLControlInfoAtom))));
  Result := Windows.RemoveProp(Window, PChar(PtrUInt(WGLControlInfoAtom)))<>0;
  if Result then begin
    Dispose(Info);
  end;
end;

function GetWGLControlInfo(Window: HWND): PWGLControlInfo;
begin
  Result:=PWGLControlInfo(Windows.GetProp(Window,
                                          PChar(PtrUInt(WGLControlInfoAtom))));
end;

end.

