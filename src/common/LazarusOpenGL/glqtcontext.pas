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
unit GLQTContext;

{$mode objfpc}{$H+}
{$LinkLib GL}
{$PACKRECORDS C}

interface

uses
  Classes, SysUtils, Controls, LCLProc, LCLType, X, XUtil, XLib, gl,
  InterfaceBase,
  WSLCLClasses,
  // Bindings
  qt4,
  qtwidgets, qtobjects, qtproc, qtint,
  QtWSControls;

// gdkgl

const
// enum _QT_GL_CONFIGS
  QT_GL_NONE                           = 0;
  QT_GL_USE_GL                         = 1;
  QT_GL_BUFFER_SIZE                    = 2;
  QT_GL_LEVEL                          = 3;
  QT_GL_RGBA                           = 4;
  QT_GL_DOUBLEBUFFER                   = 5;
  QT_GL_STEREO                         = 6;
  QT_GL_AUX_BUFFERS                    = 7;
  QT_GL_RED_SIZE                       = 8;
  QT_GL_GREEN_SIZE                     = 9;
  QT_GL_BLUE_SIZE                      = 10;
  QT_GL_ALPHA_SIZE                     = 11;
  QT_GL_DEPTH_SIZE                     = 12;
  QT_GL_STENCIL_SIZE                   = 13;
  QT_GL_ACCUM_RED_SIZE                 = 14;
  QT_GL_ACCUM_GREEN_SIZE               = 15;
  QT_GL_ACCUM_BLUE_SIZE                = 16;
  QT_GL_ACCUM_ALPHA_SIZE               = 17;

  // GLX_EXT_visual_info extension
  QT_GL_X_VISUAL_TYPE_EXT              = $22;
  QT_GL_TRANSPARENT_TYPE_EXT           = $23;
  QT_GL_TRANSPARENT_INDEX_VALUE_EXT    = $24;
  QT_GL_TRANSPARENT_RED_VALUE_EXT      = $25;
  QT_GL_TRANSPARENT_GREEN_VALUE_EXT    = $26;
  QT_GL_TRANSPARENT_BLUE_VALUE_EXT     = $27;
  QT_GL_TRANSPARENT_ALPHA_VALUE_EXT    = $28;

type
  TGLXContext = pointer;

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
procedure LOpenGLSwapBuffers(Handle: HWND);
function LOpenGLMakeCurrent(Handle: HWND): boolean;
function LOpenGLCreateContext(AWinControl: TWinControl;
             WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
             DoubleBuffered, RGBA: boolean;
             const AParams: TCreateParams): HWND;
procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
function CreateOpenGLContextAttrList(DoubleBuffered: boolean;
                                     RGBA: boolean): PInteger;

implementation

type
  //PGLXPixmap = ^GLXPixmap;
  GLXPixmap = TXID;

  //PGLXDrawable = ^GLXDrawable;
  GLXDrawable = TXID;

{ GLX 1.0 functions. }

function glXChooseVisual(dpy:PDisplay; screen:longint; attrib_list:Plongint):PXVisualInfo;cdecl;external;
procedure glXCopyContext(dpy:PDisplay; src:TGLXContext; dst:TGLXContext; mask: cardinal);cdecl;external;
function glXCreateContext(dpy:PDisplay; vis:PXVisualInfo; share_list:TGLXContext; direct:TBool):TGLXContext;cdecl;external;
function glXCreateGLXPixmap(dpy:PDisplay; vis:PXVisualInfo; pixmap:TPixmap):GLXPixmap;cdecl;external;
procedure glXDestroyContext(dpy:PDisplay; ctx:TGLXContext);cdecl;external;
procedure glXDestroyGLXPixmap(dpy:PDisplay; pix:GLXPixmap);cdecl;external;
function glXGetConfig(dpy:PDisplay; vis:PXVisualInfo; attrib:longint; value:Plongint):longint;cdecl;external;
function glXGetCurrentContext:TGLXContext;cdecl;external;
function glXGetCurrentDrawable:GLXDrawable;cdecl;external;
function glXIsDirect(dpy:PDisplay; ctx:TGLXContext):TBool;cdecl;external;
function glXMakeCurrent(dpy:PDisplay; drawable:GLXDrawable; ctx:TGLXContext):TBool;cdecl;external;
function glXQueryExtension(dpy:PDisplay; error_base:Plongint; event_base:Plongint):TBool;cdecl;external;
function glXQueryVersion(dpy:PDisplay; major:Plongint; minor:Plongint):TBool;cdecl;external;
procedure glXSwapBuffers(dpy:PDisplay; drawable:GLXDrawable);cdecl;external;
procedure glXUseXFont(font:TFont; first:longint; count:longint; list_base:longint);cdecl;external;
procedure glXWaitGL;cdecl;external;
procedure glXWaitX;cdecl;external;

function XVisualAsString(AVisual: PVisual): string;
begin
  if AVisual=nil then begin
    Result:='nil';
  end else begin
    Result:=''
        +' bits_per_rgb='+dbgs(AVisual^.bits_per_rgb)
        +' red_mask='+hexstr(AVisual^.red_mask,8)
        +' green_mask='+hexstr(AVisual^.green_mask,8)
        +' blue_mask='+hexstr(AVisual^.blue_mask,8)
        +' map_entries='+dbgs(AVisual^.map_entries)
        +'';
  end;
end;

function XDisplayAsString(ADisplay: PDisplay): string;
begin
  if ADisplay=nil then begin
    Result:='nil';
  end else begin
    Result:=''
        +'';
  end;
end;

type

  { TQtGLWidget }

  TQtGLWidget = class(TQtWidget)
  public
    xdisplay: PDisplay;
    visual: PXVisualInfo;
    glxcontext: TGLXContext;
    ref_count: integer;
    function GetGLXDrawable: GLXDrawable;
  end;

{ TQtGLWidget }

function TQtGLWidget.GetGLXDrawable: GLXDrawable;
begin
  Result:=0; {$ERROR ToDo: get GLXDrawable}
end;

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
begin
  glViewport(Left,Top,Width,Height);
end;

procedure LOpenGLSwapBuffers(Handle: HWND);
var
  Widget: TQtGLWidget;
begin
  if Handle=0 then
    RaiseGDBException('LOpenGLSwapBuffers Handle=0');

  Widget:=TQtGLWidget(Handle);
  glXSwapBuffers(Widget.xdisplay,
                 Widget.GetGLXDrawable
                 );
end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
var
  Widget: TQtGLWidget;
begin
  if Handle=0 then
    RaiseGDBException('LOpenGLSwapBuffers Handle=0');
  Result:=false;

  Widget:=TQtGLWidget(Handle);
  Result:=boolean(glXMakeCurrent(Widget.xdisplay,
                                 Widget.GetGLXDrawable,
                                 Widget.glxcontext));
end;

function LOpenGLCreateContext(AWinControl: TWinControl;
  WSPrivate: TWSPrivateClass; SharedControl: TWinControl; DoubleBuffered,
  RGBA: boolean; const AParams: TCreateParams): HWND;
var
  AttrList: PInteger;
  NewQtWidget: TQtGLWidget;
  direct: TBool;
begin
  if WSPrivate=nil then ;
  AttrList:=CreateOpenGLContextAttrList(DoubleBuffered,RGBA);
  try
    NewQtWidget:=TQtGLWidget.Create(AWinControl,AParams);
    NewQtWidget.xdisplay := QX11Info_display;
    NewQtWidget.visual:=glXChooseVisual(NewQtWidget.xdisplay,
      DefaultScreen(NewQtWidget.xdisplay), @attrList[0]);
    direct:=0;
    NewQtWidget.glxcontext := glXCreateContext(NewQtWidget.xdisplay,
                                               NewQtWidget.visual, nil, direct);
    NewQtWidget.ref_count := 1;

    NewQtWidget.AttachEvents;

    Result:=HWND(PtrUInt(Pointer(NewQtWidget)));
  finally
    FreeMem(AttrList);
  end;
end;

procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
begin
  if not AWinControl.HandleAllocated then exit;
  // nothing to do
end;

function CreateOpenGLContextAttrList(DoubleBuffered: boolean; RGBA: boolean
  ): PInteger;
var
  p: integer;

  procedure Add(i: integer);
  begin
    if Result<>nil then
      Result[p]:=i;
    inc(p);
  end;

  procedure CreateList;
  begin
    if DoubleBuffered then
      Add(QT_GL_DOUBLEBUFFER);
    if RGBA then
      Add(QT_GL_RGBA);
    Add(QT_GL_RED_SIZE);  Add(1);
    Add(QT_GL_GREEN_SIZE);  Add(1);
    Add(QT_GL_BLUE_SIZE);  Add(1);
    Add(QT_GL_DEPTH_SIZE);  Add(1);
    Add(QT_GL_STENCIL_SIZE); Add(1);
    Add(QT_GL_None);
  end;

begin
  Result:=nil;
  p:=0;
  CreateList;
  GetMem(Result,SizeOf(integer)*p);
  p:=0;
  CreateList;
end;

end.

