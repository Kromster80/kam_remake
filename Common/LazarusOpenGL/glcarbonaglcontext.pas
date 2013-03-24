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
unit GLCarbonAGLContext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLType, gl, Forms,
  MacOSAll,
  AGL, CarbonProc, CarbonDef, CarbonPrivate,
  WSLCLClasses, CarbonWSControls, CarbonUtils,
  Controls;

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
procedure LOpenGLSwapBuffers(Handle: HWND);
function LOpenGLMakeCurrent(Handle: HWND): boolean;
procedure LOpenGLClip(Handle: HWND);
function LOpenGLCreateContext(AWinControl: TWinControl;
              {%H-}WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
              DoubleBuffered, RGBA: boolean;
              const {%H-}AParams: TCreateParams): HWND;
procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
function CreateOpenGLContextAttrList(DoubleBuffered: boolean;
  RGBA: boolean): PInteger;


type
  TAGLControlInfo = record
    Control: ControlRef;
    WinControl: TWinControl;
    AGLContext: TAGLContext;
  end;
  PAGLControlInfo = ^TAGLControlInfo;

var
  AGLControlInfo_FOURCC: FourCharCode;

function CreateAGLControlInfo(Control: ControlRef; AGLContext: TAGLContext;
  WinControl: TWinControl): PAGLControlInfo;
function GetAGLControlInfo(Control: ControlRef): PAGLControlInfo;
procedure FreeAGLControlInfo(Control: ControlRef);
function GetAGLContext(Control: ControlRef): TAGLContext;

implementation

procedure LOpenGLViewport(Left, Top, Width, Height: integer);
begin
  glViewport(Left,Top,Width,Height);
end;

procedure LOpenGLSwapBuffers(Handle: HWND);
var
  AGLContext: TAGLContext;
begin
  AGLContext:=GetAGLContext(ControlRef(Handle));
  aglSwapBuffers(AGLContext);
end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
var
  AGLContext: TAGLContext;
  Info: PAGLControlInfo;
  Control: TCarbonCustomControl;
begin
  Control:=TCarbonCustomControl(Handle);
  Info:=GetAGLControlInfo(Control.Widget);
  if Info=nil then exit;
  AGLContext:=Info^.AGLContext;
  Result:=aglSetCurrentContext(AGLContext)<>0;
end;

procedure LOpenGLClip(Handle: HWND);
var
  AGLContext: TAGLContext;
  Info: PAGLControlInfo;
  Form: TCustomForm;
  Win: WindowRef;
  //b: Rect;
  clipRgn: RgnHandle;
  Control: TCarbonCustomControl;
begin
  Control:=TCarbonCustomControl(Handle);
  Info:=GetAGLControlInfo(Control.Widget);
  debugln(['LOpenGLMakeCurrent AAA0']);
  if Info=nil then exit;
  AGLContext:=Info^.AGLContext;
  aglSetCurrentContext(AGLContext);
  debugln(['LOpenGLMakeCurrent AAA1 ',dbgs(Handle)]);
  debugln(['LOpenGLMakeCurrent AAA2']);
  Form:=GetParentForm(Info^.WinControl);
  debugln(['LOpenGLMakeCurrent AAA3']);
  Win:=TCarbonWindow(Form.Handle).Window;

  debugln(['LOpenGLMakeCurrent ']);
  //GetWindowPortBounds(Win,b);
  clipRgn:=NewRgn;
  SetRectRgn(clipRgn,10,10,100,100);
  aglSetInteger(TAGLContext(GetWRefCon(Win)),AGL_CLIP_REGION,clipRgn);
  aglEnable(TAGLContext(GetWRefCon(Win)),AGL_CLIP_REGION);
  DisposeRgn(clipRgn);
end;

procedure ResizeOGLControl(AWidget: TCarbonWidget);
var
  info  : PAGLControlInfo;
  r     : array [0..3] of Integer;
  bnd   : HIRect;
  str   : MacOSAll.Rect;
  win   : WindowRef;
begin
  info:=GetAGLControlInfo(AWidget.Widget);
  if not Assigned(info) then Exit;
  win:=HIViewGetWindow(AWidget.Widget);
  if not Assigned(win) then Exit;
  GetWindowBounds(win, kWindowStructureRgn, str);
  HIViewGetBounds(AWidget.Widget, bnd);
  HIViewConvertPoint(bnd.origin, AWidget.Widget, nil);

  r[0]:=Round(bnd.origin.x);
  r[1]:=Round((str.bottom-str.top)- bnd.origin.y-bnd.size.height);
  r[2]:=Round(bnd.size.width);
  r[3]:=Round(bnd.size.height);

  aglEnable(info^.aglContext, AGL_BUFFER_RECT);
  aglSetInteger(info^.aglContext, AGL_BUFFER_RECT, @r[0]);
end;

function CarbonGLControl_Resize(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  Result:=CallNextEventHandler(ANextHandler, AEvent);
  ResizeOGLControl(AWidget);
end;

function CarbonGLControl_WindowChange(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  win : WindowRef;
begin
  Result:=CallNextEventHandler(ANextHandler, AEvent);
  if GetEventParameter(AEvent, kEventParamControlCurrentOwningWindow, typeWindowRef,
      nil, sizeof(win), nil, @win) = noErr then
    if Assigned(win) then ResizeOGLControl(AWidget);
end;

function LOpenGLCreateContext(AWinControl: TWinControl;
  WSPrivate: TWSPrivateClass; SharedControl: TWinControl;
  DoubleBuffered, RGBA: boolean;
  const AParams: TCreateParams): HWND;
var
  disp: GDHandle;
  aglPixFmt: TAGLPixelFormat;
  aglContext: TAGLContext;
  Control: TCarbonCustomControl;
  AttrList: PInteger;
  C: TCreateParams;
  AGLInfo: PAGLControlInfo;
  TempSpec: EventTypeSpec;
begin
  Result:=0;
  if AWinControl.Parent=nil then
    RaiseGDBException('GLCarbonAGLContext.LOpenGLCreateContext no parent');

  C.X := AWinControl.Left;
  C.Y := AWinControl.Top;
  C.Width := AWinControl.Width;
  C.Height := AWinControl.Height;
  // create a custom control
  Control := TCarbonCustomControl.Create(AWinControl, C);
  //debugln(['LOpenGLCreateContext ',dbgsName(Control)]);

  // create the AGL context
  disp := GetMainDevice ();
  AttrList:=CreateOpenGLContextAttrList(DoubleBuffered,RGBA);
  aglPixFmt := aglChoosePixelFormat (@disp, 1, AttrList);
  System.FreeMem(AttrList);
  aglContext := aglCreateContext (aglPixFmt, NIL);
  aglDestroyPixelFormat(aglPixFmt);

  aglSetDrawable(aglContext,
    GetWindowPort(TCarbonWindow(GetParentForm(AWinControl).Handle).Window));
  AGLControlInfo_FOURCC := MakeFourCC('ACI ');

  AGLInfo:=CreateAGLControlInfo(Control.Widget, AGLContext, AWinControl);
  if AGLInfo<>GetAGLControlInfo(Control.Widget) then
    RaiseGDBException('GLCarbonAGLContext.LOpenGLCreateContext inconsistency');

  ResizeOGLControl(Control);
  TempSpec:=MakeEventSpec(kEventClassControl, kEventControlBoundsChanged);
  InstallControlEventHandler(Control.Widget, RegisterEventHandler(@CarbonGLControl_Resize),
    1, @TempSpec, Control, nil);
  TempSpec:=MakeEventSpec(kEventClassControl, kEventControlOwningWindowChanged);
  // The control might be embeded into a window, after the its creation
  // the example of the situation is give at bug report #17244
  InstallControlEventHandler(Control.Widget, RegisterEventHandler(@CarbonGLControl_WindowChange),
    1, @TempSpec, Control, nil);

  Result:=HWnd(Control);
  //debugln(['LOpenGLCreateContext ',dbgs(Result)]);
end;

procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
var
  Ref: ControlRef;
  Info: PAGLControlInfo;
begin
  if not AWinControl.HandleAllocated then exit;
  Ref := ControlRef(TCarbonControl(AWinControl.Handle).Widget);
  Info := GetAGLControlInfo(Ref);
  if Info=nil then exit;
  aglDestroyContext(Info^.AGLContext);
  Info^.AGLContext := nil;
  FreeAGLControlInfo(Ref);
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
    Add(AGL_WINDOW);
    if DoubleBuffered then
      Add(AGL_DOUBLEBUFFER);
    if RGBA then
      Add(AGL_RGBA);
    Add(AGL_NO_RECOVERY);
    Add(AGL_MAXIMUM_POLICY);
    Add(AGL_SINGLE_RENDERER);
    Add(AGL_RED_SIZE); Add(1);
    Add(AGL_GREEN_SIZE); Add(1);
    Add(AGL_BLUE_SIZE); Add(1);
    Add(AGL_DEPTH_SIZE); Add(1);
    Add(AGL_NONE);
  end;

begin
  Result:=nil;
  p:=0;
  CreateList;
  GetMem(Result,SizeOf(integer)*p);
  p:=0;
  CreateList;
end;

function CreateAGLControlInfo(Control: ControlRef; AGLContext: TAGLContext;
  WinControl: TWinControl): PAGLControlInfo;
begin
  New(Result);
  FillByte(Result^, SizeOf(Result^), 0);
  Result^.Control:=Control;
  Result^.WinControl:=WinControl;
  Result^.AGLContext:=AGLContext;

  SetControlProperty(Control, LAZARUS_FOURCC, AGLControlInfo_FOURCC,
                     SizeOf(Result), @Result);
end;

function GetAGLControlInfo(Control: ControlRef): PAGLControlInfo;
var
  m: LongWord;
begin
  GetControlProperty(Control, LAZARUS_FOURCC, AGLControlInfo_FOURCC,
                     SizeOf(Result), @m, @Result);
end;

procedure FreeAGLControlInfo(Control: ControlRef);
var
  Info: PAGLControlInfo;
begin
  Info:=GetAGLControlInfo(Control);
  if Info=nil then exit;
  RemoveControlProperty(Control, LAZARUS_FOURCC, AGLControlInfo_FOURCC);
  System.FreeMem(Info);
end;

function GetAGLContext(Control: ControlRef): TAGLContext;
begin
  Result:=GetAGLControlInfo(TCarbonCustomControl(Control).Widget)^.AGLContext;
end;

initialization

finalization

end.

