unit KM_RenderControl;
{$I KaM_Remake.inc}

interface
uses Classes, Controls, ExtCtrls, Forms, KromOGLUtils
{$IFDEF MSWindows} , Windows {$ENDIF}
{$IFDEF Unix} , OpenGLContext, dglOpenGL {$ENDIF}
{$IFDEF WDC}, Messages {$ENDIF}
;

type
  TResizeEvent = procedure(aWidth, aHeight: Integer) of object;

  {$IFDEF MSWindows}
    //Custom flicker-free Panel
    TKMRenderControl = class(TPanel)
  {$ENDIF}
  {$IFDEF Unix}
    //TOpenGLControl is already flicker-free and supports multiple window managers
    TKMRenderControl = class(TOpenGLControl)
  {$ENDIF}
  private
    {$IFDEF MSWindows}
      h_DC: HDC;
      h_RC: HGLRC;
      //Flicker prevention stuff for Windows panel
      LastScreenPos: TPoint;
      procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
      procedure WMSize(var Message: TWMSize); message WM_SIZE;
      procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$ENDIF}
    {$IFDEF Unix}
      procedure DoOnResize; override;
    {$ENDIF}
  published
    property OnMouseWheel; //Required for Lazarus
  public
    OnRender: TNotifyEvent;
    OnResize: TResizeEvent;
    procedure CreateRenderContext;
    procedure DestroyRenderContext;
    {$IFDEF MSWindows}
      procedure SwapBuffers; //Already exists in TOpenGLControl for Unix
    {$ENDIF}
  end;



implementation
     

{ TKMRenderControl }

{$IFDEF MSWindows}
  procedure TKMRenderControl.WMEraseBkgnd(var Message: TWmEraseBkgnd);
  begin
    //Do not erase background, thats causing BG color flickering on repaint
    //Just tell it's done
    Message.Result := 1;
  end;


  procedure TKMRenderControl.WMPaint(var Message: TWMPaint);
  var
    PS: TPaintStruct;
    P: TPoint;
  begin
    P := ClientToScreen(Classes.Point(0, 0));
    //It is said to help with black borders bug in Windows
    if (LastScreenPos.X <> P.X) or (LastScreenPos.Y <> P.Y) then
    begin
      PostMessage(Handle, WM_SIZE, SIZE_RESTORED, Width + (Height shl 16));
      LastScreenPos := P;
    end;

    BeginPaint(Handle, PS);
    try
      if Assigned(OnRender) then OnRender(Self);
    finally
      EndPaint(Handle, PS);
    end;

    inherited;
  end;


  //Let the renderer know that client area size has changed ASAP
  //Handling it through VCL functions takes too long and Panel is already filled with bg color
  procedure TKMRenderControl.WMSize(var Message: TWMSize);
  begin
    //inherited;
    if Assigned(OnResize) then
      OnResize(Message.Width, Message.Height);
  end;


  procedure TKMRenderControl.SwapBuffers;
  begin
    Windows.SwapBuffers(h_DC);
  end;
{$ENDIF}


{$IFDEF Unix}
  procedure TKMRenderControl.DoOnResize;
  begin
    Inherited;
    if Assigned(OnResize) then
      OnResize(Width, Height);
  end;
{$ENDIF}


procedure TKMRenderControl.CreateRenderContext;
begin
  {$IFDEF MSWindows}
    SetRenderFrame(Handle, h_DC, h_RC);
    //Let us use glPrint to output OGL text
    BuildFont(h_DC, 16, FW_BOLD);
  {$ENDIF}
  {$IFDEF Unix}
    MakeCurrent;
    InitOpenGL;
    ReadExtensions;
    ReadImplementationProperties;
  {$ENDIF}
  SetRenderDefaults;
end;


procedure TKMRenderControl.DestroyRenderContext;
begin
  {$IFDEF MSWindows}
    wglMakeCurrent(h_DC, 0);
    wglDeleteContext(h_RC);
  {$ENDIF}
  {$IFDEF Unix}
    //Not needed?
  {$ENDIF}
end;


end.
