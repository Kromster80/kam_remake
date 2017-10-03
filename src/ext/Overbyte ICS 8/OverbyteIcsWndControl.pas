{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Octobre 2002
Description:  Composant non-visuel avec un handle de fenêtre.
Version:      8.04
EMail:        francois.piette@overbyte.be   http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2002-2014 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

How it works:
-------------
  TIcsWndHandler is a class that encapsulate a windows message queue and a
  message map. A message map in an array of message handlers indexed by a
  message number.

  Message numbers are returned upon request by calling AllocateMsgHandler.
  When a message number is no more needed, it has to be freed by calling
  UnregisterMessage.

  TIcsWndHandlerPool is a class that encapsulate a list of list of TIcsWndHandler.
  There is a list for each thread because each thread has his own message
  queue and need hidden window distinct from other threads.

  TIcsWndHandlerPool.GetWndHandler is used to get a WndHandler handling messages
  for a given thread. FreeWndHandler must be called when the handler is not
  needed anymore.

  TIcsWndControl use TIcsWndHandlerPool to register as much messages as it needs
  for his own use.

Historique:
29/04/2006 V1.01 Éric Fleming Bonilha <eric@digifort.com.br> found and fixed
                 a memory leak in FreeWndHandler.
19/08/2006 V1.02 Revised for .NET
29/10/2006 V1.03 Added global variable GWndHandlerMsgLow
30/10/2006 V1.04 Made TIcsWndControl.ThreadID property public and clear it
                 in DeallocateWnd. ThreadID is the ID of the thread owning
                 the hidden window, if any is allocated.
25/03/2007 V1.05 A. Garrels added TIcsTimer.
03/06/2007 V1.06 TIcsWndHandler got a new public event OnMessage (suggested by
                 Primož Gabrijelcic <primoz@gabrijelcic.org>). D7 code explorer
                 displays all classes again. 
21/07/2007 V1.07 Updated TIcsTimer for .NET environment.
01/05/2008 V1.08 A. Garrels - Function names adjusted according to changes in
                 OverbyteIcsLibrary.pas.
15/11/2008 V1.09 Olivier Sannier improved unit finalization, comments in source.
21/07/2009 V1.10 A. Garrels modified TIcsTimer a bit.
19/08/2010 V1.11 A. Garrels - Added TIcsWndHandlerPool.SetWndHandlerMaxMsgIDs()
                 to provide an option to change default maximum number of
                 message IDs per WndHandler at runtime.
                 TIcsWndHandler.UnregisterMessage removes all messages
                 with the unregistered message ID from the message queue now.
                 Resolved an Error Insight false positive in TIcsWndhandler.WndProc.
                 An assertion error is now raised if MsgHandlersCount exceeded
                 the maximum number of message IDs per WndHandler.
10/10/2010 V1.12 Arno - MessagePump changes/fixes.
08/11/2010 V1.14 Arno - Improved final exception handling. It's now possible to
                 specify how background exceptions (unhandled exceptions) are
                 treated on a per thread basis. Call setter function
                 SetIcsThreadLocalFinalBgExceptionHandling() to enable one of
                 the following options in current thread context. With
                 "fehAppHandleException" unhandled exceptions are passed to the
                 Application exception handler if available, with
                 "fehShowException" unhandled exceptions are displayed either
                 in the console or through Windows MessageBox API (owner HWND = 0).
                 Both options allow tools like MadExcept to catch and display
                 the exception, with "fehNone" (default) unhandled exceptions
                 are thrown away silently.
15/04/2011 V1.15 Arno prepared for 64-bit.
06/05/2011 V1.16 Arno - Make use of type TThreadID.
16/08/2011 V1.17 Arno TIcsTimer prepared for x64.µ
13/05/2012 V1.18 FPiette created InitializeGlobalHandler and GUnitInitialized
                 to check if we passed thru the initialization section which
                 is not the case when a TWSocket is dropped on a fmx form.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
15/12/2012 V8.01 (Posix only) Arno reset FHandle and FThreadID to zero in
                 TIcsWndControl.DeallocateHWnd.
Aug 18, 2013 V8.02 Arno added some default property specifiers.
Jul 9, 2014  V8.03 Angus break MessageLoop for Terminated flag,
                       suggested by Wolfgang Prinzjakowitsch
Jan 22, 2016 V8.04 Angus fixed 64-bit bug in UpdateTimer

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsWndControl;
{$ENDIF}

{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF VER_UNKNOWN}
  { Emit a single warning (see OverbyteIcsDefs.inc), this unit seems the right place }
  {$MESSAGE WARN 'You are compiling ICS with an unknown and untested compiler version!'}
{$ENDIF}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
  Ics.Posix.WinTypes,
  Ics.Posix.Messages,
{$ENDIF POSIX}
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
{$IFNDEF NOFORMS}
  {$IFDEF FMX}
    FMX.Forms,
  {$ELSE}
    {$IFDEF RTL_NAMESPACES}Vcl.Forms{$ELSE}Forms{$ENDIF},
  {$ENDIF}
{$ENDIF}
  OverbyteIcsTypes;

const
  TIcsWndControlVersion  = 804;
  CopyRight : String     = ' TIcsWndControl (c) 2002-2016 F. Piette V8.04 ';

  IcsWndControlWindowClassName = 'IcsWndControlWindowClass';

type
  TIcsBgExceptionEvent = procedure (Sender       : TObject;
                                    E            : Exception;
                                    var CanClose : Boolean) of object;
  TIcsMessageEvent =   procedure (Sender      : TObject;
                                  var MsgRec  : TMessage;
                                  var Handled : Boolean) of object; //Gp//AG
  TIcsFinalBgExceptionHandling = (fehNone, fehAppHandleException,  { V1.14 }
    fehShowException);


  EIcsException        = class(Exception);
  TIcsWndControl       = class;
{$IFDEF MSWINDOWS}
  TIcsWndHandlerList   = class;
{$ENDIF}
  TIcsWndHandler       = class;

  EIcsTimerException   = class(Exception);
  TIcsTimer = class(TComponent)                             {AG 03/25/07}
  protected
    FUID: INT_PTR;
    FInterval: Cardinal;
    FEnabled: Boolean;
    FOnTimer: TNotifyEvent;
    FLastEvent: Cardinal;
    FIcsWndControl: TIcsWndControl;
    procedure UpdateTimer; virtual;
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure SetEnabled(const Value: Boolean);
    procedure CheckWindowAvailable;
    procedure WMTimer(var msg: TMessage); virtual;
  public
    constructor Create(AOwner: TIcsWndControl); reintroduce;
    destructor Destroy; override;
    property IcsWndControl: TIcsWndControl read FIcsWndControl;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

  { Specifies an additional method that is called when an exception        }
  { is handled by HandleBackGroundException and CanClose parameter is set. }
  TIcsExceptAbortProc = procedure of object;                       { V1.14 }


  TIcsWndControl = class(TComponent)
  private
    Disposed       : Boolean;  // Track whether Dispose has been called.
  {$IFDEF POSIX}
    FMessagePump   : TIcsMessagePump;
  {$ENDIF}
  protected
    FHandle        : HWND;
    FThreadId      : TThreadID;
    FTerminated    : Boolean;
    FMultiThreaded : Boolean;
    FWndHandler    : TIcsWndHandler;
    FMsgRelease    : UINT;
    FOnBgException : TIcsBgExceptionEvent;
    FOnMessagePump : TNotifyEvent;
    FExceptAbortProc : TIcsExceptAbortProc;  { V1.14 }
    procedure   Dispose(Disposing: Boolean); virtual;
    procedure   SetMultiThreaded(const Value: Boolean); virtual;
    function    GetTerminated: Boolean; virtual;
    procedure   SetTerminated(const Value: Boolean); virtual;
    procedure   SetOnMessagePump(const Value: TNotifyEvent); virtual;
    procedure   SetOnBgException(const Value: TIcsBgExceptionEvent); virtual; { V1.14 }
    procedure   WndProc(var MsgRec: TMessage); virtual;
    procedure   HandleBackGroundException(E : Exception); virtual;
    procedure   TriggerBgException(E            : Exception;
                                   var CanClose : Boolean); virtual;
    procedure   WMRelease(var msg: TMessage); virtual;
    procedure   AllocateHWnd; virtual;
    procedure   DeallocateHWnd; virtual;
    function    GetHandle: HWND;
    function    MsgHandlersCount: Integer; virtual;
    procedure   AllocateMsgHandlers; virtual;
    procedure   FreeMsgHandlers; virtual;
    procedure   AbortComponent; virtual;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Release; virtual;
    procedure   ThreadAttach; virtual;
    procedure   ThreadDetach; virtual;
    procedure   MessageLoop; virtual;
    function    ProcessMessage : Boolean; virtual;
    procedure   ProcessMessages; virtual;
    procedure   MessagePump; virtual;
    function    PostQuitMessage: Boolean;
    property MultiThreaded   : Boolean                read  FMultiThreaded
                                                      write SetMultiThreaded
                                                      default False;
    property Terminated      : Boolean                read  GetTerminated
                                                      write SetTerminated;
    property OnMessagePump   : TNotifyEvent           read  FOnMessagePump
                                                      write SetOnMessagePump;
    property Handle          : HWND                   read  GetHandle;  // R/O
    property WndHandler      : TIcsWndHandler         read  FWndHandler
                                                      write FWndHandler;
    property ThreadID        : TThreadID              read  FThreadID;
    property OnBgException   : TIcsBgExceptionEvent   read  FOnBgException
                                                      write SetOnBgException; { V1.14 }
    property ExceptAbortProc : TIcsExceptAbortProc    read  FExceptAbortProc  { V1.14 }
                                                      write FExceptAbortProc;
  end;

{$IFDEF POSIX}
  TIcsWndHandler = class
  private
    FLastMessage: UINT;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   UnregisterMessage(var Msg : UINT);
    function    AllocateMsgHandler(Obj: TIcsWndControl) : UINT;
  end;
{$ENDIF}

{$IFDEF MSWINDOWS}
  TIcsMsgMap = array of TIcsWndControl;

  TIcsWndHandler = class(TObject)
  protected
    FHandle        : HWND;
    FMsgMap        : TIcsMsgMap;
    FMsgLow        : UINT;
    FMsgCnt        : Integer;
    FOwnerList     : TIcsWndHandlerList;
    FOnBgException : TIcsBgExceptionEvent;
    FOnMessage     : TIcsMessageEvent;
    function  TriggerMessage(var MsgRec  : TMessage) : Boolean;
    procedure WndProc(var MsgRec: TMessage);
    procedure AllocateHWnd;
    procedure DeallocateHWnd;
    procedure TriggerBgException(E: Exception; var CanClose : Boolean);
    function  GetMsgLeft: UINT;
  public
    constructor Create;
    procedure   RegisterMessage(Msg : UINT; Obj: TIcsWndControl);
    procedure   UnregisterMessage(var Msg : UINT);
    function    AllocateMsgHandler(Obj: TIcsWndControl) : UINT;
    property  Handle    : HWND     read  FHandle;
    property  MsgLow    : UINT     read  FMsgLow      write FMsgLow;
    property  MsgLeft   : UINT     read  GetMsgLeft;
    property  OnMessage : TIcsMessageEvent
                                   read FOnMessage    write FOnMessage; //Gp//AG
  end;

  TIcsWndHandlerList = class(TList)
  protected
    ThreadID : TThreadID;
  end;

  TWhMaxMsgIDs = 50..1000;

  TIcsWndHandlerPool = class(TObject)
  private
    FList     : TIcsWndHandlerList;
    FCritSect : TRTLCriticalSection;
  public
    constructor Create;
    destructor  Destroy; override;
    function    GetWndHandler(HandlerCount : UINT;
                              ThreadID     : TThreadID): TIcsWndHandler;
    procedure   FreeWndHandler(var WndHandler : TIcsWndHandler);
    procedure   Lock;
    procedure   UnLock;
    procedure   SetWndHandlerMaxMsgIDs(const Value: TWhMaxMsgIDs);
  end;
{$ENDIF MSWINDOWS}

  TOutputDebugStringType = PChar;

{ Since use of threadvars is not possible across packages/Dlls we provide a
  setter and getter to set final background exception handling per thread
  context. The default value is fehNone. } { V1.14 }
procedure SetIcsThreadLocalFinalBgExceptionHandling(
    const Value: TIcsFinalBgExceptionHandling);
function GetIcsThreadLocalFinalBgExceptionHandling: TIcsFinalBgExceptionHandling;

var
{$IFDEF MSWINDOWS}
  GWndHandlerPool     : TIcsWndHandlerPool;
  GWndHandleCount     : Integer;
  GWndHandlerCritSect : TRTLCriticalSection;
  // GWndHandlerMsgLow can be initialized before any ICS component is created
  // Typically it is done from the project source file.
  // If you don't initialize it, it will be initialized to WM_USER + 1 the
  // first time it is needed.
  // Why would you initialize it ? Because you are using some messages for your
  // own purpose and you have collision with messages allocated by ICS
  // components. ICS use this variable as starting value for his lowest
  // message number. The messages are thus numbered from GWndHandlerMsgLow to
  // the GWndHandlerMsgLow + WH_MAX_MSG
  GWndHandlerMsgLow   : Integer;
{$ENDIF MSWINDOWS}

  // Will be used by the last socket to know if it must cleanup the global
  // handler because the unit has been finalized before the socket was diposed
  GUnitFinalized: Boolean; { V1.09 }

  // Is used to assert if we have been initialized
  GUnitInitialized: Integer; { V1.18 }

implementation

uses
{$IFDEF FMX}
  Ics.Fmx.OverbyteIcsThreadTimer,
{$ELSE}
  OverbyteIcsThreadTimer,
{$ENDIF}
  OverbyteIcsUtils;

var
  GUIDOffSet   : Integer;
  G_WH_MAX_MSG : Word = 100;

threadvar
  // Initialized by the RTL to zero = fehNone
  IcsFinalBgExceptionHandling : TIcsFinalBgExceptionHandling; { V1.14 }


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} { V1.14 }
procedure SetIcsThreadLocalFinalBgExceptionHandling(
  const Value: TIcsFinalBgExceptionHandling);
begin
    IcsFinalBgExceptionHandling := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} { V1.14 }
function GetIcsThreadLocalFinalBgExceptionHandling: TIcsFinalBgExceptionHandling;
begin
    Result := IcsFinalBgExceptionHandling;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
procedure InitializeGlobalHandler;   { V1.18 }
begin
    // Check if already initialized
    if InterlockedExchange(GUnitInitialized, 1) <> 0 then
        Exit;
    // Not yet initialized, do it right now V1.18
    GWndHandlerPool  := TIcsWndHandlerPool.Create;
    InitializeCriticalSection(GWndHandlerCritSect);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure FinalizeGlobalHandler; { V1.09 }
begin
    // If the list is empty, we can safely delete the global handler,
    // else we leave it up to the last socket to do the cleanup
    GWndHandlerPool.Lock;
    try
        if GWndHandlerPool.FList.Count > 0 then
            Exit;
    finally
        GWndHandlerPool.UnLock;
    end;
    GWndHandlerPool.Free;
    GWndHandlerPool := nil;
    DeleteCriticalSection(GWndHandlerCritSect);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// WndControlWindowsProc is a callback function used for message handling
function WndControlWindowsProc(
    ahWnd   : HWND;
    auMsg   : UINT;
    awParam : WPARAM;
    alParam : LPARAM): LRESULT; stdcall;
var
    Obj    : TObject;
    MsgRec : TMessage;
begin
    // if IsConsole then WriteLn('MSG = ', auMsg);
    // When the window is created, we receive the following messages:
    // #129 WM_NCCREATE
    // #131 WM_NCCALCSIZE
    // #1   WM_CREATE
    // #5   WM_SIZE
    // #3   WM_MOVE
    // Later we receive:
    // #28  WM_ACTIVATEAPP
    // When the window is destroyed we receive
    // #2   WM_DESTROY
    // #130 WM_NCDESTROY

    // When the window was created, we stored a reference to the object
    // into the storage space we asked windows to have
{$IFDEF WIN64}
    Obj := TObject(GetWindowLongPtr(ahWnd, 0));
{$ELSE}
    Obj := TObject(GetWindowLong(ahWnd, 0));
{$ENDIF}
    // Check if the reference is actually our object type
    if not (Obj is TIcsWndHandler) then
        Result := DefWindowProc(ahWnd, auMsg, awParam, alParam)
    else begin
        // Internally, Delphi use TMessage to pass parameters to his
        // message handlers.
        MsgRec.Msg    := auMsg;
        MsgRec.wParam := awParam;
        MsgRec.lParam := alParam;
        TIcsWndHandler(Obj).WndProc(MsgRec);
        Result := MsgRec.Result;
    end;
end;
{$ENDIF MSWINDOWS}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// MsgHandlersCount _must_ be overridden in derived classes to adjust the number
// of message handlers needed.
// The overridden method should looks like this:
// function TCustomWSocket.MsgHandlersCount : Integer;
// begin
//     Result := 7 +                           // New MsgHandlers count
//               inherited MsgHandlersCount;   // Count for inherited
// end;
function TIcsWndControl.MsgHandlersCount : Integer;
begin
    Result := 1;  // For FMsgRelease
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// AllocateMsgHandlers _must_ be overridden in derived classes to allocate
// new MsgHandlers. Don't forget to call the inherited one first !
procedure TIcsWndControl.AllocateMsgHandlers;
begin
    FMsgRelease := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// FreeMsgHandlers _must_ be overridden in derived classes to free
// allocated MsgHandlers. Don't forget to call the inherited one first !
procedure TIcsWndControl.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then
        FWndHandler.UnregisterMessage(FMsgRelease);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// AllocateHWnd is used to allocate a window handle to the component.
// remember window handles are shared by TIcsWndControl derived component.
// Each derived component has to register his own message numbers.
// This is done by overriding AllocateMsgHandlers in the derived component.
procedure TIcsWndControl.AllocateHWnd;
begin
    if FHandle <> 0 then
        Exit;              // Already done
{$IFDEF MSWINDOWS}
    // Despite the precautions taken not to destroy GWndHandlerPool via the
    // finalization when a socket is still there, we could still be going
    // through this code in the following situation:
    //
    //   Application shuts down
    //   A thread is still running and creates a new socket
    //
    // Note that this is highly unlikely, but as a courtesy to developers
    // we raise an assertion error when this happens.
    // The solution to this problem is to make sure that all threads are
    // finished before the application is terminated itself.
    Assert(not GUnitFinalized, 'Unit is already finalized, check your threads!!!'); { V1.09 }
    InitializeGlobalHandler; { V1.18 }

    FThreadId := GetCurrentThreadId;
    GWndHandlerPool.Lock;
    try
        if not Assigned(FWndHandler) then
            FWndHandler := GWndHandlerPool.GetWndHandler(MsgHandlersCount,
                                                         FThreadId);
        FWndHandler.AllocateHWnd;
        FHandle := FWndHandler.Handle;
        AllocateMsgHandlers;
    finally
        GWndHandlerPool.UnLock;
    end;
{$ELSE MSWINDOWS}
    FMessagePump := TIcsMessagePump.Create;
    FThreadId := FMessagePump.ThreadId;
    FHandle := Ics.Posix.Messages.AllocateHWND(WndProc);
    AllocateMsgHandlers;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Deallocate the window handle for the component. Remember that a window handle
// is shared by several component. The actual hidden window is freed when the
// last component has deallocated.
procedure TIcsWndControl.DeallocateHWnd;
begin
    if FHandle = 0 then
        Exit;              // Already done
{$IFDEF MSWINDOWS}
    GWndHandlerPool.Lock;
    try
        FreeMsgHandlers;
        if Assigned(FWndHandler) and (FWndHandler.FMsgCnt <= 0) then
            GWndHandlerPool.FreeWndHandler(FWndHandler);
        FHandle     := 0;
        FWndHandler := nil;
        FThreadID   := 0;
    finally
        GWndHandlerPool.UnLock;
    end;
    if GUnitFinalized then     { V1.09 }
        FinalizeGlobalHandler; { V1.09 }
{$ELSE MSWINDOWS}
    FreeMsgHandlers;
    Ics.Posix.Messages.DestroyWindow(FHandle);
    FHandle := 0;
    FWndHandler.FLastMessage := WM_USER;
    FreeAndNil(FMessagePump);
    FThreadID := 0;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndControl.ThreadAttach;
begin
    if FHandle <> 0 then
        raise EIcsException.Create('Cannot attach when not detached');
    Self.AllocateHWnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndControl.ThreadDetach;
begin
    if IcsGetCurrentThreadID <> FThreadID then
        raise EIcsException.Create('Cannot detach from another thread');
    Self.DeallocateHWnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Loop thru message processing until the WM_QUIT message is received        }
{ This is intended for multithreaded application using TWSocket.            }
{ MessageLoop is different from ProcessMessages because it actually block   }
{ if no message is available. The loop is broken when WM_QUIT is retrieved. }
procedure TIcsWndControl.MessageLoop;
{$IFDEF MSWINDOWS}
var
    MsgRec : TMsg;
begin
    { If GetMessage retrieves the WM_QUIT, the return value is FALSE and    }
    { the message loop is broken.                                           }
    while (not GetTerminated) and GetMessage(MsgRec, 0, 0, 0) do begin   {V8.03 break loop early }
        TranslateMessage(MsgRec);
        DispatchMessage(MsgRec)
    end;
    SetTerminated(TRUE);
{$ELSE MSWINDOWS}
begin
  FMessagePump.HandleMessages;
  SetTerminated(TRUE);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Call this method to break the message loop, will set Terminated to TRUE   }
function TIcsWndControl.PostQuitMessage: Boolean;
begin
    Result := PostMessage(Handle, WM_QUIT, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This function is very similar to TApplication.ProcessMessage              }
{ You can also use it if your application has no TApplication object (Forms }
{ unit not referenced at all).                                              }

function TIcsWndControl.ProcessMessage : Boolean;
{$IFDEF MSWINDOWS}
var
    Msg : TMsg;
begin
    Result := FALSE;
    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then begin
        Result := TRUE;
        if Msg.Message = WM_QUIT then
            SetTerminated(TRUE)
        else begin
            TranslateMessage(Msg);
            DispatchMessage(Msg);
        end;
    end;
{$ELSE MSWINDOWS}
begin
    Result := FMessagePump.ProcessMessage;
    if FMessagePump.Terminated then
        SetTerminated(TRUE);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Loop thru message processing until all messages are processed.            }
{ This function is very similar to TApplication.ProcessMessage              }
{ This is intended for multithreaded application using TWSocket.            }
{ You can also use it if your application has no TApplication object (Forms }
{ unit not referenced at all).                                              }
procedure TIcsWndControl.ProcessMessages;
begin
    while Self.ProcessMessage do { loop };
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndControl.MessagePump;
begin
    if Assigned(FOnMessagePump) then
        FOnMessagePump(Self)
{$IFDEF MSWINDOWS}
  {$IFDEF NOFORMS}
    { The Forms unit (TApplication object) has not been included.           }
    { We used either an external message pump or our internal message pump. }
    { External message pump has to set Terminated property to TRUE when the }
    { application is terminated.                                            }
    else
        Self.ProcessMessages;
  {$ELSE}
    else if FMultiThreaded then
        Self.ProcessMessages
    else
        Application.ProcessMessages;
  {$ENDIF}
{$ENDIF}
{$IFDEF POSIX}
  else begin
      Self.ProcessMessages;
  end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndControl.Dispose(Disposing: Boolean);
begin
    //OutputDebugString(TOutputDebugStringType('Dispose ' + ClassName + ' ThreadID=' + IntToStr(GetCurrentThreadId)));
    if not Disposed then begin
        if Disposing then begin
            //OutputDebugString('Free managed resources');
        end;
        //OutputDebugString('Free unmanaged resources');
        DeallocateHWnd;  // Don't forget to deallocate the window handle
        Disposed := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsWndControl.Create(AOwner : TComponent);
begin
  {$IFDEF MSWINDOWS}
    InitializeGlobalHandler; { V1.18 }
  {$ENDIF}
  {$IFDEF POSIX}
    FWndHandler := TIcsWndHandler.Create;
  {$ENDIF}
    //OutputDebugString('TIcsWndControl.Create VCL version');
    inherited Create(AOwner);
    //OutputDebugString(TOutputDebugStringType('Create ' + ClassName + ' ThreadID=' + IntToStr(GetCurrentThreadId)));
    SetTerminated(FALSE);  { V8.03 should not be necessary, but lets play safe }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsWndControl.Destroy;
begin
    //OutputDebugString('Destroy');
    Dispose(TRUE);
{$IFDEF POSIX}
    FWndHandler.Free;
{$ENDIF}
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Getter for the handle property. It make sure a handle has been allocated
function TIcsWndControl.GetHandle: HWND;
begin
    if FHandle = 0 then
        AllocateHWnd;
    Result := FHandle;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsWndControl.GetTerminated: Boolean;
begin
{$IFDEF NOFORMS}
    Result := FTerminated;
{$ELSE}
    Result := FTerminated or Application.Terminated;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsFinalHandleException(Sender: TObject);                 { V1.14 }
begin
    { These calls are all intercepted by MadExcept.                         }
    { Pass the unhandled exception to the Application handler if assigned   }
    if (IcsFinalBgExceptionHandling = fehAppHandleException) and
       Assigned({$IFDEF RTL_NAMESPACES}System.{$ENDIF}Classes.ApplicationHandleException) then
        {$IFDEF RTL_NAMESPACES}System.{$ENDIF}Classes.ApplicationHandleException(Sender)
    else if (IcsFinalBgExceptionHandling = fehShowException) then
        { Shows it either in the console or with Windows.MessageBox API     }
        {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.ShowException(ExceptObject, ExceptAddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Message handler for all messages. This _must_ be overridden by derived
// component to handle his own messages and call inherited to handle all
// ancestor's messages.
procedure TIcsWndControl.WndProc(var MsgRec: TMessage);
{$IFDEF POSIX}
var
  Obj: TObject;
{$ENDIF}
begin
    try
        with MsgRec do begin
            if Msg = FMsgRelease then
                WMRelease(MsgRec)
          {$IFDEF MSWINDOWS}
            else
                Result := DefWindowProc(Handle, Msg, wParam, lParam);
          {$ELSE} //Posix
            else if (Msg = WM_TIMER) then begin
                Obj := TObject(WParam);
                if { (not IsBadReadPtr(Obj, GUIDOffSet + SizeOf(INT_PTR))) and }
                   (PUINT_PTR(WParam + Ics.Posix.Messages.WPARAM(GUIDOffSet))^ = WParam) and
                   (Obj is TIcsTimer) then
                    TIcsTimer(Obj).WmTimer(MsgRec);
            end
            else if (Msg = WM_ICS_THREAD_TIMER) then begin
                Obj := TObject(WParam);
                if {(not IsBadReadPtr(Obj, GUIDOffSet + SizeOf(INT_PTR))) and }
                   (PINT_PTR(WParam + Ics.Posix.Messages.WPARAM(GUIDOffSet))^ = LParam) and
                   (Obj is TIcsThreadTimer) then
                   { Actually the overridden method       }
                   { TIcsThreadTimer.WMTimer is called!   }
                   { This removes an Error Insight error. }
                    TIcsTimer(Obj).WmTimer(MsgRec);
            end;
          {$ENDIF}
        end;
    except
        // All exceptions must be handled otherwise the application
        // will terminate as soon as an exception is raised.
        on E: Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ All exceptions *MUST* be handled. If an exception is not handled, the     }
{ application will be shut down !                                           }
procedure TIcsWndControl.HandleBackGroundException(E: Exception);
var
    CanAbort : Boolean;
    Handled  : Boolean; { V1.14 }
begin
    if E is EAbort then { V1.14 }
        Exit;
    CanAbort := TRUE;
    { First call the error event handler, if any }
    Handled := Assigned(FOnBgException); { V1.14 }
    if Handled then begin                { V1.14 }
        try
            TriggerBgException(E, CanAbort);
        except
            // Ignore any exception here
        end;
    end;
    { Then abort the component }
    if CanAbort then begin
        try
            try                          { V1.14 }
                if Assigned(FExceptAbortProc) then
                    FExceptAbortProc;
            finally
                AbortComponent;
            end;
        except
            // Ignore any exception here
        end;
    end;
    if not Handled then
        IcsFinalHandleException(Self);   { V1.14 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Trigger the background exception event handler
procedure TIcsWndControl.TriggerBgException(
    E            : Exception;
    var CanClose : Boolean);
begin
    if Assigned(FOnBgException) then
        FOnBgException(Self, E, CanClose);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Release method call the destructor when all active message handlers have
// finished their work.
procedure TIcsWndControl.Release;
begin
    PostMessage(Handle, FMsgRelease, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndControl.SetMultiThreaded(const Value: Boolean);
begin
    FMultiThreaded := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndControl.SetOnBgException(const Value: TIcsBgExceptionEvent); { V1.14 }
begin
    FOnBgException := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndControl.SetOnMessagePump(const Value: TNotifyEvent);
begin
    FOnMessagePump := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndControl.SetTerminated(const Value: Boolean);
begin
    FTerminated := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Message handler for the Release method
procedure TIcsWndControl.WMRelease(var msg: TMessage);
begin
    Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndControl.AbortComponent;
begin
    // To be overridden in derived classes
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
constructor TIcsWndHandler.Create;
begin
    FLastMessage := WM_USER;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsWndHandler.Destroy;
begin
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndHandler.UnregisterMessage(var Msg: UINT);
begin
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsWndHandler.AllocateMsgHandler(Obj: TIcsWndControl): UINT;
begin
  Inc(FLastMessage);
  Result := FLastMessage;
end;
{$ENDIF POSIX}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
procedure TIcsWndHandler.AllocateHWnd;
var
    TempClass                : TWndClass;
    IcsWndControlWindowClass : TWndClass;
    ClassRegistered          : Boolean;
begin
    // Nothing to do if hidden window is already created
    if FHandle <> 0 then
        Exit;

    // We use a critical section to be sure only one thread can check if a
    // class is registered and register it if needed.
    // We must also be sure that the class is not unregistered by another
    // thread which just destroyed a previous window.
    EnterCriticalSection(GWndHandlerCritSect);
    try
        // Check if the window class is already registered
        IcsWndControlWindowClass.hInstance     := HInstance;
        IcsWndControlWindowClass.lpszClassName := IcsWndControlWindowClassName;
        ClassRegistered := GetClassInfo(HInstance,
                                        IcsWndControlWindowClass.lpszClassName,
                                        TempClass);
        if not ClassRegistered then begin
            // Not registered yet, do it right now !
            IcsWndControlWindowClass.style         := 0;
            IcsWndControlWindowClass.lpfnWndProc   := @WndControlWindowsProc;
            IcsWndControlWindowClass.cbClsExtra    := 0;
            IcsWndControlWindowClass.cbWndExtra    := SizeOf(Pointer);
            IcsWndControlWindowClass.hIcon         := 0;
            IcsWndControlWindowClass.hCursor       := 0;
            IcsWndControlWindowClass.hbrBackground := 0;
            IcsWndControlWindowClass.lpszMenuName  := nil;

           if {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.RegisterClass(IcsWndControlWindowClass) = 0 then
                raise EIcsException.Create(
                     'Unable to register TIcsWndControl hidden window class.' +
                     ' Error #' + IntToStr(GetLastError) + '.');
        end;

        // Now we are sure the class is registered, we can create a window using it
        FHandle := CreateWindowEx(WS_EX_TOOLWINDOW,
                                  IcsWndControlWindowClass.lpszClassName,
                                  '',        // Window name
                                  WS_POPUP,  // Window Style
                                  0, 0,      // X, Y
                                  0, 0,      // Width, Height
                                  0,         // hWndParent
                                  0,         // hMenu
                                  HInstance, // hInstance
                                  nil);      // CreateParam

        if FHandle = 0 then
            raise EIcsException.Create(
                'Unable to create TIcsWndControl hidden window. ' +
                ' Error #' + IntToStr(GetLastError) + '.');

        // We have a window. In the associated data, we record a reference
        // to our object. This will later allow to call the WndProc method to
        // handle messages sent to the window.
    {$IFDEF WIN64}
        SetWindowLongPtr(FHandle, 0, INT_PTR(Self));
    {$ELSE}
        SetWindowLong(FHandle, 0, Longint(Self));
    {$ENDIF}
        Inc(GWndHandleCount);
    finally
        LeaveCriticalSection(GWndHandlerCritSect);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndHandler.DeallocateHWnd;
begin
    // Pas de handle, rien à faire !
    if FHandle = 0 then
        Exit;

    // Clear message map
    FillChar(FMsgMap[0], G_WH_MAX_MSG * SizeOf(Pointer), 0);
{$IFDEF WIN64}
    SetWindowLongPtr(FHandle, 0, 0); // Supprime la référence vers l'objet
{$ELSE}
    SetWindowLong(FHandle, 0, 0); // Supprime la référence vers l'objet
{$ENDIF}
    DestroyWindow(FHandle);       // Détruit la fenêtre cachée
    FHandle := 0;                 // On n'a plus de handle !

    EnterCriticalSection(GWndHandlerCritSect);
    try
        Dec(GWndHandleCount);
        if GWndHandleCount <= 0 then
            { Unregister the window class use by the component.              }
            { This is necessary to do so from a DLL when the DLL is unloaded }
            { (that is when DllEntryPoint is called with dwReason equal to   }
            { DLL_PROCESS_DETACH.                                            }
            {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.UnregisterClass(IcsWndControlWindowClassName, HInstance);
    finally
        LeaveCriticalSection(GWndHandlerCritSect);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndHandler.RegisterMessage(Msg: UINT; Obj: TIcsWndControl);
begin
    if FMsgLow < WM_USER then
        raise EIcsException.Create('MsgLow not defined');
    if Msg >= (FMsgLow + G_WH_MAX_MSG) then
        raise EIcsException.Create('Msg value out of bound');
    if Assigned(FMsgMap[Msg - WM_USER]) then
        raise EIcsException.Create('Msg already registered');
    FMsgMap[Msg - FMsgLow] := Obj;
    Inc(FMsgCnt);
    if FHandle = 0 then
        AllocateHWnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndHandler.UnregisterMessage(var Msg: UINT);
var
    LMsg : TMsg;
begin
    if Msg = 0 then
        Exit;
    if FMsgLow < WM_USER then
        raise EIcsException.Create('MsgLow not defined');
    if Msg >= (FMsgLow + G_WH_MAX_MSG) then
        raise EIcsException.Create('Msg value out of bound');
    if not Assigned(FMsgMap[Msg - FMsgLow]) then
        raise EIcsException.Create('Msg not registered');
    FMsgMap[Msg - FMsgLow] := nil;
    Dec(FMsgCnt);
    { Remove all messages with the unregistered message ID from the message }
    { queue (those with destination HWND = FHandle).                        }
    if FHandle > 0 then
        while PeekMessage(LMsg, FHandle, Msg, Msg, PM_REMOVE) do {loop} ;
    Msg := 0;
    if FMsgCnt = 0 then
        DeallocateHWnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndHandler.TriggerBgException(E: Exception; var CanClose : Boolean);
begin
    if Assigned(FOnBgException) then
        FOnBgException(Self, E, CanClose);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsWndHandler.TriggerMessage(var MsgRec : TMessage) : Boolean;
begin
    Result := FALSE;  { Fix V1.10 }
    if Assigned(FOnMessage) then
        FOnMessage(Self, MsgRec, Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndHandler.WndProc(var MsgRec: TMessage);
var
    Dummy : Boolean;
    Obj   : TObject;
begin
    try
        with MsgRec do begin
            if (Msg >= FMsgLow) and
               (Msg < (FMsgLow + G_WH_MAX_MSG)) and
               Assigned(FMsgMap[Msg - FMsgLow]) then
                FMsgMap[Msg - FMsgLow].WndProc(MsgRec)

            else if not TriggerMessage(MsgRec) then begin
                { Not handled by event OnMessage }
                if (Msg = WM_TIMER) then begin
                    Obj := TObject(WParam);
                    if (not IsBadReadPtr(Obj, GUIDOffSet + SizeOf(INT_PTR))) and
                    {$IFDEF COMPILER16_UP} { WPARAM changed to unsigned }
                       (PUINT_PTR(WParam + {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.WPARAM(GUIDOffSet))^ = WParam) and
                    {$ELSE}
                       (PINT_PTR(WParam + GUIDOffSet)^ = WParam) and
                    {$ENDIF}
                       (Obj is TIcsTimer) then
                        TIcsTimer(Obj).WmTimer(MsgRec);
                end
                else if (Msg = WM_ICS_THREAD_TIMER) then begin
                    Obj := TObject(WParam);
                    if (not IsBadReadPtr(Obj, GUIDOffSet + SizeOf(INT_PTR))) and
                       (PINT_PTR(WParam + {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.WPARAM(GUIDOffSet))^ = LParam) and
                       (Obj is TIcsThreadTimer) then
                       { Actually the overridden method       }
                       { TIcsThreadTimer.WMTimer is called!   }
                       { This removes an Error Insight error. }
                        TIcsTimer(Obj).WmTimer(MsgRec);
                end
                else
                    Result := DefWindowProc(Handle, Msg, wParam, lParam);
            end;
        end;
    except
        // All exceptions must be handled otherwise the application
        // will terminate as soon as an exception is raised.
        on E:Exception do begin                 { V1.14 }
            if Assigned(FOnBgException) then
                TriggerBgException(E, Dummy)
            else { Unhandled }
                IcsFinalHandleException(Self);  { V1.14 }
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsWndHandler.AllocateMsgHandler(Obj: TIcsWndControl): UINT;
var
    I : UINT;
begin
    if FMsgLow < WM_USER then
        raise EIcsException.Create('MsgLow not defined');
    if FMsgCnt >= G_WH_MAX_MSG then
        raise EIcsException.Create('No more free message');
    I := 0;
    while I < G_WH_MAX_MSG do begin
        if not Assigned(FMsgMap[I]) then begin
            Result     := I + FMsgLow;
//if IsConsole then writeLn('AllocateMsgHandler = ', Result);
            FMsgMap[I] := Obj;
            Inc(FMsgCnt);
            if FHandle = INVALID_HANDLE_VALUE then
                AllocateHWnd;
            Exit;
        end;
        Inc(I);
    end;
    raise EIcsException.Create('No more free message');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsWndHandler.Create;
begin
    inherited Create;
    SetLength(FMsgMap, G_WH_MAX_MSG); { No initialization required }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsWndHandler.GetMsgLeft: UINT;
begin
    Result := G_WH_MAX_MSG - FMsgCnt;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsWndHandlerPool.Create;
begin
    inherited Create;
    FList := TIcsWndHandlerList.Create;
    InitializeCriticalSection(FCritSect);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsWndHandlerPool.Destroy;
begin
    if Assigned(FList) then begin
        // Should empty the list
        FList.Free;
    end;
    DeleteCriticalSection(FCritSect);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndHandlerPool.SetWndHandlerMaxMsgIDs(const Value: TWhMaxMsgIDs);
begin
    Lock;
    try
        if FList.Count > 0 then
            raise EIcsException.Create(
              'Maximum number of message IDs per handler can only be set ' +
              'when the pool is empty');
        G_WH_MAX_MSG := Value;
    finally
        UnLock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsWndHandlerPool.GetWndHandler(
    HandlerCount : UINT;
    ThreadID     : TThreadID) : TIcsWndHandler;
var
    I : Integer;
    L : TIcsWndHandlerList;
begin
    { If you see an assertion error here you have to increase the maximum  }
    { number of message IDs per WndHandler.                                }
    { Call GWndHandlerPool.SetWndHandlerMaxMsgIDs() to set this value.     }
    Assert(HandlerCount <= G_WH_MAX_MSG, 'ICS message IDs out of bounds!');

    // Search the list which has same thread ID
    I := FList.Count - 1;
    while (I >= 0) and
          (TIcsWndHandlerList(FList.Items[I]).ThreadID <> ThreadID) do
        Dec(I);
    if I >= 0 then
        L := TIcsWndHandlerList(FList.Items[I])
    else begin
        // No list found. Create a new one
        L          := TIcsWndHandlerList.Create;
        L.ThreadID := ThreadID;
        FList.Add(L);
    end;

    // Search the list for a WndHandler with enough MsgHandlers available
    I := 0;
    while I < L.Count do begin
        Result := TIcsWndHandler(L.Items[I]);
        if Result.GetMsgLeft >= HandlerCount then
            Exit;
        Inc(I);
    end;
    Result            := TIcsWndHandler.Create;
    Result.FOwnerList := L;
    //OutputDebugString('TIcsWndHandler.Create');
    //Result.MsgLow   := WM_USER + 1;
    if (GWndHandlerMsgLow <= WM_USER) or
       (GWndHandlerMsgLow >= (65535 - G_WH_MAX_MSG)) then
        GWndHandlerMsgLow := WM_USER + 1;
    Result.MsgLow := GWndHandlerMsgLow;
    L.Add(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndHandlerPool.FreeWndHandler(var WndHandler: TIcsWndHandler);
var
    Index : Integer;
begin
    if not Assigned(WndHandler.FOwnerList) then
        Exit;
    Index := WndHandler.FOwnerList.IndexOf(WndHandler);
    if Index >= 0 then begin
        WndHandler.FOwnerList.Delete(Index);
        if WndHandler.FOwnerList.Count <= 0 then begin
            Index := FList.IndexOf(WndHandler.FOwnerList);
            if Index >= 0 then
                FList.Delete(Index);
            //OutputDebugString('TIcsWndHandlerPool Free OwnerList');
            WndHandler.FOwnerList.Free;   // V1.01
        end;
        WndHandler.Free;
        WndHandler := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndHandlerPool.Lock;
begin
    EnterCriticalSection(FCritSect);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsWndHandlerPool.UnLock;
begin
    LeaveCriticalSection(FCritSect);
end;
{$ENDIF MSWINDOWS}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimer.CheckWindowAvailable;
begin
    if FIcsWndControl.ThreadID = 0 then
        raise EIcsTimerException.Create('No window available');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsTimer.Create(AOwner: TIcsWndControl);
begin
    if not Assigned(AOwner) then
        raise EIcsTimerException.Create('Owner not assigned');
    inherited Create(AOwner);
    FIcsWndControl := AOwner;
    FEnabled := TRUE;
    FInterval := 1000;
    FUID := INT_PTR(Self);
    if GUIDOffSet = 0 then
        GUIDOffSet := INT_PTR(@FUID) - INT_PTR(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsTimer.Destroy;
begin
    FUID := 0;
    if Assigned(FIcsWndControl) and (FIcsWndControl.ThreadID <> 0) then begin
        FEnabled := FALSE;
        UpdateTimer;
    end;
    //FIcsWndControl := nil; // Most likely required, but not 100% for sure. {not needed}
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimer.SetEnabled(const Value: Boolean);
begin
    if Value <> FEnabled then begin
        CheckWindowAvailable;
        FEnabled := Value;
        UpdateTimer;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimer.SetInterval(const Value: Cardinal);
begin
    if Value <> FInterval then begin
        CheckWindowAvailable;
        FInterval := Value;
        UpdateTimer;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimer.SetOnTimer(Value: TNotifyEvent);
begin
    CheckWindowAvailable;
    FOnTimer := Value;
    UpdateTimer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimer.WMTimer(var msg: TMessage);
{var
    CurTicks : Cardinal;}
begin
    //CurTicks := GetTickCount;
    if Assigned(FOnTimer){ and
       (IcsCalcTickDiff(FLastEvent, CurTicks) >= FInterval)} then
        FOnTimer(Self);
    //FLastEvent := CurTicks;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsTimer.UpdateTimer;
begin
    KillTimer(FIcsWndControl.Handle, UINT_PTR(Self));
    if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
        if SetTimer(FIcsWndControl.Handle,
                    UINT_PTR(Self), FInterval, nil) = 0 then begin   { V8.04 }
            FEnabled := FALSE;
            raise EIcsTimerException.Create('No more timers');
        end
        else
            FLastEvent := IcsGetTickCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
initialization
    InitializeGlobalHandler; { V1.18 }

finalization
    FinalizeGlobalHandler;  { V1.09 }
    GUnitFinalized := True; { V1.09 }
{$ENDIF}
end.

