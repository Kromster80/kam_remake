{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  TIcsThreadTimer implements a custom timer class. In doesn't
              use windows API timers but sends custom timer messages to an
              already existing ICS-window from one or more threads.
              It uses resources (handles) very sparingly so 10K timers
              are virtually possible, see OverbyteIcsThreadTimerDemo.
Creation:     Jul 24, 2009
Version:      8.00
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009 by François PIETTE
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
                 to Francois PIETTE. Use a nice stamp and mention your name,
                 street address, EMail address and any comment you like to say.

History:
07 Sept, 2010 V1.01 Added boolean property TIcsThreadTimer.KeepThreadAlive.
                    If it's set prevents the underlying, shared thread object from
                    being freed when its reference count becomes 0. This fixes
                    a serious performance leak when just a single TIcsThreadTimer
                    instance was used which was enabled/unabled or its interval
                    or event handler were changed frequently. Best performance
                    is achieved by setting this property to TRUE.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsThreadTimer;
{$ENDIF}

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }

{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}

{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
{$ELSE}
  Posix.Pthread,
  Posix.SysTypes,
  Posix.UniStd,
  Ics.Posix.WinTypes,
  Ics.Posix.Messages,
{$ENDIF}
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SyncObjs{$ELSE}SyncObjs{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SysConst{$ELSE}SysConst{$ENDIF},
{$IFDEF COMPILER17_UP}
  System.Types,
{$ENDIF}
{$IFDEF FMX}
  Ics.Fmx.OverbyteIcsWndControl,
{$ELSE}
  OverbyteIcsWndControl,
{$ENDIF}
  OverbyteIcsUtils;

type
  TIcsClock = class;

  TIcsClockThread = class(TThread)
  private
    FClock       : TIcsClock;
    FWakeUpEvent : TEvent;
    FInterval    : LongWord;
  protected
    procedure Execute; override;
  public
    procedure SetInterval(const Value: LongWord);
    constructor Create(AClock: TIcsClock);
    destructor Destroy; override;
  end;

  TIcsThreadTimer = class(TIcsTimer)
  private
    FMsgQueued   : Boolean;
    FQueuedTicks : Cardinal;
    FClock       : TIcsClock;
    FCurHandle   : HWND;
    function GetKeepThreadAlive: Boolean;
    procedure SetKeepThreadAlive(const Value: Boolean);
  protected
    procedure UpdateTimer; override;
    procedure WMTimer(var msg: TMessage); override;
  public
    property KeepThreadAlive: Boolean           read  GetKeepThreadAlive
                                                write SetKeepThreadAlive;
    constructor Create(AOwner: TIcsWndControl);
    destructor Destroy; override;
  end;

  TIcsClockPool = class;

  TIcsClock = class(TObject)
  private
    FThread          : TIcsClockThread;
    FClockPool       : TIcsClockPool;
    FTimerList       : TThreadList;
    FCritSecClock    : TIcsCriticalSection;
    FKeepThreadAlive : Boolean;
    procedure Lock; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure Unlock; {$IFDEF USE_INLINE} inline; {$ENDIF}
  public
    constructor Create(AOwner: TIcsClockPool);
    destructor  Destroy; override;
    procedure   MessageProcessed(TimerObj: TIcsThreadTimer);
    procedure   SetTimer(TimerObj: TIcsThreadTimer);
    procedure   KillTimer(TimerObj: TIcsThreadTimer);
  end;

  TIcsClockRec = record
    Clock    : TIcsClock;
    RefCount : Integer;
  end;
  PIcsClockRec = ^TIcsClockRec;

  TIcsClockPool = class(TObject)
  private
    FClockList         : TList;
    FMaxTimerPerClock  : Integer;
    FMinTimerRes       : LongWord;
    function InternalAcquire: TIcsClock;
    procedure InternalRelease(AClock: TIcsClock);
  public
    class function Acquire: TIcsClock;
    class procedure Release(AClock: TIcsClock);
    constructor Create;
    destructor  Destroy; override;
  end;

var
{$IFDEF MSWINDOWS}  // otherwise a const in Ics.Posix.Messages
  WM_ICS_THREAD_TIMER     : Cardinal;
{$ENDIF}

  { It's possible to fine tune timer behaviour by two global vars as long as }
  { no instance of  TIcsThreadTimer is allocated.                            }
  { GMaxIcsTimerPerThread  // Maximum timers per TIcsClock instance          }
  { GMinIcsTimerResolution // Ticks / Msec interval of TIcsClock             }
  GMaxIcsTimerPerThread   : Integer  = 1000;
{$IFDEF MSWINDOWS}
  GMinIcsTimerResolution  : LongWord = 100;
{$ENDIF}
{$IFDEF POSIX}
  GMinIcsTimerResolution  : LongWord = 250;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS} // otherwise defined in Ics.Posix.WinTypes
const
  ERROR_INVALID_WINDOW_HANDLE  = DWORD(1400);
{$ENDIF}

var
  GIcsClockPool       : TIcsClockPool = nil;
  GCritSecClockPool   : TIcsCriticalSection = nil;
  GTimerID            : Integer = 0;

{$I Include\Ics.InterlockedApi.inc}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TIcsClockThread }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsClockThread.Create(AClock: TIcsClock);
begin
    inherited Create(FALSE);
    FClock := AClock;
    FInterval := FClock.FClockPool.FMinTimerRes;
    FWakeUpEvent := TEvent.Create(nil, False, False, '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsClockThread.Destroy;
begin
    Terminate;
    if Assigned(FWakeUpEvent) then
      FWakeUpEvent.SetEvent;
    inherited Destroy;
    FreeAndNil(FWakeUpEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsClockThread.Execute;
var
    L         : TList;
    I         : Integer;
    CurTicks  : LongWord;
    wRes      : TWaitResult;
    CurTimer  : TIcsThreadTimer;
begin
{$IFDEF Debug}
    IcsNameThreadForDebugging('TIcsClockThread');
{$ENDIF}
    try //(wrSignaled, wrTimeout, wrAbandoned, wrError, wrIOCompletion)
        while not Terminated do begin
            wRes := FWakeUpEvent.WaitFor(FInterval);
            case wRes of

                wrTimeout:
                begin
                    CurTicks := IcsGetTickCount;
                    L := FClock.FTimerList.LockList;
                    try
                        for I := 0 to L.Count - 1 do begin
                            if Terminated then Exit;
                            CurTimer := TIcsThreadTimer(L[I]);
                            if CurTimer.FCurHandle = INVALID_HANDLE_VALUE then
                                Continue
                            else if CurTimer.FMsgQueued then
                                CurTimer.FQueuedTicks := CurTicks
                            else if (IcsCalcTickDiff(CurTimer.FQueuedTicks,
                                     CurTicks) >= CurTimer.Interval) then begin
                                if PostMessage(CurTimer.FCurHandle,
                                               WM_ICS_THREAD_TIMER,
                                               WPARAM(CurTimer),
                                               LPARAM(CurTimer.FUID)) then begin
                                    CurTimer.FQueuedTicks := CurTicks;
                                    CurTimer.FMsgQueued   := TRUE;
                                end
                                else begin
                                    case GetLastError of
                                        ERROR_INVALID_WINDOW_HANDLE :
                                            CurTimer.FCurHandle := INVALID_HANDLE_VALUE;
                                        ERROR_NOT_ENOUGH_QUOTA :
                                            Break;
                                    end;
                                end;
                            end;
                        end;
                    finally
                        FClock.FTimerList.UnlockList;
                    end;
                    //raise EIcsTimerException.Create('Test');
                end;

                wrSignaled:
                    if Terminated then
                        Break;
                wrAbandoned:
                        Break;

                else
                    raise EIcsTimerException.Create(
                                      SysErrorMessage(GetLastError));
            end; //case;
        {$IFDEF MSWINDOWS}
            Sleep(0);
        {$ENDIF}
        {$IFDEF POSIX}
            Sleep(1);
        {$ENDIF}
        end;
    finally
       if not Terminated then
          Terminate;
       FClock.FTimerList.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsClockThread.SetInterval(const Value: LongWord);
begin
    if Value < FClock.FClockPool.FMinTimerRes then
        FInterval := FClock.FClockPool.FMinTimerRes
    else
        FInterval := Value;
    FWakeUpEvent.SetEvent;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TIcsClock }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsClock.Create(AOwner: TIcsClockPool);
begin
    inherited Create;
    FClockPool := AOwner;
    FTimerList := TThreadList.Create;
    FCritSecClock := TIcsCriticalSection.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsClock.Destroy;
begin
    FreeAndNil(FThread);
    FreeAndNil(FTimerList);
    FCritSecClock.Free;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsClock.Lock;
begin
    FCritSecClock.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsClock.Unlock;
begin
    FCritSecClock.Leave;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsClock.KillTimer(TimerObj: TIcsThreadTimer);
var
    L    : TList;
    Flag : Boolean;
begin
    Lock;
    try
        L := FTimerList.LockList;
        try
            L.Remove(TimerObj);
            { Don't free the thread here - prevent  deadlock.  }
            { Instead free it in outer critical section below. }
            Flag := L.Count = 0;
        finally
            FTimerList.UnlockList;
        end;
       { if Flag then
            FreeAndNil(FThread); }
        if Flag then
        begin
            if FKeepThreadAlive and (FThread <> nil) then
                FThread.SetInterval(INFINITE)
            else
                FreeAndNil(FThread);
        end;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsClock.SetTimer(TimerObj: TIcsThreadTimer);
var
    L : TList;
begin
    Lock;
    try
        if Assigned(FThread) and FThread.Terminated then
        { Thread terminated due to an exception in Execute, should never happen }
            FreeAndNil(FThread);

        L := FTimerList.LockList;
        try
            if L.IndexOf(TimerObj) >= 0 then
                raise EIcsTimerException.Create('Timer already exists');
            if not Assigned(FThread) then
                FThread := TIcsClockThread.Create(Self);
            TimerObj.FQueuedTicks := IcsGetTickCount;
            TimerObj.FMsgQueued   := FALSE;
            TimerObj.FCurHandle   := TimerObj.FIcsWndControl.Handle;
            L.Add(TimerObj);
            if FThread.FInterval = INFINITE then
                FThread.SetInterval(FClockPool.FMinTimerRes);
        finally
            FTimerList.UnlockList;
        end;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsClock.MessageProcessed(TimerObj: TIcsThreadTimer);
begin
    FTimerList.LockList;
    try
        TimerObj.FMsgQueued    := FALSE;
        TimerObj.FQueuedTicks  := IcsGetTickCount;
    finally
        FTimerList.UnlockList;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TIcsClockPool }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CritSecClockPool_Lock; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    GCritSecClockPool.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CritSecClockPool_Unlock; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    GCritSecClockPool.Leave;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class function TIcsClockPool.Acquire: TIcsClock;
begin
    CritSecClockPool_Lock;
    try
        if not Assigned(GIcsClockPool) then
            GIcsClockPool := Create;
        Result := GIcsClockPool.InternalAcquire;
    finally
        CritSecClockPool_Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class procedure TIcsClockPool.Release(AClock: TIcsClock);
begin
    CritSecClockPool_Lock;
    try
        if Assigned(GIcsClockPool) then begin
            GIcsClockPool.InternalRelease(AClock);
            if GIcsClockPool.FClockList.Count = 0 then
                FreeAndNil(GIcsClockPool);
        end;
    finally
        CritSecClockPool_Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsClockPool.Create;
begin
    inherited Create;
    FClockList        := TList.Create;
    FMaxTimerPerClock := GMaxIcsTimerPerThread;
    FMinTimerRes      := GMinIcsTimerResolution;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsClockPool.Destroy;
var
    I : Integer;
begin
    CritSecClockPool_Lock;
    try
        for I := 0 to FClockList.Count -1 do begin
            FreeAndNil(PIcsClockRec(FClockList[I])^.Clock);
            Dispose(PIcsClockRec(FClockList[I]));
        end;
        FreeAndNil(FClockList);
    finally
        CritSecClockPool_Unlock;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsClockPool.InternalAcquire: TIcsClock;
var
    I : Integer;
    P : PIcsClockRec;
begin
    for I := 0 to FClockList.Count -1 do begin
        if PIcsClockRec(FClockList[I])^.RefCount < FMaxTimerPerClock then begin
            Result := PIcsClockRec(FClockList[I])^.Clock;
            Inc(PIcsClockRec(FClockList[I])^.RefCount);
            Exit;
        end;
    end;
    New(P);
    P^.Clock := TIcsClock.Create(Self);
    P^.RefCount := 1;
    FClockList.Add(P);
    Result := P^.Clock;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsClockPool.InternalRelease(AClock: TIcsClock);
var
    I : Integer;
    P : PIcsClockRec;
begin
    for I := 0 to FClockList.Count -1 do begin
        P := PIcsClockRec(FClockList[I]);
        if P^.Clock = AClock then begin
            if P^.Clock.FKeepThreadAlive then begin
                if (P^.RefCount > 1) then
                    Dec(P^.RefCount);
            end
            else
                Dec(P^.RefCount);
            if P^.RefCount = 0 then begin
                FreeAndNil(P^.Clock);
                Dispose(P);
                FClockList.Delete(I);
            end;
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TIcsThreadTimer }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsThreadTimer.Create(AOwner: TIcsWndControl);
begin
    inherited Create(AOwner);
    FUID   := InterlockedIncrement(GTimerID);
    FClock := TIcsClockPool.Acquire;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsThreadTimer.Destroy;
begin
    { Ensure the timer is disabled before the UID will be reset in inherited!! }
    FEnabled := FALSE;
    UpdateTimer;

    inherited Destroy;

    TIcsClockPool.Release(FClock);
    FClock := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsThreadTimer.GetKeepThreadAlive: Boolean;
begin
    Result := FClock.FKeepThreadAlive;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsThreadTimer.SetKeepThreadAlive(const Value: Boolean);
begin
    FClock.FKeepThreadAlive := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsThreadTimer.WMTimer(var msg: TMessage);
begin
    try
        if FEnabled and Assigned(FOnTimer) then
            FOnTimer(Self);
    finally
        FClock.MessageProcessed(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsThreadTimer.UpdateTimer;
begin
    FClock.KillTimer(Self);
    if FEnabled and (FInterval > 0) and Assigned(FOnTimer) then
    try
        FClock.SetTimer(Self);
    except
        FEnabled := FALSE;
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
    GCritSecClockPool := TIcsCriticalSection.Create;
  {$IFDEF MSWINDOWS} // Otherwise a const in Ics.Posix.Messages
    WM_ICS_THREAD_TIMER := RegisterWindowMessage('OVERBYTE_ICS_THREAD_TIMER');
    if WM_ICS_THREAD_TIMER = 0 then
        raise EIcsTimerException.Create(SysErrorMessage(GetLastError));
  {$ENDIF}
  
finalization
    FreeAndNil(GIcsClockPool);
    FreeAndNil(GCritSecClockPool);
end.
