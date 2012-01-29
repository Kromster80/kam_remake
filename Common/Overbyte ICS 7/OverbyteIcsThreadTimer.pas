{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  TIcsThreadTimer implements a custom timer class. In doesn't
              use windows API timers but sends custom timer messages to an
              already existing ICS-window from one or more threads.
              It uses resources (handles) very sparingly so 10K timers
              are virtually possible, see OverbyteIcsThreadTimerDemo.
Creation:     Jul 24, 2009
Version:      1.01
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
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

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsThreadTimer;

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }

{$I OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}

{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes,
  OverbyteIcsWndControl;

type
  TIcsClock = class;

  TIcsClockThread = class(TThread)
  private
    FClock       : TIcsClock;
    FWakeUpEvent : THandle;
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
    FThread       	 : TIcsClockThread;
    FClockPool    	 : TIcsClockPool;
    FTimerList    	 : TThreadList;
    FCritSecClock 	 : TRtlCriticalSection;
    FKeepThreadAlive : Boolean;
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
    FMinTimerRes       : Longword;
    function InternalAcquire: TIcsClock;
    procedure InternalRelease(AClock: TIcsClock);
  public
    class function Acquire: TIcsClock;
    class procedure Release(AClock: TIcsClock);
    constructor Create;
    destructor  Destroy; override;
  end;

var
  WM_ICS_THREAD_TIMER     : Cardinal;

  { It's possible to fine tune timer behaviour by two global vars as long as }
  { no instance of  TIcsThreadTimer is allocated.                            }
  { GMaxIcsTimerPerThread  // Maximum timers per TIcsClock instance          }
  { GMinIcsTimerResolution // Ticks / Msec interval of TIcsClock             }
  GMaxIcsTimerPerThread   : Integer  = 1000;
  GMinIcsTimerResolution  : Longword = 100;

implementation

uses
  OverbyteIcsUtils;

const
  ERROR_INVALID_WINDOW_HANDLE  = DWORD(1400);

var
  GIcsClockPool       : TIcsClockPool;
  GCritSecClockPool   : TRtlCriticalSection;
  GTimerID            : Integer;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TIcsClockThread }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsClockThread.Create(AClock: TIcsClock);
begin
    inherited Create(FALSE);
    FClock := AClock;
    FInterval := FClock.FClockPool.FMinTimerRes;
    FWakeUpEvent := CreateEvent(nil, False, False, nil);
    if FWakeUpEvent = 0 then
        raise EIcsTimerException.Create(SysErrorMessage(GetLastError));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsClockThread.Destroy;
begin
    Terminate;
    if FWakeUpEvent <> 0 then
        SetEvent(FWakeUpEvent);

    inherited Destroy;

    if FWakeUpEvent <> 0 then begin
        CloseHandle(FWakeUpEvent);
        FWakeUpEvent := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsClockThread.Execute;
var
    L        : TList;
    I        : Integer;
    CurTick  : Longword;
    wRes     : Longword;
    CurTimer : TIcsThreadTimer;
begin
{$IFDEF Debug}
    IcsNameThreadForDebugging('TIcsClockThread');
{$ENDIF}
    try
        while not Terminated do begin
            wRes := WaitForSingleObject(FWakeUpEvent, FInterval);
            case wRes of

            WAIT_TIMEOUT :
            begin
                CurTick := GetTickCount;
                L := FClock.FTimerList.LockList;
                try
                    for I := 0 to L.Count - 1 do begin
                        if Terminated then Exit;
                        CurTimer := TIcsThreadTimer(L[I]);
                        if CurTimer.FCurHandle = INVALID_HANDLE_VALUE then
                            Continue
                        else if CurTimer.FMsgQueued then
                            CurTimer.FQueuedTicks := CurTick
                        else if (IcsCalcTickDiff(CurTimer.FQueuedTicks,
                                      CurTick) >= CurTimer.Interval) then begin
                            if PostMessage(CurTimer.FCurHandle,
                                           WM_ICS_THREAD_TIMER,
                                           WPARAM(CurTimer),
                                           LPARAM(CurTimer.FUID)) then begin
                                CurTimer.FQueuedTicks := CurTick;
                                CurTimer.FMsgQueued   := TRUE;
                            end
                            else if GetLastError = ERROR_INVALID_WINDOW_HANDLE then
                                CurTimer.FCurHandle := INVALID_HANDLE_VALUE;
                        end;
                    end;
                finally
                    FClock.FTimerList.UnlockList;
                end;
                //raise EIcsTimerException.Create('Test');
            end;

            WAIT_FAILED   : raise EIcsTimerException.Create(
                                  SysErrorMessage(GetLastError));
            WAIT_OBJECT_0 : if Terminated then
                               Break;
                                      
            end;
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
    SetEvent(FWakeUpEvent);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TIcsClock }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsClock.Create(AOwner: TIcsClockPool);
begin
    inherited Create;
    FClockPool := AOwner;
    FTimerList := TThreadList.Create;
    InitializeCriticalSection(FCritSecClock);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsClock.Destroy;
begin
    FreeAndNil(FThread);
    FreeAndNil(FTimerList);
    DeleteCriticalSection(FCritSecClock);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsClock.KillTimer(TimerObj: TIcsThreadTimer);
var
    L    : TList;
    Flag : Boolean;
begin
    EnterCriticalSection(FCritSecClock);
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
        LeaveCriticalSection(FCritSecClock);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsClock.SetTimer(TimerObj: TIcsThreadTimer);
var
    L : TList;
begin
    EnterCriticalSection(FCritSecClock);
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
            TimerObj.FQueuedTicks := GetTickCount;
            TimerObj.FMsgQueued   := FALSE;
            TimerObj.FCurHandle   := TimerObj.FIcsWndControl.Handle;
            L.Add(TimerObj);
            if FThread.FInterval = INFINITE then
                FThread.SetInterval(FClockPool.FMinTimerRes);
        finally
            FTimerList.UnlockList;
        end;
    finally
        LeaveCriticalSection(FCritSecClock);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsClock.MessageProcessed(TimerObj: TIcsThreadTimer);
begin
    FTimerList.LockList;
    try
        TimerObj.FMsgQueued    := FALSE;
        TimerObj.FQueuedTicks  := GetTickCount;
    finally
        FTimerList.UnlockList;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TIcsClockPool }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class function TIcsClockPool.Acquire: TIcsClock;
begin
    EnterCriticalSection(GCritSecClockPool);
    try
        if not Assigned(GIcsClockPool) then
            GIcsClockPool := Create;
        Result := GIcsClockPool.InternalAcquire;
    finally
        LeaveCriticalSection(GCritSecClockPool);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class procedure TIcsClockPool.Release(AClock: TIcsClock);
begin
    EnterCriticalSection(GCritSecClockPool);
    try
        if Assigned(GIcsClockPool) then begin
            GIcsClockPool.InternalRelease(AClock);
            if GIcsClockPool.FClockList.Count = 0 then
                FreeAndNil(GIcsClockPool);
        end;
    finally
        LeaveCriticalSection(GCritSecClockPool);
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
    EnterCriticalSection(GCritSecClockPool);
    try
        for I := 0 to FClockList.Count -1 do begin
            FreeAndNil(PIcsClockRec(FClockList[I])^.Clock);
            Dispose(PIcsClockRec(FClockList[I]));
        end;
        FreeAndNil(FClockList);
    finally
        LeaveCriticalSection(GCritSecClockPool);
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
    InitializeCriticalSection(GCritSecClockPool);
    WM_ICS_THREAD_TIMER := RegisterWindowMessage('OVERBYTE_ICS_THREAD_TIMER');
    if WM_ICS_THREAD_TIMER = 0 then
        raise EIcsTimerException.Create(SysErrorMessage(GetLastError));
  
finalization
    FreeAndNil(GIcsClockPool);
    DeleteCriticalSection(GCritSecClockPool);
  
end.
