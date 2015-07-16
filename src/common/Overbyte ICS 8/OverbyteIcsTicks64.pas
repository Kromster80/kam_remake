{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  GetTickCount64 support for all versions of Windows
Creation:     3 September 2009
Updated:      22 May 2012
Version:      8.00
Email:        delphi@magsys.co.uk  http://www.magsys.co.uk
Legal issues: Copyright (C) 2009 by Magenta Systems Ltd, England

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


Updates:
3 Sep 2009 - 1.00 baseline Angus
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory



Background:

There is a test and demo application OverbyteIcsTicks64Demo for this unit.

A tick is one millisecond.

GetTickCount returns a 32-bit unsigned double word that wraps around after about
49 days, GetTickCount64 was added with Vista and Windows 2008 and returns a 64-bit
unsigned integer.  IcsGetTickCount64 uses QueryPerformanceCounter to replicate
GetTickCount64 on Windows 2000, XP and 2003 and GetTickCount64 on Vista and better.
There are various functions for comparing 64-bit ticks, which may act either as
event triggers or timers.

Note: GetTickCount and IcsGetTickCount64 using QueryPerformanceCounter don't start
from zero at the same time.  While this unit attempts to align the two APIs,
generally don't mix them for timing purposes, and doing so will break after 49 days.

Warning: QueryPerformanceCounter may conceivably not be implemented in hardware
and some faulty hardware may cause the counter to jump forward a few seconds
if the PCI bus is heavily loaded, see comments below.  If this is of concern,
call IcsInitTick64 (TicksAPI32) to force 32- bit GetTickCount.

IcsNowPC is similar to Now, but is corrected using the performance counter for
1ms or better resolution which may be useful when logging the duration of
events.

The IcsPerfxx functions may be used to calculate timings accurate to micro seconds.

IcsElapsedxx functions return milliseconds, seconds or minutes since an event
start was saved using IcsGetTickCount64

IcsGetTrgxx functions return a tick count so many seconds or minutes ahead, which
may be tested with IcsTestTrgTick64 to trigger an event, or set to Trigger64Disabled
to ignore the trigger or Trigger64Immediate for an immediate trigger. 


Various comments on windows timing:

http://www.geisswerks.com/ryan/FAQS/timing.html

On Windows 9x, GetTickCount has a resolution of about 1ms, but on Windows NT or
later it is usually between 10 and 15ms.  This resolution also effects the real
time clock, including Now and Time.

Knowledgebase Article ID: 274323 - Performance counter value may unexpectedly leap
forward a few seconds due to a design defect in some south bridge of some chipsets
under heavy PCI bus loads, the KB article lists known bad chipsets, including the
Intel PIIX4E South Bridge.

SetThreadAffinityMask() - make your thread stick to one core or the other, so that
QueryPerformanceCounter() does not have timing issues in dual core systems.

timeBeginPeriod(1) and timeEndPeriod(1) change timing precision from default 15ms to 1ms
for GetTickCount, Sleep (), WaitForSingleObject(), but it can also reduce overall system
performance, because the thread scheduler switches tasks more often.  Only effects current
application.

QueryPerformanceFrequency varies between PCs, commonly about 3.57 million ticks per
second, but a Core2 2.4GHz PC returned 2.4 gig ticks per second.

This unit has been tested on Windows 98, 2000, XP, 2003, Vista, 2008 and 7, the
Windows 2000 server had been running for 46,470,400 seconds which is 537 days.

}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsTicks64;

interface

uses
    Windows, SysUtils;

{$IFDEF ConditionalExpressions}
  {$IF CompilerVersion >= 18.00}
    {$DEFINE USE_INLINE}
  {$IFEND}
{$ENDIF}

{ Q-}
{ R-}

const
    ISODateMask = 'yyyy-mm-dd' ;
    ISODateTimeMask = 'yyyy-mm-dd"T"hh:nn:ss' ;
    ISODateLongTimeMask = 'yyyy-mm-dd"T"hh:nn:ss.zzz' ;
    ISOTimeMask = 'hh:nn:ss' ;
    LongTimeMask = 'hh:nn:ss:zzz' ;
    Ticks64PerDay      : int64 =  24 * 60 * 60 * 1000 ;
    Ticks64PerHour     : int64 = 60 * 60 * 1000 ;
    Ticks64PerMinute   : int64 = 60 * 1000 ;
    Ticks64PerSecond   : int64 = 1000 ;
    Trigger64Disabled  : int64 = high (int64) ;
    Trigger64Immediate : int64 = high (int64) - 1 ;

type
    TTicks64Mode = (TicksNone, TicksAPI64, TicksPerf, TicksAPI32);

    function IcsGetTickCount64: int64;
    procedure IcsInitTick64 (NewMode: TTicks64Mode);
    function IcsNowPC : TDateTime;
    procedure IcsAlignNowPC;
    function IcsLastBootDT: TDateTime;
    function IcsGetPerfCountsPerSec: int64;
    function IcsPerfCountCurrent: int64; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function IcsPerfCountCurrMilli: int64; {$IFDEF USE_INLINE} inline; {$ENDIF}
    function IcsPerfCountToMilli (LI: int64): int64;
    function IcsPerfCountGetMilli (startLI: int64): int64;
    function IcsPerfCountGetMillStr (startLI: int64): string;
    function IcsPerfCountToSecs (LI: int64): integer;
    function IcsPerfCountGetSecs (startLI: int64): integer;
    function IcsDiffTicks64 (const StartTick, EndTick: int64): int64;
    function IcsElapsedTicks64 (const StartTick: int64): int64;
    function IcsElapsedMsecs64 (const StartTick: int64): int64;
    function IcsElapsedSecs64 (const StartTick: int64): integer;
    function IcsElapsedMins64 (const StartTick: int64): integer;
    function IcsWaitingSecs64 (const EndTick: int64): integer;
    function IcsGetTrgMSecs64 (const MilliSecs: int64): int64;
    function IcsGetTrgSecs64 (const DurSecs: integer): int64;
    function IcsGetTrgMins64 (const DurMins: integer): int64;
    function IcsTestTrgTick64 (const TrgTick: int64): boolean;
    function IcsAddTrgMsecs64 (const TickCount, MilliSecs: int64): int64;
    function IcsAddTrgSecs64 (const TickCount: int64; DurSecs: integer): int64;

var
    Ticks64Mode: TTicks64Mode;


implementation

var
    _GetTickCount64: function: int64; stdcall;
    PerfFreqCountsPerSec: int64 ;
    f_PCStartValue: int64 ;
    f_TDStartValue: TDateTime ;
    f_PCCountsPerDay: extended ;
    Tick64Correction: int64 ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure LoadGetTickCount64;
var
    Kernel32: THandle;
begin
    Kernel32 := GetModuleHandle ('kernel32');
    if Kernel32 = 0 then exit;
    _GetTickCount64 := GetProcAddress (Kernel32, 'GetTickCount64') ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetPerfCountsPerSec: int64;
begin
    if PerfFreqCountsPerSec = 0 then
                    QueryPerformanceFrequency (PerfFreqCountsPerSec);
    Result := PerfFreqCountsPerSec;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsPerfCountCurrent: int64;
begin
    QueryPerformanceCounter (Result);
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsPerfCountCurrMilli: int64;
begin
    QueryPerformanceCounter (Result) ;
    Result := Result div (IcsGetPerfCountsPerSec div 1000);
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsPerfCountToMilli (LI: int64): int64 ;
begin
    Result := LI div (IcsGetPerfCountsPerSec div 1000);
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsPerfCountToSecs (LI: int64): integer;
begin
    Result := LI div IcsGetPerfCountsPerSec;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsPerfCountGetMilli (startLI: int64): int64;
var
    curLI: int64;
begin
    QueryPerformanceCounter (curLI);
    Result := IcsPerfCountToMilli (curLI - startLI);
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsPerfCountGetSecs (startLI: int64): integer;
var
    curLI: int64;
begin
    QueryPerformanceCounter (curLI);
    Result := IcsPerfCountToSecs (curLI - startLI);
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsPerfCountGetMillStr (startLI: int64): string;
begin
    Result := IntToStr (IcsPerfCountGetMilli (startLI)) + 'ms';
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// check WM_TIMECHANGE message and call this function if the real time clock changes
procedure IcsAlignNowPC;
begin
    f_TDStartValue := Now;
    f_PCStartValue := IcsPerfCountCurrent;
    f_PCCountsPerDay := IcsGetPerfCountsPerSec * SecsPerDay;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// function that returns the current time and date to a high resolution
function IcsNowPC: TDateTime;
var
    f_Now: comp;
    f_ElapsedSinceStart: extended;
begin
    if f_PCCountsPerDay = 0 then IcsAlignNowPC ;
    f_Now := IcsPerfCountCurrent;
    if f_Now = 0 then  // conceivable that performance counter not available
    begin
        Result := Now;
        exit;
    end;
    f_ElapsedSinceStart := f_Now - f_PCStartValue;
    If f_ElapsedSinceStart < 0.0 then
              f_ElapsedSinceStart := f_ElapsedSinceStart - 1;  // Rolled over
    Result := f_TDStartValue + (f_ElapsedSinceStart / f_PCCountsPerDay);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsLastBootDT: TDateTime;
begin
    if IcsPerfCountCurrent = 0 then
        Result := Now - (GetTickCount / MSecsPerDay)
    else
        Result := Now - (IcsPerfCountCurrent div IcsGetPerfCountsPerSec) / SecsPerDay;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsInitTick64 (NewMode: TTicks64Mode);
begin
    Ticks64Mode := TicksAPI32; // default to old GetTickCount
    if NewMode = TicksAPI32 then exit;

  // load API for Vista, Windows 2008, 7 or later
    if (NewMode in [TicksNone, TicksAPI64]) and (Win32MajorVersion >= 6) then
    begin
        LoadGetTickCount64;
        if Assigned (_GetTickCount64) then  // found it OK
        begin
            Ticks64Mode := TicksAPI64;
            exit;
        end ;
    end ;

   // conceivable that performance counter not available in hardware
    if (IcsGetPerfCountsPerSec <> 0) and (IcsPerfCountCurrent <> 0) then
    begin
         Ticks64Mode := TicksPerf;

       // try and align perf counter with GetTickCount provided within 15 minutes
       // warning - Virtual Machines may have poor accuracy, even 2% slow
         Tick64Correction := IcsPerfCountCurrMilli - GetTickCount;
         if Tick64Correction > 900000 then Tick64Correction := 0;
    end ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetTickCount64: int64;
begin
    if Ticks64Mode = TicksNone then IcsInitTick64 (TicksNone);
    case Ticks64Mode of
        TicksAPI64: Result := _GetTickCount64;
        TicksPerf:  Result := IcsPerfCountCurrMilli - Tick64Correction;
    else
        Result := GetTickCount;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsDiffTicks64 (const StartTick, EndTick: int64): int64;
begin
    if (StartTick = Trigger64Immediate) or (StartTick = Trigger64Disabled) then
        Result := 0
    else
    begin
        if (Ticks64Mode in [TicksAPI64, TicksPerf]) or (EndTick >= StartTick) then
            Result := EndTick - StartTick
        else
            Result := ($FFFFFFFF - StartTick) + EndTick;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsElapsedMSecs64 (const StartTick: int64): int64;
begin
    Result := IcsDiffTicks64 (StartTick, IcsGetTickCount64);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsElapsedTicks64 (const StartTick: int64): int64;
begin
    Result := IcsDiffTicks64 (StartTick, IcsGetTickCount64);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsElapsedSecs64 (const StartTick: int64): integer;
begin
    Result := (IcsDiffTicks64 (StartTick, IcsGetTickCount64)) div Ticks64PerSecond;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsWaitingSecs64 (const EndTick: int64): integer ;
begin
    if (EndTick = Trigger64Immediate) or (EndTick = Trigger64Disabled) then
        Result := 0
    else
        Result := (IcsDiffTicks64 (IcsGetTickCount64, EndTick)) div Ticks64PerSecond;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsElapsedMins64 (const StartTick: int64): integer;
begin
    Result := (IcsDiffTicks64 (StartTick, IcsGetTickCount64)) div Ticks64PerMinute;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsAddTrgMsecs64 (const TickCount, MilliSecs: int64): int64;
begin
    Result := MilliSecs + TickCount ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsAddTrgSecs64 (const TickCount: int64; DurSecs: integer): int64;
begin
    Result := TickCount ;
    if DurSecs < 0 then exit;
    Result := IcsAddTrgMsecs64 (TickCount, DurSecs * Ticks64PerSecond);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetTrgMsecs64 (const MilliSecs: int64): int64;
begin
    Result := Trigger64Immediate ;
    if MilliSecs < 0 then
        exit;
    Result := IcsAddTrgMsecs64 (IcsGetTickCount64,  MilliSecs);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetTrgSecs64 (const DurSecs: integer): int64;
begin
    Result := Trigger64Immediate;
    if DurSecs < 0 then exit;
    Result := IcsAddTrgMsecs64 (IcsGetTickCount64, DurSecs * Ticks64PerSecond);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsGetTrgMins64 (const DurMins: integer): int64;
begin
    Result := Trigger64Immediate;
    if DurMins < 0 then exit;
    Result := IcsAddTrgMsecs64 (IcsGetTickCount64, DurMins * Ticks64PerMinute);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsTestTrgTick64 (const TrgTick: int64): boolean;
var
    curtick: int64 ;
begin
    Result := FALSE;
    if TrgTick = Trigger64Disabled then exit;  { special case for trigger disabled }
    if TrgTick = Trigger64Immediate then
    begin
        Result := TRUE;  { special case for now }
        exit;
    end;
    curtick := IcsGetTickCount64;
 { 64-bit, or less than 25 days, keep it simple }
    if (Ticks64Mode in [TicksAPI64, TicksPerf]) or (curtick <= $7FFFFFFF) then
    begin
        if curtick >= TrgTick then Result := TRUE;
        exit;
    end;
    if TrgTick <= $7FFFFFFF then exit;  { trigger was wrapped, can not have been reached  }
    if curtick >= TrgTick then Result := TRUE;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
finalization

end.

