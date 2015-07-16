{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     Nov 01, 2005
Description:  Implementation of OpenSsl thread locking (Windows);
Version:      8.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2005-2011 by François PIETTE
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


How to use:  TSslStaticLock and TSslDynamicLock implement the locking callbacks
             required for OpenSSL to be thread-safe. Currently (v0.98a)
             only static locking is used by OpenSSL, dynamic locking is
             for future OSSL versions, thus TSslDynamicLock is untested so far.
             Simply drop one of the components per application on the form or
             create it from within your foreground thread at runtime. Set
             property Enabled to TRUE before any of the OpenSSL functions are
             called, that's it. Note that setting Enabled to TRUE also loads
             and initializes the OpenSSL libraries. Any multi-threaded OpenSSL
             application MUST provide a single, enabled locking component,
             otherwise random errors in OpenSSL will crash the application.

History:
March 03, 2006 Version 1.01, new property Enabled, OpenSSL is now loaded
          when Enabled is set to TRUE.
Jun 30, 2008 A.Garrels made some changes to prepare SSL code for Unicode.
May 05, 2010 V1.02 A.Garrels changed synchronisation to use TRTLCriticalSection
             instead of Mutex, removed useless contructor, should be POSIX-ready.
May 06, 2011 V1.03 Arno - Make use of new CRYPTO_THREADID_set_callback.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsSslThrdLock;
{$ENDIF}

{$B-}              { Enable partial boolean evaluation   }
{$T-}              { Untyped pointers                    }
{$X+}              { Enable extended syntax              }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

{#$DEFINE NO_DYNLOCK}

interface

{$IFDEF USE_SSL}

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Posix.SysTypes,
    Posix.Pthread,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SysConst{$ELSE}SysConst{$ENDIF},
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsWSocket,
{$ELSE}
    OverbyteIcsWSocket,
{$ENDIF}
    OverbyteIcsUtils,
    OverbyteIcsLIBEAY,
    OverbyteIcsSSLEAY;

type
    ESslLockException = class(Exception);
    TMutexBuf  = array of TIcsCriticalSection;
    TSslStaticLock = class(TComponent)
    protected
        FSslInitialized : Boolean;
        FEnabled    : Boolean;
        procedure   InitializeSsl; virtual;
        procedure   FinalizeSsl; virtual;
        procedure   SetEnabled(const Value: Boolean); virtual;
    public
        destructor  Destroy; override;
    published
        property    Enabled : Boolean read FEnabled write SetEnabled;
    end;
{$IFNDEF NO_DYNLOCK}
    { Suggested for use with future openssl versions, not yet tested }
    TSslDynamicLock = class(TSslStaticLock)
    protected
        procedure SetEnabled(const Value: Boolean); override;
    end;
{$ENDIF}
{$ENDIF} // USE_SSL

implementation

{$IFDEF USE_SSL}

var
   MutexBuf : TMutexBuf;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslStaticLock.Destroy;
begin
    if Enabled then
        SetEnabled(FALSE);
    FinalizeSsl;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslStaticLock.FinalizeSsl;
begin
    if not FSslInitialized then
        Exit;
    UnloadSsl;
    FSslInitialized := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslStaticLock.InitializeSsl;
begin
    if FSslInitialized then
        Exit;
    LoadSsl;
    FSslInitialized := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MutexSetup(var Mutex : TIcsCriticalSection);
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Mutex := TIcsCriticalSection.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MutexCleanup(var Mutex : TIcsCriticalSection);
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    FreeAndNil(Mutex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MutexLock(Mutex: TIcsCriticalSection);
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Mutex.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MutexUnlock(Mutex : TIcsCriticalSection);
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Mutex.Leave;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure StatLockCallback(Mode : Integer; N : Integer;
    const _File : PAnsiChar; Line : Integer); cdecl;
begin
    if Mode and Crypto_Lock <> 0 then
        MutexLock(MutexBuf[n])
    else
        MutexUnLock(MutexBuf[n])
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function IDCallback : Longword; cdecl;
begin
    Result := IcsGetCurrentThreadID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ELSE}
procedure ThreadIdCallback(ID : PCRYPTO_THREADID); cdecl;
begin
    f_CRYPTO_THREADID_set_pointer(ID, Pointer(IcsGetCurrentThreadID));  // ToCheck
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslStaticLock.SetEnabled(const Value: Boolean);
var
    I : Integer;
begin
    if Value = FEnabled then
        Exit;
    if not (csDesigning in ComponentState) then
    begin
        InitializeSsl;
        if Value then
        begin
            SetLength(MutexBuf, f_CRYPTO_num_locks);
            try
                FillChar(MutexBuf[0], Length(MutexBuf) * SizeOf(Pointer), 0);
                for I := Low(MutexBuf) to High(MutexBuf) do
                    MutexSetup(MutexBuf[I]);
            except
                on E: Exception do begin
                    for I := Low(MutexBuf) to High(MutexBuf) do
                        MutexCleanup(MutexBuf[I]);
                    SetLength(MutexBuf, 0);
                    raise;
                end;
            end;
        {$IFDEF MSWINDOWS}
            f_CRYPTO_set_id_callback(IDCallback);
        {$ELSE}
            if Assigned(f_CRYPTO_THREADID_set_callback) then // OpenSSL v1.0.0+
            begin
                Assert(Assigned(f_CRYPTO_THREADID_set_pointer));
                f_CRYPTO_THREADID_set_callback(ThreadIdCallback);
            end
            else
        {$ENDIF}
            f_CRYPTO_set_locking_callback(StatLockCallback);
        end
        else begin
            if FSslInitialized then begin
                f_CRYPTO_set_locking_callback(nil);
                f_CRYPTO_set_id_callback(nil);
            end;
            for I := Low(MutexBuf) to High(MutexBuf) do
                MutexCleanup(MutexBuf[I]);
            SetLength(MutexBuf, 0);
        end;
    end;
    FEnabled := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_DYNLOCK}
function DynCreateCallBack(const _file : PAnsiChar;
                           Line: Integer): PCRYPTO_dynlock_value; cdecl;
begin
    New(Result);
    MutexSetup(Result^.Mutex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DynDestroyCallBack(L : PCRYPTO_dynlock_value; _File : PAnsiChar;
    Line: Integer); cdecl;
begin
    if Assigned(L) then
    begin
        MutexCleanUp(L^.Mutex);
        Dispose(L);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DynLockCallback(Mode : Integer; L : PCRYPTO_dynlock_value;
    _File : PAnsiChar; Line: Integer); cdecl;
begin
    if Assigned(L) then
    begin
        if Mode and CRYPTO_LOCK <> 0 then
            MutexLock(L^.Mutex)
        else
            MutexUnlock(L^.Mutex);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslDynamicLock.SetEnabled(const Value: Boolean);
var
    OldValue : Boolean;
begin
    OldValue := FEnabled;
    inherited SetEnabled(Value);
    if OldValue = FEnabled then
        Exit;
    if not (csDesigning in ComponentState) then begin
        if Value then begin
            InitializeSsl;
            f_CRYPTO_set_dynlock_create_callback(DynCreateCallBack);
            f_CRYPTO_set_dynlock_lock_callback(DynLockCallback);
            f_CRYPTO_set_dynlock_destroy_callback(DynDestroyCallBack);
        end
        else begin
            if FSslInitialized then begin
                f_CRYPTO_set_dynlock_create_callback(nil);
                f_CRYPTO_set_dynlock_lock_callback(nil);
                f_CRYPTO_set_dynlock_destroy_callback(nil);
            end;
        end;

    end;
    FEnabled := Value;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF} // USE_SSL
end.
