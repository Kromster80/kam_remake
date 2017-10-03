{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels
Creation:     April 08, 2006
Description:  A very fast external SSL-session-cache component.
              Finding an element in 65536 requires at most 16 compares.
              Uses OpenSSL (http://www.openssl.org).
              Uses freeware TSslWSocket component  from ICS
              (Internet Component Suite).
Version:      8.00
EMail:        <arno.garrels@gmx.de>
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2006-2010 by Arno Garrels, Berlin, Germany,
              <arno.garrels@gmx.de>

              This software is freeware and provided 'as-is', without any
              express or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of this
              software.

              As this code make use of OpenSSL, your rights are restricted by
              OpenSSL license. See http://www.openssl.org for details.

              Further, the following restrictions applies:

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

History:
06/28/2007 v1.01 A. Garrels fixed a bug with multiple instances.
Jul 03, 2008 V1.02 A. Garrels made a few changes to prepare code for Unicode.
Mar 20, 2009 v1.03 A. Garrels exchanged 4 * PChar by PAnsiChar (was no bug!)
Sep 03, 2009 V1.04 Arno exchanged TThread.Resume by TThread.Start for D2010 and later
May 06, 2011 V1.05 Small change to prepare for 64-bit.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsSslSessionCache;
{$ENDIF}

{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF USE_SSL}
{$IFNDEF COMPILER5_UP}
    Bomb('This unit require Delphi 5  or C++ Builder 5 or higher!');
{$ENDIF}
{$B-}              { Enable partial boolean evaluation   }
{$T-}              { Untyped pointers                    }
{$X+}              { Enable extended syntax              }
{$J+}              { Allow typed constant to be modified }
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}
{$ENDIF}
interface

{$IFDEF USE_SSL}
uses
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SyncObjs{$ELSE}SyncObjs{$ENDIF},
{$IFDEF Compiler6_UP}
    {$IFDEF RTL_NAMESPACES}System.DateUtils{$ELSE}DateUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Types{$ELSE}Types{$ENDIF},
{$ENDIF}
{$IFNDEF NO_DEBUG_LOG}
    OverbyteIcsLogger,
{$ENDIF}
{$IFNDEF COMPILER15_UP}
    OverbyteIcsTypes,
{$ENDIF}
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsWSocket,
{$ELSE}
    OverbyteIcsWSocket,
{$ENDIF}
    OverbyteIcsSSLEAY, OverbyteIcsLIBEAY,
    OverbyteIcsAvlTrees, OverbyteIcsUtils;

type
    ESslSessionCacheException = class(Exception);

    TSslBaseSessionCache = class(TSslBaseComponent)
    private
        FFlushInterval      : Integer;
        FLock               : TCriticalSection;
        FStatLookupCount    : Int64;
        FStatHits           : Int64;
        FMaxCacheSize       : Integer;
        FOnCacheFull        : TNotifyEvent;
    protected
        procedure   Lock;
        procedure   Unlock;
        procedure   SetFlushInterval(const Value: Integer); virtual; abstract;
        procedure   SetMaxCacheSize(const Value: Integer); virtual; abstract;
        function    GetStatCount: Integer; virtual; abstract;
        procedure   TriggerCacheFull; virtual;
        property    FlushInterval: Integer          read  FFlushInterval
                                                    write SetFlushInterval;
        property    MaxCacheSize: Integer           read  FMaxCacheSize
                                                    write SetMaxCacheSize;
        property    StatCount: Integer              read  GetStatCount;
        property    StatHits: Int64                 read  FStatHits;
        property    StatLookupCount: Int64          read  FStatLookupCount;
        property    OnCacheFull : TNotifyEvent      read  FOnCacheFull
                                                    write FOnCacheFull;

    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Flush; virtual; abstract;
        function    GetCliSession(const Key            : String;
                                  var FreeSession      : Boolean): Pointer;
                                                              virtual; abstract;
        function    CacheCliSession(const SslSession   : Pointer;
                                    const Key          : String;
                                    var IncRefCount    : Boolean): Boolean;
                                                              virtual; abstract;
        function    GetSvrSession(const Key            : String;
                                  var IncRefCount      : Boolean): Pointer;
                                                              virtual; abstract;
        function    CacheSvrSession(const SslSession   : Pointer;
                                    const Key          : String;
                                    var AddToInternal  : Boolean): Boolean;
                                                              virtual; abstract;
    end;

    TSslCacheMagic = String[23];
    TSslStreamHdr = packed record
        ID        : TSslCacheMagic;
        Version   : Integer;
        Reserved1 : Integer;
        Reserved2 : Integer;
    end;
    PSslStreamHdr = ^TSslStreamHdr;

    TSslAvlSessionCache = class; // forward
    TCacheEventAction = (ceaSetFlush, ceaStop);
    TSslCacheWorkerThread = class(TThread)
    protected
        FWakeEvent: TEvent;
        FEventAction: TCacheEventAction;
        FSessionCache : TSslAvlSessionCache;
        procedure Execute; override;
    public
        constructor Create(CreateSuspended: Boolean);
        destructor Destroy; override;
    end;


    TSslAvlSessionCache = class(TSslBaseSessionCache)
    private
        FCacheTree      : TCacheTree;
        //FThreadID       : TThreadID;
        FWorkerThread   : TSslCacheWorkerThread;
        FStreamVersion  : Integer;
        FStream         : TStream;
        { Maximum lifetime of a session is set thru property SessionTimeout }
        { of TSslContext.  However a session can be removed from the cache  }
        { earlier if it wasn't resumed within the period of IdleTimeout.    }
        { Set IdleTimeout to zero if sessions shall be kept in cache        }
        { regardless how frequently a client resumes a session.             }
        FIdleTimeout    : Cardinal;
        //FStopEv         : THandle;
        //FSetFlushEv     : THandle;
    protected
        procedure   SetFlushInterval(const Value: Integer); override;
        procedure   SetIdleTimeout(const Value: Cardinal);
        procedure   SetMaxCacheSize(const Value: Integer); override;
        function    GetStatCount: Integer; override;
        function    CacheSession(const SslSession : Pointer;
                                    const Key     : String): Boolean;
        function    GetSession(const Key : String): Pointer;
        procedure   CacheTreeOnList(Sender     : TObject;
                                    const Key  : String;
                                    TimeStamp  : TDateTime;
                                    Data       : Pointer;
                                    Len        : Integer;
                                    Expires    : TDateTime;
                                    var Cancel : Boolean);
        procedure   CacheTreeOnFreeData(Sender : TObject;
                                        Data   : Pointer;
                                        Len    : Integer);
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Flush; override;
        procedure   Clear; 
        function    CacheCliSession(const SslSession : Pointer;
                                    const Key        : String;
                                    var IncRefCount  : Boolean): Boolean; override;
        function    GetCliSession(const Key       : String;
                                  var FreeSession : Boolean): Pointer; override;
        function    GetSvrSession(const Key       : String;
                                  var IncRefCount : Boolean): Pointer; override;
        function    CacheSvrSession(const SslSession  : Pointer;
                                    const Key         : String;
                                    var AddToInternal : Boolean): Boolean; override;
        procedure   SaveToStream(AStream        : TStream;
                                WriteHeader     : Boolean;
                                Header          : TSslStreamHdr);
        procedure   LoadFromStream(AStream      : TStream;
                                   ReadHeader   : Boolean;
                                   CheckHeader  : Boolean;
                                   Version      : Integer = 0);
        procedure   SaveToFile(FileName    : String;
                               WriteHeader : Boolean;
                               Header      : TSslStreamHdr);
        procedure   LoadFromFile(FileName       : String;
                                 ReadHeader     : Boolean;
                                 CheckHeader    : Boolean;
                                 Version        : Integer = 0);
        property    StatCount;
        property    StatHits;
        property    StatLookupCount;
    published
        property    IdleTimeout: Cardinal read FIdleTimeout  write SetIdleTimeout;
        property    FlushInterval;
        property    MaxCacheSize;
        property    OnCacheFull;
    end;

const
    SslCacheMagic : TSslCacheMagic = '#ICS_SESSION_CACHE_XYZ';

{$ENDIF} //USE_SSL

implementation

{$IFDEF USE_SSL}

resourcestring
    rsInvalidCacheFile          = 'Invalid Cache File';
    rsInvalidStreamVersion      = 'Invalid Stream Version';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetCurrentBias : TDateTime;
const
    MinsPerDay = 1440;
begin
    Result := IcsGetLocalTimezoneBias / MinsPerDay;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSslBaseSessionCache }

constructor TSslBaseSessionCache.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FLock := TCriticalSection.Create;
    FFlushInterval      := 0;
    FStatLookupCount    := 0;
    FStatHits           := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslBaseSessionCache.Destroy;
begin
    FLock.Free;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseSessionCache.Lock;
begin
    FLock.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseSessionCache.TriggerCacheFull;
begin
    if Assigned(FOnCacheFull) then
        FOnCacheFull(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseSessionCache.Unlock;
begin
    FLock.Leave;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSslCacheWorkerThread }

constructor TSslCacheWorkerThread.Create(CreateSuspended: Boolean);
begin
    FWakeEvent := TEvent.Create(nil, True, False, '');
    inherited Create(CreateSuspended);
end;

destructor TSslCacheWorkerThread.Destroy;
begin
  FWakeEvent.Free;
  inherited;
end;

procedure TSslCacheWorkerThread.Execute;
var
    dwRes        : TWaitResult;
    Interval     : Integer;
    //fhArray      : array[0..1] of THandle;
begin
    IcsNameThreadForDebugging(AnsiString(Classname));
    Interval   := MaxInt;
    //fhArray[0] := FSessionCache.FStopEv;
    //fhArray[1] := FSessionCache.FSetFlushEv;
    while FEventAction <> ceaStop do
    begin
        dwRes := FWakeEvent.WaitFor(Interval);
        if dwRes = wrTimeout then
            FSessionCache.Flush
        else if dwRes = wrSignaled then
        begin
            if FEventAction = ceaSetFlush then begin
                Interval := FSessionCache.FFlushInterval;
                FWakeEvent.ResetEvent;
            end;
        end
        else if dwRes = wrAbandoned then
            Break
        else
            RaiseLastOSError;
       (*
        dwRes := WaitForMultipleObjects(2,
                                       @fhArray,
                                           False,
                                       Interval);
        // Flush - delete expired entries
        if (dwRes = WAIT_TIMEOUT) then
            FSessionCache.Flush
        // FFlushEv - Reset the timeout
        else if (dwRes - WAIT_OBJECT_0) = 1 then begin
            Interval := FSessionCache.FFlushInterval;
            ResetEvent(FSessionCache.FSetFlushEv);
        end
        // FStopEv - exit
        else if (dwRes - WAIT_OBJECT_0) = 0 then
            Break //***
        else if ((dwRes - WAIT_ABANDONED_0) = 0) or
                         ((dwRes - WAIT_ABANDONED_0) = 1) then
            Break //***
        else
            RaiseLastOSError;
        *)
    end; // True-loop
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSslTreeSessionCache }

constructor TSslAvlSessionCache.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FCacheTree            := TCacheTree.Create;
    FCacheTree.OnList     := CacheTreeOnList;
    FCacheTree.OnFreeData := CacheTreeOnFreeData;
    //FStopEv  := 0;
    //FSetFlushEv := 0;

    if not (csDesigning in ComponentState) then
    begin
      {  FStopEv     := CreateEvent(nil, True, False, nil);
        FSetFlushEv := CreateEvent(nil, True, False, nil);
        if (FStopEv = 0) or (FSetFlushEv = 0) then
            raise Exception.Create('Cannot create Events: '
                                   + SysErrorMessage(GetLastError));}
        FWorkerThread := TSslCacheWorkerThread.Create(True);
        FWorkerThread.FSessionCache := Self;
        FWorkerThread.FreeOnTerminate := FALSE;
        //FThreadID := FWorkerThread.ThreadID;
    {$IFDEF COMPILER14_UP}
        FWorkerThread.Start;
    {$ELSE}
        FWorkerThread.Resume;
    {$ENDIF}
        Sleep(0);
    end;

    FMaxCacheSize    := 1000;       // entries
    FlushInterval    := 1000 * 10;  // ms
    FIdleTimeout     := 30;         // sec
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslAvlSessionCache.Destroy;
begin
    if Assigned(FWorkerThread) and Assigned(FWorkerThread.FWakeEvent) then begin
        FWorkerThread.FEventAction := ceaStop;
        FWorkerThread.FWakeEvent.SetEvent;
        FWorkerThread.WaitFor;
    end;
    FreeAndNil(FWorkerThread);
    Lock;
    try
        FreeAndNil(FCacheTree);
        FinalizeSsl;
    finally
        Unlock;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslAvlSessionCache.SetFlushInterval(const Value: Integer);
begin
    Lock;
    try
        if Value = FFlushInterval then
            Exit;
        FFlushInterval := Value;
        if FFlushInterval <= 0 then
            FFlushInterval := MaxInt
        else if FFlushInterval < 1000 then
            FFlushInterval := 1000;
        if (csDesigning in ComponentState) then
            Exit;    
        FWorkerThread.FWakeEvent.SetEvent;
    finally
        UnLock
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslAvlSessionCache.Flush;
begin
    Lock;
    try
        FCacheTree.Flush(Now);
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslAvlSessionCache.SetMaxCacheSize(const Value: Integer);
begin
    Lock;
    try
        if Value >= 0 then
            FMaxCacheSize := Value
        else
            FMaxCacheSize := 0;
        while (FCacheTree.Count > FMaxCacheSize) and (FCacheTree.Count > 0) do
            FCacheTree.Remove(FCacheTree.Oldest);
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslAvlSessionCache.CacheSession(const SslSession: Pointer;
  const Key: String): Boolean;
var
    Buf     : PAnsiChar;
    Len     : Integer;
    t1,t2   : TDateTime;
    Data    : Pointer;
    SessionTimeOut : Cardinal;
begin
    Result := False;
    InitializeSsl;
    Len := f_i2d_SSL_SESSION(SslSession, nil);
    if Len > 0 then
    begin
        if FCacheTree.Count >= FMaxCacheSize then
            TriggerCacheFull;
        while (FCacheTree.Count >= FMaxCacheSize) and (FCacheTree.Count > 0) do
            FCacheTree.Remove(FCacheTree.Oldest);
        GetMem(Data, Len);
        Buf := PAnsiChar(Data);
        Len := f_i2d_SSL_SESSION(SslSession, @Buf); // sets internal Time = now (UTC)
        if Len > 0 then
        begin
            t2 := NOW;
            SessionTimeOut := f_SSL_SESSION_get_timeout(SslSession);
            { The final timeout date is determined by property SessionTimeout }
            { of the SslContext object.                                       }
            t1 := IncSecond(t2, SessionTimeOut);
            if (FIdleTimeout > 0) and (FIdleTimeout < SessionTimeOut) then // if > 0 we use activity for
            begin
                t2 := IncSecond(t2, FIdleTimeout);
                FCacheTree.Insert(Key, Data, Len, t2, t1, TRUE, TRUE);
            end
            else
                FCacheTree.Insert(Key, Data, Len, t1, t1, TRUE, TRUE);

            Result := True;
       {$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslInfo) then
                DebugLog(loSslInfo,
                         Format('SSLAvlSessionCache item cached, Size ' +
                                '%d LookupCnt %d Hits %d',
                                [StatCount, FStatLookupCount, FStatHits]));
        {$ENDIF}
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslAvlSessionCache.GetSession(const Key: String): Pointer;
var
    Node   : TCacheNode;
    Buf    : PAnsiChar;
    t1, t2 : TDateTime;
begin
    Result := nil;
    InitializeSsl;
    Inc(FStatLookupCount);
    Node := FCacheTree.FindKey(Key);
    if Node <> nil then
    begin
        t1 := Now;
        if Node.IdxRef.TimeStamp < t1 then
        begin
            FCacheTree.Remove(Node);
            Exit;
        end;
        Buf := PAnsiChar(Node.Data);
        Result := f_d2i_SSL_SESSION(nil, @Buf, Node.Len);
        if Result <> nil then
        begin
            if (FIdleTimeout > 0) and (FIdleTimeout < Node.IdxRef.Expires) then
            begin
                t2 := Node.IdxRef.Expires;
                t1 := IncSecond(t1, FIdleTimeout);
                if t2 > t1 then  // update the TimeStamp
                    FCacheTree.Insert(Key, nil, 0, t1, t2, TRUE, FALSE)
                else
                    FCacheTree.Insert(Key, nil, 0, t2, t2, TRUE, FALSE);
            end;
            Inc(FStatHits);
    {$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslInfo) then
                DebugLog(loSslInfo,
                         Format('SSLAvlSessionCache item grabbed, Size ' +
                                '%d LookupCnt %d Hits %d',
                                [StatCount, FStatLookupCount, FStatHits]));
    {$ENDIF}
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslAvlSessionCache.CacheSvrSession(const SslSession: Pointer;
  const Key: String; var AddToInternal: Boolean): Boolean;
begin
    Lock;
    try
        Result := CacheSession(SslSession, Key);
        AddToInternal := False;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslAvlSessionCache.GetSvrSession(const Key: String;
    var IncRefCount: Boolean): Pointer;
begin
    Lock;
    try
        Result := GetSession(Key);
        IncRefCount := False;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslAvlSessionCache.CacheCliSession(const SslSession: Pointer;
    const Key: String; var IncRefCount: Boolean): Boolean;
begin
    Lock;
    try
        Result := CacheSession(SslSession, Key);
        IncRefCount := False;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslAvlSessionCache.GetCliSession(const Key: String;
    var FreeSession: Boolean): Pointer;
begin
    Lock;
    try
        Result := GetSession(Key);
        FreeSession := TRUE;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslAvlSessionCache.SaveToFile(FileName: String;
  WriteHeader: Boolean; Header: TSslStreamHdr);
var
    FS : TFileStream;
begin
    Lock;
    try
        FS := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
        try
            SaveToStream(FS, WriteHeader, Header);
        finally
            FreeAndNil(FS)
        end;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslAvlSessionCache.LoadFromFile(FileName: String; ReadHeader,
    CheckHeader: Boolean; Version: Integer);
var
    FS: TFileStream;
begin
    Lock;
    try
        FS := TFileStream.Create(FileName, fmOpenRead or fmShareExclusive);
        try
            LoadFromStream(FS, ReadHeader, CheckHeader, Version);
        finally
            FreeAndNil(FS)
        end;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslAvlSessionCache.LoadFromStream(AStream: TStream; ReadHeader,
  CheckHeader: Boolean; Version: Integer);
var
    Hdr     : TSslStreamHdr;
    Key     : String;
    Data    : Pointer;
    Len     : Integer;
    Time    : TDateTime;
    Expires : TDateTime;
    n       : Integer;
begin
    Lock;
    try
        if Readheader then
        begin
            AStream.Read(Hdr, SizeOf(TSslStreamHdr));
            if CheckHeader then
            begin
                if (Hdr.ID <> SslCacheMagic) then
                    raise ESslSessionCacheException.Create(rsInvalidCacheFile);
                if Hdr.Version <> Version then
                    raise ESslSessionCacheException.Create(rsInvalidStreamVersion);
            end;
            FStreamVersion := Hdr.Version;
        end
        else
            FStreamVersion := Version;
        while AStream.Position < AStream.Size do
        begin
            AStream.Read(n, SizeOf(Integer));  { Number of chars }
            SetLength(Key, n);
            AStream.Read(Pointer(Key)^, n * SizeOf(Char));
            AStream.Read(Time, SizeOf(TDateTime));
            AStream.Read(Expires, SizeOf(TDateTime));
            AStream.Read(Len, SizeOf(Integer));
            GetMem(Data, Len);
            AStream.Read(Data^, Len);
            FCacheTree.Insert(Key, Data, Len, Time, Expires);
        end;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslAvlSessionCache.SaveToStream(AStream: TStream;
  WriteHeader: Boolean; Header: TSslStreamHdr);
begin
    Lock;
    try
        FStream := AStream;
        if WriteHeader then
            FStream.Write(Header, SizeOf(TSslStreamHdr));
        FCacheTree.ListTree;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslAvlSessionCache.CacheTreeOnList(Sender: TObject;
    const Key: String; TimeStamp: TDateTime; Data: Pointer; Len: Integer;
    Expires: TDateTime; var Cancel: Boolean);
var
    n : Integer;
begin
    n := Length(Key);
    FStream.Write(n, SizeOf(Integer)); { Number of chars }
    FStream.Write(Pointer(Key)^, n * SizeOf(Char));
    FStream.Write(TimeStamp, SizeOf(TDateTime));
    FStream.Write(Expires, SizeOf(TDateTime));
    FStream.Write(Len, SizeOf(Integer));
    FStream.Write(Data^, Len);
    Cancel := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslAvlSessionCache.CacheTreeOnFreeData(Sender: TObject;
    Data: Pointer; Len: Integer);
begin
    Lock;
    try
        if Assigned(Data) then
            FreeMem(Data, Len);
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslAvlSessionCache.Clear;
begin
    Lock;
    try
        FCacheTree.Clear;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslAvlSessionCache.GetStatCount: Integer;
begin
    Result := FCacheTree.Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslAvlSessionCache.SetIdleTimeout(const Value: Cardinal);
begin
    Lock;
    try
        FIdleTimeout := Value;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF}//USE_SSL
end.

