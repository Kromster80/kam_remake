const
  GWsDLLName      = 'wsock32.dll';      { 32 bits TCP/IP system DLL }
  GWs2DLLName     = 'ws2_32.dll';       { 32 bits TCP/IP system DLL version 2}
  GWship6DLLName  = 'wship6.dll';       { IPv6 }

var
  WSocketGForced  : Boolean = FALSE;

  GWsDLLHandle      : HMODULE  = 0;
  GWs2DLLHandle     : HMODULE  = 0;
  GWship6DllHandle  : HMODULE  = 0;
  GWs2IPv6ProcHandle: HMODULE  = 0;
  GInitData         : TWSADATA;

type
    TWSAStartup            = function (wVersionRequired: word;
                                       var WSData: TWSAData): Integer; stdcall;
    TWSACleanup            = function : Integer; stdcall;
    TWSASetLastError       = procedure (iError: Integer); stdcall;
    TWSAGetLastError       = function : Integer; stdcall;
    TWSACancelAsyncRequest = function (hAsyncTaskHandle: THandle): Integer; stdcall;
    TWSAAsyncGetHostByName = function (HWindow: HWND;
                                       wMsg: u_int;
                                       name, buf: PAnsiChar;
                                       buflen: Integer): THandle; stdcall;
    TWSAAsyncGetHostByAddr = function (HWindow: HWND;
                                       wMsg: u_int; addr: PAnsiChar;
                                       len, Struct: Integer;
                                       buf: PAnsiChar;
                                       buflen: Integer): THandle; stdcall;
    TWSAAsyncSelect        = function (s: TSocket;
                                       HWindow: HWND;
                                       wMsg: u_int;
                                       lEvent: Longint): Integer; stdcall;
    TGetServByName         = function (name, proto: PAnsiChar): PServEnt; stdcall;
    TGetProtoByName        = function (name: PAnsiChar): PProtoEnt; stdcall;
    TGetHostByName         = function (name: PAnsiChar): PHostEnt; stdcall;
    TGetHostByAddr         = function (addr: Pointer; len, Struct: Integer): PHostEnt; stdcall;
    TGetHostName           = function (name: PAnsiChar; len: Integer): Integer; stdcall;
    TOpenSocket            = function (af, Struct, protocol: Integer): TSocket; stdcall;
    TShutdown              = function (s: TSocket; how: Integer): Integer; stdcall;
    TSetSockOpt            = function (s: TSocket; level, optname: Integer;
                                       optval: PAnsiChar;
                                       optlen: Integer): Integer; stdcall;
    TGetSockOpt            = function (s: TSocket; level, optname: Integer;
                                       optval: PAnsiChar;
                                       var optlen: Integer): Integer; stdcall;
    TSendTo                = function (s: TSocket; var Buf;
                                       len, flags: Integer;
                                       var addrto: TSockAddr;
                                       tolen: Integer): Integer; stdcall;
    TSend                  = function (s: TSocket; var Buf;
                                       len, flags: Integer): Integer; stdcall;
    TRecv                  = function (s: TSocket;
                                       var Buf;
                                       len, flags: Integer): Integer; stdcall;
    TRecvFrom              = function (s: TSocket;
                                       var Buf; len, flags: Integer;
                                       var from: TSockAddr;
                                       var fromlen: Integer): Integer; stdcall;
    Tntohs                 = function (netshort: u_short): u_short; stdcall;
    Tntohl                 = function (netlong: u_long): u_long; stdcall;
    TListen                = function (s: TSocket;
                                       backlog: Integer): Integer; stdcall;
    TIoctlSocket           = function (s: TSocket; cmd: DWORD;
                                       var arg: u_long): Integer; stdcall;
    TWSAIoctl              = function (s                 : TSocket;
                                       IoControlCode     : DWORD;
                                       InBuffer          : Pointer;
                                       InBufferSize      : DWORD;
                                       OutBuffer         : Pointer;
                                       OutBufferSize     : DWORD;
                                       var BytesReturned : DWORD;
                                       Overlapped        : POverlapped;
                                       CompletionRoutine : FARPROC): Integer; stdcall;
    TInet_ntoa             = function (inaddr: TInAddr): PAnsiChar; stdcall; 
    TInet_addr             = function (cp: PAnsiChar): u_long; stdcall;
    Thtons                 = function (hostshort: u_short): u_short; stdcall;
    Thtonl                 = function (hostlong: u_long): u_long; stdcall;
    TGetSockName           = function (s: TSocket; var name: TSockAddr;
                                       var namelen: Integer): Integer; stdcall;
    TGetPeerName           = function (s: TSocket; var name: TSockAddr;
                                       var namelen: Integer): Integer; stdcall;
    TConnect               = function (s: TSocket; var name: TSockAddr;
                                       namelen: Integer): Integer; stdcall;
    TCloseSocket           = function (s: TSocket): Integer; stdcall;
    TBind                  = function (s: TSocket; var addr: TSockAddr;
                                       namelen: Integer): Integer; stdcall;
    TAccept                = function (s: TSocket; addr: PSockAddr;
                                       addrlen: PInteger): TSocket; stdcall;
    TGetAddrInfoA          = function(NodeName: PAnsiChar; ServName: PAnsiChar;
                                      Hints: PAddrInfoA;
                                      var Addrinfo: PAddrInfoA): Integer; stdcall;
    TGetAddrInfoW          = function(NodeName: PWideChar; ServName: PWideChar;
                                      Hints: PAddrInfoW;
                                      var Addrinfo: PAddrInfoW): Integer; stdcall;
    TFreeAddrInfoA         = procedure(ai: PAddrInfoA); stdcall;
    TFreeAddrInfoW         = procedure(ai: PAddrInfoW); stdcall;
    TGetNameInfoA          = function(addr: PSockAddr; namelen: Integer;
                                      host: PAnsiChar; hostlen: DWORD;
                                      serv: PAnsiChar; servlen: DWORD;
                                      flags: Integer): Integer; stdcall;
    TGetNameInfoW          = function(addr: PSockAddr; namelen: Integer;
                                      host: PWideChar; hostlen: DWORD;
                                      serv: PWideChar; servlen: DWORD;
                                      flags: Integer): Integer; stdcall;
var
   FWSAStartup            : TWSAStartup = nil;
   FWSACleanup            : TWSACleanup = nil;
   FWSASetLastError       : TWSASetLastError = nil;
   FWSAGetLastError       : TWSAGetLastError = nil;
   FWSACancelAsyncRequest : TWSACancelAsyncRequest = nil;
   FWSAAsyncGetHostByName : TWSAAsyncGetHostByName = nil;
   FWSAAsyncGetHostByAddr : TWSAAsyncGetHostByAddr = nil;
   FWSAAsyncSelect        : TWSAAsyncSelect = nil;
   FGetServByName         : TGetServByName = nil;
   FGetProtoByName        : TGetProtoByName = nil;
   FGetHostByName         : TGetHostByName = nil;
   FGetHostByAddr         : TGetHostByAddr = nil;
   FGetHostName           : TGetHostName = nil;
   FOpenSocket            : TOpenSocket = nil;
   FShutdown              : TShutdown = nil;
   FSetSockOpt            : TSetSockOpt = nil;
   FGetSockOpt            : TGetSockOpt = nil;
   FSendTo                : TSendTo = nil;
   FSend                  : TSend = nil;
   FRecv                  : TRecv = nil;
   FRecvFrom              : TRecvFrom = nil;
   Fntohs                 : Tntohs = nil;
   Fntohl                 : Tntohl = nil;
   FListen                : TListen = nil;
   FIoctlSocket           : TIoctlSocket = nil;
   FWSAIoctl              : TWSAIoctl = nil;
   FInet_ntoa             : TInet_ntoa = nil;
   FInet_addr             : TInet_addr = nil;
   Fhtons                 : Thtons = nil;
   Fhtonl                 : Thtonl = nil;
   FGetSockName           : TGetSockName = nil;
   FGetPeerName           : TGetPeerName = nil;
   FConnect               : TConnect = nil;
   FCloseSocket           : TCloseSocket = nil;
   FBind                  : TBind = nil;
   FAccept                : TAccept = nil;
   FGetAddrInfoA          : TGetAddrInfoA = nil;
   FGetAddrInfoW          : TGetAddrInfoW = nil;
   FFreeAddrInfoA         : TFreeAddrInfoA = nil;
   FFreeAddrInfoW         : TFreeAddrInfoW = nil;
   FGetNameInfoA          : TGetNameInfoA = nil;
   FGetNameInfoW          : TGetNameInfoW = nil;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SocketErrorDesc(ErrCode : Integer) : String;
begin
    case ErrCode of
    0:
      Result := 'No Error';
    WSAEINTR:
      Result := 'Interrupted system call';
    WSAEBADF:
      Result := 'Bad file number';
    WSAEACCES:
      Result := 'Permission denied';
    WSAEFAULT:
      Result := 'Bad address';
    WSAEINVAL:
      Result := 'Invalid argument';
    WSAEMFILE:
      Result := 'Too many open files';
    WSAEWOULDBLOCK:
      Result := 'Operation would block';
    WSAEINPROGRESS:
      Result := 'Operation now in progress';
    WSAEALREADY:
      Result := 'Operation already in progress';
    WSAENOTSOCK:
      Result := 'Socket operation on non-socket';
    WSAEDESTADDRREQ:
      Result := 'Destination address required';
    WSAEMSGSIZE:
      Result := 'Message too long';
    WSAEPROTOTYPE:
      Result := 'Protocol wrong type for socket';
    WSAENOPROTOOPT:
      Result := 'Protocol not available';
    WSAEPROTONOSUPPORT:
      Result := 'Protocol not supported';
    WSAESOCKTNOSUPPORT:
      Result := 'Socket type not supported';
    WSAEOPNOTSUPP:
      Result := 'Operation not supported on socket';
    WSAEPFNOSUPPORT:
      Result := 'Protocol family not supported';
    WSAEAFNOSUPPORT:
      Result := 'Address family not supported by protocol family';
    WSAEADDRINUSE:
      Result := 'Address already in use';
    WSAEADDRNOTAVAIL:
      Result := 'Address not available';
    WSAENETDOWN:
      Result := 'Network is down';
    WSAENETUNREACH:
      Result := 'Network is unreachable';
    WSAENETRESET:
      Result := 'Network dropped connection on reset';
    WSAECONNABORTED:
      Result := 'Connection aborted';
    WSAECONNRESET:
      Result := 'Connection reset by peer';
    WSAENOBUFS:
      Result := 'No buffer space available';
    WSAEISCONN:
      Result := 'Socket is already connected';
    WSAENOTCONN:
      Result := 'Socket is not connected';
    WSAESHUTDOWN:
      Result := 'Can''t send after socket shutdown';
    WSAETOOMANYREFS:
      Result := 'Too many references: can''t splice';
    WSAETIMEDOUT:
      Result := 'Connection timed out';
    WSAECONNREFUSED:
      Result := 'Connection refused';
    WSAELOOP:
      Result := 'Too many levels of symbolic links';
    WSAENAMETOOLONG:
      Result := 'File name too long';
    WSAEHOSTDOWN:
      Result := 'Host is down';
    WSAEHOSTUNREACH:
      Result := 'No route to host';
    WSAENOTEMPTY:
      Result := 'Directory not empty';
    WSAEPROCLIM:
      Result := 'Too many processes';
    WSAEUSERS:
      Result := 'Too many users';
    WSAEDQUOT:
      Result := 'Disc quota exceeded';
    WSAESTALE:
      Result := 'Stale NFS file handle';
    WSAEREMOTE:
      Result := 'Too many levels of remote in path';
    WSASYSNOTREADY:
      Result := 'Network sub-system is unusable';
    WSAVERNOTSUPPORTED:
      Result := 'WinSock DLL cannot support this application';
    WSANOTINITIALISED:
      Result := 'WinSock not initialized';
    WSAHOST_NOT_FOUND:
      Result := 'Host not found';
    WSATRY_AGAIN:
      Result := 'Non-authoritative host not found';
    WSANO_RECOVERY:
      Result := 'Non-recoverable error';
    WSANO_DATA:
      Result := 'No Data';
    WSASERVICE_NOT_FOUND:
      Result := 'Service not found';
    else
      Result := 'Not a WinSock error';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetWinsockErr(ErrCode: Integer): String ;    { V5.26 }
begin
    Result := SocketErrorDesc(ErrCode) + ' (#' + IntToStr(ErrCode) + ')' ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetProc(const ProcName : AnsiString) : Pointer;
var
    LastError : Longint;
begin
    { Prevents compiler warning "Return value might be undefined"  }
    Result := nil;

    EnterCriticalSection(GWSockCritSect);
    try
        if GWsDLLHandle = 0 then begin
            GWsDLLHandle := LoadLibrary(GWsDLLName);
            if GWsDLLHandle = 0 then
                raise Exception.Create('Unable to load ' + GWsDLLName +
                              ' - ' + SysErrorMessage(GetLastError));
            LastError := Ics_WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh), GInitData);
            if LastError <> 0 then
                raise ESocketAPIException.Create('Winsock startup error ' +
                               GWs2DLLName + ' - ' + GetWinsockErr (LastError));
        end;
        if Length(ProcName) = 0 then
            Result := nil
        else begin
            Result := GetProcAddress(GWsDLLHandle, PAnsiChar(ProcName));
            if Result = nil then
                raise ESocketAPIException.Create('Procedure ' + String(ProcName) +
                                              ' not found in ' + GWsDLLName +
                                   ' - ' + SysErrorMessage(GetLastError));
        end;
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetProc2(const ProcName : AnsiString) : Pointer;
begin
    Result := nil;
    EnterCriticalSection(GWSockCritSect);
    try
        if GWs2DLLHandle = 0 then begin
                GetProc('');
            GWs2DLLHandle := LoadLibrary(GWs2DLLName);
            if GWs2DLLHandle = 0 then
                raise Exception.Create('Unable to load ' + GWs2DLLName +
                              ' - ' + SysErrorMessage(GetLastError));
        end;
        if Length(ProcName) = 0 then
            Result := nil
        else begin
            Result := GetProcAddress(GWs2DLLHandle, PAnsiChar(ProcName));
            if Result = nil then
                raise ESocketAPIException.Create('Procedure ' + String(ProcName) +
                                              ' not found in ' + GWs2DLLName +
                                ' - ' + SysErrorMessage(GetLastError));
        end;
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetProc3(const ProcName : AnsiString) : Pointer;
begin
    Result := nil;
    EnterCriticalSection(GWSockCritSect);
    try
        if GWs2IPv6ProcHandle = 0 then begin
                GetProc2('');
            GWs2IPv6ProcHandle := GWs2DLLHandle;
            @FGetAddrInfoA := GetProcAddress(GWs2IPv6ProcHandle,'getaddrinfo');
            if @FGetAddrInfoA = nil then begin
                GWship6DllHandle := LoadLibrary(GWship6DLLname);
                if GWship6DllHandle = 0 then
                    raise Exception.Create('Unable to load ' + GWship6DLLname +
                              ' - ' + SysErrorMessage(GetLastError));
                GWs2IPv6ProcHandle := GWship6DllHandle;
            end;
        end;
        if Length(ProcName) = 0 then
            Result := nil
        else begin
            Result := GetProcAddress(GWs2IPv6ProcHandle, PAnsiChar(ProcName));
            if Result = nil then
                raise ESocketAPIException.Create('Procedure ' + String(ProcName) +
                                                 ' not found in ' + GWs2DLLName +
                                ' - ' + SysErrorMessage(GetLastError));
        end;
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSocketAPILoaded : Boolean;
begin
    EnterCriticalSection(GWSockCritSect);
    try
        Result := GWsDLLHandle <> 0;
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsIPv6APIAvailable: Boolean;
begin
    { Winsock 2 is required }
    Result := ((GReqVerHigh >= 2) and (GReqVerLow >= 2)) and
    (
        { Win XP or better }
        { Actually we should also check for service pack >= 1 but that }
        { required a call to VersionInfoEx().                          }
        (Win32Platform = VER_PLATFORM_WIN32_NT) and
        ((Win32MajorVersion > 5) or
         ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1)))
    );
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check whether IPv6 is available on the system, requires to load socket API }
{ once, subsequent calls return a cached value.                               }
function IsIPv6Available: Boolean;
var
    s : TSocket;
begin
    if GIPv6Available > -1 then
        Result := (GIPv6Available = 1)
    else begin
        EnterCriticalSection(GWSockCritSect);
        try
            s := Ics_socket(AF_INET6, SOCK_DGRAM, IPPROTO_UDP);
            Result := s <> INVALID_SOCKET;
            if Result then begin
                Ics_closesocket(s);
                GIPv6Available := 1;
            end
            else
                GIPv6Available := 0;
            { If no socket created, then unload winsock immediately }
            if WSocketGCount <= 0 then
                UnloadWinsock;
        finally
            LeaveCriticalSection(GWSockCritSect);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Winsock is dynamically loaded and unloaded when needed. In some cases     }
{ you may find winsock being loaded and unloaded very often in your app     }
{ This happend for example when you dynamically create a TWSocket and       }
{ destroy a TWSocket when there is no "permanant" TWSocket (that is a       }
{ TWSocket dropped on a persitant form). It is the very inefficiant.        }
{ Calling WSocketForceLoadWinsock will increament the reference count so    }
{ that winsock will not be unloaded when the last TWSocket is destroyed.    }
procedure ForceLoadWinsock;
begin
    EnterCriticalSection(GWSockCritSect);
    try
        if not WSocketGForced then begin
            WSocketGForced := TRUE;
            Inc(WSocketGCount);
            GetProc('');
        end;
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Cancel the operation done with WSocketForceLoadWinsock.                   }
procedure CancelForceLoadWinsock;
begin
    EnterCriticalSection(GWSockCritSect);
    try
        if WSocketGForced then begin
            WSocketGForced := FALSE;
            Dec(WSocketGCount);
            if WSocketGCount <= 0 then
                UnloadWinsock;
        end;
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure UnloadWinsock;
begin
    EnterCriticalSection(GWSockCritSect);
    try
        if (GWsDLLHandle <> 0) and (WSocketGCount = 0) then begin
            Ics_WSACleanup;
            if GWs2DLLHandle <> 0 then begin
                FreeLibrary(GWs2DLLHandle);
                GWs2DLLHandle      := 0;
                GWs2IPv6ProcHandle := 0;
                FWSAIoctl          := nil;
                if GWship6DllHandle <> 0 then begin
                    FreeLibrary(GWship6DllHandle);
                    GWship6DllHandle := 0;
                end;
                FGetAddrInfoA      := nil;
                FGetAddrInfoW      := nil;
                FFreeAddrInfoA     := nil;
                FFreeAddrInfoW     := nil;
                FGetNameInfoA      := nil;
                FGetNameInfoW      := nil;
            end;
            FreeLibrary(GWsDLLHandle);
            GWsDLLHandle           := 0;
            FWSAStartup            := nil;
            FWSACleanup            := nil;
            FWSASetLastError       := nil;
            FWSAGetLastError       := nil;
            FWSACancelAsyncRequest := nil;
            FWSAAsyncGetHostByName := nil;
            FWSAAsyncGetHostByAddr := nil;
            FWSAAsyncSelect        := nil;
            FGetServByName         := nil;
            FGetProtoByName        := nil;
            FGetHostByName         := nil;
            FGetHostByAddr         := nil;
            FGetHostName           := nil;
            FOpenSocket            := nil;
            FShutdown              := nil;
            FSetSockOpt            := nil;
            FGetSockOpt            := nil;
            FSendTo                := nil;
            FSend                  := nil;
            FRecv                  := nil;
            FRecvFrom              := nil;
            Fntohs                 := nil;
            Fntohl                 := nil;
            FListen                := nil;
            FIoctlSocket           := nil;
            FWSAIoctl              := nil;
            FInet_ntoa             := nil;
            FInet_addr             := nil;
            Fhtons                 := nil;
            Fhtonl                 := nil;
            FGetSockName           := nil;
            FGetPeerName           := nil;
            FConnect               := nil;
            FCloseSocket           := nil;
            FBind                  := nil;
            FAccept                := nil;
        end;
        WSocketGForced := FALSE;
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WinsockAPIInfo : TWSADATA;
begin
    { Load winsock and initialize it as needed }
    EnterCriticalSection(GWSockCritSect);
    try
        GetProc('');
        Result := GInitData;
        { If no socket created, then unload winsock immediately }
        if WSocketGCount <= 0 then
            UnloadWinsock;
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSAStartup(
    wVersionRequested: WORD;
    var lpWSAData: TWSAData): Integer; stdcall;
begin
    Result := Ics_WSAStartup(wVersionRequested, lpWSAData);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSACleanup : Integer; stdcall;
begin
    Result := Ics_WSACleanup;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSASetLastError(iError: Integer); stdcall;
begin
    Ics_WSASetLastError(iError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSAGetLastError: Integer; stdcall;
begin
    Result := Ics_WSAGetLastError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer; stdcall;
begin
    Result := Ics_WSACancelAsyncRequest(hAsyncTaskHandle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSAAsyncGetHostByName(
    HWindow: HWND; wMsg: u_int;
    name, buf: PAnsiChar;
    buflen: Integer): THandle; stdcall;
begin
    Result := Ics_WSAAsyncGetHostByName(HWindow, wMsg, name, buf, buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSAAsyncGetHostByAddr(
    HWindow: HWND;
    wMsg: u_int; addr: PAnsiChar;
    len, Struct: Integer;
    buf: PAnsiChar;
    buflen: Integer): THandle; stdcall;
begin
    Result := Ics_WSAAsyncGetHostByAddr(HWindow, wMsg, addr, len, struct, buf, buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSAAsyncSelect(
    s: TSocket;
    HWindow: HWND;
    wMsg: u_int;
    lEvent: Longint): Integer; stdcall;
begin
    Result := Ics_WSAAsyncSelect(s, HWindow, wMsg, lEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function getservbyname(name, proto: PAnsiChar): PServEnt; stdcall;
begin
    Result := Ics_getservbyname(name, proto);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function getprotobyname(name: PAnsiChar): PProtoEnt; stdcall;
begin
    Result := Ics_getprotobyname(PAnsiChar(Name));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function gethostbyname(name: PAnsiChar): PHostEnt; stdcall;
begin
    Result := Ics_gethostbyname(name);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt; stdcall;
begin
    Result := Ics_gethostbyaddr(addr, len, Struct);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function gethostname(name: PAnsiChar; len: Integer): Integer; stdcall;
begin
    Result := Ics_gethostname(name, len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function socket(af, Struct, protocol: Integer): TSocket; stdcall;
begin
    Result := Ics_socket(af, Struct, protocol);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function shutdown(s: TSocket; how: Integer): Integer; stdcall;
begin
    Result := Ics_shutdown(s, how);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function setsockopt(s: TSocket; level, optname: Integer;
  optval: PAnsiChar; optlen: Integer): Integer; stdcall;
begin
    Result := Ics_setsockopt(s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function getsockopt(
    s: TSocket; level, optname: Integer;
    optval: PAnsiChar; var optlen: Integer): Integer; stdcall;
begin
    Result := Ics_getsockopt(s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function sendto(
    s          : TSocket;
    var Buf;
    len, flags : Integer;
    var addrto : TSockAddr;
    tolen      : Integer): Integer; stdcall;
begin
    Result := Ics_sendto(s, Buf, len, flags, addrto, tolen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function send(s: TSocket; var Buf;
  len, flags: Integer): Integer; stdcall;
begin
    Result := Ics_send(s, Buf, len, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ntohs(netshort: u_short): u_short; stdcall;
begin
    Result := Ics_ntohs(netshort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ntohl(netlong: u_long): u_long; stdcall;
begin
    Result := Ics_ntohl(netlong);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function listen(s: TSocket; backlog: Integer): Integer; stdcall;
begin
    Result := Ics_listen(s, backlog);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer; stdcall;
begin
    Result := Ics_ioctlsocket(s, cmd, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSAIoctl(
    s                 : TSocket; IoControlCode : DWORD;
    InBuffer          : Pointer; InBufferSize  : DWORD;
    OutBuffer         : Pointer; OutBufferSize : DWORD;
    var BytesReturned : DWORD; Overlapped      : POverlapped;
    CompletionRoutine : FARPROC): Integer; stdcall;
begin
    Result := Ics_WSAIoctl(s, IoControlCode, InBuffer, InBufferSize, OutBuffer,
                   OutBufferSize, BytesReturned, Overlapped, CompletionRoutine);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function inet_ntoa(inaddr: TInAddr): PAnsiChar; stdcall;
begin
    Result := Ics_inet_ntoa(inaddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function inet_addr(cp: PAnsiChar): u_long; stdcall;
begin
    Result := Ics_inet_addr(cp);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function htons(hostshort: u_short): u_short; stdcall;
begin
    Result := Ics_htons(hostshort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function htonl(hostlong: u_long): u_long; stdcall;
begin
    Result := Ics_htonl(hostlong);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function getsockname(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer; stdcall;
begin
    Result := Ics_getsockname(s, name, namelen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function getpeername(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer; stdcall;
begin
    Result := Ics_getpeername(s, name, namelen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function connect(
    s        : TSocket;
    var name : TSockAddr;
    namelen  : Integer): Integer; stdcall;
begin
    Result := Ics_connect(s, name, namelen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function closesocket(s: TSocket): Integer; stdcall;
begin
    Result := Ics_closesocket(s);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function bind(
    s: TSocket;
    var addr: TSockAddr;
    namelen: Integer): Integer; stdcall;
begin
    Result := Ics_bind(s, addr, namelen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function accept(
    s: TSocket;
    addr: PSockAddr;
    addrlen: PInteger): TSocket; stdcall;
begin
    Result := Ics_accept(s, addr, addrlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function recv(s: TSocket; var Buf;
  len, flags: Integer): Integer; stdcall;
begin
    Result := Ics_recv(s, Buf, len, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function recvfrom(
    s: TSocket;
    var Buf; len, flags: Integer;
    var from: TSockAddr;
    var fromlen: Integer): Integer; stdcall;
begin
    Result := Ics_recvfrom(s, Buf, len, flags, from, fromlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetAddrInfoA(
    NodeName    : PAnsiChar;
    ServName    : PAnsiChar;
    Hints       : PADDRINFOA;
    var Addrinfo: PADDRINFOA): Integer; stdcall;
begin
    Result := Ics_GetAddrInfoA(NodeName, ServName, Hints, Addrinfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetAddrInfoW(
    NodeName    : PWideChar;
    ServName    : PWideChar;
    Hints       : PADDRINFOW;
    var Addrinfo: PADDRINFOW): Integer; stdcall;
begin
    Result := Ics_GetAddrInfoW(NodeName, ServName, Hints, Addrinfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetAddrInfo(
    NodeName    : PChar;
    ServName    : PChar;
    Hints       : PAddrInfo;
    var Addrinfo: PAddrInfo): Integer; stdcall;
begin
    Result := Ics_GetAddrInfo(NodeName, ServName, Hints, Addrinfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure FreeAddrInfoA(ai: PADDRINFOA); stdcall;
begin
    Ics_FreeAddrInfoA(ai);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure FreeAddrInfoW(ai: PADDRINFOW); stdcall;
begin
    Ics_FreeAddrInfoW(ai);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure FreeAddrInfo(ai: PAddrInfo); stdcall;
begin
    Ics_FreeAddrInfo(ai);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetNameInfoA(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PAnsiChar;
    hostlen : LongWord;
    serv    : PAnsiChar;
    servlen : LongWord;
    flags   : Integer): Integer; stdcall;
begin
    Result := Ics_GetNameInfoA(addr, namelen, host, hostlen, serv, servlen, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetNameInfoW(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PWideChar;
    hostlen : LongWord;
    serv    : PWideChar;
    servlen : LongWord;
    flags   : Integer): Integer; stdcall;
begin
    Result := Ics_GetNameInfoW(addr, namelen, host, hostlen, serv, servlen, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetNameInfo(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PChar;
    hostlen : LongWord;
    serv    : PChar;
    servlen : LongWord;
    flags   : Integer): Integer; stdcall;
begin
    Result := Ics_GetNameInfo(addr, namelen, host, hostlen, serv, servlen, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_WSAStartup(
    wVersionRequested: WORD;
    var lpWSAData: TWSAData): Integer;
begin
    if @FWSAStartup = nil then
        @FWSAStartup := GetProc('WSAStartup');
    Result := FWSAStartup(wVersionRequested, lpWSAData);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_WSACleanup : Integer;
begin
    if @FWSACleanup = nil then
        @FWSACleanup := GetProc('WSACleanup');
    Result := FWSACleanup;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Ics_WSASetLastError(iError: Integer);
begin
    if @FWSASetLastError = nil then
        @FWSASetLastError := GetProc('WSASetLastError');
    FWSASetLastError(iError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_WSAGetLastError: Integer;
begin
    if @FWSAGetLastError = nil then
        @FWSAGetLastError := GetProc('WSAGetLastError');
    Result := FWSAGetLastError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
begin
    if @FWSACancelAsyncRequest = nil then
        @FWSACancelAsyncRequest := GetProc('WSACancelAsyncRequest');
    Result := FWSACancelAsyncRequest(hAsyncTaskHandle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_WSAAsyncGetHostByName(
    HWindow: HWND; wMsg: u_int;
    name, buf: PAnsiChar;
    buflen: Integer): THandle;
begin
    if @FWSAAsyncGetHostByName = nil then
        @FWSAAsyncGetHostByName := GetProc('WSAAsyncGetHostByName');
    Result := FWSAAsyncGetHostByName(HWindow, wMsg, name, buf, buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_WSAAsyncGetHostByAddr(
    HWindow: HWND;
    wMsg: u_int; addr: PAnsiChar;
    len, Struct: Integer;
    buf: PAnsiChar;
    buflen: Integer): THandle;
begin
    if @FWSAAsyncGetHostByAddr = nil then
        @FWSAAsyncGetHostByAddr := GetProc('WSAAsyncGetHostByAddr');
    Result := FWSAAsyncGetHostByAddr(HWindow, wMsg, addr, len, struct, buf, buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_WSAAsyncSelect(
    s: TSocket;
    HWindow: HWND;
    wMsg: u_int;
    lEvent: Longint): Integer;
begin
    if @FWSAAsyncSelect = nil then
        @FWSAAsyncSelect := GetProc('WSAAsyncSelect');
    Result := FWSAAsyncSelect(s, HWindow, wMsg, lEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_getservbyname(name, proto: PAnsiChar): PServEnt;
begin
    if @Fgetservbyname = nil then
        @Fgetservbyname := GetProc('getservbyname');
    Result := Fgetservbyname(name, proto);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_getprotobyname(name: PAnsiChar): PProtoEnt;
begin
    if @Fgetprotobyname = nil then
        @Fgetprotobyname := GetProc('getprotobyname');
    Result := Fgetprotobyname(PAnsiChar(Name));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_gethostbyname(name: PAnsiChar): PHostEnt;
begin
    if @Fgethostbyname = nil then
        @Fgethostbyname := GetProc('gethostbyname');
    Result := Fgethostbyname(name);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt;
begin
    if @Fgethostbyaddr = nil then
        @Fgethostbyaddr := GetProc('gethostbyaddr');
    Result := Fgethostbyaddr(addr, len, Struct);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_gethostname(name: PAnsiChar; len: Integer): Integer;
begin
    if @Fgethostname = nil then
        @Fgethostname := GetProc('gethostname');
    Result := Fgethostname(name, len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_socket(af, Struct, protocol: Integer): TSocket;
begin
    if @FOpenSocket= nil then
        @FOpenSocket := GetProc('socket');
    Result := FOpenSocket(af, Struct, protocol);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_shutdown(s: TSocket; how: Integer): Integer;
begin
    if @FShutdown = nil then
        @FShutdown := GetProc('shutdown');
    Result := FShutdown(s, how);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_setsockopt(s: TSocket; level, optname: Integer;
  optval: PAnsiChar; optlen: Integer): Integer;
begin
    if @FSetSockOpt = nil then
        @FSetSockOpt := GetProc('setsockopt');
    Result := FSetSockOpt(s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_getsockopt(
    s: TSocket; level, optname: Integer;
    optval: PAnsiChar; var optlen: Integer): Integer;
begin
    if @FGetSockOpt = nil then
        @FGetSockOpt := GetProc('getsockopt');
    Result := FGetSockOpt(s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_sendto(
    s          : TSocket;
    var Buf;
    len, flags : Integer;
    var addrto : TSockAddr;
    tolen      : Integer): Integer;
begin
    if @FSendTo = nil then
        @FSendTo := GetProc('sendto');
    Result := FSendTo(s, Buf, len, flags, addrto, tolen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_send(s: TSocket; var Buf;
  len, flags: Integer): Integer;
begin
    if @FSend = nil then
        @FSend := GetProc('send');
    Result := FSend(s, Buf, len, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_ntohs(netshort: u_short): u_short;
begin
    if @Fntohs = nil then
        @Fntohs := GetProc('ntohs');
    Result := Fntohs(netshort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_ntohl(netlong: u_long): u_long;
begin
    if @Fntohl = nil then
        @Fntohl := GetProc('ntohl');
    Result := Fntohl(netlong);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_listen(s: TSocket; backlog: Integer): Integer;
begin
    if @FListen = nil then
        @FListen := GetProc('listen');
    Result := FListen(s, backlog);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
begin
    if @FIoctlSocket = nil then
        @FIoctlSocket := GetProc('ioctlsocket');
    Result := FIoctlSocket(s, cmd, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_WSAIoctl(
    s                 : TSocket; IoControlCode : DWORD;
    InBuffer          : Pointer; InBufferSize  : DWORD;
    OutBuffer         : Pointer; OutBufferSize : DWORD;
    var BytesReturned : DWORD; Overlapped      : POverlapped;
    CompletionRoutine : FARPROC): Integer;
begin
    if @FWSAIoctl = nil then
        @FWSAIoctl := GetProc2('WSAIoctl');
    Result := FWSAIoctl(s, IoControlCode, InBuffer, InBufferSize, OutBuffer,
                        OutBufferSize, BytesReturned, Overlapped, CompletionRoutine);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_inet_ntoa(inaddr: TInAddr): PAnsiChar;
begin
    if @FInet_ntoa = nil then
        @FInet_ntoa := GetProc('inet_ntoa');
    Result := FInet_ntoa(inaddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_inet_addr(cp: PAnsiChar): u_long;
begin
    if @FInet_addr = nil then
        @FInet_addr := GetProc('inet_addr');
    Result := FInet_addr(cp);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_htons(hostshort: u_short): u_short;
begin
    if @Fhtons = nil then
        @Fhtons := GetProc('htons');
    Result := Fhtons(hostshort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_htonl(hostlong: u_long): u_long;
begin
    if @Fhtonl = nil then
        @Fhtonl := GetProc('htonl');
    Result := Fhtonl(hostlong);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_getsockname(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer;
begin
    if @FGetSockName = nil then
        @FGetSockName := GetProc('getsockname');
    Result := FGetSockName(s, name, namelen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_getpeername(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer;
begin
    if @FGetPeerName = nil then
        @FGetPeerName := GetProc('getpeername');
    Result := FGetPeerName(s, name, namelen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_connect(
    s        : TSocket;
    var name : TSockAddr;
    namelen  : Integer): Integer;
begin
    if @FConnect= nil then
        @FConnect := GetProc('connect');
    Result := FConnect(s, name, namelen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_closesocket(s: TSocket): Integer;
begin
    if @FCloseSocket = nil then
        @FCloseSocket := GetProc('closesocket');
    Result := FCloseSocket(s);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_bind(
    s: TSocket;
    var addr: TSockAddr;
    namelen: Integer): Integer;
begin
    if @FBind = nil then
        @FBind := GetProc('bind');
    Result := FBind(s, addr, namelen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_accept(
    s: TSocket;
    addr: PSockAddr;
    addrlen: PInteger): TSocket;
begin
    if @FAccept = nil then
        @FAccept := GetProc('accept');
    Result := FAccept(s, addr, addrlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_recv(s: TSocket; var Buf;
  len, flags: Integer): Integer;
begin
    if @FRecv= nil then
        @FRecv := GetProc('recv');
    Result := FRecv(s, Buf, len, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_recvfrom(
    s: TSocket;
    var Buf; len, flags: Integer;
    var from: TSockAddr;
    var fromlen: Integer): Integer;
begin
    if @FRecvFrom = nil then
        @FRecvFrom := GetProc('recvfrom');
    Result := FRecvFrom(s, Buf, len, flags, from, fromlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_GetAddrInfoA(
    NodeName    : PAnsiChar;
    ServName    : PAnsiChar;
    Hints       : PADDRINFOA;
    var Addrinfo: PADDRINFOA): Integer;
begin
    if @FGetAddrInfoA = nil then
        @FGetAddrInfoA := GetProc3('getaddrinfo');
    Result := FGetAddrInfoA(NodeName, ServName, Hints, Addrinfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_GetAddrInfoW(
    NodeName    : PWideChar;
    ServName    : PWideChar;
    Hints       : PADDRINFOW;
    var Addrinfo: PADDRINFOW): Integer;
begin
    if @FGetAddrInfoW = nil then
        @FGetAddrInfoW := GetProc3('GetAddrInfoW');
    Result := FGetAddrInfoW(NodeName, ServName, Hints, Addrinfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_GetAddrInfo(
    NodeName    : PChar;
    ServName    : PChar;
    Hints       : PAddrInfo;
    var Addrinfo: PAddrInfo): Integer;
begin
{$IFDEF UNICODE}
    if @FGetAddrInfoW = nil then
        @FGetAddrInfoW := GetProc3('GetAddrInfoW');
    Result := FGetAddrInfoW(NodeName, ServName, Hints, Addrinfo);
{$ELSE}
    if @FGetAddrInfoA = nil then
        @FGetAddrInfoA := GetProc3('getaddrinfo');
    Result := FGetAddrInfoA(NodeName, ServName, Hints, Addrinfo);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Ics_FreeAddrInfoA(ai: PADDRINFOA);
begin
    if @FFreeAddrInfoA = nil then
        @FFreeAddrInfoA := GetProc3('freeaddrinfo');
    FFreeAddrInfoA(ai);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Ics_FreeAddrInfoW(ai: PADDRINFOW);
begin
    if @FFreeAddrInfoW = nil then
        @FFreeAddrInfoW := GetProc3('FreeAddrInfoW');
    FFreeAddrInfoW(ai);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Ics_FreeAddrInfo(ai: PAddrInfo);
begin
{$IFDEF UNICODE}
    if @FFreeAddrInfoW = nil then
        @FFreeAddrInfoW := GetProc3('FreeAddrInfoW');
    FFreeAddrInfoW(ai);
{$ELSE}
    if @FFreeAddrInfoA = nil then
        @FFreeAddrInfoA := GetProc3('freeaddrinfo');
    FFreeAddrInfoA(ai);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_GetNameInfoA(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PAnsiChar;
    hostlen : LongWord;
    serv    : PAnsiChar;
    servlen : LongWord;
    flags   : Integer): Integer;
begin
    if @FGetNameInfoA = nil then
        @FGetNameInfoA := GetProc3('getnameinfo');
    Result := FGetNameInfoA(addr, namelen, host, hostlen, serv, servlen, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_GetNameInfoW(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PWideChar;
    hostlen : LongWord;
    serv    : PWideChar;
    servlen : LongWord;
    flags   : Integer): Integer;
begin
    if @FGetNameInfoW = nil then
        @FGetNameInfoW := GetProc3('GetNameInfoW');
    Result := FGetNameInfoW(addr, namelen, host, hostlen, serv, servlen, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_GetNameInfo(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PChar;
    hostlen : LongWord;
    serv    : PChar;
    servlen : LongWord;
    flags   : Integer): Integer;
begin
{$IFDEF UNICODE}
    if @FGetNameInfoW = nil then
        @FGetNameInfoW := GetProc3('GetNameInfoW');
    Result := FGetNameInfoW(addr, namelen, host, hostlen, serv, servlen, flags);
{$ELSE}
    if @FGetNameInfoA = nil then
        @FGetNameInfoA := GetProc3('getnameinfo');
    Result := FGetNameInfoA(addr, namelen, host, hostlen, serv, servlen, flags);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Custom macro helpers }

{const
  NZoneMask   = $0FFFFFFF;
  NLevelMask  = $F0000000;
  NLevelShift = 28;}

function ScopeIdGetLevel(const AScopeId: ULONG): ULONG;
begin
    Result := (AScopeId and $F0000000) shr 28;
end;

function ScopeIdGetZone(const AScopeId: ULONG): ULONG;
begin
    Result := AScopeId and $0FFFFFFF;
end;

procedure ScopeIdSetLevel(var AScopeId: ULONG; const ALevel: ULONG);
begin
    AScopeId := (AScopeId and $0FFFFFFF) or ((ALevel shl 28) and $F0000000);
end;

procedure ScopeIdSetZone(var AScopeId: ULONG; const AZone: ULONG);
begin
    AScopeId := (AZone and $0FFFFFFF) or (AScopeId and $F0000000);
end;

function MakeScopeId(const AZone: ULONG; const ALevel: ULONG): ULONG;
begin
    ScopeIdSetZone(Result, AZone);
    ScopeIdSetLevel(Result, ALevel);
end;

{ Macros }

function IN6ADDR_ANY_INIT: TIn6Addr;
begin
    with Result do
        FillChar(s6_addr, SizeOf(TIn6Addr), 0);
end;

function IN6ADDR_LOOPBACK_INIT: TIn6Addr;
begin
    with Result do
    begin
        FillChar(s6_addr, SizeOf(TIn6Addr), 0);
        s6_addr[15] := $01;
    end;
end;

procedure IN6ADDR_SETANY(sa: PSockAddrIn6);
begin
    if sa <> nil then
        with sa^ do
        begin
            sin6_family := AF_INET6;
            sin6_port := 0;
            sin6_flowinfo := 0;
            PULONG(@sin6_addr.s6_addr[0])^  := 0;
            PULONG(@sin6_addr.s6_addr[4])^  := 0;
            PULONG(@sin6_addr.s6_addr[8])^  := 0;
            PULONG(@sin6_addr.s6_addr[12])^ := 0;
        end;
end;

procedure IN6ADDR_SETLOOPBACK(sa: PSockAddrIn6);
begin
    if sa <> nil then begin
        with sa^ do begin
          sin6_family := AF_INET6;
          sin6_port := 0;
          sin6_flowinfo := 0;
          PULONG(@sin6_addr.s6_addr[0])^ := 0;
          PULONG(@sin6_addr.s6_addr[4])^ := 0;
          PULONG(@sin6_addr.s6_addr[8])^ := 0;
          PULONG(@sin6_addr.s6_addr[12])^ := 1;
        end;
    end;
end;

function IN6ADDR_ISANY(sa: PSockAddrIn6): Boolean;
begin
    if sa <> nil then begin
        with sa^ do begin
            Result := (sin6_family = AF_INET6) and
                      (PULONG(@sin6_addr.s6_addr[0])^ = 0) and
                      (PULONG(@sin6_addr.s6_addr[4])^ = 0) and
                      (PULONG(@sin6_addr.s6_addr[8])^ = 0) and
                      (PULONG(@sin6_addr.s6_addr[12])^ = 0);
        end;
    end
    else
      Result := False;
end;

function IN6ADDR_ISLOOPBACK(sa: PSockAddrIn6): Boolean;
begin
    if sa <> nil then begin
        with sa^ do begin
            Result := (sin6_family = AF_INET6) and
                      (PULONG(@sin6_addr.s6_addr[0])^ = 0) and
                      (PULONG(@sin6_addr.s6_addr[4])^ = 0) and
                      (PULONG(@sin6_addr.s6_addr[8])^ = 0) and
                      (PULONG(@sin6_addr.s6_addr[12])^ = 1);
        end;
    end
    else
      Result := False;
end;

function IN6_ADDR_EQUAL(const a: PIn6Addr; const b: PIn6Addr): Boolean;
begin
    Result := CompareMem(a, b, SizeOf(TIn6Addr));
end;

function IN6_IS_ADDR_UNSPECIFIED(const a: PIn6Addr): Boolean;
begin
    Result := IN6_ADDR_EQUAL(a, @in6addr_any);
end;

function IN6_IS_ADDR_LOOPBACK(const a: PIn6Addr): Boolean;
begin
    Result := IN6_ADDR_EQUAL(a, @in6addr_loopback);
end;

function IN6_IS_ADDR_MULTICAST(const a: PIn6Addr): Boolean;
begin
    if a <> nil then
        Result := (a^.s6_addr[0] = $FF)
    else
        Result := False;
end;

function IN6_IS_ADDR_LINKLOCAL(const a: PIn6Addr): Boolean;
begin
    if a <> nil then
        Result := (a^.s6_addr[0] = $FE) and ((a^.s6_addr[1] and $C0) = $80)
    else
        Result := False;
end;

function IN6_IS_ADDR_SITELOCAL(const a: PIn6Addr): Boolean;
begin
    if a <> nil then
        Result := (a^.s6_addr[0] = $FE) and ((a^.s6_addr[1] and $C0) = $C0)
    else
        Result := False;
end;

{$IFDEF STILL_NEEDS_CHECK}
function IN6_IS_ADDR_GLOBAL(const a: PIn6Addr): Boolean;
var
    LHigh : ULONG;
begin
    //
    // Check the format prefix and exclude addresses
    // whose high 4 bits are all zero or all one.
    // This is a cheap way of excluding v4-compatible,
    // v4-mapped, loopback, multicast, link-local, site-local.
    //
    if a <> nil then
    begin
        LHigh := (a^.s6_bytes[0] and $f0);
        Result := (LHigh <> 0) and (LHigh <> $f0);
    end    
    else
        Result := False;
end;
{$ENDIF}

function IN6_IS_ADDR_V4MAPPED(const a: PIn6Addr): Boolean;
begin
    if a <> nil then begin
        with a^ do begin
            Result := (Word[0] = 0) and
                      (Word[1] = 0) and
                      (Word[2] = 0) and
                      (Word[3] = 0) and
                      (Word[4] = 0) and
                      (Word[5] = $FFFF);
        end;
    end
    else
        Result := False;
end;

function IN6_IS_ADDR_V4COMPAT(const a: PIn6Addr): Boolean;
begin
    if a <> nil then begin
        with a^ do begin
          Result := (Word[0] = 0) and
                    (Word[1] = 0) and
                    (Word[2] = 0) and
                    (Word[3] = 0) and
                    (Word[4] = 0) and
                    (Word[5] = 0) and
                    not ((Word[6] = 0) and (s6_addr[14] = 0) and
                    ((s6_addr[15] = 0) or (s6_addr[15] = 1)));
        end;
    end
    else
        Result := False;
end;

function IN6_IS_ADDR_MC_NODELOCAL(const a: PIn6Addr): Boolean;
begin
    if a <> nil then
        Result := IN6_IS_ADDR_MULTICAST(a) and ((a^.s6_addr[1] and $F) = 1)
    else
        Result := False;
end;

function IN6_IS_ADDR_MC_LINKLOCAL(const a: PIn6Addr): Boolean;
begin
    if a <> nil then
        Result := IN6_IS_ADDR_MULTICAST(a) and ((a^.s6_addr[1] and $F) = 2)
    else
        Result := False;
end;

function IN6_IS_ADDR_MC_SITELOCAL(const a: PIn6Addr): Boolean;
begin
    if a <> nil then
        Result := IN6_IS_ADDR_MULTICAST(a) and ((a^.s6_addr[1] and $F) = 5)
    else
        Result := False;
end;

function IN6_IS_ADDR_MC_ORGLOCAL(const a: PIn6Addr): Boolean;
begin
    if a <> nil then
        Result := IN6_IS_ADDR_MULTICAST(a) and ((a^.s6_addr[1] and $F) = 8)
    else
        Result := False;
end;

function IN6_IS_ADDR_MC_GLOBAL(const a: PIn6Addr): Boolean;
begin
    if a <> nil then
        Result := IN6_IS_ADDR_MULTICAST(a) and ((a^.s6_addr[1] and $F) = $E)
    else
        Result := False;
end;


{ Microsoft-specific IPv4 definitions. }
{$IFDEF STILL_NEEDS_CHECK}
function IN4_CLASSA(a: u_long): Boolean;
begin
    Result := a and $00000080 = 0;
end;

function IN4_CLASSB(a: u_long): Boolean;
begin
    Result := a and $000000c0 = $00000080;
end;

function IN4_CLASSC(a: u_long): Boolean;
begin
    Result := a and $000000e0 = $000000c0;
end;

function IN4_CLASSD(a: u_long): Boolean;
begin
    Result := a and $000000f0 = $000000e0;
end;

function IN4_MULTICAST(a: u_long): Boolean;
begin
    Result := IN4_CLASSD(a);
end;

function IN4_IS_ADDR_BROADCAST(const a: PInAddr): Boolean;
begin
    if a <> nil then
        Result := DWORD(a^.s_addr) = IN4ADDR_BROADCAST
    else
        Result := False;
end;

function IN4_IS_ADDR_MULTICAST(const a: PInAddr): Boolean;
begin
    if a <> nil then
        Result := IN4_MULTICAST(a^.s_addr)
    else
        Result := False;
end;

function IN4_IS_ADDR_MC_LINKLOCAL(const a: PInAddr): Boolean;
begin
    if a <> nil then
        Result := a^.s_addr and $ffffff = $e0 // 224.0.0/24
    else
        Result := False;
end;

function IN4_IS_ADDR_MC_ADMINLOCAL(const a: PInAddr): Boolean;
begin
    if a <> nil then
        Result := (a^.s_addr and $ffff) = $ffef // 239.255/16
    else
        Result := False;
end;

function IN4_IS_ADDR_MC_SITELOCAL(const a: PInAddr): Boolean;
begin
    if a <> nil then
        Result := ((a^.s_addr and $ff) = $ef) and
                    (not IN4_IS_ADDR_MC_ADMINLOCAL(a))
    else
        Result := False;
end;

function IN4_IS_ADDR_LINKLOCAL(const a: PInAddr): Boolean;
begin
    if a <> nil then
        Result := a^.s_addr and $ffff = $fea9 // 169.254/16
    else
        Result := False;
end;

function IN4_IS_ADDR_LOOPBACK(const a: PInAddr): Boolean;
begin
    if a <> nil then
        Result := PByte(a)^ = $7f // 127/8
    else
        Result := False;
end;

function Ipv4UnicastAddressScope(const Address: PAnsiChar): TScopeLevel;
(*

Routine Description:

    Determines the scope of an IPv4 unicast address.

    For existing scenarios (e.g. ICS) to work as expected, RFC-1918 prefixes
    are deemed to be global scoped.  When appropriate, site border routers
    must explicitly filter packets with these addresses.

Arguments:

    Address - Supplies the IPv4 unicast address.

Return Value:

    Returns the scope level of the address.

Caller IRQL:

    May be called at PASSIVE through DISPATCH level.

*)
begin
    {
    IN_ADDR Ipv4Address;

    if (!INET_IS_ALIGNED(Address, IN_ADDR)) {
        Ipv4Address = *(CONST IN_ADDR UNALIGNED *)Address;
        Address = (CONST UCHAR *) &Ipv4Address;
    }
    if IN4_IS_ADDR_LINKLOCAL(PInAddr(Address)) or
        IN4_IS_ADDR_LOOPBACK(PInAddr(Address)) then
        Result := ScopeLevelLink
    else
        Result := ScopeLevelGlobal;
end;


function Ipv4MulticastAddressScope(const Address: PAnsiChar): TScopeLevel;

(*

Routine Description:

    Determines the scope of an IPv4 multicast address.
    See RFC 2365.

Arguments:

    Address - Supplies the IPv4 multicast address.

Return Value:

    Returns the scope level of the multicast address.

Caller IRQL:

    May be called at PASSIVE through DISPATCH level.

*)
begin
    {
    if (!INET_IS_ALIGNED(Address, IN_ADDR)) begin
        Ipv4Address = *(CONST IN_ADDR UNALIGNED *)Address;
        Address = (CONST UCHAR *) &Ipv4Address;
    end;
    }
    if IN4_IS_ADDR_MC_LINKLOCAL(PInAddr(Address)) then
        Result := ScopeLevelLink
    else if IN4_IS_ADDR_MC_ADMINLOCAL(PInAddr(Address)) then
        Result := ScopeLevelAdmin
    else if IN4_IS_ADDR_MC_SITELOCAL(PInAddr(Address)) then
        Result := ScopeLevelSite
    else
        Result := ScopeLevelGlobal;
end;


function Ipv4AddressScope(const Address: PAnsiChar): TScopeLevel;
(*
Routine Description:

    Examines an IPv4 address and determines its scope.

Arguments:

    Address - Supplies the address to test.

Return Value:

    Returns the scope level of the address.

Caller IRQL:

    May be called at PASSIVE through DISPATCH level.

*)

begin
    if IN4_IS_ADDR_BROADCAST(PInAddr(Address)) then
        Result := ScopeLevelLink
    else if IN4_IS_ADDR_MULTICAST(PInAddr(Address)) then
        Result := Ipv4MulticastAddressScope(Address)
    else
        Result := Ipv4UnicastAddressScope(Address);
end;

procedure IN4_UNCANONICALIZE_SCOPE_ID(const Address: PInAddr; ScopeId: PScopeID);
var
    ScopeLevel: TScopeLevel;
begin
    ScopeLevel := Ipv4AddressScope(PAnsiChar(Address));

    if IN4_IS_ADDR_LOOPBACK(Address) or (ScopeLevel = ScopeLevelGlobal) then
        ScopeId^.Value := 0;

    if TScopeLevel(ScopeIdGetLevel(ScopeId^.Value)) = ScopeLevel then
        ScopeIdSetLevel(ScopeId^.Value, 0);
    {
    if ((SCOPE_LEVEL)ScopeId->Level == ScopeLevel) {
        ScopeId->Level = 0;
    }
end;

procedure IN6ADDR_V4MAPPEDPREFIX_INIT(a6: PIn6Addr);
begin
    FillChar(a6^, Sizeof(TIn6Addr), 0);
    a6^.s6_bytes[10] := $FF;
    a6^.s6_bytes[11] := $FF;
end;

procedure IN6_SET_ADDR_V4MAPPED(a6: PIn6Addr; const a4: PInAddr);
begin
    a6^ := in6addr_v4mappedprefix;
    a6^.s6_bytes[12] := Byte(PAnsiChar(a4)[0]);
    a6^.s6_bytes[13] := Byte(PAnsiChar(a4)[1]);
    a6^.s6_bytes[14] := Byte(PAnsiChar(a4)[2]);
    a6^.s6_bytes[15] := Byte(PAnsiChar(a4)[3]);
end;

procedure IN6ADDR_SETV4MAPPED(a6: PSockAddrIn6; const a4: PInAddr;
    scope: SCOPE_ID; port: u_short);
begin
    a6^.sin6_family   := AF_INET6;
    a6^.sin6_port     := port;
    a6^.sin6_flowinfo := 0;
    IN6_SET_ADDR_V4MAPPED(PIn6Addr(@a6^.sin6_addr), a4);
    a6^.sin6_scope_struct := scope;
    IN4_UNCANONICALIZE_SCOPE_ID(a4, @a6^.sin6_scope_struct);
end;
{$ENDIF}


