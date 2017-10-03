{$ALIGN 1}

const
    WSADESCRIPTION_LEN        = 256;
    WSASYS_STATUS_LEN         = 128;
    IP_DEFAULT_MULTICAST_TTL  = 1;
    AF_INET                   = 2;         { internetwork: UDP, TCP, etc.     }
    PF_INET                   = AF_INET;
    IPPROTO_IP                = 0;         { dummy for IP                     }
    IPPROTO_TCP               = 6;         { tcp                              }
    IPPROTO_UDP               = 17;        { user datagram protocol           }
    IPPROTO_RAW               = 255;       { raw IP packet                    }
    SOCK_STREAM               = 1;         { stream socket                    }
    SOCK_DGRAM                = 2;         { datagram socket                  }
    SOCK_RAW                  = 3;         { raw-protocol interface           }
    INADDR_ANY                = $00000000;
    INADDR_LOOPBACK           = $7F000001;
    INADDR_BROADCAST          = -1;
    INADDR_NONE               = -1;
    IP_OPTIONS                = 1;
    IP_MULTICAST_IF           = 2;         { set/get IP multicast interface   }
    IP_MULTICAST_TTL          = 3;         { set/get IP multicast timetolive  }
    IP_MULTICAST_LOOP         = 4;         { set/get IP multicast loopback    }
    IP_ADD_MEMBERSHIP         = 5;         { add  an IP group membership      }
    IP_DROP_MEMBERSHIP        = 6;         { drop an IP group membership      }
    IP_TTL                    = 7;         { set/get IP Time To Live          }
    IP_TOS                    = 8;         { set/get IP Type Of Service       }
    IP_DONTFRAGMENT           = 9;         { set/get IP Don't Fragment flag   }
    SOL_SOCKET                = $ffff;     {options for socket level }
    // Option flags per-socket.
    SO_DEBUG                  = $0001;     { turn on debugging info recording }
    SO_ACCEPTCONN             = $0002;     { socket has had listen()          }
    SO_REUSEADDR              = $0004;     { allow local address reuse        }
    SO_KEEPALIVE              = $0008;     { keep connections alive           }
    SO_DONTROUTE              = $0010;     { just use interface addresses     }
    SO_BROADCAST              = $0020;     { permit sending of broadcast msgs }
    SO_USELOOPBACK            = $0040;     { bypass hardware when possible    }
    SO_LINGER                 = $0080;     { linger on close if data present  }
    SO_OOBINLINE              = $0100;     { leave received OOB data in line  }
    SO_DONTLINGER             = $ff7f;
    // Additional options.
    SO_SNDBUF                 = $1001;     { send buffer size                 }
    SO_RCVBUF                 = $1002;     { receive buffer size              }
    SO_SNDLOWAT               = $1003;     { send low-water mark              }
    SO_RCVLOWAT               = $1004;     { receive low-water mark           }
    SO_SNDTIMEO               = $1005;     { send timeout                     }
    SO_RCVTIMEO               = $1006;     { receive timeout                  }
    SO_ERROR                  = $1007;     { get error status and clear       }
    SO_TYPE                   = $1008;     { get socket type                  }
    TCP_NODELAY               = $0001;
    // Define flags to be used with the WSAAsyncSelect() call.
    FD_READ                   = $01;
    FD_WRITE                  = $02;
    FD_OOB                    = $04;
    FD_ACCEPT                 = $08;
    FD_CONNECT                = $10;
    FD_CLOSE                  = $20;

    WSABASEERR                = 10000;
    // Windows Sockets definitions of regular Microsoft C error constants
    WSAEINTR                  = (WSABASEERR+4);
    WSAEBADF                  = (WSABASEERR+9);
    WSAEACCES                 = (WSABASEERR+13);
    WSAEFAULT                 = (WSABASEERR+14);
    WSAEINVAL                 = (WSABASEERR+22);
    WSAEMFILE                 = (WSABASEERR+24);
    // Windows Sockets definitions of regular Berkeley error constants
    WSAEWOULDBLOCK            = (WSABASEERR+35);
    WSAEINPROGRESS            = (WSABASEERR+36);
    WSAEALREADY               = (WSABASEERR+37);
    WSAENOTSOCK               = (WSABASEERR+38);
    WSAEDESTADDRREQ           = (WSABASEERR+39);
    WSAEMSGSIZE               = (WSABASEERR+40);
    WSAEPROTOTYPE             = (WSABASEERR+41);
    WSAENOPROTOOPT            = (WSABASEERR+42);
    WSAEPROTONOSUPPORT        = (WSABASEERR+43);
    WSAESOCKTNOSUPPORT        = (WSABASEERR+44);
    WSAEOPNOTSUPP             = (WSABASEERR+45);
    WSAEPFNOSUPPORT           = (WSABASEERR+46);
    WSAEAFNOSUPPORT           = (WSABASEERR+47);
    WSAEADDRINUSE             = (WSABASEERR+48);
    WSAEADDRNOTAVAIL          = (WSABASEERR+49);
    WSAENETDOWN               = (WSABASEERR+50);
    WSAENETUNREACH            = (WSABASEERR+51);
    WSAENETRESET              = (WSABASEERR+52);
    WSAECONNABORTED           = (WSABASEERR+53);
    WSAECONNRESET             = (WSABASEERR+54);
    WSAENOBUFS                = (WSABASEERR+55);
    WSAEISCONN                = (WSABASEERR+56);
    WSAENOTCONN               = (WSABASEERR+57);
    WSAESHUTDOWN              = (WSABASEERR+58);
    WSAETOOMANYREFS           = (WSABASEERR+59);
    WSAETIMEDOUT              = (WSABASEERR+60);
    WSAECONNREFUSED           = (WSABASEERR+61);
    WSAELOOP                  = (WSABASEERR+62);
    WSAENAMETOOLONG           = (WSABASEERR+63);
    WSAEHOSTDOWN              = (WSABASEERR+64);
    WSAEHOSTUNREACH           = (WSABASEERR+65);
    WSAENOTEMPTY              = (WSABASEERR+66);
    WSAEPROCLIM               = (WSABASEERR+67);
    WSAEUSERS                 = (WSABASEERR+68);
    WSAEDQUOT                 = (WSABASEERR+69);
    WSAESTALE                 = (WSABASEERR+70);
    WSAEREMOTE                = (WSABASEERR+71);
    WSAEDISCON                = (WSABASEERR+101);
    // Extended Windows Sockets error constant definitions
    WSASYSNOTREADY            = (WSABASEERR+91);
    WSAVERNOTSUPPORTED        = (WSABASEERR+92);
    WSANOTINITIALISED         = (WSABASEERR+93);
    // Error return codes from gethostbyname() and gethostbyaddr()
    // (when using the resolver). Note that these errors are
    // retrieved via WSAGetLastError() and must therefore follow
    // the rules for avoiding clashes with error numbers from
    // specific implementations or language run-time systems.
    // For this reason the codes are based at WSABASEERR+1001.
    // Note also that [WSA]NO_ADDRESS is defined only for
    // compatibility purposes.
    // Authoritative Answer: Host not found
    WSAHOST_NOT_FOUND         = (WSABASEERR+1001);
    HOST_NOT_FOUND            = WSAHOST_NOT_FOUND;
    // Non-Authoritative: Host not found, or SERVERFAIL
    WSATRY_AGAIN              = (WSABASEERR+1002);
    TRY_AGAIN                 = WSATRY_AGAIN;
    // Non recoverable errors, FORMERR, REFUSED, NOTIMP
    WSANO_RECOVERY            = (WSABASEERR+1003);
    NO_RECOVERY               = WSANO_RECOVERY;
    // Valid name, no data record of requested type
    WSANO_DATA                = (WSABASEERR+1004);
    NO_DATA                   = WSANO_DATA;
    // no address, look for MX record
    WSANO_ADDRESS             = WSANO_DATA;
    NO_ADDRESS                = WSANO_ADDRESS;
    // Windows Sockets errors redefined as regular Berkeley error constants.
    // These are commented out in Windows NT to avoid conflicts with errno.h.
    // Use the WSA constants instead.
    EWOULDBLOCK               =  WSAEWOULDBLOCK;
    EINPROGRESS               =  WSAEINPROGRESS;
    EALREADY                  =  WSAEALREADY;
    ENOTSOCK                  =  WSAENOTSOCK;
    EDESTADDRREQ              =  WSAEDESTADDRREQ;
    EMSGSIZE                  =  WSAEMSGSIZE;
    EPROTOTYPE                =  WSAEPROTOTYPE;
    ENOPROTOOPT               =  WSAENOPROTOOPT;
    EPROTONOSUPPORT           =  WSAEPROTONOSUPPORT;
    ESOCKTNOSUPPORT           =  WSAESOCKTNOSUPPORT;
    EOPNOTSUPP                =  WSAEOPNOTSUPP;
    EPFNOSUPPORT              =  WSAEPFNOSUPPORT;
    EAFNOSUPPORT              =  WSAEAFNOSUPPORT;
    EADDRINUSE                =  WSAEADDRINUSE;
    EADDRNOTAVAIL             =  WSAEADDRNOTAVAIL;
    ENETDOWN                  =  WSAENETDOWN;
    ENETUNREACH               =  WSAENETUNREACH;
    ENETRESET                 =  WSAENETRESET;
    ECONNABORTED              =  WSAECONNABORTED;
    ECONNRESET                =  WSAECONNRESET;
    ENOBUFS                   =  WSAENOBUFS;
    EISCONN                   =  WSAEISCONN;
    ENOTCONN                  =  WSAENOTCONN;
    ESHUTDOWN                 =  WSAESHUTDOWN;
    ETOOMANYREFS              =  WSAETOOMANYREFS;
    ETIMEDOUT                 =  WSAETIMEDOUT;
    ECONNREFUSED              =  WSAECONNREFUSED;
    ELOOP                     =  WSAELOOP;
    ENAMETOOLONG              =  WSAENAMETOOLONG;
    EHOSTDOWN                 =  WSAEHOSTDOWN;
    EHOSTUNREACH              =  WSAEHOSTUNREACH;
    ENOTEMPTY                 =  WSAENOTEMPTY;
    EPROCLIM                  =  WSAEPROCLIM;
    EUSERS                    =  WSAEUSERS;
    EDQUOT                    =  WSAEDQUOT;
    ESTALE                    =  WSAESTALE;
    EREMOTE                   =  WSAEREMOTE;

    // ioctlsocket
    IOCPARM_MASK              = $7f;
    IOC_VOID                  = $20000000;
    IOC_OUT                   = $40000000;
    IOC_IN                    = $80000000;
    IOC_INOUT                 = (IOC_IN or IOC_OUT);
    FIONREAD                  = IOC_OUT or { get # bytes to read }
                                ((LongInt(SizeOf(LongInt))
                                  and IOCPARM_MASK) shl 16) or
                                (LongInt(Byte('f')) shl 8) or 127;
    FIONBIO                   = IOC_IN or { set/clear non-blocking i/o }
                                ((LongInt(SizeOf(LongInt))
                                  and IOCPARM_MASK) shl 16) or
                                (LongInt(Byte('f')) shl 8) or 126;
    FIOASYNC                  = IOC_IN or { set/clear async i/o }
                                ((LongInt(SizeOf(LongInt))
                                  and IOCPARM_MASK) shl 16) or
                                (LongInt(Byte('f')) shl 8) or 125;
    { Maximum queue length specifiable by listen. }
    SOMAXCONN                 = 5;
    MSG_OOB                   = $1;    {process out-of-band data }
    MSG_PEEK                  = $2;    {peek at incoming message }
    MSG_DONTROUTE             = $4;    {send without using routing tables }
    MSG_MAXIOVLEN             = 16;
    MSG_PARTIAL               = $8000; {partial send or recv for message xport }
    MAXGETHOSTSTRUCT          = 1024;

type
    [StructLayout(LayoutKind.Sequential)]
    TWSAData = record
        wVersion       : Word;
        wHighVersion   : Word;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=WSADESCRIPTION_LEN)]
        szDescription  : array[0..WSADESCRIPTION_LEN] of Char;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=WSASYS_STATUS_LEN)]
        szSystemStatus : array[0..WSASYS_STATUS_LEN]  of Char;
        iMaxSockets    : Word;
        iMaxUdpDg      : Word;
        //[MarshalAs(UnmanagedType.LPStr)]
        lpVendorInfo   : IntPtr;
    end;

    u_char  = Byte;
    u_short = Word;
    u_long  = Cardinal;
    u_int   = Integer;
    TSocket = u_int;

const
    INVALID_SOCKET            = TSocket(NOT(0));
    SOCKET_ERROR	      = -1;
    SizeOfTSockAddrIn         = 16;     // Win32 size
    SizeOfTSockAddr           = 16;     // Win32 size

type
    SunB = packed record
        s_b1, s_b2, s_b3, s_b4: u_char;
    end;

    SunW = packed record
        s_w1, s_w2: u_short;
    end;

    in_addr = record
    { case Integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: u_long); }
      S_addr: u_long;
    end;
    TInAddr = in_addr;

    sockaddr_in = record
    { case Integer of
      0: (sin_family: u_short;
          sin_port: u_short;
          sin_addr: TInAddr;
          sin_zero: array[0..7] of Char);
      1: (sa_family: u_short;
          sa_data: array[0..13] of Char) }
        sin_family : u_short;
        sin_port   : u_short;
        sin_addr   : TInAddr;
        [MarshalAs(UnmanagedType.ByValArray, SizeConst=8)]
        sin_zero   : array [0..7] of Char;
    end;
    TSockAddrIn = sockaddr_in;
    PSOCKADDR   = ^TSockAddr;
    TSockAddr   = sockaddr_in;

type
    PProtoEnt = ^TProtoEnt;
    protoent = record
        p_name    : IntPtr;   //PChar;
        p_aliases : IntPtr;   //^Pchar;
        p_proto   : Smallint;
    end;
    TProtoEnt = protoent;

    PServEnt = ^TServEnt;
    servent = record
        s_name    : IntPtr;       //PChar;
        s_aliases : IntPtr;       //^PChar;
        s_port    : Word;
        s_proto   : IntPtr;       //PChar;
    end;
    TServEnt = servent;

    PHostEnt = ^THostEnt;
    hostent = record
        h_name     : IntPtr;      //PChar;
        h_aliases  : IntPtr;      //^PChar;
        h_addrtype : Smallint;
        h_length   : Smallint;
        {case Byte of
          0: (h_addr_list: ^PChar);
          1: (h_addr: ^PChar) }
        h_addr_list: IntPtr;      //^PChar;
    end;
    THostEnt = hostent;

    PLinger = ^TLinger;
    linger = record
        l_onoff  : u_short;
        l_linger : u_short;
    end;
    TLinger = linger;

    PTimeVal = ^TTimeVal;
    timeval = record
        tv_sec  : Longint;
        tv_usec : Longint;
    end;
    TTimeVal = timeval;

    ip_mreq = record
        imr_multiaddr : in_addr;
        imr_interface : in_addr;
    end;

function  WSAStartup(wVersionRequired: WORD;
                     out WSData: TWSAData): Integer;
function  WSACleanup : Integer;
function  WSAGetLastError: Integer;
procedure WSASetLastError(iError: Integer);
function  WSAAsyncSelect(s: TSocket; HWindow: HWND; wMsg: u_int; lEvent: Longint): Integer;
function  WSAAsyncGetHostByName(HWindow: HWND; wMsg: u_int; const name : String;
                                buf: IntPtr; buflen: Integer): THandle;
function WSAAsyncGetHostByAddr(HWindow: HWND; wMsg: u_int; var addr: u_long;
  len, Struct: Integer; buf: IntPtr; buflen: Integer): THandle;
function  WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
function  gethostname(name: StringBuilder; len: Integer): Integer;
function  getprotobyname(name : String) : IntPtr;
function  getservbyname(name, proto: String): IntPtr;
function  gethostbyname(name: String): IntPtr;
function  gethostbyaddr(var addr: u_long; len, Struct: Integer): IntPtr;
function  getpeername(s: TSocket; out name: TSockAddr;
                      var namelen: Integer): Integer;
function  ntohl(netshort: u_short): u_short;
function  htons(hostshort: u_short): u_short;
function  ntohs(hostshort: u_short): u_short;
function  htonl(hostlong: u_long): u_long;
function  inet_addr(const cp: String): u_long;
function  inet_ntoa(inaddr: TInAddr): String;
function  socket(af, Struct, protocol: Integer): TSocket;
function  shutdown(s: TSocket; how: Integer): Integer;
function  closesocket(s: TSocket): Integer;
function  accept(s: TSocket; var addr: TSockAddr; var addrlen: Integer): TSocket;
function  bind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer;
function  connect(s: TSocket; var name: TSockAddr; namelen: Integer): Integer;
function  listen(s: TSocket; backlog: Integer): Integer;
function  recv(s: TSocket; [out] Buf: TBytes; len, flags: Integer): Integer;
function  recvfrom(s: TSocket; [out] Buf: TBytes; len, flags: Integer;
                   out from: TSockAddr; var fromlen: Integer): Integer;
function  ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
function  send(s: TSocket; const Buf : TBytes; len, flags: Integer): Integer;
function  sendto(s: TSocket; const Buf : TBytes; len, flags: Integer; var addrto: TSockAddr;
                 tolen: Integer): Integer;
function  getsockname(s: TSocket; out name: TSockAddr; var namelen: Integer): Integer;
function  setsockopt_integer(s: TSocket; level, optname: Integer;
                             var optval: Integer; optlen: Integer): Integer;
function  setsockopt_tinaddr(s: TSocket; level, optname: Integer;
                             var optval: TInAddr; optlen: Integer): Integer;
function  setsockopt_ip_mreq(s: TSocket; level, optname: Integer;
                             var optval: ip_mreq; optlen: Integer): Integer;
function  setsockopt_tlinger(s: TSocket; level, optname: Integer;
                             var optval: TLinger; optlen: Integer): Integer;
function  getsockopt_integer(s: TSocket; level, optname: Integer;
                             var optval: Integer; var optlen: Integer): Integer;
function  getsockopt_ip_mreq(s: TSocket; level, optname: Integer;
                             var optval: ip_mreq; var optlen: Integer): Integer;
function  getsockopt_tinaddr(s: TSocket; level, optname: Integer;
                             var optval: TInAddr; var optlen: Integer): Integer;
function  getsockopt_tlinger(s: TSocket; level, optname: Integer;
                             var optval: TLinger; var optlen: Integer): Integer;
