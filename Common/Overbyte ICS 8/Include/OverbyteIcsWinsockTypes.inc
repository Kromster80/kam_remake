{$IFDEF WIN32}
  {$ALIGN 4}
{$ELSE}
  {$ALIGN ON}
{$ENDIF}
{$MINENUMSIZE 4}

{.$IFNDEF NO_WINSOCK_2}
    (*$HPPEMIT '#include <Winsock2.h>'*)
    (*$HPPEMIT '#include <Mswsock.h>'*)
    (*$HPPEMIT '#include <Ws2tcpip.h>'*)
    //(*$HPPEMIT '#include <In6addr.h>'*)
{.$ELSE}
    //(*$HPPEMIT '#include <winsock.h>'*)
{.$ENDIF}

// the following emits are a workaround to the name conflict with
// procedure FD_SET and struct fd_set in winsock.h
(*$HPPEMIT 'namespace OverbyteIcsWinsock'*)
(*$HPPEMIT '{'*)
(*$HPPEMIT 'typedef fd_set *PFDSet;'*) // due to name conflict with procedure FD_SET
(*$HPPEMIT 'typedef fd_set TFDSet;'*)  // due to name conflict with procedure FD_SET
(*$HPPEMIT '}'*)

{.$DEFINE WS2_DLL_FUNC_VARS}
{.$DEFINE INCL_WINSOCK_API_PROTOTYPES}
{.$DEFINE INCL_WINSOCK_API_TYPEDEFS}

const
  WINSOCK_VERSION = $0202;
  {$EXTERNALSYM WINSOCK_VERSION}

type
  u_char = AnsiChar;
  {$EXTERNALSYM u_char}
  u_short = Word;
  {$EXTERNALSYM u_short}
  u_int = Integer;
  {$EXTERNALSYM u_int}
  u_long = Longint;
  {$EXTERNALSYM u_long}

{ The new type to be used in all
  instances which refer to sockets. }
{$IFDEF WIN32}
  TSocket = u_int;
{$ELSE}
  TSocket = IntPtr;
{$ENDIF}
  {$EXTERNALSYM TSocket}

const
  FD_SETSIZE     =   64;
  {$EXTERNALSYM FD_SETSIZE}

// WinSock 2 extension -- manifest constants for shutdown()
  SD_RECEIVE     = 0;
  {$EXTERNALSYM SD_RECEIVE}
  SD_SEND        = 1;
  {$EXTERNALSYM SD_SEND}
  SD_BOTH        = 2;
  {$EXTERNALSYM SD_BOTH}

type
  PFDSet = ^TFDSet;
  {$NODEFINE PFDSet}

  TFDSet = record
    fd_count: u_int;
    fd_array: array[0..FD_SETSIZE-1] of TSocket;
  end;
  {$NODEFINE TFDSet}

  PTimeVal = ^TTimeVal;
  timeval = record
    tv_sec: Longint;
    tv_usec: Longint;
  end;
  {$EXTERNALSYM timeval}
  TTimeVal = timeval;

const
  IOCPARM_MASK = $7f;
  {$EXTERNALSYM IOCPARM_MASK}
  IOC_VOID     = $20000000;
  {$EXTERNALSYM IOC_VOID}
  IOC_OUT      = $40000000;
  {$EXTERNALSYM IOC_OUT}
  IOC_IN       = $80000000;
  {$EXTERNALSYM IOC_IN}
  IOC_INOUT    = (IOC_IN or IOC_OUT);
  {$EXTERNALSYM IOC_INOUT}

  FIONREAD     = IOC_OUT or { get # bytes to read }
    ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 127;
  {$EXTERNALSYM FIONREAD}
  FIONBIO      = IOC_IN or { set/clear non-blocking i/o }
    ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 126;
  {$EXTERNALSYM FIONBIO}
  FIOASYNC     = IOC_IN or { set/clear async i/o }
    ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 125;
  {$EXTERNALSYM FIOASYNC}

type
  PHostEnt = ^THostEnt;
  hostent = record
    h_name: PAnsiChar;
    h_aliases: ^PAnsiChar;
    h_addrtype: Smallint;
    h_length: Smallint;
    case Byte of
      0: (h_addr_list: ^PAnsiChar);
      1: (h_addr: ^PAnsiChar)
  end;
  {$EXTERNALSYM hostent}
  THostEnt = hostent;

  PNetEnt = ^TNetEnt;
  netent = record
    n_name: PAnsiChar;
    n_aliases: ^PAnsiChar;
    n_addrtype: Smallint;
    n_net: u_long;
  end;
  {$EXTERNALSYM netent}
  TNetEnt = netent;

  PServEnt = ^TServEnt;
  servent = record
    s_name: PAnsiChar;
    s_aliases: ^PAnsiChar;
  {$IFDEF WIN64}
    s_proto: PAnsiChar;
    s_port: Word;
  {$ELSE}
    s_port: Word;
    s_proto: PAnsiChar;
  {$ENDIF}
  end;
  {$EXTERNALSYM servent}
  TServEnt = servent;

  PProtoEnt = ^TProtoEnt;
  protoent = record
    p_name: PAnsiChar;
    p_aliases: ^PAnsiChar;
    p_proto: Smallint;
  end;
  {$EXTERNALSYM protoent}
  TProtoEnt = protoent;

const

{ Protocols }

  IPPROTO_IP     =   0;             { dummy for IP }
  {$EXTERNALSYM IPPROTO_IP}
  IPPROTO_ICMP   =   1;             { control message protocol }
  {$EXTERNALSYM IPPROTO_ICMP}
  IPPROTO_IGMP   =   2;             { group management protocol }
  {$EXTERNALSYM IPPROTO_IGMP}
  IPPROTO_GGP    =   3;             { gateway^2 (deprecated) }
  {$EXTERNALSYM IPPROTO_GGP}
  IPPROTO_TCP    =   6;             { tcp }
  {$EXTERNALSYM IPPROTO_TCP}
  IPPROTO_PUP    =  12;             { pup }
  {$EXTERNALSYM IPPROTO_PUP}
  IPPROTO_UDP    =  17;             { user datagram protocol }
  {$EXTERNALSYM IPPROTO_UDP}
  IPPROTO_IDP    =  22;             { xns idp }
  {$EXTERNALSYM IPPROTO_IDP}
  IPPROTO_IPV6   =  41;
  {$EXTERNALSYM IPPROTO_IPV6}
  IPPROTO_ICMPV6 =  58;
  {$EXTERNALSYM IPPROTO_ICMPV6}
  IPPROTO_ND     =  77;             { UNOFFICIAL net disk proto }
  {$EXTERNALSYM IPPROTO_ND}

  IPPROTO_RAW    =  255;            { raw IP packet }
  {$EXTERNALSYM IPPROTO_RAW}
  IPPROTO_MAX    =  256;
  {$EXTERNALSYM IPPROTO_MAX}

{ Port/socket numbers: network standard functions}

  IPPORT_ECHO    =   7;
  {$EXTERNALSYM IPPORT_ECHO}
  IPPORT_DISCARD =   9;
  {$EXTERNALSYM IPPORT_DISCARD}
  IPPORT_SYSTAT  =   11;
  {$EXTERNALSYM IPPORT_SYSTAT}
  IPPORT_DAYTIME =   13;
  {$EXTERNALSYM IPPORT_DAYTIME}
  IPPORT_NETSTAT =   15;
  {$EXTERNALSYM IPPORT_NETSTAT}
  IPPORT_FTP     =   21;
  {$EXTERNALSYM IPPORT_FTP}
  IPPORT_TELNET  =   23;
  {$EXTERNALSYM IPPORT_TELNET}
  IPPORT_SMTP    =   25;
  {$EXTERNALSYM IPPORT_SMTP}
  IPPORT_TIMESERVER  =  37;
  {$EXTERNALSYM IPPORT_TIMESERVER}
  IPPORT_NAMESERVER  =  42;
  {$EXTERNALSYM IPPORT_NAMESERVER}
  IPPORT_WHOIS       =  43;
  {$EXTERNALSYM IPPORT_WHOIS}
  IPPORT_MTP         =  57;
  {$EXTERNALSYM IPPORT_MTP}

{ Port/socket numbers: host specific functions }

  IPPORT_TFTP        =  69;
  {$EXTERNALSYM IPPORT_TFTP}
  IPPORT_RJE         =  77;
  {$EXTERNALSYM IPPORT_RJE}
  IPPORT_FINGER      =  79;
  {$EXTERNALSYM IPPORT_FINGER}
  IPPORT_TTYLINK     =  87;
  {$EXTERNALSYM IPPORT_TTYLINK}
  IPPORT_SUPDUP      =  95;
  {$EXTERNALSYM IPPORT_SUPDUP}

{ UNIX TCP sockets }

  IPPORT_EXECSERVER  =  512;
  {$EXTERNALSYM IPPORT_EXECSERVER}
  IPPORT_LOGINSERVER =  513;
  {$EXTERNALSYM IPPORT_LOGINSERVER}
  IPPORT_CMDSERVER   =  514;
  {$EXTERNALSYM IPPORT_CMDSERVER}
  IPPORT_EFSSERVER   =  520;
  {$EXTERNALSYM IPPORT_EFSSERVER}

{ UNIX UDP sockets }

  IPPORT_BIFFUDP     =  512;
  {$EXTERNALSYM IPPORT_BIFFUDP}
  IPPORT_WHOSERVER   =  513;
  {$EXTERNALSYM IPPORT_WHOSERVER}
  IPPORT_ROUTESERVER =  520;
  {$EXTERNALSYM IPPORT_ROUTESERVER}

{ Ports < IPPORT_RESERVED are reserved for
  privileged processes (e.g. root). }

  IPPORT_RESERVED    =  1024;
  {$EXTERNALSYM IPPORT_RESERVED}

{ Link numbers }

  IMPLINK_IP         =  155;
  {$EXTERNALSYM IMPLINK_IP}
  IMPLINK_LOWEXPER   =  156;
  {$EXTERNALSYM IMPLINK_LOWEXPER}
  IMPLINK_HIGHEXPER  =  158;
  {$EXTERNALSYM IMPLINK_HIGHEXPER}

type
  SunB = record
    s_b1, s_b2, s_b3, s_b4: u_char;
  end;
  {$EXTERNALSYM SunB}

  SunW = record
    s_w1, s_w2: u_short;
  end;
  {$EXTERNALSYM SunW}

  PInAddr = ^TInAddr;
  in_addr = record
    case integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: u_long);
  end;
  {$EXTERNALSYM in_addr}
  TInAddr = in_addr;

  PSockAddrIn = ^TSockAddrIn;
  sockaddr_in = record
    case Integer of
      0: (sin_family: u_short;
          sin_port: u_short;
          sin_addr: TInAddr;
          sin_zero: array[0..7] of AnsiChar);
      1: (sa_family: u_short;
          sa_data: array[0..13] of AnsiChar);
  end;
  {$EXTERNALSYM sockaddr_in}
  TSockAddrIn = sockaddr_in;
  
const
  INADDR_ANY       = $00000000;
  {$EXTERNALSYM INADDR_ANY}
  INADDR_LOOPBACK  = $7F000001;
  {$EXTERNALSYM INADDR_LOOPBACK}
  INADDR_BROADCAST = DWORD($FFFFFFFF);
  {$EXTERNALSYM INADDR_BROADCAST}
  INADDR_NONE      = DWORD($FFFFFFFF);
  {$EXTERNALSYM INADDR_NONE}
  WSADESCRIPTION_LEN     =   256;
{$EXTERNALSYM WSADESCRIPTION_LEN}
  WSASYS_STATUS_LEN      =   128;
  {$EXTERNALSYM WSASYS_STATUS_LEN}

{$IFDEF STILL_NEEDS_CHECK}
  IN4ADDR_BROADCAST = INADDR_BROADCAST;
  {$EXTERNALSYM IN4ADDR_BROADCAST}
{$ENDIF}

type
{$IFDEF STILL_NEEDS_CHECK}
{ Scope ID definition }
  PScopeLevel = ^TScopeLevel;
  SCOPE_LEVEL              = (
    ScopeLevelInterface    = 1,
    ScopeLevelLink         = 2,
    ScopeLevelSubnet       = 3,
    ScopeLevelAdmin        = 4,
    ScopeLevelSite         = 5,
    ScopeLevelOrganization = 8,
    ScopeLevelGlobal       = 14,
    ScopeLevelCount        = 16
  );
  {$EXTERNALSYM SCOPE_LEVEL}
  TScopeLevel = SCOPE_LEVEL;

  PSCOPE_ID = ^SCOPE_ID;
  {$EXTERNALSYM PSCOPE_ID}
  SCOPE_ID = record
      Value: ULONG; // Dummy actually a record with C bitfields
  end;
  {$EXTERNALSYM SCOPE_ID}
  TScopeID = SCOPE_ID;
  PScopeID = PSCOPE_ID;
{$ENDIF}

  PWSAData = ^TWSAData;
  WSAData = record // !!! also WSADATA
    wVersion: Word;
    wHighVersion: Word;
  {$IFDEF WIN64}
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PAnsiChar;
    szDescription: array[0..WSADESCRIPTION_LEN] of AnsiChar;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of AnsiChar;
  {$ELSE}
    szDescription: array[0..WSADESCRIPTION_LEN] of AnsiChar;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of AnsiChar;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PAnsiChar;
  {$ENDIF}
  end;
  {$EXTERNALSYM WSAData}
  TWSAData = WSAData;

  PTransmitFileBuffers = ^TTransmitFileBuffers;
  _TRANSMIT_FILE_BUFFERS = record
      Head: Pointer;
      HeadLength: DWORD;
      Tail: Pointer;
      TailLength: DWORD;
  end;
  {$EXTERNALSYM _TRANSMIT_FILE_BUFFERS}
  TRANSMIT_FILE_BUFFERS = _TRANSMIT_FILE_BUFFERS;
  {$EXTERNALSYM TRANSMIT_FILE_BUFFERS}
  TTransmitFileBuffers = _TRANSMIT_FILE_BUFFERS;

const
  TF_DISCONNECT           = $01;
  {$EXTERNALSYM TF_DISCONNECT}
  TF_REUSE_SOCKET         = $02;
  {$EXTERNALSYM TF_REUSE_SOCKET}
  TF_WRITE_BEHIND         = $04;
  {$EXTERNALSYM TF_WRITE_BEHIND}

  { Options for use with [gs]etsockopt at the IP level. }
  IP_OPTIONS              = 1;
  {$EXTERNALSYM IP_OPTIONS}

{/////////////////////////////////////////////////////////////////////////////}
{.$IFNDEF NO_WINSOCK_2}    // Old Version 1 //
{/////////////////////////////////////////////////////////////////////////////}
(*
  {$EXTERNALSYM IP_MULTICAST_IF}
  IP_MULTICAST_IF     = 2;           { set/get IP multicast interface   }
  {$EXTERNALSYM IP_MULTICAST_TTL}
  IP_MULTICAST_TTL    = 3;           { set/get IP multicast timetolive  }
  {$EXTERNALSYM IP_MULTICAST_LOOP}
  IP_MULTICAST_LOOP   = 4;           { set/get IP multicast loopback    }
  {$EXTERNALSYM IP_ADD_MEMBERSHIP}
  IP_ADD_MEMBERSHIP   = 5;           { add  an IP group membership      }
  {$EXTERNALSYM IP_DROP_MEMBERSHIP}
  IP_DROP_MEMBERSHIP  = 6;           { drop an IP group membership      }
  {$EXTERNALSYM IP_TTL}
  IP_TTL              = 7;           { set/get IP Time To Live          }
  {$EXTERNALSYM IP_TOS}
  IP_TOS              = 8;           { set/get IP Type Of Service       }
  {$EXTERNALSYM IP_DONTFRAGMENT}
  IP_DONTFRAGMENT     = 9;           { set/get IP Don't Fragment flag   }
*)
{/////////////////////////////////////////////////////////////////////////////}
{.$ELSE}                    // New Version 2 //
{/////////////////////////////////////////////////////////////////////////////}

  IP_HDRINCL          = 2;
  {$EXTERNALSYM IP_HDRINCL}
  IP_TOS              = 3;           { set/get IP Type Of Service       }
  {$EXTERNALSYM IP_TOS}
  IP_TTL              = 4;           { set/get IP Time To Live          }
  {$EXTERNALSYM IP_TTL}
  IP_MULTICAST_IF     = 9;           { set/get IP multicast interface   }
  {$EXTERNALSYM IP_MULTICAST_IF}
  IP_MULTICAST_TTL    = 10;          { set/get IP multicast timetolive  }
  {$EXTERNALSYM IP_MULTICAST_TTL}
  IP_MULTICAST_LOOP   = 11;          { set/get IP multicast loopback    }
  {$EXTERNALSYM IP_MULTICAST_LOOP}
  IP_ADD_MEMBERSHIP   = 12;          { add  an IP group membership      }
  {$EXTERNALSYM IP_ADD_MEMBERSHIP}
  IP_DROP_MEMBERSHIP  = 13;          { drop an IP group membership      }
  {$EXTERNALSYM IP_DROP_MEMBERSHIP}
  IP_DONTFRAGMENT     = 14;          { set/get IP Don't Fragment flag   }
  {$EXTERNALSYM IP_DONTFRAGMENT}

{/////////////////////////////////////////////////////////////////////////////}
{.$ENDIF}
{/////////////////////////////////////////////////////////////////////////////}

  IP_DEFAULT_MULTICAST_TTL   = 1;    { normally limit m'casts to 1 hop  }
  {$EXTERNALSYM IP_DEFAULT_MULTICAST_TTL}
  IP_DEFAULT_MULTICAST_LOOP  = 1;    { normally hear sends if a member  }
  {$EXTERNALSYM IP_DEFAULT_MULTICAST_LOOP}
  IP_MAX_MEMBERSHIPS         = 20;   { per socket; must fit in one mbuf }
  {$EXTERNALSYM IP_MAX_MEMBERSHIPS}

type
  ip_mreq = record
    imr_multiaddr : in_addr;
    imr_interface : in_addr;
  end;
  {$EXTERNALSYM ip_mreq}
  TIpMReq = ip_mreq;
  PIpMReq = ^TIpMReq;
  
const
  
{ This is used instead of -1, since the
  TSocket type is unsigned.}

  INVALID_SOCKET    = TSocket(not(0));
  {$EXTERNALSYM INVALID_SOCKET}
  SOCKET_ERROR      = -1;
  {$EXTERNALSYM SOCKET_ERROR}

{ Types }

  SOCK_STREAM     = 1;               { stream socket }
  {$EXTERNALSYM SOCK_STREAM}
  SOCK_DGRAM      = 2;               { datagram socket }
  {$EXTERNALSYM SOCK_DGRAM}
  SOCK_RAW        = 3;               { raw-protocol interface }
  {$EXTERNALSYM SOCK_RAW}
  SOCK_RDM        = 4;               { reliably-delivered message }
  {$EXTERNALSYM SOCK_RDM}
  SOCK_SEQPACKET  = 5;               { sequenced packet stream }
  {$EXTERNALSYM SOCK_SEQPACKET}

{ Option flags per-socket. }

  SO_DEBUG        = $0001;          { turn on debugging info recording }
  {$EXTERNALSYM SO_DEBUG}
  SO_ACCEPTCONN   = $0002;          { socket has had listen() }
  {$EXTERNALSYM SO_ACCEPTCONN}
  SO_REUSEADDR    = $0004;          { allow local address reuse }
  {$EXTERNALSYM SO_REUSEADDR}
  SO_KEEPALIVE    = $0008;          { keep connections alive }
  {$EXTERNALSYM SO_KEEPALIVE}
  SO_DONTROUTE    = $0010;          { just use interface addresses }
  {$EXTERNALSYM SO_DONTROUTE}
  SO_BROADCAST    = $0020;          { permit sending of broadcast msgs }
  {$EXTERNALSYM SO_BROADCAST}
  SO_USELOOPBACK  = $0040;          { bypass hardware when possible }
  {$EXTERNALSYM SO_USELOOPBACK}
  SO_LINGER       = $0080;          { linger on close if data present }
  {$EXTERNALSYM SO_LINGER}
  SO_OOBINLINE    = $0100;          { leave received OOB data in line }
  {$EXTERNALSYM SO_OOBINLINE}

  SO_DONTLINGER  =   $ff7f;
  {$EXTERNALSYM SO_DONTLINGER}

{ Additional options. }

  SO_SNDBUF       = $1001;          { send buffer size }
  {$EXTERNALSYM SO_SNDBUF}
  SO_RCVBUF       = $1002;          { receive buffer size }
  {$EXTERNALSYM SO_RCVBUF}
  SO_SNDLOWAT     = $1003;          { send low-water mark }
  {$EXTERNALSYM SO_SNDLOWAT}
  SO_RCVLOWAT     = $1004;          { receive low-water mark }
  {$EXTERNALSYM SO_RCVLOWAT}
  SO_SNDTIMEO     = $1005;          { send timeout }
  {$EXTERNALSYM SO_SNDTIMEO}
  SO_RCVTIMEO     = $1006;          { receive timeout }
  {$EXTERNALSYM SO_RCVTIMEO}
  SO_ERROR        = $1007;          { get error status and clear }
  {$EXTERNALSYM SO_ERROR}
  SO_TYPE         = $1008;          { get socket type }
  {$EXTERNALSYM SO_TYPE}

{ Options for connect and disconnect data and options.  Used only by
  non-TCP/IP transports such as DECNet, OSI TP4, etc. }

  SO_CONNDATA     = $7000;
  {$EXTERNALSYM SO_CONNDATA}
  SO_CONNOPT      = $7001;
  {$EXTERNALSYM SO_CONNOPT}
  SO_DISCDATA     = $7002;
  {$EXTERNALSYM SO_DISCDATA}
  SO_DISCOPT      = $7003;
  {$EXTERNALSYM SO_DISCOPT}
  SO_CONNDATALEN  = $7004;
  {$EXTERNALSYM SO_CONNDATALEN}
  SO_CONNOPTLEN   = $7005;
  {$EXTERNALSYM SO_CONNOPTLEN}
  SO_DISCDATALEN  = $7006;
  {$EXTERNALSYM SO_DISCDATALEN}
  SO_DISCOPTLEN   = $7007;
  {$EXTERNALSYM SO_DISCOPTLEN}

{ Option for opening sockets for synchronous access. }

  SO_OPENTYPE     = $7008;
  {$EXTERNALSYM SO_OPENTYPE}

  SO_SYNCHRONOUS_ALERT    = $10;
  {$EXTERNALSYM SO_SYNCHRONOUS_ALERT}

  SO_SYNCHRONOUS_NONALERT = $20;
  {$EXTERNALSYM SO_SYNCHRONOUS_NONALERT}

{ Other NT-specific options. }

  SO_MAXDG        = $7009;
  {$EXTERNALSYM SO_MAXDG}
  SO_MAXPATHDG    = $700A;
  {$EXTERNALSYM SO_MAXPATHDG}
  SO_UPDATE_ACCEPT_CONTEXT     = $700B;
  {$EXTERNALSYM SO_UPDATE_ACCEPT_CONTEXT}
  SO_CONNECT_TIME = $700C;
  {$EXTERNALSYM SO_CONNECT_TIME}

{ TCP options. }

  TCP_NODELAY     = $0001;
  {$EXTERNALSYM TCP_NODELAY}
  TCP_BSDURGENT   = $7000;
  {$EXTERNALSYM TCP_BSDURGENT}

{ Address families. }

  AF_UNSPEC       = 0;               { unspecified }
  {$EXTERNALSYM AF_UNSPEC}
  AF_UNIX         = 1;               { local to host (pipes, portals) }
  {$EXTERNALSYM AF_UNIX}
  AF_INET         = 2;               { internetwork: UDP, TCP, etc. }
  {$EXTERNALSYM AF_INET}
  AF_IMPLINK      = 3;               { arpanet imp addresses }
  {$EXTERNALSYM AF_IMPLINK}
  AF_PUP          = 4;               { pup protocols: e.g. BSP }
  {$EXTERNALSYM AF_PUP}
  AF_CHAOS        = 5;               { mit CHAOS protocols }
  {$EXTERNALSYM AF_CHAOS}
  AF_NS           = 6;               { XEROX NS protocols }
  {$EXTERNALSYM AF_NS}
  AF_IPX          = AF_NS;           { IPX and SPX }
  {$EXTERNALSYM AF_IPX}
  AF_ISO          = 7;               { ISO protocols }
  {$EXTERNALSYM AF_ISO}
  AF_OSI          = AF_ISO;          { OSI is ISO }
  {$EXTERNALSYM AF_OSI}
  AF_ECMA         = 8;               { european computer manufacturers }
  {$EXTERNALSYM AF_ECMA}
  AF_DATAKIT      = 9;               { datakit protocols }
  {$EXTERNALSYM AF_DATAKIT}
  AF_CCITT        = 10;              { CCITT protocols, X.25 etc }
  {$EXTERNALSYM AF_CCITT}
  AF_SNA          = 11;              { IBM SNA }
  {$EXTERNALSYM AF_SNA}
  AF_DECnet       = 12;              { DECnet }
  {$EXTERNALSYM AF_DECnet}
  AF_DLI          = 13;              { Direct data link interface }
  {$EXTERNALSYM AF_DLI}
  AF_LAT          = 14;              { LAT }
  {$EXTERNALSYM AF_LAT}
  AF_HYLINK       = 15;              { NSC Hyperchannel }
  {$EXTERNALSYM AF_HYLINK}
  AF_APPLETALK    = 16;              { AppleTalk }
  {$EXTERNALSYM AF_APPLETALK}
  AF_NETBIOS      = 17;              { NetBios-style addresses }
  {$EXTERNALSYM AF_NETBIOS}
  AF_VOICEVIEW    = 18;              { VoiceView }
  {$EXTERNALSYM AF_VOICEVIEW}
  AF_FIREFOX      = 19;              { FireFox }
  {$EXTERNALSYM AF_FIREFOX}
  AF_UNKNOWN1     = 20;              { Somebody is using this! }
  {$EXTERNALSYM AF_UNKNOWN1}
  AF_BAN          = 21;              { Banyan }
  {$EXTERNALSYM AF_BAN}
  AF_MAX          = 22;
  {$EXTERNALSYM AF_MAX}

{.$IFNDEF NO_WINSOCK_2}
  AF_INET6        = 23;              { Internetwork Version 6 }
  {$EXTERNALSYM AF_INET6}
{.$ENDIF}

type
  { Structure used by kernel to store most addresses. }

  PSOCKADDR = ^TSockAddr;
  {$EXTERNALSYM PSOCKADDR}
  TSockAddr = sockaddr_in;
  {$EXTERNALSYM TSockAddr}

  { Structure used by kernel to pass protocol information in raw sockets. }
  PSockProto = ^TSockProto;
  sockproto = record
    sp_family: u_short;
    sp_protocol: u_short;
  end;
  {$EXTERNALSYM sockproto}
  TSockProto = sockproto;

const
{ Protocol families, same as address families for now. }

  PF_UNSPEC       = AF_UNSPEC;
  {$EXTERNALSYM PF_UNSPEC}
  PF_UNIX         = AF_UNIX;
  {$EXTERNALSYM PF_UNIX}
  PF_INET         = AF_INET;
  {$EXTERNALSYM PF_INET}
  PF_INET6         = AF_INET6;
  {$EXTERNALSYM PF_INET6}
  PF_IMPLINK      = AF_IMPLINK;
  {$EXTERNALSYM PF_IMPLINK}
  PF_PUP          = AF_PUP;
  {$EXTERNALSYM PF_PUP}
  PF_CHAOS        = AF_CHAOS;
  {$EXTERNALSYM PF_CHAOS}
  PF_NS           = AF_NS;
  {$EXTERNALSYM PF_NS}
  PF_IPX          = AF_IPX;
  {$EXTERNALSYM PF_IPX}
  PF_ISO          = AF_ISO;
  {$EXTERNALSYM PF_ISO}
  PF_OSI          = AF_OSI;
  {$EXTERNALSYM PF_OSI}
  PF_ECMA         = AF_ECMA;
  {$EXTERNALSYM PF_ECMA}
  PF_DATAKIT      = AF_DATAKIT;
  {$EXTERNALSYM PF_DATAKIT}
  PF_CCITT        = AF_CCITT;
  {$EXTERNALSYM PF_CCITT}
  PF_SNA          = AF_SNA;
  {$EXTERNALSYM PF_SNA}
  PF_DECnet       = AF_DECnet;
  {$EXTERNALSYM PF_DECnet}
  PF_DLI          = AF_DLI;
  {$EXTERNALSYM PF_DLI}
  PF_LAT          = AF_LAT;
  {$EXTERNALSYM PF_LAT}
  PF_HYLINK       = AF_HYLINK;
  {$EXTERNALSYM PF_HYLINK}
  PF_APPLETALK    = AF_APPLETALK;
  {$EXTERNALSYM PF_APPLETALK}
  PF_VOICEVIEW    = AF_VOICEVIEW;
  {$EXTERNALSYM PF_VOICEVIEW}
  PF_FIREFOX      = AF_FIREFOX;
  {$EXTERNALSYM PF_FIREFOX}
  PF_UNKNOWN1     = AF_UNKNOWN1;
  {$EXTERNALSYM PF_UNKNOWN1}
  PF_BAN          = AF_BAN;
  {$EXTERNALSYM PF_BAN}

  PF_MAX          = AF_MAX;
  {$EXTERNALSYM PF_MAX}

type
{ Structure used for manipulating linger option. }
  PLinger = ^TLinger;
  linger = record
    l_onoff: u_short;
    l_linger: u_short;
  end;
  {$EXTERNALSYM linger}
  TLinger = linger;

const
{ Level number for (get/set)sockopt() to apply to socket itself. }

  {$EXTERNALSYM SOL_SOCKET}
  SOL_SOCKET      = $ffff;          {options for socket level }

{ Maximum queue length specifiable by listen. }

  SOMAXCONN       = 5;
  {$EXTERNALSYM SOMAXCONN}

  MSG_OOB         = $1;             {process out-of-band data }
  {$EXTERNALSYM MSG_OOB}
  MSG_PEEK        = $2;             {peek at incoming message }
  {$EXTERNALSYM MSG_PEEK}
  MSG_DONTROUTE   = $4;             {send without using routing tables }
  {$EXTERNALSYM MSG_DONTROUTE}
  MSG_MAXIOVLEN   = 16;
  {$EXTERNALSYM MSG_MAXIOVLEN}
  MSG_PARTIAL     = $8000;          {partial send or recv for message xport }
  {$EXTERNALSYM MSG_PARTIAL}

{ Define constant based on rfc883, used by gethostbyxxxx() calls. }

  MAXGETHOSTSTRUCT        = 1024;
  {$EXTERNALSYM MAXGETHOSTSTRUCT}

{ Define flags to be used with the WSAAsyncSelect() call. }

{*
 * WinSock 2 extension -- bit values and indices for FD_XXX network events
 *}
  FD_READ_BIT             = 0;
  {$EXTERNALSYM FD_READ_BIT}
  FD_READ                 = 1 shl FD_READ_BIT;
  {$EXTERNALSYM FD_READ}

  FD_WRITE_BIT            = 1;
  {$EXTERNALSYM FD_WRITE_BIT}
  FD_WRITE                = 1 shl FD_WRITE_BIT;
  {$EXTERNALSYM FD_WRITE}

  FD_OOB_BIT              = 2;
  {$EXTERNALSYM FD_OOB_BIT}
  FD_OOB                  = 1 shl FD_OOB_BIT;
  {$EXTERNALSYM FD_OOB}

  FD_ACCEPT_BIT           = 3;
  {$EXTERNALSYM FD_ACCEPT_BIT}
  FD_ACCEPT               = 1 shl FD_ACCEPT_BIT;
  {$EXTERNALSYM FD_ACCEPT}

  FD_CONNECT_BIT          = 4;
  {$EXTERNALSYM FD_CONNECT_BIT}
  FD_CONNECT              = 1 shl FD_CONNECT_BIT;
  {$EXTERNALSYM FD_CONNECT}

  FD_CLOSE_BIT            = 5;
  {$EXTERNALSYM FD_CLOSE_BIT}
  FD_CLOSE                = 1 shl FD_CLOSE_BIT;
  {$EXTERNALSYM FD_CLOSE}

  FD_QOS_BIT              = 6;
  {$EXTERNALSYM FD_QOS_BIT}
  FD_QOS                  = 1 shl FD_QOS_BIT;
  {$EXTERNALSYM FD_QOS}

  FD_GROUP_QOS_BIT        = 7;
  {$EXTERNALSYM FD_GROUP_QOS_BIT}
  FD_GROUP_QOS            = 1 shl FD_GROUP_QOS_BIT;
  {$EXTERNALSYM FD_GROUP_QOS}

  FD_ROUTING_INTERFACE_CHANGE_BIT = 8;
  {$EXTERNALSYM FD_ROUTING_INTERFACE_CHANGE_BIT}
  FD_ROUTING_INTERFACE_CHANGE     = 1 shl FD_ROUTING_INTERFACE_CHANGE_BIT;
  {$EXTERNALSYM FD_ROUTING_INTERFACE_CHANGE}

  FD_ADDRESS_LIST_CHANGE_BIT      = 9;
  {$EXTERNALSYM FD_ADDRESS_LIST_CHANGE_BIT}
  FD_ADDRESS_LIST_CHANGE          = 1 shl FD_ADDRESS_LIST_CHANGE_BIT;
  {$EXTERNALSYM FD_ADDRESS_LIST_CHANGE}

  FD_MAX_EVENTS           = 10;
  {$EXTERNALSYM FD_MAX_EVENTS}
  FD_ALL_EVENTS           = ((1 shl FD_MAX_EVENTS) - 1);
  {$EXTERNALSYM FD_ALL_EVENTS}

{ All Windows Sockets error constants are biased by WSABASEERR from the "normal" }

  WSABASEERR              = 10000;
  {$EXTERNALSYM WSABASEERR}

{ Windows Sockets definitions of regular Microsoft C error constants }

  {$EXTERNALSYM WSAEINTR}
  WSAEINTR                = (WSABASEERR+4);
  {$EXTERNALSYM WSAEBADF}
  WSAEBADF                = (WSABASEERR+9);
  {$EXTERNALSYM WSAEACCES}
  WSAEACCES               = (WSABASEERR+13);
  {$EXTERNALSYM WSAEFAULT}
  WSAEFAULT               = (WSABASEERR+14);
  {$EXTERNALSYM WSAEINVAL}
  WSAEINVAL               = (WSABASEERR+22);
  {$EXTERNALSYM WSAEMFILE}
  WSAEMFILE               = (WSABASEERR+24);

{ Windows Sockets definitions of regular Berkeley error constants }

  WSAEWOULDBLOCK          = (WSABASEERR+35);
  {$EXTERNALSYM WSAEWOULDBLOCK}
  WSAEINPROGRESS          = (WSABASEERR+36);
  {$EXTERNALSYM WSAEINPROGRESS}
  WSAEALREADY             = (WSABASEERR+37);
  {$EXTERNALSYM WSAEALREADY}
  WSAENOTSOCK             = (WSABASEERR+38);
  {$EXTERNALSYM WSAENOTSOCK}
  WSAEDESTADDRREQ         = (WSABASEERR+39);
  {$EXTERNALSYM WSAEDESTADDRREQ}
  WSAEMSGSIZE             = (WSABASEERR+40);
  {$EXTERNALSYM WSAEMSGSIZE}
  WSAEPROTOTYPE           = (WSABASEERR+41);
  {$EXTERNALSYM WSAEPROTOTYPE}
  WSAENOPROTOOPT          = (WSABASEERR+42);
  {$EXTERNALSYM WSAENOPROTOOPT}
  WSAEPROTONOSUPPORT      = (WSABASEERR+43);
  {$EXTERNALSYM WSAEPROTONOSUPPORT}
  WSAESOCKTNOSUPPORT      = (WSABASEERR+44);
  {$EXTERNALSYM WSAESOCKTNOSUPPORT}
  WSAEOPNOTSUPP           = (WSABASEERR+45);
  {$EXTERNALSYM WSAEOPNOTSUPP}
  WSAEPFNOSUPPORT         = (WSABASEERR+46);
  {$EXTERNALSYM WSAEPFNOSUPPORT}
  WSAEAFNOSUPPORT         = (WSABASEERR+47);
  {$EXTERNALSYM WSAEAFNOSUPPORT}
  WSAEADDRINUSE           = (WSABASEERR+48);
  {$EXTERNALSYM WSAEADDRINUSE}
  WSAEADDRNOTAVAIL        = (WSABASEERR+49);
  {$EXTERNALSYM WSAEADDRNOTAVAIL}
  WSAENETDOWN             = (WSABASEERR+50);
  {$EXTERNALSYM WSAENETDOWN}
  WSAENETUNREACH          = (WSABASEERR+51);
  {$EXTERNALSYM WSAENETUNREACH}
  WSAENETRESET            = (WSABASEERR+52);
  {$EXTERNALSYM WSAENETRESET}
  WSAECONNABORTED         = (WSABASEERR+53);
  {$EXTERNALSYM WSAECONNABORTED}
  WSAECONNRESET           = (WSABASEERR+54);
  {$EXTERNALSYM WSAECONNRESET}
  WSAENOBUFS              = (WSABASEERR+55);
  {$EXTERNALSYM WSAENOBUFS}
  WSAEISCONN              = (WSABASEERR+56);
  {$EXTERNALSYM WSAEISCONN}
  WSAENOTCONN             = (WSABASEERR+57);
  {$EXTERNALSYM WSAENOTCONN}
  WSAESHUTDOWN            = (WSABASEERR+58);
  {$EXTERNALSYM WSAESHUTDOWN}
  WSAETOOMANYREFS         = (WSABASEERR+59);
  {$EXTERNALSYM WSAETOOMANYREFS}
  WSAETIMEDOUT            = (WSABASEERR+60);
  {$EXTERNALSYM WSAETIMEDOUT}
  WSAECONNREFUSED         = (WSABASEERR+61);
  {$EXTERNALSYM WSAECONNREFUSED}
  WSAELOOP                = (WSABASEERR+62);
  {$EXTERNALSYM WSAELOOP}
  WSAENAMETOOLONG         = (WSABASEERR+63);
  {$EXTERNALSYM WSAENAMETOOLONG}
  WSAEHOSTDOWN            = (WSABASEERR+64);
  {$EXTERNALSYM WSAEHOSTDOWN}
  WSAEHOSTUNREACH         = (WSABASEERR+65);
 {$EXTERNALSYM WSAEHOSTUNREACH}
  WSAENOTEMPTY            = (WSABASEERR+66);
  {$EXTERNALSYM WSAENOTEMPTY}
  WSAEPROCLIM             = (WSABASEERR+67);
  {$EXTERNALSYM WSAEPROCLIM}
  WSAEUSERS               = (WSABASEERR+68);
  {$EXTERNALSYM WSAEUSERS}
  WSAEDQUOT               = (WSABASEERR+69);
  {$EXTERNALSYM WSAEDQUOT}
  WSAESTALE               = (WSABASEERR+70);
  {$EXTERNALSYM WSAESTALE}
  WSAEREMOTE              = (WSABASEERR+71);
  {$EXTERNALSYM WSAEREMOTE}

{ Extended Windows Sockets error constant definitions }

  WSASYSNOTREADY          = (WSABASEERR+91);
  {$EXTERNALSYM WSASYSNOTREADY}
  WSAVERNOTSUPPORTED      = (WSABASEERR+92);
  {$EXTERNALSYM WSAVERNOTSUPPORTED}
  WSANOTINITIALISED       = (WSABASEERR+93);
  {$EXTERNALSYM WSANOTINITIALISED}
  WSAEDISCON              = (WSABASEERR+101);
  {$EXTERNALSYM WSAEDISCON}
  WSAENOMORE              = (WSABASEERR+102);
  {$EXTERNALSYM WSAENOMORE}
  WSAECANCELLED           = (WSABASEERR+103);
  {$EXTERNALSYM WSAECANCELLED}
  WSAEINVALIDPROCTABLE    = (WSABASEERR+104);
  {$EXTERNALSYM WSAEINVALIDPROCTABLE}
  WSAEINVALIDPROVIDER     = (WSABASEERR+105);
  {$EXTERNALSYM WSAEINVALIDPROVIDER}
  WSAEPROVIDERFAILEDINIT  = (WSABASEERR+106);
  {$EXTERNALSYM WSAEPROVIDERFAILEDINIT}
  WSASYSCALLFAILURE       = (WSABASEERR+107);
  {$EXTERNALSYM WSASYSCALLFAILURE}
  WSASERVICE_NOT_FOUND    = (WSABASEERR+108);
  {$EXTERNALSYM WSASERVICE_NOT_FOUND}
  WSATYPE_NOT_FOUND       = (WSABASEERR+109);
  {$EXTERNALSYM WSATYPE_NOT_FOUND}
  WSA_E_NO_MORE           = (WSABASEERR+110);
  {$EXTERNALSYM WSA_E_NO_MORE}
  WSA_E_CANCELLED         = (WSABASEERR+111);
  {$EXTERNALSYM WSA_E_CANCELLED}
  WSAEREFUSED             = (WSABASEERR+112);
  {$EXTERNALSYM WSAEREFUSED}

{ Error return codes from gethostbyname() and gethostbyaddr()
  (when using the resolver). Note that these errors are
  retrieved via WSAGetLastError() and must therefore follow
  the rules for avoiding clashes with error numbers from
  specific implementations or language run-time systems.
  For this reason the codes are based at WSABASEERR+1001.
  Note also that [WSA]NO_ADDRESS is defined only for
  compatibility purposes. }

{ Authoritative Answer: Host not found }

  WSAHOST_NOT_FOUND       = (WSABASEERR+1001);
  {$EXTERNALSYM WSAHOST_NOT_FOUND}
  HOST_NOT_FOUND          = WSAHOST_NOT_FOUND;
  {$EXTERNALSYM HOST_NOT_FOUND}

{ Non-Authoritative: Host not found, or SERVERFAIL }

  WSATRY_AGAIN            = (WSABASEERR+1002);
  {$EXTERNALSYM WSATRY_AGAIN}
  TRY_AGAIN               = WSATRY_AGAIN;
  {$EXTERNALSYM TRY_AGAIN}

{ Non recoverable errors, FORMERR, REFUSED, NOTIMP }

  WSANO_RECOVERY          = (WSABASEERR+1003);
  {$EXTERNALSYM WSANO_RECOVERY}
  NO_RECOVERY             = WSANO_RECOVERY;
  {$EXTERNALSYM NO_RECOVERY}

{ Valid name, no data record of requested type }

  WSANO_DATA              = (WSABASEERR+1004);
  {$EXTERNALSYM WSANO_DATA}
  NO_DATA                 = WSANO_DATA;
  {$EXTERNALSYM NO_DATA}

{ no address, look for MX record }

  WSANO_ADDRESS           = WSANO_DATA;
  {$EXTERNALSYM WSANO_ADDRESS}
  NO_ADDRESS              = WSANO_ADDRESS;
  {$EXTERNALSYM NO_ADDRESS}

{.$IFNDEF NO_WINSOCK_2}
{ Authoritative Answer: Host not found Securely }
  WSA_SECURE_HOST_NOT_FOUND  = (WSABASEERR+1032);
  {$EXTERNALSYM WSA_SECURE_HOST_NOT_FOUND}
{ Name based IPSEC policy could not be added }
  WSA_IPSEC_NAME_POLICY_ERROR = (WSABASEERR+1033);
  {$EXTERNALSYM WSA_IPSEC_NAME_POLICY_ERROR}
{.$ENDIF}

{ Windows Sockets errors redefined as regular Berkeley error constants.
  These are commented out in Windows NT to avoid conflicts with errno.h.
  Use the WSA constants instead. }

  EWOULDBLOCK        =  WSAEWOULDBLOCK;
  {$EXTERNALSYM EWOULDBLOCK}
  EINPROGRESS        =  WSAEINPROGRESS;
  {$EXTERNALSYM EINPROGRESS}
  EALREADY           =  WSAEALREADY;
  {$EXTERNALSYM EALREADY}
  ENOTSOCK           =  WSAENOTSOCK;
  {$EXTERNALSYM ENOTSOCK}
  EDESTADDRREQ       =  WSAEDESTADDRREQ;
  {$EXTERNALSYM EDESTADDRREQ}
  EMSGSIZE           =  WSAEMSGSIZE;
  {$EXTERNALSYM EMSGSIZE}
  EPROTOTYPE         =  WSAEPROTOTYPE;
  {$EXTERNALSYM EPROTOTYPE}
  ENOPROTOOPT        =  WSAENOPROTOOPT;
  {$EXTERNALSYM ENOPROTOOPT}
  EPROTONOSUPPORT    =  WSAEPROTONOSUPPORT;
  {$EXTERNALSYM EPROTONOSUPPORT}
  ESOCKTNOSUPPORT    =  WSAESOCKTNOSUPPORT;
  {$EXTERNALSYM ESOCKTNOSUPPORT}
  EOPNOTSUPP         =  WSAEOPNOTSUPP;
  {$EXTERNALSYM EOPNOTSUPP}
  EPFNOSUPPORT       =  WSAEPFNOSUPPORT;
  {$EXTERNALSYM EPFNOSUPPORT}
  EAFNOSUPPORT       =  WSAEAFNOSUPPORT;
  {$EXTERNALSYM EAFNOSUPPORT}
  EADDRINUSE         =  WSAEADDRINUSE;
  {$EXTERNALSYM EADDRINUSE}
  EADDRNOTAVAIL      =  WSAEADDRNOTAVAIL;
  {$EXTERNALSYM EADDRNOTAVAIL}
  ENETDOWN           =  WSAENETDOWN;
  {$EXTERNALSYM ENETDOWN}
  ENETUNREACH        =  WSAENETUNREACH;
  {$EXTERNALSYM ENETUNREACH}
  ENETRESET          =  WSAENETRESET;
  {$EXTERNALSYM ENETRESET}
  ECONNABORTED       =  WSAECONNABORTED;
  {$EXTERNALSYM ECONNABORTED}
  ECONNRESET         =  WSAECONNRESET;
  {$EXTERNALSYM ECONNRESET}
  ENOBUFS            =  WSAENOBUFS;
  {$EXTERNALSYM ENOBUFS}
  EISCONN            =  WSAEISCONN;
  {$EXTERNALSYM EISCONN}
  ENOTCONN           =  WSAENOTCONN;
  {$EXTERNALSYM ENOTCONN}
  ESHUTDOWN          =  WSAESHUTDOWN;
  {$EXTERNALSYM ESHUTDOWN}
  ETOOMANYREFS       =  WSAETOOMANYREFS;
  {$EXTERNALSYM ETOOMANYREFS}
  ETIMEDOUT          =  WSAETIMEDOUT;
  {$EXTERNALSYM ETIMEDOUT}
  ECONNREFUSED       =  WSAECONNREFUSED;
  {$EXTERNALSYM ECONNREFUSED}
  ELOOP              =  WSAELOOP;
  {$EXTERNALSYM ELOOP}
  ENAMETOOLONG       =  WSAENAMETOOLONG;
  {$EXTERNALSYM ENAMETOOLONG}
  EHOSTDOWN          =  WSAEHOSTDOWN;
  {$EXTERNALSYM EHOSTDOWN}
  EHOSTUNREACH       =  WSAEHOSTUNREACH;
 {$EXTERNALSYM EHOSTUNREACH}
  ENOTEMPTY          =  WSAENOTEMPTY;
  {$EXTERNALSYM ENOTEMPTY}
  EPROCLIM           =  WSAEPROCLIM;
  {$EXTERNALSYM EPROCLIM}
  EUSERS             =  WSAEUSERS;
  {$EXTERNALSYM EUSERS}
  EDQUOT             =  WSAEDQUOT;
  {$EXTERNALSYM EDQUOT}
  ESTALE             =  WSAESTALE;
  {$EXTERNALSYM ESTALE}
  EREMOTE            =  WSAEREMOTE;
  {$EXTERNALSYM EREMOTE}

{ WinSock 2 extension -- new error codes and type definition }

type
  WSAEVENT                      = THandle;
  {$EXTERNALSYM WSAEVENT}
  LPWSAEVENT                    = PHandle;
  {$EXTERNALSYM LPWSAEVENT}
  WSAOVERLAPPED                 = OVERLAPPED;
  {$EXTERNALSYM WSAOVERLAPPED}

const
  WSA_IO_PENDING                = (ERROR_IO_PENDING);
  {$EXTERNALSYM WSA_IO_PENDING}
  WSA_IO_INCOMPLETE             = (ERROR_IO_INCOMPLETE);
  {$EXTERNALSYM WSA_IO_INCOMPLETE}
  WSA_INVALID_HANDLE            = (ERROR_INVALID_HANDLE);
  {$EXTERNALSYM WSA_INVALID_HANDLE}
  WSA_INVALID_PARAMETER         = (ERROR_INVALID_PARAMETER);
  {$EXTERNALSYM WSA_INVALID_PARAMETER}
  WSA_NOT_ENOUGH_MEMORY         = (ERROR_NOT_ENOUGH_MEMORY);
  {$EXTERNALSYM WSA_NOT_ENOUGH_MEMORY}
  WSA_OPERATION_ABORTED         = (ERROR_OPERATION_ABORTED);
  {$EXTERNALSYM WSA_OPERATION_ABORTED}

  WSA_INVALID_EVENT             = WSAEVENT(nil);
  {$EXTERNALSYM WSA_INVALID_EVENT}
  WSA_MAXIMUM_WAIT_EVENTS       = (MAXIMUM_WAIT_OBJECTS);
  {$EXTERNALSYM WSA_MAXIMUM_WAIT_EVENTS}
  WSA_WAIT_FAILED               = (WAIT_FAILED);
  {$EXTERNALSYM WSA_WAIT_FAILED}
  WSA_WAIT_EVENT_0              = (WAIT_OBJECT_0);
  {$EXTERNALSYM WSA_WAIT_EVENT_0}
  WSA_WAIT_IO_COMPLETION        = (WAIT_IO_COMPLETION);
  {$EXTERNALSYM WSA_WAIT_IO_COMPLETION}
  WSA_WAIT_TIMEOUT              = (WAIT_TIMEOUT);
  {$EXTERNALSYM WSA_WAIT_TIMEOUT}
  WSA_INFINITE                  = (INFINITE);
  {$EXTERNALSYM WSA_INFINITE}

type
  PInAddr6 = ^in_addr6;
  in_addr6 = record
    case integer of
      0: (S6_addr: array [0..15] of Byte);
      1: (u6_addr8: array [0..15] of Byte);
      2: (u6_addr16: array [0..7] of Word);
      3: (u6_addr32: array [0..3] of Integer);
  end;
  {$EXTERNALSYM in_addr6}
  TInAddr6 = in_addr6;

  PSockAddrIn6 = ^sockaddr_in6;
  sockaddr_in6 = record
    sin6_family:   u_short;     // AF_INET6
    sin6_port:     u_short;     // Transport level port number
    sin6_flowinfo: u_long;      // IPv6 flow information
    sin6_addr:     TInAddr6;    // IPv6 address
{$IFDEF STILL_NEEDS_CHECK}
    case integer of
      0: (sin6_scope_id: u_long);      // Set of interfaces for a scope.
      1: (sin6_scope_struct: SCOPE_ID);
{$ELSE}
    sin6_scope_id: u_long;      // Scope Id: IF number for link-local SITE id for site-local
{$ENDIF}
  end;
  {$EXTERNALSYM sockaddr_in6}
  TSockAddrIn6 = sockaddr_in6;

  PIPV6_MREQ = ^ipv6_mreq;
  {$EXTERNALSYM PIPV6_MREQ}
  ipv6_mreq = record
    ipv6mr_multiaddr: TInAddr6; // IPv6 multicast address.
    ipv6mr_interface: u_long;   // Interface index.
  end;
  {$EXTERNALSYM ipv6_mreq}
  TIPv6MReq = ipv6_mreq;
  PIPv6MReq = PIPV6_MREQ;

  PADDRINFOA = ^addrinfo;
  {$EXTERNALSYM PADDRINFOA}
  addrinfo = record
    ai_flags        : Integer;      // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    ai_family       : Integer;      // PF_xxx
    ai_socktype     : Integer;      // SOCK_xxx
    ai_protocol     : Integer;      // 0 or IPPROTO_xxx for IPv4 and IPv6
    ai_addrlen      : u_int;        // Length of ai_addr
    ai_canonname    : PAnsiChar;    // Canonical name for nodename
    ai_addr         : PSOCKADDR;    // Binary address
    ai_next         : PADDRINFOA;   // Next structure in linked list
  end;
  {$EXTERNALSYM addrinfo}
  ADDRINFOA = addrinfo;
  {$EXTERNALSYM ADDRINFOA}
  TAddrInfoA = ADDRINFOA;

  PADDRINFOW = ^addrinfoW;
  {$EXTERNALSYM PADDRINFOW}
  addrinfoW  = record
    ai_flags        : Integer;      // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    ai_family       : Integer;      // PF_xxx
    ai_socktype     : Integer;      // SOCK_xxx
    ai_protocol     : Integer;      // 0 or IPPROTO_xxx for IPv4 and IPv6
    ai_addrlen      : u_int;        // Length of ai_addr
    ai_canonname    : PWideChar;    // Canonical name for nodename
    ai_addr         : PSOCKADDR;    // Binary address
    ai_next         : PADDRINFOW;   // Next structure in linked list
  end;
  {$EXTERNALSYM addrinfoW}
  TAddrInfoW = addrinfoW;

{$IFDEF UNICODE}
  PAddrInfo = PADDRINFOW;
  TAddrInfo = TAddrInfoW;
{$ELSE}
  PAddrInfo = PADDRINFOA;
  TAddrInfo = TAddrInfoA;
{$ENDIF}

const
  AI_PASSIVE            = $1;   // Socket address will be used in bind() call
  {$EXTERNALSYM AI_PASSIVE}
  AI_CANONNAME          = $2;   // Return canonical name in first ai_canonname
  {$EXTERNALSYM AI_CANONNAME}
  AI_NUMERICHOST        = $4;   // Nodename must be a numeric address string
  {$EXTERNALSYM AI_NUMERICHOST}

  // Error codes from getaddrinfo().

  EAI_AGAIN             = WSATRY_AGAIN;
  {$EXTERNALSYM EAI_AGAIN}
  EAI_BADFLAGS          = WSAEINVAL;
  {$EXTERNALSYM EAI_BADFLAGS}
  EAI_FAIL              = WSANO_RECOVERY;
  {$EXTERNALSYM EAI_FAIL}
  EAI_FAMILY            = WSAEAFNOSUPPORT;
  {$EXTERNALSYM EAI_FAMILY}
  EAI_MEMORY            = WSA_NOT_ENOUGH_MEMORY;
  {$EXTERNALSYM EAI_MEMORY}
  EAI_NOSECURENAME      = WSA_SECURE_HOST_NOT_FOUND;
  {$EXTERNALSYM EAI_NOSECURENAME}
  EAI_NODATA            = WSANO_DATA;
  {$EXTERNALSYM EAI_NODATA}
  EAI_NONAME            = WSAHOST_NOT_FOUND;
  {$EXTERNALSYM EAI_NONAME}
  EAI_SERVICE           = WSATYPE_NOT_FOUND;
  {$EXTERNALSYM EAI_SERVICE}
  EAI_SOCKTYPE          = WSAESOCKTNOSUPPORT;
  {$EXTERNALSYM EAI_SOCKTYPE}
  EAI_IPSECPOLICY       = WSA_IPSEC_NAME_POLICY_ERROR;
  {$EXTERNALSYM EAI_IPSECPOLICY}

//
// Flags for getnameinfo()
//

  NI_NOFQDN               = $01;  // Only return nodename portion for local hosts
  {$EXTERNALSYM NI_NOFQDN}
  NI_NUMERICHOST          = $02;  // Return numeric form of the host's address
  {$EXTERNALSYM NI_NUMERICHOST}
  NI_NAMEREQD             = $04;  // Error if the host's name not in DNS
  {$EXTERNALSYM NI_NAMEREQD}
  NI_NUMERICSERV          = $08;  // Return numeric form of the service (port #)
  {$EXTERNALSYM NI_NUMERICSERV}
  NI_DGRAM                = $10;  // Service is a datagram service
  {$EXTERNALSYM NI_DGRAM}

  NI_MAXHOST              = 1025; // Max size of a fully-qualified domain name.
  {$EXTERNALSYM NI_MAXHOST}
  NI_MAXSERV              = 32;   // Max size of a service name.
  {$EXTERNALSYM NI_MAXSERV}

type
  sockaddr = record
     sa_family   : u_short;                    // address family
     sa_data     : array [0..13] of AnsiChar;  // up to 14 bytes of direct address
  end;
  {$EXTERNALSYM sockaddr}

  IN6_ADDR = record
    case Integer of
        0: (Byte     : array [0..15] of u_char);
        1: (Word     : array [0..7]  of u_short);
        2: (s6_bytes : array [0..15] of Byte);
        3: (s6_addr  : array [0..15] of Byte);
        4: (s6_words : array [0..7]  of u_short);
  end;
  {$EXTERNALSYM IN6_ADDR}
  PIN6_ADDR  = ^IN6_ADDR;
  {$EXTERNALSYM PIN6_ADDR}
  TIn6Addr   = IN6_ADDR;
  PIn6Addr   = ^TIn6Addr;

var
  in6addr_any: TIn6Addr;
  {$EXTERNALSYM in6addr_any}
  in6addr_loopback: TIn6Addr;
  {$EXTERNALSYM in6addr_loopback}
{$IFDEF STILL_NEEDS_CHECK}
  in6addr_v4mappedprefix: TIn6Addr;
  {$EXTERNALSYM in6addr_v4mappedprefix}
{$ENDIF}
const
//
// Options to use with [gs]etsockopt at the IPPROTO_IPV6 level.
// These are specified in RFCs 3493 and 3542.
// The values should be consistent with the IPv6 equivalents.
//
  IPV6_HOPOPTS                        = 1;  // Set/get IPv6 hop-by-hop options.
  {$EXTERNALSYM IPV6_HOPOPTS}
  IPV6_HDRINCL                        = 2;  // Header is included with data.
  {$EXTERNALSYM IPV6_HDRINCL}
  IPV6_UNICAST_HOPS                   = 4;  // IP unicast hop limit.
  {$EXTERNALSYM IPV6_UNICAST_HOPS}
  IPV6_MULTICAST_IF                   = 9;  // IP multicast interface.
  {$EXTERNALSYM IPV6_MULTICAST_IF}
  IPV6_MULTICAST_HOPS                 = 10;  // IP multicast hop limit.
  {$EXTERNALSYM IPV6_MULTICAST_HOPS}
  IPV6_MULTICAST_LOOP                 = 11;  // IP multicast loopback.
  {$EXTERNALSYM IPV6_MULTICAST_LOOP}
  IPV6_ADD_MEMBERSHIP                 = 12;  // Add an IP group membership.
  {$EXTERNALSYM IPV6_ADD_MEMBERSHIP}
  IPV6_JOIN_GROUP                     = IPV6_ADD_MEMBERSHIP;
  {$EXTERNALSYM IPV6_JOIN_GROUP}
  IPV6_DROP_MEMBERSHIP                = 13;  // Drop an IP group membership.
  {$EXTERNALSYM IPV6_DROP_MEMBERSHIP}
  IPV6_LEAVE_GROUP                    = IPV6_DROP_MEMBERSHIP;
  {$EXTERNALSYM IPV6_LEAVE_GROUP}
  IPV6_DONTFRAG                       = 14;  // Don't fragment IP datagrams.
  {$EXTERNALSYM IPV6_DONTFRAG}
  IPV6_PKTINFO                        = 19;  // Receive packet information.
  {$EXTERNALSYM IPV6_PKTINFO}
  IPV6_HOPLIMIT                       = 21;  // Receive packet hop limit.
  {$EXTERNALSYM IPV6_HOPLIMIT}
  IPV6_PROTECTION_LEVEL               = 23;  // Set/get IPv6 protection level.
  {$EXTERNALSYM IPV6_PROTECTION_LEVEL}
  IPV6_RECVIF                         = 24;  // Receive arrival interface.
  {$EXTERNALSYM IPV6_RECVIF}
  IPV6_RECVDSTADDR                    = 25;  // Receive destination address.
  {$EXTERNALSYM IPV6_RECVDSTADDR}
  IPV6_CHECKSUM                       = 26;  // Offset to checksum for raw IP socket send.
  {$EXTERNALSYM IPV6_CHECKSUM}
  IPV6_V6ONLY                         = 27;  // Treat wildcard bind as AF_INET6-only.
  {$EXTERNALSYM IPV6_V6ONLY}
  IPV6_IFLIST                         = 28;  // Enable/Disable an interface list.
  {$EXTERNALSYM IPV6_IFLIST}
  IPV6_ADD_IFLIST                     = 29;  // Add an interface list entry.
  {$EXTERNALSYM IPV6_ADD_IFLIST}
  IPV6_DEL_IFLIST                     = 30;  // Delete an interface list entry.
  {$EXTERNALSYM IPV6_DEL_IFLIST}
  IPV6_UNICAST_IF                     = 31;  // IP unicast interface.
  {$EXTERNALSYM IPV6_UNICAST_IF}
  IPV6_RTHDR                          = 32;  // Set/get IPv6 routing header.
  {$EXTERNALSYM IPV6_RTHDR}
  IPV6_RECVRTHDR                      = 38;  // Receive the routing header.
  {$EXTERNALSYM IPV6_RECVRTHDR}
  IPV6_TCLASS                         = 39;  // Packet traffic class.
  {$EXTERNALSYM IPV6_TCLASS}
  IPV6_RECVTCLASS                     = 40;  // Receive packet traffic class.
  {$EXTERNALSYM IPV6_RECVTCLASS}

  IP_UNSPECIFIED_HOP_LIMIT            = - 1;
  {$EXTERNALSYM IP_UNSPECIFIED_HOP_LIMIT}


{$EXTERNALSYM IN6ADDR_ANY_INIT}  
function  IN6ADDR_ANY_INIT: TIn6Addr; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6ADDR_LOOPBACK_INIT} 
function  IN6ADDR_LOOPBACK_INIT: TIn6Addr; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6ADDR_SETANY} 
procedure IN6ADDR_SETANY(sa: PSockAddrIn6); {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6ADDR_SETLOOPBACK} 
procedure IN6ADDR_SETLOOPBACK(sa: PSockAddrIn6); {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6ADDR_ISANY} 
function  IN6ADDR_ISANY(sa: PSockAddrIn6): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6ADDR_ISLOOPBACK} 
function  IN6ADDR_ISLOOPBACK(sa: PSockAddrIn6): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6_ADDR_EQUAL}
function  IN6_ADDR_EQUAL(const a: PIn6Addr; const b: PIn6Addr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6_IS_ADDR_UNSPECIFIED} 
function  IN6_IS_ADDR_UNSPECIFIED(const a: PIn6Addr): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6_IS_ADDR_LOOPBACK} 
function  IN6_IS_ADDR_LOOPBACK(const a: PIn6Addr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6_IS_ADDR_MULTICAST} 
function  IN6_IS_ADDR_MULTICAST(const a: PIn6Addr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6_IS_ADDR_LINKLOCAL} 
function  IN6_IS_ADDR_LINKLOCAL(const a: PIn6Addr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6_IS_ADDR_SITELOCAL}
function  IN6_IS_ADDR_SITELOCAL(const a: PIn6Addr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF STILL_NEEDS_CHECK}
{$EXTERNALSYM IN6_IS_ADDR_GLOBAL}
function IN6_IS_ADDR_GLOBAL(const a: PIn6Addr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
{$EXTERNALSYM IN6_IS_ADDR_V4MAPPED} 
function  IN6_IS_ADDR_V4MAPPED(const a: PIn6Addr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6_IS_ADDR_V4COMPAT} 
function  IN6_IS_ADDR_V4COMPAT(const a: PIn6Addr): Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6_IS_ADDR_MC_NODELOCAL} 
function  IN6_IS_ADDR_MC_NODELOCAL(const a: PIn6Addr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6_IS_ADDR_MC_LINKLOCAL} 
function  IN6_IS_ADDR_MC_LINKLOCAL(const a: PIn6Addr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6_IS_ADDR_MC_SITELOCAL} 
function  IN6_IS_ADDR_MC_SITELOCAL(const a: PIn6Addr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6_IS_ADDR_MC_ORGLOCAL} 
function  IN6_IS_ADDR_MC_ORGLOCAL(const a: PIn6Addr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6_IS_ADDR_MC_GLOBAL}
function  IN6_IS_ADDR_MC_GLOBAL(const a: PIn6Addr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Microsoft-specific IPv4 definitions. }
{$IFDEF STILL_NEEDS_CHECK}
function IN4_CLASSA(a: u_long): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN4_CLASSA}
function IN4_CLASSB(a: u_long): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN4_CLASSB}
function IN4_CLASSC(a: u_long): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN4_CLASSC}
function IN4_CLASSD(a: u_long): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN4_CLASSD}
function IN4_MULTICAST(a: u_long): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN4_MULTICAST}
function IN4_IS_ADDR_BROADCAST(const a: PInAddr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN4_IS_ADDR_BROADCAST}
function IN4_IS_ADDR_MULTICAST(const a: PInAddr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN4_IS_ADDR_MULTICAST}
function IN4_IS_ADDR_MC_LINKLOCAL(const a: PInAddr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN4_IS_ADDR_MC_LINKLOCAL}
function IN4_IS_ADDR_MC_ADMINLOCAL(const a: PInAddr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN4_IS_ADDR_MC_ADMINLOCAL}
function IN4_IS_ADDR_MC_SITELOCAL(const a: PInAddr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN4_IS_ADDR_MC_SITELOCAL}
function IN4_IS_ADDR_LINKLOCAL(const a: PInAddr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN4_IS_ADDR_LINKLOCAL}
function IN4_IS_ADDR_LOOPBACK(const a: PInAddr): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN4_IS_ADDR_LOOPBACK}
function Ipv4UnicastAddressScope(const Address: PAnsiChar): TScopeLevel; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM Ipv4UnicastAddressScope}
function Ipv4MulticastAddressScope(const Address: PAnsiChar): TScopeLevel; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM Ipv4MulticastAddressScope}
function Ipv4AddressScope(const Address: PAnsiChar): TScopeLevel; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM Ipv4AddressScope}
procedure IN4_UNCANONICALIZE_SCOPE_ID(const Address: PInAddr; ScopeId: PScopeID); {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN4_UNCANONICALIZE_SCOPE_ID}
procedure IN6ADDR_V4MAPPEDPREFIX_INIT(a6: PIn6Addr); {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6ADDR_V4MAPPEDPREFIX_INIT}
procedure IN6_SET_ADDR_V4MAPPED(a6: PIn6Addr; const a4: PInAddr); {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6_SET_ADDR_V4MAPPED}
procedure IN6ADDR_SETV4MAPPED(a6: PSockAddrIn6; const a4: PInAddr; scope: SCOPE_ID; port: u_short); {$IFDEF USE_INLINE}inline;{$ENDIF}
{$EXTERNALSYM IN6ADDR_SETV4MAPPED}
{$ENDIF}

{*
 * WinSock 2 extension -- manifest constants for WSAIoctl()
 *}

const
    IOC_UNIX             = $00000000;       { Do not use this in Windows     }
{$EXTERNALSYM IOC_UNIX}
    IOC_WS2              = $08000000;
{$EXTERNALSYM IOC_WS2}
    IOC_PROTOCOL         = $10000000;
{$EXTERNALSYM IOC_PROTOCOL}
    IOC_VENDOR           = $18000000;
{$EXTERNALSYM IOC_VENDOR}
    SIO_RCVALL           = IOC_IN or IOC_VENDOR or 1;
{$EXTERNALSYM SIO_RCVALL}
    SIO_RCVALL_MCAST     = IOC_IN or IOC_VENDOR or 2;
{$EXTERNALSYM SIO_RCVALL_MCAST}
    SIO_RCVALL_IGMPMCAST = IOC_IN or IOC_VENDOR or 3;
{$EXTERNALSYM SIO_RCVALL_IGMPMCAST}
    SIO_KEEPALIVE_VALS   = IOC_IN or IOC_VENDOR or 4;
{$EXTERNALSYM SIO_KEEPALIVE_VALS}
    SIO_ABSORB_RTRALERT  = IOC_IN or IOC_VENDOR or 5;
{$EXTERNALSYM SIO_ABSORB_RTRALERT}
    SIO_UCAST_IF         = IOC_IN or IOC_VENDOR or 6;
{$EXTERNALSYM SIO_UCAST_IF}
    SIO_LIMIT_BROADCASTS = IOC_IN or IOC_VENDOR or 7;
{$EXTERNALSYM SIO_LIMIT_BROADCASTS}
    SIO_INDEX_BIND       = IOC_IN or IOC_VENDOR or 8;
{$EXTERNALSYM SIO_INDEX_BIND}
    SIO_INDEX_MCASTIF    = IOC_IN or IOC_VENDOR or 9;
{$EXTERNALSYM SIO_INDEX_MCASTIF}
    SIO_INDEX_ADD_MCAST  = IOC_IN or IOC_VENDOR or 10;
{$EXTERNALSYM SIO_INDEX_ADD_MCAST}
    SIO_INDEX_DEL_MCAST  = IOC_IN or IOC_VENDOR or 11;
{$EXTERNALSYM SIO_INDEX_DEL_MCAST}
    SIO_GET_INTERFACE_LIST = IOC_OUT or ((SizeOf(u_long) and IOCPARM_MASK) shl 16) or (Ord('t') shl 8) or 127;
{$EXTERNALSYM SIO_GET_INTERFACE_LIST}

//#if (_WIN32_WINNT >= 0x0600)
{*
 * WSK-specific IO control codes are Winsock2 codes with the highest-order
 * 3 bits of the Vendor/AddressFamily-specific field set to 1.
 *}
  //{$EXTERNALSYM IOC_WSK}
  //IOC_WSK                             = IOC_WS2 or $07000000;
//#endif //(_WIN32_WINNT >= 0x0600)
{
#define _WSAIO(x,y)                   (IOC_VOID|(x)|(y))
#define _WSAIOR(x,y)                  (IOC_OUT|(x)|(y))
#define _WSAIOW(x,y)                  (IOC_IN|(x)|(y))
#define _WSAIORW(x,y)                 (IOC_INOUT|(x)|(y))
}
  SIO_ASSOCIATE_HANDLE                = IOC_IN or IOC_WS2 or 1;
  {$EXTERNALSYM SIO_ASSOCIATE_HANDLE}
  SIO_ENABLE_CIRCULAR_QUEUEING        = IOC_VOID or IOC_WS2 or 2;
  {$EXTERNALSYM SIO_ENABLE_CIRCULAR_QUEUEING}
  SIO_FIND_ROUTE                      = IOC_OUT or IOC_WS2 or 3;
  {$EXTERNALSYM SIO_FIND_ROUTE}
  SIO_FLUSH                           = IOC_VOID or IOC_WS2 or 4;
  {$EXTERNALSYM SIO_FLUSH}
  SIO_GET_BROADCAST_ADDRESS           = IOC_OUT or IOC_WS2 or 5;
  {$EXTERNALSYM SIO_GET_BROADCAST_ADDRESS}
  SIO_GET_EXTENSION_FUNCTION_POINTER  = IOC_INOUT or IOC_WS2 or 6;
  {$EXTERNALSYM SIO_GET_EXTENSION_FUNCTION_POINTER}
  SIO_GET_QOS                         = IOC_INOUT or IOC_WS2 or 7;
  {$EXTERNALSYM SIO_GET_QOS}
  SIO_GET_GROUP_QOS                   = IOC_INOUT or IOC_WS2 or 8;
  {$EXTERNALSYM SIO_GET_GROUP_QOS}
  SIO_MULTIPOINT_LOOPBACK             = IOC_IN or IOC_WS2 or 9;
  {$EXTERNALSYM SIO_MULTIPOINT_LOOPBACK}
  SIO_MULTICAST_SCOPE                 = IOC_IN or IOC_WS2 or 10;
  {$EXTERNALSYM SIO_MULTICAST_SCOPE}
  SIO_SET_QOS                         = IOC_IN or IOC_WS2 or 11;
  {$EXTERNALSYM SIO_SET_QOS}
  SIO_SET_GROUP_QOS                   = IOC_IN or IOC_WS2 or 12;
  {$EXTERNALSYM SIO_SET_GROUP_QOS}
  SIO_TRANSLATE_HANDLE                = IOC_INOUT or IOC_WS2 or 13;
  {$EXTERNALSYM SIO_TRANSLATE_HANDLE}
  SIO_ROUTING_INTERFACE_QUERY         = IOC_INOUT or IOC_WS2 or 20;
  {$EXTERNALSYM SIO_ROUTING_INTERFACE_QUERY}
  SIO_ROUTING_INTERFACE_CHANGE        = IOC_IN or IOC_WS2 or 21;
  {$EXTERNALSYM SIO_ROUTING_INTERFACE_CHANGE}
  SIO_ADDRESS_LIST_QUERY              = IOC_OUT or IOC_WS2 or 22;
  {$EXTERNALSYM SIO_ADDRESS_LIST_QUERY}
  SIO_ADDRESS_LIST_CHANGE             = IOC_VOID or IOC_WS2 or 23;
  {$EXTERNALSYM SIO_ADDRESS_LIST_CHANGE}
  SIO_QUERY_TARGET_PNP_HANDLE         = IOC_OUT or IOC_WS2 or 24;
  {$EXTERNALSYM SIO_QUERY_TARGET_PNP_HANDLE}


function WSAStartup(wVersionRequested: WORD; var lpWSAData: TWSAData): Integer; stdcall;
{$EXTERNALSYM WSAStartup}
function WSACleanup: Integer; stdcall;
{$EXTERNALSYM WSACleanup}
procedure WSASetLastError(iError: Integer); stdcall;
{$EXTERNALSYM WSASetLastError}
function WSAGetLastError: Integer; stdcall;
{$EXTERNALSYM WSAGetLastError}
function WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer; stdcall;
{$EXTERNALSYM WSACancelAsyncRequest}
function WSAAsyncGetHostByName(HWindow: HWND; wMsg: u_int; name, buf: PAnsiChar;
                               buflen: Integer): THandle; stdcall;
{$EXTERNALSYM WSAAsyncGetHostByName}
function WSAAsyncGetHostByAddr(HWindow: HWND; wMsg: u_int; addr: PAnsiChar;
               len, Struct: Integer; buf: PAnsiChar; buflen: Integer): THandle; stdcall;
{$EXTERNALSYM WSAAsyncGetHostByAddr}
function WSAAsyncSelect(s: TSocket; HWindow: HWND; wMsg: u_int; lEvent: Longint): Integer; stdcall;
{$EXTERNALSYM WSAAsyncSelect}
function getservbyname(name, proto: PAnsiChar): PServEnt; stdcall;
{$EXTERNALSYM getservbyname}
function getprotobyname(name: PAnsiChar): PProtoEnt; stdcall;
{$EXTERNALSYM getprotobyname}
function gethostbyname(name: PAnsiChar): PHostEnt; stdcall;
{$EXTERNALSYM gethostbyname}
function gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt; stdcall;
{$EXTERNALSYM gethostbyaddr}
function gethostName(name: PAnsiChar; len: Integer): Integer; stdcall;
{$EXTERNALSYM gethostName}
function socket(af, Struct, protocol: Integer): TSocket; stdcall;
{$EXTERNALSYM socket}
function shutdown(s: TSocket; how: Integer): Integer; stdcall;
{$EXTERNALSYM shutdown}
function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar;
                    optlen: Integer): Integer; stdcall;
{$EXTERNALSYM setsockopt}
function getsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar;
                    var optlen: Integer): Integer; stdcall;
{$EXTERNALSYM getsockopt}
function sendto(s: TSocket; var Buf; len, flags: Integer; var addrto: TSockAddr;
                tolen: Integer): Integer; stdcall;
{$EXTERNALSYM sendto}
function send(s: TSocket; var Buf; len, flags: Integer): Integer; stdcall;
{$EXTERNALSYM send}
function recv(s: TSocket; var Buf; len, flags: Integer): Integer; stdcall;
{$EXTERNALSYM recv}
function recvfrom(s: TSocket; var Buf; len, flags: Integer; var from: TSockAddr;
                  var fromlen: Integer): Integer; stdcall;
{$EXTERNALSYM recvfrom}
function ntohs(netshort: u_short): u_short; stdcall;
{$EXTERNALSYM ntohs}
function ntohl(netlong: u_long): u_long; stdcall;
{$EXTERNALSYM ntohl}
function listen(s: TSocket; backlog: Integer): Integer; stdcall;
{$EXTERNALSYM listen}
function ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer; stdcall;
{$EXTERNALSYM ioctlsocket}
function WSAIoctl(s                 : TSocket;
                  IoControlCode     : DWORD;
                  InBuffer          : Pointer;
                  InBufferSize      : DWORD;
                  OutBuffer         : Pointer;
                  OutBufferSize     : DWORD;
                  var BytesReturned : DWORD;
                  Overlapped        : POverlapped;
                  CompletionRoutine : FARPROC): Integer; stdcall;
{$EXTERNALSYM WSAIoctl}

function inet_ntoa(inaddr: TInAddr): PAnsiChar; stdcall;
{$EXTERNALSYM inet_ntoa}
function inet_addr(cp: PAnsiChar): u_long; stdcall;
{$EXTERNALSYM inet_addr}
function htons(hostshort: u_short): u_short; stdcall;
{$EXTERNALSYM htons}
function htonl(hostlong: u_long): u_long; stdcall;
{$EXTERNALSYM htonl}
function getsockname(s: TSocket; var name: TSockAddr; var namelen: Integer): Integer; stdcall;
{$EXTERNALSYM getsockname}
function getpeername(s: TSocket; var name: TSockAddr; var namelen: Integer): Integer; stdcall;
{$EXTERNALSYM getpeername}
function connect(s: TSocket; var name: TSockAddr; namelen: Integer): Integer; stdcall;
{$EXTERNALSYM connect}
function closesocket(s: TSocket): Integer; stdcall;
{$EXTERNALSYM closesocket}
function bind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer; stdcall;
{$EXTERNALSYM bind}
function accept(s: TSocket; addr: PSockAddr; addrlen: PInteger): TSocket; stdcall;
{$EXTERNALSYM accept}

//#if (NTDDI_VERSION >= NTDDI_WINXPSP2) || (_WIN32_WINNT >= 0x0502)
function GetAddrInfoA(NodeName: PAnsiChar; ServName: PAnsiChar; Hints: PADDRINFOA;
                     var AddrInfo: PADDRINFOA): Integer; stdcall;
{$EXTERNALSYM GetAddrInfoA}

function GetAddrInfoW(NodeName: PWideChar; ServName: PWideChar; Hints: PADDRINFOW;
                     var AddrInfo: PADDRINFOW): Integer; stdcall;
{$IFDEF UNICODE}
function GetAddrInfo(NodeName: PWideChar; ServName: PWideChar; Hints: PADDRINFOW;
                     var AddrInfo: PADDRINFOW): Integer; stdcall;
{$ELSE}
function GetAddrInfo(NodeName: PAnsiChar; ServName: PAnsiChar; Hints: PADDRINFOA;
                     var AddrInfo: PADDRINFOA): Integer; stdcall;
{$ENDIF}
{$EXTERNALSYM GetAddrInfo}

procedure FreeAddrInfoA(ai: PADDRINFOA); stdcall;
{$EXTERNALSYM FreeAddrInfoA}
procedure FreeAddrInfoW(ai: PADDRINFOW); stdcall;
{$EXTERNALSYM FreeAddrInfoW}
{$IFDEF UNICODE}
procedure FreeAddrInfo(ai: PADDRINFOW); stdcall;
{$ELSE}
procedure FreeAddrInfo(ai: PADDRINFOA); stdcall;
{$ENDIF}
{$EXTERNALSYM FreeAddrInfo}

function GetNameInfoA(addr: PSockAddr; namelen: Integer; host: PAnsiChar;
   hostlen: DWORD; serv: PAnsiChar; servlen: DWORD; flags: Integer): Integer; stdcall;
{$EXTERNALSYM GetNameInfoA}
function GetNameInfoW(addr: PSockAddr; namelen: Integer; host: PWideChar;
   hostlen: DWORD; serv: PWideChar; servlen: DWORD; flags: Integer): Integer; stdcall;
{$EXTERNALSYM GetNameInfoW}
{$IFDEF UNICODE}
function GetNameInfo(addr: PSockAddr; namelen: Integer; host: PWideChar;
   hostlen: DWORD; serv: PWideChar; servlen: DWORD; flags: Integer): Integer; stdcall;
{$ELSE}
function GetNameInfo(addr: PSockAddr; namelen: Integer; host: PAnsiChar;
   hostlen: DWORD; serv: PAnsiChar; servlen: DWORD; flags: Integer): Integer; stdcall;
{$ENDIF}
{$EXTERNALSYM GetNameInfo}

{ Called by OverbyteIcsWSocket.pas in order to enable dynamic DLL loading with }
{ BCB as well as in previous ICS versions.                                     }
function Ics_WSAStartup(wVersionRequested: WORD; var lpWSAData: TWSAData): Integer;
function Ics_WSACleanup: Integer;
procedure Ics_WSASetLastError(iError: Integer);
function Ics_WSAGetLastError: Integer;
function Ics_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
function Ics_WSAAsyncGetHostByName(HWindow: HWND; wMsg: u_int; name, buf: PAnsiChar;
                               buflen: Integer): THandle;
function Ics_WSAAsyncGetHostByAddr(HWindow: HWND; wMsg: u_int; addr: PAnsiChar;
               len, Struct: Integer; buf: PAnsiChar; buflen: Integer): THandle;
function Ics_WSAAsyncSelect(s: TSocket; HWindow: HWND; wMsg: u_int; lEvent: Longint): Integer;
function Ics_getservbyname(name, proto: PAnsiChar): PServEnt;
function Ics_getprotobyname(name: PAnsiChar): PProtoEnt;
function Ics_gethostbyname(name: PAnsiChar): PHostEnt;
function Ics_gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt;
function Ics_gethostName(name: PAnsiChar; len: Integer): Integer;
function Ics_socket(af, Struct, protocol: Integer): TSocket;
function Ics_shutdown(s: TSocket; how: Integer): Integer;
function Ics_setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar;
                    optlen: Integer): Integer;
function Ics_getsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar;
                    var optlen: Integer): Integer;
function Ics_sendto(s: TSocket; var Buf; len, flags: Integer; var addrto: TSockAddr;
                tolen: Integer): Integer;
function Ics_send(s: TSocket; var Buf; len, flags: Integer): Integer;
function Ics_recv(s: TSocket; var Buf; len, flags: Integer): Integer;
function Ics_recvfrom(s: TSocket; var Buf; len, flags: Integer; var from: TSockAddr;
                  var fromlen: Integer): Integer;
function Ics_ntohs(netshort: u_short): u_short;
function Ics_ntohl(netlong: u_long): u_long;
function Ics_listen(s: TSocket; backlog: Integer): Integer;
function Ics_ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
function Ics_WSAIoctl(s                 : TSocket;
                  IoControlCode     : DWORD;
                  InBuffer          : Pointer;
                  InBufferSize      : DWORD;
                  OutBuffer         : Pointer;
                  OutBufferSize     : DWORD;
                  var BytesReturned : DWORD;
                  Overlapped        : POverlapped;
                  CompletionRoutine : FARPROC): Integer;

function Ics_inet_ntoa(inaddr: TInAddr): PAnsiChar;
function Ics_inet_addr(cp: PAnsiChar): u_long;
function Ics_htons(hostshort: u_short): u_short;
function Ics_htonl(hostlong: u_long): u_long;
function Ics_getsockname(s: TSocket; var name: TSockAddr; var namelen: Integer): Integer;
function Ics_getpeername(s: TSocket; var name: TSockAddr; var namelen: Integer): Integer;
function Ics_connect(s: TSocket; var name: TSockAddr; namelen: Integer): Integer;
function Ics_closesocket(s: TSocket): Integer;
function Ics_bind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer;
function Ics_accept(s: TSocket; addr: PSockAddr; addrlen: PInteger): TSocket;

function Ics_GetAddrInfoA(NodeName: PAnsiChar; ServName: PAnsiChar; Hints: PADDRINFOA;
                     var AddrInfo: PADDRINFOA): Integer;
function Ics_GetAddrInfoW(NodeName: PWideChar; ServName: PWideChar; Hints: PADDRINFOW;
                     var AddrInfo: PADDRINFOW): Integer;
{$IFDEF UNICODE}
function Ics_GetAddrInfo(NodeName: PWideChar; ServName: PWideChar; Hints: PADDRINFOW;
                     var AddrInfo: PADDRINFOW): Integer;

{$ELSE}
function Ics_GetAddrInfo(NodeName: PAnsiChar; ServName: PAnsiChar; Hints: PADDRINFOA;
                     var AddrInfo: PADDRINFOA): Integer;
{$ENDIF}
procedure Ics_FreeAddrInfoA(ai: PADDRINFOA);
procedure Ics_FreeAddrInfoW(ai: PADDRINFOW);
{$IFDEF UNICODE}
procedure Ics_FreeAddrInfo(ai: PADDRINFOW);
{$ELSE}
procedure Ics_FreeAddrInfo(ai: PAddrInfo);
{$ENDIF}
function Ics_GetNameInfoA(addr: PSockAddr; namelen: Integer; host: PAnsiChar;
   hostlen: DWORD; serv: PAnsiChar; servlen: DWORD; flags: Integer): Integer;
function Ics_GetNameInfoW(addr: PSockAddr; namelen: Integer; host: PWideChar;
   hostlen: DWORD; serv: PWideChar; servlen: DWORD; flags: Integer): Integer;
{$IFDEF UNICODE}
function Ics_GetNameInfo(addr: PSockAddr; namelen: Integer; host: PWideChar;
   hostlen: DWORD; serv: PWideChar; servlen: DWORD; flags: Integer): Integer;
{$ELSE}
function Ics_GetNameInfo(addr: PSockAddr; namelen: Integer; host: PAnsiChar;
   hostlen: DWORD; serv: PAnsiChar; servlen: DWORD; flags: Integer): Integer;
{$ENDIF}

{ ICS Helpers }

type
  ESocketAPIException = class(Exception);

var
  WSocketGCount   : Integer = 0;
  GWSockCritSect  : TRTLCriticalSection;
  GReqVerLow      : BYTE    = 2;
  GReqVerHigh     : BYTE    = 2;
  GIPv6Available  : Integer = -1; { -1 = unchecked, 0 = FALSE, 1 = TRUE }

procedure ForceLoadWinsock;
procedure CancelForceLoadWinsock;
procedure UnloadWinsock;
function  SocketErrorDesc(ErrCode : Integer) : String;
function  WinsockAPIInfo : TWSADATA;
function  IsSocketAPILoaded : Boolean;
function  IsIPv6APIAvailable: Boolean;
function  IsIPv6Available: Boolean;

{ Record TScopeID includes bitfields, these are helper functions }
function  ScopeIdGetLevel(const AScopeId: ULONG): ULONG;
function  ScopeIdGetZone(const AScopeId: ULONG): ULONG;
procedure ScopeIdSetLevel(var AScopeId: ULONG; const ALevel: ULONG);
procedure ScopeIdSetZone(var AScopeId: ULONG; const AZone: ULONG);
function  MakeScopeId(const AZone: ULONG; const ALevel: ULONG): ULONG;
