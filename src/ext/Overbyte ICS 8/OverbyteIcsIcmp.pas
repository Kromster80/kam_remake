{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This unit encapsulate the ICMP.DLL into an object of type TICMP.
              Using this object, you can easily ping any host on your network.
              Works only in 32 bits mode (no Delphi 1) under NT or 95.
              TICMP is perfect for a console mode program, but if you build a
              GUI program, you could use the TPing object wich is a true VCL
              encapsulating the TICMP object. Then you can use object inspector
              to change properties or event handler. This is much simpler to
              use for a GUI program.
Creation:     January 6, 1997
Version:      8.05
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2012 by François PIETTE
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

Updates:
Dec 13, 1997 V1.01 Added OnEchoRequest and OnEchoReply events and removed the
             corresponding OnDisplay event. This require to modify existing
             programs.
Mar 15, 1998 V1.02 Deplaced address resolution just before use
Sep 24, 1998 V1.02a Changed TIPAddr and others to LongInt to avoid range error
             problems with Delphi 4
Jan 24, 1999 V1.03 Surfaced Flags property to allow fragmentation check
             (Flags = IP_FLAG_DF to enable fragmentation check)
Jan 19, 2004 V1.04 Added property ICMPDLLHandle.
May 32, 2004 V1.05 Used ICSDEFS.INC
Mar 26, 2006 V6.00 Started new version 6
Mar 24, 2008 V6.01 Francois Piette made some changes to prepare code
                   for Unicode.
                   Use of AnsiString.
Aug 12, 2008 V7.00 Reverted from AnsiString to String for properties.
Jul 25, 2011 V7.01 Added directive "EXTERNALSYM"
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Jul 21, 2012 V7.02 Arno fixed a Win64 bug.
Feb 20, 2013 V8.02 Angus - icmp.dll was replaced with iphlpapi.dll from Windows XP
                   which also exports IcmpSendEcho2, IcmpSendEchoEx2 and Icmp6xx vcersions
                   Pings IPv4 or IPv6 addresses or host names
                   Added LastErrStr property to return last error message if Result=0
                   Added SrcAddress property to specify IP of interface from which to ping
                   Added PingMsg property to allow the ping packet content to be changed
                   Don't initialize ICMP in creator since raising exceptions there can cause
                      an application to fail to run and it's difficult to report errors
                   IPv6 ping returns Reply6 record, check SocketFamily to see which record
                   Results now also returned as ReplyIP (string), ReplyStatus, ReplyRTT and
                      ReplySize which are the same for  IPv4 and IPv6
                   Added AsyncPing which currently not does work, callback never called, no idea why
Mai 03, 2013 V8.03 Arno included OverbyteIcsTypes.pas to make it compile with Delphi 7
Sep 29, 2013 V8.04 Angus convert PingMsg to ANSI before sending it
Apr 17, 2014 V8.05 Angus don't call WSACleanup unless WSAStartup called, thanks to spanfkyous@163.com


API History
IcmpSendEcho - Windows 95 and later - sync
IcmpSendEcho2 - Windows 2000 and later, callback changed for Vista and later - async
Icmp6SendEcho2 - Windows XP and 2003 and later, callback changed for Vista and later - IPv6
IcmpSendEcho2Ex - Windows Vista SP1, 2008 and later - source IP

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsIcmp;
{$ENDIF}

interface
{$IFDEF MSWINDOWS}

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$A+}
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
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER80}
// This source file is *NOT* compatible with Delphi 1 because it uses
// Win 32 features.
{$ENDIF}

{$ALIGN ON}

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    OverbyteIcsWinsock,
    OverbyteIcsTypes,
    OverbyteIcsUtils,
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsWSocket;
{$ELSE}
    OverbyteIcsWSocket;
{$ENDIF FMX}

const
  IcmpVersion = 8.05;
  CopyRight : String   = ' TICMP (c) 1997-2014 F. Piette V8.05 ';
  IcmpDLL     = 'icmp.dll';
  IphlpapiDLL = 'iphlpapi.dll';     { V8.02 }

  // IP status codes returned to transports and user IOCTLs.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_SUCCESS} {$ENDIF}
  IP_SUCCESS                  = 0;
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_STATUS_BASE} {$ENDIF}
  IP_STATUS_BASE              = 11000;
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_BUF_TOO_SMALL} {$ENDIF}
  IP_BUF_TOO_SMALL            = (IP_STATUS_BASE + 1);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_DEST_NET_UNREACHABLE} {$ENDIF}
  IP_DEST_NET_UNREACHABLE     = (IP_STATUS_BASE + 2);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_DEST_HOST_UNREACHABLE} {$ENDIF}
  IP_DEST_HOST_UNREACHABLE    = (IP_STATUS_BASE + 3);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_DEST_PROT_UNREACHABLE} {$ENDIF}
  IP_DEST_PROT_UNREACHABLE    = (IP_STATUS_BASE + 4);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_DEST_PORT_UNREACHABLE} {$ENDIF}
  IP_DEST_PORT_UNREACHABLE    = (IP_STATUS_BASE + 5);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_NO_RESOURCES} {$ENDIF}
  IP_NO_RESOURCES             = (IP_STATUS_BASE + 6);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_BAD_OPTION} {$ENDIF}
  IP_BAD_OPTION               = (IP_STATUS_BASE + 7);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_HW_ERROR} {$ENDIF}
  IP_HW_ERROR                 = (IP_STATUS_BASE + 8);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_PACKET_TOO_BIG} {$ENDIF}
  IP_PACKET_TOO_BIG           = (IP_STATUS_BASE + 9);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_REQ_TIMED_OUT} {$ENDIF}
  IP_REQ_TIMED_OUT            = (IP_STATUS_BASE + 10);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_BAD_REQ} {$ENDIF}
  IP_BAD_REQ                  = (IP_STATUS_BASE + 11);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_BAD_ROUTE} {$ENDIF}
  IP_BAD_ROUTE                = (IP_STATUS_BASE + 12);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_TTL_EXPIRED_TRANSIT} {$ENDIF}
  IP_TTL_EXPIRED_TRANSIT      = (IP_STATUS_BASE + 13);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_TTL_EXPIRED_REASSEM} {$ENDIF}
  IP_TTL_EXPIRED_REASSEM      = (IP_STATUS_BASE + 14);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_PARAM_PROBLEM} {$ENDIF}
  IP_PARAM_PROBLEM            = (IP_STATUS_BASE + 15);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_SOURCE_QUENCH} {$ENDIF}
  IP_SOURCE_QUENCH            = (IP_STATUS_BASE + 16);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPTION_TOO_BIG} {$ENDIF}
  IP_OPTION_TOO_BIG           = (IP_STATUS_BASE + 17);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_BAD_DESTINATION} {$ENDIF}
  IP_BAD_DESTINATION          = (IP_STATUS_BASE + 18);

  // status codes passed up on status indications.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_ADDR_DELETED} {$ENDIF}
  IP_ADDR_DELETED             = (IP_STATUS_BASE + 19);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_SPEC_MTU_CHANGE} {$ENDIF}
  IP_SPEC_MTU_CHANGE          = (IP_STATUS_BASE + 20);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_MTU_CHANGE} {$ENDIF}
  IP_MTU_CHANGE               = (IP_STATUS_BASE + 21);

  // following added for V8.02
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_UNLOAD} {$ENDIF}
  IP_UNLOAD = (IP_STATUS_BASE + 22);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_ADDR_ADDED} {$ENDIF}
  IP_ADDR_ADDED = (IP_STATUS_BASE + 23);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_MEDIA_CONNECT} {$ENDIF}
  IP_MEDIA_CONNECT = (IP_STATUS_BASE + 24);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_MEDIA_DISCONNECT} {$ENDIF}
  IP_MEDIA_DISCONNECT = (IP_STATUS_BASE + 25);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_BIND_ADAPTER} {$ENDIF}
  IP_BIND_ADAPTER = (IP_STATUS_BASE + 26);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_UNBIND_ADAPTER} {$ENDIF}
  IP_UNBIND_ADAPTER = (IP_STATUS_BASE + 27);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_DEVICE_DOES_NOT_EXIST} {$ENDIF}
  IP_DEVICE_DOES_NOT_EXIST = (IP_STATUS_BASE + 28);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_DUPLICATE_ADDRESS} {$ENDIF}
  IP_DUPLICATE_ADDRESS = (IP_STATUS_BASE + 29);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_INTERFACE_METRIC_CHANGE} {$ENDIF}
  IP_INTERFACE_METRIC_CHANGE = (IP_STATUS_BASE + 30);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_RECONFIG_SECFLTR} {$ENDIF}
  IP_RECONFIG_SECFLTR = (IP_STATUS_BASE + 31);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_NEGOTIATING_IPSEC} {$ENDIF}
  IP_NEGOTIATING_IPSEC = (IP_STATUS_BASE + 32);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_INTERFACE_WOL_CAPABILITY_CHANGE} {$ENDIF}
  IP_INTERFACE_WOL_CAPABILITY_CHANGE = (IP_STATUS_BASE + 33);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_DUPLICATE_IPADD} {$ENDIF}
  IP_DUPLICATE_IPADD = (IP_STATUS_BASE + 34);

  // IPv6-only status codes  V8.02
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_DEST_UNREACHABLE} {$ENDIF}
  IP_DEST_UNREACHABLE = (IP_STATUS_BASE + 40);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_TIME_EXCEEDED} {$ENDIF}
  IP_TIME_EXCEEDED = (IP_STATUS_BASE + 41);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_BAD_HEADER} {$ENDIF}
  IP_BAD_HEADER = (IP_STATUS_BASE + 42);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_UNRECOGNIZED_NEXT_HEADER} {$ENDIF}
  IP_UNRECOGNIZED_NEXT_HEADER = (IP_STATUS_BASE + 43);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_ICMP_ERROR} {$ENDIF}
  IP_ICMP_ERROR = (IP_STATUS_BASE + 44);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_DEST_SCOPE_MISMATCH} {$ENDIF}
  IP_DEST_SCOPE_MISMATCH = (IP_STATUS_BASE + 45);

  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_GENERAL_FAILURE} {$ENDIF}
  IP_GENERAL_FAILURE          = (IP_STATUS_BASE + 50);

  {$IFDEF COMPILER16_UP} {$EXTERNALSYM MAX_IP_STATUS} {$ENDIF}
  MAX_IP_STATUS               = IP_GENERAL_FAILURE;

  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_PENDING} {$ENDIF}
  IP_PENDING                  = (IP_STATUS_BASE + 255);

  // IP header flags
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_FLAG_DF} {$ENDIF}
  IP_FLAG_DF                  = $02;         // Don't fragment this packet.

  // IP Option Types
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_EOL} {$ENDIF}
  IP_OPT_EOL                  = $00;         // End of list option
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_NOP} {$ENDIF}
  IP_OPT_NOP                  = $01;         // No operation
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_SECURITY} {$ENDIF}
  IP_OPT_SECURITY             = $82;         // Security option.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_LSRR} {$ENDIF}
  IP_OPT_LSRR                 = $83;         // Loose source route.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_SSRR} {$ENDIF}
  IP_OPT_SSRR                 = $89;         // Strict source route.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_RR} {$ENDIF}
  IP_OPT_RR                   = $07;         // Record route.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_TS} {$ENDIF}
  IP_OPT_TS                   = $44;         // Timestamp.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_SID} {$ENDIF}
  IP_OPT_SID                  = $88;         // Stream ID (obsolete)
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_ROUTER_ALERT} {$ENDIF}
  IP_OPT_ROUTER_ALERT         = $94;         // Router Alert Option  V8.02
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM MAX_OPT_SIZE} {$ENDIF}
  MAX_OPT_SIZE                = $40;

type
  // IP types
  TIPAddr   = LongInt;   // An IP address.
  TIPMask   = LongInt;   // An IP subnet mask.
  TIPStatus = LongInt;   // Status code returned from IP APIs.

  PIoStatusBlock = ^TIoStatusBlock;           { V8.02 }
  TIoStatusBlock = record
    Status:         DWORD;
    Information:    ulong_ptr;
  end;

  PIO_APC_ROUTINE = procedure (ApcContext: Pointer; IoStatusBlock:
                                    PIoStatusBlock; Reserved: u_long); stdcall;

  PIpv6AddressEx = ^TIpv6AddressEx;              { V8.02 }
  TIpv6AddressEx = packed record
    sin6_port:      u_short;     // Transport level port number
    sin6_flowinfo:  u_long;      // IPv6 flow information
    sin6_addr:      TInAddr6;    // IPv6 address
    sin6_scope_id:  u_long;      // Set of interfaces for a scope.
  end;

  PIPOptionInformation = ^TIPOptionInformation;
  TIPOptionInformation = packed record
     TTL:         Byte;      // Time To Live (used for traceroute)
     TOS:         Byte;      // Type Of Service (usually 0)
     Flags:       Byte;      // IP header flags (usually 0)
     OptionsSize: Byte;      // Size of options data (usually 0, max 40)
     OptionsData: PAnsiChar; // Options data buffer
  end;

  PIcmpEchoReply = ^TIcmpEchoReply;
  TIcmpEchoReply = packed record
     Address:       TIPAddr;              // Replying address
     Status:        DWord;                // IP status value
     RTT:           DWord;                // Round Trip Time in milliseconds
     DataSize:      Word;                 // Reply data size
     Reserved:      Word;                 // Reserved
     Data:          Pointer;              // Pointer to reply data buffer
     Options:       TIPOptionInformation; // Reply options
  end;

 {$ALIGN 4}
  PIcmpV6EchoReply = ^TIcmpV6EchoReply;      { V8.02 }
  TIcmpV6EchoReply = record    // NOT PACKED!!!
    Address:        TIpv6AddressEx;      // Replying IPv6 address
    Status:         DWord;               // IP status value
    RTT:            DWord;               // Round Trip Time in milliseconds
  end;

  // IcmpCreateFile:
  //     Opens a handle on which ICMP Echo Requests can be issued.
  // Arguments:
  //     None.
  // Return Value:
  //     An open file handle or INVALID_HANDLE_VALUE. Extended error information
  //     is available by calling GetLastError().
  TIcmpCreateFile  = function: THandle; stdcall;
  TIcmp6CreateFile  = function: THandle; stdcall;        { V8.02 }

  // IcmpCloseHandle:
  //     Closes a handle opened by ICMPOpenFile.
  // Arguments:
  //     IcmpHandle  - The handle to close.
  // Return Value:
  //     TRUE if the handle was closed successfully, otherwise FALSE. Extended
  //     error information is available by calling GetLastError().
  TIcmpCloseHandle = function(IcmpHandle: THandle): Boolean; stdcall;

  // IcmpSendEcho/2/2Ex:
  //     Sends an ICMP Echo request and returns one or more replies. The
  //     call returns when the timeout has expired or the reply buffer
  //     is filled.
  // Arguments:
  //     IcmpHandle         - An open handle returned by ICMPCreateFile.
  //     Event              - This is the event to be signalled whenever an IcmpResponse
  //                          comes in.
  //     ApcRoutine         - This routine would be called when the calling thread
  //                          is in an alertable thread and an ICMP reply comes in.
  //     ApcContext         - This optional parameter is given to the ApcRoutine when
  //                          this call succeeds.
  //     SourceAddress      - The source address on which to issue the echo request..
  //     DestinationAddress - The destination of the echo request.
  //     RequestData        - A buffer containing the data to send in the
  //                          request.
  //     RequestSize        - The number of bytes in the request data buffer.
  //     RequestOptions     - Pointer to the IP header options for the request.
  //                          May be NULL.
  //     ReplyBuffer        - A buffer to hold any replies to the request.
  //                          On return, the buffer will contain an array of
  //                          ICMP_ECHO_REPLY structures followed by options
  //                          and data. The buffer should be large enough to
  //                          hold at least one ICMP_ECHO_REPLY structure
  //                          and 8 bytes of data - this is the size of
  //                          an ICMP error message.
  //     ReplySize          - The size in bytes of the reply buffer.
  //     Timeout            - The time in milliseconds to wait for replies.
  // Return Value:
  //     Returns the number of replies received and stored in ReplyBuffer. If
  //     the return value is zero, extended error information is available
  //     via GetLastError().
  TIcmpSendEcho    = function(IcmpHandle:          THandle;
                              DestinationAddress:  TIPAddr;
                              RequestData:         Pointer;
                              RequestSize:         Word;
                              RequestOptions:      PIPOptionInformation;
                              ReplyBuffer:         Pointer;
                              ReplySize:           DWord;
                              Timeout:             DWord
                             ): DWord; stdcall;

  TIcmpSendEcho2    = function(IcmpHandle:         THandle;        { V8.02 }
                              Event:               Pointer;
                              ApcRoutine:          PIO_APC_ROUTINE; // Pointer;
                              ApcContext:          Pointer;
                              DestinationAddress:  TIPAddr;
                              RequestData:         Pointer;
                              RequestSize:         Word;
                              RequestOptions:      PIPOptionInformation;
                              ReplyBuffer:         Pointer;
                              ReplySize:           DWord;
                              Timeout:             DWord
                             ): DWord; stdcall;

  TIcmpSendEcho2Ex    = function(IcmpHandle:       THandle;         { V8.02 }
                              Event:               Pointer;
                              ApcRoutine:          Pointer;
                              ApcContext:          Pointer;
                              SourceAddress:       TIPAddr;
                              DestinationAddress:  TIPAddr;
                              RequestData:         Pointer;
                              RequestSize:         Word;
                              RequestOptions:      PIPOptionInformation;
                              ReplyBuffer:         Pointer;
                              ReplySize:           DWord;
                              Timeout:             DWord
                             ): DWord; stdcall;

  TIcmp6SendEcho2    = function(IcmpHandle:        THandle;          { V8.02 }
                              Event:               Pointer;
                              ApcRoutine:          Pointer;
                              ApcContext:          Pointer;
                              SourceAddress:       PSockAddrIn6;
                              DestinationAddress:  PSockAddrIn6;
                              RequestData:         Pointer;
                              RequestSize:         Word;
                              RequestOptions:      PIPOptionInformation;
                              ReplyBuffer:         Pointer;
                              ReplySize:           DWord;
                              Timeout:             DWord
                             ): DWord; stdcall;

  // Event handler type declaration for TICMP.OnDisplay event.
  TICMPDisplay = procedure(Sender: TObject; Msg : String) of object;
  TICMPReply   = procedure(Sender: TObject; Error : Integer) of object;

  // The object wich encapsulate the ICMP.DLL
  TICMP = class(TObject)
  private
    hICMPdll :        HModule;                    // Handle for ICMP.DLL
    IcmpCreateFile :  TIcmpCreateFile;
    Icmp6CreateFile : TIcmp6CreateFile;     { V8.02 }
    IcmpCloseHandle : TIcmpCloseHandle;
    IcmpSendEcho :    TIcmpSendEcho;
    IcmpSendEcho2 :   TIcmpSendEcho2;       { V8.02 }
    IcmpSendEcho2Ex : TIcmpSendEcho2Ex;     { V8.02 }
    Icmp6SendEcho2 :  TIcmp6SendEcho2;      { V8.02 }
    hICMP :           THandle;                    // Handle for the ICMP Calls
    hICMP6 :          THandle;                    // Handle for the ICMP IPv6 Calls
    FDataBuf:         Array of Byte;              // buffer for data sent
    FReplyBuf:        Array [0..1023] of Byte;    // buffer for data reply
    FRBufSize:        Integer;                    // size of FReplyBuf
    FReply:           TIcmpEchoReply;             // ICMP Echo public reply buffer
    FReply6:          TIcmpV6EchoReply;           // ICMP Echo public reply buffer IPv6 V8.02
    FAddress :        String;                     // Destination Address given
    FSocketFamily :   TSocketFamily;              // IPv4 or IPv6 address V8.02
    FSrcAddress :     String;                     // Source IPv4 Address given V8.02
    FSrcAddress6 :    String;                     // Source IPv6 Address given V8.02
    FPingMsg :        String;                     // The message to ping V8.02
    FHostName :       String;                     // Dotted IP of host (output)
    FHostIP :         String;                     // Name of host      (Output)
    FIPAddress :      TIPAddr;                    // IPv4 address of host to contact
    FIPAddress6 :     TSockAddrIn6;               // IPv6 address of host to contact
    FSrcIPAddr:       TIPAddr;                    // source IPv4 address of interface V8.02
    FSrcIPAddr6:      TSockAddrIn6;               // source IPv6 address of interface V8.02
    FSize :           Integer;                    // Packet size (default to 56)
    FTimeOut :        Integer;                    // Timeout (default to 4000mS)
    FTTL :            Integer;                    // Time To Live (for send)
    FFlags :          Integer;                    // Options flags
    FReplyIP :        String;                     // Reply IPv4 or IPv6 address (output)  V8.02
    FReplyRTT :       Integer;                    // Reply round trip time in ms (output) V8.02
    FReplyStatus :    Integer;                    // Reply status (output) V8.02
    FReplySize :      Integer;                    // Reply data size (output)             V8.02
    FOnDisplay :      TICMPDisplay;               // Event handler to display
    FOnEchoRequest :  TNotifyEvent;
    FOnEchoReply :    TICMPReply;
    FLastError :      DWORD;                      // After sending ICMP packet
    FLastErrStr :     String;                     // Last error string if Result=0 V8.02
    FAddrResolved :   Boolean;
    procedure ResolveAddr;
    procedure ParseReply;
    function    InternalPing (Async: boolean): Integer;  { V8.02 }
    procedure   SetSocketFamily(Value: TSocketFamily);   { V8.02 }
    procedure   SetAddress(Value : String);
    procedure   SetSrcAddress(Value : String);           { V8.02 }
    procedure   SetSrcAddress6(Value : String);          { V8.02 }
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   Initialize;                      { V8.02 }
    function    Ping : Integer;
    function    PingAsync : Integer;             { V8.02 }
    function    GetErrorString : String;

    property Address       : String         read  FAddress   write SetAddress;
    property SocketFamily  : TSocketFamily  read  FSocketFamily write SetSocketFamily; { V8.02 }
    property SrcAddress    : String         read  FSrcAddress write SetSrcAddress;   { V8.02 }
    property SrcAddress6   : String         read  FSrcAddress6 write SetSrcAddress6; { V8.02 }
    property PingMsg       : String         read  FPingMsg   write FPingMsg;         { V8.02 }
    property Size          : Integer        read  FSize      write FSize;
    property Timeout       : Integer        read  FTimeout   write FTimeout;
    property Reply         : TIcmpEchoReply read  FReply;
    property Reply6        : TIcmpV6EchoReply read  FReply6;                         { V8.02 }
    property TTL           : Integer        read  FTTL       write FTTL;
    Property Flags         : Integer        read  FFlags     write FFlags;
    property ErrorCode     : DWORD          read  FLastError;
    property ErrorString   : String         read  GetErrorString;
    property LastErrStr    : String         read  FLastErrStr;                       { V8.02 }
    property HostName      : String         read  FHostName;
    property HostIP        : String         read  FHostIP;
    property ICMPDLLHandle : HModule        read  hICMPdll;
    property ReplyIP       : String         read  FReplyIP;                          { V8.02 }
    Property ReplyRTT      : Integer        read  FReplyRTT;                         { V8.02 }
    Property ReplyStatus   : Integer        read  FReplyStatus;                      { V8.02 }
    Property ReplySize     : Integer        read  FReplySize;                        { V8.02 }
    property OnDisplay     : TICMPDisplay   read  FOnDisplay write FOnDisplay;
    property OnEchoRequest : TNotifyEvent   read  FOnEchoRequest
                                            write FOnEchoRequest;
    property OnEchoReply   : TICMPReply     read  FOnEchoReply
                                            write FOnEchoReply;
  end;

  TICMPException = class(Exception);
{$ENDIF MSWINDOWS}

implementation

{$IFDEF MSWINDOWS}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TICMP.Create;
begin
    hICMP    := INVALID_HANDLE_VALUE;
    hICMP6   := INVALID_HANDLE_VALUE;    { V8.02 }
    hICMPdll := 0;
    FSize    := 56;
    FTTL     := 64;
    FTimeOut := 4000;
    FPingMsg := 'Pinging from Delphi code written by F. Piette'; { V8.02 }
    FSocketFamily := DefaultSocketFamily;                        { V8.02 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TICMP.Destroy;
begin
    if hICMP <> INVALID_HANDLE_VALUE then
        IcmpCloseHandle(hICMP);
    if hICMP6 <> INVALID_HANDLE_VALUE then    { V8.02 }
        IcmpCloseHandle(hICMP6);
    if hICMPdll <> 0 then begin
        FreeLibrary(hICMPdll);
        hICMPdll := 0;
        WSACleanup;   { V8.05 only if initialised }
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MinInteger(X, Y: Integer): Integer;
begin
    if X >= Y then
        Result := Y
    else
        Result := X;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//load ICMP functions dynamically, but not in creator which causes problems
// when an exception is raised
procedure TICMP.Initialize;
var
    WSAData: TWSAData;
begin
    // register the IphlpapiDLL stuff, XP and later, V8.02
    hICMPdll := LoadLibrary(IphlpapiDLL);
    if hICMPdll <> 0 then begin
        @ICMPCreateFile   := GetProcAddress(hICMPdll, 'IcmpCreateFile');
        @IcmpCloseHandle  := GetProcAddress(hICMPdll, 'IcmpCloseHandle');
        @IcmpSendEcho     := GetProcAddress(hICMPdll, 'IcmpSendEcho');
        @IcmpSendEcho2    := GetProcAddress(hICMPdll, 'IcmpSendEcho2');
        @IcmpSendEcho2Ex  := GetProcAddress(hICMPdll, 'IcmpSendEcho2Ex');
        if IsIPv6Available then begin
            @ICMP6CreateFile  := GetProcAddress(hICMPdll, 'Icmp6CreateFile');
            @Icmp6SendEcho2   := GetProcAddress(hICMPdll, 'Icmp6SendEcho2');
        end;
    end;

    // register the icmp.dll stuff if not found already
    if (@ICMPCreateFile = Nil) or (hICMPdll = 0) then begin
        FreeLibrary(hICMPdll);
        hICMPdll := LoadLibrary(icmpDLL);
        if hICMPdll = 0 then
            raise TICMPException.Create('Unable to register ' + icmpDLL);

        @ICMPCreateFile  := GetProcAddress(hICMPdll, 'IcmpCreateFile');
        @IcmpCloseHandle := GetProcAddress(hICMPdll, 'IcmpCloseHandle');
        @IcmpSendEcho    := GetProcAddress(hICMPdll, 'IcmpSendEcho');
        @IcmpSendEcho2   := GetProcAddress(hICMPdll, 'IcmpSendEcho2');   { V8.02 }
    end;
    if (@ICMPCreateFile = Nil) or
       (@IcmpCloseHandle = Nil) or
       (@IcmpSendEcho = Nil) then
          raise TICMPException.Create('Error loading dll functions');

// initialise winsock,  V8.05 only if ICMP found
    if WSAStartup($0202, WSAData) <> 0 then
            raise TICMPException.Create('Error initialising Winsock');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// note - does a sync blocking DNS lookup for an IPv4 or IPv6 host name
// sets SocketFamily, HostIP, and FIPAddress or IPAddress6
// returns FAddrResolved=false for error

procedure TICMP.ResolveAddr;
var
    flag: boolean;
    LSocketFamily: TSocketFamily;
begin
    FAddrResolved := FALSE;
    FSrcIPAddr := 0;
    FillChar(FSrcIPAddr6, SizeOf(FSrcIPAddr6), 0);
    FAddress := IcsTrim(FAddress);
    if Length(FAddress) = 0 then begin
        FLastErrStr := 'Blank adreess';
        exit;
    end;

    // find socket family if not a host name and set IPs
    FHostName := FAddress;
    if WSocketIsIP(FAddress, LSocketFamily) then begin
        if LSocketFamily = sfIPv4 then begin
            FSocketFamily := sfIPv4 ;
            FIPAddress := WSocketStrToIPv4(FAddress, flag);
        end
        else begin
            if NOT IsIPv6Available then begin
                FHostIP := FAddress;
                FLastErrStr := 'IPv6 not available';
                exit ;
            end;
            FSocketFamily := sfIPv6 ;
            FIPAddress6.sin6_addr := TInAddr6 (WSocketStrToIPv6(FAddress, flag));
            FIPAddress6.sin6_family := PF_INET6;
        end;
        if NOT flag then exit; // failed to parse IP, should not happen
        FHostIP := FAddress;
    end

    // need a DNS lookup of host name, might return IPv4 or IPv6 address
    else begin
        { The next line will trigger an exception in case of failure }
        try
            WSocketResolveHost(FAddress, FIPAddress6, FSocketFamily, IPPROTO_TCP);
            if FIPAddress6.sin6_family <> PF_INET6 then begin
                FSocketFamily := sfIPv4 ;
                FIPAddress := PSockAddrIn(@FIPAddress6)^.sin_addr.S_addr ;
                FHostIP := WSocketIPv4ToStr (FIPAddress);
            end
            else begin
                FSocketFamily := sfIPv6;
                FHostIP := WSocketIPv6ToStr (TIcsIPv6Address (FIPAddress6.sin6_addr));
            end;
        except
            on E: Exception do begin
                FLastErrStr := 'Unable to resolve ' + FAddress + ' - ' + E.Message;
                exit ;
            end;
        end;
    end;

    // V8.02 now set optional source IP address to ping from specific interface
    // failure ignored
    if FSocketFamily = sfIPv4 then begin
        FSrcIPAddr := WSocketStrToIPv4(FSrcAddress, flag);
        if NOT flag then FSrcIPAddr := 0;  // special case, don't use IcmpSendEcho2Ex it needs a real IP
    end
    else if FSocketFamily = sfIPv6 then begin
        FSrcIPAddr6.sin6_family := PF_INET6;
        FSrcIPAddr6.sin6_addr := TInAddr6(WSocketStrToIPv6(FSrcAddress6, flag));
    end ;

    FAddrResolved := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TICMP.SetAddress(Value : String);
begin
    // Only change if needed (could take a long time)
    if FAddress = Value then
        Exit;
    FAddress      := Value;
    FAddrResolved := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TICMP.SetSrcAddress(Value : String);     { V8.02 }
begin
    // Only change if needed (could take a long time)
    if FSrcAddress = Value then
        Exit;
    FSrcAddress   := Value;
    FAddrResolved := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TICMP.SetSrcAddress6(Value : String);     { V8.02 }
begin
    // Only change if needed (could take a long time)
    if FSrcAddress6 = Value then
        Exit;
    FSrcAddress6  := Value;
    FAddrResolved := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TICMP.SetSocketFamily(Value: TSocketFamily);    { V8.02 }
begin
    // Only change if needed (could take a long time)
    if FSocketFamily = Value then
        Exit;
    FSocketFamily  := Value;
    FAddrResolved := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TICMP.GetErrorString : String;
begin
    case FLastError of
    IP_SUCCESS:               Result := 'No error';
    IP_BUF_TOO_SMALL:         Result := 'Buffer too small';
    IP_DEST_NET_UNREACHABLE:  Result := 'Destination network unreachable';
    IP_DEST_HOST_UNREACHABLE: Result := 'Destination host unreachable';
    IP_DEST_PROT_UNREACHABLE: Result := 'Destination protocol unreachable';
    IP_DEST_PORT_UNREACHABLE: Result := 'Destination port unreachable';
    IP_NO_RESOURCES:          Result := 'No resources';
    IP_BAD_OPTION:            Result := 'Bad option';
    IP_HW_ERROR:              Result := 'Hardware error';
    IP_PACKET_TOO_BIG:        Result := 'Packet too big';
    IP_REQ_TIMED_OUT:         Result := 'Request timed out';
    IP_BAD_REQ:               Result := 'Bad request';
    IP_BAD_ROUTE:             Result := 'Bad route';
    IP_TTL_EXPIRED_TRANSIT:   Result := 'TTL expired in transit';
    IP_TTL_EXPIRED_REASSEM:   Result := 'TTL expired in reassembly';
    IP_PARAM_PROBLEM:         Result := 'Parameter problem';
    IP_SOURCE_QUENCH:         Result := 'Source quench';
    IP_OPTION_TOO_BIG:        Result := 'Option too big';
    IP_BAD_DESTINATION:       Result := 'Bad Destination';
    IP_ADDR_DELETED:          Result := 'Address deleted';
    IP_SPEC_MTU_CHANGE:       Result := 'Spec MTU change';
    IP_MTU_CHANGE:            Result := 'MTU change';
    IP_UNLOAD:                Result := 'Unload';      {  following V8.02 }
    IP_ADDR_ADDED:            Result := 'Address added';
    IP_MEDIA_CONNECT:         Result := 'Media connect';
    IP_MEDIA_DISCONNECT:      Result := 'Media disconnect';
    IP_BIND_ADAPTER:          Result := 'Bind adapter';
    IP_UNBIND_ADAPTER:        Result := 'Unbind adapter';
    IP_DEVICE_DOES_NOT_EXIST: Result := 'Device does not exist';
    IP_DUPLICATE_ADDRESS:     Result := 'Duplicate address';
    IP_INTERFACE_METRIC_CHANGE: Result := 'Interface metric change';
    IP_RECONFIG_SECFLTR:      Result := 'Recognfig security filter';
    IP_NEGOTIATING_IPSEC:     Result := 'Negotiating IPsec';
    IP_INTERFACE_WOL_CAPABILITY_CHANGE :  Result := 'Interface WOL capability change';
    IP_DUPLICATE_IPADD:       Result := 'Duplicate IP address';

  // IPv6-only status codes  V8.02
    IP_DEST_UNREACHABLE:      Result := 'Destination unreachable (IPv6)';
    IP_TIME_EXCEEDED:         Result := 'Time exceeded (IPv6)';
    IP_BAD_HEADER:            Result := 'Bad header (IPv6)';
    IP_UNRECOGNIZED_NEXT_HEADER: Result := 'Unrecognised next header (IPv6)';
    IP_ICMP_ERROR:            Result := 'ICMP error (IPv6)';
    IP_DEST_SCOPE_MISMATCH:   Result := 'Destination scope mismatch (IPv6)';

    IP_GENERAL_FAILURE:       Result := 'General failure';
    IP_PENDING:               Result := 'Pending';
    else
        Result := SysErrorMessage(FLastError);    { V8.02 }
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// parse ping reply buffer
// note, with short TTL during traceroute, status=IP_TTL_EXPIRED_TRANSIT
procedure TICMP.ParseReply;
begin
    if (FSocketFamily = sfIPv4) then begin
        FReplyStatus := FReply.Status;
        FReplyIP :=  WSocketIPv4ToStr(FReply.Address);
        FReplySize := FReply.DataSize;
        FReplyRTT := FReply.RTT;
    end
    else if (FSocketFamily = sfIPv6) then begin
        FReplyStatus := FReply6.Status;
        FReplyIP :=  WSocketIPv6ToStr(TIcsIPv6Address (FReply6.Address.sin6_addr));
        FReplySize := FSize;
        FReplyRTT := FReply6.RTT;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// V8.02 callback routine for IcmpSendEcho2 when async request completes
procedure EchoApcRoutine(ApcContext: Pointer; IoStatusBlock: PIoStatusBlock;
                                             Reserved: ULong); stdcall;  { V8.02 }
var
    MyIcmp: TICMP;
begin
    MyIcmp := TICMP (ApcContext);
    if MyIcmp.FSocketFamily = sfIPv4 then
        Move(MyIcmp.FReplyBuf, MyIcmp.FReply, SizeOf (MyIcmp.FReply))
    else if MyIcmp.FSocketFamily = sfIPv6 then begin
        Move(MyIcmp.FReplyBuf, MyIcmp.FReply6, SizeOf (MyIcmp.FReply6))
    end;
    MyIcmp.ParseReply;
    if Assigned(MyIcmp.FOnEchoReply) then begin
            MyIcmp.FOnEchoReply(MyIcmp, IoStatusBlock.Status);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// synchronous ping, function returns after success or FTimeOut
function TICMP.InternalPing (Async: boolean): Integer;
var
  IPOpt: TIPOptionInformation; // IP Options for packet to send
  ApcContext, ApcRoutine: Pointer;
  S: AnsiString;
begin
    Result     := 0;
    FLastError := 0;
    FLastErrStr := '';
    FillChar(FReply, SizeOf (FReply), 0);
    FillChar(FReply6, SizeOf (FReply6), 0);
    FReplyStatus := 0;
    FReplyIP :=  '';
    FReplySize := 0;
    FReplyRTT := 0;
    if hICMPdll = 0 then Initialize;   { V8.02 }
    if (not FAddrResolved) or
        ((FSocketFamily <> sfIPv4) and (FSocketFamily <> sfIPv6)) then   { V8.02 }
                                                              ResolveAddr;
    if not FAddrResolved then exit;    { V8.02 }

    if FSocketFamily = sfIPv4 then begin
        if hICMP = INVALID_HANDLE_VALUE then begin
            hICMP := IcmpCreateFile;
            if hICMP = INVALID_HANDLE_VALUE then begin
                FLastErrStr := 'Unable to get IPv4 ICMP handle';
                Exit;
            end;
        end;
        if FIPAddress = LongInt(INADDR_NONE) then begin
            FLastErrStr := 'Invalid IPv4 host address';
            Exit;
        end;
    end
    else if FSocketFamily = sfIPv6 then begin
        if (NOT IsIPv6Available) or (@ICMP6CreateFile = Nil) then begin      { V8.02 }
            FLastErrStr := 'IPv6 is not available';
            Exit;
        end;
        if hICMP6 = INVALID_HANDLE_VALUE then begin
            hICMP6 := Icmp6CreateFile;
            if hICMP6 = INVALID_HANDLE_VALUE then begin
                FLastErrStr := 'Unable to get IPv6 ICMP handle';
                Exit;
            end;
        end;
        if (FIPAddress6.sin6_family <> AF_INET6) then begin
            FLastErrStr := 'Invalid IPv6 host address';
            Exit;
        end;
    end
    else begin
        FLastErrStr := 'Invalid socket family';
        Exit;
    end;

    // async function, set callback function
    if Async then begin
        ApcRoutine := Pointer (@EchoApcRoutine);
        ApcContext := Pointer (Self);
    end
    else begin
        ApcRoutine := Nil;
        ApcContext := Nil;
    end;

  // Fill data buffer with some data bytes and trailing spaces
    if Length (FDataBuf) < FSize then SetLength (FDataBuf, FSize);           // buffer for data sent
    FRBufSize := Length (FReplyBuf);  // space for several replies
    FillChar(FDataBuf[0], FSize, $20);
    S := AnsiString (FPingMsg);
    Move(S, FDataBuf[0], MinInteger(FSize, Length(S)));    { V8.02 Msg now property, V8.04 ANSI }
    FillChar(FReplyBuf[0], FRBufSize, 0);

  //  try
    if Assigned(FOnEchoRequest) then
        FOnEchoRequest(Self);

    FillChar(IPOpt, SizeOf(IPOpt), 0);
    IPOpt.TTL   := FTTL;
    IPOpt.Flags := FFlags;

    if FSocketFamily = sfIPv6 then begin
        if (@IcmpSendEcho2 = Nil) then exit;
        Result := Icmp6SendEcho2(hICMP6, Nil, ApcRoutine, ApcContext, @FSrcIPAddr6,
            @FIPAddress6, @FDataBuf[0], FSize, @IPOpt, @FReplyBuf[0], FRBufSize, FTimeOut);
        Move(FReplyBuf, FReply6, SizeOf (FReply6));
    end
    else if (@IcmpSendEcho2Ex <> Nil) and (FSrcIPAddr <> 0) then begin      { V8.02 }
        Result := IcmpSendEcho2Ex(hICMP, Nil, ApcRoutine, ApcContext, FSrcIPAddr,
            FIPAddress, @FDataBuf[0], FSize, @IPOpt, @FReplyBuf[0], FRBufSize, FTimeOut);
        Move(FReplyBuf, FReply, SizeOf (FReply));
    end
    else if (@IcmpSendEcho2 <> Nil) then begin      { V8.02 }
        Result := IcmpSendEcho2(hICMP, Nil, ApcRoutine, ApcContext, FIPAddress,
                        @FDataBuf[0], FSize, @IPOpt, @FReplyBuf[0], FRBufSize, FTimeOut);
        Move(FReplyBuf, FReply, SizeOf (FReply));
    end
    else begin
        Result := IcmpSendEcho(hICMP, FIPAddress, @FDataBuf[0], FSize,
                            @IPOpt, @FReplyBuf[0], FRBufSize, FTimeOut);
        Move(FReplyBuf, FReply, SizeOf (FReply));
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// synchronous ping, function returns after success or FTimeOut
function TICMP.Ping : Integer;
begin
    Result := InternalPing (false);
    if Result > 0 then
        ParseReply
    else begin
        FLastError  := GetLastError;  // should be same as ReplyStatus
        if FLastErrStr = '' then FLastErrStr := GetErrorString;  { V8.02 keep an error string }
        if Assigned(FOnDisplay) then
            FOnDisplay(Self, FLastErrStr);
    end;
    if Assigned(FOnEchoReply) then
        FOnEchoReply(Self, Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// V8.02 asynchronous ping, function returns immediately
// WARNING - testing on Windows 7 64-bit starts the async all OK, but the callback function is never called
function TICMP.PingAsync : Integer;
begin
    Result := InternalPing (true);
    if (Result = 0) and (GetLastError = ERROR_IO_PENDING) then
        Result := 1   { async call has started OK }
    else begin
        FLastError  := GetLastError;
        if FLastErrStr = '' then FLastErrStr := GetErrorString;  { V8.02 keep an error string }
        if Assigned(FOnDisplay) then
            FOnDisplay(Self, FLastErrStr);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF MSWINDOWS}
end.

