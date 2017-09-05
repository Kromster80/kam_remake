{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This unit encapsulate the ICMP.DLL into a VCL of type TPing.
              Using this object, you can easily ping any host on your network.
              Works only in 32 bits mode (no Delphi 1) under NT or 95.
              If you wants to build a console mode program, use the TICMP
              object. You'll have a much smaller program.
Version:      8.02
Creation:     January 6, 1997
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2010 by François PIETTE
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

History:
Nov 30, 1997 V1.00 Added DNSLookup capability (taken from TWSocket)
Dec 13, 1997 V1.01 Added OnEchoRequest and OnEchoReply events and removed the
             corresponding OnDisplay event. This require to modify existing
             programs.
May 05, 1998 V1.02 Changed lpszClassName from 'XSocketWindowClass' to
             'ICSPingWindowClass' to avoid class name conflict with TWSocket.
             Thanks to Bill Parke <econmodel@econmodel.com> who found the
             problem.
Dec 26, 1998 V1.10 Changed all events to make sender reference TPing object
             and added an argument 'Icmp' which point to the underlaying TIcmp
             object (this was the sender in previous version). This require
             modification of existing code.
Jan 24, 1999 V1.11 Surfaced Flags property to allow fragmentation check
             (Flags = $02 to enable fragmentation check)
Nov 10, 2002 V1.12 Changed argument name from Error to Status in OnEchoReply
             to better reflect his use. 0 means OK !
Jan 29, 2004 V1.13 Added ICMPDLLHandle property and made Ping method virtual.
May 31, 2004 V1.14 Used ICSDEFS.INC
Mar 26, 2006 V6.00 New version 6 started.
Jul 19, 2008 V6.00 F. Piette made some changes for Unicode. Address, HostName
                      and DnsResult properties made as an AnsiString.
Nov 08, 2010 V6.01 Arno improved final exception handling, more details
             in OverbyteIcsWndControl.pas (V1.14 comments).
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Feb 20, 2013 V8.02 Angus -  Pings IPv4 or IPv6 addresses or host names
                   Added LastErrStr property to return last error message if Result=0
                   Added SrcAddress property to specify IP of interface from which to ping
                   Added PingMsg property to allow the ping packet content to be changed
                   IPv6 ping returns Reply6 record, check SocketFamily to see which record
                   Results now also returned as ReplyIP (string), ReplyStatus, ReplyRTT and
                      ReplySize which are the same for  IPv4 and IPv6
                   Added AsyncPing which currently not does work, callback never called, no idea why
                   Added TPingThread which pings using a thread to allow multiple pings
                       to run without blocking the main thread, simple and trace route
                       demos in OverbyteIcsPingTst1



Pending - ping using raw ICMP socket, will be async and should work on POSIX/MacOS

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsPing;
{$ENDIF}

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
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

interface

{$IFDEF MSWINDOWS}

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsWndControl,
    Ics.Fmx.OverbyteIcsWSocket,
    Ics.Fmx.OverbyteIcsIcmp,
{$ELSE}
    OverbyteIcsWndControl,
    OverbyteIcsWSocket,
    OverbyteIcsIcmp,
{$ENDIF FMX}    
    OverbyteIcsWinsock;

const
  PingVersion           = 802;
  CopyRight : String    = ' TPing (c) 1997-2013 F. Piette V8.02 ';

type
  TDnsLookupDone = procedure (Sender: TObject; Error: Word) of object;
  TPingDisplay   = procedure(Sender: TObject; Icmp: TObject; Msg : String) of object;
  TPingReply     = procedure(Sender: TObject; Icmp: TObject; Status : Integer) of object;
  TPingRequest   = procedure(Sender: TObject; Icmp: TObject) of object;
{$IFDEF COMPILER16_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TPing = class(TIcsWndControl)
  protected
    FIcmp             : TICMP;
    FDnsResult        : String;
    FDnsSocket        : TWSocket;         { V8.02 use wsocket instead of local functions }
    FDnsLookPending   : boolean;          { V8.02 }
    FOnDnsLookupDone  : TDnsLookupDone;
    FOnEchoRequest    : TPingRequest;
    FOnEchoReply      : TPingReply;
    FOnDisplay        : TPingDisplay;
    procedure   AbortComponent; override; { V6.01 }
    procedure   SetAddress(Value : String);
    function    GetAddress : String;
    procedure   SetSrcAddress(Value : String);    { V8.02 }
    function    GetSrcAddress : String;           { V8.02 }
    procedure   SetSrcAddress6(Value : String);   { V8.02 }
    function    GetSrcAddress6 : String;          { V8.02 }
    procedure   SetSocketFamily(const Value: TSocketFamily);  { V8.02 }
    function    GetSocketFamily : TSocketFamily;  { V8.02 }
    procedure   SetPingMsg(Value : String);       { V8.02 }
    function    GetPingMsg : String;              { V8.02 }
    procedure   SetSize(Value : Integer);
    function    GetSize : Integer;
    procedure   SetTimeout(Value : Integer);
    function    GetTimeout : Integer;
    function    GetReply : TIcmpEchoReply;
    function    GetReply6 : TIcmpV6EchoReply;    { V8.02 }
    function    GetErrorCode : Integer;
    function    GetErrorString : String;
    function    GetLastErrStr : String;         { V8.02 }
    function    GetHostName : String;
    function    GetHostIP : String;
    procedure   SetTTL(Value : Integer);
    function    GetTTL : Integer;
    procedure   Setflags(Value : Integer);
    function    Getflags : Integer;
    function    GetICMPHandle : HModule;
    function    GetReplyIP : String;
    function    GetReplyStatus: Integer;
    function    GetReplyRTT : Integer;
    function    GetReplySize : Integer;
    procedure   IcmpEchoReply(Sender: TObject; Error : Integer);
    procedure   IcmpEchoRequest(Sender: TObject);
    procedure   IcmpDisplay(Sender: TObject; Msg: String);
    procedure   DnsLookupDone (Sender: TObject; Error: Word);
  public
    constructor Create(Owner : TComponent); override;
    destructor  Destroy; override;
    function    Ping : Integer; virtual;
    function    PingAsync : Integer; virtual;     { V8.02 }
    procedure   DnsLookup(HostName : String); virtual;
    procedure   CancelDnsLookup;

    property    Reply         : TIcmpEchoReply read GetReply;
    property    ReplyIPv6     : TIcmpV6EchoReply read GetReply6;   { V8.02 }
    property    ErrorCode     : Integer        read GetErrorCode;
    property    ErrorString   : String         read GetErrorString;
    property    LastErrStr    : String         read GetLastErrStr; { V8.02 }
    property    HostName      : String         read GetHostName;
    property    HostIP        : String         read GetHostIP;
    property    DnsResult     : String         read FDnsResult;
    property    ICMPDLLHandle : HModule        read GetICMPHandle;
  published
    property    Address     : String         read  GetAddress
                                             write SetAddress;
    property    SocketFamily: TSocketFamily  read  GetSocketFamily
                                             write SetSocketFamily;   { V8.02 }
    property    SrcAddress  : String         read  GetSrcAddress
                                             write SetSrcAddress;     { V8.02 }
    property    SrcAddress6 : String         read  GetSrcAddress6
                                             write SetSrcAddress6;    { V8.02 }
    property    PingMsg     : String         read  GetPingMsg
                                             write SetPingMsg;        { V8.02 }
    property    Size        : Integer        read  GetSize
                                             write SetSize;
    property    Timeout     : Integer        read  GetTimeout
                                             write SetTimeout;
    property    TTL         : Integer        read  GetTTL
                                             write SetTTL;
    property    Flags       : Integer        read  Getflags
                                             write SetFlags;
    property    ReplyIP     : String         read  GetReplyIP;        { V8.02 }
    Property    ReplyRTT    : Integer        read  GetReplyRTT;       { V8.02 }
    Property    ReplyStatus : Integer        read  GetReplyStatus;    { V8.02 }
    Property    ReplySize   : Integer        read  GetReplySize;      { V8.02 }
    property    OnDisplay   : TPingDisplay   read  FOnDisplay
                                             write FOnDisplay;
    property    OnEchoRequest : TPingRequest read  FOnEchoRequest
                                             write FOnEchoRequest;
    property    OnEchoReply   : TPingReply   read  FOnEchoReply
                                             write FOnEchoReply;
    property    OnDnsLookupDone : TDnsLookupDone
                                             read  FOnDnsLookupDone
                                             write FOnDnsLookupDone;
    property    OnBgException;               { V6.01 }
  end;

 { V8.02 added threaded ping }
type
    TPingThread = class(TThread)
    private
        FIcmp: TICMP;
    public
        PingHostName:       string;     // host name or IP address to ping
        PingSocketFamily:   TSocketFamily; // IPv4 or IPv6 address
        PingSrcAddress:     String;     // Source IPv4 Address given
        PingSrcAddress6:    String;     // Source IPv6 Address given
        PingPingMsg:        String;     // The message to ping
        PingLookupReply:    boolean;    // if true, ReplyAddress is reverse DNS looked-up to ReplyHostName
        PingSize:           Integer;    // bytes of data to ping
        PingTimeout:        Integer;    // milliseconds ping timeout
        PingTTL:            Integer;    // ping Time To Live, to stop ping short of host
        PingFlags:          Integer;    // ping options
        PingId:             integer;    // available in terminate event to distinguish multiple pings
        PingThreadNum:      integer;    // thread number (not ThreadId) to used in terminate event to remove thread
        DnsHostIP:          String;     // IP address looked up for PingHostName
        ReplyIPAddr:        String;     // Replying IP address (integer)
        ReplyStatus:        DWORD;      // IP status value
        ReplyRTT:           DWORD;      // Round Trip Time in milliseconds
        ReplyDataSize:      Integer;    // Reply data size
        ReplyTotal:         Integer;    // Number of replies to ping
        ReplyHostName:      String;     // Reply host name (if PingLookupReply=true)
        ErrCode:            Integer;    // non-zero ping or DNS error
        ErrString:          String ;    // error literal if errcode non-zero
        procedure Execute; override;
    end;


{$ENDIF MSWINDOWS}

implementation

{$IFDEF MSWINDOWS}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.AbortComponent; { V6.01 }
begin
    try
        CancelDnsLookup;
        { .. more ? }
    except
    end;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TPing.Create(Owner : TComponent);
begin
    Inherited Create(Owner);
    FIcmp               := TICMP.Create;
    FIcmp.OnDisplay     := IcmpDisplay;
    FIcmp.OnEchoRequest := IcmpEchoRequest;
    FIcmp.OnEchoReply   := IcmpEchoReply;
    FDnsSocket          := TWsocket.Create (Self) ;   { V8.02 }
    FDnsSocket.OnDnsLookupDone := DnsLookupDone ;
    FDnsLookPending     := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TPing.Destroy;
begin
    CancelDnsLookup;                 { Cancel any pending dns lookup      }
    if Assigned(FDnsSocket) then begin      { V8.02 }
        FDnsSocket.Destroy;
        FDnsSocket := nil;
    end;
    if Assigned(FIcmp) then begin
        FIcmp.Destroy;
        FIcmp := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.IcmpDisplay(Sender: TObject; Msg: String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Sender, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.IcmpEchoReply(Sender: TObject; Error : Integer);
begin
    if Assigned(FOnEchoReply) then
        FOnEchoReply(Self, Sender, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.IcmpEchoRequest(Sender: TObject);
begin
    if Assigned(FOnEchoRequest) then
        FOnEchoRequest(Self, Sender);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.Ping : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.Ping
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.PingAsync : Integer;       { V8.02 }
begin
    if Assigned(FIcmp) then
        Result := FIcmp.PingAsync
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.DnsLookupDone (Sender: TObject; Error: Word);    { V8.02 }
var
    MyFamily: TSocketFamily;
begin
    if Error = 0 then begin
        FDnsResult := FDnsSocket.DnsResult;
        if Assigned(FIcmp) then begin
            if WSocketIsIP(FDnsResult, MyFamily) then
                            FIcmp.SocketFamily := MyFamily;
        end;
    end;
    FDnsLookPending := FALSE;
    if Assigned(FOnDnsLookupDone) then
        FOnDnsLookupDone(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.CancelDnsLookup;
begin
    if NOT FDnsLookPending then exit;
    FDnsSocket.CancelDnsLookup;
    FDnsLookPending := FALSE;
    if Assigned(FOnDnsLookupDone) then
        FOnDnsLookupDone(Self, WSAEINTR);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.DnsLookup(HostName : String);
var
    LSocketFamily: TSocketFamily;
begin
    { If the address is either a valid IPv4 or IPv6 address, shik a DNS lookup }
    { change current SocketFamily.                          }
    if WSocketIsIP(HostName, LSocketFamily) then begin
        if Assigned(FIcmp) then FIcmp.SocketFamily := LSocketFamily;
        if (LSocketFamily = sfIPv4) or (LSocketFamily = sfIPv6) then begin
            FDnsResult := HostName;
            if Assigned(FOnDnsLookupDone) then
                    FOnDnsLookupDone(Self, 0);
            exit;
        end;
    end;
    FDnsLookPending := TRUE;
    if Assigned(FIcmp) then FDnsSocket.SocketFamily := FIcmp.SocketFamily;
    FDnsSocket.DnsLookup (HostName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetAddress(Value : String);
begin
    if Assigned(FIcmp) then
        FIcmp.Address := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetAddress : String;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.Address;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetSrcAddress(Value : String);    { V8.02 }
begin
    if Assigned(FIcmp) then
        FIcmp.SrcAddress := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetSrcAddress : String;           { V8.02 }
begin
    if Assigned(FIcmp) then
        Result := FIcmp.SrcAddress;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetSrcAddress6(Value : String);    { V8.02 }
begin
    if Assigned(FIcmp) then
        FIcmp.SrcAddress6 := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetSrcAddress6 : String;           { V8.02 }
begin
    if Assigned(FIcmp) then
        Result := FIcmp.SrcAddress6;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetSocketFamily(const Value: TSocketFamily);  { V8.02 }
begin
    if Assigned(FIcmp) then
        FIcmp.SocketFamily := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetSocketFamily : TSocketFamily;  { V8.02 }
begin
    if Assigned(FIcmp) then
        Result := FIcmp.SocketFamily
    else
        Result := sfAny;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetPingMsg(Value : String);       { V8.02 }
begin
    if Assigned(FIcmp) then
        FIcmp.PingMsg := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetPingMsg : String;              { V8.02 }
begin
    if Assigned(FIcmp) then
        Result := FIcmp.PingMsg;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetSize(Value : Integer);
begin
    if Assigned(FIcmp) then
        FIcmp.Size := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetSize : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.Size
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetTimeout(Value : Integer);
begin
    if Assigned(FIcmp) then
        FIcmp.Timeout := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetTimeout : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.Timeout
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetTTL(Value : Integer);
begin
    if Assigned(FIcmp) then
        FIcmp.TTL := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetTTL : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.TTL
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPing.SetFlags(Value : Integer);
begin
    if Assigned(FIcmp) then
        FIcmp.Flags := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetFlags : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.flags
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetReply : TIcmpEchoReply;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.Reply
    else
        FillChar(Result, SizeOf(Result), 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetReply6 : TIcmpV6EchoReply;    { V8.02 }
begin
    if Assigned(FIcmp) then
        Result := FIcmp.Reply6
    else
        FillChar(Result, SizeOf(Result), 0);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetReplyIP : String;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.ReplyIP
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetReplyStatus: Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.ReplyStatus
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetReplyRTT : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.ReplyRTT
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetReplySize : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.ReplySize
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetErrorCode : Integer;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.ErrorCode
    else
        Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetErrorString : String;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.ErrorString
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetLastErrStr : String;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.LastErrStr
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetHostName : String;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.HostName
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetHostIP : String;
begin
    if Assigned(FIcmp) then
        Result := FIcmp.HostIP
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TPing.GetICMPHandle: HModule;
begin
     Result := FIcmp.ICMPdllhandle;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// TPingThread
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure TPingThread.Execute;
var
    RevHost: string;
begin
    FIcmp := TIcmp.Create;
    try  // finally
    try  // except

    // see if overriding default parameters
        ErrCode := 0;
        ErrString := '';
        if PingSize <> 0 then FIcmp.Size := PingSize;
        if PingTimeout <> 0 then FIcmp.Timeout := PingTimeout;
        if PingTTL <> 0 then FIcmp.TTL := PingTTL;
        if PingPingMsg <> '' then FIcmp.PingMsg := PingPingMsg;
        FIcmp.Flags := PingFlags;
        FIcmp.SocketFamily := PingSocketFamily;
        FIcmp.Address := PingHostName;
        FIcmp.SrcAddress := PingSrcAddress;
        FIcmp.SrcAddress6 := PingSrcAddress6;

    // blocking ping, will also lookup IP address if required
        ReplyTotal := FIcmp.Ping;
        DnsHostIP := FIcmp.HostIP;
        if ReplyTotal <> 0 then begin
            PingSocketFamily := FIcmp.SocketFamily;
            ReplyIPAddr := FIcmp.ReplyIp;
            ReplyHostName := ReplyIPAddr;    // may get reversed looked up
            ReplyStatus := FIcmp.ReplyStatus;
            ReplyRTT := FIcmp.ReplyRTT;
            ReplyDataSize := FIcmp.ReplySize;

    // see if now looking up host name for reply address,
    // which might not be the same address ping if TTL was less
            if PingLookupReply and (ReplyIpAddr <> '') then begin
                RevHost := String (WSocketResolveIp(AnsiString
                                    (ReplyIpAddr), PingSocketFamily));
                if RevHost <> '' then ReplyHostName := RevHost;
            end ;
        end
        else begin // ping failed
            ErrCode := FIcmp.ErrorCode;
            ErrString := FIcmp.LastErrStr;
            if ErrString = '' then ErrString := 'Ping failed, no error message';
        end ;
    except
        on E: Exception do begin
            ErrString := E.Message;
            ErrCode := 0;
        end;
    end;
    finally
        FIcmp.Destroy;
    end ;
    Terminate ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$ENDIF MSWINDOWS}
end.

