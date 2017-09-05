{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels
              Contact address <forename.surname@gmx.de>
Description:  Crossplatform socket utilities for ICS.
Creation:     January 2012
Version:      8.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2012 by François PIETTE and Author
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

History:
Jan 04, 2012 Moved code from unit OverbyteIcsWinsock2 here and made some
             breaking changes to support OSX and IPv6.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Mar 21, 2016 - V8.01 - added Types to remove inline warning

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsSocketUtils;
{$ENDIF}

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }
{$J+}           { Allow typed constant to be modified }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  OverbyteIcsWinsock,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Base,
  Posix.SysSocket,
  Posix.NetIf,
  Posix.NetinetIn,
  Posix.ArpaInet,
{$ENDIF}
  {$IFDEF RTL_NAMESPACES}System.Types{$ELSE}Types{$ENDIF},    { V8.01 }
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF};

type
    TIcsSockAddrGen = record
    case Integer of
        0: (Address    : sockaddr);
        1: (AddressIn  : sockaddr_in);
        2: (AddressIn6 : sockaddr_in6);
    end;
    PIcsSockAddrGen = ^TIcsSockAddrGen;

    TIcsInterfaceInfo = record
        iiFlags            : LongWord;         // Interface flags
        iiAddress          : TIcsSockAddrGen;  // Interface address
        iiBroadcastAddress : TIcsSockAddrGen;  // Broadcast address
        iiNetmask          : TIcsSockAddrGen;  // Network mask
    end;
    PIcsInterfaceInfo = ^TIcsInterfaceInfo;

    TInterfaceList = class(TList)
    private
        FOwnsObjects: Boolean;
    protected
        procedure Notify(Ptr: Pointer; Action: TListNotification); override;
        function  GetItem(Index: Integer): PIcsInterfaceInfo;
        procedure SetItem(Index: Integer; IInfo: PIcsInterfaceInfo);
    public
        constructor Create; overload;
        constructor Create(AOwnsObjects: Boolean); overload;
        function  Add(IInfo: PIcsInterfaceInfo): Integer;
        function  Extract(IInfo: PIcsInterfaceInfo): PIcsInterfaceInfo;
        function  Remove(IInfo: PIcsInterfaceInfo): Integer;
        function  IndexOf(IInfo: PIcsInterfaceInfo): Integer;
        procedure Insert(Index: Integer; IInfo: PIcsInterfaceInfo);
        function  First: PIcsInterfaceInfo;
        function  Last: PIcsInterfaceInfo;
        property  OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
        property  Items[Index: Integer]: PIcsInterfaceInfo read GetItem write SetItem; default;
    end;

    procedure IcsGetInterfaceList(InterfaceList : TInterfaceList); overload;
    procedure IcsGetInterfaceList(StrList : TStrings); overload;
    function  IcsAddrSameSubNet(const LocalIPv4Addr, SomeIPv4Addr: in_addr) : Boolean; overload;
    function  IcsAddrSameSubNet(const LocalIpv4Addr, SomeIPv4Addr : AnsiString) : Boolean; overload;
    function  IcsIsIpPrivate(saddr : in_addr): Boolean;

implementation

uses
{$IFDEF FMX}
  Ics.Fmx.OverbyteIcsWSocket
{$ELSE}
  OverbyteIcsWSocket
{$ENDIF}  
  ;

{$IFDEF MSWINDOWS}
type
    TXSockAddrIn6Old = record
        sin6_family   : short;    // AF_INET6
        sin6_port     : u_short;  // Transport level port number
        sin6_flowinfo : u_long;   // IPv6 flow information
        sin6_addr     : TIn6Addr; // IPv6 address
    end;
    PXSockAddrIn6Old = ^TXSockAddrIn6Old;

    TXSockAddrGen = record
    case Integer of
        0: (Address    : sockaddr);
        1: (AddressIn  : TSockAddrIn);
        2: (AddressIn6 : TXSockAddrIn6Old);
    end;
    PXSockAddrGen = ^TXSockAddrGen;

    TXInterfaceInfo = record
        iiFlags            : u_long;        // Interface flags
        iiAddress          : TXSockAddrGen;  // Interface address
        iiBroadcastAddress : TXSockAddrGen;  // Broadcast address
        iiNetmask          : TXSockAddrGen;  // Network mask
    end;
    PXInterfaceInfo = ^TXInterfaceInfo;

    {*                            // Ws2def.h
    * SockAddr Information
    *}
    PSocketAddress = ^TSocketAddress;
    TSocketAddress = record
      lpSockaddr: PSocketAddress;
      iSockaddrLength: Integer;
    end;

    (*
    * Address list returned via SIO_ADDRESS_LIST_QUERY
    *)
    PSocketAddressList = ^TSocketAddressList;
    TSocketAddressList = record
      iAddressCount: Integer;
      Address: array[0..0] of TSocketAddress;
    end;

const
    IFF_UP              = $00000001; // Interface is up.
    IFF_BROADCAST       = $00000002; // Broadcast is  supported.
    IFF_LOOPBACK        = $00000004; // This is loopback interface.
    IFF_POINTTOPOINT    = $00000008; // This is point-to-point interface.
    IFF_MULTICAST       = $00000010; // Multicast is supported.
{$ENDIF MSWINDOWS}

{$IFDEF POSIX}
type
  u_char = UInt8;
  u_short = UInt16;

  sockaddr_dl = record
    sdl_len: u_char;    //* Total length of sockaddr */
    sdl_family: u_char; //* AF_LINK */
    sdl_index: u_short; //* if != 0, system given index for interface */
    sdl_type: u_char;   //* interface type */
    sdl_nlen: u_char;   //* interface name length, no trailing 0 reqd. */
    sdl_alen: u_char;   //* link level address length */
    sdl_slen: u_char;   //* link layer selector length */
    sdl_data: array[0..11] of AnsiChar; //* minimum work area, can be larger;
                                        //contains both if name and ll address */
  end;
  psockaddr_dl = ^sockaddr_dl;

const
  IFT_ETHER = $6;  //if_types.h

function getifaddrs(var ifap: pifaddrs): Integer; cdecl;
   external libc name _PU + 'getifaddrs';
{.$EXTERNALSYM getifaddrs}

procedure freeifaddrs(ifp: pifaddrs); cdecl;
   external libc name _PU + 'freeifaddrs';
{.$EXTERNALSYM freeifaddrs}
{$ENDIF POSIX}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsIsIpPrivate(saddr: in_addr): Boolean;
begin
    Result := (Byte(PIcsInAddr(@saddr).S_un_b.s_b1) = 10) or   // private class A
              (PIcsInAddr(@saddr).S_un_w.s_w1       = 4268) or // private class B
              (PIcsInAddr(@saddr).S_un_w.s_w1       = 43200);  // private class C
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.Add(IInfo: PIcsInterfaceInfo): Integer;
begin
    Result := inherited Add(IInfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TInterfaceList.Create;
begin
    inherited Create;
    FOwnsObjects := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TInterfaceList.Create(AOwnsObjects: Boolean);
begin
    inherited Create;
    FOwnsObjects := AOwnsObjects;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.Extract(IInfo: PIcsInterfaceInfo): PIcsInterfaceInfo;
begin
    Result := PIcsInterfaceInfo(inherited Extract(IInfo));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.First: PIcsInterfaceInfo;
begin
    Result := PIcsInterfaceInfo(inherited First);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.GetItem(Index: Integer): PIcsInterfaceInfo;
begin
    Result := PIcsInterfaceInfo(inherited Items[Index]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.IndexOf(IInfo: PIcsInterfaceInfo): Integer;
begin
    Result := inherited IndexOf(IInfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TInterfaceList.Insert(Index: Integer; IInfo: PIcsInterfaceInfo);
begin
    inherited Insert(Index, IInfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.Last: PIcsInterfaceInfo;
begin
    Result := PIcsInterfaceInfo(inherited Last);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TInterfaceList.Notify(Ptr: Pointer; Action: TListNotification);
begin
    if OwnsObjects then
        if Action = lnDeleted then
            Dispose(PIcsInterfaceInfo(Ptr));
    inherited Notify(Ptr, Action);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TInterfaceList.Remove(IInfo: PIcsInterfaceInfo): Integer;
begin
    Result := inherited Remove(IInfo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TInterfaceList.SetItem(Index: Integer; IInfo: PIcsInterfaceInfo);
begin
    inherited Items[Index] := IInfo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsGetInterfaceList(InterfaceList : TInterfaceList);
{$IFDEF MSWINDOWS}
var
    NumInterfaces   : Integer;
    BytesReturned   : Cardinal;
    PBuf            : PAnsiChar;
    P               : PAnsiChar;
    I               : Integer;
    Err             : Integer;
    BufSize         : Cardinal;
    PInfo           : PIcsInterfaceInfo;
    s               : TSocket;
    v6info          : PSocketAddressList;
    sa6             : PSockAddrIn6;
begin
    if not Assigned(InterfaceList) then
        Exit;
    InterfaceList.Clear;
    BufSize := 20 * SizeOf(TXInterfaceInfo);
    GetMem(PBuf, BufSize);
    try
        s := WSocket_Socket(AF_INET, SOCK_DGRAM, IPPROTO_IP{IPPROTO_UDP});
        if (s = INVALID_SOCKET) then
            raise ESocketException.Create(
                           'IcsGetInterfaceList: Socket creation failed');
        try
            while True do
            begin
                { Returns IPv4 only! Assumes IPv4 } // In Vista+ no loopback interface ?
                Err := WSocket_WSAIoCtl(s, SIO_GET_INTERFACE_LIST, nil, 0,
                                        PBuf, BufSize, BytesReturned,
                                        nil, nil);
                if Err = SOCKET_ERROR then
                    Err := WSocket_WSAGetLastError;
                case Err of
                    0 : Break;
                    WSAEFAULT :
                        begin
                            // How many interfaces make sense ??
                            if BufSize >= 1000 * SizeOf(TXInterfaceInfo) then
                                raise ESocketException.Create(
                               'IcsGetInterfaceList: Too many interfaces');
                            // No chance to get correct buffer size w/o probing
                            Inc(BufSize, 20 * SizeOf(TXInterfaceInfo));
                            FreeMem(PBuf);
                            GetMem(PBuf, BufSize);
                        end;
                    else
                        raise ESocketException.Create('IcsGetInterfaceList: ' +
                           GetWinsockErr(Err) + ' Error #' + IntToStr(Err));
                end;
            end;
        finally
            WSocket_Closesocket(s);
        end;
        if BytesReturned < SizeOf(TXInterfaceInfo) then
            Exit;
        P := PBuf;
        NumInterfaces := BytesReturned div SizeOf(TXInterfaceInfo);
        for I := 0 to NumInterfaces - 1 do
        begin
            if PXInterfaceInfo(P)^.iiFlags and IFF_UP <> IFF_UP then
                Continue;
            {if PXInterfaceInfo(P)^.iiAddress.AddressIn.sin_addr.S_addr = INADDR_ANY then
                Continue;}
            New(PInfo);
            FillChar(PInfo^, SizeOf(TIcsInterfaceInfo), 0);
            PInfo^.iiFlags := PXInterfaceInfo(P)^.iiFlags;
            PInfo^.iiAddress.Address := PXInterfaceInfo(P)^.iiAddress.Address;
            PInfo^.iiBroadcastAddress.Address := PXInterfaceInfo(P)^.iiBroadcastAddress.Address;
            PInfo^.iiNetmask.Address := PXInterfaceInfo(P)^.iiNetmask.Address;
            InterfaceList.Add(PInfo);
            Inc(P, SizeOf(TXInterfaceInfo));
        end;
    finally
        FreeMem(PBuf);
    end;

    { IPv6 ignore any error }
    s := WSocket_Socket(AF_INET6, SOCK_DGRAM, IPPROTO_IP);
    if s = INVALID_SOCKET then
        Exit;
    try
        BufSize := SizeOf(TSocketAddressList) + (63 * SizeOf(TSocketAddress));
        GetMem(PBuf, BufSize);
        try
            v6info := PSocketAddressList(PBuf);

            Err := WSocket_WSAIoctl(s,
                            SIO_ADDRESS_LIST_QUERY,
                            nil,
                            0,
                            v6info,
                            BufSize,
                            BytesReturned,
                            nil,
                            nil);
            if Err = SOCKET_ERROR then
                Exit;

            NumInterfaces := v6info.iAddressCount;
            for I := 0 to NumInterfaces - 1 do
            begin
                sa6 := PSockAddrIn6(v6info.Address[I].lpSockaddr);
                if sa6.sin6_family <> AF_INET6 then
                    Continue;
                if IN6_IS_ADDR_UNSPECIFIED(@sa6^.sin6_addr) then // ?
                    Continue;
                New(PInfo);
                FillChar(PInfo^, SizeOf(TIcsInterfaceInfo), 0);
                PInfo^.iiAddress.AddressIn6 := sa6^;
                InterfaceList.Add(PInfo);
            end;
        finally
            FreeMem(PBuf);
        end;
    finally
        WSocket_Closesocket(s);
    end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  ifap, Next: pifaddrs;
  PInfo: PIcsInterfaceInfo;
begin
    if getifaddrs(ifap) <> 0 then
        raise ESocketException.Create('IcsGetInterfaceList getifaddrs(), Error: ' +
                                      SysErrorMessage(GetLastError));
    try
      Next := ifap;
      while Next <> nil do begin
        if (Next.ifa_flags and IFF_UP = IFF_UP) and
           //(Next.ifa_flags and IFF_LOOPBACK = 0) and // Vista+ doesn't return the loopback interface ?
           ((Next.ifa_addr.sa_family = AF_INET) or (Next.ifa_addr.sa_family = AF_INET6)) then
        begin
            New(PInfo);
            FillChar(PInfo^, SizeOf(TIcsInterfaceInfo), 0);
            PInfo^.iiFlags := Next.ifa_flags;
            if Next.ifa_addr.sa_family = AF_INET then begin
                Move(Next.ifa_addr^, PInfo^.iiAddress.Address, SizeOf(sockaddr));
                Move(Next.ifa_netmask^, PInfo^.iiNetmask.Address, SizeOf(sockaddr));
                if Next.ifa_flags and IFF_BROADCAST = IFF_BROADCAST then
                    Move(Next.ifa_dstaddr^, PInfo^.iiBroadcastAddress.Address, SizeOf(sockaddr));
            end
            else
                Move(Next.ifa_addr^, PInfo^.iiAddress.Address, SizeOf(sockaddr_in6));
            InterfaceList.Add(PInfo);
        end;
        Next := Next.ifa_next;
      end;
    finally
      freeifaddrs(ifap);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsGetInterfaceList(StrList : TStrings); overload;
var
    iList : TInterfaceList;
    I     : Integer;
begin
    if not Assigned(StrList) then
        Exit;
    StrList.Clear;
    iList := TInterfaceList.Create;
    try
       IcsGetInterfaceList(iList);
        for I := 0 to IList.Count -1 do begin
            if IList[I]^.iiAddress.AddressIn.sin_family = AF_INET then
                StrList.Add(String(WSocket_inet_ntoa(
                     IList[I]^.iiAddress.AddressIn.sin_addr)))
            else if IList[I]^.iiAddress.AddressIn6.sin6_family = AF_INET6 then
                StrList.Add(WSocketIPv6ToStr(@IList[I]^.iiAddress.AddressIn6));
        end;
    finally
        iList.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsAddrSameSubNet(const LocalIPv4Addr, SomeIPv4Addr: in_addr) : Boolean;
var
    iList : TInterfaceList;
    I     : Integer;
    iInfo : PIcsInterfaceInfo;
    IFS_addr: {$IFDEF POSIX} LongWord {$ELSE} Integer {$ENDIF};
begin
    Result := FALSE;
    iList := TInterfaceList.Create;
    try
       IcsGetInterfaceList(iList);
        for I := 0 to iList.Count -1 do
        begin
            iInfo := IList[I];
            if iInfo.iiAddress.Address.sa_family <> AF_INET then
                Continue;
            IFS_addr := iInfo.iiAddress.addressIn.sin_addr.S_addr and
                        iInfo.iiNetMask.addressIn.sin_addr.S_addr;
            if (IFS_addr = (LocalIPv4Addr.S_addr and iInfo.iiNetMask.addressIn.sin_addr.S_addr)) and
               (IFS_addr = (SomeIPv4Addr.S_addr and iInfo.iiNetMask.addressIn.sin_addr.S_addr)) then
            begin
                Result := TRUE;
                Exit;
            end;
        end;
    finally
        iList.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  IcsAddrSameSubNet(const LocalIpv4Addr, SomeIPv4Addr : AnsiString) : Boolean;
var
    Loc, Some : TInAddr;
begin
    if (LocalIpv4Addr <> '') and (SomeIPv4Addr <> '') then begin
        Loc.S_addr := WSocket_inet_addr(PAnsiChar(LocalIpv4Addr));
        Some.S_addr := WSocket_inet_addr(PAnsiChar(SomeIPv4Addr));
        Result := IcsAddrSameSubNet(Loc, Some);
    end
    else
        Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.

