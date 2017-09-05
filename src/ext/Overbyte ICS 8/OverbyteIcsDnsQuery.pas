{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Component to query DNS records.
              Implement a subset of RFC 1035 (A and MX records).
Creation:     January 29, 1999
Version:      8.00
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1999-2010 by François PIETTE
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
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Feb 14, 1999 V0.02 Indirectly call winsock functions using wsocket because
             wsocket provide runtime dynamic link instead of loadtime link.
             This allows a program to use DnsQuery if it discover that winsock
             is installed and still run if winsock is not installed.
Feb 24, 1999 V1.00 Added code for reverse lookup (PTR record).
Mar 07, 1999 V1.01 Adapted for Delphi 1
Aug 20, 1999 V1.02 Revise compile time option. Adapted for BCB4
Jul 27, 2001 V1.03 Holger Lembke <holger@hlembke.de> implemented a few new
                   queries or propreties (QueryAny, LongLatToDMS, Loc2Geo, Loc)
                   and related data types.
Sep 04, 2003 V1.04 Replaced all htons by WSocket_htons
May 31, 2004 V1.05 Used ICSDEFS.INC
Nov 19, 2004 V1.06 Added Multithreaded property
Mar 06, 2005 V1.07 DecodeAnswer has been fixed to avoid winsock ntohs and
                   ntohl function which have range check errors because Borland
                   defined the function as returning LongInt instead of Cardinal
May 29, 2005 V1.08 Jack <jlist9@gmail.com> added TCP support
Mar 26, 2006 V6.00 New version 6 started
Jun 05, 2008 A. Garrels made some changes to prepare code for Unicode
Aug 11, 2008 V6.02 A. Garrels - Type AnsiString rolled back to String.
Oct 09, 2009 V6.03 Yaroslav Chernykh fixed a bug in WSocketSessionConnected()
                   when using UDP.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsDnsQuery;
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

interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Ics.Posix.WinTypes,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsWSocket,
{$ELSE}
    OverbyteIcsWSocket,
{$ENDIF}
    OverbyteIcsWinsock;

const
  DnsQueryVersion    = 800;
  CopyRight : String = ' TDnsQuery  (c) 1999-2012 F. Piette V8.00 ';

  { Maximum answers (responses) count }
  MAX_ANCOUNT     = 50;
  { Maximum number of MX records taken into account in responses }
  MAX_MX_RECORDS  = 50;
  MAX_A_RECORDS   = 50;
  MAX_PTR_RECORDS = 10;

  { DNS Classes }
  DnsClassIN      = 1;   { The internet                                      }
  DnsClassCS      = 2;   { The CSNET class (obsolete, used only for examples)}
  DnsClassCH      = 3;   { The CHAOS class                                   }
  DnsClassHS      = 4;   { Hesiod name service                               }
  DnsClassALL     = 255; { Any class                                         }

  { Type of query/response a DNS can handle }
  DnsQueryA       = 1;  { A     HostAddress                                  }
  DnsQueryNS      = 2;  { NS    Authoritative name server                    }
  DnsQueryMD      = 3;  { MD    MailDestination, obsolete, use Mail Exchange }
  DnsQueryMF      = 4;  { MF    MailForwarder, obsolete, use Mail Exchange   }
  DnsQueryCNAME   = 5;  { CNAME CanonicalName                                }
  DnsQuerySOA     = 6;  { SOA   Start of a Zone of Authority                 }
  DnsQueryMB      = 7;  { MB    MailBox, experimental                        }
  DnsQueryMG      = 8;  { MG    MailGroup, experimental                      }
  DnsQueryMR      = 9;  { MR    MailRename, experimental                     }
  DnsQueryNULL    = 10; { NULL  Experimental                                 }
  DnsQueryWKS     = 11; { WKS   Well Known Service Description               }
  DnsQueryPTR     = 12; { PTR   Domain Name Pointer                          }
  DnsQueryHINFO   = 13; { HINFO Host Information                             }
  DnsQueryMINFO   = 14; { MINFO Mailbox information                          }
  DnsQueryMX      = 15; { MX    Mail Exchange                                }
  DnsQueryTXT     = 16; { TXT   Text Strings                                 }
  { !!KAP!! }
  DnsQueryRP      = 17;
  DnsQueryAFSDB   = 18;
  DnsQueryX25     = 19;
  DnsQueryISDN    = 20;
  DnsQueryRT      = 21;
  DnsQueryNSAP    = 22;
  DnsQueryNSAPPTR = 23;
  DnsQuerySIG     = 24; { see RFC-2065                                       }
  DnsQueryKEY     = 25; { see RFC-2065                                       }
  DnsQueryPX      = 26;
  DnsQueryGPOS    = 27; { GPOS has the following format:
                          <owner> <ttl> <class> GPOS <longitude> <latitude> <altitude> }
  DnsQueryAAAA    = 28; { see IP6 Address                                    }
  DnsQueryLOC     = 29; (* see RFC-1876  http://rfc.net/rfc1876.html
                         <owner> <TTL> <class> LOC ( d1 [m1 [s1]] {"N"|"S"} d2 [m2 [s2]]
                               {"E"|"W"} alt["m"] [siz["m"] [hp["m"]
                               [vp["m"]]]] )
                        *)
  DnsQueryNXT     = 30; { see RFC-2065                                       }

  DnsQuerySRV     = 33; { see RFC-2052                                       }
  DnsQueryNAPTR   = 35; { see RFC-2168                                       }
  DnsQueryKX      = 36;

  { Some additional type only allowed in queries }
  DnsQueryAXFR    = 252; { Transfer for an entire zone                       }
  DnsQueryMAILB   = 253; { Mailbox related records (MB, MG or MR)            }
  DnsQueryMAILA   = 254; { MailAgent, obsolete, use MX instead               }
  DnsQueryALL     = 255; { Request ALL records                               }

  { Opcode field in query flags }
  DnsOpCodeQUERY  = 0;
  DnsOpCodeIQUERY = 1;
  DnsOpCodeSTATUS = 2;

type
  TDnsAnswerNameArray   = packed array [0..MAX_ANCOUNT - 1]     of AnsiString;
  TDnsAnswerTypeArray   = packed array [0..MAX_ANCOUNT - 1]     of Integer;
  TDnsAnswerClassArray  = packed array [0..MAX_ANCOUNT - 1]     of Integer;
  TDnsAnswerTTLArray    = packed array [0..MAX_ANCOUNT - 1]     of LongInt;
  TDnsAnswerTagArray    = packed array [0..MAX_ANCOUNT - 1]     of Integer;
  TDnsMXPreferenceArray = packed array [0..MAX_MX_RECORDS - 1]  of Integer;
  TDnsMXExchangeArray   = packed array [0..MAX_MX_RECORDS - 1]  of AnsiString;
  TDnsAddressArray      = packed array [0..MAX_A_RECORDS - 1]   of TInAddr;
  TDnsHostnameArray     = packed array [0..MAX_PTR_RECORDS - 1] of AnsiString;

  TDnsRequestDoneEvent = procedure (Sender : TObject; Error : WORD) of Object;
  TDnsRequestHeader = packed record
      ID      : WORD;
      Flags   : WORD;
      QDCount : WORD;
      ANCount : WORD;
      NSCount : WORD;
      ARCount : WORD;
  end;
  PDnsRequestHeader = ^TDnsRequestHeader;

  TLOCInfo = packed record { need to be 16 bytes }
    version    : byte;
    size       : byte;
    horizpre   : byte;
    vertpre    : byte;
    latitude   : longint;
    longitude  : longint;
    altitude   : longint;
  end;
  PLOCInfo = ^TLOCInfo;

  { Decoded TLOCInfo }
  TLogGeo = record
    version             : byte;
    longsize            : integer;
    latsize             : integer;
    horizpre            : integer;
    vertpre             : integer;
    { Latitude, degree, minutes, seconds, milliseconds }
    lad, lam, las, lams : integer;
    lahem               : ansichar;
    { same for Longitude }
    lod, lom, los, loms : integer;
    lohem               : ansichar;
    altitude            : integer;
  end;

  TDnsQuery = class(TComponent)
  protected
    FWSocket                    : TWSocket;
    FPort                       : String;
    FAddr                       : String;
    FIDCount                    : WORD;
    FQueryBuf                   : array [0..511] of ansichar;
    FQueryLen                   : Integer;
    FResponseBuf                : array [0..511] of ansichar;
    FResponseLen                : Integer;
    FResponseID                 : Integer;
    FResponseCode               : Integer;
    FResponseOpCode             : Integer;
    FResponseAuthoritative      : Boolean;
    FResponseTruncation         : Boolean;
    FResponseRecursionAvailable : Boolean;
    FResponseQDCount            : Integer;
    FResponseANCount            : Integer;
    FResponseNSCount            : Integer;
    FResponseARCount            : Integer;
    FQuestionType               : Integer;
    FQuestionClass              : Integer;
    FQuestionName               : AnsiString;
    FAnswerNameArray            : TDnsAnswerNameArray;
    FAnswerTypeArray            : TDnsAnswerTypeArray;
    FAnswerClassArray           : TDnsAnswerClassArray;
    FAnswerTTLArray             : TDnsAnswerTTLArray;
    FAnswerTagArray             : TDnsAnswerTagArray;
    FMXRecordCount              : Integer;
    FMXPreferenceArray          : TDnsMXPreferenceArray; { For MX request  }
    FMXExchangeArray            : TDnsMXExchangeArray;   { For MX request  }
    FARecordCount               : Integer;
    FAddressArray               : TDnsAddressArray;      { For A request   }
    FPTRRecordCount             : Integer;
    FHostnameArray              : TDnsHostnameArray;     { For PTR request }
    FOnRequestDone              : TDnsRequestDoneEvent;
    FProto                      : String;                { default to udp  }
    FGotPacketLength            : Boolean; { for tcp, set if packet length received }
    FLengthByte                 : array [0..1] of BYTE; {  for tcp         }
    fLOCInfo                    : TLOCInfo;
    function GetMXPreference(nIndex : Integer) : Integer;
    function GetMXExchange(nIndex : Integer)   : AnsiString;
    function GetAnswerName(nIndex : Integer)   : AnsiString;
    function GetAnswerType(nIndex : Integer)   : Integer;
    function GetAnswerClass(nIndex : Integer)  : Integer;
    function GetAnswerTTL(nIndex : Integer)    : LongInt;
    function GetAnswerTag(nIndex : Integer)    : Integer;
    function GetAddress(nIndex : Integer)      : TInAddr;
    function GetHostname(nIndex : Integer)     : AnsiString;
    procedure BuildRequestHeader(Dst       : PDnsRequestHeader;
                                 ID        : WORD;
                                 OPCode    : BYTE;
                                 Recursion : Boolean;
                                 QDCount   : WORD;
                                 ANCount   : WORD;
                                 NSCount   : WORD;
                                 ARCount   : WORD); virtual;
    function  BuildQuestionSection(Dst         : PAnsiChar;
                                   const QName : AnsiString;
                                   QType       : WORD;
                                   QClass      : WORD) : Integer; virtual;
    procedure WSocketDataAvailable(Sender: TObject; Error: WORD); virtual;
    procedure WSocketSessionConnected(Sender: TObject; Error: WORD); virtual;
    procedure TriggerRequestDone(Error: WORD); virtual;
    function  GetResponseBuf : PAnsiChar;
    procedure SendQuery;
    function  ExtractName(Base       : PAnsiChar;
                          From       : PAnsiChar;
                          var Name   : AnsiString) : PAnsiChar;
    function  DecodeQuestion(Base       : PAnsiChar;
                             From       : PAnsiChar;
                             var Name   : AnsiString;
                             var QType  : Integer;
                             var QClass : Integer) : PAnsiChar;
    function DecodeAnswer(Base         : PAnsiChar;
                          From         : PAnsiChar;
                          var Name     : AnsiString;
                          var QType    : Integer;
                          var QClass   : Integer;
                          var TTL      : LongInt;
                          var RDataPtr : Pointer;
                          var RDataLen : Integer) : PAnsiChar;
    function DecodeMXData(Base           : PAnsiChar;
                          From           : PAnsiChar;
                          var Preference : Integer;
                          var Exchange   : AnsiString) : PAnsiChar;
    function DecodeAData(Base        : PAnsiChar;
                         From        : PAnsiChar;
                         var Address : TInAddr) : PAnsiChar;
    function DecodePTRData(Base         : PAnsiChar;
                           From         : PAnsiChar;
                           var Hostname : AnsiString) : PAnsiChar;
    function  GetMultiThreaded: Boolean;
    procedure SetMultiThreaded(const Value: Boolean);
    procedure SetProto(const Value : String);
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Notification(AComponent: TComponent; operation: TOperation); override;
    function    MXLookup(Domain : AnsiString) : Integer;
    function    ALookup(Host : AnsiString) : Integer;
    function    PTRLookup(IP : AnsiString) : Integer;
    function    QueryAny(Host : AnsiString; QNumber : Integer) : Integer;
    property ResponseID                 : Integer read FResponseID;
    property ResponseCode               : Integer read FResponseCode;
    property ResponseOpCode             : Integer read FResponseOpCode;
    property ResponseAuthoritative      : Boolean read FResponseAuthoritative;
    property ResponseTruncation         : Boolean read FResponseTruncation;
    property ResponseRecursionAvailable : Boolean read FResponseRecursionAvailable;
    property ResponseQDCount            : Integer read FResponseQDCount;
    property ResponseANCount            : Integer read FResponseANCount;
    property ResponseNSCount            : Integer read FResponseNSCount;
    property ResponseARCount            : Integer read FResponseARCount;
    property ResponseBuf                : PAnsiChar   read GetResponseBuf;
    property ResponseLen                : Integer read FResponseLen;
    property QuestionType               : Integer read FQuestionType;
    property QuestionClass              : Integer read FQuestionClass;
    property QuestionName               : AnsiString  read FQuestionName;
    property AnswerName[nIndex : Integer]   : AnsiString  read GetAnswerName;
    property AnswerType[nIndex : Integer]   : Integer read GetAnswerType;
    property AnswerClass[nIndex : Integer]  : Integer read GetAnswerClass;
    property AnswerTTL[nIndex : Integer]    : LongInt read GetAnswerTTL;
    property AnswerTag[nIndex : Integer]    : Integer read GetAnswerTag;
    property MXPreference[nIndex : Integer] : Integer read GetMXPreference;
    property MXExchange[nIndex : Integer]   : AnsiString  read GetMXExchange;
    property Address[nIndex : Integer]      : TInAddr read GetAddress;
    property Hostname[nIndex : Integer]     : AnsiString  read GetHostname;
    property Loc                            : TLOCInfo read fLOCInfo;
  published
    property Port    : String read  FPort  write FPort;
    property Addr    : String read  FAddr  write FAddr;
    property Proto   : String read  FProto write SetProto;
    property MultiThreaded   : Boolean            read  GetMultiThreaded
                                                  write SetMultiThreaded;
    property OnRequestDone : TDnsRequestDoneEvent read  FOnRequestDone
                                                  write FOnRequestDone;
  end;


function ReverseIP(const IP : AnsiString) : AnsiString;
function LongLatToDMS(longlat : longint; hemis : AnsiString) : AnsiString; { !!KAP!! }
function Loc2Geo(loc : TLOCInfo) : TLogGeo;                        { !!KAP!! }

implementation

type
    PWORD  = ^WORD;
    PDWORD = ^DWORD;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ReverseIP(const IP : AnsiString) : AnsiString;
var
    I, J : Integer;
begin
    Result := '';
    if Length(IP) = 0 then
        Exit;
    J      := Length(IP);
    I      := J;
    while I >= 0 do begin
        if (I = 0) or (IP[I] = '.') then begin
            Result := Result + '.' + Copy(IP, I + 1, J - I);
            J := I - 1;
        end;
        Dec(I);
    end;
    if Result[1] = '.' then
        Delete(Result, 1, 1);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TDnsQuery.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FWSocket         := TWSocket.Create(nil);
    FPort            := '53';
    FProto           := 'udp';
    FGotPacketLength := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TDnsQuery.Destroy;
begin
    if Assigned(FWSocket) then begin
        FWSocket.Destroy;
        FWSocket := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.Notification(AComponent: TComponent; operation: TOperation);
begin
    inherited Notification(AComponent, operation);
    if operation = opRemove then begin
        if AComponent = FWSocket then
            FWSocket := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetMXPreference(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FMXPreferenceArray)) or
       (nIndex > High(FMXPreferenceArray)) then
        Result := 0
    else
        Result := FMXPreferenceArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetMXExchange(nIndex : Integer) : AnsiString;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FMXExchangeArray)) or
       (nIndex > High(FMXExchangeArray)) then
        Result := ''
    else
        Result := FMXExchangeArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerName(nIndex : Integer) : AnsiString;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerNameArray)) or
       (nIndex > High(FAnswerNameArray)) then
        Result := ''
    else
        Result := FAnswerNameArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerType(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerTypeArray)) or
       (nIndex > High(FAnswerTypeArray)) then
        Result := 0
    else
        Result := FAnswerTypeArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerClass(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerClassArray)) or
       (nIndex > High(FAnswerClassArray)) then
        Result := 0
    else
        Result := FAnswerClassArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerTTL(nIndex : Integer) : LongInt;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerTTLArray)) or
       (nIndex > High(FAnswerTTLArray)) then
        Result := 0
    else
        Result := FAnswerTTLArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAnswerTag(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerTagArray)) or
       (nIndex > High(FAnswerTagArray)) then
        Result := 0
    else
        Result := FAnswerTagArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetAddress(nIndex : Integer) : TInAddr;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAddressArray)) or
       (nIndex > High(FAddressArray)) then
        Result.S_addr := 0
    else
        Result := FAddressArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetHostname(nIndex : Integer) : AnsiString;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FHostnameArray)) or
       (nIndex > High(FHostnameArray)) then
        Result := ''
    else
        Result := FHostnameArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetResponseBuf : PAnsiChar;
begin
    Result := @FResponseBuf;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.MXLookup(Domain : AnsiString) : Integer;
begin
    Inc(FIDCount);
    BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
    FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)], Domain, DnsQueryMX, DnsClassIN);
    FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
    Result    := FIDCount;
    SendQuery;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.ALookup(Host : AnsiString) : Integer;
begin
    Inc(FIDCount);
    BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
    FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)], Host, DnsQueryA, DnsClassIN);
    FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
    Result    := FIDCount;
    SendQuery;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ !!KAP!! }
function TDnsQuery.QueryAny(Host : AnsiString; QNumber : integer) : Integer;
begin
    Inc(FIDCount);
    BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
    FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)], Host, QNumber, DnsClassIN);
    FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
    Result    := FIDCount;
    SendQuery;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.PTRLookup(IP : AnsiString) : Integer;
begin
    Inc(FIDCount);
    BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
    FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)],
                                      ReverseIP(IP) + '.in-addr.arpa',
                                      DnsQueryPTR, DnsClassIN);
    FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
    Result    := FIDCount;
    SendQuery;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.SendQuery;
begin
    FResponseLen                := -1;
    FGotPacketLength            := FALSE;
    FWSocket.OnDataAvailable    := nil;
    FWSocket.Abort;
    FWSocket.OnDataAvailable    := WSocketDataAvailable;
    FWSocket.OnSessionConnected := WSocketSessionConnected;
    FWSocket.Proto              := FProto;
    FWSocket.Port               := FPort;
    FWSocket.Addr               := FAddr;
    FWSocket.Connect;
    { Note: UDP is connectionless, nevertheless, TWSocket call              }
    { OnSessionConnected event handler immediately. For TCP the event       }
    { handler is called only when session is connected (or fails to)        }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.BuildQuestionSection(
    Dst         : PAnsiChar;
    const QName : AnsiString;
    QType       : WORD;
    QClass      : WORD) : Integer;
var
    I   : Integer;
    p   : PAnsiChar;
    Ptr : PAnsiChar;
begin
    Ptr := Dst;
    if Ptr = nil then begin
        Result := 0;
        Exit;
    end;
    I := 1;
    while I <= Length(QName) do begin
        p := Ptr;
        Inc(Ptr);
        while (I <= Length(QName)) and (QName[I] <> '.') do begin
            Ptr^ := QName[I];
            Inc(Ptr);
            Inc(I);
        end;
        p^ := AnsiChar(Ptr - p - 1);
        Inc(I);
    end;
    Ptr^ := #0;
    Inc(Ptr);
    PWORD(Ptr)^ := WSocket_htons(QType);
    Inc(Ptr, 2);
    PWORD(Ptr)^ := WSocket_htons(QClass);
    Inc(Ptr, 2);
    Result := Ptr - Dst;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.BuildRequestHeader(
    Dst       : PDnsRequestHeader;
    ID        : WORD;
    OPCode    : BYTE;
    Recursion : Boolean;
    QDCount   : WORD;
    ANCount   : WORD;
    NSCount   : WORD;
    ARCount   : WORD);
begin
    if Dst = nil then
        Exit;
    Dst^.ID      := WSocket_htons(ID);
    Dst^.Flags   := WSocket_htons((OpCode shl 11) + (Ord(Recursion) shl 8));
    Dst^.QDCount := WSocket_htons(QDCount);
    Dst^.ANCount := WSocket_htons(ANCount);
    Dst^.NSCount := WSocket_htons(NSCount);
    Dst^.ARCount := WSocket_htons(ARCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.TriggerRequestDone(Error: WORD);
begin
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.WSocketDataAvailable(Sender: TObject; Error: WORD);
var
    Len    : Integer;
    Ans    : PDnsRequestHeader;
    Flags  : Integer;
    P      : PAnsiChar;
    RDataPtr : Pointer;
    RDataLen : Integer;
    I        : Integer;
begin
    if FProto = 'tcp' then begin
        if not FGotPacketLength then begin
            Len := FWSocket.PeekData(@FLengthByte, 2);
            if Len < 2 then
                Exit;
            FWSocket.Receive(@FLengthByte, 2);
            FGotPacketLength := TRUE;
        end;

        if not FGotPacketLength then
            Exit
        else begin
            Ans := PDnsRequestHeader(@FResponseBuf);
            Len := FWSocket.PeekData(Ans, FLengthByte[0] * 256 + FLengthByte[1]);
            if Len < FLengthByte[0] * 256 + FLengthByte[1] then
                Exit;
            Len := FWSocket.Receive(Ans, FLengthByte[0] * 256 + FLengthByte[1]);
            if Error <> 0 then begin
                TriggerRequestDone(Error);
                Exit;
            end;
        end;
    end
    else begin
        Ans := PDnsRequestHeader(@FResponseBuf);
        Len := FWSocket.Receive(Ans, SizeOf(FResponseBuf));
        if Error <> 0 then begin
            TriggerRequestDone(Error);
            Exit;
        end;
    end;
    { Check for minimum response length }
    if Len < SizeOf(TDnsRequestHeader) then
        Exit;
    Flags := WSocket_ntohs(Ans^.Flags);
    { Check if we got a response }
    if (Flags and $8000) = 0 then
        Exit;
    FResponseLen := Len;
    { Decode response header }
    FResponseID                 := WSocket_ntohs(Ans^.ID);
    FResponseCode               := Flags and $000F;
    FResponseOpCode             := (Flags shr 11) and $000F;
    FResponseAuthoritative      := (Flags and $0400) = $0400;
    FResponseTruncation         := (Flags and $0200) = $0200;
    FResponseRecursionAvailable := (Flags and $0080) = $0080;
    FResponseQDCount            := WSocket_ntohs(Ans^.QDCount);
    FResponseANCount            := WSocket_ntohs(Ans^.ANCount);
    FResponseNSCount            := WSocket_ntohs(Ans^.NSCount);
    FResponseARCount            := WSocket_ntohs(Ans^.ARCount);

    P := @ResponseBuf[SizeOf(TDnsRequestHeader)];
    if FResponseQDCount = 0 then begin
        { I don't think we could receive 0 questions }
        FQuestionName  := '';
        FQuestionType  := 0;
        FQuestionClass := 0;
    end
    else begin
        { Should never be greater than 1 because we sent only one question }
        P := DecodeQuestion(@FResponseBuf, P,
                            FQuestionName, FQuestionType, FQuestionClass);
    end;
    if FResponseANCount = 0 then begin
        RDataPtr        := nil;
        RDataLen        := 0;
        FMXRecordCount  := 0;
        FARecordCount   := 0;
        FPTRRecordCount := 0;
    end
    else begin
        FMXRecordCount  := 0;
        FARecordCount   := 0;
        FPTRRecordCount := 0;
        for I := 0 to FResponseANCount - 1 do begin
            P := DecodeAnswer(@FResponseBuf,        P,
                              FAnswerNameArray[I],  FAnswerTypeArray[I],
                              FAnswerClassArray[I], FAnswerTTLArray[I],
                              RDataPtr,             RDataLen);
            FAnswerTagArray[I] := -1;
            case FAnswerTypeArray[I] of
            DnsQueryMX:
                begin
                    if FMXRecordCount <= High(FMXPreferenceArray) then begin
                        FAnswerTagArray[I] := FMXRecordCount;
                        DecodeMXData(@FResponseBuf, RDataPtr,
                                     FMXPreferenceArray[FMXRecordCount],
                                     FMXExchangeArray[FMXRecordCount]);
                        Inc(FMXRecordCount);
                    end;
                end;
            DnsQueryA:
                begin
                    if FARecordCount <= High(FAddressArray) then begin
                        FAnswerTagArray[I] := FARecordCount;
                        DecodeAData(@FResponseBuf, RDataPtr,
                                    FAddressArray[FARecordCount]);
                        Inc(FARecordCount);
                    end;
                end;
            DnsQueryPTR:
                begin
                    if FPTRRecordCount <= High(FHostnameArray) then begin
                        FAnswerTagArray[I] := FPTRRecordCount;
                        DecodePTRData(@FResponseBuf, RDataPtr,
                                      FHostnameArray[FPTRRecordCount]);
                        Inc(FPTRRecordCount);
                    end;
                end;
            { !!KAP!! }
            DnsQueryLOC:
                begin
                    { for security reasons, if recompiled with future versions of delphi }
                    if (RDataLen = 16) and (rdatalen = sizeof(fLOCInfo)) then
                        Move(rdataptr^, fLOCInfo, 16)
                    else
                        FillChar(fLOCInfo, SizeOf(fLOCInfo), 0);
                end;
            end;
        end;
        FWSocket.Close;
    end;
    TriggerRequestDone(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.WSocketSessionConnected(Sender: TObject; Error: WORD);
var
    Buf: array [0..1] of BYTE;
begin
    if Error = 0 then begin
        if FProto = 'tcp' then begin { V6.03 }
            Buf[0] := FQueryLen div 256;
            Buf[1] := FQueryLen mod 256;
            { Send 2 byte length for tcp packets, see RFC 1035 - 4.2.2. TCP usage }
            FWSocket.Send(@Buf[0], 2);
        end;
        FWSocket.Send(@FQueryBuf, FQueryLen);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.ExtractName(
    Base       : PAnsiChar;
    From       : PAnsiChar;
    var Name   : AnsiString) : PAnsiChar;
var
    N       : Integer;
    I       : Integer;
    P       : PAnsiChar;
    NameEnd : AnsiString;
begin
    P := From;
    if P^ = #0 then begin
        Name := '';
        Inc(P);
    end
    else begin
        Name := '';
        while TRUE do begin
            { Get name part length }
            N := Ord(P^);
            if (N and $C0) = $C0 then begin
                 { Message compression }
                 N := ((N and $3F) shl 8) + Ord(P[1]);
                 if Length(Name) = 0 then
                     Self.ExtractName(Base, Base + N, Name)
                 else begin
                     Self.ExtractName(Base, Base + N, NameEnd);
                     Name := Name + NameEnd;
                 end;
                 Inc(P, 2);
                 break;
            end;
            Inc(P);
            if N = 0 then
                break;
            { Copy name part }
            for I := 1 to N do begin
                Name := Name + P^;
                Inc(P);
            end;
            if P^ <> #0 then
                Name := Name + '.';
        end;
    end;
    Result := P;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.DecodeQuestion(
    Base       : PAnsiChar;
    From       : PAnsiChar;
    var Name   : AnsiString;
    var QType  : Integer;
    var QClass : Integer) : PAnsiChar;
var
    P : PAnsiChar;
begin
    P := ExtractName(Base, From, Name);
    QType  := WSocket_ntohs(PWORD(P)^);
    Inc(P, 2);
    QClass := WSocket_ntohs(PWORD(P)^);
    Inc(P, 2);
    Result := P;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ntohs(V : WORD) : Integer;
begin
    Result := ((V and $FF) shl 8) or ((V shr 8) and $FF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ntohl(V : DWORD) : LongInt;
begin
    Result := (ntohs(V and $FFFF) shl 16) or ntohs((V shr 16) and $FFFF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.DecodeAnswer(
    Base         : PAnsiChar;
    From         : PAnsiChar;
    var Name     : AnsiString;
    var QType    : Integer;
    var QClass   : Integer;
    var TTL      : LongInt;
    var RDataPtr : Pointer;
    var RDataLen : Integer) : PAnsiChar;
var
    P : PAnsiChar;
begin
    P        := ExtractName(Base, From, Name);
    QType    := ntohs(PWORD(P)^);  { 06/03/2005 WSocket_ntohs(PWORD(P)^); }
    Inc(P, 2);
    QClass   := ntohs(PWORD(P)^);  { 06/03/2005 WSocket_ntohs(PWORD(P)^); }
    Inc(P, 2);
    TTL      := ntohl(PDWORD(P)^); { 06/03/2005 WSocket_ntohl(PDWORD(P)^); }
    Inc(P, 4);
    RDataLen := ntohs(PWORD(P)^);  { 06/03/2005 WSocket_ntohs(PWORD(P)^) };
    Inc(P, 2);
    RDataPtr := P;
    Result   := P + RDataLen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.DecodeMXData(
    Base           : PAnsiChar;
    From           : PAnsiChar;
    var Preference : Integer;
    var Exchange   : AnsiString) : PAnsiChar;
begin
    Result := From;
    Preference := WSocket_ntohs(PWORD(Result)^);
    Inc(Result, 2);
    Result := ExtractName(Base, Result, Exchange);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.DecodePTRData(
    Base         : PAnsiChar;
    From         : PAnsiChar;
    var Hostname : AnsiString) : PAnsiChar;
begin
    Result := ExtractName(Base, From, Hostname);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.DecodeAData(
    Base        : PAnsiChar;
    From        : PAnsiChar;
    var Address : TInAddr) : PAnsiChar;
begin
    Result := From;
    Address.S_addr := Integer(PDWORD(Result)^);   { 06/03/2005 added cast }
    Inc(Result, 4);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{
  <0><1><129><128><0><1><0><1><0><4><0><5><7>inp
  rise<3>com<0><0><15><0><1><192><12><0>
  <15><0><1><0><1>QV<0><10><0><10><5>drui
  d<192><12><192><12><0><2><0><1><0><1>Qc<0><6><3>
  ns1<192><12><192><12><0><2><0><1><0><1>Qc<0>
  <20><3>NS1<10>SPRINTLINK
  <3>NET<0><192><12><0><2><0><1><0><1>Qc<0>
  <6><3>NS2<192>U<192><12><0><2><0><1><0><1>Q
  c<0><6><3>NS3<192>U<192>+<0><1><0><1><0>
  <1>QV<0><4><143><186><11>F<192>?<0><1><0><1><0>
  <1>Qc<0><4><207>iS<30><192>Q<0><1><0><1><0>
  <2><144>i<0><4><204>u<214><10><192>q<0><1><0><1><0>
  <2><144>i<0><4><199><2><252><10><192><131><0><1><0><1><0>
  <2><142><182><0><4><204>a<212><10>
}
{
  <0><3><129><128><0><1><0><1><0><2><0><3><4>rtf
  m<2>be<0><0><15><0><1><192><12><0><15><0><1><0>
  <1>.b<0><9><0><10><4>mail<192><12><192><12>
  <0><2><0><1><0><1>.b<0><11><2>ns<3>dn
  s<2>be<0><192><12><0><2><0><1><0><1>.b<0>
  <5><2>ns<192><12><192>'<0><1><0><1><0><1>.b
  <0><4><195><0>d<253><192>:<0><1><0><1><0><1>QY
  <0><4><134>:J!<192>Q<0><1><0><1><0><1>.b
  <0><4><195><0>d<253>
}
{
  <0><7><133><128><0><1><0><1><0><2><0><2><3>www
  <4>rtfm<2>be<0><0><1><0><1><192><12><0>
  <1><0><1><0><1>Q<128><0><4><195><0>d<253><4>rt
  fm<2>be<0><0><2><0><1><0><1>Q<128><0><5>
  <2>ns<192>-<192>-<0><2><0><1><0><1>Q<128><0>
  <9><2>ns<3>dns<192>2<192>@<0><1><0><1>
  <0><1>Q<128><0><4><195><0>d<253><192>Q<0><1><0><1>
  <0><0><26><132><0><4><134>:J!
}
(*
<0><1><129><128><0><1><0><1><0><5><0><5><9>fu-berlin
<2>de<0><0>

<29><0><1><192><12><0><29><0><1><0><0>,

<0><16><0><21><22><19><139>Av<167><130><218>L<242>
<0><152><156>\<192><12><0><2><0><1><0><0><12><176>
<0>"<4>arbi<10>informatik<13>uni-oldenburg<2>de<0>
<192><12><0><2><0><1><0><0><12><176><0><12><5>deneb<3>
dfn<192>d<192><12><0><2><0><1><0><0><12><176><0><6><3>
ns3<192><12><192><12><0><2><0><1><0><0><12><176><0><6>
<3>ns2<192><12><192><12><0><2><0><1><0><0><12><176><0>
<6><3>ns1<192><12><192>F<0><1><0><1><0><0>t<169><0><4>
<134>j<1><7><192>t<0><1><0><1><0><0>9<209><0><4><192>L
<176><9><192><140><0><1><0><1><0><0>T<19><0><4><130>
<133><1>9<192><158><0><1><0><1><0><0><28><206><0><4>
<160>-<10><12><192><176><0><1><0><1><0><0>1<198><0>
<4><160>-<8><8>
*)

{ !!KAP!! }
{raw translation of some perl-source LOC.pm from package Net::DNS::RR::LOC;

fu-berlin.de   LOC  52 27 19.591 N 13 17 40.978 E 15.00m 1000.00m 10000.00m 10.00m
}
const conv_sec = 1000.0;
      conv_min = 60.0 * conv_sec;
      conv_deg = 60.0 * conv_min;
      zh31     = 1 shl 31;

procedure SubLOCgeo(longlat : longint;
                    hemis : AnsiString;
                    var ldeg, lmin, lsec, lmsec : Extended;
                    var hemic : AnsiChar);
var
    Labs : Extended;
begin
    LongLat := WSocket_ntohl(LongLat);
    Labs    := Abs(1.0 * LongLat - zh31);
    Ldeg    := Trunc(labs / conv_deg);
    Labs    := Labs - ldeg * conv_deg;
    Lmin    := Trunc(labs / conv_min);
    Labs    := Labs - lmin * conv_min;
    Lsec    := Trunc(labs / conv_sec);
    Labs    := Labs - lsec * conv_sec;
    Lmsec   := Labs;
    Hemic   := Copy(Hemis, 1 + ord(LongLat <= zh31), 1)[1]; { yeah. }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LongLatToDMS(longlat : longint; hemis : AnsiString): AnsiString;
Var ldeg, lmin, lsec, lmsec : extended;
    hemi                    : AnsiChar;
begin
  SubLOCgeo(longlat,hemis,ldeg,lmin,lsec,lmsec,hemi);
  result := AnsiString(Format('%d %02d %02d.%03d',
               [round(ldeg), round(lmin), round(lsec),
                round(lmsec)]) + ' ' + Char(hemi));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ in cm!! }
function LocAltToAlt(Localt : LongInt) : LongInt;
begin
    Result := Round((WSocket_ntohl(localt) - 100000.0 * 100.0) / 100.0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ !!KAP!! }
function Loc2Geo(loc : TLOCInfo):TLogGeo;
  { dolle umwandlung }
  procedure du(longlat : Integer;
               hemis   : AnsiString;
               var ideg, imin, isec, imsec : Integer;
               var hemic : AnsiChar);
  var
      ldeg, lmin, lsec, lmsec : extended;
  begin
      SubLOCgeo(longlat, hemis, ldeg, lmin, lsec, lmsec, hemic);
      ideg  := Round(ldeg);
      imin  := Round(lmin);
      isec  := Round(lsec);
      imsec := Round(lmsec);
  end;

begin
    Result.version  := Loc.version;
    Result.longsize := Round(Exp(Ln(10)*(loc.size and $f)));
    Result.latsize  := Round(Exp(Ln(10)*(loc.size shr 4)));

    Result.horizpre := Loc.horizpre;
    Result.vertpre  := Loc.vertpre;

    du(loc.latitude, 'NS', result.lad, result.lam,
       result.las, result.lams, result.lahem);
    du(loc.longitude, 'EW', result.lod, result.lom,
       result.los, result.loms, result.lohem);

    Result.altitude := LocAltToAlt(loc.altitude);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.SetMultiThreaded(const Value: Boolean);
begin
    if Assigned(FWSocket) then
        FWSocket.Multithreaded := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetMultiThreaded: Boolean;
begin
    if Assigned(FWSocket) then
        Result := FWSocket.Multithreaded
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.SetProto(const Value: String);
var
    Buf : String;
begin
    Buf := LowerCase(Value);
    if not ((Buf = 'tcp') or (Buf = 'udp')) then
        raise Exception.Create('TDnsQuery accept only TCP or UDP protocol');
    FProto := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
