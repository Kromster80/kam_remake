{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Original Author: Ian Baker, ADV Systems 2003
Updated by:   Angus Robertson, Magenta Systems Ltd
Creation:     20 September 2013
Version:      8.03
Description:  Implements a TWSocket-based SMTP server component.
              For further details please see
              RFC-821, RFC-1869, RFC-1870, RFC-1893, RFC-1985,
              RFC-2034, RFC-2025, RFC-2920
EMail:        francois.piette@overbyte.be      http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2004-2015 by François PIETTE
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
//******************************************************************//
// Project      ADVmserve                                           //
//                                                                  //
// Module       WSMTPserver                                         //
//                                                                  //
// Description  Implements a TWSocket-based SMTP server component.  //
//              For further details please see                      //
//              RFC-821, RFC-1869, RFC-1870, RFC-1893, RFC-1985,    //
//              RFC-2034, RFC-2025, RFC-2920                        //
//                                                                  //
//              TWsocket is a component of The Internet Component   //
//              Suite, freely downloadable from                     //
//              http://www.overbyte.be                              //
//                                                                  //
// Copyright    This software is subject to the license at:         //
//              http://www.codecutters.org/software/license.html    //
//              with the additional conditions below:               //
//                                                                  //
//              (i)   This source code is "Open Source"             //
//              (ii)  This source code may be freely distributed &  //
//                    modified, but it's origin must not be         //
//                    misrepresented in any way. This means that    //
//                    this this header must remain intact, with     //
//                    altered portions clearly marked and commented //
//              (iii) This source code may be used in any project,  //
//                    including commercial software; a mention of   //
//                    ADVsystems and a link to the web site would   //
//                    be appreciated, but is not mandatory.         //
//              (iv)  As stated in the license terms, the author    //
//                    accepts no responsibility for damages or costs//
//                    arising from the use or misuse of this        //
//                    software; the software is supplied "as-is",   //
//                    and no claims are made to its merchantability //
//                    or fitness for a given purpose.               //
//              (v)   Please direct and comments/questions to:      //
//                    mailto:support@codecutters.org                //
//                                                                  //
//******************************************************************//
// (C) ADV Systems 2003, All rights reserved.                       //
//******************************************************************//
// Version  Date    Author     Reason                               //
// 1.00     290303  I.Baker    Initial version                      //
// 1.01     300303  I.Baker    Expanded tracing, added Client       //
//                             Context. Also exposed subcomponents  //
// 1.02     100403  I.Baker    Added Return-Path header             //
// 1.03     120403  I.Baker    Added check for zero recipients      //
// 1.04     170403  I.Baker    Allowed unknown commands to be sent  //
//                             to the user handler                  //
// 1.05     270403  I.Baker    Made SMTPtime() accessible           //
// 1.06     300403  I.Baker    Copied OnBgException setting         //
// 2.00     081003  I.Baker    Rewritten to simplify reuse          //
// 2.01     191003  I.Baker    Added ClientID to Command handlers   //
// 2.02     201003  I.Baker    Exposed all utility routines         //
// 2.03     041103  D.Aguirre  Added 2 omitted MaxMsgSize checks in //
//                  Grazio     ClientDataRx                         //
// 2.04     271103  I.Baker    Removed Buffer.Len reset within DATA //
// 2.05     291103  I.Baker    Added MessageID to Action Handler    //
// 2.06     021203  I.Baker    Added Return-Path and Received       //
//                             headers before DATA reception        //
// 2.07     141203  I.Baker    SetActive now virtual, to allow      //
//                             additional control over other        //
//                             components (e.g. a Spooler)          //
//                             Added RaiseExceptionFmt()            //
// 2.08     231203  I.Baker    Counter moved into server, Exception //
//                             in TSmtpSrvCli no longer raised,    //
//                             Added max message size check         //
//******************************************************************//

Sep 24, 2013 V8.00 Angus updated for ICS V8 with IPv6
    Reformatted closer to ICS style, private variables prefixed with F
    Using simple LineMode instead of complex dynamic buffer
    MAIL FROM command is allowed after RCPT TO but resets recipients
    Added separate events for different commands instead of one ActionHandler
    Added Options for multiple boolean configuration options
    Added Multiple Listen addresses, default to 25 and 587, could add IPv6
    Added AUTH command for login authentication with PLAIN, LOGIN, CRAM-MD5 and CRAM-SHA1
    Support VRFY command (but do nothing useful)
    Keep DATA content as Stream to avoid 50 meg memory allocations
    Optionally add Sender, X-Envelope-From and X-Envelope-To headers
    Optionally add X-Sender and X-Receiver SMTP replay headers
    Added many more response codes
    Optionally parse main email content headers
    Add Apparently-To header if no To: header found
    Made enhanced response code optional since seem to be rarely used
    Added LocalAccounts optional list of email addresses for which this server
      can accept local email, otherwise rejected with 550
    Added AliasAccounts optional list of email addresseses whose mail will
      be saved to an existing LocalAccount, in format alias=account, with
      special case of *@domain=account that catches all email for the domain
    Added ToAccounts list similar to MessageTo list of RcpTo, but blank
      if message is being relayed of different account if an alias address
    Added SSL support using STARTTLS command only (port 465 implicit is
       NOT supported since revoked)
    Optionally add X-Originating-IP header
    Optionally only allow authentication after TLS negotiated
    Optionally prevent relaying even with authentication
Dec 18, 2013 V8.01 Angus  EHLO reports AUTH even without SSL
June 2015 V8.02 Angus - fix FMX compile bug
Jan 22, 2016 V8.03 Angus - corrected 64-bit casting bug in PostMessage


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSmtpSrv;

interface

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
{$IFNDEF COMPILER7_UP}
    Bomb = 'No support for ancient compiler';
{$ENDIF}
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       ON}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    OverbyteIcsWinsock,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Variants{$ELSE}Variants{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.StrUtils{$ELSE}StrUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.DateUtils{$ELSE}DateUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Types{$ELSE}Types{$ENDIF},
{$IFNDEF NOFORMS}
  {$IFDEF FMX}
    FMX.Forms,
  {$ELSE}
    {$IFDEF RTL_NAMESPACES}Vcl.Forms{$ELSE}Forms{$ENDIF},
  {$ENDIF}
{$ENDIF}
{$IFDEF POSIX}
    //System.IOUtils,
    Posix.Unistd,
    Posix.SysSocket,
    Ics.Posix.WinTypes,
    Ics.Posix.Messages,
  {$IFDEF MACOS}
    System.Mac.CFUtils,
    Macapi.Corefoundation,
  {$ENDIF}
{$ENDIF}
{$IFDEF USE_SSL}
    OverByteIcsSSLEAY, OverByteIcsLIBEAY,
{$ENDIF}
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsWndControl,
    Ics.Fmx.OverbyteIcsWSocket,
    Ics.Fmx.OverbyteIcsWSockBuf,
    Ics.Fmx.OverbyteIcsWSocketS,
    Ics.Fmx.OverbyteIcsSmtpProt,
{$ELSE}
    OverbyteIcsWndControl,
    OverbyteIcsWSocket,
    OverbyteIcsWSockBuf,
    OverbyteIcsWSocketS,
    OverbyteIcsSmtpProt,  // just using some public utility functions
{$ENDIF}
    OverbyteIcsDnsQuery,
    OverbyteIcsMimeUtils,
    OverbyteIcsMimeDec,
    OverbyteIcsMD5,
    OverbyteIcsSha1,
    OverbyteIcsUtils,
    OverbyteIcsTypes;

const
    SmtpCliVersion     = 803;
    CopyRight : String = ' SMTP Server (c) 1997-2016 Francois Piette V8.03 ';

const
  // ESMTP commands. Please note that not all are implemented - use AddCommand() to add a handler of your own
    cHELO                = 'HELO';
    cEHLO                = 'EHLO';
    cMAIL                = 'MAIL';
    cRCPT                = 'RCPT';
    cDATA                = 'DATA';
    cQUIT                = 'QUIT';
    cEXPN                = 'EXPN';   // not supported, mailing list
    cHELP                = 'HELP';   // not supported
    cTURN                = 'TURN';   // not supported, obsolete
    cRSET                = 'RSET';
    cNOOP                = 'NOOP';
    cVRFY                = 'VFRY';   // always say OK but don't test anything
    cETRN                = 'ETRN';
    cAUTH                = 'AUTH';
{$IFDEF USE_SSL}
    cSTARTTLS            = 'STARTTLS';
{$ENDIF}

    SmtpSrvAuthLits : array [0..4] of PAnsiChar = ('', 'PLAIN', 'LOGIN', 'CRAM-MD5', 'CRAM-SHA1');

  // Number of seconds to wait for a DNS response (both PTR and MX, combined)
    cDNStimeout          = 10;
  // Number of seconds, by default, to time-out a client connection
    cClientTimeout       = 60;

type
  // forward classes
    TSmtpServer           = class;
    TSmtpSrvCli           = class;
    TSmtpSrvCliClass      = class of TSmtpSrvCli;

  // General exception
    ESMTPserver         = class(Exception);
    TExceptionEvent      = procedure (Sender: TObject; E: Exception) of object;

  // authentication types supported
    TSmtpSrvAuthType    = (smtpsAuthNone, smtpsAuthPlain, smtpsAuthLogin,
                                            smtpsAuthCramMD5, smtpsAuthCramSha1);

 // published server options
    TSmtpsOption      = (smtpsAddRecvHeaders, // add Return-Path: and Received: before other SMTP headers
                         smtpsAddEnvHeaders,  // add X-Envelope-From: and X-Envelope-To: after other SMTP headers,
                         smtpsAddReplayHdrs,  // add X-Sender: and multiple X-Receiver: before other SMTP headers
                         smtpsAddIpAddrHdr,   // add X-Originating-IP after other SMTP headers
                         smtpsParseHeaders,   // should main email headers be parsed and saved
                         smtpsAllowOpenRelay, // allow accept mail for other servers, which will spooled and relayed
                         smtpsAllowAuthRelay, // only allow relay with authentication
                         smtpsExtendedResp,   // allow ENHANCEDSTATUSCODES to be supported
                         smtpsAllowTls,       // allow TLS (SSL)
                         smtpsAuthNoTls       // allow authentication without TLS
                        );
    TSmtpsOptions     = set of TSmtpsOption;

   // note, if smtpsParseHeaders=true and To: is missing, header Apparently-To: is added with Rcpt-To envelope
   // note, smtpsAddReplayHdrs is used for Microsoft SMTP Server Replay directory forwarding (similar to Pickup)


  // This describes the client state required to run a particular SMTP primitive
    TSmtpMsgContext     = (mcConnecting, mcConnected, mcCommand, mcWaitTls, mcMessage, mcData, mcAny);

  // Action to be taken for various commands from application events
    TSmtpMailAction     = (
        wsmtpOK,           // 2xx Positive response from the calling application
        wsmtpClosingDown,  // 421 0.0 Closing down
        wsmtpMailboxBusy,  // 450 7.0 Mailbox unavailble or busy
        wsmtpGreyListed,   // 451 4.1 Greylisted, try again later
        wsmtpMsgTooLarge,  // 452 3.1 Client has exceeded maximum message size. Current message has been lost
        wsmtpAuthTempFail, // 454 xxx Authentication temporary failed
        wsmtpSyntaxError,  // 500 5.2 Syntax Error command not recognised
        wsmtpBadParams,    // 501 5.0 Bad parameter
        wsmtpBadCommand,   // 502 5.2 Command not implmented
        wsmtpBadSequence,  // 503 5.0 Command out of sequence
        wsmtpAuthRequired, // 530 5.1 Authentication required
        wsmtpAuthPermFail, // 535 7.8 Authentication failed credentials
        wsmtpBadAccount,   // 550 1.1 No such email account on this server
        wsmtpBadDomain,    // 550 1.2 Specified domain not handled by this server (e.g. could be a relaying request)
        wsmtpAccClosed,    // 550 1.6 Email account has closed, and is no longer valid
        wsmtpAccNotLocal,  // 551 7.1 Email account not local, suggest new account
        wsmtpMailboxFull,  // 552 2.2 Mailbox full, 3.4 Message too big
        wsmtpProhibited,   // 553 7.1 Recipient or complete message rejected through server policy (reason should be given)
        wsmtpSysUnavail,   // 554 3.2 System is not accepting messages (e.g. shutting-down, PM, etc.)
        wsmtpNetError,     // 554 4.0 Network error
        wsmtpCongested,    // 554 4.5 System is congested. Please try again later.
        wsmtpTooMany,      // 554 5.3 Too many recipients specified
        wsmtpBadMedia,     // 554 3.1 Media not supported (e.g. we don't like Base-64 ;o)
        wsmtpListNotAuth,  // 554 3.1 You are not authorised to send messages to this mailing list
        wsmtpListNotRec);  // 554 2.4 Mailing list does not exist

  // This handler implements a given SMTP primitive
    TSmtpCmdHandler = procedure (Sender     : TObject;          // Sender (the client object)
                               const ClientID   : cardinal;         // Unique client ID
                               var   ESMTP      : boolean;          // True if ESMTP has been specified by an EHLO.
                                     Parameters : PAnsiChar) of object; // Command parameters, if present

  // server has new client connection, we might not like it's IP or be busy
    TSmtpConnectEvent     = procedure (Sender    : TObject;
                                       Client    : TObject;
                                       const IpAddr : string;
                                       var Action   : TSmtpmailAction;
                                       var Reason   : string
                                           ) of object;

  // server has disconnected client
    TSmtpDisconnectEvent  = procedure (Sender    : TObject;
                                       Client    : TObject;
                                       Error     : Word
                                          ) of object;

  // server has received a command or sent a response
    TSmtpCommandEvent     = procedure (Sender    : TObject;
                                       Client    : TObject;
                                       const Command : string
                                           ) of object;

  // server has received a command or sent a response
    TSmtpResponseEvent    = procedure (Sender    : TObject;
                                       Client    : TObject;
                                       const Response : string
                                           ) of object;

  // server Mail From command, we may dislike them
    TSmtpMailFromEvent  =   procedure (Sender    : TObject;
                                       Client    : TObject;
                                       const MailFrom : string;
                                       var Action     : TSmtpmailAction;
                                       var Reason     : string
                                           ) of object;

  // server Rcpt To command, do we accept mail for this account, or will we relay it
    TSmtpRcptToEvent  =     procedure (Sender    : TObject;
                                       Client    : TObject;
                                       const RcptTo : string;
                                       var Action   : TSmtpmailAction;
                                       var Reason   : string
                                           ) of object;

  // server AUTH command, do they have an account
    TSmtpAuthEvent  =       procedure (Sender    : TObject;
                                       Client    : TObject;
                                       const UserName : string;
                                       var Password   : string;
                                       var Action     : TSmtpmailAction;
                                       var Reason     : string
                                           ) of object;

  // server Data command, start and end, did we accept it all
    TSmtpDataEvent  =       procedure (Sender    : TObject;
                                       Client    : TObject;
                                       var Action   : TSmtpmailAction;
                                       var Reason   : string
                                           ) of object;

    TSmtpSrvCli = class(TWSocketClient)
    protected
        FID           : cardinal;
        FClientIpAddr : string;
        FSocketFamily : TSocketFamily;
        FListenNr     : integer;
        FServIpAddr   : string;
        FServPort     : string;
        FClientDomain : string;
        FClientRDNS   : string;
        FClientMX     : string;
        FMessageID    : string;
        FMessageFrom  : string;
        FMessageTo    : TStrings;
        FToAccounts   : TStrings;
        FHdrTo        : string;
        FHdrFrom      : string;
        FHdrSubject   : string;
        FHdrDateStr   : string;
        FHdrDateDT    : TDateTime;
        FDoneHdrs     : boolean;
        FDataStream   : TStream;           // Stream created by application used to save email DATA
        FUserName     : string;            // authenticated user name
        FAuthenticated : boolean;          // true if client successfully authenticated
        FTlsDone      : boolean;           // true if TLS/SSL negotiated successfully
              // above are all published

        FESMTP        : boolean;           // did we get an ESMTP header
        FLastContact  : TDateTime;         // for timeouts
        FSmtpServer   : TSmtpServer;       // parent SMTP server
        FRcvdLine     : AnsiString;        // last received line
        FContext      : TSmtpmsgContext;   // current component state
        FAuthWait2nd  : boolean;           // true if next command is expected to be an authentication response
        FDnsQuery     : TDnsQuery;
        FSmtpSrvAuthType: TSmtpSrvAuthType;// authentication type, if any
        FAuthChallenge: AnsiString;        // authentication challenge sent for Cram-Md5 and Cram-Sha1
        FMsg_wmClientLookupDone: UINT;
        FMsg_wmClientClose: UINT;
        procedure      ClientDataRx(Sender : TObject; Error : Word);
        procedure      ProcessCommand(Str : PAnsiChar);
        procedure      AllocateMsgHandlers; override;
        procedure      FreeMsgHandlers; override;
        procedure      WndProc(var MsgRec: TMessage); override;
        procedure      LookupComplete(Sender : TObject; Error : Word);
        procedure      ClearClient;
    public
        DataFileName  : string;
        function       SendAnswer(const Str : String) : integer;
        procedure      SendStatus(const FormatStr : string; const EnhancedStat : string;
                                                                      Args : array of const);
{$IFDEF USE_SSL}
        function       SslSendPlain(Data : TWSocketData; Len : Integer) : Integer;
{$ENDIF}
        constructor    Create(AOwner : TComponent); override;
        destructor     Destroy; override;
     // internal client ID, incremented with each connection, randomised when server starts
        property       ID           : cardinal       read FID;
     // remote client IP address
        property       ClientIpAddr : string         read FClientIpAddr;
     // client socket family, IPv4 or IPv6
        property       SocketFamily : TSocketFamily  read FSocketFamily;
     // server IP address for this client, IPv4 or IPv6
        property       ServIpAddr   : string         read FServIpAddr;
     // server/client port for this client that answered
        property       ServPort     : string         read FServPort;
     // client domain from HELO or EHLO command
        property       ClientDomain : string         read FClientDomain;
     // client host from reverse DNS
        property       ClientRDNS   : string         read FClientRDNS;
     // client MX from reverse DNS
        property       ClientMX     : string         read FClientMX;
     // message ID created from time and FID
        property       MessageID    : string         read FMessageID;
     // MAIL FROM envelope command
        property       MessageFrom  : string         read FMessageFrom;
     // multiple RCPT TO envelope commands
        property       MessageTo    : TStrings       read FMessageTo;
     // authenticated user name
        property       UserName     : string         read FUserName;
     // true if client successfully authenticated
        property       Authenticated : boolean       read FAuthenticated;
     // local account to which mail should be delivered, if non blank, one for each MessageTo
        property       ToAccounts   : TStrings       read FToAccounts;
     // stream for meessage data content, must be opened in application
        property       DataStream   : TStream        read FDataStream write FDataStream;
     // To: parsed email header
        property       HdrTo        : string         read FHdrTo;
     // From: parsed email header
        property       HdrFrom      : string         read FHdrFrom;
     // Subject: parsed email header
        property       HdrSubject   : string         read FHdrSubject;
     // Date: parsed email header as string
        property       HdrDateStr   : string         read FHdrDateStr;
     // Date: parsed email header converted to TDateTime
        property       HdrDateDT    : TDateTime      read FHdrDateDT;
     // flag true once email header end reached
        property       DoneHdrs     : boolean        read FDoneHdrs   write FDoneHdrs;
    end;

  // This is the main server component. Use as-is, or derive something with the required behaviour.
    TSmtpServer = class(TIcsWndControl)  // was (TComponent)
    protected
        FServer           : TWsocketServer;
        FClientClass      : TSmtpSrvCliClass;
        FAddress          : string;
        FServerPort       : string;
        FServerHost       : string;
        FSocketFamily     : TSocketFamily;
        FServerDesc       : string;
        FMaxUsers         : cardinal;
        FMultiThread      : boolean;
        FMaxMsgSize       : integer;
        FDNSaddr          : string;
        FOptions          : TSmtpsOptions;
        FGreyDelaySecs    : integer;
        FLocalAccounts    : TStrings;
        FAliasAccounts    : TStrings;
        FLocalDomains     : TStrings;
              // above are all published

        FCommands         : array of record
                               Cmd     : AnsiString;
                               Context : TSmtpmsgContext;
                               Handler : TSmtpcmdHandler;
                            end;
        FTimeout          : integer;
        FCheckTimer       : TIcsTimer;
        FExtHandler       : TExceptionEvent;
        FCounter          : cardinal;
        FOnServerStarted  : TNotifyEvent;
        FOnServerStopped  : TNotifyEvent;
        FOnConnect        : TSmtpConnectEvent;
        FOnDisconnect     : TSmtpDisconnectEvent;
        FOnCommand        : TSmtpCommandEvent;
        FOnResponse       : TSmtpResponseEvent;
        FOnMailFrom       : TSmtpMailFromEvent;
        FOnRcptTo         : TSmtpRcptToEvent;
        FOnAuth           : TSmtpAuthEvent;
        FOnAuthPW         : TSmtpAuthEvent;
        FOnDataStart      : TSmtpDataEvent;
        FOnDataEnd        : TSmtpDataEvent;
        procedure          SetAddr(AAddr : string);
        procedure          SetPort(APort : string);
        procedure          SetHost(AHost : string);
        procedure          SetAliasAccounts(newValue : TStrings);
        procedure          SetLocalAccounts(newValue : TStrings);
        procedure          SetServerDesc(AName : string);
        procedure          SetTimeout(ATimeout : integer);
        procedure          SetMaxMsgSize(AMsgSize : integer);
        function           GetMultiListenSockets: TWSocketMultiListenCollection;
        procedure          SetMultiListenSockets(const Value: TWSocketMultiListenCollection);
        procedure          ServerSessionClosed(Sender : TObject; Error  : Word);
        procedure          ClientConnect(Sender: TObject; Client: TWSocketClient; Error: Word);
        procedure          ClientDisconnect(Sender: TObject; Client: TWSocketClient; Error: Word);
        procedure          ServerException(Sender : TObject; E : Exception; var CanClose : Boolean);
        procedure          RaiseException(const Message : string);
        procedure          RaiseExceptionFmt(const Format: string; const Args: array of const);
        procedure          CheckClientStatus(Sender : TObject);
        function           GetActive: boolean;
        function           GetClientCount: Integer;
        function           GetClient(nIndex: Integer): TSmtpSrvCli;
       // Adds or replaces a new SMTP command.
        procedure          AddCommand(Cmd : AnsiString; Handler : TSmtpcmdHandler; Context : TSmtpmsgContext = mcCommand);
       // Sends a string to the specified client. Don't forget to add a CRLF in most cases!
        procedure          SendString(Client : TObject; const Str : string);
       // Command handling routines. May be overridden.
        procedure          HandleNOOP(Sender : TObject; const ClientID : cardinal; var ESMTP : boolean; Parameters : PAnsiChar); virtual;
        procedure          HandleQUIT(Sender : TObject; const ClientID : cardinal; var ESMTP : boolean; Parameters : PAnsiChar); virtual;
        procedure          HandleRSET(Sender : TObject; const ClientID : cardinal; var ESMTP : boolean; Parameters : PAnsiChar); virtual;
        procedure          HandleVRFY(Sender : TObject; const ClientID : cardinal; var ESMTP : boolean; Parameters : PAnsiChar); virtual;
        procedure          HandleHELO(Sender : TObject; const ClientID : cardinal; var ESMTP : boolean; Parameters : PAnsiChar); virtual;
        procedure          HandleEHLO(Sender : TObject; const ClientID : cardinal; var ESMTP : boolean; Parameters : PAnsiChar); virtual;
        procedure          HandleMAIL(Sender : TObject; const ClientID : cardinal; var ESMTP : boolean; Parameters : PAnsiChar); virtual;
        procedure          HandleRCPT(Sender : TObject; const ClientID : cardinal; var ESMTP : boolean; Parameters : PAnsiChar); virtual;
        procedure          HandleDATA(Sender : TObject; const ClientID : cardinal; var ESMTP : boolean; Parameters : PAnsiChar); virtual;
        procedure          HandleAUTH(Sender : TObject; const ClientID : cardinal; var ESMTP : boolean; Parameters : PAnsiChar); virtual;
        procedure          HandleAUTH2(Sender : TObject; const ClientID : cardinal; var ESMTP : boolean; Parameters : PAnsiChar); virtual;
        procedure          SendActionFailed(Sender : TObject; Action: TSmtpmailAction; const Reason: string);
    public
       // This routine can be called to update a client when the Action handler has previously returned a wsmtpNotHandled
        constructor        Create(AOwner : TComponent); override;
        destructor         Destroy; override;
        procedure          Start; virtual;
        procedure          Stop; virtual;
        // Check  if a given object is one of our clients
        function           IsClient(SomeThing: TObject) : Boolean;
        // Runtime readonly property which gives number of connected clients
        property           ClientCount        : Integer               read  GetClientCount;
        // Client[] give direct access to anyone of our clients
        property           Client[nIndex: Integer] :  TSmtpSrvCli    read  GetClient;
       // Runtime property which tell the component class which has to be
       // instanciated to handle client connection
        property           ClientClass        : TSmtpSrvCliClass     read  FClientClass      write FClientClass;
        property           WSocketServer      : TWSocketServer       read  FServer           write FServer;
    published
       // is service active
        property           Active             : boolean              read  GetActive;
       // sets multiple listen addresses
        property           MultiListenSockets : TWSocketMultiListenCollection
                                                                     read  GetMultiListenSockets
                                                                     write SetMultiListenSockets;
       // Sets listener address. 0.0.0.0 for all NICs/IPs
        property           Addr               : string               read  FAddress          write SetAddr;
       // Sets listener port
        property           Port               : string               read  FServerPort       write SetPort;
       // sets IP v4 or v6 family
        property           SocketFamily       : TSocketFamily        read  FSocketFamily     write FSocketFamily;
       // Sets host name for mail server - usually published name in MX records
        property           ServerHost         : string               read  FServerHost       write SetHost;
       // Sets friendly service description for welcome
        property           ServerDesc         : string               read  FServerDesc       write SetServerDesc;
       // Sets maximum number of connected clients; if reduced, then current connections will be honoured.
       // Useful for controlled service shutdown as well as licensing limits. 0 for "no limit"
        property           MaxClients         : cardinal             read  FMaxUsers         write FMaxUsers;
       // Set flag if running in a threaded environment
        property           MultiThreaded      : boolean              read  FMultiThread      write FMultiThread;
       // Set maximum message size processed. Can be set dynamically, allowing throttling if storage is running low.
       // 0 for "no limit"
        property           MaxMessageSize     : integer              read  FMaxMsgSize       write SetMaxMsgSize;
       // Address of DNS to be used for all queries
        property           DnsAddress         : string               read  FDNSaddr          write FDNSaddr;
       // Client timeout, in seconds. 0 for no timeout
        property           ClientTimeout      : integer              read  FTimeout          write SetTimeout;
       // Set number of seconds after which GreyListed mail will be accepted on subsequent attempts
        property           GreyDelaySecs      : integer              read  FGreyDelaySecs    write FGreyDelaySecs;
       // set of server options
        property           Options            : TSmtpsOptions        read  FOptions          write FOptions;
       // local accounts handled by this server
        property           LocalAccounts      : TStrings          read  FLocalAccounts    write SetLocalAccounts;
       // accounts that alias a local account, allowing a local account to support multiple addresses
        property           AliasAccounts      : TStrings          read  FAliasAccounts    write SetAliasAccounts;
       // local domains handled by this server, parsed from local and alias accounts
        property           LocalDomains       : TStrings          read  FLocalDomains;
       // Event called for an unhandled exception
        property           OnException        : TExceptionEvent      read  FExtHandler       write FExtHandler;
       // Event when server has started listening
        property           OnServerStarted    : TNotifyEvent         read  FOnServerStarted  write FOnServerStarted;
       // Event when server has stopped listening
        property           OnServerStopped    : TNotifyEvent         read  FOnServerStopped  write FOnServerStopped;
       // Event called for new client connection
        property           OnConnect          : TSmtpConnectEvent    read FOnConnect         write FOnConnect;
       // Event called for client disconnection
        property           OnDisconnect       : TSmtpDisconnectEvent read FOnDisconnect      write FOnDisconnect;
       // Event called for each command received
        property           OnCommand          : TSmtpCommandEvent    read FOnCommand         write  FOnCommand ;
       // Event called for response sent
        property           OnResponse         : TSmtpResponseEvent   read FOnResponse        write FOnResponse;
       // Event called for Mail From command
        property           OnMailFrom         : TSmtpMailFromEvent   read FOnMailFrom        write FOnMailFrom;
       // Event called for each RcptTo command
        property           OnRcptTo           : TSmtpRcptToEvent     read FOnRcptTo          write FOnRcptTo;
       // Event called for Auth command completed
        property           OnAuth             : TSmtpAuthEvent       read FOnAuth            write FOnAuth;
       // Event called for Auth command needs password for Cram verification
        property           OnAuthPW           : TSmtpAuthEvent       read FOnAuthPW          write FOnAuthPW;
       // Event called when Data command is received, open stream
        property           OnDataStart        : TSmtpDataEvent       read FOnDataStart       write FOnDataStart;
       // Event called when Data command content has been received, close stream
        property           OnDataEnd          : TSmtpDataEvent       read FOnDataEnd         write FOnDataEnd;
     end;

{$IFDEF USE_SSL}
    TSslSmtpServer = class(TSmtpServer)
    protected
        FOnSslHandshakeDone                 : TSslHandshakeDoneEvent;
        FOnSslVerifyPeer                    : TSslVerifyPeerEvent;
        function  GetSslContext : TSslContext;
        procedure SetSslContext(Value : TSslContext);
        procedure HandleTLS(Sender : TObject; const ClientID : cardinal; var ESMTP : boolean; Parameters : PAnsiChar); virtual;
        procedure TlsSslVerifyPeer(Sender        : TObject;
                                   var Ok        : Integer;
                                   Cert          : TX509Base); virtual;
        procedure TlsSslHandshakeDone(Sender         : TObject;
                                      ErrCode        : Word;
                                      PeerCert       : TX509Base;
                                      var Disconnect : Boolean); virtual;
    public
        constructor Create(AOwner: TComponent); override;
    published
        property  SslContext         : TSslContext         read  GetSslContext         write SetSslContext;
        property  OnSslVerifyPeer    : TSslVerifyPeerEvent read  FOnSslVerifyPeer      write FOnSslVerifyPeer;
        property  OnSslHandshakeDone : TSslHandshakeDoneEvent  read  FOnSslHandshakeDone  write FOnSslHandshakeDone;
    end;
{$ENDIF} // USE_SSL


// format an IPv6 address with []
function FormatIpAddr (const Addr: string): string ;
// format an IPv6 address with [] and port
function FormatIpAddrPort (const Addr, Port: string): string ;
// strip [] off IPv6 addresses
function StripIpAddr (const Addr: string): string ;
// RFC1123 date parsing
function InetParseDate(DateStr: string): TDateTime;

procedure SkipWhitespace (var Ptr: PAnsiChar);
function  ExtractEmail (var Str: PAnsiChar) : String;
function  ExtractArg (var Str: PAnsiChar) : String;
function  ComputerName : string;

implementation

const
    cBlockSize           = 2048;
    cTempDir             = 'TEMP';
    cTimerInterval       = 2 * 1000;

    NUL                  = #00;
    CR                   = #13;
    LF                   = #10;
    CRLF                 = AnsiString(CR+LF);
    DOT                  = AnsiString('.');
    EOM                  = AnsiString(DOT+CR+LF);
    ESCDOT               = AnsiString(DOT+DOT);

    cAT                  = '@';
    cOK                  = 'OK';

    cFrom                = 'FROM:';
    cSize                = 'SIZE';
    cTo                  = 'TO:';

resourcestring
    xNoAddrWhenActive    = 'Cannot change server address while the server is active';
    xNoPortWhenActive    = 'Cannot change server port while the server is active';
    xNoHostWhenActive    = 'Cannot change declared host name while the server is active';
    xNoDomWhenActive     = 'Cannot change server domain while the server is active';
    xNoNameWhenActive    = 'Cannot change service name while the server is active';
    xServerNotRunning    = 'Server failed to start';
    xClientStat          = 'Error setting client %8.8x status to %s: %s';
    xInvalidObj          = 'Invalid object passed to %s';
    xMaxMsgSizeExceeded  = 'Message size of %ubytes exceeds the configured maximum';

    xShutdown            = 'service unavailable';
    xOutOfSequence       = 'Command out-of-sequence';
    xNoHello             = 'HELO or EHLO is required';
    xBadSIZEparam        = 'SIZE parameter is not compliant with RFC-1870';
    xNoMailFrom          = 'No MAIL FROM has been specified.';
    xBadFROMparam        = 'Use <MAIL FROM: <yourname@yourdomain>. See RFC-821.';
    xBadTOparam          = 'Use RCPT TO: <recipient@theirdomain>. See RFC-821.';
    xBadAccount          = 'No such account here.';
    xBadDomain           = 'Mail for that domain is not accepted here';
    xAccClosed           = 'That account is no longer valid';
    xPolicy              = 'Message not accepted due to policy violation.';
    xNoRecipients        = 'No message recipients have been specified';
    sQueued              = 'Message %s queued';
    xNoSpool             = 'Spool unavailable';
    xMsgTooLarge         = 'Message size exceeds maximum.';
    sReset               = 'Session reset';
    sClosingChannel      = 'closing transmission channel.';
    xTimeout             = 'timeout period exceeded.';
    xNoStorage           = 'insufficient system storage.';
    xTlsAreadyDone       = 'TLS already estabished';

    xNoSysUnavail        = 'System is not currently accepting messages';
    xNetError            = 'Unspecified network error';
    xCongested           = 'System is currenty congested. Please try again later';
    xTooMany             = 'Too many recipients specified';
    xBadMedia            = 'Media not supported';
    xListNotAuth         = 'You are not authorised to send messages to this mailing list';
    xListNotRec          = 'Mailing list does not exist';

    s220                 = '220 %s ESMTP server %s';
    s220s                = '220 Ready to start TLS';              // Angus
    s221                 = '221 %s %s';
    s235                 = '235 Authentication successful';          // Angus
    s250                 = '250 %s';
    s250c                = '250-%s';
    s252                 = '252 Just try using the email address';   // Angus
    s334                 = '334 %s';                                 // Angus
    s354                 = '354 Start mail input; end with <CRLF>.<CRLF>';
    s421                 = '421 %s %s, %s';
    s450                 = '450 Mailbox temporarily unavailable';             // Angus
    s451                 = '451 Greylisted, please try again in %s seconds';  // Angus
    s452                 = '452 Requested action not taken: %s';
    s454                 = '454 Authentication failed';              // Angus
    s454s                = '454 TLS not available';                  // Angus
    s500                 = '500 Invalid command: "%s"';
    s501                 = '501 %s';
    s502                 = '502 Command not implemented: "%s"';
    s503                 = '503 %s';         // command not allowed for specific reason
    s504                 = '504 Unrecognised authentication type';  // Angus
    s530                 = '530 Authentication required';           // Angus
    s535                 = '535 Authentication failed credentials'; // Angus
    s550                 = '550 %s';
    s551                 = '551 Mailbox not local, try %s';         // Angus
    s552                 = '552 Message exceeds fixed maximum message size';
    s553                 = '553 %s';
    s554                 = '554 %s';

//******************************************************************//
//  Routine      Utility routines                                   //
//******************************************************************//

// format an IPv6 address with []

function FormatIpAddr (const Addr: string): string ;
begin
    if (Pos ('.', Addr) = 0) and (Pos ('[', Addr) = 0) and (Pos (':', Addr) > 0) then
        result := '[' + Addr + ']'
    else
        result := Addr ;
end;

// format an IPv6 address with [] and port

function FormatIpAddrPort (const Addr, Port: string): string ;
begin
    result := FormatIpAddr (Addr) + ':' + Port ;
end;

// strip [] off IPv6 addresses

function StripIpAddr (const Addr: string): string ;
begin
    if (Pos ('[', Addr) = 1) and (Addr [Length (Addr)] = ']') then
        result := Copy (Addr, 2, Length (Addr) - 2)
    else
        result := Addr ;
end;

// RFC1123 date parsing borrowed from HttpApp but adapted to allow
// time hh:mm without seconds and for two digit W2K years, and with
//  fewer exceptions and errors
const
// These strings are NOT to be resourced

  Months: array[1..13] of string = (
    'Jan', 'Feb', 'Mar', 'Apr',
    'May', 'Jun', 'Jul', 'Aug',
    'Sep', 'Oct', 'Nov', 'Dec', '');
  DaysOfWeek: array[1..7] of string = (
    'Sun', 'Mon', 'Tue', 'Wed',
    'Thu', 'Fri', 'Sat');

function InetParseDate (DateStr: string): TDateTime;
var
  Month, Day, Year, Hour, Minute, Sec: Integer;
  Parser: TParser;
  StringStream: TStringStream;
  temptime: TDateTime ;

  function GetMonth: Boolean;
  begin
    Month := 1;
    while not Parser.TokenSymbolIs(Months[Month]) and (Month < 13) do Inc(Month);
    Result := Month < 13;
  end;

  procedure GetTime;
  begin
    with Parser do
    begin
      Hour := TokenInt;
      NextToken;
      if Token = ':' then NextToken;
      Minute := TokenInt;
      NextToken;
      if Token = ':' then   // angus, allow missing seconds
      begin
          NextToken;
          Sec := TokenInt;
          NextToken;
      end ;
    end;
  end;

begin
  Sec := 0 ;
  result := 0 ;
  if DateStr = '' then exit ;  // angus, ignore blank
 // angus, special case of missing day of week
  if (DateStr [1] >= '0') and (DateStr [1] <= '9') then DateStr := 'Sun, ' + DateStr ;
  StringStream := TStringStream.Create(DateStr);
  try
    Parser := TParser.Create(StringStream);
    with Parser do
    try
      NextToken;
      if Token = ':' then NextToken;
      NextToken;         // get day of week, might not exixt...
      if Token = ',' then NextToken;
      if GetMonth then
      begin
        NextToken;
        Day := TokenInt;
        NextToken;
        GetTime;
        Year := TokenInt;
      end else
      begin
        Day := TokenInt;
        NextToken;
        if Token = '-' then NextToken;
        GetMonth;
        NextToken;
        if Token = '-' then NextToken;
        Year := TokenInt;
        if Year < 50 then Inc(Year, 2000);   // Y2K pivot
        if Year < 100 then Inc(Year, 1900);
        NextToken;
        GetTime;
      end;
   // avoid exceptions
      if TryEncodeDate (Year, Month, Day, Result) then
      begin
         if TryEncodeTime (Hour, Minute, Sec, 0, temptime) then
                                        result := result + temptime ;
      end ;
    finally
      Free;
    end;
  finally
    StringStream.Free;
  end;
end;

procedure SkipWhitespace(var Ptr : PAnsiChar);
  // Skips whitespace at the start of a PAnsiChar
begin
    while (Ptr^ <> NUL) and (Ptr^ <= ' ') do
        Inc (Ptr);
end;


function ExtractArg(var Str : PAnsiChar) : String;
  // Extracts next argument, remove leading spaces, then until next space
var
    Ptr : PAnsiChar;
begin
  // Advance to first useful character
//    while (Str^ <= ' ') do
//        Inc (Str);
  // Ignore whitespace
    SkipWhitespace (Str);
  // Now locate end of string
    Ptr := Str + 1;
    if Str^ <> NUL then
    while (Ptr^ > ' ') do
      Inc (Ptr);
  // Set Result..
    SetString (Result, Str, Ptr-Str);
  // Move pointer to end of this parameter
    if Ptr^ = NUL then
        Str := Ptr
    else
        Str := Ptr+1;
end;

function ExtractEmail(var Str : PAnsiChar) : String;
  // Extracts an RFC-821 address, removes angle-brackets
var
    Ptr : PAnsiChar;
    i,j : integer;
begin
  // Advance to first useful character
    while (Str^ <= ' ') or (Str^ in ['<',NUL]) do
        Inc (Str);
  // Ignore whitespace
    SkipWhitespace (Str);
  // Now locate end of string
    Ptr := Str + 1;
    if Str^ <> NUL then
    while (Ptr^ > ' ') and (Ptr^ <> '>') do
      Inc (Ptr);
  // Set Result; remove any relay requests while we're at it..
    SetString (Result, Str, Ptr-Str);
    for i := 1 to Length (Result) do
      if (Result [i] = cAT) or (Result [i] = '!') then
          Result [i] := cAT;
    i := Pos (Result, cAT);
    if i > 0 then
    begin
    j := SUCC (i);
    while (j <= Length (Result)) and (Result [j] <> cAT) do
        Inc (j);
    if j <= Length (Result) then
        SetLength (Result, PRED (j));
    end;

  // Move pointer to end of this parameter
    if Ptr^ = NUL then
        Str := Ptr
    else
        Str := Ptr+1;
end;


function ComputerName : string;
  // Returns the Windows host name
var
    NameLen  : cardinal;
    Computer : PChar;
begin
    try
        NameLen  := SUCC(MAX_COMPUTERNAME_LENGTH);
        Computer := AllocMem(NameLen);
        try
            GetComputerName(Computer,NameLen);
            Result := string(Computer);
        finally
            FreeMem(Computer,NameLen);
        end;
    except
        Result := 'localhost';
    end;
end;


//******************************************************************//
//  Component    WSMTPserver                                        //
//******************************************************************//

//******************************************************************//
//  Routine      Constructor                                        //
//******************************************************************//

constructor TSmtpServer.Create(AOwner : TComponent);
var
    i       : integer;
    ListenItem: TWSocketMultiListenItem;
begin
    inherited;

    FClientClass    := TSmtpSrvCli;
    FAddress        := ICS_ANY_HOST_V4;
    FServerPort     := 'smtp';
    FSocketFamily   := sfIPv4;
    FMultiThread    := false;
    FMaxUsers       := 0;
    FDNSaddr        := '';
    SetTimeout (cClientTimeout);
    FOptions        := [smtpsAddRecvHeaders];
    FGreyDelaySecs  := 0;
    FCounter        := Random ((MAXINT div 2) + $10);  // used for mail IDs

  // set host name, ideally public mail server name from DNS, otherwise computer name
    FServerHost := '';
    i := 0;
    while (i < LocalIPlist.Count) and (FServerHost = '') do
    begin
        FServerHost := String(WSocketResolveIP (AnsiString (LocalIPlist [i])));
        Inc(i);
    end;
    if FServerHost = '' then FServerHost := LowerCase (ComputerName);
    if FServerHost = '' then FServerHost := 'local';

    FServerDesc   := CopyRight;

    FLocalAccounts    := TStringList.Create;
    FAliasAccounts    := TStringList.Create;
    FLocalDomains     := TStringList.Create;

    FServer  := TWSocketServer.Create(nil);
    with FServer do
    begin
        Banner             := '';
        BannerTooBusy      := '';
     // add second listening socket
        MultiListenSockets.Add ;
        ListenItem         := MultiListenSockets [0]; // as TWSocketMultiListenItem;
        ListenItem.Addr    := FAddress;
        ListenItem.Port    := '587' ;
        ListenItem.SocketFamily := FSocketFamily;
    end;

  // Define client check timer
    FCheckTimer := TIcsTimer.Create (FServer);
    FCheckTimer.Interval := cTimerInterval;
    FCheckTimer.OnTimer  := CheckClientStatus;
    FCheckTimer.Enabled  := false;

  // Add commands
    SetLength (FCommands, 0);
    AddCommand (cMAIL, HandleMAIL, mcAny);       // MAIL FROM (one)
    AddCommand (cRCPT, HandleRCPT, mcMessage);   // RCPT TO (multiple)
    AddCommand (cDATA, HandleDATA, mcMessage);   // content
    AddCommand (cHELO, HandleHELO, mcConnected);
    AddCommand (cEHLO, HandleEHLO, mcConnected);
    AddCommand (cQUIT, HandleQUIT, mcAny);
    AddCommand (cRSET, HandleRSET, mcAny);
    AddCommand (cNOOP, HandleNOOP, mcCommand);
    AddCommand (cVRFY, HandleVRFY, mcCommand);
    AddCommand (cAUTH, HandleAUTH, mcCommand);
    AddCommand (cTURN, nil);
    AddCommand (cEXPN, nil);
    AddCommand (cETRN, nil);
    AddCommand (cHELP, nil);
  end;


//******************************************************************//
//  Routine      Destructor                                         //
//******************************************************************//

destructor TSmtpServer.Destroy;
begin
    if Active then
        Stop;

    if Assigned (FCheckTimer) then  // must free timer before server, it's descended
    begin
        FCheckTimer.Free;
        FCheckTimer := Nil;
    end;
    if Assigned (FServer) then
    begin
        FServer.Free;
        FServer := nil;
    end;

    FLocalAccounts.Free;
    FAliasAccounts.Free;
    FLocalDomains.Free;

    inherited;
end;


//******************************************************************//
// Start the server. That is make FServer listening to the ports    //
//******************************************************************//
procedure TSmtpServer.Start;
begin
    if NOT Assigned(FServer) then exit;
    if FServer.State = wsListening then exit;
    try
        with FServer do
        begin
            ClientClass        := FClientClass;
            OnClientConnect    := ClientConnect;
            OnClientDisconnect := ClientDisconnect;
            OnBgException      := ServerException;
            OnSessionClosed    := ServerSessionClosed;
            Addr               := FAddress;
            Port               := FServerPort;
            SocketFamily       := FSocketFamily;
            MultiThreaded      := FMultiThread;
            MultiListen;
            if State = wsListening then
            begin
                FCheckTimer.Enabled  := true;
                if Assigned (FOnServerStarted) then
                begin
                    FOnServerStarted (Self);
                end;
            end
            else
                RaiseException(xServerNotRunning);
        end;
    except
        on E : Exception do RaiseException(E.Message);
    end;
end ;

//******************************************************************//
// Stop the server                                                  //
//******************************************************************//
procedure TSmtpServer.Stop;
var
    i : integer;
begin
    FCheckTimer.Enabled := false;
    if not Assigned(FServer) then exit;
    if FServer.State <> wsListening then exit;
    FServer.MultiClose;  // stop listening

// cleanly close active connections
     for i := PRED (FServer.ClientCount) downto 0 do
     begin
        with TSmtpSrvCli (FServer.Client [i]) do
        begin
            if State <> wsClosed then
            begin
                OnSessionClosed := nil;
                SendStatus (s421, '0.0', [FServerHost, xShutdown, sClosingChannel]);
                Close;
            end;
        end;
    end;

  // Disconnect all clients if any left
    FServer.DisconnectAll;
end;

//******************************************************************//
//  Routine      RaiseException & RaiseExceptionFmt                 //
//                                                                  //
//  Description  Raises an exception                                //
//******************************************************************//

procedure TSmtpServer.RaiseException(const Message : string);
begin
    if Assigned(FExtHandler) then
        FExtHandler(Self,ESMTPserver.Create(Message))
    else
        raise ESMTPserver.Create(Message);
end;


procedure TSmtpServer.RaiseExceptionFmt(const Format: string; const Args: array of const);
begin
    if Assigned(FExtHandler) then
        FExtHandler(Self,ESMTPserver.CreateFmt(Format,Args))
    else
        raise ESMTPserver.CreateFmt(Format,Args);
end;

//******************************************************************//
// Get function for ClientCount property from FWSocketServer.       //
//******************************************************************//
function TSmtpServer.GetClientCount;
begin
    if not Assigned(FServer) then
        Result := 0
    else
        Result := FServer.ClientCount;
end;


//******************************************************************//
// Get function for Client[] property from FServer.                 //
//******************************************************************//
function TSmtpServer.GetClient(nIndex : Integer) : TSmtpSrvCli;
begin
    if not Assigned(FServer) then
        Result := nil
    else
        Result := TSmtpSrvCli(FServer.Client[nIndex]);
end;


//******************************************************************//
// Check if an object is one of our clients from FServer.           //
//******************************************************************//
function TSmtpServer.IsClient(SomeThing : TObject) : Boolean;
begin
    if not Assigned(FServer) then
        Result := FALSE
    else
        Result := FServer.IsClient(SomeThing);
end;

//******************************************************************//
// Check if FServer is listening                                    //
//******************************************************************//
function TSmtpServer.GetActive: boolean;
begin
    if not Assigned(FServer) then
        Result := FALSE
    else
        Result := (FServer.State = wsListening);
end;


//******************************************************************//
//  Routine      SetTimeout                                         //
//                                                                  //
//  Description  Sets the client timeout                            //
//******************************************************************//

procedure TSmtpServer.SetTimeout(ATimeout : integer);
begin
    if ATimeout <= 0 then
        FTimeout := ATimeout
    else
        FTimeout := ATimeout;
end;

//******************************************************************//
//  Routine      GetMultiListenSockets, SetMultiListenSockets       //
//                                                                  //
//  Description  Get and Set multiple listen addresses              //
//******************************************************************//

function TSmtpServer.GetMultiListenSockets: TWSocketMultiListenCollection;
begin
    if Assigned(FServer) then
        Result := FServer.MultiListenSockets
    else
        Result := nil;
end;


procedure TSmtpServer.SetMultiListenSockets(const Value: TWSocketMultiListenCollection);
begin
    if Assigned(FServer) then
        FServer.MultiListenSockets := Value;
end;

//******************************************************************//
//  Routine      SetLocalAccounts                                   //
//                                                                  //
//  Description  Set local email account and domains as email addresses //
//******************************************************************//

procedure TSmtpServer.SetLocalAccounts (newValue: TStrings);
var
    I, J, K: Integer;
    Account, Domain: string;
begin
    FLocalAccounts.Clear;
    FLocalDomains.Clear;
    (FLocalAccounts as TStringList).Sorted := true;
    (FLocalDomains as TStringList).Sorted := true;
    for I := 0 to newValue.Count - 1 do
    begin
        Account := IcsLowerCase (newValue.Strings[I]);
        J := Pos ('@', Account);
        if (J > 1) and (Length (Account) >= 3) then
        begin
            if NOT (FLocalAccounts as TStringList).Find (Account, K) then
            begin
                FLocalAccounts.Add (Account);
                Domain := Copy (Account, J + 1, 999);
                if NOT (FLocalDomains as TStringList).Find (Domain, K) then
                    FLocalDomains.Add (Domain);
            end;
        end;
    end;
end;


//******************************************************************//
//  Routine      SetAliasAccounts                                   //
//                                                                  //
//  Description  Set alias email account and domains as email addresses //
//******************************************************************//

procedure TSmtpServer.SetAliasAccounts (newValue: TStrings);
var
    I, J, K: Integer;
    Pair, Alias, Account, Domain: string;
begin
    FAliasAccounts.Clear;
    (FAliasAccounts as TStringList).Sorted := true;
    for I := 0 to newValue.Count - 1 do
    begin
        Pair := IcsLowerCase (newValue.Strings[I]);
        J := Pos ('=', Pair);
        if (J > 1) and (Length (Pair) >= 3) then
        begin
            Alias := Copy (Pair, 1, J - 1);
            Account := Copy (Pair, J + 1, 999);
            if (FLocalAccounts as TStringList).Find (Account, K) then
            begin
                J := Pos ('@', Alias);
                if (J > 1) and (Length (Alias) >= 3) then
                begin
                    if FAliasAccounts.IndexOfName (Alias) < 0 then
                    begin
                        FAliasAccounts.Add (Pair);
                        Domain := Copy (Alias, J + 1, 999);
                        if NOT (FLocalDomains as TStringList).Find (Domain, K) then
                            FLocalDomains.Add (Domain);
                    end;
                end;
            end;
        end;
    end;
end;

//******************************************************************//
//  Routine      CheckClientStatus                                  //
//                                                                  //
//  Description  Runs through attached clients, looking for timeout //
//******************************************************************//

procedure TSmtpServer.CheckClientStatus(Sender : TObject);
var
    i     : integer;
    Time  : TDateTime;
    Delta : integer;
begin
    Time := Now;

    for i := PRED (FServer.ClientCount) downto 0 do
    with TSmtpSrvCli (FServer.Client [i]) do
    begin
        Delta := SecondsBetween (Time, FLastContact);
        if FContext = mcConnecting then
        begin
          // Check for DNS timeout
            if Delta >= cDNStimeout then
                PostMessage (Handle, FMsg_wmClientLookupDone, 0, lParam(Self.FServer.Client[i])) { V8.03}
        end
        else
        begin
          // Check for client timeout
            if (FTimeout > 0) and (Delta > FTimeout) then
            begin
                SendStatus(s221,'0.0',[FServerHost,xTimeout]);
                CloseDelayed;
            end
        end
    end;
end;

//******************************************************************//
//  Routine      SetAddr                                            //
//                                                                  //
//  Description  Sets the listener address                          //
//******************************************************************//

procedure TSmtpServer.SetAddr(AAddr : string);
begin
    if Active then
        RaiseException(xNoAddrWhenActive)
    else
        FAddress := Trim(AAddr);
end;


//******************************************************************//
//  Routine      SetPort                                            //
//                                                                  //
//  Description  Sets the listener port                             //
//******************************************************************//

procedure TSmtpServer.SetPort(APort : string);
begin
    if Active then
        RaiseException (xNoPortWhenActive)
    else
        FServerPort := Trim (APort);
end;


//******************************************************************//
//  Routine      SetHost                                            //
//                                                                  //
//  Description  Sets the host name of the server.                  //
//******************************************************************//

procedure TSmtpServer.SetHost(AHost : string);
begin
    if Active then
        RaiseException (xNoHostWhenActive)
    else
    begin
        FServerHost := Trim (AHost);
    end ;
  end;


//******************************************************************//
//  Routine      SetServerDesc                                      //
//                                                                  //
//  Description  Sets the service description                       //
//******************************************************************//

procedure TSmtpServer.SetServerDesc(AName : string);
begin
    if Active then
        RaiseException (xNoNameWhenActive)
    else
        FServerDesc := Trim (AName);
end;


//******************************************************************//
//  Routine      SetMaxMsgSize                                      //
//                                                                  //
//  Description  Sets the maximum message size                      //
//******************************************************************//

procedure TSmtpServer.SetMaxMsgSize(AMsgSize : integer);
begin
    if AMsgSize <= 0 then
        FMaxMsgSize := 0
  else
        FMaxMsgSize := AMsgSize;
 end;


//******************************************************************//
//  Routine      ServerException                                    //
//                                                                  //
//  Description  Handles a TWSocketServer exception                 //
//******************************************************************//

procedure TSmtpServer.ServerException(Sender   : TObject;
                                      E        : Exception;
                                      var CanClose : Boolean);
begin
    if Sender is TSmtpSrvCli then
        CanClose := true
    else
        RaiseException(E.Message);
end;

//******************************************************************//
//  Routine      ServerException                                    //
//                                                                  //
//  Description  Handles a TWSocketServer exception                 //
//******************************************************************//

procedure TSmtpServer.ServerSessionClosed(Sender : TObject; Error  : Word);
begin
     if Assigned (FOnServerStopped) then
    begin
        FOnServerStopped (Self);
    end;
end;

//******************************************************************//
//  Routine      AddCommand                                         //
//                                                                  //
//  Description  Add an SMTP command handler                        //
//******************************************************************//

procedure TSmtpServer.AddCommand(Cmd     : AnsiString;
                                 Handler : TSmtpcmdHandler;
                                 Context : TSmtpmsgContext);
var
    i : integer;
begin
  // Add NUL terminator and attempt to locate existing entry
    Cmd := Cmd + #00;
    i := Low (FCommands);
    while (i <= High (FCommands)) and (Cmd <> FCommands [i].Cmd) do
        Inc(i);
    if i > High (FCommands) then
    begin
  // Add new command
        SetLength (FCommands, SUCC (Length (FCommands)));
        FCommands [i].Cmd := Cmd;
    end;
  // Set command parameters
    FCommands [i].Context := Context;
    FCommands [i].Handler := Handler;
end;


//******************************************************************//
//  Routine      SendString                                         //
//                                                                  //
//  Description  Send a arbitrary string to a client                //
//******************************************************************//

procedure TSmtpServer.SendString(Client : TObject; const Str : String);
begin
    if Client is TSmtpSrvCli then
    begin
        with TSmtpSrvCli (Client) do
    if TWSocketServer (Owner).IsClient (Client) then
                SendAnswer (Str);
    end
    else
        RaiseException (Format (xInvalidObj, ['SendString()']));
end;


//******************************************************************//
//  Routine      ClientConnect                                      //
//                                                                  //
//  Description  Invoked when someone connects to the server        //
//******************************************************************//

procedure TSmtpServer.ClientConnect(Sender : TObject;
                                     Client : TWSocketClient;
                                     Error  : Word);
var
    ListenItem: TWSocketMultiListenItem;
begin
    with TSmtpSrvCli(Client) do
    begin
        // Assign unique session ID
        if FCounter = $FFFFFFFE then
            FCounter := 1
        else
            Inc (FCounter);
        FID := FCounter;
        FClientIpAddr := FormatIpAddr (Client.PeerAddr);
        FSocketFamily := Client.SocketFamily;
     // find which multiple listen sockets answered
        FListenNr := WSocketServer.MultiListenIndex ;
        if FListenNr = -1 then
        begin
            FServIpAddr := FormatIpAddr (WSocketServer.GetXAddr) ;
            FServPort := WSocketServer.Port ;
        end
        else
        begin
            ListenItem := MultiListenSockets [FListenNr] ;
            FServIpAddr := FormatIpAddr (ListenItem.Addr) ;
            FServPort := ListenItem.Port ;
        end ;
        FSmtpServer := Self;
        LineMode   := TRUE;  // we'll receive single lines
        LineEdit   := FALSE;
        LineEnd    := AnsiChar(LF);  // with just LF for end of line
        if (FMaxUsers > 0) and (cardinal (Self.FServer.ClientCount) >= FMaxUsers) then
        begin
          // Reject connection
            SendStatus(s452, '3.2', ['user limit reached']);
            Close;
        end
        else
        begin
            if FDNSaddr = '' then
                // No DNS available - skip lookup
                PostMessage(Handle, FMsg_wmClientLookupDone, 0, lParam(Client))   { V8.03}
            else
            begin
                FLastContact := Now;
                FDnsQuery := TDNSquery.Create(nil);
                FDnsQuery.Addr          := FDNSaddr;
                FDnsQuery.OnRequestDone := LookupComplete;
                FDnsQuery.PTRLookup (AnsiString (FClientIpAddr));
            end;
        end;
    end;
    // note, OnConnect event is called when reverse DNS lookup completes
end;


//******************************************************************//
//  Routine      ClientDisconnect                                   //
//                                                                  //
//  Description  Invoked when someone disconnects from the server   //
//******************************************************************//

procedure TSmtpServer.ClientDisconnect(Sender : TObject;
                                        Client : TWSocketClient;
                                        Error  : Word);
begin
    if Active and Assigned (FOnDisconnect) then
    begin
        FOnDisconnect (Self, Client, Error);
    end;
  end;


//******************************************************************//
//  Routine      HandleNOOP                                         //
//                                                                  //
//  Description  Handles NOOP command                               //
//******************************************************************//

procedure TSmtpServer.HandleNOOP(Sender : TObject; const ClientID : cardinal;
                                                var ESMTP : boolean; Parameters : PAnsiChar);
begin
    with TSmtpSrvCli (Sender) do
        SendStatus (s250, '0.0', [cOK]);
end;


//******************************************************************//
//  Routine      HandleQUIT                                         //
//                                                                  //
//  Description  Handles QUIT command                               //
//******************************************************************//

procedure TSmtpServer.HandleQUIT(Sender : TObject; const ClientID : cardinal;
                                                var ESMTP : boolean; Parameters : PAnsiChar);
begin
    with TSmtpSrvCli (Sender) do
    begin
        SendStatus (s221, '0.0', [FServerHost, sClosingChannel]);
        CloseDelayed;
    end;
end;


//******************************************************************//
//  Routine      HandleRSET                                         //
//                                                                  //
//  Description  Handles RSET command                               //
//******************************************************************//

procedure TSmtpServer.HandleRSET(Sender : TObject; const ClientID : cardinal;
                                                var ESMTP : boolean; Parameters : PAnsiChar);
begin
    with TSmtpSrvCli (Sender) do
    begin
        FContext := mcCommand;
        ClearClient;
        SendStatus (s250, '0.0', [sReset]);
    end;
end;


//******************************************************************//
//  Routine      HandleVRFY                                         //
//                                                                  //
//  Description  Handles VRFY command                               //
//******************************************************************//

procedure TSmtpServer. HandleVRFY(Sender : TObject; const ClientID : cardinal;
                                        var ESMTP : boolean; Parameters : PAnsiChar);
begin
    with TSmtpSrvCli (Sender) do
    begin
        SendStatus (s252, '0.0', []);  // we don't want to verify email address
    end;
end;

//******************************************************************//
//  Routine      HandleHELO                                         //
//                                                                  //
//  Description  Handles HELO command                               //
//******************************************************************//

procedure TSmtpServer.HandleHELO(Sender : TObject; const ClientID : cardinal;
                                                var ESMTP : boolean; Parameters : PAnsiChar);
begin
    ESMTP := false;
    with TSmtpSrvCli (Sender) do
    begin
        FClientDomain := ExtractEmail (Parameters);
        FContext := mcCommand;
        SendStatus (s250, '', [FServerHost]);
    end
end;


//******************************************************************//
//  Routine      HandleEHLO                                         //
//                                                                  //
//  Description  Handles EHLO command                               //
//******************************************************************//

procedure TSmtpServer.HandleEHLO(Sender : TObject; const ClientID : cardinal;
                                                var ESMTP : boolean; Parameters : PAnsiChar);
var
    CommandList : string;
    i           : integer;
begin
    ESMTP := true;
    with TSmtpSrvCli (Sender) do
    begin
        FClientDomain := ExtractEmail (Parameters);
        FContext := mcCommand;

        CommandList := Format (s250c, [FServerHost]) + CRLF;
        // Now list all non-optional commands
        for i := Low (FCommands) to High (FCommands) do
            if (StrIComp (@FCommands [i].Cmd [1],cMAIL) = 0) and
               (StrIComp (@FCommands [i].Cmd [1],cRCPT) = 0) and
               (StrIComp (@FCommands [i].Cmd [1],cDATA) = 0) and
               (StrIComp (@FCommands [i].Cmd [1],cRSET) = 0) and
               (StrIComp (@FCommands [i].Cmd [1],cNOOP) = 0) and
               (StrIComp (@FCommands [i].Cmd [1],cQUIT) = 0) and
               Assigned( FCommands [i].Handler) then
                    CommandList := CommandList + Format (s250c, [Trim (String (FCommands[i].Cmd))]) + CRLF;
        if FMaxMsgSize > 0 then
            CommandList := CommandList + Format (s250c, ['SIZE '+ IntToStr (FMaxMsgSize)]) + CRLF;
      // might not want to offer AUTH SSL negotiated
        if FTlsDone OR (NOT ((smtpsAuthNoTls in FSmtpServer.FOptions) and
                                 (smtpsAllowTls in FSmtpServer.FOptions))) then    { V8.01 }
            CommandList := CommandList + Format (s250c, ['AUTH PLAIN LOGIN CRAM-MD5 CRAM-SHA1'])+ CRLF;
        if (smtpsExtendedResp in FSmtpServer.FOptions) then
            CommandList := CommandList + Format (s250c, ['ENHANCEDSTATUSCODES'])+ CRLF;
{$IFDEF USE_SSL}
        if (NOT FTlsDone) and (smtpsAllowTls in FSmtpServer.FOptions) then
            CommandList := CommandList + Format (s250c , ['STARTTLS']) + CRLF ;
{$ENDIF}
        CommandList := CommandList + Format (s250 , ['PIPELINING']) + CRLF ;
        SendAnswer (CommandList);
    end
end;

{$IFDEF USE_SSL}
//******************************************************************//
//  Routine      HandleTLS                                          //
//                                                                  //
//  Description  Handles STARTTLS command                           //
//******************************************************************//

procedure TSslSmtpServer.HandleTLS(Sender : TObject; const ClientID : cardinal;
                                                var ESMTP : boolean; Parameters : PAnsiChar);
var
    Answer: AnsiString;
    Client: TSmtpSrvCli;
begin
    ESMTP := false;
    Client := Sender as TSmtpSrvCli;
    if (Client.FContext <> mcCommand) or (NOT (smtpsAllowTls in FOptions)) then
    begin
        Client.SendStatus (s454s, '', ['']);  // failed
        exit;
    end ;
    if Client.FTlsDone or (Client.SslState = sslEstablished) then
    begin
        Client.SendStatus (s501, '', [xTlsAreadyDone]);  // failed
        exit;
    end ;
  // must reset everything for SSL session
    Client.ClearClient;
    try
        Client.SslEnable := True;
        Client.SslMode := sslModeServer;
        Client.SslContext := FServer.SslContext;
        Client.OnSslVerifyPeer := TlsSslVerifyPeer;
        Client.OnSslHandshakeDone := TlsSslHandshakeDone;
        Client.FContext := mcWaitTls;
        Client.AcceptSslHandshake;
        Answer := AnsiString (s220s + CRLF);
        Client.SslSendPlain (Pointer(Answer), Length(Answer));
        exit;
    except
        on E:Exception do
        begin
            if Assigned(FExtHandler) then
                FExtHandler (Self, E);
        end;
    end;
    Client.SslEnable                := False;
    Client.OnSslVerifyPeer          := nil;
    Client.OnSslHandshakeDone       := nil;
    Client.SendStatus (s454s, '', ['']);  // failed
end;
{$ENDIF}

//******************************************************************//
//  Routine      HandleAUTH                                         //
//                                                                  //
//  Description  Handles AUTH command                               //
//******************************************************************//

procedure TSmtpServer.HandleAUTH(Sender : TObject; const ClientID : cardinal;
                                            var ESMTP : boolean; Parameters : PAnsiChar);
var
    Reason, EncodedAuth, DecodedAuth, Password: string;
    Action: TSmtpmailAction;
    I: integer;
begin
    with TSmtpSrvCli (Sender) do
    begin
{$IFDEF USE_SSL}
        if (smtpsAllowTls in FSmtpServer.FOptions) and
                            (smtpsAuthNoTls in FOptions) then
        begin
            if NOT FTlsDone then
            begin
                SendStatus (s503, '5.1', [xOutOfSequence]);
                exit;
            end;
        end;
{$ENDIF}
        FUserName := '';
        FSmtpSrvAuthType := smtpsAuthNone;
        FAuthenticated := false;
        FAuthChallenge := '';
        FAuthWait2nd := false;
        DecodedAuth := '';
        for I := 1 to 4 do
        begin
            if StrLIComp (SmtpSrvAuthLits [I], Parameters,
                                Length (SmtpSrvAuthLits [I])) = 0 then
            begin
                Parameters := Parameters + Length (SmtpSrvAuthLits [I]);
                FSmtpSrvAuthType := TSmtpSrvAuthType (I);
          // Extract any authentication stuff.- PLAIN and LOGIN, optionally
                EncodedAuth := ExtractArg (Parameters);
                if Length (EncodedAuth) > 1 then
                    DecodedAuth := Base64Decode (EncodedAuth);
                break;
            end;
        end;
        if FSmtpSrvAuthType = smtpsAuthNone then
        begin
            SendStatus (s504, '5.0', []);  // unknown AUTH type
        end
        else if FSmtpSrvAuthType = smtpsAuthPlain then
        begin
            if Length (DecodedAuth) > 1 then
            begin
             // contains 3 arguments, username(null)mailfrom(null)password
                I := Pos (#0, DecodedAuth);
                if I > 0 then
                begin
                    FUserName := Copy (DecodedAuth, 1, Pred (I));
                    inc (I);
                    DecodedAuth := Copy (DecodedAuth, I, 999);  // remove username
                    I := Pos (#0, DecodedAuth);
                    if I > 0 then // ignore mailfrom, badly documented
                    begin
                        inc (I);
                        if Length (DecodedAuth) >= I then Password := Copy (DecodedAuth, I, 999);
                    end;
                end;
                Reason := '';
                Action := wsmtpAuthPermFail;
                if Assigned (FOnAuth) then
                begin
                    FOnAuth (Self, Sender, FUserName, Password, Action, Reason);
                end;
                if Action = wsmtpOK then
                begin
                    FAuthenticated := true;
                    SendStatus (s235, '', [])
                end
                else
                    SendActionFailed (Sender, Action, Reason);
            end
            else
            begin
                FAuthWait2nd := true;
                SendStatus (s334, '', ['OK']);
            end;
        end
        else if FSmtpSrvAuthType = smtpsAuthLogin then
        begin
            FAuthWait2nd := true;
         // might have user name already
            if Length (DecodedAuth) > 1 then
            begin
                FUserName :=  DecodedAuth;
                EncodedAuth := Base64Encode ('Password');
            end
            else
                EncodedAuth := Base64Encode ('Username');
            SendStatus (s334, '', [EncodedAuth]);
        end
        else if (FSmtpSrvAuthType = smtpsAuthCramMD5) or
                             (FSmtpSrvAuthType = smtpsAuthCramSha1) then
        begin
            FAuthWait2nd := true;
            FAuthChallenge := AnsiString (FloatToStr (Now) + '@' +
                                                FSmtpServer.FServerHost);
            EncodedAuth := String (Base64Encode (FAuthChallenge));
            SendStatus (s334, '', [EncodedAuth]);
        end;
    end;
end;


//******************************************************************//
//  Routine      HandleAUTH2                                        //
//                                                                  //
//  Description  Handles extra responses after the AUTH command     //
//******************************************************************//
procedure TSmtpServer.HandleAUTH2(Sender : TObject; const ClientID : cardinal;
                                            var ESMTP : boolean; Parameters : PAnsiChar);
var
    Reason, EncodedAuth, DecodedAuth, Password, HDigest: string;
    Action: TSmtpmailAction;
    I: integer;
    MDigest: TMD5Digest;
    SDigest: SHA1Digest;
    Pwd: AnsiString;
begin
    Reason := '';
    Action := wsmtpAuthPermFail;
    with TSmtpSrvCli (Sender) do
    begin
        FAuthWait2nd := false;  // don't come here again

      // Extract the authentication stuff and check it
        EncodedAuth := ExtractArg (Parameters);
        if Length (EncodedAuth) > 1 then
        begin
            DecodedAuth := Base64Decode (EncodedAuth);
            if FSmtpSrvAuthType = smtpsAuthPlain then
            begin
             // contains 3 arguments, username(null)mailfrom(null)password
                I := Pos (#0, DecodedAuth);
                if I > 0 then
                begin
                    FUserName := Copy (DecodedAuth, 1, Pred (I));
                    inc (I);
                    DecodedAuth := Copy (DecodedAuth, I, 999);  // remove username
                    I := Pos (#0, DecodedAuth);
                    if I > 0 then // ignore mailfrom, badly documented
                    begin
                        inc (I);
                        if Length (DecodedAuth) >= I then Password := Copy (DecodedAuth, I, 999);
                    end;
                end;
            end
            else if FSmtpSrvAuthType = smtpsAuthLogin then
            begin
                if FUserName = '' then // first response
                begin
                    FUserName :=  DecodedAuth;
                    FAuthWait2nd := true;
                    EncodedAuth := Base64Encode ('Password');
                    SendStatus (s334, '', [EncodedAuth]);
                end
                else
                    Password := DecodedAuth;
            end
            else if (FSmtpSrvAuthType = smtpsAuthCramMD5) or
                                        (FSmtpSrvAuthType = smtpsAuthCramSha1) then
            begin
                I := Pos (' ', DecodedAuth);  // contains username(space)hash
                if I > 0 then
                begin
                    FUserName := Copy (DecodedAuth, 1, Pred (I));
                    inc (I);
                    if Length (DecodedAuth) > I then DecodedAuth := Copy (DecodedAuth, I, 999);
                end;
              // application needs to provide a password for us to hash and test
                Password := '';
                if Assigned (FOnAuthPW) then
                begin
                    FOnAuthPW (Self, Sender, FUserName, Password, Action, Reason);
                end;
                if Password <> '' then
                begin
                    Pwd := AnsiString (Password);
          //          MD5DigestInit (MDigest);
                    if FSmtpSrvAuthType = smtpsAuthCramMD5 then
                    begin
                        HMAC_MD5 (PAnsiChar (FAuthChallenge)^, Length (FAuthChallenge),
                                                    PAnsiChar (Pwd)^, Length (Pwd), MDigest);
                        HDigest := MD5DigestToLowerHex (MDigest);
                    end
                    else if FSmtpSrvAuthType = smtpsAuthCramSha1 then
                    begin
                        HMAC_SHA1(PAnsiChar (FAuthChallenge)^, Length(FAuthChallenge),
                                                    PAnsiChar (Pwd)^, Length (Pwd), SDigest);
                        HDigest := SHA1DigestToLowerHex (SDigest);
                    end;
                    if DecodedAuth = HDigest then
                            Action := wsmtpOK;
                  // set blank password for event since we don't want to test it
                    Password := '';
                 end;
            end ;
        end;
        if (NOT FAuthWait2nd) then
        begin
            if (Assigned (FOnAuth)) then
            begin
                FOnAuth (Self, Sender, FUserName, Password, Action, Reason);
            end;
            if Action = wsmtpOK then
            begin
                FAuthenticated := true;
                SendStatus (s235, '', [])
            end
            else
            begin
             // failed, so reset everything
                FUserName := '';
                FSmtpSrvAuthType := smtpsAuthNone;
                FAuthenticated := false;
                FAuthChallenge := '';
                SendActionFailed (Sender, Action, Reason);
            end;
        end;
    end;
end;


//******************************************************************//
//  Routine      HandleMAIL                                         //
//                                                                  //
//  Description  Handles MAIL FROM command                          //
//******************************************************************//

procedure TSmtpServer.HandleMAIL(Sender : TObject; const ClientID : cardinal;
                                            var ESMTP : boolean; Parameters : PAnsiChar);
var
    Reason: string;
    Action: TSmtpmailAction;
    MsgSize : integer;
begin
    Action := wsmtpOK;
    Reason := '';

  // Line should consist of FROM: <[address]>. Validate.
    with TSmtpSrvCli (Sender) do
    begin
        if StrLIComp (cFrom, Parameters, Length (cFrom)) <> 0 then
        begin
            Action := wsmtpBadParams;
            Reason :=xBadFROMparam;
        end
        else if (FServPort = '587') and (NOT FAuthenticated) then
        begin
            Action := wsmtpAuthRequired;
        end;

        // Extract the From address..
        if Action = wsmtpOK then
        begin
            Parameters := Parameters + Length (cFrom);
            FMessageFrom := IcsLowerCase (ExtractEmail (Parameters));

            // Clear details of any prior messages
            FMessageTo.Clear;
            FToAccounts.Clear;
            FHdrTo           := '';
            FHdrFrom         := '';
            FHdrSubject      := '';
            FHdrDateStr      := '';
            FHdrDateDT       := 0;
            FDoneHdrs        := false;

            // Now check for an RFC-1870 SIZE specifier
            if (FSmtpServer.FMaxMsgSize > 0) then
            begin
                SkipWhiteSpace (Parameters);
                if StrLIComp (cSize, Parameters, Length (cSize)) = 0 then
                begin
                // Check SIZE against the limit
                    SkipWhitespace (Parameters);
                    try
                        MsgSize := atoi (Parameters);
                        if MsgSize > FSmtpServer.FMaxMsgSize then
                        begin
                        // Too large.. reject and return to caller
                            SendStatus (s552, '3.4', []);
                            Exit;
                        end;
                    except
                      // Syntax error.. give 'em a hint and return to caller
                        SendStatus (s501, '5.2', [xBadSIZEparam]);
                        Exit;
                    end;
                end;
            end;

          // ask application if we are happy
            if Assigned (FOnMailFrom) then
            begin
                FOnMailFrom (Self, Sender, FMessageFrom, Action, Reason);
            end;
        end;

     // Set message context & tell 'em "full steam ahead"..
        if Action = wsmtpOK then
        begin
            FContext   := mcMessage;
            FMessageID := Format ('%1.1u%9.9x%8.8x', [YearOf (Now) mod 10,
                                                    MillisecondOfTheYear (Now), FID]);
            SendStatus(s250, '1.0', ['<' + FMessageFrom + '> ' + cOK]);
        end
        else
            SendActionFailed (Sender, Action, Reason);
    end;
end;


//******************************************************************//
//  Routine      HandleRCPT                                         //
//                                                                  //
//  Description  Handles RCPT TO command                               //
//******************************************************************//

procedure TSmtpServer.HandleRCPT(Sender : TObject; const ClientID : cardinal;
                                            var ESMTP : boolean; Parameters : PAnsiChar);
var
    Recipient, Domain, LocalAccount, Reason: string;
    Action    : TSmtpmailAction;
    Dupli, AllowRelay: boolean;
    J, K      : integer;
begin
  // Line should consist of TO: <[address]>. Validate.
    with TSmtpSrvCli (Sender) do
    begin
        Dupli := false;
        Reason := '';
        Action := wsmtpOK;
        if StrLIComp (cTo, Parameters, Length(cTo)) <> 0 then
        begin
            Action := wsmtpBadParams;
            Reason :=xBadTOparam;
        end
        else if (FServPort = '587') and (NOT FAuthenticated) then
        begin
            Action := wsmtpAuthRequired;
        end
        else if Length (FMessageFrom) = 0 then
        begin
          // no mail from yet
            Action := wsmtpBadSequence;
            Reason :=  xNoMailFrom;
        end;

      // Extract the To address..
        if Action = wsmtpOK then
        begin
            Parameters := Parameters + Length(cTo);
            Recipient := IcsLowerCase (ExtractEmail (Parameters));
            LocalAccount := '';

          // New recipient specified?
            Dupli := (FMessageTo.IndexOf (Recipient) >= 0) ;

          // test against local accounts
            if FLocalAccounts.Count > 0 then
            begin
                J := Pos ('@', Recipient);
                if (J > 1) then Domain := Copy (Recipient, J + 1, 999);
                if (FLocalAccounts as TStringList).Find (Recipient, K) then
                    LocalAccount := Recipient  // main account
                else
                begin
                  // alias for a main account, which is where it will be delivered
                    K := FAliasAccounts.IndexOfName (Recipient);
                    if K >= 0 then
                        LocalAccount := FAliasAccounts.ValueFromIndex [K]
                    else
                    begin
                    // catch-all mail for this domain if alias is *@domain
                        K := FAliasAccounts.IndexOfName ('*@' + Domain);
                        if K >= 0 then
                            LocalAccount := FAliasAccounts.ValueFromIndex [K];
                    end;
                end;
             // not local, see if allowed to relay mail
                if (LocalAccount = '') then
                begin
                    AllowRelay := false;
                    if (FAuthenticated and (smtpsAllowAuthRelay in FOptions)) then AllowRelay := true;
                    if (smtpsAllowOpenRelay in FOptions) then AllowRelay := true;
                    if NOT AllowRelay then
                    begin
                        if (J > 1) and (NOT (FLocalDomains as TStringList).Find (Domain, K)) then
                            Action := wsmtpBadDomain
                        else
                            Action := wsmtpBadAccount;
                    end;
                end;
            end;

            // New recipient
            if Assigned (FOnRcptTo) then
            begin
                FOnRcptTo (Self, Sender, Recipient, Action, Reason);
            end;
        end;

     // Handle action
        if Action = wsmtpOK then
        begin
       // Accept duplicate recipient (but don't actually add it to the list again)
            if Dupli then
                SendStatus (s250, '1.0', ['<' + Recipient + '> ' + cOK])
            else
            begin
                FMessageTo.Add (Recipient);
                FToAccounts.Add (LocalAccount);  // might be blank if not local
                SendStatus (s250, '1.5', ['<' + Recipient + '> ' + cOK]);
            end;
        end
        else
            SendActionFailed (Sender, Action, Reason);
    end;
 end;


//******************************************************************//
//  Routine      HandleDATA                                         //
//                                                                  //
//  Description  Handles DATA command                               //
//******************************************************************//

procedure TSmtpServer.HandleDATA(Sender : TObject; const ClientID : cardinal;
                                            var ESMTP : boolean; Parameters : PAnsiChar);
const
    SType   : array [false..true] of string = ('SMTP','ESMTP');
var
    CliHost : string;
    Headers : AnsiString;
    I       : integer;
    Reason  : string;
    Action  : TSmtpmailAction;
begin
    Reason := '';
    Action := wsmtpOK;
    with TSmtpSrvCli (Sender) do
    begin
        if FMessageTo.Count = 0 then
        begin
          // No recipients specified
            Action := wsmtpBadSequence;
            Reason :=  xNoRecipients;
        end
        else
        begin
         // free datastream in case left open due to band commands
            try
                if Assigned (DataStream) then
                begin
                    DataStream.Free;
                    DataStream := nil;
                end;

            // application must create Datastream in event for mail fails now
                if Assigned (FOnDataStart) then
                begin
                    FOnDataStart (Self, Sender, Action, Reason);
                end;
            except
                Action := wsmtpSysUnavail;
                Reason :=  xNoSpool;
            end;
        end;

     // check a stream has been assigned in the application to save data
        if (Action = wsmtpOK) and (NOT Assigned (FDataStream)) then
        begin
            Action := wsmtpSysUnavail;
            Reason :=  xNoStorage;
        end;

   // see if adding headers before other headers, might fail writing stream
        if Action = wsmtpOK then
        begin
            try
                FDataStream.Seek (0,0);  // reset to start
                Headers := '';
                if (smtpsAddReplayHdrs in FOptions) then
                begin
                    Headers := AnsiString ('X-Sender: <' + FMessageFrom + '>' + CRLF);
                  // optional arguments, BODY=7bit RET=HDRS/FULL, ENVID=xxx, auth=(RFC2554 shceme)
                    for I := 0 to FMessageTo.Count - 1 do
                        Headers := Headers + AnsiString ('X-Receiver: <' + FMessageTo [I] + '>' + CRLF);
                      // optional arguments, NOTIFY=NEVER/DELAY/FAILURE ORcpt=originalrecipient
                end;
                if (smtpsAddRecvHeaders in FOptions) then
                begin
                    // add Return-Path: and Received: headers
                    if FClientRDNS = '' then
                        CliHost := ''
                    else
                        CliHost := FClientRDNS + ' ';
                    Headers := Headers + AnsiString (Format ('Return-Path: <%s>'#13#10 +
                            'Received: from %s (%s[%s]) by %s'#13#10 +
                            '          with %s id <%s>;'#13#10 +
                            '          %s '#13#10,
                            [FMessageFrom, FClientDomain, CliHost, FClientIpAddr,
                            FServerHost, SType [ESMTP], FMessageID, Rfc822DateTime(Now)]));
                end;
                if Headers <> '' then
                    FDataStream.WriteBuffer (PAnsiChar(Headers)^, Length(Headers));
            except
                Action := wsmtpSysUnavail;
                Reason :=  xNoSpool;
            end;
        end;

     // Switch to DATA mode
        if Action = wsmtpOK then
        begin
            FContext   := mcData;
            SendStatus (s354, '0.0', []);
        end
        else
            SendActionFailed (Sender, Action, Reason);
    end;
end;


//******************************************************************//
//  Routine      SendActionFailed                                   //
//                                                                  //
//  Description  Send various failed responses                      //
//******************************************************************//

procedure TSmtpServer.SendActionFailed(Sender : TObject;
                                    Action: TSmtpmailAction; const Reason: string);
begin
    with TSmtpSrvCli (Sender) do
    begin
        case Action of
            wsmtpOK            : SendStatus (s250, '1.0', [Reason]);  // should not come here for OK
            wsmtpClosingDown   : SendStatus (s421,'', [FSmtpServer.FServerHost, Reason, sClosingChannel]);
            wsmtpMailboxBusy   : SendStatus (s450, '7.0', []);
            wsmtpGreylisted    : SendStatus (s451, '4.1', [IntToStr (FGreyDelaySecs)]);
            wsmtpMsgTooLarge   : SendStatus (s452, '3.1', [xNoStorage]);
            wsmtpAuthTempFail  : SendStatus (s454, '7.0', []);  // AUTH failed, unknown
            wsmtpSyntaxError   : SendStatus (s500, '5.2', [Reason]);
            wsmtpBadParams     : SendStatus (s501, '5.2', [Reason]);
            wsmtpBadCommand    : SendStatus (s502, '5.2', [Reason]);
            wsmtpBadSequence   : SendStatus (s503, '5.0', [Reason]);
            wsmtpAuthRequired  : SendStatus (s530, '5.1', []);
            wsmtpAuthPermFail  : SendStatus (s535, '7.8', []);  // AUTH failed, bad credentials
            wsmtpBadAccount    : SendStatus (s550, '1.1', [xBadAccount]);
            wsmtpBadDomain     : SendStatus (s550, '1.2', [xBadDomain]);
            wsmtpAccClosed     : SendStatus (s550, '1.6', [xAccClosed]);
            wsmtpAccNotLocal   : SendStatus (s551, '7.1', [Reason]);
            wsmtpMailboxFull   : SendStatus (s552, '2.2', []);
            wsmtpProhibited    :
            begin
                if (Reason = '') then
                    SendStatus (s553, '7.1', [xPolicy])
                else
                    SendStatus (s553, '7.1', [Reason]);
            end;
            wsmtpSysUnavail    : SendStatus (s554, '3.5', [xNoSpool]);
            wsmtpNetError      : SendStatus (s554, '4.0', [xNetError]);     // Network error
            wsmtpCongested     : SendStatus (s554, '4.5', [xCongested]);    // System is congested. Please try again later.
            wsmtpTooMany       : SendStatus (s554, '5.3', [xTooMany]);      // Too many recipients specified
            wsmtpBadMedia      : SendStatus (s554, '3.1', [xBadMedia]);     // Media not supported (e.g. we don't like Base-64 ;o)
            wsmtpListNotAuth   : SendStatus (s554, '3.1', [xListNotAuth]);  // You are not authorised to send messages to this mailing list
            wsmtpListNotRec    : SendStatus (s554, '2.4', [xListNotRec]);   // Mailing list does not exist
            else
                SendStatus (s501, '5.0', [Reason]);  // unknown or undefined
         end;
    end;
end;


//******************************************************************//
//  Component    SmtpSrvCli                                         //
//******************************************************************//

//******************************************************************//
//  Routine      Constructor/Destructor                             //
//******************************************************************//

constructor TSmtpSrvCli.Create(AOwner : TComponent);
begin
    inherited;
    FContext         := mcConnecting;
    FMessageTo       := TStringList.Create;
    FToAccounts      := TStringList.Create;
    ClearClient;

    OnBgException   := TWSocketServer( AOwner).OnBgException;
    OnDataAvailable := nil;
end;


destructor TSmtpSrvCli.Destroy;
begin
    if Assigned(DataStream) then
    begin
        DataStream.Free;
        DataStream := nil;
    end;
    FMessageTo.Free;
    FToAccounts.Free;
    inherited;
end;


//******************************************************************//
//  Routine      ClearClient                                        //
//                                                                  //
//  Description  Clear all client variables                         //
//******************************************************************//

procedure TSmtpSrvCli.ClearClient;
begin
    FESMTP           := false;
    FDnsQuery        := nil;
    FMessageFrom     := '';
    FClientDomain    := '';
    FClientRDNS      := '';
    FClientMX        := '';
    FMessageTo.Clear;
    FToAccounts.Clear;
    FUserName        := '';
    FAuthenticated   := false;
    FAuthWait2nd     := false;
    FHdrTo           := '';
    FHdrFrom         := '';
    FHdrSubject      := '';
    FHdrDateStr      := '';
    FHdrDateDT       := 0;
    FDoneHdrs        := false;
    FTlsDone         := false;
end;

//******************************************************************//
//  Routine      SendAnswer                                         //
//                                                                  //
//  Description  SendAnswer                                         //
//******************************************************************//

function TSmtpSrvCli.SendAnswer(const Str : String) : integer;
begin
    if Length (Str) > 0 then
    begin
        if Assigned (FSmtpServer.FOnResponse) then
            FSmtpServer.FOnResponse (FSmtpServer, Self, Copy (Str, 1, Length (Str) -2));
        Result := SendStr (RawByteString(Str));
    end
    else
        Result := 0;

    FLastContact := Now;
end;


//******************************************************************//
//  Routine      SendStatus                                         //
//                                                                  //
//  Description  Sets and transmits an SMTP status string           //
//******************************************************************//

procedure TSmtpSrvCli.SendStatus(const FormatStr    : string;
                                  const EnhancedStat : string;
                                        Args         : array of const);
var
    OutputBuffer : string;
begin
    try
        OutputBuffer := Format (FormatStr, Args) + CRLF;
        if (smtpsExtendedResp in FSmtpServer.FOptions) and FESMTP and
                        (EnhancedStat <> '') and (FormatStr <> '') then
            OutputBuffer := Copy (OutputBuffer, 1, 4) + FormatStr [1] + '.' +
                    EnhancedStat + Copy (OutputBuffer, 4, Length (OutputBuffer));
        SendAnswer(OutputBuffer);
    except
        on E : Exception do
        begin
            RaiseException (Format (xClientStat,[FID, Copy (FormatStr, 1, 3), E.Message]));
            Abort;
        end;
    end;
end;

//******************************************************************//
//  Routine      SslSendPlain                                       //
//                                                                  //
//  Description  Sends a response before SSL negotiated             //
//******************************************************************//
{$IFDEF USE_SSL}
function TSmtpSrvCli.SslSendPlain(Data : TWSocketData; Len : Integer) : Integer;
begin
    Result := RealSend (Data, Len);
end;
{$ENDIF}


//******************************************************************//
//  Routine      ClientDataRx                                       //
//                                                                  //
//  Description  Invoked when data is received from the client      //
//******************************************************************//

procedure TSmtpSrvCli.ClientDataRx(Sender : TObject;
                                    Error  : Word);
var
    Len, I: integer;
    Reason, RcvdLower: string;
    Headers : AnsiString;
    Action: TSmtpmailAction;

    procedure TestHdr (const Hdr: string; var Value: string);
    begin
        if Pos (Hdr, RcvdLower) <> 1 then exit;
        if (Len > 0) and (FRcvdLine[Len] = #10) then  // remove CRLF
        begin
            Dec(Len);
            if (Len > 0) and (FRcvdLine[Len] = #13) then
                Dec(Len);
         end;
       // decode inline MIME in header, ignore charset
         Value := DecodeMimeInlineValue (Copy (FRcvdLine, Length (Hdr) + 1, Len - Length (Hdr))) ;
    end;

begin
    if (Error = ERROR_SUCCESS) then
    begin
    { We use line mode. We will receive complete lines }
        FRcvdLine := ReceiveStrA;
        Len := Length (FRcvdLine);
        if (Len > 0) then
        begin
            if FContext = mcData then
            begin
            // check for end of data
                if Pos (EOM, FRcvdLine) <> 1 then
                begin
                    if (NOT Assigned (FDataStream)) then
                    begin
                        Action := wsmtpSysUnavail;
                        FContext := mcCommand; // ready for more commands

                     // tell application we died
                        if Assigned (FSmtpServer.FOnDataEnd) then
                        begin
                            FSmtpServer.FOnDataEnd (FSmtpServer, Sender, Action, Reason);
                        end;
                        SendStatus (s554, '3.5', [xNoSpool]);
                        exit;
                    end
                    else
                    begin
                    // remove escaped period at start of line
                        if Pos (ESCDOT, FRcvdLine) = 1 then
                        begin
                            Dec (Len);
                            FRcvdLine := Copy (FRcvdLine, 2, Len) ;
                        end;

                     // see if adding extra headers to bottom of existing headers
                        if (NOT FDoneHdrs) then
                        begin
                            if Pos (CRLF, FRcvdLine) = 1 then  // blank line ends headers
                            begin
                                FDoneHdrs := true;
                                if FHdrDateStr <> '' then
                                try
                                    FHdrDateDT := InetParseDate (FHdrDateStr);
                                except
                                    FHdrDateDT := 0; // some date strings are very illegal
                                end;

                             // see if adding new headers after other headers
                                Headers := '';
                                if (smtpsAddEnvHeaders in FSmtpServer.FOptions) then
                                begin
                                    Headers := AnsiString ('X-Envelope-From: ' + FMessageFrom + CRLF);
                                    for I := 0 to FMessageTo.Count - 1 do
                                        Headers := Headers + AnsiString ('X-Envelope-To: ' + FMessageTo [I] + CRLF);
                                end
                                else
                                begin
                                    if (HdrTo = '') and (smtpsParseHeaders in FSmtpServer.FOptions) then
                                    begin
                                        for I := 0 to FMessageTo.Count - 1 do
                                            Headers := Headers + AnsiString ('Apparently-To: ' + FMessageTo [I] + CRLF);
                                    end;
                                end;
                                if (smtpsAddIpAddrHdr in FSmtpServer.FOptions) then
                                    Headers := Headers + AnsiString ('X-Originating-IP: ' + FClientIpAddr + CRLF);
                                if Headers <> '' then
                                    FDataStream.WriteBuffer (PAnsiChar(Headers)^, Length(Headers));

                              // here is where we could add more extra headers, if needed
                            end;
                        end;

                      // write email content line to stream
                        try
                            FDataStream.WriteBuffer (PAnsiChar(FRcvdLine)^, Len);
                        except
                            Action := wsmtpSysUnavail;
                            FContext := mcCommand; // ready for more commands

                         // tell application we died
                            if Assigned (FSmtpServer.FOnDataEnd) then
                            begin
                                FSmtpServer.FOnDataEnd (FSmtpServer, Sender, Action, Reason);
                            end;
                            SendStatus (s554, '3.5', [xNoSpool]);
                            exit;
                        end;

                      // optionally parse a few email headers
                      // WARNING - only first line of multiple line headers is processed currently
                        if (smtpsParseHeaders in FSmtpServer.FOptions) and (NOT FDoneHdrs) then
                        begin
                            RcvdLower := IcsLowerCase (String(FRcvdLine));
                            TestHdr ('to:', FHdrTo);
                            TestHdr ('from:', FHdrFrom);
                            TestHdr ('subject:', FHdrSubject);
                            TestHdr ('date:', FHdrDateStr);
                        end;
                    end;
                end
                else
             // got a complete message
                begin
                    FContext := mcCommand; // ready for more commands
                    Reason := '';
                    Action := wsmtpOK;
                    if (FSmtpServer.FMaxMsgSize > 0) and (FDataStream.Size > FSmtpServer.FMaxMsgSize) then
                    begin
                        Action := wsmtpMsgTooLarge ;
                    end ;

                 // tell application, it can do something with the full DataStream, like close it
                    try
                        if Assigned (FSmtpServer.FOnDataEnd) then
                        begin
                            FSmtpServer.FOnDataEnd (FSmtpServer, Sender, Action, Reason);
                        end
                        else
                            Action := wsmtpSysUnavail;
                    except
                        Action := wsmtpSysUnavail;
                    end;
                    if Action = wsmtpOK then
                    begin
                        SendStatus (s250, '6.0', [Format(sQueued, [FMessageID])]);
                    end
                    else
                        FSmtpServer.SendActionFailed (Sender, Action, Reason);

                 // free datastream if not already done
                    if Assigned(DataStream) then
                    begin
                        DataStream.Free;
                        DataStream := nil;
                    end;
                end;
            end
            else
            begin
        { Remove trailing CR/LF }
                if (Len > 0) and (FRcvdLine[Len] = #10) then
                begin
                    Dec(Len);
                    if (Len > 0) and (FRcvdLine[Len] = #13) then
                        Dec(Len);
                    SetLength (FRcvdLine, Len);
                 end;
                if (Len > 0) then
                begin
                    if Assigned (FSmtpServer.FOnCommand) then
                        FSmtpServer.FOnCommand (FSmtpServer, Self, String(FRcvdLine));
                    if FAuthWait2nd then
                    begin
                        FAuthWait2nd := false;  // may get set again in AUTH2
                        FSmtpServer.HandleAUTH2 (Self, FID, FESMTP, @FRcvdLine[1]);
                    end
                    else
                        ProcessCommand (@FRcvdLine[1]);
                end;
            end;
        end;
    end
    else
    // Data reception error. Close link
        Close;
end;


//******************************************************************//
// Routine      WndProc                                             //
//                                                                  //
// Description  Handles custom message processing                   //
//******************************************************************//

procedure TSmtpSrvCli.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_wmClientLookupDone := FWndHandler.AllocateMsgHandler (Self);
    FMsg_wmClientClose := FWndHandler.AllocateMsgHandler (Self);
end;


procedure TSmtpSrvCli.FreeMsgHandlers;
begin
    if Assigned (FWndHandler) then
    begin
        FWndHandler.UnregisterMessage (FMsg_wmClientLookupDone);
        FWndHandler.UnregisterMessage (FMsg_wmClientClose);
    end;
    inherited FreeMsgHandlers;
end;

procedure TSmtpSrvCli.WndProc(var MsgRec: TMessage);
var
    Reason: string;
    Action: TSmtpmailAction;
begin
    try
        if MsgRec.Msg = FMsg_wmClientLookupDone then
        begin
            with TSmtpSrvCli (MsgRec.LParam) do
            begin
                if FContext = mcConnecting then
                begin
              // Setup connection
                    FDnsQuery.Free;
                    FDnsQuery := nil;
                    OnDataAvailable := ClientDataRx;
                // Inform "caller"
                    Action := wsmtpOK;
                    Reason := '';
                    if Assigned (FSmtpServer.FOnConnect) then
                    begin
                        FSmtpServer.FOnConnect (FSmtpServer, Self, FClientIpAddr, Action, Reason);
                    end;
                    if Action = wsmtpOK then
                    begin
                        FContext := mcConnected;
                        SendStatus (s220, '', [FSmtpServer.FServerHost, FSmtpServer.FServerDesc]);
                    end
                    else
                    begin
                        FSmtpServer.SendActionFailed (Self, Action, Reason);
                      // Reject connection
                        TSmtpSrvCli (MsgRec.LParam).Close;
                    end;
                end;
            end ;
        end
        else if MsgRec.Msg = FMsg_wmClientClose then
        begin
            TSmtpSrvCli (MsgRec.LParam).Close;
        end
        else
           inherited WndProc (MsgRec);
    except
        on E:Exception do
            HandleBackGroundException(E);
    end;
end;


//******************************************************************//
// Routine      LookupComplete                                      //
//                                                                  //
// Description  RDNS lookup complete for client connection          //
//******************************************************************//

procedure TSmtpSrvCli.LookupComplete(Sender : TObject; Error : Word);
var
    i,j : integer;
begin
    with FDnsQuery do
    begin
        if Error = ERROR_SUCCESS then
        begin
            if QuestionType = DnsQueryPTR then
            begin
                if ResponseANCount = 0 then
              // No rDNS available.
                    PostMessage(Handle, FMsg_wmClientLookupDone, 0, lParam(Self))  { V8.03}
                else
                begin
                    FClientRDNS := String(Hostname[0]);
                    MXLookup (Hostname[0]);
                end ;
            end
            else
        // MX record located
                if ResponseANCount = 0 then
                begin
          // Failed. Remove front subdomain and try again
                    i := Pos ('.', String(QuestionName));
                    if i = 0 then
               // MX does not exist
                        PostMessage (Handle, FMsg_wmClientLookupDone, 0, lParam(Self))  { V8.03}
                    else
              // Remove front portion and try again..
                        MXlookup (Copy (QuestionName, SUCC (i), Length (QuestionName)));
                end
                else
                begin
          // Locate current primary server
                    i := 0;
                    for j := 1 to PRED(ResponseANCount) do
                        if MXpreference [j] < MXpreference [i] then
                            i := j;
                    FClientMX := String(MXexchange [i]);
                end;
         end
         else
         begin
           // DNS lookup has failed.
             OnRequestDone := nil;
             PostMessage (Handle, FMsg_wmClientLookupDone, 0, lParam(Self))  { V8.03}
         end;
    end;
end;


//******************************************************************//
//  Routine      ProcessCommand                                     //
//                                                                  //
//  Description  Processes an SMTP command                          //
//******************************************************************//

procedure TSmtpSrvCli.ProcessCommand(Str : PAnsiChar);
var
    Cmd, Ptr : PAnsiChar;
    i        : integer;
begin
    if Str^ <> NUL then
    begin

    // Trim any trailing whitespace
        Ptr := StrEnd (Str) - 1;
        while (Ptr >= Str) and (Ptr^ <= ' ') do
            Dec (Ptr);
        PAnsiChar (Ptr +1 )^ := NUL;

    // Advance pointers to command and any parameters
        Cmd := Str;
        while (Cmd^ <> NUL) and (Cmd^ <= ' ') do
            Inc (Cmd);
        Ptr := Cmd;
        while (Ptr^ <> NUL) and (Ptr^ > ' ') do
            Inc (Ptr);
        if Ptr <> NUL then
        begin
      // Parameter present - skip whitespace
            Ptr^ := NUL;
            Inc (Ptr);
            while (Ptr^ <> NUL) and (Ptr^ <= ' ') do
                Inc(Ptr);
        end;

    // Search command list
        with FSmtpServer do
        begin
            i := Low (FCommands);
            while (i <= High (FCommands)) and (StrIComp (Cmd, @FCommands [i].Cmd [1]) <> 0) do
                Inc(i);
            if i < Length (FCommands) then
            begin
        // Command recognized. Is it in-context or allowed anywhere?
                if (FCommands [i].Context = FContext) or (FCommands [i].Context = mcAny) then
                begin
          // Call handler, if assigned
                    if Assigned (FCommands [i].Handler) then
                        FCommands [i].Handler (Self, FID, FESMTP, Ptr)
                    else
                // No handler.. hence not implemented
                        SendStatus (s502, '5.1', [Cmd]);
                end
                else
                begin
          // Command is out-of-sequence
                    if FContext = mcConnected then
                        SendStatus (s503, '5.1', [xNoHello])
                    else
                        SendStatus (s503, '5.1', [xOutOfSequence]);
                end;
            end
            else
                SendStatus (s500, '5.2', [Cmd]);
        end;
    end;
end;

//******************************************************************//
//  Routine      SSL Create                                         //
//                                                                  //
//  Description  Create SSL component                               //
//******************************************************************//

{$IFDEF USE_SSL}
constructor TSslSmtpServer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FServer.SslEnable := true;
    AddCommand (cSTARTTLS, HandleTLS, mcCommand);
end;


//******************************************************************//
//  Routine      SSL Get/SetContext                                 //
//                                                                  //
//  Description  Get and Set SSL Content                            //
//******************************************************************//
function TSslSmtpServer.GetSslContext: TSslContext;
begin
    Result := FServer.SslContext;
end;

procedure TSslSmtpServer.SetSslContext(Value: TSslContext);
begin
    FServer.SslContext :=  Value;
end;


//******************************************************************//
//  Routine      TlsSslHandshakeDone                                //
//                                                                  //
//  Description  SSL handshaking has completed OK or failed         //
//******************************************************************//
procedure TSslSmtpServer.TlsSslHandshakeDone(Sender: TObject;
  ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
var
    Client: TSmtpSrvCli;
begin
    Client := Sender as TSmtpSrvCli;
    if Assigned(FOnSslHandshakeDone) then
        FOnSslHandshakeDone(Sender, ErrCode, PeerCert, Disconnect);
    if (ErrCode <> 0) or Disconnect then
    begin
        PostMessage (FHandle, Client.FMsg_wmClientClose, 0, lParam(Sender));   { V8.03}
        Disconnect := FALSE;
    end
    else
    begin
        Client.ClearClient;
        Client.FContext := mcConnected;  // expect EHLO next
        Client.FTlsDone := true;
    end;
end;


//******************************************************************//
//  Routine      TlsSslVerifyPeer                                   //
//                                                                  //
//  Description  Verify Peer                                        //
//******************************************************************//
procedure TSslSmtpServer.TlsSslVerifyPeer(Sender: TObject;
  var Ok: Integer; Cert: TX509Base);
begin
    if Assigned(FOnSslVerifyPeer) then
        FOnSslVerifyPeer(Sender, Ok, Cert);
end;

{$ENDIF}

//******************************************************************//
//  Routine      Initialization/Finalization                        //
//******************************************************************//

initialization

    Randomize;

end.

