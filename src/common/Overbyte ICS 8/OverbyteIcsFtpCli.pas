{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     May 1996
Version:      V8.06
Object:       TFtpClient is a FTP client (RFC 959 implementation)
              Support FTPS (SSL) if ICS-SSL is used (RFC 2228 implementation)
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2014 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

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

Quick Reference:

Properties:
  HostName      - FTP server host name or IP address
  UserName      - User name for authentication on FTP server
  PassWord      - Passwor needed for user, can be blank
  HostDirName   - Directory as knwon of FTP server
  HostFileName  - File name as known on FTP server
  LocalFileName - Local file name (complete path)
  Binary        - Select binary or ascii file transfert (Need to call TypeSet
                  or TypeSetAsync to send it to FTP server).
  (There are other less used properties, see code below)

Methods:
  Open       - Open the connection with the FTP server
  User       - Send username
  Pass       - Send password
  Acct       - Send account
  Connect    - Open the connection, send username, password, account
  Quit       - Disconnect gracefully from FTP server
  Abort      - Disconnect (close connection) immediately
  AbortXfer  - Abort file transfer without disconnecting.
               Warning: LocalFilename property is lost after this command.
  Pwd        - Get current working directory into DirResult
  Cwd        - Change Working Directory to HostDirName
  CDup       - Change to parent directory
  TypeSet    - Set type for file transfert (see Binary property)
  TypeBinary - Set to binary type transfert and call TypeSet
  TypeAscii  - Set to ascii type transfert and call TypeSet

  Put        - Upload a file
  Transmit   - Connect, Cwd, Upload a file & Quit

  Append     - Upload a file, appending to existing
  AppendFile - Connect, Cwd, Append a file & Quit

  Get        - Download a file from HostFileName to LocalFileName or LocalStream
  Receive    - Connect, Cwd, Download a file & Quit

  RestGet    - Download a file, restarting from current local file size
  RestartGet - Connect, Cwd, Restart downloading a file & Quit

  RestPut    - Upload a file, restarting from ResumeAt property value
  RestartPut - Connect, Cwd, Restart uploading a file & Quit

  Dir        - Download a directory listing of HostFileName to LocalFileName or LocalStream
  Directory  - Connect, Cwd, Download a directory listing to a file & Quit

  Ls         - Download a file name listing of HostFileName to LocalFileName or LocalStream
  List       - Connect, Cwd, Download a file name listing to a file & Quit

  Mkd        - Create a directory on the server in HostFileName
  Mkdir      - Connect, Cwd, Create a directory on the server & Quit

  Ren        - Rename a file or directory on the server from HostFileName to LocalFileName
  Rename     - Connect, Cwd, Rename a file or directory on the server & Quit

  Dele       - Delete a file on the server in HostFileName
  Delete     - Connect, Cwd, Delete a file on the server & Quit

  Rmd        - Remove a directoy from the server in HostFileName
  Rmdir      - Connect, Cwd, Remove a directoy from the server & Quit

  Syst       - Get system information from the server
  System     - Connect, Cwd, Get system information from the server & Quit

  Size       - Get file size of HostFileName to SizeResult
  FileSize   - Connect, Cwd, get file size & Quit

  Quote      - Send literal command (use LocalFileName as command to send)
  DoQuote    - Connect, Cwd, send literal command & Quit

  Feat       - Get extensions supported
               = SupportedExtensions which features server supports

  Mlsd       - Download a directory listing of HostFileName to LocalFileName or LocalStream
               only supported if ftpFeatMLST in SupportedExtensions - V2.90

  Mlst       - Get facts for one file or directory of HostFileName
               only supported if ftpFeatMLST in SupportedExtensions - V2.90
               Result is in RemFacts property, and may be decoded by
               DecodeMlsResp (in FtpSrvT)

  Mdtm       - Get file UTC modification time of HostFileName to RemFileDT,
               only supported if ftpFeatMDTM in SupportedExtensions - V2.90

  Mdtmyy     - Set file UTC modification time of HostFileName to RemFileDT,
               only supported if ftpFeatMDTMYY in SupportedExtensions - V2.90

  Auth       - Explizit TLS/SSL authentication (FTPS protocol support) - V2.106
               Params TLS (RFC4217) or SSL (no standard tracks).
               The parameter actually used needs to be specified by property
               SslType.
  CCC        - Revert a TLS/SSL-protected control channel back to plaintext. - V2.106

  Mfmt       - Modify file UTC modification time of HostFileName to RemFileDT,
               only supported if ftpFeatMFftpFeatMfmt in
               SupportedExtensionsMT in SupportedExtensionse - V2.94

  Md5        - Get MD5 hash sum for HostFileName to Md5Result,
               only supported if ftpFeatMD5 in SupportedExtensions - V2.94

  Xmd5       - Get MD5 hash sum for HostFileName from PosStart to PosEnd to Md5Result,
               only supported if ftpFeatXMD5 in SupportedExtensions - V2.213

  Xcrc       - Get CRC32 hash sum of HostFileName from PosStart to PosEnd to CRCResult,
               only supported if ftpFeatXCrc in SupportedExtensions - V2.107

  ModeZ      - Sets Z compression or no compression, for TransferMode as
               ftpTransModeStream or ftpTransModeZDeflate
               only supported if ftpFeatModeZ in SupportedExtensions - V2.103

  Clnt       - Set Client Id string to ClientId,
               only supported if ftpFeatClnt in SupportedExtensions - V2.213

  Allo       - Check if the server can allocate disk space for upload size in PosEnd
               in HostFileName - V2.213

  Comb       - Ask server to combine two or more files listed in HostFileName
               only supported if ftpFeatClnt in SupportedExtensions - V2.213

  SitePswd   - Change the account password on the server from Password to HostFileName,
               only supported if ftpFeatSitePaswd in SupportedExtensions - V2.213

  SiteExec   - Execute a progam at the server passed in HostFileName,
               only supported if ftpFeatSiteExec in SupportedExtensions - V2.213

  SiteIndex  - Download a recusive directory and file name listing of HostFileName
                 to LocalFileName or LocalStream
               only supported if ftpFeatSiteIndex in SupportedExtensions - V2.213

  SiteZone   - Get the server time zone difference from UTC to LastResponse
               May be used to correct file time stamps returned by LIST
               only supported if ftpFeatSiteZone in SupportedExtensions - V2.213

  SiteMsg    - Send a message to the server from HostFileName,
               only supported if ftpFeatSiteMsg in SupportedExtensions - V2.213

  SiteCmlsd and XCmlsd - Download a directory listing of HostFileName to LocalFileName or LocalStream
               The path argument may be followed by optional argument -R or -subdirs
               for recursive directories, the path may be quoted if it includes spaces
               Same arguments as SiteDmlsd but returns listing on control channel,
               SiteCmlsd only supported if ftpFeatSiteDmlsd in SupportedExtensions - V2.213
               XCmlsd only supported if ftpFeatSiteDmlsd in SupportedExtensions - V7.01

  SiteDmlsd and XDmlsd - Download a directory listing of HostFileName to LocalFileName or LocalStream
               The path argument may be followed by optional argument -R or -subdirs
               for recursive directories, the path may be quoted if it includes spaces
               Same arguments as SiteCmlsd but returns listing on data channel
               SiteDmlsd only supported if ftpFeatSiteDmlsd in SupportedExtensions - V2.213
               XDmlsd only supported if ftpFeatSiteDmlsd in SupportedExtensions - V7.01

  Host       - Connect to domain (or IP address) specified in HostName (send before logon)
               Only supported if ftpFeatHost in SupportedExtensions, but can not be
                 sent once FEAT has been returned unless Rein sent first  - V7.01

  ConnectHost - Open the connection, send host, username, password, account

  Rein       - Re-initialise connection so logon process restarted and Host or User can
                 be sent again (in the original FTP RFC so should be supported by all servers)

  Lang       - Request server returns messages in language specified in Language (default EN)
               Only supported if ftpFeatLang in SupportedExtensions, with list of
                 allowed languages returned in FLangSupport, ie EN, ES, FR, GE with * the current language

  ConnectFeat - Open the connection, send username, password, account and FEAT request

  ConnectFeatHost - Open the connection, send host, username, password, account and FEAT request


  (There are two set of methods: Async and Sync. The Async are the prefered
   ones to build robust applications. Their name end with Async like GetAsync)
  (There are other less used methods, see code below)

How to use FTPS (TLS/SSL/Implicit SSL) ?                           - V2.106
   First you need to have ICS-SSL and recompile the component and your project
   having symbol USE_SSL defined. Then specify the SSLType, either use
   explicit command AUTH after Open command to request TLS/SSL protection of
   the control channel on standard port from server, or you may use one of
   the high level commands which will automatically invoke Auth TLS/SSl once
   connected to the server. Implicit SSL is another method that establishes
   always a secure control channel (no AUTH command is required). To protect
   the data channel as well issue "PBSZ 0" and "PROT P" once the control
   channel is protected, or "PROT C" to clear the data channel.
   Currently there's only one spec available specifying explizit SSL (AUTH TLS)
   it's RFC4217.

How to use a Proxy or Firewall ?
   First of all, not all proxies or firewalls are the same. So have a look at
   product documentation. However, most products support a transparent proxy
   which doesn't require any special programming:
   1) Instead of connection to a remote FTP server, you connect to the proxy
   2) User name is replaced by user name, followed by '@' sign then followed
      by target remote FTP server host name.
   3) Password is usual remote FTP server password.
   4) Most require using Passive mode.
   Example: You want to connect to ftp.borland.com, using anonymous connection,
            company firewall/proxy is running on host named proxyserver.
            FtpCli1.HostName := 'proxyserver';
            FtpCli1.UserName := 'anonymous@ftp.borland.com';
            FtpCli1.Password := 'your.email@company';
            FtpCli1.Passive  := TRUE;

FEAT Command - numerous extensions have been made to the FTP protocol over
the past few years, although support of these new commands is very sporadic.
RFC2389 describes the FEAT command, which returns a multiline list of
extension supported by the server. Note that the SIZE command is an extension,
and not supported on all FTP servers. An internet-draft 'Extensions to FTP'
document the most useful new commands. The responses of common FTP servers
to FEAT are listed below:

Microsoft IIS/5 in Windows 2000
500 'FEAT': command not understood

Microsoft IIS/6 in Windows 2003
211-FEAT
    SIZE
    MDTM
211 END

Microsoft FTP Service IIS/7 in Windows 2008
211-Extended features supported:
 LANG EN*
 UTF8
 AUTH TLS;TLS-C;SSL;TLS-P;
 PBSZ
 PROT C;P;
 CCC
 HOST
 SIZE
 MDTM
211 END

RhinoSoft Serv-U FTP 4.1
211-Extension supported
 MDTM
 MDTM YYYYMMDDHHMMSS[+-TZ] filename
 SIZE
 SITE PSWD;EXEC;SET;INDEX;ZONE;CHMOD;MSG
 REST STREAM
211 End

RhinoSoft Serv-U FTP Server v6.1
211-Extension supported
 AUTH TLS
 SSCN
 PBSZ
 PROT
 CCC
 CLNT
 MDTM
 MDTM YYYYMMDDHHMMSS[+-TZ];filename
 SIZE
 SITE PSWD;EXEC;SET;INDEX;ZONE;CHMOD;MSG
 REST STREAM
 XCRC filename;start;end
 MODE Z
 MLST Type*;Size*;Create;Modify*;Win32.ea*;
211 End

RhinoSoft Serv-U FTP Server v7.3
211-Extensions supported
    UTF8
    OPTS MODE;MLST;UTF8
    CLNT
    CSID Name; Version;
    HOST domain
    SITE PSWD;SET;INDEX;ZONE;CHMOD;MSG;EXEC;HELP
    AUTH TLS;SSL;TLS-C;TLS-P;
    PBSZ
    PROT
    CCC
    SSCN
    RMDA directoryname
    DSIZ
    AVBL
    MODE Z
    THMB BMP|JPEG|GIF|TIFF|PNG max_width max_height pathname
    REST STREAM
    SIZE
    MDTM
    MDTM YYYYMMDDHHMMSS[+-TZ];filename
    MFMT
    MFCT
    MFF Create;Modify;
    XCRC filename;start;end
    MLST Type*;Size*;Create;Modify*;Perm;Win32.ea;Win32.dt;Win32.dl
211 End (for details use "HELP commmand" where command is the command of interest)

RhinoSoft Serv-U FTP Server v10.0 (IP6 and full Unicode)
211-Extensions supported
 UTF8
 OPTS MODE;MLST;UTF8
 CLNT
 CSID Name; Version;
 HOST domain
 SITE PSWD;SET;ZONE;CHMOD;MSG;EXEC;HELP
 AUTH TLS;SSL;TLS-C;TLS-P;
 PBSZ
 PROT
 CCC
 SSCN
 RMDA directoryname
 DSIZ
 AVBL
 EPRT
 EPSV
 MODE Z
 THMB BMP|JPEG|GIF|TIFF|PNG max_width max_height pathname
 REST STREAM
 SIZE
 MDTM
 MDTM YYYYMMDDHHMMSS[+-TZ];filename
 MFMT
 MFCT
 MFF Create;Modify;
 XCRC filename;start;end
 MLST Type*;Size*;Create;Modify*;Perm;Win32.ea;Win32.dt;Win32.dl
211 End

Ipswich WS_FTP Server 3.14
211-Extensions supported
 SIZE
 MDTM
 MLST size*;type*;perm*;create*;modify*;
 LANG EN*
 REST STREAM
 TVFS
 UTF8
 AUTH SSL;TLS-P;
 PBSZ
 PROT C;P;
211 end

WS_FTP Server 6.1.1
211-Extensions supported
    SIZE
    XMD5
    XSHA1
    XSHA256
    XSHA512
    XQUOTA
    LANG EN, ES, FR, GE
    MDTM
    MLST size*;type*;perm*;create*;modify*;
    REST STREAM
    TVFS
    UTF8
    AUTH SSL;TLS-P;
    PBSZ
    PROT C;P;
211 end

Gene6 FTP Server v3.6.0
211-Extensions supported:
 AUTH TLS
 CCC
 CLNT
 CPSV
 EPRT
 EPSV
 MDTM
 MLST type*;size*;created;modify*;
 PASV
 PBSZ
 PROT
 REST STREAM
 SIZE
 SSCN
 TVFS
 UTF8
 XCRC "filename" SP EP
 XMD5 "filename" SP EP
211 End.

Gene6 FTP Server v3.10.0
211-Extensions supported:
 AUTH TLS
 CCC
 CLNT
 CPSV
 EPRT
 EPSV
 MDTM
 MFCT
 MFMT
 MLST type*;size*;create;modify*;
 PASV
 PBSZ
 PROT
 REST STREAM
 SIZE
 SSCN
 TVFS
 UTF8
 XCRC "filename" SP EP
 XMD5 "filename" SP EP
 XSHA1 "filename" SP EP
211 End.

Unknown Unix Daemon
211-Features:
 MDTM
 REST STREAM
 SIZE
211 End

Another Unix Daemon
FEAT
211-Features:
 MDTM
 REST STREAM
 SIZE
 AUTH TLS
 PBSZ
 PROT
211 End

FileZilla Server 0.9.10 beta
211-Features:
 MDTM
 REST STREAM
 SIZE
 MODE Z
 MLST type*;size*;modify*;
211 End

FileZilla Server version 0.9.24 beta
211-Features:
 MDTM
 REST STREAM
 SIZE
 MODE Z
 MLST type*;size*;modify*;
 MLSD
 AUTH SSL
 AUTH TLS
 UTF8
 CLNT
 MFMT
211 End

Internet Component Suite TFtpServer V1.39 and later
211-Extensions supported:
  SIZE
  REST STREAM
  MDTM
  MDTM YYYYMMDDHHMMSS[+-TZ] filename
  MLST size*;type*;perm*;create*;modify*;
  MFMT
  MD5
211 END

Internet Component Suite TFtpServer V1.54 and later (with SSL)
211-Extensions supported:
  SIZE
  REST STREAM
  MDTM
  MDTM YYYYMMDDHHMMSS[+-TZ] filename
  MLST size*;type*;perm*;create*;modify*;
  MFMT
  MD5
  XCRC "filename" start end
  XMD5 "filename" start end
  CLNT
  SITE INDEX;ZONE;MSG;EXEC;PSWD;CMLSD;DMLSD
  COMB
  MODE Z
  AUTH TLS;SSL;TLS-P;TLS-C
  CCC
  PROT C;P;
  PBSZ
211 END

Internet Component Suite TFtpServer V7.00 and later (with SSL)
211-Extensions supported:
  HOST
  SIZE
  REST STREAM
  MDTM
  MDTM YYYYMMDDHHMMSS[+-TZ] filename
  MLST size*;type*;perm*;create*;modify*;
  MFMT
  MD5
  XCRC "filename" start end
  XMD5 "filename" start end
  CLNT
  SITE INDEX;ZONE;MSG;EXEC;PSWD;CMLSD;DMLSD
  COMB
  MODE Z
  XCMLSD
  XDMLSD
  UTF8
  LANG EN*
  OPTS MODE;UTF8;
  AUTH TLS;SSL;TLS-P;TLS-C
  CCC
  PROT C;P;
  PBSZ
211 END


The extensions supported by V2.94 of this component are:

MLST - Machine Listing, two listing commands:
  MLSD - much better version of LIST directory
  MLST - list a single file
  an example response from either command is as follows:
  size=17199;type=file;perm=fdrwa;create=20030616152030;
       modify=20031001190114; 00master.zip
  (note the date and time includes seconds which is missing from most LIST
   responses, and the time stamps should be UTC - but don't seem to be in
   WS_FTP Server)
MDTM - Get File Modification UTC Time - useful for resumed downloads,
       to see if file has changed
SIZE - Get File Size - useful for resumed downloads, to see if file has changed
MDTM YYYYMMDDHHMMSS[+-TZ] filename - Set File Modification Time after
                                     upload (+0 is UTC)
MFMT - Modify File Modification Time after upload (UTC)
MD5  - Check MD5 hash sum for specified file, used to check for corruption

and by V7.01 and later:

XCRC - Check CRC32B hash sum for specified file, or part file
XMD5 - Check MD5 hash sum for specified file, or part file
CLNT - Send client information to server
SITE INDEX - simple listing of file names
SITE ZONE - set time zone
SITE MSG - send message to server
SITE EXEC - run program on server
SITE PSWD - change user's password
SITE CMLSD - extended directory listing with sub-directories, on control channel
SITE DMLSD - extended directory listing with sub-directories
COMB - combine two or more files together
MODE - set Z compression or no compression
XCMLSD - extended directory listing with sub-directories, on control channel
XDMLSD - extended directory listing with sub-directories
UTF8 - server support UTF8 encoding, use OPTS UTF8 ON/OFF to enable or disable,
       beware some servers ignore ON/OFF and always turn UTF8 on
LANG - set server language for messages


History:
Nov 04, 1996  Better error handling
              Property for timeout, default to 15 sec
Dec 03, 1996  Adapted display functionnality for Delphi 2
Dec 27, 1996  Added transmit functions
              Changed all procedure to function returning boolean status
Aug 13, 1997  Added multiline response support
              Added support for strange Microsoft PWS FTP behaviour
Sep 10, 1997  Added support for Dir and Ls commands
              Corrected bugs to enable correct use of separate commands
Oct 16, 1997  V2.07 Adapted for changes in TWSocket object
              Added FtpCliVersion constant
Nov 25, 1997  V2.08 Accept 250 as well as 226 for successful file transfert
              Suggested by fdragon@world-net.net
Nov 26, 1997  V2.09 don't display error message in the receive event when
              the socket is no more connected.
Nov 29, 1997  V2.10 added Mkd and Mkdir functions to create a directory.
              As suggested by Christian Rösner <christian.roesner@usa.net>
Dec 04, 1997  V2.11 Added Ren, Dele, Rmd functions
              As suggested by Frank Riemann <riemann@student.uni-kl.de>
              Changed Mkd and Mkdir functions to take HostFileName to
              specify the directory name. This is more consistent with the
              rest of the component usage.
Dec 29, 1997  V2.12 Added a TrimLeft function for Delphi 1
Dec 30, 1997  V2.13 Added Syst and System commands as suggested by
              Fire Dragon <fdragon@nosferatu.world-net.net>
              Added the LastResponse property
              Corrected a message ("Daniel Fazekas" <fdsoft@dns.gyor-ph.hu>)
Jan 10, 1998  V2.14 Accept response 150 and 125 for Get Submitted by Fire
              Dragon <fdragon@nosferatu.world-net.net>.
              Added a quick reference for most used properties and methods.
              Made TFtpCli a TComponent descendant.
              Added the Size, FileSize, Quote, DoQuote, RestartGet method.
              Made ControlSocket a readonly property (allow easy DNSLookup).
              Added a Port property.
Jan 25, 1998  V2.15
              Completely revised to make it asynchronous.
                This means that a new set of functions is born. They all have
                a name ending with Async. For example GetAsync. Asynchronous
                means that when you call the function, it returns almost
                immediately. The operation is done in the background.
                The asynchronous operation allows to make several request
                simultaneously WITHOUT using threads. Just use two or more
                TFtpClient and call each GetAsync (or other) method as those
                method returns almost instantly, all the request will be done
                in the background, generating the OnRequestDone when finished.
                Added a State property
                This allows to check for component work in the background.
                You can call methods only when State = ftpReady (except the
                Abort method which can be called at any time)
                The Asynchronous methods are the prefered ones.

              Added Pwd command
                Returns the current working directory on the server.

              Added CDup command
                Change to parrent directory on FTP server.

              Added DirResult property
                Parse the LastResponse property to return the directory.
                Do no always work when the server returns multi-line responses.
                (updated by Pwd, Cwd, CDup and Mkd commands).

              Changed function IsConnected to Connected, a read-only property.
                It's more object oriented.

              Replaced file I/O by stream I/O.
                It's the first step to allow Stream I/O outside of the component.

              New sample application (Delphi only now, CPP later).
                Every command has now a button to excercize it
                (async version only)

              The synchronous commands (old commands) are implemented by
                calling the asynchronous version and waiting.

              Multi-threaded property
                Tells the component how to wait for command completion.

              Removed the TWait component use.
                No need to have a TWait component.
Jan 31, 1998 V2.16 Accept response 150 and 125 for Put.
Feb 01, 1998 V2.17 Added intermediate message for OnRequestDone event
Feb 02, 1998 V2.18 Corrected a bug: all sync function returned always FALSE.
             Added User and Pass synchronous functions.
             Made PutAsync return ftpPutAsync in the OnrequestDone event.
Feb 04, 1998 V2.19 Added an OnCommand event to give a chance to the user to
             modify the commands to make some custom commands. Also added the
             OnResponse event to allow custom commands to get the response
             and alter it as necessary.
Feb 15, 1998 V2.20 Added a FindClose after the FindFirst in GetFileSize routine
             as pointed by "Daniel Fazekas" <fdsoft@dns.gyor-ph.hu>
Feb 21, 1998 V2.21 Enabled progress updated on put
Feb 22, 1998 V2.22 Accept result code 250 after Put command
             Implemented Append and AppendFile commands
Mar 07, 1998 V2.23 Made RequestType a R/O property
Mar 15, 1998 V2.24 Reordered PORT/REST/RETR
             Added a port command
             The ByteCount passed to OnProgress now take into account the
             restart byte offset.
             Renamed Display to TriggerDisplay and made it virtual
             Used TriggerDisplay everywhere.
             Modified the Timeout mechanism to reset the timeout each
             time the OnProgress event is called.
             Abort command call CancelDnsLookup approprietedly
Mar 27, 1998 V2.25 Adapted for C++Builder 3
Avr 01, 1998 V2.26 Made a valid LastResponse and ErrorMessage when DNS lookup
             failed. Added some compiler options.
Apr 10, 1998 V2.27 Added some ftpFctCwd in some highlevel functions.
             Suggested by Ray Andrews <ray_andrews@steeltoad.com>.
Apr 13, 1998 V2.28 Save error code when the data connection is closed to use
             it later to return the status for file transfert.
             Implemented passive mode, with help from Yaron Golan
             <yarong@shani.com>. A new property Passive enable this mode.
             Put do not work [yet] is passive mode.
Apr 14, 1998 V2.29 Made passive mode PUT work.
             Added ShareMode property (see TFileStream.Create on-line help)
             Made ResumeAt property.
Apr 15, 1998 V2.30 Added the OnReadyToTransmit event.
             Correctly handled error when local file not found.
             Checked if socket connected in SendCommand
Apr 22, 1998 V2.31 Corrected CDupAsync procedure (thanks to Eric
             Engler englere@swcp.com)
Apr 26, 1998 V2.32 Added TypeBinary and TypeAscii just to help a little bit.
May 01, 1998 V2.33 Added check for continuation lines in NextxPutAsync
May 05, 1998 V2.34 Added some more delay in WMFtpCloseData.
May 13, 1998 V2.35 In passive mode STOR or APPE, changed the sequence: now
             wait for connection established before sending the STOR or APPE
             command to FTP server.
May 19, 1998 V2.36 TransfertStats made virtual.
Jun 25, 1998 V2.37 Revised code for 'connection reset by peer' syndrome
Jul 09, 1998 V2.38 Adpted for Delphi 4
Jul 23, 1998 V2.39 Made ResumeAt property R/W
             Added code from Yaron Golan <yarong@shani.com> to fix PASV + REST
             and to add OnDisplayFile code to view a file on the fly.
Aug 04, 1998 V2.40 Frank Neuhaus <neuhaus@cpa.de> found a problem in Put command
             for some FTP server. See V240 in the comments.
Aug 12, 1998 V2.41 Added 200 to the valid CWD responses.
Aug 18, 1998 V2.42 Added code to accept continuation lines not beginning by
             a number and a dash. Thanks to Al Cantu <cantu@bfs.e-mail.com>
             for pointing this problem.
Sep 16, 1998 V2.43 Made Synchronize and WaitUntilReady virtual function in
             protected section.
Oct 01, 1998 V2.44 Checked for errors in TriggerRequestDone.
Nov 09, 1998 V2.45 Reverted V2.40 changes ! Thanks to Grant Walker
             <gw@enternet.com.au> for his help in testing.
             Made block size equal to 1514 to minimize packet fragmentation
             on ethernet.
Nov 22, 1998 V2.46 changed GetTickCount cast from Integer to LongInt because
             of overflows with Delphi 1. Suggested by Terry Byrne
             <terryb@fidlar.com>
Dec 22, 1998 V2.47 Corrected DisplayFile which forgot the last character.
             Thanks to max@zab1.chita.ru for the bug report.
             Handled exceptions while trying to connect data session.
             Replaced DisplayFlag by DysplayFileFlag.
Feb 14, 1999 V2.48 Indirectly call winsock functions using wsocket because
             wsocket provide runtime dynamic link instead of loadtime link.
             This allows a program to use FTP if it discover that winsock is
             installed and still run if winsock is not installed.
Mar 13, 1999 V2.49 Added FPutSessionOpened flag and combine it with
             FStorAnswerRcvd flag to synchronize start of data sending.
             Thanks to Frank Neuhaus for his clear analysis.
Mar 20, 1999 V2.50 Added Options property
Mar 23, 1999 V2.51 Corrected a bug introcuded in last version which truncated
             the first character of the second line of a multiline answer on
             some servers.
May 04, 1999 V2.52 Corrected an access violation in DataSocketGetDataAvailable.
             Thanks to Steve Plegge for pointing that bug.
May 21, 1999 V2.53 Added FRequestResult to ControlSocketDnsLookupDone. Thanks
             to Wu'hao <wvhao@263.net> for finding this bug.
Jul 17, 1999 V2.54 Added OnError event and DisplayFileMode property. Thanks to
             Pieter Snyman <pgws@iafrica.com> for his work.
             Accepted answer 200 for successful rename.
             Leho Kraav <macmanos@online.ee> found that some FTP server return
             this code.
             Cleared FByteCount from PortAsync as suggested by Simon Hoerup
             <cas@casdk.com> to help some progress indicator implementation.
Aug 04, 1999 V2.55 Corrected a bug with Delphi 1 where a buffer overflow may
             occurs when receiving commands longer than 254 bytes.
             Thanks to Craig Johnson <Craig_Johnson@emsinfo.com> for finding it.
             Also casted FTimeOut to LongInt for computation to prevent
             overflow with Delphi 1 for long timeout.
Aug 12, 1999 V2.56 HandleError was not correctly handling error message !
             thank to Kim Mølgård Nielsen <kmn@bcview.com>
Aug 20, 1999 V2.57 Revised conditional compilation, adapted for BCB4, set
             compile options same as TWSocket.
             Added DnsResult property as suggested by Heedong Lim
             <hdlim@dcenlp.chungbuk.ac.kr>. This property is accessible from
             OnStateChange when state is ftpWaitingBanner.
             Added checks for FLocalStream being destroyed.
Sep 5, 1999  V2.58 Heedong Lim <hdlim@dcenlp.chungbuk.ac.kr> found a missing
             assignation to FRequestResult in ControlSocketSessionConnected.
Sep 11, 1999 V2.59 Added OnBgException. Thanks to William Sorensen
             <tzimisce@mwaccess.net> for suggesting it.
Oct 30, 1999 V2.60 Changed TargetPort and DataPort from integer to WORD so
             that Delphi 1 is able to handle port greater than 32K. Bug and
             and fix by Duncan Gold <Gold@esg-us.com>.
Nov 22, 1999 V2.61 Allow continuation lines in all responses.
Nov 24, 1999 V2.62 RestPut command by Alexander Burlakov <alex@helexis.com>
             Added RestartPut. Added ftpNoAutoResumeAt option.
Dec 26, 1999 V2.63 Corrected a bug in DoPutAppendAsync.
Jan 24, 1999 V2.64 Added LongInt cast to all GetTickCount.
Apr 01, 2000 V2.65 Removed any set of integer.
             Thanks to Grant Black <grant.black@smartmove.co.nz>,
             Davie <smatters@smatters.com> and
             Stephen Williams <SWilliams@fm.optus.net.au> for their work on
             this subject.
Apr 09, 2000 V2.66 Proxy / Socks / Local streams support added.
             Pieter Snyman <pgws@iafrica.com> added proxy and socks support.
             Eric <erv@sympatico.ca> added stream support (assign LocalStream
             property to switch to stream mode and LocalFileName to switch to
             normal file mode).
Jun 10, 2000 V2.67 Added NOFORMS conditional compile to be able to build a
             program (console mode, dll or other) without using the forms unit
             (because forms unit makes programs much bigger). See NOFORMS
             related comments in wsocket.pas source file for correct use.
             See also OnMessagePump event and Terminated property.
Jul 15, 2000 V2.68 Added ProxyPort property. Handled non standard port when
             connecting thru proxy.
Jul 21, 2000 V2.69 Implemented check for ABOR, STAT and QUIT commands so that
             it doesn't check if previous command is done.
             By Davie <smatters@smatters.com>.
             Tomas Lannestedt <proprat@algonet.se> found a bug when using
             streams. Now it correctly handled stream clearing.
Sep 17, 2000 V2.70 Eugene Mayevski <Mayevski@eldos.org> moved Controls use
             out of NOFORMS way.
Nov 11, 2000 V2.71 Cleared FErrorMessage in ExecAsync. Thanks to Jake Traynham
             <jake@comm-unity.net> for finding this bug.
Nov 30, 2000 V2.72 Added a Sleep in DataSocketPutDataSent, and use CloseDelayed
             this will prevent some trucated file transfers.
Feb 17, 2001 V2.73 Better WaitUntilReady: check also ftpInternalReady.
             By Davie <smatters@smatters.com>.
Jun 16, 2001 V2.74 Added conditional compile for Delphi 6
Jun 18, 2001 V2.75 Use AllocateHWnd and DeallocateHWnd from wsocket.
Jul 26, 2001 V2.76 Accept range 150-159 for status code after RETR command.
             Peter Calum <pemca@tdk.dk> found some FTP server returning
             unusual status code (and not conforming to RFC !). I don't like to
             work arround other's bugs, but in this case this shouldn't hurt
             anything.
Jul 28, 2001 V2.77 Cleared FNextRequest in HighLevelAsync and TriggerRequestDone
             as suggested by Davie <smatters@smatters.com>.
             Added AbortXfer and AbortXferAsync to abort a running transfert
             without breaking connection.
Sep 09, 2001 V2.78 Beat Boegli <leeloo999@bluewin.ch> added LocalAddr property
             for multihomed hosts.
Sep 13, 2001 V2.79 Bug fix by Beat Boegli <leeloo999@bluewin.ch> related to
             his previous changes. Now works with passive mode put.
Nov 02, 2001 V2.80 Added DisplayLastResponse in ControlSocketDataAvailable to
             give continuation lines to OnDisplay event.
             Accept 250 answer as well as 257 for MKD command as suggested by
             Simon Horup <cas@casdk.com>.
Feb 12, 2002 V2.82 "Soltann" <soltann@wanadoo.fr> added code to extract IP
             from passive mode reply so that transfer from another server is
             possible (See TargetIP in code).
Apr 06, 2002 V2.83 Added code 257 to allowed code list for CDUpAsync as
             suggest by <Davie@smatters.com>.
             Fixed a problem in ControlSocketSessionClosed where error code was
             not checked. Bug found by <Davie@smatters.com>.
Apr 20, 2002 V2.84 Removed useless units from uses clause.
Jun 28, 2002 V2.85 Removed check for ftpFctPut and FPassive in
             TriggerRequestDone which cause trouble with passive mode and sync
             operation. Thanks to "Gunnar" <gulb@gmx.de>.
Oct 26, 2002 V2.85 Use wsoNoReceiveLoop option with data session.
             Set OnSessionAvalable handler before calling listen as suggested
             by Gunnar <gulb@gmx.de>.
Nov 11, 2002 V2.86 Changed FtpCliDeallocateHWnd argument from HWND to Cardinal
             becasue BCB doesn't like HWND when deriving acomponent from
             TFtpCli (BCB bug ?).
Jan 04, 2003 V2.87 Published OnError event
Feb 15, 2003 V2.88 Ignore empty lines in responses from server. Thansk to
             Chris Rendell <appsolca@yahoo.ca> for finding this one.
Oct 26, 2003 V2.90 Added FEAT, MLSD, MDTM commands - not supported by all
             servers (not IIS5) added LastMultiResponse with last command
             response, may be multiple lines, all with CRLF by Angus Robertson,
             angus@magsys.co.uk
Jan 03, 2004 V2.91 Made OnMessagePump public, use it when MultiThread is true.
May 20, 2004 V2.92 Implemented FTPS (SSL) support. Require ICS-SSL.
Aug 20, 2004 V2.93 Use MsgWaitForMultipleObjects in WaitUntil ready instead
             of simple Sleep(). This make CPU use slow down considerably.
Aug 22, 2004 V2.94 Angus Robertson, added MFMT modify file modification time
             added MD5 command to get file hash sum (use FileMD5(file) in
             md5.pas to check it)
             added SupportedExtensions property set of TFtpFeat which features
             server supports (note SupportedExtensions replaces SupportXXX
             flags which have gone)
Aug 30, 2004 V2.95 Added option ftpWaitUsingSleep so that WaitUntilReady use
             Sleep instead of MsgWaitForMultipleObjects because Angus said he
             found a case where it is faster to use Sleep.
Oct 31, 2004 V2.96 Added support for ACCT command.
             Thanks to Igor Siticov <support@sicomponents.com> for his code.
Dec 06, 2004 V2.97 In TCustomFtpCli.HandleBackGroundException: Replaced Abort
             by AbortAsync. Thanks to Lee Grissom <leegrissom@earthlink.net>
             for finding this one.
May 23, 2005 V2.98 Added methods DataSocketPutAppendInit and DataSocketGetInit
             to ease SSL enabled version.
             Fixed DataSocketPutDataSent to call ShutDown instead of
             CloseDelayed. This avoid file truncation that rarely occured.
May 26, 2005 V2.99 Fixed Md5Async to correctly use MD5 command. Thanks to
             Johan 'Zarkow' Munkestam <Zarkow@missy.mine.nu>.
Sept 4, 2005 V2.100 fixed a long term problem with the data connection being
             closed before winsock had sent the last block, seemed to happen
             only with put files about 9 to 10K in size, now waiting until
             wsocket has sent the correct number of characters
             Also raise exception if put fails with a wsocket error
             Thanks to Tobias Giesen <tobias@tgtools.de> for the fix
             Allow TFtpCli to used again if linked with SSL
             Also minor additions to some response codes,
             and some async literal fixes, by Angus Robertson, my original bugs
             improved TransfertStats, allow wrap at 49 days, don't show 0 secs or silly bps
Sept 6, 2005 V2.101 64-bit support for Delphi 6 and later, for transfers larger
             than 2 gigs.  Note only LocalFileName mode supports resume over 2 gigs,
             due to limitation in TStream.  Check if seek fails and report error.
             Report real error opening local file (might be seek range error, not IO)
             by Angus Robertson, angus@magsys.co.uk
Dec 15, 2005 V2.102 A. Garrels,  missing SSL-properties added to
             DataSocketPutSessionAvailable.
Dec 18, 2005 V2.103 fix for zero duration transfers, by Angus
             Added Mode Z support which compresses data using ZLIB
Dec 30, 2005 V2.104 A. Garrels, added IcsLogger.
Jan 03, 2006  V2.105 Added TBufferedFileStream from Arno Garrels to improve performance, by Angus
             Replaced more old debug code with new loggin, by Angus
Jan 10, 2006 V2.106 Commands "AUTH TLS" and "CCC" added, re-issuing of the AUTH
             command performs a SSL re-negotiation now instead of an initial
             SSL handshake, see RFC4217, changes by Arno Garrels.
Aug 06, 2006 V2.107 Angus using GetWinsockErr in wsocket to give consistent textual
             and numeric winsock errors
             wsocket fix for 64-bit transfers with range checking enabled
             added XCRC command to get CRC32 for file (for servers without MD5)
             added KeepAliveSecs property to enable keepalive on control channel
             to stop routers and firewalls closing connection on no activity
             fix conditionals for __DataSocket used for logging not SSL (Arno)
             SSL check Self = TSslFtpClient before accessing SSL properties (Arno)
Aug 29, 2006 V2.108 A.Garrels reworked stream 64-bit support.
Mar 27, 2007 V2.109 Tobias Rapp fixed DoPutAppendAsync for missing SetErrorMessage
May 06, 2007 V2.110 Exposed LocalStreamWrite as virtual method.
Jul 05, 2007 V2.111 Angus fixed long term bug where ResumeAt was not always reset
             before directory listings causing them to fail
Aug 26, 2007 V2.112 Fixed TCustomFtpCli.DoGetAsync for Resume with stream
Jan 06, 2008 V2.113 recognise more FEAT extensions, by Angus Robertson, angus@magsys.co.uk
             added support for One Time Passwords (aka S/Key), otp-md5, otp-md4
                 and otp-sha1, see RFC2289, this automatic no changes needed
             added Clnt command to sent client information to server
             added Allo command to check if server can allocate disk space for an upload
             added Comb command to ask server to combine two or more uploaded files
             added Xmd5 similar to Md5 but PosStart and PosEnd parameters
             Xcrc now supports PosStart and PosEnd parameters
             added SitePswd command to change the account password on the server
             added SiteExec command to execute a progam at the server
             added SiteIndex command to download a recusive directory and file name listing
             added SiteZone command to get the server time zone difference from UTC
             added SiteMsg command to send a message to the server
             added SiteDmlsd command, similar to MLSD but optional argument -R or -subdirs
               for recursive directories, the path may be quoted if it includes spaces
             added SiteCmlsd command, similar to SiteDmlsd but uses control channel to
              avoid lots of small data channels sessions but may be limited by some clients
             NOTE: SiteDmlsd and SiteCmlsd are new commands supported only by ICS FTP Server,
               and both are used similarly to LIST and MLSD, ditto SITE INDEX
             new property StartTick which is tick count when file transfer started,
               and DurationMsecs which is how many ticks it took to finish, but note
               this is only set if the transfer finished normally, I think...
             added LookupFTPReq and LookupFtpState functions to ease logging state information
             file sizes and performance now reported in KBytes or MBytes instead of long bytes
             added OnZlibProgress event for progress, allow cancel and a message pump
                since zlib is blocking
             ZLIB use FileStream and a temporary file instead of MemoryStream which seems
                to be 10 to 20 times faster for large files (30 megs)
             ZlibWorkDir runtime property is path where temporary ZLIB files are written
                  defaulting to system temp path
             improved error handling decompressing ZLIB stream
             (usage of new commands is detailed above in quick reference)
Mar 24, 2008 V6.01 Bumped version number to 6.01
             Francois Piette made some changes to prepare code for Unicode.
Jun 25, 2008 V6.02 A. Garrels, ZlibOnProgress needs to be compiled conditionally.
             SSL code merged.
Jun 28, 2008 v6.03 **Breaking Change** enum item "sslTypeImplizit" renamed to
             "sslTypeImplicit".
May 01, 2008 V6.04 A.Garrels added function LocalStreamWriteString to prepare
             code for Unicode changed some types from char to AnsiChar.
May 02, 2008 V6.05 A.Garrels changed code to get the temporary directory for
             ZLIB in TCustomFtpCli.Create.
May 15, 2008 V6.06 A.Garrels added OverbyteIcsLibrary.pas to uses clause.
             Some type changes from String to AnsiString of published properties.
Aug 11, 2008 V6.07 A. Garrels - Type AnsiString rolled back to String.
Oct 03, 2008 V6.10 A. Garrels moved IsDigit, IsCRLF, IsSpaceOrCRLF and StpBlk
                   to OverbyteIcsUtils.pas.
Nov 10, 2008 V7.00 Angus removed old compiler code
             increased buffer sizes from 1460 and 4096, both to 32768
             fixed SOCKS settings never implemented properly and lost after one connection
Nov 14, 2008 V7.01 Angus added UTF-8 and code page support, full Unicode is only availble
               if built with Delphi 2009 or later.  To support UTF-8, first send
               the FEAT command and check ftpFeatUtf8 in SupportedExtensions, then send
               OPTS command with NewOpts 'UTF8 ON' and set CodePage to CP_UTF8.  The
               directory stream will be UTF-8 and needs translation in the application.
             updated more FEAT comments for various FTP servers and more command usage
             changed MdtmyyAsync so it no longer adds +0 since newer Serv-U then fails
             added FTP commands HOST hostname (before logon) and REIN (re-initialise connection)
             added ConnectHost that does Open/Host/User/Pass
             added XCMLSD, XDMLSD and LANG commands and check matching FEAT options
Nov 16, 2008 V7.02 Arno simplified some code page related code, added option
             ftpAutoDetectCodePage which actually detects UTF-8 only, exchanged
             RawByteString by AnsiString in several routines.
Nov 18, 2008 V7.03 Arno - Protection level on the data channel was not set
             properly. Set it only in case of PROT command succeeded.
Nov 21, 2008 V7.04 Arno - Allow C++ Builder
Jan 7, 2009  V7.05 Angus - allow 200 response for HOST (for ws_ftp server)
Apr 6, 2009  V7.06 Angus check response for XMD5 properly (220 or 250 and no file name)
Apr 16, 2009 V7.07 Angus assume STREAM64, USE_MODEZ, USE_ONPROGRESS64_ONLY, USE_BUFFERED_STREAM
             Remove old conditional and suppressed code, OnProgress gone (BREAKING CHANGE)
Jan 4, 2010  V7.08 added TriggerResponse virtual and CreateSocket virtual
             ConnectAsync and ConnectHostAsync methods now trigger FEAT command
			 Thanks to "Anton Sviridov" <ant_s@rambler.ru>
Jun 9, 2010  V7.09 Angus - ConnectAsync and ConnectHostAsync methods no longer trigger FEAT command
             Added ConnectFeatAsync and ConnectFeatHostAsync methods which do trigger FEAT command
Sep 8, 2010  V7.10 Arno - If conditional BUILTIN_THROTTLE is defined the
             bandwidth control uses TWSocket's built-in throttle code rather
             than TFtpClient's.
Sep 19, 2010 V7.11 Arno - Do not call DataSocketPutDataSent twice! This fixes
             a bug that showed up with experimental built-in throttle but could
             also trigger without as well. For instance, if file size is smaller
             or equal send buffer size a second call to DataSocketPutDataSent
             turns on the shutdown timeout (loop) even though data is not yet
             sent. DataSocketPutDataSent MUST only be called once to init
             the send loop.
Sep 20, 2010 V7.12 Angus - ensure FMultiThreaded in TIcsWndControl is set correctly and
               not locally here
Oct 10, 2010 V7.13 Arno - MessagePump changes/fixes.
Oct 15, 2010 V7.14 Arno - Fake AUTHTLS request/response if renegotiation is
             not available and the SSL is already established.
Nov 08, 2010 V7.15 Arno improved final exception handling, more details
             in OverbyteIcsWndControl.pas (V1.14 comments).
Nov 11, 2010 V7.16 Arno re-enabled component notification for FDataSocket and
             FControlSocket that was disabled accidentally in V7.13 rev. #610 in
             method CreateSocket by creating the instances with a nil owner.
Nov 17, 2010 V7.17 Arno published property ProxyPort.
Feb 09, 2011 V7.18 Arno added HTTP v1.1 proxy-support and some conditional
             defines to be able to build with smaller internal buffers.
Feb 13, 2011 V7.19 Arno fixed a bug in TCustomFtpCli.ControlSocketSessionClosed
             that caused RequestDone being triggered with error code null if
             the server unexpectedly closed the connection without sending any
             response. Error messages from proxy servers are now available.
Feb 14, 2011 V7.20 Arno - Clear HTTP tunnel server name from data and ctrl
             socket properly as well as properties LastResponse, ErrorMessage
             and StatusCode in method OpenAsync.
Feb 15, 2011 V7.21 Arno - HTTP tunnel server name now cleared correctly.
             Use function WSocketIsProxyErrorCode from OverbyteIcsWSocket.pas.
Mar 01, 2011 V7.22 Arno - **Warning, possibly a breaking change**. The component
             does no longer enforce passive mode with native FTP-proxy
             connections, it's an option now.
Apr 15, 2011 V7.23 Arno prepared for 64-bit.
May 21, 2011 V7.24 Arno - Call CloseDelayed rather than Close in
             TCustomFtpCli.DoneQuitAsync in order to avoid error #10053 in
             OnSessionClosed event with SSL.
Jun 18, 2011 V7.25 aguser removed one compiler hint.
Jul 24, 2011 V7.26 Arno added published property DataSocketSndBufSize
             and public property DataSocketRcvBufSize. Increase DataSocketSndBufSize
             in order to make uploads faster. Both values default to value 8192
             which is the default winsock size. Removed useless call to
             WSocket_getsockopt in TCustomFtpCli.DataSocketPutSessionAvailable.
Oct 24, 2011 V7.27 Arno - Set state ftpInternalReady in DoneQuitAsync.
             Check for component connected in PortAsync.
Jan 20, 2012 V7.28 Arno - If the control connection closes with error code
             after QUIT response has been received OK we may safely ignore this
             error.
Apr 06, 2012 V7.29 **** BREAKING CHANGE ****
             Arno - Request type of two methods changed/corrected:
             1) ConnectFeatAsync() from ftpConnectAsync to ftpConnectFeatAsync.
             2) ConnectFeatHostAsync() from ftpConnectHostAsync to ftpConnectFeatHostAsync.
             These changes might require to adjust your OnRequestDone handler.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Mar 18, 2013 V8.01 - Angus added LocalAddr6 for IPv6
             Note: SocketFamily must be set to sfAny, sfIPv6 or sfAnyIPv6 to
                   allow a host name to resolve to an IPv6 address.
Mar 10, 2013 V8.02 - Arno added property ExternalIPv4. If specified, usually NAT
             router's public IP, this IP is sent with the PORT command to the server.
             Makes active mode possible behind NAT in case the router isn't smart
             enough to handle active mode automatically or when the control connection
             is encrypted. The NAT router then must also be configured to forward
             incoming packets from the server properly to the client, specifying a
             DataPortRange helps in this context.
Jul 24, 2013 V8.03 - Angus added more error reporting for not ready and more
                debug logging.
             Fixed bug in WaitUntilReady that meant some sync methods with multiple
               commands randomly terminated prematurely allowing further commands to
               be sent usually resulting in not ready errors.
Feb 07, 2014 V8.04 - Arno, in DoneQuitAsync call FControlSocket.Close rather than
             CloseDelayed.
Dec 02, 2014 V8.05 - Angus fixed PBSZAsync set incorrect TFtpFct
Dec 10, 2014 V8.06 - Angus added SslHandshakeRespMsg for better error handling

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsFtpCli;
{$ENDIF}

{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$IFNDEF BUILTIN_THROTTLE}
    {$DEFINE UseBandwidthControl}   { V2.106 }
{$ELSE}
    {$UNDEF UseBandwidthControl}
{$ENDIF}
{$H+}         { Use long strings                    }
{$J+}         { Allow typed constant to be modified }
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}
{_DEFINE TRACE}


interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    OverbyteIcsWinSock,
{$ENDIF}
{$IFDEF POSIX}
    System.IOUtils,
    Posix.Unistd,
    Posix.SysSocket,
    Ics.Posix.WinTypes,
    Ics.Posix.Messages,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
{$IFNDEF NOFORMS}
  {$IFDEF FMX}
    FMX.Forms,
  {$ELSE}
    {$IFDEF RTL_NAMESPACES}Vcl.Forms{$ELSE}Forms{$ENDIF},
  {$ENDIF}
{$ENDIF}
{ You must define USE_SSL so that SSL code is included in the component.   }
{ Either in OverbyteIcsDefs.inc or in the project/package options.         }
{$IFDEF USE_SSL}
    OverbyteIcsSSLEAY, OverbyteIcsLIBEAY,
{$ENDIF}
{$I Include\OverbyteIcsZlib.inc}
    OverbyteIcsZlibHigh,     { V2.102 }
{$IFDEF USE_ZLIB_OBJ}
    OverbyteIcsZLibObj,     {interface to access ZLIB C OBJ files}
{$ELSE}
    OverbyteIcsZLibDll,     {interface to access zLib1.dll}
{$ENDIF}
{$IFNDEF NO_DEBUG_LOG}
    OverbyteIcsLogger,
{$ENDIF}
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsWndControl,
    Ics.Fmx.OverbyteIcsWSocket,
{$ELSE}
    OverbyteIcsWndControl,
    OverbyteIcsWSocket,
{$ENDIF}
    OverbyteIcsStreams,
    OverbyteIcsUtils,
    OverbyteIcsOneTimePw,  { V2.113 }
    OverByteIcsFtpSrvT;

const
  FtpCliVersion      = 806;
  CopyRight : String = ' TFtpCli (c) 1996-2014 F. Piette V8.06 ';
  FtpClientId : String = 'ICS FTP Client V8.06 ';   { V2.113 sent with CLNT command  }

const
//  BLOCK_SIZE       = 1460; { 1514 - TCP header size }

{$IFDEF FTPCLI_BUFFER_OLD}
  FTP_SND_BUF_SIZE = 1460;  { arno V7.18 }
  FTP_RCV_BUF_SIZE = 4096;  { arno V7.18 }
{$ELSE}
  {$IFDEF FTPCLI_BUFFER_SMALL}
    FTP_SND_BUF_SIZE = 8192;  { arno V7.18 }
    FTP_RCV_BUF_SIZE = 8192;  { arno V7.18 }
  {$ELSE}
    {$IFDEF FTPCLI_BUFFER_MEDIUM}
      FTP_SND_BUF_SIZE = 16384;  { arno V7.18 }
      FTP_RCV_BUF_SIZE = 16384;  { arno V7.18 }
    {$ELSE}
      FTP_SND_BUF_SIZE = 32768;  { angus V7.00 increased from 1460 }
      FTP_RCV_BUF_SIZE = 32768;  { angus V7.00 increased from 4096 }
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

type
  { sslTypeAuthTls, sslTypeAuthSsl are known as explicit SSL }
  TFtpCliSslType  = (sslTypeNone, sslTypeAuthTls, sslTypeAuthSsl,        { V2.106 }
                     sslTypeImplicit);
  TFtpOption      = (ftpAcceptLF, ftpNoAutoResumeAt, ftpWaitUsingSleep,
                     ftpBandwidthControl, ftpAutoDetectCodePage); { V2.106 }{ AG V7.02 }
  TFtpOptions     = set of TFtpOption;
  TFtpExtension   = (ftpFeatNone, ftpFeatSize, ftpFeatRest, ftpFeatMDTMYY,
                     ftpFeatMDTM, ftpFeatMLST, ftpFeatMFMT, ftpFeatMD5,
                     ftpFeatAuthSSL, ftpFeatAuthTLS, ftpFeatProtP,
                     ftpFeatProtC, ftpFeatModeZ, ftpFeatCcc, ftpFeatPbsz,
                     ftpFeatXCrc,  ftpFeatXMD5,  ftpFeatSitePaswd,          { V2.113 }
                     ftpFeatSiteExec, ftpFeatSiteIndex, ftpFeatSiteZone,    { V2.113 }
                     ftpFeatSiteMsg, ftpFeatSiteCmlsd, ftpFeatSiteDmlsd,    { V2.113 }
                     ftpFeatClnt, ftpFeatComb, ftpFeatUtf8, ftpFeatLang,    { V2.113 }
                     ftpFeatHost, ftpFeatXCmlsd, ftpFeatXDmlsd);            { V7.01 }
  TFtpExtensions  = set of TFtpExtension; { V2.94 which features server supports }
  TFtpTransMode   = (ftpTransModeStream, ftpTransModeZDeflate) ;  { V2.102 }
  TZStreamState   = (ftpZStateNone, ftpZStateSaveDecom, ftpZStateSaveComp{,
                     ftpZStateImmDecon, ftpZStateImmComp});   { V2.102 }
  TFtpState       = (ftpNotConnected,  ftpReady,         ftpInternalReady,
                     ftpDnsLookup,     ftpConnected,     ftpAbort,
                     ftpInternalAbort, ftpWaitingBanner, ftpWaitingResponse,
                     ftpPasvReady);
  TFtpRequest     = (ftpNone,          ftpOpenAsync,     ftpUserAsync,
                     ftpPassAsync,     ftpCwdAsync,      ftpConnectAsync,
                     ftpReceiveAsync,  ftpDirAsync,      ftpLsAsync,
                     ftpPortAsync,     ftpGetAsync,      ftpDirectoryAsync,
                     ftpListAsync,     ftpSystemAsync,   ftpSystAsync,
                     ftpQuitAsync,     ftpAbortXferAsync,
                     ftpSizeAsync,     ftpPutAsync,      ftpAppendAsync,
                     ftpFileSizeAsync, ftpRqAbort,       ftpMkdAsync,
                     ftpRmdAsync,      ftpRenameAsync,   ftpDeleAsync,
                     ftpRenAsync,      ftpRenToAsync,    ftpRenFromAsync,
                     ftpDeleteAsync,   ftpMkdirAsync,    ftpRmdirAsync,
                     ftpPwdAsync,      ftpQuoteAsync,    ftpCDupAsync,
                     ftpDoQuoteAsync,  ftpTransmitAsync, ftpTypeSetAsync,
                     ftpRestAsync,     ftpRestGetAsync,  ftpRestartGetAsync,
                     ftpRestPutAsync,  ftpRestartPutAsync,
                     ftpMlsdAsync,     ftpFeatAsync,     ftpMlstAsync,
                     ftpMdtmAsync,     ftpMdtmyyAsync,   ftpAuthAsync,
                     ftpMfmtAsync,     ftpMd5Async,      ftpAccountAsync,
                     ftpProtAsync,     ftpPbszAsync,     ftpModeZAsync,
                     ftpOptsAsync,     ftpCccAsync,      ftpXCrcAsync,
                     ftpClntAsync,     ftpSitePaswdAsync, ftpSiteExecAsync,    { V2.113 }
                     ftpSiteIndexAsync, ftpSiteZoneAsync, ftpSiteMsgAsync,     { V2.113 }
                     ftpSiteCmlsdAsync, ftpSiteDmlsdAsync, ftpAlloAsync,       { V2.113 }
                     ftpCombAsync,     ftpXMd5Async,      ftpConnectHostAsync, { V2.113 }
                     ftpReinAsync,     ftpHostAsync,      ftpLangAsync,        { V6.09 }
                     ftpXCmlsdAsync,   ftpXDmlsdAsync,    ftpConnectFeatAsync, { V7.01 }
                     ftpConnectFeatHostAsync );                                    { V7.09 }
  TFtpFct         = (ftpFctNone,       ftpFctOpen,       ftpFctUser,
                     ftpFctPass,       ftpFctCwd,        ftpFctSize,
                     ftpFctMkd,        ftpFctRmd,        ftpFctRenFrom,
                     ftpFctRenTo,      ftpFctGet,        ftpFctDir,
                     ftpFctQuit,       ftpFctSyst,       ftpFctDele,
                     ftpFctPwd,        ftpFctQuote,      ftpFctPut,
                     ftpFctTypeSet,    ftpFctRest,       ftpFctCDup,
                     ftpFctLs,         ftpFctAppend,     ftpFctPort,
                     ftpFctAbortXfer,  ftpFctMlsd,       ftpFctFeat,
                     ftpFctMlst,       ftpFctMdtm,       ftpFctMdtmyy,
                     ftpFctAuth,       ftpFctMfmt,       ftpFctMd5,
                     ftpFctAcct,       ftpFctProt,       ftpFctPbsz,
                     ftpFctModeZ,      ftpFctOpts,       ftpFctCcc,
                     ftpFctXCrc,       ftpFctClnt,       ftpFctSitePaswd,     { V2.113 }
                     ftpFctSiteExec,   ftpFctSiteIndex,  ftpFctSiteZone,      { V2.113 }
                     ftpFctSiteMsg,    ftpFctSiteCmlsd,  ftpFctSiteDmlsd,     { V2.113 }
                     ftpFctAllo,       ftpFctComb,       ftpFctXMd5,          { V2.113 }
                     ftpFctRein,       ftpFctHost,       ftpFctLang,          { V6.09 }
                     ftpFctXCmlsd,     ftpFctXDmlsd);                         { V7.01 }
  TFtpFctSet      = set of TFtpFct;

  TFtpShareMode   = (ftpShareCompat,    ftpShareExclusive,
                     ftpShareDenyWrite, ftpShareDenyRead,
                     ftpShareDenyNone);
  TFtpDisplayFileMode = (ftpLineByLine, ftpBinary);
  TFtpConnectionType  = (ftpDirect, ftpProxy, ftpSocks4, ftpSocks4A, ftpSocks5, ftpHttpProxy);
  TFtpDisplay     = procedure(Sender    : TObject;
                              var Msg   : String) of object;
  TFtpProgress64  = procedure(Sender    : TObject;
                              Count     : Int64;
                              var Abort : Boolean) of object;
  TFtpCommand     = procedure(Sender    : TObject;
                              var Cmd   : String) of object;
  TFtpRequestDone = procedure(Sender    : TObject;
                              RqType    : TFtpRequest;
                              ErrCode   : Word) of object;
  TFtpReadyToTransmit = procedure(Sender      : TObject;
                                  var bCancel : Boolean) of object;
  TFtpNextProc    = procedure of object;
  TZlibProgress   = procedure(Sender: TObject;
                              Count: Int64;
                              var Cancel: Boolean) of object;  { V2.113 }

  FtpException = class(Exception);

  TCustomFtpCli = class(TIcsWndControl)
  protected
    FHostName           : String;
    FPort               : String;
    FSocketFamily       : TSocketFamily;
    FCodePage           : LongWord;
    FSystemCodepage     : LongWord; { AG 7.02 }
    FDataPortRangeStart : LongWord;  {JT}
    FDataPortRangeEnd   : LongWord;  {JT}
    FLastDataPort       : LongWord;  {JT}
    FExternalIPv4       : String;  {V8.02}
    FDSocketSndBufSize  : Integer;{AG V7.26}
    FDSocketRcvBufSize  : Integer;{AG V7.26}
    FLocalAddr          : String; {bb}
    FLocalAddr6         : String; { V8.01 IPv6 address for local interface to use }
    FUserName           : String;
    FPassWord           : String;
    FAccount            : String;
    FLocalFileName      : String;
    FHostFileName       : String;
    FHostDirName        : String;
    FDnsResult          : String;
    FType               : Char;
    FShareMode          : Word;
    FDisplayFileMode    : TFtpDisplayFileMode;
    FConnectionType     : TFTPConnectionType;
    FProxyServer        : String;
    FProxyPort          : String;
    FAppendFlag         : Boolean;
    FDisplayFileFlag    : Boolean;
    FControlSocket      : TWSocket;
    FDataSocket         : TWSocket;
    FStartTime          : LongInt;
    FStopTime           : LongInt;
    FState              : TFtpState;
    FStatusCode         : LongInt;
    FRequestResult      : Integer;
    FFctSet             : TFtpFctSet;
    FFctPrv             : TFtpFct;
    FHighLevelResult    : Integer;
    FHighLevelFlag      : Boolean;
    FRestartFlag        : Boolean;
    FMsg_WM_FTP_REQUEST_DONE : UINT;
    FMsg_WM_FTP_SENDDATA     : UINT;
    FMsg_WM_FTP_CLOSEDOWN    : UINT;
    FOptions            : TFtpOptions;
    FOnDisplay          : TFtpDisplay;
    FOnDisplayFile      : TFtpDisplay;
    FOnError            : TFtpDisplay;
    FOnCommand          : TFtpCommand;
    FOnResponse         : TNotifyEvent;
    FOnSessionConnected : TSessionConnected;
    FOnSessionClosed    : TSessionClosed;
    FOnStateChange      : TNotifyEvent;
    FOnRequestDone      : TFtpRequestDone;
    FOnReadyToTransmit  : TFtpReadyToTransmit;
    FLocalStream        : TStream;
    FRequestType        : TFtpRequest;
    FRequestDoneFlag    : Boolean;
    FReceiveBuffer      : array [0..FTP_RCV_BUF_SIZE - 1] of AnsiChar;
    FReceiveLen         : Integer;
    FLastResponse       : String;
    FLastResponseSave   : String;  { To save FLastResponse when quitting }
    FPasvResponse       : String;  { To fix REST + PASV transfers }
    FStatusCodeSave     : LongInt; { To save FStatusCode when quitting }
    FErrorMessage       : String;
    FError              : Word;    { To save Error when data connection closed }
    FGetCommand         : String;
    FConnected          : Boolean;
    FSendBuffer         : array [0..FTP_SND_BUF_SIZE - 1] of AnsiChar;  { angus 7.00 }
    FOnProgress64       : TFtpProgress64;
    FByteCount          : TFtpBigInt;         { V2.108 }
    FSizeResult         : TFtpBigInt;         { V2.108 }
    FResumeAt           : TFtpBigInt;         { V2.108 }
    FDirResult          : String;
    FNext               : TFtpNextProc;
    FWhenConnected      : TFtpNextProc;
    FDoneAsync          : TFtpNextProc;
    FOkResponses        : array [0..15] of Integer;
    FNextRequest        : TFtpNextProc;
    FServerSaidDone     : Boolean;
    FFileReceived       : Boolean;
    FFileSent           : Boolean;
    FPassive            : Boolean;
    FEofFlag            : Boolean;
    FStorAnswerRcvd     : Boolean;
    FPutSessionOpened   : Boolean;
    FStreamFlag         : Boolean;
    FDataSocketSentFlag : Boolean;    { V7.11 }      
    FSupportedExtensions : TFtpExtensions; { V2.94  which features server supports }
    FMLSTFacts          : String;     { V2.90  specific new list stuff supported   }
    FRemFileDT          : TDateTime;  { V2.90  date/time for MdtmAsync and MdtmYYYYAsync and MfmtAsync }
    FRemFacts           : String;     { V2.90 response to MLST command, facts about remote file }
    FLastMultiResponse  : String;     { V2.90  last command response, may be multiple lines, all with CRLF }
    FMd5Result          : String;     { V2.94 result for MD5 }
    FCloseEndTick       : LongWord;   { V2.100 to avoid waiting for ever  }
    FCloseEndSecs       : LongWord;      { V2.100 how long to wait for final packet to be sent }
    FNewOpts            : string;        { V2.102 arguments for OPTS command }
    FTransferMode       : TFtpTransMode; { V2.102 new tranfer mode }
    FCurrTransMode      : TFtpTransMode; { V2.102 current transfer mode }
    FSslType            : TFtpCliSslType;{ V2.106 }
    FCrcResult          : String;        { V2.107 result for XCRC }
    FKeepAliveSecs      : integer;       { V2.107 zero means window default }
    FClientIdStr        : String;        { V2.113 string sent for CLNT command }
    FPosStart           : TFtpBigInt;    { V2.113 start pos for MD5/CRC }
    FPosEnd             : TFtpBigInt;    { V2.113 end pos for MD5/CRC }
    FDurationMsecs      : Integer;       { V2.113 last transfer duration in milliseconds for FByteCount }
    FSocksPassword      : String;        { V7.00 }
    FSocksPort          : String;        { V7.00 }
    FSocksServer        : String;        { V7.00 }
    FSocksUserCode      : String;        { V7.00 }

    FHttpTunnelAuthType : THttpTunnelAuthType;
    FHttpTunnelPassword : String;        { V7.18 }
    FHttpTunnelPort     : String;        { V7.18 }
    FHttpTunnelServer   : String;        { V7.18 }
    FHttpTunnelUserCode : String;        { V7.18 }

    FLanguage           : String;        { V7.01 language argment for LANG command }
    FLangSupport        : String;        { V7.01 list of languages server supports }
    FZStreamState       : TZStreamState; { V2.102 current Zlib stream state }
  { FZStreamRec         : TZStreamRec;    V2.102 Zlib stream control record, used for Immediate }
    FModeZStream        : TStream;       { V2.113 compressed data stream, was TMemoryStream, now buffered file }
    FOnZlibProgress     : TZlibProgress; { V2.113 call back event during ZLIB processing }
    FZCompFileName      : String;        { V2.113 zlib file name of compressed file }
    FZlibWorkDir        : String;        { V2.113 zlib work directory }
{$IF DEFINED(UseBandwidthControl) or DEFINED(BUILTIN_THROTTLE)}
    FBandwidthLimit     : Integer;  // Bytes per second
    FBandwidthSampling  : Integer;  // mS sampling interval
{$IFEND}
{$IFDEF UseBandwidthControl}
    FBandwidthCount     : Int64;    // Byte counter
    FBandwidthMaxCount  : Int64;    // Bytes during sampling period
    FBandwidthTimer     : TIcsTimer;
    FBandwidthPaused    : Boolean;
    procedure BandwidthTimerTimer(Sender : TObject);
{$ENDIF}
    procedure SetSslType(const Value: TFtpCliSslType); virtual; { V2.106 }
    procedure SetKeepAliveSecs (secs: integer);
    procedure SetCodePage(const Value: LongWord); { AG 7.02 }
{$IFNDEF NO_DEBUG_LOG}
    function  GetIcsLogger: TIcsLogger;   { 2.104 }
    procedure SetIcsLogger(const Value: TIcsLogger);
    procedure DebugLog(LogOption: TLogOption; const Msg : string); virtual; { 2.104 }
    function  CheckLogOptions(const LogOption: TLogOption): Boolean; virtual; { 2.104 }
{$ENDIF}
    procedure   AbortComponent; override; { V7.15 }
    procedure   SetMultiThreaded(const Value : Boolean); override;
    procedure   SetOnBgException(const Value: TIcsBgExceptionEvent); override; { V7.15 }
    procedure   SetTerminated(const Value: Boolean); override;
    procedure   SetOnMessagePump(const Value: TNotifyEvent); override;
    procedure   SetErrorMessage;
    procedure   LocalStreamWrite(const Buffer; Count : Integer); virtual;
    procedure   LocalStreamWriteString(Str: PAnsiChar; Count: Integer); {$IFDEF COMPILER12_UP} overload;
    procedure   LocalStreamWriteString(Str: PWideChar; Count: Integer; ACodePage: LongWord); overload;
    procedure   LocalStreamWriteString(Str: PWideChar; Count: Integer); overload;
  {$ENDIF}
    procedure   DataSocketGetDataAvailable(Sender: TObject; ErrCode : word);
    procedure   DataSocketGetSessionConnected(Sender: TObject; ErrCode : word);
    procedure   DataSocketPutSessionConnected(Sender: TObject; ErrCode : word);
    procedure   DataSocketGetSessionAvailable(Sender: TObject; ErrCode : word);
    procedure   DataSocketGetSessionClosed(Sender: TObject; ErrCode : word);
    procedure   DataSocketPutDataAvailable(Sender: TObject; ErrCode : word);
    procedure   DataSocketPutDataSent(Sender: TObject; ErrCode : word);
    procedure   DataSocketPutSessionAvailable(Sender: TObject; ErrCode : word);
    procedure   DataSocketPutSessionClosed(Sender: TObject; ErrCode : word);
    procedure   SendCommand(Cmd : String); virtual;
    procedure   TriggerDisplay(Msg : String); virtual;
    procedure   TriggerReadyToTransmit(var bCancel : Boolean); virtual;
    procedure   TriggerDisplayFile(Msg : String); virtual;
    procedure   TriggerError(Msg: String); virtual;
    procedure   TriggerResponse; virtual;   { V7.08 }
    procedure   DisplayLastResponse;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    function    Progress : Boolean; virtual;
    procedure   ControlSocketDnsLookupDone(Sender: TObject; ErrCode: Word);
    procedure   ControlSocketSessionConnected(Sender: TObject; ErrCode: Word); virtual;
    procedure   ControlSocketDataAvailable(Sender: TObject; ErrCode: Word);
    procedure   ControlSocketSessionClosed(Sender: TObject; ErrCode: Word);
    procedure   DataSocketPutAppendInit(const TargetPort, TargetIP : String); virtual;
    procedure   DataSocketGetInit(const TargetPort, TargetIP : String); virtual;
    procedure   TriggerRequestDone(ErrCode: Word);
    procedure   TriggerStateChange;
    procedure   StateChange(NewState : TFtpState);
    procedure   PortAsync; virtual;
    procedure   DoneQuitAsync;
    procedure   ExecAsync(RqType      : TFtpRequest;
                          Cmd         : String;
                          OkResponses : array of Word;
                          DoneAsync   : TFtpNextProc);
    procedure   NextExecAsync;
    procedure   DoGetAsync(RqType : TFtpRequest);
    procedure   Next1GetAsync;
    procedure   Next2GetAsync;
    procedure   Next3GetAsync;
    procedure   Next1PutAsync;
    procedure   Next2PutAsync;
    procedure   Next3PutAsync;
    procedure   DoHighLevelAsync;
    procedure   DoPutAppendAsync;
    procedure   HighLevelAsync(RqType : TFtpRequest; Fcts : TFtpFctSet);
    procedure   HandleError(const Msg : String);
    function    CheckReady : Boolean;
    procedure   TransfertStats; virtual;
    procedure   ExtractMoreResults;
    procedure   SetBinary(Value: Boolean);
    function    GetBinary: Boolean;
    function    GetConnected: Boolean;
    procedure   SetShareMode(newValue: TFtpShareMode);
    function    GetShareMode: TFtpShareMode;
    procedure   SetDisplayFileMode(NewValue: TFtpDisplayFileMode);
    function    GetDisplayFileMode: TFtpDisplayFileMode;
    procedure   SetPassive(NewValue: Boolean);
    procedure   AllocateMsgHandlers; override;
    procedure   FreeMsgHandlers; override;
    function    MsgHandlersCount: Integer; override;
    procedure   WndProc(var MsgRec: TMessage); override;
    procedure   WMFtpRequestDone(var msg: TMessage); virtual;
    procedure   WMFtpSendData(var msg: TMessage); virtual;
    procedure   WMFtpCloseDown(var msg: TMessage); virtual;
    procedure   DestroyLocalStream;
    procedure   SetLocalStream (Stream:TStream);
    procedure   SetLocalFileName (FileName:String);
    procedure   SetDataPortRangeStart (NewValue: LongWord); {JT}
    procedure   SetDataPortRangeEnd (NewValue: LongWord); {JT}
    procedure   RestAsyncGetResumePos; virtual;
    function    OpenFileStream (const FileName: string; Mode: Word): TStream;  { V2.113 }
      {$IFDEF USE_INILE} inline; {$ENDIF}
    procedure   CreateLocalFileStream;         { V2.113 }
    function    CreateSocket: TWSocket; virtual;   { V7.08 }
    property    SocketFamily: TSocketFamily read FSocketFamily write FSocketFamily;
    procedure   HandleHttpTunnelError(Sender: TObject; ErrCode: Word;
        TunnelServerAuthTypes: THttpTunnelServerAuthTypes; const Msg: String);
    procedure   HandleSocksError(Sender: TObject; ErrCode: Integer; Msg: String);
    procedure   SetDSocketSndBufSize(const Value: Integer);{AG V7.26}
    procedure   SetDSocketRcvBufSize(const Value: Integer);{AG V7.26}
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   OpenAsync;       virtual;
    procedure   UserAsync;       virtual;
    procedure   PassAsync;       virtual;
    procedure   AcctAsync;       virtual;
    procedure   ConnectAsync;    virtual;
    procedure   QuitAsync;       virtual;
    procedure   AbortAsync;      virtual;
    procedure   GetAsync;        virtual;
    procedure   ExecGetAsync;    virtual;
    procedure   ReceiveAsync;    virtual;
    procedure   PutAsync;        virtual;
    procedure   ExecPutAsync;    virtual;
    procedure   TransmitAsync;   virtual;
    procedure   AppendAsync;     virtual;
    procedure   ExecAppendAsync; virtual;
    procedure   AppendFileAsync; virtual;
    procedure   ExecDirAsync;    virtual;
    procedure   DirAsync;        virtual;
    procedure   ExecLsAsync;     virtual;
    procedure   LsAsync;         virtual;
    procedure   TypeSetAsync;    virtual;
    procedure   TypeBinaryAsync; virtual;
    procedure   TypeAsciiAsync;  virtual;
    procedure   PwdAsync;        virtual;
    procedure   CwdAsync;        virtual;
    procedure   CDupAsync;       virtual;
    procedure   DirectoryAsync;  virtual;
    procedure   ListAsync;       virtual;
    procedure   SystAsync;       virtual;
    procedure   SystemAsync;     virtual;
    procedure   SizeAsync;       virtual;
    procedure   FileSizeAsync;   virtual;
    procedure   MkdAsync;        virtual;
    procedure   MkdirAsync;      virtual;
    procedure   RmdAsync;        virtual;
    procedure   RmdirAsync;      virtual;
    procedure   DeleAsync;       virtual;
    procedure   DeleteAsync;     virtual;
    procedure   RenFromAsync;    virtual;
    procedure   RenToAsync;      virtual;
    procedure   RenAsync;        virtual;
    procedure   RenameAsync;     virtual;
    procedure   QuoteAsync;      virtual;
    procedure   DoQuoteAsync;    virtual;
    procedure   AbortXferAsync;  virtual;
    procedure   RestAsync;       virtual;
    procedure   RestGetAsync;    virtual;
    procedure   RestartGetAsync; virtual;
    procedure   RestPutAsync;    virtual;
    procedure   RestartPutAsync; virtual;
    procedure   ExecMlsdAsync;   virtual;    { V2.90 }
    procedure   MlsdAsync;       virtual;    { V2.90  machine list directory     }
    procedure   MlstAsync;       virtual;    { V2.90  machine list file          }
    procedure   FeatAsync;       virtual;    { V2.90  supported extensions       }
    procedure   MdtmAsync;       virtual;    { V2.90  get file modification time }
    procedure   MdtmyyAsync;     virtual;    { V2.90  set file modification time }
    procedure   AuthAsync;       virtual;    { V2.106 }
    procedure   ProtAsync;       virtual;
    procedure   PbszAsync;       virtual;
    procedure   CccAsync;        virtual;    { V2.106 }
    procedure   AuthenticateSslAsync; virtual;
    procedure   MfmtAsync;       virtual;    { V2.94  modify file modification time }
    procedure   Md5Async;        virtual;    { V2.94  md5 hash sum  }
    procedure   XMd5Async;       virtual;    { V2.113  md5 hash sum  }
    procedure   ModeZAsync;      virtual;    { V2.102  Mode Z  }
    procedure   OptsAsync;       virtual;    { V2.102  Opts, set Mode Z options }
    procedure   XCrcAsync;       virtual;    { V2.107  crc32 hash sum  }
    procedure   ClntAsync;       virtual;    { V2.113  client string  }
    procedure   AlloAsync;       virtual;    { V2.113  check free allocation  }
    procedure   CombAsync;       virtual;    { V2.113  combine files  }
    procedure   SitePaswdAsync;  virtual;    { V2.113  change password  }
    procedure   SiteExecAsync;   virtual;    { V2.113  run program  }
    procedure   SiteIndexAsync;  virtual;    { V2.113  list files and dirs recursively  }
    procedure   SiteZoneAsync;   virtual;    { V2.113  get time zone difference }
    procedure   SiteMsgAsync;    virtual;    { V2.113  send message }
    procedure   SiteCmlsdAsync;  virtual;    { V2.113  extended MLSD using control channel }
    procedure   SiteDmlsdAsync;  virtual;    { V2.113  extended MLSD using data channel }
    procedure   ExecSiteDmlsdAsync; virtual; { V2.113  internal use }
    procedure   ExecXDmlsdAsync; virtual;    { V7.01   internal use }
    procedure   ConnectHostAsync; virtual;   { V6.09   same as Connect but also sends Host  }
    procedure   ReinAsync;       virtual;    { V6.09   re-initialize control connection  }
    procedure   HostAsync;       virtual;    { V6.09   domain/hostname, usually sent before logon  }
    procedure   LangAsync;       virtual;    { V7.01   set language for messages }
    procedure   XCmlsdAsync;     virtual;    { V7.01   extended MLSD using control channel }
    procedure   XDmlsdAsync;     virtual;    { V7.01   extended MLSD using data channel }
    procedure   ConnectFeatAsync; virtual;   { V7.09   same as Connect but also sends Feat  }
    procedure   ConnectFeatHostAsync; virtual;   { V7.09   same as Connect but also sends Feat and Host  }

    property    CodePage          : LongWord             read  FCodePage
                                                         write SetCodePage;
    property    LastResponse      : String               read  FLastResponse;
    property    LastMultiResponse : String               read  FLastMultiResponse;  { V2.90  multiple lines }
    property    ErrorMessage      : String               read  FErrorMessage;
    property    DnsResult         : String               read  FDnsResult;
    property    DirResult         : String               read  FDirResult;
    property    ControlSocket     : TWSocket             read  FControlSocket;
    property    DataSocket        : TWSocket             read  FDataSocket;
    property    Connected         : Boolean              read  GetConnected;
    property    StatusCode        : LongInt              read  FStatusCode;
    property    State             : TFtpState            read  FState;
    property    RequestType       : TFtpRequest          read  FRequestType;
    property    MLSTFacts         : String               read  FMLSTFacts;     { V2.90 specific new list stuff supported }
    property    RemFacts          : String               read  FRemFacts;      { V2.90 facts about remote file           }
    property    SupportedExtensions : TFtpExtensions     read  FSupportedExtensions;   { V2.94  which supported features }
    property    RemFileDT         : TDateTime            read  FRemFileDT      { V2.90 date/time for MdtmAsync           }
                                                         write FRemFileDT;     {       and MdtmYYYYAsync;                }
    property    Md5Result         : String               read  FMd5Result;     { V2.94 MD5 sum                           }
    property    CrcResult         : String               read  FCrcResult;     { V2.107 CRC32                            }
    property    KeepAliveSecs     : Integer              read  FKeepAliveSecs
                                                         write SetKeepAliveSecs; { V2.107 zero means window default }
    property    Options           : TFtpOptions          read  FOptions
                                                         write FOptions;
    property    LocalStream       : TStream              read  FLocalStream
                                                         write SetLocalStream;
    property    OnProgress64      : TFtpProgress64       read  FOnProgress64
                                                         write FOnProgress64;
    property    ByteCount         : TFtpBigInt           read  FByteCount;     { V2.108 }
    property    SizeResult        : TFtpBigInt           read  FSizeResult;    { V2.108 }
    property    ResumeAt          : TFtpBigInt           read  FResumeAt       { V2.108 }
                                                         write FResumeAt;      { V2.108 }
    property    ClientIdStr       : String               read  FClientIdStr
                                                         write FClientIdStr;    { V2.113 string sent for CLNT command }
    property    PosStart          : TFtpBigInt           read  FPosStart
                                                         write FPosStart;       { V2.113 start pos for MD5/CRC }
    property    PosEnd            : TFtpBigInt           read  FPosEnd
                                                         write FPosEnd;         { V2.113 end pos for MD5/CRC }
    property    DurationMsecs     : Integer              read  FDurationMsecs;  { V2.113 last transfer duration in milliseconds for FByteCount }
    property    StartTick         : Integer              read  FStartTime;      { V2.113 when last transfer started, in case it failed }
{$IF DEFINED(UseBandwidthControl) or DEFINED(BUILTIN_THROTTLE)}
    property BandwidthLimit       : Integer              read  FBandwidthLimit    { V2.106 }
                                                         write FBandwidthLimit;
    property BandwidthSampling    : Integer              read  FBandwidthSampling { V2.106 }
                                                         write FBandwidthSampling;
{$IFEND}
    property TransferMode         : TFtpTransMode        read  FTransferMode    { V2.102 }
                                                         write FTransferMode;
    property NewOpts              : string               read  FNewOpts         { V2.102 }
                                                         write FNewOpts;

    property HostName             : String               read  FHostName
                                                         write FHostName;
    property Port                 : String               read  FPort
                                                         write FPort;
    property DataPortRangeStart   : LongWord             read  FDataPortRangeStart
                                                         write SetDataPortRangeStart; {JT}
    property DataPortRangeEnd     : LongWord             read  FDataPortRangeEnd
                                                         write SetDataPortRangeEnd; {JT}
    property ExternalIPv4         : String               read  FExternalIPv4
                                                         write FExternalIPv4; {V8.02}
    property LocalAddr            : String               read  FLocalAddr
                                                         write FLocalAddr; {bb}
    property LocalAddr6           : String               read  FLocalAddr6
                                                         write FLocalAddr6; { V8.01 }
    property UserName             : String               read  FUserName
                                                         write FUserName;
    property PassWord             : String               read  FPassWord
                                                         write FPassWord;
    property HostDirName          : String               read  FHostDirName
                                                         write FHostDirName;
    property HostFileName         : String               read  FHostFileName
                                                         write FHostFileName;
{$IFNDEF NO_DEBUG_LOG}
    property IcsLogger            : TIcsLogger           read  GetIcsLogger  { 2.104 }
                                                         write SetIcsLogger;
{$ENDIF}
    property LocalFileName        : String               read  FLocalFileName
                                                         write SetLocalFileName;
    property DisplayFileFlag      : Boolean              read  FDisplayFileFlag
                                                         write FDisplayFileFlag;
    property Binary               : Boolean              read  GetBinary
                                                         write SetBinary;
    property Passive              : Boolean              read  FPassive
                                                         write SetPassive;
    property ShareMode            : TFtpShareMode        read  GetShareMode
                                                         write SetShareMode;
    property DisplayFileMode      : TFtpDisplayFileMode  read  GetDisplayFileMode
                                                         write SetDisplayFileMode;
    property ConnectionType       : TFtpConnectionType   read  FConnectionType      { V7.00 }
                                                         write FConnectionType;
    property ProxyServer          : String               read  FProxyServer
                                                         write FProxyServer;
    property ProxyPort            : String               read  FProxyPort
                                                         write FProxyPort;
    property HttpTunnelAuthType   : THttpTunnelAuthType
                                                         read  FHttpTunnelAuthType  { V7.18 }
                                                         write FHttpTunnelAuthType
                                                         default htatDetect;
    property HttpTunnelPassword   : String               read  FHttpTunnelPassword  { V7.18 }
                                                         write FHttpTunnelPassword;
    property HttpTunnelPort       : String               read  FHttpTunnelPort      { V7.18 }
                                                         write FHttpTunnelPort;
    property HttpTunnelServer     : String               read  FHttpTunnelServer    { V7.18 }
                                                         write FHttpTunnelServer;
    property HttpTunnelUserCode   : String               read  FHttpTunnelUserCode  { V7.18 }
                                                         write FHttpTunnelUserCode;

    property SocksPassword        : String               read  FSocksPassword
                                                         write FSocksPassword;      { V7.00 }
    property SocksPort            : String               read  FSocksPort
                                                         write FSocksPort;          { V7.00 }
    property SocksServer          : String               read  FSocksServer
                                                         write FSocksServer;        { V7.001 }
    property SocksUserCode        : String               read  FSocksUserCode
                                                         write FSocksUserCode;      { V7.001 }
    property Account              : String               read  FAccount
                                                         write FAccount;
    property CloseEndSecs         : LongWord             read  FCloseEndSecs        { V2.100 }
                                                         write FCloseEndSecs;
    property Language             : String               read  FLanguage
                                                         write FLanguage;           { V7.01 }
    property LangSupport          : String               read  FLangSupport;        { V7.01 }
    property DataSocketSndBufSize : Integer              read  FDSocketSndBufSize   {AG V7.26}
                                                         write SetDSocketSndBufSize default 8192;
    property DataSocketRcvBufSize : Integer              read  FDSocketRcvBufSize   {AG V7.26}
                                                         write SetDSocketRcvBufSize default 8192;
    property OnDisplay            : TFtpDisplay          read  FOnDisplay
                                                         write FOnDisplay;
    property OnDisplayFile        : TFtpDisplay          read  FOnDisplayFile
                                                         write FOnDisplayFile;
    property OnError              : TFTPDisplay          read  FOnError
                                                         write FOnError;
    property OnCommand            : TFtpCommand          read  FOnCommand
                                                         write FOnCommand;
    property OnResponse           : TNotifyEvent         read  FOnResponse
                                                         write FOnResponse;
    property OnZlibProgress       : TZlibProgress        read  FOnZlibProgress { V2.113 }
                                                         write FOnZlibProgress;
    property ZlibWorkDir          : String               read  FZlibWorkDir    { V2.113 }
                                                         write FZlibWorkDir;
    property OnSessionConnected   : TSessionConnected    read  FOnSessionConnected
                                                         write FOnSessionConnected;
    property OnSessionClosed      : TSessionClosed       read  FOnSessionClosed
                                                         write FOnSessionClosed;
    property OnRequestDone        : TFtpRequestDone      read  FOnRequestDone
                                                         write FOnRequestDone;
    property OnStateChange        : TNotifyEvent         read  FOnStateChange
                                                         write FOnStateChange;
    property OnReadyToTransmit    : TFtpReadyToTransmit  read  FOnReadyToTransmit
                                                         write FOnReadyToTransmit;
    property OnBgException;   { V7.15 }
  end;

  TFtpClient = class(TCustomFtpCli)
  protected
    FTimeout       : Integer;                 { Given in seconds }
    FTimeStop      : LongInt;                 { Milli-seconds    }
    function    Progress : Boolean; override;
    function    Synchronize(Proc : TFtpNextProc) : Boolean; virtual;
    function    WaitUntilReady : Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function    Open       : Boolean;
    function    User       : Boolean;
    function    Pass       : Boolean;
    function    acct       : Boolean;
    function    Connect    : Boolean;
    function    Cwd        : Boolean;
    function    Pwd        : Boolean;
    function    CDup       : Boolean;
    function    TypeSet    : Boolean;
    function    TypeBinary : Boolean;
    function    TypeAscii  : Boolean;
    function    Get        : Boolean;
    function    Put        : Boolean;
    function    RestPut    : Boolean;
    function    RestartPut : Boolean;
    function    Quit       : Boolean;
    function    Abort      : Boolean;
    function    Receive    : Boolean;
    function    Transmit   : Boolean;
    function    Append     : Boolean;
    function    AppendFile : Boolean;
    function    Dir        : Boolean;
    function    Directory  : Boolean;
    function    Ls         : Boolean;
    function    List       : Boolean;
    function    Mkd        : Boolean;
    function    Mkdir      : Boolean;
    function    Ren        : Boolean;
    function    Rename     : Boolean;
    function    Dele       : Boolean;
    function    Delete     : Boolean;
    function    Rmd        : Boolean;
    function    Rmdir      : Boolean;
    function    Syst       : Boolean;
    function    System     : Boolean;
    function    Size       : Boolean;
    function    FileSize   : Boolean;
    function    Quote      : Boolean;
    function    DoQuote    : Boolean;
    function    AbortXfer  : Boolean;
    function    RestGet    : Boolean;
    function    RestartGet : Boolean;
    function    Mlsd       : Boolean;    { V2.90 machine list directory     }
    function    Mlst       : Boolean;    { V2.90 machine list file          }
    function    Feat       : Boolean;    { V2.90 supported extensions       }
    function    Mdtm       : Boolean;    { V2.90 get file modification time }
    function    Mdtmyy     : Boolean;    { V2.90 set file modification time }
    function    Auth       : Boolean;    { V2.106 }
    function    Ccc        : Boolean;    { V2.106 }
    function    Prot       : Boolean;
    function    Pbsz       : Boolean;
    function    AuthenticateSsl    : Boolean;
    function    Mfmt       : Boolean;    { V2.94 modify file modification time }
    function    Md5        : Boolean;    { V2.94 get MD5 hash sum  }
    function    XMd5       : Boolean;    { V2.113 get MD5 hash sum  }
    function    ModeZ      : Boolean;    { V2.102 set mode z }
    function    Opts       : Boolean;    { V2.102 set mode z options }
    function    XCrc       : Boolean;    { V2.107 get CRC32 }
    function    Clnt       : Boolean;    { V2.113  client string  }
    function    Allo       : Boolean;    { V2.113  check free allocation  }
    function    Comb       : Boolean;    { V2.113  combine files  }
    function    SitePaswd  : Boolean;    { V2.113  change password }
    function    SiteExec   : Boolean;    { V2.113  run program  }
    function    SiteIndex  : Boolean;    { V2.113  list files and dirs recursively  }
    function    SiteZone   : Boolean;    { V2.113  get time zone difference }
    function    SiteMsg    : Boolean;    { V2.113  send message }
    function    SiteCmlsd  : Boolean;    { V2.113  extended MLSD using control channel }
    function    SiteDmlsd  : Boolean;    { V2.113  extended MLSD using data channel }
    function    ConnectHost : Boolean;   { V6.09   same as connect but sends Host  }
    function    Rein       : Boolean;    { V6.09   re-initialize control connection  }
    function    Host       : Boolean;    { V6.09   domain/hostname, usually sent before logon  }
    function    Lang       : Boolean;    { V7.01   set language for messages }
    function    XCmlsd     : Boolean;    { V7.01   extended MLSD using control channel }
    function    XDmlsd     : Boolean;    { V7.01   extended MLSD using data channel }
    function    ConnectFeat : Boolean;   { V7.09   same as connect but sends Feat  }
    function    ConnectFeatHost : Boolean;   { V7.09   same as connect but sends Feat and Host  }
  published
    property Timeout       : Integer read FTimeout       write FTimeout;
    property MultiThreaded;
    property HostName;
    property Port;
    property CodePage;
    property DataPortRangeStart; {JT}
    property DataPortRangeEnd; {JT}
    property ExternalIPv4; {V8.02}
    property LocalAddr; {bb}
    property LocalAddr6; { V8.01 }
    property UserName;
    property PassWord;
    property HostDirName;
    property HostFileName;
    property LocalFileName;
    property DisplayFileFlag;
    property Binary;
    property ErrorMessage;
    property ShareMode;
    property Options;
    property ConnectionType;
    property ProxyServer;
    property ProxyPort;   { V7.17 }
    property SocksPassword;
    property SocksPort;
    property SocksServer;
    property SocksUserCode;
    property HttpTunnelAuthType;
    property HttpTunnelPassword;
    property HttpTunnelPort;
    property HttpTunnelServer;
    property HttpTunnelUserCode;
    property Account;
    property Language;
    property DataSocketSndBufSize;                               {AG V7.26}
    property OnDisplay;
    property OnDisplayFile;
    property OnCommand;
    property OnError;
    property OnResponse;
    property OnProgress64;                                       { V2.108 }
    property OnSessionConnected;
    property OnSessionClosed;
    property OnRequestDone;
    property OnStateChange;
    property OnReadyToTransmit;
    property OnBgException;
{$IFNDEF NO_DEBUG_LOG}
    property IcsLogger;                                             { 2.104 }
{$ENDIF}
{$IF DEFINED(UseBandwidthControl) or DEFINED(BUILTIN_THROTTLE)}
    property BandwidthLimit;                                       { V2.106 }
    property BandwidthSampling;                                    { V2.106 }
{$IFEND}
    property SocketFamily;
  end;

{ You must define USE_SSL so that SSL code is included in the component.   }
{ Either in OverbyteIcsDefs.inc or in the project/package options.         }
{$IFDEF USE_SSL}
{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Description:  A component adding SSL support to TFtpCli (RFC-2228).
              Requires OpenSSL (http://www.openssl.org).
              More details in ReadMeIcsSsl.txt and IcsSslHowTo.txt.
              SSL demo applications can be found in /Delphi/SslInternet.
              If you use Delphi 7 and later, you may want to disable warnings
              for unsafe type, unsafe code and unsafe typecast in the project
              options. Those warning are intended for .NET programs. You may
              also want to turn off deprecated symbol and platform symbol
              warnings.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF COMPILER7_UP}
    Bomb('This unit requires Delphi 7 or later !');
{$ENDIF}
{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{ If you use Delphi 7, you may wants to disable warnings for unsage type,   }
{ unsafe code and unsafe typecast in the project options. Those warning are }
{ intended for .NET programs. You may also want to turn off deprecated      }
{ symbol and platform symbol warnings.                                      }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
type
    TSslFtpClient = class(TFtpClient)
    protected
        FProtLevel              : String;
        FProtLevelSent          : String;
        FProtDataFlag           : Boolean;  { AG V7.03 }
        FRenegInitFlag          : Boolean;
        FPBSZSize               : Integer;
        FOnSslHandshakeDone     : TSslHandshakeDoneEvent;
        FOnSslVerifyPeer        : TSslVerifyPeerEvent;
        FOnSslCliGetSession     : TSslCliGetSession;
        FOnSslCliNewSession     : TSslCliNewSession;
        FOnSslCliCertRequest    : TSslCliCertRequest;
        function  GetSslContext : TSslContext;
        procedure SetSslContext(Value: TSslContext);
        procedure SetSslAcceptableHosts(Value : TStrings);
        function  GetSslAcceptableHosts: TStrings;
        procedure ControlSocketSessionConnected(Sender: TObject; ErrCode: Word); override;
        procedure DataSocketPutAppendInit(const TargetPort, TargetIP : String); override;
        procedure DataSocketGetInit(const TargetPort, TargetIP : String); override;
        procedure SetProtLevel(const Value : String); virtual;
        procedure SetSslType(const Value: TFtpCliSslType); override;
        procedure ControlSocketSslShutDownComplete(Sender     : TObject;
                                                Bidirectional : Boolean;
                                                ErrCode       : Integer); virtual;
        procedure TransferSslHandshakeDone(Sender         : TObject;
                                           ErrCode        : Word;
                                           PeerCert       : TX509Base;
                                           var Disconnect : Boolean); virtual;
        procedure TransferSslVerifyPeer(Sender        : TObject;
                                        var Ok        : Integer;
                                        Cert          : TX509Base); virtual;
        procedure TransferSslCliGetSession(Sender  : TObject;
                                       var SslSession : Pointer;
                                       var FreeSession: Boolean); virtual;
        procedure TransferSslCliNewSession(Sender      : TObject;
                                           SslSession  : Pointer;
                                           WasReused   : Boolean;
                                          var IncRefCount: Boolean); virtual;
        procedure TransferSslCliCertRequest(Sender     : TObject;
                                            var Cert   : TX509Base); virtual;
    public
        constructor Create(AOwner : TComponent); override;
        procedure   SetAcceptableHostsList(const SemiColonSeparatedList : String);
        procedure   OpenAsync;       override;
        procedure   AuthAsync;       override;
        procedure   ProtAsync;       override;
        procedure   PbszAsync;       override;
        procedure   CccAsync;        override;

    published
        property SslContext         : TSslContext         read  GetSslContext
                                                          write SetSslContext;
        property SslType            : TFtpCliSslType      read  FSslType
                                                          write FSslType;
        property SslAcceptableHosts : TStrings            read  GetSslAcceptableHosts
                                                          write SetSslAcceptableHosts;
        property ProtLevel          : String              read  FProtLevel
                                                          write SetProtLevel;
        property PBSZSize           : Integer             read  FPBSZSize
                                                          write FPBSZSize;
        property OnSslVerifyPeer    : TSslVerifyPeerEvent read  FOnSslVerifyPeer
                                                          write FOnSslVerifyPeer;
        property OnSslCliGetSession : TSslCliGetSession
                                                          read  FOnSslCliGetSession
                                                          write FOnSslCliGetSession;
        property OnSslCliNewSession : TSslCliNewSession
                                                          read  FOnSslCliNewSession
                                                          write FOnSslCliNewSession;
        property OnSslHandshakeDone : TSslHandshakeDoneEvent
                                                          read  FOnSslHandshakeDone
                                                          write FOnSslHandshakeDone;
        property OnSslCliCertRequest : TSslCliCertRequest read  FOnSslCliCertRequest
                                                          write FOnSslCliCertRequest;
    end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF} // USE_SSL

function LookupFTPReq (const RqType: TFtpRequest): String;    { V2.113 angus }
function LookupFtpState (const FtpState: TFtpState): String;  { V2.113 angus }

implementation

{$B-}  { Do not evaluate boolean expressions more than necessary }

(*
{$IFNDEF WIN32}
const
    HFILE_ERROR = $FFFF;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF WIN32}
procedure SetLength(var Str : String; Len : Integer);
begin
    Str[0] := chr(Len);
end;
{$ENDIF}
*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LookupFTPReq (const RqType: TFtpRequest): String;    { V2.113 angus }
begin
   case RqType of
      ftpNone: result:='none';
      ftpOpenAsync: result:='OpenAsync';
      ftpUserAsync: result:='UserAsync';
      ftpPassAsync: result:='PassAsync';
      ftpCwdAsync: result:='CwdAsync';
      ftpConnectAsync: result:='ConnectAsync';
      ftpReceiveAsync: result:='ReceiveAsync';
      ftpDirAsync: result:='DirAsync';
      ftpLsAsync: result:='LsAsync';
      ftpPortAsync: result:='PortAsync';
      ftpGetAsync: result:='GetAsync';
      ftpDirectoryAsync: result:='DirectoryAsync';
      ftpListAsync: result:='ListAsync';
      ftpSystemAsync: result:='SystemAsync';
      ftpSystAsync: result:='SystAsync';
      ftpQuitAsync: result:='QuitAsync';
      ftpSizeAsync: result:='SizeAsync';
      ftpPutAsync: result:='PutAsync';
      ftpAppendAsync: result:='AppendAsync';
      ftpFileSizeAsync: result:='FileSizeAsync';
      ftpRqAbort: result:='RqAbort';
      ftpMkdAsync: result:='MkdAsync';
      ftpRmdAsync: result:='RmdAsync';
      ftpRenameAsync: result:='RenameAsync';
      ftpDeleAsync: result:='DeleAsync';
      ftpRenAsync: result:='RenAsync';
      ftpRenToAsync: result:='RenToAsync';
      ftpRenFromAsync: result:='RenFromAsync';
      ftpDeleteAsync: result:='DeleteAsync';
      ftpMkdirAsync: result:='MkdirAsync';
      ftpRmdirAsync: result:='RmdirAsync';
      ftpPwdAsync: result:='PwdAsync';
      ftpQuoteAsync: result:='QuoteAsync';
      ftpCDupAsync: result:='CDupAsync';
      ftpDoQuoteAsync: result:='DoQuoteAsync';
      ftpTransmitAsync: result:='TransmitAsync';
      ftpTypeSetAsync: result:='TypeSetAsync';
      ftpRestAsync: result:='RestAsync';
      ftpRestGetAsync: result:='RestGetAsync';
      ftpRestartGetAsync: result:='RestartGetAsync';
      ftpRestPutAsync: result:='RestPutAsync';
      ftpRestartPutAsync: result:='RestartPutAsync';
      ftpMlsdAsync: result:='MlsdAsync';
      ftpFeatAsync: result:='FeatAsync';
      ftpMlstAsync: result:='MlstAsync';
      ftpMdtmAsync: result:='MdtmAsync';
      ftpMdtmyyAsync: result:='MdtmyyAsync';
      ftpMfmtAsync: result:='MfmtAsync';
      ftpMd5Async: result:='Md5Async';
      ftpAccountAsync: result:='AccountAsync';
      ftpModeZAsync: result:='ModeZAsync';
      ftpOptsAsync: result:='OptsAsync';
      ftpXCrcAsync: result:='XCrcAsync';
      ftpClntAsync: result:='ClntAsync';
      ftpSitePaswdAsync: result:='SitePaswdAsync';
      ftpSiteExecAsync: result:='SiteExecAsync';
      ftpSiteIndexAsync: result:='SiteIndexAsync';
      ftpSiteZoneAsync: result:='SiteZoneAsync';
      ftpSiteMsgAsync: result:='SiteMagAsync';
      ftpSiteCmlsdAsync: result:='SiteCmlsdAsync';
      ftpSiteDmlsdAsync: result:='SiteDmlsdAsync';
      ftpAlloAsync: result:='AlloAsync';
      ftpCombAsync: result:='CombAsync';
      ftpXMd5Async: result:='XMd5Async';
      ftpConnectHostAsync: result:='ConnectHostAsync'; { V7.08 }
      ftpReinAsync: result:='ReinAsync';
      ftpHostAsync: result:='HostAsync';
      ftpLangAsync: result:='LangAsync';
      ftpXCmlsdAsync: result:='XCmlsdAsync';
      ftpXDmlsdAsync: result:='XDmlsdAsync';
      ftpConnectFeatAsync: result:='ConnectFeatAsync'; { V7.09 }
      ftpConnectFeatHostAsync: result:='ConnectFeatHostAsync'; { V7.09 }
{$IFDEF USE_SSL}
      ftpCccAsync: result:='CCCAsync';
      ftpAuthAsync: result:='AuthAsync';
      ftpProtAsync: result:='ProtAsync';
      ftpPbszAsync: result:='PbszAsync';
{$ENDIF}
   else
      result:='unknown';
   end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LookupFtpState (const FtpState: TFtpState): String;  { V2.113 angus }
begin
   case FtpState of
      ftpNotConnected: result := 'Not Connected';
      ftpReady: result := 'Ready';
      ftpInternalReady: result := 'Internal Ready';
      ftpDnsLookup: result := 'DNS Lookup';
      ftpConnected: result := 'Connected';
      ftpAbort: result := 'Abort';
      ftpInternalAbort: result := 'Internal Abort';
      ftpWaitingBanner: result := 'Waiting Banner';
      ftpWaitingResponse: result := 'Waiting Response';
      ftpPasvReady: result := 'PASV Ready';
   else
      result:='unknown';
   end;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetFileSize(FileName : String) : TFtpBigInt; { V2.108 }
var
    SR : TSearchRec;
{$IFDEF MSWINDOWS}
    TempSize: TULargeInteger;  // 64-bit integer record
{$ENDIF}
begin
    if FindFirst(FileName, faReadOnly or faHidden or
                 faSysFile or faArchive, SR) = 0 then begin
      {$IFDEF MSWINDOWS}
        TempSize.LowPart  := SR.FindData.nFileSizeLow;
        TempSize.HighPart := SR.FindData.nFileSizeHigh;
        Result := TempSize.QuadPart;
      {$ENDIF}
      {$IFDEF POSIX}
        Result := SR.Size;
      {$ENDIF}
        FindClose(SR);
    end
    else
        Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetInteger(Data : PChar; var Number : LongInt) : PChar;
var
    bSign : Boolean;
begin
    Number := 0;
    Result := StpBlk(Data);

    if Result = nil then
        Exit;

    { Remember the sign }
    if (Result^ = '-') or (Result^ = '+') then begin
        bSign := (Result^ = '-');
        Inc(Result);
    end
    else
        bSign  := FALSE;

    { Convert any number }
    while (Result^ <> #0) and IsDigit(Result^) do begin
        Number := Number * 10 + ord(Result^) - ord('0');
        Inc(Result);
    end;

    { Correct for sign }
    if bSign then
        Number := -Number;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetInt64(Data : PChar; var Number : Int64) : PChar;   { V2.101 }
var
    bSign : Boolean;
begin
    Number := 0;
    Result := StpBlk(Data);

    if Result = nil then
        Exit;

    { Remember the sign }
    if (Result^ = '-') or (Result^ = '+') then begin
        bSign := (Result^ = '-');
        Inc(Result);
    end
    else
        bSign  := FALSE;

    { Convert any number }
    while (Result^ <> #0) and IsDigit(Result^) do begin
        Number := Number * 10 + ord(Result^) - ord('0');
        Inc(Result);
    end;

    { Correct for sign }
    if bSign then
        Number := -Number;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetQuotedString(Data : PChar; var Dst : String) : PChar;
begin
    Dst := '';
    Result := StpBlk(Data);

    if (Result = nil) then
        Exit;

    if Result^ <> '"' then
        Exit;
    Inc(Result);

    while Result^ <> #0 do begin
        if Result^ <> '"' then
            Dst := Dst + Result^
        else begin
            Inc(Result);
            if Result^ <> '"' then
                Break;
            Dst := Dst + Result^;
        end;
        Inc(Result);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetNextString(Data : PChar; var Dst : String) : PChar;  { V2.94 }
begin
    Dst := '';
    Result := StpBlk(Data);

    if Result = nil then
        Exit;

    while (Result^ <> #0) and (Result^ = #32) do
        Inc(Result);  { skip leading spaces }

    while (Result^ <> #0) and (Result^ <> #32) do begin
        Dst := Dst + Result^;
        Inc(Result);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* *                                                                     * *}
{* *                            TCustomFtpCli                            * *}
{* *                                                                     * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomFtpCli.Create(AOwner: TComponent);
{$IFDEF MSWINDOWS}
var
    Len : Cardinal;
{$ENDIF}
begin
    inherited Create(AOwner);
    AllocateHWnd;
    FOnDisplay          := nil;
    FOnDisplayFile      := nil;
    FType               := 'I';
    FPort               := 'ftp';
    FDataPortRangeStart := 0; {JT}
    FDataPortRangeEnd   := 0; {JT}
    FCloseEndSecs       := 5;       { V2.100 }
    FTransferMode       := FtpTransModeZDeflate ; { V2.102 new tranfer mode }
    FCurrTransMode      := FtpTransModeStream ;   { V2.102 current transfer mode }
    FNewOpts            := 'MODE Z LEVEL 8';       { V2.102 default argument for OPTS command }
    FProxyPort          := 'ftp';
    FState              := ftpReady;
    FShareMode          := fmShareExclusive;
    FConnectionType     := ftpDirect;
    FProxyServer        := '';    { Should Socks properties be set to '' as well? }
    FSocksServer        := '';
    FOptions            := [ftpAcceptLF];
    FLocalAddr          := ICS_ANY_HOST_V4;
    FLocalAddr6         := ICS_ANY_HOST_V6;  { V8.01 }
    FKeepAliveSecs      := 0; {V2.107 for control socket only }
    FClientIdStr        := ftpClientId; {V2.113 string sent for CLNT command }
    FSocketFamily       := DefaultSocketFamily;   { V8.00 IPv6 }
    FControlSocket      := CreateSocket;   { V7.08 was  TWSocket.Create(Self); }
    FControlSocket.ExceptAbortProc    := AbortComponent; { V7.15 }
    FControlSocket.OnSessionConnected := ControlSocketSessionConnected;
    FControlSocket.OnDataAvailable    := ControlSocketDataAvailable;
    FControlSocket.OnSessionClosed    := ControlSocketSessionClosed;
    FControlSocket.OnDnsLookupDone    := ControlSocketDnsLookupDone;
    FDataSocket         := CreateSocket;    { V7.08 was  TWSocket.Create(Self); }
    FDataSocket.ExceptAbortProc       := AbortComponent; { V7.15 }
    FStreamFlag         := FALSE;
{$IFDEF MSWINDOWS}
    SetLength(FZlibWorkDir, 1024);
    Len := GetTempPath(Length(FZlibWorkDir) - 1, PChar(FZlibWorkDir));{ AG V6.03 }
    SetLength(FZlibWorkDir, Len);
{$ENDIF}
{$IFDEF POSIX}
    FZlibWorkDir := TPath.GetTempPath;
{$ENDIF}
    FZlibWorkDir := IncludeTrailingPathDelimiter (FZlibWorkDir);  { V2.113 }
{$IF DEFINED(UseBandwidthControl) or DEFINED(BUILTIN_THROTTLE)}
    FBandwidthLimit     := 10000;  // Bytes per second
    FBandwidthSampling  := 1000;   // mS sampling interval
{$IFEND}
{$IFNDEF NO_DEBUG_LOG}
    __DataSocket := FDataSocket;
{$ENDIF}
{$IFDEF USE_SSL}
    FControlSocket.SslEnable          := FALSE;
{$ENDIF}
    FSystemCodePage := GetACP; { AG V7.02 }
    FCodePage := CP_ACP;
    FLanguage := 'EN';    { V7.01 or EN-uk, FR, etc, only for messages }
    FDSocketSndBufSize := 8192;  {AG V7.26}
    FDSocketRcvBufSize := 8192;  {AG V7.26}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomFtpCli.Destroy;
begin
    DestroyLocalStream;
    FDataSocket.Free;
    FControlSocket.Free;
{$IFDEF UseBandwidthControl}
    if Assigned(FBandwidthTimer) then begin
        FBandwidthTimer.Free;
        FBandwidthTimer := nil;
    end;
{$ENDIF}
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.MsgHandlersCount : Integer;
begin
    Result := 3 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_FTP_REQUEST_DONE := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_FTP_SENDDATA     := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_FTP_CLOSEDOWN    := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_FTP_REQUEST_DONE);
        FWndHandler.UnregisterMessage(FMsg_WM_FTP_SENDDATA);
        FWndHandler.UnregisterMessage(FMsg_WM_FTP_CLOSEDOWN);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.WndProc(var MsgRec: TMessage);
begin
    try
         with MsgRec do begin
             if Msg = FMsg_WM_FTP_REQUEST_DONE then
                 WMFtpRequestDone(MsgRec)
             else if Msg = FMsg_WM_FTP_SENDDATA then
                 WMFtpSendData(MsgRec)
             else if Msg = FMsg_WM_FTP_CLOSEDOWN then
                 WMFtpCloseDown(MsgRec)     { V2.100 }
             else
                 inherited WndProc(MsgRec);
        end;
    except
        on E: Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AbortComponent; { V7.15 }
begin
    try
        AbortAsync;
    except
    end;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.WMFtpRequestDone(var msg: TMessage);
begin
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, FRequestType, Msg.LParam);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if AComponent = FControlSocket then
            FControlSocket := nil
        else if AComponent = FDataSocket then
            FDataSocket := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetDSocketSndBufSize(const Value: Integer);{AG V7.26}
begin
    if Value < 1024 then
        FDSocketSndBufSize := 1024
    else
        FDSocketSndBufSize := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetDSocketRcvBufSize(const Value: Integer);{AG V7.26}
begin
    if Value < 1024 then
        FDSocketRcvBufSize := 1024
    else
        FDSocketRcvBufSize := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetErrorMessage;
begin
    if FErrorMessage = '' then
        FErrorMessage := FLastResponse;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.CreateSocket: TWSocket;   { V7.08 }
begin
  Result := TWSocket.Create(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ZlibOnProgress(
    Sender: TObject;
    Count: Int64;
    var Cancel: Boolean);
var
    MyClient: TCustomFtpCli;
begin
    MyClient := Sender as TCustomFtpCli;
    if Assigned (MyClient.FOnZlibProgress) then begin
        MyClient.FOnZlibProgress (Sender, Count, Cancel);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DestroyLocalStream;
var
    NewSize: Int64;
begin
    if FCurrTransMode = ftpTransModeZDeflate then begin    { V1.103 }
        if FZStreamState > ftpZStateNone then begin
            if (FZStreamState = ftpZStateSaveDecom) and (FModeZStream.Size > 0) and
                  Assigned (FModeZStream) and Assigned(FLocalStream) then begin   { V1.113 more sanity checks }
                try
                    FZStreamState := ftpZStateNone;
                    FModeZStream.Position := 0;
                    NewSize := FLocalStream.Size ;
                    ZlibDecompressStreamEx (FModeZStream, FLocalStream,
                                             Self, ZlibOnProgress);  { V1.113 added callback }
                    NewSize := FLocalStream.Size - NewSize ;
                    TriggerDisplay('! Download size, compressed ' + IntToKByte
                            (FModeZStream.Size) + ' bytes, Uncompressed ' +
                                               IntToKByte (NewSize) + 'bytes') ;
                except
                    on E:Exception do begin
                        HandleError('Failed to Decompress Stream - ' + E.Message);
                    end;
                end;
            end;
            FZStreamState := ftpZStateNone;
            if Assigned (FModeZStream) then FModeZStream.Destroy;
            FModeZStream := nil;
            try
                if FileExists(FZCompFileName) then DeleteFile (FZCompFileName);  { V1.113 }
            except
            end;
        end;
    end;
    if Assigned(FLocalStream) and (FStreamFlag = FALSE) then begin
        FLocalStream.Free;
        FLocalStream := nil;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.OpenFileStream (const FileName: string; Mode: Word): TStream;  { V2.113 }
begin
    //Result := TIcsBufferedStream.Create(FileName, Mode, MAX_BUFSIZE);
    Result := TBufferedFileStream.Create(FileName, Mode, MAX_BUFSIZE);
    { Buffered stream makes sense with small block sizes only }
    //Result := TFileStream.Create(FileName, Mode);
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.CreateLocalFileStream;         { V2.113 }
begin
    try
        if NOT FStreamFlag then begin
            FreeAndNil(FLocalStream);
            FLocalStream := OpenFileStream(FLocalFileName, fmCreate);
            if FShareMode <> 0 then begin
                { Not default mode, need to close and reopen file with }
                { the given mode                                       }
                FreeAndNil(FLocalStream);
                FLocalStream := OpenFileStream(FLocalFileName, fmOpenWrite + FShareMode);
            end;
        end;
    except
        on E:Exception do begin
            FLastResponse := 'Unable to open local file ' +
                                           FLocalFileName + ': ' + E.Message;
            FStatusCode   := 550;
            SetErrorMessage;
            FRequestResult := FStatusCode;
            TriggerRequestDone(FRequestResult);
            exit;
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
procedure TCustomFtpCli.LocalStreamWriteString(Str: PWideChar; Count: Integer;
    ACodePage: LongWord);
begin
    StreamWriteString(FLocalStream, Str, Count, ACodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.LocalStreamWriteString(Str: PWideChar; Count: Integer);
begin
    StreamWriteString(FLocalStream, Str, Count, CP_ACP);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.LocalStreamWriteString(Str: PAnsiChar; Count : Integer);
begin
    FLocalStream.WriteBuffer(Str^, Count);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.LocalStreamWrite(const Buffer; Count : Integer);
begin
    FLocalStream.WriteBuffer(Buffer, Count);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetKeepAliveSecs (secs: integer);
begin
    if FKeepAliveSecs <> secs then begin
        if secs = 0 then
            FControlSocket.KeepAliveOnOff := wsKeepAliveOnSystem
        else begin
            FControlSocket.KeepAliveOnOff := wsKeepAliveOnCustom ;
            FControlSocket.KeepAliveTime := LongWord (secs) * 1000;    { how often }
            if secs < 10 then
                FControlSocket.KeepAliveInterval := 1000
            else
                FControlSocket.KeepAliveInterval := LongWord (secs div 5) * 1000;  { repeat if failed }
        end ;
    end;
    FKeepAliveSecs := secs;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetCodePage(const Value: LongWord); { AG 7.02 }
begin
    if Value = FSystemCodePage then
        FCodePage := CP_ACP
    else
    {$IFDEF COMPILER12_UP}
        FCodePage := Value;
    {$ELSE}
        if Value = CP_UTF8 then
            FCodePage := Value
        else
            FCodePage := CP_ACP;
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetLocalFileName(FileName: String);
begin
    FLocalFileName := FileName;
    if Length(FileName) > 0 then
        FStreamFlag := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetLocalStream(Stream: TStream);
begin
    FLocalStream := Stream;
    FStreamFlag  := (Stream <> nil);
    if FStreamFlag then
        SetLength(FLocalFileName, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetDataPortRangeStart(NewValue: LongWord); {JT}
begin
    if NewValue > 65535 then
        HandleError('DataPortRangeStart must be in the range 0..65535')
    else
        FDataPortRangeStart := NewValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetDataPortRangeEnd(NewValue: LongWord); {JT}
begin
    if NewValue > 65535 then
        HandleError('DataPortRangeEnd must be in the range 0..65535')
    else
        FDataPortRangeEnd := NewValue
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TriggerDisplay(Msg : String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TriggerDisplayFile(Msg : String);
begin
    if Assigned(FOnDisplayFile) then
        FOnDisplayFile(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TriggerError(Msg : String);
begin
    if Assigned(FOnError) then
        FOnError(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DisplayLastResponse;
begin
    if Pos('Will attempt to restart', FLastResponse) > 0 then
        TriggerDisplay('< DEBUG !');

    TriggerDisplay('< ' + FLastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetMultiThreaded(const Value : Boolean);
begin
    if Assigned(FDataSocket) then
        FDataSocket.MultiThreaded := Value;
    if Assigned(FControlSocket) then
        FControlSocket.MultiThreaded := Value;
    inherited SetMultiThreaded(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetTerminated(const Value: Boolean);
begin
    if Assigned(FDataSocket) then
        FDataSocket.Terminated := Value;
    if Assigned(FControlSocket) then
        FControlSocket.Terminated := Value;
    inherited SetTerminated(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetOnBgException(const Value: TIcsBgExceptionEvent); { V7.15 }
begin
    if Assigned(FDataSocket) then
        FDataSocket.OnBgException := Value;
    if Assigned(FControlSocket) then
        FControlSocket.OnBgException := Value;
    inherited SetOnBgException(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetOnMessagePump(const Value: TNotifyEvent);
begin
    if Assigned(FDataSocket) then
        FDataSocket.OnMessagePump := Value;
    if Assigned(FControlSocket) then
        FControlSocket.OnMessagePump := Value;
    inherited SetOnMessagePump(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.StateChange(NewState : TFtpState);
begin
    if FState <> NewState then begin
        FState := NewState;
        TriggerStateChange;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetBinary : Boolean;
begin
     Result := (FType = 'I');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetBinary(Value : Boolean);
begin
     if Value then
         FType := 'I'
     else
         FType := 'A';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.Progress : Boolean;
var
    Abort : Boolean;
begin
    Abort := FALSE;
    if Assigned(FOnProgress64) then   { V2.101 }
        FOnProgress64(Self, FByteCount + FResumeAt, Abort);
    if Abort then begin
     //   TriggerDisplay('! Abort requested');
     //   FDataSocket.Close;
        AbortAsync ; // Angus do it properly 
    end;

    Result := not Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SendCommand(Cmd : String);
var
    RawCmd: AnsiString; { AG V7.02 We do not need RawByteString here }
begin
    if Assigned(FOnCommand) then
        FOnCommand(Self, Cmd);
    TriggerDisplay('> ' + Cmd);
{$IFDEF COMPILER12_UP}
    RawCmd := UnicodeToAnsi(Cmd, FCodePage);             { V7.01 }
{$ELSE}
    RawCmd := ConvertCodepage(Cmd, CP_ACP, FCodePage);   { V7.01 }
{$ENDIF}
    if FControlSocket.State = wsConnected then
        FControlSocket.SendStr(RawCmd + #13#10)  { V7.01 }
    { Quit when not connected never returned. }                { 01/14/06 AG}
    else begin
        if cmd = 'QUIT' then
            FStatusCode := 200
        else
            FStatusCode := 550;

         FNextRequest   := nil;
         FDoneAsync     := nil;
         FConnected     := FALSE;
         FRequestResult := FStatusCode;
         FLastResponse  := IntToStr(FStatusCode) + ' not connected';
         if FStatusCode = 550 then begin
            SetErrorMessage;
            TriggerRequestDone(550);
         end
         else
            TriggerRequestDone(0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.HandleError(const Msg : String);
begin
    { Reset some defaults }                                         { V2.106 }
    FFctSet           := [];
    FFctPrv           := ftpFctNone;
    FLastResponse     := '';
    FErrorMessage     := '';
    FNextRequest      := nil;
    if Assigned(FOnError) then
        TriggerError(Msg)
    else
        raise FtpException.Create(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* Return TRUE if component is ready for next operation.                    }
{* Trigger an error or return FALSE if not ready                            }
function TCustomFtpCli.CheckReady : Boolean;
begin
    Result := (FState in [ftpReady, ftpInternalReady, ftpPasvReady]);
    if not Result then
        HandleError('FTP component not ready, state ' + LookupFtpState (FState));  { V8.03 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.OpenAsync;
begin
    if not CheckReady then begin
        TriggerDisplay('Not ready for Open');  { V8.03 }
        Exit;
    end;
    if FConnected then begin
        HandleError('FTP component already connected');
        Exit;
    end;

    if not FHighLevelFlag then
        FRequestType := ftpOpenAsync;

    FRequestDoneFlag     := FALSE;
    FReceiveLen          := 0;
    FRequestResult       := 0;
    FDnsResult           := '';
    FMLSTFacts           := '';   { V2.90 supported MLST facts }
    FSupportedExtensions := [];   { V2.94 supported extensions }
    FLangSupport         := '';   { V7.01 supported languages }
    FControlSocket.SocketFamily := FSocketFamily;
    FLastResponse        := '';
    FErrorMessage        := '';
    FStatusCode          := 0;

{ angus V7.00 always set proxy and SOCKS options before opening socket  }
    FControlSocket.SocksAuthentication := socksNoAuthentication;

    if FConnectionType <> ftpHttpProxy then begin
        { Disable connection thru HTTP proxy. It's not done in   }
        { in the component because the last successful AuthType  }
        { is cached to avoid slow detection. Changing property   }
        { HttpTunnelServer clears the cache.                     }
        FControlSocket.HttpTunnelServer := '';
        FDataSocket.HttpTunnelServer    := '';
    end;

    case FConnectionType of
       // ftpProxy:   FPassive := TRUE;     { V7.22 } 
        ftpSocks4:  FControlSocket.SocksLevel := '4';
        ftpSocks4A: FControlSocket.SocksLevel := '4A';
        ftpSocks5:  FControlSocket.SocksLevel := '5';
        ftpHttpProxy : { V7.18 }
            begin
                FPassive := TRUE;
                FControlSocket.HttpTunnelAuthType := HttpTunnelAuthType;
                FControlSocket.HttpTunnelServer   := FHttpTunnelServer;
                FControlSocket.HttpTunnelPort     := FHttpTunnelPort;
                FControlSocket.HttpTunnelUsercode := FHttpTunnelUserCode;
                FControlSocket.HttpTunnelPassword := FHttpTunnelPassword;
                FControlSocket.OnHttpTunnelError  := HandleHttpTunnelError;
            end;
    end;
    if FConnectionType in [ftpSocks4, ftpSocks4A, ftpSocks5] then begin
        FPassive := TRUE;
        FControlSocket.SocksAuthentication  := socksAuthenticateUsercode;
        FControlSocket.SocksServer          := FSocksServer;
        FControlSocket.SocksPort            := FSocksPort;
        FControlSocket.SocksUsercode        := FSocksUsercode;
        FControlSocket.SocksPassword        := FSocksPassword;
        FControlSocket.OnSocksError         := HandleSocksError;
    end;
    StateChange(ftpDnsLookup);
    case FConnectionType of
        ftpDirect, ftpSocks4, ftpSocks4A, ftpSocks5, ftpHttpProxy: { V7.18 }
            begin
                FControlSocket.Addr := FHostName;
                FControlSocket.DnsLookup(FHostName);
            end;
        ftpProxy:
            begin
                FControlSocket.Addr := FProxyServer;
                FControlSocket.DnsLookup(FProxyServer);
            end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecAsync(
    RqType      : TFtpRequest;
    Cmd         : String;         { Command to execute                      }
    OkResponses : array of Word;  { List of responses like '200 221 342'    }
    DoneAsync   : TFtpNextProc);  { What to do when done                    }
var
    I : Integer;
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, 'Start command, Req=' + LookupFTPReq (RqType) + ' - '  + Cmd);  { V8.03 }
{$ENDIF}
    if not((Cmd = 'ABOR') or (Cmd = 'STAT') or (Cmd = 'QUIT')) then begin
        if not CheckReady then begin
            TriggerDisplay('Not ready for next command, Req=' + LookupFTPReq (RqType) + ' - '  + Cmd);  { V8.03 }
            Exit;
        end;
        if not FConnected then begin
            HandleError('FTP component not connected');
            Exit;
        end;
    end;

    if not FHighLevelFlag then
        FRequestType := RqType;

    for I := 0 to High(OkResponses) do
        FOkResponses[I] := OkResponses[I];
    FOkResponses[High(OkResponses) + 1] := 0;

    { V2.90 some FTP responses are multiline, welcome banner, FEAT command, }
    { keep them all                                                         }
    FLastMultiResponse := '';
    FRequestDoneFlag   := FALSE;
    FNext              := NextExecAsync;
    FDoneAsync         := DoneAsync;
    FErrormessage      := '';
    StateChange(ftpWaitingResponse);
    SendCommand(Cmd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExtractMoreResults;
var
    NumericCode : LongInt;
    p           : PChar;
    S           : String;
begin
    if FRequestResult = 0 then begin
        if FFctPrv in [ftpFctSize] then begin
            p := GetInteger(@FLastResponse[1], NumericCode);
            GetInt64(p, FSizeResult);     { V2.101 }
        end;
        if FFctPrv in [ftpFctMdtm] then begin  { V2.90 get file modification time }
            p := GetInteger(@FLastResponse[1], NumericCode);
            if NumericCode = 213 then begin
               GetNextString (p, S);        { V2.94 }
               FRemFileDT := MDTM2Date(S);  { UTC time }
            end
            else
                FRemFileDT := -1;
        end;
        if FFctPrv in [ftpFctCDup, ftpFctPwd, ftpFctMkd, ftpFctCwd] then begin
            p := GetInteger(@FLastResponse[1], NumericCode);
            GetQuotedString(p, FDirResult);
        end;
        if FFctPrv in [ftpFctMd5] then begin  { V2.94 get MD5 hash sum }
            FDirResult := '';
            FMd5Result := '';
            p := GetInteger(@FLastResponse[1], NumericCode);
          { MD5 response may be 251 "filename" 8D75F2E65DF7358D7A4001E658CF6001 or 251 filename 8D75F2E65DF7358D7A4001E658CF6001 }
            if NumericCode = 251 then begin
                p := GetQuotedString(p, FDirResult);
                if FDirResult = '' then p := GetNextString(p, FDirResult);
                GetNextString(p, FMd5Result);
            end;
          { XMD5 response may be 220 8D75F2E65DF7358D7A4001E658CF6001 or  250 8D75F2E65DF7358D7A4001E658CF6001 }
            if (NumericCode = 220) or (NumericCode = 250) then begin  { V7.06 either }
                GetNextString(p, FMd5Result);
            end;
        end;
        if FFctPrv in [ftpFctXCrc] then begin  { V2.107 get CRC32 hash sum }
            FCrcResult := '';
            p := GetInteger(@FLastResponse[1], NumericCode);
         { response may be 250 XCRC 12345678 or 250 12345678  }
            if NumericCode = 250 then begin
                p := GetNextString(p, FCrcResult);
                if FCrcResult = 'XCRC' then GetNextString(p, FCrcResult);
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.NextExecAsync;
var
    I : Integer;
    p : PChar;
begin
    DisplayLastResponse;

    if not IsDigit(FLastResponse[1]) then
        Exit; { Continuation line, nothing to do }
    p := GetInteger(@FLastResponse[1], FStatusCode);
    if p^ = '-' then
        Exit; { Continuation line, nothing to do }

    if FOkResponses[0] = 0 then begin
        { The list of ok responses is empty }
        if FStatusCode >= 500 then begin
            { Not a good response }
            FRequestResult := FStatusCode;
            SetErrorMessage;
        end
        else
            FRequestResult := 0;
    end
    else begin
        { We have a list of ok response codes }
        for I := 0 to High(FOkResponses) do begin
            if FOkResponses[I] = 0 then begin
                { No good response found }
                FRequestResult := FStatusCode;
                SetErrorMessage;
                break;
            end;
            if FOkResponses[I] = FStatusCode then begin
                { Good response found }
                FRequestResult := 0;
                Break;
            end;
        end;
    end;

    if FPassive and
      (((FControlSocket.CurrentSocketFamily = sfIPv4) and (FStatusCode = 227)) or
       ((FControlSocket.CurrentSocketFamily = sfIPv6) and (FStatusCode = 229))) then
    begin    
        StateChange(ftpPasvReady);               { 19.09.2002 }
        FPasvResponse := FLastResponse;
    end;

    if (FFctPrv in [ftpFctModeZ]) and (FStatusCode = 200) then
        FCurrTransMode := FTransferMode; { V2.102 }

    ExtractMoreResults;

{$IFDEF USE_SSL}
    if not (Self is TSslFtpClient) then begin  { AG V7.03 }
        if Assigned(FDoneAsync) then
            FDoneAsync
        else
            TriggerRequestDone(FRequestResult);
        Exit;
    end;

    if (FFctPrv in [ftpFctAuth]) and
       ((FStatusCode = 234) or (FStatusCode = 334)) then begin
        { Renegotiate the session if already in secure mode }  { V2.106 }
        if Assigned(FControlSocket.SSL) and
           (f_SSL_state(FControlSocket.Ssl) = SSL_ST_OK) then begin
            if (f_SSL_version(FControlSocket.Ssl) >= SSL3_VERSION) then begin
                if not FControlSocket.SslStartRenegotiation then
                begin
                    HandleError('! SslStartRenegotiation failed');  { V7.14 }
                    Exit;
                end;
                TSslFtpClient(Self).FRenegInitFlag := TRUE;
                TriggerDisplay('! Re-negotiate SSL');
                Exit;
            end
            else begin
                { Do nothing in SSLv2 }
                if Assigned(FDoneAsync) then
                    FDoneAsync
                else
                    TriggerRequestDone(FRequestResult);
                Exit;
            end;
        end;

        FControlSocket.SslEnable := TRUE;
        try
            FControlSocket.SslMode             := sslModeClient;
            FControlSocket.OnSslHandshakeDone  := TSslFtpClient(Self).TransferSslHandshakeDone;
            FControlSocket.OnSslVerifyPeer     := TSslFtpClient(Self).TransferSslVerifyPeer;
            FControlSocket.OnSslCliGetSession  := TSslFtpClient(Self).TransferSslCliGetSession;
            FControlSocket.OnSslCliNewSession  := TSslFtpClient(Self).TransferSslCliNewSession;
            FControlSocket.OnSslCliCertRequest := TSslFtpClient(Self).TransferSslCliCertRequest;

            FControlSocket.StartSslHandshake;
        except
            on E:Exception do begin
                TriggerDisplay('! Init SSL failed ' + E.Message);
                FStatusCode    := 550;
                FNextRequest   := nil;
                FRequestResult := FStatusCode;
                FLastResponse  := IntToStr(FStatusCode) + ' ' + E.Message;
                SetErrorMessage;
                if Assigned(FDoneAsync) then
                    FDoneAsync
                else
                    TriggerRequestDone(FRequestResult);
                Exit;
            end;
        end;
        //TriggerDisplay('! Starting SSL handshake');
    end
    else if (FFctPrv in [ftpFctCcc]) and (FStatusCode = 200) then begin  { V2.106 }
        TriggerDisplay('! SSL shutdown control channel started');
        FControlSocket.OnSslShutDownComplete :=
                           TSslFtpClient(Self).ControlSocketSslShutDownComplete;
        FControlSocket.SslBiShutDownAsync;
    end
    else if (FFctPrv in [ftpFctProt]) and (FStatusCode = 200) then { AG V7.03 }
    begin
        { Change data connection SSL protection }
        if (TSslFtpClient(Self).FProtLevelSent = 'P')  then
            TSslFtpClient(Self).FProtDataFlag := TRUE
        else if (TSslFtpClient(Self).FProtLevelSent = 'C') then
            TSslFtpClient(Self).FProtDataFlag := FALSE;

        if Assigned(FDoneAsync) then
            FDoneAsync
        else
            TriggerRequestDone(FRequestResult);
    end
    else
{$ENDIF}
    if Assigned(FDoneAsync) then
        FDoneAsync
    else
        TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.QuitAsync;
begin
    DestroyLocalStream;
    FResumeAt := 0;        { V2.111 clear starting position }
    FFctPrv := ftpFctQuit;
    ExecAsync(ftpQuitAsync, 'QUIT', [221], DoneQuitAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DoneQuitAsync;
begin
   { It's IMO debatable whether or not the client has to call Close at all }
   { since the server must close the connection after QUIT.                }
   StateChange(ftpInternalReady); { V7.27 }
   FControlSocket.Close; { V8.04 reverted back to pre-IPv6 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.CwdAsync;
begin
    if Length(FHostDirName) <= 0 then begin
        HandleError('HostDirName empty');
        Exit;
    end;
    FFctPrv := ftpFctCwd;
    ExecAsync(ftpCwdAsync, 'CWD '+ FHostDirName, [200, 250, 257], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.UserAsync;
var
    CmdBuf : String;
begin
    if Length(FUserName) <= 0 then begin
        HandleError('UserName empty');
        Exit;
    end;
    FFctPrv := ftpFctUser;
    if FConnectionType = ftpProxy then begin
        if (IcsCompareText(FPort, 'ftp') = 0) or
           (IcsCompareText(FPort, '21') = 0) then
            CmdBuf := 'USER ' + FUserName + '@' + FHostName
        else
            CmdBuf := 'USER ' + FUserName + '@' + FHostName + ':' + FPort;
    end
    else
        CmdBuf := 'USER ' + FUserName;
    ExecAsync(ftpUserAsync, CmdBuf, [331, 230], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.PassAsync;
var
    NewPass: String;
begin
    if Length(FPassword) <= 0 then begin
        HandleError('Password empty');
        Exit;
    end;
  { V2.113 check for One Time Password challenge in User response, calculate OTP }
  { ie: otp-md5 999 sill116, otp-md4 998 sill117 or otp-sha1 997 sill118  }
    if OtpProcessChallenge (FLastResponse, FPassword, false, NewPass) then
        TriggerDisplay('! Generated One Time Password: ' + NewPass)
    else
        NewPass := FPassword;
    FFctPrv := ftpFctPass;
    ExecAsync(ftpPassAsync, 'PASS '+ NewPass, [230, 332], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AcctAsync;
begin
    if Length(FAccount) <= 0 then begin
        HandleError('Account empty!');
        Exit;
    end;
    FFctPrv := ftpFctAcct;
    ExecAsync(ftpAccountAsync, 'ACCT '+ FAccount, [230, 202], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SystAsync;
begin
    FFctPrv := ftpFctSyst;
    ExecAsync(ftpSystAsync, 'SYST', [215], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AuthAsync;                                    { V2.106 }
begin
    raise FtpException.Create('AUTH SSL/TLS requires ICS-SSL version');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ProtAsync;
begin
    raise FtpException.Create('PROT require ICS-SSL version');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.PbszAsync;
begin
    raise FtpException.Create('PBSZ require ICS-SSL version');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.CccAsync;                                     { V2.106 }
begin
    raise FtpException.Create('CCC require ICS-SSL version');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RestAsyncGetResumePos;
begin
    { When restarting a download, we always start from current local file   }
    { size. When restarting a upload, we restart from ResumeAt property     }
    { value. This property could be initialized using Size command.         }
    if (not (FRequestType in [ftpRestartPutAsync, ftpRestPutAsync])) and
       (not (ftpNoAutoResumeAt in FOptions)) then
        FResumeAt := GetFileSize(FLocalFileName)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RestAsync;
begin
    FFctPrv   := ftpFctRest;
    RestAsyncGetResumePos;
    if FResumeAt > 0 then
        ExecAsync(ftpRestAsync, 'REST ' + IntToStr(FResumeAt), [0], nil)
    else begin
        FRequestDoneFlag  := FALSE;
        FNext             := NextExecAsync;
        FDoneAsync        := nil;
        TriggerRequestDone(0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SizeAsync;
begin
    FSizeResult := 0;
    FFctPrv := ftpFctSize;
    ExecAsync(ftpSizeAsync, 'SIZE ' + FHostFileName, [213], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TypeSetAsync;
begin
    FFctPrv := ftpFctTypeSet;
    ExecAsync(ftpTypeSetAsync, 'TYPE ' + FType, [200], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TypeBinaryAsync;
begin
    Binary := TRUE;
    TypeSetAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TypeAsciiAsync;
begin
    Binary := FALSE;
    TypeSetAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.MkdAsync;
begin
    FFctPrv := ftpFctMkd;
    ExecAsync(ftpMkdAsync, 'MKD ' + FHostFileName, [200, 250, 257], nil);  { V2.100 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RmdAsync;
begin
    FFctPrv := ftpFctRmd;
    ExecAsync(ftpRmdAsync, 'RMD ' + FHostFileName, [200, 250, 257], nil);  { V2.100 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DeleAsync;
begin
    FFctPrv := ftpFctDele;
    ExecAsync(ftpDeleAsync, 'DELE ' + FHostFileName, [200, 250, 257], nil); { V2.100 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AbortXferAsync;
begin
    FFctPrv := ftpFctAbortXfer;
    ExecAsync(ftpAbortXferAsync, 'ABOR', [0], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.QuoteAsync;
begin
    FFctPrv := ftpFctQuote;
    ExecAsync(ftpQuoteAsync, FLocalFileName, [0], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.PwdAsync;
begin
    FFctPrv := ftpFctPwd;
    ExecAsync(ftpPwdAsync, 'PWD', [250, 257], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.CDupAsync;
begin
    FFctPrv := ftpFctCDup;
    ExecAsync(ftpCDupAsync, 'CDUP', [250, 257], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RenFromAsync;
begin
    FFctPrv := ftpFctRenFrom;
    ExecAsync(ftpRenFromAsync, 'RNFR ' + FHostFileName, [350], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RenToAsync;
begin
    FFctPrv := ftpFctRenTo;
    ExecAsync(ftpRenToAsync, 'RNTO ' + FLocalFileName, [200, 250, 257], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.MlstAsync;     { V2.90 machine list one file        }
begin
    FFctPrv   := ftpFctMlst;
    FRemFacts := '';
    ExecAsync(ftpMlstAsync, 'MLST ' + FHostFileName, [250], nil); { V2.100 }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.FeatAsync;     { V2.90 supported extensions         }
begin
    FFctPrv := ftpFctFeat;
    ExecAsync(ftpFeatAsync, 'FEAT', [211], nil); { V2.100 }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.MdtmAsync;     { V2.90 get file modification time   }
begin
    FFctPrv := ftpFctMdtm;
    ExecAsync(ftpMdtmAsync, 'MDTM ' + FHostFileName, [213], nil); { V2.100 }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.MdtmyyAsync;   { V2.90 set file modification time - RhinoSoft Serv-U }
var
    S: String;
begin
    if FRemFileDT < 10 then begin
        HandleError('Modification date empty');
        Exit;
    end;
    FFctPrv := ftpFctMdtmyy;
    S       := FormatDateTime('yyyymmddhhnnss', FRemFileDT) {+ '+0' }; // V7.01 latest Serv-U objects to +   { no time offset=UTC }
    ExecAsync(ftpMdtmyyAsync, 'MDTM ' + S + ' ' + FHostFileName, [213, 253], nil);  { V2.100 }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.MfmtAsync;   { V2.94 modify file modification time }
var
    S: String;
begin
    if FRemFileDT < 10 then begin
        HandleError('Modification date empty');
        Exit;
    end;
    FFctPrv := ftpFctMfmt;
    S       := FormatDateTime('yyyymmddhhnnss', FRemFileDT);  { UTC }
    ExecAsync(ftpMfmtAsync, 'MFMT ' + S + ' ' + FHostFileName, [213], nil);  { V2.100 }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.Md5Async;     { V2.94 get MD5 hash sum   }
begin
    FFctPrv := ftpFctMd5;
    ExecAsync(ftpMd5Async, 'MD5 ' + FHostFileName, [251], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.XMd5Async;     { V2.113 get MD5 hash sum with optional start and end }
var
    S: String;
begin
    FFctPrv := ftpFctMd5;
    S := '"' + FHostFileName + '"';
    if (FPosStart >= 0) and (FPosEnd > FPosStart) then
             S := S + ' ' + IntToStr(FPosStart) + ' ' + IntToStr(FPosEnd);
    ExecAsync(ftpMd5Async, 'XMD5 ' + S, [250], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ModeZAsync;    { V2.102  Mode Z  }
var
    S: string;
begin
    FFctPrv := ftpFctModeZ;
    if FTransferMode = ftpTransModeZDeflate then
        S := 'MODE Z'
    else
        S := 'MODE S';
    ExecAsync(ftpModeZAsync, S, [200], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.OptsAsync;     { V2.102  Opts, set Mode Z options }
begin
    FFctPrv := ftpFctOpts;
    ExecAsync(ftpOptsAsync, 'OPTS ' + FNewOpts, [200], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.XCrcAsync;     { V2.107 get CRC32 hash sum, V2.113 with optional start and end  }
var
    S: String;
begin
    FFctPrv := ftpFctXCrc;
    S := '"' + FHostFileName + '"';
    if (FPosStart >= 0) and (FPosEnd > FPosStart) then
             S := S + ' ' + IntToStr(FPosStart) + ' ' + IntToStr(FPosEnd);
    ExecAsync(ftpXCrcAsync, 'XCRC ' + S, [250], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ClntAsync;     { V2.113  client string  }
begin
    FFctPrv := ftpFctClnt;
    ExecAsync(ftpClntAsync, 'CLNT ' + FClientIdStr, [200,215], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AlloAsync;     { V2.113  check space allocation, PosEnd is upload size }
begin
    FFctPrv := ftpFctClnt;
    ExecAsync(ftpAlloAsync, 'ALLO ' + IntToStr(FPosEnd), [200], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.CombAsync;     { V2.113  combine file names  }
begin
    FFctPrv := ftpFctClnt;
    ExecAsync(ftpCombAsync, 'COMB ' + FHostFileName, [200], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SitePaswdAsync;     { V2.113  change password }
begin
    FFctPrv := ftpFctSitePaswd;
    ExecAsync(ftpSitePaswdAsync, 'SITE PSWD ' + FPassWord + ' ' + FHostFileName, [200], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SiteExecAsync;    { V2.113  run program  }
begin
    FFctPrv := ftpFctSiteExec;
    ExecAsync(ftpSiteExecAsync, 'SITE EXEC ' + FHostFileName, [200], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SiteIndexAsync;     { V2.113  list files and dirs recursively  }
begin
    FFctPrv := ftpFctSiteIndex;
    CreateLocalFileStream;
    ExecAsync(ftpSiteIndexAsync, 'SITE INDEX ' + FHostFileName, [200], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SiteZoneAsync;     { V2.113  get time zone difference }
begin
    FFctPrv := ftpFctSiteZone;
    ExecAsync(ftpSiteZoneAsync, 'SITE ZONE', [210], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SiteMsgAsync;      { V2.113  send message }
begin
    FFctPrv := ftpFctSiteMsg;
    ExecAsync(ftpSiteMsgAsync, 'SITE MSG ' + FHostFileName, [200], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SiteCmlsdAsync;      { V2.113  extended MLSD using control channel }
begin

{ data will be returned on control channel so we need stream to write it }
    FFctPrv := ftpFctSiteCmlsd;
    CreateLocalFileStream;
    ExecAsync(ftpSiteCmlsdAsync, 'SITE CMLSD ' + FHostFileName, [200,250], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ReinAsync;     { V7.01   re-initialize control connection  }
begin
    FFctPrv := ftpFctRein;
    ExecAsync(ftpReinAsync, 'REIN', [220], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.HostAsync;     { V7.01   domain/hostname, usually sent before logon  }
var
    S: String;
begin
    FFctPrv := ftpFctHost;
    if FDnsResult = FHostName then  { if host name was IP address, delimit it }
        S := '[' + FHostName + ']'
    else
        S := FHostName;
 { note: responses 504 and 530 are really domain not found but don't stop login }
 { V7.05 ws_ftp server returns 200 }
    ExecAsync(ftpHostAsync, 'HOST ' + S, [200,220,421,500,502,504,530,550], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.LangAsync;     { V7.01   language for messages }
begin
    FFctPrv := ftpFctLang;
    ExecAsync(ftpLangAsync, 'LANG ' + FLanguage, [200], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.XCmlsdAsync;      { V7.01  extended MLSD using control channel }
begin

{ data will be returned on control channel so we need stream to write it }
    FFctPrv := ftpFctXCmlsd;
    CreateLocalFileStream;
    ExecAsync(ftpXCmlsdAsync, 'XCMLSD ' + FHostFileName, [200,250], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AbortAsync;
{var
    bFlag : Boolean; }                                             { 2.106 }
begin
{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, '! Aborting');
{$ENDIF}
    {bFlag := (FState = ftpDnsLookup);}                            { 2.106 }
    StateChange(ftpAbort);
{$IFDEF USE_SSL}
    if Assigned(FControlSocket.OnSslShutDownComplete) then
        TSslFtpClient(Self).ControlSocketSslShutDownComplete(FControlSocket,
        TRUE, 426);
{$ENDIF}
    DestroyLocalStream;
    FResumeAt := 0;        { V2.111 clear starting position }

    {if bFlag then                                                 // 2.106
        FControlSocket.CancelDnsLookup;
    if FControlSocket.State <> wsClosed then
        FControlSocket.Close;
    if FDataSocket.State <> wsClosed then
        FDataSocket.Close; }
    FControlSocket.Abort;
    FDataSocket.Abort;
    {DestroyLocalStream;}
    FConnected := FALSE;                                          { 2.106 }
    StateChange(ftpReady);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DoHighLevelAsync;
begin
{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecDump) then
        DebugLog(loProtSpecDump, '! HighLevelAsync ' + IntToStr(FRequestResult));
{$ENDIF}
    if FState = ftpAbort then begin
{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
        if CheckLogOptions(loProtSpecInfo) then
            DebugLog(loProtSpecInfo, '! Abort detected');
{$ENDIF}
        FFctSet := [];
        FHighLevelResult := 426;
        FErrorMessage    := '426 Operation aborted.';
    end;

    FNextRequest := DoHighLevelAsync;

    if FRequestResult <> 0 then begin
        { Previous command had errors }
        FHighLevelResult := FRequestResult;
        if (FFctPrv = ftpFctQuit) or (not (ftpFctQuit in FFctSet)) then
            FFctSet := []
        else
            FFctSet := [ftpFctQuit];
    end;

    ExtractMoreResults;

    if ftpFctOpen in FFctSet then begin
        FFctPrv := ftpFctOpen;
        FFctSet := FFctSet - [FFctPrv];
        OpenAsync;
        Exit;
    end;

    if ftpFctHost in FFctSet then begin             { V7.01 }
        FFctPrv := ftpFctHost;
        FFctSet := FFctSet - [ftpFctHost];
        HostAsync;
        Exit;
    end;

    if (ftpFctAuth in FFctSet) and                    { V2.106 }
       ((FSslType = sslTypeAuthSsl) or (FSslType = sslTypeAuthTls)) then begin
        FFctPrv := ftpFctAuth;
        FFctSet := FFctSet - [ftpFctAuth];
        AuthAsync;
        Exit;
    end;

    if ftpFctPbsz in FFctSet then begin
        FFctPrv := ftpFctPbsz;
        FFctSet := FFctSet - [ftpFctPbsz];
        PbszAsync;
        Exit;
    end;

    if ftpFctProt in FFctSet then begin
        FFctPrv := ftpFctProt;
        FFctSet := FFctSet - [ftpFctProt];
        ProtAsync;
        Exit;
    end;

    if ftpFctCcc in FFctSet then begin                  { V2.106 }                
        FFctPrv := ftpFctCcc;
        FFctSet := FFctSet - [ftpFctCcc];
        CccAsync;
        Exit;
    end;

    if ftpFctUser in FFctSet then begin
        FFctPrv := ftpFctUser;
        FFctSet := FFctSet - [FFctPrv];
        UserAsync;
        Exit;
    end;

    if ftpFctPass in FFctSet then begin
        FFctSet := FFctSet - [ftpFctPass];
        if (FFctPrv <> ftpFctUser) or
           ((FfctPrv = ftpFctUser) and (FStatusCode = 331)) then begin
            FFctPrv := ftpFctPass;
            PassAsync;
            Exit;
        end;
    end;

    if ftpFctAcct in FFctSet then begin
        FFctSet := FFctSet - [ftpFctAcct];
        if (FFctPrv <> ftpFctPass) or
           ((FfctPrv = ftpFctPass) and (FStatusCode = 332)) then begin
            FFctPrv := ftpFctAcct;
            AcctAsync;
            Exit;
        end;
    end;

    if ftpFctCwd in FFctSet then begin
        FFctSet := FFctSet - [ftpFctCwd];
        if Length(FHostDirName) > 0 then begin
            FFctPrv := ftpFctCwd;
            CwdAsync;
            Exit;
        end;
    end;

    if ftpFctFeat in FFctSet then begin    { V7.08 }
        FFctPrv := ftpFctFeat;
        FFctSet := FFctSet - [FFctPrv];
        FeatAsync;
        Exit;
    end;

    if ftpFctCDup in FFctSet then begin
        FFctPrv := ftpFctCDup;
        FFctSet := FFctSet - [FFctPrv];
        CDupAsync;
        Exit;
    end;

    if ftpFctTypeSet in FFctSet then begin
        FFctPrv := ftpFctTypeSet;
        FFctSet := FFctSet - [FFctPrv];
        TypeSetAsync;
        Exit;
    end;

    if ftpFctPort in FFctSet then begin
        FFctPrv := ftpFctPort;
        FFctSet := FFctSet - [FFctPrv];
        PortAsync;
        Exit;
    end;

    if ftpFctRest in FFctSet then begin
        FFctPrv := ftpFctRest;
        FFctSet := FFctSet - [FFctPrv];
        RestAsync;
        Exit;
    end;

    if ftpFctGet in FFctSet then begin
        if (FFctPrv <> ftpFctRest) or
           (FResumeAt < 0) or
           ((FFctPrv = ftpFctRest) and (FStatusCode <> 350)) then
            FResumeAt := 0;

        FFctPrv   := ftpFctGet;
        FFctSet   := FFctSet - [FFctPrv];
        ExecGetAsync;
        Exit;
    end;

    if ftpFctPut in FFctSet then begin
        FFctPrv := ftpFctPut;
        FFctSet := FFctSet - [FFctPrv];
        ExecPutAsync;
        Exit;
    end;

    if ftpFctAppend in FFctSet then begin
        FFctPrv := ftpFctAppend;
        FFctSet := FFctSet - [FFctPrv];
        ExecAppendAsync;
        Exit;
    end;

    if ftpFctDir in FFctSet then begin
        FFctPrv := ftpFctDir;
        FFctSet := FFctSet - [FFctPrv];
        ExecDirAsync;
        Exit;
    end;

    if ftpFctLs in FFctSet then begin
        FFctPrv := ftpFctLs;
        FFctSet := FFctSet - [FFctPrv];
        ExecLsAsync;
        Exit;
    end;

    if ftpFctSyst in FFctSet then begin
        FFctPrv := ftpFctSyst;
        FFctSet := FFctSet - [FFctPrv];
        SystAsync;
        Exit;
    end;

    if ftpFctMkd in FFctSet then begin
        FFctPrv := ftpFctMkd;
        FFctSet := FFctSet - [FFctPrv];
        MkdAsync;
        Exit;
    end;

    if ftpFctRmd in FFctSet then begin
        FFctPrv := ftpFctRmd;
        FFctSet := FFctSet - [FFctPrv];
        RmdAsync;
        Exit;
    end;

    if ftpFctRenFrom in FFctSet then begin
        FFctPrv := ftpFctRenFrom;
        FFctSet := FFctSet - [FFctPrv];
        RenFromAsync;
        Exit;
    end;

    if ftpFctRenTo in FFctSet then begin
        FFctPrv := ftpFctRenTo;
        FFctSet := FFctSet - [FFctPrv];
        RenToAsync;
        Exit;
    end;

    if ftpFctSize in FFctSet then begin
        FFctPrv := ftpFctSize;
        FFctSet := FFctSet - [FFctPrv];
        SizeAsync;
        Exit;
    end;

    if ftpFctDele in FFctSet then begin
        FFctPrv := ftpFctDele;
        FFctSet := FFctSet - [FFctPrv];
        DeleAsync;
        Exit;
    end;

    if ftpFctPwd in FFctSet then begin
        FFctPrv := ftpFctPwd;
        FFctSet := FFctSet - [FFctPrv];
        PwdAsync;
        Exit;
    end;

    if ftpFctAbortXfer in FFctSet then begin
        FFctPrv := ftpFctAbortXfer;
        FFctSet := FFctSet - [FFctPrv];
        AbortXferAsync;
        Exit;
    end;

    if ftpFctQuote in FFctSet then begin
        FFctPrv := ftpFctQuote;
        FFctSet := FFctSet - [FFctPrv];
        QuoteAsync;
        Exit;
    end;

    if ftpFctQuit in FFctSet then begin
        FFctPrv := ftpFctQuit;
        FFctSet := FFctSet - [FFctPrv];
        FLastResponseSave := FLastResponse;
        FStatusCodeSave   := FStatusCode;
        QuitAsync;
        Exit;
    end;

    if ftpFctMlsd in FFctSet then begin     { V2.90 } 
        FFctPrv := ftpFctMlsd;
        FFctSet := FFctSet - [FFctPrv];
        ExecMlsdAsync;
        Exit;
    end;

    if ftpFctModeZ in FFctSet then begin     { V2.102 }
        FFctPrv := ftpFctModeZ;
        FFctSet := FFctSet - [FFctPrv];
        ModeZAsync;
        Exit;
    end;

    if ftpFctOpts in FFctSet then begin     { V2.102 }
        FFctPrv := ftpFctOpts;
        FFctSet := FFctSet - [FFctPrv];
        OptsAsync;
        Exit;
    end;

    if ftpFctSiteDmlsd in FFctSet then begin    { V2.113 }
        FFctPrv := ftpFctSiteDmlsd;
        FFctSet := FFctSet - [FFctPrv];
        ExecSiteDmlsdAsync;
        Exit;
    end;

    if ftpFctXDmlsd in FFctSet then begin      { V7.01 }
        FFctPrv := ftpFctXDmlsd;
        FFctSet := FFctSet - [FFctPrv];
        ExecXDmlsdAsync;
        Exit;
    end;

    if ftpFctClnt in FFctSet then begin         { V2.113 }
        FFctPrv := ftpFctClnt;
        FFctSet := FFctSet - [FFctPrv];
        ClntAsync;
        Exit;
    end;

{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecDump) then
        DebugLog(loProtSpecDump, '! HighLevelAsync done');
{$ENDIF}
    FFctSet          := [];
    FNextRequest     := nil;
    FRequestDoneFlag := FALSE;
    TriggerRequestDone(FHighLevelResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.HighLevelAsync(RqType : TFtpRequest; Fcts : TFtpFctSet);
begin
    if FConnected and (ftpFctOpen in Fcts) then begin
        HandleError('FTP component already connected');
        Exit;
    end;
    if not CheckReady then begin
        TriggerDisplay('Not ready for Request, Req=' + LookupFTPReq (RqType));  { V8.03 }
        Exit;
    end;
    FLastResponseSave := FLastResponse;
    FStatusCodeSave   := -1;
    FRequestType      := RqType;
    FRequestResult    := 0;
    FFctSet           := Fcts;
    FFctPrv           := ftpFctNone;
    FHighLevelResult  := 0;
    FHighLevelFlag    := TRUE;
    FLastResponse     := '';
    FErrorMessage     := '';
    FRestartFlag      := FALSE;
    FNextRequest      := nil;
    DoHighLevelAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ConnectAsync;
begin
    HighLevelAsync(ftpConnectAsync,
                   [ftpFctOpen, ftpFctAuth, ftpFctUser, ftpFctPass,
                    ftpFctAcct]);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ConnectHostAsync;       { V7.01 }
begin
    HighLevelAsync(ftpConnectHostAsync,
                   [ftpFctOpen, ftpFctHost, ftpFctAuth, ftpFctUser,
                    ftpFctPass, ftpFctAcct]);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ConnectFeatAsync;           { V7.09 }
begin
    HighLevelAsync(ftpConnectFeatAsync,     { V7.29 }
                   [ftpFctOpen, ftpFctAuth, ftpFctUser, ftpFctPass,
                    ftpFctAcct, ftpFctFeat]);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ConnectFeatHostAsync;       { V7.09 }
begin
    HighLevelAsync(ftpConnectFeatHostAsync, { V7.29 }
                   [ftpFctOpen, ftpFctHost, ftpFctAuth, ftpFctUser,
                    ftpFctPass, ftpFctAcct, ftpFctFeat]);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ReceiveAsync;
begin
    HighLevelAsync(ftpReceiveAsync,
                   [ftpFctOpen, ftpFctAuth, ftpFctUser, ftpFctPass,
                    ftpFctAcct, ftpFctCwd,  ftpFctTypeSet, ftpFctPort,
                    ftpFctGet,  ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.PutAsync;
begin
	DataSocket.LastError := 0; { V2.100 }
    HighLevelAsync(ftpPutAsync,
                   [ftpFctPort, ftpFctPut]);
    if DataSocket.LastError <> 0 then    { V2.100 }
       raise FtpException.Create('Socket Error - ' +
                              GetWinsockErr(DataSocket.LastError)); { V2.100 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ By A.Burlakov: new procedure for resuming uploads                         }
{ Uses REST + STOR commands instead APPEND                                  }
procedure TCustomFtpCli.RestPutAsync;
begin
    HighLevelAsync(ftpRestPutAsync,
                   [ftpFctRest, ftpFctPort, ftpFctPut]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RestartPutAsync;
begin
    HighLevelAsync(ftpRestartPutAsync,
                   [ftpFctOpen,    ftpFctAuth,    ftpFctUser,    ftpFctPass,
                    ftpFctAcct,    ftpFctCwd,     ftpFctTypeSet, ftpFctRest,
                    ftpFctPort,    ftpFctPut,     ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TransmitAsync;
begin
    HighLevelAsync(ftpTransmitAsync,
                   [ftpFctOpen, ftpFctAuth,    ftpFctUser,    ftpFctPass,
                    ftpFctAcct, ftpFctCwd,     ftpFctTypeSet, ftpFctPort,
                    ftpFctPut,  ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AppendAsync;
begin
    HighLevelAsync(ftpAppendAsync,
                   [ftpFctPort, ftpFctAppend]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AppendFileAsync;
begin
    HighLevelAsync(ftpTransmitAsync,
                   [ftpFctOpen,   ftpFctAuth,    ftpFctUser,    ftpFctPass,
                    ftpFctAcct,   ftpFctCwd,     ftpFctTypeSet, ftpFctPort,
                    ftpFctAppend, ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DirAsync;
begin
    HighLevelAsync(ftpDirAsync,
                   [ftpFctPort, ftpFctDir]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DirectoryAsync;
begin
    HighLevelAsync(ftpDirectoryAsync,
                   [ftpFctOpen, ftpFctAuth,    ftpFctUser, ftpFctPass,
                    ftpFctAcct, ftpFctCwd,     ftpFctPort, ftpFctDir,
                    ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.LsAsync;
begin
    HighLevelAsync(ftpLsAsync,
                   [ftpFctPort, ftpFctLs]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ListAsync;
begin
    HighLevelAsync(ftpListAsync,
                   [ftpFctOpen, ftpFctAuth,    ftpFctUser, ftpFctPass,
                    ftpFctAcct, ftpFctCwd,     ftpFctPort, ftpFctLs,
                    ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SystemAsync;
begin
    HighLevelAsync(ftpSystemAsync,
                   [ftpFctOpen, ftpFctAuth,    ftpFctUser, ftpFctPass,
                    ftpFctAcct, ftpFctSyst,    ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AuthenticateSslAsync;
begin
    HighLevelAsync(ftpAuthAsync,
                   [ftpFctOpen, ftpFctAuth, ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RestartGetAsync;
begin
    HighLevelAsync(ftpRestartGetAsync,
                   [ftpFctOpen,    ftpFctAuth,    ftpFctUser,    ftpFctPass,
                    ftpFctAcct,    ftpFctCwd,     ftpFctTypeSet, ftpFctRest,
                    ftpFctPort,    ftpFctGet,     ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RestGetAsync;
begin
    HighLevelAsync(ftpRestGetAsync,
                   [ftpFctRest, ftpFctPort, ftpFctGet]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.GetAsync;
begin
    HighLevelAsync(ftpGetAsync,
                   [ftpFctPort, ftpFctGet]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.MkdirAsync;
begin
    HighLevelAsync(ftpMkdirAsync,
                   [ftpFctOpen, ftpFctAuth,    ftpFctUser,    ftpFctPass,
                    ftpFctAcct, ftpFctCwd,     ftpFctMkd,     ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RmdirAsync;
begin
    HighLevelAsync(ftpRmdirAsync,
                   [ftpFctOpen, ftpFctAuth,    ftpFctUser,    ftpFctPass,
                    ftpFctAcct, ftpFctCwd,     ftpFctRmd,     ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DeleteAsync;
begin
    HighLevelAsync(ftpDeleteAsync,
                   [ftpFctOpen, ftpFctAuth,    ftpFctUser, ftpFctPass,
                    ftpFctAcct, ftpFctCwd,     ftpFctDele,    ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DoQuoteAsync;
begin
    HighLevelAsync(ftpDoQuoteAsync,
                   [ftpFctOpen,  ftpFctAuth,    ftpFctUser,    ftpFctPass,
                    ftpFctAcct,  ftpFctCwd,     ftpFctQuote,   ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RenameAsync;
begin
    HighLevelAsync(ftpRenameAsync,
                   [ftpFctOpen,    ftpFctAuth,    ftpFctUser,    ftpFctPass,
                    ftpFctAcct,    ftpFctCwd,     ftpFctRenFrom, ftpFctRenTo,
                    ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RenAsync;
begin
    HighLevelAsync(ftpRenAsync, [ftpFctRenFrom, ftpFctRenTo]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.FileSizeAsync;
begin
    HighLevelAsync(ftpSizeAsync,
                   [ftpFctOpen, ftpFctAuth,    ftpFctUser,    ftpFctPass,
                    ftpFctAcct, ftpFctCwd,     ftpFctSize,    ftpFctQuit]);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.MlsdAsync;    { V2.90 machine list directory        }
begin
    HighLevelAsync(ftpMlsdAsync,
                   [ftpFctPort, ftpFctMlsd]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SiteDmlsdAsync;  { V2.113  extended MLSD using data channel }
begin
    HighLevelAsync(ftpSiteDmlsdAsync,
                   [ftpFctPort, ftpFctSiteDmlsd]);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.XDmlsdAsync;  { V7.01  extended MLSD using data channel }
begin
    HighLevelAsync(ftpXDmlsdAsync, [ftpFctPort, ftpFctXDmlsd]);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketGetDataAvailable(
    Sender  : TObject;
    ErrCode : word);
var
    Len     : Integer;
    Buffer  : array [1..FTP_RCV_BUF_SIZE] of AnsiChar;  { V7.01 Should use a dynamic buffer instead... }
    aSocket : TWSocket;
    I, J    : Integer;
    Line    : AnsiString; { AG V7.02 RawByteString is not required }
    ACodePage : LongWord;
begin
    if not Progress then
        Exit;

    aSocket := Sender as TWSocket;

    Len := aSocket.Receive(@Buffer[1], High(Buffer));
{TriggerDisplay('! Data received ' + IntToStr(Len));}
    if Len = 0 then
        { Remote has closed, ignore }
    else if Len < 0 then begin
        { An error has occured }
        if (aSocket.State = wsConnected) and
           (aSocket.LastError <> WSAEWOULDBLOCK) then begin
            TriggerDisplay('! Data: Receive Error - ' +
                                     GetWinsockErr(aSocket.LastError));
            aSocket.Shutdown(2);
            Exit;
        end;
    end
    else begin
{$IFDEF UseBandwidthControl}
    Inc(FBandwidthCount, Len);
    if ftpBandwidthControl in FOptions then begin
        if (FBandwidthCount > FBandwidthMaxCount) and
           (not FBandwidthPaused) then begin
            FBandwidthPaused := TRUE;
            aSocket.Pause;
        end;
    end;
{$ENDIF}

{ abandon data if aborting xfer, it's probably corrupted - Angus }
        if FState in [ftpAbort, ftpInternalAbort] then begin
            TriggerDisplay('! Data ignored while aborting');
            exit;
        end ;
        if FZStreamState = ftpZStateSaveDecom then begin   { V1.103 }
            FModeZStream.WriteBuffer(Buffer, Len);
            FByteCount := FByteCount + Len;  { compressed size }
            exit;
        end ;
        if FLocalStream <> nil then begin
            try
                LocalStreamWrite(Buffer, Len);
            except
                TriggerDisplay('! Error writing local file');
                aSocket.Shutdown(2);
                Exit;
            end;
        end;

        { Update our statistics }
        FByteCount := FByteCount + Len;

        { If requested to display the received data, do it line by line }
        { V7.01 assume this function is only being used to view directory listings, which may be UTF-8 }
        if FDisplayFileFlag then begin
            { Auto-detect UTF-8 if Option is set }
            if (FCodePage <> CP_UTF8) and (ftpAutoDetectCodePage in FOptions) and
               (CharsetDetect(@Buffer, Len) = cdrUtf8) then          { AG V7.02 }
                ACodePage := CP_UTF8
            else
                ACodePage := FCodePage;

            case FDisplayFileMode of
            ftpBinary:
                begin
                    SetLength(Line, Len);
                    Move(Buffer[1], Line[1], Length(Line));
                {$IFDEF COMPILER12_UP}
                    TriggerDisplayFile(AnsiToUnicode(Line, ACodePage));
                {$ELSE}
                    TriggerDisplayFile(ConvertCodepage (Line, ACodePage, CP_ACP));
                {$ENDIF}
                end;
            ftpLineByLine:
                if Len > 0 then begin
                    i := 1;
                    while (i <= Len) do begin
                        j := 1;
                        while (i <= Len) and (Buffer[i] <> #10) and (Buffer[i] <> #13) do begin
                            i := i + 1;
                            j := j + 1;
                        end;
                        SetLength(Line, j - 1);
                        if Length(Line) > 0 then
                            Move(Buffer[i - j + 1], Line[1], Length(Line));
                    {$IFDEF COMPILER12_UP}
                        TriggerDisplayFile(AnsiToUnicode(Line, ACodePage));
                    {$ELSE}
                        TriggerDisplayFile(ConvertCodepage (Line, ACodePage, CP_ACP));
                    {$ENDIF}
                        while (i <= Len) and ((Buffer[i] = #10) or (Buffer[i] = #13)) do
                            i := i + 1;
                    end;
                end;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketGetSessionConnected(
    Sender  : TObject;
    ErrCode : word);
begin
{$IFNDEF NO_DEBUG_LOG}                                             { 2.104 }
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, '! Data Session Connected (Get)');
{$ENDIF}
    { Use the socket for the data transmission }
    FDataSocket.OnSessionClosed := DataSocketGetSessionClosed;
    FDataSocket.OnDataAvailable := DataSocketGetDataAvailable;
    FDataSocket.OnDataSent      := nil;

    { Record the starting time }
    FStartTime := LongInt(IcsGetTickCount);
    FDurationMsecs := 0;  { V2.113 }

    if ErrCode <> 0 then begin
        FLastResponse := 'Unable to establish data connection - ' +
                         WSocketGetErrorMsgFromErrorCode(ErrCode);
        FStatusCode   := 550;
        SetErrorMessage;
        FDataSocket.Close;
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
    end
    else begin
        if FDataSocket.SocketRcvBufSize <> FDSocketRcvBufSize then {AG V7.26}
            FDataSocket.SocketRcvBufSize := FDSocketRcvBufSize;    {AG V7.26}
{$IFDEF UseBandwidthControl}
        FBandwidthCount := 0; // Reset byte counter
        if ftpBandwidthControl in FOptions then begin
            if not Assigned(FBandwidthTimer) then
                FBandwidthTimer := TIcsTimer.Create(Self);
            FBandwidthTimer.Enabled  := FALSE;
            FBandwidthTimer.Interval := FBandwidthSampling;
            FBandwidthTimer.OnTimer  := BandwidthTimerTimer;
            FBandwidthTimer.Enabled  := TRUE;
            // Number of bytes we allow during a sampling period
            FBandwidthMaxCount := Int64(FBandwidthLimit) * FBandwidthSampling div 1000;
            FBandwidthPaused   := FALSE;
        end;
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Used for passive mode                                                     }
procedure TCustomFtpCli.DataSocketPutSessionConnected(
    Sender  : TObject;
    ErrCode : word);
begin
{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, '! Data Session opened (Put)');
{$ENDIF}
    { Use the socket for the data transmission }
    FDataSocket.OnSessionClosed := DataSocketPutSessionClosed;
    FDataSocket.OnDataAvailable := nil;
    FDataSocket.OnDataSent      := nil;

    { Record we opened data session }
    FPutSessionOpened := TRUE;

    { Record the starting time }
    FStartTime := LongInt(IcsGetTickCount);
    FDurationMsecs := 0;  { V2.113 }

    if ErrCode <> 0 then begin
        FLastResponse := 'Unable to establish data connection - ' +
                         WSocketGetErrorMsgFromErrorCode(ErrCode);
        FStatusCode   := 550;
        SetErrorMessage;
        FDataSocket.Close;
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;
    if FDataSocket.SocketSndBufSize <> FDSocketSndBufSize then {AG V7.26}
        FDataSocket.SocketSndBufSize := FDSocketSndBufSize;    {AG V7.26}
{$IFDEF UseBandwidthControl}
    FBandwidthCount := 0; // Reset byte counter
    if ftpBandwidthControl in FOptions then begin
        if not Assigned(FBandwidthTimer) then
            FBandwidthTimer := TIcsTimer.Create(Self);
        FBandwidthTimer.Enabled  := FALSE;
        FBandwidthTimer.Interval := FBandwidthSampling;
        FBandwidthTimer.OnTimer  := BandwidthTimerTimer;
        FBandwidthTimer.Enabled  := TRUE;
        // Number of bytes we allow during a sampling period
        FBandwidthMaxCount := Int64(FBandwidthLimit) * FBandwidthSampling div 1000;
        FBandwidthPaused   := FALSE;
    end;
{$ENDIF}
    StateChange(ftpWaitingResponse);
    FNext := Next1PutAsync;

    if FAppendFlag then
        SendCommand('APPE ' + FHostFileName)
    else
        SendCommand('STOR ' + FHostFileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketGetSessionAvailable(
    Sender  : TObject;
    ErrCode : word);
var
    aSocket : TSocket;
begin
{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, '! Data Session requested');
{$ENDIF}
    { Accept the incomming connection initiated by the FTP server for data }
    aSocket := FDataSocket.Accept;

    { Close the listening socket, we don't need it anymore }
    FDataSocket.Close;

{$IFDEF USE_SSL}
    if (Self is TSslFtpClient) then begin      { V2.107 }
        TCustomSslWSocket(FDataSocket).SslEnable := TSslFtpClient(Self).FProtDataFlag; { AG V7.03 }
        if TCustomSslWSocket(FDataSocket).SslEnable then begin
            TCustomSslWSocket(FDataSocket).SslContext          := FControlSocket.SslContext;
            TCustomSslWSocket(FDataSocket).SslMode             := sslModeClient;
            TCustomSslWSocket(FDataSocket).OnSslVerifyPeer     := FControlSocket.OnSslVerifyPeer;
            TCustomSslWSocket(FDataSocket).OnSslHandshakeDone  := FControlSocket.OnSslHandshakeDone;
            TCustomSslWSocket(FDataSocket).OnSslCliGetSession  := FControlSocket.OnSslCliGetSession;
            TCustomSslWSocket(FDataSocket).OnSslCliNewSession  := FControlSocket.OnSslCliNewSession;
            TCustomSslWSocket(FDataSocket).OnSslCliCertRequest := FControlSocket.OnSslCliCertRequest;
        end;
    end;
{$ENDIF}

    { Reuse the socket for the data transmission }
    FDataSocket.OnSessionClosed  := DataSocketGetSessionClosed;
    FDataSocket.OnDataAvailable  := DataSocketGetDataAvailable;
    FDataSocket.OnDataSent       := nil;
    FDataSocket.HSocket          := aSocket;
    if FDataSocket.SocketRcvBufSize <> FDSocketRcvBufSize then {AG V7.26}
        FDataSocket.SocketRcvBufSize := FDSocketRcvBufSize;    {AG V7.26}
    FDataSocket.ComponentOptions := [wsoNoReceiveLoop];   { 26/10/02 } { 2.109 }
{$IFDEF UseBandwidthControl}
    FBandwidthCount := 0; // Reset byte counter
    if ftpBandwidthControl in FOptions then begin
        if not Assigned(FBandwidthTimer) then
            FBandwidthTimer := TIcsTimer.Create(Self);
        FBandwidthTimer.Enabled  := FALSE;
        FBandwidthTimer.Interval := FBandwidthSampling;
        FBandwidthTimer.OnTimer  := BandwidthTimerTimer;
        FBandwidthTimer.Enabled  := TRUE;
        // Number of bytes we allow during a sampling period
        FBandwidthMaxCount := Int64(FBandwidthLimit) * FBandwidthSampling div 1000;
        FBandwidthPaused   := FALSE;
    end;
{$ENDIF}
    { Record the starting time }
    FStartTime := LongInt(IcsGetTickCount);
    FDurationMsecs := 0;  { V2.113 }
{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, '! Data Session opened');
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketGetSessionClosed(
    Sender  : TObject;
    ErrCode : word);
begin
{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, '! Data Session closed');
{$ENDIF}
{$IFDEF UseBandwidthControl}
    if Assigned(FBandwidthTimer) and FBandwidthTimer.Enabled then begin
        FBandwidthTimer.Enabled := FALSE;
        FBandwidthPaused := TRUE;
    end;
{$ENDIF}
    DestroyLocalStream;
    FFileReceived := TRUE;
    FError        := ErrCode;
    Next3GetAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketPutSessionAvailable(
    Sender  : TObject;
    ErrCode : word);
var
    aSocket : TSocket;
begin
{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, '! Data Session requested');
{$ENDIF}
    { Accept the incomming connection initiated by the FTP server for data }
    aSocket := FDataSocket.Accept;

    { Close the listening socket, we don't need it anymore }
    FDataSocket.Close;
{$IFDEF USE_SSL}                                                    // 12/15/05 Many properties missing!
    if (Self is TSslFtpClient) then begin      { V2.107 }
        TCustomSslWSocket(FDataSocket).SslMode   := sslModeClient;
        TCustomSslWSocket(FDataSocket).SslEnable := TSslFtpClient(Self).FProtDataFlag; { AG V7.03 }
        if TCustomSslWSocket(FDataSocket).SslEnable then begin
            TCustomSslWSocket(FDataSocket).SslContext          := FControlSocket.SslContext;
            TCustomSslWSocket(FDataSocket).OnSslVerifyPeer     := FControlSocket.OnSslVerifyPeer;
            TCustomSslWSocket(FDataSocket).OnSslHandshakeDone  := FControlSocket.OnSslHandshakeDone;
            TCustomSslWSocket(FDataSocket).OnSslCliGetSession  := FControlSocket.OnSslCliGetSession;
            TCustomSslWSocket(FDataSocket).OnSslCliNewSession  := FControlSocket.OnSslCliNewSession;
            TCustomSslWSocket(FDataSocket).OnSslCliCertRequest := FControlSocket.OnSslCliCertRequest;
        end;
    end;
{$ENDIF}

    { Reuse the socket for the data transmission }
    FDataSocket.OnSessionClosed  := DataSocketPutSessionClosed;
    FDataSocket.OnDataAvailable  := DataSocketPutDataAvailable;
    FDataSocket.OnDataSent       := DataSocketPutDataSent;
{   FDataSocket.OnDisplay        := FOnDisplay; } { Debugging only }
    FDataSocket.HSocket          := aSocket;
    if FDataSocket.SocketSndBufSize <> FDSocketSndBufSize then {AG V7.26}
        FDataSocket.SocketSndBufSize := FDSocketSndBufSize;    {AG V7.26}
    FDataSocket.ComponentOptions := [wsoNoReceiveLoop];   { 26/10/02 }

    { Be sure to gracefully close the socket }
    FDataSocket.LingerOnOff   := wsLingerOff;
    FDataSocket.LingerTimeout := 10;
    FDataSocket.SetLingerOption;
{   FStorAnswerRcvd := TRUE; } { V240 INSERTED line }
{$IFDEF UseBandwidthControl}
    FBandwidthCount := 0; // Reset byte counter
    if ftpBandwidthControl in FOptions then begin
        if not Assigned(FBandwidthTimer) then
            FBandwidthTimer := TIcsTimer.Create(Self);
        FBandwidthTimer.Enabled  := FALSE;
        FBandwidthTimer.Interval := FBandwidthSampling;
        FBandwidthTimer.OnTimer  := BandwidthTimerTimer;
        FBandwidthTimer.Enabled  := TRUE;
        // Number of bytes we allow during a sampling period
        FBandwidthMaxCount := Int64(FBandwidthLimit) * FBandwidthSampling div 1000;
        FBandwidthPaused   := FALSE;
    end;
{$ENDIF}
    FPutSessionOpened := TRUE;
    if FStorAnswerRcvd and (FStartTime = 0) then
        PostMessage(Handle, FMsg_WM_FTP_SENDDATA, 0, 0);

{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, '! Data Session opened');
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.WMFtpSendData(var msg: TMessage);
begin
    { Record the starting time }
    FStartTime := LongInt(IcsGetTickCount);
    FDurationMsecs := 0;  { V2.113 }

    { Send first data block }
    if not FDataSocketSentFlag then     { V7.11 }    
        DataSocketPutDataSent(FDataSocket, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure  TCustomFtpCli.WMFtpCloseDown(var msg: TMessage);      { V2.100 }
begin
    { see if last buffer has been fully sent, or given up waiting }
    if (FDataSocket.BufferedByteCount = 0) or 
       (FCloseEndTick < IcsGetTickCount) then begin
        FDataSocket.ShutDown(1); { 18/05/2005 was CloseDelayed; }
        FEofFlag := TRUE;
    end
    else if ((FControlSocket.State = wsConnected) and
             (FDataSocket.State    = wsConnected)) then
        PostMessage(Handle, FMSG_WM_FTP_CLOSEDOWN, 0, 0);  // not finished, call ourself again
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketPutDataSent(
    Sender  : TObject;
    ErrCode : word);
var
    Count : Integer;
begin
    if (FLocalStream = nil) or (not Progress) then
        Exit;
    if FLocalStream = nil then
        Exit;   { Could be set to nil by Progress function }

    if ErrCode <> 0 then begin
        TriggerDisplay('! Error sending data - ' + GetWinsockErr(ErrCode));
        FDataSocket.Close;
        Exit;
    end;

    if FEofFlag or (not FStorAnswerRcvd) or (not FPutSessionOpened) then begin
        Exit;
    end;

    if not FDataSocketSentFlag then    { V7.11 }    
        FDataSocketSentFlag := TRUE;

    try
        if FZStreamState = ftpZStateSaveComp then
            Count := FModeZStream.Read(FSendBuffer, SizeOf(FSendBuffer))  { angus 7.00 simplified }
         else
            Count := FLocalStream.Read(FSendBuffer, SizeOf(FSendBuffer));
{$IFNDEF NO_DEBUG_LOG}                                        { 2.104 }
        if CheckLogOptions(loProtSpecInfo) then
            DebugLog(loProtSpecInfo, 'DataSocketPutDataSent ' + IntToStr(Count));
{$ENDIF}
        if Count > 0 then begin
            FByteCount := FByteCount + Count;
            FDataSocket.Send(@FSendBuffer, Count);
{$IFDEF UseBandwidthControl}
            Inc(FBandwidthCount, Count);
            if ftpBandwidthControl in FOptions then begin
                if (FBandwidthCount > FBandwidthMaxCount) and
                   (not FBandwidthPaused) then begin
                    FBandwidthPaused := TRUE;
                    FDataSocket.Pause;
                end;
            end;
{$ENDIF}
        end
        else begin { EOF }
            {$IFNDEF VER80}
            { For an unknown reason, winsock need time to send last data }
            { buffer. Without this delay, we may end with a partial file }
            { transfer. See comments in DoPutAppendAsync function.       }
            { Normally using Linger option would handle this case. But   }
            { many winsock implementations will end with a 10055 error   }
            { after a lot of consecutive file transfers.                 }
            {  Sleep(100);  did not really work }

            { V2.100 new close down message which repeats until last buffer sent }
            FCloseEndTick := IcsGetTickCount + (FCloseEndSecs * 1000);  { but not forever }
            PostMessage(Handle, FMsg_WM_FTP_CLOSEDOWN, 0, 0);
            exit;
            {$ENDIF}
            FDataSocket.ShutDown(1); { 18/05/2005 was CloseDelayed; }
            FEofFlag := TRUE;
        end;
    except
        on E:Exception do begin
            TriggerDisplay('! Error reading file ' + E.ClassName + ': ' + E.Message);
            FDataSocket.Close;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketPutSessionClosed(
    Sender  : TObject;
    ErrCode : word);
begin
{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, '! Data Session closed');
{$ENDIF}
{$IFDEF UseBandwidthControl}
    if Assigned(FBandwidthTimer) and FBandwidthTimer.Enabled then begin
        FBandwidthTimer.Enabled := FALSE;
        FBandwidthPaused := TRUE;
    end;
{$ENDIF}
    { close the local file }
    DestroyLocalStream;
    FFileSent := TRUE;
    FError    := ErrCode;
    Next3PutAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketPutDataAvailable(
    Sender  : TObject;
    ErrCode : word);
var
    Buffer  : array [1..2048] of Byte;
    aSocket : TWSocket;
begin
    { We don't wants to receive data here because we are sending, not       }
    { receiving. But in order to not crash if we receive somthing, just     }
    { get it and do nothing with it !                                       }
    aSocket := Sender as TWSocket;
    aSocket.Receive(@Buffer[1], High(Buffer));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TransfertStats;
var
    Buffer   : String;
    BytesSec : Int64 ;
    Duration : Int64 ;  { V2.100 allow wrap at 49 days, don't show 0 secs or silly bps }
begin
    FStopTime := LongInt(IcsGetTickCount);
    Buffer    := IntToKByte(FByteCount) + 'bytes received/sent in ';
    if LongWord (FStopTime) >= LongWord (FStartTime) then   { V2.102 fix zero duration downloads }
        Duration := LongWord (FStopTime) - LongWord (FStartTime)
    else
        Duration := ($FFFFFFFF - LongWord (FStartTime)) + LongWord (FStopTime);
    if Duration < 5000 then
        Buffer := Buffer + IntToStr(Duration) + ' milliseconds'
    else begin
        Buffer := Buffer + IntToStr(Duration div 1000) + ' seconds';
    if FStopTime <> FStartTime then begin
        if FByteCount > 32767 then
                BytesSec := 1000 * (FByteCount div Duration)
        else
                BytesSec := (1000 * FByteCount) div Duration;
            Buffer := Buffer + ' (' + IntToKByte(BytesSec) + 'bytes/sec)';
    end;
    end;
    FDurationMsecs := Integer (Duration);  { V2.113 make it available to user }
    TriggerDisplay('! ' + Buffer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecGetAsync;
begin
    DoGetAsync(ftpGetAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecDirAsync;
begin
    DoGetAsync(ftpDirAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecLsAsync;
begin
    DoGetAsync(ftpLsAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecMlsdAsync;     { V2.90 }
begin
    DoGetAsync(ftpMlsdAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecSiteDmlsdAsync;     { V2.113 }
begin
    DoGetAsync(ftpSiteDmlsdAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecXDmlsdAsync;       { V7.01 }
begin
    DoGetAsync(ftpXDmlsdAsync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetShareMode(newValue : TFtpShareMode);
begin
{$WARNINGS OFF}
    case newValue of
        ftpShareCompat    : FShareMode := {$IFDEF MSWINDOWS} fmShareCompat {$ELSE} fmShareExclusive {$ENDIF};
        ftpShareExclusive : FShareMode := fmShareExclusive;
        ftpShareDenyWrite : FShareMode := fmShareDenyWrite;
        ftpShareDenyRead  : FShareMode := {$IFDEF MSWINDOWS} fmShareDenyRead {$ELSE} fmShareExclusive {$ENDIF};
        ftpShareDenyNone  : FShareMode := fmShareDenyNone;
    else
        FShareMode := fmShareExclusive;
    end;
{$WARNINGS ON}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetShareMode : TFtpShareMode;
begin
{$WARNINGS OFF}
    case FShareMode of
      {$IFDEF MSWINDOWS}
        fmShareCompat    : Result := ftpShareCompat;
      {$ENDIF}
        fmShareExclusive : Result := ftpShareExclusive;
        fmShareDenyWrite : Result := ftpShareDenyWrite;
      {$IFDEF MSWINDOWS}
        fmShareDenyRead  : Result := ftpShareDenyRead;
      {$ENDIF}
        fmShareDenyNone  : Result := ftpShareDenyNone;
    else
        Result := ftpShareExclusive;
    end;
{$WARNINGS ON}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetDisplayFileMode(NewValue : TFtpDisplayFileMode);
begin
    case NewValue of
        ftpLineByLine, ftpBinary : FDisplayFileMode := NewValue;
    else
        FDisplayFileMode := ftpLineByLine;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetDisplayFileMode : TFtpDisplayFileMode;
begin
    case FDisplayFileMode of
      ftpLineByLine: Result := ftpLineByLine;
      ftpBinary: Result := ftpBinary;
    else
      Result := ftpLineByLine;
    end;
end;

(*
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetConnectionType(NewValue: TFtpConnectionType);
begin
    { Must be disconnected to change the connection type }
    if FConnected then begin
        HandleError('FTP component connected');
        Exit;
    end;
    { Change connection type }
    case NewValue of
    ftpDirect: begin
                 FConnectionType                    := NewValue;
                 FControlSocket.SocksAuthentication := socksNoAuthentication;
                 FDataSocket.SocksAuthentication    := socksNoAuthentication;
               end;
    ftpProxy: begin
                FConnectionType                     := NewValue;
                FPassive                            := TRUE;
                FControlSocket.SocksAuthentication  := socksNoAuthentication;
                FDataSocket.SocksAuthentication     := socksNoAuthentication;
              end;
    ftpSocks4: begin
                 FConnectionType := NewValue;
                 FPassive        := TRUE;
                 with FControlSocket do begin
                     SocksLevel          := '4';
                     SocksAuthentication := socksAuthenticateUsercode;
                 end;
                 with FDataSocket do begin
                     SocksLevel          := '4';
                     SocksAuthentication := socksAuthenticateUsercode;
                 end;
               end;
    ftpSocks4A: begin
                  FConnectionType := NewValue;
                  FPassive        := TRUE;
                  with FControlSocket do begin
                      SocksLevel          := '4A';
                      SocksAuthentication := socksAuthenticateUsercode;
                  end;
                  with FDataSocket do begin
                      SocksLevel          := '4A';
                      SocksAuthentication := socksAuthenticateUsercode;
                  end;
                end;
    ftpSocks5: begin
                 FConnectionType := NewValue;
                 FPassive        := TRUE;
                 with FControlSocket do begin
                     SocksLevel          := '5';
                     SocksAuthentication := socksAuthenticateUsercode;
                 end;
                 with FDataSocket do begin
                   SocksLevel          := '5';
                   SocksAuthentication := socksAuthenticateUsercode;
                 end;
               end;
    else
        FConnectionType                    := ftpDirect;
        FControlSocket.SocksAuthentication := socksNoAuthentication;
        FDataSocket.SocksAuthentication    := socksNoAuthentication;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetConnectionType: TFtpConnectionType;
begin
    case FConnectionType of
    ftpDirect  : Result := ftpDirect;
    ftpProxy   : Result := ftpProxy;
    ftpSocks4  : Result := ftpSocks4;
    ftpSocks4A : Result := ftpSocks4A;
    ftpSocks5  : Result := ftpSocks5;
    else
        Result := ftpDirect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetSocksPassword(NewValue: String);
begin
    FControlSocket.SocksPassword := NewValue;
    FDataSocket.SocksPassword    := NewValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetSocksPassword: String;
begin
    Result := FControlSocket.SocksPassword;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetSocksPort(NewValue: String);
begin
    FControlSocket.SocksPort := NewValue;
    FDataSocket.SocksPort    := NewValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetSocksPort: String;
begin
    Result := FControlSocket.SocksPort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetSocksServer(const NewValue: String);
begin
    FControlSocket.SocksServer := NewValue;
    FDataSocket.SocksServer    := NewValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetSocksServer: String;
begin
    Result := FControlSocket.SocksServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetSocksUserCode(NewValue: String);
begin
    FControlSocket.SocksUserCode := NewValue;
    FDataSocket.SocksUserCode    := NewValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetSocksUserCode: String;
begin
    Result := FControlSocket.SocksUserCode;
end;
*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetPassive(NewValue: Boolean);
begin
    { Passive mode must not be changed if HTTP-proxy or Socks connection    }
    { type is selected. Most native FTP-proxies support both active and     }
    { passive mode.                                                         }
    case FConnectionType of
        ftpDirect, ftpProxy     : FPassive := NewValue;             { V7.22 }
        ftpSocks4, ftpSocks4A, 
        ftpSocks5, ftpHttpProxy : FPassive := TRUE;                 { V7.22 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketGetInit(const TargetPort, TargetIP : String);
begin
    FDataSocket.Port               := TargetPort;
    FDataSocket.Addr               := TargetIP; {ControlSocket.Addr;}
    FDataSocket.LocalAddr          := FLocalAddr; {bb}
    FDataSocket.LocalAddr6         := FLocalAddr6; { V8.01 }
    FDataSocket.OnSessionConnected := DataSocketGetSessionConnected;
    FDataSocket.LingerOnOff        := wsLingerOff;
    FDataSocket.LingerTimeout      := 0;
    FDataSocket.ComponentOptions   := [wsoNoReceiveLoop];   { 26/10/02 } { 2.109 }
{$IFDEF BUILTIN_THROTTLE}
    if ftpBandwidthControl in FOptions then begin
        FDataSocket.BandwidthLimit     := FBandwidthLimit;
        FDataSocket.BandwidthSampling  := FBandwidthSampling;
    end
    else
        FDataSocket.BandwidthLimit := 0;
{$ENDIF}
{ angus V7.00 always set proxy and SOCKS options before opening socket  }
    FDataSocket.SocksAuthentication := socksNoAuthentication;
    case FConnectionType of
        ftpSocks4:  FDataSocket.SocksLevel := '4';
        ftpSocks4A: FDataSocket.SocksLevel := '4A';
        ftpSocks5:  FDataSocket.SocksLevel := '5';
    end;
    if FConnectionType in [ftpSocks4, ftpSocks4A, ftpSocks5] then begin
        FDataSocket.SocksAuthentication  := socksAuthenticateUsercode;
        FDataSocket.SocksServer          := FSocksServer;
        FDataSocket.SocksPort            := FSocksPort;
        FDataSocket.SocksUsercode        := FSocksUsercode;
        FDataSocket.SocksPassword        := FSocksPassword;
    end
    else if FConnectionType = ftpHttpProxy then begin { V7.18 }
        FDataSocket.HttpTunnelAuthType := FControlSocket.HttpTunnelAuthType;
        FDataSocket.HttpTunnelServer   := FHttpTunnelServer;
        FDataSocket.HttpTunnelPort     := FHttpTunnelPort;
        FDataSocket.HttpTunnelUsercode := FHttpTunnelUserCode;
        FDataSocket.HttpTunnelPassword := FHttpTunnelPassword;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetZlibCacheFileName(const S : String) : String;  { V2.113 }
var
    I : Integer;
    Ticks: String;
begin
    Result := AnsiLowercase (S);
    if Length(Result) = 0 then Result := 'temp'; { might be saving to stream only }
    for I := 1 to Length(Result) do begin
        if (Result [I] = '\') or (Result [I] = '.') or
                           (Result [I] = ':') then Result[I] := '_';
    end;
    Ticks := IntToStr(IcsGetTickCountX);  { now make it unique by adding some ms }
    I := Length(Ticks);
    if I < 6 then Ticks := '123' + Ticks; { if windows running short }
    Result := Result + '_' + Copy (Ticks, I-6, 6) + '.zlib';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Receive a file or a directory list of a file list                         }
procedure TCustomFtpCli.DoGetAsync(RqType : TFtpRequest);
var
    Temp       : String;
    I {, MaxWbits} : Integer;
    TargetPort : WORD;    { 10/30/99 }
    TargetIP   : String;
    NewPos     : TFtpBigInt;           { V2.108 }
    Delim      : Char;
    DelimCnt, N: Integer;
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, 'Start command, Req=Get - '  + FLocalFileName);  { V8.03 }
{$ENDIF}
    if not FConnected then begin
        HandleError(FGetCommand + ': not connected');
        Exit;
    end;

    { If no filename was assigned, check if maybe we wanna view it, }
    { meaning - FDisplayFileFlag }
    if (Length(FLocalFileName) <= 0) and
       not (FDisplayFileFlag or FStreamFlag) then begin
        HandleError('LocalFileName empty');
        Exit;
    end;

    if not FHighLevelFlag then
        FRequestType := RqType;

    case RqType of
    ftpGetAsync:       FGetCommand := 'RETR';
    ftpDirAsync:       FGetCommand := 'LIST';
    ftpLsAsync:        FGetCommand := 'NLST';
    ftpMlsdAsync:      FGetCommand := 'MLSD';         { V2.90 }
    ftpSiteDmlsdAsync: FGetCommand := 'SITE DMLSD';   { V2.113 }
    ftpXDmlsdAsync:    FGetCommand := 'XDMLSD';       { V7.01 }
    end;

  { V2.111 never resume directory listings }
    if RqType <> ftpGetAsync then FResumeAt := 0;

    FServerSaidDone    := FALSE;
    FFileReceived      := FALSE;
    FRequestDoneFlag   := FALSE;
    FStartTime         := 0;
    FByteCount         := 0;
    FDurationMsecs     := 0;  { V2.113 }
    FError             := 0;

    FDataSocket.OnSessionAvailable := DataSocketGetSessionAvailable;

    { open the destination file }
    { Don't open a file if we're on FDisplayFileFlag }
    if not FDisplayFileFlag then
    try
        DestroyLocalStream;
        if FResumeAt <= 0 then begin
            if not Assigned(FLocalStream) and not FStreamFlag then begin
                FLocalStream := OpenFileStream(FLocalFileName, fmCreate);   { V2.105 }
                if FShareMode <> 0 then begin
                    { Not default mode, need to close and reopen file with }
                    { the given mode                                       }
                    FreeAndNil(FLocalStream);
                    FLocalStream := OpenFileStream(FLocalFileName, fmOpenWrite + FShareMode);
                end;
            end;
        end
        else begin
            if not Assigned(FLocalStream) and not FStreamFlag then begin  { V2.112 }
                FLocalStream := OpenFileStream (FLocalFileName, fmOpenWrite + FShareMode);
                { We MUST check for file size >= RestartPos since Seek in any      } { V2.108 }
                { write-mode may write to the stream returning always the correct  }
                { new position.                                                    }
                NewPos := GetFileSize(FLocalFileName);                    { V2.108 }
                if FResumeAt <= NewPos then                               { V2.108 }
                    NewPos := FLocalStream.Seek(FResumeAt, soBeginning);
                if NewPos <> FResumeAt then begin
                    FLastResponse := 'Unable to set resume position in local file';
                    FStatusCode   := 550;
                    SetErrorMessage;
                    FDataSocket.Close;
                    FRequestResult := FStatusCode;
                    TriggerRequestDone(FRequestResult);
                    exit;
                end;
            end;
        end;
        if FCurrTransMode = ftpTransModeZDeflate then begin    { V1.103 }
            zlibProblemString := '';
     //     FModeZStream := TMemoryStream.Create;  { V2.113 memorystream very slow, use filesteam }
            FZCompFileName := FZlibWorkDir + GetZlibCacheFileName(FLocalFileName);
            FModeZStream := OpenFileStream(FZCompFileName, fmCreate);

         // option 1 - save all data into stream, decompress on close
            FZStreamState := ftpZStateSaveDecom;

         // option 2 - decode immediately (still need to save a little - not done yet)
        end;
    except
        on E:Exception do begin
            if Assigned (FModeZStream) then FModeZStream.Destroy;
            FModeZStream := nil;
            try
                if FileExists(FZCompFileName) then DeleteFile (FZCompFileName);  { V1.113 }
            except
            end;
            FLastResponse := 'Unable to open local file ' +
                                           FLocalFileName + ': ' + E.Message; { V2.101}
            FStatusCode   := 550;
            SetErrorMessage;
            FDataSocket.Close;
            FRequestResult := FStatusCode;
            TriggerRequestDone(FRequestResult);
            exit;
        end;
    end;

    if FPassive then begin
        FDataSocket.SocketFamily := FControlSocket.CurrentSocketFamily;
        if FControlSocket.CurrentSocketFamily = sfIPv4 then
        begin
            Temp := FPasvResponse;
            Delete(Temp, 1, Pos('(', Temp));

            TargetIP := '';
            for I := 1 to 4 do begin
                TargetIP := TargetIP + Copy(Temp, 1, Pos(',', Temp) - 1) + '.';
                Delete(Temp, 1, Pos(',', Temp));
            end;
            TargetIP := Copy(TargetIP, 1, Length(TargetIP) - 1);

            TargetPort := StrToInt(Copy(Temp, 1, Pos(',', Temp) - 1)) * 256;
            Delete(Temp, 1, Pos(',', Temp));
            TargetPort := TargetPort + StrToInt(Copy(Temp, 1, Pos(')', Temp) - 1));

            DataSocketGetInit(IntToStr(TargetPort), TargetIP);
        end
        else begin  { EPSV IPv6 }
            { Response like: "Entering Extended Passive Mode (|||6446|)" }
            Delim := #0; DelimCnt := 0; Temp := '0'; N := 1;
            TargetIP := WSocketIPv6ToStr(PSockAddrIn6(@FControlSocket.sin6));
            for I := 1 to Length(FPasvResponse) do
            begin
                if FPasvResponse[I] = '(' then
                    Delim := FPasvResponse[I + 1]
                else if FPasvResponse[I] = Delim then
                begin
                    Inc(DelimCnt);
                    if DelimCnt = 3 then
                        N := I + 1
                    else if DelimCnt = 4 then
                    begin
                        Temp := Copy(FPasvResponse, N, (I - N));
                        Break;
                    end;
                end;
            end;
            DataSocketGetInit(Temp, TargetIP);
        end;

{$IFNDEF NO_DEBUG_LOG}                                                { 2.104 }
        __DataSocket := FDataSocket;    { V2.107 }
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, '! Data Socket Connect');
{$ENDIF}
        try
            FDataSocket.Connect;
        except
            on E:Exception do begin
                FLastResponse := '550 ' + E.ClassName + ': ' + E.Message;
                FStatusCode   := 550;
                SetErrorMessage;
                FDataSocket.Close;
                FRequestResult := FStatusCode;
                TriggerRequestDone(FRequestResult);
                exit;
            end;
        end;
    end;

    StateChange(ftpWaitingResponse);
    FNext := Next1GetAsync;
    if Length(FHostFileName) > 0 then
        SendCommand(FGetCommand + ' ' + FHostFileName)
    else
        SendCommand(FGetCommand);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We comes here when receiving the response for the RETR command we sent    }
procedure TCustomFtpCli.Next1GetAsync;
begin
    DisplayLastResponse;
    GetInteger(@FLastResponse[1], FStatusCode);
    if not (((FStatusCode div 10) = 15) or   { Accept range 150-159 }
            (FStatusCode = 125)) then begin  { Accept code 125      }
        SetErrorMessage;
        FNext := nil;
        FDataSocket.Close;
        DestroyLocalStream;
        { Reset the starting position }
        FResumeAt      := 0;
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;
    FNext := Next2GetAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We comes here when the FTP server has sent the file we asked to GET       }
procedure TCustomFtpCli.Next2GetAsync;
begin
    DisplayLastResponse;
    GetInteger(@FLastResponse[1], FStatusCode);
    if not ((FStatusCode = 125) or (FStatusCode = 226) or
            (FStatusCode = 250)) then begin
        SetErrorMessage;
        DestroyLocalStream;
        FResumeAt := 0;        { V2.111 clear starting position }
        FDataSocket.Close;
        TriggerDisplay('! RETR/LIST/NLST Failed');
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;
    FServerSaidDone := TRUE;
    Next3GetAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We comes here either if the file has been received of the FTP server has  }
{ his response.                                                             }
procedure TCustomFtpCli.Next3GetAsync;
begin
{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecDump) then
        DebugLog(loProtSpecDump, '! Next3GetAsync');
{$ENDIF}
    if (not FServerSaidDone) or (not FFileReceived) then
        Exit;

    { Display statistics }
    TransfertStats;

    { Reset the starting position }
    FResumeAt      := 0;
    FRequestResult := FError;
    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecPutAsync;
begin
    FAppendFlag  := FALSE;
    FRequestType := ftpPutAsync;
    DoPutAppendAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecAppendAsync;
begin
    FAppendFlag  := TRUE;
    FRequestType := ftpAppendAsync;
    DoPutAppendAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketPutAppendInit(const TargetPort, TargetIP : String);
begin
    FDataSocket.Port               := TargetPort;
    FDataSocket.Addr               := TargetIP; {ControlSocket.Addr;}
    FDataSocket.LocalAddr          := FLocalAddr; {bb}
    FDataSocket.LocalAddr6         := FLocalAddr6; { V8.01 }
    FDataSocket.OnSessionConnected := DataSocketPutSessionConnected;
    { Normally we should use LingerOn with a timeout. But doing so will }
    { often result in error 10055 triggered after a lot of consecutive  }
    { file transfers. There is code in DataSocketPutDataSent to make    }
    { sure last packet is sent completely.                              }
    FDataSocket.LingerOnOff        := wsLingerOff;
    FDataSocket.LingerTimeout      := 0;
    FDataSocket.ComponentOptions   := [wsoNoReceiveLoop];   { 26/10/02 }
    FDataSocketSentFlag            := FALSE;          { V7.11 }    
{$IFDEF BUILTIN_THROTTLE}
    if ftpBandwidthControl in FOptions then begin
        FDataSocket.BandwidthLimit     := FBandwidthLimit;
        FDataSocket.BandwidthSampling  := FBandwidthSampling;
    end
    else
        FDataSocket.BandwidthLimit := 0;
{$ENDIF}
{ angus V7.00 always set proxy and SOCKS options before opening socket  }
    FDataSocket.SocksAuthentication := socksNoAuthentication;
    case FConnectionType of
        ftpSocks4:  FDataSocket.SocksLevel := '4';
        ftpSocks4A: FDataSocket.SocksLevel := '4A';
        ftpSocks5:  FDataSocket.SocksLevel := '5';
    end;
    if FConnectionType in [ftpSocks4, ftpSocks4A, ftpSocks5] then begin
        FDataSocket.SocksAuthentication  := socksAuthenticateUsercode;
        FDataSocket.SocksServer          := FSocksServer;
        FDataSocket.SocksPort            := FSocksPort;
        FDataSocket.SocksUsercode        := FSocksUsercode;
        FDataSocket.SocksPassword        := FSocksPassword;
    end
    else if FConnectionType = ftpHttpProxy then begin { V7.18 }
        FDataSocket.HttpTunnelAuthType := FControlSocket.HttpTunnelAuthType;
        FDataSocket.HttpTunnelServer   := FHttpTunnelServer;
        FDataSocket.HttpTunnelPort     := FHttpTunnelPort;
        FDataSocket.HttpTunnelUsercode := FHttpTunnelUserCode;
        FDataSocket.HttpTunnelPassword := FHttpTunnelPassword;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DoPutAppendAsync;
var
    Temp        : String;
    I           : Integer;
    TargetPort  : WORD;   { 10/30/99 }
    TargetIP    : String;
    bCancel     : Boolean;
    NewPos      : TFtpBigInt;
    Uploadsize  : TFtpBigInt;
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, 'Start command, Req=Put/Append - '  + FLocalFileName);  { V8.03 }
{$ENDIF}
    if not FConnected then begin
        HandleError('STOR/APPE: not connected');
        Exit;
    end;

    if (not FStreamFlag) and (Length(FLocalFileName) <= 0) then begin
        HandleError('LocalFileName empty');
        Exit;
    end;

    FServerSaidDone    := FALSE;
    FFileSent          := FALSE;
    FRequestDoneFlag   := FALSE;
    FPutSessionOpened  := FALSE;
    FStorAnswerRcvd    := FALSE;
    FStartTime         := 0;
    FDurationMsecs     := 0;  { V2.113 }
    FByteCount         := 0;
    FError             := 0;

    bCancel := FALSE;
    TriggerReadyToTransmit(bCancel);
    if bCancel then begin
        FErrorMessage := '426 Transmit cancelled by application';
        FStatusCode   := 426;
        TriggerDisplay('! ' + FErrorMessage);
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    FDataSocket.OnSessionAvailable := DataSocketPutSessionAvailable;
    
    { open the local source file }
    try
        { Be sure to have previous instance closed }
        DestroyLocalStream;
        FEofFlag     := FALSE;
        NewPos       := FResumeAt;
        if not Assigned(FLocalStream) and not FStreamFlag then begin
            FLocalStream := OpenFileStream(FLocalFileName,
                                               fmOpenRead + FShareMode); { V2.113 }
        end;
        if FResumeAt > 0 then
            NewPos := FLocalStream.Seek(FResumeAt, soBeginning);
        if NewPos <> FResumeAt then begin
            FLastResponse := 'Unable to set resume position in local file';
            FStatusCode   := 550;
            SetErrorMessage;
            FDataSocket.Close;
            FRequestResult := FStatusCode;
            TriggerRequestDone(FRequestResult);
            exit;
        end;
    except
        on E:Exception do begin
            FLastResponse := 'Unable to open local file ' +
                                           FLocalFileName + ': ' + E.Message; { V2.101}
            FStatusCode   := 426;
            SetErrorMessage;           { 27/03/07 }
            TriggerDisplay('! ' + FErrorMessage);
            FDataSocket.Close;
            FRequestResult := FStatusCode;
            TriggerRequestDone(FRequestResult);
            Exit;
        end;
    end;

    Uploadsize := FLocalStream.Size - FLocalStream.Position;
    if FCurrTransMode = ftpTransModeZDeflate then begin    { V1.103 }
        zlibProblemString := '';
   //   FModeZStream := TMemoryStream.Create;   { V1.113 memorystream very slow, use filestream }
        FZCompFileName := FZlibWorkDir + GetZlibCacheFileName(FLocalFileName);
        FModeZStream := OpenFileStream(FZCompFileName, fmCreate);
     { option 1 - compress data into stream, send stream  }
        FZStreamState := ftpZStateSaveComp;
        try
            ZlibCompressStreamEx (FLocalStream, FModeZStream, Z_BEST_SPEED, zsZLib,
                                         false, Self, ZlibOnProgress);  { V1.113 added callback }
            FModeZStream.Position := 0 ;
            TriggerDisplay('! Upload Size, compressed ' + IntToKByte
                      (FModeZStream.Size) + ' bytes, Uncompressed ' +
                                            IntToKByte (Uploadsize) + 'bytes') ;
        except
            on E:Exception do begin
                if Assigned (FModeZStream) then FModeZStream.Destroy;
                FModeZStream := nil;
                try
                    if FileExists(FZCompFileName) then DeleteFile (FZCompFileName);  { V1.113 }
                except
                end;
                if Assigned(FLocalStream) and (FStreamFlag = FALSE) then begin   { V1.113 }
                    FreeAndNil(FLocalStream);
                end;
                HandleError('Failed to Compress Stream - ' + E.Message);
                FStatusCode   := 426;
                FDataSocket.Close;
                FRequestResult := FStatusCode;
                TriggerRequestDone(FRequestResult);
            end;
        end;

     { option 2 - compress during send - not implemented yet }
    end
    else
        TriggerDisplay('! Upload Size ' + IntToKByte (Uploadsize)) ;

    if FPassive then begin
        Temp := FPasvResponse;  { 26/12/99 }
        Delete(Temp, 1, Pos('(', Temp));
        TargetIP := '';
        for I := 1 to 4 do begin
            TargetIP := TargetIP + Copy(Temp, 1, Pos(',', Temp) - 1) + '.';
            Delete(Temp, 1, Pos(',', Temp));
        end;
        TargetIP := Copy(TargetIP, 1, Length(TargetIP) - 1);

        TargetPort := StrToInt(Copy(Temp, 1, Pos(',', Temp) - 1)) * 256;
        Delete(Temp, 1, Pos(',', Temp));
        TargetPort := TargetPort + StrToInt(Copy(Temp, 1, Pos(')', Temp) - 1));

        DataSocketPutAppendInit(IntToStr(TargetPort), TargetIP);
        try
            FDataSocket.Connect;
        except
            on E:Exception do begin
                FLastResponse := '426 ' + E.ClassName + ': ' + E.Message;
                FStatusCode   := 426;
                SetErrorMessage;
                FDataSocket.Close;
                FRequestResult := FStatusCode;
                TriggerRequestDone(FRequestResult);
                Exit;
            end;
        end;
        Exit;
    end;

    StateChange(ftpWaitingResponse);
    FNext := Next1PutAsync;

    if FAppendFlag then
        SendCommand('APPE ' + FHostFileName)
    else
        SendCommand('STOR ' + FHostFileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We comes here when receiving the response for the STOR command we sent    }
procedure TCustomFtpCli.Next1PutAsync;
var
    p : PChar;
begin
    DisplayLastResponse;
    if not IsDigit(FLastResponse[1]) then
        Exit; { Continuation line, nothing to do }
    p := GetInteger(@FLastResponse[1], FStatusCode);
    if p^ = '-' then
        Exit; { Continuation line, nothing to do }

    if not ((FStatusCode = 150) or (FStatusCode = 125)) then begin
        SetErrorMessage;
        FNext := nil;
        FDataSocket.Close;
        DestroyLocalStream;
        { Reset the starting position }
        FResumeAt      := 0;
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    if FPassive then begin
        { Send the first data block }
{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, '! Send first block');
{$ENDIF}
        FStorAnswerRcvd        := TRUE;
        FDataSocket.OnDataSent := DataSocketPutDataSent;
        DataSocketPutDataSent(FDataSocket, 0);
    end
    else begin
        { V240 FStorAnswerRcvd := TRUE; }
        FStorAnswerRcvd := TRUE;
        if FPutSessionOpened and (FStartTime = 0) then
            PostMessage(Handle, FMsg_WM_FTP_SENDDATA, 0, 0);
    end;


    FNext := Next2PutAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We comes here when the FTP server has received the file we sent (STOR)    }
procedure TCustomFtpCli.Next2PutAsync;
var
    p : PChar;
begin
    DisplayLastResponse;
    if not IsDigit(FLastResponse[1]) then
        Exit; { Continuation line, nothing to do }
    p := GetInteger(@FLastResponse[1], FStatusCode);
    if p^ = '-' then
        Exit; { Continuation line, nothing to do }
    if not ((FStatusCode = 226) or (FStatusCode = 250)) then begin
        SetErrorMessage;
        DestroyLocalStream;
        FDataSocket.Close;
        TriggerDisplay('! STOR Failed');
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;
    FServerSaidDone := TRUE;
    Next3PutAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We comes here when the file has been sent or when the FTP server tell us  }
{ he recived the file.                                                      }
procedure TCustomFtpCli.Next3PutAsync;
begin
{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecDump) then
        DebugLog(loProtSpecDump, '! Next3PutAsync');
{$ENDIF}
    if (not FServerSaidDone) or (not FFileSent) then
        Exit;

    { Display statistics }
    TransfertStats;

    { Reset the starting position }
    FResumeAt      := 0;
    FRequestResult := FError;
    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.PortAsync;
type
    T4Bytes = array[0..3] of Byte;
    P4Bytes = ^T4Bytes;
var
    Msg          : String;
    saddr        : TSockAddrIn6;
    saddrlen     : Integer;
    DataPort     : LongWord;  { 10/30/99 }
    IPAddr       : TInAddr;
    StartDataPort: LongWord;
begin
    if not FConnected then begin
        HandleError('FTP component not connected');
        Exit;
    end;
    { Makes the data socket listening for data connection }
    FDataSocket.Proto              := 'tcp';
    if FControlSocket.CurrentSocketFamily = sfIPv6 then
        FDataSocket.Addr := ICS_ANY_HOST_V6
    else
        FDataSocket.Addr := ICS_ANY_HOST_V4;
    FDataSocket.Port               := AnsiChar('0');        { IPPORT_ANY }
    FDataSocket.OnSessionAvailable := nil;
    FDataSocket.OnSessionClosed    := nil;
    FDataSocket.OnDataAvailable    := nil;
    FDataSocketSentFlag            := FALSE;     { V7.11 }
{$IFDEF BUILTIN_THROTTLE}
    if ftpBandwidthControl in FOptions then begin
        FDataSocket.BandwidthLimit     := FBandwidthLimit;
        FDataSocket.BandwidthSampling  := FBandwidthSampling;
    end
    else
        FDataSocket.BandwidthLimit := 0;
{$ENDIF}

    if FPassive then
        DataPort := 0    { Not needed, makes compiler happy }
    else begin
        if (ftpFctGet in FFctSet) or (ftpFctDir in FFctSet) or  {G.B. 2002/07/12}
           (ftpFctMlsd in FFctSet) or (ftpFctSiteDmlsd in FFctSet) then    { V2.90, V2.113 }
            FDataSocket.OnSessionAvailable := DataSocketGetSessionAvailable
        else if ftpFctPut in FFctSet then
            FDataSocket.OnSessionAvailable := DataSocketPutSessionAvailable;
        FDataSocket.LingerOnOff        := wsLingerOn;
        FDataSocket.LingerTimeout      := 10;

        if (FDataPortRangeStart = 0) and (FDataPortRangeEnd = 0) then begin
            FDataSocket.Listen;
            { Get the port number as assigned by Windows }
            saddrLen  := SizeOf(saddr);
            FDataSocket.GetSockName(PSockAddrIn(@saddr)^, saddrLen);
            DataPort  := WSocket_ntohs(saddr.sin6_port);
        end
        else begin
            { We use a data port range. Check if the range is valid }
            if FDataPortRangeStart > FDataPortRangeEnd then begin
                HandleError('DataPortRangeEnd must be greater than DataPortRangeStart');
                Exit;
            end;
            if (FLastDataPort < FDataPortRangeStart) or
               (FLastDataPort > FDataPortRangeEnd) then
                FLastDataPort := FDataPortRangeStart;
            DataPort      := FLastDataPort;
            StartDataPort := DataPort;
            while TRUE do begin
                FDataSocket.Port := IntToStr(DataPort);
                try
                    FDataSocket.Listen;
                    break;                { Found a free port }
                except
                    if FDataSocket.LastError = WSAEADDRINUSE then begin
                        DataPort := DataPort + 1;
                        if DataPort > FDataPortRangeEnd then
                            DataPort := FDataPortRangeStart;
                        if DataPort = StartDataPort then begin
                            HandleError('All ports in DataPortRange are in use');
                            Exit;
                        end;
                    end
                    else begin
                        HandleError('Data connection winsock bind failed - ' +
                                    GetWinsockErr(FDataSocket.LastError));
                        Exit;
                    end;
                end;
            end;
            FLastDataPort := DataPort + 1;
            if FLastDataPort > FDataPortRangeEnd then
                FLastDataPort := FDataPortRangeStart;
        end;
    end;

    { Get our IP address from our control socket }
    saddrlen := SizeOf(saddr);
    FControlSocket.GetSockName(PSockAddrIn(@saddr)^, saddrlen);
    IPAddr   := PSockAddrIn(@saddr).sin_addr;

    { Strange behaviour of PWS (FrontPage 97 Web Server for W95) }
    { which do not like effective address when localhost is used }
    if FPassive then begin
        if saddr.sin6_family = AF_INET6 then
            Msg := 'EPSV'
        else
            Msg := 'PASV';
    end
    else begin
        if saddr.sin6_family = AF_INET6 then
        begin
            Msg := 'EPRT |2|' +
                   WSocketIPv6ToStr(PIcsIPv6Address(@saddr.sin6_addr)^) +
                   '|' + IntToStr(DataPort) + '|';
        end
        else
        if WSocketIsIPv4(FExternalIPv4) then                           { V8.02 }
            Msg := Format('PORT %s,%d,%d',                             { V8.02 }
                          [StringReplace(FExternalIPv4, '.', ',', [rfReplaceAll]),
                           IcsHiByte(DataPort),
                           IcsLoByte(DataPort)])
        else
        if FControlSocket.sin.sin_addr.s_addr = WSocket_htonl($7F000001) then
            Msg := Format('PORT 127,0,0,1,%d,%d',
                          [IcsHiByte(DataPort),
                           IcsLoByte(DataPort)])
        else
          {$IFDEF MSWINDOWS}
            Msg := Format('PORT %d,%d,%d,%d,%d,%d',
                          [ord(IPAddr. S_un_b.s_b1),
                           ord(IPAddr.S_un_b.s_b2),
                           ord(IPAddr.S_un_b.s_b3),
                           ord(IPAddr.S_un_b.s_b4),
                           IcsHiByte(DataPort),
                           IcsLoByte(DataPort)]);
          {$ENDIF}
          {$IFDEF POSIX}
            Msg := Format('PORT %d,%d,%d,%d,%d,%d',
                          [P4Bytes(@IPAddr.s_addr)^[0],
                           P4Bytes(@IPAddr.s_addr)^[1],
                           P4Bytes(@IPAddr.s_addr)^[2],
                           P4Bytes(@IPAddr.s_addr)^[3],
                           IcsHiByte(DataPort),
                           IcsLoByte(DataPort)]);
          {$ENDIF}
    end;

    FByteCount := 0;
    FFctPrv    := ftpFctPort;
    if saddr.sin6_family = AF_INET6 then
        ExecAsync(ftpPortAsync, Msg, [200, 229], nil)
    else
        ExecAsync(ftpPortAsync, Msg, [200, 227], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ControlSocketDnsLookupDone(
    Sender  : TObject;
    ErrCode : Word);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, 'Control DNS Lookup Done - '  + FControlSocket.DnsResult);  { V8.03 }
{$ENDIF}
    if ErrCode <> 0 then begin
        FLastResponse  := '500 DNS lookup error - ' + GetWinsockErr(ErrCode) ;
        FStatusCode    := 500;
        FRequestResult :=  FStatusCode;    { 21/05/99 }
        SetErrorMessage;
        TriggerRequestDone(ErrCode);
    end
    else begin
        FDnsResult               := FControlSocket.DnsResult;
        FControlSocket.Addr      := FDnsResult;
        FControlSocket.LocalAddr := FLocalAddr; {bb}
        FControlSocket.LocalAddr6 := FLocalAddr6; { V8.01 }
        FControlSocket.Proto     := 'tcp';
{$IFDEF USE_SSL}
        FControlSocket.SslEnable := FALSE;
{$ENDIF}
        if (FConnectionType = ftpProxy) and (FProxyPort <> '') then
            FControlSocket.Port  := FProxyPort
        else
            FControlSocket.Port  := FPort;
{       FControlSocket.OnDisplay := FOnDisplay; } { Debugging only }
        StateChange(ftpWaitingBanner);
        try
            FControlSocket.Connect;
        except
            on E:Exception do begin
                FLastResponse := '500 ' + E.ClassName + ': ' + E.Message;
                FStatusCode   := 500;
                FRequestResult :=  FStatusCode;    { 21/05/99 }
                SetErrorMessage;
                TriggerRequestDone(FStatusCode);
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.HandleHttpTunnelError(
    Sender                : TObject;
    ErrCode               : Word;
    TunnelServerAuthTypes : THttpTunnelServerAuthTypes;
    const Msg             : String);
begin
    FLastResponse := Msg;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.HandleSocksError(
    Sender  : TObject;
    ErrCode : Integer;
    Msg     : String);
begin
    FLastResponse := Msg;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ControlSocketSessionConnected(Sender: TObject; ErrCode: Word);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, 'Control Socket Connect, error='  + IntToStr (ErrCode));  { V8.03 }
{$ENDIF}
    { Do not trigger the client SessionConnected from here. We must wait }
    { to have received the server banner.                                }
    if ErrCode <> 0 then begin
      {$IFDEF POSIX}
        if (ErrCode <= WSAELAST) then
      {$ELSE}
        if (ErrCode >= WSABASEERR) and (ErrCode < ICS_SOCKS_BASEERR) then
      {$ENDIF}
            FLastResponse  := '500 Connect error - ' + GetWinsockErr(ErrCode)
        else if WSocketIsProxyErrorCode(ErrCode) then
            FLastResponse  := '500 Connect error - ' + FLastResponse +
                              ' (#' + IntToStr(ErrCode) + ')'
        else
            FLastResponse  := '500 Connect Unknown Error (#' +
                              IntToStr(ErrCode) + ')';
        FStatusCode    := 500;
        FRequestResult := FStatusCode;  { Heedong Lim, 05/14/1999 }
        SetErrorMessage; { Heedong Lim, 05/14/1999 }
        FNextRequest   := nil;
        TriggerRequestDone(ErrCode);
        FControlSocket.Close;
        StateChange(ftpReady);
    end
    else
        FConnected := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ControlSocketDataAvailable(Sender: TObject; ErrCode: Word);
var
    Len  : Integer;
    I, J : Integer;
    p    : PChar;
    Feat : String;
    ACodePage : LongWord;
    RawResponse: AnsiString;  { V7.01 }{ AG V7.02 we no not need RawByteString here }
const
    NewLine =  #13#10 ;
begin
    Len := FControlSocket.Receive(@FReceiveBuffer[FReceiveLen],
                                  SizeOf(FReceiveBuffer) - FReceiveLen - 1);

    if FRequestType = ftpRqAbort then
        Exit;

    if Len = 0 then begin
        { Remote has closed. We will soon receive FD_CLOSE (OnSessionClosed) }
        { FControlSocket.Close;                                              }
        Exit;
    end;
    if Len < 0 then
        Exit;

    FReceiveBuffer[FReceiveLen + Len] := #0;
    FReceiveLen := FReceiveLen + Len;

    while FReceiveLen > 0 do begin
        if ftpAcceptLF in FOptions then begin
            I := Pos(AnsiChar(10), FReceiveBuffer);
            J := I;
        end
        else begin
            I := Pos(AnsiString(#13#10), FReceiveBuffer);
            J := I + 1;
        end;
        if I <= 0 then
            break;
        if I > FReceiveLen then
            break;
        RawResponse := Copy(FReceiveBuffer, 1, I);   { V7.01 keep it write to stream  }
        { Remove trailing control chars, V7.01 before translating UTF8 to Unicode or ANSI  }
        while (Length(RawResponse) > 0) and
              IsCRLF(RawResponse[Length(RawResponse)]) do
             SetLength(RawResponse, Length(RawResponse) - 1);
        { Auto-detect UTF-8 if Option is set }
        if (FCodePage <> CP_UTF8) and (ftpAutoDetectCodePage in FOptions) and
           (CharsetDetect(RawResponse) = cdrUtf8) then          { AG V7.02 }
            ACodePage := CP_UTF8
        else
            ACodePage := FCodePage;
        { translate UTF-8 or ANSI to local codepage }
    {$IFDEF COMPILER12_UP}
        FLastResponse := AnsiToUnicode(RawResponse, ACodePage); { AG V7.02 }
    {$ELSE}
        FLastResponse := ConvertCodepage(RawResponse, ACodePage, CP_ACP);
    {$ENDIF}
        { V2.113 don't save SITE list commands, too long }
        if not (FFctPrv in [ftpFctSiteCmlsd, ftpFctXCmlsd, ftpFctSiteIndex]) then begin

            { V2.90 some FTP responses are multiline, welcome banner, FEAT  }
            { command, keep them all but do not exceed 64KB to avoid DOS    }
            if LongInt(Length(FLastMultiResponse)) < 65536 then
                FLastMultiResponse := FLastMultiResponse + FLastResponse + #13#10;
        end;
        TriggerResponse;         { V7.08 }

{$IFNDEF NO_DEBUG_LOG}                                             { 2.105 }
    if CheckLogOptions(loProtSpecDump) then
        DebugLog(loProtSpecDump, '>|' + FLastResponse + '|' + #13#10);
{$ENDIF}
        FReceiveLen := FReceiveLen - J;
        if FReceiveLen > 0 then
            Move(FReceiveBuffer[J], FReceiveBuffer[0], FReceiveLen + 1)
        else if FReceiveLen < 0 then
            FReceiveLen := 0;

        if FState = ftpWaitingBanner then begin
            DisplayLastResponse;
            if (FLastResponse = '') or                       { 15/02/03 }
               (not IsDigit(FLastResponse[1])) then
                Continue;  { Continuation line, ignore }
            p := GetInteger(@FLastResponse[1], FStatusCode);
            if p^ = '-' then
                Continue;  { Continuation line, ignore }
            if FStatusCode <> 220 then begin
                SetErrorMessage;
                FRequestResult := FStatusCode;
                FControlSocket.Close;
                Exit;
            end;

            StateChange(ftpConnected);
            if Assigned(FOnSessionConnected) then
                FOnSessionConnected(Self, ErrCode);

            if Assigned(FWhenConnected) then
                FWhenConnected
            else begin
                TriggerRequestDone(0);
            end;
        end
        else if FState = ftpWaitingResponse then begin
            if FFctPrv in [ftpFctFeat] then begin       { V2.90 supported extensions }
                Feat := Trim (FLastResponse);
                if (Pos ('MDTM YYYYMMDDHHMMSS' {[+-TZ]'}, Feat) = 1) then  { V7.01 skip TZ part in case it disappears }
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatMDTMYY];
                if Feat = 'MDTM' then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatMDTM];
                if Feat = 'SIZE' then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatSize];
                if Feat = 'REST STREAM' then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatRest];
                if Pos ('MLST', Feat) = 1 then begin
                    FSupportedExtensions := FSupportedExtensions + [ftpFeatMLST];
                    if Length (Feat) > 6 then FMLSTFacts := Trim (Copy (Feat, 6, 99));
                end;
                if Feat = 'MFMT' then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatMFMT]; { V2.94 same as MDTM YYYYMMDDHHMMSS }
                if Feat = 'MD5' then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatMD5];   { V2.94 }
                if Pos ('XMD5', Feat) = 1 then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatXMD5];  { V2.113 }
                { AUTH, PROT by AG }
                if Pos('AUTH', Feat) = 1 then begin
                    if Pos('SSL', Feat) > 5 then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatAuthSSL];
                    if (Pos('TLS', Feat) > 5) then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatAuthTLS];
                end;
                if Pos('PROT', Feat) = 1 then begin
                    if Pos('C', Feat) > 5 then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatProtC];
                    if (Pos('P', Feat) > 5) then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatProtP];
                end;
                if Feat = 'PBSZ' then
                    FSupportedExtensions := FSupportedExtensions + [ftpFeatPbsz];    { V2.106 }
                if Feat = 'CCC' then
                    FSupportedExtensions := FSupportedExtensions + [ftpFeatCCC];
                if Pos ('XCRC', Feat) = 1 then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatXCrc];   { V2.107 }
                if Pos ('MODE Z', Feat) = 1 then begin  { V1.103 currently ignoring compression options }
                    FSupportedExtensions := FSupportedExtensions + [ftpFeatModeZ];
                end;
                if Feat = 'CLNT' then
                    FSupportedExtensions := FSupportedExtensions + [ftpFeatClnt];   { V2.113 }
                if Feat = 'COMB' then
                    FSupportedExtensions := FSupportedExtensions + [ftpFeatComb];   { V2.113 }
                if Pos('SITE', Feat) = 1 then begin                                 { V2.113 }
                    Feat := Feat + ';';
                    if Pos('PSWD;', Feat) > 4 then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatSitePaswd];
                    if (Pos('EXEC;', Feat) > 4) then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatSiteExec];
                    if (Pos('INDEX;', Feat) > 4) then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatSiteIndex];
                    if (Pos('ZONE;', Feat) > 4) then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatSiteZone];
                    if (Pos('MSG;', Feat) > 4) then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatSiteMsg];
                    if (Pos('CMLSD;', Feat) > 4) then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatSiteCmlsd];
                    if (Pos('DMLSD;', Feat) > 4) then
                        FSupportedExtensions := FSupportedExtensions + [ftpFeatSiteDmlsd];
                end;
                if Feat = 'UTF8' then
                    FSupportedExtensions := FSupportedExtensions + [ftpFeatUtf8];   { 7.01 }
                if Feat = 'HOST' then
                    FSupportedExtensions := FSupportedExtensions + [ftpFeatHost];   { 7.01 }
                if Feat = 'LANG' then begin
                    FSupportedExtensions := FSupportedExtensions + [ftpFeatLang];   { 7.01 }
                    if Length (Feat) > 6 then FLangSupport := Trim (Copy (Feat, 6, 99));
                end;
                if Feat = 'XCMLSD' then
                    FSupportedExtensions := FSupportedExtensions + [ftpFeatXCmlsd]; { 7.01 }
                if Feat = 'XDMLSD' then
                    FSupportedExtensions := FSupportedExtensions + [ftpFeatXDmlsd]; { 7.01 }
           { Other extensions which are currently being ignored
             TVFS }
            end;
            if FFctPrv in [ftpFctMlst] then begin       { V2.90 response to MLST command }
                if (Length (FLastResponse) > 4) and (FStatusCode = 250) then begin
                    if FLastResponse[1] = ' ' then
                        FRemFacts := Trim (FLastResponse);
                end;
            end;
        { V2.113 save response to SITE CMLSD, XCMLSD and SITE INDEX commands to stream }
            if (FFctPrv in [ftpFctSiteCmlsd, ftpFctXCmlsd, ftpFctSiteIndex]) then begin
                p := GetInteger(@FLastResponse[1], FStatusCode);
                if (FStatusCode in [200, 250]) and (p^ = '-') then begin
                    if FLocalStream <> nil then begin
                        try
                            { V7.01 write raw UTF8 or ANSI response to stream, less 200-  }
                            if Length(RawResponse) > 4 then
                                LocalStreamWrite(RawResponse[5], Length(RawResponse) - 4);
                            LocalStreamWrite(NewLine, 2);
                        except
                            TriggerDisplay('! Error writing local file');
                            Exit;
                        end;
                    end;
                    DisplayLastResponse;  { warning - client may want to suppress this response }
                    Continue;
                end;
             { finished reading directory, tidy up }
                if Assigned(FLocalStream) and (FStreamFlag = FALSE) then begin
                    FLocalStream.Free;
                    FLocalStream := nil;
                end;
            end;
            if (FLastResponse = '') or                            { 15/02/03 }
               (not IsDigit(FLastResponse[1])) then begin         { 22/11/99 }
                DisplayLastResponse; { 02/11/01 }
                Continue;  { Continuation line, ignore }
            end;
            p := GetInteger(@FLastResponse[1], FStatusCode);
            if p^ = '-' then begin
                DisplayLastResponse; { 02/11/01 }
                Continue;  { Continuation line, ignore }
            end;
            if Assigned(FNext) then
                FNext
            else begin
                HandleError('Program error: FNext is nil');
                Exit;
            end;
        end
        else { Unexpected data received }
            DisplayLastResponse;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ControlSocketSessionClosed(
    Sender  : TObject;
    ErrCode : Word);
var
    LClosedState : TFtpState;
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loProtSpecInfo) then
        DebugLog(loProtSpecInfo, 'Control Socket Closed, error='  + IntToStr (ErrCode));  { V8.03 }
{$ENDIF}
    { Sometimes ErrCode equals ECONNRESET after QUIT response has been received
      OK when server and client are on the same host. Seen on MacOS and was
      reported in TWSocket list for Windows as well. In such a case we may
      safely ignore an error here.                                             }
    if (ErrCode <> 0) and (FState = ftpInternalReady) and              { V7.28 }
       ((FRequestType = ftpQuitAsync) or (FFctPrv = ftpFctQuit)) then
        ErrCode := 0;

    LClosedState := FState;
    if FConnected then begin
        FConnected := FALSE;
        if FState <> ftpAbort then
            StateChange(ftpNotConnected);
        if Assigned(FOnSessionClosed) then
            FOnSessionClosed(Self, ErrCode);
    end;
    if FState <> ftpAbort then
        StateChange(ftpInternalReady);
    if FRequestType <> ftpRqAbort then begin
        if (ErrCode <> 0) or ((FRequestType <> ftpQuitAsync) and
           (LClosedState in [ftpWaitingBanner, ftpWaitingResponse])) then begin
            FLastResponse  := '500 Control connection closed - ' +
                               WSocketGetErrorMsgFromErrorCode(ErrCode);
            FStatusCode    := 500;
            FRequestResult := FStatusCode;    { 06 apr 2002 }
            SetErrorMessage;
        end;
        TriggerRequestDone(FRequestResult);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TriggerStateChange;
begin
    if Assigned(FOnStateChange) then
        FOnStateChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TriggerRequestDone(ErrCode: Word);
begin
    if not FRequestDoneFlag then begin
        FRequestDoneFlag := TRUE;
        if (ErrCode = 0) and Assigned(FNextRequest) then begin
            if (FState <> ftpAbort)
               and (FState <> ftpPasvReady) { 19.09.2002 }
              { and     28/06/2002
               not ((ftpFctPut in FFctSet) and (FPassive = TRUE))} then
                StateChange(ftpInternalReady);
            FNextRequest;
        end
        else begin
            StateChange(ftpReady);
            if FDataSocket.State <> wsClosed then
                FDataSocket.Close;
            { Restore the lastresponse saved before quit command }
            if FHighLevelFlag and (FStatusCodeSave >= 0) then begin
                 FLastResponse := FLastResponseSave;
                 FStatusCode   := FStatusCodeSave;
            end;
            FHighLevelFlag := FALSE;
            FNextRequest   := nil;
            PostMessage(Handle, FMsg_WM_FTP_REQUEST_DONE, 0, ErrCode);
            { if Assigned(FOnRequestDone) then
                FOnRequestDone(Self, FRequestType, ErrCode); }
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TriggerResponse;   { V7.08 }
begin
    if Assigned(FOnResponse) then
        FOnResponse(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TriggerReadyToTransmit(var bCancel : Boolean);
begin
    if Assigned(FOnReadyToTransmit) then
        FOnReadyToTransmit(Self, bCancel);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetConnected : Boolean;
begin
    Result := FControlSocket.State <> wsClosed;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_DEBUG_LOG}
function TCustomFtpCli.GetIcsLogger: TIcsLogger;                    { 2.104 }
begin
    Result := FControlSocket.IcsLogger;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetIcsLogger(const Value: TIcsLogger);      { 2.104 }
begin
    FControlSocket.IcsLogger := Value;
    FDataSocket.IcsLogger    := Value;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.CheckLogOptions(const LogOption: TLogOption): Boolean; { 2.104 }
begin
    Result := Assigned(IcsLogger) and (LogOption in IcsLogger.LogOptions);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DebugLog(LogOption: TLogOption; const Msg: string);  { 2.104 }
begin
    if Assigned(IcsLogger) then
        IcsLogger.DoDebugLog(Self, LogOption, Msg);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetSslType(const Value: TFtpCliSslType);             { V2.106 }
begin
    FSslType := sslTypeNone;
    if Value <> sslTypeNone then
        raise FtpException.Create('SSL requires ICS-SSL version');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF UseBandwidthControl}
procedure TCustomFtpCli.BandwidthTimerTimer(Sender : TObject);
begin
    if FBandwidthPaused then begin
        FBandwidthPaused := FALSE;
        Dec(FBandwidthCount, FBandwidthMaxCount);
        FDataSocket.Resume;
    end;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* *                                                                     * *}
{* *                              TFtpClient                             * *}
{* *                                                                     * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TFtpClient.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FTimeout := 15;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Open : Boolean;
begin
    Result := Synchronize(OpenAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.User : Boolean;
begin
    Result := Synchronize(UserAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Pass : Boolean;
begin
    Result := Synchronize(PassAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Acct : Boolean;
begin
    Result := Synchronize(AcctAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Connect : Boolean;
begin
    Result := Synchronize(ConnectASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Cwd : Boolean;
begin
    Result := Synchronize(CwdASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Pwd : Boolean;
begin
    Result := Synchronize(PwdASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.CDup : Boolean;
begin
    Result := Synchronize(CDupASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.TypeSet : Boolean;
begin
    Result := Synchronize(TypeSetASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.TypeBinary : Boolean;
begin
    Binary := TRUE;
    Result := Synchronize(TypeSetASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.TypeAscii : Boolean;
begin
    Binary := FALSE;
    Result := Synchronize(TypeSetASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Get : Boolean;
begin
    Result := Synchronize(GetASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Put : Boolean;
begin
    Result := Synchronize(PutASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ By A.Burlakov: new function for resuming uploads                          }
{ Uses REST + STOR commands instead APPEND                                  }
function TFtpClient.RestPut : Boolean;
begin
    Result := Synchronize(RestPutASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.RestartPut : Boolean;
begin
    Result := Synchronize(RestartPutASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Append : Boolean;
begin
    Result := Synchronize(AppendASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Quit : Boolean;
begin
    Result := Synchronize(QuitASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Abort : Boolean;
begin
    Result := Synchronize(AbortASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Receive : Boolean;
begin
    Result := Synchronize(ReceiveASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Transmit : Boolean;
begin
    Result := Synchronize(TransmitASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.AppendFile : Boolean;
begin
    Result := Synchronize(AppendFileASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Dir : Boolean;
begin
    Result := Synchronize(DirASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Directory : Boolean;
begin
    Result := Synchronize(DirectoryASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Ls : Boolean;
begin
    Result := Synchronize(LsASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.List : Boolean;
begin
    Result := Synchronize(ListASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Mkd : Boolean;
begin
    Result := Synchronize(MkdASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Mkdir : Boolean;
begin
    Result := Synchronize(MkdirASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Ren : Boolean;
begin
    Result := Synchronize(RenASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Rename : Boolean;
begin
    Result := Synchronize(RenameASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Dele : Boolean;
begin
    Result := Synchronize(DeleASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Delete : Boolean;
begin
    Result := Synchronize(DeleteASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Rmd : Boolean;
begin
    Result := Synchronize(RmdASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Rmdir : Boolean;
begin
    Result := Synchronize(RmdirASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Syst : Boolean;
begin
    Result := Synchronize(SystASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.System : Boolean;
begin
    Result := Synchronize(SystemASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Auth : Boolean;                                          { V2.106 }
begin
    Result := Synchronize(AuthASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Ccc : Boolean;                                           { V2.106 }
begin
    Result := Synchronize(CccASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Prot: Boolean;
begin
    Result := Synchronize(ProtASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Pbsz: Boolean;
begin
    Result := Synchronize(PbszASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.AuthenticateSsl : Boolean;
begin
    Result := Synchronize(AuthenticateSslASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Size : Boolean;
begin
    Result := Synchronize(SizeASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.FileSize : Boolean;
begin
    Result := Synchronize(FileSizeASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.AbortXfer: Boolean;
begin
    Result := Synchronize(AbortXferASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Quote : Boolean;
begin
    Result := Synchronize(QuoteASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.DoQuote : Boolean;
begin
    Result := Synchronize(DoQuoteASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.RestGet : Boolean;
begin
    Result := Synchronize(RestGetASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.RestartGet : Boolean;
begin
    Result := Synchronize(RestartGetASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Mlsd       : Boolean;    { V2.90 machine list directory }
begin
    Result := Synchronize(MlsdASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Mlst       : Boolean;    { V2.90 machine list file      }
begin
    Result := Synchronize(MlstASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Feat       : Boolean;    { V2.90 supported extensions   }
begin
    Result := Synchronize(FeatASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Mdtm       : Boolean;    { V2.90 get file modification time }
begin
    Result := Synchronize(MdtmASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Mdtmyy     : Boolean;    { V2.90 set file modification time }
begin
    Result := Synchronize(MdtmyyASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.Mfmt       : Boolean;    { V2.94 modify file modification time }
begin
    Result := Synchronize(MfmtASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.Md5       : Boolean;    { V2.94 get MD5 hash sum }
begin
    Result := Synchronize(Md5ASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.XMd5      : Boolean;    { V2.113 get MD5 hash sum }
begin
    Result := Synchronize(XMd5ASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.ModeZ      : Boolean;    { V2.102 set mode z }
begin
    Result := Synchronize(ModeZASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.Opts       : Boolean;    { V2.102 set mode z options }
begin
    Result := Synchronize(OptsASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.XCrc       : Boolean;    { V2.107 get CCRC32 hash sum }
begin
    Result := Synchronize(XCrcASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.Clnt       : Boolean;    { V2.113  client string  }
begin
    Result := Synchronize(ClntASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.Allo       : Boolean;    { V2.113  check allocated space }
begin
    Result := Synchronize(AlloASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.Comb       : Boolean;    { V2.113  combine files  }
begin
    Result := Synchronize(CombASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.SitePaswd  : Boolean;    { V2.113  change password }
begin
    Result := Synchronize(SitePaswdASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.SiteExec   : Boolean;    { V2.113  run program  }
begin
    Result := Synchronize(SiteExecASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.SiteIndex  : Boolean;    { V2.113  list files and dirs recursively  }
begin
    Result := Synchronize(SiteIndexASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.SiteZone   : Boolean;    { V2.113  get time zone difference }
begin
    Result := Synchronize(SiteZoneASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.SiteMsg    : Boolean;    { V2.113  send message }
begin
    Result := Synchronize(SiteMsgASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.SiteCmlsd  : Boolean;    { V2.113  extended MLSD using control channel }
begin
    Result := Synchronize(SiteCmlsdASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.SiteDmlsd  : Boolean;    { V2.113  extended MLSD using data channel }
begin
    Result := Synchronize(SiteDmlsdASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.ConnectHost  : Boolean;     { V7.01   same as connect, but sends HOST  }
begin
    Result := Synchronize(ConnectHostASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.Rein  : Boolean;     { V7.01   re-initialize control connection  }
begin
    Result := Synchronize(ReinASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.Host  : Boolean;     { V7.01   domain/hostname, usually sent before logon  }
begin
    Result := Synchronize(HostASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.Lang    : Boolean;    { V7.01  language for messages }
begin
    Result := Synchronize(LangASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.XCmlsd  : Boolean;    { V7.01  extended MLSD using control channel }
begin
    Result := Synchronize(XCmlsdASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.XDmlsd  : Boolean;    { V7.01  extended MLSD using data channel }
begin
    Result := Synchronize(XDmlsdASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.ConnectFeat  : Boolean;     { V7.09   same as connect, but sends FEAT  }
begin
    Result := Synchronize(ConnectFeatASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TFtpClient.ConnectFeatHost  : Boolean;     { V7.09   same as connect, but sends FEAT and HOST  }
begin
    Result := Synchronize(ConnectFeatHostASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Progress : Boolean;
begin
    Result := inherited Progress;
    { Evaluate the timeout period again }
    if FTimeout > 0 then
        FTimeStop := LongInt(IcsGetTickCount) + LongInt(FTimeout) * 1000;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.WaitUntilReady : Boolean;
{$IFDEF MSWINDOWS}
var
    DummyHandle     : THandle;
{$ENDIF}
begin
{$IFNDEF WIN64}                  { V7.25 }
    Result    := TRUE;           { Make dcc32 happy }
{$ENDIF}
    FTimeStop := LongInt(IcsGetTickCount) + LongInt(FTimeout) * 1000;
  {$IFDEF MSWINDOWS}
    DummyHandle := INVALID_HANDLE_VALUE;
  {$ENDIF}
    while TRUE do begin
        { V8.03 InternalReady happens between multiple commands, ignore it }
        if FState in [ftpReady {, ftpInternalReady}] then begin
            { Back to ready state, the command is finished }
            Result := (FRequestResult = 0);
            break;
        end;

        if Terminated or
           ((FTimeout > 0) and (LongInt(IcsGetTickCount) > FTimeStop)) then begin
            { Timeout occured }
            AbortAsync;
            FErrorMessage := '426 Timeout';
            FStatusCode   := 426;
            Result        := FALSE; { Command failed }
            break;
        end;
      {$IFDEF MSWINDOWS}
        { Do not use 100% CPU }
        if ftpWaitUsingSleep in FOptions then
            Sleep(0)
        else
            MsgWaitForMultipleObjects(0, DummyHandle, FALSE, 1000, QS_ALLINPUT);
      {$ENDIF}
        MessagePump;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Synchronize(Proc : TFtpNextProc) : Boolean;
begin
    try
        Proc;
        Result := WaitUntilReady;
    except
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslFtpClient.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FProtLevel     := 'C';
    FProtLevelSent := '';
    FPBSZSize      := 0;
    FRenegInitFlag := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.SetProtLevel(const Value : String);
begin
    if (Value <> 'P') and (Value <> 'C') then
        raise Exception.Create('Protection level must be ''P'' or ''C''.');
    FProtLevel := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.OpenAsync;
begin
    FProtLevelSent := '';
    FProtDataFlag  := FALSE;
    inherited OpenAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.AuthAsync;
var
    S : String;
begin    
    { Some SSL versions and some OpenSSL versions do not support renegotiation }
    { In such a case we fake an OK response if the SSL is already established. }
    if FConnected and Assigned(FControlSocket.Ssl) and                 { V7.14 }
       (f_SSL_state(FControlSocket.Ssl) = SSL_ST_OK) then
    begin
        if (f_SSL_version(FControlSocket.Ssl) < SSL3_VERSION) or
           IsSslRenegotiationDisallowed(FControlSocket) then
        begin
            if not CheckReady then begin
                TriggerDisplay('Not ready for Auth');  { V8.03 }
                Exit;
            end;
            if not FHighLevelFlag then
                FRequestType := ftpAuthAsync;
            FFctPrv := ftpFctAuth;
            FOkResponses[0]     := 234;
            FOkResponses[1]     := 0;
            FStatusCode         := 234;
            FLastMultiResponse  := '';
            FLastResponse       := '234 OK Secure connection already established';
            FRequestDoneFlag    := FALSE;
            FNext               := NextExecAsync;
            FDoneAsync          := nil;
            FErrormessage       := '';
            StateChange(ftpWaitingResponse);
            TriggerRequestDone(0);
            Exit;
        end;
    end;

    if FSslType = sslTypeAuthSsl then
        S := 'AUTH SSL'
    else
        S := 'AUTH TLS';
    FFctPrv := ftpFctAuth;
    ExecAsync(ftpAuthAsync, S, [234, 334], nil); // 334 invalid see RFC4217
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.CccAsync;
begin
    FFctPrv  := ftpFctCcc;
    ExecAsync(ftpCccAsync, 'CCC', [200], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.ProtAsync;
begin
    FFctPrv        := ftpFctProt;
    FProtLevelSent := FProtLevel;
    ExecAsync(ftpProtAsync, 'PROT ' + FProtLevelSent, [200], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.PBSZAsync;
begin
    FFctPrv := ftpFctPbsz;    { V8.05 }
    ExecAsync(ftpPbszAsync, 'PBSZ ' + IntToStr(FPBSZSize), [200], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslFtpClient.GetSslContext: TSslContext;
begin
    Result := FControlSocket.SslContext
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.SetSslContext(Value: TSslContext);
begin
    FControlSocket.SslContext := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.TransferSslHandshakeDone(Sender: TObject;
    ErrCode: Word; PeerCert: TX509Base;  var Disconnect : Boolean);
begin
    if (ErrCode <> 0) then begin
    {    FLastResponse := '535 SSL handshake failed. Error #' + IntToStr(ErrCode);  }
        FLastResponse := '535 SSL handshake failed - ' + (Sender as TCustomSslWSocket).SslHandshakeRespMsg;  { V8.06 }
        DisplayLastResponse;
        FStatusCode    := 535;
        FRequestResult := FStatusCode;
        SetErrorMessage;
    end
    else
        TriggerDisplay('! ' + (Sender as TCustomSslWSocket).SslHandshakeRespMsg);  { V8.06 }

    if Assigned(FOnSslHandshakeDone) then
        FOnSslHandshakeDone(Self, ErrCode, PeerCert, Disconnect);   // 12/14/05

    { Trigger RequestDone when we initiated a re-negotiation on AUTH }
    if FRenegInitFlag then begin
        FRenegInitFlag := FALSE;
        if Assigned(FDoneAsync) then
            FDoneAsync
        else
            TriggerRequestDone(FRequestResult);
        Exit;
    end;

    if ((Sender = FDataSocket) or (FSslType = sslTypeImplicit) or
        (Sender as TCustomSslWSocket).SslInRenegotiation) and
       (FState <> ftpAbort) and FConnected then
        Exit;

    if Assigned(FDoneAsync) then
        FDoneAsync
    else
        TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.TransferSslCliCertRequest(
    Sender      : TObject;
    var Cert    : TX509Base);
begin
    if Assigned(FOnSslCliCertRequest) then
        FOnSslCliCertRequest(Self, Cert);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.TransferSslVerifyPeer(
    Sender        : TObject;
    var Ok        : Integer;
    Cert          : TX509Base);
begin
    if Assigned(FOnSslVerifyPeer) then
        FOnSslVerifyPeer(Self, Ok, Cert);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.TransferSslCliGetSession(Sender: TObject;
    var SslSession: Pointer; var FreeSession: Boolean);
begin
    if Assigned(FOnSslCliGetSession) then
        FOnSslCliGetSession(Self, SslSession, FreeSession);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.TransferSslCliNewSession(Sender: TObject;
    SslSession: Pointer; WasReused  : Boolean; var IncRefCount: Boolean);
begin
    if Assigned(FOnSslCliNewSession) then
        FOnSslCliNewSession(Self, SslSession, WasReused, IncRefCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.ControlSocketSslShutDownComplete(
    Sender          : TObject;
    Bidirectional   : Boolean;
    ErrCode         : Integer);
begin
    TriggerDisplay('! Control channel SSL shutdown complete');
    FControlSocket.OnSslShutDownComplete := nil;
    if (ErrCode > 0) then begin
        FStatusCode    := ErrCode;
        FRequestResult := ErrCode;
        FLastResponse  := IntToStr(ErrCode) + ' Bidirectional SSL shutdown failed';
        FNextRequest   := nil;
        SetErrorMessage;
    end;
    if Assigned(FDoneAsync) then
        FDoneAsync
    else
        TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Just to make UI easier: parse a semi-colon delimited texte string with
// a list of hosts and build the FSslAcceptableHosts list.
procedure TSslFtpClient.SetAcceptableHostsList(
    const SemiColonSeparatedList : String);
var
    Host : String;
    Buf  : String;
    I    : Integer;
begin
    SslAcceptableHosts.Clear;
    Buf := SemiColonSeparatedList;
    while TRUE do begin
        I := Pos(';', Buf);
        if I > 0 then begin
            Host := Trim(Copy(Buf, 1, I - 1));
            if Host > '' then
                SslAcceptableHosts.Add(Host);
            Buf := Copy(Buf, I + 1, Length(Buf));
            //System.Delete(Buf, 1, I);
        end
        else begin
            Host := Trim(Buf);
            if Host > '' then
                SslAcceptableHosts.Add(Host);
            break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.SetSslAcceptableHosts(Value : TStrings);
begin
    if Assigned(FControlSocket) then
        FControlSocket.SslAcceptableHosts := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TSslFtpClient.GetSslAcceptableHosts: TStrings;
begin
    if Assigned(FControlSocket) then
        Result := FControlSocket.SslAcceptableHosts
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.DataSocketPutAppendInit(const TargetPort, TargetIP : String);
begin
    inherited DataSocketPutAppendInit(TargetPort, TargetIP);
    FDataSocket.OnSessionClosed := DataSocketPutSessionClosed;
    if FSslType <> ssltypeNone then begin
        FDataSocket.SslAcceptableHosts        := FControlSocket.SslAcceptableHosts;
        FDataSocket.SslEnable                 := FProtDataFlag; { AG V7.03 }
        if FDataSocket.SslEnable then begin
            FDataSocket.SslContext            := FControlSocket.SslContext;
            FDataSocket.SslMode               := SslModeClient;
            FDataSocket.OnSslVerifyPeer       := FControlSocket.OnSslVerifyPeer;
            FDataSocket.OnSslHandshakeDone    := FControlSocket.OnSslHandshakeDone;
            FDataSocket.OnSslCliGetSession    := FControlSocket.OnSslCliGetSession;
            FDataSocket.OnSslCliNewSession    := FControlSocket.OnSslCliNewSession;
            FDataSocket.OnSslCliCertRequest   := FControlSocket.OnSslCliCertRequest;
        end;
    end
    else
        FDataSocket.SslEnable := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.DataSocketGetInit(const TargetPort, TargetIP : String);
begin
    inherited DataSocketGetInit(TargetPort, TargetIP);
    FDataSocket.SslAcceptableHosts        := FControlSocket.SslAcceptableHosts;
    FDataSocket.SslEnable                 := FProtDataFlag; { AG V7.03 }
    if FDataSocket.SslEnable then begin
        FDataSocket.SslContext            := FControlSocket.SslContext;
        FDataSocket.SslMode               := SslModeClient;
        FDataSocket.OnSslVerifyPeer       := FControlSocket.OnSslVerifyPeer;
        FDataSocket.OnSslHandshakeDone    := FControlSocket.OnSslHandshakeDone;
        FDataSocket.OnSslCliGetSession    := FControlSocket.OnSslCliGetSession;
        FDataSocket.OnSslCliNewSession    := FControlSocket.OnSslCliNewSession;
        FDataSocket.OnSslCliCertRequest   := FControlSocket.OnSslCliCertRequest;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.ControlSocketSessionConnected(Sender: TObject; ErrCode: Word);
begin
    inherited ControlSocketSessionConnected(Sender, ErrCode);
    if FConnected then begin
        FControlSocket.SslEnable := FSslType = sslTypeImplicit;
        if FControlSocket.SslEnable then
        try
            FControlSocket.SslMode             := sslModeClient;
            FControlSocket.OnSslHandshakeDone  := TransferSslHandshakeDone;
            FControlSocket.OnSslVerifyPeer     := TransferSslVerifyPeer;
            FControlSocket.OnSslCliGetSession  := TransferSslCliGetSession;
            FControlSocket.OnSslCliNewSession  := TransferSslCliNewSession;
            FControlSocket.OnSslCliCertRequest := TransferSslCliCertRequest;
            FControlSocket.StartSslHandshake;
            //TriggerDisplay('! Starting SSL handshake');
        except
            on E:Exception do begin
                TriggerDisplay('! Init SSL failed ' + E.Message);
                FStatusCode    := 550;
                FRequestResult := FStatusCode;
                FLastResponse  := IntToStr(FStatusCode) + ' ' + E.Message;
                SetErrorMessage; { Heedong Lim, 05/14/1999 }
                FNextRequest   := nil;
                TriggerRequestDone(FRequestResult);
                FControlSocket.Close;
                StateChange(ftpReady);
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpClient.SetSslType(const Value: TFtpCliSslType);
begin
    FSslType := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF} // USE_SSL

end.


