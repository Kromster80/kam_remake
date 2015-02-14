{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TFtpServer class encapsulate the FTP protocol (server side)
              See RFC-959 for a complete protocol description.
Creation:     April 21, 1998
Version:      8.05
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1998-2013 by François PIETTE
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

History:
If not otherwise noted, changes are by Francois Piette
Apr 29, 1998  V0.90 released for beta testing.
May 01, 1998  V0.92 Adapted for Delphi 1.0
May 03, 1998  V0.93 Adapted for Delphi 2.0 and C++Builder
May 04, 1998  V0.94 Use '/' or '\' as path delimiter. Expose only '/' to the
              outside. Stripped any telnet options (IE send two !). Handled
              absolute path. Implemented SIZE and REST commands.
              Added support for UNC (not finished !)
May 06, 1998  V0.95 Corrected spurious 226 message on PASV mode STOR.
              Made GetInteger retunrs a LongInt.
              Use a LongInt for N in CommandPORT (needed for 16 bits)
              Added slash substitution in BuildFilePath command.
Jul 09, 1998  V1.00 Adapted for Delphi 4, removed beta status.
Jul 21, 1998  V1.01 Added OnValidateDele event
              Changed function to get file size (do not open the file)
Feb 14, 1999  V1.02 Replaced straight winsock call by indirect calls thru
              wsocket (this provide runtime link to winsock DLL).
Mar 06, 1999  V1.03 Added code from  Plegge, Steve <jsp@nciinc.com> to add
              APPE, XMKD, KRMD and STRU commands support.
Jul 24, 1999  V1.04 Replaced msgStorDisabled value from '500 Cannot STOR.' to
              '501 Permission Denied' because CuteFTP doesn't like error 500.
              Suggested by Cedric Veilleux <webmaster@smashweb.com>.
Aug 20, 1999  V1.05 Added compile time options. Revised for BCB4.
              Added Addr property to select interface in multihomed computers.
Oct 02, 1999  V1.06 Added OnValidateRnFr and OnValidateRnTo events.
              Initialized Allowed variable to TRUE before triggerValidateDele.
Nov 24, 1999  V1.07 Added MTDM support. Thanks to Bruce Christensen
              <bkc51831234@hotmail.com> for his code.
Jan 24, 2000  V1.08 Patch IE5 bug in file names. Thanks to <dsnake@infonie.fr>
Jun 08, 2000  V1.09 Added 'A N' type for type command for AIX systems.
Oct 25, 2000  V1.10 Exposed list of clients thru Client[] property.
Oct 29, 2000  V1.11 Added IsClient() method.
              Implemented OnValidateRmd event.
Nov 01, 2000  V1.12 Implemented proposals from Carl@Smotricz.com:
              (1) support for MODE command, but only the default do-nothing
              option S. (2) binding the data socket to the local host address
              and port 20 ('ftp-data'). (3) detection of failure to open the
              data connection for STOR or RETR.
              Added option wsoNoReceiveLoop to sockets. See comments in TWSocket
              about this option. Help in very fast LAN.
Nov 11, 2000  V1.13 Checked for DOS attack. Close connection when buffer
              overflow occured. Thanks to Lester <les@lester.co.uk> for finding
              this security hole.
Jun 18, 2001  V1.14 Fixed file left open when storing and client broken data
              connection. Thanks to Davie <smatters@smatters.com>
Jul 27, 2001  V1.15 I fixed a race condition between WMFtpSrvClientClosed and
              WMFtpSrvCloseData found by Matthew Comb <matt@filesafe.co.nz> who
              worked with Davie <smatters@smatters.com>. Now WMFtpSrvCloseData
              receive Client in LParam and check if client is still in client
              list.
              Fixed a but with resumed put. Thanks Yvan Turkan iturcan@gamo.sk !
              Added a procedure to disconnect a single client.
              Changed all Exception by FtpServerException.
              Changed all "Error" by "AError" to avoid conflict with global var.
              Added Client.ID property to uniquely indentify the client. Pass
              this ID along with all posted messages and verify if the correct
              client still exists when message is processed.
Jul 30, 2001  V1.16 Added same check as above for WMFtpSrvCloseData.
Sep 09, 2001  V1.17 Eric Pascual <e.pascual@cstb.fr> added Store Unique (STOU)
              command.
Feb 26, 2002  V1.18 Fastream Technologies (http://www.fastream.com) found a bug
              in Disconnect and DisconnectAll which prevented data connection
              to be closed and client component to be destroyed.
Jul 06, 2002  V1.19 Fastream Technologies (http://www.fastream.com) fixed
              CommandXPWD and CommandPWD to make the path in answer as
              "/c:/windows" instead of "c:/windows" which is more compatible
              with the UNIX standard that most clients expect.
Sep 16, 2002  V1.20 Added OnValidateSize event.
              Allowed "REST 0" as a valid command.
Sep 17, 2002  V1.21 Sven Schmidts <sven.schmidts@nusec.de> added partional FEAT
              command, must extended, because I doesn't know what commands are
              special-featured.
Oct 26, 2002  V1.22 Introduced OnBuildFilePath to allow component use to change
              the file path on the fly.
              Thanks to Serge Chelli <serge@aceinformatique.com> who proposed
              this change.
Nov 01, 2002  V1.23 When client request passive mode, select a port from a
              range of ports instead of letting the OS choose one. This ease the
              use of a FTP server behind a firewall. Passive mode transferts
              will use port in the specified range.
              Also implemented fixed IP for passive mode.
              Thanks to Ian Tuck <ituck@noglobalborders.com> for code base.
Nov 06, 2002  V1.24 Added definition for PBoolean which is missing in some
              older Delphi version and in BCB.
Nov 11, 2002  V1.25 Revised for Delphi 1
Jan 26, 2003  V1.26 ByteCount fix. Thanks to wilfried@mestdagh.biz and
              fastream@fastream.com for the fix.
Sep 15, 2003  V1.27 Added ICSDEF feature to the source code. Thanks to Marco
              van de Voort <marcov@stack.nl> for his help.
Nov 01, 2003  V1.28 Corrected FormatUnixDirEntry for files greater than 2GB.
Dec 15, 2003  V1.29 Changed ClientRetrSessionConnected to check if file exists
              to avoid TStream exception opening a non existant file.
Jan 15, 2004  V1.30 Made BuildFilePath virtual.
Feb 16, 2004  V1.31 Andreas Mueller <Amueller@Nord-Vision.de> updated
              CommandRNFR and CommandRNTO to handle directories.
Feb 24, 2004  V1.32 Wilfried changed Close by Shutdown(1) in WMFtpSrvCloseData.
Mar 06, 2004  V1.33 Added DirectoryExists function for Delphi below V5
May 26, 2004  V1.34 Added support for hidden files. Thanks to Martin Koberstein
              <MKoberstein@nord-vision.de>.
Jun 07, 2004  V1.35 Fixed DirExists to "see" hidden directories. This
              permit deletion of hidden directories
Jun 08, 2004  V1.36 Removed DirectoryExists function and used DirExists instead.
Jul 23, 2004  V1.37 Added type keyword to "TFtpString = type String;"
Aug 6, 2004   V1.38 Angus Robertson, angus@magsys.co.uk added new Options property
              added MDTM YYYYMMDDHHMMSS support (set file mod date)
              added MLST and MLSD commands for better file listings
              CWD now returns 550 if new directory does not exist and Options=ftpsCWDCheck
              changing to a higher level directory than HomeDir is blocked if Options=ftpsCdupHome
              corrected DirExists to strip trailing backslash so it works
Aug 19, 2004  V1.39 Angus Robertson, corrected Options=ftpsCWDCheck to allow
              root (c:\)
              Options passed to Client as ftpCwdCheck, ftpCdupHome so they
              can be changed per client
              MDTM checks logged-in, new trigger before changing file time stamp
              Added MFMT modify file modification time (same as
              MDTM YYYYMMDDHHMMSS but draft RFC'd)
              (not yet supporting MFCT create time or MFF file facts commands)
              Added MD5 command which returns hash of file content to allow
              corruption check
              (not yet supporting MMD5 multiple file or XMD5 file range commands)
Sep 08, 2004 V1.40 MD5 has been renamed to IcsMD5
Oct 20, 2004 V1.41 Angus Robertson, MLSD command failed in passive mode
Mar 11, 2005 V1.42 Marco van de Voort <marcov@stack.nl> updated the component
             to be compatible with NOFORMS concept.
             He implemented FtpSrvAllocateHWnd and FtpSrvDeallocateHWnd based
             on TWSocket versions.
             Angus Robertson, using ftpCwdCheck and ftpcUNC allow CWD to change
             to root
Sept 6, 2005 V1.43 64-bit support for Delphi 6 and later, for transfers larger
             than 2 gigs, added error handling for failed seeks and TStream issues
             by Angus Robertson, angus@magsys.co.uk
Oct 21, 2005 V1.44 Arno Garrels added SSL features.
Dec 29, 2005 V1.45 Peter Feldbaumer feldbaumer@feldtech.com fixed excessive
             226-response for cancelled passive data-connections. He also
             fixed ByteCount handling, unified passive data-connection-setup.
Dec 30, 2005 V1.46 Arno Garrels added IcsLogger.
Jan 18, 2006 V1.47 TLS/SSL related changes.
Aug 6, 2006  V1.48 using GetWinsockErr in wsocket to give consistent textual and
             numeric winsock errors, by Angus (some constant literals changed from %d to %s)
             wsocket fix for 64-bit transfers with range checking enabled
             for address in use error, report port in proper decimal
             SSL check Self = TSslFtpServer before accessing SSL properties (Arno)
Aug 31, 2006 V1.49 A.Garrels reworked 64-bit streams support.
Sep 20, 2006 V1.50 A.Garrels implemented smarter MD5 calculation.
             How it works: On new uploads new option ftpsCalcMD5OnTheFly forces
             calculation of the MD5 sum in chunks in ClientStorDataAvailable.
             New property Md5UseThreadFileSize determines whether the checksum
             is calculated in a blocking manner or inside a worker thread
             when FTP command MD5 is processed. Therefore I introduced a new
             ProcessingThread in TFtpCtrlSocket that may be used for any
             lengthy processing inside the component.
             New event OnMD5Calculated triggers either when the internal MD5
             calculation finished or when the sum needs to be updated, an
             empty parameter Md5Sum signals to delete a possibly cached entry
             except the file has been renamed.
Oct 27, 2006 V1.51 A.Garrels made the command table a dynamic array. Some
             improvements with PasvIpAddr: New options ftpsNoPasvIpAddrInLan
             and ftpsNoPasvIpAddrSameSubnet. New event OnPasvIpAddr.
Dec 05, 2006 Fixed FreeCurrentPasvPort
May 09, 2007 V1.52 changes by A.Garrels. Added two new events (sponsored by
             Fastream Technologies). OnEnterSecurityContext and
             OnLeaveSecurityContext make it possible to switch Windows'
             security context to the context of the logged client. New option
             ftpsHidePhysicalPath. Changed/fixed the STOU command. Fixed
             some security issues: If ftpCdUphome is in the options it's no
             longer possible to change, list, remove, rename or create
             directories above the home directory. Removed trailing
             slash from response-paths except upon root directories.
             Note: ftpsHidePhysicalPath is ignored unless ftpsCdUphome is also
             set.
June 11, 2007 V1.53 MDTM command failed with a directory name.
             Andreas Haas <andreas.haas@ops.de>
             Angus Robertson, MFMT command now supports millisecs when updating
             file time stamp (because MFMD command already returned millisecs)
             Note: sysutils FileAge functions used only supports round seconds
             Angus Robertson, Passive IP 0.0.0.0 now raises exception.
04 Dec 2007 V1.54 added more FEAT extensions, by Angus Robertson, angus@magsys.co.uk
               Note: some FEATs only reported if new events are created
             added support for One Time Passwords (aka S/Key), otp-md5, otp-md4 and otp-sha1,
               see RFC2289, uses new events OnOtpMethodEvent and OnOtpGetPasswordEvent
               (see OverbyteIcsFtpServ1 demo for OTP usage, ignore events for no OTP)
             added timeouts to close sockets on inactivity, new properties
               TimeoutSecsLogin (default 60 seconds), TimeoutSecsIdle (300) and
               TimeoutSecsXfer (900, same as IIS/4) before client is closed with
               421 answer. Note timeout is checked using a timer event once every
               five seconds so not second accurate. This event could be used for other jobs.
               Uses new event OnTimeout which can reset the timeout if needed
             added Clnt command to accept client information, calls new event
                onClntStr and sets Client.ClntStr
             added Allo command to return disk space allocation (before upload)
               checks and returns space on user's volume, but only says OK if
               AlloExtraSpace (default 1 Byte) still left as well, calls OnValidateAllo
               which may check space allocated for a specific account
             added Comb command to allow multiple upload files to be combined,
               used new event onCombine (where the actual file handling should be added)
             added Site Pswd command to change the account password, uses new
               event onSitePswd where the old and new passwords may be parsed
             added Site Exec command to execute a progam, uses new event
               onSiteExec which can decide whether the program should be run, and do it
             added Site Index command to generate a recusive directory and file name
               listing (no date or size) on the control channel, supported by Serv-U
             added Site Zone command to return the server time zone difference from
               UTC (GMT) which the client may use to adjust file listing time stamps
             added SiteMsg command to accept a message to the server, uses
               new event onSiteMsg
             added Site Dmlsd command, similar to MLSD but optional argument
               -SUBDIRS or -R for recursive directories, the path may be quoted if
               it includes spaces, the listing file names includes paths
             added Site Cmlsd command, similar to Site Dmlsd but uses control channel
               to avoid lots of small data channels sessions
             NOTE: Site Dmlsd and SiteCmlsd are new commands supported only by ICS
                FTP Server and may be disabled by removing the option ftpsSiteXmlsd
             the LIST, NLST, MLSD commands now also support -R for recursive
               sub-directores and a quoted file name
             recursive subdirectories are processsed in a thread unless option
                ftpsThreadRecurDirs is removed, ftpsThreadAllDirs similarly for all lists
             added Xcrc command to generate hash optional start and end positions,
               uses new events OnCalculateCrc and OnCrcCalculated, uses
               MD5UseThreadFileSize to check if a thread is used
             added Xmd5 command to generate hash optional start and end positions,
               used existing events OnCalculateMd5 and OnMd5Calculated
             added Mode Z and Opts commands and support for ZLIB compression if
                option ftpsModeZCompress set for server and ftpModeZCompress not disabled
                for client, ZlibMinLevel (1) and ZlibMaxLevel (9) properties restrict
                min and max compress stategies (max takes longer), ZlibNoCompExt is
                list of file extensions which compress with level 0 means no compress,
                defaults to '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;',
                ZlibWorkDir runtime property is path where temporary ZLIB files are written
                  defaults to '(systmppath)\icsftpsrv\'
                ZlinMinSpace property is minimum space on ZlibWorkDir for Mode Z to
                  be allowed, defaults to 50MByte
             added onDisplay event for server to log special information
             now reporting upload and download performance via OnDisplay event
             added various Client counters that may be used in the events to track
               server and account usage and performance, TotGetBytes and TotPutBytes
               are cumulative totals for the client which should be read in
               onClientDisconnect, SessStartTick is when client started, SessIdInfo
               may be set with client account info during logon, ReqStartTick is when
               the last request started and ReqDurMilliSecs is set to it's duration
               when the request finishes, XferStartTick is when an upload or download
               connected and starting sending data.
               Note: OverbyteIcsFtpSrvT has new functions for tick processing and timing
             moved building file directory functions to OverbyteIcsFtpSrvC so they
               can be used from client thread
             moved slash/backslash functions to OverbyteIcsFtpSrvT
             using Arno's TBufferedFileStream for improved performance
09 Dec 2007 V1.55 mode z bug fix for resumed transfers, by Angus Robertson, angus@magsys.co.uk
             added ftpsModeZNoResume option to disable resume while in Mode Z
             added ZlibMaxSize property to restrict maximum size of file that can compressed
             added callback in zlib funcs to update LastTick
06 Jan 2008 V1.56 corrected timer interval, timeout improvements
             passive port pool now issues incrementing ports instead of the sames ones
03 Mar 2008 V1.57 added SrvFileModeRead and SrvFileModeWrite as public so share locking
               can be changed, use SrvFileModeRead for MD5SUM (not locked)
            ensure file stream closed if session terminates unexpectedly
Apr 15, 2008 V1.58 A. Garrels, Unicode changes. Changed type of RcvBuf to
             PAnsiChar, corrected line indents.
Apr 25, 2008 V1.59 Removed checks for faVolumeID.
May 01, 2008 V1.60 A.Garrels added new functions DataStreamWriteString and
             DataStreamReadString.
Mar 24, 2008 V6.01 Bumped version number to 6.01
             Francois Piette made some changes to prepare code for Unicode.
Jun 25, 2008 V6.02 A. Garrels SSL code merged.
Apr 14, 2008 V6.03 A. Garrels, some Unicode related changes, basic features
             do work now. Receive buffer type-change from PChar to PAnsiChar.
             Check the various DataAvailableEvents. Fixed a bug in function
             GetInteger().
Apr 22, 2008 Some more Unicode related changes.
May 01, 2008 V6.04 A. Garrels - call new function DataStreamWriteString in
             TFtpServer.BuildDirectory.
May 12, 2008 v6.05 A. Garrels changed call of GetTempPath in constructor.
             Some type changes from String to AnsiString of published properties.
             Added Setters and Getters for those properties since current
             compiler is not able to handle AnsiString properties correctly.
Jul 11, 2008 V6.03 Angus fixed 'Unicode' bug introduced in V6.01 that stopped PORT command working
             (Just the change log from v6 added here and minor cosmetic changes
              to keep both verions in sync, this issue was already fixed in v7)
Jul 13, 2008 V6.04 Revised socket names used for debugging purpose
                   Added ListenBackLog property
Jul 13, 2008 V6.04 Made Client ReadCount a public property
Aug 04, 2008 V6.07 A. Garrels - CommandAUTH TLS sent Unicode response.
             Removed some getter and setters, they are no longer needed.
Aug 11, 2008 V6.08 A. Garrels - Type AnsiString rolled back to String.
Sep 21, 2008 V6.11 Arno removed some old compiler switches (CBuilder compat.)
Nov 6, 2008  V7.00 Angus component now uses TSocketServer to accept connections
             Client code from OverbyteFtpSrvC now in this unit
             Removed conditional code for obsolete compilers, D7 minimum
             Added more public client variables for account information
             Increased DefaultRcvSize to 16384 from 2048 for performance
             Fixed exception with threaded MD5Sum progress
             Client.Id is now allocated by TSocketServer
Nov 8, 2008  V7.01 Angus added new commands HOST, REIN, LANG and OPTS UTF8 ON/OFF
               (HOST is supported by Microsoft IIS/7 in Windows 2008)
               HOST domain allows multiple domains on the same IP address (like HTTP)
               REINialise reverts the control channel to just connected with no logon
             Added UTF8 and CodePage support, defaults to ANSI mode for compatibility
             Added ftpsEnableUtf8 and ftpsDefaultUtf8On to Options but
                ftpsDefaultUtf8On should generally not be set, otherwise the client
                is expected to be using UTF8 (FileZilla Server default to on!)
             Note UTF8 only supports full Unicode with D2009, otherwise an ANSI CodePage
             Added new commands XCMLSD and XDMLSD same as SITE CMLSD and DMLSD
Nov 14, 2008 V7.02 Arno fixed a few thread issues. And reworked UTF-8 support.
             Angus ensure BuildDirectory adds errors to directory stream in UTF-8
             default options for ftpsCwdCheck and ftpsCdupHome
Nov 21, 2008 V7.03 Angus fixed TriggerSendAnswer/AnswerToClient did not allow answer
                to be changed, raw protocol no longer available as public
Nov 21, 2008 V7.04 Arno completed V7.03, it did not compile in D2009,
             allow C++ Builder.
Nov 22, 2008 V7.05 Arno fixed the FEAT response, rfc2389 says that each feature
             line in the feature-listing begins with a single space. But in the
             ICS FTP server a feature line in the listing began with two spaces
             which prevented some clients from seeing the features.
Mar 17, 2009 V7.06 Angus added MaxAttempts property to limit failed login attempts
             (default 12) and delays the failed password answer by about five seconds
             after a third of the failed attempts have been exceeded.
             This feature should be combined with an IP black list to
             stop new connections for failed logins (one hacker tried 250,000
             passwords over 36 hours on one of my FTP servers)
Apr 16, 2009 V7.07 Angus MD5 and CRC use buffered stream
             Clean-up MD5 on the fly
             Assume STREAM64, USE_BUFFERED_STREAM, USE_MODEZ
May 17, 2009 V7.08 Angus renamed FileMD5ThreadOnProgress to UpdateThreadProgress since
                used for various functions other than md5 and made public
             UpdateTheadProgress called by Client.BuildDirList so session does not timeout
                 indexing large directories and can be aborted
             Rename Client.BuildDirectory to BuildDirList to avoid confusion with
                Server.BuildDirectory, made virtual, removed Path no longer used and
                return total files listed, and log number of files listed
             Added OnAddVirtFiles event called from BuildDirList which allows extra virtual
                directories or files to be added to directory listings
             Fixed bug in Server.BuildDirectory that meant BuildFilePath was bypassed for a
                blank directory argument which prevented virtual directories working
             Fixed bugs in CommandChangeDir and CommandALLO that meant virtual directories not
                supported because BuildFilePath was not called when checking directory
             IsPathAllowed now calls BuildFilePath event which may validate if a virtual
                directory is allowed by returning non-blank
             FormatResponsePath also calls BuildFilePath to convert translated virtual
                path back to original path in home directory, so it can be removed
             TriggerLang now calls correct event handler
June 04, 2009 V7.09 Angus called TriggerMd5Calculated when changing file date/time
             default for TimeoutSecsXfer reduced from 900 to 60 secs and only aborts
                 data channel not control channel, it must be shorter than TimeoutSecsIdle
             SessIdInfo add client numeric id to identify separate sessions from same IP
Sept 03, 2009 V7.10 Arno exchanged TThread.Resume by TThread.Start for D2010 and later
Dec  15, 2009 V7.11 Arno added type TFtpSrvCommandTable to make C++Builder happy.
June 10, 2010 V7.12 Angus added bandwidth throttling using TCustomThrottledWSocket
              Set BandwidthLimit property to maximum bytes server wide, for
              specific clients set CBandwidthLimit in a client connect event
              (requires BUILTIN_THROTTLE to be defined in project options or
              enabled OverbyteIcsDefs.inc)
Sep 05, 2010 V7.13 Arno renamed conditional defines EXPERIMENTAL_THROTTLE and
             EXPERIMENTAL_TIMEOUT to BUILTIN_THROTTLE and BUILTIN_TIMEOUT.
Oct 12, 2010 V7.14 Arno published OnBgException from underlaying server socket.
Nov 08, 2010 V7.15 Arno improved final exception handling, more details
             in OverbyteIcsWndControl.pas (V1.14 comments).
Feb 7,  2010 V7.16 Angus ensure control channel is correctly BandwidthLimited
May 21, 2011 V7.17 Arno ensure CommandAUTH resets the SSL prot-level correctly.
Jul 18, 2011 V7.18 Arno added Unicode normalization.
Aug 8,  2011 V7.19 Angus added client SndBufSize and RcvBufSize to set data socket
             buffers sizes for better performance, set to 32K to double speeds
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
                   New SocketFamily property (sfAny, sfAnyIPv4, sfAnyIPv6, sfIPv4, sfIPv6)
                   New MultiListenSockets property to add extra listening sockets,
                     each with Addr/Port/SocketFamily/FtpSslTypes properties
                     in events check MultiListenIndex, -1 is main socket, >=0 is
                     index into MultiListenSockets[] for socket raising event
Aug 13, 2012 V8.01 Angus ensure SSL not enabled by default, corrected MultiListen
                   Arno added TSslFtpWSocketMultiListenItem with FtpSslTypes
                     for each MultiListen socket
Jul 01, 2013 V8.02 Arno fixed an exception raised in ClientStorSessionClosed()
                   when an upload was aborted.
Jul 02, 2003 V8.03 Arno fixed a bug in TClientProcessingThread (thread-safety).
Jun 24, 2013 V8.04 Angus added new Options of ftpsCompressDirs defaults false
                   ftpsThreadRecurDirs default false due to rare thread bug
                   Skip using thread in zmode if level=Z_NO_COMPRESSION and
                      size less than one meg since really a straight stream copy
Dec 09, 2014 V8.05 - Angus added SslHandshakeRespMsg for better error handling


Angus pending -
CRC on the fly
MD5 on the fly for downloads if not cached already
test app - cache zlib files and CRCs and lock updates

Known Issue (V8.04 Angus)
On some processors only (multi-core Xeon?), using the XDMLSD -R command with a root
directory in passive mode and a thread for ftpsThreadRecurDirs or zmode compression
causes output to be randomly lost, raising an exception in the thread.  The issue is
probably memory corruption, but can not trace the reason.  V8.04 mitigates this bug
by disabling ftpsThreadRecurDirs and ftpsCompressDirs compression for directories and
not using a thread if no compression needed (note in zmode a ZLIB must still be
called but does not actually compress the file).

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsFtpSrv;
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
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$IFNDEF COMPILER7_UP}
    Bomb('This component requires Delphi 7 or later');
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    OverbyteIcsWinSock,
{$ENDIF}
{$IFDEF POSIX}
    Posix.Errno,
    Posix.Stdio,
    Posix.Time,
    Posix.SysTypes,
    Posix.Unistd,
    Posix.Fcntl,
    Posix.SysSocket,
    Ics.Posix.Wintypes,
    Ics.Posix.Messages,
    System.IOUtils,
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
{$IFNDEF NO_DEBUG_LOG}
    OverbyteIcsLogger,
{$ENDIF}
{$IFDEF USE_SSL}
    OverByteIcsSSLEAY,
{$ENDIF}
    OverbyteIcsStreams,
    {$I Include\OverbyteIcsZlib.inc}
    OverbyteIcsZlibHigh,
    {$IFDEF USE_ZLIB_OBJ}
        OverbyteIcsZLibObj,     {interface to access ZLIB C OBJ files}
    {$ELSE}
        OverbyteIcsZLibDll,     {interface to access zLib1.dll}
    {$ENDIF}
    OverbyteIcsTypes,
    OverbyteIcsUtils,
  {$IFDEF FMX}
    Ics.Fmx.OverbyteIcsSocketUtils,
    Ics.Fmx.OverbyteIcsWndControl,
    Ics.Fmx.OverbyteIcsWSocket,
    Ics.Fmx.OverbyteIcsWSocketS,
  {$ELSE}
    OverbyteIcsWndControl,
    { AG V1.51 }
    OverbyteIcsWSocket,
    OverbyteIcsSocketUtils,
    OverbyteIcsWSocketS, { angus V7.00 }
  {$ENDIF}
    OverbyteIcsFtpSrvT,
    OverbyteIcsOneTimePw,  { angus V1.54 }
    OverbyteIcsCRC,        { angus V1.54 }
    OverbyteIcsMD5,
    OverbyteIcsWSockBuf;   { AG V6.02 }



const
    FtpServerVersion         = 805;
    CopyRight : String       = ' TFtpServer (c) 1998-2014 F. Piette V8.05 ';
    UtcDateMaskPacked        = 'yyyymmddhhnnss';         { angus V1.38 }
    DefaultRcvSize           = 16384;    { V7.00 used for both xmit and recv, was 2048, too small }

    { Angus Nov 2007 - previously the values in this table did not match the
      command table, which is why dimensioning was incorrect, now corrected  }
const
    ftpcPORT      = 0;
    ftpcSTOR      = 1;
    ftpcRETR      = 2;
    ftpcCWD       = 3;
    ftpcXPWD      = 4;
    ftpcPWD       = 5;
    ftpcUSER      = 6;
    ftpcPASS      = 7;
    ftpcLIST      = 8;
    ftpcNLST      = 9;
    ftpcTYPE      = 10;
    ftpcSYST      = 11;
    ftpcQUIT      = 12;
    ftpcDELE      = 13;
    ftpcSIZE      = 14;
    ftpcREST      = 15;
    ftpcRNFR      = 16;
    ftpcRNTO      = 17;
    ftpcMKD       = 18;
    ftpcRMD       = 19;
    ftpcABOR      = 20;
    ftpcPASV      = 21;
    ftpcNOOP      = 22;
    ftpcCDUP      = 23;
    ftpcAPPE      = 24;
    ftpcSTRU      = 25;   {jsp - Added APPE and STRU types }
    ftpcXMKD      = 26;
    ftpcXRMD      = 27;
    ftpcMDTM      = 28;   {bkc - Added MDTM type           }
    ftpcMODE      = 29;
    ftpcOVER      = 31;
    ftpcSTOU      = 32;   {ep  - Added STOU type           }
    ftpcFEAT      = 33;   {SSV - Added FEAT type           }
    ftpcMLST      = 34;   {angus Added MLST type           }
    ftpcMLSD      = 35;   {angus Added MLSD type           }
    ftpcMFMT      = 36;   {angus Added MFMT type           }
    ftpcMD5       = 37;   {angus Added MD5 type            }
    ftpcXCRC      = 38;   {angus Added XCRC type           }
    ftpcXMD5      = 39;   {angus Added XMD5 type           }
    ftpcALLO      = 40;   {angus Added ALLO type           }
    ftpcCLNT      = 41;   {angus Added CLNT type           }
    ftpcOPTS      = 42;   {angus Added OPTS type           }
    ftpcSitePaswd = 43;   {angus Added SITE PASWD type     }
    ftpcSiteExec  = 44;   {angus Added SITE EXEC type      }
    ftpcSiteIndex = 45;   {angus Added SITE INDEX type     }
    ftpcSiteZone  = 46;   {angus Added SITE ZONE type      }
    ftpcSiteMsg   = 47;   {angus Added SITE MSG type       }
    ftpcSiteCmlsd = 48;   {angus Added SITE CMLSD type     }
    ftpcSiteDmlsd = 49;   {angus Added SITE DMLSD type     }
    ftpcCOMB      = 50;   {angus Added COMB                }
    ftpcXCMLSD    = 51;   {angus Added XCMLSD type         }
    ftpcXDMLSD    = 52;   {angus Added XDMLSD type         }
    ftpcHOST      = 53;   {angus Added HOST type           }
    ftpcREIN      = 54;   {angus Added REIN type           }
    ftpcLANG      = 55;   {angus Added LANG type           }
    ftpcEPRT      = 56;
    ftpcEPSV      = 57;
{$IFNDEF USE_SSL}
    ftpcLast      = 57;   {angus used to dimension FCmdTable}
{$ELSE}
    ftpcAUTH      = 58;
    ftpcCCC       = 59;
    ftpcPBSZ      = 60;   {V1.45}
    ftpcPROT      = 61;
    ftpcLast      = 61;
{$ENDIF}

  {$IFDEF POSIX}
    PathDelim       = '/';
  {$ELSE}
    PathDelim       = '\';
  {$ENDIF}

type
 { published server options }
    TFtpsOption      = (ftpsCwdCheck, ftpsCdupHome,      { angus V1.38 }
                        ftpsCalcMD5OnTheFly,             { AG V1.50 }
                        ftpsCalcCRCOnTheFly,             { angus V1.54 }
                        ftpsNoPasvIpAddrInLan,           { AG V1.51 }
                        ftpsNoPasvIpAddrSameSubnet,      { AG V1.51 }
                        ftpsHidePhysicalPath,            { AG V1.52 }
                        ftpsModeZCompress,               { angus V1.54 }
                        ftpsSiteXmlsd,                   { angus V1.54 }
                        ftpsThreadRecurDirs,             { angus V1.54 }
                        ftpsThreadAllDirs,               { angus V1.54 }
                        ftpsModeZNoResume,               { angus V1.55 }
                        ftpsEnableUtf8,                  { angus V7.01 support Utf8 }
                        ftpsDefaultUtf8On,               { angus V7.01 default Utf8 off is normal for ANSI compatibility }
                        ftpsAutoDetectCodePage,          { AG V7.02 actually detects UTF-8 only! }
                                                         { requires ftpsEnableUtf8 and sets ftpEnableUtf8   }
                                                         { once a valid UTF-8 buffer has been received from }
                        ftpsCompressDirs                 { angus V8.04 zmode compress directory listings }
                                                         { a client.                                        }
                         );
    TFtpsOptions     = set of TFtpsOption;               { angus V1.38 }

 { client options }
    TFtpOption    = (ftpcUNC,                { angus V1.39 }
                     ftpCwdCheck,
                     ftpCdupHome,
                     ftpHidePhysicalPath,    { AG V1.52 }
                     ftpModeZCompress,       { angus V1.54 }
                     ftpUtf8On,              { angus V7.01 this is changed by the OPTS UTF8 ON/OFF command }
                     ftpAutoDetectCodePage   { AG V7.02 actually detects UTF-8 only! }
                                             { requires ftpsEnableUtf8 and sets ftpEnableUtf8   }
                                             { once a valid UTF-8 buffer has been received from }
                                             { a client.                                        }
                     );      { angus V1.54 }
    TFtpOptions   = set of TFtpOption;

    PBoolean = ^Boolean;
    FtpServerException  = class(Exception);
    TFtpString = type String;

{$IFDEF USE_SSL}
    TFtpSslType  = (ftpAuthSsl,      ftpAuthTls,     ftpAuthTlsP,
                    ftpAuthTlsC ,    ftpImplicitSsl);
    TFtpSslTypes = set of TFtpSslType;
    TCurFtpSslType  = (curftpSslNone,   curftpAuthSsl,      curftpAuthTls,
                       curftpAuthTlsP,  curftpAuthTlsC ,    curftpImplicitSsl);
{$ENDIF}

    TFtpTransMode   = (ftpTransModeStream, ftpTransModeZDeflate) ;  { angus V1.54 }
    TZStreamState   = (ftpZStateNone, ftpZStateSaveDecom, ftpZStateSaveComp{,
                     ftpZStateImmDecon, ftpZStateImmComp});         { angus V1.54 }
    TListType        = (ListTypeName,
                        ListTypeUnix, ListTypeFacts);    { angus V1.54 same as Server }
type
    EFtpCtrlSocketException = class(Exception);
    TFtpCtrlState = (ftpcInvalid, ftpcWaitingUserCode, ftpcWaitingPassword,
                     ftpcReady, ftpcWaitingAnswer, ftpcFailedAuth);  { angus V7.06 }

    { TFtpCmdType is now defined as a byte and enumerated items as constants, }
    { so that new values can be added by sub-components who add new commands  }
    TFtpCmdType   = Byte;

type
    TDisplayEvent = procedure (Sender : TObject; Msg : String) of object;
    TCommandEvent = procedure (Sender : TObject; CmdBuf : PAnsiChar; CmdLen : Integer) of object; { AG 7.02 Convert the buffer in TriggerCommand}

    TFtpCtrlSocket = class; //Forward

    TClientProcessingThread = class(TThread)  { AG V1.46}
    public
        Client    : TFtpCtrlSocket;
        Keyword   : String;
        Params    : String;
        InData    : String;
        OutData   : String;
        AuxData   : String;        { AG V8.03 }
        ClientID  : Integer;
        StartTick : LongWord;      { angus V1.54 }
        Sender    : TObject;       { angus V1.54 }
    protected                                  { AG 7.02 }
        procedure TriggerEnterSecurityContext; { AG 7.02 }
        procedure TriggerLeaveSecurityContext; { AG 7.02 }
        procedure Execute; override;
    end;

    TFtpServer     = class; //Forward          { AG 7.02 }

    TFtpCtrlSocket = class(TWSocketClient)   { angus V7.00 }
    protected
        FDataSocket        : TWSocket;
        FRcvBuf            : PAnsiChar;
        FRcvCnt            : Integer;
        FRcvSize           : Integer;   { used for both smit and revc }
        FBusy              : Boolean;
        FConnectedSince    : TDateTime;
        FLastCommand       : TDateTime;
        FCommandCount      : LongInt;
        FBanner            : String;
        FUserName          : String;
        FPassWord          : String;
        FCloseRequest      : Boolean;
        FHomeDir           : String;
        FDirectory         : String;
        FFtpState          : TFtpCtrlState;
        FAbortingTransfer  : Boolean;
        FUserData          : LongInt;        { Reserved for component user }
        FPeerAddr          : String;
        FPeerSAddr         : TSockAddr;      { AG V1.47 }
        FHost              : String;         { angus V7.01 }
        FLang              : String;         { angus V7.01 }
        FOnDisplay         : TDisplayEvent;
        FOnCommand         : TCommandEvent;
        FOptions           : TFtpOptions;    { AG 7.02 }
        FCodePage          : LongWord;       { AG 7.02 }
        FCurrentCodePage   : LongWord;       { AG 7.02 }
        FEpsvAllArgReceived: Boolean;
        FSndBufSize        : Integer;        { Angus V7.19}
        FRcvBufSize        : Integer;        { Angus V7.19}
        procedure TriggerSessionConnected(Error : Word); override;
        function  TriggerDataAvailable(Error : Word) : boolean; override;
        procedure TriggerCommand(CmdBuf : PAnsiChar; CmdLen : Integer); virtual; { AG 7.02 }
        procedure SetRcvSize(newValue : Integer);
        procedure SetHomeDir(const newValue: String);   { AG V1.52}
        procedure SetOptions(const Opts : TFtpOptions); { AG 7.02 }
        procedure SetCodePage(const Value: LongWord);   { AG 7.02 }
        procedure SetCurrentCodePage(const Value: LongWord); { AG 7.02 }
        procedure SetOnBgException(const Value: TIcsBgExceptionEvent); override; { V7.15 }
        procedure SetRcvBufSize(newValue : Integer);   { Angus V7.19}
        procedure SetSndBufSize(newValue : Integer);   { Angus V7.19}
    public
        FtpServer         : TFtpServer; { AG V7.02 }
        BinaryMode        : Boolean;
        DataAddr          : String;
        DataPort          : String;
        FileName          : String;
        FilePath          : String;
        DataSessionActive : Boolean;
        DataStream        : TStream;
        HasOpenedFile     : Boolean;
        TransferError     : String;
        DataSent          : Boolean;
        CurCmdType        : TFtpCmdType;
        MD5Digest         : TMD5Digest;  { AG V1.46}
        MD5Context        : TMD5Context; { AG V1.46}
        MD5OnTheFlyFlag   : Boolean;     { AG V1.46}
        ProcessingThread  : TClientProcessingThread; { AG V1.46}
        AnswerDelayed     : Boolean;     { AG V1.46}
        ByteCount         : Int64;       { upload or download bytes for current data session }
        RestartPos        : Int64;
        HashStartPos      : Int64;       { angus V1.54 start for MD5/CRC }
        HashEndPos        : Int64;       { angus V1.54 start for MD5/CRC }
        FromFileName      : String;
        ToFileName        : String;
        PassiveMode       : Boolean;
        PassiveStart      : Boolean;
        PassiveConnected  : Boolean;
        OtpMethod         : TOtpMethod;  { angus V1.54 One Time Password authentication method }
        OtpSequence       : Integer;     { angus V1.54 One Time Password current sequence }
        OtpSeed           : String;      { angus V1.54 One Time Password current seed }
        LastTick          : Longword;    { angus V1.54 last tick for time out checking }
        ClntStr           : String;      { angus V1.54 from clnt command }
        DirListPath       : String;      { angus V1.54 last parsed directory listing path }
        DirListSubDir     : Boolean;     { angus V1.54 did we list subdirs }
        DirListHidden     : Boolean;     { angus V1.54 did we list hidden files }
        DirListType       : TListType;   { angus V1.54 how we list files }
        CurrTransMode     : TFtpTransMode; {angus V1.54 current zlib transfer mode }
        ZStreamState      : TZStreamState; { angus V1.54 current Zlib stream state }
        ZReqLevel         : Integer;     { angus V1.54 requested Zlib compression level 1 to 9 }
        ZCurLevel         : Integer;     { angus V1.54 current Zlib compression level 0 to 9 }
   {    ZStreamRec        : TZStreamRec;   angus V1.54 Zlib stream control record for immediate mode }
        ZCompFileName     : String;      { angus V1.54 zlib file name of compressed file }
        ZFileStream       : TStream;     { angus V1.54 Zlib compressed file stream  }
        ZCompInfo         : String;      { angus V1.54 zlib compress information to return with 251 OK }
        ZCompFileDelete   : Boolean;     { angus V1.54 zlib delete compressed file when closing it }
        SessStartTick     : Longword;    { angus V1.54 tick when client session started, for duration check }
        ReqStartTick      : Longword;    { angus V1.54 tick when last request started, for duration check }
        XferStartTick     : Longword;    { angus V1.54 tick when last xfer started, for performance check }
        ReqDurMilliSecs   : Integer;     { angus V1.54 how long last request took, in ticks }
        TotGetBytes       : Int64;       { angus V1.54 how many bytes GET during session, data and control }
        TotPutBytes       : Int64;       { angus V1.54 how many bytes PUT during session, data and control }
        SessIdInfo        : String;      { angus V1.54 session identificaton information for application use }
        FileModeRead      : Word;        { angus V1.57 file access fmOpenxx and fmSharexx flags for read  }
        FileModeWrite     : Word;        { angus V1.57 file access fmOpenxx and fmSharexx flags for write }
        AccountIniName    : String;      { angus V7.00 client account information, INI file }
        AccountPassword   : String;      { angus V7.00 client account expected password }
        AccountReadOnly   : Boolean;     { angus V7.00 client account read only file access, no uploads  }
        FailedAttempts    : Integer;     { angus V7.06 }
        DelayAnswerTick   : Longword;    { angus V7.06 tick when delayed answer should be sent }
{$IFDEF BUILTIN_THROTTLE}
        CBandwidthLimit    : LongWord;   { angus V7.12 Bytes per second, null = disabled }
        CBandwidthSampling : LongWord;   { angus V7.12 Msec sampling interval }
{$ENDIF}
{$IFDEF USE_SSL}
        ProtP             : Boolean;
        AuthFlag          : Boolean;
        CccFlag           : Boolean;
        FtpSslTypes       : TFtpSslTypes;
        CurFtpSslType     : TCurFtpSslType;
{$ENDIF}
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   SendAnswer(const Answer : RawByteString);         { angus V7.01 }{ AG V7.02 }
        procedure   SetDirectory(newValue : String); virtual;
        procedure   SetAbortingTransfer(newValue : Boolean);
        procedure   BuildDirList(var TotalFiles: integer); virtual; {angus V7.08 was BuildDirectory, made virtual }
        procedure   TriggerSessionClosed(Error : Word); override;
{$IFDEF USE_SSL}
        function    SslSendPlain(Data : TWSocketData; Len : Integer) : Integer;
{$ENDIF}
        procedure   DataStreamWriteString(const Str: AnsiString);  overload; { AG 7.02 }
        procedure   DataStreamWriteString(const Str: AnsiString; DstCodePage: LongWord);  overload; { AG 7.02 }
    {$IFDEF COMPILER12_UP}
        procedure   DataStreamWriteString(const Str: UnicodeString; DstCodePage: LongWord); overload;
        procedure   DataStreamWriteString(const Str: UnicodeString); overload;
    {$ENDIF}
        procedure   DataStreamReadString(var Str: AnsiString; Len: TFtpBigInt); overload;
        procedure   DataStreamReadString(var Str: AnsiString; Len: TFtpBigInt; SrcCodePage: LongWord); overload;    { AG 7.02 }
    {$IFDEF COMPILER12_UP}
        procedure   DataStreamReadString(var Str: UnicodeString; Len: TFtpBigInt; SrcCodePage: LongWord); overload; { AG 7.02 }
        procedure   DataStreamReadString(var Str: UnicodeString; Len: TFtpBigInt); overload;
    {$ENDIF}
        property    DataSocket     : TWSocket    read  FDataSocket;
        property    CodePage       : LongWord    read  FCodePage        { AG 7.02 }
                                                 write SetCodePage;
        property    CurrentCodePage: LongWord    read  FCurrentCodePage { AG 7.02 }
                                                 write SetCurrentCodePage;
        property    ConnectedSince : TDateTime   read  FConnectedSince;
        property    LastCommand    : TDateTime   read  FLastCommand;
        property    CommandCount   : LongInt     read  FCommandCount;
        property    RcvBuf         : PAnsiChar   read  FRcvBuf;
        property    RcvdCount;
        property    CloseRequest   : Boolean     read  FCloseRequest
                                                 write FCloseRequest;
        property    Directory      : String      read  FDirectory
                                                 write SetDirectory;
        property    HomeDir        : String      read  FHomeDir
                                                 write SetHomeDir;  { AG V1.52}
        property    AbortingTransfer : Boolean   read  FAbortingTransfer
                                                 write SetAbortingTransfer;
        property    ID             : LongInt     read  FCliId       { angus V7.00 }
                                                 write FCliId;
        property    Options        : TFtpOptions read  FOptions
                                                 write SetOptions;
        property    PeerSAddr      : TSockAddr   read  FPeerSAddr;  { AG V1.47 }
        property    ReadCount      : Int64       read  FReadCount;

    published
        property    FtpState       : TFtpCtrlState
                                                 read  FFtpState
                                                 write FFtpState;
        property    Banner         : String      read  FBanner
                                                 write FBanner;
        property    RcvSize        : Integer     read  FRcvSize
                                                 write SetRcvSize;
        property    Busy           : Boolean     read  FBusy
                                                 write FBusy;
        property    UserName       : String      read  FUserName
                                                 write FUserName;
        property    PassWord       : String      read  FPassWord
                                                 write FPassWord;
        property    UserData       : LongInt     read  FUserData
                                                 write FUserData;
        property    Host           : String      read  FHost
                                                 write FHost;
        property    Lang           : String      read  FHost
                                                 write FHost;
        property    SndBufSize     : Integer     read FSndBufSize       { Angus V7.19}
                                                 write SetSndBufSize;
        property    RcvBufSize     : Integer     read FRcvBufSize       { Angus V7.19}
                                                 write SetRcvBufSize;
        property    OnDisplay      : TDisplayEvent
                                                 read  FOnDisplay
                                                 write FOnDisplay;
        property    OnCommand      : TCommandEvent
                                                 read  FOnCommand
                                                 write FOnCommand;
        property    OnSessionClosed;
        property    OnDataSent;
        property    HSocket;
        property    AllSent;
        property    State;
{$IFNDEF NO_DEBUG_LOG}
        property    IcsLogger;
{$ENDIF}
    end;

    TFtpCtrlSocketClass = class of TFtpCtrlSocket;
    TFtpSrvAuthenticateEvent  =  procedure (Sender   : TObject;
                                            Client   : TFtpCtrlSocket;
                                            UserName : TFtpString;
                                            Password : TFtpString;
                                            var Authenticated : Boolean) of object;
    TFtpSrvOtpMethodEvent  =  procedure (Sender   : TObject;                      { angus V1.54 }
                                         Client   : TFtpCtrlSocket;
                                         UserName : TFtpString;
                                         var OtpMethod : TOtpMethod) of object;
    TFtpSrvOtpGetPasswordEvent =  procedure (Sender           : TObject;
                                             Client           : TFtpCtrlSocket;
                                             UserName         : TFtpString;
                                             var UserPassword : String) of object; { angus V1.54 }
    TFtpSrvChangeDirectoryEvent =  procedure (Sender      : TObject;
                                              Client      : TFtpCtrlSocket;
                                              Directory   : TFtpString;
                                              var Allowed : Boolean) of object;
    TFtpSrvBuildDirectoryEvent =  procedure (Sender        : TObject;
                                             Client        : TFtpCtrlSocket;
                                             var Directory : TFtpString;
                                             Detailed      : Boolean) of object;
    TFtpSrvClientConnectEvent = procedure (Sender  : TObject;
                                           Client  : TFtpCtrlSocket;
                                           AError  : Word) of object;
    TFtpSrvDataSessionConnectedEvent = procedure (Sender  : TObject;
                                                  Client  : TFtpCtrlSocket;
                                                  Data    : TWSocket;
                                                  AError  : Word) of object;
    TFtpSrvClientCommandEvent = procedure (Sender        : TObject;
                                           Client        : TFtpCtrlSocket;
                                           var Keyword   : TFtpString;
                                           var Params    : TFtpString;
                                           var Answer    : TFtpString) of object;
    TFtpSrvAnswerToClientEvent = procedure (Sender        : TObject;
                                            Client        : TFtpCtrlSocket;
                                            var Answer    : TFtpString) of object;
    TFtpSrvValidateXferEvent  = procedure (Sender        : TObject;
                                           Client        : TFtpCtrlSocket;
                                           var FilePath  : TFtpString;
                                           var Allowed   : Boolean) of object;
    TFtpSrvCalculateMd5Event  = procedure (Sender        : TObject;        { angus V1.39 }
                                           Client        : TFtpCtrlSocket;
                                           var FilePath  : TFtpString;
                                           var Md5Sum    : TFtpString;
                                           var Allowed   : Boolean) of object;
    TFtpSrvMd5CalculatedEvent = procedure (Sender         : TObject;       { AG V1.50 }
                                           Client         : TFtpCtrlSocket;
                                           const FilePath : TFtpString;
                                           const Md5Sum   : TFtpString) of object;
    TFtpSrvOnPasvIpAddrEvent = procedure  (Sender : TObject;               { AG V1.51 }
                                           Client : TFtpCtrlSocket;
                                           var APasvIpAddr: TFtpString;
                                           var SetPasvIpAddr : Boolean) of object;
    TFtpSrvBuildFilePathEvent = procedure (Sender        : TObject;
                                           Client        : TFtpCtrlSocket;
                                           const Directory   : String;
                                           const FileName    : String;
                                           var   NewFileName : String) of object;
    TFtpSrvDataAvailableEvent = procedure (Sender : TObject;
                                           Client : TFtpCtrlSocket;
                                           Data   : TWSocket;
                                           Buf    : PAnsiChar;  { AG V6.02 }
                                           Len    : LongInt;
                                           AError : Word) of object;
    TFtpSrvRetrDataSentEvent  = procedure (Sender : TObject;
                                           Client : TFtpCtrlSocket;
                                           Data   : TWSocket;
                                           AError : Word) of object;
    TFtpSrvGetUniqueFileNameEvent = procedure (Sender       : TObject;
                                               Client       : TFtpCtrlSocket;
                                               var FileName : TFtpString) of object;
    TFtpSrvGetProcessingEvent     = procedure (Sender          : TObject;
                                               Client          : TFtpCtrlSocket;
                                               var DelayedSend : Boolean) of object;
    TFtpSrvCommandProc        = procedure (Client        : TFtpCtrlSocket;
                                           var Keyword   : TFtpString;
                                           var Params    : TFtpString;
                                           var Answer    : TFtpString) of object;
    TFtpSrvCommandTableItem   = record
                                    KeyWord : String;
                                    Proc    : TFtpSrvCommandProc;
                                end;
    TFtpSrvCommandTable = array of TFtpSrvCommandTableItem; { AG V7.11 }

    TFtpSecurityContextEvent  = procedure (Sender : TObject;     { AG V1.52 }
                                           Client : TFtpCtrlSocket) of object;
    TFtpSrvGeneralEvent = procedure (Sender        : TObject;      { angus V1.54 }
                                     Client        : TFtpCtrlSocket;
                                     var Params    : TFtpString;
                                     var Answer    : TFtpString) of object;
    TFtpSrvTimeoutEvent =  procedure (Sender      : TObject;               { angus V1.54 }
                                      Client      : TFtpCtrlSocket;
                                      Duration    : Integer;
                                      var Abort   : Boolean) of object;
    TFtpSrvCompressFileEvent  = procedure (Sender        : TObject;        { angus V1.54 }
                                           Client        : TFtpCtrlSocket;
                                           var Done      : Boolean) of object;
    TFtpSrvCompressedFileEvent = procedure (Sender       : TObject;        { angus V1.54 }
                                            Client       : TFtpCtrlSocket) of object;
    TFtpSrvDisplayEvent = procedure (Sender        : TObject;      { angus V1.54 }
                                     Client        : TFtpCtrlSocket;
                                     Msg           : TFtpString) of object;
    TFtpSrvHostEvent =  procedure (Sender      : TObject;                { angus V7.01 }
                                   Client      : TFtpCtrlSocket;
                                   Host        : TFtpString;
                                   var Allowed : Boolean) of object;
    TFtpSrvReinEvent =  procedure (Sender      : TObject;                { angus V7.01 }
                                   Client      : TFtpCtrlSocket;
                                   var Allowed : Boolean) of object;
    TFtpSrvLangEvent =  procedure (Sender      : TObject;                { angus V7.01 }
                                   Client      : TFtpCtrlSocket;
                                   Lang        : TFtpString;
                                   var Allowed : Boolean) of object;
    TFtpSrvAddVirtFilesEvent = procedure (Sender          : TObject;             { angus V7.08 }
                                          Client          : TFtpCtrlSocket;
                                          var LocFiles    : TIcsFileRecs;
                                          var LocFileList : TList;
                                          var TotalFiles  : Integer;
                                          ProgressCallback: TMD5Progress) of object;

    TFtpServer = class(TIcsWndControl)
    protected
        FAddr                   : String;
        FSocketFamily           : TSocketFamily;
        FPort                   : String;
        FListenBackLog          : Integer;
        FBanner                 : String;
        FSocketServer           : TWSocketServer ;    { new  angus V7.00 }
        FClientClass            : TFtpCtrlSocketClass;
        FMaxClients             : LongInt;
        FCmdTable               : TFtpSrvCommandTable; { AG V7.11 }
        FLastCmd                : Integer;
        FUserData               : LongInt;      { Reserved for component user }
        FPasvPortRangeStart     : Integer;
        FPasvPortRangeSize      : Integer;
        FPasvPortTable          : PBoolean;
        FPasvPortTableSize      : Integer;
        FPasvIpAddr             : String;
        FPasvNextNr             : Integer;      { angus V1.56 }
        FMd5UseThreadFileSize   : Integer;      { AG V1.50 }
        FTimeoutSecsLogin       : Integer;      { angus V1.54 }
        FTimeoutSecsIdle        : Integer;      { angus V1.54 }
        FTimeoutSecsXfer        : Integer;      { angus V1.54 }
        FEventTimer             : TIcsTimer;    { angus V1.54 }
        FZlibMinLevel           : Integer;      { angus V1.54 }
        FZlibMaxLevel           : Integer;      { angus V1.54 }
        FZlibNoCompExt          : String;       { angus V1.54 }
        FZlibWorkDir            : String;       { angus V1.54 }
        FZlibMinSpace           : Integer;      { angus V1.54 }
        FAlloExtraSpace         : Integer;      { angus V1.54 }
        FZlibMaxSize            : Int64;        { angus V1.55 }
        FCodePage               : LongWord;     { angus V7.01 for UTF8 support }
        FLanguage               : String;       { angus V7.01 for UTF8 support }
        FMaxAttempts            : Integer;      { angus V7.06 }
        FBindFtpData            : Boolean;
{$IFDEF BUILTIN_THROTTLE}
        FBandwidthLimit         : LongWord;     { angus V7.12 Bytes per second, null = disabled }
        FBandwidthSampling      : LongWord;     { angus V7.12 Msec sampling interval }
{$ENDIF}
        FMsg_WM_FTPSRV_CLOSE_REQUEST  : UINT;
        FMsg_WM_FTPSRV_ABORT_TRANSFER : UINT;
        FMsg_WM_FTPSRV_CLOSE_DATA     : UINT;
        FMsg_WM_FTPSRV_START_SEND     : UINT;
        FOnStart                : TNotifyEvent;
        FOnStop                 : TNotifyEvent;
        FOnAuthenticate         : TFtpSrvAuthenticateEvent;
        FOnOtpMethod            : TFtpSrvOtpMethodEvent;           { angus V1.54 }
        FOnOtpGetPassword       : TFtpSrvOtpGetPasswordEvent;      { angus V1.54 }
        FOnClientConnect        : TFtpSrvClientConnectEvent;
        FOnClientDisconnect     : TFtpSrvClientConnectEvent;
        FOnClientCommand        : TFtpSrvClientCommandEvent;
        FOnAnswerToClient       : TFtpSrvAnswerToClientEvent;
        FOnChangeDirectory      : TFtpSrvChangeDirectoryEvent;
        FOnMakeDirectory        : TFtpSrvChangeDirectoryEvent;
        FOnBuildDirectory       : TFtpSrvBuildDirectoryEvent;
        FOnAlterDirectory       : TFtpSrvBuildDirectoryEvent;
        FOnValidatePut          : TFtpSrvValidateXferEvent;
        FOnValidateSize         : TFtpSrvValidateXferEvent;
        FOnValidateDele         : TFtpSrvValidateXferEvent;
        FOnValidateRmd          : TFtpSrvValidateXferEvent;
        FOnValidateRnFr         : TFtpSrvValidateXferEvent;
        FOnValidateRnTo         : TFtpSrvValidateXferEvent;
        FOnStorSessionConnected : TFtpSrvDataSessionConnectedEvent;
        FOnStorSessionClosed    : TFtpSrvDataSessionConnectedEvent;
        FOnStorDataAvailable    : TFtpSrvDataAvailableEvent;
        FOnValidateGet          : TFtpSrvValidateXferEvent;
        FOnRetrSessionConnected : TFtpSrvDataSessionConnectedEvent;
        FOnRetrSessionClosed    : TFtpSrvDataSessionConnectedEvent;
        FOnRetrDataSent         : TFtpSrvRetrDataSentEvent;
        FOnGetUniqueFileName    : TFtpSrvGetUniqueFileNameEvent;
        FOnGetProcessing        : TFtpSrvGetProcessingEvent;
        FOnBuildFilePath        : TFtpSrvBuildFilePathEvent; { serge le 5/10/2002 }
        FOnValidateMfmt         : TFtpSrvValidateXferEvent;  { angus V1.39 }
        FOnCalculateMd5         : TFtpSrvCalculateMd5Event;  { angus V1.39 }
        FOnCalculateCrc         : TFtpSrvCalculateMd5Event;  { angus V1.54 }
        FOptions                : TFtpsOptions;
        FOnMd5Calculated        : TFtpSrvMd5CalculatedEvent; { AG V1.50 }
        FOnCrcCalculated        : TFtpSrvMd5CalculatedEvent; { angus V1.54 }
        FOnPasvIpAddr           : TFtpSrvOnPasvIpAddrEvent;  { AG V1.51 }
        FOnEnterSecurityContext : TFtpSecurityContextEvent;  { AG V1.52 }
        FOnLeaveSecurityContext : TFtpSecurityContextEvent;  { AG V1.52 }
        FOnValidateAllo         : TFtpSrvGeneralEvent;       { angus V1.54 }
        FOnClntStr              : TFtpSrvGeneralEvent;       { angus V1.54 }
        FOnSiteMsg              : TFtpSrvGeneralEvent;       { angus V1.54 }
        FOnSiteExec             : TFtpSrvGeneralEvent;       { angus V1.54 }
        FOnSitePaswd            : TFtpSrvGeneralEvent;       { angus V1.54 }
        FOnCombine              : TFtpSrvGeneralEvent;       { angus V1.54 }
        FOnTimeout              : TFtpSrvTimeoutEvent;       { angus V1.54 }
        FOnDownCompressFile     : TFtpSrvCompressFileEvent;  { angus V1.54 }
        FOnUpCompressFile       : TFtpSrvCompressFileEvent;  { angus V1.54 }
        FOnUpCompressedFile     : TFtpSrvCompressedFileEvent; { angus V1.54 }
        FOnDisplay              : TFtpSrvDisplayEvent;       { angus V1.54 }
        FOnHost                 : TFtpSrvHostEvent;          { angus V7.01 }
        FOnRein                 : TFtpSrvReinEvent;          { angus V7.01 }
        FOnLang                 : TFtpSrvLangEvent;          { angus V7.01 }
        FSystemCodePage         : LongWord;                  { AG 7.02 }
        FOnAddVirtFiles         : TFtpSrvAddVirtFilesEvent;  { angus V7.08 }
        procedure CreateSocket; virtual;
        function  GetMultiListenIndex: Integer;
        function  GetMultiListenSockets: TWSocketMultiListenCollection;
        procedure SetMultiListenSockets(const Value: TWSocketMultiListenCollection);
        procedure SetOnBgException(const Value: TIcsBgExceptionEvent); override; { V7.15 }
{$IFNDEF NO_DEBUG_LOG}
        function  GetIcsLogger: TIcsLogger;                                      { V1.46 }
        procedure SetIcsLogger(const Value: TIcsLogger);                         { V1.46 }
        procedure DebugLog(LogOption: TLogOption; const Msg : string); virtual;  { V1.46 }
        function  CheckLogOptions(const LogOption: TLogOption): Boolean; virtual;{ V1.46 }
{$ENDIF}
        procedure ClientProcessingThreadTerminate(Sender : TObject); { AG V1.50 }
        procedure Notification(AComponent: TComponent; operation: TOperation); override;
        procedure ServSocketStateChange(Sender : TObject; OldState, NewState : TSocketState);
        procedure ClientDataSent(Sender : TObject; AError  : Word); virtual; { V1.47 }
        procedure ClientCommand(Sender : TObject; CmdBuf : PAnsiChar; CmdLen : Integer); { AG 7.02 }
        procedure ClientPassiveSessionAvailable(Sender : TObject; AError  : Word); virtual; {AG SSL}
        procedure ClientStorSessionConnected(Sender : TObject; AError  : Word);
        procedure ClientStorSessionClosed(Sender : TObject; AError  : Word);
        procedure ClientStorDataAvailable(Sender: TObject; AError  : word); virtual;
        procedure ClientRetrSessionConnected(Sender : TObject; AError  : Word); virtual;
        procedure ClientRetrSessionClosed(Sender : TObject; AError  : Word);
        procedure ClientRetrDataSent(Sender : TObject; AError  : Word);
        procedure SendAnswer(Client : TFtpCtrlSocket; Answer : TFtpString);  virtual; {AG SSL}
        procedure SendNextDataChunk(Client : TFtpCtrlSocket; Data : TWSocket); virtual;
        procedure StartSendData(Client : TFtpCtrlSocket);
        procedure PrepareStorDataSocket(Client : TFtpCtrlSocket);
        procedure PreparePassiveStorDataSocket(Client : TFtpCtrlSocket);
        procedure PreparePassiveRetrDataSocket(Client : TFtpCtrlSocket);
        function  IsPathAllowed(Client : TFtpCtrlSocket; const Path : String;
                                ExcludeBackslash : Boolean = FALSE): Boolean; { V1.52 AG}
        function  FormatResponsePath(Client: TFtpCtrlSocket; const InPath : TFtpString): TFtpString; { AG V1.52 angus V7.08 }
        procedure BuildDirectory(Client : TFtpCtrlSocket; var Path : TFtpString); { angus V1.54, V7.08 }
        procedure EventTimerOnTimer(Sender : TObject);                            { angus V1.54 }
        procedure ServerClientConnect(Sender: TObject;
                                Client: TWSocketClient; Error: Word);    { angus V7.00 }
        procedure ServerClientDisconnect(Sender: TObject;
                                Client: TWSocketClient; Error: Word);    { angus V7.00 }

        procedure TriggerServerStart; virtual;
        procedure TriggerServerStop; virtual;
        procedure TriggerAuthenticate(Client            : TFtpCtrlSocket;
                                      UserName          : String;
                                      PassWord          : String;
                                      var Authenticated : Boolean); virtual;
        procedure TriggerOtpMethod   (Client   : TFtpCtrlSocket;
                                      UserName : TFtpString;
                                      var OtpMethod : TOtpMethod); virtual; { angus V1.54 }
        procedure TriggerOtpGetPassword(Client           : TFtpCtrlSocket;
                                        UserName         : TFtpString;
                                        var UserPassword : String); virtual; { angus V1.54 }
        procedure TriggerChangeDirectory(Client         : TFtpCtrlSocket;
                                         Directory      : String;
                                         var Allowed    : Boolean); virtual;
        procedure TriggerMakeDirectory(Client         : TFtpCtrlSocket;
                                       Directory      : String;
                                       var Allowed    : Boolean); virtual;
        procedure TriggerBuildDirectory(Client        : TFtpCtrlSocket;
                                        var Params    : TFtpString;
                                        Detailed      : Boolean); virtual;
        procedure TriggerAlterDirectory(Client        : TFtpCtrlSocket;
                                        var Params    : TFtpString;
                                        Detailed      : Boolean); virtual;
        procedure TriggerSendAnswer(Client : TFtpCtrlSocket;
                                    var Answer : TFtpString); virtual;
        procedure TriggerClientConnect(Client : TFtpCtrlSocket; AError  : Word); virtual;
        procedure TriggerClientDisconnect(Client : TFtpCtrlSocket; AError  : Word); virtual;
        procedure TriggerClientCommand(Client      : TFtpCtrlSocket;
                                       var Keyword : TFtpString;
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerStorSessionConnected(Client : TFtpCtrlSocket;
                                              Data   : TWSocket;
                                              AError : Word); virtual;
        procedure TriggerStorSessionClosed(Client : TFtpCtrlSocket;
                                           Data   : TWSocket;
                                           AError : Word); virtual;
        procedure TriggerValidatePut(Client        : TFtpCtrlSocket;
                                     var FilePath  : TFtpString;
                                     var Allowed   : Boolean); virtual;
        procedure TriggerValidateSize(Client        : TFtpCtrlSocket;
                                      var FilePath  : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerValidateDele(Client        : TFtpCtrlSocket;
                                      var FilePath  : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerValidateRmd(Client        : TFtpCtrlSocket;
                                     var FilePath  : TFtpString;
                                     var Allowed   : Boolean); virtual;
        procedure TriggerValidateRnFr(Client        : TFtpCtrlSocket;
                                      var FilePath  : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerValidateRnTo(Client        : TFtpCtrlSocket;
                                      var FilePath  : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerRetrSessionConnected(Client : TFtpCtrlSocket;
                                              Data   : TWSocket;
                                              AError : Word); virtual;
        procedure TriggerRetrSessionClosed(Client : TFtpCtrlSocket;
                                           Data   : TWSocket;
                                           AError : Word); virtual;
        procedure TriggerValidateGet(Client        : TFtpCtrlSocket;
                                     var FilePath  : TFtpString;
                                     var Allowed   : Boolean); virtual;
        procedure TriggerStorDataAvailable(Client : TFtpCtrlSocket;
                                       Data   : TWSocket;
                                       Buf    : PAnsiChar;  { AG V6.02 }
                                       Len    : LongInt;
                                       AError : Word); virtual;
        procedure TriggerRetrDataSent(Client : TFtpCtrlSocket;
                                      Data   : TWSocket;
                                      AError : Word); virtual;
        procedure TriggerGetUniqueFileName(Client       : TFtpCtrlSocket;
                                           var FileName : TFtpString); virtual;
        procedure TriggerBuildFilePath(Client            : TFtpCtrlSocket;
                                       const Directory   : String;
                                       const FileName    : String;
                                       var   NewFileName : String); virtual;
        procedure TriggerValidateMfmt(Client        : TFtpCtrlSocket;   { angus V1.39 }
                                      var FilePath  : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerCalculateMd5 (Client        : TFtpCtrlSocket;   { angus V1.39 }
                                      var FilePath  : TFtpString;
                                      var Md5Sum    : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerMd5Calculated(Client         : TFtpCtrlSocket;  { AG V1.50 }
                                      const FilePath  : TFtpString;
                                      const Md5Sum    : TFtpString); virtual;
        procedure TriggerCalculateCrc (Client        : TFtpCtrlSocket;   { angus V1.54 }
                                      var FilePath  : TFtpString;
                                      var Md5Sum    : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerCrcCalculated(Client         : TFtpCtrlSocket;  { angus V1.54 }
                                      const FilePath  : TFtpString;
                                      const Md5Sum    : TFtpString); virtual;
        procedure TriggerEnterSecurityContext(Client : TFtpCtrlSocket); virtual; { AG V1.52 }
        procedure TriggerLeaveSecurityContext(Client : TFtpCtrlSocket); virtual; { AG V1.52 }
        procedure TriggerValidateAllo (Client      : TFtpCtrlSocket;           { angus V1.54 }
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerClntStr      (Client      : TFtpCtrlSocket;           { angus V1.54 }
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerSiteMsg      (Client      : TFtpCtrlSocket;           { angus V1.54 }
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerSiteExec     (Client      : TFtpCtrlSocket;           { angus V1.54 }
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerSitePaswd    (Client      : TFtpCtrlSocket;           { angus V1.54 }
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerCombine      (Client      : TFtpCtrlSocket;           { angus V1.54 }
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerTimeout      (Client      : TFtpCtrlSocket;            { angus V1.54 }
                                       Duration    : Integer;
                                       var Abort   : Boolean); virtual;
        procedure TriggerDownCompressFile (Client    : TFtpCtrlSocket;          { angus V1.54 }
                                           var Done  : Boolean); virtual;
        procedure TriggerUpCompressFile (Client    : TFtpCtrlSocket;            { angus V1.54 }
                                         var Done  : Boolean); virtual;
        procedure TriggerUpCompressedFile (Client  : TFtpCtrlSocket); virtual;  { angus V1.54 }
        procedure TriggerDisplay       (Client      : TFtpCtrlSocket;
                                       Msg         : TFtpString); virtual;      { angus V1.54 }
        procedure TriggerHost          (Client        : TFtpCtrlSocket;
                                        Host          : TFtpString;
                                        var Allowed   : Boolean); virtual;      { angus V7.01 }
        procedure TriggerRein          (Client        : TFtpCtrlSocket;
                                        var Allowed   : Boolean); virtual;      { angus V7.01 }
        procedure TriggerLang          (Client        : TFtpCtrlSocket;
                                        Lang          : TFtpString;
                                        var Allowed   : Boolean); virtual;      { angus V7.01 }
        procedure TriggerAddVirtFiles  (Client          : TFtpCtrlSocket;
                                        var LocFiles    : TIcsFileRecs;
                                        var LocFileList : TList;
                                        var TotalFiles  : Integer;
                                        ProgressCallback: TMD5Progress); virtual; { angus V7.08 }
        function BuildFilePath(Client      : TFtpCtrlSocket;
                               Directory   : String;
                               FileName    : String) : String; virtual;
        function  GetClientCount : Integer; virtual;
        function  GetClient(nIndex : Integer) : TFtpCtrlSocket; virtual;
{ !!!!!!!!!!!!!!!! NGB: Added next two lines }
        procedure FreeCurrentPasvPort(AClient : TFtpCtrlSocket);
        function  GetNextAvailablePasvPort : String;
{ !!!!!!!!!!!!!!!! NGB: Added last two lines }
        function  GetActive : Boolean;
        procedure SetActive(newValue : Boolean);
        procedure SetPasvPortRangeSize(const NewValue: Integer);
        procedure SetPasvPortRangeStart(const NewValue: Integer);
        procedure SetClientClass(const NewValue: TFtpCtrlSocketClass);     { angus V7.00 }
        procedure AddCommand(const Keyword : String;
                             const Proc : TFtpSrvCommandProc); virtual;
        procedure WMFtpSrvCloseRequest(var msg: TMessage); virtual;
        procedure WMFtpSrvAbortTransfer(var msg: TMessage); virtual;
        procedure WMFtpSrvCloseData(var msg: TMessage); virtual;
        procedure WMFtpSrvStartSend(var msg: TMessage); virtual;
        procedure CommandDirectory(Client      : TFtpCtrlSocket;
                                   var Keyword : TFtpString;
                                   var Params  : TFtpString;
                                   var Answer  : TFtpString;
                                   Detailed    : Boolean); virtual;
        procedure CommandDirectory2(Client      : TFtpCtrlSocket;
                                   var Keyword : TFtpString;
                                   var Params  : TFtpString;
                                   var Answer  : TFtpString;
                                   ListType    : TListType);
        procedure CommandUSER(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPASS(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandQUIT(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandNOOP(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandLIST(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandNLST(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandDELE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSIZE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandREST(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandRNFR(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandRNTO(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPORT(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSTOR(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandRETR(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandTYPE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandCWD (Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandChangeDir(Client : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMKD (Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandRMD (Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandCDUP(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandXPWD(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPWD (Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSYST(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandABOR(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPASV(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandAPPE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSTRU(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMDTM(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMODE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandOverflow(Client  : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSTOU(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandFEAT(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMLST(Client      : TFtpCtrlSocket;   { angus V1.38 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMLSD(Client      : TFtpCtrlSocket;   { angus V1.38 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMD5 (Client      : TFtpCtrlSocket;   { angus V1.39 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandXCRC (Client     : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandALLO (Client     : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandCLNT (Client     : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandOPTS (Client     : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSitePaswd (Client : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSiteExec (Client : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSiteIndex (Client : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSiteZone (Client : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSiteMsg (Client  : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSiteCmlsd (Client : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSiteDmlsd (Client : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandComb (Client     : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandXCmlsd (Client : TFtpCtrlSocket;     { angus V7.01 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandXDmlsd (Client : TFtpCtrlSocket;     { angus V7.01 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandHost (Client : TFtpCtrlSocket;       { angus V7.01 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandRein (Client : TFtpCtrlSocket;       { angus V7.01 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandLang (Client : TFtpCtrlSocket;       { angus V7.01 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandEPRT(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandEPSV(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;

    public
        SrvFileModeRead   : Word;   { angus V1.57 }
        SrvFileModeWrite  : Word;   { angus V1.57 }
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Start;
        procedure   Stop;
        procedure   Disconnect(Client : TFtpCtrlSocket);
        procedure   DisconnectAll;
        procedure   DoStartSendData(Client: TFtpCtrlSocket; var Answer : TFtpString); virtual;
        procedure   AllocateMsgHandlers; override;
        procedure   FreeMsgHandlers; override;
        function    MsgHandlersCount: Integer; override;
        procedure   WndProc(var MsgRec: TMessage); override;
        { Check  if a given object is one of our clients }
        function    IsClient(SomeThing : TObject) : Boolean;
        function    OpenFileStream(const FileName: string; Mode: Word): TStream;    { angus V1.54 }
        procedure   CloseFileStreams(Client : TFtpCtrlSocket);                      { angus V1.54 }
        property  ServSocket    : TWSocketServer      read  FSocketServer;          { angus V7.00 }
        property  ClientCount   : Integer             read  GetClientCount;
        property  Active        : Boolean             read  GetActive
                                                      write SetActive;
        property  ClientClass            : TFtpCtrlSocketClass
                                                      read  FClientClass
                                                      write SetClientClass;           { angus V7.00 }
        { Client[] give direct access to anyone of our clients }
        property  Client[nIndex : Integer] : TFtpCtrlSocket
                                                      read  GetClient;
        property  ZlibWorkDir            : String     read  FZlibWorkDir    { angus V1.54 }
                                                      write FZlibWorkDir;
        property  MultiListenIndex       : Integer    read  GetMultiListenIndex;  { V8.01 }
    published
{$IFNDEF NO_DEBUG_LOG}
        property IcsLogger               : TIcsLogger  read  GetIcsLogger  { V1.46 }
                                                      write SetIcsLogger;
{$ENDIF}
        property  Addr                   : String     read  FAddr
                                                      write FAddr;
        property  BindFtpData            : Boolean    read  FBindFtpData
                                                      write FBindFtpData default True;
        property  SocketFamily           : TSocketFamily
                                                      read  FSocketFamily
                                                      write FSocketFamily;
        property  Port                   : String     read  FPort
                                                      write FPort;
        property  ListenBackLog          : Integer    read  FListenBackLog
                                                      write FListenBackLog;
        property MultiListenSockets      : TWSocketMultiListenCollection
                                                      read  GetMultiListenSockets
                                                      write SetMultiListenSockets;
        property  Banner                 : String     read  FBanner
                                                      write FBanner;
        property  UserData               : LongInt    read  FUserData
                                                      write FUserData;
        property  MaxClients             : LongInt    read  FMaxClients
                                                      write FMaxClients;
        property  PasvIpAddr             : String     read  FPasvIpAddr
                                                      write FPasvIpAddr;
        property  PasvPortRangeStart     : Integer    read  FPasvPortRangeStart
                                                      write SetPasvPortRangeStart;
        property  PasvPortRangeSize      : Integer    read  FPasvPortRangeSize
                                                      write SetPasvPortRangeSize;
        property  Options                : TFtpsOptions
                                                      read  FOptions
                                                      write FOptions;
        property  MD5UseThreadFileSize   : Integer    read  FMd5UseThreadFileSize
                                                      write FMd5UseThreadFileSize;
        property  TimeoutSecsLogin       : Integer    read FTimeoutSecsLogin
                                                      write FTimeoutSecsLogin; { angus V1.54 }
        property  TimeoutSecsIdle        : Integer    read FTimeoutSecsIdle
                                                      write FTimeoutSecsIdle;  { angus V1.54 }
        property  TimeoutSecsXfer        : Integer    read FTimeoutSecsXfer
                                                      write FTimeoutSecsXfer;  { angus V1.54 }
        property  ZlibMinLevel           : Integer    read FZlibMinLevel
                                                      write FZlibMinLevel;   { angus V1.54 }
        property  ZlibMaxLevel           : Integer    read FZlibMaxLevel
                                                      write FZlibMaxLevel;   { angus V1.54 }
        property  ZlibNoCompExt          : String     read  FZlibNoCompExt
                                                      write FZlibNoCompExt;  { angus V1.54 }
        property  AlloExtraSpace         : Integer    read FAlloExtraSpace
                                                      write FAlloExtraSpace; { angus V1.54 }
        property  ZlibMinSpace           : Integer    read FZlibMinSpace
                                                      write FZlibMinSpace;   { angus V1.54 }
        property  ZlibMaxSize            : Int64      read  FZlibMaxSize
                                                      write FZlibMaxSize ;   { angus V1.55 }
        property  CodePage               : LongWord   read  FCodePage
                                                      write FCodePage;       { angus V7.01 }
        property  Language               : String     read  FLanguage
                                                      write FLanguage;       { angus V7.01 }
        property  MaxAttempts            : Integer    read  FMaxAttempts
                                                      write FMaxAttempts ;   { angus V7.06 }
{$IFDEF BUILTIN_THROTTLE}
        property  BandwidthLimit         : LongWord   read  FBandwidthLimit
                                                      write FBandwidthLimit;     { angus V7.12 }
        property  BandwidthSampling      : LongWord   read  FBandwidthSampling
                                                      write FBandwidthSampling;  { angus V7.12 }
{$ENDIF}
        property  OnStart                : TNotifyEvent
                                                      read  FOnStart
                                                      write FOnStart;
        property  OnStop                 : TNotifyEvent
                                                      read  FOnStop
                                                      write FOnStop;
        property  OnAuthenticate         : TFtpSrvAuthenticateEvent
                                                      read  FOnAuthenticate
                                                      write FOnAuthenticate;
        property  OnOtpMethod            : TFtpSrvOtpMethodEvent     { angus V1.54 }
                                                      read FOnOtpMethod
                                                      write FOnOtpMethod;
        property  OnOtpGetPassword       : TFtpSrvOtpGetPasswordEvent     { angus V1.54 }
                                                      read FOnOtpGetPassword
                                                      write FOnOtpGetPassword;
        property  OnClientDisconnect     : TFtpSrvClientConnectEvent
                                                      read  FOnClientDisconnect
                                                      write FOnClientDisconnect;
        property  OnClientConnect        : TFtpSrvClientConnectEvent
                                                      read  FOnClientConnect
                                                      write FOnClientConnect;
        property  OnClientCommand        : TFtpSrvClientCommandEvent
                                                      read  FOnClientCommand
                                                      write FOnClientCommand;
        property  OnAnswerToClient       : TFtpSrvAnswerToClientEvent
                                                      read  FOnAnswerToClient
                                                      write FOnAnswerToClient;
        property  OnChangeDirectory      : TFtpSrvChangeDirectoryEvent
                                                      read  FOnChangeDirectory
                                                      write FOnChangeDirectory;
        property  OnMakeDirectory        : TFtpSrvChangeDirectoryEvent
                                                      read  FOnMakeDirectory
                                                      write FOnMakeDirectory;
        property  OnBuildDirectory       : TFtpSrvBuildDirectoryEvent
                                                      read  FOnBuildDirectory
                                                      write FOnBuildDirectory;
        property  OnAlterDirectory       : TFtpSrvBuildDirectoryEvent
                                                      read  FOnAlterDirectory
                                                      write FOnAlterDirectory;
        property  OnStorSessionConnected : TFtpSrvDataSessionConnectedEvent
                                                      read  FOnStorSessionConnected
                                                      write FOnStorSessionConnected;
        property  OnRetrSessionConnected : TFtpSrvDataSessionConnectedEvent
                                                      read  FOnRetrSessionConnected
                                                      write FOnRetrSessionConnected;
        property  OnStorSessionClosed    : TFtpSrvDataSessionConnectedEvent
                                                      read  FOnStorSessionClosed
                                                      write FOnStorSessionClosed;
        property  OnRetrSessionClosed    : TFtpSrvDataSessionConnectedEvent
                                                      read  FOnRetrSessionClosed
                                                      write FOnRetrSessionClosed;
        property  OnRetrDataSent         : TFtpSrvRetrDataSentEvent
                                                      read  FOnRetrDataSent
                                                      write FOnRetrDataSent;
        property  OnValidatePut          : TFtpSrvValidateXferEvent
                                                      read  FOnValidatePut
                                                      write FOnValidatePut;
        property  OnValidateSize         : TFtpSrvValidateXferEvent
                                                      read  FOnValidateSize
                                                      write FOnValidateSize;
        property  OnValidateDele         : TFtpSrvValidateXferEvent
                                                      read  FOnValidateDele
                                                      write FOnValidateDele;
        property  OnValidateRmd          : TFtpSrvValidateXferEvent
                                                      read  FOnValidateRmd
                                                      write FOnValidateRmd;
        property  OnValidateRnFr         : TFtpSrvValidateXferEvent
                                                      read  FOnValidateRnFr
                                                      write FOnValidateRnFr;
        property  OnValidateRnTo         : TFtpSrvValidateXferEvent
                                                      read  FOnValidateRnTo
                                                      write FOnValidateRnTo;
        property  OnValidateGet          : TFtpSrvValidateXferEvent
                                                      read  FOnValidateGet
                                                      write FOnValidateGet;
        property  OnStorDataAvailable    : TFtpSrvDataAvailableEvent
                                                      read  FOnStorDataAvailable
                                                      write FOnStorDataAvailable;
        property  OnGetUniqueFileName    : TFtpSrvGetUniqueFileNameEvent
                                                      read  FOnGetUniqueFileName
                                                      write FOnGetUniqueFileName;
        property  OnGetProcessing        : TFtpSrvGetProcessingEvent
                                                      read  FOnGetProcessing
                                                      write FOnGetProcessing;
        property  OnBuildFilePath        : TFtpSrvBuildFilePathEvent
                                                      read  FOnBuildFilePath
                                                      write FOnBuildFilePath;
        property  OnValidateMfmt         : TFtpSrvValidateXferEvent        { angus V1.39 }
                                                      read  FOnValidateMfmt
                                                      write FOnValidateMfmt;
        property  OnCalculateMd5         : TFtpSrvCalculateMd5Event        { angus V1.39 }
                                                      read  FOnCalculateMd5
                                                      write FOnCalculateMd5;
        property  OnMd5Calculated        : TFtpSrvMd5CalculatedEvent       { AG V1.50 }
                                                      read  FOnMd5Calculated
                                                      write FOnMd5Calculated;
        property  OnCalculateCrc         : TFtpSrvCalculateMd5Event        { angus V1.54 }
                                                      read  FOnCalculateCrc
                                                      write FOnCalculateCrc;
        property  OnCrcCalculated        : TFtpSrvMd5CalculatedEvent       { angus V1.54 }
                                                      read  FOnCrcCalculated
                                                      write FOnCrcCalculated;
        property  OnPasvIpAddr           : TFtpSrvOnPasvIpAddrEvent       { AG V1.51 }
                                                      read  FOnPasvIpAddr
                                                      write FOnPasvIpAddr;
        property  OnEnterSecurityContext : TFtpSecurityContextEvent { AG V1.52 }
                                                      read  FOnEnterSecurityContext
                                                      write FOnEnterSecurityContext;
        property  OnLeaveSecurityContext : TFtpSecurityContextEvent { AG V1.52 }
                                                      read  FOnLeaveSecurityContext
                                                      write FOnLeaveSecurityContext;
        property  OnValidateAllo         : TFtpSrvGeneralEvent          { angus V1.54 }
                                                      read  FOnValidateAllo
                                                      write FOnValidateAllo;
        property  OnClntStr              : TFtpSrvGeneralEvent          { angus V1.54 }
                                                      read  FOnClntStr
                                                      write FOnClntStr;
        property  OnSiteMsg              : TFtpSrvGeneralEvent          { angus V1.54 }
                                                      read  FOnSiteMsg
                                                      write FOnSiteMsg;
        property  OnSiteExec             : TFtpSrvGeneralEvent          { angus V1.54 }
                                                      read  FOnSiteExec
                                                      write FOnSiteExec;
        property  OnSitePaswd            : TFtpSrvGeneralEvent          { angus V1.54 }
                                                      read  FOnSitePaswd
                                                      write FOnSitePaswd;
        property  OnCombine              : TFtpSrvGeneralEvent          { angus V1.54 }
                                                      read  FOnCombine
                                                      write FOnCombine;
        property  OnTimeout              : TFtpSrvTimeoutEvent          { angus V1.54 }
                                                      read  FOnTimeout
                                                      write FOnTimeout;
        property  OnDownCompressFile     : TFtpSrvCompressFileEvent          { angus V1.54 }
                                                      read  FOnDownCompressFile
                                                      write FOnDownCompressFile;
        property  OnUpCompressFile       : TFtpSrvCompressFileEvent          { angus V1.54 }
                                                      read  FOnUpCompressFile
                                                      write FOnUpCompressFile;
        property  OnUpCompressedFile     : TFtpSrvCompressedFileEvent        { angus V1.54 }
                                                      read  FOnUpCompressedFile
                                                      write FOnUpCompressedFile;
        property  OnDisplay              : TFtpSrvDisplayEvent               { angus V1.54 }
                                                      read  FOnDisplay
                                                      write FOnDisplay;
        property  OnHost                 : TFtpSrvHostEvent                  { angus V7.01 }
                                                      read  FOnHost
                                                      write FOnHost;
        property  OnRein                 : TFtpSrvReinEvent                  { angus V7.01 }
                                                      read  FOnRein
                                                      write FOnRein;
        property  OnLang                 : TFtpSrvLangEvent                  { angus V7.01 }
                                                      read  FOnLang
                                                      write FOnLang;
        property  OnAddVirtFiles         : TFtpSrvAddVirtFilesEvent          { angus V7.08 }
                                                      read  FOnAddVirtFiles
                                                      write FOnAddVirtFiles;
        property  OnBgException;
    end;

{ You must define USE_SSL so that SSL code is included in the component.   }
{ Either in OverbyteIcsDefs.inc or in the project/package options.         }
{$IFDEF USE_SSL}
{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels
Description:  A component adding TLS/SSL support to TFtpServer.
              Requires OpenSSL (http://www.openssl.org).
              More details in ReadMeIcsSsl.txt and IcsSslHowTo.txt.
              SSL demo applications can be found in /Delphi/SslInternet.
              If you use Delphi 7 and later, you may want to disable warnings
              for unsage type, unsafe code and unsafe typecast in the project
              options. Those warning are intended for .NET programs. You may
              also want to turn off deprecated symbol and platform symbol
              warnings.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

    TSslFtpWSocketMultiListenItem = class(TSslWSocketMultiListenItem)
    private
      FFtpSslTypes : TFtpSslTypes;
      procedure SetFtpSslTypes(const Value: TFtpSslTypes);
    public
      constructor Create(Collection: TCollection); override;
    published
      property  FtpSslTypes        : TFtpSslTypes        read  FFtpSslTypes
                                                         write SetFtpSslTypes;
    end;

    TFtpSslWSocketServer = class(TSslWSocketServer)
    protected
        function  MultiListenItemClass: TWSocketMultiListenItemClass; override;
    end;

    TSslFtpServer = class(TFtpServer)
    protected
        FFtpSslTypes                        : TFtpSslTypes;
        FOnSslHandshakeDone                 : TSslHandshakeDoneEvent;
        FOnSslVerifyPeer                    : TSslVerifyPeerEvent;
        FOnSslSvrGetSession                 : TSslSvrGetSession;
        FOnSslSvrNewSession                 : TSslSvrNewSession;
        FOnSslSetSessionIDContext           : TSslSetSessionIDContext;
        FMsg_WM_FTPSRV_ABORT_TRANSFER       : UINT;
        FMsg_WM_FTPSRV_Close_Data           : UINT;
        procedure CreateSocket; override;
        procedure ClientPassiveSessionAvailable(Sender : TObject;
                                                AError : Word); override;
        procedure ClientDataSent(Sender : TObject; AError : Word); override; { 1.03 }
        procedure TriggerClientConnect(Client        : TFtpCtrlSocket;
                                       AError        : Word); override;
        procedure SendAnswer(Client                  : TFtpCtrlSocket;
                             Answer                  : TFtpString); override;
        procedure TriggerStorSessionConnected(Client : TFtpCtrlSocket;
                                              Data   : TWSocket;
                                              AError : Word); override;
        procedure TriggerRetrSessionConnected(Client : TFtpCtrlSocket;
                                              Data   : TWSocket;
                                              AError : Word); override;
        function  GetSslContext : TSslContext;
        procedure SetSslContext(Value : TSslContext);
        procedure CommandCCC(Client       : TFtpCtrlSocket;   { 1.03 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPBSZ(Client      : TFtpCtrlSocket;   { 1.03 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandAUTH(Client      : TFtpCtrlSocket;   { AG }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPROT(Client      : TFtpCtrlSocket;   { AG }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure TransferSslVerifyPeer(Sender        : TObject;
                                        var Ok        : Integer;
                                        Cert          : TX509Base); virtual;
        procedure TransferSslHandshakeDone(Sender         : TObject;
                                           ErrCode        : Word;
                                           PeerCert       : TX509Base;
                                           var Disconnect : Boolean); virtual;
        procedure TransferSslSetSessionIDContext(Sender : TObject;
                                   var SessionIDContext : TSslSessionIdContext); virtual;
        procedure TransferSslSvrNewSession(Sender       : TObject;
                                        SslSession      : Pointer;
                                        SessId          : Pointer;
                                        Idlen           : Integer;
                                 var AddToInternalCache : Boolean); virtual;
        procedure TransferSslSvrGetSession(Sender          : TObject;
                                         var SslSession : Pointer;
                                         SessId         : Pointer;
                                         Idlen          : Integer;
                                         var IncRefCount: Boolean); virtual;
        procedure SetFtpSslTypes(const Value: TFtpSslTypes); { 1.04 }
    public
        constructor Create(AOwner: TComponent); override;
        function  MsgHandlersCount : Integer; override;
        procedure AllocateMsgHandlers; override;
        procedure FreeMsgHandlers; override;
        //destructor  Destroy; override;
    published
        property  SslContext         : TSslContext         read  GetSslContext
                                                           write SetSslContext;
        property  OnSslVerifyPeer    : TSslVerifyPeerEvent read  FOnSslVerifyPeer
                                                           write FOnSslVerifyPeer;
        property  OnSslSetSessionIDContext : TSslSetSessionIDContext
                                                           read  FOnSslSetSessionIDContext
                                                           write FOnSslSetSessionIDContext;
        property  OnSslSvrNewSession : TSslSvrNewSession   read  FOnSslSvrNewSession
                                                           write FOnSslSvrNewSession;
        property  OnSslSvrGetSession : TSslSvrGetSession   read  FOnSslSvrGetSession
                                                           write FOnSslSvrGetSession;
        property  OnSslHandshakeDone : TSslHandshakeDoneEvent
                                                           read  FOnSslHandshakeDone
                                                           write FOnSslHandshakeDone;
        property  FtpSslTypes        : TFtpSslTypes        read  FFtpSslTypes  { 1.03 }
                                                           write SetFtpSslTypes; { 1.04 }
    end;
{$ENDIF} // USE_SSL

function GetZlibCacheFileName(const S : String) : String;  { angus V1.54 }
function  IsUNC(S : String) : Boolean;
procedure PatchIE5(var S : String);
function FormatFactsDirEntry(F : TSearchRec; const FileName: string) : String;  { angus 1.54  }
function FormatUnixDirEntry(F : TSearchRec; const FileName: String) : String;   { V7.08 }
procedure UpdateThreadOnProgress(Obj: TObject; Count: Int64; var Cancel: Boolean);          { V7.08 }

implementation

var
    ThisYear, ThisMonth, ThisDay : Word;

const
    DefaultBanner     = '220-ICS FTP Server ready';
    msgSyntaxParam    = '501 Syntax error in parameter.';        { V1.52 AG }
    msgSyntaxParamFmt = '501 Syntax error in parameter: %s.';    { V1.52 AG }
    msgDftBanner      = '220 ICS FTP Server ready.';
    msgTooMuchClients = '421 Too many users connected.';
    msgCmdUnknown     = '500 ''%s'': command not understood.';
    msgLoginFailed    = '530 Login incorrect.';
    msgNotLogged      = '530 Please login with USER and PASS.';
    msgEPSVALLDeny    = '501 %s command not allowed after EPSV ALL.';
    msgNoUser         = '503 Login with USER first.';
    msgLogged         = '230 User %s logged in.';
    msgPassRequired   = '331 Password required for %s.';
    msgOptRespRequired = '331 Response to %s required for %s.';   { angus V1.54 }
    msgCWDSuccess     = '250 CWD command successful. "%s" is current directory.';
    msgCWDFailed      = '501 CWD failed. %s';
    msgPWDSuccess     = '257 "%s" is current directory.';
    msgQuit           = '221 Goodbye.';
    msgPortSuccess    = '200 Port command successful.';
    msgPortFailed     = '501 Invalid PORT command.';
    msgInvalidProto   = '522 Network protocol not supported, use (%s).';
    msgStorDisabled   = '501 Permission Denied'; {'500 Cannot STOR.';}
    msgStorSuccess    = '150 Opening data connection for %s.';
    msgStorFailed     = '501 Cannot STOR. %s';
    msgStorAborted    = '426 Connection closed; %s.';
    msgStorOk         = '226 File received ok';
{   msgStorOk         = '226-Multiple lines answer'#13#10'  Test'#13#10#13#10'226 File received OK'; }
    msgStorError      = '426 Connection closed; transfer aborted. Error %s';
    msgRetrDisabled   = '500 Cannot RETR.';
    msgRetrSuccess    = '150 Opening data connection for %s.';
    msgRetrFailed     = '501 Cannot RETR. %s';
    msgRetrAborted    = '426 Connection closed; %s.';
    msgRetrOk         = '226 File sent ok';
    msgRetrError      = '426 Connection closed; transfer aborted. Error %s';
    msgRetrNotExists  = '550 ''%s'': no such file or directory.';     { angus V1.54 }
    msgRetrFileErr    = '451 Cannot open file: %s.';                  { angus V1.54 }
    msgSystem         = '215 UNIX Type: L8 Internet Component Suite';
    msgDirOpen        = '150 Opening data connection for directory list.';
    msgDirFailed      = '451 Failed: %s.';
    msgTypeOk         = '200 Type set to %s.';
    msgTypeFailed     = '500 ''TYPE %s'': command not understood.';
    msgDeleNotExists  = '550 ''%s'': no such file or directory.';
    msgDeleOk         = '250 File ''%s'' deleted.';
    msgDeleFailed     = '450 File ''%s'' can''t be deleted.';
    msgDeleSyntax     = msgSyntaxParam;//'501 Syntax error in parameter.';  { V1.52 AG }
    msgDeleDisabled   = '550 Cannot delete.';
    msgRnfrNotExists  = '550 ''%s'': no such file or directory.';
    msgRnfrSyntax     = msgSyntaxParam;//'501 Syntax error in parameter.';  { V1.52 AG }
    msgRnfrOk         = '350 File exists, ready for destination name.';
    msgRnFrDisabled   = '500 Cannot RNFR.';
    msgRntoNotExists  = '550 ''%s'': no such file or directory.';
    msgRntoAlready    = '553 ''%s'': file already exists.';
    msgRntoOk         = '250 File ''%s'' renamed to ''%s''.';
    msgRntoFailed     = '450 File ''%s'' can''t be renamed.';
    msgRntoSyntax     = msgSyntaxParam;//'501 Syntax error in parameter.';  { V1.52 AG }
    msgRnToDisabled   = '500 Cannot RNTO.';
    msgMkdOk          = '257 ''%s'': directory created.';
    msgMkdAlready     = '550 ''%s'': file or directory already exists.';
    msgMkdFailed      = '550 ''%s'': can''t create directory.';
    msgMkdSyntax      = msgSyntaxParam;//'501 Syntax error in parameter.';  { V1.52 AG }
    msgRmdOk          = '250 ''%s'': directory removed.';
    msgRmdNotExists   = '550 ''%s'': no such directory.';
    msgRmdFailed      = '550 ''%s'': can''t remove directory.';
    msgRmdDisabled    = '500 Cannot remove directory.';
    msgRmdSyntax      = msgSyntaxParam;//'501 Syntax error in parameter.';  { V1.52 AG }
    msgNoopOk         = '200 Ok. Parameter was ''%s''.';
    msgAborOk         = '225 ABOR command successful.';
    msgPasvLocal      = '227 Entering Passive Mode (127,0,0,1,%d,%d).';
    msgPasvRemote     = '227 Entering Passive Mode (%d,%d,%d,%d,%d,%d).';
    msgEPSVOk         = '229 Entering Extended Passive Mode (|||%d|)';
    msgPasvExcept     = '500 PASV exception: ''%s''.';
    msgSizeOk         = '213 %d';
    msgSizeDisabled   = '501 Permission Denied';
    msgSizeFailed     = '550 Command failed: %s.';
    msgSizeSyntax     = msgSyntaxParam;//'501 Syntax error in parameter.';  { V1.52 AG }
    msgRestOk         = '350 REST supported. Ready to resume at byte offset %d.';
    msgRestZero       = '501 Required byte offset parameter bad or missing.';
    msgRestFailed     = msgSyntaxParamFmt;//'501 Syntax error in parameter: %s.'; { V1.52 AG }
    msgRestNotModeZ   = '501 REST not supported while using Mode Z';    { angus V1.55 }
    msgAppeFailed     = '550 APPE failed.';
    msgAppeSuccess    = '150 Opening data connection for %s (append).';
    msgAppeDisabled   = '500 Cannot APPE.';
    msgAppeAborted    = '426 Connection closed; %s.';
    msgAppeOk         = '226 File received ok';
    msgAppeError      = '426 Connection closed; transfer aborted. Error %s';
    msgAppeReady      = '150 APPE supported.  Ready to append file "%s" at offset %d.';
    msgStruOk         = '200 Ok. STRU parameter ''%s'' ignored.';
    msgMdtmOk         = '213 %s';
    msgMdtmFailed     = '550 %s';
    msgMdtmSyntax     = '501 Syntax error in MDTM/MFMT parameter.';
    msgMdtmNotExists  = '550 ''%s'': no such file or directory.';
    msgModeOK         = '200 MODE %s Ok';                               { angus V1.54 add param }
    msgModeSyntax     = '501 Missing argument for MODE';
    msgModeNotS       = '502 MODE %s not supported';                    { angus V1.54 add param }
    msgOverflow       = '500 Command too long';
    msgStouOk         = '250 ''%s'': file created.';
    msgStouSuccess    = msgStorSuccess;
    msgStouFailed     = '501 Cannot STOU. %s';
    msgStouAborted    = msgStorAborted;
    msgStouError      = msgStorError;
    msgFeatFollows    = '211-Extensions supported:';
    msgFeatFollowDone = '211 END';
    msgFeatFailed     = '211 No-Features';
    msgMdtmChangeOK   = '253 Date/time changed OK';                  { angus V1.38 }
    msgMfmtChangeOK   = '213 Date/time changed OK';                  { angus V1.39 }
    msgMdtmChangeFail = '550 MDTM/MFMT cannot change date/time on this server';  { angus V1.38 }
    msgCWDNoDir       = '550 CWD Failed to change directory to %s';  { angus V1.38 }
    msgMlstFollows    = '250-Listing ';                              { angus V1.38 }
    msgMlstFollowDone = '250 END';                                   { angus V1.38 }
    msgMlstNotExists  = '550 ''%s'': no such file or directory.';    { angus V1.38 }
    msgMlstDenied     = '550 Access denied';                         { AG V1.52 }
    msgMd5NotFound    = '550 ''%s'': no such file.';                 { angus V1.39 }
    msgMd5Failed      = '550 MD5 SUM failed : ''%s''.';              { angus V1.39 }
    msgMd5Ok          = '251 "%s" %s';                               { angus V1.39 }
    msgTimeout        = '421 Connection closed, timed out after %d secs.'; { angus V1.54 }
    msgNotedOK        = '200 Noted OK.';                             { angus V1.54 }
    msgSiteZone       = '210 UTC%s';                                 { angus V1.54 }
    msgCrcOk          = '250 %s';                                    { angus V1.54 }
    msgCrcFailed      = '550 CRC failed : ''%s''.';                  { angus V1.54 }
    msgSiteFailed     = '550 SITE command failed.';                  { angus V1.54 }
    msgIndexFollows   = '200-Index %s';                              { angus V1.54 }
    msgIndexDone      = '200 END Index';                             { angus V1.54 }
    msgOtpsOK         = '200 %s Ok.';                                { angus V1.54 }
    msgOptsFailed     = '501 %s is invalid.';                        { angus V1.54 }
    msgAlloOK         = '200 ALLO OK, %d bytes available.';          { angus V1.54 }
    msgAlloFail       = '501 Invalid size parameter.';               { angus V1.54 }
    msgAlloFull       = '501 Insufficient disk space, only %d bytes available.';  { angus V1.54 }
    msgHostOK         = '220 HOST Ok, FTP Server ready.';            { angus V7.01 }
    msgHostUnavail    = '421 HOST unavailable.';                     { angus V7.01 }
    msgHostSyntax     = msgSyntaxParam;  { 501 }                     { angus V7.01 }
    msgHostTooLate    = '503 HOST no longer allowed.';               { angus V7.01 }
    msgHostUnknown    = '504 HOST unknown or not allowed.';          { angus V7.01 }
    msgReinOK         = '220 Reinitialise Ok, FTP Server ready.';    { angus V7.01 }
    msgReinUnavail    = '421 Reinitialise unavailable.';             { angus V7.01 }
    msgLangOK         = '200 %s Ok.';                                { angus V7.01 }
    msgLangUnknown    = '504 %s unknown.' ;                          { angus V7.01 }
    msgNotAllowed     = '421 Connection not allowed.';               { angus V7.06 }

{$IFDEF USE_SSL}
    msgAuthOk         = '234 Using authentication type %s';
    msgAuthDenied     = '502 %s authentication not allowed'; // SSL/TLS
    msgAuthYetSetOkV2 = '234 Auth type already set.';
    msgAuthYetSetOkV3 = msgAuthYetSetOkV2 + ' SSL re-negotiation allowed';
    //msgAuthYetSetErr  = '534 Auth type already set to %s';
    msgAuthInitError  = '431 Could not initialize %s connection';
    msgAuthNoSupport  = '504 Auth type ''%s'' not supported';

    msgErrInSslOnly   = '533 %s requires a secure connection';
    msgProtOk         = '200 Protection level set to %s';
    msgProtNoSupport  = '504 Protection level ''%s'' not supported';
    msgProtUnknown    = '504 Protection level ''%s'' not recognized';
    msgErrSslInit     = 'Fatal error on initializing SSL';
    msgPbszOk         = '200 PBSZ set to 0';
    msgCccOk          = '200 CCC OK Continue using plaintext commands';
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DirExists(const Dir : String) : Boolean;                 { V1.52 AG}
{ INVALID_HANDLE_VALUE = INVALID_FILE_ATTRIBUTES = DWORD(-1) }
{$IFDEF MSWINDOWS}
var
    Res : DWORD;
begin
    Res := GetFileAttributes(PChar(Dir));
    Result := (Res <> INVALID_HANDLE_VALUE) and
              ((Res and FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;
{$ENDIF}
{$IFDEF POSIX}
begin
    Result := SysUtils.DirectoryExists(Dir);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CreateUniqueFile(Dir, Prefix, Extension: String): String; { V1.52 AG}
var
    FileName  : String;
    I         : Integer;
  {$IFDEF MSWINDOWS}
    hFile     : THandle;
  {$ENDIF}
  {$IFDEF POSIX}
    FileHandle : Integer;
  {$ENDIF}
    Err       : DWord;
begin
    Result := '';
    Dir := Trim(Dir);
    if (Length(Dir) = 0) or (not DirExists(Dir)) then
        Exit;
    Dir := IncludeTrailingPathDelimiter(Dir);
    Prefix := Trim(Prefix);
    if Length(Prefix) > 3 then
        SetLength(Prefix, 3);
    Extension := Trim(Extension);
    Dir := Dir + Prefix + FormatDateTime('yymdh', Now);
    I   := 0;
  {$IFDEF MSWINDOWS}
    Err := ERROR_FILE_EXISTS;
    while (Err = ERROR_FILE_EXISTS) and (I < MaxInt) do begin
        FileName := Dir + IntToStr(I) + Extension;
        if Length(FileName) > MAX_PATH then
            Break;
        hFile := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
                            0, nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
        if hFile <> INVALID_HANDLE_VALUE then begin
            CloseHandle(hFile);
            Result := FileName;
            Break;
        end
        else
            Err := GetLastError;
        Inc(I);
    end;
  {$ENDIF}
  {$IFDEF POSIX}   { ToBeChecked }
    ERR := EEXIST;
    while (Err = EEXIST) and (I < MaxInt) do begin
        FileName := Dir + IntToStr(I) + Extension;
        if Length(FileName) > MAX_PATH then
            Break;
        FileHandle := __open(PAnsiChar(UTF8String(FileName)), O_RDWR or O_CREAT or O_EXCL, FileAccessRights);
        if FileHandle <> -1 then begin
            __close(FileHandle);
            Result := FileName;
            Break;
        end
        else
            Err := GetLastError;
        Inc(I);
    end;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetZlibCacheFileName(const S : String) : String;  { angus V1.54 }
var
    I : Integer;
    Ticks: String;
begin
    Result := AnsiLowercase (S);
    for I := 1 to Length(Result) do begin
        if (Result [I] = PathDelim) or (Result [I] = '.') or
                           (Result [I] = ':') then Result[I] := '_';
    end;
    Ticks := IntToStr(IcsGetTickCountX);  { now make it unique by adding some ms }
    I := Length(Ticks);
    if I < 6 then Ticks := '123' + Ticks; { if windows running short }
    Result := Result + '_' + Copy (Ticks, I-6, 6) + '.zlib';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSpace(Ch : Char) : Boolean;
begin
    Result := (Ch = ' ') or (Ch = Char($09));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsDigit(Ch : Char) : Boolean; { V6.03 }
begin
    Result := (Ch >= '0') and (Ch <= '9');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsLetterOrDigit(Ch : Char) : Boolean;
begin
    Result := ((Ch >= 'a') and (Ch <= 'z')) or
              ((Ch >= 'A') and (Ch <= 'Z')) or
              ((Ch >= '0') and (Ch <= '9'));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function atosi(const value : String) : Integer;  { angus V1.38 signed integer, added "const", AG }
var
    i, j : Integer;
begin
    Result := 0;
    i := 1;
    while (i <= Length(Value)) and (Value[i] = ' ') do
        i := i + 1;
    j := i;
    while (i <= Length(Value)) and ((Value[i] = '+') or (Value[i] = '-')) do
       i := i + 1;
    while (i <= Length(Value)) and (Value[i] >= '0') and (Value[i] <= '9')do begin
        Result := Result * 10 + ord(Value[i]) - ord('0');
        i := i + 1;
    end;
    if j < Length(Value) then begin
        if value[j] = '-' then
            Result := -Result;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CreateSocket;
begin
    FSocketServer := TWSocketServer.Create(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TFtpServer.Create(AOwner: TComponent);
{$IFDEF MSWINDOWS}
var
    Len : Cardinal;
{$ENDIF}
begin
    inherited Create(AOwner);
    AllocateHWnd;
 { angus V7.00 WSocketServer instead of WSocket }
    FClientClass          := TFtpCtrlSocket;
    //FSocketServer         := TWSocketServer.Create(Self);
    CreateSocket;
    FSocketServer.Name    := 'WSocketServer';
    FSocketServer.ClientClass         := FClientClass;
    FSocketServer.OnClientConnect     := ServerClientConnect;
    FSocketServer.OnClientDisconnect  := ServerClientDisconnect;
{$IFNDEF NO_DEBUG_LOG}
    FSocketServer.IcsLogger           := GetIcsLogger ;
{$ENDIF}

    FPort               := 'ftp';
    FSocketFamily       := DefaultSocketFamily;
    FAddr               := ICS_ANY_HOST_V4;
    FBanner             := msgDftBanner;
    FListenBackLog      := 5;
    FOptions            := [{tpsThreadRecurDirs,}ftpsSiteXmlsd, ftpsCwdCheck, ftpsCdupHome] ;   { angus V7.02, V8.04 stop thread}
    FMd5UseThreadFileSize   := 0;  { AG V1.50 }
    FTimeoutSecsLogin   := 60;      { angus V1.54 }
    FTimeoutSecsIdle    := 300;     { angus V1.54 }
    FTimeoutSecsXfer    := 60;      { angus V1.54, V7.09 reduced from 900 to 60 secs }
    FZlibMinLevel       := 1;       { angus V1.54 }
    FZlibMaxLevel       := 9;       { angus V1.54 }
    FZlibNoCompExt      := '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;'; { angus V1.54 }
    SetLength(FZlibWorkDir, 1024);
  {$IFDEF MSWINDOWS}
    Len := GetTempPath(Length(FZlibWorkDir) - 1, PChar(FZlibWorkDir));{ AG V6.04 }
    SetLength(FZlibWorkDir, Len);                                     { AG V6.04 }
  {$ENDIF}
  {$IFDEF POSIX}
    FZlibWorkDir := TPath.GetTempPath;
  {$ENDIF}
    FZlibWorkDir        := IncludeTrailingPathDelimiter (FZlibWorkDir) + 'icsftpsrv\' ;  { angus V1.54 }
    FZlibMinSpace       := 50000000;               { angus V1.54 50 Mbyte }
    FZlibMaxSize        := 500000000;              { angus V1.55 - 500 meg }
    FAlloExtraSpace     := 1000000;                { angus V1.54 1 Mbyte }
    FEventTimer         := TIcsTimer.Create(Self); { angus V1.54 }
    FEventTimer.Enabled := false;                  { angus V1.54 }
    FEventTimer.OnTimer := EventTimerOnTimer;      { angus V1.54 }
    FEventTimer.Interval := 5000;     { angus V1.56 only used for timeouts, slow }
    SrvFileModeRead     := fmOpenRead + fmShareDenyNone;         { angus V1.57 }
    SrvFileModeWrite    := fmOpenReadWrite or fmShareDenyWrite;  { angus V1.57 }
    FCodePage           := CP_ACP;  { angus V7.01 }
    FLanguage           := 'EN*';   { angus V7.01 we only support ENglish }
    FSystemCodePage     := GetAcp;  { AG 7.02 }
    FMaxAttempts        := 12 ;     { angus V7.06 }
    FBindFtpData        := True;
{$IFDEF BUILTIN_THROTTLE}
    FBandwidthLimit     := 0;       { angus V7.12 no bandwidth limit, yet, bytes per second }
    FBandwidthSampling  := 1000;    { angus V7.12 Msec sampling interval, less is not possible }
{$ENDIF}
 { !!!!!!!!!!! NGB: Added next five lines }
    FPasvIpAddr         := '';
    FPasvPortRangeStart := 0;
    FPasvPortRangeSize  := 0;
    FPasvPortTable      := nil;
    FPasvPortTableSize  := 0;
{ !!!!!!!!!!! NGB: Added previous five lines }
    FPasvNextNr         := 0;  { angus V1.56 }
    SetLength(FCmdTable, ftpcLast + 1 + 5);
    AddCommand('PORT', CommandPORT);
    AddCommand('STOR', CommandSTOR);
    AddCommand('RETR', CommandRETR);
    AddCommand('CWD',  CommandCWD);
    AddCommand('XPWD', CommandXPWD);
    AddCommand('PWD',  CommandPWD);
    AddCommand('USER', CommandUSER);
    AddCommand('PASS', CommandPASS);
    AddCommand('LIST', CommandLIST);
    AddCommand('NLST', CommandNLST);
    AddCommand('TYPE', CommandTYPE);
    AddCommand('SYST', CommandSYST);
    AddCommand('QUIT', CommandQUIT);
    AddCommand('DELE', CommandDELE);
    AddCommand('SIZE', CommandSIZE);
    AddCommand('REST', CommandREST);
    AddCommand('RNFR', CommandRNFR);
    AddCommand('RNTO', CommandRNTO);
    AddCommand('MKD',  CommandMKD);
    AddCommand('RMD',  CommandRMD);
    AddCommand('ABOR', CommandABOR);
    AddCommand('PASV', CommandPASV);
    AddCommand('NOOP', CommandNOOP);
    AddCommand('CDUP', CommandCDUP);
    AddCommand('APPE', CommandAPPE);
    AddCommand('STRU', CommandSTRU);
    AddCommand('XMKD', CommandMKD);
    AddCommand('XRMD', CommandRMD);
    AddCommand('MDTM', CommandMDTM);
    AddCommand('MODE', CommandMODE);
    AddCommand('OVER', CommandOverflow);
    AddCommand('STOU', CommandSTOU);
    AddCommand('FEAT', CommandFEAT);
    AddCommand('MLST', CommandMLST);  { angus V1.38 }
    AddCommand('MLSD', CommandMLSD);  { angus V1.38 }
    AddCommand('MFMT', CommandMDTM);  { angus V1.39 }
    AddCommand('MD5', CommandMD5);    { angus V1.39 }
    AddCommand('XCRC', CommandXCRC);  { angus V1.54 }
    AddCommand('XMD5', CommandMD5);   { angus V1.54 note same handler as MD5 }
    AddCommand('ALLO', CommandALLO);  { angus V1.54 }
    AddCommand('CLNT', CommandCLNT);  { angus V1.54 }
    AddCommand('OPTS', CommandOPTS);  { angus V1.54 }
    AddCommand('SITE PSWD', CommandSitePaswd);   { angus V1.54 }
    AddCommand('SITE EXEC', CommandSiteExec);    { angus V1.54 }
    AddCommand('SITE INDEX', CommandSiteIndex);  { angus V1.54 }
    AddCommand('SITE ZONE', CommandSiteZone);    { angus V1.54 }
    AddCommand('SITE MSG', CommandSiteMsg);      { angus V1.54 }
    AddCommand('SITE CMLSD', CommandSiteCmlsd);  { angus V1.54 }
    AddCommand('SITE DMLSD', CommandSiteDmlsd);  { angus V1.54 }
    AddCommand('COMB', CommandCOMB);      { angus V1.54 }
    AddCommand('XCMLSD', CommandXCMLSD);  { angus V7.01 }
    AddCommand('XDMLSD', CommandXDMLSD);  { angus V7.01 }
    AddCommand('HOST', CommandHOST);      { angus V7.01 }
    AddCommand('REIN', CommandREIN);      { angus V7.01 }
    AddCommand('LANG', CommandLANG);      { angus V7.01 }
    AddCommand('EPRT', CommandEprt);
    AddCommand('EPSV', CommandEpsv);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TFtpServer.Destroy;
begin
    if Assigned(FEventTimer) then begin  { angus V1.54 }
        FEventTimer.Destroy;
        FEventTimer := nil;
    end;
    if Assigned(FSocketServer) then begin      { angus V7.00 }
        FSocketServer.Destroy;
        FSocketServer := nil;
    end;
    if Assigned(FPasvPortTable) then begin
        FreeMem(FPasvPortTable, FPasvPortTableSize);
        FPasvPortTable     := nil;
        FPasvPortTableSize := 0;
    end;
    SetLength(FCmdTable, 0);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.MsgHandlersCount : Integer;
begin
    Result := 5 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_FTPSRV_CLOSE_REQUEST  := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_FTPSRV_ABORT_TRANSFER := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_FTPSRV_CLOSE_DATA     := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_FTPSRV_START_SEND     := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_FTPSRV_CLOSE_REQUEST);
        FWndHandler.UnregisterMessage(FMsg_WM_FTPSRV_ABORT_TRANSFER);
        FWndHandler.UnregisterMessage(FMsg_WM_FTPSRV_CLOSE_DATA);
        FWndHandler.UnregisterMessage(FMsg_WM_FTPSRV_START_SEND);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WndProc(var MsgRec: TMessage);
begin
    try
        with MsgRec do begin
            if  Msg = FMsg_WM_FTPSRV_CLOSE_REQUEST  then
                WMFtpSrvCloseRequest(MsgRec)
            else if Msg = FMsg_WM_FTPSRV_ABORT_TRANSFER then
                WMFtpSrvAbortTransfer(MsgRec)
            else if Msg = FMsg_WM_FTPSRV_CLOSE_DATA then
                WMFtpSrvCloseData(MsgRec)
            else if Msg = FMsg_WM_FTPSRV_START_SEND then
                WMFtpSrvStartSend(MsgRec)
            else
                inherited WndProc(MsgRec);
        end;
    except
        on E:Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WMFtpSrvCloseRequest(var msg: TMessage);
var
    Client : TFtpCtrlSocket;
begin
    Client := TFtpCtrlSocket(msg.LParam);
    if FSocketServer.IsClient(Client) then begin      { angus V7.00 }
        { Check if client.ID is still the same as when message where posted }
        if WPARAM(Client.ID) = Msg.WParam then begin
            if Client.AllSent then
                Client.Close
            else
                Client.CloseRequest := TRUE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.Notification(AComponent: TComponent; operation: TOperation);
begin
    inherited Notification(AComponent, operation);
    if operation = opRemove then begin
        if AComponent = FSocketServer then         { angus V7.00 }
            FSocketServer := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.OpenFileStream (const FileName: string; Mode: Word): TStream;   { V1.54 }
begin
    Result := TBufferedFileStream.Create(FileName, Mode, MAX_BUFSIZE);
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CloseFileStreams(Client : TFtpCtrlSocket);    { V1.54 }
begin
  { delete temporary ZLIB file if not from cache }
    try
        if Assigned (Client.ZFileStream) then Client.ZFileStream.Destroy;
        Client.ZFileStream := Nil;
        if (Client.ZStreamState > ftpzStateNone) and
                                        Client.ZCompFileDelete then begin
            try
                if FileExists(Client.ZCompFileName) then
                                           DeleteFile (Client.ZCompFileName);
            except
            end;
        end;
    except
    end;
    Client.ZStreamState := ftpZStateNone;
    if Client.HasOpenedFile then begin
        if Assigned(Client.DataStream) then Client.DataStream.Destroy;
        Client.DataStream    := nil;
        Client.HasOpenedFile := FALSE;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.AddCommand(
    const Keyword : String;
    const Proc    : TFtpSrvCommandProc);
begin
    if FLastCmd > High(FCmdTable) then
        raise FtpServerException.Create('Too many command');
    FCmdTable[FLastCmd].KeyWord := KeyWord;
    FCmdTable[FLastCmd].Proc    := Proc;
    Inc(FLastCmd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.Start;
begin
    if FSocketServer.State = wsListening then                { angus V7.00 }
        Exit;             { Server is already running }
    FSocketServer.Port              := Port;
    FSocketServer.Proto             := 'tcp';
    FSocketServer.SocketFamily      := FSocketFamily;
    FSocketServer.Addr              := FAddr;
    FSocketServer.ListenBacklog     := FListenBackLog;
    FSocketServer.MaxClients        := FMaxClients;
    FSocketServer.Banner            := FBanner;
    FSocketServer.BannerTooBusy     := msgTooMuchClients;
    FSocketServer.OnChangeState     := ServSocketStateChange;
    FSocketServer.ComponentOptions  := [wsoNoReceiveLoop];
    FSocketServer.BandwidthLimit    := fBandwidthLimit;     { angus V7.16 in client connect }
    FSocketServer.BandwidthSampling := fBandwidthSampling;  { angus V7.16 }
    FSocketServer.MultiListen;                    { V8.01 }
    FEventTimer.Enabled := true;                  { angus V1.54 }
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loProtSpecInfo) then                            { V1.46 }
        DebugLog(loProtSpecInfo, Name + ' started');                   { V1.46 }
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.Stop;
begin
    FEventTimer.Enabled := false;                  { angus V1.54 }
    FSocketServer.Close;                           { angus V7.00 }
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loProtSpecInfo) then                            { V1.46 }
        DebugLog(loProtSpecInfo, Name + ' stopped');                   { V1.46 }
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.DisconnectAll;
begin
    FSocketServer.DisconnectAll;      { angus V7.00 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.Disconnect(Client : TFtpCtrlSocket);
begin
    if NOT FSocketServer.IsClient(Client) then
        raise FtpServerException.Create('Disconnect: Not one of our clients');
    FSocketServer.Disconnect(Client);       { angus V7.00 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.GetActive : Boolean;
begin
    Result := (FSocketServer.State = wsListening);    { angus V7.00 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SetActive(newValue : Boolean);
begin
    if newValue then
        Start
    else
        Stop;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SetClientClass(const NewValue: TFtpCtrlSocketClass);    { angus V7.00 }
begin
    if NewValue <> FSocketServer.ClientClass then begin
        FClientClass := NewValue;
        FSocketServer.ClientClass := NewValue;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ServSocketStateChange(Sender : TObject; OldState, NewState : TSocketState);
begin
    if csDestroying in ComponentState then
        Exit;
    if NewState = wsListening then
        TriggerServerStart
    else if NewState = wsClosed then
        TriggerServerStop;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ServerClientConnect(Sender: TObject;
                                Client: TWSocketClient; Error: Word);    { angus V7.00 }
var
    MyClient: TFtpCtrlSocket;   { renamed to avoid conflict with TWSocketClient angus V7.00 }
begin
    if Error <> 0 then
        raise FtpServerException.Create('Session Available Error - ' +
                                                    GetWinsockErr(Error));
    MyClient := Client as TFtpCtrlSocket;
    MyClient.DataSocket.Name := Name + '_DataWSocket' + IntToStr(MyClient.ID);
    MyClient.OnCommand       := ClientCommand;
    MyClient.OnDataSent      := ClientDataSent;
    MyClient.FtpServer       := Self; {AG V7.02 }
{$IFNDEF NO_DEBUG_LOG}
    MyClient.IcsLogger       := IcsLogger;                     { V1.46 }
    MyClient.DataSocket.IcsLogger := IcsLogger;                    //<= 01/01/06 AG
{$ENDIF}
{$IFDEF USE_SSL}
    if Self is TSslFtpServer then begin     {  V1.48 }
        if MultiListenIndex = -1 then
          MyClient.FtpSslTypes := TSslFtpserver(Self).FFtpSslTypes
        else
          MyClient.FtpSslTypes := TSslFtpWSocketMultiListenItem(MultiListenSockets[MultiListenIndex]).FFtpSslTypes;
        if ftpImplicitSsl in MyClient.FtpSslTypes then   { V1.47 }
            MyClient.CurFtpSslType := curftpImplicitSsl;               { V1.47 }
    end;
{$ENDIF}
    if ftpsCdupHome in FOptions then
        MyClient.Options := MyClient.Options + [ftpCdupHome];   { angus V1.39 }
    if ftpsCwdCheck in FOptions then
        MyClient.Options := MyClient.Options + [ftpCwdCheck];   { angus V1.39 }
    if ftpsHidePhysicalPath in FOptions then
        MyClient.Options := MyClient.Options + [ftpHidePhysicalPath]; { AG V1.52 }
    if ftpsModeZCompress in FOptions then
        MyClient.Options := MyClient.Options + [ftpModeZCompress];
    MyClient.CodePage        := FCodePage;           { AG V7.02 }
    MyClient.CurrentCodePage := FCodePage;           { AG V7.02 }
    if (ftpsEnableUtf8 in FOptions) and (ftpsDefaultUtf8On in FOptions) then
        MyClient.Options := MyClient.Options + [ftpUtf8On]     { angus V7.01 }
    else if (ftpsEnableUtf8 in FOptions) and (ftpsAutoDetectCodepage in FOptions) then
        MyClient.FOptions := MyClient.Options + [ftpAutoDetectCodepage]; { AG V7.02 }

{$IFNDEF NO_DEBUG_LOG}                                       { V1.46 }
    if CheckLogOptions(loProtSpecDump) then
        DebugLog(loProtSpecDump,  IntToHex(INT_PTR(MyClient), SizeOf(Pointer) * 2) +
                 ' Client Connect Error - ' + GetWinsockErr(Error) + ' ' +
                  IntToStr(MyClient.HSocket));
{$ENDIF}
    { angus V1.54 may be changed during event, V7.09 add client numeric id to identify separate sessions from same IP }
    MyClient.SessIdInfo      := Client.GetPeerAddr + ' [' + IntToStr (Client.CliId) + ']' ;
    MyClient.CurrTransMode   := FtpTransModeStream ; { angus V1.54 current zlib transfer mode }
    MyClient.ZReqLevel       := FZlibMinLevel;       { angus V1.54 initial compression level, minimum }
    MyClient.FConnectedSince := Now;
    MyClient.FLastCommand    := 0;
    MyClient.FCommandCount   := 0;
    MyClient.FFtpState       := ftpcWaitingUserCode;
    MyClient.FileModeRead    := SrvFileModeRead;     { angus V1.57 }
    MyClient.FileModeWrite   := SrvFileModeWrite;    { angus V1.57 }
{$IFDEF BUILTIN_THROTTLE}
    MyClient.CBandwidthLimit    := fBandwidthLimit;     { angus V7.12 may be changed in event for different limit }
    MyClient.CBandwidthSampling := fBandwidthSampling;  { angus V7.12 }
{$ENDIF}
    TriggerClientConnect(MyClient, Error);
{$IFDEF BUILTIN_THROTTLE}
    MyClient.BandwidthLimit     := MyClient.CBandwidthLimit;     { angus V7.12 slow down control connection }
    MyClient.BandwidthSampling  := MyClient.CBandwidthSampling;  { angus V7.12 }
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SendAnswer(Client : TFtpCtrlSocket; Answer : TFtpString);
var
    RawAnswer: RawByteString;
begin
    try
        { Angus 7.03 fixed trigger needed before UTF8 conversion }
         Client.ReqDurMilliSecs := IcsElapsedMsecs (Client.ReqStartTick);
         TriggerSendAnswer(Client, Answer);
    { AG 7.02 }
    {$IFDEF COMPILER12_UP}
        RawAnswer := UnicodeToAnsi(Answer, Client.CurrentCodePage);
    {$ELSE}
        if Client.CurrentCodePage = CP_UTF8 then
            RawAnswer := StringToUtf8(Answer)
        else
            RawAnswer := Answer;
    {$ENDIF}
        Client.SendAnswer(RawAnswer);
    except
        { Just ignore any exception here }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientCommand(
    Sender : TObject;
    CmdBuf : PAnsiChar;
    CmdLen : Integer);
const
    TELNET_IAC       = #255;
    TELNET_IP        = #244;
    TELNET_DATA_MARK = #242;
var
    Client  : TFtpCtrlSocket;
    Answer  : TFtpString;
    Params  : TFtpString;
    KeyWord : TFtpString;
    I       : Integer;
    RawParams: RawByteString;
begin
    Client := Sender as TFtpCtrlSocket;
    Answer := '';

    { Copy the command received, removing any telnet option }
    try
        Client.ReqStartTick := IcsGetTickCountX;    { angus V1.54 tick when request started }
        Client.ReqDurMilliSecs := 0;                { angus V1.54 how long last request took, in ticks }
        RawParams := '';
        I      := 0;
        while I < CmdLen do begin
            if CmdBuf[I] <> TELNET_IAC then begin
                RawParams := RawParams + CmdBuf[I];
                Inc(I);
            end
            else begin
                Inc(I);
                if CmdBuf[I] = TELNET_IAC then
                    RawParams := RawParams + CmdBuf[I];
                Inc(I);
            end;
        end;
        { AG V7.02 - Optionally detect UTF-8. Set option ftpUtf8ON which }
        { in turn sets CurrentCodePage to CP_UTF8 if UTF-8 is detected.  }
        if (Client.CurrentCodePage <> CP_UTF8) and
           (ftpAutoDetectCodepage in Client.Options) and
           (CharsetDetect(RawParams) = cdrUtf8) then
            Client.Options := Client.Options + [ftpUtf8ON];  { AG V7.02  }
      {$IFDEF COMPILER12_UP}
        { Convert buffer data to UnicodeString AG V7.02 }
        Params := AnsiToUnicode(RawParams, Client.CurrentCodePage);
      {$IFNDEF NO_UNICODE_NORMALIZATION}                               { AG V7.18 }
        if Client.CurrentCodePage = CP_UTF8 then                       { AG V7.18 }
            Params := IcsNormalizeString(Params, icsNormalizationC);   { AG V7.18 }
      {$ENDIF NO_UNICODE_NORMALIZATION}                                { AG V7.18 }
      {$ELSE}
        { Convert buffer data to AnsiString ( potential data loss! ) AG V7.02 }
        if (Client.CurrentCodePage = CP_UTF8) then
            Params := Utf8ToStringA(RawParams)
        else
            Params := RawParams;
      {$ENDIF COMPILER12_UP}

        { Extract keyword, ignoring leading spaces and tabs }
        I := 1; { angus V1.54 moved argument parsing code to FtpSrvT to avoid duplication }
        KeyWord := UpperCase(ScanGetAsciiArg (Params, I));
        if KeyWord = 'SITE' then begin  { angus 1.54 special case for two word command }
            KeyWord := 'SITE ' + UpperCase(ScanGetAsciiArg (Params, I));
        end ;
        ScanFindArg (Params, I);

        { Extract parameters, ignoring leading spaces and tabs }
        Params := Copy(Params, I, Length(Params));

        { Pass the command to the component user to let him a chance to }
        { handle it. If it does, he must return the answer.             }
        TriggerClientCommand(Client, Keyword, Params, Answer);
        if Answer <> '' then begin
            { Event handler has processed the client command, send the answer }
            SendAnswer(Client, Answer);
            Exit;
        end;

        { The command has not been processed, we'll process it }
        if Keyword = '' then begin
            { Empty keyword (should never occurs) }
            SendAnswer(Client, Format(msgCmdUnknown, [Params]));
            Exit;
        end;

        { We need to process the client command, search our command table }
        I := 0;
        while I <= High(FCmdTable) do begin
            if FCmdTable[I].KeyWord = KeyWord then begin
                if I <> ftpcABOR then   { AG V8.02 }
                    Client.CurCmdType := I;             { angus V1.54 }
                Client.AnswerDelayed := FALSE; { AG V1.50 }
                FCmdTable[I].Proc(Client, KeyWord, Params, Answer);
                if not Client.AnswerDelayed then  { AG V1.50 }
                            SendAnswer(Client, Answer);
                Exit;
            end;
            Inc(I);
        end;
        SendAnswer(Client, Format(msgCmdUnknown, [KeyWord]));
    except
        on E:Exception do begin
            SendAnswer(Client, '501 ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientDataSent(Sender : TObject; AError  : Word);
var
    Client  : TFtpCtrlSocket;
begin
    Client := Sender as TFtpCtrlSocket;
    if Client.CloseRequest then begin
        PostMessage(Handle, FMsg_WM_FTPSRV_CLOSE_REQUEST,
                    WPARAM(Client.ID), LPARAM(Client));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ServerClientDisconnect(Sender: TObject;
                                Client: TWSocketClient; Error: Word); { angus V7.00 }
var
    MyClient: TFtpCtrlSocket;
begin
    try
        MyClient := Client as TFtpCtrlSocket;
      { close data channel if still open }
        if MyClient.DataSocket.State = wsConnected then begin
            MyClient.TransferError    := 'ABORT on Disconnect';
            MyClient.AbortingTransfer := TRUE;
            MyClient.DataSocket.Close;
        end;
        CloseFileStreams(MyClient);      { angus V1.57 }
        TriggerClientDisconnect(MyClient, Error);
    except
        { Just ignore any exception here }
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WMFtpSrvAbortTransfer(var msg: TMessage);
var
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
begin
    Client := TFtpCtrlSocket(Msg.LParam);
    { Check if client still in our client list }
    if FSocketServer.IsClient(Client) then begin
        { Check if client.ID is still the same as when message where posted }
        if WPARAM(Client.ID) = Msg.WParam then begin
            Data := Client.DataSocket;
            { make sure to free PasvPort even on aborted connections ! }
            if Assigned(Data) then begin
                if Client.PassiveMode then // FLD 29.12.05
                    FreeCurrentPasvPort(Client);

                Data.ShutDown(2);
                Data.Close;
            end;
            CloseFileStreams(Client);      { angus V1.57 }
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WMFtpSrvCloseData(var msg: TMessage);
var
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
{ !!!!!!!!!!! NGB: next line changed }
    {PortNumber : String;}
{ !!!!!!!!!!! NGB: previous line changed }
begin
    Client := TFtpCtrlSocket(Msg.LParam);
    { Check if client still in our client list }
    if FSocketServer.IsClient(Client) then begin
        { Check if client.ID is still the same as when message where posted }
        if WPARAM(Client.ID) = Msg.WParam then begin
            Data := Client.DataSocket;
{ !!!!!!!!!!! NGB: Free Up Current Port - next 5 lines changed }
            if Assigned(Data) then begin
                if Client.PassiveMode then // FLD 29.12.05
                    FreeCurrentPasvPort(Client);
                Data.ShutDown(1);    {  Wilfried 24/02/04 }
            end;
{ !!!!!!!!!!! NGB: previous 5 lines changed }
            CloseFileStreams(Client);      { angus V1.57 }
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.GetClient(nIndex : Integer) : TFtpCtrlSocket;
begin
    Result := FSocketServer.Client [nIndex] as TFtpCtrlSocket;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check  if a given object is one of our clients }
function TFtpServer.IsClient(SomeThing : TObject) : Boolean;
begin
    Result := FSocketServer.IsClient(Something);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.GetClientCount : Integer;
begin
    Result := FSocketServer.ClientCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerServerStart;
begin
    if Assigned(FOnStart) then
        FOnStart(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerServerStop;
begin
    if Assigned(FOnStop) then
        FOnStop(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerAuthenticate(
    Client            : TFtpCtrlSocket;
    UserName          : String;
    PassWord          : String;
    var Authenticated : Boolean);
begin
    if Assigned(FOnAuthenticate) then
        FOnAuthenticate(Self, Client, UserName, Password, Authenticated);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerOtpMethod(           { angus V1.54 }
    Client          : TFtpCtrlSocket;
    UserName        : TFtpString;
    var OtpMethod   : TOtpMethod);
begin
    if Assigned(FOnOtpMethod) then
        FOnOtpMethod(Self, Client, UserName, OtpMethod);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerOtpGetPassword(      { angus V1.54 }
    Client           : TFtpCtrlSocket;
    UserName         : TFtpString;
    var UserPassword : String);
begin
    if Assigned(FOnOtpGetPassword) then
        FOnOtpGetPassword(Self, Client, UserName, UserPassword);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerChangeDirectory(
    Client         : TFtpCtrlSocket;
    Directory      : String;
    var Allowed    : Boolean);
begin
    if Assigned(FOnChangeDirectory) then
        FOnChangeDirectory(Self, Client, Directory, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerMakeDirectory(
    Client         : TFtpCtrlSocket;
    Directory      : String;
    var Allowed    : Boolean);
begin
    if Assigned(FOnMakeDirectory) then
        FOnMakeDirectory(Self, Client, Directory, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerBuildDirectory(
    Client        : TFtpCtrlSocket;
    var Params    : TFtpString;
    Detailed      : Boolean);
begin
    if Assigned(FOnBuildDirectory) then
        FOnBuildDirectory(Self, Client, Params, Detailed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerAlterDirectory(
    Client        : TFtpCtrlSocket;
    var Params    : TFtpString;
    Detailed      : Boolean);
begin
    if Assigned(FOnAlterDirectory) then
        FOnAlterDirectory(Self, Client, Params, Detailed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerSendAnswer(
    Client     : TFtpCtrlSocket;
    var Answer : TFtpString);
begin
    if Assigned(FOnAnswerToClient) then
        FOnAnswerToClient(Self, Client, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerClientDisconnect(Client : TFtpCtrlSocket; AError  : Word);
begin
    if Assigned(FOnClientDisconnect) then
        FOnClientDisconnect(Self, Client, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerClientConnect(Client : TFtpCtrlSocket; AError  : Word);
begin
    if Assigned(FOnClientConnect) then
        FOnClientConnect(Self, Client, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerStorSessionConnected(
    Client : TFtpCtrlSocket; Data : TWSocket; AError  : Word);
begin
    if Assigned(FOnStorSessionConnected) then
        FOnStorSessionConnected(Self, Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerRetrSessionConnected(
    Client : TFtpCtrlSocket; Data : TWSocket; AError  : Word);
begin
    if Assigned(FOnRetrSessionConnected) then
        FOnRetrSessionConnected(Self, Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerStorSessionClosed(
    Client : TFtpCtrlSocket; Data : TWSocket; AError  : Word);
begin
    if Assigned(FOnStorSessionClosed) then
        FOnStorSessionClosed(Self, Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerRetrSessionClosed(
    Client : TFtpCtrlSocket; Data : TWSocket; AError  : Word);
begin
    if Assigned(FOnRetrSessionClosed) then
        FOnRetrSessionClosed(Self, Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerClientCommand(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnClientCommand) then
        FOnClientCommand(Self, Client, KeyWord, Params, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidatePut(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidatePut) then
        FOnValidatePut(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateSize(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateSize) then
        FOnValidateSize(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateDele(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateDele) then
        FOnValidateDele(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateRmd(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateRmd) then
        FOnValidateRmd(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateRnFr(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateRnFr) then
        FOnValidateRnFr(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateRnTo(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateRnTo) then
        FOnValidateRnTo(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateGet(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateGet) then
        FOnValidateGet(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerStorDataAvailable(
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    Buf    : PAnsiChar; { AG V6.02 }
    Len    : LongInt;
    AError : Word);
begin
    if Assigned(FOnStorDataAvailable) then
        FOnStorDataAvailable(Self, Client, Data, Buf, Len, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerRetrDataSent(
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    AError : Word);
begin
    if Assigned(FOnRetrDataSent) then
        FOnRetrDataSent(Self, Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerGetUniqueFileName(
    Client       : TFtpCtrlSocket;
    var FileName : TFtpString);
begin
    if Assigned (FOnGetUniqueFileName) then
        FOnGetUniqueFileName (Self, Client, FileName);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateMfmt(  { angus V1.39 }
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned (FOnValidateMfmt) then
        FOnValidateMfmt (Self, Client, FilePath, Allowed);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerCalculateMd5(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Md5Sum    : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnCalculateMd5) then
        FOnCalculateMd5(Self, Client, FilePath, Md5Sum, Allowed);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerMd5Calculated(Client: TFtpCtrlSocket; { AG V1.50 }
  const FilePath, Md5Sum: TFtpString);
begin
    if Assigned(FOnMd5Calculated) then
        FOnMd5Calculated(Self, Client, FilePath, Md5Sum);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerCalculateCrc(                           { angus V1.54 }
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Md5Sum    : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnCalculateCrc) then
        FOnCalculateCrc(Self, Client, FilePath, Md5Sum, Allowed);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerCrcCalculated(Client: TFtpCtrlSocket;  { angus V1.54 }
  const FilePath, Md5Sum: TFtpString);
begin
    if Assigned(FOnCrcCalculated) then
        FOnCrcCalculated(Self, Client, FilePath, Md5Sum);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerEnterSecurityContext(                  { AG V1.52 }
    Client : TFtpCtrlSocket);
begin
    if Assigned(FOnEnterSecurityContext) then
        FOnEnterSecurityContext(Self, Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerLeaveSecurityContext(                  { AG V1.52 }
    Client : TFtpCtrlSocket);
begin
    if Assigned(FOnLeaveSecurityContext) then
        FOnLeaveSecurityContext(Self, Client);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateAllo(                          { angus V1.54 }
    Client      : TFtpCtrlSocket;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnValidateAllo) then
        FOnValidateAllo(Self, Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerClntStr (                            { angus V1.54 }
    Client      : TFtpCtrlSocket;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnClntStr) then
        FOnClntStr(Self, Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerSiteMsg (                            { angus V1.54 }
    Client      : TFtpCtrlSocket;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnSiteMsg ) then
        FOnSiteMsg (Self, Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerSiteExec (                            { angus V1.54 }
    Client      : TFtpCtrlSocket;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnSiteExec) then
        FOnSiteExec(Self, Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerSitePaswd (                            { angus V1.54 }
    Client      : TFtpCtrlSocket;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnSitePaswd) then
        FOnSitePaswd(Self, Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerCombine (                            { angus V1.54 }
    Client      : TFtpCtrlSocket;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnCombine) then
        FOnCombine(Self, Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerTimeout(
    Client      : TFtpCtrlSocket;            { angus V1.54 }
    Duration    : Integer;
    var Abort   : Boolean);
begin
    if Assigned(FOnTimeout) then
        FOnTimeout(Self, Client, Duration, Abort);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerDownCompressFile(
    Client    : TFtpCtrlSocket;          { angus V1.54 }
    var Done  : Boolean);
begin
    if Assigned(FOnDownCompressFile) then
        FOnDownCompressFile(Self, Client, Done);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerUpCompressFile(
    Client    : TFtpCtrlSocket;            { angus V1.54 }
    var Done  : Boolean);
begin
    if Assigned(FOnUpCompressFile) then
        FOnUpCompressFile(Self, Client, Done);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerUpCompressedFile(
      Client  : TFtpCtrlSocket);   { angus V1.54 }
begin
    if Assigned(FOnUpCompressedFile) then
        FOnUpCompressedFile(Self, Client);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerDisplay(
       Client      : TFtpCtrlSocket;
       Msg         : TFtpString);  { angus V1.54 }
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Client, Msg);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerHost(
    Client        : TFtpCtrlSocket;
    Host          : TFtpString;
    var Allowed   : Boolean);      { angus V7.01 }
begin
    if Assigned(FOnHost) then
        FOnHost(Self, Client, Host, Allowed);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerRein(
    Client        : TFtpCtrlSocket;
    var Allowed   : Boolean);      { angus V7.01 }
begin
    if Assigned(FOnRein) then
        FOnRein(Self, Client, Allowed);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerLang(
    Client        : TFtpCtrlSocket;
    Lang          : TFtpString;
    var Allowed   : Boolean);      { angus V7.01 }
begin
    if Assigned(FOnLang) then
        FOnLang(Self, Client, Lang, Allowed);     { angus V7.08 }
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerAddVirtFiles(     { angus V7.08 }
    Client          : TFtpCtrlSocket;
    var LocFiles    : TIcsFileRecs;
    var LocFileList : TList;
    var TotalFiles  : Integer;
    ProgressCallback: TMD5Progress);
begin
    if Assigned(FOnAddVirtFiles) then
        FOnAddVirtFiles(Self, Client, LocFiles, LocFileList, TotalFiles, ProgressCallback);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandUSER(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Challenge: string;
begin
    Client.CurCmdType := ftpcUSER;
    Client.UserName   := Trim(Params);
    Client.FtpState   := ftpcWaitingPassword;
  { angus V1.54 - check if user account is set-up for authentication using a
    one time password. If so, OtpMethod is changed to the method and
    Client.OtpSequence and Client.OtpSeed set to the last values saved for
    the account, or OtpSequence set to -1 to generate a new seed }
    TriggerOtpMethod(Client, Client.UserName, Client.OtpMethod);
    if Client.OtpMethod = OtpKeyNone then
        Answer := Format(msgPassRequired, [Client.UserName])
    else begin
        Challenge := OtpCreateChallenge(Client.OtpMethod,
                                        Client.OtpSequence, Client.OtpSeed);
        Answer := Format(msgOptRespRequired, [Challenge, Client.UserName])
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandPASS(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Authenticated : Boolean;
    UserPassword : String ;
    Secs: Integer ;
begin
    if Client.FtpState <> ftpcWaitingPassword then
        Answer := msgNoUser
    else begin
        Client.CurCmdType    := ftpcPASS;
        Client.PassWord      := Trim(Params);
        Authenticated        := TRUE;
     {  angus V1.54 - if authenticating using a one time password, we need to get
       the user account password so that it can tested against the hashed OTP
       password created by the client from the sequence and seed sent in the challenge.
       Note the TriggerAuthenticate event is still called but with Authenticated set
       false if the OTP password failed, allowing the client to check for a clear
       password if required, or log the failure.  If OTP is successful, the new
       Client.OtpSequence should be saved in the user account details }
        if Client.OtpMethod > OtpKeyNone then begin
            UserPassword := '' ;
            TriggerOtpGetPassword(Client, Client.UserName, UserPassword);
            Authenticated := OtpTestPassword(Client.PassWord, UserPassword,
                            Client.OtpMethod, Client.OtpSequence, Client.OtpSeed);
        end;
        TriggerAuthenticate(Client, Client.UserName, Client.PassWord, Authenticated);
        if Authenticated then begin
            Client.FtpState  := ftpcReady;
            Client.Directory := Client.HomeDir;
            Answer           := Format(msgLogged, [Client.UserName])
        end
        else begin
            {angus V7.06 - count failed login attempts, after third MaxAttempts
              delay answer to slow down extra attempts, finally close client
              once MaxAttempts reached (done in EventTimer event) }
            inc (Client.FailedAttempts) ;
            if Client.FailedAttempts > (FMaxAttempts div 3) then begin
                Secs := (Client.FailedAttempts * 2);
                if (Secs > FTimeoutSecsLogin) then Secs := FTimeoutSecsLogin;
                Client.DelayAnswerTick := IcsGetTrgSecs (Secs);
                Client.FtpState        := ftpcFailedAuth;
                Client.AnswerDelayed   := true;
            end
            else begin
                Client.FtpState  := ftpcWaitingUserCode;
                Answer           := msgLoginFailed;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandCDUP(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcCDUP;
    Params := '..';
    CommandChangeDir(Client, Keyword, Params, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandCWD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType    := ftpcCWD;
    CommandChangeDir(Client, Keyword, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandChangeDir(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Allowed : Boolean;
    OldDir  : String;
    DExists : Boolean;
begin
    OldDir := Client.Directory;
    try
      {$IFDEF MSWINDOWS}
        Params := SlashesToBackSlashes(Params);
      {$ELSE}
        Params := BackSlashesToSlashes(Params);
      {$ENDIF}
        Client.Directory := Trim(Params);
        Allowed := IsPathAllowed(Client, Client.Directory);  { V1.52 AG }
        { should this event be before the ftpsCdupHome test??? }
        TriggerChangeDirectory(Client, Client.Directory, Allowed);
        if Allowed then begin
            TriggerEnterSecurityContext(Client);             { V1.52 AG }
            try
                DExists := DirExists(BuildFilePath(Client, Client.Directory, '')); { angus V7.08 support virtual path }
            finally
                TriggerLeaveSecurityContext(Client);         { V1.52 AG }
            end;
            { angus V1.38 make sure windows path exists }
            if (not (ftpCwdCheck in Client.Options)) or DExists or
                    (DExists and (Length(Client.Directory) <= 3)) or  { angus V1.39 }
                  {$IFDEF MSWINDOWS}
                    (AnsiLowerCase(Client.HomeDir) = AnsiLowerCase(Client.Directory)) then { angus V1.42 }
                  {$ELSE}
                    (Client.HomeDir = Client.Directory) then
                  {$ENDIF}
                Answer := Format(msgCWDSuccess, [FormatResponsePath(Client, Client.Directory)])
            else begin
                Answer := Format(msgCWDNoDir, [FormatResponsePath(Client, Client.Directory)]);   { angus V1.38 }
                Client.Directory := OldDir;        { angus V1.38 }
            end;
        end
        else begin
            Client.Directory := OldDir;
            Answer           := Format(msgCWDFailed, ['No permission']);
        end;
    except
        on E:Exception do begin
            Client.Directory := OldDir;
            Answer           := Format(msgCWDFailed, [E.Message]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandXPWD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcXPWD;
    Answer := Format(msgPWDSuccess,
                   [FormatResponsePath(Client, Client.Directory)]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandPWD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcPWD;
    Answer := Format(msgPWDSuccess,
                   [FormatResponsePath(Client, Client.Directory)]); { AG V1.52 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandQUIT(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    Client.CurCmdType := ftpcQUIT;
    Answer            := msgQuit;
    PostMessage(Handle, FMsg_WM_FTPSRV_CLOSE_REQUEST,
                WPARAM(Client.ID), LPARAM(Client));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetInteger(var I : Integer; const Src : String) : LongInt;
begin
    { Skip leading white spaces }
    while (I <= Length(Src)) and IsSpace(Src[I]) do
        Inc(I);
    Result := 0;
    while (I <= Length(Src)) and IsDigit(Src[I]) do begin    { V6.03 }
        Result := Result * 10 + Ord(Src[I]) - Ord('0');
        Inc(I);
    end;
    { Skip trailing white spaces }
    while (I <= Length(Src)) and IsSpace(Src[I]) do
        Inc(I);
    { Check if end of string of comma. If not, error, returns -1 }
    if I <= Length(Src) then begin
        if Src[I] = ',' then
            Inc(I)        { skip comma           }
        else
            raise FtpServerException.Create('GetInteger: unexpected char'); { error, must be comma }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandPORT(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    I : Integer;
    N : LongInt;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    if Client.FEpsvAllArgReceived then begin
        Answer := Format(msgEPSVALLDeny, [Keyword]);
        Exit;
    end;
    try
        Client.CurCmdType := ftpcPORT;
        I                 := 1;
        Client.DataAddr   := IntToStr(GetInteger(I, Params));
        Client.DataAddr   := Client.DataAddr + '.' + IntToStr(GetInteger(I, Params));
        Client.DataAddr   := Client.DataAddr + '.' + IntToStr(GetInteger(I, Params));
        Client.DataAddr   := Client.DataAddr + '.' + IntToStr(GetInteger(I, Params));
        N := GetInteger(I, Params);
        N := (N shl 8) + GetInteger(I, Params);
        Client.DataPort := IcsIntToStrA(N);
        Answer := msgPortSuccess;
    except
        Answer := msgPortFailed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSTOR(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Allowed  : Boolean;
    FilePath : TFtpString;
begin
    try
        if Client.FtpState <> ftpcReady then begin
            Answer := msgNotLogged;
            Exit;
        end;
        if Params = '' then begin                                { V1.52 AG }
            Answer := Format(msgStorFailed, ['File name not specified']);
            Exit;
        end;
        try
            Client.CurCmdType       := ftpcSTOR;
            Client.FileName         := SlashesToBackSlashes(Params);
            Client.HasOpenedFile    := FALSE;
            FilePath                := BuildFilePath(Client, Client.Directory, Client.FileName);
            Allowed := IsPathAllowed(Client, FilePath); { AG V1.52 }
            TriggerValidatePut(Client, FilePath, Allowed);
            if not Allowed then begin
                Answer := msgStorDisabled;
                Exit;
            end;
            Client.FilePath := FilePath;
            PrepareStorDataSocket(Client);
            Answer := Format(msgStorSuccess, [Params]);
        except
            on E:Exception do begin
                Answer := Format(msgStorFailed, [E.Message]);
            end;
        end;
    finally
        { check for success 150..159 in passive mode              }
        if Client.PassiveMode and (Copy(Answer, 1, 2) <> '15') then begin
            { flag for ClientStorSessionClosed that the error-message was already sent! }
            Client.TransferError    := '';
            Client.AbortingTransfer := TRUE;
            { set up Passive DataSocket.EventHandlers }
            { otherwise FreeCurrentPasvPort won't be called ! }
            PreparePassiveStorDataSocket(Client);
        end;
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ New setup fuction for all STOR-based connections                            }
{ Performes the same task as StartSendData for RETR-based connections         }
procedure TFtpServer.PrepareStorDataSocket(Client : TFtpCtrlSocket);
begin
    Client.AbortingTransfer := FALSE;
    Client.TransferError    := 'Transfer Ok';

    if Client.PassiveMode then begin
        PreparePassiveStorDataSocket(Client);
    end
    else begin
        Client.DataSocket.Proto               := 'tcp';
        Client.DataSocket.Addr                := Client.DataAddr;
        Client.DataSocket.Port                := Client.DataPort;
        Client.DataSocket.OnSessionConnected  := ClientStorSessionConnected;
        Client.DataSocket.OnSessionClosed     := ClientStorSessionClosed;
        Client.DataSocket.OnDataAvailable     := ClientStorDataAvailable;
        Client.DataSocket.OnDataSent          := nil;
        Client.DataSocket.LingerOnOff         := wsLingerOff;
        Client.DataSocket.LingerTimeout       := 0;
        if FBindFtpData then begin
            Client.DataSocket.LocalAddr           := Client.GetXAddr;
            Client.DataSocket.LocalPort           := 'ftp-data'; {20}
        end;
        Client.DataSocket.ComponentOptions    := [wsoNoReceiveLoop];
{$IFDEF BUILTIN_THROTTLE}
        Client.DataSocket.BandwidthLimit      := Client.CBandwidthLimit;     { angus V7.12 }
        Client.DataSocket.BandwidthSampling   := Client.CBandwidthSampling;  { angus V7.12 }
{$ENDIF}
        Client.DataSocket.Connect;
        if Client.DataSocket.SocketRcvBufSize <> Client.FRcvBufSize then     { angus  V7.18 }
           Client.DataSocket.SocketRcvBufSize := Client.FRcvBufSize;         { angus  V7.18 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ New setup fuction for all Passive STOR-based data connections               }
procedure TFtpServer.PreparePassiveStorDataSocket(Client : TFtpCtrlSocket);
begin
    Client.DataSocket.OnSessionConnected  := ClientStorSessionConnected;
    Client.DataSocket.OnSessionClosed     := ClientStorSessionClosed;
    Client.DataSocket.OnDataAvailable     := ClientStorDataAvailable;
    Client.DataSocket.OnDataSent          := nil;
    if Client.PassiveConnected then
        Client.DataSocket.OnSessionConnected(Client.DataSocket, 0)
    else
        Client.PassiveStart := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientStorSessionConnected(Sender : TObject; AError  : Word);
var
    Client      : TFtpCtrlSocket;
    Data        : TWSocket;
begin
    Data                     := TWSocket(Sender);
    Client                   := TFtpCtrlSocket(Data.Owner);
    Client.DataSessionActive := TRUE;
    Client.ByteCount := 0;
    Client.XferStartTick := IcsGetTickCountX; { angus V1.54 tick when last xfer started, for performance check }
    Client.LastTick := IcsGetTickCountX;      { angus V1.54 last tick for time out checking }
    Client.ZStreamState := ftpZStateNone;

    if Client.AbortingTransfer then
        Exit; // primary command (e.g. STOR) failed - don't trigger StorSessionConnected
    TriggerStorSessionConnected(Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientStorSessionClosed(Sender : TObject; AError  : Word);
var
    Client      : TFtpCtrlSocket;
    Data        : TWSocket;
    Md5Sum      : String;  { AG V1.50 }
    Duration    : Integer;
    S           : String;
    BytesSec    : Int64;
    Answer      : String;
begin
    Data                     := TWSocket(Sender);
    Client                   := TFtpCtrlSocket(Data.Owner);
{ !!!!!!!! NGB: Free Up Current Port - next 2 lines added }
    if Client.PassiveMode then // FLD 29.12.05
        FreeCurrentPasvPort(Client);
{ !!!!!!!! NGB: previous 2 lines added }

    Client.DataSessionActive := FALSE;
    Client.PassiveStart      := FALSE;
    Client.PassiveConnected  := FALSE;
    Client.RestartPos        := 0;
    { Reset data port to standard value }
    Client.DataPort          := 'ftp-data';

{ angus V1.54 report performance }
    if Assigned(FOnDisplay) then begin
        Duration := IcsElapsedMsecs (Client.XferStartTick);
        S := Client.FilePath + ' ' +
                IntToKbyte(Client.ByteCount) + 'bytes received in ';
        if Duration < 2000 then
            S := S + IntToStr(Duration) + ' milliseconds'
        else begin
            S := S + IntToStr(Duration div 1000) + ' seconds';
            if Client.ByteCount > 32767 then
                BytesSec := 1000 * (Client.ByteCount div Duration)
            else
                BytesSec := (1000 * Client.ByteCount) div Duration;
            S := S + ' (' + IntToKbyte(BytesSec) + 'bytes/sec)';
        end;
        TriggerDisplay (Client, S);
    end;

    if Client.AbortingTransfer and (Client.TransferError = '') then
        Exit; { This happens when the Command itself was failed - do not      }
              { reply on command channel and don't trigger StorSessionClosed! }

    Answer := '';  { angus V1.54 don't send answer yet }
    case Client.CurCmdType of
    ftpcSTOR :
        begin
            if Client.AbortingTransfer then
                Answer := Format(msgStorAborted, [Client.TransferError])
            else if AError = 0 then
                Answer := msgStorOk
            else
                Answer := Format(msgStorError, [GetWinsockErr(AError)]);
        end;
    ftpcAPPE :
        begin
            if Client.AbortingTransfer then
                Answer := Format(msgAppeAborted, [Client.TransferError])
            else if AError = 0 then
                Answer := msgAppeOk
            else
                Answer := Format(msgAppeError, [GetWinsockErr(AError)]);
        end;
    ftpcSTOU :
        begin
            if Client.AbortingTransfer then
                Answer := Format(msgStouAborted, [Client.TransferError])
            else if AError = 0 then
                Answer := Format (msgStouOk, [Client.FileName])
            else
                Answer := Format(msgStouError, [GetWinsockErr(AError)]);
        end;
    else { Should never comes here }
        raise Exception.Create('Program error in ClientStorSessionClosed');
        exit;
    end;

    if (Client.ZStreamState = ftpZStateSaveDecom) and
         (Client.ZFileStream.Size > 0) and Assigned(Client.DataStream) and
                     (NOT Client.AbortingTransfer) and (AError = 0) then begin
        try
            TriggerDisplay(Client, 'Using thread to decompress download file: ' +
                                         Client.ZCompFileName);
            Client.ProcessingThread := TClientProcessingThread.Create(TRUE);
            Client.ProcessingThread.Client := Client;
            Client.ProcessingThread.Sender := Data;
            Client.ProcessingThread.InData := Answer;
            Client.ProcessingThread.Keyword := 'DECOMPRESS';
            Client.ProcessingThread.OnTerminate := ClientProcessingThreadTerminate;
            Client.ProcessingThread.FreeOnTerminate := TRUE;
        {$IFDEF COMPILER14_UP}
            Client.ProcessingThread.Start;
        {$ELSE}
            Client.ProcessingThread.Resume;
        {$ENDIF}
            Client.AnswerDelayed := TRUE;
            exit;
        except
            on E:Exception do begin
                Answer := Format(msgStouError, ['Failed to start decompress - ' + E.Message]);
            end;
        end;
    end;

    { If we had opened a data stream ourself, then close it }
    CloseFileStreams(Client);      { angus V1.54 }

    TriggerStorSessionClosed(Client, Data, AError);

    if Client.MD5OnTheFlyFlag then begin { AG V1.50 }
        MD5Final(Client.MD5Digest, Client.MD5Context);
        Md5Sum := MD5DigestToHex(Client.MD5Digest);   { V7.07 }
        TriggerMd5Calculated(Client, Client.FilePath, UpperCase(Md5Sum));
    end;
    SendAnswer(Client, Answer);  { angus V1.54 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientStorDataAvailable(Sender: TObject; AError  : word);
var
    Len    : Integer;
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    NewPos : TFtpBigInt;
begin
    Data   := TWSocket(Sender);
    Client := TFtpCtrlSocket(Data.Owner);
    Len    := Data.Receive(Client.RcvBuf, Client.RcvSize);
    if Len <= 0 then
        Exit;

    if Client.AbortingTransfer then
        Exit;
    Client.LastTick := IcsGetTickCountX;  { angus V1.54 last tick for time out checking }

    try
        { Trigger the user event for the received data }
        TriggerStorDataAvailable(Client, Data, Client.RcvBuf, Len, AError);

        { We need to open a datastream if not already done and a FilePath }
        { exists (the component user can have nullified the FilePath      }
        if (not Client.HasOpenedFile) and
                     (Length(Client.FilePath) > 0) and
                           (not Assigned(Client.DataStream)) then begin
            { Store the file size temporarily }
            NewPos := IcsGetFileSize(Client.FilePath); { V1.49 }
            { Use different file modes for APPE vs STOR }
            if (Client.CurCmdType = ftpcAPPE) and (NewPos > -1) then begin
                TriggerEnterSecurityContext(Client);  { AG V1.52 }
                try
                    Client.DataStream := OpenFileStream(Client.FilePath,
                                                    Client.FileModeWrite); { angus V1.57 }
                finally
                    TriggerLeaveSecurityContext(Client); { AG V1.52 }
                end;
                { Cached Md5Sum should be deleted } { AG V1.50 }
                if (ftpsCalcMD5OnTheFly in FOptions) then
                    TriggerMd5Calculated(Client, Client.FilePath, '');
            end
            else if (Client.RestartPos > 0) and (NewPos > -1) then begin // check file exists!
                TriggerEnterSecurityContext(Client); { AG V1.52 }
                try
                    Client.DataStream := OpenFileStream(Client.FilePath,
                                                    Client.FileModeWrite); { angus V1.57 }
                finally
                    TriggerLeaveSecurityContext(Client); { AG V1.52 }
                end;
                { Cached Md5Sum should be deleted } { AG V1.50 }
                if (ftpsCalcMD5OnTheFly in FOptions) then
                    TriggerMd5Calculated(Client, Client.FilePath, '');
            end
            else begin
                TriggerEnterSecurityContext(Client); { AG V1.52 }
                try
                    Client.DataStream := OpenFileStream(Client.FilePath, fmCreate);  { angus V1.54 }
                finally
                    TriggerLeaveSecurityContext(Client); { AG V1.52 }
                end;
                NewPos := 0;

              { Calcutate MD5 checksum on the fly, when a new file is uploaded } { AG V1.50 }
                Client.MD5OnTheFlyFlag := ftpsCalcMD5OnTheFly in FOptions;
                if (Client.CurrTransMode = ftpTransModeZDeflate) then
                                             Client.MD5OnTheFlyFlag := false; { angus 1.54 }
                if Client.MD5OnTheFlyFlag then begin
                    MD5DigestInit(Client.MD5Digest); { V7.07 }
                    MD5Init(Client.MD5Context);
                    Client.HashStartPos := 0;   { angus 1.54 }
                    Client.HashEndPos := 0;
                end;
            end;
            { We MUST check for file size >= RestartPos since Seek in any      } { V1.49 }
            { write-mode may write to the stream returning always the correct  }
            { new position.                                                    }
            if Client.RestartPos <= NewPos then begin
                TriggerEnterSecurityContext(Client); { AG V1.52 }
                try
                    NewPos := Client.DataStream.Seek(Client.RestartPos, soBeginning);  { V1.49 }
                finally
                    TriggerLeaveSecurityContext(Client); { AG V1.52 }
                end;
            end;
            if NewPos <> Client.RestartPos then begin
                Client.TransferError    := 'Unable to set resume position in local file';
                Client.AbortingTransfer := TRUE;
                PostMessage(Handle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                            WPARAM(Client.ID), LPARAM(Client));
                Exit;
            end;
            Client.HasOpenedFile := TRUE;
        end;

        { If we have a DataStream, then we need to write the data }
        if Assigned(Client.DataStream) then begin
            Client.ByteCount := Client.ByteCount + Len;
            Client.TotPutBytes := Client.TotPutBytes + Len;    { angus V1.54 }
            TriggerEnterSecurityContext(Client);{ AG V1.52 }
            try
                if (Client.CurrTransMode = ftpTransModeZDeflate) and
                         (Client.ZStreamState = ftpZStateNone) then begin
                 { save compressed data into temp file, decompress on close  }
                    zlibProblemString := '';
                    Client.ZCompFileName := FZlibWorkDir +
                                            GetZlibCacheFileName(Client.FilePath);
                    Client.ZCompFileDelete := True;
                    Client.ZFileStream := OpenFileStream(Client.ZCompFileName, fmCreate);
                    Client.ZStreamState := ftpZStateSaveDecom;
                end;
                if Client.ZStreamState = ftpZStateSaveDecom then
                    Client.ZFileStream.WriteBuffer(Client.RcvBuf^, Len)
                else
                    Client.DataStream.WriteBuffer(Client.RcvBuf^, Len);
            finally
                TriggerLeaveSecurityContext(Client); { AG V1.52 }
            end;
            if Client.MD5OnTheFlyFlag then { AG V1.50 }
                MD5UpdateBuffer(Client.MD5Context, Client.RcvBuf, Len);
        end;
    except
        { An exception occured, so we abort the transfer }
        on E:Exception do begin
            Client.TransferError    := E.Message;
            Client.AbortingTransfer := TRUE;
            PostMessage(Handle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                        WPARAM(Client.ID), LPARAM(Client));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerBuildFilePath(
    Client            : TFtpCtrlSocket;
    const Directory   : String;
    const FileName    : String;
    var   NewFileName : String);
begin
    if Assigned(FOnBuildFilePath) then
         FOnBuildFilePath(Self, Client, Directory, FileName, NewFileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.IsPathAllowed(                                 { AG V1.52 }
    Client           : TFtpCtrlSocket;
    const Path       : String;
    ExcludeBackslash : Boolean) : Boolean;
var
    NewFileName  : String;    { angus V7.08 }
begin
    if (ftpCdUpHome in Client.Options) then begin
    { angus V7.08 check if a virtual directory is being used, assume allowed if non-blank }
        NewFileName := '';
        TriggerBuildFilePath(Client, Path, '?', NewFileName);   { ? used as flag for reverse translation }
        if NewFileName <> '' then
            Result := TRUE  { end V7.08 change }
        else if ExcludeBackslash then
        {$IFDEF MSWINDOWS}
          Result := Pos(AnsiLowerCase(ExcludeTrailingPathDelimiter(Client.HomeDir)),
                        AnsiLowerCase(Path)) = 1
        {$ELSE}
          Result := Pos(ExcludeTrailingPathDelimiter(Client.HomeDir), Path) = 1
        {$ENDIF}
        else
       {$IFDEF MSWINDOWS}
          Result := Pos(AnsiLowerCase(Client.HomeDir), AnsiLowerCase(Path)) = 1;
       {$ELSE}
          Result := Pos(Client.HomeDir, Path) = 1;
       {$ENDIF}
    end
    else
        Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.FormatResponsePath(                                       { AG V1.52 angus V7.08 }
    Client       : TFtpCtrlSocket;
    const InPath : TFtpString): TFtpString;
const
    Slash = '/';
var
    Home : String;
    NewPath : String ;     { angus V7.08 }
begin
    Result := InPath;
    if (ftpHidePhysicalPath in Client.Options) and
       (ftpCdUpHome in Client.Options) then begin
    { angus V7.08 check if a translated virtual directory is being used, returns original virtual path }
        NewPath := '';
        TriggerBuildFilePath(Client, InPath, '?', NewPath);  { ? used as flag for reverse translation }
        if NewPath = '' then NewPath := InPath ;         { no virtual dir, use original path }
        Home := ExcludeTrailingPathDelimiter(Client.HomeDir);
      {$IFDEF MSWINDOWS}
        if Pos(AnsiLowerCase(Home), AnsiLowerCase(InPath)) = 1 then
      {$ELSE}
        if Pos(Home, InPath) = 1 then
      {$ENDIF}
            Result := Copy(InPath, Length(Home) + 1, Length(InPath));
    end;
    while (Length(Result) > 0) and (Result[Length(Result)] = PathDelim) do
        SetLength(Result, Length(Result) - 1);
    if (Length(Result) = 0) then
        Result := Slash
    else begin
        Result := BackSlashesToSlashes(Result);
        if Result[Length(Result)] = ':' then
            Result := Result + Slash;
        if Result[1] <> Slash then
            Result := Slash + Result;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ serge le 5/10/2002 }
function TFtpServer.BuildFilePath(
    Client      : TFtpCtrlSocket;
    Directory   : String;
    FileName    : String) : String;
var
    Drive : String;
    Path  : String;
begin
  {$IFDEF MSWINDOWS}
    FileName := SlashesToBackSlashes(FileName);
  {$ELSE}
    FileName := BackSlashesToSlashes(FileName);
  {$ENDIF}
    PatchIE5(FileName);

    { Gives the application a chance to do the work for us }
    Result := '';
    TriggerBuildFilePath(Client, Directory, FileName, Result);
    if Length(Result) > 0 then
        Exit;                     { Work is done at the app level, done }

    if IsUNC(FileName) then
        Result := FileName
    else if IsUNC(Directory) then begin
        if (Length(FileName) > 0) and (FileName[1] = PathDelim) then begin
            if (ftpCdUpHome in Client.Options) then              { AG V1.52 }
                { absolute path, HomeDir }
                Result := Client.HomeDir + Copy(FileName, 2, Length(FileName))
            else
                Result := ExtractFileDrive(Directory) + FileName;
        end
        else
            Result := Directory + FileName;
    end
    else begin
        if (Length(FileName) > 1) and (FileName[2] = ':') then begin
            Drive := UpperCase(Copy(FileName, 1, 2));
            Path  := Copy(FileName, 3, Length(FileName));
        end
        else if (ftpCdUpHome in Client.Options) and              { AG V1.52 }
                (Length(FileName) > 0) and (FileName[1] = PathDelim) then begin
                { absolute path, HomeDir }
                Drive := ExtractFileDrive(Client.HomeDir);
                Path  := Copy(Client.HomeDir, Length(Drive) + 1, Length(Client.HomeDir)) +
                              Copy(FileName, 2, Length(FileName));
        end
        else begin
            Drive := Copy(Directory, 1, 2);
            Path  := FileName;
        end;
        if (Length(Path) > 0) and (Path[1] = PathDelim) then
            Result := Drive + Path
        else begin
            if Drive <> Copy(Directory, 1, 2) then
                raise FtpServerException.Create('No current dir for ''' + Drive + '''');
            Result := Directory + Path;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandRETR(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Allowed     : Boolean;
    FilePath    : TFtpString;
    DelayedSend : Boolean;
begin
    try
        if Client.FtpState <> ftpcReady then begin
            Answer := msgNotLogged;
            Exit;
        end;

        try
            Client.CurCmdType    := ftpcRETR;
            Client.HasOpenedFile := FALSE;
            Client.ZStreamState  := ftpZStateNone;
            Client.FileName      := SlashesToBackSlashes(Params);
            FilePath             := BuildFilePath(Client, Client.Directory, Client.FileName);
            Allowed := IsPathAllowed(Client, FilePath); { AG V1.52 }
            TriggerValidateGet(Client, FilePath, Allowed);
            if not Allowed then begin
                Answer := msgRetrDisabled;
                Exit;
            end;
            Client.FilePath := FilePath;
            Answer          := Format(msgRetrSuccess, [Params]);
            DelayedSend     := FALSE;
            if Assigned(FOnGetProcessing) then
                FOnGetProcessing(Self, Client, DelayedSend);
            if not DelayedSend then
                DoStartSendData(Client, Answer);  { angus V1.54 added Answer }
        except
            on E:Exception do begin
                Answer := Format(msgRetrFailed, [E.Message]);
            end;
        end;
    finally
        { check for success 150..159 in passive mode }
        if Client.PassiveMode and (Copy(Answer, 1, 2) <> '15') then begin
            { flag for ClientStorSessionClosed that the error-message was already sent! }
            Client.TransferError    := '';
            Client.AbortingTransfer := TRUE;
            { set up Passive DataSocket.EventHandlers        }
            { otherwise FreeCurrentPasvPort won't be called! }
            PreparePassiveRetrDataSocket(Client);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.DoStartSendData(Client : TFtpCtrlSocket; var Answer : TFtpString);
var
    NewPos  : TFtpBigInt;
    FileExt : String;
    Done    : Boolean;
    FreeSpace: Int64;
begin
 { angus V1.54 moved main file opening here from ClientRetrSessionConnected so it's
   done before opening the data connection so 451 answer can be given for a missing
   file, and also check if thread needs to be started to compress file }
    Client.HashStartPos := 0;
    Client.HashEndPos := 0;
    Client.ZStreamState := ftpZStateNone;
    Client.ZCompInfo := '';  { text added to 226 OK answer }

{ We need to open a datastream if not already done and a FilePath }
{ exists the component user can have nullified the FilePath or    }
{ created his own data stream (virtual file feature)              }
    try
        if (not Client.HasOpenedFile) and (Length(Client.FilePath) > 0) and
                                       (not Assigned(Client.DataStream)) then begin
            TriggerEnterSecurityContext(Client);
            try
                if not FileExists(Client.FilePath) then begin
                    Answer := Format(msgRetrNotExists,
                                     [FormatResponsePath(Client, Client.FilePath)]);
                    Exit;
                end;
                Client.DataStream := OpenFileStream(Client.FilePath,
                                                       Client.FileModeRead); { angus V1.57 }
                NewPos := Client.DataStream.Seek(Client.RestartPos, soBeginning);
            finally
                TriggerLeaveSecurityContext(Client);
            end;
            if NewPos <> Client.RestartPos then begin
                Answer := Format(msgRetrFailed, ['Unable to set resume position in local file']);
                CloseFileStreams(Client);      { angus V1.54 }
                Exit;
            end;
            Client.HasOpenedFile := TRUE;
        end;
        if (not Assigned(Client.DataStream)) then begin
            Answer := Format(msgRetrFailed, ['Failed to open local file']);
            Exit;
        end;
        Client.LastTick := IcsGetTickCountX;  { angus V1.54 last tick for time out checking }

     { angus V1.54 see if compressing the file with zlib }
        if (Client.CurrTransMode = ftpTransModeZDeflate) then begin
            Client.ZStreamState := ftpZStateSaveComp;
            Client.ZCurLevel := Client.ZReqLevel;
            Done := false;
            FreeAndNil (Client.ZFileStream);
            if Client.FilePath <> '' then begin { directory listings don't have file name }
             { don't try to compress certain files any further }
                FileExt := ExtractFileExt(LowerCase(Client.FilePath));
                if Pos (FileExt, FZlibNoCompExt) > 0 then
                                               Client.ZCurLevel := Z_NO_COMPRESSION;
                if Client.DataStream.Size > FZlibMaxSize then            { angus V1.55 }
                                               Client.ZCurLevel := Z_NO_COMPRESSION;
             { check sufficient space on work volume for compressed file }
                FreeSpace := GetFreeSpacePath (FZlibWorkDir);
                if (Client.DataStream.Size + 100000) > FreeSpace then begin   { don't fill volume!! }
                    TriggerDisplay(Client, 'Insufficient space on ' + FZlibWorkDir +
                        ', need ' + IntToKByte (Client.DataStream.Size) + ', free ' + IntToKByte (FreeSpace));
                    Answer := Format(msgRetrFailed, ['Failed to compress file, insufficient space']);
                    Exit;
                end;
                Client.ZCompFileName := FZlibWorkDir + GetZlibCacheFileName(Client.FilePath);
                Client.ZCompFileDelete := True;
                TriggerUpCompressFile (Client, Done);
                if Done then begin
                    if NOT Assigned (Client.ZFileStream) then begin
                        Done := false;
                        TriggerDisplay(Client, 'Error: no cache file set in UpCompressFile event');
                    end;
                    Client.ZCompInfo := ' compressed size ' + IntToKbyte
                             (Client.ZFileStream.Size) + 'bytes, uncompressed size ' +
                                         IntToKbyte (Client.DataStream.Size) + 'bytes' ;
                end;
            end
            else begin
                if ftpsCompressDirs in Options then         { V8.04 see if compressing directories }
                    Client.ZCurLevel := Z_BEST_SPEED
                else
                    Client.ZCurLevel := Z_NO_COMPRESSION;   { V8.04 skip compressing directories }
                Client.ZCompFileName := 'Directory: ' + Client.DirListPath ;
                Client.ZCompFileDelete := False;
            end;
            if NOT Done then begin
                if (Client.ProcessingThread <> nil) then begin
                    Answer := Format(msgRetrFailed, ['Failed to compress file, busy']);
                    CloseFileStreams(Client);
                    Exit;
                end;
            { pending - need to allow for another client still compressing this file }
                try
                    TriggerEnterSecurityContext(Client);{ AG 7.02 }
                    try
                        if Client.FilePath <> '' then begin
                            if FileExists(Client.ZCompFileName) then
                                                 DeleteFile (Client.ZCompFileName);
                            Client.ZFileStream := OpenFileStream(Client.ZCompFileName, fmCreate);
                        end
                        else
                            Client.ZFileStream := TMemoryStream.Create;
                    finally
                        TriggerLeaveSecurityContext(Client);{ AG 7.02 }
                    end;
                except
                    Answer := Format(msgRetrFailed, ['Failed to create compress file']);
                    CloseFileStreams(Client);
                    Exit;
                end;
         {angus V8.04 don't use thread if no real compression needed unless more than one meg }
                if (Client.ZCurLevel = Z_NO_COMPRESSION) and
                                            (Client.DataStream.Size < 1000000) then begin
                    TriggerDisplay(Client, 'Skipped thread to compress upload file, no compression');
                    try
                        ZlibCompressStreamEx(Client.DataStream, Client.ZFileStream,
                                    Client.ZCurLevel, zsZLib, false, Self, UpdateThreadOnProgress);
                        Client.ZFileStream.Position := 0 ;
                        Client.ZCompInfo := '' ;
                  { close data file now, not needed any more }
                        Client.DataStream.Destroy;
                        Client.DataStream := Nil;
                    except
                        on E:Exception do begin
                            TriggerDisplay(Client, 'Failed to compress file - ' + E.Message);
                        end;
                    end;
                end
                else begin
                    TriggerDisplay(Client, 'Using thread to compress upload file: ' +
                             Client.ZCompFileName + ', Level ' + IntToStr (Client.ZCurLevel));
                    Client.ProcessingThread := TClientProcessingThread.Create(TRUE);
                    Client.ProcessingThread.Client := Client;
                    Client.ProcessingThread.InData := Answer;
                    Client.ProcessingThread.Keyword := 'COMPRESS';
                    Client.ProcessingThread.OnTerminate := ClientProcessingThreadTerminate;
                    Client.ProcessingThread.FreeOnTerminate := TRUE;
                {$IFDEF COMPILER14_UP}
                    Client.ProcessingThread.Start;
                {$ELSE}
                    Client.ProcessingThread.Resume;
                {$ENDIF}
                    { Since answer is sent later when the thread returns we need }
                    { to set this flag!                                          }
                    Client.AnswerDelayed := TRUE;
                    exit;
                end;
            end;
        end;
        PostMessage(Handle, FMsg_WM_FTPSRV_START_SEND, 0, LPARAM(Client));
    except
        on E: Exception do begin
            Answer := Format(msgRetrFailed, [E.Message]);
            CloseFileStreams(Client);      { angus V1.54 }
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WMFtpSrvStartSend(var msg: TMessage);
var
    Client      : TFtpCtrlSocket;
begin
    Client := TObject(Msg.LParam) as TFtpCtrlSocket;
    StartSendData(Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientPassiveSessionAvailable(Sender : TObject; AError  : Word);
var
    HSocket : TSocket;
    Client  : TFtpCtrlSocket;
    Data    : TWSocket;
begin
    Data    := TWSocket(Sender);
    Client  := TFtpCtrlSocket(Data.Owner);
    HSocket := Data.Accept;
    Data.OnSessionClosed := nil;
    Data.Close;   { We don't need to listen any more }

    if Client.CurCmdType in [ftpcSTOR, ftpcAPPE, ftpcSTOU] then begin { FLD V1.45 fixed ftpcSTOU }
        Client.DataSocket.OnSessionConnected  := ClientStorSessionConnected;
        Client.DataSocket.OnSessionClosed     := ClientStorSessionClosed;
        Client.DataSocket.OnDataAvailable     := ClientStorDataAvailable;
        Client.DataSocket.OnDataSent          := nil;
    end
    else if Client.CurCmdType in [ftpcRETR, ftpcLIST, ftpcNLST, ftpcMLSD,
                                   ftpcSiteDMLSD, ftpcXDMLSD] then begin  { angus V1.41, V1.54, V7.01 }
        Client.DataSocket.OnSessionConnected  := ClientRetrSessionConnected;
        Client.DataSocket.OnSessionClosed     := ClientRetrSessionClosed;
        Client.DataSocket.OnDataAvailable     := nil;
        Client.DataSocket.OnDataSent          := ClientRetrDataSent;
    end
    else begin
        Client.DataSocket.OnSessionConnected  := nil;
        Client.DataSocket.OnSessionClosed     := nil;
        Client.DataSocket.OnDataAvailable     := nil;
        Client.DataSocket.OnDataSent          := nil;
    end;
    Client.DataSocket.LingerOnOff             := wsLingerOff;
    Client.DataSocket.LingerTimeout           := 0;
{$IFDEF BUILTIN_THROTTLE}
    Client.DataSocket.BandwidthLimit          := Client.CBandwidthLimit;     { angus V7.12 }
    Client.DataSocket.BandwidthSampling       := Client.CBandwidthSampling;  { angus V7.12 }
{$ENDIF}
    Client.DataSocket.HSocket                 := HSocket;
    Client.PassiveConnected                   := TRUE;
    if Client.DataSocket.SocketRcvBufSize <> Client.FRcvBufSize then         { angus  V7.18 }
        Client.DataSocket.SocketRcvBufSize    := Client.FRcvBufSize;         { angus  V7.18 }
    if Client.DataSocket.SocketSndBufSize <> Client.FSndBufSize then         { angus  V7.18 }
        Client.DataSocket.SocketSndBufSize    := Client.FSndBufSize;         { angus  V7.18 }
    if Client.PassiveStart then
        Client.DataSocket.OnSessionConnected(Client.DataSocket, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.StartSendData(Client : TFtpCtrlSocket);
begin
    Client.AbortingTransfer              := FALSE;
    Client.DataSent                      := FALSE;
    Client.TransferError                 := 'Transfer Ok';
    if Client.PassiveMode then begin
        PreparePassiveRetrDataSocket(Client);
    end
    else begin
        Client.DataSocket.Close;
        Client.DataSocket.Proto              := 'tcp';
        Client.DataSocket.Addr               := Client.DataAddr;
        Client.DataSocket.Port               := Client.DataPort;
        Client.DataSocket.OnSessionConnected := ClientRetrSessionConnected;
        Client.DataSocket.OnSessionClosed    := ClientRetrSessionClosed;
        Client.DataSocket.OnDataAvailable    := nil;
        Client.DataSocket.OnDataSent         := ClientRetrDataSent;
        Client.DataSocket.LingerOnOff        := wsLingerOff;
        Client.DataSocket.LingerTimeout      := 0;
        if FBindFtpData then begin
            Client.DataSocket.LocalAddr           := Client.GetXAddr;
            Client.DataSocket.LocalPort           := 'ftp-data'; {20}
        end;
        Client.DataSocket.ComponentOptions    := [wsoNoReceiveLoop];
{$IFDEF BUILTIN_THROTTLE}
        Client.DataSocket.BandwidthLimit      := Client.CBandwidthLimit;     { angus V7.12 }
        Client.DataSocket.BandwidthSampling   := Client.CBandwidthSampling;  { angus V7.12 }
{$ENDIF}
        Client.DataSocket.Connect;
        if Client.DataSocket.SocketSndBufSize <> Client.FSndBufSize then     { angus  V7.18 }
            Client.DataSocket.SocketSndBufSize := Client.FSndBufSize;        { angus  V7.18 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ New setup fuction for all Passive RETR-based data connections               }
procedure TFtpServer.PreparePassiveRetrDataSocket(Client : TFtpCtrlSocket);
begin
    Client.DataSocket.OnSessionConnected  := ClientRetrSessionConnected;
    Client.DataSocket.OnSessionClosed     := ClientRetrSessionClosed;
    Client.DataSocket.OnDataAvailable     := nil;
    Client.DataSocket.OnDataSent          := ClientRetrDataSent;
    if Client.PassiveConnected then
        Client.DataSocket.OnSessionConnected(Client.DataSocket, 0)
    else
        Client.PassiveStart := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientRetrSessionConnected(Sender : TObject; AError  : Word);
var
    Client      : TFtpCtrlSocket;
    Data        : TWSocket;
begin
    Data                     := TWSocket(Sender);
    Client                   := TFtpCtrlSocket(Data.Owner);
    Client.DataSessionActive := (AError = 0);

    if Client.AbortingTransfer then
        Exit; { primary command (e.g. RETR) failed - don't trigger }
              { RetrSessionConnected or prepare any data/stream    }

    try
        TriggerRetrSessionConnected(Client, Data, AError);
        if AError <> 0 then
        begin
            raise FtpServerException.Create('Client data socket connection Error - ' +
               GetWinsockErr(AError) + ' - ' + Client.DataAddr + ':' + Client.DataPort); { V1.48 report port in proper decimal }
        end;
    except
        on E: Exception do begin
            Client.AbortingTransfer := TRUE;
            Client.TransferError    := E.Message;
            PostMessage(Handle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                        WPARAM(Client.ID), LPARAM(Client));
            Exit;
        end;
    end;
  { now start sending data stream }
    Client.ByteCount := 0;
    Client.XferStartTick := IcsGetTickCountX; { angus V1.54 tick when last xfer started, for performance check }
    Client.LastTick := IcsGetTickCountX;      { angus V1.54 last tick for time out checking }
    SendNextDataChunk(Client, Data);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientRetrSessionClosed(Sender : TObject; AError  : Word);
var
    Client      : TFtpCtrlSocket;
    Data        : TWSocket;
    Duration    : Integer;
    S           : String;
    BytesSec    : Int64;
begin
    Data                     := TWSocket(Sender);
    Client                   := TFtpCtrlSocket(Data.Owner);

{ !!!!!!!! NGB: Free Up Current Port - next 2 lines added }
    if Client.PassiveMode then // FLD 29.12.05
        FreeCurrentPasvPort(Client);
{ !!!!!!!! NGB: previous 2 lines added }

    Client.DataSessionActive := FALSE;
    Client.PassiveStart      := FALSE;
    Client.PassiveConnected  := FALSE;
    Client.RestartPos        := 0;
    { Reset data port to standard value }
    Client.DataPort          := 'ftp-data';

    { If we had opened a data stream ourself, then close it }
    CloseFileStreams(Client);      { angus V1.54 }

{ angus V1.54 report performance }
    if Assigned(FOnDisplay) then begin
        Duration := IcsElapsedMsecs (Client.XferStartTick);
        S := Client.FilePath;
        if S = '' then S := 'Directory';
        S := S + ' ' + IntToKbyte(Client.ByteCount) + 'bytes sent in ';
        if Duration < 2000 then
            S := S + IntToStr(Duration) + ' milliseconds'
        else begin
            S := S + IntToStr(Duration div 1000) + ' seconds';
            if Client.ByteCount > 32767 then
                BytesSec := 1000 * (Client.ByteCount div Duration)
            else
                BytesSec := (1000 * Client.ByteCount) div Duration;
            S := S + ' (' + IntToKbyte(BytesSec) + 'bytes/sec)';
        end;
        TriggerDisplay (Client, S);
    end;

    if Client.AbortingTransfer and (Client.TransferError = '') then
        Exit; { This happens when the command itself was failed - do not      }
              { reply on command channel and don't trigger RetrSessionClosed! }

    if Client.AbortingTransfer then
        SendAnswer(Client, Format(msgRetrFailed, [Client.TransferError]))
    else if AError <> 0 then
        SendAnswer(Client, Format(msgRetrFailed, ['Error - ' + GetWinsockErr(AError)]))
    else
        SendAnswer(Client, msgRetrOk + Client.ZCompInfo);  { angus V1.54 }

    TriggerRetrSessionClosed(Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SendNextDataChunk(
    Client : TFtpCtrlSocket;
    Data   : TWSocket);
var
    Count : LongInt;
begin
    try
        Count := 0;
        TriggerEnterSecurityContext(Client);           { AG V1.52 }
        try
     { angus V1.54 }
            if Client.ZStreamState = ftpZStateSaveComp then begin
                if Assigned(Client.ZFileStream) then
                    Count := Client.ZFileStream.Read(Client.RcvBuf^, Client.RcvSize);
            end
            else begin
                if Assigned(Client.DataStream) then
                    Count := Client.DataStream.Read(Client.RcvBuf^, Client.RcvSize);
            end;
        finally
            TriggerLeaveSecurityContext(Client);       { AG V1.52 }
        end;
        Client.LastTick := IcsGetTickCountX;  { angus V1.54 last tick for time out checking }

        if Count > 0 then begin
            Client.ByteCount := Client.ByteCount + Count;
            Client.TotGetBytes := Client.TotGetBytes + Count;    { angus V1.54 }
            Data.Send(Client.RcvBuf, Count);
        end
        else begin { EOF }
            if not Client.DataSent then begin
                Client.DataSent := TRUE;
                PostMessage(Handle, FMsg_WM_FTPSRV_CLOSE_DATA,
                            WPARAM(Client.ID), LPARAM(Client));
            end;
        end;
    except
        { An exception occured, so we abort the transfer }
        on E:Exception do begin
            Client.TransferError    := E.Message;
            Client.AbortingTransfer := TRUE;
            PostMessage(Handle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                        WPARAM(Client.ID), LPARAM(Client));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientRetrDataSent(Sender : TObject; AError : Word);
var
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
begin
    Data   := TWSocket(Sender);
    Client := TFtpCtrlSocket(Data.Owner);

    if Client.AbortingTransfer then
        Exit;

    try
        { Trigger the user event for the received data }
        TriggerRetrDataSent(Client, Data, AError);
        if AError <> 0 then
            raise FtpServerException.Create('Send Error - ' + GetWinsockErr(AError));
        SendNextDataChunk(Client, Data);
    except
        { An exception occured, so we abort the transfer }
        on E:Exception do begin
            Client.TransferError    := E.Message;
            Client.AbortingTransfer := TRUE;
            SendAnswer(Client, Format(msgRetrAborted, [Client.TransferError]));
            PostMessage(Handle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                        WPARAM(Client.ID), LPARAM(Client));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSYST(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcSYST;
    Answer            := msgSystem;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandDirectory(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString;
    Detailed    : Boolean);
var
    ListType: TListType;              { angus V1.38 }
begin
    if Detailed then
        ListType := ListTypeUnix
    else
        ListType := ListTypeName;
    CommandDirectory2(Client, Keyword, Params, Answer, ListType); { angus V1.38 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandDirectory2(     { angus V1.38 }
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString;
    ListType    : TListType);               { angus V1.38 }
var
    Path       : TFtpString;
    Args       : String;
    Offset     : Integer;
begin
    try
        CloseFileStreams(Client);      { angus V1.54 }
        Client.AbortingTransfer := FALSE;  { V7.08 }

        try
{ angus 1.54  parse optional file and directory parameters, ie
    (blank)                    ( list all files in working directory set by CWD, shown by PWD )
    -AL                        ( list all files including hidden )
    -R                         ( list all files recursively, include sub-directories )
    -SUBDIRS                   ( list all files recursively, include sub-directories )
    *.zip                      ( list all files with zip extension )
    index.html                 ( list a single file )
    "my index.html"            ( list a single file )
    temp                       ( list all files in specified directory )
    /temp                      ( list all files in specified directory )
    /temp/ -R                  ( list all files in specified directory and sub-directory )
    '/program files' -R
    "/program files/*.zip" -R

        NOTE1: we currently support all parameters for DIR, LIST, NLST, MLSD, SITE INDEX,
              SITE CMLSD, XCMLSD, SITE DMLSD, XDMLSD which is not really RFC compliant, but useful
        NOTE2: we don't yet support multiple arguments, ie only -R or -AL, not both
        NOTE3: -R or -SUBDIRS recursive listings have a file name with path and leading /, ie
               /download/ALLDEPOTS/all/30=page-022864.zip  }
         {$IFDEF MSWINDOWS}
            Params := SlashesToBackSlashes(Params);
         {$ELSE}
            Params := BackSlashesToSlashes(Params);
         {$ENDIF}
            Path := '';
            Args := '';
            Client.DirListHidden := FALSE;
            Client.DirListSubDir := FALSE;
            Client.DirListType := ListType;

         { angus 1.54  parse parameter for file/path and one argument }
            if Length (Params) > 0 then begin
                if Params [1] = '-' then   { just found a argument }
                    Args := Params
                else begin                 { otherwise filename and option argument }
                   Offset := 1;
                   Path := ScanGetNextArg (Params, Offset);  { keep path or file name }
                   Args := ScanGetNextArg (Params, Offset);  { and argument, if any }
                end;
            end;

         { angus 1.54  check directory arguments }
            if (UpperCase(Args) = '-LA') or (UpperCase(Args)= '-AL') then
                                                         Client.DirListHidden := TRUE;
            if (Args = '-R') or (UpperCase(Args) = '-SUBDIRS') then
                                                         Client.DirListSubDir := TRUE;
            if (Client.CurCmdType = ftpcSiteIndex) then Client.DirListSubDir := TRUE;

         { see if application wants to build listing, if not we do it }
            TriggerBuildDirectory(Client, Path, (ListType <> ListTypeName));      { angus V1.38 }
            Client.FilePath := '';       { make sure no file open attempt }
            if not Assigned(Client.DataStream) then begin
                Client.DataStream    := TMemoryStream.Create;
                Client.HasOpenedFile := TRUE;
                BuildDirectory(Client, Path);          { angus V1.54  }
                if Client.AnswerDelayed then exit ;    { angus V1.54 using a thread }
                TriggerAlterDirectory(Client, Path, (ListType <> ListTypeName));  { angus V1.38 }
                Client.DataStream.Seek(0, 0);
            end;

         { angus V1.54 see if returning listing on control socket instead of data socket }
            if Client.CurCmdType in [ftpcSiteIndex, ftpcSiteCmlsd, ftpcXCMLSD] then begin   { angus 7.01 }
                Client.DataStreamReadString(String(Answer), Client.DataStream.Size, Client.CurrentCodePage); { AG 7.02 }
                if Client.CurCmdType = ftpcSiteIndex then
                     Answer := Format (msgIndexFollows, [Params]) +
                                                     #13#10 + Answer + msgIndexDone;
                if Client.CurCmdType in [ftpcSiteCmlsd, ftpcXCMLSD] then    { angus 7.01 }
                     Answer := msgMlstFollows + #13#10 + Answer + msgMlstFollowDone;
                CloseFileStreams(Client);
            end
            else
            begin
                Answer := msgDirOpen;
                DoStartSendData(Client, Answer);  { angus V1.54 added Answer }
            end;
        except
            on E:Exception do begin
                Answer := Format(msgDirFailed, [E.Message])
            end;
        end;
    finally
        { check for success 150..159 in passive mode }
        if (Client.HasOpenedFile) and (Client.PassiveMode) and
                     (NOT Client.AnswerDelayed) and (Copy(Answer, 1, 2) <> '15') then begin  { V7.08 }
            { flag for ClientRetrSessionClosed that the error-message was already sent! }
            Client.TransferError    := '';
            Client.AbortingTransfer := TRUE;
            { set up Passive DataSocket.EventHandlers        }
            { otherwise FreeCurrentPasvPort won't be called! }
            PreparePassiveRetrDataSocket(Client);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandLIST(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcLIST;
    CommandDirectory(Client, KeyWord, Params, Answer, TRUE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandNLST(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcNLST;
    CommandDirectory(Client, KeyWord, Params, Answer, FALSE);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.BuildDirectory(
    Client     : TFtpCtrlSocket;
    var Path   : TFtpString);    { angus 1.54 now Client.Stream and Client.DirListType }
var
    Buf        : String;
    Allowed    : Boolean;
    TotalFiles : integer;  { V7.08 }
begin
    DecodeDate(Now, ThisYear, ThisMonth, ThisDay);  { V7.08 moved from BuildDirList }

 {  angus 1.54 hidden argument now parsed in CommandDirectory2,
               params is now only path or file name }

 { angus 1.54 remove leading / to keep BuildFilePath happy, probably not backward compatible!! }
    if (Length (Path) >= 1) and (Path [1] = PathDelim) then Path := Copy (Path, 2, 999);
    if Path = '' then
{        Client.DirListPath := Client.Directory + '*.*'   V7.08 }
        Client.DirListPath := BuildFilePath(Client, Client.Directory, '*.*')  { angus V7.08 must not skip buildpath }
    else begin
        if Path[Length(Path)] = PathDelim then Path := Path + '*.*';
        Client.DirListPath := BuildFilePath(Client, Client.Directory, Path);
    end;

    Allowed := IsPathAllowed(Client, Client.DirListPath);                 { AG V1.52 }
    if not Allowed then { AG V1.52 }
    begin
        Buf := FormatResponsePath(Client, Client.DirListPath) +
                                                 ' Permission denied' + #13#10;
        Client.DataStreamWriteString(Buf, Client.CurrentCodePage); { angus 7.02 }
        Exit; //***
    end;

 { angus 1.54 see if using a thread to list directory }
    if (((ftpsThreadRecurDirs in Options) and (Client.DirListSubDir)) OR
               (ftpsThreadAllDirs in Options)) and
                        (Client.ProcessingThread = nil) then begin
        TriggerDisplay(Client, 'Using thread to list directory');
        Client.ProcessingThread := TClientProcessingThread.Create(TRUE);
        Client.ProcessingThread.Client := Client;
        Client.ProcessingThread.InData := Path;
        Client.ProcessingThread.Keyword := 'DIRECTORY';
        Client.ProcessingThread.OnTerminate := ClientProcessingThreadTerminate;
        Client.ProcessingThread.FreeOnTerminate := TRUE;
    {$IFDEF COMPILER14_UP}
        Client.ProcessingThread.Start;
    {$ELSE}
        Client.ProcessingThread.Resume;
    {$ENDIF}
        { Since answer is sent later when the thread returns we need }
        { to set this flag!                                          }
        Client.AnswerDelayed := TRUE;
        exit;
    end;
    TriggerEnterSecurityContext(Client);                  { AG V1.52 }
    try
     { angus 1.54 moved all listing code to FtpSrvC }
        Client.BuildDirList(TotalFiles);         { V7.08 }
        if TotalFiles = -1 then
            TriggerDisplay(Client, 'Completed directory listing for: ' +
                                                    Client.DirListPath + ' failed')
        else
            TriggerDisplay(Client, 'Completed directory listing for: ' +
                        Client.DirListPath + ', Total Files: ' + IntToStr (TotalFiles));
    finally
        TriggerLeaveSecurityContext(Client);              { AG V1.52 }
    end;

    if Client.DataStream.Size = 0 then begin
        if TotalFiles = -1 then
            Buf := 'Listing failed' + #13#10  { V7.08 }
        else
            Buf := FormatResponsePath(Client, Client.DirListPath) + ' not found' + #13#10; { AG V1.52 }
        Client.DataStreamWriteString(Buf, Client.CurrentCodePage);  { AG V6.03 }{ AG 7.02 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandTYPE(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Buf : String;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcTYPE;
    Buf := UpperCase(Trim(Params));
    if (Buf = 'A') or (Buf = 'A N') or (Buf = 'I') then begin
        Answer            := Format(msgTypeOk, [Params]);
        Client.BinaryMode := (Buf = 'I');
    end
    else
        Answer := Format(msgTypeFailed, [Params]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandDELE(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName : TFtpString;
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcDELE;
    FileName          := BuildFilePath(Client, Client.Directory, Params);
    Allowed := IsPathAllowed(Client, FileName); { AG V1.52 }
    TriggerValidateDele(Client, FileName, Allowed);
    if not Allowed then begin
        Answer := msgDeleDisabled;
        Exit;
    end;
    if Params = '' then begin
        Answer := msgDeleSyntax;                             { V1.52 AG}
        Exit;
    end;
    Allowed := FALSE;
    TriggerEnterSecurityContext(Client);
    try
        if FileExists(FileName) then begin
            if DeleteFile(FileName) then begin
                Answer := Format(msgDeleOk, [FormatResponsePath(Client, FileName)]);
                Allowed := TRUE;
            end
            else
                Answer := Format(msgDeleFailed, [FormatResponsePath(Client, FileName)]);
        end
        else
            Answer := Format(msgDeleNotExists, [FormatResponsePath(Client, FileName)]);
    finally
        TriggerLeaveSecurityContext(Client);
    end;
    if Allowed then
        { Cached Md5Sum should be deleted }
        TriggerMd5Calculated(Client, FileName, ''); { AG V1.50 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSIZE(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FilePath : TFtpString;
    Allowed  : Boolean;
    Size     : TFtpBigInt;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcSIZE;
    FilePath          := BuildFilePath(Client, Client.Directory, Params);
    Allowed := IsPathAllowed(Client, FilePath); { AG V1.52 }
    TriggerValidateSize(Client, FilePath, Allowed);
    if not Allowed then begin
        Answer := msgSizeDisabled;
        Exit;
    end;

    if Params = '' then
        Answer := msgSizeSyntax                               { V1.52 AG}
    else begin
        try
            TriggerEnterSecurityContext(Client);               { V1.52 AG }
            try
                Size := IcsGetFileSize(FilePath);
                if Size >= 0 then
                    Answer := Format(msgSizeOk, [Size])
                else
                    Answer := Format(msgSizeFailed, ['File not found']);
            finally
                TriggerLeaveSecurityContext(Client);           { V1.52 AG }
            end;
        except
            on E:Exception do begin
                Answer := Format(msgSizeFailed, [E.Message])
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandREST(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcREST;
    try
        Client.RestartPos := atoi64(Params);
        if Client.RestartPos < 0 then begin        { 20020916 }
            Answer            := msgRestZero;
            Client.RestartPos := 0;
        end
        else begin
            if (ftpsModeZNoResume in Options) and
                    (Client.CurrTransMode = ftpTransModeZDeflate) then   { angus V1.55 }
                Answer := msgRestNotModeZ
            else
                Answer := Format(msgRestOk, [Client.RestartPos]);
        end;
    except
        on E:Exception do begin
            Answer            := Format(msgRestFailed, [E.Message]);
            Client.RestartPos := 0;
        end;
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandRNFR(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName : TFtpString;
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcRNFR;
    FileName          := BuildFilePath(Client, Client.Directory, Params);
    Allowed := IsPathAllowed(Client, FileName); { AG V1.52 }
    TriggerValidateRnFr(Client, FileName, Allowed);
    if not Allowed then begin
        Answer := msgRnFrDisabled;
        Exit;
    end;
    if Params = '' then
        Answer := msgRnfrSyntax                              { V1.52 AG}
    else begin
        TriggerEnterSecurityContext(Client);                 { V1.52 AG }
        try
            if FileExists(FileName) or DirExists(Filename) then begin
                Client.FromFileName := FileName;
                Answer              := msgRnfrOk;            { V1.52 AG }
            end
            else
                Answer := Format(msgRnfrNotExists, [FormatResponsePath(Client, FileName)]);
        finally
            TriggerLeaveSecurityContext(Client);             { V1.52 AG }
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandRNTO(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName : TFtpString;
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcRNTO;
    FileName          := BuildFilePath(Client, Client.Directory, Params);
    Allowed := IsPathAllowed(Client, FileName); { AG V1.52 }
    TriggerValidateRnTo(Client, FileName, Allowed);
    if not Allowed then begin
        Answer := msgRnToDisabled;
        Exit;
    end;
    if Params = '' then begin
        Answer := msgRntoSyntax;                              { V1.52 AG}
        Exit;
    end;
    Allowed := FALSE;                                         { V1.52 AG }
    TriggerEnterSecurityContext(Client);                      { V1.52 AG }
    try
        if FileExists(FileName) or DirExists(Filename) then
            Answer := Format(msgRntoAlready, [FormatResponsePath(Client, FileName)])
        else if (not FileExists(Client.FromFileName)) and
           (not DirExists(Client.FromFileName)) then
            Answer := Format(msgRntoNotExists, [FormatResponsePath(Client, Client.FromFileName)])
        else begin
            Client.ToFileName := FileName;
            Allowed := RenameFile(Client.FromFileName, Client.ToFileName);
        end;
    finally
        TriggerLeaveSecurityContext(Client);                  { V1.52 AG }
    end;
    if Allowed then begin
        Answer := Format(msgRntoOk, [FormatResponsePath(Client, Client.FromFileName),
                                    FormatResponsePath(Client, Client.ToFileName)]);
        { Cached Md5Sum should be updated with a new key } { AG V1.50 }
        TriggerMd5Calculated(Client, FileName, '');
    end
    else
        Answer := Format(msgRntoFailed, [FormatResponsePath(Client, Client.FromFileName)]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandNOOP(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    Client.CurCmdType := ftpcNOOP;
    Answer            := Format(MsgNoopOk, [Params]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandMKD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Dir : TFtpString;                                    { V1.52 AG}
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    try
        Client.CurCmdType := ftpcMKD;
        Dir               := BuildFilePath(Client, Client.Directory, Params);
        Allowed := IsPathAllowed(Client, Dir); { AG V1.52 }
        TriggerMakeDirectory(Client, Dir, Allowed);
        if not Allowed then
            Answer := Format(msgMkdFailed, [FormatResponsePath(Client, Dir)]) { V1.52 AG }
        else if Params = '' then
            Answer := msgMkdSyntax                              { V1.52 AG}
        else begin
            TriggerEnterSecurityContext(Client);                { V1.52 AG }
            try
                if DirExists(Dir) or FileExists(Dir) then       { V1.52 AG }
                    Answer := Format(msgMkdAlready, [FormatResponsePath(Client, Dir)]) { V1.52 AG }
                else begin
                    {$I-}
                    MkDir(Dir);
                    if IOResult = 0 then
                        Answer := Format(msgMkdOk, [FormatResponsePath(Client, Dir)]) { V1.52 AG }
                    else
                        Answer := Format(msgMkdFailed, [FormatResponsePath(Client, Dir)]); { V1.52 AG }
                    {$I+}
                end;
            finally
                TriggerLeaveSecurityContext(Client);            { V1.52 AG }
            end;
        end;
    except
        on E:Exception do begin
            Answer := Format(msgMkdFailed, [E.Message])
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandAPPE(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Allowed  : Boolean;
    FilePath : TFtpString;
begin
    try
        if Client.FtpState <> ftpcReady then begin
            Answer := msgNotLogged;
            Exit;
        end;

        try
            Client.CurCmdType       := ftpcAPPE;
            Client.FileName         := SlashesToBackSlashes(Params);
            Client.HasOpenedFile    := FALSE;
            FilePath                := BuildFilePath(Client, Client.Directory, Client.FileName);
            Allowed := IsPathAllowed(Client, FilePath); { AG V1.52 }
            TriggerValidatePut(Client, FilePath, Allowed);
            if not Allowed then begin
                Answer := msgAppeDisabled;
                Exit;
            end;
            Client.FilePath := FilePath;
            PrepareStorDataSocket(Client);
            Client.RestartPos := IcsGetFileSize(Client.FilePath);
            if Client.RestartPos < 0 then
                Client.RestartPos := 0;
            Answer := Format(msgAppeReady, [Params,Client.RestartPos]);
        except
            on E:Exception do begin
                Answer := Format(msgAppeFailed, [E.Message]);
            end;
        end;
    finally
        { check for success 150..159 in passive mode }
        if Client.PassiveMode and (Copy(Answer, 1, 2) <> '15') then begin
            { flag for ClientStorSessionClosed that the error-message was already sent! }
            Client.TransferError    := '';
            Client.AbortingTransfer := TRUE;

            { set up Passive DataSocket.EventHandlers         }
            {  otherwise FreeCurrentPasvPort won't be called! }
            PreparePassiveStorDataSocket(Client);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSTRU(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    Client.CurCmdType := ftpcSTRU;
    Answer            := Format(MsgStruOk, [Params]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandRMD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Dir      : TFtpString;   { V1.52 AG }
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcRMD;
    Dir               := BuildFilePath(Client, Client.Directory, Params);
    Allowed := IsPathAllowed(Client, Dir); { AG V1.52 }
    TriggerValidateRmd(Client, Dir, Allowed);
    if not Allowed then begin
        Answer := msgRmdDisabled;
        Exit;
    end;
    if Params = '' then begin
        Answer := msgMkdSyntax;                            { V1.52 AG}
        Exit;
    end;
    TriggerEnterSecurityContext(Client);                    { V1.52 AG }
    try
        if not DirExists(Dir) then
            Answer := Format(msgRmdNotExists, [FormatResponsePath(Client, Dir)])
        else begin
            {$I-}
            RmDir(Dir);
            if IOResult = 0 then
                Answer := Format(msgRmdOk, [FormatResponsePath(Client, Dir)])
            else
                Answer := Format(msgRmdFailed, [FormatResponsePath(Client, Dir)]);
            {$I+}
        end;
    finally
        TriggerLeaveSecurityContext(Client);               { V1.52 AG }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandABOR(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.DataSocket.State = wsConnected then begin
        Client.TransferError    := 'ABORT requested by client';
        Client.AbortingTransfer := TRUE;
        Client.DataSocket.Close;
    end;
    Answer := msgAborOk;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.GetNextAvailablePasvPort : String;
var
    I        : Integer;
    NewPort  : Integer;
    TablePtr : PBoolean;
begin
    if (FPasvPortRangeSize = 0) or (FPasvPortRangeStart = 0) then
        Result := AnsiChar('0')
    else begin
        Result := AnsiChar('0');
        I := 0;
  { angus V1.56 - allocate sequential ports within range instead of same low ports }
        if FPasvNextNr >= FPasvPortRangeSize then FPasvNextNr := 0;      { angus V1.56 }
        while TRUE do begin
            TablePtr := IncPtr(FPasvPortTable, SizeOf(Boolean) * FPasvNextNr); { AG V6.02 }
            if TablePtr^ = FALSE then begin
                TablePtr^ := TRUE;
                NewPort   := FPasvPortRangeStart + FPasvNextNr;          { angus V1.56 }
                Inc(FPasvNextNr);                                        { angus V1.56 }
                Result    := IntToStr(NewPort);
                break;
            end;
            Inc(FPasvNextNr);                                            { angus V1.56 }
            if FPasvNextNr >= FPasvPortRangeSize then FPasvNextNr := 0;  { angus V1.56 }
            Inc(I);
            if I >= FPasvPortRangeSize then
                break;  { no free ports in range - angus V1.56 }
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.FreeCurrentPasvPort(AClient : TFtpCtrlSocket);
var
    CurrentPort : Integer;
{$IFNDEF COMPILER12_UP}
    ErrorCode   : Integer;
{$ENDIF}
begin
    if (FPasvPortRangeSize = 0) or (FPasvPortRangeStart = 0) then
        Exit;
    { FLD changed following lines, because                                   }
    { FreeCurrentPasvPort might be called when the socket is already closed! }
    if AClient.DataSocket.State = wsClosed then
{$IFNDEF COMPILER12_UP}
        Val(AClient.DataSocket.Port, CurrentPort, ErrorCode)
{$ELSE}
        CurrentPort := atoi(AClient.DataSocket.Port)
{$ENDIF}
    else
{$IFNDEF COMPILER12_UP}
        Val(AClient.DataSocket.GetXPort, CurrentPort, ErrorCode);
{$ELSE}
        CurrentPort := atoi(AClient.DataSocket.GetXPort);
{$ENDIF}
    if (CurrentPort >= FPasvPortRangeStart) and
       (CurrentPort <= (FPasvPortRangeStart + FPasvPortRangeSize)) then begin
        PBoolean(IncPtr(FPasvPortTable,                        { AG V6.02 }
                 SizeOf(Boolean) * (CurrentPort - FPasvPortRangeStart)))^ := FALSE;
    end;
    AClient.PassiveMode := FALSE;  // FLD 29.12.05
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandPASV(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    saddr     : TSockAddrIn;
    saddrlen  : Integer;
    DataPort  : Integer;
    IPAddr    : TIcsInAddr;
    PASVAddr  : TIcsInAddr;
    APasvIp   : TFtpString;
    SetPasvIp : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    if Client.FEpsvAllArgReceived then begin
        Answer := Format(msgEPSVALLDeny, [Keyword]);
        Exit;
    end;

    try
        { Get our IP address from our control socket }
        saddrlen := SizeOf(saddr);
        Client.GetSockName(saddr, saddrlen);
        IPAddr   := PIcsInAddr(@saddr.sin_addr)^;

        { FLD Make sure to free up a previous connected passive data-socket!! }
        { can happen if a PASV-command is issued, but a passive connection is }
        { never connected, and then a subsequent PASV-command is issued.      }
        if Client.PassiveMode then // FLD 29.12.05
            FreeCurrentPasvPort(Client);

        Client.DataSocket.Close;
        Client.DataSocket.Addr  := '0.0.0.0';   { Any addr }

        Client.DataSocket.Port  := GetNextAvailablePasvPort; { '0';          Any port  }
        if Client.DataSocket.Port = '' then
            raise Exception.Create('No available PASV Ports');

        Client.DataSocket.Proto := 'tcp';
        Client.DataSocket.OnSessionAvailable := ClientPassiveSessionAvailable;
        Client.DataSocket.OnSessionConnected := nil;
        Client.DataSocket.OnSessionClosed    := nil;
        Client.DataSocket.OnDataAvailable    := nil;
        Client.DataSocket.ComponentOptions   := [wsoNoReceiveLoop];
        Client.DataSocket.Listen;

        { Get the port assigned by winsock }
        saddrlen := SizeOf(saddr);
        Client.DataSocket.GetSockName(saddr, saddrlen);
        DataPort := WSocket_ntohs(saddr.sin_port);

        if Client.sin.sin_addr.s_addr = WSocket_htonl($7F000001) then
            Answer := Format(msgPasvLocal,
                          [IcsHiByte(DataPort),
                           IcsLoByte(DataPort)])
        else begin
            APasvIp := FPasvIpAddr;
            SetPasvIp := (APasvIp <> '') and (not
                         (((ftpsNoPasvIpAddrInLan in FOptions) and
                           IcsIsIpPrivate(Client.PeerSAddr.sin_addr)) or
                          ((ftpsNoPasvIpAddrSameSubnet in FOptions) and
                           IcsAddrSameSubNet(PInAddr(@IPAddr.S_addr)^, Client.PeerSAddr.sin_addr))));

            if Assigned(FOnPasvIpAddr) then begin
                FOnPasvIpAddr(Self, Client, APasvIp, SetPasvIp);
                SetPasvIp := SetPasvIp and (APasvIp <> '');
            end;

            if not SetPasvIp then
                Answer := Format(msgPasvRemote,
                          [ord(IPAddr.S_un_b.s_b1),
                           ord(IPAddr.S_un_b.s_b2),
                           ord(IPAddr.S_un_b.s_b3),
                           ord(IPAddr.S_un_b.s_b4),
                           IcsHiByte(DataPort),
                           IcsLoByte(DataPort)])
            else begin
                PASVAddr.S_addr := WSocket_inet_addr(AnsiString(APasvIp));
                if (PASVAddr.S_addr = u_long(INADDR_NONE)) or
                            (PASVAddr.S_addr = 0) then { angus v1.53 0.0.0.0 not allowed }
                    raise Exception.Create('Invalid PASV IP Address')
                else
                    Answer := Format(msgPasvRemote,
                          [ord(PASVAddr.S_un_b.s_b1),
                           ord(PASVAddr.S_un_b.s_b2),
                           ord(PASVAddr.S_un_b.s_b3),
                           ord(PASVAddr.S_un_b.s_b4),
                           IcsHiByte(DataPort),
                           IcsLoByte(DataPort)]);
            end;
        end;

        Client.PassiveMode      := TRUE;
        Client.PassiveStart     := FALSE;
        Client.PassiveConnected := FALSE;
    except
        on E:Exception do begin
            Answer := Format(msgPasvExcept, [E.Message]);
            try
                Client.DataSocket.Close;
            except
                { Ignore any exception here }
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ angus V1.38  added set modification date and time version                        }
{ angus v1.53  support fractional seconds, usually milliseconds, updating time
{ MDTM default.asp                    get modification date and time               }
{ MFMT 20040804102811 default.asp     set modification date and time UTC time      }
{ MDTM 20040804102811 default.asp     set modification date and time local time    }
{ MDTM 20040804102811+60 default.asp  set modification date and time UTC + 60 mins }
{ MDTM 20040804102811-60 default.asp  set modification date and time UTC - 60 mins }
{ MFMT 20040804102811.1 default.asp   set modification date and time UTC time      }
{ MFMT 20040804102811.12 default.asp  set modification date and time UTC time      }
{ MFMT 20040804102811.123 default.asp set modification date and time UTC time      }
procedure TFtpServer.CommandMDTM(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileTime : String;
    FileName : TFtpString;
    I, J     : Integer;
    UtcFlag  : Boolean;
    SuccFlag : Boolean;
    FileDT   : TDateTime;
    Bias     : Integer;
    Allowed  : Boolean;         { angus V1.39 }
    FExists  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin   { angus V1.39 }
        Answer := msgNotLogged;
        Exit;
    end;

    try
        if Keyword = 'MFMT' then            { angus V1.39 else assume MDTM }
            Client.CurCmdType := ftpcMFMT
        else
            Client.CurCmdType := ftpcMDTM;
        J                 := 1;
        FileDT            := 0;
        UtcFlag           := FALSE;
        Allowed           := TRUE;

        { look for numeric date and time - angus V1.53 with or without millisecs }
        while (J <= Length(Params)) and
              (((Params[J] >= '0') and (Params[J] <= '9')) or (Params[J] = '.')) do
           Inc(J);
        if (J >= 15) and (J <= 19) then begin  { found date and time so we are setting it, not getting it }
            FileDT := MDTM2Date (Copy (Params, 1, J - 1));
            if FileDT < 10 then begin
                Answer := msgMdtmSyntax;
                Exit;
            end;
            I := J;

            { see if UTC time offset in minutes is passed }
            while (J <= Length(Params)) and
                  ((Params[J] = '+') or (Params[J] = '-') or
                   ((Params[J] >= '0') and (Params[J] <= '9'))) do
                Inc(J);
            if Client.CurCmdType = ftpcMFMT then
                UtcFlag := TRUE
            else begin
                if I <> J then begin
                    UtcFlag := TRUE;
                    Bias := atosi(Copy (Params, I, 4));   { signed integer, +60, -120, +0 }
                    if Bias <> 0 then FileDT := FileDT + (Bias / (60.0 * 24.0));
                end;
            end;
        end
        else
            J := 1;
        while (J <= Length(Params)) and (Params[J] = ' ') do
           Inc(J);
        FileName := BuildFilePath(Client, Client.Directory , Copy (Params, J, 999));
        if Params = '' then begin
            Answer := msgMdtmSyntax;
            Exit;
        end;
        TriggerEnterSecurityContext(Client);                  { V1.52 AG }
        try
            FExists := FileExists(FileName) OR DirExists(FileName);  { A. Haas, V1.53 }
        finally
            TriggerLeaveSecurityContext(Client);              { V1.52 AG }
        end;
        if not FExists then
            Answer := Format(msgMdtmNotExists, [FormatResponsePath(Client, FileName)])
        else if FileDT <> 0 then begin     { set file time stamp }
            Allowed := IsPathAllowed(Client, FileName); { AG V1.52 }
            TriggerValidateMfmt(Client, FileName, Allowed);   { angus V1.39 }
            if not Allowed then begin
                Answer := msgStorDisabled;
                Exit;
            end;
            TriggerEnterSecurityContext(Client);              { V1.52 AG }
            try
                if UtcFlag then
                    SuccFlag := UpdateUFileAge (FileName, FileDT)
                else
                    SuccFlag := UpdateFileAge (FileName, FileDT);
            finally
                TriggerLeaveSecurityContext(Client);          { V1.52 AG }
            end;
            if SuccFlag then begin
              { Cached Md5Sum should be updated with a new time and date } { angus V7.09 }
                TriggerMd5Calculated(Client, FileName, '');
                if Client.CurCmdType = ftpcMFMT then    { angus V1.39 }
                    Answer := msgMfmtChangeOK
                else
                    Answer := msgMdtmChangeOK ;
            end
            else
                Answer := msgMdtmChangeFail;
        end
        else if Client.CurCmdType = ftpcMFMT then   { angus V1.39 never returns time }
            Answer := msgMdtmSyntax
        else begin
            TriggerEnterSecurityContext(Client);              { V1.52 AG }
            try
                FileTime := FileUtcStr(FileName);   { return file time stamp }
            finally
                TriggerLeaveSecurityContext(Client);          { V1.52 AG }
            end;
            if Length(FileTime) <> 0 then
                Answer := Format(msgMdtmOk, [FileTime])
            else
                Answer := Format(msgMdtmFailed,
                                 ['UTC File time retrieval failed']) ;
        end;
    except
        on E:Exception do begin
            Answer := Format(msgMdtmChangeFail, [E.Message])
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandMode(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FreeSpace: Int64;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    if (Params = '') then begin
        Answer := msgModeSyntax;
        Exit;
    end;
    Params := Uppercase (Params);
    if (Params <> 'S') then begin
        Answer := Format (msgModeNotS, [Params]);
   { angus V1.54 }
        if (ftpModeZCompress in Client.Options) and (Params = 'Z') then begin
       { check sufficient space on work volume for compressed files }
            try
                ForceDirectories(FZlibWorkDir);
                FreeSpace := GetFreeSpacePath (FZlibWorkDir);
            except
                FreeSpace := -1;
            end;
            if FZlibMinSpace > FreeSpace then begin   { don't fill volume!! }
                if FreeSpace = -1 then
                    TriggerDisplay(Client, 'Error, working directory volume not available ' +
                                   FZlibWorkDir + ' - ' + GetWindowsErr (GetLastError))
                else
                    TriggerDisplay(Client, 'Insufficient space on ' + FZlibWorkDir +
                     ', need ' + IntToKByte(FZlibMinSpace) + ', free ' + IntToKByte(FreeSpace));
                Client.CurrTransMode := FtpTransModeStream;
            end
            else begin
                Client.CurrTransMode := FtpTransModeZDeflate;
                Answer := Format (msgModeOK, [Params]);
            end;
        end;
        Exit;
    end;
    Client.CurrTransMode := FtpTransModeStream;
    Answer := Format (msgModeOK, [Params]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandOverflow(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Buf : array [0..1023] of char;
begin
    Client.CurCmdType := ftpcOVER;
    { Disable receiving }
    Client.Shutdown(0);
    { Flush receive buffer }
    while (Client.Receive(@Buf, SizeOf(buf)) > 0) do;
    { Answer to client }
    Answer := msgOverflow;
    { Will close connection }
    PostMessage(Handle, FMsg_WM_FTPSRV_CLOSE_REQUEST,
                WPARAM(Client.ID), LPARAM(Client));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ [ep] STOU command support                                                   }
{ This code is more or less the same as CommandSTOR, with the addition of     }
{ GetUniqueFileName event triggering to let the user a chance to provide a    }
{ file name.                                                                  }
procedure TFtpServer.CommandSTOU(
    Client: TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    UniqueName : TFtpString;
    Allowed    : Boolean;
    FilePath   : TFtpString;
begin
    try
        if Client.FtpState <> ftpcReady then begin
            Answer := msgNotLogged;
            Exit;
        end;

        try
            Client.CurCmdType       := ftpcSTOU;
            Client.HasOpenedFile    := FALSE;
            UniqueName              := '';//SlashesToBackSlashes(Params); { V1.52 AG }

            { Fire the GetUniqueFileName event to get the file name  }
            { to be used to store data                               }
            TriggerGetUniqueFileName (Client, UniqueName);

            TriggerEnterSecurityContext(Client);             { V1.52 AG }
            try
                { no file name has been provided, or provided one        }
                { already exists => create one                           }
                if (UniqueName = '') or
                   (FileExists(BuildFilePath(Client, Client.Directory,
                                              UniqueName))) then begin
                    UniqueName := ExtractFilename(CreateUniqueFile(
                                        Client.Directory, 'FTP', ''));
                    if UniqueName = '' then begin
                        Answer := Format(msgStouFailed, ['Error creating unique file']);
                        Exit;
                    end;
                end;
            finally
                TriggerLeaveSecurityContext(Client);         { V1.52 AG }
            end;

            Client.FileName   := UniqueName;
            FilePath          := BuildFilePath(Client, Client.Directory,
                                                     Client.FileName);
            Allowed := IsPathAllowed(Client, FilePath); { V1.52 AG }
            TriggerValidatePut(Client, FilePath, Allowed);
            if not Allowed then begin
                Answer := msgStorDisabled;
                DeleteFile(FilePath); // delete the created file { V1.52 AG }
                Exit;
            end;
            Client.FilePath := FilePath;
            PrepareStorDataSocket(Client);
            Answer := Format(msgStouSuccess, [UniqueName]);
        except
            on E:Exception do begin
                Answer := Format(msgStouFailed, [E.Message]);
            end;
        end;
    finally
        { check for success 150..159 in passive mode }
        if Client.PassiveMode and (Copy(Answer, 1, 2) <> '15') then begin
            { flag for ClientStorSessionClosed that the error-message was already sent! }
            Client.TransferError    := '';
            Client.AbortingTransfer := TRUE;
            { set up Passive DataSocket.EventHandlers        }
            { otherwise FreeCurrentPasvPort won't be called! }
            PreparePassiveStorDataSocket(Client);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandFEAT(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    try
        Client.CurCmdType := ftpcFEAT;
        Answer := msgFeatFollows + #13#10 +
                  ' HOST'+ #13#10 +             { angus V7.01 }
                  ' SIZE'+ #13#10 +
                  ' REST STREAM'+ #13#10 +      { angus V1.39 (been supported for years) }
                  ' MDTM'+ #13#10 +
                  ' MDTM YYYYMMDDHHMMSS[+-TZ] filename'+ #13#10 +       { angus V1.38 }
                  ' MLST size*;type*;perm*;create*;modify*;'+ #13#10 +  { angus V1.38 }
                  ' MFMT'+ #13#10 +                                     { angus V1.39 }
                  ' MD5'+ #13#10 +                                      { angus V1.39 }
                  ' XCRC "filename" start end'+ #13#10 +                { angus V1.54 }
                  ' XMD5 "filename" start end'+ #13#10 +                { angus V1.54 }
                  ' CLNT'+ #13#10 +                                     { angus V1.54 }
                  ' SITE INDEX;ZONE';                                   { angus V1.54 }
        if Assigned (FOnSiteMsg) then Answer := Answer + ';MSG';         { angus V1.54 }
        if Assigned (FOnSiteExec) then Answer := Answer + ';EXEC';       { angus V1.54 }
        if Assigned (FOnSitePaswd) then Answer := Answer + ';PSWD';      { angus V1.54 }
        if ftpsSiteXmlsd in FOptions then
                        Answer := Answer + ';CMLSD;DMLSD'; { angus V1.54 }
        Answer := Answer + #13#10;
        if Assigned (FOnCombine) then
               Answer := Answer + ' COMB'+ #13#10; { angus V1.54 }
        if ftpModeZCompress in Client.Options then
               Answer := Answer + ' MODE Z'+ #13#10;
        if ftpsSiteXmlsd in FOptions then
               Answer := Answer + ' XCMLSD' + #13#10 +
                                  ' XDMLSD' + #13#10;        { angus V7.01 }
        if ftpsEnableUtf8 in FOptions then
               Answer := Answer + ' UTF8' + #13#10 +
                                  ' LANG ' + FLanguage + #13#10 +
                                  ' OPTS MODE;UTF8;' + #13#10; { angus V7.01 }
    {$IFDEF USE_SSL}
        if Self is TSslFtpServer then begin     {  V1.48 }
            if Client.FtpSslTypes <> [] then begin             { V1.47 }
                if not (ftpImplicitSsl in Client.FtpSslTypes) then begin
                    Answer := Answer + ' AUTH ';
                if ftpAuthTls in Client.FtpSslTypes then
                    Answer := Answer + 'TLS;';
                if ftpAuthSsl in Client.FtpSslTypes then
                    Answer := Answer + 'SSL;';
                if ftpAuthTlsP in Client.FtpSslTypes then
                    Answer := Answer + 'TLS-P;';
                if ftpAuthTlsC in Client.FtpSslTypes then
                    Answer := Answer + 'TLS-C;';
                Answer := Answer +  #13#10 +
                          ' CCC'+ #13#10;
            {if Client.FtpSslTypes(Self).FFtpSslType = sslTypeAuthSsl then
                Answer := Answer + '  AUTH TLS;SSL;' + #13#10;}
            end;
            Answer := Answer + ' PROT C;P;' + #13#10 +
                               ' PBSZ'      + #13#10;
            end;
        end;
    {$ENDIF}
        Answer := Answer + msgFeatFollowDone;
    except
        on E:Exception do begin
            Answer := Format(msgFeatFailed, [E.Message]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SetPasvPortRangeSize(const NewValue: Integer);
var
    OldValue : Integer;
    TablePtr : PBoolean;
    I        : Integer;
begin
    if (NewValue < 0) or (NewValue > 65535) then
        raise ERangeError.CreateFmt('Invalid PasvPortRangeSize %d.', [NewValue]);
    if FPasvPortRangeSize = NewValue then
        Exit;
    OldValue := FPasvPortRangeSize;

    { If we reduce the range, we must be sure to not affect any port in use }
    if NewValue < OldValue then begin
        { Check if any port is used before changing }
        TablePtr := IncPtr(FPasvPortTable, SizeOf(Boolean) * NewValue); { AG V6.02 }
        I        := NewValue;
        while I < OldValue do begin
            if TablePtr^ then
                raise Exception.Create('Unable to change PasvPortRangeSize ' +
                                       'when port is in use.');
            Inc(I);
            Inc(TablePtr);
        end;
    end;
    ReallocMem(FPasvPortTable, NewValue);
    FPasvPortTableSize := NewValue;
    FPasvPortRangeSize := NewValue;
    if OldValue >= NewValue then
        Exit;

    TablePtr := IncPtr(FPasvPortTable, SizeOf(Boolean) * OldValue); { AG V6.02 }
    while OldValue < NewValue do begin
        TablePtr^ := FALSE;
        Inc(TablePtr);
        Inc(OldValue);
    end;
    FPasvNextNr := 0;  { angus V1.56 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SetPasvPortRangeStart(const NewValue: Integer);
var
    TablePtr : PBoolean;
    I        : Integer;
begin
    if (NewValue < 0) or (NewValue > 65535) then
        raise ERangeError.CreateFmt('Invalid PasvPortRangeStart %d.', [NewValue]);
    if FPasvPortRangeStart = NewValue then
        Exit;
    { Check if any port is used before changing }
    TablePtr := FPasvPortTable;
    I        := 0;
    while I < FPasvPortRangeSize do begin
        if TablePtr^ then
            raise Exception.Create('Unable to change PasvPortRangeStart ' +
                                   'when port is in use.');
        Inc(I);
        Inc(TablePtr);
    end;

    { Now we can change PasvPortRangeStart }
    FPasvPortRangeStart := NewValue;
    FPasvNextNr := 0;  { angus V1.56 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandMLST(   { angus V1.38 }
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    F          : TSearchRec;
    FileName   : String;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcMLST;
    if Params = '' then Params := '*.*';   { current directory }
    FileName := BuildFilePath(Client, Client.Directory, Params);
    if not IsPathAllowed(Client, FileName) then begin  { V1.52 AG }
        Answer := msgMlstDenied;
        Exit;
    end;
    TriggerEnterSecurityContext(Client);                    { V1.52 AG }
    try
        if FindFirst(FileName, faArchive + faDirectory, F) = 0 then
            Answer := msgMlstFollows + Params + #13#10 +
                      ' ' + FormatFactsDirEntry(F, F.Name) + #13#10 + { angus 1.54 added name }
                      msgMlstFollowDone
        else
            Answer := Format(msgMlstNotExists, [Params]);
        FindClose(F);
    finally
        TriggerLeaveSecurityContext(Client);                { V1.52 AG }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandMLSD(   { angus V1.38 }
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcMLSD;
    CommandDirectory2(Client, KeyWord, Params, Answer, ListTypeFacts);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} { angus V1.54 }
procedure FileMD5OnProgress(
    Obj: TObject;
    Count: Int64;
    var Cancel: Boolean);
begin
    Cancel := (Obj as TFtpCtrlSocket).AbortingTransfer;
    (Obj as TFtpCtrlSocket).LastTick := IcsGetTickCountX;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandMD5(   { angus V1.39 }
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName  : TFtpString;
    Md5Sum    : TFtpString;
    Allowed   : Boolean;
    FileSize  : TFtpBigInt; { AG V1.50 }
    Offset    : Integer;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Md5Sum  := '';
    Client.HashStartPos := 0;
    Client.HashEndPos := 0;

    try
        if Keyword = 'XMD5' then begin    { angus V1.54 }
            Client.CurCmdType := ftpcXMD5;
            Offset := 1;
            FileName := ScanGetNextArg(Params, Offset);                     { keep file name }
            Client.HashStartPos := atoi64(ScanGetNextArg (Params, Offset));  { start position, if any }
            Client.HashEndPos := atoi64(ScanGetNextArg (Params, Offset));    { end position, if any }
            if (Client.HashStartPos > 0) and (Client.HashEndPos = 0) then begin
                Client.HashEndPos := Client.HashStartPos;  { single argument is end position }
                Client.HashStartPos := 0;
            end ;
        end
        else begin
            Client.CurCmdType := ftpcMD5;
            FileName := Params;
        end;
        FileName := BuildFilePath(Client, Client.Directory, FileName);
        Allowed := IsPathAllowed(Client, FileName); { AG V1.52 }
        { Ideally the MD5 sum is being retrieved from a cache or file so it's not  }
        { done repeatedly, if left blank we'll do it here. MD5 may be used to check}
        { uploaded/downloaded files, so keep a timestamp with the sum.             }
        TriggerCalculateMd5(Client, FileName, Md5Sum, Allowed);
        if not Allowed then begin
             Answer := msgRetrDisabled;
             Exit;
        end;
        if Md5Sum = '' then begin
            FileSize := IcsGetFileSize(FileName); { AG V1.50 }
            if FileSize = -1 then begin { AG V1.50 }
                TriggerMd5Calculated(Client, FileName, Md5Sum); { AG V1.50 }
                Answer := Format(msgMd5NotFound, [Params]);
                Exit;
            end ;
            { Calculate a 32-byte MD5 sum. If file size is small we may use }
            { a blocking function.                                 AG V1.50 }
            if (FMd5UseThreadFileSize = 0) or
                               (FileSize < FMd5UseThreadFileSize) then begin
                Md5Sum := FtpFileMD5(FileName, Client, FileMD5OnProgress,
                        Client.HashStartPos, Client.HashEndPos, Client.FileModeRead); { angus V1.57 V7.07 }
                TriggerMd5Calculated(Client, FileName, UpperCase(Md5Sum));
            end
            else begin
                { Use a thread to calculate MD5 checksum which otherwise }
                { would block the server.                       AG V1.50 }
                if Client.ProcessingThread <> nil then begin
                    //TriggerMd5Calculated(Client, FileName, '');
                    Answer := Format(msgMd5Failed, [Params]);
                    Exit;
                end ;
                { AG V1.50 }
                TriggerDisplay(Client, 'Using thread to calculate MD5Sum');  { angus V1.54 }
                Client.ProcessingThread := TClientProcessingThread.Create(TRUE);
                Client.ProcessingThread.Client := Client;
                Client.ProcessingThread.InData := FileName;
                Client.ProcessingThread.Params := Params;
                Client.ProcessingThread.Keyword := Keyword;
                Client.ProcessingThread.OnTerminate := ClientProcessingThreadTerminate;
                Client.ProcessingThread.FreeOnTerminate := TRUE;
            {$IFDEF COMPILER14_UP}
                Client.ProcessingThread.Start;
            {$ELSE}
                Client.ProcessingThread.Resume;
            {$ENDIF}
                { Since answer is sent later when the thread returns we need }
                { to set this flag!                                          }
                Client.AnswerDelayed := TRUE;
                exit;                                                { angus V1.54 }
            end;
        end;
        Client.LastTick := IcsGetTickCountX;                         { angus V1.54 }
        if Md5Sum = '' then                                          { angus V1.54 }
             Answer := Format(msgMd5Failed, [Params])
        else begin
            if Client.CurCmdType = ftpcXMD5 then
                Answer := Format(msgCrcOk , [Uppercase (Md5Sum)])
            else
                Answer := Format(msgMd5Ok, [Params, Uppercase (Md5Sum)]);
        end;
    except
        on E:Exception do begin
            Answer := Format(msgMd5Failed, [E.Message]);
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandXCRC (  { angus V1.54 }
     Client      : TFtpCtrlSocket;
     var Keyword : TFtpString;
     var Params  : TFtpString;
     var Answer  : TFtpString);
var
    FileName  : TFtpString;
    Crc32b    : TFtpString;
    Allowed   : Boolean;
    FileSize  : TFtpBigInt;
    Offset    : Integer;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcXCRC;
    Crc32b  := '';

    try
      { get file name and optional start and end arguments }
        Offset := 1;
        FileName := ScanGetNextArg(Params, Offset);             { keep file name }
        Client.HashStartPos := atoi64(ScanGetNextArg (Params, Offset));  { start position, if any }
        Client.HashEndPos := atoi64(ScanGetNextArg (Params, Offset));    { end position, if any }
        if (Client.HashStartPos > 0) and (Client.HashEndPos = 0) then begin
            Client.HashEndPos := Client.HashStartPos;  { single argument is end position }
            Client.HashStartPos := 0;
        end ;
        FileName := BuildFilePath(Client, Client.Directory, FileName);
        Allowed := IsPathAllowed(Client, FileName);
        { Ideally the CRC sum is being retrieved from a cache or file so it's not  }
        { done repeatedly, if left blank we'll do it here. CRC may be used to check}
        { uploaded/downloaded files, so keep a timestamp with the sum.             }
        TriggerCalculateCrc(Client, FileName, Crc32b, Allowed);
        if not Allowed then begin
             Answer := msgRetrDisabled;
             Exit;
        end;
        if Crc32b = '' then begin
            FileSize := IcsGetFileSize(FileName);
            if FileSize = -1 then begin
                TriggerCrcCalculated(Client, FileName, Crc32b);
                Answer := Format(msgMd5NotFound, [Params]);
                Exit;
            end ;
            { Calculate a 32-byte CRC sum. If file size is small we may use }
            { a blocking function.                                          }
            if (FMd5UseThreadFileSize = 0) or
                                   (FileSize < FMd5UseThreadFileSize) then begin
                Crc32b := FtpFileCRC32B(FileName, Client, FileMD5OnProgress,
                        Client.HashStartPos, Client.HashEndPos, Client.FileModeRead); { angus V1.57 V7.07 }
                TriggerCrcCalculated(Client, FileName, UpperCase(Crc32b));
            end
            else begin
                { Use a thread to calculate CRC checksum which otherwise }
                { would block the server.                        }
                if Client.ProcessingThread <> nil then begin
                    Answer := Format(msgCrcFailed, [Params]);
                    Exit;
                end ;
                TriggerDisplay(Client, 'Using thread to calculate CRC32B');
                Client.ProcessingThread := TClientProcessingThread.Create(TRUE);
                Client.ProcessingThread.Client := Client;
                Client.ProcessingThread.InData := FileName;
                Client.ProcessingThread.Params := Params;
                Client.ProcessingThread.Keyword := Keyword;
                Client.ProcessingThread.OnTerminate := ClientProcessingThreadTerminate;
                Client.ProcessingThread.FreeOnTerminate := TRUE;
            {$IFDEF COMPILER14_UP}
                Client.ProcessingThread.Start;
            {$ELSE}
                Client.ProcessingThread.Resume;
            {$ENDIF}
                { Since answer is sent later when the thread returns we need }
                { to set this flag!                                          }
                Client.AnswerDelayed := TRUE;
                exit;
            end;
        end;
        Client.LastTick := IcsGetTickCountX;
        if Crc32b = '' then
             Answer := Format(msgCrcFailed, [Params])
        else
             Answer := Format(msgCrcOk , [Uppercase (Crc32b)]);
    except
        on E:Exception do begin
            Answer := Format(msgMd5Failed, [E.Message]);
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandALLO (  { angus V1.54 short for allocation }
     Client      : TFtpCtrlSocket;
     var Keyword : TFtpString;
     var Params  : TFtpString;
     var Answer  : TFtpString);
var
    Size, FreeSpace : Int64;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcALLO;
    Answer := '';
  { may need to check client account for disk space allocation remaining }
    TriggerValidateAllo (Client, Params, Answer);
    if Answer <> '' then exit;

  { otherwise check for free space on drive with working directory }
    try
        Size := atoi64(Params);
        FreeSpace := GetFreeSpacePath (BuildFilePath(Client, Client.Directory, '')); { angus V7.08 support virtual path }
        if FreeSpace < 0 then
           Answer := Format(msgAlloOk, [0])   { failed, but pretend Ok for backward compatibility }
        else if (Size = 0) then
            Answer := msgAlloFail             { invalid size }
        else begin
            if (Size + FAlloExtraSpace) < FreeSpace then  { don't allow files to fill drive }
                Answer := Format(msgAlloOk, [FreeSpace])
            else
                Answer := Format(msgAlloFull, [FreeSpace]);
        end;
    except
        on E:Exception do begin
            Answer := msgAlloFail;
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandCLNT (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcCLNT;
    Client.ClntStr := Params;
    Answer := msgNotedOK;
    TriggerClntStr (Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandOPTS (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
var
    Arg: string;
    Offset: Integer;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Answer := Format(msgOptsFailed, [Params]);
    Params := Uppercase (Params);
    Offset := 1;
    Arg := ScanGetNextArg (Params, Offset);
    if Arg = 'MODE' then begin
        Arg := ScanGetNextArg (Params, Offset);
        if Arg <> 'Z' then exit;
        Arg := ScanGetNextArg (Params, Offset);
        if Arg <> 'LEVEL' then exit;
        Arg := ScanGetNextArg (Params, Offset);
        if Arg = '' then exit;
        Offset := atoi (Arg);
        if (Offset >= FZlibMinLevel) and (Offset <= FZlibMaxLevel) then begin
            Client.ZReqLevel := Offset;
            Answer := Format(msgOtpsOK, ['MODE Z LEVEL set to ' + Arg]);
        end;
    end
    else if ((Arg = 'UTF8') or (Arg = 'UTF-8')) then begin       { angus V7.01 }
        if NOT (ftpsEnableUtf8 in FOptions) then begin
            Answer := Format(msgOptsFailed, ['UTF8 not supported']);
            exit;
        end ;
        Arg := ScanGetNextArg (Params, Offset);
        if Arg = 'ON' then begin
            Client.Options := Client.Options + [ftpUtf8On];
            Answer := Format(msgOtpsOK, ['UTF8 ON']);
        end
        else if Arg = 'OFF' then begin
            Client.Options := Client.Options - [ftpUtf8On];
            Answer := Format(msgOtpsOK, ['UTF8 OFF']);
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSitePaswd (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSitePaswd;
    Answer := msgSiteFailed;
    TriggerSitePaswd (Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSiteExec (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSiteExec;
    Answer := msgSiteFailed;
    TriggerSiteExec (Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSiteIndex (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSiteIndex;
    CommandDirectory2(Client, Keyword, Params, Answer, ListTypeName);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSiteZone (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
var
    mins: integer;
    S: string ;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSiteZone;
    mins := GetLocalBiasUTC;
    S := IntToStr (mins);
    if mins >= 0 then S := '+' + S;
    Answer := Format(msgSiteZone, [S])
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSiteMsg (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSiteMsg;
    Answer := msgSiteFailed;
    TriggerSiteMsg (Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSiteCmlsd (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSiteCmlsd;
    CommandDirectory2(Client, KeyWord, Params, Answer, ListTypeFacts);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSiteDmlsd (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSiteDmlsd;
    CommandDirectory2(Client, KeyWord, Params, Answer, ListTypeFacts);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandComb (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcComb;
    Answer := Format(msgCmdUnknown, ['COMB']);
    TriggerCombine (Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandXCmlsd (  { angus V7.01 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcXCMLSD;
    CommandDirectory2(Client, KeyWord, Params, Answer, ListTypeFacts);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandXDmlsd (  { angus V7.01 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcXDMLSD;
    CommandDirectory2(Client, KeyWord, Params, Answer, ListTypeFacts);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandHost (  { angus V7.01 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
var
    Allowed: boolean;
begin
    if Client.FtpState <> ftpcWaitingUserCode then begin  // host is only allowed before logon
        Answer := msgHostTooLate;
        Exit;
    end;
{ expecting HOST ftp.domain.com or HOST [123.123.123.123]   }
    Params := Trim (LowerCase (Params));
    Format(msgHostSyntax, [Params]);
    if Length (Params) <= 2 then exit; // host is only allowed before logon
    if Params [1] = '[' then begin
        if Params [Length (Params)] <> ']' then exit ;
    end;
    Client.CurCmdType := ftpcHost;
    Allowed := false;
    TriggerHost(Client, Params, Allowed);
    if not Allowed then begin
        Answer := msgHostUnknown;   { could be msgHostUnavail }
        Exit;
    end;
    Client.Host := Params;
    Answer := msgHostOK;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandRein (  { angus V7.01 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
var
    Allowed: boolean;
begin
    Client.CurCmdType := ftpcRein;
    Allowed := false;
    TriggerRein(Client, Allowed);
    if not Allowed then begin
        Answer := msgReinUnavail;
        Exit;
    end;

{ reinialise session, as if not yet logged on }
    Client.FtpState := ftpcWaitingUserCode;
// angus pending, more stuff from ServerClientConnect
    Client.Host := '';
    Client.Lang := '' ;
    if (ftpsEnableUtf8 in FOptions) and (ftpsDefaultUtf8On in FOptions) then
        Client.Options := Client.Options + [ftpUtf8On]
    else
        Client.Options := Client.Options - [ftpUtf8On];
    Answer := msgReinOK;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandLang (  { angus V7.01 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
var
    Allowed: boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Params := Trim (UpperCase (Params));
    Client.CurCmdType := ftpcLang;
    Allowed := (Pos (Params, FLanguage) > 0) OR (Length (Params) = 0);
    TriggerLang(Client, Params, Allowed);
    if not Allowed then begin
        Answer := Format(msgLangUnknown, [Params]);
        Exit;
    end;
    if Length (Params) = 0 then Params := FLanguage;
    Client.Lang := Params;
    Answer := Format(msgLangOK, [Params]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandEPRT(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    I       : Integer;
    N       : LongInt;
    Delim   : Char;
    Proto   : Integer;
    AFamily : TSocketFamily;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    if Client.FEpsvAllArgReceived then begin
        Answer := Format(msgEPSVALLDeny, [Keyword]);
        Exit;
    end;

    try
        Client.CurCmdType := ftpcEPRT;
        Proto             := 0;
        Client.DataAddr   := '';
        Client.DataPort   := '';

        if Length(Params) > 0 then
        begin
            Delim := Params[1];
            if not (Ord(Delim) in [33..126]) then
            begin
                Answer := msgSyntaxParam;
                Exit;
            end;
            N := 2;
            { Get proto }
            for I := N to Length(Params) do
            begin
                if Params[I] = Delim then
                begin
                    Proto := atoi(Copy(Params, N, (I - N)));
                    N := I + 1;
                    Break;
                end;
            end;
            { Check proto }
            if not(((Proto = 1) and (Client.CurrentSocketFamily = sfIPv4)) or
                   ((Proto = 2) and (Client.CurrentSocketFamily = sfIPv6))) then
            begin
                if Client.CurrentSocketFamily = sfIPv6 then
                    Answer := Format(msgInvalidProto, ['2'])
                else
                    Answer := Format(msgInvalidProto, ['1']);
                Exit;
            end;

            { Get address }
            for I := N to Length(Params) do
            begin
                if Params[I] = Delim then
                begin
                    Client.DataAddr := Copy(Params, N, (I - N));
                    N := I + 1;
                    Break;
                end;
            end;
            { Check address }
            if (not WSocketIsIP(Client.DataAddr, AFamily)) then
            begin
                Answer := msgSyntaxParam;
                Exit;
            end
            else if ((Proto = 1) and (AFamily <> sfIPv4)) or
               ((Proto = 2) and (AFamily <> sfIPv6)) then
            begin
                Answer := msgSyntaxParam;
                Exit;
            end;
            {Get port }
            for I := N to Length(Params) do
            begin
                if Params[I] = Delim then
                begin
                    Client.DataPort := Copy(Params, N, (I - N));
                    Break;
                end;
            end;
            { Check port }
            N := atoi(Client.DataPort);
            if (N < 1) or (N > 65535) then
                Answer := msgSyntaxParam
            else
                Answer := msgPortSuccess;

            { Remove a possible scope ID. It is IMO a bug on the client  }
            { side. Filezilla sends it with link local addresses.        }
            { Or should we return a syntax error?                        }
            if (Proto = 2) then
            begin
                N := Pos('%', Client.DataAddr);
                if N > 0 then
                    Client.DataAddr := Copy(Client.DataAddr, 1, N - 1);
            end;
        end
        else begin
            Answer := msgSyntaxParam;
        end;
    except
        Answer := msgPortFailed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandEPSV(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    saddr     : TSockAddrIn6;
    saddrlen  : Integer;
    DataPort  : Integer;
    Proto     : Integer;
    IPAddr    : TInAddr6;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    try
        Proto := 0;
        if Params = 'ALL' then
            Client.FEpsvAllArgReceived := TRUE
        else if (Params <> '') then
        begin
            Proto := atoi(Params);
            if Proto = 0 then
            begin
                Answer := msgSyntaxParam;
                Exit;
            end;
            if not(((Proto = 1) and (Client.CurrentSocketFamily = sfIPv4)) or
                   ((Proto = 2) and (Client.CurrentSocketFamily = sfIPv6))) then
            begin
                if Client.CurrentSocketFamily = sfIPv6 then
                    Answer := Format(msgInvalidProto, ['2'])
                else
                    Answer := Format(msgInvalidProto, ['1']);
                Exit;
            end;
        end;

        { Get our IP address from our control socket }
        saddrlen := SizeOf(saddr);
        Client.GetSockName(PSockAddr(@saddr)^, saddrlen);
        IPAddr   := saddr.sin6_addr;

        if Client.PassiveMode then
            FreeCurrentPasvPort(Client);

        Client.DataSocket.Close;
        if Proto = 0 then
        begin
            if saddr.sin6_family = AF_INET6 then
                Client.DataSocket.Addr  := ICS_ANY_HOST_V6
            else
                Client.DataSocket.Addr  := ICS_ANY_HOST_V4;
        end
        else begin
            if Proto = 1 then
                Client.DataSocket.Addr  := ICS_ANY_HOST_V4
            else
                Client.DataSocket.Addr  := ICS_ANY_HOST_V6;
        end;

        Client.DataSocket.Port  := GetNextAvailablePasvPort; { '0';          Any port  }
        if Client.DataSocket.Port = '' then
            raise Exception.Create('No available PASV Ports');

        Client.DataSocket.Proto := 'tcp';
        Client.DataSocket.OnSessionAvailable := ClientPassiveSessionAvailable;
        Client.DataSocket.OnSessionConnected := nil;
        Client.DataSocket.OnSessionClosed    := nil;
        Client.DataSocket.OnDataAvailable    := nil;
        Client.DataSocket.ComponentOptions   := [wsoNoReceiveLoop];
        Client.DataSocket.Listen;

        { Get the port assigned by winsock }
        saddrlen := SizeOf(saddr);
        Client.DataSocket.GetSockName(PSockAddr(@saddr)^, saddrlen);
        DataPort := WSocket_ntohs(saddr.sin6_port);
        Answer := Format(msgEPSVOk, [DataPort]);
        (*
        if Client.sin.sin_addr.s_addr = WSocket_htonl($7F000001) then
            Answer := Format(msgPasvLocal,
                          [HiByte(DataPort),
                           LoByte(DataPort)])
        else begin
            APasvIp := FPasvIpAddr;
            SetPasvIp := (APasvIp <> '') and (not
                         (((ftpsNoPasvIpAddrInLan in FOptions) and
                           IsIpPrivate(Client.PeerSAddr.sin_addr)) or
                          ((ftpsNoPasvIpAddrSameSubnet in FOptions) and
                           WSocket2IsAddrInSubNet(Client.PeerSAddr.sin_addr))));

            if Assigned(FOnPasvIpAddr) then begin
                FOnPasvIpAddr(Self, Client, APasvIp, SetPasvIp);
                SetPasvIp := SetPasvIp and (APasvIp <> '');
            end;

            if not SetPasvIp then
                Answer := Format(msgPasvRemote,
                          [ord(IPAddr.S_un_b.s_b1),
                           ord(IPAddr.S_un_b.s_b2),
                           ord(IPAddr.S_un_b.s_b3),
                           ord(IPAddr.S_un_b.s_b4),
                           HiByte(DataPort),
                           LoByte(DataPort)])
            else begin
                PASVAddr.S_addr := WSocket_inet_addr(AnsiString(APasvIp));
                if (PASVAddr.S_addr = u_long(INADDR_NONE)) or
                            (PASVAddr.S_addr = 0) then { angus v1.53 0.0.0.0 not allowed }
                        raise Exception.Create('Invalid PASV IP Address')
                else
                        Answer := Format(msgPasvRemote,
                              [ord(PASVAddr.S_un_b.s_b1),
                               ord(PASVAddr.S_un_b.s_b2),
                               ord(PASVAddr.S_un_b.s_b3),
                               ord(PASVAddr.S_un_b.s_b4),
                               HiByte(DataPort),
                               LoByte(DataPort)]);
            end;
        end;
        *)
        Client.PassiveMode      := TRUE;
        Client.PassiveStart     := FALSE;
        Client.PassiveConnected := FALSE;
    except
        on E:Exception do begin
            Answer := Format(msgPasvExcept, [E.Message]);
            try
                Client.DataSocket.Close;
            except
                { Ignore any exception here }
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF NOFORMS}
{ This function is a callback function. It means that it is called by       }
{ windows. This is the very low level message handler procedure setup to    }
{ handle the message sent by windows (winsock) to handle messages.          }
function FtpSrvWindowProc(
    ahWnd   : HWND;
    auMsg   : Integer;
    awParam : WPARAM;
    alParam : LPARAM): Integer; stdcall;
var
    Obj    : TObject;
    MsgRec : TMessage;
begin
    { At window creation asked windows to store a pointer to our object     }
    Obj := TObject(GetWindowLong(ahWnd, 0));

    { If the pointer doesn't represent a Tftpserver, just call the default procedure}
    if not (Obj is Tftpserver) then
        Result := DefWindowProc(ahWnd, auMsg, awParam, alParam)
    else begin
        { Delphi use a TMessage type to pass parameter to his own kind of   }
        { windows procedure. So we are doing the same...                    }
        MsgRec.Msg    := auMsg;
        MsgRec.wParam := awParam;
        MsgRec.lParam := alParam;
        { May be a try/except around next line is needed. Not sure ! }
        TFtpServer(Obj).WndProc(MsgRec);
        Result := MsgRec.Result;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.GetMultiListenIndex: Integer;
begin
  if Assigned(FSocketServer) then
        Result := FSocketServer.MultiListenIndex
    else
        Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.GetMultiListenSockets: TWSocketMultiListenCollection;
begin
    if Assigned(FSocketServer) then
        Result := FSocketServer.MultiListenSockets
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SetMultiListenSockets(
  const Value: TWSocketMultiListenCollection);
begin
    if Assigned(FSocketServer) then
        FSocketServer.MultiListenSockets := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_DEBUG_LOG}
function TFTPServer.GetIcsLogger: TIcsLogger;                         { V1.46 }
begin
    Result := FSocketServer.IcsLogger;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFTPServer.SetIcsLogger(const Value: TIcsLogger);           { V1.46 }
begin
    FSocketServer.IcsLogger := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFTPServer.CheckLogOptions(const LogOption: TLogOption): Boolean; { V1.46 }
begin
    Result := Assigned(IcsLogger) and (LogOption in IcsLogger.LogOptions);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFTPServer.DebugLog(LogOption: TLogOption; const Msg: string);  { V1.46 }
begin
    if Assigned(IcsLogger) then
        IcsLogger.DoDebugLog(Self, LogOption, Msg);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SetOnBgException(const Value: TIcsBgExceptionEvent); { V7.15 }
begin
    if Assigned(FSocketServer) then
        FSocketServer.OnBgException := Value;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientProcessingThreadTerminate(Sender: TObject); { AG V1.50 }
var
    Answer    : TFtpString;
    AThread   : TClientProcessingThread;
    Params    : TFtpString;
    Data      : TWSocket;
begin
    AThread := TClientProcessingThread(Sender);
    if IsClient(AThread.Client) and
       (AThread.Client.ID = AThread.ClientID) then begin
        AThread.Client.ProcessingThread := nil;
        if AThread.Client.State <> wsConnected then
            Exit;

        AThread.Client.LastTick := IcsGetTickCountX;                          { angus V1.54 }
        if (AThread.Keyword = 'MD5') or (AThread.Keyword = 'XMD5') then begin { angus V1.54 }
            if AThread.OutData = '' then
                Answer := Format(msgMd5Failed, [AThread.Params])
            else begin
                if (AThread.Keyword = 'XMD5') then                            { angus V1.54 }
                    Answer := Format(msgCrcOk, [Uppercase (AThread.OutData)])
                else
                    Answer := Format(msgMd5Ok, [AThread.Params,
                                                Uppercase (AThread.OutData)]);
            end;
            if Assigned(FOnMd5Calculated) then
                FOnMd5Calculated(Self, AThread.Client,
                                 AThread.InData, UpperCase(AThread.OutData));
        end
        else if (AThread.Keyword = 'XCRC') then begin { angus V1.54 }
            if AThread.OutData = '' then
                Answer := Format(msgCrcFailed, [AThread.Params])
            else
                Answer := Format(msgCrcOk, [Uppercase (AThread.OutData)]);
            if Assigned(FOnCrcCalculated) then
                FOnCrcCalculated(Self, AThread.Client,
                                 AThread.InData, UpperCase(AThread.OutData));
        end
        else if (AThread.Keyword = 'DIRECTORY') then begin { angus V1.54 }
            with AThread.Client do begin
                Params := AThread.InData;
                try
                    if AThread.AuxData <> '' then                           { AG V8.03 }
                        TriggerDisplay(AThread.Client, AThread.AuxData);    { AG V8.03 }
                    TriggerAlterDirectory(AThread.Client, Params,
                                            (DirListType <> ListTypeName));
                    DataStream.Seek(0, 0);
                    FilePath := '';
                    if AThread.OutData <> AThread.Keyword then
                    begin
                        Answer := Format(msgDirFailed, ['Thread Processing Error']);
                        CloseFileStreams(AThread.Client);      { angus V1.54 }
                    end
                 { see if returning listing on control socket instead of data socket }
                    else if CurCmdType in [ftpcSiteIndex, ftpcSiteCmlsd, ftpcXCMLSD] then begin   { angus 7.01 }
                        DataStreamReadString(String(Answer), DataStream.Size, CurrentCodePage); { AG 7.02 }
                        if CurCmdType = ftpcSiteIndex then
                             Answer := Format (msgIndexFollows, [Params]) +
                                                             #13#10 + Answer + msgIndexDone
                        else if CurCmdType in [ftpcSiteCmlsd, ftpcXCMLSD] then   { angus 7.01 }
                             Answer := msgMlstFollows + #13#10 + Answer + msgMlstFollowDone;
                        CloseFileStreams(AThread.Client);      { angus V1.54 }
                    end
                    else
                    begin
                        Answer := msgDirOpen;
                        AThread.Client.AnswerDelayed := FALSE;
                        DoStartSendData(AThread.Client, Answer);   { angus V1.54 added Answer }
                        if AThread.Client.AnswerDelayed then Exit; { about to compress stream }
                    end;
                except
                    on E:Exception do begin
                        Answer := Format(msgDirFailed, [E.Message])
                    end;
                end;

             { check for success 150..159 in passive mode }
                if (HasOpenedFile) and (PassiveMode) and
                                                    (Copy(Answer, 1, 2) <> '15') then begin
                    { flag for ClientRetrSessionClosed that the error-message was already sent! }
                    TransferError    := '';
                    AbortingTransfer := TRUE;
                    { set up Passive DataSocket.EventHandlers        }
                    { otherwise FreeCurrentPasvPort won't be called! }
                    PreparePassiveRetrDataSocket(AThread.Client);
                end;
            end;
        end
     { angus V1.54 }
        else if (AThread.Keyword = 'COMPRESS') then begin
             if AThread.OutData = '' then begin
                TriggerDisplay(AThread.Client, AThread.Client.FilePath + ' took ' +
                         IntToStr(IcsElapsedMsecs(AThread.StartTick)) + 'ms,' +
                                                         AThread.Client.ZCompInfo);
                if AThread.Client.ZCompFileDelete then
                                    TriggerUpCompressedFile(AThread.Client);
                Answer := AThread.InData;
                AThread.Client.AnswerDelayed := FALSE;
                PostMessage(Handle, FMsg_WM_FTPSRV_START_SEND, 0,
                                                     LPARAM(AThread.Client));
            end
            else begin  { compress failed }
                CloseFileStreams(AThread.Client);
                Answer := AThread.OutData;
            end;
        end
        else if (AThread.Keyword = 'DECOMPRESS') then begin
            if AThread.OutData = '' then begin
                TriggerDisplay(AThread.Client, AThread.Client.FilePath + ' took ' +
                    IntToStr(IcsElapsedMsecs(AThread.StartTick)) + 'ms,' +
                                                         AThread.Client.ZCompInfo);
                Answer := AThread.InData + AThread.Client.ZCompInfo;
                CloseFileStreams(AThread.Client);
                Data := TWSocket(AThread.Sender);
                TriggerStorSessionClosed(AThread.Client, Data, 0);
            end
            else begin  { decompress failed }
                CloseFileStreams(AThread.Client);
                Answer := AThread.OutData;
            end;
        end
        else
            Answer := Format('500 Executing command %s failed', [AThread.Keyword]);
        AThread.Client.AnswerDelayed := FALSE;  { angus V1.54 }
        SendAnswer(AThread.Client, Answer);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.EventTimerOnTimer (Sender : TObject); { angus V1.54 }
var
    Client   : TFtpCtrlSocket;
    I        : integer;
    Timeout  : integer;
    Duration : integer;
    Abort    : boolean ;
    CurTicks : LongWord;
begin
    FEventTimer.Enabled := false;
    try
        if FSocketServer.ClientCount = 0 then exit;                              { no clients }
        CurTicks := IcsGetTickCountX; {  V1.56 AG }
        for I := 0 to Pred (FSocketServer.ClientCount) do begin
            Client := FSocketServer.Client[I] as TFtpCtrlSocket;
            if Client.FSessionClosedFlag then Continue;  { Client will close soon AG }

          { V7.06 angus - failed authentication, send delayed answer to slow to
              failed login attempts, closing connection after MaxAttempts }
            if Client.FtpState = ftpcFailedAuth then begin
                if IcsTestTrgTick (Client.DelayAnswerTick) then begin
                    Client.DelayAnswerTick := TriggerDisabled;
                    Client.FtpState := ftpcWaitingUserCode;
                    if Client.FailedAttempts >= FMaxAttempts then begin
                        SendAnswer(Client, msgNotAllowed);
                     { close control channel }
                        Client.Close;
                    end
                    else begin
                        SendAnswer(Client, msgLoginFailed);
                    end;
                    continue ; // skip testing timeouts
                end;
            end ;

         { different length timeouts depending on what's happening }
            Timeout := 0;
            case Client.FtpState of
                ftpcWaitingUserCode, ftpcWaitingPassword: Timeout := FTimeoutSecsLogin;
                ftpcReady, ftpcWaitingAnswer: Timeout := FTimeoutSecsIdle;
            end;
            if Client.DataSocket.State = wsConnected then begin
                if FTimeoutSecsXfer < FTimeoutSecsIdle then Timeout := FTimeoutSecsXfer;  { V7.09 xfer timeout must be shorted than idle }
            end;
            if Timeout > 0 then begin
                Duration :=  IcsDiffTicks(Client.LastTick, CurTicks) div TicksPerSecond; { V1.56 AG}
                if Duration >= Timeout then begin   { seconds }
                    Abort := true;
                    TriggerTimeout(Client, Duration, Abort);
                    if NOT Abort then
                        Client.LastTick := IcsGetTickCountX  { extend timeout }
                    else begin
                      { close data channel }
                        if Client.DataSocket.State = wsConnected then begin
                            Client.TransferError    := 'ABORT on Timeout';
                            Client.AbortingTransfer := TRUE;
                            Client.DataSocket.Close;
                        end
                        else begin  { V7.09 xfer timeout only close data channel }
                            SendAnswer(Client, WideFormat(msgTimeout, [Duration]));
                          { close control channel }
                            Client.Close;
                        end;
                    end;
                end;
            end;
        end;
    finally
        FEventTimer.Enabled := true;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TFtpCtrlSocket.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FDataSocket      := TWSocket.Create(Self);
{$IFDEF USE_SSL}
    ProtP            := FALSE;
    AuthFlag         := FALSE;
    CccFlag          := FALSE;
    CurFtpSslType    := curftpSslNone;
    FtpSslTypes      := [];
{$ENDIF}
    FDataSocket.Name := 'DataWSocket';
    FBanner          := DefaultBanner;
    FFtpState        := ftpcInvalid;
    FHomeDir         := 'C:\TEMP\';  { Must include a trailing backslash !!}
    FDirectory       := FHomeDir;    { Must include a trailing backslash !!}
    SetRcvSize(DefaultRcvSize);
    OtpMethod        := OtpKeyNone;  { angus V1.54 One Time Password authentication method }
    OtpSequence      := -1;          { angus V1.54 One Time Password current sequence }
    OtpSeed          := '';          { angus V1.54 One Time Password current seed }
    LastTick         := IcsGetTickCountX;  { angus V1.54 last tick for time out checking }
    SessStartTick    := IcsGetTickCountX;     { angus V1.54 tick when client session started, for duration check }
    ReqStartTick     := 0;    { angus V1.54 tick when last request started, for duration check }
    ReqDurMilliSecs  := 0;    { angus V1.54 how long last request took, in ticks }
    TotGetBytes      := 0;    { angus V1.54 how many bytes GET during session, data and control }
    TotPutBytes      := 0;    { angus V1.54 how many bytes PUT during session, data and control }
    FailedAttempts   := 0;    { angus V7.06 count failed login attempts }
    DelayAnswerTick  := TriggerDisabled;  { angus V7.06 when to send a delayed failed login answer }
    FSndBufSize      := DefaultRcvSize;   { Angus V7.19 datasocket buffer}
    FRcvBufSize      := DefaultRcvSize;   { Angus V7.19 datasocket buffer}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TFtpCtrlSocket.Destroy;
begin
    FRcvCnt := 0;      { Clear received data }
    SetRcvSize(0);     { Free the buffer     }
    if Assigned(ProcessingThread) then begin { AG V7.02 }
        ProcessingThread.OnTerminate := nil; { AG V7.02 }
        FreeAndNil(ProcessingThread);        { AG V7.02 }
    end;                                     { AG V7.02 }
    if Assigned(FDataSocket) then begin
        FDataSocket.Destroy;
        FDataSocket := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SetRcvSize(newValue : Integer);
begin
    if FRcvCnt <> 0 then
        raise EFtpCtrlSocketException.Create('Data in buffer, can''t change size');

    if FRcvSize < 0 then
        FRcvSize := 0;

    if FRcvSize = newValue then
        Exit; { No change, nothing to do }

    { Free previously allocated buffer }
    if FRcvBuf <> nil then begin
        FreeMem(FRcvBuf, FRcvSize);
        FRcvBuf := nil;
    end;

    { Allocate new buffer }
    FRcvSize := newValue;

    { If size is nul, then do not allocated the buffer }
    if newValue > 0 then
        GetMem(FRcvBuf, FRcvSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SetRcvBufSize(newValue : Integer);   { Angus V7.19}
begin
    if newValue < 1024 then
        FRcvBufSize := 1024
    else
        FRcvBufSize := newValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SetSndBufSize(newValue : Integer);   { Angus V7.19}
begin
    if newValue < 1024 then
        FSndBufSize := 1024
    else
        FSndBufSize := newValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SetCodePage(const Value: LongWord);{ AG 7.02 }
begin
    if Value = FtpServer.FSystemCodePage then
        FCodePage := CP_ACP
    else
        FCodePage := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SetCurrentCodePage(const Value: LongWord);{ AG 7.02 }
begin
    if Value = FtpServer.FSystemCodePage then
        FCurrentCodePage := CP_ACP
    else
    {$IFDEF COMPILER12_UP}
        FCurrentCodePage := Value;
    {$ELSE}
        if Value = CP_UTF8 then
            FCurrentCodePage := Value
        else
            FCurrentCodePage := CP_ACP;
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SetOnBgException(const Value: TIcsBgExceptionEvent); { V7.15 }
begin
    if Assigned(FDataSocket) then
        FDataSocket.OnBgException := Value;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SetOptions(const Opts : TFtpOptions);{ AG 7.02 }
begin
    FOptions := Opts;
    if ftpUtf8On in FOptions then
        CurrentCodePage := CP_UTF8
    else
        CurrentCodePage := FCodePage;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.TriggerSessionClosed(Error: Word);
begin
    if Assigned(ProcessingThread) then
        ProcessingThread.Terminate;
    inherited TriggerSessionClosed(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.TriggerSessionConnected(Error : Word);
begin
    FPeerAddr := inherited GetPeerAddr;
    inherited TriggerSessionConnected(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.TriggerCommand(CmdBuf : PAnsiChar; CmdLen : Integer);{ AG 7.02 }
begin
    if Assigned(FOnCommand) then
        FOnCommand(Self as TFtpCtrlSocket, CmdBuf, CmdLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpCtrlSocket.TriggerDataAvailable(Error : Word) : Boolean;
var
    Len  : Integer;
    I    : Integer;
begin
    Result := TRUE;                                { We read data }

    Len := Receive(@FRcvBuf[FRcvCnt], FRcvSize - FRcvCnt - 1);
    if Len <= 0 then
        Exit;

    FRcvCnt := FRcvCnt + Len;
    FRcvBuf[FRcvCnt] := #0;
    LastTick := IcsGetTickCountX;  { angus V1.54 last tick for time out checking }
    TotPutBytes := TotPutBytes + Len;    { angus V1.54 }

    while TRUE do begin
        I := 0;
        while (I < FRcvCnt) and (FRcvBuf[I] <> #10) do
            Inc(I);
        if I >= FRcvCnt then begin
            { Check line overflow. }
            if FRcvCnt >= (FRcvSize - 1) then begin
                StrPCopy(FRcvBuf, 'OVER' + #13#10);
                FRcvCnt := StrLen(FRcvBuf);
                I       := FRcvCnt - 1;
            end
            else
                Exit;
        end;
        FRcvBuf[I]   := #0;
        FLastCommand := Now;
        Inc(FCommandCount);
        if (I > 1) and (FRcvBuf[I - 1] = #13) then begin
            FRcvBuf[I - 1] := #0;
            TriggerCommand(FRcvBuf, I - 1);{ AG 7.02 }
            FRcvBuf[I - 1] := #13;
        end
        else
            TriggerCommand(FRcvBuf, I);{ AG 7.02 }

        FRcvBuf[I] := #10;
        if I >= (FRcvCnt - 1) then begin
            FRcvCnt    := 0;
            FRcvBuf[0] := #0;
            break;
        end;
        Move(FRcvBuf[I + 1], FRcvBuf^, FRcvCnt - I);
        FRcvCnt := FRcvCnt - I - 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SendAnswer(const Answer : RawByteString);    { angus V7.01  }{ AG 7.02 }
begin
    SendStr(Answer + #13#10);
    LastTick := IcsGetTickCountX;  { angus V1.54 last tick for time out checking }
    TotGetBytes := TotGetBytes + Length (Answer) + 2;    { angus V1.54 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
procedure TFtpCtrlSocket.DataStreamWriteString(
    const Str: UnicodeString;
    DstCodePage: LongWord);{ AG 7.02 }
begin
    StreamWriteString(DataStream, Str, DstCodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.DataStreamWriteString(const Str: UnicodeString);
begin
    StreamWriteString(DataStream, Str, CP_ACP);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.DataStreamWriteString(const Str: AnsiString);
begin
    DataStream.Write(Pointer(Str)^, Length(Str));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.DataStreamWriteString(                     { AG 7.02 }
  const Str: AnsiString; DstCodePage: LongWord);
var
    S : AnsiString;
begin
    if DstCodePage = CP_ACP then
        DataStream.Write(Pointer(Str)^, Length(Str))
    else begin
        S := ConvertCodePage(Str, CP_ACP, DstCodePage);
        DataStream.Write(Pointer(S)^, Length(S));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.DataStreamReadString(var Str: AnsiString;
  Len: TFtpBigInt);
var
    ReadLen: Cardinal;
begin
    SetLength(Str, Len);
    ReadLen := DataStream.Read(Pointer(Str)^, Len);
    if ReadLen < Len then
        SetLength(Str, ReadLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Potential data loss if SrcCodePage <> CP_ACP!                       AG 7.02 }
procedure TFtpCtrlSocket.DataStreamReadString(
  var Str: AnsiString; Len: TFtpBigInt; SrcCodePage: LongWord);
var
    BytesRead : Cardinal;
    Buf       : PAnsiChar;
    BufW      : PWideChar;
    L1, L2    : Integer;
begin
    SetLength(Str, 0);
    if Len < 0 then Exit;
    if (SrcCodePage = CP_ACP) then
    begin
        SetLength(Str, Len);
        BytesRead := DataStream.Read(Pointer(Str)^, Len);
        if BytesRead < Len then
            SetLength(Str, BytesRead);
    end
    else begin
        GetMem(Buf, Len);
        try
            BytesRead := DataStream.Read(Buf^, Len);
            L1 :=  IcsMbToWc{MultiByteToWideChar}(SrcCodePage, 0, Buf, BytesRead, nil, 0);
            GetMem(BufW, L1 * SizeOf(WideChar));
            try
                IcsMbToWc{MultiByteToWideChar}(SrcCodePage, 0, Buf, BytesRead, BufW, L1);
                L2 := IcsWcToMb{WideCharToMultibyte}(CP_ACP, 0, BufW, L1, nil, 0, nil, nil);
                if L2 <> Len then
                    ReallocMem(Buf, L2);
                L1 := IcsWcToMb{WideCharToMultibyte}(CP_ACP, 0, BufW, L1, Buf, L2, nil, nil);
                SetLength(Str, L1);
                Move(Buf[0], Pointer(Str)^, L1);
            finally
                ReallocMem(BufW, 0);
            end;
        finally
            ReallocMem(Buf, 0);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
procedure TFtpCtrlSocket.DataStreamReadString(var Str: UnicodeString;
  Len: TFtpBigInt; SrcCodePage: LongWord); { AG 7.02 }
var
    SBuf : array [0..2047] of AnsiChar;
    HBuf : PAnsiChar;
    eLen : Cardinal;
begin
    if SrcCodePage <> 1200 {CP_UTF16} then begin
        if Len <= SizeOf(SBuf) then begin
            eLen := DataStream.Read(SBuf[0], Len);
            Len := IcsMbToWc{MultiByteToWideChar}(SrcCodePage, 0, @SBuf, eLen, nil, 0);
            SetLength(Str, Len);
            IcsMbToWc{MultiByteToWideChar}(SrcCodePage, 0, @SBuf, eLen, Pointer(Str), Len);
        end
        else begin
            GetMem(HBuf, Len);
            try
                eLen := DataStream.Read(HBuf^, Len);
                Len := IcsMbToWc{MultiByteToWideChar}(SrcCodePage, 0, HBuf, eLen, nil, 0);
                SetLength(Str, Len);
                IcsMbToWc{MultiByteToWideChar}(SrcCodePage, 0, HBuf, eLen, Pointer(Str), Len);
            finally
                FreeMem(HBuf);
            end;
        end;
    end
    else begin
        SetLength(Str, Len);
        eLen := DataStream.Read(Pointer(Str)^, Len * SizeOf(WideChar));
        if (eLen div SizeOf(WideChar)) < Len then
            SetLength(Str, (eLen div SizeOf(WideChar)));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.DataStreamReadString(var Str: UnicodeString;
  Len: TFtpBigInt);
begin
    DataStreamReadString(Str, Len, CP_ACP);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsUNC(S : String) : Boolean;
begin
    Result := (Length(S) >= 2) and (S[2] = '\') and (S[1] = '\');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure PatchIE5(var S : String);
begin
    { \c:\Temp\ -> c:\Temp\ IE5 like this invalid syntax !}
    if (Length(S) >= 3) and (S[3] = ':') and (S[1] = '\') then
        Delete(S, 1, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
procedure TFtpCtrlSocket.SetDirectory(newValue : String);
var
    newDrive : String;
    newPath  : String;
    I        : Integer;
begin
    if FDirectory = newValue then
        Exit;
    PatchIE5(newValue);
    newDrive := ExtractFileDrive(newValue);
    if IsUNC(newDrive) then begin
        if not (ftpcUNC in Options) then
            raise Exception.Create('Cannot accept UNC path');
        FDirectory := newValue;
        { Always terminate with a backslash }
        if (Length(FDirectory) > 0) and (FDirectory[Length(FDirectory)] <> '\') then
            FDirectory := FDirectory + '\';
        Exit;
    end;
    if Length(newDrive) = 0 then begin        { AG V1.52 }
        if (ftpCdUpHome in Options) then begin
            if (Length(newValue) > 0) and (newValue[1] = '\') then begin
                { absolute path, HomeDir }
                newDrive := ExtractFileDrive(FHomeDir);
                newPath  := Copy(FHomeDir, Length(newDrive) + 1, Length(FHomeDir)) +
                                 Copy(newValue, 2, Length(newValue))
            end
            else begin
                newDrive := ExtractFileDrive(FDirectory);
                newPath  := newValue;
            end;
        end
        else begin
          newDrive := ExtractFileDrive(FDirectory);
          newPath  := newValue;
        end;
    end
    else
        newPath := Copy(newValue, 3, Length(newValue));


    if Pos(':', newPath) <> 0 then
        raise Exception.Create('Invalid directory name syntax');

    if newPath = '..' then begin
        if IsUNC(FDirectory) then begin
            I := Length(FDirectory) - 1;
            while (I > 0) and (FDirectory[I] <> '\') do
                Dec(I);
            if I > Length(newDrive) then
                SetLength(FDirectory, I);
            Exit;
        end
        else begin
            newPath := Copy(FDirectory, 3, Length(FDirectory));
            I := Length(newPath) - 1;
            while (I > 0) and (newPath[I] <> '\') do
                Dec(I);
            SetLength(newPath, I);
        end;
    end;

    if (Length(newPath) > 0) and (newPath[1] <> '\') then begin
        { Relative path }
        if IsUNC(FDirectory) then begin
            FDirectory := FDirectory + newPath;
            { Always terminate with a backslash }
            if (Length(FDirectory) > 0) and (FDirectory[Length(FDirectory)] <> '\') then
                FDirectory := FDirectory + '\';
            Exit;
        end
        else begin
            if UpperCase(newDrive[1]) <> UpperCase(FDirectory[1]) then
                raise Exception.Create('Cannot accept path not relative to current directory');
            if Pos('.\', newPath) <> 0 then
                raise Exception.Create('Cannot accept relative path using dot notation');
            if newPath = '.' then
                newPath := Copy(FDirectory, 3, Length(FDirectory))
            else
                newPath := Copy(FDirectory, 3, Length(FDirectory)) + newPath;
        end;
    end
    else begin
        if Pos('.\', newPath) <> 0 then
            raise Exception.Create('Cannot accept relative path using dot notation');
    end;

    if Length(newPath) = 0 then begin
        if UpperCase(newDrive[1]) <> UpperCase(FDirectory[1]) then
            newPath := '\'
        else
            newPath := Copy(FDirectory, 3, Length(FDirectory));
    end;

    { Always terminate with a backslash }
    if (Length(newPath) > 0) and (newPath[Length(newPath)] <> '\') then
        newPath := newPath + '\';

    FDirectory := newDrive + newPath;
end;
{$ENDIF MSWINDOWS}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
procedure TFtpCtrlSocket.SetDirectory(newValue : String);
var
    newPath  : String;
    I        : Integer;
begin
    if FDirectory = newValue then
        Exit;
    PatchIE5(newValue);

    if (newValue = FHomeDir) then begin
        FDirectory := FHomeDir;
        Exit;
    end;

    if (ftpCdUpHome in Options) then begin
        if (Length(newValue) > 0) and (newValue[1] = PathDelim) then begin
            { absolute path, HomeDir }
            newPath  := FHomeDir + Copy(newValue, 2, MaxInt)
        end
        else begin
            newPath  := newValue;
        end;
    end
    else begin
      newPath  := newValue;
    end;

    if Pos(':', newPath) <> 0 then
        raise Exception.Create('Invalid directory name syntax');

    if newPath = '..' then begin
        newPath := FDirectory;
        I := Length(newPath) - 1;
        while (I > 0) and (newPath[I] <> PathDelim) do
            Dec(I);
        SetLength(newPath, I);
    end;

    if (Length(newPath) > 0) and (newPath[1] <> PathDelim) then begin
        { Relative path }
        if Pos('.\', newPath) <> 0 then
            raise Exception.Create('Cannot accept relative path using dot notation');
        if newPath = '.' then
            newPath := FDirectory
        else
            newPath := FDirectory + newPath;

    end
    else begin
        if Pos('.\', newPath) <> 0 then
            raise Exception.Create('Cannot accept relative path using dot notation');
    end;

    if Length(newPath) = 0 then begin
        newPath := FDirectory;
    end;

    { Always terminate with a backslash }
    if (Length(newPath) > 0) and (newPath[Length(newPath)] <> PathDelim) then
        newPath := newPath + PathDelim;

    FDirectory := newPath;
end;
{$ENDIF POSIX}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SetHomeDir(const newValue: String);
begin
    if FHomeDir = newValue then
        Exit;
    if (Length(newValue) > 0) and (newValue[Length(newValue)] <> PathDelim) then
        FHomeDir := newValue + PathDelim
    else
        FHomeDir := newValue;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SetAbortingTransfer(newValue : Boolean);
begin
    FAbortingTransfer := newValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} { AG V1.46}
procedure UpdateThreadOnProgress(
    Obj: TObject;
    Count: Int64;
    var Cancel: Boolean);
begin
    if (Obj is TClientProcessingThread) then   { V7.08 }
    begin
        Cancel := (Obj as TClientProcessingThread).Terminated;
        (Obj as TClientProcessingThread).Client.LastTick := IcsGetTickCountX;
    end
    else if (Obj is TFtpCtrlSocket) then       { V7.08 }
    begin
        Cancel := (Obj as TFtpCtrlSocket).AbortingTransfer;
        (Obj as TFtpCtrlSocket).LastTick := IcsGetTickCountX;
    end
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FormatUnixDirEntry(F : TSearchRec; const FileName: string) : String;
var
    Attr             : String;
    Ext              : String;
    Day, Month, Year : Integer;
    Hour, Min        : Integer;
    SizeStr          : String;
    TimeStr          : String;
  {$IFDEF POSIX}
    UT: tm;
  {$ENDIF}
const
    StrMonth : array [1..12] of String =
        ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
begin
{$WARNINGS OFF}
    { Owner - Group - Others }
    Attr := '-rw-rw-rw-';
    if (F.Attr and faDirectory) <> 0 then
        Attr[1] := 'd';

    if (F.Attr and faReadOnly) <> 0 then begin
        Attr[3] := '-';
        Attr[6] := '-';
        Attr[9] := '-';
    end;
{$WARNINGS ON}

    Ext := UpperCase(ExtractFileExt(FileName));
    if (Ext = '.EXE') or (Ext = '.COM') or (Ext = '.BAT') then begin
        Attr[4]  := 'x';
        Attr[7]  := 'x';
        Attr[10] := 'x';
    end;
{$IFDEF MSWINDOWS}
    Year  := IcsHiWord(F.Time) shr 9 + 1980;
    Month := IcsHiWord(F.Time) shr 5 and $0F;
    Day   := IcsHiWord(F.Time) and $1F;
    Hour  := IcsLoWord(F.Time) shr 11;
    Min   := IcsLoWord(F.Time) shr 5 and $3F;
    (*
    Day   := (IcsHiWord(F.Time) and $1F);
    Month := ((IcsHiWord(F.Time) shr 5) and $0F);
    Year  := ((IcsHiWord(F.Time) shr 9) and $3F) + 1980;
    Min   := ((F.Time shr 5) and $3F);
    Hour  := ((F.Time shr 11) and $1F);
    *)
{$ENDIF}
{$IFDEF POSIX}
    localtime_r(F.Time, UT);
    Year  := UT.tm_year + 1900;
    Month := UT.tm_mon + 1;
    Day   := UT.tm_mday;
    Hour  := UT.tm_hour;
    Min   := UT.tm_min;
{$ENDIF}

  {$IFDEF MSWINDOWS}
    if F.FindData.nFileSizeHigh = 0 then
        SizeStr := IntToStr(F.FindData.nFileSizeLow)
    else
        SizeStr := IntToStr(F.FindData.nFileSizeLow +
                           (Int64(F.FindData.nFileSizeHigh) shl 32));
  {$ENDIF}
  {$IFDEF POSIX}
        SizeStr := IntToStr(F.Size);
  {$ENDIF}
    if Year = ThisYear then
        TimeStr := Format('%2.2d:%2.2d', [Hour, Min])
    else
        TimeStr := Format('%5d', [Year]);

    Result := Attr + '   1 ftp      ftp  ' +
              Format('%11s %s %2.2d %5s ',
                     [SizeStr, StrMonth[Month], Day, TimeStr]) +
              FileName + #13#10;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function FileTimeToStr(const FileTime: TFileTime): String;     { angus V1.38 }
const
  FileTimeBase = -109205.0;   { days between years 1601 and 1900 }
  FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; { 100 nsec per Day }
var
    F64    : Comp absolute FileTime;
    TempDT : TDateTime;
begin
    TempDT := F64 / FileTimeStep;
    TempDT := TempDT + FileTimeBase;
    Result := FormatDateTime (UtcDateMaskPacked, TempDT);
end;
{$ENDIF}
{$IFDEF POSIX}
function FileTimeToStr(const DT: TDateTime): String; // creation time
begin
    Result := FormatDateTime(UtcDateMaskPacked, DT);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{FTP MLSD command, same format for MSLT for a single file
much nice than LIST since it has a proper date with year, and seconds, and is much easier to parse
size=0;type=cdir;perm=fdelcmp;create=20020616151738;modify=20031002125810; .
size=0;type=pdir;perm=fdelcmp;create=20020616151738;modify=20031002125810; ..
size=17199;type=file;perm=fdrwa;create=20030616152030;modify=20031001190100; 00master.zip
size=182928;type=file;perm=fdrwa;create=20030922195127;modify=20030922190600; 12=page-004394.zip
size=134503;type=file;perm=fdrwa;create=20030923181732;modify=20030923170800; 12=page-004399.zip
size=225460;type=file;perm=fdrwa;create=20030923193147;modify=20030923185600; 12=page-004400.zip
size=205011;type=file;perm=fdrwa;create=20030923120836;modify=20030922225700; 12=page-004405.zip
size=191721;type=file;perm=fdrwa;create=20030905141821;modify=20030904181100; 20=page-004320.zip
size=183977;type=file;perm=fdrwa;create=20030905142247;modify=20030904181100; 20=page-004321.zip
size=0;type=dir;perm=fdelcmp;create=20030219123018;modify=20030305153855; errors
size=0;type=dir;perm=fdelcmp;create=20021217151845;modify=20030903193625; new software
size=0;type=dir;perm=fdelcmp;create=20020805160304;modify=20031002133003; sql logs
size=70806;type=file;perm=fdrwa;create=20030718113340;modify=20031001185600; vehinfiles.zip
size=0;type=dir;perm=fdelcmp;create=20020801100314;modify=20031004124403; zip logs  }

function FormatFactsDirEntry(F : TSearchRec; const FileName: string) : String;  { angus V1.38, 1.54 added FileName }
var
    SizeStr : String;
  {$IFDEF POSIX}
    CreateDT, ModifyDT : TDateTime;
  {$ENDIF}
begin
{$WARNINGS OFF}
    (*  faVolumeID not used in Win32
    if ((F.Attr and faVolumeID) <> 0)  then begin
        { Ignore volume ID entries }
        Result := '';
        Exit;
    end;
    *)
  {$IFDEF MSWINDOWS}
    if F.FindData.nFileSizeHigh = 0 then
        SizeStr := IntToStr(F.FindData.nFileSizeLow)
    else
        SizeStr := IntToStr(F.FindData.nFileSizeLow +
                           (Int64(F.FindData.nFileSizeHigh) shl 32));
  {$ENDIF}
  {$IFDEF POSIX}
        SizeStr := IntToStr(F.Size);
  {$ENDIF}
    { PERMissions is advisory only, max 10 characters - not properly set here }
    { a - APPE allowed for a file                                             }
    { c - files may be created in this directory                              }
    { d - may be deleted                                                      }
    { e - directory entry allowed                                             }
    { f - may be renamed                                                      }
    { l - directory may be listed                                             }
    { m - new directories may be made                                         }
    { p - file may be deleted from the directory                              }
    { r - RETR allowed for a file                                             }
    { w - STOR allowed for a file                                             }
    if (F.Attr and faDirectory) <> 0 then begin
        if FileName = '.' then
            result := 'size=0;type=cdir;perm=fdelcmp;'
        else if FileName = '..' then
            result := 'size=0;type=pdir;perm=fdelcmp;'
        else
            result := 'size=0;type=dir;perm=fdelcmp;'
    end
    else begin
        result := 'size=' + SizeStr + ';type=file;perm=';
        if (F.Attr and faReadOnly) <> 0 then
            result := result + 'rw;'
        else
            result := result + 'fdrwa;';
    end;
  {$IFDEF MSWINDOWS}
    result := result +
        'create=' + FileTimeToStr (F.FindData.ftCreationTime) +
        ';modify=' + FileTimeToStr (F.FindData.ftLastWriteTime) +
        '; ' + FileName;    { note space before filename is delimiter }
  {$ENDIF}

  {$IFDEF POSIX}
    if (F.Attr and faDirectory) <> 0 then begin
        CreateDT := TDirectory.GetCreationTimeUtc(F.PathOnly);
        ModifyDT := TDirectory.GetLastWriteTimeUtc(F.PathOnly);
    end
    else begin
        CreateDT := TFile.GetCreationTimeUtc(F.PathOnly + FileName);
        ModifyDT := TFile.GetLastWriteTimeUtc(F.PathOnly + FileName);
    end;
    result := result +
        'create=' + FileTimeToStr(CreateDT) +
        ';modify=' + FileTimeToStr(ModifyDT) +
        '; ' + FileName;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.BuildDirList(var TotalFiles: integer);  { angus V7.08 was BuildDirectory, removed Path, added TotalFiles }
var
    Buf        : String;
    LocFiles   : TIcsFileRecs;  { angus 1.54 dynamic array of File Records }
    LocFileList: TList;         { angus 1.54 sorted pointers to File Records }
    I          : Integer;
    TotFiles   : Integer;
    FileRecX   : PTIcsFileRec;
begin
    TotalFiles := 0 ;  { V7.08 }
{    DecodeDate(Now, ThisYear, ThisMonth, ThisDay);   V7.08 moved to BuildDirectory so this procedure can be overriden }

 { angus 1.54 build sorted recursive directory }
    SetLength (LocFiles, 250);   { initial expected number of files }
    LocFileList := TList.Create;
    try
     { fill LocFiles dynamic array with SearchRecs, sorted by LocFileList }
        TotFiles := IcsGetDirList (DirListPath, DirListSubDir,
                     DirListHidden, LocFiles, LocFileList, Self, UpdateThreadOnProgress) ;  { V7.08 }
     { V7.08 allow extra virtual files to be added to the dynamic File Records array }
        if TotFiles <> -1 then
            FtpServer.TriggerAddVirtFiles(Self, LocFiles, LocFileList, TotFiles, UpdateThreadOnProgress) ;
        if TotFiles > 0 then begin
          { need a descendent of TMemoryStream with SetCapacity }
          {  TMemoryStream (Stream).SetCapacity (TotFiles * 128);  }
            for I := 0 to Pred (TotFiles) do begin
                if LocFileList [I] = Nil then continue ;
                FileRecX := LocFileList [I] ;   { get file record pointer }
                if DirListSubDir then   { add path before file name }
                    Buf := BackSlashesToSlashes(FileRecX^.FrSubDirs) +
                                                 FileRecX^.FrSearchRec.Name
                else
                    Buf := FileRecX^.FrSearchRec.Name;

            { build single line according to listing style }
                if DirListType = ListTypeUnix then
                    Buf := FormatUnixDirEntry(FileRecX^.FrSearchRec, Buf)
                else if DirListType = ListTypeFacts then
                    Buf := FormatFactsDirEntry(FileRecX^.FrSearchRec, Buf) + #13#10
                else
                    Buf := Buf + #13#10;
                if Length(Buf) > 0 then begin
                    if CurCmdType = ftpcSiteIndex then Buf := '200-' + Buf;
                    if CurCmdType in [ftpcSiteCmlsd, ftpcXCMLSD] then
                                                       Buf := '250-' + Buf;    { angus 7.01 }
                    DataStreamWriteString(Buf, CurrentCodePage);
                    inc (TotalFiles) ;   { V7.08 }
                end;
            end;
        end
        else
            TotalFiles := TotFiles;   { V7.08 -1 is an error }
    finally
        SetLength (LocFiles, 0);
        LocFileList.Free;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF USE_SSL}
function TFtpCtrlSocket.SslSendPlain(Data : TWSocketData; Len : Integer) : Integer;
begin
    Result := RealSend(Data, Len);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Get the function pointer which cannot be niled by another thread. AG V7.02  }
procedure TClientProcessingThread.TriggerEnterSecurityContext;
var
    f_EnterSecurityContext : TFtpSecurityContextEvent;
begin
    f_EnterSecurityContext := Client.FtpServer.FOnEnterSecurityContext;
    if Assigned(f_EnterSecurityContext) then
        f_EnterSecurityContext(Client.FtpServer, Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Get the function pointer which cannot be niled by another thread. AG V7.02  }
procedure TClientProcessingThread.TriggerLeaveSecurityContext;
var
    f_LeaveSecurityContext : TFtpSecurityContextEvent;
begin
    f_LeaveSecurityContext := Client.FtpServer.FOnLeaveSecurityContext;
    if Assigned(f_LeaveSecurityContext) then
        f_LeaveSecurityContext(Client.FtpServer, Client);

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Any lengthy task executed here must stop when Terminated is detected.       }
procedure TClientProcessingThread.Execute;                        { AG V1.46}
var
    NewSize: Int64;
    TotalFiles: integer;  { V7.08 }
    Buf: TFtpString;      { V7.08 }
begin
    ClientID := Client.ID;
    try
        with Client.ProcessingThread do begin
            StartTick := IcsGetTickCountX;
            if (Keyword = 'MD5') or (Keyword = 'XMD5')  then    { angus V1.54 }
                OutData := FtpFileMD5(InData, Self, UpdateThreadOnProgress,  { AG V7.02, angus V7.08  }
                   Client.HashStartPos, Client.HashEndPos, Client.FileModeRead) { angus V1.57 }
            else if (Keyword = 'XCRC') then                                   { angus V1.54 }
                OutData := FtpFileCRC32B(InData, Self, UpdateThreadOnProgress, { AG V7.02 }
                   Client.HashStartPos, Client.HashEndPos, Client.FileModeRead) { angus V1.57, V7.08 }
            else if (Keyword = 'DIRECTORY') then begin                  { angus V1.54 }
                OutData := Keyword;
                try
                    TriggerEnterSecurityContext;      { AG V7.02 }
                    try
                        Client.BuildDirList(TotalFiles);         { V7.08 }
                       (*  !! Not thread-safe !!    { AG V8.03 }
                        if TotalFiles = -1 then
                            Client.FtpServer.TriggerDisplay(Client, 'Completed directory listing for: ' +
                                                               Client.DirListPath + ' failed')
                        else
                            Client.FtpServer.TriggerDisplay(Client, 'Completed directory listing for: ' +
                                                  Client.DirListPath + ', Total Files: ' + IntToStr (TotalFiles));
                        *)
                        { AG V8.03 }
                        if TotalFiles = -1 then
                            AuxData := 'Completed directory listing for: ' +
                                       Client.DirListPath + ' failed'
                        else
                            AuxData := 'Completed directory listing for: ' +
                                      Client.DirListPath + ', Total Files: ' + IntToStr (TotalFiles);
                        { / AG V8.03 }
                        Client.DataStream.Seek(0, 0);
                    finally
                        TriggerLeaveSecurityContext;  { AG V7.02 }
                    end;
                    if Client.DataStream.Size = 0 then begin   { V7.08 }
                        if TotalFiles = -1 then
                            Buf := 'Listing failed' + #13#10
                        else
                            Buf := Client.FtpServer.FormatResponsePath(Client, Client.DirListPath) + ' not found' + #13#10;
                        Client.DataStreamWriteString(Buf, Client.CurrentCodePage);
                    end;
                except
                    on E:Exception do begin  {angus V8.04 }
                        AuxData := 'Failed to build directory listing - ' + E.Message;
                        Buf := AuxData + #13#10;
                        Client.DataStream.Seek(0, 0);
                        Client.DataStreamWriteString(Buf, Client.CurrentCodePage);
                    end;
                end;
            end
     { angus V1.54 }
            else if (Keyword = 'COMPRESS') then begin { angus V1.54 }
                with Client do begin
                    try
                     { angus V1.55 data stream may be set to restart position, but check sensible }
                        NewSize := DataStream.Size - DataStream.Position;
                        if NewSize < 0 then begin
                            OutData := 'Failed to compress file - Invalid restart position or';
                            ZCompFileDelete := True;
                            Exit;
                        end;
                        ZlibCompressStreamEx(DataStream, ZFileStream, ZCurLevel,
                                         zsZLib, false, Self, UpdateThreadOnProgress); { angus V1.55, V7.08 }
                        if ZFileStream = Nil then begin   {angus V8.04 trap a bug when stream freed accidentally }
                            OutData := 'Failed to compress file - ZFileStream Empty After Zlib';
                            Exit;
                        end;
                        ZFileStream.Position := 0 ;
                        ZCompInfo := ' compressed size ' + IntToKbyte(ZFileStream.Size) +
                            'bytes, uncompressed size ' + IntToKbyte(NewSize) + 'bytes' ;
                     { close data file now, not needed any more }
                        DataStream.Destroy;
                        DataStream := Nil;
                        OutData := ''; { OK }
                    except
                        on E:Exception do begin
                            OutData := 'Failed to compress file - ' + E.Message;
                            ZCompFileDelete := True;
                        end;
                    end;
                end;
            end
            else if (Keyword = 'DECOMPRESS') then begin { angus V1.54 }
                with Client do begin
                    try
                        ZFileStream.Position := 0;
                        NewSize := DataStream.Size ;
                        ZlibDecompressStreamEx(ZFileStream, DataStream,
                                                 Self, UpdateThreadOnProgress) ;   { angus V1.55, V7.08 }
                        NewSize := DataStream.Size - NewSize ;
                        ZCompInfo := ' compressed size ' + IntToKbyte(Client.ZFileStream.Size) +
                             'bytes, uncompressed size ' + IntToKbyte(NewSize) + 'bytes' ;
                        OutData := ''; { OK }
                    except
                        on E:Exception do begin
                            OutData := 'Failed to decompress file - ' + E.Message;
                        end;
                    end;
                end;
            end
            else
                OutData := '';
        end;
    except
        OutData := '';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.SetFtpSslTypes(const Value: TFtpSslTypes); { 1.04 }
begin
    { Implicit SSL cannot be combined with explicit SSL }
    if Value <> FFtpSslTypes then begin
        if (ftpImplicitSsl in Value) and
           ((ftpAuthSsl in Value) or
           (ftpAuthTls in Value) or
           (ftpAuthTlsP in Value) or
           (ftpAuthTlsC in Value)) then begin
            FFtpSslTypes := [];
            raise Exception.Create('Option ftpImplicitSsl cannot be combined ' +
                                   'with explicit SSL types.');
         end
         else
            FFtpSslTypes := Value;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.ClientDataSent(Sender : TObject; AError : Word);    { 1.03 }
var
    Client : TFtpCtrlSocket;
begin
    Client := Sender as TFtpCtrlSocket;
    if Client.CccFlag then begin
        if AError = 0 then begin
            Client.SslBiShutDownAsync;
            Client.CurFtpSslType := curftpSslNone;
        end;
        Client.CccFlag := FALSE;
    end;
    inherited ClientDataSent(Sender, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.CommandCCC(                                           { 1.03 }
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if (Client.FtpSslTypes = []) or (ftpImplicitSsl in Client.FtpSslTypes) then begin
        Answer := Format(msgCmdUnknown, [Keyword]);
        Exit;
    end;
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcCCC;
    if (Client.SslState <> sslEstablished) then begin
        Answer := Format(msgErrInSslOnly, ['CCC']);
        Exit;
    end;
    if (Client.CurFtpSslType = curftpSslNone) then begin
        Answer := Format(msgAuthNoSupport, [Params]);
        Exit;
    end;
    Answer := msgCccOk;
    Client.CccFlag := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.CommandAUTH(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    PlainAnswer : TFtpString;
    TmpCurFtpSslType : TCurFtpSslType;
begin
    {
    msgAuthOk         = '234 Using authentication type %s';
    msgAuthDenied     = '502 %s authentication not allowed'; // SSL/TLS
    msgAuthInitError  = '431 Could not initialize %s connection';
    msgAuthNoSupport  = '504 Auth type "%s" not supported';

    // AUTH TLS-P = AUTH SSL + PROT P
    // AUTH TLS-C = AUTH SSL + PROT C
    }
    if (FFtpSslTypes = []) or (ftpImplicitSsl in FFtpSslTypes) then begin
        Answer := Format(msgCmdUnknown, [Keyword]);
        Exit;
    end;

    Client.CurCmdType := ftpcAUTH;
    TmpCurFtpSslType  := curftpSslNone;
    if      (Params = 'TLS')   and (ftpAuthTls  in FFtpSslTypes) then TmpCurFtpSslType := curftpAuthTls
    else if (Params = 'SSL')   and (ftpAuthSsl  in FFtpSslTypes) then TmpCurFtpSslType := curftpAuthSsl
    else if (Params = 'TLS-C') and (ftpAuthTlsC in FFtpSslTypes) then TmpCurFtpSslType := curftpAuthTlsC
    else if (Params = 'TLS-P') and (ftpAuthTlsP in FFtpSslTypes) then TmpCurFtpSslType := curftpAuthTlsP;

    if (TmpCurFtpSslType = curftpSslNone) then begin
        Answer := Format(msgAuthNoSupport, [Params]);
        Exit;
    end;

    try
        Client.SslEnable                := True;
        Client.SslMode                  := sslModeServer;
        Client.SslContext               := FSocketServer.SslContext;    { angus V7.00 }
        Client.OnSslVerifyPeer          := TransferSslVerifyPeer;
        Client.OnSslHandshakeDone       := TransferSslHandshakeDone;
        Client.OnSslSvrNewSession       := TransferSslSvrNewSession;
        Client.OnSslSvrGetSession       := TransferSslSvrGetSession;
        Client.OnSslSetSessionIDContext := TransferSslSetSessionIDContext;
        { AUTH in plaintext mode }
        if (Client.SslState = sslNone) then begin
            Client.AcceptSslHandshake;
            PlainAnswer := Format(msgAuthOk, [Params]) ;
            TriggerSendAnswer(Client, PlainAnswer);
            PlainAnswer := PlainAnswer + #13#10;
            Client.SslSendPlain(Pointer(AnsiString(PlainAnswer)), Length(PlainAnswer)); // includes only ASCII chars
            Client.CurFtpSslType  := TmpCurFtpSslType;
        end  { AUTH in SSL mode, negotiates a new SSL session }
        else
            if (Client.SslState = sslEstablished) and Assigned(Client.Ssl) then begin
                if f_SSL_version(Client.SSL) >= SSL3_VERSION then
                    Answer := msgAuthYetSetOkV3
                else
                    Answer := msgAuthYetSetOkV2;
                Client.CurFtpSslType  := TmpCurFtpSslType;
        end
        else
            Answer := Format(msgAuthDenied, ['SSL/TLS']);
        Client.FtpState  := ftpcWaitingUserCode;     // Need to force re-login

        { V7.17 }
        if Client.CurFtpSslType = curftpAuthTlsP then // Need to reset prot-level as well
            Client.ProtP := TRUE
        else if Client.CurFtpSslType = curftpAuthTlsC then
            Client.ProtP := FALSE;
        { else as is }
        { / V7.17 }

    except
        Client.CurFtpSslType            := curftpSslNone;
        Client.SslEnable                := False;
        Client.OnSslVerifyPeer          := nil;
        Client.OnSslHandshakeDone       := nil;
        Client.OnSslSvrNewSession       := nil;
        Client.OnSslSvrGetSession       := nil;
        Client.OnSslSetSessionIDContext := nil;
        Answer := Format(msgAuthInitError, [Params]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ DATA CHANNEL PROTECTION LEVEL }
procedure TSslFtpServer.CommandPROT(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    { Possible levels
    C - Clear
    S - Safe
    E - Confidential
    P - Private

    msgProtOk         = '200 Protection level set to %s';
    msgProtNotExists  = '504 Protection level ''%s'' not supported';
    msgProtUnknown    = '504 Protection level ''%s'' not recognized';
   }
    if (FFtpSslTypes = []) then begin
        Answer := Format(msgCmdUnknown, [Keyword]);
        Exit;
    end;
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcPROT;
    if (Client.SslState = sslEstablished) then
    begin
        if (Params = 'C') or (Params = 'P') then begin
            Client.ProtP := Params = 'P';
            Answer := Format(msgProtOK, [Params]);
        end else
        if (Params = 'S') or (Params = 'E') then
            Answer := Format(msgProtNoSupport, [Params])
        else
            Answer := Format(msgProtUnknown, [Params])
    end else
       Answer := Format(msgErrInSslOnly, ['PROT']);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.CommandPBSZ(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    { Dummy command to fullfill RFC4217 }
    if (FFtpSslTypes = []) then begin
        Answer := Format(msgCmdUnknown, [Keyword]);
        Exit;
    end;
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    if (Client.SslState = sslEstablished) then
    begin
        Client.CurCmdType := ftpcPBSZ;
        Answer            := msgPbszOk;
    end
    else
        Answer := Format(msgErrInSslOnly, ['PBSZ']);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.CreateSocket;
begin
    FSocketServer := TFtpSslWSocketServer.Create(Self);// TSslWSocketServer.Create(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslFtpServer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FFtpSslTypes   := [];
    FSocketServer.SslEnable := false;  { V8.01 }
    AddCommand('AUTH', CommandAUTH);
    AddCommand('PROT', CommandPROT);
    AddCommand('PBSZ', CommandPBSZ);
    AddCommand('CCC',  CommandCCC);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslFtpServer.GetSslContext: TSslContext;
begin
    Result := FSocketServer.SslContext;    { angus V7.00 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.SetSslContext(Value: TSslContext);
begin
    FSocketServer.SslContext :=  Value;    { angus V7.00 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.SendAnswer(Client: TFtpCtrlSocket;
  Answer: TFtpString);
begin
    if (Client.CurCmdType = ftpcAUTH) and (Answer = '') then
        Exit;
    inherited SendAnswer(Client, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TransferSslHandshakeDone(Sender: TObject;
  ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
var
    Client  : TFtpCtrlSocket;
begin
    if Assigned(FOnSslHandshakeDone) then
        FOnSslHandshakeDone(Sender, ErrCode, PeerCert, Disconnect);
    { If SSL handshake failed fatal the socket has been closed already. }
    { Then a "226 File OK" is sent anyway, even with code below.        } //fix needed?
    if (ErrCode <> 0) or Disconnect then begin
        if not (Sender is TFtpCtrlSocket) then begin
            Client := TFtpCtrlSocket((Sender as TWSocket).Owner);
            Client.AbortingTransfer := TRUE;
            Client.TransferError    := 'SSL handshake failed - ' + Client.SslHandshakeRespMsg;  { V8.05 }
            PostMessage(FHandle, FMsg_WM_FTPSRV_Close_Data,
                        WPARAM(Client.ID), LPARAM(Client));
            Disconnect := FALSE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TransferSslSetSessionIDContext(Sender: TObject;
  var SessionIDContext: TSslSessionIdContext);
begin
    if Assigned(FOnSslSetSessionIDContext) then
        FOnSslSetSessionIDContext(Sender, SessionIDContext);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TransferSslSvrGetSession(Sender: TObject;
  var SslSession: Pointer; SessId: Pointer; Idlen: Integer;
  var IncRefCount: Boolean);
begin
    if Assigned(FOnSslSvrGetSession) then
        FOnSslSvrGetSession(Sender, SslSession, SessId, IdLen, IncRefCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TransferSslSvrNewSession(Sender: TObject;
  SslSession, SessId: Pointer; Idlen: Integer;
  var AddToInternalCache: Boolean);
begin
    if Assigned(FOnSslSvrNewSession) then
        FOnSslSvrNewSession(Sender, SslSession, SessID, IDLen, AddToInternalCache);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TransferSslVerifyPeer(Sender: TObject;
  var Ok: Integer; Cert: TX509Base);
begin
    if Assigned(FOnSslVerifyPeer) then
        FOnSslVerifyPeer(Sender, Ok, Cert);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TriggerClientConnect(
    Client  : TFtpCtrlSocket;
    AError  : Word);
begin
    inherited TriggerClientConnect(Client, AError);
    if NOT FSocketServer.IsClient(Client) then       { angus V7.00 }
        Exit;
    { The event handler may have closed the connection }
    if Client.State <> wsConnected then
        Exit;
    Client.SslEnable  := ftpImplicitSsl in Client.FtpSslTypes;
    if Client.SslEnable then begin
        Client.CurFtpSslType            := curftpImplicitSsl;
        Client.SslMode                  := sslModeServer;
        Client.SslContext               := FSocketServer.SslContext;    { angus V7.00 }
        Client.OnSslVerifyPeer          := TransferSslVerifyPeer;
        Client.OnSslHandshakeDone       := TransferSslHandshakeDone;
        Client.OnSslSvrNewSession       := TransferSslSvrNewSession;
        Client.OnSslSvrGetSession       := TransferSslSvrGetSession;
        Client.OnSslSetSessionIDContext := TransferSslSetSessionIDContext;
        try
            Client.AcceptSslHandshake;
        except
            Client.SslEnable := False;
            Client.Banner := msgErrSslInit;
            Client.StartConnection;
            Client.Close;
        end;
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.ClientPassiveSessionAvailable(Sender : TObject; AError : Word);
var
    Client  : TFtpCtrlSocket;
    Data    : TWSocket;
begin
    Data    := TWSocket(Sender);
    Client  := TFtpCtrlSocket(Data.Owner);
    Client.DataSocket.SslEnable := False; // we need to start SSL by ourself

    inherited ClientPassiveSessionAvailable(Sender, AError);

    if (not Client.PassiveStart) and Client.ProtP and
       (Client.DataSocket.SslState = sslNone) then
    begin
        Client.DataSocket.SslEnable                 := TRUE;
        Client.DataSocket.SslMode                   := sslModeServer;
        Client.DataSocket.SslContext                := SslContext;
        Client.DataSocket.OnSslVerifyPeer           := TransferSslVerifyPeer;
        Client.DataSocket.OnSslHandshakeDone        := TransferSslHandshakeDone;
        Client.DataSocket.OnSslSvrNewSession        := TransferSslSvrNewSession;
        Client.DataSocket.OnSslSvrGetSession        := TransferSslSvrGetSession;
        Client.DataSocket.OnSslSetSessionIDContext  := TransferSslSetSessionIDContext;
        try
            Client.DataSocket.AcceptSslHandshake;
        except
            Client.DataSocket.SslEnable                := False;
            Client.DataSocket.OnSslVerifyPeer          := nil;
            Client.DataSocket.OnSslHandshakeDone       := nil;
            Client.DataSocket.OnSslSvrNewSession       := nil;
            Client.DataSocket.OnSslSvrGetSession       := nil;
            Client.DataSocket.OnSslSetSessionIDContext := nil;
            SendAnswer(Client, Format(msgAuthInitError, ['SSL']));
            PostMessage(FHandle, FMsg_WM_FTPSRV_Close_Data,
                        WPARAM(Client.ID), LPARAM(Client));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TriggerStorSessionConnected(Client: TFtpCtrlSocket;
  Data: TWSocket; AError: Word);
begin
    inherited TriggerStorSessionConnected(Client, Data, AError);
    if ((not Client.PassiveStart) and Client.PassiveConnected) or
       (AError <> 0) then
        Exit;
    Client.DataSocket.SslEnable := False;
    if (Client.DataSocket.State = wsConnected) then
    begin
        if Client.ProtP and (Client.DataSocket.SslState = sslNone) then
        begin
            Client.DataSocket.SslEnable                 := True;
            Client.DataSocket.SslMode                   := sslModeServer;
            Client.DataSocket.SslContext                := SslContext;
            Client.DataSocket.OnSslVerifyPeer           := TransferSslVerifyPeer;
            Client.DataSocket.OnSslHandshakeDone        := TransferSslHandshakeDone;
            Client.DataSocket.OnSslSvrNewSession        := TransferSslSvrNewSession;
            Client.DataSocket.OnSslSvrGetSession        := TransferSslSvrGetSession;
            Client.DataSocket.OnSslSetSessionIDContext  := TransferSslSetSessionIDContext;
            try
                Client.DataSocket.AcceptSslHandshake;
            except
                Client.DataSocket.SslEnable := False;
                Client.DataSocket.OnSslVerifyPeer           := nil;
                Client.DataSocket.OnSslHandshakeDone        := nil;
                Client.DataSocket.OnSslSvrNewSession        := nil;
                Client.DataSocket.OnSslSvrGetSession        := nil;
                Client.DataSocket.OnSslSetSessionIDContext  := nil;
                Client.AbortingTransfer := TRUE;
                Client.TransferError    := msgErrSslInit;
                PostMessage(FHandle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                        WPARAM(Client.ID), LPARAM(Client));
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TriggerRetrSessionConnected(Client: TFtpCtrlSocket;
  Data: TWSocket; AError: Word);
begin
    inherited TriggerRetrSessionConnected(Client, Data, AError);

    if ((not Client.PassiveStart) and Client.PassiveConnected) or
       (AError <> 0) then
        Exit;

    Client.DataSocket.SslEnable := False;
    if Client.ProtP and (Client.DataSocket.SslState = sslNone) then begin
        Client.DataSocket.SslEnable                 := True;
        Client.DataSocket.SslMode                   := sslModeServer;
        Client.DataSocket.SslContext                := SslContext;
        Client.DataSocket.OnSslVerifyPeer           := TransferSslVerifyPeer;
        Client.DataSocket.OnSslHandshakeDone        := TransferSslHandshakeDone;
        Client.DataSocket.OnSslSvrNewSession        := TransferSslSvrNewSession;
        Client.DataSocket.OnSslSvrGetSession        := TransferSslSvrGetSession;
        Client.DataSocket.OnSslSetSessionIDContext  := TransferSslSetSessionIDContext;
        try
            Client.DataSocket.AcceptSslHandshake;
        except
            Client.DataSocket.SslEnable                := False;
            Client.DataSocket.OnSslVerifyPeer          := nil;
            Client.DataSocket.OnSslHandshakeDone       := nil;
            Client.DataSocket.OnSslSvrNewSession       := nil;
            Client.DataSocket.OnSslSvrGetSession       := nil;
            Client.DataSocket.OnSslSetSessionIDContext := nil;
            Client.AbortingTransfer := TRUE;
            raise Exception.Create(msgErrSslInit);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslFtpServer.MsgHandlersCount : Integer;
begin
    Result := 2 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_FTPSRV_ABORT_TRANSFER    := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_FTPSRV_Close_Data     := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_FTPSRV_ABORT_TRANSFER);
        FWndHandler.UnregisterMessage(FMsg_WM_FTPSRV_Close_Data);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSslFtpWSocketMultiListenItem }
constructor TSslFtpWSocketMultiListenItem.Create(Collection: TCollection);
begin
    inherited Create(Collection);
    SslEnable       := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpWSocketMultiListenItem.SetFtpSslTypes(
  const Value: TFtpSslTypes);
begin
  { Implicit SSL cannot be combined with explicit SSL }
    if Value <> FFtpSslTypes then begin
        if (ftpImplicitSsl in Value) and
           ((ftpAuthSsl in Value) or
           (ftpAuthTls in Value) or
           (ftpAuthTlsP in Value) or
           (ftpAuthTlsC in Value)) then begin
            FFtpSslTypes := [];
            raise Exception.Create('Option ftpImplicitSsl cannot be combined ' +
                                   'with explicit SSL types.');
         end
         else
            FFtpSslTypes := Value;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TFtpSslWSocketServer }
function TFtpSslWSocketServer.MultiListenItemClass: TWSocketMultiListenItemClass;
begin
    Result := TSslFtpWSocketMultiListenItem;
end;

{$ENDIF} // USE_SSL{$ENDIF}
end.


