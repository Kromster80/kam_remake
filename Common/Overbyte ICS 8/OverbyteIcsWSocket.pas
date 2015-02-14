{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TWSocket class encapsulate the Windows Socket paradigm
Creation:     April 1996
Version:      8.14
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
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

History:
If not otherwise noted, changes are by Francois Piette
Jul 18, 1996  Move all low level socket to winsock to be Delphi 2.x compatible
Sep 18, 1996  Use structured exception for handling errors
Sep 19, 1996  Check csDestroying before invoking event handler
Nov 04, 1996  Better error handling
Jan 31, 1997  Changed property assignation for Addr, Port and Proto
              Added notification handler
Feb 14, 1997  Corrected bug in property assignation for Addr, Port and Proto
Mar 26, 1997  Make UDP protocol work correctly
              Enable UDP broadcasting by using addr 255.255.255.255
Apr 1, 1997   Added class function when independent of any open socket
              Moved InitData as global
              Added ReceivedFrom function
              Added ResolveHost function
Jul 22, 1997  Adapted to Delphi 3 which has a modified winsock.accept
Aug 13, 1997  'sin' member made public
Aug 24, 1997  Create the only help
              Makes writing HSocket the same as calling Dup.
Sep 5, 1997   Version 2.01, added WinsockInfo function
Sep 21, 1997  Version 2.02, make it really thread safe
                            created global WSocketVersion
Sep 25, 1997  Version 2.04, port to C++Builder
Sep 27, 1997  Version 2.05. All class methods converted to global
              procedure or function because C++Builder do not like
              class method very much.
              Old class method              New global function
              ----------------              -------------------
              WinsockInfo                   WinsockInfo
              SocketErrorDesc               WSocketErrorDesc
              GetHostByAddr                 WSocketGetHostByAddr
              GetHostByName                 WSocketGetHostByName
              ResolveHost                   WSocketResolveHost
              HostName                      LocalHostName
Oct 02, 1997  V2.06 Added a check in destructor to avoid calling WSACleanup at
              design time which crashes the excellent Eagle Software CDK.
Oct 16, 1997  V2.07 Added PortNum property with numeric value for Port.
              Added RcvdCount property to return the number of
              characters received in the buffer but not read yet. Do not
              confuse with ReadCount which returns the number of chars
              already received.
              Added a check for FWait assignation in front of ReadLine
              Prefixed each TSocketState value by 'ws' to avoid name conflict.
              Moved FHSocket member to private section because the property
              HSocket does the right job.
              Added a check for state closed when changing Port, Proto and Addr.
Oct 22, 1997  V2.08 Added Flush method (asked by john@nexnix.co.uk) and
              FlushTimeout property (default to 60 seconds).
Oct 22, 1997  V2.09 Added SendFlags property to enable sending in or out of
              band data (normal or urgent, see RFC-1122)
Oct 28, 1997  V2.10 Added an OnLineTooLong event and code to handle the case
              where ReadLine has been called and the buffer overflowed (line
              long)
Oct 29, 1997  V2.11 Added DnsLookup functionnality (DnsLookup method, DnsResult
              property and DnsLookupDone event).
              Calling the connect method with a hostname work well except that
              it could block for a long period (ie: 2 minutes) if DNS do not
              respond. Calling the connect method with a numeric IP address will
              never block. So you can call DnsLookup to start hostname
              resolution in the background, after some time you evenutually
              receive the OnDnsLookupDone event. The copy the DnsResult property
              to the Addr property and call connect.
Oct 30, 1997  V2.12 added a check in DnsLookup to handel numeric IP which do
              not require any lookup. The numeric IP is treated immediately
              and immediately trigger the DnsLookupDone event.
              I modified the code to be compatible with Delphi 1.
Oct 31, 1997  V2.13 added CancelDnsLookup procedure.
Nov 09, 1997  V2.14 add LocalIPList function to get the list of local IP
              addresses (you have two IP addresses when connected to a LAN
              and an ISP).
Nov 11, 1997  V2.15 Made TCustomWSocket with virtual functions. This will
              allow to easily descend a new component from TCustomWSocket.
              Make ReadLine stop when the connection is broken.
Nov 12, 1997  V2.16 Corrected bug (Justin Yunke <yunke@productivity.org>)
              in LocalIPList: phe should be checked for nil.
Nov 18, 1997  Added ReceiveStr function (Suggested by FLDKNHA@danisco.com)
Nov 30, 1997  V2.18 Added a call to OnDnsLookupDone when canceling.
Dec 04, 1997  V2.19 Added LocalPort property and SessionConnected event
              for UDP socket.
              V2.20 Modified MessageLoop and ProcessMessages to process not
              only the socket messages, but all messages (necessary if the
              thread has several TWSocket for example).
Dec 09, 1997  V2.21 Corrected a minor bug in ReceiveStr. Detected by
              david@e.co.za (David Butler).
Dec 10, 1997  V2.22 Corrected a minor bug in Send which now correctly
              returns the number of bytes sent. Detected by
              james.huggins@blockbuster.com
Dec 16, 1997  V2.23 Corrected a bug which prevented the receiving of datagram
              from a UDP socket.
              Thank to Mark Melvin (melvin@misrg.ml.org) for pointing it.
Dec 20, 1997  V2.24 Added the PeekData function as suggested by Matt Rose
              mcrose@avproinc.com
Dec 26, 1997  V2.25 Added the Text property as suggested by Daniel P. Stasinski
              <dse@pacific.net>. Made GetXPort work even when listening as
              suggested by is81024@cis.nctu.edu.tw.
Jan 10, 1998  V2.26 Check for null hostname in DNSLookup
              Added DnsResultList with all IP addresses returned form DNS
Jan 13, 1998  V2.27 a Added MultiThreaaded property to tell the component that
              it is working in a thread and should take care of it (call
              internal ProcessMessages in place of Application.ProcessMessages,
              and do not use the WaitCtrl object).
Jan 15, 1998  V2.28 WMAsyncSelect revisited to work properly with NT winsock 2.
Feb 10, 1998  V2.29 Added an OnError event. If not assigned, then the component
              raise an exception when the error occurs.
Feb 14, 1998  V2.30 Published Text property
Feb 16, 1998  V2.31 Added virtual methods to trigger events
              Renamed all event handler variable to begin with FOn
Feb 26, 1998  V2.32 Added procedure PutDataInSendBuffer and PutStringInSendBuffer
              Using PutDataInSendBuffer you can place data in the send buffer
              without actualy trying to send it. This allows to place several
              (probably small) data chunk before the component attempt to send
              it. This prevent small packet to be sent. You can call
              Send(nil, 0) to force the component to begin to send data.
              If the buffer was not empty, PutDataInSendBuffer will just queue
              data to the buffer. This data will be sent in sequence.
Mar 02, 1998  V2.33 Changed the error check with WSAstartup as pointed out by
              Donald Strenczewilk (dstrenz@servtech.com)
Mar 06, 1998  V2.34 Added a runtime property to change the buffer size.
Mar 27, 1998  V2.35 Adapted for C++Builder 3
Apr 08, 1998  V2.36 Made SetDefaultValue virtual
Apr 13, 1998  V2.37 Reset FDnsLookupHandle to 0 after a failed call to
              WSACancelAsyncRequest
Apr 22, 1998  V2.38 Published AllSent property to let outside know if our
              buffer has some data unsent.
Apr 28, 1998  V2.39 Added LingerOnOff and LingerTimeout. Default values are
              wsLingerOn and timeout = 0 to behave by default as before.
              This value is setup just before Connect. Call SetLingerOption to
              set the linger option on the fly (the connection must be
              established to set the option). See winsock.closesocket on line
              help (winsock.hlp or win32.hlp) for a dsicussion of this option
              usage.
May 06, 1998  V2.40 Added a workaround for Trumpet winsock inet_addr bug.
              Thanks to Andrej Cuckov <andrej@cuckov.com> for his code.
May 18, 1998  V2.41 Jan Tomasek <xtomasej@feld.cvut.cz> found that Trumpet
              Winsock (Win 3.11) has some bugs and suggested a workaround in
              TryToSend procedure. This workaround makes TWSocket blocking in
              some cases. A new property enables the workaround. See code.
Jun 01, 1998  V2.42 In finalization section, check for not assigned IPList.
Jun 15, 1998  V2.43 Added code to finalization section to unload winsock if
              still loaded at that point (this happend if no socket where
              created but WinsockInfo called). Suggested by Daniel Fazekas
              <fdsoft@dns.gyor-ph.hu>
Jun 27, 1998  V2.44 Added checks for valid arguments in SetPort, SetProto
              and SetAddr. Deferred address resolution until Connect or Listen.
Jul 08, 1998  V2.45 Adadpted for Delphi 4
Jul 20, 1998  V2.46 Added SetWindowLong(FWindowHandle, 0, 0) in the destructor
              and a check for TWSocket class in XSocketWindowProc.
              Added virtual method RealSend.
Jul 23, 1998  V2.47 Added a TriggerSessionClosed from TryToSend in case of
              send error. This was called before, but with a nul error argument.
              Now it correctly gives the error number.
              Added a trashcan to receive data if no OnDataAvailable event
              handler is installed. Just receive the data and throw it away.
              Added reverse dns lookup asynchronous code (IP -> HostName).
              Thanks to Daniel Fazekas <fdsoft@dns.gyor-ph.hu> for his code.
Jul 30, 1998  V2.48 Changed local variable "error" by FLastError in SocketError
              to make it available from the OnError handler. Thanks to
              dana@medical-info.com for finding this bug.
              In Abort procedure, deleted all buffered data because it was send
              the next time the socket is opened !
              Added CancelDnsLookup in Abort procedure.
Aug 28, 1998  V2.49 Made InternalClose and ReceiveStr virtual
Sep 01, 1998  V2.50 Ignore CancelDnsLookup exception during destroy
Sep 29, 1998  V2.51 In InternalClose, protect AssignDefaultValue with
              try/except because SessionClosed event handler may have destroyed
              the component.
Oct 11, 1998  V2.52 Changed Shutdown(2) to Shutdown(1) in Internal Close to
              prevent data lost on send. You may have to call Shutdown(2) in
              your own code before calling Close to have the same behaviour as
              before.
              Changed argument type for ASyncReceive and passed 0 from FD_CLOSE
              message handler.
Oct 28, 1998  V2.53 Made WSocketLoadWinsock and WSocketUnloadWinsock public.
Nov 11, 1998  V2.54 Added OnDisplay event for debugging purpose
Nov 16, 1998  V2.55 Ignore WSANOTINITIALIZED error calling CloseSocket. This
              occurs when using TWSocket from a DLL and the finalization
              section is called before destroying TWSocket components (this is
              a program logic error).
              Made some properties and methods protected instead of private.
              Made some methods virtual.
              Added an Error argument to InternalClose.
              Added DoRecv virtual function.
              Added WSocketResolvePort
              Added WSocketResolveProto
              Deferred port and protocol resolution until really needed
              Transformed Listen to procedure (in case of failure Listen
              always calls SocketError which triggers an exception or the
              OnError event).
Nov 22, 1998  V3.00 Skipped from V2.55 to V3.00. Socks support is major update!
              Added SOCKS5 (RFC-1928) support for TCP connection and
              simple usercode passwword authentication.
              Consider the socks code as beta !
              New properties: SocksServer, SocksPort, SocksUsercode,
              SocksPassword, FSocksAuthentication. New events: OnSocksError,
              OnSocksConnected, OnSocksAuthState.
              I used WinGate 2.1d to test my code. Unfortunately WinGate do
              not correctly handle user authentication, so the code here is
              just untested...
Dec 05, 1998  V3.10 Removed ReadLine feature using TWait component.
              Added new TCustomLineWSocket and TCustomSyncWSocket.
              Those modifications implies that the ReadLine functionnality is
              slightly changed. Notably, the end of line marker is now
              configurable and remains in the received line unless a timeout
              occurs or the buffer is too small.
Dec 10, 1998  V3.11 Added missing code to resolve port in the Listen method.
Dec 12, 1998  V3.12 Added write method for LocalPort property. Thanks to
              Jan Tomasek <xtomasej@feld.cvut.cz> for his code.
              Added background exception handling.
              Fixed a bug in TCustomLineWSocket.TriggerDataAvailable which was
              not calling the inherited function when it actually should.
              Added a check on multithreaded in WaitForClose to call the
              correct ProcessMessages procedure.
              Added SOCKS4 support (only tcp connect is supported).
Dec 28, 1998  V3.13 Changed WSocketResolveHost to check for invalid numeric
              IP addresses whitout trying to use them as hostnames.
Dec 30, 1998  V3.14 Changed SetPort to SetRemotePort to solve the SetPort
              syndrome with BCB. Also chnaged GetPort to be consistant.
Jan 12, 1999  V3.15 Introduced DoRecvFrom virtual function. This correct a bug
              introduced in V3.14 related to UDP and RecvFrom.
Jan 23, 1999  V3.16 Changed FRcvdFlag computation in DoRecv and DoRecvFrom
              because it caused problems with HTTP component and large blocks.
              Removed modification by Jan Tomasek in TriggerDataAvailable
Jan 30, 1999  V3.17 Added WSocketResolveIp function.
              Checked for tcp protocol before setting linger off in abort.
              Moved a lot of variables from private to protected sections.
              Removed check for Assigned(FOnDataSent) in WMASyncSelect.
Feb 03, 1999  V3.18 Removed useless units in the uses clause.
Feb 14, 1999  V4.00 Jump to next major version number because lots of
              fundamental changes have been done. See below.

              Use runtime dynamic link with winsock. All winsock functions
              used by TWSocket are linked at runtime instead of loadtime. This
              allows programs to run without winsock installed, provided program
              doesn't try to use TWSocket or winsock function without first
              checking for winsock installation.
              Removed WSocketLoadWinsock and all use to DllStarted because it
              is no longer necessary because winsock is automatically loaded
              and initialized with the first call to a winsock function.

              Added MessagePump to centralize call to the message pump.
              It is a virtual procedure so that you can override it to
              cutomize your message pump. Also changed slightly ProcessMessages
              to closely match what is done in the forms unit.

              Removed old stuff related to WaitCtrl (was already excluded from
              compilation using a conditional directive).

              Added NOFORMS conditional compilation to exclude the Forms unit
              from wsocket. This will reduce exe or dll size by 100 to 150KB.
              To use this feature, you have to add NOFORMS in your project
              options in the "defines" edit box in the "directory/conditional"
              tab. Then you must add a message pump to your application and
              call it from TWSocket.OnMessagePump event handler. TWSocket really
              need a message pump in order to receive messages from winsock.
              Depending on how your application is built, you can use either
              TWSocket.MessageLoop or TWSocket.ProcessMessages to quickly build
              a working message pump. Or you may build your own custom message
              pump taylored to your needs. Your message pump must set
              TWSocket.Terminated property to TRUE when your application
              terminates or you may experience long delays when closing your
              application.
              You may use NOFORMS setting even if you use the forms unit (GUI
              application). Simply call Application.ProcessMessages in the
              OnMessagePump event handler.
              OnMessagePump event is not visible in the object inspector. You
              must assign it at run-time before using the component and after
              having created it (in a GUI application you can do that in the
              FormCreate event, in a console application, you can do it right
              after TWSocket.Create call).
Feb 17, 1999  V4.01 Added LineEcho and LineEdit features.
Feb 27, 1999  V4.02 Added TCustomLineWSocket.GetRcvdCount to make RcvdCount
              property and ReceiveStr work in line mode.
Mar 01, 1999  V4.03 Added conditional compile for BCB4. Thanks to James
              Legg <jlegg@iname.com>.
Mar 14, 1999  V4.04 Corrected a bug: wsocket hangup when there was no
              OnDataAvailable handler and line mode was on.
Apr 21, 1999  V4.05 Added H+ (long strings) and X+ (extended syntax)
              compilation options
May 07, 1999  V4.06 Added WSAECONNABORTED to valid error codes in TryToSend.
Jul 21, 1999  V4.07 Added GetPeerPort method, PeerPort and PeerAddr propertied
              as suggested by J. Punter <JPunter@login-bv.com>.
Aug 20, 1999  V4.05 Changed conditional compilation so that default is same
              as latest compiler (currently Delphi 4, Bcb 4). Should be ok for
              Delphi 5.
              Added LocalAddr property as suggested by Rod Pickering
              <fuzzylogic123@yahoo.com>. LocalAddr default to '0.0.0.0' and is
              intended to be used by a client when connecting to a server, to
              select a local interface for multihomed computer. Note that to
              select an interface for a server, use Addr property before
              listening.
              LocalAddr has to be an IP address in dotted form. Valid values are
              '0.0.0.0' for any interface, '127.0.0.1' for localhost or any
              value returned by LocalIPList.
              Replaced loadtime import for ntohs and getpeername by runtime
              load.
              Revised check for dotted numeric IP address in WSocketResolveHost
              to allow correct handling of hostnames beginning by a digit.
              Added OnSendData event. Triggered each time data has been sent
              to winsock. Do not confuse with OnDataSent which is triggered
              when TWSocket internal buffer is emptyed. This event has been
              suggested by Paul Gertzen" <pgertzen@livetechnology.com> to
              easyly implement progress bar.
              Corrected WSocketGetHostByAddr to make it dynamically link to
              winsock.
Sep 5, 1999   V4.09 Added CloseDelayed method.
              Make sure that TriggerSessionClosed is called from WMASyncSelect
              and InternalClose, even if there is no OnSessionClosed event
              handler assigned. This is required to make derived components
              work correctly.
              Created message WM_TRIGGER_EXCEPTION to help checking background
              exception handling (OnBgException event).
              Corrected bug for Delphi 1 and ReallocMem.
Oct 02, 1999  V4.10 Added Release method.
Oct 16, 1999  V4.11 Corrected a bug in TCustomLineWSocket.DoRecv: need to move
              data in front of buffer instead of changing buffer pointer which
              will crash the whole thing at free time.
Oct 23, 1999  V4.12 Made WSocketIsDottedIP a public function
Nov 12, 1999  V4.13 removed 3 calls to TriggerSocksAuthState because it was
                    called twice. By A. Burlakov <alex@helexis.com>.
Jan 24, 1999  V4.14 Call Receive instead of DoRecv from ReceiveStr to be sure
              to set LastError correctly. Thanks to Farkas Balazs
              <megasys@www.iridium.hu>
              Suppressed FDllName and used winsocket constant directly. I had
              troubles with some DLL code and string handling at program
              termination.
Apr 09, 2000 V4.15 Added error number when resolving proto and port
Apr 29, 2000 V4.16 Added WSocketForceLoadWinsock and
             WSocketCancelForceLoadWinsock. Thanks to Steve Williams.
             Created variable FSelectEvent to store current async event mask.
             Added ComponentOptions property with currently only one options
             wsoNoReceiveLoop which disable a receive loop in AsyncReceive.
             This loop breaking was suggested by Davie <smatters@smatters.com>
             to lower resource usage with really fast LAN and large transfers.
             By default, this option is disabled so there is no change needed
             in current code.
May 20, 2000 V4.17 Made TSocket = u_int (same def as in winsock.pas)
             Moved bind after setting options.
             Thanks to Primoz Gabrijelcic <fab@siol.net>
Jul 15, 2000 V4.18 Alon Gingold <gingold@hiker.org.il> changed
             TCustomSocksWSocket calls to inherited triggers of
             TriggerSessionConnected and TriggerDataAvailable.
             Now, it calls the trigger directly. This solves the problem
             of descendent classes with overridden triggers, not being
             called when a REAL connection was established, and when real
             data starts coming in. Special care MUST be taken in such
             overridden triggers to ONLY call the inherited trigger AND
             IMMEDIATELY EXIT when FSocksState <> socksData to avoid loopback
Jul 22, 2000 V4.19 John Goodwin <john@jjgoodwin.com> found a failure in the
             logic for DnsLookup. He also implemented a workaround.
             See DnsLookup comments for explanation.
Aug 09, 2000 V4.20 Alon Gingold <gingold2@mrm-multicat.com> found a bug in
             SOCKS4 implementation where a nul byte was incorrectly added
             (it should be added only with SOCKS4A version, not straith
             SOCKS4).
Sep 17, 2000 V4.21 Eugene Mayevski <Mayevski@eldos.org> added TWndMethod for
             NOFORMS applications in other components.
Oct 15, 2000 V4.22 Added method GetXAddr which returns local IP address to
             which a socket has been bound. There was already a GetXPort.
             Thanks to Wilfried Mestdagh <wilfried_sonal@compuserve.com>
             and Steve Williams <stevewilliams@kromestudios.com>.
Nov 08, 2000 V4.23 Moved FSelectEvent from private to protected section.
Nov 11, 2000 V4.24 Added LineLimit property and OnLineLimitExceeded event.
             When using line mode, line length is checked as each data block is
             comming. If the length is greater than the limit, then the event
             is triggered. You have the opportunity to close the socket or
             change the limit to a higher value. Thus you can prevent a hacker
             from locking your system by sending unlimited line which otherwise
             would eat up all system resources.
             Changed line handling variables to LongInt
             Checked all length involved in StrPCopy calls.
Nov 26, 2000 V4.25 Do not trust GetRcvdCount. Always call Receive to check for
             incomming data (sometime NT4 will hang if we don't do that).
Jan 24, 2001 V4.26 Blaine R Southam <bsoutham@iname.com> fixed out of bound
             error in TCustomLineWSocket.TriggerDataAvailable
Feb 17, 2001 V4.27 Davie <smatters@smatters.com> fixed a bug causing byte lost
             when closing (related to wsoNoReceiveLoop option).
May 04, 2001 V4.28 Fixed W2K bug (winsock message ordering)
Jun 18, 2001 V4.29 Added AllocateHWnd and DeallocateHWnd from Forms unit to
             avoid warning from Delphi 6 in all other components.
Jul 08, 2001 V4.30 Fixed small bug related to NOFOMRS and V4.29
Jul 26, 2001 V4.31 Checked csDesigning in GetRcvdCount so that Delphi 6 does'nt
             crash when object inspector wants to display RcvdCount value.
             Added multicast capability and UDP ReuseAddr. Thanks to Mark
             G. Lewis <Lewis@erg.sri.com> for his code.
             Added TriggerSessionClosed to SocketError as suggested by Wilfried
             Mestdagh <wilfried_sonal@compuserve.com>
Jul 28, 2001 V4.32 New option wsoTcpNoDelay implemented. Code by Arnaldo Braun
             <abraun@th.com.br>
Jul 30, 2001 V4.33 Corrected at few glitches with Delphi 1
Sep 08, 2001 V4.34 Added ThreadAttach and related functions
Nov 27, 2001 V4.35 Added type definition for in_addr and Delphi 2 (Yes there are
             still some peoples who wants to use it. Don't ask me why !).
Dec 02, 2001 V4.36 david.brock2@btinternet.com found a bug in SOCKS4 where
             error check incorrectly checked "FRcvBuf[1] = #$90" instead of
             "FRcvBuf[1] <> #90". He also found a bug when receiving domain name
             where length of name was incorrectly copyed to the buffer.
Dec 23, 2001 V4.37 Removed bWrite, nMoreCnt, bMoreFlag and nMoreMax which where
             not more really used. Thanks to Al Kirk <akirk@pacific.net> for
             showing that.
Feb 24, 2002 V4.38 Wilfried Mestdagh <wilfried@mestdagh.biz> added ThreadDetach
             and a property editor for LineEnd. XSocketDeallocateHWnd made a
             function.
             I created a new unit WSocketE.pas to put Wilfried's property
             editor so that it works correctly with Delphi 6.
Apr 24, 2002 V4.39 Removed OnLineTooLong event which was not used anywhere.
             Use OnLineLimitExceeded event if you used this event.
             Thanks to Alex Kook <cookis@mail.ru> for finding this one.
Apr 27, 2002 V4.40 Added procedure WSocketUnregisterClass to be able to
             unregister hidden window. This is necessary when TWSocket is
             used within a DLL which is unloaded and reloaded by applications,
             specially when running with Windows-XP. Thanks to Jean-Michel Aliu
             <jmaliu@jmasoftware.com> who provided a test case.
Jun 02, 2002 V4.41 allow SOCK_RAW in Connect method for any protocol which is
             not TCP or UDP. Thanks to Holger Lembke <holger@hlembke.de>.
Jun 04, 2002 V4.42 Do not call Listen for SOCK_RAW.
             Thanks to Holger Lembke <holger@hlembke.de>.
Jun 08, 2002 V4.43 Add a dummy Register procedure for BCB1.
             Thanks to Marc-Alexander Prowe <listen@mohajer.de>.
Jul 07, 2002 V4.44 Added code in Connect method to check if socket still opened
             after OnChangeState event. If not, trigger an error WSAINVAL.
Sep 16, 2002 V4.45 Exposed RcvdPtr and RcvdCnt readonly properties.
Sep 17, 2002 V4.46 Used InterlockedIncrement/InterlockedDecrement to Inc/Dec
             socket count safely when TWSocket is used within a thread. This
             was proposed by Matthew Meadows <matthew.meadows@inquisite.com>
Sep 28, 2002 V4.47 Changed DnsLookup so that a hostname is checked for dotted
             IP addresse and resolve it numerically. Thanks to Bogdan Calin
             <soul4blade@yahoo.com> who found this bug. Alos loaded the result
             list with the address to be consistant with real lookup result.
Nov 17, 2002 V4.48 Roland Klabunde <roland.klabunde@gmx.net> found a bug in
             multicast code: listening on a specific interface was ignored.
             He fixed Listen and Connect.
Nov 27, 2002 V4.49 Added ListenBacklog property, default to 5.
Dec 17, 2002 V4.50 Moved code to virtual function to permit SSL implementation.
Jan 19, 2003 V5.00 First pre-release for ICS-SSL. New major version number
             V5.01 Gabi Slonto <buffne01@gmx.net> found a bug in DnsLookup
             when hostname was actulally a dotted IP address.
Mar 18, 2003 V5.02 Fixed WSocketIsDottedIP: reordering of boolean expressions
             involaving a string. Thanks to Ian Baker <ibaker@codecutters.org>
Apr 30, 2003 V5.03 Replaced all calls to setsockopt by calls to
             WSocket_setsockopt to avoid statically linked winsock DLL.
             Thanks to Piotr Dalek <enigmatical@interia.pl>.
             Also replaced inet_addr by WSocket_inet_addr.
Aug 27, 2003 V5.04 Marco van de Voort <marcov@stack.nl> added FreePascal (FPC)
             conditional compilation. Please contact him for any FPC support
             question.
Aug 28, 2003 V5.05 Fixed a multithreading issue related to windows class
             registration. Now using a critical section around the code.
             Thanks to Bogdan Ureche <bureche@omnivex.com> for his precious help.
Aug 31, 2003 V5.06 Added warning about deprecated procedures Synchronize,
             WaitUntilReady and ReadLine. Do not use them in new applications.
Sep 03, 2003 V5.07 Bogdan Ureche <bureche@omnivex.com> added a critical section
             to avoid problem when winsock.dll is unloaded by a thread while
             another thread is still using some TWSocket.
Sep 15, 2003 V5.08 Fixed finalization section to no free critical section if
             a TWSocket is still existing. This happend for example when a
             TWSocket is on a form and Halt is called from FormCreate event.
             Changed SendStr argument to const.
Nov 09, 2003 V5.09 Added manifest constants for Shutdown
             Added TCustomLineWSocket.SendLine method.
Jan 16, 2004 V5.10 Added "const" in front of all method using strings.
Jan 17, 2004 V5.11 Modified TriggerDataAvailable so that when in LineMode, we
             check if a line is still in the buffer of already received data.
             Also updated WMTriggerDataAvailable to avoid infinite loops.
             Introduced FLineFound to flag when a line has been found.
             See "OLD_20040117" to find this code.
Jan 21, 2004 V5.12 Checked null string in PutStringInSendBuffer and null
             pointer in PutDataInSendBuffer.
Jan 26, 2004 V5.13 Conditional compilation for BCB for constants for Shutdown.
             Reordered uses clause for FPC compatibility.
             Fixed TCustomLineWSocket.TriggerDataAvailable to deliver data
             already received while in line mode but after component user
             turned line mode off in the middle of the way. This could occur
             for example in a HTTP application where line mode is used to
             receive HTTP header line and turned off when last header line is
             found. At that point, if posted data (HTTP document) was completely
             in the same packet as the last header line, that data was not
             delivered until the next packet comes, which could never occur !
Mar 20, 2004 V5.14 Added partial support for RAW socket.
             To use RAW sockets, set Proto to 'raw_ip', 'raw_icmp', ...
             Set Port to '0' or whatever value is useful for the protocol.
             When using IP protocol, you can add option wsoSIO_RCVALL so that
             your program receive ALL datagrams when you listen on a given
             interface (You can't use 0.0.0.0).
             Do not use Connect with RAW socket. Always use Listen and then
             use SendTo to send datagrams use the socket.
             Added ReqVerHigh and ReqVerLow properties to be able to select
             which winsock version you want to load. Default to 1.1 but need
             2.2 for RAW sockets to be used.
Mar 24, 2004 V5.15 Changed WSocket_Synchronized_ResolveProto to hard code
             protocol number for tcp, udp and raw.
Apr 17, 2004 V6.00 New major release started. Move all platform and runtime
             dependencies to separate units. New base component for handling
             component with window handle.
Jun 20, 2004 V 5.16 John Mulvey <john@mulvey.eurobell.co.uk> fixed error message
             in GetPeerAddr which incorrectly reported an error about
             GetPeerName.
May 23, 2005 V5.17 PutDataInSendBuffer set bAllSent to false.
Jun 03, 2005 V5.18 Added SocketSndBufSize property which gives the size of
             winsock internal send buffer. When using TCP, you must make sure
             you never use a BufSize equal or greater than this value or
             you'll experience bad performances. See description in MSDN
             http://support.microsoft.com/default.aspx?scid=kb;en-us;823764
             Default value for BufSize is 1460 and SocketSndBufSize is 8192 so
             there is no problem when not changing those values.
Jun 18, 2005 V5.19 Made TCustomSocksWSocket.Connect accept 'tcp' as well as '6'
             for protocol. By Piotr "Hellrayzer" Dalek.
             Renamed event OnDisplay to OnDebugDisplay.
Sept 4, 2005 V5.20 added BufferedByteCount property used to ensure winsock has sent
             data, currently used in TFtpCli to check a put has finished correctly
             Thanks to Tobias Giesen <tobias@tgtools.de> for the fix
Dec 27, 2005 V6.00a Updated new release with change done in the old release.
Dec 31, 2005 V6.00b added new debug and logging event and log levels, replacing
             conditional debug code with optional code to avoid rebuilding apps.
             Works in combination with new component TIcsLogger.
             This is controlled by the new LogOptions property:
               loDestEvent - write to OnIcsLogEvent (called from higher level protocols)
               loDestFile - write to file debug_out.myprog.txt
               loDestOutDebug - write to OutputDebugString (shown in Debugger Event Log window)
               loAddStamp - time stamp each log line (accurate only to about 18ms)
               loWsockErr - log wsocket errors
               loWsockInfo - log wsocket general information
               loWsockDump - log wsocket data (not implemented yet)
               loSslErr - log SSL errors
               loSslInfo - log SSL general information
               loSslDump - log SSL packets and data
               loProtSpecErr - log protocol specific error
               loProtSpecInfo - log protocol specific general information
               loProtSpecDump - log protocol specific data and packets
Jan 22, 2006 V6.00c Added some KeepAlive stuff (seems winsock is bugged and
             doesn't care any setting done !).
Jan 28, 2006 V6.00d Gerhard Rattinger fixed SetKeepAliveOption for Delphi 3
Mar 09, 2006 V6.00e Arno made properties to select keepalive parameters.
             He also fixed ReverseDnsLookup to return a list of
             host names (aliases) instead of just the first entry. Added func.
             ReverseDnsLookupSync.
Apr 27, 2006 V6.00f Roger Tinembart <tinembart@brain.ch> added a critical section
             around the list of sendbuffers (FBufHandler) to avoid problems when
             the data is placed in the sendbuffer (for example with SendStr)
             by a different thread than the one that is effectively sending the
             data with TryToSend
June 11, 2006 V6.01 Use new TIcsBufferHandler.
Aug 06, 2006 V6.02 Angus added GetWinsockErr to give alpha and numeric winsock
             errors and improved many other error messages,
             and fixed FReadCount for 64-bit downloads
             added some EXTERNALSYM for BCB compatiblity
Aug 18, 2006 V6.03 Fixed a bug in ASyncReceive(). This bug caused data loss.
Oct 28, 2006 V6.04 Added setter for SocketSndBufSize and SocketRcvBufSize
Dec 22, 2006 V6.05 Oliver Grahl fixed SendLine to properly count LineEnd characters.
Jan 18, 2007 V6.06 Fixed constructor and DeleteBufferedData to behave correctly
             when an exception occur in AllocateSocketHWnd.
Mar 23, 2007 V6.07 Removed FD_CONNECT from dup().
Apr 04, 2007 V6.08 Arno Garrels updated SetKeepAliveOption
Mar 10, 2008 V6.09 Francois Piette & Arno Garrels made some changes to
                   prepare code for Unicode
                   WSocket_gethostname conversion from String to AnsiString
                   WSocketGetProc and WSocket2GetProc use AnsiString
                   GetAliasList simplified and use AnsiString
Apr 25, 2008 V6.10 A. Garrels, added some getters/setters to store and use some
             string-property-values as AnsiString internally.
             This reduced number of string casts with potential data loss to 17.
             These ansi-values are used to call winsock API that doesn't provide
             W functions. Modified depending code including some type changes
             from PChar to PAnsiChar. Made some casts Unicode => Ansi with
             potential data loss *explicit* casts (conditionally compiled) some
             unicode strings with only 7 bit ASCII characters are casted using
             new function UnicodeToAscii() in new unit OverbyteIcsUtils which
             should be fast and reliable and doesn't produce compiler warnings.
             Added new warning symbols.
Apr 30, 2008 V6.11 A. Garrels - Function names adjusted according to changes in
             OverbyteIcsLibrary.pas.
May 11, 2008 V6.12 USchuster removed local atoi implementation (atoi is now in
             OverbyteIcsUtils.pas)
May 15, 2008 V6.13 AGarrels type change of some published String properties
             to AnsiString, this is an attempt to avoid too many implicit
             string casts, only a few higher level components have been adjusted
             accordingly so far.
Jun 30, 2008 A.Garrels made some changes to prepare SSL code for Unicode.
Jul 04, 2008 V6.11 Rev.58 SSL - Still lacked a few changes I made last year.
Jul 13, 2008 V6.12 Added SafeWSocketGCount
Aug 03, 2008 V6.16 A. Garrels removed packed from record TExtension.
Jul 07, 2008 V6.17 Still a small fix from December 2007 missing in SSL code.
Aug 11, 2008 V6.18 A. Garrels - Type AnsiString rolled back to String.
             Two bugs fixed in SSL code introduced with Unicode change.
             Socks was not fully prepared for Unicode.
Sep 19, 2008 V6.19 A. Garrels changed some AnsiString types to RawByteString.
Sep 21, 2008 V6.20 A. Garrels removed BoolToStr(), available since D7
Oct 22, 2008 V7.21 A. Garrels removed the const modifier from parameter Data
             in function SendTo to fix a bug in C++ Builder.
Nov 03, 2008 V7.22 Added property Counter, a class reference to TWSocketCounter
             which provides some useful automatic counters. By default property
             Counter is unassigned and has to be enabled by a call to
             CreateCounter.
Apr 24, 2009 V7.23 A. Garrels added *experimental* OpenSSL engine support which
             is not compiled in by default. You have to uncomment conditional
             define OPENSSL_NO_ENGINE in OverbyteIcsSslDefs.inc and rebuild your
             packages to get it included. With engine support included a new
             published property AutoEnableBuiltinEngines of TSslContext has to
             be set to TRUE in order to enable OpenSSL's built-in hardware
             accelerators support, that's all.

             ******************************************************************
             * Due to the lack of hardware this feature is completely untested*
             ******************************************************************

             Any feedback and fixes are welcome, please contact the ICS mailing
             list. The OpenSSL engine documentation can be found here:
             http://openssl.org/docs/crypto/engine.html

             Additionally a new component TSslEngine is installed on the palette.
             Its purpose is to control (dynamic) engines.

             Typically control commands of an OpenSC dynamic pkcs11 engine
             (SmartCard) are :

             Cmds.Add('SO_PATH=d:\opensc\bin\engine_pkcs11.dll');
             Cmds.Add('ID=pkcs11');
             Cmds.Add('LIST_ADD=1');
             Cmds.Add('LOAD=');
             Cmds.Add('MODULE_PATH=d:\opensc\bin\opensc-pkcs11.dll');
             Cmds.Add('INIT='); <= Special ICS-control command to initialize the engine

             Sample test code (Dod couldn't get it working :(

             It assumes that the X509 certificate has been exported from
             the SmartCard to PEM file that is available in property
             SslCertFile. It's also assumed that SslEngine1 is created
             dynamically at run-time in this sample.
             We are in new event TSslContext.OnBeforeInit:

             if not Assigned(SslEngine1) then
             begin
                SslEngine1 := TSslEngine.Create(Self);
                try
                  SslEngine1.NameID := 'dynamic';

                  // The SmartCard holds the private key.
                  // Next two lines advise SslContext to load the key
                  // from the engine instead from PEM file.
                  TSslContext(Sender).CtxEngine := SslEngine1;
                  SslEngine1.CtxCapabilities := [eccLoadPrivKey];

                  // The PIN code is expected in property SslPassPhrase
                  TSslContext(Sender).SslPassPhrase := 'ics';

                  // Tell the engine which key to use.
                  SslEngine1.KeyID := KeyIdEdit.Text;

                  // At first open the engine
                  if not SslEngine1.Open then
                      raise Exception.Create(FEngine.LastErrorMsg);

                  // Now send our vendor specific control commands
                  for I := 0 to Cmds.Count -1 do
                  begin
                    if not SslEngine1.Control(Cmds.Names[I],
                                              Cmds.ValueFromIndex[I]) then
                        raise Exception.Create(SslEngine1.LastErrorMsg);
                  end;

                  Display('Engine set up and loaded successfully');
                except
                    FreeAndNil(SslEngine1);
                    raise;
                end;
             end;

Jun 12, 2009 V7.24 Angus added WriteCount property, how many bytes sent since
                     connection opened
                   Only reset ReadCount when connection opened, not closed
Jul 16, 2009 V7.25 Arno fixed and changed SetCounterClass()
Jul 19, 2009 V7.26 Arno - SSL code ignored FPaused flag, the change is in
                   TCustomSslWSocket.TriggerEvent.
Sep 04, 2009 V7.27 Set option TCP_NODELAY in Dup as well as provide a public
                   method to set this option, similar as suggested by
                   Samuel Soldat.
Sep 08, 2009 V7.28 Arno - Minor Unicode bugfix in TX509Base.GetExtension().
Sep 09, 2009 V7.29 Arno - Added new public methods TX509Base.WriteToBio() and
                   TX509Base.ReadFromBio(). Method SafeToPemFile got an arg.
                   that adds human readable certificate text to the output.
                   InitializeSsl inlined. Removed a Delphi 1 conditional.
Sep 17, 2009 V7.30 Anton Sviridov optimized setting of SSL options.
Sep 17, 2009 V7.31 Arno fixed a Unicode bug in TX509Base.GetExtension and
                   a general bug in TX509Base.GetSha1Hash (AnsiString as
                   digest buffer should really be avoided)
Sep 18, 2009 V7.32 Arno changed visibility of TX509Base.WriteToBio() and
                   TX509Base.ReadFromBio() to protected.
Nov 01, 2009 V7.33 Arno fixed a memory overwrite bug in
                   TCustomSocksWSocket.DoRecv().
Nov 07, 2009 V7.34 OpenSSL V0.9.8L disables session renegotiation due to
                   TLS renegotiation vulnerability.
Dec 20, 2009 V7.35 Arno added support for SSL Server Name Indication (SNI).
                   SNI has to be turned on in OverbyteIcsSslDefs.inc, see define
                   "OPENSSL_NO_TLSEXT". Exchanged symbol "NO_ADV_MT" in the
                   SSL source by "NO_SSL_MT" (This and SNI was sponsored by
                   Fastream Technologies).
                   SNI Howto: In SSL server mode assign event OnSslServerName,
                   it triggers whenever a client sent a server name in the TLS
                   client helo. From the event handler read public property
                   SslServerName, lookup and pass a matching, valid and
                   initialized SslContext instance associated with the server name.
                   In SSL client mode, if property SslServerName was not empty
                   this server name is sent to the server in the TLS client helo.
                   Currently IE 7 and FireFox >= V2 support SNI, note that both
                   browers don't send both "localhost" and IP addresses as
                   server names, this is specified in RFC.
Dec 24, 2009 V7.36 SSL SNI - Do not switch context if not initialized.
Dec 26, 2009 V7.37 Arno fixed TCustomSyncWSocket.ReadLine for Unicode. It
                   now takes an AnsiString buffer. Since this method is highly
                   deprecated it's also marked as "deprecated". Do not use it
                   in new applications.
May 08, 2010 V7.38 Arno Garrels added support for OpenSSL 0.9.8n. Read comments
                   in OverbyteIcsLIBEAY.pas for details.
May 16, 2010 V7.39 Arno Garrels reenabled check for nil in WMAsyncGetHostByName.
Jun 10, 2010 V7.40 Arno Garrels added experimental timeout and throttle feature
                   to TWSocket. Currently both features have to be enabled
                   explicitly with conditional defines BUILTIN_TIMEOUT
                   and/or BUILTIN_THROTTLE (see OverbyteIcsDefs.inc )
Aug 02, 2010 V7.41 Arno removed an option to send plain UTF-16 strings with
                   SendStr() and SendLine() by passing 1200 (CP_UTF16) in the
                   codepage parameter. Changed SendLine() to return correct
                   number of bytes written.
Aug 08, 2010 V7.42 FPiette prevented socket close in TCustomWSocket.Destroy when
                   socket state is wsInvalidState (this happend when an
                   exception is raise early in the constructor).
Sep 05, 2010 V7.43 Arno fixed a bug in the experimental throttle and timeout
                   source which made it impossible to use both features at the
                   same time. Renamed conditionals EXPERIMENTAL_THROTTLE and
                   EXPERIMENTAL_TIMEOUT to BUILTIN_THROTTLE and BUILTIN_TIMEOUT.
                   It's now possible to either enable them in OverbyteIcsDefs.inc
                   or define them in project options.
Sep 08, 2010 V7.44 Arno reworked the experimental timeout and throttle code.
                   Method names of TCustomTimeoutWSocket **changed**, they all
                   got prefix "Timeout". Removed the crappy TCustomTimerWSocket
                   class, both throttle and timeout use their own TIcsThreadTimer
                   instance now.
Sep 08, 2010 V7.45 Fixed a typo in experimental throttle code.
Sep 11, 2010 V7.46 Arno added two more SSL debug log entries and a call to
                   RaiseLastOpenSslError in TCustomSslWSocket.InitSSLConnection.
                   Added function OpenSslErrMsg.
Sep 23, 2010 V7.47 Arno fixed a bug in the experimental throttle code and made
                   it more accurate. Thanks to Angus for testing and reporting.
                   Method Resume with SSL enabled did not always work.
Oct 10, 2010 V7.48 Arno - MessagePump changes/fixes.
Oct 14, 2010 V7.49 Arno - Abort TCustomLineWSocket as soon as possible.
Oct 15, 2010 V7.50 Arno - Made function IsSslRenegotiationDisallowed available.
Oct 16, 2010 V7.51 Arno removed dummy ancestor TBaseParentWSocket, it was not
                   required to make D7's structure view happy.
Nov 08, 2010 V7.52 Arno improved final exception handling, more details
                   in OverbyteIcsWndControl.pas (V1.14 comments).
Dec 06, 2010 V7.53 Arno added thread-safe TSslContext.FreeNotification and
                   TSslContext.RemoveFreeNotification.
Dec 07, 2010 V7.54 Arno - TSslBaseComponent, thread-safe removal of IcsLogger's
                   free notification.
Feb 04, 2011 V7.55 Angus - allow BandwidthLimit to be changed during connection
Feb 09, 2011 V7.56 Arno added HTTP V1.1 transparent proxy support including
                   Basic and NTLM authentication implemented as
                   TCustomHttpTunnelWSocket. This feature is still in beta
                   state, mainly because I only tested against WinGate proxy
                   server. My goal was to make it as fast as possible, there
                   are no or just very few protected methods available to
                   override so far. Best performance is achieved with one of the
                   authentication types "htatNone", "htatBasic" or "htatNtlm".
                   With "htatDetect" the last successful authentication type is
                   cached until the proxy server name changes or the TWSocket
                   object is freed.
Feb 09, 2011 V7.57 Arno added public property HttpTunnelLastResponse to
                   TCustomHttpTunnelWSocket. HTTP status codes are no longer
                   triggered as is but added to a base of global const
                   "ICS_HTTP_TUNNEL_BASEERR".
Feb 13, 2011 V7.58 Arno - HTTP tunnel accepts LF as end-of-line marker.
                   In both SOCKS and HTTP tunnel trigger an error from
                   SessionClosed if the proxy drops the connection unexpectedly.
                   New functions to get error messages from error codes.
                   WSocketHttpTunnelErrorDesc, WSocketSocksErrorDesc
                   WSocketProxyErrorDesc, WSocketErrorMsgFromErrorCode
                   WSocketGetErrorMsgFromErrorCode. The latter two are general
                   purpose functions that currently try to translate winsock and
                   proxy error codes to string.
Feb 14, 2011 V7.59 Arno - SessionConnected triggered twice when a connection to
                   proxy could not be established, introduced in V7.58.
                   Added function WSocketIsProxyErrorCode(): Boolean.
Feb 14, 2011 V7.60 Arno - SOCKS4/SOCKS4A Unicode bug fixed in
                   TCustomSocksWSocket.SocksDoConnect.
Feb 15, 2011 V7.61 Arno - Workaround a false version number (5) in WinGate's
                   SOCKS5 authentication response when user credentials are
                   wrong (reports an authentication error now rather than a
                   SOCKS version error). WSocketProxyErrorDesc and
                   WSocketGetErrorMsgFromErrorCode added a hyphen as separator
                   to the message. Removed a string cast warning.
Feb 16, 2011 V7.62 Arno fixed a memory overwrite bug in TCustomHttpTunnelWSocket
                   DataAvailable. Improved keep-alive handling and removed the
                   hard check for correct HTTP version. It still supports
                   HTTP/1.1 only, however is now partly compatible with 3Proxy.
                   It has been tested successfully with WinGate, Sambar,
                   Fastream IQ Proxy Server and 3Proxy. Note that detection of
                   authentication type works only with persistent connections,
                   with 3Proxy you have to explicitly set the authentication type.
Feb 20, 2011 V7.63 Arno added digest authentication to TCustomHttpTunnelWSocket.
                   Define "NO_HTTP_TUNNEL_AUTHDIGEST" to exclude it from a build,
                   so far it's tested with ICS-based IQ Proxy Server only. Some
                   new property setters.
Feb 21, 2011 V7.64 Arno - New ComponentOption "wsoNoHttp10Tunnel" treats HTTP/1.0
                   responses as errors. If "wsoNoHttp10Tunnel" is not set send
                   "Keep-Alive" header with NTLM message #1 in order to make
                   HTTP/1.0 proxies happy (MS Proxy Server 2.0 tested).
Feb 26, 2011 V7.65 Arno - TCustomHttpTunnelWSocket strongly improved.
                 - Bugfix: Ensure that internally buffered application data
                   is read by the upper layer by posting a fake FD_READ message
                   if required.
                 - Better support of HTTP/1.0 proxies.
                 - Performs an internal reconnect when the connection has to
                   be closed after a 407 status code and the server provides a
                   supported authentication method, as a result htatDetect
                   works with non-persistent connections as well.
                 - Tested with Squid 2.7.STABLE8 for Windows successfully.
                 - Tested with CSM proxy 4.2 successfully after adding a
                   workaround (requires at least two header lines being sent).
                 - ISA Server 2006 tested successfully except with digest auth.
                   I wasn't able to get digest authentication working neither IE
                   nor Firefox got authenticated. Interesting is that ISA uses
                   digest algorithm "MD5-sess" that I enabled in
                   OverbyteIcsDigestAuth.pas now, untested!
Feb 28, 2011 V7.66 Arno - TCustomHttpTunnelWSocket bugfix, do not explicitly
                   close the connection in HttpTunnelTriggerResultOrContinue
                   when FD_CLOSE notification from winsock has been received yet.
Feb 28, 2011 V7.67 Arno - TCustomHttpTunnelWSocket HttpTunnelGetNtlmMessage3,
                   send challenge domain name only if user code doesn't
                   include one.
Mar 16, 2011 V7.68 Anton S. added two debug messages.
Mar 21, 2011 V7.69 Method Abort no longer triggers an exception nor event OnError
                   if a call to winsock API WSACancelAsyncRequest in method
                   CancelDnsLookup failed for some reason.
Apr 10, 2011 V7.70 Arno added property SslVerifyFlags to the TSslContext.
                   This enables the component user to include CRLs added
                   thru SslCRLFile and SslCRLPath in the certificate verification
                   process. However enabling CRL-checks needs some additional
                   action in the OnSslVerifyPeer event since OpenSSL will the
                   trigger any error related to CRLs. If there's, for instance,
                   no CRL available or it has expired etc.. You find the possible
                   error codes in OverbyteIcsLibeay.pas search i.e. for
                   X509_V_ERR_UNABLE_TO_GET_CRL.
Apr 15, 2011 V7.71 Arno prepared for 64-bit.
Apr 21, 2011 V7.72 Éric Fleming Bonilha found a bug in SetSocketRcvBufSize.
Apr 23, 2011 V7.73 Arno added support for OpenSSL 0.9.8r and 1.0.0d.
Apr 24, 2011 V7.74 Arno fixed compatibility with OpenSSL 1.0.0d.
Apr 26, 2011 V7.75 Anton S. found that TrashCanSize was not set correctly in
                   non .NET environments, only important if OnDataAvailable
                   is not assigned with non-listening sockets, which should
                   never be the case.
May 03, 2011 V7.76 Arno improved error messages on loading OpenSSL libs.
                   Added method Insert to TX509List. Removed a few useless
                   type casts.
May 08, 2011 v7.77 Arno added TX509List.SortChain and use of new function
                   f_ERR_remove_thread_state(nil) with OpenSSL v1.0.0+
May 11, 2011 v7.78 Arno made the SSL peer certificate available as property
                   TSslWSocket.SslPeerCert. The SslPeerCert is instantiated
                   lazily once and usable after a SSL handshake including
                   the SSL session was reused from cache until the next SSL
                   connection is initialized. It is usable when SslPeerCert.X508
                   is non-nil. In previous versions the PeerCert passed to
                   OnSslHandshakeDone could be just a reference to one of the
                   certs in the SslCertChain which was eval.
May 17, 2011 v7.79 Arno added Sha1Hex, Sha1Digest, IssuedBy, IssuerOf and
                   SelfSigned to TX509Base. Deprecated Sha1Hash since it was
                   buggy storing binary in strings. Return values of Sha1Hex
                   and Sha1Digest are cached. Reworked TX509List and added new
                   methods.
Jun 08, 2011 v7.80 Arno added x64 assembler routines, untested so far.
Jun 18, 2011 v7.81 aguser removed some compiler hints.

May 21, 2011 V7.82 Arno - Make sure receipt of a SSL shutdown notification
                   closes the connection if no bidirectional SSL shutdown is
                   wanted. There are servers in the wild expecting a SSL
                   shutdown confirmation before they close the connection.
Jul 22, 2011 V7.83 Arno - OEM NTLM changes.
Sep 26, 2011 V7.84 Angus - Set SocketSndBufSize and SocketRcvBufSize for
                   Listen sockets, note only worth increasing sizes for UDP
Feb 17, 2012 V7.86 Arno added NTLMv2 and NTLMv2 session security (basics),
                   read comment "HowTo NTLMv2" in OverbyteIcsNtlmMsgs.pas.
Apr 30, 2012 V7.87 Arno - Some SSL debug log strings adjusted.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
                   added property SocketFamily, sfAny = System default preference,
                     sfAnyIPv4 = IPv4 preference, sfAnyIPv6 = IPv6 preference,
                     sfIPv4 = explicit IPv4, sfIPv6 = explicit IPv6.
                   added functions WSocketIPv4ToStr, WSocketIPv6ToStr,
                     WSocketStrToIPv4, WSocketStrToIPv6, WSocketStrToMappedIPv4,
                     WSocketIsIPv4, WSocketIsIP (finds SocketFamily from string)
Aug 5, 2012 V8.01 - Angus added WSocketIsIPEx (finds SocketFamily from string,
                     including AnyIPv4/IPv6), added SocketFamilyNames
Feb 16. 2013 V8.02 Angus - WSocketResolveIp no exception for IPv6 lookups
Mar 16, 2013 V8.03 Arno added new property LocalAddr6. This is a breaking change
                   if you ever assigned some IPv6 to property LocalAddr in
                   existing code. LocalAddr6 should be assigned a local IPv6,
                   LocalAddr should be assigned a local IPv4, provided it is
                   actually required to bind the socket to a particular interface.
                   By default both properties LocalAddr and LocalAddr6 do not
                   require a value.
Jun 03, 2013 V8.04 FPiette added unit "Types" so that some inlines are
                   expanded.
Jun 03, 2013 V8.05 Eric Fleming Bonilha found a serious bug with closing the
                   socket. The problem was that winsock may continue to post
                   notification messages after closesocket() has been called.
Aug 18, 2013 V8.06 Arno added some default property specifiers.
Oct 22. 2013 V8.07 Angus - Added SendTo6 and ReceiveFrom6 for IPv6 UDP
Dec 24. 2013 V8.08 Francois - fixed range check error in various PostMessages
Feb 12, 2014 V8.09 Angus - fixed TX509Base.PostConnectionCheck to check multiple
                   DNS or IP entries
Jul 9, 2014  V8.10 Angus - added sslCiphersXXX literals some taken from Mozilla
                   with much higher security for servers
Aug 15, 2014 V8.11 Angus made WriteCount public in descendent components
Oct 20, 2014 V8.12 Angus added sslCiphersMozillaSrvInter which excludes SSLv3
Nov 8,  2014 V8.13 Eugene Kotlyarov added namespace for all RTL units for XE2 and
                    later, note only this unit has bumped version
Dec 11, 2014 V8.14 Angus made LastOpenSslErrMsg public for better error reporting (dump=true)
                   Added SslHandshakeRespMsg with friendly error or success message
                   Added SslHandshakeErr with full SSL error code (event error is reason only)
                   Added SslCipherDesc with OpenSsl long cipher description
                   Added SslEncryption, SslKeyExchange and SslMessAuth extracted from SslCipherDesc
                   Stop SSL reporting handshaking steps as errors and report real SSL errors
                   Other various SSL error reporting improvements
}

{
About multithreading and event-driven:
    TWSocket is a pure asynchronous component. It is non-blocking and
    event-driven. It means that when you request an operation such as connect,
    the component start the operation your requested and give control back
    immediately while performing the operation in the background automatically.
    When the operation is done, an event is triggered (such as
    OnSessionConnected if you called Connect).

    This asynchronous non-blocking behaviour is very high performance but a
    little bit difficult to start with. For example, you can't call Connect and
    immediately call SendStr the line below. If you try, you'll have an
    exception triggered saying you are not connected. Calling connect will start
    connection process but will return long before connection is established.
    Calling SendStr at the next line will not work because the socket is not
    connected yet. To make it works the right way, you have to put your SendStr
    in the OnSessionConnected event.

    The asynchronous operation allows you to do several TCP/IP I/O
    simultaneously. Just use as many component as you need. Each one will
    operate independently of the other without blocking each other ! So you
    basically don't need multi-threading with TWSocket, unless YOUR processing
    is lengthy and blocking.

    If you have to use multithreading, you have two possibilities:
    1) Create your TWSocket from your thread's Execute method
    2) Attach a TWSocket to a given thread using ThreadAttach.
    In both cases, you must set MultiThreaded property to TRUE.
    If you don't use one of those methods, you'll end up with a false
    multithreaded program: all events will be processed by the main tread !
    For both methods to work, you MUST have a message loop withing your thread.
    Delphi create a message loop automatically for the main thread (it's in
    the Forms unit), but does NOT create one in a thread ! For your convenience,
    TWSocket has his own MessageLoop procedure. You can use it from your thread.

    Sample program MtSrv uses first method while ThrdSrv uses second method.
    Sample program TcpSrv is much the same as ThrdSrv but doesn't use any
    thread. You'll see that it is able to server a lot of simultaneous clients
    as well and it is much simpler.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsWSocket;
{$ENDIF}

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }
{$J+}           { Allow typed constant to be modified }
{$ALIGN 8}
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF USE_SSL}
    {$I Include\OverbyteIcsSslDefs.inc}
{$ENDIF}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
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
{$IFDEF CPUX64}
  {.$DEFINE PUREPASCAL}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Types{$ELSE}Types{$ENDIF},
  OverbyteIcsWinsock,
{$ENDIF}
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Contnrs{$ELSE}Contnrs{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SyncObjs{$ELSE}SyncObjs{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SysConst{$ELSE}SysConst{$ENDIF},
{$IFDEF POSIX}
  System.Generics.Collections,
  Posix.Pthread,
  Posix.SysTypes,
  Posix.Base, Posix.Errno,
  Posix.SysSocket,
  Posix.NetinetIn,
  Posix.NetinetTCP,
  {Posix.SysSelect}
  Posix.SysTime,
  Posix.ArpaInet,
  Posix.NetDB,
  Posix.UniStd,
  Posix.Fcntl,
{$WARN UNIT_PLATFORM OFF}
  Posix.StrOpts,
{$WARN UNIT_PLATFORM ON}
  Ics.Posix.Messages,
  Ics.Posix.KEventTypes,
  Ics.Posix.KEventApi,
  Ics.Posix.WinTypes,
{$ENDIF}
{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
  OverbyteIcsSSLEAY, OverbyteIcsLIBEAY,
  {$IFDEF RTL_NAMESPACES}System.Masks{$ELSE}Masks{$ENDIF}, { Masks added AG 06/20/07 }
{$ENDIF}
{$IFNDEF NO_DEBUG_LOG}
  OverbyteIcsLogger,
{$ENDIF}
  OverbyteIcsUtils, OverbyteIcsAvlTrees,
{$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
  OverbyteIcsDigestAuth,
{$ENDIF}
{$IFDEF FMX}
  {$IF DEFINED(BUILTIN_THROTTLE) or DEFINED(BUILTIN_TIMEOUT)}
    Ics.Fmx.OverbyteIcsThreadTimer,
  {$IFEND}
  Ics.Fmx.OverbyteIcsWndControl,
{$ELSE FMX} // VCL
  {$IF DEFINED(BUILTIN_THROTTLE) or DEFINED(BUILTIN_TIMEOUT)}
    OverbyteIcsThreadTimer,
  {$IFEND}
  OverbyteIcsWndControl,
{$ENDIF FMX}
  OverbyteIcsNtlmMsgs,
  OverbyteIcsWSockBuf,
  OverbyteIcsMimeUtils,
  OverbyteIcsTypes;

type
  { sfAny = System default preference, sfAnyIPv4 = IPv4 preference,            }
  { sfAnyIPv6 = IPv6 preference, sfIPv4 = explicit IPv4, sfIPv6 = explicit IPv6 }
  TSocketFamily = (sfAny, sfAnyIPv4, sfAnyIPv6, sfIPv4, sfIPv6);

const
  WSocketVersion            = 814;
  CopyRight    : String     = ' TWSocket (c) 1996-2014 Francois Piette V8.14 ';
  WSA_WSOCKET_TIMEOUT       = 12001;
  DefaultSocketFamily       = sfIPv4;

{$IFDEF POSIX}
  {.$DEFINE NO_ADV_MT}
const
  {$IFDEF LINUX} // ?
    FIONREAD                    = $541B;
    FIONBIO                     = $5421;
  {$ENDIF}
  {$IFDEF MACOS}
    FIONREAD                    = $4004667F;
    FIONBIO                     = $8004667E;
  {$ENDIF}
    INVALID_SOCKET              = -1;
    INVALID_FILE_HANDLE         = -1;
    SOCKET_ERROR                = -1;
    INADDR_NONE                 = $FFFFFFFF;
    //MSG_NOSIGNAL                = 0;

    FD_READ                     = $0001;
    FD_WRITE                    = $0002;
   // FD_OOB                      = $0004; Not implemented
    FD_ACCEPT                   = $0008;
    FD_CONNECT                  = $0010;
    FD_CLOSE                    = $0020;
  // winsock 2 not working
   { FD_QOS                      = $0040;
    FD_GROUP_QOS                = $0080; }
    FD_ROUTING_INTERFACE_CHANGE = $100;
    FD_ADDRESS_LIST_CHANGE      = $200;
    FD_MAX_EVENTS               = 10;

    { Socket error codes mapped }
    WSABASEERR                  = 0;//10000;
    WSAHOST_NOT_FOUND           = EPERM;                   // EAI_NONAME
    WSATRY_AGAIN                = ENOENT;                  // EAI_AGAIN
    WSANO_RECOVERY              = ESRCH;                   // EAI_FAIL
    WSAEINTR                    = EINTR;
    WSASERVICE_NOT_FOUND        = ENOEXEC;                 // EAI_SERVICE
    WSAEBADF                    = EBADF;
    WSAEACCES                   = EACCES;
    WSAEFAULT                   = EFAULT;
    WSAEINVAL                   = EINVAL;                  // EAI_BADFLAGS
    WSAEMFILE                   = EMFILE;
    WSAEWOULDBLOCK              = EWOULDBLOCK;
    WSAEINPROGRESS              = EINPROGRESS;
    WSAEALREADY                 = EALREADY;
    WSAENOTSOCK                 = ENOTSOCK;
    WSAEDESTADDRREQ             = EDESTADDRREQ;
    WSAEMSGSIZE                 = EMSGSIZE;
    WSAEPROTOTYPE               = EPROTOTYPE;
    WSAENOPROTOOPT              = ENOPROTOOPT;
    WSAEPROTONOSUPPORT          = EPROTONOSUPPORT;
    WSAESOCKTNOSUPPORT          = ESOCKTNOSUPPORT;
    WSAEOPNOTSUPP               = EOPNOTSUPP;
    WSAEPFNOSUPPORT             = EPFNOSUPPORT;
    WSAEAFNOSUPPORT             = EAFNOSUPPORT;            // EAI_FAMILY
    WSAEADDRINUSE               = EADDRINUSE;
    WSAEADDRNOTAVAIL            = EADDRNOTAVAIL;
    WSAENETDOWN                 = ENETDOWN;
    WSAENETUNREACH              = ENETUNREACH;
    WSAENETRESET                = ENETRESET;
    WSAECONNABORTED             = ECONNABORTED;
    WSAECONNRESET               = ECONNRESET;
    WSAENOBUFS                  = ENOBUFS;
    WSAEISCONN                  = EISCONN;
    WSAENOTCONN                 = ENOTCONN;
    WSAESHUTDOWN                = ESHUTDOWN;
    WSAETOOMANYREFS             = ETOOMANYREFS;
    WSAETIMEDOUT                = ETIMEDOUT;
    WSAECONNREFUSED             = ECONNREFUSED;
    WSAELOOP                    = ELOOP;
    WSAENAMETOOLONG             = ENAMETOOLONG;
    WSAEHOSTDOWN                = EHOSTDOWN;
    WSAEHOSTUNREACH             = EHOSTUNREACH;
    WSAENOTEMPTY                = ENOTEMPTY;
    WSAEPROCLIM                 = EPROCLIM;
    WSAEUSERS                   = EUSERS;
    WSAEDQUOT                   = EDQUOT;
    WSAESTALE                   = ESTALE;
    WSAEREMOTE                  = EREMOTE;
    WSANO_DATA                  = ENODATA;

   { WSA startup etc
    WSASYSNOTREADY              = ELAST + 1;
    WSAVERNOTSUPPORTED          = ELAST + 2;
    WSANOTINITIALISED           = ELAST + 3; }

    WSAELAST                    = WSANO_DATA;

    MAXGETHOSTSTRUCT      = 1024;
    IPV6_ADD_MEMBERSHIP   = IPV6_JOIN_GROUP;

    IsIPv6APIAvailable    = True;
    IsIPv6Available       = True;

type
    TSocket               = Integer;
    PSocket               = ^TSocket;
    TSockAddr             = sockaddr_in;//sockaddr; as in windows
    PSockAddr             = ^TSockAddr;
    TInAddr               = in_addr;
    PInAddr               = Pin_addr;
    TSockAddrIn           = sockaddr_in;
    PSockAddrIn           = Psockaddr_in;
    TSockAddrIn6          = sockaddr_in6;
    PSockAddrIn6          = Psockaddr_in6;
    TInAddr6              = in6_addr;
    PInAddr6              = Pin6_addr;
    TAddrInfo             = addrinfo;
    TIpv6MReq             = ipv6_mreq;

    u_short               = UInt16;
    u_long                = UInt32;
    u_int                 = UInt32;
    TLinger               = linger;

    SunB = packed record
      s_b1, s_b2, s_b3, s_b4: Byte;
    end;

    SunW = packed record
      s_w1, s_w2: Word;
    end;

    TIcsInAddr = record
      case integer of
        0: (S_un_b: SunB);
        1: (S_un_w: SunW);
        2: (S_addr: u_long);
    end;
    PIcsInAddr = ^TIcsInAddr;

    TPipeFd = record
      Read  : Integer;
      Write : Integer;
    end;
    PPipeFd = ^TPipeFd;

    TTimeVal = timeval;
    PTimeval = ^TTimeVal;

    IN6_ADDR = record
    case Integer of
        0: (Byte     : array [0..15] of AnsiChar);
        1: (Word     : array [0..7]  of Word);
        2: (s6_bytes : array [0..15] of Byte);
        3: (s6_addr  : array [0..15] of Byte);
        4: (s6_words : array [0..7]  of Word);
    end;
    TIn6Addr   = IN6_ADDR;
    PIn6Addr   = ^TIn6Addr;
    PIN6_ADDR  = PIn6Addr;

function IN6_ADDR_EQUAL(const a: PIn6Addr; const b: PIn6Addr): Boolean;
function IN6ADDR_ISANY(sa: PSockAddrIn6): Boolean;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
type
    TIcsInAddr = TInAddr;
    PIcsInAddr = ^TIcsInAddr;
{$ENDIF}

type
  { C++Builder < 2010 (2009?) cannot wrap an array as record with methods as }
  { function result. So changed type to a record :(                          }
  //TIcsIPv6Address    = array [0..7] of Word;
  TIcsIPv6Address = record
    Words : array [0..7] of Word;
  end;
  PIcsIPv6Address    = ^TIcsIPv6Address;
  TIcsIPv4Address    = {$IFDEF POSIX} LongWord {$ELSE} Integer {$ENDIF};
  PIcsIPv4Address    = ^TIcsIPv4Address;
  TWndMethod         = procedure(var Message: TMessage) of object;
  ESocketException   = class(Exception);
  TBgExceptionEvent  = TIcsBgExceptionEvent; { V7.35 }
  TSocketState       = (wsInvalidState,
                        wsOpened,     wsBound,
                        wsConnecting, wsSocksConnected, wsConnected,
                        wsAccepting,  wsListening,
                        wsClosed);
  TSocketSendFlags   = (wsSendNormal, wsSendUrgent);
  TSocketLingerOnOff = (wsLingerOff, wsLingerOn, wsLingerNoSet);
  TSocketKeepAliveOnOff = (wsKeepAliveOff, wsKeepAliveOnCustom,
                           wsKeepAliveOnSystem);
const
  SocketStateNames: array [TSocketState] of PChar = ('Invalid', 'Opened', 'Bound',
      'Connecting', 'SocksConnected', 'Connected', 'Accepting', 'Listening',
      'Closed');
  SocketFamilyNames: array [TSocketFamily] of PChar = ('Any', 'AnyIPv4', 'AnyIPv6',  { V8.01 }
      'IPv4', 'IPv6');
type
  TNetChangeEvent    = procedure (Sender: TObject; ErrCode: Word) of object;
  TDataAvailable     = procedure (Sender: TObject; ErrCode: Word) of object;
  TDataSent          = procedure (Sender: TObject; ErrCode: Word) of object;
  TSendData          = procedure (Sender: TObject; BytesSent: Integer) of object;
  TSessionClosed     = procedure (Sender: TObject; ErrCode: Word) of object;
  TSessionAvailable  = procedure (Sender: TObject; ErrCode: Word) of object;
  TSessionConnected  = procedure (Sender: TObject; ErrCode: Word) of object;
  TDnsLookupDone     = procedure (Sender: TObject; ErrCode: Word) of object;
  TChangeState       = procedure (Sender: TObject;
                                 OldState, NewState : TSocketState) of object;
  TDebugDisplay      = procedure (Sender: TObject; var Msg : String) of object;
  TWSocketSyncNextProc = procedure of object;
  TWSocketOption       = (wsoNoReceiveLoop, wsoTcpNoDelay, wsoSIO_RCVALL,
                         { The HTTP tunnel supports HTTP/1.1. If next option }
                         { is set HTTP/1.0 responses are treated as errors.  }
                          wsoNoHttp10Tunnel,
                          wsoNotifyAddressListChange,
                          wsoNotifyRoutingInterfaceChange);
  TWSocketOptions      = set of TWSocketOption;

  TTcpKeepAlive = packed record
    OnOff             : u_long;
    KeepAliveTime     : u_long;
    KeepAliveInterval : u_long;
  end;

type  { <== Required to make D7 code explorer happy, AG 05/24/2007 }

  TWSocketCounter = class(TObject)
  private
    FConnectDT    : TDateTime;
    FConnectTick  : Cardinal;
    FLastRecvTick : Cardinal;
    FLastSendTick : Cardinal;
    function  GetLastAliveTick : Cardinal;
  public
    procedure SetConnected; virtual;
    property  ConnectTick   : Cardinal  read FConnectTick  write FConnectTick;
    property  ConnectDT     : TDateTime read FConnectDT    write FConnectDT;
    property  LastAliveTick : Cardinal  read GetLastAliveTick;
    property  LastRecvTick  : Cardinal  read FLastRecvTick write FLastRecvTick;
    property  LastSendTick  : Cardinal  read FLastSendTick write FLastSendTick;
  end;
  TWSocketCounterClass = class of TWSocketCounter;

{$IFDEF POSIX}
  TIcsAsyncEventState = set of (aesCloseNotified, aesConnectNotified, aesShutDown0Called, aesShutDown1Called);
  IIcsEventSource = interface(IInterface) { AG 8.11.2011 }
  ['{EDA1AB33-D3F0-4C14-AA99-67E6D28A38F3}']
    function  GetEventMask: LongWord;
    procedure SetEventMask(const AValue: LongWord);
    function  GetNotifyMessageID: UINT;
    function  GetNotifyWindow: HWND;
    function  GetEventState: TIcsAsyncEventState;
    function  GetFileDescriptor: Integer;
    procedure SetFileDescriptor(const AValue: Integer);
    function  GetObject: TObject;
    procedure SetEventState(const AValue: TIcsAsyncEventState);
    procedure SetNotifyWindow(const AValue: HWND);
    procedure SetNotifyMessageID(const AValue: UINT);
    function  GetObjectID: NativeInt; // Must be > 0 and unique!!

    property  EventMask: LongWord read GetEventMask write SetEventMask;
    property  NotifyMessageID: UINT read GetNotifyMessageID write SetNotifyMessageID;
    property  NotifyWindow: HWND read GetNotifyWindow write SetNotifyWindow;
    property  EventState: TIcsAsyncEventState read GetEventState write SetEventState;
    property  FileDescriptor: Integer read GetFileDescriptor write SetFileDescriptor;
    property  ObjectID: NativeInt read GetObjectID; // Must be > 0 and unique!!
  end;
{$ENDIF POSIX}

  TCustomWSocket = class(TIcsWndControl {$IFDEF POSIX}, IIcsEventSource {$ENDIF})
  private
    FSocketFamily       : TSocketFamily;
    FOldSocketFamily    : TSocketFamily;
    FCurrentAddrFamily  : Word; // Addr family cache
    Fsin                : TSockAddrIn6;
    FDnsResult          : String;
    FDnsResultList      : TStrings;
    FSendFlags          : Integer;
    FLastError          : Integer;
    //FWindowHandle       : HWND;
  {$IFDEF MSWINDOWS}
    FDnsLookupBuffer    : array [0..MAXGETHOSTSTRUCT] of AnsiChar;
  {$ENDIF}
    FDnsLookupCheckMsg  : Boolean;
    FDnsLookupTempMsg   : TMessage;
    // FHandle             : HWND;
    FCounter            : TWSocketCounter;
    FCounterClass       : TWsocketCounterClass;
    { ThreadID at the time of the first call to one of the IcsAsyncXxxx methods}
    FLookupThreadID     : THandle;
    { Pointer to a TIcsAsyncDnsLookup instance shared by all TWSocket instances}
    { which called one of the IcsAsyncXxxx methods from the same thread context}
    FAsyncLookupPtr     : Pointer;
{$IFDEF POSIX} { IIcsEventSource }
  strict private
    FPxEventMask        : LongWord;
    FPxFileDescriptor   : Integer;
    FPxEventState       : TIcsAsyncEventState;
    FPxEventMessageID   : UINT;
    FPxEventWindow      : HWND;
    FPxObjectID         : NativeInt;
    function  GetEventMask: LongWord;
    procedure SetEventMask(const AValue: LongWord);
    function  GetNotifyMessageID: UINT;
    procedure SetNotifyMessageID(const AValue: UINT);
    function  GetNotifyWindow: HWND;
    function  GetEventState: TIcsAsyncEventState;
    function  GetFileDescriptor: Integer;
    procedure SetFileDescriptor(const AValue: Integer);
    function  GetObject: TObject;
    procedure SetEventState(const AValue: TIcsAsyncEventState);
    procedure SetNotifyWindow(const AValue: HWND);
    function  GetObjectID: NativeInt;
{$ENDIF POSIX IIcsEventSource}
  protected
    FHSocket            : TSocket;
    FASocket            : TSocket;               { Accepted socket }

    FMsg_WM_ASYNCSELECT            : UINT;
    FMsg_WM_ASYNCGETHOSTBYNAME     : UINT;
    FMsg_WM_ASYNCGETHOSTBYADDR     : UINT;
    FMsg_WM_CLOSE_DELAYED          : UINT;
    //FMsg_WM_WSOCKET_RELEASE      : UINT;
    FMsg_WM_TRIGGER_EXCEPTION      : UINT;
    FMsg_WM_TRIGGER_DATA_AVAILABLE : UINT;
    FAddrStr            : String;
    FAddrResolved       : Boolean;
    FAddrFormat         : Integer;
    FAddrAssigned       : Boolean;
    FProto              : Integer;
    FProtoAssigned      : Boolean;
    FProtoResolved      : Boolean;
    FLocalPortResolved  : Boolean;
    FProtoStr           : String;
    FPortStr            : String;
    FPortAssigned       : Boolean;
    FPortResolved       : Boolean;
    FPortNum            : Integer;
    FLocalPortStr       : String;
    FLocalPortNum       : Integer;
    FLocalAddr          : String;     { IP address for local interface to use }
    FLocalAddr6         : String;     { IPv6 address for local interface to use }
    FType               : Integer;
    FBufHandler         : TIcsBufferHandler;
    FLingerOnOff        : TSocketLingerOnOff;
    FLingerTimeout      : Integer;              { In seconds, 0 = disabled }
    FKeepAliveOnOff     : TSocketKeepAliveOnOff;
    FKeepAliveTime      : Integer;              { In milliseconds }
    FKeepAliveInterval  : Integer;              { In milliseconds }
    FListenBacklog      : Integer;
    ReadLineCount       : Integer;
    bAllSent            : Boolean;
    FReadCount          : Int64;   { V5.26 }
    FWriteCount         : Int64;   { V7.24 }
    FPaused             : Boolean;
    FCloseInvoked       : Boolean;
    FBufferedByteCount  : LongInt;   { V5.20 how man xmit bytes unsent        }
    FFlushTimeout       : Integer;   { This property is not used anymore      }
    FDnsLookupHandle    : THandle;
    { More info about multicast can be found at:                              }
    {    http://ntrg.cs.tcd.ie/undergrad/4ba2/multicast/antony/               }
    {    http://www.tldp.org/HOWTO/Multicast-HOWTO-6.html                     }
    FMultiCast          : Boolean;
    { Multicast addresses consists of a range of addresses from 224.0.0.0 to  }
    { 239.255.255.255. However, the multicast addresses from 224.0.0.0 to     }
    { 224.0.0.255 are reserved for multicast routing information; Application }
    { programs should use multicast addresses outside this range.             }
    FMultiCastAddrStr   : String;
    FMultiCastIpTTL     : Integer;
    FReuseAddr          : Boolean;
    FComponentOptions   : TWSocketOptions;
    FState              : TSocketState;
    FRcvdFlag           : Boolean;
    FSelectEvent        : LongInt;
    FSelectMessage      : WORD;
    FOnSessionAvailable : TSessionAvailable;
    FOnSessionConnected : TSessionConnected;
    FOnSessionClosed    : TSessionClosed;
    FOnChangeState      : TChangeState;
    FOnDataAvailable    : TDataAvailable;
    FOnDataSent         : TDataSent;
    FOnSendData         : TSendData;
    { FOnLineTooLong      : TNotifyEvent; }
    FOnDnsLookupDone    : TDnsLookupDone;
    FOnError            : TNotifyEvent;
    FOnDebugDisplay     : TDebugDisplay;       { 18/06/05 }
    //FThreadId           : THandle;
    FSocketSndBufSize   : Integer;  { Winsock internal socket send buffer size }
    FSocketRcvBufSize   : Integer;  { Winsock internal socket Recv buffer size }
    FOnAddressListChanged : TNetChangeEvent;
    FOnRoutingInterfaceChanged : TNetChangeEvent;
{$IFNDEF NO_DEBUG_LOG}
    FIcsLogger          : TIcsLogger;                                           { V5.21 }
    procedure   SetIcsLogger(const Value : TIcsLogger); virtual;                { V5.21 }
    procedure   DebugLog(LogOption : TLogOption; const Msg : String); virtual;  { V5.21 }
    function    CheckLogOptions(const LogOption: TLogOption): Boolean; virtual; { V5.21 }
{$ENDIF}
    procedure   AbortComponent; override; { V7.35 }
    procedure   WndProc(var MsgRec: TMessage); override;
    function    MsgHandlersCount: Integer; override;
    procedure   AllocateMsgHandlers; override;
    procedure   FreeMsgHandlers; override;
    procedure   AllocateSocketHWnd; virtual;
    procedure   DeallocateSocketHWnd; virtual;
    procedure   SocketError(sockfunc: String; LastError: Integer = 0);
    procedure   WMASyncSelect(var msg: TMessage); virtual;
    procedure   WMAsyncGetHostByName(var msg: TMessage);
    procedure   WMAsyncGetHostByAddr(var msg: TMessage);
    procedure   WMCloseDelayed(var msg: TMessage);
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure   ChangeState(NewState : TSocketState);
    procedure   TryToSend; virtual;
    procedure   ASyncReceive(Error : Word; MySocketOptions : TWSocketOptions);
    procedure   AssignDefaultValue; virtual;
    procedure   InternalClose(bShut : Boolean; Error : Word); virtual;
    procedure   InternalAbort(ErrCode : Word); virtual;
    procedure   InternalCancelDnsLookup(IgnoreErrors: Boolean);
    procedure   SetSendFlags(newValue : TSocketSendFlags);
    function    GetSendFlags : TSocketSendFlags;
    procedure   SetAddr(const InAddr : String);
    procedure   SetCounterClass(const Value: TWSocketCounterClass);
    procedure   SetRemotePort(sPort : String); virtual;
    function    GetRemotePort : String;
    procedure   SetLocalAddr(const sLocalAddr : String);
    procedure   SetLocalAddr6(const sLocalAddr6 : String);
    procedure   SetMultiCastAddrStr(const sMultiCastAddrStr: String);
    procedure   SetLocalPort(const sLocalPort : String);
    procedure   SetProto(sProto : String); virtual;
    procedure   SetSocketFamily(const Value: TSocketFamily);
    procedure   SetOnRoutingInterfaceChanged(const Value: TNetChangeEvent);
    procedure   SetOnAddressListChanged(const Value: TNetChangeEvent);
    function    GetRcvdCount : LongInt; virtual;
    procedure   SetBufSize(Value : Integer); virtual;
    function    GetBufSize: Integer; virtual;
    procedure   SetSocketRcvBufSize(BufSize : Integer); virtual;
    procedure   SetSocketSndBufSize(BufSize : Integer); virtual;
    procedure   BindSocket; virtual;
    procedure   SendText(const Str : RawByteString); {$IFDEF COMPILER12_UP} overload;
    procedure   SendText(const Str : UnicodeString); overload;
    procedure   SendText(const Str : UnicodeString; ACodePage : LongWord); overload;
                                                     {$ENDIF}
    function    RealSend(var Data : TWSocketData; Len : Integer) : Integer; virtual;
    procedure   RaiseException(const Msg : String); virtual;
    function    GetReqVerLow: BYTE;
    procedure   SetReqVerLow(const Value: BYTE);
    function    GetReqVerHigh: BYTE;
    procedure   SetReqVerHigh(const Value: BYTE);
    procedure   TriggerDebugDisplay(Msg : String); { 18/06/05 }
    procedure   TriggerSendData(BytesSent : Integer);
    function    TriggerDataAvailable(Error : Word) : Boolean; virtual;
    procedure   TriggerSessionAvailable(Error : Word); virtual;
    procedure   TriggerSessionConnectedSpecial(Error : Word); virtual;
    procedure   TriggerSessionConnected(Error : Word); virtual;
    procedure   TriggerSessionClosed(Error : Word); virtual;
    procedure   TriggerDataSent(Error : Word); virtual;
    procedure   TriggerChangeState(OldState, NewState : TSocketState); virtual;
    procedure   TriggerDNSLookupDone(Error : Word); virtual;
    procedure   TriggerError; virtual;
    procedure   TriggerAddressListChanged(ErrCode: Word);
    procedure   TriggerRoutingInterfaceChanged(ErrCode: Word);
    function    DoRecv(var Buffer : TWSocketData;
                       BufferSize : Integer;
                       Flags      : Integer) : Integer; virtual;
    function    DoRecvFrom(FHSocket    : TSocket;
                           var Buffer  : TWSocketData;
                           BufferSize  : Integer;
                           Flags       : Integer;
                           var From    : TSockAddr;
                           var FromLen : Integer) : Integer; virtual;
    procedure Do_FD_CONNECT(var msg: TMessage); virtual;
    procedure Do_FD_READ(var msg: TMessage); virtual;
    procedure Do_FD_WRITE(var msg: TMessage); virtual;
    procedure Do_FD_ACCEPT(var msg: TMessage); virtual;
    procedure Do_FD_CLOSE(var msg: TMessage); virtual;
    procedure Do_FD_ROUTING_INTERFACE_CHANGE(var msg: TMessage); virtual;
    procedure Do_FD_ADDRESS_LIST_CHANGE(var msg: TMessage); virtual;
    procedure DupConnected; virtual;
    procedure SetSin(const Value: TSockAddrIn);
    function  GetSin: TSockAddrIn;
    function  GetCurrentSocketFamily: TSocketFamily;
    { The next three are wrappers around methods of TIcsAsyncDnsLookup. They   }
    { mimic the MS native async DNS looup API that isn't available for IPv6.   }
    function IcsAsyncGetHostByName(AWnd                : HWND;
                                   AMsgID              : UINT;
                                   const ASocketFamily : TSocketFamily;
                                   const AName         : String;
                                   const AProtocol     : Integer): THandle;
    function IcsAsyncGetHostByAddr(AWnd                : HWND;
                                   AMsgID              : UINT;
                                   const ASocketFamily : TSocketFamily;
                                   const AAddr         : String;
                                   const AProtocol     : Integer): THandle;
    function IcsCancelAsyncRequest(const ARequest: THandle): Integer;
  public
    property sin  : TSockAddrIn read GetSin write SetSin;
    property sin6 : TSockAddrIn6 read Fsin write Fsin;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Connect; virtual;
    procedure   Close; virtual;
    procedure   CloseDelayed; virtual;
    procedure   Abort; virtual;
    procedure   Flush; virtual;
    procedure   WaitForClose; virtual;
    procedure   Listen; virtual;
    function    Accept: TSocket; virtual;
    function    Receive(Buffer : TWSocketData; BufferSize: Integer) : Integer; virtual;
    function    ReceiveStr : String; virtual;
    function    ReceiveStrA : AnsiString; virtual;
{$IFDEF COMPILER12_UP}
    function    ReceiveStrW(ACodePage: LongWord) : UnicodeString; overload; virtual;
    function    ReceiveStrW : UnicodeString; overload; virtual;
{$ENDIF}
    function    ReceiveFrom(Buffer      : TWSocketData;
                            BufferSize  : Integer;
                            var From    : TSockAddr;
                            var FromLen : Integer) : Integer; virtual;
    function    ReceiveFrom6(Buffer      : TWSocketData;   { V8.07 }
                             BufferSize  : Integer;
                             var From    : TSockAddrIn6;
                             var FromLen : Integer) : Integer; virtual;
    function    PeekData(Buffer : TWSocketData; BufferSize: Integer) : Integer;
    function    Send(Data : TWSocketData; Len : Integer) : Integer; overload; virtual;
    function    Send(DataByte : Byte) : Integer; overload; virtual;
    function    SendTo(Dest       : TSockAddr;
                       DestLen    : Integer;
                       Data       : TWSocketData;
                       Len        : Integer) : Integer; virtual;
    function    SendTo6(Dest       : TSockAddrIn6;       { V8.07 }
                        DestLen    : Integer;
                        Data       : TWSocketData;
                        Len        : Integer) : Integer; virtual;
    function    SendStr(const Str : RawByteString) : Integer; {$IFDEF COMPILER12_UP} overload; {$ENDIF} virtual;
{$IFDEF COMPILER12_UP}
    function    SendStr(const Str : UnicodeString; ACodePage: LongWord) : Integer; overload; virtual;
    function    SendStr(const Str : UnicodeString) : Integer; overload; virtual;
{$ENDIF}
    procedure   DnsLookup(const AHostName : String); overload; virtual;
    procedure   DnsLookup(const AHostName : String; const AProtocol: Integer); overload; virtual;
    procedure   ReverseDnsLookup(const HostAddr: String); overload; virtual;
    procedure   ReverseDnsLookup(const HostAddr: String; const AProtocol: Integer); overload; virtual;
    procedure   ReverseDnsLookupSync(const HostAddr: String); overload; virtual;  {AG 03/03/06}
    procedure   ReverseDnsLookupSync(const HostAddr: String; const AProtocol: Integer); overload; virtual;
    procedure   CancelDnsLookup; virtual;
    { Sets behavior of the internal DNS lookup object.                         }
    { AMinThreads determines the number of persistent DNS lookup threads while }
    { AMaxThreads determines the maximum number of threads the internal DNS    }
    { lookup object may create per caller's thread context. After an idle time }
    { out of 60 seconds the non-persistent threads exit.                       }
    { Works only with new API, that is when SocketFamily is not sfIPv4.        }
    procedure   SetMinMaxIcsAsyncDnsLookupThreads(AMinThreads, AMaxThreads: Byte);
    { Helper method that assigns FAsyncLookupPtr internally                   }
    procedure   RegisterIcsAsyncDnsLookup;
    procedure   UnregisterIcsAsyncDnsLookup;

    function    GetPeerAddr: String; virtual;
    function    GetPeerPort: String; virtual;
    function    GetPeerName(var Name : TSockAddrIn; NameLen : Integer) : Integer; virtual;
    function    GetXPort: String; virtual;
    function    GetXAddr: String; virtual;
    function    TimerIsSet(var tvp : TTimeVal) : Boolean; virtual;
    procedure   TimerClear(var tvp : TTimeVal); virtual;
    function    TimerCmp(var tvp : TTimeVal; var uvp : TTimeVal; IsEqual : Boolean) : Boolean; virtual;
    function    GetSockName(var saddr : TSockAddrIn; var saddrlen : Integer) : Integer; virtual;
    procedure   SetLingerOption;
    procedure   SetKeepAliveOption;
    function    SetTcpNoDelayOption: Boolean; { V7.27 }
    function    SetRoutingInterfaceChangeNotification: Boolean; virtual;
    function    SetAddressListChangeNotification: Boolean; virtual;
    procedure   Dup(NewHSocket : TSocket); virtual;
    procedure   Shutdown(How : Integer); virtual;
    procedure   Pause; virtual;
    procedure   Resume; virtual;
    procedure   PutDataInSendBuffer(Data : TWSocketData; Len : Integer); virtual;
    function    PutStringInSendBuffer(const Str : RawByteString): Integer; {$IFDEF COMPILER12_UP} overload; {$ENDIF}
{$IFDEF COMPILER12_UP}
    function    PutStringInSendBuffer(const Str : UnicodeString; ACodePage: LongWord): Integer; overload;
    function    PutStringInSendBuffer(const Str : UnicodeString): Integer; overload;
{$ENDIF}
    procedure   DeleteBufferedData;
    procedure   ThreadAttach; override;
    procedure   ThreadDetach; override;
    procedure   CreateCounter; virtual;
    procedure   DestroyCounter;
    property    BufferedByteCount  : LongInt        read FBufferedByteCount;  { V5.20 }
    property    CurrentSocketFamily: TSocketFamily  read GetCurrentSocketFamily;
  protected
{$IFNDEF NO_DEBUG_LOG}
    property IcsLogger : TIcsLogger                 read  FIcsLogger          { V5.21 }
                                                    write SetIcsLogger;       { V5.21 }
{$ENDIF}
    property PortNum : Integer                      read  FPortNum;
    property FWindowHandle : HWND                   read  FHandle;
    property HSocket : TSocket                      read  FHSocket
                                                    write Dup;
    property Addr : String                          read  FAddrStr
                                                    write SetAddr;
    property Port : String                          read  GetRemotePort
                                                    write SetRemotePort;
    property LocalPort : String                     read  FLocalPortStr
                                                    write SetLocalPort;
    property LocalAddr : String                     read  FLocalAddr
                                                    write SetLocalAddr;
    property LocalAddr6: String                     read  FLocalAddr6
                                                    write SetLocalAddr6;
    property Proto : String                         read  FProtoStr
                                                    write SetProto;
    property MultiCast       : Boolean              read  FMultiCast
                                                    write FMultiCast
                                                    default False;
    property MultiCastAddrStr: String               read  FMultiCastAddrStr
                                                    write SetMultiCastAddrStr;
    property MultiCastIpTTL  : Integer              read  FMultiCastIpTTL
                                                    write FMultiCastIpTTL
                                                    default 1;
    property ReuseAddr       : Boolean              read  FReuseAddr
                                                    write FReuseAddr
                                                    default False;
    property PeerAddr : String                      read  GetPeerAddr;
    property PeerPort : String                      read  GetPeerPort;
    property DnsResult : String                     read  FDnsResult;
    property DnsResultList : TStrings               read  FDnsResultList;
    property State : TSocketState                   read  FState;
    property AllSent   : Boolean                    read  bAllSent;
    property ReadCount : Int64                      read  FReadCount;    { V5.26 }
    property WriteCount : Int64                     read  FWriteCount;   { V7.24 }
    property RcvdCount : LongInt                    read  GetRcvdCount;
    property LastError : Integer                    read  FLastError
                                                    write FLastError   { V5.20 }
                                                    default 0;
    property ComponentOptions : TWSocketOptions     read  FComponentOptions
                                                    write FComponentOptions;
    property BufSize          : Integer             read  GetBufSize
                                                    write SetBufSize;
    property SocketRcvBufSize : Integer             read  FSocketRcvBufSize
                                                    write SetSocketRcvBufSize;
    property SocketSndBufSize : Integer             read  FSocketSndBufSize
                                                    write SetSocketSndBufSize;
    property ListenBacklog    : Integer             read  FListenBacklog
                                                    write FListenBacklog
                                                    default 5;
    property ReqVerLow       : BYTE                 read  GetReqVerLow
                                                    write SetReqVerLow
                                                    default 2;
    property ReqVerHigh      : BYTE                 read  GetReqVerHigh
                                                    write SetReqVerHigh
                                                    default 2;
    property OnDataAvailable : TDataAvailable       read  FOnDataAvailable
                                                    write FOnDataAvailable;
    property OnDataSent      : TDataSent            read  FOnDataSent
                                                    write FOnDataSent;
    property OnSendData      : TSendData            read  FOnSendData
                                                    write FOnSendData;
    property OnSessionClosed : TSessionClosed       read  FOnSessionClosed
                                                    write FOnSessionClosed;
    property OnSessionAvailable : TSessionAvailable read  FOnSessionAvailable
                                                    write FOnSessionAvailable;
    property OnSessionConnected : TSessionConnected read  FOnSessionConnected
                                                    write FOnSessionConnected;
    property OnChangeState      : TChangeState      read  FOnChangeState
                                                    write FOnChangeState;
    property OnDnsLookupDone    : TDnsLookupDone    read  FOnDnsLookupDone
                                                    write FOnDnsLookupDone;
    property OnError            : TNotifyEvent      read  FOnError
                                                    write FOnError;
    { FlushTimeout property is not used anymore }
    property FlushTimeout : Integer                 read  FFlushTimeOut
                                                    write FFlushTimeout
                                                    default 60;
    property SendFlags : TSocketSendFlags           read  GetSendFlags
                                                    write SetSendFlags
                                                    default wsSendNormal;
    property Text: String                           read  ReceiveStr
                                                    write SendText;
    property LingerOnOff   : TSocketLingerOnOff     read  FLingerOnOff
                                                    write FLingerOnOff
                                                    default wsLingerOn;
    property LingerTimeout : Integer                read  FLingerTimeout
                                                    write FLingerTimeout
                                                    default 0;
    property KeepAliveOnOff: TSocketKeepAliveOnOff  read  FKeepAliveOnOff
                                                    write FKeepAliveOnOff
                                                    default wsKeepAliveOff;
    property KeepAliveTime : Integer                read  FKeepAliveTime
                                                    write FKeepAliveTime
                                                    default 0;
    property KeepAliveInterval : Integer            read  FKeepAliveInterval
                                                    write FKeepAliveInterval
                                                    default 0;
    property OnDebugDisplay : TDebugDisplay         read  FOnDebugDisplay
                                                    write FOnDebugDisplay;
    property Counter      : TWSocketCounter         read  FCounter;
    property CounterClass : TWsocketCounterClass    read  FCounterClass
                                                    write SetCounterClass;
    property SocketFamily : TSocketFamily           read  FSocketFamily
                                                    write SetSocketFamily
                                                    default DefaultSocketFamily;
    property OnAddressListChanged : TNetChangeEvent read  FOnAddressListChanged
                                                    write SetOnAddressListChanged;
    property OnRoutingInterfaceChanged : TNetChangeEvent
                                                    read  FOnRoutingInterfaceChanged
                                                    write SetOnRoutingInterfaceChanged;
  end;

  THttpTunnelAuthType = (htatDetect, htatNone, htatBasic,
                    {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
                         htatDigest,
                    {$ENDIF}
                         htatNtlm
                         );
  THttpTunnelServerAuthTypes = set of (htsatBasic, htsatNtlm, htsatDigest);
  THttpTunnelState = (htsData, htsConnecting, htsConnected,
                      htsWaitResp0, htsWaitResp1, htsWaitResp2);
  THttpTunnelErrorEvent = procedure(Sender               : TObject;
                                   ErrCode               : Word;
                                   TunnelServerAuthTypes : THttpTunnelServerAuthTypes;
                                   const Msg             : String) of object;
  THttpTunnelChunkState  = (htcsGetSize,     htcsGetExt, htcsGetData,
                            htcsGetBoundary, htcsDone);
  THttpTunnelProto       = (htp11, htp10);
  THttpTunnelReconnectRequest = (htrrNone, htrrBasic, htrrDigest, htrrNtlm1);
  TCustomHttpTunnelWSocket = class(TCustomWSocket)
  private
      FHttpTunnelAuthChallenge    : AnsiString;
  {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
      FHttpTunnelAuthDigestCached : Boolean;
      FHttpTunnelAuthDigestValid  : Boolean;
      FHttpTunnelAuthDigestHash   : THashHex;
      FHttpTunnelAuthDigestInfo   : TAuthDigestResponseInfo;
  {$ENDIF}
      FHttpTunnelAuthType         : THttpTunnelAuthType;
      FHttpTunnelBuf              : TBytes;
      FHttpTunnelBufSize          : Integer;
      FHttpTunnelChunked          : Boolean;
      FHttpTunnelChunkRcvd        : Integer;
      FHttpTunnelChunkSize        : Integer;
      FHttpTunnelChunkState       : THttpTunnelChunkState;
      FHttpTunnelCloseNotified    : Boolean;
      FHttpTunnelContentLength    : Integer;
      FHttpTunnelCurAuthType      : THttpTunnelAuthType;
      FHttpTunnelKeepsAlive       : Boolean;
      FHttpTunnelLastResponse     : AnsiString; // Also hijacked for internal error messages
      FHttpTunnelLmCompatLevel    : LongWord;   { V7.86 }
      FHttpTunnelReconnectRequest : THttpTunnelReconnectRequest;
      FHttpTunnelPassword         : String;
      FHttpTunnelPort             : AnsiString;
      FHttpTunnelPortAssigned     : Boolean;
      FHttpTunnelProto            : THttpTunnelProto;
      FHttpTunnelRcvdCnt          : Integer;
      FHttpTunnelRcvdIdx          : Integer;
      FHttpTunnelServer           : AnsiString;
      FHttpTunnelServerAssigned   : Boolean;
      FHttpTunnelServerAuthTypes  : THttpTunnelServerAuthTypes;
      FHttpTunnelState            : THttpTunnelState;
      FHttpTunnelStatusCode       : Word;
      FHttpTunnelUsercode         : String;
      FHttpTunnelWaitingBody      : Boolean;
      FMsg_WM_TUNNEL_RECONNECT    : UINT;
      FOnHttpTunnelConnected      : TSessionConnected;
      FOnHttpTunnelError          : THttpTunnelErrorEvent;

      function  GetHttpTunnelLastResponse: String;
      function  GetHttpTunnelServer: String;
      function  GetHttpTunnelPort: String;
      procedure HttpTunnelClear;
      function  HttpTunnelGetNtlmMessage3: String;
      function  HttpTunnelProcessHdrLine(Data: PAnsiChar; Cnt: Integer): Boolean;
      procedure HttpTunnelSendAuthBasic;
  {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
      procedure HttpTunnelSendAuthDigest;
  {$ENDIF}
      procedure HttpTunnelSendAuthNtlm_1;
      procedure HttpTunnelSendAuthNtlm_3;
      procedure HttpTunnelSendPlainConnect;
      function  HttpTunnelTriggerResultOrContinue: Boolean;
      procedure SetHttpTunnelAuthType(const Value: THttpTunnelAuthType);
      procedure SetHttpTunnelBufferSize(BufSize: Integer);
      procedure SetHttpTunnelServer(const Value: String);
      procedure SetHttpTunnelPassword(const Value: String);
      procedure SetHttpTunnelPort(const Value: String);
      procedure SetHttpTunnelUsercode(const Value: String);
      procedure TriggerHttpTunnelConnected(ErrCode : Word);
      procedure TriggerHttpTunnelError(ErrCode: Word);
  protected
      procedure AllocateMsgHandlers; override;
      procedure AssignDefaultValue; override;
      procedure Do_FD_CLOSE(var msg: TMessage); override;
      function  DoRecv(var Buffer : TWSocketData;
                       BufferSize : Integer;
                       Flags      : Integer) : Integer; override;
      procedure FreeMsgHandlers; override;
      function  GetRcvdCount : LongInt; override;
      function  MsgHandlersCount : Integer; override;
      procedure RaiseException(const Msg : String); override;
      function  TriggerDataAvailable(ErrCode : Word) : Boolean; override;
      procedure TriggerSessionClosed(ErrCode : Word); override;
      procedure TriggerSessionConnectedSpecial(ErrCode: Word); override;
      procedure WndProc(var MsgRec: TMessage); override;
      procedure WMHttpTunnelReconnect(var MsgRec: TMessage);

      property  HttpTunnelAuthType   : THttpTunnelAuthType
                                                    read  FHttpTunnelAuthType
                                                    write SetHttpTunnelAuthType
                                                    default htatDetect;
      property  HttpTunnelBufferSize : Integer      read  FHttpTunnelBufSize
                                                    write SetHttpTunnelBufferSize;
      property  HttpTunnelLastResponse : String     read  GetHttpTunnelLastResponse;
      property  HttpTunnelLmCompatLevel : LongWord  read  FHttpTunnelLmCompatLevel   { V7.86 }
                                                    write FHttpTunnelLmCompatLevel;  { V7.86 }
      property  HttpTunnelPassword   : String       read  FHttpTunnelPassword
                                                    write SetHttpTunnelPassword;
      property  HttpTunnelPort       : String       read  GetHttpTunnelPort
                                                    write SetHttpTunnelPort;
      property  HttpTunnelServer     : String       read  GetHttpTunnelServer
                                                    write SetHttpTunnelServer;
      property  HttpTunnelUsercode   : String       read  FHttpTunnelUsercode
                                                    write SetHttpTunnelUsercode;

      property  HttpTunnelCurrentAuthType : THttpTunnelAuthType
                                                    read FHttpTunnelCurAuthType;

      property  OnHttpTunnelError  : THttpTunnelErrorEvent
                                                    read  FOnHttpTunnelError
                                                    write FOnHttpTunnelError;
      property  OnHttpTunnelConnected : TSessionConnected
                                                    read  FOnHttpTunnelConnected
                                                    write FOnHttpTunnelConnected;
  public
      constructor Create(AOwner: TComponent); override;
      procedure Connect; override;
      procedure Listen; override;
  end;

  TSocksState          = (socksData, socksNegociateMethods, socksAuthenticate, socksConnect);
  TSocksAuthentication = (socksNoAuthentication, socksAuthenticateUsercode);
  TSocksAuthState      = (socksAuthStart, socksAuthSuccess, socksAuthFailure, socksAuthNotRequired);
  TSocksAuthStateEvent = procedure(Sender : TObject; AuthState : TSocksAuthState) of object;
  TSocksErrorEvent     = procedure(Sender : TObject; Error : Integer; Msg : String) of Object;

  TCustomSocksWSocket = class(TCustomHttpTunnelWSocket)
  protected
      FSocksState          : TSocksState;
      FSocksServer         : String;
      FSocksLevel          : String;
      FSocksPort           : String;
      FSocksPortAssigned   : Boolean;
      FSocksServerAssigned : Boolean;
      FSocksUsercode       : String;
      FSocksPassword       : String;
      FSocksAuthentication : TSocksAuthentication;
      FSocksAuthNumber     : AnsiChar;
      FBoundAddr           : AnsiString;
      FBoundPort           : AnsiString;
      FRcvBuf              : array [0..127] of Byte;
      FRcvCnt              : Integer;
      FSocksRcvdCnt        : Integer;
      FSocksRcvdPtr        : Integer;
      FOnSocksError        : TSocksErrorEvent;
      FOnSocksConnected    : TSessionConnected;
      FOnSocksAuthState    : TSocksAuthStateEvent;
      procedure   AssignDefaultValue; override;
      procedure   TriggerSessionConnectedSpecial(Error : Word); override;
      procedure   TriggerSocksConnected(Error : Word); virtual;
      procedure   TriggerSessionClosed(Error : Word); override;
      function    TriggerDataAvailable(Error : Word) : Boolean; override;
      function    GetSocksPort: String;
      procedure   SetSocksPort(sPort : String); virtual;
      function    GetSocksServer: String;
      procedure   SetSocksServer(sServer : String); virtual;
      procedure   TriggerSocksError(Error : Integer; Msg : String); virtual;
      procedure   TriggerSocksAuthState(AuthState : TSocksAuthState);
      function    GetRcvdCount : LongInt; override;
      procedure   SetSocksLevel(newValue : String);
      function    DoRecv(var Buffer : TWSocketData;
                         BufferSize : Integer;
                         Flags      : Integer) : Integer; override;
      procedure   SocksDoConnect;
      procedure   SocksDoAuthenticate;
      procedure   DataAvailableError(ErrCode : Integer; Msg : String);
  public
      constructor Create(AOwner : TComponent); override;
      procedure   Connect; override;
      procedure   Listen; override;
  protected
      property SocksServer   : String               read  GetSocksServer
                                                    write SetSocksServer;
      property SocksLevel    : String               read  FSocksLevel
                                                    write SetSocksLevel;
      property SocksPort     : String               read  FSocksPort
                                                    write SetSocksPort;
      property SocksUsercode : String               read  FSocksUsercode
                                                    write FSocksUsercode;
      property SocksPassword : String               read  FSocksPassword
                                                    write FSocksPassword;
      property SocksAuthentication : TSocksAuthentication
                                                    read  FSocksAuthentication
                                                    write FSocksAuthentication
                                                    default socksNoAuthentication;
      property OnSocksError  : TSocksErrorEvent     read  FOnSocksError
                                                    write FOnSocksError;
      property OnSocksConnected : TSessionConnected read  FOnSocksConnected
                                                    write FOnSocksConnected;
      property OnSocksAuthState : TSocksAuthStateEvent
                                                    read  FOnSocksAuthState
                                                    write FOnSocksAuthState;
  end;

{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  A component adding SSL support to TWSocket.
              This unit contains the interface for the component.
              It is included in WSocket.pas unit when USE_SSL is defined.
              The implementation part is in WSocketImplSsl.inc.
              Make use of OpenSSL (http://www.openssl.org).
              Make use of freeware TWSocket component from ICS.
              This version has been developped with the excellent collaboration
              and expertize from Arno Garrels <arno.garrels@gmx.de> and
              Benjamin Stadin <stadin@gmx.de>. They worked very hard to make
              this code working.
Creation:     Jan 11, 2003
Version:      1.00.9

Reference guide:
    SslCertFile     Filename of the certificate sent to the remote site for
                    authetification, in PEM format.
    SslPassPhrase   Password phrase used to protect SslCertFile.
    SslPrivKeyFile  Private key used to encrypt data. Must correspond to the
                    public key stored in the certificate identified by
                    SslCertFile.
    SslCAFile       Filename of CA certificates in PEM format. The file can
                    contain several CA certificates identified by
                    -----BEGIN CERTIFICATE-----
                    ... (CA certificate in base64 encoding) ...
                    -----END CERTIFICATE-----
                    sequences. Before, between, and after the certificates text
                    is allowed which can be used e.g. for descriptions of the
                    certificates.
                    CAFile can be an empty string if CAPath is used.
    SslCAPath       Directory containing CA certificates in PEM format. The
                    files each contain one CA certificate. The files are looked
                    up by the CA subject name hash value, which must hence be
                    available.
                    If more than one CA certificate with the same name hash
                    value exist, the extension must be different (e.g.
                    9d66eef0.0, 9d66eef0.1 etc). The search is performed in
                    the ordering of the extension number, regardless of other
                    properties of the certificates.
                    To create the hash value for filenames, use the command
                    line: openssl x509 -hash -noout -in YourCert.pem
                    The output is a 8 digit hex number you _must_ use as file
                    name for a given certificate in CAPath directory. Can be
                    any extension, using a numeric extension is handy.

History:
Jan 19, 2003 V1.00.2 First pre-relase version. Works with TWSocket version 5.00.
             Lot of things remains to do. Currently support basic connections
             (Socks doesn't work, line mode doesn't work).
Mar 04, 2003 V1.00.3 Socks and LineMode support
Apr 14, 2003 V1.00.4 Fixed bugs related to premature session close
Apr 04, 2004 V1.00.5 Verified with new WSocket version
Aug 31, 2005 V1.00.8 Use the code from Arno Garrels <arno.garrels@gmx.de> and
              Benjamin Stadin <stadin@gmx.de>. They worked very hard to make
              this code working.
Dec 07, 2005 V1.00.9 A. Garrels fixed an issue with BIO I/O functions.
             Support of OSSL v0.9.8a added. Changed load order of OpenSSL
             libraries. A received SSL shutdown notification in Do_FD_READ was
             not detected, fixed. OpenSSL releases from 0.9.7g up to 0.9.8a
             should be supported. New OpenSSL version check, an exception is
             raised if version is not in the range of supported versions. In
             order to disable the version check uncomment define
             NO_OSSL_VERSION_CHECK in IcsLIBEAY.pas and rebuild all. Two new
             methods of TSslContext to ease verification of client certificates.
             They create/modify the list of acceptable CAs sent to the client
             when a server requests a client certificate, AddClientCAFromFile
             and SetClientCAListFromFile, see comments on top of the functions.
             SslOptions modified. SSLv3 renegotiaton added, there are two
             new functions SslStartRenegotiation and SslRenegotiatePending,
             see comments on top of the functions. When renegotiation is
             requested in server mode a new SslOption should be set also it's
             sslOpt_NO_SESSION_RESUMPTION_ON_RENEGOTIATION.
Dec 19, 2005 Angus new wsocket logging
Jan 18, 2006 Arno Garrels: A lot of bugs fixed probably alot of new added ;-)
             bidirectional shutdown added.
Jan 26, 2006 Type of TSslSessionIdContext changed to AnsiString due to problems
             with BCB.
Mar 02, 2006 Removed function SslStateToStr which was wrong and not used.
             Arno Garrels fixed TCustomSslWSocket.Do_FD_CLOSE
Mar 06, 2006 A. Garrels: Removed the so called fix from Mar 02 in Do_FD_CLOSE
             because it it wasn't a real fix. Instead several changes at
             several places were required to fix the shutdown problems. Fixed
             error "Undeclared identifier RaiseLastOpenSslError" when
             NO_DEBUG_LOG was defined. Added properties ValidNotBefore and
             ValidNotAfter to TX509Base.
             Multi-threading: OpenSSL library is thread safe as long as the
             application provides an appropriate locking callback. Implemented
             such callbacks as two components see unit IcsSslThrdLock.
             Changed InitContext to always set session cache options, because
             the default OpenSSL setting is to use the internal cache.
Jun 20, 2007 Changes by Arno Garrels: Fixed TX509Base.PostConnectionCheck to
             handle wildcard certificates. Property TX509Base.SubjectCName may
             now include a list of strings separated by CRLF (many certificates
             use multiple common name fields). Common name fields encoded
             Unicode or UTF-8 are now converted to ansi string. New properties
             TX509Base.FirstVerifyResult and TX509Base.FirstVerifyErrMsg
             hold the first verify result, because a certificate may pass
             verification process several times which overwrites value of
             VerifyResult.
Nov 08, 2007 A. Garrels added property PublicKey to TX509Base.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
    Bomb('This unit require a 32 bit compiler !');
{$ENDIF}
{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{$IFDEF DEBUG_DUMP}
    {$DEFINE DEBUG_OUTPUT}
{$ENDIF}
const
     SslWSocketVersion            = 100;
     SslWSocketDate               = 'Jan 18, 2006';
     SslWSocketCopyRight : String = ' TSslWSocket (c) 2003-2010 Francois Piette V1.00.5e ';

const

     //SSL_POST_CONNECTION_CHECK_FAILED = 12101;
     sslProtocolError                 = 20100;
     SSL_BUFFER_SIZE                  = 4096;
     msgSslCtxNotInit                 = 'SSL context not initialized';

  { V8.10 - TSslContext default SslCipherList
    note a client generally wants to talk to as many servers as possible so use sCiphersNormal,
    while a server wants to force clients to use the highest security possible, and Mozilla
    kindly publishes it's internal recommendations for OpenSSL server configuration, see below. }
    sslCiphersNormal = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH';
    sslCiphersServer = 'TLSv1+HIGH:!SSLv2:RC4+MEDIUM:!aNULL:!eNULL:!3DES:!CAMELLIA@STRENGTH';

  { from https://wiki.mozilla.org/Security/Server_Side_TLS - Version 3.3 - 20th October 2013
    Configuration   Oldest compatible client
        sslCiphersMozillaSrvHigh - Firefox 27, Chrome 22, IE 11, Opera 14, Safari 7, Android 4.4, Java 8
        sslCiphersMozillaSrvInter -  Firefox 1, Chrome 1, IE 7, Opera 5, Safari 1, Windows XP IE8, Android 2.3, Java 7
        sslCiphersMozillaSrvBack - Windows XP IE6, Java 6 }

   { Backward Compatible, works with all clients back to Windows XP/IE6,  Versions: SSLv3, TLSv1, TLSv1.1, TLSv1.2
    RSA key size: 2048, DH Parameter size: 1024, Elliptic curves: secp256r1, secp384r1, secp521r1 }
    sslCiphersMozillaSrvBack =
        'ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:' +
        'ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:' +
        'ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:' +
        'ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:' +
        'DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:' +
        'DHE-RSA-AES256-SHA:ECDHE-RSA-DES-CBC3-SHA:ECDHE-ECDSA-DES-CBC3-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:' +
        'AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA:AES:DES-CBC3-SHA:HIGH:!aNULL:!eNULL:!EXPORT:!DES:' +
        '!RC4:!MD5:!PSK:!aECDH:!EDH-DSS-DES-CBC3-SHA:!EDH-RSA-DES-CBC3-SHA:!KRB5-DES-CBC3-SHA' ;
  { For services that don't need backward compatibility, the parameters below provide a higher level of security
   Versions: TLSv1.1, TLSv1.2, RSA key size: 2048, DH Parameter size: 2048, Elliptic curves: secp256r1, secp384r1, secp521r1 }
    sslCiphersMozillaSrvHigh =
        'ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:' +
        'ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:' +
        'ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:' +
        'ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:' +
        'DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:' +
        'DHE-RSA-AES256-SHA:!aNULL:!eNULL:!EXPORT:!DES:!RC4:!3DES:!MD5:!PSK' ;
  {Intermediate compatibility - For services that don't need compatibility with legacy clients (mostly WinXP),
   but still need to support a wide range of clients, this configuration is recommended. It is is compatible with
   Firefox 1, Chrome 1, IE 7, Opera 5 and Safari 1.   Versions: TLSv1, TLSv1.1, TLSv1.2,  RSA key size: 2048
    DH Parameter size: 2048 (1024 tolerable), Elliptic curves: secp256r1, secp384r1, secp521r1 (at a minimum)
    Certificate signature: SHA-256 }
    sslCiphersMozillaSrvInter =
        'ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:' +
        'ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:' +
        'ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:' +
        'ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:' +
        'DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:' +
        'DHE-RSA-AES256-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA:' +
        'AES:CAMELLIA:DES-CBC3-SHA:!aNULL:!eNULL:!EXPORT:!DES:!RC4:!MD5:!PSK:!aECDH:!EDH-DSS-DES-CBC3-SHA:' +
        '!EDH-RSA-DES-CBC3-SHA:!KRB5-DES-CBC3-SHA';

{$IFNDEF NO_SSL_MT}
var
     LockPwdCB          : TIcsCriticalSection;
     LockVerifyCB       : TIcsCriticalSection;
     LockInfoCB         : TIcsCriticalSection;
     LockRemSessCB      : TIcsCriticalSection;
     LockNewSessCB      : TIcsCriticalSection;
     LockGetSessCB      : TIcsCriticalSection;
     LockClientCertCB   : TIcsCriticalSection;
   {$IFNDEF OPENSSL_NO_TLSEXT}
     LockServerNameCB   : TIcsCriticalSection;
   {$ENDIF}
{$ENDIF}
     procedure UnloadSsl;
     procedure LoadSsl;

type
//    TSslDebugLevel = (ssldbgNone, ssldbgError, ssldbgInfo, ssldbgDump); angus

    EOpenSslError = class(Exception);
    TSslBaseComponent = class(TComponent)
    protected
        FSslInitialized : Boolean;
        FLastSslError   : Integer;

    {$IFNDEF NO_DEBUG_LOG}                                             { V5.21 }
        FIcsLogger  : TIcsLogger;
        procedure   SetIcsLogger(const Value : TIcsLogger); virtual;   { V5.21 }
        procedure   Notification(AComponent  : TComponent;             { V5.21 }
                                 Operation   : TOperation); override;
        procedure   DebugLog(LogOption : TLogOption;                   { V5.21 }
                             const Msg : string); virtual;
        function    CheckLogOptions(const LogOption: TLogOption): Boolean; virtual; { V5.21 }
    {$ENDIF}
        procedure   RaiseLastOpenSslError(EClass          : ExceptClass;
                                          Dump            : Boolean = FALSE;
                                          const CustomMsg : String  = ''); virtual;
        procedure   InitializeSsl; {$IFDEF USE_INLINE} inline; {$ENDIF}
        procedure   FinalizeSsl;
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        property    LastSslError : Integer read FLastSslError;
{$IFNDEF NO_DEBUG_LOG}
    published
        property    IcsLogger : TIcsLogger                read  FIcsLogger    { V5.21 }
                                                          write SetIcsLogger;
{$ENDIF}
    end;
    (*
    TX509Stack = class(TObject) // Not yet used, but will be soon!
    private
        FStack : PSTACK;
        FCount : Integer;
    protected
        function    GetCert(Index : Integer): PX509;
        procedure   SetCert(Index : Integer; const Value : PX509);
        procedure   SetStack(const Value : PStack);
        function    InternalInsert(Cert : PX509; Index : Integer): Integer;
    public
        constructor Create;
        destructor  Destroy; override;
        function    Add(Cert : PX509): Integer;
        procedure   Clear;
        procedure   Insert(Cert : PX509; Index : Integer);
        function    IndexOf(Cert : PX509): Integer;
        procedure   Delete(Index : Integer);
        property    Count                   : Integer           read  FCount;
        property    Certs[index : Integer]  : PX509             read  GetCert
                                                                write SetCert; default;
        property    Stack: PSTACK                               read  FStack
                                                                write SetStack;
    end;
    *)
{$IFNDEF COMPILER6_UP}
const                                                             {AG 02/06/06}
    MinDateTime: TDateTime = -657434.0;      { 01/01/0100 12:00:00.000 AM }
    MaxDateTime: TDateTime =  2958465.99999; { 12/31/9999 11:59:59.999 PM }
{$ENDIF}
type
    EX509Exception = class(Exception);

    TExtension = record
        Critical  : Boolean;
        ShortName : String;
        Value     : String; // may be also one or multiple Name=value pairs,
    end;                    // separated by a CRLF
    PExtension = ^TExtension;

    THashBytes20 = array of Byte;

    TBioOpenMethode = (bomRead, bomWrite);
    TX509Base = class(TSslBaseComponent)
    private
        FX509               : Pointer;
        FPrivateKey         : Pointer;
        FSha1Digest         : THashBytes20;
        FSha1Hex            : String;
    protected
        FVerifyResult       : Integer;  // current verify result
        //FSha1Hash           : AnsiString; // Deprecated
        FVerifyDepth        : Integer;
        FCustomVerifyResult : Integer;
        FFirstVerifyResult  : Integer;                      {05/21/2007 AG}
        procedure   FreeAndNilX509;
        procedure   FreeAndNilPrivateKey;
        procedure   SetX509(X509: Pointer);
        procedure   SetPrivateKey(PKey: Pointer);
        function    GetPublicKey: Pointer;
        function    GetVerifyErrorMsg: String;
        function    GetFirstVerifyErrorMsg: String;         {05/21/2007 AG}
        function    GetIssuerOneLine: String;
        function    GetSubjectOneLine: String;
        function    GetSerialNum: Integer; virtual;
        function    GetSubjectCName: String;
        function    GetSubjectAltName: TExtension; virtual;
        function    GetExtension(Index: Integer): TExtension; virtual;
        function    GetExtensionCount: Integer;
        function    GetValidNotBefore: TDateTime;              {AG 02/06/06}
        function    GetValidNotAfter: TDateTime;               {AG 02/06/06}
        function    GetHasExpired: Boolean;                    {AG 02/06/06}
        function    GetSelfSigned: Boolean;
        procedure   AssignDefaults; virtual;
        function    UnknownExtDataToStr(Ext: PX509_Extension) : String;
        function    GetSha1Hash: AnsiString; deprecated
          {$IFDEF COMPILER12_UP}'Use GetSha1Digest or GetSha1Hex'{$ENDIF};
        function    GetSha1Digest: THashBytes20;
        function    GetSha1Hex: String;
        function    OpenFileBio(const FileName  : String;
                                Methode         : TBioOpenMethode): PBIO;
        procedure   ReadFromBio(ABio: PBIO; IncludePrivateKey: Boolean = FALSE;
                                const Password: String = ''); virtual;
        procedure   WriteToBio(ABio: PBIO; IncludePrivateKey: Boolean = FALSE;
                               AddRawText: Boolean = FALSE); virtual;
    public
        constructor Create(AOwner: TComponent; X509: Pointer = nil); reintroduce;
        destructor  Destroy; override;
        function    ExtByName(const ShortName: String): Integer;
        function    PostConnectionCheck(HostOrIp: String): Boolean; virtual;
        function    GetRawText: String;                        {05/21/2007 AG}
        procedure   LoadFromPemFile(const FileName: String;
                                    IncludePrivateKey: Boolean = False;
                                    const Password: String = '');
        procedure   SaveToPemFile(const FileName: String;
                                  IncludePrivateKey: Boolean = FALSE;
                                  AddRawText: Boolean = FALSE);
        procedure   PrivateKeyLoadFromPemFile(const FileName: String;
                                              const Password: String = '');
        procedure   PrivateKeySaveToPemFile(const FileName: String);
        function    IssuedBy(ACert: TX509Base): Boolean;
        function    IssuerOf(ACert: TX509Base): Boolean;
        function    SameHash(const ACert: TX509Base): Boolean;
        property    IssuerOneLine       : String        read  GetIssuerOneLine;
        property    SubjectOneLine      : String        read  GetSubjectOneLine;
        property    SerialNum           : Integer       read  GetSerialNum;
        property    VerifyResult        : Integer       read  FVerifyResult
                                                        write FVerifyResult;
        property    VerifyErrMsg        : String        read  GetVerifyErrorMsg;
        property    VerifyDepth         : Integer       read  FVerifyDepth
                                                        write FVerifyDepth;
        property    CustomVerifyResult  : Integer       read  FCustomVerifyResult
                                                        write FCustomVerifyResult;
        property    FirstVerifyResult   : Integer       read  FFirstVerifyResult {05/21/2007 AG}
                                                        write FFirstVerifyResult;
        property    FirstVerifyErrMsg   : String        read  GetFirstVerifyErrorMsg; {05/21/2007 AG}
        property    X509                : Pointer       read  FX509
                                                        write SetX509;
        property    PrivateKey          : Pointer       read  FPrivateKey
                                                        write SetPrivateKey;
        property    PublicKey           : Pointer       read  GetPublicKey;      {AG 11/08/07}
        property    SubjectCName        : String        read  GetSubjectCName;
        property    SubjectAltName      : TExtension    read  GetSubjectAltName;
        property    ExtensionCount      : Integer       read  GetExtensionCount;
        property    Extensions[index: Integer] : TExtension read GetExtension;
        function    Sha1Hash            : AnsiString;   deprecated
          {$IFDEF COMPILER12_UP}'Use Sha1Digest or Sha1Hex'{$ENDIF};
        property    Sha1Digest          : THashBytes20  read  GetSha1Digest;
        property    Sha1Hex             : String        read  GetSha1Hex;
        property    ValidNotBefore      : TDateTime     read  GetValidNotBefore; {AG 02/06/06}
        property    ValidNotAfter       : TDateTime     read  GetValidNotAfter;  {AG 02/06/06}
        property    HasExpired          : Boolean       read  GetHasexpired;     {AG 02/06/06}
        property    SelfSigned          : Boolean       read  GetSelfSigned;
    end;

    TX509Class = class of TX509Base;

    TCustomSslWSocket = class; //forward

    TX509ListSort = (xsrtIssuerFirst, xsrtIssuedFirst);
    TX509List  = class(TObject)
    private
        FList               : TComponentList;
        FX509Class          : TX509Class;
        FOwner              : TComponent;
        FLastVerifyResult   : Integer;
    protected
        function    GetCount: Integer;
        function    GetX509Base(Index: Integer): TX509Base;
        procedure   SetX509Base(Index: Integer; Value: TX509Base);
        function    GetByPX509(const X509: PX509) : TX509Base; deprecated
                    {$IFDEF COMPILER12_UP}'Use method Find'{$ENDIF};
        procedure   SetX509Class(const Value: TX509Class);
        function    GetOwnsObjects: Boolean;
        procedure   SetOwnsObjects(const Value: Boolean);
    public
        constructor Create(AOwner: TComponent; AOwnsObjects: Boolean = TRUE); reintroduce;
        destructor  Destroy; override;
        procedure   Clear;
        function    Add(X509 : PX509 = nil) : TX509Base;
        function    AddItem(AItem: TX509Base): Integer;
        function    Insert(Index: Integer; X509: PX509 = nil): TX509Base;
        procedure   InsertItem(Index: Integer; AItem: TX509Base);
        procedure   Delete(const Index: Integer);
        function    IndexOf(const X509Base : TX509Base): Integer;
        function    GetByHash(const Sha1Hash : AnsiString): TX509Base; deprecated
                    {$IFDEF COMPILER12_UP}'Use method Find'{$ENDIF};
        function    Find(const ASha1Digest: THashBytes20): TX509Base; overload;
        function    Find(const ASha1Hex: String): TX509Base; overload;
        function    Find(const AX509: PX509): TX509Base; overload;
        function    Remove(Item: TX509Base): Integer;
        function    Extract(Item: TX509Base): TX509Base;
        procedure   SortChain(ASortOrder: TX509ListSort);
        property    Count                       : Integer       read  GetCount;
        property    Items[index: Integer]       : TX509Base     read  GetX509Base
                                                                write SetX509Base; default;
        property    X509Class                   : TX509Class    read  FX509Class
                                                                write SetX509Class;
        property    LastVerifyResult            : Integer       read  FLastVerifyResult;
        property    OwnsObjects                 : Boolean       read  GetOwnsObjects
                                                                write SetOwnsObjects;
    end;

    TSslContextRemoveSession = procedure(Sender: TObject;
                                         SslSession : Pointer) of object;
    // SSL Version selection
    TSslVersionMethod = (sslV2,
                         sslV2_CLIENT,
                         sslV2_SERVER,
                         sslV3,
                         sslV3_CLIENT,
                         sslV3_SERVER,
                         sslTLS_V1,
                         sslTLS_V1_CLIENT,
                         sslTLS_V1_SERVER,
                         sslV23,
                         sslV23_CLIENT,
                         sslV23_SERVER);

    TSslVerifyPeerMode = (SslVerifyMode_NONE,
                          SslVerifyMode_PEER,
                          SslVerifyMode_FAIL_IF_NO_PEER_CERT,
                          SslVerifyMode_CLIENT_ONCE);
    TSslVerifyPeerModes = set of TSslVerifyPeerMode;

    TSslVerifyFlag  = (
                      sslX509_V_FLAG_CB_ISSUER_CHECK,
                      sslX509_V_FLAG_USE_CHECK_TIME,
                      sslX509_V_FLAG_CRL_CHECK,
                      sslX509_V_FLAG_CRL_CHECK_ALL,
                      sslX509_V_FLAG_IGNORE_CRITICAL,
                      sslX509_V_FLAG_X509_STRICT,
                      sslX509_V_FLAG_ALLOW_PROXY_CERTS);
    TSslVerifyFlags = set of TSslVerifyFlag;

    TSslOption  = (sslOpt_CIPHER_SERVER_PREFERENCE,
                   sslOpt_MICROSOFT_SESS_ID_BUG,
                   sslOpt_NETSCAPE_CHALLENGE_BUG,
                   sslOpt_NETSCAPE_REUSE_CIPHER_CHANGE_BUG,
                   sslOpt_SSLREF2_REUSE_CERT_TYPE_BUG,
                   sslOpt_MICROSOFT_BIG_SSLV3_BUFFER,
                   sslOpt_MSIE_SSLV2_RSA_PADDING,
                   sslOpt_SSLEAY_080_CLIENT_DH_BUG,
                   sslOpt_TLS_D5_BUG,
                   sslOpt_TLS_BLOCK_PADDING_BUG,
                   sslOpt_TLS_ROLLBACK_BUG,
                   sslOpt_DONT_INSERT_EMPTY_FRAGMENTS,
                   sslOpt_SINGLE_DH_USE,
                   sslOpt_EPHEMERAL_RSA,
                   sslOpt_NO_SSLv2,
                   sslOpt_NO_SSLv3,
                   sslOpt_NO_TLSv1,
                   sslOpt_PKCS1_CHECK_1,
                   sslOpt_PKCS1_CHECK_2,
                   sslOpt_NETSCAPE_CA_DN_BUG,
                   //sslOP_NO_TICKET,
                   sslOpt_NO_SESSION_RESUMPTION_ON_RENEGOTIATION, // 12/09/05
                   sslOpt_NETSCAPE_DEMO_CIPHER_CHANGE_BUG,
                   sslOpt_ALLOW_UNSAFE_LEGACY_RENEGOTIATION);  // Since OSSL 0.9.8n
    TSslOptions = set of TSslOption;

    TSslSessCacheMode = (//sslSESS_CACHE_OFF,
                         sslSESS_CACHE_CLIENT,
                         sslSESS_CACHE_SERVER,
                         //sslSESS_CACHE_BOTH,
                         sslSESS_CACHE_NO_AUTO_CLEAR,
                         sslSESS_CACHE_NO_INTERNAL_LOOKUP,
                         sslSESS_CACHE_NO_INTERNAL_STORE{,
                         sslSESS_CACHE_NO_INTERNAL});
    TSslSessCacheModes = set of TSslSessCacheMode;

    TSslSessionIdContext = String;//[SSL_MAX_SSL_SESSION_ID_LENGTH];

    {
    TSslX509Trust = (ssl_X509_TRUST_NOT_DEFINED, // Custom value
                     ssl_X509_TRUST_COMPAT,
                     ssl_X509_TRUST_SSL_CLIENT,
                     ssl_X509_TRUST_SSL_SERVER,
                     ssl_X509_TRUST_EMAIL,
                     ssl_X509_TRUST_OBJECT_SIGN,
                     ssl_X509_TRUST_OCSP_SIGN,
                     ssl_X509_TRUST_OCSP_REQUEST); }

{$IFNDEF OPENSSL_NO_ENGINE}
    ESslEngineError = class(Exception);
    TSslEngineState = (esClosed, esOpen, esInit);
    TSslEngineCtxCapabilities = set of (eccLoadPrivKey, eccLoadPubKey{, eccLoadClientCert});
    TSslEngine = class(TSslBaseComponent)
    private
        FEngine           : PEngine;
        FNameID           : String;
        FState            : TSslEngineState;
        FCtxCapabilities  : TSslEngineCtxCapabilities;
        FLastErrorMsg     : String;
        FKeyID            : String;
        procedure SetNameID(const Value: String);
    public
        destructor Destroy; override;
        function  Open: Boolean;
        function  Control(const Cmd, Arg: String): Boolean;
        procedure Close;
        function  Init: Boolean;
        property  E : PEngine read FEngine;
        property  State : TSslEngineState read FState;
        property  LastErrorMsg : String read FLastErrorMsg write FLastErrorMsg;
    published
        property  KeyID : String read FKeyID write FKeyID;
        property  NameID : String read FNameID write SetNameID;
        property  CtxCapabilities : TSslEngineCtxCapabilities read FCtxCapabilities write FCtxCapabilities;
    end;
{$ENDIF}

    ESslContextException = class(Exception);

    TInfoExtractMode = (emCert, {emKey,} emCRL);
    // TSslCertKeyFormat = (ckfPem {$IFNDEF OPENSSL_NO_ENGINE}, ckfEngine {$ENDIF}); {ckfPkcs12,}
    TSslContext = class(TSslBaseComponent)
    protected
        FSslCtx                     : PSSL_CTX;
        FSslVersionMethod           : TSslVersionMethod;
        FSslCertFile                : String;
        FSslPassPhrase              : String;
        FSslPrivKeyFile             : String;
        FSslCAFile                  : String;
        FSslCAPath                  : String;
        FSslCRLFile                 : String;
        FSslCRLPath                 : String;
        //FSslIntermCAFile            : String;
        //FSslIntermCAPath            : String;
        FSslVerifyPeer              : Boolean;
        FSslVerifyDepth             : Integer;
        FSslVerifyFlags             : Integer;
        FSslOptionsValue            : Longint;
        FSslCipherList              : String;
        FSslSessCacheModeValue      : Longint;
        FSslSessionCacheSize        : Longint;
        FSslSessionTimeout          : Longword;
        FSslDefaultSessionIDContext : TSslSessionIdContext;
        FOnRemoveSession            : TSslContextRemoveSession;
        FSslVerifyPeerModes         : TSslVerifyPeerModes;
        FSslVerifyPeerModesValue    : Integer;
        //FSslX509Trust               : TSslX509Trust;
        FOnBeforeInit               : TNotifyEvent;
        //FSslKeyFormat             : TSslCertKeyFormat;
    {$IFNDEF OPENSSL_NO_ENGINE}
        FAutoEnableBuiltinEngines   : Boolean;
        FCtxEngine                  : TSslEngine;
    {$ENDIF}
{$IFNDEF NO_SSL_MT}
        FLock                       : TIcsCriticalSection;
        procedure Lock; {$IFDEF USE_INLINE} inline; {$ENDIF}
        procedure Unlock; {$IFDEF USE_INLINE} inline; {$ENDIF}
{$ENDIF}
        function  InitializeCtx : PSSL_CTX;
        procedure SetSslCertFile(const Value : String);
        procedure SetSslPassPhrase(const Value : String);
        procedure SetSslPrivKeyFile(const Value : String);
        procedure SetSslCAFile(const Value : String);
        procedure SetSslCAPath(const Value : String);
        procedure SetSslCRLFile(const Value : String);
        procedure SetSslCRLPath(const Value : String);
        procedure SetSslSessionCacheSize(Value : Longint);
        procedure SetSslOptions(Value : TSslOptions);
        function  GetSslOptions : TSslOptions;
        procedure SetSslSessCacheModes(Value : TSslSessCacheModes);
        function  GetSslSessCacheModes : TSslSessCacheModes;
        procedure SetSslCipherList(const Value : String);
        procedure SetSslVerifyPeerModes(const Value : TSslVerifyPeerModes);
        procedure SetSslVerifyPeer(const Value: Boolean);
        procedure SetSslDefaultSessionIDContext(Value: TSslSessionIdContext);
        procedure SetSslSessionTimeout(Value : Longword);
        procedure SetSslVersionMethod(Value : TSslVersionMethod);
        function  OpenFileBio(const FileName : String;
            Methode : TBioOpenMethode): PBIO;
        function  LoadStackFromInfoFile(const FileName : String;
            Mode : TInfoExtractMode): PStack;
        procedure LoadVerifyLocations(const CAFile, CAPath: String);
        procedure LoadCertFromChainFile(const FileName : String);
        procedure LoadPKeyFromFile(const FileName : String);
        //procedure DebugLogInfo(const Msg: string);        { V5.21 }
        //procedure SetSslX509Trust(const Value: TSslX509Trust);
        function  GetIsCtxInitialized : Boolean;
        function  GetSslVerifyFlags: TSslVerifyFlags;
        procedure SetSslVerifyFlags(const Value: TSslVerifyFlags);
    {$IFNDEF OPENSSL_NO_ENGINE}
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
        procedure SetCtxEngine(const Value: TSslEngine);
    {$ENDIF}
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   InitContext;
        procedure   DeInitContext;
        function    TrustCert(Cert : TX509Base): Boolean;
        procedure   LoadCrlFromFile(const Filename: String);
        procedure   LoadCrlFromPath(const Path: String);
    {$IFNDEF OPENSSL_NO_ENGINE}
        procedure   LoadPKeyFromEngine(CtxEngine: TSslEngine);
        //function    SetupEngine(Engine: String; Commands: TStrings): PENGINE;
    {$ENDIF}
        procedure   AddClientCAFromFile(const FileName: String);
        procedure   FreeNotification(AComponent: TComponent);
        procedure   RemoveFreeNotification(AComponent: TComponent);
        procedure   SetClientCAListFromFile(const FileName: String);
        property    IsCtxInitialized : Boolean read GetIsCtxInitialized;
    published
        property  SslCertFile       : String            read  FSslCertFile
                                                        write SetSslCertFile;
        property  SslPassPhrase     : String            read  FSslPassPhrase
                                                        write SetSslPassPhrase;
        property  SslPrivKeyFile    : String            read  FSslPrivKeyFile
                                                        write SetSslPrivKeyFile;
        property  SslCAFile         : String            read  FSslCAFile
                                                        write SetSslCAFile;
        property  SslCAPath         : String            read  FSslCAPath
                                                        write SetSslCAPath;
        property  SslCRLFile        : String            read  FSslCRLFile
                                                        write SetSslCRLFile;
        property  SslCRLPath        : String            read  FSslCRLPath
                                                        write SetSslCRLPath;
        {property  SslIntermCAFile   : String            read  FSslIntermCAFile
                                                        write FSslIntermCAFile;
        property  SslIntermCAPath    : String           read  FSslIntermCAPath
                                                        write FSslIntermCAPath;}
        property  SslVerifyPeer     : Boolean           read  FSslVerifyPeer
                                                        write SetSslVerifyPeer;
        property  SslVerifyDepth    : Integer           read  FSslVerifyDepth
                                                        write FSslVerifyDepth;
        property  SslVerifyFlags    : TSslVerifyFlags   read  GetSslVerifyFlags
                                                        write SetSslVerifyFlags;
        property  SslOptions        : TSslOptions       read  GetSslOptions
                                                        write SetSslOptions;
        property  SslVerifyPeerModes : TSslVerifyPeerModes
                                                    read  FSslVerifyPeerModes
                                                    write SetSslVerifyPeerModes;
        property  SslSessionCacheModes : TSslSessCacheModes
                                                    read  GetSslSessCacheModes
                                                    write SetSslSessCacheModes;
        property  SslCipherList     : String        read  FSslCipherList
                                                    write SetSslCipherList;
        property  SslVersionMethod  : TSslVersionMethod
                                                    read  FSslVersionMethod
                                                    write SetSslVersionMethod;
        property  SslSessionTimeout : Longword      read  FSslSessionTimeout
                                                    write SetSslSessionTimeout;
        property  SslSessionCacheSize : Integer
                                                    read  FSslSessionCacheSize
                                                    write SetSslSessionCacheSize;
        property  SslDefaultSessionIDContext : TSslSessionIdContext
                                                read  FSslDefaultSessionIDContext
                                                write SetSslDefaultSessionIDContext;
        {property  SslX509Trust      : TSslX509Trust         read  FSslX509Trust
                                                            write SetSslX509Trust;}
        property  OnRemoveSession   : TSslContextRemoveSession
                                                    read  FOnRemoveSession
                                                    write FOnRemoveSession;
        property  OnBeforeInit  : TNotifyEvent      read  FOnBeforeInit
                                                    write FOnBeforeInit;
        {property  SslKeyFormat : TSslCertKeyFormat
                                                     read  FSslKeyFormat
                                                     write FSslKeyFormat;}
    {$IFNDEF OPENSSL_NO_ENGINE}
        property  AutoEnableBuiltinEngines : Boolean read  FAutoEnableBuiltinEngines
                                                     write FAutoEnableBuiltinEngines;
        property  CtxEngine : TSslEngine read FCtxEngine write SetCtxEngine;
    {$ENDIF}
    end;

    {TSslState = (sslNone,
                 sslWantConnect,
                 sslConnectWantRead,
                 sslConnectWantWrite,
                 sslConnected,
                 sslAccepted,
                 sslAcceptWantRead,
                 sslAcceptWantWrite,
                 sslWriteWantRead,
                 sslWriteWantWrite,
                 sslShutdown,
                 sslShutdownWantRead,
                 sslShutdownWantWrite);
     }

    TSslState = (sslNone,  // Not yet finished, Francois, could you care about states ??
                 sslHandshakeInit,
                 sslHandshakeStarted,
                 sslHandshakeFailed,
                 sslEstablished,
                 sslInShutdown,
                 sslShutdownComplete);

  TSslVerifyPeerEvent = procedure (Sender    : TObject;
                                   var Ok    : Integer;
                                   Cert      : TX509Base) of object;
  TSslHandshakeDoneEvent = procedure (Sender            : TObject;
                                      ErrCode           : Word;
                                      PeerCert          : TX509Base;
                                      var Disconnect    : Boolean) of object;
  TSslEvent = (sslFdRead, sslFdWrite, sslFdClose);
  TSslPendingEvents = set of TSslEvent;
  TSslMode  = (sslModeClient, sslModeServer);
  //Client-side session caching
  TSslCliGetSession         = procedure(Sender          : TObject;
                                        var SslSession  : Pointer;
                                        var FreeSession : Boolean) of object;
  TSslCliNewSession         = procedure(Sender          : TObject;
                                        SslSession      : Pointer;
                                        WasReused       : Boolean;
                                        var IncRefCount : Boolean) of object;

  //Server-side session caching
  TSslSetSessionIDContext   = procedure(Sender                  : TObject;
                                        var SessionIDContext    : TSslSessionIdContext) of object;
  TSslSvrNewSession         = procedure(Sender                  : TObject;
                                        SslSession              : Pointer;
                                        SessId                  : Pointer;
                                        Idlen                   : Integer;
                                        var AddToInternalCache  : Boolean) of object;
  TSslSvrGetSession         = procedure(Sender                  : TObject;
                                        var SslSession          : Pointer;
                                        SessId                  : Pointer;
                                        Idlen                   : Integer;
                                        var IncRefCount         : Boolean) of object;

  TSslCliCertRequest        = procedure(Sender     : TObject;
                                        var Cert   : TX509Base) of object;
  TSslShutDownComplete      = procedure(Sender          : TObject;
                                        Bidirectional   : Boolean;
                                        ErrCode         : Integer) of object;
{$IFNDEF OPENSSL_NO_TLSEXT}
  TTlsExtError = (teeOk, teeAlertWarning, teeAlertFatal, teeNoAck);
{
  SSL_TLSEXT_ERR_OK                           = 0;
  SSL_TLSEXT_ERR_ALERT_WARNING                = 1;
  SSL_TLSEXT_ERR_ALERT_FATAL                  = 2;
  SSL_TLSEXT_ERR_NOACK                        = 3;
}
  TSslServerNameEvent       = procedure(Sender               : TObject;
                                        var Ctx              : TSslContext;
                                        var ErrCode          : TTlsExtError) of object;
{$ENDIF}

  TCustomSslWSocket = class(TCustomSocksWSocket)
  private
      procedure SslShutDownAsync(How: Integer); { V7.80 }
  protected
        FSslContext                 : TSslContext;
        FOnSslSvrNewSession         : TSslSvrNewSession;
        FOnSslSvrGetSession         : TSslSvrGetSession;
        FOnSslCliGetSession         : TSslCliGetSession;
        FOnSslCliNewSession         : TSslCliNewSession;
        FOnSslSetSessionIDContext   : TSslSetSessionIDContext;
    {$IFNDEF OPENSSL_NO_TLSEXT}
        FOnSslServerName            : TSslServerNameEvent;
    {$ENDIF}
        FOnSslCliCertRequest        : TSslCliCertRequest;
        FX509Class                  : TX509Class;
        FSslCertChain               : TX509List;
        FSslPeerCert                : TX509Base;
        FSslMode                    : TSslMode;
        //FTriggerCount               : Integer; //Test
        FSslBufList                 : TIcsBufferHandler;
        FExplizitSsl                : Boolean;
        bSslAllSent                 : Boolean;
        FMayTriggerFD_Read          : Boolean;
        FMayTriggerFD_Write         : Boolean;
        FMayTriggerDoRecv           : Boolean;
        FMayTriggerSslTryToSend     : Boolean;
        //FHandShakeDoneInvoked       : Boolean;
        FCloseCalled                : Boolean;
        //FCloseReceived              : Boolean;
        FPendingSslEvents           : TSslPendingEvents;
        FSslIntShutDown             : Integer;
        FShutDownHow                : Integer;
        FSslEnable                  : Boolean;
        //FSslEstablished           : Boolean;
        FLastSslError               : Integer;
        FSslInRenegotiation         : Boolean;    // <= 01/01/06
        FSslBioWritePendingBytes    : Integer;
        FSendPending                : Boolean;
        FSslBiShutDownFlag          : Boolean;    // <= 01/08/06
        FOnSslShutDownComplete      : TSslShutDownComplete;
        FNetworkError               : Integer;
        FSslInitialized             : Boolean;
        FInHandshake                : Boolean;
        FHandshakeDone              : Boolean;
        FSslHandshakeErr            : Integer;       { V8.14  }
        FSslHandshakeRespMsg        : string;        { V8.14  }
        FSslVersNum                 : Integer;        //12/09/05
        FSSLState                   : TSslState;
        FSsl_In_CB                  : Boolean;
        FSsl                        : PSSL;
        FSslBio                     : PBIO;
        FIBio                       : PBIO;
        FNBio                       : PBIO;
        FSslAcceptableHosts         : TStrings;
        FSslVerifyResult            : Integer;
        FSslVersion                 : String;
        FSslCipher                  : String;
        FSslCipherDesc              : String;       { V8.14  }
        FSslEncryption              : String;       { V8.14  }
        FSslKeyExchange             : String;       { V8.14  }
        FSslMessAuth                : String;       { V8.14  }
        FSslTotalBits               : Integer;
        FSslSecretBits              : Integer;
        FSslSupportsSecureRenegotiation : Boolean;
        FMsg_WM_TRIGGER_DATASENT    : UINT;
        FMsg_WM_SSL_ASYNCSELECT     : UINT;
        FMsg_WM_RESET_SSL           : UINT;
        FMsg_WM_BI_SSL_SHUTDOWN     : UINT;
        FMsg_WM_TRIGGER_SSL_SHUTDOWN_COMPLETED : UINT;
        FOnSslVerifyPeer            : TSslVerifyPeerEvent;
        FOnSslHandshakeDone         : TSslHandshakeDoneEvent;
        FHandShakeCount             : Integer;
    {$IFNDEF OPENSSL_NO_TLSEXT}
        FSslServerName              : String;
    {$ENDIF}
        //procedure   SetSslEnable(const Value: Boolean); virtual;
        procedure   RaiseLastOpenSslError(EClass          : ExceptClass;
                                          Dump            : Boolean = FALSE;
                                          const CustomMsg : String  = ''); virtual;
        function  SocketDataPending : Boolean;
        procedure InternalShutdown(How: Integer);
        procedure PutDataInSslBuffer(Data: Pointer; Len: Integer);
        procedure DeleteBufferedSslData;
        function  GetRcvdCount : LongInt; override;
        procedure WMSslBiShutDown(var msg: TMessage);
        procedure WMSslASyncSelect(var msg: TMessage);
        procedure WMTriggerSslShutDownComplete(var msg: TMessage);
        procedure Do_SSL_FD_READ(var Msg: TMessage);
        function  TriggerEvent(Event: TSslEvent; ErrCode: Word): Boolean;

        procedure AssignDefaultValue; override;
        procedure Do_FD_CONNECT(var Msg : TMessage); override;
        procedure Do_FD_READ(var Msg : TMessage); override;
        procedure Do_FD_WRITE(var Msg : TMessage); override;
        procedure Do_FD_CLOSE(var Msg : TMessage); override;
        procedure Do_FD_ACCEPT(var Msg : TMessage); override;
        //procedure WMSslHandshakeDone(var msg: TMessage); message WM_TRIGGER_SSLHANDSHAKEDONE;
        function  SslShutdownCompleted(How: Integer) : Boolean;
        function  DoRecv(var Buffer : TWSocketData;
                         BufferSize : Integer;
                         Flags      : Integer) : Integer; override;
        procedure TryToSend; override;
        procedure InitializeSsl;
        procedure FinalizeSsl;
        procedure InitSSLConnection(ClientMode : Boolean; pSSLContext : PSSL_CTX = nil);
        //function  LoadCertificate(out ErrMsg : String) : Boolean;
        procedure DupConnected; override;
        procedure InternalClose(bShut : Boolean; Error : Word); override;
        procedure InternalAbort(ErrCode : Word); override;
        procedure WndProc(var MsgRec: TMessage); override;
        procedure SetSslAcceptableHosts(Value : TStrings);
        procedure TriggerEvents;
        procedure TriggerSessionConnected(ErrCode : Word); override;
        procedure TriggerSslHandshakeDone(ErrCode : Word); virtual;
        procedure TriggerSslVerifyPeer(var Ok     : Integer;
                                       Cert       : TX509Base); virtual;
        procedure TriggerSslCliNewSession; virtual;
        procedure SetSslContext(const Value: TSslContext);
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
        procedure TriggerSslShutDownComplete(ErrCode: Integer); virtual;
        function  MsgHandlersCount : Integer; override;
        procedure AllocateMsgHandlers; override;
        procedure FreeMsgHandlers; override;
        function  GetSslPeerCert : TX509Base;
        property  X509Class : TX509Class read FX509Class write FX509Class;
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        procedure   ResetSSL;
        procedure   Listen; override;
        function    Accept : TSocket; override;
        procedure   Close; override;
        procedure   Dup(NewHSocket: TSocket); override;
        procedure   ThreadAttach; override;
        procedure   Resume; override;
        //procedure DoSslShutdown;
        procedure   ResetSslDelayed;
        procedure   SslBiShutDownAsync;
        function    SslStartRenegotiation : Boolean;
        function    SslRenegotiatePending : Boolean;
        function    SslSessionReused : Boolean;
        procedure   Shutdown(How : Integer); override;
        procedure   PutDataInSendBuffer(Data : TWSocketData; Len : Integer); override;
        procedure   StartSslHandshake;
        procedure   AcceptSslHandshake;
        procedure   SetAcceptableHostsList(const SemiColonSeparatedList : String);

        property    LastSslError       : Integer          read FLastSslError;
        property    ExplizitSsl        : Boolean          read  FExplizitSsl
                                                          write FExplizitSsl;
    {$IFNDEF OPENSSL_NO_TLSEXT}
        property    SslServerName      : String           read  FSslServerName
                                                          write FSslServerName;
    {$ENDIF}
        property  OnSslShutDownComplete : TSslShutDownComplete
                                               read   FOnSslShutDownComplete
                                               write  FOnSslShutDownComplete;
        property  SSL                :  PSsl              read  FSsl;
                                                          //write FSsl;
        property  SslInRenegotiation : Boolean            read  FSslInRenegotiation; //<= 01/01/06 AG
        property  SslEnable          : Boolean            read  FSslEnable
                                                          write FSslEnable;
        //property  SslEstablished      : Boolean           read  FSslEstablished;
        property  SslState            : TSslState         read  FSslState;
        property  SslContext          : TSslContext       read  FSslContext
                                                          write SetSslContext;
        property  SslCertChain        : TX509List         read  FSslCertChain;

        property  OnSslVerifyPeer : TSslVerifyPeerEvent   read  FOnSslVerifyPeer
                                                          write FOnSslVerifyPeer;
        property  OnSslCliCertRequest : TSslCliCertRequest
                                                          read  FOnSslCliCertRequest
                                                          write FOnSslCliCertRequest;
        property  OnSslHandshakeDone : TSslHandshakeDoneEvent
                                                          read  FOnSslHandshakeDone
                                                          write FOnSslHandshakeDone;
        property  OnSslSvrNewSession : TSslSvrNewSession  read  FOnSslSvrNewSession
                                                          write FOnSslSvrNewSession;
        property  OnSslSvrGetSession : TSslSvrGetSession  read  FOnSslSvrGetSession
                                                          write FOnSslSvrGetSession;
        property  OnSslCliGetSession : TSslCliGetSession
                                                          read  FOnSslCliGetSession
                                                          write FOnSslCliGetSession;
        property  OnSslCliNewSession : TSslCliNewSession  read  FOnSslCliNewSession
                                                          write FOnSslCliNewSession;
        property  OnSslSetSessionIDContext : TSslSetSessionIDContext
                                                          read  FOnSslSetSessionIDContext
                                                          write FOnSslSetSessionIDContext;
    {$IFNDEF OPENSSL_NO_TLSEXT}
        property  OnSslServerName    : TSslServerNameEvent
                                                          read  FOnSslServerName
                                                          write FOnSslServerName;
    {$ENDIF}
        property  SslAcceptableHosts : TStrings           read  FSslAcceptableHosts
                                                          write SetSslAcceptableHosts;
        property  SslMode            : TSslMode           read  FSslMode
                                                          write FSslMode;
        property  SslVersion    : String                  read  FSslVersion;
        property  SslCipher     : String                  read  FSslCipher;
        property  SslTotalBits  : Integer                 read  FSslTotalBits;
        property  SslSecretBits : Integer                 read  FSslSecretBits;
        property  SslPeerCert   : TX509Base               read  GetSslPeerCert;
        property  SslHandshakeErr      : Integer          read  FSslHandshakeErr;        { V8.14  }
        property  SslHandshakeRespMsg  : string           read  FSslHandshakeRespMsg;    { V8.14  }
        property  SslCipherDesc  : String                 read  FSslCipherDesc;          { V8.14  }
        property  SslEncryption  : String                 read  FSslEncryption;          { V8.14  }
        property  SslKeyExchange : String                 read  FSslKeyExchange;         { V8.14  }
        property  SslMessAuth    : String                 read  FSslMessAuth;            { V8.14  }
  private
      function my_WSocket_recv(s: TSocket;
                               var Buf: TWSocketData; len, flags: Integer): Integer;
      function my_RealSend(Buf : TWSocketData; Len : Integer) : Integer;
{$IFNDEF NO_DEBUG_LOG}
      function GetMyBioName(B: PBIO) : String;
{$ENDIF}
      function my_BIO_ctrl_pending(B: PBIO) : integer;
      function my_BIO_read(B: PBIO; Buf: Pointer; Len: Integer): Integer;
      function my_BIO_write(B: PBIO; Buf: Pointer; Len: Integer): Integer;
      function my_BIO_ctrl(bp: PBIO; Cmd: Integer; LArg: LongInt; PArg: Pointer): LongInt;
      function my_BIO_ctrl_get_write_guarantee(b: PBIO): Integer;
      function my_BIO_ctrl_get_read_request(b: PBIO): Integer;
      function my_BIO_should_retry(b: PBIO): Boolean;
      procedure HandleSslError;
  end;

//procedure OutputDebugString(const Msg: String);

var
    SslCritSect : TIcsCriticalSection;

type

{$ENDIF} // USE_SSL

  TLineLimitEvent = procedure (Sender        : TObject;
                               RcvdLength    : LongInt;
                               var ClearData : Boolean) of object;

{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
  TCustomLineWSocket = class (TCustomSslWSocket)
{$ELSE}
  TCustomLineWSocket = class (TCustomSocksWSocket)
{$ENDIF}
  protected
      FRcvdPtr             : TWSocketData;
      FRcvBufSize          : LongInt;
      FRcvdCnt             : LongInt;
      FLineEnd             : AnsiString;
      FLineMode            : Boolean;
      FLineLength          : Integer;    { When a line is available  }
      FLineLimit           : LongInt;    { Max line length we accept }
      FLineReceivedFlag    : Boolean;
      FLineFound           : Boolean;
      FLineClearData       : Boolean;
      FLineEcho            : Boolean;    { Echo received data    }
      FLineEdit            : Boolean;    { Edit received data    }
      FTimeout             : LongInt;    { Given in milliseconds }
      FTimeStop            : LongInt;    { Milliseconds          }
      FOnLineLimitExceeded : TLineLimitEvent;
      procedure   InternalAbort(ErrCode : Word); override; { V7.49 }
      procedure   WndProc(var MsgRec: TMessage); override;
      procedure   WMTriggerDataAvailable(var msg: TMessage);
      function    TriggerDataAvailable(ErrCode : Word) : Boolean; override;
      procedure   TriggerSessionClosed(Error : Word); override;
      procedure   TriggerLineLimitExceeded(Cnt: Integer;
                                           var ClearData : Boolean); virtual;
      procedure   SetLineMode(newValue : Boolean); virtual;
      procedure   EditLine(var Len : Integer); virtual;
      function    GetRcvdCount : LongInt; override;
      function    DoRecv(var Buffer : TWSocketData;
                         BufferSize : Integer;
                         Flags      : Integer) : Integer; override;
  public
      constructor Create(AOwner : TComponent); override;
      destructor  Destroy; override;
      function    SendLine(const Str : RawByteString) : Integer; {$IFDEF COMPILER12_UP} overload; {$ENDIF} virtual;
{$IFDEF COMPILER12_UP}
      function    SendLine(const Str : UnicodeString; ACodePage: LongWord) : Integer; overload; virtual;
      function    SendLine(const Str : UnicodeString) : Integer; overload; virtual;
{$ENDIF}
      property    LineLength : Integer      read  FLineLength;
      property    RcvdPtr    : TWSocketData read  FRcvdPtr;
      property    RcvdCnt    : LongInt      read  FRcvdCnt;
  published
      property LineMode : Boolean           read  FLineMode
                                            write SetLineMode
                                            default False;
      property LineLimit : LongInt          read  FLineLimit
                                            write FLineLimit
                                            default 65536;
      property LineEnd  : AnsiString        read  FLineEnd
                                            write FLineEnd;
      property LineEcho : Boolean           read  FLineEcho
                                            write FLineEcho
                                            default False;
      property LineEdit : Boolean           read  FLineEdit
                                            write FLineEdit
                                            default False;
      property OnLineLimitExceeded : TLineLimitEvent
                                            read  FOnLineLimitExceeded
                                            write FOnLineLimitExceeded;
  end;



  { DEPRECATED: DO NOT USE Synchronize, WaitUntilReady, ReadLine procedure }
  { for a new application.                                                 }
  TCustomSyncWSocket = class(TCustomLineWSocket)
  protected
      FLinePointer : ^AnsiString;
      function    Synchronize(Proc         : TWSocketSyncNextProc;
                              var DoneFlag : Boolean) : Integer; virtual;
      function    WaitUntilReady(var DoneFlag : Boolean) : Integer; virtual;
      procedure   InternalDataAvailable(Sender: TObject; Error: Word);
  public
      procedure   ReadLine(Timeout : Integer; var Buffer : AnsiString); deprecated
          {$IFDEF COMPILER12_UP}'Do not use in new applications'{$ENDIF};
  end;

{$IFDEF BUILTIN_TIMEOUT}
  TTimeoutReason = (torConnect, torIdle);
  TTimeoutEvent = procedure (Sender: TObject; Reason: TTimeoutReason) of object;
  TCustomTimeoutWSocket = class(TCustomSyncWSocket)
  private
      FTimeoutConnect         : LongWord;
      FTimeoutIdle            : LongWord;
      FTimeoutSampling        : LongWord;
      FOnTimeout              : TTimeoutEvent;
      FTimeoutTimer           : TIcsThreadTimer;
      FTimeoutConnectStartTick: LongWord;
      FTimeoutOldTimerEnabled : Boolean;
      FTimeoutKeepThreadAlive : Boolean;
      procedure TimeoutHandleTimer(Sender: TObject);
      procedure SetTimeoutSampling(const Value: LongWord);
      procedure SetTimeoutKeepThreadAlive(const Value: Boolean);
  protected
      procedure TriggerTimeout(Reason: TTimeoutReason); virtual;
      procedure TriggerSessionConnectedSpecial(Error: Word); override;
      procedure TriggerSessionClosed(Error: Word); override;
      procedure DupConnected; override;
  public
      constructor Create(AOwner: TComponent); override;
      procedure Connect; override;
      procedure TimeoutStartSampling;
      procedure TimeoutStopSampling;
      procedure ThreadAttach; override;
      procedure ThreadDetach; override;
      property  TimeoutKeepThreadAlive: Boolean    read  FTimeoutKeepThreadAlive
                                                   write SetTimeoutKeepThreadAlive
                                                   default TRUE;
  //published
      property TimeoutSampling: LongWord           read  FTimeoutSampling
                                                   write SetTimeoutSampling;
      property TimeoutConnect: LongWord            read  FTimeoutConnect
                                                   write FTimeoutConnect;
      property TimeoutIdle: LongWord read FTimeoutIdle write FTimeoutIdle;
      property OnTimeout: TTimeoutEvent read FOnTimeout write FOnTimeout;
  end;
{$ENDIF}

{$IFDEF BUILTIN_THROTTLE}
  {$IFDEF BUILTIN_TIMEOUT}
  TCustomThrottledWSocket = class(TCustomTimeoutWSocket)
  {$ELSE}
  TCustomThrottledWSocket = class(TCustomSyncWSocket)
  {$ENDIF}
  private
      FBandwidthLimit           : LongWord;  // Bytes per second, null = disabled
      FBandwidthSampling        : LongWord;  // Msec sampling interval
      FBandwidthCount           : LongWord;  // Byte counter
      FBandwidthMaxCount        : LongWord;  // Bytes during sampling period
      FBandwidthTimer           : TIcsThreadTimer;
      FBandwidthPaused          : Boolean;
      FBandwidthEnabled         : Boolean;
      FBandwidthOldTimerEnabled : Boolean;
      FBandwidthKeepThreadAlive : Boolean;
      procedure BandwidthHandleTimer(Sender: TObject);
      procedure SetBandwidthControl;
      procedure SetBandwidthLimit(const Value: LongWord);   { V7.55 }
      procedure SetBandwidthSampling(const Value: LongWord);
      procedure SetBandwidthKeepThreadAlive(const Value: Boolean);
  protected
      procedure DupConnected; override;
      function  RealSend(var Data: TWSocketData; Len : Integer) : Integer; override;
      procedure TriggerSessionConnectedSpecial(Error: Word); override;
      procedure TriggerSessionClosed(Error: Word); override;
  public
      constructor Create(AOwner: TComponent); override;
      function  Receive(Buffer: TWSocketData; BufferSize: Integer) : Integer; override;
      procedure ThreadAttach; override;
      procedure ThreadDetach; override;
      property  TimeoutKeepThreadAlive: Boolean    read  FBandwidthKeepThreadAlive
                                                   write SetBandwidthKeepThreadAlive
                                                   default TRUE;
  //published
      property BandwidthLimit       : LongWord     read  FBandwidthLimit
                                                   write SetBandwidthLimit;
      property BandwidthSampling    : LongWord     read  FBandwidthSampling
                                                   write SetBandwidthSampling;
  end;
{$ENDIF}

{$IFDEF BUILTIN_THROTTLE}
  TWSocket = class(TCustomThrottledWSocket)
{$ELSE}
  {$IFDEF BUILTIN_TIMEOUT}
  TWSocket = class(TCustomTimeoutWSocket)
  {$ELSE}
  TWSocket = class(TCustomSyncWSocket)
  {$ENDIF}
{$ENDIF}
  public
    property PortNum;
    property Handle;
    property HSocket;
    property BufSize;
    property Text;
    property AllSent;
    property PeerAddr;
    property PeerPort;
    property State;
    property DnsResult;
    property DnsResultList;
    property ReadCount;
    property WriteCount;            { V8.11 }
    property RcvdCount;
    property SocketRcvBufSize;     {AG 03/10/07}
    property SocketSndBufSize;     {AG 03/10/07}
    property OnDebugDisplay;
    property Counter;
    property HttpTunnelCurrentAuthType;
    property HttpTunnelBufferSize;
    property HttpTunnelLastResponse;
    property HttpTunnelLmCompatLevel;  { V7.86 }
    property OnAddressListChanged;
    property OnRoutingInterfaceChanged;
  published
    property Addr;
    property SocketFamily;
    property Port;
    property Proto;
    property LocalAddr;
    property LocalAddr6;
    property LocalPort;
    property MultiThreaded;
    property MultiCast;
    property MultiCastAddrStr;
    property MultiCastIpTTL;
    property FlushTimeout;
    property SendFlags;
    property LingerOnOff;
    property LingerTimeout;
    property KeepAliveOnOff;
    property KeepAliveTime;
    property KeepAliveInterval;
    property SocksLevel;
    property SocksServer;
    property SocksPort;
    property SocksUsercode;
    property SocksPassword;
    property SocksAuthentication;
    property LastError;
    property ReuseAddr;
    property ComponentOptions;
    property ListenBacklog;
    property ReqVerLow;
    property ReqVerHigh;

    property HttpTunnelAuthType;
    property HttpTunnelPassword;
    property HttpTunnelPort;
    property HttpTunnelServer;
    property HttpTunnelUsercode;
    property OnHttpTunnelError;
    property OnHttpTunnelConnected;

    property OnDataAvailable;
    property OnDataSent;
    property OnSendData;
    property OnSessionClosed;
    property OnSessionAvailable;
    property OnSessionConnected;
    property OnSocksConnected;
    property OnChangeState;
    { property OnLineTooLong; }
    property OnDnsLookupDone;
    property OnError;
    property OnBgException;
    property OnSocksError;
    property OnSocksAuthState;
{$IFNDEF NO_DEBUG_LOG}
    property IcsLogger;                       { V5.21 }
{$ENDIF}
  end;

  TSocksWSocket = class(TWSocket)
  end;

{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
  TSslWSocket = class(TWSocket)
  public
      property  SslVersion;
      property  SslCipher;
      property  SslTotalBits;
      property  SslSecretBits;
      property  X509Class;
      //property  SslEstablished;
      property  SslState;
{$IFNDEF OPENSSL_NO_TLSEXT}
      property  SslServerName;
      property  OnSslServerName;
{$ENDIF}
  published
{$IFNDEF NO_DEBUG_LOG}
      property IcsLogger;                      { V5.21 }
{$ENDIF}
      property  SslContext;
      property  SslEnable;
      property  SslAcceptableHosts;
      property  SslMode;
      property  OnSslVerifyPeer;
      property  OnSslHandshakeDone;
      property  OnSslCliGetSession;
      property  OnSslCliNewSession;
      property  OnSslSvrNewSession;
      property  OnSslSvrGetSession;
      property  OnSslSetSessionIDContext;
      property  OnSslShutDownComplete;
      property  OnSslCliCertRequest;
  end;
{$ENDIF}

function  HasOption(OptSet : TWSocketOptions; Opt : TWSocketOption) : Boolean;
function  RemoveOption(OptSet : TWSocketOptions; Opt : TWSocketOption) : TWSocketOptions;
function  AddOptions(Opts: array of TWSocketOption): TWSocketOptions;
{$IFDEF MSWINDOWS}
function  WinsockInfo : TWSADATA;
{$ENDIF}
function  WSocketErrorDesc(ErrCode: Integer) : String;
function  WSocketProxyErrorDesc(ErrCode : Integer) : String;
function  WSocketIsProxyErrorCode(ErrCode: Integer): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  WSocketHttpTunnelErrorDesc(ErrCode : Integer) : String;
function  WSocketSocksErrorDesc(ErrCode : Integer) : String;
function  WSocketErrorMsgFromErrorCode(ErrCode : Integer) : String;
function  WSocketGetErrorMsgFromErrorCode(ErrCode : Integer) : String;
function  GetWinsockErr(ErrCode: Integer) : String;
function  GetWindowsErr(ErrCode: Integer): String;
function  WSocketGetHostByAddr(Addr : AnsiString) : PHostEnt;
function  WSocketGetHostByName(Name : AnsiString) : PHostEnt;
function  LocalHostName : AnsiString;
function  LocalIPList(const ASocketFamily: TSocketFamily = DefaultSocketFamily;
  const AProtocol: Integer = IPPROTO_TCP) : TStrings;
procedure GetLocalIPList(AIPList: TStrings;
  const ASocketFamily: TSocketFamily = DefaultSocketFamily;
  const AProtocol: Integer = IPPROTO_TCP);
function  WSocketResolveIp(const IpAddr : AnsiString;
  const ASocketFamily: TSocketFamily = DefaultSocketFamily;
  const AProtocol: Integer = IPPROTO_TCP) : AnsiString;
function  WSocketResolveHost(InAddr : AnsiString) : TInAddr; overload;
procedure WSocketResolveHost(const AHostName: string; var AAddr: TSockAddrIn6;
                             const ASocketFamily: TSocketFamily;
                             const AProtocol: Integer); overload;
function  WSocketResolvePort(Port : AnsiString; Proto : AnsiString) : Word;
function  WSocketResolveProto(sProto : AnsiString) : Integer;
procedure WSocketForceLoadWinsock; {$IFDEF USE_INLINE} inline; {$ENDIF}
procedure WSocketCancelForceLoadWinsock; {$IFDEF USE_INLINE} inline; {$ENDIF}
procedure WSocketUnloadWinsock; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  WSocketIsDottedIP(const S : AnsiString) : Boolean;
function  WSocketIPv4ToStr(const AIcsIPv4Addr: TIcsIPv4Address): string;
function  WSocketIPv6ToStr(const AIcsIPv6Addr: TIcsIPv6Address): string; overload;
function  WSocketIPv6ToStr(const AIn6: PSockAddrIn6): string; overload;
function  WSocketStrToIPv4(const S: string; out Success: Boolean): TIcsIPv4Address;
function  WSocketStrToIPv6(const S: string; out Success: Boolean): TIcsIPv6Address; overload;
function  WSocketStrToIPv6(const S: string; out Success : Boolean; out ScopeID : LongWord): TIcsIPv6Address; overload;
{$IFDEF STILL_NEEDS_CHECK}
function  WSocketStrToMappedIPv4(const IPv4Str: string; APortNum: Word;
  AScopeIDValue: LongWord; out Success: Boolean): TSockAddrIn6;
{$ENDIF}
function  WSocketIsIPv4(const S: string): Boolean;
function  WSocketIsIP(const S: string; out ASocketFamily: TSocketFamily): Boolean;
function  WSocketIsIPEx(const S: string; out ASocketFamily: TSocketFamily): Boolean;  { V8.01 }
{ function  WSocketLoadWinsock : Boolean; 14/02/99 }
{$IFDEF MSWINDOWS}
function WSocket_WSAStartup(wVersionRequired: word;
                           var WSData: TWSAData): Integer;
function WSocket_WSACleanup : Integer;
function WSocket_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
{$ENDIF MSWINDOWS}
procedure WSocket_WSASetLastError(iError: Integer);
function WSocket_WSAGetLastError: Integer;
{$IFDEF MSWINDOWS}
function WSocket_WSAAsyncGetHostByName(HWindow: HWND; wMsg: u_int;
                                      name, buf: PAnsiChar;
                                      buflen: Integer): THandle;
function WSocket_WSAAsyncGetHostByAddr(HWindow: HWND;
                                      wMsg: u_int; addr: PAnsiChar;
                                      len, Struct: Integer;
                                      buf: PAnsiChar;
                                      buflen: Integer): THandle;
function WSocket_WSAAsyncSelect(s: TSocket; HWindow: HWND; wMsg: u_int; lEvent: Longint): Integer;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
function WSocket_WSAAsyncSelect(IEventSrc: IIcsEventSource; s: TSocket; HWindow: HWND; wMsg: u_int; lEvent: Longint): Integer;
{ Must be called whenever the socket handle was closed }
procedure WSocketSynchronizedRemoveEvents(AEventSource: IIcsEventSource; FdClosed: Boolean = False);
procedure WSocketSynchronizedEnableReadEvent(AEventSource: IIcsEventSource);
procedure WSocketSynchronizedEnableAcceptEvent(AEventSource: IIcsEventSource);
{ Must be called before any call to shutdown() }
procedure WSocketSynchronizedSetShutdownCalled(IEventSrc: IIcsEventSource; How: Integer);
function WSocketGenerateObjectID: NativeInt;
{$ENDIF}

function WSocket_recv(s: TSocket;
                      var Buf: TWSocketData; len, flags: Integer): Integer;
function WSocket_recvfrom(s: TSocket;
                         var Buf: TWSocketData; len, flags: Integer;
                         var from: TSockAddr;
                         var fromlen: Integer): Integer;
function WSocket_getservbyname(name, proto: PAnsiChar): PServEnt;
function WSocket_getprotobyname(name: PAnsiChar): PProtoEnt;
function WSocket_gethostbyname(name: PAnsiChar): PHostEnt;
function WSocket_gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt;
function WSocket_gethostname(out name: AnsiString): Integer;
function WSocket_socket(af, Struct, protocol: Integer): TSocket;
function WSocket_shutdown(s: TSocket; how: Integer): Integer;
function WSocket_setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar;
                            optlen: Integer): Integer; overload;
function WSocket_setsockopt(s: TSocket; level, optname: Integer; var optval: TLinger;
                            optlen: Integer): Integer; overload;
function WSocket_getsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar;
                            var optlen: Integer): Integer;
function WSocket_sendto(s: TSocket; var Buf : TWSocketData; len, flags: Integer;
                        var addrto: TSockAddr;
                        tolen: Integer): Integer;
function WSocket_send(s: TSocket; var Buf : TWSocketData; len, flags: Integer): Integer;
function WSocket_ntohs(netshort: u_short): u_short;
function WSocket_ntohl(netlong: u_long): u_long;
function WSocket_listen(s: TSocket; backlog: Integer): Integer;
function WSocket_ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
{$IFDEF MSWINDOWS}
function WSocket_WSAIoctl(s                 : TSocket; IoControlCode : DWORD;
                          InBuffer          : Pointer; InBufferSize  : DWORD;
                          OutBuffer         : Pointer; OutBufferSize : DWORD;
                          var BytesReturned : DWORD; Overlapped      : POverlapped;
                          CompletionRoutine : FARPROC): Integer;
{$ENDIF}
function WSocket_inet_ntoa(inaddr: TInAddr): AnsiString;
function WSocket_inet_addr(const cp: AnsiString): u_long;
function WSocket_htons(hostshort: u_short): u_short;
function WSocket_htonl(hostlong: u_long): u_long;
function WSocket_getsockname(s: TSocket; var name: TSockAddr;
                             var namelen: Integer): Integer;
function WSocket_getpeername(s: TSocket; var name: TSockAddr;
                             var namelen: Integer): Integer;
function WSocket_connect(s: TSocket; var name: TSockAddr;
                         namelen: Integer): Integer;
function WSocket_closesocket(s: TSocket): Integer;
function WSocket_bind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer;
function WSocket_accept(s: TSocket; addr: PSockAddr; addrlen: PInteger): TSocket;

{ * Winsock2 *}
{$IFDEF MSWINDOWS}
function  WSocket_GetAddrInfo(NodeName: PChar;ServName: PChar; Hints: PAddrInfo;
                              var Addrinfo: PAddrInfo): Integer;
function  WSocket_GetNameInfo(addr: PSockAddr; namelen: Integer; host: PChar;
                              hostlen: LongWord; serv: PChar; servlen: LongWord;
                              flags: Integer): Integer;
{$ENDIF}
{$IFDEF POSIX}
function  WSocket_GetAddrInfo(NodeName: PAnsiChar; ServName: PAnsiChar;
                            Hints: PAddrInfo; var Addrinfo: PAddrInfo): Integer;
function  WSocket_GetNameInfo(addr: PSockAddr; namelen: Integer; host: PAnsiChar;
                              hostlen: LongWord; serv: PAnsiChar; servlen: LongWord;
                              flags: Integer): Integer;
{$ENDIF}
procedure WSocket_FreeAddrInfo(ai: PAddrInfo);

function WSocket_ResolveName(const AName: string; const AReverse: Boolean;
                             const AFamily: TSocketFamily;
                             AResultList: TStrings;
                             const AProtocol: Integer): Integer;

const
    ICS_LOCAL_HOST_V4  = '127.0.0.1';
    ICS_LOCAL_HOST_V6  = '::1';
    ICS_ANY_HOST_V4    = '0.0.0.0';
    ICS_ANY_HOST_V6    = '::';
    ICS_BROADCAST_V4   = '255.255.255.255';
    ICS_BROADCAST_V6   = 'ffff::1';
    ICS_ANY_PORT       = '0';

{$IFNDEF NO_ADV_MT}
function SafeWSocketGCount : Integer;
{$ENDIF}

{$IFDEF USE_SSL}
function OpenSslErrMsg(const AErrCode: LongWord): String;
function LastOpenSslErrMsg(Dump: Boolean): AnsiString;  { V8.14 made public }
function IsSslRenegotiationDisallowed(Obj: TCustomSslWSocket): Boolean;
    {$IFDEF USE_INLINE} inline; {$ENDIF}
{$ENDIF}
{
const
    WSocketGCount   : Integer = 0;
    WSocketGForced  : boolean = FALSE;
    GReqVerLow      : BYTE    = 2;
    GReqVerHigh     : BYTE    = 2;
}
(* Icke
{$EXTERNALSYM IOC_UNIX}
    IOC_UNIX             = $00000000;       { Do not use this in Windows     }
{   IOCPARM_MASK         = $000007F7;  }    { Parameters must be < 128 bytes }
 {$EXTERNALSYM IOC_WS2}
    IOC_WS2              = $08000000;
 {$EXTERNALSYM IOC_PROTOCOL}
    IOC_PROTOCOL         = $10000000;
{   IOC_VOID             = $20000000;  }    { No parameters                  }
{   IOC_OUT              = $40000000;  }    { Copy out parameters            }
{$EXTERNALSYM IOC_IN}
    IOC_IN               = $80000000;       { Copy in parameters             }
{   IOC_INOUT            = (IOC_IN or IOC_OUT); }
{$EXTERNALSYM IOC_VENDOR}
    IOC_VENDOR           = $18000000;
    SIO_RCVALL           = IOC_IN or IOC_VENDOR or 1;
    SIO_RCVALL_MCAST     = IOC_IN or IOC_VENDOR or 2;
    SIO_RCVALL_IGMPMCAST = IOC_IN or IOC_VENDOR or 3;
    SIO_KEEPALIVE_VALS   = IOC_IN or IOC_VENDOR or 4;
    SIO_ABSORB_RTRALERT  = IOC_IN or IOC_VENDOR or 5;
    SIO_UCAST_IF         = IOC_IN or IOC_VENDOR or 6;
    SIO_LIMIT_BROADCASTS = IOC_IN or IOC_VENDOR or 7;
    SIO_INDEX_BIND       = IOC_IN or IOC_VENDOR or 8;
    SIO_INDEX_MCASTIF    = IOC_IN or IOC_VENDOR or 9;
    SIO_INDEX_ADD_MCAST  = IOC_IN or IOC_VENDOR or 10;
    SIO_INDEX_DEL_MCAST  = IOC_IN or IOC_VENDOR or 11;
*)

{$IFNDEF NO_DEBUG_LOG}
var
    __DataSocket : TCustomWSocket;
{$ENDIF}
{$IFNDEF COMPILER12_UP}
var
    CPUCount     : Integer;
{$ENDIF}

const
    ICS_SOCKS_BASEERR                   = WSABASEERR + 10000;// 20000;
    ICS_SOCKS_MAXERR                    = ICS_SOCKS_BASEERR + 17;
    ICS_HTTP_TUNNEL_MAXSTAT             = 599; // Max. HTTP status code
    ICS_HTTP_TUNNEL_BASEERR             = ICS_SOCKS_BASEERR + 1000;//21000;
    ICS_HTTP_TUNNEL_MAXERR              = ICS_HTTP_TUNNEL_BASEERR + ICS_HTTP_TUNNEL_MAXSTAT;
    ICS_HTTP_TUNNEL_PROTERR             = ICS_HTTP_TUNNEL_BASEERR;
    ICS_HTTP_TUNNEL_GENERR              = ICS_HTTP_TUNNEL_BASEERR + 1;
    ICS_HTTP_TUNNEL_VERSIONERR          = ICS_HTTP_TUNNEL_GENERR  + 1;

implementation

{ R 'OverbyteIcsWSocket.TWSocket.bmp'}
{$I Include\Ics.InterlockedApi.inc}

const
    socksNoError              = ICS_SOCKS_BASEERR;
    socksProtocolError        = ICS_SOCKS_BASEERR + 1;
    socksVersionError         = ICS_SOCKS_BASEERR + 2;
    socksAuthMethodError      = ICS_SOCKS_BASEERR + 3;
    socksGeneralFailure       = ICS_SOCKS_BASEERR + 4;
    socksConnectionNotAllowed = ICS_SOCKS_BASEERR + 5;
    socksNetworkUnreachable   = ICS_SOCKS_BASEERR + 6;
    socksHostUnreachable      = ICS_SOCKS_BASEERR + 7;
    socksConnectionRefused    = ICS_SOCKS_BASEERR + 8;
    socksTtlExpired           = ICS_SOCKS_BASEERR + 9;
    socksUnknownCommand       = ICS_SOCKS_BASEERR + 10;
    socksUnknownAddressType   = ICS_SOCKS_BASEERR + 11;
    socksUnassignedError      = ICS_SOCKS_BASEERR + 12;
    socksInternalError        = ICS_SOCKS_BASEERR + 13;
    socksDataReceiveError     = ICS_SOCKS_BASEERR + 14;
    socksAuthenticationFailed = ICS_SOCKS_BASEERR + 15;
    socksRejectedOrFailed     = ICS_SOCKS_BASEERR + 16;
    socksHostResolutionFailed = ICS_SOCKS_BASEERR + 17;

var
{$IFNDEF NO_ADV_MT}
    CritSecIpList : TIcsCriticalSection;
{$ENDIF}
    IPList        : TStrings;

{$IFDEF POSIX}
type
  { It's a singleton running one thread with a kernel event queue per  }
  { application that receives socket events and post them to the right }
  { recipients.                                                        }
  TIcsEventQueue = class;

  TIcsAsyncSocketThread = class(TThread)
  private
    FEventQueue: TIcsEventQueue;
    procedure TerminateThread;
  protected
    procedure Execute; override;
  end;

  TIcsKEventList = array of TKEvent;
  EIcsEventQueue = class(Exception);
  TIcsEventQueue = class
  strict private
    FChangeList         : TIcsKEventList;
    FThreadChangeList   : TIcsKEventList;
    FEventList          : TIcsKEventList;
    FThrdChangeListLen  : Integer;
    FEventListLen       : Integer;
    FPipeFd             : TPipeFd;
    FQueue              : Integer;
    FFreeIndex          : Integer;
    FCapacity           : Integer;
    FAsyncThread        : TIcsAsyncSocketThread;
    FQueueSection       : TIcsCriticalSection;
    FObjIdentList       : TDictionary<NativeInt, TObject>;
    FInLoop             : Boolean;
    FRequireWakeup      : Boolean;
    FInitialized        : Boolean;
 //strict private class threadvar
    //FCurrentEventQueue: TIcsEventQueue;
    procedure Grow;
    procedure AddReadEvent(FD: Integer; UData: NativeInt; Edge: Boolean);
    procedure AddWriteEvent(FD: Integer; UData: NativeInt);
    procedure RemoveReadEvent(FD: Integer; UData: NativeInt);
    procedure RemoveWriteEvent(FD: Integer; UData: NativeInt);
    procedure DisableReadEvent(FD: Integer; UData: NativeInt);
    function EnableReadEvent(FD: Integer; UData: NativeInt): Boolean;
    function Init: Boolean;
    function DeInit: Boolean;
    function CheckChangeEvent(FD: Integer; UData: NativeInt;
      const OldMask: LongWord; var NewMask: LongWord): Boolean;
    function Notify(AMsg: Byte): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
    procedure InternalRemoveEvents(IEventSrc: IIcsEventSource; FdClosed: Boolean = False);
    function InternalAsyncSelect(IEventSrc: IIcsEventSource; AWndHandle: HWND;
      AMsgID: UINT; AEvents: LongWord; AWakeupThread: Boolean): Integer;
    procedure RemoveFromObjIdentList(IEventSrc: IIcsEventSource);
    procedure AddToObjIdentList(IEventSrc: IIcsEventSource);
  {$IFDEF NEVER}
    function  KQueueAddReadEvent(FD: Integer; UData: NativeInt; Edge: Boolean): Boolean;
    function  KQueueAddWriteEvent(FD: Integer; UData: NativeInt): Boolean;
    function  KQueueRemoveReadEvent(FD: Integer): Boolean;
    function  KQueueRemoveWriteEvent(FD: Integer): Boolean;
    function  KQueueDisableReadEvent(FD: Integer; UData: NativeInt): Boolean;
    function  KQueueEnableReadEvent(FD: Integer; UData: NativeInt; Edge: Boolean): Boolean;
  {$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //class function NewInstance: TObject; override;
    //procedure FreeInstance; override;
    //procedure AfterConstruction; override;
    //procedure BeforeDestruction; override;
    function SynchronizedAsyncSelect(IEventSrc: IIcsEventSource;
      FD: Integer; AWndHandle: HWND; AMsgID: UINT; AEvents: LongWord): Integer;
    function HandleEvents: Boolean;
    function SynchronizedEnableReadEvent(IEventSrc: IIcsEventSource): Boolean;
    function SynchronizedEnableAcceptEvent(IEventSrc: IIcsEventSource): Boolean;
    function SynchronizedRemoveEvents(IEventSrc: IIcsEventSource; FdClosed: Boolean): Boolean;
    procedure SynchronizedSetShutdownCalled(IEventSrc: IIcsEventSource; How: Integer);
    function Wakeup: Boolean;
  end;

var
  GAsyncSocketQueue : TIcsEventQueue = nil;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ From OverbyteIcsWinsock.pas }
function IN6_ADDR_EQUAL(const a: PIn6Addr; const b: PIn6Addr): Boolean;
begin
    Result := CompareMem(a, b, SizeOf(TIn6Addr));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IN6ADDR_ISANY(sa: PSockAddrIn6): Boolean;
begin
    if sa <> nil then begin
        with sa^ do begin
            Result := (sin6_family = AF_INET6) and
                      (PLongWord(@sin6_addr.s6_addr[0])^ = 0) and
                      (PLongWord(@sin6_addr.s6_addr[4])^ = 0) and
                      (PLongWord(@sin6_addr.s6_addr[8])^ = 0) and
                      (PLongWord(@sin6_addr.s6_addr[12])^ = 0);
        end;
    end
    else
      Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF POSIX}

type
  TIcsAsyncDnsLookupRequestState = (lrsNone, lrsInWork, lrsAlready);
  TIcsAsyncDnsLookupRequest = class(TObject)
  private
    FWndHandle    : HWND;
    FMsgID        : UINT;
    FSocketFamily : TSocketFamily;
    FProtocol     : Integer;
    FState        : TIcsAsyncDnsLookupRequestState;
    FReverse      : Boolean;
    FCanceled     : Boolean;
    FLookupName   : string;
    FResultList   : TStrings;
  public
    property ResultList: TStrings read FResultList;
  end;

  TIcsAsyncDnsLookupThread = class;
  { TIcsAsyncDnsLookup provides async name resolution with new API, IPv6 and IPv4 }
  TIcsAsyncDnsLookup = class(TObject)
  private
    FThreads      : TList;
    FQueue        : TList;
    FMaxThreads   : Integer;
    FMinThreads   : Integer;
    FQueueLock    : TIcsCriticalSection;
    FThreadsLock  : TIcsCriticalSection;
    FDestroying   : Boolean;
    FThreadIdleTimeoutMsec : LongWord;
    procedure LockQueue;
    procedure UnlockQueue;
    procedure LockThreadList;
    procedure UnlockThreadList;
    function ExecAsync(AWnd: HWND; AMsgID: UINT; ASocketFamily: TSocketFamily;
      const AName: string; AReverse: Boolean; AProtocol: Integer): THandle;
    function GetNextRequest(AThread: TIcsAsyncDnsLookupThread): TIcsAsyncDnsLookupRequest;
    function RemoveRequest(AReq: TIcsAsyncDnsLookupRequest): Boolean;
    function CancelAsyncRequest(AReq: THandle): Integer;
  public
    constructor Create(const AMaxThreads: Integer; const AMinThreads: Integer = 0;
      const AThreadIdleTimeoutSec: LongWord = 60);
    destructor Destroy; override;
    procedure SetMinMaxThreads(AMinThreads, AMaxThreads: Byte);
  end;

  TIcsAsyncDnsLookupThread = class(TThread)
  private
    FEvent         : TEvent;
    FBusy          : Boolean;
    FDnsLookup     : TIcsAsyncDnsLookup;
    FDnsResultList : TStringList;
  protected
    procedure Execute; override;
  public
    constructor Create(ADnsLookup: TIcsAsyncDnsLookup); reintroduce;
    destructor Destroy; override;
  end;

  TThreadStoreTree = class (TIcsAvlPointerTree)
  protected
    { If Data1 < Data2 return False otherwise True }
    function  CompareData(Data1, Data2: Pointer): Boolean; override;
    { Return True if Data1 equals Data2 otherwise False }
    function  SameData(Data1, Data2: Pointer): Boolean; override;
    procedure Notification(Data: Pointer; Action: TIcsAvlTreeNotification); override;
  end;

  TThreadStoreItem = record
    ThreadID  : THandle;
    RefCnt    : Integer;
    Data      : Pointer;
  end;
  PThreadStoreItem = ^TThreadStoreItem;

  TThreadLocalStore = class // Not really thread local
  private
    FTree : TThreadStoreTree;
    FTemp : TThreadStoreItem;
    FLock : TIcsCriticalSection;
  public
    constructor Create;
    destructor  Destroy; override;
    function  RegisterStore(ThreadID: THandle): PPointer;
    function  UnregisterStore(ThreadID: THandle): Pointer;
    procedure Lock;
    procedure Unlock;
  end;

var
  GThreadLocalStore : TThreadLocalStore = nil;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
procedure WSocketSynchronizedRemoveEvents(AEventSource: IIcsEventSource;
  FdClosed: Boolean = False);
begin
    GAsyncSocketQueue.SynchronizedRemoveEvents(AEventSource, FdClosed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketSynchronizedEnableReadEvent(AEventSource: IIcsEventSource);
begin
    GAsyncSocketQueue.SynchronizedEnableReadEvent(AEventSource);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketSynchronizedEnableAcceptEvent(AEventSource: IIcsEventSource);
begin
    GAsyncSocketQueue.SynchronizedEnableAcceptEvent(AEventSource);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketSynchronizedSetShutdownCalled(IEventSrc: IIcsEventSource;
  How: Integer);
begin
  GAsyncSocketQueue.SynchronizedSetShutdownCalled(IEventSrc, How);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsDigit(Ch : AnsiChar) : Boolean;
begin
    Result := (ch >= '0') and (ch <= '9');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF COMPILER12_UP}
function GetCpuCount: Integer;
var
    SysInfo : TSystemInfo;
begin
    GetSystemInfo(SysInfo);
    Result := SysInfo.dwNumberOfProcessors;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check for a valid numeric dotted IP address such as 192.161.65.25         }
{ Accept leading and trailing spaces.                                       }
function WSocketIsDottedIP(const S : AnsiString) : Boolean;
var
    I          : Integer;
    DotCount   : Integer;
    NumVal     : Integer;
begin
    Result     := FALSE;
    DotCount   := 0;
    NumVal     := 0;
    I          := 1;

    { Skip leading spaces }
    while (I <= Length(S)) and (S[I] = ' ') do
        Inc(I);
    { Can't begin with a dot }
    if (I <= Length(S)) and (S[I] = '.') then
        Exit;
    { Scan full string }
    while I <= Length(S) do begin
        if S[I] = '.' then begin
            Inc(DotCount);
            if (DotCount > 3) or (NumVal > 255) then
                Exit;
            NumVal := 0;
            { A dot must be followed by a digit }
            if (I >= Length(S)) or (not (AnsiChar(S[I + 1]) in ['0'..'9'])) then
                Exit;
        end
        else if AnsiChar(S[I]) in ['0'..'9'] then
            NumVal := NumVal * 10 + Ord(S[I]) - Ord('0')
        else begin
            { Not a digit nor a dot. Accept spaces until end of string }
            while (I <= Length(S)) and (S[I] = ' ') do
                Inc(I);
            if I <= Length(S) then
                Exit;  { Not a space, do not accept }
            break;     { Only spaces, accept        }
        end;
        Inc(I);
    end;
    { We must have exactly 3 dots }
    if (DotCount <> 3) or (NumVal > 255) then
        Exit;
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketIsIPv4(const S: string): Boolean;
var
    I      : Integer;
    DotCnt : Integer;
    NumVal : Integer;
    Ch     : Char;
    P      : PChar;
begin
    Result := FALSE;
    DotCnt := 0;
    NumVal := -1;
    P      := PChar(S);
    for I := 1 to Length(S) do
    begin
        Ch := P[I - 1];
        case Ch of
          '.' :
              begin
                  Inc(DotCnt);
                  if (DotCnt > 3) or (NumVal = -1) then
                      Exit;
                  NumVal := -1;
              end;
          '0'..'9':
              begin
                  if NumVal = -1 then
                      NumVal := Ord(Ch) - Ord('0')
                  else
                      NumVal := NumVal * 10 + Ord(Ch) - Ord('0');
                  if NumVal > 255 then
                      Exit;
              end;
          else
              Exit;
        end;
    end;

    Result := DotCnt = 3;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Byte order translated }
function WSocketIPv4ToStr(const AIcsIPv4Addr: TIcsIPv4Address): string;
begin
{$IFNDEF BIG_ENDIAN}
    Result := IntToStr(AIcsIPv4Addr and $FF)+ '.' +
              IntToStr((AIcsIPv4Addr shr  8) and $FF) + '.' +
              IntToStr((AIcsIPv4Addr shr 16) and $FF) + '.' +
              IntToStr((AIcsIPv4Addr shr 24) and $FF);
{$ELSE}
    Result := IntToStr((AIcsIPv4Addr shr 24) and $FF) + '.' +
              IntToStr((AIcsIPv4Addr shr 16) and $FF) + '.' +
              IntToStr((AIcsIPv4Addr shr  8) and $FF) + '.' +
              IntToStr(AIcsIPv4Addr and $FF);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Byte order translated }
function WSocketStrToIPv4(const S: string; out Success: Boolean): TIcsIPv4Address;
var
    I          : Integer;
    DotCount   : Integer;
    NumVal     : Integer;
    Len        : Integer;
    Ch         : Char;
    Bytes      : array [0..3] of Byte;
begin
    Result    := TIcsIPv4Address($FFFFFFFF);
    Success   := FALSE;
    Len       := Length(S);
    if Len < 6 then
        Exit;
    DotCount  := 0;
    NumVal    := -1;
    for I := 1 to Len do
    begin
        Ch := S[I];
        case Ch of
          '.' :
              begin
                  if (NumVal > -1) and (DotCount < 3) then
                      Bytes[DotCount] := NumVal
                  else
                      Exit;
                  Inc(DotCount);
                  NumVal := -1;
              end;
          '0'..'9':
              begin
                  if NumVal < 0 then
                      NumVal := Ord(Ch) - Ord('0')
                  else
                      NumVal := NumVal * 10 + Ord(Ch) - Ord('0');
                  if NumVal > 255 then
                      Exit;
              end;
          else
              Exit;
        end;
    end;

    if (NumVal > -1) and (DotCount = 3) then
    begin
        Bytes[DotCount] := NumVal;
    {$IFNDEF BIG_ENDIAN}
        Result := PIcsIPv4Address(@Bytes)^;
    {$ELSE}
        Result := IcsSwap32(PIcsIPv4Address(@Bytes)^);
    {$ENDIF}
        Success := TRUE;
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF STILL_NEEDS_CHECK}
function WSocketStrToMappedIPv4(const IPv4Str: string; APortNum: Word;
  AScopeIDValue: LongWord; out Success: Boolean): TSockAddrIn6;
var
    a4    : TInAddr;
    scope : TScopeID;
begin
    a4.S_addr := WSocketStrToIPv4(IPv4Str, Success);
    if Success then
    begin
        scope.Value := AScopeIDValue;
        IN6ADDR_SETV4MAPPED(@Result, @a4, scope,
            {$IFNDEF BIG_ENDIAN} IcsSwap16 {$ENDIF} (APortNum));
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Returns the scope ID as well, if not null }
function WSocketIPv6ToStr(const AIn6: PSockAddrIn6): string;
begin
    if AIn6 <> nil then
    begin
        if AIn6^.sin6_scope_id = 0 then
            Result := WSocketIPv6ToStr(PIcsIPv6Address(@AIn6^.sin6_addr)^)
        else
            Result := WSocketIPv6ToStr(PIcsIPv6Address(@AIn6^.sin6_addr)^) +
                      '%' + IntToStr(AIn6^.sin6_scope_id);
    end
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Byte order translated }
function WSocketIPv6ToStr(const AIcsIPv6Addr: TIcsIPv6Address): string;
var
    I : Integer;
    Zeros1, Zeros2 : set of Byte;
    Zeros1Cnt, Zeros2Cnt : Byte;
    OmitFlag : Boolean;
begin
    Result := '';
    Zeros1 := [];
    Zeros2 := [];
    Zeros1Cnt := 0;
    Zeros2Cnt := 0;
    for I := Low(AIcsIPv6Addr.Words) to High(AIcsIPv6Addr.Words) do
    begin
        if AIcsIPv6Addr.Words[I] = 0 then
        begin
            Include(Zeros1, I);
            Inc(Zeros1Cnt);
        end
        else if Zeros1Cnt > Zeros2Cnt then
        begin
            Zeros2Cnt := Zeros1Cnt;
            Zeros2    := Zeros1;
            Zeros1    := [];
            Zeros1Cnt := 0;
        end;
    end;
    if Zeros1Cnt > Zeros2Cnt then
    begin
        Zeros2    := Zeros1;
        Zeros2Cnt := Zeros1Cnt;
    end;

   if Zeros2Cnt = 0 then
   begin
        for I := Low(AIcsIPv6Addr.Words) to High(AIcsIPv6Addr.Words) do
        begin
            if I = 0 then
            {$IFNDEF BIG_ENDIAN}
                Result := IntToHex(IcsSwap16(AIcsIPv6Addr.Words[I]), 1)
            {$ELSE}
                Result := IntToHex(AIcsIPv6Addr.Words[I], 1)
            {$ENDIF}
            else
            {$IFNDEF BIG_ENDIAN}
                Result := Result + ':' + IntToHex(IcsSwap16(AIcsIPv6Addr.Words[I]), 1);
            {$ELSE}
                Result := Result + ':' + IntToHex(AIcsIPv6Addr.Words[I], 1);
            {$ENDIF}
        end;
    end
    else begin
        OmitFlag := FALSE;
        for I := Low(AIcsIPv6Addr.Words) to High(AIcsIPv6Addr.Words) do
        begin
            if not (I in Zeros2) then
            begin
                if OmitFlag then
                begin
                    if Result = '' then
                        Result := '::'
                    else
                        Result := Result + ':';
                    OmitFlag := FALSE;
                end;
                if I < High(AIcsIPv6Addr.Words) then
                {$IFNDEF BIG_ENDIAN}
                    Result := Result + IntToHex(IcsSwap16(AIcsIPv6Addr.Words[I]), 1) + ':'
                {$ELSE}
                    Result := Result + IntToHex(AIcsIPv6Addr.Words[I], 1) + ':'
                {$ENDIF}
                else
                {$IFNDEF BIG_ENDIAN}
                    Result := Result + IntToHex(IcsSwap16(AIcsIPv6Addr.Words[I]), 1);
                {$ELSE}
                    Result := Result + IntToHex(AIcsIPv6Addr.Words[I], 1);
                {$ENDIF}
            end
            else
                OmitFlag := TRUE;
        end;
        if OmitFlag then
        begin
            if Result = '' then
                Result := '::'
            else
                Result := Result + ':';
        end;
        if Result = '' then
            Result := '::';
    end;
    Result := IcsLowerCase(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Byte order translated }
function WSocketStrToIPv6(const S: string; out Success: Boolean): TIcsIPv6Address;
var
    ScopeID : LongWord;
begin
    Result := WSocketStrToIPv6(S, Success, ScopeID);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Byte order translated }
function WSocketStrToIPv6(
    const S     : string;
    out Success : Boolean;
    out ScopeID : LongWord): TIcsIPv6Address;
const
    Colon       = ':';
    Percent     = '%';
var
    ColonCnt    : Integer;
    I           : Integer;
    NumVal      : Integer;
    Ch          : Char;
    P           : PChar;
    SLen        : Integer;
    OmitPos     : Integer;
    OmitCnt     : Integer;
    PartCnt     : Byte;
    ScopeFlag   : Boolean;
begin
    Success     := FALSE;
    FillChar(Result.Words[0], SizeOf(Result), 0);
    SLen := Length(S);
    if (SLen < 1) or (SLen > (4 * 8) + 7) then
        Exit;
    ColonCnt := 0;
    P := PChar(S);
    for I := 0 to SLen - 1 do
        if (P[I] = Colon) then
            Inc(ColonCnt);
    if ColonCnt > 7 then
        Exit;
    OmitPos := Pos('::', S) - 1;
    if OmitPos > -1 then
        OmitCnt := 8 - ColonCnt
    else begin
        OmitCnt := 0; // Make the compiler happy
        if (P[0] = Colon) or (P[SLen - 1] = Colon) then
            Exit;
    end;
    NumVal    := -1;
    ColonCnt  := 0;
    PartCnt   := 0;
    I         := 0;
    ScopeID   := 0;
    ScopeFlag := FALSE;
    while I < SLen do
    begin
        Ch := P[I];
        case Ch of
            Percent : // scope_id / interface ID follows
                begin
                    if ScopeFlag then
                        Exit
                    else
                        ScopeFlag := TRUE;

                    PartCnt := 0;
                    if NumVal > -1 then
                    begin
                    {$IFNDEF BIG_ENDIAN}
                        Result.Words[ColonCnt] := IcsSwap16(NumVal);
                    {$ELSE}
                        Result.Words[ColonCnt] := NumVal;
                    {$ENDIF}
                        NumVal := -1;
                    end;
                end;
            Colon :
                begin
                    if ScopeFlag then
                        Exit;
                    PartCnt := 0;
                    if NumVal > -1 then
                    begin
                    {$IFNDEF BIG_ENDIAN}
                        Result.Words[ColonCnt] := IcsSwap16(NumVal);
                    {$ELSE}
                        Result.Words[ColonCnt] := NumVal;
                    {$ENDIF}
                        NumVal := -1;
                    end;
                    if (OmitPos = I) then
                    begin
                        Inc(ColonCnt, OmitCnt);
                        Inc(I);
                    end;
                    Inc(ColonCnt);
                    if ColonCnt > 7 then
                        Exit;
                end;
            '0'..'9':
                begin
                    Inc(PartCnt);
                    if NumVal < 0 then
                        NumVal := (Ord(Ch) - Ord('0'))
                    else if ScopeFlag then
                        NumVal := NumVal * 10 + (Ord(Ch) - Ord('0'))
                    else
                        NumVal := NumVal * 16 + (Ord(Ch) - Ord('0'));
                    if (NumVal > High(Word)) or (PartCnt > 4) then
                        Exit;
                end;
            'a'..'z',
            'A'..'Z' :
                begin
                    if ScopeFlag then
                        Exit;
                    Inc(PartCnt);
                    if NumVal < 0 then
                        NumVal := ((Ord(Ch) and 15) + 9)
                    else
                        NumVal := NumVal * 16 + ((Ord(Ch) and 15) + 9);
                    if (NumVal > High(Word)) or (PartCnt > 4) then
                        Exit;
                end;
            else
                Exit;
        end;
        Inc(I);
    end;

    if (NumVal > -1) and (ColonCnt > 1) then
    begin
        if not ScopeFlag then
        begin
        {$IFNDEF BIG_ENDIAN}
            Result.Words[ColonCnt] := IcsSwap16(NumVal);
        {$ELSE}
            Result.Words[ColonCnt] := NumVal;
        {$ENDIF}
        end
        else
            ScopeID := NumVal;
    end;
    Success := ColonCnt > 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketIsIP(const S: string; out ASocketFamily: TSocketFamily): Boolean;
begin
    Result := WSocketIsIPv4(S);
    if Result then
        ASocketFamily := sfIPv4
    else begin
        WSocketStrToIPv6(S, Result);
        if Result then
            ASocketFamily := sfIPv6
        else
            ASocketFamily := sfAny;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketIsIPEx(const S: string; out ASocketFamily: TSocketFamily): Boolean;   { V8.01 }
begin
    Result := WSocketIsIP(S, ASocketFamily);
    if Result then begin
        if S = ICS_ANY_HOST_V4 then
            ASocketFamily := sfAnyIPv4
        else if S = ICS_ANY_HOST_V6 then
            ASocketFamily := sfAnyIPv6;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.RaiseException(const Msg : String);
begin
    if Assigned(FOnError) then
        TriggerError                 { Should be modified to pass Msg ! }
    else
        raise ESocketException.Create(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//procedure TCustomWSocket.RaiseExceptionFmt(const Fmt : String; args : array of const);
//begin
//    if Assigned(FOnError) then
//        TriggerError
//    else
//        raise ESocketException.CreateFmt(Fmt, args);
//end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_Synchronized_WSAStartup(
    wVersionRequired: word;
    var WSData: TWSAData): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSAStartup(wVersionRequired, WSData)
   (*
    if @FWSAStartup = nil then
        @FWSAStartup := WSocketGetProc('WSAStartup');
    Result := FWSAStartup(wVersionRequired, WSData);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSACleanup : Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSACleanup;
    (*
    if @FWSACleanup = nil then
        @FWSACleanup := WSocketGetProc('WSACleanup');
    Result := FWSACleanup;
    *)
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_Synchronized_WSASetLastError(iError: Integer);
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Ics_WSASetLastError(iError);
  {$ENDIF}
  {$IFDEF POSIX}
    SetLastError(IError);
  {$ENDIF}
    (*
    if @FWSASetLastError = nil then
        @FWSASetLastError := WSocketGetProc('WSASetLastError');
    FWSASetLastError(iError);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAGetLastError: Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_WSAGetLastError;
  {$ELSE}
    Result := GetLastError;
    {$IFDEF MACOS} // Likely more mappings are required, add them here for now
      case Result of
          EPIPE : Result := WSAECONNRESET; // ?
      end;
    {$ENDIF}
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_Synchronized_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSACancelAsyncRequest(hAsyncTaskHandle);
    (*
    if @FWSACancelAsyncRequest = nil then
        @FWSACancelAsyncRequest := WSocketGetProc('WSACancelAsyncRequest');
    Result := FWSACancelAsyncRequest(hAsyncTaskHandle);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncGetHostByName(
    HWindow: HWND; wMsg: u_int;
    name, buf: PAnsiChar;
    buflen: Integer): THandle; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSAAsyncGetHostByName(HWindow, wMsg, name, buf, buflen);
    (*
    if @FWSAAsyncGetHostByName = nil then
        @FWSAAsyncGetHostByName := WSocketGetProc('WSAAsyncGetHostByName');
    Result := FWSAAsyncGetHostByName(HWindow, wMsg, name, buf, buflen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncGetHostByAddr(
    HWindow: HWND;
    wMsg: u_int; addr: PAnsiChar;
    len, Struct: Integer;
    buf: PAnsiChar;
    buflen: Integer): THandle; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSAAsyncGetHostByAddr(HWindow, wMsg, addr,
                                                      len, struct, buf, buflen);
    (*
    if @FWSAAsyncGetHostByAddr = nil then
        @FWSAAsyncGetHostByAddr := WSocketGetProc('WSAAsyncGetHostByAddr');
    Result := FWSAAsyncGetHostByAddr(HWindow, wMsg, addr, len, struct, buf, buflen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncSelect(
    s: TSocket;
    HWindow: HWND;
    wMsg: u_int;
    lEvent: Longint): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSAAsyncSelect(s, HWindow, wMsg, lEvent);
    (*
    if @FWSAAsyncSelect = nil then
        @FWSAAsyncSelect := WSocketGetProc('WSAAsyncSelect');
    Result := FWSAAsyncSelect(s, HWindow, wMsg, lEvent);
    *)
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getservbyname(name, proto: PAnsiChar): PServEnt;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_getservbyname(name, proto);
  {$ELSE}
    Result := getservbyname(name, proto);
  {$ENDIF}
    (*
    if @Fgetservbyname = nil then
        @Fgetservbyname := WSocketGetProc('getservbyname');
    Result := Fgetservbyname(name, proto);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getprotobyname(const Name: AnsiString): PProtoEnt;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_getprotobyname(PAnsiChar(Name));
  {$ELSE}
    Result := getprotobyname(PAnsiChar(Name));
  {$ENDIF}
    (*
    if @Fgetprotobyname = nil then
        @Fgetprotobyname := WSocketGetProc('getprotobyname');
    Result := Fgetprotobyname(PAnsiChar(Name));
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
function internal_gethostbyname(name: PAnsiChar): PHostEnt; cdecl;
  external libc name _PU + 'gethostbyname';
{$ENDIF}

function WSocket_Synchronized_gethostbyname(name: PAnsiChar): PHostEnt;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_gethostbyname(name);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := internal_gethostbyname(name);
  {$ENDIF}
    (*
    if @Fgethostbyname = nil then
        @Fgethostbyname := WSocketGetProc('gethostbyname');
    Result := Fgethostbyname(name);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
function internal_gethostbyaddr(addr: Pointer; Len: Integer; struct: Integer): PHostEnt; cdecl;
  external libc name _PU + 'gethostbyaddr';
{$ENDIF}

function WSocket_Synchronized_gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_gethostbyaddr(addr, len, Struct);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := internal_gethostbyaddr(addr, len, Struct);
  {$ENDIF}
    (*
    if @Fgethostbyaddr = nil then
        @Fgethostbyaddr := WSocketGetProc('gethostbyaddr');
    Result := Fgethostbyaddr(addr, len, Struct);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_gethostname(name: PAnsiChar; len: Integer): Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_gethostname(name, len);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := gethostname(name, len);
  {$ENDIF}
    (*
    if @Fgethostname = nil then
        @Fgethostname := WSocketGetProc('gethostname');
    Result := Fgethostname(name, len);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_socket(af, Struct, protocol: Integer): TSocket;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_socket(af, Struct, protocol);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := socket(af, Struct, protocol);
  {$ENDIF}
    (*
    if @FOpenSocket= nil then
        @FOpenSocket := WSocketGetProc('socket');
    Result := FOpenSocket(af, Struct, protocol);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_shutdown(s: TSocket; how: Integer): Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_shutdown(s, how);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := shutdown(s, how);
  {$ENDIF}
    (*
    if @FShutdown = nil then
        @FShutdown := WSocketGetProc('shutdown');
    Result := FShutdown(s, how);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer;
  optval: PAnsiChar; optlen: Integer): Integer; overload;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_setsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := setsockopt(s, level, optname, optval^, optlen);
  {$ENDIF}
    (*
    if @FSetSockOpt = nil then
        @FSetSockOpt := WSocketGetProc('setsockopt');
    Result := FSetSockOpt(s, level, optname, optval, optlen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer;
  var optval: TLinger; optlen: Integer): Integer; overload;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_setsockopt(s, level, optname, @optval, optlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := setsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
    (*
    if @FSetSockOpt = nil then
        @FSetSockOpt := WSocketGetProc('setsockopt');
    Result := FSetSockOpt(s, level, optname, @optval, optlen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer;
  var optval: ip_mreq; optlen: Integer): Integer; overload;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_setsockopt(s, level, optname, @optval, optlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := setsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
    (*
    if @FSetSockOpt = nil then
        @FSetSockOpt := WSocketGetProc('setsockopt');
    Result := FSetSockOpt(s, level, optname, @optval, optlen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer;
  var optval: Integer; optlen: Integer): Integer; overload;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_setsockopt(s, level, optname, @optval, optlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := setsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
    (*
    if @FSetSockOpt = nil then
        @FSetSockOpt := WSocketGetProc('setsockopt');
    Result := FSetSockOpt(s, level, optname, @optval, optlen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_setsockopt(s: TSocket; level, optname: Integer;
  var optval: TInAddr; optlen: Integer): Integer; overload;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_setsockopt(s, level, optname, @optval, optlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := setsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
    (*
    if @FSetSockOpt = nil then
        @FSetSockOpt := WSocketGetProc('setsockopt');
    Result := FSetSockOpt(s, level, optname, @optval, optlen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
  function internal_getsockopt(socket, level, option_name: Integer;
    option_value: PAnsiChar; var option_len: integer): Integer; cdecl;
    external libc name _PU + 'getsockopt';
{$ENDIF}
function WSocket_Synchronized_getsockopt(
    s: TSocket; level, optname: Integer;
    optval: PAnsiChar; var optlen: Integer): Integer;
    {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_getsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := internal_getsockopt(s, level, optname, optval, optlen);
  {$ENDIF}
    (*
    if @FGetSockOpt = nil then
        @FGetSockOpt := WSocketGetProc('getsockopt');
    Result := FGetSockOpt(s, level, optname, optval, optlen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_sendto(
    s          : TSocket;
    const Buf  : TWSocketData;
    len, flags : Integer;
    var addrto : TSockAddr;
    tolen      : Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_sendto(s, Buf^, len, flags, addrto, tolen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := sendto(s, Buf^, len, flags, Posix.SysSocket.psockaddr(@addrto)^, tolen);
  {$ENDIF}
    (*
    if @FSendTo = nil then
        @FSendTo := WSocketGetProc('sendto');
    Result := FSendTo(s, Buf^, len, flags, addrto, tolen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_send(s: TSocket; var Buf : TWSocketData;
  len, flags: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_send(s, Buf^, len, flags);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := send(s, Buf^, len, flags);
  {$ENDIF}
    (*
    if @FSend = nil then
        @FSend := WSocketGetProc('send');
    Result := FSend(s, Buf^, len, flags);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ntohs(netshort: u_short): u_short;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_ntohs(netshort);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := ntohs(netshort);
  {$ENDIF}
    (*
    if @Fntohs = nil then
        @Fntohs := WSocketGetProc('ntohs');
    Result := Fntohs(netshort);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ntohl(netlong: u_long): u_long;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_ntohl(netlong);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := ntohl(netlong);
  {$ENDIF}
    (*
    if @Fntohl = nil then
        @Fntohl := WSocketGetProc('ntohl');
    Result := Fntohl(netlong);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_listen(s: TSocket; backlog: Integer): Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_listen(s, backlog);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := listen(s, backlog);
  {$ENDIF}
    (*
    if @FListen = nil then
        @FListen := WSocketGetProc('listen');
    Result := FListen(s, backlog);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ioctlsocket(s: TSocket; cmd: LongWord; var arg: u_long): Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_ioctlsocket(s, cmd, arg);
  {$ENDIF}
  {$IFDEF POSIX}
    Result :=  Posix.StrOpts.ioctl(s, cmd, @arg);
  {$ENDIF}
    (*
    if @FIoctlSocket = nil then
        @FIoctlSocket := WSocketGetProc('ioctlsocket');
    Result := FIoctlSocket(s, cmd, arg);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_Synchronized_WSAIoctl(
    s                 : TSocket; IoControlCode : DWORD;
    InBuffer          : Pointer; InBufferSize  : DWORD;
    OutBuffer         : Pointer; OutBufferSize : DWORD;
    var BytesReturned : DWORD; Overlapped      : POverlapped;
    CompletionRoutine : FARPROC): Integer;
    {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_WSAIoctl(s, IoControlCode, InBuffer,
                        InBufferSize, OutBuffer, OutBufferSize, BytesReturned,
                        Overlapped, CompletionRoutine);
    (*
    if @FWSAIoctl = nil then
        @FWSAIoctl := WSocket2GetProc('WSAIoctl');
    Result := FWSAIoctl(s, IoControlCode, InBuffer, InBufferSize, OutBuffer,
                        OutBufferSize, BytesReturned, Overlapped, CompletionRoutine);
    *)
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_inet_ntoa(inaddr: TInAddr): PAnsiChar;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_inet_ntoa(inaddr);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := inet_ntoa(inaddr);
  {$ENDIF}
    (*
    if @FInet_ntoa = nil then
        @FInet_ntoa := WSocketGetProc('inet_ntoa');
    Result := FInet_ntoa(inaddr);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_inet_addr(const cp: AnsiString): u_long;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_inet_addr(PAnsiChar(cp));
  {$ENDIF}
  {$IFDEF POSIX}
    Result := inet_addr(PAnsiChar(cp));
  {$ENDIF}
    (*
    if @FInet_addr = nil then
        @FInet_addr := WSocketGetProc('inet_addr');
    Result := FInet_addr(PAnsiChar(cp));
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_htons(hostshort: u_short): u_short;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_htons(hostshort);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := htons(hostshort);
  {$ENDIF}
    (*
    if @Fhtons = nil then
        @Fhtons := WSocketGetProc('htons');
    Result := Fhtons(hostshort);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_htonl(hostlong: u_long): u_long;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_htonl(hostlong);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := htonl(hostlong);
  {$ENDIF}
    (*
    if @Fhtonl = nil then
        @Fhtonl := WSocketGetProc('htonl');
    Result := Fhtonl(hostlong);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getsockname(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_getsockname(s, name, namelen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := getsockname(s, Posix.SysSocket.psockaddr(@name)^, LongWord(namelen));
  {$ENDIF}
    (*
    if @FGetSockName = nil then
        @FGetSockName := WSocketGetProc('getsockname');
    Result := FGetSockName(s, name, namelen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_getpeername(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_getpeername(s, name, namelen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := getpeername(s, Posix.SysSocket.psockaddr(@name)^, LongWord(namelen));
  {$ENDIF}
    (*
    if @FGetPeerName = nil then
        @FGetPeerName := WSocketGetProc('getpeername');
    Result := FGetPeerName(s, name, namelen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_connect(
    s        : TSocket;
    var name : TSockAddr;
    namelen  : Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_connect(s, name, namelen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := connect(s, Posix.SysSocket.psockaddr(@name)^, namelen);
  {$ENDIF}
    (*
    if @FConnect= nil then
        @FConnect := WSocketGetProc('connect');
    Result := FConnect(s, name, namelen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_closesocket(s: TSocket): Integer;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_closesocket(s);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := Posix.UniStd.__close(s);
  {$ENDIF}
    (*
    if @FCloseSocket = nil then
        @FCloseSocket := WSocketGetProc('closesocket');
    Result := FCloseSocket(s);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_bind(
    s: TSocket;
    var addr: TSockAddr;
    namelen: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF POSIX}
    Result := bind(s, Posix.SysSocket.psockaddr(@addr)^, namelen);
  {$ELSE}
    Result := Ics_bind(s, addr, namelen);
  {$ENDIF}
    (*
    if @FBind = nil then
        @FBind := WSocketGetProc('bind');
    Result := FBind(s, addr, namelen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
function internal_accept(socket: Integer; address: Psockaddr;
  address_len: PInteger): Integer; cdecl;
  external libc name _PU + 'accept';
{$ENDIF}

function WSocket_Synchronized_accept(
    s: TSocket;
    addr: PSockAddr;
    addrlen: PInteger): TSocket; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_accept(s, addr, addrlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := internal_accept(s, addr, addrlen);
  {$ENDIF}
    (*
    if @FAccept = nil then
        @FAccept := WSocketGetProc('accept');
    Result := FAccept(s, addr, addrlen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_recv(s: TSocket; var Buf: TWSocketData;
  len, flags: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_recv(s, Buf^, len, flags);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := recv(s, Buf^, len, flags);
  {$ENDIF}
    (*
    if @FRecv= nil then
        @FRecv := WSocketGetProc('recv');
    Result := FRecv(s, Buf^, len, flags);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_recvfrom(
    s: TSocket;
    var Buf: TWSocketData; len, flags: Integer;
    var from: TSockAddr;
    var fromlen: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := Ics_recvfrom(s, Buf^, len, flags, from, fromlen);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := recvfrom(s, Buf^, len, flags, Posix.SysSocket.psockaddr(@from)^, LongWord(fromlen));
  {$ENDIF}
    (*
    if @FRecvFrom = nil then
        @FRecvFrom := WSocketGetProc('recvfrom');
    Result := FRecvFrom(s, Buf^, len, flags, from, fromlen);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_Synchronized_GetAddrInfo(
    NodeName    : PChar;
    ServName    : PChar;
    Hints       : PAddrInfo;
    var Addrinfo: PAddrInfo): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_GetAddrInfo(NodeName, ServName, Hints, Addrinfo);
    (*
    if @FGetAddrInfo = nil then
        @FGetAddrInfo := WSocket2GetProc(
           {$IFDEF UNICODE}'GetAddrInfoW' {$ELSE} 'getaddrinfo' {$ENDIF});
    Result := FGetAddrInfo(NodeName, ServName, Hints, Addrinfo);
    *)
end;
{$ENDIF}
{$IFDEF POSIX}
function WSocket_Synchronized_GetAddrInfo(
    NodeName    : PAnsiChar;
    ServName    : PAnsiChar;
    Hints       : PAddrInfo;
    var Addrinfo: PAddrInfo): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := GetAddrInfo(NodeName, ServName, Hints^, Addrinfo);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_Synchronized_FreeAddrInfo(ai: PAddrInfo);
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF POSIX}
    FreeAddrInfo(ai^);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    Ics_FreeAddrInfo(ai);
  {$ENDIF}
    (*
    if @FFreeAddrInfo = nil then
        @FFreeAddrInfo := WSocket2GetProc(
              {$IFDEF UNICODE}'FreeAddrInfoW' {$ELSE} 'freeaddrinfo' {$ENDIF});
    FFreeAddrInfo(ai);
    *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_Synchronized_GetNameInfo(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PChar;
    hostlen : LongWord;
    serv    : PChar;
    servlen : LongWord;
    flags   : Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := Ics_GetNameInfo(addr, namelen, host, hostlen, serv,
                                             servlen, flags);
    (*
    if @FGetNameInfo = nil then
        @FGetNameInfo := WSocket2GetProc(
                 {$IFDEF UNICODE}'GetNameInfoW' {$ELSE} 'getnameinfo' {$ENDIF});
    Result := FGetNameInfo(addr, namelen, host, hostlen, serv, servlen, flags);
    *)
end;
{$ENDIF}
{$IFDEF POSIX}
function WSocket_Synchronized_GetNameInfo(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PAnsiChar;
    hostlen : LongWord;
    serv    : PAnsiChar;
    servlen : LongWord;
    flags   : Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := GetNameInfo(Posix.SysSocket.psockaddr(addr)^, namelen, host,
                          hostlen, serv, servlen, flags);

end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ResolveName(const AName: string;
  const AReverse: Boolean; const AFamily: TSocketFamily;
  AResultList: TStrings; const AProtocol: Integer): Integer; forward;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Winsock is dynamically loaded and unloaded when needed. In some cases     }
{ you may find winsock being loaded and unloaded very often in your app     }
{ This happend for example when you dynamically create a TWSocket and       }
{ destroy a TWSocket when there is no "permanant" TWSocket (that is a       }
{ TWSocket dropped on a persitant form). It is the very inefficiant.        }
{ Calling WSocketForceLoadWinsock will increament the reference count so    }
{ that winsock will not be unloaded when the last TWSocket is destroyed.    }
procedure WSocketForceLoadWinsock;
begin
{$IFDEF MSWINDOWS}
    OverbyteIcsWinsock.ForceLoadWinsock;
{$ENDIF}
(*
{$IFDEF MSWINDOWS}
    _EnterCriticalSection(GWSockCritSect);
    try
        if not WSocketGForced then begin
            WSocketGForced := TRUE;
            Inc(WSocketGCount);
            WSocketGetProc('');
        end;
    finally
        _LeaveCriticalSection(GWSockCritSect);
    end;
{$ENDIF}
*)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Cancel the operation done with WSocketForceLoadWinsock.                   }
procedure WSocketCancelForceLoadWinsock;
begin
{$IFDEF MSWINDOWS}
    OverbyteIcsWinsock.CancelForceLoadWinsock;
{$ENDIF}
(*
{$IFDEF MSWINDOWS}
    _EnterCriticalSection(GWSockCritSect);
    try
        if WSocketGForced then begin
            WSocketGForced := FALSE;
            Dec(WSocketGCount);
            if WSocketGCount <= 0 then
                WSocketUnloadWinsock;
        end;
    finally
        _LeaveCriticalSection(GWSockCritSect);
    end;
{$ENDIF}
*)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketUnloadWinsock;
begin
{$IFDEF MSWINDOWS}
    OverbyteIcsWinsock.UnloadWinsock;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WinsockInfo : TWSADATA;
begin
    Result := OverbyteIcsWinsock.WinsockAPIInfo;
end;

{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_ADV_MT}
procedure SafeIncrementCount;
begin
  {$IFNDEF POSIX}
    EnterCriticalSection(GWSockCritSect);
    Inc(WSocketGCount);
    LeaveCriticalSection(GWSockCritSect);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure SafeDecrementCount;
begin
  {$IFDEF MSWINDOWS}
    EnterCriticalSection(GWSockCritSect);
    Dec(WSocketGCount);
    LeaveCriticalSection(GWSockCritSect);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SafeWSocketGCount : Integer;
begin
  {$IFDEF MSWINDOWS}
    EnterCriticalSection(GWSockCritSect);
    Result := WSocketGCount;
    LeaveCriticalSection(GWSockCritSect);
  {$ELSE}
    Result := 0;
  {$ENDIF}
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_WSAStartup(
    wVersionRequired : WORD;
    var WSData       : TWSAData): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAStartup(wVersionRequired, WSData);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSACleanup : Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSACleanup;
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF MSWINDOWS}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_WSASetLastError(iError: Integer);
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        WSocket_Synchronized_WSASetLastError(iError);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAGetLastError: Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAGetLastError;
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSACancelAsyncRequest(hAsyncTaskHandle);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_WSAAsyncGetHostByName(
    HWindow: HWND; wMsg: u_int;
    name, buf: PAnsiChar;
    buflen: Integer): THandle;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAAsyncGetHostByName(
                      HWindow, wMsg, name, buf, buflen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAAsyncGetHostByAddr(
    HWindow: HWND;
    wMsg: u_int; addr: PAnsiChar;
    len, Struct: Integer;
    buf: PAnsiChar;
    buflen: Integer): THandle;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAAsyncGetHostByAddr(
                       HWindow, wMsg, addr, len, struct, buf, buflen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAAsyncSelect(
    s: TSocket;
    HWindow: HWND;
    wMsg: u_int;
    lEvent: Longint): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAAsyncSelect(
                      s, HWindow, wMsg, lEvent);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF MSWINDOWS}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
function WSocket_WSAAsyncSelect(
    IEventSrc: IIcsEventSource;
    s: TSocket;
    HWindow: HWND;
    wMsg: u_int;
    lEvent: Longint): Integer;
begin
    Result := GAsyncSocketQueue.SynchronizedAsyncSelect(IEventSrc, s, HWindow, wMsg, LEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_WSAAsyncSelect(
    IEventSrc: IIcsEventSource;
    s: TSocket;
    HWindow: HWND;
    wMsg: u_int;
    lEvent: Longint): Integer;
begin
    Result := GAsyncSocketQueue.SynchronizedAsyncSelect(IEventSrc, s, HWindow, wMsg, LEvent);
end;
{$ENDIF POSIX}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getservbyname(name, proto: PAnsiChar): PServEnt;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getservbyname(name, proto);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getprotobyname(name: PAnsiChar): PProtoEnt;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getprotobyname(name);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostbyname(name: PAnsiChar): PHostEnt;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_gethostbyname(name);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostbyaddr(addr: Pointer; len, Struct: Integer): PHostEnt;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_gethostbyaddr(addr, len, Struct);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostname(out name: AnsiString): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        SetLength(Name, 256);
        Result := WSocket_Synchronized_gethostname(PAnsiChar(name), 256);
        if Result >= 0 then
            // Unicode will convert on the fly
            SetLength(Name, StrLen(PAnsiChar(Name))) // Unicode change
        else
            SetLength(Name, 0);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_socket(af, Struct, protocol: Integer): TSocket;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_socket(af, Struct, protocol);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_shutdown(s: TSocket; how: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_Shutdown(s, how);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar;
                            optlen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_setsockopt(s, level, optname, optval, optlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_setsockopt(s: TSocket; level, optname: Integer; var optval: TLinger;
                            optlen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_setsockopt(s, level, optname, optval, optlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockopt(
    s: TSocket; level, optname: Integer;
    optval: PAnsiChar; var optlen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getsockopt(s, level, optname, optval, optlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_sendto(
    s          : TSocket;
    var Buf    : TWSocketData;
    len, flags : Integer;
    var addrto : TSockAddr;
    tolen      : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_sendto(s, Buf, len, flags, addrto, tolen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_send(s: TSocket; var Buf : TWSocketData; len, flags: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_send(s, Buf, len, flags);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ntohs(netshort: u_short): u_short;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ntohs(netshort);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ntohl(netlong: u_long): u_long;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ntohl(netlong);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_listen(s: TSocket; backlog: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_listen(s, backlog);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ioctlsocket(s, cmd, arg);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_WSAIoctl(
    s                 : TSocket; IoControlCode : DWORD;
    InBuffer          : Pointer; InBufferSize  : DWORD;
    OutBuffer         : Pointer; OutBufferSize : DWORD;
    var BytesReturned : DWORD; Overlapped      : POverlapped;
    CompletionRoutine : FARPROC): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_WSAIoctl(
                      s, IoControlCode, InBuffer, InBufferSize, OutBuffer,
                      OutBufferSize, BytesReturned, Overlapped, CompletionRoutine);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_inet_ntoa(inaddr: TInAddr): AnsiString;
var
    Temp : PAnsiChar;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Temp := WSocket_Synchronized_inet_ntoa(inaddr);
        if Temp = nil then
            Result := ''
        else
            Result := Temp;
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_inet_addr(const cp: AnsiString): u_long;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_inet_addr(PAnsiChar(cp));
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_htons(hostshort: u_short): u_short;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_htons(hostshort);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_htonl(hostlong: u_long): u_long;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_htonl(hostlong);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockname(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getsockname(s, name, namelen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getpeername(
    s           : TSocket;
    var name    : TSockAddr;
    var namelen : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_getpeername(s, name, namelen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_connect(
    s        : TSocket;
    var name : TSockAddr;
    namelen  : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_connect(s, name, namelen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_closesocket(s: TSocket): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_closesocket(s);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_bind(
    s: TSocket;
    var addr: TSockAddr;
    namelen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_bind(s, addr, namelen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_accept(
    s: TSocket;
    addr: PSockAddr;
    addrlen: PInteger): TSocket;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_accept(s, addr, addrlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_recv(s: TSocket; var Buf : TWSocketData; len, flags: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_recv(s, Buf, len, flags);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_recvfrom(
    s: TSocket;
    var Buf: TWSocketData; len, flags: Integer;
    var from: TSockAddr;
    var fromlen: Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_recvfrom(s, Buf, len, flags, from, fromlen);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_GetAddrInfo(
    NodeName    : PChar;
    ServName    : PChar;
    Hints       : PAddrInfo;
    var Addrinfo: PAddrInfo): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_GetAddrInfo(NodeName, ServName, Hints, Addrinfo);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}

{$IFDEF POSIX}
function WSocket_GetAddrInfo(
    NodeName    : PAnsiChar;
    ServName    : PAnsiChar;
    Hints       : PAddrInfo;
    var Addrinfo: PAddrInfo): Integer;
begin
  Result := WSocket_Synchronized_GetAddrInfo(NodeName, ServName, Hints, Addrinfo);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_FreeAddrInfo(ai: PAddrInfo);
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        WSocket_Synchronized_FreeAddrInfo(ai);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function WSocket_GetNameInfo(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PChar;
    hostlen : LongWord;
    serv    : PChar;
    servlen : LongWord;
    flags   : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_GetNameInfo(addr, namelen, host, hostlen, serv, servlen, flags);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;
{$ENDIF}

{$IFDEF POSIX}
function WSocket_GetNameInfo(
    addr    : PSockAddr;
    namelen : Integer;
    host    : PAnsiChar;
    hostlen : LongWord;
    serv    : PAnsiChar;
    servlen : LongWord;
    flags   : Integer): Integer;
begin
  Result := WSocket_Synchronized_GetNameInfo(addr, namelen, host, hostlen, serv, servlen, flags);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketCounter.GetLastAliveTick : Cardinal;
{$IFDEF PUREPASCAL}
begin
    if FLastRecvTick > FLastSendTick then
        if FLastRecvTick > FConnectTick then
            Result := FLastRecvTick
        else
            Result := FConnectTick
    else
        if FLastSendTick > FConnectTick then
            Result := FLastSendTick
        else
            Result := FConnectTick;
{$ELSE}
asm
{$IFDEF CPUX64}
    MOV EDX, [RCX].FLastSendTick
    MOV EAX, [RCX].FConnectTick
    MOV ECX, [RCX].FLastRecvTick
    CMP EAX, EDX
    JB  @below
    MOV EDX, ECX
    JMP @more
@below:
    MOV EAX, ECX
@more:
    CMP EAX, EDX
    JB  @done
    RET
@done:
    MOV EAX, EDX
{$ELSE}
    mov ecx, [eax].FLastRecvTick
    mov edx, [eax].FLastSendTick
    mov eax, [eax].FConnectTick
    cmp eax, edx
    jb  @below
    mov edx, ecx
    jmp @more
@below:
    mov eax, ecx
@more:
    cmp eax, edx
    jb  @done
    ret
@done:
    mov eax, edx
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketCounter.SetConnected;
begin
    FLastRecvTick := 0;
    FLastSendTick := 0;
    FConnectTick  := IcsGetTickCount;
    FConnectDT    := Now;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if operation = opRemove then begin
    {$IFNDEF NO_DEBUG_LOG}
        if AComponent = FIcsLogger then                               { V5.21 }
            FIcsLogger := nil;                                        { V5.21 }
    {$ENDIF}                                                          { V5.21 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure InitializeAddr(var AAddr: TSockAddrIn6; AIPVersion: TSocketFamily);
    {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    FillChar(AAddr, SizeOf(TSockAddrIn6), 0);
    if AIPVersion = sfIPv6 then
        AAddr.sin6_family := AF_INET6
    else
        AAddr.sin6_family := AF_INET;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SizeOfAddr(const AAddr: TSockAddrIn6): Integer;
    {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    if AAddr.sin6_family = AF_INET6 then
        Result := SizeOf(TSockAddrIn6)
    else
        Result := SizeOf(TSockAddrIn);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.AssignDefaultValue;
begin
    InitializeAddr(Fsin, FSocketFamily);
    FAddrFormat         := Fsin.sin6_family;
    FCurrentAddrFamily  := AF_UNSPEC;
    FPortAssigned       := FALSE;
    FAddrAssigned       := FALSE;
    FAddrResolved       := FALSE;
    FPortResolved       := FALSE;
    FProtoResolved      := FALSE;
    FLocalPortResolved  := FALSE;

    FProtoAssigned      := TRUE;
    FProto              := IPPROTO_TCP;
    FProtoStr           := 'tcp';
    FType               := SOCK_STREAM;
    FLocalPortStr       := ICS_ANY_PORT;
    FLocalAddr6         := ICS_ANY_HOST_V6;
    FLocalAddr          := ICS_ANY_HOST_V4;
    FLingerOnOff        := wsLingerOn;
    FLingerTimeout      := 0;
    FHSocket            := INVALID_SOCKET;
    FSelectEvent        := 0;
    FState              := wsClosed;
    bAllSent            := TRUE;
    FPaused             := FALSE;
{   FReadCount          := 0;  V7.24 only reset when connection opened, not closed }
    FCloseInvoked       := FALSE;
    FFlushTimeout       := 60;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
var
    GlObjectID: NativeInt = 1;
    GLObjectIDSection: TIcsCriticalSection = nil;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketGenerateObjectID: NativeInt;
begin
    { It's not unique but should be OK for our purpose }
    GLObjectIDSection.Enter;
    try
        if GlObjectID = High(NativeInt) then
          GlObjectID := 1
        else
          Inc(GlObjectID);
        Result := GlObjectID;
    finally
        GLObjectIDSection.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Impl IIcsEventSource }
function TCustomWSocket.GetEventMask: LongWord;
begin
    Result := FPxEventMask;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetEventMask(const AValue: LongWord);
begin
    FPxEventMask := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetNotifyMessageID: UINT;
begin
    Result := FPxEventMessageID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetNotifyMessageID(const AValue: UINT);
begin
    FPxEventMessageID := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetNotifyWindow: HWND;
begin
    Result := FPxEventWindow;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetNotifyWindow(const AValue: HWND);
begin
    FPxEventWindow := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetEventState: TIcsAsyncEventState;
begin
    Result := FPxEventState;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetEventState(const AValue: TIcsAsyncEventState);
begin
    FPxEventState := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetFileDescriptor: Integer;
begin
    Result := FPxFileDescriptor;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetFileDescriptor(const AValue: Integer);
begin
    FPxFileDescriptor := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetObject: TObject;
begin
    Result := Self;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetObjectID: NativeInt;
begin
    Result := FPxObjectID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF POSIX IIcsEventSource}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.AbortComponent; { V7.35 }
begin
    try
        Abort;
    except
    end;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure handle all messages for TWSocket. All exceptions must be   }
{ handled or the application will be shutted down !                         }
{ If WndProc is overridden in descendent components, then the same exception }
{ handling *MUST* be setup because descendent component code is executed    }
{ before the base class code.                                               }
procedure TCustomWSocket.WndProc(var MsgRec: TMessage);
begin
    try
        with MsgRec do begin
            if Msg = FMsg_WM_ASYNCSELECT then
                WMASyncSelect(MsgRec)
            else if Msg = FMsg_WM_ASYNCGETHOSTBYNAME then
                WMAsyncGetHostByName(MsgRec)
            else if Msg = FMsg_WM_ASYNCGETHOSTBYADDR then
                WMAsyncGetHostByAddr(MsgRec)
            else if Msg = FMsg_WM_CLOSE_DELAYED then
                WMCloseDelayed(MsgRec)
//            else if Msg = FMsg_WM_WSOCKET_RELEASE then
//                WMRelease(MsgRec)
            else if Msg = FMsg_WM_TRIGGER_EXCEPTION then
                { This is useful to check for background exceptions            }
                { In your application, use following code to test your handler }
                { PostMessage(WSocket1.Handle, WM_TRIGGER_EXCEPTION, 0, 0);    }
                raise ESocketException.Create('Test exception in WSocket')
            else
                inherited WndProc(MsgRec);
                //Result := DefWindowProc(Handle, Msg, wParam, LParam);
        end;
    except
        on E:Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.MsgHandlersCount : Integer;
begin
    Result := 6 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_ASYNCSELECT            := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_ASYNCGETHOSTBYNAME     := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_ASYNCGETHOSTBYADDR     := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_CLOSE_DELAYED          := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_TRIGGER_EXCEPTION      := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_TRIGGER_DATA_AVAILABLE := FWndHandler.AllocateMsgHandler(Self);
//  FMsg_WM_WSOCKET_RELEASE        := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_ASYNCSELECT);
        FWndHandler.UnregisterMessage(FMsg_WM_ASYNCGETHOSTBYNAME);
        FWndHandler.UnregisterMessage(FMsg_WM_ASYNCGETHOSTBYADDR);
        FWndHandler.UnregisterMessage(FMsg_WM_CLOSE_DELAYED);
        FWndHandler.UnregisterMessage(FMsg_WM_TRIGGER_EXCEPTION);
        FWndHandler.UnregisterMessage(FMsg_WM_TRIGGER_DATA_AVAILABLE);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.AllocateSocketHWnd;
begin
    inherited AllocateHWnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DeallocateSocketHWnd;
begin
    inherited DeallocateHWnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ThreadAttach;
begin
    inherited ThreadAttach;
    if FHSocket <> INVALID_SOCKET then
        WSocket_Synchronized_WSAASyncSelect(
                                          {$IFDEF POSIX}
                                            Self,
                                          {$ENDIF}
                                            FHSocket,
                                            Handle,
                                            FMsg_WM_ASYNCSELECT, FSelectEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ThreadDetach;
begin
    if (IcsGetCurrentThreadID = FThreadID) and (FHSocket <> INVALID_SOCKET) then
        WSocket_Synchronized_WSAASyncSelect(
                                          {$IFDEF POSIX}
                                            Self,
                                          {$ENDIF}
                                            FHSocket,
                                            Handle, 0, 0);
    inherited ThreadDetach;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomWSocket.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);

    FHSocket            := INVALID_SOCKET;           { FP: 18/01/2007 }
    AllocateSocketHWnd;
    FBufHandler         := TIcsBufferHandler.Create(Self);
    FBufHandler.BufSize := 1460; {1514;}             { Default buffer size }
    FDnsResultList      := TStringList.Create;
    FMultiCastIpTTL     := IP_DEFAULT_MULTICAST_TTL;
    ListenBacklog       := 5;
    FBufferedByteCount  := 0;  { V5.20 }
    FMultiCastAddrStr   := '';
    FAddrStr            := '';
    FPortStr            := '';
    FCounterClass       := TWSocketCounter;
    FSocketFamily       := DefaultSocketFamily;
    FOldSocketFamily    := FSocketFamily;
    AssignDefaultValue;
{$IFDEF MSWINDOWS}
    EnterCriticalSection(GWSockCritSect);
    try
        Inc(WSocketGCount);
    finally
        LeaveCriticalSection(GWSockCritSect);
    end;
{$ENDIF}
{$IFDEF POSIX}
    FPxObjectID := WSocketGenerateObjectID;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomWSocket.Destroy;
begin
    try
        InternalCancelDnsLookup(TRUE); { Cancel any pending dns lookup      }
    except
        { Ignore any exception here }
    end;
    UnregisterIcsAsyncDnsLookup;

    if FState <> wsInvalidState then begin              { FPiette V7.42 }
        { wsInvalidState happend when an exception is raised early in the constructor }
        { Close the socket if not yet closed }
        if FState <> wsClosed then
            Close;
    {$IFDEF MSWINDOWS}
        EnterCriticalSection(GWSockCritSect);
        try
            Dec(WSocketGCount);
            if WSocketGCount <= 0 then begin
                WSocketUnloadWinsock;
    {           WSocketGCount := 0;  // it is set to 0 in WSocketUnloadWinsock }
            end;
        finally
            LeaveCriticalSection(GWSockCritSect);
        end;
    {$ENDIF}
    end;

    if Assigned(FBufHandler) then begin
        FBufHandler.Free;
        FBufHandler := nil;
    end;
    if Assigned(FDnsResultList) then begin
        FDnsResultList.Free;
        FDnsResultList := nil;
    end;

    if Assigned(FCounter) then begin
        FCounter.Free;
        FCounter := nil;
    end;
{$IFNDEF NO_DEBUG_LOG}
    { Removes TIcsLogger's free notification in a thread-safe way }
    SetIcsLogger(nil);
{$ENDIF}
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.CreateCounter;
begin
    if Assigned(FCounter) then
        FreeAndNil(FCounter);
    FCounter := FCounterClass.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DestroyCounter;
begin
    FreeAndNil(FCounter);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetCounterClass(const Value: TWSocketCounterClass);
var
    NewCounter : TWSocketCounter;
begin
    if Value = nil then
        raise ESocketException.Create('Property CounterClass may not be nil!');
    if Value <> FCounterClass then begin
        FCounterClass := Value;
        if Assigned(FCounter) then begin
            NewCounter              := FCounterClass.Create;
            NewCounter.ConnectDT    := FCounter.ConnectDT;
            NewCounter.ConnectTick  := FCounter.ConnectTick;
            NewCounter.LastRecvTick := FCounter.LastRecvTick;
            NewCounter.LastSendTick := FCounter.LastSendTick;
            FCounter.Free;
            FCounter := NewCounter;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetReqVerLow: BYTE;
begin
  {$IFDEF POSIX}
    Result := 0;
  {$ELSE}
    Result := GReqVerLow;
  {$ENDIF}

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetReqVerLow(const Value: BYTE);
begin
  {$IFDEF MSWINDOWS}
    if GReqVerLow <> Value then begin
        if IsSocketAPILoaded then
            SocketError('SetReqVerLow: WinSock version can''t be changed now')
        else
            GReqVerLow := Value;
    end;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetReqVerHigh: BYTE;
begin
  {$IFDEF POSIX}
    Result := 0;
  {$ELSE}
    Result := GReqVerHigh;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetReqVerHigh(const Value: BYTE);
begin
  {$IFNDEF POSIX}
    if GReqVerHigh <> Value then begin
        if IsSocketAPILoaded then
            SocketError('SetReqVerHigh: WinSock version can''t be changed now')
        else
            GReqVerHigh := Value;
    end;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Dup(NewHSocket : TSocket);
var
    iStatus : Integer;
    optlen  : Integer;
  {$IFDEF MACOS}
    optval  : Integer;
  {$ENDIF}
begin
    if (NewHSocket = 0) or (NewHSocket = INVALID_SOCKET) then begin
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('Dup');
        Exit;
    end;

    if FState <> wsClosed then begin
        iStatus := WSocket_Synchronized_closesocket(FHSocket);
        FHSocket := INVALID_SOCKET;
        if iStatus <> 0 then begin
            SocketError('Dup (closesocket)');
            Exit;
        end;

        ChangeState(wsClosed);
    end;
    FHsocket := NewHSocket;

  {$IFDEF MACOS}
    { No SIGPIPE on writes but EPIPE in errno }
    optlen  := SizeOf(Integer);
    optval  := 1;
    iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET, SO_NOSIGPIPE,
                                  PAnsiChar(@optval), optlen);
    if iStatus <> 0 then begin
        SocketError('setsockopt(SO_NOSIGPIPE)');
        Exit;
    end;
  {$ENDIF}

    { Get winsock send buffer size }
    optlen  := SizeOf(FSocketSndBufSize);
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF,
                                  PAnsiChar(@FSocketSndBufSize), optlen);

    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_SNDBUF)');
        Exit;
    end;

    { Get winsock receive buffer size }
    optlen  := SizeOf(FSocketRcvBufSize);
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF,
                                  PAnsiChar(@FSocketRcvBufSize), optlen);

    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_RCVBUF)');
        Exit;
    end;

    if HasOption(FComponentOptions, wsoTcpNoDelay) and { V7.27 }
                (not SetTcpNoDelayOption) then
        Exit;
    SetLingerOption;
    SetKeepAliveOption;  // AG { 05/23/07)

    { FD_CONNECT is not needed for dup(): The socket is already connected }
    FSelectEvent := FD_READ or FD_WRITE or FD_CLOSE { or FD_CONNECT };
    iStatus      := WSocket_Synchronized_WSAASyncSelect(
                                                      {$IFDEF POSIX}
                                                        Self,
                                                      {$ENDIF}
                                                        FHSocket,
                                                        Handle,
                                                        FMsg_WM_ASYNCSELECT,
                                                        FSelectEvent);
    if iStatus <> 0 then begin
        SocketError('WSAAsyncSelect');
        Exit;
    end;
    DupConnected;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DupConnected;
begin
    if Assigned(FCounter) then
        FCounter.SetConnected;
    FReadCount  := 0;  { 7.24 }
    FWriteCount := 0;  { 7.24 }
    ChangeState(wsConnected);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetBufSize(Value : Integer);
begin
    FBufHandler.BufSize := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetBufSize: Integer;
begin
    Result := FBufHandler.BufSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Get the number of char received and waiting to be read                    }
function TCustomWSocket.GetRcvdCount : LongInt;
var
    Temp : u_long;
begin
    if csDesigning in ComponentState then begin
        Result := -1;
        Exit;
    end;
    if WSocket_Synchronized_ioctlsocket(FHSocket, FIONREAD, Temp) = SOCKET_ERROR then begin
        Result := -1;
        SocketError('ioctlSocket');
        Exit;
    end;
    Result := LongInt(Temp);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ChangeState(NewState : TSocketState);
var
    OldState : TSocketState;
begin
    OldState := FState;
    FState   := NewState;
    if OldState <> NewState then       { 20030226 }
        TriggerChangeState(OldState, NewState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ DoRecv is a simple wrapper around winsock recv function to make it        }
{ a virtual function.                                                       }
function TCustomWSocket.DoRecv(
    var Buffer : TWSocketData;
    BufferSize : Integer;
    Flags      : Integer) : Integer;
begin
{ MoulinCnt := (MoulinCnt + 1) and 3; }
{ Write('R', Moulin[MoulinCnt], #13); }
    Result := WSocket_Synchronized_recv(FHSocket, Buffer, BufferSize, Flags);
  {$IFDEF POSIX}
    if (not FPaused) and ((Result > -1) or (errno = WSAEWOULDBLOCK)) then
        WSocketSynchronizedEnableReadEvent(Self);
  {$ENDIF}
{   FRcvdFlag := (Result > 0);}
    { If we received the requested size, we may need to receive more }
    FRcvdFlag := (Result >= BufferSize);
    if Assigned(FCounter) and (Result > 0) then
        FCounter.FLastRecvTick := IcsGetTickCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The socket is non-blocking, so this routine will only receive as much     }
{ data as it is available.                                                  }
function TCustomWSocket.Receive(Buffer : TWSocketData; BufferSize: Integer) : Integer;
begin
    Result := DoRecv(Buffer, BufferSize, 0);
    if Result < 0 then
        FLastError := WSocket_Synchronized_WSAGetLastError
    else
        { Here we should check for overflows ! It is well possible to }
        { receive more than 2GB during a single session.              }
        { Or we could use an Int64 variable...                        }
        FReadCount := FReadCount + Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function TCustomWSocket.ReceiveStrW(ACodePage : LongWord) : UnicodeString;
begin
    Result :=  AnsiToUniCode(ReceiveStrA, ACodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.ReceiveStrW : UnicodeString;
begin
    Result := ReceiveStrW(CP_ACP);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.ReceiveStrA : AnsiString;
var
    lCount : LongInt;
begin
    lCount := GetRcvdCount;

    if lCount < 0 then begin  { GetRcvdCount returned an error }
        SetLength(Result, 0);
        Exit;
    end;

    if lCount = 0 then        { GetRcvdCount say nothing, will try anyway }
        LCount := 255;        { some reasonable arbitrary value           }

    SetLength(Result, lCount);
    lCount := Receive(@Result[1], lCount);
    if lCount > 0 then
        SetLength(Result, lCount)
    else
        SetLength(Result, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Receive as much data as possible into a string                            }
{ You should avoid this function and use Receive. Using string will be      }
{ much slower because data will be copied several times.                    }
{ ReceiveStr will *NOT* wait for a line to be received. It just read        }
{ already received characters and return them as a string.                  }
function TCustomWSocket.ReceiveStr : String;
begin
{$IFDEF COMPILER12_UP}
    Result := ReceiveStrW;
{$ELSE}
    Result := ReceiveStrA;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.DoRecvFrom(
    FHSocket    : TSocket;
    var Buffer  : TWSocketData;
    BufferSize  : Integer;
    Flags       : Integer;
    var From    : TSockAddr;
    var FromLen : Integer) : Integer;
begin
    Result := WSocket_Synchronized_recvfrom(FHSocket, Buffer, BufferSize,
                                            Flags, From, FromLen);
  {$IFDEF POSIX}
    if (not FPaused) and ((Result > -1) or (errno = WSAEWOULDBLOCK)) then
        WSocketSynchronizedEnableReadEvent(Self);
  {$ENDIF}
{   FRcvdFlag := (Result > 0); }
    FRcvdFlag := (Result >= BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.ReceiveFrom(
    Buffer      : TWSocketData;
    BufferSize  : Integer;
    var From    : TSockAddr;
    var FromLen : Integer) : Integer;
begin
    Result := DoRecvFrom(FHSocket, Buffer, BufferSize, 0, From, FromLen);
    if Result < 0 then
        FLastError := WSocket_Synchronized_WSAGetLastError
    else
        FReadCount := FReadCount + Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.ReceiveFrom6(       { V8.07 }
    Buffer      : TWSocketData;
    BufferSize  : Integer;
    var From    : TSockAddrIn6;
    var FromLen : Integer) : Integer;
begin
    Result := DoRecvFrom(FHSocket, Buffer, BufferSize, 0, PSockAddrIn(@From)^, FromLen);
    if Result < 0 then
        FLastError := WSocket_Synchronized_WSAGetLastError
    else
        FReadCount := FReadCount + Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.PeekData(Buffer : TWSocketData; BufferSize: Integer) : Integer;
begin
    Result := DoRecv(Buffer, BufferSize, MSG_PEEK);
    if Result < 0 then
        FLastError := WSocket_Synchronized_WSAGetLastError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{
function SearchChar(Data : PChar; Len : Integer; Ch : Char) : PChar;
begin
    while Len > 0 do begin
        Len := Len - 1;
        if Data^ = Ch then begin
            Result := Data;
            exit;
        end;
        Data := Data + 1;
    end;
    Result := nil;
end;
}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This function should be used with UDP IPv4 only. Use Send for TCP.        }
function TCustomWSocket.SendTo(
    Dest       : TSockAddr;
    DestLen    : Integer;
    Data       : TWSocketData;
    Len        : Integer) : Integer;
begin
    Result := WSocket_Synchronized_SendTo(FHSocket, Data, Len, FSendFlags,
                                          TSockAddr(Dest), DestLen);
    if Result > 0 then begin
        FWriteCount := FWriteCount + Result;  { 7.24 }
        TriggerSendData(Result);
        { Post FD_WRITE message to have OnDataSent event triggered }
        if bAllSent and (FType = SOCK_DGRAM) then
            PostMessage(Handle,
                        FMsg_WM_ASYNCSELECT,
                        WParam(FHSocket),            { V8.08 }
                        IcsMakeLong(FD_WRITE, 0));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This function should be used with UDP IPv6 only. Use Send for TCP.        }
function TCustomWSocket.SendTo6(      { V8.07 }
    Dest       : TSockAddrIn6;
    DestLen    : Integer;
    Data       : TWSocketData;
    Len        : Integer) : Integer;
begin
    Result := WSocket_Synchronized_SendTo(FHSocket, Data, Len, FSendFlags,
                                                   PSockAddrIn(@Dest)^, DestLen);
    if Result > 0 then begin
        FWriteCount := FWriteCount + Result;  { 7.24 }
        TriggerSendData(Result);
        { Post FD_WRITE message to have OnDataSent event triggered }
        if bAllSent and (FType = SOCK_DGRAM) then
            PostMessage(Handle,
                        FMsg_WM_ASYNCSELECT,
                        WParam(FHSocket),            { V8.08 }
                        IcsMakeLong(FD_WRITE, 0));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.RealSend(var Data : TWSocketData; Len : Integer) : Integer;
begin
{ MoulinCnt := (MoulinCnt + 1) and 3; }
{ Write('S', Moulin[MoulinCnt], #13); }
    if FType = SOCK_DGRAM then
        Result := WSocket_Synchronized_SendTo(FHSocket, Data, Len, FSendFlags,
                                                 PSockAddr(@Fsin)^, SizeOfAddr(Fsin))
    else
        Result := WSocket_Synchronized_Send(FHSocket, Data, Len, FSendFlags);
    if Result > 0 then begin
        FWriteCount := FWriteCount + Result;  { 7.24 }
        if Assigned(FCounter) then
            FCounter.FLastSendTick := IcsGetTickCount;
        TriggerSendData(Result);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TryToSend;
var
    Len       : Integer;
    Count     : Integer;
    Data      : TWSocketData;
    LastError : Integer;
begin
    FBufHandler.Lock;
    try
        if (FHSocket = INVALID_SOCKET) or (FBufHandler.IsEmpty) then begin
            bAllSent := TRUE;
            Exit;
        end;

        while TRUE do begin
            Len := FBufHandler.Peek(Data);
            if Len <= 0 then begin
                // Buffer is empty, every thing has been sent
                bAllSent := TRUE;
                break;
            end;
            Count := RealSend(Data, Len);
            if Count > 0 then begin
                Dec(FBufferedByteCount, Count);
                if FBufferedByteCount < 0 then
                    FBufferedByteCount := 0;
            end;
            if Count = 0 then
                break;  // Closed by remote

            if Count = SOCKET_ERROR then begin
                LastError := WSocket_Synchronized_WSAGetLastError;
                if (LastError = WSAECONNRESET) or (LastError = WSAENOTSOCK) or
                   (LastError = WSAENOTCONN)   or (LastError = WSAEINVAL)   or
                   (LastError = WSAECONNABORTED)     { 07/05/99 }
                then begin
                  {$IFNDEF NO_DEBUG_LOG}
                    if CheckLogOptions(loWsockErr) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                            DebugLog(loWsockErr, Name +
                                     ' Winsock Send failed - ' +
                                     GetWinsockErr(LastError));
                  {$ENDIF}
                    FCloseInvoked := TRUE;           { 23/07/98 }
                    Close;
                    TriggerSessionClosed(LastError); { 23/07/98 }
                end
                else if LastError <> WSAEWOULDBLOCK then begin
                    SocketError('TryToSend failed');
                    break;
                end;
                break;
            end;
            FBufHandler.Remove(Count);
            if Count < Len then
                break; // Could not write as much as we wanted. Stop sending
        end;
    finally
        FBufHandler.UnLock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.PutStringInSendBuffer(const Str : RawByteString): Integer;
begin
    Result := Length(Str);
    if Result > 0 then
        PutDataInSendBuffer(Pointer(Str), Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
function TCustomWSocket.PutStringInSendBuffer(const Str : UnicodeString; ACodePage : LongWord): Integer;
begin
    Result := PutStringInSendBuffer(UnicodeToAnsi(Str, ACodePage));  // Explicit cast
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.PutStringInSendBuffer(const Str : UnicodeString): Integer;
begin
    Result := PutStringInSendBuffer(AnsiString(Str));  // Explicit cast
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.PutDataInSendBuffer(
    Data : TWSocketData;
    Len  : Integer);
begin
    if (Len <= 0) or (Data = nil) then
        Exit;

    FBufHandler.Lock;
    try
        FBufHandler.Write(Data, Len);
        Inc(FBufferedByteCount, Len);
        bAllSent := FALSE;
    finally
        FBufHandler.UnLock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return -1 if error, else return number of byte written                    }
function TCustomWSocket.Send(Data : TWSocketData; Len : Integer) : Integer;
begin
    if (FState <> wsConnected) and (FState <> wsSocksConnected) then begin
        WSocket_Synchronized_WSASetLastError(WSAENOTCONN);
        SocketError('Send');
        Result := -1;
        Exit;
    end;

    bAllSent := FALSE;
    if Len <= 0 then
        Result := 0
    else begin
        Result   := Len;
        PutDataInSendBuffer(Data, Len);
    end;

    if bAllSent then
        Exit;

    TryToSend;

    if bAllSent then begin
        { We post a message to fire the FD_WRITE message which in turn will}
        { fire the OnDataSent event. We cannot fire the event ourself      }
        { because the event handler will eventually call send again.       }
        { Sending the message prevent recursive call and stack overflow.   }
        { The PostMessage function posts (places) a message in a window's  }
        { message queue and then returns without waiting for the           }
        { corresponding window to process the message. The message will be }
        { seen and routed by Delphi a litle later, when we will be out of  }
        { the send function.                                               }
        PostMessage(Handle,
                    FMsg_WM_ASYNCSELECT,
                    WParam(FHSocket),            { V8.08 }
                    IcsMakeLong(FD_WRITE, 0));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.Send(DataByte : Byte) : Integer;
begin
    Result := Send(@DataByte, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return -1 if error, else return number of bytes written }
{$IFDEF COMPILER12_UP}
function TCustomWSocket.SendStr(const Str : UnicodeString; ACodePage : LongWord) : Integer;
begin
    Result := SendStr(UnicodeToAnsi(Str, ACodePage));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Converts UnicodeString to AnsiString using System.DefaultSystemCodePage   }
function TCustomWSocket.SendStr(const Str : UnicodeString) : Integer;
begin
    Result := SendStr(AnsiString(Str)); // RTL convert
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return -1 if error, else return number of byte written                    }
function TCustomWSocket.SendStr(const Str : RawByteString) : Integer;
begin
    Result := Length(Str);
    if Result > 0 then
        Result := Send(PAnsiChar(Str), Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SendText(const Str : RawByteString);
begin
    SendStr(Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
procedure TCustomWSocket.SendText(const Str : UnicodeString);
begin
    SendStr(AnsiString(Str));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SendText(const Str : UnicodeString; ACodePage : LongWord);
begin
    SendStr(Str, ACodePage);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function HasOption(
    OptSet : TWSocketOptions;
    Opt    : TWSocketOption): Boolean;
begin
    Result := Opt in OptSet;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AddOptions(Opts: array of TWSocketOption): TWSocketOptions;
var
    I : Integer;
begin
    Result := [];
    for I := Low(Opts) to High(Opts) do
        //Result := Result + [Opts[I]];  { Anton Sviridov }
        Include(Result, Opts[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  RemoveOption(
    OptSet : TWSocketOptions;
    Opt    : TWSocketOption) : TWSocketOptions;
begin
    Result := OptSet - [Opt];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ASyncReceive(
    Error           : Word;
    MySocketOptions : TWSocketOptions);
var
    bMore        : Boolean;
    lCount       : {$IFDEF FPC} LongWord; {$ELSE} u_long; {$ENDIF}
    TrashCanBuf  : array [0..1023] of AnsiChar;  { AG 1/12/08 }
    TrashCan     : TWSocketData;
    TrashCanSize : Integer;
begin
    bMore := TRUE;
    while bMore do begin
        FLastError := 0;

        try
            if not TriggerDataAvailable(Error) then begin
                { Nothing wants to receive, we will receive and throw away  23/07/98 }
                TrashCanSize := SizeOf(TrashCanBuf); { V7.75 }
                TrashCan     := @TrashCanBuf;
                if DoRecv(TrashCan, TrashCanSize, 0) = SOCKET_ERROR then begin
                    FLastError := WSocket_Synchronized_WSAGetLastError;
                    if FLastError = WSAEWOULDBLOCK then begin
                        FLastError := 0;
                        break;
                    end;
                end;
            end;

            { DLR Honor the socket options being passed as parameters }
            if HasOption({FComponentOptions}MySocketOptions, wsoNoReceiveLoop) then  { V6.03 }
                break;

            if FLastError <> 0 then begin
                bMore := FALSE;
                { -1 value is not a true error but is used to break the loop }
                if FLastError = -1 then
                    FLastError := 0;
            end
            { Check if we have something new arrived, if yes, process it }
            else if WSocket_Synchronized_ioctlsocket(FHSocket, FIONREAD,
                                                     lCount) = SOCKET_ERROR then begin
                FLastError := WSocket_Synchronized_WSAGetLastError;
                bMore      := FALSE;
            end
            else if lCount = 0 then
                bMore := FALSE;
        except
            bMore := FALSE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_CONNECT(var msg: TMessage);
begin
    if FState <> wsConnected then begin
        ChangeState(wsConnected);
        TriggerSessionConnectedSpecial(IcsHiWord(msg.LParam));
        if (IcsHiWord(msg.LParam) <> 0) and (FState <> wsClosed) then
            Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_READ(var msg: TMessage);
begin
    if FState <> wsConnected then begin
      ChangeState(wsConnected);
      TriggerSessionConnectedSpecial(IcsHiWord(msg.LParam));
    end;
    ASyncReceive(IcsHiWord(msg.LParam), FComponentOptions);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_WRITE(var msg: TMessage);
begin
    TryToSend;
{ If you wants to test background exception, uncomment the next 2 lines. }
{   if bAllSent then                                                }
{       raise Exception.Create('Test TWSocket exception');          }
    if bAllSent then
        TriggerDataSent(IcsHiWord(msg.LParam));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_CLOSE(var msg: TMessage);
begin
    { In some strange situations I found that we receive a FD_CLOSE  }
    { during the connection phase, breaking the connection early !   }
    { This occurs for example after a failed FTP transfert Probably  }
    { something related to bugged winsock. Doesn't hurt with good    }
    { winsock. So let the code there !                               }
    if (FState <> wsConnecting) and (FHSocket <> INVALID_SOCKET) then begin
        { Check if we have something arrived, if yes, process it     }
        { DLR, since we are closing MAKE SURE WE LOOP in the receive }
        { function to get ALL remaining data                         }

        if (FState <> wsListening) then // We should not receive with listening sockets {AG}
            ASyncReceive(0, RemoveOption(FComponentOptions, wsoNoReceiveLoop));

        if not FCloseInvoked then begin
            FCloseInvoked := TRUE;
            TriggerSessionClosed(IcsHiWord(msg.LParam));
        end;

        if FState <> wsClosed then
            Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_ACCEPT(var msg: TMessage);
begin
    TriggerSessionAvailable(IcsHiWord(msg.LParam));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_ROUTING_INTERFACE_CHANGE(var msg: TMessage);
begin
    TriggerRoutingInterfaceChanged(IcsHiWord(msg.LParam));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_ADDRESS_LIST_CHANGE(var msg: TMessage);
begin
    TriggerAddressListChanged(IcsHiWord(msg.LParam));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerAddressListChanged(ErrCode: Word);
begin
    if Assigned(FOnAddressListChanged) then
        FOnAddressListChanged(Self, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerRoutingInterfaceChanged(ErrCode: Word);
begin
    if Assigned(FOnRoutingInterfaceChanged) then
        FOnRoutingInterfaceChanged(Self, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetOnAddressListChanged(
    const Value: TNetChangeEvent);
begin
    FOnAddressListChanged := Value;
    if Assigned(FOnAddressListChanged) then
        Include(FComponentOptions, wsoNotifyAddressListChange)
    else
        Exclude(FComponentOptions, wsoNotifyAddressListChange);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetOnRoutingInterfaceChanged(
    const Value: TNetChangeEvent);
begin
    FOnRoutingInterfaceChanged := Value;
    if Assigned(FOnRoutingInterfaceChanged) then
        Include(FComponentOptions, wsoNotifyRoutingInterfaceChange)
    else
        Exclude(FComponentOptions, wsoNotifyRoutingInterfaceChange);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_DEBUG_LOG}
function WinsockMsgToString(var msg: TMessage) : String;
begin
    Result := '';
    if (msg.lParam and FD_CONNECT) <> 0 then
        Result := Result + ' FD_CONNECT';
    if (msg.lParam and FD_READ) <> 0 then
        Result := Result + ' FD_READ';
    if (msg.lParam and FD_WRITE) <> 0 then
        Result := Result + ' FD_WRITE';
    if (msg.lParam and FD_CLOSE) <> 0 then
        Result := Result + ' FD_CLOSE';
    if (msg.lParam and FD_ACCEPT) <> 0 then
        Result := Result + ' FD_ACCEPT';
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMASyncSelect(var msg: TMessage);
var
    Check   : Word;
    ParamLo : Word;
const
    TTTCount : Integer = 0;
begin
{TriggerDisplay('AsyncSelect ' + IntToStr(msg.wParam) + ', ' + IntToStr(msg.LParamLo));}
    { Verify that the socket handle is ours handle }
    if msg.wParam <> WPARAM(FHSocket) then
        Exit;

    if FPaused then
        exit;

    ParamLo := LoWord(msg.lParam);

    Check := ParamLo and FD_CONNECT;
    if Check <> 0 then begin
        FSelectMessage := FD_CONNECT;
        Do_FD_CONNECT(msg);
    end;

    Check := ParamLo and FD_READ;
    if Check <> 0 then begin
        FSelectMessage := FD_READ;
        Do_FD_READ(msg);
    end;

    Check := ParamLo and FD_WRITE;
    if Check <> 0 then begin
        FSelectMessage := FD_WRITE;
        Do_FD_WRITE(msg);
    end;

    Check := ParamLo and FD_ACCEPT;
    if Check <> 0 then begin
        FSelectMessage := FD_ACCEPT;
        Do_FD_ACCEPT(msg);
    end;

    Check := ParamLo and FD_CLOSE;
    if Check <> 0 then begin
        FSelectMessage := FD_CLOSE;
        {WriteLn('FD_CLOSE ', FHSocket);}
        Do_FD_CLOSE(msg);
    end;

 {$IFDEF MSWINDOWS}
    if ParamLo and FD_ROUTING_INTERFACE_CHANGE <> 0 then begin
        FSelectMessage := FD_ROUTING_INTERFACE_CHANGE;
        Do_FD_ROUTING_INTERFACE_CHANGE(msg);
    end;

    if ParamLo and FD_ADDRESS_LIST_CHANGE <> 0 then begin
        FSelectMessage := FD_ADDRESS_LIST_CHANGE;
        Do_FD_ADDRESS_LIST_CHANGE(msg);
    end;
 {$ENDIF}

    FSelectMessage := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure GetIPList(phe  : PHostEnt; ToList : TStrings);
type
    TaPInAddr = array [0..255] of PInAddr;
    PaPInAddr = ^TaPInAddr;
var
    pptr : PaPInAddr;
    I    : Integer;
begin
    pptr := PaPInAddr(Phe^.h_addr_list);

    I := 0;
    while pptr^[I] <> nil do begin
        ToList.Add(String(WSocket_inet_ntoa(pptr^[I]^)));
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure GetAliasList(phe : PHostEnt; ToList : TStrings);
type
    TaPAnsiChar = array [0..255] of PAnsiChar;
    PaPAnsiChar = ^TaPAnsiChar;
var
    pptr : PaPAnsiChar;
    I    : Integer;
begin
    pptr := PaPAnsiChar(Phe^.h_aliases);
    I    := 0;
    while pptr^[I] <> nil do begin
        ToList.Add(String(pptr^[I]));
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMAsyncGetHostByName(var msg: TMessage);
var
    ErrCode : Word;
{$IFDEF MSWINDOWS}
    Phe     : Phostent;
{$ENDIF}
begin
    if FDnsLookupHandle = 0 then begin
        { We are still executing WSAAsyncGetHostByName and FDnsLookupHandle }
        { has not been assigned yet ! We should proceed later.              }
        FDnsLookupTempMsg  := msg;
        FDnsLookupCheckMsg := TRUE;
        Exit;
    end
    else if msg.wParam <> WPARAM(FDnsLookupHandle) then
        Exit;

    FDnsLookupHandle := 0;
    ErrCode := IcsHiWord(Msg.LParam);
    if ErrCode = 0 then begin
      {$IFDEF MSWINDOWS}
        if FSocketFamily = sfIPv4 then begin
            Phe := PHostent(@FDnsLookupBuffer);
            if phe <> nil then begin
                GetIpList(Phe, FDnsResultList);
                FDnsResult := FDnsResultList.Strings[0];
            end;
        end
        else begin
      {$ENDIF}
            FDnsResultList.Assign(TIcsAsyncDnsLookupRequest(msg.WParam).ResultList);
            if FDnsResultList.Count > 0 then
                FDnsResult := FDnsResultList[0];
      {$IFDEF MSWINDOWS}
        end;
      {$ENDIF}
    end;
    TriggerDnsLookupDone(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMAsyncGetHostByAddr(var msg: TMessage);
var
{$IFDEF MSWINDOWS}
    Phe     : Phostent;
{$ENDIF}
    ErrCode : Word;
begin
    if msg.wParam <> WPARAM(FDnsLookupHandle) then
        Exit;
    FDnsLookupHandle := 0;
    ErrCode          := IcsHiWord(Msg.LParam);
    if ErrCode = 0 then begin
        FDnsResultList.Clear;
      {$IFDEF MSWINDOWS}
        if FSocketFamily = sfIPv4 then begin
            Phe := PHostent(@FDnsLookupBuffer);
            if phe <> nil then begin
                //SetLength(FDnsResult, StrLen(Phe^.h_name));
                //StrCopy(PAnsiChar(FDnsResult), Phe^.h_name);
                FDnsResult := String(StrPas(Phe^.h_name));
                FDnsResultList.Add(FDnsResult);
                GetAliasList(Phe, FDnsResultList);  {AG 03/03/06}
            end;
        end
        else begin
            FDnsResultList.Assign(TIcsAsyncDnsLookupRequest(msg.WParam).ResultList);
        end;
      {$ENDIF}
      {$IFDEF POSIX}
        FDnsResultList.Assign(TIcsAsyncDnsLookupRequest(msg.WParam).ResultList);
      {$ENDIF}
        if FDnsResultList.Count > 0 then
            FDnsResult := FDnsResultList[0];
    end;
    TriggerDnsLookupDone(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetProto(sProto : String);
begin
    if FProtoAssigned and (sProto = FProtoStr) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Proto if not closed');
        Exit;
    end;

    FProtoStr := IcsTrim(sProto);

    if Length(FProtoStr) = 0 then begin
        FProtoAssigned := FALSE;
        Exit;
    end;

    FProtoResolved := FALSE;
    FProtoAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetRemotePort(sPort : String);
begin
    if FPortAssigned and (FPortStr = sPort) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Port if not closed');
        Exit;
    end;

    FPortStr := IcsTrim(sPort);

    if Length(FPortStr) = 0 then begin
        FPortAssigned := FALSE;
        Exit;
    end;

    FPortResolved := FALSE;
    FPortAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetRemotePort : String;
begin
    Result := FPortStr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLocalPort(const sLocalPort : String);
begin
    if FState <> wsClosed then begin
        RaiseException('Cannot change LocalPort if not closed');
        Exit;
    end;

    FLocalPortStr      := sLocalPort;
    FLocalPortResolved := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLocalAddr(const sLocalAddr : String);
begin
    if FState <> wsClosed then begin
        RaiseException('Cannot change LocalAddr if not closed');
        Exit;
    end;
    FLocalAddr := IcsTrim(sLocalAddr);
    if FLocalAddr = '' then
      FLocalAddr := ICS_ANY_HOST_V4;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLocalAddr6(const sLocalAddr6: String);
begin
    if FState <> wsClosed then begin
        RaiseException('Cannot change LocalAddr6 if not closed');
        Exit;
    end;
    FLocalAddr6 := IcsTrim(sLocalAddr6);
    if FLocalAddr6 = '' then
      FLocalAddr6 := ICS_ANY_HOST_V6;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetMultiCastAddrStr(const sMultiCastAddrStr: String);
begin
    FMultiCastAddrStr := IcsTrim(sMultiCastAddrStr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetXPort: String;
var
    saddr    : TSockAddrIn6;
    saddrlen : Integer;
    port     : Integer;
begin
    Result := 'error';
    if FState in [wsConnected, wsBound, wsListening] then begin
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetSockName(FHSocket, PSockAddr(@saddr)^,
                                            saddrlen) = 0 then begin
            port     := WSocket_Synchronized_ntohs(saddr.sin6_port);
            Result   := IntToStr(port);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetXAddr: String;
var
    saddr    : TSockAddrIn6;
    saddrlen : Integer;
begin
    Result := 'error';
    if FState in [wsConnected, wsBound, wsListening] then begin
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetSockName(FHSocket, PSockAddr(@saddr)^,
                                            saddrlen) = 0 then
        begin
            if saddr.sin6_family = AF_INET then
                Result := WSocketIPv4ToStr(PSockAddrIn(@saddr)^.sin_addr.S_addr)
            else
                Result := WSocketIPv6ToStr(@saddr);
            //Result := String(WSocket_Synchronized_inet_ntoa(saddr.sin_addr));
        end;
     end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetAddr(const InAddr : String);
var
    LSocketFamily: TSocketFamily;
begin
    if FAddrAssigned and (FAddrStr = InAddr) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Addr if not closed');
        Exit;
    end;

    FAddrStr := IcsTrim(InAddr);

    if Length(FAddrStr) = 0 then begin
        FAddrAssigned := FALSE;
        Exit;
    end;

    { If the address is either a valid IPv4 or IPv6 address }
    { change current SocketFamily.                          }
    if WSocketIsIP(FAddrStr, LSocketFamily) then
    begin
        if (LSocketFamily = sfIPv4) or (IsIPv6APIAvailable) then
            FSocketFamily := LSocketFamily
        else
            FSocketFamily := FOldSocketFamily;
    end
    else
        FSocketFamily := FOldSocketFamily;

    FAddrResolved       := FALSE;
    FAddrAssigned       := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.RegisterIcsAsyncDnsLookup;
var
    PP: PPointer;
begin
    if FAsyncLookupPtr = nil then begin
        GThreadLocalStore.Lock;
        try
            FLookupThreadID := IcsGetCurrentThreadID;
            PP := GThreadLocalStore.RegisterStore(FLookupThreadID);
            if (PP^ = nil) then
                PP^ := TIcsAsyncDnsLookup.Create(CpuCount);
            FAsyncLookupPtr := PP^;
        finally
            GThreadLocalStore.Unlock;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.UnRegisterIcsAsyncDnsLookup;
begin
    if FAsyncLookupPtr <> nil then begin
        GThreadLocalStore.Lock;
        try
            if GThreadLocalStore.UnregisterStore(FLookupThreadID) = FAsyncLookupPtr then
                FreeAndNil(TObject(FAsyncLookupPtr));
        finally
            GThreadLocalStore.Unlock;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetMinMaxIcsAsyncDnsLookupThreads(
  AMinThreads : Byte; AMaxThreads : Byte);
begin
    if FAsyncLookupPtr = nil then
        RegisterIcsAsyncDnsLookup;
    TIcsAsyncDnsLookup(FAsyncLookupPtr).SetMinMaxThreads(AMinThreads, AMaxThreads);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.IcsAsyncGetHostByName(AWnd: HWND; AMsgID: UINT;
  const ASocketFamily: TSocketFamily; const AName: String;
  const AProtocol: Integer): THandle;
begin
    if FAsyncLookupPtr = nil then
        RegisterIcsAsyncDnsLookup;
    Result := TIcsAsyncDnsLookup(FAsyncLookupPtr).ExecAsync(
                          AWnd, AMsgID, ASocketFamily, AName, FALSE, AProtocol);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.IcsAsyncGetHostByAddr(AWnd: HWND; AMsgID: UINT;
  const ASocketFamily: TSocketFamily; const AAddr: String;
  const AProtocol: Integer): THandle;
begin
    if FAsyncLookupPtr = nil then
        RegisterIcsAsyncDnsLookup;
    Result := TIcsAsyncDnsLookup(FAsyncLookupPtr).ExecAsync(
                           AWnd, AMsgID, ASocketFamily, AAddr, TRUE, AProtocol);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.IcsCancelAsyncRequest(
  const ARequest: THandle): Integer;
begin
    if FAsyncLookupPtr <> nil then
        Result := TIcsAsyncDnsLookup(FAsyncLookupPtr).CancelAsyncRequest(ARequest)
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_Synchronized_ResolveHost(
    const AHostName  : string;
    var ASockAddrIn6 : TSockAddrIn6;
    const AFamily    : TSocketFamily;
    const AProtocol: Integer); overload;
var
    Hints     : TAddrInfo;
    AddrInfo  : PAddrInfo;
    NextInfo  : PAddrInfo;
    RetVal    : Integer;
    IPv4Addr  : LongWord;
    Success   : Boolean;
    PInfo     : PAddrInfo;
begin
    if AHostName = '' then
        raise ESocketException.Create('WSocket Resolve Host: Invalid Hostname');
    if (AFamily <> sfIPv6) and WSocketIsIPv4(AHostName) then
    begin
        { Address is a dotted numeric IPv4 address like 192.161.124.32 }
        ASockAddrIn6.sin6_family := AF_INET;
        IPv4Addr := WSocketStrToIPv4(AHostName, Success);
        if IPv4Addr = LongWord(INADDR_NONE) then
        begin
            if AHostName = ICS_BROADCAST_V4 then
            begin
                PSockAddrIn(@ASockAddrIn6)^.sin_addr.S_addr := Integer(INADDR_BROADCAST);
                Exit;
            end;
            raise ESocketException.Create('Winsock Resolve Host: ''' + AHostName +
                                         ''' Invalid IP address.');
        end;
        PSockAddrIn(@ASockAddrIn6)^.sin_addr.S_addr := IPv4Addr;
        Exit;
    end
    else begin
        (*  Call GetAddrInfo() below in order to get the scope_id
        if AFamily = sfIPv4 then
            Success := FALSE
        else
            ASockAddrIn6.sin6_addr := TInAddr6(WSocketStrToIPv6(AHostName, Success));
        if Success then
        begin
            { Address is valid IPv6 IP address }
            ASockAddrIn6.sin6_family := AF_INET6;
            Exit;
        end
        else begin
        *)
            FillChar(Hints, SizeOf(Hints), 0);
            if AFamily = sfIPv4 then
                Hints.ai_family := AF_INET
            else if AFamily = sfIPv6 then
                Hints.ai_family := AF_INET6;
            {else
                Hints.ai_family := AF_UNSPEC;} // = 0 anyway

            AddrInfo := nil;
            Hints.ai_protocol := AProtocol;
          {$IFDEF POSIX}
            RetVal   := WSocket_Synchronized_GetAddrInfo(PAnsiChar(UnicodeToAnsi(AHostName, CP_UTF8)),
                                                         nil, @Hints, AddrInfo);
          {$ELSE}
            RetVal   := WSocket_Synchronized_GetAddrInfo(PChar(AHostName),
                                                         nil, @Hints, AddrInfo);
          {$ENDIF}
            if RetVal <> 0 then
                raise ESocketException.Create(
                 'Winsock Resolve Host: Cannot convert host address ''' +
                 AHostName + ''' - ' + GetWinsockErr(RetVal));
            try
                PInfo:= nil;
                NextInfo := AddrInfo;
                while NextInfo <> nil do
                begin
                    if NextInfo.ai_family = AF_INET then
                    begin
                        if (AFamily = sfIPv4) or (AFamily = sfAnyIPv4) or
                           (AFamily = sfAny) then
                        begin
                            ASockAddrIn6.sin6_family := NextInfo.ai_family;
                            PSockAddrIn(@ASockAddrIn6)^.sin_addr := PSockAddrIn(NextInfo.ai_addr).sin_addr;
                            Exit;
                        end;
                        if PInfo = nil then
                            PInfo := NextInfo;
                    end
                    else if NextInfo.ai_family = AF_INET6 then
                    begin
                        if (AFamily = sfIPv6) or (AFamily = sfAnyIPv6) or
                           (AFamily = sfAny) then
                        begin
                            ASockAddrIn6.sin6_family := NextInfo.ai_family;
                            ASockAddrIn6.sin6_addr := PSockAddrIn6(NextInfo.ai_addr)^.sin6_addr;
                            ASockAddrIn6.sin6_scope_id := PSockAddrIn6(NextInfo.ai_addr)^.sin6_scope_id;
                            Exit;
                        end;
                        if PInfo = nil then
                            PInfo := NextInfo;
                    end;

                    NextInfo := NextInfo.ai_next;
                end;

                if ((AFamily = sfAnyIPv6) or (AFamily = sfAnyIPv4)) and
                   (PInfo <> nil) then
                begin
                    ASockAddrIn6.sin6_family := PInfo.ai_family;
                    if PInfo.ai_family = AF_INET6 then
                    begin
                        ASockAddrIn6.sin6_addr := PSockAddrIn6(PInfo.ai_addr)^.sin6_addr;
                        ASockAddrIn6.sin6_scope_id := PSockAddrIn6(PInfo.ai_addr)^.sin6_scope_id;
                    end
                    else
                        PSockAddrIn(@ASockAddrIn6)^.sin_addr := PSockAddrIn(PInfo.ai_addr).sin_addr;
                end
                else
                    raise ESocketException.Create(
                           'Winsock Resolve Host: Cannot convert host address ''' +
                            AHostName + '''');
            finally
                WSocket_Synchronized_FreeAddrInfo(AddrInfo);
            end;
        //end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsIPv6AddrFromAddrInfo(AddrInfo: PAddrInfo): TIcsIPv6Address;
  {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := PIcsIPv6Address(@PSockAddrIn6(AddrInfo^.ai_addr)^.sin6_addr)^;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ResolveHost(InAddr : AnsiString) : TInAddr; overload;
var
    Phe     : Phostent;
    IPAddr  : u_long;
begin
    if InAddr = '' then
      {  raise ESocketException.Create('WSocketResolveHost: ''' + InAddr + ''' Invalid Hostname.'); }
        raise ESocketException.Create('Winsock Resolve Host: ''' + String(InAddr) + ''' Invalid Hostname.');   { V5.26 }

    if WSocketIsDottedIP(InAddr) then begin
        { Address is a dotted numeric address like 192.161.124.32 }
        IPAddr := WSocket_Synchronized_inet_addr(PAnsiChar(InAddr));
        if IPAddr = u_long(INADDR_NONE) then begin
            if InAddr = ICS_BROADCAST_V4 then begin
                Result.s_addr := u_long(INADDR_BROADCAST);
                Exit;
            end;
            raise ESocketException.Create('Winsock Resolve Host: ''' + String(InAddr) +
                                         ''' Invalid IP address.');   { V5.26 }
        end;
        Result.s_addr := IPAddr;
        Exit;
    end;
    { Address is a hostname }
    Phe := WSocket_Synchronized_GetHostByName(PAnsiChar(InAddr));
    if Phe = nil then
        raise ESocketException.Create(
                 'Winsock Resolve Host: Cannot convert host address ''' +
                 String(InAddr) + ''' - ' +
                 GetWinsockErr(WSocket_Synchronized_WSAGetLastError));
    Result.s_addr := PInAddr(Phe^.h_addr_list^)^.s_addr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveHost(InAddr : AnsiString) : TInAddr;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ResolveHost(InAddr);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketResolveHost(
    const AHostName     : string;
    var AAddr           : TSockAddrIn6;
    const ASocketFamily : TSocketFamily;
    const AProtocol: Integer);
begin
    {$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        WSocket_Synchronized_ResolveHost(AHostName, AAddr, ASocketFamily, AProtocol);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert port name or number to number in host order (ftp -> 21)           }
function WSocket_Synchronized_ResolvePort(Port : AnsiString; Proto : AnsiString) : WORD;
var
    Pse      : Pservent;
begin
    if Port = '' then
      { raise ESocketException.Create('WSocketResolvePort: Invalid Port.'); }
        raise ESocketException.Create('Winsock Resolve Port: Invalid Port.');

    if Proto = '' then
        raise ESocketException.Create('Winsock Resolve Port: Invalid Proto.');

    if IsDigit(Port[1]) then
        Result := atoi(Port)
    else begin
        Pse := WSocket_Synchronized_GetServByName(PAnsiChar(Port), PAnsiChar(Proto));
        if Pse = nil then
            raise ESocketException.Create(
                      'Winsock Resolve Port: Cannot convert port ''' +
                      String(Port) + ''' - ' +
                      GetWinsockErr(WSocket_Synchronized_WSAGetLastError)); { V5.26 }
        Result := WSocket_Synchronized_ntohs(Pse^.s_port);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert port name or number to number in host order (ftp -> 21)           }
function WSocketResolvePort(Port : AnsiString; Proto : AnsiString) : Word;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ResolvePort(Port, Proto);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ResolveProto(sProto : AnsiString) : Integer; overload;
var
    Ppe     : Pprotoent;
begin
    if sProto = '' then
      {  raise ESocketException.Create('WSocketResolveProto: Invalid Protocol.');  }
        raise ESocketException.Create('Winsock Resolve Proto: Invalid Protocol.');  { V5.26 }

    if IsDigit(sProto[1]) then
        Result := atoi(sProto)
    else begin
        sProto := IcsLowerCase(IcsTrim(sProto));
        if sProto = 'tcp' then
            Result := IPPROTO_TCP
        else if sProto = 'udp' then
            Result := IPPROTO_UDP
        else if sProto = 'raw' then
            Result := IPPROTO_RAW
        else begin
            ppe := WSocket_Synchronized_getprotobyname(PAnsiChar(sProto));
            if Ppe = nil then
                raise ESocketException.Create(
                          'Winsock Resolve Proto: Cannot convert protocol ''' +
                          String(sProto) + ''' - ' +
                          GetWinsockErr(WSocket_Synchronized_WSAGetLastError));    { V5.26 }
            Result := ppe^.p_proto;
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveProto(sProto : AnsiString) : Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ResolveProto(sProto);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetSockName(var saddr : TSockAddrIn; var saddrlen : Integer) : Integer;
begin
    Result := WSocket_Synchronized_GetSockName(FHSocket, saddr, saddrlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetPeerAddr: String;
var
    saddr    : TSockAddrIn6;
    saddrlen : Integer;
begin
    Result := 'error';
    if FState = wsConnected then begin
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetPeerName(FHSocket,
              PSockAddrIn(@saddr)^, saddrlen) = 0 then
        begin
            if saddr.sin6_family = AF_INET then
                Result := WSocketIPv4ToStr(PSockAddrIn(@saddr)^.sin_addr.S_addr)
            else
                Result := WSocketIPv6ToStr(@saddr);
            //Result := String(WSocket_Synchronized_inet_ntoa(saddr.sin6_addr))
        end
        else begin
            SocketError('GetPeerName');
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetPeerPort: String;
var
    saddr    : TSockAddrIn6;
    saddrlen : Integer;
begin
    Result := 'error';
    if FState = wsConnected then begin
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetPeerName(FHSocket, PSockAddrIn(@saddr)^,
                                            saddrlen) = 0 then
            Result := IntToStr(WSocket_Synchronized_ntohs(saddr.sin6_port))
        else begin
            SocketError('GetPeerPort');
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetPeerName(var Name : TSockAddrIn; NameLen : Integer) : Integer;
begin
    if FState = wsConnected then
        Result := WSocket_Synchronized_GetPeerName(FHSocket, Name, NameLen)
    else
        Result := SOCKET_ERROR;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.InternalCancelDnsLookup(IgnoreErrors: Boolean);
var
    RetVal: Integer;
begin
    if FDnsLookupHandle = 0 then
        Exit;
  {$IFDEF MSWINDOWS}
    if FSocketFamily = sfIPv4 then
        RetVal := WSocket_Synchronized_WSACancelAsyncRequest(FDnsLookupHandle)
    else
  {$ENDIF}
        RetVal := IcsCancelAsyncRequest(FDnsLookupHandle);
    if (RetVal <> 0) and (not IgnoreErrors) then begin
        FDnsLookupHandle := 0;
        SocketError('WSACancelAsyncRequest');
        Exit;
    end;
    FDnsLookupHandle := 0;
    if not (csDestroying in ComponentState) then
        TriggerDnsLookupDone(WSAEINTR);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.CancelDnsLookup;
begin
    InternalCancelDnsLookup(FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DnsLookup(const AHostName : String);
begin
    DnsLookup(AHostName, IPPROTO_TCP);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DnsLookup(const AHostName : String;
  const AProtocol: Integer);
var
    IPAddr   : TInAddr;
    IPv6Addr : TIcsIPv6Address;
    HostName : AnsiString;
    Success  : Boolean;
    ScopeID  : LongWord;
begin
    if AHostName = '' then begin
        RaiseException('DNS lookup: invalid host name.');
        TriggerDnsLookupDone(WSAEINVAL);
        Exit;
    end;

    { Cancel any pending lookup }
    if FDnsLookupHandle <> 0 then begin
    {$IFDEF MSWINDOWS}
        if FSocketFamily = sfIPv4 then
            WSocket_Synchronized_WSACancelAsyncRequest(FDnsLookupHandle)
        else
    {$ENDIF}
            IcsCancelAsyncRequest(FDnsLookupHandle);
        FDnsLookupHandle := 0;
    end;

    FDnsResult := '';
    FDnsResultList.Clear;

    HostName := AnsiString(AHostName);

    if (FSocketFamily <> sfIPv6) and
       WSocketIsDottedIP(Hostname) then begin   { 28/09/2002 }
        IPAddr.S_addr := WSocket_Synchronized_inet_addr(PAnsiChar(HostName));
        if IPAddr.S_addr <> u_long(INADDR_NONE) then begin
            FDnsResult := String(WSocket_Synchronized_inet_ntoa(IPAddr));
            FDnsResultList.Add(FDnsResult);     { 28/09/2002 }{ 12/02/2003 }
            TriggerDnsLookupDone(0);
            Exit;
        end;
    end;

    if (FSocketFamily <> sfIPv4) then
    begin
        IPv6Addr := WSocketStrToIPv6(IcsTrim(AHostName), Success, ScopeID);
        if Success and (ScopeID = 0) then
        begin
            FDnsResult := WSocketIPv6ToStr(IPv6Addr);
            FDnsResultList.Add(FDnsResult);
            TriggerDnsLookupDone(0);
            Exit;
        end;
    end;

    if FWindowHandle = 0 then
        RaiseException('DnsLookup: Window not assigned');

    { John Goodwin found a case where winsock dispatch WM_ASYNCGETHOSTBYNAME }
    { message before returning from WSAAsyncGetHostByName call. Because of   }
    { that, FDnsLookupHandle is not yet assigned when execution comes in     }
    { WMAsyncGetHostByName. John use a flag to check this situation.         }
    FDnsLookupCheckMsg := FALSE;
  {$IFDEF MSWINDOWS}
    if FSocketFamily = sfIPv4 then
        FDnsLookupHandle   := WSocket_Synchronized_WSAAsyncGetHostByName(
                                  FWindowHandle,
                                  FMsg_WM_ASYNCGETHOSTBYNAME,
                                  @HostName[1],
                                  @FDnsLookupBuffer,
                                  SizeOf(FDnsLookupBuffer))
    else
  {$ENDIF}
        FDnsLookupHandle   := IcsAsyncGetHostByName(
                                  FWindowHandle,
                                  FMsg_WM_ASYNCGETHOSTBYNAME,
                                  FSocketFamily,
                                  IcsTrim(AHostName),
                                  AProtocol);

    if FDnsLookupHandle = 0 then begin
        RaiseException(String(HostName) + ': can''t start DNS lookup - ' +
                       GetWinsockErr(WSocket_Synchronized_WSAGetLastError));  { V5.26 }
        Exit;
    end;
    if FDnsLookupCheckMsg then begin
        FDnsLookupCheckMsg := FALSE;
        WMAsyncGetHostByName(FDnsLookupTempMsg);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ReverseDnsLookup(const HostAddr: String);
begin
    ReverseDnsLookup(HostAddr, IPPROTO_TCP);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ReverseDnsLookup(const HostAddr: String;
  const AProtocol: Integer);
{$IFDEF MSWINDOWS}
var
    lAddr  : u_long;
{$ENDIF}
begin
    if HostAddr = '' then begin
        RaiseException('Reverse DNS Lookup: Invalid host name.'); { V5.26 }
        TriggerDnsLookupDone(WSAEINVAL);
        Exit;
    end;
    { Cancel any pending lookup }
    if FDnsLookupHandle <> 0 then begin
      {$IFDEF MSWINDOWS}
        if FSocketFamily = sfIPv4 then
            WSocket_Synchronized_WSACancelAsyncRequest(FDnsLookupHandle)
        else
      {$ENDIF}
            IcsCancelAsyncRequest(FDnsLookupHandle);
        FDnsLookupHandle := 0;
    end;

    FDnsResult := '';
    FDnsResultList.Clear;
  {$IFDEF MSWINDOWS}
    if FSocketFamily = sfIPv4 then
        lAddr := WSocket_Synchronized_inet_addr(PAnsiChar(AnsiString(HostAddr)));
  {$ENDIF}
    if FWindowHandle = 0 then
        RaiseException('Reverse DNS Lookup: Window not assigned');  { V5.26 }
  {$IFDEF MSWINDOWS}
    if FSocketFamily = sfIPv4 then
        FDnsLookupHandle := WSocket_Synchronized_WSAAsyncGetHostByAddr(
                            FWindowHandle,
                            FMsg_WM_ASYNCGETHOSTBYADDR,
                            PAnsiChar(@lAddr), 4, PF_INET,
                            @FDnsLookupBuffer,
                            SizeOf(FDnsLookupBuffer))
    else
  {$ENDIF}
        FDnsLookupHandle := {WSocket_Synchronized_}IcsAsyncGetHostByAddr(
                            FWindowHandle,
                            FMsg_WM_ASYNCGETHOSTBYADDR,
                            FSocketFamily,
                            IcsTrim(HostAddr),
                            AProtocol);

    if FDnsLookupHandle = 0 then
        RaiseException(HostAddr + ': can''t start reverse DNS lookup - ' +
                       GetWinsockErr(WSocket_Synchronized_WSAGetLastError));  { V5.26 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ReverseDnsLookupSync(const HostAddr: String);
begin
    ReverseDnsLookupSync(HostAddr, IPPROTO_TCP);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ReverseDnsLookupSync(const HostAddr: String;
  const AProtocol: Integer); {AG 03/03/06}
var
    szAddr : array [0..256] of AnsiChar;
    lAddr  : u_long;
    Phe    : Phostent;

begin
    if (Length(HostAddr) = 0) or (Length(HostAddr) >= SizeOf(szAddr)) then begin
        RaiseException('Reverse DNS Lookup: Invalid host name.');   { V5.26 }
        TriggerDnsLookupDone(WSAEINVAL);
        Exit;
    end;
    { Cancel any pending lookup }
    if FDnsLookupHandle <> 0 then
    begin
      {$IFDEF MSWINDOWS}
        if FSocketFamily = sfIPv4 then
            WSocket_Synchronized_WSACancelAsyncRequest(FDnsLookupHandle)
        else
      {$ENDIF}
            IcsCancelAsyncRequest(FDnsLookupHandle);
    end;
    FDnsResult := '';
    if FSocketFamily = sfIPv4 then
    begin
        FDnsResultList.Clear;
        StrPCopy(szAddr, AnsiString(HostAddr)); { Length already checked above }

        lAddr := WSocket_Synchronized_inet_addr(szAddr);

        Phe := WSocket_Synchronized_gethostbyaddr(PAnsiChar(@lAddr), 4, AF_INET);
        if Phe = nil then
            TriggerDnsLookupDone(WSocket_Synchronized_WSAGetLastError)
        else begin
          {$IFDEF POSIX}
            FDnsResult := String(StrPas(Phe^.hname));
          {$ELSE}
            FDnsResult := String(StrPas(Phe^.h_name)); // Typo in Posix header
          {$ENDIF}
            FDnsResultList.Add(FDnsResult);
            GetAliasList(Phe, FDnsResultList);
            TriggerDnsLookupDone(0);
        end;
    end
    else begin
        lAddr := WSocket_Synchronized_ResolveName(HostAddr, TRUE, FSocketFamily,
                                                  FDnsResultList, AProtocol);
        if lAddr <> 0 then
            TriggerDnsLookupDone(lAddr)
        else begin
            if FDnsResultList.Count > 0 then
                FDnsResult := FDnsResultList[0];
            TriggerDnsLookupDone(0);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.BindSocket;
var
    SockName      : TSockAddrIn6;
    SockNamelen   : Integer;
    LocalSockName : TSockAddrIn6;
    LLocalAddr    : String;
    LSocketFamily : TSocketFamily;
begin
    if FAddrFormat = AF_INET6 then // requires Addr being resolved
    begin
        LSocketFamily := sfIPv6;
        LLocalAddr := FLocalAddr6;
    end
    else begin
        LSocketFamily := sfIPv4;
        LLocalAddr := FLocalAddr;
    end;
    FillChar(LocalSockName, Sizeof(LocalSockName), 0);
    if FSocketFamily = sfIPv4 then begin
        LocalSockName.sin6_family      := AF_INET;
        LocalSockName.sin6_port        := WSocket_Synchronized_htons(FLocalPortNum);
        PSockAddrIn(@LocalSockName)^.sin_addr.S_addr :=
              WSocket_Synchronized_ResolveHost(AnsiString(LLocalAddr)).s_addr;
    end
    else begin
        WSocket_Synchronized_ResolveHost(LLocalAddr, LocalSockName, LSocketFamily, FProto);
        LocalSockName.sin6_port := WSocket_Synchronized_htons(FLocalPortNum);
    end;
    SockNamelen := SizeOfAddr(LocalSockName);
    if WSocket_Synchronized_bind(HSocket, PSockAddrIn(@LocalSockName)^, SockNamelen) <> 0 then begin
        RaiseException('Bind socket failed - ' +
                       GetWinsockErr(WSocket_Synchronized_WSAGetLastError));  { V5.26 }
        Exit;
    end;
    if WSocket_Synchronized_GetSockName(FHSocket, PSockAddrIn(@SockName)^, SockNamelen) <> 0 then begin
        RaiseException('Winsock get socket name failed - ' +
                       GetWinsockErr(WSocket_Synchronized_WSAGetLastError));  { V5.26 }
        Exit;
    end;
    FLocalPortNum := WSocket_Synchronized_ntohs(SockName.sin6_port);
    FLocalPortStr := IntToStr(FLocalPortNum);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetKeepAliveOption;
var
    OptVal        : Integer;
    Status        : Integer;
  {$IFDEF MSWINDOWS}
    KeepAliveIn   : TTcpKeepAlive;
    KeepAliveOut  : TTcpKeepAlive;
    BytesReturned : Cardinal;
  {$ENDIF}
begin
    if FKeepAliveOnOff = wsKeepAliveOff then
        Exit;
    Assert(FHSocket <> INVALID_SOCKET); { V7.27 }
    if FKeepAliveOnOff = wsKeepAliveOnSystem then begin
        OptVal := 1;
        Status := WSocket_Synchronized_setsockopt(FHSocket, SOL_SOCKET,
                                                  SO_KEEPALIVE, @OptVal,
                                                  SizeOf(OptVal));

        if Status <> 0 then
            SocketError('setsockopt(SO_KEEPALIVE)');
        Exit;
    end;
  {$IFDEF MSWINDOWS}
    FillChar(KeepAliveIn, SizeOf(KeepAliveIn), 0);
    FillChar(KeepAliveOut, SizeOf(KeepAliveOut), 0);
    BytesReturned := 0;

    KeepAliveIn.OnOff             := 1;
    KeepAliveIn.KeepAliveInterval := FKeepAliveInterval;
    KeepAliveIn.KeepAliveTime     := FKeepAliveTime;
    Status := WSocket_WSAIoctl(FHSocket,      SIO_KEEPALIVE_VALS,
                               @KeepAliveIn,  SizeOf(KeepAliveIn),
                               @KeepAliveOut, SizeOf(KeepAliveOut),
                               BytesReturned, nil, nil);
    if Status <> 0 then begin
        SocketError('WSocket_WSAIoctl(SIO_KEEPALIVE_VALS)');
        Exit;
    end;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLingerOption;
var
    iStatus : Integer;
    li      : TLinger;
begin
    if FLingerOnOff = wsLingerNoSet then
        Exit;                            { Option set is disabled, ignore }

    if FHSocket = INVALID_SOCKET then begin
        RaiseException('Cannot set linger option at this time');
        Exit;
    end;

    li.l_onoff  := Ord(FLingerOnOff);    { 0/1 = disable/enable linger }
    li.l_linger := FLingerTimeout;       { timeout in seconds          }
    iStatus     := WSocket_Synchronized_setsockopt(FHSocket, SOL_SOCKET,
                                      SO_LINGER, @li, SizeOf(li));

    if iStatus <> 0 then begin
        SocketError('setsockopt(SO_LINGER)');
        Exit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.SetTcpNoDelayOption: Boolean; { V7.27 }
var
    optval  : Integer;
begin
    Assert(FHSocket <> INVALID_SOCKET);
    if HasOption(FComponentOptions, wsoTcpNoDelay) then
        optval := -1 { true }
    else
        optval := 0; { false }
    Result := WSocket_Synchronized_setsockopt(FHSocket, IPPROTO_TCP,
                                              TCP_NODELAY,
                                              @optval, SizeOf(optval)) = 0;
    if not Result then
        SocketError('setsockopt(IPPROTO_TCP, TCP_NODELAY)');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.SetAddressListChangeNotification: Boolean;
{$IFDEF MSWINDOWS}
var
    LBytesRcvd : LongWord;
begin
    if FHSocket <> INVALID_SOCKET then begin
        if FSelectEvent and FD_ADDRESS_LIST_CHANGE = 0 then begin
            FSelectEvent := FSelectEvent or FD_ADDRESS_LIST_CHANGE;
            Result := WSocket_Synchronized_WSAASyncSelect(FHSocket,
                                                          Handle,
                                                          FMsg_WM_ASYNCSELECT,
                                                          FSelectEvent) <> SOCKET_ERROR;
        end
        else
            Result := True;
        if Result then
            Result := (WSocket_WSAIoctl(FHSocket, SIO_ADDRESS_LIST_CHANGE, nil, 0,
                                        nil, 0, LBytesRcvd, nil, nil) <> SOCKET_ERROR) or
                      (WSocket_Synchronized_WSAGetLastError = WSAEWOULDBLOCK);
    end
    else
        Result := True;
{$ENDIF}
{$IFDEF POSIX}
begin
    Result := False;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.SetRoutingInterfaceChangeNotification: Boolean;
{$IFDEF MSWINDOWS}
var
    LBytesRcvd : LongWord;
begin
    if FHSocket <> INVALID_SOCKET then begin
        if FSelectEvent and FD_ROUTING_INTERFACE_CHANGE = 0 then begin
            FSelectEvent := FSelectEvent or FD_ROUTING_INTERFACE_CHANGE;
            Result := WSocket_Synchronized_WSAASyncSelect(FHSocket,
                                                          Handle,
                                                          FMsg_WM_ASYNCSELECT,
                                                          FSelectEvent) <> SOCKET_ERROR;
        end
        else
            Result := True;
        if Result then
            Result := (WSocket_WSAIoctl(FHSocket, SIO_ROUTING_INTERFACE_CHANGE,
                                        @Fsin, SizeOfAddr(Fsin), nil, 0, LBytesRcvd,
                                        nil, nil) <> SOCKET_ERROR) or
                      (WSocket_Synchronized_WSAGetLastError = WSAEWOULDBLOCK);
    end
    else
        Result := False;
{$ENDIF}
{$IFDEF POSIX}
begin
    Result := False;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Connect;
var
    iStatus : Integer;
    optval  : Integer;
    optlen  : Integer;
    lAddr   : TSockAddrIn6;
begin
    if (FHSocket <> INVALID_SOCKET) and (FState <> wsClosed) then begin
        RaiseException('Connect: Socket already in use');
        Exit;
    end;

    if  not FPortAssigned then begin
        RaiseException('Connect: No Port Specified');
        Exit;
    end;

    if not FAddrAssigned then begin
        RaiseException('Connect: No IP Address Specified');
        Exit;
    end;

    if not FProtoAssigned then begin
        RaiseException('Connect: No Protocol Specified');
        Exit;
    end;

    try
        if not FProtoResolved then begin
            { The next line will trigger an exception in case of failure }
            FProto := WSocket_Synchronized_ResolveProto(AnsiString(FProtoStr));
            case FProto of
                IPPROTO_UDP: FType := SOCK_DGRAM;
                IPPROTO_TCP: FType := SOCK_STREAM;
                IPPROTO_RAW: FType := SOCK_RAW;
            else
                FType := SOCK_RAW;
            end;
            FProtoResolved := TRUE;
        end;

        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FPortNum       := WSocket_Synchronized_ResolvePort(AnsiString(FPortStr), AnsiString(FProtoStr));
            Fsin.sin6_port := WSocket_Synchronized_htons(FPortNum);
            FPortResolved  := TRUE;
        end;

        if not FLocalPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FLocalPortNum      := WSocket_Synchronized_ResolvePort(AnsiString(FLocalPortStr), AnsiString(FProtoStr));
            FLocalPortResolved := TRUE;
        end;

        if not FAddrResolved then begin
            { The next line will trigger an exception in case of failure }
            if FSocketFamily = sfIPv4 then
            begin
                Fsin.sin6_family := AF_INET;
                PSockAddrIn(@Fsin).sin_addr.S_addr := WSocket_Synchronized_ResolveHost(AnsiString(FAddrStr)).s_addr;
            end
            else
                WSocket_Synchronized_ResolveHost(FAddrStr, Fsin, FSocketFamily, FProto);

            FAddrResolved := TRUE;
            FAddrFormat := Fsin.sin6_family;
        end;

    except
        on E:Exception do begin
            RaiseException('connect: ' + E.Message);
            Exit;
        end;
    end;

    { Remove any data from the internal output buffer }
    { (should already be empty !)                     }
    DeleteBufferedData;

    { Open the socket }
    FHSocket := WSocket_Synchronized_socket(FAddrFormat, FType, FProto);
    if FHSocket = INVALID_SOCKET then begin
        SocketError('Connect (socket)');
        Exit;
    end;

  {$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loWsockInfo) then
        DebugLog(loWsockInfo,
                IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' ' +
                'Socket handle created ' + IntToStr(FHSocket));
  {$ENDIF}

  {$IFDEF MACOS}
    { No SIGPIPE on writes but EPIPE in errno }
    optlen  := SizeOf(Integer);
    optval  := 1;
    iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET, SO_NOSIGPIPE,
                                  PAnsiChar(@optval), optlen);
    if iStatus <> 0 then begin
        SocketError('setsockopt(SO_NOSIGPIPE)');
        Exit;
    end;
  {$ENDIF}

    { Get winsock send buffer size }
    optlen  := SizeOf(FSocketSndBufSize);
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF,
                                  PAnsiChar(@FSocketSndBufSize), optlen);
    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_SNDBUF)');
        Exit;
    end;

    { Get winsock receive buffer size }
    optlen  := SizeOf(FSocketRcvBufSize);
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF,
                                  PAnsiChar(@FSocketRcvBufSize), optlen);
    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_RCVBUF)');
        Exit;
    end;

    { Trigger OnChangeState event }
    ChangeState(wsOpened);

    if FState <> wsOpened then begin  { 07/07/02 }
        { Socket has been closed in the OnChangeState event ! }
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('Connect (Invalid operation in OnChangeState)');
        Exit;
    end;
    if FType = SOCK_DGRAM then begin
        BindSocket;
        if FMultiCast then begin
            if FMultiCastIpTTL <> IP_DEFAULT_MULTICAST_TTL then begin
                optval  := FMultiCastIpTTL; { set time-to-live for multicast }
                if FAddrFormat = PF_INET then
                    iStatus := WSocket_Synchronized_SetSockOpt(FHSocket,
                                                           IPPROTO_IP,
                                                           IP_MULTICAST_TTL,
                                                           @optval,
                                                           SizeOf(optval))
                else if FAddrFormat = PF_INET6 then
                    iStatus := WSocket_Synchronized_SetSockOpt(FHSocket,
                                                           IPPROTO_IPV6,
                                                           IPV6_MULTICAST_HOPS,
                                                           @optval,
                                                           SizeOf(optval))
                else begin
                    SocketError('setsockopt(IP_MULTICAST_TTL) Invalid address format');
                        Exit;
                end;
                if iStatus <> 0 then begin
                        SocketError('setsockopt(IP_MULTICAST_TTL)');
                        Exit;
                end;
            end;
            if ((FAddrFormat = AF_INET) and (FLocalAddr <> ICS_ANY_HOST_V4)) or
               ((FAddrFormat = AF_INET6) and (FLocalAddr6 <> ICS_ANY_HOST_V6)) then begin
                if FAddrFormat = PF_INET then
                    PSockAddrIn(@laddr)^.sin_addr.S_addr :=
                    WSocket_Synchronized_ResolveHost(AnsiString(FLocalAddr)).S_addr
                else
                    WSocket_Synchronized_ResolveHost(FLocalAddr6, laddr, sfIPv6, FProto);

                if FAddrFormat = PF_INET then
                    iStatus := WSocket_Synchronized_SetSockOpt(FHSocket,
                                                          IPPROTO_IP,
                                                          IP_MULTICAST_IF,
                                                          @PSockAddr(@laddr)^.sin_addr.S_addr,
                                                          SizeOf(PSockAddr(@laddr)^.sin_addr.S_addr))
                else if FAddrFormat = PF_INET6 then
                begin
                    OptVal := 0; // Default IPv6 interface, fix me!
                    iStatus := WSocket_Synchronized_SetSockOpt(FHSocket,
                                                          IPPROTO_IPV6,
                                                          IPV6_MULTICAST_IF,
                                                          PAnsiChar(@OptVal),
                                                          SizeOf(OptVal));
                end
                else begin
                    SocketError('setsockopt(IP_MULTICAST_TTL) Invalid address format');
                        Exit;
                end;

                if iStatus <> 0 then begin
                    SocketError('setsockopt(IP_MULTICAST_IF)');
                    Exit;
                end;
            end;                                                       { /RK }
        end;
        //if sin.sin_addr.S_addr = u_long(INADDR_BROADCAST) then begin
        if ((Fsin.sin6_family = AF_INET) and (PSockAddrIn(@FSin).sin_addr.S_addr = u_long(INADDR_BROADCAST))) or
           ((Fsin.sin6_family = AF_INET6) and IN6_IS_ADDR_MULTICAST({$IFNDEF POSIX}@{$ENDIF}Fsin.sin6_addr)) then
        begin
            OptVal  := 1;
            iStatus := WSocket_Synchronized_setsockopt(FHSocket, SOL_SOCKET,
                                                       SO_BROADCAST,
                                                       @OptVal, SizeOf(OptVal));
            if iStatus <> 0 then begin
                SocketError('setsockopt(SO_BROADCAST)');
                Exit;
            end;
        end;

        FSelectEvent := FD_READ or FD_WRITE;
    end
    else begin
        { Socket type is SOCK_STREAM }
        optval  := -1;
        iStatus := WSocket_Synchronized_setsockopt(FHSocket, SOL_SOCKET,
                                                   SO_REUSEADDR, @optval,
                                                   SizeOf(optval));

        if iStatus <> 0 then begin
            SocketError('setsockopt(SO_REUSEADDR)');
            Exit;
        end;

        if HasOption(FComponentOptions, wsoTcpNoDelay) and { V7.27 }
                    (not SetTcpNoDelayOption) then
            Exit;
        SetLingerOption;
        SetKeepAliveOption;

        if (FLocalPortNum <> 0) or
           ((FAddrFormat = AF_INET) and (FLocalAddr <> ICS_ANY_HOST_V4)) or
           ((FAddrFormat = AF_INET6) and (FLocalAddr6 <> ICS_ANY_HOST_V6)) then
            BindSocket;

        FSelectEvent := FD_READ or FD_WRITE or FD_CLOSE or FD_CONNECT;
    end;

  {$IFDEF MSWINDOWS}
    if wsoNotifyAddressListChange in ComponentOptions then
        FSelectEvent := FSelectEvent or FD_ADDRESS_LIST_CHANGE;
    if wsoNotifyRoutingInterfaceChange in ComponentOptions then
        FSelectEvent := FSelectEvent or FD_ROUTING_INTERFACE_CHANGE;
  {$ENDIF}

    iStatus       := WSocket_Synchronized_WSAASyncSelect(
                                                       {$IFDEF POSIX}
                                                         Self,
                                                       {$ENDIF}
                                                         FHSocket,
                                                         Handle,
                                                         FMsg_WM_ASYNCSELECT,
                                                         FSelectEvent);
    if iStatus <> 0 then begin
        SocketError('WSAAsyncSelect');
        Exit;
    end;

  {$IFDEF MSWINDOWS}
    if (wsoNotifyAddressListChange in ComponentOptions) and
       (not SetAddressListChangeNotification) then begin
        SocketError('Connect: SetAddressListChangeNotification');
        Exit;
    end;

    if (wsoNotifyRoutingInterfaceChange in ComponentOptions) and
       (not SetRoutingInterfaceChangeNotification) then begin
        SocketError('Connect: SetRoutingInterfaceChangeNotification');
        Exit;
    end;
  {$ENDIF}

    if FType = SOCK_DGRAM then begin
        ChangeState(wsConnected);
        TriggerSessionConnectedSpecial(0);
    end
    else begin
      {$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loWsockInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            if Fsin.sin6_family = AF_INET then
                DebugLog(loWsockInfo, 'TWSocket will connect to ' +
                  WSocket_Synchronized_inet_ntoa(PSockAddrIn(@Fsin).sin_addr) + ':' +
                  IntToStr(WSocket_Synchronized_ntohs(PSockAddrIn(@Fsin).sin_port)))
            else
                DebugLog(loWsockInfo, 'TWSocket will connect to ' +
                  WSocketIPv6ToStr(PIcsIpv6Address(@Fsin.sin6_addr)^) + ':' +
                  IntToStr(WSocket_Synchronized_ntohs(Fsin.sin6_port)));

      {$ENDIF}
        iStatus := WSocket_Synchronized_Connect(FHSocket, PSockAddrIn(@Fsin)^,
                                                SizeOfAddr(Fsin));
        if iStatus = 0 then begin
            ChangeState(wsConnecting);
        end
        else begin
            iStatus := WSocket_Synchronized_WSAGetLastError;
            if (iStatus = WSAEWOULDBLOCK)
               {$IFDEF POSIX} or (iStatus = WSAEINPROGRESS) {$ENDIF} then
                ChangeState(wsConnecting)
            else begin
                FLastError := WSocket_Synchronized_WSAGetLastError;
                SocketError('Connect');
                Exit;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Listen;
var
    iStatus        : Integer;
    optval         : Integer;
    optlen         : Integer;
    mreq           : ip_mreq;
    mreqv6         : TIpv6MReq;
    Success        : Boolean;
{$IFDEF MSWINDOWS}
    dwBufferInLen  : DWORD;
    dwBufferOutLen : DWORD;
    dwDummy        : DWORD;
{$ENDIF}
begin
    if not FPortAssigned then begin
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('listen: port not assigned');
        Exit;
    end;

    if not FProtoAssigned then begin
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('listen: protocol not assigned');
        Exit;
    end;

    if not FAddrAssigned then begin
        WSocket_Synchronized_WSASetLastError(WSAEINVAL);
        SocketError('listen: address not assigned');
        Exit;
    end;

    try
        if not FProtoResolved then begin
            { The next line will trigger an exception in case of failure }
            if IcsCompareText(Copy(FProtoStr, 1, 4), 'raw_') = 0 then begin
                FType  := SOCK_RAW;
                FProto := WSocket_Synchronized_ResolveProto(AnsiString(Copy(FProtoStr, 5, 10)));
            end
            else begin
                FProto := WSocket_Synchronized_ResolveProto(AnsiString(FProtoStr));
                if FProto = IPPROTO_UDP then
                    FType := SOCK_DGRAM
                else
                    FType := SOCK_STREAM;
            end;
            FProtoResolved := TRUE;
        end;

        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FPortNum       := WSocket_Synchronized_ResolvePort(
                                  AnsiString(FPortStr), AnsiString(FProtoStr));
            Fsin.sin6_port := WSocket_Synchronized_htons(FPortNum);
            FPortResolved  := TRUE;
        end;

        if not FAddrResolved then begin
            { The next line will trigger an exception in case of failure }
            if FSocketFamily = sfIPv4 then
            begin
                Fsin.sin6_family := AF_INET;
                PSockAddrIn(@Fsin).sin_addr.s_addr := WSocket_Synchronized_ResolveHost(AnsiString(FAddrStr)).s_addr;
            end
            else
                WSocket_Synchronized_ResolveHost(FAddrStr, Fsin, FSocketFamily, FProto);

            FAddrResolved := TRUE;
            FAddrFormat := Fsin.sin6_family;
        end;
    except
        on E:Exception do begin
            RaiseException('listen: ' + E.Message);
            Exit;
        end;
    end;

    { Remove any data from the internal output buffer }
    { (should already be empty !)                     }
    DeleteBufferedData;

    FHSocket := WSocket_Synchronized_socket(FAddrFormat, FType, FProto);

    if FHSocket = INVALID_SOCKET then begin
        SocketError('socket');
        exit;
    end;

    if FType = SOCK_DGRAM then begin
        if FReuseAddr then begin
        { Enable multiple tasks to listen on duplicate address and port }
            optval  := -1;
            iStatus := WSocket_Synchronized_SetSockOpt(FHSocket, SOL_SOCKET,
                                                       SO_REUSEADDR,
                                                       @optval, SizeOf(optval));

            if iStatus <> 0 then begin
                SocketError('setsockopt(SO_REUSEADDR)');
                Close;
                Exit;
            end;
        end;
    end;

    iStatus := WSocket_Synchronized_bind(FHSocket, PSockAddr(@Fsin)^,
                                         SizeOfAddr(Fsin));
    if iStatus = 0 then
        ChangeState(wsBound)
    else begin
        try
            SocketError('Bind');
        finally
            WSocket_Synchronized_closesocket(FHSocket);
            FHSocket := INVALID_SOCKET;
        end;
        Exit;
    end;

    case FType of
{$IFDEF MSWINDOWS}
    SOCK_RAW :
        begin
            if HasOption(FComponentOptions, wsoSIO_RCVALL) then begin
                dwBufferInLen  := 1;
                dwBufferOutLen := 0;
                iStatus := WSocket_Synchronized_WSAIoctl(FHSocket, SIO_RCVALL,
                    @dwBufferInLen,  SizeOf(dwBufferInLen),
                    @dwBufferOutLen, SizeOf(dwBufferOutLen),
                    dwDummy, nil, nil);

                if iStatus = SOCKET_ERROR then begin
                    SocketError('WSAIoctl(SIO_RCVALL)');
                    Close;
                    Exit;
                end;
            end;
            ChangeState(wsListening);
            ChangeState(wsConnected);
            TriggerSessionConnectedSpecial(0);
        end;
{$ENDIF}
    SOCK_DGRAM :
        begin
            if FMultiCast then begin
                if FAddrFormat = AF_INET then begin
                    { Use setsockopt() to join a multicast group }
                    { mreq.imr_multiaddr.s_addr := WSocket_inet_addr('225.0.0.37');}
                    { mreq.imr_multiaddr.s_addr := sin.sin_addr.s_addr;}
                    { mreq.imr_multiaddr.s_addr := WSocket_inet_addr(FAddrStr);}
                    mreq.imr_multiaddr.s_addr := WSocket_Synchronized_inet_addr(PAnsiChar(AnsiString(FMultiCastAddrStr)));
                    { mreq.imr_interface.s_addr := htonl(INADDR_ANY);} { RK}
                    mreq.imr_interface.s_addr := WSocket_Synchronized_ResolveHost(AnsiString(FAddrStr)).s_addr;
                    iStatus := WSocket_Synchronized_SetSockOpt(FHSocket, IPPROTO_IP,
                                                               IP_ADD_MEMBERSHIP,
                                                               @mreq, SizeOf(mreq));

                    if iStatus <> 0 then begin
                        SocketError('setsockopt(IP_ADD_MEMBERSHIP)');
                        Exit;
                    end;
                end
                else if FAddrFormat = AF_INET6 then begin
                    PIcsIPv6Address(@mreqv6.ipv6mr_multiaddr)^ := WSocketStrToIPv6(FMultiCastAddrStr, Success);
                    if not Success then begin
                        SocketError('setsockopt(IPV6_ADD_MEMBERSHIP) Invalid multicast address');
                        Exit;
                    end;
                    mreqv6.ipv6mr_interface := 0; // IPv6 default interface, fix me!
                    iStatus := WSocket_Synchronized_SetSockOpt(FHSocket,
                                                           IPPROTO_IPV6,
                                                           IPV6_ADD_MEMBERSHIP,
                                                           PAnsiChar(@mreqv6),
                                                           SizeOf(mreqv6));

                    if iStatus <> 0 then begin
                        SocketError('setsockopt(IPV6_ADD_MEMBERSHIP)');
                        Exit;
                    end;
                end
                else begin
                    SocketError('setsockopt(IP_ADD_MEMBERSHIP) Invalid address format');
                        Exit;
                end;
            end;
            ChangeState(wsListening);
            ChangeState(wsConnected);
            TriggerSessionConnectedSpecial(0);
        end;
    SOCK_STREAM :
        begin
            iStatus := WSocket_Synchronized_Listen(FHSocket, FListenBacklog);
            if iStatus = 0 then
                ChangeState(wsListening)
            else begin
                SocketError('Listen');
                Exit;
            end;
        end;
    else
        SocketError('Listen: unexpected protocol.');
        Exit;
    end;

    { Get winsock send buffer size - V7.84 }
    optlen  := SizeOf(FSocketSndBufSize);
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF,
                                  PAnsiChar(@FSocketSndBufSize), optlen);

    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_SNDBUF)');
        Exit;
    end;

    { Get winsock receive buffer size }
    optlen  := SizeOf(FSocketRcvBufSize);
    iStatus := WSocket_getsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF,
                                  PAnsiChar(@FSocketRcvBufSize), optlen);

    if iStatus <> 0 then begin
        SocketError('getsockopt(SO_RCVBUF)');
        Exit;
    end;

    { FP:26/09/06 Are FD_READ and FD_WRITE really necessary ? Probably not ! }
    { Lodewijk Ellen reported a problem with W2K3SP1 triggering an AV in     }
    { accept. Keeping only FD_ACCEPT and FD_CLOSE solved the problem.        }
    { Anyway, a listening socket doesn't send nor receive any data so those  }
    { notification are useless.                                              }
    if FType = SOCK_STREAM then
        FSelectEvent := FD_ACCEPT or FD_CLOSE
    else
        FSelectEvent := FD_READ or FD_WRITE; // works in both Win and Posix

  {$IFDEF MSWINDOWS}
    if wsoNotifyAddressListChange in ComponentOptions then
        FSelectEvent := FSelectEvent or FD_ADDRESS_LIST_CHANGE;
    if wsoNotifyRoutingInterfaceChange in ComponentOptions then
        FSelectEvent := FSelectEvent or FD_ROUTING_INTERFACE_CHANGE;
  {$ENDIF}

    iStatus      := WSocket_Synchronized_WSAASyncSelect(
                                                      {$IFDEF POSIX}
                                                        Self,
                                                      {$ENDIF}
                                                        FHSocket,
                                                        Handle,
                                                        FMsg_WM_ASYNCSELECT,
                                                        FSelectEvent);
    if iStatus <> 0 then begin
        SocketError('WSAASyncSelect');
        exit;
    end;

  {$IFDEF MSWINDOWS}
    if (wsoNotifyAddressListChange in ComponentOptions) and
       (not SetAddressListChangeNotification) then begin
        SocketError('Listen: SetAddressListChangeNotification');
        Exit;
    end;

    if (wsoNotifyRoutingInterfaceChange in ComponentOptions) and
       (not SetRoutingInterfaceChangeNotification) then begin
        SocketError('Listen: SetRoutingInterfaceChangeNotification');
        Exit;
    end;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.Accept: TSocket;
var
    len : Integer;
  {$IFDEF POSIX}
    LastErr: Integer;
  {$ENDIF}
begin
  {$IFDEF POSIX}
    try
  {$ENDIF}
        if FState <> wsListening then begin
            WSocket_Synchronized_WSASetLastError(WSAEINVAL);
            SocketError('not a listening socket');
            Result := INVALID_SOCKET;
            Exit;
        end;
        len := SizeOfAddr(sin6);
        FASocket := WSocket_Synchronized_Accept(FHSocket, @sin6, @len);
        Result := FASocket;

        if (FASocket = INVALID_SOCKET) then begin
          {$IFDEF MSWINDOWS}
            SocketError('Accept');
          {$ENDIF}
          {$IFDEF POSIX}
            LastErr := WSocket_WSAGetLastError;
            if LastErr <> WSAEWOULDBLOCK then
                SocketError('Accept', LastErr);
          {$ENDIF}
            Exit;
        end;
  {$IFDEF POSIX}
    finally
        if FState = wsListening then
            WSocketSynchronizedEnableAcceptEvent(Self);
    end;
  {$ENDIF}

{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loWsockInfo) then
        DebugLog(loWsockInfo,
                IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' ' +
                'Socket accepted ' + IntToStr(FASocket));
{$ENDIF}

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Pause;
begin
    FPaused := TRUE;
    WSocket_Synchronized_WSAASyncSelect(
                                      {$IFDEF POSIX}
                                        Self,
                                      {$ENDIF}
                                        FHSocket,
                                        Handle, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Resume;
begin
    FPaused := FALSE;
    WSocket_Synchronized_WSAASyncSelect(
                                      {$IFDEF POSIX}
                                        Self,
                                      {$ENDIF}
                                        FHSocket,
                                        Handle,
                                        FMsg_WM_ASYNCSELECT, FSelectEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Shutdown(How : Integer);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loWsockInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
      DebugLog(loWsockInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' ' +
               'TCustomWSocket.Shutdown ' + IntToStr(How) + ' ' + IntToStr(FHSocket));
{$ENDIF}
    if FHSocket <> INVALID_SOCKET then begin
      {$IFDEF POSIX}
        WSocketSynchronizedSetShutdownCalled(Self, How);
      {$ENDIF}
        WSocket_Synchronized_Shutdown(FHSocket, How);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DeleteBufferedData;
begin
    if Assigned(FBufHandler) then begin
        FBufHandler.Lock;
        try
            FBufHandler.DeleteAllData;
            FBufferedByteCount := 0;
        finally
            FBufHandler.UnLock;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Abort;
begin
    InternalAbort(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.InternalAbort(ErrCode : Word);
begin
    InternalCancelDnsLookup(TRUE);
    DeleteBufferedData;
    { Be sure to close as fast as possible (abortive close) }
    if (State = wsConnected) and (FProto = IPPROTO_TCP) then begin
        LingerOnOff := wsLingerOff;
        SetLingerOption;
    end;
    InternalClose(FALSE, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Close;
begin
    InternalClose(TRUE, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.CloseDelayed;
begin
    PostMessage(Handle, FMsg_WM_CLOSE_DELAYED, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//procedure TCustomWSocket.Release;
//begin
//    PostMessage(Handle, FMsg_WM_WSOCKET_RELEASE, 0, 0);
//end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMCloseDelayed(var msg: TMessage);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//procedure TCustomWSocket.WMRelease(var msg: TMessage);
//begin
//    Destroy;
//end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Flush;
begin
    while (FHSocket <> INVALID_SOCKET) and     { No more socket   }
          (not bAllSent) do begin              { Nothing to send  }
            { Break; }
        TryToSend;
        MessagePump;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.InternalClose(bShut : Boolean; Error : Word);
var
    iStatus : Integer;
{    Buffer  : array [0..127] of Char; }
begin
    if FHSocket = INVALID_SOCKET then begin
        if FState <> wsClosed then begin
            ChangeState(wsClosed);
            AssignDefaultValue;
        end;
        Exit;
    end;

    if FState = wsClosed then
        Exit;

{ 11/10/98 called shutdown(1) instead of shutdown(2). This disables only    }
{ sends. Disabling receives as well produced data lost is some cases.       }
{ Manifest constants for Shutdown                                           }
{  SD_RECEIVE = 0;   disables receives                                      }
{  SD_SEND    = 1;   disables sends, *Use this one for graceful close*      }
{  SD_BOTH    = 2;   disables both sends and receives                       }

    if bShut then
        ShutDown(1);

    if FHSocket <> INVALID_SOCKET then begin
        repeat
          {$IFDEF MSWINDOWS}
            { Disable winsock notification otherwise notifications may be }
            { posted even after the call to closesocket()                 }
            WSocket_Synchronized_WSAASyncSelect(FHSocket, Handle, 0, 0); { V8.05 }
          {$ENDIF}
            { Close the socket }
            iStatus := WSocket_Synchronized_closesocket(FHSocket);
            if iStatus <> 0 then begin
                FLastError := WSocket_Synchronized_WSAGetLastError;
                if FLastError <> WSAEWOULDBLOCK then begin
                  {$IFDEF POSIX}
                    WSocketSynchronizedRemoveEvents(Self, False);
                    IcsClearMessages(Handle, FMsg_WM_ASYNCSELECT, WPARAM(FHSocket));
                  {$ENDIF}
                    FHSocket := INVALID_SOCKET;
                  {$IFDEF MSWINDOWS}
                    { Ignore the error occuring when winsock DLL not      }
                    { initialized (occurs when using TWSocket from a DLL) }
                    if FLastError = WSANOTINITIALISED then
                        break;
                  {$ENDIF}
                    SocketError('Disconnect (closesocket)');
                    Exit;
                end;
              {$IFDEF MSWINDOWS}
                { Next line is untested, however I think we have to reenable }
                { socket notification here.  (AG)                            }
                WSocket_Synchronized_WSAASyncSelect(FHSocket, Handle,     { V8.05 }
                                        FMsg_WM_ASYNCSELECT, FSelectEvent);
              {$ENDIF}
                MessagePump;
            end;
        until iStatus = 0;
      {$IFDEF POSIX}
        WSocketSynchronizedRemoveEvents(Self, True);
        IcsClearMessages(Handle, FMsg_WM_ASYNCSELECT, WPARAM(FHSocket));
      {$ENDIF}
        FHSocket := INVALID_SOCKET;
    end;

    ChangeState(wsClosed);
    if (not (csDestroying in ComponentState)) and
       (not FCloseInvoked) {and Assigned(FOnSessionClosed)} then begin
        FCloseInvoked := TRUE;
       TriggerSessionClosed(Error);
    end;
    { 29/09/98 Protect AssignDefaultValue because SessionClosed event handler }
    { may have destroyed the component.                                       }
    try
        AssignDefaultValue;
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WaitForClose;
var
    lCount    : {$IFDEF FPC} LongWord; {$ELSE} u_long; {$ENDIF}
    Status    : Integer;
    DataBuf   : TWSocketData;
    Ch        : AnsiChar;
begin
    while (FHSocket <> INVALID_SOCKET) and (FState <> wsClosed) do begin
        MessagePump;

        if WSocket_Synchronized_ioctlsocket(FHSocket, FIONREAD, lCount) = SOCKET_ERROR then
            break;
        if lCount > 0 then
            TriggerDataAvailable(0);
        DataBuf := @Ch;
        Status := DoRecv(DataBuf, 1, 0);
        if Status <= 0 then begin
            FLastError := WSocket_Synchronized_WSAGetLastError;
            if FLastError <> WSAEWOULDBLOCK then
                break;
        end;
        MessagePump;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketGetHostByAddr(Addr : AnsiString) : PHostEnt;
var
    szAddr : array[0..256] of AnsiChar;
    lAddr  : u_long;
begin
    if (Length(Addr) = 0) or (Length(Addr) >= SizeOf(szAddr)) then
        raise ESocketException.Create('Winsock Get Host Addr: Invalid address.');   { V5.26 }

    StrPCopy(szAddr, Addr); { Length already checked above }
    lAddr  := WSocket_inet_addr(szAddr);
    Result := WSocket_gethostbyaddr(PAnsiChar(@lAddr), 4, PF_INET);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveIp(
    const IpAddr        : AnsiString;
    const ASocketFamily : TSocketFamily = DefaultSocketFamily;
    const AProtocol     : Integer  = IPPROTO_TCP) : AnsiString;
var
    Phe     : PHostEnt;
    ResList : TStringList;
begin
    if ASocketFamily = sfIPv4 then begin
        phe := WSocketGetHostByAddr(IpAddr);
        if Phe = nil then
            Result := ''
        else begin
          {$IFDEF MSWINDOWS}
            SetLength(Result, StrLen(Phe^.h_name));
            StrCopy(@Result[1], Phe^.h_name);
          {$ELSE}
            SetLength(Result, StrLen(Phe^.hname));
            StrCopy(@Result[1], Phe^.hname);
          {$ENDIF}
        end;
    end
    else begin
        ResList := TStringList.Create;
        try
            if (WSocket_ResolveName(string(IpAddr), TRUE, ASocketFamily, ResList, AProtocol) <> 0) or
               (ResList.Count = 0) then
            begin
                Result := '';
            end
            else
                Result := AnsiString(ResList[0]);
        finally
            ResList.Free;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketGetHostByName(Name : AnsiString) : PHostEnt;
var
    szName : array[0..256] of AnsiChar;
begin
    if (Length(Name) = 0) or (Length(Name) >= SizeOf(szName)) then
        raise ESocketException.Create('Winsock Get Host Name: Invalid Hostname.');   { V5.26 }

    StrPCopy(szName, Name);
    Result := WSocket_gethostbyname(szName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure GetLocalIPList(AIPList: TStrings;
  const ASocketFamily: TSocketFamily = DefaultSocketFamily;
  const AProtocol: Integer = IPPROTO_TCP);
begin
{$IFNDEF NO_ADV_MT}
    CritSecIpList.Enter;
    try
{$ENDIF}
        AIPList.Assign(LocalIPList(ASocketFamily, AProtocol));
{$IFNDEF NO_ADV_MT}
    finally
        CritSecIpList.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LocalIPList(const ASocketFamily: TSocketFamily = DefaultSocketFamily;
  const AProtocol: Integer  = IPPROTO_TCP) : TStrings;
var
    phe           : PHostEnt;
    Hints         : TAddrInfo;
    AddrInfo      : PAddrInfo;
    AddrInfoNext  : PAddrInfo;
    RetVal        : Integer;
    LHostName     : string;
    Idx           : Integer;
begin
{$IFNDEF NO_ADV_MT}
    CritSecIpList.Enter;
    try
{$ENDIF}
        IPList.Clear;
        Result := IPList;

        if ASocketFamily = sfIPv4 then begin
            phe  := WSocketGetHostByName(LocalHostName);
            if phe <> nil then
                GetIpList(Phe, IPList);
        end
        else begin
            LHostName := string(LocalHostName);
            FillChar(Hints, SizeOf(Hints), 0);
            if ASocketFamily = sfIPv6 then
                Hints.ai_family := AF_INET6
            else if ASocketFamily = sfIPv4 then
                Hints.ai_family := AF_INET;
            AddrInfo := nil;
            Hints.ai_protocol := AProtocol;
          {$IFDEF MSWINDOWS}
            RetVal := WSocket_GetAddrInfo(PChar(LHostName), nil, @Hints, AddrInfo);
          {$ELSE}
            RetVal := WSocket_GetAddrInfo(PAnsiChar(UnicodeToAnsi(LHostName, CP_UTF8)), nil, @Hints, AddrInfo);
          {$ENDIF}
            if RetVal <> 0 then
                raise ESocketException.Create(
                    'Winsock GetAddrInfo: Cannot convert host address ''' +
                    LHostName + ''' - ' + GetWinsockErr(RetVal));
            try
                AddrInfoNext := AddrInfo;
                IDX := 0;
                while AddrInfoNext <> nil do
                begin
                    if AddrInfoNext.ai_family = AF_INET then
                    begin
                        if ASocketFamily = sfAnyIPv4 then
                        begin
                            IPList.Insert(IDX,
                          {$IFDEF MSWINDOWS}
                            WSocketIPv4ToStr(AddrInfoNext^.ai_addr^.sin_addr.S_addr));
                          {$ELSE}
                            WSocketIPv4ToStr(PSockAddrIn(AddrInfoNext^.ai_addr)^.sin_addr.S_addr));
                          {$ENDIF}
                            Inc(IDX);
                        end
                        else
                            IPList.Add(
                          {$IFDEF MSWINDOWS}
                            WSocketIPv4ToStr(AddrInfoNext^.ai_addr^.sin_addr.S_addr)
                          {$ELSE}
                            WSocketIPv4ToStr(PSockAddrIn(AddrInfoNext^.ai_addr)^.sin_addr.S_addr)
                          {$ENDIF}
                            );
                    end
                    else if AddrInfoNext.ai_family = AF_INET6 then
                    begin
                        if ASocketFamily = sfAnyIPv6 then
                        begin
                            IPList.Insert(IDX,
                            WSocketIPv6ToStr(PSockAddrIn6(AddrInfoNext.ai_addr)));
                            Inc(IDX);
                        end
                        else
                            IPList.Add(
                            WSocketIPv6ToStr(PSockAddrIn6(AddrInfoNext.ai_addr)));
                    end;
                    AddrInfoNext := AddrInfoNext.ai_next;
                end;
            finally
                WSocket_FreeAddrInfo(AddrInfo);
            end;
        end;
{$IFNDEF NO_ADV_MT}
    finally
        CritSecIpList.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LocalHostName : AnsiString;
begin
    if WSocket_gethostname(Result) <> 0 then
        raise ESocketException.Create('Winsock Get Host Name failed');

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.TimerIsSet(var tvp : TTimeVal) : Boolean;
begin
    Result := (tvp.tv_sec <> 0) or (tvp.tv_usec <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.TimerCmp(var tvp : TTimeVal; var uvp : TTimeVal; IsEqual : Boolean) : Boolean;
begin
    Result := (tvp.tv_sec = uvp.tv_sec) and (tvp.tv_usec = uvp.tv_usec);
    if not IsEqual then
        Result := not Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TimerClear(var tvp : TTimeVal);
begin
   tvp.tv_sec  := 0;
   tvp.tv_usec := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSendFlags(newValue : TSocketSendFlags);
begin
    case newValue of
    wsSendNormal: FSendFlags := 0;
    wsSendUrgent: FSendFlags := MSG_OOB;
    else
        RaiseException('Invalid SendFlags');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetSendFlags : TSocketSendFlags;
begin
    case FSendFlags of
    0       : Result := wsSendNormal;
    MSG_OOB : Result := wsSendUrgent;
    else
        RaiseException('Invalid internal SendFlags');
        Result := wsSendNormal;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerDebugDisplay(Msg : String);
begin
    if Assigned(FOnDebugDisplay) then
        FOnDebugDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSendData(BytesSent : Integer);
begin
    if Assigned(FOnSendData) then
        FOnSendData(Self, BytesSent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionAvailable(Error : Word);
begin
    if Assigned(FOnSessionAvailable) then
        FOnSessionAvailable(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionConnectedSpecial(Error : Word);
begin
    if Assigned(FCounter) and (FType = SOCK_STREAM) and (Error = 0) then
        FCounter.SetConnected;
    TriggerSessionConnected(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionConnected(Error : Word);
begin
    FReadCount  := 0;  { 7.24 }
    FWriteCount := 0;  { 7.24 }
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionClosed(Error : Word);
begin
    if Assigned(FOnSessionClosed) then
        FOnSessionClosed(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.TriggerDataAvailable(Error : Word) : Boolean;
begin
    Result := Assigned(FOnDataAvailable);
    if not Result then
        Exit;
{$IFDEF TOMASEK}                    { 23/01/99 }
    { Do not allow FD_READ messages, this will prevent reentering the }
    { OnDataAvailable event handler.                                  }
    FSelectEvent := FD_WRITE or FD_CLOSE or FD_CONNECT;
    WSocket_Synchronized_WSAASyncSelect({$IFDEF POSIX}
                                        Self,
                                        {$ELSE}
                                        FHSocket,
                                        {$ENDIF}
                                        Handle, WM_ASYNCSELECT, FSelectEvent);
    try
        FRcvdFlag := TRUE;
        while Result and FRcvdFlag do begin
            { Trigger user code. This will normally call DoRecv which will }
            { update FRcvdFlag.                                            }
            { If user code is wrong, we'll loop forever !                  }
            FOnDataAvailable(Self, Error);
            Result := Assigned(FOnDataAvailable);
        end;
    finally
        { Allow all events now }
        FSelectEvent := FD_READ or FD_WRITE or FD_CLOSE or FD_CONNECT;
        WSocket_Synchronized_WSAASyncSelect({$IFDEF POSIX}
                                            Self,
                                            {$ELSE}
                                            FHSocket,
                                            {$ENDIF}
                                            Handle, WM_ASYNCSELECT, FSelectEvent);
    end;
{$ELSE}                             { 23/01/99 }
    FOnDataAvailable(Self, Error);  { 23/01/99 }
{$ENDIF}                            { 23/01/99 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerDataSent(Error : Word);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loWsockDump) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loWsockDump,
                IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) + ' ' +
                'TriggerDataSent ' + IntToStr(FHSocket));
{$ENDIF}
    if Assigned(FOnDataSent) then
        FOnDataSent(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerError;
begin
    if Assigned(FOnError) then
        FOnError(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerDNSLookupDone(Error : Word);
begin
    if Assigned(FOnDNSLookupDone) then
        FOnDNSLookupDone(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerChangeState(OldState, NewState : TSocketState);
begin
    if Assigned(FOnChangeState) then
        FOnChangeState(Self, OldState, NewState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SocketError(sockfunc: String; LastError: Integer = 0);
var
    Error  : Integer;
    Line   : String;
begin
    if LastError = 0 then
        Error := WSocket_Synchronized_WSAGetLastError
    else
        Error := LastError;
{    Line  := 'Error '+ IntToStr(Error) + ' in function ' + sockfunc +
             #13#10 + WSocketErrorDesc(Error);  }
    Line  := WSocketErrorDesc(Error) + ' (#' + IntToStr(Error) +
                                         ' in ' + sockfunc + ')' ;   { V5.26 }

    if (Error = WSAECONNRESET) or
       (Error = WSAENOTCONN) then begin
        WSocket_Synchronized_closesocket(FHSocket);
        FHSocket := INVALID_SOCKET;
        if FState <> wsClosed then
           TriggerSessionClosed(Error);
        ChangeState(wsClosed);
    end;

    FLastError := Error;
    RaiseException(Line);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} { V5.21 }
{$IFNDEF NO_DEBUG_LOG}
function TCustomWSocket.CheckLogOptions(const LogOption: TLogOption): Boolean;
begin
    Result := Assigned(FIcsLogger) and (LogOption in FIcsLogger.LogOptions);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DebugLog(LogOption: TLogOption; const Msg: String);  { V5.21 }
begin
    if Assigned(FIcsLogger) then
        FIcsLogger.DoDebugLog(Self, LogOption, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} { V5.21 }
procedure TCustomWSocket.SetIcsLogger(const Value: TIcsLogger);
begin
    if Value <> FIcsLogger then begin
        if FIcsLogger <> nil then
            FIcsLogger.RemoveFreeNotification(Self);
        if Value <> nil then
            Value.FreeNotification(Self);
        FIcsLogger := Value;
    end;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSocketFamily(const Value: TSocketFamily);
begin
    if (FState <> wsClosed) or (FDnsLookupHandle <> 0) then begin
        RaiseException('Cannot change SocketFamily when not closed or in DNS lookup');
        Exit;
    end;

    if Value <> FSocketFamily then begin
        if Value <> sfIPv4 then begin
            try
                if not IsIPv6APIAvailable then
                    raise ESocketException.Create(
                     'SetSocketFamily: New API requires winsock 2.2 ' +
                     'and Windows XP, property "SocketFamily" reset to "sfIPv4"');
            except
                FSocketFamily := sfIPv4;
                FOldSocketFamily := FSocketFamily;
                Exit;
            end;
        end;
        FSocketFamily := Value;
        FOldSocketFamily := FSocketFamily;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSocketRcvBufSize(BufSize : Integer);
var
    iStatus : Integer;
    optlen  : Integer;
begin
    optlen  := SizeOf(BufSize);
    iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF,
                                  PAnsiChar(@BufSize), optlen);
    if iStatus = 0 then
        FSocketRcvBufSize := BufSize
    else
        SocketError('setsockopt(SO_RCVBUF)');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSocketSndBufSize(BufSize : Integer);
var
    iStatus : Integer;
    optlen  : Integer;
begin
    optlen  := SizeOf(BufSize);
    iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF,
                                  PAnsiChar(@BufSize), optlen);
    if iStatus = 0 then
        FSocketSndBufSize := BufSize
    else
        SocketError('setsockopt(SO_SNDBUF)');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketSocksErrorDesc(ErrCode : Integer) : String;
begin
    case ErrCode of
        socksNoError              : Result := 'No Error';
        socksProtocolError        : Result := 'Protocol Error';
        socksVersionError         : Result := 'Version Error';
        socksAuthMethodError      : Result := 'Authentication Method Error';
        socksGeneralFailure       : Result := 'General Failure';
        socksConnectionNotAllowed : Result := 'Connection Not Allowed';
        socksNetworkUnreachable   : Result := 'Network Unreachable';
        socksHostUnreachable      : Result := 'Host Unreachable';
        socksConnectionRefused    : Result := 'Connection Refused';
        socksTtlExpired           : Result := 'TTL Expired';
        socksUnknownCommand       : Result := 'Unknown Command';
        socksUnknownAddressType   : Result := 'Unknown Address Type';
        socksUnassignedError      : Result := 'Unassigned Error';
        socksInternalError        : Result := 'Internal Error';
        socksDataReceiveError     : Result := 'Data Receive Error';
        socksAuthenticationFailed : Result := 'Authentication Failed';
        socksRejectedOrFailed     : Result := 'Rejected Or Failed';
        socksHostResolutionFailed : Result := 'Host Resolution Failed';
        else
            Result := 'Not a SOCKS error';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketHttpStatusCodeDesc(HttpStatusCode : Integer) : String;
const
    sHttpStatusCode = 'HTTP status code';
begin
    { Rather lengthy, I know, anyway RAM is cheap today. }
    if (HttpStatusCode >= 100) and (HttpStatusCode < 600) then
    begin
        case HttpStatusCode of
            100       : Result := 'Continue';
            101       : Result := 'Switching Protocols';
            102       : Result := 'Processing';

            200       : Result := 'OK';
            201       : Result := 'Created';
            202       : Result := 'Accepted';
            203       : Result := 'Non-Authoritative Information';
            204       : Result := 'No Content';
            205       : Result := 'Reset Content';
            206       : Result := 'Partial Content';
            207       : Result := 'Multi-Status (WebDAV)';

            300       : Result := 'Multiple Choices';
            301       : Result := 'Moved Permanently';
            302       : Result := 'Found';
            303       : Result := 'See Other';
            304       : Result := 'Not Modified';
            305       : Result := 'Use Proxy';
            306       : Result := 'Switch Proxy';
            307       : Result := 'Temporary Redirect';

            400       : Result := 'Bad Request';
            401       : Result := 'Unauthorized';
            402       : Result := 'Payment Required';
            403       : Result := 'Forbidden';
            404       : Result := 'Not Found';
            405       : Result := 'Method Not Allowed';
            406       : Result := 'Not Acceptable';
            407       : Result := 'Proxy Authentication Required';
            408       : Result := 'Request Timeout';
            409       : Result := 'Conflict';
            410       : Result := 'Gone';
            411       : Result := 'Length Required';
            412       : Result := 'Precondition Failed';
            413       : Result := 'Request Entity Too Large';
            414       : Result := 'Request-URI Too Long';
            415       : Result := 'Unsupported Media Type';
            416       : Result := 'Requested Range Not Satisfiable';
            417       : Result := 'Expectation Failed';
            418       : Result := 'I''m a teapot';
            422       : Result := 'Unprocessable Entity (WebDAV)';
            423       : Result := 'Locked (WebDAV)';
            424       : Result := 'Failed Dependency (WebDAV)';
            425       : Result := 'Unordered Collection';
            444       : Result := 'No Response';
            426       : Result := 'Upgrade Required';
            449       : Result := 'Retry With';
            450       : Result := 'Blocked by Windows Parental Controls';
            499       : Result := 'Client Closed Request';

            500       : Result := 'Internal Server Error';
            501       : Result := 'Not Implemented';
            502       : Result := 'Bad Gateway';
            503       : Result := 'Service Unavailable';
            504       : Result := 'Gateway Timeout';
            505       : Result := 'HTTP Version Not Supported';
            506       : Result := 'Variant Also Negotiates';
            507       : Result := 'Insufficient Storage (WebDAV)';
            509       : Result := 'Bandwidth Limit Exceeded';
            510       : Result := 'Not Extended';
            else
                Result := sHttpStatusCode + ' ' + IntToStr(HttpStatusCode);
        end;
    end
    else
        Result := 'Not a ' + sHttpStatusCode;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
  sHttpVersionError = 'Proxy server must support HTTP/1.1';

function WSocketHttpTunnelErrorDesc(ErrCode : Integer) : String;
const
    sNotAHttpTunnelError = 'Not a HTTP tunnel error';
var
    LErr : Integer;
begin
    if (ErrCode >= ICS_HTTP_TUNNEL_BASEERR) and
       (ErrCode <= ICS_HTTP_TUNNEL_MAXERR) then
    begin
        LErr := ErrCode - ICS_HTTP_TUNNEL_BASEERR;
        if (LErr >= 100) and (LErr < 600) then
        begin
            if LErr = 200 then
                Result := 'No Error'
            else
                Result := WSocketHttpStatusCodeDesc(LErr);
        end
        else begin
            case ErrCode of
                ICS_HTTP_TUNNEL_PROTERR :
                    Result := 'Protocol Error';
                ICS_HTTP_TUNNEL_GENERR  :
                    Result := 'General Failure';
                ICS_HTTP_TUNNEL_VERSIONERR :
                    Result := sHttpVersionError;
                else
                    Result := sNotAHttpTunnelError;
            end;
        end;
    end
    else
        Result := sNotAHttpTunnelError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketProxyErrorDesc(ErrCode : Integer) : String;
begin
    if (ErrCode >= ICS_HTTP_TUNNEL_BASEERR) and (ErrCode <= ICS_HTTP_TUNNEL_MAXERR) then
        Result := 'HTTP Proxy - ' + WSocketHttpTunnelErrorDesc(ErrCode)
    else if (ErrCode >= ICS_SOCKS_BASEERR) and ((ErrCode <= ICS_SOCKS_MAXERR)) then
        Result := 'SOCKS - ' + WSocketSocksErrorDesc(ErrCode)
    else
        Result := 'Not a proxy error';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketIsProxyErrorCode(ErrCode: Integer): Boolean;
begin
    Result := ((ErrCode >= ICS_SOCKS_BASEERR) and
               (ErrCode <= ICS_SOCKS_MAXERR)) or
              ((ErrCode >= ICS_HTTP_TUNNEL_BASEERR) and
               (ErrCode <= ICS_HTTP_TUNNEL_MAXERR));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketErrorMsgFromErrorCode(ErrCode: Integer) : String;
begin
    if WSocketIsProxyErrorCode(ErrCode) then
        Result := WSocketProxyErrorDesc(ErrCode)
    else
        Result := {$IFDEF MSWINDOWS} 'Winsock - ' {$ELSE} 'Socket - ' {$ENDIF} +
                  WSocketErrorDesc(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketGetErrorMsgFromErrorCode(ErrCode : Integer) : String;
begin
    Result := WSocketErrorMsgFromErrorCode(ErrCode) + ' (#' + IntToStr(ErrCode) + ')';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSin(const Value: TSockAddrIn);
begin
    PSockAddrIn(@Fsin)^ := Value;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetSin: TSockAddrIn;
begin
    Result := PSockAddrIn(@Fsin)^;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetCurrentSocketFamily: TSocketFamily;
var
    saddr    : TSockAddrIn6;
    saddrlen : Integer;
begin
    if FCurrentAddrFamily <> AF_UNSPEC then
    begin
        if FCurrentAddrFamily = AF_INET6 then
            Result := sfIPv6
        else
            Result := sfIPv4;
    end
    else if FState in [wsConnected, wsBound, wsListening] then
    begin
        Result := FSocketFamily; // Dummy
        saddrlen := sizeof(saddr);
        if WSocket_Synchronized_GetSockName(FHSocket, PSockAddrIn(@saddr)^,
                                            saddrlen) = 0 then
        begin
            FCurrentAddrFamily := saddr.sin6_family;
            if FCurrentAddrFamily = AF_INET then
                Result := sfIPv4
            else if FCurrentAddrFamily = AF_INET6 then
                Result := sfIPv6
            else
                raise ESocketException.Create('Unknown socket family');
        end
        else
            SocketError('GetSockName');
    end
    else
        Result := FSocketFamily;
end;


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
  {$IFDEF MSWINDOWS}
    WSASYSNOTREADY:
      Result := 'Network sub-system is unusable';
    WSAVERNOTSUPPORTED:
      Result := 'WinSock DLL cannot support this application';
    WSANOTINITIALISED:
      Result := 'WinSock not initialized';
  {$ENDIF}
    WSAHOST_NOT_FOUND:
      Result := 'Host not found';
    WSATRY_AGAIN:
      Result := 'Non-authoritative host not found';
    WSANO_RECOVERY:
      Result := 'Non-recoverable error';
    WSANO_DATA:
      Result := 'No Data';
    WSASERVICE_NOT_FOUND:
      Result := 'Service not found'; // Name resolution
    else
  {$IFDEF MSWINDOWS}
      Result := 'Not a Winsocket error';
  {$ELSE}
      Result := 'Not a socket error';
  {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketErrorDesc(ErrCode : Integer) : String;
begin
    Result := SocketErrorDesc(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetWinsockErr(ErrCode: Integer): String ;    { V5.26 }
begin
    Result := SocketErrorDesc(ErrCode) + ' (#' + IntToStr(ErrCode) + ')' ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetWindowsErr(ErrCode: Integer): String ;    { V5.26 }
    {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    Result := SysErrorMessage(ErrCode) + ' (#' + IntToStr(ErrCode) + ')' ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

         X X        X X        X X       X      X      X X      X X X X
       X     X    X     X    X     X     X     X     X     X    X
       X          X     X    X           X   X       X          X
         X X      X     X    X           X X           X X        X X
             X    X     X    X           X   X             X          X
       X     X    X     X    X     X     X     X     X     X    X     X
         X X        X X        X X       X      X      X  X       X X

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomSocksWSocket.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FSocksUsercode := '';
    FSocksPassword := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.AssignDefaultValue;
begin
    inherited AssignDefaultValue;
    FSocksState          := socksData;
    FSocksServer         := '';
    FSocksPort           := '';
    FSocksLevel          := '5';
    FSocksRcvdCnt        := 0;
    FSocksPortAssigned   := FALSE;
    FSocksServerAssigned := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SetSocksLevel(newValue : String);
begin
    if State <> wsClosed then begin
        RaiseException('Can''t change socks level if not closed');
        Exit;
    end;
    if (newValue <> '4')  and (newValue <> '5') and
       (newValue <> '4A') and (newValue <> '4a') then begin
        RaiseException('Invalid socks level. Must be 4, 4A or 5.');
        Exit;
    end;
    FSocksLevel := IcsUpperCase(newValue);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.GetSocksPort: String;
begin
    Result := FSocksPort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SetSocksPort(sPort : String);
begin
    if State <> wsClosed then begin
        RaiseException('Can''t change socks port if not closed');
        Exit;
    end;

    FSocksPort := IcsTrim(sPort);

    if Length(FSocksPort) = 0 then begin
        FSocksPortAssigned := FALSE;
        Exit;
    end;
    FSocksPortAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.GetSocksServer: String;
begin
    Result := FSocksServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SetSocksServer(sServer : String);
begin
    if State <> wsClosed then begin
        RaiseException('Can''t change socks server if not closed');
        Exit;
    end;

    FSocksServer := IcsTrim(sServer);
    FSocksServerAssigned := Length(FSocksServer) > 0;

    if FHttpTunnelServerAssigned and FSocksServerAssigned then
    begin
        FSocksServer         := '';
        FSocksServerAssigned := FALSE;
        raise Exception.Create('Can''t use Socks when HTTP proxy is used as well');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.Listen;
begin
    { Check if we really wants to use socks server }
    if not FSocksServerAssigned then begin
        { No socks server assigned, Listen as usual }
        inherited Listen;
        Exit;
    end;
    RaiseException('Listening is not supported thru socks server');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.Connect;
begin
    { Check if we really wants to use socks server }
    if not FSocksServerAssigned then begin
        { No socks server assigned, connect as usual }
        inherited Connect;
        Exit;
    end;

    if (IcsLowerCase(FProtoStr) <> 'tcp') and (IcsTrim(FProtoStr) <> '6') then begin
        RaiseException('TCP is the only protocol supported thru socks server'); { V5.26 }
        Exit;
    end;

    try
        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            Fsin.sin6_port := WSocket_Synchronized_htons(
                              WSocket_Synchronized_ResolvePort(
                              AnsiString(FSocksPort), AnsiString(FProtoStr)));
            FPortResolved  := TRUE;
        end;

        if not FAddrResolved then begin
            { The next line will trigger an exception in case of failure }
            if FSocketFamily = sfIPv4 then
            begin
                Fsin.sin6_family := AF_INET;
                PSockAddrIn(@Fsin).sin_addr.s_addr :=
                WSocket_Synchronized_ResolveHost(AnsiString(FSocksServer)).s_addr;
            end
            else begin
                WSocket_Synchronized_ResolveHost(FSocksServer, Fsin, FSocketFamily, IPPROTO_TCP);
                if (Fsin.sin6_family <> AF_INET) and (FSocksLevel[1] <> '5') then
                    raise ESocketException.Create('IPv6 not supported with current socks version');
            end;
            FAddrResolved := TRUE;
        end;
        { The next line will trigger an exception in case of failure }
        FPortNum := WSocket_Synchronized_ResolvePort(AnsiString(FPortStr), AnsiString(FProtoStr));
    except
        on E:Exception do begin
            RaiseException('Connect: ' + E.Message);  { V5.26 }
            Exit;
        end;
    end;

    FSocksState := socksNegociateMethods;
    FRcvCnt     := 0;
    inherited Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{function BufToStr(Buf : PChar; Cnt : Integer) : String;
begin
    Result := '';
    while Cnt > 0 do begin
        if Buf^ in [#32..#126] then
            Result := Result + Buf^
        else
            Result := Result + '#' + Format('%2.2d', [ord(Buf^)]);
        Inc(Buf);
        Dec(Cnt);
    end;
end;}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSessionConnectedSpecial(Error : Word);
var
    Buf : array [0..2] of AnsiChar;
begin
    if FSocksState = socksNegociateMethods then begin
        {ChangeState(wsSocksConnected);}
        TriggerSocksConnected(Error);
        if Error <> 0 then begin
            FSocksState := socksData;
            inherited TriggerSessionConnectedSpecial(Error);
            Exit;
        end;
        if FSocksLevel[1] = '4' then
            SocksDoConnect
        else begin
            if FSocksAuthentication = socksNoAuthentication then
                FSocksAuthNumber := #$00        { No authentification }
            else
                FSocksAuthNumber := #$02;       { Usercode/Password   }
            Buf[0] := #$05;                     { Version number      }
            Buf[1] := #$01;                     { Number of methods   }
            Buf[2] := FSocksAuthNumber;         { Method identifier   }
            Send(@Buf, 3);

        end;
    end
    else
        inherited TriggerSessionConnectedSpecial(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSessionClosed(Error : Word);
begin
    if FSocksState = socksAuthenticate then
        TriggerSocksAuthState(socksAuthFailure);
    if FSocksState <> socksData then
        DataAvailableError(socksGeneralFailure,
                           WSocketErrorMsgFromErrorCode(socksGeneralFailure));
    inherited TriggerSessionClosed(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSocksConnected(Error : Word);
begin
    if Assigned(FOnSocksConnected) then
        FOnSocksConnected(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSocksError(Error : Integer; Msg : String);
begin
    if Assigned(FOnSocksError) then
        FOnSocksError(Self, Error, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSocksAuthState(AuthState : TSocksAuthState);
begin
    if Assigned(FOnSocksAuthState) then
        FOnSocksAuthState(Self, AuthState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Rfc1929  Username/Password Autentication protocol.
The UNAME field contains the username as known to the source operating system.
The PLEN field contains the length of the PASSWD field that follows.
The PASSWD field contains the password association with the given UNAME.

Rfc1929 does not mention anything about character sets allowed so currently
the Win32 code below converts the user name and password to ANSI using the
default system code page.                                                   }

procedure TCustomSocksWSocket.SocksDoAuthenticate;
var
    Buf     : array [0..127] of AnsiChar;
    I       : Integer;
    TempS   : AnsiString;
begin
    FSocksState := socksAuthenticate;
    TriggerSocksAuthState(socksAuthStart);
    Buf[0] := #$01; {06/03/99}           { Socks version }
    I      := 1;
    TempS  := AnsiString(FSocksUsercode);
    Buf[I] := AnsiChar(Length(TempS));
    Move(TempS[1], Buf[I + 1], Length(TempS));
    I := I + 1 + Length(TempS);

    TempS  := AnsiString(FSocksPassword);
    Buf[I] := AnsiChar(Length(TempS));
    Move(TempS[1], Buf[I + 1], Length(TempS));
    I := I + 1 + Length(TempS);
    try
{TriggerDisplay('Send = ''' + BufToStr(Buf, I) + '''');}
        Send(@Buf, I);
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SocksDoConnect;
type
    pu_long = ^u_long;
var
    Buf     : array [0..127] of AnsiChar;
    I       : Integer;
    ErrCode : Integer;
{$IFDEF COMPILER12_UP}
    S : AnsiString;
{$ENDIF}
begin
    FSocksState := socksConnect;
    if FSocksLevel[1] = '4' then begin
        Buf[0] := #4;                                 { Version number  }
        Buf[1] := #1;                                 { Connect command }
        PWORD(@Buf[2])^  := WSocket_Synchronized_ntohs(FPortNum);  { Dest port       }
        if FSocksLevel = '4A' then
            { Conventional IP saying we can't convert the destination   }
            { host's domain name to find its IP address                 }
            { The destination must then follow the user ID              }
            pu_long(@Buf[4])^ := WSocket_Synchronized_inet_addr('0.0.0.1')
        else begin
            { With original SOCKS4, we have to supply the dest address  }
            try
                pu_long(@Buf[4])^ := WSocket_Synchronized_ResolveHost(AnsiString(FAddrStr)).s_addr;
            except
                on E:Exception do begin
                     ErrCode := socksHostResolutionFailed;
                     TriggerSocksError(ErrCode, E.ClassName + ' ' + E.Message);
                     InternalClose(TRUE, ErrCode);
                     Exit;
                end;
            end;
        end;
        I := 8;
        if Length(FSocksUsercode) > 0 then begin
            { I'm not sure it has to be like that ! Should I also use the }
            { password or not ?                                           }
        {$IFDEF COMPILER12_UP}
            S := AnsiString(FSocksUsercode);
            if Length(S) > 0 then begin
                Move(Pointer(S)^, Buf[I], Length(S));
                I := I + Length(S);
            end;
        {$ELSE}
            Move(FSocksUsercode[1], Buf[I], Length(FSocksUsercode));
            I := I + Length(FSocksUsercode);
        {$ENDIF}
        end;
        Buf[I] := #0;
        Inc(I);
        if FSocksLevel = '4A' then begin
            { We have to supply the destination host name                 }
            Move(AnsiString(FAddrStr)[1], Buf[I], Length(FAddrStr));  // No length change expected (ASCII)
            I := I + Length(FAddrStr);
            Buf[I] := #0;  { Alon Gingold }
            Inc(I);        { Alon Gingold }
        end;
        { Buf[I] := #0;      Alon Gingold }
        { Inc(I);            Alon Gingold }
    end
    else begin
        Buf[0] := #$05;            { Socks version }
        Buf[1] := #$01;            { Connect command }
        Buf[2] := #$00;            { Reserved, must be $00 }
        Buf[3] := #$03;            { Address type is domain name }
        Buf[4] := AnsiChar((Length(FAddrStr)));
        { Should check buffer overflow }
        Move(AnsiString(FAddrStr)[1], Buf[5], Length(FAddrStr)); // No length change expected (ASCII)
        I := 5 + Length(FAddrStr);
        PWord(@Buf[I])^ := WSocket_Synchronized_htons(FPortNum);
        I := I + 2;
    end;

    try
{TriggerDisplay('Send = ''' + BufToStr(Buf, I + 2) + '''');}
        Send(@Buf, I);
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.DataAvailableError(
    ErrCode : Integer;
    Msg     : String);
begin
{   TriggerSocksError(ErrCode, Msg); }
{   inherited TriggerSessionConnectedSpecial(ErrCode); }
{   InternalClose(TRUE, ErrCode); }
    TriggerSocksError(ErrCode, Msg);
    FSocksState := socksData;
    {**ALON** Added, so TriggerSessionConnectedSpecial will only call inherited}
    {inherited} TriggerSessionConnectedSpecial(ErrCode);
    {**ALON** removed 'inherited' now calls top level}
    InternalClose(TRUE, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.TriggerDataAvailable(Error : Word) : Boolean;
var
    Len     : Integer;
    I       : Integer;
    ErrCode : Word;
    ErrMsg  : String;
    InAddr  : TInAddr;
    AnsLen  : Integer;
begin
    if FSocksState = socksData then begin
        Result := inherited TriggerDataAvailable(Error);
        Exit;
    end;

    if Error <> 0 then begin
        DataAvailableError(Error, 'data receive error');
        Result := FALSE;
        Exit;
    end;


    if FSocksState = socksNegociateMethods then begin
        Result := TRUE;
        Len := Receive(@FRcvBuf[FRcvCnt], Sizeof(FRcvBuf) - FRcvCnt - 1);
        if Len < 0 then
            Exit;
        FRcvCnt := FRcvCnt + Len;

{TriggerDisplay('socksNegociateMethods FrcvBuf = ''' + BufToStr(FRcvBuf, FRcvCnt) + '''');}
        if FSocksLevel[1] = '4' then begin
            { We should never comes here }
            DataAvailableError(socksProtocolError, 'TWSocket logic error');
            Exit;
        end
        else begin  { SOCKS5 }
            { We are waiting only two bytes }
            if FRcvCnt < 2 then
                Exit;
{            if FRcvCnt <> 2 then begin  06/03/99}
{                DataAvailableError(socksProtocolError, 'too much data availaible');}
{                Exit;                                                              }
{            end;                                                                   }
            FRcvCnt := 0; { Clear receive counter }
            if FRcvBuf[0] <> $05 then begin
                DataAvailableError(socksVersionError, 'socks version error');
                Exit;
            end;
            if FRcvBuf[1] = $00 then begin
                { No authentication required }
                if FSocksAuthNumber <> #$00 then
                    { We asked for authentification, so complains... }
                    TriggerSocksAuthState(socksAuthNotRequired);
            end
            else if FRcvBuf[1] = $02 then begin
                { Usercode/Password authentication required }
                SocksDoAuthenticate;
                Exit;
            end
            else begin
                DataAvailableError(socksAuthMethodError, 'authentification method not acceptable');
                Exit;
            end;
            SocksDoConnect;
        end;
    end
    else if FSocksState = socksConnect then begin
        Result := TRUE;
{TriggerDisplay('socksConnect FrcvBuf = ''' + BufToStr(FRcvBuf, FRcvCnt) + '''');}
        if FSocksLevel[1] = '4' then begin
            { We want at most 8 characters }
            Len := Receive(@FRcvBuf[FRcvCnt], 8 - FRcvCnt);
            if Len < 0 then
                Exit;
            FRcvCnt := FRcvCnt + Len;

            { We are waiting for 8 bytes }
            if FRcvCnt < 8 then
                Exit;
            FRcvCnt := 0; { Clear receive counter }
            if FRcvBuf[0] <> 0 then begin
                DataAvailableError(socksVersionError, 'socks version error');
                Exit;
            end;
            if FRcvBuf[1] <> 90 then begin  { david.brock }
                case FRcvBuf[1] of
                91: ErrCode := socksRejectedOrFailed;
                92: ErrCode := socksConnectionRefused;
                93: ErrCode := socksAuthenticationFailed;
                else
                   ErrCode := socksUnassignedError;
                end;
                case ErrCode of
                socksRejectedOrFailed :
                    ErrMsg := 'request rejected or failed';
                socksConnectionRefused :
                    ErrMsg := 'connection refused';
                socksAuthenticationFailed :
                    ErrMsg := 'authentification failed';
                else
                    ErrMsg := 'unassigned error #' + IntToStr(Ord(FRcvBuf[1]));
                end;
                DataAvailableError(ErrCode, ErrMsg);
                Exit;
            end;
            FSocksState := socksData;
{           inherited TriggerSessionConnectedSpecial(0); }
{           Result := inherited TriggerDataAvailable(0); }
            {inherited} TriggerSessionConnectedSpecial(0);
            {**ALON** removed 'inherited' now calls top level}
            Result := {inherited} TriggerDataAvailable(0);
            {**ALON** removed 'inherited' now calls top level}
        end
        else begin { SOCKS5 }
            Len := Receive(@FRcvBuf[FRcvCnt], Sizeof(FRcvBuf) - FRcvCnt - 1);
            if Len < 0 then
                Exit;
            FRcvCnt := FRcvCnt + Len;
            if FRcvCnt >= 1 then begin
                { First byte is version, we expect version 5 }
                if FRcvBuf[0] <> $05 then begin
                    DataAvailableError(socksVersionError, 'socks version error');
                    Exit;
                end;
            end;
            if FRcvCnt >= 2 then begin
                if FRcvBuf[1] <> $00 then begin
                    case FRcvBuf[1] of
                    1: ErrCode := socksGeneralFailure;
                    2: ErrCode := socksConnectionNotAllowed;
                    3: ErrCode := socksNetworkUnreachable;
                    4: ErrCode := socksHostUnreachable;
                    5: ErrCode := socksConnectionRefused;
                    6: ErrCode := socksTtlExpired;
                    7: ErrCode := socksUnknownCommand;
                    8: ErrCode := socksUnknownAddressType;
                    else
                       ErrCode := socksUnassignedError;
                    end;
                    case ErrCode of
                    socksGeneralFailure :
                        ErrMsg := 'general SOCKS server failure';
                    socksConnectionNotAllowed :
                        ErrMsg := 'connection not allowed by ruleset';
                    socksNetworkUnreachable :
                        ErrMsg := 'network unreachable';
                    socksHostUnreachable :
                        ErrMsg := 'host unreachable';
                    socksConnectionRefused :
                        ErrMsg := 'connection refused';
                    socksTtlExpired :
                        ErrMsg := 'time to live expired';
                    socksUnknownCommand :
                        ErrMsg := 'command not supported';
                    socksUnknownAddressType :
                        ErrMsg := 'address type not supported';
                    else
                        ErrMsg := 'unassigned error #' + IntToStr(Ord(FRcvBuf[1]));
                    end;
                    DataAvailableError(ErrCode, ErrMsg);
                    Exit;
                end;
            end;
            if FRcvCnt < 5 then
                Exit;

            { We have enough data to learn the answer length }
            if FRcvBuf[3] = $01 then
                AnsLen := 10                     { IP V4 address }
            else if FRcvBuf[3] = $03 then
                AnsLen := 7 + Ord(FRcvBuf[4])    { Domain name   }
            else
                AnsLen := 5;                     { Other unsupported }

            if FRcvCnt < AnsLen then
                Exit;

            if FRcvBuf[3] = $01 then begin
                { IP V4 address }
                //Move(FRcvBuf[4], InAddr, 4);
                InAddr.S_addr := FRcvBuf[4] or
                                 (FRcvBuf[5] shl 8) or
                                 (FRcvBuf[6] shl 16) or
                                 (FRcvBuf[7] shl 24);
                FBoundAddr := WSocket_Synchronized_inet_ntoa(InAddr);
                I := 4 + 4;
            end
            else if FRcvBuf[3] = $03 then begin
                { Domain name }
                SetLength(FBoundAddr, Ord(FRcvBuf[4]));
                Move(FRcvBuf[5], FBoundAddr[1], Length(FBoundAddr)); { david.brock }
                I := 4 + Ord(FRcvBuf[4]) + 1;
            end
            else begin
                { Unsupported address type }
                DataAvailableError(socksUnknownAddressType, 'address type not supported');
                Exit;
            end;

            FBoundPort  := IcsIntToStrA(WSocket_Synchronized_ntohs(
                                        FRcvBuf[I] or (FRcvBuf[I + 1] shl 8)));
            I           := I + 2;
            FSocksState := socksData;
{           inherited TriggerSessionConnectedSpecial(0); }
{ if IsConsole then WriteLn('SOCKS5 NEGOCIATION OK');}
            {inherited} TriggerSessionConnectedSpecial(0);
            {**ALON** removed 'inherited' now calls top level}
            FSocksRcvdCnt := FRcvCnt - I;
            if FSocksRcvdCnt < 0 then
                FSocksRcvdCnt := 0
            else
                FSocksRcvdPtr := I; //@FRcvBuf[I];
{           Result := inherited TriggerDataAvailable(0);}
            Result := {inherited} TriggerDataAvailable(0);
            {**ALON** removed 'inherited' now calls top level}
        end;
    end
    else if FSocksState = socksAuthenticate then begin
        Result := TRUE;
        Len := Receive(@FRcvBuf[FRcvCnt], Sizeof(FRcvBuf) - FRcvCnt - 1);
        if Len < 0 then
            Exit;
        FRcvCnt := FRcvCnt + Len;
        { We expect 2 bytes }
        if FRcvCnt < 2 then  {AG 15/02/11}
            Exit;            {AG 15/02/11}
{TriggerDisplay('socksAuthenticate FrcvBuf = ''' + BufToStr(FRcvBuf, FRcvCnt) + '''');}
        //if FRcvCnt >= 1 then begin
            { First byte is version of the subnegotiation proto (rfc1929), }
            { we expect version 1. However i.e. WinGate returns socks      }
            { version 5 here if user credentials are wrong, so don't       }
            { trigger a version error but socksAuthenticationFailed. AG 15/02/11 }
            if FRcvBuf[0] <> $01 then begin { 06/03/99 }
{                TriggerSocksAuthState(socksAuthFailure); Burlakov 12/11/99 }
{                DataAvailableError(socksVersionError, 'socks version error'); AG 15/02/11 }
                DataAvailableError(socksAuthenticationFailed, 'socks authentication failed'); {AG 15/02/11}
                Exit;
            end;
        //end;
        if FRcvCnt = 2 then begin
            { Second byte is status }
            if FRcvBuf[1] <> $00 then begin
{                TriggerSocksAuthState(socksAuthFailure); Burlakov 12/11/99 }
                DataAvailableError(socksAuthenticationFailed, 'socks authentication failed');
                Exit;
            end;
        end
        else if FRcvCnt > 2 then begin
{            TriggerSocksAuthState(socksAuthFailure); Burlakov 12/11/99 }
            DataAvailableError(socksProtocolError, 'too much data availaible');
            Exit;
        end;
        FRcvCnt := 0; { 06/03/99 }
        TriggerSocksAuthState(socksAuthSuccess);
        SocksDoConnect;
    end
    else begin
        { We should never comes here ! }
        DataAvailableError(socksInternalError, 'internal error');
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.GetRcvdCount : LongInt;
begin
    if FSocksRcvdCnt <= 0 then
        Result := inherited GetRcvdCount
    else
        Result := FSocksRcvdCnt;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.DoRecv(
    var Buffer : TWSocketData;
    BufferSize : Integer;
    Flags      : Integer) : Integer;
begin
    if FSocksRcvdCnt <= 0 then begin
        Result := inherited DoRecv(Buffer, BufferSize, Flags);
        Exit;
    end;
    { We already have received data into our internal buffer }
    if FSocksRcvdCnt <= BufferSize then begin
        { User buffer is greater than received data, copy all and clear }
        Move(FRcvBuf[FSocksRcvdPtr], Buffer^, FSocksRcvdCnt); { V7.33 }
        Result        := FSocksRcvdCnt;
        FSocksRcvdCnt := 0;
        Exit;
    end;
    { User buffer is smaller, copy as much as possible }
    Move(FRcvBuf[FSocksRcvdPtr], Buffer^, BufferSize); { V7.33 }
    Result        := BufferSize;
    FSocksRcvdPtr := FSocksRcvdPtr + BufferSize;
    FSocksRcvdCnt := FSocksRcvdCnt - BufferSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

              X          X     X       X      X X X X
              X          X     X X     X      X
              X          X     X   X   X      X
              X          X     X     X X      X X X
              X          X     X       X      X
              X          X     X       X      X
              X X X X    X     X       X      X X X X

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomLineWSocket.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FLineEnd   := #13#10;
    FLineMode  := FALSE;
    FLineEdit  := FALSE;  { AG 2/12/07}
    FLineLimit := 65536;  { Arbitrary line limit }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomLineWSocket.Destroy;
begin
    if FRcvdPtr <> nil then begin
        FreeMem(FRcvdPtr, FRcvBufSize);
        FRcvdPtr     := nil;
        FRcvBufSize := 0;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.WndProc(var MsgRec: TMessage);
begin
    with MsgRec do begin
        if Msg = FMsg_WM_TRIGGER_DATA_AVAILABLE then begin
            { We *MUST* handle all exception to avoid application shutdown }
            try
                WMTriggerDataAvailable(MsgRec)
            except
                on E:Exception do
                    HandleBackGroundException(E);
            end;
        end
        else
            inherited WndProc(MsgRec);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.WMTriggerDataAvailable(var msg: TMessage);
var
    Count : Integer;
begin
{$IFDEF OLD_20040117}
    while FRcvdCnt > 0 do
        TriggerDataAvailable(0);
{$ELSE}
    Count := 0;
    while FRcvdCnt > 0 do begin
        Inc(Count);
        FLineFound := FALSE;
        TriggerDataAvailable(0);
        if (FRcvdCnt <= 0) or
           (FLineMode and (Count > 3) and (not FLineFound)) then
            Break;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.SetLineMode(newValue : Boolean);
begin
    if FLineMode = newValue then
        Exit;
    FLineMode := newValue;
    if (FRcvdCnt > 0) or (FLineLength > 0) then
        PostMessage(Handle, FMsg_WM_TRIGGER_DATA_AVAILABLE, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
{ Returns -1 on error only if event OnError is assigned, otherwise an       }
{ ESocketException may be raised. Returns the number of bytes written on    }
{ success. LineEnd is treated as a raw sequence of bytes, hence it's not    }
{ converted but sent as is.                                                 }
function TCustomLineWSocket.SendLine(
    const Str : UnicodeString;
    ACodePage : LongWord) : Integer;
begin
    Result := PutStringInSendBuffer(Str, ACodePage);
    if Result > 0 then begin
        if SendStr(LineEnd) > -1 then
            Inc(Result, Length(LineEnd))
        else
            Result := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.SendLine(const Str : UnicodeString) : Integer;
begin
    Result := PutStringInSendBuffer(Str);
    if Result > 0 then begin
        if SendStr(LineEnd) > -1 then
            Inc(Result, Length(LineEnd))
        else
            Result := -1;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Returns -1 on error only if event OnError is assigned, otherwise an       }
{ ESocketException may be raised. Returns the number of bytes written on    }
{ success.                                                                  }
function TCustomLineWSocket.SendLine(const Str : RawByteString) : Integer;
begin
    Result := PutStringInSendBuffer(Str);
    if Result > 0 then begin
        if SendStr(LineEnd) > -1 then
            Inc(Result, Length(LineEnd))
        else
            Result := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.GetRcvdCount : LongInt;
begin
    if not FLineMode then
        Result := inherited GetRcvdCount
    else
        Result := FLineLength;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.DoRecv(
    var Buffer : TWSocketData;
    BufferSize : Integer;
    Flags      : Integer) : Integer;
begin
    if FLineMode and (FLineLength > 0) then begin
        { We are in line mode and a line is received }
        if FLineLength <= BufferSize then begin
            { User buffer is greater than received data, copy all and clear }
            Move(FRcvdPtr^, Buffer^, FLineLength);
            Result      := FLineLength;
            FLineLength := 0;
            Exit;
        end;
        { User buffer is smaller, copy as much as possible }
        Move(FRcvdPtr^, Buffer^, BufferSize);
        { Move the end of line to beginning of buffer to be read the next time }
        Move(PAnsiChar(FRcvdPtr)[BufferSize], FRcvdPtr^, FLineLength - BufferSize);
        Result      := BufferSize;
        FLineLength := FLineLength - BufferSize;
        Exit;
    end;

    if FLineMode or (FRcvdCnt <= 0) then begin
        { There is nothing in our internal buffer }
        Result := inherited DoRecv(Buffer, BufferSize, Flags);
        Exit;
    end;

    { We already have received data into our internal buffer }
    if FRcvdCnt <= BufferSize then begin
        { User buffer is greater than received data, copy all and clear }
        Move(FRcvdPtr^, Buffer^, FRcvdCnt);
        Result   := FRcvdCnt;
        FRcvdCnt := 0;
        Exit;
    end;
    { User buffer is smaller, copy as much as possible }
    Move(FRcvdPtr^, Buffer^, BufferSize);
    { Then move remaining data to front og buffer  16/10/99 }
    Move(PAnsiChar(FRcvdPtr)[BufferSize], FRcvdPtr^, FRcvdCnt - BufferSize + 1);
    Result   := BufferSize;
    FRcvdCnt := FRcvdCnt - BufferSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Edit received data. Handle TAB and BACKSPACE characters.                  }
{ A data packet has been received into FRcvPtr buffer, starting from        }
{ FRcvdCnt offset. Packet size if passed as the Len argument.               }
procedure TCustomLineWSocket.EditLine(var Len : Integer);
var
    Buf     : PAnsiChar;
    BufSize : LongInt;
    I       : LongInt;
    J       : LongInt;
    Edited  : Boolean;
    NewCnt  : LongInt;
    NewSize : LongInt;
const
    BackString : String = #8 + ' ' + #8;
begin
    BufSize := 0;
    try
        Edited := FALSE;
        I      := FRcvdCnt;
        J      := FRcvdCnt;
        NewCnt := FRcvdCnt;
        { Loop to process all received char }
        while I < (FRcvdCnt + Len) do begin
            if PAnsiChar(FRcvdPtr)[I] = #8 then begin   { BACKSPACE character }
                if FLineEcho and (J > 0) then
                    SendStr(BackString);
                if not Edited then begin
                    { Not edited yet, so we allocate a buffer to store }
                    { edited data and we remember we edited data.      }
                    Edited := TRUE;
                    { Computer buffer size as a multiple of 256 bytes  }
                    BufSize := ((FRcvdCnt + Len + 256) shr 8) shl 8;
                    GetMem(Buf, BufSize);
                    { Copy data already processed }
                    Move(FRcvdPtr^, Buf^, I);
                end;
                if J > 0 then begin
                    Dec(J);
                    if J < NewCnt then
                        NewCnt := J;
                end;
                Inc(I);
            end
            else if PAnsiChar(FRcvdPtr)[I] = #9 then begin  { TAB character }
                if not Edited then begin
                    { Not edited yet, so we allocate a buffer to store }
                    { edited data and we remember we edited data.      }
                    Edited := TRUE;
                    { Computer buffer size as a multiple of 256 bytes  }
                    BufSize := ((FRcvdCnt + Len + 256) shr 8) shl 8;
                    GetMem(Buf, BufSize);
                    { Copy data already processed }
                    Move(FRcvdPtr^, Buf^, I);
                end;
                repeat
                    if FLineEcho then
                        SendStr(' ');
                    Buf[J] := ' ';
                    Inc(J);
                until (J and 7) = 0;
                Inc(I);
            end
            else begin
                if FLineEcho then
                    Send(@PAnsiChar(FRcvdPtr)[I], 1);
                if Edited then begin
                    if J >= BufSize then begin
                        { Need to allocate more buffer space }
                        NewSize := BufSize + 256;
                        ReallocMem(Buf, NewSize);
                        BufSize := NewSize;
                    end;
                    Buf[J] := PAnsiChar(FRcvdPtr)[I];
                end;
                Inc(I);
                Inc(J);
            end;
        end;
        if Edited then begin
            if J >= FRcvBufSize then begin
                { Current buffer is too small, allocate larger }
                NewSize := J + 1;
                ReallocMem(FRcvdPtr, NewSize);
                FRcvBufSize := NewSize;
            end;
            { Move edited data back to original buffer }
            Move(Buf^, FRcvdPtr^, J);
            PAnsiChar(FRcvdPtr)[J] := #0;
            FRcvdCnt := NewCnt;
            Len      := J - FRcvdCnt;
        end;
    finally
        if BufSize > 0 then
            FreeMem(Buf, BufSize);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.TriggerLineLimitExceeded(
    Cnt           : Integer;
    var ClearData : Boolean);
begin
    if Assigned(FOnLineLimitExceeded) then
        FOnLineLimitExceeded(Self, Cnt, ClearData);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.TriggerDataAvailable(ErrCode : Word) : Boolean;
var
    Cnt        : Integer;
    Len        : Integer;
    NewSize    : LongInt;
    SearchFrom : LongInt;
    I          : LongInt;
    Found      : Boolean;
begin
{  if (not FLineMode) or (Length(FLineEnd) = 0) then begin }
    if (not FLineMode) or (Length(FLineEnd) = 0) or
       (FSocksState <> socksData) or (FHttpTunnelState <> htsData)
    {**ALON** added check so, if data is received while still handshaking }
    { with the socks server, we ask the TCustomSocksWSocket to handle it  }
    then begin
        { We are not in line mode }
        Result := inherited TriggerDataAvailable(ErrCode);
        Exit;
    end;

    { We are in line mode. We receive data ourself }

    Result := TRUE;
    Cnt    := inherited GetRcvdCount;
    { if Cnt <= 0 then }
    {    Exit;         }
    if Cnt < 0 then
        Exit;
    if Cnt = 0 then
        Cnt := 255;

    if (FRcvdCnt + Cnt + 1) > FRcvBufSize then begin
        { Current buffer is too small, allocate larger }
        NewSize := FRcvdCnt + Cnt + 1;
        ReallocMem(FRcvdPtr, NewSize);
        FRcvBufSize := NewSize;
    end;

    Len := Receive(IncPtr(FRcvdPtr, FRcvdCnt), Cnt);
{$IFDEF OLD_20040117}
    if Len <= 0 then
        Exit;
    FRcvdPtr[FRcvdCnt + Len] := #0;
{$ELSE}
    if Len <= 0 then begin
        if FRcvdCnt <= 0 then
            Exit;
        Len := 0;
    end;
{$ENDIF}

    if Len > 0 then begin
        if FLineEdit then
            EditLine(Len)
        else if FLineEcho then
            Send(IncPtr(FRcvdPtr, FRcvdCnt), Len);
    end;

    SearchFrom := FRcvdCnt - Length(FLineEnd);
    if SearchFrom < 0 then
        SearchFrom := 0;
    FRcvdCnt := FRcvdCnt + Len;
    while FLineMode do begin
        Found := FALSE;
        I := SearchFrom;
        while I < (FRcvdCnt - Length(FLineEnd) + 1) do begin
            if PAnsiChar(FRcvdPtr)[I] = AnsiChar(FLineEnd[1]) then begin
                Found := StrLComp(PAnsiChar(@(PAnsiChar(FRcvdPtr)[I])),
                                  PAnsiChar(FLineEnd), Length(FLineEnd)) = 0;
                if Found then
                    break;    { Found the end of line marker }
            end;
            Inc(I);
        end;
        if not Found then begin
            if ((FLineLimit > 0) and (FRcvdCnt > FLineLimit)) then begin
                FLineClearData := TRUE;
                TriggerLineLimitExceeded(FRcvdCnt, FLineClearData);
                if FLineClearData then begin
                    FLineLength        := 0;
                    FRcvdCnt           := 0;
                    FLineClearData     := FALSE;
                end;
            end;
            break;
        end;
        FLineLength       := I + Length(FLineEnd);
        FLineReceivedFlag := TRUE;
        FLineFound        := TRUE;
        { We received a complete line. We need to signal it to application }
        { The application may not have a large buffer so we may need       }
        { several events to read the entire line. In the meanwhile, the    }
        { application may turn line mode off.                              }
        while FLineMode and (FLineLength > 0) do begin
            if not inherited TriggerDataAvailable(0) then
                { There is no handler installed }
                FLineLength := 0;
        end;
        { Move remaining data in front of buffer }
        if FLineLength > 0 then begin
            { Line mode was turned off in the middle of a line read. }
            { We preserve unread line and other received data.       }
            Move(PAnsiChar(FRcvdPtr)[I], PAnsiChar(FRcvdPtr)[FLineLength],
                 FRcvdCnt - I);
            FRcvdCnt := FRcvdCnt - I + FLineLength;
        end
        else begin
            Move(PAnsiChar(FRcvdPtr)[I + Length(FLineEnd)], PAnsiChar(FRcvdPtr)[0],
                 FRcvdCnt - I - Length(FLineEnd));
            FRcvdCnt := FRcvdCnt - I - Length(FLineEnd);
        end;
        if FRcvdCnt >= 0 then
            PAnsiChar(FRcvdPtr)[FRcvdCnt] := #0;
        SearchFrom := 0;
        { It is possible the user has turned line mode to off. If data is }
        { still available in the receive buffer, we will deliver it.      }
        while (not FLineMode) and (FRcvdCnt > 0) do            { 26/01/04 }
            inherited TriggerDataAvailable(0);                 { 26/01/04 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.InternalAbort(ErrCode : Word);   { V7.49 }
begin
    { Abort as soon as possible, see TriggerSessionClosed below.      }
    FLineClearData := TRUE; { Skip subsequent calls to DataAvailable. }
    inherited InternalAbort(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.TriggerSessionClosed(Error : Word);
begin
    FLineReceivedFlag := TRUE;
    if FRcvdPtr <> nil then begin
        if FLineMode and (FRcvdCnt > 0) and (not FLineClearData) then begin
            FLineLength       := FRcvdCnt;
            while FLineMode and (FLineLength > 0) do
                inherited TriggerDataAvailable(0);
        end;
        FreeMem(FRcvdPtr, FRcvBufSize);
        FRcvdPtr    := nil;
        FRcvBufSize := 0;
        FRcvdCnt    := 0;
    end;
    inherited TriggerSessionClosed(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                 X X      X     X    X       X     X X X
               X     X      X   X    X X     X   X      X
               X              X X    X   X   X   X
                 X X            X    X     X X   X
                     X          X    X       X   X
               X     X    X     X    X       X   X      X
                 X X        X X      X       X     X X X

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSyncWSocket.InternalDataAvailable(
    Sender : TObject;
    Error  : Word);
var
    Len : Integer;
begin
    SetLength(FLinePointer^, FLineLength);
    Len := Receive(@FLinePointer^[1], FLineLength);
    if Len <= 0 then
        FLinePointer^ := ''
    else
        SetLength(FLinePointer^, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSyncWSocket.WaitUntilReady(var DoneFlag : Boolean) : Integer;
begin
    Result := 0;           { Suppose success }
    FTimeStop := Integer(IcsGetTickCount) + FTimeout;
    while not DoneFlag do begin
    {while TRUE do begin                     V7.81
        if DoneFlag then begin
            Result := 0;
            break;
        end;}

        if ((FTimeout > 0) and (Integer(IcsGetTickCount) > FTimeStop)) or
           Terminated then begin
            { Application is terminated or timeout occured }
            Result := WSA_WSOCKET_TIMEOUT;
            break;
        end;
        MessagePump;
{$IFDEF COMPILER2_UP}
        { Do not use 100% CPU, but slow down transfert on high speed LAN }
        Sleep(0);
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ DEPRECATED: DO NOT USE Synchronize procedure for a new application.       }
{ Instead, use pure event-driven design.                                    }
function TCustomSyncWSocket.Synchronize(
    Proc : TWSocketSyncNextProc;
    var DoneFlag : Boolean) : Integer;
begin
    DoneFlag := FALSE;
    if Assigned(Proc) then
        Proc;
    Result := WaitUntilReady(DoneFlag);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ DEPRECATED: DO NOT USE ReadLine procedure for a new application.          }
{ Instead, use pure event-driven design using OnDataAvailable event.        }
procedure TCustomSyncWSocket.ReadLine(
    Timeout    : Integer;  { seconds if positive, milli-seconds if negative }
    var Buffer : AnsiString);
var
    OldDataAvailable : TDataAvailable;
    OldLineMode      : Boolean;
    Status           : Integer;
begin
    Buffer            := '';
    if FState <> wsConnected then begin
        RaiseException('ReadLine failed: not connected');
        Exit;
    end;

    { Positive timeout means seconds. Negative means milli-seconds }
    { Null means 60 seconds.                                       }
    if TimeOut = 0 then
        FTimeOut      := 60000
    else if TimeOut > 0 then
        FTimeOut      := Timeout * 1000
    else
        FTimeOut      := -Timeout;

    FLineReceivedFlag := FALSE;
    FLinePointer      := @Buffer;
    { Save existing OnDataAvailable handler and install our own }
    OldDataAvailable  := FOnDataAvailable;
    FOnDataAvailable  := InternalDataAvailable;
    { Save existing line mode and turn it on }
    OldLineMode       := FLineMode;
    FLineMode         := TRUE;
    try
        Status := Synchronize(nil, FLineReceivedFlag);
        if Status = WSA_WSOCKET_TIMEOUT then begin
             { Sender didn't send line end within allowed time. Get all }
             { data available so far.                                   }
             if FRcvdCnt > 0 then begin
                 SetLength(Buffer, FRcvdCnt);
                 Move(FRcvdPtr^, Buffer[1], FRcvdCnt);
                 FRcvdCnt := 0;
             end;
        end;
        { Should I raise an exception to tell the application that       }
        { some error occured ?                                           }
    finally
        FOnDataAvailable := OldDataAvailable;
        FLineMode        := OldLineMode;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF BUILTIN_TIMEOUT}

{ TCustomTimeoutWSocket }

const
    MIN_TIMEOUT_SAMPLING_INTERVAL = 1000;

constructor TCustomTimeoutWSocket.Create(AOwner: TComponent);
begin
    inherited;
    FTimeoutKeepThreadAlive := TRUE;
    FTimeoutSampling := 5000;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.TimeoutHandleTimer(
    Sender: TObject);
begin
    if (FTimeoutConnect > 0) and (FState <> wsConnected) then begin
        if IcsCalcTickDiff(FTimeoutConnectStartTick,
                           IcsGetTickCount) > FTimeoutConnect then begin
            TimeoutStopSampling;
            TriggerTimeout(torConnect);
        end;
    end
    else if (FTimeoutIdle > 0) then begin
        if IcsCalcTickDiff(FCounter.GetLastAliveTick,
                           IcsGetTickCount) > FTimeoutIdle then begin
            TimeoutStopSampling;
            TriggerTimeout(torIdle);
        end;
    end
    else
        TimeoutStopSampling;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.Connect;
begin
    if FTimeoutConnect > 0 then begin
        TimeoutStartSampling;
        FTimeoutConnectStartTick := IcsGetTickCount;
    end
    else if FTimeoutIdle > 0 then
        TimeoutStartSampling;
    inherited Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.SetTimeoutKeepThreadAlive(const Value: Boolean);
begin
    FTimeoutKeepThreadAlive := Value;
    if FTimeoutTimer <> nil then
        FTimeoutTimer.KeepThreadAlive := FTimeoutKeepThreadAlive;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.SetTimeoutSampling(const Value: LongWord);
begin
    if (Value > 0) and (Value < MIN_TIMEOUT_SAMPLING_INTERVAL) then
       FTimeoutSampling := MIN_TIMEOUT_SAMPLING_INTERVAL
    else
       FTimeoutSampling := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.TimeoutStartSampling;
begin
    if not Assigned(FTimeoutTimer) then begin
        FTimeoutTimer := TIcsThreadTimer.Create(Self);
        FTimeoutTimer.KeepThreadAlive := FTimeoutKeepThreadAlive;
        FTimeoutTimer.OnTimer := TimeoutHandleTimer;
    end;
    if not Assigned(FCounter) then
        CreateCounter
    else
        FCounter.LastSendTick := IcsGetTickCount; // Init
    if FTimeoutTimer.Interval <> FTimeoutSampling then
        FTimeoutTimer.Interval := FTimeoutSampling;
    if not FTimeoutTimer.Enabled then
        FTimeoutTimer.Enabled := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.TimeoutStopSampling;
begin
    if Assigned(FTimeoutTimer) then
        FTimeoutTimer.Enabled := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.DupConnected;
begin
    if FTimeoutIdle > 0 then
        TimeoutStartSampling
    else
        TimeoutStopSampling;
    inherited DupConnected;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.ThreadAttach;
begin
    inherited ThreadAttach;
    if Assigned(FTimeoutTimer) then
        FTimeoutTimer.Enabled := FTimeoutOldTimerEnabled;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.ThreadDetach;
begin
    if Assigned(FTimeoutTimer) and
      (IcsGetCurrentThreadID = FThreadID) then begin
        FTimeoutOldTimerEnabled := FTimeoutTimer.Enabled;
        if FTimeoutOldTimerEnabled then
            FTimeoutTimer.Enabled := FALSE;
    end;
    inherited ThreadDetach;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.TriggerSessionClosed(Error: Word);
begin
    TimeoutStopSampling;
    inherited TriggerSessionClosed(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.TriggerSessionConnectedSpecial(
  Error: Word);
begin
    if (Error = 0) and (FTimeoutIdle > 0) then
        TimeoutStartSampling
    else
        TimeoutStopSampling;
    inherited TriggerSessionConnectedSpecial(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomTimeoutWSocket.TriggerTimeout(Reason: TTimeoutReason);
begin
    if Assigned(FOnTimeout) then
        FOnTimeout(Self, Reason);
end;
{$ENDIF BUILTIN_TIMEOUT}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF BUILTIN_THROTTLE}

{ TCustomThrottledWSocket }

constructor TCustomThrottledWSocket.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FBandwidthKeepThreadAlive := TRUE;
    FBandwidthSampling := 1000; { Msec sampling interval }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.ThreadAttach;
begin
    inherited ThreadAttach;
    if Assigned(FBandwidthTimer) then
        FBandwidthTimer.Enabled := FBandwidthOldTimerEnabled;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.ThreadDetach;
begin
    if Assigned(FBandwidthTimer) and
      (IcsGetCurrentThreadID = FThreadID) then begin
        FBandwidthOldTimerEnabled := FBandwidthTimer.Enabled;
        if FBandwidthOldTimerEnabled then
            FBandwidthTimer.Enabled := FALSE;
    end;
    inherited ThreadDetach;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.DupConnected;
begin
    inherited DupConnected;
    SetBandwidthControl;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.SetBandwidthControl;
var
    I : Int64;
begin
    FBandwidthCount := 0;
    if FBandwidthLimit > 0 then
    begin
        if not Assigned(FBandwidthTimer) then begin
            FBandwidthTimer := TIcsThreadTimer.Create(Self);
            FBandwidthTimer.KeepThreadAlive := FBandwidthKeepThreadAlive;
            FBandwidthTimer.OnTimer := BandwidthHandleTimer;
        end;
        FBandwidthTimer.Interval := FBandwidthSampling;
        if not FBandwidthTimer.Enabled then
            FBandwidthTimer.Enabled := TRUE;
        // Number of bytes we allow during a sampling period, max integer max.
        I := Int64(FBandwidthLimit) * FBandwidthSampling div 1000;
        if I < MaxInt then
            FBandwidthMaxCount := I
        else
            FBandwidthMaxCount := MaxInt;
        FBandwidthPaused := FALSE;
        Include(FComponentOptions, wsoNoReceiveLoop);
        FBandwidthEnabled := TRUE;
    {$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loWsockInfo) then
            DebugLog(loWsockInfo,
                     IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' Bandwidth ON ' + IntToStr(FHSocket));
    {$ENDIF}
    end
    else begin
        if Assigned(FBandwidthTimer) then begin
            if FBandwidthTimer.Enabled then
                FBandwidthTimer.Enabled := FALSE;
            if FBandwidthEnabled then begin
                FBandwidthEnabled := FALSE;
            {$IFNDEF NO_DEBUG_LOG}
                if CheckLogOptions(loWsockInfo) then
                    DebugLog(loWsockInfo,
                            IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                            ' Bandwidth OFF ' + IntToStr(FHSocket));
            {$ENDIF}
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.SetBandwidthKeepThreadAlive(
  const Value: Boolean);
begin
    FBandwidthKeepThreadAlive := Value;
    if FBandwidthTimer <> nil then
        FBandwidthTimer.KeepThreadAlive := FBandwidthKeepThreadAlive;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.SetBandwidthLimit(const Value: LongWord);   { V7.55 }
begin
    if Value <> FBandwidthLimit then begin
        FBandwidthLimit := Value;
        if Assigned(FBandwidthTimer) then SetBandwidthControl;  { only if done this already, to change it }
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.SetBandwidthSampling(const Value: LongWord);
begin
    if FBandwidthSampling <> Value then begin   { V7.55 }
        if Value < 500 then
            FBandwidthSampling := 500
        else
            FBandwidthSampling := Value;
        if Assigned(FBandwidthTimer) then SetBandwidthControl;  { only if done this already, to change it }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomThrottledWSocket.RealSend(var Data: TWSocketData;
  Len: Integer): Integer;
begin
    if not FBandwidthEnabled then
        Result := inherited RealSend(Data, Len)
    else begin
        { Try to adjust amount of data actually passed to winsock }
        if (Len > 0) and (FBandwidthCount < FBandwidthMaxCount) and
           (FBandwidthCount + LongWord(Len) > FBandwidthMaxCount) then
            Len := (FBandwidthMaxCount - FBandwidthCount) + 1;

        Result := inherited RealSend(Data, Len);

        if (Result > 0) then begin
            Inc(FBandwidthCount, Result);
            if (FBandwidthCount > FBandwidthMaxCount) and
               (not FBandwidthPaused) then begin
                FBandwidthPaused := TRUE;
                Pause;
           {$IFNDEF NO_DEBUG_LOG}
                if CheckLogOptions(loWsockInfo) then
                    DebugLog(loWsockInfo,
                            IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                            ' Bandwidth Paused on send ' + IntToStr(FHSocket));
           {$ENDIF}
           end;
       end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomThrottledWSocket.Receive(Buffer: TWSocketData;
  BufferSize: Integer): Integer;
begin
    { The Receive throttle does not work if FD_CLOSE message has been received }
    { yet since handler Do_FD_CLOSE removes option wsoNoReceiveLoop.           }
    if (not FBandwidthEnabled) or not (wsoNoReceiveLoop in ComponentOptions) then
        Result := inherited Receive(Buffer, BufferSize)
    else begin
        { Try to adjust amount of data to be received from winsock }
        if (BufferSize > 0) and (FBandwidthCount < FBandwidthMaxCount) and
           (FBandwidthCount + LongWord(BufferSize) > FBandwidthMaxCount) then
            BufferSize := (FBandwidthMaxCount - FBandwidthCount) + 1;

        Result := inherited Receive(Buffer, BufferSize);

        if (Result > 0) then begin
            Inc(FBandwidthCount, Result);
            if (FBandwidthCount > FBandwidthMaxCount) and
               (not FBandwidthPaused) then begin
                FBandwidthPaused := TRUE;
                Pause;
            {$IFNDEF NO_DEBUG_LOG}
                if CheckLogOptions(loWsockInfo) then
                    DebugLog(loWsockInfo,
                             IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                             ' Bandwidth Paused on receive ' + IntToStr(FHSocket));
            {$ENDIF}
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.BandwidthHandleTimer(
    Sender: TObject);
begin
    if FBandwidthPaused then begin
        FBandwidthPaused := FALSE;
        Dec(FBandwidthCount, FBandwidthMaxCount);
        if FBandwidthCount > FBandwidthMaxCount then
            FBandwidthCount := FBandwidthMaxCount;
        if (FHSocket <> INVALID_SOCKET) then begin
        {$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loWsockInfo) then
                DebugLog(loWsockInfo,
                         IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                         ' Bandwidth Resume ' + IntToStr(FHSocket));
        {$ENDIF}
            Resume;
        end;
    end
    else
        FBandwidthCount := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.TriggerSessionClosed(Error: Word);
begin
    if Assigned(FBandwidthTimer) then begin
        FBandwidthTimer.Enabled := FALSE;
        FBandwidthEnabled       := FALSE;
    end;
    inherited TriggerSessionClosed(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomThrottledWSocket.TriggerSessionConnectedSpecial(Error: Word);
begin
    { Turn on the throttle early, inherited TriggerSessionConnectedSpecial }
    { might already process the first data chunk.                          }
    if (Error = 0) then
        SetBandwidthControl;
    inherited TriggerSessionConnectedSpecial(Error);
    if (Error <> 0) and Assigned(FBandwidthTimer) then begin
        FBandwidthTimer.Enabled := FALSE;
        FBandwidthEnabled       := FALSE;
    end;
end;
{$ENDIF BUILTIN_THROTTLE}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}

var
    //GSslInitialized     : Integer = 0;
    SslRefCount               : Integer = 0;
    GSslRegisterAllCompleted  : Boolean = FALSE;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(* procedure OutputDebugString(const Msg : String);  angus
begin
{$IFDEF DEBUG_OUTPUT}
        WriteLn(LogFile, Msg {+ ' ThreadID: ' + IntToHex(GetCurrentThreadID, 8)});
        Flush(LogFile);
{#$ELSE}
    //WinProcs.OutputDebugString(PChar(Msg));
{$ENDIF}
end;  *)


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
var TraceCount : Integer = 0;
(*procedure OutputTrace(const Msg: String);
begin
    OutputDebugString(Msg);
    if TraceCount = 15 then
         TraceCount := 15;
end; *)


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure LoadSsl;
const
    LenErrMsg = 512;
var
    Tick : Cardinal;
    S    : String;
{$IFDEF LOADSSL_ERROR_FILE} // Optional define in OverbyteIcsSslDefs.inc
    F    : TextFile;
    I, J : Integer;
{$ENDIF}
begin
    SslCritSect.Enter;
    try
        if SslRefCount = 0 then begin
            // Load LIBEAY DLL
            // Must be loaded before SSlEAY for the versioncheck to work!
            if not OverbyteIcsLIBEAY.Load then begin
                S := OverbyteIcsLIBEAY.WhichFailedToLoad;
            {$IFDEF LOADSSL_ERROR_FILE}
                AssignFile(F, _ExtractFilePath(ParamStr(0)) + 'FailedIcsLIBEAY.txt');
                Rewrite(F);
                I := 1;
                while I < Length(S) do begin
                    J := I;
                    while (I <= Length(S)) and (S[I] <> ' ') do
                        Inc(I);
                    Inc(I);
                    WriteLn(F, Copy(S, J, I - J));
                end;
                CloseFile(F);
            {$ENDIF}
                if OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle <> 0 then begin
                    FreeLibrary(OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle);
                    OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle := 0
                end;
                if Length(S) > LenErrMsg then begin
                    SetLength(S, LenErrMsg);
                    S[Length(S) - 1] := '.';
                    S[Length(S)]     := '.';
                end;
                raise EIcsLibeayException.Create('Unable to load LIBEAY DLL. Can''t find ' + S);
            end;
            // Load SSlEAY DLL
            if not OverbyteIcsSSLEAY.Load then begin
                S := OverbyteIcsSSLEAY.WhichFailedToLoad;
            {$IFDEF LOADSSL_ERROR_FILE}
                AssignFile(F, _ExtractFilePath(ParamStr(0)) + 'FailedIcsSSLEAY.txt');
                Rewrite(F);
                I := 1;
                while I < Length(S) do begin
                    J := I;
                    while (I <= Length(S)) and (S[I] <> ' ') do
                        Inc(I);
                    Inc(I);
                    WriteLn(F, Copy(S, J, I - J));
                end;
                CloseFile(F);
            {$ENDIF}
                if OverbyteIcsSSLEAY.GSSLEAY_DLL_Handle <> 0 then begin
                    FreeLibrary(OverbyteIcsSSLEAY.GSSLEAY_DLL_Handle);
                    OverbyteIcsSSLEAY.GSSLEAY_DLL_Handle := 0;
                end;
                if OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle <> 0 then begin
                    FreeLibrary(OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle);
                    OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle := 0
                end;
                if Length(S) > LenErrMsg then begin
                    SetLength(S, LenErrMsg);
                    S[Length(S) - 1] := '.';
                    S[Length(S)]     := '.';
                end;
                raise EIcsSsleayException.Create('Unable to load SSLEAY DLL. Can''t find ' + S);
            end;

            // Global system initialization
            if f_SSL_library_init <> 1 then begin
                if OverbyteIcsSSLEAY.GSSLEAY_DLL_Handle <> 0 then begin
                    FreeLibrary(OverbyteIcsSSLEAY.GSSLEAY_DLL_Handle);
                    OverbyteIcsSSLEAY.GSSLEAY_DLL_Handle := 0;
                end;
                if OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle <> 0 then begin
                    FreeLibrary(OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle);
                    OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle := 0
                end;
            end;
            f_SSL_load_error_strings;
            Tick := IcsGetTickCount;           // probably weak
            f_RAND_seed(@Tick, SizeOf(Tick));
        {$IFNDEF OPENSSL_NO_ENGINE}
            //* Load all bundled ENGINEs into memory and make them visible */
            f_ENGINE_load_builtin_engines;
        {$ENDIF}
        end; // SslRefCount = 0
        Inc(SslRefCount);
    finally
        SslCritSect.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Reminder:
  /* thread-local cleanup */
  ERR_remove_state(0);  // deprecated
  ERR_remove_thread_state(nil) // v1.0.0+ ** check for nil **

  /* thread-safe cleanup */
  ENGINE_cleanup();
  CONF_modules_unload(1);

  /* global application exit cleanup (after all SSL activity is shutdown) */
  ERR_free_strings();
  EVP_cleanup();
  CRYPTO_cleanup_all_ex_data();
}
procedure UnloadSsl;
begin
    SslCritSect.Enter;
    try
        if SslRefCount > 0 then        {AG 12/30/07}
            Dec(SslRefCount);
        if SslRefCount = 0 then begin  {AG 12/30/07}

            //* thread-local cleanup */
            if @f_ERR_remove_thread_state <> nil then
                f_ERR_remove_thread_state(nil) // OSSL v1.0.0+
            else
                f_ERR_remove_state(0); // deprecated

            //* thread-safe cleanup */
            f_CONF_modules_unload(1);
        {$IFNDEF OPENSSL_NO_ENGINE}
            f_ENGINE_cleanup;
        {$ENDIF}

            //* global application exit cleanup (after all SSL activity is shutdown) */
            f_ERR_free_strings;
            f_EVP_cleanup;
            f_CRYPTO_cleanup_all_ex_data;

            if OverbyteIcsSSLEAY.GSSLEAY_DLL_Handle <> 0 then begin
                FreeLibrary(OverbyteIcsSSLEAY.GSSLEAY_DLL_Handle);
                OverbyteIcsSSLEAY.GSSLEAY_DLL_Handle := 0;
            end;
            if OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle <> 0 then begin
                FreeLibrary(OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle);
                OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle := 0
            end;
        end;
    finally
        SslCritSect.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SslErrorToStr(Err: Integer): String;
begin
    case Err of
        SSL_ERROR_ZERO_RETURN      :  Result := 'SSL_ERROR_ZERO_RETURN';  // A closure alert has occurred in the protocol
        SSL_ERROR_WANT_CONNECT     :  Result := 'SSL_ERROR_WANT_CONNECT';
        SSL_ERROR_WANT_ACCEPT      :  Result := 'SSL_ERROR_WANT_ACCEPT';
        SSL_ERROR_WANT_READ        :  Result := 'SSL_ERROR_WANT_READ';
        SSL_ERROR_WANT_WRITE       :  Result := 'SSL_ERROR_WANT_WRITE';
        SSL_ERROR_WANT_X509_LOOKUP :  Result := 'SSL_ERROR_WANT_X509_LOOKUP';
        SSL_ERROR_SYSCALL          :  Result := 'SSL_ERROR_SYSCALL';
        SSL_ERROR_SSL              :  Result := 'SSL_ERROR_SSL';
        else
            Result := 'Unknown';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function print_errors: AnsiString;
var
    Flags    : Integer;
    Line     : Integer;
    Data     : PAnsiChar;
    FileName : PAnsiChar;
    ErrCode  : Cardinal;
begin
    result := '' ;
    ErrCode := f_ERR_get_error_line_data(@FileName, @Line, @Data, @Flags);
    while ErrCode <> 0 do begin
        if Result <> '' then Result := Result + #13#10;
        Result := Result + 'error code: ' + IcsIntToStrA(ErrCode) +
                          ' in ' + FileName + ' line ' + IcsIntToStrA(line);
        if (Data <> nil) and ((Flags and ERR_TXT_STRING) <> 0) then
                Result := Result + #13#10 + 'error data: ' + StrPas(Data);
        ErrCode := f_ERR_get_error_line_data(@FileName, @Line, @Data, @Flags);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function print_error: AnsiString;
var
    ErrCode : Integer;
begin
    ErrCode := f_ERR_peek_error;
    SetLength(result, 255);
    f_ERR_error_string_n(ErrCode, PAnsiChar(Result), Length(Result));
    SetLength(Result, StrLen(PAnsiChar(Result)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslErrMsg(const AErrCode: LongWord): String;
var
    Buf : AnsiString;
begin
    SetLength(Buf, 127);
    f_ERR_error_string_n(AErrCode, PAnsiChar(Buf), Length(Buf));
    SetLength(Buf, StrLen(PAnsiChar(Buf)));
    Result := String(Buf);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LastOpenSslErrMsg(Dump: Boolean): AnsiString;
var
    ErrMsg  : AnsiString;
    ErrCode : Integer;
begin
    ErrCode := f_ERR_get_error;
    SetLength(Result, 120);
    f_ERR_error_string_n(ErrCode, PAnsiChar(Result), Length(Result));
    SetLength(Result, StrLen(PAnsiChar(Result)));
    if Dump then begin
        ErrCode := f_ERR_get_error;
        while ErrCode <> 0 do begin
            SetLength(ErrMsg, 120);
            f_ERR_error_string_n(ErrCode, PAnsiChar(ErrMsg), Length(ErrMsg));
            SetLength(ErrMsg, StrLen(PAnsiChar(ErrMsg)));
            Result := Result + #13#10 + ErrMsg;
            ErrCode := f_ERR_get_error;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseComponent.RaiseLastOpenSslError(
    EClass          : ExceptClass;
    Dump            : Boolean = FALSE;
    const CustomMsg : String  = '');
begin
    FLastSslError := f_ERR_peek_error;
    if Length(CustomMsg) > 0 then
        raise EClass.Create(#13#10 + CustomMsg + #13#10 +
                            String(LastOpenSslErrMsg(Dump)) + #13#10)
    else
        raise EClass.Create(#13#10 + String(LastOpenSslErrMsg(Dump)) + #13#10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslBaseComponent.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FLastSslError   := 0;
    FSslInitialized := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslBaseComponent.Destroy;
begin
    FinalizeSsl;
{$IFNDEF NO_DEBUG_LOG}
    { Removes IcsLogger's free notification in a thread-safe way }
    SetIcsLogger(nil);
{$ENDIF}
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseComponent.FinalizeSsl;
begin
    if not FSslInitialized then
        Exit;
    UnloadSsl;
    FSslInitialized := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseComponent.InitializeSsl;
begin
    if FSslInitialized then
        Exit;
    LoadSsl;
    FSslInitialized := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}  { V5.21 }
{$IFNDEF NO_DEBUG_LOG}
function TSslBaseComponent.CheckLogOptions(const LogOption: TLogOption): Boolean;
begin
    Result := Assigned(FIcsLogger) and (LogOption in FIcsLogger.LogOptions);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseComponent.DebugLog(LogOption: TLogOption; const Msg: String);
begin
    if Assigned(FIcsLogger) then
        {if loAddStamp in FIcsLogger.LogOptions then
            FIcsLogger.DoDebugLog(Self, LogOption,
                                  IcsLoggerAddTimeStamp + ' ' + Msg)
        else}
        FIcsLogger.DoDebugLog(Self, LogOption, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseComponent.Notification(
    AComponent : TComponent;
    Operation  : TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if AComponent = FIcsLogger then
            FIcsLogger := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslBaseComponent.SetIcsLogger(const Value: TIcsLogger);
begin
    if Value <> FIcsLogger then begin
        if FIcsLogger <> nil then
            FIcsLogger.RemoveFreeNotification(Self);
        if Value <> nil then
            Value.FreeNotification(Self);
        FIcsLogger := Value;
    end;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TX509List.Create(AOwner: TComponent; AOwnsObjects: Boolean = TRUE);
begin
    inherited Create;
    FOwner            := AOwner;
    FX509Class        := TX509Base;
    FList             := TComponentList.Create(AOwnsObjects);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TX509List.Destroy;
begin
    FList.Free;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.Clear;
begin
    FList.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.Delete(const Index: Integer);
begin
    FList.Delete(Index);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.Remove(Item: TX509Base): Integer;
begin
    Result := FList.Remove(Item);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.Extract(Item: TX509Base): TX509Base;
begin
    Result := TX509Base(FList.Extract(Item));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.GetByHash(const Sha1Hash: AnsiString): TX509Base;
{ * Deprecated use Find * }
var
    I, J : Integer;
    P1, P2 : PInteger;
begin
    if Length(Sha1Hash) = 20 then begin
        for I := 0 to FList.Count -1 do begin
            Result := TX509Base(FList[I]);
            if Assigned(Result) and Assigned(Result.X509) then begin
                P1 := @Result.Sha1Digest[0];
                P2 := Pointer(Sha1Hash);
                for J := 1 to 4 do begin
                    if (P1^ <> P2^) then
                        Break;
                    Inc(P1);
                    Inc(P2);
                end;
                if P1^ = P2^ then
                    Exit;
            end;
        end;
    end;
    Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.Find(const ASha1Digest: THashBytes20): TX509Base;
var
    I, J : Integer;
    P1, P2 : PInteger;
begin
    if Length(ASha1Digest) = 20 then begin
        for I := 0 to FList.Count -1 do begin
            Result := TX509Base(FList[I]);
            if Assigned(Result) and Assigned(Result.X509) then begin
                P1 := @Result.Sha1Digest[0];
                P2 := @ASha1Digest[0];
                for J := 1 to 4 do begin
                    if (P1^ <> P2^) then
                        Break;
                    Inc(P1);
                    Inc(P2);
                end;
                if P1^ = P2^ then
                    Exit;
            end;
        end;
    end;
    Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.Find(const ASha1Hex: String): TX509Base;
var
    Digest: THashBytes20;
    I, J: Integer;
begin
    Result := nil;
    if Length(ASha1Hex) <> 40 then
        Exit;
    SetLength(Digest, 20);
    J := 0;
    for I := 1 to 39 do begin
        if Odd(I) then begin
            if (not IsXDigit(ASha1Hex[I])) or
               (not IsXDigit(ASha1Hex[I + 1])) then
                Exit;
            Digest[J] := XDigit2(PChar(@ASha1Hex[I]));
            Inc(J);
        end;
    end;
    Result := Find(Digest);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.Find(const AX509: PX509): TX509Base;
var
    Len  : Integer;
    Digest : THashBytes20;
begin
    if Assigned(AX509) then begin
        Len := 20;
        SetLength(Digest, Len);
        if f_X509_digest(AX509, f_EVP_sha1, @Digest[0], @Len) <> 0 then
            Result := Find(Digest)
        else
            Result := nil;
    end
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.GetCount: Integer;
begin
    Result := FList.Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.GetX509Base(Index: Integer): TX509Base;
begin
    Result := TX509Base(FList[Index]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.IndexOf(const X509Base: TX509Base): Integer;
begin
    Result := FList.IndexOf(X509Base);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.SetX509Base(Index: Integer; Value: TX509Base);
begin
    Assert(Value is FX509Class);
    FList[Index] := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.SetX509Class(const Value: TX509Class);
begin
    if Value <> FX509Class then begin
        Assert(GetCount = 0,
               'The X509 class should only be set when the list is empty');
        FX509Class := Value;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.GetOwnsObjects: Boolean;
begin
    Result := FList.OwnsObjects;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.SetOwnsObjects(const Value: Boolean);
begin
    FList.OwnsObjects := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.Add(X509: PX509 = nil): TX509Base;
begin
    Result := FX509Class.Create(FOwner, X509);
    FList.Add(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.AddItem(AItem: TX509Base): Integer;
begin
    Assert(AItem is FX509Class);
    Result := FList.Add(AItem);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.Insert(Index: Integer; X509: PX509 = nil): TX509Base;
begin
    Result := FX509Class.Create(FOwner, X509);
    try
        FList.Insert(Index, Result);
    except
        Result.Free;
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.InsertItem(Index: Integer; AItem: TX509Base);
begin
    Assert(AItem is FX509Class);
    FList.Insert(Index, AItem);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509List.GetByPX509(const X509: PX509): TX509Base;
{ * Deprecated use Find * }
begin
    Result := Find(X509);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509List.SortChain(ASortOrder: TX509ListSort);
var
    I      : Integer;
    Cur    : Integer;
    x1, x2 : PX509;
begin
    Cur := 0;
    while Cur < FList.Count do begin
        x1 := TX509Base(FList[Cur]).X509;
        for I := 0 to FList.Count - 1 do begin
            x2 := TX509Base(FList[I]).X509;
            if f_X509_check_issued(x1, x2) = 0 then begin
                if ASortOrder = xsrtIssuerFirst then begin
                    if Cur + 1 <> I then
                        FList.Move(Cur, I);
                end
                else if Cur - 1 <> I then
                    FList.Move(I, Cur);
                Break;
            end
            else if f_X509_check_issued(x2, x1) = 0 then begin
                if ASortOrder = xsrtIssuedFirst then begin
                    if Cur + 1 <> I then
                        FList.Move(Cur, I);
                end
                else if Cur - 1 <> I then
                    FList.Move(I, Cur);
                Break;
            end;
            if I = FList.Count - 1 then begin
                { Current is neither issuer nor issued by a cert in chain }
                { This should not happen in a valid certificate chain.    }
                if ASortOrder = xsrtIssuedFirst then
                    FList.Move(Cur, 0)
                else
                    FList.Move(Cur, FList.Count - 1);
            end;
        end;
        Inc(Cur);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSslEngine }

{$IFNDEF OPENSSL_NO_ENGINE}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslEngine.Close;
begin
    try
        case FState of
            esInit : f_ENGINE_finish(FEngine); // release the functional reference
            esOpen : f_ENGINE_free(FEngine); // release the structural reference
            else
                Exit;
        end;
        FEngine := nil;
        FState  := esClosed;
    except
        FEngine := nil;
        FState  := esClosed;
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslEngine.Control(const Cmd, Arg: String): Boolean;
var
    PArg : PAnsiChar;
    Msg  : String;
begin
    if FState = esClosed then
        raise ESslEngineError.Create('Cannot control a closed engine');

    if IcsCompareStr(Cmd, 'INIT') = 0 then // special ICS control command
    begin
        Result := Init;
        Exit;
    end;

    if Arg = '' then
    begin
        PArg := nil;
        Msg  := Format('Executing engine control command %s', [Cmd]);
    end
    else begin
        PArg := PAnsiChar(AnsiString(Arg));
        Msg :=  Format('Executing engine control command %s:%s', [Cmd, Arg]);
    end;

    if f_ENGINE_ctrl_cmd_string(FEngine, PAnsiChar(AnsiString(Cmd)), PArg, 0) = 0 then
    begin
        FLastSslError := f_ERR_peek_last_error;
        FLastErrorMsg := Msg  + ' ' + String(LastOpenSslErrMsg(TRUE));
        Result        := FALSE;
    end
    else begin
        FLastSslError := 0;
        FLastErrorMsg := Msg;
        Result        := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslEngine.Destroy;
begin
    Close;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslEngine.Init: Boolean;
begin
    if FState = esClosed then
        raise ESslEngineError.Create('Cannot initialize a closed engine');
    FLastErrorMsg := 'Engine ' + FNameID + 'initialized';
    FLastSslError := 0;
    Result        := TRUE;
    if FState = esInit then
        Exit;
    if f_ENGINE_init(FEngine) = 0 then
    begin
        FLastSslError := f_ERR_peek_last_error;
        FLastErrorMsg := 'ENGINE_init'#13#10 + String(LastOpenSslErrMsg(TRUE));
        Result        := FALSE;
    end
    else begin
        { This should always succeed if 'FEngine' was initialised OK }
        f_ENGINE_set_default(FEngine, ENGINE_METHOD_ALL);
        FState := esInit;
        f_ENGINE_free(FEngine); // release the structural reference
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslEngine.Open: Boolean;
begin
    InitializeSsl;
    {if IcsCompareStr(FNameID, 'auto') = 0 then
    begin
        f_ENGINE_register_all_complete;
        FLastErrorMsg := 'Auto engine support enabled';
        Result := TRUE;
        Exit;
    end;}

    Close; // close the previous one (if assigned)
    FEngine := f_ENGINE_by_id(PAnsiChar(AnsiString(FNameID)));

    if FEngine = nil then
    begin
        FLastSslError := f_ERR_peek_last_error;
        FLastErrorMsg := String(LastOpenSslErrMsg(TRUE));
        Result        := FALSE;
    end
    else begin
        FState := esOpen;
        Result := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslEngine.SetNameID(const Value: String);
begin
    Close;
    FNameID := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF OPENSSL_NO_ENGINE}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
    SslIntOptions: array[TSslOption] of Integer =                   { V7.30 }
           (SSL_OP_CIPHER_SERVER_PREFERENCE,
            SSL_OP_MICROSOFT_SESS_ID_BUG,
            SSL_OP_NETSCAPE_CHALLENGE_BUG,
            SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG,
            SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG,
            SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER,
            SSL_OP_MSIE_SSLV2_RSA_PADDING,
            SSL_OP_SSLEAY_080_CLIENT_DH_BUG,
            SSL_OP_TLS_D5_BUG,
            SSL_OP_TLS_BLOCK_PADDING_BUG,
            SSL_OP_TLS_ROLLBACK_BUG,
            SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS,
            SSL_OP_SINGLE_DH_USE,
            SSL_OP_EPHEMERAL_RSA,
            SSL_OP_NO_SSLv2,
            SSL_OP_NO_SSLv3,
            SSL_OP_NO_TLSv1,
            SSL_OP_PKCS1_CHECK_1,
            SSL_OP_PKCS1_CHECK_2,
            SSL_OP_NETSCAPE_CA_DN_BUG,
            SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION,
            SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG,
            SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION); // Since OSSL 0.9.8n

  SslIntSessCacheModes: array[TSslSessCacheMode] of Integer =     { V7.30 }
            (SSL_SESS_CACHE_CLIENT,
             SSL_SESS_CACHE_SERVER,
             SSL_SESS_CACHE_NO_AUTO_CLEAR,
             SSL_SESS_CACHE_NO_INTERNAL_LOOKUP,
             SSL_SESS_CACHE_NO_INTERNAL_STORE);


  SslIntVerifyFlags: array[TSslVerifyFlag] of Integer =
           (X509_V_FLAG_CB_ISSUER_CHECK,
            X509_V_FLAG_USE_CHECK_TIME,
            X509_V_FLAG_CRL_CHECK,
            X509_V_FLAG_CRL_CHECK_ALL,
            X509_V_FLAG_IGNORE_CRITICAL,
            X509_V_FLAG_X509_STRICT,
            X509_V_FLAG_ALLOW_PROXY_CERTS);


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

constructor TSslContext.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
{$IFNDEF NO_SSL_MT}
    FLock := TIcsCriticalSection.Create;
{$ENDIF}
    FSslCtx := nil;
    SetSslVerifyPeerModes([SslVerifyMode_PEER]);
    SetSslCipherList(sslCiphersNormal);  // V8.10 same as 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    FSslVersionMethod    := sslV23;
    SslVerifyDepth       := 9;
    FSslSessionTimeOut   := 0; // OSSL-default
    FSslSessionCacheSize := SSL_SESSION_CACHE_MAX_SIZE_DEFAULT;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslContext.Destroy;
begin
    DeInitContext;
{$IFNDEF NO_SSL_MT}
    FLock.Free;
{$ENDIF}
    inherited Destroy;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.FreeNotification(AComponent: TComponent);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        inherited FreeNotification(AComponent);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.RemoveFreeNotification(AComponent: TComponent);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        inherited RemoveFreeNotification(AComponent);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
procedure TSslContext.TriggerDebugLog (LogOption: TLogOption;   { V5.21 }
                                                 const Msg, Data: String);
var
    S: String;
begin
    if loOptStamp in FLogOptions then
        S := WSocketAddTimeStamp + ' ' + Msg
    else
        S := Msg;
    if loOptEvent in FLogOptions then begin
        if Assigned (FOnIcsLogEvent) then
                        FOnIcsLogEvent(Self, LogOption, S, '');
    end;
    if loOptOutDebug in FLogOptions then OutputDebugString(Pchar(S));
    if loOptFile in FLogOptions then begin
        if WSocketOpenLogFile then WriteLn(WSocketLogFile, S);
    end;
end;     *)


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslContext.InitializeCtx: PSSL_CTX;
var
    Meth : PSSL_METHOD;
begin
    case FSslVersionMethod of
    sslV2:            Meth := f_SSLv2_method;
    sslV2_CLIENT:     Meth := f_SSLv2_client_method;
    sslV2_SERVER:     Meth := f_SSLv2_server_method;
    sslV3:            Meth := f_SSLv3_method;
    sslV3_CLIENT:     Meth := f_SSLv3_client_method;
    sslV3_SERVER:     Meth := f_SSLv3_server_method;
    sslTLS_V1:        Meth := f_TLSv1_method;
    sslTLS_V1_CLIENT: Meth := f_TLSv1_client_method;
    sslTLS_V1_SERVER: Meth := f_TLSv1_server_method;
    sslV23:           Meth := f_SSLv23_method;
    sslV23_CLIENT:    Meth := f_SSLv23_client_method;
    sslV23_SERVER:    Meth := f_SSLv23_server_method;
    else              raise ESslContextException.Create('Unknown SslVersionMethod');
    end;
    Result := f_SSL_CTX_new(Meth);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslContext.GetIsCtxInitialized : Boolean;
begin
    Result := FSslCtx <> nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslContext.TrustCert(Cert: TX509Base): Boolean;
var
    St : PX509_STORE;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        Result := FALSE;
        if (not Assigned(FSslCtx)) then
            raise ESslContextException.Create(msgSslCtxNotInit);
        if (not Assigned(Cert)) or (not Assigned(Cert.X509)) then
            Exit;
        //St := nil;
        St := f_SSL_CTX_get_cert_store(FSslCtx);
        if Assigned(St) then
            Result := f_X509_STORE_add_cert(St, Cert.X509) <> 0;
        { Fails if cert exists in store }
{$IFNDEF NO_DEBUG_LOG}
        if (not Result) and
            CheckLogOptions(loSslErr) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslErr, String(LastOpenSslErrMsg(TRUE)));
{$ELSE}
        if (not Result) then
            f_ERR_clear_error;
{$ENDIF}
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function PasswordCallBack(
    Buf      : PAnsiChar;
    Num      : Integer;
    RWFlag   : Integer;
    UserData : Pointer) : Integer; cdecl;
var
    Obj : TSslContext;
    SslPassPhraseA : AnsiString;
begin
{$IFNDEF NO_SSL_MT}
    LockPwdCB.Enter;
    try
{$ENDIF}
        Obj := TSslContext(UserData);
        if (Num < (Length(Obj.SslPassPhrase) + 1)) or
           (Length(Obj.SslPassPhrase) = 0) then
            Result := 0
        else begin
            SslPassPhraseA := AnsiString(Obj.SslPassPhrase);
            Move(Pointer(SslPassPhraseA)^, Buf^, Length(SslPassPhraseA) + 1);
            Result := Length(SslPassPhraseA);
        end;
{$IFNDEF NO_SSL_MT}
    finally
        LockPwdCB.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF OPENSSL_NO_ENGINE}
function PinCallback(ui: PUI; uis: PUI_STRING): Integer; cdecl;
var
    Obj : TSslContext;
begin
{$IFNDEF NO_SSL_MT}
    LockPwdCB.Enter;
    try
{$ENDIF}
        Obj := TSslContext(f_Ics_UI_get_app_data(ui));
        f_UI_set_result(ui, uis, PAnsiChar(AnsiString(Obj.FSslPassPhrase)));
        Result := 1;

{$IFNDEF NO_SSL_MT}
    finally
        LockPwdCB.Leave;
    end;
{$ENDIF}
end;
{$ENDIF}



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function PeerVerifyCallback(
    Ok       : Integer;
    StoreCtx : PX509_STORE_CTX) : Integer; cdecl;
var
    MySsl   : PSSL;
    Obj     : TCustomSslWSocket;
    Cert    : PX509;
    CurCert : TX509Base;
begin
{$IFNDEF NO_SSL_MT}
    LockVerifyCB.Enter;
    try
{$ENDIF}
        // Retrieve the pointer to the SSL of the current connection
        MySsl := f_X509_STORE_CTX_get_ex_data(
                                StoreCtx, f_SSL_get_ex_data_X509_STORE_CTX_idx);
        // Retrieve the object reference we stored at index 0
        Obj := TCustomSslWSocket(f_SSL_get_ex_data(MySsl, 0));
        if Assigned(Obj) then begin
            Obj.Pause;
            Obj.FSsl_In_CB := TRUE;
            try
                Cert := f_X509_STORE_CTX_get_current_cert(StoreCtx);
                { Lookup this cert in our custom list (chain) }
                CurCert := Obj.SslCertChain.Find(Cert);
                { Add it to our list }
                if not Assigned(CurCert) then begin
                    //SslCertChain.X509Class was set in InitSslConnection
                    //Obj.SslCertChain.X509Class := Obj.X509Class;
                    CurCert := Obj.SslCertChain.Add(Cert);
                    CurCert.VerifyResult := f_X509_STORE_CTX_get_error(StoreCtx);
                    CurCert.FFirstVerifyResult := CurCert.VerifyResult;
                end
                else { Unfortunately me must overwrite here }
                    CurCert.VerifyResult := f_X509_STORE_CTX_get_error(StoreCtx);
                CurCert.VerifyDepth := f_X509_STORE_CTX_get_error_depth(StoreCtx);
                //CurCert.CustomVerifyResult := CurCert.VerifyResult; // don't overwrite
                Obj.SslCertChain.FLastVerifyResult := CurCert.VerifyResult;
{$IFNDEF NO_DEBUG_LOG}
                if Obj.CheckLogOptions(loSslInfo) then begin  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                    Obj.DebugLog(loSslInfo,'VCB> VerifyPeer: Subject = '  + CurCert.SubjectOneLine);
                    Obj.DebugLog(loSslInfo,'VCB> VerifyPeer: Serial  = $' + IntToHex(CurCert.SerialNum, 8));
                    Obj.DebugLog(loSslInfo,'VCB> VerifyPeer: Error   = '  + CurCert.VerifyErrMsg);
                end;
{$ENDIF}
                // Save verify result
                Obj.FSslVerifyResult := CurCert.VerifyResult;
                Obj.TriggerSslVerifyPeer(Ok, CurCert);
                if Ok <> 0 then
                    Obj.FSslVerifyResult := X509_V_OK;
            finally
                Obj.Resume;
                Obj.FSsl_In_CB := FALSE;
                if Obj.FHSocket = INVALID_SOCKET then
                    PostMessage(Obj.FWindowHandle, Obj.FMsg_WM_RESET_SSL, 0, 0);
            end;
        end;
        Result := Ok;
{$IFNDEF NO_SSL_MT}
    finally
        LockVerifyCB.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RemoveSessionCallback(const Ctx : PSSL_CTX; Sess : PSSL_SESSION); cdecl;
var
    Obj : TSslContext;
begin
   { If remove_session_cb is not null, it will be called when               }
   { a session-id is removed from the cache.  After the call,               }
   { OpenSSL will SSL_SESSION_free() it.                                    }
   { Also: It is invoked whenever a SSL_SESSION is destroyed.  It is called }
   { just before the session object is destroyed because it is invalid or   }
   { has expired.                                                           }
{$IFNDEF NO_SSL_MT}
    LockRemSessCB.Enter;
    try
{$ENDIF}
        Obj := TSslContext(f_SSL_CTX_get_ex_data(Ctx, 0));
        if Assigned(Obj) then begin
{$IFNDEF NO_DEBUG_LOG}
            if Obj.CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                Obj.DebugLog(loSslInfo,'RSCB> Session removed');
{$ENDIF}
            if Assigned(Obj.FOnRemoveSession) then
                Obj.FOnRemoveSession(Obj, Sess);
        end;
{$IFNDEF NO_SSL_MT}
    finally
        LockRemSessCB.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_EVP_PKEY_dup(PKey: PEVP_PKEY): PEVP_PKEY;
begin
    Result := nil;
    if PKey <> nil then begin
        Ics_Ssl_EVP_PKEY_IncRefCnt(PKey);
        Result := PKey;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ClientCertCallback(
    Ssl     : PSSL;
    X509    : PPX509;
    PKEY    : PPEVP_PKEY): Integer; cdecl;
var
    Obj  : TCustomSslWSocket;
    Cert : TX509Base;
    X, P : Pointer;
begin
    { It's called when a client certificate is requested by a server and no    }
    { certificate was yet set for the SSL object. client_cert_cb() is the      }
    { application defined callback. If it wants to set a certificate, a        }
    { certificate/private key combination must be set using the x509 and pkey  }
    { arguments and ``1'' must be returned. The certificate will be installed  }
    { into ssl, see the NOTES and BUGS sections. If no certificate should be   }
    { set, ``0'' has to be returned and no certificate will be sent.           }
    { A negative return value will suspend the handshake and the handshake     }
    { function will return immediatly. SSL_get_error(3) will return            }
    { SSL_ERROR_WANT_X509_LOOKUP to indicate, that the handshake was suspended.}
    { The next call to the handshake function will again lead to the call of   }
    { client_cert_cb(). It is the job of the client_cert_cb() to store         }
    { information about the state of the last call, if required to continue.   }

    { Called when a client certificate is requested but there is not one set   }
    { against the SSL_CTX or the SSL.  If the callback returns 1, x509 and     }
    { pkey need to point to valid data.  The library will free these when      }
    { required so if the application wants to keep these around, increment     }
    { their reference counts.  If 0 is returned, no client cert is             }
    { available.  If -1 is returned, it is assumed that the callback needs     }
    { to be called again at a later point in time.  SSL_connect will return    }
    { -1 and SSL_want_x509_lookup(ssl) returns TRUE.  Remember that            }
    { application data can be attached to an SSL structure via the             }

{$IFNDEF NO_SSL_MT}
    LockClientCertCB.Enter;
    try
{$ENDIF}
        Result := 0;
        Obj := TCustomSslWSocket(f_SSL_get_ex_data(Ssl, 0));
        if Assigned(Obj) then begin
            Obj.FSsl_In_CB := TRUE;
            Obj.Pause;
            try
                if Assigned(Obj.FOnSslCliCertRequest) then begin
                    Cert := nil;
                    try
                        Obj.FOnSslCliCertRequest(Obj, Cert);
                        if (Cert <> nil) and (Cert.X509 <> nil) and
                           (Cert.PrivateKey <> nil) then begin
                            X     := f_X509_dup(Cert.X509);
                            P     := Ics_EVP_PKEY_dup(Cert.FPrivateKey);
                            X509^  := X;
                            PKEY^  := P;
                            Result := 1;
                        end
                        else begin
                            //X509  := nil;
                            //PKEY  := nil;
                        end;
                    except
                        // psst
                    end;
                end;
            finally
                Obj.Resume;
                Obj.FSsl_In_CB := FALSE;
                if Obj.FHSocket = INVALID_SOCKET then
                    PostMessage(Obj.FWindowHandle, Obj.FMsg_WM_RESET_SSL, 0, 0);
            end;
        end;
{$IFNDEF NO_SSL_MT}
    finally
        LockClientCertCB.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_SSL_MT}
procedure TSslContext.Lock;
begin
    FLock.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.Unlock;
begin
    FLock.Leave;
end;

{$ENDIF}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NewSessionCallback(const SSL : PSSL; Sess : PSSL_SESSION): Integer; cdecl;
var
    Obj                : TCustomSslWSocket;
    AddToInternalCache : Boolean;
    SessID             : Pointer;
    IdLen              : Integer;
begin
   { If this callback is not null, it will be called each                  }
   { time a session id is added to the cache.  If this function            }
   { returns 1, it means that the callback will do a                       }
   { SSL_SESSION_free() when it has finished using it. Otherwise,          }
   { on 0, it means the callback has finished with it.                     }
   { Also: If this function returns 0,  the session object will not be     }
   { cached. A nonzero return allows the session to be cached              }

{$IFNDEF NO_SSL_MT}
    LockNewSessCB.Enter;
    try
{$ENDIF}
        Result := 0;
        Obj := TCustomSslWSocket(f_SSL_get_ex_data(SSL, 0));
        if not Assigned(Obj) then
            raise Exception.Create('NewSessionCallback Obj not assigned');
        Obj.FSsl_In_CB := TRUE;
        try
{$IFNDEF NO_DEBUG_LOG}
            if Obj.CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                Obj.DebugLog(loSslInfo, 'NSCB> New session created');
{$ENDIF}
            //f_SSL_session_get_id(Sess, SessID, IdLen); { 03/02/07 AG }
            SessID := f_SSL_SESSION_get_id(Sess, IdLen); { 03/02/07 AG }
            AddToInternalCache := FALSE; // not sure about the default value
            if Assigned(Obj.FOnSslSvrNewSession) then
                Obj.FOnSslSvrNewSession(Obj, Sess, SessID, IdLen, AddToInternalCache);
            if AddToInternalCache then
                Result := 1
            else
                Result := 0;
        finally
            Obj.FSsl_In_CB := FALSE;
            if Obj.FHSocket = INVALID_SOCKET then
                PostMessage(Obj.FWindowHandle, Obj.FMsg_WM_RESET_SSL, 0, 0);
        end;
{$IFNDEF NO_SSL_MT}
    finally
        LockNewSessCB.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetSessionCallback(
    const SSL : PSSL;
    SessId    : Pointer;
    IdLen     : Integer;
    Ref       : PInteger) : PSSL_SESSION; cdecl;
var
    Obj         : TCustomSslWSocket;
    Sess        : Pointer;
    IncRefCount : Boolean;
begin
    { SessId = Session ID that's being requested by the peer.             }
    { The Session ID is distinctly different from the session ID context  }
    { Ref = An output from the callback. It is used to allow the          }
    { callback to specify whether the reference count on the returned     }
    { session object should be incremented or not. It returns as          }
    { nonzero if the object's reference count should be incremented;      }
    { otherwise, zero is returned                                         }

{$IFNDEF NO_SSL_MT}
    LockGetSessCB.Enter;
    try
{$ENDIF}
        Result := nil;
        Obj := TCustomSslWSocket(f_SSL_get_ex_data(SSL, 0));
        if not Assigned(Obj) then
            raise Exception.Create('GetSessionCallback Obj not assigned');
        Obj.FSsl_In_CB := TRUE;
        try
{$IFNDEF NO_DEBUG_LOG}
            if Obj.CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                Obj.DebugLog(loSslInfo, 'GSCB> Get session');
{$ENDIF}
            Sess := nil;
            IncRefCount := (Ref^ <> 0);
            if Assigned(Obj.FOnSslSvrGetSession) then
                Obj.FOnSslSvrGetSession(Obj, Sess, SessId, IdLen, IncRefCount);
            if IncRefCount then
                Ref^ := 1
            else
                Ref^ := 0;
            Result := Sess;
        finally
            Obj.FSsl_In_CB := FALSE;
            if Obj.FHSocket = INVALID_SOCKET then
                PostMessage(Obj.FWindowHandle, Obj.FMsg_WM_RESET_SSL, 0, 0);
        end;
{$IFNDEF NO_SSL_MT}
    finally
        LockGetSessCB.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslContext.OpenFileBio(
    const FileName    : String;
    Methode           : TBioOpenMethode): PBIO;
begin
    if Filename = '' then
        raise ESslContextException.Create('File name not specified');
    if (Methode = bomRead) and (not FileExists(Filename)) then
        raise ESslContextException.Create('File not found "' +
                                          Filename + '"');
    if Methode = bomRead then
        Result := f_BIO_new_file(PAnsiChar(AnsiString(Filename)), PAnsiChar('r+'))
    else
        Result := f_BIO_new_file(PAnsiChar(AnsiString(Filename)), PAnsiChar('w+'));
    if Result = nil then
        RaiseLastOpenSslError(ESslContextException, FALSE,
                              'Error on opening file "' + Filename + '"');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ A X509_INFO may contain x509/crl/pkey sets, PEM format only }
function TSslContext.LoadStackFromInfoFile(const FileName: String;
    Mode: TInfoExtractMode): PStack;
var
    InfoStack   : PStack;
    CertInfo    : PX509_INFO;
    InBIO       : PBIO;
    //PKey        : PX509_PKEY;
begin
    //InfoStack := nil;
    //CertInfo  := nil;
    //InBIO     := nil;
    Result      := nil;
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if FileName = '' then
        Exit;
    InBIO := OpenFileBio(FileName, bomRead);
    try
        // This loads from a file, a stack of x509/crl/pkey sets
        InfoStack := PStack(f_PEM_X509_INFO_read_bio(InBIO, nil, nil, nil));
        if not Assigned(InfoStack) then
            raise ESslContextException.CreateFmt('Error reading info file "%s"',
                                                 [FileName]);
        try
            if f_sk_num(InfoStack) > 0 then
                Result := f_sk_new_null
            else
                Exit;
            if Result = nil then
                raise ESslContextException.Create('Error creating Stack');
            // Scan over it and pull out what is needed
            while f_sk_num(InfoStack) > 0 do begin
                CertInfo := PX509_INFO(f_sk_delete(InfoStack, 0));
                case Mode of
                emCert :
                    if CertInfo^.x509 <> nil then
                        f_sk_insert(Result, PAnsiChar(f_X509_dup(CertInfo^.x509)),
                                                  f_sk_num(Result) + 1);
                { A Dup-function for X509_PKEY is still missing in OpenSsl arrg!
                emKey :
                    if CertInfo^.x_pkey <> nil then
                        f_sk_insert(Result, PChar(f_X509_PKEY_dup(CertInfo^.x_pkey)),
                                                  f_sk_num(Result) + 1);}
                emCrl :
                    if CertInfo^.crl <> nil then
                        f_sk_insert(Result, PAnsiChar(f_X509_CRL_dup(CertInfo^.crl)),
                                                  f_sk_num(Result) + 1);
                end; //case
                f_X509_INFO_free(CertInfo);
            end;
        finally
             f_sk_pop_free(InfoStack, @f_X509_INFO_free);
        end;
    finally
       f_Bio_free(InBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ PEM format only, the file may contain multiple certificates.              }
{ Loads intermediate CA certificates needed to build a complete chain.      }
{ PEM format only, any file of a given directory }
{ PEM format only, the file may contain multiple CRL's }
procedure TSslContext.LoadCrlFromFile(const Filename: String);
var
    CRL      : PX509_CRL;
    St       : PX509_STORE;
    CrlStack : PStack;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if not Assigned(FSslCtx) then
            raise ESslContextException.Create(msgSslCtxNotInit);
        if (Filename <> '') and (not FileExists(Filename)) then
            raise ESslContextException.Create('CRL file not found "' +
                                              Filename + '"');
        if Filename <> '' then begin
            //CrlStack := nil;
            CrlStack := LoadStackFromInfoFile(FileName, emCrl);
            if not Assigned(CrlStack) then
                raise ESslContextException.Create('Error on reading CRL file "' +
                                                  Filename + '"');
            try
                //St := nil;
                St := f_SSL_CTX_get_cert_store(FSslCtx);
                if not Assigned(St) then
                    raise ESslContextException.Create('Error on opening store');
                while f_sk_num(CrlStack) > 0 do begin
                    //Crl := nil;
                    Crl := PX509_CRL(f_sk_delete(CrlStack, 0));
                    if Assigned(Crl) then
                        try
                            { Fails if CRL is already in hash table }
                            if f_X509_STORE_add_crl(St, Crl) = 0 then
{$IFNDEF NO_DEBUG_LOG}
                                if CheckLogOptions(loSslErr) then  { V5.21 }
                                    DebugLog(loSslErr, String(LastOpenSslErrMsg(True)));
{$ELSE}
                                f_ERR_clear_error;
{$ENDIF};
                        finally
                            f_X509_CRL_free(Crl);
                        end;
                    end;
            finally
                f_sk_pop_free(CrlStack, @f_X509_CRL_free);
            end;
        end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ PEM format only, any file of a given directory }
procedure TSslContext.LoadCrlFromPath(const Path: String);
var
    SRec  : TSearchRec;
    Found : Boolean;
    S     : String;
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (Path <> '') and (not DirectoryExists(Path)) then
        raise ESslContextException.Create('CRL directory not found "' +
                                          Path + '"');
    if Path <> '' then begin
        S := IncludeTrailingPathDelimiter(Path);
        Found := FindFirst(S + '*.*', faAnyFile - faDirectory, SRec) = 0;
        if Found then
            try
                while Found do begin
                    LoadCrlFromFile(S + SRec.Name);
                    Found := FindNext(SRec) = 0;
                end;
            finally
                FindClose(SRec);
            end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.LoadVerifyLocations(const CAFile, CAPath: String);
var
    PCAPath : PAnsiChar;
    PCAFile : PAnsiChar;
begin
    // Load the CAs we trust
    //
    // If CAfile is not NIL, it points to a file of CA certificates in PEM
    // format. The file can contain several CA certificates.
    //
    // If CApath is not NIL, it points to a directory containing CA
    // certificates in PEM format. The files each contain one CA certificate.
    // The files are looked up by the CA subject name hash value, which must
    // hence be available. If more than one CA certificate with the same name
    // hash value exist, the extension must be different (e.g. 9d66eef0.0,
    // 9d66eef0.1 etc). The search is performed in the ordering of the
    // extension number, regardless of other properties of the certificates.
    // The certificates in CApath are only looked up when required, e.g. when
    // building the certificate chain or when actually performing the
    // verification of a peer certificate. When looking up CA certificates,
    // the OpenSSL library will first search the certificates in CAfile, then
    // those in CApath.

    if FSslCtx = nil then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (CAFile <> '') and (not FileExists(CAFile)) then
        raise ESslContextException.Create('File not found "' + CAFile + '"');
    if (Length(CAPath) > 0) and (not DirectoryExists(CAPath)) then
        raise ESslContextException.Create('Directory not found "' + CAPath + '"');

    if CAPath <> '' then
        PCAPath := PAnsiChar(AnsiString(CAPath))
    else
        PCAPath := nil;
    if CAFile <> '' then
        PCAFile := PAnsiChar(AnsiString(CAFile))
    else
        PCAFile := nil;
    if ((PCAFile <> nil) or (PCAPath <> nil)) and
       (f_SSL_CTX_load_verify_locations(FSslCtx,
                                        PCAFile, PCAPath) = 0) then
        RaiseLastOpenSslError(ESslContextException, TRUE,
                              'Can''t read CA File "' +
                              FSslCAFile + '" or ' +
                              'CA Path "' + CAPath + '"');
    if (PCAFile = nil) and (PCAPath = nil) and
       (f_SSL_CTX_set_default_verify_paths(FSslCtx) <> 1) then
        RaiseLastOpenSslError(ESslContextException, TRUE,
                              'Error loading default CA file ' +
                              'and/or directory');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The PEM file specified may contain just a single certificate or a         }
{ certificate chain. A chain must start with the server or client           }
{ certificate followed by intermediate CA certificates until the root       }
{ certificate (optional). This entire chain is sent to the peer for         }
{ verification.                                                             }
procedure TSslContext.LoadCertFromChainFile(const FileName: String);
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (FileName <> '') and (not FileExists(FileName)) then
        raise ESslContextException.Create('File not found "' + FileName + '"');
    if (FileName <> '') and
       (f_SSL_CTX_use_certificate_chain_file(FSslCtx,
                                             PAnsiChar(AnsiString(FileName))) = 0) then begin
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslErr) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslErr, String(LastOpenSslErrMsg(TRUE)));
{$ELSE}
        f_ERR_clear_error;
{$ENDIF}
        RaiseLastOpenSslError(ESslContextException, TRUE,
                              'Can''t read certificate ' +
                              'file "' + FileName + '"');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.LoadPKeyFromFile(const FileName: String);
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (FileName <> '') and (not FileExists(FileName)) then
        raise ESslContextException.Create('File not found "' + FileName + '"');
    if (FileName <> '') and
       (f_SSL_CTX_use_PrivateKey_file(FSslCtx, PAnsiChar(AnsiString(FileName)),
                                      SSL_FILETYPE_PEM) = 0) then begin
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslInfo, String(LastOpenSslErrMsg(TRUE)));
{$ELSE}
        f_ERR_clear_error;
{$ENDIF}
        RaiseLastOpenSslError(ESslContextException, TRUE,
                              'Can''t load private key ' +
                              'file "' + FileName + '"');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF OPENSSL_NO_ENGINE}
procedure TSslContext.Notification(
    AComponent : TComponent;
    Operation  : TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if AComponent = FCtxEngine then
            FCtxEngine := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetCtxEngine(const Value: TSslEngine);
begin
    FCtxEngine := Value;
    if Value <> nil then
        Value.FreeNotification(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.LoadPKeyFromEngine(CtxEngine: TSslEngine);
var
    PKey : PEVP_PKEY;
    Uim  : PUI_METHOD;
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (CtxEngine = nil) or (CtxEngine.KeyID = '') then
        raise ESslContextException.Create('Engine and KeyID may not be empty');

    if CtxEngine.State <> esInit then
        if not CtxEngine.Init then
            raise ESslContextException.Create(CtxEngine.LastErrorMsg);
        Uim := f_UI_create_method(PAnsiChar('ICS WIN32 UI'));
        f_UI_method_set_reader(Uim, PinCallback);
        PKey := f_ENGINE_load_private_key(CtxEngine.E,
                                          PAnsiChar(AnsiString(CtxEngine.KeyID)),
                                          Uim, Pointer(Self));

        if PKey = nil then
            RaiseLastOpenSslError(ESslContextException, TRUE,
                              'Can''t load private key from Engine');

        if f_SSL_CTX_use_PrivateKey(FSslCtx, PKey) = 0 then
            RaiseLastOpenSslError(ESslContextException, TRUE,
                                  'Can''t use private key');
end;
{$ENDIF OPENSSL_NO_ENGINE}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Open a PEM CA certificate file and add the CA name extracted              }
{ to the list of CAs sent to the client when requesting a client            }
{ certificate, usefull only in server mode.                                 }
procedure TSslContext.AddClientCAFromFile(const FileName: String);
var
    X : TX509Base;
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (Filename <> '') and (not FileExists(Filename)) then
        raise ESslContextException.Create('Certificate file not found "' +
                                          Filename + '"');
    if Filename <> '' then begin
        X := TX509Base.Create(nil);
        try
            X.LoadFromPemFile(FileName);
            if f_SSL_CTX_add_client_CA(FSslCtx, X.X509) <> 1 then
                RaiseLastOpenSslError(ESslContextException, TRUE,
                                      'Can''t load client CA ' +
                                      'file "' + FileName + '"');
        finally
            X.Free;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Scan all certificates in a PEM CAfile and list their names as acceptable  }
{ CAs sent to the client when we request a client certificate. Usefull only }
{ in server mode.                                                           }
procedure TSslContext.SetClientCAListFromFile(const FileName: String);
var
    Sk : PSTACK_OF_X509_NAME;
begin
    if not Assigned(FSslCtx) then
        raise ESslContextException.Create(msgSslCtxNotInit);
    if (Filename <> '') and (not FileExists(Filename)) then
        raise ESslContextException.Create('Certificate file not found "' +
                                          Filename + '"');
    if Filename <> '' then begin
        Sk := f_SSL_load_client_CA_file(PAnsiChar(AnsiString(FileName)));
        if not Assigned(Sk) then
            raise ESslContextException.Create('Error on reading certificate ' +
                                              'file "' + Filename + '"');
        f_SSL_CTX_set_client_CA_list(FSslCTX, Sk); // frees Sk
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.InitContext;
var
    SslSessCacheModes : TSslSessCacheModes;
    LOpts: LongWord;
begin
    InitializeSsl; //loads libs
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
    {$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
         DebugLog(loSslInfo, 'InitCtx> OpenSSL version: ' + OpenSslVersion);
    {$ENDIF}
    {$IFNDEF OPENSSL_NO_ENGINE}
        if (not GSslRegisterAllCompleted) and FAutoEnableBuiltinEngines then
        begin
            // Register all of them for every algorithm they collectively implement /
            f_ENGINE_register_all_complete;
            GSslRegisterAllCompleted := TRUE;
        end;
    {$ENDIF}

        if not Assigned(FSslCtx) then begin
            // Create new context
            FSslCtx := InitializeCtx;
            if not Assigned(FSslCtx) then
                raise ESslContextException.Create('Failed to initialize context');
        end;

        try
            if Assigned(FOnBeforeInit) then
                FOnBeforeInit(Self);

            // Load our key and certificate
        {$IFNDEF OPENSSL_NO_ENGINE}
            if (FCtxEngine <> nil) and
               (eccLoadPrivKey in FCtxEngine.CtxCapabilities) then
                LoadPKeyFromEngine(FCtxEngine)
            else begin
        {$ENDIF}
                // Set the password callback and our custom user data
                f_SSL_CTX_set_default_passwd_cb(FSslCtx, PasswordCallBack);
                f_SSL_CTX_set_default_passwd_cb_userdata(FSslCtx, Self);

                LoadPKeyFromFile(FSslPrivKeyFile);
        {$IFNDEF OPENSSL_NO_ENGINE}
            end;
        {$ENDIF}

            LoadCertFromChainFile(FSslCertFile);

            // See notes in the procedure
            LoadVerifyLocations(FSslCAFile, FSslCAPath);

            LoadCRLFromFile(FSslCRLFile);
            LoadCRLFromPath(FSslCRLPath);
            //f_SSL_CTX_ctrl(FSslCtx, SSL_CTRL_MODE, SSL_MODE_ENABLE_PARTIAL_WRITE, nil); // Test

            f_X509_STORE_set_flags(f_SSL_CTX_get_cert_store(FSslCtx),
                                   FSslVerifyFlags);

            //raise Exception.Create('Test');

            // Now the verify stuff
            SetSslVerifyPeerModes(SslVerifyPeerModes);
            {if FSslX509Trust <> ssl_X509_TRUST_NOT_DEFINED then
                if f_SSL_CTX_set_trust(FSslCtx, Integer(FSslX509Trust)) = 0 then
                    raise Exception.Create('Error setting trust'); }

            { No tickets yet                                       }
            LOpts := FSslOptionsValue or SSL_OP_NO_TICKET;

            if ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_1000 then begin
                { This is a workaround a possible bug in OSSL 1.0.0(d)
                  check if future versions fix it.
                  SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER causes
                  error:1408F044:SSL routines:SSL3_GET_RECORD:internal error
                  on session resumption in InitSslConnection. }
                if LOpts and SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER =
                  SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER then
                    LOpts := LOpts and not SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER;
            end;
            { Adds the options set via bitmask to Ctx.    }
            { Options already set before are not cleared? }
            f_SSL_CTX_set_options(FSslCtx, LOpts);

            if FSslCipherList <> '' then begin
                if f_SSL_CTX_set_cipher_list(FSslCtx,
                                             PAnsiChar(AnsiString(FSslCipherList))) = 0 then
                    RaiseLastOpenSslError(ESslContextException, TRUE,
                                          'Error loading cipher list');
            end
            else
                raise ESslContextException.Create('Cipher list empty');

            // Session caching stuff
            SslSessCacheModes := GetSslSessCacheModes;

            //if SslSessCacheModes <> [] then   // AG 03/03/06 internal cache is ON by default
                f_SSL_CTX_set_session_cache_mode(FSslCtx, FSslSessCacheModeValue);

            if not (sslSESS_CACHE_NO_INTERNAL_STORE in SslSessCacheModes) then begin
                { Exdata needed in RemoveCallback only }
                if f_SSL_CTX_set_ex_data(FSslCtx, 0, PAnsiChar(Self)) = 0 then
                    RaiseLastOpenSslError(ESslContextException, TRUE,
                                          'SSL_CTX_set_ex_data failed');
                f_SSL_CTX_sess_set_remove_cb(FSslCtx, RemoveSessionCallback);
                if FSslSessionCacheSize <> SSL_SESSION_CACHE_MAX_SIZE_DEFAULT then
                    f_SSL_CTX_sess_set_cache_size(FSslCtx, FSslSessionCacheSize);
            end;
            if (sslSESS_CACHE_SERVER in SslSessCacheModes) then begin
                { Set the timeout for newly created sessions                }
                if FSslSessionTimeout > 0 then
                    f_SSL_CTX_set_timeout(FSslCtx, FSslSessionTimeout);
                { Set session callbacks, ssl server mode only               }
                f_SSL_CTX_sess_set_new_cb(FSslCtx, NewSessionCallback);
                f_SSL_CTX_sess_set_get_cb(FSslCtx, GetSessionCallback);
                if Length(FSslDefaultSessionIDContext) > 0 then
                    if f_SSL_CTX_set_session_id_context(FSslCtx,
                                  @FSslDefaultSessionIDContext[1],
                                  Length(FSslDefaultSessionIDContext)) = 0 then
                        RaiseLastOpenSslError(ESslContextException, TRUE,
                                              'ssl_ctx_set_session_id_context ' +
                                              'failed');
            end;
        except
            if Assigned(FSslCtx) then begin
                f_SSL_CTX_free(FSslCtx);
                FSslCtx := nil;
            end;
            raise
        end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.DeInitContext;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if Assigned(FSslCtx) then begin
            { The context lives as long as there are open sessions associated }
            { even when we called f_SSL_CTX_free(), so some cleanup is needed }
            f_SSL_CTX_set_ex_data(FSslCtx, 0, nil);    //MainFix    // AG 12/25/07
            { It may be a good idea to disable all callbacks as well }
            { before freeing the context pointer, should not hurt,   }
            { otherwise please let me know }
            f_SSL_CTX_sess_set_remove_cb(FSslCtx, nil);             // AG 12/25/07
            f_SSL_CTX_sess_set_new_cb(FSslCtx, nil);                // AG 12/25/07
            f_SSL_CTX_sess_set_get_cb(FSslCtx, nil);                // AG 12/25/07
            f_SSL_CTX_set_default_passwd_cb(FSslCtx, nil);          // AG 12/25/07
            f_SSL_CTX_set_default_passwd_cb_userdata(FSslCtx, nil); // AG 12/25/07
            f_SSL_CTX_free(FSslCtx);
            FSslCtx := nil;
        end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
    FinalizeSsl;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCAFile(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if IcsCompareStr(FSslCAFile, Value) = 0 then
            Exit;
        FSslCAFile := Value;
        if Assigned(FSslCtx) then
            LoadVerifyLocations(FSslCAFile, FSslCAPath);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCAPath(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if IcsCompareStr(FSslCAPath, Value) = 0 then
            Exit;
        FSslCAPath := Value;
        if Assigned(FSslCtx) then
            LoadVerifyLocations(FSslCAFile, FSslCAPath);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCertFile(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if IcsCompareStr(Value, FSslCertFile) = 0 then
            Exit;
        FSslCertFile := Value;
        if Assigned(FSslCtx) then
            LoadCertFromChainFile(FSslCertFile);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCRLFile(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslCRLFile := Value
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCRLPath(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslCRLPath := Value
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslPassPhrase(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslPassPhrase := Value
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslPrivKeyFile(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if (IcsCompareStr(Value, FSslPrivKeyFile) = 0) then
            Exit;
        FSslPrivKeyFile := Value;
        if Assigned(FSslCtx) then
            LoadPKeyFromFile(FSslPrivKeyFile);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslSessionCacheSize(Value: Longint);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslSessionCacheSize := Value
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslSessionTimeout(Value: Longword);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslSessionTimeout := Value
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslVersionMethod(Value: TSslVersionMethod);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslVersionMethod := Value
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslContext.GetSslOptions: TSslOptions; { V7.30 }
var
    Opt: TSslOption;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        Result := [];
        for Opt := Low(TSslOption) to High(TSslOption) do
            if (FSslOptionsValue and SslIntOptions[Opt]) <> 0 then
                Include(Result, Opt);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslOptions(Value: TSslOptions); { V7.30 }
var
    Opt: TSslOption;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslOptionsValue := 0;
        for Opt := Low(TSslOption) to High(TSslOption) do
            if Opt in Value then
                FSslOptionsValue := FSslOptionsValue or SslIntOptions[Opt];
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslSessCacheModes(Value: TSslSessCacheModes); { V7.30 }
var
    SessMode: TSslSessCacheMode;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslSessCacheModeValue := SSL_SESS_CACHE_OFF;
        for SessMode := Low(TSslSessCacheMode) to High(TSslSessCacheMode) do
            if SessMode in Value then
                FSslSessCacheModeValue := FSslSessCacheModeValue or SslIntSessCacheModes[SessMode];
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslContext.GetSslSessCacheModes: TSslSessCacheModes; { V7.30 }
var
    SessMode: TSslSessCacheMode;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        Result := [];
        for SessMode := Low(TSslSessCacheMode) to High(TSslSessCacheMode) do
            if FSslSessCacheModeValue and SslIntSessCacheModes[SessMode] <> 0 then
                Include(Result, SessMode);

{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslContext.GetSslVerifyFlags: TSslVerifyFlags;
var
    VFlag: TSslVerifyFlag;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        Result := [];
        for VFlag := Low(TSslVerifyFlag) to High(TSslVerifyFlag) do
            if (FSslVerifyFlags and SslIntVerifyFlags[VFlag]) <> 0 then
                Include(Result, VFlag);
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslVerifyFlags(
  const Value: TSslVerifyFlags);
var
    VFlag: TSslVerifyFlag;
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        FSslVerifyFlags := 0;
        for VFlag := Low(TSslVerifyFlag) to High(TSslVerifyFlag) do
            if VFlag in Value then
               FSslVerifyFlags := FSslVerifyFlags or SslIntVerifyFlags[VFlag];
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslCipherList(const Value: String);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if FSslCipherList = Value then
            Exit;   // No change, do nothing
        // Now should check the syntax. Will do later :-)
        FSslCipherList := Value;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslVerifyPeerModes(
    const Value: TSslVerifyPeerModes);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if Value <> FSslVerifyPeerModes then begin
            FSslVerifyPeerModesValue := 0;
            if (SslVerifyMode_NONE in Value) then
                FSslVerifyPeerModesValue := FSslVerifyPeerModesValue or SSL_VERIFY_NONE;
            if (SslVerifyMode_PEER in Value) then
                FSslVerifyPeerModesValue := FSslVerifyPeerModesValue or SSL_VERIFY_PEER;
            if (SslVerifyMode_FAIL_IF_NO_PEER_CERT in Value) then
                FSslVerifyPeerModesValue := FSslVerifyPeerModesValue or SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
            if (SslVerifyMode_CLIENT_ONCE in Value) then
                FSslVerifyPeerModesValue := FSslVerifyPeerModesValue or SSL_VERIFY_CLIENT_ONCE;
            FSslVerifyPeerModes := Value;
        end;

        if not Assigned(FSslCtx) then
            Exit;

        { We may change these settings any time since they won't change active Ssl's }
        if FSslVerifyPeer then begin
            if f_SSL_CTX_get_verify_mode(FSslCtx) <> FSslVerifyPeerModesValue then begin
                f_SSL_CTX_set_verify(FSslCtx, FSslVerifyPeerModesValue, PeerVerifyCallback);
{$IFDEF OPENSSL_VERSION_NUMBER_LESS_THAN_0x00905100L}
                f_SSL_CTX_set_verify_depth(FSslCtx, 1);
{$ELSE}
                f_SSL_CTX_set_verify_depth(FSslCtx, FSslVerifyDepth);
{$ENDIF}
            end;
        end
        else begin
            f_SSL_CTX_set_verify(FSslCtx, 0, nil);
            f_SSL_CTX_set_verify_depth(FSslCtx, 0);
        end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslVerifyPeer(const Value: Boolean);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if Value <> FSslVerifyPeer then begin
            FSslVerifyPeer := Value;
            SetSslVerifyPeerModes(FSslVerifyPeerModes);
        end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslContext.SetSslDefaultSessionIDContext(
    Value: TSslSessionIdContext);
begin
{$IFNDEF NO_SSL_MT}
    Lock;
    try
{$ENDIF}
        if Length(Value) > SSL_MAX_SSL_SESSION_ID_LENGTH then
            SetLength(Value, SSL_MAX_SSL_SESSION_ID_LENGTH);
        if FSslDefaultSessionIDContext <> Value then begin
            FSslDefaultSessionIDContext := Value;
            if Assigned(FSslCtx) and (SSL_SESS_CACHE_SERVER and
               FSslSessCacheModeValue <> 1) then begin
                if Length(Value) > 0 then
                    f_SSL_CTX_set_session_id_context(FSslCtx,
                                                     @Value[1],
                                                     Length(Value))
                else
                    f_SSL_CTX_set_session_id_context(FSslCtx, nil, 0);
            end;
        end;
{$IFNDEF NO_SSL_MT}
    finally
        Unlock
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{procedure TSslContext.SetSslX509Trust(const Value: TSslX509Trust);
begin
    if Value <> FSslX509Trust then begin
        FSslX509Trust := Value;
        if Assigned(FSslCtx) then
            if FSslX509Trust <> ssl_X509_TRUST_NOT_DEFINED then
                if f_SSL_CTX_set_trust(FSslCtx, Integer(FSslX509Trust)) = 0 then
                    raise Exception.Create('Error setting trust');
    end;
end; }



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
{ TX509Stack }
constructor TX509Stack.Create;
begin
    inherited Create;
    FStack := nil;
    FStack := f_sk_new_null;
    FCount := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TX509Stack.Destroy;
begin
    if Assigned(FStack) then begin
        Clear;
        f_sk_free(FStack);
        FStack := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Stack.Add(Cert: PX509): Integer;
begin
    Result := InternalInsert(Cert, f_sk_num(FStack)) - 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Stack.Clear;
begin
     while FCount > 0 do
        Delete(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Stack.SetStack(const Value: PSTACK);
var
    I: Integer;
begin
    Clear;
    if Value <> nil then
        for I := 0 to f_sk_num(Value) - 1 do
            Add(PX509(f_sk_value(Value, I)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Stack.Delete(Index: Integer);
var
    P : PChar;
begin
    P := nil;
    P := f_sk_delete(FStack, Index);
    if P <> nil then begin
        Dec(FCount);
        f_X509_free(PX509(P));
    end else
        raise EX509Exception.Create('Delete failed');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Stack.IndexOf(Cert: PX509): Integer;
begin
    Result := 0;
    while (Result < FCount) and
          (PX509(f_sk_value(FStack, Result)) <> Cert) do
        Inc(Result);
    if Result = FCount then
        Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Stack.Insert(Cert: PX509; Index: Integer);
begin
    InternalInsert(Cert, Index)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Stack.InternalInsert(Cert: PX509; Index: Integer): Integer;
var
    P : PX509;
begin
    if (Index < 0) then
        raise EX509Exception.Create('Invalid index');
    if Cert = nil then
        raise EX509Exception.Create('Cert not assigned');
    P := nil;
    P := f_X509_dup(Cert);  // increment reference count
    if P = nil then
        raise EX509Exception.Create('X509_dup failed');
    Result := f_sk_insert(FStack, PChar(P), Index);
    if Result = 0 then
        f_X509_free(P)
    else
        FCount := Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Stack.GetCert(Index: Integer): PX509;
begin
    Result := PX509(f_sk_value(FStack, Index));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Stack.SetCert(Index: Integer; const Value: PX509);
begin
    if (Index < 0) or (Index >= FCount) then
        raise EX509Exception.Create('Invalid index');
    Delete(Index);
    InternalInsert(Value, Index);
end;

*)
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TX509Base.Create(AOwner: TComponent; X509: Pointer = nil);
begin
    inherited Create(AOwner);
    FPrivateKey := nil;
    AssignDefaults;
    if Assigned(X509) then begin
        InitializeSsl;
        FX509     := f_X509_dup(X509);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TX509Base.Destroy;
begin
    FreeAndNilX509;
    FreeAndNilPrivateKey;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.FreeAndNilX509;
begin
    if Assigned(FX509) then begin
        f_X509_free(FX509);
        FX509 := nil;
        //FSha1Hash := '';
        FSha1Hex  := '';
        FSha1Digest  := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.FreeAndNilPrivateKey;
begin
    if Assigned(FPrivateKey) then begin
        f_EVP_PKEY_free(FPrivateKey);
        FPrivateKey := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.SetX509(X509: Pointer);
begin
    InitializeSsl;
    FreeAndNilX509;
    AssignDefaults;
    if Assigned(X509) then begin
        FX509     := f_X509_dup(X509);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.SetPrivateKey(PKey: Pointer);
begin
    InitializeSsl;
    if Assigned(FPrivateKey) then begin
        f_EVP_PKEY_free(FPrivateKey);
        FPrivateKey := nil;
    end;
    if Assigned(PKey) then
        FPrivateKey := Ics_EVP_PKEY_dup(PKey);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetPublicKey: Pointer;                       {AG 11/08/07}
begin
    if Assigned(FX509) then
        Result := f_X509_get_pubkey(FX509)
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetExtensionCount: Integer;
begin
    if Assigned(FX509) then
        Result := f_X509_get_ext_count(FX509)
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.ExtByName(const ShortName: String): Integer;
var
    Ext     : PX509_EXTENSION;
    Count   : Integer;
    I       : Integer;
    Len     : Integer;
    ExtStr  : PAnsiChar;
    B       : PBIO;
    Nid     : Integer;
begin
    Result := -1;
    if Assigned(FX509) then begin
        Count := GetExtensionCount;
        for I := 0 to Count -1 do begin
            //Ext := nil;
            Ext := f_X509_get_ext(FX509, I);
            if not Assigned(Ext) then
                Continue;
            Nid := f_OBJ_obj2nid(f_X509_EXTENSION_get_object(Ext));
            if Nid <> NID_undef then begin
                ExtStr := f_OBJ_nid2sn(Nid);
                if StrLIComp(ExtStr, PAnsiChar(AnsiString(ShortName)), 255) = 0 then begin
                    Result := I;
                    Exit;
                end;
            end
            else begin // custom extension
                //B := nil;
                B := f_BIO_new(f_BIO_s_mem);
                if Assigned(B) then begin
                    try
                        f_i2a_ASN1_OBJECT(B, f_X509_EXTENSION_get_object(Ext));
                        Len := f_BIO_ctrl(B, BIO_CTRL_PENDING, 0, nil);
                        if Len > 0 then begin
                            GetMem(ExtStr, Len);
                            try
                                f_Bio_read(B, ExtStr, Len);
                                if StrLIComp(ExtStr, PAnsiChar(AnsiString(ShortName)), 255) = 0 then begin
                                    Result := I;
                                    Exit;
                                end;
                            finally
                                FreeMem(ExtStr);
                            end;
                        end;
                    finally
                        f_bio_free(B);
                    end;
                end;
            end;
        end;
    end
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetIssuerOneLine: String;
var
    Str : AnsiString;
begin
    Result := '';
    if not Assigned(FX509) then
        Exit;

    SetLength(Str, 512);
    Str := f_X509_NAME_oneline(f_X509_get_issuer_name(FX509),
                               PAnsiChar(Str),
                               Length(Str));
    SetLength(Str, StrLen(PAnsiChar(Str)));
    Result := String(Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSerialNum: Integer;
begin
    if Assigned(FX509) then
        Result := f_ASN1_INTEGER_get(f_X509_get_serialNumber(FX509))
    else
        Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSha1Hash: AnsiString; { V7.31 }
{ * Deprecated and slow, use GetSha1Digest or GetSha1Hex * }
begin
    if Assigned(FX509) then begin
        SetLength(Result, 20);
        Move(Sha1Digest[0], Pointer(Result)^, 20);
    end
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.Sha1Hash: AnsiString;
{ * Deprecated and slow, use Sha1Digest or Sha1Hex * }
begin
    Result := GetSha1Hash;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSha1Digest: THashBytes20;
var
    Len : Integer;
begin
    if Assigned(FX509) and (FSha1Digest = nil) then begin
        SetLength(FSha1Digest, 20);
        if f_X509_digest(FX509, f_EVP_sha1, @FSha1Digest[0], @Len) = 0 then
        begin
            FSha1Digest := nil;
            RaiseLastOpenSslError(EX509Exception, TRUE);
        end;
    end;
    Result := FSha1Digest;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSha1Hex: String;
begin
    if FSha1Hex = '' then
        FSha1Hex := IcsBufferToHex(Sha1Digest[0], 20);
    Result := FSha1Hex;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.UnknownExtDataToStr(Ext: PX509_Extension) : String;
{var
    B   : PBIO;
    Len : Integer; }
begin
    Result := Asn1ToString(PASN1_STRING(f_X509_EXTENSION_get_data(Ext)));
    {B := f_BIO_new(f_BIO_s_mem);
    if Assigned(B) then begin
        try
            f_ASN1_STRING_print(B, PASN1_STRING(f_X509_EXTENSION_get_data(Ext)));
            Len := f_BIO_ctrl(B, BIO_CTRL_PENDING, 0, nil);
            SetLength(Result, Len);
            if Len > 0 then begin
                f_Bio_read(B, PChar(Result), Len);
                SetLength(Result, StrLen(PChar(Result)));
            end;
        finally
            f_BIO_free(B);
        end;
    end
    else
        Result := '';}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetExtension(Index: Integer): TExtension;
var
    ExtCount : Integer;
    J        : Integer;
    Ext      : PX509_EXTENSION;
    Value    : PAnsiChar;
    Meth     : PX509V3_EXT_METHOD;
    Data     : PAnsiChar;
    Val      : PSTACK;
    NVal     : PCONF_VALUE;
    ext_str  : Pointer;
    B        : PBIO;
    Nid      : Integer;
    ABuf     : AnsiString;
begin
    Result.Critical  := FALSE;
    Result.ShortName := '';
    Result.Value     := '';
    if not Assigned(FX509) then
        Exit;
    ExtCount := ExtensionCount;
    if (Index < 0) or (Index > ExtCount -1) then
        raise EX509Exception.Create('Extension index out of bounds');
    Value   := nil;
    Meth    := nil;
    Val     := nil;
    ext_str := nil;
    //Ext   := nil;
    Ext     := f_X509_get_ext(FX509, Index);
    if not Assigned(Ext) then
        raise EX509Exception.Create('Extension not assigned');
    Result.Critical := f_X509_EXTENSION_get_critical(Ext) > 0;
    Nid := f_OBJ_obj2nid(f_X509_EXTENSION_get_object(Ext));
    if Nid <> NID_undef then
        Result.ShortName := String(StrPas(f_OBJ_nid2sn(Nid)))
    else begin // custom extension
        //B := nil;
        B := f_BIO_new(f_BIO_s_mem);
        if Assigned(B) then begin
            try
                f_i2a_ASN1_OBJECT(B, f_X509_EXTENSION_get_object(Ext));
                J := f_BIO_ctrl(B, BIO_CTRL_PENDING, 0, nil);
                SetLength(ABuf, J);
                if J > 0 then begin
                    f_Bio_read(B, PAnsiChar(ABuf), J);
                    SetLength(ABuf, StrLen(PAnsiChar(ABuf)));
                    Result.ShortName := String(ABuf);
                end;
            finally
                f_bio_free(B);
            end;
        end;
    end;

    try
        Meth := f_X509V3_EXT_get(Ext);
        if Meth = nil then begin
            Result.Value := UnknownExtDataToStr(Ext);
            Exit;
        end;
        Data := Ext^.value^.data;
        if Assigned(Meth^.it) then
            ext_str := f_ASN1_item_d2i(nil,
                                       @Data,
                                       Ext^.value^.length,
                                       ASN1_ITEM_ptr(Meth^.it))
        else
            ext_str := Meth^.d2i(nil, @Data, Ext^.value^.length);

        if not Assigned(ext_str) then begin
            Result.Value := UnknownExtDataToStr(Ext);
            Exit;
        end;

        if Assigned(Meth^.i2s) then begin
            Value := Meth^.i2s(Meth, ext_str);
            if Assigned(Value) then
                Result.Value := String(StrPas(Value));
        end
        else if Assigned(Meth^.i2v) then begin
            Val := Meth^.i2v(Meth, ext_str, nil);
            if not Assigned(Val) then
                Exit;
            J := 0;
            while J < f_sk_num(val) do begin
                NVal := PCONF_VALUE(f_sk_value(Val, J));
                if Length(Result.Value) > 0 then
                    Result.Value := Result.Value + #13#10;
                Result.Value := Result.Value + String(StrPas(NVal^.name));
                if (StrPas(NVal^.value) <> '') and (StrPas(NVal^.name) <> '') then
                    Result.Value := Result.Value + '=';
                Result.Value := Result.Value + String(StrPas(NVal^.value));
                Inc(J);
            end;
        end
        else if Assigned(Meth^.i2r) then begin
            //B := nil;
            B := f_BIO_new(f_BIO_s_mem);
            if Assigned(B) then
                try
                    Meth.i2r(Meth, ext_str, B, 0);
                    J := f_BIO_ctrl(B, BIO_CTRL_PENDING, 0, nil);
                    SetLength(ABuf, J);                          { V7.31 }
                    if J > 0 then begin
                        f_Bio_read(B, PAnsiChar(ABuf), J);
                        SetLength(ABuf, StrLen(PAnsiChar(ABuf)));
                        Result.Value := String(ABuf);
                        { This method separates multiple values by LF } // should I remove this stuff?
                        while (Length(Result.Value) > 0) and
                              (Result.Value[Length(Result.Value)] = #10) do
                            SetLength(Result.Value, Length(Result.Value) -1);
                        Result.Value := StringReplace(Result.Value, #10, #13#10, [rfReplaceAll]);
                    end;
                finally
                    f_bio_free(B);
                end;
        end;
    finally
        if Assigned(Val) then
            f_sk_pop_free(Val, @f_X509V3_conf_free);
        if Assigned(Value) then
            f_CRYPTO_free(Value);
        if Assigned(Meth) and Assigned(ext_str) then
            if Assigned(Meth^.it) then
                f_ASN1_item_free(ext_str, ASN1_ITEM_ptr(Meth^.it))
            else
                Meth^.ext_free(ext_str);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubjectAltName: TExtension;
var
    I : Integer;
begin
    Result.Critical  := FALSE;
    Result.ShortName := '';
    Result.Value     := '';
    I := ExtByName('subjectAltName');
    if I > -1 then
        Result := GetExtension(I);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Changed to return a list, separated by CRLF }
function TX509Base.GetSubjectCName: String;
var
    Subj    : PX509_NAME;
    Entry   : PX509_NAME_ENTRY;
    Asn1    : PASN1_STRING;
    LastPos : Integer;
begin
    Result := '';
{$IFNDEF WIN64}    { V7.81 }
    Entry  := nil; { Make dcc32 happy }
{$ENDIF}
    if not Assigned(FX509) then
        Exit;
    Subj := f_X509_get_subject_name(FX509);
    if Subj <> nil then
    begin
        LastPos := -1;
        repeat
            LastPos := f_X509_NAME_get_index_by_NID(Subj, NID_commonName, LastPos);
            if LastPos > -1 then
                Entry := f_X509_NAME_get_entry(Subj, LastPos)
            else
                Break;
            if Assigned(Entry) then begin
                Asn1 := f_X509_NAME_ENTRY_get_data(Entry);
                if Assigned(Asn1) then
                    Result := Result + Asn1ToString(Asn1) + #13#10;
            end;
        until
            LastPos = -1;

        while (Length(Result) > 0) and (Word(Result[Length(Result)]) in [Ord(#13), Ord(#10)]) do
            SetLength(Result, Length(Result) - 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSubjectOneLine: String;
var
    Str : AnsiString;
begin
    Result := '';
    if not Assigned(FX509) then
        Exit;
    SetLength(Str, 512);
    Str := f_X509_NAME_oneline(f_X509_get_subject_name(FX509),
                               PAnsiChar(Str),
                               Length(Str));
    SetLength(Str, StrLen(PAnsiChar(Str)));
    Result := String(Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetVerifyErrorMsg: String;
begin
    if Assigned(FX509) then
        Result := String(StrPas(f_X509_verify_cert_error_string(FVerifyResult)))
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetFirstVerifyErrorMsg: String;            {05/21/2007 AG}
begin
    if Assigned(FX509) then
        Result := String(StrPas(f_X509_verify_cert_error_string(FFirstVerifyResult)))
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetValidNotAfter: TDateTime;                 {AG 02/06/06}
begin
    if Assigned(FX509) then
        if Asn1ToUTDateTime(f_Ics_X509_get_notAfter(FX509), Result) then
            Exit;
    Result := MinDateTime;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetValidNotBefore: TDateTime;                {AG 02/06/06}
begin
    if Assigned(FX509) then
        if Asn1ToUTDateTime(f_Ics_X509_get_notBefore(FX509), Result) then
            Exit;
    Result := MaxDateTime;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetHasExpired: Boolean;                      {AG 02/06/06}

    function GetCurrentBias : TDateTime;
    const
        MinsPerDay = 1440;
    begin
        Result := IcsGetLocalTimeZoneBias / MinsPerDay;
    end;

    function CompDateTime(const A, B: TDateTime): Integer;
    begin
        if Trunc(A) = Trunc(B) then
            Result := 0
        else if A < B then
            Result := -1
        else
            Result := 1;
    end;

var
    CurUT  : TDateTime;
begin
    CurUT  :=  Now + GetCurrentBias;
    Result := (CompDateTime(CurUT, ValidNotAfter)  = 1) or
                   (CompDateTime(CurUT, ValidNotBefore) = -1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.AssignDefaults;
begin
    FVerifyDepth        := 0;
    //FSha1Hash           := '';
    FSha1Hex            := '';
    FSha1Digest         := nil;
    FVerifyResult       := X509_V_ERR_APPLICATION_VERIFICATION;
    FCustomVerifyResult := X509_V_ERR_APPLICATION_VERIFICATION;
    FFirstVerifyResult  := X509_V_ERR_APPLICATION_VERIFICATION;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.PostConnectionCheck(HostOrIp: String): Boolean;
var
    I      : Integer;
    Ext    : TExtension;
    Li     : TStringList;
    Mask   : TMask;
begin
    Result := FALSE;
    if (not Assigned(FX509)) or (Length(HostOrIp) = 0) then
        Exit;
    Li := TStringList.Create;
    try
        Li.Text := GetSubjectCName;
        for I := 0 to Li.Count - 1 do begin
            if Li[I] <> '' then begin
                Mask := TMask.Create(Li[I]);
                try
                    Result := Mask.Matches(HostOrIP);
                    if Result then Exit;
                finally
                    Mask.Free;
                end;
            end;
        end;

        Ext := GetSubjectAltName;
        if Length(Ext.ShortName) > 0 then begin
            Li.Text := Ext.Value;
            for I := 0 to Li.Count -1 do begin
                if (Pos('IP',  IcsUpperCase(Li.Names[I])) = 1) or
                   (Pos('DNS', IcsUpperCase(Li.Names[I])) = 1) then begin
            {        Mask := TMask.Create(Li.Values[Li.Names[I]]); only checks first name, ignores alternatives  }
                    Mask := TMask.Create(Copy(Li[I], Length(Li.Names[I])+2,999)); { V8.09 }
                    try
                        Result := Mask.Matches(HostOrIP);
                        if Result then Exit;
                    finally
                        Mask.Free;
                    end;
                end;
            end;
        end;
    finally
        Li.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.OpenFileBio(
    const FileName    : String;
    Methode           : TBioOpenMethode): PBIO;
begin
    if (Filename = '') then
        raise EX509Exception.Create('File name not specified');
    if (Methode = bomRead) and (not FileExists(Filename)) then
        raise EX509Exception.Create('File not found "' +
                                          Filename + '"');
    if Methode = bomRead then
        Result := f_BIO_new_file(PAnsiChar(AnsiString(Filename)), PAnsiChar('r+'))
    else
        Result := f_BIO_new_file(PAnsiChar(AnsiString(Filename)), PAnsiChar('w+'));

    if (Result = nil) then
        RaiseLastOpenSslError(EX509Exception, TRUE,
                             'Error on opening file "' + Filename + '"');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.PrivateKeyLoadFromPemFile(const FileName: String;
    const Password: String = '');
var
    PKey    : PEVP_PKEY;
    FileBio : PBIO;
begin
    InitializeSsl;
    FileBio := OpenFileBio(FileName, bomRead);
    try
        PKey := f_PEM_read_bio_PrivateKey(FileBio, nil, nil, PAnsiChar(AnsiString(Password)));
        if not Assigned(PKey) then
            RaiseLastOpenSslError(EX509Exception, TRUE,
                                  'Error reading private key file "' +
                                   Filename + '"');
        try
            if Assigned(FX509) then
                if f_X509_check_private_key(FX509, PKey) < 1 then
                    raise EX509Exception.Create('Certificate and private key ' +
                                                'do not match');
            PrivateKey := PKey;
        finally
            f_EVP_PKEY_free(PKey);
        end;
    finally
        f_bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ todo: Actually the private key is written unprotected }
procedure TX509Base.PrivateKeySaveToPemFile(const FileName: String);
var
    FileBio : PBIO;
begin
    InitializeSsl;
    if not Assigned(FPrivateKey) then
        raise EX509Exception.Create('Private key not assigned');
    FileBio := OpenFileBio(FileName, bomWrite);
    try
        if f_PEM_write_bio_PrivateKey(FileBio, FPrivateKey, nil, nil, 0,
                                      nil, nil) = 0 then
            RaiseLastOpenSslError(EX509Exception, TRUE,
                                  'Error writing private key to file');
    finally
        f_bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.LoadFromPemFile(const FileName: String;
    IncludePrivateKey: Boolean = FALSE; const Password: String = '');
var
    FileBio : PBIO;
begin
    InitializeSsl;
    FileBio := OpenFileBio(FileName, bomRead);
    try
        ReadFromBio(FileBio, IncludePrivateKey, Password);
    finally
        f_bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ todo: Currently private key is written unprotected }
procedure TX509Base.SaveToPemFile(const FileName: String;
    IncludePrivateKey: Boolean = FALSE; AddRawText: Boolean = FALSE);
var
    FileBio : PBIO;
begin
    InitializeSsl;
    FileBio := OpenFileBio(FileName, bomWrite);
    try
        WriteToBio(FileBio, IncludePrivateKey, AddRawText);
    finally
        f_bio_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TX509Base.ReadFromBio(ABio: PBIO; IncludePrivateKey: Boolean = FALSE;
  const Password: String = '');
var
    X     : PX509;
    PKey  : PEVP_PKEY;
begin
    InitializeSsl;
    if not Assigned(ABio) then
        raise EX509Exception.Create('BIO not assigned');
    X := f_PEM_read_bio_X509(ABio, nil, nil, PAnsiChar(AnsiString(Password)));
    if not Assigned(X) then
        RaiseLastOpenSslError(EX509Exception, TRUE,
                              'Error reading certificate from BIO');
    try
        if (not IncludePrivateKey) and Assigned(FPrivateKey) then
            if f_X509_check_private_key(X, FPrivateKey) < 1 then
                raise EX509Exception.Create('Certificate and private key ' +
                                            'do not match');
        if IncludePrivateKey then begin
            f_BIO_ctrl(ABio, BIO_CTRL_RESET, 0, nil);
            PKey := f_PEM_read_bio_PrivateKey(ABio, nil, nil,
                                              PAnsiChar(AnsiString(Password)));
            if not Assigned(PKey) then
                RaiseLastOpenSslError(EX509Exception, TRUE,
                                      'Error reading private key from BIO');
            try
                if f_X509_check_private_key(X, PKey) < 1 then
                    raise EX509Exception.Create('Certificate and private key ' +
                                                'do not match');
                 X509       := X;
                 PrivateKey := PKey;
            finally
                f_EVP_PKEY_free(PKey);
            end;
        end else
            X509 := X;
    finally
        f_X509_free(X);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ todo: Currently private key is written unprotected }
procedure TX509Base.WriteToBio(ABio: PBIO;
  IncludePrivateKey: Boolean = FALSE; AddRawText: Boolean = FALSE);
begin
    InitializeSsl;
    if not Assigned(ABio) then
        raise EX509Exception.Create('BIO not assigned');
    if not Assigned(FX509) then
        raise EX509Exception.Create('X509 not assigned');
    if IncludePrivateKey and not Assigned(FPrivateKey) then
        raise EX509Exception.Create('Private key not assigned');
    if AddRawText and
       (f_X509_print(ABio, FX509) = 0) then
        RaiseLastOpenSslError(EX509Exception, TRUE,
                              'Error writing raw text to BIO');
    if IncludePrivateKey and
       (f_PEM_write_bio_PrivateKey(ABio, FPrivateKey, nil, nil, 0,
                                   nil, nil) = 0) then
        RaiseLastOpenSslError(EX509Exception, TRUE,
                              'Error writing private key to BIO');
    if f_PEM_write_bio_X509(ABio, FX509) = 0 then
        RaiseLastOpenSslError(EX509Exception, TRUE,
                              'Error writing certificate to BIO');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetRawText: String;                        {05/21/2007 AG}
var
    Bio  : PBIO;
    Len  : Integer;
    AStr : AnsiString;
begin
    Result := '';
    if FX509 = nil then Exit;
    Bio := f_BIO_new(f_BIO_s_mem);
    if Assigned(Bio) then
    try
        f_X509_print(Bio, FX509);
        Len := f_BIO_ctrl(Bio, BIO_CTRL_PENDING, 0, nil);
        SetLength(AStr, Len);
        if Len > 0 then begin
            f_Bio_read(Bio, PAnsiChar(AStr), Len);
            SetLength(AStr, StrLen(PAnsiChar(AStr)));
            Result := String(AStr);
        end;
    finally
        f_bio_free(Bio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.GetSelfSigned: Boolean;
begin
    if Assigned(FX509) then
        Result := f_X509_check_issued(FX509, FX509) = 0
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.IssuedBy(ACert: TX509Base): Boolean;
begin
    if Assigned(ACert) and Assigned(ACert.X509) and Assigned(FX509) then
        Result := f_X509_check_issued(ACert.X509, FX509) = 0
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.IssuerOf(ACert: TX509Base): Boolean;
begin
    if Assigned(ACert) and Assigned(ACert.X509) and Assigned(FX509) then
        Result := f_X509_check_issued(FX509, ACert.X509) = 0
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TX509Base.SameHash(const ACert: TX509Base): Boolean;
var
    I : Integer;
    P1, P2 : PInteger;
begin
    if (FX509 <> nil) and (ACert <> nil) and (ACert.X509 <> nil) then begin
        P1 := @ACert.Sha1Digest[0];
        P2 := @Sha1Digest[0];
        for I := 1 to 4 do begin
            if (P1^ <> P2^) then
                Break;
            Inc(P1);
            Inc(P2);
        end;
        if P1^ = P2^ then
            Result := TRUE
        else
            Result := FALSE
    end
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$IFNDEF NO_DEBUG_LOG}
function TCustomSslWSocket.GetMyBioName(B: PBIO) : String;
begin
         if (b = Fibio)   then Result := 'ibio'
    else if (b = Fnbio)   then Result := 'nbio'
    else if (b = Fsslbio) then Result := 'sslbio'
    else                       Result := 'bio';
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_BIO_ctrl_pending(B: PBIO) : integer;
begin
    HandleSslError;
    if b = nil then begin
        Result := 0;
        Exit;
    end;
    Result := f_BIO_ctrl_pending(B);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then begin  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
    Inc(TraceCount);
        DebugLog(loSslInfo, Format('%s BIO_ctrl_pending(%s) = %d   [%d]',
                 [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                 GetMyBioName(b), Result, TraceCount]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DataToString(Buf : Pointer; Len : Integer) : String;
var
    P : PChar;
begin
    P      := PChar(Buf);
    Result := '';
    while Len > 0 do begin
        if Word(P^) in [Ord(#32)..Ord(#126)] then
            Result := Result + P^
        else
            Result := Result + '$' + IntToHex(Ord(P^), 2) + ' ';
        Inc(P);
        Dec(Len);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_BIO_read(B: PBIO; Buf: Pointer; Len: Integer): Integer;
begin
    HandleSslError;
    if b = nil then begin
        Result := 0;
        Exit;
    end;
    Result := f_BIO_read(B, Buf, Len);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then begin  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
    Inc(TraceCount);
        DebugLog(loSslInfo, Format('%s BIO_read(%s, 0x%x, %d) = %d   [%d]',
                       [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                       GetMyBioName(b), INT_PTR(Buf), Len, Result, TraceCount]));
    end
    else if CheckLogOptions(loSslDump) then begin
        Inc(TraceCount);
        DebugLog(loSslDump, Format('%s BIO_read(%s, 0x%x, %d) = %d   [%d] Data:%s',
                       [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                       GetMyBioName(b), INT_PTR(Buf), Len, Result,
                       TraceCount, DataToString(Buf, Result)]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_BIO_ctrl(
    bp: PBIO; Cmd: Integer; LArg: LongInt; PArg: Pointer): LongInt;
{$IFNDEF NO_DEBUG_LOG}
var
    CmdName  : String;
    LArgName : String;
{$ENDIF}
begin
    if bp = nil then begin
        Result := 0;
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then begin  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        LArgName := IntToStr(LArg);
        case (cmd) of
        BIO_CTRL_FLUSH:         CmdName := 'BIO_CTRL_FLUSH';
        BIO_CTRL_PENDING:       CmdName := 'BIO_CTRL_PENDING';
        BIO_C_DO_STATE_MACHINE: CmdName := 'BIO_C_DO_STATE_MACHINE'; // <= 02/01/06 AG
        BIO_C_SET_SSL:
            begin
                CmdName := 'BIO_C_SET_SSL';
                case (larg) of
                BIO_NOCLOSE: LArgName := 'BIO_NOCLOSE';
                end;
            end;
        else
            CmdName := IntToStr(Cmd);
        end;
    end;
{$ENDIF}
    Result := f_BIO_ctrl(bp, Cmd, LArg, PArg);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then begin  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
    Inc(TraceCount);
        DebugLog(loSslInfo, Format('%s BIO_ctrl(%s, %s, %s, 0x%x) = %d   [%d]',
                             [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                             GetMyBioName(bp), CmdName, LArgName, INT_PTR(PArg),
                             Result, TraceCount]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_BIO_ctrl_get_write_guarantee(b: PBIO): Integer;
begin
    HandleSslError;
    if b = nil then begin
        Result := 0;
        Exit;
    end;
    Result := f_BIO_ctrl_get_write_guarantee(b);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then begin  { V5.21 }
    Inc(TraceCount);
        DebugLog(loSslInfo,
                 Format('%s BIO_ctrl_get_write_guarantee(%s) = %d   [%d]',
                 [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2), GetMyBioName(b),
                 Result, TraceCount]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{#$IFDEF SSL_NEVER}
function TCustomSslWSocket.my_BIO_ctrl_get_read_request(b: PBIO): Integer;
begin
    HandleSslError;
    if b = nil then begin
        Result := 0;
        Exit;
    end;
    Result := f_BIO_ctrl_get_read_request(b);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslInfo, Format('%s BIO_ctrl_get_read_request(%s) = %d   [%d]',
                                   [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                                   GetMyBioName(b), Result, TraceCount]));
    end;
{$ENDIF}
end;
{#$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_BIO_write(B: PBIO; Buf: Pointer; Len: Integer): Integer;
begin
    HandleSslError;
    if b = nil then begin
        Result := 0;
        Exit;
    end;
    Result := f_BIO_write(B, Buf, Len);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslInfo, Format('%s BIO_write(%s, 0x%x, %d) = %d   [%d]',
                                   [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                                   GetMyBioName(b),
                                   INT_PTR(Buf),  Len, Result, TraceCount]));
    end
    else if CheckLogOptions(loSslDump) then begin
        Inc(TraceCount);
        DebugLog(loSslDump, Format('%s BIO_write(%s, 0x%x, %d) = %d   [%d] Data:%s',
                                   [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                                   GetMyBioName(b),
                                   INT_PTR(Buf), Len, Result,
                                   TraceCount, DataToString(Buf, Result)]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.HandleSslError;
begin
    FLastSslError := f_ERR_peek_error;
    if FLastSslError = 0 then
        Exit;
    FSslHandshakeErr := FLastSslError; { V8.14 }
    FSslHandshakeRespMsg := String(LastOpenSslErrMsg(TRUE));  { V8.14 }
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslErr) then begin
        Inc(TraceCount);
        DebugLog(loSslErr, Format('%s  %d  [%d] %s',
                                  [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                                   FHSocket, TraceCount,
                                   FSslHandshakeRespMsg]));
    end
    else
        f_ERR_clear_error;
{$ELSE}
    f_ERR_clear_error;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_WSocket_recv(s: TSocket; var Buf: TWSocketData;
    len, flags: Integer): Integer;
begin
    Result := WSocket_recv(s, Buf, Len, Flags);
{$IFDEF POSIX}
    if (not FPaused) and ((Result > -1) or (errno = WSAEWOULDBLOCK)) then
        WSocketSynchronizedEnableReadEvent(Self);
{$ENDIF}
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslInfo, Format('%s Winsock recv( %d, 0x%x, %d, %d) = %d   [%d]',
                                   [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                                   s, INT_PTR(Buf), Len, Flags, Result, TraceCount]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_RealSend(Buf : TWSocketData; Len : Integer) : Integer;
begin
    Result := RealSend(Buf, Len);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslInfo, Format('%s my_RealSend (0x%x, %d, %d) = %d   [%d]',
                                   [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                                   FHSocket,
                                   INT_PTR(Buf), Len, Result, TraceCount]));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.my_BIO_should_retry(b: PBIO): Boolean;
begin
    if b = nil then begin
        Result := FALSE;
        Exit;
    end;
    Result := BIO_should_retry(b);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslInfo, Format('%s BIO_should_retry(%s) = %d   [%d]',
                               [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                               GetMyBioName(b), Ord(Result), TraceCount]))
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomSslWSocket.Create(AOwner: TComponent);
begin
    FSslEnable              := FALSE;
    FSslContext             := nil;
    FSslAcceptableHosts     := TStringList.Create;
    FSslCertChain           := TX509List.Create(nil);
    FX509Class              := TX509Base;
    {
    FSsl                    := nil;
    FSslbio                 := nil;
    FIBIO                   := nil;
    FNBIO                   := nil;

    FSslInitialized         := FALSE;
    FNetworkError           := 0;
    FSslIntShutDown         := 0;
    FSslVerifyResult        := 0;
    }
    FMayTriggerFD_Read      := TRUE;
    FMayTriggerFD_Write     := TRUE;
    FMayTriggerDoRecv       := TRUE;
    FMayTriggerSslTryToSend := TRUE;
    //FCloseCalled            := FALSE;
    //FCloseReceived          := FALSE;

    //FMayInternalSslShut     := TRUE;
    inherited Create(AOwner);
    FSslBufList := TIcsBufferHandler.Create(nil);
    FSslBufList.BufSize := SSL_BUFFER_SIZE;  // 4096              {AG 10/10/07}
{ IFDEF DEBUG_OUTPUT}
//    FSslDebugLevel := ssldbgDump;
{ ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomSslWSocket.Destroy;
begin
    FreeAndNil(FSslPeerCert);
    { Removes TSslContext's free notification in a thread-safe way }
    SetSslContext(nil);
    inherited Destroy;
    FreeAndNil(FSslAcceptableHosts);
    DeleteBufferedSslData;
    FreeAndNil(FSslBufList);
    FreeAndNil(FSslCertChain);
    FinalizeSSL;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.InitializeSsl;
begin
    if FSslInitialized then
        Exit;
    LoadSsl;
    FSslInitialized := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.FinalizeSsl;
begin
    if not FSslInitialized then
        Exit;
    ResetSsl;
    UnloadSsl;
    FSslInitialized := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.Accept: TSocket;
begin
    Result := inherited Accept;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Do_FD_ACCEPT(var Msg: TMessage);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' Do_FD_ACCEPT ' + IntToStr(FHSocket));
{$ENDIF}
    inherited Do_FD_ACCEPT(msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.SocketDataPending : Boolean;
var
    Count : u_long;
begin
    FLastError := WSocket_Synchronized_ioctlsocket(FHSocket, FIONREAD, Count);
    Result := Count > 0;
    if FLastError = SOCKET_ERROR then begin
        FLastError := WSocket_WSAGetLastError;
        if (FLastError > WSABASEERR) and (FLastError <> WSAEWOULDBLOCK) and
           (FLastError <> WSAENOTCONN) then
        else
            FLastError := 0;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                 ' Socket data pending: ' + IntToStr(Count) + ' Err: ' +
                 IntToStr(FLastError) + ' ' + IntToStr(FHSocket));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Do_FD_CLOSE(var Msg: TMessage);
var
    SslStOk : Boolean;
begin
    if (not FSslEnable) or (FSocksState <> socksData) or
       (FHttpTunnelState <> htsData) or
       (not Assigned(FSsl)) then begin
        inherited Do_FD_CLOSE(msg);
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' TCustomSslWSocket.Do_FD_CLOSE error #' +
                      IntToStr(msg.LParamHi) + ' ' + IntToStr(FHSocket));
{$ENDIF}
    if (FState = wsConnecting) or (FHSocket = INVALID_SOCKET) then
        Exit;

    SslStOk := f_SSL_state(FSsl) = SSL_ST_OK;

    if not FCloseCalled then begin
        FCloseCalled := TRUE;
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
            DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' *CloseCalled ' + IntToStr(FHSocket));
{$ENDIF}
    end;
    if FNetworkError = 0 then begin
        { The connection was closed , we need to read as much as we can }
        { as well as process data pending in the SslBio }
        if (SslStOk and (my_BIO_ctrl_pending(FSslbio) > 0)) then begin
            TriggerEvents;
            Exit;
        end
        else if SocketDataPending and (FLastError = 0) then begin
          {$IFDEF POSIX}
            { We'll receive a FD_READ message in Windows only!    }
            { in POSIX we have to trigger a read event explicitly }
            TriggerEvent(sslFdRead, 0);
          {$ENDIF}
            Exit;
        end
        else if (FLastError > WSABASEERR) then
            FNetworkError := FLastError;

        if SslStOk and (FSslIntShutDown < 2) then begin
            FSslBiShutDownFlag := FALSE;
            InternalShutDown(1);
            Exit;
        end
    end;

    if FNetworkError > 0 then begin
        if (msg.LParamHi = 0) then
            msg.LParamHi := FNetworkError;
    end;

    if (not SslStOk) and
       (not (csDestroying in ComponentState)) then begin         // AG 03/03/06
        TriggerSslHandshakeDone(1);
        if (FState = wsConnected) and (FSslIntShutDown < 2) and  // AG 03/03/06
           (msg.LParamHi = 0) then begin                         // AG 03/03/06
            inherited ShutDown(1);                               // AG 03/03/06
        end;
    end;

{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                 ' FCloseInvoked=' + IntToStr(Ord(FCloseInvoked)) + ' ' +
                 IntToStr(FHSocket));
{$ENDIF}
    if (FHSocket <> INVALID_SOCKET) and (not FCloseInvoked) and {AG 12/30/07}
       (not (csDestroying in ComponentState)) then begin   // AG 03/03/06
        FCloseInvoked := TRUE;
        TriggerSessionClosed(msg.LParamHi);
    end;

    FSslEnable := FALSE;
    ResetSsl;

    if FState <> wsClosed then begin
        //inherited ShutDown(1);              // call winsock shutdown
        inherited InternalClose(FALSE, msg.LParamHi);  // close the socket
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Do_FD_CONNECT(var Msg: TMessage);
begin
    {if FSslEnable and (Msg.LParamHi = 0) then
        StartSslHandshake;} // moved to TriggerSessionConnected
    FCloseCalled    := FALSE;
    FSslIntShutDown := 0;
    inherited Do_FD_CONNECT(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.TriggerEvent(Event: TSslEvent; ErrCode: Word): Boolean;
{$IFNDEF NO_DEBUG_LOG}
var
    S : String;
{$ENDIF}
begin
    Result := FALSE;
    if (not FSslEnable) or FPaused then { AG V7.26 FPause condition added }
        Exit;
    { Returns TRUE if a message was posted successfully and the socket isn't paused }
    if not (Event in FPendingSslEvents) then begin
        case Event of
            sslFdRead  :  Result := PostMessage(Handle, FMsg_WM_SSL_ASYNCSELECT,
                                        WParam(FHSocket), IcsMakeLong(FD_READ, ErrCode));  { V8.08 }
            sslFdWrite :  Result := PostMessage(Handle, FMsg_WM_SSL_ASYNCSELECT,
                                        WParam(FHSocket), IcsMakeLong(FD_WRITE, ErrCode)); { V8.08 }
            sslFdClose :  Result := PostMessage(Handle, FMsg_WM_SSL_ASYNCSELECT,
                                        WParam(FHSocket), IcsMakeLong(FD_CLOSE, ErrCode)); { V8.08 }
        end;
        if Result then
            FPendingSslEvents := FPendingSslEvents + [Event];
    end;
{$IFNDEF NO_DEBUG_LOG}
    if Result and CheckLogOptions(loSslInfo) then begin
        case Event of
            sslFdRead  : S := 'sslFdRead ';
            sslFdWrite : S := 'sslFdWrite ';
            sslFdClose : S := 'sslFdClose ';
            else
                S := 'Unknown';
        end;
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                 ' TriggerEvent ' + S + IntToStr(FHSocket));
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.ThreadAttach;
begin
    inherited ThreadAttach;
    { Re-post pending events to the new window }
    if sslFdRead in FPendingSslEvents then begin
        FPendingSslEvents := FPendingSslEvents - [sslFdRead];
        TriggerEvent(sslFdRead, 0);
    end;
    if sslFdWrite in FPendingSslEvents then begin
        FPendingSslEvents := FPendingSslEvents - [sslFdWrite];
        TriggerEvent(sslFdWrite, 0);
    end;
    if sslFdClose in FPendingSslEvents then begin
        FPendingSslEvents := FPendingSslEvents - [sslFdClose];
        TriggerEvent(sslFdClose, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Do_SSL_FD_READ(var Msg: TMessage);
begin
    WSocket_Synchronized_WSAASyncSelect(
                                      {$IFDEF POSIX}
                                        Self,
                                      {$ENDIF}
                                        FHSocket,
                                        Handle,
                                        FMsg_WM_ASYNCSELECT,
                                        FD_WRITE or FD_CLOSE or FD_CONNECT);
    try
        Do_FD_READ(Msg);
    finally
        WSocket_Synchronized_WSAASyncSelect(
                                          {$IFDEF POSIX}
                                            Self,
                                          {$ENDIF}
                                            FHSocket,
                                            Handle,
                                            FMsg_WM_ASYNCSELECT,
                                            FD_READ or FD_WRITE or FD_CLOSE or
                                            FD_CONNECT);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Do_FD_READ(var Msg: TMessage);
var
    Len        : Integer; // How much to receive
    Buffer     : array [0..(SSL_BUFFER_SIZE * 2) -1] of AnsiChar;
    NumRead    : Integer;
    nError     : Integer;
    Res        : Integer;
    PBuf       : TWSocketData;
    Dummy      : Byte;
begin
    if (not FSslEnable) or (FSocksState <> socksData) or
       (FHttpTunnelState <> htsData) then begin
        inherited Do_FD_READ(msg);
        Exit;
    end;

  {$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' TCustomSslWSocket.Do_FD_READ ' + IntToStr(FHSocket));
  {$ENDIF}

    if (FNetworkError > 0) then
        Exit;

    if (f_SSL_state(FSsl) = SSL_ST_OK) and (FSslBioWritePendingBytes < 0) and // <= 12/08/05
       (my_BIO_ctrl_pending(FSslbio) > 0) then begin
      {$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
               ' TriggerDataAvailable (Do_FD_READ_1) ' + IntToStr(FHSocket));
      {$ENDIF}
        TriggerDataAvailable(0);
        Exit;
    end;

    FMayTriggerFD_Read := FALSE;

    { Get number of bytes we can receive and store in the network input bio.  }
    { New call to BIO_ctrl_get_read_request in order to read only the amount  }
    { of data from the socket that is needed to satisfy a read request, if    }
    { any. Without that call I had random errors on bi-directional shutdowns. }
    Len := my_BIO_ctrl_get_read_request(FNBio);
    if Len = 0 then
        Len := my_BIO_ctrl_get_write_guarantee(FNBio);
    if Len > SizeOf(Buffer) then
        Len := SizeOf(Buffer)
    else if Len = 0 then begin
        FMayTriggerFD_Read := TRUE;
        TriggerEvents;
        Exit;
    end;
    // Receive data
    PBuf := @Buffer[0];
    NumRead := my_WSocket_recv(FHSocket, PBuf, Len, 0);
    if (NumRead > 0) then begin
        // Store it in the network input bio and process data
        my_BIO_write(FNBio, @Buffer, NumRead);
        my_BIO_ctrl(FNBio, BIO_CTRL_FLUSH, 0, nil);
        // Look if input data was valid.
        // We may not call BIO_read if a write operation is pending !!
        if (FSslBioWritePendingBytes < 0) then begin
            Res := my_BIO_read(FSslBio, @Dummy, 0); //Pointer(1)
            if Res < 0 then begin
                if not my_BIO_should_retry(FSslBio) then begin
                    HandleSslError;
                    if (not FExplizitSsl) or
                       (f_SSL_state(FSsl) <> SSL_ST_OK) then begin
                        WSocket_WSASetLastError(WSAECONNABORTED);
                        FNetworkError := WSAECONNABORTED;
                        FLastError    := WSAECONNABORTED; //XX
                        TriggerEvent(sslFdClose, 0);
                    end
                    else begin
                        WSocket_WSASetLastError(WSAEWOULDBLOCK);
                        FLastError := WSAEWOULDBLOCK; //XX
                        FSslEnable := False;
                        ResetSsl;
                        if FSslIntShutDown < 2 then
                            TriggerSslShutDownComplete(FLastSslError);
                    end;
                  {$IFNDEF NO_DEBUG_LOG}
                    if CheckLogOptions(loSslInfo) then  { V5.21 }
                        DebugLog(loSslInfo,
                                IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                                 ' NetworkError #' + IntToStr(FNetworkError));
                  {$ENDIF}
                    Exit;
                end
                else begin
                    FMayTriggerDoRecv := TRUE;
                    WSocket_WSASetLastError(WSAEWOULDBLOCK);
                    FLastError := WSAEWOULDBLOCK; //XX
                end;
            end
            else if (FSslVersNum >= SSL3_VERSION) then begin // Doesn't work in SSLv2 - 12/06/05
                if f_SSL_get_Error(FSSL, Res) = SSL_ERROR_ZERO_RETURN then begin
                { SSL closure alert received }
                    if FSslState < sslInShutdown then begin
                        FSslState := sslInShutdown;
                        { V7.80 }
                        if (not FSslBiShutDownFlag) then // May be set later in the message handler if a SSL bi-shutdown message is pending
                        begin
                            SslShutDownAsync(1); // If a SSL bi-shutdown is pending this will be ignored in the message handler
                            WSocket_WSASetLastError(WSAEWOULDBLOCK);
                            FLastError := WSAEWOULDBLOCK;
                            Exit;
                        end;
                        { / V7.80 }
                    end;
                    if (not FSslBiShutDownFlag) and (FSslIntShutDown = 2) then
                        TriggerEvent(sslFdClose, FNetWorkError);
                end;
                WSocket_WSASetLastError(WSAEWOULDBLOCK);
                FLastError := WSAEWOULDBLOCK; //XX
                FMayTriggerDoRecv := TRUE;
            end;
        end
        else begin
            FMayTriggerDoRecv := TRUE;
            WSocket_WSASetLastError(WSAEWOULDBLOCK);
            FLastError := WSAEWOULDBLOCK; //XX
          {$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslInfo) then  { V5.21 }
               DebugLog(loSslInfo, 'SslBio write operation pending: ' +
                                        IntToStr(FSslBioWritePendingBytes));
          {$ENDIF}
        end;
    end else
    if Numread = 0 then begin
        if FState = wsconnected then begin
            TriggerEvent(sslFdClose, msg.LParamHi);
        end;
    end
    else if Numread = SOCKET_ERROR then begin
        nError := WSocket_WSAGetLastError;
        if (nError > WSABASEERR) and (nError <> WSAEWOULDBLOCK) and
           (nError <> WSAENOTCONN) then begin
            FNetworkError := nError;
            FLastError    := FNetworkError;
            TriggerEvent(sslFdClose, 0);
          {$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslErr) then  { V5.21 }
                DebugLog(loSslErr, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                         ' NetworkError #' + IntToStr(FNetworkError));
          {$ENDIF}
            Exit;
        end;
    end;

    if (f_SSL_state(FSsl) = SSL_ST_OK) {(FSslState >= sslEstablished)} and
       (FSslBioWritePendingBytes < 0) and // <= 12/08/05
       (my_BIO_ctrl_pending(FSslbio) > 0) then begin
      {$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' TriggerDataAvailable (Do_FD_READ_2) ' +
                     IntToStr(FHSocket));
      {$ENDIF}
        TriggerDataAvailable(0);
    end;

    if (FSslIntShutDown = 1) and SslShutDownCompleted(FShutDownHow) then begin
        if not FSslBiShutDownFlag then begin
            TriggerEvent(sslFdClose, 0);
            Exit;
        end;
    end;

    TriggerEvents;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Do_FD_WRITE(var Msg: TMessage);
var
    Len        : Integer;    // How much to send
    Buffer     : array [0..16383] of AnsiChar;
    NumRead    : Integer;
    NumSent    : Integer;
    Err        : Longword;
begin
    if (not FSslEnable) or (FSocksState <> socksData) or
       (FHttpTunnelState <> htsData) then begin
        inherited Do_FD_WRITE(msg);
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' TCustomSslWSocket.Do_FD_WRITE ' + IntToStr(FHSocket));
{$ENDIF}
    if (FNetworkError > 0) then
        Exit;

    FMayTriggerFD_Write := FALSE;

    // Send encrypted data in the send buffer
    inherited TryToSend;
    // May have closed the connection
    if (FHSocket = INVALID_SOCKET) then begin
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                                          ' INVALID_SOCKET');
{$ENDIF}
        Exit;
    end
    else if not bAllSent then begin
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                               ' * Not bAllSent ' + IntToStr(FHSocket));
{$ENDIF}
        FMayTriggerFD_Write := TRUE;
        { AG 01/10/07 - Outcommented next line in order to avoid too many  }
        { socket errors WSAEWOULDBLOCK, this is experimental and needs to  }
        { be tested heavily! If you experience strange hangs uncomment it  }
        { again.                                                           }
        //TriggerEvents;
        Exit;  // We have not sent everything
    end;

    // Send the data waiting in the network bio
    Len     := my_BIO_ctrl_pending(FNBio);
    NumRead := my_BIO_read(FNBio, @Buffer, Len);
    if NumRead <= 0 then
        FMayTriggerFD_Write := TRUE;

    while (NumRead > 0) do begin
        NumSent := my_RealSend(@Buffer, NumRead{len});
        if NumSent = 0 then begin
            if FState = wsconnected then
                TriggerEvent(sslFdClose, 0);
        end;
        if (NumSent = SOCKET_ERROR) or (NumSent < NumRead) then begin
            if NumSent = SOCKET_ERROR then begin
                Err := WSocket_WSAGetLastError;
                if (Err > WSABASEERR) and
                   (Err <> WSAEWOULDBLOCK) and
                   (Err <> WSAENOTCONN) then begin
                    FNetworkError := Err;
                    FLastError    := Err; //XX
                    TriggerEvent(sslFdClose, 0);
{$IFNDEF NO_DEBUG_LOG}
                    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                        DebugLog(loSslInfo,
                            IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                            ' Winsock Error ' + WSocketErrorDesc(FNetworkError) +
                            ' ' + IntToStr(FHSocket));
{$ENDIF}
                    Exit;
                end
                else
                    NumSent := 0;
            end;
            bAllSent := FALSE;
            inherited PutDataInSendBuffer(@Buffer[NumSent], NumRead - NumSent);
        end;
        if NumSent = 0 then
            break;

        Len := my_BIO_ctrl_pending(FNBio);
        if Len = 0 then begin
            FMayTriggerFD_Write := TRUE;
            break;
        end;
        NumRead := my_BIO_read(FNBio, @Buffer, Len);
        if Numread <= 0 then
            FMayTriggerFD_Write := TRUE;
    end;

    if (f_Ssl_State(FSsl) = SSL_ST_OK) then begin                    // <= 12/08/05
        if not bSslAllSent then
            TryToSend
        (* else if {bSslAllSent and} bAllSent and (my_BIO_ctrl_pending(FNBio)= 0) and
            (FSslState = sslEstablished) {FSslEstablished} then begin *)
        else if bAllSent and // condition replaced, note check in front 12/08/05
            (my_BIO_ctrl_pending(FNBio)= 0) and FSendPending then begin
            //Inc(FTriggerCount); //test
            FSendPending := FALSE;
            TriggerDataSent(0);
        end;
    end;

    if (FSslIntShutDown = 1) and SslShutDownCompleted(FShutDownHow) then begin
        if not FSslBiShutDownFlag then begin
            TriggerEvent(sslFdClose, 0);
            Exit;
        end;
    end;

    TriggerEvents;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.DoRecv(
    var Buffer : TWSocketData;
    BufferSize : Integer;
    Flags      : Integer): Integer;
var
    Numread : Integer;
begin
    if (not FSslEnable) or (FSocksState <> socksData) or
       (FHttpTunnelState <> htsData) then begin
        Result := inherited DoRecv(Buffer, BufferSize, Flags);
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' TCustomSslWSocket.DoRecv ' + IntToStr(FHSocket));
{$ENDIF}
    if FNetworkError > 0 then begin
        if (my_BIO_ctrl_pending(FSslbio) > 0) and
           (FSslIntShutDown = 0) then begin
            Result := my_BIO_read(FSslbio, @Buffer, BufferSize);
            Exit;
        end;
        WSocket_WSASetLastError(FNetworkError);
        FLastError := FNetworkError; //XX
        Result     := SOCKET_ERROR;
        Exit;
    end;
    if FSslIntShutDown = 1 then begin
        WSocket_WSASetLastError(WSAESHUTDOWN);
        FLastError := WSAESHUTDOWN; //XX
        Result := SOCKET_ERROR;
        Exit;
    end;
    if (BufferSize = 0) then begin
        Result := 0;
        Exit;
    end;
    if (f_SSL_state(FSsl) <> SSL_ST_OK) or                //<= 01/01/06 AG
       (my_BIO_ctrl_pending(FSslbio) = 0) then begin
        if FState = wsclosed then begin
            Result := 0;
            Exit;
        end;
        if FCloseCalled then begin
            TriggerEvent(sslFdClose, 0);
            Result := 0;
            Exit;
        end
        else if FState <> wsconnected then begin
            WSocket_WSASetLastError(FNetworkError);
            FLastError := FNetworkError; //XX
            Result := SOCKET_ERROR;
            Exit;
        end;
        FMayTriggerDoRecv := TRUE;
        TriggerEvents;
        WSocket_WSASetLastError(WSAEWOULDBLOCK);
        FLastError := WSAEWOULDBLOCK; //XX
        Result := SOCKET_ERROR;
        Exit;
    end;

    Numread := my_BIO_read(FSslbio, Buffer, BufferSize);

    if Numread = 0 then begin
        if (FSslVersNum >= SSL3_VERSION) and { V7.80 }
           (f_SSL_get_error(FSsl, Numread) = SSL_ERROR_ZERO_RETURN) then begin
            { SSL closure alert received }
            if FSslState < sslInShutdown then begin
                FSslState := sslInShutdown;
                { V7.80 }
                if (not FSslBiShutDownFlag) then // May be set later in the message handler if a SSL bi-shutdown message is pending
                begin
                    SslShutDownAsync(1); // If SSL bi-shutdown is pending this will be ignored in the message handler
                    WSocket_WSASetLastError(WSAEWOULDBLOCK);
                    FLastError := WSAEWOULDBLOCK;
                    Result := SOCKET_ERROR;
                    Exit;
                end;
                { / V7.80 }
            end;
        end;
        FMayTriggerDoRecv := TRUE;
        TriggerEvents;
        WSocket_WSASetLastError(WSAEWOULDBLOCK);
        FLastError := WSAEWOULDBLOCK; //XX
        Result := SOCKET_ERROR;
        Exit;
    end;

    if Numread < 0 then begin
        if not my_BIO_should_retry(FSslbio) then begin
            HandleSslError;
            if not FExplizitSsl then begin
                FNetworkError := WSAECONNABORTED;
                WSocket_WSASetLastError(WSAECONNABORTED);
                FLastError := WSAECONNABORTED; //XX
                TriggerEvent(sslFdClose, 0);
                Result := SOCKET_ERROR;
            end
            else begin
                WSocket_WSASetLastError(WSAEWOULDBLOCK);
                FLastError := WSAEWOULDBLOCK; //XX
                Result     := SOCKET_ERROR;
                FSslEnable := FALSE;
                ResetSsl;
                if FSslIntShutDown < 2 then
                    TriggerSslShutDownComplete(FLastSslError);
            end;
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                                 ' NetworkError #' + IntToStr(FNetworkError));
{$ENDIF}
            Exit;
        end
        else begin
            FMayTriggerDoRecv := TRUE;
            // FMayTriggerFD_READ := TRUE;     // <= 12/14/05 ???
            TriggerEvents;
            WSocket_WSASetLastError(WSAEWOULDBLOCK);
            FLastError := WSAEWOULDBLOCK; //XX
            Result     := SOCKET_ERROR;
            Exit;
        end;
    end;
    FMayTriggerDoRecv := TRUE;
    TriggerEvents;
    Result := Numread;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.GetRcvdCount : LongInt;
begin
    if csDesigning in ComponentState then begin
        Result := -1;
        Exit;
    end;

    if (not FSslEnable) or (FSocksState <> socksData) or
       (FHttpTunnelState <> htsData) then
        Result := inherited GetRcvdCount
    else
        Result := my_BIO_ctrl_pending(FSslbio);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.GetSslPeerCert: TX509Base;
begin
    if not Assigned(FSslPeerCert) then
        FSslPeerCert := FX509Class.Create(nil);
    Result := FSslPeerCert;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
procedure TCustomSslWSocket.DoSslShutdown;
var
    ErrCode : Integer;
begin
    if Assigned(FSsl) then begin
        ErrCode := f_SSL_get_shutdown(FSsl);
        if ErrCode = 0 then begin // nobody has done anything so far
            FSSL_State := sslShutdown;
            ErrCode    := f_SSL_shutdown(FSsl);
            if ErrCode = 0 then begin
                f_SSL_shutdown(FSsl);
                OutputDebugString(IntToHex(Integer(Self), 8) +
                                  ' SSL_shutdown has to be called once more');
                // The shutdown is not yet finished. SSL_shutdown() has to be called
                // for a second time, if a bidirectional shutdown shall be performed.
            end;
        end;
    end;
end;

*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Close;
begin
    if not FCloseCalled then begin
        FCloseCalled       := TRUE;
        FSslBiShutDownFlag := FALSE;
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
            DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' *CloseCalled ' + IntToStr(FHSocket));
{$ENDIF}
    end;

    if FSslEnable and Assigned(FSsl) and
      (f_SSL_state(FSsl) = SSL_ST_OK) and (my_BIO_ctrl_pending(FSslbio) > 0) then
        TriggerEvents
    else
       inherited Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Shutdown(How : Integer);
begin
    if (FHSocket = INVALID_SOCKET) then
        Exit;
    if (not FSslEnable) or (not Assigned(FSsl)) then begin
        inherited ShutDown(How);
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                 ' TCustomSslWSocket.ShutDown ' + IntToStr(How) + ' ' +
                 IntToStr(FHSocket));
{$ENDIF}
    FShutDownHow       := How;
    FSslBiShutDownFlag := FALSE;
    InternalShutDown(How);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.SslShutdownCompleted(How: Integer) : Boolean;
var
    Buffer  : array [0..1023] of Char;
    NumRead : Integer;
begin
    if (not FSslEnable) or (not Assigned(FSsl)) then begin
        Result := TRUE;
        if FSslIntShutDown < 2 then begin
            FSslBiShutDownFlag := FALSE;
            FSslIntShutDown    := 2;
            if Assigned(FSsl) then
                ResetSsl;
            TriggerSslShutDownComplete(FNetworkError);
        end;
        if FState <> wsClosed then begin
            inherited ShutDown(How);
            inherited InternalClose(FALSE, 0);
        end;
        Exit;
    end;
    Result := FALSE;
{   Manifest constants for Shutdown
    SD_RECEIVE                = 0;  // disables receives
    SD_SEND                   = 1;  // disables sends, Use this one for graceful close
    SD_BOTH                   = 2;  // disables both sends and receives
}
    try
        if (FNetworkError = 0) and
           ((FSslIntShutDown = 0) or (not bAllSent) {or (not bSslAllSent)}) then
            Exit;

        { A bi-directional SSL shutdown w/o socket  close. We need to       }
        { receive peer's shutdown notification before the SSL can be killed }
        { and communication may continue in plaintext data.                 }
        if FSslBiShutDownFlag and (FNetworkError = 0) then begin
            NumRead := f_SSL_get_shutdown(FSsl);
            if (NumRead and SSL_RECEIVED_SHUTDOWN = 0) or
               (NumRead and SSL_SENT_SHUTDOWN = 0) then
                Exit;
        end;

        // Empty read buffer
        repeat
            Numread := f_BIO_read(FSslbio, @Buffer, SizeOf(Buffer));
        until numread <= 0;

        if (my_BIO_ctrl_pending(FNbio) > 0) and (FNetworkError = 0) then
            Exit
        else begin  // SSL ShutDown is finished
            Result := TRUE;
            if FSslIntShutDown < 2 then begin
                FSslIntShutDown := 2;
                FSslState := sslShutdownComplete;
                if FSslBiShutDownFlag then begin
                    FSslEnable := FALSE;
                    ResetSsl;
                end;
                TriggerSslShutDownComplete(FNetworkError);
                if not FSslBiShutDownFlag then begin
                    inherited ShutDown(FShutDownHow);
                    //if not FCloseCalled then
                        TriggerEvent(sslFdClose, 0);
                end;
            end;
        end;
    finally
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then
            DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' SslShutdownCompleted *'+ IntToStr(Ord(Result)) + '* ' +
                     IntToStr(FHSocket));
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.InternalShutdown(How: Integer);
var
    Res, Err : Integer;
begin
    // Apache server www.dfn-pca.de:443
{   Manifest constants for Shutdown
    SD_RECEIVE                = 0;  // disables receives
    SD_SEND                   = 1;  // disables sends, Use this one for graceful close
    SD_BOTH                   = 2;   //disables both sends and receives
}
    if (FHSocket = INVALID_SOCKET) or
       (not FSslEnable) or (not Assigned(FSsl) or
       (FSslState = sslShutdownComplete)) then begin
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                 ' SslInternalShutdown ' + IntToStr(FHSocket));
{$ENDIF}

    if FSslIntShutDown = 0 then
        FSslIntShutDown := 1
    else begin
        if not SslShutDownCompleted(How) then begin
            TriggerEvents;
            WSocket_Synchronized_WSASetLastError(WSAEWOULDBLOCK);
            FLastError := WSAEWOULDBLOCK;
        end;
        Exit;
    end;

    if not FSslBiShutDownFlag then begin
        Res := f_SSL_get_shutdown(FSsl);
        if (Res and SSL_RECEIVED_SHUTDOWN = 0) then // not yet received a notify
            Res := f_SSL_shutdown(FSsl)             // send our notify
        else
            Res := 1;
    end
    else
        Res := f_SSL_shutdown(FSsl);             // send our notify

    if Res >= 0 then begin
        if Res = 0 then begin  // we have not yet received a notify from the peer
            FSslState := sslInShutdown;
            f_SSL_shutdown(FSsl);
        end;
        if not SslShutDownCompleted(How) then begin
            FMayTriggerFD_Write := TRUE;
            TriggerEvents;
            WSocket_Synchronized_WSASetLastError(WSAEWOULDBLOCK);
            FLastError := WSAEWOULDBLOCK {WSAECONNABORTED}; //XX  AG //03/03/06
        end;
    end
    else begin
        Err := f_SSL_get_error(FSsl, -1);
        if (Err = SSL_ERROR_WANT_READ) or
           (Err = SSL_ERROR_WANT_WRITE) then begin
            TriggerEvents;
            WSocket_Synchronized_WSASetLastError(WSAEWOULDBLOCK);
            FLastError := WSAEWOULDBLOCK {WSAECONNABORTED}; //XX  AG //03/03/06
        end
        else if not SslShutDownCompleted(How) then begin
            TriggerEvents;
            WSocket_Synchronized_WSASetLastError(WSAEWOULDBLOCK);
            FLastError := WSAEWOULDBLOCK {WSAECONNABORTED}; //XX  AG //03/03/06
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.WMTriggerSslShutDownComplete(var msg: TMessage);
begin
    TriggerSslShutDownComplete(msg.LParam);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.TriggerSslShutDownComplete(ErrCode: Integer);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then
        DebugLog(loSslInfo,
                 Format('%s TriggerSslShutDownComplete(%d) %d',
                        [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                        ErrCode, FHSocket]));
{$ENDIF}
    if Assigned(FOnSslShutdownComplete) then
        FOnSslShutdownComplete(Self, FSslBiShutDownFlag, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
// Return FALSE if check failed and connection must be close
function TCustomSslWSocket.PostConnectionCheck : Integer;
var
    Cert     : PX509;
    ExtCount : Integer;
    I, J     : Integer;
    Ext      : PX509_EXTENSION;
    ExtStr   : PChar;
    Meth     : PX509V3_EXT_METHOD;
    Data     : PChar;
    Val      : PSTACK;
    NVal     : PCONF_VALUE;
    Subj     : PX509_NAME;
    HostBuf  : String;
begin
    Result := X509_V_ERR_APPLICATION_VERIFICATION;
    Cert   := f_SSL_get_peer_certificate(FSsl);
    if Cert = nil then begin
        OutputDebugString('PostConnectionCheck: No certificate');
        Exit;
    end;

    ExtCount := f_X509_get_ext_count(Cert);
    if ExtCount > 0 then begin
        for I := 0 to ExtCount - 1 do begin
            Ext    := f_X509_get_ext(Cert, I);
            ExtStr := f_OBJ_nid2sn(f_OBJ_obj2nid(f_X509_EXTENSION_get_object(Ext)));
            if StrLIComp(ExtStr, 'subjectAltName', 255) = 0 then begin
                // UNTESTED CODE
                Meth := f_X509V3_EXT_get(Ext);
                if Meth = nil then
                    break;
                if not Assigned(Meth.i2v) then
                    Break;
                Data := Ext^.value^.data;
                Val  := Meth^.i2v(Meth,
                                  meth^.d2i(nil, @Data, Ext^.value^.length),
                                  nil);
                J := 0;
                while J < f_sk_num(val) do begin
                    NVal := PCONF_VALUE(f_sk_value(Val, J));
                    if (StrLIComp(NVal^.name, 'DNS', 255) = 0) then begin
                        HostBuf := StrPas(NVal^.value);
                        if FSslAcceptableHosts.IndexOf(HostBuf) >= 0 then begin
                            Result := FSslVerifyResult;
                            f_X509_free(Cert);
                            Exit;
                        end;
                    end;
                    Inc(J);
                end;
                // END OF UNTESTED CODE
            end;
        end;
    end;

    Subj := f_X509_get_subject_name(Cert);
    if Subj <> nil then begin
        SetLength(HostBuf, 256);
        if f_X509_NAME_get_text_by_NID(Subj, NID_commonName, @HostBuf[1], Length(HostBuf)) > 0 then begin
            SetLength(HostBuf, StrLen(@HostBuf[1]));
            if FSslAcceptableHosts.IndexOf(HostBuf) < 0 then begin
                // Not found
                f_X509_free(Cert);
                Exit;
            end
        end;
    end;

    Result := FSslVerifyResult;
    f_X509_free(Cert);
end;

 *)
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.SetSslAcceptableHosts(Value : TStrings);
begin
    FSslAcceptableHosts.Assign(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.SetSslContext(const Value: TSslContext);
begin
    if Value <> FSslContext then begin
        if FSslContext <> nil then
            FSslContext.RemoveFreeNotification(Self);
        if Value <> nil then
            Value.FreeNotification(Self);
        FSslContext := Value;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Dup(NewHSocket: TSocket);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' Dup accepting accepted socket = ' +
                      IntToStr(NewHSocket));
{$ENDIF}
    inherited Dup(NewHSocket);
    ChangeState(wsConnected);
    if FSslEnable then begin
        try
            case FSslMode of
                sslModeServer : AcceptSslHandshake;
                sslModeClient : StartSslHandshake;
            else
                raise Exception.Create('Invalid SslMode');
            end;
        except
            on E : Exception do begin
                FSslEnable := FALSE;
                ResetSSL;
                inherited InternalClose(FALSE, WSAECONNABORTED);
                HandleBackGroundException(E);
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSslRenegotiationDisallowed(Obj: TCustomSslWSocket): Boolean;
begin
    Assert((Obj <> nil) and (Obj.FSsl <> nil));
    Result :=
     { In OSSL v0.9.8L and v0.9.8m renegotiation support was disabled   }
     { due to renegotiation vulnerability of the SSL protocol.          }
     ICS_SSL_NO_RENEGOTIATION or // v0.9.8L and v0.9.8m
     (
        (ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_0908N) and
        (
          { In v0.9.8n renegotiation support was re-enabled and RFC5746 }
          { implemented but require the extension as needed.            }
          { It's also possible to enable unsafe legacy renegotiation    }
          { explicitly by setting option                                }
          { sslOpt_ALLOW_UNSAFE_LEGACY_RENEGOTIATION                    }

          { The SSL-connection doesn't support secure renegotiations.   }
          (not Obj.FSslSupportsSecureRenegotiation) and
          { Unsafe legacy renegotiation is not set.                     }
          (f_SSL_get_options(Obj.FSsl) and SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION = 0)
        )
     );
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure InfoCallBack(const ssl: PSSL; Where: Integer; Ret: Integer); cdecl;
var
{$IFNDEF NO_DEBUG_LOG}
    Str : String;
    Pre : String;
    W   : Integer;
    Err : Integer;
{$ENDIF}
    Obj : TCustomSslWSocket;
begin
{$IFNDEF NO_SSL_MT}
    LockInfoCB.Enter;
    try
{$ENDIF}
        // TSslDebugLevel = (ssldbgNone, ssldbgError, ssldbgInfo, ssldbgDump);
        Obj := TCustomSslWSocket(f_SSL_get_ex_data(Ssl, 0));
        if not Assigned(Obj) then
            raise Exception.Create('ICB> Extended data not assigned fatal error!');
        Obj.FSsl_In_CB := TRUE;
        try
{$IFNDEF NO_DEBUG_LOG}
            if Obj.CheckLogOptions(loSslErr) or
               Obj.CheckLogOptions(loSslInfo) then begin

                Pre := IntToHex(INT_PTR(Obj), SizeOf(Pointer) * 2) + ' ICB> ';

                W := Where and (not SSL_ST_MASK);
                if (W and SSL_ST_CONNECT) <> 0 then
                    Str := 'SSL_connect: '
                else if (w and SSL_ST_ACCEPT) <> 0 then
                    Str := 'SSL_accept: '
                else
                    Str := 'undefined: ';

                if ((Where and SSL_CB_LOOP) <> 0) then begin
                    if Obj.CheckLogOptions(loSslInfo) then
                        Obj.DebugLog(loSslInfo, Pre + Str +
                                        String(f_SSL_state_string_long(ssl)));
                end
                else if ((Where and SSL_CB_ALERT) <> 0) and
                        Obj.CheckLogOptions(loSslInfo) then begin
                    if (Where and SSL_CB_READ) <> 0 then
                        Str := 'read '
                    else
                        Str := 'write ';

                    Obj.DebugLog(loSslInfo, Pre + 'SSL3 alert ' + Str +
                                 String(f_SSL_alert_type_string_long(ret)) + ' ' +
                                 String(f_SSL_alert_desc_string_long(ret)));
                end
                else if (Where and SSL_CB_EXIT) <> 0 then begin
                    if Ret = 0 then begin
                        if Obj.CheckLogOptions(loSslInfo) then
                            Obj.DebugLog(loSslInfo, Pre + Str + 'failed in ' +
                                            String(f_SSL_state_string_long(ssl)));
                    end
                    else if Ret < 0 then begin
                        Err := f_ssl_get_error(ssl, Ret);
                      {  if ((Err <> SSL_ERROR_WANT_READ) or
                            (Err <> SSL_ERROR_WANT_WRITE)) then  }
                        if NOT ((Err = SSL_ERROR_WANT_READ) or        { V8.14 only want real errors }
                            (Err = SSL_ERROR_WANT_WRITE)) then begin
                                if Err = SSL_ERROR_SSL then  { V8.14 report proper error }
                                    Obj.HandleSslError
                                else
                                    Obj.DebugLog(loSslInfo, Pre + Str + 'error ' + IntToStr (Err) + { V8.14 actual error }
                                     ' in ' + String(f_SSL_state_string_long(ssl)));
                            end;
                    end;
                end;
            end;
{$ENDIF}
            if (Where and SSL_CB_HANDSHAKE_START) <> 0 then begin
                Obj.FInHandshake   := TRUE;
                Inc(Obj.FHandShakeCount);
                if (Obj.FHandShakeCount > 1) and
                    IsSslRenegotiationDisallowed(Obj) then begin
                //if ICS_SSL_NO_RENEGOTIATION and (Obj.FHandShakeCount > 1) then
                    Obj.CloseDelayed;
                   { todo: We need to handle this much better }
                {$IFNDEF NO_DEBUG_LOG}
                    if Obj.CheckLogOptions(loSslErr) or
                       Obj.CheckLogOptions(loSslInfo) then
                        Obj.DebugLog(loSslInfo, Pre + 'Renegotiaton not supported ' +
                                                'or not allowed. Connection ' +
                                                'closed delayed');
                {$ENDIF}
                end;
                if Obj.FHandShakeCount > 1 then
                    Obj.FSslInRenegotiation := TRUE;
{$IFNDEF NO_DEBUG_LOG}
                if Obj.CheckLogOptions(loSslInfo) then
                    Obj.DebugLog(loSslInfo, Pre + 'SSL_CB_HANDSHAKE_START');
{$ENDIF}
            end
            else if (Where and SSL_CB_HANDSHAKE_DONE) > 0 then begin
                Obj.FInHandshake     := FALSE;
                Obj.FHandshakeDone   := TRUE;
                //PostMessage(Obj.FWindowHandle, WM_TRIGGER_SSLHANDSHAKEDONE, 0, 0);
                //Obj.TriggerSslHandshakeDone(Err);
                //Obj.FSSLState  := sslEstablished;
{$IFNDEF NO_DEBUG_LOG}
                if Obj.CheckLogOptions(loSslErr) or
                   Obj.CheckLogOptions(loSslInfo) then begin
                    Err := f_SSL_get_verify_result(Ssl);
                    if Obj.CheckLogOptions(loSslInfo) or (Err <> X509_V_OK) then
                       Obj.DebugLog(loSslInfo, Pre + 'SSL_CB_HANDSHAKE_DONE, Error ' +
                                    String(StrPas(f_X509_verify_cert_error_string(Err))));   { V8.14 real literal }
                                  //   'Error: ' + IntToStr(Err));
                end;
{$ENDIF}
            end
        finally
            Obj.FSsl_In_CB := FALSE;
            if Obj.FHSocket = INVALID_SOCKET then
                PostMessage(Obj.FWindowHandle, Obj.FMsg_WM_RESET_SSL, 0, 0);
        end;
{$IFNDEF NO_SSL_MT}
    finally
        LockInfoCB.Leave;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF OPENSSL_NO_TLSEXT}
function ServerNameCallback(SSL: PSSL; var ad: Integer; arg: Pointer): Longint; cdecl;
var
    Ws : TCustomSslWSocket;
    PServerName : PAnsiChar; // Pointer to UTF-8 string
    Ctx : TSslContext;
    Err : TTlsExtError;
begin
{$IFNDEF NO_SSL_MT}
    _EnterCriticalSection(LockServerNameCB);
    try
{$ENDIF}
    PServerName := f_SSL_get_servername(SSL, TLSEXT_NAMETYPE_host_name);
    if Assigned(PServerName) then
    begin
        Ws := TCustomSslWSocket(f_SSL_get_ex_data(SSL, 0));
        if Assigned(Ws) and Assigned(Ws.FOnSslServerName) then
        begin
            Ws.FSsl_In_CB := TRUE;
            try
                Ws.FSslServerName := String(UTF8String(PServerName));
                Ctx := nil;
                Err := teeAlertWarning; //SSL_TLSEXT_ERR_ALERT_WARNING
                Ws.FOnSslServerName(Ws, Ctx, Err);
                { Do not switch context if not initialized }
                if Assigned(Ctx) and Assigned(Ctx.FSslCtx) then
                begin
                    if Ws.SslContext <> Ctx then
                    begin
                        { Clear the options inherited from current Ctx.    }
                        { Not sure whether it is required, shouldn't hurt. }
                        f_SSL_clear_options(SSL,
                                 f_SSL_CTX_get_options(Ws.SslContext.FSslCtx));
                        Ws.SslContext := Ctx;
                        f_SSL_set_SSL_CTX(SSL, Ctx.FSslCtx);
                        f_SSL_set_options(SSL, f_SSL_CTX_get_options(ctx.FSslCtx));
                        f_SSL_CTX_set_tlsext_servername_callback(Ctx.FSslCtx,
                                                         @ServerNameCallBack);
                    {$IFNDEF NO_DEBUG_LOG}
                        if Ws.CheckLogOptions(loSslInfo) then
                            Ws.DebugLog(loSslInfo,
                                    'SNICB> Switching context server_name "'
                                    + Ws.FSslServerName + '"');
                    {$ENDIF}
                    end;
                    Result := SSL_TLSEXT_ERR_OK;
                end
                else
                    Result := Ord(Err);
            finally
                Ws.FSsl_In_CB := FALSE;
                if Ws.FHSocket = INVALID_SOCKET then
                    PostMessage(Ws.FWindowHandle, Ws.FMsg_WM_RESET_SSL, 0, 0);
            end;
        end
        else
            Result := SSL_TLSEXT_ERR_OK;// SSL_TLSEXT_ERR_NOACK;
    end
    else
        Result := SSL_TLSEXT_ERR_OK;
{$IFNDEF NO_SSL_MT}
    finally
        _LeaveCriticalSection(LockServerNameCB);
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF NEVER}
procedure TlsExtension_cb(SSL: PSSL; client_server: Integer; type_: Integer;
  data: PAnsiChar; len: Integer; arg: Pointer); cdecl;
var
    ExtName : String;
    CS : String;
    Ws : TCustomSslWSocket;
begin
    Ws := TCustomSslWSocket(f_SSL_get_ex_data(Ssl, 0));
    case type_ of
      TLSEXT_TYPE_server_name :
          ExtName := 'server name';
      TLSEXT_TYPE_max_fragment_length :
          ExtName := 'max fragment length';
      TLSEXT_TYPE_client_certificate_url :
          ExtName := 'client certificate URL';
      TLSEXT_TYPE_trusted_ca_keys :
          ExtName := 'trusted CA keys';
      TLSEXT_TYPE_truncated_hmac :
          ExtName := 'truncated HMAC';
      TLSEXT_TYPE_status_request :
          ExtName := 'status request';
      TLSEXT_TYPE_elliptic_curves :
          ExtName := 'elliptic curves';
      TLSEXT_TYPE_ec_point_formats :
          ExtName := 'EC point formats';
      TLSEXT_TYPE_session_ticket :
          ExtName := 'server ticket';
    else
        ExtName := 'unknown';
    end;
    if client_server = 0 then
        CS := 'client'
    else
        CS := 'server';
    if Ws.CheckLogOptions(loSslInfo) then
         Ws.DebugLog(loSslInfo, Format('TLSExtCB> TLS %s extension "%s" (id=%d), len=%d',
                                        [CS, ExtName, Type_, len]));
end;
{$ENDIF}

{$ENDIF OPENSSL_NO_TLSEXT}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{
procedure TCustomSslWSocket.WMSslHandshakeDone(var msg: TMessage);
begin
    TriggerSslHandshakeDone(0);
end;
}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*procedure TCustomSslWSocket.SslDebugLog(Msg : String);
begin
    // Not yet fully done, ToDo!
{$IFDEF DEBUG_OUTPUT}
    OutputDebugString(Msg);
{$ENDIF}
    //TriggerDebugDisplay(Msg);
end;    *)


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.SslSessionReused : Boolean;
begin
    Result := FSslEnable and Assigned(FSsl) and (f_SSL_session_reused(FSsl) = 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Server mode checks whether a renegotiation request is pending             }
function TCustomSslWSocket.SslRenegotiatePending : Boolean;
begin
    Result := FSslEnable and Assigned(FSsl) and
              (f_SSL_renegotiate_pending(FSsl) = 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Forces a SSL re-negotiation, app. data can still be received.             }
function TCustomSslWSocket.SslStartRenegotiation : Boolean;
var
    TmpInt  : Integer;
    Ver     : Integer;
    Pen     : Integer;
    NoReneg : Boolean;
begin
    Result := FALSE;
    if FSslEnable and Assigned(FSsl) then begin
        TmpInt  := f_SSL_state(FSsl);
        Ver     := f_SSL_version(FSsl);
        Pen     := f_SSL_renegotiate_pending(FSsl);
        NoReneg := IsSslRenegotiationDisallowed(Self);
        if NoReneg or
            not ((TmpInt = SSL_ST_OK) and
                 (Ver >= SSL3_VERSION) and
                 (Pen = 0)) then begin
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslErr) then begin
                DebugLog(loSslErr,
                         Format('%s ! Cannot start re-negotiation  State ' +
                                '%d Version (0x%x) RenegotiatePending %d ' +
                                'NO_RENEGOTIATION %d HSocket %d',
                                [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                                TmpInt, Ver, Pen, Ord(NoReneg),
                                FHSocket]));
            end;
{$ENDIF}
            Exit;
        end;
        if f_SSL_renegotiate(FSsl) = 0 then begin
            HandleSslError;
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslErr) then begin  { V5.21 }
                DebugLog(loSslErr, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                         ' ! SSL_renegotiate() failed');
            end
{$ENDIF}
        end
        else begin
            TmpInt := my_BIO_ctrl(FSslBio, BIO_C_DO_STATE_MACHINE, 0, nil); //<= 02/01/06 BS => f_SSL_do_handshake(FSsl)
            FSslBioWritePendingBytes := -1;
            if TmpInt = -1 then begin
                if not my_BIO_should_retry(FSslBio) then begin
                    HandleSslError;
{$IFNDEF NO_DEBUG_LOG}
                    if CheckLogOptions(loSslErr) then
                        DebugLog(loSslErr,
                                 IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                                 ' ! Re-negotiation failed');
{$ENDIF}
                    FNetworkError := WSAECONNABORTED;
                    WSocket_WSASetLastError(WSAECONNABORTED);
                    TriggerEvent(sslFdClose, 0);
                    Exit;
                end;
            end;
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslInfo) then  { V5.21 }
                DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                         ' ! Re-negotiation started ' +  IntToStr(FHSocket));
{$ENDIF}
            Result := TRUE;
            if FMayTriggerFD_Write then begin          //<= 02/01/06 AG
                if TriggerEvent(sslFdWrite, 0) then    //<= 02/01/06 AG
                    FMayTriggerFD_Write := False;      //<= 02/01/06 AG
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.StartSslHandshake;
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' StartSslHandshake ' + IntToStr(FHSocket));
{$ENDIF}
    if not FSslEnable then
        Exit;
    if not Assigned(FSslContext) then
        raise ESslContextException.Create('SSL requires a context object');
    if Assigned(FSsl) then
        ResetSsl;
    FSslState := sslHandshakeInit;
    FSslMode  := sslModeClient;
    try
        if (FSslContext.FSslCtx = nil) then
            FSslContext.InitContext;
        InitSslConnection(TRUE, FSslContext.FSslCtx);
    except
        on E : Exception do begin
            FSslState := sslNone;
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslInfo) then  { V5.21 }
                DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                          ' Fatal error SSL handshake ' + IntToStr(FHSocket) +
                          ' ' + E.Classname + ' ' + E.Message);
{$ENDIF}
            //TriggerSslHandshakeDone(2); ?
            raise
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.AcceptSslHandshake;
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' AcceptSslHandshake ' + IntToStr(FHSocket));
{$ENDIF}
    if not FSslEnable then
        Exit;
    if not Assigned(FSslContext) then
        raise ESslContextException.Create('SSL requires a context object');
    if Assigned(FSsl) then
        ResetSsl;
    FSslState      := sslHandshakeInit;
    FSslMode       := sslModeServer;
    try
        if (FSslContext.FSslCtx = nil) then
            FSslContext.InitContext;
        InitSslConnection(FALSE, FSslContext.FSslCtx);
    except
        on E : Exception do begin
            FSslState  := sslNone;
{$IFNDEF NO_DEBUG_LOG}
            if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                          ' Fatal error SSL handshake ' + IntToStr(FHSocket) +
                          ' ' + E.Classname + ': ' + E.Message);
{$ENDIF}
            //TriggerSslHandshakeDone(2); ?
            raise
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.DupConnected;
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' DupConnected');
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSSLWSocket.InternalAbort(ErrCode : Word);
begin
    FSslEnable := FALSE;
    ResetSsl;
    InternalCancelDnsLookup(TRUE);
    DeleteBufferedData;
    { Be sure to close as fast as possible (abortive close) }
    if (State = wsConnected) and (FProto = IPPROTO_TCP) then begin
        LingerOnOff := wsLingerOff;
        SetLingerOption;
    end;
    //inherited ShutDown(2);
    inherited InternalClose(FALSE, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.InternalClose(bShut: Boolean; Error: Word);
begin
    if (not FSslEnable) or (not Assigned(FSsl)) then begin
        inherited InternalClose(bShut, Error);
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' SslInternalClose ' + IntToStr(FHSocket));
{$ENDIF}
    if FHSocket = INVALID_SOCKET then begin
        if FState <> wsClosed then begin
            ChangeState(wsClosed);
            AssignDefaultValue;
        end;
        Exit;
    end;
    if FState = wsClosed then
        Exit;


    if bShut then begin
        if (not (csDestroying in Componentstate)) and (FSslIntShutDown = 0) then // AG 03/03/06
            ShutDown(1) // sends a SSL shutdown notify, then calls inherited ShutDown(1);
        else begin
            if FSslIntShutDown = 0 then
                inherited ShutDown(1);
            inherited InternalClose(FALSE, Error); // close the socket
        end;
    end
    else
        inherited InternalClose(FALSE, Error); // close the socket
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Listen;
begin
    inherited Listen;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' Listening');
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.TriggerSslVerifyPeer(
    var Ok     : Integer;
    Cert       : TX509Base);
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, 'TriggerSslVerifyPeer');
{$ENDIF}
    if Assigned(FOnSslVerifyPeer) then
        FOnSslVerifyPeer(Self, Ok, Cert);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{var
    FCount : Integer = 0; //Test}
procedure TCustomSslWSocket.InitSSLConnection(ClientMode : Boolean;
    pSSLContext : PSSL_CTX = nil);
var
    Options           : Integer;
    SessionIDContext  : TSslSessionIdContext; // for session caching in ssl server mode
    SslCachedSession  : Pointer;
    FreeSession       : Boolean;
    SIdCtxLen         : Integer;
    Dummy             : Byte;
begin
    if not FSslEnable then
        Exit;
    //Inc(FCount); // Test
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' InitSSLConnection ' + IntToStr(FHSocket));
{$ENDIF}
    FSslCertChain.Clear;
    FSslCertChain.X509Class := FX509Class;
    FSslCertChain.FLastVerifyResult := 0;
    if not Assigned(FSslPeerCert) then
        FSslPeerCert := FX509Class.Create(nil);
    FSslPeerCert.AssignDefaults;
    FSslPeerCert.FreeAndNilX509;
    //FTriggerCount            := 0; //Test
    FShutDownHow             := 1;
    FSslIntShutDown          := 0;
    FNetworkError            := 0;
    FLastSslError            := 0;
    FSslBiShutDownFlag       := FALSE;
    FCloseCalled             := FALSE;
    //FCloseReceived           := FALSE;
    //FHandShakeDoneInvoked    := FALSE;
    FSslHandshakeRespMsg     := '';  { V8.14 set with success or failure message once handshake completes }
    FSslHandshakeErr         := 0;   { V8.14 }
    FSslCipherDesc           := '';  { V8.14  }
    FSslEncryption           := '';  { V8.14  }
    FSslKeyExchange          := '';  { V8.14  }
    FSslMessAuth             := '';  { V8.14  }
    FPendingSslEvents        := [];
    FMayTriggerFD_Read       := TRUE;  // <= 01/06/2006 AG
    FMayTriggerFD_Write      := TRUE;  // <= 01/06/2006 AG
    FMayTriggerDoRecv        := TRUE;  // <= 01/06/2006 AG
    FMayTriggerSslTryToSend  := TRUE;  // <= 01/06/2006 AG
    InitializeSsl; // Load libraries
    try
        SslCritSect.Enter;
        try
            //Create new SSL Object
            FSsl := f_SSL_new(pSSLContext); // FSsl inherits all options
            if not Assigned(FSsl) then
                RaiseLastOpenSslError(Exception, TRUE,
                                      'Error on creating the Ssl object');
            //Create BIOs
            FSslBio := f_BIO_new(f_BIO_f_ssl);
            f_BIO_new_bio_pair(@FIBio, SSL_BUFFER_SIZE, @FNBio, SSL_BUFFER_SIZE);
            if (FSslBio = nil) or (FNBio = nil) or (FIBio = nil) then
                RaiseLastOpenSslError(Exception, TRUE,
                                      'Creating BIOs failed');

            (*
            Since FSsl inherits all options this was not required,
            also adding SSL_OP_ALL overwrote user defined options.
            Just set the right ctx-options in TSslContext.InitContext.
            Options := f_SSL_get_options(FSsl);
            { Currently no Tickets! Otherwise handshake fatal }
            { errors or session resumption won't work.        }
            Options := Options or SSL_OP_ALL or SSL_OP_NO_TICKET;
            f_SSL_set_options(FSsl, Options);
            *)

            if f_SSL_set_ex_data(FSsl, 0, Self) <> 1 then
                RaiseLastOpenSslError(Exception, TRUE,
                                      'SSL_set_ex_data failed');

            {if FCount mod 2 = 0 then  // Test
                raise Exception.Create('Test exception');}

            // Init SSL connection
            if ClientMode then begin
                // If we want send client certificates different from default
                if (FSslContext.SslCertFile = '') and
                   Assigned(FOnSslCliCertRequest) and
                   (not Assigned(f_SSL_CTX_get_client_cert_cb(
                          pSSLContext{FSslContext.FSslCtx}))) then
                    f_SSL_CTX_set_client_cert_cb(pSSLContext{FSslContext.FSslCtx},
                                                 ClientCertCallBack);
                // Get a cached session from the application
                SslCachedSession := nil;
                FreeSession      := TRUE;
                if Assigned(FOnSslCliGetSession) then
                    FOnSslCliGetSession(Self, SslCachedSession, FreeSession);
                if Assigned(SslCachedSession) and
                   (f_SSL_set_session(FSsl, SslCachedSession) = 1) then begin  // 01/14/06 AG
                {$IFNDEF NO_DEBUG_LOG}
                    if CheckLogOptions(loSslInfo) then  { V5.21 }
                        DebugLog(loSslInfo,
                                 IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                                 ' Reuse session [' +
                                 IntToHex(INT_PTR(SslCachedSession),
                                 SizeOf(SslCachedSession) * 2) +']');
                {$ENDIF}
                    if FreeSession then
                        f_SSL_SESSION_Free(SslCachedSession);
                    SslCachedSession := nil;
                end
                else
                    f_SSL_set_session(FSsl, nil);

            {$IFNDEF OPENSSL_NO_TLSEXT}
                { FSslServerName is the servername to be sent in client helo. }
                { If not empty, enables SNI in SSL client mode.               }
                if (FSslServerName <> '') and
                    (f_SSL_set_tlsext_host_name(FSsl, FSslServerName) = 0) then
                        RaiseLastOpenSslError(EOpenSslError, TRUE,
                             'Unable to set TLS servername extension');
            {$ENDIF}

                f_SSL_set_connect_state(FSsl);
            end
            else begin // Server mode
                if Assigned(FOnSslSetSessionIDContext) then begin
                    SessionIDContext := '';
                    FOnSslSetSessionIDContext(Self, SessionIDContext);
                    { This is so bad. We should consider a breaking change }
                    { and use AnsiString, same for session keys :(         }
                    SIdCtxLen := Length(SessionIDContext) * SizeOf(Char);
                    if SIdCtxLen > 0 then begin
                        if SIdCtxLen > SSL_MAX_SSL_SESSION_ID_LENGTH then
                        { Should trigger an exception rather than silently }
                        { truncate the data..                              }
                            SIdCtxLen := SSL_MAX_SSL_SESSION_ID_LENGTH;
                        { So with Unicode there are only 16 items left.    }
                        if f_ssl_set_session_id_context(FSsl,
                             @SessionIDContext[1], SIdCtxLen) = 0 then begin
                    {$IFNDEF NO_DEBUG_LOG}
                            if CheckLogOptions(loSslErr) then { V5.21 }
                                DebugLog(loSslErr,
                                IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                                    ' ssl_set_session_id_context failed' +
                                    IntToStr(FHSocket));
                        end
                        else begin
                            if CheckLogOptions(loSslInfo) then
                                DebugLog(loSslInfo,
                                IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                                ' SessionIDContext: ' + SessionIDContext + ' ' +
                                IntToStr(FHSocket));
                    {$ENDIF}
                        end;
                    end;
                end;

            {$IFNDEF OPENSSL_NO_TLSEXT}
                { FSslServerName receives the servername from client helo if }
                { FOnSslServerName was assigned in SSL server mode.          }
                FSslServerName := '';
                if Assigned(FOnSslServerName) and
                   (f_SSL_CTX_set_tlsext_servername_callback(pSSLContext,
                                            @ServerNameCallBack) = 0) then
                    RaiseLastOpenSslError(EOpenSslError, TRUE,
                            'Unable to initialize servername callback for SNI');
                {$IFDEF NEVER}
                    if CheckLogOptions(loSslInfo) then
                        f_SSL_set_tlsext_debug_callback(FSsl, @TlsExtension_CB);
                {$ENDIF}
            {$ENDIF}

                f_SSL_set_accept_state(FSsl);
            end;

            f_SSL_set_bio(FSsl, FIBio, FIBio);
            f_SSL_set_info_callback(FSsl, InfoCallBack);

            f_ERR_clear_error;

            my_BIO_ctrl(FSslBio, BIO_C_SET_SSL, BIO_NOCLOSE, FSsl);

            Options := my_BIO_read(FSslbio, @Dummy, 0); // reuse of int var Options!
            if Options < 0 then begin
                if not my_BIO_should_retry(FSslbio) then
                    //HandleSslError;
                    { Usually happens with an invalid context option set }
                    RaiseLastOpenSslError(EOpenSslError, TRUE,
                                          'InitSSLConnection:');
            end;
            //Initialize SSL negotiation
            if (FState = wsConnected) then begin
                TriggerEvent(sslFdRead, 0);
                TriggerEvent(sslFdWrite, 0);
            end;
            // Not a real error. Used to break the loop in TCustomWSocket.ASyncReceive
            FLastError := -1;
            FSslState  := sslHandshakeStarted;
        finally
            SslCritSect.Leave;
        end;
    except
        SslCachedSession := nil;                                 // 01/14/06 AG
        FSslState := sslHandshakeFailed; // just to allow the Reset
        ResetSsl;
        FSslState := sslHandshakeFailed;
        raise
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.AssignDefaultValue;
begin
    ResetSsl;
    inherited AssignDefaultValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.DeleteBufferedSslData;
begin
    if not Assigned(FSslBufList) then
        Exit;
    { Delete all data buffer }
    FSslBufList.Lock;
    try
        FSslBufList.DeleteAllData;
    finally
        FSslBufList.UnLock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.PutDataInSslBuffer(Data : Pointer; Len : Integer);
begin
    FSendPending := TRUE;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslInfo,
                 Format('%s PutDataInSslBuffer %s len %d  [%d] ',
                       [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                       IntToStr(FHSocket), Len, TraceCount]));
    end
    else if CheckLogOptions(loSslDump) then begin  { V5.21 }
        Inc(TraceCount);
        DebugLog(loSslDump,
                 Format('%s PutDataInSslBuffer %s [%d] Data:%s',
                        [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                        IntToStr(FHSocket),
                        TraceCount, DataToString(Data, Len)]));
    end;
{$ENDIF}
    if (Len <= 0) or (Data = nil) then
        Exit;

    FSslBufList.Lock;
    try
        FSslBufList.Write(Data, Len);
        bSslAllSent := FALSE;
    finally
        FSslBufList.UnLock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.ResetSslDelayed;
begin
    PostMessage(FWindowHandle, FMsg_WM_RESET_SSL, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Resume;
begin
    inherited;
    TriggerEvents;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.ResetSSL;
begin
    if FSsl_In_CB or (FSslState = sslHandshakeInit) then
        Exit;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) and Assigned(FSsl) then  { V5.21 }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                 ' ResetSslSession ' + IntToStr(FHSocket));
{$ENDIF}
    DeleteBufferedSslData;
    FSslVersion              := 'Unknown';
    FSslCipher               := FSslVersion;
    FSslTotalBits            := 0;
    FSslSecretBits           := 0;
    FSslVersNum              := 0;
    FSslCipherDesc           :='';     { V8.14 }

    FInHandshake             := FALSE;
    FHandshakeDone           := FALSE;
    FSslHandshakeRespMsg     := '';  { V8.14 set with success or failure message once handshake completes }
    FSslHandshakeErr         := 0;   { V8.14 }
    FSslCipherDesc           := '';  { V8.14  }
    FSslEncryption           := '';  { V8.14  }
    FSslKeyExchange          := '';  { V8.14  }
    FSslMessAuth             := '';  { V8.14  }
    FSslBioWritePendingBytes := -1;
    FSslInRenegotiation      := FALSE;
    FNetworkError            := 0;
    FSslState                := sslNone;
    FHandShakeCount          := 0;
    FSslSupportsSecureRenegotiation := FALSE;

    if Assigned(FSsl) then // Avoids sending a shutdown notify on freeing the SSL
        f_SSL_set_shutdown(FSsl, SSL_SENT_SHUTDOWN);

    if Assigned(FSslbio) then
        f_BIO_free(FSslbio);
    FSslbio := nil;

    if Assigned(FNbio) then
        f_BIO_free(FNbio);
    FNbio := nil;

    if Assigned(Fibio) then
        f_BIO_free(FIbio);
    FIbio := nil;

    if Assigned(FSsl) then
        f_SSL_free(FSsl);
    FSsl := nil;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.RaiseLastOpenSslError(
    EClass          : ExceptClass;
    Dump            : Boolean = FALSE;
    const CustomMsg : String  = '');
begin
    FLastSslError := f_ERR_peek_error;
    if Length(CustomMsg) > 0 then
        raise EClass.Create(#13#10 + CustomMsg + #13#10 +
                            String(LastOpenSslErrMsg(Dump)) + #13#10)
    else
        raise EClass.Create(#13#10 + String(LastOpenSslErrMsg(Dump)) + #13#10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function TCustomSslWSocket.LoadCertificate(out ErrMsg : String) : Boolean;
var
    PCAPath    : PChar;
    PCAFile    : PChar;
    Mode       : Integer;
    ErrCode    : Integer;
    LibErrCode : Integer;
    FctErrCode : Integer;
    WhyErrCode : Integer;
begin
    Result := FALSE;
    try
        // Load our keys and certificates
        if (FSslCertFile <> '') and
           (not FileExists(FSslCertFile)) then begin
            ErrMsg := 'File not found "' + FSslCertFile + '"';
            Exit;
        end;
        if (FSslPrivKeyFile <> '') and
           (not FileExists(FSslPrivKeyFile)) then begin
            ErrMsg := 'File not found "' + FSslPrivKeyFile + '"';
            Exit;
           end;
        if (FSslCAFile <> '') and
           (not FileExists(FSslCAFile)) then begin
            ErrMsg := 'File not found "' + FSslCAFile + '"';
            Exit;
        end;

        if (FSslCertFile <> '') and
           (f_SSL_CTX_use_certificate_chain_file(FSslCtx,
                                                 PChar(FSslCertFile)) = 0)
        then begin
            ErrCode    := f_ERR_peek_error;
            LibErrCode := ERR_GET_LIB(ErrCode);
            FctErrCode := ERR_GET_FUNC(ErrCode);
            WhyErrCode := ERR_GET_REASON(ErrCode);
            if ((LibErrCode = ERR_LIB_SSL) or
                (LibErrCode = ERR_LIB_SSL23) or
                (LibErrCode = ERR_LIB_SSL2) or
                (LibErrCode = ERR_LIB_SSL3)) then begin
                // No an error related to the certificate file, but the SSL
                // protocol (probably remote has shutdown). This error is
                // catched later. We can safely ignore here.
                // ToDo: confirm this behaviour
                Result := TRUE;
                Exit;
            end;
            OutputDebugString('ErrCode           = ' + IntToHex(ErrCode, 8));
            OutputDebugString('Library(ErrCode)  = ' + IntToStr(LibErrCode));
            OutputDebugString('Function(ErrCode) = ' + IntToStr(FctErrCode));
            OutputDebugString('Reason(ErrCode)   = ' + IntToStr(WhyErrCode));
            print_errors;
            ErrMsg := 'Can''t read certificate file "' + FSslCertFile + '"';
            Exit;
        end;

        f_SSL_CTX_set_default_passwd_cb(FSslCtx, @PasswordCallBack);
        f_SSL_CTX_set_default_passwd_cb_userdata(FSslCtx, Self);
        if (Length(FSslPrivKeyFile) > 0) and
           (f_SSL_CTX_use_PrivateKey_file(FSslCtx, PChar(FSslPrivKeyFile),
                                          SSL_FILETYPE_PEM) = 0) then begin
            ErrCode    := f_ERR_peek_error;
            LibErrCode := ERR_GET_LIB(ErrCode);
            FctErrCode := ERR_GET_FUNC(ErrCode);
            WhyErrCode := ERR_GET_REASON(ErrCode);
            OutputDebugString('ErrCode           = ' + IntToHex(ErrCode, 8));
            OutputDebugString('Library(ErrCode)  = ' + IntToStr(LibErrCode));
            OutputDebugString('Function(ErrCode) = ' + IntToStr(FctErrCode));
            OutputDebugString('Reason(ErrCode)   = ' + IntToStr(WhyErrCode));
            print_errors;
            ErrMsg := 'Can''t read key file "' + FSslPrivKeyFile + '"';
            Exit;
        end;

        if Length(FSslContext.SslCAPath) > 0 then
            PCAPath := PChar(FSslContext.SslCAPath)
        else
            PCAPath := nil;
        if Length(FSslCAFile) > 0 then
            PCAFile := PChar(FSslCAFile)
        else
            PCAFile := nil;

        // Load the CAs we trust
        //
        // If CAfile is not NIL, it points to a file of CA certificates in PEM
        // format. The file can contain several CA certificates.
        //
        // If CApath is not NIL, it points to a directory containing CA
        // certificates in PEM format. The files each contain one CA certificate.
        // The files are looked up by the CA subject name hash value, which must
        // hence be available. If more than one CA certificate with the same name
        // hash value exist, the extension must be different (e.g. 9d66eef0.0,
        // 9d66eef0.1 etc). The search is performed in the ordering of the
        // extension number, regardless of other properties of the certificates.
        if ((PCAFile <> nil) or (PCAPath <> nil)) and
           (f_SSL_CTX_load_verify_locations(FSslCtx,
                                            PCAFile, PCAPath) = 0) then begin
            ErrMsg := 'Can''t read CA File "' + FSslCAFile + '" or ' +
                                   'CA Path "' + FSslContext.SslCAPath + '"';
            Exit;
        end;

        if (PCAFile = nil) and (PCAPath = nil) and
           (f_SSL_CTX_set_default_verify_paths(FSslCtx) <> 1) then begin
            ErrMsg := 'Error loading default CA file and/or directory';
            Exit;
        end;

        if FSslVerifyPeer then begin
            Mode := 0;
            if (SslVerifyMode_NONE in FSslVerifyPeerModes) then
                Mode := Mode or SSL_VERIFY_NONE;
            if (SslVerifyMode_PEER in FSslVerifyPeerModes) then
                Mode := Mode or SSL_VERIFY_PEER;
            if (SslVerifyMode_FAIL_IF_NO_PEER_CERT in FSslVerifyPeerModes) then
                Mode := Mode or SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
            if (SslVerifyMode_CLIENT_ONCE in FSslVerifyPeerModes) then
                Mode := Mode or SSL_VERIFY_CLIENT_ONCE;

            f_SSL_CTX_set_verify(FSslCtx, Mode, PeerVerifyCallback);
            f_SSL_CTX_set_verify_depth(FSslCtx, FSslVerifyDepth);
        end;

        if FSslOptionsValue <> 0 then
            f_SSL_CTX_set_options(FSslCtx, FSslOptionsValue);

        if FSslCipherList <> '' then begin
            if f_SSL_CTX_set_cipher_list(FSslCtx,
                                         PChar(FSslCipherList)) <> 1 then begin
                ErrMsg := 'Error loading cipher list';
            end;
        end;

{$IFDEF  OPENSSL_VERSION_NUMBER_LESS_THAN_0x00905100L}
        f_SSL_CTX_set_verify_depth(FSslCtx, 1);
{$ENDIF}
        Result := TRUE;
    except
        on E:Exception do begin
            ErrMsg := 'LoadCertificate failed. ' +
                      E.ClassName + ': ' + E.Message;
            OutputDebugString(ErrMsg);
        end;
    end;
end;
 *)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Writes unencrypted data from the buffer to the SslBio                     }
procedure TCustomSslWSocket.TryToSend;
var
    Len       : Integer;
    Count     : Integer;
    Data      : TWSocketData;
begin
    if (not FSslEnable) or (FSocksState <> socksData) or
       (FHttpTunnelState <> htsData) then begin
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' TryToSend ' + IntToStr(FHSocket));
{$ENDIF}
        inherited TryToSend;
        Exit;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                      ' SslTryToSend ' + IntToStr(FHSocket));
{$ENDIF}
    FSslBufList.Lock;
    try
        if (FHSocket = INVALID_SOCKET) or (FSslBufList.IsEmpty) then begin
            bSslAllSent := TRUE;
            Exit;
        end;

        while TRUE do begin
            Len := FSslBufList.Peek(Data);
            if Len <= 0 then begin
                // Buffer is empty, every thing has been sent
                bSslAllSent := TRUE;
                break;
            end;
            if (FHSocket = INVALID_SOCKET) or                { No more socket  }
               (FSslBufList.IsEmpty) then                    { Nothing to send }
                Exit;
            if FNetworkError > 0 then begin
                WSocket_WSASetLastError(FNetworkError);
                FLastError := FNetworkError; //XX
                TriggerEvent(sslFdClose, 0);  // AG 03/03/06
                Exit;
            end;
            if FCloseCalled then begin      // AG 03/03/06  moved here
            { We don't trigger any error so far, just ignoring user data }
            { to be sent. We could close at once with WSAESHUTDOWN ?     }
                //FLastError := WSAESHUTDOWN;
                TriggerEvent(sslFdClose, 0);
                Exit;
            end;
            if (FSslIntShutDown > 0) then begin
                WSocket_WSASetLastError(WSAESHUTDOWN);
                FLastError := WSAESHUTDOWN; //XX
                if not FSslBiShutDownFlag then
                    TriggerEvent(sslFdClose, 0);
                Exit;
            end;
            if (not Assigned(FSsl)) or (f_SSL_state(FSsl) <> SSL_ST_OK) or {(FSslState < sslEstablished)}
               (f_SSL_renegotiate_pending(FSsl) = 1) then begin    // <= 12/31/05 AG
               { Don't write app. data while in handshake }        // <= 12/31/05 AG
                FMayTriggerSslTryToSend := TRUE;
                WSocket_WSASetLastError(WSAEWOULDBLOCK);
                FLastError := WSAEWOULDBLOCK; //XX
                Exit;
            end;

            {if FCloseCalled then begin      // AG 03/03/06 moved up
                TriggerEvent(sslFdClose, 0);
                Exit;
            end;}

            if FSslBioWritePendingBytes >= 0 then
                Len := FSslBioWritePendingBytes;

            Count := my_BIO_write(FSslbio, Data, Len);
            if Count = 0 then begin
                FSslBioWritePendingBytes := -1;
                my_BIO_ctrl(FSslbio, BIO_CTRL_FLUSH, 0, nil);
                WSocket_WSASetLastError(WSAECONNABORTED);
                FLastError := WSAECONNABORTED; //XX
                break;
            end;
            if Count < 0 then begin
                if my_BIO_should_retry(FSslbio) then begin
                    FSslBioWritePendingBytes := Len;
                    if FState = wsClosed then
                        Exit;
                    if FState <> wsConnected then begin
                        WSocket_WSASetLastError(FNetworkError);
                        FLastError := FNetworkError; //XX
                        Exit;
                    end;
                    FMayTriggerSslTryToSend := TRUE;
                    TriggerEvents;
                    WSocket_WSASetLastError(WSAEWOULDBLOCK);
                    FLastError := WSAEWOULDBLOCK; //XX
                    Exit;
                end;
                { Fatal error if BIO_should_retry = FALSE }
                HandleSslError;
                if FState = wsClosed then
                    Exit
                else if FState <> wsConnected then begin
                    WSocket_WSASetLastError(FNetworkError);
                    FLastError := FNetworkError; //XX
                    Exit;
                end;
(*
{$IFNDEF NO_DEBUG_LOG}
                    if CheckLogOptions(loSslInfo) then begin  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
                        Err := f_SSL_get_error(FSSL, Count);
                        DebugLog(loSslInfo, IntToHex(Integer(Self), 8) +
                                 ' BIO_write error: ' + IntToStr(Err) +
                                 ' ' +  SslErrorToStr(Err) + ' ' +
                                 IntToStr(FHSocket));
                        DebugLog(loSslInfo, LastOpenSslErrMsg(TRUE));
                    end;
{$ENDIF}
*)
                WSocket_WSASetLastError(WSAECONNABORTED);
                FLastError := WSAECONNABORTED; //XX
                TriggerEvent(sslFdClose, 0);
                Exit;
            end;
            my_BIO_ctrl(FSslbio, BIO_CTRL_FLUSH, 0, nil);
            FSslBioWritePendingBytes := -1;
            FSslBufList.Remove(Count);
            if Count < Len then
                { Could not write as much as we wanted. Stop sending }
                break;
        end; //while
    finally
        FSslBufList.Unlock;
    end;
    FMayTriggerSslTryToSend := TRUE;
    TriggerEvents;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.SslBiShutDownAsync;
begin
    PostMessage(FWindowHandle, FMsg_WM_BI_SSL_SHUTDOWN, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.SslShutDownAsync(How: Integer);         { V7.80 }
begin
    PostMessage(FWindowHandle, FMsg_WM_BI_SSL_SHUTDOWN, How, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.WMSslBiShutDown(var msg: TMessage);
begin
    if FSslEnable and (FSslIntShutDown = 0) then begin
        FSslBiShutDownFlag := (msg.LParam = 0);                     { V7.80 }
        InternalShutdown(msg.WParam);                               { V7.80 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.WMSslASyncSelect(var msg: TMessage);
begin
{ Select messages not posted by the socket but from the component }
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then begin { V5.21 }
        if __DataSocket = Self then
            DebugLog(loSslInfo,
                     IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' SslAsyncSelect DataSocket ' +
                     IntToStr(msg.wParam) +  ', ' +
                     IntToStr(msg.LParamLo) + WinsockMsgToString(Msg))
        else
            DebugLog(loSslInfo,
                     IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' SslAsyncSelect ' +
                     IntToStr(msg.wParam) + ', ' +
                     IntToStr(msg.LParamLo) + WinsockMsgToString(Msg));
    end;
{$ENDIF}
    if (msg.wParam <> WPARAM(FHSocket)) then
        Exit;
    {  ?
    if FPaused then
        exit;
    }
    if msg.lParamLo and FD_READ <> 0 then begin
        FPendingSslEvents := FPendingSslEvents - [sslFdRead];
        Do_Ssl_FD_READ(Msg);
    end
    else if msg.lParamLo and FD_WRITE <> 0 then begin
        FPendingSslEvents := FPendingSslEvents - [sslFdWrite];
        Do_FD_WRITE(Msg)
    end
    else if msg.lParamLo and FD_CLOSE <> 0 then begin
        FPendingSslEvents := FPendingSslEvents - [sslFdClose];
        Do_FD_CLOSE(Msg)
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.WndProc(var MsgRec: TMessage);
begin
    try                                                          // <= 12/12/05
        if MsgRec.Msg = FMsg_WM_SSL_ASyncSelect then
            WMSslASyncSelect(MsgRec)
        else if MsgRec.Msg = FMsg_WM_TRIGGER_DATASENT then
            TriggerDataSent(0)
        {else if MsgRec.Msg = WM_TRIGGER_SSLHANDSHAKEDONE then
            WMSslHandshakeDone(MsgRec)}
        else if MsgRec.Msg = FMsg_WM_RESET_SSL then
            ResetSsl
        else if MsgRec.Msg = FMsg_WM_BI_SSL_SHUTDOWN then
            WMSslBiShutDown(MsgRec)
        else if MsgRec.Msg = FMsg_WM_TRIGGER_SSL_SHUTDOWN_COMPLETED then
            WMTriggerSslShutDownComplete(MsgRec)
        else
            inherited WndProc(MsgRec);
    except                                                       // <= 12/12/05
        on E:Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.Notification(AComponent: TComponent; operation: TOperation);
begin
    inherited Notification(AComponent, operation);
    if operation = opRemove then begin
        if AComponent = FSslContext then
            FSslContext := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.TriggerEvents;
var
    State   : Integer;
{$IFNDEF NO_DEBUG_LOG}
    Str   : String;
{$ENDIF}
begin
    if not Assigned(FSsl) or (not FSslEnable) then
        Exit;
    State := f_SSL_state(FSsl);
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then begin { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        Str := IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
               ' TriggerEvents ' + IntToStr(FHSocket) + ' SslState: ';
        if State and SSL_ST_INIT <> 0  then
            Str := Str + 'SSL_ST_INIT '
        else if State = SSL_ST_OK then
            Str := Str + 'SSL_ST_OK '
        else
            Str := Str + IntToHex(State, 8);

        DebugLog(loSslInfo, Str +
              ' // MayFD_Read='      + BoolToStr(FMayTriggerFD_Read, FALSE) +
              ' MayDoRecv='       + BoolToStr(FMayTriggerDoRecv, FALSE)  +
              ' MayFD_Write='     + BoolToStr(FMayTriggerFD_Write, FALSE) +
              ' MaySslTryToSend=' + BoolToStr(FMayTriggerSslTryToSend, FALSE) +
              ' bSslAllSent='     + BoolToStr(bSslAllSent, FALSE) +
              ' bAllSent='        + BoolToStr(bAllSent, FALSE));
    end;
{$ENDIF}

    if FHandshakeDone = TRUE then begin
        FHandshakeDone := FALSE;
        TriggerSslHandshakeDone(0);
        if FSslInRenegotiation then
            FSslInRenegotiation := FALSE;
    end;

    if (my_BIO_ctrl_pending(FNbio) > 0) then begin
        if FMayTriggerFD_Write then begin
            if TriggerEvent(sslFdWrite, 0) then
                FMayTriggerFD_Write := FALSE;
        end;
    end
    else if (not bAllSent) and (State = SSL_ST_OK) and
            (my_BIO_ctrl_get_write_guarantee(FSslbio) > 0) and
             FMayTriggerFD_Write{FMayTriggerSslTryToSend} then begin  // AG 03/03/06
        if TriggerEvent(sslFdWrite, 0) then
            FMayTriggerFD_Write{FMayTriggerSslTryToSend} := FALSE;    // AG 03/03/06
    end
    else if (not bSslAllSent) and (State = SSL_ST_OK) and
             FMayTriggerSslTryToSend then begin
        FMayTriggerSslTryToSend := FALSE;
        TryToSend;
    end
    else if bAllSent and bSslAllSent and FSendPending and
       (State = SSL_ST_OK) then begin
        //Inc(FTriggerCount);
        FSendPending := FALSE;
        TriggerDataSent(0);
    end;

    if (State = SSL_ST_OK) and (my_BIO_ctrl_pending(FSslbio) > 0) then begin
        if FMayTriggerDoRecv  then begin
            if TriggerEvent(sslFdRead, 0) then
                FMayTriggerDoRecv := FALSE;
        end;
    end
    else if (my_BIO_ctrl_get_write_guarantee(FNbio) > 0) and
             FMayTriggerFD_Read then begin
        if TriggerEvent(sslFdRead, 0) then
            FMayTriggerFD_Read := FALSE;
    end;

    {if FCloseReceived and (FSslIntShutDown = 0) then
        TriggerEvent(sslFdClose, 0)
    else}
    if ((FCloseCalled and (FSslIntShutDown = 0)) or
       ((FSslIntShutDown = 2) and not FSslBiShutdownFlag)) and   // AG 03/03/06
       (State = SSL_ST_OK) and (my_BIO_ctrl_pending(FSslbio) = 0) then
        TriggerEvent(sslFdClose, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.TriggerSessionConnected(ErrCode: Word);
begin
    inherited TriggerSessionConnected(ErrCode);
    { An upper layer may have started the SSL! So we check FSsl=nil as well }
    if FSslEnable and (ErrCode = 0) and (FSsl = nil) then begin
        try
            { Both procedures may raise an exception! }
            if FSslMode = sslModeClient then
                StartSslHandshake
            else
                AcceptSslHandshake;
                //raise Exception.Create('******** TEST ************');
        except
            on E : Exception do begin
                FSslEnable := FALSE;
                ResetSsl;
                inherited InternalClose(FALSE, WSAECONNABORTED);
                HandleBackGroundException(E);
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Just to make UI easier: parse a semi-colon delimited texte string with
// a list of hosts and build the FSslAcceptableHosts list.
procedure TCustomSslWSocket.SetAcceptableHostsList(
    const SemiColonSeparatedList : String);
var
    Host : String;
    Buf  : String;
    I    : Integer;
begin
    FSslAcceptableHosts.Clear;
    Buf := SemiColonSeparatedList;
    while TRUE do begin
        I := Pos(';', Buf);
        if I > 0 then begin
            Host := IcsTrim(Copy(Buf, 1, I - 1));
            if Host > '' then
                FSslAcceptableHosts.Add(Host);
            Delete(Buf, 1, I);
        end
        else begin
            Host := IcsTrim(Buf);
            if Host > '' then
                FSslAcceptableHosts.Add(Host);
            break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.TriggerSslCliNewSession;
var
    CurrentSession : Pointer;
    IncRefCount    : Boolean;
begin
    // Sessions are created by a ssl server only
    if (FSslMode = sslModeClient) and Assigned(FOnSslCliNewSession) and
        Assigned(FSsl) then begin
        if FSslState = sslEstablished then
            CurrentSession := f_SSL_get_Session(FSsl)
        else
            CurrentSession := nil; // bad session
        IncRefCount := FALSE;
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
            DebugLog(loSslInfo, IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2) +
                     ' CliNewSession [' +
                     IntToHex(INT_PTR(CurrentSession), SizeOf(CurrentSession) * 2) + '] ' +
                     'Reused: ' + BoolToStr(SslSessionReused, TRUE));
{$ENDIF}
        SslCritSect.Enter;
        try
            FOnSslCliNewSession(Self, CurrentSession,
                                SslSessionReused,
                                IncRefCount);
            if IncRefCount and (CurrentSession <> nil) then
                f_SSL_get1_Session(FSsl);
        finally
            SslCritSect.Leave;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.TriggerSslHandshakeDone(ErrCode : Word);
var
    Cipher       : Pointer;
    PeerX        : PX509;
    Disconnect   : Boolean;
    RefCert      : TX509Base;
    Buffer       : array [0..128] of AnsiChar; { V8.14 }

    function FindCiphArg (const key: string): string;
    var
        I: integer;
    begin
        I := Pos (key, FSslCipherDesc);
        if I <= 0 then
            Result := ''
        else begin
            Result := Copy (FSslCipherDesc, I + Length (key), 99);
            I := Pos (' ', Result);
            if I <= 0 then I := Pos (#10, Result);
            if I > 0 then SetLength(Result, I - 1);
        end;
    end;

begin
    PeerX := nil;
    if (FHSocket = INVALID_SOCKET) then
        ErrCode := 1;
    if FSslHandshakeErr > 0 then begin
        ErrCode := Ics_Ssl_ERR_GET_REASON(FSslHandshakeErr);  { V8.14 keep error reason only  }
        case ErrCode of
            SSL_R_HTTPS_PROXY_REQUEST:  FSslHandshakeRespMsg := 'Error, HTTPS proxy request, no SSL handshake';
            SSL_R_HTTP_REQUEST:         FSslHandshakeRespMsg := 'Error, HTTP request, no SSL handshake';
            SSL_R_WRONG_VERSION_NUMBER: FSslHandshakeRespMsg := 'Error, wrong SSL version';
            SSL_R_UNKNOWN_PROTOCOL:     FSslHandshakeRespMsg := 'Error, unknown SSL protocol';
        end;
    end;
    if (ErrCode = 0) and Assigned(FSsl) then
        FSslState := sslEstablished
    else
        FSslState := sslHandshakeFailed;

    if FSslState = sslEstablished then begin
        FSslSupportsSecureRenegotiation :=
            f_SSL_get_secure_renegotiation_support(FSsl) = 1;
        FSslVersion := String(f_SSL_get_version(FSsl));
        FSslVersNum := f_SSL_version(FSsl);
        Cipher      := f_SSL_get_current_cipher(FSsl);
        if Assigned(Cipher) then begin
            FSslCipher     := String(f_SSL_CIPHER_get_name(Cipher));
            FSslSecretBits := f_SSL_CIPHER_get_bits(Cipher, @FSslTotalBits);
            FSslCipherDesc := string(f_SSL_CIPHER_description (Cipher, Buffer, 128));     { V8.14 }
            FSslEncryption := FindCiphArg ('Enc=');       { V8.14  }
            FSslKeyExchange := FindCiphArg ('Kx=');       { V8.14  }
            FSslMessAuth   := FindCiphArg ('Mac=');       { V8.14  }
        end;
        if FSslContext.SslVerifyPeer then
        { Get the peer cert from OSSL. Note that servers always send their }
        { certificates, clients on server request only. This gets the peer }
        { cert also when a session was reused.                             }
            PeerX := f_SSL_get_peer_certificate(FSsl);
     { V8.14 set with success or failure message once handshake completes }
        FSslHandshakeRespMsg := Format('SSL Connected OK with %s, cipher %s, key exchange %s, ' +
                        'encryption %s, message authentication %s',
                          [SslVersion, SslCipher, FSslKeyExchange, FSslEncryption, FSslMessAuth]);
    end  // FSslState = sslEstablished
    else begin
        if (FSslHandshakeRespMsg = '') then begin  { V8.14  }
            if (ErrCode = 1) then
               FSslHandshakeRespMsg := 'Error, connection closed unexpectedly'
            else
               FSslHandshakeRespMsg := String(LastOpenSslErrMsg(true));
        end;
    end;
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loSslInfo) then  { V5.21 } { replaces $IFDEF DEBUG_OUTPUT  }
        DebugLog(loSslInfo, Format('%s SslHandshakeDone(%d) %d. Secure connection ' +
                             'with %s, cipher %s, %d secret bits (%d total), ' +
                             'session reused=%s',
                             [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                             ErrCode, FHSocket, SslVersion, SslCipher, SslSecretBits,
                             SslTotalBits, BoolToStr(SslSessionReused, TRUE)]));
{$ENDIF}
    FSslPeerCert.X509 := PeerX;
    if Assigned(PeerX) then begin
        { Do we have the peer cert in our chain?                          }
        RefCert := FSslCertChain.Find(PeerX);
        f_X509_free(PeerX);
        { If we have a chain with the peer certificate (new session)      }
        { copy some values.                                               }
        if RefCert <> nil then
        begin
            FSslPeerCert.FSha1Digest        := RefCert.FSha1Digest;
            FSslPeerCert.FSha1Hex           := RefCert.FSha1Hex;
            FSslPeerCert.VerifyResult       := RefCert.VerifyResult;
            FSslPeerCert.CustomVerifyResult := RefCert.CustomVerifyResult;
            FSslPeerCert.FirstVerifyResult  := RefCert.FirstVerifyResult;
        end
        { We don't have a chain with peer certificate (reused session )   }
        { get the session verify result from OSSL and assign it. This     }
        { verify result is the original one which is i.e. not changed if  }
        { we set "Ok := 1;" in OnSslVerifyPeer event.                     }
        else begin
            FSslPeerCert.VerifyResult := f_SSL_get_verify_result(FSsl);
            //FSslPeerCert.FirstVerifyResult := FSslPeerCert.VerifyResult; ?
        end;
    end;

    Disconnect := FALSE;
    if Assigned(FOnSslHandshakeDone) then
        FOnSslHandshakeDone(Self, ErrCode, FSslPeerCert, Disconnect);
    if Disconnect and (ErrCode = 0) then
        Close{Delayed?}
    else if ErrCode = 0 then
        // Publish the new session so that the application can cache it.
        TriggerSslCliNewSession;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.PutDataInSendBuffer(Data : TWSocketData; Len : Integer);
begin
    if (not FSslEnable) or (FSocksState <> socksData) or
       (FHttpTunnelState <> htsData) then begin
{$IFNDEF NO_DEBUG_LOG}
        if CheckLogOptions(loSslInfo) then begin { V5.21 }
            Inc(TraceCount);
            DebugLog(loSslDump, Format('%s PutDataInSendBuffer %s  len %d [%d]',
                             [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                             IntToStr(FHSocket), Len, TraceCount]));
        end
        else if CheckLogOptions(loSslDump) then begin { V5.21 }
            Inc(TraceCount);
            DebugLog(loSslDump, Format('%s PutDataInSendBuffer %s [%d] Data:%s',
                             [IntToHex(INT_PTR(Self), SizeOf(Pointer) * 2),
                             IntToStr(FHSocket), TraceCount,
                             DataToString(Data, Len)]));
        end;
{$ENDIF}
        inherited PutDataInSendBuffer(Data, Len);
        Exit;
    end;
    if Len <= 0 then
        Exit;
    bSslAllSent := FALSE;
    PutDataInSslBuffer(Data, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSslWSocket.MsgHandlersCount : Integer;
begin
    Result := 5 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_TRIGGER_DATASENT    := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_SSL_ASYNCSELECT     := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_RESET_SSL           := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_BI_SSL_SHUTDOWN     := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_TRIGGER_SSL_SHUTDOWN_COMPLETED := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSslWSocket.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_TRIGGER_DATASENT);
        FWndHandler.UnregisterMessage(FMsg_WM_SSL_ASYNCSELECT);
        FWndHandler.UnregisterMessage(FMsg_WM_RESET_SSL);
        FWndHandler.UnregisterMessage(FMsg_WM_BI_SSL_SHUTDOWN);
        FWndHandler.UnregisterMessage(FMsg_WM_TRIGGER_SSL_SHUTDOWN_COMPLETED);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF} // USE_SSL
//type


//var
  //GAsyncDnsLookup : TIcsAsyncDnsLookup = nil;

{ TCustomHttpTunnelWSocket }

const
  sHttpProxyConnect         = 'CONNECT';
  sHttpProxyKeepAlive       = 'Proxy-Connection: Keep-Alive';
  sHttpProxyAuthorization   = 'Proxy-Authorization: ';
  sCrLf                     = #13#10;
  sCrLfCrLf                 = sCrLf + sCrLf;
  sHttpProto                = 'HTTP/1.1';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_TUNNEL_RECONNECT := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.AssignDefaultValue;
begin
    if FHttpTunnelReconnectRequest = htrrNone then
        inherited AssignDefaultValue
    else begin
        { We will reconnect so do not clear all }
        FHSocket            := INVALID_SOCKET;
        FSelectEvent        := 0;
        FState              := wsClosed;
        bAllSent            := TRUE;
        FPaused             := FALSE;
        FCloseInvoked       := FALSE;
    end;
    FHttpTunnelState         := htsData;
    FHttpTunnelRcvdCnt       := 0;
    FHttpTunnelRcvdIdx       := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.Connect;
begin
    if not FHttpTunnelServerAssigned then
        inherited Connect

    else if (IcsLowerCase(FProtoStr) <> 'tcp') and (IcsTrim(FProtoStr) <> '6') then
        RaiseException('TCP is the only protocol supported by HTTP proxies')
    else begin
        try
            if not FPortResolved then begin
                { The next line will trigger an exception in case of failure }
                Fsin.sin6_port  := WSocket_Synchronized_htons(
                    WSocket_Synchronized_ResolvePort(FHttpTunnelPort,
                                                     AnsiString(FProtoStr)));
                FPortResolved := TRUE;
            end;

            if not FAddrResolved then begin
                { The next line will trigger an exception in case of failure }
                if FSocketFamily = sfIPv4 then
                begin
                    Fsin.sin6_family := AF_INET;
                    PSockAddrIn(@Fsin).sin_addr.S_addr :=
                     WSocket_Synchronized_ResolveHost(FHttpTunnelServer).s_addr;
                end
                else
                    WSocket_Synchronized_ResolveHost(HttpTunnelServer, Fsin,
                                                     FSocketFamily, IPPROTO_TCP);
                FAddrResolved := TRUE;
            end;
            { The next line will trigger an exception in case of failure }
            FPortNum := WSocket_Synchronized_ResolvePort(AnsiString(FPortStr),
                                                         AnsiString(FProtoStr));
        except
            on E: Exception do begin
                RaiseException('Connect: ' + E.Message);
                Exit;
            end;
        end;
        FHttpTunnelCloseNotified   := FALSE;
        FHttpTunnelRcvdCnt         := 0;
        FHttpTunnelRcvdIdx         := 0;
        FHttpTunnelServerAuthTypes := [];
        FHttpTunnelLastResponse    := '';
        FHttpTunnelStatusCode      := ICS_HTTP_TUNNEL_PROTERR;
        if Length(FHttpTunnelBuf) <> FHttpTunnelBufSize then
            SetLength(FHttpTunnelBuf, FHttpTunnelBufSize);
        FHttpTunnelState := htsConnecting;
        inherited Connect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomHttpTunnelWSocket.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FHttpTunnelBufSize  := 1024;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then
        FWndHandler.UnregisterMessage(FMsg_WM_TUNNEL_RECONNECT);
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.GetRcvdCount : LongInt;
begin
    if FHttpTunnelRcvdCnt <= 0 then
        Result := inherited GetRcvdCount
    else
        Result := FHttpTunnelRcvdCnt;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.Do_FD_CLOSE(var msg: TMessage);
begin
    FHttpTunnelCloseNotified := TRUE;
    inherited Do_FD_CLOSE(msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.DoRecv(var Buffer: TWSocketData; BufferSize,
  Flags: Integer): Integer;
begin
    if (FHttpTunnelRcvdCnt > 0) and (FHttpTunnelState = htsData) then
    begin
        { We still have previously received data in our internal buffer }
        if FHttpTunnelRcvdCnt <= BufferSize then begin
            { User buffer is greater or equal buffered data, copy all and clear }
            Move(FHttpTunnelBuf[FHttpTunnelRcvdIdx], Buffer^, FHttpTunnelRcvdCnt);
            Result             := FHttpTunnelRcvdCnt;
            FHttpTunnelRcvdCnt := 0;
            FHttpTunnelRcvdIdx := 0;
        end
        else begin
            { User buffer is smaller, copy as much as possible }
            Move(FHttpTunnelBuf[FHttpTunnelRcvdIdx], Buffer^, BufferSize);
            Result             := BufferSize;
            FHttpTunnelRcvdIdx := FHttpTunnelRcvdIdx + BufferSize;
            FHttpTunnelRcvdCnt := FHttpTunnelRcvdCnt - BufferSize;
            { We've still buffered data, we MUST ensure it's read!   }
            { Otherwise the application might wait for it infinitely }
            if (FState = wsConnected) then
                PostMessage(Handle, FMsg_WM_ASYNCSELECT,
                             WPARAM(FHSocket), LPARAM(FD_READ));
        end;
    end
    else
        Result := inherited DoRecv(Buffer, BufferSize, Flags)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.GetHttpTunnelLastResponse: String;
begin
    Result := String(FHttpTunnelLastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.GetHttpTunnelServer: String;
begin
    Result := String(FHttpTunnelServer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.GetHttpTunnelPort: String;
begin
    Result := String(FHttpTunnelPort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.HttpTunnelGetNtlmMessage3: String;
var
    Hostname : String;
    NtlmInfo : TNTLM_Msg2_Info;
    LDomain, LUser: String;
begin
    { get local hostname }
    try
        Hostname := String(LocalHostName);
    except
        Hostname := '';
    end;
    NtlmInfo := NtlmGetMessage2(String(FHttpTunnelAuthChallenge));
    NtlmParseUserCode(HttpTunnelUsercode, LDomain, LUser, FALSE);
    { With Squid proxy LDomain may not be empty (at the time of writing), }
    { a user code of <DOMAIN>\<UserName> works with Squid. Fix below      }
    { works with Squid, however I'm not 100% sure whether to include it   }
    { by default.                                                         }
    if (LDomain = '') and (Pos('@', LUser) = 0) then
        LDomain := NtlmInfo.Domain;

    { hostname is the local hostname }
    Result := NtlmGetMessage3(LDomain,
                              Hostname,
                              LUser,
                              FHttpTunnelPassword,
                              NtlmInfo,                  { V7.86 }
                              CP_ACP,
                              FHttpTunnelLmCompatLevel); { V7.86 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.HttpTunnelClear;
begin
    { Only called before request }
    FHttpTunnelStatusCode       := ICS_HTTP_TUNNEL_PROTERR;
    FHttpTunnelKeepsAlive       := TRUE;
    FHttpTunnelContentLength    := 0;
    FHttpTunnelProto            := htp11;
    FHttpTunnelRcvdCnt          := 0;
    FHttpTunnelRcvdIdx          := 0;
    FHttpTunnelWaitingBody      := FALSE;
    FHttpTunnelLastResponse     := '';
    FHttpTunnelReconnectRequest := htrrNone;
{$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
    FHttpTunnelAuthDigestCached := FALSE;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.HttpTunnelSendAuthBasic;
var
    LAuthHdr : String;
begin
    HttpTunnelClear;
    FHttpTunnelCurAuthType := htatBasic;
    FHttpTunnelState       := htsWaitResp1;
    LAuthHdr := sCrLf + sHttpProxyAuthorization + 'Basic ' +
                Base64Encode(FHttpTunnelUsercode + ':' +
                FHttpTunnelPassword);
    SendStr(sHttpProxyConnect + ' ' + FAddrStr + ':' + IntToStr(FPortNum) +
            ' ' + sHttpProto + LAuthHdr + sCrLfCrLf);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
procedure TCustomHttpTunnelWSocket.HttpTunnelSendAuthDigest;
var
    LAuthHdr, Uri : String;
begin
    HttpTunnelClear;
    FHttpTunnelCurAuthType := htatDigest;
    FHttpTunnelState       := htsWaitResp2;
    Inc(FHttpTunnelAuthDigestInfo.Nc);
    Uri := '/';  // Required for Squid!
    LAuthHdr := sCrLf + sHttpProxyAuthorization + 'Digest ' +
                AuthDigestGenerateRequest(FHttpTunnelUsercode,
                                          FHttpTunnelPassword,
                                          sHttpProxyConnect,
                                          Uri,
                                          FHttpTunnelAuthDigestHash,
                                          FHttpTunnelAuthDigestInfo);
    SendStr(sHttpProxyConnect + ' ' + FAddrStr + ':' + IntToStr(FPortNum) +
            ' ' + sHttpProto + LAuthHdr + sCrLfCrLf);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.HttpTunnelSendAuthNtlm_1;
var
    LAuthHdr : String;
begin
    HttpTunnelClear;
    FHttpTunnelCurAuthType := htatNtlm;
    FHttpTunnelState       := htsWaitResp1;
    LAuthHdr := sCrLf + sHttpProxyAuthorization + 'NTLM ' +
                NtlmGetMessage1('', '', FHttpTunnelLmCompatLevel); { V7.86 }
    if not (wsoNoHttp10Tunnel in FComponentOptions) then
        { Make some HTTP/1.0 proxies happy, i.e MSP 2.0 }
        LAuthHdr := LAuthHdr + sCrLf + sHttpProxyKeepAlive;
    SendStr(sHttpProxyConnect + ' ' + FAddrStr + ':' + IntToStr(FPortNum) +
            ' ' + sHttpProto + LAuthHdr + sCrLfCrLf);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.HttpTunnelSendAuthNtlm_3;
var
    LAuthHdr : String;
begin
    HttpTunnelClear;
    FHttpTunnelState := htsWaitResp2;
    LAuthHdr := sCrLf + sHttpProxyAuthorization + 'NTLM ' +
                HttpTunnelGetNtlmMessage3;
    SendStr(sHttpProxyConnect + ' ' + FAddrStr + ':' + IntToStr(FPortNum) +
            ' ' + sHttpProto + LAuthHdr + sCrLfCrLf);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.HttpTunnelSendPlainConnect;
begin
    HttpTunnelClear;
    FHttpTunnelState := htsWaitResp0;
    { We send the Keep-Alive header only since there are proxies i.e. the      }
    { CSM 4.2 that require at least two header lines in order to send a reply. }
    SendStr(sHttpProxyConnect + ' ' + FAddrStr + ':' + IntToStr(FPortNum) +
            ' ' + sHttpProto + sCrLf + sHttpProxyKeepAlive + sCrLfCrLf);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.HttpTunnelTriggerResultOrContinue: Boolean;
begin
    Result := TRUE;
    if FHttpTunnelStatusCode = 200 then begin
        FHttpTunnelState := htsData;
    {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
        FHttpTunnelAuthDigestCached := (FHttpTunnelCurAuthType = htatDigest);
    {$ENDIF}
        TriggerSessionConnected(0);
    end
    else if FHttpTunnelCurAuthType in [htatNone, htatBasic] then begin
        TriggerHttpTunnelError(FHttpTunnelStatusCode);
        Result := FALSE;
    end
    else if (FHttpTunnelStatusCode <> 407) then begin
        TriggerHttpTunnelError(FHttpTunnelStatusCode);
        Result := FALSE;
    end
    { ------------- 407 starts here ------------------ }
    else if not FHttpTunnelKeepsAlive then begin
        { Connection has to be closed, happens with both HTTP 1.0 and 1.1 }
        { check if we have to reconnect asynchronously.                   }
        if (FHttpTunnelState = htsWaitResp0) then begin
            { It's a response to our plain CONNECT request }
            if FHttpTunnelCurAuthType = htatDetect then begin
                if htsatBasic in FHttpTunnelServerAuthTypes then
                    FHttpTunnelReconnectRequest := htrrBasic
            {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
                else if (htsatDigest in FHttpTunnelServerAuthTypes) and
                    FHttpTunnelAuthDigestValid then
                  FHttpTunnelReconnectRequest := htrrDigest
            {$ENDIF}
                else if htsatNtlm in FHttpTunnelServerAuthTypes then
                    FHttpTunnelReconnectRequest := htrrNtlm1;
            end
        {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
            else if FHttpTunnelCurAuthType = htatDigest then begin
                if (htsatDigest in FHttpTunnelServerAuthTypes) and
                    FHttpTunnelAuthDigestValid then
                    FHttpTunnelReconnectRequest := htrrDigest;
            end
        {$ENDIF};
        end;
        if FHttpTunnelReconnectRequest = htrrNone then
            TriggerHttpTunnelError(FHttpTunnelStatusCode)
        else begin
            { Close the connection and start a new one asynchronously in }
            { in TriggerSessionClosed.                                   }
            FHttpTunnelState := htsData;
            FHttpTunnelStatusCode := ICS_HTTP_TUNNEL_PROTERR;
            if not FHttpTunnelCloseNotified then
                inherited InternalClose(TRUE, 0);
        end;
        Result := FALSE;
    end
{$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
    else if FHttpTunnelCurAuthType = htatDigest then begin
        { Digest }
        if (htsatDigest in FHttpTunnelServerAuthTypes) and
            FHttpTunnelAuthDigestValid and
           (FHttpTunnelState in [htsWaitResp0, htsWaitResp1]) then
            HttpTunnelSendAuthDigest
        else begin
            TriggerHttpTunnelError(FHttpTunnelStatusCode);
            Result := FALSE;
        end;
    end
{$ENDIF}
    else if FHttpTunnelCurAuthType = htatNtlm then begin
        { NTLM }
        if (htsatNtlm in FHttpTunnelServerAuthTypes) and
           (FHttpTunnelState = htsWaitResp1) then
            HttpTunnelSendAuthNtlm_3
        else begin
            TriggerHttpTunnelError(FHttpTunnelStatusCode);
            Result := FALSE;
        end;
    end
    else if FHttpTunnelCurAuthType = htatDetect then begin
        { Detect AuthType supported by the proxy }
        if htsatBasic in FHttpTunnelServerAuthTypes then
            HttpTunnelSendAuthBasic
    {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
        else if (htsatDigest in FHttpTunnelServerAuthTypes) and
                FHttpTunnelAuthDigestValid then
            HttpTunnelSendAuthDigest
    {$ENDIF}
        else if htsatNtlm in FHttpTunnelServerAuthTypes then
            HttpTunnelSendAuthNtlm_1
        else begin
            TriggerHttpTunnelError(FHttpTunnelStatusCode);
            Result := FALSE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.HttpTunnelProcessHdrLine(
    Data  : PAnsiChar;
    Cnt   : Integer): Boolean;
var
    I : Integer;
    LStatusCode : Integer;
    LStr : AnsiString;
begin
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loWsockDump) then begin
        if Cnt > 0 then begin
            SetLength(LStr, Cnt);
            Move(Data^, Pointer(LStr)^, Cnt);
        end;
        DebugLog(loWsockDump, String(LStr));
    end;
{$ENDIF}

    Result := TRUE;
    if Cnt = 0 then begin
        { Empty line, end of header }
        FHttpTunnelWaitingBody := (FHttpTunnelContentLength > 0) or FHttpTunnelChunked;
        if not FHttpTunnelWaitingBody then
            Result := HttpTunnelTriggerResultOrContinue
        else if FHttpTunnelChunked then begin
            FHttpTunnelChunkSize  := 0;
            FHttpTunnelChunkRcvd  := 0;
            FHttpTunnelChunkState := htcsGetSize;
        end;
    end
    else if FHttpTunnelStatusCode = ICS_HTTP_TUNNEL_PROTERR then
    begin
        { HTTP/1.x <status code> }
        if (Cnt >= 12) and (StrLIComp(Data, PAnsiChar('HTTP/1.'), 7) = 0) then begin
            FHttpTunnelChunked := FALSE;
            if Data[7] = '0' then begin
                FHttpTunnelProto := htp10;
                if wsoNoHttp10Tunnel in FComponentOptions then begin
                    FHttpTunnelLastResponse := sHttpVersionError;
                    TriggerHttpTunnelError(ICS_HTTP_TUNNEL_VERSIONERR);
                    Result := FALSE;
                    Exit; //***
                end;
            end;
            FHttpTunnelKeepsAlive := (FHttpTunnelProto = htp11);
            SetLength(FHttpTunnelLastResponse, Cnt);
                Move(Data^, Pointer(FHttpTunnelLastResponse)^, Cnt);

            LStatusCode := 0;
            I           := 8;
            while (I < Cnt) and (Data[I] = #$20) do Inc(I);
            while (I < Cnt) and (Data[I] in ['0'..'9']) do begin
                LStatusCode := LStatusCode * 10 + Byte(Data[I]) - Byte('0');
                Inc(I);
            end;
            if LStatusCode >= 100 then
                FHttpTunnelStatusCode := LStatusCode;
        end;
        if FHttpTunnelStatusCode = ICS_HTTP_TUNNEL_PROTERR then begin
            TriggerHttpTunnelError(FHttpTunnelStatusCode);
            Result := FALSE;
        end;
    end
    else if (Cnt > 25) and (StrLIComp(Data, 'Transfer-Encoding:', 18) = 0) then begin
        I := 18;
        while (I < Cnt) and (Data[I] = #$20) do Inc(I);
        Inc(Data, I);
        FHttpTunnelChunked := (Cnt >= I + 7) and
                              (StrLIComp(Data, PAnsiChar('chunked'), 7) = 0);
    end
    else if (Cnt > 22) and (StrLIComp(Data, 'Proxy-Connection:', 17) = 0) then begin
        I := 17;
        while (I < Cnt) and (Data[I] = #$20) do Inc(I);
        Inc(Data, I);
        if (Cnt >= I + 10) and (StrLIComp(Data, PAnsiChar('keep-alive'), 10) = 0) then
            FHttpTunnelKeepsAlive := TRUE
        else if (Cnt >= I + 5) and (StrLIComp(Data, PAnsiChar('close'), 5) = 0) then
            FHttpTunnelKeepsAlive := FALSE;
    end
    else if (Cnt > 15) and (StrLIComp(Data, 'Content-Length:', 15) = 0) then begin
        I := 15;
        while (I < Cnt) and (Data[I] = #$20) do Inc(I);
        while (I < Cnt) and (Data[I] in ['0'..'9']) do begin
            FHttpTunnelContentLength := FHttpTunnelContentLength * 10 +
                                        Byte(Data[I]) - Byte('0');
            Inc(I);
        end;
    end
    else if (FHttpTunnelStatusCode = 407) then begin
        { Auth required.
          Get the NTLM challenge and collect FHttpTunnelServerAuthTypes. }
        if (Cnt >= 24) and (StrLIComp(Data,
                            PAnsiChar('Proxy-Authenticate:'), 19) = 0) then begin
            I := 19;
            while (I < Cnt) and (Data[I] = #$20) do Inc(I);
            Inc(Data, I);
            if (Cnt >= I + 20) and (StrLIComp(Data, PAnsiChar('Digest'), 6) = 0) then begin
            { Digest challenge }
            {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
                Inc(I, 6);
                Inc(Data, 6);
                while (I < Cnt) and (Data^ = #$20) do begin
                    Inc(I);
                    Inc(Data);
                end;
                SetLength(LStr, Cnt - I);
                Move(Data^, Pointer(LStr)^, Cnt - I);
                AuthDigestParseChallenge(String(LStr), FHttpTunnelAuthDigestInfo);
                FHttpTunnelAuthDigestValid :=
                      AuthDigestValidateResponse(FHttpTunnelAuthDigestInfo);
            {$ENDIF}
                Include(FHttpTunnelServerAuthTypes, htsatDigest);
            end
            else if (Cnt >= I + 5) and (StrLIComp(Data, PAnsiChar('Basic'), 5) = 0) then
                Include(FHttpTunnelServerAuthTypes, htsatBasic)
            else if (Cnt >= I + 4) and (StrLIComp(Data, PAnsiChar('NTLM'), 4) = 0) then begin
                if (FHttpTunnelState = htsWaitResp1) then begin
                    if (Cnt > 100) then begin
                        { NTLM challenge }
                        Inc(I, 5);
                        Inc(Data, 5);
                        while (I < Cnt) and (Data^ = #$20) do begin
                            Inc(I);
                            Inc(Data);
                        end;
                        SetLength(FHttpTunnelAuthChallenge, Cnt - I);
                        Move(Data^, Pointer(FHttpTunnelAuthChallenge)^, Cnt - I);
                    end;
                    Include(FHttpTunnelServerAuthTypes, htsatNtlm);
                end
                else if (FHttpTunnelState = htsWaitResp0) then
                    Include(FHttpTunnelServerAuthTypes, htsatNtlm);
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.Listen;
begin
    if not FHttpTunnelServerAssigned then
        inherited Listen
    else
        RaiseException('Listening is not supported thru HTTP proxy servers');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.MsgHandlersCount : Integer;
begin
    Result := 1 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.RaiseException(const Msg : String);
begin
    HttpTunnelClear;
    inherited RaiseException(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.SetHttpTunnelAuthType(
  const Value: THttpTunnelAuthType);
begin
    if State <> wsClosed then
        RaiseException('Can''t change HTTP proxy authentication if not closed')
    else begin
        if FHttpTunnelAuthType <> Value then
        begin
            FHttpTunnelAuthType    := Value;
            FHttpTunnelCurAuthType := Value;
        {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
            FHttpTunnelAuthDigestCached := FALSE;
            FHttpTunnelAuthDigestValid  := FALSE;
        {$ENDIF}
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.SetHttpTunnelBufferSize(BufSize: Integer);
begin
    FHttpTunnelBufSize := BufSize;
    if FHttpTunnelBufSize < 0 then
        FHttpTunnelBufSize := 0;
    SetLength(FHttpTunnelBuf, FHttpTunnelBufSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.SetHttpTunnelPassword(const Value: String);
begin
    if State <> wsClosed then
        RaiseException('Can''t change HTTP proxy password if not closed')
    else begin
        if FHttpTunnelPassword <> Value then begin
            FHttpTunnelCurAuthType := FHttpTunnelAuthType;
            FHttpTunnelPassword := Value;
        {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
            FHttpTunnelAuthDigestCached := FALSE;
            FHttpTunnelAuthDigestValid  := FALSE;
        {$ENDIF}
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.SetHttpTunnelPort(const Value: String);
var
    NewValue : AnsiString;
begin
    if State <> wsClosed then
        RaiseException('Can''t change HTTP proxy port if not closed')
    else begin
        NewValue := AnsiString(IcsTrim(Value));
        if NewValue <> FHttpTunnelPort then begin
            FHttpTunnelCurAuthType      := FHttpTunnelAuthType;
        {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
            FHttpTunnelAuthDigestCached := FALSE;
            FHttpTunnelAuthDigestValid  := FALSE;
        {$ENDIF}
        end;
        FHttpTunnelPort := NewValue;
        FHttpTunnelPortAssigned := FHttpTunnelPort <> '';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.SetHttpTunnelServer(const Value: String);
var
    NewValue : AnsiString;
begin
    if State <> wsClosed then
        RaiseException('Can''t change HTTP proxy if not closed')
    else begin
        NewValue := AnsiString(IcsTrim(Value));
        if NewValue <> FHttpTunnelServer then begin
            FHttpTunnelCurAuthType      := FHttpTunnelAuthType;
            FHttpTunnelServer           := NewValue;
        {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
            FHttpTunnelAuthDigestCached := FALSE;
            FHttpTunnelAuthDigestValid  := FALSE;
        {$ENDIF}
        end;
        FHttpTunnelServerAssigned := FHttpTunnelServer <> '';
        if FHttpTunnelServerAssigned and
           TCustomSocksWSocket(Self).FSocksServerAssigned then begin
            FHttpTunnelServer   := '';
            FHttpTunnelServerAssigned := FALSE;
            raise Exception.Create('Can''t use HTTP proxy when Socks is used as well');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.SetHttpTunnelUsercode(const Value: String);
begin
    if State <> wsClosed then
        RaiseException('Can''t change HTTP proxy usercode if not closed')
    else begin
        if FHttpTunnelUsercode <> Value then begin
            FHttpTunnelCurAuthType := FHttpTunnelAuthType;
            FHttpTunnelUsercode := Value;
        {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
            FHttpTunnelAuthDigestCached := FALSE;
            FHttpTunnelAuthDigestValid  := FALSE;
        {$ENDIF}
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomHttpTunnelWSocket.TriggerDataAvailable(ErrCode: Word): Boolean;
var
    I       : Integer;
    LRcvd   : Integer;
    LBufIdx : Integer;
begin
    if FHttpTunnelState = htsData then
        Result := inherited TriggerDataAvailable(ErrCode)
    else if FHttpTunnelState in [htsWaitResp0, htsWaitResp1, htsWaitResp2] then begin
        if ErrCode <> 0 then begin
            FHttpTunnelLastResponse := 'THttpTunnelWSocket - DataAvailable error';
            TriggerHttpTunnelError(ErrCode);
            Result := FALSE;
            Exit;
        end;

        Result := TRUE;

        LRcvd := Receive(@FHttpTunnelBuf[FHttpTunnelRcvdCnt],
                         FHttpTunnelBufSize - FHttpTunnelRcvdCnt);
        if LRcvd <= 0 then
            Exit;

        Inc(FHttpTunnelRcvdCnt, LRcvd);
        I := 0;
        LBufIdx := 0;
        while I < FHttpTunnelRcvdCnt do begin
            { Parse header lines, omit CRLF }
            if FHttpTunnelWaitingBody or (FHttpTunnelState = htsData) then
                Break;
            if (FHttpTunnelBuf[I] = $0A) then
            begin
                { Some proxies use just a LF as the end-of-line marker }
                { which violates the HTTP specs.                       }
                if (I > 0) and (FHttpTunnelBuf[I - 1] = $0D) then
                begin
                    if not HttpTunnelProcessHdrLine(@FHttpTunnelBuf[LBufIdx],
                                                   (I - 1) - LBufIdx) then
                        Exit;
                end
                else begin
                    if not HttpTunnelProcessHdrLine(@FHttpTunnelBuf[LBufIdx],
                                                    I - LBufIdx) then
                        Exit;
                end;
                LBufIdx := I + 1;
            end;
            Inc(I);
        end;

        if not FHttpTunnelWaitingBody then begin
            { Still in header or request sent or application data follows }
            if (LBufIdx > 0) and (FHttpTunnelRcvdCnt >= LBufIdx) then begin
                Dec(FHttpTunnelRcvdCnt, LBufIdx);
                if FHttpTunnelRcvdCnt > 0 then begin
                    Move(FHttpTunnelBuf[LBufIdx], FHttpTunnelBuf[0],
                         FHttpTunnelRcvdCnt);
                    { We've buffered data, we MUST ensure it's read!         }
                    { Otherwise the application might wait for it infinitely }
                    if (FHttpTunnelState = htsData) and (FState = wsConnected) then
                        PostMessage(Handle, FMsg_WM_ASYNCSELECT, WPARAM(FHSocket),
                                     LPARAM(FD_READ));
                end;
            end;
        end
        else begin { Skip body data }
            if FHttpTunnelChunked then begin
                { Skip body data with chunked transfer encoding }
                while (LBufIdx < FHttpTunnelRcvdCnt) and
                      (FHttpTunnelChunkState <> htcsDone) do begin
                    if FHttpTunnelChunkState = htcsGetSize then begin
                        while (LBufIdx < FHttpTunnelRcvdCnt) do begin
                            if not IsXDigit(AnsiChar(FHttpTunnelBuf[LBufIdx])) then begin
                                FHttpTunnelChunkState := htcsGetExt;
                                break;
                            end;
                            FHttpTunnelChunkSize := FHttpTunnelChunkSize * 16 +
                                     XDigit(AnsiChar(FHttpTunnelBuf[LBufIdx]));
                            Inc(LBufIdx);
                        end;
                    end;

                    if FHttpTunnelChunkState = htcsGetExt then begin
                        while (LBufIdx < FHttpTunnelRcvdCnt) do begin
                            if FHttpTunnelBuf[LBufIdx] = $0A then begin
                                if FHttpTunnelChunkSize > 0 then
                                    FHttpTunnelChunkState := htcsGetData
                                else
                                    FHttpTunnelChunkState := htcsGetBoundary;
                                Inc(LBufIdx);
                                Break;
                            end;
                            Inc(LBufIdx);
                        end;
                    end;

                    if FHttpTunnelChunkState = htcsGetData then begin
                        I := FHttpTunnelRcvdCnt - LBufIdx; // Remaining
                        if FHttpTunnelChunkRcvd + I < FHttpTunnelChunkSize then begin
                            Inc(LBufIdx, I);
                            Inc(FHttpTunnelChunkRcvd, I);
                        end
                        else if FHttpTunnelChunkRcvd + I >= FHttpTunnelChunkSize then begin
                            Inc(LBufIdx, FHttpTunnelChunkSize - FHttpTunnelChunkRcvd);
                            FHttpTunnelChunkSize  := 0;
                            FHttpTunnelChunkRcvd  := 0;
                            FHttpTunnelChunkState := htcsGetSize;
                        end;
                    end;

                    if FHttpTunnelChunkState = htcsGetBoundary then begin
                        while (LBufIdx < FHttpTunnelRcvdCnt) do begin
                            if FHttpTunnelBuf[LBufIdx] = $0A then begin
                                Inc(LBufIdx);
                                FHttpTunnelChunkState := htcsDone;
                                Break;
                            end;
                            Inc(LBufIdx);
                        end;
                    end;

                    if FHttpTunnelChunkState = htcsDone then begin
                        HttpTunnelTriggerResultOrContinue;
                        FHttpTunnelChunked := FALSE;
                    end;
                end;

                if LBufIdx > 0 then begin
                    Dec(FHttpTunnelRcvdCnt, LBufIdx);
                    if FHttpTunnelRcvdCnt > 0 then begin
                        Move(FHttpTunnelBuf[LBufIdx], FHttpTunnelBuf[0],
                             FHttpTunnelRcvdCnt);
                        { We've buffered data, we MUST ensure it is read!        }
                        { Otherwise the application might wait for it infinitely }
                        if (FHttpTunnelChunkState = htcsDone) and
                           (FState = wsConnected) then
                            PostMessage(Handle, FMsg_WM_ASYNCSELECT,
                                         WPARAM(FHSocket), LPARAM(FD_READ));
                    end;
                end;
            end
            else begin
               { Skip body data with Content-Length header }
                I := FHttpTunnelRcvdCnt - LBufIdx; // Remaining
                if FHttpTunnelContentLength >= I then begin
                    FHttpTunnelRcvdCnt := 0;
                    Dec(FHttpTunnelContentLength, I);
                end
                else begin
                    Inc(LBufIdx, FHttpTunnelContentLength);
                    Dec(FHttpTunnelRcvdCnt, LBufIdx);
                    FHttpTunnelContentLength := 0;
                    if FHttpTunnelRcvdCnt > 0 then begin
                        Move(FHttpTunnelBuf[LBufIdx], FHttpTunnelBuf[0],
                             FHttpTunnelRcvdCnt);
                    end;
                end;
                if FHttpTunnelContentLength = 0 then begin
                    HttpTunnelTriggerResultOrContinue;
                    { We've buffered data, we MUST ensure it is read!        }
                    { Otherwise the application might wait for it infinitely }
                    if (FHttpTunnelRcvdCnt > 0) and (FState = wsConnected) then
                        PostMessage(Handle, FMsg_WM_ASYNCSELECT,
                                     WPARAM(FHSocket), LPARAM(FD_READ));
                end;
            end;
        end;
        if FHttpTunnelRcvdCnt = FHttpTunnelBufSize then begin
          { No CRLF found in entire buffer (default size 1024 bytes) }
            FHttpTunnelLastResponse := 'Received header line too long. ' +
                                       'Increase HttpTunnelBufferSize.';
            TriggerHttpTunnelError(ICS_HTTP_TUNNEL_PROTERR);
        end;
    end
    else begin
        Result := FALSE; // Should never happen
        FHttpTunnelLastResponse := 'THttpTunnelWSocket - Fatal: Invalid state ' +
                                  'in TriggerDataAvailable';
        TriggerHttpTunnelError(FHttpTunnelStatusCode);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.TriggerSessionClosed(ErrCode: Word);
begin
    if FHttpTunnelState <> htsData then begin
        FHttpTunnelLastResponse :=
          AnsiString(WSocketErrorMsgFromErrorCode(ICS_HTTP_TUNNEL_GENERR));
        TriggerHttpTunnelError(ICS_HTTP_TUNNEL_GENERR);
    end
    else if FHttpTunnelReconnectRequest <> htrrNone then
        { Start ansynchronous reconnect by posting a custom message. }
        { This message is processed in method WMHttpTunnelReconnect. }
        PostMessage(Handle, FMsg_WM_TUNNEL_RECONNECT, 0, 0)
    else
        inherited TriggerSessionClosed(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.TriggerHttpTunnelConnected(ErrCode: Word);
begin
    if Assigned(FOnHttpTunnelConnected) then
        FOnHttpTunnelConnected(Self, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.TriggerHttpTunnelError(ErrCode: Word);
begin
    if ErrCode < ICS_HTTP_TUNNEL_MAXSTAT then
        ErrCode := ICS_HTTP_TUNNEL_BASEERR + ErrCode;
    if Assigned(FOnHttpTunnelError) then
        FOnHttpTunnelError(Self, ErrCode, FHttpTunnelServerAuthTypes,
                           String(FHttpTunnelLastResponse));
    FHttpTunnelState            := htsData;
    FHttpTunnelReconnectRequest := htrrNone;
    if FHttpTunnelCurAuthType <> FHttpTunnelAuthType then
        FHttpTunnelCurAuthType := FHttpTunnelAuthType;
    TriggerSessionConnected(ErrCode);
    InternalClose(TRUE, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.TriggerSessionConnectedSpecial(
  ErrCode: Word);
begin
    if FHttpTunnelState = htsConnecting then begin
        if ErrCode = 0 then
            FHttpTunnelState := htsConnected;
        TriggerHttpTunnelConnected(ErrCode);
        if ErrCode <> 0 then begin
            FHttpTunnelState            := htsData;
            FHttpTunnelReconnectRequest := htrrNone;
            inherited TriggerSessionConnectedSpecial(ErrCode);
        end
        else begin
            if FHttpTunnelReconnectRequest <> htrrNone then begin
                { This block is only executed after the component performed   }
                { a silent reconnect in the background after status code 407, }
                { FHttpTunnelReconnectRequest is reset in the send-functions. }
                case FHttpTunnelReconnectRequest of
                    htrrBasic  : HttpTunnelSendAuthBasic;
                {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
                    htrrDigest : HttpTunnelSendAuthDigest;
                {$ENDIF}
                    htrrNtlm1  : HttpTunnelSendAuthNtlm_1;
                end;
            end
            else begin
                if (FHttpTunnelUsercode <> '') then begin
                    if (FHttpTunnelAuthType <> htatDetect) and
                       (FHttpTunnelCurAuthType <> FHttpTunnelAuthType) then
                        FHttpTunnelCurAuthType := FHttpTunnelAuthType;
                end
                else begin
                    FHttpTunnelCurAuthType := htatNone;
                end;
                case FHttpTunnelCurAuthType of
                    htatDetect,
                    htatNone     : HttpTunnelSendPlainConnect;
                    htatBasic    : HttpTunnelSendAuthBasic;
                {$IFNDEF NO_HTTP_TUNNEL_AUTHDIGEST}
                    htatDigest   : if FHttpTunnelAuthDigestCached then
                                        HttpTunnelSendAuthDigest
                                   else
                                        HttpTunnelSendPlainConnect;
                {$ENDIF}
                    htatNtlm     : HttpTunnelSendAuthNtlm_1;
                end;
            end;

        end;
    end
    else
        inherited TriggerSessionConnectedSpecial(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.WMHttpTunnelReconnect(var MsgRec: TMessage);
begin
    if FHttpTunnelReconnectRequest <> htrrNone then
        Connect
    else begin // May not happen
        FHttpTunnelLastResponse := 'THttpTunnelWSocket - Fatal: Internal error ' +
                                   'in WMHttpTunnelReconnect';
        TriggerHttpTunnelError(FHttpTunnelStatusCode);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomHttpTunnelWSocket.WndProc(var MsgRec: TMessage);
begin
    if MsgRec.Msg = FMsg_WM_TUNNEL_RECONNECT then
    try
        WMHttpTunnelReconnect(MsgRec)
    except
        on E: Exception do
            HandleBackGroundException(E);
    end
    else
        inherited WndProc(MsgRec);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsAsyncSocketThread }   { Arno Garrels 8.11.2011 }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF POSIX}
procedure TIcsAsyncSocketThread.TerminateThread;
begin
  Terminate;
  if Assigned(FEventQueue) then
    FEventQueue.WakeUp; // Wakeup thread
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncSocketThread.Execute;
begin
{$IFDEF DEBUG}
  IcsNameThreadForDebugging(AnsiString(ClassName));
{$ENDIF}
  while not Terminated do
  begin
    if not FEventQueue.HandleEvents then
    begin
      raise EIcsEventQueue.Create(SysErrorMessage(GetLastError));
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsEventQueue } { Arno Garrels 8.11.2011 }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
  IcsEventQueueIgnoreFlag = LongWord(-1);

{$IFDEF NEVER}
function TIcsEventQueue.KQueueEnableReadEvent(FD: Integer; UData: NativeInt;
  Edge: Boolean): Boolean;
var
  LFlags : Word;
  Evt: TKEvent;
begin
  if Edge then // is it required?
    LFlags := EV_ENABLE or EV_CLEAR
  else
    LFlags := EV_ENABLE;

  EV_SET(@Evt, FD, EVFILT_READ, LFlags, 0, 0, Pointer(UData));
  Result := KEvent(FQueue, @Evt, 1, nil, 0, nil) > -1;
  if not Result then
    raise EIcsEventQueue.Create(_SysErrorMessage(errno));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.KQueueDisableReadEvent(FD: Integer;
  UData: NativeInt): Boolean;
var
  Evt: TKEvent;
begin
  EV_SET(@Evt, FD, EVFILT_READ, EV_DISABLE, 0, 0, Pointer(UData));
  Result := KEvent(FQueue, @Evt, 1, nil, 0, nil) > -1;
  if not Result then
    raise EIcsEventQueue.Create(_SysErrorMessage(errno));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.KQueueAddReadEvent(FD: Integer; UData: NativeInt;
  Edge: Boolean): Boolean;
var
  Evt: TKevent;
  LFlags: Word;
begin
  if Edge then
    LFlags := EV_ADD or EV_CLEAR  // edge triggered
  else
    LFlags := EV_ADD;
  EV_SET(@Evt, FD, EVFILT_READ, LFlags, 0, 0, Pointer(UData));
  Result := KEvent(FQueue, @Evt, 1, nil, 0, nil) > -1;
  if not Result then
    raise EIcsEventQueue.Create(_SysErrorMessage(errno));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.KQueueAddWriteEvent(FD: Integer; UData: NativeInt): Boolean;
var
  Evt: TKevent;
begin
  EV_SET(@Evt, FD, EVFILT_WRITE, EV_ADD or EV_CLEAR, 0, 0, Pointer(UData)); // edge triggered
  Result := KEvent(FQueue, @Evt, 1, nil, 0, nil) > -1;
  if not Result then
    raise EIcsEventQueue.Create(_SysErrorMessage(errno));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.KQueueRemoveReadEvent(FD: Integer): Boolean;
var
  Evt: TKevent;
begin
  EV_SET(@Evt, FD, EVFILT_READ, EV_DELETE, 0, 0, nil);
  Result := KEvent(FQueue, @Evt, 1, nil, 0, nil) > -1;
  if not Result then
    raise EIcsEventQueue.Create(_SysErrorMessage(errno));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.KQueueRemoveWriteEvent(FD: Integer): Boolean;
var
  Evt: TKevent;
begin
  EV_SET(@Evt, FD, EVFILT_WRITE, EV_DELETE, 0, 0, nil);
  Result := KEvent(FQueue, @Evt, 1, nil, 0, nil) > -1;
  if not Result then
    raise EIcsEventQueue.Create(_SysErrorMessage(errno));
end;
{$ENDIF NEVER}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.AddToObjIdentList(IEventSrc: IIcsEventSource);
begin
  if not FObjIdentList.ContainsKey(IEventSrc.ObjectID) then
    FObjIdentList.Add(IEventSrc.ObjectID, IEventSrc.GetObject);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.RemoveFromObjIdentList(IEventSrc: IIcsEventSource);
begin
  if FObjIdentList.ContainsKey(IEventSrc.ObjectID) then
    FObjIdentList.Remove(IEventSrc.ObjectID);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.AddReadEvent(FD: Integer; UData: NativeInt;
  Edge: Boolean);
var
  LFlags: Word;
begin
  if FFreeIndex >= FCapacity then
    Grow;

  if Edge then
    LFlags := EV_ADD or EV_CLEAR  // edge triggered
  else
    LFlags := EV_ADD;

  EV_SET(@FChangeList[FFreeIndex], FD, EVFILT_READ,
         LFlags, 0, 0, Pointer(UData));
  Inc(FFreeIndex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.AddWriteEvent(FD: Integer; UData: NativeInt);
begin
  { Write filter is always edge triggered }
  if FFreeIndex >= FCapacity then
    Grow;
  EV_SET(@FChangeList[FFreeIndex], FD, EVFILT_WRITE,
         EV_ADD or EV_CLEAR, 0, 0, Pointer(UData));
  Inc(FFreeIndex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.RemoveReadEvent(FD: Integer; UData: NativeInt);
var
  I: Integer;
begin
  { Keeps the true change list short }
  for I := 0 to FFreeIndex -1 do
  begin
    if (FChangeList[I].Ident = NativeUInt(FD)) and
       (FChangeList[I].Filter = EVFILT_READ) and
       (FChangeList[I].UData = Pointer(UData)) then
      FChangeList[I].FFlags := IcsEventQueueIgnoreFlag;
  end;

  if FFreeIndex >= FCapacity then
    Grow;
  EV_SET(@FChangeList[FFreeIndex], FD, EVFILT_READ,
         EV_DELETE, 0, 0, nil);
  Inc(FFreeIndex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.RemoveWriteEvent(FD: Integer; UData: NativeInt);
begin
  if FFreeIndex >= FCapacity then
    Grow;
  EV_SET(@FChangeList[FFreeIndex], FD, EVFILT_WRITE,
         EV_DELETE, 0, 0, nil);
  Inc(FFreeIndex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.DisableReadEvent(FD: Integer; UData: NativeInt);
var
  I: Integer;
begin
  { Keeps the true change list short }
  for I := 0 to FFreeIndex -1 do
  begin
    if (FChangeList[I].Ident = NativeUInt(FD)) and
       (FChangeList[I].Filter = EVFILT_READ) and
       (FChangeList[I].uData = Pointer(UData)) then
    begin
      if FChangeList[I].Flags = EV_ENABLE then  // socket wanted to enable
      begin
        FChangeList[I].Flags  := EV_DISABLE;
        FChangeList[I].FFlags := IcsEventQueueIgnoreFlag;
        Exit;
      end
      else if FChangeList[I].Flags = EV_DISABLE then
        Exit;
    end;
  end;

  if FFreeIndex >= FCapacity then
    Grow;
  EV_SET(@FChangeList[FFreeIndex], FD, EVFILT_READ,
         EV_DISABLE, 0, 0, Pointer(UData));
  Inc(FFreeIndex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.EnableReadEvent(FD: Integer; UData: NativeInt): Boolean;
var
  I: Integer;
begin
  { Keeps the true change list short }
  for I := 0 to FFreeIndex -1 do
  begin
    if (FChangeList[I].Ident = NativeUInt(FD)) and
       (FChangeList[I].Filter = EVFILT_READ) and
       (FChangeList[I].uData = Pointer(UData)) then
    begin
      if FChangeList[I].Flags = EV_DISABLE then // thread wanted to disable
      begin
        FChangeList[I].Flags  := EV_ENABLE;
        FChangeList[I].FFlags := IcsEventQueueIgnoreFlag;
        Exit(False);
      end
      else if (FChangeList[I].Flags = EV_ENABLE) then
        Exit(False);
    end;
  end;

  if FFreeIndex >= FCapacity then
    Grow;
  EV_SET(@FChangeList[FFreeIndex], FD, EVFILT_READ,
         EV_ENABLE, 0, 0, Pointer(UData));
  Inc(FFreeIndex);
  Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.InternalRemoveEvents(IEventSrc: IIcsEventSource;
  FdClosed: Boolean = False);
begin
  if not FdClosed then
  begin
    if (FD_ACCEPT and IEventSrc.EventMask = FD_ACCEPT) or
       (FD_READ and IEventSrc.EventMask = FD_READ) then
      RemoveReadEvent(IEventSrc.FileDescriptor, IEventSrc.ObjectID);
    if (FD_WRITE and IEventSrc.EventMask = FD_WRITE) or
       (FD_CONNECT and IEventSrc.EventMask = FD_CONNECT) then
      RemoveWriteEvent(IEventSrc.FileDescriptor, IEventSrc.ObjectID);
  end;

  RemoveFromObjIdentList(IEventSrc);

  IEventSrc.EventMask       := 0;
  IEventSrc.NotifyWindow    := 0;
  IEventSrc.EventState      := [];
  IEventSrc.FileDescriptor  := INVALID_SOCKET;
  IEventSrc.NotifyMessageID := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.SynchronizedRemoveEvents(IEventSrc: IIcsEventSource;
  FdClosed: Boolean): Boolean;
begin
  if (IEventSrc.EventMask <> 0) and (IEventSrc.FileDescriptor <> INVALID_SOCKET) then
  begin
    FQueueSection.Enter;
    try
      InternalRemoveEvents(IEventSrc, FdClosed);
      if (not FdClosed) and FRequireWakeup then
        Result := Notify(0)
      else
        Result := True;
    finally
      FQueueSection.Leave;
    end;
  end
  else
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.SynchronizedEnableReadEvent(IEventSrc: IIcsEventSource): Boolean;
begin
  FQueueSection.Enter;
  try
    Result := FD_READ and IEventSrc.EventMask = FD_READ;
    if Result then
    begin
      if EnableReadEvent(IEventSrc.FileDescriptor, IEventSrc.ObjectID) and
         FRequireWakeup then
        Result := Notify(0);
    end;
  finally
    FQueueSection.Leave;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.SynchronizedEnableAcceptEvent(IEventSrc: IIcsEventSource): Boolean;
begin
  FQueueSection.Enter;
  try
    Result := IEventSrc.EventMask and FD_ACCEPT = FD_ACCEPT;
    if Result then
    begin
      if EnableReadEvent(IEventSrc.FileDescriptor, IEventSrc.ObjectID) and
         FRequireWakeup then
        Result := Notify(0);
    end;
  finally
    FQueueSection.Leave;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.SynchronizedSetShutdownCalled(
  IEventSrc: IIcsEventSource; How: Integer);
begin
  FQueueSection.Enter;
  try
    case How of
      0 : IEventSrc.EventState := IEventSrc.EventState  + [aesShutDown0Called];
      1 : IEventSrc.EventState := IEventSrc.EventState  + [aesShutDown1Called];
      2 : IEventSrc.EventState := IEventSrc.EventState  + [aesShutDown0Called, aesShutDown1Called];
    end;
  finally
    FQueueSection.Leave;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.CheckChangeEvent(FD: Integer; UData: NativeInt;
  const OldMask: LongWord; var NewMask: LongWord): Boolean;
begin
  Result := False;
  if (FD_WRITE and OldMask = FD_WRITE) or (FD_CONNECT and OldMask = FD_CONNECT) then
  begin
    if not ((FD_WRITE and NewMask = FD_WRITE) or (FD_CONNECT and NewMask = FD_CONNECT)) then
    begin
      RemoveWriteEvent(FD, UData);
      Result := True;
    end;
  end
  else begin
    if (FD_WRITE and NewMask = FD_WRITE) or (FD_CONNECT and NewMask = FD_CONNECT) then
    begin
      AddWriteEvent(FD, UData);
      Result := True;
    end;
  end;

  if (FD_READ and OldMask = FD_READ) or (FD_ACCEPT and OldMask = FD_ACCEPT) then
  begin
    if not ((FD_READ and NewMask = FD_READ) or (FD_ACCEPT and NewMask = FD_ACCEPT)) then
    begin
      RemoveReadEvent(FD, UData);
      Result := True;
    end;
  end
  else begin
    if (FD_READ and NewMask = FD_READ) or (FD_ACCEPT and NewMask = FD_ACCEPT) then
    begin
      AddReadEvent(FD, UData, False); // edge trigger makes trouble, don't use
      Result := True;
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.InternalAsyncSelect(IEventSrc: IIcsEventSource;
  AWndHandle: HWND; AMsgID: UINT; AEvents: LongWord; AWakeupThread: Boolean): Integer;
var
  LDirty: Boolean;
begin
  {if IEventSrc.FileDescriptor = INVALID_SOCKET then // Check done in SynchronizedAsyncSelect
    Exit(SOCKET_ERROR); }
  LDirty := CheckChangeEvent(IEventSrc.FileDescriptor, IEventSrc.ObjectID,
                             IEventSrc.EventMask, AEvents);
  IEventSrc.EventMask := AEvents;
  IEventSrc.NotifyWindow := AWndHandle;
  IEventSrc.NotifyMessageID := AMsgID;

  if LDirty then
  begin
    if (AEvents = 0) then
      RemoveFromObjIdentList(IEventSrc)
    else
      AddToObjIdentList(IEventSrc);
    if AWakeupThread and FRequireWakeup then
      Wakeup;
  end;
  Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.SynchronizedAsyncSelect(IEventSrc: IIcsEventSource;
  FD: Integer; AWndHandle: HWND; AMsgID: UINT; AEvents: LongWord): Integer;

  function SetNonBlocking: Boolean;
  var
    LFlags : Integer;
  begin
    Result := True;
    LFlags := Posix.Fcntl.fcntl(FD, F_GETFL);
    if LFlags < 0 then
      Exit(False);
    if (LFlags and O_NONBLOCK <> 0) then
      Exit;
    LFlags := (LFlags or O_NONBLOCK);
    Result := Posix.Fcntl.fcntl(FD, F_SETFL, LFlags) <> -1;
  end;

begin
  if not SetNonBlocking then
    Exit(SOCKET_ERROR);
  if not IsWindow(AWndHandle) then
  begin
    SetLastError(ERROR_INVALID_WINDOW_HANDLE);
    Exit(SOCKET_ERROR);
  end;
  FQueueSection.Enter;
  try
    IEventSrc.FileDescriptor := FD;
    Result := InternalAsyncSelect(IEventSrc, AWndHandle, AMsgID, AEvents, True);
  finally
    FQueueSection.Leave;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.HandleEvents: Boolean;
type
  TEvtDelAction = (edaNone, edaAll, edaWrite);
var
  nEvents   : Integer;
  I         : Integer;
  IEventSrc : IIcsEventSource;
  LoParam   : Word;
  HiParam   : Word;
  CurEvt    : TKEvent;
  nChanges  : Integer;
  LLastErr  : Integer;
  LEvtMask  : LongWord;
  LDelFlag  : TEvtDelAction;
  LEof      : Boolean;
  Buf       : Byte;
  LMsgID    : UINT;
  LHwnd     : HWND;
  LPostMsgFunc : function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): Boolean;
begin
  FQueueSection.Enter;
  try
    if FInLoop then
      Exit(True);
    FInLoop := True;
    FRequireWakeup := True;
    nChanges := FFreeIndex;
    if nChanges > 0 then
    begin
      FFreeIndex := 0;//**
      if FThrdChangeListLen < nChanges then
      begin
        FThrdChangeListLen := nChanges * 2;
        SetLength(FThreadChangeList, FThrdChangeListLen);
      end;
      nEvents := 0;
      for I := 0 to nChanges -1 do
      begin
        if (FChangeList[I].FFlags <> IcsEventQueueIgnoreFlag) then
        begin
          FThreadChangeList[nEvents] := FChangeList[I];
          Inc(nEvents);
        end; // else skip this one
      end;
      nChanges := nEvents;
      if nEvents = 0 then
        nEvents := 1;
    end
    else
      nEvents := 1;
    { FEventList length at least nEvents }
    if FEventListLen < nEvents then
    begin
      FEventListLen := nEvents;
      SetLength(FEventList, FEventListLen);
    end;
  finally
    FQueueSection.Leave;
  end;

  { Set changes and wait for events }
  nEvents := KEvent(FQueue, @FThreadChangeList[0], nChanges,
                    @FEventList[0], nEvents, nil);

  FQueueSection.Enter;
  try
    FRequireWakeup := False;
    if (nEvents < 0) then
      Exit(errno = EINTR);
    for I := 0 to nEvents -1 do
    begin
      CurEvt := FEventList[I];
      if (CurEvt.uData = nil) then
        Continue;
      LDelFlag      := edaNone;
      LoParam       := 0;
      HiParam       := CurEvt.FFlags; // IO error code
      LEof          := CurEvt.Flags and EV_EOF = EV_EOF;
      IEventSrc     := nil;

      { Error on add or delete events }
      if (CurEvt.Flags and EV_ERROR <> 0) and (CurEvt.Data <> 0) then
      begin
        { Error descriptions from LibEvent }
        case CurEvt.Data of
          { Can occur on delete if we are not currently
            watching any events on this fd.  That can
            happen when the fd was closed and another
            file was opened with that fd. }
          ENOENT,
          { Can occur for reasons not fully understood
            on FreeBSD. }
          EINVAL:
            Continue;
          { Can occur on a delete if the fd is closed.  Can
            occur on an add if the fd was one side of a pipe,
            and the other side was closed. }
          EBADF,
          { These two can occur on an add if the fd was one side
            of a pipe, and the other side was closed. }
          EPERM,
          EPIPE:
            begin
              if IntPtr(CurEvt.uData) = -1 then  // pipe error
              begin
                SetLastError(Integer(CurEvt.Data));
                Exit(False);
              end
              else begin
                CurEvt.Data := CurEvt.Data; { Debug }
                Continue;
              end;
            end;
          else // case
            { Other errors shouldn't occur. }
            SetLastError(Word(CurEvt.Data));
            Exit(False);
        end; // case
      end // Error on add or delete events

      { Triggered Events }

      else if CurEvt.Ident = LongWord(FPipeFd.Read) then
      begin
        { It's our pipe just read and continue }
        __read(FPipeFd.Read, @Buf, SizeOf(Buf));
        Continue;
      end

      { Socket events }

      else if FObjIdentList.ContainsKey(NativeInt(CurEvt.uData)) then
      begin
        FObjIdentList.Items[NativeInt(CurEvt.uData)].GetInterface(IIcsEventSource, IEventSrc);
        Assert(IEventSrc <> nil); { Debug }
        Assert(Integer(CurEvt.Ident) = IEventSrc.FileDescriptor); { Debug }

        LEvtMask := IEventSrc.EventMask;

        { Trigger Write, Connect or Close }
        if CurEvt.Filter = EVFILT_WRITE then
        begin
          { Trigger Connect }
          if not (aesConnectNotified in IEventSrc.EventState) then
          begin
            IEventSrc.EventState := IEventSrc.EventState + [aesConnectNotified];
            if FD_CONNECT and LEvtMask = FD_CONNECT then
              LoParam := FD_CONNECT
            else if FD_WRITE and LEvtMask = FD_WRITE then
              LoParam := FD_WRITE;
            if LEof then
              LDelFlag := edaAll; // Delete both read and write events
          end
          { Trigger Write or Close }
          else begin
            if LEof then
            begin
              { Application called shutdown(1) (disabled writes) and a    }
              { FIN ACK was sent to the peer or the peer sent us a RST.   }
              { If a read filter is still active we'll get another close  }
              { event when the peer acked the FIN request.                }
              if (aesShutDown1Called in IEventSrc.EventState) then
              begin
                if (not (aesCloseNotified in IEventSrc.EventState)) and
                   ((FD_READ and LEvtMask = FD_READ) or
                    (FD_ACCEPT and LEvtMask = FD_ACCEPT)) then
                  { So we will be notified when the peer send us FIN ACK }
                  EnableReadEvent(CurEvt.Ident, IEventSrc.ObjectID);
                LDelFlag := edaWrite; // Delete write event
              end
              else if (not (aesCloseNotified in IEventSrc.EventState)) then
              begin
                { Post a close or write message, it's likely we got a RST }
                { and an error code in HiParam.                           }
                if (FD_CLOSE and LEvtMask = FD_CLOSE) then
                  LoParam := FD_CLOSE
                else if FD_WRITE and LEvtMask = FD_WRITE then
                  LoParam := FD_WRITE; // on socket write -> socket error
                IEventSrc.EventState := IEventSrc.EventState + [aesCloseNotified];
                LDelFlag := edaAll    // Delete all
              end;
            end
            else if FD_WRITE and LEvtMask = FD_WRITE then
              LoParam := FD_WRITE;
          end;
        end

        { Trigger Read, Accept or Close }
        else if CurEvt.Filter = EVFILT_READ then
        begin
          if LEof then
          begin
            { Peer closed the connection or application called shutdown(0) }
            { (disabled reads). If the application called shutdown(0) we   }
            { should not trigger FD_CLOSE but disable read events. If the  }
            { peer closed the connection it is likely still data to be     }
            { read.                                                        }
            if aesShutDown0Called in IEventSrc.EventState then
              IEventSrc.EventState := IEventSrc.EventState - [aesShutDown0Called]
            else if not (aesCloseNotified in IEventSrc.EventState) then
            begin
              IEventSrc.EventState := IEventSrc.EventState + [aesCloseNotified];
              if (FD_CLOSE and LEvtMask = FD_CLOSE) then
                LoParam := FD_CLOSE
              else if FD_READ and LEvtMask = FD_READ then
                LoParam := FD_READ; // on socket read -> socket error
              if CurEvt.Data = 0 then
                LDelFlag := edaAll;
            end;
            if (FD_READ and LEvtMask = FD_READ) and (CurEvt.Data > 0) then
              { CurEvt.Data contains the number of bytes ready to be read. }
              { If > 0 bitwise or FD_READ, the message handler executes    }
              { FD_READ before FD_CLOSE.                                   }
              LoParam := LoParam or FD_READ;
          end // LEof
          { Trigger Accept }
          else if FD_ACCEPT and LEvtMask = FD_ACCEPT then
            LoParam := FD_ACCEPT
          { Trigger Read }
          else if FD_READ and LEvtMask = FD_READ then
            LoParam := FD_READ;

          if LDelFlag <> edaAll then
            DisableReadEvent(CurEvt.Ident, IEventSrc.ObjectID);
        end;

        { Copy these since they may be cleared when LDelFlag is handled below }
        LMsgID := IEventSrc.NotifyMessageID;
        LHwnd  := IEventSrc.NotifyWindow;

        { Process LDelFlag here since we might have to leave the critical     }
        { section when PostMessage() below failed due to a full queue         }

        case LDelFlag of
          edaAll :
            InternalRemoveEvents(IEventSrc);
          edaWrite :
            begin
              if (LEvtMask and FD_READ = FD_READ) or
                 (LEvtMask and FD_ACCEPT = FD_ACCEPT) then
              begin
                RemoveWriteEvent(CurEvt.Ident, IEventSrc.ObjectID);
                IEventSrc.EventMask := (LEvtMask and not FD_WRITE) and not FD_CONNECT;
              end
              else
                InternalRemoveEvents(IEventSrc);
            end;
        end;

        if LoParam > 0 then // finally post the socket event if any
        begin
          if (LoParam = FD_ACCEPT) then // all events have to be queued/posted
            LPostMsgFunc := PostMessage
          else
            { PostUniqueMessage() posts the message only if it doesn't already }
            { exist in the destination queue. This is especially important     }
            { with FD_READ messages if wsoNoReceiveLoop isn't in the options   }
            { in order to not overflow the queue with useless and performance  }
            { killing messages.                                                }
            LPostMsgFunc := PostUniqueMessage;

          while not LPostMsgFunc(LHwnd, LMsgID, WPARAM(CurEvt.Ident),
                                 LPARAM(IcsMakeLong(LoParam, HiParam))) do
          begin
            LLastErr := GetLastError;
            case LLastErr of
              ERROR_NOT_ENOUGH_QUOTA :
                begin
                  Result := True; // removes compiler warning
                  if FAsyncThread.Terminated then
                    Break;
                  try
                    { Raise a debug exception. Actually this should not happen }
                    { at least not caused by this thread.                      }
                    raise EIcsMessageQueueFull.Create(ClassName + ': Message queue full');
                  except
                    // silent
                  end;
                  { All we can do is wait for destination thread dequeues some  }
                  { messages, we have to leave and reenter the critical section }
                  FQueueSection.Leave;
                  FAsyncThread.Sleep(100);
                  FQueueSection.Enter;
                end;

              ERROR_INVALID_WINDOW_HANDLE,
              ERROR_ACCESS_DENIED, // destination message queue is going down and locked
              EINVAL :
                begin
                  { Should never ever happen, if so, doesn't hurt to ignore }
                  InternalRemoveEvents(IEventSrc);
                  Break;
                end;

              else
                Result := False; // removes compiler warning
                raise EIcsEventQueue.Create(ClassName + ': Unknown error on PostMessage #' +
                  IntToStr(LLastErr));
            end; // case
          end;
        end;
      end; // if FObjIdentList.ContainsKey(NativeInt(CurEvt.uData)) means IEventSrc is assigned
    end; // For Loop

    Result := True;

  finally
    FInLoop := False;
    FQueueSection.Leave;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.DeInit: Boolean;
begin
  if FPipeFd.Read <> INVALID_FILE_HANDLE then
  begin
    __close(FPipeFd.Read);
    FPipeFd.Read := INVALID_FILE_HANDLE;
  end;
  if FPipeFd.Write <> INVALID_FILE_HANDLE then
  begin
    __close(FPipeFd.Write);
    FPipeFd.Write := INVALID_FILE_HANDLE
  end;
  if FQueue <> INVALID_FILE_HANDLE then
  begin
    __close(FQueue);
    FQueue := INVALID_FILE_HANDLE;
  end;
  FFreeIndex := 0;
  Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.Init: Boolean;
begin
  Result := True;
  if FInitialized then
    Exit;
  FQueue := KQueue;
  if FQueue = INVALID_FILE_HANDLE then
    raise Exception.Create(SysErrorMessage(GetLastError));
  if pipe(@FPipeFd) <> 0 then
  begin
    FPipeFd.Read := INVALID_FILE_HANDLE;
    FPipeFd.Write := INVALID_FILE_HANDLE;
    raise Exception.Create('pipe() failed');
  end;
  Grow;
  EV_SET(@FChangeList[FFreeIndex], FPipeFd.Read, EVFILT_READ,
         EV_ADD, 0, 0, Pointer(-1));
  Inc(FFreeIndex);
  FInitialized := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsEventQueue.Create;
begin
  inherited Create;
  FQueueSection := TIcsCriticalSection.Create;
  FObjIdentList := TDictionary<NativeInt, TObject>.Create;
  Init;
  FAsyncThread := TIcsAsyncSocketThread.Create(True);
  FAsyncThread.FreeOnTerminate := False;
  FAsyncThread.FEventQueue := Self;
  FAsyncThread.Start;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsEventQueue.Destroy;
begin
  if Assigned(FAsyncThread) then
  begin
    if not FAsyncThread.Terminated then
      FAsyncThread.TerminateThread;
    FAsyncThread.WaitFor;
    FAsyncThread.Free;
  end;
  DeInit;
  FObjIdentList.Free;
  FQueueSection.Free;
  inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsEventQueue.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  Inc(FCapacity, Delta);
  SetLength(FChangeList, FCapacity);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.Notify(AMsg: Byte): Boolean;
begin
  Result := __write(FPipeFd.Write, @AMsg, SizeOf(AMsg)) = SizeOf(AMsg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsEventQueue.Wakeup: Boolean;
begin
  Result := Notify(0);
end;
{$ENDIF POSIX}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsAsyncDnsLookupThread }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsAsyncDnsLookupThread.Create(ADnsLookup: TIcsAsyncDnsLookup);
begin
    inherited Create(TRUE);
    FDnsResultList := TStringList.Create;
    FDnsLookup := ADnsLookup;
    FEvent := TEvent.Create(nil, TRUE, TRUE, '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsAsyncDnsLookupThread.Destroy;
begin
    Terminate;
    FEvent.SetEvent;
    inherited Destroy;
    FDnsResultList.Free;
    FEvent.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_ResolveName(
    const AName        : string;
    const AReverse     : Boolean;
    const AFamily      : TSocketFamily;
    AResultList        : TStrings;
    const AProtocol    : Integer): Integer;
var
    Hints     : TAddrInfo;
    AddrInfo  : PAddrInfo;
    NextInfo  : PAddrInfo;
    RetVal    : Integer;
    LHost     : {$IFNDEF POSIX} string; {$ELSE} AnsiString; {$ENDIF}
    IDX       : Integer;
begin
    AResultList.Clear;
    FillChar(Hints, SizeOf(Hints), 0);
    if AFamily = sfIPv4 then
        Hints.ai_family := AF_INET
    else if AFamily = sfIPv6 then
        Hints.ai_family := AF_INET6;
    {else
        Hints.ai_family := AF_UNSPEC;}

    if AReverse then
        Hints.ai_flags := AI_NUMERICHOST;

    AddrInfo := nil;
    Hints.ai_protocol := AProtocol;
  {$IFNDEF POSIX}
    Result   := Ics_GetAddrInfo(PChar(AName), nil, @Hints, AddrInfo);
  {$ELSE}
    Result   := GetAddrInfo(PAnsiChar(AnsiString(AName)), nil, Hints, AddrInfo);
  {$ENDIF}
    if Result = 0 then
    try
        IDX := 0;
        NextInfo := AddrInfo;
        while NextInfo <> nil do
        begin
            if (NextInfo.ai_family = AF_INET) or (NextInfo.ai_family = AF_INET6) then
            begin
                if AReverse then
                begin
                    SetLength(LHost, NI_MAXHOST);
                  {$IFDEF MSWINDOWS}
                    RetVal := Ics_GetNameInfo(NextInfo^.ai_addr,
                                          NextInfo^.ai_addrlen,
                                          PChar(LHost), NI_MAXHOST, nil, 0, 0);
                  {$ELSE}
                    RetVal := GetNameInfo(NextInfo^.ai_addr^,
                                          NextInfo^.ai_addrlen,
                                          PAnsiChar(LHost), NI_MAXHOST, nil, 0, 0);
                  {$ENDIF}
                    if RetVal = 0 then
                    begin
                      {$IFNDEF POSIX}
                        AResultList.Add(PChar(LHost));
                      {$ELSE}
                        AResultList.Add(PChar(string(LHost)));
                      {$ENDIF}
                    end
                    else begin
                        Result := WSocket_Synchronized_WSAGetLastError;{WSAGetLastError;}// Or just the RetVal.
                        Break;
                    end;
                end
                else begin
                    if NextInfo.ai_family = AF_INET then
                    begin
                        if AFamily = sfAnyIPv4 then
                        begin
                            AResultList.Insert(IDX,
                            WSocketIPv4ToStr(PSockAddrIn(NextInfo.ai_addr)^.sin_addr.S_addr));
                            Inc(IDX);
                        end
                        else
                            AResultList.Add(
                            WSocketIPv4ToStr(PSockAddrIn(NextInfo.ai_addr)^.sin_addr.S_addr));
                    end
                    else begin
                        if AFamily = sfAnyIPv6 then
                        begin
                            AResultList.Insert(IDX,
                            WSocketIPv6ToStr(PSockAddrIn6(NextInfo.ai_addr)));
                            Inc(IDX);
                        end
                        else
                            AResultList.Add(
                            WSocketIPv6ToStr(PSockAddrIn6(NextInfo.ai_addr)));
                    end;
                end;
            end;
            NextInfo := NextInfo.ai_next;
        end;
    finally
      {$IFDEF MSWINDOWS}
        Ics_FreeAddrInfo(AddrInfo);
      {$ELSE}
        FreeAddrInfo(AddrInfo^);
      {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ResolveName(
    const AName      : string;
    const AReverse   : Boolean;
    const AFamily    : TSocketFamily;
    AResultList      : TStrings;
    const AProtocol  : Integer): Integer;
begin
{$IFNDEF NO_ADV_MT}
    SafeIncrementCount;
    try
{$ENDIF}
        Result := WSocket_Synchronized_ResolveName(AName, AReverse, AFamily,
                                                   AResultList, AProtocol);
{$IFNDEF NO_ADV_MT}
    finally
        SafeDecrementCount;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncDnsLookupThread.Execute;
var
    Request : TIcsAsyncDnsLookupRequest;
    LRes : WORD;
{$IFDEF POSIX}
    LMsgPump : TIcsMessagePump;
{$ENDIF}
begin
{$IFDEF DEBUG}
    IcsNameThreadForDebugging('TIcsAsyncDnsLookupThread');
{$ENDIF}
{$IFDEF POSIX}
    LMsgPump := TIcsMessagePump.Create; // Used for SendMessage
    try
{$ENDIF}
    while not Terminated do
    begin
        case FEvent.WaitFor(FDnsLookup.FThreadIdleTimeoutMsec) of
            wrError :
                begin
                    if not Terminated then
                    {$IFDEF MSWINDOWS}
                        raise Exception.Create(SysErrorMessage(FEvent.LastError));
                    {$ELSE}
                        raise Exception.Create(SysErrorMessage(GetLastError));
                    {$ENDIF}
                end;
            wrAbandoned : Break;
            wrTimeout :
                begin
                    //if _TryEnterCriticalSection(FDnsLookup.FThreadsLock) then
                    if FDnsLookup.FThreadsLock.TryEnter then
                    try { If we cannot enter the critsec next timeout might be }
                        { successful. EnterCriticalSection may not be called   }
                        { since it might lead to a deadlock when FDnsLookup    }
                        { is freed.                                            }
                        if (not FBusy) and (not Terminated) and
                           (FDnsLookup.FThreads.Count > FDnsLookup.FMinThreads) then
                        begin
                            FDnsLookup.FThreads.Delete(FDnsLookup.FThreads.IndexOf(Self));
                            FreeOnTerminate := TRUE;
                            Break;
                        end;
                    finally
                        //_LeaveCriticalSection(FDnsLookup.FThreadsLock);
                        FDnsLookup.FThreadsLock.Leave;
                    end;
                end;
        end;
        if Terminated then
            Break;
        Request := FDnsLookup.GetNextRequest(Self);
        if Request <> nil then
        try
            if not Request.FCanceled then
            try
                LRes := WSocket_ResolveName(
                                           Request.FLookupName,
                                           Request.FReverse,
                                           Request.FSocketFamily,
                                           FDnsResultList,
                                           Request.FProtocol);
                if (not Terminated) and (not Request.FCanceled) and
                    IsWindow(Request.FWndHandle) then
                begin
                    { Ensure CancelAsyncRequest() returns correct result }
                    FDnsLookup.LockQueue;
                    try
                        if Request.FCanceled then
                            Continue
                        else
                          { Too late to cancel this request }
                            Request.FState := lrsAlready;
                    finally
                        FDnsLookup.UnlockQueue;
                    end;
                    Request.FResultList := FDnsResultList;
                    SendMessage(Request.FWndHandle, Request.FMsgID,
                                WPARAM(Request), IcsMakeLong(0, LRes));
                end;
                { If you see an AV on SendMessage() or somewhere else while }
                { debugging and stepping thru this code it's most likely a  }
                { debugger bug. If you think it's not, let me know (Arno).  }
            except
            {$IFDEF DEBUG}
                on E: Exception do
                  {$IFDEF MSWINDOWS}
                    OutputDebugString(PChar('[' + E.ClassName + '] ' + E.Message));
                  {$ENDIF}
            {$ENDIF}
            end;
        finally
            FDnsLookup.RemoveRequest(Request);
            Request.Free;
        end;
    end;
{$IFDEF POSIX}
    finally
        LMsgPump.Free;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsAsyncDnsLookup }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MACOS}
  {$HINTS OFF}
{$ENDIF}
function TIcsAsyncDnsLookup.ExecAsync(
    AWnd          : HWND;
    AMsgID        : UINT;
    ASocketFamily : TSocketFamily;
    const AName   : string;
    AReverse      : Boolean;
    AProtocol     : Integer): THandle;
var
    Req    : TIcsAsyncDnsLookupRequest;
    Thread : TIcsAsyncDnsLookupThread;
    I      : Integer;
begin
    Result := 0; // unused variable hint in OS X
    LockQueue;
    try
      {$IFDEF MSWINDOWS}
        if not IsIPv6APIAvailable then begin
            SetLastError(WSAVERNOTSUPPORTED);
            Exit;
        end;
      {$ENDIF}
        Req := TIcsAsyncDnsLookupRequest.Create;
        Req.FWndHandle    := AWnd;
        Req.FMsgID        := AMsgID;
        Req.FSocketFamily := ASocketFamily;
        Req.FReverse      := AReverse;
        Req.FLookupName   := AName;
        Req.FProtocol     := AProtocol;
        FQueue.Add(Req);
    finally
        UnlockQueue;
    end;

    Thread := nil;
    LockThreadList;
    try
        for I := 0 to FThreads.Count - 1 do begin
            if not TIcsAsyncDnsLookupThread(FThreads[I]).FBusy then begin
                Thread := TIcsAsyncDnsLookupThread(FThreads[I]);
                Thread.FBusy := TRUE;
                Thread.FEvent.SetEvent;
                Break;
            end;
        end;
        if (Thread = nil) and (FThreads.Count < FMaxThreads) then begin
            Thread := TIcsAsyncDnsLookupThread.Create(Self);
            FThreads.Add(Thread);
            Thread.FBusy := TRUE;
        {$IF CompilerVersion < 21}
            Thread.Resume;
        {$ELSE}
            Thread.Start;
        {$IFEND}
        end;
        Result := THandle(Req);
    finally
        UnlockThreadList;
    end;
end;
{$IFDEF MACOS}
  {$HINTS ON}
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsAsyncDnsLookup.GetNextRequest(
  AThread: TIcsAsyncDnsLookupThread): TIcsAsyncDnsLookupRequest;
var
    I : Integer;
begin
    LockQueue;
    try
        Result := nil;
        if FDestroying then
            Exit;
        for I := 0 to FQueue.Count - 1 do begin
            if TIcsAsyncDnsLookupRequest(FQueue[I]).FState = lrsNone then begin
                Result := FQueue[I];
                Result.FState := lrsInWork;
                Break;
            end;
        end;
        if Result = nil then
        begin
            AThread.FEvent.ResetEvent;
            AThread.FBusy := FALSE;
        end;
    finally
        UnLockQueue;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsAsyncDnsLookup.CancelAsyncRequest(
  AReq: THandle): Integer;
var
    I   : Integer;
    Req : TIcsAsyncDnsLookupRequest;
begin
    LockQueue;
    try
      {$IFDEF MSWINDOWS}
        if not IsIPv6APIAvailable then begin
            Result := -1;
            SetLastError(WSAVERNOTSUPPORTED);
            Exit;
        end;
      {$ENDIF}
        I := FQueue.IndexOf(Pointer(AReq));
        if (I > -1) then
            Req := TIcsAsyncDnsLookupRequest(FQueue[I])
        else
            Req := nil;

        if Req <> nil then begin
            if Req.FState = lrsNone then begin
                Req.Free;
                FQueue.Delete(I);
                Result := 0;
            end
            else begin
                if Req.FState = lrsAlready then begin
                    Result := -1;
                    SetLastError(WSAEALREADY);
                end
                else begin
                    Req.FCanceled := TRUE;
                    Result := 0;
                end;
            end;
        end
        else begin
            Result := -1;
            SetLastError(WSAEINVAL);
        end;
    finally
        UnlockQueue;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncDnsLookup.SetMinMaxThreads(AMinThreads, AMaxThreads: Byte);
begin
    LockThreadList;
    try
        if AMaxThreads = 0 then
            AMaxThreads := 1;
        FMaxThreads := AMaxThreads;
        if AMinThreads > AMaxThreads then
            FMinThreads := AMaxThreads
        else
            FMinThreads := AMinThreads;
    finally
        UnlockThreadList;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsAsyncDnsLookup.Create(
    const AMaxThreads           : Integer;
    const AMinThreads           : Integer = 0;
    const AThreadIdleTimeoutSec : LongWord = 60);
begin
    inherited Create;
    FQueueLock := TIcsCriticalSection.Create;
    FThreadsLock := TIcsCriticalSection.Create;
    //FMaxThreads := AMaxThreads;
    //FMinThreads := AMinThreads;
    FThreadIdleTimeoutMsec := AThreadIdleTimeoutSec * 1000;
    FQueue   := TList.Create;
    FThreads := TList.Create;
    SetMinMaxThreads(AMinThreads, AMaxThreads);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsAsyncDnsLookup.Destroy;
var
    I : Integer;
begin
    FDestroying := TRUE;
    LockThreadList;
    try
        if Assigned(FThreads) then begin
            for I := 0 to FThreads.Count -1 do
                TObject(FThreads[I]).Free; // No problem since D7
            FreeAndNil(FThreads);
        end;
        if Assigned(FQueue) then begin
            for I := 0 to FQueue.Count -1 do
                TObject(FQueue[I]).Free;
            FreeAndNil(FQueue);
        end;
    finally
        UnlockThreadList;
    end;

    FThreadsLock.Free;
    FQueueLock.Free;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncDnsLookup.LockQueue;
begin
    FQueueLock.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncDnsLookup.LockThreadList;
begin
    FThreadsLock.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsAsyncDnsLookup.RemoveRequest(AReq: TIcsAsyncDnsLookupRequest): Boolean;
var
    I : Integer;
begin
    LockQueue;
    try
        I := FQueue.IndexOf(AReq);
        Result := (I > -1);
        if Result then
            FQueue.Delete(I);
    finally
        UnlockQueue;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncDnsLookup.UnlockQueue;
begin
    FQueueLock.Leave;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsAsyncDnsLookup.UnlockThreadList;
begin
    FThreadsLock.Leave;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TThreadStoreTree }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TThreadStoreTree.CompareData(Data1, Data2: Pointer): Boolean;
begin
    Result := PThreadStoreItem(Data1)^.ThreadID < PThreadStoreItem(Data2)^.ThreadID;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThreadStoreTree.Notification(Data: Pointer;
  Action: TIcsAvlTreeNotification);
begin
    if Action = atnRemoved then
        Dispose(PThreadStoreItem(Data));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TThreadStoreTree.SameData(Data1, Data2: Pointer): Boolean;
begin
    Result := PThreadStoreItem(Data1)^.ThreadID = PThreadStoreItem(Data2)^.ThreadID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TThreadLocalStore }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TThreadLocalStore.Create;
begin
    inherited Create;
    FLock := TIcsCriticalSection.Create;
    FTree:= TThreadStoreTree.Create(FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TThreadLocalStore.Destroy;
begin
    FTree.Free;
    inherited Destroy;
    FLock.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThreadLocalStore.Lock;
begin
    FLock.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TThreadLocalStore.RegisterStore(ThreadID: THandle): PPointer;
var
    Found : Boolean;
    PItem : PThreadStoreItem;
begin
    FTemp.ThreadID := ThreadID;
    PItem := FTree.Find(@FTemp, Found);
    if PItem <> nil then begin
        Inc(PItem^.RefCnt);
        Result := @PItem^.Data;
    end
    else begin
        New(PItem);
        PItem^.ThreadID := FTemp.ThreadID;
        PItem^.RefCnt   := 1;
        PItem^.Data     := nil;
        FTree.Add(PItem);
        Result := @PItem^.Data;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThreadLocalStore.Unlock;
begin
    FLock.Leave;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TThreadLocalStore.UnregisterStore(ThreadID: THandle): Pointer;
var
    Found : Boolean;
    PItem : PThreadStoreItem;
begin
    FTemp.ThreadID := ThreadID;
    PItem := FTree.Find(@FTemp, Found);
    if PItem <> nil then begin
        Dec(PItem^.RefCnt);
        if PItem^.RefCnt = 0 then
        begin
            Result := PItem^.Data;
            FTree.Remove(PItem);
        end
        else
            Result := nil;
    end
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function WSocket_Synchronized_IcsAsyncGetHostByName(AWnd: HWND; AMsgID: UINT;
  const ASocketFamily: TSocketFamily; const AName: string): THandle;
begin
    Result := GAsyncDnsLookup.ExecAsync(AWnd, AMsgID, ASocketFamily, AName, FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_IcsAsyncGetHostByAddr(AWnd: HWND; AMsgID: UINT;
  const ASocketFamily: TSocketFamily; const AAddr: string): THandle;
begin
    Result := GAsyncDnsLookup.ExecAsync(AWnd, AMsgID, ASocketFamily, AAddr, TRUE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_Synchronized_IcsCancelAsyncRequest(
  const ARequest: THandle): Integer;
begin
    Result := GAsyncDnsLookup.CancelAsyncRequest(ARequest);
end;
*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
{$IFNDEF NO_ADV_MT}
    CritSecIpList := TIcsCriticalSection.Create;
{$ENDIF}
    IPList     := TStringList.Create;
    //GAsyncDnsLookup := TIcsAsyncDnsLookup.Create(TIcsAsyncDnsLookup.CpuCount); // more or less max. threads ?
    GThreadLocalStore := TThreadLocalStore.Create;
{$IFNDEF COMPILER12_UP}
    CPUCount := GetCPUCount;
{$ENDIF}
{$IFDEF USE_SSL}
    SslCritSect := TIcsCriticalSection.Create;
    {$IFNDEF NO_SSL_MT}
        LockPwdCB         := TIcsCriticalSection.Create;
        LockVerifyCB      := TIcsCriticalSection.Create;
        LockInfoCB        := TIcsCriticalSection.Create;
        LockRemSessCB     := TIcsCriticalSection.Create;
        LockNewSessCB     := TIcsCriticalSection.Create;
        LockGetSessCB     := TIcsCriticalSection.Create;
        LockClientCertCB  := TIcsCriticalSection.Create;
      {$IFNDEF OPENSSL_NO_TLSEXT}
        LockServerNameCB  := TIcsCriticalSection.Create;
      {$ENDIF}
    {$ENDIF}
{$ENDIF}
{$IFDEF POSIX}
    GAsyncSocketQueue := TIcsEventQueue.Create;
    GLObjectIDSection := TIcsCriticalSection.Create;
{$ENDIF}

finalization
    if Assigned(IPList) then begin
        IPList.Free;
        IPList := nil;
    end;
{$IFNDEF NO_ADV_MT}
    CritSecIpList.Free;
{$ENDIF}
    //FreeAndNil(GAsyncDnsLookup);
    FreeAndNil(GThreadLocalStore);
{$IFDEF USE_SSL}
    {$IFNDEF NO_SSL_MT}
        FreeAndNil(LockPwdCB);
        FreeAndNil(LockVerifyCB);
        FreeAndNil(LockInfoCB);
        FreeAndNil(LockRemSessCB);
        FreeAndNil(LockNewSessCB);
        FreeAndNil(LockGetSessCB);
        FreeAndNil(LockClientCertCB);
      {$IFNDEF OPENSSL_NO_TLSEXT}
        FreeAndNil(LockServerNameCB);
      {$ENDIF}
    {$ENDIF}
    FreeAndNil(SslCritSect);
{$ENDIF}
{$IFDEF POSIX}
    FreeAndNil(GAsyncSocketQueue);
    FreeAndNil(GLObjectIDSection);
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

