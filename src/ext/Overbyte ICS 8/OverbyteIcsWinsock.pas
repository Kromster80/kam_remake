{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  WinSock API for Delphi 8 for the Microsoft .NET framework
              This is the subset needed for ICS components.
Creation:     December 2003
Version:      8.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2011 by François PIETTE
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
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWinsock;

interface
{$I Include\OverbyteIcsDefs.inc}

{$IFDEF MSWINDOWS}
uses
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF};
  {$I Include\OverbyteIcsWinsockTypes.inc}
{$ENDIF MSWINDOWS}

{$IFDEF CLR}
uses
    System.Runtime.InteropServices,
    System.Text,
    OverbyteIcsTypes;
    {$I Include\OverbyteIcsDotNetSockTypes.inc}
{$ENDIF CLR}

implementation
{$IFDEF MSWINDOWS}
  {$I Include\OverbyteIcsWinsockImpl.inc}

initialization
    InitializeCriticalSection(GWSockCritSect);
    in6addr_any := IN6ADDR_ANY_INIT;
    in6addr_loopback := IN6ADDR_LOOPBACK_INIT;
  {$IFDEF STILL_NEEDS_CHECK}
    IN6ADDR_V4MAPPEDPREFIX_INIT(@in6addr_v4mappedprefix);
  {$ENDIF STILL_NEEDS_CHECK}

finalization
    DeleteCriticalSection(GWSockCritSect);

{$ENDIF MSWINDOWS}

{$IFDEF CLR}
    {$I Include\OverbyteIcsDotNetSockImpl.inc}
{$ENDIF}

end.



