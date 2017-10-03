{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Common definitions for TSysLogClient and TSysLogServer classes.
              See RFC3164 for reference.
Creation:     September 2009
Version:      8.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009 by François PIETTE
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
Feb 08, 2010 V1.01 F. Piette renamed NILVALUE to SYSLOG_NILVALUE it here from
                   the client component so that it can be used from server
                   component.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSysLogDefs;

interface

type
    // Facility (See RFC 3164)
    TSysLogFacility = (
        SYSLOG_FACILITY_KERNEL        = 0,  // kernel messages
        SYSLOG_FACILITY_USER          = 1,  // user-level messages
        SYSLOG_FACILITY_MAIL          = 2,  // mail system
        SYSLOG_FACILITY_SYS_DAEMON    = 3,  // system daemons
        SYSLOG_FACILITY_SECURITY1     = 4,  // security/authorization messages (note 1)
        SYSLOG_FACILITY_INTERNAL      = 5,  // messages generated internally by syslogd
        SYSLOG_FACILITY_LPR           = 6,  // line printer subsystem
        SYSLOG_FACILITY_NNTP          = 7,  // network news subsystem
        SYSLOG_FACILITY_UUCP          = 8,  // UUCP subsystem
        SYSLOG_FACILITY_CLOCK1        = 9,  // clock daemon (note 1)
        SYSLOG_FACILITY_SECURITY2     = 10, // security/authorization messages (note 2)
        SYSLOG_FACILITY_FTP           = 11, // FTP daemon
        SYSLOG_FACILITY_NTP           = 12, // NTP subsystem
        SYSLOG_FACILITY_AUDIT         = 13, // log audit (note 1)
        SYSLOG_FACILITY_ALERT         = 14, // log alert (note 1)
        SYSLOG_FACILITY_CLOCK2        = 15, // clock daemon (note 2)
        SYSLOG_FACILITY_LOCAL0        = 16, // local use 0  (local0)
        SYSLOG_FACILITY_LOCAL1        = 17, // local use 1  (local1)
        SYSLOG_FACILITY_LOCAL2        = 18, // local use 2  (local2)
        SYSLOG_FACILITY_LOCAL3        = 19, // local use 3  (local3)
        SYSLOG_FACILITY_LOCAL4        = 20, // local use 4  (local4)
        SYSLOG_FACILITY_LOCAL5        = 21, // local use 5  (local5)
        SYSLOG_FACILITY_LOCAL6        = 22, // local use 6  (local6)
        SYSLOG_FACILITY_LOCAL7        = 23  // local use 7  (local7)
    );
const
    SysLogFacilityString : array [TSysLogFacility] of String = (
        'KERNEL',    'USER',     'MAIL',      'SYS_DAEMON',
        'SECURITY1', 'INTERNAL', 'LPR',       'NNTP',
        'UUCP',      'CLOCK1',   'SECURITY2', 'FTP',
        'NTP',       'AUDIT',    'ALERT',     'CLOCK2',
        'LOCAL0',    'LOCAL1',   'LOCAL2',    'LOCAL3',
        'LOCAL4',    'LOCAL5',   'LOCAL6',    'LOCAL7');

type
    // Note 1 - Various operating systems have been found to utilize
    //          Facilities 4, 10, 13 and 14 for security/authorization,
    //          audit, and alert messages which seem to be similar.
    // Note 2 - Various operating systems have been found to utilize
    //          both Facilities 9 and 15 for clock (cron/at) messages.

    // Severity (See RFC 3164)
    TSysLogSeverity = (
        SYSLOG_SEVERITY_EMERGENCY     = 0, // Emergency: system is unusable
        SYSLOG_SEVERITY_ALERT         = 1, // Alert: action must be taken immediately
        SYSLOG_SEVERITY_CRITICAL      = 2, // Critical: critical conditions
        SYSLOG_SEVERITY_ERROR         = 3, // Error: error conditions
        SYSLOG_SEVERITY_WARNING       = 4, // Warning: warning conditions
        SYSLOG_SEVERITY_NOTICE        = 5, // Notice: normal but significant condition
        SYSLOG_SEVERITY_INFORMATIONAL = 6, // Informational: informational messages
        SYSLOG_SEVERITY_DEBUG         = 7  // Debug: debug-level messages
    );
const
    SysLogSeverityString : array [TSysLogSeverity] of String = (
        'EMERGENCY', 'ALERT',  'CRITICAL',      'ERROR',
        'WARNING',   'NOTICE', 'INFORMATIONAL', 'DEBUG');

const
    SysLogMonthNames : array [1..12] of string = (
                    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

const
    // The next constant is specific to RFC 5424
    SYSLOG_NILVALUE  = '-';          // Empty value

implementation

end.
