{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TSysLogClient class encapsulate the client side of the SysLog
              protocol as described in RFC3164 and RFC5424 but UTF-8 encoding
              is not supported.
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
Feb 08, 2010 V1.01 F. Piette moved NILVALUE to SysLogDefs so that it can be
                   used from client and server components.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSysLogClient;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }
{$J+}           { Allow typed constant to be modified }
{$ALIGN 8}
{$I Include\OverbyteIcsDefs.inc}

interface

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    OverbyteIcsSysLogDefs,
    OverbyteIcsWSocket;

type
    ESysLogClientException = class(Exception);
    TSysLogClient = class(TComponent)
    protected
        FUdpSocket  : TWSocket;
        FServer     : String;
        FPort       : String;
        FLocalPort  : String;
        FRawMessage : String;
        FPID        : Integer;
        FFacility   : TSysLogFacility;
        FSeverity   : TSysLogSeverity;
        FText       : String;
        FProcess    : String;
        FHostName   : String;
        FTimeStamp  : TDateTime;
        FRFC5424    : Boolean;
        FMsgID      : String;    // RFC5424 only
        FStructData : String;    // RFC5424 only
    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        procedure Send;
        procedure RawSend(const ARawMessage: String);
        procedure Close;
        procedure BuildRawMessage;
    published
        property Server     : String    read  FServer     write FServer;
        property Port       : String    read  FPort       write FPort;
        property LocalPort  : String    read  FLocalPort  write FLocalPort;
        property RawMessage : String    read  FRawMessage write FRawMessage;
        property Process    : String    read  FProcess    write FProcess;
        property HostName   : String    read  FHostName   write FHostName;
        property Text       : String    read  FText       write FText;
        property Facility   : TSysLogFacility   read  FFacility   write FFacility;
        property Severity   : TSysLogSeverity   read  FSeverity   write FSeverity;
        property PID        : Integer   read  FPID        write FPID;
        property TimeStamp  : TDateTime read  FTimeStamp  write FTimeStamp;
        property RFC5424    : Boolean   read  FRFC5424    write FRFC5424;
        property MsgID      : String    read  FMsgID      write FMsgID;
        property StructData : String    read  FStructData write FStructData;
    end;

function TimeZoneBias : String;

implementation

const
    // The next constants are specific to RFC 5424
    SYSLOGVER = 1;            // Version
    BOM       = #$EF#$BB#$BF; // Begin of UTF-8 string


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSysLogClient.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FUdpSocket       := TWSocket.Create(Self);
    FPort            := '514';
    FLocalPort       := '';    // Will be selected by OS
    FServer          := '127.0.0.1';
    FPid             := -1;
    FFacility        := SYSLOG_FACILITY_USER;
    FSeverity        := SYSLOG_SEVERITY_NOTICE;
    FRFC5424         := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSysLogClient.Destroy;
begin
    FreeAndNil(FUdpSocket);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClient.BuildRawMessage;
var
    Day, Month, Year : Word;
    Hour, Minute     : Word;
    Second, MilliSec : Word;
    TS               : TDateTime;
    DT               : String;
    SMsgID           : String;
    SStructData      : String;
begin
    if FPid = -1 then
        FPid := GetCurrentProcessID;

    if FProcess = '' then begin
        SetLength(FProcess, 256);
        SetLength(FProcess, GetModuleFileName(0, @FProcess[1], Length(FProcess)));
        FProcess := ExtractFileName(FProcess);
        FProcess := ChangeFileExt(FProcess, '');
    end;

    if FHostName = '' then
        FHostName := String(LocalHostName);

    if FTimeStamp = 0 then
        TS := Now
    else
        TS := FTimeStamp;

    DecodeDate(TS, Year, Month, Day);
    DecodeTime(TS, Hour, Minute, Second, MilliSec);
    if FRFC5424 then begin
        DT := Format('%04.4d-%02.2d-%02.2dT%02.2d:%02.2d:%02.2d.%03.3d%s',
                     [Year, Month, Day, Hour, Minute, Second, MilliSec,
                      TimeZoneBias]);
        if FMsgID = '' then
            SMsgID := SYSLOG_NILVALUE
        else
            SMsgID := FMsgID;
        if FStructData = '' then
            SStructData := SYSLOG_NILVALUE
        else begin
            SStructData := FStructData;
            if (Length(SStructData) < 2) or
               (SStructData[1] <> '[') or
               (SStructData[Length(SStructData)] <> ']') then
                raise ESysLogClientException.Create(
                      'Structured data must be enclosed in ''[]''');
        end;

        RawMessage := Format('<%d>%d %s %s %s %d %s %s %s',
                             [Ord(FFacility) * 8 + Ord(FSeverity),
                              SYSLOGVER,
                              DT, FHostName, FProcess, FPID,
                              SMsgID, SStructData, FText]);
    end
    else begin
        DT := Format('%s %2d %02.2d:%02.2d:%02.2d',
                     [SysLogMonthNames[Month], Day, Hour, Minute, Second]);

        // <13>Sep  8 14:37:40 MyHostName MyProcess[1234]: This is my message text
        RawMessage := Format('<%d>%s %s %s[%d]: %s',
                             [Ord(FFacility) * 8 + Ord(FSeverity),
                              DT, FHostName, FProcess, FPID, FText]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClient.Send;
begin
    BuildRawMessage;
    RawSend(FRawMessage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClient.RawSend(const ARawMessage : String);
begin
    if FUdpSocket.State <> wsConnected then begin
        FUdpSocket.Proto     := 'udp';
        FUdpSocket.Addr      := FServer;
        FUdpSocket.Port      := FPort;
        if FLocalPort <> '' then
            FUdpSocket.LocalPort := FLocalPort;
        FUdpSocket.Connect;
    end;
    FUdpSocket.SendStr(ARawMessage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogClient.Close;
begin
    FUdpSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TimeZoneBias : String;
const
    Time_Zone_ID_DayLight = 2;
var
    TZI       : tTimeZoneInformation;
    TZIResult : Integer;
    aBias     : Integer;
begin
    TZIResult := GetTimeZoneInformation(TZI);
    if TZIResult = -1 then
        Result := '-0000'
    else begin
         if TZIResult = Time_Zone_ID_DayLight then   { 10/05/99 }
             aBias := TZI.Bias + TZI.DayLightBias
         else
             aBias := TZI.Bias + TZI.StandardBias;
         Result := Format('-%.2d:%.2d', [Abs(aBias) div 60, Abs(aBias) mod 60]);
         if aBias < 0 then
             Result[1] := '+';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
