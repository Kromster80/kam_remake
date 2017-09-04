{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TSysLogServer class encapsulate the server side of the SysLog
              protocol as described in RFC3164.
Creation:     September 2009
Version:      8.01
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009-2013 by François PIETTE
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
Feb 08, 2010 V1.01 F. Piette used SYSLOG_NILVALUE instead of '-'. Tested
                   this value for hostname and process which can be nil.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
                   fixed DecimalSeparator warning with XE and later
Apr 2013 - V8.01 - Arno minor XE4 changes.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSysLogServer;

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
    {$IFDEF RTL_NAMESPACES}Winapi.Winsock{$ELSE}Winsock{$ENDIF},
    OverbyteIcsWinSock,
    OverbyteIcsSysLogDefs,
    OverbyteIcsUtils,
    OverbyteIcsWSocket;

type
    TSysLogServerDataAvailable = procedure (Sender : TObject;
                                            const SrcIP, SrcPort : AnsiString;
                                            const RawMessage : AnsiString) of object;
    ESysLogParseException = class(Exception);

    TSysLogData = record
        PID             : Integer;
        PRI             : Integer;
        Facility        : TSysLogFacility;
        Severity        : TSysLogSeverity;
        Text            : AnsiString;
        Process         : String;
        HostName        : String;
        Year            : Word;
        Month           : Word;
        Day             : Word;
        Hour            : Word;
        Minute          : Word;
        Second          : Word;
        MilliSec        : Word;
        TimeString      : String;
        RFC5424         : Boolean;
        // The remaining members are only valid when an RFC5424 message is received
        MsgVer          : Integer;
        MsgID           : String;
        StructData      : String;
        TZBias          : TDateTime;
    end;

    TSysLogServer = class(TComponent)
    protected
        FUdpSocket       : TWSocket;
        FAddr            : String;
        FPort            : String;
        FRelaxedSyntax   : Boolean;
        FOnDataAvailable : TSysLogServerDataAvailable;
        procedure UdpDataAvailable(Sender: TObject; ErrCode : Word);
    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        procedure Listen;
        procedure Close;
        procedure ParseRawMessage(const RawMessage     : AnsiString;
                                  out   DecodedMessage : TSysLogData);
        procedure ParseRawMessageRFC3164(const RawMessage   : AnsiString;
                                         out DecodedMessage : TSysLogData);
        procedure ParseRawMessageRFC5424(const RawMessage   : AnsiString;
                                         out DecodedMessage : TSysLogData);
        procedure HandleMalformedMessage(const RawMessage     : AnsiString;
                                         out   DecodedMessage : TSysLogData;
                                         Offset               : Integer = 1);
    published
        property Addr            : String            read  FAddr
                                                     write FAddr;
        property Port            : String            read  FPort
                                                     write FPort;
        property RelaxedSyntax   : Boolean           read  FRelaxedSyntax
                                                     write FRelaxedSyntax;
        property OnDataAvailable : TSysLogServerDataAvailable
                                                     read  FOnDataAvailable
                                                     write FOnDataAvailable;
    end;


implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSysLogServer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FUdpSocket       := TWSocket.Create(Self);
    FPort            := '514';
    FAddr            := '0.0.0.0';  // isten on all interfaces
    FRelaxedSyntax   := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSysLogServer.Destroy;
begin
    FreeAndNil(FUdpSocket);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogServer.Listen;
begin
    FUdpSocket.Proto           := 'udp';
    FUdpSocket.Port            := FPort;
    FUdpSocket.Addr            := FAddr;
    FUdpSocket.OnDataAvailable := UdpDataAvailable;
    FUdpSocket.Listen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogServer.Close;
begin
    FUdpSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetDigits(const RawMessage : AnsiString;
                   var   Index      : Integer) : String;
var
    I : Integer;
begin
    I := Index;
    while (Index > 0) and
          (Index <= Length(RawMessage)) and
          (RawMessage[Index] >= '0') and
          (RawMessage[Index] <= '9') do
        Inc(Index);
    if Index <= I then
        Result := ''
    else
        Result := String(Copy(RawMessage, I, Index - I));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Accept alphanumeric, underline and dot characters
function GetWord(const RawMessage : AnsiString;
                 var   Index      : Integer) : String;
var
    I : Integer;
begin
    I := Index;
    while (Index > 0) and
          (Index <= Length(RawMessage)) and
          (
           ((RawMessage[Index] >= 'A') and (RawMessage[Index] <= 'Z')) or
           ((RawMessage[Index] >= 'a') and (RawMessage[Index] <= 'z')) or
           ((RawMessage[Index] >= '0') and (RawMessage[Index] <= '9')) or
            (RawMessage[Index]  = '.') or (RawMessage[Index]  = '-') or
            (RawMessage[Index]  = '_')
          ) do
        Inc(Index);
    if Index <= I then
        Result := ''
    else
        Result := String(Copy(RawMessage, I, Index - I));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure SkipSpaces(const RawMessage : AnsiString;
                     var   Index      : Integer);
begin
    while (Index > 0) and
          (Index <= Length(RawMessage)) and
          (RawMessage[Index] = ' ') do
        Inc(Index);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetStructData(
    const RawMessage : AnsiString;
    var   Index      : Integer) : String;
var
    I : Integer;
    Bracket : Integer;
begin
    Result := '';
    Bracket := 0;
    I := Index;
    while (Index > 0) and
          (Index <= Length(RawMessage)) do begin
        if RawMessage[Index] = '[' then begin
            Inc(Bracket);
        end
        else if RawMessage[Index] = ']' then begin
            Dec(Bracket);
            Inc(Index);
            if (Index <= Length(RawMessage)) and
               (RawMessage[Index] = '[') then begin
                Continue;
            end;
            break;
        end
        else if RawMessage[Index] = '\' then begin
            if RawMessage[Index + 1] in ['"', '\', ']'] then
                Inc(Index);
        end;
        Inc(Index);
    end;
    if Bracket <> 0 then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid structured data ("' +
                  String(RawMessage) + '"');

    if Index <= I then
        Result := ''
    else
        Result := String(Copy(RawMessage, I, Index - I));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsDigit(const Ch : AnsiChar) : Boolean;
begin
    Result := (Ch >= '0') and (Ch <= '9');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogServer.ParseRawMessage(
    const RawMessage     : AnsiString;
    out   DecodedMessage : TSysLogData);
var
    Index : Integer;
    PRI   : String;
    Buf   : AnsiString;
begin
    // RFC 5424 has version number immediately after PRI while RFC 3164
    // has a time stamp starting by the month name. Version number has maximum
    // 2 digits.
    // RFC5424:  <13>1 2009-09-23T14:30:45.003+02:00....
    // RFC3164:  <13>Sep 23 14:30:45....
    if (Length(RawMessage) > 0) and (RawMessage[1] <> '<') then begin
        // Simplified SysLog message is handled by RFC3164 variant
        ParseRawMessageRFC3164(RawMessage, DecodedMessage);
    end
    else begin
        Index := 2;
        PRI   := GetDigits(RawMessage, Index);
        if (Index > Length(RawMessage)) or
           (RawMessage[Index] <> '>') then
            raise ESysLogParseException.Create(
                      'SysLog message has invalid PRI delimiter ("' +
                      String(RawMessage) + '"');
        Inc(Index);
        if Index > Length(RawMessage) then
            raise ESysLogParseException.Create(
                      'SysLog message too short ("' +
                      String(RawMessage) + '"');
        Buf := Copy(RawMessage, Index, 21);
        if (Length(Buf) = 21) and
           (IsDigit(RawMessage[Index])) and
           (RawMessage[Index + 1] = ' ') and
           (IsDigit(RawMessage[Index + 2])) and
           (IsDigit(RawMessage[Index + 3])) and
           (IsDigit(RawMessage[Index + 4])) and
           (IsDigit(RawMessage[Index + 5])) and
           (RawMessage[Index + 6] = '-') and
           (IsDigit(RawMessage[Index + 7])) and
           (IsDigit(RawMessage[Index + 8])) and
           (RawMessage[Index + 9] = '-') and
           (IsDigit(RawMessage[Index + 10])) and
           (IsDigit(RawMessage[Index + 11])) and
           (RawMessage[Index + 12] = 'T') and
           (IsDigit(RawMessage[Index + 13])) and
           (IsDigit(RawMessage[Index + 14])) and
           (RawMessage[Index + 15] = ':') and
           (IsDigit(RawMessage[Index + 16])) and
           (IsDigit(RawMessage[Index + 17])) and
           (RawMessage[Index + 18] = ':') and
           (IsDigit(RawMessage[Index + 19])) and
           (IsDigit(RawMessage[Index + 20])) then
            ParseRawMessageRFC5424(RawMessage, DecodedMessage)
        else
            ParseRawMessageRFC3164(RawMessage, DecodedMessage);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogServer.ParseRawMessageRFC5424(
    const RawMessage     : AnsiString;
    out   DecodedMessage : TSysLogData);
var
    Index     : Integer;
    Index1    : Integer;
    PRI       : String;
    NPRI      : Integer;
    SVer      : String;
    SYear     : String;
    SMonth    : String;
    SDay      : String;
    SHour     : String;
    SMinute   : String;
    SSecond   : String;
    SMSec     : String;
    Delim     : AnsiChar;
    SPID      : String;
begin
    DecodedMessage.RFC5424 := TRUE;
    // <13>1 2009-09-13T16:44:31.000+02:00 Toshiba3 OverbyteIcsSysLogClientDemo 5448 ID47 [exampleSDID@32473 iut="3" eventSource="Application" eventID="1011"] Hello http://www.OverByte.be
    if Length(RawMessage) < 21 then
        raise ESysLogParseException.Create(
                  'SysLog message too short ("' +
                  String(RawMessage) + '"');
    if RawMessage[1] <> '<' then
        raise ESysLogParseException.Create(
                  'SysLog message must start with ''<'' ("' +
                  String(RawMessage) + '"');
    Index := 2;
    PRI   := GetDigits(RawMessage, Index);
    if (Index > Length(RawMessage)) or
       (RawMessage[Index] <> '>') then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid PRI delimiter ("' +
                  String(RawMessage) + '"');
    if PRI = '' then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid PRI ("' +
                  String(RawMessage) + '"');
    NPRI := StrToIntDef(PRI, -1);
    if (NPRI < (Ord(Low(TSysLogFacility)) * 8  + Ord(Low(TSysLogSeverity)))) or
       (NPRI > (Ord(High(TSysLogFacility)) * 8 + Ord(High(TSysLogSeverity)))) then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid PRI ("' +
                  String(RawMessage) + '"');
    DecodedMessage.PRI      := NPRI;
    DecodedMessage.Severity := TSysLogSeverity(NPRI mod 8);
    DecodedMessage.Facility := TSysLogFacility(NPRI div 8);
    Inc(Index);
    if (RawMessage[Index] < '0') and (RawMessage[Index] > '9') then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid version number ("' +
                  String(RawMessage) + '"');
    SVer   := GetDigits(RawMessage, Index);
    DecodedMessage.MsgVer := StrToIntDef(SVer, -1);
    if DecodedMessage.MsgVer <> 1 then
        raise ESysLogParseException.Create(
                  'SysLog message has unsupported version number ("' +
                  String(RawMessage) + '"');
    if (Index > Length(RawMessage)) or
       (RawMessage[Index] <> ' ') then
        raise ESysLogParseException.Create(
                  'SysLog message has missing space after version ("' +
                  String(RawMessage) + '"');
    Inc(Index);
    // 2009-09-13T16:44:31.000+02:00
    Index1 := Index;
    SYear := GetDigits(RawMessage, Index);
    if (SYear = '') or
       (Index > Length(RawMessage)) or
       (RawMessage[Index] <> '-') then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid date format ("' +
                  String(RawMessage) + '"');
    Inc(Index);
    SMonth := GetDigits(RawMessage, Index);;
    if (SMonth = '') or
       (Index > Length(RawMessage)) or
       (RawMessage[Index] <> '-') then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid date format ("' +
                  String(RawMessage) + '"');
    Inc(Index);
    SDay := GetDigits(RawMessage, Index);
    if (SDay = '') or
       (Index > Length(RawMessage)) or
       (RawMessage[Index] <> 'T') then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid date format ("' +
                  String(RawMessage) + '"');
    Inc(Index);
    SHour := GetDigits(RawMessage, Index);
    if (SHour = '') or
       (Index > Length(RawMessage)) or
       (RawMessage[Index] <> ':') then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid date format ("' +
                  String(RawMessage) + '"');
    Inc(Index);
    SMinute := GetDigits(RawMessage, Index);
    if (SMinute = '') or
       (Index > Length(RawMessage)) or
       (RawMessage[Index] <> ':') then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid date format ("' +
                  String(RawMessage) + '"');
    Inc(Index);
    SSecond := GetDigits(RawMessage, Index);
    if (SSecond = '') or
       (Index > Length(RawMessage)) then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid date format ("' +
                  String(RawMessage) + '"');
    Delim := RawMessage[Index];
    if Delim = '.' then begin
        Inc(Index);
        SMSec := GetDigits(RawMessage, Index);
        Delim := RawMessage[Index];
    end
    else begin
        SMSec := '0';
    end;

{$IFDEF COMPILER15_UP}
    SMsec := '0' + FormatSettings.DecimalSeparator + SMSec;
{$ELSE}
    SMsec := '0' + DecimalSeparator + SMSec;
{$ENDIF}
    DecodedMessage.Year     := StrToInt(SYear);
    DecodedMessage.Month    := StrToInt(SMonth);
    DecodedMessage.Day      := StrToInt(SDay);
    DecodedMessage.Hour     := StrToInt(SHour);
    DecodedMessage.Minute   := StrToInt(SMinute);
    DecodedMessage.Second   := StrToInt(SSecond);
    DecodedMessage.MilliSec := Trunc(1000 * StrToFloat(SMsec));

    case Delim of
    'Z', ' ' : DecodedMessage.TZBias := 0;
    '+', '-' :
          begin
              Inc(Index);
              SHour := GetDigits(RawMessage, Index);
              if (SHour = '') or
                 (Index > Length(RawMessage)) or
                 (RawMessage[Index] <> ':') then
                  raise ESysLogParseException.Create(
                            'SysLog message has invalid date format ("' +
                            String(RawMessage) + '"');
              Inc(Index);
              SMinute := GetDigits(RawMessage, Index);
              if (SMinute = '') or
                 (Index > Length(RawMessage)) or
                 (RawMessage[Index] <> ' ') then
                  raise ESysLogParseException.Create(
                            'SysLog message has invalid date format ("' +
                            String(RawMessage) + '"');
              DecodedMessage.TZBias := EncodeTime(StrToInt(SHour),
                                                  StrToInt(SMinute), 0, 0);
              if Delim = '-' then
                  DecodedMessage.TZBias := -DecodedMessage.TZBias;
          end;
    else
        raise ESysLogParseException.Create(
              'SysLog message has invalid date format ("' +
              String(RawMessage) + '"');
    end;

    DecodedMessage.TimeString := String(Copy(RawMessage, Index1, Index - Index1));

    if (Index > Length(RawMessage)) or
       (RawMessage[Index] <> ' ') then
        raise ESysLogParseException.Create(
                  'SysLog message has missing space after date ("' +
                  String(RawMessage) + '"');
    SkipSpaces(RawMessage, Index);
    if RawMessage[Index] = SYSLOG_NILVALUE then
        DecodedMessage.HostName := ''
    else
        DecodedMessage.HostName := GetWord(RawMessage, Index);
    SkipSpaces(RawMessage, Index);
    if RawMessage[Index] = SYSLOG_NILVALUE then
        DecodedMessage.Process := ''
    else
        DecodedMessage.Process := GetWord(RawMessage, Index);
    SkipSpaces(RawMessage, Index);
    if Index > Length(RawMessage) then
        raise ESysLogParseException.Create(
                  'SysLog message too short ("' +
                  String(RawMessage) + '"');

    if RawMessage[Index] = SYSLOG_NILVALUE then begin
        DecodedMessage.PID := 0;
        Inc(Index);
    end
    else begin
        SPID := GetDigits(RawMessage, Index);
        DecodedMessage.PID := StrToIntDef(SPID, 0);
    end;
    if RawMessage[Index] <> ' ' then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid PID delimter ("' +
                  String(RawMessage) + '"');
    SkipSpaces(RawMessage, Index);
    DecodedMessage.MsgID := GetWord(RawMessage, Index);
    SkipSpaces(RawMessage, Index);
    Delim := RawMessage[Index];
    if Delim = '[' then
         DecodedMessage.StructData := GetStructData(RawMessage, Index)
    else
         DecodedMessage.StructData := '';
    if (Index <= Length(RawMessage)) and (RawMessage[Index] = ' ') then
        Inc(Index);
    if Index > Length(RawMessage) then
        DecodedMessage.Text := ''
    else
        DecodedMessage.Text := Copy(RawMessage, Index, MAXINT);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogServer.HandleMalformedMessage(
    const RawMessage     : AnsiString;
    out   DecodedMessage : TSysLogData;
    Offset               : Integer = 1);
var
    Today     : TDateTime;
    MilliSec  : Word;
begin
    DecodedMessage.PID      := 0;
    DecodedMessage.Facility := SYSLOG_FACILITY_USER;
    DecodedMessage.Severity := SYSLOG_SEVERITY_NOTICE;
    DecodedMessage.PRI      := Ord(DecodedMessage.Facility) * 8 +
                               Ord(DecodedMessage.Severity);
    DecodedMessage.Text     := Copy(RawMessage, Offset, MAXINT);
    DecodedMessage.Process  := '';
    DecodedMessage.HostName := '';
    Today := Now;
    DecodeDate(Today, DecodedMessage.Year, DecodedMessage.Month, DecodedMessage.Day);
    DecodeTime(Today, DecodedMessage.Hour, DecodedMessage.Minute,
                      DecodedMessage.Second, MilliSec);
    DecodedMessage.TimeString := Format('%s %2d %02.2d:%02.2d:%02.2d',
      [SysLogMonthNames[DecodedMessage.Month], DecodedMessage.Day,
       DecodedMessage.Hour, DecodedMessage.Minute, DecodedMessage.Second]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogServer.ParseRawMessageRFC3164(
    const RawMessage     : AnsiString;
    out   DecodedMessage : TSysLogData);
var
    Index     : Integer;
    PRI       : String;
    NPRI      : Integer;
    IPRI      : Integer;
    MonthName : String;
    SDay      : String;
    SHour     : String;
    SMinute   : String;
    SSecond   : String;
    SPID      : String;
    Today     : TDateTime;
begin
    DecodedMessage.RFC5424 := FALSE;
    // <13>Sep  8 14:37:40 MyHostName MyProcess[1234]: This is my message text
    if (Length(RawMessage) > 0) and (RawMessage[1] <> '<') then begin
        // There is a special case where the syslog message is just the text
        if FRelaxedSyntax then begin
            HandleMalformedMessage(RawMessage, DecodedMessage);
            Exit;
        end
        else
            raise ESysLogParseException.Create(
                      'SysLog message has invalid priority ("' +
                      String(RawMessage) + '")');
    end;

    if Length(RawMessage) < 21 then
        raise ESysLogParseException.Create(
                  'SysLog message too short ("' +
                  String(RawMessage) + '")');
    if RawMessage[1] <> '<' then
        raise ESysLogParseException.Create(
                  'SysLog message must start with ''<'' ("' +
                  String(RawMessage) + '")');
    Index := 2;
    PRI   := GetDigits(RawMessage, Index);
    if (Index > Length(RawMessage)) or
       (RawMessage[Index] <> '>') then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid PRI delimiter ("' +
                  String(RawMessage) + '")');
    if PRI = '' then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid PRI ("' +
                  String(RawMessage) + '")');
    NPRI := StrToIntDef(PRI, -1);
    if (NPRI < (Ord(Low(TSysLogFacility)) * 8  + Ord(Low(TSysLogSeverity)))) or
       (NPRI > (Ord(High(TSysLogFacility)) * 8 + Ord(High(TSysLogSeverity)))) then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid PRI ("' +
                  String(RawMessage) + '")');
    DecodedMessage.PRI      := NPRI;
    DecodedMessage.Severity := TSysLogSeverity(NPRI mod 8);
    DecodedMessage.Facility := TSysLogFacility(NPRI div 8);
    Inc(Index);
    IPRI  := Index;
    Today := Now;
    DecodeDate(Today, DecodedMessage.Year, DecodedMessage.Month, DecodedMessage.Day);
    MonthName := GetWord(RawMessage, Index);
    if Length(MonthName) <> 3 then begin
        if FRelaxedSyntax then begin
            HandleMalformedMessage(RawMessage, DecodedMessage, IPRI);
            Exit;
        end
        else
            raise ESysLogParseException.Create(
                      'SysLog message has invalid month in date ("' +
                      String(RawMessage) + '")');
    end;

    if (Index > Length(RawMessage)) or
       (RawMessage[Index] <> ' ') then
        raise ESysLogParseException.Create(
                  'SysLog message has missing space after month name in date ("' +
                  String(RawMessage) + '")');
    DecodedMessage.Month := 1;
    while (DecodedMessage.Month <= 12) and
          (not SameText(SysLogMonthNames[DecodedMessage.Month], MonthName)) do
        Inc(DecodedMessage.Month);
    if DecodedMessage.Month > 12 then begin
        // We have a bad month name. Assume we have a malformed SysLog message
        if FRelaxedSyntax then begin
            HandleMalformedMessage(RawMessage, DecodedMessage, IPRI);
            Exit;
        end
        else
            raise ESysLogParseException.Create(
                      'SysLog message has invalid month in date ("' +
                      String(RawMessage) + '")');
    end;

    SkipSpaces(RawMessage, Index);
    SDay := GetDigits(RawMessage, Index);
    if (Index > Length(RawMessage)) or
       (RawMessage[Index] <> ' ') then
        raise ESysLogParseException.Create(
                  'SysLog message has missing space after day in date ("' +
                  String(RawMessage) + '")');
    DecodedMessage.Day := StrToIntDef(SDay, -1);
    if (DecodedMessage.Day <= 0) or (DecodedMessage.Day > 31) then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid day number ("' +
                  String(RawMessage) + '")');
    // <13>Sep  8 14:37:40 MyHostName MyProcess[1234]: This is my message text
    SkipSpaces(RawMessage, Index);
    SHour   := GetDigits(RawMessage, Index);
    if (Index > Length(RawMessage)) or
       (RawMessage[Index] <> ':') then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid time delimiter ("' +
                  String(RawMessage) + '")');
    DecodedMessage.Hour := StrToIntDef(SHour, 99);
    Inc(Index);
    SMinute := GetDigits(RawMessage, Index);
    if (Index > Length(RawMessage)) or
       (RawMessage[Index] <> ':') then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid time delimiter ("' +
                  String(RawMessage) + '")');
    DecodedMessage.Minute := StrToIntDef(SMinute, 99);
    Inc(Index);
    SSecond := GetDigits(RawMessage, Index);
    if (Index > Length(RawMessage)) or
       (RawMessage[Index] <> ' ') then
        raise ESysLogParseException.Create(
                  'SysLog message has missing space after time ("' +
                  String(RawMessage) + '")');
    DecodedMessage.Second := StrToIntDef(SSecond, 99);
    if (DecodedMessage.Hour > 23) or
       (DecodedMessage.Minute > 59) or
       (DecodedMessage.Second > 59) then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid time ("' +
                  String(RawMessage) + '")');
    DecodedMessage.TimeString := Format('%s %2d %02.2d:%02.2d:%02.2d',
        [SysLogMonthNames[DecodedMessage.Month], DecodedMessage.Day,
         DecodedMessage.Hour, DecodedMessage.Minute, DecodedMessage.Second]);
    SkipSpaces(RawMessage, Index);
    DecodedMessage.HostName := GetWord(RawMessage, Index);
    SkipSpaces(RawMessage, Index);
    DecodedMessage.Process := GetWord(RawMessage, Index);
    if (Index <= Length(RawMessage)) and (RawMessage[Index] = '[') then begin
        Inc(Index);
        SPID := GetDigits(RawMessage, Index);
        DecodedMessage.PID := StrToIntDef(SPID, 0);
        if RawMessage[Index] <> ']' then
            raise ESysLogParseException.Create(
                      'SysLog message has invalid PID delimter ("' +
                      String(RawMessage) + '")');
        Inc(Index);
    end;
    if (Index > Length(RawMessage)) or (RawMessage[Index] <> ':') then
        raise ESysLogParseException.Create(
                  'SysLog message has invalid content delimiter ("' +
                  String(RawMessage) + '")');
    Inc(Index);
    if (Index <= Length(RawMessage)) and (RawMessage[Index] = ' ') then
        Inc(Index);
    if Index > Length(RawMessage) then
        DecodedMessage.Text := ''
    else
        DecodedMessage.Text := Copy(RawMessage, Index, MAXINT);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSysLogServer.UdpDataAvailable(Sender: TObject; ErrCode: Word);
var
    RawMessage : AnsiString;
    Len        : Integer;
    Src        : TSockAddrIn;
    SrcLen     : Integer;
    SrcIP      : AnsiString;
    SrcPort    : AnsiString;
begin
    SrcLen := SizeOf(Src);
    Len    := FUdpSocket.RcvdCount;
    SetLength(RawMessage, Len);
    Len    := FUdpSocket.ReceiveFrom(@RawMessage[1], Length(RawMessage),
                                     Src, SrcLen);
    if Len < 0 then
        Exit;
    SetLength(RawMessage, Len);
    SrcIP   := IcsStrPas(inet_ntoa(Src.sin_addr));
    SrcPort := AnsiString(IntToStr(ntohs(Src.sin_port)));
    if Assigned(FOnDataAvailable) then
        FOnDataAvailable(Self, SrcIP, SrcPort, RawMessage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
