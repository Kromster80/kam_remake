{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  Logger class donated to ICS.
Creation:     December 2005
Version:      8.00
EMail:        francois.piette@overbyte.be      http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2005-2011 by Fran�ois PIETTE
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
                 to Fran�ois PIETTE. Use a nice stamp and mention your
                 name, street address, EMail address and any comment you like
                 to say.

History:
Dec 31, 2005 V1.01 F. Piette cleaned the code
Jan 18, 2006 V1.02 Fixed an AV on destroy
Mar 26, 2006 V6.00 Started new version 6
Aug 20, 2006 V6.01 F. Piette adapted to Delphi.NET
Jul 03, 2008 V6.02 A. Garrels made some changes to prepare code for Unicode.
                   When new LogFileEncoding is set to lfeUtf16 logging to file
                   will be probably faster than lfeUtf8.
May 08, 2009 V6.03 Added properties TimeStampFormatString and TimeStampSeparator
                   similar as suggested by Anton Sviridov.
Dec 20, 2009 V6.04 Exchanged symbol "NO_ADV_MT" by "NO_LOGGER_MT".
Dec 06, 2010 V6.05 Thread-safe FreeNotification and RemoveFreeNotification.
Apr 15, 2011 V6.06 Arno prepared for 64-bit.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsLogger;

{$B-}              { Enable partial boolean evaluation   }
{$T-}              { Untyped pointers                    }
{$X+}              { Enable extended syntax              }
{$H+}              { Use long Strings                    }
{$J+}              { Allow typed constant to be modified }
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
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

{ If NO_LOGGER_MT is defined, then there is less multi-thread code compiled. }

{#$DEFINE NO_LOGGER_MT}

interface

uses
  {$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SyncObjs{$ELSE}SyncObjs{$ENDIF},
    OverbyteIcsUtils;

const
    TIcsLoggerVersion   = 800;
    CopyRight : String  = ' IcsLogger (c) 2005-2012 by Fran�ois PIETTE V8.00 ';

type
    ELoggerException = class(Exception);
    TLogOption = (loDestEvent,   loDestFile,     loDestOutDebug,  { Output Destinations }
                  loAddStamp,                                     { Adds something (slow) }
                  loWsockErr,    loWsockInfo,    loWsockDump,
                  loSslErr,      loSslInfo,      loSslDump,
                  loProtSpecErr, loProtSpecInfo, loProtSpecDump);
    TLogOptions = set of TLogOption;
    TLogFileOption = (lfoAppend, lfoOverwrite);
{$IFDEF COMPILER12_UP}
    TLogFileEncoding = (lfeUtf8, lfeUtf16);
{$ENDIF}
const
    LogAllOptErr  =  [loWsockErr, loSslErr, loProtSpecErr];
    LogAllOptInfo =  LogAllOptErr + [loWsockInfo, loSslInfo, loProtSpecInfo];
    LogAllOptDump =  LogAllOptInfo + [loWsockDump, loSslDump, loProtSpecDump];

type
    TNTEventType = (etError,        etWarning,      etInformation,
                    etAuditSuccess, etAuditFailure);
    TIcsLogEvent = procedure (Sender: TObject; LogOption: TLogOption;
                              const Msg : String) of object;
    TIcsLogger = class(TComponent)
    protected
        FLogOptions             : TLogOptions;
        FOnIcsLogEvent          : TIcsLogEvent;
        FLogFileName            : String;
        FLogFile                : TFileStream;
        FLogFileOption          : TLogFileOption;
        FTimeStampFormatString  : String;  {V6.03}
        FTimeStampSeparator     : String;  {V6.03}
    {$IFDEF COMPILER12_UP}
        FLogFileEncoding        : TLogFileEncoding;
        FLogFileInternalEnc     : TLogFileEncoding;
    {$ENDIF}
    {$IFNDEF NO_LOGGER_MT}
        FLock                   : TCriticalSection;
        procedure   Lock;
        procedure   UnLock;
    {$ENDIF}
        procedure   WriteToLogFile(const S: String);
        procedure   SetLogFileOption(const Value: TLogFileOption);
        procedure   SetLogOptions(const Value: TLogOptions);
        procedure   SetLogFileName(const Value: String);
        procedure   SetOnIcsLogEvent(const Value: TIcsLogEvent);
        procedure   InternalOpenLogFile;
        procedure   InternalCloseLogFile;
        function    AddTimeStamp: String;  {V6.03}
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   OpenLogFile;
        procedure   CloseLogFile;
        procedure   DoDebugLog (Sender    : TObject;
                                LogOption : TLogOption;
                                const Msg : String);
        procedure   FreeNotification(AComponent: TComponent);
        procedure   RemoveFreeNotification(AComponent: TComponent);
    published
        property    TimeStampFormatString : String       read  FTimeStampFormatString {V6.03}
                                                         write FTimeStampFormatString;
        property    TimeStampSeparator : String          read  FTimeStampSeparator {V6.03}
                                                         write FTimeStampSeparator;
        property    LogFileOption : TLogFileOption       read  FLogFileOption
                                                         write SetLogFileOption;
{$IFDEF COMPILER12_UP}
        property    LogFileEncoding : TLogFileEncoding   read  FLogFileEncoding
                                                         write FLogFileEncoding;
{$ENDIF}
        property    LogFileName   : String               read  FLogFileName
                                                         write SetLogFileName;
        property    LogOptions    : TLogOptions          read  FLogOptions
                                                         write SetLogOptions;
        property    OnIcsLogEvent : TIcsLogEvent         read  FOnIcsLogEvent
                                                         write SetOnIcsLogEvent;
    end;


implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure OutputDebugStr(const AMsg: String); {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  {$IFDEF POSIX}
    System.WriteLn(AMsg);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.OutputDebugString(PChar(AMsg));
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsLogger.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
{$IFNDEF NO_LOGGER_MT}
    FLock := TCriticalSection.Create;
{$ENDIF}
    FTimeStampFormatString := 'hh:nn:ss:zzz'; {V6.03}
    FTimeStampSeparator    := ' ';            {V6.03}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsLogger.Destroy;
begin
    CloseLogFile;
{$IFNDEF NO_LOGGER_MT}
    FLock.Free;
{$ENDIF}
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.FreeNotification(AComponent: TComponent);
begin
{$IFNDEF NO_LOGGER_MT}
    Lock;
    try
{$ENDIF}
        inherited FreeNotification(AComponent);
{$IFNDEF NO_LOGGER_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.RemoveFreeNotification(AComponent: TComponent);
begin
{$IFNDEF NO_LOGGER_MT}
    Lock;
    try
{$ENDIF}
        inherited RemoveFreeNotification(AComponent);
{$IFNDEF NO_LOGGER_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.InternalCloseLogFile;
begin
    if Assigned(FLogFile) then begin
        FLogFile.Free;
        FLogFile := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.InternalOpenLogFile;
{$IFDEF COMPILER12_UP}
var
    Bom : array [0..1] of Byte;
    Len : Integer;
{$ENDIF}    
begin
    InternalCloseLogFile;
    if Length(FLogFileName) = 0 then
        //FLogFilename := 'Debug_Out_' + ChangeFileExt(ExtractFilename(ParamStr(0)), '.txt');
        raise ELoggerException.Create('File name empty');
    if not FileExists(FLogFileName) then begin
        try
            FLogFile := TFileStream.Create(FLogFileName, fmCreate);
        except
            on E : Exception do
                raise ELoggerException.Create(E.Message);
        end;
        InternalCloseLogFile;
    end;
    try
        FLogFile := TFileStream.Create(FLogFileName,
                                       fmOpenReadWrite or fmShareDenyWrite);
    except
        on E : Exception do
            raise ELoggerException.Create(E.Message);
    end;
{$IFNDEF COMPILER12_UP}
    if FLogFileOption = lfoAppend then
        FLogFile.Seek(0, soFromEnd)
    else
        FLogFile.Size := 0;
{$ELSE}
    if FLogFileOption = lfoAppend then
    begin
        Len := FLogFile.Read(Bom[0], Length(Bom));
        if Len = 0 then begin
            FLogFileInternalEnc := FLogFileEncoding;
            if FLogFileInternalEnc = lfeUtf16 then
            begin
                Bom[0] := $FF; Bom[1] := $FE;
                FLogFile.Write(Bom[0], Length(Bom));
            end;
        end
        else if (Len = 2) and (Bom[0] = $FF) and (Bom[1] = $FE) then
            FLogFileInternalEnc := lfeUtf16
        else
            FLogFileInternalEnc := lfeUtf8;
        FLogFile.Seek(0, soFromEnd);
    end
    else begin
        FLogFile.Size := 0;
        FLogFileInternalEnc := FLogFileEncoding;
        if FLogFileInternalEnc = lfeUtf16 then
        begin
            Bom[0] := $FF; Bom[1] := $FE;
            FLogFile.Write(Bom[0], Length(Bom));
        end;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsLogger.AddTimeStamp: String; {V6.03}
begin
    DateTimeToString(Result, FTimeStampFormatString, Now);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.WriteToLogFile(const S: String);
{$IFDEF COMPILER12_UP}
var
    UStr : Utf8String;
{$ENDIF}
begin
    if not Assigned(FLogFile) then
        InternalOpenLogFile;
{$IFDEF COMPILER12_UP}
    if FLogFileInternalEnc = lfeUtf8 then begin
        UStr := Utf8Encode(S);
        FLogFile.Write(Pointer(UStr)^, Length(UStr));
    end
    else
        FLogFile.Write(Pointer(S)^, Length(S) * SizeOf(Char));
{$ELSE}
    FLogFile.Write(Pointer(S)^, Length(S));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.DoDebugLog(
    Sender      : TObject; 
    LogOption   : TLogOption;
    const Msg   : String);
begin
{$IFNDEF NO_LOGGER_MT}
    Lock;
    try
{$ENDIF}
        if csDestroying in ComponentState then             { V1.02 }
            Exit;
        if loAddStamp in FLogOptions then begin
            if loDestEvent in FLogOptions then
                if Assigned(FOnIcsLogEvent) then
                    FOnIcsLogEvent(Sender, LogOption, AddTimeStamp +
                                   FTimeStampSeparator + Msg); {V6.03}
            if loDestOutDebug in FLogOptions then
                OutputDebugStr(AddTimeStamp +
                                  FTimeStampSeparator + Msg); {V6.03}
            if loDestFile in FLogOptions then
                WriteToLogFile(AddTimeStamp + FTimeStampSeparator +
                               Msg + #13#10); {V6.03}
        end
        else begin
            if loDestEvent in FLogOptions then
                if Assigned(FOnIcsLogEvent) then
                    FOnIcsLogEvent(Sender, LogOption, Msg);
            if loDestOutDebug in FLogOptions then
                OutputDebugStr(Msg);
            if loDestFile in FLogOptions then
                WriteToLogFile(Msg + #13#10);
        end;
{$IFNDEF NO_LOGGER_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.SetLogFileName(const Value: String);
begin
{$IFNDEF NO_LOGGER_MT}
    Lock;
    try
{$ENDIF}
        if (CompareStr(Value, FLogFileName) <> 0) then begin
            FLogFileName := Value;
            InternalCloselogFile;
        end
        else if Value <> FLogFileName then
            FLogFileName := Value;
{$IFNDEF NO_LOGGER_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.SetLogFileOption(const Value: TLogFileOption);
begin
{$IFNDEF NO_LOGGER_MT}
    Lock;
    try
{$ENDIF}
        FLogFileOption := Value;
{$IFNDEF NO_LOGGER_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.SetLogOptions(const Value: TLogOptions);
begin
{$IFNDEF NO_LOGGER_MT}
    Lock;
    try
{$ENDIF}
        FLogOptions := Value;
{$IFNDEF NO_LOGGER_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.SetOnIcsLogEvent(const Value: TIcsLogEvent);
begin
{$IFNDEF NO_LOGGER_MT}
    Lock;
    try
{$ENDIF}
        FOnIcsLogEvent := Value;
{$IFNDEF NO_LOGGER_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_LOGGER_MT}
procedure TIcsLogger.Lock;
begin
    FLock.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.UnLock;
begin
    FLock.Leave;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.CloseLogFile;
begin
{$IFNDEF NO_LOGGER_MT}
    Lock;
    try
{$ENDIF}
        InternalCloseLogFile;
{$IFNDEF NO_LOGGER_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.OpenLogFile;
begin
{$IFNDEF NO_LOGGER_MT}
    Lock;
    try
{$ENDIF}
        InternalOpenLogFile;
{$IFNDEF NO_LOGGER_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
