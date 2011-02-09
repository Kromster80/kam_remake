{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels
Description:  Logger class donated to ICS project.
Creation:     December 2005
Version:      1.02
EMail:        <arno.garrels@gmx.de>
              francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2005-2007 by François PIETTE
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

History:
Dec 31, 2005 V1.01 F. Piette cleaned the code
Jan 18, 2006 V1.02 Fixed an AV on destroy

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit IcsLogger;
{$B-}              { Enable partial boolean evaluation   }
{$T-}              { Untyped pointers                    }
{$X+}              { Enable extended syntax              }
{$I ICSDEFS.INC}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF COMPILER2_UP}{ Not for Delphi 1                    }
    {$H+}            { Use long Strings                    }
    {$J+}            { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

{ If NO_ADV_MT is defined, then there is less multithread code compiled.    }

{#$DEFINE NO_ADV_MT}

{$IFDEF DELPHI1}
    {$DEFINE NO_ADV_MT}
{$ENDIF}

interface

uses
{$IFDEF CLR}
    System.Text,
    System.IO,
{$ENDIF}
    Windows, SysUtils, Classes;

const
    TIcsLoggerVersion   = 101;
    CopyRight : String  = ' IcsLogger (c) 2005-2007 Arno Garrels V1.01 ';

type
    ELoggerException = class(Exception);
    TLogOption = (loDestEvent,   loDestFile,     loDestOutDebug,  { Output Destinations }
                  loAddStamp,                                     { Adds something (slow) }
                  loWsockErr,    loWsockInfo,    loWsockDump,
                  loSslErr,      loSslInfo,      loSslDump,
                  loProtSpecErr, loProtSpecInfo, loProtSpecDump);
    TLogOptions = set of TLogOption;
    TLogFileOption = (lfoAppend, lfoOverwrite);
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
        FLogOptions         : TLogOptions;
        FOnIcsLogEvent      : TIcsLogEvent;
        FLogFileName        : String;
        FLogFile            : TFileStream;
        FLogFileOption      : TLogFileOption;
    {$IFNDEF NO_ADV_MT}
        FLock               : TRtlCriticalSection;
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
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   OpenLogFile;
        procedure   CloseLogFile;
        procedure   DoDebugLog (Sender    : TObject;
                                LogOption : TLogOption;
                                const Msg : String);
    published
        property    LogFileOption : TLogFileOption       read  FLogFileOption
                                                         write SetLogFileOption;
        property    LogFileName   : String               read  FLogFileName
                                                         write SetLogFileName;
        property    LogOptions    : TLogOptions          read  FLogOptions
                                                         write SetLogOptions;
        property    OnIcsLogEvent : TIcsLogEvent         read  FOnIcsLogEvent
                                                         write SetOnIcsLogEvent;
    end;

{$IFDEF CLR}
  TOutputDebugStringType = type String;
{$ENDIF}
{$IFDEF WIN32}
  TOutputDebugStringType = PChar;
{$ENDIF}

function IcsLoggerAddTimeStamp: String;
procedure Register;

implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TIcsLogger]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsLoggerAddTimeStamp: String;
begin
    DateTimeToString(Result, 'hh:nn:ss:zzz', Time);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsLogger.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
{$IFNDEF NO_ADV_MT}
    InitializeCriticalSection(FLock);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsLogger.Destroy;
begin
    CloseLogFile;
{$IFNDEF NO_ADV_MT}
    DeleteCriticalSection(FLock);
{$ENDIF}
    inherited Destroy;
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
    if FLogFileOption = lfoAppend then
        FLogFile.Seek(0, soFromEnd)
    else
        FLogFile.Size := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.WriteToLogFile(const S: String);
{$IFDEF CLR}
var
    Ascii : ASCIIEncoding;
    B     : TBytes;
begin
    if not Assigned(FLogFile) then
        InternalOpenLogFile;
    Ascii := ASCIIEncoding.Create;
    B     := Ascii.GetBytes(S);
    FLogFile.Write(B, Length(B));
end;
{$ELSE}
begin
    if not Assigned(FLogFile) then
        InternalOpenLogFile;
    FLogFile.Write(PChar(S)^, Length(S));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.DoDebugLog(
    Sender      : TObject;
    LogOption   : TLogOption;
    const Msg   : String);
begin
{$IFNDEF NO_ADV_MT}
    Lock;
    try
{$ENDIF}
        if csDestroying in Componentstate then             { V1.02 }
            Exit;
        if loAddStamp in FLogOptions then begin
            if loDestEvent in FLogOptions then
                if Assigned(FOnIcsLogEvent) then
                    FOnIcsLogEvent(Sender, LogOption, IcsLoggerAddTimeStamp +
                                   ' ' + Msg);
            if loDestOutDebug in FLogOptions then
                OutputDebugString(TOutputDebugStringType(IcsLoggerAddTimeStamp + ' ' + Msg));
            if loDestFile in FLogOptions then
                WriteToLogFile(IcsLoggerAddTimeStamp + ' ' + Msg + #13#10);
        end
        else begin
            if loDestEvent in FLogOptions then
                if Assigned(FOnIcsLogEvent) then
                    FOnIcsLogEvent(Sender, LogOption, Msg);
            if loDestOutDebug in FLogOptions then
                OutputDebugString(TOutputDebugStringType(Msg));
            if loDestFile in FLogOptions then
                WriteToLogFile(Msg + #13#10);
        end;
{$IFNDEF NO_ADV_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.SetLogFileName(const Value: String);
begin
{$IFNDEF NO_ADV_MT}
    Lock;
    try
{$ENDIF}
        if (CompareStr(Value, FLogFileName) <> 0) then begin
            FLogFileName := Value;
            InternalCloselogFile;
        end
        else if Value <> FLogFileName then
            FLogFileName := Value;
{$IFNDEF NO_ADV_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.SetLogFileOption(const Value: TLogFileOption);
begin
{$IFNDEF NO_ADV_MT}
    Lock;
    try
{$ENDIF}
        FLogFileOption := Value;
{$IFNDEF NO_ADV_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.SetLogOptions(const Value: TLogOptions);
begin
{$IFNDEF NO_ADV_MT}
    Lock;
    try
{$ENDIF}
        FLogOptions := Value;
{$IFNDEF NO_ADV_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.SetOnIcsLogEvent(const Value: TIcsLogEvent);
begin
{$IFNDEF NO_ADV_MT}
    Lock;
    try
{$ENDIF}
        FOnIcsLogEvent := Value;
{$IFNDEF NO_ADV_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_ADV_MT}
procedure TIcsLogger.Lock;
begin
    EnterCriticalSection(FLock)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.UnLock;
begin
    LeaveCriticalSection(FLock)
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.CloseLogFile;
begin
{$IFNDEF NO_ADV_MT}
    Lock;
    try
{$ENDIF}
        InternalCloseLogFile;
{$IFNDEF NO_ADV_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsLogger.OpenLogFile;
begin
{$IFNDEF NO_ADV_MT}
    Lock;
    try
{$ENDIF}
        InternalOpenLogFile;
{$IFNDEF NO_ADV_MT}
    finally
        Unlock;
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
