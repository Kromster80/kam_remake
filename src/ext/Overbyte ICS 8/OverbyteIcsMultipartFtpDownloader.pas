{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     May 6, 2007
Version:      8.00 ALPHA CODE
Description:  TMultipartFtpDownloader is a component to download files using
              simultaneous connections to speedup download. The demo make
              also use of the TMultiProgressBar (included in ICS) which is
              a segmented progress bar.
EMail:        francois.piette@overbyte.be         http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2007 by François PIETTE
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

Updates:
Sep 10, 2007 Added timeout to Size operation
Oct 30, 2010 0.99b In DownloadDocData, fixed call to Seek so that the int64
             overloaded version is used.
Oct 31, 2010 0.99c In DownloadDocData, restart the timeout timer.
             Renamed protected TMultipartFtpDownloader.FHttp member to
             FMyFtp (Breaking change. Not an issue since we are in alpha phase).
Nov 08, 2010 0.99d Arno improved final exception handling, more details
             in OverbyteIcsWndControl.pas (V1.14 comments).
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsMultipartFtpDownloader;
{$ENDIF}

{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Ics.Posix.WinTypes,
    Ics.Posix.Messages,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.IniFiles{$ELSE}IniFiles{$ENDIF},
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsFtpCli,
    Ics.Fmx.OverbyteIcsWndControl,
{$ELSE}
    OverbyteIcsFtpCli,
    OverbyteIcsWndControl,
{$ENDIF}
    OverbyteIcsUrl,
    OverbyteIcsUtils;

type
    TFtpBigInt                = int64;
    TDisplayEvent             = procedure (Sender       : TObject;
                                           const Msg    : String) of object;
    TRequestDoneEvent         = procedure (Sender       : TObject;
                                           ErrorCode    : Integer;
                                           const Reason : String) of object;
    TProgressAddSegmentEvent  = procedure (Sender       : TObject;
                                           StartOffset  : Int64;
                                           ASpan        : Int64;
                                           InitPos      : Int64) of Object;
    TProgressSetPositionEvent = procedure (Sender       : TObject;
                                           Index        : Integer;
                                           Position     : Int64) of object;
    TFtpDocDataEvent          = procedure (Sender       : TObject;
                                           Data         : Pointer;
                                           Len          : Integer) of object;

    TMyFtpCli = class(TFtpClient)
    protected
        FDataCount   : TFtpBigInt;
        FDataMax     : TFtpBigInt;
        FStartOffset : TFtpBigInt;
        FEndOffset   : TFtpBigInt;
        FIndex       : Integer;
        FDone        : Boolean;
        FOnDocData   : TFtpDocDataEvent;
        procedure LocalStreamWrite(const Buffer; Count : Integer); override;
    published
        property OnDocData   : TFtpDocDataEvent read  FOnDocData
                                                write FOnDocData;
    end;

    TMultipartFtpDownloader = class(TIcsWndControl)
    protected
        FMyFtp                 : array of TMyFtpCli;
        FPassive               : Boolean;
        FBinary                : Boolean;
        FPartCount             : Integer;
        FPass                  : String;
        FUser                  : String;
        FServer                : String;
        FFileName              : String;
        FDir                   : String;
        FPort                  : String;
        FAbortFlag             : Boolean;
        FPauseFlag             : Boolean;
        FTimeoutFlag           : Boolean;
        FStateFileName         : String;
        FFileStream            : TStream;
        FContentLength         : TFtpBigInt;
        FTotalCount            : TFtpBigInt;
        FPrevCount             : TFtpBigInt;
        FPrevTick              : Cardinal;
        FStartTime             : TDateTime;
        FElapsedTime           : TDateTime;
        FCurSpeed              : Double;
        FPercentDone           : Double;
        FTimeoutValue          : Integer;  // Milli-seconds
        FAssumedSize           : TFtpBigInt;  // DOESN'T WORK !
        FOnDisplay             : TDisplayEvent;
        FOnRequestDone         : TRequestDoneEvent;
        FOnProgressAddSegment  : TProgressAddSegmentEvent;
        FOnProgressSetPosition : TProgressSetPositionEvent;
        FOnShowStats           : TNotifyEvent;
        FMsg_WM_START_MULTI    : UINT;
        FMsg_WM_PART_DONE      : UINT;
        Timer1                 : TIcsTimer;
        TimeoutTimer           : TIcsTimer;
        procedure AbortComponent; override; { 0.99d }
        procedure AllocateMsgHandlers; override;
        procedure FreeMsgHandlers; override;
        function  MsgHandlersCount: Integer; override;
        procedure SizeASyncRequestDone(Sender    : TObject;
                                       Request   : TFtpRequest;
                                       ErrorCode : Word);
        procedure SizeSessionClosed(Sender: TObject; ErrCode: Word);
        procedure GetASyncRequestDone(Sender    : TObject;
                                      Request   : TFtpRequest;
                                      ErrorCode : Word);
        procedure Display(const Msg: String);
        procedure DisplayHandler(Sender : TObject; var Msg: String);
        procedure WMStartMulti(var msg: TMessage);
        procedure WMPartDone(var msg: TMessage);
        procedure WndProc(var MsgRec: TMessage); override;
        procedure DownloadRequestDone(Sender    : TObject;
                                      Request   : TFtpRequest;
                                      ErrorCode : Word);
        procedure DownloadDocData(Sender : TObject;
                                  Data   : Pointer;
                                  Len    : Integer);
        procedure CheckDone(ErrCode : Integer; const Reason : String);
        procedure Timer1Timer(Sender: TObject);
        procedure SizeTimeoutTimerTimer(Sender: TObject);
        procedure RestartDownload(MyFtp : TMyFtpCli);
        procedure TriggerRequestDone(ErrCode      : Integer;
                                     const Reason : String); virtual;
        procedure TriggerShowStats; virtual;
        procedure TriggerProgressSetPosition(Index    : Integer;
                                             Position : Int64); virtual;
        procedure TriggerProgressAddSegment(StartOffset, ASpan,
                                            InitPos: Int64); virtual;
        procedure LoadStatus;
        procedure SaveStatus;
        procedure SetStateFileName(const Value: String);
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Start;
        procedure Abort;
        procedure Pause;
        procedure Resume;
        property  TotalCount            : TFtpBigInt read FTotalCount;
        property  ContentLength         : TFtpBigInt read FContentLength;
        property  CurSpeed              : Double      read FCurSpeed;
        property  ElapsedTime           : TDateTime   read FElapsedTime;
        property  PercentDone           : Double      read FPercentDone;
    published
        property Server        : String  read  FServer
                                         write FServer;
        property Port          : String  read  FPort
                                         write FPort;
        property User          : String  read  FUser
                                         write FUser;
        property Pass          : String  read  FPass
                                         write FPass;
        property Dir           : String  read  FDir
                                         write FDir;
        property FileName      : String  read  FFileName
                                         write FFileName;
        property Passive       : Boolean read  FPassive
                                         write FPassive;
        property Binary        : Boolean read  FBinary
                                         write FBinary;
        property PartCount     : Integer read  FPartCount
                                         write FPartCount;
        property FileStream    : TStream read  FFileStream
                                         write FFileStream;
        property StateFileName : String  read  FStateFileName
                                         write SetStateFileName;
        property TimeoutValue  : Integer read  FTimeoutValue
                                         write FTimeoutValue;
        property AssumedSize   : TFtpBigInt        read  FAssumedSize
                                                   write FAssumedSize;
        property OnDisplay     : TDisplayEvent     read  FOnDisplay
                                                   write FOnDisplay;
        property OnRequestDone : TRequestDoneEvent read  FOnRequestDone
                                                   write FOnRequestDone;
        property OnProgressAddSegment  : TProgressAddSegmentEvent
                                                   read  FOnProgressAddSegment
                                                   write FOnProgressAddSegment;
        property OnProgressSetPosition : TProgressSetPositionEvent
                                                   read  FOnProgressSetPosition
                                                   write FOnProgressSetPosition;
        property OnShowStats           : TNotifyEvent
                                                   read  FOnShowStats
                                                   write FOnShowStats;
        property OnBgException;                                       { 0.99d }
    end;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMultipartFtpDownloader.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    AllocateHWnd;
    FTimeoutValue         := 30000;   // 30 second timeout by default
    Timer1                := TIcsTimer.Create(Self);
    Timer1.Enabled        := FALSE;
    Timer1.Interval       := 1000;
    Timer1.OnTimer        := Timer1Timer;
    TimeoutTimer          := TIcsTimer.Create(Self);
    TimeoutTimer.Enabled  := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMultipartFtpDownloader.Destroy;
var
    I : Integer;
begin
    for I := 0 to Length(FMyFtp) - 1 do
        FreeAndNil(FMyFtp[I]);
    SetLength(FMyFtp, 0);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.Display(const Msg: String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.DisplayHandler(
    Sender  : TObject;
    var Msg : String);
var
    FtpCli : TMyFtpCli;
begin
    FtpCli := Sender as TMyFtpCli;
    Display(Format('%03.3d %s', [FtpCli.FIndex + 1, Msg]));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.Start;
var
    I : Integer;
begin
    if FPartCount < 1 then
        raise ERangeError.Create('PartCount (' + IntToStr(FPartCount) +
                                 ') must be positive and not nul');
    FAbortFlag     := FALSE;
    FPauseFlag     := FALSE;
    FTimeoutFlag   := FALSE;
    FContentLength := -1;
    FTotalCount    := 0;
    FPrevCount     := 0;
    FPrevTick      := IcsGetTickCount;
    FStartTime     := Now;
    FElapsedTime   := 0;
    TriggerShowStats;

    for I := 0 to Length(FMyFtp) - 1 do
        FreeAndNil(FMyFtp[I]);

    // First we need to get the size of the file
    SetLength(FMyFtp, 1);
    FMyFtp[0]                  := TMyFtpCli.Create(Self);
    FMyFtp[0].FIndex           := 0;
    FMyFtp[0].HostName         := FServer;
    FMyFtp[0].Port             := FPort;
    FMyFtp[0].Username         := FUser;
    FMyFtp[0].Password         := FPass;
    FMyFtp[0].HostDirName      := FDir;
    FMyFtp[0].HostFileName     := FFileName;
    FMyFtp[0].OnDisplay        := DisplayHandler;
    FMyFtp[0].OnBgException    := OnBgException;  { 0.99d }
    FMyFtp[0].ExceptAbortProc  := AbortComponent; { 0.99d }
    // We don't need to get a size if we have an assumed size
    if FAssumedSize > 0 then begin
        FContentLength := FAssumedSize;
        PostMessage(Handle, FMsg_WM_START_MULTI, 0, 0);
        Exit;
    end;

    FMyFtp[0].OnRequestDone    := SizeASyncRequestDone;
    FMyFtp[0].OnSessionClosed  := SizeSessionClosed;
    FMyFtp[0].OpenAsync;
    Display('SizeASync');
    TimeoutTimer.OnTimer  := SizeTimeoutTimerTimer;
    TimeoutTimer.Interval := FTimeoutValue;
    TimeoutTimer.Enabled  := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// This event handler is called for all operation related to getting file size
procedure TMultipartFtpDownloader.SizeASyncRequestDone(
    Sender    : TObject;
    Request   : TFtpRequest;
    ErrorCode : Word);
var
    FtpCli : TMyFtpCli;
begin
    Display('SizeASyncRequestDone');
    TimeoutTimer.Enabled := FALSE;
    FtpCli := Sender as TMyFtpCli;

    if Request = ftpQuitAsync then begin
        // We can safely ignore any error here
        // We will get an OnSessionClosed event
        exit;
    end;

    if FTimeoutFlag then begin
        // ToDo: Check if 426 is correct error code to return
        TriggerRequestDone(426, 'Download failed: timeout');
        Exit;
    end;

    if FAbortFlag then begin
        Display('Abort has been called');
        // ToDo: Check if 426 is correct error code to return
        TriggerRequestDone(426, 'Download aborted');
        Exit;
    end;

    if ErrorCode <> 0 then begin
        Display('ErrorCode = ' + IntToStr(ErrorCode));
        TriggerRequestDone(ErrorCode, 'Download failed');
        Exit;
    end;
    case Request of
    ftpOpenAsync:  FtpCli.UserAsync;
    ftpUserAsync:  FtpCli.PassAsync;
    ftpPassAsync:  FtpCli.CwdAsync;
    ftpCwdAsync:   FtpCli.SizeAsync;
    ftpSizeAsync:
        begin
            FContentLength := FtpCli.SizeResult;
            FtpCli.QuitAsync;
        end;
    else
        Display('Unexpected FTP component state ' + IntToStr(Ord(Request)));
        TriggerRequestDone(-1, 'Download failed, internal error');
        Exit;
    end;
    TimeoutTimer.Interval := FTimeoutValue;
    TimeoutTimer.Enabled  := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.SizeSessionClosed(
    Sender  : TObject;
    ErrCode : Word);
begin
    Display('SizeSessionClosed');
    if FAbortFlag then begin
        Display('Abort has been called');
        Exit;
    end;

    if FContentLength > 0 then begin
        // We are happy with a document to get
        PostMessage(Handle, FMsg_WM_START_MULTI, 0, 0);
        Exit;
    end;
    if FContentLength = 0 then begin
        TriggerRequestDone(0, 'Download success, empty file');
        Exit;
    end;
    TriggerRequestDone(ErrCode, 'Download failed');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.GetASyncRequestDone(
    Sender    : TObject;
    Request   : TFtpRequest;
    ErrorCode : Word);
var
    FtpCli : TMyFtpCli;
begin
    Display('GetASyncRequestDone');
    if FContentLength > 0 then begin
        // We are happy with a document to get
        PostMessage(Handle, FMsg_WM_START_MULTI, 0, 0);
        Exit;
    end;

    FtpCli := Sender as TMyFtpCli;
    if ErrorCode <> 0 then begin
        Display('ErrorCode = ' + IntToStr(ErrorCode));
        TriggerRequestDone(ErrorCode, 'Download failed');
        Exit;
    end;
    if FtpCli.StatusCode <> 200 then begin
        Display('StatusCode = ' + IntToStr(FtpCli.StatusCode) + ' ' +
                FtpCli.LastResponse);
        TriggerRequestDone(FtpCli.StatusCode, FtpCli.LastResponse);
        Exit;
    end;
    Display('RequestDone DataCount = ' + IntToStr(FtpCli.FDataCount));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMultipartFtpDownloader.MsgHandlersCount : Integer;
begin
    Result := 2 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.AbortComponent; { 0.99d }
begin
    try
        Abort;
    except
    end;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_START_MULTI := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_PART_DONE   := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_PART_DONE);
        FWndHandler.UnregisterMessage(FMsg_WM_START_MULTI);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.WndProc(var MsgRec: TMessage);
begin
    try { 0.99d }
        with MsgRec do begin
            if Msg = FMsg_WM_START_MULTI then
                WMStartMulti(MsgRec)
            else if Msg = FMsg_WM_PART_DONE then
                WMPartDone(MsgRec)
            else
                inherited WndProc(MsgRec);
        end;
    except { 0.99d }
        on E: Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.WMStartMulti(var msg: TMessage);
var
    Chunk  : TFtpBigInt;
    Offset : TFtpBigInt;
    I      : Integer;
    MyFtp  : TMyFtpCli;
    OldFtp : TMyFtpCli;
begin
    // Do not destroy current component before having allocated the new
    // it still has to purge events from the queue. Chances are that the
    // new components handle those events !
    OldFtp                 := FMyFtp[0];
    OldFtp.OnRequestDone   := nil;
    OldFtp.OnSessionClosed := nil;
    OldFtp.OnDisplay       := nil;

    Chunk  := FContentLength div FPartCount;
    Offset := 0;
    SetLength(FMyFtp, FPartCount);
    for I := 0 to FPartCount - 1 do begin
        FMyFtp[I]                := TMyFtpCli.Create(Self);
        MyFtp                   := FMyFtp[I];
        MyFtp.FStartOffset      := Offset;
        if I < (FPartCount - 1) then
            MyFtp.FEndOffset    := Offset + Chunk
        else
            MyFtp.FEndOffset    := FContentLength;
        Offset                   := Offset + Chunk + 1;
        MyFtp.ResumeAt          := MyFtp.FStartOffset;
        TriggerProgressAddSegment(MyFtp.FStartOffset,
                                  MyFtp.FEndOffset - MyFtp.FStartOffset,
                                  MyFtp.FStartOffset);
        MyFtp.FIndex            := I;
        MyFtp.HostName          := FServer;
        MyFtp.Port              := FPort;
        MyFtp.Username          := FUser;
        MyFtp.Password          := FPass;
        MyFtp.HostDirName       := FDir;
        MyFtp.HostFileName      := FFileName;
        MyFtp.LocalStream       := FFileStream;
        MyFtp.Passive           := FPassive;
        MyFtp.Binary            := FBinary;
        MyFtp.OnRequestDone     := DownloadRequestDone;
        MyFtp.OnDocData         := DownloadDocData;
        MyFtp.OnDisplay         := DisplayHandler;
        MyFtp.OnBgException     := OnBgException;  { 0.99d }
        MyFtp.ExceptAbortProc   := AbortComponent; { 0.99d }
        MyFtp.FDataCount        := 0;
        MyFtp.FDone             := FALSE;
        MyFtp.Options           := [ftpNoAutoResumeAt];
        //ListBox1.Items.Add('0');
    end;
    for I := 0 to Length(FMyFtp) - 1 do
        FMyFtp[I].OpenASync;
    Timer1.Enabled := TRUE;
    // Now it is safe to destroy the old component (the one used for Size)
    OldFtp.Free;
    Display('OpenASync');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.LoadStatus;
const
    Default = '0';
var
    SectionNames  : TStringList;
    Cnt           : Integer;
    I             : Integer;
    MyFtp        : TMyFtpCli;
    IniFile       : TIniFile;
    //Dls           : TDownloadState;
begin
    for I := 0 to Length(FMyFtp) - 1 do
        FreeAndNil(FMyFtp[I]);
    SetLength(FMyFtp, 0);

    FTotalCount    := 0;
    FContentLength := 0;
    FServer        := '';
    FPort          := '';
    FPartCount     := 0;

    if not FileExists(FStateFileName) then
        Exit;

    IniFile     := TIniFile.Create(FStateFileName);
    try
        //FFileName := IniFile.ReadString('GLOBAL', 'FileName', '');
        FContentLength := StrToInt64Def(IniFile.ReadString('GLOBAL', 'ContentLength', Default), 0);
        FServer        := IniFile.ReadString('GLOBAL', 'Server', '');
        FPort          := IniFile.ReadString('GLOBAL', 'Port', '');
        FPartCount     := IniFile.ReadInteger('GLOBAL', 'PartCount', 1);
        SetLength(FMyFtp, FPartCount);
        if FPartCount > 0 then begin
            SectionNames := TStringList.Create;
            try
                IniFile.ReadSections(SectionNames);
                for I := 0 to SectionNames.Count - 1 do begin
                    if Copy(SectionNames[I], 1, 5) <> 'PART_' then
                        Continue;
                    Cnt                     := StrToInt(Copy(SectionNames[I], 6, 8));
                    FMyFtp[Cnt]              := TMyFtpCli.Create(Self);
                    MyFtp                  := FMyFtp[Cnt];
                    MyFtp.FIndex           := Cnt;
                    MyFtp.FStartOffset     := StrToInt64Def(IniFile.ReadString(SectionNames[I], 'StartOffset', Default), 0);
                    MyFtp.FEndOffset       := StrToInt64Def(IniFile.ReadString(SectionNames[I], 'EndOffset',   Default), 0);
                    MyFtp.FDataCount       := StrToInt64Def(IniFile.ReadString(SectionNames[I], 'DataCount',   Default), 0);
//                  MyFtp.FDone            := IniFile.ReadBool(SectionNames[I], 'Done', FALSE);
                    MyFtp.HostName         := IniFile.ReadString(SectionNames[I], 'Server', '');
                    MyFtp.Port             := IniFile.ReadString(SectionNames[I], 'Port', '');
                    MyFtp.OnRequestDone    := DownloadRequestDone;
                    MyFtp.OnDocData        := DownloadDocData;
                    Inc(FTotalCount, MyFtp.FDataCount);
                end;
                FPrevCount    := FTotalCount;
                FPrevTick     := IcsGetTickCount;
                FStartTime    := Now;
                FElapsedTime  := 0;
            finally
                FreeAndNil(SectionNames);
            end;
        end;
    finally
        FreeAndNil(IniFile);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.SaveStatus;
var
    SectionNames  : TStringList;
    I             : Integer;
    IniFile       : TIniFile;
begin
    IniFile := TIniFile.Create(FStateFileName);
    try
        SectionNames := TStringList.Create;
        try
            IniFile.ReadSections(SectionNames);
            for I := 0 to SectionNames.Count - 1 do begin
                if Copy(SectionNames[I], 1, 5) = 'PART_' then
                    IniFile.EraseSection(SectionNames[I]);
            end;
        finally
            FreeAndNil(SectionNames);
        end;
        IniFile.EraseSection('GLOBAL');
        IniFile.WriteString('GLOBAL',  'Server', FServer);
        IniFile.WriteString('GLOBAL',  'Port',   FPort);
        IniFile.WriteString('GLOBAL',  'ContentLength', IntToStr(FContentLength));
        IniFile.WriteInteger('GLOBAL', 'PartCount', Length(FMyFtp));
        for I := 0 to Length(FMyFtp) - 1 do begin
            IniFile.WriteString('PART_' + IntToStr(I), 'StartOffset', IntToStr(FMyFtp[I].FStartOffset));
            IniFile.WriteString('PART_' + IntToStr(I), 'EndOffset', IntToStr(FMyFtp[I].FEndOffset));
            IniFile.WriteString('PART_' + IntToStr(I), 'DataCount', IntToStr(FMyFtp[I].FDataCount));
            IniFile.WriteString('PART_' + IntToStr(I), 'Server', FMyFtp[I].HostName);
            IniFile.WriteString('PART_' + IntToStr(I), 'Port',   FMyFtp[I].Port);
            FMyFtp[I].FDone := (FMyFtp[I].FDataCount >= (FMyFtp[I].FEndOffset - FMyFtp[I].FStartOffset));
        end;
    finally
        FreeAndNil(IniFile);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
type
    TOKStatus = record
        Rq   : TFtpRequest;
        St   : array [0..4] of Integer;
    end;

procedure TMultipartFtpDownloader.DownloadRequestDone(
    Sender    : TObject;
    Request   : TFtpRequest;
    ErrorCode : Word);
var
    FtpCli   : TMyFtpCli;
    ErrCode   : Integer;
    Reason    : String;
const
    OKStatus : array [0..1] of TOKStatus = (
       (Rq: ftpOpenAsync; St: (0, 0, 0, 0, 0)),
       (Rq: ftpUserAsync; St: (0, 0, 0, 0, 0))
    );
begin
    FtpCli := Sender as TMyFtpCli;

    if FAbortFlag then begin
        // We are aborting transfert, just ignore any error
        Display(Format('%03.3d %s',
                       [FtpCli.FIndex + 1, 'Download done : Aborted']));
        ErrCode := 503;  // 503 is service unavailable
        Reason  := 'Transfert aborted';
    end
    else if FPauseFlag then begin
        // We are aborting transfert, just ignore any error
        Display(Format('%03.3d %s',
                       [FtpCli.FIndex + 1, 'Download done : Paused']));
        ErrCode := 204;
        Reason  := 'Transfert paused';
    end
    else if FtpCli.FDone then begin
        Display(Format('%03.3d %s',
                       [FtpCli.FIndex + 1,  'DownloadRequestDone']));
        ErrCode := 0;  // This part is finished, it is OK
        Reason  := 'Part finished';
    end
    else begin
        if Request = ftpQuitAsync then begin
            // We can safely ignore any error here
            Exit;
        end;

        if ErrorCode <> 0 then begin
            Display(Format('%03.3d Download  ErrorCode = %d, Msg = %s',
                           [FtpCli.FIndex + 1,
                            ErrorCode, FtpCli.LastResponse]));
            // RestartDownload(FtpCli);
            // Exit;                      
        end;

        try
            case Request of
            ftpOpenAsync:    FtpCli.UserAsync;
            ftpUserAsync:    FtpCli.PassAsync;
            ftpPassAsync:    FtpCli.CwdAsync;
            ftpCwdAsync:     FtpCli.TypeSetAsync;
            ftpTypeSetAsync: FtpCli.RestGetAsync;
            ftpRestGetAsync: FtpCli.GetAsync;
            ftpGetAsync:     FtpCli.QuitAsync;
            else
                Display(Format('%03.3d %s %d',
                               [FtpCli.FIndex + 1,
                                'Unexpected FTP component state',
                                Ord(Request)]));
                TriggerRequestDone(-1, 'Download failed, internal error');
            end;
        except
            on E:Exception do begin
                if Request <> ftpGetAsync then begin
                    Display(Format('%03.3d %s %s: %s',
                                   [FtpCli.FIndex + 1,
                                   'Download  exception =',
                                    E.ClassName, E.Message]));
                    RestartDownload(FtpCli);
                end;
            end;
        end;
        Exit;
    end;
    CheckDone(ErrCode, Reason);

    FtpCli.FDone := TRUE;
    if FAbortFlag then begin
        // We are aborting transfert, just ignore any error
        Display(Format('%03.3d %s',
                       [FtpCli.FIndex + 1,  'Download done Aborted']));
        ErrCode := 503;  // 503 is service unavailable
        Reason  := 'Transfert aborted';
    end
    else if FPauseFlag then begin
        // We are aborting transfert, just ignore any error
        Display(Format('%03.3d %s',
                       [FtpCli.FIndex + 1,  'Download done Paused']));
        ErrCode := 204;
        Reason  := 'Transfert paused';
    end
    else begin
        if ErrorCode = 426 then begin
            // 426 is transfert aborted because part is finished
        end
        else if ErrorCode <> 0 then begin
            Display(Format('%03.3d %s %d',
                           [FtpCli.FIndex + 1,
                            'Download done ErrorCode =',
                            ErrorCode]));
            RestartDownload(FtpCli);
            Exit;
        end
        else if FtpCli.StatusCode <> 206 then begin
            Display(Format('%03.3d %s %d',
                           [FtpCli.FIndex + 1,
                            'Download done Status =',
                            FtpCli.StatusCode]));
            RestartDownload(FtpCli);
            Exit;
        end;
        Display(Format('%03.3d %s', [FtpCli.FIndex + 1, 'Download done OK']));
        ErrCode := 200;
        Reason  := 'OK';
    end;
    CheckDone(ErrCode, Reason);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.CheckDone(
    ErrCode      : Integer;
    const Reason : String);
var
    Done : Boolean;
    I    : Integer;
begin
    Done := TRUE;
    for I := 0 to Length(FMyFtp) - 1 do
        Done := Done and (FMyFtp[I].FDone);
    if Done then begin
        Timer1.Enabled := FALSE;
        Timer1.OnTimer(nil);
        Display('All done');
        if FPauseFlag then
            SaveStatus;
        TriggerRequestDone(ErrCode, Reason);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.DownloadDocData(
    Sender : TObject;
    Data   : Pointer;
    Len    : Integer);
var
    FtpCli : TMyFtpCli;
begin
    if Len <= 0 then
        Exit;
    if FPauseFlag then
        Exit;
    TimeoutTimer.Enabled  := FALSE;  // Stop timeout timer...
    TimeoutTimer.Enabled  := TRUE;   // and restart timeout timer
    FtpCli            := Sender as TMyFtpCli;
    FFileStream.Seek(FtpCli.FStartOffset + FtpCli.FDataCount,
                     soBeginning); // Warning: Using soFromBeginning make sthe compiler pick the 32 bit overload !
    FFileStream.WriteBuffer(Data^, Len);
    FtpCli.FDataCount := FtpCli.FDataCount + Len;
    FTotalCount        := FTotalCount + Len;
    TriggerProgressSetPosition(FtpCli.FIndex,
                               FtpCli.FStartOffset + FtpCli.FDataCount);

    // Todo: Check if that component has finished his range and abort the
    // transfer when finished.
    if FtpCli.FDataCount > (FtpCli.FEndOffset - FtpCli.FStartOffset) then begin
        // Component has finished his part
        Display(Format('%03.3d %s',
                       [FtpCli.FIndex + 1, 'Part done DownloadDocData']));
        PostMessage(Handle, FMsg_WM_PART_DONE, 0, LParam(FtpCli));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.WMPartDone(var msg: TMessage);
var
    FtpCli : TMyFtpCli;
begin
    FtpCli := TObject(Msg.LParam) as TMyFtpCli;
//  Display(Format('%03.3d %s', [FtpCli.FIndex + 1, 'Part done handler']));
    if not Assigned(FtpCli) then
        Exit;
    if FtpCli.FDone then
        Exit;
    FtpCli.FDone := TRUE;
    FtpCli.AbortAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.TriggerProgressSetPosition(
    Index    : Integer;
    Position : Int64);
begin
    if Assigned(FOnProgressSetPosition) then
        FOnProgressSetPosition(Self, Index, Position);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.TriggerProgressAddSegment(
    StartOffset : Int64;
    ASpan       : Int64;
    InitPos     : Int64);
begin
    if Assigned(FOnProgressAddSegment) then
        FOnProgressAddSegment(Self, StartOffset, ASpan, InitPos);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.TriggerShowStats;
begin
    if Assigned(FOnShowStats) then
        FOnShowStats(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.TriggerRequestDone(
    ErrCode      : Integer;
    const Reason : String);
begin
    if Assigned(FOnrequestDone) then
        FOnRequestDone(Self, ErrCode, Reason);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.SizeTimeoutTimerTimer(Sender: TObject);
begin
    Display('Timeout');
    FTimeoutFlag := TRUE;
    Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.Timer1Timer(Sender: TObject);
var
    FtpCli : TMyFtpCli;
    I       : Integer;
    Done    : Boolean;
    Tick    : Cardinal;
begin
    Done := TRUE;
    for I := 0 to Length(FMyFtp) - 1 do begin
        FtpCli := FMyFtp[I];
        Done    := Done and (FtpCli.FDone);
    end;

    Tick         := IcsGetTickCount;
    FCurSpeed    := 8 * (FTotalCount - FPrevCount) / (Tick - FPrevTick);
    FElapsedTime := Now - FStartTime;
    if FContentLength = 0 then
        FPercentDone := 0
    else
        FPercentDone := 100.0 * FTotalCount / FContentLength;
    TriggerShowStats;
    FPrevTick    := Tick;
    FPrevCount   := FTotalCount;
    if not Done then
        Exit;

    // Download is finished
    Timer1.Enabled := FALSE;
    FCurSpeed := 8 * FTotalCount / (FElapsedTime * 86400000);
    TriggerShowStats;
    FreeAndNil(FFileStream);
    Display('Done');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.RestartDownload(MyFtp: TMyFtpCli);
begin
    Display('Restarting ' + IntToStr(MyFtp.FIndex + 1));
    TriggerProgressSetPosition(MyFtp.FIndex, MyFtp.FStartOffset);
    MyFtp.ResumeAt          := MyFtp.FStartOffset;
    MyFtp.Username          := FUser;
    MyFtp.Password          := FPass;
    MyFtp.FDataCount        := 0;
    MyFtp.FDone             := FALSE;
    MyFtp.GetASync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.Abort;
var
    FtpCli : TMyFtpCli;
    I       : Integer;
begin
    FAbortFlag := TRUE;
    for I := 0 to Length(FMyFtp) - 1 do begin
        FtpCli := FMyFtp[I];
        FtpCli.Abort;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.Pause;
var
    FtpCli : TMyFtpCli;
    I       : Integer;
begin
    if StateFileName = '' then
        raise Exception.Create('TMultipartHttpDownloader.Pause: ' +
                               'No file name specified');
    FPauseFlag := TRUE;
    for I := 0 to Length(FMyFtp) - 1 do begin
        FtpCli := FMyFtp[I];
        FtpCli.Abort;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.Resume;
var
    FtpCli : TMyFtpCli;
    I       : Integer;
begin
    if StateFileName = '' then
        raise Exception.Create('TMultipartHttpDownloader.Resume: ' +
                               'No file name specified');

    FAbortFlag     := FALSE;
    FPauseFlag     := FALSE;
    LoadStatus;
    for I := 0 to Length(FMyFtp) - 1 do begin
        FtpCli                   := FMyFtp[I];
        FtpCli.HostName          := FServer;
        FtpCli.Port              := FPort;
        FtpCli.Username          := FUser;
        FtpCli.Password          := FPass;
        FtpCli.HostDirName       := FDir;
        FtpCli.HostFileName      := FFileName;
        FtpCli.LocalStream       := FFileStream;
        FtpCli.Passive           := FPassive;
        FtpCli.Binary            := FBinary;
        FtpCli.OnRequestDone     := DownloadRequestDone;
        FtpCli.OnDocData         := DownloadDocData;
        FtpCli.OnDisplay         := DisplayHandler;
        FtpCli.FDone             := (FtpCli.FDataCount >= (FtpCli.FEndOffset - FtpCli.FStartOffset));
        FtpCli.Options           := [ftpNoAutoResumeAt];
        FtpCli.ResumeAt          := FtpCli.FStartOffset + FtpCli.FDataCount;
        Display('Resuming ' + IntToStr(I) +
                ' Offset=' + IntToStr(FtpCli.ResumeAt) +
                ' DataCount=' + IntToStr(FtpCli.FDataCount));
        TriggerProgressAddSegment(FtpCli.FStartOffset,
                                  FtpCli.FEndOffset - FtpCli.FStartOffset,
                                  FtpCli.FStartOffset + FtpCli.FDataCount);
    end;
    // Start all download which is not done yet
    for I := 0 to Length(FMyFtp) - 1 do begin
        if not FMyFtp[I].FDone then
            FMyFtp[I].OpenASync;
    end;
    Timer1.Enabled := TRUE;
    CheckDone(0, 'OK');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartFtpDownloader.SetStateFileName(const Value: String);
begin
    // We use TIniFile which create a file with no path into Windows directory
    // Add the current directory specifier to the filename if required.
    if ExtractFilePath(Value) = '' then
        FStateFileName := '.\' + Value
    else
        FStateFileName := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMyFtpCli.LocalStreamWrite(const Buffer; Count: Integer);
begin
    if Assigned(FOnDocData) then
        FOnDocData(Self, @Buffer, Count);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
