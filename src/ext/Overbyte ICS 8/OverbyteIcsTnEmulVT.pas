{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Program:      TNEMULVT.PAS
Description:  Delphi component combining both TnCnx and EmulVT components.
              Hence it does ANSI emulation using TCP/IP telnet protocol.
Author:       François PIETTE
Creation:     May, 1996
Version:      8.02
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2014 by François PIETTE
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
Jul 22, 1997 Revised Connect method to be callable from FormCreate event
             Adapted to Delphi 3
Sep 05, 1997 TnCnx made public, Minor change to method visibility
             Added OnTermType and OnDataAvailable events.
Sep 23, 1997 V202. Added local echo support (incomplete because we should ask
             the remote host not to echo characters. Will implement later)
             Added TnEmultVTVersion
Sep 24, 1997 V2.03 Complete local echo support.
Sep 25, 1997 V2.04 Port to C++Builder
Feb 19, 1998 V2.05 Replaced private section by protected.
             Added TriggerDataAvailable virtual function.
Dec 21, 1998 V2.06 Added fixes from Steve Endicott.
Mar 01, 1999 V2.07 Added conditional compile for BCB4. Thanks to James
             Legg <jlegg@iname.com>.
Mar 14, 1999 V2.08 Added OnKeyDown event to allow key trapping.
             Ignore any exception when sending fct keys.
Aug 15, 1999 V2.09 Move KeyPress procedure to public section for BCB4 compat.
Aug 20, 1999 V2.10 Added compile time options. Revised for BCB4.
Sep 25, 1999 V2.11 Corrected GetSelTextBuf so that lines are returned in
             corrected order (they where returned in reverse order).
             Thanks to Laurent Navarro <r2363c@email.sps.mot.com> for finding
             this bug and fixing it.
Jan 12, 2002 V2.12 Replaced TnCnx public member by TnConn property to avoid
             conflit with unit named TnCnx. This will require change in your
             own code if you used this property. Added a notification procedure
             to remove FTnCnx if component is externally destroyed. Added code
             to check if we still have the component with us.
Oct 23, 2002 V2.13 Changed Buffer arg in OnDataAvailable to Pointer instead
             of PChar to avoid Delphi 7 messing everything with AnsiChar.
May 31, 2004 V2.14 Used ICSDEFS.INC, removed unused unit
Mar 26, 2006 V2.15 Fixed TnCnxSessionConnected where global Error variable was
             used instead of local argument.
             Renamed Erc argument to ErrorCode.
Mar 26, 2005 V6.00 New version 6 started from V5
Mar 24, 2008 V6.01 Francois Piette made some changes to prepare code
             for Unicode.
Aug 15, 2008 V7.00 Delphi 2009 (Unicode) support. The terminal is not
             unicode, but the component support unicode strings.
Jul 17, 2011 V7.01 Arno fixed some bugs with non-Windows-1252 code pages.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Apr 11, 2013  V8.01 Angus added SocketFamily, LocalAddr and LocalAddr6 for IPv6
                    Added default font if host not saved yet
                    Free Options form each time it's opened
May 28, 2014  v8.02 DrJohn fixed problem with (border) colours
                    AutoResize property added with improved font resizing
                    SoundOn property added
                    GetScreenText function added
                    Fixed bug during connect if control not visible



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsTnEmulVT;

{$R-}           { Range Check off                     }
{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
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
{$H+}         { Use long strings                    }
{$J+}         { Allow typed constant to be modified }
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
{$IFDEF USEWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
{$IFDEF COMPILER20_UP}
    {$IFDEF RTL_NAMESPACES}System.UITypes{$ELSE}UITypes{$ENDIF},
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.Graphics{$ELSE}Graphics{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.Controls{$ELSE}Controls{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.IniFiles{$ELSE}IniFiles{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.Forms{$ELSE}Forms{$ENDIF},
    OverbyteIcsEmulVT,   OverbyteIcsTnCnx,
    OverbyteIcsTnOptFrm, OverbyteIcsWSocket;

const
  TnEmultVTVersion   = 802;
  CopyRight : String = ' TTnEmulVT (c) 1996-2014 F. Piette V8.02 ';

type
  TTnEmulVTDataAvailable = procedure (Sender  : TObject;
                                      Buffer  : Pointer;
                                      var Len : Integer) of object;
  TTnEmulVT = class(TEmulVT)
  protected
    FTnCnx              : TTnCnx;
    FError              : Word;
    FIniFilename        : String;
    FSectionName        : String;
    FKeyName            : String;
    FHostName           : String;
    FPort               : String;
    FTag                : LongInt;
    FUpperLock          : Boolean;
    FLocalEcho          : Boolean;
    FOnSessionClosed    : TNotifyEvent;
    FOnSessionConnected : TNotifyEvent;
    FOnNamesClick       : TNotifyEvent;
    FOnSendLoc          : TNotifyEvent;
    FOnTermType         : TNotifyEvent;
    FOnLocalEcho        : TNotifyEvent;
    FOnDataAvailable    : TTnEmulVTDataAvailable;
    FMouseDown          : Boolean;
    FMouseCaptured      : Boolean;
    FMouseTop           : Integer;
    FMouseLeft          : Integer;
    FFocusDrawn         : Boolean;
    FFocusRect          : TRect;
    FSocketFamily       : TSocketFamily;  { V8.01 }
    FLocalAddr          : String;         { V8.01 }
    FLocalAddr6         : String;         { V8.01 }
    procedure   TriggerDataAvailable(Buffer : Pointer; Len: Integer); virtual;
    procedure   TnCnxDataAvailable(Sender: TTnCnx; Buffer : Pointer; Len : Integer);
    procedure   TnCnxSessionClosed(Sender: TTnCnx; ErrorCode: Word);
    procedure   TnCnxSessionConnected(Sender: TTnCnx; ErrorCode: Word);
    procedure   TnCnxSendLoc(Sender: TObject);
    procedure   TnCnxTermType(Sender: TObject);
    procedure   TnCnxLocalEcho(Sender: TObject);
    procedure   Display(Msg : String);
    procedure   DoKeyBuffer(Buffer : PChar; Len : Integer); override;
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState;
                          X, Y: Integer); override;
    procedure   MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
{$IFNDEF BCB_FLAG}
    procedure   Notification(AComponent : TComponent;
                             Operation  : TOperation); override;
{$ENDIF}
    procedure   SetOnEndOfRecord(Value : TNotifyEvent);
    function    GetOnEndOfRecord : TNotifyEvent;
    procedure   SetLocation(Value : String);
    function    GetLocation : String;
    procedure   SetHostName(newValue : String);
  public
    procedure   RequestLocalEcho(newValue : Boolean);
    function    GetLocalEcho : Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Connect;
    procedure   Disconnect;
    procedure   EditOptions;
    procedure   RestoreOptions;
    function    IsConnected : Boolean;
    function    Send(Data : Pointer; Len : Integer) : Integer;
    function    SendStr(Data : String) : Integer;
    function    GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer;
    function    GetScreenText(Buffer: PChar; BufSize: Integer): Integer;  {drjohn}
    procedure   KeyPress(var Key: Char); override;
    property    TnConn     : TTnCnx  read FTnCnx;
  published
    property IniFilename   : String  read FIniFileName write FIniFileName;
    property SectionName   : String  read FSectionName write FSectionName;
    property KeyName       : String  read FKeyName     write FKeyName;
    property Error         : Word    read FError       write FError;
    property HostName      : String  read FHostName    write SetHostName;
    property Port          : String  read FPort        write FPort;
    property Tag           : LongInt read FTag         write FTag;
    property Location      : String  read GetLocation  write SetLocation;
    property UpperLock     : Boolean read FUpperLock   write FUpperLock;
    property LocalEcho     : Boolean read FLocalEcho   write FLocalEcho;
    property SocketFamily  : TSocketFamily read FSocketFamily  write FSocketFamily; { V8.01 }
    property LocalAddr     : String  read FLocalAddr   write FLocalAddr;            { V8.01 }
    property LocalAddr6    : String  read FLocalAddr6  write FLocalAddr6;           { V8.01 }
    property OnKeyDown;
    property OnSessionClosed    : TNotifyEvent         read  FOnSessionClosed
                                                       write FOnSessionClosed;
    property OnSessionConnected : TNotifyEvent         read  FOnSessionConnected
                                                       write FOnSessionConnected;
    property OnEndOfRecord : TNotifyEvent              read  GetOnEndOfRecord
                                                       write SetOnEndOfRecord;
    property OnNamesClick : TNotifyEvent               read  FOnNamesClick
                                                       write FOnNamesClick;
    property OnSendLoc :    TNotifyEvent               read  FOnSendLoc
                                                       write FOnSendLoc;
    property OnTermType :    TNotifyEvent              read  FOnTermType
                                                       write FOnTermType;
    property OnLocalEcho :    TNotifyEvent             read  FOnLocalEcho
                                                       write FOnLocalEcho;
    property OnDataAvailable : TTnEmulVTDataAvailable  read  FOnDataAvailable
                                                       write FOnDataAvailable;
  end;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure FontToIni(Font : TFont; IniFile : TIniFile; Section : String);
var
    nBuf     : Integer;
begin
    IniFile.WriteString(Section,  'FontName',  Font.Name);
    IniFile.WriteInteger(Section, 'FontSize',  Font.Size);
    IniFile.WriteInteger(Section, 'FontPitch', ord(Font.Pitch));
    nBuf := 0;
    if fsBold in Font.Style then
        nBuf := nBuf or 1;
    if fsItalic in Font.Style then
        nBuf := nBuf or 2;
    if fsUnderline in Font.Style then
        nBuf := nBuf or 4;
    if fsStrikeOut in Font.Style then
        nBuf := nBuf or 8;
    IniFile.WriteInteger(Section, 'FontStyle', nBuf);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IniToFont(Font : TFont; IniFile : TIniFile; Section : String);
var
    FontName : String;
    nBuf     : Integer;
begin
    FontName := IniFile.ReadString(Section, 'FontName', '*');
    if FontName <> '*' then begin
        Font.Name  := FontName;
        Font.Size  := IniFile.ReadInteger(Section, 'FontSize', 9);
        Font.Pitch := TFontPitch(IniFile.ReadInteger(Section, 'FontPitch', 0));
        nBuf       := IniFile.ReadInteger(Section, 'FontStyle', 0);
        Font.Style := [];
        if (nBuf and 1) <> 0 then
            Font.Style := Font.Style + [fsBold];
        if (nBuf and 2) <> 0 then
            Font.Style := Font.Style + [fsItalic];
        if (nBuf and 4) <> 0 then
            Font.Style := Font.Style + [fsUnderline];
        if (nBuf and 8) <> 0 then
            Font.Style := Font.Style + [fsStrikeOut];
    end
    else   { V8.01 added default font if host not saved yet }
    begin
        Font.Name := 'Courier New';
        Font.Size := 8 ;
        Font.Pitch := fpDefault ;
        Font.Style := [];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TTnEmulVT.Create(AOwner: TComponent);
var
    Rect : TRect;
begin
    inherited Create(AOwner);

    FTnCnx                    := TTnCnx.Create(Self);
    FTnCnx.OnDataAvailable    := TnCnxDataAvailable;
    FTnCnx.OnSessionClosed    := TnCnxSessionClosed;
    FTnCnx.OnSessionConnected := TnCnxSessionConnected;
    FTnCnx.OnSendLoc          := TnCnxSendLoc;
    FTnCnx.OnTermType         := TnCnxTermType;
    FTnCnx.OnLocalEcho        := TnCnxLocalEcho;

    FIniFileName := 'TNEMULVT.INI';
    FSectionName := 'Windows';
    FKeyName     := 'TnEmulVT';
    FPort        := 'telnet';
    Rect.Top     := -1;
    SelectRect   := Rect;
    FSocketFamily := DefaultSocketFamily;   { V8.01 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor  TTnEmulVT.Destroy;
begin
    if Assigned(FTnCnx) then begin
        FTnCnx.Free;
        FTnCnx := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF BCB_FLAG}
procedure TTnEmulVT.Notification(
    AComponent : TComponent;
    Operation  : TOperation);
begin
    inherited Notification(AComponent, operation);
    if Operation = opRemove then begin
        if AComponent = FTnCnx then
            FTnCnx:= nil;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.SetHostName(newValue : String);
begin
    FHostName := newValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnEmulVT.GetLocalEcho : Boolean;
begin
    if Assigned(FTnCnx) then
        Result := FTnCnx.LocalEcho
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.RequestLocalEcho(newValue : Boolean);
begin
    if Assigned(FTnCnx) then begin
        if newValue then
            FTnCnx.DontOption(AnsiChar(TN_ECHO))
        else
            FTnCnx.DoOption(AnsiChar(TN_ECHO));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.SetLocation(Value : String);
begin
    if Assigned(FTnCnx) then
        FTnCnx.Location := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnEmulVT.GetLocation : String;
begin
    if Assigned(FTnCnx) then
        Result := FTnCnx.Location;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.Display(Msg : String);
begin
    WriteStr(Msg);
    Repaint;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.SetOnEndOfRecord(Value : TNotifyEvent);
begin
    if Assigned(FTnCnx) then
        FTnCnx.OnEndOfRecord := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnEmulVT.GetOnEndOfRecord : TNotifyEvent;
begin
    if Assigned(FTnCnx) then
        Result := FTnCnx.OnEndOfRecord;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TnCnxSendLoc(Sender: TObject);
begin
    if Assigned(FOnSendLoc) then
        FOnSendLoc(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TnCnxTermType(Sender: TObject);
begin
    if Assigned(FOnTermType) then
        FOnTermType(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TnCnxLocalEcho(Sender: TObject);
begin
    if Assigned(FOnLocalEcho) then
        FOnLocalEcho(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TriggerDataAvailable(Buffer : Pointer; Len: Integer);
begin
    if Assigned(FOnDataAvailable) then
        FOnDataAvailable(Self, Buffer, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TnCnxDataAvailable(
    Sender : TTnCnx;
    Buffer : Pointer;
    Len    : Integer);
var
    I : Integer;
begin
    TriggerDataAvailable(Buffer, Len);

    if Len <= 0 then
        Exit;

    for I := 0 to Len - 1 do begin
        try
            WriteChar((PAnsiChar(Buffer) + I)^);
        except
            Break;
        end;
    end;
    UpdateScreen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TnCnxSessionClosed(Sender: TTnCnx; ErrorCode: Word);
begin
    Display(#13 + #10 + '*** Server has closed ***' + #13 + #10);
    if SoundOn then                                                  {drjohn}
      MessageBeep(MB_ICONASTERISK);
    FError := ErrorCode;
    if Assigned(FOnSessionClosed) then
        FOnSessionClosed(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TnCnxSessionConnected(Sender: TTnCnx; ErrorCode: Word);
begin
    if ErrorCode = 0 then begin
        Display('Connected' + #13 + #10);
    end
    else begin
        Display('Connection failed: ' +
                WSocketErrorDesc(ErrorCode) +
                #13 + #10);
        if SoundOn then                                               {drjohn}
          MessageBeep(MB_ICONASTERISK);
    end;
    FError := ErrorCode;
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.RestoreOptions;
var
    IniFile  : TIniFile;
    xFont    : TNFont;                                           {drjohn}
begin
    if Length(FHostname) <= 0 then
        Exit;

    IniFile  := TIniFile.Create(IniFilename);
    xFont := TNFont.Create;                                      {drjohn}
    IniToFont(xFont, IniFile, HostName);
    Font := xFont;
    xFont.Free;

    LineHeight  := IniFile.ReadInteger(HostName, 'LineHeight',  12);
    Rows        := IniFile.ReadInteger(HostName, 'Rows',        25);
    Cols        := IniFile.ReadInteger(HostName, 'Cols',        80);
    FKeys       := IniFile.ReadInteger(HostName, 'FKeys',       0);
    AutoCr      := IniFile.ReadInteger(HostName, 'AutoCR',      0) <> 0;
    AutoLF      := IniFile.ReadInteger(HostName, 'AutoLF',      0) <> 0;
    LocalEcho   := IniFile.ReadInteger(HostName, 'LocalEcho',   0) <> 0;
    MonoChrome  := IniFile.ReadInteger(HostName, 'MonoChrome',  0) <> 0;
    SoundOn     := IniFile.ReadInteger(HostName, 'SoundOn',     1) <> 0;              {drjohn}
    AutoResize  := IniFile.ReadInteger(HostName, 'AutoResize',  1) <> 0;              {drjohn}
    UpperLock   := IniFile.ReadInteger(HostName, 'UpperLock',   0) <> 0;
    Xlat        := IniFile.ReadInteger(HostName, 'Xlat',        0) <> 0;
    GraphicDraw := IniFile.ReadInteger(HostName, 'GraphicDraw', 0) <> 0;
    CharZoom    := StrToFloat(IniFile.ReadString(HostName, 'CharZoom', '1'));
    LineZoom    := StrToFloat(IniFile.ReadString(HostName, 'LineZoom', '1'));
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.Connect;
var
    Form : TCustomForm;
begin
    if Length(FHostname) <= 0 then
        Exit;

    Clear;
    if not Assigned(FTnCnx) then
        Exit;
    FTnCnx.Port  := FPort;
    FTnCnx.Host  := FHostName;
    FTnCnx.LocalAddr := FLocalAddr;       { V8.01 }
    FTnCnx.LocalAddr6 := FLocalAddr6;     { V8.01 }
    FTnCnx.SocketFamily := FSocketFamily; { V8.01 }
    FTnCnx.Connect;
    Display('Connecting to ''' + HostName + '''' + #13 + #10);
    if Self.Showing then begin         {drjohn}
        Form := GetParentForm(Self);
        Form.ActiveControl := Self;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.Disconnect;
begin
    if Assigned(FTnCnx) then
        FTnCnx.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnEmulVT.Send(Data : Pointer; Len : Integer) : Integer;
begin
    if Assigned(FTnCnx) then
        Result := FTnCnx.Send(Data, Len)
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnEmulVT.SendStr(Data : String) : Integer;
begin
    if Assigned(FTnCnx) then
        Result := FTnCnx.SendStr(Data)
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnEmulVT.IsConnected : Boolean;
begin
    if Assigned(FTnCnx) then
        Result := FTnCnx.IsConnected
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.EditOptions;
var
    IniFile : TIniFile;
begin
    if Length(FHostname) <= 0 then
        Exit;

//    if OptForm = nil then   // Angus Apr 2013 create and free form
    OptForm := TOptForm.Create(Self);
    try
        OptForm.IniFileName  := FIniFileName;
        OptForm.OnNamesClick := FOnNamesClick;

        RestoreOptions;
        OptForm.AFont.Assign(Font);
        OptForm.LineHeight  := LineHeight;
        OptForm.AutoCR      := AutoCr;
        OptForm.AutoLF      := AutoLF;
        OptForm.LocalEcho   := LocalEcho;
        OptForm.MonoChrome  := MonoChrome;
        OptForm.UpperLock   := UpperLock;
        OptForm.Xlat        := Xlat;
        OptForm.GraphicDraw := GraphicDraw;
        OptForm.Rows        := Rows;
        OptForm.Cols        := Cols;
        OptForm.HostName    := FHostName;
        OptForm.FKeys       := FKeys;
        OptForm.CharZoom    := CharZoom;
        OptForm.LineZoom    := LineZoom;
        if OptForm.ShowModal = IDOK then begin
            Font        := OptForm.AFont;
            LineHeight  := OptForm.LineHeight;
            AutoCR      := OptForm.AutoCr;
            AutoLF      := OptForm.AutoLF;
            LocalEcho   := OptForm.LocalEcho;
            MonoChrome  := OptForm.MonoChrome;
            UpperLock   := OptForm.UpperLock;
            Xlat        := OptForm.Xlat;
            GraphicDraw := OptForm.GraphicDraw;
            Rows        := OptForm.Rows;
            Cols        := OptForm.Cols;
            FKeys       := OptForm.FKeys;
            LineZoom    := OptForm.LineZoom;
            CharZoom    := OptForm.CharZoom;
            IniFile     := TIniFile.Create(FIniFilename);
            FontToIni(OptForm.AFont, IniFile, FHostName);
            IniFile.WriteInteger(FHostName, 'LineHeight',  Trunc(LineHeight));            {drjohn}
            IniFile.WriteInteger(FHostName, 'Rows',        Rows);
            IniFile.WriteInteger(FHostName, 'Cols',        Cols);
            IniFile.WriteInteger(FHostName, 'AutoCR',      ord(AutoCR));
            IniFile.WriteInteger(FHostName, 'AutoLF',      ord(AutoLF));
            IniFile.WriteInteger(FHostName, 'LocalEcho',   ord(LocalEcho));
            IniFile.WriteInteger(FHostName, 'MonoChrome',  ord(MonoChrome));
            IniFile.WriteInteger(HostName,  'SoundOn',     ord(SoundOn));                 {drjohn}
            IniFile.WriteInteger(HostName,  'AutoResize',  ord(AutoResize));              {drjohn}
            IniFile.WriteInteger(FHostName, 'UpperLock',   ord(UpperLock));
            IniFile.WriteInteger(FHostName, 'Xlat',        ord(Xlat));
            IniFile.WriteInteger(FHostName, 'GraphicDraw', ord(GraphicDraw));
            IniFile.WriteInteger(FHostName, 'FKeys',       FKeys);
            IniFile.WriteString(FHostName, 'LineZoom',     Format('%5.3f', [LineZoom]));
            IniFile.WriteString(FHostName, 'CharZoom',     Format('%5.3f', [CharZoom]));
            IniFile.Free;
            Repaint;
        end;
    finally
        OptForm.Free ;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.KeyPress(var Key: Char);
begin
    inherited KeyPress(Key);
{$IFDEF SizeOf(Char) <> 1}
    if Ord(Key) > 255 then
        raise Exception.Create('TTnEmulVT.KeyPress detected a non-ansi char');
{$ENDIF}
    if FUpperLock and (Key >= 'a') and (Key <= 'z') then
        Key := chr(ord(Key) and $DF);
    if Key <> #0 then begin
        try
            if FLocalEcho then
                WriteChar(AnsiChar(Key));
            if Assigned(FTnCnx) then
                FTnCnx.Send(@Key, 1);
        except
            { Ignore any exception ! }
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.DoKeyBuffer(Buffer : PChar; Len : Integer);
begin
    try
        if FLocalEcho then
            WriteBuffer(Buffer, Len);
        if Assigned(FTnCnx) then
            FTnCnx.Send(Buffer, Len);
    except
        { Ignore exception ! }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure DrawFocusRectangle(Wnd: HWnd; Rect: TRect);
var
    DC : HDC;
begin
    DC := GetDC(Wnd);
    DrawFocusRect(DC, Rect);
    ReleaseDC(Wnd, DC);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
    inherited MouseDown(Button, Shift, X, Y);
    FMouseDown := TRUE;
    if FFocusDrawn then begin
        DrawFocusRectangle(Handle, FFocusRect);
        FFocusDrawn := FALSE;
    end;
    if SelectRect.Top <> -1 then begin
        FFocusRect.Top := -1;
        SelectRect := FFocusRect;
        Repaint;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.MouseMove(Shift: TShiftState; X, Y: Integer);
var
    Rect  : TRect;
    Point : TPoint;
begin
    inherited MouseMove(Shift, X, Y);
    if not FMouseDown then
        Exit;

    if not FMouseCaptured then begin
        SetCapture(Handle);
        FMouseCaptured    := TRUE;
        FMouseTop         := SnapPixelToRow(Y);
        FMouseLeft        := SnapPixelToCol(X);

        Point.X          := 0;
        Point.Y          := 0;
        Rect.TopLeft     := ClientToScreen(Point);
        Point.X          := Width - 16;
        Point.Y          := Height;
        Rect.BottomRight := ClientToScreen(Point);
        ClipCursor(@Rect);
    end
    else if (FMouseTop <> Y) or (FMouseLeft <> X) then begin
        if FFocusDrawn then
            DrawFocusRectangle(Handle, FFocusRect);
        Rect.Top    := FMouseTop;
        Rect.Left   := FMouseLeft;
            Rect.Bottom := SnapPixelToRow(Y) + Trunc(LineHeight) + 4;             {drjohn}
            Rect.Right  := SnapPixelToCol(X) + Trunc(CharWidth);              {drjohn}
        DrawFocusRectangle(Handle, Rect);
        FFocusRect   := Rect;
        FFocusDrawn  := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
    inherited MouseUp(Button, Shift, X, Y);
    FMouseDown := FALSE;
    if FMouseCaptured then begin
        ReleaseCapture;
        FMouseCaptured := FALSE;
        ClipCursor(nil);
    end;
    if FFocusDrawn then begin
        DrawFocusRectangle(Handle, FFocusRect);
        FFocusDrawn := FALSE;
        if (FFocusRect.Right - FFocusRect.Left) < CharWidth then
            FFocusRect.Top := -1;
        if (FFocusRect.Bottom - FFocusRect.Top) < LineHeight then
            FFocusRect.Top := -1;
        SelectRect  := FFocusRect;
        Repaint;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
function TTnEmulVT.GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer;
var
    StartRow : Integer;
    StopRow  : Integer;
    StartCol : Integer;
    StopCol  : Integer;
    nRow     : Integer;
    nCol     : Integer;
    Line     : TLine;
    nCnt     : Integer;
begin
    nCnt := 0;
    if (SelectRect.Top = -1) or (BufSize < 1) then begin
        if BufSize > 0 then
            Buffer[0] := #0;
        Result := nCnt;
        Exit;
    end;

    StartRow := PixelToRow(SelectRect.Top);
    StopRow  := PixelToRow(SelectRect.Bottom) - 1;
    StartCol := PixelToCol(SelectRect.Left);
    StopCol  := PixelToCol(SelectRect.Right) - 1;

    for nRow := StartRow to StopRow do begin
        if BufSize < 2 then
            Break;
        Line := Screen.FLines^[Rows - 1 - nRow];
        for nCol := StartCol to StopCol do begin
{$IF SizeOf(Line.Txt[0]) <> 1}
            Buffer[0] := Line.Txt[nCol];
{$ELSE}
            Buffer[0] := Char(Line.Txt[nCol]);
{$IFEND}
            Inc(Buffer);
            Dec(BufSize);
            Inc(nCnt);
            if BufSize < 2 then
                Break;
        end;
        if nRow < StopRow then begin
            if BufSize < 4 then
                Break;
            Buffer[0] := #13;
            Buffer[1] := #10;
            Inc(Buffer);
            Inc(Buffer);
            Dec(BufSize);
            Dec(BufSize);
            Inc(nCnt);
            Inc(nCnt);
        end;
    end;

    Buffer[0] := #0;
    Result := nCnt;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
function TTnEmulVT.GetScreenText(Buffer: PChar; BufSize: Integer ): Integer;              {drjohn}
var
    StartRow : Integer;
    StopRow  : Integer;
    StartCol : Integer;
    StopCol  : Integer;
    nRow     : Integer;
    nCol     : Integer;
    Line     : TLine;
    nCnt     : Integer;
  redraw : boolean;
begin
    nCnt := 0;
    if (BufSize < 1) then begin
        if BufSize > 0 then
            Buffer[0] := #0;
        Result := nCnt;
        Exit;
    end;

  StartRow := 0;
  StopRow := 24;
  StartCol := 0;
  StopCol := 79;
  redraw := FALSE;

    for nRow := StartRow to StopRow do begin
        if BufSize < 2 then
            Break;
        Line := Screen.FLines^[Rows - 1 - nRow];
        for nCol := StartCol to StopCol do
        begin
{$IF SizeOf(Line.Txt[0]) <> 1}
       if Line.Txt[nCol] = AnsiChar(#0) then
       begin
          Buffer[0] := ' ';
          Line.Txt[nCol] := ' ';
          redraw := TRUE;
       end
       else
          Buffer[0] := Line.Txt[nCol];
{$ELSE}
       if Line.Txt[nCol] = Char(#0) then
       begin
          Buffer[0] := ' ';
          Line.Txt[nCol] := ' ';
          redraw := TRUE;
       end
       else
          Buffer[0] := Char(Line.Txt[nCol]);
{$IFEND}
            Inc(Buffer);
            Dec(BufSize);
            Inc(nCnt);
            if BufSize < 2 then
                Break;
        end;
        if nRow < StopRow then begin
            if BufSize < 4 then
                Break;
            Buffer[0] := #13;
            Buffer[1] := #10;
            Inc(Buffer);
            Inc(Buffer);
            Dec(BufSize);
            Dec(BufSize);
            Inc(nCnt);
            Inc(nCnt);
        end;
    end;

  if redraw  then
     RePaint;

    Buffer[0] := #0;
    Result := nCnt;
end;


end.

