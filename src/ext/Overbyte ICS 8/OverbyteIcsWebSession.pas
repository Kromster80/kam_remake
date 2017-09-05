{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Classes to handle session for THttpAppSrv and MidWare.
Creation:     Dec 20, 2003
Version:      8.01
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list midware@elists.org or twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1998-2010 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software and or any
              derived or altered versions for any purpose, excluding commercial
              applications. You can use this software for personal use only.
              You may distribute it freely untouched.
              The following restrictions applies:

              1. The origin of this software must not be misrepresented, you
                 must not claim that you wrote the original software.

              2. If you use this software in a product, an acknowledgment in
                 the product documentation and displayed on screen is required.
                 The text must be: "This product is based on MidWare. Freeware
                 source code is available at http://www.overbyte.be."

              3. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              4. This notice may not be removed or altered from any source
                 distribution and must be added to the product documentation.

Updates:
Apr 19, 2010 V1.01 Angus, stop MaxAge (SessionTimeout) being restored with saved
                      session data since it can never then be changed
                   Added SessionDataCount so client can use it, only set by AssignName
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Aug 17, 2012 V8.01 Angus, added MaxSessions to allow more than 100 web sessions


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *_*}
unit OverbyteIcsWebSession;

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
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

interface

uses
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SyncObjs{$ELSE}SyncObjs{$ENDIF},
    OverbyteIcsTimeList,
    OverbyteIcsUtils;

type
    TWebSessions = class;

    TWebSessionData = class(TComponent)
    private
        FSessionDataCount: integer;  // V1.01 Angus 17 Apr 2010 keep counter so client can use it
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        procedure   AssignName; virtual;
    published
        property SessionDataCount: integer read FSessionDataCount write FSessionDataCount;
    end;

    PWebSession = ^TWebSession;
    TWebSession = class(TComponent)
    protected
        FVarName     : array of String;
        FVarValue    : array of String;
        FTimeRec     : PTimeRec;
        FTimeMark    : TDateTime;
        FSession     : Integer;
        FRefCount    : Integer;
        FCritSec     : TCriticalSection;
        FSessionData : TWebSessionData;
        FSessions    : TWebSessions;
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
        function  GetVarNames(nIndex: Integer): String;
        function  GetVarValues(nIndex: Integer): String;
        function  GetCount: Integer;
        function  GetTimeMark: TDateTime;
        procedure SetTimeMark(const Value: TDateTime);
        function  GetValues(VName: String): String;
        procedure SetValues(VName: String; const Value: String);
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        procedure Refresh;
        function  SetValue(const VarName   : String;
                           const VarValue  : String) : Integer;
        function  GetValue(const VarName   : String;
                           var   VarValue  : String) : Integer;
        function  DecRefCount : Integer;
        function  IncRefCount : Integer;
        function  GetRefCount : Integer;
        procedure DefineProperties(Filer : TFiler); override;
        procedure ReadSessionDataProperty(Reader: TReader);
        procedure WriteSessionDataProperty(Writer: TWriter);
        procedure ReadVarNamesValuesProperty(Reader : TReader);
        procedure WriteVarNamesValuesProperty(Writer : TWriter);
        property  RefCount : Integer                    read  GetRefCount;
        property  Count : Integer                       read  GetCount;
        property  Values[VName : String] : String       read  GetValues
                                                        write SetValues;
        property  VarNames[nIndex : Integer] : String   read  GetVarNames;
        property  VarValues[nIndex : Integer] : String  read  GetVarValues;
        property  SessionData : TWebSessionData         read   FSessionData
                                                        write  FSessionData;
        property  Sessions    : TWebSessions            read   FSessions
                                                        write  FSessions;
        property  TimeRec  : PTimeRec                   read   FTimeRec;
    published
        property  TimeMark : TDateTime                  read   GetTimeMark
                                                        write  SetTimeMark;
        property  Session  : Integer                    read  FSession
                                                        write FSession;
    end;

    TDeleteSessionEvent = procedure (Sender  : TObject;
                                     Session : TWebSession) of Object;

    TWebSessions = class(TComponent)
    protected
        FTimeList        : TTimeList;
        FCritSec         : TCriticalSection;
        FOnCreateSession : TDeleteSessionEvent;
        FOnDeleteSession : TDeleteSessionEvent;
        procedure TimeListDeleteHandler(Sender: TObject; PItem: PTimeRec);
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        function    AddSession(const Value : String) : TWebSession; virtual;
        function    CreateSession(const Param     : String;
                                  var   SessionID : String) : TWebSession; virtual;
        function    CreateSessionEx(const Param     : String;
                                    var   SessionID : String;
                                    SessionData     : TWebSessionData) : TWebSession; virtual;
        procedure   ValidateSession(const SessionID : String;
                                    SessionRef : PWebSession); virtual;
        procedure   ReleaseSession(SessionRef : PWebSession); virtual;
        procedure   RemoveAged; virtual;
        function    FindSession(const Value : String): TWebSession;
        function    FindSessionEx(const Value : String;
                                  Refresh     : Boolean): TWebSession;
        function    FindSessionValue(nIndex : Integer):  String;
        function    DeleteSession(const Value : String): Boolean;
        function    RefreshSession(const Value : String): TWebSession;
        function    GetMaxAge: Integer;
        procedure   SetMaxAge(const Value: Integer);
        function    GetMaxSessions: Integer;               { V8.02 }
        procedure   SetMaxSessions(const Value: Integer);  { V8.02 }
        function    GetCount: Integer;
        function    GetSessions(nIndex: Integer): TWebSession;
        procedure   Clear;
        procedure   Lock;
        procedure   Unlock;
        procedure   SaveToFile(const FileName: String);
        procedure   SaveToStream(Dest: TStream);
        procedure   LoadFromFile(const FileName: String);
        procedure   LoadFromStream(Src: TStream);
        procedure   DefineProperties(Filer : TFiler); override;
        procedure   ReadSessionsProperty(Reader : TReader);
        procedure   WriteSessionsProperty(Writer : TWriter);
        property    Count : Integer              read  GetCount;
        property    Sessions[nIndex : Integer] : TWebSession read GetSessions;
    published
        property MaxAge   : Integer              read  GetMaxAge    // Seconds
                                                 write SetMaxAge;
        property MaxSessions : Integer           read   GetMaxSessions  { V8.02 }
                                                 write  SetMaxSessions;
        property OnDeleteSession : TDeleteSessionEvent read  FOnDeleteSession
                                                       write FOnDeleteSession;
        property OnCreateSession : TDeleteSessionEvent read  FOnCreateSession
                                                       write FOnCreateSession;
    end;

procedure GWebSessionsCreate(MaxAge : Integer);    // Seconds
procedure GWebSessionsDestroy;
var
    GWebSessions : TWebSessions;

implementation

{$I Include\Ics.InterlockedApi.inc}

var
    GSessionID        : Integer    = 0;
    GSessionDataCount : Integer    = 0;
    GSignature        : AnsiString = 'WebSessions V1.01' + #13#10#26;
threadvar
    LockCount  : Integer;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure GWebSessionsCreate(MaxAge : Integer);
begin
    if not Assigned(GWebSessions) then
        GWebSessions := TWebSessions.Create(nil);
    GWebSessions.MaxAge := MaxAge;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure GWebSessionsDestroy;
begin
    if Assigned(GWebSessions) then
        FreeAndNil(GWebSessions);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Base64Encode(Input : String) : String;
var
    Final : String;
    Count : Integer;
    Len   : Integer;
const
    Base64Out: array [0..64] of Char =
    ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
     'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/', '=');
begin
    Final := '';
    Count := 1;
    Len   := Length(Input);
    while Count <= Len do begin
        Final := Final + Base64Out[(Byte(Input[Count]) and $FC) shr 2];
        if (Count + 1) <= Len then begin
            Final := Final + Base64Out[((Byte(Input[Count]) and $03) shl 4) +
                                       ((Byte(Input[Count+1]) and $F0) shr 4)];
            if (Count+2) <= Len then begin
                Final := Final + Base64Out[((Byte(Input[Count+1]) and $0F) shl 2) +
                                           ((Byte(Input[Count+2]) and $C0) shr 6)];
                Final := Final + Base64Out[(Byte(Input[Count+2]) and $3F)];
            end
            else begin
                Final := Final + Base64Out[(Byte(Input[Count+1]) and $0F) shl 2];
                Final := Final + '=';
            end
        end
        else begin
            Final := Final + Base64Out[(Byte(Input[Count]) and $03) shl 4];
            Final := Final + '==';
        end;
        Count := Count + 3;
    end;
    Result := Final;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Base64Decode(Input : String) : String;
var
    Final   : String;
    Count   : Integer;
    Len     : Integer;
    DataIn0 : Byte;
    DataIn1 : Byte;
    DataIn2 : Byte;
    DataIn3 : Byte;
const
    Base64In: array[0..127] of Byte =
    (255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     255, 255, 255, 255,  62, 255, 255, 255,  63,  52,  53,  54,  55,
      56,  57,  58,  59,  60,  61, 255, 255, 255,  64, 255, 255, 255,
       0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,
      13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
     255, 255, 255, 255, 255, 255,  26,  27,  28,  29,  30,  31,  32,
      33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,
      46,  47,  48,  49,  50,  51, 255, 255, 255, 255, 255);
begin
    Final := '';
    Count := 1;
    Len := Length(Input);
    while Count <= Len do begin
        DataIn0 := Base64In[Byte(Input[Count])];
        DataIn1 := Base64In[Byte(Input[Count+1])];
        DataIn2 := Base64In[Byte(Input[Count+2])];
        DataIn3 := Base64In[Byte(Input[Count+3])];

        Final := Final + Char(((DataIn0 and $3F) shl 2) +
                              ((DataIn1 and $30) shr 4));
        if DataIn2 <> $40 then begin
            Final := Final + Char(((DataIn1 and $0F) shl 4) +
                                  ((DataIn2 and $3C) shr 2));
            if DataIn3 <> $40 then
                Final := Final + Char(((DataIn2 and $03) shl 6) +
                                      (DataIn3 and $3F));
        end;
        Count := Count + 4;
    end;
    Result := Final;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{                                                                           }
{                                  TWebSession                              }
{                                                                           }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TWebSession.Create(AOwner: TComponent);
begin
    inherited;
    FCritSec     := TCriticalSection.Create;
//WriteLn('TWebSession.Create');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TWebSession.Destroy;
begin
//WriteLn('TWebSession.Destroy. RefCount = ', FRefCount);
    if Assigned(FSessionData) then
        FreeAndNil(FSessionData);
    if Assigned(FCritSec) then
        FreeAndNil(FCritSec);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSession.Refresh;
begin
    FTimeRec.TimeMark := Now;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSession.GetRefCount: Integer;
begin
    FCritSec.Enter;
    try
        Result := FRefCount;
    finally
        FCritSec.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSession.DecRefCount: Integer;
begin
    FCritSec.Enter;
    try
        Dec(FRefCount);
        Result := FRefCount;
    finally
        FCritSec.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSession.IncRefCount: Integer;
begin
    FCritSec.Enter;
    try
        Inc(FRefCount);
        Result := FRefCount;
    finally
        FCritSec.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Used for property Values[]
function TWebSession.GetValues(VName: String): String;
begin
    GetValue(VName, Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Return -1 if VarName not found, VarValue set to empty string
// Return index if found, VarValue set to data value
function TWebSession.GetValue(
    const VarName   : String;
    var   VarValue  : String): Integer;
begin
    FCritSec.Enter;
    try
        Result := 0;
        while Result < Length(FVarName) do begin
            if CompareText(FVarName[Result], VarName) = 0 then begin
                VarValue := FVarValue[Result];
                Exit;
            end;
            Inc(Result);
        end;
        // not found
        Result   := -1;
        VarValue := '';
    finally
        FCritSec.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Used for property Values[]
procedure TWebSession.SetValues(VName: String; const Value: String);
begin
    SetValue(VName, Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Set or create a new VarValue. Returns the index into the arrays
function TWebSession.SetValue(const VarName, VarValue: String) : Integer;
var
    OldValue : String;
begin
    FCritSec.Enter;
    try
        Result := GetValue(VarName, OldValue);
        if Result >= 0 then begin
            // VarName already exists, replace VarValue
            FVarValue[Result] := VarValue;
        end
        else begin
            // VarName doesn't exist, add new item
            Result := Length(FVarName) + 1;
            SetLength(FVarName, Result);
            SetLength(FVarValue, Result);
            FVarName[Result - 1]  := VarName;
            FVarValue[Result - 1] := VarValue;
        end;
    finally
        FCritSec.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSession.GetCount: Integer;
begin
    FCritSec.Enter;
    try
        Result := Length(FVarName);
    finally
        FCritSec.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSession.GetVarNames(nIndex: Integer): String;
begin
    FCritSec.Enter;
    try
        Result := FVarName[nIndex];
    finally
        FCritSec.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSession.GetVarValues(nIndex: Integer): String;
begin
    FCritSec.Enter;
    try
        Result := FVarValue[nIndex];
    finally
        FCritSec.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSession.GetTimeMark: TDateTime;
begin
    FCritSec.Enter;
    try
        Result := FTimeMark;
    finally
        FCritSec.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSession.SetTimeMark(const Value: TDateTime);
begin
    FCritSec.Enter;
    try
        FTimeMark := Value;
        if Assigned(FTimeRec) then
            FTimeRec^.TimeMark := FTimeMark;
    finally
        FCritSec.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSession.DefineProperties(Filer: TFiler);
begin
    inherited;
    Filer.DefineProperty('VarNamesValues',
                         ReadVarNamesValuesProperty,
                         WriteVarNamesValuesProperty, TRUE);
    Filer.DefineProperty('SessionData',
                         ReadSessionDataProperty,
                         WriteSessionDataProperty, TRUE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSession.ReadSessionDataProperty(Reader : TReader);
begin
    if Reader.ReadBoolean then
        SessionData := Reader.ReadComponent(SessionData) as TWebSessionData;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSession.WriteSessionDataProperty(Writer : TWriter);
begin
    Writer.WriteBoolean(SessionData <> nil);
    if SessionData <> nil then
        Writer.WriteComponent(SessionData);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSession.ReadVarNamesValuesProperty(Reader: TReader);
var
    N, V : String;
begin
    SetLength(FVarName, 0);
    SetLength(FVarValue, 0);
    Reader.ReadListBegin;
    while not Reader.EndOfList do begin
        N := Reader.ReadString;
        V := Reader.ReadString;
        SetValue(N, V);
    end;
    Reader.ReadListEnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSession.WriteVarNamesValuesProperty(Writer: TWriter);
var
    I : Integer;
begin
    Writer.WriteListBegin;
    for I := 0 to Count - 1 do begin
        Writer.WriteString(FVarName[I]);
        Writer.WriteString(FVarValue[I]);
    end;
    Writer.WriteListEnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSession.Notification(
    AComponent : TComponent;
    Operation  : TOperation);
begin
//WriteLn('TWebSession.Notification op=' + IntToStr(Ord(operation)) + ' AComponent=$' + IntToHex(Integer(AComponent), 8) + ' ' + AComponent.ClassName);
    inherited Notification(AComponent, operation);
    if Operation = opRemove then begin
        if AComponent = FSessionData then
            FSessionData := nil
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{                                                                           }
{                                  TWebSessions                             }
{                                                                           }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TWebSessions.Create(AOwner: TComponent);
begin
    inherited;
    FTimeList := TTimeList.Create(Self);
    FTimeList.OnDelete := TimeListDeleteHandler;
    FCritSec  := TCriticalSection.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TWebSessions.Destroy;
begin
    Lock;
    try
        if Assigned(FTimeList) then begin
            Clear;
            FreeAndNil(FTimeList);
        end;
    finally
        Unlock;
    end;
    if Assigned(FCritSec) then
        FreeAndNil(FCritSec);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSessions.AddSession(const Value : String) : TWebSession;
begin
    Lock;
    try
        Result := nil;
        if not Assigned(FTimeList) then
            Exit;

        Result := TWebSession.Create(Self);
        InterlockedIncrement(GSessionID);
        Result.Session   := GSessionID;
        Result.Name      := 'WebSession' + IntToStr(GSessionID);
        Result.FTimeRec  := FTimeList.AddWithData(Value, Result, nil);
        Result.FTimeMark := Result.FTimeRec^.TimeMark;
        Result.FSessions := Self;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSessions.FindSession(const Value: String): TWebSession;
begin
    Result := FindSessionEx(Value, FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSessions.FindSessionEx(
    const Value : String;
    Refresh     : Boolean): TWebSession;
var
    Item : Integer;
begin
    Lock;
    try
        if not Assigned(FTimeList) then begin
            Result := nil;
            Exit;
        end;
        Item := FTimeList.IndexOf(Value);
        if Item < 0 then begin
            Result := nil;
        end
        else if FTimeList.RemoveItemIfAged(Item) then begin
            Result := nil;
        end
        else begin
            Result := TWebSession(FTimeList.Items[Item].Data);
            Result.Refresh;
            //FTimeList.Items[Item].TimeMark := Now;
        end;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Set TimeMark to Now for the given session
function TWebSessions.RefreshSession(const Value: String): TWebSession;
begin
    Result := FindSessionEx(Value, TRUE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSessions.FindSessionValue(nIndex : Integer):  String;
begin
    Lock;
    try
        if not Assigned(FTimeList) then begin
            Result := '';
            Exit;
        end;
        Result := FTimeList.Items[nIndex].Value;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSessions.DeleteSession(const Value: String) : Boolean;
var
    TheSession : TWebSession;
begin
    Result := FALSE;
    Lock;
    try
        if not Assigned(FTimeList) then
            Exit;
        TheSession := FindSession(Value);
        if not Assigned(TheSession) then begin
            Exit;
        end;
//WriteLn('DeleteSession. RefCount = ', TheSession.RefCount);
        // Delete will call TimeListDeleteHandler which will free the session
        FTimeList.Delete(Value);
//        if TheSession.DecRefCount < 0 then
//            TheSession.Destroy;
        Result := TRUE;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.TimeListDeleteHandler(
    Sender : TObject;
    PItem  : PTimeRec);
var
    TheSession : TWebSession;
begin
    if PItem.Data <> nil then begin
        TheSession := TWebSession(PItem.Data);
        if Assigned(FOnDeleteSession) then
            FOnDeleteSession(Self, TheSession);
        // Do not free WebSessions that has a positive RefCount
        if TheSession.DecRefCount < 0 then
            TheSession.Free;
        PItem.Data := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.Clear;
begin
    Lock;
    try
        if Assigned(FTimeList) then begin
            while FTimeList.Count > 0 do
                DeleteSession(PTimeRec(FTimeList.Items[0]).Value);
        end;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSessions.GetMaxAge: Integer;
begin
    Lock;
    try
        if Assigned(FTimeList) then
            Result := FTimeList.MaxAge
        else
            Result := 0;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.SetMaxSessions(const Value: Integer);    { V8.01 }
begin
    Lock;
    try
        if Assigned(FTimeList) then
            FTimeList.MaxItems := Value;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSessions.GetMaxSessions: Integer;            { V8.01 } 
begin
    Lock;
    try
        if Assigned(FTimeList) then
            Result := FTimeList.MaxItems
        else
            Result := 0;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.RemoveAged;
begin
    Lock;
    try
        if Assigned(FTimeList) then
            FTimeList.RemoveAged;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.SetMaxAge(const Value: Integer);
begin
    Lock;
    try
        if Assigned(FTimeList) then
            FTimeList.MaxAge := Value;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// CreateSession _must_ be matched with a ReleaseSession
function TWebSessions.CreateSession(
    const Param     : String;
    var   SessionID : String): TWebSession;
begin
    Result := CreateSessionEx(Param, SessionID, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// CreateSession _must_ be matched with a ReleaseSession
function TWebSessions.CreateSessionEx(
    const Param     : String;
    var   SessionID : String;
    SessionData     : TWebSessionData): TWebSession;
var
    TheSessionID         : String;
    Year, Month, Day     : Word;
    Hour, Min, Sec, MSec : Word;
    Today                : TDateTime;
begin
    Today := Now;
    DecodeDate(Today, Year, Month, Day);
    DecodeTime(Today, Hour, Min, Sec, MSec);
    TheSessionID := Format('^%s^%s^%04d%02d%02d %02d%02d%02d.%03d^',
                           [UpperCase(Trim(Param)),
                            IntToHex(GSessionID, 8),
                            Year, Month, Day,
                            Hour, Min, Sec, MSec]);
    SessionID    := Base64Encode(TheSessionID);
    Lock;
    try
        Result       := AddSession(SessionID);
        if Assigned(Result) then begin
            Result.IncRefCount;
            Result.SessionData := SessionData;
        end;
//WriteLn('CreateSession. RefCount = ', Result.RefCount);
    finally
        Unlock;
    end;
    if Assigned(FOnCreateSession) then
        FOnCreateSession(Self, Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Works with ReleaseSession.
// Each call to ValidateSession _must_ be balanced by a call to ReleaseSession
procedure TWebSessions.ValidateSession(
    const SessionID : String;
    SessionRef      : PWebSession);
begin
    Lock;
    try
        RemoveAged;
        SessionRef^ := FindSession(SessionID);
        if Assigned(SessionRef^) then begin
            SessionRef^.FTimeRec := FTimeList.AddWithData(SessionID, SessionRef^, nil);
            SessionRef^.IncRefCount;
//WriteLn('ValidateSession. RefCount = ', SessionRef^.RefCount);
        end
        else begin
        end;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSessions.GetCount: Integer;
begin
    Lock;
    try
        if not Assigned(FTimeList) then
            Result := 0
        else
            Result := FTimeList.Count;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWebSessions.GetSessions(nIndex: Integer): TWebSession;
var
    PItem : PTimeRec;
begin
    Lock;
    try
        if not Assigned(FTimeList) then
            Result := nil
        else begin
            PItem := FTimeList.Items[nIndex];
            if PItem = nil then
                Result := nil
            else
                Result := TWebSession(PItem.Data);
        end;
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.Lock;
begin
    Inc(LockCount);               // LockCount is a threadvar !
    if (LockCount = 1) and Assigned(FCritSec) then
        FCritSec.Enter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.Unlock;
begin
    if LockCount <= 0 then        // Should never occur ! Unlock called
        LockCount := 0            // without first calling Lock.
    else begin
        Dec(LockCount);           // LockCount is a threadvar !
        if (LockCount = 0)and Assigned(FCritSec) then
            FCritSec.Leave;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Works with ValidateSession.
// Each call to ReleaseSession _must_ be balanced by a preceding call
// to ValidateSession
procedure TWebSessions.ReleaseSession(SessionRef: PWebSession);
begin
    if not Assigned(SessionRef^) then
        Exit;
//WriteLn('ReleaseSession. RefCount = ', SessionRef^.RefCount);
    SessionRef^.DecRefCount;
    if SessionRef^.RefCount < 0 then
        SessionRef^.Destroy;
    SessionRef^ := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.SaveToStream(Dest : TStream);
begin
    Lock;
    try
        Dest.Write(GSignature[1], Length(GSignature));
        Dest.WriteComponent(Self);
    finally
        Unlock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.SaveToFile(const FileName : String);
var
    Stream : TFileStream;
begin
    Stream := TFileStream.Create(FileName, fmCreate);
    try
        SaveToStream(Stream);
    finally
        Stream.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.DefineProperties(Filer: TFiler);
begin
    inherited;
    Filer.DefineProperty('Sessions',
                         ReadSessionsProperty,
                         WriteSessionsProperty, Count > 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.ReadSessionsProperty(Reader: TReader);
var
    WebSession : TWebSession;
    Value      : String;
begin
    Reader.ReadListBegin;
    FTimeList.Clear;
    while not Reader.EndOfList do begin
        Value      := Reader.ReadString;
        WebSession := Reader.ReadComponent(nil) as TWebSession;
        WebSession.FTimeRec := FTimeList.AddWithData(Value, WebSession, nil);
        WebSession.FTimeRec^.TimeMark := WebSession.FTimeMark;
        WebSession.FSessions          := Self;
    end;
    Reader.ReadListEnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.WriteSessionsProperty(Writer: TWriter);
var
    I:Integer;
begin
    Writer.WriteListBegin;
    for I := 0 to Count - 1 do begin
        Writer.WriteString(PTimeRec(FTimeList.Items[I]).Value);
        Writer.WriteComponent(Sessions[I]);
    end;
    Writer.WriteListEnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.LoadFromFile(const FileName: String);
var
    Stream : TFileStream;
begin
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
        LoadFromStream(Stream);
    finally
        Stream.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessions.LoadFromStream(Src: TStream);
var
    Buf              : AnsiString;
    I, J             : Integer;
    CName            : String;
    OldDeleteSession : TDeleteSessionEvent;
    OldMaxAge        : Integer;
begin
    if Src.Size = 0 then
        Exit;
    // First, check the signature
    SetLength(Buf, Length(GSignature));
    I := Src.Read(Buf[1], Length(GSignature));
    if (I <> Length(GSignature)) or (Buf <> GSignature) then
        raise Exception.Create('TWebSessions.LoadFromStream: ' +
                               'invalid data format');

    // Disable OnDeleteSession event because most of the time deleteing or
    // adding a session will result in a SaveToFile which would fails
    // when we are in the load process (the file is opened).
    OldDeleteSession := OnDeleteSession;
    OnDeleteSession  := nil;
    OldMaxAge := MaxAge ; // V1.01 Angus 17 Apr 2010 keep MaxAge since it about to read from file
    try
        // Delete all existing data
        Clear;
        // Load data from stream (we use Delphi serialization to save)
        Src.ReadComponent(Self);
        // Remove old sessions we just loaded
        RemoveAged;
    finally
        OnDeleteSession := OldDeleteSession;
    end;
    MaxAge := OldMaxAge ; // V1.01 Angus 17 Apr 2010 restore MaxAge

    // Update the global counters according to what was loaded
    for I := Count - 1 downto 0 do begin
        if Sessions[I].Session > GSessionID then
            GSessionID := Sessions[I].Session;
        if Assigned(Sessions[I].SessionData) then begin
            if  Sessions[I].SessionData.SessionDataCount > 0 then // V1.01 Angus 17 Apr 2010 might be available as integer
                J :=  Sessions[I].SessionData.SessionDataCount
            else begin
                CName := Sessions[I].SessionData.Name;
                J := Length(CName);
//              while (J > 0) and (CName[J] in ['0'..'9']) do
                while (J > 0) and IsCharInSysCharSet(CName[J], ['0'..'9']) do
                    Dec(J);
                J := StrToInt(Copy(CName, J + 1, 10));
            end;
            if J > GSessionDataCount then
                GSessionDataCount := J;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CompName(const ClassName : String; Count : Integer) : String;
begin
    Result := Copy(ClassName, 2, Length(ClassName)) + IntToStr(Count);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TWebSessionData.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    InterlockedIncrement(GSessionDataCount);
// Name has to be set by the caller. For example calling AssignName method
//    Name := CompName(ClassName, GSessionDataCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TWebSessionData.Destroy;
begin
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebSessionData.AssignName;
begin
    Name := CompName(ClassName, GSessionDataCount);
    SessionDataCount := GSessionDataCount;  // V1.01 Angus 17 Apr 2010 keep counter
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
    RegisterClass(TWebSession);
    RegisterClass(TWebSessions);
    RegisterClass(TWebSessionData);


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
