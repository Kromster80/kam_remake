{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TSnmpCli class encapsulate the SNMP client paradigm
Creation:     March 2011
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2011 by François PIETTE
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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSnmpCli;

{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$H+}             { Use long strings                    }
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.ExtCtrls{$ELSE}ExtCtrls{$ENDIF},
    OverbyteIcsWndControl,
    OverbyteIcsWinsock,
    OverbyteIcsWSocket,
    OverbyteIcsAsn1Utils,
    OverbyteIcsSnmpMsgs;

const
  SnmpClientVersion         = 800;
  CopyRight    : String     = ' TSnmpClient (c) 2012 Francois Piette V8.00 ';


type
    TSnmpCliReq = (srqGet, srqGetNext);
    TSnmpCliAct = (sacNone, sacContinue, sacBreak);
    TRequestDoneEvent = procedure (Sender     : TObject;
                                   ReqType    : TSnmpCliReq;
                                   var Action : TSnmpCliAct;
                                   ErrCode    : Word) of object;

    TDebugWSocket = class(TWSocket)
    protected
        function    TriggerDataAvailable(Error : Word) : Boolean; override;
    end;

    TSnmpCli = class(TIcsWndControl)
    protected
        FWSocket              : TDebugWSocket;
        FReqType              : TSnmpCliReq;
        FVersion              : Integer;
        FCommunity            : AnsiString;
        FQuery                : TSNMPRec;
        FReply                : TSNMPRec;
        FBusy                 : Boolean;
        FOID                  : String;
        FHostIP               : String;
        FPort                 : String;
        FAddr                 : String;
        FTimeout              : Integer;
        FResponseValue        : String;
        FResponseValueType    : Integer;
        FResponseOID          : String;
        FDestAddr             : TSockAddr;
        FMsg_WM_LISTENING     : UINT;
        FMsg_WM_GETNEXT       : UINT;
        FBaseOID              : String;
        FCurrentID            : Integer;  // Incremented by InternalSendTo
        FOnRequestDone        : TRequestDoneEvent;
{$IFNDEF SNMP_BUILTIN_TIMEOUT}
        FTimeoutTimer         : TTimer;
{$ENDIF}
        procedure WSocketDataAvailable(Sender: TObject; ErrCode: Word);
{$IFDEF SNMP_BUILTIN_TIMEOUT}
        procedure WSocketTimeout(Sender: TObject; Reason: TTimeoutReason);
{$ELSE}
        procedure TimeoutTimerTimer(Sender: TObject);
{$ENDIF}
        procedure TriggerRequestDone(ReqType    : TSnmpCliReq;
                                     var Action : TSnmpCliAct;
                                     ErrCode    : Word); virtual;
        function  MsgHandlersCount: Integer; override;
        procedure AllocateMsgHandlers; override;
        procedure FreeMsgHandlers; override;
        procedure WndProc(var MsgRec: TMessage); override;
        procedure WMListening(var Msg: TMessage);
        procedure WMGetNext(var Msg: TMessage);
        function  InternalSendTo : Boolean;
        function  InternalGetNext(const OID: String): Boolean;
        procedure Display(const Msg : String);
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        function  SendTo : Boolean;
        function  Get : Boolean;
        function  GetNext : Boolean;
        procedure Abort;
        property Query             : TSNMPRec      read  FQuery;
        property Reply             : TSNMPRec      read  FReply;
        property Busy              : Boolean       read  FBusy;
        property ResponseValue     : String        read  FResponseValue;
        property ResponseValueType : Integer       read  FResponseValueType;
        property ResponseOID       : String        read  FResponseOID;
        // CurrentID is incremented just before sending the request
        property CurrentID         : Integer       read  FCurrentID;
    published
        property Version       : Integer           read  FVersion
                                                   write FVersion;
        property Community     : AnsiString        read  FCommunity
                                                   write FCommunity;
        property Port          : String            read  FPort
                                                   write FPort;
        property Addr          : String            read  FAddr
                                                   write FAddr;
        property HostIP        : String            read  FHostIP
                                                   write FHostIP;
        property Timeout       : Integer           read  FTimeout
                                                   write FTimeout;
        property OID           : String            read  FOID
                                                   write FOID;
        property OnRequestDone : TRequestDoneEvent read  FOnRequestDone
                                                   write FOnRequestDone;
    end;

function UnicodeIntoAnsiToString(const S : AnsiString) : String;

var
    GDisplayProc : procedure (Sender : TObject;
                              const Msg : String) of object = nil;

const
    SNMP_TIMEOUT = 20300;

implementation

{ TSnmpCli }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSnmpCli.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    AllocateHWnd;
    FWSocket       := TDebugWSocket.Create(Self);
    FPort          := '4000';
    FAddr          := '0.0.0.0';
    FQuery         := TSNMPRec.Create;
    FReply         := TSNMPRec.Create;
    FTimeoutTimer  := TTimer.Create(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSnmpCli.Destroy;
begin
    FreeAndNil(FWSocket);
    FreeAndNil(FQuery);
    FreeAndNil(FReply);
    FreeAndNil(FTimeoutTimer);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpCli.Display(const Msg: String);
begin
    if Assigned(GDisplayProc) then
        GDisplayProc(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpCli.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_LISTENING := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_GETNEXT   := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpCli.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_LISTENING);
        FWndHandler.UnregisterMessage(FMsg_WM_GETNEXT);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSnmpCli.MsgHandlersCount: Integer;
begin
    Result := 2 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpCli.WndProc(var MsgRec: TMessage);
begin
    try
        with MsgRec do begin
            if Msg = FMsg_WM_LISTENING then
                WMListening(MsgRec)
            else if Msg = FMsg_WM_GETNEXT then
                WMGetNext(MsgRec)
            else
                inherited WndProc(MsgRec);
        end;
    except
        on E:Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpCli.WMListening(var Msg: TMessage);
var
    Buffer   : AnsiString;
begin
    Buffer := FQuery.EncodeBuf;
    ZeroMemory(@FDestAddr, SizeOf(FDestAddr));
    FDestAddr.sin_family      := AF_INET;
    FDestAddr.sin_addr.S_addr := WSocket_inet_addr(AnsiString(Trim(FHostIP)));
    FDestAddr.sin_port        := WSocket_htons(StrToInt(cSnmpProtocol));
{$IFDEF SNMP_BUILTIN_TIMEOUT}
    FWSocket.TimeoutIdle      := FTimeout;
    FWSocket.TimeoutSampling  := 1000;
    FWSocket.TimeoutStartSampling;
{$ELSE}
    FTimeoutTimer.Interval    := FTimeout;
    FTimeoutTimer.Enabled     := TRUE;
{$ENDIF}
    FWSocket.SendTo(FDestAddr, SizeOf(FDestAddr),
                    PAnsiChar(Buffer), Length(Buffer));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpCli.TriggerRequestDone(
    ReqType    : TSnmpCliReq;
    var Action : TSnmpCliAct;
    ErrCode    : Word);
begin
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, ReqType, Action, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSnmpCli.SendTo : Boolean;
begin
    if FBusy then
        raise Exception.Create('SNMP component is busy');
    FReqType     := srqGet;
    Result       := InternalSendTo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSnmpCli.InternalSendTo : Boolean;
begin
    Result   := FALSE;
    FBusy    := TRUE;
    try
        Inc(FCurrentID);
        if FCurrentID = MAXINT then  // Wrap around to avoid overflow !
            FCurrentID := 1;
        Query.ID        := FCurrentID;
        Query.Version   := Version;
        Query.Community := Community;
        FResponseValue  := '';
        if FWSocket.State in [wsConnected, wsListening] then
            FWSocket.Close;
        FReply.Clear;
        FWSocket.OnDataAvailable := WSocketDataAvailable;
{$IFDEF SNMP_BUILTIN_TIMEOUT}
        FWSocket.OnTimeout       := WSocketTimeout;
{$ELSE}
        FTimeoutTimer.OnTimer    := TimeoutTimerTimer;
{$ENDIF}
        FWSocket.Proto           := 'udp';
        FWSocket.Port            := FPort;
        FWSocket.Addr            := FAddr;
        FWSocket.Listen;
        Sleep(0);
        PostMessage(Handle, FMsg_WM_LISTENING, 0, 0);
        Result := TRUE;
    except
        FWSocket.Abort;
        FBusy := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF SNMP_BUILTIN_TIMEOUT}
procedure TSnmpCli.WSocketTimeout(Sender : TObject; Reason : TTimeoutReason);
begin
    FWSocket.Abort;
    FBusy := FALSE;
    TriggerRequestDone(SNMP_TIMEOUT);
end;
{$ELSE}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpCli.TimeoutTimerTimer(Sender: TObject);
var
    Action : TSnmpCliAct;
begin
    FTimeoutTimer.Enabled := FALSE;
    FWSocket.Abort;
    FBusy  := FALSE;
    Action := sacNone;    // Here it is not used
    TriggerRequestDone(FReqType, Action, SNMP_TIMEOUT);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpCli.WSocketDataAvailable(Sender : TObject; ErrCode : Word);
var
    Src    : TSockAddrIn;
    SrcLen : Integer;
    Buffer : AnsiString;
    Len    : Integer;
    Action : TSnmpCliAct;
begin
    Action := sacNone;
    if ErrCode <> 0 then begin
        FWSocket.Abort;
        FResponseValue := 'Error #' + IntToStr(ErrCode);
    end
    else begin
        SetLength(Buffer, 1024 * 16);
        SrcLen := SizeOf(Src);
        ZeroMemory(@Src, SrcLen);
        Len := FWSocket.ReceiveFrom(PAnsiChar(Buffer), Length(Buffer),
                                    Src, SrcLen);
        if Len <= 0 then begin
            Display('DataAvailable Len=-1');
            Exit;
        end;
        // Reply must come from SNMP port
        if WSocket_ntohs(Src.sin_port) <> StrToInt(cSnmpProtocol) then
            Exit;
        // Reply must come from our destination
        if Src.sin_addr.S_addr <> FDestAddr.sin_addr.S_addr then
            Exit;
        // Answer is from our destination on SNMP port, process it
        SetLength(Buffer, Len);
        if not FReply.DecodeBuf(Buffer) then
            raise Exception.Create('Error decoding reply');

        // Reply ID must match query ID
        if FReply.ID <> FQuery.ID then begin
            Display('SNMP reply ID (' + IntToStr(FReply.ID) +
                    ' do not match query ID (' + IntToStr(FQuery.ID) +
                    ', ignoring');
            Exit;
        end;

        if FReply.PDUType <> PDUGetResponse then begin
            Display('Unexpected reply PDU type ' +
                    IntToStr(Freply.PDUType));
            Exit;
        end;

        FTimeoutTimer.Enabled := FALSE;
        if FReqType = srqGet then begin
            FResponseOID       := FOID;
            FResponseValue     := UnicodeIntoAnsiToString(
                                      FReply.MIBGet(AnsiString(Trim(FOID))));
            FResponseValueType := TSNMPMib(Reply.SNMPMibList[0]).ValueType;
        end
        else if FReqType = srqGetNext then begin
            if Reply.SNMPMibList.Count > 0 then begin
                FResponseOID := String(TSNMPMib(Reply.SNMPMibList[0]).OID);
                if Pos(FBaseOID, FResponseOID) = 1 then begin
                    FResponseValue     := UnicodeIntoAnsiToString(TSNMPMib(Reply.SNMPMibList[0]).Value);
                    FResponseValueType := TSNMPMib(Reply.SNMPMibList[0]).ValueType;
                    Action             := sacContinue;
                    TriggerRequestDone(FReqType, Action, ErrCode);
                    if Action <> sacContinue then
                        FBusy := FALSE
                    else
                        PostMessage(Handle, FMsg_WM_GETNEXT, 0, 0);
                    Exit;
                end;
            end
            else begin
                FResponseOID       := '';
                FResponseValue     := '';
                FResponseValueType := ASN1_NULL;
            end;
        end
        else
            raise Exception.Create('Unexcepted ReqType');
    end;
{$IFDEF SNMP_BUILTIN_TIMEOUT}
    FWSocket.TimeoutStopSampling;
{$ELSE}
    FTimeoutTimer.Enabled := FALSE;
{$ENDIF}
    FBusy  := FALSE;
    TriggerRequestDone(FReqType, Action, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpCli.Abort;
begin
    FWSocket.Abort;
    FBusy := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TDebugWSocket }
function TDebugWSocket.TriggerDataAvailable(Error: Word): Boolean;
begin
//    if Assigned(GDisplayProc) then
//        GDisplayProc('TriggerDataAvailable');
    Result := inherited TriggerDataAvailable(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// This function tries to discover we have unicode characters into an
// ansistring and convert to string
function UnicodeIntoAnsiToString(const S : AnsiString) : String;
var
    Len, I, J : Integer;
begin
    // If the length is not even, then it can't be a unicode (UTF-16) string
    Len := Length(S);
    if (Len and 1) <> 0 then begin
        Result := String(S);
        Exit;
    end;
    I := 2;
    J := Len div 4;
    while (I <= Len) and (J >= 0)  do begin
        if S[I] = #0 then
            Dec(J);
        Inc(I, 2);
    end;
    if J < 0 then begin
        // Enough #à, consider it is already unicode
        SetLength(Result, Len div 2);
        Move(PAnsiChar(S)^, PChar(Result)^, Len);
    end
    else
        Result := String(S);  // Consider it is AnsiString
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSnmpCli.Get: Boolean;
begin
    if FBusy then
        raise Exception.Create('SNMP component is busy');

    Query.Clear;
    Query.PDUType   := PDUGetRequest;
    Query.MIBAdd(AnsiString(OID), '', ASN1_NULL);

    FReqType     := srqGet;
    Result       := InternalSendTo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSnmpCli.GetNext: Boolean;
begin
    if FBusy then
        raise Exception.Create('SNMP component is busy');
    FBaseOID         := OID;
    FResponseOID     := FBaseOID;
    Result           := InternalGetNext(FResponseOID);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSnmpCli.InternalGetNext(const OID : String) : Boolean;
begin
    Query.Clear;
    Query.PDUType := PDUGetNextRequest;
    // remove all MIBs already insert, we will insert our own
    while Query.MIBCount > 0 do
        Query.MIBDelete(0);
    Query.MIBAdd(AnsiString(OID), '', ASN1_NULL);
    FReqType := srqGetNext;
    Result   := InternalSendTo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpCli.WMGetNext(var Msg: TMessage);
begin
    InternalGetNext(FResponseOID);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
