{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  A TWSocket that has server functions: it listen to connections
              an create other TWSocket to handle connection for each client.
Creation:     Aug 29, 1999
Version:      5.04
EMail:        francois.piette@overbyte.be     http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2008 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

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

Quick reference guide:
TWSocketServer will normally be used to listen on a given tcp port. When a
client connect, it will instanciate a new TWSocketClient component to handle
communication with client. Normally you will derive your own component from
TWSocketClient to add private data and methods to handle it. You tell
TWSocketServer which component it has to instanciate using ClientClass
property. You have to initialize instances from OnClientConnect event handler.
TWSocketServer maintain a list of connected clients. You can access it using
Client[] indexed property and ClientCount property.

History:
Sep 05, 1999 V1.01 Adpted for Delphi 1
Oct 09, 1999 V1.02 Added intermediate class TCustomWSocket
Nov 12, 1999 V1.03 Added OnClientCreate event just after client component has
                   been created.
Apr 02, 2000 V1.04 Added FSessionClosedFlag to avoid double SessionClosed
                   event triggering
Apr 13, 2002 V1.05 When sending banner to client, add LineEnd instead of CR/LF
                   as suggested by David Aguirre Grazio <djagra@xaire.com>
Sep 13, 2002 V1.06 Check if Assigned(Server) in TriggerSessionClosed.
                   Reported by Matthew Meadows <matthew.meadows@inquisite.com>
Sep 16, 2002 V1.07 Fixed a Delphi 1 issue in TriggerSessionClosed where
                   property was used in place of field variable.
Jan 04, 2003 V1.08 Renamed BannerToBusy to BannerTooBusy. This will cause
                   trouble in applications already using this property. You
                   have to rename the property in your app !
Jan 24, 2003 V5.00 Skipped to version 5 because of SSL code
Jan 26, 2004 V5.01 Introduced ICSDEFS.INC and reordered uses for FPC
                   compatibility.
May 01, 2004 V5.02 WMClientClosed was incorrectly referencing global Error
                   variable instead of the real winsock error code. Now pass
                   the errcode in WParam at the time of PostMessage.
                   Removed Forms and Graphics units from the uses clause.
May 23, 2005 V5.03 Added intermediate variable NewHSocket in procedure
                   TriggerSessionAvailable
Dec 30, 2005 V5.04 A.Garrels added IcsLogger

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit WSocketS;

interface

{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in IcsDefs.inc or in the project/package options.                  }

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I ICSDEFS.INC}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF DELPHI1}
    {$DEFINE NO_DEBUG_LOG} { Not for Delphi 1 }
{$ENDIF}

uses
    Messages,
{$IFDEF USEWINDOWS}
    Windows,
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
    SysUtils, Classes,
{$IFNDEF NO_DEBUG_LOG}
    IcsLogger,
{$ENDIF}
    WSocket, Winsock;

const
    WSocketServerVersion     = 504;
    CopyRight : String       = ' TWSocketServer (c) 1999-2007 F. Piette V5.04 ';
    WM_CLIENT_CLOSED         = WM_USER + 30;
    DefaultBanner            = 'Welcome to TcpSrv';

type
    TCustomWSocketServer       = class;
    TWSocketClient             = class;
    TWSocketClientClass        = class of TWSocketClient;
    TWSocketClientCreateEvent  = procedure (Sender : TObject;
                                            Client : TWSocketClient) of object;
    TWSocketClientConnectEvent = procedure (Sender : TObject;
                                            Client : TWSocketClient;
                                            Error  : Word) of object;

    { TWSocketClient is used to handle all client connections.           }
    { Altough you may use it directly, you'll probably wants to use your }
    { own derived component to add data and methods suited to your       }
    { application.                                                       }
    { If you use a derived component, then assign it's class to          }
    { TWSocketServer ClientClass property.                               }
    TWSocketClient = class(TWSocket)
    protected
        FBanner            : String;
        FServer            : TCustomWSocketServer;
        FPeerAddr          : String;
        FPeerPort          : String;
        FSessionClosedFlag : Boolean;
    public
        procedure   StartConnection; virtual;
        procedure   TriggerSessionClosed(ErrCode : Word); override;
        procedure   Dup(newHSocket : TSocket); override;
        function    GetPeerAddr: String; override;
        function    GetPeerPort: String; override;
        property    Server : TCustomWSocketServer read  FServer
                                                  write FServer;
    published
        property Banner : String           read  FBanner
                                           write FBanner;
    end;

    { TWSocketServer is made for listening for tcp client connections.      }
    { For each connection, it instanciate a new TWSocketClient (or derived) }
    { to handle connection. Use ClientClass to specify your derived.        }
    TCustomWSocketServer = class(TWSocket)
    protected
        FBanner                 : String;
        FBannerTooBusy          : String;
        FClientClass            : TWSocketClientClass;
        FClientList             : TList;
        FClientNum              : LongInt;
        FMaxClients             : LongInt;
        FOnClientCreate         : TWSocketClientCreateEvent;
        FOnClientConnect        : TWSocketClientConnectEvent;
        FOnClientDisconnect     : TWSocketClientConnectEvent;
        procedure WndProc(var MsgRec: TMessage); override;
        procedure Notification(AComponent: TComponent; operation: TOperation); override;
        procedure TriggerSessionAvailable(Error : Word); override;
        procedure TriggerClientCreate(Client : TWSocketClient); virtual;
        procedure TriggerClientConnect(Client : TWSocketClient; Error : Word); virtual;
        procedure TriggerClientDisconnect(Client : TWSocketClient; Error : Word); virtual;
        function  GetClientCount : Integer; virtual;
        function  GetClient(nIndex : Integer) : TWSocketClient; virtual;
        procedure WMClientClosed(var msg: TMessage);
                                 message WM_CLIENT_CLOSED;
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        { Check  if a given object is one of our clients }
        function  IsClient(SomeThing : TObject) : Boolean;
    protected
        { TWSocketClient derived class to instanciate for each client }
        property  ClientClass            : TWSocketClientClass
                                                      read  FClientClass
                                                      write FClientClass;
        { How many active clients we currently have }
        property  ClientCount   : Integer             read  GetClientCount;
        { Client[] give direct access to anyone of our clients }
        property  Client[nIndex : Integer] : TWSocketClient
                                                      read  GetClient;
    published
        { Banner sent to client as welcome message. Can be empty. }
        property  Banner                 : String     read  FBanner
                                                      write FBanner;
        property  BannerTooBusy          : String     read  FBannerTooBusy
                                                      write FBannerTooBusy;
        property  MaxClients             : LongInt    read  FMaxClients
                                                      write FMaxClients;
        { Triggered when a client disconnect }
        property  OnClientDisconnect     : TWSocketClientConnectEvent
                                                      read  FOnClientDisconnect
                                                      write FOnClientDisconnect;
        { Triggerred when a new client is connecting }
        property  OnClientConnect        : TWSocketClientConnectEvent
                                                      read  FOnClientConnect
                                                      write FOnClientConnect;
        { Triggerred when a new client component has been created }
        property  OnClientCreate         : TWSocketClientCreateEvent
                                                      read  FOnClientCreate
                                                      write FOnClientCreate;
    end;

    TWSocketServer = class(TCustomWSocketServer)
    public
        property  ClientClass;
        property  ClientCount;
        property  Client;
    published
    {$IFNDEF NO_DEBUG_LOG}
        property  IcsLogger;                                 { V5.04 }
    {$ENDIF}
        property  Banner;
        property  BannerTooBusy;
        property  MaxClients;
        property  OnClientDisconnect;
        property  OnClientConnect;
    end;

{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in IcsDefs.inc or in the project/package options.                  }
{$IFDEF USE_SSL}
{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  A component adding SSL support to TWSocketServer.
              Requires OpenSSL (http://www.openssl.org).
              More details in ReadMeIcsSsl.txt and IcsSslHowTo.txt.
              SSL demo applications can be found in /Delphi/SslInternet.
              If you use Delphi 7 and later, you may want to disable warnings
              for unsage type, unsafe code and unsafe typecast in the project
              options. Those warning are intended for .NET programs. You may
              also want to turn off deprecated symbol and platform symbol
              warnings.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
     SslWSocketServerVersion            = 100;
     SslWSocketServerDate               = 'Feb 02, 2003';
     SslWSocketServerCopyRight : String = ' TSslWSocket (c) 2003 Francois Piette V1.00.3 ';

type
    TSslWSocketClient = class(TWSocketClient)
    public
        constructor Create(AOwner : TComponent); override;
        procedure   StartConnection; override;
    end;

    TSslWSocketServer = class(TWSocketServer)
    protected
        procedure TriggerClientConnect(Client : TWSocketClient; Error : Word); override;
        procedure TriggerClientCreate(Client : TWSocketClient); override;
    public
        constructor Create(AOwner : TComponent); override;
        property  ClientClass;
        property  ClientCount;
        property  Client;
        property  SslMode;
    published
        property  SslContext;
        property  Banner;
        property  BannerTooBusy;
        property  MaxClients;
        property  OnClientDisconnect;
        property  OnClientConnect;
        property  SslEnable;
        property  SslAcceptableHosts;
        property  OnSslVerifyPeer;
        property  OnSslSetSessionIDContext;
        property  OnSslSvrNewSession;
        property  OnSslSvrGetSession;
        property  OnSslHandshakeDone;
    end;

{$ENDIF} //SSL

procedure Register;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TWSocketServer
{$IFDEF USE_SSL}
                                   , TSslWSocketServer
{$ENDIF}
                                  ]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomWSocketServer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FClientList      := TList.Create;
    FClientClass     := TWSocketClient;
    FBanner          := DefaultBanner;
    FBannerTooBusy   := 'Sorry, too many clients';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomWSocketServer.Destroy;
var
    I : Integer;
begin
    if Assigned(FClientList) then begin
        { We need to destroy all clients }
        for I := FClientList.Count - 1 downto 0 do begin
            try
                TWSocketClient(FClientList.Items[I]).Destroy;
            except
                { Ignore any exception here }
            end;
        end;
        { Then we can destroy client list }
        FClientList.Free;
        FClientList := nil;
    end;
    { And finally destroy ourself }
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Message handler                                                           }
procedure TCustomWSocketServer.WndProc(var MsgRec: TMessage);
begin
    with MsgRec do begin
        if Msg = WM_CLIENT_CLOSED then begin
            { We *MUST* handle all exception to avoid application shutdown }
            try
                WMClientClosed(MsgRec)
            except
                on E:Exception do
                    HandleBackGroundException(E);
            end;
        end
        else
            inherited WndProc(MsgRec);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Called by destructor when child component (a clients) is create or        }
{ destroyed.                                                                }
procedure TCustomWSocketServer.Notification(
    AComponent : TComponent;
    Operation  : TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Assigned(FClientList) and (AComponent is TWSocketClient) then begin
        if Operation = opInsert then
            { A new client has been created, add it to our list }
            FClientList.Add(AComponent)
        else if Operation = opRemove then
            { If one of our client has been destroyed, remove it from our list }
            FClientList.Remove(AComponent);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Called when a session is available, that is when a client is connecting   }
procedure TCustomWSocketServer.TriggerSessionAvailable(Error : Word);
var
    Client     : TWSocketClient;
    NewHSocket : TSocket;
begin
{$IFDEF DEBUG_OUTPUT}
    OutputDebugString('OnSessionAvailable');
{$ENDIF}
    { Call parent event handler }
    inherited TriggerSessionAvailable(Error);
    { In case of error, do nothing }
    if Error <> 0 then
        Exit;

    NewHSocket             := Accept;
{$IFDEF DEBUG_OUTPUT}
    OutputDebugString('NewHSocket = ' + IntToStr(NewHSocket));
{$ENDIF}
    Inc(FClientNum);
    Client                 := FClientClass.Create(Self);
    TriggerClientCreate(Client);
    Client.Name            := Name + 'Client' + IntToStr(FClientNum);
    Client.Banner          := FBanner;
    Client.Server          := Self;
{$IFNDEF NO_DEBUG_LOG}
    Client.IcsLogger       := IcsLogger;                           { V5.04 }
{$ENDIF}
    Client.Dup(NewHSocket);
    TriggerClientConnect(Client, Error);
    { The event handler may have destroyed the client ! }
    if FClientList.IndexOf(Client) < 0 then
        Exit;
    { The event handler may have closed the connection }
    if Client.State <> wsConnected then
        Exit;
    { Ok, the client is still there, process with the connection }
    if (FMaxClients > 0) and (FMaxClients < ClientCount) then begin
        { Sorry, toomuch clients }
        Client.Banner := FBannerTooBusy;
        Client.StartConnection;
        Client.Close;
    end
    else
        Client.StartConnection;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.TriggerClientConnect(
    Client : TWSocketClient; Error : Word);
begin
    if Assigned(FOnClientConnect) then
        FOnClientConnect(Self, Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.TriggerClientCreate(Client : TWSocketClient);
begin
    if Assigned(FOnClientCreate) then
        FOnClientCreate(Self, Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.TriggerClientDisconnect(
    Client : TWSocketClient; Error : Word);
begin
    if Assigned(FOnClientDisconnect) then
        FOnClientDisconnect(Self, Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ get number of connect clients                                               }
function TCustomWSocketServer.GetClientCount : Integer;
begin
    if Assigned(FClientList) then
        Result := FClientList.Count
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Acces method to return a client by index.                                   }
{ Return nil if index is out of range.                                        }
function TCustomWSocketServer.GetClient(nIndex : Integer) : TWSocketClient;
begin
    if not Assigned(FClientList) then begin
        Result := nil;
        Exit;
    end;
    if (nIndex < 0) or (nIndex >= FClientList.Count) then begin
        Result := nil;
        Exit;
    end;
    Result := TWSocketClient(FClientList.Items[nIndex]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Client has closed. Remove it from client list and destroy component.        }
procedure TCustomWSocketServer.WMClientClosed(var msg: TMessage);
var
    Client : TWSocketClient;
begin
    Client := TWSocketClient(Msg.LParam);
    try
        TriggerClientDisconnect(Client, Msg.WParam);
    finally
        { Calling Destroy will automatically remove client from list because }
        { we installed a notification handler.                               }
        Client.Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check if a given object is one of our clients.                              }
function TCustomWSocketServer.IsClient(SomeThing : TObject) : Boolean;
begin
    if not Assigned(FClientList) then
        Result := FALSE
    else
        Result := (FClientList.IndexOf(Pointer(SomeThing)) >= 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{*                                                                           *}
{*                            TWSocketClient                                 *}
{*                                                                           *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketClient.StartConnection;
begin
    if Length(FBanner) > 0 then
        SendStr(FBanner + FLineEnd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Triggered when socket is closed. Need to inform server socket to update   }
{ client list and trigger client disconnect event.                          }
procedure TWSocketClient.TriggerSessionClosed(ErrCode : Word);
begin
    if not FSessionClosedFlag then begin
        FSessionClosedFlag := TRUE;
        if Assigned(FServer) then
            PostMessage(Server.Handle, WM_CLIENT_CLOSED, ErrCode, LongInt(Self));
        inherited TriggerSessionClosed(ErrCode);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This override base class GetPeerAddr. It return cached value.             }
function TWSocketClient.GetPeerAddr: String;
begin
    Result := FPeerAddr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This override base class GetPeerPort. It return cached value.             }
function TWSocketClient.GetPeerPort: String;
begin
    Result := FPeerPort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Override base class. Dup is called when a client is assigned to a         }
{ TWSocket. Assigning HSocket property will call Dup.                       }
procedure TWSocketClient.Dup(newHSocket : TSocket);
begin
    inherited Dup(newHSocket);
    { Cache PeerAddr value }
    FPeerAddr := inherited GetPeerAddr;
    FPeerPort := inherited GetPeerPort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in IcsDefs.inc or in the project/package options.                  }
{$IFDEF USE_SSL}
constructor TSslWSocketServer.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    // Server socket doesn't use SSL to listen for clients
    FSslEnable       := TRUE;
    Port             := '443';
    Proto            := 'tcp';
    Addr             := '0.0.0.0';
    SslMode          := sslModeServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWSocketServer.TriggerClientCreate(Client : TWSocketClient);
begin
    inherited TriggerClientCreate(Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWSocketServer.TriggerClientConnect(
    Client : TWSocketClient; Error : Word);
begin
    inherited TriggerClientConnect(Client, Error);
    { The event handler may have closed the connection }
    { The event handler may also have started the SSL }
    if (Error <> 0) or (Client.State <> wsConnected) or
       (Client.SslState > sslNone) then
        Exit;
    Client.SslEnable := FSslEnable;
    if Client.SslEnable then begin
        Client.SslMode                  := FSslMode;
        Client.SslAcceptableHosts       := FSslAcceptableHosts;
        Client.SslContext               := FSslContext;
        Client.OnSslVerifyPeer          := OnSslVerifyPeer;
        Client.OnSslSetSessionIDContext := OnSslSetSessionIDContext;
        Client.OnSslSvrNewSession       := OnSslSvrNewSession;
        Client.OnSslSvrGetSession       := OnSslSvrGetSession;
        Client.OnSslHandshakeDone       := OnSslHandshakeDone;
        try
            if Client.SslMode = sslModeClient then
                Client.StartSslHandshake
            else
                Client.AcceptSslHandshake;
        except
            on E: Exception do begin                            // AG 12/18/05
                Client.SslEnable := False;
                Client.Abort;
                { Don't abort silently }
                Client.HandleBackGroundException(E);            // AG 12/18/05
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslWSocketClient.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWSocketClient.StartConnection;
begin
    inherited StartConnection;
end;
{$ENDIF} // SSL


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

