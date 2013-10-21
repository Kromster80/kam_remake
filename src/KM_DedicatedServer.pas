unit KM_DedicatedServer;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, Classes, Math,
  {$IFDEF MSWindows}Windows,{$ENDIF}
  KM_NetServer, KM_MasterServer, KM_CommonTypes, KM_Defaults;

type
  TKMDedicatedServer = class
  private
    fLastPing, fLastAnnounce: cardinal;
    fNetServer: TKMNetServer;
    fMasterServer: TKMMasterServer;
    fOnMessage: TUnicodeStringEvent;
    fPublishServer: boolean;
    fAnnounceInterval: word;
    fPingInterval: word;
    fPort:string;
    fServerName: UnicodeString;
    procedure StatusMessage(const aData: string);
    procedure MasterServerError(const aData: string);
  public
    constructor Create(aMaxRooms, aKickTimeout, aPingInterval, aAnnounceInterval:word;
                       const aMasterServerAddress:string; const aHTMLStatusFile:string;
                       const aWelcomeMessage:string; aDedicated:Boolean);
    destructor Destroy; override;

    procedure Start(const aServerName: UnicodeString; const aPort:string; aPublishServer:boolean);
    procedure Stop;
    procedure UpdateState;
    procedure UpdateSettings(const aServerName: UnicodeString; aPublishServer:boolean; aKickTimeout, aPingInterval, aAnnounceInterval:word;
                             const aMasterServerAddress:string; const aHTMLStatusFile:string; const aWelcomeMessage:string);
    property OnMessage: TUnicodeStringEvent write fOnMessage;
    
    procedure GetServerInfo(var aList: TList);
  end;


implementation
uses KM_Utils;

  //Enforce a minimum so our master server doesn't get spammed
  const MINIMUM_ANNOUNCE_INTERVAL = 180;



//Announce interval of -1 means the server will not be published (LAN)
constructor TKMDedicatedServer.Create(aMaxRooms, aKickTimeout, aPingInterval, aAnnounceInterval:word;
                                      const aMasterServerAddress:string; const aHTMLStatusFile:string;
                                      const aWelcomeMessage:string; aDedicated:Boolean);
begin
  inherited Create;
  fNetServer := TKMNetServer.Create(aMaxRooms, aKickTimeout, aHTMLStatusFile, aWelcomeMessage);
  fMasterServer := TKMMasterServer.Create(aMasterServerAddress, aDedicated);
  fMasterServer.OnError := MasterServerError;
  fAnnounceInterval := Max(MINIMUM_ANNOUNCE_INTERVAL, aAnnounceInterval);
  fPingInterval := aPingInterval;
  fLastPing := 0;
  fLastAnnounce := 0;
end;


destructor TKMDedicatedServer.Destroy;
begin
  fNetServer.Free;
  fMasterServer.Free;
  StatusMessage('Server destroyed');
  inherited;
end;


procedure TKMDedicatedServer.Start(const aServerName: UnicodeString; const aPort:string; aPublishServer:boolean);
begin
  fPort := aPort;
  fServerName := aServerName;
  fPublishServer := aPublishServer;
  fNetServer.OnStatusMessage := StatusMessage;
  fNetServer.StartListening(fPort, aServerName);
end;


procedure TKMDedicatedServer.Stop;
begin
  fNetServer.StopListening;
  fNetServer.ClearClients;
  StatusMessage('Stopped listening');
end;


procedure TKMDedicatedServer.UpdateState;
var TickCount:Cardinal;
begin
  fNetServer.UpdateStateIdle;
  fMasterServer.UpdateStateIdle;

  if not fNetServer.Listening then Exit; //Do not measure pings or announce the server if we are not listening

  TickCount := TimeGet;
  if GetTimeSince(fLastPing) >= fPingInterval then
  begin
    fNetServer.MeasurePings;
    fLastPing := TickCount;
  end;

  if fPublishServer and (GetTimeSince(fLastAnnounce) >= fAnnounceInterval*1000) then
  begin
    fMasterServer.AnnounceServer(fServerName,fPort,fNetServer.GetPlayerCount,fAnnounceInterval+20);
    fLastAnnounce := TickCount;
  end;
end;


procedure TKMDedicatedServer.UpdateSettings(const aServerName: UnicodeString; aPublishServer:boolean; aKickTimeout, aPingInterval, aAnnounceInterval:word;
                                            const aMasterServerAddress:string; const aHTMLStatusFile:string; const aWelcomeMessage:string);
begin
  fAnnounceInterval := Max(MINIMUM_ANNOUNCE_INTERVAL, aAnnounceInterval);
  fPingInterval := aPingInterval;
  fMasterServer.MasterServerAddress := aMasterServerAddress;
  fServerName := aServerName;
  fPublishServer := aPublishServer;

  fNetServer.UpdateSettings(aKickTimeout, aHTMLStatusFile, aWelcomeMessage, aServerName);

  fLastAnnounce := 0; //Make the server announce itself next update so the changes are sent to the master server ASAP
end;


procedure TKMDedicatedServer.StatusMessage(const aData: string);
begin
  if Assigned(fOnMessage) then fOnMessage(aData);
end;


procedure TKMDedicatedServer.MasterServerError(const aData: string);
begin
  StatusMessage('HTTP Master Server: '+aData);
end;


procedure TKMDedicatedServer.GetServerInfo(var aList: TList);
begin
  fNetServer.GetServerInfo(aList);
end;


end.
