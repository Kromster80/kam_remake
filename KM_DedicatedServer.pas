unit KM_DedicatedServer;
interface
uses
  SysUtils,
  {$IFDEF MSWindows}Windows,{$ENDIF}
  KM_NetServer, KM_MasterServer, KM_CommonEvents, KM_Defaults;

type
  TKMDedicatedServer = class
  private
    fLastPing, fLastAnnounce: cardinal;
    fNetServer: TKMNetServer;
    fMasterServer: TKMMasterServer;
    fOnMessage: TStringEvent;
    fPublishServer: boolean;
    fAnnounceInterval: word;
    fPingInterval: word;
    fPort:string;
    fServerName:string;
    procedure StatusMessage(const aData: string);
    procedure MasterServerError(const aData: string);
  public
    constructor Create(aMaxRooms, aKickTimeout, aPingInterval, aAnnounceInterval:word;
                       const aMasterServerAddress:string; const aHTMLStatusFile:string;
                       const aWelcomeMessage:string);
    destructor Destroy; override;

    procedure Start(const aServerName:string; const aPort:string; aPublishServer:boolean; aHandleException:boolean);
    procedure Stop;
    procedure UpdateState;
    procedure UpdateSettings(const aServerName:string; aPublishServer:boolean; aKickTimeout, aPingInterval, aAnnounceInterval:word;
                             const aMasterServerAddress:string; const aHTMLStatusFile:string; const aWelcomeMessage:string);
    property OnMessage: TStringEvent write fOnMessage;
  end;


implementation
{$IFDEF Unix} uses KM_Utils; {$ENDIF} //Needed in Linux for FakeGetTickCount


//Announce interval of -1 means the server will not be published (LAN)
constructor TKMDedicatedServer.Create(aMaxRooms, aKickTimeout, aPingInterval, aAnnounceInterval:word;
                                      const aMasterServerAddress:string; const aHTMLStatusFile:string;
                                      const aWelcomeMessage:string);
begin
  Inherited Create;
  fNetServer := TKMNetServer.Create(aMaxRooms, aKickTimeout, aHTMLStatusFile, aWelcomeMessage);
  fMasterServer := TKMMasterServer.Create(aMasterServerAddress);
  fMasterServer.OnError := MasterServerError;
  fAnnounceInterval := aAnnounceInterval;
  fPingInterval := aPingInterval;
  fLastPing := 0;
  fLastAnnounce := 0;
end;


destructor TKMDedicatedServer.Destroy;
begin
  fNetServer.Free;
  fMasterServer.Free;
  StatusMessage('Server destroyed');
  Inherited;
end;


procedure TKMDedicatedServer.Start(const aServerName:string; const aPort:string; aPublishServer:boolean; aHandleException:boolean);
begin
  fPort := aPort;
  fServerName := aServerName;
  fPublishServer := aPublishServer;
  fNetServer.OnStatusMessage := StatusMessage;
  try
    fNetServer.StartListening(fPort,aServerName);
  except
    on E : Exception do
    begin
      //Server failed to start
      StatusMessage('SERVER FAILED TO START! '+E.ClassName+': '+E.Message);
      Stop;
      if not aHandleException then
        raise Exception.Create(E.ClassName+': '+E.Message);
    end;
  end;
end;


procedure TKMDedicatedServer.Stop;
begin
  fNetServer.StopListening;
  fNetServer.ClearClients;
  StatusMessage('Stopped listening');
end;


procedure TKMDedicatedServer.UpdateState;
var TickCount:DWord;
begin
  fNetServer.UpdateStateIdle;
  fMasterServer.UpdateStateIdle;

  if not fNetServer.Listening then Exit; //Do not measure pings or announce the server if we are not listening

  TickCount := {$IFDEF MSWindows}GetTickCount{$ENDIF}
               {$IFDEF Unix} FakeGetTickCount{$ENDIF};
  //They must be cast as Int64 otherwise it crashes when TickCount > fLastPing with "Integer Overflow"
  if abs(Int64(TickCount)-Int64(fLastPing)) >= fPingInterval then
  begin
    fNetServer.MeasurePings;
    fLastPing := TickCount;
  end;

  //They must be cast as Int64 otherwise it crashes when TickCount > fLastPing with "Integer Overflow"
  if fPublishServer and (abs(Int64(TickCount)-Int64(fLastAnnounce)) >= fAnnounceInterval*1000) then
  begin
    fMasterServer.AnnounceServer(fServerName,fPort,fNetServer.GetPlayerCount,fAnnounceInterval+20);
    fLastAnnounce := TickCount;
  end;
end;


procedure TKMDedicatedServer.UpdateSettings(const aServerName:string; aPublishServer:boolean; aKickTimeout, aPingInterval, aAnnounceInterval:word;
                                            const aMasterServerAddress:string; const aHTMLStatusFile:string; const aWelcomeMessage:string);
begin
  fAnnounceInterval := aAnnounceInterval;
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


end.
