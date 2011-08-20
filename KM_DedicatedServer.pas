unit KM_DedicatedServer;
interface
uses
  SysUtils,
  {$IFDEF MSWindows}Windows,{$ENDIF}
  KM_NetServer, KM_MasterServer, KM_Defaults, KM_CommonTypes;

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
    procedure StatusMessage(const aData: string);
    procedure MasterServerError(const aData: string);
  public
    constructor Create(aMaxRooms, aKickTimeout, aPingInterval, aAnnounceInterval:word; const aMasterServerAddress:string);
    destructor Destroy; override;

    procedure Start(const aPort:string; aPublishServer:boolean; aHandleException:boolean);
    procedure Stop;
    procedure UpdateState;
    property OnMessage: TStringEvent write fOnMessage;
  end;


implementation
{$IFDEF Unix} uses KM_Utils; {$ENDIF} //Needed in Linux for FakeGetTickCount


//Announce interval of -1 means the server will not be published (LAN)
constructor TKMDedicatedServer.Create(aMaxRooms, aKickTimeout, aPingInterval, aAnnounceInterval:word; const aMasterServerAddress:string);
begin
  Inherited Create;
  fNetServer := TKMNetServer.Create(aMaxRooms, aKickTimeout);
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
  StatusMessage('Server destroyed');
  Inherited;
end;


procedure TKMDedicatedServer.Start(const aPort:string; aPublishServer:boolean; aHandleException:boolean);
begin
  fPort := aPort;
  fPublishServer := aPublishServer;
  fNetServer.OnStatusMessage := StatusMessage;
  try
    fNetServer.StartListening(fPort);
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
  if TickCount-fLastPing >= fPingInterval then
  begin
    fNetServer.MeasurePings;
    fLastPing := TickCount;
  end;

  if fPublishServer and (TickCount-fLastAnnounce >= fAnnounceInterval*1000) then
  begin
    fMasterServer.AnnounceServer(fPort,fAnnounceInterval+10);
    fLastAnnounce := TickCount;
  end;
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
