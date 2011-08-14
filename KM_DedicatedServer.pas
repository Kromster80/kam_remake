unit KM_DedicatedServer;
interface
uses
  SysUtils,
  {$IFDEF MSWindows}Windows,{$ENDIF}
  KM_NetServer, KM_MasterServer, KM_Defaults, KM_CommonTypes;

const
  AnnounceInterval = 60; //Should be an INI setting later

type
  TKMDedicatedServer = class
  private
    fLastPing, fLastAnnounce: cardinal;
    fNetServer: TKMNetServer;
    fMasterServer: TKMMasterServer;
    fOnMessage: TStringEvent;
    procedure StatusMessage(const aData: string);
    procedure MasterServerError(const aData: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure UpdateState;
    property OnMessage: TStringEvent write fOnMessage;
  end;


implementation
{$IFDEF Unix} uses KM_Utils; {$ENDIF} //Needed in Linux for FakeGetTickCount


constructor TKMDedicatedServer.Create;
begin
  Inherited;
  fNetServer := TKMNetServer.Create(true); //Allow rooms in the dedicated server
  fMasterServer := TKMMasterServer.Create('http://lewin.hodgman.id.au/kam_remake_master_server/');
  fMasterServer.OnError := MasterServerError;
  fLastPing := 0;
  fLastAnnounce := 0;
end;


destructor TKMDedicatedServer.Destroy;
begin
  fNetServer.Free;
  StatusMessage('Server destroyed');
  Inherited;
end;


procedure TKMDedicatedServer.Start;
begin
  fNetServer.OnStatusMessage := StatusMessage;
  try
    fNetServer.StartListening(KAM_PORT);
  except
    on E : Exception do
    begin
      //Server failed to start
      StatusMessage('SERVER FAILED TO START! '+E.ClassName+': '+E.Message);
      Stop;
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

  TickCount := {$IFDEF MSWindows}GetTickCount{$ENDIF}
               {$IFDEF Unix} FakeGetTickCount{$ENDIF};
  if TickCount-fLastPing >= 1000 then
  begin
    fNetServer.MeasurePings;
    fLastPing := TickCount;
  end;

  if TickCount-fLastAnnounce >= AnnounceInterval*1000 then
  begin
    fMasterServer.AnnounceServer(KAM_PORT,AnnounceInterval+10);
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
