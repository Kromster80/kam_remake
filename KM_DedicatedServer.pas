unit KM_DedicatedServer;

interface

uses
  SysUtils, Windows, KM_NetServer, KM_Defaults, KM_CommonTypes;

type
  TKMDedicatedServer = class
    private
      fLastPing: cardinal;
      fNetServer: TKMNetServer;
      fOnMessage: TStringEvent;
      procedure StatusMessage(const aData: string);
    public
      constructor Create;
      destructor Destroy; override;

      procedure Start;
      procedure Stop;
      procedure UpdateState;
      property OnMessage: TStringEvent write fOnMessage;
  end;

implementation
        
constructor TKMDedicatedServer.Create;
begin
  Inherited;
  fNetServer := TKMNetServer.Create;
  fLastPing := 0;
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
  fNetServer.StartListening(KAM_PORT);
end;

procedure TKMDedicatedServer.Stop;
begin
  fNetServer.StopListening;
  fNetServer.ClearClients;
  StatusMessage('Stopped listening');
end;

procedure TKMDedicatedServer.UpdateState;
begin
  if GetTickCount-fLastPing >= 1000 then
  begin
    fNetServer.MeasurePings;
    fLastPing := GetTickCount
  end;
end;

procedure TKMDedicatedServer.StatusMessage(const aData: string);
begin
  if Assigned(fOnMessage) then fOnMessage(aData);
end;

end.
