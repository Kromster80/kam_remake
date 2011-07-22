unit KM_DedicatedServer;
interface
uses
  SysUtils,
  {$IFDEF MSWindows}Windows,{$ENDIF}
  KM_NetServer, KM_Defaults, KM_CommonTypes;


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
  uses KM_Utils; //Needed in Linux for FakeGetTickCount


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
  TickCount := {$IFDEF MSWindows}GetTickCount{$ENDIF}
               {$IFDEF Unix} FakeGetTickCount{$ENDIF};
  fNetServer.UpdateStateIdle;
  if TickCount-fLastPing >= 1000 then
  begin
    fNetServer.MeasurePings;
    fLastPing := TickCount;
  end;
end;


procedure TKMDedicatedServer.StatusMessage(const aData: string);
begin
  if Assigned(fOnMessage) then fOnMessage(aData);
end;


end.
