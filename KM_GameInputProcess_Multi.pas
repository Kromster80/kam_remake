unit KM_GameInputProcess_Multi;
{$I KaM_Remake.inc}
interface
uses KM_GameInputProcess, KM_Network;


type
  TGameInputProcess_Multi = class(TGameInputProcess)
    private
      KMNetwork: TKMNetwork;
    protected  
      procedure TakeCommand(aCommand:TGameInputCommand); override;
    public
      constructor Create(aReplayState:TGIPReplayState);
      destructor Destroy; override;
      procedure Tick(aTick:cardinal); override;
  end;


implementation


constructor TGameInputProcess_Multi.Create(aReplayState:TGIPReplayState);
begin
  Inherited Create(aReplayState);
end;


destructor TGameInputProcess_Multi.Destroy;
begin
  Inherited;
end;


procedure TGameInputProcess_Multi.TakeCommand(aCommand:TGameInputCommand);
begin

end;


procedure TGameInputProcess_Multi.Tick(aTick:cardinal);
begin
  Inherited;
end;

end.
