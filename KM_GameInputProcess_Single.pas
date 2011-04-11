unit KM_GameInputProcess_Single;
{$I KaM_Remake.inc}
interface
uses KM_GameInputProcess;


type
  TGameInputProcess_Single = class(TGameInputProcess)
  protected
    procedure TakeCommand(aCommand:TGameInputCommand); override;
  public
    procedure Timer(aTick:cardinal); override;
  end;


implementation


procedure TGameInputProcess_Single.TakeCommand(aCommand:TGameInputCommand);
begin
  ExecCommand(aCommand); //Execute the command now
  StoreCommand(aCommand); //Store the command for the replay
end;


procedure TGameInputProcess_Single.Timer(aTick:cardinal);
begin
  Random(maxint); //thats our CRC used in Multiplayer. We do it here to maintain replay compatibility
end;


end.
