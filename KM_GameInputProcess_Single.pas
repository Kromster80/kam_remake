unit KM_GameInputProcess_Single;
{$I KaM_Remake.inc}
interface
uses KM_GameInputProcess;


type
  TGameInputProcess_Single = class(TGameInputProcess)
    protected
      procedure TakeCommand(aCommand:TGameInputCommand); override;
    public
  end;


implementation


procedure TGameInputProcess_Single.TakeCommand(aCommand:TGameInputCommand);
begin
  ExecCommand(aCommand); //Execute the command now
  StoreCommand(aCommand); //Store the command for the replay
end;


end.
