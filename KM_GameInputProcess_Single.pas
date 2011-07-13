unit KM_GameInputProcess_Single;
{$I KaM_Remake.inc}
interface
uses SysUtils, KM_GameInputProcess;


type
  TGameInputProcess_Single = class(TGameInputProcess)
  protected
    procedure TakeCommand(aCommand:TGameInputCommand); override;
  public
    procedure ReplayTimer(aTick:cardinal); override;
    procedure RunningTimer(aTick:cardinal); override;
  end;


implementation
uses KM_Game, KM_Defaults, KM_Utils;


procedure TGameInputProcess_Single.TakeCommand(aCommand:TGameInputCommand);
begin
  StoreCommand(aCommand); //Store the command for the replay (store it first in case Exec crashes and we want to debug it)
  ExecCommand(aCommand); //Execute the command now
end;


procedure TGameInputProcess_Single.ReplayTimer(aTick:cardinal);
var MyRand:cardinal;
begin
  Random(maxint); //This is to match up with multiplayer random check generation, so multiplayer replays can be replayed in singleplayer mode
  //There are still more commands left
  if fCursor <= Count then
  begin
    while (aTick > fQueue[fCursor].Tick) and (fQueue[fCursor].Command.CommandType <> gic_None) do
      inc(fCursor);

    while (aTick = fQueue[fCursor].Tick) do //Could be several commands in one Tick
    begin
      MyRand := Cardinal(Random(maxint)); //Just like in StoreCommand
      ExecCommand(fQueue[fCursor].Command);
      //CRC check after the command
      if CRASH_ON_REPLAY and (fQueue[fCursor].Rand <> MyRand) then //Should always be called to maintain randoms flow
      begin
        fGame.GameError(KMPoint(0,0), 'Replay mismatch: '+IntToStr(fQueue[fCursor].Rand)+' on tick '+IntToStr(aTick));
        Exit; //GameError sometimes calls GIP.Free, so exit immidiately
      end;
      inc(fCursor);
    end;
  end;
end;


procedure TGameInputProcess_Single.RunningTimer(aTick:cardinal);
begin
  Random(maxint); //This is to match up with multiplayer CRC generation, so multiplayer replays can be replayed in singleplayer mode
end;

end.
