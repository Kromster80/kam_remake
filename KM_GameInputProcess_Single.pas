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
  end;


implementation
uses KM_Game, KM_Defaults, KM_Utils;


procedure TGameInputProcess_Single.TakeCommand(aCommand:TGameInputCommand);
begin
  ExecCommand(aCommand); //Execute the command now
  StoreCommand(aCommand); //Store the command for the replay
end;


procedure TGameInputProcess_Single.ReplayTimer(aTick:cardinal);
var MyRand:cardinal;
begin
  if fCursor > Count then 
    Exit; //There are no more commands left
    
  while (aTick > fQueue[fCursor].Tick) and (fQueue[fCursor].Command.CommandType <> gic_None) do
    inc(fCursor);

  while (aTick = fQueue[fCursor].Tick) do //Could be several commands in one Tick
  begin
    ExecCommand(fQueue[fCursor].Command);
    MyRand := Cardinal(Random(maxint)); //Just like in StoreCommand
    //CRC check after the command
    if CRASH_ON_REPLAY and (fQueue[fCursor].Rand <> MyRand) then //Should always be called to maintain randoms flow
    begin
      fGame.GameError(KMPoint(0,0), 'Replay mismatch: '+IntToStr(fQueue[fCursor].Rand)+' on tick '+IntToStr(aTick));
      Exit; //GameError sometimes calls GIP.Free, so exit immidiately
    end;
    inc(fCursor);
  end;
end;


end.
