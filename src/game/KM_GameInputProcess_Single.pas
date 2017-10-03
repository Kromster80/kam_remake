unit KM_GameInputProcess_Single;
{$I KaM_Remake.inc}
interface
uses
  KM_GameInputProcess;


type
  TGameInputProcess_Single = class(TGameInputProcess)
  protected
    procedure TakeCommand(aCommand: TGameInputCommand); override;
  public
    procedure ReplayTimer(aTick: Cardinal); override;
    procedure RunningTimer(aTick: Cardinal); override;
  end;


implementation
uses
  KM_Game, KM_Defaults, KM_CommonUtils;


procedure TGameInputProcess_Single.TakeCommand(aCommand: TGameInputCommand);
begin
  if gGame.GameMode in [gmReplaySingle, gmReplayMulti] then Exit;

  StoreCommand(aCommand); //Store the command for the replay (store it first in case Exec crashes and we want to debug it)
  ExecCommand(aCommand);  //Execute the command now
end;


procedure TGameInputProcess_Single.ReplayTimer(aTick: Cardinal);
var MyRand: Cardinal;
begin
  KaMRandom(MaxInt); //This is to match up with multiplayer random check generation, so multiplayer replays can be replayed in singleplayer mode
  //There are still more commands left
  if fCursor <= Count then
  begin
    while (aTick > fQueue[fCursor].Tick) and (fQueue[fCursor].Command.CommandType <> gic_None) do
      Inc(fCursor);

    while (fCursor <= Count) and (aTick = fQueue[fCursor].Tick) do //Could be several commands in one Tick
    begin
      MyRand := Cardinal(KaMRandom(maxint)); //Just like in StoreCommand
      ExecCommand(fQueue[fCursor].Command);
      //CRC check after the command
      if CRASH_ON_REPLAY and (fQueue[fCursor].Rand <> MyRand) then //Should always be called to maintain randoms flow
      begin
        Inc(fCursor); //Must be done before exiting in case user decides to continue the replay
        gGame.ReplayInconsistancy;
        Exit; //ReplayInconsistancy sometimes calls GIP.Free, so exit immidiately
      end;
      Inc(fCursor);
    end;
  end;
end;


procedure TGameInputProcess_Single.RunningTimer(aTick: Cardinal);
begin
  inherited;

  KaMRandom(MaxInt); //This is to match up with multiplayer CRC generation, so multiplayer replays can be replayed in singleplayer mode
end;

end.
