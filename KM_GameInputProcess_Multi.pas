unit KM_GameInputProcess_Multi;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, Math, KromUtils, KM_GameInputProcess, KM_Networking, KM_Defaults, KM_CommonTypes, KM_Player;

const
  MAX_SCHEDULE = 100; //Size of ring buffers (10 sec) Make them large so overruns do not occur
  DELAY_ADJUST = 40; //How often to adjust fDelay (every 4 seconds) This must be higher than MAX_DELAY
  MAX_DELAY = 32; //Maximum number of ticks (3.2 sec) to plan ahead (highest value fDelay can take)

type
  TKMDataType = (kdp_Commands, kdp_RandomCheck);

  TCommandsPack = class
  private
    fCount:integer;
    fItems:array of TGameInputCommand; //1..n
    function GetItem(aIndex:integer):TGameInputCommand;
  public
    property  Count:integer read fCount;
    procedure Clear;
    procedure Add(aCommand:TGameInputCommand);
    function CRC:cardinal;
    property Items[aIndex:integer]:TGameInputCommand read GetItem;
    procedure Save(aStream:TKMemoryStream);
    procedure Load(aStream:TKMemoryStream);
  end;

  TRandomCheck = record
    OurCheck: cardinal;
    PlayerCheck:array[TPlayerIndex] of cardinal;
    PlayerWasChecked:array[TPlayerIndex] of boolean;
  end;

  TGameInputProcess_Multi = class (TGameInputProcess)
  private
    fNetworking:TKMNetworking;
    fDelay:word; //How many ticks ahead the commands are scheduled

    fNumberConsecutiveWaits:word; //Number of consecutive times we have been waiting for network

    //Each player can have any number of commands scheduled for execution in one tick
    fSchedule:array[0..MAX_SCHEDULE-1, TPlayerIndex] of TCommandsPack; //Ring buffer

    //All players must send us data every tick
    fRecievedData:array[0..MAX_SCHEDULE-1, TPlayerIndex] of boolean; //Ring buffer

    //Mark commands we've already sent to other players
    fSent:array[0..MAX_SCHEDULE-1] of boolean; //Ring buffer

    //Store random seeds at each tick then confirm with other players
    fRandomCheck:array[0..MAX_SCHEDULE-1] of TRandomCheck; //Ring buffer

    procedure SendCommands(aTick:cardinal);
    procedure SendRandomCheck(aTick:cardinal);
    procedure DoRandomCheck(aTick:cardinal; aPlayerIndex:TPlayerIndex);

    procedure SetDelay(aNewDelay:integer);
    procedure AdjustDelay;
  protected
    procedure TakeCommand(aCommand:TGameInputCommand); override;
  public
    constructor Create(aReplayState:TGIPReplayState; aNetworking:TKMNetworking);
    destructor Destroy; override;
    procedure WaitingForConfirmation(aTick:cardinal); override;
    function GetNetworkDelay:word;
    property GetNumberConsecutiveWaits:word read fNumberConsecutiveWaits;
    procedure GetWaitingPlayers(aTick:cardinal; aPlayersList:TStringList);
    procedure RecieveCommands(const aData:string); //Called by TKMNetwork when it has data for us
    function CommandsConfirmed(aTick:cardinal):boolean; override;
    procedure RunningTimer(aTick:cardinal); override;
    procedure UpdateState(aTick:cardinal); override;
  end;


implementation
uses KM_Game, KM_PlayersCollection, KM_NetPlayersList, KM_Utils;


{ TCommandsPack }
procedure TCommandsPack.Clear;
begin
  fCount := 0;
end;


procedure TCommandsPack.Add(aCommand:TGameInputCommand);
begin
  inc(fCount);
  if fCount >= length(fItems) then
    SetLength(fItems, fCount+8);

  fItems[fCount] := aCommand;
end;


function TCommandsPack.GetItem(aIndex:integer):TGameInputCommand;
begin
  Result := fItems[aIndex];
end;


//Return CRC of the pack
function TCommandsPack.CRC:cardinal;
var i:integer;
begin
  Result := 0;    
  for i:=1 to fCount do
    Result := Result xor Adler32CRC(@fItems[i], SizeOf(fItems[i]))
end;


procedure TCommandsPack.Save(aStream:TKMemoryStream);
var i:integer;
begin
  aStream.Write(fCount);
  for i:=1 to fCount do
    aStream.Write(fItems[i], SizeOf(fItems[i]));
end;


procedure TCommandsPack.Load(aStream:TKMemoryStream);
var i:integer;
begin
  aStream.Read(fCount);
  SetLength(fItems, fCount+1);

  for i:=1 to fCount do
    aStream.Read(fItems[i], SizeOf(fItems[i]));
end;


{ TGameInputProcess_Multi }
constructor TGameInputProcess_Multi.Create(aReplayState:TGIPReplayState; aNetworking:TKMNetworking);
var i:integer; k:TPlayerIndex;
begin
  Inherited Create(aReplayState);
  fNetworking := aNetworking;
  fNetworking.OnCommands := RecieveCommands;
  AdjustDelay; //Initialise the delay

  //Allocate memory for all commands packs
  for i:=0 to MAX_SCHEDULE-1 do for k:=0 to MAX_PLAYERS-1 do
  begin
    fSchedule[i,k] := TCommandsPack.Create;
    fRandomCheck[i].PlayerWasChecked[k] := true; //We don't have anything to be checked yet
  end;
end;


destructor TGameInputProcess_Multi.Destroy;
var i:integer; k:TPlayerIndex;
begin
  for i:=0 to MAX_SCHEDULE-1 do for k:=0 to fPlayers.Count do
    fSchedule[i,k].Free;
  Inherited;
end;


//Stack the command into schedule
procedure TGameInputProcess_Multi.TakeCommand(aCommand:TGameInputCommand);
var i,Tick:Cardinal;
begin
  Assert(fDelay < MAX_SCHEDULE, 'Error, fDelay >= MAX_SCHEDULE');

  //Find first unsent pack
  Tick := MAX_SCHEDULE; //Out of range value
  for i:=fGame.GameTickCount + fDelay to fGame.GameTickCount + MAX_SCHEDULE-1 do
  if not fSent[i mod MAX_SCHEDULE] then
  begin
    Tick := i mod MAX_SCHEDULE; //Place in a ring buffer
    Break;
  end;
  Assert(Tick < MAX_SCHEDULE, 'Could not find place for new commands');

  fSchedule[Tick, aCommand.PlayerIndex].Add(aCommand);
end;


procedure TGameInputProcess_Multi.WaitingForConfirmation(aTick:cardinal);
begin
  //This is a notification that the game is waiting for a tick to be ready
  inc(fNumberConsecutiveWaits);
  //Mostly unused at the moment, could be used later for e.g. better fDelay calculation.
end;


function TGameInputProcess_Multi.GetNetworkDelay:word;
begin
  Result := fDelay;
end;


procedure TGameInputProcess_Multi.SetDelay(aNewDelay:integer);
begin
  fDelay := EnsureRange(aNewDelay,1,MAX_DELAY);
end;


procedure TGameInputProcess_Multi.AdjustDelay;
begin
  //Half of the maximum round trip is a good guess for delay.
  //This could be improved by averaging the latency rather than using the instantanious measurement
  SetDelay( Ceil((fNetworking.NetPlayers.GetHighestRoundTripLatency+10)/200) );
end;


procedure TGameInputProcess_Multi.SendCommands(aTick:cardinal);
var Msg:TKMemoryStream;
begin
  Msg := TKMemoryStream.Create;
  try
    Msg.Write(byte(kdp_Commands));
    Msg.Write(aTick); //Target Tick in 1..n range
    Msg.Write(MyPlayer.PlayerIndex, SizeOf(MyPlayer.PlayerIndex));
    fSchedule[aTick mod MAX_SCHEDULE, MyPlayer.PlayerIndex].Save(Msg); //Write all commands to the stream
    fNetworking.SendCommands(Msg); //Send to all opponents
  finally
    Msg.Free;
  end;
end;


procedure TGameInputProcess_Multi.SendRandomCheck(aTick:cardinal);
var Msg:TKMemoryStream;
begin
  Msg := TKMemoryStream.Create;
  try
    Msg.Write(byte(kdp_RandomCheck));
    Msg.Write(aTick); //Target Tick in 1..n range
    Msg.Write(MyPlayer.PlayerIndex, SizeOf(MyPlayer.PlayerIndex));
    Msg.Write(fRandomCheck[aTick mod MAX_SCHEDULE].OurCheck); //Write our random check to the stream
    fNetworking.SendCommands(Msg); //Send to all opponents
  finally
    Msg.Free;
  end;
end;


procedure TGameInputProcess_Multi.DoRandomCheck(aTick:cardinal; aPlayerIndex:TPlayerIndex);
begin
  with fRandomCheck[aTick mod MAX_SCHEDULE] do
  begin
    Assert(OurCheck = PlayerCheck[aPlayerIndex],Format('Random check mismatch for tick %d from player %d processed at tick %d',
                                                       [aTick, aPlayerIndex, fGame.GameTickCount]));
    PlayerWasChecked[aPlayerIndex] := true;
  end;
end;


//Decode recieved messages (Commands from other players, Confirmations, Errors)
procedure TGameInputProcess_Multi.RecieveCommands(const aData:string);
var M:TKMemoryStream; D:TKMDataType; Tick:Cardinal; PlayerIndex:TPlayerIndex; CRC:cardinal;
begin
  M := TKMemoryStream.Create;
  try
    M.WriteAsText(aData);
    M.Read(D, 1); //Decode header
    M.Read(Tick); //Target tick
    M.Read(PlayerIndex, SizeOf(PlayerIndex)); //Message sender

    case D of
      kdp_Commands:
          begin
            Assert(Tick > fGame.GameTickCount, Format('Commands for tick %d from player %d recieved too late at %d',
                                                     [Tick, PlayerIndex, fGame.GameTickCount]));
            fSchedule[Tick mod MAX_SCHEDULE, PlayerIndex].Load(M);
            fRecievedData[Tick mod MAX_SCHEDULE, PlayerIndex] := true;
          end;
      kdp_RandomCheck: //Other player is confirming that random seeds matched at a tick in the past
          begin
            M.Read(CRC); //Read the random check from the message
            fRandomCheck[Tick mod MAX_SCHEDULE].PlayerCheck[PlayerIndex] := CRC; //Store it for this player
            fRandomCheck[Tick mod MAX_SCHEDULE].PlayerWasChecked[PlayerIndex] := false;
            //If we have processed this tick already, check now
            if Tick <= fGame.GameTickCount then
              DoRandomCheck(Tick, PlayerIndex);
          end;
    end;
  finally
    M.Free;
  end;
end;


//Are all the commands are confirmed?
function TGameInputProcess_Multi.CommandsConfirmed(aTick:cardinal):boolean;
var i:integer;
begin
  Result := True;
  for i:=1 to fNetworking.NetPlayers.Count do
    Result := Result and (fRecievedData[aTick mod MAX_SCHEDULE, fNetworking.NetPlayers[i].PlayerIndex.PlayerIndex] or
                         (fNetworking.NetPlayers[i].PlayerType = pt_Computer) or not fNetworking.NetPlayers[i].Alive);
end;


procedure TGameInputProcess_Multi.GetWaitingPlayers(aTick:cardinal; aPlayersList:TStringList);
var i: integer;
begin
  for i:=1 to fNetworking.NetPlayers.Count do
    if not (fRecievedData[aTick mod MAX_SCHEDULE, fNetworking.NetPlayers[i].PlayerIndex.PlayerIndex] or
           (fNetworking.NetPlayers[i].PlayerType = pt_Computer) or not fNetworking.NetPlayers[i].Alive) then
      aPlayersList.Add(fNetworking.NetPlayers[i].Nikname);
end;


//Timer is called after all commands from player are taken,
//upcoming commands will be stacked into next batch
procedure TGameInputProcess_Multi.RunningTimer(aTick:cardinal);
var i,k,Tick:Cardinal;
begin
  fNumberConsecutiveWaits := 0; //We are not waiting if the game is running
  Tick := aTick mod MAX_SCHEDULE; //Place in a ring buffer
  fRandomCheck[Tick].OurCheck := cardinal(KaMRandom(maxint)); //thats our CRC (must go before commands for replay compatibility)

  //Execute commands, in order players go (1,2,3..)
  for i:=0 to fPlayers.Count-1 do
    for k:=1 to fSchedule[Tick, i].Count do
    begin
      //if fNetworking.NetPlayers[i].Alive then //todo: Skip dead players
      begin
        StoreCommand(fSchedule[Tick, i].Items[k]); //Store the command first so if Exec fails we still have it in the replay
        ExecCommand(fSchedule[Tick, i].Items[k]);
      end;
      fSchedule[Tick, i].Clear;
    end;

  SendRandomCheck(aTick);
  //It is possible that we have already recieved other player's random checks, if so check them now
  for i:=0 to fPlayers.Count-1 do
    if not fRandomCheck[Tick].PlayerWasChecked[i] then
      DoRandomCheck(aTick, i);

  FillChar(fRecievedData[Tick], SizeOf(fRecievedData[Tick]), #0); //Reset
  fSent[Tick] := false;

  if aTick mod DELAY_ADJUST = 0 then AdjustDelay; //Adjust fDelay every X ticks
end;


procedure TGameInputProcess_Multi.UpdateState(aTick:cardinal);
var i:integer;
begin
  for i:=aTick+1 to aTick+fDelay do
  if not fSent[i mod MAX_SCHEDULE] then
  begin
    SendCommands(i);
    fSent[i mod MAX_SCHEDULE] := true;
    fRecievedData[i mod MAX_SCHEDULE, MyPlayer.PlayerIndex] := true; //Recieved commands from self
  end;
end;


end.
