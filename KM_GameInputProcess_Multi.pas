unit KM_GameInputProcess_Multi;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, Math, KromUtils, KM_GameInputProcess, KM_Networking, KM_Defaults, KM_CommonClasses;

const
  MAX_SCHEDULE = 100; //Size of ring buffers (10 sec) Make them large so overruns do not occur
  DELAY_ADJUST = 40; //How often to adjust fDelay (every 4 seconds) This must be higher than MAX_DELAY
  MIN_DELAY = 2; //A delay of 1 is not possible because that means the command shall be processed on the next tick after it was issued, but that could be 0.0001ms after the player clicks, meaning there is no way the command would have been sent. Therefore the delay must be 2 at minimum.
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
    PlayerCheckPending:array[TPlayerIndex] of boolean;
  end;

  TGameInputProcess_Multi = class (TGameInputProcess)
  private
    fNetworking:TKMNetworking;
    fDelay:word; //How many ticks ahead the commands are scheduled
    fLastSentTick:cardinal; //Needed for resync

    fNumberConsecutiveWaits:word; //Number of consecutive times we have been waiting for network

    //Each player can have any number of commands scheduled for execution in one tick
    fSchedule:array[0..MAX_SCHEDULE-1, TPlayerIndex] of TCommandsPack; //Ring buffer

    //All players must send us data every tick
    fRecievedData:array[0..MAX_SCHEDULE-1, TPlayerIndex] of boolean; //Ring buffer

    //Mark commands we've already sent to other players
    fSent:array[0..MAX_SCHEDULE-1] of boolean; //Ring buffer

    //Did the player issue a command for this tick? If not it must be cleared from last time (we can't clear it earlier as it might be needed for resync)
    fCommandIssued:array[0..MAX_SCHEDULE-1] of boolean;

    //Store random seeds at each tick then confirm with other players
    fRandomCheck:array[0..MAX_SCHEDULE-1] of TRandomCheck; //Ring buffer

    procedure SendCommands(aTick:cardinal; aPlayerIndex:TPlayerIndex=-1);
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
    procedure ResyncFromTick(aSender:Integer; aTick:cardinal);
    function CommandsConfirmed(aTick:cardinal):boolean; override;
    procedure RunningTimer(aTick:cardinal); override;
    procedure UpdateState(aTick:cardinal); override;
  end;


implementation
uses KM_Game, KM_PlayersCollection, KM_Utils, KM_Sound, KM_TextLibrary;


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
    SaveCommandToMemoryStream(fItems[i], aStream);
end;


procedure TCommandsPack.Load(aStream:TKMemoryStream);
var i:integer;
begin
  aStream.Read(fCount);
  SetLength(fItems, fCount+1);

  for i:=1 to fCount do
    LoadCommandFromMemoryStream(fItems[i], aStream);
end;


{ TGameInputProcess_Multi }
constructor TGameInputProcess_Multi.Create(aReplayState:TGIPReplayState; aNetworking:TKMNetworking);
var i:integer; k:TPlayerIndex;
begin
  Inherited Create(aReplayState);
  fNetworking := aNetworking;
  fNetworking.OnCommands := RecieveCommands;
  fNetworking.OnResyncFromTick := ResyncFromTick;
  AdjustDelay; //Initialise the delay

  //Allocate memory for all commands packs
  for i:=0 to MAX_SCHEDULE-1 do for k:=0 to MAX_PLAYERS-1 do
  begin
    fSchedule[i,k] := TCommandsPack.Create;
    fRandomCheck[i].PlayerCheckPending[k] := false; //We don't have anything to be checked yet
  end;
end;


destructor TGameInputProcess_Multi.Destroy;
var i:integer; k:TPlayerIndex;
begin
  for i:=0 to MAX_SCHEDULE-1 do for k:=0 to MAX_PLAYERS-1 do
    fSchedule[i,k].Free;
  Inherited;
end;


//Stack the command into schedule
procedure TGameInputProcess_Multi.TakeCommand(aCommand:TGameInputCommand);
var i,Tick:Cardinal;
begin
  Assert(fDelay < MAX_SCHEDULE, 'Error, fDelay >= MAX_SCHEDULE');

  if fGame.IsPeaceTime and (aCommand.CommandType in BlockedByPeaceTime) then
  begin
    fGame.Networking.PostLocalMessage(fTextLibrary[TX_MP_BLOCKED_BY_PEACETIME],false);
    fSoundLib.Play(sfx_CantPlace);
    exit;
  end;

  //Find first unsent pack
  Tick := MAX_SCHEDULE; //Out of range value
  for i:=fGame.GameTickCount + fDelay to fGame.GameTickCount + MAX_SCHEDULE-1 do
  if not fSent[i mod MAX_SCHEDULE] then
  begin
    Tick := i mod MAX_SCHEDULE; //Place in a ring buffer
    Break;
  end;
  Assert(Tick < MAX_SCHEDULE, 'Could not find place for new commands');

  if not fCommandIssued[Tick] then
  begin
    fSchedule[Tick, aCommand.PlayerIndex].Clear; //Clear old data (it was kept in case it was required for resync)
    fCommandIssued[Tick] := true;
  end;
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
  fDelay := EnsureRange(aNewDelay,MIN_DELAY,MAX_DELAY);
end;


procedure TGameInputProcess_Multi.AdjustDelay;
begin
  //Half of the maximum round trip is a good guess for delay.
  //This could be improved by averaging the latency rather than using the instantanious measurement
  SetDelay( Ceil((fNetworking.NetPlayers.GetMaxHighestRoundTripLatency+10)/200) );
end;


procedure TGameInputProcess_Multi.SendCommands(aTick:cardinal; aPlayerIndex:TPlayerIndex=-1);
var Msg:TKMemoryStream;
begin
  Msg := TKMemoryStream.Create;
  try
    Msg.Write(byte(kdp_Commands));
    Msg.Write(aTick); //Target Tick in 1..n range
    Msg.Write(MyPlayer.PlayerIndex, SizeOf(MyPlayer.PlayerIndex));
    fSchedule[aTick mod MAX_SCHEDULE, MyPlayer.PlayerIndex].Save(Msg); //Write all commands to the stream
    fNetworking.SendCommands(Msg, aPlayerIndex); //Send to all players by default
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
    PlayerCheckPending[aPlayerIndex] := false;
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
            //Recieving commands too late will happen during reconnections, so just ignore it
            if Tick > fGame.GameTickCount then
            begin
              fSchedule[Tick mod MAX_SCHEDULE, PlayerIndex].Load(M);
              fRecievedData[Tick mod MAX_SCHEDULE, PlayerIndex] := true;
            end;
          end;
      kdp_RandomCheck: //Other player is confirming that random seeds matched at a tick in the past
          begin
            M.Read(CRC); //Read the random check from the message
            fRandomCheck[Tick mod MAX_SCHEDULE].PlayerCheck[PlayerIndex] := CRC; //Store it for this player
            fRandomCheck[Tick mod MAX_SCHEDULE].PlayerCheckPending[PlayerIndex] := true;
            //If we have processed this tick already, check now
            if Tick <= fGame.GameTickCount then
              DoRandomCheck(Tick, PlayerIndex);
          end;
    end;
  finally
    M.Free;
  end;
end;


//We must resend the commands from aTick to the last sent tick to the specified player
procedure TGameInputProcess_Multi.ResyncFromTick(aSender:Integer; aTick:cardinal);
var i:cardinal;
begin
  for i:=aTick to fLastSentTick do SendCommands(i, aSender);
end;


//Are all the commands are confirmed?
function TGameInputProcess_Multi.CommandsConfirmed(aTick:cardinal):boolean;
var i:integer;
begin
  Result := True;
  for i:=1 to fNetworking.NetPlayers.Count do
    Result := Result and (fRecievedData[aTick mod MAX_SCHEDULE, fNetworking.NetPlayers[i].PlayerIndex.PlayerIndex] or
                         (not fNetworking.NetPlayers[i].IsHuman) or fNetworking.NetPlayers[i].Dropped);
end;


procedure TGameInputProcess_Multi.GetWaitingPlayers(aTick:cardinal; aPlayersList:TStringList);
var i: integer;
begin
  for i:=1 to fNetworking.NetPlayers.Count do
    if not (fRecievedData[aTick mod MAX_SCHEDULE, fNetworking.NetPlayers[i].PlayerIndex.PlayerIndex] or
           (not fNetworking.NetPlayers[i].IsHuman) or fNetworking.NetPlayers[i].Dropped) then
      aPlayersList.Add(fNetworking.NetPlayers[i].Nikname);
end;


//Timer is called after all commands from player are taken,
//upcoming commands will be stacked into next batch
procedure TGameInputProcess_Multi.RunningTimer(aTick:cardinal);
var i,k,Tick:Cardinal; NetplayersIndex:integer;
begin
  fNumberConsecutiveWaits := 0; //We are not waiting if the game is running
  Tick := aTick mod MAX_SCHEDULE; //Place in a ring buffer
  fRandomCheck[Tick].OurCheck := cardinal(KaMRandom(maxint)); //thats our CRC (must go before commands for replay compatibility)

  //Execute commands, in order players go (1,2,3..)
  for i:=0 to fPlayers.Count-1 do
    for k:=1 to fSchedule[Tick, i].Count do
    begin
      NetplayersIndex := fNetworking.NetPlayers.PlayerIndexToLocal(i);
      // In the case where a player was removed from a save, NetplayersIndex = -1
      if (NetplayersIndex <> -1) and not fNetworking.NetPlayers[NetplayersIndex].Dropped then
      begin
        StoreCommand(fSchedule[Tick, i].Items[k]); //Store the command first so if Exec fails we still have it in the replay
        ExecCommand(fSchedule[Tick, i].Items[k]);
      end;
    end;

  //If we miss a few random checks during reconnections no one cares, inconsistencies will be detected as soon as it is over
  if fNetworking.Connected then SendRandomCheck(aTick);
  //It is possible that we have already recieved other player's random checks, if so check them now
  for i:=0 to fPlayers.Count-1 do
  begin
    NetplayersIndex := fNetworking.NetPlayers.PlayerIndexToLocal(i);
    // In the case where a player was removed from a save, NetplayersIndex = -1
    if (NetplayersIndex <> -1) and not fNetworking.NetPlayers[NetplayersIndex].Dropped
    and fRandomCheck[Tick].PlayerCheckPending[i] then
        DoRandomCheck(aTick, i);
  end;

  FillChar(fRecievedData[Tick], SizeOf(fRecievedData[Tick]), #0); //Reset
  fSent[Tick] := false;

  if aTick mod DELAY_ADJUST = 0 then AdjustDelay; //Adjust fDelay every X ticks
end;


procedure TGameInputProcess_Multi.UpdateState(aTick:cardinal);
var i:integer;
begin
  for i:=aTick+1 to aTick+fDelay do
  //If the network is not connected then we must send the commands later (fSent will remain false)
  if (not fSent[i mod MAX_SCHEDULE]) and (fNetworking.Connected) then
  begin
    if not fCommandIssued[i mod MAX_SCHEDULE] then
      fSchedule[i mod MAX_SCHEDULE, MyPlayer.PlayerIndex].Clear; //No one has used it since last time through the ring buffer
    fCommandIssued[i mod MAX_SCHEDULE] := false; //Make it as requiring clearing next time around

    fLastSentTick := i;
    SendCommands(i);
    fSent[i mod MAX_SCHEDULE] := true;
    fRecievedData[i mod MAX_SCHEDULE, MyPlayer.PlayerIndex] := true; //Recieved commands from self
  end;
end;


end.
