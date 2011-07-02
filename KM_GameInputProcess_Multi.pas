unit KM_GameInputProcess_Multi;
{$I KaM_Remake.inc}
interface
uses SysUtils, KromUtils, KM_GameInputProcess, KM_Networking, KM_Defaults, KM_CommonTypes, KM_Player;

const
  MAX_SCHEDULE = 32; //How many turns to plan ahead (3.2sec)

type
  TKMDataType = (kdp_Commands, kdp_Confirmation);

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

  TGameInputProcess_Multi = class (TGameInputProcess)
  private
    fNetworking:TKMNetworking;
    fDelay:word; //How many ticks ahead the commands are scheduled

    //Each player can have any number of commands scheduled for execution in one tick
    fSchedule:array[0..MAX_SCHEDULE-1, TPlayerIndex] of TCommandsPack; //Ring buffer

    //All players must confirm they have recieved our GIPList
    fConfirmation:array[0..MAX_SCHEDULE-1, TPlayerIndex] of boolean; //Ring buffer

    //Mark commands we've already sent to other players
    fSent:array[0..MAX_SCHEDULE-1] of boolean; //Ring buffer

    procedure SendCommands(aTick:cardinal);
    procedure SendConfirmation(aTick:cardinal; aPlayerIndex:TPlayerIndex);
  protected
    procedure TakeCommand(aCommand:TGameInputCommand); override;
  public
    constructor Create(aReplayState:TGIPReplayState; aNetworking:TKMNetworking);
    destructor Destroy; override;
    procedure RecieveCommands(const aData:string); //Called by TKMNetwork when it has data for us
    function CommandsConfirmed(aTick:cardinal):boolean; override;
    procedure RunningTimer(aTick:cardinal); override;
    procedure UpdateState(aTick:cardinal); override;
  end;


implementation
uses KM_Game, KM_PlayersCollection, KM_NetPlayersList;


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
  fDelay := 10; //1sec

  //Allocate memory for all commands packs
  for i:=0 to MAX_SCHEDULE-1 do for k:=0 to fPlayers.Count do
    fSchedule[i,k] := TCommandsPack.Create;
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
var i:cardinal; Tick:integer;
begin
  Assert(fDelay < MAX_SCHEDULE, 'Error, fDelay >= MAX_SCHEDULE');

  //Find first unsent pack
  Tick := -1;
  for i:=fGame.GameTickCount + fDelay to fGame.GameTickCount + MAX_SCHEDULE-1 do
  if not fSent[i mod MAX_SCHEDULE] then
  begin
    Tick := i mod MAX_SCHEDULE; //Place in a ring buffer
    Break;
  end;
  Assert(Tick<>-1, 'Could not find place for new commands');

  fSchedule[Tick, aCommand.PlayerIndex].Add(aCommand);
  FillChar(fConfirmation[Tick], SizeOf(fConfirmation[Tick]), #0); //Reset to false
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


//Confirm that we have recieved the commands with CRC
procedure TGameInputProcess_Multi.SendConfirmation(aTick:cardinal; aPlayerIndex:TPlayerIndex);
var Msg:TKMemoryStream;
begin
  Msg := TKMemoryStream.Create;
  try
    Msg.Write(byte(kdp_Confirmation));
    Msg.Write(aTick); //Target Tick in 1..n range
    Msg.Write(MyPlayer.PlayerIndex, SizeOf(MyPlayer.PlayerIndex));
    Msg.Write(fSchedule[aTick mod MAX_SCHEDULE, aPlayerIndex].CRC);
    fNetworking.SendCommands(Msg, aPlayerIndex); //Send to opponent
  finally
    Msg.Free;
  end;
end;


//Decode recieved messages (Commands from other players, Confirmations, Errors)
procedure TGameInputProcess_Multi.RecieveCommands(const aData:string);
var M:TKMemoryStream; D:TKMDataType; Tick:integer; PlayerIndex:TPlayerIndex; CRC:cardinal;
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
            fSchedule[Tick mod MAX_SCHEDULE, PlayerIndex].Load(M);
            SendConfirmation(Tick, PlayerIndex);
          end;
      kdp_Confirmation: //Recieved CRC should match our commands pack
          begin
            M.Read(CRC);
            Assert(CRC = fSchedule[Tick mod MAX_SCHEDULE, MyPlayer.PlayerIndex].CRC);
            fConfirmation[Tick mod MAX_SCHEDULE, PlayerIndex] := true;
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
    Result := Result and (fConfirmation[aTick mod MAX_SCHEDULE, fNetworking.NetPlayers[i].PlayerIndex.PlayerIndex] or
                         (fNetworking.NetPlayers[i].PlayerType = pt_Computer) or not fNetworking.NetPlayers[i].Alive);
end;


//Timer is called after all commands from player are taken,
//upcoming commands will be stacked into next batch
procedure TGameInputProcess_Multi.RunningTimer(aTick:cardinal);
var i,k,Tick:integer;
begin
  Random(maxint); //thats our CRC
  //todo: Remember CRC and do the CRC checking once in a while

  Tick := aTick mod MAX_SCHEDULE; //Place in a ring buffer

  //Execute commands, in order players go (1,2,3..)
  for i:=0 to fPlayers.Count-1 do
    for k:=1 to fSchedule[Tick, i].Count do
    begin
      //if fNetworking.NetPlayers[i].Alive then //todo: Skip dead players
      begin
        ExecCommand(fSchedule[Tick, i].Items[k]);
        StoreCommand(fSchedule[Tick, i].Items[k]);
      end;
      fSchedule[Tick, i].Clear;
    end;

  FillChar(fConfirmation[Tick], SizeOf(fConfirmation[Tick]), #0); //Reset
  fSent[Tick] := false;
end;


//todo: Check CRCs
procedure TGameInputProcess_Multi.UpdateState(aTick:cardinal);
var i:integer;
begin
  //if not CRC = CRC then
  //  fOnCRCFail(Self);

  for i:=aTick+1 to aTick+fDelay do
  if not fSent[i mod MAX_SCHEDULE] then
  begin
    SendCommands(i);
    fSent[i mod MAX_SCHEDULE] := true;
    fConfirmation[i mod MAX_SCHEDULE, MyPlayer.PlayerIndex] := true; //Self confirmed
  end;
end;


end.
