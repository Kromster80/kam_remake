unit KM_GameInputProcess_Multi;
{$I KaM_Remake.inc}
interface
uses SysUtils, KM_GameInputProcess, KM_Networking, KM_Defaults, KM_CommonTypes;

const
  MAX_SCHEDULE = 32; //How many turns to plan ahead (3.2sec)

type
  TKMDataType = (kdp_Commands, kdp_Confirmation);

  TCommandsPack = class
  private
    fCount:integer;
    fList:array of TGameInputCommand; //1..n
  public
    property  Count:integer read fCount;
    procedure Clear;
    procedure Add(aCommand:TGameInputCommand);
    function  Get(aIndex:integer):TGameInputCommand;
    function CRC:cardinal;
    procedure Save(aStream:TKMemoryStream);
    procedure Load(aStream:TKMemoryStream);
  end;

  TGameInputProcess_Multi = class (TGameInputProcess)
  private
    fNetworking:TKMNetworking;
    fDelay:word; //How many ticks ahead the commands are scheduled

    //Each player can have any number of commands scheduled for execution in one tick
    fSchedule:array[0..MAX_SCHEDULE-1, 1..MAX_PLAYERS] of TCommandsPack; //Ring buffer

    //All players must confirm they have recieved our GIPList
    fConfirmation:array[0..MAX_SCHEDULE-1, 1..MAX_PLAYERS] of boolean; //Ring buffer

    //Mark commands we've already sent to other players
    fSent:array[0..MAX_SCHEDULE-1] of boolean; //Ring buffer

    procedure SendCommands(aTick:cardinal);
    procedure SendConfirmation(aTick:cardinal; aPlayerLoc:byte);
  protected
    procedure TakeCommand(aCommand:TGameInputCommand); override;
  public
    constructor Create(aReplayState:TGIPReplayState; aNetworking:TKMNetworking);
    destructor Destroy; override;
    procedure RecieveCommands(const aData:string); //Called by TKMNetwork when it has data for us
    function CommandsConfirmed(aTick:cardinal):boolean; override;
    procedure Timer(aTick:cardinal); override;
    procedure UpdateState(aTick:cardinal); override;
  end;


implementation
uses KM_Game, KM_PlayersCollection;


{ TGIPList }
procedure TCommandsPack.Clear;
begin
  fCount := 0;
end;


procedure TCommandsPack.Add(aCommand:TGameInputCommand);
begin
  inc(fCount);
  if fCount >= length(fList) then
    setlength(fList, fCount+8);

  fList[fCount] := aCommand;
end;


function TCommandsPack.Get(aIndex:integer):TGameInputCommand;
begin
  Result := fList[aIndex];
end;


function TCommandsPack.CRC:cardinal;
begin
  //todo: return CRC of the pack
  Result := 0;
end;


procedure TCommandsPack.Save(aStream:TKMemoryStream);
var i:integer;
begin
  aStream.Write(fCount);
  for i:=1 to fCount do
  begin
    aStream.Write(fList[i].CommandType, SizeOf(fList[i].CommandType));
    aStream.Write(fList[i].Params[1]);
    aStream.Write(fList[i].Params[2]);
    aStream.Write(fList[i].Params[3]);
    aStream.Write(fList[i].Params[4]);
    //Skip Players ID
  end;               
end;


procedure TCommandsPack.Load(aStream:TKMemoryStream);
var i:integer;
begin
  aStream.Read(fCount);
  for i:=1 to fCount do
  begin
    aStream.Read(fList[i].CommandType, SizeOf(fList[i].CommandType));
    aStream.Read(fList[i].Params[1]);
    aStream.Read(fList[i].Params[2]);
    aStream.Read(fList[i].Params[3]);
    aStream.Read(fList[i].Params[4]);
    //Skip Players ID
  end;               
end;


{ TGameInputProcess_Multi }
constructor TGameInputProcess_Multi.Create(aReplayState:TGIPReplayState; aNetworking:TKMNetworking);
var i,k:integer;
begin
  Inherited Create(aReplayState);
  fNetworking := aNetworking;
  fNetworking.OnCommands := RecieveCommands;
  fDelay := 10; //1sec

  //Allocate memory for all commands packs
  for i:=0 to MAX_SCHEDULE-1 do for k:=1 to MAX_PLAYERS do
    fSchedule[i,k] := TCommandsPack.Create;
end;


destructor TGameInputProcess_Multi.Destroy;
var i,k:integer;
begin
  for i:=0 to MAX_SCHEDULE-1 do for k:=1 to MAX_PLAYERS do
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
  for i:=fDelay to MAX_SCHEDULE do
  if not fSent[(fGame.GameTickCount + i) mod MAX_SCHEDULE] then
  begin
    Tick := (fGame.GameTickCount + i) mod MAX_SCHEDULE; //Place in a ring buffer
    break;
  end;
  Assert(Tick<>-1, 'Could not find place for new commands');

  fSchedule[Tick, byte(aCommand.PlayerID)].Add(aCommand);
  FillChar(fConfirmation[Tick], SizeOf(fConfirmation[Tick]), #0); //Reset to false

  fConfirmation[Tick, byte(aCommand.PlayerID)] := true; //Self confirmed
end;


procedure TGameInputProcess_Multi.SendCommands(aTick:cardinal);
var Msg:TKMemoryStream;
begin
  Msg := TKMemoryStream.Create;
  try
    Msg.Write(byte(kdp_Commands));
    Msg.Write(aTick); //Target Tick in 1..n range
    Msg.Write(MyPlayer.PlayerID, SizeOf(MyPlayer.PlayerID));

    fSchedule[aTick mod MAX_SCHEDULE, byte(MyPlayer.PlayerID)].Save(Msg); //Write all commands to the stream
    fNetworking.SendCommands(Msg); //Send to all opponents
    fSent[aTick mod MAX_SCHEDULE] := true;
  finally
    Msg.Free;
  end;

  //Now we wait for the Tick and hopefully everyone will confirm our commands
end;


procedure TGameInputProcess_Multi.SendConfirmation(aTick:cardinal; aPlayerLoc:byte);
var Msg:TKMemoryStream;
begin
  Msg := TKMemoryStream.Create;
  try
    Msg.Write(byte(kdp_Confirmation));
    Msg.Write(aTick); //Target Tick in 1..n range
    Msg.Write(MyPlayer.PlayerID, SizeOf(MyPlayer.PlayerID));
    Msg.Write(fSchedule[aTick mod MAX_SCHEDULE, aPlayerLoc].CRC);
    fNetworking.SendConfirmation(Msg, aPlayerLoc); //Send to opponent
  finally
    Msg.Free;
  end;
end;


//Decode recieved messages (Commands from other players, Confirmations, Errors)
procedure TGameInputProcess_Multi.RecieveCommands(const aData:string);
var M:TKMemoryStream; D:TKMDataType; Tick:integer; PlayID:TPlayerID; CRC:cardinal;
begin
  M := TKMemoryStream.Create;
  try
    M.WriteAsText(aData);
    M.Read(D, 1); //Decode header
    M.Read(Tick); //Target tick
    M.Read(PlayID, SizeOf(PlayID)); //Message sender

    case D of
      kdp_Commands:
          begin
            fSchedule[Tick mod MAX_SCHEDULE, byte(PlayID)].Load(M);
            SendConfirmation(Tick, byte(PlayID));
          end;
      kdp_Confirmation: //Recieved CRC should match our commands pack
          begin
            M.Read(CRC);
            Assert(CRC = fSchedule[Tick mod MAX_SCHEDULE, byte(MyPlayer.PlayerID)].CRC);
            fConfirmation[Tick mod MAX_SCHEDULE, byte(PlayID)] := true;
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
    Result := Result and (fConfirmation[aTick, fNetworking.NetPlayers[i].StartLocID] or not fNetworking.NetPlayers[i].Alive);
end;


//Timer is called after all commands from player are taken,
//upcoming commands will be stacked into next batch
procedure TGameInputProcess_Multi.Timer(aTick:cardinal);
var i,k,Tick:integer;
begin
  Inherited;
  Assert(ReplayState <> gipReplaying);

  Tick := aTick mod MAX_SCHEDULE; //Place in a ring buffer

  //Execute commands, in order players go (1,2,3..)
  for i:=1 to fNetworking.NetPlayers.Count do
  for k:=1 to fSchedule[Tick, fNetworking.NetPlayers[i].StartLocID].Count do
  begin
    if fNetworking.NetPlayers[i].Alive then //Skip dead players
    begin
      ExecCommand(fSchedule[Tick, fNetworking.NetPlayers[i].StartLocID].Get(k));
      StoreCommand(fSchedule[Tick, fNetworking.NetPlayers[i].StartLocID].Get(k));
    end;
    fSchedule[Tick, fNetworking.NetPlayers[i].StartLocID].Clear;
    FillChar(fConfirmation[Tick], SizeOf(fConfirmation[Tick]), #0); //Reset
    fSent[Tick] := false;
  end;
end;


//todo: Join unsent commands from player
//todo: Check CRCs
procedure TGameInputProcess_Multi.UpdateState(aTick:cardinal);
var i:integer;
begin
  //if not CRC = CRC then
  //  fOnCRCFail(Self);
  for i:=aTick to aTick+fDelay do
    SendCommands(i);
end;


end.
