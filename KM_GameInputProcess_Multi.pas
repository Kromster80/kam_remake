unit KM_GameInputProcess_Multi;
{$I KaM_Remake.inc}
interface
uses KM_GameInputProcess, KM_Network, KM_Defaults;


const MAX_SCHEDULE = 32; //How many turns to plan ahead (3.2sec)

type TGIPList = class
       private
         fCount:integer;
         fList:array of TGameInputCommand; //1..n
       public
         procedure Clear;
         property Count:integer read fCount;
         procedure Add(aCommand:TGameInputCommand);
         function Get(aIndex:integer):TGameInputCommand;
     end;

type
  TGameInputProcess_Multi = class(TGameInputProcess)
    private
      fNetwork: TKMNetwork;
      fDelay:word; //How many ticks ahead the commands are scheduled
      fPlayersEnabled:array[1..MAX_PLAYERS]of boolean; //See which players confirmations do we need
      fSchedule: array[0..MAX_SCHEDULE-1, 1..MAX_PLAYERS] of packed record //Ring buffer
        Commands:TGIPList; //Each player can have any number of commands scheduled for execution in one tick
        Confirmed:array[1..MAX_PLAYERS]of boolean; //All players must confirm they have the same commands in schedule
      end;
      procedure SendCommands;
    protected
      procedure TakeCommand(aCommand:TGameInputCommand); override;
    public
      constructor Create(aReplayState:TGIPReplayState);
      destructor Destroy; override;
      procedure RecieveCommands(aData:string); //Called by TKMNetwork
      procedure Tick(aTick:cardinal); override;
  end;


implementation
uses KM_Game;


procedure TGIPList.Clear;
begin
  fCount := 0;
end;


function TGIPList.Get(aIndex:integer):TGameInputCommand;
begin
  Result := fList[aIndex];
end;


procedure TGIPList.Add(aCommand:TGameInputCommand);
begin
  inc(fCount);
  if fCount >= length(fList) then
    setlength(fList, fCount+8);

  fList[fCount] := aCommand;
end;


constructor TGameInputProcess_Multi.Create(aReplayState:TGIPReplayState);
var i,k:integer;
begin
  Inherited Create(aReplayState);
  fDelay := 10;

  fNetwork := TKMNetwork.Create;

  //Allocate memory for all commands lists
  for i:=0 to MAX_SCHEDULE-1 do for k:=1 to MAX_PLAYERS do
    fSchedule[i,k].Commands := TGIPList.Create;
end;


destructor TGameInputProcess_Multi.Destroy;
var i,k:integer;
begin
  for i:=0 to MAX_SCHEDULE-1 do for k:=1 to MAX_PLAYERS do
    fSchedule[i,k].Commands.Free;
  fNetwork.Free;
  Inherited;
end;


procedure TGameInputProcess_Multi.TakeCommand(aCommand:TGameInputCommand);
var id:integer; MyPlayerID:byte;
begin
  Assert(fDelay < MAX_SCHEDULE, 'Error, fDelay >= MAX_SCHEDULE');

  id := (fGame.GetTickCount + fDelay) mod MAX_SCHEDULE; //Place in a ring buffer
  MyPlayerID := 1; //todo: replace with real ID

  fSchedule[id,MyPlayerID].Commands.Add(aCommand);
  fSchedule[id,MyPlayerID].Confirmed[MyPlayerID] := true; //Confirm by self
end;


procedure TGameInputProcess_Multi.SendCommands;
var id:integer; MyPlayerID:byte;
begin
  id := (fGame.GetTickCount + fDelay) mod MAX_SCHEDULE;
  MyPlayerID := 1; //todo: replace with real ID
  //fNetwork.SendBatch(fSchedule[id,MyPlayerID].Commands);

  //Now we just wait till the tick and hopefully everyone will confirm
  //to have recieved our commands
end;


procedure TGameInputProcess_Multi.RecieveCommands(aData:string);
begin
  //todo: Decode recieved messages (Commands from other players, Confirmations, Errors)
end;


procedure TGameInputProcess_Multi.Tick(aTick:cardinal);
var i,k,id:integer; Conf:boolean;
begin
  Inherited;
  if ReplayState = gipReplaying then exit;

  //Tick is called after all commands from player are taken,
  //upcoming commands will be stacked into next batch
  SendCommands;

  id := fGame.GetTickCount mod MAX_SCHEDULE; //Place in a ring buffer

  Conf := true; //See if all players confirm this tick commands list
  for i:=1 to MAX_PLAYERS do
    for k:=1 to MAX_PLAYERS do
      Conf := Conf and (fSchedule[id,i].Confirmed[k] or not fPlayersEnabled[k]);

  //Handle the issue
  if not Conf then
  begin
    Assert(false,'Unconfirmed command');
    //todo: handle delay with fGame?
  end;

  //Commands are executed in order players go
  if Conf then
  for i:=1 to MAX_PLAYERS do
    for k:=1 to fSchedule[id,i].Commands.Count do
    begin
      if fPlayersEnabled[k] then begin //Skip missing players
        ExecCommand(fSchedule[id,i].Commands.Get(k));
        StoreCommand(fSchedule[id,i].Commands.Get(k));
      end;
      fSchedule[id,i].Commands.Clear;
      FillChar(fSchedule[id,i].Confirmed, SizeOf(fSchedule[id,i].Confirmed), #0); //Reset
    end;

end;


end.
